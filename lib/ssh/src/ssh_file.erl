%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%

%%% Description: SSH file handling

-module(ssh_file).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").

%%%--------------------- server exports ---------------------------
-behaviour(ssh_server_key_api).
-export([host_key/2, is_auth_key/3]).
-export_type([system_dir_daemon_option/0]).
-type system_dir_daemon_option()   :: {system_dir, string()}.

%%%--------------------- client exports ---------------------------
-behaviour(ssh_client_key_api).
-export([is_host_key/5, user_key/2, add_host_key/4]).
-export_type([pubkey_passphrase_client_options/0]).
-type pubkey_passphrase_client_options() ::   {dsa_pass_phrase,      string()}
                                            | {rsa_pass_phrase,      string()}
%% Not yet implemented:                     | {ed25519_pass_phrase,  string()}
%% Not yet implemented:                     | {ed448_pass_phrase,    string()}
                                            | {ecdsa_pass_phrase,    string()} .

%%%--------------------- common exports ---------------------------
-export_type([user_dir_common_option/0,
              user_dir_fun_common_option/0
             ]).

-type user_dir_common_option()     :: {user_dir,  string()}.
-type user_dir_fun_common_option() :: {user_dir_fun, user2dir()}.
-type user2dir() :: fun((RemoteUserName::string()) -> UserDir :: string()) .

-type optimize_key_lookup() :: {optimize, time|space} .

%%%================================================================
%%%
%%% API
%%%

%%%---------------- SERVER API ------------------------------------
-spec host_key(Algorithm, Options) -> Result when
      Algorithm :: ssh:pubkey_alg(),
      Result :: {ok, public_key:private_key()} | {error, term()},
      Options :: ssh_server_key_api:daemon_key_cb_options(none()).

host_key(Algorithm, Opts) ->
    read_ssh_key_file(system, private, Algorithm, Opts).

%%%................................................................
-spec is_auth_key(Key, User, Options) -> boolean() when
      Key :: public_key:public_key(),
      User :: string(),
      Options :: ssh_server_key_api:daemon_key_cb_options(optimize_key_lookup()).

is_auth_key(Key0, User, Opts) ->
    Dir = ssh_dir({remoteuser,User}, Opts),
    ok = assure_file_mode(Dir, user_read),
    KeyType = normalize_alg(
                erlang:atom_to_binary(ssh_transport:public_algo(Key0), latin1)),
    Key = encode_key(Key0),
    lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys"), Opts)
        orelse
    lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys2"), Opts).

%%%---------------- CLIENT API ------------------------------------
-spec user_key(Algorithm, Options) -> Result when
      Algorithm :: ssh:pubkey_alg(),
      Result :: {ok, public_key:private_key()} |
                {error, string()},
      Options :: ssh_client_key_api:client_key_cb_options(none()).

user_key(Algorithm, Opts) ->
    read_ssh_key_file(user, private, Algorithm, Opts).

%%%................................................................
%%% New style (with port number)
-spec is_host_key(Key, Host, Port, Algorithm, Options) -> Result when
      Key :: public_key:public_key(),
      Host :: inet:ip_address() | inet:hostname() | [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number(),
      Algorithm :: ssh:pubkey_alg(),
      Options :: ssh_client_key_api:client_key_cb_options(optimize_key_lookup()),
      Result :: boolean() | {error, term()} .

is_host_key(Key0, Hosts0, Port, Algorithm, Opts) ->
    Dir = ssh_dir(user, Opts),
    File = filename:join(Dir, "known_hosts"),
    Hosts = [list_to_binary(H) || H <- normalize_hosts_list(Hosts0, Port)],
    KeyType = normalize_alg(erlang:atom_to_binary(Algorithm, latin1)),
    Key = encode_key(Key0),
    ok = assure_file_mode(File, user_read),
    lookup_host_keys(Hosts, KeyType, Key, File, Opts).

%%%----------------------------------------------------------------
-spec add_host_key(Host, Port, Key, Options) -> Result when 
      Host :: inet:ip_address() | inet:hostname()
            | [inet:ip_address() | inet:hostname()],
      Port :: inet:port_number(),
      Key :: public_key:public_key(),
      Options :: ssh_client_key_api:client_key_cb_options(none()),
      Result :: ok | {error, term()}.

add_host_key(Hosts0, Port, Key, Opts) ->
    File = file_name(user, "known_hosts", Opts),
    assure_file_mode(File, user_write),
    case file:open(File, [write,append]) of
	{ok, Fd} ->
            KeyType = erlang:atom_to_binary(ssh_transport:public_algo(Key), latin1),
            EncKey = ssh_message:ssh2_pubkey_encode(Key),
            Hosts1 = normalize_hosts_list(Hosts0, Port),
            SshBin =
                iolist_to_binary(["\n", % Just in case the last line is not terminated by \n
                                  lists:join(",", Hosts1), " ",
                                  KeyType," ",base64:encode(iolist_to_binary(EncKey)),
                                  "\n"]),
            Res = file:write(Fd, SshBin),
	    file:close(Fd),
	    Res;
	{error,Error} ->
	    {error,{add_host_key,Error}}
    end.

%%%================================================================
%%%
%%% Local functions
%%%

%%%---------------- SERVER FUNCTIONS ------------------------------

lookup_auth_keys(KeyType, Key, File, Opts) ->
    case get_kb_option(optimize, Opts, time) of
        time ->
            case file:read_file(File) of
                {ok,Bin} ->
                    Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
                    find_key(KeyType, Key, Lines);
                _ ->
                    false
            end;
        space ->
            case file:open(File, [read, binary]) of
                {ok, Fd} ->
                    Result =
                        read_test_loop(Fd,
                                       fun(Line) ->
                                               find_key(KeyType, Key, [Line])
                                       end),
                    file:close(Fd),
                    Result;
                {error,_Error} ->
                    false
            end;
        Other ->
            {error,{is_auth_key,{opt,Other}}}
    end.


find_key(KeyType, Key, [<<"#",_/binary>> | Lines]) ->
    find_key(KeyType, Key, Lines);
find_key(KeyType, Key, [Line | Lines]) ->
    try
        [E1,E2|Es] = binary:split(Line, <<" ">>, [global,trim_all]),
        [normalize_alg(E1), normalize_alg(E2) | Es] % KeyType is in first or second element
    of
        [_Options, KeyType, Key | _Comment] ->
            true;
        [KeyType, Key | _Comment] ->
            true;
        _ ->
            find_key(KeyType, Key, Lines)
    catch
        _:_ ->
            find_key(KeyType, Key, Lines)
    end;
find_key(_, _, _) ->
    false.


%%%---------------- CLIENT FUNCTIONS ------------------------------

normalize_alg(<<"rsa-sha2-",_/binary>>) -> <<"ssh-rsa">>;
normalize_alg(X) -> X.

%%%--------------------------------
normalize_hosts_list(Hosts, Port) when is_list(hd(Hosts)) ->
    lists:reverse(
      lists:foldl(fun(H0, Acc) ->
                          H1s = add_ip(replace_localhost(H0)),
                          Hs = case Port of
                                   22 -> H1s;
                                   _ -> [lists:concat(["[",Hx,"]:",Port]) || Hx <- H1s]
                              end,
                          lists:foldl(
                            fun(Hy, Acc2) ->
                                    case lists:member(Hy, Acc2) of
                                        true ->
                                            Acc2;
                                        false ->
                                            [Hy|Acc2]
                                    end
                            end, Acc, Hs)
                  end, [], Hosts));
normalize_hosts_list(Hosts, Port) ->
    normalize_hosts_list([Hosts], Port).

replace_localhost(any) ->
    replace_localhost("localhost");
replace_localhost(loopback) ->
    replace_localhost("localhost");
replace_localhost("localhost") ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
replace_localhost(H) when is_atom(H) ->
    replace_localhost(atom_to_list(H));
replace_localhost(Host) ->
    Host.

add_ip(IP) when is_tuple(IP) ->
    [ssh_connection:encode_ip(IP)];
add_ip(Host) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    case ssh_connection:encode_ip(Addr) of
		false -> [Host];
                Host -> [Host];
		IPString -> [Host,IPString]
	    end;
	_ -> [Host]
    end.

%%%--------------------------------
encode_key(Key) ->
    base64:encode(
      iolist_to_binary(
        ssh_message:ssh2_pubkey_encode(Key))).

%%%--------------------------------
read_test_loop(Fd, Test) ->
    case io:get_line(Fd, '') of
	eof ->
            file:close(Fd),
	    false;
	{error,Error} ->
	    %% Rare... For example NFS errors
	    {error,Error};
	Line0 ->
            case binary:split(Line0, <<"\n">>, [global,trim_all]) of % remove trailing \n
                [Line] ->
                    case Test(Line) of
                        false ->
                            read_test_loop(Fd, Test);
                        Other ->
                            Other
                    end;
                _ ->
                    read_test_loop(Fd, Test)
            end
    end.
                    
%%%--------------------------------

lookup_host_keys(Hosts, KeyType, Key, File, Opts) ->
    case get_kb_option(optimize, Opts, time) of
        time ->
            case file:read_file(File) of
                {ok,Bin} ->
                    Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
                    case find_host_key(Hosts, KeyType, Key, Lines) of
                        {true,RestLines} ->
                            case revoked_key(Hosts, KeyType, Key, RestLines) of
                                true ->
                                    {error,revoked_key};
                                false ->
                                    true
                            end;
                        false ->
                            false
                    end;
                {error,enoent} ->
                    false;
                {error,Error} ->
                    {error,{is_host_key,Error}}
            end;
        space ->
            case file:open(File, [read, binary]) of
                {ok, Fd} ->
                    Result =
                        case read_test_loop(Fd,
                                            fun(Line) ->
                                                    find_host_key(Hosts, KeyType, Key, [Line])
                                            end)
                        of
                            {true,_} ->
                                %% The key is found, now check the rest of the file to see if it is
                                %% revoked
                                case read_test_loop(Fd,
                                                    fun(Line) ->
                                                            revoked_key(Hosts, KeyType, Key, [Line])
                                                    end)
                                of
                                    true ->
                                        {error,revoked_key};
                                    false ->
                                        true
                                end;
                            {error,Error} ->
                                {error,{is_host_key,Error}};
                            Other ->
                                Other
                        end,
                    file:close(Fd),
                    Result;
                {error,Error} ->
                    {error,Error}
            end;
        Other ->
            {error,{is_host_key,{opt,Other}}}
    end.


find_host_key(Hosts, KeyType, EncKey, [<<"#",_/binary>>|PatternLines]) ->
    %% skip comments
    find_host_key(Hosts, KeyType, EncKey, PatternLines);
find_host_key(Hosts, KeyType, EncKey, [Line|PatternLines]) ->
    %% split the line into the separate parts:
    %%    option? pattern(,pattern)* keytype key comment?
    SplitLine = binary:split(Line, <<" ">>, [global,trim_all]),
    case known_key_in_line(Hosts, KeyType, EncKey, SplitLine) of
        true ->
            {true, PatternLines};
        false ->
            find_host_key(Hosts, KeyType, EncKey, PatternLines)
    end;
find_host_key(_, _, _, []) ->
    false.


revoked_key(Hosts, KeyType, EncKey, [<<"@revoked ",RestLine/binary>> | Lines]) ->
    case binary:split(RestLine, <<" ">>, [global,trim_all]) of
        [Patterns, KeyType, EncKey|_Comment] ->
            %% Very likeley to be a revoked key,
            %% but does any of the hosts match the pattern?
            case host_match(Hosts, Patterns) of
                true ->
                    true;
                false ->
                    revoked_key(Hosts, KeyType, EncKey, Lines)
            end;
        _ ->
            revoked_key(Hosts, KeyType, EncKey, Lines)
    end;
revoked_key(Hosts, KeyType, EncKey, [_ | Lines]) ->
    %% Not a revokation line, check the rest
    revoked_key(Hosts, KeyType, EncKey, Lines);
revoked_key(_, _, _, _) ->
    false.


known_key_in_line(Hosts, KeyType, EncKey, FullLine=[Option | Rest]) ->
    case line_match(Hosts, KeyType, EncKey, Rest) of
        true ->
            case Option of
                <<"@revoked">> ->
                    {error, revoked_key};
                _ ->
                    %% No other options than @revoked handled (but the key matched)
                    false
            end;
        false ->
            line_match(Hosts, KeyType, EncKey, FullLine)
    end;
known_key_in_line(_, _, _, _) ->
    false.


line_match(Hosts, KeyType, EncKey, [Patterns, KeyType0, EncKey0|_Comment]) ->
    KeyType==normalize_alg(KeyType0)
        andalso EncKey==EncKey0
        andalso host_match(Hosts, Patterns);
line_match(_, _, _, _) ->
    false.



host_match(Hosts, Patterns) ->
    PatternList = binary:split(Patterns, <<",">>, [global]),
    host_matchL(Hosts, PatternList).
    
host_matchL([H|Hosts], Patterns) ->
    case one_host_match(H, Patterns) of
        true ->
            true;
        false ->
            host_matchL(Hosts, Patterns)
    end;
host_matchL(_, _) ->
    false.


one_host_match(H, [Pat|Patterns]) ->
    case pos_match(H, Pat) of
        true ->
            %% Not true if there is any "!" pattern that matches
            not lists:any(fun(P) -> neg_match(H,P) end,
                          Patterns);
        false ->
            one_host_match(H, Patterns)
    end;
one_host_match(_, _) ->
    false.


neg_match(H, <<"!",P/binary>>) ->
    pos_match(H, P);
neg_match(_, _) ->
    false.


pos_match(_, <<"*">>    ) -> true;
pos_match(_, <<"*:*">>  ) -> true;
pos_match(_, <<"[*]:*">>) -> true;
pos_match(H, <<"!",P/binary>>) -> not pos_match(H, P);
pos_match(H, H) -> true;
pos_match(H, P) ->
    case
        {binary:split(H,<<":">>),
         binary:split(P, <<":">>)}
    of
        {[Hh,_], [Ph,<<"*">>]} ->
            %% [host]:port [host]:*
            Ph == Hh;

        {[Hh], [Ph,<<"*">>]} ->
            %% host [host]:*
            Sz = size(Hh),
            Ph == <<"[", Hh:Sz/binary, "]">>;

        {[Hh], [Ph,<<"22">>]} ->
            %% host [host]:22
            Sz = size(Hh),
            Ph == <<"[", Hh:Sz/binary, "]">>;

        _ ->
            false
    end.

%%%---------------- COMMON FUNCTIONS ------------------------------

assure_file_mode(File, user_write) -> assure_file_mode(File, 8#200);
assure_file_mode(File, user_read) -> assure_file_mode(File, 8#400);
assure_file_mode(File, Mode) ->
    case file:read_file_info(File) of
        {ok,#file_info{mode=FileMode}} ->
            case (FileMode band Mode) of % is the wanted Mode set?
                Mode -> 
                    %% yes
                    ok;
                _ ->
                    %% no
                    file:change_mode(File, FileMode bor Mode) % set missing bit(s)
            end;
        {error,enoent} ->
            %% Not yet created
            ok;
        {error,Error} ->
            {error,Error}
    end.


get_kb_option(Key, Opts, Default) ->
    try
        proplists:get_value(Key, 
                            proplists:get_value(key_cb_private, Opts, []),
                            Default)
    catch
        _:_ ->
            Default
    end.


read_ssh_key_file(Role, PrivPub, Algorithm, Opts) ->
    File = file_name(Role, file_base_name(Role,Algorithm), Opts),
    Password = %% Pwd for Host Keys is an undocumented option and should not be used
        proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),

    ok = assure_file_mode(File, user_read),
    case file:read_file(File) of
        {ok, Pem} ->
            try
                decode_ssh_file(PrivPub, Algorithm, Pem, Password)
            catch
                throw:Reason ->
                    {error, Reason};
                error:Reason ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.


decode_ssh_file(PrivPub, Algorithm, Pem, Password) ->
    %% Private Key
    try get_key_part(Pem) of
        {'openssh-key-v1', Bin, _KeyValues} ->
            %% Holds both public and private keys
            KeyPairs = new_openssh_decode(Bin, Password),
            ValidKeys =
                [Key || {Pub,Priv} <- KeyPairs,
                        Key <- [Pub,Priv],
                        ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm)],
            %% Select one (for now, just pick the first found):
            case ValidKeys of
                [Key|_] -> {ok,Key};
                [] -> {error,bad_keytype_in_file}
            end;

        {rfc4716, Bin, _KeyValues} ->
            %% rfc4716 only defines public keys
            Key = ssh_message:ssh2_pubkey_decode(Bin),
            case ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm) of
                true -> {ok,Key};
                false -> {error,bad_keytype_in_file}
            end;

        {Type, Bin, KeyValues} ->
            Key =
                case get_encrypt_hdrs(KeyValues) of
                    not_encrypted ->
                        public_key:pem_entry_decode({Type,Bin,not_encrypted});
                    [Cipher,Salt] when is_binary(Cipher),
                                       is_binary(Salt),
                                       Password =/= ignore ->
                        CryptInfo =
                            {binary_to_list(Cipher), unhex(binary_to_list(Salt))},
                        public_key:pem_entry_decode({Type,Bin,CryptInfo}, Password);
                    _X ->
                        throw("No pass phrase provided for private key file")
                end,
            case ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm) of
                true -> {ok,Key};
                false -> {error,bad_keytype_in_file}
            end
    catch
        _:_ -> error(bad_or_unsupported_key_format)
    end.

get_encrypt_hdrs(KVs) ->
    lists:foldl(fun({<<"Proc-Type">>, <<"4,ENCRYPTED", _/binary>>}, _Acc) ->
                        {proc_type, <<"4,ENCRYPTED">>};
                   ({<<"DEK-Info">>, DEKinfo}, {proc_type,_}) ->
                        binary:split(DEKinfo, <<",">>);
                   (_, Acc) ->
                        Acc
                end, not_encrypted, KVs).

unhex(S) ->
    %% I would like to do erlang:list_to_integer(S,16), but that does not fit
    %% the public_key:pem_entry_decode API
    list_to_binary(
      lists:foldr(fun(D2, {D1,Acc}) ->
                          [erlang:list_to_integer([D2,D1], 16) | Acc]; % sic!
                     (D1, Acc) when is_list(Acc) ->
                          {D1,Acc}
                  end, [], S)).

file_base_name(user,   'ecdsa-sha2-nistp256') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp384') -> "id_ecdsa";
file_base_name(user,   'ecdsa-sha2-nistp521') -> "id_ecdsa";
file_base_name(user,   'rsa-sha2-256'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-384'       ) -> "id_rsa";
file_base_name(user,   'rsa-sha2-512'       ) -> "id_rsa";
file_base_name(user,   'ssh-dss'            ) -> "id_dsa";
file_base_name(user,   'ssh-ed25519'        ) -> "id_ed25519";
file_base_name(user,   'ssh-ed448'          ) -> "id_ed448";
file_base_name(user,   'ssh-rsa'            ) -> "id_rsa";
file_base_name(system, 'ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key";
file_base_name(system, 'ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key";
file_base_name(system, 'rsa-sha2-256'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-384'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'rsa-sha2-512'       ) -> "ssh_host_rsa_key";
file_base_name(system, 'ssh-dss'            ) -> "ssh_host_dsa_key";
file_base_name(system, 'ssh-ed25519'        ) -> "ssh_host_ed25519_key";
file_base_name(system, 'ssh-ed448'          ) -> "ssh_host_ed448_key";
file_base_name(system, 'ssh-rsa'            ) -> "ssh_host_rsa_key";
file_base_name(system, _                    ) -> "ssh_host_key".


identity_pass_phrase('ssh-dss'            ) -> dsa_pass_phrase;
identity_pass_phrase('ssh-rsa'            ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-256'       ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-384'       ) -> rsa_pass_phrase;
identity_pass_phrase('rsa-sha2-512'       ) -> rsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp256') -> ecdsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp384') -> ecdsa_pass_phrase;
identity_pass_phrase('ecdsa-sha2-nistp521') -> ecdsa_pass_phrase;
%% Not yet implemented: identity_pass_phrase('ssh-ed25519'   ) -> ed25519_pass_phrase;
%% Not yet implemented: identity_pass_phrase('ssh-ed448'     ) -> ed448_pass_phrase;
identity_pass_phrase(_) -> undefined.


%%%----------------------------------------------------------------
file_name(Type, Name, Opts) ->
    filename:join(ssh_dir(Type, Opts), Name).


%%%--------------------------------
ssh_dir({remoteuser, User}, Opts) ->
    %% server use this to find individual keys for an individual
    %% user when user tries to login with publickey
    case proplists:get_value(user_dir_fun, Opts) of
	undefined ->
            %% Try the local user instead
            ssh_dir(user, Opts);
	FUN ->
	    FUN(User)
    end;

ssh_dir(user, Opts) ->
    %% client use this to find client ssh keys
    case proplists:get_value(user_dir, Opts, false) of
	false -> default_user_dir();
	D -> D
    end;

ssh_dir(system, Opts) ->
    %% server use this to find server host keys
    proplists:get_value(system_dir, Opts, "/etc/ssh").

%%%--------------------------------
default_user_dir() ->
    try
	default_user_dir(os:getenv("HOME"))
    catch
	_:_ ->
	    default_user_dir(init:get_argument(home))
    end.

default_user_dir({ok,[[Home|_]]}) ->
    default_user_dir(Home);
default_user_dir(Home) when is_list(Home) ->
    UserDir = filename:join(Home, ".ssh"),
    ok = filelib:ensure_dir(filename:join(UserDir, "dummy")),
    UserDir.

%%%################################################################
get_key_part(RawBin) when is_binary(RawBin) ->
    case binary:split(
           binary:replace(RawBin, <<"\\\n">>, <<"">>, [global]),
           <<"\n">>, [global,trim_all])
    of
        [<<"---- BEGIN SSH2 PUBLIC KEY ----">> | Lines0] ->
            %% RFC 4716 format
            {KeyValues,Lines} = get_hdr_lines(Lines0, []),
            ExpectedEndLine = <<"---- END SSH2 PUBLIC KEY ----">>,
            {rfc4716, get_body(Lines,ExpectedEndLine), KeyValues};

        [<<"-----BEGIN ", Rest/binary>> | Lines0] ->
            %% PEM format
            ExpectedEndLine = <<"-----END ",Rest/binary>>,
            [MiddlePart, <<>>] = binary:split(Rest,  <<" KEY-----">>),
            {KeyValues,Lines} = get_hdr_lines(Lines0, []),
            {asn1_type(MiddlePart), get_body(Lines,ExpectedEndLine), KeyValues}
    end.
            

get_hdr_lines(Lines, Acc) ->
    Line1 = hd(Lines),
    case binary:split(Line1, <<":">>) of
        [Line1] ->
            {lists:reverse(Acc), Lines};
        [Key,Value] ->
            get_hdr_lines(tl(Lines), [{trim(Key),trim(Value)}|Acc])
    end.


get_body(Lines, ExpectedEndLine) ->
    {KeyPart, [ExpectedEndLine]} = lists:split(length(Lines)-1, Lines),
    base64:mime_decode(iolist_to_binary(KeyPart)).

trim(<<" ",B/binary>>) -> trim(B);
trim(B) -> B.

asn1_type(<<"RSA PRIVATE">>) -> 'RSAPrivateKey';
asn1_type(<<"RSA PUBLIC">>) -> 'RSAPublicKey';
asn1_type(<<"DSA PRIVATE">>) -> 'DSAPrivateKey';
asn1_type(<<"EC PRIVATE">>) -> 'ECPrivateKey';
asn1_type(<<"OPENSSH PRIVATE">>) -> 'openssh-key-v1';
asn1_type(_) -> undefined.

%%%================================================================
%%% From https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.key
%%%

-define(NON_CRYPT_BLOCKSIZE, 8).

new_openssh_decode(<<"openssh-key-v1",0,
                     ?DEC_BIN(CipherName, _L1),
                     ?DEC_BIN(KdfName, _L2),
                     ?DEC_BIN(KdfOptions, _L3),
                     ?UINT32(N),      % number of keys
                     Rest/binary
                   >>, Pwd) ->
    new_openssh_decode(Rest, N, Pwd, CipherName, KdfName, KdfOptions, N, []).


new_openssh_decode(<<?DEC_BIN(BinKey,_L1), Rest/binary>>, I, Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAcc) when I>0 ->
    PublicKey = ssh_message:ssh2_pubkey_decode(BinKey),
    new_openssh_decode(Rest, I-1, Pwd, CipherName, KdfName, KdfOptions, N, [PublicKey|PubKeyAcc]);

new_openssh_decode(<<?DEC_BIN(Encrypted,_L)>>,
                   0, Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAccRev) ->
    PubKeys = lists:reverse(PubKeyAccRev),
    try
        Plain = decrypt_new_openssh(Encrypted, KdfName, KdfOptions, CipherName, Pwd),
        new_openssh_decode_priv_keys(Plain, N, N, [], [])
    of
        {PrivKeys, _Comments} ->
            lists:map(fun({ {ed_pub,A,Pub}, {ed_pri,A,Pub,Pri0} }) ->
                              Pri = binary:part(Pri0, {0,size(Pri0)-size(Pub)}),
                              {{ed_pub,A,Pub}, {ed_pri,A,Pub,Pri}};
                         (Pair) ->
                              Pair
                      end, lists:zip(PubKeys, PrivKeys))
    catch
        error:{decryption, DecryptError} ->
            error({decryption, DecryptError})
    end.


new_openssh_decode_priv_keys(Bin, I, N, KeyAcc, CmntAcc) when I>0 ->
    {PrivKey, <<?DEC_BIN(Comment,_Lc),Rest/binary>>} = ssh_message:ssh2_privkey_decode2(Bin),
    new_openssh_decode_priv_keys(Rest, I-1, N, [PrivKey|KeyAcc], [Comment|CmntAcc]);
new_openssh_decode_priv_keys(_Padding, 0, _N, PrivKeyAccRev, CommentAccRev) ->
    {lists:reverse(PrivKeyAccRev),
     lists:reverse(CommentAccRev)}.


decrypt_new_openssh(Encrypted, <<"none">>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_new_openssh(Encrypted, <<>>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_new_openssh(_Encrypted, <<"bcrypt">>, <<?DEC_BIN(_Salt,_L),?UINT32(_Rounds)>>, _CipherName, _Pwd) ->
    error({decryption, {not_supported,bcrypt}});
decrypt_new_openssh(_Encrypted, KdfName, _KdfOpts, _CipherName, _Pwd) ->
    error({decryption, {not_supported,KdfName}}).


check_valid_decryption(<<?UINT32(Checkint1),?UINT32(Checkint2),Plain/binary>>, BlockSize) when Checkint2==Checkint1 ->
    case check_padding(Plain, BlockSize) of
        true ->
            Plain;
        false ->
            error({decryption,bad_padding})
    end;
check_valid_decryption(_, _) ->
    error({decryption,bad_result}).


check_padding(Bin, BlockSize) ->
    N = binary:last(Bin),
    if
        N < BlockSize ->
            %% Check that Bin is <<...,1,2,...,N>>
            Padding = binary:part(Bin, {byte_size(Bin),-N}),
            ExpectedPadding = list_to_binary(lists:seq(1,N)), % <<1,2,...,N>>
            Padding == ExpectedPadding;
        true ->
            true
    end.

%%%================================================================
%%%
