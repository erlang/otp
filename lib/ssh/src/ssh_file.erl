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
-export([is_host_key/4, user_key/2, add_host_key/3]).
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

%%%================================================================
%%%
%%% API
%%%

%%%---------------- SERVER API ------------------------------------
host_key(Algorithm, Opts) ->
    read_ssh_key_file(system, private, Algorithm, Opts).

is_auth_key(Key, User ,Opts) ->
    KeyType =  erlang:atom_to_binary(ssh_transport:public_algo(Key), latin1),
    Dir = ssh_dir({remoteuser,User}, Opts),
    lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys"))
        orelse
    lookup_auth_keys(KeyType, Key, filename:join(Dir,"authorized_keys2")).

%%%---------------- CLIENT API ------------------------------------
user_key(Algorithm, Opts) ->
    read_ssh_key_file(user, private, Algorithm, Opts).

is_host_key(Key, PeerName, Algorithm, Opts) ->
    KeyType = erlang:atom_to_binary(Algorithm, latin1),
    Hosts = binary:split(list_to_binary(replace_localhost(PeerName)),
                         <<",">>, [global]), % make a list of hosts
    Dir = ssh_dir(user, Opts),
    lookup_host_keys(Hosts, KeyType, Key, filename:join(Dir,"known_hosts")).

add_host_key(Host, Key, Opts) ->
    Host1 = add_ip(replace_localhost(Host)),
    KnownHosts = file_name(user, "known_hosts", Opts),
    case file:open(KnownHosts, [write,append]) of
	{ok, Fd} ->
	    ok = file:change_mode(KnownHosts, 8#644),
            KeyType = erlang:atom_to_binary(ssh_transport:public_algo(Key), latin1),
            EncKey = ssh_message:ssh2_pubkey_encode(Key),
            SshBin =
                iolist_to_binary([Host1, " ",
                                  KeyType," ",base64:encode(iolist_to_binary(EncKey)),
                                  "\n"]),
            Res = file:write(Fd, SshBin),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.

%%%================================================================
%%%
%%% Local functions
%%%

%%%---------------- SERVER FUNCTIONS ------------------------------

lookup_auth_keys(KeyType, Key, File) ->
    case file:read_file(File) of
        {ok,Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
            find_key(KeyType, Key, Lines);
        _ ->
            false
    end.

find_key(KeyType, Key, [Line|Lines]) ->
    case find_key_in_line(KeyType, Key, binary:split(Line, <<" ">>, [global,trim_all])) of
        true ->
            true;
        false ->
            find_key(KeyType, Key, Lines)
    end;
find_key(_, _, _) ->
    false.

        
find_key_in_line(_KeyType, _Key, [<<"#",_/binary>> |_]) ->
    false;
find_key_in_line(KeyType, Key, [KeyType, Base64EncodedKey, _Comment]) ->
    %% Right KeyType. Try to decode to see if it matches
    Key == decode_key(Base64EncodedKey);
find_key_in_line(KeyType, Key, [_Option | [_,_,_|_]=Rest]) ->
    %% Dont care for options
    find_key_in_line(KeyType, Key, Rest);
find_key_in_line(_, _, _) ->
    false.

decode_key(Base64EncodedKey) ->
    ssh_message:ssh2_pubkey_decode(
      base64:mime_decode(Base64EncodedKey)).

%%%---------------- CLIENT FUNCTIONS ------------------------------

%%%--------------------------------
%% in: "host" out: "host,1.2.3.4.
add_ip(IP) when is_tuple(IP) ->
    ssh_connection:encode_ip(IP);
add_ip(Host) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    case ssh_connection:encode_ip(Addr) of
		false -> Host;
                Host -> Host;
		IPString -> Host ++ "," ++ IPString
	    end;
	_ -> Host
    end.

replace_localhost("localhost") ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
replace_localhost(Host) ->
    Host.

%%%--------------------------------
lookup_host_keys(Hosts, KeyType, Key, File) ->
    case file:read_file(File) of
        {ok,Bin} ->
            Lines = binary:split(Bin, <<"\n">>, [global,trim_all]),
            find_key(Hosts, KeyType, Key, Lines);
        _ ->
            false
    end.

find_key(Hosts, KeyType, Key, [Line|Lines]) ->
    case find_key_in_line(Hosts, KeyType, Key, binary:split(Line, <<" ">>, [global,trim_all])) of
        true ->
            true;
        false ->
            find_key(Hosts, KeyType, Key, Lines)
    end;
find_key(_, _, _, _) ->
    false.


find_key_in_line(_Hosts, _KeyType, _Key, [<<"#",_/binary>> |_]) ->
    false;
find_key_in_line(Hosts, KeyType, Key, [Patterns, KeyType, Base64EncodedKey, _Comment]) ->
    host_match(Hosts, Patterns) andalso
        Key == decode_key(Base64EncodedKey);
find_key_in_line(Hosts, KeyType, Key, [Patterns, KeyType, Base64EncodedKey]) ->
    host_match(Hosts, Patterns) andalso
        Key == decode_key(Base64EncodedKey);
find_key_in_line(Hosts, KeyType, Key, [_Option | [_,_,_|_]=Rest]) ->
    %% Dont care for options
    find_key_in_line(Hosts, KeyType, Key, Rest);
find_key_in_line(_, _, _, _) ->
    false.


host_match(Hosts, PatternsBin) ->
    Patterns = binary:split(PatternsBin, <<",">>, [global]),
    lists:any(fun(Pat) ->
                      lists:any(fun(Hst) ->
                                        Pat == Hst
                                end, Hosts)
              end, Patterns).

%%%---------------- COMMON FUNCTIONS ------------------------------

read_ssh_key_file(Role, PrivPub, Algorithm, Opts) ->
    File = file_name(Role, file_base_name(Role,Algorithm), Opts),
    Password = %% Pwd for Host Keys is an undocumented option and should not be used
        proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),

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
    {ok,Info} = file:read_file_info(UserDir),
    #file_info{mode=Mode} = Info,
    case (Mode band 8#777) of
	8#700 ->
	    ok;
	_Other ->
	    ok = file:change_mode(UserDir, 8#700)
    end,
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
