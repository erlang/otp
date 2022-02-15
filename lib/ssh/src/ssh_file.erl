%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2022. All Rights Reserved.
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

%% experimental:
-export([decode_ssh_file/4
        ]).

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

%%%--------------------- utility exports ---------------------------
-export([decode/2, encode/2]).

-define(ENCODED_LINE_LENGTH, 68).

%%%--------------------- common exports ---------------------------
-export_type([user_dir_common_option/0,
              user_dir_fun_common_option/0
             ]).

-type user_dir_common_option()     :: {user_dir,  string()}.
-type user_dir_fun_common_option() :: {user_dir_fun, user2dir()}.
-type user2dir() :: fun((RemoteUserName::string()) -> UserDir :: string()) .

-type optimize_key_lookup() :: {optimize, time|space} .

-type key() :: public_key:public_key() | public_key:private_key() .
-type experimental_openssh_key_v1() :: [{key(), openssh_key_v1_attributes()}].
-type openssh_key_v1_attributes() :: [{atom(),term()}].

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
                iolist_to_binary([lists:join(",", Hosts1), " ",
                                  KeyType," ",base64:encode(iolist_to_binary(EncKey)),
                                  "\n"]),
            Res = file:write(Fd, SshBin),
	    file:close(Fd),
	    Res;
	{error,Error} ->
	    {error,{add_host_key,Error}}
    end.

%%%---------------- UTILITY API -----------------------------------
%%% In public key before OTP-24.0 as ssh_decode/2 and ssh_encode/2

-spec decode(SshBin, Type) -> Decoded | {error,term()}
                                  when SshBin :: binary(),
                                       Type :: ssh2_pubkey
                                             | public_key
                                             | openssh_key
                                             | rfc4716_key
                                             | openssh_key_v1  % Experimental
                                             | known_hosts
                                             | auth_keys,
                                       Decoded :: Decoded_ssh2_pubkey
                                                | Decoded_public
                                                | Decoded_openssh
                                                | Decoded_rfc4716
                                                | Decoded_openssh_key_v1
                                                | Decoded_known_hosts
                                                | Decoded_auth_keys,

                                       Decoded_ssh2_pubkey :: public_key:public_key(),
                                       Decoded_public :: Decoded_rfc4716
                                                       | Decoded_openssh_key_v1
                                                       | Decoded_openssh,
                                       Decoded_openssh :: [{public_key:public_key(), [{comment,string()}]}],
                                       Decoded_rfc4716 :: [{key(), [{headers,Attrs}]}],
                                       Decoded_openssh_key_v1 :: experimental_openssh_key_v1(),
                                       Decoded_known_hosts :: [{public_key:public_key(), [{comment,string()}
                                                                                          | {hostnames,[string()]}]}],
                                       Decoded_auth_keys :: [{public_key:public_key(), [{comment,string()}
                                                                                        | {options,[string()]}]}],
                                       Attrs :: {Key::string(), Value::string()} .

decode(KeyBin, ssh2_pubkey) when is_binary(KeyBin) ->
    ssh_message:ssh2_pubkey_decode(KeyBin);

decode(KeyBin, public_key) when is_binary(KeyBin) ->
    Type = case KeyBin of
               <<"-----BEGIN OPENSSH",_/binary>> -> openssh_key_v1;
               <<"----",_/binary>> -> rfc4716_key;
               _ -> openssh_key
           end,
    decode(KeyBin, Type);

decode(KeyBin, Type) when is_binary(KeyBin) andalso 
                          (Type==rfc4716_key orelse
                           Type==openssh_key_v1 % Experimental
                          ) ->
    %% Ex: <<"---- BEGIN SSH2 PUBLIC KEY ----\n....">>     (rfc4716_key)
    %%     <<"-----BEGIN OPENSSH PRIVATE KEY-----\n....">> (openssh_key_v1)
    case decode_ssh_file(public, any, KeyBin, ignore) of
        {ok,Keys} ->
            [{Key,
              if
                  Attrs =/= [] ->
                      [{headers, [{binary_to_list(K),binary_to_list(V)} || {K,V} <- Attrs]}];
                  Attrs == [] ->
                      []
              end
             }
             || {Key,Attrs} <- Keys];

        {error,Error} ->
            {error,Error}
    end;

decode(KeyBin0, openssh_key) when is_binary(KeyBin0) ->
    %% Ex: <<"ssh-rsa AAAAB12....3BC someone@example.com">>
    try
        [begin
             [_,K|Rest] = binary:split(Line, <<" ">>, [global,trim_all]),
             Key = ssh_message:ssh2_pubkey_decode(base64:decode(K)),
             case Rest of
                 [Comment] -> {Key, [{comment,binary_to_list(Comment)}]};
                 [] -> {Key,[]}
             end
         end || Line <- split_in_nonempty_lines(KeyBin0)
        ]
    catch
        _:_ -> {error, key_decode_failed}
    end;

decode(Bin, known_hosts) when is_binary(Bin) ->
    [begin
         Attrs = 
             [
              {comment, binary_to_list(erlang:iolist_to_binary(lists:join(" ", Comment)))}
              || Comment =/= []
             ] ++
             [
              {hostnames,
               [binary_to_list(HP)
                || HP <- binary:split(HostPort,<<",">>,[global,trim_all])
               ]}
             ],
         {ssh_message:ssh2_pubkey_decode(base64:decode(KeyBin)),
          Attrs
         }
     end
     || L <- split_in_nonempty_lines(Bin),
        [HostPort,_KeyType,KeyBin|Comment] <- [binary:split(L,<<" ">>,[global,trim_all])]
    ];

decode(Bin, auth_keys) when is_binary(Bin) ->
    [begin
         Attrs = 
             [
              {comment, binary_to_list(erlang:iolist_to_binary(lists:join(" ", Comment)))}
              || Comment =/= []
             ] ++
             [
              {options, lists:map(fun erlang:binary_to_list/1, Options)}
              || Options =/= []
             ],
         {ssh_message:ssh2_pubkey_decode(base64:decode(KeyBin)),
          Attrs
         }
     end
     || L <- split_in_nonempty_lines(Bin),
        [Options,_KeyType,KeyBin|Comment] <-
            case binary:match(L, [<<"ssh-rsa">>,
                                  <<"rsa-sha2-">>,
                                  <<"ssh-dss">>,
                                  <<"ecdsa-sha2-nistp">>,
                                  <<"ssh-ed">>
                                 ]) of
                nomatch ->
                    [];
                {0, Len} when is_integer(Len) ->
                    [ [[] | binary:split(L,<<" ">>,[global,trim_all])] ];
                {Pos,Len} when is_integer(Pos), is_integer(Len) ->
                    [ [binary:split(binary:part(L,0,Pos-1), <<",">>,[global,trim_all]) |
                       binary:split(binary:part(L,Pos,size(L)-Pos), <<" ">>, [global,trim_all])]
                    ]
            end
    ];

decode(_KeyBin, _Type) ->
    error(badarg).

%%%----------------------------------------------------------------
-spec encode(InData, Type) -> binary() | {error,term()}
                                  when Type :: ssh2_pubkey
                                             | openssh_key
                                             | rfc4716_key
                                             | openssh_key_v1  % Experimental
                                             | known_hosts
                                             | auth_keys,
                                       InData :: InData_ssh2_pubkey
                                               | InData_openssh
                                               | InData_rfc4716
                                               | InData_openssh_key_v1
                                               | InData_known_hosts
                                               | InData_auth_keys,

                                       InData_ssh2_pubkey :: public_key:public_key(),
                                       InData_openssh :: [{public_key:public_key(), [{comment,string()}]}],
                                       InData_rfc4716 :: [{key(), [{headers,Attrs}]}],
                                       InData_openssh_key_v1 :: experimental_openssh_key_v1(),
                                       InData_known_hosts :: [{public_key:public_key(), [{comment,string()}
                                                                                          | {hostnames,[string()]}]}],
                                       InData_auth_keys :: [{public_key:public_key(), [{comment,string()}
                                                                                        | {options,[string()]}]}],
                                       Attrs :: {Key::string(), Value::string()} .

encode(Key, ssh2_pubkey) ->
    ssh_message:ssh2_pubkey_encode(Key);

encode(KeyAttrs, Type) when Type==rfc4716_key ;
                            Type==openssh_key_v1 % Experimental
                            ->
    {Begin, End, F} =
        case Type of
            rfc4716_key ->
                {"---- BEGIN SSH2 PUBLIC KEY ----\n",
                 "---- END SSH2 PUBLIC KEY ----\n",
                 fun ssh_message:ssh2_pubkey_encode/1};
            openssh_key_v1 ->
                {"-----BEGIN OPENSSH PRIVATE KEY-----\n",
                 "-----END OPENSSH PRIVATE KEY-----\n",
                 fun openssh_key_v1_encode/1}
        end,
    iolist_to_binary(
      [
       [Begin,
        [rfc4716_encode_header(H) || H <- proplists:get_value(headers, Attrs, [])],
        split_long_lines( base64:encode( F(Key) ) ),
        "\n",
        End
       ] ||
          {Key,Attrs} <- KeyAttrs
      ]
     );

encode(KeyAttrs, Type) when Type == known_hosts;
                            Type == auth_keys ;
                            Type == openssh_key ->
    FirstArgTag =
        case Type of
            known_hosts -> hostnames;
            auth_keys -> options;
            openssh_key -> '*no tag*'
        end,
    iolist_to_binary(
      [
       begin
           <<?DEC_BIN(KeyType,__0),_/binary>> = Enc = ssh_message:ssh2_pubkey_encode(Key),
           [case lists:join(",", proplists:get_value(FirstArgTag, Attributes, [])) of
                [] -> "";
                C -> [C," "]
            end,
            KeyType, " ",
            base64:encode(Enc), " ",
            case proplists:get_value(comment, Attributes, []) of
                [] -> "";
                C -> C
            end,
            "\n"
           ]
       end
       || {Key,Attributes} <- KeyAttrs
      ]
     );

encode(_KeyBin, _Type) ->
    error(badarg).

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
                    Lines = split_in_lines(Bin),
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
            case split_in_lines(Line0) of % remove trailing EOL
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
                    Lines = split_in_lines(Bin),
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
            %% Very likely to be a revoked key,
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

%%%---------------- UTILITY ---------------------------------------
rfc4716_encode_header({Tag, Value}) ->
    TagLen = length(Tag),
    ValueLen = length(Value),
    case TagLen + 1 + ValueLen of
	N when N > ?ENCODED_LINE_LENGTH ->
	    NumOfChars =  ?ENCODED_LINE_LENGTH - (TagLen + 1),
	    {First, Rest} = lists:split(NumOfChars, Value),
	    [Tag,": " , First, [$\\], "\n", rfc4716_encode_value(Rest) , "\n"];
	_ ->
	    [Tag, ": ", Value, "\n"]
    end.

rfc4716_encode_value(Value) ->
    case length(Value) of
	N when N > ?ENCODED_LINE_LENGTH ->
	    {First, Rest} = lists:split(?ENCODED_LINE_LENGTH, Value),
	    [First, [$\\], "\n", rfc4716_encode_value(Rest)];
	_ ->
	    Value
    end.

split_long_lines(<<Text:?ENCODED_LINE_LENGTH/binary, Rest/binary>>) when Rest =/= <<"">> ->
    [Text, $\n | split_long_lines(Rest)];
split_long_lines(Bin) ->
    [Bin].

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
            of
                {ok, [{Key,_Attrs}|_Keys]} ->
                    {ok,Key};
                {error, Reason} ->
                    {error, Reason}
            catch
                throw:Reason ->
                    {error, Reason};
                error:Reason ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.


-spec decode_ssh_file(PrivPub, Algorithm, Pem, Password) -> Result when
      PrivPub :: private | public,
      Algorithm :: ssh:pubkey_alg() | any,
      Pem :: binary(),
      Password :: string() | ignore,
      Result :: {ok, Keys} | {error, any()},
      Keys :: [{Key,Attrs}],
      Attrs :: [{any(),any()}],
      Key :: public_key:private_key() | public_key:public_key() .

decode_ssh_file(PrivPub, Algorithm, Pem, Password) ->
    try decode_pem_keys(Pem, Password)
    of
        {ok, Keys} when Algorithm == any ->
            {ok, Keys};

        {ok, Keys0} ->
            case [{Key,Attrs} || {Key,Attrs} <- Keys0,
                                 ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm)] of
                [] ->
                    {error,no_key_found};
                Keys ->
                    {ok,Keys}
            end;

        {error,Error} ->
            {error,Error}

    catch
        _:_ ->
            {error, key_decode_failed}
    end.


decode_pem_keys(RawBin, Password) ->
    PemLines = split_in_lines(
                 binary:replace(RawBin, [<<"\\\n">>,<<"\\\r\\\n">>],  <<"">>, [global])
                ),
    decode_pem_keys(PemLines, Password, []).
decode_pem_keys([], _, Acc) ->
    {ok,lists:reverse(Acc)};


decode_pem_keys(PemLines, Password, Acc) ->
    %% Private Key
    try get_key_part(PemLines) of
        {'openssh-key-v1', Bin, Attrs, RestLines} ->
            %% -----BEGIN OPENSSH PRIVATE KEY-----
            %% Holds both public and private keys
            KeyPairs = openssh_key_v1_decode(Bin, Password),
            Keys = [{Key,Attrs} || {Pub,Priv} <- KeyPairs,
                                   Key <- [Pub,Priv]],
            decode_pem_keys(RestLines, Password, Keys ++ Acc);

        {rfc4716, Bin, Attrs, RestLines} ->
            %% ---- BEGIN SSH2 PUBLIC KEY ----
            %% rfc4716 only defines public keys
            Key = ssh_message:ssh2_pubkey_decode(Bin),
            decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

        {Type, Bin, Attrs, RestLines} ->
            %% -----BEGIN (RSA|DSA|EC) PRIVATE KEY-----
            %% and possibly others
            case get_encrypt_hdrs(Attrs) of
                not_encrypted ->
                    Key = public_key:pem_entry_decode({Type,Bin,not_encrypted}),
                    decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

                [Cipher,Salt] when is_binary(Cipher),
                                   is_binary(Salt),
                                   Password =/= ignore ->
                    CryptInfo =
                        {binary_to_list(Cipher), unhex(binary_to_list(Salt))},
                    Key = public_key:pem_entry_decode({Type,Bin,CryptInfo}, Password),
                    decode_pem_keys(RestLines, Password, [{Key,Attrs}|Acc]);

                _X ->
                    {error, no_pass_phrase}
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
get_key_part([<<"---- BEGIN SSH2 PUBLIC KEY ----">> | Lines0]) ->
    %% RFC 4716 format
    {KeyValues,Lines} = get_hdr_lines(Lines0, []),
    ExpectedEndLine = <<"---- END SSH2 PUBLIC KEY ----">>,
    {Key,RestLines} = get_body(Lines,ExpectedEndLine),
    {rfc4716, Key, KeyValues, RestLines};

get_key_part([<<"-----BEGIN ", Rest/binary>> | Lines0]) ->
    %% PEM format
    ExpectedEndLine = <<"-----END ",Rest/binary>>,
    [MiddlePart, <<>>] = binary:split(Rest,  <<" KEY-----">>),
    {KeyValues,Lines} = get_hdr_lines(Lines0, []),
    {Key,RestLines} = get_body(Lines,ExpectedEndLine),
    {asn1_type(MiddlePart), Key, KeyValues, RestLines}.


get_hdr_lines(Lines, Acc) ->
    Line1 = hd(Lines),
    case binary:split(Line1, <<":">>) of
        [Line1] ->
            {lists:reverse(Acc), Lines};
        [Key,Value] ->
            get_hdr_lines(tl(Lines), [{trim(Key),trim(Value)}|Acc])
    end.


get_body(Lines, ExpectedEndLine) ->
    {KeyPart, [ExpectedEndLine|RestLines]} = 
        lists:splitwith(fun(L) -> L=/=ExpectedEndLine end, Lines),
    {base64:mime_decode(iolist_to_binary(KeyPart)), RestLines}.

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

openssh_key_v1_decode(<<"openssh-key-v1",0,
                        ?DEC_BIN(CipherName, _L1),
                        ?DEC_BIN(KdfName, _L2),
                        ?DEC_BIN(KdfOptions, _L3),
                        ?UINT32(N),      % number of keys
                        Rest/binary
                      >>, Pwd) ->
    openssh_key_v1_decode(Rest, N, Pwd, CipherName, KdfName, KdfOptions, N, []).


openssh_key_v1_decode(<<?DEC_BIN(BinKey,_L1), Rest/binary>>, I,
                      Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAcc) when I>0 ->
    PublicKey = ssh_message:ssh2_pubkey_decode(BinKey),
    openssh_key_v1_decode(Rest, I-1, Pwd, CipherName, KdfName, KdfOptions, N, [PublicKey|PubKeyAcc]);

openssh_key_v1_decode(<<?DEC_BIN(Encrypted,_L)>>,
                      0, Pwd, CipherName, KdfName, KdfOptions, N, PubKeyAccRev) ->
    PubKeys = lists:reverse(PubKeyAccRev),
    try
        Plain = decrypt_openssh_key_v1(Encrypted, KdfName, KdfOptions, CipherName, Pwd),
        openssh_key_v1_decode_priv_keys(Plain, N, N, [], [])
    of
        {PrivKeys, _Comments} ->
            lists:zip(PubKeys, PrivKeys)
            %% lists:zip3(PubKeys, PrivKeys,_ Comments))
    catch
        error:{decryption, DecryptError} ->
            error({decryption, DecryptError})
    end.


openssh_key_v1_decode_priv_keys(Bin, I, N, KeyAcc, CmntAcc) when I>0 ->
    {PrivKey, <<?DEC_BIN(Comment,_Lc),Rest/binary>>} = ssh_message:ssh2_privkey_decode2(Bin),
    openssh_key_v1_decode_priv_keys(Rest, I-1, N, [PrivKey|KeyAcc], [Comment|CmntAcc]);
openssh_key_v1_decode_priv_keys(_Padding, 0, _N, PrivKeyAccRev, CommentAccRev) ->
    {lists:reverse(PrivKeyAccRev),
     lists:reverse(CommentAccRev)}.


decrypt_openssh_key_v1(Encrypted, <<"none">>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_openssh_key_v1(Encrypted, <<>>, <<>>, _CipherName, _Pwd) ->
    check_valid_decryption(Encrypted, ?NON_CRYPT_BLOCKSIZE);
decrypt_openssh_key_v1(_Encrypted, <<"bcrypt">>, <<?DEC_BIN(_Salt,_L),?UINT32(_Rounds)>>, _CipherName, _Pwd) ->
    error({decryption, {not_supported,bcrypt}});
decrypt_openssh_key_v1(_Encrypted, KdfName, _KdfOpts, _CipherName, _Pwd) ->
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

%%%----------------------------------------------------------------
%% KeyPairs :: [ {Pub,Priv,Comment} ]
openssh_key_v1_encode(KeyPairs) ->
    CipherName = <<"none">>,
    BlockSize = ?NON_CRYPT_BLOCKSIZE, % Cipher dependent
    KdfName = <<"none">>,
    KdfOptions = <<>>,
    NumKeys = length(KeyPairs),
    CheckInt = crypto:strong_rand_bytes(4),
    UnEncrypted0 = <<CheckInt/binary,
                     CheckInt/binary,
                     (openssh_key_v1_encode_priv_keys_cmnts(KeyPairs))/binary>>,
    UnEncrypted = <<UnEncrypted0/binary,
                    (pad(size(UnEncrypted0), BlockSize))/binary>>,
    Encrypted = encrypt_openssh_key_v1(UnEncrypted,  KdfName, KdfOptions, CipherName, ignore),
    <<"openssh-key-v1",0,
      ?STRING(CipherName),
      ?STRING(KdfName),
      ?STRING(KdfOptions),
      ?UINT32(NumKeys),
      (openssh_key_v1_encode_pub_keys(KeyPairs))/binary,
      ?STRING(Encrypted)>>.

%%%----
openssh_key_v1_encode_pub_keys(KeyPairs) ->
    openssh_key_v1_encode_pub_keys(KeyPairs, []).

openssh_key_v1_encode_pub_keys([{Priv = #'ECPrivateKey'{}, _Cmnt} | Ks], Acc) ->
    Pub = ssh_transport:extract_public_key(Priv),
    Bk = ssh_message:ssh2_pubkey_encode(Pub),
    openssh_key_v1_encode_pub_keys(Ks, [<<?STRING(Bk)>>|Acc]);
openssh_key_v1_encode_pub_keys([{K,_,_C}|Ks], Acc) ->
    Bk = ssh_message:ssh2_pubkey_encode(K),
    openssh_key_v1_encode_pub_keys(Ks, [<<?STRING(Bk)>>|Acc]);
openssh_key_v1_encode_pub_keys([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


%%%----
openssh_key_v1_encode_priv_keys_cmnts(KeyPairs) ->
    openssh_key_v1_encode_priv_keys_cmnts(KeyPairs, []).

openssh_key_v1_encode_priv_keys_cmnts([{K = #'ECPrivateKey'{}, C} | Ks], Acc) ->
    Bk = ssh_message:ssh2_privkey_encode(K),
    openssh_key_v1_encode_priv_keys_cmnts(Ks, [<<Bk/binary,?STRING(C)>>|Acc]);
openssh_key_v1_encode_priv_keys_cmnts([{_,K,C}|Ks], Acc) ->
    Bk = ssh_message:ssh2_privkey_encode(K),
    openssh_key_v1_encode_priv_keys_cmnts(Ks, [<<Bk/binary,?STRING(C)>>|Acc]);
openssh_key_v1_encode_priv_keys_cmnts([], Acc) ->
    list_to_binary(lists:reverse(Acc)).

encrypt_openssh_key_v1(UnEncrypted, <<"none">>, <<>>, _CipherName, _Pwd) ->
    UnEncrypted;
encrypt_openssh_key_v1(_UnEncrypted,  KdfName, _KdfOptions, _CipherName, _Pwd) ->
    error({decryption, {not_supported,KdfName}}).

pad(N, BlockSize) when N>BlockSize -> pad(N rem BlockSize, BlockSize);
pad(N, BlockSize) -> list_to_binary(lists:seq(1,BlockSize-N)).

%%%================================================================
%%%
split_in_nonempty_lines(Bin) ->
    skip_blank_lines_and_comments( split_in_lines(Bin) ).

split_in_lines(Bin) ->
    binary:split(Bin, [<<"\n">>,<<"\r\n">>], [global,trim_all]).

skip_blank_lines_and_comments(Lines) ->
    lists:filter(fun(<<"#",_/binary>>) ->
                         %% skip comments
                         false;
                    (L) ->
                         %% skip blank lines
                         re:run(L, "^(\t|\s)+$") == nomatch
                 end, Lines).
