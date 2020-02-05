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
                      
is_auth_key(Key, User,Opts) ->
    case lookup_user_key(Key, User, Opts) of
	{ok, Key} ->
	    true;
	_ ->
	    false
    end.

%%%---------------- CLIENT API ------------------------------------
user_key(Algorithm, Opts) ->
    read_ssh_key_file(user, private, Algorithm, Opts).

is_host_key(Key, PeerName, Algorithm, Opts) ->
    case ssh_transport:valid_key_sha_alg(public, Key, Algorithm) of
        true ->
            case file:open(file_name(user, "known_hosts", Opts), [read, binary]) of
                {ok, Fd} ->
                    Res = is_host_key_fd(Fd, Key, replace_localhost(PeerName)),
                    file:close(Fd),
                    Res;
                _ ->
                    false
            end;
        false ->
            false
    end.


add_host_key(Host, Key, Opts) ->
    Host1 = add_ip(replace_localhost(Host)),
    KnownHosts = file_name(user, "known_hosts", Opts),
    case file:open(KnownHosts, [write,append]) of
   	{ok, Fd} ->
	    ok = file:change_mode(KnownHosts, 8#644),
            SshBin = public_key:ssh_encode([{Key, [{hostnames, [Host1]}]}], known_hosts),
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

%%% Try to find the User's public key Key in "authorized_keys" or "authorized_keys2"
lookup_user_key(Key, User, Opts) ->
    SshDir = ssh_dir({remoteuser,User}, Opts),
    case lookup_user_key_f(Key, User, SshDir, "authorized_keys", Opts) of
	{ok, Key} ->
	    {ok, Key};
	_ ->
	    lookup_user_key_f(Key, User, SshDir, "authorized_keys2", Opts)
    end.

lookup_user_key_f(_, _User, [], _F, _Opts) ->
    {error, nouserdir};
lookup_user_key_f(_, _User, nouserdir, _F, _Opts) ->
    {error, nouserdir};
lookup_user_key_f(Key, _User, Dir, F, _Opts) ->
    FileName = filename:join(Dir, F),
    case file:open(FileName, [read, binary]) of
	{ok, Fd} ->
	    Res = lookup_user_key_fd(Fd, Key),
	    file:close(Fd),
	    Res;
	{error, Reason} ->
	    {error, {{openerr, Reason}, {file, FileName}}}
    end.

lookup_user_key_fd(Fd, Key) ->
    case io:get_line(Fd, '') of
	eof ->
	    {error, not_found};
	{error,Error} ->
	    %% Rare... For example NFS errors
	    {error,Error};
	Line ->
	    try public_key:ssh_decode(Line, auth_keys)
            of
		[{Key, _}] ->
                    {ok, Key};
                _ ->
                    lookup_user_key_fd(Fd, Key)
            catch
                _:_ ->
                    []
	    end
    end.

%%%---------------- CLIENT FUNCTIONS ------------------------------

is_host_key_fd(Fd, KeyToMatch, Host) ->
    case io:get_line(Fd, '') of
	eof ->
	    false;
	{error,_} ->
	    %% Rare... For example NFS errors
	    false;
	Line ->
	    try public_key:ssh_decode(Line, known_hosts) of
		[{Key, Attributes}] when KeyToMatch == Key ->
                    HostList = proplists:get_value(hostnames, Attributes),
                    case lists:member(Host, HostList) of
                        true ->
                            true;
                        _ ->
                            is_host_key_fd(Fd, KeyToMatch, Host)
                    end;
		_ ->
		    is_host_key_fd(Fd, KeyToMatch, Host)
            catch
                _:_ ->
                    false
	    end
    end.


%%%--------------------------------
%% in: "host" out: "host,1.2.3.4.
add_ip(IP) when is_tuple(IP) ->
    ssh_connection:encode_ip(IP);
add_ip(Host) ->
    case inet:getaddr(Host, inet) of
	{ok, Addr} ->
	    case ssh_connection:encode_ip(Addr) of
		false -> Host;
		IPString -> Host ++ "," ++ IPString
	    end;
	_ -> Host
    end.

replace_localhost("localhost") ->
    {ok, Hostname} = inet:gethostname(),
    Hostname;
replace_localhost(Host) ->
    Host.

%%%---------------- COMMON FUNCTIONS ------------------------------

read_ssh_key_file(Role, PrivPub, Algorithm, Opts) ->
    File = file_name(Role, file_base_name(Role,Algorithm), Opts),
    Password = %% Pwd for Host Keys is an undocumented option and should not be used
        proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),

    case file:read_file(File) of
        {ok, Pem} ->
            try decode_ssh_file(Pem, Password) of
                Key ->
                    check_key_type(PrivPub, Key, Algorithm)
            catch
                throw:Reason ->
                    {error, Reason};
                error:Reason ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.


decode_ssh_file(Pem, Password) ->
    %% Private Key
    case public_key:pem_decode(Pem) of
        [{{no_asn1,new_openssh}, Bin, _}] ->
            public_key:ssh_decode(Bin, new_openssh);
        
        Other ->
            pem_entry_decode(Other, Password)
    end.


check_key_type(PrivPub, Key, Algorithm) ->
    case ssh_transport:valid_key_sha_alg(PrivPub, Key, Algorithm) of
        true -> {ok,Key};
        false -> {error,bad_keytype_in_file}
    end.


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
    

pem_entry_decode([{_, _, not_encrypted} = Entry], _Password) ->
    public_key:pem_entry_decode(Entry);
pem_entry_decode([Entry], Password) when Password =/= ignore ->
    public_key:pem_entry_decode(Entry, Password);
pem_entry_decode(_, _) ->
    throw("No pass phrase provided for private key file").

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
%%%################################################################
%%%################################################################
%%%################################################################

