%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-behaviour(ssh_server_key_api).
-behaviour(ssh_client_key_api).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-include("ssh.hrl").

-export([host_key/2,
	 user_key/2,
	 is_host_key/4,
	 add_host_key/3,
	 is_auth_key/3]).


-define(PERM_700, 8#700).
-define(PERM_644, 8#644).


%%% API

%%% client
-spec add_host_key(string(),
		   public_key:public_key(),
		   proplists:proplist()) -> ok | {error,term()}.

-spec is_host_key(public_key:public_key(),
		  string(),
		  ssh_client_key_api:algorithm(),
		  proplists:proplist()) -> boolean().

-spec user_key(ssh_client_key_api:algorithm(),
	       proplists:proplist()) -> {ok, public_key:private_key()} | {error,term()}.

%%% server
-spec host_key(ssh_server_key_api:algorithm(),
	       proplists:proplist()) ->  {ok, public_key:private_key()} | {error,term()}.

-spec is_auth_key(public_key:public_key(),
		  string(), proplists:proplist()) -> boolean().


%% Used by server
host_key(Algorithm, Opts) ->
    File = file_name(system, file_base_name(Algorithm), Opts),
    %% We do not expect host keys to have pass phrases
    %% so probably we could hardcod Password = ignore, but
    %% we keep it as an undocumented option for now.
    Password = proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),
    case decode(File, Password) of
	{ok,Key} ->
	    case ssh_transport:valid_key_sha_alg(Key,Algorithm) of
                true -> {ok,Key};
                false -> {error,bad_keytype_in_file}
	    end;
	{error,DecodeError} ->
            {error,DecodeError}
    end.

is_auth_key(Key, User,Opts) ->
    case lookup_user_key(Key, User, Opts) of
	{ok, Key} ->
	    true;
	_ ->
	    false
    end.


%% Used by client
is_host_key(Key, PeerName, Algorithm, Opts) ->
    case lookup_host_key(Key, PeerName, Algorithm, Opts) of
	{ok, Key} ->
	    true;
	_ ->
	    false
    end.

user_key(Algorithm, Opts) ->
    File = file_name(user, identity_key_filename(Algorithm), Opts),
    Password = proplists:get_value(identity_pass_phrase(Algorithm), Opts, ignore),
    decode(File, Password).


%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_base_name('ssh-rsa'            ) -> "ssh_host_rsa_key";
file_base_name('rsa-sha2-256'       ) -> "ssh_host_rsa_key";
file_base_name('rsa-sha2-384'       ) -> "ssh_host_rsa_key";
file_base_name('rsa-sha2-512'       ) -> "ssh_host_rsa_key";
file_base_name('ssh-dss'            ) -> "ssh_host_dsa_key";
file_base_name('ecdsa-sha2-nistp256') -> "ssh_host_ecdsa_key";
file_base_name('ecdsa-sha2-nistp384') -> "ssh_host_ecdsa_key";
file_base_name('ecdsa-sha2-nistp521') -> "ssh_host_ecdsa_key";
file_base_name(_                    ) -> "ssh_host_key".

decode(File, Password) ->
    try {ok, decode_ssh_file(read_ssh_file(File), Password)}
    catch 
	throw:Reason ->
	    {error, Reason};
	error:Reason ->
	    {error, Reason}
    end.      

read_ssh_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%% Public key
decode_ssh_file(SshBin, public_key) ->
    public_key:ssh_decode(SshBin, public_key);

%% Private Key
decode_ssh_file(Pem, Password) ->
    case public_key:pem_decode(Pem) of
	[{_, _, not_encrypted} = Entry]  -> 
	    public_key:pem_entry_decode(Entry);
	[Entry] when Password =/= ignore ->
	    public_key:pem_entry_decode(Entry, Password);
	 _ ->
	    throw("No pass phrase provided for private key file")
    end.
    

%% lookup_host_key
%% return {ok, Key(s)} or {error, not_found}
%%

lookup_host_key(KeyToMatch, Host, Alg, Opts) ->
    Host1 = replace_localhost(Host),
    do_lookup_host_key(KeyToMatch, Host1, Alg, Opts).
	    

add_host_key(Host, Key, Opts) ->
    Host1 = add_ip(replace_localhost(Host)),
    KnownHosts = file_name(user, "known_hosts", Opts),
    case file:open(KnownHosts, [write,append]) of
   	{ok, Fd} ->
	    ok = file:change_mode(KnownHosts, ?PERM_644),
   	    Res = add_key_fd(Fd, Host1, Key),
   	    file:close(Fd),
   	    Res;
   	Error ->
   	    Error
    end.

lookup_user_key(Key, User, Opts) ->
    SshDir = ssh_dir({remoteuser,User}, Opts),
    case lookup_user_key_f(Key, User, SshDir, "authorized_keys", Opts) of
	{ok, Key} ->
	    {ok, Key};
	_ ->
	    lookup_user_key_f(Key, User, SshDir, "authorized_keys2", Opts)
    end.


%%
%% Utils
%%

%% server use this to find individual keys for
%% an individual user when user tries to login
%% with publickey
ssh_dir({remoteuser, User}, Opts) ->
    case proplists:get_value(user_dir_fun, Opts) of
	undefined ->
	    case proplists:get_value(user_dir, Opts, false) of
		false ->
		    default_user_dir();
		Dir ->
		    Dir
	    end;
	FUN ->
	    FUN(User)
    end;

%% client use this to find client ssh keys
ssh_dir(user, Opts) ->
    case proplists:get_value(user_dir, Opts, false) of
	false -> default_user_dir();
	D -> D
    end;

%% server use this to find server host keys
ssh_dir(system, Opts) ->
    proplists:get_value(system_dir, Opts, "/etc/ssh").


file_name(Type, Name, Opts) ->
    FN = filename:join(ssh_dir(Type, Opts), Name),
    FN.



%% in: "host" out: "host,1.2.3.4.
add_ip(IP) when is_tuple(IP) ->
    ssh_connection:encode_ip(IP);
add_ip(Host)                                                             ->
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

do_lookup_host_key(KeyToMatch, Host, Alg, Opts) ->
    case file:open(file_name(user, "known_hosts", Opts), [read, binary]) of
	{ok, Fd} ->
	    Res = lookup_host_key_fd(Fd, KeyToMatch, Host, Alg),
	    file:close(Fd),
	    Res;
	{error, enoent} ->
	    {error, not_found};
	Error ->
	    Error
    end.

identity_key_filename('ssh-dss'            ) -> "id_dsa";
identity_key_filename('ssh-rsa'            ) -> "id_rsa";
identity_key_filename('rsa-sha2-256'       ) -> "id_rsa";
identity_key_filename('rsa-sha2-384'       ) -> "id_rsa";
identity_key_filename('rsa-sha2-512'       ) -> "id_rsa";
identity_key_filename('ecdsa-sha2-nistp256') -> "id_ecdsa";
identity_key_filename('ecdsa-sha2-nistp384') -> "id_ecdsa";
identity_key_filename('ecdsa-sha2-nistp521') -> "id_ecdsa".

identity_pass_phrase("ssh-dss"       ) -> dsa_pass_phrase;
identity_pass_phrase("ssh-rsa"       ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-256"  ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-384"  ) -> rsa_pass_phrase;
identity_pass_phrase("rsa-sha2-512"  ) -> rsa_pass_phrase;
identity_pass_phrase("ecdsa-sha2-"++_) -> ecdsa_pass_phrase;
identity_pass_phrase(P) when is_atom(P) -> 
    identity_pass_phrase(atom_to_list(P)).
    
lookup_host_key_fd(Fd, KeyToMatch, Host, KeyType) ->
    case io:get_line(Fd, '') of
	eof ->
	    {error, not_found};
	{error,Error} ->
	    %% Rare... For example NFS errors
	    {error,Error};
	Line ->
	    case ssh_decode_line(Line, known_hosts) of
		[{Key, Attributes}] ->
		    handle_host(Fd, KeyToMatch, Host, proplists:get_value(hostnames, Attributes), Key, KeyType);
		[] ->
		    lookup_host_key_fd(Fd, KeyToMatch, Host, KeyType)
	    end
    end.

ssh_decode_line(Line, Type) ->
    try 
	public_key:ssh_decode(Line, Type) 
    catch _:_ ->
	    []
    end.

handle_host(Fd, KeyToMatch, Host, HostList, Key, KeyType) ->
    Host1 = host_name(Host),
    case lists:member(Host1, HostList) andalso key_match(Key, KeyType) of
	true when KeyToMatch == Key ->
	    {ok,Key};
	_ ->
	    lookup_host_key_fd(Fd, KeyToMatch, Host, KeyType)
    end.

host_name(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
host_name(List) ->
    List.

key_match(#'RSAPublicKey'{}, 'ssh-rsa') ->
    true;
key_match({_, #'Dss-Parms'{}}, 'ssh-dss') ->
    true;
key_match({#'ECPoint'{},{namedCurve,Curve}}, Alg) ->
    case atom_to_list(Alg) of
	"ecdsa-sha2-"++IdS ->
	    Curve == public_key:ssh_curvename2oid(list_to_binary(IdS));
	_ ->
	    false
    end;
key_match(_, _) ->
    false.

add_key_fd(Fd, Host,Key) ->
    SshBin = public_key:ssh_encode([{Key, [{hostnames, [Host]}]}], known_hosts),
    file:write(Fd, SshBin).

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
	    case ssh_decode_line(Line, auth_keys) of
		[{AuthKey, _}] ->
		    case is_auth_key(Key, AuthKey) of
			true ->
			    {ok, Key};
			false ->
			    lookup_user_key_fd(Fd, Key)
		    end;
		[] ->
		    lookup_user_key_fd(Fd, Key)
	    end
    end.

is_auth_key(Key, Key) -> 
    true;
is_auth_key(_,_) -> 
    false.


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
	?PERM_700 ->
	    ok;
	_Other ->
	    ok = file:change_mode(UserDir, ?PERM_700)
    end,
    UserDir.
