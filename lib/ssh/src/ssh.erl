%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-module(ssh).

-include("ssh.hrl").
-include("ssh_connect.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

-export([start/0, start/1, stop/0,
	 connect/2, connect/3, connect/4,
	 close/1, connection_info/2,
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 daemon_info/1,
	 default_algorithms/0,
	 stop_listener/1, stop_listener/2,  stop_listener/3,
	 stop_daemon/1, stop_daemon/2, stop_daemon/3,
	 shell/1, shell/2, shell/3
	]).

%%% Type exports
-export_type([connection_ref/0,
	      channel_id/0
	     ]).

%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
-spec start(permanent | transient | temporary) -> ok | {error, term()}.
%%
%% Description: Starts the ssh application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssh).

start(Type) ->
    application:start(crypto, Type),
    application:start(asn1),
    application:start(public_key, Type),
    application:start(ssh, Type).

%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
%%
%% Description: Stops the ssh application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
-spec connect(port(), proplists:proplist()) -> {ok, pid()} |  {error, term()}.

-spec connect(port(),   proplists:proplist(), timeout()) -> {ok, pid()} |  {error, term()}
           ; (string(), integer(), proplists:proplist()) -> {ok, pid()} |  {error, term()}.

-spec connect(string(), integer(), proplists:proplist(), timeout()) -> {ok, pid()} |  {error, term()}.
%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
connect(Socket, Options) ->
    connect(Socket, Options, infinity).

connect(Socket, Options, Timeout) when is_port(Socket) ->
    case handle_options(Options) of
	{error, Error} ->
	    {error, Error};
	{_SocketOptions, SshOptions} ->
	    case valid_socket_to_use(Socket, Options) of
		ok -> 
		    {ok, {Host,_Port}} = inet:sockname(Socket),
		    Opts =  [{user_pid,self()}, {host,fmt_host(Host)} | SshOptions],
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error,SockError} ->
		    {error,SockError}
	    end
    end;

connect(Host, Port, Options) when is_integer(Port), Port>0 ->
    connect(Host, Port, Options, infinity).

connect(Host, Port, Options, Timeout) ->
    case handle_options(Options) of
	{error, _Reason} = Error ->
	    Error;
	{SocketOptions, SshOptions} ->
	    {_, Transport, _} = TransportOpts =
		proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}),
	    ConnectionTimeout = proplists:get_value(connect_timeout, Options, infinity),
	    try Transport:connect(Host, Port,  [ {active, false} | SocketOptions], ConnectionTimeout) of
		{ok, Socket} ->
		    Opts =  [{user_pid,self()}, {host,Host} | SshOptions],
		    ssh_connection_handler:start_connection(client, Socket, Opts, Timeout);
		{error, Reason} ->
		    {error, Reason}
	    catch
		exit:{function_clause, _} ->
		    {error, {options, {transport, TransportOpts}}};
		exit:badarg ->
		    {error, {options, {socket_options, SocketOptions}}}
	    end
    end.

%%--------------------------------------------------------------------
-spec close(pid()) -> ok.
%%
%% Description: Closes an ssh connection.
%%--------------------------------------------------------------------	
close(ConnectionRef) ->
    ssh_connection_handler:stop(ConnectionRef).

%%--------------------------------------------------------------------
-spec connection_info(pid(), [atom()]) -> [{atom(), term()}].
%%
%% Description: Retrieves information about a connection. 
%%--------------------------------------------------------------------	
connection_info(ConnectionRef, Options) ->
    ssh_connection_handler:connection_info(ConnectionRef, Options).

%%--------------------------------------------------------------------
-spec channel_info(pid(), channel_id(), [atom()]) -> [{atom(), term()}].
%%
%% Description: Retrieves information about a connection. 
%%--------------------------------------------------------------------	
channel_info(ConnectionRef, ChannelId, Options) ->
    ssh_connection_handler:channel_info(ConnectionRef, ChannelId, Options).

%%--------------------------------------------------------------------
-spec daemon(integer()) -> {ok, pid()} | {error, term()}.
-spec daemon(integer()|port(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
-spec daemon(any | inet:ip_address(), integer(), proplists:proplist()) -> {ok, pid()} | {error, term()}.

%% Description: Starts a server listening for SSH connections 
%% on the given port.
%%--------------------------------------------------------------------	
daemon(Port) ->
    daemon(Port, []).

daemon(Port, Options) when is_integer(Port) ->
    daemon(any, Port, Options);

daemon(Socket, Options0) when is_port(Socket) ->
    Options = daemon_shell_opt(Options0),
    start_daemon(Socket, Options).

daemon(HostAddr, Port, Options0) ->
    Options1 = daemon_shell_opt(Options0),
    {Host, Inet, Options} = daemon_host_inet_opt(HostAddr, Options1),
    start_daemon(Host, Port, Options, Inet).

%%--------------------------------------------------------------------
daemon_info(Pid) ->
    case catch ssh_system_sup:acceptor_supervisor(Pid) of
	AsupPid when is_pid(AsupPid) ->
	    [Port] =
		[Prt || {{ssh_acceptor_sup,any,Prt,default},
			 _WorkerPid,worker,[ssh_acceptor]} <- supervisor:which_children(AsupPid)],
	    {ok, [{port,Port}]};

	_ ->
	    {error,bad_daemon_ref}
    end.

%%--------------------------------------------------------------------
-spec stop_listener(pid()) -> ok.
-spec stop_listener(inet:ip_address(), integer()) -> ok.
%%
%% Description: Stops the listener, but leaves 
%% existing connections started by the listener up and running.
%%--------------------------------------------------------------------	
stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).
stop_listener(Address, Port) ->
    stop_listener(Address, Port, ?DEFAULT_PROFILE).
stop_listener(Address, Port, Profile) ->
    ssh_system_sup:stop_listener(Address, Port, Profile).

%%--------------------------------------------------------------------
-spec stop_daemon(pid()) -> ok.
-spec stop_daemon(inet:ip_address(), integer()) -> ok.
%%
%% Description: Stops the listener and all connections started by 
%% the listener.
%%--------------------------------------------------------------------	
stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).
stop_daemon(Address, Port) ->
    ssh_system_sup:stop_system(Address, Port, ?DEFAULT_PROFILE).
stop_daemon(Address, Port, Profile) ->
    ssh_system_sup:stop_system(Address, Port, Profile).
%%--------------------------------------------------------------------
-spec shell(port() | string()) ->  _.
-spec shell(port() | string(), proplists:proplist()) ->  _.
-spec shell(string(), integer(), proplists:proplist()) ->  _.

%%   Host = string()
%%   Port = integer()
%%   Options = [{Option, Value}]
%%
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
shell(Socket) when is_port(Socket) ->
    shell(Socket, []);
shell(Host) ->
    shell(Host, ?SSH_DEFAULT_PORT, []).

shell(Socket, Options) when is_port(Socket) ->
    start_shell( connect(Socket, Options) );
shell(Host, Options) ->
    shell(Host, ?SSH_DEFAULT_PORT, Options).

shell(Host, Port, Options) ->
    start_shell( connect(Host, Port, Options) ).


start_shell({ok, ConnectionRef}) ->
    case ssh_connection:session_channel(ConnectionRef, infinity) of
	{ok,ChannelId}  ->
	    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
	    Args = [{channel_cb, ssh_shell}, 
		    {init_args,[ConnectionRef, ChannelId]},
		    {cm, ConnectionRef}, {channel_id, ChannelId}],
	    {ok, State} = ssh_channel:init([Args]),
	    ssh_channel:enter_loop(State);
	Error ->
	    Error
    end;
start_shell(Error) ->
    Error.

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
default_algorithms() -> 
    ssh_transport:default_algorithms().

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
valid_socket_to_use(Socket, Options) ->
    case proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}) of
	{tcp,_,_} ->
	    %% Is this tcp-socket a valid socket?
	    case {is_tcp_socket(Socket),
		  {ok,[{active,false}]} == inet:getopts(Socket, [active])
		 }
	    of
		{true, true} ->
		    ok;
		{true, false} ->
		    {error, not_passive_mode};
		_ ->
		    {error, not_tcp_socket}
	    end;
	{L4,_,_} ->
	    {error, {unsupported,L4}}
    end.

is_tcp_socket(Socket) -> {ok,[]} =/= inet:getopts(Socket, [delay_send]).



daemon_shell_opt(Options) ->
     case proplists:get_value(shell, Options) of
	 undefined ->
	     [{shell, {shell, start, []}}  | Options];
	 _ ->
	     Options
     end.

daemon_host_inet_opt(HostAddr, Options1) ->
    case HostAddr of
	any ->
	    {ok, Host0} = inet:gethostname(), 
	    {Host0,  proplists:get_value(inet, Options1, inet), Options1};
	{_,_,_,_} ->
	    {HostAddr, inet, 
	     [{ip, HostAddr} | Options1]};
	{_,_,_,_,_,_,_,_} ->
	    {HostAddr, inet6, 
	     [{ip, HostAddr} | Options1]}
    end.


start_daemon(Socket, Options) ->
    case handle_options(Options) of
	{error, Error} ->
	    {error, Error};
	{SocketOptions, SshOptions} ->
	    case valid_socket_to_use(Socket, Options) of
		ok -> 
		    try 
			do_start_daemon(Socket, [{role,server}|SshOptions], SocketOptions)
		    catch
			throw:bad_fd -> {error,bad_fd};
			_C:_E -> {error,{cannot_start_daemon,_C,_E}}
		    end;
		{error,SockError} ->
		    {error,SockError}
	    end
    end.

start_daemon(Host, Port, Options, Inet) ->
    case handle_options(Options) of
	{error, _Reason} = Error ->
	    Error;
	{SocketOptions, SshOptions}->
	    try 
		do_start_daemon(Host, Port, [{role,server}|SshOptions] , [Inet|SocketOptions])
	    catch
		throw:bad_fd -> {error,bad_fd};
		_C:_E -> {error,{cannot_start_daemon,_C,_E}}
	    end
    end.
    
do_start_daemon(Socket, SshOptions, SocketOptions) ->
    {ok, {IP,Port}} = 
	try {ok,_} = inet:sockname(Socket)
	catch
	    _:_ -> throw(bad_socket)
	end,
    Host = fmt_host(IP),
    Profile = proplists:get_value(profile, SshOptions, ?DEFAULT_PROFILE),
    Opts = [{asocket, Socket},
	    {asock_owner,self()},
	    {address, Host},
	    {port, Port},
	    {role, server},
	    {socket_opts, SocketOptions}, 
	    {ssh_opts, SshOptions}],
    {_, Callback, _} = proplists:get_value(transport, SshOptions, {tcp, gen_tcp, tcp_closed}),
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    %% It would proably make more sense to call the
	    %% address option host but that is a too big change at the
	    %% monent. The name is a legacy name!
	    try sshd_sup:start_child(Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		Result = {ok,_} ->
		    ssh_acceptor:handle_connection(Callback, Host, Port, Opts, Socket),
		    Result;
		Result = {error, _} ->
		    Result
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{ok, _} ->
		    ssh_acceptor:handle_connection(Callback, Host, Port, Opts, Socket),
		    {ok, Sup};
		Other ->
		    Other
	    end
    end.

do_start_daemon(Host0, Port0, SshOptions, SocketOptions) ->
    {Host,Port1} = 
	try
	    case proplists:get_value(fd, SocketOptions) of
		undefined ->
		    {Host0,Port0};
		Fd when Port0==0 ->
		    find_hostport(Fd);
		_ ->
		    {Host0,Port0}
	    end
	catch
	    _:_ -> throw(bad_fd)
	end,
    Profile = proplists:get_value(profile, SshOptions, ?DEFAULT_PROFILE),
    {Port, WaitRequestControl, Opts0} =
	case Port1 of
	    0 -> %% Allocate the socket here to get the port number...
		{_, Callback, _} =  
		    proplists:get_value(transport, SshOptions, {tcp, gen_tcp, tcp_closed}),
		{ok,LSock} = ssh_acceptor:callback_listen(Callback, 0, SocketOptions),
		{ok,{_,LPort}} = inet:sockname(LSock),
		{LPort, 
		 {LSock,Callback}, 
		 [{lsocket,LSock},{lsock_owner,self()}]
		};
	    _ ->
		{Port1, false, []}
	end,
    Opts = [{address, Host}, 
	    {port, Port},
	    {role, server},
	    {socket_opts, SocketOptions}, 
	    {ssh_opts, SshOptions} | Opts0],
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    %% It would proably make more sense to call the
	    %% address option host but that is a too big change at the
	    %% monent. The name is a legacy name!
	    try sshd_sup:start_child(Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		Result = {ok,_} ->
		    sync_request_control(WaitRequestControl),
		    Result;
		Result = {error, _} ->
		    Result
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, Opts) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{ok, _} ->
		    sync_request_control(WaitRequestControl),
		    {ok, Sup};
		Other ->
		    Other
	    end
    end.

sync_request_control(false) ->
    ok;
sync_request_control({LSock,Callback}) ->
    receive
	{request_control,LSock,ReqPid} ->
	    ok = Callback:controlling_process(LSock, ReqPid),
	    ReqPid ! {its_yours,LSock},
	    ok
    end.

find_hostport(Fd) ->
    %% Using internal functions inet:open/8 and inet:close/0.
    %% Don't try this at home unless you know what you are doing!
    {ok,S} = inet:open(Fd, {0,0,0,0}, 0, [], tcp, inet, stream, inet_tcp),
    {ok, HostPort} = inet:sockname(S),
    ok = inet:close(S),
    HostPort.
    

handle_options(Opts) ->
    try handle_option(algs_compatibility(proplists:unfold(Opts)), [], []) of
	{Inet, Ssh} ->
	    {handle_ip(Inet), Ssh}
    catch
	throw:Error ->
	    Error
    end.


algs_compatibility(Os0) ->
    %% Take care of old options 'public_key_alg' and 'pref_public_key_algs'
    case proplists:get_value(public_key_alg, Os0) of
	undefined -> 
	    Os0;
	A when is_atom(A) -> 
	    %% Skip public_key_alg if pref_public_key_algs is defined:
	    Os = lists:keydelete(public_key_alg, 1, Os0),
	    case proplists:get_value(pref_public_key_algs,Os) of
		undefined when A == 'ssh-rsa' ; A==ssh_rsa ->
		    [{pref_public_key_algs,['ssh-rsa','ssh-dss']} | Os];
		undefined when A == 'ssh-dss' ; A==ssh_dsa ->
		    [{pref_public_key_algs,['ssh-dss','ssh-rsa']} | Os];
		undefined ->
		    throw({error, {eoptions, {public_key_alg,A} }});
		_ -> 
		    Os
	    end;
	V ->
	    throw({error, {eoptions, {public_key_alg,V} }})
    end.


handle_option([], SocketOptions, SshOptions) ->
    {SocketOptions, SshOptions};
handle_option([{system_dir, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{user_dir, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{user_dir_fun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{silently_accept_hosts, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{user_interaction, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{connect_timeout, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{user, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{dsa_pass_phrase, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{rsa_pass_phrase, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{password, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{user_passwords, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{pwdfun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{key_cb, {Module, Options}} | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option({key_cb, Module}),
                                        handle_ssh_priv_option({key_cb_private, Options}) |
                                        SshOptions]);
handle_option([{key_cb, Module} | Rest], SocketOptions, SshOptions) ->
    handle_option([{key_cb, {Module, []}} | Rest], SocketOptions, SshOptions);
handle_option([{keyboard_interact_fun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
%%Backwards compatibility
handle_option([{allow_user_interaction, Value}  | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option({user_interaction, Value}) | SshOptions]);
handle_option([{infofun, _} = Opt | Rest],SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{connectfun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{disconnectfun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{unexpectedfun, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{failfun, _} = Opt | Rest],  SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{ssh_msg_debug_fun, _} = Opt | Rest],  SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
%%Backwards compatibility should not be underscore between ip and v6 in API
handle_option([{ip_v6_disabled, Value} | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option({ipv6_disabled, Value}) | SshOptions]);
handle_option([{ipv6_disabled, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{transport, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{subsystems, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{ssh_cli, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{shell, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{exec, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{auth_methods, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{auth_method_kb_interactive_data, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{pref_public_key_algs, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{preferred_algorithms,_} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{dh_gex_groups,_} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{dh_gex_limits,_} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{quiet_mode, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{idle_time, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{rekey_limit, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{max_sessions, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{max_channels, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{negotiation_timeout, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{parallel_login, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
%% (Is handled by proplists:unfold above:)
%% handle_option([parallel_login|Rest], SocketOptions, SshOptions) ->
%%     handle_option(Rest, SocketOptions, [handle_ssh_option({parallel_login,true}) | SshOptions]);
handle_option([{minimal_remote_max_packet_size, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{id_string, _ID} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{profile, _ID} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{max_random_length_padding, _Bool} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{tstflg, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, [handle_inet_option(Opt) | SocketOptions], SshOptions).


handle_ssh_option({tstflg,_F} = Opt) -> Opt;
handle_ssh_option({minimal_remote_max_packet_size, Value} = Opt) when is_integer(Value), Value >=0 ->
    Opt;
handle_ssh_option({system_dir, Value} = Opt) when is_list(Value) ->
    check_dir(Opt);
handle_ssh_option({user_dir, Value} = Opt) when is_list(Value) ->
    check_dir(Opt);
handle_ssh_option({user_dir_fun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({silently_accept_hosts, Value} = Opt) when is_boolean(Value) ->
    Opt;
handle_ssh_option({user_interaction, Value} = Opt) when is_boolean(Value) ->
    Opt;
handle_ssh_option({preferred_algorithms,[_|_]} = Opt) -> 
    handle_pref_algs(Opt);

handle_ssh_option({dh_gex_groups,L0}) when is_list(L0) ->
    {dh_gex_groups,
     collect_per_size(
       lists:foldl(
	 fun({N,G,P}, Acc) when is_integer(N),N>0,
				is_integer(G),G>0,
				is_integer(P),P>0 -> 
		 [{N,{G,P}} | Acc];
	    ({N,{G,P}}, Acc) when is_integer(N),N>0,
				  is_integer(G),G>0,
				  is_integer(P),P>0 ->
		 [{N,{G,P}} | Acc];
	    ({N,GPs}, Acc) when is_list(GPs) ->
		 lists:foldr(fun({Gi,Pi}, Acci) when is_integer(Gi),Gi>0,
						     is_integer(Pi),Pi>0 -> 
				     [{N,{Gi,Pi}} | Acci]
			     end, Acc, GPs)
	 end, [], L0))};

handle_ssh_option({dh_gex_groups,{Tag,File=[C|_]}}=Opt) when is_integer(C), C>0,
							      Tag == file ;
							      Tag == ssh_moduli_file ->
    {ok,GroupDefs} =
	case Tag of
	    file -> 
		file:consult(File);
	    ssh_moduli_file ->
		case file:open(File,[read]) of
		    {ok,D} ->
			try
			    {ok,Moduli} = read_moduli_file(D, 1, []),
			    file:close(D),
			    {ok, Moduli}
			catch
			    _:_ ->
				throw({error, {{eoptions, Opt}, "Bad format in file "++File}})
			end;
		    {error,enoent} ->
			throw({error, {{eoptions, Opt}, "File not found:"++File}});
		    {error,Error} ->
			throw({error, {{eoptions, Opt}, io_lib:format("Error reading file ~s: ~p",[File,Error])}})
		end
	end,

    try
	handle_ssh_option({dh_gex_groups,GroupDefs})
    catch
	_:_ ->
	    throw({error, {{eoptions, Opt}, "Bad format in file: "++File}})
    end;	    
    

handle_ssh_option({dh_gex_limits,{Min,Max}} = Opt) when is_integer(Min), Min>0, 
							is_integer(Max), Max>=Min ->
    %% Server
    Opt;
handle_ssh_option({dh_gex_limits,{Min,I,Max}} = Opt) when is_integer(Min), Min>0, 
							  is_integer(I),   I>=Min,
							  is_integer(Max), Max>=I ->
    %% Client
    Opt;
handle_ssh_option({pref_public_key_algs, Value} = Opt) when is_list(Value), length(Value) >= 1 ->
    case handle_user_pref_pubkey_algs(Value, []) of
	{true, NewOpts} ->
	    {pref_public_key_algs, NewOpts};
	_ ->
	    throw({error, {eoptions, Opt}})
    end;
handle_ssh_option({connect_timeout, Value} = Opt) when is_integer(Value); Value == infinity ->
    Opt;
handle_ssh_option({max_sessions, Value} = Opt) when is_integer(Value), Value>0 ->
    Opt;
handle_ssh_option({max_channels, Value} = Opt) when is_integer(Value), Value>0 ->
    Opt;
handle_ssh_option({negotiation_timeout, Value} = Opt) when is_integer(Value); Value == infinity ->
    Opt;
handle_ssh_option({parallel_login, Value} = Opt) when Value==true ; Value==false ->
    Opt;
handle_ssh_option({user, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({dsa_pass_phrase, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({rsa_pass_phrase, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({password, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({user_passwords, Value} = Opt) when is_list(Value)->
    Opt;
handle_ssh_option({pwdfun, Value} = Opt) when is_function(Value,2) ->
    Opt;
handle_ssh_option({pwdfun, Value} = Opt) when is_function(Value,4) ->
    Opt;
handle_ssh_option({key_cb, Value} = Opt)  when is_atom(Value) ->
    Opt;
handle_ssh_option({key_cb, {CallbackMod, CallbackOptions}} = Opt) when is_atom(CallbackMod),
                                                                      is_list(CallbackOptions) ->
    Opt;
handle_ssh_option({keyboard_interact_fun, Value} = Opt) when is_function(Value,3) ->
    Opt;
handle_ssh_option({compression, Value} = Opt) when is_atom(Value) ->
    Opt;
handle_ssh_option({exec, {Module, Function, _}} = Opt) when is_atom(Module), 
							    is_atom(Function) ->
    Opt;
handle_ssh_option({exec, Function} = Opt) when is_function(Function) ->
    Opt;
handle_ssh_option({auth_methods, Value} = Opt)  when is_list(Value) ->
    Opt;
handle_ssh_option({auth_method_kb_interactive_data, {Name,Instruction,Prompt,Echo}} = Opt) when is_list(Name),
												is_list(Instruction),
												is_list(Prompt),
												is_boolean(Echo) ->
    Opt;
handle_ssh_option({auth_method_kb_interactive_data, F} = Opt) when is_function(F,3) ->
    Opt;
handle_ssh_option({infofun, Value} = Opt)  when is_function(Value) ->
    Opt;
handle_ssh_option({connectfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({disconnectfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({unexpectedfun, Value} = Opt) when is_function(Value,2) ->
    Opt;
handle_ssh_option({failfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({ssh_msg_debug_fun, Value} = Opt) when is_function(Value,4) ->
    Opt;

handle_ssh_option({ipv6_disabled, Value} = Opt) when is_boolean(Value) ->
    throw({error, {{ipv6_disabled, Opt}, option_no_longer_valid_use_inet_option_instead}});
handle_ssh_option({transport, {Protocol, Cb, ClosTag}} = Opt) when is_atom(Protocol),
								   is_atom(Cb),
								   is_atom(ClosTag) ->
    Opt;
handle_ssh_option({subsystems, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({ssh_cli, {Cb, _}}= Opt) when is_atom(Cb) ->
    Opt;
handle_ssh_option({ssh_cli, no_cli} = Opt) ->
    Opt;
handle_ssh_option({shell, {Module, Function, _}} = Opt)  when is_atom(Module),
							      is_atom(Function) ->
    Opt;
handle_ssh_option({shell, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({quiet_mode, Value} = Opt) when is_boolean(Value) ->
    Opt;
handle_ssh_option({idle_time, Value} = Opt) when is_integer(Value), Value > 0 ->
    Opt;
handle_ssh_option({rekey_limit, Value} = Opt) when is_integer(Value) -> 
    Opt;
handle_ssh_option({id_string, random}) ->
    {id_string, {random,2,5}}; %% 2 - 5 random characters
handle_ssh_option({id_string, ID} = Opt) when is_list(ID) ->
    Opt;
handle_ssh_option({max_random_length_padding, Value} = Opt) when is_integer(Value),
								 Value =< 255 ->
    Opt;
handle_ssh_option({profile, Value} = Opt) when is_atom(Value) ->
    Opt;
handle_ssh_option(Opt) ->
    throw({error, {eoptions, Opt}}).

handle_ssh_priv_option({key_cb_private, Value} = Opt) when is_list(Value) ->
    Opt.

handle_inet_option({active, _} = Opt) ->
    throw({error, {{eoptions, Opt}, "SSH has built in flow control, "
		   "and active is handled internally, user is not allowed"
		   "to specify this option"}});

handle_inet_option({inet, Value}) when (Value == inet) or (Value == inet6) ->
    Value;
handle_inet_option({reuseaddr, _} = Opt) ->
    throw({error, {{eoptions, Opt},"Is set internally, user is not allowed"
		   "to specify this option"}});
%% Option verified by inet
handle_inet_option(Opt) ->
    Opt.


%% Check preferred algs

handle_pref_algs({preferred_algorithms,Algs}) ->
    try alg_duplicates(Algs, [], []) of
	[] ->
	    {preferred_algorithms,
	     [try ssh_transport:supported_algorithms(Key)
	      of
		  DefAlgs -> handle_pref_alg(Key,Vals,DefAlgs)
	      catch
		  _:_ -> throw({error, {{eoptions, {preferred_algorithms,Key}}, 
					"Bad preferred_algorithms key"}})
	      end  || {Key,Vals} <- Algs]
	    };
		    
	Dups ->
	    throw({error, {{eoptions, {preferred_algorithms,Dups}}, "Duplicates found"}})
    catch
	_:_ ->
	    throw({error, {{eoptions, preferred_algorithms}, "Malformed"}})
    end.

alg_duplicates([{K,V}|KVs], Ks, Dups0) ->
    Dups =
	case lists:member(K,Ks) of
	    true ->
		[K|Dups0];
	    false ->
		Dups0
	end,
    case V--lists:usort(V) of
	[] ->
	    alg_duplicates(KVs, [K|Ks], Dups);
	Ds ->
	    alg_duplicates(KVs, [K|Ks], Dups++Ds)
    end;
alg_duplicates([], _Ks, Dups) ->
    Dups.

handle_pref_alg(Key,
		Vs=[{client2server,C2Ss=[_|_]},{server2client,S2Cs=[_|_]}],
		[{client2server,Sup_C2Ss},{server2client,Sup_S2Cs}]
	       ) ->
    chk_alg_vs(Key, C2Ss, Sup_C2Ss),
    chk_alg_vs(Key, S2Cs, Sup_S2Cs),
    {Key, Vs};

handle_pref_alg(Key,
		Vs=[{server2client,[_|_]},{client2server,[_|_]}],
		Sup=[{client2server,_},{server2client,_}]
	       ) ->
    handle_pref_alg(Key, lists:reverse(Vs), Sup);

handle_pref_alg(Key, 
		Vs=[V|_],
		Sup=[{client2server,_},{server2client,_}]
	       ) when is_atom(V) ->
    handle_pref_alg(Key, [{client2server,Vs},{server2client,Vs}], Sup);

handle_pref_alg(Key, 
		Vs=[V|_],
		Sup=[S|_]
	       ) when is_atom(V), is_atom(S) ->
    chk_alg_vs(Key, Vs, Sup),
    {Key, Vs};

handle_pref_alg(Key, Vs, _) ->
    throw({error, {{eoptions, {preferred_algorithms,[{Key,Vs}]}}, "Badly formed list"}}).

chk_alg_vs(OptKey, Values, SupportedValues) ->
    case (Values -- SupportedValues) of
	[] -> Values;
	Bad -> throw({error, {{eoptions, {OptKey,Bad}}, "Unsupported value(s) found"}})
    end.
	    
handle_ip(Inet) -> %% Default to ipv4
    case lists:member(inet, Inet) of
	true ->
	    Inet;
	false ->
	    case lists:member(inet6, Inet) of
		true -> 
		    Inet;
		false ->
		    [inet | Inet]
	    end
    end.

check_dir({_,Dir} = Opt) ->
    case directory_exist_readable(Dir) of
	ok ->
	    Opt;
	{error,Error} ->
	    throw({error, {eoptions,{Opt,Error}}})
    end.

directory_exist_readable(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory,
			access = Access}} ->
	    case Access of
		read -> ok;
		read_write -> ok;
		_ -> {error, eacces}
	    end;

	{ok, #file_info{}}->
	    {error, enotdir};

	{error, Error} ->
	    {error, Error}
    end.
		
		    

collect_per_size(L) ->
    lists:foldr(
      fun({Sz,GP}, [{Sz,GPs}|Acc]) -> [{Sz,[GP|GPs]}|Acc];
	 ({Sz,GP}, Acc) -> [{Sz,[GP]}|Acc]
      end, [], lists:sort(L)).

read_moduli_file(D, I, Acc) ->
    case io:get_line(D,"") of
	{error,Error} ->
	    {error,Error};
	eof ->
	    {ok, Acc};
	"#" ++ _ -> read_moduli_file(D, I+1, Acc);
	<<"#",_/binary>> ->  read_moduli_file(D, I+1, Acc);
	Data ->
	    Line = if is_binary(Data) -> binary_to_list(Data);
		      is_list(Data) -> Data
		   end,
	    try
		[_Time,_Type,_Tests,_Tries,Size,G,P] = string:tokens(Line," \r\n"),
		M = {list_to_integer(Size),
		     {list_to_integer(G), list_to_integer(P,16)}
		    },
		read_moduli_file(D, I+1, [M|Acc])
	    catch
		_:_ ->
		    read_moduli_file(D, I+1, Acc)
	    end
    end.
			   
handle_user_pref_pubkey_algs([], Acc) ->
    {true, lists:reverse(Acc)};
handle_user_pref_pubkey_algs([H|T], Acc) ->
    case lists:member(H, ?SUPPORTED_USER_KEYS) of
	true ->
	    handle_user_pref_pubkey_algs(T, [H| Acc]);

	false when H==ssh_dsa -> handle_user_pref_pubkey_algs(T, ['ssh-dss'| Acc]);
	false when H==ssh_rsa -> handle_user_pref_pubkey_algs(T, ['ssh-rsa'| Acc]);

	false ->
	    false
    end.

fmt_host({A,B,C,D}) -> 
    lists:concat([A,".",B,".",C,".",D]);
fmt_host(T={_,_,_,_,_,_,_,_}) -> 
    lists:flatten(string:join([io_lib:format("~.16B",[A]) || A <- tuple_to_list(T)], ":")).
