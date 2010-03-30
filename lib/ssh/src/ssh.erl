%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssh).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-export([start/0, start/1, stop/0, connect/3, connect/4, close/1, connection_info/2,
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 stop_listener/1, stop_listener/2, stop_daemon/1, stop_daemon/2,
	 shell/1, shell/2, shell/3]).

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(ssh).

start(Type) ->
    application:start(ssh, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
%% Function: connect(Host, Port, Options) -> 
%%           connect(Host, Port, Options, Timeout -> ConnectionRef | {error, Reason}
%% 
%%	Host - string()
%%	Port - integer()
%%	Options - [{Option, Value}]
%%      Timeout - infinity | integer().
%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).
connect(Host, Port, Options, Timeout) ->
    {SocketOpts, Opts} = handle_options(Options),
    DisableIpv6 =  proplists:get_value(ip_v6_disabled, Opts, false),
    Inet = inetopt(DisableIpv6),
    try sshc_sup:start_child([[{address, Host}, {port, Port}, 
			      {role, client},
			      {channel_pid, self()},
			      {socket_opts, [Inet | SocketOpts]}, 
			      {ssh_opts, [{host, Host}| Opts]}]]) of 
 	{ok, ConnectionSup} ->
	    {ok, Manager} = 
		ssh_connection_controler:connection_manager(ConnectionSup),
	    MRef = erlang:monitor(process, Manager),
	    receive 
		{Manager, is_connected} ->
		    do_demonitor(MRef, Manager),
		    {ok, Manager};
		%% When the connection fails 
		%% ssh_connection_sup:connection_manager
		%% might return undefined as the connection manager
		%% could allready have terminated, so we will not
		%% match the Manager in this case
		{_, not_connected, {error, Reason}} ->
		    do_demonitor(MRef, Manager),
		    {error, Reason};
		{_, not_connected, Other} ->
		    do_demonitor(MRef, Manager),
		    {error, Other};
		{'DOWN', MRef, _, Manager, Reason} when is_pid(Manager) ->
		    receive %% Clear EXIT message from queue
			{'EXIT', Manager, _What} -> 
			    {error, Reason}
		    after 0 ->
			    {error, Reason}
		    end
	    after Timeout  ->
		    do_demonitor(MRef, Manager),
		    ssh_connection_manager:stop(Manager),
		    {error, timeout}
	    end
    catch 
	exit:{noproc, _} ->
 	    {error, ssh_not_started}
    end.

do_demonitor(MRef, Manager) ->
    erlang:demonitor(MRef),
    receive 
	{'DOWN', MRef, _, Manager, _} -> 
	    ok
    after 0 -> 
	    ok
    end.


%%--------------------------------------------------------------------
%% Function: close(ConnectionRef) -> ok
%%
%% Description: Closes an ssh connection.
%%--------------------------------------------------------------------	
close(ConnectionRef) ->
    ssh_connection_manager:stop(ConnectionRef).

%%--------------------------------------------------------------------
%% Function: connection_info(ConnectionRef) -> [{Option, Value}]
%%
%% Description: Retrieves information about a connection. 
%%--------------------------------------------------------------------	
connection_info(ConnectionRef, Options) ->
    ssh_connection_manager:connection_info(ConnectionRef, Options).

%%--------------------------------------------------------------------
%% Function: channel_info(ConnectionRef) -> [{Option, Value}]
%%
%% Description: Retrieves information about a connection. 
%%--------------------------------------------------------------------	
channel_info(ConnectionRef, ChannelId, Options) ->
    ssh_connection_manager:channel_info(ConnectionRef, ChannelId, Options).

%%--------------------------------------------------------------------
%% Function: daemon(Port) ->
%%           daemon(Port, Options) ->
%%           daemon(Address, Port, Options) -> SshSystemRef
%%
%% Description: Starts a server listening for SSH connections 
%% on the given port.
%%--------------------------------------------------------------------	
daemon(Port) ->
    daemon(Port, []).

daemon(Port, Options) ->
    daemon(any, Port, Options).

daemon(HostAddr, Port, Options0) ->
    Options1 = case proplists:get_value(shell, Options0) of
		   undefined ->
		       [{shell, {shell, start, []}}  | Options0];
		   _ ->
		       Options0
	       end,
    DisableIpv6 =  proplists:get_value(ip_v6_disabled, Options0, false),
    {Host, Inet, Options} = case HostAddr of
				any ->
				    {ok, Host0} = inet:gethostname(), 
				    {Host0, inetopt(DisableIpv6), Options1};
				{_,_,_,_} ->
				    {HostAddr, inet, 
				     [{ip, HostAddr} | Options1]};
				{_,_,_,_,_,_,_,_} ->
				    {HostAddr, inet6, 
				     [{ip, HostAddr} | Options1]}
			    end,
    start_daemon(Host, Port, [{role, server} | Options], Inet).

%%--------------------------------------------------------------------
%% Function: stop_listener(SysRef) -> ok
%%           stop_listener(Address, Port) -> ok
%%
%%
%% Description: Stops the listener, but leaves 
%% existing connections started by the listener up and running.
%%--------------------------------------------------------------------	
stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).
stop_listener(Address, Port) ->
    ssh_system_sup:stop_listener(Address, Port).

%%--------------------------------------------------------------------
%% Function: stop_daemon(SysRef) -> ok
%%%          stop_daemon(Address, Port) -> ok
%%
%%
%% Description: Stops the listener and all connections started by 
%% the listener.
%%--------------------------------------------------------------------	
stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).
stop_daemon(Address, Port) ->
    ssh_system_sup:stop_system(Address, Port).

%%--------------------------------------------------------------------
%% Function: shell(Host [,Port,Options]) -> {ok, ConnectionRef} | 
%%                                                     {error, Reason}
%%
%%   Host = string()
%%   Port = integer()
%%   Options = [{Option, Value}]
%%
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
shell(Host) ->
    shell(Host, ?SSH_DEFAULT_PORT, []).
shell(Host, Options) ->
    shell(Host, ?SSH_DEFAULT_PORT, Options).
shell(Host, Port, Options) ->
    case connect(Host, Port, Options) of
	{ok, ConnectionRef} ->
	    case ssh_connection:session_channel(ConnectionRef, infinity) of
		{ok,ChannelId}  ->
		    Args = [{channel_cb, ssh_shell}, 
			    {init_args,[ConnectionRef, ChannelId]},
			    {cm, ConnectionRef}, {channel_id, ChannelId}],
		    {ok, State} = ssh_channel:init([Args]),
		    ssh_channel:enter_loop(State);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_daemon(Host, Port, Options, Inet) ->
    {SocketOpts, Opts} = handle_options(Options),
    case ssh_system_sup:system_supervisor(Host, Port) of
	undefined ->
	    %% TODO: It would proably make more sense to call the
	    %% address option host but that is a too big change at the
	    %% monent. The name is a legacy name!
	    try sshd_sup:start_child([{address, Host}, 
				      {port, Port}, {role, server},
				      {socket_opts, [Inet | SocketOpts]}, 
				      {ssh_opts, Opts}]) of
		{ok, SysSup} ->
		    {ok, SysSup};
		{error, {already_started, _}} ->
		    {error, eaddrinuse}
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    case ssh_system_sup:restart_acceptor(Host, Port) of
		{ok, _} ->
		    {ok, Sup};
		_  ->
		    {error, eaddrinuse}
	    end
    end.

handle_options(Opts) ->
    handle_options(proplists:unfold(Opts), [], []).
handle_options([], SockOpts, Opts) ->
    {SockOpts, Opts};
%% TODO: Could do some type checks here on plain ssh-opts
handle_options([{system_dir, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_dir, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_dir_fun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{silently_accept_hosts, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_interaction, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{public_key_alg, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{connect_timeout, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{password, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_passwords, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{pwdfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_auth, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{key_cb, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{role, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{channel, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{compression, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{allow_user_interaction, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{infofun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{connectfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{disconnectfun , _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{failfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{ip_v6_disabled, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{ip, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt |SockOpts], Opts);
handle_options([{ifaddr, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt |SockOpts], Opts);
handle_options([{fd, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt | SockOpts], Opts);
handle_options([{nodelay, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt | SockOpts], Opts);
handle_options([Opt | Rest], SockOpts, Opts) ->
    handle_options(Rest, SockOpts, [Opt | Opts]).

%% Has IPv6 been disabled?
inetopt(true) ->
    inet;
inetopt(false) ->
    inet6.


