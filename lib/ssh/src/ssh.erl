%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
-include_lib("public_key/include/public_key.hrl").

-export([start/0, start/1, stop/0, connect/3, connect/4, close/1, connection_info/2,
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 stop_listener/1, stop_listener/2, stop_daemon/1, stop_daemon/2,
	 shell/1, shell/2, shell/3]).

%%--------------------------------------------------------------------
-spec start() -> ok.
-spec start(permanent | transient | temporary) -> ok.
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
-spec stop() -> ok.
%%
%% Description: Stops the ssh application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
-spec connect(string(), integer(), proplists:proplists()) -> {ok, pid()} |  {error, term()}.
-spec connect(string(), integer(), proplists:proplists(), timeout()) -> {ok, pid()} |  {error, term()}.
%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
connect(Host, Port, Options) ->
    connect(Host, Port, Options, infinity).
connect(Host, Port, Options, Timeout) ->
    case handle_options(Options) of
	{error, _Reason} = Error ->
	    Error;
	{SocketOptions, SshOptions} ->
	    {_, Transport, _} = TransportOpts =
		proplists:get_value(transport, Options, {tcp, gen_tcp, tcp_closed}),
	    Inet = proplists:get_value(inet, SshOptions, inet),
	    try Transport:connect(Host, Port,  [ {active, false}, Inet | SocketOptions], Timeout) of
		{ok, Socket} ->
		    Opts =  [{user_pid, self()}, {host, Host} | fix_idle_time(SshOptions)],
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
-spec daemon(integer()) -> {ok, pid()}.
-spec daemon(integer(), proplists:proplist()) -> {ok, pid()}.
-spec daemon(any | inet:ip_address(), integer(), proplists:proplist()) -> {ok, pid()}.

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

    {Host, Inet, Options} = case HostAddr of
				any ->
				    {ok, Host0} = inet:gethostname(), 
				    {Host0,  proplists:get_value(inet, Options1, inet), Options1};
				{_,_,_,_} ->
				    {HostAddr, inet, 
				     [{ip, HostAddr} | Options1]};
				{_,_,_,_,_,_,_,_} ->
				    {HostAddr, inet6, 
				     [{ip, HostAddr} | Options1]}
			    end,
    start_daemon(Host, Port, Options, Inet).

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
    ssh_system_sup:stop_listener(Address, Port).

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
    ssh_system_sup:stop_system(Address, Port).

%%--------------------------------------------------------------------
-spec shell(string()) ->  _.
-spec shell(string(), proplists:proplist()) ->  _.
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
fix_idle_time(SshOptions) ->
    case proplists:get_value(idle_time, SshOptions) of
	undefined ->
	    [{idle_time, infinity}|SshOptions];
	_ ->
	    SshOptions
    end.
start_daemon(Host, Port, Options, Inet) ->
    case handle_options(Options) of
	{error, _Reason} = Error ->
	    Error;
	{SocketOptions, SshOptions}->
	    do_start_daemon(Host, Port,[{role, server} |SshOptions] , [Inet | SocketOptions])
    end.
    
do_start_daemon(Host, Port, Options, SocketOptions) ->
    case ssh_system_sup:system_supervisor(Host, Port) of
	undefined ->
	    %% It would proably make more sense to call the
	    %% address option host but that is a too big change at the
	    %% monent. The name is a legacy name!
	    try sshd_sup:start_child([{address, Host}, 
				      {port, Port}, {role, server},
				      {socket_opts, SocketOptions}, 
				      {ssh_opts, Options}]) of
		{ok, SysSup} ->
		    {ok, SysSup};
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{error, R} ->
		    {error, R}
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
    try handle_option(proplists:unfold(Opts), [], []) of
	{_,_} = Options ->
	    Options
    catch
	throw:Error ->
	    Error
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
handle_option([{public_key_alg, _} = Opt | Rest], SocketOptions, SshOptions) ->
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
handle_option([{key_cb, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{role, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{compression, _} = Opt | Rest], SocketOptions, SshOptions) ->
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
handle_option([{failfun, _} = Opt | Rest],  SocketOptions, SshOptions) ->
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
handle_option([{pref_public_key_algs, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{quiet_mode, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{idle_time, _} = Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([{rekey_limit, _} = Opt|Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, SocketOptions, [handle_ssh_option(Opt) | SshOptions]);
handle_option([Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, [handle_inet_option(Opt) | SocketOptions], SshOptions).

handle_ssh_option({system_dir, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({user_dir, Value} = Opt) when is_list(Value) ->
    Opt;
handle_ssh_option({user_dir_fun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({silently_accept_hosts, Value} = Opt) when is_boolean(Value) ->
    Opt;
handle_ssh_option({user_interaction, Value} = Opt) when is_boolean(Value) ->
    Opt;
handle_ssh_option({public_key_alg, ssh_dsa}) ->
    {public_key_alg, 'ssh-dss'};
handle_ssh_option({public_key_alg, ssh_rsa})  ->
    {public_key_alg, 'ssh-rsa'};
handle_ssh_option({public_key_alg, Value} = Opt) when Value == 'ssh-rsa'; Value == 'ssh-dss' ->
    Opt;
handle_ssh_option({pref_public_key_algs, Value} = Opt) when is_list(Value), length(Value) >= 1 ->
    case handle_pref_algs(Value, []) of
	{true, NewOpts} ->
	    NewOpts;
	_ ->
	    throw({error, {eoptions, Opt}})
    end;
handle_ssh_option({connect_timeout, Value} = Opt) when is_integer(Value); Value == infinity ->
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
handle_ssh_option({pwdfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({key_cb, Value} = Opt)  when is_atom(Value) ->
    Opt;
handle_ssh_option({compression, Value} = Opt) when is_atom(Value) ->
    Opt;
handle_ssh_option({exec, {Module, Function, _}} = Opt) when is_atom(Module), 
							    is_atom(Function) ->

    Opt;
handle_ssh_option({auth_methods, Value} = Opt)  when is_list(Value) ->
    Opt;
handle_ssh_option({infofun, Value} = Opt)  when is_function(Value) ->
    Opt;
handle_ssh_option({connectfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({disconnectfun , Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({failfun, Value} = Opt) when is_function(Value) ->
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
handle_ssh_option(Opt) ->
    throw({error, {eoptions, Opt}}).

handle_inet_option({active, _} = Opt) ->
    throw({error, {{eoptions, Opt}, "Ssh has built in flow control, "
		   "and activ is handled internaly user is not allowd"
		   "to specify this option"}});
handle_inet_option({inet, Value} = Opt) when (Value == inet) or (Value == inet6) ->
    Opt;
handle_inet_option({reuseaddr, _} = Opt) ->
    throw({error, {{eoptions, Opt},"Is set internaly user is not allowd"
		   "to specify this option"}});
%% Option verified by inet
handle_inet_option(Opt) ->
    Opt.
%% Check preferred algs
handle_pref_algs([], Acc) ->
    {true, lists:reverse(Acc)};
handle_pref_algs([H|T], Acc) ->
    case H of
	ssh_dsa ->
	    handle_pref_algs(T, ['ssh-dss'| Acc]);
	ssh_rsa ->
	    handle_pref_algs(T, ['ssh-rsa'| Acc]);
	'ssh-dss' ->
	    handle_pref_algs(T, ['ssh-dss'| Acc]);
	'ssh-rsa' ->
	    handle_pref_algs(T, ['ssh-rsa'| Acc]);
	_ ->
	    false
    end.
