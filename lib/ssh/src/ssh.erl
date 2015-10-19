%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

-export([start/0, start/1, stop/0, connect/3, connect/4, close/1, connection_info/2,
	 channel_info/3,
	 daemon/1, daemon/2, daemon/3,
	 default_algorithms/0,
	 stop_listener/1, stop_listener/2,  stop_listener/3,
	 stop_daemon/1, stop_daemon/2, stop_daemon/3,
	 shell/1, shell/2, shell/3]).

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
-spec connect(string(), integer(), proplists:proplist()) -> {ok, pid()} |  {error, term()}.
-spec connect(string(), integer(), proplists:proplist(), timeout()) -> {ok, pid()} |  {error, term()}.
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
	    ConnectionTimeout = proplists:get_value(connect_timeout, Options, infinity),
	    try Transport:connect(Host, Port,  [ {active, false} | SocketOptions], ConnectionTimeout) of
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
-spec daemon(integer()) -> {ok, pid()} | {error, term()}.
-spec daemon(integer(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
-spec daemon(any | inet:ip_address(), integer(), proplists:proplist()) -> {ok, pid()} | {error, term()}.

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
		    success = ssh_connection:ptty_alloc(ConnectionRef, ChannelId, []),
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
%%--------------------------------------------------------------------
default_algorithms() -> 
    ssh_transport:default_algorithms().

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
    Profile = proplists:get_value(profile, Options, ?DEFAULT_PROFILE),
    case ssh_system_sup:system_supervisor(Host, Port, Profile) of
	undefined ->
	    %% It would proably make more sense to call the
	    %% address option host but that is a too big change at the
	    %% monent. The name is a legacy name!
	    try sshd_sup:start_child([{address, Host}, 
				      {port, Port}, {role, server},
				      {socket_opts, SocketOptions}, 
				      {ssh_opts, Options}]) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		Result = {Code, _} when (Code == ok) or (Code == error) ->
		    Result
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    AccPid = ssh_system_sup:acceptor_supervisor(Sup),
	    case ssh_acceptor_sup:start_child(AccPid, [{address, Host},
						       {port, Port}, {role, server},
						       {socket_opts, SocketOptions},
						       {ssh_opts, Options}]) of
		{error, {already_started, _}} ->
		    {error, eaddrinuse};
		{ok, _} ->
		    {ok, Sup};
		Other ->
		    Other
	    end
    end.

handle_options(Opts) ->
    try handle_option(algs_compatibility(proplists:unfold(Opts)), [], []) of
	{Inet, Ssh} ->
	    {handle_ip(Inet), Ssh}
    catch
	throw:Error ->
	    Error
    end.


algs_compatibility(Os) ->
    %% Take care of old options 'public_key_alg' and 'pref_public_key_algs'
    comp_pk(proplists:get_value(preferred_algorithms,Os),
	    proplists:get_value(pref_public_key_algs,Os),
	    proplists:get_value(public_key_alg, Os),
	    [{K,V} || {K,V} <- Os,
		      K =/= public_key_alg,
		      K =/= pref_public_key_algs]
	   ).

comp_pk(undefined, undefined, undefined, Os) -> Os;
comp_pk( PrefAlgs,         _,         _, Os) when PrefAlgs =/= undefined -> Os;

comp_pk(undefined, undefined,   ssh_dsa, Os) -> comp_pk(undefined, undefined, 'ssh-dss', Os);
comp_pk(undefined, undefined,   ssh_rsa, Os) -> comp_pk(undefined, undefined, 'ssh-rsa', Os);
comp_pk(undefined, undefined,        PK, Os) -> 
    PKs = [PK | ssh_transport:supported_algorithms(public_key)--[PK]],
    [{preferred_algorithms, [{public_key,PKs}] } | Os];

comp_pk(undefined, PrefPKs, _, Os) when PrefPKs =/= undefined ->
    PKs = [case PK of
	       ssh_dsa -> 'ssh-dss';
	       ssh_rsa -> 'ssh-rsa';
	       _ -> PK
	   end || PK <- PrefPKs],
    [{preferred_algorithms, [{public_key,PKs}]} | Os].


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
handle_option([{key_cb, _} = Opt | Rest], SocketOptions, SshOptions) ->
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
handle_option([Opt | Rest], SocketOptions, SshOptions) ->
    handle_option(Rest, [handle_inet_option(Opt) | SocketOptions], SshOptions).


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
handle_ssh_option({dh_gex_groups,L=[{I1,I2,I3}|_]}) when is_integer(I1), I1>0, 
							 is_integer(I2), I2>0,
							 is_integer(I3), I3>0 ->
    {dh_gex_groups, lists:map(fun({N,G,P}) -> {N,{G,P}} end, L)};
handle_ssh_option({dh_gex_groups,{file,File=[C|_]}}=Opt) when is_integer(C), C>0 ->
    %% A string, (file name)
    case file:consult(File) of
	{ok, List} ->
	    try handle_ssh_option({dh_gex_groups,List}) of
		{dh_gex_groups,_} = NewOpt ->
		    NewOpt
	    catch
		_:_ ->
		    throw({error, {{eoptions, Opt}, "Bad format in file"}})
	    end;
	Error ->
	    throw({error, {{eoptions, Opt},{"Error reading file",Error}}})
    end;
handle_ssh_option({dh_gex_limits,{Min,I,Max}} = Opt) when is_integer(Min), Min>0, 
							  is_integer(I),   I>=Min,
							  is_integer(Max), Max>=I ->
    Opt;
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
handle_ssh_option({pwdfun, Value} = Opt) when is_function(Value) ->
    Opt;
handle_ssh_option({key_cb, Value} = Opt)  when is_atom(Value) ->
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
		
		    

