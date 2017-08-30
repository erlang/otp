%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_iiop_pm.erl
%% Description:
%%    This file contains the mapping of addresses on the format {Host, Port} 
%%    to a proxy pid.
%%
%%-----------------------------------------------------------------
-module(orber_iiop_pm).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("kernel/include/inet.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([connect/7, 
	 close_connection/1, close_connection/2,
	 list_existing_connections/0, 
	 list_setup_connections/0, 
	 list_all_connections/0,
	 init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/0, setup_connection/8,
	 reconfigure/1, reconfigure/3, reconfigure/4, add_connection/3,
	 sockname2peername/2, peername2sockname/2]).

%%-----------------------------------------------------------------
%% Macros/Defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-define(PM_CONNECTION_DB, orber_iiop_pm_db).

-record(state, {connections, queue}).

-record(connection, {hp, child, interceptors, slave, 
		     flags = 0, alias = 0, socketdata = {"Unavailable", 0}}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.
start(Opts) ->
    gen_server:start_link({local, 'orber_iiop_pm'}, ?MODULE, Opts, []).


connect(Host, Port, SocketType, Timeout, Chars, Wchars, Ctx)
  when SocketType == normal ->
    Key = create_key(Host, Port, Ctx),
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[#connection{child = connecting}] ->
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    [], Chars, Wchars, Key}, Timeout);
	[] ->
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    [], Chars, Wchars, Key}, Timeout);
	[#connection{hp = {_, _, 0}, child = P, interceptors = I}] ->
	    {ok, P, [], I, 0};
	[#connection{hp = {_, _, Interface}, child = P, interceptors = I}] ->
	    {ok, P, [], I, [Interface]}
    end;
connect(Host, Port, SocketType, Timeout, Chars, Wchars, Ctx) 
  when SocketType == ssl ->
    Key = create_key(Host, Port, Ctx),
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[#connection{child = connecting}] ->
	    SocketOptions = get_ssl_socket_options(Ctx),
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    SocketOptions, Chars, Wchars, Key}, 
			    Timeout);
	[] ->
	    SocketOptions = get_ssl_socket_options(Ctx),
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    SocketOptions, Chars, Wchars, Key}, 
			    Timeout);
	[#connection{hp = {_, _, 0}, child = P, interceptors = I}] ->
	    {ok, P, [], I, 0};
	[#connection{hp = {_, _, Interface}, child = P, interceptors = I}] ->
	    {ok, P, [], I, [Interface]}
    end.

get_ssl_socket_options([]) ->
    SSLOpts = 
	case orber_env:ssl_client_options() of
	    [] ->
		[{verify, orber_env:ssl_client_verify()},
		 {depth, orber_env:ssl_client_depth()},
		 {certfile, orber_env:ssl_client_certfile()},
		 {cacertfile, orber_env:ssl_client_cacertfile()},
		 {password, orber_env:ssl_client_password()},
		 {keyfile, orber_env:ssl_client_keyfile()},
		 {ciphers, orber_env:ssl_client_ciphers()},
		 {cachetimeout, orber_env:ssl_client_cachetimeout()},
		 {keepalive, orber_env:iiop_ssl_out_keepalive()}];
	    Opts ->
		case orber_tb:check_illegal_tcp_options(Opts) of
		    ok -> 
			check_old_ssl_client_options([]),
			Opts;
		    {error, IllegalOpts} ->
			error_logger:error_report([{application, orber},
						   "TCP options not allowed to set on a connection", 
						   IllegalOpts]),
			error("Illegal TCP option")
		end
	end,
    ssl_client_extra_options(SSLOpts, []);
get_ssl_socket_options([#'IOP_ServiceContext'
			{context_id=?ORBER_GENERIC_CTX_ID, 
			 context_data = {configuration, Options}}|_]) ->
    SSLOpts = 
	case orber_tb:keysearch(ssl_client_options, Options,
				orber_env:ssl_client_options()) of
	    [] ->
		Verify = orber_tb:keysearch(ssl_client_verify, Options, 
					    orber_env:ssl_client_verify()),
		Depth = orber_tb:keysearch(ssl_client_depth, Options, 
					   orber_env:ssl_client_depth()),
		Cert = orber_tb:keysearch(ssl_client_certfile, Options, 
					  orber_env:ssl_client_certfile()),
		CaCert = orber_tb:keysearch(ssl_client_cacertfile, Options, 
					    orber_env:ssl_client_cacertfile()),
		Pwd = orber_tb:keysearch(ssl_client_password, Options, 
					 orber_env:ssl_client_password()),
		Key = orber_tb:keysearch(ssl_client_keyfile, Options, 
					 orber_env:ssl_client_keyfile()),
		Ciphers = orber_tb:keysearch(ssl_client_ciphers, Options, 
					     orber_env:ssl_client_ciphers()),
		Timeout = orber_tb:keysearch(ssl_client_cachetimeout, Options, 
					     orber_env:ssl_client_cachetimeout()),
		KeepAlive = orber_tb:keysearch(ssl_server_cachetimeout, Options, 
					       orber_env:iiop_ssl_out_keepalive()),
		[{verify, Verify},
		 {depth, Depth},
		 {certfile, Cert},
		 {cacertfile, CaCert},
		 {password, Pwd},
		 {keyfile, Key},
		 {ciphers, Ciphers},
		 {cachetimeout, Timeout},
		 {keepalive, KeepAlive}];
	    Opts ->	
		case orber_tb:check_illegal_tcp_options(Opts) of
		    ok -> 
			check_old_ssl_client_options(Options),
			Opts;
		    {error, IllegalOpts} ->
			error_logger:error_report([{application, orber},
						   "TCP options not allowed to set on a connection", 
						   IllegalOpts]),
			error("Illegal TCP option")
		end
	end,
    ssl_client_extra_options(SSLOpts, []);
get_ssl_socket_options([_|T]) ->
    get_ssl_socket_options(T).


ssl_client_extra_options([], Acc) ->
    Acc;
ssl_client_extra_options([{_Type, []}|T], Acc) ->
    ssl_client_extra_options(T, Acc);
ssl_client_extra_options([{_Type, infinity}|T], Acc) ->
    ssl_client_extra_options(T, Acc);
ssl_client_extra_options([{Type, Value}|T], Acc) ->
    ssl_client_extra_options(T, [{Type, Value}|Acc]).

add_connection(Key, Key, SockData) ->
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[Connection] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       Connection#connection{socketdata = SockData});
	[] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       #connection{hp= Key, child = connecting,
				   socketdata = SockData})
    end;
add_connection(Key, NewKey, SockData) ->
    add_connection(Key, Key, SockData),
    add_connection(NewKey, NewKey, SockData).

get_socket_data(Key) ->
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[#connection{socketdata = SockData}] ->
	    SockData;
	_ ->
	    {"Unable to extract socket information", 0}
    end.

sockname2peername(SockHost, SockPort) ->
    orber_tb:unique(
      do_select([{#connection{hp = {'$1', '$2', '_'},
			      socketdata = {match_type(SockHost), 
					    match_type(SockPort)}, 
			      _='_'}, [], [{{'$1', '$2'}}]}])).


peername2sockname(PeerHost, PeerPort) ->
    orber_tb:unique(
      do_select([{#connection{hp = {match_type(PeerHost), 
				    match_type(PeerPort),
				    '_'},
			      socketdata = '$1', 
			      _='_'}, [], ['$1']}])).

match_type(0) -> 
    %% Wildcard port number
    '_';
match_type("") -> 
    %% Wildcard host
    '_';
match_type(Key) -> 
    %% Wildcard not used.
    Key.

create_key(Host, Port, []) ->
    {Host, Port, 0};
create_key(Host, Port, 
	   [#'IOP_ServiceContext'
	    {context_id=?ORBER_GENERIC_CTX_ID, 
	     context_data = {interface, Interface}}|_]) when is_list(Interface) ->
    {Host, Port, Interface};
create_key(Host, Port, 
	   [#'IOP_ServiceContext'
	    {context_id=?ORBER_GENERIC_CTX_ID, 
	     context_data = {interface, Interface}}|_]) ->
    orber:dbg("[~p] orber_iiop_pm:create_key(~p, ~p);~n"
	      "The supplied interface must be a string.",
	      [?LINE, Host, Port, Interface], ?DEBUG_LEVEL),
    corba:raise(#'BAD_CONTEXT'{completion_status=?COMPLETED_NO});
create_key(Host, Port, [_|T]) ->
    create_key(Host, Port, T).

reconfigure(Options) ->
    {Local, Proxy} = check_options(Options, [], []),
    reconfigure_local(Local),
    reconfigure_proxy(Proxy).


reconfigure(Options, Host, Port) ->
    reconfigure(Options, Host, Port, 0).
reconfigure(Options, Host, Port, Interface) ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port, Interface}) of
	[#connection{child = P}] when is_pid(P) ->
	    case check_options(Options, [], []) of
		{[], Proxy} ->
		    reconfigure_proxy(Proxy, [P]);
		{Local, Proxy} ->
		    reconfigure_proxy(Proxy, [P]),
		    gen_server:call(orber_iiop_pm, {reconfigure, Local, 
						    Host, Port, Interface}, infinity)
	    end;
	_ ->
	    {error, "No proxy matched the supplied reference"}
    end.

reconfigure_local([]) ->
    ok;
reconfigure_local(Options) ->
    gen_server:call(orber_iiop_pm, {reconfigure, Options}, infinity).

reconfigure_proxy([]) ->
    ok;
reconfigure_proxy(Options) ->
    reconfigure_proxy(Options, do_select([{#connection{child = '$1', _='_'}, 
					   [], ['$1']}])).

reconfigure_proxy(Options, [Pid|T]) ->
    Pid ! {reconfigure, Options},
    reconfigure_proxy(Options, T);
reconfigure_proxy(_Options, []) ->
    ok.


check_options([{interceptors, false}|Options], Local, Proxy) ->
    check_options(Options, [{interceptors, false}|Local], Proxy);
check_options([{interceptors, {native, LPIs}}|Options], Local, Proxy) ->
    check_options(Options, [{interceptors, {native, LPIs}}|Local], Proxy);
check_options([{fake, option}|Options], Local, Proxy) ->
    check_options(Options, Local, [{fake, option}|Proxy]);
check_options([_|Options], Local, Proxy) ->
    check_options(Options, Local, Proxy);
check_options([], Local, Proxy) ->
    {Local, Proxy}.


close_connection(PeerData) ->
    close_connection(PeerData, 0).

close_connection(PeerData, Interface) ->
    gen_server:call(orber_iiop_pm, {disconnect, PeerData, Interface}, infinity).


list_existing_connections() ->
    transform(
      lists:sort(
	do_select([{#connection{hp = {'$2','$3','$4'}, child = '$1', _='_'}, 
		    [{is_pid, '$1'}], [{{'$1', '$2','$3','$4'}}]}])), []).

list_setup_connections() ->
    transform(
      lists:sort(
	do_select([{#connection{hp = {'$1','$2','$3'}, child = connecting, _='_'}, [], 
			     [{{'$1','$2','$3'}}]}])), []).

list_all_connections() ->
    transform(
      lists:sort(
	do_select([{#connection{hp = {'$2','$3','$4'}, child = '$1', _='_'}, [], 
		    [{{'$1','$2','$3', '$4'}}]}])), []).

%% Since the connections interface can be 0 or an ip-address we want to
%% transform those containing 0.
transform([{C, H, P, 0}, {C, H, P, I}|T], Acc) ->
    %% ACL defined interface. Drop the anonymous one.
    transform(T, [{H, P, I}|Acc]);
transform([{_C, H, P, 0}|T], Acc) ->
    %% No interface supplied. Drop the 0.
    transform(T, [{H, P}|Acc]);
transform([{_C, H, P, I}|T], Acc) ->
    %% Interface supplied. Keep it.
    transform(T, [{H, P, I}|Acc]);
transform([{H,P,0}|T], Acc) ->
    transform(T, [{H,P}|Acc]);
transform([{H,P,I}|T], Acc) ->
    transform(T, [{H,P,I}|Acc]);
transform([H|T], Acc) ->
    transform(T, [H|Acc]);
transform([], Acc) ->
    Acc.

do_select(Pattern) ->   
    case catch ets:select(?PM_CONNECTION_DB, Pattern) of
	{'EXIT', _What} ->
	    [];
	Result ->
	    Result
    end.

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/0 (Only used for test purpose !!!!!!)
%%-----------------------------------------------------------------
stop() ->
    gen_server:call(orber_iiop_pm, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(_Opts) ->
    process_flag(trap_exit, true),
    {ok, #state{connections = ets:new(orber_iiop_pm_db, 
				      [{keypos, 2}, set, public, named_table]),
		queue = ets:new(orber_iiop_pm_queue, [bag])}}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(_Reason, #state{queue = Q}) ->
    %% Kill all proxies and close table before terminating
    stop_all_proxies(ets:first(?PM_CONNECTION_DB)),
    ets:delete(?PM_CONNECTION_DB),
    ets:delete(Q),
    ok.

stop_all_proxies('$end_of_table') ->
    ok;
stop_all_proxies(Key) ->
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[] ->
	    ok;
	[#connection{child = connecting, interceptors = I}] ->
	    invoke_connection_closed(I);
	[#connection{child = P, interceptors = I}] ->
	    invoke_connection_closed(I),
	    catch orber_iiop_outproxy:stop(P)
    end,
    stop_all_proxies(ets:next(?PM_CONNECTION_DB, Key)).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({connect, Host, Port, SocketType, SocketOptions, Chars, Wchars, Key}, 
	    From, State) ->
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[#connection{child = connecting}] ->
	    %% Another client already requested a connection to the given host/port. 
	    %% Just add this client to the queue.
	    ets:insert(State#state.queue, {Key, From}),
	    {noreply, State};
	[#connection{hp = {_,_,0}, child = P, interceptors = I}] ->
	    %% This case will occur if the PortMapper completed a connection
	    %% between the client's ets:lookup and receiving this request.
	    {reply, {ok, P, [], I, 0}, State};
	[#connection{hp = {_,_,Intf}, child = P, interceptors = I}] ->
	    %% This case will occur if the PortMapper completed a connection
	    %% between the client's ets:lookup and receiving this request.
	    {reply, {ok, P, [], I, [Intf]}, State};
	[] ->
	    %% The first time a connection is requested to the given host/port.
	    case catch spawn_link(?MODULE, setup_connection, 
				  [self(), Host, Port, SocketType, 
				   SocketOptions, Chars, Wchars, Key]) of
		Slave when is_pid(Slave) ->
		    ets:insert(?PM_CONNECTION_DB, 
			       #connection{hp = Key, child = connecting, 
					   interceptors = false, slave = Slave}),
		    ets:insert(State#state.queue, {Key, From}),
		    {noreply, State};
		What ->
		    orber:dbg("[~p] orber_iiop_pm:handle_call(connect);~n"
			      "Unable to invoke setup_connection due to: ~n~p~n", 
			      [?LINE, What], ?DEBUG_LEVEL),
		    {reply, 
		     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}, 
		     State}
	    end
    end;
handle_call({disconnect, PeerData, Interface}, _From, State) ->
    {reply, do_disconnect(PeerData, Interface, State), State};
handle_call({reconfigure, Options, Host, Port, Interface}, 
	    _From, State) ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port, Interface}) of
	[] ->
	    {reply, {error, "No proxy matched the supplied reference"}, State};
	[Connection] ->
	    NewConnection = update_connection(Connection, Options),
	    ets:insert(?PM_CONNECTION_DB, NewConnection),
	    {reply, ok, State}    
    end;
handle_call({reconfigure, Options}, _From, State) ->
    case catch update_db(ets:first(?PM_CONNECTION_DB), Options) of
	ok ->
	    {reply, ok, State};
	_What ->
	    {reply, {error, "Unable to change configuration"}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

update_db('$end_of_table', _) ->
    ok;
update_db(Key, Options) ->
    [Connection] = ets:lookup(?PM_CONNECTION_DB, Key),
    NewConnection = update_connection(Connection, Options),
    ets:insert(?PM_CONNECTION_DB, NewConnection),
    update_db(ets:next(?PM_CONNECTION_DB, Key), Options).


update_connection(Connection, [{interceptors, false}|Options]) ->
    update_connection(Connection#connection{interceptors = false}, Options);
update_connection(#connection{interceptors = false,
			      hp = {PH, PP, _},
			      socketdata = {SH, SP}} = Connection, 
		  [{interceptors, {native, LPIs}}|Options]) ->
    %% No Interceptor(s). Add the same Ref used by the built in interceptors.
    update_connection(Connection#connection{interceptors = 
					    {native, {PH, PP, SH, SP}, LPIs}},
		      Options);
update_connection(#connection{interceptors = {native, Ref, _}} = Connection, 
		  [{interceptors, {native, LPIs}}|Options]) ->
    %% Interceptor(s) already in use. We must use the same Ref as before.
    update_connection(Connection#connection{interceptors = 
					    {native, Ref, LPIs}},
		      Options);
update_connection(Connection, [H|T]) ->
    orber:dbg("[~p] orber_iiop_pm:update_connection(~p, ~p)~n"
	      "Unable to update the connection.~n", 
	      [?LINE, Connection, H], ?DEBUG_LEVEL),
    update_connection(Connection, T);
update_connection(Connection, []) ->
    Connection.

do_disconnect([], _Interface, _State) ->
    ok;
do_disconnect([{Host, Port}|T], Interface, State) ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port, Interface}) of
	[] ->
	    ok;
	[#connection{child = connecting, interceptors = I}] ->
	    ets:delete(?PM_CONNECTION_DB, {Host, Port, Interface}),
	    Exc = {'EXCEPTION',#'INTERNAL'{completion_status = ?COMPLETED_NO}},
	    send_reply_to_queue(ets:lookup(State#state.queue, 
					   {Host, Port, Interface}), Exc),
	    ets:delete(State#state.queue, {Host, Port, Interface}),
	    invoke_connection_closed(I);
	[#connection{child = P, interceptors = I}] ->
	    unlink(P),
	    catch orber_iiop_outproxy:stop(P),
	    ets:delete(?PM_CONNECTION_DB, {Host, Port, Interface}),
	    invoke_connection_closed(I)
    end,
    do_disconnect(T, Interface, State).

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Trapping exits 
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Check the most common scenario first, i.e., a proxy terminates.
    case ets:match_object(?PM_CONNECTION_DB, #connection{child = Pid, _='_'}) of
	[#connection{hp = K, interceptors = I}] ->
	    ets:delete(?PM_CONNECTION_DB, K),
	    invoke_connection_closed(I),
	    {noreply, State};
	[#connection{hp = K, interceptors = I}, #connection{hp = K2}] ->
	    ets:delete(?PM_CONNECTION_DB, K),
	    ets:delete(?PM_CONNECTION_DB, K2),
	    invoke_connection_closed(I),
	    {noreply, State};
	[] when Reason == normal ->
	    %% This might have been a spawned 'setup_connection' which terminated
	    %% after sucessfully setting up a new connection.
	    {noreply, State};
	[] ->
	    %% Wasn't a proxy. Hence, we must test if it was a spawned
	    %% 'setup_connection' that failed.
	    case ets:match_object(?PM_CONNECTION_DB, #connection{slave = Pid, _='_'}) of
		[#connection{hp = K, child = connecting, interceptors = I}] ->
		    ets:delete(?PM_CONNECTION_DB, K),
		    invoke_connection_closed(I),
		    Exc = {'EXCEPTION',#'INTERNAL'{completion_status = ?COMPLETED_NO}},
		    send_reply_to_queue(ets:lookup(State#state.queue, K), Exc),
		    ets:delete(State#state.queue, K),
		    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p);~n"
			      "It was not possible to create a connection to the"
			      " given host/port.",
			      [?LINE, K], ?DEBUG_LEVEL),
		    {noreply, State};
		[#connection{hp = K, child = connecting, interceptors = I}, 
		 #connection{hp = K2}] ->
		    ets:delete(?PM_CONNECTION_DB, K),
		    ets:delete(?PM_CONNECTION_DB, K2),
		    invoke_connection_closed(I),
		    Exc = {'EXCEPTION',#'INTERNAL'{completion_status = ?COMPLETED_NO}},
		    send_reply_to_queue(ets:lookup(State#state.queue, K), Exc),
		    ets:delete(State#state.queue, K),
		    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p);~n"
			      "It was not possible to create a connection to the"
			      " given host/port.",
			      [?LINE, K], ?DEBUG_LEVEL),
		    {noreply, State};
		_ ->
		    {noreply, State}
	    end
    end;
handle_info({setup_failed, {Host, Port, _} = Key, Key, Exc}, State) ->
    %% Deletet the data from the connection DB first to avoid clients from
    %% trying to access it again.
    ets:delete(?PM_CONNECTION_DB, Key),
    %% Now we can send whatever exception received.
    send_reply_to_queue(ets:lookup(State#state.queue, Key), Exc),
    ets:delete(State#state.queue, Key),
    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p ~p);~n"
	      "It was not possible to create a connection to the given host/port.", 
	      [?LINE, Host, Port], ?DEBUG_LEVEL),
    {noreply, State};
handle_info({setup_failed, {Host, Port, _} = Key, NewKey, Exc}, State) ->
    %% Deletet the data from the connection DB first to avoid clients from
    %% trying to access it again.
    ets:delete(?PM_CONNECTION_DB, Key),
    ets:delete(?PM_CONNECTION_DB, NewKey),
    %% Now we can send whatever exception received.
    send_reply_to_queue(ets:lookup(State#state.queue, Key), Exc),
    ets:delete(State#state.queue, Key),
    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p ~p);~n"
	      "It was not possible to create a connection to the given host/port.", 
	      [?LINE, Host, Port], ?DEBUG_LEVEL),
    {noreply, State};
handle_info({setup_successfull, Key, Key, {Child, Ctx, Int}}, State) ->
    %% Create a link to the proxy and store it in the connection DB.
    link(Child),
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[Connection] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       Connection#connection{hp = Key, child = Child, 
					     interceptors = Int, 
					     slave = undefined});
	[] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       #connection{hp = Key, child = Child, 
				   interceptors = Int, 
				   slave = undefined})
    end,
    %% Send the Proxy reference to all waiting clients.
    case Key of
	{_, _, 0} ->
	    send_reply_to_queue(ets:lookup(State#state.queue, Key),
				{ok, Child, Ctx, Int, 0});
	{_, _, Interface} ->
	    send_reply_to_queue(ets:lookup(State#state.queue, Key),
				{ok, Child, Ctx, Int, [Interface]})
    end,
    %% Reset the queue.
    ets:delete(State#state.queue, Key),
    {noreply, State};
handle_info({setup_successfull, Key, NewKey, {Child, Ctx, Int}}, State) ->
    %% Create a link to the proxy and store it in the connection DB.
    link(Child),
    case ets:lookup(?PM_CONNECTION_DB, NewKey) of
	[Connection] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       Connection#connection{hp = NewKey, child = Child, 
					     interceptors = Int, 
					     slave = undefined});
	[] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       #connection{hp = NewKey, child = Child, 
				   interceptors = Int, 
				   slave = undefined})
    end,
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[Connection2] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       Connection2#connection{hp = Key, child = Child, 
					     interceptors = Int, 
					     slave = undefined});
	[] ->
	    ets:insert(?PM_CONNECTION_DB, 
		       #connection{hp = Key, child = Child, 
				   interceptors = Int, 
				   slave = undefined})
    end,
    %% Send the Proxy reference to all waiting clients.
    case NewKey of
	{_, _, 0} ->
	    send_reply_to_queue(ets:lookup(State#state.queue, Key),
				{ok, Child, Ctx, Int, 0});
	{_, _, Interface} ->
	    send_reply_to_queue(ets:lookup(State#state.queue, Key),
				{ok, Child, Ctx, Int, [Interface]})
    end,
    %% Reset the queue.
    ets:delete(State#state.queue, Key),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.


send_reply_to_queue([], _) ->
    ok;
send_reply_to_queue([{_, Client}|T], Reply) ->
    gen_server:reply(Client, Reply),
    send_reply_to_queue(T, Reply). 

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
setup_connection(PMPid, Host, Port, SocketType, SocketOptions, Chars, Wchars, Key) ->
    case catch access_allowed(Host, Port, SocketType, Key) of
	ok  ->
	    do_setup_connection(PMPid, Host, Port, SocketType, SocketOptions, 
				Chars, Wchars, Key, Key);
	{ok, Interface} ->
	    do_setup_connection(PMPid, Host, Port, SocketType, 
				[{ip, Interface}|SocketOptions], 
				Chars, Wchars, Key, Key);
	{ok, Interface, NewKey} ->
	    do_setup_connection(PMPid, Host, Port, SocketType, 
				[{ip, Interface}|SocketOptions], 
				Chars, Wchars, Key, NewKey);
	false ->
	    orber_tb:info("Blocked connect attempt to ~s - ~p", [Host, Port]),
	    PMPid ! {setup_failed, Key, Key,
		     {'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}},
	    ok;
	Reason ->
	    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p); failed~n"
		      "Reason: ~p", 
		      [?LINE, Host, Port, Reason], ?DEBUG_LEVEL),
	    PMPid ! {setup_failed, Key, Key,
		     {'EXCEPTION', #'COMM_FAILURE'{completion_status=?COMPLETED_NO}}},
	    ok
    end.


do_setup_connection(PMPid, Host, Port, SocketType, SocketOptions, Chars, 
		    Wchars, Key, NewKey) ->
    case catch orber_iiop_outsup:connect(Host, Port, SocketType, 
					 SocketOptions, PMPid, Key, NewKey) of
	{error, {'EXCEPTION', E}} ->
	    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p);~n"
		      "Raised Exc: ~p", 
		      [?LINE, Host, Port, E], ?DEBUG_LEVEL),
	    PMPid ! {setup_failed, Key, NewKey, {'EXCEPTION', E}},
	    ok;
	{error, Reason} ->
	    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p);~n"
		      "Got EXIT: ~p", 
		      [?LINE, Host, Port, Reason], ?DEBUG_LEVEL),
	    PMPid ! {setup_failed, Key, NewKey, 
		     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}},
	    ok;
	{ok, undefined} ->
	    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p);~n"
		      "Probably no listener on the given Node/Port or timedout.",
		      [?LINE, Host, Port], ?DEBUG_LEVEL),
	    PMPid ! {setup_failed, Key, NewKey, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 1),
						   completion_status=?COMPLETED_NO}}},
	    ok;
	{ok, Child} ->
	    case init_interceptors(Host, Port, get_socket_data(Key)) of
		{'EXCEPTION', E} ->
		    PMPid ! {setup_failed, Key, NewKey, {'EXCEPTION', E}},
		    ok;
		Interceptors ->
		    BiDirCtx = orber:bidir_context(),
		    Ctx = case orber:exclude_codeset_ctx() of
			      true ->
				  BiDirCtx;
			      _ ->
				  CodeSetCtx = 
				      #'CONV_FRAME_CodeSetContext'
				    {char_data =  Chars, 
				     wchar_data = Wchars},
				  [#'IOP_ServiceContext'
				   {context_id=?IOP_CodeSets, 
				    context_data = CodeSetCtx} | BiDirCtx]
			  end,
		    PMPid ! {setup_successfull, Key, NewKey, 
			     {Child, Ctx, Interceptors}},
		    ok
	    end
    end.

access_allowed(Host, Port, Type, {_,_,UserInterface}) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_ACL_OUTGOING) of
	false when UserInterface == 0 ->
	    get_local_interface(Type);
	false ->
	    inet:getaddr(UserInterface, get_ip_family(UserInterface));
	true ->
	    SearchFor = 
		case Type of
		    normal ->
			tcp_out;
		    ssl ->
			ssl_out
		end,
	    {ok, Ip} = inet:getaddr(Host, get_ip_family(Host)),
	    case orber_acl:match(Ip, SearchFor, true) of
		{true, [], 0} ->
		    get_local_interface(Type);
		{true, [], Port} ->
		    get_local_interface(Type);
		{true, [], {Min, Max}} when Port >= Min, Port =< Max ->
		    get_local_interface(Type);
		{true, [Interface], 0} ->		    
		    {ok, NewIp} = inet:getaddr(Interface, get_ip_family(Interface)),
		    {ok, NewIp, {Host, Port, 0}};
		{true, [Interface], Port} ->
		    
		    {ok, NewIp} = inet:getaddr(Interface, get_ip_family(Interface)),
		    {ok, NewIp, {Host, Port, 0}};
		{true, [Interface], {Min, Max}} when Port >= Min, Port =< Max ->
		    
		    {ok, NewIp} = inet:getaddr(Interface, get_ip_family(Interface)),
		    {ok, NewIp, {Host, Port, 0}};
		_ ->
		    false
	    end
    end.

get_local_interface(normal) ->
    case orber_env:ip_address_local() of
	[] ->
	    ok;
	[Interface] ->
	    inet:getaddr(Interface, get_ip_family(Interface))
    end;
get_local_interface(ssl) ->
    case orber_env:iiop_ssl_ip_address_local() of
	[] ->
	    ok;
	[Interface] ->
	    inet:getaddr(Interface, get_ip_family(Interface))
    end.

get_ip_family(Addr) ->
    [Family] = orber_socket:get_ip_family_opts(Addr),
    Family.

invoke_connection_closed(false) ->
    ok;
invoke_connection_closed({native, Ref, PIs}) ->
    (catch orber_pi:closed_out_connection(PIs, Ref));
invoke_connection_closed({_Type, _PIs}) ->
    ok.


init_interceptors(Host, Port, {SHost, SPort}) ->
    case orber:get_interceptors() of
	{native, PIs} ->
	    case catch orber_pi:new_out_connection(PIs, Host, Port, SHost, SPort) of
		{'EXIT', R} ->
		    orber:dbg("[~p] orber_iiop_pm:init_interceptors(~p); Got Exit: ~p.~n"
			      "One or more Interceptor incorrect or undefined?", 
			      [?LINE, PIs, R], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 2), 
						  completion_status=?COMPLETED_NO}};
		IntRef ->
		    {native, IntRef, PIs}
	    end;
	Other ->
            %% Either 'false' or {Type, PIs}.
	    Other
    end.


check_old_ssl_client_options(Options) ->
    try
	0 = orber_tb:keysearch(ssl_client_verify, Options, 
			       orber_env:ssl_client_verify()),
    	1 = orber_tb:keysearch(ssl_client_depth, Options, 
			       orber_env:ssl_client_depth()),
     	[] = orber_tb:keysearch(ssl_client_certfile, Options, 
				orber_env:ssl_client_certfile()),
	[] = orber_tb:keysearch(ssl_client_cacertfile, Options, 
				orber_env:ssl_client_cacertfile()),
	[] = orber_tb:keysearch(ssl_client_password, Options, 
				orber_env:ssl_client_password()),
	[] = orber_tb:keysearch(ssl_client_keyfile, Options, 
				orber_env:ssl_client_keyfile()),
	[] = orber_tb:keysearch(ssl_client_ciphers, Options, 
				orber_env:ssl_client_ciphers()),
	infinity = orber_tb:keysearch(ssl_client_cachetimeout, Options, 
				      orber_env:ssl_client_cachetimeout()),
	false = orber_tb:keysearch(iiop_ssl_out_keepalive, Options, 
				   orber_env:iiop_ssl_out_keepalive())

    catch
	_:_ ->
	    error_logger:warning_report([{application, orber},
			 "Ignoring deprecated ssl client options used together with the ssl_client_options"])
    end.
   

    

%%-----------------------------------------------------------------
%% END OF MODULE
%%-----------------------------------------------------------------
