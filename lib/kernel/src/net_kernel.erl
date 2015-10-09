%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
-module(net_kernel).

-behaviour(gen_server).

-define(nodedown(N, State), verbose({?MODULE, ?LINE, nodedown, N}, 1, State)).
-define(nodeup(N, State), verbose({?MODULE, ?LINE, nodeup, N}, 1, State)).

%%-define(dist_debug, true).

%-define(DBG,erlang:display([?MODULE,?LINE])).

-ifdef(dist_debug).
-define(debug(Term), erlang:display(Term)).
-else.
-define(debug(Term), ok).
-endif.

-ifdef(DEBUG).
-define(connect_failure(Node,Term),
	io:format("Net Kernel 2: Failed connection to node ~p, reason ~p~n",
		  [Node,Term])).
-else.
-define(connect_failure(Node,Term),noop).
-endif.

%% Default ticktime change transition period in seconds
-define(DEFAULT_TRANSITION_PERIOD, 60).

%-define(TCKR_DBG, 1).

-ifdef(TCKR_DBG).
-define(tckr_dbg(X), erlang:display({?LINE, X})).
-else.
-define(tckr_dbg(X), ok).
-endif.

%% User Interface Exports
-export([start/1, start_link/1, stop/0,
	 kernel_apply/3,
	 monitor_nodes/1,
	 monitor_nodes/2,
	 longnames/0,
	 allow/1,
	 protocol_childspecs/0,
	 epmd_module/0]).

-export([connect/1, disconnect/1, hidden_connect/1, passive_cnct/1]).
-export([connect_node/1, hidden_connect_node/1]). %% explicit connect
-export([set_net_ticktime/1, set_net_ticktime/2, get_net_ticktime/0]).

-export([node_info/1, node_info/2, nodes_info/0,
	 connecttime/0,
	 i/0, i/1, verbose/1]).

-export([publish_on_node/1, update_publish_nodes/1]).

%% Internal Exports
-export([do_spawn/3,
	 spawn_func/6,
	 ticker/2,
	 ticker_loop/2,
	 aux_ticker/4]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2,code_change/3]).

-export([passive_connect_monitor/2]).

-import(error_logger,[error_msg/2]).

-record(state, {
	  name,         %% The node name
	  node,         %% The node name including hostname
	  type,         %% long or short names
	  tick,         %% tick information
	  connecttime,  %% the connection setuptime.
	  connections,  %% table of connections
	  conn_owners = [], %% List of connection owner pids,
	  pend_owners = [], %% List of potential owners
	  listen,       %% list of  #listen
	  allowed,       %% list of allowed nodes in a restricted system
	  verbose = 0,   %% level of verboseness
	  publish_on_nodes = undefined
	 }).

-record(listen, {
		 listen,     %% listen pid
		 accept,     %% accepting pid
		 address,    %% #net_address
		 module      %% proto module
		}).

-define(LISTEN_ID, #listen.listen).
-define(ACCEPT_ID, #listen.accept).

-record(connection, {
		     node,          %% remote node name
		     state,         %% pending | up | up_pending
		     owner,         %% owner pid
	             pending_owner, %% possible new owner
		     address,       %% #net_address
		     waiting = [],  %% queued processes
		     type           %% normal | hidden
		    }).

-record(barred_connection, {
	  node %% remote node name
	 }).


-record(tick, {ticker,        %% ticker                     : pid()
	       time           %% Ticktime in milli seconds  : integer()
	      }).

-record(tick_change, {ticker, %% Ticker                     : pid()
		      time,   %% Ticktime in milli seconds  : integer()
		      how     %% What type of change        : atom()
		     }).

%% Default connection setup timeout in milliseconds.
%% This timeout is set for every distributed action during
%% the connection setup.
-define(SETUPTIME, 7000).

-include("net_address.hrl").

%%% BIF

-export([dflag_unicode_io/1]).

-spec dflag_unicode_io(pid()) -> boolean().

dflag_unicode_io(_) ->
    erlang:nif_error(undef).

%%% End of BIF

%% Interface functions

kernel_apply(M,F,A) ->         request({apply,M,F,A}).

-spec allow(Nodes) -> ok | error when
      Nodes :: [node()].
allow(Nodes) ->                request({allow, Nodes}).

longnames() ->                 request(longnames).

-spec stop() -> ok | {error, Reason} when
      Reason :: not_allowed | not_found.
stop() ->                      erl_distribution:stop().

node_info(Node) ->             get_node_info(Node).
node_info(Node, Key) ->        get_node_info(Node, Key).
nodes_info() ->                get_nodes_info().
i() ->                         print_info().
i(Node) ->                     print_info(Node).

verbose(Level) when is_integer(Level) ->
    request({verbose, Level}).

-spec set_net_ticktime(NetTicktime, TransitionPeriod) -> Res when
      NetTicktime :: pos_integer(),
      TransitionPeriod :: non_neg_integer(),
      Res :: unchanged
           | change_initiated
           | {ongoing_change_to, NewNetTicktime},
      NewNetTicktime :: pos_integer().
set_net_ticktime(T, TP) when is_integer(T), T > 0, is_integer(TP), TP >= 0 ->
    ticktime_res(request({new_ticktime, T*250, TP*1000})).

-spec set_net_ticktime(NetTicktime) -> Res when
      NetTicktime :: pos_integer(),
      Res :: unchanged
           | change_initiated
           | {ongoing_change_to, NewNetTicktime},
      NewNetTicktime :: pos_integer().
set_net_ticktime(T) when is_integer(T) ->
    set_net_ticktime(T, ?DEFAULT_TRANSITION_PERIOD).

-spec get_net_ticktime() -> Res when
      Res :: NetTicktime | {ongoing_change_to, NetTicktime} | ignored,
      NetTicktime :: pos_integer().
get_net_ticktime() ->
    ticktime_res(request(ticktime)).


%% The monitor_nodes() feature has been moved into the emulator.
%% The feature is reached via (intentionally) undocumented process
%% flags (we may want to move it elsewhere later). In order to easily
%% be backward compatible, errors are created here when process_flag()
%% fails.
-spec monitor_nodes(Flag) -> ok | Error when
      Flag :: boolean(),
      Error :: error | {error, term()}.
monitor_nodes(Flag) ->
    case catch process_flag(monitor_nodes, Flag) of
	true -> ok;
	false -> ok;
	_ -> mk_monitor_nodes_error(Flag, [])
    end.

-spec monitor_nodes(Flag, Options) -> ok | Error when
      Flag :: boolean(),
      Options :: [Option],
      Option :: {node_type, NodeType}
              | nodedown_reason,
      NodeType :: visible | hidden | all,
      Error :: error | {error, term()}.
monitor_nodes(Flag, Opts) ->
    case catch process_flag({monitor_nodes, Opts}, Flag) of
	true -> ok;
	false -> ok;
	_ -> mk_monitor_nodes_error(Flag, Opts)
    end.

%% ...
ticktime_res({A, I}) when is_atom(A), is_integer(I) -> {A, I div 250};
ticktime_res(I)      when is_integer(I)          -> I div 250;
ticktime_res(A)      when is_atom(A)             -> A.

%% Called though BIF's

connect(Node) ->               do_connect(Node, normal, false).
%%% Long timeout if blocked (== barred), only affects nodes with
%%% {dist_auto_connect, once} set.
passive_cnct(Node) ->              do_connect(Node, normal, true).
disconnect(Node) ->            request({disconnect, Node}).

%% connect but not seen
hidden_connect(Node) ->        do_connect(Node, hidden, false).

%% Should this node publish itself on Node?
publish_on_node(Node) when is_atom(Node) ->
    request({publish_on_node, Node}).

%% Update publication list
update_publish_nodes(Ns) ->
    request({update_publish_nodes, Ns}).

-spec connect_node(Node) -> boolean() | ignored when
      Node :: node().
%% explicit connects
connect_node(Node) when is_atom(Node) ->
    request({connect, normal, Node}).
hidden_connect_node(Node) when is_atom(Node) ->
    request({connect, hidden, Node}).

do_connect(Node, Type, WaitForBarred) -> %% Type = normal | hidden
    case catch ets:lookup(sys_dist, Node) of
	{'EXIT', _} ->
	    ?connect_failure(Node,{table_missing, sys_dist}),
	    false;
	[#barred_connection{}] ->
	    case WaitForBarred of
		false ->
		    false;
		true ->
		    Pid = spawn(?MODULE,passive_connect_monitor,[self(),Node]),
		    receive
			{Pid, true} ->
			    %%io:format("Net Kernel: barred connection (~p) "
			    %%          "connected from other end.~n",[Node]),
			    true;
			{Pid, false} ->
			    ?connect_failure(Node,{barred_connection,
						   ets:lookup(sys_dist, Node)}),
			    %%io:format("Net Kernel: barred connection (~p) "
			    %%      "- failure.~n",[Node]),
			    false
		    end
	    end;
	Else ->
	    case application:get_env(kernel, dist_auto_connect) of
		{ok, never} ->
		    ?connect_failure(Node,{dist_auto_connect,never}),
		    false;
		% This might happen due to connection close
		% not beeing propagated to user space yet.
		% Save the day by just not connecting...
		{ok, once} when Else =/= [],
				(hd(Else))#connection.state =:= up ->
		    ?connect_failure(Node,{barred_connection,
				ets:lookup(sys_dist, Node)}),
		    false;
		_ ->
		    request({connect, Type, Node})
	    end
    end.

passive_connect_monitor(Parent, Node) ->
    ok = monitor_nodes(true,[{node_type,all}]),
    case lists:member(Node,nodes([connected])) of
	true ->
	    ok = monitor_nodes(false,[{node_type,all}]),
	    Parent ! {self(),true};
	_ ->
	    Ref = make_ref(),
	    Tref = erlang:send_after(connecttime(),self(),Ref),
	    receive
		Ref ->
		    ok = monitor_nodes(false,[{node_type,all}]),
		    Parent ! {self(), false};
		{nodeup,Node,_} ->
		    ok = monitor_nodes(false,[{node_type,all}]),
		    _ = erlang:cancel_timer(Tref),
		    Parent ! {self(),true}
	    end
    end.

%% If the net_kernel isn't running we ignore all requests to the
%% kernel, thus basically accepting them :-)
request(Req) ->
    case whereis(net_kernel) of
	P when is_pid(P) ->
	    gen_server:call(net_kernel,Req,infinity);
	_ -> ignored
    end.

%% This function is used to dynamically start the
%% distribution.

start(Args) ->
    erl_distribution:start(Args).

%% This is the main startup routine for net_kernel
%% The defaults are longnames and a ticktime of 15 secs to the tcp_drv.

start_link([Name]) ->
    start_link([Name, longnames]);

start_link([Name, LongOrShortNames]) ->
    start_link([Name, LongOrShortNames, 15000]);

start_link([Name, LongOrShortNames, Ticktime]) ->
    case gen_server:start_link({local, net_kernel}, net_kernel,
			       {Name, LongOrShortNames, Ticktime}, []) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	_Error ->
	    exit(nodistribution)
    end.

%% auth:get_cookie should only be able to return an atom
%% tuple cookies are unknowns

init({Name, LongOrShortNames, TickT}) ->
    process_flag(trap_exit,true),
    case init_node(Name, LongOrShortNames) of
	{ok, Node, Listeners} ->
	    process_flag(priority, max),
	    Ticktime = to_integer(TickT),
	    Ticker = spawn_link(net_kernel, ticker, [self(), Ticktime]),
	    {ok, #state{name = Name,
			node = Node,
			type = LongOrShortNames,
			tick = #tick{ticker = Ticker, time = Ticktime},
			connecttime = connecttime(),
			connections =
			ets:new(sys_dist,[named_table,
					  protected,
					  {keypos, 2}]),
			listen = Listeners,
			allowed = [],
			verbose = 0
		       }};
	Error ->
	    {stop, Error}
    end.


%% ------------------------------------------------------------
%% handle_call.
%% ------------------------------------------------------------

%%
%% Set up a connection to Node.
%% The response is delayed until the connection is up and
%% running.
%%
handle_call({connect, _, Node}, From, State) when Node =:= node() ->
    async_reply({reply, true, State}, From);
handle_call({connect, Type, Node}, From, State) ->
    verbose({connect, Type, Node}, 1, State),
    case ets:lookup(sys_dist, Node) of
	[Conn] when Conn#connection.state =:= up ->
	    async_reply({reply, true, State}, From);
	[Conn] when Conn#connection.state =:= pending ->
	    Waiting = Conn#connection.waiting,
	    ets:insert(sys_dist, Conn#connection{waiting = [From|Waiting]}),
	    {noreply, State};
	[Conn] when Conn#connection.state =:= up_pending ->
	    Waiting = Conn#connection.waiting,
	    ets:insert(sys_dist, Conn#connection{waiting = [From|Waiting]}),
	    {noreply, State};
	_ ->
	    case setup(Node,Type,From,State) of
		{ok, SetupPid} ->
		    Owners = [{SetupPid, Node} | State#state.conn_owners],
		    {noreply,State#state{conn_owners=Owners}};
		_  ->
		    ?connect_failure(Node, {setup_call, failed}),
		    async_reply({reply, false, State}, From)
	    end
    end;

%%
%% Close the connection to Node.
%%
handle_call({disconnect, Node}, From, State) when Node =:= node() ->
    async_reply({reply, false, State}, From);
handle_call({disconnect, Node}, From, State) ->
    verbose({disconnect, Node}, 1, State),
    {Reply, State1} = do_disconnect(Node, State),
    async_reply({reply, Reply, State1}, From);

%%
%% The spawn/4 BIF ends up here.
%%
handle_call({spawn,M,F,A,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([no_link,{From,Tag},M,F,A,Gleader],[],State);

%%
%% The spawn_link/4 BIF ends up here.
%%
handle_call({spawn_link,M,F,A,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([link,{From,Tag},M,F,A,Gleader],[],State);

%%
%% The spawn_opt/5 BIF ends up here.
%%
handle_call({spawn_opt,M,F,A,O,L,Gleader},{From,Tag},State) when is_pid(From) ->
    do_spawn([L,{From,Tag},M,F,A,Gleader],O,State);

%%
%% Only allow certain nodes.
%%
handle_call({allow, Nodes}, From, State) ->
    case all_atoms(Nodes) of
	true ->
	    Allowed = State#state.allowed,
            async_reply({reply,ok,State#state{allowed = Allowed ++ Nodes}},
                        From);
	false ->
	    async_reply({reply,error,State}, From)
    end;

%%
%% authentication, used by auth. Simply works as this:
%% if the message comes through, the other node IS authorized.
%%
handle_call({is_auth, _Node}, From, State) ->
    async_reply({reply,yes,State}, From);

%%
%% Not applicable any longer !?
%%
handle_call({apply,_Mod,_Fun,_Args}, {From,Tag}, State)
  when is_pid(From), node(From) =:= node() ->
    async_gen_server_reply({From,Tag}, not_implemented),
%    Port = State#state.port,
%    catch apply(Mod,Fun,[Port|Args]),
    {noreply,State};

handle_call(longnames, From, State) ->
    async_reply({reply, get(longnames), State}, From);

handle_call({update_publish_nodes, Ns}, From, State) ->
    async_reply({reply, ok, State#state{publish_on_nodes = Ns}}, From);

handle_call({publish_on_node, Node}, From, State) ->
    NewState = case State#state.publish_on_nodes of
		   undefined ->
		       State#state{publish_on_nodes =
				   global_group:publish_on_nodes()};
		   _ ->
		       State
	       end,
    Publish = case NewState#state.publish_on_nodes of
		  all ->
		      true;
		  Nodes ->
		      lists:member(Node, Nodes)
	      end,
    async_reply({reply, Publish, NewState}, From);


handle_call({verbose, Level}, From, State) ->
    async_reply({reply, State#state.verbose, State#state{verbose = Level}},
                From);

%%
%% Set new ticktime
%%

%% The tick field of the state contains either a #tick{} or a
%% #tick_change{} record if the ticker process has been upgraded;
%% otherwise, an integer or an atom.

handle_call(ticktime, From, #state{tick = #tick{time = T}} = State) ->
    async_reply({reply, T, State}, From);
handle_call(ticktime, From, #state{tick = #tick_change{time = T}} = State) ->
    async_reply({reply, {ongoing_change_to, T}, State}, From);

handle_call({new_ticktime,T,_TP}, From, #state{tick = #tick{time = T}} = State) ->
    ?tckr_dbg(no_tick_change),
    async_reply({reply, unchanged, State}, From);

handle_call({new_ticktime,T,TP}, From, #state{tick = #tick{ticker = Tckr,
							time = OT}} = State) ->
    ?tckr_dbg(initiating_tick_change),
    start_aux_ticker(T, OT, TP),
    How = case T > OT of
	      true ->
		  ?tckr_dbg(longer_ticktime),
		  Tckr ! {new_ticktime,T},
		  longer;
	      false ->
		  ?tckr_dbg(shorter_ticktime),
		  shorter
	  end,
    async_reply({reply, change_initiated,
                 State#state{tick = #tick_change{ticker = Tckr,
                                                 time = T,
                                                 how = How}}}, From);

handle_call({new_ticktime,_T,_TP},
	    From,
	    #state{tick = #tick_change{time = T}} = State) ->
    async_reply({reply, {ongoing_change_to, T}, State}, From);

handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% ------------------------------------------------------------
%% handle_cast.
%% ------------------------------------------------------------

handle_cast(_, State) ->
    {noreply,State}.

%% ------------------------------------------------------------
%% code_change.
%% ------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

%% ------------------------------------------------------------
%% terminate.
%% ------------------------------------------------------------

terminate(no_network, State) ->
    lists:foreach(
      fun({Node, Type}) ->
	      case Type of
		  normal -> ?nodedown(Node, State);
		  _ -> ok
	      end
      end, get_up_nodes() ++ [{node(), normal}]);
terminate(_Reason, State) ->
    lists:foreach(
      fun(#listen {listen = Listen,module = Mod}) ->
	      Mod:close(Listen)
      end, State#state.listen),
    lists:foreach(
      fun({Node, Type}) ->
	      case Type of
		  normal -> ?nodedown(Node, State);
		  _ -> ok
	      end
      end, get_up_nodes() ++ [{node(), normal}]).


%% ------------------------------------------------------------
%% handle_info.
%% ------------------------------------------------------------

%%
%% accept a new connection.
%%
handle_info({accept,AcceptPid,Socket,Family,Proto}, State) ->
    MyNode = State#state.node,
    case get_proto_mod(Family,Proto,State#state.listen) of
	{ok, Mod} ->
	    Pid = Mod:accept_connection(AcceptPid,
					Socket,
					MyNode,
					State#state.allowed,
					State#state.connecttime),
	    AcceptPid ! {self(), controller, Pid},
	    {noreply,State};
	_ ->
	    AcceptPid ! {self(), unsupported_protocol},
	    {noreply, State}
    end;

%%
%% A node has successfully been connected.
%%
handle_info({SetupPid, {nodeup,Node,Address,Type,Immediate}},
	    State) ->
    case {Immediate, ets:lookup(sys_dist, Node)} of
	{true, [Conn]} when Conn#connection.state =:= pending,
			    Conn#connection.owner =:= SetupPid ->
	    ets:insert(sys_dist, Conn#connection{state = up,
						 address = Address,
						 waiting = [],
						 type = Type}),
	    SetupPid ! {self(), inserted},
	    reply_waiting(Node,Conn#connection.waiting, true),
	    {noreply, State};
	_ ->
	    SetupPid ! {self(), bad_request},
	    {noreply, State}
    end;

%%
%% Mark a node as pending (accept) if not busy.
%%
handle_info({AcceptPid, {accept_pending,MyNode,Node,Address,Type}}, State) ->
    case ets:lookup(sys_dist, Node) of
	[#connection{state=pending}=Conn] ->
	    if
		MyNode > Node ->
		    AcceptPid ! {self(),{accept_pending,nok_pending}},
		    {noreply,State};
		true ->
		    %%
		    %% A simultaneous connect has been detected and we want to
		    %% change pending process.
		    %%
		    OldOwner = Conn#connection.owner,
		    ?debug({net_kernel, remark, old, OldOwner, new, AcceptPid}),
		    exit(OldOwner, remarked),
		    receive
			{'EXIT', OldOwner, _} ->
			    true
		    end,
		    Owners = lists:keyreplace(OldOwner,
					      1,
					      State#state.conn_owners,
					      {AcceptPid, Node}),
		    ets:insert(sys_dist, Conn#connection{owner = AcceptPid}),
		    AcceptPid ! {self(),{accept_pending,ok_pending}},
		    State1 = State#state{conn_owners=Owners},
		    {noreply,State1}
	    end;
	[#connection{state=up}=Conn] ->
	    AcceptPid ! {self(), {accept_pending, up_pending}},
	    ets:insert(sys_dist, Conn#connection { pending_owner = AcceptPid,
						  state = up_pending }),
	    Pend = [{AcceptPid, Node} | State#state.pend_owners ],
	    {noreply, State#state { pend_owners = Pend }};
	[#connection{state=up_pending}] ->
	    AcceptPid ! {self(), {accept_pending, already_pending}},
	    {noreply, State};
	_ ->
	    ets:insert(sys_dist, #connection{node = Node,
					     state = pending,
					     owner = AcceptPid,
					     address = Address,
					     type = Type}),
	    AcceptPid ! {self(),{accept_pending,ok}},
	    Owners = [{AcceptPid,Node} | State#state.conn_owners],
	    {noreply, State#state{conn_owners = Owners}}
    end;

handle_info({SetupPid, {is_pending, Node}}, State) ->
    Reply = lists:member({SetupPid,Node},State#state.conn_owners),
    SetupPid ! {self(), {is_pending, Reply}},
    {noreply, State};


%%
%% Handle different types of process terminations.
%%
handle_info({'EXIT', From, Reason}, State) when is_pid(From) ->
    verbose({'EXIT', From, Reason}, 1, State),
    handle_exit(From, Reason, State);

%%
%% Handle badcookie and badname messages !
%%
handle_info({From,registered_send,To,Mess},State) ->
    send(From,To,Mess),
    {noreply,State};

%% badcookies SHOULD not be sent
%% (if someone does erlang:set_cookie(node(),foo) this may be)
handle_info({From,badcookie,_To,_Mess}, State) ->
    error_logger:error_msg("~n** Got OLD cookie from ~w~n",
			   [getnode(From)]),
    {_Reply, State1} = do_disconnect(getnode(From), State),
    {noreply,State1};

%%
%% Tick all connections.
%%
handle_info(tick, State) ->
    ?tckr_dbg(tick),
    lists:foreach(fun({Pid,_Node}) -> Pid ! {self(), tick} end,
		  State#state.conn_owners),
    {noreply,State};

handle_info(aux_tick, State) ->
    ?tckr_dbg(aux_tick),
    lists:foreach(fun({Pid,_Node}) -> Pid ! {self(), aux_tick} end,
		  State#state.conn_owners),
    {noreply,State};

handle_info(transition_period_end,
	    #state{tick = #tick_change{ticker = Tckr,
				       time = T,
				       how = How}} = State) ->
    ?tckr_dbg(transition_period_ended),
    case How of
	shorter -> Tckr ! {new_ticktime, T}, done;
	_       -> done
    end,
    {noreply,State#state{tick = #tick{ticker = Tckr, time = T}}};

handle_info(X, State) ->
    error_msg("Net kernel got ~w~n",[X]),
    {noreply,State}.

%% -----------------------------------------------------------
%% Handle exit signals.
%% We have 6 types of processes to handle.
%%
%%    1. The Listen process.
%%    2. The Accept process.
%%    3. Connection owning processes.
%%    4. The ticker process.
%%   (5. Garbage pid.)
%%
%% The process type function that handled the process throws
%% the handle_info return value !
%% -----------------------------------------------------------

handle_exit(Pid, Reason, State) ->
    catch do_handle_exit(Pid, Reason, State).

do_handle_exit(Pid, Reason, State) ->
    listen_exit(Pid, State),
    accept_exit(Pid, State),
    conn_own_exit(Pid, Reason, State),
    pending_own_exit(Pid, State),
    ticker_exit(Pid, State),
    {noreply,State}.

listen_exit(Pid, State) ->
    case lists:keymember(Pid, ?LISTEN_ID, State#state.listen) of
	true ->
	    error_msg("** Netkernel terminating ... **\n", []),
	    throw({stop,no_network,State});
	false ->
	    false
    end.

accept_exit(Pid, State) ->
    Listen = State#state.listen,
    case lists:keysearch(Pid, ?ACCEPT_ID, Listen) of
	{value, ListenR} ->
	    ListenS = ListenR#listen.listen,
	    Mod = ListenR#listen.module,
	    AcceptPid = Mod:accept(ListenS),
	    L = lists:keyreplace(Pid, ?ACCEPT_ID, Listen,
				 ListenR#listen{accept = AcceptPid}),
	    throw({noreply, State#state{listen = L}});
	_ ->
	    false
    end.

conn_own_exit(Pid, Reason, State) ->
    Owners = State#state.conn_owners,
    case lists:keysearch(Pid, 1, Owners) of
	{value, {Pid, Node}} ->
	    throw({noreply, nodedown(Pid, Node, Reason, State)});
	_ ->
	    false
    end.

pending_own_exit(Pid, State) ->
    Pend = State#state.pend_owners,
    case lists:keysearch(Pid, 1, Pend) of
	{value, {Pid, Node}} ->
	    NewPend = lists:keydelete(Pid, 1, Pend),
	    State1 = State#state { pend_owners = NewPend },
	    case get_conn(Node) of
		{ok, Conn} when Conn#connection.state =:= up_pending ->
		    reply_waiting(Node,Conn#connection.waiting, true),
		    Conn1 = Conn#connection { state = up,
					      waiting = [],
					      pending_owner = undefined },
		    ets:insert(sys_dist, Conn1);
		_ ->
		    ok
	    end,
	    throw({noreply, State1});
	_ ->
	    false
    end.

ticker_exit(Pid, #state{tick = #tick{ticker = Pid, time = T} = Tck} = State) ->
    Tckr = restart_ticker(T),
    throw({noreply, State#state{tick = Tck#tick{ticker = Tckr}}});
ticker_exit(Pid, #state{tick = #tick_change{ticker = Pid,
					    time = T} = TckCng} = State) ->
    Tckr = restart_ticker(T),
    throw({noreply, State#state{tick = TckCng#tick_change{ticker = Tckr}}});
ticker_exit(_, _) ->
    false.

%% -----------------------------------------------------------
%% A node has gone down !!
%% nodedown(Owner, Node, Reason, State) -> State'
%% -----------------------------------------------------------

nodedown(Owner, Node, Reason, State) ->
    case get_conn(Node) of
	{ok, Conn} ->
	    nodedown(Conn, Owner, Node, Reason, Conn#connection.type, State);
	_ ->
	    State
    end.

get_conn(Node) ->
    case ets:lookup(sys_dist, Node) of
	[Conn = #connection{}] -> {ok, Conn};
	_      -> error
    end.

nodedown(Conn, Owner, Node, Reason, Type, OldState) ->
    Owners = lists:keydelete(Owner, 1, OldState#state.conn_owners),
    State = OldState#state{conn_owners = Owners},
    case Conn#connection.state of
	pending when Conn#connection.owner =:= Owner ->
	    pending_nodedown(Conn, Node, Type, State);
	up when Conn#connection.owner =:= Owner ->
	    up_nodedown(Conn, Node, Reason, Type, State);
	up_pending when Conn#connection.owner =:= Owner ->
	    up_pending_nodedown(Conn, Node, Reason, Type, State);
	_ ->
	    OldState
    end.

pending_nodedown(Conn, Node, Type, State) ->
    % Don't bar connections that have never been alive
    %mark_sys_dist_nodedown(Node),
    % - instead just delete the node:
    ets:delete(sys_dist, Node),
    reply_waiting(Node,Conn#connection.waiting, false),
    case Type of
	normal ->
	    ?nodedown(Node, State);
	_ ->
	    ok
    end,
    State.

up_pending_nodedown(Conn, Node, _Reason, _Type, State) ->
    AcceptPid = Conn#connection.pending_owner,
    Owners = State#state.conn_owners,
    Pend = lists:keydelete(AcceptPid, 1, State#state.pend_owners),
    Conn1 = Conn#connection { owner = AcceptPid,
			      pending_owner = undefined,
			      state = pending },
    ets:insert(sys_dist, Conn1),
    AcceptPid ! {self(), pending},
    State#state{conn_owners = [{AcceptPid,Node}|Owners], pend_owners = Pend}.


up_nodedown(_Conn, Node, _Reason, Type, State) ->
    mark_sys_dist_nodedown(Node),
    case Type of
	normal -> ?nodedown(Node, State);
	_ -> ok
    end,
    State.

mark_sys_dist_nodedown(Node) ->
    case application:get_env(kernel, dist_auto_connect) of
	{ok, once} ->
	    ets:insert(sys_dist, #barred_connection{node = Node});
	_ ->
	    ets:delete(sys_dist, Node)
    end.

%% -----------------------------------------------------------
%% End handle_exit/2 !!
%% -----------------------------------------------------------


%% -----------------------------------------------------------
%% monitor_nodes/[1,2] errors
%% -----------------------------------------------------------

check_opt(Opt, Opts) ->
    check_opt(Opt, Opts, false, []).

check_opt(_Opt, [], false, _OtherOpts) ->
    false;
check_opt(_Opt, [], {true, ORes}, OtherOpts) ->
    {true, ORes, OtherOpts};
check_opt(Opt, [Opt|RestOpts], false, OtherOpts) ->
    check_opt(Opt, RestOpts, {true, Opt}, OtherOpts);
check_opt(Opt, [Opt|RestOpts], {true, Opt} = ORes, OtherOpts) ->
    check_opt(Opt, RestOpts, ORes, OtherOpts);
check_opt({Opt, value}=TOpt,
	  [{Opt, _Val}=ORes|RestOpts],
	  false,
	  OtherOpts) ->
    check_opt(TOpt, RestOpts, {true, ORes}, OtherOpts);
check_opt({Opt, value}=TOpt,
	  [{Opt, _Val}=ORes|RestOpts],
	  {true, ORes}=TORes,
	  OtherOpts) ->
    check_opt(TOpt, RestOpts, TORes, OtherOpts);
check_opt({Opt, value},
	  [{Opt, _Val} = ORes1| _RestOpts],
	  {true, {Opt, _OtherVal} = ORes2},
	  _OtherOpts) ->
    throw({error, {option_value_mismatch, [ORes1, ORes2]}});
check_opt(Opt, [OtherOpt | RestOpts], TORes, OtherOpts) ->
    check_opt(Opt, RestOpts, TORes, [OtherOpt | OtherOpts]).

check_options(Opts) when is_list(Opts) ->
    RestOpts1 = case check_opt({node_type, value}, Opts) of
		    {true, {node_type,Type}, RO1} when Type =:= visible;
						       Type =:= hidden;
						       Type =:= all ->
			RO1;
		    {true, {node_type, _Type} = Opt, _RO1} ->
			throw({error, {bad_option_value, Opt}});
		    false ->
			Opts
		end,
    RestOpts2 = case check_opt(nodedown_reason, RestOpts1) of
		    {true, nodedown_reason, RO2} ->
			RO2;
		    false ->
			RestOpts1
		end,
    case RestOpts2 of
	[] ->
	    %% This should never happen since we only call this function
	    %% when we know there is an error in the option list
	    {error, internal_error};
	_ ->
	    {error, {unknown_options, RestOpts2}}
    end;
check_options(Opts) ->
    {error, {options_not_a_list, Opts}}.

mk_monitor_nodes_error(Flag, _Opts) when Flag =/= true, Flag =/= false ->
    error;
mk_monitor_nodes_error(_Flag, Opts) ->
    case catch check_options(Opts) of
	{error, _} = Error ->
	    Error;
	UnexpectedError ->
	    {error, {internal_error, UnexpectedError}}
    end.

% -------------------------------------------------------------

do_disconnect(Node, State) ->
    case ets:lookup(sys_dist, Node) of
	[Conn] when Conn#connection.state =:= up ->
	    disconnect_pid(Conn#connection.owner, State);
	[Conn] when Conn#connection.state =:= up_pending ->
	    disconnect_pid(Conn#connection.owner, State);
	_ ->
	    {false, State}
    end.


disconnect_pid(Pid, State) ->
    exit(Pid, disconnect),
    %% Sync wait for connection to die!!!
    receive
	{'EXIT',Pid,Reason} ->
	    {_,State1} = handle_exit(Pid, Reason, State),
	    {true, State1}
    end.

%%
%%
%%
get_nodes(Which) ->
    get_nodes(ets:first(sys_dist), Which).

get_nodes('$end_of_table', _) ->
    [];
get_nodes(Key, Which) ->
    case ets:lookup(sys_dist, Key) of
	[Conn = #connection{state = up}] ->
	    [Conn#connection.node | get_nodes(ets:next(sys_dist, Key),
					      Which)];
	[Conn = #connection{}] when Which =:= all ->
	    [Conn#connection.node | get_nodes(ets:next(sys_dist, Key),
					      Which)];
	_ ->
	    get_nodes(ets:next(sys_dist, Key), Which)
    end.

%% Return a list of all nodes that are 'up'.
get_up_nodes() ->
    get_up_nodes(ets:first(sys_dist)).

get_up_nodes('$end_of_table') -> [];
get_up_nodes(Key) ->
    case ets:lookup(sys_dist, Key) of
 	[#connection{state=up,node=Node,type=Type}] ->
 	    [{Node,Type}|get_up_nodes(ets:next(sys_dist, Key))];
 	_ ->
 	    get_up_nodes(ets:next(sys_dist, Key))
    end.

ticker(Kernel, Tick) when is_integer(Tick) ->
    process_flag(priority, max),
    ?tckr_dbg(ticker_started),
    ticker_loop(Kernel, Tick).

to_integer(T) when is_integer(T) -> T;
to_integer(T) when is_atom(T) ->
    list_to_integer(atom_to_list(T));
to_integer(T) when is_list(T) ->
    list_to_integer(T).

ticker_loop(Kernel, Tick) ->
    receive
	{new_ticktime, NewTick} ->
	    ?tckr_dbg({ticker_changed_time, Tick, NewTick}),
	    ?MODULE:ticker_loop(Kernel, NewTick)
    after Tick ->
	    Kernel ! tick,
	    ?MODULE:ticker_loop(Kernel, Tick)
    end.

start_aux_ticker(NewTick, OldTick, TransitionPeriod) ->
    spawn_link(?MODULE, aux_ticker,
	       [self(), NewTick, OldTick, TransitionPeriod]).

aux_ticker(NetKernel, NewTick, OldTick, TransitionPeriod) ->
    process_flag(priority, max),
    ?tckr_dbg(aux_ticker_started),
    TickInterval = case NewTick > OldTick of
		       true  -> OldTick;
		       false -> NewTick
		   end,
    NoOfTicks = case TransitionPeriod > 0 of
		    true ->
			%% 1 tick to start
			%% + ticks to cover the transition period
			1 + (((TransitionPeriod - 1) div TickInterval) + 1);
		    false ->
			1
		end,
    aux_ticker1(NetKernel, TickInterval, NoOfTicks).

aux_ticker1(NetKernel, _, 1) ->
    NetKernel ! transition_period_end,
    NetKernel ! aux_tick,
    bye;
aux_ticker1(NetKernel, TickInterval, NoOfTicks) ->
    NetKernel ! aux_tick,
    receive
    after TickInterval ->
	    aux_ticker1(NetKernel, TickInterval, NoOfTicks-1)
    end.

send(_From,To,Mess) ->
    case whereis(To) of
	undefined ->
	    Mess;
	P when is_pid(P) ->
	    P ! Mess
    end.

-ifdef(UNUSED).

safesend(Name,Mess) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    Mess;
	P when is_pid(P) ->
	    P ! Mess
    end;
safesend(Pid, Mess) -> Pid ! Mess.

-endif.

do_spawn(SpawnFuncArgs, SpawnOpts, State) ->
    [_,From|_] = SpawnFuncArgs,
    case catch spawn_opt(?MODULE, spawn_func, SpawnFuncArgs, SpawnOpts) of
	{'EXIT', {Reason,_}} ->
            async_reply({reply, {'EXIT', {Reason,[]}}, State}, From);
	{'EXIT', Reason} ->
	    async_reply({reply, {'EXIT', {Reason,[]}}, State}, From);
	_ ->
	    {noreply,State}
    end.

%% This code is really intricate. The link will go first and then comes
%% the pid, This means that the client need not do a network link.
%% If the link message would not arrive, the runtime system shall
%% generate a nodedown message

spawn_func(link,{From,Tag},M,F,A,Gleader) ->
    link(From),
    gen_server:reply({From,Tag},self()),  %% ahhh
    group_leader(Gleader,self()),
    apply(M,F,A);
spawn_func(_,{From,Tag},M,F,A,Gleader) ->
    gen_server:reply({From,Tag},self()),  %% ahhh
    group_leader(Gleader,self()),
    apply(M,F,A).

%% -----------------------------------------------------------
%% Set up connection to a new node.
%% -----------------------------------------------------------

setup(Node,Type,From,State) ->
    Allowed = State#state.allowed,
    case lists:member(Node, Allowed) of
	false when Allowed =/= [] ->
	    error_msg("** Connection attempt with "
		      "disallowed node ~w ** ~n", [Node]),
	    {error, bad_node};
	_ ->
	    case select_mod(Node, State#state.listen) of
		{ok, L} ->
		    Mod = L#listen.module,
		    LAddr = L#listen.address,
		    MyNode = State#state.node,
		    Pid = Mod:setup(Node,
				    Type,
				    MyNode,
				    State#state.type,
				    State#state.connecttime),
		    Addr = LAddr#net_address {
					      address = undefined,
					      host = undefined },
		    ets:insert(sys_dist, #connection{node = Node,
						     state = pending,
						     owner = Pid,
						     waiting = [From],
						     address = Addr,
						     type = normal}),
		    {ok, Pid};
		Error ->
		    Error
	    end
    end.

%%
%% Find a module that is willing to handle connection setup to Node
%%
select_mod(Node, [L|Ls]) ->
    Mod = L#listen.module,
    case Mod:select(Node) of
	true -> {ok, L};
	false -> select_mod(Node, Ls)
    end;
select_mod(Node, []) ->
    {error, {unsupported_address_type, Node}}.


get_proto_mod(Family,Protocol,[L|Ls]) ->
    A = L#listen.address,
    if A#net_address.family =:= Family,
       A#net_address.protocol =:= Protocol ->
	    {ok, L#listen.module};
       true ->
	    get_proto_mod(Family,Protocol,Ls)
    end;
get_proto_mod(_Family, _Protocol, []) ->
    error.

%% -------- Initialisation functions ------------------------

init_node(Name, LongOrShortNames) ->
    {NameWithoutHost,_Host} = lists:splitwith(fun($@)->false;(_)->true end,
				  atom_to_list(Name)),
    case create_name(Name, LongOrShortNames, 1) of
	{ok,Node} ->
	    case start_protos(list_to_atom(NameWithoutHost),Node) of
		{ok, Ls} ->
		    {ok, Node, Ls};
		Error ->
		    Error
	    end;
	Error ->
 	    Error
    end.

%% Create the node name
create_name(Name, LongOrShortNames, Try) ->
    put(longnames, case LongOrShortNames of
		       shortnames -> false;
		       longnames -> true
		   end),
    {Head,Host1} = create_hostpart(Name, LongOrShortNames),
    case Host1 of
	{ok,HostPart} ->
	    {ok,list_to_atom(Head ++ HostPart)};
	{error,long} when Try =:= 1 ->
	    %% It could be we haven't read domain name from resolv file yet
	    inet_config:do_load_resolv(os:type(), longnames),
	    create_name(Name, LongOrShortNames, 0);
	{error,Type} ->
	    error_logger:info_msg(
	      lists:concat(["Can\'t set ",
			    Type,
			    " node name!\n"
			    "Please check your configuration\n"])),
	    {error,badarg}
    end.

create_hostpart(Name, LongOrShortNames) ->
    {Head,Host} = lists:splitwith(fun($@)->false;(_)->true end,
				  atom_to_list(Name)),
    Host1 = case {Host,LongOrShortNames} of
		{[$@,_|_],longnames} ->
		    {ok,Host};
		{[$@,_|_],shortnames} ->
		    case lists:member($.,Host) of
			true -> {error,short};
			_ -> {ok,Host}
		    end;
		{_,shortnames} ->
		    case inet_db:gethostname() of
			H when is_list(H), length(H)>0 ->
			    {ok,"@" ++ H};
			_ ->
			    {error,short}
		    end;
		{_,longnames} ->
		    case {inet_db:gethostname(),inet_db:res_option(domain)} of
			{H,D} when is_list(D), is_list(H),
                        length(D)> 0, length(H)>0 ->
			    {ok,"@" ++ H ++ "." ++ D};
			_ ->
			    {error,long}
		    end
	    end,
    {Head,Host1}.

%%
%%
%%
protocol_childspecs() ->
    case init:get_argument(proto_dist) of
	{ok, [Protos]} ->
	    protocol_childspecs(Protos);
	_ ->
	    protocol_childspecs(["inet_tcp"])
    end.

protocol_childspecs([]) ->
    [];
protocol_childspecs([H|T]) ->
    Mod = list_to_atom(H ++ "_dist"),
    case (catch Mod:childspecs()) of
	{ok, Childspecs} when is_list(Childspecs) ->
	    Childspecs ++ protocol_childspecs(T);
	_ ->
	    protocol_childspecs(T)
    end.


%%
%% epmd_module() -> module_name of erl_epmd or similar gen_server_module.
%%

epmd_module() ->
    case init:get_argument(epmd_module) of
	{ok,[[Module]]} ->
	    list_to_atom(Module);
	_ ->
	    erl_epmd
    end.

%%
%% Start all protocols
%%

start_protos(Name,Node) ->
    case init:get_argument(proto_dist) of
	{ok, [Protos]} ->
	    start_protos(Name,Protos, Node);
	_ ->
	    start_protos(Name,["inet_tcp"], Node)
    end.

start_protos(Name,Ps, Node) ->
    case start_protos(Name, Ps, Node, []) of
	[] -> {error, badarg};
	Ls -> {ok, Ls}
    end.

start_protos(Name, [Proto | Ps], Node, Ls) ->
    Mod = list_to_atom(Proto ++ "_dist"),
    case catch Mod:listen(Name) of
	{ok, {Socket, Address, Creation}} ->
	    case set_node(Node, Creation) of
		ok ->
		    AcceptPid = Mod:accept(Socket),
		    auth:sync_cookie(),
		    L = #listen {
		      listen = Socket,
		      address = Address,
		      accept = AcceptPid,
		      module = Mod },
		    start_protos(Name,Ps, Node, [L|Ls]);
		_ ->
		    Mod:close(Socket),
		    error_logger:info_msg("Invalid node name: ~p~n", [Node]),
		    start_protos(Name, Ps, Node, Ls)
	    end;
	{'EXIT', {undef,_}} ->
	    error_logger:info_msg("Protocol: ~tp: not supported~n", [Proto]),
	    start_protos(Name,Ps, Node, Ls);
	{'EXIT', Reason} ->
	    error_logger:info_msg("Protocol: ~tp: register error: ~tp~n",
				  [Proto, Reason]),
	    start_protos(Name,Ps, Node, Ls);
	{error, duplicate_name} ->
	    error_logger:info_msg("Protocol: ~tp: the name " ++
				  atom_to_list(Node) ++
				  " seems to be in use by another Erlang node",
				  [Proto]),
	    start_protos(Name,Ps, Node, Ls);
	{error, Reason} ->
	    error_logger:info_msg("Protocol: ~tp: register/listen error: ~tp~n",
				  [Proto, Reason]),
	    start_protos(Name,Ps, Node, Ls)
    end;
start_protos(_,[], _Node, Ls) ->
    Ls.

set_node(Node, Creation) when node() =:= nonode@nohost ->
    case catch erlang:setnode(Node, Creation) of
	true ->
	    ok;
	{'EXIT',Reason} ->
	    {error,Reason}
    end;
set_node(Node, _Creation) when node() =:= Node ->
    ok.

connecttime() ->
    case application:get_env(kernel, net_setuptime) of
	{ok,Time} when is_number(Time), Time >= 120 ->
	    120 * 1000;
	{ok,Time} when is_number(Time), Time > 0 ->
	    round(Time * 1000);
	_ ->
	    ?SETUPTIME
    end.

%% -------- End initialisation functions --------------------

%% ------------------------------------------------------------
%% Node information.
%% ------------------------------------------------------------

get_node_info(Node) ->
    case ets:lookup(sys_dist, Node) of
	[Conn = #connection{owner = Owner, state = State}] ->
	    case get_status(Owner, Node, State) of
		{ok, In, Out} ->
		    {ok, [{owner, Owner},
			  {state, State},
			  {address, Conn#connection.address},
			  {type, Conn#connection.type},
			  {in, In},
			  {out, Out}]};
		_ ->
		    {error, bad_node}
	    end;
	_ ->
	    {error, bad_node}
    end.

%%
%% We can't do monitor_node here incase the node is pending,
%% the monitor_node/2 call hangs until the connection is ready.
%% We will not ask about in/out information either for pending
%% connections as this also would block this call awhile.
%%
get_status(Owner, Node, up) ->
    monitor_node(Node, true),
    Owner ! {self(), get_status},
    receive
	{Owner, get_status, Res} ->
	    monitor_node(Node, false),
	    Res;
	{nodedown, Node} ->
	    error
    end;
get_status(_, _, _) ->
    {ok, 0, 0}.

get_node_info(Node, Key) ->
    case get_node_info(Node) of
	{ok, Info} ->
	    case lists:keysearch(Key, 1, Info) of
		{value, {Key, Value}} -> {ok, Value};
		_                     -> {error, invalid_key}
	    end;
	Error ->
	    Error
    end.

get_nodes_info() ->
    get_nodes_info(get_nodes(all), []).

get_nodes_info([Node|Nodes], InfoList) ->
    case get_node_info(Node) of
	{ok, Info} -> get_nodes_info(Nodes, [{Node, Info}|InfoList]);
	_          -> get_nodes_info(Nodes, InfoList)
    end;
get_nodes_info([], InfoList) ->
    {ok, InfoList}.

%% ------------------------------------------------------------
%% Misc. functions
%% ------------------------------------------------------------

reply_waiting(_Node, Waiting, Rep) ->
    case Rep of
	false ->
	    ?connect_failure(_Node, {setup_process, failure});
	_ ->
	    ok
    end,
    reply_waiting1(lists:reverse(Waiting), Rep).

reply_waiting1([From|W], Rep) ->
    async_gen_server_reply(From, Rep),
    reply_waiting1(W, Rep);
reply_waiting1([], _) ->
    ok.


-ifdef(UNUSED).

delete_all(From, [From |Tail]) -> delete_all(From, Tail);
delete_all(From, [H|Tail]) ->  [H|delete_all(From, Tail)];
delete_all(_, []) -> [].

-endif.

all_atoms([]) -> true;
all_atoms([N|Tail]) when is_atom(N) ->
    all_atoms(Tail);
all_atoms(_) -> false.

%% It is assumed that only net_kernel uses restart_ticker()
restart_ticker(Time) ->
    ?tckr_dbg(restarting_ticker),
    self() ! aux_tick,
    spawn_link(?MODULE, ticker, [self(), Time]).

%% ------------------------------------------------------------
%% Print status information.
%% ------------------------------------------------------------

print_info() ->
    nformat("Node", "State", "Type", "In", "Out", "Address"),
    {ok, NodesInfo} = nodes_info(),
    {In,Out} = lists:foldl(fun display_info/2, {0,0}, NodesInfo),
    nformat("Total", "", "",
	    integer_to_list(In), integer_to_list(Out), "").

display_info({Node, Info}, {I,O}) ->
    State = atom_to_list(fetch(state, Info)),
    In = fetch(in, Info),
    Out = fetch(out, Info),
    Type = atom_to_list(fetch(type, Info)),
    Address = fmt_address(fetch(address, Info)),
    nformat(atom_to_list(Node), State, Type,
	    integer_to_list(In), integer_to_list(Out), Address),
    {I+In,O+Out}.

fmt_address(undefined) ->
    "-";
fmt_address(A) ->
    case A#net_address.family of
	inet ->
	    case A#net_address.address of
		{IP,Port} ->
		    inet_parse:ntoa(IP) ++ ":" ++ integer_to_list(Port);
		_ -> "-"
	    end;
	inet6 ->
	    case A#net_address.address of
		{IP,Port} ->
		    inet_parse:ntoa(IP) ++ "/" ++ integer_to_list(Port);
		_ -> "-"
	    end;
	_ ->
	    lists:flatten(io_lib:format("~p", [A#net_address.address]))
    end.


fetch(Key, Info) ->
    case lists:keysearch(Key, 1, Info) of
	{value, {_, Val}} -> Val;
	false -> 0
    end.

nformat(A1, A2, A3, A4, A5, A6) ->
    io:format("~-20s ~-7s ~-6s ~8s ~8s ~s~n", [A1,A2,A3,A4,A5,A6]).

print_info(Node) ->
    case node_info(Node) of
	{ok, Info} ->
	    State = fetch(state, Info),
	    In = fetch(in, Info),
	    Out = fetch(out, Info),
	    Type = fetch(type, Info),
	    Address = fmt_address(fetch(address, Info)),
	    io:format("Node     = ~p~n"
		      "State    = ~p~n"
		      "Type     = ~p~n"
		      "In       = ~p~n"
		      "Out      = ~p~n"
		      "Address  = ~s~n",
		      [Node, State, Type, In, Out, Address]);
	Error ->
	    Error
    end.

verbose(Term, Level, #state{verbose = Verbose}) when Verbose >= Level ->
    error_logger:info_report({net_kernel, Term});
verbose(_, _, _) ->
    ok.

getnode(P) when is_pid(P) -> node(P);
getnode(P) -> P.

async_reply({reply, Msg, State}, From) ->
    async_gen_server_reply(From, Msg),
    {noreply, State}.

async_gen_server_reply(From, Msg) ->
    {Pid, Tag} = From,
    M = {Tag, Msg},
    case catch erlang:send(Pid, M, [nosuspend, noconnect]) of
        ok ->
            ok;
        nosuspend ->
            _ = spawn(fun() -> catch erlang:send(Pid, M, [noconnect]) end),
	    ok;
        noconnect ->
            ok; % The gen module takes care of this case.
        {'EXIT', _} ->
            ok
    end.
