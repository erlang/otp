%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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

%%%----------------------------------------------------------------------
%%% File    : erl_link_SUITE.erl
%%% Author  : Rickard Green <rickard.green@uab.ericsson.se>
%%% Purpose : Test erlang links
%%% Created : 13 Dec 2001 by Rickard Green <rickard.green@uab.ericsson.se>
%%%----------------------------------------------------------------------

-module(erl_link_SUITE).
-author('rickard.green@uab.ericsson.se').

%-define(line_trace, 1).
-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

% Test cases
-export([links/1,
	 dist_links/1,
	 monitor_nodes/1,
	 process_monitors/1,
	 dist_process_monitors/1,
	 busy_dist_port_monitor/1,
	 busy_dist_port_link/1,
	 otp_5772_link/1,
	 otp_5772_dist_link/1,
	 otp_5772_monitor/1,
	 otp_5772_dist_monitor/1,
	 otp_7946/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

% Internal exports
-export([test_proc/0]).


-define(LINK_UNDEF, 0).
-define(LINK_PID,   1).
-define(LINK_NODE,  3).


% These are to be kept in sync with erl_monitors.h 
-define(MON_ORIGIN, 1).
-define(MON_TARGET, 3).


-record(erl_link, {type = ?LINK_UNDEF,
		   pid = [],
		   targets = []}).

% This is to be kept in sync with erl_bif_info.c (make_monitor_list)

-record(erl_monitor, {
            type, % MON_ORIGIN or MON_TARGET (1 or 3)
	    ref,
	    pid, % Process or nodename
	    name = [] % registered name or []
          }).



suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [links, dist_links, monitor_nodes, process_monitors,
     dist_process_monitors, busy_dist_port_monitor,
     busy_dist_port_link, otp_5772_link, otp_5772_dist_link,
     otp_5772_monitor, otp_5772_dist_monitor, otp_7946].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    catch erts_debug:set_internal_state(available_internal_state, false).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


links(doc) -> ["Tests node local links"];
links(suite) -> [];
links(Config) when is_list(Config) ->
    ?line common_link_test(node(), node()),
    ?line true = link(self()),
    ?line [] = find_erl_link(self(), ?LINK_PID, self()),
    ?line true = unlink(self()),
    ?line ok.

dist_links(doc) -> ["Tests distributed links"];
dist_links(suite) -> [];
dist_links(Config) when is_list(Config) ->
    ?line [NodeName] = get_names(1, dist_link),
    ?line {ok, Node} = start_node(NodeName),
    ?line common_link_test(node(), Node),
    ?line TP4 = spawn(?MODULE, test_proc, []),
    ?line TP5 = spawn(?MODULE, test_proc, []),
    ?line TP6 = spawn(Node, ?MODULE, test_proc, []),
    ?line true = tp_call(TP6, fun() -> link(TP4) end),
    ?line check_link(TP4, TP6),
    ?line true = tp_call(TP5,
			 fun() ->
				 process_flag(trap_exit,true),
				 link(TP6)
			 end),
    ?line check_link(TP5, TP6),
    ?line rpc:cast(Node, erlang, halt, []),
    ?line wait_until(fun () -> ?line is_proc_dead(TP4) end),
    ?line check_unlink(TP4, TP6),
    ?line true = tp_call(TP5,
			 fun() ->
				 receive
				     {'EXIT', TP6, noconnection} ->
					 true
				 end
			 end),
    ?line check_unlink(TP5, TP6),
    ?line tp_cast(TP5, fun() -> exit(normal) end),
    ?line ok.

common_link_test(NodeA, NodeB) ->
    ?line TP1 = spawn(NodeA, ?MODULE, test_proc, []),
    ?line check_unlink(TP1, self()),
    ?line TP2 = tp_call(TP1,
			fun () ->
				spawn_link(NodeB, ?MODULE, test_proc, [])
			end),
    ?line check_link(TP1, TP2),
    ?line true = tp_call(TP2, fun() -> unlink(TP1) end),
    ?line check_unlink(TP1, TP2),
    ?line true = tp_call(TP2, fun() -> link(TP1) end),
    ?line check_link(TP1, TP2),
    ?line false = tp_call(TP2, fun() -> process_flag(trap_exit, true) end),
    ?line tp_cast(TP1, fun () -> exit(died) end),
    ?line true = tp_call(TP2, fun() ->
				      receive 
					  {'EXIT', TP1, died} ->
					      true
				      end
			      end),
    ?line check_unlink(TP1, TP2),
    ?line TP3 = tp_call(TP2,
			fun () ->
				spawn_link(NodeA, ?MODULE, test_proc, [])
			end),
    ?line check_link(TP3, TP2),
    ?line tp_cast(TP2, fun() -> exit(died) end),
    ?line wait_until(fun () -> ?line is_proc_dead(TP3) end),
    ?line check_unlink(TP3, TP2),
    ?line ok.

monitor_nodes(doc) -> ["Tests monitor of nodes"];
monitor_nodes(suite) -> [];
monitor_nodes(Config) when is_list(Config) ->
    ?line [An, Bn, Cn, Dn] = get_names(4, dist_link),
    ?line {ok, A} = start_node(An),
    ?line {ok, B} = start_node(Bn),
    ?line C = list_to_atom(lists:concat([Cn, "@", hostname()])),
    ?line D = list_to_atom(lists:concat([Dn, "@", hostname()])),
    ?line 0 = no_of_monitor_node(self(), A),
    ?line 0 = no_of_monitor_node(self(), B),
    ?line monitor_node(A, true),
    ?line monitor_node(B, true),
    ?line monitor_node(D, true),
    ?line monitor_node(D, true),

    %% Has been known to crash the emulator.
    ?line {memory,_} = process_info(self(), memory),

    ?line monitor_node(A, false),
    ?line monitor_node(B, true),
    ?line monitor_node(C, true),
    ?line monitor_node(C, false),
    ?line monitor_node(C, true),
    ?line monitor_node(B, true),
    ?line monitor_node(A, false),
    ?line monitor_node(B, true),
    ?line monitor_node(B, false),
    ?line monitor_node(A, true),
    ?line check_monitor_node(self(), A, 1),
    ?line check_monitor_node(self(), B, 3),
    ?line check_monitor_node(self(), C, 0),
    ?line check_monitor_node(self(), D, 0),
    ?line receive {nodedown, C} -> ok end,
    ?line receive {nodedown, C} -> ok end,
    ?line receive {nodedown, C} -> ok end,
    ?line receive {nodedown, D} -> ok end,
    ?line receive {nodedown, D} -> ok end,
    ?line stop_node(A),
    ?line receive {nodedown, A} -> ok end,
    ?line check_monitor_node(self(), A, 0),
    ?line check_monitor_node(self(), B, 3),
    ?line stop_node(B),
    ?line receive {nodedown, B} -> ok end,
    ?line receive {nodedown, B} -> ok end,
    ?line receive {nodedown, B} -> ok end,
    ?line check_monitor_node(self(), B, 0),
    ?line receive
	      {nodedown, X} ->
		  ?line ?t:fail({unexpected_nodedown, X})
	  after 0 ->
		  ?line ok
	  end,
    ?line ok.
    

process_monitors(doc) -> ["Tests node local process monitors"];
process_monitors(suite) -> [];
process_monitors(Config) when is_list(Config) ->
    ?line common_process_monitors(node(), node()),
    ?line Mon1 = erlang:monitor(process,self()),
    ?line [] = find_erl_monitor(self(), Mon1),
    ?line [Name] = get_names(1, process_monitors),
    ?line true = register(Name, self()),
    ?line Mon2 = erlang:monitor(process, Name),
    ?line [] = find_erl_monitor(self(), Mon2),
    ?line receive
	      {'DOWN', Mon1, _, _, _} = Msg ->
		  ?line ?t:fail({unexpected_down_msg, Msg});
	      {'DOWN', Mon2, _, _, _} = Msg ->
		  ?line ?t:fail({unexpected_down_msg, Msg})
	  after 500 ->
		  ?line true = erlang:demonitor(Mon1),
		  ?line true = erlang:demonitor(Mon2),
		  ?line ok
	  end.

dist_process_monitors(doc) -> ["Tests distributed process monitors"];
dist_process_monitors(suite) -> [];
dist_process_monitors(Config) when is_list(Config) -> 
    ?line [Name] = get_names(1,dist_process_monitors),
    ?line {ok, Node} = start_node(Name),
    ?line common_process_monitors(node(), Node),
    ?line TP1 = spawn(Node, ?MODULE, test_proc, []),
    ?line R1 = erlang:monitor(process, TP1),
    ?line TP1O = get_down_object(TP1, self()),
    ?line check_process_monitor(self(), TP1, R1),
    ?line tp_cast(TP1, fun () -> halt() end),
    ?line receive
	      {'DOWN',R1,process,TP1O,noconnection} ->
		  ?line ok
	  end,
    ?line check_process_demonitor(self(), TP1, R1),
    ?line R2 = erlang:monitor(process, TP1),
    ?line receive
	      {'DOWN',R2,process,TP1O,noconnection} ->
		  ?line ok
	  end,
    ?line check_process_demonitor(self(), TP1, R2),
    ?line ok.


common_process_monitors(NodeA, NodeB) ->
    ?line TP1 = spawn(NodeA, ?MODULE, test_proc, []),
    ?line TP2 = spawn(NodeB, ?MODULE, test_proc, []),
    ?line run_common_process_monitors(TP1, TP2),
    ?line TP3 = spawn(NodeA, ?MODULE, test_proc, []),
    ?line TP4 = spawn(NodeB, ?MODULE, test_proc, []),
    ?line [TP4N] = get_names(1, common_process_monitors),
    ?line true = tp_call(TP4, fun () -> register(TP4N,self()) end),
    ?line run_common_process_monitors(TP3,
				      case node() == node(TP4) of
					  true -> TP4N;
					  false -> {TP4N, node(TP4)}
				      end),
    ?line ok.

run_common_process_monitors(TP1, TP2) ->
    ?line R1 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    ?line check_process_monitor(TP1, TP2, R1),

    ?line tp_call(TP2, fun () -> catch erlang:demonitor(R1) end),
    ?line check_process_monitor(TP1, TP2, R1),

    ?line true = tp_call(TP1, fun () -> erlang:demonitor(R1) end),
    ?line check_process_demonitor(TP1, TP2, R1),

    ?line R2 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    ?line TP2O = get_down_object(TP2, TP1),
    ?line check_process_monitor(TP1, TP2, R2),
    ?line tp_cast(TP2, fun () -> exit(bye) end),
    ?line wait_until(fun () -> ?line is_proc_dead(TP2) end),
    ?line ok = tp_call(TP1, fun () ->
				    ?line receive
					      {'DOWN',R2,process,TP2O,bye} ->
						  ?line ok
				    end
			    end),
    ?line check_process_demonitor(TP1, TP2, R2),
    
    ?line R3 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    ?line ok = tp_call(TP1, fun () ->
				    ?line receive
					      {'DOWN',R3,process,TP2O,noproc} ->
						  ?line ok
				    end
			    end),
    ?line check_process_demonitor(TP1, TP2, R3),

    ?line tp_cast(TP1, fun () -> exit(normal) end),
    ?line wait_until(fun () -> ?line is_proc_dead(TP1) end),
    ?line ok.
    

busy_dist_port_monitor(doc) -> ["Tests distributed monitor/2, demonitor/1, "
				"and 'DOWN' message over busy distribution "
				"port"];
busy_dist_port_monitor(suite) -> [];
busy_dist_port_monitor(Config) when is_list(Config) ->

    ?line Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
		       "true" -> start_busy_dist_port_tracer();
		       _ -> false
		   end,

    ?line [An] = get_names(1, busy_dist_port_monitor),
    ?line {ok, A} = start_node(An),
    ?line TP1 = spawn(A, ?MODULE, test_proc, []),
    %% Check monitor over busy port
    ?line M1 = suspend_on_busy_test(A,
				    "erlang:monitor(process, TP1)",
				    fun () -> erlang:monitor(process, TP1) end),
    ?line check_process_monitor(self(), TP1, M1),
    %% Check demonitor over busy port
    ?line suspend_on_busy_test(A,
			       "erlang:demonitor(M1)",
			       fun () -> erlang:demonitor(M1) end),
    ?line check_process_demonitor(self(), TP1, M1),
    %% Check down message over busy port
    ?line TP2 = spawn(?MODULE, test_proc, []),
    ?line M2 = tp_call(TP1, fun () -> erlang:monitor(process, TP2) end),
    ?line check_process_monitor(TP1, TP2, M2),
    ?line Ref = make_ref(),
    ?line Busy = make_busy(A, 1000),
    ?line receive after 100 -> ok end,
    ?line tp_cast(TP2, fun () -> exit(Ref) end),
    ?line receive after 100 -> ok end,
    ?line unmake_busy(Busy),
    ?line Ref = tp_call(TP1, fun () ->
				     receive
					 {'DOWN', M2, process, TP2, Ref} ->
					     Ref
				     end
			     end),
    ?line tp_cast(TP1, fun () -> exit(normal) end),
    ?line stop_node(A),
    ?line stop_busy_dist_port_tracer(Tracer),
    ?line ok.

busy_dist_port_link(doc) -> ["Tests distributed link/1, unlink/1, and 'EXIT'",
			     " message over busy distribution port"];
busy_dist_port_link(suite) -> [];
busy_dist_port_link(Config) when is_list(Config) ->
    ?line Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
		       "true" -> start_busy_dist_port_tracer();
		       _ -> false
		   end,

    ?line [An] = get_names(1, busy_dist_port_link),
    ?line {ok, A} = start_node(An),
    ?line TP1 = spawn(A, ?MODULE, test_proc, []),
    %% Check link over busy port
    ?line suspend_on_busy_test(A,
			       "link(TP1)",
			       fun () -> link(TP1) end),
    ?line check_link(self(), TP1),
    %% Check unlink over busy port
    ?line suspend_on_busy_test(A,
			       "unlink(TP1)",
			       fun () -> unlink(TP1) end),
    ?line check_unlink(self(), TP1),
    %% Check trap exit message over busy port
    ?line TP2 = spawn(?MODULE, test_proc, []),
    ?line ok = tp_call(TP1, fun () ->
				    process_flag(trap_exit, true),
				    link(TP2),
				    ok
			    end),
    ?line check_link(TP1, TP2),
    ?line Ref = make_ref(),
    ?line Busy = make_busy(A, 1000),
    ?line receive after 100 -> ok end,
    ?line tp_cast(TP2, fun () -> exit(Ref) end),
    ?line receive after 100 -> ok end,
    ?line unmake_busy(Busy),
    ?line Ref = tp_call(TP1, fun () ->
				     receive
					 {'EXIT', TP2, Ref} ->
					     Ref
				     end
			     end),
    ?line tp_cast(TP1, fun () -> exit(normal) end),
    ?line stop_node(A),
    ?line stop_busy_dist_port_tracer(Tracer),
    ?line ok.


otp_5772_link(doc) -> [];
otp_5772_link(suite) -> [];
otp_5772_link(Config) when is_list(Config) ->
    ?line otp_5772_link_test(node()).

otp_5772_dist_link(doc) -> [];
otp_5772_dist_link(suite) -> [];
otp_5772_dist_link(Config) when is_list(Config) ->
    ?line [An] = get_names(1, otp_5772_dist_link),
    ?line {ok, A} = start_node(An),
    ?line otp_5772_link_test(A),
    ?line stop_node(A).

otp_5772_link_test(Node) ->
    ?line Prio = process_flag(priority, high),
    ?line TE = process_flag(trap_exit, true),
    ?line TP1 = spawn_opt(Node, ?MODULE, test_proc, [],
			  [link, {priority, low}]),
    exit(TP1, bang),
    unlink(TP1),
    ?line receive
	      {'EXIT', TP1, _} ->
		  ?line ok
	  after 0 ->
		  ?line ok
	  end,
    ?line receive
	      {'EXIT', TP1, _} = Exit ->
		  ?line ?t:fail({got_late_exit_message, Exit})
	  after 1000 ->
		  ?line ok
	  end,
    ?line process_flag(trap_exit, TE),
    ?line process_flag(priority, Prio),
    ?line ok.

otp_5772_monitor(doc) -> [];
otp_5772_monitor(suite) -> [];
otp_5772_monitor(Config) when is_list(Config) ->
    ?line otp_5772_monitor_test(node()).

otp_5772_dist_monitor(doc) -> [];
otp_5772_dist_monitor(suite) -> [];
otp_5772_dist_monitor(Config) when is_list(Config) ->
    ?line [An] = get_names(1, otp_5772_dist_monitor),
    ?line {ok, A} = start_node(An),
    ?line otp_5772_monitor_test(A),
    ?line stop_node(A),
    ?line ok.

otp_5772_monitor_test(Node) ->
    ?line Prio = process_flag(priority, high),
    ?line TP1 = spawn_opt(Node, ?MODULE, test_proc, [], [{priority, low}]),
    ?line M1 = erlang:monitor(process, TP1),
    ?line exit(TP1, bang),
    ?line erlang:demonitor(M1),
    ?line receive
	      {'DOWN', M1, _, _, _} ->
		  ?line ok
	  after 0 ->
		  ?line ok
	  end,
    ?line receive
	      {'DOWN', M1, _, _, _} = Down ->
		  ?line ?t:fail({got_late_down_message, Down})
	  after 1000 ->
		  ?line ok
	  end,
    ?line process_flag(priority, Prio),
    ?line ok.

otp_7946(Config) when is_list(Config) ->
    ?line [NodeName] = get_names(1, otp_7946),
    ?line {ok, Node} = start_node(NodeName),
    ?line Proc = rpc:call(Node, erlang, whereis, [net_kernel]),
    ?line Mon = erlang:monitor(process, Proc),
    ?line rpc:cast(Node, erlang, halt, []),
    ?line receive {'DOWN', Mon, process, Proc , _} -> ok end,
    ?line {Linker, LMon} = spawn_monitor(fun () ->
						 link(Proc),
						 receive
						 after infinity -> ok
						 end
					 end),
    ?line receive
	      {'DOWN', LMon, process, Linker, Reason} ->
		  ?line ?t:format("Reason=~p~n", [Reason]),
		  ?line Reason = noconnection
	  end.

%%
%% -- Internal utils --------------------------------------------------------
%%

-define(BUSY_DATA_KEY, '__busy__port__data__').
-define(BUSY_DATA_SIZE, 1024*1024).

busy_data() ->
    case get(?BUSY_DATA_KEY) of
	undefined ->
	    set_busy_data([]);
	Data ->
	    true = is_binary(Data),
	    true = size(Data) == ?BUSY_DATA_SIZE,
	    Data
    end.

set_busy_data(SetData) ->
    case get(?BUSY_DATA_KEY) of
	undefined ->
	    Data = case SetData of
		       D when is_binary(D), size(D) == ?BUSY_DATA_SIZE ->
			   SetData;
		       _ ->
			   list_to_binary(lists:duplicate(?BUSY_DATA_SIZE, 253))
		   end,
	    put(?BUSY_DATA_KEY, Data),
	    Data;
	OldData ->
	    OldData
    end.

freeze_node(Node, MS) ->
    Own = 500,
    DoingIt = make_ref(),
    Freezer = self(),
    spawn_link(Node,
	       fun () ->
		       erts_debug:set_internal_state(available_internal_state,
						     true),
		       dport_send(Freezer, DoingIt),
		       receive after Own -> ok end,
		       erts_debug:set_internal_state(block, MS+Own)
	       end),
    receive DoingIt -> ok end,
    receive after Own -> ok end.

make_busy(Node, Time) when is_integer(Time) ->
    Own = 500,
    freeze_node(Node, Time+Own), 
    Data = busy_data(),
    %% first make port busy
    Pid = spawn_link(fun () ->
			     forever(fun () ->
					     dport_reg_send(Node,
							    '__noone__',
							    Data)
				     end)
		     end),
    receive after Own -> ok end,
    wait_until(fun () ->
		       case process_info(Pid, status) of
			   {status, suspended} -> true;
			   _ -> false
		       end
	       end),
    %% then dist entry
    make_busy(Node, [nosuspend], Data),
    Pid.

make_busy(Node, Opts, Data) ->
    case erlang:send({'__noone__', Node}, Data, Opts) of
	nosuspend -> nosuspend;
	_ -> make_busy(Node, Opts, Data)
    end.

unmake_busy(Pid) ->
    unlink(Pid),
    exit(Pid, bang).

suspend_on_busy_test(Node, Doing, Fun) ->
    Tester = self(),
    DoIt = make_ref(),
    Done = make_ref(),
    Data = busy_data(),
    spawn_link(fun () ->
		       set_busy_data(Data),
		       Busy = make_busy(Node, 1000),
		       Tester ! DoIt,
		       receive after 100 -> ok end,
		       Info = process_info(Tester, [status, current_function]),
		       unmake_busy(Busy),
		       ?t:format("~p doing ~s: ~p~n", [Tester, Doing, Info]),
		       Tester ! {Done, Info}
	       end),
    receive DoIt -> ok end,
    Res = Fun(),
    receive
	{Done, MyInfo} ->
	    %% Don't match arity; it is different in
	    %% debug and optimized emulator
	    [{status, suspended},
	     {current_function, {erlang, bif_return_trap, _}}] = MyInfo,
	    ok
    end,
    Res.

% get_node(Name) when is_atom(Name) ->
%     ?line node();
% get_node({Name, Node}) when is_atom(Name) ->
%     ?line Node;
% get_node(NC) when is_pid(NC); is_port(NC); is_reference(NC) ->
%     ?line node(NC).

get_down_object(Item, _) when is_pid(Item) ->
    Item;
get_down_object({Name, Node} = Item, _) when is_atom(Name); is_atom(Node) ->
    Item;
get_down_object(Item, Watcher) when is_atom(Item), is_pid(Watcher) ->
    {Item, node(Watcher)};
get_down_object(Item, {_,Node}) when is_atom(Item), is_atom(Node) ->
    {Item, Node};
get_down_object(Item, Watcher) when is_atom(Item), is_atom(Watcher) ->
    {Item, node()}.

is_proc_dead(P) ->
    case is_proc_alive(P) of
	true -> false;
	false -> true
    end.

is_proc_alive(Pid) when is_pid(Pid), node(Pid) == node() ->
    ?line is_process_alive(Pid);
is_proc_alive(Name) when is_atom(Name) ->
    ?line case catch whereis(Name) of
	      Pid when is_pid(Pid) ->
		  ?line is_proc_alive(Pid);
	      _ ->
		  ?line false
	  end;
is_proc_alive({Name, Node}) when is_atom(Name), Node == node() ->
    ?line is_proc_alive(Name);
is_proc_alive(Proc) ->
    ?line is_remote_proc_alive(Proc).

is_remote_proc_alive({Name, Node}) when is_atom(Name), is_atom(Node) ->
    ?line is_remote_proc_alive(Name, Node);
is_remote_proc_alive(Pid) when is_pid(Pid) ->
    ?line is_remote_proc_alive(Pid, node(Pid));
is_remote_proc_alive(_) ->
    ?line false.

is_remote_proc_alive(PN, Node) ->		 
    ?line S = self(),
    ?line R = make_ref(),
    ?line monitor_node(Node, true),
    ?line _P = spawn(Node, fun () -> S ! {R, is_proc_alive(PN)} end),
    ?line receive
	      {R, Bool} ->
		  ?line monitor_node(Node, false),
		  ?line Bool;
	      {nodedown, Node} ->
		  ?line false
	  end.

wait_until(Fun) ->
    ?line case Fun() of
	      true ->
		  ?line ok;
	      _ ->
		  ?line receive
			after 100 ->
				?line wait_until(Fun)
			end
	  end.

forever(Fun) ->
    Fun(),
    forever(Fun).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    ?line Dog = ?t:timetrap(?t:minutes(1)),
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> ok;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    ?line [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    ?line Dog = ?config(watchdog, Config),
    ?line ?t:timetrap_cancel(Dog).

tp_call(Tp, Fun) ->
    ?line R = make_ref(),
    ?line Tp ! {call, self(), R, Fun},
    ?line receive
	      {R, Res} ->
		  ?line Res
	  end.

tp_cast(Tp, Fun) ->
    ?line Tp ! {cast, Fun}.

test_proc() ->
    ?line receive
	      {call, From, Ref, Fun} ->
		  ?line From ! {Ref, Fun()};
	      {cast, Fun} ->
		  ?line Fun()
	  end,
    ?line test_proc().

expand_link_list([#erl_link{type = ?LINK_NODE, targets = N} = Rec | T]) ->
    lists:duplicate(N,Rec#erl_link{targets = []}) ++ expand_link_list(T);
expand_link_list([#erl_link{targets = [#erl_link{pid = Pid}]} = Rec | T]) ->
    [Rec#erl_link{targets = [Pid]} | expand_link_list(T)];
expand_link_list([#erl_link{targets = [#erl_link{pid = Pid}|TT]} = Rec | T]) ->
    [ Rec#erl_link{targets = [Pid]} | expand_link_list( 
				       [Rec#erl_link{targets = TT} | T])]; 
expand_link_list([#erl_link{targets = []} = Rec | T]) ->
    [Rec | expand_link_list(T)];
expand_link_list([]) ->
    [].

get_local_link_list(Obj) ->
    case catch erts_debug:get_internal_state({link_list, Obj}) of
	LL when is_list(LL) ->
	    expand_link_list(LL);
	_ ->
	    []
    end.

get_remote_link_list(Node, Obj) ->
    case catch rpc:call(Node, erts_debug, get_internal_state,
			[{link_list, Obj}]) of
	LL when is_list(LL) ->
	    expand_link_list(LL);
	_ ->
	    []
    end.


get_link_list({Node, DistEntry}) when Node == node(), is_atom(DistEntry) ->
    get_local_link_list(DistEntry);
get_link_list({Node, DistEntry}) when is_atom(Node), is_atom(DistEntry) ->
    get_remote_link_list(Node, DistEntry);
get_link_list(P) when is_pid(P); is_port(P) ->
    case node(P) of
	      Node when Node == node() ->
		  get_local_link_list(P);
	      Node ->
		  get_remote_link_list(Node, P)
	  end;
get_link_list(undefined) ->
    [].

get_local_monitor_list(Obj) ->
    case catch erts_debug:get_internal_state({monitor_list, Obj}) of
	      LL when is_list(LL) ->
		  LL;
	      _ ->
		  []
	  end.

get_remote_monitor_list(Node, Obj) ->
    case catch rpc:call(Node, erts_debug, get_internal_state,
			[{monitor_list, Obj}]) of
	      LL when is_list(LL) ->
		  LL;
	      _ ->
		  []
	  end.


get_monitor_list({Node, DistEntry}) when Node == node(), is_atom(DistEntry) ->
    get_local_monitor_list(DistEntry);
get_monitor_list({Node, DistEntry}) when is_atom(Node), is_atom(DistEntry) ->
    get_remote_monitor_list(Node, DistEntry);
get_monitor_list(P) when is_pid(P) ->
    case node(P) of
	      Node when Node == node() ->
		  get_local_monitor_list(P);
	      Node ->
		  get_remote_monitor_list(Node, P)
	  end;
get_monitor_list(undefined) ->
    [].


find_erl_monitor(Pid, Ref) when is_reference(Ref) ->
    lists:foldl(fun (#erl_monitor{ref = R} = EL, Acc) when R == Ref ->
			      [EL|Acc];
			  (_, Acc) ->
			      Acc
		      end,
		      [],
		      get_monitor_list(Pid)).

% find_erl_link(Obj, Ref) when is_reference(Ref) -> 
%     ?line lists:foldl(fun (#erl_link{ref = R} = EL, Acc) when R == Ref ->
% 			      ?line [EL|Acc];
% 			  (_, Acc) ->
% 			      ?line Acc
% 		      end,
% 		      [],
% 		      get_link_list(Obj)).

find_erl_link(Obj, Type, [Item, Data]) when is_pid(Item);
					    is_port(Item);
					    is_atom(Item) -> 
    lists:foldl(fun (#erl_link{type = T, pid = I, targets = D} = EL,
			   Acc) when T == Type, I == Item ->
			      case Data of
					D ->
					    [EL|Acc];
					[] ->
					    [EL|Acc];
					_ ->
					    Acc
				    end;
			  (_, Acc) ->
			      Acc
		      end,
		      [],
		      get_link_list(Obj));
find_erl_link(Obj, Type, Item) when is_pid(Item); is_port(Item); is_atom(Item) ->
    find_erl_link(Obj, Type, [Item, []]). 

	

check_link(A, B) ->
    ?line [#erl_link{type = ?LINK_PID,
		      pid = B,
		      targets = []}] = find_erl_link(A, ?LINK_PID, B),
    ?line [#erl_link{type = ?LINK_PID,
		      pid = A,
		      targets = []}] = find_erl_link(B, ?LINK_PID, A),
    ?line case node(A) == node(B) of
	      false ->
		  ?line [#erl_link{type = ?LINK_PID,
				   pid = A,
				   targets = [B]}] = find_erl_link({node(A),
							       node(B)},
							      ?LINK_PID,
							      [A, [B]]),
		  ?line [#erl_link{type = ?LINK_PID,
				   pid = B,
				   targets = [A]}] = find_erl_link({node(B),
								    node(A)},
								   ?LINK_PID,
								   [B, [A]]);
	      true ->
		  ?line [] = find_erl_link({node(A), node(B)},
					   ?LINK_PID,
					   [A, [B]]),
		  ?line [] = find_erl_link({node(B), node(A)},
					   ?LINK_PID,
					   [B, [A]])
	  end,
    ?line ok.

check_unlink(A, B) ->
    ?line [] = find_erl_link(A, ?LINK_PID, B),
    ?line [] = find_erl_link(B, ?LINK_PID, A),
    ?line [] = find_erl_link({node(A), node(B)}, ?LINK_PID, [A, [B]]),
    ?line [] = find_erl_link({node(B), node(A)}, ?LINK_PID, [B, [A]]),
    ?line ok.

check_process_monitor(From, {Name, Node}, Ref) when is_pid(From),
						    is_atom(Name),
						    Node == node(From),
						    is_reference(Ref) ->
    ?line check_process_monitor(From, Name, Ref);
check_process_monitor(From, {Name, Node}, Ref) when is_pid(From),
						    is_atom(Name),
						    is_atom(Node),
						    is_reference(Ref) ->
    ?line MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    ?line [#erl_monitor{type = ?MON_ORIGIN,
			ref = Ref,
			pid = Node,
			name = Name}] = find_erl_monitor(From, Ref),
    ?line [#erl_monitor{type = ?MON_TARGET,
			ref = Ref,
			pid = From,
			name = Name}] = 	find_erl_monitor({node(From), Node}, Ref),
    ?line [#erl_monitor{type = ?MON_ORIGIN,
			ref = Ref,
			pid = MonitoredPid,
			name = Name}] = find_erl_monitor({Node, node(From)}, Ref),
    ?line [#erl_monitor{type = ?MON_TARGET,
			ref = Ref,
			pid = From,
			name = Name}] = find_erl_monitor(MonitoredPid, Ref),
    ?line ok;
check_process_monitor(From, Name, Ref) when is_pid(From),
					    is_atom(Name),
					    undefined /= Name,
					    is_reference(Ref) ->
    ?line MonitoredPid = rpc:call(node(From), erlang, whereis, [Name]),
    
    ?line [#erl_monitor{type = ?MON_ORIGIN,
			ref = Ref,
			pid = MonitoredPid,
			name = Name}] = find_erl_monitor(From, Ref),


    ?line [#erl_monitor{type = ?MON_TARGET,
			ref = Ref,
			pid = From,
			name = Name}] = find_erl_monitor(MonitoredPid,Ref),
    ok;
check_process_monitor(From, To, Ref) when is_pid(From),
					  is_pid(To),
					  is_reference(Ref) ->
    ?line OriMon = [#erl_monitor{type = ?MON_ORIGIN,
				 ref = Ref,
				 pid = To}],

    ?line OriMon = find_erl_monitor(From, Ref), 

    ?line TargMon = [#erl_monitor{type = ?MON_TARGET,
				 ref = Ref,
				 pid = From}],
    ?line TargMon = find_erl_monitor(To, Ref),
			

    ?line case node(From) == node(To) of
	      false ->
		  ?line TargMon = find_erl_monitor({node(From), node(To)}, Ref),
		  ?line OriMon = find_erl_monitor({node(To), node(From)}, Ref);
	      true ->
		  ?line [] = find_erl_monitor({node(From), node(From)}, Ref)
	  end,
    ?line ok.


check_process_demonitor(From, {undefined, Node}, Ref) when is_pid(From),
							   is_reference(Ref) ->
    ?line [] = find_erl_monitor(From, Ref),
    ?line case node(From) == Node of
	      false ->
		  ?line [] = find_erl_monitor({node(From), Node}, Ref),
		  ?line [] = find_erl_monitor({Node, node(From)}, Ref);
	      true ->
		  ?line [] = find_erl_monitor({Node, Node}, Ref)
	  end,
    ?line ok;
check_process_demonitor(From, {Name, Node}, Ref) when is_pid(From),
						      is_atom(Name),
						      Node == node(From),
						      is_reference(Ref) ->
    ?line MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    ?line case rpc:call(Node, erlang, whereis, [Name]) of
	      undefined ->
		  ?line check_process_demonitor(From, {undefined, Node}, Ref);
	      MonitoredPid ->
		  ?line check_process_demonitor(From, MonitoredPid, Ref)
	  end;
check_process_demonitor(From, {Name, Node}, Ref) when is_pid(From),
						      is_atom(Name),
						      is_atom(Node),
						      is_reference(Ref) ->
    ?line MonitoredPid = rpc:call(Node, erlang, whereis, [Name]),
    ?line [] = find_erl_monitor(From, Ref),
    ?line [] = find_erl_monitor({node(From), Node}, Ref),
    ?line [] = find_erl_monitor({Node, node(From)}, Ref),
    ?line [] = find_erl_monitor(MonitoredPid, Ref),
    ?line ok;
check_process_demonitor(From, undefined, Ref) when is_pid(From),
						   is_reference(Ref) ->
    ?line [] = find_erl_monitor(From, Ref),
    ?line case node(From) == node() of
	      false ->
		  ?line [] = find_erl_monitor({node(From), node()}, Ref),
		  ?line [] = find_erl_monitor({node(), node(From)}, Ref);
	      true ->
		  ?line [] = find_erl_monitor({node(), node()}, Ref)
	  end,
    ?line ok;
check_process_demonitor(From, Name, Ref) when is_pid(From),
					      is_atom(Name),
					      undefined /= Name,
					      is_reference(Ref) ->
    ?line check_process_demonitor(From, {Name, node()}, Ref);
check_process_demonitor(From, To, Ref) when is_pid(From),
					    is_pid(To),
					    is_reference(Ref) ->
    ?line [] = find_erl_monitor(From, Ref),
    ?line [] = find_erl_monitor(To, Ref),
    ?line case node(From) == node(To) of
	      false ->
		  ?line [] = find_erl_monitor({node(From), node(To)}, Ref),
		  ?line [] = find_erl_monitor({node(To), node(From)}, Ref);
	      true ->
		  ?line [] = find_erl_monitor({node(From), node(From)}, Ref)
	  end,
    ?line ok.

no_of_monitor_node(From, Node) when is_pid(From), is_atom(Node) ->
    ?line length(find_erl_link(From, ?LINK_NODE, Node)).

check_monitor_node(From, Node, No) when is_pid(From),
					is_atom(Node),
					is_integer(No),
					No >= 0 ->
    ?line LL = lists:duplicate(No, #erl_link{type = ?LINK_NODE, pid = Node}),
    ?line DLL = lists:duplicate(No, #erl_link{type = ?LINK_NODE, pid = From}),
    ?line LL = find_erl_link(From, ?LINK_NODE, Node),
    ?line DLL = find_erl_link({node(From), Node}, ?LINK_NODE, From),
    ?line ok.



hostname() ->
    ?line from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_H, []) -> [].

get_names(N, T) when is_atom(T) ->
    get_names(N, T, []).
get_names(0, _, Acc) ->
    Acc;
get_names(N, T, Acc) ->
    get_names(N-1, T, [list_to_atom(atom_to_list(?MODULE)
				    ++ "-"
				    ++ atom_to_list(T)
				    ++ "-"
				    ++ integer_to_list(erlang:system_time(seconds))
				    ++ "-"
				    ++ integer_to_list(erlang:unique_integer([positive]))) | Acc]).

start_node(Name) ->
    ?line start_node(Name, "").

start_node(Name, Args) ->
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line Res = ?t:start_node(Name, slave, [{args,  Args ++ " -pa " ++ Pa}]),
    ?line {ok, Node} = Res,
    ?line rpc:call(Node, erts_debug, set_internal_state,
		   [available_internal_state, true]),
    ?line Res.
    

stop_node(Node) ->
    ?line ?t:stop_node(Node).

-define(COOKIE, '').
-define(DOP_LINK,		1).
-define(DOP_SEND,		2).
-define(DOP_EXIT,		3).
-define(DOP_UNLINK,		4).
-define(DOP_REG_SEND,		6).
-define(DOP_GROUP_LEADER,	7).
-define(DOP_EXIT2,		8).

-define(DOP_SEND_TT,		12).
-define(DOP_EXIT_TT,		13).
-define(DOP_REG_SEND_TT,	16).
-define(DOP_EXIT2_TT,		18).

-define(DOP_MONITOR_P,		19).
-define(DOP_DEMONITOR_P,	20).
-define(DOP_MONITOR_P_EXIT,	21).

dport_send(To, Msg) ->
    Node = node(To),
    DPrt = case dport(Node) of
	       undefined ->
		   pong = net_adm:ping(Node),
		   dport(Node);
	       Prt ->
		   Prt
	   end,
    port_command(DPrt, [dmsg_hdr(),
			dmsg_ext({?DOP_SEND,
				  ?COOKIE,
				  To}),
			dmsg_ext(Msg)]).

dport_reg_send(Node, Name, Msg) ->
    DPrt = case dport(Node) of
	       undefined ->
		   pong = net_adm:ping(Node),
		   dport(Node);
	       Prt ->
		   Prt
	   end,
    port_command(DPrt, [dmsg_hdr(),
			dmsg_ext({?DOP_REG_SEND,
				  self(),
				  ?COOKIE,
				  Name}),
			dmsg_ext(Msg)]).

dport(Node) when is_atom(Node) ->
    case catch erts_debug:get_internal_state(available_internal_state) of
	true -> true;
	_ -> erts_debug:set_internal_state(available_internal_state, true)
    end,
    erts_debug:get_internal_state({dist_port, Node}).

dmsg_hdr() ->
    [131, % Version Magic
     $D,  % Dist header
     0].  % No atom cache referenses

dmsg_ext(Term) ->	
    <<131, Res/binary>> = term_to_binary(Term),
    Res.

start_busy_dist_port_tracer() ->
    Tracer = spawn_link(fun () -> busy_dist_port_tracer() end),
    erlang:system_monitor(Tracer, [busy_dist_port]),
    Tracer.

stop_busy_dist_port_tracer(Tracer) when is_pid(Tracer) ->
    unlink(Tracer),
    exit(Tracer, bye);
stop_busy_dist_port_tracer(_) ->
    true.

busy_dist_port_tracer() ->
    receive
	{monitor, _SuspendedProcess, busy_dist_port, _Port} = M ->
	    erlang:display(M),
	    busy_dist_port_tracer()
    end.
	    
    


