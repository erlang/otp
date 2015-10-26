%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2012. All Rights Reserved.
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
-module(bif_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([ 
	 spawn1/1, spawn2/1, spawn3/1, spawn4/1,

	
	 spawn_link1/1, spawn_link2/1, spawn_link3/1, spawn_link4/1,

	
	 spawn_opt2/1, spawn_opt3/1, spawn_opt4/1, spawn_opt5/1,

	 spawn_failures/1,

	 run_fun/1,
     decode_packet_delim/1,
	 wilderness/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, spawn_tests}, {group, spawn_link_tests},
     {group, spawn_opt_tests}, spawn_failures, wilderness].

groups() -> 
    [{spawn_tests, [], [spawn1, spawn2, spawn3, spawn4]},
     {spawn_link_tests, [],
      [spawn_link1, spawn_link2, spawn_link3, spawn_link4]},
     {spawn_opt_tests, [],
      [spawn_opt2, spawn_opt3, spawn_opt4, spawn_opt5]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


spawn1(doc) -> ["Test spawn/1"];
spawn1(suite) ->
    [];
spawn1(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn
    ?line P = spawn(fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(false, normal, FA, 0, PV)
	  end,
    ok.

spawn2(doc) -> ["Test spawn/2"];
spawn2(suite) ->
    [];
spawn2(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn2),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn(Node,
		    fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(false, normal, FA, 0, PV)
	  end,

    ?line true = stop_node(Node),
    ok.


spawn3(doc) -> ["Test spawn/3"];
spawn3(suite) ->
    [];
spawn3(Config) when is_list(Config) ->
    ?line Node = node(),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn(?MODULE,
		    run_fun,
		    [fun() ->
			     Parent ! {self(), fetch_proc_vals(self())}
		     end]),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(false, normal, FA, 0, PV)
	  end,
    ok.

spawn4(doc) -> ["Test spawn/4"];
spawn4(suite) ->
    [];
spawn4(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn4),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn(Node,
		    ?MODULE,
		    run_fun,
		    [fun() ->
			     Parent ! {self(), fetch_proc_vals(self())}
		     end]),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(false, normal, FA, 0, PV)
	  end,

    ?line true = stop_node(Node),
    ok.



spawn_link1(doc) -> ["Test spawn_link/1"];
spawn_link1(suite) ->
    [];
spawn_link1(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn_link(fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(true, normal, FA, 0, PV)
	  end,
    ok.

spawn_link2(doc) -> ["Test spawn_link/2"];
spawn_link2(suite) ->
    [];
spawn_link2(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn_link2),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn_link(Node,
			 fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(true, normal, FA, 0, PV)
	  end,

    ?line true = stop_node(Node),
    ok.

spawn_link3(doc) -> ["Test spawn_link/3"];
spawn_link3(suite) ->
    [];
spawn_link3(Config) when is_list(Config) ->
    ?line Node = node(),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn_link(?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end]),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(true, normal, FA, 0, PV)
	  end,
    ok.

spawn_link4(doc) -> ["Test spawn_link/4"];
spawn_link4(suite) ->
    [];
spawn_link4(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn_link4),

    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

    % spawn_link
    ?line P = spawn_link(Node,
			 ?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end]),
    ?line receive
	      {P, PV} ->
		  ?line Node = node(P),
		  ?line check_proc_vals(true, normal, FA, 0, PV)
	  end,

    ?line true = stop_node(Node),
    ok.


spawn_opt2(doc) -> ["Test spawn_opt/2"];
spawn_opt2(suite) ->
    [];
spawn_opt2(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),

     ?line P1 = spawn_opt(fun() ->
 				 Parent ! {self(), fetch_proc_vals(self())}
 			 end,
			  [{fullsweep_after, 0},{min_heap_size, 1000},
			   link, {priority, max}]),
     ?line receive
 	      {P1, PV1} ->
 		  ?line Node = node(P1),
 		  ?line check_proc_vals(true, max, 0, 1000, PV1)
 	  end,
    ?line P2 = spawn_opt(fun() -> Parent ! {self(), fetch_proc_vals(self())} end,
			 [{min_heap_size, 10}]),
    ?line receive
	      {P2, PV2} ->
		  ?line Node = node(P2),
		  ?line check_proc_vals(false, normal, FA, 10, PV2)
	  end,
    ok.

spawn_opt3(doc) -> ["Test spawn_opt/3"];
spawn_opt3(suite) ->
    [];
spawn_opt3(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn_opt3),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),
    ?line P1 = spawn_opt(Node,
			 fun() ->
				 Parent ! {self(), fetch_proc_vals(self())}
			 end,
			 [{fullsweep_after,0}, {min_heap_size,1000},
			  link, {priority, max}]),
    ?line receive
	      {P1, PV1} ->
		  ?line Node = node(P1),
		  ?line check_proc_vals(true, max, 0, 1000, PV1)
	  end,
    ?line P2 = spawn_opt(Node,
			fun() -> Parent ! {self(), fetch_proc_vals(self())} end,
			 [{min_heap_size, 10}]),
    ?line receive
	      {P2, PV2} ->
		  ?line Node = node(P2),
		  ?line check_proc_vals(false, normal, FA, 10, PV2)
	  end,
    ?line true = stop_node(Node),
    ok.

spawn_opt4(doc) -> ["Test spawn_opt/4"];
spawn_opt4(suite) ->
    [];
spawn_opt4(Config) when is_list(Config) ->
    ?line Node = node(),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),
    ?line P1 = spawn_opt(?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end],
			 [{fullsweep_after,0}, {min_heap_size,1000},
			  link, {priority, max}]),
    ?line receive
	      {P1, PV1} ->
		  ?line Node = node(P1),
		  ?line check_proc_vals(true, max, 0, 1000, PV1)
	  end,
    ?line P2 = spawn_opt(?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end],
			     [{min_heap_size, 10}]),
    ?line receive
	      {P2, PV2} ->
		  ?line Node = node(P2),
		  ?line check_proc_vals(false, normal, FA, 10, PV2)
	  end,
    ok.

spawn_opt5(doc) -> ["Test spawn_opt/5"];
spawn_opt5(suite) ->
    [];
spawn_opt5(Config) when is_list(Config) ->
    ?line {ok, Node} = start_node(spawn_opt5),
    ?line Parent = self(),
    ?line {_, _, FA, _} = fetch_proc_vals(self()),
    ?line P1 = spawn_opt(Node,
			 ?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end],
			 [{fullsweep_after,0}, {min_heap_size,1000},
			  link, {priority, max}]),
    ?line receive
	      {P1, PV1} ->
		  ?line Node = node(P1),
		  ?line check_proc_vals(true, max, 0, 1000, PV1)
	  end,
    ?line P2 = spawn_opt(Node,
			 ?MODULE,
			 run_fun,
			 [fun() ->
				  Parent ! {self(), fetch_proc_vals(self())}
			  end],
			  [{min_heap_size, 10}]),
    ?line receive
	      {P2, PV2} ->
		  ?line Node = node(P2),
		  ?line check_proc_vals(false, normal, FA, 10, PV2)
	  end,
    ?line true = stop_node(Node),
    ok.

spawn_failures(doc) ->
    ["Test failure behavior of spawn bifs"];
spawn_failures(suite) ->
    [];
spawn_failures(Config) when is_list(Config) ->
    ?line ThisNode = node(),
    ?line {ok, Node} = start_node(spawn_remote_failure),

    % unknown nodes
    test_server:format("Testing unknown nodes~n", []),
    ?line CrashPid1 = (catch spawn_opt('unknown@node',
				       erlang,
				       nodes,
				       [],
				       [])),
    ?line true = is_pid(CrashPid1),
    ?line ThisNode = node(CrashPid1),
    ?line CrashPid2 = (catch spawn_opt('unknown@node',
				       fun () -> erlang:nodes() end,
				       [])),
    ?line true = is_pid(CrashPid2),
    ?line ThisNode = node(CrashPid2),

    ?line CrashPid3 = (catch spawn('unknown@node',
				   erlang,
				   nodes,
				   [])),
    ?line true = is_pid(CrashPid3),
    ?line ThisNode = node(CrashPid3),
    ?line CrashPid4 = (catch spawn('unknown@node',
				   fun () -> erlang:nodes() end)),
    ?line true = is_pid(CrashPid4),
    ?line ThisNode = node(CrashPid4),

    ?line OTE = process_flag(trap_exit,true),
    ?line CrashPid5 = (catch spawn_link('unknown@node',
					erlang,
					nodes,
					[])),
    receive
	{'EXIT', CrashPid5, noconnection} ->
	    ?line true = is_pid(CrashPid5),
	    ?line ThisNode = node(CrashPid5)
    end,
    ?line CrashPid6 = (catch spawn_link('unknown@node',
					fun () -> erlang:nodes() end)),
    receive
	{'EXIT', CrashPid6, noconnection} ->
	    ?line true = is_pid(CrashPid6),
	    ?line ThisNode = node(CrashPid6)
    end,
    process_flag(trap_exit,OTE),
    case OTE of
	false ->
	    receive
		{'EXIT', P, R} ->
		    ?line test_server:fail({'EXIT', P, R})
	    after 0 ->
		    ok
	    end;
	_ ->
	    ok
    end,

    % bad node
    test_server:format("Testing bad nodes~n", []),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt("Node",erlang,nodes,[],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt("Node",
						   fun () ->
							   erlang:nodes()
						   end,
						   [])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link("Node",
						    fun () ->
							    erlang:nodes()
						    end)),
    ?line {'EXIT', {badarg, _}} = (catch spawn("Node",erlang,nodes,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn("Node",
					       fun () ->
						       erlang:nodes()
					       end)),

    % bad module
    test_server:format("Testing bad modules~n", []),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(Node,"erlang",nodes,[],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt("erlang",nodes,[],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(Node,"erlang",nodes,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link("erlang",nodes,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn(Node,"erlang",nodes,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn("erlang",nodes,[])),

    % bad function
    test_server:format("Testing bad functions~n", []),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,"nodes",[],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(Node,not_a_fun,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,"nodes",[],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(not_a_fun,[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(Node,erlang,"nodes",[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(Node,not_a_fun)),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(erlang,"nodes",[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(not_a_fun)),
    ?line {'EXIT', {badarg, _}} = (catch spawn(Node,erlang,"nodes",[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn(Node,not_a_fun)),
    ?line {'EXIT', {badarg, _}} = (catch spawn(erlang,"nodes",[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn(not_a_fun)),


    % bad argument
    test_server:format("Testing bad arguments~n", []),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,nodes,[a|b],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,nodes,[a|b],[])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(Node,erlang,nodes,[a|b])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_link(erlang,nodes,[a|b])),
    ?line {'EXIT', {badarg, _}} = (catch spawn(Node,erlang,nodes,[a|b])),
    ?line {'EXIT', {badarg, _}} = (catch spawn(erlang,nodes,[a|b])),

    % bad option
    test_server:format("Testing bad options~n", []),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,nodes,[],[a|b])),
    ?line {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,nodes,[],[a|b])),


    ?line true = stop_node(Node),
    ok.

check_proc_vals(Link, Priority, FullsweepAfter, MinHeapSize, {Ls, P, FA, HS}) ->
    ?line Link = lists:member(self(), Ls),
    ?line Priority = P,
    FullsweepAfter = FA,
    true = (HS >= MinHeapSize),
    ?line ok.

fetch_proc_vals(Pid) ->
    ?line PI = process_info(Pid),
    ?line {value,{links, Ls}} = lists:keysearch(links, 1, PI),
    ?line {value,{priority,P}} = lists:keysearch(priority, 1, PI),
    {value,{garbage_collection,Gs}} =
	lists:keysearch(garbage_collection, 1, PI),
    {value,{fullsweep_after,FA}} =
	lists:keysearch(fullsweep_after, 1, Gs),
    {value,{heap_size,HS}} = lists:keysearch(heap_size, 1, PI),
    ?line {Ls, P, FA, HS}.
     
decode_packet_delim(doc) ->
    ["Test erlang:packet_delim/3 with {line_delimiter,0} option"];
decode_packet_delim(suite) ->
    [];
decode_packet_delim(Config) when is_list(Config) ->
    {ok,<<"abc",0>>,<<"efg",0>>} =
        erlang:decode_packet(line, <<"abc",0,"efg",0>>, [{line_delimiter, 0}]),
    {more, undefined} = erlang:decode_packet(line, <<"abc",0,"efg",0>>, []).

% This testcase should probably be moved somewhere else
wilderness(doc) ->
    ["Test that memory allocation command line options affecting the"
     "wilderness of the heap are interpreted correct by the emulator "];
wilderness(suite) ->
    [];
wilderness(Config) when is_list(Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    ?line OKParams = {512, 8},
    ?line Alloc = erlang:system_info(allocator),
    ?line test_server:format("Test server allocator info:~n~p", [Alloc]),
    Result = case Alloc of
		 {Allocator, _, _, _} when Allocator == glibc;
					   Allocator == dlmalloc ->
		     ?line run_wilderness_test(OKParams, OKParams),
		     ?line {comment,
			    "Allocator used: " ++ atom_to_list(Allocator)};
		 {OtherAllocator, _, _, _} ->
		     ?line {skipped,
			    "Only run when glibc is used. "
			    "Allocator used: "
			    ++ atom_to_list(OtherAllocator)}
	     end,
    ?line test_server:timetrap_cancel(Dog),
    Result.
    
run_wilderness_test({Set_tt, Set_tp}, {Exp_tt, Exp_tp}) ->
    Self = self(),
    Ref = make_ref(),
    SuiteDir = filename:dirname(code:which(?MODULE)),
    ?line {ok, Node} = test_server:start_node(allocator_test,
					      slave,
					      [{args,
						" -pa "
						++ SuiteDir
						++" +MYtt "++to_string(Set_tt)
						++" +MYtp "++to_string(Set_tp)},
					       {linked, false}]),
    spawn(Node, fun () ->
			Self ! {Ref, erlang:system_info(allocator)}
		end),
    receive
	{Ref, {A, V, F, S}} ->
	    Ett = Exp_tt*1024,
	    Etp = Exp_tp*1024,
	    ?line test_server:format("Test allocator info:~n~p",
				     [{A, V, F, S}]),
	    ?line {value, {sys_alloc, SA_Opts}}
		= lists:keysearch(sys_alloc, 1, S),
	    ?line {value, {tt, Ett}} = lists:keysearch(tt, 1, SA_Opts),
	    ?line {value, {tp, Etp}} = lists:keysearch(tp, 1, SA_Opts)
    end,
    stop_node(Node).
	     
to_string(X) when is_integer(X) ->
    integer_to_list(X);
to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_list(X) ->
    X.

get_nodenames(N, T) ->
    get_nodenames(N, T, []).

get_nodenames(0, _, Acc) ->
    Acc;
get_nodenames(N, T, Acc) ->
    {A, B, C} = now(),
    get_nodenames(N-1, T, [list_to_atom(atom_to_list(?MODULE)
					++ "-"
					++ atom_to_list(T)
					++ "-"
					++ integer_to_list(A)
					++ "-"
					++ integer_to_list(B)
					++ "-"
					++ integer_to_list(C)) | Acc]).

start_node(TestCase) ->
    ?line [Name] = get_nodenames(1, TestCase),
    ?line Pa = filename:dirname(code:which(?MODULE)),
    ?line test_server:start_node(Name, slave, [{args, "-pa " ++ Pa}]).

stop_node(Node) ->
    ?line true = test_server:stop_node(Node).

run_fun(Fun) ->
    Fun().
