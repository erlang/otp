%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

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


%% Test spawn/1.
spawn1(Config) when is_list(Config) ->
    Node = node(),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn
    P = spawn(fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(false, normal, FA, 0, PV)
    end,
    ok.

%% Test spawn/2.
spawn2(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn2),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn(Node,
	      fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(false, normal, FA, 0, PV)
    end,

    true = stop_node(Node),
    ok.


%% Test spawn/3.
spawn3(Config) when is_list(Config) ->
    Node = node(),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn(?MODULE,
	      run_fun,
	      [fun() ->
		       Parent ! {self(), fetch_proc_vals(self())}
	       end]),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(false, normal, FA, 0, PV)
    end,
    ok.

%% Test spawn/4.
spawn4(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn4),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn(Node,
	      ?MODULE,
	      run_fun,
	      [fun() ->
		       Parent ! {self(), fetch_proc_vals(self())}
	       end]),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(false, normal, FA, 0, PV)
    end,

    true = stop_node(Node),
    ok.



%% Test spawn_link/1.
spawn_link1(Config) when is_list(Config) ->
    Node = node(),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn_link(fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(true, normal, FA, 0, PV)
    end,
    ok.

%% Test spawn_link/2.
spawn_link2(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn_link2),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn_link(Node,
		   fun() -> Parent ! {self(), fetch_proc_vals(self())} end),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(true, normal, FA, 0, PV)
    end,

    true = stop_node(Node),
    ok.

%% Test spawn_link/3.
spawn_link3(Config) when is_list(Config) ->
    Node = node(),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn_link(?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end]),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(true, normal, FA, 0, PV)
    end,
    ok.

%% Test spawn_link/4.
spawn_link4(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn_link4),

    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    %% spawn_link
    P = spawn_link(Node,
		   ?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end]),
    receive
	{P, PV} ->
	    Node = node(P),
	    check_proc_vals(true, normal, FA, 0, PV)
    end,

    true = stop_node(Node),
    ok.


%% Test spawn_opt/2.
spawn_opt2(Config) when is_list(Config) ->
    Node = node(),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),

    P1 = spawn_opt(fun() ->
			   Parent ! {self(), fetch_proc_vals(self())}
		   end,
		   [{fullsweep_after, 0},{min_heap_size, 1000},
		    link, {priority, max}]),
    receive
	{P1, PV1} ->
	    Node = node(P1),
	    check_proc_vals(true, max, 0, 1000, PV1)
    end,
    P2 = spawn_opt(fun() -> Parent ! {self(), fetch_proc_vals(self())} end,
		   [{min_heap_size, 10}]),
    receive
	{P2, PV2} ->
	    Node = node(P2),
	    check_proc_vals(false, normal, FA, 10, PV2)
    end,
    ok.

%% Test spawn_opt/3.
spawn_opt3(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn_opt3),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),
    P1 = spawn_opt(Node,
		   fun() ->
			   Parent ! {self(), fetch_proc_vals(self())}
		   end,
		   [{fullsweep_after,0}, {min_heap_size,1000},
		    link, {priority, max}]),
    receive
	{P1, PV1} ->
	    Node = node(P1),
	    check_proc_vals(true, max, 0, 1000, PV1)
    end,
    P2 = spawn_opt(Node,
		   fun() -> Parent ! {self(), fetch_proc_vals(self())} end,
		   [{min_heap_size, 10}]),
    receive
	{P2, PV2} ->
	    Node = node(P2),
	    check_proc_vals(false, normal, FA, 10, PV2)
    end,
    true = stop_node(Node),
    ok.

%% Test spawn_opt/4.
spawn_opt4(Config) when is_list(Config) ->
    Node = node(),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),
    P1 = spawn_opt(?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end],
		   [{fullsweep_after,0}, {min_heap_size,1000},
		    link, {priority, max}]),
    receive
	{P1, PV1} ->
	    Node = node(P1),
	    check_proc_vals(true, max, 0, 1000, PV1)
    end,
    P2 = spawn_opt(?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end],
		   [{min_heap_size, 10}]),
    receive
	{P2, PV2} ->
	    Node = node(P2),
	    check_proc_vals(false, normal, FA, 10, PV2)
    end,
    ok.

%% Test spawn_opt/5.
spawn_opt5(Config) when is_list(Config) ->
    {ok, Node} = start_node(spawn_opt5),
    Parent = self(),
    {_, _, FA, _} = fetch_proc_vals(self()),
    P1 = spawn_opt(Node,
		   ?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end],
		   [{fullsweep_after,0}, {min_heap_size,1000},
		    link, {priority, max}]),
    receive
	{P1, PV1} ->
	    Node = node(P1),
	    check_proc_vals(true, max, 0, 1000, PV1)
    end,
    P2 = spawn_opt(Node,
		   ?MODULE,
		   run_fun,
		   [fun() ->
			    Parent ! {self(), fetch_proc_vals(self())}
		    end],
		   [{min_heap_size, 10}]),
    receive
	{P2, PV2} ->
	    Node = node(P2),
	    check_proc_vals(false, normal, FA, 10, PV2)
    end,
    true = stop_node(Node),
    ok.

%% Test failure behavior of spawn bifs.
spawn_failures(Config) when is_list(Config) ->
    ThisNode = node(),
    {ok, Node} = start_node(spawn_remote_failure),

    %% unknown nodes
    io:format("Testing unknown nodes~n", []),
    CrashPid1 = (catch spawn_opt('unknown@node',
				 erlang,
				 nodes,
				 [],
				 [])),
    true = is_pid(CrashPid1),
    ThisNode = node(CrashPid1),
    CrashPid2 = (catch spawn_opt('unknown@node',
				 fun () -> erlang:nodes() end,
				 [])),
    true = is_pid(CrashPid2),
    ThisNode = node(CrashPid2),

    CrashPid3 = (catch spawn('unknown@node',
			     erlang,
			     nodes,
			     [])),
    true = is_pid(CrashPid3),
    ThisNode = node(CrashPid3),
    CrashPid4 = (catch spawn('unknown@node',
			     fun () -> erlang:nodes() end)),
    true = is_pid(CrashPid4),
    ThisNode = node(CrashPid4),

    OTE = process_flag(trap_exit,true),
    CrashPid5 = (catch spawn_link('unknown@node',
				  erlang,
				  nodes,
				  [])),
    receive
	{'EXIT', CrashPid5, noconnection} ->
	    true = is_pid(CrashPid5),
	    ThisNode = node(CrashPid5)
    end,
    CrashPid6 = (catch spawn_link('unknown@node',
				  fun () -> erlang:nodes() end)),
    receive
	{'EXIT', CrashPid6, noconnection} ->
	    true = is_pid(CrashPid6),
	    ThisNode = node(CrashPid6)
    end,
    process_flag(trap_exit,OTE),
    case OTE of
	false ->
	    receive
		{'EXIT', P, R} ->
		    ct:fail({'EXIT', P, R})
	    after 0 ->
		    ok
	    end;
	_ ->
	    ok
    end,

    %% bad node
    io:format("Testing bad nodes~n", []),
    {'EXIT', {badarg, _}} = (catch spawn_opt("Node",erlang,nodes,[],[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt("Node",
					     fun () ->
						     erlang:nodes()
					     end,
					     [])),
    {'EXIT', {badarg, _}} = (catch spawn_link("Node",
					      fun () ->
						      erlang:nodes()
					      end)),
    {'EXIT', {badarg, _}} = (catch spawn("Node",erlang,nodes,[])),
    {'EXIT', {badarg, _}} = (catch spawn("Node",
					 fun () ->
						 erlang:nodes()
					 end)),

    %% bad module
    io:format("Testing bad modules~n", []),
    {'EXIT', {badarg, _}} = (catch spawn_opt(Node,"erlang",nodes,[],[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt("erlang",nodes,[],[])),
    {'EXIT', {badarg, _}} = (catch spawn_link(Node,"erlang",nodes,[])),
    {'EXIT', {badarg, _}} = (catch spawn_link("erlang",nodes,[])),
    {'EXIT', {badarg, _}} = (catch spawn(Node,"erlang",nodes,[])),
    {'EXIT', {badarg, _}} = (catch spawn("erlang",nodes,[])),

    %% bad function
    io:format("Testing bad functions~n", []),
    {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,"nodes",[],[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt(Node,not_a_fun,[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,"nodes",[],[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt(not_a_fun,[])),
    {'EXIT', {badarg, _}} = (catch spawn_link(Node,erlang,"nodes",[])),
    {'EXIT', {badarg, _}} = (catch spawn_link(Node,not_a_fun)),
    {'EXIT', {badarg, _}} = (catch spawn_link(erlang,"nodes",[])),
    {'EXIT', {badarg, _}} = (catch spawn_link(not_a_fun)),
    {'EXIT', {badarg, _}} = (catch spawn(Node,erlang,"nodes",[])),
    {'EXIT', {badarg, _}} = (catch spawn(Node,not_a_fun)),
    {'EXIT', {badarg, _}} = (catch spawn(erlang,"nodes",[])),
    {'EXIT', {badarg, _}} = (catch spawn(not_a_fun)),


    %% bad argument
    io:format("Testing bad arguments~n", []),
    {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,nodes,[a|b],[])),
    {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,nodes,[a|b],[])),
    {'EXIT', {badarg, _}} = (catch spawn_link(Node,erlang,nodes,[a|b])),
    {'EXIT', {badarg, _}} = (catch spawn_link(erlang,nodes,[a|b])),
    {'EXIT', {badarg, _}} = (catch spawn(Node,erlang,nodes,[a|b])),
    {'EXIT', {badarg, _}} = (catch spawn(erlang,nodes,[a|b])),

    %% bad option
    io:format("Testing bad options~n", []),
    {'EXIT', {badarg, _}} = (catch spawn_opt(Node,erlang,nodes,[],[a|b])),
    {'EXIT', {badarg, _}} = (catch spawn_opt(erlang,nodes,[],[a|b])),


    true = stop_node(Node),
    ok.

check_proc_vals(Link, Priority, FullsweepAfter, MinHeapSize, {Ls, P, FA, HS}) ->
    Link = lists:member(self(), Ls),
    Priority = P,
    FullsweepAfter = FA,
    true = (HS >= MinHeapSize),
    ok.

fetch_proc_vals(Pid) ->
    PI = process_info(Pid),
    {value,{links, Ls}} = lists:keysearch(links, 1, PI),
    {value,{priority,P}} = lists:keysearch(priority, 1, PI),
    {value,{garbage_collection,Gs}} =
	lists:keysearch(garbage_collection, 1, PI),
    {value,{fullsweep_after,FA}} =
	lists:keysearch(fullsweep_after, 1, Gs),
    {value,{heap_size,HS}} = lists:keysearch(heap_size, 1, PI),
    {Ls, P, FA, HS}.

%% Test erlang:packet_delim/3 with {line_delimiter,0} option.
decode_packet_delim(Config) when is_list(Config) ->
    {ok,<<"abc",0>>,<<"efg",0>>} =
        erlang:decode_packet(line, <<"abc",0,"efg",0>>, [{line_delimiter, 0}]),
    {more, undefined} = erlang:decode_packet(line, <<"abc",0,"efg",0>>, []).

%% This testcase should probably be moved somewhere else

%% Test that memory allocation command line options affecting the
%% wilderness of the heap are interpreted correct by the emulator.
wilderness(Config) when is_list(Config) ->
    OKParams = {512, 8},
    Alloc = erlang:system_info(allocator),
    io:format("Test server allocator info:~n~p", [Alloc]),
    Result = case Alloc of
		 {Allocator, _, _, _} when Allocator == glibc;
					   Allocator == dlmalloc ->
		     run_wilderness_test(OKParams, OKParams),
		     {comment,
		      "Allocator used: " ++ atom_to_list(Allocator)};
		 {OtherAllocator, _, _, _} ->
		     {skipped,
		      "Only run when glibc is used. "
		      "Allocator used: "
		      ++ atom_to_list(OtherAllocator)}
	     end,
    Result.

run_wilderness_test({Set_tt, Set_tp}, {Exp_tt, Exp_tp}) ->
    Self = self(),
    Ref = make_ref(),
    SuiteDir = filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(allocator_test,
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
	    io:format("Test allocator info:~n~p",
		      [{A, V, F, S}]),
	    {value, {sys_alloc, SA_Opts}}
		= lists:keysearch(sys_alloc, 1, S),
	    {value, {tt, Ett}} = lists:keysearch(tt, 1, SA_Opts),
	    {value, {tp, Etp}} = lists:keysearch(tp, 1, SA_Opts)
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
    [Name] = get_nodenames(1, TestCase),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa}]).

stop_node(Node) ->
    true = test_server:stop_node(Node).

run_fun(Fun) ->
    Fun().
