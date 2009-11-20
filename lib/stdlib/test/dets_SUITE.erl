%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(dets_SUITE).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t, test_server).
-define(privdir(_), "./dets_SUITE_priv").
-define(datadir(_), "./dets_SUITE_data").
-else.
-include("test_server.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), ?config(priv_dir, Conf)).
-define(datadir(Conf), ?config(data_dir, Conf)).
-endif.

-export([all/1, not_run/1, newly_started/1, basic_v8/1, basic_v9/1,
	 open_v8/1, open_v9/1, sets_v8/1, sets_v9/1, bags_v8/1,
	 bags_v9/1, duplicate_bags_v8/1, duplicate_bags_v9/1,
	 access_v8/1, access_v9/1, dirty_mark/1, dirty_mark2/1,
	 bag_next_v8/1, bag_next_v9/1, oldbugs_v8/1, oldbugs_v9/1,
	 unsafe_assumptions/1, truncated_segment_array_v8/1,
	 truncated_segment_array_v9/1, open_file_v8/1, open_file_v9/1,
	 init_table_v8/1, init_table_v9/1, repair_v8/1, repair_v9/1,
	 hash_v8b_v8c/1, phash/1, fold_v8/1, fold_v9/1, fixtable_v8/1,
	 fixtable_v9/1, match_v8/1, match_v9/1, select_v8/1,
	 select_v9/1, update_counter/1, badarg/1, cache_sets_v8/1,
	 cache_sets_v9/1, cache_bags_v8/1, cache_bags_v9/1,
	 cache_duplicate_bags_v8/1, cache_duplicate_bags_v9/1,
	 otp_4208/1, otp_4989/1, many_clients/1, otp_4906/1, otp_5402/1,
         simultaneous_open/1, insert_new/1, repair_continuation/1,
         otp_5487/1, otp_6206/1, otp_6359/1, otp_4738/1, otp_7146/1,
         otp_8070/1]).

-export([dets_dirty_loop/0]).

-export([histogram/1, sum_histogram/1, ave_histogram/1]).

-export([init_per_testcase/2, fin_per_testcase/2]).

%% Internal export.
-export([client/2]).

-import(lists, 
	[append/1, delete/2, duplicate/2, filter/2, foreach/2, keysearch/3, 
	 last/1, map/2, member/2, reverse/1, seq/2, sort/1, usort/1]).

-include_lib("kernel/include/file.hrl").

-define(DETS_SERVER, dets).

%% HEADSZ taken from dets_v8.erl and dets_v9.erl.
-define(HEADSZ_v8, 40).
-define(HEADSZ_v9, (56+28*4+16)).
-define(NO_KEYS_POS_v9, 36).
-define(CLOSED_PROPERLY_POS, 8).

-define(NOT_PROPERLY_CLOSED,0).
-define(CLOSED_PROPERLY,1).

init_per_testcase(_Case, Config) ->
    Dog=?t:timetrap(?t:minutes(15)),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, _Config) ->
    Dog=?config(watchdog, _Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(suite) ->
    case os:type() of
	vxworks ->
	    [not_run];
	_ ->
	    {req,[stdlib],
	     [basic_v8, basic_v9, open_v8, open_v9, sets_v8, sets_v9,
	      bags_v8, bags_v9, duplicate_bags_v8, duplicate_bags_v9,
	      newly_started, open_file_v8, open_file_v9,
	      init_table_v8, init_table_v9, repair_v8, repair_v9,
	      access_v8, access_v9, oldbugs_v8, oldbugs_v9,
	      unsafe_assumptions, truncated_segment_array_v8,
	      truncated_segment_array_v9, dirty_mark, dirty_mark2,
	      bag_next_v8, bag_next_v9, hash_v8b_v8c, phash, fold_v8,
	      fold_v9, fixtable_v8, fixtable_v9, match_v8, match_v9,
	      select_v8, select_v9, update_counter, badarg,
	      cache_sets_v8, cache_sets_v9, cache_bags_v8,
	      cache_bags_v9, cache_duplicate_bags_v8,
	      cache_duplicate_bags_v9, otp_4208, otp_4989, many_clients,
              otp_4906, otp_5402, simultaneous_open, insert_new, 
              repair_continuation, otp_5487, otp_6206, otp_6359, otp_4738,
              otp_7146, otp_8070]} 
    end.

not_run(suite) -> [];
not_run(Conf) when is_list(Conf) ->
    {comment, "Not runnable VxWorks/NFS"}.

newly_started(doc) ->
    ["OTP-3621"];
newly_started(suite) -> 
    [];
newly_started(Config) when is_list(Config) ->
    ?line true = is_alive(),
    ?line {ok, Node} = test_server:start_node(slave1, slave, []),
    ?line [] = rpc:call(Node, dets, all, []),
    ?line test_server:stop_node(Node),
    ok.

basic_v8(doc) ->
    ["Basic test case."];
basic_v8(suite) -> 
    [];
basic_v8(Config) when is_list(Config) ->
    basic(Config, 8).

basic_v9(doc) ->
    ["Basic test case."];
basic_v9(suite) -> 
    [];
basic_v9(Config) when is_list(Config) ->
    basic(Config, 9).

basic(Config, Version) ->
    ?line Tab = dets_basic_test,
    ?line FName = filename(Tab, Config),

    P0 = pps(),
    ?line {ok, _} = dets:open_file(Tab,[{file, FName},{version,Version}]),
    ?line ok = dets:insert(Tab,{mazda,japan}),
    ?line ok = dets:insert(Tab,{toyota,japan}),
    ?line ok = dets:insert(Tab,{suzuki,japan}),
    ?line ok = dets:insert(Tab,{honda,japan}),
    ?line ok = dets:insert(Tab,{renault,france}),
    ?line ok = dets:insert(Tab,{citroen,france}),
    ?line ok = dets:insert(Tab,{opel,germany}),
    ?line ok = dets:insert(Tab,{saab,sweden}),
    ?line ok = dets:insert(Tab,{volvo,sweden}),
    ?line [{opel,germany}] = dets:lookup(Tab,opel),
    ?line Japs = dets:traverse(Tab, fun(Obj) -> 
					   case Obj of 
					       {_, japan} -> {continue, Obj};
					       _ -> continue
					   end
				    end),
    ?line 4  = length(Japs),
    ?line ok = dets:close(Tab),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.
    

open_v8(doc) ->
    [];
open_v8(suite) -> 
    [];
open_v8(Config) when is_list(Config) ->
    open(Config, 8).

open_v9(doc) ->
    [];
open_v9(suite) -> 
    [];
open_v9(Config) when is_list(Config) ->
    open(Config, 9).

open(Config, Version) ->
    %% Running this test twice means that the Dets server is restarted
    %% twice. dets_sup specifies a maximum of 4 restarts in an hour.
    %% If this becomes a problem, one should consider running this
    %% test on a slave node.

    ?line {Sets, Bags, Dups} = args(Config),
    
    ?line All = Sets ++ Bags ++ Dups,
    ?line delete_files(All),

    ?line Data = make_data(1),

    P0 = pps(),
    ?line Tabs = open_files(1, All, Version),
    ?line initialize(Tabs, Data),
    ?line check(Tabs, Data),

    ?line foreach(fun(Tab) -> ok = dets:close(Tab) end, Tabs),
    %% Now reopen the files
    ?format("Reopening closed files \n", []),
    ?line Tabs = open_files(1, All, Version),
    ?format("Checking contents of reopened files \n", []),
    ?line check(Tabs, Data),
    %% crash the dets server

    ?format("Crashing dets server \n", []),
    process_flag(trap_exit, true),
    Procs = [whereis(?DETS_SERVER) | map(fun(Tab) -> dets:info(Tab, pid) end,
				 Tabs)],
    foreach(fun(Pid) -> exit(Pid, kill) end, Procs),
    timer:sleep(100),
    c:flush(),  %% flush all the EXIT sigs
    timer:sleep(200),

    %% Now reopen the files again
    ?format("Reopening crashed files \n", []),
    ?line open_files(1, All, Version),
    ?format("Checking contents of repaired files \n", []),
    ?line check(Tabs, Data),
    
    ?line close_all(Tabs),

    ?line delete_files(All),
    P1 = pps(),
    {Ports0, Procs0} = P0,
    {Ports1, Procs1} = P1,
    ?line true = Ports1 =:= Ports0,
    %% The dets_server process has been restarted:
    ?line [_] = Procs0 -- Procs1,
    ?line [_] = Procs1 -- Procs0,
    ok.
    
check(Tabs, Data) ->
    foreach(fun(Tab) ->
		    ?line Kp = dets:info(Tab, keypos), 
		    ?format("checking ~p~n", [Tab]),
		    foreach(fun(Item) ->
				    case dets:lookup(Tab, k(Kp,Item)) of
					[Item] -> ok;
					_Other -> bad(Tab,Item)
				    end
			    end, Data)
	    end, Tabs),
    ok.
    
k(Kp, Obj) -> element(Kp, Obj).

bad(_Tab, _Item) ->
    ?format("Can't find item ~p in ~p ~n", [_Item, _Tab]),
    exit(badtab).

sets_v8(doc) ->
    ["Performs traversal and match testing on set type dets tables."];
sets_v8(suite) ->
    [];
sets_v8(Config) when is_list(Config) ->
    sets(Config, 8).

sets_v9(doc) ->
    ["Performs traversal and match testing on set type dets tables."];
sets_v9(suite) ->
    [];
sets_v9(Config) when is_list(Config) ->
    sets(Config, 9).

sets(Config, Version) ->
    ?line {Sets, _, _} = args(Config),

    ?line Data = make_data(1),
    ?line delete_files(Sets),
    P0 = pps(),
    ?line Tabs = open_files(1, Sets, Version),
    Bigger = [{17,q,w,w}, {48,q,w,w,w,w,w,w}], % 48 requires a bigger buddy
    ?line initialize(Tabs, Data++Bigger++Data), % overwrite
    ?line Len = length(Data),
    ?line foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    ?line size_test(Len, Tabs),
    ?line no_keys_test(Tabs),
    ?line foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) ->
			  Len = dets:info(Tab, size) end, 
		  Tabs),
    ?line foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    ?line foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    
    ?line close_all(Tabs),
    ?line delete_files(Sets),
    ?line check_pps(P0),
    ok.

bags_v8(doc) ->
    ["Performs traversal and match testing on bag type dets tables."];
bags_v8(suite) ->
    [];
bags_v8(Config) when is_list(Config) ->
    bags(Config, 8).

bags_v9(doc) ->
    ["Performs traversal and match testing on bag type dets tables."];
bags_v9(suite) ->
    [];
bags_v9(Config) when is_list(Config) ->
    bags(Config, 9).

bags(Config, Version) ->
    {_, Bags, _} = args(Config),
    ?line Data = make_data(1, bag),  %% gives twice as many objects
    ?line delete_files(Bags),
    P0 = pps(),
    ?line Tabs = open_files(1, Bags, Version),
    ?line initialize(Tabs, Data++Data),
    ?line Len = length(Data),
    ?line foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    ?line size_test(Len, Tabs),
    ?line no_keys_test(Tabs),
    ?line foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) ->
			  Len = dets:info(Tab, size) end, 
		  Tabs),
    ?line foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    ?line foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    ?line close_all(Tabs),
    ?line delete_files(Bags),
    ?line check_pps(P0),
    ok.


duplicate_bags_v8(doc) ->
   ["Performs traversal and match testing on duplicate_bag type dets tables."];
duplicate_bags_v8(suite) ->
    [];
duplicate_bags_v8(Config) when is_list(Config) ->
    duplicate_bags(Config, 8).

duplicate_bags_v9(doc) ->
   ["Performs traversal and match testing on duplicate_bag type dets tables."];
duplicate_bags_v9(suite) ->
    [];
duplicate_bags_v9(Config) when is_list(Config) ->
    duplicate_bags(Config, 9).

duplicate_bags(Config, Version) when is_list(Config) ->
    {_, _, Dups} = args(Config),
    ?line Data = make_data(1, duplicate_bag), %% gives twice as many objects
    ?line delete_files(Dups),
    P0 = pps(),
    ?line Tabs = open_files(1, Dups, Version),
    ?line initialize(Tabs, Data),
    ?line Len = length(Data),
    ?line foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    ?line size_test(Len, Tabs),
    ?line no_keys_test(Tabs),
    ?line foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    ?line initialize(Tabs, Data),
    ?line foreach(fun(Tab) ->
			  Len = dets:info(Tab, size) end, 
		  Tabs),
    ?line foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    ?line foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    ?line close_all(Tabs),
    ?line delete_files(Dups),
    ?line check_pps(P0),
    ok.


access_v8(doc) ->
    [];
access_v8(suite) ->
    [];
access_v8(Config) when is_list(Config) ->
    access(Config, 8).

access_v9(doc) ->
    [];
access_v9(suite) ->
    [];
access_v9(Config) when is_list(Config) ->
    access(Config, 9).

access(Config, Version) ->
    Args_acc = [[{ram_file, true}, {access, read}],
		[{access, read}]],
    Args = [[{ram_file, true}],
	    []],
    
    ?line {Args_acc_1, _, _} = zip_filename(Args_acc, [], [], Config),
    ?line delete_files(Args_acc_1),
    ?line {Args_1, _, _} = zip_filename(Args, [], [], Config),

    P0 = pps(),
    ?line {error, {file_error,_,enoent}} = dets:open_file('1', hd(Args_acc_1)),

    ?line Tabs = open_files(1, Args_1, Version),
    ?line close_all(Tabs),
    ?line Tabs = open_files(1, Args_acc_1, Version),

    ?line foreach(fun(Tab) ->
			  {error, {access_mode,_}} = dets:insert(Tab, {1,2}),
			  [] = dets:lookup(Tab, 11),
			  '$end_of_table' = dets:first(Tab),
			  {error, {access_mode,_}} = dets:delete(Tab, 22)
		  end, Tabs),
    ?line close_all(Tabs),
    ?line delete_files(Args_acc_1),
    ?line check_pps(P0),
    ok.


dirty_mark(doc) ->
    ["Test that the table is not marked dirty if not written"];
dirty_mark(suite) ->
    [];
dirty_mark(Config) when is_list(Config) ->
    ?line true = is_alive(),
    ?line Tab = dets_dirty_mark_test,
    ?line FName = filename(Tab, Config),
    P0 = pps(),
    ?line dets:open_file(Tab,[{file, FName}]),
    ?line dets:insert(Tab,{mazda,japan}),
    ?line dets:insert(Tab,{toyota,japan}),
    ?line dets:insert(Tab,{suzuki,japan}),
    ?line dets:insert(Tab,{honda,japan}),
    ?line dets:insert(Tab,{renault,france}),
    ?line dets:insert(Tab,{citroen,france}),
    ?line dets:insert(Tab,{opel,germany}),
    ?line dets:insert(Tab,{saab,sweden}),
    ?line dets:insert(Tab,{volvo,sweden}),
    ?line [{opel,germany}] = dets:lookup(Tab,opel),
    ?line ok = dets:close(Tab),
    ?line Call = fun(P,A) ->
		   P ! {self(), A},
		   receive
		       {P, Ans} ->
			   Ans
		   after 5000 ->
			   exit(other_process_dead)
		   end
	   end,
    ?line {ok, Node} = test_server:start_node(dets_dirty_mark, 
					      slave,
					      [{linked, false},
					       {args, "-pa " ++ 
					       filename:dirname
						(code:which(?MODULE))}]),
    ?line ok = ensure_node(20, Node),
    %% io:format("~p~n",[rpc:call(Node, code, get_path, [])]),
    %% io:format("~p~n",[rpc:call(Node, file, get_cwd, [])]),
    %% io:format("~p~n",[Config]),
    ?line Pid = rpc:call(Node,erlang, spawn, 
			 [?MODULE, dets_dirty_loop, []]),
    ?line {ok, Tab} = Call(Pid, [open, Tab, [{file, FName}]]),
    ?line [{opel,germany}] = Call(Pid, [read,Tab,opel]),
    ?line test_server:stop_node(Node),
    ?line {ok, Tab} = dets:open_file(Tab,[{file, FName}, 
					  {repair,false}]),
    ?line ok = dets:close(Tab),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.

dirty_mark2(doc) ->
    ["Test that the table is flushed when auto_save is in effect"];
dirty_mark2(suite) ->
    [];
dirty_mark2(Config) when is_list(Config) ->
    ?line true = is_alive(),
    ?line Tab = dets_dirty_mark2_test,
    ?line FName = filename(Tab, Config),
    P0 = pps(),
    ?line dets:open_file(Tab,[{file, FName}]),
    ?line dets:insert(Tab,{toyota,japan}),
    ?line dets:insert(Tab,{suzuki,japan}),
    ?line dets:insert(Tab,{honda,japan}),
    ?line dets:insert(Tab,{renault,france}),
    ?line dets:insert(Tab,{citroen,france}),
    ?line dets:insert(Tab,{opel,germany}),
    ?line dets:insert(Tab,{saab,sweden}),
    ?line dets:insert(Tab,{volvo,sweden}),
    ?line [{opel,germany}] = dets:lookup(Tab,opel),
    ?line ok = dets:close(Tab),
    ?line Call = fun(P,A) ->
		   P ! {self(), A},
		   receive
		       {P, Ans} ->
			   Ans
		   after 5000 ->
			   exit(other_process_dead)
		   end
	   end,
    ?line {ok, Node} = test_server:start_node(dets_dirty_mark2, 
					      slave,
					      [{linked, false},
					       {args, "-pa " ++ 
					       filename:dirname
						(code:which(?MODULE))}]),
    ?line ok = ensure_node(20, Node),
    ?line Pid = rpc:call(Node,erlang, spawn, 
			 [?MODULE, dets_dirty_loop, []]),
    ?line {ok, Tab} = Call(Pid, [open, Tab, [{file, FName},{auto_save,1000}]]),
    ?line ok = Call(Pid, [write,Tab,{mazda,japan}]),
    ?line timer:sleep(2100),
    %% Read something, just to give auto save time to finish.
    ?line [{opel,germany}] = Call(Pid, [read,Tab,opel]),
    ?line test_server:stop_node(Node),
    ?line {ok, Tab} = dets:open_file(Tab, [{file, FName}, {repair,false}]),
    ?line ok = dets:close(Tab),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.

dets_dirty_loop() ->
    receive 
	{From, [open, Name, Args]} ->
	    Ret = dets:open_file(Name, Args),
	    From ! {self(), Ret},
	    dets_dirty_loop();
	{From, [read, Name, Key]} ->
	    Ret = dets:lookup(Name, Key),
	    From ! {self(), Ret},
	    dets_dirty_loop();
	{From, [write, Name, Value]} ->
	    Ret = dets:insert(Name, Value),
	    From ! {self(), Ret},
	    dets_dirty_loop()
    end.


bag_next_v8(suite) ->
    [];
bag_next_v8(doc) ->
    ["Check that bags and next work as expected."];
bag_next_v8(Config) when is_list(Config) ->
    bag_next(Config, 8).

bag_next_v9(suite) ->
    [];
bag_next_v9(doc) ->
    ["Check that bags and next work as expected."];
bag_next_v9(Config) when is_list(Config) ->
    ?line Tab = dets_bag_next_test,
    ?line FName = filename(Tab, Config),

    %% first and next crash upon error
    ?line dets:open_file(Tab,[{file, FName}, {type, bag},{version,9}]),
    ?line ok = dets:insert(Tab, [{1,1},{2,2},{3,3},{4,4}]),
    ?line FirstKey = dets:first(Tab),
    ?line NextKey = dets:next(Tab, FirstKey),
    ?line [FirstObj | _] = dets:lookup(Tab, FirstKey),
    ?line [NextObj | _] = dets:lookup(Tab, NextKey),
    ?line {ok, FirstPos} = dets:where(Tab, FirstObj),
    ?line {ok, NextPos} = dets:where(Tab, NextObj),
    crash(FName, NextPos+12),
    ?line {'EXIT',BadObject1} = (catch dets:next(Tab, FirstKey)),
    ?line bad_object(BadObject1, FName),
    crash(FName, FirstPos+12),
    ?line {'EXIT',BadObject2} = (catch dets:first(Tab)),
    ?line bad_object(BadObject2, FName),
    ?line dets:close(Tab),
    ?line file:delete(FName),

    bag_next(Config, 9).

bag_next(Config, Version) ->
    ?line Tab = dets_bag_next_test,
    ?line FName = filename(Tab, Config),
    P0 = pps(),
    ?line dets:open_file(Tab,[{file, FName}, {type, bag},{version,Version}]),
    ?line dets:insert(Tab,{698,hopp}),
    ?line dets:insert(Tab,{186,hopp}),
    ?line dets:insert(Tab,{hej,hopp}),
    ?line dets:insert(Tab,{186,plopp}),
    Loop = fun(N, Last, Self) ->
		   case N of
		       0 ->
			   exit({unterminated_first_next_sequence, N, Last});
		       _ ->
			   case Last of
			       '$end_of_table' ->
				   ok;
			       _ ->
				   Self(N-1, dets:next(Tab,Last), Self)
			   end
		   end
	   end,
    ?line ok = Loop(4,dets:first(Tab),Loop),
    ?line dets:close(Tab),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.

oldbugs_v8(doc) ->
    [];
oldbugs_v8(suite) ->
    [];
oldbugs_v8(Config) when is_list(Config) ->
    oldbugs(Config, 8).

oldbugs_v9(doc) ->
    [];
oldbugs_v9(suite) ->
    [];
oldbugs_v9(Config) when is_list(Config) ->
    oldbugs(Config, 9).

oldbugs(Config, Version) ->
    FName = filename(dets_suite_oldbugs_test, Config),
    P0 = pps(),
    ?line {ok, ob} = dets:open_file(ob, [{version, Version},
					 {type, bag}, {file, FName}]),
    ?line ok = dets:insert(ob, {1, 2}),
    ?line ok = dets:insert(ob, {1,3}),
    ?line ok = dets:insert(ob, {1, 2}),
    ?line 2 = dets:info(ob, size),  %% assertion
    ?line ok = dets:close(ob),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.

unsafe_assumptions(suite) -> [];
unsafe_assumptions(doc) ->
    "Tests that shrinking an object and then expanding it works.";
unsafe_assumptions(Config) when is_list(Config) ->
    FName = filename(dets_suite_unsafe_assumptions_test, Config),
    ?line file:delete(FName),
    P0 = pps(),
    ?line {ok, a} = dets:open_file(a, [{version,8},{file, FName}]),
    O0 = {2,false},
    O1 = {1, false},
    O2 = {1, true},
    O3 = {1, duplicate(20,false)},
    O4 = {1, duplicate(25,false)}, % same 2-log as O3
    ?line ok = dets:insert(a, O1),
    ?line ok = dets:insert(a, O0),
    ?line true = [O1,O0] =:= sort(get_all_objects(a)),
    ?line true = [O1,O0] =:= sort(get_all_objects_fast(a)),
    ?line ok = dets:insert(a, O2),
    ?line true = [O2,O0] =:= sort(get_all_objects(a)),
    ?line true = [O2,O0] =:= sort(get_all_objects_fast(a)),
    ?line ok = dets:insert(a, O3),
    ?line true = [O3,O0] =:= sort(get_all_objects(a)),
    ?line true = [O3,O0] =:= sort(get_all_objects_fast(a)),
    ?line ok = dets:insert(a, O4),
    ?line true = [O4,O0] =:= sort(get_all_objects(a)),
    ?line true = [O4,O0] =:= sort(get_all_objects_fast(a)),
    ?line ok = dets:close(a),
    ?line file:delete(FName),
    ?line check_pps(P0),
    ok.

truncated_segment_array_v8(suite) -> [];
truncated_segment_array_v8(doc) ->
    "Tests that a file where the segment array has been truncated "
    "is possible to repair.";
truncated_segment_array_v8(Config) when is_list(Config) ->
    trunc_seg_array(Config, 8).

truncated_segment_array_v9(suite) -> [];
truncated_segment_array_v9(doc) ->
    "Tests that a file where the segment array has been truncated "
    "is possible to repair.";
truncated_segment_array_v9(Config) when is_list(Config) ->
    trunc_seg_array(Config, 9).

trunc_seg_array(Config, V) ->
    TabRef = dets_suite_truncated_segment_array_test,
    Fname = filename(TabRef, Config),
    %% Create file that needs to be repaired
    ?line file:delete(Fname),
    P0 = pps(),
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file, Fname},{version,V}]),
    ?line ok = dets:close(TabRef),
    
    %% Truncate the file
    ?line HeadSize = headsz(V),
    ?line truncate(Fname, HeadSize + 10),
    
    %% Open the truncated file
    ?line io:format("Expect repair:~n"),
    ?line {ok, TabRef} = dets:open_file(TabRef, 
					[{file, Fname}, {repair, true}]),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),
    ?line check_pps(P0),
    ok.

open_file_v8(doc) ->
    ["open_file/1 test case."];
open_file_v8(suite) -> 
    [];
open_file_v8(Config) when is_list(Config) ->
    open_1(Config, 8).

open_file_v9(doc) ->
    ["open_file/1 test case."];
open_file_v9(suite) -> 
    [];
open_file_v9(Config) when is_list(Config) ->
    T = open_v9,
    Fname = filename(T, Config),
    ?line {ok, _} = dets:open_file(T, [{file,Fname},{version,9}]),
    ?line 9 = dets:info(T, version),
    ?line true = [self()] =:= dets:info(T, users),
    ?line {ok, _} = dets:open_file(T, [{file,Fname},{version,9}]),
    ?line {error,incompatible_arguments} = 
	dets:open_file(T, [{file,Fname},{version,8}]),
    ?line true = [self(),self()] =:= dets:info(T, users),
    ?line ok = dets:close(T),
    ?line true = [self()] =:= dets:info(T, users),
    ?line ok = dets:close(T),
    ?line undefined = ets:info(T, users),
    ?line file:delete(Fname),

    open_1(Config, 9).

open_1(Config, V) ->
    TabRef = open_file_1_test,
    Fname = filename(TabRef, Config),
    ?line file:delete(Fname),

    P0 = pps(),
    ?line {error,{file_error,Fname,enoent}} = dets:open_file(Fname),
    
    ?line ok = file:write_file(Fname, duplicate(100,65)),
    ?line {error,{not_a_dets_file,Fname}} = dets:open_file(Fname),
    ?line file:delete(Fname),

    HeadSize = headsz(V),
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file, Fname},{version,V}]),
    ?line ok = dets:close(TabRef),
    ?line truncate(Fname, HeadSize + 10),
    ?line true = dets:is_dets_file(Fname),
    ?line io:format("Expect repair:~n"),
    ?line {ok, Ref} = dets:open_file(Fname), % repairing
    ?line ok = dets:close(Ref),
    ?line file:delete(Fname),

    %% truncated file header, invalid type
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = ins(TabRef, 3000),
    ?line ok = dets:close(TabRef),
    TypePos = 12,
    crash(Fname, TypePos),
    ?line {error, {invalid_type_code,Fname}} = dets:open_file(Fname),
    ?line truncate(Fname, HeadSize - 10),    
    ?line {error, {tooshort,Fname}} = dets:open_file(Fname),
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {error,{file_error,{foo,bar},_}} = dets:is_dets_file({foo,bar}),
    ?line check_pps(P0),
    ok.

init_table_v8(doc) ->
    ["initialize_table/2 and from_ets/2 test case."];
init_table_v8(suite) -> 
    [];
init_table_v8(Config) when is_list(Config) ->
    init_table(Config, 8).

init_table_v9(doc) ->
    ["initialize_table/2 and from_ets/2 test case."];
init_table_v9(suite) -> 
    [];
init_table_v9(Config) when is_list(Config) ->
    %% Objects are returned in "time order".
    T = init_table_v9,
    Fname = filename(T, Config),
    ?line file:delete(Fname),
    L = [{1,a},{2,b},{1,c},{2,c},{1,c},{2,a},{1,b}],
    Input = init([L]),
    ?line {ok, _} = dets:open_file(T, [{file,Fname},{version,9},
				       {type,duplicate_bag}]),
    ?line ok = dets:init_table(T, Input),
    ?line [{1,a},{1,c},{1,c},{1,b}] = dets:lookup(T, 1),
    ?line [{2,b},{2,c},{2,a}] = dets:lookup(T, 2),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    init_table(Config, 9),
    fast_init_table(Config).

init_table(Config, V) ->
    TabRef = init_table_test,
    Fname = filename(TabRef, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{version,V},{auto_save,120000}],
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', _} = 
	(catch dets:init_table(TabRef, fun(foo) -> bar end)),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', _} = (catch dets:init_table(TabRef, fun() -> foo end)),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', {badarg, _}} = (catch dets:init_table(TabRef, nofun)),
    ?line {'EXIT', {badarg, _}} = 
	(catch dets:init_table(TabRef, fun(_X) -> end_of_input end, 
			       [{foo,bar}])),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line away = (catch dets:init_table(TabRef, fun(_) -> throw(away) end)),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {error, {init_fun, fopp}} = 
	dets:init_table(TabRef, fun(read) -> fopp end),
    dets:close(TabRef),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line dets:safe_fixtable(TabRef, true),
    ?line {error, {fixed_table, TabRef}} = dets:init_table(TabRef, init([])),
    ?line dets:safe_fixtable(TabRef, false),
    ?line ET = ets:new(foo,[]),
    ?line ok = dets:from_ets(TabRef, ET),
    ?line [] = get_all_objects(TabRef),
    ?line [] = get_all_objects_fast(TabRef),
    ?line true = ets:insert(ET, {1,a}),
    ?line true = ets:insert(ET, {2,b}),
    ?line ok = dets:from_ets(TabRef, ET),
    ?line [{1,a},{2,b}] = sort(get_all_objects(TabRef)),
    ?line [{1,a},{2,b}] = sort(get_all_objects_fast(TabRef)),
    ?line true = ets:delete(ET),
    ?line 120000 = dets:info(TabRef, auto_save),
    ?line ok = dets:close(TabRef),

    ?line {ok, _} = dets:open_file(TabRef, [{access,read} | Args]),
    ?line {error, {access_mode, Fname}} = dets:init_table(TabRef, init([])),
    ?line ok = dets:close(TabRef),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {error, invalid_objects_list} =
	(catch dets:init_table(TabRef, init([[{1,2},bad,{3,4}]]))),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    L1 = [[{1,a},{2,b}],[],[{3,c}],[{4,d}],[]],
    bulk_init(L1, set, 4, Config, V),
    L2 = [[{1,a},{2,b}],[],[{2,q},{3,c}],[{4,d}],[{4,e},{2,q}]],
    bulk_init(L2, set, 4, Config, V),
    bulk_init(L2, bag, 6, Config, V),
    bulk_init(L2, duplicate_bag, 7, Config, V),
    bulk_init(L1, set, 4, 512, Config, V),
    bulk_init([], set, 0, 10000, Config, V),
    file:delete(Fname),

    %% Initiate a file that contains a lot of objects.
    ?line {ok, _} = dets:open_file(TabRef, [{min_no_slots,10000} | Args]),
    ?line ok = ins(TabRef, 6000),
    Fun = init_fun(0, 10000),
    ?line ok = dets:init_table(TabRef, Fun,{format,term}),
    ?line All = sort(get_all_objects(TabRef)),
    ?line FAll = get_all_objects_fast(TabRef),
    ?line true = All =:= sort(FAll),
    ?line true = length(All) =:= 10000,
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, [{min_no_slots,4000} | Args]),
    ?line ok = ins(TabRef, 6000),
    ?line FileSize1 = dets:info(TabRef, file_size),
    Fun2 = init_fun(0, 4000),
    ?line ok = dets:init_table(TabRef, Fun2),
    ?line FileSize2 = dets:info(TabRef, file_size),
    ?line ok = dets:close(TabRef),
    ?line true = FileSize1 > FileSize2,
    ?line file:delete(Fname),

    ?line check_pps(P0),
    ok.

bulk_init(Ls, Type, N, Config, V) ->
    bulk_init(Ls, Type, N, 256, Config, V).

bulk_init(Ls, Type, N, Est, Config, V) ->
    T = init_table_test,
    Fname = filename(T, Config),
    ?line file:delete(Fname),
    Input = init(Ls),
    Args = [{ram_file,false}, {type,Type},{keypos,1},{file,Fname},
	    {estimated_no_objects, Est},{version,V}],
    ?line {ok, T} = dets:open_file(T, Args),
    ?line ok = dets:init_table(T, Input),
    ?line All = sort(get_all_objects(T)),
    ?line FAll = get_all_objects_fast(T),
    ?line true = All =:= sort(FAll),
    ?line true = length(All) =:= N,
    ?line true = dets:info(T, size) =:= N,
    ?line ok = dets:close(T),
    
    ?line {ok, T} = dets:open_file(T, Args),
    ?line All2 = sort(get_all_objects(T)),
    ?line FAll2 = get_all_objects_fast(T),
    ?line true = All =:= All2,
    ?line true = All =:= sort(FAll2),
    ?line ok = dets:close(T),
    ?line file:delete(Fname).

init(L) ->
    fun(close) ->
	    ok;
       (read) when [] =:= L ->
	    end_of_input;
       (read) ->
	    [E | Es] = L,
	    {E, init(Es)}
    end.

init_fun(I, N) ->
    fun(read) when I =:= N ->
	    end_of_input;
       (read) ->
	    {NewN, Items} = items(I, N, 1000, []),
	    {Items, init_fun(NewN, N)};
       (close) ->
	    ignored
    end.

fast_init_table(Config) ->
    V = 9,
    TabRef = init_table_test,
    Fname = filename(TabRef, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{version,V},{auto_save,120000}],

    Source = init_table_test_source,
    SourceFname = filename(Source, Config),
    ?line file:delete(SourceFname),
    SourceArgs = [{file,SourceFname},{version,V},{auto_save,120000}],

    ?line {ok, Source} = dets:open_file(Source, SourceArgs),
    
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', _} = 
	(catch dets:init_table(TabRef, fun(foo) -> bar end, {format,bchunk})),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', _} = (catch dets:init_table(TabRef, fun() -> foo end,
					       {format,bchunk})),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', {badarg, _}} = 
	(catch dets:init_table(TabRef, nofun, {format,bchunk})),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line away = (catch dets:init_table(TabRef, fun(_) -> throw(away) end,
					{format,bchunk})),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {error, {init_fun, fopp}} = 
	dets:init_table(TabRef, fun(read) -> fopp end, {format,bchunk}),
    dets:close(TabRef),
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line dets:safe_fixtable(TabRef, true),
    ?line {error, {fixed_table, TabRef}} = 
	dets:init_table(TabRef, init([]), {format,bchunk}),
    ?line dets:safe_fixtable(TabRef, false),
    ?line ok = dets:close(TabRef),

    ?line {ok, _} = dets:open_file(TabRef, [{access,read} | Args]),
    ?line {error, {access_mode, Fname}} = 
	dets:init_table(TabRef, init([]), {format,bchunk}),
    ?line ok = dets:close(TabRef),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {error, {init_fun,{1,2}}} =
	dets:init_table(TabRef, init([[{1,2},bad,{3,4}]]), {format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {error, {init_fun, end_of_input}} = 
	dets:init_table(TabRef, init([]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line {'EXIT', {badarg, _}} = 
	(catch dets:init_table(TabRef, init([]),{format,foppla})),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line ok = ins(TabRef, 100),

    ?line [BParms | Objs] = collect_bchunk(TabRef, init_bchunk(TabRef)),
    ?line Parms = binary_to_term(BParms),
    ?line {error, {init_fun, <<"foobar">>}} = 
	dets:init_table(TabRef, init([[<<"foobar">>]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line Parms1 = setelement(1, Parms, foobar),
    BParms1 = term_to_binary(Parms1),
    ?line {error, {init_fun, BParms1}} = 
	dets:init_table(TabRef, init([[BParms1 | Objs]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    [{Sz1,No1} | NoColls17] = element(tuple_size(Parms), Parms),
    Parms2 = setelement(tuple_size(Parms), Parms, [{Sz1,No1+1} | NoColls17]),
    BParms2 = term_to_binary(Parms2),
    ?line {error, invalid_objects_list} = 
	dets:init_table(TabRef, init([[BParms2 | Objs]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line [{LSize1,Slot1,Obj1} | ObjsRest] = Objs,
  
    ?line BadSize = byte_size(Obj1)-1,
    ?line <<BadSizeObj:BadSize/binary,_:1/binary>> = Obj1,
    ?line BadObjs = [{LSize1,Slot1,BadSizeObj} | ObjsRest],
    ?line {error, invalid_objects_list} = 
	dets:init_table(TabRef, init([[BParms | BadObjs]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line <<Size:32,BigObj0/binary>> = list_to_binary(lists:duplicate(16,Obj1)),
    ?line BigObj = <<(Size*16):32,BigObj0/binary>>,
    ?line BadColl = [BParms, {LSize1+4,Slot1,BigObj} | ObjsRest],
    ?line {error, invalid_objects_list} = 
         dets:init_table(TabRef, init([BadColl]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, Args),
    BadObj = <<"foobar">>,
    ?line {error, invalid_objects_list} = 
	dets:init_table(TabRef, init([[BParms, BadObj]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(TabRef, [{type,bag} | Args]),
    ?line {error, {init_fun, _}} = 
	dets:init_table(TabRef, init([[BParms]]),{format,bchunk}),
    ?line _ = dets:close(TabRef),
    ?line file:delete(Fname),

    ?line ok = dets:close(Source),
    ?line file:delete(SourceFname),

    L1 = [{1,a},{2,b},{3,c},{4,d}],
    fast_bulk_init(L1, set, 4, 4, Config, V),
    L2 = [{1,a},{2,b},{2,q},{3,c},{4,d},{4,e},{2,q}],
    fast_bulk_init(L2, set, 4, 4, Config, V),
    fast_bulk_init(L2, bag, 6, 4, Config, V),
    fast_bulk_init(L2, duplicate_bag, 7, 4, Config, V),
    fast_bulk_init(L1, set, 4, 4, 512, Config, V),
    fast_bulk_init([], set, 0, 0, 10000, Config, V),
    file:delete(Fname),

    %% Initiate a file that contains a lot of objects.
    ?line {ok, _} = dets:open_file(Source, [{min_no_slots,10000} | SourceArgs]),
    Fun1 = init_fun(0, 10000),
    ?line ok = dets:init_table(Source, Fun1, {format,term}),
    
    ?line {ok, _} = dets:open_file(TabRef, [{min_no_slots,10000} | Args]),
    ?line ok = ins(TabRef, 6000),
    Fun2 = init_bchunk(Source),
    ?line true = 
        dets:is_compatible_bchunk_format(TabRef, 
                                         dets:info(Source, bchunk_format)),
    ?line false = dets:is_compatible_bchunk_format(TabRef, <<"foobar">>),
    ?line ok = dets:init_table(TabRef, Fun2, {format, bchunk}),
    ?line ok = dets:close(Source),
    ?line file:delete(SourceFname),
    ?line All = sort(get_all_objects(TabRef)),
    ?line FAll = get_all_objects_fast(TabRef),
    ?line true = All =:= sort(FAll),
    ?line true = length(All) =:= 10000,
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    %% Initiate inserts fewer objects than the table contains.
    ?line {ok, _} = dets:open_file(Source, [{min_no_slots,1000} | SourceArgs]),
    ?line ok = ins(Source, 4000),
    
    ?line {ok, _} = dets:open_file(TabRef, [{min_no_slots,1000} | Args]),
    ?line ok = ins(TabRef, 6000),
    ?line FileSize1 = dets:info(TabRef, file_size),
    Fun4 = init_bchunk(Source),
    ?line ok = dets:init_table(TabRef, Fun4, {format, bchunk}),
    ?line ok = dets:close(Source),
    ?line file:delete(SourceFname),
    ?line FileSize2 = dets:info(TabRef, file_size),
    ?line All_2 = sort(get_all_objects(TabRef)),
    ?line FAll_2 = get_all_objects_fast(TabRef),
    ?line true = All_2 =:= sort(FAll_2),
    ?line true = length(All_2) =:= 4000,
    ?line ok = dets:close(TabRef),
    ?line true = FileSize1 > FileSize2,

    %% Bchunk and fixed table.
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line NoItems = dets:info(TabRef, no_objects),
    ?line AllObjects1 = sort(get_all_objects_fast(TabRef)),
    ?line dets:safe_fixtable(TabRef, true),
    ?line true = dets:info(TabRef, fixed),
    ?line Cont1 = init_bchunk(TabRef),
    ?line NoDel = 
	dets:select_delete(TabRef, [{{'_',{item,'_','_'}},[],[true]}]),
    ?line true = (NoDel > 0),
    ?line AllObjects2 = sort(get_all_objects_fast(TabRef)),
    ?line true = dets:info(TabRef, fixed),
    ?line Cont2 = init_bchunk(TabRef),
    ?line NoItems2 = dets:info(TabRef, no_objects),
    ?line true = (NoItems =:= NoItems2 + NoDel),
    ?line NoDel2 = dets:select_delete(TabRef, [{'_',[],[true]}]),
    ?line true = (NoDel2 > 0),
    ?line AllObjects3 = sort(get_all_objects_fast(TabRef)),
    ?line NoItems3 = dets:info(TabRef, no_objects),
    ?line true = (NoItems3 =:= 0),
    ?line true = dets:info(TabRef, fixed),
    ?line true = (NoItems2 =:= NoItems3 + NoDel2),
    ?line Cont3 = init_bchunk(TabRef),

    ?line BinColl1 = collect_bchunk(TabRef, Cont1),
    ?line BinColl2 = collect_bchunk(TabRef, Cont2),
    ?line BinColl3 = collect_bchunk(TabRef, Cont3),
    ?line dets:safe_fixtable(TabRef, false),
    ?line ok = dets:close(TabRef),    
    ?line file:delete(Fname),

    %% Now check that the above collected binaries are correct.
    ?line {ok, _} = dets:open_file(TabRef, Args),
    ?line ok = dets:init_table(TabRef, init([BinColl1]),{format,bchunk}),
    ?line true = (AllObjects1 =:= sort(get_all_objects_fast(TabRef))),
    ?line true = (length(AllObjects1) =:= dets:info(TabRef, no_objects)),
    ?line ok = dets:init_table(TabRef, init([BinColl2]),{format,bchunk}),
    ?line true = (AllObjects2 =:= sort(get_all_objects_fast(TabRef))),
    ?line true = (length(AllObjects2) =:= dets:info(TabRef, no_objects)),
    ?line ok = dets:init_table(TabRef, init([BinColl3]),{format,bchunk}),
    ?line true = (AllObjects3 =:= sort(get_all_objects_fast(TabRef))),
    ?line true = (length(AllObjects3) =:= dets:info(TabRef, no_objects)),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),
    ?line check_pps(P0),
    ok.

fast_bulk_init(L, Type, N, NoKeys, Config, V) ->
    fast_bulk_init(L, Type, N, NoKeys, 256, Config, V).

fast_bulk_init(L, Type, N, NoKeys, Est, Config, V) ->
    T = init_table_test,
    Fname = filename(T, Config),
    ?line file:delete(Fname),

    Args0 = [{ram_file,false}, {type,Type},{keypos,1},
	    {estimated_no_objects, Est},{version,V}],
    Args = [{file,Fname} | Args0],
    S = init_table_test_source,
    SFname = filename(S, Config),
    ?line file:delete(SFname),
    SArgs = [{file,SFname} | Args0],

    ?line {ok, S} = dets:open_file(S, SArgs),
    ?line ok = dets:insert(S, L),

    Input = init_bchunk(S),
    ?line {ok, T} = dets:open_file(T, Args),
    ?line ok = dets:init_table(T, Input, [{format,bchunk}]),
    ?line All = sort(get_all_objects(T)),
    ?line FAll = get_all_objects_fast(T),
    ?line true = All =:= sort(FAll),
    ?line true = length(All) =:= N,
    ?line true = dets:info(T, size) =:= N,
    ?line true = dets:info(T, no_keys) =:= NoKeys,
    ?line ok = dets:close(T),
    
    ?line {ok, T} = dets:open_file(T, Args),
    ?line All2 = sort(get_all_objects(T)),
    ?line FAll2 = get_all_objects_fast(T),
    ?line true = All =:= All2,
    ?line true = All =:= sort(FAll2),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    ?line ok = dets:close(S),
    ?line file:delete(SFname),
    ok.

init_bchunk(T) ->
    Start = dets:bchunk(T, start),
    init_bchunk(T, Start).

init_bchunk(Tab, State) ->
    fun(read) when State =:= '$end_of_table' ->
	    end_of_input;
       (read) when element(1, State) =:= error ->
	    State;
       (read) ->
	    {Cont, Objs} = State,
	    {Objs, init_bchunk(Tab, dets:bchunk(Tab, Cont))};
       (close) ->
	    ok
    end.

collect_bchunk(Tab, Fun) ->
    collect_bchunk(Tab, Fun, []).

collect_bchunk(Tab, Fun, L) ->
    case Fun(read) of
	end_of_input ->
	    lists:append(lists:reverse(L));
	{Objs, Fun2} when is_list(Objs) ->
	    collect_bchunk(Tab, Fun2, [Objs | L]);
	Error ->
	    Error
    end.

items(I, N, C, L) when I =:= N; C =:= 0 ->
    {I, L};
items(I, N, C, L) ->
    items(I+1, N, C-1, [{I, item(I)} | L]).

repair_v8(doc) ->
    ["open_file and repair."];
repair_v8(suite) -> 
    [];
repair_v8(Config) when is_list(Config) ->
    repair(Config, 8).

repair_v9(doc) ->
    ["open_file and repair."];
repair_v9(suite) -> 
    [];
repair_v9(Config) when is_list(Config) ->
    %% Convert from format 9 to format 8.
    T = convert_98,
    Fname = filename(T, Config),
    ?line file:delete(Fname),
    ?line {ok, _} = dets:open_file(T, [{file,Fname},{version,9},
				       {type,duplicate_bag}]),
    ?line 9 = dets:info(T, version),
    ?line true = is_binary(dets:info(T, bchunk_format)),
    ?line ok = dets:insert(T, [{1,a},{2,b},{1,c},{2,c},{1,c},{2,a},{1,b}]),
    ?line dets:close(T),
    ?line {error, {version_mismatch, _}} = 
	dets:open_file(T, [{file,Fname},{version,8},{type,duplicate_bag}]),
    ?line {ok, _} = dets:open_file(T, [{file,Fname},{version,8},
				       {type,duplicate_bag},{repair,force}]),
    ?line 8 = dets:info(T, version),
    ?line true = undefined =:= dets:info(T, bchunk_format),
    ?line [{1,a},{1,b},{1,c},{1,c}] = sort(dets:lookup(T, 1)),
    ?line [{2,a},{2,b},{2,c}] = sort(dets:lookup(T, 2)),
    ?line 7 = dets:info(T, no_objects),
    ?line no_keys_test(T),
    ?line _ = histogram(T, silent),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    %% The short lived format 9(a).
    %% Not very throughly tested here.
    A9 = a9,
    ?line Version9aS = filename:join(?datadir(Config), "version_9a.dets"),
    ?line Version9aT = filename('v9a.dets', Config),
    ?line {ok, _} = file:copy(Version9aS, Version9aT),
    ?line {ok, A9} = dets:open_file(A9, [{file,Version9aT}]),
    ?line undefined = dets:info(A9, bchunk_format),
    ?line [{1,a},{2,b},{3,c}] = sort(dets:match_object(A9, '_')),
    ?line ok = dets:insert(A9, {4,d}),
    ?line ok = dets:close(A9),
    ?line {ok, A9} = dets:open_file(A9, [{file,Version9aT}]),
    ?line {error, old_version} = dets:bchunk(A9, start),
    ?line ok = dets:close(A9),
    ?line io:format("Expect forced repair:~n"),
    ?line {ok, A9} = dets:open_file(A9, [{file,Version9aT},{repair,force}]),
    ?line {_, _} = dets:bchunk(A9, start),
    ?line ok = dets:close(A9),
    ?line file:delete(Version9aT),

    repair(Config, 9).

repair(Config, V) ->
    TabRef = repair_test,
    Fname = filename(TabRef, Config),
    ?line file:delete(Fname),
    HeadSize = headsz(V),

    P0 = pps(),
    ?line {'EXIT', {badarg, _}} = 
	(catch dets:open_file(TabRef, [{min_no_slots,1000},
				       {max_no_slots,500}])),
    ?line {error,{file_error,hoppla,enoent}} = dets:file_info(hoppla),
    ?line {error,{file_error,Fname,enoent}} = 
	dets:open_file(TabRef, [{file, Fname}, {access, read}]),

    %% compacting, and some kind of test that free lists are saved OK on file
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line 0 = dets:info(TabRef, size),
    ?line ok = ins(TabRef, 30000),
    ?line ok = del(TabRef, 30000, 3),
    ?line ok = dets:close(TabRef),
    ?line {error, {access_mode,Fname}} =
        dets:open_file(foo, [{file,Fname},{repair,force},{access,read}]),
    ?line {ok, Ref3} = dets:open_file(Fname), % no repair!
    ?line 20000 = dets:info(Ref3, size),
    ?line 20000 = dets:foldl(fun(_, N) -> N+1 end, 0, Ref3),
    ?line 20000 = count_objects_quite_fast(Ref3), % actually a test of match
    ?line no_keys_test(Ref3),
    ?line ok = dets:close(Ref3),
    if
        V =:= 8 ->
            ?line {ok, TabRef} = dets:open_file(TabRef, 
                                   [{file, Fname},{version,V},{access,read}]),
            ?line ok = dets:close(TabRef),
            ?line io:format("Expect compacting repair:~n"),
            ?line {ok, TabRef} = dets:open_file(TabRef, 
                                                [{file, Fname},{version,V}]),
            ?line 20000 = dets:info(TabRef, size),
	    ?line _ = histogram(TabRef, silent),
            ?line ok = dets:close(TabRef);
        true ->
            ok
    end,
    ?line {error,{keypos_mismatch,Fname}} = 
	dets:open_file(TabRef, [{file, Fname},{keypos,17}]),
    ?line {error,{type_mismatch,Fname}} = 
	dets:open_file(TabRef, [{file, Fname},{type,duplicate_bag}]),

    %% make one of the temporary files unwritable
    TmpFile = if 
		  V =:= 8 -> 
		      Fname ++ ".TMP.10000"; 
		  true -> Fname ++ ".TMP.1" 
	      end,
    ?line file:delete(TmpFile),
    ?line {ok, TmpFd} = file:open(TmpFile, [read,write]),
    ?line ok = file:close(TmpFd),
    ?line unwritable(TmpFile),
    ?line {error,{file_error,TmpFile,eacces}} = dets:fsck(Fname, V),
    ?line {ok, _} = dets:open_file(TabRef, 
                        [{repair,false},{file, Fname},{version,V}]),
    ?line 20000 = length(get_all_objects(TabRef)),
    ?line _ = histogram(TabRef, silent),
    ?line 20000 = length(get_all_objects_fast(TabRef)),
    ?line ok = dets:close(TabRef),
    ?line writable(TmpFile),
    ?line file:delete(TmpFile),

    ?line truncate(Fname, HeadSize + 10),
    ?line {error,{not_closed, Fname}} = 
	dets:open_file(TabRef, [{file, Fname}, {access, read}]),
    ?line {error,{not_closed, Fname}} = 
	dets:open_file(TabRef, [{file, Fname}, {access, read}, 
                                {repair,force}]),
    ?line {error,{needs_repair, Fname}} = 
	dets:open_file(TabRef, [{file, Fname}, {repair, false}]),
    ?line file:delete(Fname),

    %% truncated file header
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = ins(TabRef, 100),
    ?line ok = dets:close(TabRef),
    ?line truncate(Fname, HeadSize - 10),    
    %% a new file is created ('tooshort')
    ?line {ok, TabRef} = dets:open_file(TabRef, 
					[{file,Fname},{version,V},
					 {min_no_slots,1000},
					 {max_no_slots,1000000}]),
    case dets:info(TabRef, no_slots) of
	undefined -> ok;
	{Min1,Slot1,Max1} ->
	    ?line true = Min1 =< Slot1, true = Slot1 =< Max1,
	    ?line true = 1000 < Min1, true = 1000+256 > Min1,
	    ?line true = 1000000 < Max1, true = (1 bsl 20)+256 > Max1
    end,
    ?line 0 = dets:info(TabRef, size),
    ?line no_keys_test(TabRef),
    ?line _ = histogram(TabRef, silent),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    %% version bump (v8)
    ?line Version7S = filename:join(?datadir(Config), "version_r2d.dets"),
    ?line Version7T = filename('v2.dets', Config),
    ?line {ok, _} = file:copy(Version7S, Version7T),
    ?line {error,{version_bump, Version7T}} = dets:open_file(Version7T),
    ?line {error,{version_bump, Version7T}} = 
	dets:open_file(Version7T, [{file,Version7T},{repair,false}]),
    ?line {error,{version_bump, Version7T}} = 
	dets:open_file(Version7T, [{file, Version7T}, {access, read}]),
    ?line io:format("Expect upgrade:~n"),
    ?line {ok, _} = dets:open_file(Version7T, 
                                   [{file, Version7T},{version, V}]),
    ?line [{1,a},{2,b}] = sort(get_all_objects(Version7T)),
    ?line [{1,a},{2,b}] = sort(get_all_objects_fast(Version7T)),
    Phash = if 
		V =:= 8 -> phash;
		true -> phash2
	    end,
    ?line Phash = dets:info(Version7T, hash),
    ?line _ = histogram(Version7T, silent),
    ?line ok = dets:close(Version7T),
    ?line {ok, _} = dets:open_file(Version7T, [{file, Version7T}]),
    ?line Phash = dets:info(Version7T, hash),
    ?line ok = dets:close(Version7T),
    ?line file:delete(Version7T),

    %% converting free lists
    ?line Version8aS = filename:join(?datadir(Config), "version_r3b02.dets"),
    ?line Version8aT = filename('v3.dets', Config),
    ?line {ok, _} = file:copy(Version8aS, Version8aT),
    %% min_no_slots and max_no_slots are ignored - no repair is taking place
    ?line {ok, _} = dets:open_file(version_8a, 
				   [{file, Version8aT},{min_no_slots,1000},
				    {max_no_slots,100000}]),
    ?line [{1,b},{2,a},{a,1},{b,2}] = sort(get_all_objects(version_8a)),
    ?line [{1,b},{2,a},{a,1},{b,2}] = sort(get_all_objects_fast(version_8a)),
    ?line ok = ins(version_8a, 1000),
    ?line 1002 = dets:info(version_8a, size),
    ?line no_keys_test(version_8a),
    ?line All8a = sort(get_all_objects(version_8a)),
    ?line 1002 = length(All8a),
    ?line FAll8a = sort(get_all_objects_fast(version_8a)),
    ?line true = sort(All8a) =:= sort(FAll8a),
    ?line ok = del(version_8a, 300, 3),
    ?line 902 = dets:info(version_8a, size),
    ?line no_keys_test(version_8a),
    ?line All8a2 = sort(get_all_objects(version_8a)),
    ?line 902 = length(All8a2),
    ?line FAll8a2 = sort(get_all_objects_fast(version_8a)),
    ?line true = sort(All8a2) =:= sort(FAll8a2),
    ?line _ = histogram(version_8a, silent),
    ?line ok = dets:close(version_8a),
    ?line file:delete(Version8aT),

    %% will fail unless the slots are properly sorted when repairing (v8)
    BArgs = [{file, Fname},{type,duplicate_bag},
	     {delayed_write,{3000,10000}},{version,V}],
    ?line {ok, TabRef} = dets:open_file(TabRef, BArgs),
    Seq = seq(1, 500),
    Small = map(fun(X) -> {X,X} end, Seq),
    Big = map(fun(X) -> erlang:make_tuple(20, X) end, Seq),
    ?line ok = dets:insert(TabRef, Small),
    ?line ok = dets:insert(TabRef, Big),
    ?line ok = dets:insert(TabRef, Small),
    ?line ok = dets:insert(TabRef, Big),
    ?line All = sort(safe_get_all_objects(TabRef)),
    ?line ok = dets:close(TabRef),
    ?line io:format("Expect forced repair:~n"),
    ?line {ok, _} = 
         dets:open_file(TabRef, [{repair,force},{min_no_slots,2000} | BArgs]),
    if 
	V =:= 9 ->
	    ?line {MinNoSlots,_,MaxNoSlots} = dets:info(TabRef, no_slots),
	    ?line ok = dets:close(TabRef),
	    ?line io:format("Expect compaction:~n"),
	    ?line {ok, _} = 
		dets:open_file(TabRef, [{repair,force},
					{min_no_slots,MinNoSlots},
					{max_no_slots,MaxNoSlots} | BArgs]);
	true ->
	    ok
    end,
    ?line All2 = get_all_objects(TabRef),
    ?line true = All =:= sort(All2),
    ?line FAll2 = get_all_objects_fast(TabRef),
    ?line true = All =:= sort(FAll2),
    ?line true = length(All) =:= dets:info(TabRef, size),
    ?line no_keys_test(TabRef),
    Fun = fun(X) -> 4 = length(dets:lookup(TabRef, X)) end,
    ?line foreach(Fun, Seq),
    ?line _ = histogram(TabRef, silent),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    %% object bigger than segments, the "hole" is taken care of
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file, Fname},{version,V}]),
    Tuple = erlang:make_tuple(1000, foobar), % > 2 kB
    ?line ok = dets:insert(TabRef, Tuple),
    %% at least one full segment (objects smaller than 2 kB):
    ?line ins(TabRef, 2000), 
    ?line ok = dets:close(TabRef),

    if 
        V =:= 8 ->
            %% first estimated number of objects is wrong, repair once more
            ?line {ok, Fd} = file:open(Fname, read_write),
            NoPos = HeadSize - 8,  % no_objects
            ?line file:pwrite(Fd, NoPos, <<0:32>>), % NoItems
            ok = file:close(Fd),
            ?line dets:fsck(Fname, V),
            ?line {ok, _} = 
                dets:open_file(TabRef, 
                               [{repair,false},{file, Fname},{version,V}]),
            ?line 2001 = length(get_all_objects(TabRef)),
            ?line _ = histogram(TabRef, silent),
            ?line 2001 = length(get_all_objects_fast(TabRef)),
            ?line ok = dets:close(TabRef);
        true ->
            ok
    end,

    ?line {ok, _} = 
        dets:open_file(TabRef, 
                       [{repair,false},{file, Fname},{version,V}]),
    ?line {ok, ObjPos} = dets:where(TabRef, {66,{item,number,66}}),
    ?line ok = dets:close(TabRef),
    %% Damaged object.
    Pos = 12, % v9: compaction fails, proper repair follows
    crash(Fname, ObjPos+Pos),
    ?line io:format(
	    "Expect forced repair (possibly after attempted compaction):~n"),
    ?line {ok, _} = 
	dets:open_file(TabRef, [{repair,force},{file, Fname},{version,V}]),
    ?line true = dets:info(TabRef, size) < 2001,
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    %% The file is smaller than the padded object.
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = dets:insert(TabRef, Tuple),
    ?line ok = dets:close(TabRef),
    ?line io:format("Expect forced repair or compaction:~n"),
    ?line {ok, _} = 
	dets:open_file(TabRef, [{repair,force},{file, Fname},{version,V}]),
    ?line true = 1 =:= dets:info(TabRef, size),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),

    %% Damaged free lists.
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = ins(TabRef, 300),
    ?line ok = dets:sync(TabRef),
    ?line ok = del(TabRef, 300, 3),
    %% FileSize is approximately where the free lists will be written.
    ?line FileSize = dets:info(TabRef, memory),
    ?line ok = dets:close(TabRef),
    crash(Fname, FileSize+20),
    ?line {error, {bad_freelists, Fname}} = 
	dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line file:delete(Fname),

    %% File not closed, opening with read and read_write access tried.
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = ins(TabRef, 300),
    ?line ok = dets:close(TabRef),
    ?line crash(Fname, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    ?line {error, {not_closed, Fname}} =
       dets:open_file(foo, [{file,Fname},{version,V},{repair,force},
                            {access,read}]),
    ?line {error, {not_closed, Fname}} =
       dets:open_file(foo, [{file,Fname},{version,V},{repair,true},
                            {access,read}]),
    ?line io:format("Expect repair:~n"),
    ?line {ok, TabRef} =
       dets:open_file(TabRef, [{file,Fname},{version,V},{repair,true},
                               {access,read_write}]),
    ?line ok = dets:close(TabRef),
    ?line crash(Fname, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    ?line io:format("Expect forced repair:~n"),
    ?line {ok, TabRef} =
       dets:open_file(TabRef, [{file,Fname},{version,V},{repair,force},
                               {access,read_write}]),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),    

    %% The size of an object is huge.
    ?line {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{version,V}]),
    ?line ok = dets:insert(TabRef, [{1,2,3},{2,3,4}]),
    ?line {ok, ObjPos2} = dets:where(TabRef, {1,2,3}),
    ?line ok = dets:close(TabRef),
    ObjPos3 = if
                  V =:= 8 -> ObjPos2 + 4;
                  V =:= 9 -> ObjPos2
              end,
    crash(Fname, ObjPos3, 255),
    ?line io:format("Expect forced repair:~n"),
    ?line {ok, TabRef} = 
         dets:open_file(TabRef, [{file,Fname},{version,V},{repair,force}]),
    ?line ok = dets:close(TabRef),
    ?line file:delete(Fname),    

    ?line check_pps(P0),
    ok.

hash_v8b_v8c(doc) ->
    ["Test the use of different hashing algorithms in v8b and v8c of the "
     "Dets file format."];
hash_v8b_v8c(suite) ->
    [];
hash_v8b_v8c(Config) when is_list(Config) ->
    ?line Source = 
	filename:join(?datadir(Config), "dets_test_v8b.dets"),
    %% Little endian version of old file (there is an endianess bug in 
    %% the old hash). This is all about version 8 of the dets file format.

    P0 = pps(),
    ?line SourceLE = 
	filename:join(?datadir(Config), 
		      "dets_test_v8b_little_endian.dets"),
    ?line Target1 = filename('oldhash1.dets', Config),
    ?line Target1LE = filename('oldhash1le.dets', Config),
    ?line Target2 = filename('oldhash2.dets', Config),
    ?line {ok, Bin} = file:read_file(Source),
    ?line {ok, BinLE} = file:read_file(SourceLE),
    ?line ok = file:write_file(Target1,Bin),
    ?line ok = file:write_file(Target1LE,BinLE),
    ?line ok = file:write_file(Target2,Bin),
    ?line {ok, d1} = dets:open_file(d1,[{file,Target1}]),
    ?line {ok, d1le} = dets:open_file(d1le,[{file,Target1LE}]),
    ?line {ok, d2} = dets:open_file(d2,[{file,Target2},{repair,force},
                                        {version,8}]),
    ?line FF = fun(N,_F,_T) when N > 16#FFFFFFFFFFFFFFFF -> 
		       ok;
		  (N,F,T) -> 
		       V = integer_to_list(N),
		       case dets:lookup(T,N) of
				 [{N,V}] ->
				     F(N*2,F,T);
				 _Error ->
				     exit({failed,{lookup,T,N}})
			     end
	       end,
    ?line Mess = case (catch FF(1,FF,d1)) of
		     {'EXIT', {failed, {lookup,_,_}}} ->
			 ?line ok = dets:close(d1),
			 ?line FF(1,FF,d1le),
			 ?line hash = dets:info(d1le,hash),
			 ?line dets:insert(d1le,{33333333333,hejsan}),
			 ?line [{33333333333,hejsan}] = 
			     dets:lookup(d1le,33333333333),
			 ?line ok = dets:close(d1le),
			 ?line {ok, d1le} = dets:open_file(d1le,
							   [{file,Target1LE}]),
			 ?line [{33333333333,hejsan}] = 
			     dets:lookup(d1le,33333333333),
			 ?line FF(1,FF,d1le),
			 ?line ok = dets:close(d1le),
			 "Seems to be a little endian machine";
		     {'EXIT', Fault} ->
			 exit(Fault);
		     _ ->
			 ?line ok = dets:close(d1le),
			 ?line hash = dets:info(d1,hash),
			 ?line dets:insert(d1,{33333333333,hejsan}),
			 ?line [{33333333333,hejsan}] = 
			     dets:lookup(d1,33333333333),
			 ?line ok = dets:close(d1),
			 ?line {ok, d1} = dets:open_file(d1,[{file,Target1}]),
			 ?line [{33333333333,hejsan}] = 
			     dets:lookup(d1,33333333333),
			 ?line FF(1,FF,d1),
			 ?line ok = dets:close(d1),
			 "Seems to be a big endian machine"
		 end,
    ?line FF(1,FF,d2),
    ?line phash = dets:info(d2,hash),
    ?line ok = dets:close(d2),
    ?line file:delete(Target1),
    ?line file:delete(Target1LE),
    ?line file:delete(Target2),
    ?line check_pps(P0),
    {comment, Mess}.

phash(doc) ->
    ["Test version 9(b) with erlang:phash/2 as hash function."];
phash(suite) ->
    [];
phash(Config) when is_list(Config) ->
    T = phash,
    Phash_v9bS = filename:join(?datadir(Config), "version_9b_phash.dat"),
    Fname = filename('v9b.dets', Config),
    ?line {ok, _} = file:copy(Phash_v9bS, Fname),
    
    %% Deleting all objects changes the hash function. 
    %% A feature... (it's for free)
    ?line {ok, T} = dets:open_file(T, [{file, Fname}]),
    ?line phash = dets:info(T, hash),
    ?line dets:delete_all_objects(T),
    ?line phash2 = dets:info(T, hash),
    ?line [] = get_all_objects(T),
    ?line [] = get_all_objects_fast(T),
    ?line ok = dets:close(T),

    %% The hash function is kept when compacting a table.
    ?line {ok, _} = file:copy(Phash_v9bS, Fname),
    ?line io:format("Expect compaction:~n"),
    ?line {ok, T} = dets:open_file(T, [{file, Fname},{repair,force}]),
    ?line phash = dets:info(T, hash),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e}] =
	lists:sort(dets:lookup_keys(T, [1,2,3,4,5])),
    ?line ok = dets:close(T),

    %% The hash function is updated when repairing a table (no cost).
    ?line {ok, _} = file:copy(Phash_v9bS, Fname),
    crash(Fname, ?CLOSED_PROPERLY_POS+3, 0),
    ?line io:format("Expect repair:~n"),
    ?line {ok, T} = dets:open_file(T, [{file, Fname}]),
    ?line phash2 = dets:info(T, hash),
    ?line [{1,a},{2,b},{3,c},{4,d},{5,e}] =
	lists:sort(dets:lookup_keys(T, [1,2,3,4,5])),
    ?line ok = dets:close(T),
    
    %% One cannot use the bchunk format when copying between a phash
    %% table and a phash2 table. (There is no test for the case an R9
    %% (or later) node (using phash2) copies a table to an R8 node
    %% (using phash).) See also the comment on HASH_PARMS in dets_v9.erl.
    ?line {ok, _} = file:copy(Phash_v9bS, Fname),
    ?line {ok, T} = dets:open_file(T, [{file, Fname}]),
    ?line Type = dets:info(T, type),
    ?line KeyPos = dets:info(T, keypos),
    Input = init_bchunk(T),    
    T2 = phash_table,
    Fname2 = filename(T2, Config),
    Args = [{type,Type},{keypos,KeyPos},{version,9},{file,Fname2}],
    ?line {ok, T2} = dets:open_file(T2, Args),
    ?line {error, {init_fun, _}} = 
	dets:init_table(T2, Input, {format,bchunk}),
    ?line _ = dets:close(T2),
    ?line ok = dets:close(T),
    ?line file:delete(Fname2),

    ?line file:delete(Fname),
    ok.

fold_v8(doc) ->
    ["foldl, foldr, to_ets"];
fold_v8(suite) ->
    [];
fold_v8(Config) when is_list(Config) ->
    fold(Config, 8).

fold_v9(doc) ->
    ["foldl, foldr, to_ets"];
fold_v9(suite) ->
    [];
fold_v9(Config) when is_list(Config) ->
    fold(Config, 9).

fold(Config, Version) ->
    T = test_table,
    N = 100,
    ?line Fname = filename(T, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    Args = [{version, Version}, {file,Fname}, {estimated_no_objects, N}],
    ?line {ok, _} = dets:open_file(T, Args),

    ?line ok = ins(T, N),

    ?line Ets = ets:new(to_ets, [public]),
    ?line dets:to_ets(T, Ets),
    ?line true = N =:= ets:info(Ets, size),
    ?line ets:delete(Ets),

    ?line Ets2 = ets:new(to_ets, [private]),
    ?line dets:to_ets(T, Ets2),
    ?line true = N =:= ets:info(Ets2, size),
    ?line ets:delete(Ets2),

    ?line {'EXIT', {badarg, _}} = (catch dets:to_ets(T, not_an_ets_table)),

    F0 = fun(X, A) -> [X | A] end,
    ?line true = N =:= length(dets:foldl(F0, [], T)),
    ?line true = N =:= length(dets:foldr(F0, [], T)),

    F1 = fun(_X, _A) -> throw(away) end, 
    ?line away = (catch dets:foldl(F1, [], T)),
    ?line away = (catch dets:foldr(F1, [], T)),

    F2 = fun(X, A) -> X + A end, 
    ?line {'EXIT', _} = (catch dets:foldl(F2, [], T)),
    ?line {'EXIT', _} = (catch dets:foldr(F2, [], T)),

    F3 = fun(_X) -> throw(away) end,
    ?line away = (catch dets:traverse(T, F3)),

    F4 = fun(X) -> X + 17 end,
    ?line {'EXIT', _} = (catch dets:traverse(T, F4)),

    ?line F5 = fun(_X) -> done end,
    ?line done = dets:traverse(T, F5),

    ?line {ok, ObjPos} = dets:where(T, {66,{item,number,66}}),
    ?line ok = dets:close(T),

    %% Damaged object.
    Pos = if 
	      Version =:= 8 -> 12;
	      Version =:= 9 -> 8
	  end,
    crash(Fname, ObjPos+Pos),
    ?line {ok, _} = dets:open_file(T, Args),
    ?line io:format("Expect corrupt table:~n"),
    ?line BadObject1 = dets:foldl(F0, [], T),
    ?line bad_object(BadObject1, Fname),
    ?line BadObject2 = dets:close(T), 
    ?line bad_object(BadObject2, Fname),

    ?line file:delete(Fname),
    ?line check_pps(P0),
    ok.

fixtable_v8(doc) ->
    ["Add objects to a fixed table."];
fixtable_v8(suite) ->
    [];
fixtable_v8(Config) when is_list(Config) ->
    fixtable(Config, 8).

fixtable_v9(doc) ->
    ["Add objects to a fixed table."];
fixtable_v9(suite) ->
    [];
fixtable_v9(Config) when is_list(Config) ->
    fixtable(Config, 9).

fixtable(Config, Version) when is_list(Config) ->
    T = fixtable,
    ?line Fname = filename(fixtable, Config),
    ?line file:delete(Fname),
    Args = [{version,Version},{file,Fname}],
    P0 = pps(),
    ?line {ok, _} = dets:open_file(T, Args),

    %% badarg
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} =
	(catch dets:safe_fixtable(no_table,true)),
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[T,undefined]}|_]}} =
	(catch dets:safe_fixtable(T,undefined)),

    %% The table is not allowed to grow while the elements are inserted:

    ?line ok = ins(T, 500),
    ?line dets:safe_fixtable(T, false),    
    %% Now the table can grow. At the same time as elements are inserted,
    %% the table tries to catch up with the previously inserted elements.
    ?line ok = ins(T, 1000),
    ?line 1000 = dets:info(T, size),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(T, [{type, duplicate_bag} | Args]),
    %% In a fixed table, delete and re-insert an object.
    ?line ok = dets:insert(T, {1, a, b}),
    ?line dets:safe_fixtable(T, true),
    ?line ok = dets:match_delete(T, {1, a, b}),
    ?line ok = dets:insert(T, {1, a, b}),
    ?line dets:safe_fixtable(T, false),
    ?line 1 = length(dets:match_object(T, '_')),

    ?line ok = dets:match_delete(T, '_'),
    %% In a fixed table, delete and insert a smaller object.
    ?line ok = dets:insert(T, {1, duplicate(100, e)}),
    ?line dets:safe_fixtable(T, true),
    ?line ok = dets:match_delete(T, {1, '_'}),
    ?line ok = dets:insert(T, {1, a, b}),
    ?line dets:safe_fixtable(T, false),
    ?line 1 = length(dets:match_object(T, '_')),

    ?line ok = dets:delete_all_objects(T),
    %% Like the last one, but one extra object.
    ?line ok = dets:insert(T, {1, duplicate(100, e)}),
    ?line ok = dets:insert(T, {2, duplicate(100, e)}),
    ?line dets:safe_fixtable(T, true),
    ?line ok = dets:match_delete(T, {1, '_'}),
    ?line ok = dets:insert(T, {1, a, b}),
    ?line dets:safe_fixtable(T, false),
    ?line 2 = length(dets:match_object(T, '_')),
    ?line dets:safe_fixtable(T, true),
    ?line ok = dets:delete_all_objects(T),
    ?line true = dets:info(T, fixed),
    ?line 0 = length(dets:match_object(T, '_')),

    ?line ok = dets:close(T),
    ?line file:delete(Fname),    
    ?line check_pps(P0),
    ok.

match_v8(doc) ->
    ["Matching objects of a fixed table."];
match_v8(suite) ->
    [];
match_v8(Config) when is_list(Config) ->
    match(Config, 8).

match_v9(doc) ->
    ["Matching objects of a fixed table."];
match_v9(suite) ->
    [];
match_v9(Config) when is_list(Config) ->
    match(Config, 9).

match(Config, Version) ->
    T = match,
    ?line Fname = filename(match, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    Args = [{version, Version}, {file,Fname}, {type, duplicate_bag},
	    {estimated_no_objects,550}],
    ?line {ok, _} = dets:open_file(T, Args),
    ?line ok = dets:insert(T, {1, a, b}),
    ?line ok = dets:insert(T, {1, b, a}),
    ?line ok = dets:insert(T, {2, a, b}),
    ?line ok = dets:insert(T, {2, b, a}),

    %% match, badarg
    MSpec = [{'_',[],['$_']}],
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:match(no_table, '_')),
    ?line {'EXIT', {badarg, [{dets,match,[T,'_',not_a_number]}|_]}} = 
	(catch dets:match(T, '_', not_a_number)),
    ?line {EC1, _} = dets:select(T, MSpec, 1),
    ?line {'EXIT', {badarg, [{dets,match,[EC1]}|_]}} = 
	(catch dets:match(EC1)),

    %% match_object, badarg
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:match_object(no_table, '_')),
    ?line {'EXIT', {badarg, [{dets,match_object,[T,'_',not_a_number]}|_]}} = 
	(catch dets:match_object(T, '_', not_a_number)),
    ?line {EC2, _} = dets:select(T, MSpec, 1),
    ?line {'EXIT', {badarg, [{dets,match_object,[EC2]}|_]}} = 
	(catch dets:match_object(EC2)),

    dets:safe_fixtable(T, true),
    ?line {[_, _], C1} = dets:match_object(T, '_', 2),
    ?line {[_, _], C2} = dets:match_object(C1),
    ?line '$end_of_table' = dets:match_object(C2),
    ?line {[_, _], C3} = dets:match_object(T, {1, '_', '_'}, 100),
    ?line '$end_of_table' = dets:match_object(C3),
    ?line '$end_of_table' = dets:match_object(T, {'_'}, default),
    ?line dets:safe_fixtable(T, false),

    ?line dets:safe_fixtable(T, true),
    ?line {[_, _], C30} = dets:match(T, '$1', 2),
    ?line {[_, _], C31} = dets:match(C30),
    ?line '$end_of_table' = dets:match(C31),
    ?line {[_, _], C32} = dets:match(T, {1, '$1', '_'}, 100),
    ?line '$end_of_table' = dets:match(C32),
    ?line '$end_of_table' = dets:match(T, {'_'}, default),
    ?line dets:safe_fixtable(T, false),
    ?line [[1],[1],[2],[2]] = sort(dets:match(T, {'$1','_','_'})),

    %% delete and insert while chunking
    %% (this case almost worthless after changes in OTP-5232)
    ?line ok = dets:match_delete(T, '_'),
    L500 = seq(1, 500),
    Fun = fun(X) -> ok = dets:insert(T, {X, a, b, c, d}) end,
    ?line foreach(Fun, L500),
    %% Select one object DI in L3 below to be deleted.
    ?line {_, TmpCont} = dets:match_object(T, '_', 200),    
    ?line {_, TmpCont1} = dets:match_object(TmpCont),
    ?line {TTL, _} = dets:match_object(TmpCont1),
    ?line DI = if Version =:= 8 -> last(TTL); Version =:= 9 -> hd(TTL) end,
    ?line dets:safe_fixtable(T, true),
    ?line {L1, C20} = dets:match_object(T, '_', 200),    
    ?line true = 200 =< length(L1),
    ?line ok = dets:match_delete(T, {'2','_','_'}), % no match
    ?line ok = dets:match_delete(T, DI), % last object
    Tiny = {1050},
    ?line ok = dets:insert(T, Tiny),
    ?line true = member(Tiny, dets:match_object(T, '_')),
    ?line {_L2, C21} = dets:match_object(C20),
    ?line {_L3, _C22} = dets:match_object(C21),
    %% It used to be that Tiny was not visible here, but since the 
    %% scanning of files was changed to inspect the free lists every
    %% now and then it may very well be visible here.
    %% ?line false = member(Tiny, _L3),
    %% DI used to visible here, but the above mentioned modification
    %% has changed that; it may or may not be visible.
    %% ?line true = member(DI, _L3),
    ?line dets:safe_fixtable(T, false),
    ?line true = dets:member(T, 1050),
    ?line true = member(Tiny, dets:match_object(T, '_')),
    ?line false = member(DI, dets:match_object(T, '_')),

    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    N = 100,
    ?line {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ?line ok = ins(T, N),
    Obj = {66,{item,number,66}},
    Spec = {'_','_'},
    ?line {ok, ObjPos} = dets:where(T, Obj),
    ?line ok = dets:close(T),
    %% Damaged object.
    crash(Fname, ObjPos+12),
    ?line {ok, _} = dets:open_file(T, Args),
    ?line io:format("Expect corrupt table:~n"),
    ?line case ins(T, N) of
	      ok ->
                  ?line bad_object(dets:sync(T), Fname);
	      Else ->
		  ?line bad_object(Else, Fname)
	  end,
    ?line io:format("Expect corrupt table:~n"),
    ?line bad_object(dets:match(T, Spec), Fname),
    ?line io:format("Expect corrupt table:~n"),
    ?line bad_object(dets:match_delete(T, Spec), Fname),
    ?line bad_object(dets:close(T), Fname),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ?line ok = ins(T, N),
    ?line {ok, ObjPos2} = dets:where(T, Obj),
    ?line ok = dets:close(T),

    %% Damaged size of object.
    %% In v8, there is a next pointer before the size.
    CrashPos = if Version =:= 8 -> 5; Version =:= 9 -> 1 end,
    crash(Fname, ObjPos2+CrashPos),
    ?line {ok, _} = dets:open_file(T, Args),
    ?line io:format("Expect corrupt table:~n"),
    ?line case ins(T, N) of
	      ok ->
		  ?line bad_object(dets:sync(T), Fname);
	      Else2 ->
		  ?line bad_object(Else2, Fname)
	  end,
    %% Just echoes...
    ?line bad_object(dets:match(T, Spec), Fname),
    ?line bad_object(dets:match_delete(T, Spec), Fname),
    ?line bad_object(dets:close(T), Fname),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ?line ok = ins(T, N),
    ?line {ok, ObjPos3} = dets:where(T, Obj),
    ?line ok = dets:close(T),

    %% match_delete finds an error
    CrashPos3 = if Version =:= 8 -> 12; Version =:= 9 -> 16 end,
    crash(Fname, ObjPos3+CrashPos3),
    ?line {ok, _} = dets:open_file(T, Args),
    ?line bad_object(dets:match_delete(T, Spec), Fname),
    ?line bad_object(dets:close(T), Fname),
    ?line file:delete(Fname),

    %% The key is not fixed, but not all objects with the key are removed.
    ?line {ok, _} = dets:open_file(T, Args),
    ?line ok = dets:insert(T, [{1,a},{1,b},{1,c},{1,a},{1,b},{1,c}]),
    ?line 6 = dets:info(T, size),
    ?line ok = dets:match_delete(T, {'_',a}),
    ?line 4 = dets:info(T, size),
    ?line [{1,b},{1,b},{1,c},{1,c}] = 
	sort(dets:match_object(T,{'_','_'})),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    ?line check_pps(P0),
    ok.

select_v8(doc) ->
    ["Selecting objects of a fixed table."];
select_v8(suite) ->
    [];
select_v8(Config) when is_list(Config) ->
    select(Config, 8).

select_v9(doc) ->
    ["Selecting objects of a fixed table."];
select_v9(suite) ->
    [];
select_v9(Config) when is_list(Config) ->
    select(Config, 9).

select(Config, Version) ->
    T = select,
    ?line Fname = filename(select, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    ?line Args = [{version,Version}, {file,Fname}, {type, duplicate_bag},
		  {estimated_no_objects,550}],
    ?line {ok, _} = dets:open_file(T, Args),
    ?line ok = dets:insert(T, {1, a, b}),
    ?line ok = dets:insert(T, {1, b, a}),
    ?line ok = dets:insert(T, {2, a, b}),
    ?line ok = dets:insert(T, {2, b, a}),
    ?line ok = dets:insert(T, {3, a, b}),
    ?line ok = dets:insert(T, {3, b, a}),

    %% badarg
    MSpec = [{'_',[],['$_']}],
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:select(no_table, MSpec)),
    ?line {'EXIT', {badarg, [{dets,select,[T,<<17>>]}|_]}} = 
	(catch dets:select(T, <<17>>)),
    ?line {'EXIT', {badarg, [{dets,select,[T,[]]}|_]}} = 
	(catch dets:select(T, [])),
    ?line {'EXIT', {badarg, [{dets,select,[T,MSpec,not_a_number]}|_]}} = 
	(catch dets:select(T, MSpec, not_a_number)),
    ?line {EC, _} = dets:match(T, '_', 1),
    ?line {'EXIT', {badarg, [{dets,select,[EC]}|_]}} = 
	(catch dets:select(EC)),

    AllSpec = [{'_',[],['$_']}],

    ?line dets:safe_fixtable(T, true),
    ?line {[_, _], C1} = dets:select(T, AllSpec, 2),
    ?line {[_, _], C2} = dets:select(C1),
    ?line {[_, _], C2a} = dets:select(C2),
    ?line '$end_of_table' = dets:select(C2a),
    ?line {[_, _], C3} = dets:select(T, [{{1,'_','_'},[],['$_']}], 100),
    ?line '$end_of_table' = dets:select(C3),
    ?line '$end_of_table' = dets:select(T, [{{'_'},[],['$_']}], default),
    ?line dets:safe_fixtable(T, false),
    Sp1 = [{{1,'_','_'},[],['$_']},{{1,'_','_'},[],['$_']},
	   {{2,'_','_'},[],['$_']}],
    ?line [_,_,_,_] = dets:select(T, Sp1),
    Sp2 = [{{1,'_','_'},[],['$_']},{{1,'_','_'},[],['$_']},
	   {{'_','_','_'},[],['$_']}],
    ?line [_,_,_,_,_,_] = dets:select(T, Sp2),

    AllDeleteSpec = [{'_',[],[true]}],
    %% delete and insert while chunking
    %% (this case almost worthless after changes in OTP-5232)
    ?line 6 = dets:select_delete(T, AllDeleteSpec),
    L500 = seq(1, 500),
    Fun = fun(X) -> ok = dets:insert(T, {X, a, b, c, d}) end,
    ?line foreach(Fun, L500),
    %% Select one object DI in L3 below to be deleted.
    ?line {_, TmpCont} = dets:match_object(T, '_', 200),    
    ?line {_, TmpCont1} = dets:match_object(TmpCont),
    ?line {TTL, _} = dets:match_object(TmpCont1),
    ?line DI = if Version =:= 8 -> last(TTL); Version =:= 9 -> hd(TTL) end,
    ?line dets:safe_fixtable(T, true),
    ?line {L1, C20} = dets:select(T, AllSpec, 200),
    ?line true = 200 =< length(L1),
    ?line 0 = dets:select_delete(T, [{{2,'_','_'},[],[true]}]),
    ?line 1 = dets:select_delete(T, [{DI,[],[true]}]), % last object
    Tiny = {1050},
    ?line ok = dets:insert(T, Tiny),
    ?line true = member(Tiny, dets:select(T, AllSpec)),
    ?line {_L2, C21} = dets:select(C20),
    ?line {_L3, _C22} = dets:select(C21),
    %% It used to be that Tiny was not visible here, but since the 
    %% scanning of files was changed to inspect the free lists every
    %% now and then it may very well be visible here.
    %% ?line false = member(Tiny, _L3),
    %% DI used to visible here, but the above mentioned modification
    %% has changed that; it may or may not be visible.
    %% ?line true = member(DI, _L3),
    ?line true = dets:member(T, 1050),
    ?line true = member(Tiny, dets:select(T, AllSpec)),
    ?line false = member(DI, dets:select(T, AllSpec)),
    ?line dets:safe_fixtable(T, false),
    ?line true = dets:member(T, 1050),
    ?line true = member(Tiny, dets:select(T, AllSpec)),
    ?line false = member(DI, dets:select(T, AllSpec)),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    %% The key is not fixed, but not all objects with the key are removed.
    ?line {ok, _} = dets:open_file(T, Args),
    ?line ok = dets:insert(T, [{1,a},{1,b},{1,c},{1,a},{1,b},{1,c}]),
    ?line 6 = dets:info(T, size),
    ?line 2 = dets:select_delete(T, [{{'_',a},[],[true]}]),
    ?line 4 = dets:info(T, size),
    ?line [{1,b},{1,b},{1,c},{1,c}] = sort(dets:select(T, AllSpec)),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    ?line check_pps(P0),
    ok.

update_counter(doc) ->
    ["Test update_counter/1."];
update_counter(suite) ->
    [];
update_counter(Config) when is_list(Config) ->
    T = update_counter,
    ?line Fname = filename(select, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    ?line {'EXIT', {badarg, [{dets,update_counter,[no_table,1,1]}|_]}} = 
	(catch dets:update_counter(no_table, 1, 1)),

    Args = [{file,Fname},{keypos,2}],
    ?line {ok, _} = dets:open_file(T, [{type,set} | Args]),
    ?line {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ?line ok = dets:insert(T, {1,a}),
    ?line {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ?line ok = dets:insert(T, {0,1}),
    ?line {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ?line ok = dets:insert(T, {0,1,0}),
    ?line 1 = dets:update_counter(T, 1, 1),
    ?line 2 = dets:update_counter(T, 1, 1),
    ?line 6 = dets:update_counter(T, 1, {3,4}),
    ?line {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, {0,3})),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),

    ?line {ok, _} = dets:open_file(T, [{type,bag} | Args]),
    ?line ok = dets:insert(T, {0,1,0}),
    ?line {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ?line ok = dets:close(T),
    ?line file:delete(Fname),
    ?line check_pps(P0),

    ok.

badarg(doc) ->
    ["Call some functions with bad arguments."];
badarg(suite) ->
    [];
badarg(Config) when is_list(Config) ->
    T = badarg,
    ?line Fname = filename(select, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{keypos,3}],
    ?line {ok, _} = dets:open_file(T, [{type,set} | Args]),
    % ?line dets:verbose(),

    %% badargs are tested in match, select and fixtable too.

    %% open
    ?line {'EXIT', {badarg, [{dets,open_file,[{a,tuple},[]]}|_]}} = 
	(catch dets:open_file({a,tuple},[])),
    ?line {'EXIT', {badarg, [{dets,open_file,[{a,tuple}]}|_]}} = 
	(catch dets:open_file({a,tuple})),
    ?line {'EXIT', {badarg, [{dets,open_file,[file,[foo]]}|_]}} = 
	(catch dets:open_file(file,[foo])),
    ?line {'EXIT', {badarg,[{dets,open_file,[{hej,san},[{type,set}|3]]}|_]}} =
	(catch dets:open_file({hej,san},[{type,set}|3])),

    %% insert
    ?line {'EXIT', {badarg, [{dets,insert,[no_table,{1,2}]}|_]}} = 
	(catch dets:insert(no_table, {1,2})),
    ?line {'EXIT', {badarg, [{dets,insert,[no_table,[{1,2}]]}|_]}} = 
	(catch dets:insert(no_table, [{1,2}])),
    ?line {'EXIT', {badarg, [{dets,insert,[T,{1,2}]}|_]}} = 
	(catch dets:insert(T, {1,2})),
    ?line {'EXIT', {badarg, [{dets,insert,[T,[{1,2}]]}|_]}} = 
	(catch dets:insert(T, [{1,2}])),
    ?line {'EXIT', {badarg, [{dets,insert,[T,[{1,2,3}|3]]}|_]}} = 
	      (catch dets:insert(T, [{1,2,3} | 3])),

    %% lookup{_keys}
    ?line {'EXIT', {badarg, [{dets,lookup_keys,[badarg,[]]}|_]}} = 
	(catch dets:lookup_keys(T, [])),
    ?line {'EXIT', {badarg, [{dets,lookup,[no_table,1]}|_]}} = 
	(catch dets:lookup(no_table, 1)),
    ?line {'EXIT', {badarg, [{dets,lookup_keys,[T,[1|2]]}|_]}} = 
	(catch dets:lookup_keys(T, [1 | 2])),

    %% member
    ?line {'EXIT', {badarg, [{dets,member,[no_table,1]}|_]}} = 
	(catch dets:member(no_table, 1)),

    %% sync
    ?line {'EXIT', {badarg, [{dets,sync,[no_table]}|_]}} = 
	(catch dets:sync(no_table)),

    %% delete{_keys}
    ?line {'EXIT', {badarg, [{dets,delete,[no_table,1]}|_]}} = 
	(catch dets:delete(no_table, 1)),

    %% delete_object
    ?line {'EXIT', {badarg, [{dets,delete_object,[no_table,{1,2,3}]}|_]}} = 
	(catch dets:delete_object(no_table, {1,2,3})),
    ?line {'EXIT', {badarg, [{dets,delete_object,[T,{1,2}]}|_]}} = 
	(catch dets:delete_object(T, {1,2})),
    ?line {'EXIT', {badarg, [{dets,delete_object,[no_table,[{1,2,3}]]}|_]}} = 
	(catch dets:delete_object(no_table, [{1,2,3}])),
    ?line {'EXIT', {badarg, [{dets,delete_object,[T,[{1,2}]]}|_]}} = 
	(catch dets:delete_object(T, [{1,2}])),
    ?line {'EXIT', {badarg, [{dets,delete_object,[T,[{1,2,3}|3]]}|_]}} = 
	(catch dets:delete_object(T, [{1,2,3} | 3])),

    %% first,next,slot
    ?line {'EXIT', {badarg, [{dets,first,[no_table]}|_]}} =
	(catch dets:first(no_table)),
    ?line {'EXIT', {badarg, [{dets,next,[no_table,1]}|_]}} =
	(catch dets:next(no_table, 1)),
    ?line {'EXIT', {badarg, [{dets,slot,[no_table,0]}|_]}} =
	(catch dets:slot(no_table, 0)),

    %% info
    ?line undefined = dets:info(no_table),
    ?line undefined = dets:info(no_table, foo),
    ?line undefined = dets:info(T, foo),

    %% match_delete
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:match_delete(no_table, '_')),

    %% delete_all_objects
    ?line {'EXIT', {badarg, [{dets,delete_all_objects,[no_table]}|_]}} = 
	(catch dets:delete_all_objects(no_table)),

    %% select_delete
    MSpec = [{'_',[],['$_']}],
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:select_delete(no_table, MSpec)),
    ?line {'EXIT', {badarg, [{dets,select_delete,[T, <<17>>]}|_]}} = 
	(catch dets:select_delete(T, <<17>>)),

    %% traverse, fold
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:traverse(no_table, fun(_) -> continue end)),
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:foldl(fun(_, A) -> A end, [], no_table)),
    ?line {'EXIT', {badarg, [{dets,safe_fixtable,[no_table,true]}|_]}} = 
	(catch dets:foldr(fun(_, A) -> A end, [], no_table)),

    %% close
    ?line ok = dets:close(T),
    ?line {error, not_owner} = dets:close(T),
    ?line {error, not_owner} = dets:close(T),

    %% init_table
    ?line {'EXIT', {badarg,[{dets,init_table,[no_table,_,[]]}|_]}} =
	(catch dets:init_table(no_table, fun(X) -> X end)),
    ?line {'EXIT', {badarg,[{dets,init_table,[no_table,_,[]]}|_]}} =
	(catch dets:init_table(no_table, fun(X) -> X end, [])),

    %% from_ets
    Ets = ets:new(ets,[]),
    ?line {'EXIT', {badarg,[{dets,from_ets,[no_table,_]}|_]}} =
	(catch dets:from_ets(no_table, Ets)),
    ets:delete(Ets),

    ?line {ok, T} = dets:open_file(T, Args),
    ?line {error,incompatible_arguments} = 
	dets:open_file(T, [{type,bag} | Args]),
    ?line ok = dets:close(T),

    file:delete(Fname),
    ?line check_pps(P0),
    ok.

cache_sets_v8(doc) ->
    ["Test the write cache for sets."];
cache_sets_v8(suite) ->
    [];
cache_sets_v8(Config) when is_list(Config) ->
    cache_sets(Config, 8).

cache_sets_v9(doc) ->
    ["Test the write cache for sets."];
cache_sets_v9(suite) ->
    [];
cache_sets_v9(Config) when is_list(Config) ->
    cache_sets(Config, 9).

cache_sets(Config, Version) ->
    Small = 2,
    cache_sets(Config, {0,0}, false, Small, Version),
    cache_sets(Config, {0,0}, true, Small, Version),
    cache_sets(Config, {5000,5000}, false, Small, Version),
    cache_sets(Config, {5000,5000}, true, Small, Version),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_sets(Config, {0,0}, false, Big, Version),
    cache_sets(Config, {0,0}, true, Big, Version),
    cache_sets(Config, {5000,5000}, false, Big, Version),
    cache_sets(Config, {5000,5000}, true, Big, Version),
    ok.
    
cache_sets(Config, DelayedWrite, Extra, Sz, Version) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    ?line Fname = filename(cache, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    ?line {ok, _} = 
	dets:open_file(T,[{version, Version}, {file,Fname}, {type,set}, 
			  {delayed_write, DelayedWrite}]),

    Dups = 1,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		?line dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),    

    %% The values of keys in the same slot as Key are checked.
    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ?line ok = dets:insert(T, Tuple),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    ?line [Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    ?line true = dets:member(T, Key),

    %% If delay, this happens without file access.
    ?line ok = dets:delete(T,Key),
    ?line ok = dets:insert(T,Tuple),
    ?line ok = dets:insert(T,Tuple),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:sync(T),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),

    %% Key's objects are is on file only, 
    %% key 'toto' in the cache (if there is one).
    ?line ok = dets:delete(T,toto),
    ?line ok = dets:insert(T,[{toto,b},{toto,b}]),
    ?line true = sort([Tuple,{toto,b}]) =:= 
                 sort(dets:lookup_keys(T, [Key,toto])),
    ?line true = dets:member(T, toto),

    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),
    ?line false = dets:member(T, Key),
    ?line Size = dets:info(T, size),

    %% No object with the key on the file.
    %% Delete, add one object.
    Size1 = Size + 2,
    del_and_ins(key, T, Size1, Tuple, Key, 1),
    del_and_ins(object, T, Size1, Tuple, Key, 1),
    del_and_ins(both, T, Size1, Tuple, Key, 1),

    %% One object with the key on the file.
    %% Delete it, add one object.
    Size2 = Size + 2,
    del_and_ins(key, T, Size2, Tuple, Key, 1),
    del_and_ins(object, T, Size2, Tuple, Key, 1),
    del_and_ins(both, T, Size2, Tuple, Key, 1),

    %% Overwrite an old objekt with exactly the same size.
    Element = case element(2, Tuple) of
		  255 -> 254;
		  E -> E + 1
	      end,
    Tuple2 = setelement(2, Tuple, Element),
    ?line ok = dets:sync(T),
    ?line ok = dets:insert(T, Tuple2),
    ?line [Tuple2] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:sync(T),    
    ?line [Tuple2] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),

    ?line ok = dets:insert(T, {3,a}),
    ?line ok = dets:insert(T, {3,b}),
    ?line ok = dets:delete_object(T, {3,c}),
    ?line ok = dets:delete_object(T, {3,d}),
    ?line [{3,b}] = dets:lookup(T, 3),

    ?line ok = dets:delete(T, 3),
    ?line ok = dets:delete_object(T, {3,c}),
    ?line ok = dets:delete_object(T, {3,d}),
    ?line [] = dets:lookup(T, 3),

    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow a while, if it needs to.
	    ?line All1 = get_all_objects(T),
	    ?line dets:safe_fixtable(T, false),
	    ?line timer:sleep(1000),
	    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
	    ?line dets:safe_fixtable(T, true),
	    ?line All2 = get_all_objects(T),
	    ?line FAll2 = get_all_objects_fast(T),
	    ?line true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    test_server:fail({sets,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ?line ok = dets:close(T),

    file:delete(Fname),
    ?line check_pps(P0),
    ok.
    
cache_bags_v8(doc) ->
    ["Test the write cache for bags."];
cache_bags_v8(suite) ->
    [];
cache_bags_v8(Config) when is_list(Config) ->
    cache_bags(Config, 8).

cache_bags_v9(doc) ->
    ["Test the write cache for bags."];
cache_bags_v9(suite) ->
    [];
cache_bags_v9(Config) when is_list(Config) ->
    cache_bags(Config, 9).

cache_bags(Config, Version) ->
    Small = 2,
    cache_bags(Config, {0,0}, false, Small, Version),
    cache_bags(Config, {0,0}, true, Small, Version),
    cache_bags(Config, {5000,5000}, false, Small, Version),
    cache_bags(Config, {5000,5000}, true, Small, Version),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_bags(Config, {0,0}, false, Big, Version),
    cache_bags(Config, {0,0}, true, Big, Version),
    cache_bags(Config, {5000,5000}, false, Big, Version),
    cache_bags(Config, {5000,5000}, true, Big, Version),
    ok.
    
cache_bags(Config, DelayedWrite, Extra, Sz, Version) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    ?line Fname = filename(cache, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    ?line {ok, _} = 
	dets:open_file(T,[{version, Version}, {file,Fname}, {type,bag}, 
			  {delayed_write, DelayedWrite}]),

    Dups = 1,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		?line dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),    

    %% The values of keys in the same slot as Key are checked.
    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ?line ok = dets:insert(T, Tuple),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    ?line [Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    ?line true = dets:member(T, Key),

    %% If delay, this happens without file access. 
    %% (This is no longer true; cache lookup has been simplified.)
    ?line ok = dets:delete(T,Key),
    ?line ok = dets:insert(T,Tuple),
    ?line ok = dets:insert(T,Tuple),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:sync(T),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),

    %% Key's objects are is on file only, 
    %% key toto in the cache (if there is one).
    ?line ok = dets:delete(T,toto),
    ?line false = dets:member(T, toto),
    ?line ok = dets:insert(T,[{toto,b},{toto,b}]),
    ?line true = sort([Tuple,{toto,b}]) =:= 
                 sort(dets:lookup_keys(T, [Key,toto])),
    ?line true = dets:member(T, toto),

    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),
    ?line Size = dets:info(T, size),

    %% No object with the key on the file.
    %% Delete, add one object.
    Size1 = Size + 2,
    del_and_ins(key, T, Size1, Tuple, Key, 1),
    del_and_ins(object, T, Size1, Tuple, Key, 1),
    del_and_ins(both, T, Size1, Tuple, Key, 1),

    %% One object with the key on the file.
    %% Delete it, add one object.
    Size2 = Size + 2,
    del_and_ins(key, T, Size2, Tuple, Key, 1),
    del_and_ins(object, T, Size2, Tuple, Key, 1),
    del_and_ins(both, T, Size2, Tuple, Key, 1),

    %% Overwrite an objekt on file with the same object.
    ?line ok = dets:insert(T, Tuple),
    ?line ok = dets:sync(T),
    ?line [Tuple2] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:insert(T, Tuple),
    ?line ok = dets:sync(T),
    ?line [Tuple2] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),

    %% A mix of insert and delete.
    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),
    ?line ok = dets:delete(T, Key),
    ?line ok = dets:insert(T, {Key,foo}),
    ?line ok = dets:insert(T, {Key,bar}),
    ?line [{Key,bar},{Key,foo}] = sort(dets:lookup(T, Key)),
    ?line true = dets:member(T, Key),
    ?line ok = dets:delete_object(T, {Key,foo}),
    ?line ok = dets:insert(T, {Key,kar}),
    ?line [{Key,bar},{Key,kar}] = sort(dets:lookup(T, Key)),
    ?line true = dets:member(T, Key),
    ?line ok = dets:insert(T, [{Key,kar},{Key,kar}]),
    ?line [{Key,bar},{Key,kar}] = sort(dets:lookup(T, Key)),
    ?line true = dets:member(T, Key),
    ?line ok = dets:delete_object(T, {Key,bar}),
    ?line ok = dets:delete_object(T, {Key,kar}),
    ?line [] = dets:lookup(T, Key),
    ?line false = dets:member(T, Key),
    ?line ok = dets:sync(T),
    ?line [] = dets:lookup(T, Key),
    ?line false = dets:member(T, Key),

    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow for a while, if it needs to.
	    ?line All1 = get_all_objects(T),
	    ?line dets:safe_fixtable(T, false),
	    ?line timer:sleep(1200),
	    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
	    ?line dets:safe_fixtable(T, true),
	    ?line All2 = get_all_objects(T),
	    ?line FAll2 = get_all_objects_fast(T),
	    ?line true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    test_server:fail({bags,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ?line ok = dets:close(T),
    file:delete(Fname),

    %% Second object of a key added and looked up simultaneously.
    R1 = {index_test,1,2,3,4},
    R2 = {index_test,2,2,13,14},
    R3 = {index_test,1,12,13,14},
    ?line {ok, _} = dets:open_file(T,[{version,Version},{type,bag},
				      {keypos,2},{file,Fname}]),
    ?line ok = dets:insert(T,R1),
    ?line ok = dets:sync(T),
    ?line ok = dets:insert(T,R2),
    ?line ok = dets:sync(T),
    ?line ok = dets:insert(T,R3),
    ?line [R1,R3] = sort(dets:lookup(T,1)),
    ?line true = dets:member(T, 1),
    ?line [R1,R3] = sort(dets:lookup(T,1)),
    ?line true = dets:member(T, 1),
    ?line ok = dets:close(T),
    file:delete(Fname),

    ?line check_pps(P0),
    ok.
    
cache_duplicate_bags_v8(doc) ->
    ["Test the write cache for duplicate bags."];
cache_duplicate_bags_v8(suite) ->
    [];
cache_duplicate_bags_v8(Config) when is_list(Config) ->
    cache_duplicate_bags(Config, 8).

cache_duplicate_bags_v9(doc) ->
    ["Test the write cache for duplicate bags."];
cache_duplicate_bags_v9(suite) ->
    [];
cache_duplicate_bags_v9(Config) when is_list(Config) ->
    cache_duplicate_bags(Config, 9).

cache_duplicate_bags(Config, Version) ->
    Small = 2,
    cache_dup_bags(Config, {0,0}, false, Small, Version),
    cache_dup_bags(Config, {0,0}, true, Small, Version),
    cache_dup_bags(Config, {5000,5000}, false, Small, Version),
    cache_dup_bags(Config, {5000,5000}, true, Small, Version),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_dup_bags(Config, {0,0}, false, Big, Version),
    cache_dup_bags(Config, {0,0}, true, Big, Version),
    cache_dup_bags(Config, {5000,5000}, false, Big, Version),
    cache_dup_bags(Config, {5000,5000}, true, Big, Version).

cache_dup_bags(Config, DelayedWrite, Extra, Sz, Version) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    ?line Fname = filename(cache, Config),
    ?line file:delete(Fname),
    P0 = pps(),

    ?line {ok, _} = 
	dets:open_file(T,[{version, Version}, {file,Fname}, 
			  {type,duplicate_bag}, 
			  {delayed_write, DelayedWrite}]),

    Dups = 2,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		?line dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ?line ok = dets:delete(T, Key),
    ?line ok = dets:sync(T),    
    ?line false = dets:member(T, Key),

    %% The values of keys in the same slot as Key are checked.
    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ?line ok = dets:insert(T, Tuple),
    ?line [Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    ?line [Tuple,Tuple,Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    ?line true = dets:member(T, Key),

    %% If delay, this happens without file access.
    %% (This is no longer true; cache lookup has been simplified.)
    ?line ok = dets:delete(T,Key),
    ?line ok = dets:insert(T,Tuple),
    ?line ok = dets:insert(T,Tuple),
    ?line [Tuple,Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),
    ?line ok = dets:sync(T),
    ?line [Tuple,Tuple] = dets:lookup(T, Key),
    ?line true = dets:member(T, Key),

    %% One object in the cache, one on the file.
    ?line ok = dets:delete(T,Key),
    ?line ok = dets:insert(T,Tuple),
    ?line ok = dets:sync(T),
    ?line ok = dets:insert(T,Tuple),
    ?line true = dets:member(T, Key), % should not read the file, but it does..

    %% Key's objects are is on file only, 
    %% key toto in the cache (if there is one).
    ?line ok = dets:delete(T,toto),
    ?line ok = dets:insert(T,[{toto,b},{toto,b}]),
    ?line true = sort([Tuple,Tuple,{toto,b},{toto,b}]) =:= 
                 sort(dets:lookup_keys(T, [Key,toto])),
    ?line true = dets:member(T, toto),
    ?line Size = dets:info(T, size),

    %% Two objects with the same key on the file.
    %% Delete them, add two objects.
    del_and_ins(key, T, Size, Tuple, Key, 2),

    del_and_ins(object, T, Size, Tuple, Key, 2),
    del_and_ins(both, T, Size, Tuple, Key, 2),

    %% Two objects with the same key on the file.
    %% Delete them, add three objects.
    del_and_ins(key, T, Size, Tuple, Key, 3),
    del_and_ins(object, T, Size, Tuple, Key, 3),
    del_and_ins(both, T, Size, Tuple, Key, 3),

    %% Two objects with the same key on the file.
    %% Delete them, add one object.
    del_and_ins(key, T, Size, Tuple, Key, 1),
    del_and_ins(object, T, Size, Tuple, Key, 1),
    del_and_ins(both, T, Size, Tuple, Key, 1),

    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow for a while, if it needs to.
	    ?line All1 = get_all_objects(T),
	    ?line dets:safe_fixtable(T, false),
	    ?line timer:sleep(1200),
	    ?line OtherValues = sort(lookup_keys(T, OtherKeys)),
	    ?line dets:safe_fixtable(T, true),
	    ?line All2 = get_all_objects(T),
	    ?line FAll2 = get_all_objects_fast(T),
	    ?line true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    test_server:fail({dup_bags,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ?line ok = dets:close(T),

    file:delete(Fname),
    ?line check_pps(P0),
    ok.
    
lookup_keys(_T, []) ->
    [];
lookup_keys(T, Keys) ->
    dets:lookup_keys(T, Keys).

del_and_ins(W, T, Size, Obj, Key, N) ->
    case W of
	object -> 
	    ?line ok = dets:delete_object(T, Obj);
	key ->

	    ?line ok = dets:delete(T, Key);
	both ->
	    ?line ok = dets:delete(T, Key),
	    ?line ok = dets:delete_object(T, Obj)
    end,
    Objs = duplicate(N, Obj),
    ?line [] = dets:lookup(T, Key),
    ?line ok = dets:insert(T, Objs),
    ?line Objs = dets:lookup_keys(T, [snurrespratt,Key]),
    ?line true = Size + length(Objs)-2 =:= dets:info(T, size),
    ?line Objs = dets:lookup(T, Key).


insert_objs(T, N, Sz, Dups) ->
    Seq = seq(N,N+255),
    L0 = map(fun(I) -> erlang:make_tuple(Sz, I) end, Seq),
    L = append(duplicate(Dups, L0)),
    ?line ok = dets:insert(T, L),
    ?line case search_slot(T, 0) of
	      false ->
		  insert_objs(T, N+256, Sz, Dups);
	      Keys ->
		  Keys
	  end.

search_slot(T, I) ->
    ?line case dets:slot(T, I) of
	      '$end_of_table' ->
		  false;
	      Objs ->
		  case usort(map(fun(X) -> element(1, X) end, Objs)) of
		      [_, Key, _ | _] = Keys0 ->
			  Keys = delete(Key, Keys0),
			  {Key, Keys};
		      _ ->
			  search_slot(T, I+1)
		  end
	  end.

symdiff(L1, L2) ->
    {X, _, Y} = 
	sofs:symmetric_partition(sofs:set(L1), sofs:set(L2)),
    {sofs:to_external(X), sofs:to_external(Y)}.

otp_4208(doc) ->
    ["Read only table and traversal caused crash."];
otp_4208(suite) ->
    [];
otp_4208(Config) when is_list(Config) ->
    Tab = otp_4208,
    ?line FName = filename(Tab, Config),
    Expected = sort([{3,ghi,12},{1,abc,10},{4,jkl,13},{2,def,11}]),

    file:delete(FName),
    ?line {ok, Tab} = dets:open_file(Tab, [{file,FName}]),
    ?line ok = dets:insert(Tab, [{1,abc,10},{2,def,11},{3,ghi,12},{4,jkl,13}]),
    ?line Expected = sort(dets:traverse(Tab, fun(X) -> {continue, X} end)),
    ?line ok = dets:close(Tab),

    ?line {ok, Tab} = dets:open_file(Tab, [{access, read},{file,FName}]),
    ?line Expected = sort(dets:traverse(Tab, fun(X) -> {continue, X} end)),
    ?line ok = dets:close(Tab),
    file:delete(FName),
    
    ok.

otp_4989(doc) ->
    ["Read only table and growth."];
otp_4989(suite) ->
    [];
otp_4989(Config) when is_list(Config) ->
    Tab = otp_4989,
    ?line FName = filename(Tab, Config),

    %% Do exactly as in the error report.
    ?line _Ets = ets:new(Tab, [named_table]),
    ets_init(Tab, 100000),
    ?line {ok, Tab} = 
        dets:open_file(Tab, [{access, read_write}, {file,FName}, {keypos,2}]),
    ?line ok = dets:from_ets(Tab, Tab),
    ?line ok = dets:close(Tab),
    %% Restore.
    ?line  {ok, Tab} = 
        dets:open_file(Tab, [{access, read}, {keypos, 2}, {file, FName}]),
    ?line true = ets:delete_all_objects(Tab),
    ?line true = ets:from_dets(Tab, Tab),
    ?line ok = dets:close(Tab),
    ets:delete(Tab),
    file:delete(FName),
    ok.

ets_init(_Tab, 0) ->
    ok;
ets_init(Tab, N) ->
    ets:insert(Tab, {N,N}),
    ets_init(Tab, N - 1).

many_clients(doc) ->
    ["Several clients accessing a table simultaneously."];
many_clients(suite) ->
    [];
many_clients(Config) when is_list(Config) ->
    Tab = many_clients,
    ?line FName = filename(Tab, Config),

    Server = self(),

    ?line file:delete(FName),
    P0 = pps(),
    ?line {ok, _} = dets:open_file(Tab,[{file, FName},{version,9}]),
    ?line [P1,P2,P3,P4] = new_clients(4, Tab),

    %% dets:init_table/2 is used for making sure that all processes
    %% start sending requests before the Dets process begins to handle
    %% them; the requests arrive "in parallel".

    %% Four processes accessing the same table at almost the same time.

    %% One key is read, updated, and read again.
    Seq1 = [{P1,[{lookup,1,[{1,a}]}]}, {P2,[{insert,{1,b}}]},
	    {P3,[{lookup,1,[{1,b}]}]}, {P4,[{lookup,1,[{1,b}]}]}],
    ?line atomic_requests(Server, Tab, [[{1,a}]], Seq1),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Different keys read by different processes
    Seq2 = [{P1,[{member,1,true}]}, {P2,[{lookup,2,[{2,b}]}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{lookup,3,[{3,c}]}]}],
    ?line atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq2),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Reading deleted key.
    Seq3 = [{P1,[{delete_key,2}]}, {P2,[{lookup,1,[{1,a}]}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{member,2,false}]}],
    ?line atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq3),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Inserting objects.
    Seq4 = [{P1,[{insert,[{1,a},{2,b}]}]}, {P2,[{insert,[{2,c},{3,a}]}]},
	    {P3,[{insert,[{3,b},{4,d}]}]},
	    {P4,[{lookup_keys,[1,2,3,4],[{1,a},{2,c},{3,b},{4,d}]}]}],
    ?line atomic_requests(Server, Tab, [], Seq4),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Deleting objects.
    Seq5 = [{P1,[{delete_object,{1,a}}]}, {P2,[{delete_object,{1,a}}]},
	    {P3,[{delete_object,{3,c}}]},
	    {P4,[{lookup_keys,[1,2,3,4],[{2,b}]}]}],
    ?line atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq5),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Some request not streamed.
    Seq6 = [{P1,[{lookup,1,[{1,a}]}]}, {P2,[{info,size,3}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{info,size,3}]}],
    ?line atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq6),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Some request not streamed.
    Seq7 = [{P1,[{insert,[{3,a}]}]}, {P2,[{insert,[{3,b}]}]},
	    {P3,[{delete_object,{3,c}}]},
	    {P4,[{lookup,3,[{3,b}]}]}],
    ?line atomic_requests(Server, Tab, [[{3,c}]], Seq7),
    ?line true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    ?line put_requests(Server, [{P1,stop},{P2,stop},{P3,stop},{P4,stop}]),
    ?line ok = dets:close(Tab),
    ?line file:delete(FName),

    %% Check that errors are handled correctly by the streaming operators.
    ?line {ok, _} = dets:open_file(Tab,[{file, FName},{version,9}]),
    ?line ok = ins(Tab, 100),
    Obj = {66,{item,number,66}},
    ?line {ok, ObjPos} = dets:where(Tab, Obj),
    ?line ok = dets:close(Tab),
    %% Damaged object.
    crash(FName, ObjPos+12),
    ?line {ok, _} = dets:open_file(Tab,[{file, FName},{version,9}]),
    ?line BadObject1 = dets:lookup_keys(Tab, [65,66,67,68,69]),
    ?line bad_object(BadObject1, FName),
    ?line _Error = dets:close(Tab),
    ?line file:delete(FName),

    ?line check_pps(P0),

    ok.

%% Tab is initiated with the objects in Objs. Objs = [[object()]].
atomic_requests(Server, Tab, Objs, Req) ->
    ok = dets:init_table(Tab, atomic_requests(Server, Objs, Req)).

atomic_requests(Server, L, Req) ->
    fun(close) ->
	    ok;
       (read) when [] =:= L ->
	    put_requests(Server, Req),
	    end_of_input;
       (read) ->
	    [E | Es] = L,
	    {E, atomic_requests(Server, Es, Req)}
    end.

put_requests(Server, L) ->
    lists:foreach(fun({Pid,R}) -> Pid ! {Server,R}, timer:sleep(1) end, L).

get_replies(L) ->
    lists:all(fun({Pid,Reply}) -> Reply =:= get_reply(Pid) end, L).

get_reply(Pid) ->
    ?line receive {Pid, Reply} -> Reply end.

new_clients(0, _Tab) ->
    [];
new_clients(N, Tab) ->
    [new_client(Tab) | new_clients(N-1, Tab)].

new_client(Tab) ->
    spawn(?MODULE, client, [self(), Tab]).

client(S, Tab) ->
    receive 
	{S, stop} ->
	    exit(normal);
	{S, ToDo} ->
	    ?line Reply = eval(ToDo, Tab),
	    case Reply of
		{error, _} -> io:format("~p: ~p~n", [self(), Reply]);
		_ -> ok
	    end,
	    S ! {self(), Reply}
    end,
    client(S, Tab).

eval([], _Tab) ->
    ok;
eval([sync | L], Tab) ->
    ?line case dets:sync(Tab) of
	      ok -> eval(L, Tab);
	      Error -> {error, {sync,Error}}
	  end;
eval([{insert,Stuff} | L], Tab) ->
    ?line case dets:insert(Tab, Stuff) of
	      ok -> eval(L, Tab);
	      Error -> {error, {insert,Stuff,Error}}
	  end;
eval([{lookup,Key,Expected} | L], Tab) ->
    ?line case dets:lookup(Tab, Key) of
	      Expected -> eval(L, Tab);
	      Else -> {error, {lookup,Key,Expected,Else}}
	  end;
eval([{lookup_keys,Keys,Expected} | L], Tab) ->
    %% Time order is destroyed...
    ?line case dets:lookup_keys(Tab, Keys) of
	      R when is_list(R) -> 
		  case lists:sort(Expected) =:= lists:sort(R) of
		      true -> eval(L, Tab);
		      false -> {error, {lookup_keys,Keys,Expected,R}}
		  end;
	      Else -> {error, {lookup_keys,Keys,Expected,Else}}
	  end;
eval([{member,Key,Expected} | L], Tab) ->
    ?line case dets:member(Tab, Key) of
	      Expected -> eval(L, Tab);
	      Else -> {error, {member,Key,Expected,Else}}
	  end;
eval([{delete_key,Key} | L], Tab) ->
    ?line case dets:delete(Tab, Key) of
	      ok -> eval(L, Tab);
	      Else -> {error, {delete_key,Key,Else}}
	  end;
eval([{delete_object,Object} | L], Tab) ->
    ?line case dets:delete_object(Tab, Object) of
	      ok -> eval(L, Tab);
	      Else -> {error, {delete_object,Object,Else}}
	  end;
eval([{info,Tag,Expected} | L], Tab) ->
    ?line case dets:info(Tab, Tag) of
	      Expected -> eval(L, Tab);
	      Else -> {error, {info,Tag,Else,Expected}}
	  end;
eval(Else, _Tab) ->
    {error, {bad_request,Else}}.

otp_4906(doc) ->
    ["More than 128k keys caused crash."];
otp_4906(suite) ->
    [];
otp_4906(Config) when is_list(Config) ->
    N = 256*512 + 400,
    Tab = otp_4906,
    ?line FName = filename(Tab, Config),
    
    file:delete(FName),
    ?line {ok, Tab} = dets:open_file(Tab, [{file, FName}]),
    ?line ok = ins_small(Tab, 0, N),
    ?line ok = dets:close(Tab),
    ?line {ok, Tab} = dets:open_file(Tab, [{file, FName}]),
    ?line ok = read_4906(Tab, N-1),
    ?line ok = dets:close(Tab),
    file:delete(FName),

    %% If the (only) process fixing a table updates the table, the
    %% process will no longer be punished with a 1 ms delay (hm, the
    %% server is delayed, it should be the client...). In this example
    %% the writing process *is* delayed.
    ?line {ok,Tab} = dets:open_file(Tab, [{file,FName}]),
    Parent = self(),
    FixPid = spawn_link(fun() -> 
                                dets:safe_fixtable(Tab, true),
                                receive {Parent, stop} -> ok end
                        end),
    ?line ok = ins_small(Tab, 0, 1000),
    FixPid ! {Parent, stop},
    timer:sleep(1),
    ?line ok = dets:close(Tab),
    file:delete(FName),
    ok.

read_4906(_T, N) when N < 0 ->
    ok;
read_4906(T, N) ->
    ?line [_] = dets:lookup(T, N),
    read_4906(T, N-1).

ins_small(_T, I, N) when I =:= N ->
    ok;
ins_small(T, I, N) ->
    ?line ok = dets:insert(T, {I}),
    ins_small(T, I+1, N).

otp_5402(doc) ->
    ["Unwritable ramfile caused krasch."];
otp_5402(suite) ->
    [];
otp_5402(Config) when is_list(Config) ->
    Tab = otp_5402,
    ?line File = filename:join([cannot, write, this, file]),

    %% close
    ?line{ok, T} = dets:open_file(Tab, [{ram_file,true},
                                        {file, File}]),
    ?line ok = dets:insert(T, {1,a}),
    ?line {error,{file_error,_,_}} = dets:close(T),

    %% sync
    ?line {ok, T} = dets:open_file(Tab, [{ram_file,true},
                                         {file, File}]),
    ?line ok = dets:insert(T, {1,a}),
    ?line {error,{file_error,_,_}} = dets:sync(T),
    ?line {error,{file_error,_,_}} = dets:close(T),

    %% auto_save
    ?line {ok, T} = dets:open_file(Tab, [{ram_file,true},
                                         {auto_save, 2000},
                                         {file, File}]),
    ?line ok = dets:insert(T, {1,a}),
    ?line timer:sleep(5000),
    ?line {error,{file_error,_,_}} = dets:close(T),
    ok.

simultaneous_open(doc) ->
    ["Several clients open and close tables simultaneously."];
simultaneous_open(suite) ->
    [];
simultaneous_open(Config) ->
    Tab = sim_open,
    File = filename(Tab, Config),
    
    ?line ok = monit(Tab, File),
    ?line ok = kill_while_repairing(Tab, File),
    ?line ok = kill_while_init(Tab, File),
    ?line ok = open_ro(Tab, File),
    ?line ok = open_w(Tab, File, 0, Config),
    ?line ok = open_w(Tab, File, 100, Config),
    ok.

%% One process logs and another process closes the log. Before
%% monitors were used, this would make the client never return.
monit(Tab, File) ->
    file:delete(File),
    {ok, Tab} = dets:open_file(Tab, [{file,File}]),
    F1 = fun() -> dets:close(Tab) end,
    F2 = fun() -> {'EXIT', {badarg, _}} = do_log(Tab) end,
    spawn(F2),
    timer:sleep(100),
    spawn(F1),
    dets:close(Tab),
    file:delete(File),
    ok.

do_log(Tab) ->
    case catch dets:insert(Tab, {hej,san,sa}) of
        ok -> do_log(Tab);
        Else -> Else
    end.

%% Kill the Dets process while repair is in progress.
kill_while_repairing(Tab, File) ->
    ?line create_opened_log(File),
    Delay = 1000,
    dets:start(),
    Parent = self(),
    Ps = processes(),
    F = fun() -> 
                R = (catch dets:open_file(Tab, [{file,File}])),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    ?line P1 = spawn(F), % will repair
    timer:sleep(100),
    ?line P2 = spawn(F), % pending...
    ?line P3 = spawn(F), % pending...
    ?line DetsPid = find_dets_pid([P1, P2, P3 | Ps]),
    exit(DetsPid, kill),

    ?line receive {P1,R1} -> {'EXIT', {dets_process_died, _}} = R1 end,
    ?line receive {P2,R2} -> {ok, _} = R2 end,
    ?line receive {P3,R3} -> {ok, _} = R3 end,

    timer:sleep(200),
    case dets:info(Tab) of
        undefined -> 
            ok;
        _Info ->
            timer:sleep(5000),
            ?line undefined = dets:info(Tab)
    end,

    file:delete(File),
    ok.

find_dets_pid(P0) ->
    case lists:sort(processes() -- P0) of
        [P, _] -> P;
        _ -> timer:sleep(100), find_dets_pid(P0)
    end.

%% Kill the Dets process when there are users and an on-going
%% initiailization.
kill_while_init(Tab, File) ->
    file:delete(File),
    Parent = self(),
    F = fun() -> 
                R = dets:open_file(Tab, [{file,File}]),
                Parent ! {self(), R},
                receive {Parent, die} -> ok end,
                {error, not_owner} = dets:close(Tab)
        end,
    ?line P1 = spawn(F),
    ?line P2 = spawn(F),
    ?line P3 = spawn(F),
    IF = fun() ->
                 R = dets:open_file(Tab, [{file,File}]),
                 Parent ! {self(), R},
                 Fun = fun(_) -> timer:sleep(100000) end,
                 {'EXIT', {badarg, _}} = (catch dets:init_table(Tab, Fun)),
                 receive {Parent, die} -> ok end
          end,
    ?line P4 = spawn(IF),
    ?line receive {P1,R1} -> {ok, _} = R1 end,
    ?line receive {P2,R2} -> {ok, _} = R2 end,
    ?line receive {P3,R3} -> {ok, _} = R3 end,
    ?line receive {P4,R4} -> {ok, _} = R4 end,
    ?line [DetsPid] = 
        lists:filter(fun(P) -> dets:pid2name(P) =/= undefined end, 
                     erlang:processes()),
    exit(DetsPid, kill),
    
    timer:sleep(1000),
    ?line undefined = dets:info(Tab),
    ?line P1 ! {Parent, die},
    ?line P2 ! {Parent, die},
    ?line P3 ! {Parent, die},
    ?line P4 ! {Parent, die},

    file:delete(File),
    timer:sleep(100),
    ok.

open_ro(Tab, File) ->
    ?line create_opened_log(File),
    Delay = 1000,
    Parent = self(),
    F = fun() ->
                R = dets:open_file(Tab, [{file,File},{access,read}]),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    ?line P1 = spawn(F),
    ?line P2 = spawn(F),
    ?line P3 = spawn(F),
    
    ?line receive {P1,R1} -> {error,{not_closed,_}} = R1 end,
    ?line receive {P2,R2} -> {error,{not_closed,_}} = R2 end,
    ?line receive {P3,R3} -> {error,{not_closed,_}} = R3 end,
    ok.

open_w(Tab, File, Delay, Config) ->
    ?line create_opened_log(File),
    Parent = self(),
    F = fun() -> 
                R = dets:open_file(Tab, [{file,File}]),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    ?line Pid1 = spawn(F),
    ?line Pid2 = spawn(F),
    ?line Pid3 = spawn(F),
    ?line undefined = dets:info(Tab), % is repairing now
    ?line 0 = qlen(),

    Tab2 = t2,
    File2 = filename(Tab2, Config),
    ?line file:delete(File2),
    ?line {ok,Tab2} = dets:open_file(Tab2, [{file,File2}]),
    ?line ok = dets:close(Tab2),
    ?line file:delete(File2),
    ?line 0 = qlen(), % still repairing

    ?line receive {Pid1,R1} -> {ok, Tab} = R1 end,
    ?line receive {Pid2,R2} -> {ok, Tab} = R2 end,
    ?line receive {Pid3,R3} -> {ok, Tab} = R3 end,
    timer:sleep(200),
    case dets:info(Tab) of
        undefined -> 
            ok;
        _Info ->
            timer:sleep(5000),
            ?line undefined = dets:info(Tab)
    end,

    file:delete(File),
    ok.

qlen() ->
    {_, {_, N}} = lists:keysearch(message_queue_len, 1, process_info(self())),
    N.

create_opened_log(File) ->
    Tab = t,
    file:delete(File),
    ?line {ok, Tab} = dets:open_file(Tab, [{file,File}]),
    ?line ok = ins(Tab, 60000),
    ?line ok = dets:close(Tab),
    ?line crash(File, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    ok.

insert_new(doc) ->
    ["OTP-5075. insert_new/2"];
insert_new(suite) ->
    [];
insert_new(Config) ->
    Tab = insert_new,
    File = filename(Tab, Config),
    file:delete(File),
    ?line {ok, T} = dets:open_file(Tab, [{file,File}]),
    ?line {'EXIT', {badarg, _}} = (catch dets:insert_new(Tab, 14)),
    ?line {'EXIT', {badarg, _}} = (catch dets:insert_new(Tab, {})),
    ?line true = dets:insert_new(Tab, {1,a}),
    ?line false = dets:insert_new(Tab, {1,a}),
    ?line true = dets:insert_new(Tab, [{2,b}, {3,c}]),
    ?line false = dets:insert_new(Tab, [{2,b}, {3,c}]),
    ?line false = dets:insert_new(Tab, [{1,a}, {4,d}]),
    ?line ok = dets:close(Tab),

    file:delete(File),    
    ?line {ok, T} = dets:open_file(Tab, [{file,File},{type,bag}]),
    ?line true = dets:insert_new(Tab, {1,a}),
    ?line false = dets:insert_new(Tab, {1,b}),
    ?line true = dets:insert_new(Tab, [{2,b}, {3,c}]),
    ?line false = dets:insert_new(Tab, [{2,a}, {3,d}]),
    ?line false = dets:insert_new(Tab, [{1,a}, {4,d}]),
    ?line ok = dets:close(Tab),
    

    file:delete(File),
    ok.
    
repair_continuation(doc) ->
    ["OTP-5126. repair_continuation/2"];
repair_continuation(suite) ->
    [];
repair_continuation(Config) ->
    Tab = repair_continuation_table,
    ?line Fname = filename(repair_cont, Config),
    ?line file:delete(Fname),
    ?line {ok, _} = dets:open_file(Tab, [{file,Fname}]),
    ?line ok = dets:insert(Tab, [{1,a},{2,b},{3,c}]),

    ?line MS = [{'_',[],[true]}],

    ?line {[true], C1} = dets:select(Tab, MS, 1),
    ?line C2 = binary_to_term(term_to_binary(C1)),
    ?line {'EXIT', {badarg, _}} = (catch dets:select(C2)),
    ?line C3 = dets:repair_continuation(C2, MS),
    ?line {[true], C4} = dets:select(C3),
    ?line C5 = dets:repair_continuation(C4, MS),
    ?line {[true], _} = dets:select(C5),
    ?line {'EXIT', {badarg, _}} = (catch dets:repair_continuation(Tab, bu)),

    ?line ok = dets:close(Tab),
    ?line file:delete(Fname),
    ok.

otp_5487(doc) ->
    ["OTP-5487. Growth of read-only table (again)."];
otp_5487(suite) ->
    [];
otp_5487(Config) ->
    otp_5487(Config, 9),
    otp_5487(Config, 8),
    ok.

otp_5487(Config, Version) ->
    Tab = otp_5487,
    ?line Fname = filename(otp_5487, Config),
    ?line file:delete(Fname),
    ?line Ets = ets:new(otp_5487, [public, set]),
    ?line lists:foreach(fun(I) -> ets:insert(Ets, {I,I+1}) end,
                        lists:seq(0,1000)),
    ?line {ok, _} = dets:open_file(Tab, [{file,Fname},{version,Version}]),
    ?line ok = dets:from_ets(Tab, Ets),
    ?line ok = dets:sync(Tab),
    ?line ok = dets:close(Tab),
    ?line {ok, _} = dets:open_file(Tab, [{file,Fname},{access,read}]),
    ?line [{1,2}] = dets:lookup(Tab, 1),
    ?line ok = dets:close(Tab),
    ?line ets:delete(Ets),
    ?line file:delete(Fname).

otp_6206(doc) ->
    ["OTP-6206. Badly formed free lists."];
otp_6206(suite) ->
    [];
otp_6206(Config) ->
    Tab = otp_6206,
    File = filename(Tab, Config),

    file:delete(File),
    Options = [{file,File}],
    ?line {ok, Tab} = dets:open_file(Tab, Options),
    NObjs = 13006,
    ?line ok = ins(Tab, NObjs),
    ?line ok = del(Tab, NObjs, 2),
    ?line ok = dets:close(Tab),

    %% Used to return {badmatch,{error,{bad_freelists,File}}.
    ?line {ok, Tab} = dets:open_file(Tab, [{repair,false}|Options]),
    ?line ok = dets:close(Tab),
    file:delete(File),
    ok.

otp_6359(doc) ->
    ["OTP-6359. select and match never return the empty list."];
otp_6359(suite) ->
    [];
otp_6359(Config) ->
    Tab = otp_6359,
    File = filename(Tab, Config),

    file:delete(File),
    ?line {ok, _} = dets:open_file(Tab, [{file, File}]),
    %% Used to return {[], Cont}:
    ?line '$end_of_table' = dets:match(Tab, '_', 100),
    ?line ok = dets:close(Tab),
    file:delete(File),
    ok.

otp_4738(doc) ->
    ["OTP-4738. ==/2 and =:=/2."];
otp_4738(suite) ->
    [];
otp_4738(Config) ->
    %% Version 8 has not been corrected.
    %% (The constant -12857447 is for version 9 only.)
    otp_4738_set(9, Config),
    otp_4738_bag(9, Config),
    otp_4738_dupbag(9, Config),
    ok.

otp_4738_dupbag(Version, Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    I = -12857447,
    F = float(I),
    One = 1,
    FOne = float(One),
    Args = [{file,File},{type,duplicate_bag},{version,Version}],
    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ?line ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ?line ok = dets:sync(Tab),
    ?line [{F,One},{F,FOne}] = dets:lookup(Tab, F),
    ?line [{I,One},{I,FOne}] = dets:lookup(Tab, I),
    ?line ok = dets:insert(Tab, [{F,One},{F,FOne}]),
    ?line [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},{F,FOne}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:insert(Tab, [{F,FOne},{F,One}]),
    ?line [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},
           {F,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:delete_object(Tab, {I,FOne}),
    ?line [{I,One},{F,One},{F,FOne},{F,One},{F,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:insert(Tab, {I,FOne}),
    ?line [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},
           {F,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:delete_object(Tab, {F,FOne}),
    ?line [{I,One},{I,FOne},{F,One},{F,One},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:delete(Tab, F),
    ?line [{I,One},{I,FOne}] = dets:match_object(Tab, '_'),
    ?line ok = dets:close(Tab),
    file:delete(File),

    Zero = 0,
    FZero = float(Zero),
    ?line {ok, Tab} = dets:open_file(Tab, Args),    
    ?line ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ?line ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ?line ok = dets:insert(Tab, [{I,Zero},{F,Zero},{I,FZero},{I,FZero}]),
    ?line Objs0 = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    crash(File, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    io:format("Expect repair:~n"),
    ?line {ok, Tab} = dets:open_file(Tab, Args),    
    ?line Objs1 = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    ?line Objs1 = Objs0,
    file:delete(File),
    ok.

otp_4738_bag(Version, Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    I = -12857447,
    F = float(I),
    One = 1,
    FOne = float(One),
    Args = [{file,File},{type,bag},{version,Version}],
    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ?line ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ?line ok = dets:sync(Tab),
    ?line [{F,One},{F,FOne}] = dets:lookup(Tab, F),
    ?line [{I,One},{I,FOne}] = dets:lookup(Tab, I),
    ?line ok = dets:insert(Tab, [{F,One},{F,FOne}]),
    ?line [{I,One},{I,FOne},{F,One},{F,FOne}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:insert(Tab, [{F,FOne},{F,One}]),
    ?line [{I,One},{I,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:delete_object(Tab, {I,FOne}),
    ?line [{I,One},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:insert(Tab, {I,FOne}),
    ?line [{I,One},{I,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:delete(Tab, F),
    ?line [{I,One},{I,FOne}] = dets:match_object(Tab, '_'),
    ?line ok = dets:close(Tab),
    file:delete(File).

otp_4738_set(Version, Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    Args = [{file,File},{type,set},{version,Version}],

    %% I and F share the same slot.
    I = -12857447,
    F = float(I),
    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ?line ok = dets:insert(Tab, [{I},{F}]),
    ?line ok = dets:sync(Tab),
    ?line [{F}] = dets:lookup(Tab, F),
    ?line [{I}] = dets:lookup(Tab, I),
    ?line ok = dets:insert(Tab, [{F}]),
    ?line [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    file:delete(File),
    
    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ?line ok = dets:insert(Tab, [{I}]),
    ?line ok = dets:sync(Tab),
    ?line [] = dets:lookup(Tab, F),
    ?line [{I}] = dets:lookup(Tab, I),
    ?line ok = dets:insert(Tab, [{F}]),
    ?line [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    file:delete(File),

    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I},{F}]),
    %% {insert, ...} in the cache, try lookup:
    ?line [{F}] = dets:lookup(Tab, F),
    ?line [{I}] = dets:lookup(Tab, I),
    %% Both were found, but that cannot be verified.
    ?line [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    file:delete(File),

    ?line {ok, Tab} = dets:open_file(Tab, Args),
    ?line ok = dets:insert(Tab, [{I}]),
    ?line ok = dets:sync(Tab),
    ?line ok = dets:insert(Tab, [{F}]),
    %% {insert, ...} in the cache, try lookup:
    ?line [{F}] = dets:lookup(Tab, F),
    ?line [{I}] = dets:lookup(Tab, I),
    ?line [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    file:delete(File),

    ?line {ok, Tab} = dets:open_file(Tab, Args),
    %% Both operations in the cache:
    ?line ok = dets:insert(Tab, [{I}]),
    ?line ok = dets:insert(Tab, [{F}]),
    ?line [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ?line ok = dets:close(Tab),
    file:delete(File),
    ok.

otp_7146(doc) ->
    ["OTP-7146. Bugfix: missing test when re-hashing."];
otp_7146(suite) ->
    [];
otp_7146(Config) ->
    Tab = otp_7146,
    File = filename(Tab, Config),
    file:delete(File),

    Max = 2048,
    ?line {ok, Tab} = dets:open_file(Tab, [{max_no_slots,Max}, {file,File}]),
    write_dets(Tab, Max),
    ?line ok = dets:close(Tab),

    file:delete(File),
    ok.

write_dets(Tab, Max) ->
    write_dets(Tab, 0, Max).

write_dets(_Tab, N, Max) when N > Max ->
    ok;
write_dets(Tab, N, Max) ->
    ok = dets:insert(Tab,{ N, {entry,N}}),
    write_dets(Tab, N+1, Max).

otp_8070(doc) ->
    ["OTP-8070. Duplicated objects with insert_new() and duplicate_bag."];
otp_8070(suite) ->
    [];
otp_8070(Config) when is_list(Config) ->
    Tab = otp_8070,
    File = filename(Tab, Config),
    file:delete(File),
    ?line {ok, _} = dets:open_file(Tab, [{file,File},{type, duplicate_bag}]),
    ?line ok = dets:insert(Tab, [{3,0}]),
    ?line false = dets:insert_new(Tab, [{3,1},{3,1}]),
    ?line [{3,0}] = dets:lookup(Tab, 3),
    ?line ok = dets:close(Tab),
    file:delete(File),
    ok.

%%
%% Parts common to several test cases
%% 

crash(File, Where) ->
    crash(File, Where, 10).

crash(File, Where, What) when is_integer(What) ->
    ?line {ok, Fd} = file:open(File, read_write),
    ?line file:position(Fd, Where),
    ?line ok = file:write(Fd, [What]),
    ?line ok = file:close(Fd).

args(Config) ->
    {Sets, Bags, Dups} = 
	{[
	  [], 
	  [{type, set}, {estimated_no_objects, 300},  
	   {ram_file, true}],
	  [{type, set}, {estimated_no_objects, 300}],
	  [{type, set}, {estimated_no_objects, 300}],
	  [{auto_save,20}, {type, set}, 
	   {estimated_no_objects, 300}] 
	 ],

	 [
	  [{type, bag}, {estimated_no_objects, 300}, {ram_file, true}],
	  [{type, bag}],
	  [{type, bag}, {estimated_no_objects, 300}],
	  [{type, bag}, {estimated_no_objects, 300}],
	  [{type, bag}, 
	   {auto_save,20}, {estimated_no_objects, 300}],
	  [{type, bag}, {estimated_no_objects, 300},   {ram_file, true}]
	 ],

	 [
	  [{type, duplicate_bag}, {estimated_no_objects, 300}, 
	   {ram_file, true}],
	  [{type, duplicate_bag}],
	  [{type, duplicate_bag}, {estimated_no_objects, 300}],
	  [{type, duplicate_bag}, {estimated_no_objects, 300}],
	  [{type, duplicate_bag},
	   {auto_save,20}, {estimated_no_objects, 300}],
	  [{type, duplicate_bag}, {estimated_no_objects, 300},   
	   {ram_file, true}]
	 ]
	},
    zip_filename(Sets, Bags, Dups, Config).

zip_filename(S, B, D, Conf) ->
    zip_filename(S, B, D, [], [], [], 1, Conf).

zip_filename([H|T], B, D, S1, B1, D1, I, Conf) ->
    zip_filename(T, B, D, [[{file, new_filename(I, Conf)} | H] | S1],
		 B1, D1, I+1, Conf);
zip_filename([], [H|B], D, S1, B1, D1, I, Conf) ->
    zip_filename([], B, D, S1, [[{file, new_filename(I, Conf)} | H] | B1],
		 D1, I+1, Conf);
zip_filename([], [], [H|T], S1, B1, D1, I, Conf) ->
    zip_filename([], [], T, S1, B1, [[{file, new_filename(I, Conf)} | H] | D1],
		 I+1, Conf);
zip_filename([], [], [], S1, B1, D1, _, _Conf) ->
    {reverse(S1), reverse(B1), reverse(D1)}.

del_test(Tab) ->
    ?format("Deltest on ~p~n", [Tab]),
    ?line Objs = safe_get_all_objects(Tab),
    ?line Keys = map(fun(X) -> element(1, X) end, Objs),
    ?line foreach(fun(Key) -> dets:delete(Tab, Key) end, Keys),
    ?line 0 = length(get_all_objects(Tab)),
    ?line [] = get_all_objects_fast(Tab),
    ?line 0 = dets:info(Tab, size).

del_obj_test(Tab) ->
    ?format("Delobjtest on ~p~n", [Tab]),
    ?line Objs = safe_get_all_objects(Tab),
    ?line LL = length(Objs),
    ?line LL = dets:info(Tab, size),
    ?line foreach(fun(Obj) -> dets:delete_object(Tab, Obj) end, Objs),
    ?line 0 = length(get_all_objects(Tab)),
    ?line [] = get_all_objects_fast(Tab),
    ?line 0 = dets:info(Tab, size).

match_del_test(Tab) ->
    ?line ?format("Match delete test on ~p~n", [Tab]),
    ?line ok = dets:match_delete(Tab, {'_','_','_'}),
    ?line Sz = dets:info(Tab, size),
    ?line true = Sz =:= length(dets:match_object(Tab, '_')),
    ?line ok = dets:match_delete(Tab, '_'),
    ?line 0 = dets:info(Tab, size),
    ?line 0 = length(get_all_objects(Tab)),
    ?line [] = get_all_objects_fast(Tab).

trav_test(_Data, Len, Tab) ->
    ?format("Travtest on ~p~n", [Tab]),
    ?line _X0 = dets:traverse(Tab, fun(_X) -> continue end),
    ?line XX = dets:traverse(Tab, fun(X) -> {continue, X} end),
    ?line case Len =:= length(XX) of
	      false -> ?format("DIFF ~p~n", [XX -- _Data]);
	      true -> ok
	  end,
    ?line 1 = length(dets:traverse(Tab, fun(X) -> {done, X} end)).

match_test(Data, Tab) ->
    ?line ?format("Match test on ~p~n", [Tab]),
    ?line Data1 = sort(filter(fun(X) when tuple_size(X) =:= 3 -> true;
				 (_X) -> false
			      end, Data)),
    ?line Data1 = sort(dets:match_object(Tab, {'$1', '$2', '$3'})),

    ?line Len = length(Data),
    ?line Len = length(dets:match(Tab, '_')),
    ?line Len2 = length(Data1),
    ?line Len2 = length(dets:match(Tab, {'$1', '_', '_'})),
    
    ?line Data3 = 
	filter(fun(X) ->
		       K = element(1, X),
		       if
			   tuple_size(X) =:= 3, tuple_size(K) =:= 2 -> true;
			   true -> false
		       end
	       end, Data),
    ?line Len3 = length(Data3),
    ?line Len3 = length(dets:match(Tab, {{'$1', '$2'}, '_', '_'})),
    ?line Len3 = length(dets:match_object(Tab, {{'$1', '$2'}, '_', '_'})),
    
    ?line R = make_ref(),
    ?line dets:insert(Tab, {{R, R}, 33 ,44}),
    ?line 1 = length(dets:match(Tab, {{R, R}, '_', '_'})),
    ?line 1 = length(dets:match_object(Tab, {{R, R}, '_', '_'})).

%%
%% Utilities
%%

headsz(8) ->
    ?HEADSZ_v8;
headsz(_) ->
    ?HEADSZ_v9.

unwritable(Fname) ->
    ?line {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    ?line file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    ?line {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    ?line file:write_file_info(Fname, Info#file_info{mode = Mode}).

truncate(File, Where) ->
    ?line {ok, Fd} = file:open(File, read_write),
    ?line file:position(Fd, Where),
    ?line ok = file:truncate(Fd),
    ?line ok = file:close(Fd).

new_filename(Name, _Config) when is_integer(Name) ->
    filename:join(?privdir(_Config),
		  integer_to_list(Name) ++ ".DETS").

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, _Config) ->
    filename:join(?privdir(_Config), Name).

open_files(_Name, [], _Version) ->
    [];
open_files(Name0, [Args | Tail], Version) ->
    ?format("init ~p~n", [Args]),
    ?line Name = list_to_atom(integer_to_list(Name0)),
    ?line {ok, Name} = dets:open_file(Name, [{version,Version} | Args]),
    [Name | open_files(Name0+1, Tail, Version)].
    
close_all(Tabs) -> foreach(fun(Tab) -> ok = dets:close(Tab) end, Tabs).

delete_files(Args) ->   
    Fun = fun(F) ->
		  {value, {file, File}} = keysearch(file, 1, F),
		  file:delete(File),
		  File
	  end,
    map(Fun, Args).

%% Initialize all tables
initialize(Tabs, Data) ->
    ?line foreach(fun(Tab) ->
			  Fun = fun(Obj) -> ok =  dets:insert(Tab, Obj) end,
			  foreach(Fun, Data),
			  dets:sync(Tab)
		  end, Tabs).

%% need more than 512 objects to really trig overflow
make_data(Kp) ->
    make_data(Kp, set).

make_data(Kp, Type) ->
    dup(Type, make_data(Kp, Type, 520)).

dup(duplicate_bag, [H1, H2 |T]) ->
    [H1,H2, H1, H2 | dup(duplicate_bag, T)];
dup(_, Other) ->
    Other.
    
make_data(_Kp, Type, 0) -> 
    odd_keys(Type);
make_data(1, set, I) ->
    [{I, q,w} | make_data(1, set, I-1)];
make_data(2, set, I) ->
    [{hh, I, q,w} | make_data(2, set, I-1)];
make_data(1, bag, I) ->
    [{I, q,w} , {I, hah, 77} | make_data(1, bag, I-1)];
make_data(2, bag, I) ->
    [{hh, I, q,w} , {hh, I, lalal, 900} | make_data(2, bag, I-1)];
make_data(1, duplicate_bag, I) ->
    [{I, q,w} , {I, hah, 77} | make_data(1, duplicate_bag, I-1)];
make_data(2, duplicate_bag, I) ->
    [{hh, I, q,w} , {hh, I, lalal, 900} | make_data(2, duplicate_bag, I-1)].

odd_keys(_) ->
    [{foo, 1 ,2},
     {{foo, foo}, 2,3},
     {"kakaka", {{{}}}, jj},
     {{"kallll", "kkk", []}, 66.7777},
     {make_ref(), 99, 66},
     {{1},2,3,4,5,6,7,duplicate(50, 8)},
     {self(), 7,8,88},
     {[self()], 8, 11}].


ins(_T, 0) ->
    ok;
ins(T, N) ->
    case dets:insert(T, {N, item(N)}) of
	ok -> ins(T, N-1);
	Error -> Error
    end.

item(N) when N rem 2 =:= 0 ->
    {item, number, N};
item(N) ->
    {item, number, N, a, much, bigger, one, i, think}.

del(_T, N, _I) when N =< 0 ->
    ok;
del(T, N, I) ->
    ok = dets:delete(T, N),
    del(T, N-I, I).

ensure_node(0, _Node) ->
    could_not_start_node;
ensure_node(N, Node) ->
    case net_adm:ping(Node) of
	pang ->
	    receive after 1000 ->
			     ok
		    end,
	    ensure_node(N-1,Node);
	pong ->
	    ok
    end.

size_test(Len, Tabs) ->
    ?line foreach(fun(Tab) ->
			  Len = dets:info(Tab, size)
		  end, Tabs).

no_keys_test([T | Ts]) ->
    no_keys_test(T),
    no_keys_test(Ts);
no_keys_test([]) ->
    ok;
no_keys_test(T) ->
    case dets:info(T, version) of
	8 ->
	    ok;
	9 ->
	    Kp = dets:info(T, keypos),
	    ?line All = dets:match_object(T, '_'),
	    ?line L = lists:map(fun(X) -> element(Kp, X) end, All),
	    ?line NoKeys = length(lists:usort(L)),
	    ?line case {dets:info(T, no_keys), NoKeys} of
		      {N, N} ->
			  ok;
		      {N1, N2} ->
			  exit({no_keys_test, N1, N2})
		  end
    end.

safe_get_all_objects(Tab) ->
    dets:safe_fixtable(Tab, true),
    Objects = get_all_objects(Tab),
    dets:safe_fixtable(Tab, false),
    Objects.

%% Caution: unless the table has been fixed, strange results can be returned.
get_all_objects(Tab) -> get_all_objects(dets:first(Tab), Tab, []).

%% Assuming no key matches {error, Reason}...
get_all_objects('$end_of_table', _Tab, L) -> L;
get_all_objects({error, Reason}, _Tab, _L) ->
    exit({get_all_objects, get(line), {error, Reason}});
get_all_objects(Key, Tab, L) -> 
    Objs = dets:lookup(Tab, Key),
    ?line get_all_objects(dets:next(Tab, Key), Tab, Objs ++ L).

count_objects_quite_fast(Tab) ->
    ?line R1 = dets:match_object(Tab, '_', 1),
    count_objs_1(R1, 0).

count_objs_1('$end_of_table', N) ->
    N;
count_objs_1({Ts,C}, N) when is_list(Ts) ->
    count_objs_1(dets:match_object(C), length(Ts) + N).

get_all_objects_fast(Tab) ->
    dets:match_object(Tab, '_').

%% Relevant for version 8.
histogram(Tab) ->
    OnePercent = case dets:info(Tab, no_slots) of
	undefined -> undefined;
	{_, NoSlots, _} -> NoSlots/100
    end,
    histogram(Tab, OnePercent).

histogram(Tab, OnePercent) ->
    ?line E = ets:new(histo, []),
    ?line dets:safe_fixtable(Tab, true),
    ?line Hist = histo(Tab, E, 0, OnePercent, OnePercent),
    ?line dets:safe_fixtable(Tab, false),
    ?line case Hist of
	      ok ->
		  ?line H = ets:tab2list(E),
		  ?line true = ets:delete(E),
		  sort(H);
	      Error ->
		  ets:delete(E),
		  Error
	  end.

histo(T, E, I, One, Count) when is_number(Count), I > Count ->
    io:format("."),
    histo(T, E, I, One, Count+One);
histo(T, E, I, One, Count) ->
    ?line case dets:slot(T, I) of
	      '$end_of_table' when is_number(Count) ->
		  io:format("~n"),
		  ok;
	      '$end_of_table' ->
		  ok;
	       Objs when is_list(Objs) ->
		  L = length(Objs),
		  case catch ets:update_counter(E, L, 1) of
		      {'EXIT', _} ->
			  ets:insert(E, {L, 1});
		      _ ->
			  ok
		  end,
		  histo(T, E, I+1, One, Count);
	      Error ->
		  Error
	  end.

sum_histogram(H) ->
    sum_histogram(H, 0).

sum_histogram([{S,N1} | H], N) ->
    sum_histogram(H, N + S*N1);
sum_histogram([], N) ->
    N.

ave_histogram(H) ->
    ave_histogram(H, 0)/sum_histogram(H).

ave_histogram([{S,N1} | H], N) ->
    ave_histogram(H, N + S*S*N1);
ave_histogram([], N) ->
    N.

bad_object({error,{bad_object,FileName}}, FileName) ->
    ok; % Version 8, no debug.
bad_object({error,{{bad_object,_,_},FileName}}, FileName) ->
    ok; % Version 8, debug...
bad_object({error,{{bad_object,_}, FileName}}, FileName) ->
    ok; % No debug.
bad_object({error,{{{bad_object,_,_},_,_,_}, FileName}}, FileName) ->
    ok. % Debug.

check_pps(P0) ->
    case pps() of
        P0 ->
            ok;
        _ ->
            %% On some (rare) occasions the dets process is still
            %% running although the call to close() has returned, as
            %% it seems...
            timer:sleep(500),
            case pps() of
                P0 ->
                    ok;
                P1 -> 
                    io:format("failure, got ~p~n, expected ~p\n", [P1, P0]),
                    {Ports0,Procs0} = P0,
                    {Ports1,Procs1} = P1,
                    show("Old ports", Ports0 -- Ports1),
                    show("New ports", Ports1 -- Ports0),
                    show("Old procs", Procs0 -- Procs1),
                    show("New procs", Procs1 -- Procs0),
                    ?t:fail()
            end
    end.

show(_S, []) ->
    ok;
show(S, L) ->
    io:format("~s: ~p~n", [S, L]).

pps() ->
    dets:start(),
    {port_list(), process_list()}.

port_list() ->
    [{P,safe_second_element(erlang:port_info(P, name))} || 
        P <- erlang:ports()].

process_list() ->
    [{P,process_info(P, registered_name),
      safe_second_element(process_info(P, initial_call))} || 
        P <- processes()].

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.
