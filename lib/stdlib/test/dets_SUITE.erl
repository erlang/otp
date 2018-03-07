%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(dets_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(config(X,Y), foo).
-define(t, test_server).
-define(privdir(_), "./dets_SUITE_priv").
-define(datadir(_), "./dets_SUITE_data").
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), proplists:get_value(priv_dir, Conf)).
-define(datadir(Conf), proplists:get_value(data_dir, Conf)).
-endif.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, newly_started/1, basic/1,
         open/1, sets/1, bags/1, duplicate_bags/1, access/1, dirty_mark/1,
         dirty_mark2/1, bag_next/1, oldbugs/1,
         truncated_segment_array/1, open_file/1, init_table/1, repair/1,
	 phash/1, fold/1, fixtable/1, match/1, select/1, update_counter/1,
         badarg/1, cache_sets/1, cache_bags/1, cache_duplicate_bags/1,
	 otp_4208/1, otp_4989/1, many_clients/1, otp_4906/1, otp_5402/1,
         simultaneous_open/1, insert_new/1, repair_continuation/1,
         otp_5487/1, otp_6206/1, otp_6359/1, otp_4738/1, otp_7146/1,
         otp_8070/1, otp_8856/1, otp_8898/1, otp_8899/1, otp_8903/1,
         otp_8923/1, otp_9282/1, otp_11245/1, otp_11709/1, otp_13229/1,
         otp_13260/1, otp_13830/1]).

-export([dets_dirty_loop/0]).

-export([histogram/1, sum_histogram/1, ave_histogram/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

%% Internal export.
-export([client/2]).

-import(lists, 
	[append/1, delete/2, duplicate/2, filter/2, foreach/2, keysearch/3, 
	 last/1, map/2, member/2, reverse/1, seq/2, sort/1, usort/1]).

-include_lib("kernel/include/file.hrl").

-define(DETS_SERVER, dets).

%% HEADSZ taken from dets_v9.erl.
-define(HEADSZ_v9, (56+28*4+16)).
-define(NO_KEYS_POS_v9, 36).
-define(CLOSED_PROPERLY_POS, 8).

-define(NOT_PROPERLY_CLOSED,0).
-define(CLOSED_PROPERLY,1).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,15}}].

all() -> 
    [
	basic, open, sets, bags, duplicate_bags, newly_started, open_file,
	init_table, repair, access, oldbugs,
	truncated_segment_array, dirty_mark, dirty_mark2, bag_next,
        phash, fold, fixtable, match, select, update_counter, badarg,
	cache_sets, cache_bags, cache_duplicate_bags, otp_4208, otp_4989,
	many_clients, otp_4906, otp_5402, simultaneous_open,
	insert_new, repair_continuation, otp_5487, otp_6206,
	otp_6359, otp_4738, otp_7146, otp_8070, otp_8856, otp_8898,
	otp_8899, otp_8903, otp_8923, otp_9282, otp_11245, otp_11709,
        otp_13229, otp_13260, otp_13830
    ].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% OTP-3621
newly_started(Config) when is_list(Config) ->
    true = is_alive(),
    {ok, Node} = test_server:start_node(slave1, slave, []),
    [] = rpc:call(Node, dets, all, []),
    test_server:stop_node(Node),
    ok.

basic(Config) when is_list(Config) ->
    Tab = dets_basic_test,
    FName = filename(Tab, Config),

    P0 = pps(),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    ok = dets:insert(Tab,{mazda,japan}),
    ok = dets:insert(Tab,{toyota,japan}),
    ok = dets:insert(Tab,{suzuki,japan}),
    ok = dets:insert(Tab,{honda,japan}),
    ok = dets:insert(Tab,{renault,france}),
    ok = dets:insert(Tab,{citroen,france}),
    ok = dets:insert(Tab,{opel,germany}),
    ok = dets:insert(Tab,{saab,sweden}),
    ok = dets:insert(Tab,{volvo,sweden}),
    [{opel,germany}] = dets:lookup(Tab,opel),
    Japs = dets:traverse(Tab, fun(Obj) ->
                                      case Obj of
                                          {_, japan} -> {continue, Obj};
                                          _ -> continue
                                      end
                              end),
    4  = length(Japs),
    ok = dets:close(Tab),
    file:delete(FName),
    check_pps(P0),
    ok.
    

open(Config) when is_list(Config) ->
    %% Running this test twice means that the Dets server is restarted
    %% twice. dets_sup specifies a maximum of 4 restarts in an hour.
    %% If this becomes a problem, one should consider running this
    %% test on a slave node.

    {Sets, Bags, Dups} = args(Config),
    
    All = Sets ++ Bags ++ Dups,
    delete_files(All),

    Data = make_data(1),

    P0 = pps(),
    Tabs = open_files(1, All),
    initialize(Tabs, Data),
    check(Tabs, Data),

    foreach(fun(Tab) -> ok = dets:close(Tab) end, Tabs),
    %% Now reopen the files
    ?format("Reopening closed files \n", []),
    Tabs = open_files(1, All),
    ?format("Checking contents of reopened files \n", []),
    check(Tabs, Data),
    %% crash the dets server

    ?format("Crashing dets server \n", []),
    process_flag(trap_exit, true),
    Procs = [whereis(?DETS_SERVER) | [dets:info(Tab, pid) || Tab <- Tabs]],
    foreach(fun(Pid) -> exit(Pid, kill) end, Procs),
    timer:sleep(100),
    c:flush(),  %% flush all the EXIT sigs
    timer:sleep(200),

    %% Now reopen the files again
    ?format("Reopening crashed files \n", []),
    open_files(1, All),
    ?format("Checking contents of repaired files \n", []),
    check(Tabs, Data),

    close_all(Tabs),
    delete_files(All),

    {Ports0, Procs0} = P0,
    Test = fun() ->
                   P1 = pps(),
                   {Ports1, Procs1} = P1,
                   show("Old port", Ports0 -- Ports1),
                   show("New port", Ports1 -- Ports0),
                   show("Old procs", Procs0 -- Procs1),
                   show("New procs", Procs1 -- Procs0),
                   io:format("Remaining Dets-pids (should be nil): ~p~n",
                             [find_dets_pids()]),
                   true = Ports1 =:= Ports0,
                   %% The dets_server process has been restarted:
                   [_] = Procs0 -- Procs1,
                   [_] = Procs1 -- Procs0,
                   ok
           end,
    case catch Test() of
        ok -> ok;
        _ ->
            timer:sleep(500),
            ok = Test()
    end.
    
check(Tabs, Data) ->
    foreach(fun(Tab) ->
		    Kp = dets:info(Tab, keypos),
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

%% Perform traversal and match testing on set type dets tables.
sets(Config) when is_list(Config) ->
    {Sets, _, _} = args(Config),

    Data = make_data(1),
    delete_files(Sets),
    P0 = pps(),
    Tabs = open_files(1, Sets),
    Bigger = [{17,q,w,w}, {48,q,w,w,w,w,w,w}], % 48 requires a bigger buddy
    initialize(Tabs, Data++Bigger++Data), % overwrite
    Len = length(Data),
    foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    size_test(Len, Tabs),
    no_keys_test(Tabs),
    foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) ->
                    Len = dets:info(Tab, size) end,
            Tabs),
    foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    
    close_all(Tabs),
    delete_files(Sets),
    check_pps(P0),
    ok.

%% Perform traversal and match testing on bag type dets tables.
bags(Config) when is_list(Config) ->
    {_, Bags, _} = args(Config),
    Data = make_data(1, bag),  %% gives twice as many objects
    delete_files(Bags),
    P0 = pps(),
    Tabs = open_files(1, Bags),
    initialize(Tabs, Data++Data),
    Len = length(Data),
    foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    size_test(Len, Tabs),
    no_keys_test(Tabs),
    foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) ->
                    Len = dets:info(Tab, size) end,
            Tabs),
    foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    close_all(Tabs),
    delete_files(Bags),
    check_pps(P0),
    ok.


%% Perform traversal and match testing on duplicate_bag type dets tables.
duplicate_bags(Config) when is_list(Config) ->
    {_, _, Dups} = args(Config),
    Data = make_data(1, duplicate_bag), %% gives twice as many objects
    delete_files(Dups),
    P0 = pps(),
    Tabs = open_files(1, Dups),
    initialize(Tabs, Data),
    Len = length(Data),
    foreach(fun(Tab) -> trav_test(Data, Len, Tab) end, Tabs),
    size_test(Len, Tabs),
    no_keys_test(Tabs),
    foreach(fun(Tab) -> del_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) -> del_obj_test(Tab) end, Tabs),
    initialize(Tabs, Data),
    foreach(fun(Tab) ->
                    Len = dets:info(Tab, size) end,
            Tabs),
    foreach(fun(Tab) -> match_test(Data, Tab) end, Tabs),
    foreach(fun(Tab) -> match_del_test(Tab) end, Tabs),
    close_all(Tabs),
    delete_files(Dups),
    check_pps(P0),
    ok.


access(Config) when is_list(Config) ->
    Args_acc = [[{ram_file, true}, {access, read}],
		[{access, read}]],
    Args = [[{ram_file, true}],
	    []],
    
    {Args_acc_1, _, _} = zip_filename(Args_acc, [], [], Config),
    delete_files(Args_acc_1),
    {Args_1, _, _} = zip_filename(Args, [], [], Config),

    P0 = pps(),
    {error, {file_error,_,enoent}} = dets:open_file('1', hd(Args_acc_1)),

    Tabs = open_files(1, Args_1),
    close_all(Tabs),
    Tabs = open_files(1, Args_acc_1),

    foreach(fun(Tab) ->
                    {error, {access_mode,_}} = dets:insert(Tab, {1,2}),
                    [] = dets:lookup(Tab, 11),
                    '$end_of_table' = dets:first(Tab),
                    {error, {access_mode,_}} = dets:delete(Tab, 22)
            end, Tabs),
    close_all(Tabs),
    delete_files(Args_acc_1),
    check_pps(P0),
    ok.


%% Test that the table is not marked dirty if not written.
dirty_mark(Config) when is_list(Config) ->
    true = is_alive(),
    Tab = dets_dirty_mark_test,
    FName = filename(Tab, Config),
    P0 = pps(),
    dets:open_file(Tab,[{file, FName}]),
    dets:insert(Tab,{mazda,japan}),
    dets:insert(Tab,{toyota,japan}),
    dets:insert(Tab,{suzuki,japan}),
    dets:insert(Tab,{honda,japan}),
    dets:insert(Tab,{renault,france}),
    dets:insert(Tab,{citroen,france}),
    dets:insert(Tab,{opel,germany}),
    dets:insert(Tab,{saab,sweden}),
    dets:insert(Tab,{volvo,sweden}),
    [{opel,germany}] = dets:lookup(Tab,opel),
    ok = dets:close(Tab),
    Call = fun(P,A) ->
		   P ! {self(), A},
		   receive
		       {P, Ans} ->
			   Ans
		   after 5000 ->
			   exit(other_process_dead)
		   end
	   end,
    {ok, Node} = test_server:start_node(dets_dirty_mark,
                                        slave,
                                        [{linked, false},
                                         {args, "-pa " ++
                                              filename:dirname
						(code:which(?MODULE))}]),
    ok = ensure_node(20, Node),
    %% io:format("~p~n",[rpc:call(Node, code, get_path, [])]),
    %% io:format("~p~n",[rpc:call(Node, file, get_cwd, [])]),
    %% io:format("~p~n",[Config]),
    Pid = rpc:call(Node,erlang, spawn,
			 [?MODULE, dets_dirty_loop, []]),
    {ok, Tab} = Call(Pid, [open, Tab, [{file, FName}]]),
    [{opel,germany}] = Call(Pid, [read,Tab,opel]),
    test_server:stop_node(Node),
    {ok, Tab} = dets:open_file(Tab,[{file, FName},
                                    {repair,false}]),
    ok = dets:close(Tab),
    file:delete(FName),
    check_pps(P0),
    ok.

%% Test that the table is flushed when auto_save is in effect.
dirty_mark2(Config) when is_list(Config) ->
    true = is_alive(),
    Tab = dets_dirty_mark2_test,
    FName = filename(Tab, Config),
    P0 = pps(),
    dets:open_file(Tab,[{file, FName}]),
    dets:insert(Tab,{toyota,japan}),
    dets:insert(Tab,{suzuki,japan}),
    dets:insert(Tab,{honda,japan}),
    dets:insert(Tab,{renault,france}),
    dets:insert(Tab,{citroen,france}),
    dets:insert(Tab,{opel,germany}),
    dets:insert(Tab,{saab,sweden}),
    dets:insert(Tab,{volvo,sweden}),
    [{opel,germany}] = dets:lookup(Tab,opel),
    ok = dets:close(Tab),
    Call = fun(P,A) ->
		   P ! {self(), A},
		   receive
		       {P, Ans} ->
			   Ans
		   after 5000 ->
			   exit(other_process_dead)
		   end
	   end,
    {ok, Node} = test_server:start_node(dets_dirty_mark2,
                                        slave,
                                        [{linked, false},
                                         {args, "-pa " ++
                                              filename:dirname
						(code:which(?MODULE))}]),
    ok = ensure_node(20, Node),
    Pid = rpc:call(Node,erlang, spawn,
                   [?MODULE, dets_dirty_loop, []]),
    {ok, Tab} = Call(Pid, [open, Tab, [{file, FName},{auto_save,1000}]]),
    ok = Call(Pid, [write,Tab,{mazda,japan}]),
    timer:sleep(2100),
    %% Read something, just to give auto save time to finish.
    [{opel,germany}] = Call(Pid, [read,Tab,opel]),
    test_server:stop_node(Node),
    {ok, Tab} = dets:open_file(Tab, [{file, FName}, {repair,false}]),
    ok = dets:close(Tab),
    file:delete(FName),
    check_pps(P0),
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
	    dets_dirty_loop();
        {From, [close, Name]} ->
            Ret = dets:close(Name),
            From ! {self(), Ret},
            dets_dirty_loop()
    end.


%% Check that bags and next work as expected.
bag_next(Config) when is_list(Config) ->
    Tab = dets_bag_next_test,
    FName = filename(Tab, Config),

    %% first and next crash upon error
    dets:open_file(Tab,[{file, FName}, {type, bag}]),
    ok = dets:insert(Tab, [{1,1},{2,2},{3,3},{4,4}]),
    FirstKey = dets:first(Tab),
    NextKey = dets:next(Tab, FirstKey),
    [FirstObj | _] = dets:lookup(Tab, FirstKey),
    [NextObj | _] = dets:lookup(Tab, NextKey),
    {ok, FirstPos} = dets:where(Tab, FirstObj),
    {ok, NextPos} = dets:where(Tab, NextObj),
    crash(FName, NextPos+12),
    {'EXIT',BadObject1} = (catch dets:next(Tab, FirstKey)),
    bad_object(BadObject1, FName),
    crash(FName, FirstPos+12),
    {'EXIT',BadObject2} = (catch dets:first(Tab)),
    bad_object(BadObject2, FName),
    dets:close(Tab),
    file:delete(FName),

    P0 = pps(),
    dets:open_file(Tab,[{file, FName}, {type, bag}]),
    dets:insert(Tab,{698,hopp}),
    dets:insert(Tab,{186,hopp}),
    dets:insert(Tab,{hej,hopp}),
    dets:insert(Tab,{186,plopp}),
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
    ok = Loop(4,dets:first(Tab),Loop),
    dets:close(Tab),
    file:delete(FName),
    check_pps(P0),
    ok.

oldbugs(Config) when is_list(Config) ->
    FName = filename(dets_suite_oldbugs_test, Config),
    P0 = pps(),
    {ok, ob} = dets:open_file(ob, [{type, bag}, {file, FName}]),
    ok = dets:insert(ob, {1, 2}),
    ok = dets:insert(ob, {1,3}),
    ok = dets:insert(ob, {1, 2}),
    2 = dets:info(ob, size),  %% assertion
    ok = dets:close(ob),
    file:delete(FName),
    check_pps(P0),
    ok.

%% Test that a file where the segment array has been truncated
%% is possible to repair.
truncated_segment_array(Config) when is_list(Config) ->
    TabRef = dets_suite_truncated_segment_array_test,
    Fname = filename(TabRef, Config),
    %% Create file that needs to be repaired
    file:delete(Fname),
    P0 = pps(),
    {ok, TabRef} = dets:open_file(TabRef, [{file, Fname}]),
    ok = dets:close(TabRef),
    
    %% Truncate the file
    HeadSize = headsz(),
    truncate(Fname, HeadSize + 10),
    
    %% Open the truncated file
    io:format("Expect repair:~n"),
    {ok, TabRef} = dets:open_file(TabRef,
					[{file, Fname}, {repair, true}]),
    ok = dets:close(TabRef),
    file:delete(Fname),
    check_pps(P0),
    ok.

%% Test open_file/1.
open_file(Config) when is_list(Config) ->
    T = open_v9,
    Fname = filename(T, Config),
    {ok, _} = dets:open_file(T, [{file,Fname}]),
    9 = dets:info(T, version), % Backwards compatibility.
    true = [self()] =:= dets:info(T, users),
    {ok, _} = dets:open_file(T, [{file,Fname}]),
    true = [self(),self()] =:= dets:info(T, users),
    ok = dets:close(T),
    true = [self()] =:= dets:info(T, users),
    ok = dets:close(T),
    undefined = ets:info(T, users),
    file:delete(Fname),

    open_1(Config).

open_1(Config) ->
    TabRef = open_file_1_test,
    Fname = filename(TabRef, Config),
    file:delete(Fname),

    P0 = pps(),
    {error,{file_error,Fname,enoent}} = dets:open_file(Fname),
    
    ok = file:write_file(Fname, duplicate(100,65)),
    {error,{not_a_dets_file,Fname}} = dets:open_file(Fname),
    file:delete(Fname),

    HeadSize = headsz(),
    {ok, TabRef} = dets:open_file(TabRef, [{file, Fname}]),
    ok = dets:close(TabRef),
    truncate(Fname, HeadSize + 10),
    true = dets:is_dets_file(Fname),
    io:format("Expect repair:~n"),
    {ok, Ref} = dets:open_file(Fname), % repairing
    ok = dets:close(Ref),
    file:delete(Fname),

    %% truncated file header, invalid type
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = ins(TabRef, 3000),
    ok = dets:close(TabRef),
    TypePos = 12,
    crash(Fname, TypePos),
    {error, {invalid_type_code,Fname}} = dets:open_file(Fname),
    truncate(Fname, HeadSize - 10),
    {error,{not_a_dets_file,Fname}} = dets:open_file(Fname),
    {error,{not_a_dets_file,Fname}} =
        dets:open_file(TabRef, [{file,Fname}]),
    file:delete(Fname),

    {error,{file_error,{foo,bar},_}} = dets:is_dets_file({foo,bar}),
    check_pps(P0),
    ok.

%% Test initialize_table/2 and from_ets/2.
init_table(Config) when is_list(Config) ->
    %% Objects are returned in "time order".
    T = init_table_v9,
    Fname = filename(T, Config),
    file:delete(Fname),
    L = [{1,a},{2,b},{1,c},{2,c},{1,c},{2,a},{1,b}],
    Input = init([L]),
    {ok, _} = dets:open_file(T, [{file,Fname},{type,duplicate_bag}]),
    ok = dets:init_table(T, Input),
    [{1,a},{1,c},{1,c},{1,b}] = dets:lookup(T, 1),
    [{2,b},{2,c},{2,a}] = dets:lookup(T, 2),
    ok = dets:close(T),
    file:delete(Fname),

    init_table_1(Config),
    fast_init_table(Config).

init_table_1(Config) ->
    TabRef = init_table_test,
    Fname = filename(TabRef, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{auto_save,120000}],
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', _} =
	(catch dets:init_table(TabRef, fun(foo) -> bar end)),
    dets:close(TabRef),
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', _} = (catch dets:init_table(TabRef, fun() -> foo end)),
    dets:close(TabRef),
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', {badarg, _}} = (catch dets:init_table(TabRef, nofun)),
    {'EXIT', {badarg, _}} =
	(catch dets:init_table(TabRef, fun(_X) -> end_of_input end, 
			       [{foo,bar}])),
    dets:close(TabRef),
    {ok, _} = dets:open_file(TabRef, Args),
    away = (catch dets:init_table(TabRef, fun(_) -> throw(away) end)),
    dets:close(TabRef),
    {ok, _} = dets:open_file(TabRef, Args),
    {error, {init_fun, fopp}} =
	dets:init_table(TabRef, fun(read) -> fopp end),
    dets:close(TabRef),

    {ok, _} = dets:open_file(TabRef, Args),
    dets:safe_fixtable(TabRef, true),
    {error, {fixed_table, TabRef}} = dets:init_table(TabRef, init([])),
    dets:safe_fixtable(TabRef, false),
    ET = ets:new(foo,[]),
    ok = dets:from_ets(TabRef, ET),
    [] = get_all_objects(TabRef),
    [] = get_all_objects_fast(TabRef),
    true = ets:insert(ET, {1,a}),
    true = ets:insert(ET, {2,b}),
    ok = dets:from_ets(TabRef, ET),
    [{1,a},{2,b}] = sort(get_all_objects(TabRef)),
    [{1,a},{2,b}] = sort(get_all_objects_fast(TabRef)),
    true = ets:delete(ET),
    120000 = dets:info(TabRef, auto_save),
    ok = dets:close(TabRef),

    {ok, _} = dets:open_file(TabRef, [{access,read} | Args]),
    {error, {access_mode, Fname}} = dets:init_table(TabRef, init([])),
    ok = dets:close(TabRef),

    {ok, _} = dets:open_file(TabRef, Args),
    {error, invalid_objects_list} =
	(catch dets:init_table(TabRef, init([[{1,2},bad,{3,4}]]))),
    _ = dets:close(TabRef),
    file:delete(Fname),

    L1 = [[{1,a},{2,b}],[],[{3,c}],[{4,d}],[]],
    bulk_init(L1, set, 4, Config),
    L2 = [[{1,a},{2,b}],[],[{2,q},{3,c}],[{4,d}],[{4,e},{2,q}]],
    bulk_init(L2, set, 4, Config),
    bulk_init(L2, bag, 6, Config),
    bulk_init(L2, duplicate_bag, 7, Config),
    bulk_init(L1, set, 4, 512, Config),
    bulk_init([], set, 0, 10000, Config),
    file:delete(Fname),

    %% Initiate a file that contains a lot of objects.
    {ok, _} = dets:open_file(TabRef, [{min_no_slots,10000} | Args]),
    ok = ins(TabRef, 6000),
    Fun = init_fun(0, 10000),
    ok = dets:init_table(TabRef, Fun,{format,term}),
    All = sort(get_all_objects(TabRef)),
    FAll = get_all_objects_fast(TabRef),
    true = All =:= sort(FAll),
    true = length(All) =:= 10000,
    ok = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, [{min_no_slots,4000} | Args]),
    ok = ins(TabRef, 6000),
    FileSize1 = dets:info(TabRef, file_size),
    Fun2 = init_fun(0, 4000),
    ok = dets:init_table(TabRef, Fun2),
    FileSize2 = dets:info(TabRef, file_size),
    ok = dets:close(TabRef),
    true = FileSize1 > FileSize2,
    file:delete(Fname),

    check_pps(P0),
    ok.

bulk_init(Ls, Type, N, Config) ->
    bulk_init(Ls, Type, N, 256, Config).

bulk_init(Ls, Type, N, Est, Config) ->
    T = init_table_test,
    Fname = filename(T, Config),
    file:delete(Fname),
    Input = init(Ls),
    Args = [{ram_file,false}, {type,Type},{keypos,1},{file,Fname},
	    {estimated_no_objects, Est}],
    {ok, T} = dets:open_file(T, Args),
    ok = dets:init_table(T, Input),
    All = sort(get_all_objects(T)),
    FAll = get_all_objects_fast(T),
    true = All =:= sort(FAll),
    true = length(All) =:= N,
    true = dets:info(T, size) =:= N,
    ok = dets:close(T),
    
    {ok, T} = dets:open_file(T, Args),
    All2 = sort(get_all_objects(T)),
    FAll2 = get_all_objects_fast(T),
    true = All =:= All2,
    true = All =:= sort(FAll2),
    ok = dets:close(T),
    file:delete(Fname).

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
    TabRef = init_table_test,
    Fname = filename(TabRef, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{auto_save,120000}],

    Source = init_table_test_source,
    SourceFname = filename(Source, Config),
    file:delete(SourceFname),
    SourceArgs = [{file,SourceFname},{auto_save,120000}],

    {ok, Source} = dets:open_file(Source, SourceArgs),
    
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', _} =
	(catch dets:init_table(TabRef, fun(foo) -> bar end, {format,bchunk})),
    dets:close(TabRef),
    file:delete(Fname),
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', _} = (catch dets:init_table(TabRef, fun() -> foo end,
					       {format,bchunk})),
    dets:close(TabRef),
    file:delete(Fname),
    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', {badarg, _}} =
	(catch dets:init_table(TabRef, nofun, {format,bchunk})),
    dets:close(TabRef),
    {ok, _} = dets:open_file(TabRef, Args),
    away = (catch dets:init_table(TabRef, fun(_) -> throw(away) end,
					{format,bchunk})),
    dets:close(TabRef),
    file:delete(Fname),
    {ok, _} = dets:open_file(TabRef, Args),
    {error, {init_fun, fopp}} =
	dets:init_table(TabRef, fun(read) -> fopp end, {format,bchunk}),
    dets:close(TabRef),
    file:delete(Fname),
    {ok, _} = dets:open_file(TabRef, Args),
    dets:safe_fixtable(TabRef, true),
    {error, {fixed_table, TabRef}} =
	dets:init_table(TabRef, init([]), {format,bchunk}),
    dets:safe_fixtable(TabRef, false),
    ok = dets:close(TabRef),

    {ok, _} = dets:open_file(TabRef, [{access,read} | Args]),
    {error, {access_mode, Fname}} =
	dets:init_table(TabRef, init([]), {format,bchunk}),
    ok = dets:close(TabRef),

    {ok, _} = dets:open_file(TabRef, Args),
    {error, {init_fun,{1,2}}} =
	dets:init_table(TabRef, init([[{1,2},bad,{3,4}]]), {format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    {error, {init_fun, end_of_input}} =
	dets:init_table(TabRef, init([]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    {'EXIT', {badarg, _}} =
	(catch dets:init_table(TabRef, init([]),{format,foppla})),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    ok = ins(TabRef, 100),

    [BParms | Objs] = collect_bchunk(TabRef, init_bchunk(TabRef)),
    Parms = binary_to_term(BParms),
    {error, {init_fun, <<"foobar">>}} =
	dets:init_table(TabRef, init([[<<"foobar">>]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    Parms1 = setelement(1, Parms, foobar),
    BParms1 = term_to_binary(Parms1),
    {error, {init_fun, BParms1}} =
	dets:init_table(TabRef, init([[BParms1 | Objs]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    [{Sz1,No1} | NoColls17] = element(tuple_size(Parms), Parms),
    Parms2 = setelement(tuple_size(Parms), Parms, [{Sz1,No1+1} | NoColls17]),
    BParms2 = term_to_binary(Parms2),
    {error, invalid_objects_list} =
	dets:init_table(TabRef, init([[BParms2 | Objs]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    [{LSize1,Slot1,Obj1} | ObjsRest] = Objs,
  
    BadSize = byte_size(Obj1)-1,
    <<BadSizeObj:BadSize/binary,_:1/binary>> = Obj1,
    BadObjs = [{LSize1,Slot1,BadSizeObj} | ObjsRest],
    {error, invalid_objects_list} =
	dets:init_table(TabRef, init([[BParms | BadObjs]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    <<Size:32,BigObj0/binary>> = list_to_binary(lists:duplicate(16,Obj1)),
    BigObj = <<(Size*16):32,BigObj0/binary>>,
    BadColl = [BParms, {LSize1+4,Slot1,BigObj} | ObjsRest],
    {error, invalid_objects_list} =
         dets:init_table(TabRef, init([BadColl]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, Args),
    BadObj = <<"foobar">>,
    {error, invalid_objects_list} =
	dets:init_table(TabRef, init([[BParms, BadObj]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    {ok, _} = dets:open_file(TabRef, [{type,bag} | Args]),
    {error, {init_fun, _}} =
	dets:init_table(TabRef, init([[BParms]]),{format,bchunk}),
    _ = dets:close(TabRef),
    file:delete(Fname),

    ok = dets:close(Source),
    file:delete(SourceFname),

    L1 = [{1,a},{2,b},{3,c},{4,d}],
    fast_bulk_init(L1, set, 4, 4, Config),
    L2 = [{1,a},{2,b},{2,q},{3,c},{4,d},{4,e},{2,q}],
    fast_bulk_init(L2, set, 4, 4, Config),
    fast_bulk_init(L2, bag, 6, 4, Config),
    fast_bulk_init(L2, duplicate_bag, 7, 4, Config),
    fast_bulk_init(L1, set, 4, 4, 512, Config),
    fast_bulk_init([], set, 0, 0, 10000, Config),
    file:delete(Fname),

    %% Initiate a file that contains a lot of objects.
    {ok, _} = dets:open_file(Source, [{min_no_slots,10000} | SourceArgs]),
    Fun1 = init_fun(0, 10000),
    ok = dets:init_table(Source, Fun1, {format,term}),
    
    {ok, _} = dets:open_file(TabRef, [{min_no_slots,10000} | Args]),
    ok = ins(TabRef, 6000),
    Fun2 = init_bchunk(Source),
    true =
        dets:is_compatible_bchunk_format(TabRef, 
                                         dets:info(Source, bchunk_format)),
    false = dets:is_compatible_bchunk_format(TabRef, <<"foobar">>),
    ok = dets:init_table(TabRef, Fun2, {format, bchunk}),
    ok = dets:close(Source),
    file:delete(SourceFname),
    All = sort(get_all_objects(TabRef)),
    FAll = get_all_objects_fast(TabRef),
    true = All =:= sort(FAll),
    true = length(All) =:= 10000,
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% Initiate inserts fewer objects than the table contains.
    {ok, _} = dets:open_file(Source, [{min_no_slots,1000} | SourceArgs]),
    ok = ins(Source, 4000),
    
    {ok, _} = dets:open_file(TabRef, [{min_no_slots,1000} | Args]),
    ok = ins(TabRef, 6000),
    FileSize1 = dets:info(TabRef, file_size),
    Fun4 = init_bchunk(Source),
    ok = dets:init_table(TabRef, Fun4, {format, bchunk}),
    ok = dets:close(Source),
    file:delete(SourceFname),
    FileSize2 = dets:info(TabRef, file_size),
    All_2 = sort(get_all_objects(TabRef)),
    FAll_2 = get_all_objects_fast(TabRef),
    true = All_2 =:= sort(FAll_2),
    true = length(All_2) =:= 4000,
    ok = dets:close(TabRef),
    true = FileSize1 > FileSize2,

    %% Bchunk and fixed table.
    {ok, _} = dets:open_file(TabRef, Args),
    NoItems = dets:info(TabRef, no_objects),
    AllObjects1 = sort(get_all_objects_fast(TabRef)),
    dets:safe_fixtable(TabRef, true),
    true = dets:info(TabRef, fixed),
    Cont1 = init_bchunk(TabRef),
    NoDel =
	dets:select_delete(TabRef, [{{'_',{item,'_','_'}},[],[true]}]),
    true = (NoDel > 0),
    AllObjects2 = sort(get_all_objects_fast(TabRef)),
    true = dets:info(TabRef, fixed),
    Cont2 = init_bchunk(TabRef),
    NoItems2 = dets:info(TabRef, no_objects),
    true = (NoItems =:= NoItems2 + NoDel),
    NoDel2 = dets:select_delete(TabRef, [{'_',[],[true]}]),
    true = (NoDel2 > 0),
    AllObjects3 = sort(get_all_objects_fast(TabRef)),
    NoItems3 = dets:info(TabRef, no_objects),
    true = (NoItems3 =:= 0),
    true = dets:info(TabRef, fixed),
    true = (NoItems2 =:= NoItems3 + NoDel2),
    Cont3 = init_bchunk(TabRef),

    BinColl1 = collect_bchunk(TabRef, Cont1),
    BinColl2 = collect_bchunk(TabRef, Cont2),
    BinColl3 = collect_bchunk(TabRef, Cont3),
    dets:safe_fixtable(TabRef, false),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% Now check that the above collected binaries are correct.
    {ok, _} = dets:open_file(TabRef, Args),
    ok = dets:init_table(TabRef, init([BinColl1]),{format,bchunk}),
    true = (AllObjects1 =:= sort(get_all_objects_fast(TabRef))),
    true = (length(AllObjects1) =:= dets:info(TabRef, no_objects)),
    ok = dets:init_table(TabRef, init([BinColl2]),{format,bchunk}),
    true = (AllObjects2 =:= sort(get_all_objects_fast(TabRef))),
    true = (length(AllObjects2) =:= dets:info(TabRef, no_objects)),
    ok = dets:init_table(TabRef, init([BinColl3]),{format,bchunk}),
    true = (AllObjects3 =:= sort(get_all_objects_fast(TabRef))),
    true = (length(AllObjects3) =:= dets:info(TabRef, no_objects)),
    ok = dets:close(TabRef),
    file:delete(Fname),
    check_pps(P0),
    ok.

fast_bulk_init(L, Type, N, NoKeys, Config) ->
    fast_bulk_init(L, Type, N, NoKeys, 256, Config).

fast_bulk_init(L, Type, N, NoKeys, Est, Config) ->
    T = init_table_test,
    Fname = filename(T, Config),
    file:delete(Fname),

    Args0 = [{ram_file,false}, {type,Type},{keypos,1},
	    {estimated_no_objects, Est}],
    Args = [{file,Fname} | Args0],
    S = init_table_test_source,
    SFname = filename(S, Config),
    file:delete(SFname),
    SArgs = [{file,SFname} | Args0],

    {ok, S} = dets:open_file(S, SArgs),
    ok = dets:insert(S, L),

    Input = init_bchunk(S),
    {ok, T} = dets:open_file(T, Args),
    ok = dets:init_table(T, Input, [{format,bchunk}]),
    All = sort(get_all_objects(T)),
    FAll = get_all_objects_fast(T),
    true = All =:= sort(FAll),
    true = length(All) =:= N,
    true = dets:info(T, size) =:= N,
    true = dets:info(T, no_keys) =:= NoKeys,
    ok = dets:close(T),
    
    {ok, T} = dets:open_file(T, Args),
    All2 = sort(get_all_objects(T)),
    FAll2 = get_all_objects_fast(T),
    true = All =:= All2,
    true = All =:= sort(FAll2),
    ok = dets:close(T),
    file:delete(Fname),

    ok = dets:close(S),
    file:delete(SFname),
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

%% Test open_file and repair.
repair(Config) when is_list(Config) ->
    %% The short lived format 9(a).
    %% Not very throughly tested here.
    A9 = a9,
    Version9aS = filename:join(?datadir(Config), "version_9a.dets"),
    Version9aT = filename('v9a.dets', Config),
    {ok, _} = file:copy(Version9aS, Version9aT),
    {ok, A9} = dets:open_file(A9, [{file,Version9aT}]),
    undefined = dets:info(A9, bchunk_format),
    [{1,a},{2,b},{3,c}] = sort(dets:match_object(A9, '_')),
    ok = dets:insert(A9, {4,d}),
    ok = dets:close(A9),
    {ok, A9} = dets:open_file(A9, [{file,Version9aT}]),
    {error, old_version} = dets:bchunk(A9, start),
    ok = dets:close(A9),
    io:format("Expect forced repair:~n"),
    {ok, A9} = dets:open_file(A9, [{file,Version9aT},{repair,force}]),
    {_, _} = dets:bchunk(A9, start),
    ok = dets:close(A9),
    file:delete(Version9aT),

    repair_1(Config).

repair_1(Config) ->
    TabRef = repair_test,
    Fname = filename(TabRef, Config),
    file:delete(Fname),
    HeadSize = headsz(),

    P0 = pps(),
    {'EXIT', {badarg, _}} =
	(catch dets:open_file(TabRef, [{min_no_slots,1000},
				       {max_no_slots,500}])),
    {error,{file_error,hoppla,enoent}} = dets:file_info(hoppla),
    {error,{file_error,Fname,enoent}} =
	dets:open_file(TabRef, [{file, Fname}, {access, read}]),

    %% compacting, and some kind of test that free lists are saved OK on file
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    0 = dets:info(TabRef, size),
    ok = ins(TabRef, 30000),
    ok = del(TabRef, 30000, 3),
    ok = dets:close(TabRef),
    {error, {access_mode,Fname}} =
        dets:open_file(foo, [{file,Fname},{repair,force},{access,read}]),
    {ok, Ref3} = dets:open_file(Fname), % no repair!
    20000 = dets:info(Ref3, size),
    20000 = dets:foldl(fun(_, N) -> N+1 end, 0, Ref3),
    20000 = count_objects_quite_fast(Ref3), % actually a test of match
    no_keys_test(Ref3),
    ok = dets:close(Ref3),
    {error,{keypos_mismatch,Fname}} =
	dets:open_file(TabRef, [{file, Fname},{keypos,17}]),
    {error,{type_mismatch,Fname}} =
	dets:open_file(TabRef, [{file, Fname},{type,duplicate_bag}]),

    %% make one of the temporary files unwritable
    TmpFile = Fname ++ ".TMP.1",
    file:delete(TmpFile),
    {ok, TmpFd} = file:open(TmpFile, [read,write]),
    ok = file:close(TmpFd),
    unwritable(TmpFile),
    {error,{file_error,TmpFile,eacces}} = dets:fsck(Fname),
    {ok, _} = dets:open_file(TabRef,
                             [{repair,false},{file, Fname}]),
    20000 = length(get_all_objects(TabRef)),
    _ = histogram(TabRef, silent),
    20000 = length(get_all_objects_fast(TabRef)),
    ok = dets:close(TabRef),
    writable(TmpFile),
    file:delete(TmpFile),

    truncate(Fname, HeadSize + 10),
    {error,{not_closed, Fname}} =
	dets:open_file(TabRef, [{file, Fname}, {access, read}]),
    {error,{not_closed, Fname}} =
	dets:open_file(TabRef, [{file, Fname}, {access, read}, 
                                {repair,force}]),
    {error,{needs_repair, Fname}} =
	dets:open_file(TabRef, [{file, Fname}, {repair, false}]),
    file:delete(Fname),

    %% truncated file header
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = ins(TabRef, 100),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% FIXME.
    %% will fail unless the slots are properly sorted when repairing (v8)
    BArgs = [{file, Fname},{type,duplicate_bag},
	     {delayed_write,{3000,10000}}],
    {ok, TabRef} = dets:open_file(TabRef, BArgs),
    Seq = seq(1, 500),
    Small = map(fun(X) -> {X,X} end, Seq),
    Big = map(fun(X) -> erlang:make_tuple(20, X) end, Seq),
    ok = dets:insert(TabRef, Small),
    ok = dets:insert(TabRef, Big),
    ok = dets:insert(TabRef, Small),
    ok = dets:insert(TabRef, Big),
    All = sort(safe_get_all_objects(TabRef)),
    ok = dets:close(TabRef),
    io:format("Expect forced repair:~n"),
    {ok, _} =
         dets:open_file(TabRef, [{repair,force},{min_no_slots,2000} | BArgs]),

    {MinNoSlots,_,MaxNoSlots} = dets:info(TabRef, no_slots),
    ok = dets:close(TabRef),
    io:format("Expect compaction:~n"),
    {ok, _} =
        dets:open_file(TabRef, [{repair,force},
                                {min_no_slots,MinNoSlots},
                                {max_no_slots,MaxNoSlots} | BArgs]),
    All2 = get_all_objects(TabRef),
    true = All =:= sort(All2),
    FAll2 = get_all_objects_fast(TabRef),
    true = All =:= sort(FAll2),
    true = length(All) =:= dets:info(TabRef, size),
    no_keys_test(TabRef),
    Fun = fun(X) -> 4 = length(dets:lookup(TabRef, X)) end,
    foreach(Fun, Seq),
    _ = histogram(TabRef, silent),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% object bigger than segments, the "hole" is taken care of
    {ok, TabRef} = dets:open_file(TabRef, [{file, Fname}]),
    Tuple = erlang:make_tuple(1000, foobar), % > 2 kB
    ok = dets:insert(TabRef, Tuple),
    %% at least one full segment (objects smaller than 2 kB):
    ins(TabRef, 2000),
    ok = dets:close(TabRef),

    {ok, _} =
        dets:open_file(TabRef, [{repair,false},{file, Fname}]),
    {ok, ObjPos} = dets:where(TabRef, {66,{item,number,66}}),
    ok = dets:close(TabRef),
    %% Damaged object.
    Pos = 12, % v9: compaction fails, proper repair follows
    crash(Fname, ObjPos+Pos),
    io:format(
	    "Expect forced repair (possibly after attempted compaction):~n"),
    {ok, _} = dets:open_file(TabRef, [{repair,force},{file, Fname}]),
    true = dets:info(TabRef, size) < 2001,
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% The file is smaller than the padded object.
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = dets:insert(TabRef, Tuple),
    ok = dets:close(TabRef),
    io:format("Expect forced repair or compaction:~n"),
    {ok, _} =
	dets:open_file(TabRef, [{repair,force},{file, Fname}]),
    true = 1 =:= dets:info(TabRef, size),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% Damaged free lists.
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = ins(TabRef, 300),
    ok = dets:sync(TabRef),
    ok = del(TabRef, 300, 3),
    %% FileSize is approximately where the free lists will be written.
    FileSize = dets:info(TabRef, memory),
    ok = dets:close(TabRef),
    crash(Fname, FileSize+20),
    %% Used to return bad_freelists, but that changed in OTP-9622
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% File not closed, opening with read and read_write access tried.
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = ins(TabRef, 300),
    ok = dets:close(TabRef),
    crash(Fname, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    {error, {not_closed, Fname}} =
       dets:open_file(foo, [{file,Fname},{repair,force},
                            {access,read}]),
    {error, {not_closed, Fname}} =
       dets:open_file(foo, [{file,Fname},{repair,true},
                            {access,read}]),
    io:format("Expect repair:~n"),
    {ok, TabRef} =
       dets:open_file(TabRef, [{file,Fname},{repair,true},
                               {access,read_write}]),
    ok = dets:close(TabRef),
    crash(Fname, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    io:format("Expect forced repair:~n"),
    {ok, TabRef} =
       dets:open_file(TabRef, [{file,Fname},{repair,force},
                               {access,read_write}]),
    ok = dets:close(TabRef),
    file:delete(Fname),

    %% The size of an object is huge.
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname}]),
    ok = dets:insert(TabRef, [{1,2,3},{2,3,4}]),
    {ok, ObjPos2} = dets:where(TabRef, {1,2,3}),
    ok = dets:close(TabRef),
    crash(Fname, ObjPos2, 255),
    io:format("Expect forced repair:~n"),
    {ok, TabRef} = dets:open_file(TabRef, [{file,Fname},{repair,force}]),
    ok = dets:close(TabRef),
    file:delete(Fname),

    check_pps(P0),
    ok.


%% Test version 9(b) with erlang:phash/2 as hash function.
phash(Config) when is_list(Config) ->
    T = phash,
    Phash_v9bS = filename:join(?datadir(Config), "version_9b_phash.dat"),
    Fname = filename('v9b.dets', Config),
    {ok, _} = file:copy(Phash_v9bS, Fname),
    
    %% Deleting all objects changes the hash function. 
    %% A feature... (it's for free)
    {ok, T} = dets:open_file(T, [{file, Fname}]),
    phash = dets:info(T, hash),
    dets:delete_all_objects(T),
    phash2 = dets:info(T, hash),
    [] = get_all_objects(T),
    [] = get_all_objects_fast(T),
    ok = dets:close(T),

    %% The hash function is kept when compacting a table.
    {ok, _} = file:copy(Phash_v9bS, Fname),
    io:format("Expect compaction:~n"),
    {ok, T} = dets:open_file(T, [{file, Fname},{repair,force}]),
    phash = dets:info(T, hash),
    [{1,a},{2,b},{3,c},{4,d},{5,e}] =
	lists:sort(dets:lookup_keys(T, [1,2,3,4,5])),
    ok = dets:close(T),

    %% The hash function is updated when repairing a table (no cost).
    {ok, _} = file:copy(Phash_v9bS, Fname),
    crash(Fname, ?CLOSED_PROPERLY_POS+3, 0),
    io:format("Expect repair:~n"),
    {ok, T} = dets:open_file(T, [{file, Fname}]),
    phash2 = dets:info(T, hash),
    [{1,a},{2,b},{3,c},{4,d},{5,e}] =
	lists:sort(dets:lookup_keys(T, [1,2,3,4,5])),
    ok = dets:close(T),
    
    %% One cannot use the bchunk format when copying between a phash
    %% table and a phash2 table. (There is no test for the case an
    %% Erlang/OTP R9 (or later) node (using phash2) copies a table to
    %% an Erlang/OTP R8 node (using phash).) See also the comment on
    %% HASH_PARMS in dets_v9.erl.
    {ok, _} = file:copy(Phash_v9bS, Fname),
    {ok, T} = dets:open_file(T, [{file, Fname}]),
    Type = dets:info(T, type),
    KeyPos = dets:info(T, keypos),
    Input = init_bchunk(T),    
    T2 = phash_table,
    Fname2 = filename(T2, Config),
    Args = [{type,Type},{keypos,KeyPos},{file,Fname2}],
    {ok, T2} = dets:open_file(T2, Args),
    {error, {init_fun, _}} =
	dets:init_table(T2, Input, {format,bchunk}),
    _ = dets:close(T2),
    ok = dets:close(T),
    file:delete(Fname2),

    file:delete(Fname),
    ok.

%% Test foldl, foldr, to_ets.
fold(Config) when is_list(Config) ->
    T = test_table,
    N = 100,
    Fname = filename(T, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname}, {estimated_no_objects, N}],
    {ok, _} = dets:open_file(T, Args),

    ok = ins(T, N),

    Ets = ets:new(to_ets, [public]),
    dets:to_ets(T, Ets),
    true = N =:= ets:info(Ets, size),
    ets:delete(Ets),

    Ets2 = ets:new(to_ets, [private]),
    dets:to_ets(T, Ets2),
    true = N =:= ets:info(Ets2, size),
    ets:delete(Ets2),

    {'EXIT', {badarg, _}} = (catch dets:to_ets(T, not_an_ets_table)),

    F0 = fun(X, A) -> [X | A] end,
    true = N =:= length(dets:foldl(F0, [], T)),
    true = N =:= length(dets:foldr(F0, [], T)),

    F1 = fun(_X, _A) -> throw(away) end, 
    away = (catch dets:foldl(F1, [], T)),
    away = (catch dets:foldr(F1, [], T)),

    F2 = fun(X, A) -> X + A end, 
    {'EXIT', _} = (catch dets:foldl(F2, [], T)),
    {'EXIT', _} = (catch dets:foldr(F2, [], T)),

    F3 = fun(_X) -> throw(away) end,
    away = (catch dets:traverse(T, F3)),

    F4 = fun(X) -> X + 17 end,
    {'EXIT', _} = (catch dets:traverse(T, F4)),

    F5 = fun(_X) -> done end,
    done = dets:traverse(T, F5),

    {ok, ObjPos} = dets:where(T, {66,{item,number,66}}),
    ok = dets:close(T),

    %% Damaged object.
    Pos = 8,
    crash(Fname, ObjPos+Pos),
    {ok, _} = dets:open_file(T, Args),
    io:format("Expect corrupt table:~n"),
    BadObject1 = dets:foldl(F0, [], T),
    bad_object(BadObject1, Fname),
    BadObject2 = dets:close(T),
    bad_object(BadObject2, Fname),

    file:delete(Fname),
    check_pps(P0),
    ok.

%% Add objects to a fixed table.
fixtable(Config) when is_list(Config) ->
    T = fixtable,
    Fname = filename(fixtable, Config),
    file:delete(Fname),
    Args = [{file,Fname}],
    P0 = pps(),
    {ok, _} = dets:open_file(T, Args),

    %% badarg
    check_badarg(catch dets:safe_fixtable(no_table,true),
		       dets, safe_fixtable, [no_table,true]),
    check_badarg(catch dets:safe_fixtable(T,undefined),
		       dets, safe_fixtable, [T,undefined]),

    %% The table is not allowed to grow while the elements are inserted:

    ok = ins(T, 500),
    dets:safe_fixtable(T, false),
    %% Now the table can grow. At the same time as elements are inserted,
    %% the table tries to catch up with the previously inserted elements.
    ok = ins(T, 1000),
    1000 = dets:info(T, size),
    ok = dets:close(T),
    file:delete(Fname),

    {ok, _} = dets:open_file(T, [{type, duplicate_bag} | Args]),
    %% In a fixed table, delete and re-insert an object.
    ok = dets:insert(T, {1, a, b}),
    SysBefore = erlang:timestamp(),
    MonBefore = erlang:monotonic_time(),
    dets:safe_fixtable(T, true),
    MonAfter = erlang:monotonic_time(),
    SysAfter = erlang:timestamp(),
    Self = self(),
    {FixMonTime,[{Self,1}]} = dets:info(T,safe_fixed_monotonic_time),
    {FixSysTime,[{Self,1}]} = dets:info(T,safe_fixed),
    true = is_integer(FixMonTime),
    true = MonBefore =< FixMonTime,
    true = FixMonTime =< MonAfter,
    {FstMs,FstS,FstUs} = FixSysTime,
    true = is_integer(FstMs),
    true = is_integer(FstS),
    true = is_integer(FstUs),
    case erlang:system_info(time_warp_mode) of
	no_time_warp ->
	    true = timer:now_diff(FixSysTime, SysBefore) >= 0,
	    true = timer:now_diff(SysAfter, FixSysTime) >= 0;
	_ ->
	    %% ets:info(Tab,safe_fixed) not timewarp safe...
	    ignore
    end,
    ok = dets:match_delete(T, {1, a, b}),
    ok = dets:insert(T, {1, a, b}),
    {FixMonTime,[{Self,1}]} = dets:info(T,safe_fixed_monotonic_time),
    {FixSysTime,[{Self,1}]} = dets:info(T,safe_fixed),
    dets:safe_fixtable(T, false),
    1 = length(dets:match_object(T, '_')),

    ok = dets:match_delete(T, '_'),
    %% In a fixed table, delete and insert a smaller object.
    ok = dets:insert(T, {1, duplicate(100, e)}),
    dets:safe_fixtable(T, true),
    ok = dets:match_delete(T, {1, '_'}),
    ok = dets:insert(T, {1, a, b}),
    dets:safe_fixtable(T, false),
    1 = length(dets:match_object(T, '_')),

    ok = dets:delete_all_objects(T),
    %% Like the last one, but one extra object.
    ok = dets:insert(T, {1, duplicate(100, e)}),
    ok = dets:insert(T, {2, duplicate(100, e)}),
    dets:safe_fixtable(T, true),
    ok = dets:match_delete(T, {1, '_'}),
    ok = dets:insert(T, {1, a, b}),
    dets:safe_fixtable(T, false),
    2 = length(dets:match_object(T, '_')),
    dets:safe_fixtable(T, true),
    ok = dets:delete_all_objects(T),
    true = dets:info(T, fixed),
    0 = length(dets:match_object(T, '_')),

    ok = dets:close(T),
    file:delete(Fname),
    check_pps(P0),
    ok.

%% Matching objects of a fixed table.
match(Config) when is_list(Config) ->
    T = match,
    Fname = filename(match, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname}, {type, duplicate_bag}, {estimated_no_objects,550}],
    {ok, _} = dets:open_file(T, Args),
    ok = dets:insert(T, {1, a, b}),
    ok = dets:insert(T, {1, b, a}),
    ok = dets:insert(T, {2, a, b}),
    ok = dets:insert(T, {2, b, a}),

    %% match, badarg
    MSpec = [{'_',[],['$_']}],
    check_badarg(catch dets:match(no_table, '_'),
                 dets, match, [no_table,'_']),
    check_badarg(catch dets:match(T, '_', not_a_number),
                 dets, match, [T,'_',not_a_number]),
    {EC1, _} = dets:select(T, MSpec, 1),
    check_badarg(catch dets:match(EC1),
                 dets, match, [EC1]),

    %% match_object, badarg
    check_badarg(catch dets:match_object(no_table, '_'),
                 dets, match_object, [no_table,'_']),
    check_badarg(catch dets:match_object(T, '_', not_a_number),
                 dets, match_object, [T,'_',not_a_number]),
    {EC2, _} = dets:select(T, MSpec, 1),
    check_badarg(catch dets:match_object(EC2),
                 dets, match_object, [EC2]),

    dets:safe_fixtable(T, true),
    {[_, _], C1} = dets:match_object(T, '_', 2),
    {[_, _], C2} = dets:match_object(C1),
    '$end_of_table' = dets:match_object(C2),
    {[_, _], C3} = dets:match_object(T, {1, '_', '_'}, 100),
    '$end_of_table' = dets:match_object(C3),
    '$end_of_table' = dets:match_object(T, {'_'}, default),
    dets:safe_fixtable(T, false),

    dets:safe_fixtable(T, true),
    {[_, _], C30} = dets:match(T, '$1', 2),
    {[_, _], C31} = dets:match(C30),
    '$end_of_table' = dets:match(C31),
    {[_, _], C32} = dets:match(T, {1, '$1', '_'}, 100),
    '$end_of_table' = dets:match(C32),
    '$end_of_table' = dets:match(T, {'_'}, default),
    dets:safe_fixtable(T, false),
    [[1],[1],[2],[2]] = sort(dets:match(T, {'$1','_','_'})),

    %% delete and insert while chunking
    %% (this case almost worthless after changes in OTP-5232)
    ok = dets:match_delete(T, '_'),
    L500 = seq(1, 500),
    Fun = fun(X) -> ok = dets:insert(T, {X, a, b, c, d}) end,
    foreach(Fun, L500),
    %% Select one object DI in L3 below to be deleted.
    {_, TmpCont} = dets:match_object(T, '_', 200),
    {_, TmpCont1} = dets:match_object(TmpCont),
    {TTL, _} = dets:match_object(TmpCont1),
    DI = hd(TTL),
    dets:safe_fixtable(T, true),
    {L1, C20} = dets:match_object(T, '_', 200),
    true = 200 =< length(L1),
    ok = dets:match_delete(T, {'2','_','_'}), % no match
    ok = dets:match_delete(T, DI), % last object
    Tiny = {1050},
    ok = dets:insert(T, Tiny),
    true = member(Tiny, dets:match_object(T, '_')),
    {_L2, C21} = dets:match_object(C20),
    {_L3, _C22} = dets:match_object(C21),
    %% It used to be that Tiny was not visible here, but since the 
    %% scanning of files was changed to inspect the free lists every
    %% now and then it may very well be visible here.
    %% false = member(Tiny, _L3),
    %% DI used to visible here, but the above mentioned modification
    %% has changed that; it may or may not be visible.
    %% true = member(DI, _L3),
    dets:safe_fixtable(T, false),
    true = dets:member(T, 1050),
    true = member(Tiny, dets:match_object(T, '_')),
    false = member(DI, dets:match_object(T, '_')),

    ok = dets:close(T),
    file:delete(Fname),

    N = 100,
    {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ok = ins(T, N),
    Obj = {66,{item,number,66}},
    Spec = {'_','_'},
    {ok, ObjPos} = dets:where(T, Obj),
    ok = dets:close(T),
    %% Damaged object.
    crash(Fname, ObjPos+12),
    {ok, _} = dets:open_file(T, Args),
    io:format("Expect corrupt table:~n"),
    case ins(T, N) of
	      ok ->
                  bad_object(dets:sync(T), Fname);
	      Else ->
		  bad_object(Else, Fname)
	  end,
    io:format("Expect corrupt table:~n"),
    bad_object(dets:match(T, Spec), Fname),
    io:format("Expect corrupt table:~n"),
    bad_object(dets:match_delete(T, Spec), Fname),
    bad_object(dets:close(T), Fname),
    file:delete(Fname),

    {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ok = ins(T, N),
    {ok, ObjPos2} = dets:where(T, Obj),
    ok = dets:close(T),

    %% Damaged size of object.
    CrashPos = 1,
    crash(Fname, ObjPos2+CrashPos),
    {ok, _} = dets:open_file(T, Args),
    case dets:insert_new(T, Obj) of % OTP-12024
        ok ->
            bad_object(dets:sync(T), Fname);
        Else3 ->
            bad_object(Else3, Fname)
    end,
    io:format("Expect corrupt table:~n"),
    case ins(T, N) of
        ok ->
            bad_object(dets:sync(T), Fname);
        Else2 ->
            bad_object(Else2, Fname)
    end,
    %% Just echoes...
    bad_object(dets:match(T, Spec), Fname),
    bad_object(dets:match_delete(T, Spec), Fname),
    bad_object(dets:close(T), Fname),
    file:delete(Fname),

    {ok, _} = dets:open_file(T, [{estimated_no_objects,N} | Args]),
    ok = ins(T, N),
    {ok, ObjPos3} = dets:where(T, Obj),
    ok = dets:close(T),

    %% match_delete finds an error
    CrashPos3 = 16,
    crash(Fname, ObjPos3+CrashPos3),
    {ok, _} = dets:open_file(T, Args),
    bad_object(dets:match_delete(T, Spec), Fname),
    bad_object(dets:close(T), Fname),
    file:delete(Fname),

    %% The key is not fixed, but not all objects with the key are removed.
    {ok, _} = dets:open_file(T, Args),
    ok = dets:insert(T, [{1,a},{1,b},{1,c},{1,a},{1,b},{1,c}]),
    6 = dets:info(T, size),
    ok = dets:match_delete(T, {'_',a}),
    4 = dets:info(T, size),
    [{1,b},{1,b},{1,c},{1,c}] =
	sort(dets:match_object(T,{'_','_'})),
    ok = dets:close(T),
    file:delete(Fname),

    check_pps(P0),
    ok.

%% Selecting objects of a fixed table.
select(Config) when is_list(Config) ->
    T = select,
    Fname = filename(select, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname}, {type, duplicate_bag},{estimated_no_objects,550}],
    {ok, _} = dets:open_file(T, Args),
    ok = dets:insert(T, {1, a, b}),
    ok = dets:insert(T, {1, b, a}),
    ok = dets:insert(T, {2, a, b}),
    ok = dets:insert(T, {2, b, a}),
    ok = dets:insert(T, {3, a, b}),
    ok = dets:insert(T, {3, b, a}),

    %% badarg
    MSpec = [{'_',[],['$_']}],
    check_badarg(catch dets:select(no_table, MSpec),
                 dets, select, [no_table,MSpec]),
    check_badarg(catch dets:select(T, <<17>>),
                 dets, select, [T,<<17>>]),
    check_badarg(catch dets:select(T, []),
                 dets, select, [T,[]]),
    check_badarg(catch dets:select(T, MSpec, not_a_number),
                 dets, select, [T,MSpec,not_a_number]),
    {EC, _} = dets:match(T, '_', 1),
    check_badarg(catch dets:select(EC),
                 dets, select, [EC]),

    AllSpec = [{'_',[],['$_']}],

    dets:safe_fixtable(T, true),
    {[_, _], C1} = dets:select(T, AllSpec, 2),
    {[_, _], C2} = dets:select(C1),
    {[_, _], C2a} = dets:select(C2),
    '$end_of_table' = dets:select(C2a),
    {[_, _], C3} = dets:select(T, [{{1,'_','_'},[],['$_']}], 100),
    '$end_of_table' = dets:select(C3),
    '$end_of_table' = dets:select(T, [{{'_'},[],['$_']}], default),
    dets:safe_fixtable(T, false),
    Sp1 = [{{1,'_','_'},[],['$_']},{{1,'_','_'},[],['$_']},
	   {{2,'_','_'},[],['$_']}],
    [_,_,_,_] = dets:select(T, Sp1),
    Sp2 = [{{1,'_','_'},[],['$_']},{{1,'_','_'},[],['$_']},
	   {{'_','_','_'},[],['$_']}],
    [_,_,_,_,_,_] = dets:select(T, Sp2),

    AllDeleteSpec = [{'_',[],[true]}],
    %% delete and insert while chunking
    %% (this case almost worthless after changes in OTP-5232)
    6 = dets:select_delete(T, AllDeleteSpec),
    L500 = seq(1, 500),
    Fun = fun(X) -> ok = dets:insert(T, {X, a, b, c, d}) end,
    foreach(Fun, L500),
    %% Select one object DI in L3 below to be deleted.
    {_, TmpCont} = dets:match_object(T, '_', 200),
    {_, TmpCont1} = dets:match_object(TmpCont),
    {TTL, _} = dets:match_object(TmpCont1),
    DI = hd(TTL),
    dets:safe_fixtable(T, true),
    {L1, C20} = dets:select(T, AllSpec, 200),
    true = 200 =< length(L1),
    0 = dets:select_delete(T, [{{2,'_','_'},[],[true]}]),
    1 = dets:select_delete(T, [{DI,[],[true]}]), % last object
    Tiny = {1050},
    ok = dets:insert(T, Tiny),
    true = member(Tiny, dets:select(T, AllSpec)),
    {_L2, C21} = dets:select(C20),
    {_L3, _C22} = dets:select(C21),
    %% It used to be that Tiny was not visible here, but since the 
    %% scanning of files was changed to inspect the free lists every
    %% now and then it may very well be visible here.
    %% false = member(Tiny, _L3),
    %% DI used to visible here, but the above mentioned modification
    %% has changed that; it may or may not be visible.
    %% true = member(DI, _L3),
    true = dets:member(T, 1050),
    true = member(Tiny, dets:select(T, AllSpec)),
    false = member(DI, dets:select(T, AllSpec)),
    dets:safe_fixtable(T, false),
    true = dets:member(T, 1050),
    true = member(Tiny, dets:select(T, AllSpec)),
    false = member(DI, dets:select(T, AllSpec)),
    ok = dets:close(T),
    file:delete(Fname),

    %% The key is not fixed, but not all objects with the key are removed.
    {ok, _} = dets:open_file(T, Args),
    ok = dets:insert(T, [{1,a},{1,b},{1,c},{1,a},{1,b},{1,c}]),
    6 = dets:info(T, size),
    2 = dets:select_delete(T, [{{'_',a},[],[true]}]),
    4 = dets:info(T, size),
    [{1,b},{1,b},{1,c},{1,c}] = sort(dets:select(T, AllSpec)),
    ok = dets:close(T),
    file:delete(Fname),

    check_pps(P0),
    ok.

%% Test update_counter/1.
update_counter(Config) when is_list(Config) ->
    T = update_counter,
    Fname = filename(select, Config),
    file:delete(Fname),
    P0 = pps(),

    check_badarg(catch dets:update_counter(no_table, 1, 1),
                 dets, update_counter, [no_table,1,1]),

    Args = [{file,Fname},{keypos,2}],
    {ok, _} = dets:open_file(T, [{type,set} | Args]),
    {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ok = dets:insert(T, {1,a}),
    {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ok = dets:insert(T, {0,1}),
    {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ok = dets:insert(T, {0,1,0}),
    1 = dets:update_counter(T, 1, 1),
    2 = dets:update_counter(T, 1, 1),
    6 = dets:update_counter(T, 1, {3,4}),
    {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, {0,3})),
    ok = dets:close(T),
    file:delete(Fname),

    {ok, _} = dets:open_file(T, [{type,bag} | Args]),
    ok = dets:insert(T, {0,1,0}),
    {'EXIT', {badarg, _}} = (catch dets:update_counter(T, 1, 1)),
    ok = dets:close(T),
    file:delete(Fname),
    check_pps(P0),

    ok.

%% Call some functions with bad arguments.
badarg(Config) when is_list(Config) ->
    T = badarg,
    Fname = filename(select, Config),
    file:delete(Fname),
    P0 = pps(),

    Args = [{file,Fname},{keypos,3}],
    {ok, _} = dets:open_file(T, [{type,set} | Args]),

    %% badargs are tested in match, select and fixtable too.

    %% open
    check_badarg(catch dets:open_file({a,tuple},[]),
                 dets, open_file, [{a,tuple},[]]),
    check_badarg(catch dets:open_file({a,tuple}),
                 dets, open_file,[{a,tuple}]),
    check_badarg(catch dets:open_file(file,[foo]),
                 dets, open_file, [file,[foo]]),
    check_badarg(catch dets:open_file({hej,san},[{type,set}|3]),
                 dets, open_file, [{hej,san},[{type,set}|3]]),

    %% insert
    check_badarg(catch dets:insert(no_table, {1,2}),
                 dets, insert, [no_table,{1,2}]),
    check_badarg(catch dets:insert(no_table, [{1,2}]),
                 dets, insert, [no_table,[{1,2}]]),
    check_badarg(catch dets:insert(T, {1,2}),
                 dets, insert, [T,{1,2}]),
    check_badarg(catch dets:insert(T, [{1,2}]),
                 dets, insert, [T,[{1,2}]]),
    check_badarg(catch dets:insert(T, [{1,2,3} | 3]),
                 dets, insert, [T,[{1,2,3}|3]]),

    %% lookup{_keys}
    check_badarg(catch dets:lookup_keys(T, []),
                 dets, lookup_keys, [badarg,[]]),
    check_badarg(catch dets:lookup(no_table, 1),
                 dets, lookup, [no_table,1]),
    check_badarg(catch dets:lookup_keys(T, [1 | 2]),
                 dets, lookup_keys, [T,[1|2]]),

    %% member
    check_badarg(catch dets:member(no_table, 1),
                 dets, member, [no_table,1]),

    %% sync
    check_badarg(catch dets:sync(no_table),
                 dets, sync, [no_table]),

    %% delete{_keys}
    check_badarg(catch dets:delete(no_table, 1),
                 dets, delete, [no_table,1]),

    %% delete_object
    check_badarg(catch dets:delete_object(no_table, {1,2,3}),
                 dets, delete_object, [no_table,{1,2,3}]),
    check_badarg(catch dets:delete_object(T, {1,2}),
                 dets, delete_object, [T,{1,2}]),
    check_badarg(catch dets:delete_object(no_table, [{1,2,3}]),
                 dets, delete_object, [no_table,[{1,2,3}]]),
    check_badarg(catch dets:delete_object(T, [{1,2}]),
                 dets, delete_object, [T,[{1,2}]]),
    check_badarg(catch dets:delete_object(T, [{1,2,3} | 3]),
                 dets, delete_object, [T,[{1,2,3}|3]]),

    %% first,next,slot
    check_badarg(catch dets:first(no_table),
                 dets, first, [no_table]),
    check_badarg(catch dets:next(no_table, 1),
                 dets, next, [no_table,1]),
    check_badarg(catch dets:slot(no_table, 0),
                 dets, slot, [no_table,0]),

    %% info
    undefined = dets:info(no_table),
    undefined = dets:info(no_table, foo),
    undefined = dets:info(T, foo),

    %% match_delete
    check_badarg(catch dets:match_delete(no_table, '_'),
                 dets, match_delete, [no_table,'_']),

    %% delete_all_objects
    check_badarg(catch dets:delete_all_objects(no_table),
                 dets, delete_all_objects, [no_table]),

    %% select_delete
    MSpec = [{'_',[],['$_']}],
    check_badarg(catch dets:select_delete(no_table, MSpec),
                 dets, select_delete, [no_table,MSpec]),
    check_badarg(catch dets:select_delete(T, <<17>>),
                 dets, select_delete, [T, <<17>>]),

    %% traverse, fold
    TF = fun(_) -> continue end,
    check_badarg(catch dets:traverse(no_table, TF),
                 dets, traverse, [no_table,TF]),
    FF = fun(_, A) -> A end,
    check_badarg(catch dets:foldl(FF, [], no_table),
                 dets, foldl, [FF,[],no_table]),
    check_badarg(catch dets:foldr(FF, [], no_table),
                 dets, foldl, [FF,[],no_table]),

    %% close
    ok = dets:close(T),
    {error, not_owner} = dets:close(T),
    {error, not_owner} = dets:close(T),

    %% init_table
    IF = fun(X) -> X end,
    check_badarg(catch dets:init_table(no_table, IF),
                 dets, init_table, [no_table,IF,[]]),
    check_badarg(catch dets:init_table(no_table, IF, []),
                 dets, init_table, [no_table,IF,[]]),

    %% from_ets
    Ets = ets:new(ets,[]),
    check_badarg(catch dets:from_ets(no_table, Ets),
                 dets, from_ets, [no_table,Ets]),
    ets:delete(Ets),

    {ok, T} = dets:open_file(T, Args),
    {error,incompatible_arguments} =
	dets:open_file(T, [{type,bag} | Args]),
    ok = dets:close(T),

    file:delete(Fname),
    check_pps(P0),
    ok.

%% Test the write cache for sets.
cache_sets(Config) when is_list(Config) ->
    Small = 2,
    cache_sets(Config, {0,0}, false, Small),
    cache_sets(Config, {0,0}, true, Small),
    cache_sets(Config, {5000,5000}, false, Small),
    cache_sets(Config, {5000,5000}, true, Small),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_sets(Config, {0,0}, false, Big),
    cache_sets(Config, {0,0}, true, Big),
    cache_sets(Config, {5000,5000}, false, Big),
    cache_sets(Config, {5000,5000}, true, Big),
    ok.
    
cache_sets(Config, DelayedWrite, Extra, Sz) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    Fname = filename(cache, Config),
    file:delete(Fname),
    P0 = pps(),

    {ok, _} = dets:open_file(T,[{file,Fname}, {type,set},
                                {delayed_write, DelayedWrite}]),

    Dups = 1,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ok = dets:delete(T, Key),
    ok = dets:sync(T),

    %% The values of keys in the same slot as Key are checked.
    OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ok = dets:insert(T, Tuple),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    [Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    true = dets:member(T, Key),

    %% If delay, this happens without file access.
    ok = dets:delete(T,Key),
    ok = dets:insert(T,Tuple),
    ok = dets:insert(T,Tuple),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:sync(T),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),

    %% Key's objects are is on file only, 
    %% key 'toto' in the cache (if there is one).
    ok = dets:delete(T,toto),
    ok = dets:insert(T,[{toto,b},{toto,b}]),
    true = sort([Tuple,{toto,b}]) =:=
        sort(dets:lookup_keys(T, [Key,toto])),
    true = dets:member(T, toto),

    ok = dets:delete(T, Key),
    ok = dets:sync(T),
    false = dets:member(T, Key),
    Size = dets:info(T, size),

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
    ok = dets:sync(T),
    ok = dets:insert(T, Tuple2),
    [Tuple2] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:sync(T),
    [Tuple2] = dets:lookup(T, Key),
    true = dets:member(T, Key),

    ok = dets:insert(T, {3,a}),
    ok = dets:insert(T, {3,b}),
    ok = dets:delete_object(T, {3,c}),
    ok = dets:delete_object(T, {3,d}),
    [{3,b}] = dets:lookup(T, 3),

    ok = dets:delete(T, 3),
    ok = dets:delete_object(T, {3,c}),
    ok = dets:delete_object(T, {3,d}),
    [] = dets:lookup(T, 3),

    OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow a while, if it needs to.
	    All1 = get_all_objects(T),
	    dets:safe_fixtable(T, false),
	    timer:sleep(1000),
	    OtherValues = sort(lookup_keys(T, OtherKeys)),
	    dets:safe_fixtable(T, true),
	    All2 = get_all_objects(T),
	    FAll2 = get_all_objects_fast(T),
	    true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    ct:fail({sets,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ok = dets:close(T),

    file:delete(Fname),
    check_pps(P0),
    ok.
    
%% Test the write cache for bags.
cache_bags(Config) when is_list(Config) ->
    Small = 2,
    cache_bags(Config, {0,0}, false, Small),
    cache_bags(Config, {0,0}, true, Small),
    cache_bags(Config, {5000,5000}, false, Small),
    cache_bags(Config, {5000,5000}, true, Small),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_bags(Config, {0,0}, false, Big),
    cache_bags(Config, {0,0}, true, Big),
    cache_bags(Config, {5000,5000}, false, Big),
    cache_bags(Config, {5000,5000}, true, Big),
    ok.
    
cache_bags(Config, DelayedWrite, Extra, Sz) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    Fname = filename(cache, Config),
    file:delete(Fname),
    P0 = pps(),

    {ok, _} = dets:open_file(T,[{file,Fname}, {type,bag},
                                {delayed_write, DelayedWrite}]),

    Dups = 1,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ok = dets:delete(T, Key),
    ok = dets:sync(T),

    %% The values of keys in the same slot as Key are checked.
    OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ok = dets:insert(T, Tuple),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    [Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    true = dets:member(T, Key),

    %% If delay, this happens without file access. 
    %% (This is no longer true; cache lookup has been simplified.)
    ok = dets:delete(T,Key),
    ok = dets:insert(T,Tuple),
    ok = dets:insert(T,Tuple),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:sync(T),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),

    %% Key's objects are is on file only, 
    %% key toto in the cache (if there is one).
    ok = dets:delete(T,toto),
    false = dets:member(T, toto),
    ok = dets:insert(T,[{toto,b},{toto,b}]),
    true = sort([Tuple,{toto,b}]) =:=
        sort(dets:lookup_keys(T, [Key,toto])),
    true = dets:member(T, toto),

    ok = dets:delete(T, Key),
    ok = dets:sync(T),
    Size = dets:info(T, size),

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
    ok = dets:insert(T, Tuple),
    ok = dets:sync(T),
    [Tuple2] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:insert(T, Tuple),
    ok = dets:sync(T),
    [Tuple2] = dets:lookup(T, Key),
    true = dets:member(T, Key),

    %% A mix of insert and delete.
    ok = dets:delete(T, Key),
    ok = dets:sync(T),
    ok = dets:delete(T, Key),
    ok = dets:insert(T, {Key,foo}),
    ok = dets:insert(T, {Key,bar}),
    [{Key,bar},{Key,foo}] = sort(dets:lookup(T, Key)),
    true = dets:member(T, Key),
    ok = dets:delete_object(T, {Key,foo}),
    ok = dets:insert(T, {Key,kar}),
    [{Key,bar},{Key,kar}] = sort(dets:lookup(T, Key)),
    true = dets:member(T, Key),
    ok = dets:insert(T, [{Key,kar},{Key,kar}]),
    [{Key,bar},{Key,kar}] = sort(dets:lookup(T, Key)),
    true = dets:member(T, Key),
    ok = dets:delete_object(T, {Key,bar}),
    ok = dets:delete_object(T, {Key,kar}),
    [] = dets:lookup(T, Key),
    false = dets:member(T, Key),
    ok = dets:sync(T),
    [] = dets:lookup(T, Key),
    false = dets:member(T, Key),

    OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow for a while, if it needs to.
	    All1 = get_all_objects(T),
	    dets:safe_fixtable(T, false),
	    timer:sleep(1200),
	    OtherValues = sort(lookup_keys(T, OtherKeys)),
	    dets:safe_fixtable(T, true),
	    All2 = get_all_objects(T),
	    FAll2 = get_all_objects_fast(T),
	    true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    ct:fail({bags,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ok = dets:close(T),
    file:delete(Fname),

    %% Second object of a key added and looked up simultaneously.
    R1 = {index_test,1,2,3,4},
    R2 = {index_test,2,2,13,14},
    R3 = {index_test,1,12,13,14},
    {ok, _} = dets:open_file(T,[{type,bag}, {keypos,2},{file,Fname}]),
    ok = dets:insert(T,R1),
    ok = dets:sync(T),
    ok = dets:insert(T,R2),
    ok = dets:sync(T),
    ok = dets:insert(T,R3),
    [R1,R3] = sort(dets:lookup(T,1)),
    true = dets:member(T, 1),
    [R1,R3] = sort(dets:lookup(T,1)),
    true = dets:member(T, 1),
    ok = dets:close(T),
    file:delete(Fname),

    check_pps(P0),
    ok.
    
%% Test the write cache for duplicate bags.
cache_duplicate_bags(Config) when is_list(Config) ->
    Small = 2,
    cache_dup_bags(Config, {0,0}, false, Small),
    cache_dup_bags(Config, {0,0}, true, Small),
    cache_dup_bags(Config, {5000,5000}, false, Small),
    cache_dup_bags(Config, {5000,5000}, true, Small),
    %% Objects of size greater than 2 kB.
    Big = 1200,
    cache_dup_bags(Config, {0,0}, false, Big),
    cache_dup_bags(Config, {0,0}, true, Big),
    cache_dup_bags(Config, {5000,5000}, false, Big),
    cache_dup_bags(Config, {5000,5000}, true, Big).

cache_dup_bags(Config, DelayedWrite, Extra, Sz) ->
    %% Extra = bool(). Insert tuples until the tested key is not alone.
    %% Sz = integer(). Size of the inserted tuples.

    T = cache,
    Fname = filename(cache, Config),
    file:delete(Fname),
    P0 = pps(),

    {ok, _} = dets:open_file(T,[{file,Fname}, {type,duplicate_bag},
                                {delayed_write, DelayedWrite}]),

    Dups = 2,
    {Key, OtherKeys} = 
	if 
	    Extra ->
		%% Insert enough to get three keys in some slot.
		dets:safe_fixtable(T, true),
		insert_objs(T, 1, Sz, Dups);
	    true ->
		{1,[]}
	  end,
    Tuple = erlang:make_tuple(Sz, Key),
    ok = dets:delete(T, Key),
    ok = dets:sync(T),
    false = dets:member(T, Key),

    %% The values of keys in the same slot as Key are checked.
    OtherValues = sort(lookup_keys(T, OtherKeys)),
    
    ok = dets:insert(T, Tuple),
    [Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:insert(T, [Tuple,Tuple]),
    %% If no delay, the cache gets filled immediately, and written.
    [Tuple,Tuple,Tuple] = dets:lookup_keys(T, [Key,a,b,c,d,e,f]),
    true = dets:member(T, Key),

    %% If delay, this happens without file access.
    %% (This is no longer true; cache lookup has been simplified.)
    ok = dets:delete(T,Key),
    ok = dets:insert(T,Tuple),
    ok = dets:insert(T,Tuple),
    [Tuple,Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),
    ok = dets:sync(T),
    [Tuple,Tuple] = dets:lookup(T, Key),
    true = dets:member(T, Key),

    %% One object in the cache, one on the file.
    ok = dets:delete(T,Key),
    ok = dets:insert(T,Tuple),
    ok = dets:sync(T),
    ok = dets:insert(T,Tuple),
    true = dets:member(T, Key), % should not read the file, but it does..

    %% Key's objects are is on file only, 
    %% key toto in the cache (if there is one).
    ok = dets:delete(T,toto),
    ok = dets:insert(T,[{toto,b},{toto,b}]),
    true = sort([Tuple,Tuple,{toto,b},{toto,b}]) =:=
                 sort(dets:lookup_keys(T, [Key,toto])),
    true = dets:member(T, toto),
    Size = dets:info(T, size),

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

    OtherValues = sort(lookup_keys(T, OtherKeys)),
    if
	Extra ->
	    %% Let the table grow for a while, if it needs to.
	    All1 = get_all_objects(T),
	    dets:safe_fixtable(T, false),
	    timer:sleep(1200),
	    OtherValues = sort(lookup_keys(T, OtherKeys)),
	    dets:safe_fixtable(T, true),
	    All2 = get_all_objects(T),
	    FAll2 = get_all_objects_fast(T),
	    true = sort(All2) =:= sort(FAll2),
            case symdiff(All1, All2) of
		{[],[]} ->  ok;
		{X,Y} ->
		    NoBad = length(X) + length(Y),
		    ct:fail({dup_bags,DelayedWrite,Extra,Sz,NoBad})
	    end;
	true ->
	    ok
    end,
    ok = dets:close(T),

    file:delete(Fname),
    check_pps(P0),
    ok.
    
lookup_keys(_T, []) ->
    [];
lookup_keys(T, Keys) ->
    dets:lookup_keys(T, Keys).

del_and_ins(W, T, Size, Obj, Key, N) ->
    case W of
	object -> 
	    ok = dets:delete_object(T, Obj);
	key ->

	    ok = dets:delete(T, Key);
	both ->
	    ok = dets:delete(T, Key),
	    ok = dets:delete_object(T, Obj)
    end,
    Objs = duplicate(N, Obj),
    [] = dets:lookup(T, Key),
    ok = dets:insert(T, Objs),
    Objs = dets:lookup_keys(T, [snurrespratt,Key]),
    true = Size + length(Objs)-2 =:= dets:info(T, size),
    Objs = dets:lookup(T, Key).


insert_objs(T, N, Sz, Dups) ->
    Seq = seq(N,N+255),
    L0 = map(fun(I) -> erlang:make_tuple(Sz, I) end, Seq),
    L = append(duplicate(Dups, L0)),
    ok = dets:insert(T, L),
    case search_slot(T, 0) of
        false ->
            insert_objs(T, N+256, Sz, Dups);
        Keys ->
            Keys
    end.

search_slot(T, I) ->
    case dets:slot(T, I) of
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

%% Test read-only tables and traversal caused crashes.
otp_4208(Config) when is_list(Config) ->
    Tab = otp_4208,
    FName = filename(Tab, Config),
    Expected = sort([{3,ghi,12},{1,abc,10},{4,jkl,13},{2,def,11}]),

    file:delete(FName),
    {ok, Tab} = dets:open_file(Tab, [{file,FName}]),
    ok = dets:insert(Tab, [{1,abc,10},{2,def,11},{3,ghi,12},{4,jkl,13}]),
    Expected = sort(dets:traverse(Tab, fun(X) -> {continue, X} end)),
    ok = dets:close(Tab),

    {ok, Tab} = dets:open_file(Tab, [{access, read},{file,FName}]),
    Expected = sort(dets:traverse(Tab, fun(X) -> {continue, X} end)),
    ok = dets:close(Tab),
    file:delete(FName),
    
    ok.

%% Test read-only tables and growth.
otp_4989(Config) when is_list(Config) ->
    Tab = otp_4989,
    FName = filename(Tab, Config),

    %% Do exactly as in the error report.
    _Ets = ets:new(Tab, [named_table]),
    ets_init(Tab, 100000),
    {ok, Tab} =
        dets:open_file(Tab, [{access, read_write}, {file,FName}, {keypos,2}]),
    ok = dets:from_ets(Tab, Tab),
    ok = dets:close(Tab),
    %% Restore.
     {ok, Tab} =
        dets:open_file(Tab, [{access, read}, {keypos, 2}, {file, FName}]),
    true = ets:delete_all_objects(Tab),
    true = ets:from_dets(Tab, Tab),
    ok = dets:close(Tab),
    ets:delete(Tab),
    file:delete(FName),
    ok.

ets_init(_Tab, 0) ->
    ok;
ets_init(Tab, N) ->
    ets:insert(Tab, {N,N}),
    ets_init(Tab, N - 1).

%% OTP-8898. Truncated Dets file.
otp_8898(Config) when is_list(Config) ->
    Tab = otp_8898,
    FName = filename(Tab, Config),

    Server = self(),

    file:delete(FName),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    [P1,P2,P3] = new_clients(3, Tab),

    Seq = [{P1,[sync]},{P2,[{lookup,1,[]}]},{P3,[{insert,{1,b}}]}],
    atomic_requests(Server, Tab, [[]], Seq),
    true = get_replies([{P1,ok},{P2,ok},{P3,ok}]),
    ok = dets:close(Tab),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    file:delete(FName),

    ok.

%% OTP-8899. Several clients. Updated Head was ignored.
otp_8899(Config) when is_list(Config) ->
    Tab = many_clients,
    FName = filename(Tab, Config),

    Server = self(),

    file:delete(FName),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    [P1,P2,P3,P4] = new_clients(4, Tab),

    MC = [Tab],
    Seq6a = [{P1,[{insert,[{used_to_be_skipped_by,match}]},
                  {lookup,1,[{1,a}]}]},
             {P2,[{verbose,true,MC}]},
             {P3,[{lookup,1,[{1,a}]}]}, {P4,[{verbose,true,MC}]}],
    atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq6a),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),
    [{1,a},{2,b},{3,c},{used_to_be_skipped_by,match}] =
        lists:sort(dets:match_object(Tab, '_')),
    _ = dets:close(Tab),
    file:delete(FName),

    ok.

%% Test several clients accessing a table simultaneously.
many_clients(Config) when is_list(Config) ->
    Tab = many_clients,
    FName = filename(Tab, Config),

    Server = self(),

    file:delete(FName),
    P0 = pps(),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    [P1,P2,P3,P4] = new_clients(4, Tab),

    %% dets:init_table/2 is used for making sure that all processes
    %% start sending requests before the Dets process begins to handle
    %% them; the requests arrive "in parallel".

    %% Four processes accessing the same table at almost the same time.

    %% One key is read, updated, and read again.
    Seq1 = [{P1,[{lookup,1,[{1,a}]}]}, {P2,[{insert,{1,b}}]},
	    {P3,[{lookup,1,[{1,b}]}]}, {P4,[{lookup,1,[{1,b}]}]}],
    atomic_requests(Server, Tab, [[{1,a}]], Seq1),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Different keys read by different processes
    Seq2 = [{P1,[{member,1,true}]}, {P2,[{lookup,2,[{2,b}]}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{lookup,3,[{3,c}]}]}],
    atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq2),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Reading deleted key.
    Seq3 = [{P1,[{delete_key,2}]}, {P2,[{lookup,1,[{1,a}]}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{member,2,false}]}],
    atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq3),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Inserting objects.
    Seq4 = [{P1,[{insert,[{1,a},{2,b}]}]}, {P2,[{insert,[{2,c},{3,a}]}]},
	    {P3,[{insert,[{3,b},{4,d}]}]},
	    {P4,[{lookup_keys,[1,2,3,4],[{1,a},{2,c},{3,b},{4,d}]}]}],
    atomic_requests(Server, Tab, [], Seq4),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Deleting objects.
    Seq5 = [{P1,[{delete_object,{1,a}}]}, {P2,[{delete_object,{1,a}}]},
	    {P3,[{delete_object,{3,c}}]},
	    {P4,[{lookup_keys,[1,2,3,4],[{2,b}]}]}],
    atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq5),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Some request not streamed.
    Seq6 = [{P1,[{lookup,1,[{1,a}]}]}, {P2,[{info,size,3}]},
	    {P3,[{lookup,1,[{1,a}]}]}, {P4,[{info,size,3}]}],
    atomic_requests(Server, Tab, [[{1,a},{2,b},{3,c}]], Seq6),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    %% Some request not streamed.
    Seq7 = [{P1,[{insert,[{3,a}]}]}, {P2,[{insert,[{3,b}]}]},
	    {P3,[{delete_object,{3,c}}]},
	    {P4,[{lookup,3,[{3,b}]}]}],
    atomic_requests(Server, Tab, [[{3,c}]], Seq7),
    true = get_replies([{P1,ok}, {P2,ok}, {P3,ok}, {P4,ok}]),

    put_requests(Server, [{P1,stop},{P2,stop},{P3,stop},{P4,stop}]),
    ok = dets:close(Tab),
    file:delete(FName),

    %% Check that errors are handled correctly by the streaming operators.
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    ok = ins(Tab, 100),
    Obj = {66,{item,number,66}},
    {ok, ObjPos} = dets:where(Tab, Obj),
    ok = dets:close(Tab),
    %% Damaged object.
    crash(FName, ObjPos+12),
    {ok, _} = dets:open_file(Tab,[{file, FName}]),
    BadObject1 = dets:lookup_keys(Tab, [65,66,67,68,69]),
    bad_object(BadObject1, FName),
    _Error = dets:close(Tab),
    file:delete(FName),

    check_pps(P0),

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
    receive {Pid, Reply} -> Reply end.

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
	    Reply = eval(ToDo, Tab),
	    case Reply of
		{error, _} -> io:format("~p: ~p~n", [self(), Reply]);
		_ -> ok
	    end,
	    S ! {self(), Reply}
    end,
    client(S, Tab).

eval([], _Tab) ->
    ok;
eval([{verbose,Bool,Expected} | L], Tab) ->
    case dets:verbose(Bool) of
        Expected -> eval(L, Tab);
        Error -> {error, {verbose,Error}}
    end;
eval([sync | L], Tab) ->
    case dets:sync(Tab) of
        ok -> eval(L, Tab);
        Error -> {error, {sync,Error}}
    end;
eval([{insert,Stuff} | L], Tab) ->
    case dets:insert(Tab, Stuff) of
        ok -> eval(L, Tab);
        Error -> {error, {insert,Stuff,Error}}
    end;
eval([{lookup,Key,Expected} | L], Tab) ->
    case dets:lookup(Tab, Key) of
        Expected -> eval(L, Tab);
        Else -> {error, {lookup,Key,Expected,Else}}
    end;
eval([{lookup_keys,Keys,Expected} | L], Tab) ->
    %% Time order is destroyed...
    case dets:lookup_keys(Tab, Keys) of
        R when is_list(R) ->
            case lists:sort(Expected) =:= lists:sort(R) of
                true -> eval(L, Tab);
                false -> {error, {lookup_keys,Keys,Expected,R}}
            end;
        Else -> {error, {lookup_keys,Keys,Expected,Else}}
    end;
eval([{member,Key,Expected} | L], Tab) ->
    case dets:member(Tab, Key) of
        Expected -> eval(L, Tab);
        Else -> {error, {member,Key,Expected,Else}}
    end;
eval([{delete_key,Key} | L], Tab) ->
    case dets:delete(Tab, Key) of
        ok -> eval(L, Tab);
        Else -> {error, {delete_key,Key,Else}}
    end;
eval([{delete_object,Object} | L], Tab) ->
    case dets:delete_object(Tab, Object) of
        ok -> eval(L, Tab);
        Else -> {error, {delete_object,Object,Else}}
    end;
eval([{info,Tag,Expected} | L], Tab) ->
    case dets:info(Tab, Tag) of
        Expected -> eval(L, Tab);
        Else -> {error, {info,Tag,Else,Expected}}
    end;
eval(Else, _Tab) ->
    {error, {bad_request,Else}}.

%% More than 128k keys caused crash.
otp_4906(Config) when is_list(Config) ->
    N = 256*512 + 400,
    Tab = otp_4906,
    FName = filename(Tab, Config),
    
    file:delete(FName),
    {ok, Tab} = dets:open_file(Tab, [{file, FName}]),
    ok = ins_small(Tab, 0, N),
    ok = dets:close(Tab),
    {ok, Tab} = dets:open_file(Tab, [{file, FName}]),
    ok = read_4906(Tab, N-1),
    ok = dets:close(Tab),
    file:delete(FName),

    %% If the (only) process fixing a table updates the table, the
    %% process will no longer be punished with a 1 ms delay (hm, the
    %% server is delayed, it should be the client...). In this example
    %% the writing process *is* delayed.
    {ok,Tab} = dets:open_file(Tab, [{file,FName}]),
    Parent = self(),
    FixPid = spawn_link(fun() -> 
                                dets:safe_fixtable(Tab, true),
                                receive {Parent, stop} -> ok end
                        end),
    ok = ins_small(Tab, 0, 1000),
    FixPid ! {Parent, stop},
    timer:sleep(1),
    ok = dets:close(Tab),
    file:delete(FName),
    ok.

read_4906(_T, N) when N < 0 ->
    ok;
read_4906(T, N) ->
    [_] = dets:lookup(T, N),
    read_4906(T, N-1).

ins_small(_T, I, N) when I =:= N ->
    ok;
ins_small(T, I, N) ->
    ok = dets:insert(T, {I}),
    ins_small(T, I+1, N).

%% Unwritable ramfile caused crash.
otp_5402(Config) when is_list(Config) ->
    Tab = otp_5402,
    File = filename:join(["cannot", "write", "this", "file"]),

    %% close
    {ok, T} = dets:open_file(Tab, [{ram_file,true},
                                   {file, File}]),
    ok = dets:insert(T, {1,a}),
    {error,{file_error,_,_}} = dets:close(T),

    %% sync
    {ok, T} = dets:open_file(Tab, [{ram_file,true},
                                   {file, File}]),
    ok = dets:insert(T, {1,a}),
    {error,{file_error,_,_}} = dets:sync(T),
    {error,{file_error,_,_}} = dets:close(T),

    %% auto_save
    {ok, T} = dets:open_file(Tab, [{ram_file,true},
                                   {auto_save, 2000},
                                   {file, File}]),
    ok = dets:insert(T, {1,a}),
    timer:sleep(5000),
    {error,{file_error,_,_}} = dets:close(T),
    ok.

%% Several clients open and close tables simultaneously.
simultaneous_open(Config) ->
    Tab = sim_open,
    File = filename(Tab, Config),
    
    ok = monit(Tab, File),
    case feasible() of
        false -> {comment, "OK, but did not run all of the test"};
        true ->
            ok = kill_while_repairing(Tab, File),
            ok = kill_while_init(Tab, File),
            ok = open_ro(Tab, File),
            ok = open_w(Tab, File, 0, Config),
            ok = open_w(Tab, File, 100, Config)
    end.

feasible() ->
    LP = erlang:system_info(logical_processors),
    (is_integer(LP)
     andalso LP >= erlang:system_info(schedulers_online)
     andalso not erlang:system_info(debug_compiled)
     andalso not erlang:system_info(lock_checking)).

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
    ok = file:delete(File).

do_log(Tab) ->
    case catch dets:insert(Tab, {hej,san,sa}) of
        ok -> do_log(Tab);
        Else -> Else
    end.

%% Kill the Dets process while repair is in progress.
kill_while_repairing(Tab, File) ->
    create_opened_log(File),
    Delay = 1000,
    dets:start(),
    Parent = self(),
    F = fun() -> 
                R = (catch dets:open_file(Tab, [{file,File}])),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    %% One of these will open the file, the other will be pending
    %% until the file has been repaired:
    P1 = spawn(F),
    P2 = spawn(F),
    P3 = spawn(F),
    DetsPid = find_dets_pid(),
    exit(DetsPid, kill),

    receive {P1,R1} -> R1 end,
    receive {P2,R2} -> R2 end,
    receive {P3,R3} -> R3 end,
    io:format("Killed pid: ~p~n", [DetsPid]),
    io:format("Remaining Dets-pids (should be nil): ~p~n",
              [find_dets_pids()]),
    {replies,[{'EXIT', {dets_process_died, _}}, {ok,_}, {ok, _}]} =
         {replies,lists:sort([R1, R2, R3])},

    timer:sleep(200),
    case dets:info(Tab) of
        undefined -> 
            ok;
        _Info ->
            timer:sleep(5000),
            undefined = dets:info(Tab)
    end,

    file:delete(File),
    ok.

find_dets_pid() ->
    case find_dets_pids() of
        [] ->
            timer:sleep(100),
            find_dets_pid();
        [Pid] ->
            Pid
    end.

find_dets_pids() ->
    lists:filter(fun(P) -> dets:pid2name(P) =/= undefined end,
                 erlang:processes()).

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
    P1 = spawn(F),
    P2 = spawn(F),
    P3 = spawn(F),
    IF = fun() ->
                 R = dets:open_file(Tab, [{file,File}]),
                 Parent ! {self(), R},
                 Fun = fun(_) -> timer:sleep(100000) end,
                 {'EXIT', {badarg, _}} = (catch dets:init_table(Tab, Fun)),
                 receive {Parent, die} -> ok end
          end,
    P4 = spawn(IF),
    receive {P1,R1} -> {ok, _} = R1 end,
    receive {P2,R2} -> {ok, _} = R2 end,
    receive {P3,R3} -> {ok, _} = R3 end,
    receive {P4,R4} -> {ok, _} = R4 end,
    DetsPid = find_dets_pid(),
    exit(DetsPid, kill),
    
    timer:sleep(1000),
    undefined = dets:info(Tab),
    P1 ! {Parent, die},
    P2 ! {Parent, die},
    P3 ! {Parent, die},
    P4 ! {Parent, die},

    file:delete(File),
    timer:sleep(100),
    ok.

open_ro(Tab, File) ->
    create_opened_log(File),
    Delay = 1000,
    Parent = self(),
    F = fun() ->
                R = dets:open_file(Tab, [{file,File},{access,read}]),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    P1 = spawn(F),
    P2 = spawn(F),
    P3 = spawn(F),
    
    receive {P1,R1} -> {error,{not_closed,_}} = R1 end,
    receive {P2,R2} -> {error,{not_closed,_}} = R2 end,
    receive {P3,R3} -> {error,{not_closed,_}} = R3 end,
    ok.

open_w(Tab, File, Delay, Config) ->
    create_opened_log(File),

    Tab2 = t2,
    File2 = filename(Tab2, Config),
    file:delete(File2),
    {ok,Tab2} = dets:open_file(Tab2, [{file,File2}]),
    ok = dets:close(Tab2),

    Parent = self(),
    F = fun() -> 
                R = dets:open_file(Tab, [{file,File}]),
                timer:sleep(Delay),
                Parent ! {self(), R}
        end,
    Pid1 = spawn(F),
    Pid2 = spawn(F),
    Pid3 = spawn(F),

    ok = wait_for_repair_to_start(Tab),

    %% It is assumed that it takes some time to repair the file.
    {ok,Tab2} = dets:open_file(Tab2, [{file,File2}]),
    %% The Dets server managed to handle to open_file request.
    0 = qlen(), % still repairing

    ok = dets:close(Tab2),
    file:delete(File2),

    receive {Pid1,R1} -> {ok, Tab} = R1 end,
    receive {Pid2,R2} -> {ok, Tab} = R2 end,
    receive {Pid3,R3} -> {ok, Tab} = R3 end,
    timer:sleep(200),
    case dets:info(Tab) of
        undefined -> 
            ok;
        _Info ->
            timer:sleep(5000),
            undefined = dets:info(Tab)
    end,

    file:delete(File),
    ok.

wait_for_repair_to_start(Tab) ->
    case catch dets_server:get_pid(Tab) of
        {'EXIT', _} ->
            timer:sleep(1),
            wait_for_repair_to_start(Tab);
        Pid when is_pid(Pid) ->
            ok
    end.

qlen() ->
    {_, {_, N}} = lists:keysearch(message_queue_len, 1, process_info(self())),
    N.

create_opened_log(File) ->
    Tab = t,
    file:delete(File),
    {ok, Tab} = dets:open_file(Tab, [{file,File}]),
    ok = ins(Tab, 60000),
    ok = dets:close(Tab),
    crash(File, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    ok.

%% OTP-5075. insert_new/2
insert_new(Config) ->
    Tab = insert_new,
    File = filename(Tab, Config),
    file:delete(File),
    {ok, T} = dets:open_file(Tab, [{file,File}]),
    {'EXIT', {badarg, _}} = (catch dets:insert_new(Tab, 14)),
    {'EXIT', {badarg, _}} = (catch dets:insert_new(Tab, {})),
    true = dets:insert_new(Tab, {1,a}),
    false = dets:insert_new(Tab, {1,a}),
    true = dets:insert_new(Tab, [{2,b}, {3,c}]),
    false = dets:insert_new(Tab, [{2,b}, {3,c}]),
    false = dets:insert_new(Tab, [{1,a}, {4,d}]),
    ok = dets:close(Tab),

    file:delete(File),    
    {ok, T} = dets:open_file(Tab, [{file,File},{type,bag}]),
    true = dets:insert_new(Tab, {1,a}),
    false = dets:insert_new(Tab, {1,b}),
    true = dets:insert_new(Tab, [{2,b}, {3,c}]),
    false = dets:insert_new(Tab, [{2,a}, {3,d}]),
    false = dets:insert_new(Tab, [{1,a}, {4,d}]),
    ok = dets:close(Tab),
    

    file:delete(File),
    ok.
    
%% OTP-5126. repair_continuation/2
repair_continuation(Config) ->
    Tab = repair_continuation_table,
    Fname = filename(repair_cont, Config),
    file:delete(Fname),
    {ok, _} = dets:open_file(Tab, [{file,Fname}]),
    ok = dets:insert(Tab, [{1,a},{2,b},{3,c}]),

    MS = [{'_',[],[true]}],

    SRes = term_to_binary(dets:select(Tab, MS, 1)),
    %% Get rid of compiled match spec
    lists:foreach(fun (P) ->
                          garbage_collect(P)
                  end, processes()),
    {[true], C2} = binary_to_term(SRes),

    {'EXIT', {badarg, _}} = (catch dets:select(C2)),
    C3 = dets:repair_continuation(C2, MS),
    {[true], C4} = dets:select(C3),
    C5 = dets:repair_continuation(C4, MS),
    {[true], _} = dets:select(C5),
    {'EXIT', {badarg, _}} = (catch dets:repair_continuation(Tab, bu)),

    ok = dets:close(Tab),
    file:delete(Fname),
    ok.

%% OTP-5487. Growth of read-only table (again).
otp_5487(Config) ->
    Tab = otp_5487,
    Fname = filename(otp_5487, Config),
    file:delete(Fname),
    Ets = ets:new(otp_5487, [public, set]),
    lists:foreach(fun(I) -> ets:insert(Ets, {I,I+1}) end,
                  lists:seq(0,1000)),
    {ok, _} = dets:open_file(Tab, [{file,Fname}]),
    ok = dets:from_ets(Tab, Ets),
    ok = dets:sync(Tab),
    ok = dets:close(Tab),
    {ok, _} = dets:open_file(Tab, [{file,Fname},{access,read}]),
    [{1,2}] = dets:lookup(Tab, 1),
    ok = dets:close(Tab),
    ets:delete(Ets),
    file:delete(Fname).

%% OTP-6206. Badly formed free lists.
otp_6206(Config) ->
    Tab = otp_6206,
    File = filename(Tab, Config),

    file:delete(File),
    Options = [{file,File}],
    {ok, Tab} = dets:open_file(Tab, Options),
    NObjs = 13006,
    ok = ins(Tab, NObjs),
    ok = del(Tab, NObjs, 2),
    ok = dets:close(Tab),

    %% Used to return {badmatch,{error,{bad_freelists,File}}.
    {ok, Tab} = dets:open_file(Tab, [{repair,false}|Options]),
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-6359. select and match never return the empty list.
otp_6359(Config) ->
    Tab = otp_6359,
    File = filename(Tab, Config),

    file:delete(File),
    {ok, _} = dets:open_file(Tab, [{file, File}]),
    %% Used to return {[], Cont}:
    '$end_of_table' = dets:match(Tab, '_', 100),
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-4738. ==/2 and =:=/2.
otp_4738(Config) ->
    otp_4738_set(Config),
    otp_4738_bag(Config),
    otp_4738_dupbag(Config),
    ok.

otp_4738_dupbag(Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    I = -12857447,
    F = float(I),
    One = 1,
    FOne = float(One),
    Args = [{file,File},{type,duplicate_bag}],
    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ok = dets:sync(Tab),
    [{F,One},{F,FOne}] = dets:lookup(Tab, F),
    [{I,One},{I,FOne}] = dets:lookup(Tab, I),
    ok = dets:insert(Tab, [{F,One},{F,FOne}]),
    [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},{F,FOne}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:insert(Tab, [{F,FOne},{F,One}]),
    [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},
           {F,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:delete_object(Tab, {I,FOne}),
    [{I,One},{F,One},{F,FOne},{F,One},{F,FOne},{F,FOne},{F,One}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:insert(Tab, {I,FOne}),
    [{I,One},{I,FOne},{F,One},{F,FOne},{F,One},
           {F,FOne},{F,FOne},{F,One}] = 
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:delete_object(Tab, {F,FOne}),
    [{I,One},{I,FOne},{F,One},{F,One},{F,One}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:delete(Tab, F),
    [{I,One},{I,FOne}] = dets:match_object(Tab, '_'),
    ok = dets:close(Tab),
    file:delete(File),

    Zero = 0,
    FZero = float(Zero),
    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ok = dets:insert(Tab, [{I,Zero},{F,Zero},{I,FZero},{I,FZero}]),
    Objs0 = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    crash(File, ?CLOSED_PROPERLY_POS+3, ?NOT_PROPERLY_CLOSED),
    io:format("Expect repair:~n"),
    {ok, Tab} = dets:open_file(Tab, Args),
    Objs1 = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    Objs1 = Objs0,
    file:delete(File),
    ok.

otp_4738_bag(Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    I = -12857447,
    F = float(I),
    One = 1,
    FOne = float(One),
    Args = [{file,File},{type,bag}],
    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I,One},{F,One},{I,FOne},{F,FOne}]),
    ok = dets:sync(Tab),
    [{F,One},{F,FOne}] = dets:lookup(Tab, F),
    [{I,One},{I,FOne}] = dets:lookup(Tab, I),
    ok = dets:insert(Tab, [{F,One},{F,FOne}]),
    [{I,One},{I,FOne},{F,One},{F,FOne}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:insert(Tab, [{F,FOne},{F,One}]),
    [{I,One},{I,FOne},{F,FOne},{F,One}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:delete_object(Tab, {I,FOne}),
    [{I,One},{F,FOne},{F,One}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:insert(Tab, {I,FOne}),
    [{I,One},{I,FOne},{F,FOne},{F,One}] =
        dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:delete(Tab, F),
    [{I,One},{I,FOne}] = dets:match_object(Tab, '_'),
    ok = dets:close(Tab),
    file:delete(File).

otp_4738_set(Config) ->
    Tab = otp_4738,
    File = filename(Tab, Config),
    file:delete(File),
    Args = [{file,File},{type,set}],

    %% I and F share the same slot.
    I = -12857447,
    F = float(I),
    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I},{F}]),
    ok = dets:sync(Tab),
    [{F}] = dets:lookup(Tab, F),
    [{I}] = dets:lookup(Tab, I),
    ok = dets:insert(Tab, [{F}]),
    [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    file:delete(File),
    
    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I}]),
    ok = dets:sync(Tab),
    [] = dets:lookup(Tab, F),
    [{I}] = dets:lookup(Tab, I),
    ok = dets:insert(Tab, [{F}]),
    [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    file:delete(File),

    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I},{F}]),
    %% {insert, ...} in the cache, try lookup:
    [{F}] = dets:lookup(Tab, F),
    [{I}] = dets:lookup(Tab, I),
    %% Both were found, but that cannot be verified.
    [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    file:delete(File),

    {ok, Tab} = dets:open_file(Tab, Args),
    ok = dets:insert(Tab, [{I}]),
    ok = dets:sync(Tab),
    ok = dets:insert(Tab, [{F}]),
    %% {insert, ...} in the cache, try lookup:
    [{F}] = dets:lookup(Tab, F),
    [{I}] = dets:lookup(Tab, I),
    [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    file:delete(File),

    {ok, Tab} = dets:open_file(Tab, Args),
    %% Both operations in the cache:
    ok = dets:insert(Tab, [{I}]),
    ok = dets:insert(Tab, [{F}]),
    [{I},{F}] = dets_utils:mkeysort(1, dets:match_object(Tab, '_')),
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-7146. Bugfix: missing test when re-hashing.
otp_7146(Config) ->
    Tab = otp_7146,
    File = filename(Tab, Config),
    file:delete(File),

    Max = 2048,
    {ok, Tab} = dets:open_file(Tab, [{max_no_slots,Max}, {file,File}]),
    write_dets(Tab, Max),
    ok = dets:close(Tab),

    file:delete(File),
    ok.

write_dets(Tab, Max) ->
    write_dets(Tab, 0, Max).

write_dets(_Tab, N, Max) when N > Max ->
    ok;
write_dets(Tab, N, Max) ->
    ok = dets:insert(Tab,{ N, {entry,N}}),
    write_dets(Tab, N+1, Max).

%% OTP-8070. Duplicated objects with insert_new() and duplicate_bag.
otp_8070(Config) when is_list(Config) ->
    Tab = otp_8070,
    File = filename(Tab, Config),
    file:delete(File),
    {ok, _} = dets:open_file(Tab, [{file,File},{type, duplicate_bag}]),
    ok = dets:insert(Tab, [{3,0}]),
    false = dets:insert_new(Tab, [{3,1},{3,1}]),
    [{3,0}] = dets:lookup(Tab, 3),
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-8856. insert_new() bug.
otp_8856(Config) when is_list(Config) ->
    Tab = otp_8856,
    File = filename(Tab, Config),
    file:delete(File),
    Me = self(),
    {ok, _} = dets:open_file(Tab, [{type, bag}, {file, File}]),
    spawn(fun()-> Me ! {1, dets:insert(Tab, [])} end),
    spawn(fun()-> Me ! {2, dets:insert_new(Tab, [])} end),
    receive {1, ok} -> ok end,
    receive {2, true} -> ok end,
    ok = dets:close(Tab),
    file:delete(File),

    {ok, _} = dets:open_file(Tab, [{type, set}, {file, File}]),
    spawn(fun() -> dets:delete(Tab, 0) end),
    spawn(fun() -> Me ! {3, dets:insert_new(Tab, {0,0})} end),
    receive {3, true} -> ok end,
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-8903. bchunk/match/select bug.
otp_8903(Config) when is_list(Config) ->
    Tab = otp_8903,
    File = filename(Tab, Config),
    {ok,T} = dets:open_file(bug, [{file,File}]),
    ok = dets:insert(T, [{1,a},{2,b},{3,c}]),
    dets:safe_fixtable(T, true),
    {[_],C1} = dets:match_object(T, '_', 1),
    {BC1,_D} = dets:bchunk(T, start),
    ok = dets:close(T),
    {'EXIT', {badarg, _}} = (catch {foo,dets:match_object(C1)}),
    {'EXIT', {badarg, _}} = (catch {foo,dets:bchunk(T, BC1)}),
    {ok,T} = dets:open_file(bug, [{file,File}]),
    false = dets:info(T, safe_fixed),
    {'EXIT', {badarg, _}} = (catch {foo,dets:match_object(C1)}),
    {'EXIT', {badarg, _}} = (catch {foo,dets:bchunk(T, BC1)}),
    ok = dets:close(T),
    file:delete(File),
    ok.

%% OTP-8923. rehash due to lookup after initialization.
otp_8923(Config) when is_list(Config) ->
    Tab = otp_8923,
    File = filename(Tab, Config),
    %% Create a file with more than 256 keys:
    file:delete(File),
    Bin = list_to_binary([ 0 || _ <- lists:seq(1, 400) ]),
    BigBin = list_to_binary([ 0 ||_ <- lists:seq(1, 4000)]),
    Ets = ets:new(temp, [{keypos,1}]),
    [ true = ets:insert(Ets, {C,Bin}) || C <- lists:seq(1, 700) ],
    true = ets:insert(Ets, {helper_data,BigBin}),
    true = ets:insert(Ets, {prim_btree,BigBin}),
    true = ets:insert(Ets, {sec_btree,BigBin}),
    %% Note: too few slots; re-hash will take place
    {ok, Tab} = dets:open_file(Tab, [{file,File}]),
    Tab = ets:to_dets(Ets, Tab),
    ok = dets:close(Tab),
    true = ets:delete(Ets),
    
    {ok,Ref} = dets:open_file(File),
    [{1,_}] = dets:lookup(Ref, 1),
    ok = dets:close(Ref),

    {ok,Ref2} = dets:open_file(File),
    [{helper_data,_}] = dets:lookup(Ref2, helper_data),
    ok = dets:close(Ref2),

    file:delete(File),
    ok.

%% OTP-9282. The name of a table can be an arbitrary term.
otp_9282(Config) when is_list(Config) ->
    some_calls(make_ref(), Config),
    some_calls({a,typical,name}, Config),
    some_calls(fun() -> a_funny_name end, Config),
    ok.

some_calls(Tab, Config) ->
    File = filename(ref, Config),
    {ok,T} = dets:open_file(Tab, [{file,File}]),
    T = Tab,
    false = dets:info(T, safe_fixed),
    File = dets:info(T, filename),
    ok = dets:insert(Tab, [{3,0}]),
    [{3,0}] = dets:lookup(Tab, 3),
    [{3,0}] = dets:traverse(Tab, fun(X) -> {continue, X} end),
    ok = dets:close(T),
    file:delete(File).


%% OTP-11245. Tables remained fixed after traversal.
otp_11245(Config) when is_list(Config) ->
    Tab = otp_11245,
    File = filename(Tab, Config),
    {ok, Tab} = dets:open_file(Tab, [{file,File}]),
    N = 1024,
    ins(Tab, N),
    N = length(dets:match(Tab, '_')),
    false = dets:info(Tab, safe_fixed),
    dets:traverse(Tab, fun(_) -> continue end),
    false = dets:info(Tab, safe_fixed),
    N = dets:foldl(fun(_, N2) -> N2+1 end, 0, Tab),
    false = dets:info(Tab, safe_fixed),
    N = dets:foldr(fun(_, N2) -> N2+1 end, 0, Tab),
    false = dets:info(Tab, safe_fixed),
    ok = dets:close(Tab),
    file:delete(File),
    ok.

%% OTP-11709. Bugfixes.
otp_11709(Config) when is_list(Config) ->
    Short = <<"foo">>,
    Long = <<"a sufficiently long text">>,

    %% Bug: leaking file descriptor
    P0 = pps(),
    File = filename(otp_11709, Config),
    ok = file:write_file(File, Long),
    false = dets:is_dets_file(File),
    check_pps(P0),

    %% Bug: deleting file
    Args = [[{access, A}, {repair, R}] ||
               A <- [read, read_write],
               R <- [true, false, force]],
    Fun1 = fun(S, As) ->
                   P1 = pps(),
                   ok = file:write_file(File, S),
                   {error,{not_a_dets_file,File}} = dets:open_file(File, As),
                   {ok, S} = file:read_file(File),
                   check_pps(P1)
           end,
    Fun2 = fun(S) ->
                   _ = [Fun1(S, As) || As <- Args],
                   ok
           end,
    ok = Fun2(Long),  % no change here
    ok = Fun2(Short), % mimic the behaviour for longer files

    %% open_file/1
    ok = file:write_file(File, Long),
    {error,{not_a_dets_file,File}} = dets:open_file(File), % no change
    ok = file:write_file(File, Short),
    {error,{not_a_dets_file,File}} = dets:open_file(File), % mimic

    _ = file:delete(File),
    ok.

%% OTP-13229. open_file() exits with badarg when given binary file name.
otp_13229(_Config) ->
    F = <<"binfile.tab">>,
    try dets:open_file(name, [{file, F}]) of
        R ->
            exit({open_succeeded, R})
    catch
        error:badarg ->
            ok
    end.

%% OTP-13260. Race when opening a table.
otp_13260(Config) ->
    [ok] = lists:usort([otp_13260_1(Config) || _ <- lists:seq(1, 3)]),
    ok.

otp_13260_1(Config) ->
    Tab = otp_13260,
    File = filename(Tab, Config),
    N = 20,
    P = self(),
    Pids = [spawn_link(fun() -> counter(P, Tab, File) end) ||
               _ <- lists:seq(1, N)],
    Rs = rec(Pids),
    true = lists:all(fun(R) -> is_integer(R) end, Rs),
    wait_for_close(Tab).

rec([]) ->
    [];
rec([Pid | Pids]) ->
    receive {Pid, R} ->
            [R | rec(Pids)]
    end.

%% One may have to run the test several times to trigger the bug.
counter(P, Tab, File) ->
    Key = key,
    N = case catch dets:update_counter(Tab, Key, 1) of
            {'EXIT', _} ->
                {ok, Tab} = dets:open_file(Tab, [{file, File}]),
                ok = dets:insert(Tab, {Key, 1}),
                dets:update_counter(Tab, Key, 1);
            N1 when is_integer(N1) ->
                N1;
            DetsBug ->
                DetsBug
        end,
    P ! {self(), N}.

wait_for_close(Tab) ->
    case dets:info(Tab, owner) of
        undefined ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for_close(Tab)
    end.

%% OTP-13830. Format 8 is no longer supported.
otp_13830(Config) ->
    Tab = otp_13830,
    File8 = filename:join(?datadir(Config), "version_8.dets"),
    {error,{format_8_no_longer_supported,_}} =
        dets:open_file(Tab, [{file, File8}]),
    File = filename(Tab, Config),
    %% Check the 'version' option, for backwards compatibility:
    {ok, Tab} = dets:open_file(Tab, [{file, File}, {version, 9}]),
    ok = dets:close(Tab),
    {ok, Tab} = dets:open_file(Tab, [{file, File}, {version, default}]),
    ok = dets:close(Tab).

%%
%% Parts common to several test cases
%% 

crash(File, Where) ->
    crash(File, Where, 10).

crash(File, Where, What) when is_integer(What) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:write(Fd, [What]),
    ok = file:close(Fd).

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
    Objs = safe_get_all_objects(Tab),
    Keys = map(fun(X) -> element(1, X) end, Objs),
    foreach(fun(Key) -> dets:delete(Tab, Key) end, Keys),
    0 = length(get_all_objects(Tab)),
    [] = get_all_objects_fast(Tab),
    0 = dets:info(Tab, size).

del_obj_test(Tab) ->
    ?format("Delobjtest on ~p~n", [Tab]),
    Objs = safe_get_all_objects(Tab),
    LL = length(Objs),
    LL = dets:info(Tab, size),
    foreach(fun(Obj) -> dets:delete_object(Tab, Obj) end, Objs),
    0 = length(get_all_objects(Tab)),
    [] = get_all_objects_fast(Tab),
    0 = dets:info(Tab, size).

match_del_test(Tab) ->
    ?format("Match delete test on ~p~n", [Tab]),
    ok = dets:match_delete(Tab, {'_','_','_'}),
    Sz = dets:info(Tab, size),
    true = Sz =:= length(dets:match_object(Tab, '_')),
    ok = dets:match_delete(Tab, '_'),
    0 = dets:info(Tab, size),
    0 = length(get_all_objects(Tab)),
    [] = get_all_objects_fast(Tab).

trav_test(_Data, Len, Tab) ->
    ?format("Travtest on ~p~n", [Tab]),
    _X0 = dets:traverse(Tab, fun(_X) -> continue end),
    XX = dets:traverse(Tab, fun(X) -> {continue, X} end),
    case Len =:= length(XX) of
	      false -> ?format("DIFF ~p~n", [XX -- _Data]);
	      true -> ok
	  end,
    1 = length(dets:traverse(Tab, fun(X) -> {done, X} end)).

match_test(Data, Tab) ->
    ?format("Match test on ~p~n", [Tab]),
    Data1 = sort(filter(fun(X) when tuple_size(X) =:= 3 -> true;
                           (_X) -> false
                        end, Data)),
    Data1 = sort(dets:match_object(Tab, {'$1', '$2', '$3'})),

    Len = length(Data),
    Len = length(dets:match(Tab, '_')),
    Len2 = length(Data1),
    Len2 = length(dets:match(Tab, {'$1', '_', '_'})),
    
    Data3 =
	filter(fun(X) ->
		       K = element(1, X),
		       if
			   tuple_size(X) =:= 3, tuple_size(K) =:= 2 -> true;
			   true -> false
		       end
	       end, Data),
    Len3 = length(Data3),
    Len3 = length(dets:match(Tab, {{'$1', '$2'}, '_', '_'})),
    Len3 = length(dets:match_object(Tab, {{'$1', '$2'}, '_', '_'})),
    
    R = make_ref(),
    dets:insert(Tab, {{R, R}, 33 ,44}),
    1 = length(dets:match(Tab, {{R, R}, '_', '_'})),
    1 = length(dets:match_object(Tab, {{R, R}, '_', '_'})).

%%
%% Utilities
%%

headsz() ->
    ?HEADSZ_v9.

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

truncate(File, Where) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:truncate(Fd),
    ok = file:close(Fd).

new_filename(Name, _Config) when is_integer(Name) ->
    filename:join(?privdir(_Config),
		  integer_to_list(Name) ++ ".DETS").

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, _Config) ->
    filename:join(?privdir(_Config), Name).

open_files(_Name, []) ->
    [];
open_files(Name0, [Args | Tail]) ->
    ?format("init ~p~n", [Args]),
    Name = list_to_atom(integer_to_list(Name0)),
    {ok, Name} = dets:open_file(Name, Args),
    [Name | open_files(Name0+1, Tail)].
    
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
    foreach(fun(Tab) ->
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
    foreach(fun(Tab) ->
                    Len = dets:info(Tab, size)
            end, Tabs).

no_keys_test([T | Ts]) ->
    no_keys_test(T),
    no_keys_test(Ts);
no_keys_test([]) ->
    ok;
no_keys_test(T) ->
    Kp = dets:info(T, keypos),
    All = dets:match_object(T, '_'),
    L = lists:map(fun(X) -> element(Kp, X) end, All),
    NoKeys = length(lists:usort(L)),
    case {dets:info(T, no_keys), NoKeys} of
        {N, N} ->
            ok;
        {N1, N2} ->
            exit({no_keys_test, N1, N2})
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
    exit({get_all_objects, {error, Reason}});
get_all_objects(Key, Tab, L) -> 
    Objs = dets:lookup(Tab, Key),
    get_all_objects(dets:next(Tab, Key), Tab, Objs ++ L).

count_objects_quite_fast(Tab) ->
    R1 = dets:match_object(Tab, '_', 1),
    count_objs_1(R1, 0).

count_objs_1('$end_of_table', N) ->
    N;
count_objs_1({Ts,C}, N) when is_list(Ts) ->
    count_objs_1(dets:match_object(C), length(Ts) + N).

get_all_objects_fast(Tab) ->
    dets:match_object(Tab, '_').

histogram(Tab) ->
    OnePercent = case dets:info(Tab, no_slots) of
	undefined -> undefined;
	{_, NoSlots, _} -> NoSlots/100
    end,
    histogram(Tab, OnePercent).

histogram(Tab, OnePercent) ->
    E = ets:new(histo, []),
    dets:safe_fixtable(Tab, true),
    Hist = histo(Tab, E, 0, OnePercent, OnePercent),
    dets:safe_fixtable(Tab, false),
    case Hist of
        ok ->
            H = ets:tab2list(E),
            true = ets:delete(E),
            sort(H);
        Error ->
            ets:delete(E),
            Error
    end.

histo(T, E, I, One, Count) when is_number(Count), I > Count ->
    io:format("."),
    histo(T, E, I, One, Count+One);
histo(T, E, I, One, Count) ->
    case dets:slot(T, I) of
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

bad_object({error,{{bad_object,_}, FileName}}, FileName) ->
    ok; % No debug.
bad_object({error,{{{bad_object,_,_},_,_,_}, FileName}}, FileName) ->
    ok. % Debug.

check_badarg({'EXIT', {badarg, [{M,F,Args,_} | _]}}, M, F, Args) ->
    true;
check_badarg({'EXIT', {badarg, [{M,F,A,_} | _]}}, M, F, Args)  ->
    true = test_server:is_native(M) andalso length(Args) =:= A.

check_pps({Ports0,Procs0} = P0) ->
    ok = check_dets_tables(),
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
                {Ports1,Procs1} = P1 ->
		    case {Ports1 -- Ports0, Procs1 -- Procs0} of
			{[], []} -> ok;
			{PortsDiff,ProcsDiff} ->
			    io:format("failure, got ~p~n, expected ~p\n", [P1, P0]),
			    show("Old port", Ports0 -- Ports1),
			    show("New port", PortsDiff),
			    show("Old proc", Procs0 -- Procs1),
			    show("New proc", ProcsDiff),
			    ct:fail(failed)
		    end
	    end
    end.

%% Copied from dets_server.erl:
-define(REGISTRY, dets_registry).
-define(OWNERS, dets_owners).
-define(STORE, dets).

check_dets_tables() ->
    Store = [T ||
                T <- ets:all(),
                ets:info(T, name) =:= ?STORE,
                owner(T) =:= dets],
    S = case Store of
            [Tab] -> ets:tab2list(Tab);
            [] -> []
        end,
    case {ets:tab2list(?REGISTRY), ets:tab2list(?OWNERS), S} of
        {[], [], []} -> ok;
        {R, O, _} ->
            io:format("Registry: ~p~n", [R]),
            io:format("Owners: ~p~n", [O]),
            io:format("Store: ~p~n", [S]),
            not_ok
    end.

owner(Tab) ->
    Owner = ets:info(Tab, owner),
    case process_info(Owner, registered_name) of
	{registered_name, Name} -> Name;
	_ -> Owner
    end.

show(_S, []) ->
    ok;
show(S, [{Pid, Name, InitCall}|Pids]) when is_pid(Pid) ->
    io:format("~s: ~w (~w), ~w: ~p~n",
              [S, Pid, proc_reg_name(Name), InitCall,
               erlang:process_info(Pid)]),
    show(S, Pids);
show(S, [{Port, _}|Ports]) when is_port(Port)->
    io:format("~s: ~w: ~p~n", [S, Port, erlang:port_info(Port)]),
    show(S, Ports).

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

proc_reg_name({registered_name, Name}) -> Name;
proc_reg_name([]) -> no_reg_name.

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.
