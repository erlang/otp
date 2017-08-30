%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(mnesia_cost).
-compile(export_all).

%% This code exercises the mnesia system and produces a bunch
%% of measurements on what various things cost

-define(TIMES, 1000).  %% set to at least 1000 when running for real !!

%% This is the record we perform all ops on in this test

-record(item, {a = 1234,
	       b = foobar,
	       c = "1.2.3.4",
	       d = {'Lennart', 'Hyland'},
	       e = true 
	      }).

go() ->
    go([node() | nodes()]).
    
go(Nodes) when hd(Nodes) == node() ->
    {ok, Out} = file:open("MNESIA_COST", write),
    put(out, Out),
    
    rpc:multicall(Nodes, mnesia, lkill, []),
    ok = mnesia:delete_schema(Nodes),
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),
    TabDef = [{attributes, record_info(fields, item)}],
    {atomic, ok} = mnesia:create_table(item, TabDef),

    round("single ram copy", "no index"),
    {atomic, ok} = mnesia:add_table_index(item, #item.e),
    round("single ram copy", "One index"),

    {atomic, ok} = mnesia:add_table_index(item, #item.c),
    round("single ram copy", "Two indexes"),

    {atomic, ok} = mnesia:del_table_index(item, #item.e),
    {atomic, ok} = mnesia:del_table_index(item, #item.c),

    {atomic, ok} = mnesia:change_table_copy_type(item, node(), disc_copies),
    round("single disc copy", "no index"),

    {atomic, ok} = mnesia:change_table_copy_type(item, node(), ram_copies),

    case length(Nodes) of
	Len when Len < 2 ->
	    format("<WARNING> replication skipped. Too few nodes.", []);
	_Len ->
	    N2 = lists:nth(2, Nodes),
	    {atomic, ok} = mnesia:add_table_copy(item, N2, ram_copies),
	    round("2 replicated ram copy", "no index")
    end,
    file:close(Out),
    erase(out),
    ok.

round(Replication, Index) ->
    run(Replication, Index, [write],
	fun() -> mnesia:write(#item{}) end),


    run(Replication, Index, [read],
	fun() -> mnesia:read({item, 1234}) end),

    run(Replication, Index, [read, write],
	fun() -> mnesia:read({item, 1234}),
		 mnesia:write(#item{}) end),

    run(Replication, Index, [wread, write],
	fun() -> mnesia:wread({item, 1234}),
		 mnesia:write(#item{}) end),


    run(Replication, Index, [match, write, write, write],
	fun() -> mnesia:match_object({item, 1, '_', '_', '_', true}),
		 mnesia:write(#item{a =1}),
		 mnesia:write(#item{a =2}),
		 mnesia:write(#item{a =3}) end).


format(F, As) ->
    io:format(get(out), F, As).

run(What, OtherInfo, Ops, F) ->
    run(t, What, OtherInfo, Ops, F).

run(How, What, OtherInfo, Ops, F) ->
    T1 = erlang:monotonic_time(),
    statistics(runtime),
    do_times(How, ?TIMES, F),
    {_, RunTime} = statistics(runtime),
    T2 = erlang:monotonic_time(),
    RealTime = subtr(T1, T2),
    report(How, What, OtherInfo, Ops, RunTime, RealTime).

report(t, What, OtherInfo, Ops, RunTime, RealTime) ->
    format("~s, ~s,  transaction call ", [What, OtherInfo]),
    format("Ops is ", []),
    lists:foreach(fun(Op) -> format("~w-", [Op]) end, Ops),

    format("~n   ~w/~w Millisecs/Trans ~w/~w MilliSecs/Operation ~n~n",
	      [RunTime/?TIMES, 
	       RealTime/?TIMES,
	       RunTime/(?TIMES*length(Ops)),
	       RealTime/(?TIMES*length(Ops))]);

report(dirty, What, OtherInfo, Ops, RunTime, RealTime) ->
    format("~s, ~s, dirty calls ", [What, OtherInfo]),
    format("Ops is ", []),
    lists:foreach(fun(Op) -> format("~w-", [Op]) end, Ops),

    format("~n   ~w/~w Millisecs/Bunch ~w/~w MilliSecs/Operation ~n~n",
	      [RunTime/?TIMES, 
	       RealTime/?TIMES,
	       RunTime/(?TIMES*length(Ops)),
	       RealTime/(?TIMES*length(Ops))]).


subtr(Before, After) ->
    erlang:convert_time_unit(After-Before, native, milli_seconds).

do_times(t, I, F) ->
    do_trans_times(I, F);
do_times(dirty, I, F) ->
    do_dirty(I, F).

do_trans_times(I, F) when I /= 0 ->
    {atomic, _} = mnesia:transaction(F),
    do_trans_times(I-1, F);
do_trans_times(_,_) -> ok.

do_dirty(I, F) when I /= 0 ->
    F(),
    do_dirty(I-1, F);
do_dirty(_,_) -> ok.

    
    
table_load([N1,N2| _ ] = Ns) ->    
    Nodes = [N1,N2],
    rpc:multicall(Ns, mnesia, lkill, []),
    ok = mnesia:delete_schema(Ns),
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, mnesia, start, []),
    TabDef = [{disc_copies,[N1]},{ram_copies,[N2]},
	      {attributes,record_info(fields,item)},{record_name,item}],
    Tabs   = [list_to_atom("tab" ++ integer_to_list(I)) || I <- lists:seq(1,400)],
    
    [mnesia:create_table(Tab,TabDef) || Tab <- Tabs],

%%     InitTab = fun(Tab) ->
%% 		      mnesia:write_lock_table(Tab),
%% 		      InitRec = fun(Key) -> mnesia:write(Tab,#item{a=Key},write) end,
%% 		      lists:foreach(InitRec, lists:seq(1,100))
%% 	      end,
%%     
%%    {Time,{atomic,ok}} = timer:tc(mnesia,transaction, [fun() ->lists:foreach(InitTab, Tabs) end]),
    mnesia:dump_log(),
%%    io:format("Init took ~p msec ~n", [Time/1000]),
    rpc:call(N2, mnesia, stop, []),    timer:sleep(1000),
    mnesia:stop(), timer:sleep(500),
    %% Warmup
    ok = mnesia:start([{no_table_loaders, 1}]),    
    timer:tc(mnesia, wait_for_tables, [Tabs, infinity]),
    mnesia:dump_log(),
    rpc:call(N2, mnesia, dump_log, []),
    io:format("Initialized ~n",[]),

    mnesia:stop(), timer:sleep(1000),
    ok = mnesia:start([{no_table_loaders, 1}]),
    {T1, ok} = timer:tc(mnesia, wait_for_tables, [Tabs, infinity]),
    io:format("Loading from disc with 1 loader ~p msec~n",[T1/1000]),
    mnesia:stop(), timer:sleep(1000),
    ok = mnesia:start([{no_table_loaders, 4}]),
    {T2, ok} = timer:tc(mnesia, wait_for_tables, [Tabs, infinity]),
    io:format("Loading from disc with 4 loader ~p msec~n",[T2/1000]),

    %% Warmup
    rpc:call(N2, ?MODULE, remote_load, [Tabs,4]),
    io:format("Initialized ~n",[]),

    
    T3 = rpc:call(N2, ?MODULE, remote_load, [Tabs,1]),
    io:format("Loading from net with 1 loader ~p msec~n",[T3/1000]),
    
    T4 = rpc:call(N2, ?MODULE, remote_load, [Tabs,4]),
    io:format("Loading from net with 4 loader ~p msec~n",[T4/1000]),

    ok.

remote_load(Tabs,Loaders) ->
    ok = mnesia:start([{no_table_loaders, Loaders}]),
%%    io:format("~p ~n", [mnesia_controller:get_info(500)]),
    {Time, ok} = timer:tc(mnesia, wait_for_tables, [Tabs, infinity]),
    timer:sleep(1000), mnesia:stop(), timer:sleep(1000),
    Time.
