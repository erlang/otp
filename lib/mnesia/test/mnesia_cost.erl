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
-export([go/0, go/1]).

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
