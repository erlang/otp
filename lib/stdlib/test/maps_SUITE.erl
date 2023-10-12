%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
%%%----------------------------------------------------------------
%%% Purpose: Test suite for the 'maps' module.
%%%-----------------------------------------------------------------

-module(maps_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).

-export([t_update_with_3/1, t_update_with_4/1,
         t_get_3/1, t_filter_2/1, t_filtermap_2/1,
         t_fold_3/1,t_map_2/1,t_size_1/1, t_foreach_2/1,
         t_iterator_1/1, t_iterator_2/1,
         t_iterator_valid/1,
         t_put_opt/1, t_merge_opt/1,
         t_with_2/1,t_without_2/1,
         t_intersect/1, t_intersect_with/1,
         t_merge_with/1, t_from_keys/1,
         error_info/1,
         t_from_list_kill_process/1,
         t_from_keys_kill_process/1,
         t_values_kill_process/1,
         t_keys_kill_process/1,
         t_from_list_check_trapping/1,
         t_from_keys_check_trapping/1,
         t_keys_trapping/1,
         t_values_trapping/1,
         t_groups_from_list/1]).

-define(badmap(V,F,Args), {'EXIT', {{badmap,V}, [{maps,F,Args,_}|_]}}).
-define(badkey(K,F,Args), {'EXIT', {{badkey,K}, [{maps,F,Args,_}|_]}}).
-define(badarg(F,Args), {'EXIT', {badarg, [{maps,F,Args,_}|_]}}).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [t_update_with_3,t_update_with_4,
     t_get_3,t_filter_2,t_filtermap_2,
     t_fold_3,t_map_2,t_size_1,t_foreach_2,
     t_iterator_1,t_iterator_2,
     t_iterator_valid,
     t_put_opt,t_merge_opt,
     t_with_2,t_without_2,
     t_intersect, t_intersect_with,
     t_merge_with, t_from_keys,
     error_info,
     t_from_list_kill_process,
     t_from_keys_kill_process,
     t_values_kill_process,
     t_keys_kill_process,
     t_from_list_check_trapping,
     t_from_keys_check_trapping,
     t_keys_trapping,
     t_values_trapping,
     t_groups_from_list].

t_from_list_kill_process(Config) when is_list(Config) ->
    Killer = self(),
    {Child, ChildRef} =
        spawn_monitor(
          fun() ->
                  MapSize = 10000,
                  List = [{X, X} || X <- lists:seq(1, MapSize)],
                  (fun Loop(Round) ->
                          io:format("Sarting Round ~p~n", [Round]),
                          case Round =:= 0 of
                              true ->
                                  Killer ! starting;
                              false ->
                                  ok
                          end,
                          Map = maps:from_list(List),
                          io:format("Child survived round ~p ~p~n", [Round, maps:size(Map)]),
                          Loop(Round + 1)
                  end)(0)
          end),
    receive
        starting -> ok
    end,
    exit(Child, kill),
    %wait for exit message
    receive
        {'DOWN', ChildRef, process, _, killed} -> ok
    end,
    ok.

t_from_keys_kill_process(Config) when is_list(Config) ->
    Killer = self(),
    {Child, ChildRef} =
        spawn_monitor(
          fun() ->
                  MapSize = 10000,
                  List = lists:seq(1, MapSize),
                  (fun Loop(Round) ->
                          io:format("Sarting Round ~p~n", [Round]),
                          case Round =:= 0 of
                              true ->
                                  Killer ! starting;
                              false ->
                                  ok
                          end,
                          Map = maps:from_keys(List, ok),
                          io:format("Child survived round ~p ~p~n", [Round, maps:size(Map)]),
                          Loop(Round + 1)
                  end)(0)
          end),
    receive
        starting -> ok
    end,
    exit(Child, kill),
    %wait for exit message
    receive
        {'DOWN', ChildRef, process, _, killed} -> ok
    end,
    ok.

t_keys_kill_process(Config) when is_list(Config) ->
    Killer = self(),
    {Child, ChildRef} =
        spawn_monitor(
          fun() ->
                  MapSize = 500000,
                  List = lists:seq(1, MapSize),
                  Map = maps:from_keys(List, ok),
                  (fun Loop(Round) ->
                          io:format("Sarting Round ~p~n", [Round]),
                          case Round =:= 0 of
                              true ->
                                  Killer ! starting;
                              false ->
                                  ok
                          end,
                          Keys = maps:keys(Map),
                          io:format("Child survived round ~p ~p~n", [Round, hd(Keys)]),
                          Loop(Round + 1)
                  end)(0)
          end),
    receive
        starting -> ok
    end,
    exit(Child, kill),
    %wait for exit message
    receive
        {'DOWN', ChildRef, process, _, killed} -> ok
    end,
    ok.

t_values_kill_process(Config) when is_list(Config) ->
    Killer = self(),
    {Child, ChildRef} =
        spawn_monitor(
          fun() ->
                  MapSize = 500000,
                  List = [{V, V} || V <- lists:seq(1, MapSize)],
                  Map = maps:from_list(List),
                  (fun Loop(Round) ->
                          io:format("Sarting Round ~p~n", [Round]),
                          case Round =:= 0 of
                              true ->
                                  Killer ! starting;
                              false ->
                                  ok
                          end,
                          Values = maps:values(Map),
                          io:format("Child survived round ~p ~p~n", [Round, hd(Values)]),
                          Loop(Round + 1)
                  end)(0)
          end),
    receive
        starting -> ok
    end,
    exit(Child, kill),
    %wait for exit message
    receive
        {'DOWN', ChildRef, process, _, killed} -> ok
    end,
    ok.

%% Check that maps:from_list/1 is trapping
t_from_list_check_trapping(Config) when is_list(Config) ->
    FunToExecute =
        fun() ->
                ListTmp = [{X,X} || X <- [X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1,100000)])]],
                List = [{-1,-1} | ListTmp],
                M = maps:from_list(List),
                %% To avoid that compiler optimizes away the call above
                maps:get(-1, M)
        end,
    NoYields = count_nr_of_yields(FunToExecute, {maps, from_list, 1}),
    io:format("No of yields: ~p~n", [NoYields]),
    true = NoYields > 2.

%% Check that maps:from_keys/2 is trapping
t_from_keys_check_trapping(Config) when is_list(Config) ->
    FunToExecute =
        fun() ->
                ListTmp = [X || X <- [X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1,1000000)])]],
                List = [-1 | ListTmp],
                M = maps:from_keys(List, ok),
                %% To avoid that compiler optimizes away the call above
                _ = maps:get(-1, M),
                ok
        end,
    NoYields = count_nr_of_yields(FunToExecute, {maps, from_keys, 2}),
    io:format("No of yields: ~p~n", [NoYields]),
    true = NoYields > 2.

%% Check that maps:keys/1 is trapping
t_keys_trapping(Config) when is_list(Config) ->
    FunToExecute =
        fun() ->
                ListTmp = [X || X <- [X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1,1000000)])]],
                List = [-1 | ListTmp],
                M = maps:from_keys(List, ok),
                Keys = maps:keys(M),
                %% To avoid that compiler optimizes away the call above
                [-1 | _] = lists:sort(Keys),
                ok
        end,
    NoYields = count_nr_of_yields(FunToExecute, {maps, keys, 1}),
    io:format("No of yields: ~p~n", [NoYields]),
    true = NoYields > 2.

%% Check that maps:values/1 is trapping
t_values_trapping(Config) when is_list(Config) ->
    FunToExecute =
        fun() ->
                ListTmp = [{X,X} || X <- [X || {_,X} <- lists:sort([ {rand:uniform(), N} || N <- lists:seq(1,1000000)])]],
                List = [{-1,-1} | ListTmp],
                M = maps:from_list(List),
                Values = maps:values(M),
                %% To avoid that compiler optimizes away the call above
                [-1 | _] = lists:sort(Values),
                ok
        end,
    NoYields = count_nr_of_yields(FunToExecute, {maps, values, 1}),
    io:format("No of yields: ~p~n", [NoYields]),
    true = NoYields > 2.

count_nr_of_yields(FunToExecute, FunctionId) ->
    Tracer = self(),
    {Pid, Mon} =
        spawn_monitor(
          fun () ->
                  erlang:trace(self(),true,[running,{tracer,Tracer}]),
                  FunToExecute()
          end),
    receive
	{'DOWN', Mon, process, Pid, Reason} ->
	    normal = Reason
    end,
    TD = erlang:trace_delivered(Pid),
    receive
	{trace_delivered, Pid, TD} ->
            trace_get_number_of_yields(Pid, FunctionId, 0)
    end.

trace_get_number_of_yields(P, TrapFunc, N) ->
    receive
	{trace, P, out, TrapFunc} ->
	    receive
		{trace, P, in, TrapFunc} ->
                    trace_get_number_of_yields(P, TrapFunc, N+1)
	    after 0 ->
		    exit(trap_sched_mismatch)
	    end;
	{trace, P, out, Func} ->
	    receive
		{trace, P, in, Func} ->
		    trace_get_number_of_yields(P, TrapFunc, N)
	    after 0 ->
		    exit(other_sched_mismatch)
	    end
    after 0 ->
	    N
    end.

t_from_keys(Config) when is_list(Config) ->
    Map0 = maps:from_keys(["a", 2, {three}], value),
    3 = map_size(Map0),
    #{"a":=value,2:=value,{three}:=value} = Map0,

    Map1 = maps:from_keys([1, 2, 2], {complex,value}),
    2 = map_size(Map1),
    #{1:={complex,value},2:={complex,value}} = Map1,

    Map2 = maps:from_keys([], value),
    0 = map_size(Map2),

    ?badarg(from_keys,[[a|b],value]) = (catch maps:from_keys([a|b],value)),
    ?badarg(from_keys,[not_list,value]) = (catch maps:from_keys(not_list,value)),
    ok.

t_update_with_3(Config) when is_list(Config) ->
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    Map = #{ key1 => V1, key2 => V2, "key3" => V3 },
    Fun = fun(V) -> [V,V,{V,V}] end,

    #{ key1 := [V1,V1,{V1,V1}] } = maps:update_with(key1,Fun,Map),
    #{ key2 := [V2,V2,{V2,V2}] } = maps:update_with(key2,Fun,Map),
    #{ "key3" := [V3,V3,{V3,V3}] } = maps:update_with("key3",Fun,Map),

    %% error case
    ?badmap(b,update_with,[[a,b],a,b]) = (catch maps:update_with([a,b],id(a),b)),
    ?badarg(update_with,[[a,b],a,#{}]) = (catch maps:update_with([a,b],id(a),#{})),
    ?badkey([a,b],update_with,[[a,b],Fun,#{}]) = (catch maps:update_with([a,b],Fun,#{})),
    ok.

t_update_with_4(Config) when is_list(Config) ->
    V1 = value1,
    V2 = <<"value2">>,
    V3 = "value3",
    Map = #{ key1 => V1, key2 => V2, "key3" => V3 },
    Fun = fun(V) -> [V,V,{V,V}] end,
    Init = 3,

    #{ key1 := [V1,V1,{V1,V1}] } = maps:update_with(key1,Fun,Init,Map),
    #{ key2 := [V2,V2,{V2,V2}] } = maps:update_with(key2,Fun,Init,Map),
    #{ "key3" := [V3,V3,{V3,V3}] } = maps:update_with("key3",Fun,Init,Map),

    #{ key3 := Init } = maps:update_with(key3,Fun,Init,Map),

    %% error case
    ?badmap(b,update_with,[[a,b],a,b]) = (catch maps:update_with([a,b],id(a),b)),
    ?badarg(update_with,[[a,b],a,#{}]) = (catch maps:update_with([a,b],id(a),#{})),
    ok.


t_get_3(Config) when is_list(Config) ->
    Map = #{ key1 => value1, key2 => value2 },
    DefaultValue = "Default value",
    value1 = maps:get(key1, Map, DefaultValue),
    value2 = maps:get(key2, Map, DefaultValue),
    DefaultValue = maps:get(key3, Map, DefaultValue),

    %% error case
    {'EXIT', {{badmap,a}, _}} = (catch maps:get([a,b],id(a),def)),

    ok.

t_without_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100) -- Ki]),
    M1 = maps:without([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,without,[[a,b],a]) = (catch maps:without([a,b],id(a))),
    ?badmap(a,without,[{a,b},a]) = (catch maps:without({a,b},id(a))),
    ?badmap({0,<<>>,97},without,[[],{0,<<>>,97}]) = (catch maps:without([], {0,<<>>,97})),
    ?badmap({0,<<>>,97},without,[[false, -20, -8],{0,<<>>,97}]) = (catch maps:without([false, -20, -8], {0, <<>>, 97})),
    ?badarg(without,[a,#{}]) = (catch maps:without(a,#{})),
    ok.

t_with_2(_Config) ->
    Ki = [11,22,33,44,55,66,77,88,99],
    M0 = maps:from_list([{{k,I},{v,I}}||I<-lists:seq(1,100)]),
    M1 = maps:from_list([{{k,I},{v,I}}||I<-Ki]),
    M1 = maps:with([{k,I}||I <- Ki],M0),

    %% error case
    ?badmap(a,with,[[a,b],a]) = (catch maps:with([a,b],id(a))),
    ?badmap(a,with,[{a,b},a]) = (catch maps:with({a,b},id(a))),
    ?badmap({0,<<>>,97},with,[[],{0,<<>>,97}]) = (catch maps:with([], {0,<<>>,97})),
    ?badmap({0,<<>>,97},with,[[false, -20, -8],{0,<<>>,97}]) = (catch maps:with([false, -20, -8], {0, <<>>, 97})),
    ?badarg(with,[a,#{}]) = (catch maps:with(a,#{})),
    ok.

t_filter_2(Config) when is_list(Config) ->
    M = #{a => 2, b => 3, c=> 4, "a" => 1, "b" => 2, "c" => 4},
    Pred1 = fun(K,V) -> is_atom(K) andalso (V rem 2) =:= 0 end,
    Pred2 = fun(K,V) -> is_list(K) andalso (V rem 2) =:= 0 end,
    #{a := 2,c := 4} = maps:filter(Pred1,M),
    #{"b" := 2,"c" := 4} = maps:filter(Pred2,M),
    #{a := 2,c := 4} = maps:filter(Pred1,maps:iterator(M)),
    #{"b" := 2,"c" := 4} = maps:filter(Pred2,maps:iterator(M)),
    %% error case
    ?badmap(a,filter,[_,a]) = (catch maps:filter(fun(_,_) -> true end,id(a))),
    ?badmap({a,b,c},filter,[_,{a,b,c}]) = (catch maps:filter(fun(_,_) -> true end,id({a,b,c}))),
    ?badarg(filter,[<<>>,#{}]) = (catch maps:filter(id(<<>>),#{})),
    ok.

t_filtermap_2(Config) when is_list(Config) ->
    M0 = maps:from_list([{I, I} || I <- lists:seq(1, 30)]),
    Pred = fun(K,_) when K=<10 -> true; (K,_) when K=<20 -> false; (_,V) -> {true, V * V} end,
    M1 = maps:filtermap(Pred, M0),
    M2 = maps:filtermap(Pred, maps:iterator(M0)),
    #{1 := 1, 10 := 10, 21 := 21 * 21, 30 := 30 * 30} = M1,
    false = maps:is_key(11, M1),
    false = maps:is_key(20, M1),
    true = M1 =:= M2,
    %% error case
    ?badmap(a,filtermap,[_,a]) = (catch maps:filtermap(fun(_,_) -> true end,id(a))),
    ?badmap({a,b,c},filtermap,[_,{a,b,c}]) = (catch maps:filtermap(fun(_,_) -> true end,id({a,b,c}))),
    ?badarg(filtermap,[<<>>,#{}]) = (catch maps:filtermap(id(<<>>),#{})),
    ok.

t_fold_3(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    Tot0 = lists:sum(Vs),
    Tot1 = maps:fold(fun({k,_},V,A) -> A + V end, 0, M0),
    true = Tot0 =:= Tot1,
    Tot2 = maps:fold(fun({k,_},V,A) -> A + V end, 0, maps:iterator(M0)),
    true = Tot0 =:= Tot2,

    %% error case
    ?badmap(a,fold,[_,0,a]) = (catch maps:fold(fun(_,_,_) -> ok end,0,id(a))),
    ?badmap({a,b,c},fold,[_,0,{a,b,c}]) = (catch maps:fold(fun(_,_,_) -> ok end,0,id({a,b,c}))),
    ?badarg(fold,[<<>>,0,#{}]) = (catch maps:fold(id(<<>>),0,#{})),
    ok.

t_map_2(Config) when is_list(Config) ->
    Vs = lists:seq(1,200),
    M0 = maps:from_list([{{k,I},I}||I<-Vs]),
    #{ {k,1} := 1, {k,200} := 200} = M0,
    M1 = maps:map(fun({k,_},V) -> V + 42 end, M0),
    #{ {k,1} := 43, {k,200} := 242} = M1,
    M2 = maps:map(fun({k,_},V) -> V + 42 end, maps:iterator(M0)),
    #{ {k,1} := 43, {k,200} := 242} = M2,

    %% error case
    ?badmap(a,map,[_,a]) = (catch maps:map(fun(_,_) -> ok end, id(a))),
    ?badmap({a,b,c},map,[_,{a,b,c}]) = (catch maps:map(fun(_,_) -> ok end, id({a,b,c}))),
    ?badarg(map,[<<>>,#{}]) = (catch maps:map(id(<<>>),#{})),
    ok.

t_foreach_2(Config) when is_list(Config) ->
    %% error case
    ?badmap(a,foreach,[_,a]) = (catch maps:foreach(fun(_,_) -> ok end, id(a))),
    ?badmap([],foreach,[_,[]]) = (catch maps:foreach(fun(_,_) -> ok end, id([]))),
    ?badmap({},foreach,[_,{}]) = (catch maps:foreach(fun(_,_) -> ok end, id({}))),
    ?badmap(42,foreach,[_,42]) = (catch maps:foreach(fun(_,_) -> ok end, id(42))),
    ?badmap(<<>>,foreach,[_,<<>>]) = (catch maps:foreach(fun(_,_) -> ok end, id(<<>>))),
    ?badmap({a,b,c},foreach,[_,{a,b,c}]) = (catch maps:foreach(fun(_,_) -> ok end, id({a,b,c}))),

    ?badarg(foreach,[<<>>,#{}]) = (catch maps:foreach(id(<<>>),#{})),
    F0 = fun() -> ok end,
    F3 = fun(_, _, _) -> ok end,
    ?badarg(foreach,[F0, #{}]) = (catch maps:foreach(id(F0), #{})),
    ?badarg(foreach,[F3, #{}]) = (catch maps:foreach(id(F3), #{})),
    ?badarg(foreach,[a, #{}]) = (catch maps:foreach(id(a), #{})),
    ?badarg(foreach,[[], #{}]) = (catch maps:foreach(id([]), #{})),
    ?badarg(foreach,[{}, #{}]) = (catch maps:foreach(id({}), #{})),
    ?badarg(foreach,[42, #{}]) = (catch maps:foreach(id(42), #{})),
    ok.

t_iterator_1(Config) when is_list(Config) ->

    %% Small map test
    M0 = #{ a => 1, b => 2 },
    I0 = maps:iterator(M0),
    {K1,V1,I1} = maps:next(I0),
    {K2,V2,I2} = maps:next(I1),
    none = maps:next(I2),

    KVList = lists:sort([{K1,V1},{K2,V2}]),
    KVList = lists:sort(maps:to_list(M0)),
    KList = lists:sort([K1,K2]),
    KList = lists:sort(maps:keys(M0)),

    %% Large map test

    Vs2 = lists:seq(1,200),
    M2 = maps:from_list([{{k,I},I}||I<-Vs2]),
    KVList2 = lists:sort(iter_kv(maps:iterator(M2))),
    KVList2 = lists:sort(maps:to_list(maps:iterator(M2))),
    KVList2 = lists:sort(maps:to_list(M2)),

    %% Larger map test

    Vs3 = lists:seq(1,10000),
    M3 = maps:from_list([{{k,I},I}||I<-Vs3]),
    KVList3 = lists:sort(iter_kv(maps:iterator(M3))),
    KVList3 = lists:sort(maps:to_list(maps:iterator(M3))),
    KVList3 = lists:sort(maps:to_list(M3)),
    ok.

t_iterator_2(Config) when is_list(Config) ->

    AOrdCmpFun = fun(A, B) -> A =< B end,
    ARevCmpFun = fun(A, B) -> B =< A end,

    %% Small map test
    M0 = #{ a => 1, b => 2 },
    TOrdI0 = maps:iterator(M0, ordered),
    {K1 = a, V1 = 1, TOrdI1} = maps:next(TOrdI0),
    {K2 = b, V2 = 2, TOrdI2} = maps:next(TOrdI1),
    none = maps:next(TOrdI2),

    TRevI0 = maps:iterator(M0, reversed),
    {K2 = b, V2 = 2, TRevI1} = maps:next(TRevI0),
    {K1 = a, V1 = 1, TRevI2} = maps:next(TRevI1),
    none = maps:next(TRevI2),

    AOrdI0 = maps:iterator(M0, AOrdCmpFun),
    {K1 = a, V1 = 1, AOrdI1} = maps:next(AOrdI0),
    {K2 = b, V2 = 2, AOrdI2} = maps:next(AOrdI1),
    none = maps:next(AOrdI2),

    ARevI0 = maps:iterator(M0, ARevCmpFun),
    {K2 = b, V2 = 2, ARevI1} = maps:next(ARevI0),
    {K1 = a, V1 = 1, ARevI2} = maps:next(ARevI1),
    none = maps:next(ARevI2),

    OrdKVList = [{K1, V1}, {K2, V2}],
    OrdKVList = maps:to_list(TOrdI0),
    OrdKVList = maps:to_list(AOrdI0),

    RevKVList = [{K2, V2}, {K1, V1}],
    RevKVList = maps:to_list(TRevI0),
    RevKVList = maps:to_list(ARevI0),

    %% Large map test

    Vs2 = lists:seq(1, 200),
    OrdKVList2 = [{{k, I}, I} || I <- Vs2],
    M2 = maps:from_list(OrdKVList2),
    ok = iterator_2_check_order(M2, ordered, reversed),
    ok = iterator_2_check_order(M2, AOrdCmpFun, ARevCmpFun),

    %% Larger map test

    Vs3 = lists:seq(1, 10000),
    OrdKVList3 = [{{k, I}, I} || I <- Vs3],
    M3 = maps:from_list(OrdKVList3),
    ok = iterator_2_check_order(M3, ordered, reversed),
    ok = iterator_2_check_order(M3, AOrdCmpFun, ARevCmpFun),

    %% Float and integer keys

    M4 = #{-1.0 => a, 0.0 => b, -1 => c, 0 => d},
    OrdIter4 = maps:iterator(M4, ordered),
    [{-1, c}, {0, d}, {-1.0, a}, {0.0, b}] = maps:to_list(OrdIter4),
    ok = iterator_2_check_order(M4, ordered, reversed),

    ok.

iterator_2_option_to_fun(ordered) ->
    fun(A, B) -> erts_internal:cmp_term(A, B) =< 0 end;
iterator_2_option_to_fun(reversed) ->
    fun(A, B) -> erts_internal:cmp_term(B, A) =< 0 end;
iterator_2_option_to_fun(F) when is_function(F, 2) ->
    F.

iterator_2_check_order(M, OrdOption, RevOption) ->
    OrdCmpFun = iterator_2_option_to_fun(OrdOption),
    RevCmpFun = iterator_2_option_to_fun(RevOption),
    OrdKVCmpFun = fun({A, _}, {B, _}) -> OrdCmpFun(A, B) end,
    RevKVCmpFun = fun({A, _}, {B, _}) -> RevCmpFun(A, B) end,

    OrdKVList = lists:sort(OrdKVCmpFun, maps:to_list(M)),
    RevKVList = lists:sort(RevKVCmpFun, maps:to_list(M)),
    RevKVList = lists:reverse(OrdKVList),

    Iter = maps:iterator(M, undefined),
    OrdIter = maps:iterator(M, OrdOption),
    RevIter = maps:iterator(M, RevOption),

    OrdKVList = lists:sort(OrdKVCmpFun, iter_kv(Iter)),
    OrdKVList = lists:sort(OrdKVCmpFun, maps:to_list(Iter)),
    OrdKVList = iter_kv(OrdIter),
    OrdKVList = maps:to_list(OrdIter),

    RevKVList = iter_kv(RevIter),
    RevKVList = maps:to_list(RevIter),

    ok.

iter_kv(I) ->
    case maps:next(I) of
        none ->
            [];
        {K,V,NI} ->
            [{K,V} | iter_kv(NI)]
    end.

t_iterator_valid(Config) when is_list(Config) ->
    %% good iterators created via maps:iterator
    true = maps:is_iterator_valid(maps:iterator(#{})),
    true = maps:is_iterator_valid(maps:iterator(#{a => b})),
    true = maps:is_iterator_valid(maps:iterator(#{a => b, c => d})),
    true = maps:is_iterator_valid(maps:iterator(#{}, undefined)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b}, undefined)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b, c => d}, undefined)),
    true = maps:is_iterator_valid(maps:iterator(#{}, ordered)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b}, ordered)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b, c => d}, ordered)),
    true = maps:is_iterator_valid(maps:iterator(#{}, reversed)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b}, reversed)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b, c => d}, reversed)),
    true = maps:is_iterator_valid(maps:iterator(#{}, fun erlang:'=<'/2)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b}, fun erlang:'=<'/2)),
    true = maps:is_iterator_valid(maps:iterator(#{a => b, c => d}, fun erlang:'=<'/2)),

    %% good makeshift iterators
    true = maps:is_iterator_valid(none),
    true = maps:is_iterator_valid({a, b, none}),
    true = maps:is_iterator_valid({a, b, {c, d, none}}),
    true = maps:is_iterator_valid({a, b, {c, d, {e, f, none}}}),
    true = maps:is_iterator_valid({a, b, maps:iterator(#{})}),
    true = maps:is_iterator_valid({a, b, maps:iterator(#{c => d})}),

    %% not iterators
    false = maps:is_iterator_valid(no_iter),
    false = maps:is_iterator_valid(1),
    false = maps:is_iterator_valid(1.0),
    false = maps:is_iterator_valid([]),
    false = maps:is_iterator_valid("foo"),
    false = maps:is_iterator_valid(<<"foo">>),
    false = maps:is_iterator_valid(fun() -> ok end),
    false = maps:is_iterator_valid(self()),
    false = maps:is_iterator_valid(make_ref()),
    false = maps:is_iterator_valid(#{}),
    false = maps:is_iterator_valid({}),
    false = maps:is_iterator_valid({a}),
    false = maps:is_iterator_valid({a, b}),
    false = maps:is_iterator_valid({a, b, c, d}),

    %% bad makeshift iterators that only fail on later (ie, subsequent) calls to maps:next/1
    %%    maps:next({a, b, c}) -> {a, b, c}
    %%    maps:next(c) -> badarg
    false = maps:is_iterator_valid({a, b, c}),
    %%    maps:next({a, b, {c, d, e}}) -> {a, b, {c, d, e}}
    %%    maps:next({c, d, e}) -> {c, d, e}
    %%    maps:next(e) -> badarg
    false = maps:is_iterator_valid({a, b, {c, d, e}}),

    ok.

t_put_opt(Config) when is_list(Config) ->
    Value = id(#{complex => map}),
    Small = id(#{a => Value}),
    true = erts_debug:same(maps:put(a, Value, Small), Small),

    LargeBase = maps:from_list([{I,I}||I<-lists:seq(1,200)]),
    Large = LargeBase#{a => Value},
    true = erts_debug:same(maps:put(a, Value, Large), Large),
    ok.

t_merge_opt(Config) when is_list(Config) ->
    Small = id(#{a => 1}),
    true = erts_debug:same(maps:merge(#{}, Small), Small),
    true = erts_debug:same(maps:merge(Small, #{}), Small),
    true = erts_debug:same(maps:merge(Small, Small), Small),

    Large = maps:from_list([{I,I}||I<-lists:seq(1,200)]),
    true = erts_debug:same(maps:merge(#{}, Large), Large),
    true = erts_debug:same(maps:merge(Large, #{}), Large),
    true = erts_debug:same(maps:merge(Large, Large), Large),

    List = id([a|b]),
    ?badmap([a|b],merge,[[a|b],[a|b]]) = (catch maps:merge(List, List)),

    ok.

random_map(SizeConstant, InitSeed) ->
    {Ret, _} =
        lists:foldl(
          fun(_, {Map, Seed}) ->
                  rand:uniform_s(Seed),
                  {K, Seed2} = rand:uniform_s(SizeConstant, Seed),
                  {V, Seed3} = rand:uniform_s(SizeConstant*100, Seed2),
                  {Map#{K => V}, Seed3}
          end,
          {#{}, rand:seed_s(exsss, SizeConstant + InitSeed)},
          lists:seq(1, SizeConstant)),
    Ret.

check_map_combiners_same_small(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(
      fun(SizeConstant) ->
              lists:foreach(
                fun(SeedMult) ->
                   RandMap1 = random_map(SizeConstant,
                                         SizeConstant + 100000*SeedMult + Seed),
                   RandMap2 = random_map(SizeConstant,
                                         SizeConstant + 200000*SeedMult + Seed),
                   Comb1Res = MapCombiner1(RandMap1, RandMap2),
                   Comb2Res = MapCombiner2(RandMap1, RandMap2),
                   Comb1Res = Comb2Res
                end,
                lists:seq(1,100))

      end,
      lists:seq(1,10)).


check_map_combiners_same_large(MapCombiner1, MapCombiner2, Seed) ->
    lists:foreach(
      fun(SizeConstant) ->
              RandMap1 = random_map(SizeConstant, SizeConstant + Seed),
              RandMap2 = random_map(SizeConstant, SizeConstant + Seed),
              Comb1Res = MapCombiner1(RandMap1, RandMap2),
              Comb2Res = MapCombiner2(RandMap1, RandMap2),
              Comb1Res = Comb2Res
      end,
      [1000, 10000]),
    ok.

t_merge_with(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := {1,3}, 2 := {3,2}, 10 := 10} =
        maps:merge_with(fun(1, 1, 3) -> {1, 3};
                           (2, 3, 2) -> {3, 2}
                        end,
                        Small,
                        Large),

    %% Swapping input maps should reverse tuples

    #{1 := {3,1}, 2 := {2,3}, 10 := 10} =
        maps:merge_with(fun(1, V1, V2) -> {V1, V2};
                           (2, V1, V2) -> {V1, V2}
                        end,
                        Large,
                        Small),

    %% Swapping parameters in the output of the fun should also reverse
    %% tuples

    #{1 := {3,1}, 2 := {2,3}, 10 := 10} =
        maps:merge_with(fun(1, V1, V2) -> {V2, V1};
                           (2, V1, V2) -> {V2, V1}
                        end,
                        Small,
                        Large),

    %% Should give the same result as maps:merge/2 with the right combiner

    DefaultCombiner = fun(_, _, V2) -> V2 end,
    Merge2FromMerge3 = fun (M1, M2) -> maps:merge_with(DefaultCombiner, M1, M2) end,
    check_map_combiners_same_small(fun maps:merge/2, Merge2FromMerge3, 1),
    check_map_combiners_same_large(fun maps:merge/2, Merge2FromMerge3, 2),

    %% Should conceptually compute the same thing as
    %% lists:ukey_merge/2 with the right combiner

    MergeFromUKeyMerge =
        fun(M1, M2) ->
                L1 = lists:sort(maps:to_list(M1)),
                L2 = lists:sort(maps:to_list(M2)),
                %% ukeymerge takes from the first when collision
                ResList = lists:ukeymerge(1, L2, L1),
                maps:from_list(ResList)
        end,
    check_map_combiners_same_small(MergeFromUKeyMerge, Merge2FromMerge3, 3),
    check_map_combiners_same_large(MergeFromUKeyMerge, Merge2FromMerge3, 4),

    %% Empty maps

    Large = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                            Large,
                            #{}),
    Large = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                            #{},
                            Large),
    #{} = maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                          #{},
                          #{}),

    %% Errors

    {'EXIT', {badarg, _}} =
        (catch maps:merge_with(not_a_fun,#{},#{})),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:merge_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    ok.

t_intersect(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := 3,2 := 2} = maps:intersect(Small, Large),

    %% Swapping input maps can make a difference

    #{1 := 1, 2 := 3} = maps:intersect(Large, Small),

    %% Should conceptually compute the same thing as
    %% gb_sets:intersect/2 with the right combiner

    IntersectFromGBSets =
        fun(M1, M2) ->
                Map1Keys = maps:keys(M1),
                Map2Keys = maps:keys(M2),
                GBSet1 = gb_sets:from_list(Map1Keys),
                GBSet2 = gb_sets:from_list(Map2Keys),
                GBSetIntersection = gb_sets:intersection(GBSet1, GBSet2),
                IntersectList = gb_sets:to_list(GBSetIntersection),
                lists:foldl(
                  fun(Key, SoFar) ->
                          SoFar#{Key => maps:get(Key, M2)}
                  end,
                  #{},
                  IntersectList)
        end,
    check_map_combiners_same_small(fun maps:intersect/2,
                                   IntersectFromGBSets,
                                   11),
    check_map_combiners_same_large(fun maps:intersect/2,
                                   IntersectFromGBSets,
                                   13),

    %% Empty maps

    #{} = maps:intersect(Large, #{}),
    #{} = maps:intersect(#{}, Large),
    #{} = maps:intersect(#{}, #{}),

    %% Errors

    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect(a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:intersect(#{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect(a, b)),
    ok.

t_intersect_with(Config) when is_list(Config) ->
    Small = #{1 => 1, 2 => 3},
    Large = #{1 => 3, 2 => 2, 10=>10},
    #{1 := {1,3}, 2 := {3,2}} =
        maps:intersect_with(fun(1, 1, 3) -> {1, 3};
                               (2, 3, 2) -> {3, 2}
                            end,
                            Small,
                            Large),

    %% Swapping input maps should reverse tuples

    #{1 := {3,1}, 2 := {2,3}} =
        maps:intersect_with(fun(1, V1, V2) -> {V1, V2};
                               (2, V1, V2) -> {V1, V2}
                            end,
                            Large,
                            Small),

    %% Swapping parameters in the output of the fun should also reverse
    %% tuples

    #{1 := {3,1}, 2 := {2,3}} =
        maps:intersect_with(fun(1, V1, V2) -> {V2, V1};
                               (2, V1, V2) -> {V2, V1}
                            end,
                            Small,
                            Large),

    %% Should give the same result as intersect/2 with the right combiner

    DefaultCombiner = fun(_, _, V2) -> V2 end,
    Intersect2FromIntersect3 =
        fun (M1, M2) -> maps:intersect_with(DefaultCombiner, M1, M2) end,
    check_map_combiners_same_small(fun maps:intersect/2,
                                   Intersect2FromIntersect3,
                                   7),
    check_map_combiners_same_large(fun maps:intersect/2,
                                   Intersect2FromIntersect3,
                                   8),

    %% Empty maps

    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              Large,
                              #{}),
    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              #{},
                              Large),
    #{} = maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end,
                              #{},
                              #{}),

    %% Errors

    {'EXIT', {badarg, _}} =
        (catch maps:intersect_with(not_a_fun,#{},#{})),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(should_not_happen) end, a, #{})),
    {'EXIT', {{badmap, b}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, #{}, b)),
    {'EXIT', {{badmap, a}, _}} =
        (catch maps:intersect_with(fun(_K, _V1, _V2) -> error(ok) end, a, b)),
    ok.

t_size_1(Config) when is_list(Config) ->
      0 = maps:size(#{}),
     10 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,10)])),
     20 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,20)])),
     30 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,30)])),
     40 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,40)])),
     50 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,50)])),
     60 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,60)])),
    600 = maps:size(maps:from_list([{{"k",I},I}||I<-lists:seq(1,600)])),

    %% error case
    %%
    %% Note that the stack trace is ignored because the compiler may have
    %% rewritten maps:size/2 to map_size.
    {'EXIT', {{badmap,a}, _}} = (catch maps:size(id(a))),
    {'EXIT', {{badmap,<<>>}, _}} = (catch maps:size(id(<<>>))),
    ok.

t_groups_from_list(_Config) ->
    #{} = maps:groups_from_list(fun erlang:length/1, []),
    #{3 := ["tna","tac"], 5 := ["ognid"], 7 := ["olaffub"]} =
        maps:groups_from_list(
          fun erlang:length/1,
          fun lists:reverse/1,
          ["ant", "buffalo", "cat", "dingo"]
         ),
    #{0 := [2], 1 := [1, 3]} = maps:groups_from_list(fun(X) -> X rem 2 end, [1, 2, 3]).

error_info(_Config) ->
    BadIterator = [-1|#{}],
    BadIterator2 = {x, y, z},
    GoodIterator = maps:iterator(#{}),
    BadOrder = fun(_) -> true end,
    GoodOrder = fun(A, B) -> A =< B end,

    L = [
         {filter, [fun(_, _) -> true end, abc]},
         {filter, [fun(_, _) -> true end, BadIterator]},
         {filter, [fun(_, _) -> true end, BadIterator2]},
         {filter, [bad_fun, BadIterator],[{1,".*"},{2,".*"}]},
         {filter, [bad_fun, BadIterator2],[{1,".*"},{2,".*"}]},
         {filter, [bad_fun, GoodIterator]},

         {filtermap, [fun(_, _) -> true end, abc]},
         {filtermap, [fun(_, _) -> true end, BadIterator]},
         {filtermap, [fun(_, _) -> true end, BadIterator2]},
         {filtermap, [fun(_) -> true end, GoodIterator]},
         {filtermap, [fun(_) -> ok end, #{}]},

         {find, [key, no_map]},

         {fold, [fun(_, _, _) -> true end, init, abc]},
         {fold, [fun(_, _, _) -> true end, init, BadIterator]},
         {fold, [fun(_, _, _) -> true end, init, BadIterator2]},
         {fold, [fun(_) -> true end, init, GoodIterator]},
         {fold, [fun(_) -> ok end, init, #{}]},

         {foreach, [fun(_, _) -> ok end, no_map]},
         {foreach, [fun(_, _) -> ok end, BadIterator]},
         {foreach, [fun(_, _) -> ok end, BadIterator2]},
         {foreach, [fun(_) -> ok end, GoodIterator]},
         {foreach, [fun(_) -> ok end, #{}]},

         {from_keys, [#{a => b}, whatever]},
         {from_keys, [[a|b], whatever]},

         {from_list, [#{a => b}]},
         {from_list, [[a|b]]},

         {get, [key, #{}]},
         {get, [key, {no,map}]},
         {get, [key, {no,map}, default]},

         {groups_from_list, [not_a_fun, []]},
         {groups_from_list, [fun hd/1, not_a_list]},

         {groups_from_list, [not_a_fun, fun(_) -> ok end, []]},
         {groups_from_list, [fun(_) -> ok end, not_a_fun, []]},
         {groups_from_list, [fun(_) -> ok end, fun(_) -> ok end, not_a_list]},

         {intersect, [#{a => b}, y]},
         {intersect, [x, #{a => b}]},
         {intersect, [x, y],[{1,".*"},{2,".*"}]},

         {intersect_with, [fun(_, _, _) -> ok end, #{a => b}, y]},
         {intersect_with, [fun(_, _, _) -> ok end, x, #{a => b}]},
         {intersect_with, [fun(_, _, _) -> ok end, x, y],[{2,".*"},{3,".*"}]},
         {intersect_with, [fun(_, _) -> ok end, #{}, #{}]},

         {is_iterator_valid, [GoodIterator], [no_fail]},
         {is_iterator_valid, [BadIterator], [no_fail]},
         {is_iterator_valid, [BadIterator2], [no_fail]},

         {is_key,[key, no_map]},

         {iterator,[{no,map}]},

         {iterator, [{no,map}, undefined], [{1, ".*"}]},
         {iterator, [{no,map}, ordered], [{1, ".*"}]},
         {iterator, [{no,map}, reversed], [{1, ".*"}]},
         {iterator, [{no,map}, GoodOrder], [{1, ".*"}]},
         {iterator, [#{a => b}, BadOrder], [{2, ".*"}]},
         {iterator, [{no,map}, BadOrder], [{1, ".*"}, {2, ".*"}]},

         {keys, [{no,map}]},

         {map, [fun(_, _) -> true end, abc]},
         {map, [fun(_, _) -> true end, BadIterator]},
         {map, [fun(_, _) -> true end, BadIterator2]},
         {map, [fun(_) -> true end, GoodIterator]},
         {map, [fun(_) -> ok end, #{}]},

         {merge,[#{a => b}, {a,b}]},
         {merge,[{x,y}, #{a => b}]},
         {merge,[{x,y}, {a,b}],[{1,".*"},{2,".*"}]},

         {merge_with, [fun(_, _) -> ok end, #{}, #{}]},
         {merge_with, [a, b, c],[{1,".*"},{2,".*"},{3,".*"}]},

         {next,[no_iterator]},
         {next,[BadIterator]},

         {put, [key, value, {no,map}]},

         {remove,[key, {no,map}]},

         {size, [no_map]},

         {take, [key, no_map]},

         {to_list,[xyz]},
         {to_list,[BadIterator]},
         {to_list,[BadIterator2]},

         {update,[key, value, no_map]},

         {update_with, [key, fun(_) -> ok end, {no,map}]},
         {update_with, [key, fun() -> ok end, #{}]},

         {update_with, [key, fun(_) -> ok end, init, {no,map}]},
         {update_with, [key, fun() -> ok end, init, #{}]},

         {values,[no_map]},

         {with, [not_list, #{}]},
         {with, [[1,2,3], {no,map}]},

         {without, [not_list, #{}]},
         {without, [[1,2,3], {no,map}]}
        ],
    error_info_lib:test_error_info(maps, L).

id(I) -> I.
