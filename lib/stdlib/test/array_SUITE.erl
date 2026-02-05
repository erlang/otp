%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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

-module(array_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([
         new_test/1,
         fix_test/1,
         relax_test/1,
         resize_test/1,
         set_get_test/1,
         to_list_test/1,
         sparse_to_list_test/1,
         from_list_test/1,
         from_test/1,
         to_orddict_test/1,
         sparse_to_orddict_test/1,
         from_orddict_test/1,
         map_test/1,
         sparse_map_test/1,
         foldl_test/1,
         sparse_foldl_test/1,
         foldr_test/1,
         sparse_foldr_test/1,
         import_export/1,
         doctests/1
        ]).


-export([t/0,t/1]).

-import(array,
        [new/0, new/1, new/2, is_array/1, set/3, get/2, %size/1,
         sparse_size/1, default/1, reset/2, to_list/1, sparse_to_list/1,
         from/2, from/3,
         from_list/1, from_list/2, to_orddict/1, sparse_to_orddict/1,
         from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
         foldr/3, sparse_foldl/3, sparse_foldr/3, fix/1, relax/1, is_fix/1,
         resize/1, resize/2]).

%%
%% all/1
%%
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [new_test, fix_test, relax_test, resize_test,
     set_get_test, to_list_test, sparse_to_list_test,
     from_test,
     from_list_test, to_orddict_test, sparse_to_orddict_test,
     from_orddict_test, map_test, sparse_map_test,
     foldl_test, sparse_foldl_test, foldr_test, sparse_foldr_test,
     import_export, doctests].

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


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

-define(LEAFSIZE,8).
-define(NODESIZE,?LEAFSIZE).

-record(array,  {size,          %% number of defined entries
                 fix,           %% not automatically growing
                 default,       %% the default value (usually 'undefined')
                 cache,         %% cached leaf tuple
                 cache_index,   %% low index of cache
                 elements,      %% the tuple tree
                 max
                }).

-define(test(Expr), begin Expr end).

-define(LET(Var,Expr, Test), begin (fun(Var) -> Test end)(Expr) end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some helpers to be able to run the tests without testserver
%%%%%%%%%%%%%%%%%%%%%%%%%
t() -> t(all()--[doctests]).

t(What) when not is_list(What) ->
    t([What]);
t(What) ->
    lists:foreach(fun(T) ->
                          io:format("Test ~p ~n",[T]),
                          try
                              ?MODULE:T([])
                          catch _E:_R:_S ->
                                  Line = get(test_server_loc),
                                  io:format("Failed ~p:~p ~p ~p~n   ~p~n",
                                            [T,Line,_E,_R,_S])
                          end
                  end, What).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actual tests

new_test(_Config) ->
    N0 = ?LEAFSIZE,
    N01 = N0+1,
    N1 = ?NODESIZE*N0,
    N11 = N1+1,
    N2 = ?NODESIZE*N1,
    ?test(new()),

    ?test(new([])),
    ?test(new(10)),
    ?test(new({size,10})),
    ?test(new(fixed)),
    ?test(new({fixed,true})),
    ?test(new({fixed,false})),
    ?test(new({default,undefined})),
    ?test(new([{size,100},{fixed,false},{default,undefined}])),
    ?test(new([100,fixed,{default,0}])),

    ?assert(new() =:= new([])),
    ?assert(new() =:= new([{size,0},{default,undefined},{fixed,false}])),
    ?assert(new() =:= new(0, {fixed,false})),
    ?assert(new(fixed) =:= new(0)),
    ?assert(new(fixed) =:= new(0, [])),
    ?assert(new(10) =:= new([{size,0},{size,5},{size,10}])),
    ?assert(new(10) =:= new(0, {size,10})),
    ?assert(new(10, []) =:= new(10, [{default,undefined},{fixed,true}])),

    ?assertError(badarg, new(-1)),
    ?assertError(badarg, new(10.0)),
    ?assertError(badarg, new(undefined)),
    ?assertError(badarg, new([undefined])),
    ?assertError(badarg, new([{default,0} | fixed])),

    ?assertError(badarg, new(-1, [])),
    ?assertError(badarg, new(10.0, [])),
    ?assertError(badarg, new(undefined, [])),

    ?assertMatch(#array{size=0,fix=false,default=undefined},
                  new()),
    ?assertMatch(#array{size=0,fix=true,default=undefined},
                  new(fixed)),
    ?assertMatch(#array{size=N0,fix=false},
                  new(N0, {fixed,false})),
    ?assertMatch(#array{size=N01,fix=false},
                  new(N01, {fixed,false})),
    ?assertMatch(#array{size=N1,fix=false},
                  new(N1, {fixed,false})),
    ?assertMatch(#array{size=N11,fix=false},
                  new(N11, {fixed,false})),
    ?assertMatch(#array{size=N2, fix=false, default=42},
                  new(N2, [{fixed,false},{default,42}])),

    ?assert(0 =:= array:size(new())),
    ?assert(17 =:= array:size(new(17))),
    ?assert(8 =:= array:size(array:set(7,0,new()))),
    ?assert(100 =:= array:size(array:set(99,0,new()))),
    ?assert(100 =:= array:size(array:set(7,0,array:set(99,0,new())))),
    ?assert(100 =:= array:size(array:set(99,0,array:set(7,0,new())))),
    ?assertError(badarg, array:size({bad_data,gives_error})),

    ?assert(undefined =:= default(new())),
    ?assert(4711 =:= default(new({default,4711}))),
    ?assert(0 =:= default(new(10, {default,0}))),
    ?assertError(badarg, default({bad_data,gives_error})),

    ?assert(is_array(new())),
    ?assert(false =:= is_array({foobar, 23, 23})),
    ?assert(false =:= is_array(#array{size=bad})),
    %%?assert(false =:= is_array(#array{fix=bad})),
    ?assert(is_array(new(10))),
    ?assert(is_array(new(10, {fixed,false}))).

fix_test(_Config) ->
    ?assert(is_array(fix(new()))),
    %%?assert(fix(new()) =:= new(fixed)),

    ?assertNot(is_fix(new())),
    ?assertNot(is_fix(new([]))),
    ?assertNot(is_fix(new({fixed,false}))),
    ?assertNot(is_fix(new(10, {fixed,false}))),
    ?assert(is_fix(new({fixed,true}))),
    ?assert(is_fix(new(fixed))),
    ?assert(is_fix(new(10))),
    ?assert(is_fix(new(10, []))),
    ?assert(is_fix(new(10, {fixed,true}))),
    ?assert(is_fix(fix(new()))),
    ?assert(is_fix(fix(new({fixed,false})))),

    ?test(set(0, 17, new())),
    ?assertError(badarg, set(0, 17, new(fixed))),
    ?assertError(badarg, set(1, 42, fix(set(0, 17, new())))),

    ?test(set(9, 17, new(10))),
    ?assertError(badarg, set(10, 17, new(10))),
    ?assertError(badarg, set(10, 17, fix(new(10, {fixed,false})))).

relax_test(_Config) ->
    [?assert(is_array(relax(new(fixed)))),
     ?assertNot(is_fix(relax(fix(new())))),
     ?assertNot(is_fix(relax(new(fixed)))),

     ?assert(new() =:= relax(new(fixed))),
     ?assert(new() =:= relax(new(0))),
     ?assert(new(17, {fixed,false}) =:= relax(new(17)))
  %, ?assert(new(100, {fixed,false})
  %           =:= relax(fix(new(100, {fixed,false}))))
    ].

resize_test(_Config) ->
    [?assert(resize(0, new()) =:= new()),
     ?assert(resize(99, new(99)) =:= new(99)),
     ?assert(resize(99, relax(new(99))) =:= relax(new(99))),
     ?assert(is_fix(resize(100, new(10)))),
     ?assertNot(is_fix(resize(100, relax(new(10))))),

     ?assert(array:size(resize(100, new())) =:= 100),
     ?assert(array:size(resize(0, new(100))) =:= 0),
     ?assert(array:size(resize(99, new(10))) =:= 99),
     ?assert(array:size(resize(99, new(1000))) =:= 99),

     ?assertError(badarg, set(99, 17, new(10))),
     ?test(set(99, 17, resize(100, new(10)))),
     ?assertError(badarg, set(100, 17, resize(100, new(10)))),

     ?test(set(9, 17, resize(10, new(100)))),
     ?assertError(badarg, set(10, 17, resize(10, new(100)))),

     ?test(set(9, 17, resize(10, fix(set(99, 17, new()))))),
     ?assertError(badarg, set(10, 17, resize(10, fix(set(99, 17, new()))))),

     ?assert(17 =:= get(99, resize(100, set(99, 17, set(999, 17, new(1000)))))),

     ?assert(undefined =:= get(55, resize(100, resize(10, set(55, 17, new()))))),
     ?assert(17 =:= get(55, resize(100, resize(56, set(55, 17, new()))))),
     ?assert(undefined =:= get(55, resize(100, resize(55, set(55, 17, new()))))),

     ?assertError(badarg, get(0, resize(0, set(0, 17, new(100))))),
     ?assert(undefined =:= get(0, resize(0, set(0, 17, set(99, 17, new()))))),

     ?assert(array:size(resize(new())) =:= 0),
     ?assert(array:size(resize(new(8))) =:= 0),
     ?assert(array:size(resize(array:set(7, 0, new()))) =:= 8),
     ?assert(array:size(resize(array:set(7, 0, new(10)))) =:= 8),
     ?assert(array:size(resize(array:set(99, 0, new(10,{fixed,false}))))
              =:= 100),
     ?assert(array:size(resize(array:set(7, undefined, new()))) =:= 0),
     ?assert(array:size(resize(array:from_list([1,2,3,undefined])))
              =:= 3),
     ?assert(array:size(
                resize(array:from_orddict([{3,0},{17,0},{99,undefined}])))
              =:= 18),
     ?assertError(badarg, resize(foo, bad_argument))
    ].

set_get_test(_Config) ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    [?assert(array:get(0, new()) =:= undefined),
     ?assert(array:get(1, new()) =:= undefined),
     ?assert(array:get(99999, new()) =:= undefined),

     ?assert(array:get(0, new(1)) =:= undefined),
     ?assert(array:get(0, new(1,{default,0})) =:= 0),
     ?assert(array:get(9, new(10)) =:= undefined),

     ?assertError(badarg, array:get(0, new(fixed))),
     ?assertError(badarg, array:get(1, new(1))),
     ?assertError(badarg, array:get(-1, new(1))),
     ?assertError(badarg, array:get(10, new(10))),
     ?assertError(badarg, array:set(-1, foo, new(10))),
     ?assertError(badarg, array:set(10, foo, no_array)),

     ?assert(array:size(set(0, 17, new())) =:= 1),
     ?assert(array:size(set(N1-1, 17, new())) =:= N1),
     ?assert(array:size(set(0, 42, set(0, 17, new()))) =:= 1),
     ?assert(array:size(set(9, 42, set(0, 17, new()))) =:= 10),

     ?assert(array:get(0, set(0, 17, new())) =:= 17),
     ?assert(array:get(0, set(1, 17, new())) =:= undefined),
     ?assert(array:get(1, set(1, 17, new())) =:= 17),

     ?assert(array:get(0, fix(set(0, 17, new()))) =:= 17),
     ?assertError(badarg, array:get(1, fix(set(0, 17, new())))),

     ?assert(array:get(N1-2, set(N1-1, 17, new())) =:= undefined),
     ?assert(array:get(N1-1, set(N1-1, 17, new())) =:= 17),
     ?assertError(badarg, array:get(N1, fix(set(N1-1, 17, new())))),

     ?assert(array:get(0, set(0, 42, set(0, 17, new()))) =:= 42),

     array:get(12, array:set(12, foo, array:from_list(lists:seq(1, 12)))),

     ?assertError(badarg, array:get(0, reset(11, new([{size,10}])))),
     ?assertError(badarg, array:get(0, reset(-1, new([{size,10}])))),
     ?assert(array:get(0, reset(0,  new())) =:= undefined),
     ?assert(array:get(0, reset(0,  set(127, set_cache, set(0,  17, new())))) =:= undefined),
     ?assert(array:get(9, reset(9,  set(127, set_cache, set(9,  17, new())))) =:= undefined),
     ?assert(array:get(21, reset(21, set(127, set_cache, set(21, 17, new())))) =:= undefined),
     ?assert(array:get(0, reset(32, set(127, set_cache, set(22, 17, new())))) =:= undefined),
     ?assert(array:get(1, reset(1,  set(22, 17, new()))) =:= undefined),
     ?assert(array:get(0, reset(11, new())) =:= undefined),
     ?assert(array:get(0, reset(11, new(31))) =:= undefined),
     ?assert(array:get(0, reset(0,  set(127, set_cache, set(0,  17, new({default,42}))))) =:= 42),
     ?assert(array:get(0, reset(0,  set(127, set_cache, new({default,42})))) =:= 42)
    ].

to_list_test(_Config) ->
    N0 = ?LEAFSIZE,
    [?assert([] =:= to_list(new())),
     ?assert([undefined] =:= to_list(new(1))),
     ?assert([undefined,undefined] =:= to_list(new(2))),
     ?assert(lists:duplicate(N0,0) =:= to_list(new(N0,{default,0}))),
     ?assert(lists:duplicate(N0+1,1) =:= to_list(new(N0+1,{default,1}))),
     ?assert(lists:duplicate(N0+2,2) =:= to_list(new(N0+2,{default,2}))),
     ?assert(lists:duplicate(666,6) =:= to_list(new(666,{default,6}))),
     ?assert([1,2,3] =:= to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?assert([3,2,1] =:= to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?assert([1|lists:duplicate(N0-2,0)++[1]] =:=
              to_list(set(N0-1,1,set(0,1,new({default,0}))))),
     ?assert([1|lists:duplicate(N0-1,0)++[1]] =:=
              to_list(set(N0,1,set(0,1,new({default,0}))))),
     ?assert([1|lists:duplicate(N0,0)++[1]] =:=
              to_list(set(N0+1,1,set(0,1,new({default,0}))))),
     ?assert([1|lists:duplicate(N0*3,0)++[1]] =:=
              to_list(set((N0*3)+1,1,set(0,1,new({default,0}))))),
     ?assertError(badarg, to_list(no_array))
    ].

sparse_to_list_test(_Config) ->
    N0 = ?LEAFSIZE,
    [?assert([] =:= sparse_to_list(new())),
     ?assert([] =:= sparse_to_list(new(1))),
     ?assert([] =:= sparse_to_list(new(1,{default,0}))),
     ?assert([] =:= sparse_to_list(new(2))),
     ?assert([] =:= sparse_to_list(new(2,{default,0}))),
     ?assert([] =:= sparse_to_list(new(N0,{default,0}))),
     ?assert([] =:= sparse_to_list(new(N0+1,{default,1}))),
     ?assert([] =:= sparse_to_list(new(N0+2,{default,2}))),
     ?assert([] =:= sparse_to_list(new(666,{default,6}))),
     ?assert([1,2,3] =:= sparse_to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?assert([3,2,1] =:= sparse_to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?assert([0,1] =:= sparse_to_list(set(N0-1,1,set(0,0,new())))),
     ?assert([0,1] =:= sparse_to_list(set(N0,1,set(0,0,new())))),
     ?assert([0,1] =:= sparse_to_list(set(N0+1,1,set(0,0,new())))),
     ?assert([0,1,2] =:= sparse_to_list(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?assertError(badarg, sparse_to_list(no_array))
    ].

from_list_test(_Config) ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?assert(array:size(from_list([])) =:= 0),
     ?assert(array:is_fix(from_list([])) =:= false),
     ?assert(array:size(from_list([undefined])) =:= 1),
     ?assert(array:is_fix(from_list([undefined])) =:= false),
     ?assert(array:size(from_list(lists:seq(1,N1))) =:= N1),
     ?assert(to_list(from_list(lists:seq(1,N0))) =:= lists:seq(1,N0)),
     ?assert(to_list(from_list(lists:seq(1,N0+1))) =:= lists:seq(1,N0+1)),
     ?assert(to_list(from_list(lists:seq(1,N0+2))) =:= lists:seq(1,N0+2)),
     ?assert(to_list(from_list(lists:seq(1,N2))) =:= lists:seq(1,N2)),
     ?assert(to_list(from_list(lists:seq(1,N2+1))) =:= lists:seq(1,N2+1)),
     ?assert(to_list(from_list(lists:seq(0,N3))) =:= lists:seq(0,N3)),
     ?assert(to_list(from_list(lists:seq(0,N4))) =:= lists:seq(0,N4)),
     ?assertError(badarg, from_list([a,b,a,c|d])),
     ?assertError(badarg, from_list(no_array))
    ].

from_test(_Config) ->
    Seq = fun({N,Max}) ->
                  if N =< Max -> {N, {N+1, Max}};
                     true -> done
                  end
          end,
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?assert(array:size(from(Seq, {1,0})) =:= 0),
     ?assert(array:is_fix(from(Seq, {1,0})) =:= false),
     ?assert(array:size(from(Seq, {1,1})) =:= 1),
     ?assert(array:is_fix(from(Seq, {1,1})) =:= false),
     ?assert(to_list(from(Seq, {1,N0-1})) =:= lists:seq(1,N0-1)),
     ?assert(to_list(from(Seq, {1,N0})) =:= lists:seq(1,N0)),
     ?assert(to_list(from(Seq, {1,N0+1})) =:= lists:seq(1,N0+1)),
     ?assert(to_list(from(Seq, {1,N0+2})) =:= lists:seq(1,N0+2)),
     ?assert(to_list(from(Seq, {1,N2-1})) =:= lists:seq(1,N2-1)),
     ?assert(to_list(from(Seq, {1,N2})) =:= lists:seq(1,N2)),
     ?assert(to_list(from(Seq, {1,N2+1})) =:= lists:seq(1,N2+1)),
     ?assert(to_list(from(Seq, {0,N3})) =:= lists:seq(0,N3)),
     ?assert(to_list(from(Seq, {0,N4})) =:= lists:seq(0,N4)),
     ?assert(array:size(from(Seq, {1,N1})) =:= N1),
     ?assertError(badarg, from(fun(A) -> A end, foo)),
     ?assertError(badarg, from(no_fun, foo))
    ].



to_orddict_test(_Config) ->
    N0 = ?LEAFSIZE,
    [?assert([] =:= to_orddict(new())),
     ?assert([{0,undefined}] =:= to_orddict(new(1))),
     ?assert([{0,undefined},{1,undefined}] =:= to_orddict(new(2))),
     ?assert([{N,0}||N<-lists:seq(0,N0-1)]
              =:= to_orddict(new(N0,{default,0}))),
     ?assert([{N,1}||N<-lists:seq(0,N0)]
              =:= to_orddict(new(N0+1,{default,1}))),
     ?assert([{N,2}||N<-lists:seq(0,N0+1)]
              =:= to_orddict(new(N0+2,{default,2}))),
     ?assert([{N,6}||N<-lists:seq(0,665)]
              =:= to_orddict(new(666,{default,6}))),
     ?assert([{0,1},{1,2},{2,3}] =:=
              to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?assert([{0,3},{1,2},{2,1}] =:=
              to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-2)]++[{N0-1,1}]]
              =:= to_orddict(set(N0-1,1,set(0,1,new({default,0}))))),
     ?assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-1)]++[{N0,1}]]
              =:= to_orddict(set(N0,1,set(0,1,new({default,0}))))),
     ?assert([{0,1}|[{N,0}||N<-lists:seq(1,N0)]++[{N0+1,1}]]
              =:= to_orddict(set(N0+1,1,set(0,1,new({default,0}))))),
     ?assert([{0,0} | [{N,undefined}||N<-lists:seq(1,N0*2)]] ++
              [{N0*2+1,1} | [{N,undefined}||N<-lists:seq(N0*2+2,N0*10)]] ++
              [{N0*10+1,2}] =:=
              to_orddict(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?assertError(badarg, to_orddict(no_array))
    ].

sparse_to_orddict_test(_Config) ->
    N0 = ?LEAFSIZE,
    [?assert([] =:= sparse_to_orddict(new())),
     ?assert([] =:= sparse_to_orddict(new(1))),
     ?assert([] =:= sparse_to_orddict(new(1,{default,0}))),
     ?assert([] =:= sparse_to_orddict(new(2))),
     ?assert([] =:= sparse_to_orddict(new(2,{default,0}))),
     ?assert([] =:= sparse_to_orddict(new(N0,{default,0}))),
     ?assert([] =:= sparse_to_orddict(new(N0+1,{default,1}))),
     ?assert([] =:= sparse_to_orddict(new(N0+2,{default,2}))),
     ?assert([] =:= sparse_to_orddict(new(666,{default,6}))),
     ?assert([{0,1},{1,2},{2,3}] =:=
              sparse_to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?assert([{0,3},{1,2},{2,1}] =:=
              sparse_to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?assert([{0,1},{N0-1,1}] =:=
              sparse_to_orddict(set(N0-1,1,set(0,1,new({default,0}))))),
     ?assert([{0,1},{N0,1}] =:=
              sparse_to_orddict(set(N0,1,set(0,1,new({default,0}))))),
     ?assert([{0,1},{N0+1,1}] =:=
              sparse_to_orddict(set(N0+1,1,set(0,1,new({default,0}))))),
     ?assert([{0,0},{N0*2+1,1},{N0*10+1,2}] =:=
              sparse_to_orddict(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?assertError(badarg, sparse_to_orddict(no_array))
    ].

from_orddict_test(_Config) ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?assert(array:size(from_orddict([])) =:= 0),
     ?assert(array:is_fix(from_orddict([])) =:= false),
     ?assert(array:size(from_orddict([{0,undefined}])) =:= 1),
     ?assert(array:is_fix(from_orddict([{0,undefined}])) =:= false),
     ?assert(array:size(from_orddict([{N0-1,undefined}])) =:= N0),
     ?assert(array:size(from_orddict([{N,0}||N<-lists:seq(0,N1-1)]))
              =:= N1),
     ?assertError({badarg,_}, from_orddict([foo])),
     ?assertError({badarg,_}, from_orddict([{200,foo},{1,bar}])),
     ?assertError({badarg,_}, from_orddict([{N,0}||N<-lists:seq(0,N0-1)] ++ not_a_list)),
     ?assertError(badarg, from_orddict(no_array)),


     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N0-1)],
                   L =:= to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N0)],
                   L =:= to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N2-1)],
                   L =:= to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N2)],
                   L =:= to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N3-1)],
                   L =:= to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N,0}||N<-lists:seq(0,N4-1)],
                   L =:= to_orddict(from_orddict(L)))),

     %% Hole in the begining
     ?assert(?LET(L, [{0,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N0,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N3,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N4,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N0-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N1-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N3-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{N4-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),

     %% Hole in middle

     ?assert(?LET(L, [{0,0},{N0,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N3,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N4,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N0-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N1-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N3-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L)))),
     ?assert(?LET(L, [{0,0},{N4-1,0}],
                   L =:= sparse_to_orddict(from_orddict(L))))

    ].

map_test(_Config) ->
    N0 = ?LEAFSIZE,
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    Default = fun(_K,undefined) ->  no_value;
                 (K,V) -> K+V
              end,
    [?assertError(badarg, map([], new())),
     ?assertError(badarg, map([], new(10))),
     ?assert(to_list(map(Id, new())) =:= []),
     ?assert(to_list(map(Id, new(1))) =:= [undefined]),
     ?assert(to_list(map(Id, new(5,{default,0}))) =:= [0,0,0,0,0]),
     ?assert(to_list(map(Id, from_list([1,2,3,4]))) =:= [1,2,3,4]),
     ?assert(to_list(map(Plus(1), from_list([0,1,2,3]))) =:= [1,2,3,4]),
     ?assert(to_list(map(Plus(-1), from_list(lists:seq(1,11))))
              =:= lists:seq(0,10)),
     ?assert(to_list(map(Plus(11), from_list(lists:seq(0,99999))))
              =:= lists:seq(11,100010)),
     ?assert([{0,0},{N0*2+1,N0*2+1+1},{N0*100+1,N0*100+1+2}] =:=
              sparse_to_orddict((map(Default,
                                     set(N0*100+1,2,
                                         set(N0*2+1,1,
                                             set(0,0,new())))))#array{default = no_value}))
    ].

sparse_map_test(_Config) ->
    N0 = ?LEAFSIZE,
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    KeyPlus = fun (K,X) -> K+X end,
    [?assertError(badarg, sparse_map([], new())),
     ?assertError(badarg, sparse_map([], new(10))),
     ?assert(to_list(sparse_map(Id, new())) =:= []),
     ?assert(to_list(sparse_map(Id, new(1))) =:= [undefined]),
     ?assert(to_list(sparse_map(Id, new(5,{default,0}))) =:= [0,0,0,0,0]),
     ?assert(to_list(sparse_map(Id, from_list([1,2,3,4]))) =:= [1,2,3,4]),
     ?assert(to_list(sparse_map(Plus(1), from_list([0,1,2,3])))
              =:= [1,2,3,4]),
     ?assert(to_list(sparse_map(Plus(-1), from_list(lists:seq(1,11))))
              =:= lists:seq(0,10)),
     ?assert(to_list(sparse_map(Plus(11), from_list(lists:seq(0,99999))))
              =:= lists:seq(11,100010)),
     ?assert(to_list(sparse_map(Plus(1), set(1,1,new({default,0}))))
              =:= [0,2]),
     ?assert(to_list(sparse_map(Plus(1),
                                 set(3,4,set(0,1,new({default,0})))))
              =:= [2,0,0,5]),
     ?assert(to_list(sparse_map(Plus(1),
                                 set(9,9,set(1,1,new({default,0})))))
              =:= [0,2,0,0,0,0,0,0,0,10]),
     ?assert([{0,0},{N0*2+1,N0*2+1+1},{N0*100+1,N0*100+1+2}] =:=
              sparse_to_orddict(sparse_map(KeyPlus,
                                           set(N0*100+1,2,
                                               set(N0*2+1,1,
                                                   set(0,0,new()))))))

    ].

foldl_test(_Config) ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
              (K,X,{C,L}) -> {C,[K+X|L]}
           end,
    [?assertError(badarg, foldl([], 0, new())),
     ?assertError(badarg, foldl([], 0, new(10))),
     ?assert(foldl(Count, 0, new()) =:= 0),
     ?assert(foldl(Count, 0, new(1)) =:= 1),
     ?assert(foldl(Count, 0, new(10)) =:= 10),
     ?assert(foldl(Count, 0, from_list([1,2,3,4])) =:= 4),
     ?assert(foldl(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) =:= 20),
     ?assert(foldl(Count, 1000, from_list(lists:seq(0,999))) =:= 2000),
     ?assert(foldl(Sum, 0, from_list(lists:seq(0,10))) =:= 55),
     ?assert(foldl(Reverse, [], from_list(lists:seq(0,1000)))
              =:= lists:reverse(lists:seq(0,1000))),
     ?assert({N0*100-1,[N0*100+1+2,N0*2+1+1,0]} =:=
              foldl(Vals, {0,[]},
                    set(N0*100+1,2,
                        set(N0*2+1,1,
                            set(0,0,new())))))
    ].

sparse_foldl_test(_Config) ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
              (K,X,{C,L}) -> {C,[K+X|L]}
           end,
    [?assertError(badarg, sparse_foldl([], 0, new())),
     ?assertError(badarg, sparse_foldl([], 0, new(10))),
     ?assert(sparse_foldl(Count, 0, new()) =:= 0),
     ?assert(sparse_foldl(Count, 0, new(1)) =:= 0),
     ?assert(sparse_foldl(Count, 0, new(10,{default,1})) =:= 0),
     ?assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4],0)) =:= 4),
     ?assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
              =:= 9),
     ?assert(sparse_foldl(Count, 0, from_list(lists:seq(0,999),0))
              =:= 999),
     ?assert(sparse_foldl(Sum, 0, from_list(lists:seq(0,10), 5)) =:= 50),
     ?assert(sparse_foldl(Reverse, [], from_list(lists:seq(0,1000), 0))
              =:= lists:reverse(lists:seq(1,1000))),
     ?assert({0,[N0*100+1+2,N0*2+1+1,0]} =:=
              sparse_foldl(Vals, {0,[]},
                           set(N0*100+1,2,
                               set(N0*2+1,1,
                                   set(0,0,new())))))
    ].

foldr_test(_Config) ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
              (K,X,{C,L}) -> {C,[K+X|L]}
           end,
    [?assertError(badarg, foldr([], 0, new())),
     ?assertError(badarg, foldr([], 0, new(10))),
     ?assert(foldr(Count, 0, new()) =:= 0),
     ?assert(foldr(Count, 0, new(1)) =:= 1),
     ?assert(foldr(Count, 0, new(10)) =:= 10),
     ?assert(foldr(Count, 0, from_list([1,2,3,4])) =:= 4),
     ?assert(foldr(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) =:= 20),
     ?assert(foldr(Count, 1000, from_list(lists:seq(0,999))) =:= 2000),
     ?assert(foldr(Sum, 0, from_list(lists:seq(0,10))) =:= 55),
     ?assert(foldr(List, [], from_list(lists:seq(0,1000)))
              =:= lists:seq(0,1000)),
     ?assert({N0*100-1,[0,N0*2+1+1,N0*100+1+2]} =:=
                  foldr(Vals, {0,[]},
                        set(N0*100+1,2,
                            set(N0*2+1,1,
                                set(0,0,new())))))
    ].

sparse_foldr_test(_Config) ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
              (K,X,{C,L}) -> {C,[K+X|L]}
           end,
    [?assertError(badarg, sparse_foldr([], 0, new())),
     ?assertError(badarg, sparse_foldr([], 0, new(10))),
     ?assert(sparse_foldr(Count, 0, new()) =:= 0),
     ?assert(sparse_foldr(Count, 0, new(1)) =:= 0),
     ?assert(sparse_foldr(Count, 0, new(10,{default,1})) =:= 0),
     ?assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4],0)) =:= 4),
     ?assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
              =:= 9),
     ?assert(sparse_foldr(Count, 0, from_list(lists:seq(0,999),0))
              =:= 999),
     ?assert(sparse_foldr(Sum, 0, from_list(lists:seq(0,10),5)) =:= 50),
     ?assert(sparse_foldr(List, [], from_list(lists:seq(0,1000),0))
              =:= lists:seq(1,1000)),

     ?assert(sparse_size(new()) =:= 0),
     ?assert(sparse_size(new(8)) =:= 0),
     ?assert(sparse_size(array:set(7, 0, new())) =:= 8),
     ?assert(sparse_size(array:set(7, 0, new(10))) =:= 8),
     ?assert(sparse_size(array:set(99, 0, new(10,{fixed,false})))
              =:= 100),
     ?assert(sparse_size(array:set(7, undefined, new())) =:= 0),
     ?assert(sparse_size(array:from_list([1,2,3,undefined])) =:= 3),
     ?assert(sparse_size(array:from_orddict([{3,0},{17,0},{99,undefined}]))
                          =:= 18),
     ?assert({0,[0,N0*2+1+1,N0*100+1+2]} =:=
              sparse_foldr(Vals, {0,[]},
                           set(N0*100+1,2,
                               set(N0*2+1,1,
                                   set(0,0,new())))))
    ].

import_export(_Config) ->
    %% Some examples of usages
    FloatBin = << <<N:32/float-native>> || N <- lists:seq(1, 20000)>>,
    ToFloat32 = fun(_K, V, Acc) when is_binary(Acc) ->
                        <<Acc/binary, V:32/float-native>>
                end,
    FromFloat32 = fun(<<N:32/float-native, Rest/binary>>) ->
                          {N, Rest};
                     (<<>>) ->
                          done
                  end,

    ?assert(FloatBin =:= array:foldl(ToFloat32, <<>>, array:from(FromFloat32, FloatBin))),
    RGBBin = << <<N:8, N:8, N:8>> || N <- lists:seq(1, 256)>>,
    RGB2Bin = fun(_K, {R,G,B}, Acc) ->
                      <<Acc/binary, R:8, G:8, B:8>>
              end,
    Bin2RGB = fun(<<R:8,G:8,B:8, Rest/binary>>) ->
                      {{R,G,B}, Rest};
                 (<<>>) ->
                      done
              end,
    ?assert(RGBBin =:= array:foldl(RGB2Bin, <<>>, array:from(Bin2RGB, RGBBin))),

    ok.

doctests(Config) when is_list(Config) ->
    shell_docs:test(array, []).
