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
         shift_test/1,
         slice_test/1,
         prepend_test/1,
         append_test/1,
         concat_test/1,
         to_orddict_test/1,
         sparse_to_orddict_test/1,
         from_orddict_test/1,
         map_test/1,
         sparse_map_test/1,
         foldl_test/1,
         sparse_foldl_test/1,
         foldr_test/1,
         sparse_foldr_test/1,
         mapfoldl_test/1,
         import_export/1,
         old_format/1,
         doctests/1,
         %% Property tests
         prop_new/1, prop_is_array/1, prop_set_get/1, prop_size/1,
         prop_sparse_size/1, prop_default/1, prop_fix_relax/1,
         prop_resize/1, prop_reset/1, prop_to_list/1, prop_from_list/1,
         prop_to_orddict/1, prop_from_orddict/1, prop_map/1,
         prop_foldl/1, prop_foldr/1, prop_shift/1, prop_slice/1,
         prop_append_prepend/1, prop_concat/1, prop_mapfoldl/1, prop_mapfoldr/1,
         prop_sparse_mapfoldl/1, prop_sparse_mapfoldr/1
        ]).


-export([t/0,t/1]).

-import(array,
        [new/0, new/1, new/2, is_array/1, set/3, get/2, sparse_size/1,
         default/1, reset/2, to_list/1, sparse_to_list/1, shift/2, slice/3,
         prepend/2, append/2, concat/2, concat/1, from/2, from/3,
         from_list/1, from_list/2, to_orddict/1, sparse_to_orddict/1,
         from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
         foldr/3, sparse_foldl/3, sparse_foldr/3, fix/1, relax/1, is_fix/1,
         resize/1, resize/2, mapfoldl/3, mapfoldr/3, sparse_mapfoldl/3,
         sparse_mapfoldr/3]).

%%
%% all/1
%%
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [new_test, fix_test, relax_test, resize_test,
     set_get_test, to_list_test, sparse_to_list_test,
     from_list_test, from_test,
     shift_test, slice_test, prepend_test, append_test,
     concat_test, to_orddict_test, sparse_to_orddict_test,
     from_orddict_test, map_test, sparse_map_test,
     foldl_test, sparse_foldl_test, foldr_test, sparse_foldr_test,
     mapfoldl_test,
     import_export, old_format, doctests,
     {group, property}].

groups() ->
    [{property, [],
      [prop_new, prop_is_array, prop_set_get, prop_size,
       prop_sparse_size, prop_default, prop_fix_relax,
       prop_resize, prop_reset, prop_to_list, prop_from_list,
       prop_to_orddict, prop_from_orddict, prop_map,
       prop_foldl, prop_foldr, prop_shift, prop_slice,
       prop_append_prepend, prop_concat, prop_mapfoldl, prop_mapfoldr,
       prop_sparse_mapfoldl, prop_sparse_mapfoldr]}].

init_per_suite(Config0) ->
    case ct_property_test:init_per_suite(Config0) of
        Config when is_list(Config) ->
            Config;
        {skip, _} -> Config0;
        Fail -> Fail
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(property, Config) ->
    case proplists:get_value(property_test_tool, Config, none) of
        none ->
            {skip, "No known property based tool found"};
        _ ->
            Config
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

-define(LEAFSIZE,16).
-define(NODESIZE,?LEAFSIZE).

-record(array,  {size,          %% number of defined entries
                 zero,          %% offset of zero point
                 fix,           %% not automatically growing
                 default,       %% the default value (usually 'undefined')
                 cache,         %% cached leaf tuple
                 cache_index,   %% low index of cache
                 elements,      %% the tuple tree
                 bits
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
    ?assert(resize(0, new()) =:= new()),
    ?assert(array:to_list(resize(1, new())) == [undefined]), %% Bug found by prop tests
    ?assert(resize(99, new(99)) =:= new(99)),
    ?assert(resize(99, relax(new(99))) =:= relax(new(99))),
    ?assert(is_fix(resize(100, new(10)))),
    ?assertNot(is_fix(resize(100, relax(new(10))))),

    ?assert(array:size(resize(100, new())) =:= 100),
    ?assert(array:size(resize(0, new(100))) =:= 0),
    ?assert(array:size(resize(1, new(100))) =:= 1),
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
    ?assertError(badarg, resize(foo, bad_argument)),

    %% Regression test for shrink bugs
    A258 = array:from_list(lists:seq(1,258)),
    ?assert(244 =:= array:get(0, array:resize(1, array:set(1, 1744, array:set(0, 244, array:new()))))),
    ?assert(258 =:= array:get(257, array:resize(261, array:set(261, foo, A258)))),
    ?assert(258 =:= array:get(257, array:resize(315, array:set(0, x, array:set(600, foo, A258))))),
    %% Test multi-level collapse wrapping
    ?test(begin
              A0 = array:new(),
              A1 = lists:foldl(fun(I, Acc) -> array:set(I, I, Acc) end, A0, lists:seq(0, 300)),
              A2 = array:resize(50, A1),
              ?assert(49 =:= array:get(49, A2)),
              A3 = array:resize(300, A2),
              ?assert(49 =:= array:get(49, A3)),
              ?assert(undefined =:= array:get(299, A3))
          end),

    ok.

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

shift_test(_Config) ->
    [
     ?assertEqual(5, array:size(shift(0, fix(array:new(5))))),
     ?assertEqual(4, array:size(shift(1, fix(array:new(5))))),
     ?assertEqual(0, array:size(shift(5, fix(array:new(5))))),

     ?assertEqual(1, array:get(0, shift(0, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(2, array:get(0, shift(1, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(4, array:get(0, shift(3, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(0, array:size(shift(5, from_list(lists:seq(1,5))))),
     ?assertError(badarg, shift(6, from_list(lists:seq(1,5)))),
     ?assertError(badarg, shift(0, no_array)),

     ?assertEqual([1,2,3,4,5], to_list(shift(0, from_list(lists:seq(1,5))))),
     ?assertEqual([2,3,4,5], to_list(shift(1, from_list(lists:seq(1,5))))),
     ?assertEqual([4,5], to_list(shift(3, from_list(lists:seq(1,5))))),
     ?assertEqual([], to_list(shift(5, from_list(lists:seq(1,5))))),

     ?assertEqual(lists:seq(?LEAFSIZE+2,?LEAFSIZE*?NODESIZE),
                  to_list(shift(?LEAFSIZE+1, from_list(lists:seq(1,?LEAFSIZE*?NODESIZE))))),
     ?assertEqual(lists:seq(3,3*?NODESIZE+1),
                  to_list(shift(-3, shift(5, from_list(lists:seq(1,3*?NODESIZE+1)))))),
     ?assertEqual(lists:seq(13,3*?NODESIZE+1),
                  to_list(shift(-5, shift(?LEAFSIZE+1, from_list(lists:seq(1,3*?NODESIZE+1)))))),

     ?assertEqual(6, array:size(shift(-1, fix(array:new(5))))),
     ?assertEqual(5+?LEAFSIZE, array:size(shift(-?LEAFSIZE, fix(array:new(5))))),
     ?assertEqual(3*?LEAFSIZE+7, array:size(shift(-7, fix(array:new(3*?LEAFSIZE))))),
     ?assertEqual([x] ++ lists:duplicate(2+?LEAFSIZE+1,undefined), array:to_list(array:set(0,x,shift(-3, fix(array:new(?LEAFSIZE+1)))))),

     ?assertEqual(undefined, array:get(0, shift(-1, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(1, array:get(1, shift(-1, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(3, array:get(4, shift(-2, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(1, array:get(?LEAFSIZE, shift(-?LEAFSIZE, fix(from_list(lists:seq(1,5)))))),

     ?assertEqual([undefined,1,2,3,4,5], to_list(shift(-1, from_list(lists:seq(1,5))))),
     ?assertEqual([undefined,undefined,undefined,1,2,3,4,5], to_list(shift(-3, from_list(lists:seq(1,5))))),
     ?assertEqual(lists:duplicate(?LEAFSIZE+1,undefined) ++ lists:seq(1,5), to_list(shift(-(?LEAFSIZE+1), from_list(lists:seq(1,5)))))
    ].

slice_test(_Config) ->
    [
     ?assertEqual(lists:seq(1,?LEAFSIZE), to_list(slice(0, ?LEAFSIZE, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual(lists:seq(2,?LEAFSIZE), to_list(slice(1, ?LEAFSIZE-1, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual(lists:seq(3,?LEAFSIZE), to_list(slice(2, ?LEAFSIZE-2, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual(lists:seq(1,?LEAFSIZE-1), to_list(slice(0, ?LEAFSIZE-1, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual(lists:seq(1,?LEAFSIZE-2), to_list(slice(0, ?LEAFSIZE-2, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual(lists:seq(4,?LEAFSIZE-3), to_list(slice(3, ?LEAFSIZE-6, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual([4], to_list(slice(3, 1, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual([], to_list(slice(3, 0, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertEqual([], to_list(slice(0, -1, fix(from_list(lists:seq(1,?LEAFSIZE)))))),
     ?assertError(badarg, to_list(slice(-1, 3, fix(from_list(lists:seq(1,?LEAFSIZE))))))
    ].

prepend_test(_Config) ->
    [
     ?assertEqual(6, array:size(prepend(0, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(0, array:get(0, prepend(0, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(1, array:get(1, prepend(0, from_list(lists:seq(1,5))))),
     ?assertEqual([0,1,2,3,4,5], to_list(prepend(0, from_list(lists:seq(1,5))))),
     ?assertEqual(lists:seq(0,?LEAFSIZE+1), to_list(prepend(0, from_list(lists:seq(1,?LEAFSIZE+1)))))
    ].

append_test(_Config) ->
    [
     ?assertEqual(6, array:size(append(6, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(6, array:get(5, append(6, fix(from_list(lists:seq(1,5)))))),
     ?assertEqual(5, array:get(4, append(6, from_list(lists:seq(1,5))))),
     ?assertEqual([1,2,3,4,5,6], to_list(append(6, from_list(lists:seq(1,5))))),
     ?assertEqual(lists:seq(1,?LEAFSIZE+1), to_list(append(?LEAFSIZE+1, from_list(lists:seq(1,?LEAFSIZE))))),
     ?assertError(badarg, append(5, foo))
    ].

concat_test(_Config) ->
    ?assertEqual([1,2], to_list(concat(from_list([1]), from_list([2])))),
    ?assertEqual([1,2,3,4,5,6], to_list(concat(from_list([1,2,3]), from_list([4,5,6])))),
    ?assertEqual([2,3,4,5,6], to_list(concat(from_list([2,3]), from_list([4,5,6])))),
    ?assertEqual([1,2,3], to_list(concat(from_list([1,2,3]), from_list([])))),
    ?assertEqual([1,2,3], to_list(concat(from_list([]), from_list([1,2,3])))),
    ?assertEqual(a, default(concat(new({default, a}), from_list([1,2,3], b)))),
    ?assertEqual(false, is_fix(concat(new(), fix(from_list([1,2,3], b))))),
    ?assertEqual([], to_list(concat(from_list([]), from_list([])))),
    ?assertEqual([], to_list(concat(new(), new()))),
    ?assertError(badarg, concat(from_list([1,2,3]),no_array)),
    ?assertError(badarg, concat(no_array,from_list([1,2,3]))),
    ?assertNot(is_fix(concat(from_list([1,2,3]), from_list([4,5,6])))),

    ?assertEqual([2,3,4,5,6], to_list(concat([from_list([2,3]), from_list([4,5,6])]))),
    ?assertEqual([1,2,3,4,5,6], to_list(concat([from_list([1]), from_list([2,3]), new(), from_list([4,5,6]), new()]))),
    ?assertError(badarg, concat(no_list)),
    ?assertError(badarg, concat([])).

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
     ?assertEqual([{0,0},{N0*2+1,N0*2+1+1},{N0*100+1,N0*100+1+2}],
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
     ?assertError(badarg, foldl([], 0, not_an_array)),
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
     ?assertError(badarg, sparse_foldl([], 0, not_an_array)),
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

mapfoldl_test(_Config) ->
    N0 = ?LEAFSIZE,
    Inc = fun(_, X, Acc) -> {X+1, Acc+X} end,
    Double = fun(K, X, Acc) -> {X*2, Acc+K} end,
    
    %% mapfoldl
    ?assertError(badarg, mapfoldl([], 0, new())),
    ?assertError(badarg, mapfoldl([], 0, not_an_array)),

    ?assert(begin {A1, 0} = mapfoldl(Inc, 0, new()),
                  to_list(A1) =:= [] end),
    ?assert(begin {A3, 10} = mapfoldl(Inc, 0, from_list([1,2,3,4])),
                  to_list(A3) =:= [2,3,4,5] end),
    ?assert(begin {A4, 55} = mapfoldl(Inc, 0, from_list(lists:seq(0,10))),
                  to_list(A4) =:= lists:seq(1,11) end),
    ?assert(begin {A5, 45} = mapfoldl(Double, 0, from_list(lists:seq(1,10))),
                  to_list(A5) =:= [2,4,6,8,10,12,14,16,18,20] end),
    
    %% mapfoldr
    ?assertError(badarg, mapfoldr([], 0, new())),
    ?assertError(badarg, mapfoldr([], 0, not_an_aray)),
    ?assert(begin {A6, 0} = mapfoldr(Inc, 0, new()),
                  to_list(A6) =:= [] end),
    ?assert(begin {A7, 10} = mapfoldr(Inc, 0, from_list([1,2,3,4])),
                  to_list(A7) =:= [2,3,4,5] end),
    ?assert(begin {A8, 55} = mapfoldr(Inc, 0, from_list(lists:seq(0,10))),
                  to_list(A8) =:= lists:seq(1,11) end),

    %% sparse_mapfoldl
    ?assertError(badarg, sparse_mapfoldl([], 0, new())),
    ?assertError(badarg, sparse_mapfoldl([], 0, not_an_aray)),
    ?assert(begin {A9, 0} = sparse_mapfoldl(Inc, 0, new()), to_list(A9) =:= [] end),
    ?assert(begin {A11, 10} = sparse_mapfoldl(Inc, 0, from_list([0,1,2,3,4],0)),
                  to_list(A11) =:= [0,2,3,4,5] end),
    ?assert(begin {A12, 45} = sparse_mapfoldl(Inc, 0, from_list(lists:seq(0,9),0)),
                  to_list(A12) =:= [0,2,3,4,5,6,7,8,9,10] end),
    
    %% sparse_mapfoldr
    ?assertError(badarg, sparse_mapfoldr([], 0, new())),
    ?assertError(badarg, sparse_mapfoldr([], 0, not_an_aray)),
    ?assert(begin {A13, 0} = sparse_mapfoldr(Inc, 0, new()), to_list(A13) =:= [] end),
    ?assert(begin {A14, 10} = sparse_mapfoldr(Inc, 0, from_list([0,1,2,3,4],0)),
                  to_list(A14) =:= [0,2,3,4,5] end),
    ?assert(begin {A15, 45} = sparse_mapfoldr(Inc, 0, from_list(lists:seq(0,9),0)),
                  to_list(A15) =:= [0,2,3,4,5,6,7,8,9,10] end),
    
    %% Test with sparse array
    ?assert(begin {A16, 2} = sparse_mapfoldl(Inc, 0, set(N0*2+1,1,set(0,1,new()))),
            sparse_to_orddict(A16) =:= [{0,2},{N0*2+1,2}] end),
    
    ok.

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

old_format(_Config) ->
    %% array:fix(array:from_orddict([{3,a}, {42,b}, {43,c}, {44,d}, {45,e}, {500,x}], foo)).
    List = [{3,a}, {42,b}, {43,c}, {44,d}, {45,e}, {500,x}],
    Old = {array,501,0,foo,
           {{{foo,foo,foo,a,foo,foo,foo,foo,foo,foo},
             10,10,10,
             {foo,foo,b,c,d,e,foo,foo,foo,foo},
             10,10,10,10,10,10},
            100,100,100,100,
            {{x,foo,foo,foo,foo,foo,foo,foo,foo,foo},
             10,10,10,10,10,10,10,10,10,10},
            100,100,100,100,100}},
    A = array:upgrade(Old),
    ?assert(List =:= array:sparse_to_orddict(A)),
    ?assert(true =:= array:is_fix(A)),
    ?assert(foo =:= array:default(A)),
    ?assert(x =:= array:get(500, A)),
    ?assert(foo =:= array:get(499, A)),
    ?assert(A =:= array:upgrade(A)),
    ok.


doctests(Config) when is_list(Config) ->
    shell_docs:test(array, []).


%%
%% Property-based tests
%%

prop_new(Config) ->
    do_proptest(prop_new, Config).

prop_is_array(Config) ->
    do_proptest(prop_is_array, Config).

prop_set_get(Config) ->
    do_proptest(prop_set_get, Config).

prop_size(Config) ->
    do_proptest(prop_size, Config).

prop_sparse_size(Config) ->
    do_proptest(prop_sparse_size, Config).

prop_default(Config) ->
    do_proptest(prop_default, Config).

prop_fix_relax(Config) ->
    do_proptest(prop_fix_relax, Config).

prop_resize(Config) ->
    do_proptest(prop_resize, Config).

prop_reset(Config) ->
    do_proptest(prop_reset, Config).

prop_to_list(Config) ->
    do_proptest(prop_to_list, Config).

prop_from_list(Config) ->
    do_proptest(prop_from_list, Config).

prop_to_orddict(Config) ->
    do_proptest(prop_to_orddict, Config).

prop_from_orddict(Config) ->
    do_proptest(prop_from_orddict, Config).

prop_map(Config) ->
    do_proptest(prop_map, Config).

prop_foldl(Config) ->
    do_proptest(prop_foldl, Config).

prop_foldr(Config) ->
    do_proptest(prop_foldr, Config).

prop_shift(Config) ->
    do_proptest(prop_shift, Config).

prop_slice(Config) ->
    do_proptest(prop_slice, Config).

prop_append_prepend(Config) ->
    do_proptest(prop_append_prepend, Config).

prop_concat(Config) ->
    do_proptest(prop_concat, Config).

prop_mapfoldl(Config) ->
    do_proptest(prop_mapfoldl, Config).

prop_mapfoldr(Config) ->
    do_proptest(prop_mapfoldr, Config).

prop_sparse_mapfoldl(Config) ->
    do_proptest(prop_sparse_mapfoldl, Config).

prop_sparse_mapfoldr(Config) ->
    do_proptest(prop_sparse_mapfoldr, Config).

do_proptest(Prop, Config) ->
    ct_property_test:quickcheck(
        array_prop:Prop(),
        Config).
