%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

%% This module tests the orddict, dict, and gb_trees modules.
%%

-module(dict_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
         create/1,store/1,iterate/1]).

-include_lib("test_server/include/test_server.hrl").

-import(lists, [foldl/3]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [create, store, iterate].

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
    Dog = ?t:timetrap(?t:minutes(5)),
    [{watchdog,Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

create(Config) when is_list(Config) ->
    test_all(fun create_1/1).

create_1(M) ->
    D0 = M(empty, []),
    [] = M(to_list, D0),
    0 = M(size, D0),
    true = M(is_empty, D0),
    D0.

store(Config) when is_list(Config) ->
    test_all([{0,132},{253,258},{510,514}], fun store_1/2).

store_1(List, M) ->
    D0 = M(from_list, List),

    %% Make sure that we get the same result by inserting
    %% elements one at the time.
    D1 = foldl(fun({K,V}, Dict) -> M(enter, {K,V,Dict}) end,
	       M(empty, []), List),
    true = M(equal, {D0,D1}),
    case List of
	[] ->
	    true = M(is_empty, D0),
	    true = M(is_empty, D1);
	[_|_] ->
	    false = M(is_empty, D0),
	    false = M(is_empty, D1)
    end,
    D0.

%%%
%%% Test specifics for gb_trees.
%%%

iterate(Config) when is_list(Config) ->
    test_all(fun iterate_1/1).

iterate_1(M) ->
    case M(module, []) of
	gb_trees -> iterate_2(M);
	_ -> ok
    end,
    M(empty, []).

iterate_2(M) ->
    random:seed(1, 2, 42),
    iter_tree(M, 1000).

iter_tree(_M, 0) ->
    ok;
iter_tree(M, N) ->
    L = [{I, I} || I <- lists:seq(1, N)],
    T = M(from_list, L),
    L = lists:reverse(iterate_tree(M, T)),
    R = random:uniform(N),
    KV = lists:reverse(iterate_tree_from(M, R, T)),
    KV = [P || P={K,_} <- L, K >= R],
    iter_tree(M, N-1).

iterate_tree(M, Tree) ->
    I = M(iterator, Tree),
    iterate_tree_1(M, M(next, I), []).

iterate_tree_from(M, Start, Tree) ->
    I = M(iterator_from, {Start, Tree}),
    iterate_tree_1(M, M(next, I), []).

iterate_tree_1(_, none, R) ->
    R;
iterate_tree_1(M, {K, V, I}, R) ->
    iterate_tree_1(M, M(next, I), [{K, V} | R]).

%%%
%%% Helper functions.
%%%

dict_mods() ->
    Orddict = dict_test_lib:new(orddict, fun(X, Y) -> X == Y end),
    Dict = dict_test_lib:new(dict, fun(X, Y) ->
					   lists:sort(dict:to_list(X)) == 
					       lists:sort(dict:to_list(Y)) end),
    Gb = dict_test_lib:new(gb_trees, fun(X, Y) ->
					     gb_trees:to_list(X) == 
						 gb_trees:to_list(Y) end),
    [Orddict,Dict,Gb].

test_all(Tester) ->
    Pids = [spawn_tester(M, Tester) || M <- dict_mods()],
    collect_all(Pids, []).

spawn_tester(M, Tester) ->
    Parent = self(),
    spawn_link(fun() ->
		       random:seed(1, 2, 42),
		       S = Tester(M),
		       Res = {M(size, S),lists:sort(M(to_list, S))},
		       Parent ! {result,self(),Res}
	       end).

collect_all([Pid|Pids], Acc) ->
    receive
	{result,Pid,Result} ->
	    collect_all(Pids, [Result|Acc])
    end;
collect_all([], Acc) ->
    all_same(Acc).

test_all(ListTemplate, Tester) ->
    List = random_list(ListTemplate),
    test_all(fun(M) -> Tester(List, M) end).

all_same([H|T]) ->
    all_same_1(T, H).

all_same_1([H|T], H) ->
    all_same_1(T, H);
all_same_1([], _) -> ok.

rnd_list(Sz) ->
    rnd_list_1(Sz, []).

random_list([{Low,High}|T]) ->
    random_list(lists:seq(Low, High)++T);
random_list([Sz|T]) when is_integer(Sz) ->
    rnd_list(Sz)++random_list(T);
random_list([]) -> [].

rnd_list_1(0, Acc) ->
    Acc;
rnd_list_1(N, Acc) ->
    Key = atomic_rnd_term(),
    Value = random:uniform(100),
    rnd_list_1(N-1, [{Key,Value}|Acc]).

atomic_rnd_term() ->
    case random:uniform(3) of
	 1 -> list_to_atom(integer_to_list($\s+random:uniform(94))++"rnd");
	 2 -> random:uniform();
	 3 -> random:uniform(50)-37
    end.
