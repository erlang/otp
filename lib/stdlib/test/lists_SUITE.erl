%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
%%% Purpose: Test suite for the 'lists' module.
%%%-----------------------------------------------------------------

-module(lists_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([member/1, reverse/1,
	 keymember/1, keysearch_keyfind/1,
         keystore/1, keytake/1, keyreplace/1,
	 append_1/1, append_2/1,
	 seq_loop/1, seq_2/1, seq_3/1, seq_2_e/1, seq_3_e/1,

	 sublist_2/1, sublist_3/1, sublist_2_e/1, sublist_3_e/1,
	 flatten_1/1, flatten_2/1, flatten_1_e/1, flatten_2_e/1,
	 dropwhile/1, takewhile/1,
	 sort_1/1, sort_stable/1, merge/1, rmerge/1, sort_rand/1,
	 usort_1/1, usort_stable/1, umerge/1, rumerge/1,usort_rand/1,
	 keymerge/1, rkeymerge/1,
	 keysort_1/1, keysort_i/1, keysort_stable/1,
	 keysort_rand/1, keysort_error/1,
	 ukeymerge/1, rukeymerge/1,
	 ukeysort_1/1, ukeysort_i/1, ukeysort_stable/1,
	 ukeysort_rand/1, ukeysort_error/1,
	 funmerge/1, rfunmerge/1,
	 funsort_1/1, funsort_stable/1, funsort_rand/1,
	 funsort_error/1,
	 ufunmerge/1, rufunmerge/1,
	 ufunsort_1/1, ufunsort_stable/1, ufunsort_rand/1,
	 ufunsort_error/1,
	 zip_unzip/1, zip_unzip3/1, zipwith/1, zipwith3/1,
	 filter_partition/1, 
	 join/1,
	 otp_5939/1, otp_6023/1, otp_6606/1, otp_7230/1,
	 suffix/1, subtract/1, droplast/1, search/1, hof/1]).

%% Sort randomized lists until stopped.
%%
%% If you update some of the sort or merge functions, you should
%% definitely let sort_loop work for a couple of hours or days. Try
%% both sort_loop/0 and sort_loop/1 with a small argument (30-50 say).

-export([sort_loop/0, sort_loop/1, sloop/1]).

%% Internal export.
-export([make_fun/1]).

%%
%% all/1
%%
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() -> 
    [{group, append},
     {group, key},
     {group,sort},
     {group, usort},
     {group, keysort},
     {group, ukeysort},
     {group, funsort},
     {group, ufunsort},
     {group, sublist},
     {group, flatten},
     {group, seq},
     {group, tickets},
     {group, zip},
     {group, misc}].

groups() -> 
    [{append, [parallel], [append_1, append_2]},
     {usort, [parallel],
      [umerge, rumerge, usort_1, usort_rand, usort_stable]},
     {keysort, [parallel],
      [keymerge, rkeymerge, keysort_1, keysort_rand,
       keysort_i, keysort_stable, keysort_error]},
     {key, [parallel], [keymember, keysearch_keyfind, keystore,
			keytake, keyreplace]},
     {sort,[parallel],[merge, rmerge, sort_1, sort_rand]},
     {ukeysort, [parallel],
      [ukeymerge, rukeymerge, ukeysort_1, ukeysort_rand,
       ukeysort_i, ukeysort_stable, ukeysort_error]},
     {funsort, [parallel],
      [funmerge, rfunmerge, funsort_1, funsort_stable,
       funsort_error, funsort_rand]},
     {ufunsort, [parallel],
      [ufunmerge, rufunmerge, ufunsort_1, ufunsort_stable,
       ufunsort_error, ufunsort_rand]},
     {seq, [parallel], [seq_loop, seq_2, seq_3, seq_2_e, seq_3_e]},
     {sublist, [parallel],
      [sublist_2, sublist_3, sublist_2_e, sublist_3_e]},
     {flatten, [parallel],
      [flatten_1, flatten_2, flatten_1_e, flatten_2_e]},
     {tickets, [parallel], [otp_5939, otp_6023, otp_6606, otp_7230]},
     {zip, [parallel], [zip_unzip, zip_unzip3, zipwith, zipwith3]},
     {misc, [parallel], [reverse, member, dropwhile, takewhile,
			 filter_partition, suffix, subtract, join,
			 hof, droplast, search]}
    ].

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

%%
%% Test cases starts here.
%%

append_1(Config) when is_list(Config) ->
    "abcdef"=lists:append(["abc","def"]),
    [hej, du,[glade, [bagare]]]=
	lists:append([[hej], [du], [[glade, [bagare]]]]),
    [10, [elem]]=lists:append([[10], [[elem]]]),
    ok.

append_2(Config) when is_list(Config) ->
    "abcdef"=lists:append("abc", "def"),
    [hej, du]=lists:append([hej], [du]),
    [10, [elem]]=lists:append([10], [[elem]]),
    ok.

%% Tests the lists:reverse() implementation. The function is
%% `non-blocking', and only processes a fixed number of elements at a
%% time.
reverse(Config) when is_list(Config) ->
    reverse_test(0),
    reverse_test(1),
    reverse_test(2),
    reverse_test(128),
    reverse_test(256),
    reverse_test(1000),
    reverse_test(1998),
    reverse_test(1999),
    reverse_test(2000),
    reverse_test(2001),
    reverse_test(3998),
    reverse_test(3999),
    reverse_test(4000),
    reverse_test(4001),
    reverse_test(60001),
    reverse_test(100007),
    ok.

reverse_test(0) ->
    case lists:reverse([]) of
	[] ->
	    ok;
	_Other ->
	    error
    end;
reverse_test(Num) ->
    List0 = ['The Element'|lists:duplicate(Num, 'Ele')],
    List = lists:reverse(List0),
    ['Ele'|_] = List,
    'The Element' = lists:last(List),
    List0 = lists:reverse(List),
    ok.

%% Test the lists:member() implementation.  This test case depends on
%% lists:reverse() to work, wich is tested in a separate test case.
member(Config) when is_list(Config) ->
    {'EXIT',{badarg,_}} = (catch lists:member(45, {a,b,c})),
    {'EXIT',{badarg,_}} = (catch lists:member(45, [0|non_list_tail])),
    false = lists:member(4233, []),
    member_test(1),
    member_test(100),
    member_test(256),
    member_test(1000),
    member_test(1998),
    member_test(1999),
    member_test(2000),
    member_test(2001),
    member_test(3998),
    member_test(3999),
    member_test(4000),
    member_test(4001),
    member_test(100008),
    ok.

member_test(Num) ->
    List0 = ['The Element'|lists:duplicate(Num, 'Elem')],
    true = lists:member('The Element', List0),
    true = lists:member('Elem', List0),
    false = lists:member(arne_anka, List0),
    false = lists:member({a,b,c}, List0),
    List = lists:reverse(List0),
    true = lists:member('The Element', List),
    true = lists:member('Elem', List),
    false = lists:member(arne_anka, List),
    false = lists:member({a,b,c}, List).

keymember(Config) when is_list(Config) ->
    false = lists:keymember(anything_goes, 1, []),
    {'EXIT',{badarg,_}} = (catch lists:keymember(anything_goes, -1, [])),
    {'EXIT',{badarg,_}} = (catch lists:keymember(anything_goes, 0, [])),
    {'EXIT',{badarg,_}} = (catch lists:keymember(anything_goes, 1, {1,2,3})),
    List = [{52.0,a},{-19,b,c},{37.5,d},an_atom,42.0,{39},{45,{x,y,z}}],

    false = lists:keymember(333, 5, List),
    false = lists:keymember(333, 999, List),
    false = lists:keymember(37, 1, List),

    true = lists:keymember(52.0, 1, List),
    true = lists:keymember(52, 1, List),
    true = lists:keymember(-19, 1, List),
    true = lists:keymember(-19.0, 1, List),
    true = lists:keymember(37.5, 1, List),
    true = lists:keymember(39, 1, List),
    true = lists:keymember(39.0, 1, List),
    true = lists:keymember(45, 1, List),
    true = lists:keymember(45.0, 1, List),

    true = lists:keymember(a, 2, List),
    true = lists:keymember(b, 2, List),
    true = lists:keymember(c, 3, List),
    true = lists:keymember(d, 2, List),
    true = lists:keymember({x,y,z}, 2, List),

    Long0 = lists:seq(1, 100007),
    false = lists:keymember(kalle, 1, Long0),
    Long = lists:foldl(fun(E, A) -> [{1/E,E}|A] end, [], Long0),
    true = lists:keymember(1, 2, Long),
    true = lists:keymember(2, 2, Long),
    true = lists:keymember(1.0, 2, Long),
    true = lists:keymember(2.0, 2, Long),
    true = lists:keymember(100006, 2, Long),
    ok.

keysearch_keyfind(Config) when is_list(Config) ->
    false = key_search_find(anything_goes, 1, []),
    {'EXIT',{badarg,_}} = (catch key_search_find(anything_goes, -1, [])),
    {'EXIT',{badarg,_}} = (catch key_search_find(anything_goes, 0, [])),
    {'EXIT',{badarg,_}} = (catch key_search_find(anything_goes, 1, {1,2,3})),

    First = {x,42.0},
    Second = {y,-77},
    Third = {z,[a,b,c],{5.0}},
    List = [First,Second,Third],

    false = key_search_find(333, 1, []),
    false = key_search_find(333, 5, List),
    false = key_search_find(333, 999, List),
    false = key_search_find(37, 1, List),

    {value,First} = key_search_find(42, 2, List),
    {value,First} = key_search_find(42.0, 2, List),

    {value,Second} = key_search_find(-77, 2, List),
    {value,Second} = key_search_find(-77.0, 2, List),

    {value,Third} = key_search_find(z, 1, List),
    {value,Third} = key_search_find([a,b,c], 2, List),
    {value,Third} = key_search_find({5}, 3, List),
    {value,Third} = key_search_find({5.0}, 3, List),

    Long0 = lists:seq(1, 100007),
    false = key_search_find(kalle, 1, Long0),
    Long = lists:foldl(fun(E, A) -> [{1/E,float(E)}|A] end, [], Long0),
    {value,{_,1.0}} = key_search_find(1, 2, Long),
    {value,{_,1.0}} = key_search_find(1.0, 2, Long),
    {value,{_,2.0}} = key_search_find(2, 2, Long),
    {value,{_,2.0}} = key_search_find(2.0, 2, Long),
    {value,{_,33988.0}} = key_search_find(33988, 2, Long),
    {value,{_,33988.0}} = key_search_find(33988.0, 2, Long),
    ok.

%% Test both lists:keysearch/3 and lists:keyfind/3. The only
%% difference between these two functions is that lists:keysearch/3
%% wraps a successfully returned tuple in a value tuple.
%%
key_search_find(Key, Pos, List) ->
    case lists:keyfind(Key, Pos, List) of
	false ->
	    false = lists:keysearch(Key, Pos, List);
	Tuple when is_tuple(Tuple) ->
	    {value,Tuple} = lists:keysearch(Key, Pos, List)
    end.

dropwhile(Config) when is_list(Config) ->
    F = fun(C) -> C =:= $@ end,

    [] = lists:dropwhile(F, []),
    [a] = lists:dropwhile(F, [a]),
    [a,b] = lists:dropwhile(F, [a,b]),
    [a,b,c] = lists:dropwhile(F, [a,b,c]),

    [] = lists:dropwhile(F, [$@]),
    [] = lists:dropwhile(F, [$@,$@]),
    [a,$@] = lists:dropwhile(F, [$@,a,$@]),

    [$k] = lists:dropwhile(F, [$@,$k]),
    [$k,$l] = lists:dropwhile(F, [$@,$@,$k,$l]),
    [a] = lists:dropwhile(F, [$@,$@,$@,a]),

    [a,$@,b] = lists:dropwhile(F, [$@,a,$@,b]),
    [a,$@,b] = lists:dropwhile(F, [$@,$@,a,$@,b]),
    [a,$@,b] = lists:dropwhile(F, [$@,$@,$@,a,$@,b]),

    Long = lists:seq(1, 1024),
    Shorter = lists:seq(800, 1024),

    Shorter = lists:dropwhile(fun(E) -> E < 800 end, Long),

    ok.

takewhile(Config) when is_list(Config) ->
    F = fun(C) -> C =/= $@ end,

    [] = lists:takewhile(F, []),
    [a] = lists:takewhile(F, [a]),
    [a,b] = lists:takewhile(F, [a,b]),
    [a,b,c] = lists:takewhile(F, [a,b,c]),

    [] = lists:takewhile(F, [$@]),
    [] = lists:takewhile(F, [$@,$@]),
    [a] = lists:takewhile(F, [a,$@]),

    [$k] = lists:takewhile(F, [$k,$@]),
    [$k,$l] = lists:takewhile(F, [$k,$l,$@,$@]),
    [a] = lists:takewhile(F, [a,$@,$@,$@]),

    [] = lists:takewhile(F, [$@,a,$@,b]),
    [] = lists:takewhile(F, [$@,$@,a,$@,b]),
    [] = lists:takewhile(F, [$@,$@,$@,a,$@,b]),

    Long = lists:seq(1, 1024),
    Shorter = lists:seq(1, 400),

    Shorter = lists:takewhile(fun(E) -> E =< 400 end, Long),

    ok.

keystore(Config) when is_list(Config) ->
    {'EXIT',_} = (catch lists:keystore(key, 0, [], {1})),
    {'EXIT',_} = (catch lists:keystore(key, 1, {}, {})),
    {'EXIT',_} = (catch lists:keystore(key, 1, {a,b}, {})),
    {'EXIT', _} = (catch lists:keystore(a, 2, [{1,a}], b)),
    T = {k,17},
    [T] = lists:keystore(a, 2, [], T),
    [{1,a},{2,b},{k,17}] = lists:keystore(c, 2, [{1,a},{2,b}],T),
    L = [{1,a},{2,b},{3,c}],
    [{k,17},{2,b},{3,c}] = lists:keystore(a, 2, L, T),
    [{1,a},{k,17},{3,c}] = lists:keystore(b, 2, L, T),
    [{1,a},{2,b},{k,17}] = lists:keystore(c, 2, L, T),
    [{2,b}] = lists:keystore(a, 2, [{1,a}], {2,b}),
    [{1,a}] = lists:keystore(foo, 1, [], {1,a}),
    ok.

keytake(Config) when is_list(Config) ->
    {'EXIT',_} = (catch lists:keytake(key, 0, [])),
    {'EXIT',_} = (catch lists:keytake(key, 1, {})),
    {'EXIT',_} = (catch lists:keytake(key, 1, {a,b})),
    false = lists:keytake(key, 2, [{a}]),
    false = lists:keytake(key, 1, [a]),
    false = lists:keytake(k, 1, []),
    false = lists:keytake(k, 1, [{a},{b},{c}]),
    L = [{a,1},{b,2},{c,3}],
    {value,{a,1},[{b,2},{c,3}]} = lists:keytake(1, 2, L),
    {value,{b,2},[{a,1},{c,3}]} = lists:keytake(2, 2, L),
    {value,{c,3},[{a,1},{b,2}]} = lists:keytake(3, 2, L),
    false = lists:keytake(4, 2, L),
    ok.

%% Test lists:keyreplace/4.
keyreplace(Config) when is_list(Config) ->
    [{new,42}] = lists:keyreplace(k, 1, [{k,1}], {new,42}),
    [atom,{new,a,b}] = lists:keyreplace(k, 1, [atom,{k,1}], {new,a,b}),
    [a,{x,y,z}] = lists:keyreplace(a, 5, [a,{x,y,z}], {no,use}),

    %% Error cases.
    {'EXIT',_} = (catch lists:keyreplace(k, 1, [], not_tuple)),
    {'EXIT',_} = (catch lists:keyreplace(k, 0, [], {a,b})),
    ok.

merge(Config) when is_list(Config) ->

    %% merge list of lists
    [] = lists:merge([]),
    [] = lists:merge([[]]),
    [] = lists:merge([[],[]]),
    [] = lists:merge([[],[],[]]),
    [1] = lists:merge([[1]]),
    [1,1,2,2] = lists:merge([[1,2],[1,2]]),
    [1] = lists:merge([[1],[],[]]),
    [1] = lists:merge([[],[1],[]]),
    [1] = lists:merge([[],[],[1]]),
    [1,2] = lists:merge([[1],[2],[]]),
    [1,2] = lists:merge([[1],[],[2]]),
    [1,2] = lists:merge([[],[1],[2]]),
    [1,2,3,4,5,6] = lists:merge([[1,2],[],[5,6],[],[3,4],[]]),
    [1,2,3,4] = lists:merge([[4],[3],[2],[1]]),
    [1,2,3,4,5] = lists:merge([[1],[2],[3],[4],[5]]),
    [1,2,3,4,5,6] = lists:merge([[1],[2],[3],[4],[5],[6]]),
    [1,2,3,4,5,6,7,8,9] =
	lists:merge([[1],[2],[3],[4],[5],[6],[7],[8],[9]]),
    Seq = lists:seq(1,100),
    true = Seq == lists:merge(lists:map(fun(E) -> [E] end, Seq)),

    Two = [1,2],
    Six = [1,2,3,4,5,6],

    %% 2-way merge
    [] = lists:merge([], []),
    Two = lists:merge(Two, []),
    Two = lists:merge([], Two),
    Six = lists:merge([1,3,5], [2,4,6]),
    Six = lists:merge([2,4,6], [1,3,5]),
    Six = lists:merge([1,2,3], [4,5,6]),
    Six = lists:merge([4,5,6], [1,2,3]),
    Six = lists:merge([1,2,5],[3,4,6]),
    [1,2,3,5,7] = lists:merge([1,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:merge([1,3,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:merge([1,3,5,7], [2,4,6]),
    [1,2,3,5,7] = lists:merge([2], [1,3,5,7]),
    [1,2,3,4,5,7] = lists:merge([2,4], [1,3,5,7]),
    [1,2,3,4,5,6,7] = lists:merge([2,4,6], [1,3,5,7]),

    %% 3-way merge
    [] = lists:merge3([], [], []),
    Two = lists:merge3([], [], Two),
    Two = lists:merge3([], Two, []),
    Two = lists:merge3(Two, [], []),
    Six = lists:merge3([], [1,3,5], [2,4,6]),
    Six = lists:merge3([1,3,5], [], [2,4,6]),
    Six = lists:merge3([1,3,5], [2,4,6], []),
    Nine = lists:merge3([1,4,7],[2,5,8],[3,6,9]),
    Nine = lists:merge3([1,4,7],[3,6,9],[2,5,8]),
    Nine = lists:merge3([3,6,9],[1,4,7],[2,5,8]),
    Nine = lists:merge3([4,5,6],[1,2,3],[7,8,9]),
    Nine = lists:merge3([1,2,3],[4,5,6],[7,8,9]),
    Nine = lists:merge3([7,8,9],[4,5,6],[1,2,3]),
    Nine = lists:merge3([4,5,6],[7,8,9],[1,2,3]),

    ok.

%% reverse merge functions
rmerge(Config) when is_list(Config) ->

    Two = [2,1],
    Six = [6,5,4,3,2,1],

    %% 2-way reversed merge
    [] = lists:rmerge([], []),
    Two = lists:rmerge(Two, []),
    Two = lists:rmerge([], Two),
    Six = lists:rmerge([5,3,1], [6,4,2]),
    Six = lists:rmerge([6,4,2], [5,3,1]),
    Six = lists:rmerge([3,2,1], [6,5,4]),
    Six = lists:rmerge([6,5,4], [3,2,1]),
    Six = lists:rmerge([4,3,2],[6,5,1]),
    [7,6,5,3,1] = lists:rmerge([7,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rmerge([7,5,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rmerge([7,5,3,1], [6,4,2]),
    [7,5,3,2,1] = lists:rmerge([2], [7,5,3,1]),
    [7,5,4,3,2,1] = lists:rmerge([4,2], [7,5,3,1]),
    [7,6,5,4,3,2,1] = lists:rmerge([6,4,2], [7,5,3,1]),

    Nine = [9,8,7,6,5,4,3,2,1],

    %% 3-way reversed merge
    [] = lists:rmerge3([], [], []),
    Two = lists:rmerge3([], [], Two),
    Two = lists:rmerge3([], Two, []),
    Two = lists:rmerge3(Two, [], []),
    Six = lists:rmerge3([], [5,3,1], [6,4,2]),
    Six = lists:rmerge3([5,3,1], [], [6,4,2]),
    Six = lists:rmerge3([5,3,1], [6,4,2], []),
    Nine = lists:rmerge3([7,4,1],[8,5,2],[9,6,3]),
    Nine = lists:rmerge3([7,4,1],[9,6,3],[8,5,2]),
    Nine = lists:rmerge3([9,6,3],[7,4,1],[8,5,2]),
    Nine = lists:rmerge3([6,5,4],[3,2,1],[9,8,7]),
    Nine = lists:rmerge3([3,2,1],[6,5,4],[9,8,7]),
    Nine = lists:rmerge3([9,8,7],[6,5,4],[3,2,1]),
    Nine = lists:rmerge3([6,5,4],[9,8,7],[3,2,1]),

    ok.

sort_1(Config) when is_list(Config) ->
    [] = lists:sort([]),
    [a] = lists:sort([a]),
    [a,a] = lists:sort([a,a]),
    [a,b] = lists:sort([a,b]),
    [a,b] = lists:sort([b,a]),
    [1,1] = lists:sort([1,1]),
    [1,1,2,3] = lists:sort([1,1,3,2]),
    [1,2,3,3] = lists:sort([3,3,1,2]),
    [1,1,1,1] = lists:sort([1,1,1,1]),
    [1,1,1,2,2,2,3,3,3] = lists:sort([3,3,3,2,2,2,1,1,1]),
    [1,1,1,2,2,2,3,3,3] = lists:sort([1,1,1,2,2,2,3,3,3]),

    lists:foreach(fun check/1, perms([1,2,3])),
    lists:foreach(fun check/1, perms([1,2,3,4,5,6,7,8])),
    ok.

%% sort/1 on big randomized lists
sort_rand(Config) when is_list(Config) ->
    ok = check(biglist(10)),
    ok = check(biglist(100)),
    ok = check(biglist(1000)),
    ok = check(biglist(10000)),
    ok.

%% sort/1 was really stable for a while - the order of equal elements
%% was kept - but since the performance suffered a bit, this "feature"
%% was removed.

%% sort/1 should be stable for equal terms.
sort_stable(Config) when is_list(Config) ->
    ok = check_stability(bigfunlist(10)),
    ok = check_stability(bigfunlist(100)),
    ok = check_stability(bigfunlist(1000)),
    case erlang:system_info(modified_timing_level) of
	undefined -> ok = check_stability(bigfunlist(10000));
	_ -> ok
    end,
    ok.

check([]) ->
    ok;
check(L) ->
    S = lists:sort(L),
    case {length(L) == length(S), check(hd(S), tl(S))} of
	{true,ok} ->
	    ok;
	_ ->
	    io:format("~w~n", [L]),
	    erlang:error(check)
    end.

check(_A, []) ->
    ok;
check(A, [B | L]) when A =< B ->
    check(B, L);
check(_A, _L) ->
    no.

%% The check that sort/1 is stable is no longer used.
%% Equal elements are no longer always kept in order.
check_stability(L) ->
    S = lists:sort(L),
    LP = explicit_pid(L),
    SP = explicit_pid(S),
    check_sorted(1, 2, LP, SP).

explicit_pid(L) ->
    lists:reverse(expl_pid(L, [])).

expl_pid([{I,F} | T], L) when is_function(F) ->
    expl_pid(T, [{I,fun_pid(F)} | L]);
expl_pid([], L) ->
    L.


usort_1(Conf) when is_list(Conf) ->
    [] = lists:usort([]),
    [1] = lists:usort([1]),
    [1] = lists:usort([1,1]),
    [1] = lists:usort([1,1,1,1,1]),
    [1,2] = lists:usort([1,2]),
    [1,2] = lists:usort([1,2,1]),
    [1,2] = lists:usort([1,2,2]),
    [1,2,3] = lists:usort([1,3,2]),
    [1,3] = lists:usort([3,1,3]),
    [0,1,3] = lists:usort([3,1,0]),
    [1,2,3] = lists:usort([3,1,2]),
    [1,2] = lists:usort([2,1,1]),
    [1,2] = lists:usort([2,1]),
    [0,3,4,8,9] = lists:usort([3,8,9,0,9,4]),

    lists:foreach(fun ucheck/1, perms([1,2,3])),
    lists:foreach(fun ucheck/1, perms([1,2,3,4,5,6,2,1])),

    ok.

umerge(Conf) when is_list(Conf) ->
    %% merge list of lists
    [] = lists:umerge([]),
    [] = lists:umerge([[]]),
    [] = lists:umerge([[],[]]),
    [] = lists:umerge([[],[],[]]),
    [1] = lists:umerge([[1]]),
    [1,2] = lists:umerge([[1,2],[1,2]]),
    [1] = lists:umerge([[1],[],[]]),
    [1] = lists:umerge([[],[1],[]]),
    [1] = lists:umerge([[],[],[1]]),
    [1,2] = lists:umerge([[1],[2],[]]),
    [1,2] = lists:umerge([[1],[],[2]]),
    [1,2] = lists:umerge([[],[1],[2]]),
    [1,2,3,4,5,6] = lists:umerge([[1,2],[],[5,6],[],[3,4],[]]),
    [1,2,3,4] = lists:umerge([[4],[3],[2],[1]]),
    [1,2,3,4,5] = lists:umerge([[1],[2],[3],[4],[5]]),
    [1,2,3,4,5,6] = lists:umerge([[1],[2],[3],[4],[5],[6]]),
    [1,2,3,4,5,6,7,8,9] =
        lists:umerge([[1],[2],[3],[4],[5],[6],[7],[8],[9]]),
    [1,2,4,6,8] = lists:umerge([[1,2],[2,4,6,8]]),
    Seq = lists:seq(1,100),
    true = Seq == lists:umerge(lists:map(fun(E) -> [E] end, Seq)),

    Two = [1,2],
    Six = [1,2,3,4,5,6],

    %% 2-way unique merge
    [] = lists:umerge([], []),
    Two = lists:umerge(Two, []),
    Two = lists:umerge([], Two),
    Six = lists:umerge([1,3,5], [2,4,6]),
    Six = lists:umerge([2,4,6], [1,3,5]),
    Six = lists:umerge([1,2,3], [4,5,6]),
    Six = lists:umerge([4,5,6], [1,2,3]),
    Six = lists:umerge([1,2,5],[3,4,6]),
    [1,2,3,5,7] = lists:umerge([1,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:umerge([1,3,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:umerge([1,3,5,7], [2,4,6]),
    [1,2,3,5,7] = lists:umerge([2], [1,3,5,7]),
    [1,2,3,4,5,7] = lists:umerge([2,4], [1,3,5,7]),
    [1,2,3,4,5,6,7] = lists:umerge([2,4,6], [1,3,5,7]),

    [1,2,3,5,7] = lists:umerge([1,2,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:umerge([1,2,3,4,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:umerge([1,2,3,4,5,6,7], [2,4,6]),
    [1,2,3,5,7] = lists:umerge([2], [1,2,3,5,7]),
    [1,2,3,4,5,7] = lists:umerge([2,4], [1,2,3,4,5,7]),
    [1,2,3,4,5,6,7] = lists:umerge([2,4,6], [1,2,3,4,5,6,7]),

    %% 3-way unique merge
    [] = lists:umerge3([], [], []),
    Two = lists:umerge3([], [], Two),
    Two = lists:umerge3([], Two, []),
    Two = lists:umerge3(Two, [], []),
    Six = lists:umerge3([], [1,3,5], [2,4,6]),
    Six = lists:umerge3([1,3,5], [], [2,4,6]),
    Six = lists:umerge3([1,3,5], [2,4,6], []),
    Nine = lists:umerge3([1,4,7],[2,5,8],[3,6,9]),
    Nine = lists:umerge3([1,4,7],[3,6,9],[2,5,8]),
    Nine = lists:umerge3([3,6,9],[1,4,7],[2,5,8]),
    Nine = lists:umerge3([4,5,6],[1,2,3],[7,8,9]),
    Nine = lists:umerge3([1,2,3],[4,5,6],[7,8,9]),
    Nine = lists:umerge3([7,8,9],[4,5,6],[1,2,3]),
    Nine = lists:umerge3([4,5,6],[7,8,9],[1,2,3]),

    [1,2,3] = lists:umerge3([1,2,3],[1,2,3],[1,2,3]),
    [1,2,3,4] = lists:umerge3([2,3,4],[1,2,3],[2,3,4]),
    [1,2,3] = lists:umerge3([1,2,3],[2,3],[1,2,3]),
    [1,2,3,4] = lists:umerge3([2,3,4],[3,4],[1,2,3]),

    ok.

rumerge(Conf) when is_list(Conf) ->
    Two = [2,1],
    Six = [6,5,4,3,2,1],

    %% 2-way reversed unique merge
    [] = lists:rumerge([], []),
    Two = lists:rumerge(Two, []),
    Two = lists:rumerge([], Two),
    Six = lists:rumerge([5,3,1], [6,4,2]),
    Six = lists:rumerge([6,4,2], [5,3,1]),
    Six = lists:rumerge([3,2,1], [6,5,4]),
    Six = lists:rumerge([6,5,4], [3,2,1]),
    Six = lists:rumerge([4,3,2],[6,5,1]),
    [7,6,5,3,1] = lists:rumerge([7,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rumerge([7,5,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rumerge([7,5,3,1], [6,4,2]),
    [7,5,3,2,1] = lists:rumerge([2], [7,5,3,1]),
    [7,5,4,3,2,1] = lists:rumerge([4,2], [7,5,3,1]),
    [7,6,5,4,3,2,1] = lists:rumerge([6,4,2], [7,5,3,1]),

    [7,6,5,3,1] = lists:rumerge([7,6,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rumerge([7,6,5,4,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rumerge([7,6,5,4,3,2,1], [6,4,2]),
    [7,5,3,2,1] = lists:rumerge([2], [7,5,3,2,1]),
    [7,5,4,3,2,1] = lists:rumerge([4,2], [7,5,4,3,2,1]),
    [7,6,5,4,3,2,1] = lists:rumerge([6,4,2], [7,6,5,4,3,2,1]),

    Nine = [9,8,7,6,5,4,3,2,1],

    %% 3-way reversed unique merge
    [] = lists:rumerge3([], [], []),
    Two = lists:rumerge3([], [], Two),
    Two = lists:rumerge3([], Two, []),
    Two = lists:rumerge3(Two, [], []),
    Six = lists:rumerge3([], [5,3,1], [6,4,2]),
    Six = lists:rumerge3([5,3,1], [], [6,4,2]),
    Six = lists:rumerge3([5,3,1], [6,4,2], []),
    Nine = lists:rumerge3([7,4,1],[8,5,2],[9,6,3]),
    Nine = lists:rumerge3([7,4,1],[9,6,3],[8,5,2]),
    Nine = lists:rumerge3([9,6,3],[7,4,1],[8,5,2]),
    Nine = lists:rumerge3([6,5,4],[3,2,1],[9,8,7]),
    Nine = lists:rumerge3([3,2,1],[6,5,4],[9,8,7]),
    Nine = lists:rumerge3([9,8,7],[6,5,4],[3,2,1]),
    Nine = lists:rumerge3([6,5,4],[9,8,7],[3,2,1]),

    [3,2,1] = lists:rumerge3([3,2,1],[3,2,1],[3,2,1]),
    [4,3,2,1] = lists:rumerge3([4,3,2],[3,2,1],[3,2,1]),
    [5,4,3,2,1] = lists:rumerge3([4,3,2],[5,4,3,2],[5,4,3,2,1]),
    [6,5,4,3,2] = lists:rumerge3([4,3,2],[5,4,3,2],[6,5,4,3]),

    L1 = [c,d,e],
    L2 = [b,c,d],
    true =
	lists:umerge(L1, L2) == 
	lists:reverse(lists:rumerge(lists:reverse(L1), lists:reverse(L2))),
    ok.

%% usort/1 on big randomized lists.
usort_rand(Config) when is_list(Config) ->
    ok = ucheck(biglist(10)),
    ok = ucheck(biglist(100)),
    ok = ucheck(biglist(1000)),
    ok = ucheck(biglist(10000)),

    ok = ucheck(ubiglist(10)),
    ok = ucheck(ubiglist(100)),
    ok = ucheck(ubiglist(1000)),
    ok = ucheck(ubiglist(10000)),
    ok.

%% usort/1 should keep the first duplicate.
usort_stable(Config) when is_list(Config) ->
    ok = ucheck_stability(bigfunlist(3)),
    ok = ucheck_stability(bigfunlist(10)),
    ok = ucheck_stability(bigfunlist(100)),
    ok = ucheck_stability(bigfunlist(1000)),
    case erlang:system_info(modified_timing_level) of
	undefined -> ok = ucheck_stability(bigfunlist(10000));
	_ -> ok
    end,
    ok.

ucheck([]) ->
    ok;
ucheck(L) ->
    S = lists:usort(L),
    case ucheck(hd(S), tl(S)) of
	ok ->
	    ok;
	_ ->
	    io:format("~w~n", [L]),
	    erlang:error(ucheck)
    end.

ucheck(_A, []) ->
    ok;
ucheck(A, [B | L]) when A < B ->
    ucheck(B, L);
ucheck(_A, _L) ->
    no.

%% Check that usort/1 is stable and correct relative ukeysort/2.
ucheck_stability(L) ->
    S = no_dups(lsort(L)),
    U = lists:usort(L),
    check_stab(L, U, S, "usort/1", "ukeysort/2").


%% Key merge two lists.
keymerge(Config) when is_list(Config) ->

    Two = [{1,a},{2,b}],
    Six = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],

    %% 2-way keymerge
    [] = lists:keymerge(1, [], []),
    Two = lists:keymerge(1, Two, []),
    Two = lists:keymerge(1, [], Two),
    Six = lists:keymerge(1, [{1,a},{3,c},{5,e}], [{2,b},{4,d},{6,f}]),
    Six = lists:keymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e}]),
    Six = lists:keymerge(1, [{1,a},{2,b},{3,c}], [{4,d},{5,e},{6,f}]),
    Six = lists:keymerge(1, [{4,d},{5,e},{6,f}], [{1,a},{2,b},{3,c}]),
    Six = lists:keymerge(1, [{1,a},{2,b},{5,e}],[{3,c},{4,d},{6,f}]),
    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:keymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d},{6,f}]),
    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:keymerge(1, [{2,b}], [{1,a},{3,c},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:keymerge(1, [{2,b},{4,d}], [{1,a},{3,c},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:keymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e},{7,g}]),

    [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:keymerge(1,[{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

%% Reverse key merge two lists.
rkeymerge(Config) when is_list(Config) ->

    Two = [{2,b},{1,a}],
    Six = [{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}],

    %% 2-way reversed keymerge
    [] = lists:rkeymerge(1, [], []),
    Two = lists:rkeymerge(1, Two, []),
    Two = lists:rkeymerge(1, [], Two),
    Six = lists:rkeymerge(1, [{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    Six = lists:rkeymerge(1, [{6,f},{4,d},{2,b}], [{5,e},{3,c},{1,a}]),
    Six = lists:rkeymerge(1, [{3,c},{2,b},{1,a}], [{6,f},{5,e},{4,d}]),
    Six = lists:rkeymerge(1, [{6,f},{5,e},{4,d}], [{3,c},{2,b},{1,a}]),
    Six = lists:rkeymerge(1, [{4,d},{3,c},{2,b}],[{6,f},{5,e},{1,a}]),
    [{7,g},{6,f},{5,e},{3,c},{1,a}] =
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] =
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rkeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    [{7,g},{5,e},{3,c},{2,b},{1,a}] =
	lists:rkeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rkeymerge(1, [{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rkeymerge(1, [{6,f},{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),

    L1 = [{c,11},{c,12},{e,5}],
    L2 = [{b,2},{c,21},{c,22}],
    true =
	lists:keymerge(1, L1, L2) == 
	lists:reverse(lists:rkeymerge(1,lists:reverse(L1), 
				      lists:reverse(L2))),

    ok.

keysort_1(Config) when is_list(Config) ->
    ok = keysort_check(1, [], []),
    ok = keysort_check(1, [{a,b}], [{a,b}]),
    ok = keysort_check(1, [{a,b},{a,b}], [{a,b},{a,b}]),
    ok = keysort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ok = keysort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ok = keysort_check(1,
		       [{1,e},{3,f},{2,y},{0,z},{x,14}],
		       [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ok = keysort_check(1,
		       [{1,a},{1,a},{1,a},{1,a}],
		       [{1,a},{1,a},{1,a},{1,a}]),

    [{b,1},{c,1}] = lists:keysort(1, [{c,1},{b,1}]),
    [{a,0},{b,2},{c,3},{d,4}] =
	lists:keysort(1, [{d,4},{c,3},{b,2},{a,0}]),
    [{a,0},{b,1},{b,2},{c,1}] =
	lists:keysort(1, [{c,1},{b,1},{b,2},{a,0}]),
    [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	lists:keysort(1, [{c,1},{b,1},{b,2},{a,0},{d,4}]),

    SFun = fun(L) -> fun(X) -> keysort_check(1, X, L) end end,
    L1 = [{1,a},{2,b},{3,c}],
    lists:foreach(SFun(L1), perms(L1)),
    L2 = [{1,a},{1,a},{2,b}],
    lists:foreach(SFun(L2), perms(L2)),
    L3 = [{1,a},{1,a},{1,a},{2,b}],
    lists:foreach(SFun(L3), perms(L3)),
    L4 = [{a,1},{a,1},{b,2},{b,2},{c,3},{d,4},{e,5},{f,6}],
    lists:foreach(SFun(L4), perms(L4)),

    ok.

%% keysort should be stable
keysort_stable(Config) when is_list(Config) ->
    ok = keysort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ok = keysort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ok = keysort_check(1,
		       [{1,c},{1,b},{2,x},{3,p},{2,a}],
		       [{1,c},{1,b},{2,x},{2,a},{3,p}]),
    ok = keysort_check(1,
		       [{1,a},{1,b},{1,a},{1,a}],
		       [{1,a},{1,b},{1,a},{1,a}]),
    ok.

%% keysort should exit when given bad arguments
keysort_error(Config) when is_list(Config) ->
    {'EXIT', _} = (catch lists:keysort(0, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:keysort(3, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:keysort(1.5, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:keysort(x, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:keysort(x, [])),
    {'EXIT', _} = (catch lists:keysort(x, [{1,b}])),
    {'EXIT', _} = (catch lists:keysort(1, [a,b])),
    {'EXIT', _} = (catch lists:keysort(1, [{1,b} | {1,c}])),
    ok.

%% keysort with other key than first element
keysort_i(Config) when is_list(Config) ->
    ok = keysort_check(2, [{a,2},{b,1},{c,3}], [{b,1},{a,2},{c,3}]),
    ok.

%% keysort on big randomized lists
keysort_rand(Config) when is_list(Config) ->
    ok = keysort_check3(1, biglist(10)),
    ok = keysort_check3(1, biglist(100)),
    ok = keysort_check3(1, biglist(1000)),
    ok = keysort_check3(1, biglist(10000)),

    ok = keysort_check3(2, biglist(10)),
    ok = keysort_check3(2, biglist(100)),
    ok = keysort_check3(2, biglist(1000)),
    ok = keysort_check3(2, biglist(10000)),
    ok.

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
keysort_check(I, Input, Expected) ->
    Expected = lists:keysort(I, Input),
    check_sorted(I, Input, Expected).

keysort_check3(I, Input) ->
    check_sorted(I, 3, Input, lists:keysort(I, Input)).

check_sorted(I, Input, L) ->
    check_sorted(I, I, Input, L).

%%% Check that a list is keysorted by element I. Elements comparing equal
%%% should be sorted according to element J.
check_sorted(_I, _J, _Input, []) ->
    ok;
check_sorted(I, J, Input, [A | Rest]) ->
    case catch check_sorted1(I, J, A, Rest) of
	{'EXIT', _} ->
	    io:format("~w~n", [Input]),
	    erlang:error(check_sorted);
	Reply ->
	    Reply
    end.

check_sorted1(_I, _J, _A, []) ->
    ok;
check_sorted1(I, J, A, [B | Rest]) ->
    ok = keycompare(I, J, A, B),
    check_sorted1(I, J, B, Rest).

keycompare(I, _J, A, B) when element(I, A) < element(I, B) ->
    ok;
keycompare(I, J, A, B) when element(I, A) == element(I, B), 
			    element(J, A) =< element(J, B) ->
    ok.


%% Merge two lists while removing duplicates.
ukeymerge(Conf) when is_list(Conf) ->

    Two = [{1,a},{2,b}],
    Six = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],

    %% 2-way unique keymerge
    [] = lists:ukeymerge(1, [], []),
    Two = lists:ukeymerge(1, Two, []),
    Two = lists:ukeymerge(1, [], Two),
    [] = lists:ukeymerge(1, [], []),
    Two = lists:ukeymerge(1, Two, []),
    Two = lists:ukeymerge(1, [], Two),
    Six = lists:ukeymerge(1, [{1,a},{3,c},{5,e}], [{2,b},{4,d},{6,f}]),
    Six = lists:ukeymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e}]),
    Six = lists:ukeymerge(1, [{1,a},{2,b},{3,c}], [{4,d},{5,e},{6,f}]),
    Six = lists:ukeymerge(1, [{4,d},{5,e},{6,f}], [{1,a},{2,b},{3,c}]),
    Six = lists:ukeymerge(1, [{1,a},{2,b},{5,e}],[{3,c},{4,d},{6,f}]),
    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{7,g}], [{2,b},{4,d},{6,f}]),
    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:ukeymerge(1, [{2,b}], [{1,a},{3,c},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:ukeymerge(1, [{2,b},{4,d}], [{1,a},{3,c},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:ukeymerge(1, [{2,b},{4,d},{6,f}], [{1,a},{3,c},{5,e},{7,g}]),

    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:ukeymerge(1, [{1,a},{2,b},{3,c},{5,e},{7,g}], [{2,b}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:ukeymerge(1, [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}], 
			[{2,b},{4,d}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:ukeymerge(1, [{1,a},{3,c},{5,e},{6,f},{7,g}], 
			[{2,b},{4,d},{6,f}]),
    [{1,a},{2,b},{3,c},{5,e},{7,g}] =
	lists:ukeymerge(1, [{2,b}], [{1,a},{2,b},{3,c},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}] =
	lists:ukeymerge(1, [{2,b},{4,d}], 
			[{1,a},{2,b},{3,c},{4,d},{5,e},{7,g}]),
    [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}] =
	lists:ukeymerge(1, [{2,b},{4,d},{6,f}], 
			[{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g}]),

    L1 = [{a,1},{a,3},{a,5},{a,7}],
    L2 = [{b,1},{b,3},{b,5},{b,7}],
    L1 = lists:ukeymerge(2, L1, L2),

    ok.

%% Reverse merge two lists while removing duplicates.
rukeymerge(Conf) when is_list(Conf) ->

    Two = [{2,b},{1,a}],
    Six = [{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}],

    %% 2-way reversed unique keymerge
    [] = lists:rukeymerge(1, [], []),
    Two = lists:rukeymerge(1, Two, []),
    Two = lists:rukeymerge(1, [], Two),
    Six = lists:rukeymerge(1, [{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    Six = lists:rukeymerge(1, [{6,f},{4,d},{2,b}], [{5,e},{3,c},{1,a}]),
    Six = lists:rukeymerge(1, [{3,c},{2,b},{1,a}], [{6,f},{5,e},{4,d}]),
    Six = lists:rukeymerge(1, [{6,f},{5,e},{4,d}], [{3,c},{2,b},{1,a}]),
    Six = lists:rukeymerge(1, [{4,d},{3,c},{2,b}],[{6,f},{5,e},{1,a}]),
    [{7,g},{6,f},{5,e},{3,c},{1,a}] =
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] =
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{7,g},{5,e},{3,c},{1,a}], [{6,f},{4,d},{2,b}]),
    [{7,g},{5,e},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{6,f},{4,d},{2,b}], [{7,g},{5,e},{3,c},{1,a}]),

    [{7,g},{6,f},{5,e},{3,c},{1,a}] =
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{3,c},{1,a}], [{6,f}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}] =
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{4,d},{3,c},{1,a}], 
			 [{6,f},{4,d}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}], 
			 [{6,f},{4,d},{2,b}]),
    [{7,g},{5,e},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{2,b}], [{7,g},{5,e},{3,c},{2,b},{1,a}]),
    [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{4,d},{2,b}], 
			 [{7,g},{5,e},{4,d},{3,c},{2,b},{1,a}]),
    [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}] =
	lists:rukeymerge(1, [{6,f},{4,d},{2,b}], 
			 [{7,g},{6,f},{5,e},{4,d},{3,c},{2,b},{1,a}]),

    L1 = [{a,1},{a,3},{a,5},{a,7}],
    L2 = [{b,1},{b,3},{b,5},{b,7}],
    true =
	lists:ukeymerge(2, L1, L2) == 
	lists:reverse(lists:rukeymerge(2, lists:reverse(L1), 
				       lists:reverse(L2))),

    ok.

ukeysort_1(Config) when is_list(Config) ->
    ok = ukeysort_check(1, [], []),
    ok = ukeysort_check(1, [{a,b}], [{a,b}]),
    ok = ukeysort_check(1, [{a,b},{a,b}], [{a,b}]),
    ok = ukeysort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ok = ukeysort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ok = ukeysort_check(1,
			[{1,e},{3,f},{2,y},{0,z},{x,14}],
			[{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ok = ukeysort_check(1, [{1,a},{1,a},{1,a},{1,a}], [{1,a}]),

    L1 = [{1,a},{1,b},{1,a}],
    L1u = lists:ukeysort(1, L1),
    L2 = [{1,a},{1,b},{1,a}],
    L2u = lists:ukeysort(1, L2),
    ok = ukeysort_check(1, lists:keymerge(1, L1, L2),
			lists:ukeymerge(1, L1u, L2u)),
    L3 = [{1,a},{1,b},{1,a},{2,a}],
    L3u = lists:ukeysort(1, L3),
    ok = ukeysort_check(1, lists:keymerge(1, L3, L2),
			lists:ukeymerge(1, L3u, L2u)),
    L4 = [{1,b},{1,a}],
    L4u = lists:ukeysort(1, L4),
    ok = ukeysort_check(1, lists:keymerge(1, L1, L4),
			lists:ukeymerge(1, L1u, L4u)),
    L5 = [{1,a},{1,b},{1,a},{2,a}],
    L5u = lists:ukeysort(1, L5),
    ok = ukeysort_check(1, lists:keymerge(1, [], L5),
			lists:ukeymerge(1, [], L5u)),
    ok = ukeysort_check(1, lists:keymerge(1, L5, []),
			lists:ukeymerge(1, L5u, [])),
    L6 = [{3,a}],
    L6u = lists:ukeysort(1, L6),
    ok = ukeysort_check(1, lists:keymerge(1, L5, L6),
			lists:ukeymerge(1, L5u, L6u)),

    [{b,1},{c,1}] = lists:ukeysort(1, [{c,1},{c,1},{c,1},{c,1},{b,1}]),
    [{a,0},{b,2},{c,3},{d,4}] =
	lists:ukeysort(1, [{d,4},{c,3},{b,2},{b,2},{a,0}]),
    [{a,0},{b,1},{c,1}] =
	lists:ukeysort(1, [{c,1},{b,1},{b,1},{b,2},{b,2},{a,0}]),
    [{a,0},{b,1},{c,1},{d,4}] =
	lists:ukeysort(1, [{c,1},{b,1},{b,2},{a,0},{a,0},{d,4},{d,4}]),

    SFun = fun(L) -> fun(X) -> ukeysort_check(2, X, L) end end,
    PL = [{a,1},{b,2},{c,3},{d,4},{e,5},{f,6}],
    Ps = perms([{a,1},{b,2},{c,3},{d,4},{e,5},{f,6},{b,2},{a,1}]),
    lists:foreach(SFun(PL), Ps),

    M1L = [{1,a},{1,a},{2,b}],
    M1s = [{1,a},{2,b}],
    lists:foreach(SFun(M1s), perms(M1L)),
    M2L = [{1,a},{2,b},{2,b}],
    M2s = [{1,a},{2,b}],
    lists:foreach(SFun(M2s), perms(M2L)),
    M3 = [{1,a},{2,b},{3,c}],
    lists:foreach(SFun(M3), perms(M3)),

    ok.

%% ukeysort should keep the first duplicate.
ukeysort_stable(Config) when is_list(Config) ->
    ok = ukeysort_check(1, [{1,b},{1,c}], [{1,b}]),
    ok = ukeysort_check(1, [{1,c},{1,b}], [{1,c}]),
    ok = ukeysort_check(1,
			[{1,c},{1,b},{2,x},{3,p},{2,a}],
			[{1,c},{2,x},{3,p}]),

    ok = ukeysort_check(1, [{1,a},{1,b},{1,b}], [{1,a}]),
    ok = ukeysort_check(1, [{2,a},{1,b},{2,a}], [{1,b},{2,a}]),

    ok = ukeysort_check_stability(bigfunlist(3)),
    ok = ukeysort_check_stability(bigfunlist(10)),
    ok = ukeysort_check_stability(bigfunlist(100)),
    ok = ukeysort_check_stability(bigfunlist(1000)),
    case erlang:system_info(modified_timing_level) of
	undefined -> ok = ukeysort_check_stability(bigfunlist(10000));
	_ -> ok
    end,
    ok.

%% ukeysort should exit when given bad arguments.
ukeysort_error(Config) when is_list(Config) ->
    {'EXIT', _} = (catch lists:ukeysort(0, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:ukeysort(3, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:ukeysort(1.5, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:ukeysort(x, [{1,b},{1,c}])),
    {'EXIT', _} = (catch lists:ukeysort(x, [])),
    {'EXIT', _} = (catch lists:ukeysort(x, [{1,b}])),
    {'EXIT', _} = (catch lists:ukeysort(1, [a,b])),
    {'EXIT', _} = (catch lists:ukeysort(1, [{1,b} | {1,c}])),
    ok.

%% ukeysort with other key than first element.
ukeysort_i(Config) when is_list(Config) ->
    ok = ukeysort_check(2, [{a,2},{b,1},{c,3}], [{b,1},{a,2},{c,3}]),
    ok.

%% ukeysort on big randomized lists.
ukeysort_rand(Config) when is_list(Config) ->
    ok = ukeysort_check3(2, biglist(10)),
    ok = ukeysort_check3(2, biglist(100)),
    ok = ukeysort_check3(2, biglist(1000)),
    ok = ukeysort_check3(2, biglist(10000)),

    ok = gen_ukeysort_check(1, ubiglist(10)),
    ok = gen_ukeysort_check(1, ubiglist(100)),
    ok = gen_ukeysort_check(1, ubiglist(1000)),
    ok = gen_ukeysort_check(1, ubiglist(10000)),
    ok.

%% Check that ukeysort/2 is stable and correct relative keysort/2.
%% (this is not affected by the fact that keysort/2 is no longer really 
%%  stable; ucheck_stability/1 checks ukeysort/2 (and usort/1, of course))
gen_ukeysort_check(I, Input) ->
    U = lists:ukeysort(I, Input),
    S = lists:keysort(I, Input),
    case U == no_dups_keys(S, I) of
	true ->
	    ok;
	false ->
	    io:format("~w~n", [Input]),
	    erlang:error(gen_ukeysort_check)
    end.

%% Used for checking that the first copy is kept.
ukeysort_check_stability(L) ->
    I = 1,
    U = lists:ukeysort(I, L),
    S = no_dups_keys(lkeysort(I, L), I),
    check_stab(L, U, S, "ukeysort/2", "usort/2").

%%% Uniquely keysort a list, check that the returned list is what we
%%% expected, and that it is actually sorted.
ukeysort_check(I, Input, Expected) ->
    Expected = lists:ukeysort(I, Input),
    ucheck_sorted(I, Input, Expected).

ukeysort_check3(I, Input) ->
    ucheck_sorted(I, 3, Input, lists:ukeysort(I, Input)).

ucheck_sorted(I, Input, L) ->
    ucheck_sorted(I, I, Input, L).

%%% Check that a list is ukeysorted by element I. Elements comparing
%%% equal should be sorted according to element J.
ucheck_sorted(_I, _J, _Input, []) ->
    ok;
ucheck_sorted(I, J, Input, [A | Rest]) ->
    case catch ucheck_sorted1(I, J, A, Rest) of
	{'EXIT', _} ->
	    io:format("~w~n", [Input]),
	    erlang:error(ucheck_sorted);
	Reply ->
	    Reply
    end.

ucheck_sorted1(_I, _J, _A, []) ->
    ok;
ucheck_sorted1(I, J, A, [B | Rest]) ->
    ok = ukeycompare(I, J, A, B),
    ucheck_sorted1(I, J, B, Rest).

ukeycompare(I, _J, A, B) when element(I, A) < element(I, B) ->
    ok;
ukeycompare(I, J, A, B) when A =/= B,
			     element(I, A) == element(I, B), 
			     element(J, A) =< element(J, B) ->
    ok.



%% Merge two lists using a fun.
funmerge(Config) when is_list(Config) ->

    Two = [1,2],
    Six = [1,2,3,4,5,6],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way merge
    [] = lists:merge(F, [], []),
    Two = lists:merge(F, Two, []),
    Two = lists:merge(F, [], Two),
    Six = lists:merge(F, [1,3,5], [2,4,6]),
    Six = lists:merge(F, [2,4,6], [1,3,5]),
    Six = lists:merge(F, [1,2,3], [4,5,6]),
    Six = lists:merge(F, [4,5,6], [1,2,3]),
    Six = lists:merge(F, [1,2,5],[3,4,6]),
    [1,2,3,5,7] = lists:merge(F, [1,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:merge(F, [1,3,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:merge(F, [1,3,5,7], [2,4,6]),
    [1,2,3,5,7] = lists:merge(F, [2], [1,3,5,7]),
    [1,2,3,4,5,7] = lists:merge(F, [2,4], [1,3,5,7]),
    [1,2,3,4,5,6,7] = lists:merge(F, [2,4,6], [1,3,5,7]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    [{b,2},{c,11},{c,12},{c,21},{c,22},{e,5}] =
	lists:merge(F2,[{c,11},{c,12},{e,5}], [{b,2},{c,21},{c,22}]),

    ok.

%% Reverse merge two lists using a fun.
rfunmerge(Config) when is_list(Config) ->

    Two = [2,1],
    Six = [6,5,4,3,2,1],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way reversed merge
    [] = lists:rmerge(F, [], []),
    Two = lists:rmerge(F, Two, []),
    Two = lists:rmerge(F, [], Two),
    Six = lists:rmerge(F, [5,3,1], [6,4,2]),
    Six = lists:rmerge(F, [6,4,2], [5,3,1]),
    Six = lists:rmerge(F, [3,2,1], [6,5,4]),
    Six = lists:rmerge(F, [6,5,4], [3,2,1]),
    Six = lists:rmerge(F, [4,3,2],[6,5,1]),
    [7,6,5,3,1] = lists:rmerge(F, [7,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rmerge(F, [7,5,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rmerge(F, [7,5,3,1], [6,4,2]),
    [7,5,3,2,1] = lists:rmerge(F, [2], [7,5,3,1]),
    [7,5,4,3,2,1] = lists:rmerge(F, [4,2], [7,5,3,1]),
    [7,6,5,4,3,2,1] = lists:rmerge(F, [6,4,2], [7,5,3,1]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    L1 = [{c,11},{c,12},{e,5}],
    L2 = [{b,2},{c,21},{c,22}],
    true =
	lists:merge(F2, L1, L2) == 
	lists:reverse(lists:rmerge(F2,lists:reverse(L1), lists:reverse(L2))),

    ok.


funsort_1(Config) when is_list(Config) ->
    ok = funsort_check(1, [], []),
    ok = funsort_check(1, [{a,b}], [{a,b}]),
    ok = funsort_check(1, [{a,b},{a,b}], [{a,b},{a,b}]),
    ok = funsort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ok = funsort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ok = funsort_check(1,
		       [{1,e},{3,f},{2,y},{0,z},{x,14}],
		       [{0,z},{1,e},{2,y},{3,f},{x,14}]),
    F = funsort_fun(1),

    [{b,1},{c,1}] = lists:sort(F, [{c,1},{b,1}]),
    [{a,0},{b,2},{c,3},{d,4}] =
	lists:sort(F, [{d,4},{c,3},{b,2},{a,0}]),
    [{a,0},{b,1},{b,2},{c,1}] =
	lists:sort(F, [{c,1},{b,1},{b,2},{a,0}]),
    [{a,0},{b,1},{b,2},{c,1},{d,4}] =
	lists:sort(F, [{c,1},{b,1},{b,2},{a,0},{d,4}]),

    SFun = fun(L) -> fun(X) -> funsort_check(1, X, L) end end,
    L1 = [{1,a},{1,a},{2,b},{2,b},{3,c},{4,d},{5,e},{6,f}],
    lists:foreach(SFun(L1), perms(L1)),

    ok.

%% sort/2 should be stable.
funsort_stable(Config) when is_list(Config) ->
    ok = funsort_check(1, [{1,b},{1,c}], [{1,b},{1,c}]),
    ok = funsort_check(1, [{1,c},{1,b}], [{1,c},{1,b}]),
    ok = funsort_check(1,
		       [{1,c},{1,b},{2,x},{3,p},{2,a}],
		       [{1,c},{1,b},{2,x},{2,a},{3,p}]),
    ok.

%% sort/2 should exit when given bad arguments.
funsort_error(Config) when is_list(Config) ->
    {'EXIT', _} = (catch lists:sort(1, [{1,b} , {1,c}])),
    {'EXIT', _} = (catch lists:sort(fun(X,Y) -> X =< Y end,
				    [{1,b} | {1,c}])),
    ok.

%% sort/2 on big randomized lists.
funsort_rand(Config) when is_list(Config) ->
    ok = funsort_check3(1, biglist(10)),
    ok = funsort_check3(1, biglist(100)),
    ok = funsort_check3(1, biglist(1000)),
    ok = funsort_check3(1, biglist(10000)),
    ok.

%% Do a keysort
funsort(I, L) ->
    lists:sort(funsort_fun(I), L).

funsort_check3(I, Input) ->
    check_sorted(I, 3, Input, funsort(I, Input)).

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
funsort_check(I, Input, Expected) ->
    Expected = funsort(I, Input),
    check_sorted(I, Input, Expected).


%% Merge two lists while removing duplicates using a fun.
ufunmerge(Conf) when is_list(Conf) ->

    Two = [1,2],
    Six = [1,2,3,4,5,6],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way unique merge
    [] = lists:umerge(F, [], []),
    Two = lists:umerge(F, Two, []),
    Two = lists:umerge(F, [], Two),
    Six = lists:umerge(F, [1,3,5], [2,4,6]),
    Six = lists:umerge(F, [2,4,6], [1,3,5]),
    Six = lists:umerge(F, [1,2,3], [4,5,6]),
    Six = lists:umerge(F, [4,5,6], [1,2,3]),
    Six = lists:umerge(F, [1,2,5],[3,4,6]),
    [1,2,3,5,7] = lists:umerge(F, [1,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:umerge(F, [1,3,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:umerge(F, [1,3,5,7], [2,4,6]),
    [1,2,3,5,7] = lists:umerge(F, [2], [1,3,5,7]),
    [1,2,3,4,5,7] = lists:umerge(F, [2,4], [1,3,5,7]),
    [1,2,3,4,5,6,7] = lists:umerge(F, [2,4,6], [1,3,5,7]),

    [1,2,3,5,7] = lists:umerge(F, [1,2,3,5,7], [2]),
    [1,2,3,4,5,7] = lists:umerge(F, [1,2,3,4,5,7], [2,4]),
    [1,2,3,4,5,6,7] = lists:umerge(F, [1,3,5,6,7], [2,4,6]),
    [1,2,3,5,7] = lists:umerge(F, [2], [1,2,3,5,7]),
    [1,2,3,4,5,7] = lists:umerge(F, [2,4], [1,2,3,4,5,7]),
    [1,2,3,4,5,6,7] = lists:umerge(F, [2,4,6], [1,2,3,4,5,6,7]),

    L1 = [{a,1},{a,3},{a,5},{a,7}],
    L2 = [{b,1},{b,3},{b,5},{b,7}],
    F2 = fun(X,Y) -> element(2,X) =< element(2,Y) end,
    L1 = lists:umerge(F2, L1, L2),
    [{b,2},{e,5},{c,11},{c,12},{c,21},{c,22}] =
	lists:umerge(F2, [{e,5},{c,11},{c,12}], [{b,2},{c,21},{c,22}]),

    ok.

%% Reverse merge two lists while removing duplicates using a fun.
rufunmerge(Conf) when is_list(Conf) ->
    Two = [2,1],
    Six = [6,5,4,3,2,1],
    F = fun(X, Y) -> X =< Y end,

    %% 2-way reversed unique merge
    [] = lists:rumerge(F, [], []),
    Two = lists:rumerge(F, Two, []),
    Two = lists:rumerge(F, [], Two),
    Six = lists:rumerge(F, [5,3,1], [6,4,2]),
    Six = lists:rumerge(F, [6,4,2], [5,3,1]),
    Six = lists:rumerge(F, [3,2,1], [6,5,4]),
    Six = lists:rumerge(F, [6,5,4], [3,2,1]),
    Six = lists:rumerge(F, [4,3,2],[6,5,1]),
    [7,6,5,3,1] = lists:rumerge(F, [7,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rumerge(F, [7,5,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rumerge(F, [7,5,3,1], [6,4,2]),
    [7,5,3,2,1] = lists:rumerge(F, [2], [7,5,3,1]),
    [7,5,4,3,2,1] = lists:rumerge(F, [4,2], [7,5,3,1]),
    [7,6,5,4,3,2,1] = lists:rumerge(F, [6,4,2], [7,5,3,1]),

    [7,6,5,3,1] = lists:rumerge(F, [7,6,5,3,1], [6]),
    [7,6,5,4,3,1] = lists:rumerge(F, [7,6,5,4,3,1], [6,4]),
    [7,6,5,4,3,2,1] = lists:rumerge(F, [7,6,5,4,3,2,1], [6,4,2]),
    [7,5,3,2,1] = lists:rumerge(F, [2], [7,5,3,2,1]),
    [7,5,4,3,2,1] = lists:rumerge(F, [4,2], [7,5,4,3,2,1]),
    [7,6,5,4,3,2,1] = lists:rumerge(F, [6,4,2], [7,6,5,4,3,2,1]),

    F2 = fun(X,Y) -> element(1,X) =< element(1,Y) end,
    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    true = lists:umerge(F2, L1, L2) ==
	lists:reverse(lists:rumerge(F, lists:reverse(L2), lists:reverse(L1))),

    L3 = [{c,11},{c,12},{e,5}],
    L4 = [{b,2},{c,21},{c,22}],
    true =
	lists:umerge(F2, L3, L4) == 
	lists:reverse(lists:rumerge(F2,lists:reverse(L3), lists:reverse(L4))),

    ok.

ufunsort_1(Config) when is_list(Config) ->
    ok = ufunsort_check(1, [], []),
    ok = ufunsort_check(1, [{a,b}], [{a,b}]),
    ok = ufunsort_check(1, [{a,b},{a,b}], [{a,b}]),
    ok = ufunsort_check(1, [{a,b},{b,c}], [{a,b},{b,c}]),
    ok = ufunsort_check(1, [{b,c},{a,b}], [{a,b},{b,c}]),
    ok = ufunsort_check(1,
			[{1,e},{3,f},{2,y},{0,z},{x,14}],
			[{0,z},{1,e},{2,y},{3,f},{x,14}]),
    ok = ufunsort_check(1,
			[{1,a},{2,b},{3,c},{2,b},{1,a},{2,b},{3,c},
			 {2,b},{1,a}],
			[{1,a},{2,b},{3,c}]),
    ok = ufunsort_check(1,
			[{1,a},{1,a},{1,b},{1,b},{1,a},{2,a}],
			[{1,a},{2,a}]),

    F = funsort_fun(1),
    L1 = [{1,a},{1,b},{1,a}],
    L2 = [{1,a},{1,b},{1,a}],
    ok = ufunsort_check(1, lists:keymerge(1, L1, L2),
			lists:umerge(F, lists:usort(F, L1),
				     lists:usort(F, L2))),
    L3 = [{1,a},{1,b},{1,a},{2,a}],
    ok = ufunsort_check(1, lists:keymerge(1, L3, L2),
			lists:umerge(F, lists:usort(F, L3),
				     lists:usort(F, L2))),
    L4 = [{1,b},{1,a}],
    ok = ufunsort_check(1, lists:keymerge(1, L1, L4),
			lists:umerge(F, lists:usort(F, L1),
				     lists:usort(F, L4))),
    L5 = [{1,a},{1,b},{1,a},{2,a}],
    ok = ufunsort_check(1, lists:keymerge(1, L5, []),
			lists:umerge(F, lists:usort(F, L5), [])),
    L6 = [{3,a}],
    ok = ufunsort_check(1, lists:keymerge(1, L5, L6),
			lists:umerge(F, lists:usort(F, L5),
				     lists:usort(F, L6))),

    [{b,1},{c,1}] = lists:usort(F, [{c,1},{c,1},{b,1}]),
    [{a,0},{b,2},{c,3},{d,4}] =
	lists:usort(F, [{d,4},{c,3},{b,2},{b,2},{a,0}]),
    [{a,0},{b,1},{c,1}] =
	lists:usort(F, [{c,1},{b,1},{b,1},{b,2},{b,2},{a,0}]),
    [{a,0},{b,1},{c,1},{d,4}] =
	lists:usort(F, [{c,1},{b,1},{b,2},{a,0},{a,0},{d,4},{d,4}]),

    SFun = fun(L) -> fun(X) -> ufunsort_check(1, X, L) end end,
    PL = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f}],
    Ps = perms([{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{2,b},{1,a}]),
    lists:foreach(SFun(PL), Ps),

    ok.

%% usort/2 should be stable.
ufunsort_stable(Config) when is_list(Config) ->
    ok = ufunsort_check(1, [{1,b},{1,c}], [{1,b}]),
    ok = ufunsort_check(1, [{1,c},{1,b}], [{1,c}]),
    ok = ufunsort_check(1,
			[{1,c},{1,b},{2,x},{3,p},{2,a}],
			[{1,c},{2,x},{3,p}]),

    ok = ufunsort_check_stability(bigfunlist(10)),
    ok = ufunsort_check_stability(bigfunlist(100)),
    ok = ufunsort_check_stability(bigfunlist(1000)),
    case erlang:system_info(modified_timing_level) of
	undefined -> ok = ufunsort_check_stability(bigfunlist(10000));
	_ -> ok
    end,
    ok.

%% usort/2 should exit when given bad arguments.
ufunsort_error(Config) when is_list(Config) ->
    {'EXIT', _} = (catch lists:usort(1, [{1,b} , {1,c}])),
    {'EXIT', _} = (catch lists:usort(fun(X,Y) -> X =< Y end,
				     [{1,b} | {1,c}])),
    ok.

%% usort/2 on big randomized lists.
ufunsort_rand(Config) when is_list(Config) ->
    ok = ufunsort_check3(1, biglist(10)),
    ok = ufunsort_check3(1, biglist(100)),
    ok = ufunsort_check3(1, biglist(1000)),
    ok = ufunsort_check3(1, biglist(10000)),

    ok = gen_ufunsort_check(1, ubiglist(100)),
    ok = gen_ufunsort_check(1, ubiglist(1000)),
    ok = gen_ufunsort_check(1, ubiglist(10000)),
    ok.

%% Check that usort/2 is stable and correct relative sort/2.
gen_ufunsort_check(I, Input) ->
    U = ufunsort(I, Input),
    S = funsort(I, Input),
    case U == no_dups_keys(S, I) of
	true ->
	    ok;
	false ->
	    io:format("~w~n", [Input]),
	    erlang:error(gen_ufunsort_check)
    end.

%% Used for checking that the first copy is kept.
ufunsort_check_stability(L) ->
    I = 1,
    U = ufunsort(I, L),
    S = no_dups(funsort(I, L)),
    check_stab(L, U, S, "usort/2", "sort/2").

ufunsort_check3(I, Input) ->
    ucheck_sorted(I, 3, Input, ufunsort(I, Input)).

%%% Keysort a list, check that the returned list is what we expected,
%%% and that it is actually sorted.
ufunsort_check(I, Input, Expected) ->
    Expected = ufunsort(I, Input),
    ucheck_sorted(I, Input, Expected).

%% Do a keysort
ufunsort(I, L) ->
    lists:usort(funsort_fun(I), L).

funsort_fun(I) ->
    fun(A, B) when tuple_size(A) >= I, tuple_size(B) >= I ->
            element(I, A) =< element(I, B)
    end.

check_stab(L, U, S, US, SS) ->
    UP = explicit_pid(U),
    SP = explicit_pid(S),
    case UP == SP of
	true ->
	    ok;
	false ->
	    io:format("In: ~w~n", [explicit_pid(L)]),
	    io:format("~s: ~w~n", [US, UP]),
	    io:format("~s:  ~w~n", [SS, SP]),
	    erlang:error(unstable)
    end.

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 3-tuples with
%%% random integer elements in the range 0..44 as elements 1 and 2.
%%% Element 3 in the tuple is the position of the tuple in the list.

biglist(N) ->
    rand:seed(exsplus),
    biglist(N, []).

biglist(0, L) ->
    L;
biglist(N, L) ->
    E = random_tuple(45, N),
    biglist(N-1, [E|L]).

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 2-tuples with
%%% random integer elements in the range 0..10 as element 1.
%%% Element 2 in the tuple is a random integer in the range 0..5.
%%% No sequence number.

ubiglist(N) ->
    rand:seed(exsplus),
    ubiglist(N, []).

ubiglist(0, L) ->
    L;
ubiglist(N, L) ->
    E = urandom_tuple(11, 6),
    ubiglist(N-1, [E|L]).

urandom_tuple(N, I) ->
    R1 = randint(N),
    R2 = randint(I),
    {R1, R2}.

%%%------------------------------------------------------------
%%% Generate lists of given length, containing 2-tuples with random
%%% integer elements in the range 0..10 as elements 1. All tuples have
%%% the same function as element 2, but every function is created in a
%%% unique process. ==/2 will return 'true' for any pair of functions,
%%% but erlang:fun_info(Fun, pid) can be used for distinguishing
%%% functions created in different processes. The pid acts like a
%%% sequence number.

bigfunlist(N) ->
    rand:seed(exsplus),
    bigfunlist_1(N).

bigfunlist_1(N) when N < 30000 -> % Now (R8) max 32000 different pids.
    case catch bigfunlist(N, 0, []) of
	{'EXIT', _} ->
	    bigfunlist_1(N);
	Reply ->
	    Reply
    end.

bigfunlist(0, _P, L) ->
    lists:reverse(L);
bigfunlist(N, P, L) ->
    {E, NP} = random_funtuple(P, 11),
    bigfunlist(N-1, NP, [E | L]).

random_funtuple(P, N) ->
    R = randint(N),
    F = make_fun(),
    NP = fun_pid(F),
    true = NP > P,
    {{R, F}, NP}.

make_fun() ->
    Pid = spawn(?MODULE, make_fun, [self()]),
    receive {Pid, Fun} -> Fun end.

make_fun(Pid) ->
    Pid ! {self(), fun make_fun/1}.

fun_pid(Fun) ->
    erlang:fun_info(Fun, pid).

random_tuple(N, Seq) ->
    R1 = randint(N),
    R2 = randint(N),
    {R1, R2, Seq}.

randint(N) ->
    trunc(rand:uniform() * N).

%% The first "duplicate" is kept.
no_dups([]) ->
    [];
no_dups([H | T]) ->
    no_dups(H, T, []).

no_dups(H, [H1 | T], L) when H == H1 ->
    no_dups(H, T, L);
no_dups(H, [H1 | T], L) ->
    no_dups(H1, T, [H | L]);
no_dups(H, [], L) ->
    lists:reverse([H | L]).

%% The first "duplicate" is kept.
no_dups_keys([], _I) ->
    [];
no_dups_keys([H | T], I) ->
    no_dups_keys(H, T, [], I).

no_dups_keys(H, [H1 | T], L, I) when element(I, H) == element(I, H1) ->
    no_dups_keys(H, T, L, I);
no_dups_keys(H, [H1 | T], L, I) ->
    no_dups_keys(H1, T, [H | L], I);
no_dups_keys(H, [], L, _I) ->
    lists:reverse([H | L]).

perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L--[H])].

%%%------------------------------------------------------------
%%% Test the sort routines with randomly generated lists.

-record(state, {sort = 0, usort = 0, stable = 0}).

%% Run it interactively. 'stop' or 'info' recognized commands.
sort_loop() ->
    sort_loop(5000).

sort_loop(N) when is_integer(N), N > 0 ->
    Pid = spawn_link(?MODULE, sloop, [N]),
    sort_loop_1(Pid).

sort_loop_1(Pid) ->
    case io:get_line('? ') of
	eof ->
	    ok;
	"stop\n" ->
	    Pid ! {self(), stop},
	    receive {Pid, S} -> display_state(S) end;
	"info\n" ->
	    Pid ! {self(), info},
	    receive {Pid, S} -> display_state(S) end,
	    sort_loop_1(Pid);
	_Other ->
	    sort_loop_1(Pid)
    end.

sloop(N) ->
    rand:seed(exsplus),
    sloop(N, #state{}).

sloop(N, S) ->
    receive
	{From, stop} ->
	    From ! {self(), S};
	{From, info} ->
	    From ! {self(), S},
	    sloop(N, S)
    after 0 ->
	    Len = randint(N),
	    NS = case randint(3) of
		     0 ->
			 BL = biglist(Len, []),
			 ok = check(BL),
			 ok = keysort_check3(1, BL),
			 ok = funsort_check3(1, BL),
			 S#state{sort = S#state.sort + 1};
		     1 ->
			 BL = ubiglist(Len, []),
			 ok = ucheck(BL),
			 ok = gen_ukeysort_check(1, BL),
			 ok = gen_ufunsort_check(1, BL),
			 S#state{usort = S#state.usort + 1};
		     2 ->
			 BL = bigfunlist(Len),
			 %% ok = check_stability(BL),
			 ok = ucheck_stability(BL),
			 ok = ukeysort_check_stability(BL),
			 ok = ufunsort_check_stability(BL),
			 S#state{stable = S#state.stable + 1}
		 end,
	    sloop(N, NS)
    end.

display_state(S) ->    
    io:format("sort:   ~p~n", [S#state.sort]),
    io:format("usort:  ~p~n", [S#state.usort]),
    io:format("stable: ~p~n", [S#state.stable]).

%% This version of sort/1 is really stable; the order of equal
%% elements is kept. It is used for checking the current
%% implementation of usort/1 etc.

lsort([X, Y | L] = L0) when X =< Y ->
    case L of
	[] -> 
	    L0;
	[Z] when Y =< Z ->
	    L0;
	[Z] when X =< Z ->
	    [X, Z, Y];
	[Z] ->
	    [Z, X, Y];
	_ ->
	    split_1(X, Y, L, [], [])
    end;
lsort([X, Y | L]) ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X =< Z ->
	    [Y, X | L];
	[Z] when Y =< Z ->
	    [Y, Z, X];
	[Z] ->
	    [Z, Y, X];
	_ ->
	    split_2(X, Y, L, [], [])
    end;
lsort([_] = L) ->
    L;
lsort([] = L) ->
    L.

split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
    split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
    split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
    split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
    split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
    rmergel([[Y, X | R] | Rs], [], asc).

split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
    split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
    split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
    split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
    split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
    rmergel([[S], [Y, X | R] | Rs], [], asc).

split_2(X, Y, [Z | L], R, Rs) when Z < Y ->
    split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z < X ->
    split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
    split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
    split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
    mergel([[Y, X | R] | Rs], [], desc).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z < Y ->
    split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z < X ->
    split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
    split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
    mergel([[S], [Y, X | R] | Rs], [], desc).

mergel([[] | L], Acc, O) ->
    mergel(L, Acc, O);
mergel([T1, [H2 | T2] | L], Acc, asc) ->
    mergel(L, [merge2_1(T1, H2, T2, []) | Acc], asc);
mergel([[H2 | T2], T1 | L], Acc, desc) ->
    mergel(L, [merge2_1(T1, H2, T2, []) | Acc], desc);
mergel([L], [], _O) ->
    L;
mergel([L], Acc, O) ->
    rmergel([lists:reverse(L, []) | Acc], [], O);
mergel([], [], _O) ->
    [];
mergel([], Acc, O) ->
    rmergel(Acc, [], O);
mergel([A, [] | L], Acc, O) ->
    mergel([A | L], Acc, O);
mergel([A, B, [] | L], Acc, O) ->
    mergel([A, B | L], Acc, O).

rmergel([[H2 | T2], T1 | L], Acc, asc) ->
    rmergel(L, [rmerge2_1(T1, H2, T2, []) | Acc], asc);
rmergel([T1, [H2 | T2] | L], Acc, desc) ->
    rmergel(L, [rmerge2_1(T1, H2, T2, []) | Acc], desc);
rmergel([L], Acc, O) ->
    mergel([lists:reverse(L, []) | Acc], [], O);
rmergel([], Acc, O) ->
    mergel(Acc, [], O).

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_2(T1, H1, [H2 | T2], M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_2(T1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_2(T1, H1, [H2 | T2], M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_2(T1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).



%% This version of keysort/2 is really stable; the order of equal
%% elements is kept. It is used for checking the current
%% implementation of ukeysort/2 etc.

lkeysort(Index, L) when is_integer(Index), Index > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    EX = element(Index, X),
	    EY = element(Index, Y),
	    if
		EX =< EY ->
		    keysplit_1(Index, X, EX, Y, EY, T, [], []);
		true ->
		    keysplit_2(Index, Y, EY, T, [X])
	    end
    end.

keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    EZ = element(I, Z),
    if 
	EY =< EZ ->
	    keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
	EX =< EZ ->
	    keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
	true, R == [] ->
	    keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
	true ->
	    keysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], []).

%% One out-of-order element, S.
keysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
	    keysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
	EX =< EZ ->
	    keysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
	ES =< EZ ->
	    keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
	true ->
	    keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
keysplit_2(I, Y, EY, [Z | L], R) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        true ->
            keysplit_2(I, Z, EZ, L, [Y | R])
    end;
keysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

keymergel(I, [T1, [H2 | T2] | L], Acc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
keymergel(_I, [L], []) ->
    L;
keymergel(I, [L], Acc) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], []);
keymergel(I, [], Acc) ->
    rkeymergel(I, Acc, []).

rkeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
rkeymergel(I, [L], Acc) ->
    keymergel(I, [lists:reverse(L, []) | Acc], []);
rkeymergel(I, [], Acc) ->
    keymergel(I, Acc, []).

keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if 
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rkeymerge2_2(I, T1, E1, [H2 | T2], M, H1) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_2(_I, T1, _E1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).


%%%------------------------------------------------------------


%% Test for infinite loop (OTP-2404).
seq_loop(Config) when is_list(Config) ->
    _ = (catch lists:seq(1, 5, -1)),
    ok.

%% Non-error cases for seq/2.
seq_2(Config) when is_list(Config) ->
    [1,2,3] = lists:seq(1,3),
    [1] = lists:seq(1,1),
    Big = 748274827583793785928592859,
    Big1 = Big+1,
    Big2 = Big+2,
    [Big, Big1, Big2] = lists:seq(Big, Big+2),
    ok.

%% Error cases for seq/2.
seq_2_e(Config) when is_list(Config) ->
    seq_error([4, 2]),
    seq_error([1, a]),
    seq_error([1.0, 2.0]),
    ok.

seq_error(Args) ->
    {'EXIT', _} = (catch apply(lists, seq, Args)).

%% Non-error cases for seq/3.
seq_3(Config) when is_list(Config) ->
    [1,2,3] = lists:seq(1,3,1),
    [1] = lists:seq(1,1,1),
    Big = 748274827583793785928592859,
    Big1 = Big+1,
    Big2 = Big+2,
    [Big, Big1, Big2] = lists:seq(Big, Big+2,1),

    [3,2,1] = lists:seq(3,1,-1),
    [1] = lists:seq(1,1,-1),

    [3,1] = lists:seq(3,1,-2),
    [1] = lists:seq(1, 10, 10),
    [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 19, 3),
    [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 20, 3),
    [1, 4, 7, 10, 13, 16, 19] = lists:seq(1, 21, 3),

    [1] = lists:seq(1, 1, 0),		%OTP-2613
    ok.

%% Error cases for seq/3.
seq_3_e(Config) when is_list(Config) ->
    seq_error([4, 2, 1]),
    seq_error([3, 5, -1]),
    seq_error([1, a, 1]),
    seq_error([1.0, 2.0, 1]),

    seq_error([1, 3, 1.0]),
    seq_error([1, 3, a]),
    seq_error([1, 3, 0]),

    seq_error([a, a, 0]),
    ok.

%% OTP-7230. seq/1,2 returns the empty list.
otp_7230(Config) when is_list(Config) ->
    From = -10,
    To = 10,
    StepFrom = -10,
    StepTo = 10,

    L = lists:seq(From, To),
    SL = lists:seq(StepFrom, StepTo),
    [] =
	[{F, T, S} ||
	    F <- L, T <- L, S <- SL,
	    not check_seq(F, T, S, catch lists:seq(F, T, S))
		orelse
		S =:= 1 andalso not check_seq(F, T, S, catch lists:seq(F, T))
        ].

check_seq(From, To, 0, R) ->
    From =:= To andalso R =:= [From]
	orelse
	From =/= To andalso is_tuple(R) andalso element(1, R) =:= 'EXIT';
check_seq(From, To, Step, []) when Step =/= 0 ->
    0 =:= property(From, To, Step)
	andalso
	  (
	  Step > 0 andalso To < From andalso From-To =< Step
	  orelse
	  Step < 0 andalso To > From andalso To-From =< -Step
	 );
check_seq(From, To, Step, R) when R =/= [], To < From, Step > 0 ->
    is_tuple(R) andalso element(1, R) =:= 'EXIT';
check_seq(From, To, Step, R) when R =/= [], To > From, Step < 0 ->
    is_tuple(R) andalso element(1, R) =:= 'EXIT';
check_seq(From, To, Step, L) when is_list(L), L =/= [], Step =/= 0 ->
    First = hd(L),
    Last = lists:last(L),
    Min = lists:min(L),
    Max = lists:max(L),

    [] =:= [E || E <- L, not is_integer(E)]
	andalso
    %% The difference between two consecutive elements is Step:
	begin
	    LS = [First-Step]++L,
	    LR = L++[Last+Step],
	    [Step] =:= lists:usort([B-A || {A,B} <- lists:zip(LS, LR)])
	end
	andalso
    %% The first element of L is From:
	From =:= First
	andalso
    %% No element outside the given interval:
	Min >= lists:min([From, To])
	andalso
	Max =< lists:max([From, To])
	andalso
    %% All elements are present:
	abs(To-Last) < abs(Step)
	andalso
	length(L) =:= property(From, To, Step);
check_seq(_From, _To, _Step, _R) ->
    false.

property(From, To, Step) ->
    ((To-From+Step) div Step).

%%%------------------------------------------------------------


-define(sublist_error2(X,Y), {'EXIT', _} = (catch lists:sublist(X,Y))).
-define(sublist_error3(X,Y,Z), {'EXIT', _} = (catch lists:sublist(X,Y,Z))).

sublist_2(Config) when is_list(Config) ->
    [] = lists:sublist([], 0),
    [] = lists:sublist([], 1),
    [] = lists:sublist([a], 0),
    [a] = lists:sublist([a], 1),
    [a] = lists:sublist([a], 2),
    [a] = lists:sublist([a|b], 1),

    [a,b] = lists:sublist([a,b|c], 2),

    ok.

%% sublist/2 error cases.
sublist_2_e(Config) when is_list(Config) ->
    ?sublist_error2([], -1),
    ?sublist_error2(a, -1),
    ?sublist_error2(a, 0),
    ?sublist_error2([a|b], 2),
    ?sublist_error2([a], x),
    ?sublist_error2([a], 1.5),
    ?sublist_error2([], x),
    ?sublist_error2([], 1.5),
    ok.

sublist_3(Config) when is_list(Config) ->
    [] = lists:sublist([], 1, 0),
    [] = lists:sublist([], 1, 1),
    [] = lists:sublist([a], 1, 0),
    [a] = lists:sublist([a], 1, 1),
    [a] = lists:sublist([a], 1, 2),
    [a] = lists:sublist([a|b], 1, 1),

    [] = lists:sublist([], 1, 0),
    [] = lists:sublist([], 1, 1),
    [] = lists:sublist([a], 1, 0),
    [a] = lists:sublist([a], 1, 1),
    [a] = lists:sublist([a], 1, 2),
    [] = lists:sublist([a], 2, 1),
    [] = lists:sublist([a], 2, 2),
    [] = lists:sublist([a], 2, 79),
    [] = lists:sublist([a,b|c], 1, 0),
    [] = lists:sublist([a,b|c], 2, 0),
    [a] = lists:sublist([a,b|c], 1, 1),
    [b] = lists:sublist([a,b|c], 2, 1),
    [a,b] = lists:sublist([a,b|c], 1, 2),

    [] = lists:sublist([a], 2, 0),

    ok.

%% sublist/3 error cases
sublist_3_e(Config) when is_list(Config) ->
    ?sublist_error3([], 1, -1),
    ?sublist_error3(a, 1, -1),
    ?sublist_error3(a, 1, 0),
    ?sublist_error3([a|b], 1, 2),
    ?sublist_error3([a], 1, x),
    ?sublist_error3([a], 1, 1.5),
    ?sublist_error3([], 1, x),
    ?sublist_error3([], 1, 1.5),

    ?sublist_error3([], -1, 0),
    ?sublist_error3(a, x, -1),
    ?sublist_error3([a,b], 0.5, 1),
    ?sublist_error3([a,b], 1.5, 1),
    ?sublist_error3([a], 1, x),
    ?sublist_error3([a], 1, 1.5),
    ?sublist_error3([], 1, x),
    ?sublist_error3([], 1, 1.5),

    ?sublist_error3([a], 0, -1),
    ?sublist_error3([a], 1, -1),
    ?sublist_error3([a], 2, -1),
    ?sublist_error3([a], 0, 0),
    ?sublist_error3([a], 0, 1),

    ?sublist_error3([a,b|c], 2, 2),
    ?sublist_error3([a,b|c], 3, 0),
    ?sublist_error3([a,b|c], 3, 1),
    ok.

%%%------------------------------------------------------------


-define(flatten_error1(X), {'EXIT', _} = (catch lists:flatten(X))).
-define(flatten_error2(X,Y), {'EXIT', _} = (catch lists:flatten(X,Y))).

%% Test lists:flatten/1,2 and lists:flatlength/1.
flatten_1(Config) when is_list(Config) ->
    [] = lists_flatten([]),
    [1,2] = lists_flatten([1,2]),
    [1,2] = lists_flatten([1,[2]]),
    [1,2] = lists_flatten([[1],2]),
    [1,2] = lists_flatten([[1],[2]]),
    [1,2] = lists_flatten([[1,2]]),
    [a,b,c,d] = lists_flatten([[a],[b,c,[d]]]),

    ok.

lists_flatten(List) ->
    Flat = lists:flatten(List),
    Flat = lists:flatten(List, []),
    Len = lists:flatlength(List),
    Len = length(Flat),
    Flat.

%% flatten/1 error cases
flatten_1_e(Config) when is_list(Config) ->
    ?flatten_error1(a),
    ?flatten_error1([a|b]),
    ?flatten_error1([[a],[b|c],[d]]),
    ok.

%%% [arndt] What if second arg isn't a proper list? This issue isn't
%%% clear-cut. Right now, I think that any term should be allowed.
%%% But I also wish this function didn't exist at all.

%% Test lists:flatten/2.
flatten_2(Config) when is_list(Config) ->
    [] = lists:flatten([], []),
    [a] = lists:flatten([a], []),
    [a,b,c,[no,flatten]] = lists:flatten([[a,[b,c]]], [[no,flatten]]),
    ok.

%% flatten/2 error cases.
flatten_2_e(Config) when is_list(Config) ->
    ok.

%% Test lists:zip/2, lists:unzip/1.
zip_unzip(Config) when is_list(Config) ->
    [] = lists:zip([], []),
    [{a,b}] = lists:zip([a], [b]),
    [{42.0,{kalle,nisse}},{a,b}] = lists:zip([42.0,a], [{kalle,nisse},b]),

    %% Longer lists.
    SeqA = lists:seq(45, 200),
    SeqB = [A*A || A <- SeqA],
    AB = lists:zip(SeqA, SeqB),
    SeqA = [A || {A,_} <- AB],
    SeqB = [B || {_,B} <- AB],
    {SeqA,SeqB} = lists:unzip(AB),

    %% Some more unzip/1.
    {[],[]} = lists:unzip([]),
    {[a],[b]} = lists:unzip([{a,b}]),
    {[a,c],[b,d]} = lists:unzip([{a,b},{c,d}]),

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:zip([], [b])),
    {'EXIT',{function_clause,_}} = (catch lists:zip([a], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zip([a], [b,c])),
    {'EXIT',{function_clause,_}} = (catch lists:zip([a], [b,c])),
    ok.

%% Test lists:zip3/3, lists:unzip3/1.
zip_unzip3(Config) when is_list(Config) ->
    [] = lists:zip3([], [], []),
    [{a,b,c}] = lists:zip3([a], [b], [c]),

    %% Longer lists.
    SeqA = lists:seq(45, 200),
    SeqB = [2*A || A <- SeqA],
    SeqC = [A*A || A <- SeqA],
    ABC = lists:zip3(SeqA, SeqB, SeqC),
    SeqA = [A || {A,_,_} <- ABC],
    SeqB = [B || {_,B,_} <- ABC],
    SeqC = [C || {_,_,C} <- ABC],
    {SeqA,SeqB,SeqC} = lists:unzip3(ABC),

    %% Some more unzip3/1.
    {[],[],[]} = lists:unzip3([]),
    {[a],[b],[c]} = lists:unzip3([{a,b,c}]),

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:zip3([], [], [c])),
    {'EXIT',{function_clause,_}} = (catch lists:zip3([], [b], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zip3([a], [], [])),

    ok.

%% Test lists:zipwith/3.
zipwith(Config) when is_list(Config) ->
    Zip = fun(A, B) -> [A|B] end,

    [] = lists:zipwith(Zip, [], []),
    [[a|b]] = lists:zipwith(Zip, [a], [b]),

    %% Longer lists.
    SeqA = lists:seq(77, 300),
    SeqB = [A*A || A <- SeqA],
    AB = lists:zipwith(Zip, SeqA, SeqB),
    SeqA = [A || [A|_] <- AB],
    SeqB = [B || [_|B] <- AB],

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:zipwith(badfun, [], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith(Zip, [], [b])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith(Zip, [a], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith(Zip, [a], [b,c])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith(Zip, [a], [b,c])),
    ok.

%% Test lists:zipwith3/4.
zipwith3(Config) when is_list(Config) ->
    Zip = fun(A, B, C) -> [A,B,C] end,

    [] = lists:zipwith3(Zip, [], [], []),
    [[a,b,c]] = lists:zipwith3(Zip, [a], [b], [c]),

    %% Longer lists.
    SeqA = lists:seq(45, 200),
    SeqB = [2*A || A <- SeqA],
    SeqC = [A*A || A <- SeqA],
    ABC = lists:zipwith3(Zip, SeqA, SeqB, SeqC),
    SeqA = [A || [A,_,_] <- ABC],
    SeqB = [B || [_,B,_] <- ABC],
    SeqC = [C || [_,_,C] <- ABC],

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:zipwith3(badfun, [], [], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith3(Zip, [], [], [c])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith3(Zip, [], [b], [])),
    {'EXIT',{function_clause,_}} = (catch lists:zipwith3(Zip, [a], [], [])),

    ok.

%% Test lists:join/2
join(Config) when is_list(Config) ->
    A = [a,b,c],
    Sep = x,
    [a,x,b,x,c] = lists:join(Sep, A),

    B = [b],
    [b] = lists:join(Sep, B),

    C = [],
    [] = lists:join(Sep, C),
    ok.

%% Test lists:filter/2, lists:partition/2.
filter_partition(Config) when is_list(Config) ->
    F = fun(I) -> I rem 2 =:= 0 end,
    filpart(F, [], []),
    filpart(F, [1], []),
    filpart(F, [1,3,17], []),
    filpart(F, [1,2,3,17], [2]),
    filpart(F, [6,8,1,2,3,17], [6,8,2]),
    filpart(F, [6,8,1,2,42,3,17], [6,8,2,42]),

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:filter(badfun, [])),
    {'EXIT',{function_clause,_}} = (catch lists:partition(badfun, [])),
    ok.

filpart(F, All, Exp) ->
    Exp = lists:filter(F, All),
    Other = lists:filter(fun(E) -> not F(E) end, All),
    {Exp,Other} = lists:partition(F, All).


%% OTP-5939. Guard tests added.
otp_5939(Config) when is_list(Config) ->
    Fun1 = fun(A) -> A end,
    Fun2 = fun(A, B) -> {A,B} end,
    Fun3 = fun(A, B, C) -> {A,B,C} end,
    Pred = fun(_A) -> true end,
    Fold = fun(_E, A) -> A end,
    MapFold = fun(E, A) -> {E,A} end,

    {'EXIT', _} = (catch lists:usort( [asd], [qwe])),

    {'EXIT', _} = (catch lists:zipwith(func, [], [])),
    [] = lists:zipwith(Fun2, [], []),
    {'EXIT', _} = (catch lists:zipwith3(func, [], [], [])),
    [] = lists:zipwith3(Fun3, [], [], []),
    {'EXIT', _} = (catch lists:keymap(func, 1, [])),
    {'EXIT', _} = (catch lists:keymap(Fun1, 0, [])),
    [] = lists:keymap(Fun1, 1, []),
    {'EXIT', _} = (catch lists:merge(func, [], [1])),
    {'EXIT', _} = (catch lists:merge(func, [1], [])),
    [] = lists:merge(Fun2, [], []),
    {'EXIT', _} = (catch lists:rmerge(func, [], [1])),
    {'EXIT', _} = (catch lists:rmerge(func, [1], [])),
    [] = lists:rmerge(Fun2, [], []),
    {'EXIT', _} = (catch lists:usort(func, [])),
    {'EXIT', _} = (catch lists:usort(func, [a])),
    {'EXIT', _} = (catch lists:usort(func, [a, b])),
    [] = lists:usort(Fun2, []),
    {'EXIT', _} = (catch lists:umerge(func, [], [1])),
    {'EXIT', _} = (catch lists:merge(func, [1], [])),
    [] = lists:umerge(Fun2, [], []),
    {'EXIT', _} = (catch lists:rumerge(func, [], [1])),
    {'EXIT', _} = (catch lists:rumerge(func, [1], [])),
    [] = lists:rumerge(Fun2, [], []),
    {'EXIT', _} = (catch lists:all(func, [])),
    true = lists:all(Pred, []),
    {'EXIT', _} = (catch lists:any(func, [])),
    false = lists:any(Pred, []),
    {'EXIT', _} = (catch lists:map(func, [])),
    [] = lists:map(Fun1, []),
    {'EXIT', _} = (catch lists:flatmap(func, [])),
    [] = lists:flatmap(Fun1, []),
    {'EXIT', _} = (catch lists:foldl(func, [], [])),
    [] = lists:foldl(Fold, [], []),
    {'EXIT', _} = (catch lists:foldr(func, [], [])),
    [] = lists:foldr(Fold, [], []),
    {'EXIT', _} = (catch lists:filter(func, [])),
    [] = lists:filter(Pred, []),
    {'EXIT', _} = (catch lists:partition(func, [])),
    {[],[]} = lists:partition(Pred, []),
    {'EXIT', _} = (catch lists:filtermap(func, [])),
    [] = lists:filtermap(Fun1, []),
    {'EXIT', _} = (catch lists:foreach(func, [])),
    ok = lists:foreach(Fun1, []),
    {'EXIT', _} = (catch lists:mapfoldl(func, [], [])),
    {[],[]} = lists:mapfoldl(MapFold, [], []),
    {'EXIT', _} = (catch lists:mapfoldr(func, [], [])),
    {[],[]} = lists:mapfoldr(MapFold, [], []),
    {'EXIT', _} = (catch lists:takewhile(func, [])),
    [] = lists:takewhile(Pred, []),
    {'EXIT', _} = (catch lists:dropwhile(func, [])),
    [] = lists:dropwhile(Pred, []),
    {'EXIT', _} = (catch lists:splitwith(func, [])),
    {[],[]} = lists:splitwith(Pred, []),

    ok.

%% OTP-6023. lists:keyreplace/4, a typecheck.
otp_6023(Config) when is_list(Config) ->
    {'EXIT', _} = (catch lists:keyreplace(a, 2, [{1,a}], b)),
    [{2,b}] = lists:keyreplace(a, 2, [{1,a}], {2,b}),

    ok.

%% OTP-6606. sort and keysort bug.
otp_6606(Config) when is_list(Config) ->
    I = 1,
    F = float(1),
    L1 = [{F,I},{F,F},{I,I},{I,F}],
    L1 = lists:keysort(1, L1),
    L1 = lists:sort(L1),
    L2 = [{I,I},{I,F},{F,I},{F,F}],
    L2 = lists:keysort(1, L2),
    L2 = lists:sort(L2),
    ok.

%% Test lists:suffix/2.
suffix(Config) when is_list(Config) ->
    true = lists:suffix([], []),
    true = lists:suffix([], [a]),
    true = lists:suffix([], [a,b]),
    true = lists:suffix([], [a,b,c]),
    true = lists:suffix([a], lists:duplicate(200000, a)),
    true = lists:suffix(lists:seq(1, 1024),
			lists:seq(2, 64000) ++ lists:seq(1, 1024)),
    true = lists:suffix(lists:duplicate(20000, a),
			lists:duplicate(200000, a)),
    true = lists:suffix([2.0,3.0], [1.0,2.0,3.0]),

    %% False cases.
    false = lists:suffix([a], []),
    false = lists:suffix([a,b,c], []),
    false = lists:suffix([a,b,c], [b,c]),
    false = lists:suffix([a,b,c], [a,b,c,a,b]),
    false = lists:suffix(lists:duplicate(199999, a)++[b],
			 lists:duplicate(200000, a)),
    false = lists:suffix([2.0,3.0], [1,2,3]),

    %% Error cases.
    {'EXIT',_} = (catch lists:suffix({a,b,c}, [])),
    {'EXIT',_} = (catch lists:suffix([], {a,b})),
    {'EXIT',_} = (catch lists:suffix([a|b], [])),
    {'EXIT',_} = (catch lists:suffix([a,b|c], [a|b])),
    {'EXIT',_} = (catch lists:suffix([a|b], [a,b|c])),
    {'EXIT',_} = (catch lists:suffix([a|b], [a|b])),

    ok.

%% Test lists:subtract/2 and the '--' operator.
subtract(Config) when is_list(Config) ->
    [] = sub([], []),
    [] = sub([], [a]),
    [] = sub([], lists:seq(1, 1024)),
    sub_non_matching([a], []),
    sub_non_matching([1,2], [make_ref()]),
    sub_non_matching(lists:seq(1, 1024), [make_ref(),make_ref()]),

    %% Matching subtracts.
    [] = sub([a], [a]),
    [a] = sub([a,b], [b]),
    [a] = sub([a,b], [b,c]),
    [a] = sub([a,b,c], [b,c]),
    [a] = sub([a,b,c], [b,c]),
    [d,a,a] = sub([a,b,c,d,a,a], [a,b,c]),
    [d,x,a] = sub([a,b,c,d,a,x,a], [a,b,c,a]),
    [1,2,3,4,5,6,7,8,9,9999,10000,20,21,22] =
	sub(lists:seq(1, 10000)++[20,21,22], lists:seq(10, 9998)),

    %% Floats/integers.
    [42.0,42.0] = sub([42.0,42,42.0], [42,42,42]),
    [1,2,3,4,43.0] = sub([1,2,3,4,5,42.0,43.0], [42.0,5]),

    %% Crashing subtracts.
    {'EXIT',_} = (catch sub([], [a|b])),
    {'EXIT',_} = (catch sub([a], [a|b])),
    {'EXIT',_} = (catch sub([a|b], [])),
    {'EXIT',_} = (catch sub([a|b], [])),
    {'EXIT',_} = (catch sub([a|b], [a])),

    ok.

sub_non_matching(A, B) ->
    A = sub(A, B).

sub(A, B) ->
    Res = A -- B,
    Res = lists:subtract(A, B).

%% Test lists:droplast/1
droplast(Config) when is_list(Config) ->
    [] = lists:droplast([x]),
    [x] = lists:droplast([x, y]),
    {'EXIT', {function_clause, _}} = (catch lists:droplast([])),
    {'EXIT', {function_clause, _}} = (catch lists:droplast(x)),

    ok.

%% Test lists:search/2
search(Config) when is_list(Config) ->
    F = fun(I) -> I rem 2 =:= 0 end,
    F2 = fun(A, B) -> A > B end,

    {value, 2} = lists:search(F, [1,2,3,4]),
    false = lists:search(F, [1,3,5,7]),
    false = lists:search(F, []),

    %% Error cases.
    {'EXIT',{function_clause,_}} = (catch lists:search(badfun, [])),
    {'EXIT',{function_clause,_}} = (catch lists:search(F2, [])),
    ok.

%% Briefly test the common high-order functions to ensure they
%% are covered.
hof(Config) when is_list(Config) ->
    L = [1,2,3],
    [1,4,9] = lists:map(fun(N) -> N*N end, L),
    [1,4,5,6] = lists:flatmap(fun(1) -> [1];
				 (2) -> [];
				 (3) -> [4,5,6]
			      end, L),
    [{1,[a]},{2,[b]},{3,[c]}] =
	lists:keymap(fun(A) -> [A] end, 2, [{1,a},{2,b},{3,c}]),

    [1,3] = lists:filter(fun(N) -> N rem 2 =:= 1 end, L),
    FilterMapFun = fun(1) -> true;
		      (2) -> {true,42};
		      (3) -> false
		   end,
    [1,42] = lists:filtermap(FilterMapFun, L),
    [1,42] = lists:zf(FilterMapFun, L),

    [3,2,1] = lists:foldl(fun(E, A) -> [E|A] end, [], L),
    [1,2,3] = lists:foldr(fun(E, A) -> [E|A] end, [], L),
    {[1,4,9],[3,2,1]} = lists:mapfoldl(fun(E, A) ->
					       {E*E,[E|A]}
				       end, [], L),
    {[1,4,9],[1,2,3]} = lists:mapfoldr(fun(E, A) ->
					       {E*E,[E|A]}
				       end, [], L),

    true = lists:any(fun(N) -> N =:= 2 end, L),
    false = lists:any(fun(N) -> N =:= 42 end, L),

    true = lists:all(fun(N) -> is_integer(N) end, L),
    false = lists:all(fun(N) -> N rem 2 =:= 0 end, L),

    ok.
