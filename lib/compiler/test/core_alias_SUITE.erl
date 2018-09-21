%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
-module(core_alias_SUITE).

-export([all/0, suite/0, groups/0,init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         tuples/1, cons/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [tuples, cons]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


id(X) -> X.

tuples(Config) when is_list(Config) ->
    Tuple = {ok,id(value)},

    true = erts_debug:same(Tuple, simple_tuple(Tuple)),
    true = erts_debug:same(Tuple, simple_tuple_in_map(#{hello => Tuple})),
    true = erts_debug:same(Tuple, simple_tuple_case_repeated(Tuple, Tuple)),
    true = erts_debug:same(Tuple, simple_tuple_fun_repeated(Tuple, Tuple)),
    true = erts_debug:same(Tuple, simple_tuple_twice_head(Tuple, Tuple)),

    {Tuple1, Tuple2} = simple_tuple_twice_body(Tuple),
    true = erts_debug:same(Tuple, Tuple1),
    true = erts_debug:same(Tuple, Tuple2),

    Nested = {nested,Tuple},
    true = erts_debug:same(Tuple, nested_tuple_part(Nested)),
    true = erts_debug:same(Nested, nested_tuple_whole(Nested)),
    true = erts_debug:same(Nested, nested_tuple_with_alias(Nested)),

    true = erts_debug:same(Tuple, tuple_rebinding_after(Tuple)),

    Tuple = unaliased_tuple_rebinding_before(Tuple),
    false = erts_debug:same(Tuple, unaliased_tuple_rebinding_before(Tuple)),
    Nested = unaliased_literal_tuple_head(Nested),
    false = erts_debug:same(Nested, unaliased_literal_tuple_head(Nested)),
    Nested = unaliased_literal_tuple_body(Nested),
    false = erts_debug:same(Nested, unaliased_literal_tuple_body(Nested)),
    Nested = unaliased_different_var_tuple(Nested, Tuple),
    false = erts_debug:same(Nested, unaliased_different_var_tuple(Nested, Tuple)).

simple_tuple({ok,X}) ->
    {ok,X}.
simple_tuple_twice_head({ok,X}, {ok,X}) ->
    {ok,X}.
simple_tuple_twice_body({ok,X}) ->
    {{ok,X},{ok,X}}.
simple_tuple_in_map(#{hello := {ok,X}}) ->
    {ok,X}.
simple_tuple_fun_repeated({ok,X}, Y) ->
    io:format("~p~n", [X]),
    (fun({ok,X}) -> {ok,X} end)(Y).
simple_tuple_case_repeated({ok,X}, Y) ->
    io:format("~p~n", [X]),
    case Y of {ok,X} -> {ok,X} end.

nested_tuple_part({nested,{ok,X}}) ->
    {ok,X}.
nested_tuple_whole({nested,{ok,X}}) ->
    {nested,{ok,X}}.
nested_tuple_with_alias({nested,{ok,_}=Y}) ->
    {nested,Y}.

tuple_rebinding_after(Y) ->
    (fun(X) -> {ok,X} end)(Y),
    case Y of {ok,X} -> {ok,X} end.
unaliased_tuple_rebinding_before({ok,X}) ->
    io:format("~p~n", [X]),
    (fun(X) -> {ok,X} end)(value).
unaliased_literal_tuple_head({nested,{ok,value}=X}) ->
    io:format("~p~n", [X]),
    {nested,{ok,value}}.
unaliased_literal_tuple_body({nested,{ok,value}=X}) ->
    Res = {nested,Y={ok,value}},
    io:format("~p~n", [[X,Y]]),
    Res.
unaliased_different_var_tuple({nested,{ok,value}=X}, Y) ->
    io:format("~p~n", [X]),
    {nested,Y}.

cons(Config) when is_list(Config) ->
    Cons = [ok|id(value)],

    true = erts_debug:same(Cons, simple_cons(Cons)),
    true = erts_debug:same(Cons, simple_cons_in_map(#{hello => Cons})),
    true = erts_debug:same(Cons, simple_cons_case_repeated(Cons, Cons)),
    true = erts_debug:same(Cons, simple_cons_fun_repeated(Cons, Cons)),
    true = erts_debug:same(Cons, simple_cons_twice_head(Cons, Cons)),

    {Cons1,Cons2} = simple_cons_twice_body(Cons),
    true = erts_debug:same(Cons, Cons1),
    true = erts_debug:same(Cons, Cons2),

    Nested = [nested,Cons],
    true = erts_debug:same(Cons, nested_cons_part(Nested)),
    true = erts_debug:same(Nested, nested_cons_whole(Nested)),
    true = erts_debug:same(Nested, nested_cons_with_alias(Nested)),
    true = erts_debug:same(Cons, cons_rebinding_after(Cons)),

    Unstripped = id([a,b]),
    Stripped = cons_with_binary([<<>>|Unstripped]),
    true = erts_debug:same(Unstripped, Stripped),

    Cons = unaliased_cons_rebinding_before(Cons),
    false = erts_debug:same(Cons, unaliased_cons_rebinding_before(Cons)),
    Nested = unaliased_literal_cons_head(Nested),
    false = erts_debug:same(Nested, unaliased_literal_cons_head(Nested)),
    Nested = unaliased_literal_cons_body(Nested),
    false = erts_debug:same(Nested, unaliased_literal_cons_body(Nested)),
    Nested = unaliased_different_var_cons(Nested, Cons),
    false = erts_debug:same(Nested, unaliased_different_var_cons(Nested, Cons)).

simple_cons([ok|X]) ->
    [ok|X].
simple_cons_twice_head([ok|X], [ok|X]) ->
    [ok|X].
simple_cons_twice_body([ok|X]) ->
    {[ok|X],[ok|X]}.
simple_cons_in_map(#{hello := [ok|X]}) ->
    [ok|X].
simple_cons_fun_repeated([ok|X], Y) ->
    io:format("~p~n", [X]),
    (fun([ok|X]) -> [ok|X] end)(Y).
simple_cons_case_repeated([ok|X], Y) ->
    io:format("~p~n", [X]),
    case Y of [ok|X] -> [ok|X] end.

nested_cons_part([nested,[ok|X]]) ->
    [ok|X].
nested_cons_whole([nested,[ok|X]]) ->
    [nested,[ok|X]].
nested_cons_with_alias([nested,[ok|_]=Y]) ->
    [nested,Y].

cons_with_binary([<<>>,X|Y]) ->
    cons_with_binary([X|Y]);
cons_with_binary(A) ->
    A.

cons_rebinding_after(Y) ->
    (fun(X) -> [ok|X] end)(Y),
    case Y of [ok|X] -> [ok|X] end.
unaliased_cons_rebinding_before([ok|X]) ->
    io:format("~p~n", [X]),
    (fun(X) -> [ok|X] end)(value).
unaliased_literal_cons_head([nested,[ok|value]=X]) ->
    io:format("~p~n", [X]),
    [nested,[ok|value]].
unaliased_literal_cons_body([nested,[ok|value]=X]) ->
    Res = [nested,Y=[ok|value]],
    io:format("~p~n", [[X, Y]]),
    Res.
unaliased_different_var_cons([nested,[ok|value]=X], Y) ->
    io:format("~p~n", [X]),
    [nested,Y].
