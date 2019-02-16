%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
-module(apply_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,mfa/1,fun_apply/1]).

-export([foo/0,bar/1,baz/2]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [mfa, fun_apply].

groups() -> 
    [].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


-define(APPLY0(M, F), (fun(Res) -> Res = M:F() end)(apply(M, F, []))).
-define(APPLY1(M, F, A1), (fun(Res) -> Res = M:F(A1) end)(apply(M, F, [A1]))).
-define(APPLY2(M, F, A1, A2), (fun(Res) -> Res = M:F(A1, A2) end)(apply(M, F, [A1,A2]))).

mfa(Config) when is_list(Config) ->
    ok = ?APPLY0(?MODULE, foo),
    {[a,b]} = ?APPLY1(?MODULE, bar, [a,b]),
    {39,{a}} = ?APPLY2(?MODULE, baz, 39, {a}),

    Mod = id(?MODULE),
    ok = ?APPLY0(Mod, foo),
    {[a,b]} = ?APPLY1(Mod, bar, [a,b]),
    {39,{a}} = ?APPLY2(Mod, baz, 39, {a}),

    ok = ?APPLY0(?MODULE, (id(foo))),
    {[a,b]} = ?APPLY1(?MODULE, (id(bar)), [a,b]),
    {39,{a}} = ?APPLY2(?MODULE, (id(baz)), 39, {a}),

    ok = ?APPLY0(Mod, (id(foo))),
    {[a,b]} = ?APPLY1(Mod, (id(bar)), [a,b]),
    {39,{a}} = ?APPLY2(Mod, (id(baz)), 39, {a}),

    {'EXIT',_} = (catch ?APPLY2(Mod, (id(bazzzzzz)), a, b)),
    {'EXIT',_} = (catch ?APPLY2({}, baz, a, b)),
    {'EXIT',_} = (catch ?APPLY2(?MODULE, [], a, b)),
    {'EXIT',_} = (catch bad_literal_call(1)),

    ok = apply(Mod, foo, id([])),
    {[a,b|c]} = apply(Mod, bar, id([[a,b|c]])),
    {[xx],{a}} = apply(?MODULE, baz, id([[xx],{a}])),

    Erlang = id(erlang),
    Self = self(),
    Self = ?APPLY0(Erlang, self),
    42.0 = ?APPLY1(Erlang, abs, -42.0),
    b = ?APPLY2(Erlang, element, 2, {a,b,c}),
    true = ?APPLY1(Erlang, is_function, fun erlang:list_to_binary/1),
    true = ?APPLY1(Erlang, is_function, fun() -> ok end),
    false = ?APPLY1(Erlang, is_function, blurf),
    true = ?APPLY2(Erlang, is_function, fun erlang:list_to_binary/1, 1),
    true = ?APPLY2(Erlang, is_function, fun() -> ok end, 0),
    false = ?APPLY2(Erlang, is_function, blurf, 0),

    apply(Mod, foo, []).

%% The single call to this function with a literal argument caused type
%% optimization to swap out the 'mod' field of a #b_remote{}, which was
%% mishandled during code generation as it assumed that the module would always
%% be an atom.
bad_literal_call(I) ->
    I:foo().

foo() ->
    ok.

bar(A) ->
    {A}.

baz(A, B) ->
    {A,B}.

-define(FUNAPPLY0(F), (fun(Res) -> Res = F() end)(apply(F, []))).
-define(FUNAPPLY1(F, A1), (fun(Res) -> Res = F(A1) end)(apply(F, [A1]))).
-define(FUNAPPLY2(F, A1, A2), (fun(Res) -> Res = F(A1, A2) end)(apply(F, [A1,A2]))).

fun_apply(Config) when is_list(Config) ->
    Self = self(),

    Self = ?FUNAPPLY0(fun() -> self() end),
    Self = ?FUNAPPLY0((id(fun() -> self() end))),
    ok = ?FUNAPPLY0(fun ?MODULE:foo/0),
    ok = ?FUNAPPLY0((id(fun ?MODULE:foo/0))),

    -42 = ?FUNAPPLY1(fun(A) -> -A end, 42),
    [x,yy] = ?FUNAPPLY1((id(fun(T) -> [x|T] end)), [yy]),
    {[a|b]} = ?FUNAPPLY1(fun ?MODULE:bar/1, [a|b]),
    {[a|b]} = ?FUNAPPLY1((id(fun ?MODULE:bar/1)), [a|b]),

    {a,b} = ?FUNAPPLY2(fun(A, B) -> {A,B} end, a, b),
    {a,[b]} = ?FUNAPPLY2((id(fun(A, B) -> {A,B} end)), a, [b]),
    {42,{a}} = ?FUNAPPLY2((id(fun ?MODULE:baz/2)), 42, {a}),

    ok.

id(I) -> I.
