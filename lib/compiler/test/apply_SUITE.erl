%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(apply_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,mfa/1,fun_apply/1]).

-export([foo/0,bar/1,baz/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [mfa, fun_apply].

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


-define(APPLY0(M, F), (fun(Res) -> Res = M:F() end)(apply(M, F, []))).
-define(APPLY1(M, F, A1), (fun(Res) -> Res = M:F(A1) end)(apply(M, F, [A1]))).
-define(APPLY2(M, F, A1, A2), (fun(Res) -> Res = M:F(A1, A2) end)(apply(M, F, [A1,A2]))).

mfa(Config) when is_list(Config) ->
    ?line ok = ?APPLY0(?MODULE, foo),
    ?line {[a,b]} = ?APPLY1(?MODULE, bar, [a,b]),
    ?line {39,{a}} = ?APPLY2(?MODULE, baz, 39, {a}),

    ?line Mod = id(?MODULE),
    ?line ok = ?APPLY0(Mod, foo),
    ?line {[a,b]} = ?APPLY1(Mod, bar, [a,b]),
    ?line {39,{a}} = ?APPLY2(Mod, baz, 39, {a}),

    ?line ok = ?APPLY0(?MODULE, (id(foo))),
    ?line {[a,b]} = ?APPLY1(?MODULE, (id(bar)), [a,b]),
    ?line {39,{a}} = ?APPLY2(?MODULE, (id(baz)), 39, {a}),

    ?line ok = ?APPLY0(Mod, (id(foo))),
    ?line {[a,b]} = ?APPLY1(Mod, (id(bar)), [a,b]),
    ?line {39,{a}} = ?APPLY2(Mod, (id(baz)), 39, {a}),

    ?line {'EXIT',_} = (catch ?APPLY2(Mod, (id(bazzzzzz)), a, b)),
    ?line {'EXIT',_} = (catch ?APPLY2({}, baz, a, b)),
    ?line {'EXIT',_} = (catch ?APPLY2(?MODULE, [], a, b)),

    ?line ok = apply(Mod, foo, id([])),
    ?line {[a,b|c]} = apply(Mod, bar, id([[a,b|c]])),
    ?line {[xx],{a}} = apply(?MODULE, baz, id([[xx],{a}])),

    ?line Erlang = id(erlang),
    ?line Self = self(),
    ?line Self = ?APPLY0(Erlang, self),
    ?line 42.0 = ?APPLY1(Erlang, abs, -42.0),
    ?line b = ?APPLY2(Erlang, element, 2, {a,b,c}),
    ?line true = ?APPLY1(Erlang, is_function, fun erlang:list_to_binary/1),
    ?line true = ?APPLY1(Erlang, is_function, fun() -> ok end),
    ?line false = ?APPLY1(Erlang, is_function, blurf),
    ?line true = ?APPLY2(Erlang, is_function, fun erlang:list_to_binary/1, 1),
    ?line true = ?APPLY2(Erlang, is_function, fun() -> ok end, 0),
    ?line false = ?APPLY2(Erlang, is_function, blurf, 0),

    ?line apply(Mod, foo, []).

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
    ?line Self = self(),

    ?line Self = ?FUNAPPLY0(fun() -> self() end),
    ?line Self = ?FUNAPPLY0((id(fun() -> self() end))),
    ?line ok = ?FUNAPPLY0(fun ?MODULE:foo/0),
    ?line ok = ?FUNAPPLY0((id(fun ?MODULE:foo/0))),

    ?line -42 = ?FUNAPPLY1(fun(A) -> -A end, 42),
    ?line [x,yy] = ?FUNAPPLY1((id(fun(T) -> [x|T] end)), [yy]),
    ?line {[a|b]} = ?FUNAPPLY1(fun ?MODULE:bar/1, [a|b]),
    ?line {[a|b]} = ?FUNAPPLY1((id(fun ?MODULE:bar/1)), [a|b]),

    ?line {a,b} = ?FUNAPPLY2(fun(A, B) -> {A,B} end, a, b),
    ?line {a,[b]} = ?FUNAPPLY2((id(fun(A, B) -> {A,B} end)), a, [b]),
    ?line {42,{a}} = ?FUNAPPLY2((id(fun ?MODULE:baz/2)), 42, {a}),

    ok.

id(I) -> I.
