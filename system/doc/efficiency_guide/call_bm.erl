%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(call_bm).

-include("bench.hrl").

-export([benchmarks/0]).
-export([local_call/1,external_call/1,fun_call/1,apply_fun/1,
	 apply_mfa_implicit/1, apply_mfa_explicit/1]).
-export([foo/0]).

benchmarks() ->
    {400000,[local_call,external_call,fun_call,apply_fun,
	     apply_mfa_implicit, apply_mfa_explicit]}.

local_call(0) ->
    ok;
local_call(Iter) ->
    ?rep40(foo()),
    local_call(Iter-1).

external_call(0) ->
    ok;
external_call(Iter) ->
    ?rep40(?MODULE:foo()),
    external_call(Iter-1).

fun_call(Iter) ->
    fun_call(Iter, fun() -> ok end).
fun_call(0, _) ->
    ok;
fun_call(Iter, Fun) ->
    ?rep40(Fun()),
    fun_call(Iter-1, Fun).

apply_fun(Iter) ->
    apply_fun(Iter, fun() -> ok end).
apply_fun(0, _) ->
    ok;
apply_fun(Iter, Fun) ->
    ?rep40(apply(Fun, [])),
    apply_fun(Iter-1, Fun).

apply_mfa_explicit(0) ->
    ok;
apply_mfa_explicit(Iter) ->
    ?rep40(apply(?MODULE, foo, [])),
    apply_mfa_explicit(Iter-1).

apply_mfa_implicit(Iter) ->
    apply_mfa_implicit(?MODULE, foo, Iter).

apply_mfa_implicit(_, _, 0) ->
    ok;
apply_mfa_implicit(Module, Function, Iter) ->
    ?rep40(Module:Function()),
    apply_mfa_implicit(Module, Function, Iter-1).

foo() -> ok.
