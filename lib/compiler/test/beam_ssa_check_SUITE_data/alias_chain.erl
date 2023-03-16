%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
%% This module tests that beam_ssa_alias_opt:opt/2 does not annotate
%% instructions with information about aliased operands when more than
%% MAX_REPETITIONS are required for the analysis to converge.
%%
%% A specially crafted module which is designed to hit the alias
%% analysis iteration limit if functions are visited naïvely in the
%% order they occur in the module.
%%
-compile(no_ssa_opt_destructive_update).

-module(alias_chain).

-export([top/1]).

top(L) ->
%ssa% (A) when post_ssa_opt ->
%ssa% _ = call(...) { aliased => [A] }.
    fF(L, <<>>).

fF([H|T], AccA) ->
    fE(T, <<AccA/binary, H:8>>);
fF([], Acc) ->
    Acc.

fE([H|T], AccA) ->
    fD(T, <<AccA/binary, H:8>>);
fE([], Acc) ->
    Acc.

fD([H|T], AccA) ->
    fC(T, <<AccA/binary, H:8>>);
fD([], Acc) ->
    Acc.

fC([H|T], AccA) ->
    fB(T, <<AccA/binary, H:8>>);
fC([], Acc) ->
    Acc.

fB([H|T], AccA) ->
    fA(T, <<AccA/binary, H:8>>);
fB([], Acc) ->
    Acc.

fA([H|T], AccA) ->
    f9(T, <<AccA/binary, H:8>>);
fA([], Acc) ->
    Acc.

f9([H|T], AccA) ->
    f8(T, <<AccA/binary, H:8>>);
f9([], Acc) ->
    Acc.

f8([H|T], AccA) ->
    f7(T, <<AccA/binary, H:8>>);
f8([], Acc) ->
    Acc.

f7([H|T], AccA) ->
    f6(T, <<AccA/binary, H:8>>);
f7([], Acc) ->
    Acc.

f6([H|T], AccA) ->
    f5(T, <<AccA/binary, H:8>>);
f6([], Acc) ->
    Acc.

f5([H|T], AccA) ->
    f4(T, <<AccA/binary, H:8>>);
f5([], Acc) ->
    Acc.

f4([H|T], AccA) ->
    f3(T, <<AccA/binary, H:8>>);
f4([], Acc) ->
    Acc.

f3([H|T], AccA) ->
    f2(T, <<AccA/binary, H:8>>);
f3([], Acc) ->
    Acc.

f2([H|T], AccA) ->
    f1(T, <<AccA/binary, H:8>>);
f2([], Acc) ->
    Acc.

f1([H|T], AccA) ->
    f0(T, <<AccA/binary, H:8>>);
f1([], Acc) ->
    Acc.

f0([H|T], AccA) ->
    f3(T, <<AccA/binary, H:8>>);
f0([], Acc) ->
    Acc.

