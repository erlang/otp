%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

-module(test_setelement).
-export([?MODULE/0, id/1]).

?MODULE() ->
    State = id({state, a, b, c, d, e, f, g}),
    {state, a, b, c, r5, r6, r7, r8} = setelement_1(State),

    {no_state, a, b, c, r5a, r6, r7, r8a} = setelement_2(a, State),
    {no_state, a, b, c, r5b, r6, r7, r8b} = setelement_2(b, State),
    {no_state, a, b, c, d, r6c, r7, g} = setelement_2(c, State),

    {no_state, a, b, c, r5a, e, r7, g} = setelement_3(a, State),
    {no_state, a, b, c, r5b, e, r7, g} = setelement_3(b, State),

    {{state, r2, b, c, d, e, f, g},
     {state, a, b, c, d, r6, r7, g},
     {state, a, r3, c, r5, r6, f, g}} = setelement_4(State),

    {state, r2, b, c, d, r6new, r7, g} = setelement_5(State),

    ok.

setelement_1(State0) ->
    State1 = setelement(6, State0, r6),
    State2 = setelement(8, State1, r8),
    State3 = setelement(5, State2, r5),
    setelement(7, State3, r7).

setelement_2(Branch, State0) ->
    State1 = setelement(6, State0, r6),
    State3 = case Branch of
                 a ->
                     State2 = setelement(8, State1, r8a),
                     setelement(5, State2, r5a);
                 b ->
                     State2 = setelement(8, State1, r8b),
                     setelement(5, State2, r5b);
                 c ->
                     State2 = setelement(6, State0, r6c),
                     setelement(7, State2, r7c)
             end,
    State4 = setelement(7, State3, r7),
    setelement(1, State4, no_state).

setelement_3(Branch, State0) ->
    State1 = case Branch of
                 a ->
                     setelement(5, State0, r5a);
                 b ->
                     setelement(5, State0, r5b)
             end,
    State2 = setelement(7, State1, r7),
    setelement(1, State2, no_state).

setelement_4(State0) ->
    State1 = setelement(6, State0, r6),
    State2 = setelement(5, State1, r5),
    {setelement(2, State0, r2),
     setelement(7, State1, r7),
     setelement(3, State2, r3)}.

setelement_5(State0) ->
    State1 = setelement(6, State0, r6),
    State2 = setelement(7, State1, r7),
    State3 = setelement(6, State2, r6new),
    setelement(2, State3, r2).

id(I) ->
    I.
