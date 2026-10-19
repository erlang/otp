%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(is_integer_3).

-export([no_lower/1, no_upper/1, no_bounds/1, guard/1,
         open_bounds/0, bad_lower/1, bad_upper/1]).

no_lower(X) ->
    is_integer(X, undefined, 10).

no_upper(X) ->
    is_integer(X, 1, undefined).

no_bounds(X) ->
    is_integer(X, undefined, undefined).

guard(X) when is_integer(X, undefined, 10) ->
    lower;
guard(X) when is_integer(X, 1, undefined) ->
    upper;
guard(_) ->
    other.

open_bounds() ->
    {in_range(5, undefined, 10),
     in_range(5, 1, undefined),
     in_range(5, undefined, undefined)}.

in_range(X, LB, UB) ->
    is_integer(X, LB, UB).

bad_lower(X) ->
    is_integer(X, foo, 10).

bad_upper(X) ->
    is_integer(X, 1, 10.0).
