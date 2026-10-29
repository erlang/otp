%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2026. All Rights Reserved.
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

-module(dialyzer_utils_SUITE).

-behaviour(ct_suite).

-export([all/0,
         p_map_implements_map/1,
         p_map_handles_errors_like_map_does/1,
         p_map_preserves_ordering/1
         ]).

all() ->
    [p_map_implements_map,
     p_map_handles_errors_like_map_does,
     p_map_preserves_ordering
    ].

p_map_implements_map(_Config) ->
    Fun = fun (N) -> N + 2 end,
    List = [2,1,3,2],
    Expected = lists:map(Fun, List),
    Expected = dialyzer_utils:p_map(Fun, List).

p_map_handles_errors_like_map_does(_Config) ->
    Fun = fun (3) -> throw("an error"); (N) -> N + 2 end,
    List = [2,1,3,2],
    ListsOk =
      try
        lists:map(Fun, List),
        false
      catch _:_ ->
        true
      end,
    case ListsOk of
      true -> ok;
      false -> ct:fail("Expected lists:map/2 to throw")
    end,
    UtilsOk =
      try
        dialyzer_utils:p_map(Fun, List),
        false
      catch _:_ ->
        true
      end,
    case UtilsOk of
      true -> ok;
      false -> ct:fail("Expected dialyzer_utils:p_map/2 to throw")
    end.

p_map_preserves_ordering(_Config) ->
    Fun = fun (N) -> timer:sleep(N * 50), N + 2 end,
    List = [2,1,3,2,1,5,2,1,3,2,9,4,2,1,3,2,5,4],
    Expected = lists:map(Fun, List),
    Expected = dialyzer_utils:p_map(Fun, List).
