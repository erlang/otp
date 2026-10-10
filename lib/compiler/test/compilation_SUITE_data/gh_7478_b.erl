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

-module(gh_7478_b).
-export([?MODULE/0, gh_7478_b/1]).

?MODULE() ->
    <<>> = gh_7478_b([]),
    <<>> = gh_7478_b([1]),
    ok = try gh_7478_b(<<>>) catch _:_ -> ok end,
    ok = try gh_7478_b(a) catch _:_ -> ok end,
    ok.

gh_7478_b(L) ->
    <<0 || erlang:yield() and ([0 || _ <- L] =< 0),
           <<>> <= ok >>.
