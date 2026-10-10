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

-module(gh_7478_a).
-export([?MODULE/0, gh_7478_a/1]).

?MODULE() ->
    ok = try gh_7478_a(0) catch _:_ -> ok end,
    ok = try gh_7478_a([]) catch _:_ -> ok end,
    ok = try gh_7478_a(<<>>) catch _:_ -> ok end,
    ok = try gh_7478_a(a) catch _:_ -> ok end,
    ok.

gh_7478_a(A) ->
    [ 0 ||
        begin
            _ = bit_size(maybe
                             [] ?= maybe
                                       0 ?= A,
                                       << 0 || _ <- []>>,
                                       ok
                                   end,
                             A
                         else
                             A -> A;
                             ok -> A
                         end),
            A end,
        _ <- ok].
