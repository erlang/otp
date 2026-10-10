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

-module(gh_7478_c).
-export([?MODULE/0, gh_7478_c/1]).

?MODULE() ->
    ok = try gh_7478_c(false) catch error:{bad_generator,ok} -> ok end,
    ok = try gh_7478_c(true) catch error:{bad_generator,ok} -> ok end,
    ok = try gh_7478_c([]) catch error:{bad_generator,ok} -> ok end,
    ok.

gh_7478_c(A) ->
    <<0 || try
               [ 0 || _ := _ <- ok]
           catch
               _ ->
                   false
           end and A,
           _ <- ok
    >>.
