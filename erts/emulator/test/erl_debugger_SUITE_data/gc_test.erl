-module(gc_test).
-export([go/1]).

%% NB. If N is high enough, we will eventually need to do a GC to
%% grow the stack. Since we will set a breakpoint before the
%% recursive call, we will be forced to the GC while processing
%% the breakpoint
go({max_recursion_depth, 0}) ->
    1;
go({max_recursion_depth, N}) ->
    N_1 = N-1,
    Acc = ?MODULE:go({max_recursion_depth, N_1}),
    Acc + 1.

%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
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
