%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

-module(opaque_bug7).

-export([loop/1]).
-export_type([unused_dummy/0]).

-type other_type() :: opaque_bug7_adt:adt().

%% Required for decorate/3 to be called, jarring the bug loose. Does not need
%% to be used anywhere.
-opaque unused_dummy() :: {pid(), binary()}.

-spec loop(other_type()) -> no_return().
loop(OtherType) ->
    receive
        _X ->
            loop(OtherType)
    after timer:minutes(30) ->
            opaque_bug7_adt:do(OtherType)
    end.
