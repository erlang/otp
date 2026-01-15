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
-module(observer_backend_SUITE).
-include_lib("runtime_tools/include/observer_backend.hrl").

%% Test server specific exports
-export([all/0, suite/0]).

%% Test cases
-export([procs_info/1, etop_collect/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [procs_info, etop_collect].


procs_info(_Config) ->
    [spawn_link(fun() -> timer:sleep(5000) end) || _ <- lists:seq(1, 20000)],
    observer_backend:procs_info(self()),
    10000 = get_chunk_length(),
    10000 = get_chunk_length(),
    ChunkLength = get_chunk_length(),
    true = (ChunkLength > 0).

etop_collect(_Config) ->
    [spawn_link(fun() -> timer:sleep(5000) end) || _ <- lists:seq(1, 20000)],
    ok = observer_backend:etop_collect(self()),
    EtopLength = get_etop_length(),
    true = (EtopLength > 20000).

get_chunk_length() ->
    Self = self(),
    receive
        {procs_info, Self, ProcInfo} ->
            length(ProcInfo)
    after 0 ->
        ct:fail("Did not receive message in time.")
    end.

get_etop_length() ->
    Self = self(),
    receive
        {Self, #etop_info{n_procs = NProcs}} ->
            NProcs
    after 0 ->
        ct:fail("Did not receive message in time.")
    end.
