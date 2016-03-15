%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(dets_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-spec start_link() -> {'ok', pid()} | 'ignore' | {'error', term()}.

start_link() ->
    supervisor:start_link({local, dets_sup}, dets_sup, []).

-spec init([]) ->
        {'ok', {{'simple_one_for_one', 4, 3600},
		[{'dets', {'dets', 'istart_link', []},
		  'temporary', 30000, 'worker', ['dets']}]}}.

init([]) ->
    SupFlags = {simple_one_for_one, 4, 3600},
    Child = {dets, {dets, istart_link, []}, temporary, 30000, worker, [dets]},
    {ok, {SupFlags, [Child]}}.
