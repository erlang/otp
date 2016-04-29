%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(disk_log_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local, disk_log_sup}, disk_log_sup, []).

init([]) ->
    SupFlags = {simple_one_for_one, 4, 3600},
    Child = {disk_log, {disk_log, istart_link, []}, temporary,
	     1000, worker, [disk_log]},
    {ok, {SupFlags, [Child]}}.
