%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(archive_script_dummy_sup).
-behaviour(supervisor).

%% Public
-export([start_link/1]).

%% Internal
-export([init/1]).

start_link(Debug) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Debug]).

init([Debug]) ->
    Flags = {one_for_one, 0, 3600},
    {ok, {Flags, []}}.
