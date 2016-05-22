%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(group_leader_sup).

-behaviour(supervisor).

%% External exports
-export([start/2]).

%% Internal exports
-export([init/1]).

start(_, _) ->
    supervisor:start_link(group_leader_sup, []).

init([]) ->
    SupFlags = {one_for_one,4,3600},
    Config = {group_leader,
	      {group_leader,start_link,[]},
	      temporary,4000,worker,[group_leader]},
    {ok,{SupFlags,[Config]}}.
