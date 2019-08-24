%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id$
%%
-module(a_sup).


-behaviour(supervisor).

%% External exports
-export([start/2]).

%% Internal exports
-export([init/1]).

start(_, _) ->
    supervisor:start_link({local, a_sup}, a_sup, []).

init([]) ->
    SupFlags = {one_for_all, 4, 3600},
    Config = {a,
	      {a, start_link, []},
	      permanent, brutal_kill, worker, [a]},
    {ok, {SupFlags, [Config]}}.
