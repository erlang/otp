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
-module(c_sup).


-behaviour(supervisor).

%% External exports
-export([start/2]).

%% Internal exports
-export([init/1]).

start(_, _) ->
    supervisor:start_link({local, c_sup}, c_sup, []).

init([]) ->
    SupFlags = {one_for_one, 4, 3600},
    Config1 = {c,
	       {aa, start_link, []},
	       permanent, 2000, worker, [aa]},
    Config2 = {b,
	       {b, start_link, []},
	       permanent, 2000, worker, [b]},
    {ok, {SupFlags, [Config1, Config2]}}.
