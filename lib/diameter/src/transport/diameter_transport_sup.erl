%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

%%
%% Top supervisor for transport processes.
%%

-module(diameter_transport_sup).

-behaviour(supervisor).

-export([start_link/0,   %% supervisor start
         start_child/2]).

%% supervisor callback
-export([init/1]).

%% ---------------------------------------------------------------------------

%% start_link/0
%%
%% Start the transport top supervisor. This is started as a child at
%% at application start, from diameter_sup.erl. Protocol-specific
%% supervisors are started as children of this supervisor dynamically
%% by calling start_child/2. (Eg. diameter_tcp_sup:start/0, which
%% is called from diameter_tcp:start/3 to start supervisors the
%% first time a TCP transport process is started.)

start_link() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

%% start_child/2
%%
%% Start a protocol-specific supervisor under the top supervisor.

start_child(Name, Module) ->
    Spec = {Name,
            {Module, start_link, [Name]},
            permanent,
            infinity,
            supervisor,
            [Module]},
    supervisor:start_child(?MODULE, Spec).

%% ---------------------------------------------------------------------------

%% Top supervisor callback.
init([]) ->
    Flags   = {one_for_one, 0, 1},
    Workers = [],  %% Each protocol starts its supervisor on demand.
    {ok, {Flags, Workers}}.
