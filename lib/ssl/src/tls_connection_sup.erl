%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Supervisor for a SSL/TLS connection
%%----------------------------------------------------------------------
-module(tls_connection_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/0, start_link_dist/0]).
-export([start_child/1, start_child_dist/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_dist() ->
    supervisor:start_link({local, tls_dist_connection_sup}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

start_child_dist(Args) ->
    supervisor:start_child(tls_dist_connection_sup, Args).
    
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>   0,
                 period    => 3600
                },
    ChildSpecs = [#{id      => undefined,
                    restart => temporary,
                    type    => supervisor,
                    start   => {tls_dyn_connection_sup, start_link, []}}],
    {ok, {SupFlags, ChildSpecs}}.


