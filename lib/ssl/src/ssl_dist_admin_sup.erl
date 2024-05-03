%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2024. All Rights Reserved.
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

-module(ssl_dist_admin_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
			
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================

init([]) ->    
    ChildSpecs = [pem_cache_child_spec(), 
                  session_and_cert_manager_child_spec()], 
    SupFlags = #{strategy  => rest_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    {ok, {SupFlags, ChildSpecs}}.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

pem_cache_child_spec() ->
    #{id        => ssl_pem_cache_dist,
      start     => {ssl_pem_cache, start_link_dist, [[]]},
      restart   => permanent, 
      shutdown  => 4000,
      modules   => [ssl_pem_cache],
      type      => worker
     }.
session_and_cert_manager_child_spec() ->
    Opts = ssl_admin_sup:manager_opts(),
    #{id       => ssl_dist_manager,
      start    => {ssl_manager, start_link_dist, [Opts]},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [ssl_manager],
      type     => worker
     }.
