%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2014-2024. All Rights Reserved.
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
%% Purpose: Supervisor for a listen options tracker
%%----------------------------------------------------------------------
-module(tls_server_session_ticket_sup).
-moduledoc false.

-behaviour(supervisor).

%% API
-export([start_link/0, 
         start_link_dist/0]).
-export([start_child/1, 
         start_child_dist/1]).

%% Supervisor callback
-export([init/1,
        sup_name/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, sup_name(normal)}, ?MODULE, []).

start_link_dist() ->
    supervisor:start_link({local, sup_name(dist)}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(sup_name(normal), Args).

start_child_dist(Args) ->
    supervisor:start_child(sup_name(dist), Args).

sup_name(normal) ->
    ?MODULE;
sup_name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_dist").
  
%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>   0,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined,
                    start    => {tls_server_session_ticket, start_link, []},
                    restart  => transient, 
                    shutdown => 4000,
                    modules  => [tls_server_session_ticket],
                    type     => worker
                   }], 
    {ok, {SupFlags, ChildSpecs}}.
