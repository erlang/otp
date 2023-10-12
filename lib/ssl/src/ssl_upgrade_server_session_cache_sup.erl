%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2021. All Rights Reserved.
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
-module(ssl_upgrade_server_session_cache_sup).

-behaviour(supervisor).

-include("ssl_internal.hrl").

%% API
-export([start_link/0,
         start_link_dist/0]).
-export([start_child/1]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link() ->
    supervisor:start_link({local, sup_name(normal)}, ?MODULE, []).

start_link_dist() ->
    supervisor:start_link({local, sup_name(dist)}, ?MODULE, []).

start_child(Type) ->
    SupName = sup_name(Type),
    Children = supervisor:count_children(SupName),
    Workers = proplists:get_value(workers, Children),
     case Workers of
         0 ->
             %% In case two upgrade servers are started very close to each other
             %% only one will be able to grab the local name and we will use
             %% that process for handling pre TLS-1.3 sessions for
             %% servers with to us unknown listeners. 
             case supervisor:start_child(SupName, [ssl_unknown_listener, ssl_config:pre_1_3_session_opts(server)]) of
                 {error, {already_started, Child}} ->
                     {ok, Child};
                 {ok, _} = Return ->
                     Return
             end;
         1 ->
             [{_,Child,_, _}] = supervisor:which_children(SupName),
             {ok, Child}
     end.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>   3,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined,
                    start    =>  {ssl_server_session_cache, start_link, []},
                    restart  => transient, 
                    shutdown => 4000,
                    modules  => [ssl_server_session_cache],
                    type     => worker
                   }],     
    {ok, {SupFlags, ChildSpecs}}.

sup_name(normal) ->
    ?MODULE;
sup_name(dist) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_dist").
