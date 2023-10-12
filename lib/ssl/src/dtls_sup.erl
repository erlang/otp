%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2021. All Rights Reserved.
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

-module(dtls_sup).

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
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>   10,
                 period    => 3600
                },
    Children = [dtls_connection_child_spec(), server_instance_child_spec()],    
    {ok, {SupFlags, Children}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
dtls_connection_child_spec() ->
    #{id       => dtls_connection_sup,
      start    => {dtls_connection_sup, start_link, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [dtls_connection_sup],
      type     => supervisor
     }.

server_instance_child_spec() ->
    #{id       => dtls_server_sup,
      start    => {dtls_server_sup, start_link, []},
      restart  => permanent, 
      shutdown => 4000,
      modules  => [dtls_server_sup],
      type     => supervisor
     }.
