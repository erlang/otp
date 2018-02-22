%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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
%% Purpose: Ssh connection supervisor.
%%----------------------------------------------------------------------

-module(ssh_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_child/2]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

start_child(Sup, Args) ->
    supervisor:start_child(Sup, Args).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_) ->
    SupFlags = #{strategy  => simple_one_for_one, 
                 intensity =>    0,
                 period    => 3600
                },
    ChildSpecs = [#{id       => undefined, % As simple_one_for_one is used.
                    start    => {ssh_connection_handler, start_link, []},
                    restart  => temporary % because there is no way to restart a crashed connection
                   }
                 ],
    {ok, {SupFlags,ChildSpecs}}.
