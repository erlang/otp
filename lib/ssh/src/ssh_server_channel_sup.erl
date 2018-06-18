%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
%% Purpose: Ssh channel supervisor.
%%----------------------------------------------------------------------
-module(ssh_server_channel_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/5]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  Internal API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

start_child(Sup, Callback, Id, Args, Exec) ->
    ChildSpec =
        #{id       => make_ref(),
          start    => {ssh_server_channel, start_link, [self(), Id, Callback, Args, Exec]},
          restart  => temporary,
          type     => worker,
          modules  => [ssh_server_channel]
         },
    supervisor:start_child(Sup, ChildSpec).

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_Args) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
