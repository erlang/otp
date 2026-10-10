%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2026. All Rights Reserved.
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
-module(ssh_channel_sup).
-moduledoc false.

-behaviour(supervisor).

-export([start_link/1, start_child/7]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  Internal API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).


start_child(client, ChannelSup, ConnRef, Callback, Id, Args, Exec) when is_pid(ConnRef) ->
    start_the_channel(ssh_client_channel, ChannelSup, ConnRef, Callback, Id, Args, Exec);

start_child(server, ChannelSup, ConnRef, Callback, Id, Args, Exec) when is_pid(ConnRef) ->
    start_the_channel(ssh_server_channel, ChannelSup, ConnRef, Callback, Id, Args, Exec).


%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init(_Args) ->
    ssh_lib:set_label(channel_sup),
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = [],
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================
start_the_channel(ChanMod, ChannelSup, ConnRef, Callback, Id, Args, Exec) ->
    ChildSpec =
        #{id       => make_ref(),
          start    => {ChanMod, start_link, [ConnRef, Id, Callback, Args, Exec]},
          restart  => temporary,
          type     => worker,
          modules  => [ChanMod]
         },
    case supervisor:start_child(ChannelSup, ChildSpec) of
        {ok, Pid} ->              {ok, Pid};
        {ok, Pid, _Info} ->       {ok, Pid};
        {error, {Error,_Info}} -> {error, Error};
        {error, Error} ->         {error, Error}
    end.

