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
-module(ssh_channel_sup).

-behaviour(supervisor).
-include("ssh.hrl").

-export([start_link/1, start_child/8]).

%% Supervisor callback
-export([init/1]).

%%%=========================================================================
%%%  Internal API
%%%=========================================================================
start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).


start_child(client, ChannelSup, ConnRef, Callback, Id, Args, Exec, _Opts) when is_pid(ConnRef) ->
    start_the_child(ssh_client_channel, ChannelSup, ConnRef, Callback, Id, Args, Exec);
start_child(server, ChannelSup, ConnRef, Callback, Id, Args, Exec, Opts) when is_pid(ConnRef) ->
     case max_num_channels_not_exceeded(ChannelSup, Opts) of
        true ->
             start_the_child(ssh_server_channel, ChannelSup, ConnRef, Callback, Id, Args, Exec);
        false ->
             throw(max_num_channels_exceeded)
    end.


start_the_child(ChanMod, ChannelSup, ConnRef, Callback, Id, Args, Exec) ->
    ChildSpec =
        #{id       => make_ref(),
          start    => {ChanMod, start_link, [ConnRef, Id, Callback, Args, Exec]},
          restart  => temporary,
          type     => worker,
          modules  => [ChanMod]
         },
    case supervisor:start_child(ChannelSup, ChildSpec) of
        {error,{Error,_Info}} ->
            throw(Error);
        Others ->
            Others
    end.

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
max_num_channels_not_exceeded(ChannelSup, Opts) ->
    MaxNumChannels = ?GET_OPT(max_channels, Opts),
    NumChannels = length([x || {_,_,worker,[ssh_server_channel]} <- 
				   supervisor:which_children(ChannelSup)]),
    %% Note that NumChannels is BEFORE starting a new one
    NumChannels < MaxNumChannels.
