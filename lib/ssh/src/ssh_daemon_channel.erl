%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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
%% Description: a gen_server implementing a simple
%% terminal (using the group module) for a CLI
%% over SSH

-module(ssh_daemon_channel).

%% API to special server side channel that can be pluged into the erlang ssh daemeon
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::integer(), State::term()}. 
-callback handle_ssh_msg({ssh_cm, ConnectionRef::term(), SshMsg::term()},
			 State::term()) -> {ok, State::term()} |
					   {stop, ChannelId::integer(),
					    State::term()}.

%%% API
-export([start/4, start/5, start_link/4, start_link/5, enter_loop/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

enter_loop(State) ->
    ssh_channel:enter_loop(State).

init(Args) ->
    ssh_channel:init(Args).
terminate(Reason, State) ->
    ssh_channel:terminate(Reason, State).
