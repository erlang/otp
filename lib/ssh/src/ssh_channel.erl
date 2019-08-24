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

-module(ssh_channel).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::ssh:channel_id(), State::term()}. 

-callback handle_ssh_msg({ssh_cm, ConnectionRef::ssh:connection_ref(), SshMsg::term()}, 
 			 State::term()) -> {ok, State::term()} | 
 					   {stop, ChannelId::ssh:channel_id(), 
 					    State::term()}.
%%% API
-export([start/4, start/5, start_link/4, start_link/5, call/2, call/3,
         init/1,
	 cast/2, reply/2, enter_loop/1]).

%%====================================================================
%% API
%%====================================================================

call(ChannelPid, Msg) ->
    ssh_client_channel:call(ChannelPid, Msg).

call(ChannelPid, Msg, TimeOute) ->
    ssh_client_channel:call(ChannelPid, Msg, TimeOute).

cast(ChannelPid, Msg) ->
    ssh_client_channel:cast(ChannelPid, Msg).

reply(From, Msg) ->
    ssh_client_channel:reply(From, Msg).

init(Args) ->
    ssh_client_channel:init(Args).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_client_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_client_channel:start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    ssh_client_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    ssh_client_channel:start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec).

enter_loop(State) ->
    ssh_client_channel:enter_loop(State).
