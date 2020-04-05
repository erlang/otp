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

-module(ssh_peername_sockname_server).

%% The purpose of this module is to perform tests on the server side of an
%% ssh connection.


-behaviour(ssh_server_channel).
-record(state, {}).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

init([]) ->
    {ok, #state{}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    [{peer, {_Name, Peer}}] = ssh:connection_info(ConnectionManager, [peer]),
    [{sockname, Sock}] = ssh:connection_info(ConnectionManager, [sockname]),
    ssh_connection:send(ConnectionManager, ChannelId,
			term_to_binary({Peer, Sock})),
    {ok, State}.

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}},
	       State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _CM, _}, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
