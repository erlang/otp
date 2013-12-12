%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%

-module(ssh_peername_sockname_server).

%% The purpose of this module is to perform tests on the server side of an
%% ssh connection.


-behaviour(ssh_daemon_channel).
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
