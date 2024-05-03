%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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

-module(ssh_tcpip_forward_srv).
-moduledoc false.

-behaviour(ssh_server_channel).

-record(state, {
          id, cm,
          fwd_socket
	 }).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

init([FwdSocket]) ->
    {ok, #state{fwd_socket=FwdSocket}}.


handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok, State#state{id = ChannelId,
		     cm = ConnectionManager}};

handle_msg({tcp,Sock,Data}, #state{fwd_socket = Sock,
                                   cm = CM,
                                   id = ChId} = State) ->
    ssh_connection:send(CM, ChId, Data),
    inet:setopts(Sock, [{active,once}]),
    {ok, State};

handle_msg({tcp_closed,Sock}, #state{fwd_socket = Sock,
                                     cm = CM,
                                     id = ChId} = State) ->
    ssh_connection:send_eof(CM, ChId),
    {stop, ChId, State#state{fwd_socket=undefined}}.



handle_ssh_msg({ssh_cm, _CM, {data, _ChannelId, _Type, Data}}, #state{fwd_socket=Sock} = State) ->
    gen_tcp:send(Sock, Data),
    {ok, State};

handle_ssh_msg({ssh_cm, _CM, {eof, ChId}}, State) ->
    {stop, ChId, State};

handle_ssh_msg({ssh_cm, _CM, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _CM, {exit_signal, ChId, _, _Error, _}}, State) ->
    {stop, ChId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChId, _Status}}, State) ->
    {stop, ChId, State}.


terminate(_Reason, #state{fwd_socket=Sock}) ->
    gen_tcp:close(Sock),
    ok.
