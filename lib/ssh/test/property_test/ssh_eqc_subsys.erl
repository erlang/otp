%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2014. All Rights Reserved.
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

-module(ssh_eqc_subsys).

-behaviour(ssh_daemon_channel).

-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-export([response/2]).

-record(state, {id, 
		cm,
		subsyst
	       }).

init([SS]) ->
    {ok, #state{subsyst=SS}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    {ok, State#state{id = ChannelId,
		     cm = ConnectionManager}}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, Type, Data}}, S) ->
    ssh_connection:send(CM, ChannelId, Type, response(Data,S)),
    {ok, S};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}}, State) ->
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ok.


response(Msg, #state{subsyst=SS}) -> response(Msg, SS);
response(Msg, SS) -> <<"Resp: ",Msg/binary,(list_to_binary(SS))/binary>>.
