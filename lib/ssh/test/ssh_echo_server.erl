%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%% Description: Example ssh server
-module(ssh_echo_server).
-behaviour(ssh_daemon_channel).
-record(state, {
	  n,
	  id,
	  cm,
	  dbg = false
	 }).
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-define(DBG(State,Fmt,Args),
	case State#state.dbg of
	    true -> ct:log("~p:~p ~p  "++Fmt, [?MODULE,?LINE,self()|Args]);
	    false -> ok
	end).


init([N]) ->
    {ok, #state{n = N}};
init([N,Opts]) ->
    State = #state{n = N,
		   dbg = proplists:get_value(dbg,Opts,false)
		  },
    ?DBG(State, "init([~p])",[N]),
    {ok, State}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}, State) ->
    ?DBG(State, "ssh_channel_up Cid=~p ConnMngr=~p",[ChannelId,ConnectionManager]),
    {ok, State#state{id = ChannelId,
		     cm = ConnectionManager}}.

handle_ssh_msg({ssh_cm, CM, {data, ChannelId, 0, Data}}, #state{n = N} = State) ->
    M = N - size(Data),
    case M > 0 of
	true ->
	    ?DBG(State, "ssh_cm data Cid=~p size(Data)=~p M=~p",[ChannelId,size(Data),M]),
	    ssh_connection:send(CM, ChannelId, Data),
	    {ok, State#state{n = M}};
	false ->
	    <<SendData:N/binary, _/binary>> = Data,
	    ?DBG(State, "ssh_cm data Cid=~p size(Data)=~p M=~p size(SendData)=~p~nSend eof",[ChannelId,size(Data),M,size(SendData)]),
	    ssh_connection:send(CM, ChannelId, SendData),
	    ssh_connection:send_eof(CM, ChannelId),
	    {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, _ConnectionManager,
		{data, _ChannelId, 1, Data}}, State) ->
    ?DBG(State, "stderr: ~p",[Data]),
    error_logger:format(standard_error, " ~p~n", [binary_to_list(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, _ConnectionManager, {eof, _ChannelId}}, State) ->
    ?DBG(State, "{eof ~p}",[_ChannelId]),
    {ok, State};

handle_ssh_msg({ssh_cm, _, _Sig={signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    ?DBG(State, "~p",[_Sig]),
    {ok, State};

handle_ssh_msg({ssh_cm, _, _Sig={exit_signal, ChannelId, _, _Error, _}}, State) ->
    ?DBG(State, "~p",[_Sig]),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, _Sig={exit_status, ChannelId, _Status}}, State) ->
    ?DBG(State, "~p",[_Sig]),
    {stop, ChannelId, State}.

terminate(_Reason, _State) ->
    ?DBG(_State, "terminate ~p",[_Reason]),
    ok.
