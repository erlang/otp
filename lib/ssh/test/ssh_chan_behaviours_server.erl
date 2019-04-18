%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
-module(ssh_chan_behaviours_server).
-behaviour(ssh_server_channel).
-record(state, {
          parent,
          cm,
          ch,
          dbg
	 }).
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

-define(DBG(State,Fmt,Args),
	case State#state.dbg of
	    true -> ct:log("~p:~p ~p C=~p Ch=~p "++Fmt,
                           [?MODULE,?LINE,self(),State#state.cm,State#state.ch|Args]);
	    false -> ok
	end).


init([Pid,Dbg|_Exec]) ->
    {ok, #state{parent=Pid,
                dbg=Dbg}}.

handle_msg({ssh_channel_up, ChannelId, ConnectionManager}=M, State0) ->
    State = State0#state{cm = ConnectionManager,
                         ch = ChannelId},
    tell_parent(M, State),
    ?DBG(State, "ssh_channel_up",[]),
    {ok, State}.

handle_ssh_msg({ssh_cm, C, {data, Ch, 0, Data}}=M, #state{ch=Ch,cm=C} = State) ->
    tell_parent(M, State),
    ?DBG(State, "ssh_cm data size(Data)=~p",[size(Data)]),
    {ok, State};

handle_ssh_msg({ssh_cm, C, {data, Ch, Type, Data}}=M, #state{ch=Ch,cm=C} = State) ->
    tell_parent(M, State),
    ?DBG(State, "ssh_cm data Type=~p : ~p",[Type,Data]),
    {ok, State};

handle_ssh_msg({ssh_cm, C, {eof, Ch}}=M, #state{ch=Ch,cm=C} = State) ->
    tell_parent(M, State),
    ?DBG(State, "eof",[]),
    {ok, State};

handle_ssh_msg({ssh_cm, C, {signal, Ch, _SigNameStr}=Sig} = M, #state{ch=Ch,cm=C} = State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    tell_parent(M, State),
    ?DBG(State, "~p",[Sig]),
    {ok, State};

handle_ssh_msg({ssh_cm, C, {exit_signal, Ch, _, _Error, _}=Sig}=M, #state{ch=Ch,cm=C} = State) ->
    tell_parent(M, State),
    ?DBG(State, "~p",[Sig]),
    {stop, Ch, State};

handle_ssh_msg({ssh_cm, C, {exit_status, Ch, _Status}=Sig}=M, #state{ch=Ch,cm=C} = State) ->
    tell_parent(M, State),
    ?DBG(State, "~p",[Sig]),
    {stop, Ch, State}.

terminate(Reason, State) ->
    tell_parent({terminate,Reason}, State),
    ?DBG(State, "terminate Reason = ~p",[Reason]),
    ok.

%%%================================================================
%%%
%%%

tell_parent(Msg, #state{parent = Parent,
                        cm = C,
                        ch = Ch}) -> Parent ! {{C,Ch}, Msg}.
    
