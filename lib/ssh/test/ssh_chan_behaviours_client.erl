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

%%% Description: Example ssh client
-module(ssh_chan_behaviours_client).
-behaviour(ssh_client_channel).
-record(state, {
          parent,
          cm,
          ch,
          dbg
         }).
-export([start_link/1, start/1,
         stop/1, send_eof/1,
         init/1, handle_msg/2, handle_ssh_msg/2, terminate/2,
         code_change/3, handle_call/3, handle_cast/2
        ]).

-define(DBG(State,Fmt,Args),
        case State#state.dbg of
            true -> ct:log("~p:~p ~p C=~p Ch=~p "++Fmt,
                           [?MODULE,?LINE,self(),State#state.cm,State#state.ch|Args]);
            false -> ok
        end).


start_link(C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    ssh_client_channel:start_link(C, Ch, ssh_chan_behaviours_client, [C, Ch, self(), true]).

start(C) ->
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    ssh_client_channel:start(C, Ch, ssh_chan_behaviours_client, [C, Ch, self(), true]).

send_eof(ChRef) ->
    ssh_client_channel:call(ChRef, send_eof).

stop(ChRef) ->
    ssh_client_channel:call(ChRef, stop).


init([C, Ch, Parent, Dbg|_Exec]) ->
    case ssh_connection:subsystem(C, Ch, "ch1", infinity) of
        success ->
            State = #state{cm = C,
                           ch = Ch,
                           parent=Parent,
                           dbg=Dbg},
            ?DBG(State, "callback spawned, parent = ~p", [Parent]),
            {ok, State};

        Other ->
            {stop, Other}
    end.

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


handle_call(send_eof, _From,#state{ch=Ch,cm=C} =  State) ->
    {reply, ssh_connection:send_eof(C,Ch), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    ?DBG(State, "Unknown call ~p", [Msg]),
    {reply, {unknown_call,Msg}, State}.


terminate(Reason, State) ->
    tell_parent({terminate,Reason}, State),
    ?DBG(State, "terminate Reason = ~p",[Reason]).


handle_cast(Msg, State) -> 
    ?DBG(State, "Unknown cast ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%================================================================
%%%
%%%

tell_parent(Msg, #state{parent = Parent,
                        cm = C,
                        ch = Ch}) -> Parent ! {{C,Ch}, Msg}.

