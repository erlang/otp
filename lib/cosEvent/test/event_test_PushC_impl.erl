%%------------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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
%%------------------------------------------------------------------------
%% Description: a very simple implementation of Push Consumer interface
%%------------------------------------------------------------------------
 
-module(event_test_PushC_impl).
 
-export([init/1, terminate/2, push/2, disconnect_push_consumer/1, get_data/1]).

init(_) ->
        {ok, []}.

terminate(_From, _Reason) ->
        ok.

push(Events, Event) ->
    {reply, ok, [Event|Events]}.

disconnect_push_consumer(Events) ->
    io:format("event_test_PushC terminates: ~p~n", [Events]),
    {stop, normal, ok, Events}.
 

get_data(Events) ->
    %% Returns Events in FIFO order and reset state.
    {reply, lists:reverse(Events), []}.

