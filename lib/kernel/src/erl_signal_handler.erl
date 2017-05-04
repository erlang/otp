%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

-module(erl_signal_handler).
-behaviour(gen_event).
-export([init/1, format_status/2,
         handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{}).

init(_Args) ->
    {ok, #state{}}.

handle_event(sigusr1, S) ->
    erlang:halt("Received SIGUSR1"),
    {ok, S};
handle_event(sigquit, S) ->
    erlang:halt(),
    {ok, S};
handle_event(sigterm, S) ->
    error_logger:info_msg("SIGTERM received - shutting down~n"),
    ok = init:stop(),
    {ok, S};
handle_event(_SignalMsg, S) ->
    {ok, S}.

handle_info(_Info, S) ->
    {ok, S}.

handle_call(_Request, S) ->
    {ok, ok, S}.

format_status(_Opt, [_Pdict,_S]) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

terminate(_Args, _S) ->
    ok.
