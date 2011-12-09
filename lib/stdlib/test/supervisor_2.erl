%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
%% Description: Simulates the behaviour that a child process may have.
%% Is used by the supervisor_SUITE test suite.
-module(supervisor_2).

-export([start_child/1, init/1]).

-export([handle_call/3, handle_info/2, terminate/2]).

start_child(Time) when is_integer(Time), Time > 0 ->
    gen_server:start_link(?MODULE, Time, []).

init(Time) ->
    process_flag(trap_exit, true),
    {ok, Time}.

handle_call(Req, _From, State) ->
    {reply, Req, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, Time) ->
    timer:sleep(Time),
    ok.
