%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% Description: Simulates the behaviour that a child process may have.
%% Is used by the supervisor_SUITE test suite.
-module(supervisor_3).

-export([start_child/2, init/1]).

-export([handle_call/3, handle_info/2, terminate/2]).

start_child(Name, Caller) ->
    gen_server:start_link(?MODULE, [Name, Caller], []).

init([Name, Caller]) ->
    Caller ! {Name, self()},
    receive
        {Result, Caller} ->
            Result
    end.

handle_call(Req, _From, State) ->
    {reply, Req, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, Time) ->
    timer:sleep(Time),
    ok.
