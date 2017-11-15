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
-module(supervisor_1).

-export([start_child/0, start_child/1, init/1]).

-export([handle_call/3, handle_info/2, terminate/2]).

start_child(ignore) ->
    case get(child_ignored) of
	true ->
	    start_child();
	_ ->
	    put(child_ignored, true),
	    ignore
    end;

start_child(error) ->
    case get(start_child_error) of
	undefined ->
	    put(start_child_error, set),
	    start_child();
	set -> gen_server:start_link(?MODULE, error, [])
    end;

start_child({return, Term}) ->
    Term;

start_child(Extra) ->
    {ok, Pid} = gen_server:start_link(?MODULE, normal, []),
    {ok, Pid, Extra}.

start_child() ->
    gen_server:start_link(?MODULE, normal, []).

init(normal) ->
    process_flag(trap_exit, true),
    {ok, {}}.

handle_call(Req, _From, State) ->
    {reply, Req, State}.

handle_info(die, State) ->
    {stop, died, State};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'EXIT',_,shutdown}, State) ->
    {stop, shutdown, State};

handle_info({'EXIT',_,{shutdown,Term}}, State) ->
    {stop, {shutdown,Term}, State};

handle_info({sleep, Time}, State) ->
    io:format("FOO: ~p~n", [Time]),
    timer:sleep(Time),
    io:format("FOO: sleept~n", []),
    handle_info({sleep, Time}, State);

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.




