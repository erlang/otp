%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Used when performing testing appup tests
%%
%% {ok, P} = megaco_appup_mgc:start().
%% megaco_appup_mgc:stop(P).
%% megaco_appup_mgc:verbosity(P,silence).
%% megaco_appup_mgc:verbosity(P,debug).
%% 
%%----------------------------------------------------------------------

-module(megaco_appup_mgc).

-export([start/0, start/1, stop/1]).
-export([verbosity/2]).

-export([main/2]).

-define(START(Mid, ET, Verb), 
	megaco_test_mgc:start(node(), Mid, ET, Verb)).
-define(STOP(Pid),        megaco_test_mgc:stop(Pid)).
-define(REQ_HANDS(Pid),   megaco_test_mgc:request_handle_sloppy(Pid)).
-define(VERBOSITY(Pid,V), megaco_test_mgc:verbosity(Pid,V)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    start(silence).
start(V) ->
    proc_lib:start_link(?MODULE, main, [self(), V]).

stop(Pid) ->
    Pid ! stop.

verbosity(Pid, V) ->
    Pid ! {verbosity, V}.


%% ------------------------------------------------------------------------

main(Parent, V) ->
    d("starting"),
    Mgc = init(V),
    proc_lib:init_ack(Parent, {ok, self()}),
    d("started"),
    loop(Mgc).

init(V) ->
    Mid = {deviceName, "mgc"},
    ET  = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],

    d("start MGC"),    
    {ok, Mgc} = ?START(Mid, ET, debug),
 
    ?VERBOSITY(Mgc, V),

    Mgc.

loop(Mgc) ->
    d("awaiting request..."),
    receive

	stop ->
	    d("stopping"),
	    ?STOP(Mgc),
	    exit(normal);

	{verbosity, V} ->
	    d("verbosity: ~p", [V]),
	    ?VERBOSITY(Mgc, V);

        Any ->
	    error("received unknown request: ~n~p", [Any])

    end,
    loop(Mgc).

%% ------------------------------------------------------------------------
    
error(F, A) ->
    io:format("AMGC-ERROR: " ++ F, A).

d(F) ->
    d(F, []).

d(F, A) ->
    io:format("AMGC: " ++ F ++ "~n", A).
