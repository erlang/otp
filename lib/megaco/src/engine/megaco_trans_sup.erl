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
%% Purpose: The supervisor for the Megaco/H.248 transaction sender 
%%          processes. 
%%----------------------------------------------------------------------

-module(megaco_trans_sup).

-behaviour(supervisor).

%% public
-export([start/0, stop/1, init/1]).
-export([start_trans_sender/5]).

%% -define(d(F,A), io:format("~p~p:" ++ F ++ "~n", [self(),?MODULE|A])).
-define(d(F,A), ok).

start() ->
    ?d("start -> entry",[]),
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> 
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    ?d("init -> entry",[]),
    Flags     = {one_for_one, 500, 100},
    Workers   = [],
    ?d("init -> done",[]),
    {ok, {Flags, Workers}}.


%%----------------------------------------------------------------------
%% Function: start_ack_sender/3
%% Description: Starts a transient worker (child) process
%%----------------------------------------------------------------------

start_trans_sender(CH, To, ReqsMaxSz, ReqsMax, AcksMax) ->
    ?d("start_ack_sender -> entry with"
	"~n   CH:        ~p"
	"~n   To:        ~p"
	"~n   ReqsMaxSz: ~p"
	"~n   ReqsMax:   ~p"
	"~n   AxksMax:   ~p", [CH, To, ReqsMaxSz, ReqsMax, AcksMax]),
    M = megaco_trans_sender,
    F = start_link,
    A = [CH, To, ReqsMaxSz, ReqsMax, AcksMax],
    N = {M,CH}, 
    Spec = {N, 
	    {M,F,A}, 
	    temporary, timer:seconds(1), worker, [M,gen_server]},
    case supervisor:start_child(?MODULE, Spec) of
	{error, already_present} ->
	    supervisor:restart_child(?MODULE, N);
	Else ->
	    Else
    end.
	    


    


