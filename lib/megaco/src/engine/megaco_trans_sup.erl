%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
	    


    


