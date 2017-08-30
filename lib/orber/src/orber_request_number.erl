%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_request_number.erl
%% 
%% Description:
%%    This file contains the request number server in Orber
%% 
%%-----------------------------------------------------------------
-module(orber_request_number).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, get/0, reset/0]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_reqno}, orber_request_number, Opts, []).

get() ->
    gen_server:call(orber_reqno, get, infinity).

reset() ->
    gen_server:call(orber_reqno, reset, infinity).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
init(_Opts) ->
    {ok, 0}.

terminate(_Reason, _State) ->
	    ok.
%% Max is ulong 0 .. 2^32-1
handle_call(get, _From, State) when State < ?ULONGMAX ->
    {reply, State, State+1};
handle_call(get, _From, _State) ->
    {reply, ?ULONGMAX, 0};
handle_call(reset, _From, _State) ->
    {reply, ok, 0}.

handle_cast(_, State) ->
    {noreply,  State}.

handle_info(_, State) ->
    {noreply,  State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


