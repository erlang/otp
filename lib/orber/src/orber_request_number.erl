%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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


