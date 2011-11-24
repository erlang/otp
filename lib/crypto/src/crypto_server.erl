%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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

%% Purpose: Provide cryptographic algorithms.

-module(crypto_server).

-behaviour(gen_server).

-export([start_link/0]).

%% Internal exports, call-back functions.
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,code_change/3,
	 terminate/2]).


%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

start_link() ->
    gen_server:start_link({local, crypto_server}, crypto_server, [], []).

init([]) ->
    {ok,[]}.



%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    [].





    
