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
%% File: CosNaming_BindingIterator_impl.erl
%%
%%-----------------------------------------------------------------
-module('CosNaming_BindingIterator_impl').

-include_lib("orber/include/corba.hrl").
-include("CosNaming.hrl").
-include("orber_cosnaming.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, code_change/3]).
-export([next_one/1, next_n/2, destroy/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Args:
%% Returns: 
%%-----------------------------------------------------------------
init(State) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Args:
%% Returns: 
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

next_one([]) ->
    NoBinding = #'CosNaming_Binding'{binding_name=[],
				     binding_type=nobject},
    {reply, {false, NoBinding}, []};
next_one([Binding]) ->
    {reply, {true, Binding}, []};
next_one([Binding|Rest]) ->
    {reply, {true, Binding}, Rest}.

next_n([], _) ->
    {reply, {false, []}, []};
next_n(List, HowMany) ->
    {More, Acc, NewList} = split(List, HowMany, []),
    {reply, {More, Acc}, NewList}.

split([], _, Acc) ->
    {false, Acc, []};
split(Rest, 0, Acc) ->
    {true, Acc, Rest};
split([H|T], N, Acc) ->
    split(T, N-1, [H|Acc]).

		    
destroy(OE_State) ->
    {stop, normal, ok, OE_State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
