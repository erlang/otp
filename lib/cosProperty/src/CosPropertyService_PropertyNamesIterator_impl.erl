%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% File    : CosPropertyService_PropertyNamesIterator_impl.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module('CosPropertyService_PropertyNamesIterator_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("CosPropertyService.hrl").
-include("cosProperty.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory server functions
-export([init/1, 
	 terminate/2,
	 code_change/3]).

-export([reset/1, 
	 next_one/1, 
	 next_n/2, 
	 destroy/1]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {properties, counter=1, length}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
-define(CreateInitState(L),    #state{properties = L, length = length(L)}).
-define(get_Properties(S),     S#state.properties).
-define(get_Counter(S),        S#state.counter).
-define(get_Length(S),         S#state.length).

-define(set_Properties(S, P),  S#state{properties = P}).
-define(set_Counter(S, C),     S#state{counter = C}).
-define(set_Length(S, L),      S#state{length = L}).

-define(increment_Counter(S),  S#state{counter = S#state.counter+1}).
-define(decrement_Counter(S),  S#state{counter = S#state.counter-1}).
-define(addto_Counter(S, N),   S#state{counter = S#state.counter+N}).
-define(subfrom_Counter(S, N), S#state{counter = S#state.counter-N}).


%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Description: Initiates the server
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%%----------------------------------------------------------------------
init(Properties) ->
    {ok, ?CreateInitState(Properties)}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Description: Shutdown the server
%% Returns    : any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Description: Convert process state when code is changed
%% Returns    : {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% Function   : reset
%% Arguments  : -
%% Description: 
%% Returns    : {ok, NewState}
%%----------------------------------------------------------------------
reset(State) ->
    {reply, ok, ?set_Counter(State, 1)}.

%%---------------------------------------------------------------------%
%% Function   : next_one
%% Arguments  : -
%% Description: 
%% Returns    : {ok, {bool(), PropertyName}, NewState}
%%----------------------------------------------------------------------
next_one(State) when ?get_Counter(State) > ?get_Length(State) ->
    {reply, {false, ""}, State};
next_one(State) ->
    {reply, {true, lists:nth(?get_Counter(State), ?get_Properties(State))}, 
     ?set_Counter(State, 1+?get_Counter(State))}.

%%---------------------------------------------------------------------%
%% Function   : next_n
%% Arguments  : N - how many properties we should return.
%% Description: 
%% Returns    : {ok, {bool(), PropertyNameList}, NewState}
%%----------------------------------------------------------------------
next_n(State, N) ->
    case lists:sublist(?get_Properties(State), ?get_Counter(State), N) of
	Properties when N+?get_Counter(State) < ?get_Length(State) ->
	    {reply, {true, Properties}, ?set_Counter(State, N+?get_Counter(State))};
	Properties ->
	    {reply, {false, Properties}, ?set_Counter(State, ?get_Length(State))}
    end.

%%---------------------------------------------------------------------%
%% Function   : destroy
%% Arguments  : -
%% Description: Terminate the object
%% Returns    : {ok, NewState}
%%----------------------------------------------------------------------
destroy(State) ->
    {stop, normal, ok, State}.

%%======================================================================
%% Internal functions
%%======================================================================

%%======================================================================
%% END OF MODULE
%%======================================================================
