%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%%% @doc Event handler module
%%%
%%% <p>This is an event handler module used for testing that 
%%%    Common Test generates events as expected.</p>
%%%
-module(eh_A).

-behaviour(gen_event).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {cbm=ct_test_support,
		trace_level=50}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init(String = [X|_]) when is_integer(X) ->
    case erl_scan:string(String++".") of
	{ok,Ts,_} ->
	    case erl_parse:parse_term(Ts) of
		{ok,Args} ->
		    init(Args);
		_ ->
		    init(String)
	    end;
	_ ->
	    init(String)
    end;

init(Args) ->
    
    S1 = case lists:keysearch(cbm, 1, Args) of
	     {_,{cbm,CBM}} ->
		 #state{cbm=CBM};
	     _ ->
		 #state{}
	 end,
    S2 = case lists:keysearch(trace_level, 1, Args) of
	     {_,{trace_level,Level}} ->
		 S1#state{trace_level=Level};
	     _ ->
		 S1
	 end,
    print(S2#state.trace_level, "Event Handler ~w started!~n", [?MODULE]),
    {ok,S2}.

%%--------------------------------------------------------------------
%% Function:  
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%--------------------------------------------------------------------
handle_event(Event, State=#state{cbm=CBM, trace_level=_Level}) ->
    % print(_Level, "~p: ~p~n", [Event#event.name,Event#event.data]),
    CBM:handle_event(?MODULE, Event),
    {ok,State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Req, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

print(Level, _Str, _Args) ->
    test_server:format(Level, _Str,_Args).


