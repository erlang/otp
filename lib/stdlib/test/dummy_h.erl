%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(dummy_h).

%% Test event handler for gen_event_SUITE.erl

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init(make_error) ->
    {error, my_error};
init([Parent]) ->
    {ok, Parent};  %% We will send special responses for every handled event.
init([Parent,hibernate]) ->
    {ok, Parent, hibernate}.  %% We will send special responses for every handled event.

handle_event({swap_event,Mod,Args}, State) ->
    {swap_handler, swap, State, Mod, Args};
handle_event(error_event, _State) ->
    {return, faulty};
handle_event(do_crash, _State) ->
    erlang:error({badmatch,4});
handle_event(hibernate, _State) ->
   {ok,[],hibernate};
handle_event(wakeup, _State) ->
    {ok,[]};
handle_event(Event, Parent) ->
    Parent ! {dummy_h, Event},
    {ok, Parent}.

handle_call(hejsan, State) ->
    {ok, {ok, hejhopp}, State};
handle_call({swap_call,Mod,Args}, State) ->
    {swap_handler, {ok, swapped}, swap, State, Mod, Args};
handle_call(error_call, _State) ->
    {return, faulty};
handle_call(exit_call, _State) ->
    erlang:error({badmatch,4});
handle_call(hibernate, _State) ->
    {ok,true,[],hibernate};
handle_call(hibernate_later, _State) ->
    timer:send_after(1000,sleep),
    {ok,later,[]};
handle_call(_Query, State) ->
    {ok, ok, State}.

handle_info({swap_info,Mod,Args}, State) ->
    {swap_handler, swap, State, Mod, Args};
handle_info(error_info, _State) ->
    {return, faulty};
handle_info(do_crash, _State) ->
    erlang:error({badmatch,4});
handle_info(sleep, _State) ->
    {ok, [], hibernate};
handle_info(wake, _State) ->
    {ok, []};
handle_info(gnurf, _State) ->
    {ok, []};
handle_info(Info, Parent) ->
    Parent ! {dummy_h, Info},
    {ok, Parent}.

terminate(return_hej, _State) ->
    return_hej;
terminate(swap, State) ->
    {ok, State};
terminate({error, {return, faulty}}, Parent) ->
    Parent ! {dummy_h, returned_error};
terminate(_Reason, _State) ->
    ok.


