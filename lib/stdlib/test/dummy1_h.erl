%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(dummy1_h).

%% Test event handler for gen_event_SUITE.erl

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, format_status/2]).

init(make_error) ->
    {error, my_error};
init({_, error}) -> % swap from non-existing handler.
    non_existing;
init({swap, {ok, OldState}}) ->
    {ok, OldState};
init([Parent]) ->
    {ok, Parent}.  %% We will send special responses for every handled event.

handle_event(delete_event, _Parent) ->
    remove_handler;
handle_event(do_crash, _State) ->
    erlang:error({badmatch,4});
%%Inverse of dummy_h
handle_event(hibernate, Parent) ->
    {ok,Parent};
handle_event(wakeup, Parent) ->
    {ok,Parent,hibernate};
handle_event(Event, Parent) ->
    Parent ! {dummy1_h, Event},
    {ok, Parent}.

handle_call(delete_call, _State) ->
    {remove_handler, ok};
handle_call(_Query, State) ->
    {ok, ok, State}.

handle_info(delete_info, _Parent) ->
    remove_handler;
handle_info(do_crash, _State) ->
    erlang:error({badmatch,4});
handle_info(gnurf, Parent) ->
    {ok, Parent, hibernate};
handle_info(Info, Parent) ->
    Parent ! {dummy1_h, Info},
    {ok, Parent}.

terminate(return_hej, _State) ->
    return_hej;
terminate(remove_handler, Parent) ->
    Parent ! {dummy1_h, removed};
terminate(_Reason, _State) ->
    ok.

format_status(_Opt, [_PDict, _State]) ->
    "dummy1_h handler state".
