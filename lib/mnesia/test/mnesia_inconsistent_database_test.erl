%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(mnesia_inconsistent_database_test).
-author('hakan@erix.ericsson.se').

-behaviour(gen_event).

%%-compile([export_all]).
-include("mnesia_test_lib.hrl").

%% gen_event callback interface
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).


init(_Args) ->
    ?verbose("~p installed as event_module~n", [?MODULE]),
    {ok, []}.

handle_event(Msg, State) ->
    handle_any_event(Msg, State).

handle_info(Msg, State) ->
    handle_any_event(Msg, State).


handle_call(Msg, State) ->
    handle_any_event(Msg, State).


%% The main...

handle_any_event({mnesia_system_event, Event}, State)
        when element(1, Event) == inconsistent_database ->
    ?error("Got event: ~p~n", [Event]),
    {ok, State};
handle_any_event(Msg, State) ->
    ?verbose("Got event: ~p~n", [Msg]),
    {ok, State}.

%%-----------------------------------------------------------------
%% terminate(Reason, State) ->
%%     AnyVal
%%-----------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, _State, _Extra) ->
    exit(not_supported).

