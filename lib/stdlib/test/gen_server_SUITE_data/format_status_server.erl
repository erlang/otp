%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2021. All Rights Reserved.
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
-module(format_status_server).

-behaviour(gen_server).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, format_status/1]).

start(Arg) ->
    gen_server:start(?MODULE, ok, Arg).
init(Args) ->
    gen_server_SUITE:init(Args).
handle_call(Call, From, State) ->
    gen_server_SUITE:handle_call(Call, From, State).
handle_cast(Cast, State) ->
    gen_server_SUITE:handle_cast(Cast, State).
handle_info(Info, State) ->
    gen_server_SUITE:handle_info(Info, State).
terminate(Reason, State) ->
    gen_server_SUITE:terminate(Reason, State).

format_status(#{ state := Fun } = S) when is_function(Fun) ->
    Fun(S);
format_status(#{ state := {_,_,Fun} } = S) when is_function(Fun) ->
    Fun(S);
format_status(#{ message := Msg } = S) when not is_map_key(state, S) ->
    S#{message := {message,Msg}};
format_status(#{ reason := _, state := State } = Map) ->
    ct:pal("format_status(~p)",[Map]),
    Map#{ state => {formatted, State}};
format_status(Map) ->
    ct:pal("format_status(~p)",[Map]),
    Map#{ state => format_status_called }.
