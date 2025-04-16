%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-module(prim_tty_sighandler).
-moduledoc false.

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, code_change/3]).

init(#{parent := _, reader := _} = State) ->
    ok = os:set_signal(sigcont, handle),
    ok = os:set_signal(sigwinch, handle),
    {ok, State}.

handle_event(Signal, #{parent := Parent, reader := ReaderRef} = State)
  when Signal =:= sigcont orelse Signal =:= sigwinch ->
    Parent ! {ReaderRef, {signal, Signal}},
    {ok, State};
handle_event(_Signal, State) ->
    %% Other signals are ignored by `prim_tty'.
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
