%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2000-2025. All Rights Reserved.
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
-module(erl_init).
-moduledoc false.

%% Initial process of an Erlang system.

-export([start/2, restart/0]).

%% This gets the module name given by the +i option (default 'init')
%% and the list of command line arguments

-spec start(Mod, BootArgs) -> no_return() when
      Mod :: module(),
      BootArgs :: [binary()].
start(Mod, BootArgs) ->
    %% Load the static nifs
    zlib:on_load(),
    erl_tracer:on_load(),
    prim_buffer:on_load(),
    prim_file:on_load(),
    %% prim_socket:on_load(), prim_net:on_load(),
    if_loaded(
      prim_socket,
      fun () ->
              prim_socket:on_load(),
              prim_net:on_load(),
              ok
      end),
    %% Proceed to the specified boot module
    run(Mod, boot, BootArgs).

restart() ->
    erts_internal:erase_persistent_terms(),
    if_loaded(
      prim_socket,
      fun () ->
              prim_socket:init(),
              ok
      end).


run(M, F, A) ->
    case erlang:function_exported(M, F, 1) of
	false ->
	    erlang:display({fatal,error,module,M,"does not export",F,"/1"}),
	    halt(1);
	true ->
            M:F(A)
    end.


if_loaded(CondMod, Fun) ->
    if_loaded(CondMod, Fun, erlang:loaded()).
%%
if_loaded(_CondMod, _Fun, []) ->
    ok;
if_loaded(CondMod, Fun, [CondMod | _]) ->
    Fun();
if_loaded(CondMod, Fun, [_ | Loaded]) ->
    if_loaded(CondMod, Fun, Loaded).
