%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

-module(erts_test_sync_tracer).

%% A NIF test tracer module to sync threads
%% at trace points (like GC).
%% The test case must make sure to run on different threads
%% otherwise a single thread will block in trace point
%% waiting for itself.

-export([init/1,
         enabled/3, trace/5,
         enable_trace/1,
         set_sync/1, wait_sync/2]).
-nifs([nifs_loaded/0,
       enabled/3, trace/5,
       enable_trace/1,
       set_sync/1, get_sync/0]).

init(Config) ->
    case nifs_loaded() of
        true ->
            ok;
        false ->
            Data = proplists:get_value(data_dir, Config),
            NifLib = filename:join(Data, atom_to_list(?MODULE)),
            ok = erlang:load_nif(NifLib, []),
            ok
    end.

nifs_loaded() ->
    false.

%% Wait for other thread to block at trace point
wait_sync(Sync, Timeout) ->
    case {get_sync(), Timeout} of
        {Sync, _} ->
            ok;
        {Other, 0} ->
            {timeout, Other};
        {Other, _} ->
            case Timeout rem 1000 of
                0 -> erlang:display({"Waiting", Timeout,"for",Sync,"got",Other});
                _ -> ok
            end,
            timer:sleep(1),
            wait_sync(Sync, Timeout - 1)
    end.

%% Our own control NIFs
enable_trace(_) ->
    erlang:nif_error(~"NIF not loaded").
set_sync(_Sync) ->
    erlang:nif_error(~"NIF not loaded").
get_sync() ->
    erlang:nif_error(~"NIF not loaded").

%% erl_tracer callback NIFs:
enabled(_, _, _) ->
    erlang:nif_error(~"NIF not loaded").
trace(_, _, _, _, _) ->
    erlang:nif_error(~"NIF not loaded").
