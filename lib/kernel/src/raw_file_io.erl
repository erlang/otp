%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(raw_file_io).

-export([open/2]).

open(Filename, Modes) ->
    %% Layers are applied in this order, and the listed modules will call this
    %% function again as necessary. eg. a raw compressed delayed file in list
    %% mode will walk through [_list -> _compressed -> _delayed -> _raw].
    ModuleOrder = [{raw_file_io_list, fun match_list/1},
                   {raw_file_io_compressed, fun match_compressed/1},
                   {raw_file_io_delayed, fun match_delayed/1},
                   {raw_file_io_raw, fun match_raw/1}],
    open_1(ModuleOrder, Filename, add_implicit_modes(Modes)).
open_1([], _Filename, _Modes) ->
    error(badarg);
open_1([{Module, Match} | Rest], Filename, Modes) ->
    case lists:any(Match, Modes) of
        true ->
            {Options, ChildModes} =
                lists:partition(fun(Mode) -> Match(Mode) end, Modes),
            Module:open_layer(Filename, ChildModes, Options);
        false ->
            open_1(Rest, Filename, Modes)
    end.

%% 'read' and 'list' mode are enabled unless disabled by another option, so
%% we'll explicitly add them to avoid duplicating this logic in child layers.
add_implicit_modes(Modes0) ->
    Modes1 = add_unless_matched(Modes0, fun match_writable/1, read),
    add_unless_matched(Modes1, fun match_binary/1, list).
add_unless_matched(Modes, Match, Default) ->
    case lists:any(Match, Modes) of
        false -> [Default | Modes];
        true -> Modes
    end.

match_list(list) -> true;
match_list(_Other) -> false.

match_compressed(compressed) -> true;
match_compressed(_Other) -> false.

match_delayed({delayed_write, _Size, _Timeout}) -> true;
match_delayed(delayed_write) -> true;
match_delayed(_Other) -> false.

match_raw(raw) -> true;
match_raw(_Other) -> false.

match_writable(write) -> true;
match_writable(append) -> true;
match_writable(exclusive) -> true;
match_writable(_Other) -> false.

match_binary(binary) -> true;
match_binary(_Other) -> false.
