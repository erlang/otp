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
%% This module tests that beam_ssa_bsm:opt/2 correctly handles modules
%% containing nifs.
%%
-module(nifs_many_blocks).

-export([load_nif/0, a_nif/2]).

-nifs([a_nif/2]).

load_nif() ->
    ok = erlang:load_nif("dummy", 0).

a_nif(1, <<Tag, Rest/binary>>) ->
%ssa% (_, _) { parameter_info => #{} } when post_ssa_opt ->
%ssa% ret(_).
    case Tag of
        $a -> {ok, a, a_nif(1, Rest)};
        $b -> {ok, b};
        $c -> {ok, c};
        $d -> {ok, d};
        $e -> {ok, e};
        $f -> {ok, f};
        $g -> {ok, g};
        $h -> {ok, h};
        $i -> {ok, i};
        $j -> {ok, j};
        $k -> {ok, k};
        $l -> {ok, l};
        $m -> {ok, m};
        $n -> {ok, n};
        $o -> {ok, o};
        $p -> {ok, p};
        $q -> {ok, q};
        $r -> {ok, r};
        $s -> {ok, s};
        $t -> {ok, t};
        $u -> {ok, u};
        $v -> {ok, v};
        $w -> {ok, w};
        $x -> {ok, x};
        $y -> {ok, y}
    end;
a_nif(2, Bin) ->
    <<Tag, _Rest/binary>> = Bin,
    Tag.
