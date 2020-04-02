%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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

-module(compile_prop).
-compile([export_all, nowarn_export_all]).

%% This module only supports proper, as we don't have an eqc license to test
%% with.

-proptest([proper]).

-ifdef(PROPER).

-define(BEAM_TYPES_INTERNAL, true).
-include_lib("compiler/src/beam_types.hrl").

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc, proper).

-import(lists, [duplicate/2,foldl/3]).

-define(REPETITIONS, 1000).

compile() ->
    numtests(?REPETITIONS, compile_1()).

compile_1() ->
    Opts = [{resize,true}],
    ?FORALL(Abstr, proper_abstr:module(Opts),
            ?WHENFAIL(
               begin
                   io:format("~ts\n", [[erl_pp:form(F) || F <- Abstr]]),
                   compile(Abstr, [binary,report_errors])
               end,
               case compile(Abstr, [binary]) of
                   {error, _Es, _Ws} -> false;
                   _ -> true
               end)).

compile(Abstr, Opts) ->
    compile:noenv_forms(Abstr, Opts).
-endif.
