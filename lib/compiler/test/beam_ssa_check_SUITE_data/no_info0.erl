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
%% Extracts from beam_clean.erl to test cases when special handling of
%% the no_info sharing state is required for correct functioning.
%%
-module(no_info0).
-moduledoc false.

-export([clean_labels/1]).

-import(lists, [reverse/1]).

-type label() :: beam_asm:label().

-record(st, {lmap :: [{label(),label()}], %Translation tables for labels.
	     entry :: beam_asm:label(),   %Number of entry label.
	     lc :: non_neg_integer()      %Label counter
	     }).

clean_labels(Fs0) ->
    St0 = #st{lmap=[],entry=1,lc=1},
    function_renumber(Fs0, St0, []).

function_renumber([{function,Name,Arity,_Entry,Asm0}|Fs], St0, Acc) ->
    {Asm,St} = renumber_labels(Asm0, [], St0),
    function_renumber(Fs, St, [{function,Name,Arity,St#st.entry,Asm}|Acc]);
function_renumber([], St, Acc) -> {Acc,St}.

renumber_labels([{label,Old}|Is], [{label,New}|_]=Acc, #st{lmap=D0}=St) ->
%ssa% (_, _, Rec) when post_ssa_opt ->
%ssa% _ = update_record(inplace, 4, Rec, ...),
%ssa% _ = update_record(inplace, 4, Rec, ...),
%ssa% _ = update_record(inplace, 4, Rec, ...).
    D = [{Old,New}|D0],
    renumber_labels(Is, Acc, St#st{lmap=D});
renumber_labels([{label,Old}|Is], Acc, St0) ->
    New = St0#st.lc,
    D = [{Old,New}|St0#st.lmap],
    renumber_labels(Is, [{label,New}|Acc], St0#st{lmap=D,lc=New+1});
renumber_labels([{func_info,_,_,_}=Fi|Is], Acc, St0) ->
    renumber_labels(Is, [Fi|Acc], St0#st{entry=St0#st.lc});
renumber_labels([I|Is], Acc, St0) ->
    renumber_labels(Is, [I|Acc], St0);
renumber_labels([], Acc, St) -> {Acc,St}.
