%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2007-2025. All Rights Reserved.
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
%% Purpose : Common utilities used by several optimization passes.
%%

-module(beam_utils).
-moduledoc false.
-export([replace_labels/4,split_even/1]).

-export_type([code_index/0,module_code/0,instruction/0]).

-import(lists, [map/2,reverse/1]).

%% instruction() describes all instructions that are used during optimization
%% (from beam_a to beam_z).
-type instruction() :: atom() | tuple().

-type code_index() :: gb_trees:tree(beam_asm:label(), [instruction()]).

-type int_function() :: {'function',beam_asm:function_name(),arity(),
                         beam_asm:label(),[instruction()]}.

-type module_code() ::
        {module(),[_],[_],[int_function()],pos_integer()}.

%% replace_labels(FunctionIs, Tail, ReplaceDb, Fallback) -> FunctionIs.
%%  Replace all labels in instructions according to the ReplaceDb.
%%  If label is not found the Fallback is called with the label to
%%  produce a new one.

-spec replace_labels([instruction()],
                     [instruction()],
                     #{beam_asm:label() => beam_asm:label()},
                     fun((beam_asm:label()) -> term())) -> [instruction()].
replace_labels(Is, Acc, D, Fb) ->
    replace_labels_1(Is, Acc, D, Fb).

%% split_even/1
%% [1,2,3,4,5,6] -> {[1,3,5],[2,4,6]}

-spec split_even(list()) -> {list(),list()}.

split_even(Rs) -> split_even(Rs, [], []).

%%%
%%% Local functions.
%%%
replace_labels_1([{test,Test,{f,Lbl},Ops}|Is], Acc, D, Fb) ->
    I = {test,Test,{f,label(Lbl, D, Fb)},Ops},
    replace_labels_1(Is, [I | Acc], D, Fb);
replace_labels_1([{test,Test,{f,Lbl},Live,Ops,Dst}|Is], Acc, D, Fb) ->
    I = {test,Test,{f,label(Lbl, D, Fb)},Live,Ops,Dst},
    replace_labels_1(Is, [I | Acc], D, Fb);
replace_labels_1([{select,I,R,{f,Fail0},Vls0}|Is], Acc, D, Fb) ->
    Vls = map(fun ({f,L}) -> {f,label(L, D, Fb)};
		   (Other) -> Other
	      end, Vls0),
    Fail = label(Fail0, D, Fb),
    replace_labels_1(Is, [{select,I,R,{f,Fail},Vls}|Acc], D, Fb);
replace_labels_1([{'try',R,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{'try',R,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{'catch',R,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{'catch',R,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{jump,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{jump,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{loop_rec,{f,Lbl},R}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{loop_rec,{f,label(Lbl, D, Fb)},R}|Acc], D, Fb);
replace_labels_1([{loop_rec_end,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{loop_rec_end,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{wait,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{wait,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{wait_timeout,{f,Lbl},To}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{wait_timeout,{f,label(Lbl, D, Fb)},To}|Acc], D, Fb);
replace_labels_1([{bif,Name,{f,Lbl},As,R}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{bif,Name,{f,label(Lbl, D, Fb)},As,R}|Acc], D, Fb);
replace_labels_1([{gc_bif,Name,{f,Lbl},Live,As,R}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{gc_bif,Name,{f,label(Lbl, D, Fb)},Live,As,R}|Acc], D, Fb);
replace_labels_1([{call,Ar,{f,Lbl}}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{call,Ar,{f,label(Lbl, D, Fb)}}|Acc], D, Fb);
replace_labels_1([{call_fun2,{f,Lbl},Ar,Func}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{call_fun2,{f,label(Lbl, D, Fb)},Ar,Func}|Acc], D, Fb);
replace_labels_1([{make_fun3,{f,Lbl},U1,U2,U3,U4}|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [{make_fun3,{f,label(Lbl, D, Fb)},U1,U2,U3,U4}|Acc], D, Fb);
replace_labels_1([{bs_create_bin,{f,Lbl},Alloc,Live,Unit,Dst,{list,List}}|Is], Acc, D, Fb)
  when Lbl =/= 0 ->
    replace_labels_1(Is, [{bs_create_bin,{f,label(Lbl, D, Fb)},
                           Alloc,Live,Unit,Dst,{list,List}}|Acc], D, Fb);
replace_labels_1([{put_map=I,{f,Lbl},Op,Src,Dst,Live,List}|Is], Acc, D, Fb)
  when Lbl =/= 0 ->
    replace_labels_1(Is, [{I,{f,label(Lbl, D, Fb)},Op,Src,Dst,Live,List}|Acc], D, Fb);
replace_labels_1([{get_map_elements=I,{f,Lbl},Src,List}|Is], Acc, D, Fb) when Lbl =/= 0 ->
    replace_labels_1(Is, [{I,{f,label(Lbl, D, Fb)},Src,List}|Acc], D, Fb);
replace_labels_1([{bs_start_match4,{f,Lbl},Live,Src,Dst}|Is], Acc, D, Fb) ->
    I = {bs_start_match4,{f,label(Lbl, D, Fb)},Live,Src,Dst},
    replace_labels_1(Is, [I | Acc], D, Fb);
replace_labels_1([{bs_match,{f,Lbl},Ctx,List}|Is], Acc, D, Fb) ->
    I = {bs_match,{f,label(Lbl, D, Fb)},Ctx,List},
    replace_labels_1(Is, [I | Acc], D, Fb);
replace_labels_1([I|Is], Acc, D, Fb) ->
    replace_labels_1(Is, [I|Acc], D, Fb);
replace_labels_1([], Acc, _, _) -> Acc.

label(Old, D, Fb) ->
    case D of
        #{Old := New} -> New;
        _ -> Fb(Old)
    end.

split_even([], Ss, Ds) ->
    {reverse(Ss),reverse(Ds)};
split_even([S,D|Rs], Ss, Ds) ->
    split_even(Rs, [S|Ss], [D|Ds]).
