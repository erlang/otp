%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose : Converts intermediate assembly code to final format.

-module(beam_flatten).

-export([module/2]).

-import(lists, [reverse/1,reverse/2]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,[function(F) || F <- Fs],Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    Is = block(Is0),
    {function,Name,Arity,CLabel,Is}.

block(Is) ->
    block(Is, []).

block([{block,Is0}|Is1], Acc) -> block(Is1, norm_block(Is0, Acc));
block([I|Is], Acc) -> block(Is, [I|Acc]);
block([], Acc) -> reverse(Acc).

norm_block([{set,[],[],{alloc,R,Alloc}}|Is], Acc0) ->
    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc0));
norm_block([I|Is], Acc) ->
    norm_block(Is, [norm(I)|Acc]);
norm_block([], Acc) -> Acc.

norm({set,[D],As,{bif,N,F}})      -> {bif,N,F,As,D};
norm({set,[D],As,{alloc,R,{gc_bif,N,F}}}) -> {gc_bif,N,F,R,As,D};
norm({set,[D],[],init})           -> {init,D};
norm({set,[D],[S],move})          -> {move,S,D};
norm({set,[D],[S],fmove})         -> {fmove,S,D};
norm({set,[D],[S],fconv})         -> {fconv,S,D};
norm({set,[D],[S1,S2],put_list})  -> {put_list,S1,S2,D};
norm({set,[D],Els,put_tuple2})    -> {put_tuple2,D,{list,Els}};
norm({set,[D],[],{put_tuple,A}})  -> {put_tuple,A,D};
norm({set,[],[S],put})            -> {put,S};
norm({set,[D],[S],{get_tuple_element,I}}) -> {get_tuple_element,S,I,D};
norm({set,[],[S,D],{set_tuple_element,I}}) -> {set_tuple_element,S,D,I};
norm({set,[D],[S],get_hd})        -> {get_hd,S,D};
norm({set,[D],[S],get_tl})        -> {get_tl,S,D};
norm({set,[D],[S|Puts],{alloc,R,{put_map,Op,F}}}) ->
    {put_map,F,Op,S,D,R,{list,Puts}};
norm({set,[],[],remove_message})   -> remove_message;
norm({set,[],[],fclearerror}) -> fclearerror;
norm({set,[],[],fcheckerror}) -> {fcheckerror,{f,0}};
norm({set,[],[],{line,_}=Line}) -> Line.

norm_allocate({_Zero,nostack,Nh,[]}, Regs) ->
    [{test_heap,Nh,Regs}];
norm_allocate({zero,0,Nh,[]}, Regs) ->
    norm_allocate({nozero,0,Nh,[]}, Regs);
norm_allocate({zero,Ns,0,[]}, Regs) ->
    [{allocate_zero,Ns,Regs}];
norm_allocate({zero,Ns,Nh,[]}, Regs) ->
    [{allocate_heap_zero,Ns,Nh,Regs}];
norm_allocate({nozero,Ns,0,Inits}, Regs) ->
    [{allocate,Ns,Regs}|Inits];
norm_allocate({nozero,Ns,Nh,Inits}, Regs) ->
    [{allocate_heap,Ns,Nh,Regs}|Inits].
