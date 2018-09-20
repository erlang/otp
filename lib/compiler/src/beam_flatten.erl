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

norm_block([{set,[],[],{alloc,R,{_,nostack,_,_}=Alloc}}|Is], Acc0) ->
    case insert_alloc_in_bs_init(Acc0, Alloc) of
	impossible ->
	    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc0));
	Acc ->
	    norm_block(Is, Acc)
    end;
norm_block([{set,[],[],{alloc,R,Alloc}}|Is], Acc0) ->
    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc0));
norm_block([{set,[D1],[S],get_hd},{set,[D2],[S],get_tl}|Is], Acc) ->
    I = {get_list,S,D1,D2},
    norm_block(Is, [I|Acc]);
norm_block([I|Is], Acc) -> norm_block(Is, [norm(I)|Acc]);
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

%% insert_alloc_in_bs_init(ReverseInstructionStream, AllocationInfo) ->
%%                                  impossible | ReverseInstructionStream'
%%   A bs_init/6 instruction should not be followed by a test heap instruction.
%%   Given the AllocationInfo from a test heap instruction, merge the
%%   allocation amounts into the previous bs_init/6 instruction (if any).
%%
insert_alloc_in_bs_init([{bs_put,_,_,_}=I|Is], Alloc) ->
    %% The instruction sequence ends with an bs_put/4 instruction.
    %% We'll need to search backwards for the bs_init/6 instruction.
    insert_alloc_1(Is, Alloc, [I]);
insert_alloc_in_bs_init(_, _) -> impossible.

insert_alloc_1([{bs_init=Op,Fail,Info0,Live,Ss,Dst}|Is],
	       {_,nostack,Ws2,[]}, Acc) when is_integer(Live) ->
    %% The number of extra heap words is always in the second position
    %% in the Info tuple.
    Ws1 = element(2, Info0),
    Al = beam_utils:combine_heap_needs(Ws1, Ws2),
    Info = setelement(2, Info0, Al),
    I = {Op,Fail,Info,Live,Ss,Dst},
    reverse(Acc, [I|Is]);
insert_alloc_1([{bs_put,_,_,_}=I|Is], Alloc, Acc) ->
    insert_alloc_1(Is, Alloc, [I|Acc]).
