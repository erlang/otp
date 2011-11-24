%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose : Converts intermediate assembly code to final format.

-module(beam_flatten).

-export([module/2]).

-import(lists, [reverse/1,reverse/2]).

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,[function(F) || F <- Fs],Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    Is1 = block(Is0),
    Is = opt(Is1),
    {function,Name,Arity,CLabel,Is}.

block(Is) ->
    block(Is, []).

block([{block,Is0}|Is1], Acc) -> block(Is1, norm_block(Is0, Acc));
block([I|Is], Acc) -> block(Is, [I|Acc]);
block([], Acc) -> reverse(Acc).

norm_block([{set,[],[],{alloc,R,Alloc}}|Is], Acc0) ->
    case insert_alloc_in_bs_init(Acc0, Alloc) of
	impossible ->
	    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc0));
	Acc ->
	    norm_block(Is, Acc)
    end;
norm_block([I|Is], Acc) -> norm_block(Is, [norm(I)|Acc]);
norm_block([], Acc) -> Acc.
    
norm({set,[D],As,{bif,N,F}})      -> {bif,N,F,As,D};
norm({set,[D],As,{alloc,R,{gc_bif,N,F}}}) -> {gc_bif,N,F,R,As,D};
norm({set,[D],[S],move})          -> {move,S,D};
norm({set,[D],[S],fmove})         -> {fmove,S,D};
norm({set,[D],[S],fconv})         -> {fconv,S,D};
norm({set,[D],[S1,S2],put_list})  -> {put_list,S1,S2,D};
norm({set,[D],[],{put_tuple,A}})  -> {put_tuple,A,D};
norm({set,[],[S],put})            -> {put,S};
norm({set,[D],[S],{get_tuple_element,I}}) -> {get_tuple_element,S,I,D};
norm({set,[],[S,D],{set_tuple_element,I}}) -> {set_tuple_element,S,D,I};
norm({set,[D1,D2],[S],get_list})          -> {get_list,S,D1,D2};
norm({set,[],[],remove_message})   -> remove_message;
norm({set,[],[],fclearerror}) -> fclearerror;
norm({set,[],[],fcheckerror}) -> {fcheckerror,{f,0}}.

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
%%   A bs_init2/6 instruction should not be followed by a test heap instruction.
%%   Given the AllocationInfo from a test heap instruction, merge the
%%   allocation amounts into the previous bs_init2/6 instruction (if any).
%%
insert_alloc_in_bs_init([I|_]=Is, Alloc) ->
    case is_bs_constructor(I) of
	false -> impossible;
	true -> insert_alloc_1(Is, Alloc, [])
    end.

insert_alloc_1([{bs_init2=Op,Fail,Bs,Ws1,Regs,F,Dst}|Is], {_,nostack,Ws2,[]}, Acc) ->
    Al = beam_utils:combine_heap_needs(Ws1, Ws2),
    I = {Op,Fail,Bs,Al,Regs,F,Dst},
    reverse(Acc, [I|Is]);
insert_alloc_1([{bs_init_bits=Op,Fail,Bs,Ws1,Regs,F,Dst}|Is], {_,nostack,Ws2,[]}, Acc) ->
    Al = beam_utils:combine_heap_needs(Ws1, Ws2),
    I = {Op,Fail,Bs,Al,Regs,F,Dst},
    reverse(Acc, [I|Is]);
insert_alloc_1([{bs_append,Fail,Sz,Ws1,Regs,U,Bin,Fl,Dst}|Is],
	       {_,nostack,Ws2,[]}, Acc) ->
    Al = beam_utils:combine_heap_needs(Ws1, Ws2),
    I = {bs_append,Fail,Sz,Al,Regs,U,Bin,Fl,Dst},
    reverse(Acc, [I|Is]);
insert_alloc_1([I|Is], Alloc, Acc) ->
    insert_alloc_1(Is, Alloc, [I|Acc]).


%% is_bs_constructor(Instruction) -> true|false.
%%  Test whether the instruction is a bit syntax construction
%%  instruction that can occur at the end of a bit syntax
%%  construction. (Since an empty binary would be expressed
%%  as a literal, the bs_init2/6 instruction will not occur
%%  at the end and therefore it is no need to test for it here.)
%%
is_bs_constructor({bs_put_integer,_,_,_,_,_}) -> true;
is_bs_constructor({bs_put_utf8,_,_,_}) -> true;
is_bs_constructor({bs_put_utf16,_,_,_}) -> true;
is_bs_constructor({bs_put_utf32,_,_,_}) -> true;
is_bs_constructor({bs_put_float,_,_,_,_,_}) -> true;
is_bs_constructor({bs_put_binary,_,_,_,_,_}) -> true;
is_bs_constructor({bs_put_string,_,_}) -> true;
is_bs_constructor(_) -> false.

%% opt(Is0) -> Is
%%  Simple peep-hole optimization to move a {move,Any,{x,0}} past
%%  any kill up to the next call instruction. (To give the loader
%%  an opportunity to combine the 'move' and the 'call' instructions.)
%%
opt(Is) ->
    opt_1(Is, []).

opt_1([{move,_,{x,0}}=I|Is0], Acc0) ->
    case move_past_kill(Is0, I, Acc0) of
	impossible -> opt_1(Is0, [I|Acc0]);
	{Is,Acc} -> opt_1(Is, Acc)
    end;
opt_1([I|Is], Acc) ->
    opt_1(Is, [I|Acc]);
opt_1([], Acc) -> reverse(Acc).

move_past_kill([{kill,Src}|_], {move,Src,_}, _) ->
    impossible;
move_past_kill([{kill,_}=I|Is], Move, Acc) ->
    move_past_kill(Is, Move, [I|Acc]);
move_past_kill([{trim,N,_}=I|Is], {move,Src,Dst}=Move, Acc) ->
    case Src of
	{y,Y} when Y < N-> impossible;
	{y,Y} -> {Is,[{move,{y,Y-N},Dst},I|Acc]};
	_ -> {Is,[Move,I|Acc]}
    end;
move_past_kill(Is, Move, Acc) ->
    {Is,[Move|Acc]}.
