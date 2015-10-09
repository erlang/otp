%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: beam_flatten.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
%% Purpose : Converts intermediate assembly code to final format.

-module(beam_flatten).

-export([module/2]).
-import(lists, [reverse/1,reverse/2,map/2]).

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    Is1 = block(Is0),
    Is = opt(Is1),
    {function,Name,Arity,CLabel,Is}.

block(Is) ->
    block(Is, []).

block([{block,Is0}|Is1], Acc) -> block(Is1, norm_block(Is0, Acc));
block([I|Is], Acc) -> block(Is, [I|Acc]);
block([], Acc) -> reverse(Acc).

norm_block([{allocate,R,Alloc}|Is], Acc0) ->
    case insert_alloc_in_bs_init(Acc0, Alloc) of
	not_possible ->
	    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc0));
	Acc ->
	    norm_block(Is, Acc)
    end;
norm_block([I|Is], Acc) -> norm_block(Is, [norm(I)|Acc]);
norm_block([], Acc) -> Acc.

norm({set,[D],As,{bif,N}})        -> {bif,N,nofail,As,D};
norm({set,[D],As,{bif,N,F}})      -> {bif,N,F,As,D};
norm({set,[D],[S],move})          -> {move,S,D};
norm({set,[D],[S],fmove})         -> {fmove,S,D};
norm({set,[D],[S],fconv})         -> {fconv,S,D};
norm({set,[D],[S1,S2],put_list})  -> {put_list,S1,S2,D};
norm({set,[D],[],{put_tuple,A}})  -> {put_tuple,A,D};
norm({set,[],[S],put})            -> {put,S};
norm({set,[D],[],{put_string,L,S}})       -> {put_string,L,S,D};
norm({set,[D],[S],{get_tuple_element,I}}) -> {get_tuple_element,S,I,D};
norm({set,[],[S,D],{set_tuple_element,I}}) -> {set_tuple_element,S,D,I};
norm({set,[D1,D2],[S],get_list})          -> {get_list,S,D1,D2};
norm({set,[],[],remove_message})   -> remove_message;
norm({set,[],[],fclearerror}) -> fclearerror;
norm({set,[],[],fcheckerror}) -> {fcheckerror,{f,0}};
norm({'%',_}=Comment)   -> Comment;
norm({'%live',R})                  -> {'%live',R}.

norm_allocate({_Zero,nostack,Nh,[]}, Regs) ->
    [{test_heap,Nh,Regs}];
norm_allocate({_Zero,nostack,Nh,Nf,[]}, Regs) ->
    [{test_heap,alloc_list(Nh, Nf),Regs}];
norm_allocate({zero,0,Nh,[]}, Regs) ->
    norm_allocate({nozero,0,Nh,[]}, Regs);
norm_allocate({zero,0,Nh,Nf,[]}, Regs) ->
    norm_allocate({nozero,0,Nh,Nf,[]}, Regs);
norm_allocate({zero,Ns,0,[]}, Regs) ->
    [{allocate_zero,Ns,Regs}];
norm_allocate({zero,Ns,Nh,[]}, Regs) ->
    [{allocate_heap_zero,Ns,Nh,Regs}];
norm_allocate({nozero,Ns,0,Inits}, Regs) ->
    [{allocate,Ns,Regs}|Inits];
norm_allocate({nozero,Ns,Nh,Inits}, Regs) ->
    [{allocate_heap,Ns,Nh,Regs}|Inits];
norm_allocate({nozero,Ns,Nh,Floats,Inits}, Regs) ->
    [{allocate_heap,Ns,alloc_list(Nh, Floats),Regs}|Inits];
norm_allocate({zero,Ns,Nh,Floats,Inits}, Regs) ->
    [{allocate_heap_zero,Ns,alloc_list(Nh, Floats),Regs}|Inits].

insert_alloc_in_bs_init([I|_]=Is, Alloc) ->
    case is_bs_put(I) of
	false ->
	    not_possible;
	true ->
	    insert_alloc_1(Is, Alloc, [])
    end.

insert_alloc_1([{bs_init2,Fail,Bs,Ws,Regs,F,Dst}|Is], {_,nostack,Nh,Nf,[]}, Acc) ->
    Al = alloc_list(Ws+Nh, Nf),
    I = {bs_init2,Fail,Bs,Al,Regs,F,Dst},
    reverse(Acc, [I|Is]);
insert_alloc_1([I|Is], Alloc, Acc) ->
    insert_alloc_1(Is, Alloc, [I|Acc]).

is_bs_put({bs_put_integer,_,_,_,_,_}) -> true;
is_bs_put({bs_put_float,_,_,_,_,_}) -> true;
is_bs_put({bs_put_binary,_,_,_,_,_}) -> true;
is_bs_put({bs_put_string,_,_}) -> true;
is_bs_put(_) -> false.

alloc_list(Words, Floats) ->
    {alloc,[{words,Words},{floats,Floats}]}.


%% opt(Is0) -> Is
%%  Simple peep-hole optimization to move a {move,Any,{x,0}} past
%%  any kill up to the next call instruction.

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

move_past_kill([{'%live',_}|Is], Move, Acc) ->
    move_past_kill(Is, Move, Acc);
move_past_kill([{kill,Src}|_], {move,Src,_}, _) ->
    impossible;
move_past_kill([{kill,_}=I|Is], Move, Acc) ->
    move_past_kill(Is, Move, [I|Acc]);
move_past_kill(Is, Move, Acc) ->
    {Is,[Move|Acc]}.
