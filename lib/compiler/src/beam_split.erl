%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(beam_split).
-export([module/2]).

-import(lists, [reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [split_blocks(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% We must split the basic block when we encounter instructions with labels,
%% such as catches and BIFs. All labels must be visible outside the blocks.

split_blocks({function,Name,Arity,CLabel,Is0}) ->
    Is = split_blocks(Is0, []),
    {function,Name,Arity,CLabel,Is}.

split_blocks([{block,Bl}|Is], Acc0) ->
    Acc = split_block(Bl, [], Acc0),
    split_blocks(Is, Acc);
split_blocks([I|Is], Acc) ->
    split_blocks(Is, [I|Acc]);
split_blocks([], Acc) -> reverse(Acc).

split_block([{set,[R],[_,_,_]=As,{bif,is_record,{f,Lbl}}}|Is], Bl, Acc) ->
    %% is_record/3 must be translated by beam_clean; therefore,
    %% it must be outside of any block.
    split_block(Is, [], [{bif,is_record,{f,Lbl},As,R}|make_block(Bl, Acc)]);
split_block([{set,[R],As,{bif,N,{f,Lbl}=Fail}}|Is], Bl, Acc) when Lbl =/= 0 ->
    split_block(Is, [], [{bif,N,Fail,As,R}|make_block(Bl, Acc)]);
split_block([{set,[],[],{line,_}=Line},
             {set,[R],As,{bif,raise,{f,_}=Fail}}|Is], Bl, Acc) ->
    split_block(Is, [], [{bif,raise,Fail,As,R},Line|make_block(Bl, Acc)]);
split_block([{set,[R],As,{alloc,Live,{gc_bif,N,{f,Lbl}=Fail}}}|Is], Bl, Acc)
  when Lbl =/= 0 ->
    split_block(Is, [], [{gc_bif,N,Fail,Live,As,R}|make_block(Bl, Acc)]);
split_block([{set,[D],[S|Puts],{alloc,R,{put_map,Op,{f,Lbl}=Fail}}}|Is],
	    Bl, Acc) when Lbl =/= 0 ->
    split_block(Is, [], [{put_map,Fail,Op,S,D,R,{list,Puts}}|
			 make_block(Bl, Acc)]);
split_block([{set,[R],[],{try_catch,Op,L}}|Is], Bl, Acc) ->
    split_block(Is, [], [{Op,R,L}|make_block(Bl, Acc)]);
split_block([I|Is], Bl, Acc) ->
    split_block(Is, [I|Bl], Acc);
split_block([], Bl, Acc) -> make_block(Bl, Acc).

make_block([], Acc) -> Acc;
make_block([{set,[D],Ss,{bif,Op,Fail}}|Bl]=Bl0, Acc) ->
    %% If the last instruction in the block is a comparison or boolean operator
    %% (such as '=:='), move it out of the block to facilitate further
    %% optimizations.
    Arity = length(Ss),
    case erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity) orelse
	erl_internal:bool_op(Op, Arity) of
	false ->
	    [{block,reverse(Bl0)}|Acc];
	true ->
	    I = {bif,Op,Fail,Ss,D},
	    case Bl =:= [] of
		true -> [I|Acc];
		false -> [I,{block,reverse(Bl)}|Acc]
	    end
    end;
make_block([{set,[Dst],[Src],move}|Bl], Acc) ->
    %% Make optimization of {move,Src,Dst}, {jump,...} possible.
    I = {move,Src,Dst},
    case Bl =:= [] of
	true -> [I|Acc];
	false -> [I,{block,reverse(Bl)}|Acc]
    end;
make_block(Bl, Acc) -> [{block,reverse(Bl)}|Acc].
