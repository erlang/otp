%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

-module(beam_dead).

-export([module/2]).

%%% Dead code is code that is executed but has no effect. This
%%% optimization pass either removes dead code or jumps around it,
%%% potentially making it unreachable and a target for the
%%% the beam_jump pass.

-import(lists, [mapfoldl/3,reverse/1]).

module({Mod,Exp,Attr,Fs0,_}, _Opts) ->
    {Fs1,Lc1} = beam_clean:clean_labels(Fs0),
    {Fs,Lc} = mapfoldl(fun function/2, Lc1, Fs1),
    %%{Fs,Lc} = {Fs1,Lc1},
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, Lc0) ->
    try
	Is1 = beam_jump:remove_unused_labels(Is0),

	%% Initialize label information with the code
	%% for the func_info label. Without it, a register
	%% may seem to be live when it is not.
	[{label,L}|FiIs] = Is1,
	D0 = beam_utils:empty_label_index(),
	D = beam_utils:index_label(L, FiIs, D0),

	%% Optimize away dead code.
	{Is2,Lc} = forward(Is1, Lc0),
	Is3 = backward(Is2, D),
	Is = move_move_into_block(Is3, []),
	{{function,Name,Arity,CLabel,Is},Lc}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%% 'move' instructions outside of blocks may thwart the jump optimizer.
%% Move them back into the block.

move_move_into_block([{block,Bl0},{move,S,D}|Is], Acc) ->
    Bl = Bl0 ++ [{set,[D],[S],move}],
    move_move_into_block([{block,Bl}|Is], Acc);
move_move_into_block([{move,S,D}|Is], Acc) ->
    Bl = [{set,[D],[S],move}],
    move_move_into_block([{block,Bl}|Is], Acc);
move_move_into_block([I|Is], Acc) ->
    move_move_into_block(Is, [I|Acc]);
move_move_into_block([], Acc) -> reverse(Acc).

%%%
%%% Scan instructions in execution order and remove redundant 'move'
%%% instructions. 'move' instructions are redundant if we know that
%%% the register already contains the value being assigned, as in the
%%% following code:
%%%
%%%           test is_eq_exact SomeLabel Src Dst
%%%           move Src Dst
%%%
%%% or in:
%%%
%%%           test is_nil SomeLabel Dst
%%%           move nil Dst
%%%
%%% or in:
%%%
%%%           select_val Register FailLabel [... Literal => L1...]
%%%                      .
%%%                      .
%%%                      .
%%%   L1:     move Literal Register
%%%
%%% Also add extra labels to help the second backward pass.
%%%

forward(Is, Lc) ->
    forward(Is, #{}, Lc, []).

forward([{move,_,_}=Move|[{label,L}|_]=Is], D, Lc, Acc) ->
    %% move/2 followed by jump/1 is optimized by backward/3.
    forward([Move,{jump,{f,L}}|Is], D, Lc, Acc);
forward([{bif,_,_,_,_}=Bif|[{label,L}|_]=Is], D, Lc, Acc) ->
    %% bif/4 followed by jump/1 is optimized by backward/3.
    forward([Bif,{jump,{f,L}}|Is], D, Lc, Acc);
forward([{block,[]}|Is], D, Lc, Acc) ->
    %% Empty blocks can prevent optimizations.
    forward(Is, D, Lc, Acc);
forward([{select,select_val,Reg,_,List}=I|Is], D0, Lc, Acc) ->
    D = update_value_dict(List, Reg, D0),
    forward(Is, D, Lc, [I|Acc]);
forward([{label,Lbl}=LblI,{block,[{set,[Dst],[Lit],move}|BlkIs]}=Blk|Is], D, Lc, Acc) ->
    %% Assumption: The target labels in a select_val/3 instruction
    %% cannot be reached in any other way than through the select_val/3
    %% instruction (i.e. there can be no fallthrough to such label and
    %% it cannot be referenced by, for example, a jump/1 instruction).
    Key = {Lbl,Dst},
    Block = case D of
                #{Key := Lit} -> {block,BlkIs}; %Safe to remove move instruction.
                _ -> Blk                        %Must keep move instruction.
            end,
    forward([Block|Is], D, Lc, [LblI|Acc]);
forward([{label,Lbl}=LblI|[{move,Lit,Dst}|Is1]=Is0], D, Lc, Acc) ->
    %% Assumption: The target labels in a select_val/3 instruction
    %% cannot be reached in any other way than through the select_val/3
    %% instruction (i.e. there can be no fallthrough to such label and
    %% it cannot be referenced by, for example, a jump/1 instruction).
    Is = case maps:find({Lbl,Dst}, D) of
	     {ok,Lit} -> Is1;     %Safe to remove move instruction.
	     _ -> Is0		  %Keep move instruction.
	 end,
    forward(Is, D, Lc, [LblI|Acc]);
forward([{test,is_eq_exact,_,[Same,Same]}|Is], D, Lc, Acc) ->
    forward(Is, D, Lc, Acc);
forward([{test,is_eq_exact,_,[Dst,Src]}=I,
	 {block,[{set,[Dst],[Src],move}|Bl]}|Is], D, Lc, Acc) ->
    forward([I,{block,Bl}|Is], D, Lc, Acc);
forward([{test,is_nil,_,[Dst]}=I,
	 {block,[{set,[Dst],[nil],move}|Bl]}|Is], D, Lc, Acc) ->
    forward([I,{block,Bl}|Is], D, Lc, Acc);
forward([{test,is_eq_exact,_,[Dst,Src]}=I,{move,Src,Dst}|Is], D, Lc, Acc) ->
    forward([I|Is], D, Lc, Acc);
forward([{test,is_nil,_,[Dst]}=I,{move,nil,Dst}|Is], D, Lc, Acc) ->
    forward([I|Is], D, Lc, Acc);
forward([{test,_,_,_}=I|Is]=Is0, D, Lc, Acc) ->
    %% Help the second, backward pass to by inserting labels after
    %% relational operators so that they can be skipped if they are
    %% known to be true.
    case useful_to_insert_label(Is0) of
	false -> forward(Is, D, Lc, [I|Acc]);
	true -> forward(Is, D, Lc+1, [{label,Lc},I|Acc])
    end;
forward([I|Is], D, Lc, Acc) ->
    forward(Is, D, Lc, [I|Acc]);
forward([], _, Lc, Acc) -> {Acc,Lc}.

update_value_dict([Lit,{f,Lbl}|T], Reg, D0) ->
    Key = {Lbl,Reg},
    D = case D0 of
            #{Key := inconsistent} -> D0;
            #{Key := _} -> D0#{Key := inconsistent};
            _ -> D0#{Key => Lit}
        end,
    update_value_dict(T, Reg, D);
update_value_dict([], _, D) -> D.

useful_to_insert_label([_,{label,_}|_]) ->
    false;
useful_to_insert_label([{test,Op,_,_}|_]) ->
    case Op of
	is_lt -> true;
	is_ge -> true;
	is_eq_exact -> true;
	is_ne_exact -> true;
	_ -> false
    end.

%%%
%%% Scan instructions in reverse execution order and try to
%%% shortcut branch instructions.
%%%
%%% For example, in this code:
%%%
%%%             move Literal Register
%%%             jump L1
%%%                .
%%%                .
%%%                .
%%%     L1:     test is_{integer,atom} FailLabel Register
%%%             select_val {x,0} FailLabel [... Literal => L2...]
%%%                .
%%%                .
%%%                .
%%%     L2:        ...
%%%
%%% the 'selectval' instruction will always transfer control to L2,
%%% so we can just as well jump to L2 directly by rewriting the
%%% first part of the sequence like this:
%%%
%%%           move Literal Register
%%%           jump L2
%%%
%%% If register Register is killed at label L2, we can remove the
%%% 'move' instruction, leaving just the 'jump' instruction:
%%%
%%%           jump L2
%%%
%%% These transformations may leave parts of the code unreachable.
%%% The beam_jump pass will remove the unreachable code.

backward(Is, D) ->
    backward(Is, D, []).

backward([{test,is_eq_exact,Fail,[Dst,{integer,Arity}]}=I|
	  [{bif,tuple_size,Fail,[Reg],Dst}|Is]=Is0], D, Acc) ->
    %% Provided that Dst is killed following this sequence,
    %% we can rewrite the instructions like this:
    %%
    %% bif tuple_size Fail Reg Dst  ==>  is_tuple Fail Reg
    %% is_eq_exact Fail Dst Integer    	 test_arity Fail Reg Integer
    %%
    %% (still two instructions, but they they will be combined to
    %% one by the loader).
    case beam_utils:is_killed(Dst, Acc, D) andalso (Arity bsr 32) =:= 0 of
	false ->
	    %% Not safe because the register Dst is not killed
	    %% (probably cannot not happen in practice) or the arity
	    %% does not fit in 32 bits (the loader will fail to load
	    %% the module). We must move the first instruction to the
	    %% accumulator to avoid an infinite loop.
	    backward(Is0, D, [I|Acc]);
	true ->
	    %% Safe.
	    backward([{test,test_arity,Fail,[Reg,Arity]},
		      {test,is_tuple,Fail,[Reg]}|Is], D, Acc)
    end;
backward([{label,Lbl}=L|Is], D, Acc) ->
    backward(Is, beam_utils:index_label(Lbl, Acc, D), [L|Acc]);
backward([{select,select_val,Reg,{f,Fail0},List0}|Is], D, Acc) ->
    List1 = shortcut_select_list(List0, Reg, D, []),
    Fail1 = shortcut_label(Fail0, D),
    Fail = shortcut_bs_test(Fail1, Is, D),
    List = prune_redundant(List1, Fail),
    case List of
	[] ->
	    Jump = {jump,{f,Fail}},
	    backward([Jump|Is], D, Acc);
	[V,F] ->
	    Test = {test,is_eq_exact,{f,Fail},[Reg,V]},
	    Jump = {jump,F},
	    backward([Jump,Test|Is], D, Acc);
	[{atom,B1},F,{atom,B2},F] when B1 =:= not B2 ->
	    Test = {test,is_boolean,{f,Fail},[Reg]},
	    Jump = {jump,F},
	    backward([Jump,Test|Is], D, Acc);
	[_|_] ->
	    Sel = {select,select_val,Reg,{f,Fail},List},
	    backward(Is, D, [Sel|Acc])
    end;
backward([{jump,{f,To0}},{move,Src,Reg}=Move|Is], D, Acc) ->
    To = shortcut_select_label(To0, Reg, Src, D),
    Jump = {jump,{f,To}},
    case is_killed_at(Reg, To, D) of
	false -> backward([Move|Is], D, [Jump|Acc]);
	true -> backward([Jump|Is], D, Acc)
    end;
backward([{jump,{f,To}}=J|[{bif,Op,_,Ops,Reg}|Is]=Is0], D, Acc) ->
    try replace_comp_op(To, Reg, Op, Ops, D) of
	I -> backward(Is, D, I++Acc)
    catch
	throw:not_possible -> backward(Is0, D, [J|Acc])
    end;
backward([{test,bs_start_match2,F,_,[R,_],Ctxt}=I|Is], D,
	 [{test,bs_match_string,F,[Ctxt,Bs]},
	  {test,bs_test_tail2,F,[Ctxt,0]}|Acc0]=Acc) ->
    case beam_utils:is_killed(Ctxt, Acc0, D) of
	true ->
	    Eq = {test,is_eq_exact,F,[R,{literal,Bs}]},
	    backward(Is, D, [Eq|Acc0]);
	false ->
	    backward(Is, D, [I|Acc])
    end;
backward([{test,bs_start_match2,{f,To0},Live,[Src|_]=Info,Dst}|Is], D, Acc) ->
    To = shortcut_bs_start_match(To0, Src, D),
    I = {test,bs_start_match2,{f,To},Live,Info,Dst},
    backward(Is, D, [I|Acc]);
backward([{test,Op,{f,To0},Ops0}|Is], D, Acc) ->
    To1 = shortcut_bs_test(To0, Is, D),
    To2 = shortcut_label(To1, D),
    To3 = shortcut_rel_op(To2, Op, Ops0, D),

    %% Try to shortcut a repeated test:
    %%
    %%        test Op {f,Fail1} Operands	test Op {f,Fail2} Operands
    %%        . . .		          ==>   ...
    %% Fail1: test Op {f,Fail2} Operands        Fail1: test Op {f,Fail2} Operands
    %%
    To = case beam_utils:code_at(To3, D) of
	     [{test,Op,{f,To4},Ops}|_] ->
		 case equal_ops(Ops0, Ops) of
		     true -> To4;
		     false -> To3
		 end;
	     _Code ->
		 To3
	 end,
    I = case Op of
	    is_eq_exact -> combine_eqs(To, Ops0, D, Acc);
	    _ -> {test,Op,{f,To},Ops0}
	end,
    case {I,Acc} of
	{{test,is_atom,Fail,Ops0},[{test,is_boolean,Fail,Ops0}|_]} ->
	    %% An is_atom test before an is_boolean test (with the
	    %% same failure label) is redundant.
	    backward(Is, D, Acc);
	{{test,is_atom,Fail,[R]},
	 [{test,is_eq_exact,Fail,[R,{atom,_}]}|_]} ->
	    %% An is_atom test before a comparison with an atom (with
	    %% the same failure label) is redundant.
	    backward(Is, D, Acc);
	{{test,is_integer,Fail,[R]},
	 [{test,is_eq_exact,Fail,[R,{integer,_}]}|_]} ->
	    %% An is_integer test before a comparison with an integer
	    %% (with the same failure label) is redundant.
	    backward(Is, D, Acc);
	{{test,_,_,_},_} ->
	    %% Still a test instruction. Done.
	    backward(Is, D, [I|Acc]);
	{_,_} ->
	    %% Rewritten to a select_val. Rescan.
	    backward([I|Is], D, Acc)
    end;
backward([{test,Op,{f,To0},Live,Ops0,Dst}|Is], D, Acc) ->
    To1 = shortcut_bs_test(To0, Is, D),
    To2 = shortcut_label(To1, D),
    %% Try to shortcut a repeated test:
    %%
    %%        test Op {f,Fail1} _ Ops _ 	test Op {f,Fail2} _ Ops _
    %%        . . .		          ==>   ...
    %% Fail1: test Op {f,Fail2} _ Ops _   Fail1: test Op {f,Fail2} _ Ops _
    %%
    To = case beam_utils:code_at(To2, D) of
	     [{test,Op,{f,To3},_,Ops,_}|_] ->
		 case equal_ops(Ops0, Ops) of
		     true -> To3;
		     false -> To2
		 end;
	     _Code ->
		 To2
	 end,
    I = {test,Op,{f,To},Live,Ops0,Dst},
    backward(Is, D, [I|Acc]);
backward([{kill,_}=I|Is], D, [{line,_},Exit|_]=Acc) ->
    case beam_jump:is_exit_instruction(Exit) of
	false -> backward(Is, D, [I|Acc]);
	true -> backward(Is, D, Acc)
    end;
backward([I|Is], D, Acc) ->
    backward(Is, D, [I|Acc]);
backward([], _D, Acc) -> Acc.

equal_ops([{field_flags,FlA0}|T0], [{field_flags,FlB0}|T1]) ->
    FlA = lists:keydelete(anno, 1, FlA0),
    FlB = lists:keydelete(anno, 1, FlB0),
    FlA =:= FlB andalso equal_ops(T0, T1);
equal_ops([Op|T0], [Op|T1]) ->
    equal_ops(T0, T1);
equal_ops([], []) -> true;
equal_ops(_, _) -> false.
    
shortcut_select_list([Lit,{f,To0}|T], Reg, D, Acc) ->
    To = shortcut_select_label(To0, Reg, Lit, D),
    shortcut_select_list(T, Reg, D, [{f,To},Lit|Acc]);
shortcut_select_list([], _, _, Acc) -> reverse(Acc).

shortcut_label(To0, D) ->
    case beam_utils:code_at(To0, D) of
  	[{jump,{f,To}}|_] -> shortcut_label(To, D);
	_ -> To0
    end.

shortcut_select_label(To, Reg, Lit, D) ->
    shortcut_rel_op(To, is_ne_exact, [Reg,Lit], D).

prune_redundant([_,{f,Fail}|T], Fail) ->
    prune_redundant(T, Fail);
prune_redundant([V,F|T], Fail) ->
    [V,F|prune_redundant(T, Fail)];
prune_redundant([], _) -> [].

%% Replace a comparison operator with a test instruction and a jump.
%% For example, if we have this code:
%%
%%     	  bif '=:=' Fail Src1 Src2 {x,0}
%%    	  jump L1
%%           .
%%           .
%%           .
%%   L1:  select_val {x,0} FailLabel [... true => L2..., ...false => L3...]
%%
%% the first two instructions can be replaced with
%%
%%        test is_eq_exact L3 Src1 Src2
%%	  jump L2
%%
%% provided that {x,0} is killed at both L2 and L3.

replace_comp_op(To, Reg, Op, Ops, D) ->
    False = comp_op_find_shortcut(To, Reg, {atom,false}, D),
    True = comp_op_find_shortcut(To, Reg, {atom,true}, D),
    [bif_to_test(Op, Ops, False),{jump,{f,True}}].

comp_op_find_shortcut(To0, Reg, Val, D) ->
    case shortcut_select_label(To0, Reg, Val, D) of
	To0 ->
	    not_possible();
	To ->
	    case is_killed_at(Reg, To, D) of
		false -> not_possible();
		true -> To
	    end
    end.

bif_to_test(Name, Args, Fail) ->
    try
	beam_utils:bif_to_test(Name, Args, {f,Fail})
    catch
	error:_ -> not_possible()
    end.

not_possible() -> throw(not_possible).

%% combine_eqs(To, Operands, Acc) -> Instruction.
%%  Combine two is_eq_exact instructions or (an is_eq_exact
%%  instruction and a select_val instruction) to a select_val
%%  instruction if possible.
%%
%%  Example:
%%
%%      is_eq_exact F1 Reg Lit1		    select_val Reg F2 [ Lit1 L1
%%   L1:                .                                       Lit2 L2 ]
%%                      .
%%                      .	     ==>
%%                      .
%%   F1:  is_eq_exact F2 Reg Lit2          F1: is_eq_exact F2 Reg Lit2
%%   L2:  ....				   L2:
%%
combine_eqs(To, [Reg,{Type,_}=Lit1]=Ops, D, [{label,L1}|_])
  when Type =:= atom; Type =:= integer ->
    case beam_utils:code_at(To, D) of
	[{test,is_eq_exact,{f,F2},[Reg,{Type,_}=Lit2]},
	 {label,L2}|_] when Lit1 =/= Lit2 ->
	    {select,select_val,Reg,{f,F2},[Lit1,{f,L1},Lit2,{f,L2}]};
	[{select,select_val,Reg,{f,F2},[{Type,_}|_]=List0}|_] ->
	    List = remove_from_list(Lit1, List0),
	    {select,select_val,Reg,{f,F2},[Lit1,{f,L1}|List]};
	_Is ->
	    {test,is_eq_exact,{f,To},Ops}
	end;
combine_eqs(To, Ops, _D, _Acc) ->
    {test,is_eq_exact,{f,To},Ops}.

remove_from_list(Lit, [Lit,{f,_}|T]) ->
    T;
remove_from_list(Lit, [Val,{f,_}=Fail|T]) ->
    [Val,Fail|remove_from_list(Lit, T)];
remove_from_list(_, []) -> [].

%% shortcut_bs_test(TargetLabel, ReversedInstructions, D) -> TargetLabel'
%%  Try to shortcut the failure label for bit syntax matching.

shortcut_bs_test(To, Is, D) ->
    shortcut_bs_test_1(beam_utils:code_at(To, D), Is, To, D).

shortcut_bs_test_1([{bs_restore2,Reg,SavePoint},
		    {label,_},
		    {test,bs_test_tail2,{f,To},[_,TailBits]}|_],
		   PrevIs, To0, D) ->
    case count_bits_matched(PrevIs, {Reg,SavePoint}, 0) of
	Bits when Bits > TailBits ->
	    %% This instruction will fail. We know because a restore has been
	    %% done from the previous point SavePoint in the binary, and we
	    %% also know that the binary contains at least Bits bits from
	    %% SavePoint.
	    %%
	    %% Since we will skip a bs_restore2 if we shortcut to label To,
	    %% we must now make sure that code at To does not depend on
	    %% the position in the context in any way.
	    case shortcut_bs_pos_used(To, Reg, D) of
		false -> To;
		true -> To0
	    end;
	_Bits ->
	    To0
    end;
shortcut_bs_test_1([_|_], _, To, _) -> To.

%% counts_bits_matched(ReversedInstructions, SavePoint, Bits) -> Bits'
%%  Given a reversed instruction stream, determine the minimum number
%%  of bits that will be matched by bit syntax instructions up to the
%%  given save point.

count_bits_matched([{test,bs_get_utf8,{f,_},_,_,_}|Is], SavePoint, Bits) ->
    count_bits_matched(Is, SavePoint, Bits+8);
count_bits_matched([{test,bs_get_utf16,{f,_},_,_,_}|Is], SavePoint, Bits) ->
    count_bits_matched(Is, SavePoint, Bits+16);
count_bits_matched([{test,bs_get_utf32,{f,_},_,_,_}|Is], SavePoint, Bits) ->
    count_bits_matched(Is, SavePoint, Bits+32);
count_bits_matched([{test,_,_,_,[_,Sz,U,{field_flags,_}],_}|Is], SavePoint, Bits) ->
    case Sz of
	{integer,N} -> count_bits_matched(Is, SavePoint, Bits+N*U);
	_ -> count_bits_matched(Is, SavePoint, Bits)
    end;
count_bits_matched([{test,bs_match_string,_,[_,Bs]}|Is], SavePoint, Bits) ->
    count_bits_matched(Is, SavePoint, Bits+bit_size(Bs));
count_bits_matched([{test,_,_,_}|Is], SavePoint, Bits) ->
    count_bits_matched(Is, SavePoint, Bits);
count_bits_matched([{bs_save2,Reg,SavePoint}|_], {Reg,SavePoint}, Bits) ->
    %% The save point we are looking for - we are done.
    Bits;
count_bits_matched([_|_], _, Bits) -> Bits.

shortcut_bs_pos_used(To, Reg, D) ->
    shortcut_bs_pos_used_1(beam_utils:code_at(To, D), Reg, D).

shortcut_bs_pos_used_1([{bs_context_to_binary,Reg}|_], Reg, _) ->
    false;
shortcut_bs_pos_used_1(Is, Reg, D) ->
    not beam_utils:is_killed(Reg, Is, D).

%% shortcut_bs_start_match(TargetLabel, Reg) -> TargetLabel
%%  A failing bs_start_match2 instruction means that the source (Reg)
%%  cannot be a binary. That means that it is safe to skip
%%  bs_context_to_binary instructions operating on Reg, and
%%  bs_start_match2 instructions operating on Reg.

shortcut_bs_start_match(To, Reg, D) ->
    shortcut_bs_start_match_1(beam_utils:code_at(To, D), Reg, To, D).

shortcut_bs_start_match_1([{bs_context_to_binary,Reg}|Is], Reg, To, D) ->
    shortcut_bs_start_match_1(Is, Reg, To, D);
shortcut_bs_start_match_1([{jump,{f,To}}|_], Reg, _, D) ->
    Code = beam_utils:code_at(To, D),
    shortcut_bs_start_match_1(Code, Reg, To, D);
shortcut_bs_start_match_1([{test,bs_start_match2,{f,To},_,[Reg|_],_}|_],
			  Reg, _, D) ->
    Code = beam_utils:code_at(To, D),
    shortcut_bs_start_match_1(Code, Reg, To, D);
shortcut_bs_start_match_1(_, _, To, _) ->
    To.

%% shortcut_rel_op(FailLabel, Operator, [Operand], D) -> FailLabel'
%%  Try to shortcut the given test instruction. Example:
%%
%%           is_ge L1 {x,0} 48
%%     .
%%     .
%%     .
%%     L1:   is_ge L2 {x,0} 65
%%
%%  The first test instruction can be rewritten to "is_ge L2 {x,0} 48"
%%  since the instruction at L1 will also fail.
%%
%%  If there are instructions between L1 and the other test instruction
%%  it may still be possible to do the shortcut. For example:
%%
%%     L1:   is_eq_exact L3 {x,0} 92
%%           is_ge L2 {x,0} 65
%%
%%  Since the first test instruction failed, we know that {x,0} must
%%  be less than 48; therefore, we know that {x,0} cannot be equal to
%%  92 and the jump to L3 cannot happen.

shortcut_rel_op(To, Op, Ops, D) ->
    case normalize_op({test,Op,{f,To},Ops}) of
	{{NormOp,A,B},_} ->
	    Normalized = {negate_op(NormOp),A,B},
	    shortcut_rel_op_fp(To, Normalized, D);
	{_,_} ->
	    To;
	error ->
	    To
    end.

shortcut_rel_op_fp(To0, Normalized, D) ->
    Code = beam_utils:code_at(To0, D),
    case shortcut_any_label(Code, Normalized) of
	error ->
	    To0;
	To ->
	    shortcut_rel_op_fp(To, Normalized, D)
    end.

%% shortcut_any_label([Instruction], PrevCondition) -> FailLabel | error
%%  Using PrevCondition (a previous condition known to be true),
%%  try to shortcut to another failure label.

shortcut_any_label([{jump,{f,Lbl}}|_], _Prev) ->
    Lbl;
shortcut_any_label([{label,Lbl}|_], _Prev) ->
    Lbl;
shortcut_any_label([{select,select_val,R,{f,Fail},L}|_], Prev) ->
    shortcut_selectval(L, R, Fail, Prev);
shortcut_any_label([I|Is], Prev) ->
    case normalize_op(I) of
	error ->
	    error;
	{Normalized,Fail} ->
	    %% We have a relational operator.
	    case will_succeed(Prev, Normalized) of
		no ->
		    %% This test instruction will always branch
		    %% to Fail.
		    Fail;
		yes ->
		    %% This test instruction will never branch,
		    %% so we will look at the next instruction.
		    shortcut_any_label(Is, Prev);
		maybe ->
		    %% May or may not branch. From now on, we can only
		    %% shortcut to the this specific failure label
		    %% Fail.
		    shortcut_specific_label(Is, Fail, Prev)
	    end
    end.

%% shortcut_specific_label([Instruction], FailLabel, PrevCondition) ->
%%    FailLabel | error
%%  We have previously encountered a test instruction that may or
%%  may not branch to FailLabel. Therefore we are only allowed
%%  to do the shortcut to the same fail label (FailLabel).

shortcut_specific_label([{label,_}|Is], Fail, Prev) ->
    shortcut_specific_label(Is, Fail, Prev);
shortcut_specific_label([{select,select_val,R,{f,F},L}|_], Fail, Prev) ->
    case shortcut_selectval(L, R, F, Prev) of
	Fail -> Fail;
	_ -> error
    end;
shortcut_specific_label([I|Is], Fail, Prev) ->
    case normalize_op(I) of
	error ->
	    error;
	{Normalized,Fail} ->
	    case will_succeed(Prev, Normalized) of
		no ->
		    %% Will branch to FailLabel.
		    Fail;
		yes ->
		    %% Will definitely never branch.
		    shortcut_specific_label(Is, Fail, Prev);
		maybe ->
		    %% May branch, but still OK since it will branch
		    %% to FailLabel.
		    shortcut_specific_label(Is, Fail, Prev)
	    end;
	{Normalized,_} ->
	    %% This test instruction will branch to a different
	    %% fail label, if it branches at all.
	    case will_succeed(Prev, Normalized) of
		yes ->
		    %% Still OK, since the branch will never be
		    %% taken.
		    shortcut_specific_label(Is, Fail, Prev);
		no ->
		    %% Give up. The branch will definitely be taken
		    %% to a different fail label.
		    error;
		maybe ->
		    %% Give up. If the branch is taken, it will be
		    %% to a different fail label.
		    error
	    end
    end.


%% shortcut_selectval(List, Reg, Fail, PrevCond) -> FailLabel | error
%%  Try to shortcut a selectval instruction. A selectval instruction
%%  is equivalent to the following instruction sequence:
%%
%%      is_ne_exact L1 Reg Value1
%%              .
%%              .
%%              .
%%      is_ne_exact LN Reg ValueN
%%      jump DefaultFailLabel
%%
shortcut_selectval([Val,{f,Lbl}|T], R, Fail, Prev) ->
    case will_succeed(Prev, {'=/=',R,get_literal(Val)}) of
	yes -> shortcut_selectval(T, R, Fail, Prev);
	no -> Lbl;
	maybe -> error
    end;
shortcut_selectval([], _, Fail, _) -> Fail.

%% will_succeed(PrevCondition, Condition) -> yes | no | maybe
%%  PrevCondition is a condition known to be true. This function
%%  will tell whether Condition will succeed.

will_succeed({Op1,Reg,A}, {Op2,Reg,B}) ->
    will_succeed_1(Op1, A, Op2, B);
will_succeed({'=:=',Reg,{literal,A}}, {TypeTest,Reg}) ->
    case erlang:TypeTest(A) of
	false -> no;
	true -> yes
    end;
will_succeed({_,_,_}, maybe) ->
    maybe;
will_succeed({_,_,_}, Test) when is_tuple(Test) ->
    maybe.

will_succeed_1('=:=', A, '<', B) ->
    if
	B =< A -> no;
	true -> yes
    end;
will_succeed_1('=:=', A, '=<', B) ->
    if
	B < A -> no;
	true -> yes
    end;
will_succeed_1('=:=', A, '=:=', B) ->
    if
	A =:= B -> yes;
	true -> no
    end;
will_succeed_1('=:=', A, '=/=', B) ->
    if
	A =:= B -> no;
	true -> yes
    end;
will_succeed_1('=:=', A, '>=', B) ->
    if
	B > A -> no;
	true -> yes
    end;
will_succeed_1('=:=', A, '>', B) ->
    if
	B >= A -> no;
	true -> yes
    end;

will_succeed_1('=/=', A, '=/=', B) when A =:= B -> yes;
will_succeed_1('=/=', A, '=:=', B) when A =:= B -> no;

will_succeed_1('<', A, '=:=', B)  when B >= A -> no;
will_succeed_1('<', A, '=/=', B)  when B >= A -> yes;
will_succeed_1('<', A, '<',   B)  when B >= A -> yes;
will_succeed_1('<', A, '=<',  B)  when B > A  -> yes;
will_succeed_1('<', A, '>=',  B)  when B > A  -> no;
will_succeed_1('<', A, '>',   B)  when B >= A -> no;

will_succeed_1('=<', A, '=:=', B) when B > A  -> no;
will_succeed_1('=<', A, '=/=', B) when B > A  -> yes;
will_succeed_1('=<', A, '<',   B) when B > A  -> yes;
will_succeed_1('=<', A, '=<',  B) when B >= A -> yes;
will_succeed_1('=<', A, '>=',  B) when B > A  -> no;
will_succeed_1('=<', A, '>',   B) when B >= A -> no;

will_succeed_1('>=', A, '=:=', B) when B < A  -> no;
will_succeed_1('>=', A, '=/=', B) when B < A  -> yes;
will_succeed_1('>=', A, '<',   B) when B =< A -> no;
will_succeed_1('>=', A, '=<',  B) when B < A  -> no;
will_succeed_1('>=', A, '>=',  B) when B =< A -> yes;
will_succeed_1('>=', A, '>',   B) when B < A  -> yes;

will_succeed_1('>', A, '=:=', B)  when B =< A -> no;
will_succeed_1('>', A, '=/=', B)  when B =< A -> yes;
will_succeed_1('>', A, '<',   B)  when B =< A -> no;
will_succeed_1('>', A, '=<',  B)  when B < A  -> no;
will_succeed_1('>', A, '>=',  B)  when B =< A -> yes;
will_succeed_1('>', A, '>',   B)  when B < A  -> yes;

will_succeed_1(_, _, _, _) -> maybe.

%% normalize_op(Instruction) -> {Normalized,FailLabel} | error
%%    Normalized = {Operator,Register,Literal} |
%%                 {TypeTest,Register} |
%%                 maybe
%%    Operation = '<' | '=<' | '=:=' | '=/=' | '>=' | '>'
%%    TypeTest = is_atom | is_integer ...
%%    Literal = {literal,Term}
%%
%%  Normalize a relational operator to facilitate further
%%  comparisons between operators. Always make the register
%%  operand the first operand. Thus the following instruction:
%%
%%    {test,is_ge,{f,99},{integer,13},{x,0}}
%%
%%  will be normalized to:
%%
%%    {'=<',{x,0},{literal,13}}
%%
%%  NOTE: Bit syntax test instructions are scary. They may change the
%%  state of match contexts and update registers, so we don't dare
%%  mess with them.

normalize_op({test,is_ge,{f,Fail},Ops}) ->
    normalize_op_1('>=', Ops, Fail);
normalize_op({test,is_lt,{f,Fail},Ops}) ->
    normalize_op_1('<', Ops, Fail);
normalize_op({test,is_eq_exact,{f,Fail},Ops}) ->
    normalize_op_1('=:=', Ops, Fail);
normalize_op({test,is_ne_exact,{f,Fail},Ops}) ->
    normalize_op_1('=/=', Ops, Fail);
normalize_op({test,is_nil,{f,Fail},[R]}) ->
    normalize_op_1('=:=', [R,nil], Fail);
normalize_op({test,Op,{f,Fail},[R]}) ->
    case erl_internal:new_type_test(Op, 1) of
	true -> {{Op,R},Fail};
	false -> {maybe,Fail}
    end;
normalize_op({test,_,{f,Fail},_}=I) ->
    case beam_utils:is_pure_test(I) of
	true -> {maybe,Fail};
	false -> error
    end;
normalize_op(_) ->
    error.

normalize_op_1(Op, [Op1,Op2], Fail) ->
    case {get_literal(Op1),get_literal(Op2)} of
	{error,error} ->
	    %% Both operands are registers.
	    {maybe,Fail};
	{error,Lit} ->
	    {{Op,Op1,Lit},Fail};
	{Lit,error} ->
	    {{turn_op(Op),Op2,Lit},Fail};
	{_,_} ->
	    %% Both operands are literals. Can probably only
	    %% happen if the Core Erlang optimizations passes were
	    %% turned off, so don't bother trying to do something
	    %% smart here.
	    {maybe,Fail}
    end.

turn_op('<') -> '>';
turn_op('>=') -> '=<';
turn_op('=:='=Op) -> Op;
turn_op('=/='=Op) -> Op.

negate_op('>=') -> '<';
negate_op('<') -> '>=';
negate_op('=<') -> '>';
negate_op('>') -> '=<';
negate_op('=:=') -> '=/=';
negate_op('=/=') -> '=:='.

get_literal({atom,Val}) ->
    {literal,Val};
get_literal({integer,Val}) ->
    {literal,Val};
get_literal({float,Val}) ->
    {literal,Val};
get_literal(nil) ->
    {literal,[]};
get_literal({literal,_}=Lit) ->
    Lit;
get_literal({_,_}) -> error.


%%%
%%% Removing stores to Y registers is not always safe
%%% if there is an instruction that causes an exception
%%% within a catch. In practice, there are few or no
%%% opportunities for removing stores to Y registers anyway
%%% if sys_core_fold has been run.
%%%

is_killed_at({x,_}=Reg, Lbl, D) ->
    beam_utils:is_killed_at(Reg, Lbl, D);
is_killed_at({y,_}, _, _) ->
    false.
