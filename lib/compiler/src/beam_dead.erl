%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

-module(beam_dead).

-export([module/2]).

%%% The following optimisations are done:
%%%
%%% (1) In this code
%%%
%%%     	move DeadValue {x,0}
%%%     	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	move Anything {x,0}
%%%        .
%%%        .
%%%        .
%%%
%%%     the first assignment to {x,0} has no effect (is dead),
%%%     so it can be removed. Besides removing a move instruction,
%%%     if the move was preceeded by a label, the resulting code
%%%	will look this
%%%
%%%     L1:	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	move Anything {x,0}
%%%        .
%%%        .
%%%        .
%%%
%%%	which can be further optimized by the jump optimizer (beam_jump).
%%%
%%% (2) In this code
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	test is_atom FailLabel {x,0}
%%%    		select_val {x,0}, FailLabel [... AtomLiteral => L3...]
%%%        .
%%%        .
%%%        .
%%%	L3:	...
%%%
%%%     FailLabel: ...
%%%
%%%	the first code fragment can be changed to
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump L3
%%%
%%%     If the literal is not included in the table of literals in the
%%%     select_val instruction, the first code fragment will instead be
%%%     rewritten as:
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump FailLabel
%%%
%%%	The move instruction will be removed by optimization (1) above,
%%%	if the code following the L3 label overwrites {x,0}.
%%%
%%% 	The code following the L2 label will be kept, but it will be removed later
%%%	by the jump optimizer.
%%%
%%% (3) In this code
%%%
%%%     	test is_eq_exact ALabel Src Dst
%%%     	move Src Dst
%%%
%%%	the move instruction can be removed.
%%%     Same thing for
%%%
%%%     	test is_nil ALabel Dst
%%%     	move [] Dst
%%%
%%%
%%% (4) In this code
%%%
%%%    		select_val {x,Reg}, ALabel [... Literal => L1...]
%%%        .
%%%        .
%%%        .
%%%	L1:	move Literal {x,Reg}
%%%
%%%     we can remove the move instruction.
%%%
%%% (5) In the following code
%%%
%%%     	bif '=:=' Fail Src1 Src2 {x,0}
%%%    		jump L1
%%%	   .
%%%	   .
%%%	   .
%%%        L1:	select_val {x,0}, ALabel [... true => L2..., ...false => L3...]
%%%	   .
%%%	   .
%%%	   .
%%%        L2: ....      L3: ....
%%%
%%%  the first two instructions can be replaced with
%%%
%%%		test is_eq_exact L3 Src1 Src2
%%%		jump L2
%%%
%%%  provided that {x,0} is killed at both L2 and L3.
%%%

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
%%% Scan instructions in execution order and remove dead code.
%%%

forward(Is, Lc) ->
    forward(Is, gb_trees:empty(), Lc, []).

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
    Block = case gb_trees:lookup({Lbl,Dst}, D) of
		{value,Lit} -> {block,BlkIs}; %Safe to remove move instruction.
		_ -> Blk		      %Must keep move instruction.
	    end,
    forward([Block|Is], D, Lc, [LblI|Acc]);
forward([{label,Lbl}=LblI|[{move,Lit,Dst}|Is1]=Is0], D, Lc, Acc) ->
    %% Assumption: The target labels in a select_val/3 instruction
    %% cannot be reached in any other way than through the select_val/3
    %% instruction (i.e. there can be no fallthrough to such label and
    %% it cannot be referenced by, for example, a jump/1 instruction).
    Is = case gb_trees:lookup({Lbl,Dst}, D) of
	     {value,Lit} -> Is1;     %Safe to remove move instruction.
	     _ -> Is0		     %Keep move instruction.
	 end,
    forward(Is, D, Lc, [LblI|Acc]);
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
forward([{test,is_eq_exact,_,_}=I|Is], D, Lc, Acc) ->
    case Is of
	[{label,_}|_] -> forward(Is, D, Lc, [I|Acc]);
	_ -> forward(Is, D, Lc+1, [{label,Lc},I|Acc])
    end;
forward([{test,is_ne_exact,_,_}=I|Is], D, Lc, Acc) ->
    case Is of
	[{label,_}|_] -> forward(Is, D, Lc, [I|Acc]);
	_ -> forward(Is, D, Lc+1, [{label,Lc},I|Acc])
    end;
forward([I|Is], D, Lc, Acc) ->
    forward(Is, D, Lc, [I|Acc]);
forward([], _, Lc, Acc) -> {Acc,Lc}.

update_value_dict([Lit,{f,Lbl}|T], Reg, D0) ->
    Key = {Lbl,Reg},
    D = case gb_trees:lookup(Key, D0) of
	    none -> gb_trees:insert(Key, Lit, D0); %New.
	    {value,inconsistent} -> D0;		%Inconsistent.
	    {value,_} -> gb_trees:update(Key, inconsistent, D0)
	end,
    update_value_dict(T, Reg, D);
update_value_dict([], _, D) -> D.

%%%
%%% Scan instructions in reverse execution order and remove dead code.
%%%

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
    List = shortcut_select_list(List0, Reg, D, []),
    Fail1 = shortcut_label(Fail0, D),
    Fail = shortcut_bs_test(Fail1, Is, D),
    Sel = {select,select_val,Reg,{f,Fail},List},
    backward(Is, D, [Sel|Acc]);
backward([{jump,{f,To0}},{move,Src,Reg}=Move0|Is], D, Acc) ->
    {To,Move} = case Src of
		    {atom,Val0} ->
			To1 = shortcut_select_label(To0, Reg, Val0, D),
			{To2,Val} = shortcut_boolean_label(To1, Reg, Val0, D),
			{To2,{move,{atom,Val},Reg}};
		    _ ->
			{shortcut_label(To0, D),Move0}
		end,
    Jump = {jump,{f,To}},
    case beam_utils:is_killed_at(Reg, To, D) of
	false -> backward([Move|Is], D, [Jump|Acc]);
	true -> backward([Jump|Is], D, Acc)
    end;
backward([{jump,{f,To}}=J|[{bif,Op,_,Ops,Reg}|Is]=Is0], D, Acc) ->
    try replace_comp_op(To, Reg, Op, Ops, D) of
	I -> backward(Is, D, I++Acc)
    catch
	throw:not_possible -> backward(Is0, D, [J|Acc])
    end;
backward([{test,bs_start_match2,{f,To0},Live,[Src|_]=Info,Dst}|Is], D, Acc) ->
    To = shortcut_bs_start_match(To0, Src, D),
    I = {test,bs_start_match2,{f,To},Live,Info,Dst},
    backward(Is, D, [I|Acc]);
backward([{test,is_eq_exact,{f,To0},[Reg,{atom,Val}]=Ops}|Is], D, Acc) ->
    To1 = shortcut_bs_test(To0, Is, D),
    To = shortcut_fail_label(To1, Reg, Val, D),
    I = combine_eqs(To, Ops, D, Acc),
    backward(Is, D, [I|Acc]);
backward([{test,Op,{f,To0},Ops0}|Is], D, Acc) ->
    To1 = shortcut_bs_test(To0, Is, D),
    To2 = shortcut_label(To1, D),
    %% Try to shortcut a repeated test:
    %%
    %%        test Op {f,Fail1} Operands	test Op {f,Fail2} Operands
    %%        . . .		          ==>   ...
    %% Fail1: test Op {f,Fail2} Operands        Fail1: test Op {f,Fail2} Operands
    %%
    To = case beam_utils:code_at(To2, D) of
	     [{test,Op,{f,To3},Ops}|_] ->
		 case equal_ops(Ops0, Ops) of
		     true -> To3;
		     false -> To2
		 end;
	     _Code ->
		 To2
	 end,
    I = case Op of
	    is_eq_exact -> combine_eqs(To, Ops0, D, Acc);
	    _ -> {test,Op,{f,To},Ops0}
	end,
    backward(Is, D, [I|Acc]);
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
    
shortcut_select_list([{_,Val}=Lit,{f,To0}|T], Reg, D, Acc) ->
    To = shortcut_select_label(To0, Reg, Val, D),
    shortcut_select_list(T, Reg, D, [{f,To},Lit|Acc]);
shortcut_select_list([], _, _, Acc) -> reverse(Acc).

shortcut_label(To0, D) ->
    case beam_utils:code_at(To0, D) of
  	[{jump,{f,To}}|_] -> shortcut_label(To, D);
	_ -> To0
    end.

shortcut_select_label(To0, Reg, Val, D) ->
    case beam_utils:code_at(To0, D) of
 	[{jump,{f,To}}|_] ->
 	    shortcut_select_label(To, Reg, Val, D);
	[{test,is_atom,_,[Reg]},{select,select_val,Reg,{f,Fail},Map}|_] ->
	    To = find_select_val(Map, Val, Fail),
	    shortcut_select_label(To, Reg, Val, D);
  	[{test,is_eq_exact,{f,_},[Reg,{atom,Val}]},{label,To}|_] when is_atom(Val) ->
	    shortcut_select_label(To, Reg, Val, D);
  	[{test,is_eq_exact,{f,_},[Reg,{atom,Val}]},{jump,{f,To}}|_] when is_atom(Val) ->
	    shortcut_select_label(To, Reg, Val, D);
  	[{test,is_eq_exact,{f,To},[Reg,{atom,AnotherVal}]}|_]
	when is_atom(Val), Val =/= AnotherVal ->
	    shortcut_select_label(To, Reg, Val, D);
  	[{test,is_ne_exact,{f,To},[Reg,{atom,Val}]}|_] when is_atom(Val) ->
	    shortcut_select_label(To, Reg, Val, D);
  	[{test,is_ne_exact,{f,_},[Reg,{atom,_}]},{label,To}|_] when is_atom(Val) ->
	    shortcut_select_label(To, Reg, Val, D);
	[{test,is_tuple,{f,To},[Reg]}|_] when is_atom(Val) ->
	    shortcut_select_label(To, Reg, Val, D);
	_ ->
	    To0
    end.

shortcut_fail_label(To0, Reg, Val, D) ->
    case beam_utils:code_at(To0, D) of
 	[{jump,{f,To}}|_] ->
	    shortcut_fail_label(To, Reg, Val, D);
  	[{test,is_eq_exact,{f,To},[Reg,{atom,Val}]}|_] when is_atom(Val) ->
	    shortcut_fail_label(To, Reg, Val, D);
	_ ->
	    To0
    end.

shortcut_boolean_label(To0, Reg, Bool0, D) when is_boolean(Bool0) ->
    case beam_utils:code_at(To0, D) of
	[{line,_},{bif,'not',_,[Reg],Reg},{jump,{f,To}}|_] ->
	    Bool = not Bool0,
	    {shortcut_select_label(To, Reg, Bool, D),Bool};
	_ ->
	    {To0,Bool0}
    end;
shortcut_boolean_label(To, _, Bool, _) -> {To,Bool}.

find_select_val([{_,Val},{f,To}|_], Val, _) -> To;
find_select_val([{_,_}, {f,_}|T], Val, Fail) ->
    find_select_val(T, Val, Fail);
find_select_val([], _, Fail) -> Fail.

replace_comp_op(To, Reg, Op, Ops, D) ->
    False = comp_op_find_shortcut(To, Reg, false, D),
    True = comp_op_find_shortcut(To, Reg, true, D),
    [bif_to_test(Op, Ops, False),{jump,{f,True}}].

comp_op_find_shortcut(To0, Reg, Val, D) ->
    case shortcut_select_label(To0, Reg, Val, D) of
	To0 ->
	    not_possible();
	To ->
	    case beam_utils:is_killed_at(Reg, To, D) of
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
%%              	.
%%                	.	     ==>
%%                	.
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

%% shortcut_bs_test(TargetLabel, [Instruction], D) -> TargetLabel'
%%  Try to shortcut the failure label for a bit syntax matching.
%%  We know that the binary contains at least Bits bits after
%%  the latest save point.

shortcut_bs_test(To, Is, D) ->
    shortcut_bs_test_1(beam_utils:code_at(To, D), Is, To, D).

shortcut_bs_test_1([{bs_restore2,Reg,SavePoint}|Is], PrevIs, To, D) ->
    shortcut_bs_test_2(Is, {Reg,SavePoint}, PrevIs, To, D);
shortcut_bs_test_1([_|_], _, To, _) -> To.

shortcut_bs_test_2([{label,_}|Is], Save, PrevIs, To, D) ->
    shortcut_bs_test_2(Is, Save, PrevIs, To, D);
shortcut_bs_test_2([{test,bs_test_tail2,{f,To},[_,TailBits]}|_],
		   {Reg,_Point} = RP, PrevIs, To0, D) ->
    case count_bits_matched(PrevIs, RP, 0) of
	Bits when Bits > TailBits ->
	    %% This instruction will fail. We know because a restore has been
	    %% done from the previous point SavePoint in the binary, and we also know
	    %% that the binary contains at least Bits bits from SavePoint.
	    %%
	    %% Since we will skip a bs_restore2 if we shortcut to label To,
	    %% we must now make sure that code at To does not depend on the position
	    %% in the context in any way.
	    case shortcut_bs_pos_used(To, Reg, D) of
		false -> To;
		true -> To0
	    end;
	_Bits ->
	    To0
    end;
shortcut_bs_test_2([_|_], _, _, To, _) -> To.

count_bits_matched([{test,_,_,_,[_,Sz,U,{field_flags,_}],_}|Is], SavePoint, Bits) ->
    case Sz of
	{integer,N} -> count_bits_matched(Is, SavePoint, Bits+N*U);
	_ -> count_bits_matched(Is, SavePoint, Bits)
    end;
count_bits_matched([{test,bs_match_string,_,[_,Bits,_]}|Is], SavePoint, Bits0) ->
    count_bits_matched(Is, SavePoint, Bits0+Bits);
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
%%  A failing bs_start_match2 instruction means that the source
%%  cannot be a binary, so there is no need to jump bs_context_to_binary/1
%%  or another bs_start_match2 instruction.

shortcut_bs_start_match(To, Reg, D) ->
    shortcut_bs_start_match_1(beam_utils:code_at(To, D), Reg, To).

shortcut_bs_start_match_1([{bs_context_to_binary,Reg}|Is], Reg, To) ->
    shortcut_bs_start_match_2(Is, Reg, To);
shortcut_bs_start_match_1(_, _, To) -> To.

shortcut_bs_start_match_2([{jump,{f,To}}|_], _, _) ->
    To;
shortcut_bs_start_match_2([{test,bs_start_match2,{f,To},_,[Reg|_],_}|_], Reg, _) ->
    To;
shortcut_bs_start_match_2(_Is, _Reg, To) ->
    To.
