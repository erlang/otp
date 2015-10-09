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
%%     $Id: beam_jump.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
%%% Purpose : Optimise jumps and remove unreachable code.

-module(beam_jump).

-export([module/2,module_labels/1,
	 is_unreachable_after/1,remove_unused_labels/1]).

%%% The following optimisations are done:
%%%
%%% (1) This code with two identical instruction sequences
%%%
%%%     L1: <Instruction sequence>
%%%     L2:
%%%          . . .
%%%     L3: <Instruction sequence>
%%%     L4:
%%%
%%%     can be replaced with
%%%
%%%     L1: jump L3
%%%     L2:
%%%          . . .
%%%     L3: <Instruction sequence>
%%%     L4
%%%
%%%     Note: The instruction sequence must end with an instruction
%%%     such as a jump that never transfers control to the instruction
%%%     following it.
%%%
%%% (2) case_end, if_end, and badmatch, and function calls that cause an
%%%     exit (such as calls to exit/1) are moved to the end of the function.
%%%     The purpose is to allow further optimizations at the place from
%%%     which the code was moved.
%%%
%%% (3) Any unreachable code is removed.  Unreachable code is code after
%%%     jump, call_last and other instructions which never transfer control
%%%     to the following instruction.  Code is unreachable up to the next
%%%     *referenced* label.  Note that the optimisations below might
%%%     generate more possibilities for removing unreachable code.
%%%
%%% (4) This code:
%%%	L1:	jump L2
%%%          . . .
%%%     L2: ...
%%%
%%%    will be changed to
%%%
%%%    jump L2
%%%          . . .
%%%    L1:
%%%    L2: ...
%%%
%%%    If the jump is unreachable, it will be removed according to (1).
%%%
%%% (5) In
%%%
%%%	 jump L1
%%%      L1:
%%%
%%%	 the jump will be removed.
%%%
%%% (6) If test instructions are used to skip a single jump instruction,
%%%      the test is inverted and the jump is eliminated (provided that
%%%      the test can be inverted).  Example:
%%%
%%%      is_eq L1 {x,1} {x,2}
%%%      jump L2
%%%      L1:
%%%
%%%      will be changed to
%%%
%%%      is_ne L2 {x,1} {x,2}
%%%
%%%      (The label L1 will be retained if there were previous references to it.)
%%%
%%% (7) Some redundant uses of is_boolean/1 is optimized away.
%%%
%%% Terminology note: The optimisation done here is called unreachable-code
%%% elimination, NOT dead-code elimination.  Dead code elimination
%%% means the removal of instructions that are executed, but have no visible
%%% effect on the program state.
%%%

-import(lists, [reverse/1,reverse/2,map/2,mapfoldl/3,foldl/3,
		last/1,foreach/2,member/2]).

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = map(fun function/1, Fs0),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

module_labels({Mod,Exp,Attr,Fs,Lc}) ->
    {Mod,Exp,Attr,map(fun function_labels/1, Fs),Lc}.

function_labels({function,Name,Arity,CLabel,Asm0}) ->
    Asm = remove_unused_labels(Asm0),
    {function,Name,Arity,CLabel,Asm}.

function({function,Name,Arity,CLabel,Asm0}) ->
    Asm1 = share(Asm0),
    Asm2 = bopt(Asm1),
    Asm3 = move(Asm2),
    Asm4 = opt(Asm3, CLabel),
    Asm = remove_unused_labels(Asm4),
    {function,Name,Arity,CLabel,Asm}.

%%%
%%% (1) We try to share the code for identical code segments by replacing all
%%% occurrences except the last with jumps to the last occurrence.
%%%

share(Is) ->
    share_1(reverse(Is), gb_trees:empty(), [], []).

share_1([{label,_}=Lbl|Is], Dict, [], Acc) ->
    share_1(Is, Dict, [], [Lbl|Acc]);
share_1([{label,L}=Lbl|Is], Dict0, Seq, Acc) ->
    case is_unreachable_after(last(Seq)) of
	false ->
	    share_1(Is, Dict0, [], [Lbl|Seq ++ Acc]);
	true ->
	    case gb_trees:lookup(Seq, Dict0) of
		none ->
		    Dict = gb_trees:insert(Seq, L, Dict0),
		    share_1(Is, Dict, [], [Lbl|Seq ++ Acc]);
		{value,Label} ->
		    share_1(Is, Dict0, [], [Lbl,{jump,{f,Label}}|Acc])
	    end
    end;
share_1([{func_info,_,_,_}=I|Is], _, [], Acc) ->
    Is++[I|Acc];
share_1([I|Is], Dict, Seq, Acc) ->
    case is_unreachable_after(I) of
	false ->
	    share_1(Is, Dict, [I|Seq], Acc);
	true ->
	    share_1(Is, Dict, [I], Acc)
    end.

%%%
%%% (2) Move short code sequences ending in an instruction that causes an exit
%%% to the end of the function.
%%%

move(Is) ->
    move_1(Is, [], []).

move_1([I|Is], End, Acc) ->
    case is_exit_instruction(I) of
	false -> move_1(Is, End, [I|Acc]);
	true -> move_2(I, Is, End, Acc)
    end;
move_1([], End, Acc) ->
    reverse(Acc, reverse(End)).

move_2(Exit, Is, End, [{block,_},{label,_},{func_info,_,_,_}|_]=Acc) ->
    move_1(Is, End, [Exit|Acc]);
move_2(Exit, Is, End, [{kill,_Y}|Acc]) ->
    move_2(Exit, Is, End, Acc);
move_2(Exit, Is, End, [{block,_}=Blk,{label,_}=Lbl,Dead|More]=Acc) ->
    case is_unreachable_after(Dead) of
	false ->
	    move_1(Is, End, [Exit|Acc]);
	true ->
	    move_1([Dead|Is], [Exit,Blk,Lbl|End], More)
    end;
move_2(Exit, Is, End, [{label,_}=Lbl,Dead|More]=Acc) ->
    case is_unreachable_after(Dead) of
	false ->
	    move_1(Is, End, [Exit|Acc]);
	true ->
	    move_1([Dead|Is], [Exit,Lbl|End], More)
    end;
move_2(Exit, Is, End, Acc) ->
    move_1(Is, End, [Exit|Acc]).

%%%
%%% (7) Remove redundant is_boolean tests.
%%%

bopt(Is) ->
    bopt_1(Is, []).

bopt_1([{test,is_boolean,_,_}=I|Is], Acc0) ->
    case opt_is_bool(I, Acc0) of
	no -> bopt_1(Is, [I|Acc0]);
	yes -> bopt_1(Is, Acc0);
	{yes,Acc} -> bopt_1(Is, Acc)
    end;
bopt_1([I|Is], Acc) -> bopt_1(Is, [I|Acc]);
bopt_1([], Acc) -> reverse(Acc).

opt_is_bool({test,is_boolean,{f,Lbl},[Reg]}, Acc) ->
    opt_is_bool_1(Acc, Reg, Lbl).

opt_is_bool_1([{test,is_eq_exact,{f,Lbl},[Reg,{atom,true}]}|_], Reg, Lbl) ->
    %% Instruction not needed in this context.
    yes;
opt_is_bool_1([{test,is_ne_exact,{f,Lbl},[Reg,{atom,true}]}|Acc], Reg, Lbl) ->
    %% Rewrite to shorter test.
    {yes,[{test,is_eq_exact,{f,Lbl},[Reg,{atom,false}]}|Acc]};
opt_is_bool_1([{test,_,{f,Lbl},_}=Test|Acc0], Reg, Lbl) ->
    case opt_is_bool_1(Acc0, Reg, Lbl) of
	{yes,Acc} -> {yes,[Test|Acc]};
	Other -> Other
    end;
opt_is_bool_1(_, _, _) -> no.

%%%
%%% (3) (4) (5) (6) Jump and unreachable code optimizations.
%%%

-record(st, {fc,				%Label for function class errors.
	     entry,				%Entry label (must not be moved).
	     mlbl,				%Moved labels.
	     labels				%Set of referenced labels.
	    }).

opt([{label,Fc}|_]=Is, CLabel) ->
    Lbls = initial_labels(Is),
    St = #st{fc=Fc,entry=CLabel,mlbl=dict:new(),labels=Lbls},
    opt(Is, [], St).

opt([{test,Test0,{f,Lnum}=Lbl,Ops}=I|Is0], Acc, St) ->
    case Is0 of
	[{jump,To}|[{label,Lnum}|Is2]=Is1] ->
	    case invert_test(Test0) of
		not_possible ->
		    opt(Is0, [I|Acc], label_used(Lbl, St));
		Test ->
		    Is = case is_label_used(Lnum, St) of
			     true -> Is1;
			     false -> Is2
			 end,
		    opt([{test,Test,To,Ops}|Is], Acc, label_used(To, St))
	    end;
	_Other ->
	    opt(Is0, [I|Acc], label_used(Lbl, St))
    end;
opt([{select_val,_R,Fail,{list,Vls}}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used([Fail|Vls], St));
opt([{select_tuple_arity,_R,Fail,{list,Vls}}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used([Fail|Vls], St));
opt([{'try',_R,Lbl}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{'catch',_R,Lbl}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{label,L}=I|Is], Acc, #st{entry=L}=St) ->
    %% NEVER move the entry label.
    opt(Is, [I|Acc], St);
opt([{label,L1},{jump,{f,L2}}=I|Is], [Prev|Acc], St0) ->
    St = St0#st{mlbl=dict:append(L2, L1, St0#st.mlbl)},
    opt([Prev,I|Is], Acc, label_used({f,L2}, St));
opt([{label,Lbl}=I|Is], Acc, #st{mlbl=Mlbl}=St0) ->
    case dict:find(Lbl, Mlbl) of
	{ok,Lbls} ->
	    %% Essential to remove the list of labels from the dictionary,
	    %% since we will rescan the inserted labels.  We MUST rescan.
	    St = St0#st{mlbl=dict:erase(Lbl, Mlbl)},
	    insert_labels([Lbl|Lbls], Is, Acc, St);
	error -> opt(Is, [I|Acc], St0)
    end;
opt([{jump,{f,Lbl}},{label,Lbl}=I|Is], Acc, St) ->
    opt([I|Is], Acc, St);
opt([{jump,Lbl}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used(Lbl, St));
opt([{loop_rec,Lbl,_R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bif,_Name,Lbl,_As,_R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_integer,Lbl,_Bits,_Unit,_Fl,_Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_binary,Lbl,_Bits,_Unit,_Fl,_Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_float,Lbl,_Bits,_Unit,_Fl,_Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_final,Lbl,_R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_init2,Lbl,_,_,_,_,_}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_add,Lbl,_,_}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_bits_to_bytes,Lbl,_,_}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([I|Is], Acc, St) ->
    case is_unreachable_after(I) of
	true  -> skip_unreachable(Is, [I|Acc], St);
	false -> opt(Is, [I|Acc], St)
    end;
opt([], Acc, #st{fc=Fc,mlbl=Mlbl}) ->
    Code = reverse(Acc),
    case dict:find(Fc, Mlbl) of
	{ok,Lbls} -> insert_fc_labels(Lbls, Mlbl, Code);
	error -> Code
    end.

insert_fc_labels([L|Ls], Mlbl, Acc0) ->
    Acc = [{label,L}|Acc0],
    case dict:find(L, Mlbl) of
	error ->
	    insert_fc_labels(Ls, Mlbl, Acc);
	{ok,Lbls} ->
	    insert_fc_labels(Lbls++Ls, Mlbl, Acc)
    end;
insert_fc_labels([], _, Acc) -> Acc.

%% invert_test(Test0) -> not_possible | Test

invert_test(is_ge) ->       is_lt;
invert_test(is_lt) ->       is_ge;
invert_test(is_eq) ->       is_ne;
invert_test(is_ne) ->       is_eq;
invert_test(is_eq_exact) -> is_ne_exact;
invert_test(is_ne_exact) -> is_eq_exact;
invert_test(_) ->           not_possible.

insert_labels([L|Ls], Is, [{jump,{f,L}}|Acc], St) ->
    insert_labels(Ls, [{label,L}|Is], Acc, St);
insert_labels([L|Ls], Is, Acc, St) ->
    insert_labels(Ls, [{label,L}|Is], Acc, St);
insert_labels([], Is, Acc, St) ->
    opt(Is, Acc, St).

%% Skip unreachable code up to the next referenced label.

skip_unreachable([{label,L}|Is], [{jump,{f,L}}|Acc], St) ->
    opt([{label,L}|Is], Acc, St);
skip_unreachable([{label,L}|Is], Acc, St) ->
    case is_label_used(L, St) of
	true  -> opt([{label,L}|Is], Acc, St);
	false -> skip_unreachable(Is, Acc, St)
    end;
skip_unreachable([_|Is], Acc, St) ->
    skip_unreachable(Is, Acc, St);
skip_unreachable([], Acc, St) ->
    opt([], Acc, St).

%% Add one or more label to the set of used labels.

label_used({f,0}, St) -> St;
label_used({f,L}, St) -> St#st{labels=gb_sets:add(L, St#st.labels)};
label_used([H|T], St0) -> label_used(T, label_used(H, St0));
label_used([], St) -> St;
label_used(_Other, St) -> St.

%% Test if label is used.

is_label_used(L, St) ->
    gb_sets:is_member(L, St#st.labels).

%% is_unreachable_after(Instruction) -> true|false
%%  Test whether the code after Instruction is unreachable.

is_unreachable_after({func_info,_M,_F,_A}) -> true;
is_unreachable_after(return) -> true;
is_unreachable_after({call_ext_last,_Ar,_ExtFunc,_D}) -> true;
is_unreachable_after({call_ext_only,_Ar,_ExtFunc}) -> true;
is_unreachable_after({call_last,_Ar,_Lbl,_D}) -> true;
is_unreachable_after({call_only,_Ar,_Lbl}) -> true;
is_unreachable_after({apply_last,_Ar,_N}) -> true;
is_unreachable_after({jump,_Lbl}) -> true;
is_unreachable_after({select_val,_R,_Lbl,_Cases}) -> true;
is_unreachable_after({select_tuple_arity,_R,_Lbl,_Cases}) -> true;
is_unreachable_after({loop_rec_end,_}) -> true;
is_unreachable_after({wait,_}) -> true;
is_unreachable_after(I) -> is_exit_instruction(I).

%% is_exit_instruction(Instruction) -> true|false
%%  Test whether the instruction Instruction always
%%  causes an exit/failure.

is_exit_instruction({call_ext,_,{extfunc,M,F,A}}) ->
    is_exit_instruction_1(M, F, A);
is_exit_instruction({call_ext_last,_,{extfunc,M,F,A},_}) ->
    is_exit_instruction_1(M, F, A);
is_exit_instruction({call_ext_only,_,{extfunc,M,F,A}}) ->
    is_exit_instruction_1(M, F, A);
is_exit_instruction(if_end) -> true;
is_exit_instruction({case_end,_}) -> true;
is_exit_instruction({try_case_end,_}) -> true;
is_exit_instruction({badmatch,_}) -> true;
is_exit_instruction(_) -> false.

is_exit_instruction_1(erlang, exit, 1) -> true;
is_exit_instruction_1(erlang, throw, 1) -> true;
is_exit_instruction_1(erlang, error, 1) -> true;
is_exit_instruction_1(erlang, error, 2) -> true;
is_exit_instruction_1(erlang, fault, 1) -> true;
is_exit_instruction_1(erlang, fault, 2) -> true;
is_exit_instruction_1(_, _, _) -> false.

%% remove_unused_labels(Instructions0) -> Instructions
%%  Remove all unused labels.

remove_unused_labels(Is) ->
    Used0 = initial_labels(Is),
    Used = foldl(fun ulbl/2, Used0, Is),
    rem_unused(Is, Used, []).

rem_unused([{label,Lbl}=I|Is], Used, Acc) ->
    case gb_sets:is_member(Lbl, Used) of
	false -> rem_unused(Is, Used, Acc);
	true -> rem_unused(Is, Used, [I|Acc])
    end;
rem_unused([I|Is], Used, Acc) ->
    rem_unused(Is, Used, [I|Acc]);
rem_unused([], _, Acc) -> reverse(Acc).

initial_labels(Is) ->
    initial_labels(Is, []).

initial_labels([{label,Lbl}|Is], Acc) ->
    initial_labels(Is, [Lbl|Acc]);
initial_labels([{func_info,_,_,_},{label,Lbl}|_], Acc) ->
    gb_sets:from_list([Lbl|Acc]).

ulbl({test,_,Fail,_}, Used) ->
    mark_used(Fail, Used);
ulbl({select_val,_,Fail,{list,Vls}}, Used) ->
    mark_used_list(Vls, mark_used(Fail, Used));
ulbl({select_tuple_arity,_,Fail,{list,Vls}}, Used) ->
    mark_used_list(Vls, mark_used(Fail, Used));
ulbl({'try',_,Lbl}, Used) ->
    mark_used(Lbl, Used);
ulbl({'catch',_,Lbl}, Used) ->
    mark_used(Lbl, Used);
ulbl({jump,Lbl}, Used) ->
    mark_used(Lbl, Used);
ulbl({loop_rec,Lbl,_}, Used) ->
    mark_used(Lbl, Used);
ulbl({loop_rec_end,Lbl}, Used) ->
    mark_used(Lbl, Used);
ulbl({wait,Lbl}, Used) ->
    mark_used(Lbl, Used);
ulbl({wait_timeout,Lbl,_To}, Used) ->
    mark_used(Lbl, Used);
ulbl({bif,_Name,Lbl,_As,_R}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_init2,Lbl,_,_,_,_,_}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_put_integer,Lbl,_Bits,_Unit,_Fl,_Val}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_put_float,Lbl,_Bits,_Unit,_Fl,_Val}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_put_binary,Lbl,_Bits,_Unit,_Fl,_Val}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_final,Lbl,_}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_add,Lbl,_,_}, Used) ->
    mark_used(Lbl, Used);
ulbl({bs_bits_to_bytes,Lbl,_,_}, Used) ->
    mark_used(Lbl, Used);
ulbl(_, Used) -> Used.

mark_used({f,0}, Used) -> Used;
mark_used({f,L}, Used) -> gb_sets:add(L, Used);
mark_used(_, Used) -> Used.

mark_used_list([H|T], Used) ->
    mark_used_list(T, mark_used(H, Used));
mark_used_list([], Used) -> Used.
