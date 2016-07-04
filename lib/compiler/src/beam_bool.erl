%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% Purpose: Optimizes booleans in guards.

-module(beam_bool).

-export([module/2]).

-import(lists, [reverse/1,reverse/2,foldl/3,mapfoldl/3,map/2]).

-record(st,
	{next,					%Next label number.
	 ll					%Live regs at labels.
	}).
	 
module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    %%io:format("~p:\n", [Mod]),
    {Fs,_} = mapfoldl(fun(Fn, Lbl) -> function(Fn, Lbl) end, 100000000, Fs0),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, Lbl0) ->
    try
	{Is,#st{next=Lbl}} = bool_opt(Is0, Lbl0),
	{{function,Name,Arity,CLabel,Is},Lbl}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

%%
%% Optimize boolean expressions that use guard bifs. Rewrite to
%% use test instructions if possible.
%%

bool_opt(Asm, Lbl) ->
    LiveInfo = beam_utils:index_labels(Asm),
    bopt(Asm, [], #st{next=Lbl,ll=LiveInfo}).

bopt([{block,Bl0}=Block|
      [{jump,{f,Succ}},
       {label,Fail},
       {block,[{set,[Dst],[{atom,false}],move}]},
       {label,Succ}|Is]=Is0], Acc0, St) ->
    case split_block(Bl0, Dst, Fail, Acc0, true) of
	failed ->
	    bopt(Is0, [Block|Acc0], St);
	{Bl,PreBlock} ->
	    Acc1 = case PreBlock of
		       [] -> Acc0;
		       _ -> [{block,PreBlock}|Acc0]
		   end,
	    Acc = [{protected,[Dst],Bl,{Fail,Succ}}|Acc1],
	    bopt(Is, Acc, St)
    end;
bopt([{test,is_eq_exact,{f,Fail},[Reg,{atom,true}]}=I|Is], [{block,_}|_]=Acc0, St0) ->
    case bopt_block(Reg, Fail, Is, Acc0, St0) of
	failed -> bopt(Is, [I|Acc0], St0);
	{Acc,St} -> bopt(Is, Acc, St)
    end;
bopt([I|Is], Acc, St) ->
    bopt(Is, [I|Acc], St);
bopt([], Acc, St) ->
    {bopt_reverse(Acc, []),St}.

bopt_reverse([{protected,[Dst],Block,{Fail,Succ}}|Is], Acc0) ->
    Acc = [{block,Block},{jump,{f,Succ}},
	   {label,Fail},
	   {block,[{set,[Dst],[{atom,false}],move}]},
	   {label,Succ}|Acc0],
    bopt_reverse(Is, Acc);
bopt_reverse([I|Is], Acc) ->
    bopt_reverse(Is, [I|Acc]);
bopt_reverse([], Acc) -> Acc.

%% bopt_block(Reg, Fail, OldIs, Accumulator, St) -> failed | {NewAcc,St}
%%  Attempt to optimized a block of guard BIFs followed by a test
%%  instruction.
bopt_block(Reg, Fail, OldIs, [{block,Bl0}|Acc0], St0) ->
    case split_block(Bl0, Reg, Fail, Acc0, false) of
	failed ->
	    %% Reason for failure: The block either contained no
	    %% guard BIFs with the failure label Fail, or the final
	    %% instruction in the block did not assign the Reg register.

	    %%io:format("split ~p: ~P\n", [Reg,Bl0,20]),
	    failed;
	{Bl1,BlPre} ->
	    %% The block has been splitted. Bl1 is a non-empty list
	    %% of guard BIF instructions having the failure label Fail.
	    %% BlPre is a (possibly empty list) of instructions preceeding
	    %% Bl1.
	    Acc1 = make_block(BlPre, Acc0),
	    {Bl,Acc} = extend_block(Bl1, Fail, Acc1),
	    try
		{NewCode,St} = bopt_tree_cg(Bl, Fail, St0),
		ensure_opt_safe(Bl, NewCode, OldIs, Fail, Acc, St),
		{NewCode++Acc,St}
	    catch
		%% Not possible to rewrite because a boolean value is
		%% passed to another guard bif, e.g. 'abs(A > B)'
		%% (in this case, obviously nonsense code). Rare in
		%% practice.
		throw:mixed ->
		    failed;

		%% There was a reference to a boolean expression
		%% from inside a protected block (try/catch), to
		%% a boolean expression outside.
		throw:protected_barrier ->
		    failed;

		%% The 'xor' operator was used. We currently don't
		%% find it worthwile to translate 'xor' operators
		%% (the code would be clumsy).
		throw:'xor' ->
		    failed;

		%% The block does not contain a boolean expression,
		%% but only a call to a guard BIF.
		%% For instance: ... when element(1, T) ->
		throw:not_boolean_expr ->
 		    failed;

		%% The optimization is not safe. (A register
		%% used by the instructions following the
		%% optimized code is either not assigned a
		%% value at all or assigned a different value.)
		throw:all_registers_not_killed ->
		    failed;
		throw:registers_used ->
		    failed;

		%% A protected block refered to the value
		%% returned by another protected block,
		%% probably because the Core Erlang code
		%% used nested try/catches in the guard.
		%% (v3_core never produces nested try/catches
		%% in guards, so it must have been another
		%% Core Erlang translator.)
		throw:protected_violation ->
		    failed;

		%% Failed to work out the live registers for a GC
		%% BIF. For example, if the number of live registers
		%% needed to be 4 because {x,3} was a source register,
		%% but {x,2} was not known to be initialized, this
		%% exception would be thrown.
		throw:gc_bif_alloc_failure ->
		    failed

	    end
    end.

%% ensure_opt_safe(OriginalCode, OptCode, FollowingCode, Fail,
%%             ReversedPrecedingCode, State) -> ok
%%  Comparing the original code to the optimized code, determine
%%  whether the optimized code is guaranteed to work in the same
%%  way as the original code.
%%
%%  Throw an exception if the optimization is not safe.
%%
ensure_opt_safe(Bl, NewCode, OldIs, Fail, PrecedingCode, St) ->
    %% Here are the conditions that must be true for the
    %% optimization to be safe.
    %%
    %% 1. If a register is INITIALIZED by PrecedingCode,
    %%    then if that register assigned a value in the original
    %%    code, but not in the optimized code, it must be UNUSED or KILLED
    %%    in the code that follows.
    %%
    %% 2. If a register is not known to be INITIALIZED by PreccedingCode,
    %%    then if that register assigned a value in the original
    %%    code, but not in the optimized code, it must be KILLED
    %%    by the code that follows.
    %%
    %% 3. Any register that is assigned a value in the optimized
    %%    code must be UNUSED or KILLED in the following code,
    %%    unless we can be sure that it is always assigned the same
    %%    value.

    InitInPreceding = initialized_regs(PrecedingCode),

    PrevDst = dst_regs(Bl),
    NewDst = dst_regs(NewCode),
    NotSet = ordsets:subtract(PrevDst, NewDst),
    MustBeKilled = ordsets:subtract(NotSet, InitInPreceding),

    case all_killed(MustBeKilled, OldIs, Fail, St) of
	false -> throw(all_registers_not_killed);
	true -> ok
    end,
    MustBeUnused = ordsets:subtract(ordsets:union(NotSet, NewDst),
				    MustBeKilled),
    case none_used(MustBeUnused, OldIs, Fail, St) of
	false -> throw(registers_used);
	true -> ok
    end,
    ok.

update_fail_label([{set,Ds,As,{bif,N,{f,_}}}|Is], Fail, Acc) ->
    update_fail_label(Is, Fail, [{set,Ds,As,{bif,N,{f,Fail}}}|Acc]);
update_fail_label([{set,Ds,As,{alloc,Regs,{gc_bif,N,{f,_}}}}|Is], Fail, Acc) ->
    update_fail_label(Is, Fail,
		      [{set,Ds,As,{alloc,Regs,{gc_bif,N,{f,Fail}}}}|Acc]);
update_fail_label([], _, Acc) -> reverse(Acc).

make_block(Bl) ->    
    make_block(Bl, []).

make_block([], Acc) -> Acc;
make_block(Bl, Acc) -> [{block,Bl}|Acc].

extend_block(BlAcc, Fail, [{protected,_,_,_}=Prot|OldAcc]) ->
    extend_block([Prot|BlAcc], Fail, OldAcc);
extend_block(BlAcc0, Fail, [{block,Is0}|OldAcc]) ->
    case extend_block_1(reverse(Is0), Fail, BlAcc0) of
	{BlAcc,[]} -> extend_block(BlAcc, Fail, OldAcc);
	{BlAcc,Is} -> {BlAcc,[{block,Is}|OldAcc]}
    end;
extend_block(BlAcc, _, OldAcc) -> {BlAcc,OldAcc}.

extend_block_1([{set,[{x,_}],_,{bif,_,{f,Fail}}}=I|Is], Fail, Acc) ->
    extend_block_1(Is, Fail, [I|Acc]);
extend_block_1([{set,[{x,_}],As,{bif,Bif,_}}=I|Is]=Is0, Fail, Acc) ->
    case safe_bool_op(Bif, length(As)) of
	false -> {Acc,reverse(Is0)};
	true -> extend_block_1(Is, Fail, [I|Acc])
    end;
extend_block_1([_|_]=Is, _, Acc) -> {Acc,reverse(Is)};
extend_block_1([], _, Acc) -> {Acc,[]}.

%% split_block([Instruction], Destination, FailLabel, [PreInstruction],
%%             ProhibitFailLabelInPreBlock) -> failed | {Block,PreBlock}
%% Split a sequence of instructions into two blocks - one containing
%% all guard bif instructions and a pre-block all instructions before
%% the guard BIFs.

split_block(Is0, Dst, Fail, PreIs, ProhibitFailLabel) ->
    case ProhibitFailLabel andalso beam_jump:is_label_used_in(Fail, PreIs) of
	true ->
	    %% The failure label was used in one of the instructions (most
	    %% probably bit syntax construction) preceeding the block,
	    %% the caller might eliminate the label.
	    failed;
	false ->
	    case reverse(Is0) of
		[{set,[Dst],_,_}|_]=Is ->
		    split_block_1(Is, Fail, ProhibitFailLabel);
		_ -> failed
	    end
    end.

split_block_1(Is, Fail, ProhibitFailLabel) ->
    case split_block_2(Is, Fail, []) of
	{[],_} -> failed;
	{_,PreBlock}=Res ->
	    case ProhibitFailLabel andalso
		split_block_label_used(PreBlock, Fail) of
		true ->
		    %% The failure label was used in the pre-block;
		    %% not allowed, because the label may be removed.
		    failed;
		false ->
		    Res
	    end
    end.

split_block_2([{set,[_],_,{bif,_,{f,Fail}}}=I|Is], Fail, Acc) ->
    split_block_2(Is, Fail, [I|Acc]);
split_block_2([{set,[_],_,{alloc,_,{gc_bif,_,{f,Fail}}}}=I|Is], Fail, Acc) ->
    split_block_2(Is, Fail, [I|Acc]);
split_block_2(Is0, _, Acc) ->
    Is = reverse(Is0),
    {Acc,Is}.

split_block_label_used([{set,[_],_,{bif,_,{f,Fail}}}|_], Fail) ->
    true;
split_block_label_used([{set,[_],_,{alloc,_,{gc_bif,_,{f,Fail}}}}|_], Fail) ->
    true;
split_block_label_used([{set,[_],_,{alloc,_,{put_map,_,{f,Fail}}}}|_], Fail) ->
    true;
split_block_label_used([_|Is], Fail) ->
    split_block_label_used(Is, Fail);
split_block_label_used([], _) -> false.

dst_regs(Is) ->
    dst_regs(Is, []).

dst_regs([{block,Bl}|Is], Acc) ->
    dst_regs(Bl, dst_regs(Is, Acc));
dst_regs([{set,[D],_,{bif,_,{f,_}}}|Is], Acc) ->
    dst_regs(Is, [D|Acc]);
dst_regs([{set,[D],_,{alloc,_,{gc_bif,_,{f,_}}}}|Is], Acc) ->
    dst_regs(Is, [D|Acc]);
dst_regs([{protected,_,Bl,_}|Is], Acc) ->
    dst_regs(Bl, dst_regs(Is, Acc));
dst_regs([_|Is], Acc) ->
    dst_regs(Is, Acc);
dst_regs([], Acc) -> ordsets:from_list(Acc).

all_killed([R|Rs], OldIs, Fail, St) ->
    case is_killed(R, OldIs, Fail, St) of
 	false -> false;
	true -> all_killed(Rs, OldIs, Fail, St)
    end;
all_killed([], _, _, _) -> true.

none_used([R|Rs], OldIs, Fail, St) ->
    case is_not_used(R, OldIs, Fail, St) of
 	false -> false;
	true -> none_used(Rs, OldIs, Fail, St)
    end;
none_used([], _, _, _) -> true.

bopt_tree_cg(Block0, Fail, St) ->
    Free = free_variables(Block0),
    Block = ssa_block(Block0),
%%     io:format("~p\n", [Block0]),
%%    io:format("~p\n", [Block]),
%%    io:format("~p\n", [gb_trees:to_list(Free)]),
    case bopt_tree(Block, Free, []) of
	{Pre0,[{_,Tree}]} ->
	    Pre1 = update_fail_label(Pre0, Fail, []),
	    Regs0 = init_regs(gb_trees:keys(Free)),
%%  	    io:format("~p\n", [dst_regs(Block0)]),
%%  	    io:format("~p\n", [Pre1]),
%%  	    io:format("~p\n", [Tree]),
%%  	    io:nl(),
	    {Pre,Regs} = rename_regs(Pre1, Regs0),
%%  	    io:format("~p\n", [Regs0]),
%%  	    io:format("~p\n", [Pre]),
	    bopt_cg(Tree, Fail, Regs, make_block(Pre), St);
	_Res ->
	    throw(not_boolean_expr)
    end.

bopt_tree([{set,[Dst],As0,{bif,'not',_}}|Is], Forest0, Pre) ->
    {[Arg],Forest1} = bopt_bool_args(As0, Forest0),
    Forest = gb_trees:enter(Dst, {'not',Arg}, Forest1),
    bopt_tree(Is, Forest, Pre);
bopt_tree([{set,[Dst],As0,{bif,'and',_}}|Is], Forest0, Pre) ->
    {As,Forest1} = bopt_bool_args(As0, Forest0),
    Node = make_and_node(As),
    Forest = gb_trees:enter(Dst, Node, Forest1),
    bopt_tree(Is, Forest, Pre);
bopt_tree([{set,[Dst],As0,{bif,'or',_}}|Is], Forest0, Pre) ->
    {As,Forest1} = bopt_bool_args(As0, Forest0),
    Node = make_or_node(As),
    Forest = gb_trees:enter(Dst, Node, Forest1),
    bopt_tree(Is, Forest, Pre);
bopt_tree([{set,_,_,{bif,'xor',_}}|_], _, _) ->
    throw('xor');
bopt_tree([{protected,[Dst],Code,_}|Is], Forest0, Pre) ->
    ProtForest0 = gb_trees:from_orddict([P || {_,any}=P <- gb_trees:to_list(Forest0)]),
    case bopt_tree(Code, ProtForest0, []) of
        {ProtPre,[{_,ProtTree}]} ->
            Prot = {prot,ProtPre,ProtTree},
            Forest = gb_trees:enter(Dst, Prot, Forest0),
            bopt_tree(Is, Forest, Pre);
        _Res ->
            throw(not_boolean_expr)
    end;
bopt_tree([{set,[Dst],As,{bif,N,_}}=Bif|Is], Forest0, Pre) ->
    Ar = length(As),
    case safe_bool_op(N, Ar) of
	false ->
	    bopt_good_args(As, Forest0),
	    Forest = gb_trees:enter(Dst, any, Forest0),
	    bopt_tree(Is, Forest, [Bif|Pre]);
	true ->
	    bopt_good_args(As, Forest0),
	    Test = bif_to_test(Dst, N, As),
	    Forest = gb_trees:enter(Dst, Test, Forest0),
	    bopt_tree(Is, Forest, Pre)
    end;
bopt_tree([{set,[Dst],As,{alloc,_,{gc_bif,_,_}}}=Bif|Is], Forest0, Pre) ->
    bopt_good_args(As, Forest0),
    Forest = gb_trees:enter(Dst, any, Forest0),
    bopt_tree(Is, Forest, [Bif|Pre]);
bopt_tree([], Forest, Pre) ->
    {reverse(Pre),[R || {_,V}=R <- gb_trees:to_list(Forest), V =/= any]}.

safe_bool_op(N, Ar) ->
    erl_internal:new_type_test(N, Ar) orelse erl_internal:comp_op(N, Ar).

bopt_bool_args([V0,V0], Forest0) ->
    {V,Forest} = bopt_bool_arg(V0, Forest0),
    {[V,V],Forest};
bopt_bool_args(As, Forest) ->
    mapfoldl(fun bopt_bool_arg/2, Forest, As).

bopt_bool_arg({T,_}=R, Forest) when T =:= x; T =:= y; T =:= tmp ->
    Val = case gb_trees:lookup(R, Forest) of
	      {value,any} -> {test,is_eq_exact,fail,[R,{atom,true}]};
	      {value,Val0} -> Val0;
              none -> throw(mixed)
	  end,
    {Val,gb_trees:delete(R, Forest)};
bopt_bool_arg(Term, Forest) ->
    {Term,Forest}.

bopt_good_args([A|As], Regs) ->
    bopt_good_arg(A, Regs),
    bopt_good_args(As, Regs);
bopt_good_args([], _) -> ok.

bopt_good_arg({Tag,_}=X, Regs) when Tag =:= x; Tag =:= tmp ->
    case gb_trees:lookup(X, Regs) of
	{value,any} -> ok;
	{value,_} -> throw(mixed);
	none -> throw(protected_barrier)
    end;
bopt_good_arg(_, _) -> ok.

bif_to_test(_, N, As) ->
    beam_utils:bif_to_test(N, As, fail).

make_and_node(Is) ->
    AndList0 = make_and_list(Is),
    case simplify_and_list(AndList0) of
	[] -> {atom,true};
	[Op] -> Op;
	AndList -> {'and',AndList}
    end.

make_and_list([{'and',As}|Is]) ->
    make_and_list(As++Is);
make_and_list([I|Is]) ->
    [I|make_and_list(Is)];
make_and_list([]) -> [].

simplify_and_list([{atom,true}|T]) ->
    simplify_and_list(T);
simplify_and_list([{atom,false}=False|_]) ->
    [False];
simplify_and_list([H|T]) ->
    [H|simplify_and_list(T)];
simplify_and_list([]) -> [].

make_or_node(Is) ->
    OrList0 = make_or_list(Is),
    case simplify_or_list(OrList0) of
	[] -> {atom,false};
	[Op] -> Op;
	OrList -> {'or',OrList}
    end.

make_or_list([{'or',As}|Is]) ->
    make_or_list(As++Is);
make_or_list([I|Is]) ->
    [I|make_or_list(Is)];
make_or_list([]) -> [].

simplify_or_list([{atom,false}|T]) ->
    simplify_or_list(T);
simplify_or_list([{atom,true}=True|_]) ->
    [True];
simplify_or_list([H|T]) ->
    [H|simplify_or_list(T)];
simplify_or_list([]) -> [].

%% Code generation for a boolean tree.

bopt_cg({'not',Arg}, Fail, Rs, Acc, St) ->
    I = bopt_cg_not(Arg),
    bopt_cg(I, Fail, Rs, Acc, St);
bopt_cg({'and',As}, Fail, Rs, Acc, St) ->
    bopt_cg_and(As, Fail, Rs, Acc, St);
bopt_cg({'or',As}, Fail, Rs, Acc, St0) ->
    {Succ,St} = new_label(St0),
    bopt_cg_or(As, Succ, Fail, Rs, Acc, St);
bopt_cg({test,N,fail,As0}, Fail, Rs, Acc, St) ->
    As = rename_sources(As0, Rs),
    Test = {test,N,{f,Fail},As},
    {[Test|Acc],St};
bopt_cg({inverted_test,N,fail,As0}, Fail, Rs, Acc, St0) ->
    As = rename_sources(As0, Rs),
    {Lbl,St} = new_label(St0),
    {[{label,Lbl},{jump,{f,Fail}},{test,N,{f,Lbl},As}|Acc],St};
bopt_cg({prot,Pre0,Tree}, Fail, Rs0, Acc, St0) ->
    Pre1 = update_fail_label(Pre0, Fail, []),
    {Pre,Rs} = rename_regs(Pre1, Rs0),
    bopt_cg(Tree, Fail, Rs, make_block(Pre, Acc), St0);
bopt_cg({atom,true}, _Fail, _Rs, Acc, St) ->
    {Acc,St};
bopt_cg({atom,false}, Fail, _Rs, Acc, St) ->
    {[{jump,{f,Fail}}|Acc],St};
bopt_cg(_, _, _, _, _) ->
    throw(not_boolean_expr).

bopt_cg_not({'and',As0}) ->
    As = [bopt_cg_not(A) || A <- As0],
    {'or',As};
bopt_cg_not({'or',As0}) ->
    As = [bopt_cg_not(A) || A <- As0],
    {'and',As};
bopt_cg_not({'not',Arg}) ->
    bopt_cg_not_not(Arg);
bopt_cg_not({test,Test,Fail,As}) ->
    {inverted_test,Test,Fail,As};
bopt_cg_not({atom,Bool}) when is_boolean(Bool) ->
    {atom,not Bool};
bopt_cg_not(_) ->
    throw(not_boolean_expr).

bopt_cg_not_not({'and',As}) ->
    {'and',[bopt_cg_not_not(A) || A <- As]};
bopt_cg_not_not({'or',As}) ->
    {'or',[bopt_cg_not_not(A) || A <- As]};
bopt_cg_not_not({'not',Arg}) ->
    bopt_cg_not(Arg);
bopt_cg_not_not(Leaf) -> Leaf.

bopt_cg_and([I|Is], Fail, Rs, Acc0, St0) ->
    {Acc,St} = bopt_cg(I, Fail, Rs, Acc0, St0),
    bopt_cg_and(Is, Fail, Rs, Acc, St);
bopt_cg_and([], _, _, Acc, St) -> {Acc,St}.

bopt_cg_or([I], Succ, Fail, Rs, Acc0, St0) ->
    {Acc,St} = bopt_cg(I, Fail, Rs, Acc0, St0),
    {[{label,Succ}|Acc],St};
bopt_cg_or([I|Is], Succ, Fail, Rs, Acc0, St0) ->
    {Lbl,St1} = new_label(St0),
    {Acc,St} = bopt_cg(I, Lbl, Rs, Acc0, St1),
    bopt_cg_or(Is, Succ, Fail, Rs, [{label,Lbl},{jump,{f,Succ}}|Acc], St).
    
new_label(#st{next=LabelNum}=St) when is_integer(LabelNum) ->
    {LabelNum,St#st{next=LabelNum+1}}.

free_variables(Is) ->
    E = gb_sets:empty(),
    free_vars_1(Is, E, E, E).

free_vars_1([{set,Ds,As,{bif,_,_}}|Is], F0, N0, A) ->
    F = gb_sets:union(F0, gb_sets:difference(var_list(As), N0)),
    N = gb_sets:union(N0, var_list(Ds)),
    free_vars_1(Is, F, N, A);
free_vars_1([{set,Ds,As,{alloc,Regs,{gc_bif,_,_}}}|Is], F0, N0, A0) ->
    A = gb_sets:union(A0, gb_sets:from_list(free_vars_regs(Regs))),
    F = gb_sets:union(F0, gb_sets:difference(var_list(As), N0)),
    N = gb_sets:union(N0, var_list(Ds)),
    free_vars_1(Is, F, N, A);
free_vars_1([{protected,_,Pa,_}|Is], F, N, A) ->
    free_vars_1(Pa++Is, F, N, A);
free_vars_1([], F0, N, A) ->
    F = case gb_sets:is_empty(A) of
	    true ->
		%% No GC BIFs.
		{x,X} = gb_sets:smallest(N),
		P = ordsets:from_list(free_vars_regs(X)),
		ordsets:union(gb_sets:to_list(F0), P);
	    false ->
		%% At least one GC BIF.
		gb_sets:to_list(gb_sets:union(F0, gb_sets:difference(A, N)))
	end,
    gb_trees:from_orddict([{K,any} || K <- F]).

var_list(Is) ->
    var_list_1(Is, gb_sets:empty()).

var_list_1([{Tag,_}=X|Is], D) when Tag =:= x; Tag =:= y ->
    var_list_1(Is, gb_sets:add(X, D));
var_list_1([_|Is], D) ->
    var_list_1(Is, D);
var_list_1([], D) -> D.

free_vars_regs(0) -> [];
free_vars_regs(X) -> [{x,X-1}|free_vars_regs(X-1)].

rename_regs(Is, Regs) ->
    rename_regs(Is, Regs, []).

rename_regs([{set,[Dst0],Ss0,{alloc,_,Info}}|Is], Regs0, Acc) ->
    Live = live_regs(Regs0),
    Ss = rename_sources(Ss0, Regs0),
    Regs = put_reg(Dst0, Regs0),
    Dst = fetch_reg(Dst0, Regs),
    rename_regs(Is, Regs, [{set,[Dst],Ss,{alloc,Live,Info}}|Acc]);
rename_regs([{set,[Dst0],Ss0,Info}|Is], Regs0, Acc) ->
    Ss = rename_sources(Ss0, Regs0),
    Regs = put_reg(Dst0, Regs0),
    Dst = fetch_reg(Dst0, Regs),
    rename_regs(Is, Regs, [{set,[Dst],Ss,Info}|Acc]);
rename_regs([], Regs, Acc) -> {reverse(Acc),Regs}.

rename_sources(Ss, Regs) ->
    map(fun({x,_}=R) -> fetch_reg(R, Regs);
	   ({tmp,_}=R) -> fetch_reg(R, Regs);
	   (E) -> E
	end, Ss).

%%%
%%% Keeping track of register assignments.
%%%

init_regs(Free) ->
    init_regs_1(Free, 0).
	
init_regs_1([{x,I}=V|T], I) ->
    [{I,V}|init_regs_1(T, I+1)];
init_regs_1([{x,X}|_]=T, I) when I < X ->
    [{I,reserved}|init_regs_1(T, I+1)];
init_regs_1([{y,_}|_], _) -> [];
init_regs_1([], _) -> [].
    
put_reg(V, Rs) -> put_reg_1(V, Rs, 0).

put_reg_1(V, [R|Rs], I) -> [R|put_reg_1(V, Rs, I+1)];
put_reg_1(V, [], I) -> [{I,V}].

fetch_reg(V, [{I,V}|_]) -> {x,I};
fetch_reg(V, [_|SRs]) -> fetch_reg(V, SRs).

live_regs([{_,reserved}|_]) ->
    %% We are not sure that this register is initialized, so we must
    %% abort the optimization.
    throw(gc_bif_alloc_failure);
live_regs([{I,_}]) ->
    I+1;
live_regs([{_,_}|Regs]) ->
    live_regs(Regs);
live_regs([]) ->
    0.

    
%%%
%%% Convert a block to Static Single Assignment (SSA) form.
%%%

-record(ssa,
	{live=0,				%Variable counter.
	 sub=gb_trees:empty(),			%Substitution table.
	 prot=gb_sets:empty(),			%Targets assigned by protecteds.
	 in_prot=false				%Inside a protected.
	}).
	 
ssa_block(Is0) ->
    {Is,_} = ssa_block_1(Is0, #ssa{}, []),
    Is.

ssa_block_1([{protected,[_],Pa0,Pb}|Is], Sub0, Acc) ->
    {Pa,Sub1} = ssa_block_1(Pa0, Sub0#ssa{in_prot=true}, []),
    Dst = ssa_last_target(Pa),
    Sub = Sub1#ssa{prot=gb_sets:insert(Dst, Sub1#ssa.prot),
		   in_prot=Sub0#ssa.in_prot},
    ssa_block_1(Is, Sub, [{protected,[Dst],Pa,Pb}|Acc]);
ssa_block_1([{set,[Dst],As,Bif}|Is], Sub0, Acc0) ->
    Sub1 = ssa_in_use_list(As, Sub0),
    Sub = ssa_assign(Dst, Sub1),
    Acc = [{set,[ssa_sub(Dst, Sub)],ssa_sub_list(As, Sub0),Bif}|Acc0],
    ssa_block_1(Is, Sub, Acc);
ssa_block_1([], Sub, Acc) -> {reverse(Acc),Sub}.

ssa_in_use_list(As, Sub) ->
    foldl(fun ssa_in_use/2, Sub, As).

ssa_in_use({x,_}=R, #ssa{sub=Sub0}=Ssa) ->
    case gb_trees:is_defined(R, Sub0) of
	true -> Ssa;
	false ->
	    Sub = gb_trees:insert(R, R, Sub0),
	    Ssa#ssa{sub=Sub}
    end;
ssa_in_use(_, Ssa) -> Ssa.

ssa_assign({x,_}=R, #ssa{sub=Sub0}=Ssa0) ->
    {NewReg,Ssa} = ssa_new_reg(Ssa0),
    case gb_trees:is_defined(R, Sub0) of
	false ->
	    Sub = gb_trees:insert(R, NewReg, Sub0),
	    Ssa#ssa{sub=Sub};
	true ->
	    Sub1 = gb_trees:update(R, NewReg, Sub0),
	    Sub = gb_trees:insert(NewReg, NewReg, Sub1),
	    Ssa#ssa{sub=Sub}
    end.

ssa_sub_list(List, Sub) ->
    [ssa_sub(E, Sub) || E <- List].

ssa_sub(R0, #ssa{sub=Sub,prot=Prot,in_prot=InProt}) ->
    case gb_trees:lookup(R0, Sub) of
	none -> R0;
	{value,R} ->
	    case InProt andalso gb_sets:is_element(R, Prot) of
		true ->
		    throw(protected_violation);
		false ->
		    R
	    end
    end.

ssa_new_reg(#ssa{live=Reg}=Ssa) ->
    {{tmp,Reg},Ssa#ssa{live=Reg+1}}.

ssa_last_target([{set,[Dst],_,_}]) -> Dst;
ssa_last_target([_|Is]) -> ssa_last_target(Is).

%% is_killed(Register, [Instruction], FailLabel, State) -> true|false
%%  Determine whether a register is killed in the instruction sequence.
%%  The state is used to allow us to determine the kill state
%%  across branches.

is_killed(R, Is, Label, #st{ll=Ll}) ->
    beam_utils:is_killed(R, Is, Ll) andalso
	beam_utils:is_killed_at(R, Label, Ll).

%% is_not_used(Register, [Instruction], FailLabel, State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still referenced by an allocate instruction, meaning that
%%  it MUST be initialized).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used(R, Is, Label, #st{ll=Ll}) ->
    beam_utils:is_not_used(R, Is, Ll) andalso
	beam_utils:is_not_used_at(R, Label, Ll).

%% initialized_regs([Instruction]) -> [Register])
%%  Given a REVERSED instruction sequence, return a list of the registers
%%  that are guaranteed to be initialized (not contain garbage).

initialized_regs(Is) ->
    initialized_regs(Is, ordsets:new()).

initialized_regs([{set,Dst,_Src,{alloc,Live,_}}|_], Regs0) ->
    Regs = add_init_regs(free_vars_regs(Live), Regs0),
    add_init_regs(Dst, Regs);
initialized_regs([{set,Dst,Src,_}|Is], Regs) ->
    initialized_regs(Is, add_init_regs(Dst, add_init_regs(Src, Regs)));
initialized_regs([{test,_,_,Src}|Is], Regs) ->
    initialized_regs(Is, add_init_regs(Src, Regs));
initialized_regs([{block,Bl}|Is], Regs) ->
    initialized_regs(reverse(Bl, Is), Regs);
initialized_regs([{bs_context_to_binary,Src}|Is], Regs) ->
    initialized_regs(Is, add_init_regs([Src], Regs));
initialized_regs([{label,_},{func_info,_,_,Arity}|_], Regs) ->
    InitRegs = free_vars_regs(Arity),
    add_init_regs(InitRegs, Regs);
initialized_regs([_|_], Regs) -> Regs.

add_init_regs([{x,_}=X|T], Regs) ->
    add_init_regs(T, ordsets:add_element(X, Regs));
add_init_regs([_|T], Regs) ->
    add_init_regs(T, Regs);
add_init_regs([], Regs) -> Regs.
