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
%%     $Id: beam_bool.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%% Purpose: Optimizes booleans in guards.

-module(beam_bool).

-export([module/2]).

-import(lists, [reverse/1,foldl/3,mapfoldl/3,sort/1,member/2]).
-define(MAXREG, 1024).

-record(st,
	{next,					%Next label number.
	 ll					%Live regs at labels.
	}).

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    %%io:format("~p:\n", [Mod]),
    {Fs,_} = mapfoldl(fun(Fn, Lbl) -> function(Fn, Lbl) end, 100000000, Fs0),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}, Lbl0) ->
    %%io:format("~p/~p:\n", [Name,Arity]),
    {Is,#st{next=Lbl}} = bool_opt(Is0, Lbl0),
    {{function,Name,Arity,CLabel,Is},Lbl}.

%%
%% Optimize boolean expressions that use guard bifs. Rewrite to
%% use test instructions if possible.
%%

bool_opt(Asm, Lbl) ->
    LiveInfo = index_instructions(Asm),
    bopt(Asm, [], #st{next=Lbl,ll=LiveInfo}).

bopt([{block,Bl0}=Block|
      [{jump,{f,Succ}},
       {label,Fail},
       {block,[{set,[Dst],[{atom,false}],move},{'%live',Live}]},
       {label,Succ}|Is]=Is0], Acc0, St) ->
    case split_block(Bl0, Dst, Fail) of
	failed ->
	    bopt(Is0, [Block|Acc0], St);
	{Bl,PreBlock} ->
	    Acc1 = case PreBlock of
		       [] -> Acc0;
		       _ -> [{block,PreBlock}|Acc0]
		   end,
	    Acc = [{protected,[Dst],Bl,{Fail,Succ,Live}}|Acc1],
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

bopt_reverse([{protected,[Dst],Block,{Fail,Succ,Live}}|Is], Acc0) ->
    Acc = [{block,Block},{jump,{f,Succ}},
	   {label,Fail},
	   {block,[{set,[Dst],[{atom,false}],move},{'%live',Live}]},
	   {label,Succ}|Acc0],
    bopt_reverse(Is, Acc);
bopt_reverse([I|Is], Acc) ->
    bopt_reverse(Is, [I|Acc]);
bopt_reverse([], Acc) -> Acc.

%% bopt_block(Reg, Fail, OldIs, Accumulator, St) -> failed | {NewAcc,St}
%%  Attempt to optimized a block of guard BIFs followed by a test
%%  instruction.
bopt_block(Reg, Fail, OldIs, [{block,Bl0}|Acc0], St0) ->
    case split_block(Bl0, Reg, Fail) of
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
	    case catch bopt_block_1(Bl, Fail, St0) of
		{'EXIT',_Reason} ->
		    %% Optimization failed for one of the following reasons:
		    %%
		    %% 1. Not possible to rewrite because a boolean value is
		    %%    passed to another guard bif, e.g. 'abs(A > B)'
		    %%    (in this case, obviously nonsense code). Rare in
		    %%    practice.
		    %%
		    %% 2. Not possible to rewrite because we have not seen
		    %%    the complete boolan expression (it is spread out
		    %%    over several blocks with jumps and labels).
		    %%    The 'or' and 'and' instructions need to that fully
		    %%    known operands in order to be eliminated.
		    %%
		    %% 3. Other bug or limitation.

		    %%io:format("~P\n", [_Reason,20]),
		    failed;
		{NewCode,St} ->
		    case is_opt_safe(Bl, NewCode, OldIs, St) of
			false ->
			    %% The optimization is not safe. (A register
			    %% used by the instructions following the
			    %% optimized code is either not assigned a
			    %% value at all or assigned a different value.)

			    %%io:format("\nNot safe:\n"),
			    %%io:format("~p\n", [Bl]),
			    %%io:format("~p\n", [reverse(NewCode)]),
			    failed;
			true -> {NewCode++Acc,St}
		    end
	    end
    end.

bopt_block_1(Block, Fail, St) ->
    {Pre0,[{_,Tree}]} = bopt_tree(Block),
    Pre = update_fail_label(Pre0, Fail, []),
    bopt_cg(Tree, Fail, make_block(Pre, []), St).

%% is_opt_safe(OriginalCode, OptCode, FollowingCode, State) -> true|false
%%  Comparing the original code to the optimized code, determine
%%  whether the optimized code is guaranteed to work in the same
%%  way as the original code.

is_opt_safe(Bl, NewCode, OldIs, St) ->
    %% Here are the conditions that must be true for the
    %% optimization to be safe.
    %%
    %% 1. Any register that was assigned a value in the original
    %%    code, but is not in the optimized code, must be guaranteed
    %%    to be KILLED in the following code. (NotSet below.)
    %%
    %% 2. Any register that is assigned a value in the optimized
    %%    code must be UNUSED in the following code. (NewDst, Set.)
    %%    (Possible future improvement: Registers that are known
    %%    to be assigned the SAME value in the original and optimized
    %%    code don't need to be unused in the following code.)

    PrevDst = dst_regs(Bl),
    NewDst = dst_regs(NewCode),
    NotSet = ordsets:subtract(PrevDst, NewDst),

    %% Note: The following line is an optimization. We don't need
    %% to test whether variables in NotSet for being unused, because
    %% they will all be tested for being killed (a stronger condition
    %% than being unused).

    Set = ordsets:subtract(NewDst, NotSet),

    all_killed(NotSet, OldIs, St) andalso
	none_used(Set, OldIs, St).

% update_fail_label([{set,_,_,{bif,_,{f,0}}}=I|Is], Fail, Acc) ->
%     update_fail_label(Is, Fail, [I|Acc]);
update_fail_label([{set,Ds,As,{bif,N,{f,_}}}|Is], Fail, Acc) ->
    update_fail_label(Is, Fail, [{set,Ds,As,{bif,N,{f,Fail}}}|Acc]);
update_fail_label([], _, Acc) -> Acc.

make_block([], Acc) -> Acc;
make_block(Bl, Acc) -> [{block,Bl}|Acc].

extend_block(BlAcc, Fail, [{protected,_,_,_}=Prot|OldAcc]) ->
    extend_block([Prot|BlAcc], Fail, OldAcc);
extend_block(BlAcc0, Fail, [{block,Is0}|OldAcc]=OldAcc0) ->
    case extend_block_1(reverse(Is0), Fail, BlAcc0) of
	{[],_} -> {BlAcc0,OldAcc0};
	{BlAcc,[]} -> extend_block(BlAcc, Fail, OldAcc);
	{BlAcc,Is} -> {BlAcc,[{block,Is}|OldAcc]}
    end;
extend_block(BlAcc, _, OldAcc) -> {BlAcc,OldAcc}.

extend_block_1([{set,[_],_,{bif,_,{f,Fail}}}=I|Is], Fail, Acc) ->
    extend_block_1(Is, Fail, [I|Acc]);
extend_block_1([{set,[_],As,{bif,Bif,_}}=I|Is]=Is0, Fail, Acc) ->
    case safe_bool_op(Bif, length(As)) of
	false -> {Acc,reverse(Is0)};
	true -> extend_block_1(Is, Fail, [I|Acc])
    end;
extend_block_1([_|_]=Is, _, Acc) -> {Acc,reverse(Is)};
extend_block_1([], _, Acc) -> {Acc,[]}.

split_block(Is0, Dst, Fail) ->
    case reverse(Is0) of
	[{'%live',_}|[{set,[Dst],_,_}|_]=Is] ->
	    split_block_1(Is, Fail);
	[{set,[Dst],_,_}|_]=Is ->
	    split_block_1(Is, Fail);
	_ -> failed
    end.

split_block_1(Is, Fail) ->
    case split_block_2(Is, Fail, []) of
	{[],_} -> failed;
	{_,_}=Res -> Res
    end.

% split_block_2([{set,[_],_,{bif,_,{f,0}}}=I|Is], Fail, Acc) ->
%     split_block_2(Is, Fail, [I|Acc]);
split_block_2([{set,[_],_,{bif,_,{f,Fail}}}=I|Is], Fail, Acc) ->
    split_block_2(Is, Fail, [I|Acc]);
split_block_2([{'%live',_}|Is], Fail, Acc) ->
    split_block_2(Is, Fail, Acc);
split_block_2(Is, _, Acc) -> {Acc,reverse(Is)}.

dst_regs(Is) ->
    dst_regs(Is, []).

dst_regs([{block,Bl}|Is], Acc) ->
    dst_regs(Bl, dst_regs(Is, Acc));
dst_regs([{set,[D],_,{bif,_,{f,_}}}|Is], Acc) ->
    dst_regs(Is, [D|Acc]);
dst_regs([_|Is], Acc) ->
    dst_regs(Is, Acc);
dst_regs([], Acc) -> ordsets:from_list(Acc).

all_killed([R|Rs], OldIs, St) ->
    case is_killed(R, OldIs, St) of
	false -> false;
	true -> all_killed(Rs, OldIs, St)
    end;
all_killed([], _, _) -> true.

none_used([R|Rs], OldIs, St) ->
    case is_not_used(R, OldIs, St) of
	false -> false;
	true -> none_used(Rs, OldIs, St)
    end;
none_used([], _, _) -> true.

bopt_tree(Block0) ->
    Block = ssa_block(Block0),
    Reg = free_variables(Block),
    %%io:format("~p\n", [Block]),
    %%io:format("~p\n", [Reg]),
    Res = bopt_tree_1(Block, Reg, []),
    %%io:format("~p\n", [Res]),
    Res.

bopt_tree_1([{set,[Dst],As0,{bif,'not',_}}|Is], Forest0, Pre) ->
    {[Arg],Forest1} = bopt_bool_args(As0, Forest0),
    Forest = gb_trees:enter(Dst, {'not',Arg}, Forest1),
    bopt_tree_1(Is, Forest, Pre);
bopt_tree_1([{set,[Dst],As0,{bif,'and',_}}|Is], Forest0, Pre) ->
    {As,Forest1} = bopt_bool_args(As0, Forest0),
    AndList = make_and_list(As),
    Forest = gb_trees:enter(Dst, {'and',AndList}, Forest1),
    bopt_tree_1(Is, Forest, Pre);
bopt_tree_1([{set,[Dst],[L0,R0],{bif,'or',_}}|Is], Forest0, Pre) ->
    L = gb_trees:get(L0, Forest0),
    R = gb_trees:get(R0, Forest0),
    Forest1 = gb_trees:delete(L0, gb_trees:delete(R0, Forest0)),
    OrList = make_or_list([L,R]),
    Forest = gb_trees:enter(Dst, {'or',OrList}, Forest1),
    bopt_tree_1(Is, Forest, Pre);
bopt_tree_1([{protected,[Dst],_,_}=Prot|Is], Forest0, Pre) ->
    Forest = gb_trees:enter(Dst, Prot, Forest0),
    bopt_tree_1(Is, Forest, Pre);
bopt_tree_1([{set,[Dst],As,{bif,N,_}}=Bif|Is], Forest0, Pre) ->
    Ar = length(As),
    case safe_bool_op(N, Ar) of
	false ->
	    bopt_good_args(As, Forest0),
	    Forest = gb_trees:enter(Dst, any, Forest0),
	    bopt_tree_1(Is, Forest, [Bif|Pre]);
	true ->
	    bopt_good_args(As, Forest0),
	    Test = bif_to_test(Dst, N, As),
	    Forest = gb_trees:enter(Dst, Test, Forest0),
	    bopt_tree_1(Is, Forest, Pre)
    end;
bopt_tree_1([], Forest, Pre) ->
    {Pre,[R || {_,V}=R <- gb_trees:to_list(Forest), V =/= any]}.

safe_bool_op(internal_is_record, 3) -> true;
safe_bool_op(N, Ar) ->
    erl_internal:new_type_test(N, Ar) orelse erl_internal:comp_op(N, Ar).

bopt_bool_args(As, Forest) ->
    mapfoldl(fun bopt_bool_arg/2, Forest, As).

bopt_bool_arg({T,_}=R, Forest) when T == x; T == y ->
    {gb_trees:get(R, Forest),gb_trees:delete(R, Forest)};
bopt_bool_arg(Term, Forest) ->
    {Term,Forest}.

bopt_good_args([A|As], Regs) ->
    bopt_good_arg(A, Regs),
    bopt_good_args(As, Regs);
bopt_good_args([], _) -> ok.

bopt_good_arg({x,_}=X, Regs) ->
    case gb_trees:get(X, Regs) of
	any -> ok;
	_Other ->
	    %%io:format("not any: ~p: ~p\n", [X,_Other]),
	    exit(bad_contents)
    end;
bopt_good_arg(_, _) -> ok.

bif_to_test(_, N, As) ->
    bif_to_test(N, As).

bif_to_test(internal_is_record, [_,_,_]=As) ->
    {test,internal_is_record,fail,As};
bif_to_test('=:=', As) -> {test,is_eq_exact,fail,As};
bif_to_test('=/=', As) -> {test,is_ne_exact,fail,As};
bif_to_test('==', As) -> {test,is_eq,fail,As};
bif_to_test('/=', As) -> {test,is_ne,fail,As};
bif_to_test('=<', [L,R]) -> {test,is_ge,fail,[R,L]};
bif_to_test('>=', As) -> {test,is_ge,fail,As};
bif_to_test('>', [L,R]) -> {test,is_lt,fail,[R,L]};
bif_to_test('<', As) -> {test,is_lt,fail,As};
bif_to_test(Name, [_]=As) ->
    case erl_internal:new_type_test(Name, 1) of
	false -> exit({bif_to_test,Name,As,failed});
	true -> {test,Name,fail,As}
    end.

make_and_list([{'and',As}|Is]) ->
    make_and_list(As++Is);
make_and_list([I|Is]) ->
    [I|make_and_list(Is)];
make_and_list([]) -> [].

make_or_list([{'or',As}|Is]) ->
    make_or_list(As++Is);
make_or_list([I|Is]) ->
    [I|make_or_list(Is)];
make_or_list([]) -> [].

%% Code generation for a boolean tree.

bopt_cg({'not',Arg}, Fail, Acc, St) ->
    I = bopt_cg_not(Arg),
    bopt_cg(I, Fail, Acc, St);
bopt_cg({'and',As}, Fail, Acc, St) ->
    bopt_cg_and(As, Fail, Acc, St);
bopt_cg({'or',As}, Fail, Acc, St0) ->
    {Succ,St} = new_label(St0),
    bopt_cg_or(As, Succ, Fail, Acc, St);
bopt_cg({test,is_tuple_element,fail,[Tmp,Tuple,RecordTag]}, Fail, Acc, St) ->
    {[{test,is_eq_exact,{f,Fail},[Tmp,RecordTag]},
      {get_tuple_element,Tuple,0,Tmp}|Acc],St};
bopt_cg({inverted_test,is_tuple_element,fail,[Tmp,Tuple,RecordTag]}, Fail, Acc, St) ->
    {[{test,is_ne_exact,{f,Fail},[Tmp,RecordTag]},
      {get_tuple_element,Tuple,0,Tmp}|Acc],St};
bopt_cg({test,N,fail,As}, Fail, Acc, St) ->
    Test = {test,N,{f,Fail},As},
    {[Test|Acc],St};
bopt_cg({inverted_test,N,fail,As}, Fail, Acc, St0) ->
    {Lbl,St} = new_label(St0),
    {[{label,Lbl},{jump,{f,Fail}},{test,N,{f,Lbl},As}|Acc],St};
bopt_cg({protected,_,Bl0,{_,_,_}}, Fail, Acc, St0) ->
    {Bl,St} = bopt_block_1(Bl0, Fail, St0),
    {Bl++Acc,St};
bopt_cg([_|_]=And, Fail, Acc, St) ->
    bopt_cg_and(And, Fail, Acc, St).

bopt_cg_not({'and',As0}) ->
    As = [bopt_cg_not(A) || A <- As0],
    {'or',As};
bopt_cg_not({'or',As0}) ->
    As = [bopt_cg_not(A) || A <- As0],
    {'and',As};
bopt_cg_not({test,Test,Fail,As}) ->
    {inverted_test,Test,Fail,As}.

bopt_cg_and([{atom,false}|_], Fail, _, St) ->
    {[{jump,{f,Fail}}],St};
bopt_cg_and([{atom,true}|Is], Fail, Acc, St) ->
    bopt_cg_and(Is, Fail, Acc, St);
bopt_cg_and([I|Is], Fail, Acc0, St0) ->
    {Acc,St} = bopt_cg(I, Fail, Acc0, St0),
    bopt_cg_and(Is, Fail, Acc, St);
bopt_cg_and([], _, Acc, St) -> {Acc,St}.

bopt_cg_or([I], Succ, Fail, Acc0, St0) ->
    {Acc,St} = bopt_cg(I, Fail, Acc0, St0),
    {[{label,Succ}|Acc],St};
bopt_cg_or([I|Is], Succ, Fail, Acc0, St0) ->
    {Lbl,St1} = new_label(St0),
    {Acc,St} = bopt_cg(I, Lbl, Acc0, St1),
    bopt_cg_or(Is, Succ, Fail, [{label,Lbl},{jump,{f,Succ}}|Acc], St).

new_label(#st{next=LabelNum}=St) when is_integer(LabelNum) ->
    {LabelNum,St#st{next=LabelNum+1}}.

free_variables(Is) ->
    E = gb_sets:empty(),
    free_vars_1(Is, E, E).

free_vars_1([{set,[Dst],As,{bif,_,_}}|Is], F0, N0) ->
    F = gb_sets:union(F0, gb_sets:difference(var_list(As), N0)),
    N = gb_sets:union(N0, var_list([Dst])),
    free_vars_1(Is, F, N);
free_vars_1([{protected,_,Pa,_}|Is], F, N) ->
    free_vars_1(Pa++Is, F, N);
free_vars_1([], F, _) ->
    gb_trees:from_orddict([{K,any} || K <- gb_sets:to_list(F)]).

var_list(Is) ->
    var_list_1(Is, gb_sets:empty()).

var_list_1([{x,_}=X|Is], D) ->
    var_list_1(Is, gb_sets:add(X, D));
var_list_1([_|Is], D) ->
    var_list_1(Is, D);
var_list_1([], D) -> D.

%%%
%%% Convert a block to Static Single Assignment (SSA) form.
%%%

-record(ssa,
	{live,
	 sub}).

ssa_block(Is0) ->
    Next = ssa_first_free(Is0, 0),
    {Is,_} = ssa_block_1(Is0, #ssa{live=Next,sub=gb_trees:empty()}, []),
    Is.

ssa_block_1([{protected,[_],Pa0,Pb}|Is], Sub0, Acc) ->
    {Pa,Sub} = ssa_block_1(Pa0, Sub0, []),
    Dst = ssa_last_target(Pa),
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
    case gb_trees:is_defined(R, Sub0) of
	false ->
	    Sub = gb_trees:insert(R, R, Sub0),
	    Ssa0#ssa{sub=Sub};
	true ->
	    {NewReg,Ssa} = ssa_new_reg(Ssa0),
	    Sub1 = gb_trees:update(R, NewReg, Sub0),
	    Sub = gb_trees:insert(NewReg, NewReg, Sub1),
	    Ssa#ssa{sub=Sub}
    end;
ssa_assign(_, Ssa) -> Ssa.

ssa_sub_list(List, Sub) ->
    [ssa_sub(E, Sub) || E <- List].

ssa_sub(R0, #ssa{sub=Sub}) ->
    case gb_trees:lookup(R0, Sub) of
	none -> R0;
	{value,R} -> R
    end.

ssa_new_reg(#ssa{live=Reg}=Ssa) ->
    {{x,Reg},Ssa#ssa{live=Reg+1}}.

ssa_first_free([{protected,Ds,_,_}|Is], Next0) ->
    Next = ssa_first_free_list(Ds, Next0),
    ssa_first_free(Is, Next);
ssa_first_free([{set,[Dst],As,_}|Is], Next0) ->
    Next = ssa_first_free_list([Dst|As], Next0),
    ssa_first_free(Is, Next);
ssa_first_free([], Next) -> Next.

ssa_first_free_list(Regs, Next) ->
    foldl(fun({x,R}, N) when R >= N -> R+1;
	     (_, N) -> N end, Next, Regs).

ssa_last_target([{set,[Dst],_,_},{'%live',_}]) -> Dst;
ssa_last_target([{set,[Dst],_,_}]) -> Dst;
ssa_last_target([_|Is]) -> ssa_last_target(Is).

%% index_instructions(FunctionIs) -> GbTree([{Label,Is}])
%%  Index the instruction sequence so that we can quickly
%%  look up the instruction following a specific label.

index_instructions(Is) ->
    ii_1(Is, []).

ii_1([{label,Lbl}|Is0], Acc) ->
    Is = lists:dropwhile(fun({label,_}) -> true;
			    (_) -> false end, Is0),
    ii_1(Is0, [{Lbl,Is}|Acc]);
ii_1([_|Is], Acc) ->
    ii_1(Is, Acc);
ii_1([], Acc) -> gb_trees:from_orddict(sort(Acc)).

%% is_killed(Register, [Instruction], State) -> true|false
%%  Determine whether a register is killed in the instruction sequence.
%%  The state is used to allow us to determine the kill state
%%  across branches.

is_killed(R, Is, St) ->
    case is_killed_1(R, Is, St) of
	false ->
	    %%io:format("nk ~p: ~P\n", [R,Is,15]),
	    false;
	true -> true
    end.

is_killed_1(R, [{block,Blk}|Is], St) ->
    case is_killed_1(R, Blk, St) of
	true -> true;
	false -> is_killed_1(R, Is, St)
    end;
is_killed_1(R, [{test,_,{f,Fail},As}|Is], St) ->
    case not member(R, As) andalso is_reg_killed_at(R, Fail, St) of
	false -> false;
	true -> is_killed_1(R, Is, St)
    end;
is_killed_1(R, [{select_val,R,_,_}|_], _) -> false;
is_killed_1(R, [{select_val,_,Fail,{list,Branches}}|_], St) ->
    is_killed_at_all(R, [Fail|Branches], St);
is_killed_1(R, [{jump,{f,F}}|_], St) ->
    is_reg_killed_at(R, F, St);
is_killed_1(Reg, Is, _) ->
    beam_block:is_killed(Reg, Is).

is_reg_killed_at(R, Lbl, #st{ll=Ll}=St) ->
    Is = gb_trees:get(Lbl, Ll),
    is_killed_1(R, Is, St).

is_killed_at_all(R, [{f,Lbl}|T], St) ->
    case is_reg_killed_at(R, Lbl, St) of
	false -> false;
	true -> is_killed_at_all(R, T, St)
    end;
is_killed_at_all(R, [_|T], St) ->
    is_killed_at_all(R, T, St);
is_killed_at_all(_, [], _) -> true.

%% is_not_used(Register, [Instruction], State) -> true|false
%%  Determine whether a register is never used in the instruction sequence
%%  (it could still referenced by an allocate instruction, meaning that
%%  it MUST be initialized).
%%    The state is used to allow us to determine the usage state
%%  across branches.

is_not_used(R, Is, St) ->
    case is_not_used_1(R, Is, St) of
	false ->
	    %%io:format("used ~p: ~P\n", [R,Is,15]),
	    false;
	true -> true
    end.

is_not_used_1(R, [{block,Blk}|Is], St) ->
    case is_not_used_1(R, Blk, St) of
	true -> true;
	false -> is_not_used_1(R, Is, St)
    end;
is_not_used_1(R, [{test,_,{f,Fail},As}|Is], St) ->
    case not member(R, As) andalso is_reg_not_used_at(R, Fail, St) of
	false -> false;
	true -> is_not_used_1(R, Is, St)
    end;
is_not_used_1(R, [{select_val,R,_,_}|_], _) -> false;
is_not_used_1(R, [{select_val,_,Fail,{list,Branches}}|_], St) ->
    is_used_at_none(R, [Fail|Branches], St);
is_not_used_1(R, [{jump,{f,F}}|_], St) ->
    is_reg_not_used_at(R, F, St);
is_not_used_1(Reg, Is, _) ->
    beam_block:is_not_used(Reg, Is).

is_reg_not_used_at(R, Lbl, #st{ll=Ll}=St) ->
    Is = gb_trees:get(Lbl, Ll),
    is_not_used_1(R, Is, St).

is_used_at_none(R, [{f,Lbl}|T], St) ->
    case is_reg_not_used_at(R, Lbl, St) of
	false -> false;
	true -> is_used_at_none(R, T, St)
    end;
is_used_at_none(R, [_|T], St) ->
    is_used_at_none(R, T, St);
is_used_at_none(_, [], _) -> true.
