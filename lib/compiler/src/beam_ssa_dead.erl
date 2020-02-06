%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
%% Dead code is code that is executed but has no effect. This
%% optimization pass either removes dead code or jumps around it,
%% potentially making it unreachable so that it can be dropped
%% the next time beam_ssa:linearize/1 is called.
%%

-module(beam_ssa_dead).
-export([opt/1]).

-include("beam_ssa.hrl").
-import(lists, [append/1,keymember/3,last/1,member/2,
                reverse/1,sort/1,takewhile/2]).

-type used_vars() :: #{beam_ssa:label():=cerl_sets:set(beam_ssa:var_name())}.

-type basic_type_test() :: atom() | {'is_tagged_tuple',pos_integer(),atom()}.
-type type_test() :: basic_type_test() | {'not',basic_type_test()}.
-type op_name() :: atom().
-type basic_rel_op() :: {op_name(),beam_ssa:b_var(),beam_ssa:value()} |
                         {basic_type_test(),beam_ssa:value()}.
-type rel_op() :: {op_name(),beam_ssa:b_var(),beam_ssa:value()} |
                  {type_test(),beam_ssa:value()}.

-record(st,
        {bs :: beam_ssa:block_map(),
         us :: used_vars(),
         skippable :: #{beam_ssa:label():='true'},
         rel_op=none :: 'none' | rel_op(),
         target=any :: 'any' | 'one_way' | beam_ssa:label()
        }).

-spec opt([{Label0,Block0}]) -> [{Label,Block}] when
      Label0 :: beam_ssa:label(),
      Block0 :: beam_ssa:b_blk(),
      Label :: beam_ssa:label(),
      Block :: beam_ssa:b_blk().

opt(Linear) ->
    {Used,Skippable} = used_vars(Linear),
    Blocks0 = maps:from_list(Linear),
    St0 = #st{bs=Blocks0,us=Used,skippable=Skippable},
    St = shortcut_opt(St0),
    #st{bs=Blocks} = combine_eqs(St#st{us=#{}}),
    beam_ssa:linearize(Blocks).

%%%
%%% Shortcut br/switch targets.
%%%
%%% A br/switch may branch to another br/switch that in turn always
%%% branches to another target. Rewrite br/switch to refer to the
%%% ultimate targets directly. That will save execution time, but
%%% could also reduce the size of the code if some of the original
%%% targets become unreachable and be deleted.
%%%
%%% When rewriting branches, we must be careful not to skip instructions
%%% that have side effects or that bind variables that will be used
%%% at the new target.
%%%
%%% We must also avoid branching to phi nodes.  The reason is
%%% twofold. First, we might create a critical edge which is strictly
%%% forbidden. Second, there will be a branch from a block that is not
%%% listed in the list of predecessors in the phi node.  Those
%%% limitations could probably be overcome, but it is not clear how
%%% much that would improve the code.
%%%

shortcut_opt(#st{bs=Blocks}=St) ->
    %% Processing the blocks in reverse post order seems to give more
    %% opportunities for optimizations compared to post order. (Based on
    %% running scripts/diffable with both PO and RPO and looking at
    %% the diff.)
    %%
    %% Unfortunately, processing the blocks in reverse post order
    %% potentially makes the time complexity quadratic, instead of
    %% linear for post order processing. We avoid drastic slowdowns by
    %% limiting how far we search forward to a common block that
    %% both the success and failure label will reach (see the comment
    %% in the first clause of shortcut_2/5).

    Ls = beam_ssa:rpo(Blocks),
    shortcut_opt(Ls, St).

shortcut_opt([L|Ls], #st{bs=Blocks0}=St) ->
    #b_blk{is=Is,last=Last0} = Blk0 = get_block(L, St),
    case shortcut_terminator(Last0, Is, L, St) of
        Last0 ->
            %% No change. No need to update the block.
            shortcut_opt(Ls, St);
        Last ->
            %% The terminator was simplified in some way.
            %% Update the block.
            Blk = Blk0#b_blk{last=Last},
            Blocks = Blocks0#{L=>Blk},
            shortcut_opt(Ls, St#st{bs=Blocks})
    end;
shortcut_opt([], St) -> St.

shortcut_terminator(#b_br{bool=#b_literal{val=true},succ=Succ0},
                    _Is, From, St0) ->
    St = St0#st{rel_op=none},
    shortcut(Succ0, From, #{}, St);
shortcut_terminator(#b_br{bool=#b_var{}=Bool,succ=Succ0,fail=Fail0}=Br,
                    Is, From, St0) ->
    St = St0#st{target=one_way},
    RelOp = get_rel_op(Bool, Is),

    %% The boolean in a `br` is seldom used by the successors. By
    %% not binding its value unless it is actually used we might be able
    %% to skip some work in shortcut/4 and sub/2.
    SuccBs = bind_var_if_used(Succ0, Bool, #b_literal{val=true}, St),
    BrSucc = shortcut(Succ0, From, SuccBs, St#st{rel_op=RelOp}),
    FailBs = bind_var_if_used(Fail0, Bool, #b_literal{val=false}, St),
    BrFail = shortcut(Fail0, From, FailBs, St#st{rel_op=invert_op(RelOp)}),

    case {BrSucc,BrFail} of
        {#b_br{bool=#b_literal{val=true},succ=Succ},
         #b_br{bool=#b_literal{val=true},succ=Fail}}
          when Succ =/= Succ0; Fail =/= Fail0 ->
            %% One or both of the targets were cut short.
            beam_ssa:normalize(Br#b_br{succ=Succ,fail=Fail});
        {_,_} ->
            %% No change.
            Br
    end;
shortcut_terminator(#b_switch{arg=Bool,fail=Fail0,list=List0}=Sw,
                    _Is, From, St) ->
    Fail = shortcut_sw_fail(Fail0, List0, Bool, From, St),
    List = shortcut_sw_list(List0, Bool, From, St),
    beam_ssa:normalize(Sw#b_switch{fail=Fail,list=List});
shortcut_terminator(Last, _Is, _From, _St) ->
    Last.

shortcut_sw_fail(Fail0, List, Bool, From, St0) ->
    case sort(List) of
        [{#b_literal{val=false},_},
         {#b_literal{val=true},_}] ->
            RelOp = {{'not',is_boolean},Bool},
            St = St0#st{rel_op=RelOp,target=one_way},
            #b_br{bool=#b_literal{val=true},succ=Fail} =
                shortcut(Fail0, From, #{}, St),
            Fail;
        _ ->
            Fail0
    end.

shortcut_sw_list([{Lit,L0}|T], Bool, From, St0) ->
    RelOp = {'=:=',Bool,Lit},
    St = St0#st{rel_op=RelOp},
    #b_br{bool=#b_literal{val=true},succ=L} =
        shortcut(L0, From, bind_var(Bool, Lit, #{}), St#st{target=one_way}),
    [{Lit,L}|shortcut_sw_list(T, Bool, From, St0)];
shortcut_sw_list([], _, _, _) -> [].

shortcut(L, _From, Bs, #st{rel_op=none,target=one_way}) when map_size(Bs) =:= 0 ->
    %% There is no way that we can find a suitable branch, because there is no
    %% relational operator stored, there are no bindings, and the block L can't
    %% have any phi nodes from which we could pick bindings because when the target
    %% is `one_way`, it implies the From block has a two-way `br` terminator.
    #b_br{bool=#b_literal{val=true},succ=L,fail=L};
shortcut(L, From, Bs, St) ->
    shortcut_1(L, From, Bs, cerl_sets:new(), St).

shortcut_1(L, From, Bs0, UnsetVars0, St) ->
    case shortcut_2(L, From, Bs0, UnsetVars0, St) of
        none ->
            %% No more shortcuts found. Package up the previous
            %% label in an unconditional branch.
            #b_br{bool=#b_literal{val=true},succ=L,fail=L};
        {#b_br{bool=#b_var{}}=Br,_,_} ->
            %% This is a two-way branch. We can't do any better.
            Br;
        {#b_br{bool=#b_literal{val=true},succ=Succ},Bs,UnsetVars} ->
            %% This is a safe `br`, but try to find a better one.
            shortcut_1(Succ, L, Bs, UnsetVars, St)
    end.

%% Try to shortcut this block, branching to a successor.
shortcut_2(L, From, Bs, UnsetVars, St) ->
    case cerl_sets:size(UnsetVars) of
        SetSize when SetSize > 128 ->
            %% This is an heuristic to limit the search for a forced label
            %% before it drastically slows down the compiler. Experiments
            %% with scripts/diffable showed that limits larger than 31 did not
            %% find any more opportunities for optimization.
            none;
        _SetSize ->
            shortcut_3(L, From, Bs, UnsetVars, St)
    end.

shortcut_3(L, From, Bs0, UnsetVars0, St) ->
    #b_blk{is=Is,last=Last} = get_block(L, St),
    case eval_is(Is, From, Bs0, St) of
        none ->
            %% It is not safe to avoid this block because it
            %% has instructions with potential side effects.
            none;
        Bs ->
            %% The instructions in the block (if any) don't
            %% have any side effects and can be skipped.
            %% Evaluate the terminator.
            case eval_terminator(Last, Bs, St) of
                none ->
                    %% The terminator is not suitable (could be
                    %% because it is a switch that can't be simplified
                    %% or it is a ret instruction).
                    none;
                #b_br{}=Br ->
                    %% We have a potentially suitable br.
                    %% Now update the set of variables that will never
                    %% be set if this block will be skipped.
                    case update_unset_vars(L, Is, Br, UnsetVars0, St) of
                        unsafe ->
                            %% It is unsafe to use this br,
                            %% because it refers to a variable defined
                            %% in this block.
                            shortcut_unsafe_br(Br, L, Bs, UnsetVars0, St);
                        UnsetVars ->
                            %% Continue checking whether this br is
                            %% suitable.
                            shortcut_test_br(Br, L, Bs, UnsetVars, St)
                    end
            end
    end.

shortcut_test_br(Br, From, Bs, UnsetVars, St) ->
    case is_br_safe(UnsetVars, Br, St) of
        false ->
            shortcut_unsafe_br(Br, From, Bs, UnsetVars, St);
        true ->
            shortcut_safe_br(Br, From, Bs, UnsetVars, St)
    end.

shortcut_unsafe_br(Br, From, Bs, UnsetVars, #st{target=Target}=St) ->
    %% Branching using this `br` is unsafe, either because it
    %% is an unconditional branch to a phi node, or because
    %% one or more of the variables that are not set will be
    %% used. Try to follow branches of this `br`, to find a
    %% safe `br`.
    case Br of
        #b_br{bool=#b_literal{val=true},succ=L} ->
            case Target of
                L ->
                    %% We have reached the forced target, and it
                    %% is unsafe. Give up.
                    none;
                _ ->
                    %% Try following this branch to see whether it
                    %% leads to a safe `br`.
                    shortcut_2(L, From, Bs, UnsetVars, St)
            end;
        #b_br{bool=#b_var{},succ=Succ,fail=Fail} ->
            case {Succ,Fail} of
                {L,Target} ->
                    %% The failure label is the forced target.
                    %% Try following the success label to see
                    %% whether it also ultimately ends up at the
                    %% forced target.
                    shortcut_2(L, From, Bs, UnsetVars, St);
                {Target,L} ->
                    %% The success label is the forced target.
                    %% Try following the failure label to see
                    %% whether it also ultimately ends up at the
                    %% forced target.
                    shortcut_2(L, From, Bs, UnsetVars, St);
                {_,_} ->
                    case Target of
                        any ->
                            %% This two-way branch is unsafe. Try
                            %% reducing it to a one-way branch.
                            shortcut_two_way(Br, From, Bs, UnsetVars, St);
                        one_way ->
                            %% This two-way branch is unsafe. Try
                            %% reducing it to a one-way branch.
                            shortcut_two_way(Br, From, Bs, UnsetVars, St);
                        _ when is_integer(Target) ->
                            %% This two-way branch is unsafe, and
                            %% there already is a forced target.
                            %% Give up.
                            none
                    end
            end
    end.

shortcut_safe_br(Br, From, Bs, UnsetVars, #st{target=Target}=St) ->
    %% This `br` instruction is safe. It does not branch to a phi
    %% node, and all variables that will be used are guaranteed to be
    %% defined.
    case Br of
        #b_br{bool=#b_literal{val=true},succ=L} ->
            %% This is a one-way branch.
            case Target of
                any ->
                    %% No forced target. Success!
                    {Br,Bs,UnsetVars};
                one_way ->
                    %% The target must be a one-way branch, which this
                    %% `br` is. Success!
                    {Br,Bs,UnsetVars};
                L when is_integer(Target) ->
                    %% The forced target is L. Success!
                    {Br,Bs,UnsetVars};
                _ when is_integer(Target) ->
                    %% Wrong forced target. Try following this branch
                    %% to see if it ultimately ends up at the forced
                    %% target.
                    shortcut_2(L, From, Bs, UnsetVars, St)
            end;
        #b_br{bool=#b_var{}} ->
            %% This is a two-way branch.
            if
                Target =:= any; Target =:= one_way ->
                    %% No specific forced target. Try to reduce the
                    %% two-way branch to an one-way branch.
                    case shortcut_two_way(Br, From, Bs, UnsetVars, St) of
                        none when Target =:= any ->
                            %% This `br` can't be reduced to a one-way
                            %% branch. Return the `br` as-is.
                            {Br,Bs,UnsetVars};
                        none when Target =:= one_way ->
                            %% This `br` can't be reduced to a one-way
                            %% branch. The caller wants a one-way
                            %% branch.  Give up.
                            none;
                        {_,_,_}=Res ->
                            %% This `br` was successfully reduced to a
                            %% one-way branch.
                            Res
                    end;
                is_integer(Target) ->
                    %% There is a forced target, which can't
                    %% be reached because this `br` is a two-way
                    %% branch. Give up.
                    none
            end
    end.

update_unset_vars(L, Is, Br, UnsetVars, #st{skippable=Skippable}) ->
    case is_map_key(L, Skippable) of
        true ->
            %% None of the variables used in this block are used in
            %% the successors. Thus, there is no need to add the
            %% variables to the set of unset variables.
            case Br of
                #b_br{bool=#b_var{}=Bool} ->
                    case keymember(Bool, #b_set.dst, Is) of
                        true ->
                            %% Bool is a variable defined in this
                            %% block. Using the br instruction from
                            %% this block (and skipping the body of
                            %% the block) is unsafe.
                            unsafe;
                        false ->
                            %% Bool is either a variable not defined
                            %% in this block or a literal. Adding it
                            %% to the UnsetVars set would not change
                            %% the outcome of the tests in
                            %% is_br_safe/2.
                            UnsetVars
                    end;
                #b_br{} ->
                    UnsetVars
            end;
        false ->
            %% Some variables defined in this block are used by
            %% successors. We must update the set of unset variables.
            SetInThisBlock = [V || #b_set{dst=V} <- Is],
            cerl_sets:union(UnsetVars, cerl_sets:from_list(SetInThisBlock))
    end.

shortcut_two_way(#b_br{succ=Succ,fail=Fail}, From, Bs0, UnsetVars0, St0) ->
    case shortcut_2(Succ, From, Bs0, UnsetVars0, St0#st{target=Fail}) of
        {#b_br{bool=#b_literal{},succ=Fail},_,_}=Res ->
            Res;
        none ->
            St = St0#st{target=Succ},
            case shortcut_2(Fail, From, Bs0, UnsetVars0, St) of
                {#b_br{bool=#b_literal{},succ=Succ},_,_}=Res ->
                    Res;
                none ->
                    none
            end
    end.

get_block(L, St) ->
    #st{bs=#{L:=Blk}} = St,
    Blk.

is_br_safe(UnsetVars, Br, #st{us=Us}=St) ->
    %% Check that none of the unset variables will be used.
    case Br of
        #b_br{bool=#b_var{}=V,succ=Succ,fail=Fail} ->
            #{Succ:=Used0,Fail:=Used1} = Us,

            %% A two-way branch never branches to a phi node, so there
            %% is no need to check for phi nodes here.
            not cerl_sets:is_element(V, UnsetVars) andalso
                cerl_sets:is_disjoint(Used0, UnsetVars) andalso
                cerl_sets:is_disjoint(Used1, UnsetVars);
        #b_br{succ=Same,fail=Same} ->
            %% An unconditional branch must not jump to
            %% a phi node.
            not is_forbidden(Same, St) andalso
                cerl_sets:is_disjoint(map_get(Same, Us), UnsetVars)
    end.

is_forbidden(L, St) ->
    case get_block(L, St) of
        #b_blk{is=[#b_set{op=phi}|_]} ->
            true;
        #b_blk{is=[#b_set{}=I|_]} ->
            beam_ssa:is_loop_header(I);
        #b_blk{} -> false
    end.


%% Evaluate the instructions in the block.
%% Return the updated bindings, or 'none' if there is
%% any instruction with potential side effects.

eval_is([#b_set{op=phi,dst=Dst,args=Args}|Is], From, Bs0, St) ->
    Val = get_phi_arg(Args, From),
    Bs = bind_var(Dst, Val, Bs0),
    eval_is(Is, From, Bs, St);
eval_is([#b_set{op=succeeded,dst=Dst,args=[Var]}], _From, Bs, _St) ->
    case Bs of
        #{Var:=failed} ->
            bind_var(Dst, #b_literal{val=false}, Bs);
        #{Var:=#b_literal{}} ->
            bind_var(Dst, #b_literal{val=true}, Bs);
        #{} ->
            Bs
    end;
eval_is([#b_set{op={bif,_},dst=Dst}=I0|Is], From, Bs, St) ->
    I = sub(I0, Bs),
    case eval_bif(I, St) of
        #b_literal{}=Val ->
            eval_is(Is, From, bind_var(Dst, Val, Bs), St);
        failed ->
            eval_is(Is, From, bind_var(Dst, failed, Bs), St);
        none ->
            eval_is(Is, From, Bs, St)
    end;
eval_is([#b_set{op=Op,dst=Dst}=I|Is], From, Bs, St)
  when Op =:= is_tagged_tuple; Op =:= is_nonempty_list ->
    #b_set{args=Args} = sub(I, Bs),
    case eval_rel_op(Op, Args, St) of
        #b_literal{}=Val ->
            eval_is(Is, From, bind_var(Dst, Val, Bs), St);
        none ->
            eval_is(Is, From, Bs, St)
    end;
eval_is([#b_set{}=I|Is], From, Bs, St) ->
    case beam_ssa:no_side_effect(I) of
        true ->
            %% This instruction has no side effects. It can
            %% safely be omitted.
            eval_is(Is, From, Bs, St);
        false ->
            %% This instruction may have some side effect.
            %% It is not safe to avoid this instruction.
            none
    end;
eval_is([], _From, Bs, _St) -> Bs.

get_phi_arg([{Val,From}|_], From) -> Val;
get_phi_arg([_|As], From) -> get_phi_arg(As, From).

eval_terminator(#b_br{bool=#b_var{}=Bool}=Br, Bs, _St) ->
    case get_value(Bool, Bs) of
        #b_literal{val=Val}=Lit ->
            case is_boolean(Val) of
                true ->
                    beam_ssa:normalize(Br#b_br{bool=Lit});
                false ->
                    %% Non-boolean literal. This means that this `br`
                    %% terminator will never actually be reached with
                    %% these bindings. (There must be a previous two-way
                    %% branch that branches the other way when Bool
                    %% is bound to a non-boolean literal.)
                    none
            end;
        #b_var{}=Var ->
            beam_ssa:normalize(Br#b_br{bool=Var})
    end;
eval_terminator(#b_br{bool=#b_literal{}}=Br, _Bs, _St) ->
    beam_ssa:normalize(Br);
eval_terminator(#b_switch{arg=Arg,fail=Fail,list=List}=Sw, Bs, St) ->
    case get_value(Arg, Bs) of
        #b_literal{}=Val ->
            %% Literal argument. Simplify to a `br`.
            beam_ssa:normalize(Sw#b_switch{arg=Val});
        #b_var{} ->
            %% Try optimizing the switch.
            case eval_switch(List, Arg, St, Fail) of
                none ->
                    none;
                To when is_integer(To) ->
                    %% Either one of the values in the switch
                    %% matched a previous value in a '=:=' test, or
                    %% none of the values matched a previous test.
                    #b_br{bool=#b_literal{val=true},succ=To,fail=To}
            end
    end;
eval_terminator(#b_ret{}, _Bs, _St) ->
    none.

eval_switch(List, Arg, #st{rel_op={_,Arg,_}=PrevOp}, Fail) ->
    %% There is a previous relational operator testing the same variable.
    %% Optimization may be possible.
    eval_switch_1(List, Arg, PrevOp, Fail);
eval_switch(_, _, _, _) ->
    %% There is either no previous relational operator, or it tests
    %% a different variable. Nothing to optimize.
    none.

eval_switch_1([{Lit,Lbl}|T], Arg, PrevOp, Fail) ->
    RelOp = {'=:=',Arg,Lit},
    case will_succeed(PrevOp, RelOp) of
        yes ->
            %% Success. This branch will always be taken.
            Lbl;
        no ->
            %% This branch will never be taken.
            eval_switch_1(T, Arg, PrevOp, Fail);
        maybe ->
            %% This label could be reached.
            eval_switch_1(T, Arg, PrevOp, none)
    end;
eval_switch_1([], _Arg, _PrevOp, Fail) ->
    %% Fail is now either the failure label or 'none'.
    Fail.

bind_var_if_used(L, Var, Val, #st{us=Us}) ->
    case cerl_sets:is_element(Var, map_get(L, Us)) of
        true -> #{Var=>Val};
        false -> #{}
    end.

bind_var(Var, failed, Bs) ->
    Bs#{Var=>failed};
bind_var(Var, Val0, Bs) ->
    Val = get_value(Val0, Bs),
    Bs#{Var=>Val}.

get_value(#b_var{}=Var, Bs) ->
    case Bs of
        #{Var:=Val} -> get_value(Val, Bs);
        #{} -> Var
    end;
get_value(#b_literal{}=Lit, _Bs) -> Lit.

eval_bif(#b_set{op={bif,Bif},args=Args}, St) ->
    Arity = length(Args),
    case erl_bifs:is_pure(erlang, Bif, Arity) of
        false ->
            none;
        true ->
            case get_lit_args(Args) of
                none ->
                    %% Not literal arguments. Try to evaluate
                    %% it based on a previous relational operator.
                    eval_rel_op({bif,Bif}, Args, St);
                LitArgs ->
                    try apply(erlang, Bif, LitArgs) of
                        Val -> #b_literal{val=Val}
                    catch
                        error:_ -> none
                    end
            end
    end.

get_lit_args([#b_literal{val=Lit1}]) ->
    [Lit1];
get_lit_args([#b_literal{val=Lit1},
              #b_literal{val=Lit2}]) ->
    [Lit1,Lit2];
get_lit_args([#b_literal{val=Lit1},
              #b_literal{val=Lit2},
              #b_literal{val=Lit3}]) ->
    [Lit1,Lit2,Lit3];
get_lit_args(_) -> none.

%%%
%%% Handling of relational operators.
%%%

get_rel_op(Bool, [_|_]=Is) ->
    case last(Is) of
        #b_set{op=Op,dst=Bool,args=Args} ->
            normalize_op(Op, Args);
        #b_set{} ->
            none
    end;
get_rel_op(_, []) -> none.

%% normalize_op(Instruction) -> {Normalized,FailLabel} | error
%%    Normalized = {Operator,Variable,Variable|Literal} |
%%                 {TypeTest,Variable}
%%    Operation = '<' | '=<' | '=:=' | '=/=' | '>=' | '>'
%%    TypeTest = is_atom | is_integer ...
%%    Variable = #b_var{}
%%    Literal = #b_literal{}
%%
%%  Normalize a relational operator to facilitate further
%%  comparisons between operators. Always make the register
%%  operand the first operand. If there are two registers,
%%  order the registers in lexical order.
%%
%%  For example, this instruction:
%%
%%    #b_set{op={bif,=<},args=[#b_literal{}, #b_var{}}
%%
%%  will be normalized to:
%%
%%    {'=<',#b_var{},#b_literal{}}

-spec normalize_op(Op, Args) -> NormalizedOp | 'none' when
      Op :: beam_ssa:op(),
      Args :: [beam_ssa:value()],
      NormalizedOp :: basic_rel_op().

normalize_op(is_tagged_tuple, [Arg,#b_literal{val=Size},#b_literal{val=Tag}])
  when is_integer(Size), is_atom(Tag) ->
    {{is_tagged_tuple,Size,Tag},Arg};
normalize_op(is_nonempty_list, [Arg]) ->
    {is_nonempty_list,Arg};
normalize_op({bif,Bif}, [Arg]) ->
    case erl_internal:new_type_test(Bif, 1) of
        true -> {Bif,Arg};
        false -> none
    end;
normalize_op({bif,Bif}, [_,_]=Args) ->
    case erl_internal:comp_op(Bif, 2) of
        true ->
            normalize_op_1(Bif, Args);
        false ->
            none
    end;
normalize_op(_, _) -> none.

normalize_op_1(Bif, Args) ->
    case Args of
        [#b_literal{}=Arg1,#b_var{}=Arg2] ->
            {turn_op(Bif),Arg2,Arg1};
        [#b_var{}=Arg1,#b_literal{}=Arg2] ->
            {Bif,Arg1,Arg2};
        [#b_var{}=A,#b_var{}=B] ->
            if A < B -> {Bif,A,B};
               true -> {turn_op(Bif),B,A}
            end;
        [#b_literal{},#b_literal{}] ->
            none
    end.

-spec invert_op(basic_rel_op() | 'none') -> rel_op() | 'none'.

invert_op({Op,Arg1,Arg2}) ->
    {invert_op_1(Op),Arg1,Arg2};
invert_op({TypeTest,Arg}) ->
    {{'not',TypeTest},Arg};
invert_op(none) -> none.

invert_op_1('>=') -> '<';
invert_op_1('<') -> '>=';
invert_op_1('=<') -> '>';
invert_op_1('>') -> '=<';
invert_op_1('=:=') -> '=/=';
invert_op_1('=/=') -> '=:=';
invert_op_1('==') -> '/=';
invert_op_1('/=') -> '=='.

turn_op('<') -> '>';
turn_op('=<') -> '>=';
turn_op('>') -> '<';
turn_op('>=') -> '=<';
turn_op('=:='=Op) -> Op;
turn_op('=/='=Op) -> Op;
turn_op('=='=Op) -> Op;
turn_op('/='=Op) -> Op.

eval_rel_op(_Bif, _Args, #st{rel_op=none}) ->
    none;
eval_rel_op(Bif, Args, #st{rel_op=Prev}) ->
    case normalize_op(Bif, Args) of
        none ->
            eval_boolean(Prev, Bif, Args);
        RelOp ->
            case will_succeed(Prev, RelOp) of
                yes -> #b_literal{val=true};
                no -> #b_literal{val=false};
                maybe -> none
            end
    end.

eval_boolean({{'not',is_boolean},Var}, {bif,'not'}, [Var]) ->
    failed;
eval_boolean({{'not',is_boolean},Var}, {bif,Op}, Args)
  when Op =:= 'and'; Op =:= 'or' ->
    case member(Var, Args) of
        true -> failed;
        false -> none
    end;
eval_boolean(_, _, _) ->
    none.

%% will_succeed(PrevCondition, Condition) -> yes | no | maybe
%%  PrevCondition is a condition known to be true. This function
%%  will tell whether Condition will succeed.

will_succeed({_,_,_}=Same, {_,_,_}=Same) ->
    %% Repeated test.
    yes;
will_succeed({Op1,Var,#b_literal{val=A}}, {Op2,Var,#b_literal{val=B}}) ->
    will_succeed_1(Op1, A, Op2, B);
will_succeed({Op1,Var,#b_var{}=A}, {Op2,Var,#b_var{}=B}) ->
    will_succeed_vars(Op1, A, Op2, B);
will_succeed({'=:=',Var,#b_literal{val=A}}, {TypeTest,Var}) ->
    eval_type_test(TypeTest, A);
will_succeed({_,_}=Same, {_,_}=Same) ->
    %% Repeated type test.
    yes;
will_succeed({Test1,Var}, {Test2,Var}) ->
    will_succeed_test(Test1, Test2);
will_succeed({{'not',is_boolean},Var}, {'=:=',Var,#b_literal{val=Lit}})
  when is_boolean(Lit) ->
    no;
will_succeed({_,_}, {_,_}) ->
    maybe;
will_succeed({_,_}, {_,_,_}) ->
    maybe;
will_succeed({_,_,_}, {_,_}) ->
    maybe;
will_succeed({_,_,_}, {_,_,_}) ->
    maybe.

will_succeed_test({'not',Test1}, Test2) ->
    case Test1 =:= Test2 of
        true -> no;
        false -> maybe
    end;
will_succeed_test(is_tuple, {is_tagged_tuple,_,_}) ->
    maybe;
will_succeed_test({is_tagged_tuple,_,_}, is_tuple) ->
    yes;
will_succeed_test(is_list, is_nonempty_list) ->
    maybe;
will_succeed_test(is_nonempty_list, is_list) ->
    yes;
will_succeed_test(_T1, _T2) ->
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
will_succeed_1('=:=', A, '=:=', B) when A =/= B ->
    no;
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

will_succeed_1('=/=', A, '=:=', B) when A =:= B -> no;

will_succeed_1('<', A, '=:=', B)  when B >= A -> no;
will_succeed_1('<', A, '=/=', B)  when B >= A -> yes;
will_succeed_1('<', A, '<',   B)  when B >= A -> yes;
will_succeed_1('<', A, '=<',  B)  when B >= A -> yes;
will_succeed_1('<', A, '>=',  B)  when B >= A -> no;
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
will_succeed_1('>', A, '=<',  B)  when B =< A -> no;
will_succeed_1('>', A, '>=',  B)  when B =< A -> yes;
will_succeed_1('>', A, '>',   B)  when B =< A -> yes;

will_succeed_1('==', A, '==', B) ->
    if
	A == B -> yes;
	true -> no
    end;
will_succeed_1('==', A, '/=', B) ->
    if
	A == B -> no;
	true -> yes
    end;
will_succeed_1('/=', A, '/=', B) when A == B -> yes;
will_succeed_1('/=', A, '==', B) when A == B -> no;

will_succeed_1(_, _, _, _) -> maybe.

will_succeed_vars('=/=', Val, '=:=', Val) -> no;
will_succeed_vars('=:=', Val, '=/=', Val) -> no;
will_succeed_vars('=:=', Val, '>=',  Val) -> yes;
will_succeed_vars('=:=', Val, '=<',  Val) -> yes;

will_succeed_vars('/=', Val1, '==', Val2) when Val1 == Val2 -> no;
will_succeed_vars('==', Val1, '/=', Val2) when Val1 == Val2 -> no;

will_succeed_vars(_, _, _, _) -> maybe.

eval_type_test(Test, Arg) ->
    case eval_type_test_1(Test, Arg) of
        true -> yes;
        false -> no
    end.

eval_type_test_1(is_nonempty_list, Arg) ->
    case Arg of
        [_|_] -> true;
        _ -> false
    end;
eval_type_test_1({is_tagged_tuple,Sz,Tag}, Arg) ->
    if
        tuple_size(Arg) =:= Sz, element(1, Arg) =:= Tag ->
            true;
        true ->
            false
    end;
eval_type_test_1(Test, Arg) ->
    erlang:Test(Arg).

%%%
%%% Combine bif:'=:=' and switch instructions
%%% to switch instructions.
%%%
%%% Consider this code:
%%%
%%%     0:
%%%       @ssa_bool = bif:'=:=' Var, literal 1
%%%       br @ssa_bool, label 2, label 3
%%%
%%%     2:
%%%       ret literal a
%%%
%%%     3:
%%%       @ssa_bool:7 = bif:'=:=' Var, literal 2
%%%       br @ssa_bool:7, label 4, label 999
%%%
%%%     4:
%%%       ret literal b
%%%
%%%     999:
%%%       .
%%%       .
%%%       .
%%%
%%% The two bif:'=:=' instructions can be combined
%%% to a switch:
%%%
%%%     0:
%%%       switch Var, label 999, [ { literal 1, label 2 },
%%%                                { literal 2, label 3 } ]
%%%
%%%     2:
%%%       ret literal a
%%%
%%%     4:
%%%       ret literal b
%%%
%%%     999:
%%%       .
%%%       .
%%%       .
%%%

combine_eqs(#st{bs=Blocks}=St) ->
    Ls = reverse(beam_ssa:rpo(Blocks)),
    combine_eqs_1(Ls, St).

combine_eqs_1([L|Ls], #st{bs=Blocks0}=St0) ->
    case comb_get_sw(L, St0) of
        none ->
            combine_eqs_1(Ls, St0);
        {_,Arg,_,Fail0,List0} ->
            case comb_get_sw(Fail0, St0) of
                {true,Arg,Fail1,Fail,List1} ->
                    %% Another switch/br with the same arguments was
                    %% found. Try combining them.
                    case combine_lists(Fail1, List0, List1, Blocks0) of
                        none ->
                            %% Different types of literals in the lists,
                            %% or the success cases in the first switch
                            %% could branch to the second switch
                            %% (increasing code size and repeating tests).
                            combine_eqs_1(Ls, St0);
                        List ->
                            %% Everything OK! Combine the lists.
                            Sw0 = #b_switch{arg=Arg,fail=Fail,list=List},
                            Sw = beam_ssa:normalize(Sw0),
                            Blk0 = map_get(L, Blocks0),
                            Blk = Blk0#b_blk{last=Sw},
                            Blocks = Blocks0#{L:=Blk},
                            St = St0#st{bs=Blocks},
                            combine_eqs_1(Ls, St)
                    end;
                {true,_OtherArg,_,_,_} ->
                    %% The other switch/br uses a different Arg.
                    combine_eqs_1(Ls, St0);
                {false,_,_,_,_} ->
                    %% Not safe: Bindings of variables that will be used
                    %% or execution of instructions with potential
                    %% side effects will be skipped.
                    combine_eqs_1(Ls, St0);
                none ->
                    %% No switch/br at this label.
                    combine_eqs_1(Ls, St0)
            end
    end;
combine_eqs_1([], St) -> St.

comb_get_sw(L, #st{bs=Blocks,skippable=Skippable}) ->
    #b_blk{is=Is,last=Last} = map_get(L, Blocks),
    Safe0 = is_map_key(L, Skippable),
    case Last of
        #b_ret{} ->
            none;
        #b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail} ->
            case comb_is(Is, Bool, Safe0) of
                {none,_} ->
                    none;
                {#b_set{op={bif,'=:='},args=[#b_var{}=Arg,#b_literal{}=Lit]},Safe} ->
                    {Safe,Arg,L,Fail,[{Lit,Succ}]};
                {#b_set{},_} ->
                    none
            end;
        #b_br{} ->
            none;
        #b_switch{arg=#b_var{}=Arg,fail=Fail,list=List} ->
            {none,Safe} = comb_is(Is, none, Safe0),
            {Safe,Arg,L,Fail,List}
    end.

comb_is([#b_set{dst=#b_var{}=Bool}=I], Bool, Safe) ->
    {I,Safe};
comb_is([#b_set{}=I|Is], Bool, Safe0) ->
    Safe = Safe0 andalso beam_ssa:no_side_effect(I),
    comb_is(Is, Bool, Safe);
comb_is([], _Bool, Safe) ->
    {none,Safe}.

%% combine_list(Fail, List1, List2, Blocks) -> List|none.
%%  Try to combine two switch lists, returning the combined
%%  list or 'none' if not possible.
%%
%%  The values in the two lists must be all of the same type.
%%
%%  The code reached from the labels in the first list must
%%  not reach the failure label (if they do, tests could
%%  be repeated).
%%

combine_lists(Fail, L1, L2, Blocks) ->
    Ls = beam_ssa:rpo([Lbl || {_,Lbl} <- L1], Blocks),
    case member(Fail, Ls) of
        true ->
            %% One or more of labels in the first list
            %% could reach the failure label. That
            %% means that the second switch/br instruction
            %% will be retained, increasing code size and
            %% potentially also execution time.
            none;
        false ->
            %% The combined switch will replace both original
            %% br/switch instructions, leading to a reduction in code
            %% size and potentially also in execution time.
            combine_lists_1(L1, L2)
    end.

combine_lists_1(List0, List1) ->
    case are_lists_compatible(List0, List1) of
        true ->
            First = maps:from_list(List0),
            List0 ++ [{Val,Lbl} || {Val,Lbl} <- List1,
                                   not is_map_key(Val, First)];
        false ->
            none
    end.

are_lists_compatible([{#b_literal{val=Val1},_}|_],
                     [{#b_literal{val=Val2},_}|_]) ->
    case lit_type(Val1) of
        none -> false;
        Type -> Type =:= lit_type(Val2)
    end.

lit_type(Val) ->
    if
        is_atom(Val) -> atom;
        is_float(Val) -> float;
        is_integer(Val) -> integer;
        true -> none
    end.

%%%
%%% Calculate used variables for each block.
%%%

used_vars(Linear) ->
    used_vars(reverse(Linear), #{}, #{}).

used_vars([{L,#b_blk{is=Is}=Blk}|Bs], UsedVars0, Skip0) ->
    %% Calculate the variables used by each block and its
    %% successors. This information is used by
    %% shortcut_opt/1.

    Successors = beam_ssa:successors(Blk),
    Used0 = used_vars_succ(Successors, L, UsedVars0, cerl_sets:new()),
    Used = used_vars_blk(Blk, Used0),
    UsedVars = used_vars_phis(Is, L, Used, UsedVars0),

    %% combine_eqs/1 needs different variable usage information than
    %% shortcut_opt/1. The Skip map will have an entry for each block
    %% that can be skipped (does not bind any variable used in
    %% successor). This information is also useful for speeding up
    %% shortcut_opt/1.

    Defined0 = [Def || #b_set{dst=Def} <- Is],
    Defined = cerl_sets:from_list(Defined0),
    MaySkip = cerl_sets:is_disjoint(Defined, Used0),
    case MaySkip of
        true ->
            Skip = Skip0#{L=>true},
            used_vars(Bs, UsedVars, Skip);
        false ->
            used_vars(Bs, UsedVars, Skip0)
    end;
used_vars([], UsedVars, Skip) ->
    {UsedVars,Skip}.

used_vars_succ([S|Ss], L, LiveMap, Live0) ->
    Key = {S,L},
    case LiveMap of
        #{Key:=Live} ->
            %% The successor has a phi node, and the value for
            %% this block in the phi node is a variable.
            used_vars_succ(Ss, L, LiveMap, cerl_sets:union(Live, Live0));
        #{S:=Live} ->
            %% No phi node in the successor, or the value for
            %% this block in the phi node is a literal.
            used_vars_succ(Ss, L, LiveMap, cerl_sets:union(Live, Live0));
        #{} ->
            %% A peek_message block which has not been processed yet.
            used_vars_succ(Ss, L, LiveMap, Live0)
    end;
used_vars_succ([], _, _, Acc) -> Acc.

used_vars_phis(Is, L, Live0, UsedVars0) ->
    UsedVars = UsedVars0#{L=>Live0},
    Phis = takewhile(fun(#b_set{op=Op}) -> Op =:= phi end, Is),
    case Phis of
        [] ->
            UsedVars;
        [_|_] ->
            PhiArgs = append([Args || #b_set{args=Args} <- Phis]),
            case [{P,V} || {#b_var{}=V,P} <- PhiArgs] of
                [_|_]=PhiVars ->
                    PhiLive0 = rel2fam(PhiVars),
                    PhiLive = [{{L,P},cerl_sets:union(cerl_sets:from_list(Vs), Live0)} ||
                                  {P,Vs} <- PhiLive0],
                    maps:merge(UsedVars, maps:from_list(PhiLive));
                [] ->
                    %% There were only literals in the phi node(s).
                    UsedVars
            end
    end.

used_vars_blk(#b_blk{is=Is,last=Last}, Used0) ->
    Used = cerl_sets:union(Used0, cerl_sets:from_list(beam_ssa:used(Last))),
    used_vars_is(reverse(Is), Used).

used_vars_is([#b_set{op=phi}|Is], Used) ->
    used_vars_is(Is, Used);
used_vars_is([#b_set{dst=Dst}=I|Is], Used0) ->
    Used1 = cerl_sets:union(Used0, cerl_sets:from_list(beam_ssa:used(I))),
    Used = cerl_sets:del_element(Dst, Used1),
    used_vars_is(Is, Used);
used_vars_is([], Used) ->
    Used.

%%%
%%% Common utilities.
%%%

sub(#b_set{args=Args}=I, Sub) when map_size(Sub) =/= 0 ->
    I#b_set{args=[sub_arg(A, Sub) || A <- Args]};
sub(I, _Sub) -> I.

sub_arg(#b_var{}=Old, Sub) ->
    case Sub of
        #{Old:=New} -> New;
        #{} -> Old
    end;
sub_arg(Old, _Sub) -> Old.

rel2fam(S0) ->
    S1 = sofs:relation(S0),
    S = sofs:rel2fam(S1),
    sofs:to_external(S).
