%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2018-2025. All Rights Reserved.
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
-moduledoc false.
-export([opt/1]).

-include("beam_ssa.hrl").
-import(lists, [append/1,foldl/3,keymember/3,last/1,member/2,
                reverse/1,reverse/2,takewhile/2]).

-type used_vars() :: #{beam_ssa:label():=sets:set(beam_ssa:b_var())}.

-type basic_type_test() :: atom() | {'is_tagged_tuple',pos_integer(),atom()}.
-type type_test() :: basic_type_test() | {'not',basic_type_test()}.
-type op_name() :: atom().
-type basic_test() :: {op_name(),beam_ssa:b_var(),beam_ssa:value()} |
                      {basic_type_test(),beam_ssa:value()}.
-type test() :: {op_name(),beam_ssa:b_var(),beam_ssa:value()} |
                {type_test(),beam_ssa:value()}.

-record(st,
        {bs :: beam_ssa:block_map(),
         us :: used_vars(),
         skippable :: #{beam_ssa:label():='true'},
         test=none :: 'none' | test(),
         target=any :: 'any' | 'one_way' | beam_ssa:label()
        }).

-spec opt([{Label0,Block0}]) -> [{Label,Block}] when
      Label0 :: beam_ssa:label(),
      Block0 :: beam_ssa:b_blk(),
      Label :: beam_ssa:label(),
      Block :: beam_ssa:b_blk().

opt(Linear0) ->
    {Used,Skippable} = used_vars(Linear0),
    Blocks0 = maps:from_list(Linear0),
    St0 = #st{bs=Blocks0,us=Used,skippable=Skippable},
    St = shortcut_opt(St0),
    #st{bs=Blocks} = combine_eqs(St#st{us=#{}}),
    opt_redundant_tests(Blocks).

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
    St = St0#st{test=none},
    shortcut(Succ0, From, #{}, St);
shortcut_terminator(#b_br{bool=#b_var{}=Bool,succ=Succ0,fail=Fail0}=Br,
                    Is, From, St0) ->
    St = St0#st{target=one_way},
    Test = get_test(Bool, Is),

    %% The boolean in a `br` is seldom used by the successors. By
    %% not binding its value unless it is actually used we might be able
    %% to skip some work in shortcut/4 and sub/2.
    SuccBs = bind_var_if_used(Succ0, Bool, #b_literal{val=true}, St),
    BrSucc = shortcut(Succ0, From, SuccBs, St#st{test=Test}),
    FailBs = bind_var_if_used(Fail0, Bool, #b_literal{val=false}, St),
    BrFail = shortcut(Fail0, From, FailBs, St#st{test=invert_test(Test)}),

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

    %% There no need to call beam_ssa:normalize/1 (and invoke the
    %% cost of sorting List), because the previous optimizations
    %% could only have changed labels.
    Sw#b_switch{fail=Fail,list=List};
shortcut_terminator(Last, _Is, _From, _St) ->
    Last.

shortcut_sw_fail(Fail0, List, Bool, From, St0) ->
    %% List has been sorted by beam_ssa:normalize/1.
    case List of
        [{#b_literal{val=false},_},
         {#b_literal{val=true},_}] ->
            Test = {{'not',is_boolean},Bool},
            St = St0#st{test=Test,target=one_way},
            #b_br{bool=#b_literal{val=true},succ=Fail} =
                shortcut(Fail0, From, #{}, St),
            Fail;
        _ ->
            Fail0
    end.

shortcut_sw_list([{Lit,L0}|T], Bool, From, St0) ->
    Test = {'=:=',Bool,Lit},
    St = St0#st{test=Test},
    #b_br{bool=#b_literal{val=true},succ=L} =
        shortcut(L0, From, bind_var(Bool, Lit, #{}), St#st{target=one_way}),
    [{Lit,L}|shortcut_sw_list(T, Bool, From, St0)];
shortcut_sw_list([], _, _, _) -> [].

shortcut(L, _From, Bs, #st{test=none,target=one_way}) when map_size(Bs) =:= 0 ->
    %% There is no way that we can find a suitable branch, because
    %% there are no stored tests, there are no bindings, and the block
    %% L can't have any phi nodes from which we could pick bindings
    %% because when the target is `one_way`, it implies that the From
    %% block has a two-way `br` terminator.
    #b_br{bool=#b_literal{val=true},succ=L,fail=L};
shortcut(L, From, Bs, St) ->
    shortcut_1(L, From, Bs, sets:new(), St).

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
    case sets:size(UnsetVars) of
        SetSize when SetSize > 64 ->
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
                        {safe, UnsetVars} ->
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
                            {safe, UnsetVars}
                    end;
                #b_br{} ->
                    {safe, UnsetVars}
            end;
        false ->
            %% Some variables defined in this block are used by
            %% successors. We must update the set of unset variables.
            SetInThisBlock = [V || #b_set{dst=V} <:- Is],
            {safe, list_set_union(SetInThisBlock, UnsetVars)}
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
            not sets:is_element(V, UnsetVars) andalso
                sets:is_disjoint(Used0, UnsetVars) andalso
                sets:is_disjoint(Used1, UnsetVars);
        #b_br{succ=Same,fail=Same} ->
            %% An unconditional branch must not jump to
            %% a phi node.
            not is_forbidden(Same, St) andalso
                sets:is_disjoint(map_get(Same, Us), UnsetVars)
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
eval_is([#b_set{op={succeeded,guard},dst=Dst,args=[Var]}], _From, Bs, _St) ->
    case Bs of
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
        none ->
            eval_is(Is, From, Bs, St)
    end;
eval_is([#b_set{op=Op,dst=Dst}=I|Is], From, Bs, St)
  when Op =:= is_tagged_tuple; Op =:= is_nonempty_list ->
    #b_set{args=Args} = sub(I, Bs),
    case eval_test(Op, Args, St) of
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

eval_switch(List, Arg, #st{test={_,Arg,_}=PrevOp}, Fail) ->
    %% There is a previous relational operator testing the same variable.
    %% Optimization may be possible.
    eval_switch_1(List, Arg, PrevOp, Fail);
eval_switch(_, _, _, _) ->
    %% There is either no previous relational operator, or it tests
    %% a different variable. Nothing to optimize.
    none.

eval_switch_1([{Lit,Lbl}|T], Arg, PrevOp, Fail) ->
    Test = {'=:=',Arg,Lit},
    case will_succeed(PrevOp, Test) of
        yes ->
            %% Success. This branch will always be taken.
            Lbl;
        no ->
            %% This branch will never be taken.
            eval_switch_1(T, Arg, PrevOp, Fail);
        'maybe' ->
            %% This label could be reached.
            eval_switch_1(T, Arg, PrevOp, none)
    end;
eval_switch_1([], _Arg, _PrevOp, Fail) ->
    %% Fail is now either the failure label or 'none'.
    Fail.

bind_var_if_used(L, Var, Val, #st{us=Us}) ->
    case sets:is_element(Var, map_get(L, Us)) of
        true -> #{Var=>Val};
        false -> #{}
    end.

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
                    eval_test({bif,Bif}, Args, St);
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

get_test(Bool, [_|_]=Is) ->
    case last(Is) of
        #b_set{op=Op,dst=Bool,args=Args} ->
            normalize_test(Op, Args);
        #b_set{} ->
            none
    end;
get_test(_, []) -> none.

%% normalize_test(Instruction) -> {Normalized,FailLabel} | error
%%    Normalized = {Operator,Variable,Variable|Literal} |
%%                 {TypeTest,Variable}
%%    Operation = '<' | '=<' | '=:=' | '=/=' | '>=' | '>' | '==' | '/='
%%    TypeTest = is_atom | is_integer ...
%%    Variable = #b_var{}
%%    Literal = #b_literal{}
%%
%%  Normalize type tests and relational operators to facilitate
%%  further comparisons between test. Always make the register
%%  operand the first operand. If there are two registers, order the
%%  registers in lexical order.
%%
%%  For example, this instruction:
%%
%%    #b_set{op={bif,'<'},args=[#b_literal{}, #b_var{}}
%%
%%  will be normalized to:
%%
%%    {'>',#b_var{},#b_literal{}}

-spec normalize_test(Op, Args) -> NormalizedTest | 'none' when
      Op :: beam_ssa:op(),
      Args :: [beam_ssa:value()],
      NormalizedTest :: basic_test().

normalize_test(is_tagged_tuple, [Arg,#b_literal{val=Size},#b_literal{val=Tag}])
  when is_integer(Size), is_atom(Tag) ->
    {{is_tagged_tuple,Size,Tag},Arg};
normalize_test(is_nonempty_list, [Arg]) ->
    {is_nonempty_list,Arg};
normalize_test({bif,Bif}, [Arg]) ->
    case erl_internal:new_type_test(Bif, 1) of
        true -> {Bif,Arg};
        false -> none
    end;
normalize_test({bif,Bif}, [_,_]=Args) ->
    case erl_internal:comp_op(Bif, 2) of
        true ->
            normalize_test_1(Bif, Args);
        false ->
            none
    end;
normalize_test(_, _) -> none.

normalize_test_1(Bif, Args) ->
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

-spec invert_test(basic_test() | 'none') -> test() | 'none'.

invert_test({Op,Arg1,Arg2}) ->
    {invert_op(Op),Arg1,Arg2};
invert_test({TypeTest,Arg}) ->
    {{'not',TypeTest},Arg};
invert_test(none) -> none.

invert_op('>=') -> '<';
invert_op('<') -> '>=';
invert_op('=<') -> '>';
invert_op('>') -> '=<';
invert_op('=:=') -> '=/=';
invert_op('=/=') -> '=:=';
invert_op('==') -> '/=';
invert_op('/=') -> '=='.

turn_op('<') -> '>';
turn_op('=<') -> '>=';
turn_op('>') -> '<';
turn_op('>=') -> '=<';
turn_op('=:='=Op) -> Op;
turn_op('=/='=Op) -> Op;
turn_op('=='=Op) -> Op;
turn_op('/='=Op) -> Op.

eval_test(_Bif, _Args, #st{test=none}) ->
    none;
eval_test(Bif, Args, #st{test=Prev}) ->
    case normalize_test(Bif, Args) of
        none ->
            none;
        Test ->
            case will_succeed(Prev, Test) of
                yes -> #b_literal{val=true};
                no -> #b_literal{val=false};
                'maybe' -> none
            end
    end.

%% will_succeed(PrevCondition, Condition) -> yes | no | 'maybe'
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
    'maybe';
will_succeed({_,_}, {_,_,_}) ->
    'maybe';
will_succeed({_,_,_}, {_,_}) ->
    'maybe';
will_succeed({_,_,_}, {_,_,_}) ->
    'maybe'.

will_succeed_test({'not',Test1}, Test2) ->
    case Test1 =:= Test2 of
        true -> no;
        false -> 'maybe'
    end;
will_succeed_test(is_tuple, {is_tagged_tuple,_,_}) ->
    'maybe';
will_succeed_test({is_tagged_tuple,_,_}, is_tuple) ->
    yes;
will_succeed_test(is_list, is_nonempty_list) ->
    'maybe';
will_succeed_test(is_nonempty_list, is_list) ->
    yes;
will_succeed_test(_T1, _T2) ->
    'maybe'.

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
will_succeed_1('<', A, '<',   B)  when B >= A -> yes;
will_succeed_1('<', A, '=<',  B)  when B >= A -> yes;
will_succeed_1('<', A, '>=',  B)  when B >= A -> no;
will_succeed_1('<', A, '>',   B)  when B >= A -> no;

will_succeed_1('=<', A, '=:=', B) when B > A  -> no;
will_succeed_1('=<', A, '<',   B) when B > A  -> yes;
will_succeed_1('=<', A, '=<',  B) when B >= A -> yes;
will_succeed_1('=<', A, '>=',  B) when B > A  -> no;
will_succeed_1('=<', A, '>',   B) when B >= A -> no;

will_succeed_1('>=', A, '=:=', B) when B < A  -> no;
will_succeed_1('>=', A, '<',   B) when B =< A -> no;
will_succeed_1('>=', A, '=<',  B) when B < A  -> no;
will_succeed_1('>=', A, '>=',  B) when B =< A -> yes;
will_succeed_1('>=', A, '>',   B) when B < A  -> yes;

will_succeed_1('>', A, '=:=', B)  when B =< A -> no;
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

will_succeed_1(_, _, _, _) -> 'maybe'.

will_succeed_vars('=/=', Var, '=:=', Var) -> no;
will_succeed_vars('=:=', Var, '=/=', Var) -> no;
will_succeed_vars('=:=', Var, '>=',  Var) -> yes;
will_succeed_vars('=:=', Var, '=<',  Var) -> yes;

will_succeed_vars('/=', Var, '==', Var) -> no;
will_succeed_vars('==', Var, '/=', Var) -> no;

will_succeed_vars(_, _, _, _) -> 'maybe'.

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
%%% Combine bif:'=:=', is_boolean/1 tests, and switch instructions
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
            %% Look for a switch instruction at the fail label
            case comb_get_sw(Fail0, St0) of
                {true,Arg,Fail1,Fail,List1} ->
                    %% Another switch/br with the same arguments was
                    %% found at the fail label. Try combining them.
                    case combine_lists(Fail1, List0, List1, Blocks0) of
                        none ->
                            %% Different types of literals in the lists,
                            %% or the success cases in the first switch
                            %% could branch to the second switch
                            %% (increasing code size and repeating tests).
                            combine_eqs_1(Ls, St0);
                        List ->
                            %% The lists were successfully combined.
                            St = combine_build_sw(L, Arg, Fail, List, St0),
                            combine_eqs_1(Ls, St)
                    end;
                _ ->
                    %% There was no switch of the correct kind found at the
                    %% fail label. Look for a switch at the first success label.
                    [{_,Succ}|_] = List0,
                    case comb_get_sw(Succ, St0) of
                        {true,Arg,_,_,_} ->
                            %% Since we found a switch at the success
                            %% label, the switch for this block (L)
                            %% must have been constructed out of a
                            %% is_boolean test or a two-way branch
                            %% instruction (if the switch at L had
                            %% been present when the shortcut_opt/1
                            %% pass was run, its success branches
                            %% would have been cut short and no longer
                            %% point at the switch at the fail label).
                            %%
                            %% Therefore, keep this constructed
                            %% switch. It will be further optimized
                            %% the next time shortcut_opt/1 is run.
                            St = combine_build_sw(L, Arg, Fail0, List0, St0),
                            combine_eqs_1(Ls, St);
                        _ ->
                            combine_eqs_1(Ls, St0)
                    end
            end
    end;
combine_eqs_1([], St) -> St.

combine_build_sw(From, Arg, Fail, List, #st{bs=Blocks0}=St) ->
    Sw0 = #b_switch{arg=Arg,fail=Fail,list=List},
    Sw = beam_ssa:normalize(Sw0),
    Blk0 = map_get(From, Blocks0),
    Blk = Blk0#b_blk{last=Sw},
    Blocks = Blocks0#{From := Blk},
    St#st{bs=Blocks}.

comb_get_sw(L, #st{bs=Blocks,skippable=Skippable}) ->
    #b_blk{is=Is,last=Last} = map_get(L, Blocks),
    Safe0 = is_map_key(L, Skippable),
    case Last of
        #b_ret{} ->
            none;
        #b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail} ->
            case comb_is(Is, Bool, Safe0) of
                {none,Safe} ->
                    {Safe,Bool,L,Fail,[{#b_literal{val=true},Succ}]};
                {#b_set{op={bif,'=:='},args=[#b_var{}=Arg,#b_literal{}=Lit]},Safe} ->
                    {Safe,Arg,L,Fail,[{Lit,Succ}]};
                {#b_set{op={bif,is_boolean},args=[#b_var{}=Arg]},Safe} ->
                    SwList = [{#b_literal{val=false},Succ},
                              {#b_literal{val=true},Succ}],
                    {Safe,Arg,L,Fail,SwList};
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
    Ls = beam_ssa:rpo([Lbl || {_,Lbl} <:- L1], Blocks),
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
            List0 ++ [{Val,Lbl} || {Val,Lbl} <:- List1,
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
%%% Remove redundant tests.
%%%
%%% Repeated tests can be introduced by inlining, macros, or
%%% complex guards such as:
%%%
%%%     is_head(M, S) when M =:= <<1>>, S =:= <<2>> ->
%%%         true;
%%%     is_head(M, S) when M =:= <<1>>, S =:= <<3>> ->
%%%         false.
%%%
%%% The repeated test is not removed by any of the other optimizing
%%% passes:
%%%
%%%     0:
%%%       _2 = bif:'=:=' _0, `<<1>>`
%%%       br _2, ^19, ^3
%%%
%%%     19:
%%%       _3 = bif:'=:=' _1, `<<2>>`
%%%       br _3, ^7, ^4
%%%
%%%     7:
%%%       ret `true`
%%%
%%%     4:
%%%       _4 = bif:'=:=' _0, `<<1>>`
%%%       br _4, ^15, ^3
%%%
%%%     15:
%%%       _5 = bif:'=:=' _1, `<<3>>`
%%%       br _5, ^11, ^3
%%%
%%%     11:
%%%       ret `false`
%%%
%%%     3:
%%%       %% Generate function clause error.
%%%       . . .
%%%
%%% This sub pass will keep track of all tests that are known to have
%%% been executed at each block in the SSA code. If a repeated or
%%% inverted test is seen, it can be eliminated. For the example
%%% above, this sub pass will rewrite block 4 like this:
%%%
%%%     4:
%%%       _4 = bif:'=:=' `true`, `true`
%%%       br ^15
%%%
%%% This sub pass also removes redundant inverted test such as the
%%% last test in this code:
%%%
%%%     if
%%%         A < B -> . . . ;
%%%         A >= B -> . . .
%%%     end
%%%
%%% and this code:
%%%
%%%     if
%%%         A < B -> . . . ;
%%%         A > B -> . . . ;
%%%         A == B -> . . .
%%%     end
%%%

opt_redundant_tests(Blocks) ->
    All = #{0 => #{}, ?EXCEPTION_BLOCK => #{}},
    RPO = beam_ssa:rpo(Blocks),
    Linear = opt_redundant_tests(RPO, Blocks, All),
    beam_ssa:trim_unreachable(Linear).

opt_redundant_tests([L|Ls], Blocks, All0) ->
    case All0 of
        #{L := Tests} ->
            Blk0 = map_get(L, Blocks),
            Tests = map_get(L, All0),
            Blk1 = opt_switch(Blk0, Tests),
            #b_blk{is=Is0} = Blk1,
            case opt_redundant_tests_is(Is0, Tests, []) of
                none ->
                    All = update_successors(Blk1, Tests, All0),
                    [{L,Blk1}|opt_redundant_tests(Ls, Blocks, All)];
                {new_test,Bool,Test,MustInvert} ->
                    All = update_successors(Blk1, Bool, Test, MustInvert,
                                            Tests, All0),
                    [{L,Blk1}|opt_redundant_tests(Ls, Blocks, All)];
                {old_test,Is,BoolVar,BoolValue} ->
                    Blk = case Blk1 of
                              #b_blk{last=#b_br{bool=BoolVar}=Br0} ->
                                  Br = beam_ssa:normalize(Br0#b_br{bool=BoolValue}),
                                  Blk1#b_blk{is=Is,last=Br};
                              #b_blk{}=Blk2 ->
                                  Blk2#b_blk{is=Is}
                          end,
                    All = update_successors(Blk, Tests, All0),
                    [{L,Blk}|opt_redundant_tests(Ls, Blocks, All)]
            end;
        #{} ->
            opt_redundant_tests(Ls, Blocks, All0)
    end;
opt_redundant_tests([], _Blocks, _All) -> [].

opt_switch(#b_blk{last=#b_switch{arg=Arg,list=List0}=Sw}=Blk, Tests)
  when map_size(Tests) =/= 0 ->
    List = opt_switch_1(List0, Arg, Tests),
    Blk#b_blk{last=Sw#b_switch{list=List}};
opt_switch(Blk, _Tests) -> Blk.

opt_switch_1([{Lit,_}=H|T], Arg, Tests) ->
    case Tests of
        #{{'=:=',Arg,Lit} := false} ->
            opt_switch_1(T, Arg, Tests);
        #{} ->
            [H|opt_switch_1(T, Arg, Tests)]
    end;
opt_switch_1([], _, _) -> [].

opt_redundant_tests_is([#b_set{op=Op,args=Args,dst=Bool}=I0], Tests, Acc) ->
    case canonical_test(Op, Args) of
        none ->
            none;
        {Test,MustInvert} ->
            case old_result(Test, Tests) of
                Result0 when is_boolean(Result0) ->
                    case gains_type_information(I0) of
                        false ->
                            Result = #b_literal{val=Result0 xor MustInvert},
                            I = I0#b_set{op={bif,'=:='},args=[Result,#b_literal{val=true}]},
                            {old_test,reverse(Acc, [I]),Bool,Result};
                        true ->
                            %% At least one variable will gain type
                            %% information from this `=:=`
                            %% operation. Removing it could make it
                            %% impossible for beam_validator to
                            %% realize that the code is type-safe.
                            none
                    end;
                none ->
                    {new_test,Bool,Test,MustInvert}
            end
    end;
opt_redundant_tests_is([I|Is], Tests, Acc) ->
    opt_redundant_tests_is(Is, Tests, [I|Acc]);
opt_redundant_tests_is([], _Tests, _Acc) -> none.

%% Will any of the variables gain type information from this
%% operation?
gains_type_information(#b_set{anno=Anno,op={bif,'=:='},args=Args}) ->
    Types0 = maps:get(arg_types, Anno, #{}),
    Types = complete_type_information(Args, 0, Types0),
    case map_size(Types) of
        0 ->
            false;
        1 ->
            true;
        2 ->
            case Types of
                #{0 := Same,1 := Same} ->
                    false;
                #{} ->
                    true
            end
    end;
gains_type_information(#b_set{}) -> false.

complete_type_information([#b_literal{val=Value}|As], N, Types) ->
    Type = beam_types:make_type_from_value(Value),
    complete_type_information(As, N+1, Types#{N => Type});
complete_type_information([#b_var{}|As], N, Types) ->
    complete_type_information(As, N+1, Types);
complete_type_information([], _, Types) -> Types.

old_result(Test, Tests) ->
    case Tests of
        #{Test := Val} -> Val;
        #{} -> old_result_1(Test, Tests)
    end.

%%
%% Remove the last test in a sequence of tests (in any order):
%%
%%   if
%%     Val1 < Val2   -> . . .
%%     Val1 > Val2   -> . . .
%%     Val1 == Val2 -> . . .
%%   end
%%
%% NOTE: The same optimization is not possible to do with `=:=`, unless
%% we have type information so that we know that `==` and `=:=` produces
%% the same result.
%%

old_result_1({'==',A,B}, Tests) ->
    case Tests of
        #{{'<',A,B} := false, {'=<',A,B} := true} ->
            %% not A < B, not A > B  ==>  A == B
            true;
        #{} ->
            none
    end;
old_result_1({'=<',A,B}, Tests) ->
    case Tests of
        #{{'<',A,B} := false, {'==',A,B} := false} ->
            %% not A < B, not A == B    ==>  A > B
            false;
        #{} ->
            none
    end;
old_result_1({'<',A,B}, Tests) ->
    case Tests of
        #{{'=<',A,B} := true, {'==',A,B} := false} ->
            %% not A < B, not A == B   ==>  A < B
            true;
        #{} ->
            none
    end;
old_result_1({is_nonempty_list,A}, Tests) ->
    case Tests of
        #{{is_list,A} := false} -> false;
        #{} -> none
    end;
old_result_1(_, _) -> none.

%% canonical_test(Op0, Args0) -> {CanonicalTest, MustInvert}
%%    CanonicalTest = {Operator,Variable,Variable|Literal} |
%%                  {TypeTest,Variable}
%%    Operation = '<' | '=<' | '=:=' | '=='
%%    TypeTest = is_atom | is_integer ...
%%    Variable = #b_var{}
%%    Literal = #b_literal{}
%%    MustInvert = true | false
%%
%%  Canonicalize a test. Always make the register
%%  operand the first operand. If there are two registers,
%%  order the registers in lexical order. Invert four of
%%  the relation operators and indicate with MustInvert
%%  whether the operator was inverted.
%%
%%  For example, this instruction:
%%
%%    #b_set{op={bif,'=:='},args=[#b_literal{}, #b_var{}}
%%
%%  will be canonicalized to:
%%
%%    {{'=:=',#b_var{},#b_literal{}}, false}
%%
%%  while:
%%
%%    #b_set{op={bif,'>'},args=[#b_var{}, #b_literal{}}}
%%
%%  will be canonicalized to:
%%
%%    {{'=<',#b_var{},#b_literal{}}, true}
%%
canonical_test(Op, Args) ->
    case normalize_test(Op, Args) of
        none ->
            none;
        Test ->
            Inv = case Test of
                      {'=/=',_,_} -> true;
                      {'/=',_,_} -> true;
                      {'>',_,_} -> true;
                      {'>=',_,_} -> true;
                      _ -> false
                  end,
            case Inv of
                true -> {invert_test(Test),true};
                false -> {Test,false}
            end
    end.

update_successors(#b_blk{last=#b_br{bool=Bool,succ=Succ,fail=Fail}},
                  Bool, Test, MustInvert, Tests, All0) ->
    All1 = update_successor(Succ, Tests#{Test => not MustInvert}, All0),
    update_successor(Fail, Tests#{Test => MustInvert}, All1);
update_successors(Blk, _, _, _, TestsA, All) ->
    update_successors(Blk, TestsA, All).

update_successors(#b_blk{last=#b_ret{}}, _Tests, All) ->
    All;
update_successors(#b_blk{last=#b_switch{arg=Arg,fail=Fail,list=List}},
                  Tests, All0) ->
    All1 = update_successors_sw_fail(List, Arg, Fail, Tests, All0),
    update_successors_sw(List, Arg, Tests, All1);
update_successors(Blk, Tests, All) ->
    foldl(fun(L, A) ->
                  update_successor(L, Tests, A)
          end, All, beam_ssa:successors(Blk)).

update_successors_sw_fail(List, Arg, Fail, Tests0, All) ->
    Tests = foldl(fun({Lit,_}, A) ->
                          A#{{'=:=',Arg,Lit} => false}
                  end, Tests0, List),
    update_successor(Fail, Tests, All).

update_successors_sw([{Lit,L}|T], Arg, Tests, All0) ->
    All = update_successor(L, Tests#{{'=:=',Arg,Lit} => true}, All0),
    update_successors_sw(T, Arg, Tests, All);
update_successors_sw([], _, _, All) -> All.

update_successor(?EXCEPTION_BLOCK, _Tests, All) ->
    All;
update_successor(L, TestsA, All0) ->
    case All0 of
        #{L := TestsB} ->
            All0#{L := maps_intersect_kv(TestsA, TestsB)};
        #{} ->
            All0#{L => TestsA}
    end.

maps_intersect_kv(Map, Map) ->
    Map;
maps_intersect_kv(Map1, Map2) ->
    if
        map_size(Map1) < map_size(Map2) ->
            map_intersect_kv_1(Map1, Map2);
        true ->
            map_intersect_kv_1(Map2, Map1)
    end.

map_intersect_kv_1(SmallMap, BigMap) ->
    Next = maps:next(maps:iterator(SmallMap)),
    case maps_is_subset_kv(Next, BigMap) of
        true -> SmallMap;
        false -> map_intersect_kv_2(Next, BigMap, [])
    end.

map_intersect_kv_2({K, V, Iterator}, BigMap, Acc) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{K := V} ->
            map_intersect_kv_2(Next, BigMap, [{K,V}|Acc]);
        #{} ->
            map_intersect_kv_2(Next, BigMap, Acc)
    end;
map_intersect_kv_2(none, _BigMap, Acc) ->
    maps:from_list(Acc).

maps_is_subset_kv({K, V, Iterator}, BigMap) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{K := V} ->
            maps_is_subset_kv(Next, BigMap);
        #{} ->
            false
    end;
maps_is_subset_kv(none, _BigMap) -> true.

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
    Used0 = used_vars_succ(Successors, L, UsedVars0, sets:new()),
    Used = used_vars_blk(Blk, Used0),
    UsedVars = used_vars_phis(Is, L, Used, UsedVars0),

    %% combine_eqs/1 needs different variable usage information than
    %% shortcut_opt/1. The Skip map will have an entry for each block
    %% that can be skipped (does not bind any variable used in
    %% successor). This information is also useful for speeding up
    %% shortcut_opt/1.

    Defined0 = [Def || #b_set{dst=Def} <:- Is],
    Defined = sets:from_list(Defined0),
    MaySkip = sets:is_disjoint(Defined, Used0),
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
            used_vars_succ(Ss, L, LiveMap, sets:union(Live, Live0));
        #{S:=Live} ->
            %% No phi node in the successor, or the value for
            %% this block in the phi node is a literal.
            used_vars_succ(Ss, L, LiveMap, sets:union(Live, Live0));
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
            PhiArgs = append([Args || #b_set{args=Args} <:- Phis]),
            case [{P,V} || {#b_var{}=V,P} <- PhiArgs] of
                [_|_]=PhiVars ->
                    PhiLive0 = rel2fam(PhiVars),
                    PhiLive = #{{L,P} => list_set_union(Vs, Live0) ||
                                  {P,Vs} <:- PhiLive0},
                    maps:merge(UsedVars, PhiLive);
                [] ->
                    %% There were only literals in the phi node(s).
                    UsedVars
            end
    end.

used_vars_blk(#b_blk{is=Is,last=Last}, Used0) ->
    Used = list_set_union(beam_ssa:used(Last), Used0),
    used_vars_is(reverse(Is), Used).

used_vars_is([#b_set{op=phi}|Is], Used) ->
    used_vars_is(Is, Used);
used_vars_is([#b_set{dst=Dst}=I|Is], Used0) ->
    Used1 = list_set_union(beam_ssa:used(I), Used0),
    Used = sets:del_element(Dst, Used1),
    used_vars_is(Is, Used);
used_vars_is([], Used) ->
    Used.

%%%
%%% Common utilities.
%%%

list_set_union([], Set) ->
    Set;
list_set_union([E], Set) ->
    sets:add_element(E, Set);
list_set_union(List, Set) ->
    sets:union(sets:from_list(List), Set).

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
