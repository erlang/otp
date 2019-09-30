%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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
%% The purpose of this pass is to optimize boolean expressions in
%% guards. Instead of evaluating a boolean expression and finally
%% comparing it to 'true', evaluate the expression using control flow.
%%
%% This pass is run directly after conversion to SSA code because some
%% optimizations in beam_ssa_opt (especially sinking of
%% get_tuple_element instructions) would prevent these optimizations
%% or at least make them much more difficult to perform.
%%
%% As an example, take the guard:
%%
%%     when is_integer(V0), is_atom(V1) ->
%%
%% The unoptimized SSA code translated to pseudo BEAM code would look
%% like:
%%
%%     bif is_integer V0 => Bool0
%%     bif is_atom V1    => Bool1
%%     bif and Bool0 Bool1 => Bool
%%     test Bool =:= true else goto Fail
%%     ...
%%   Fail:
%%     ...
%%
%%  The optimized code would look like:
%%
%%     test is_integer V0 else goto Fail
%%     test is_atom V1    else goto Fail
%%     ...
%%   Fail:
%%     ...
%%
%%  An 'or' operation is only slightly more complicated:
%%
%%     test is_integer V0 else goto NotFailedYet
%%     goto Success
%%
%%   NotFailedYet:
%%     test is_atom V1 else goto Fail
%%
%%   Success:
%%     ...
%%   Fail:
%%     ...
%%
%% The unoptimized SSA code for the first example looks like:
%%
%% 0:
%%   _2 = bif:is_integer _0
%%   _3 = bif:is_atom _1
%%   _7 = bif:'and' _2, _3
%%   @ssa_bool = succeeded _7
%%   br @ssa_bool, label 4, label 3
%%
%% 4:
%%   @ssa_bool:5 = bif:'=:=' _7, literal true
%%   br @ssa_bool:5, label 6, label 3
%%
%% 6:
%%   ret literal ok
%%
%% 3: Error.
%%   ...
%%
%% The optimized SSA code looks like:
%%
%% 0:
%%   _2 = bif:is_integer _0
%%   br _2, label 11, label 3
%%
%% 11:
%%   _3 = bif:is_atom _1
%%   br _3, label 6, label 3
%%
%% 6:
%%   ret literal ok
%%
%% 3: Error.
%%   ...

-module(beam_ssa_bool).
-export([module/2]).

%% Debugging.
-define(DEBUG, false).
-if(?DEBUG).
-export([dump/1]).
-endif.

-import(lists, [all/2,foldl/3,keyfind/3,last/1,partition/2,
                reverse/1,reverse/2,sort/1]).

-include("beam_ssa.hrl").

-record(st, {defs=#{},
             ldefs=#{},
             count :: beam_ssa:label(),
             dom,
             uses}).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,Module#b_module{body=Fs}}.

function(#b_function{anno=Anno}=F) ->
    try
        opt_function(F)
    catch
        Class:Error:Stack ->
            #{func_info:={_,Name,Arity}} = Anno,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

opt_function(#b_function{bs=Blocks0,cnt=Count0}=F) ->
    {Blocks1,Count1} = pre_opt(Blocks0, Count0),
    DefVars = interesting_defs(Blocks1),
    if
        map_size(DefVars) > 1 ->
            Dom = beam_ssa:dominators(Blocks1),
            Uses = beam_ssa:uses(Blocks1),
            St0 = #st{defs=DefVars,count=Count1,dom=Dom,uses=Uses},
            {Blocks,St} = bool_opt(Blocks1, St0),
            Count = St#st.count,
            F#b_function{bs=Blocks,cnt=Count};
        true ->
            %% There are no boolean operators that can be optimized in
            %% this function.
            F#b_function{bs=Blocks1,cnt=Count1}
    end.

%%%
%%% Do some optimizations to help the main boolean optimization pass:
%%%
%%%    * Remove `succeeded` instructions that can't fail after `and`,
%%%      `or`, and `not`. The main optimization pass can only optimize
%%%      boolean operators that are known not to fail.
%%%
%%%    * Rewrite a boolean #b_switch{} to a #b_br{} if the fail label
%%%      can't be reached or is not important. (The main optimization
%%%      can't handle #b_switch{}.)
%%%
%%%    * Simplify phi nodes, eliminating them if they only have one
%%%      value. Also annotate phi nodes that are known to evaluate
%%%      to a boolean.
%%%

-type var() :: beam_ssa:b_var().

%% Note: We use the substitution map for both substitutions and type
%% information. If the associated value for a variable is a #b_set{},
%% it means that the value is a boolean.
-type pre_sub_val() ::
        beam_ssa:value() |                      %Value to be substituted.
        beam_ssa:b_set() |                      %This variable is a boolean.
        {'true_or_any',beam_ssa:label()} |
        '=:='.

-type pre_sub_map() :: #{'uses' => {'uses',beam_ssa:block_map() | list()},
                         var() => pre_sub_val()}.

pre_opt(Blocks, Count) ->
    Top = beam_ssa:rpo(Blocks),

    %% Collect information to help the pre_opt pass to optimize
    %% `switch` instructions.
    Sub0 = #{uses => {uses,Blocks}},
    Sub1 = get_phi_info(Top, Blocks, Sub0),
    Sub = maps:remove(uses, Sub1),

    %% Now do the actual optimizations.
    Reached = gb_sets:singleton(hd(Top)),
    pre_opt(Top, Sub, Reached, Count, Blocks).

-spec get_phi_info(Ls, Blocks, Sub0) -> Sub when
      Ls :: [beam_ssa:label()],
      Blocks :: beam_ssa:block_map(),
      Sub0 :: pre_sub_map(),
      Sub :: pre_sub_map().

%% get_phi_info([Label], Blocks, Sub0) -> Sub.
%%  Collect information to help us optimize `switch` instructions
%%  such as:
%%
%%    switch SomeVar, label _, [ {literal false, _ }, {literal true, _ } ]
%%      .
%%      .
%%      .
%%    PhiVar = phi { SomeVar, _ }, { literal fail, _ }, { literal false, _}
%%    EqBool = bif:'=:=' PhiVar, literal true
%%
%%  Here it can be seen that `SomeVar` is compared to `true`. If
%%  `SomeVar` is not `true`, it does not matter whether its value is
%%  `false` or some other value. That means that the `switch` can be
%%  replaced with a two-way `br`:
%%
%%    NewBoolVar = bif:'=:=' SomeVar, literal true
%%    br NewBoolVar, label _, label _
%%
%%  For this example, the value {true_or_any,LabelOfPhiBlock} will be
%%  added for the key `SomeVar` in the substitution map.

get_phi_info([L|Ls], Blocks, Sub0) ->
    Sub = get_phi_info(Ls, Blocks, Sub0),
    #b_blk{is=Is} = map_get(L, Blocks),
    get_phi_info_is(Is, L, Sub);
get_phi_info([], _, Sub) -> Sub.

get_phi_info_is([I|Is], From, Sub0) ->
    Sub = get_phi_info_is(Is, From, Sub0),
    get_phi_info_instr(I, From, Sub);
get_phi_info_is([], _, Sub) -> Sub.

get_phi_info_instr(#b_set{op={bif,'=:='},
                          args=[#b_var{}=Bool,#b_literal{val=true}]},
                   _From, Sub) ->
    Sub#{Bool=>'=:='};
get_phi_info_instr(#b_set{op=phi,dst=Dst,args=Args}, From, Sub0) ->
    {Safe,Sub} =
        case Sub0 of
            #{Dst:='=:='} ->
                get_phi_info_single_use(Dst, Sub0);
            #{Dst:={true_or_any,_}} ->
                {true,Sub0};
            #{} ->
                {false,Sub0}
        end,
    case Safe of
        true ->
            foldl(fun({#b_var{}=V,_}, A) ->
                          A#{V => {true_or_any,From}};
                     (_, A) -> A
                  end, Sub, Args);
        false -> Sub
    end;
get_phi_info_instr(_, _, Sub) -> Sub.

get_phi_info_single_use(Var, Sub) ->
    case map_get(uses, Sub) of
        Uses when is_map(Uses) ->
            {case Uses of
                 #{Var:=[_]} -> true;
                 #{Var:=[_|_]} -> false
             end,Sub};
        {uses,Blocks} ->
            Uses = beam_ssa:uses(Blocks),
            get_phi_info_single_use(Var, Sub#{uses => Uses})
    end.

-spec pre_opt(Ls, Sub, Reached, Count0, Blocks0) -> {Blocks,Count} when
      Ls :: [beam_ssa:label()],
      Reached :: gb_sets:set(beam_ssa:label()),
      Count0 :: beam_ssa:label(),
      Blocks0 :: beam_ssa:block_map(),
      Sub :: pre_sub_map(),
      Count :: beam_ssa:label(),
      Blocks :: beam_ssa:block_map().

pre_opt([L|Ls], Sub0, Reached0, Count0, Blocks) ->
    case gb_sets:is_member(L, Reached0) of
        false ->
            %% This block will never be reached.
            pre_opt(Ls, Sub0, Reached0, Count0, maps:remove(L, Blocks));
        true ->
            #b_blk{is=Is0,last=Last0} = Blk0 = map_get(L, Blocks),
            {Is,Sub} = pre_opt_is(Is0, Reached0, Sub0, []),
            case pre_opt_terminator(Last0, Sub, Blocks) of
                {#b_set{}=Test0,#b_br{}=Br0} ->
                    %% Here is a #b_switch{} that has been reduced to
                    %% a '=:=' followed by a two-way `br`.
                    Bool = #b_var{name={'@ssa_bool',Count0}},
                    Count = Count0 + 1,
                    Test = Test0#b_set{dst=Bool},
                    Br = Br0#b_br{bool=Bool},
                    Blk = Blk0#b_blk{is=Is++[Test],last=Br},
                    Successors = beam_ssa:successors(Blk),
                    Reached = gb_sets:union(Reached0,
                                            gb_sets:from_list(Successors)),
                    pre_opt(Ls, Sub, Reached, Count, Blocks#{L:=Blk});
                Last ->
                    Blk = Blk0#b_blk{is=Is,last=Last},
                    Successors = beam_ssa:successors(Blk),
                    Reached = gb_sets:union(Reached0,
                                            gb_sets:from_list(Successors)),
                    pre_opt(Ls, Sub, Reached, Count0, Blocks#{L:=Blk})
            end
    end;
pre_opt([], _, _, Count, Blocks) ->
    {Blocks,Count}.

pre_opt_is([#b_set{op=phi,dst=Dst,args=Args0}=I0|Is], Reached, Sub0, Acc) ->
    Args1 = [{Val,From} || {Val,From} <- Args0,
                           gb_sets:is_member(From, Reached)],
    Args = sub_args(Args1, Sub0),
    case all_same(Args) of
        true ->
            %% Single value or all values are the same. We can remove
            %% the phi node.
            {Arg,_} = hd(Args),
            Sub = Sub0#{Dst=>Arg},
            pre_opt_is(Is, Reached, Sub, Acc);
        false ->
            case pre_is_phi_bool(Args, Sub0) of
                true ->
                    %% The value of the phi node is always a
                    %% boolean. Update type information in the sub map
                    %% and add an annotation.
                    Anno = I0#b_set.anno,
                    I = I0#b_set{args=Args,anno=Anno#{boolean_phi=>true}},
                    Sub = Sub0#{Dst=>I},
                    pre_opt_is(Is, Reached, Sub, [I|Acc]);
                false ->
                    I = I0#b_set{args=Args},
                    pre_opt_is(Is, Reached, Sub0, [I|Acc])
            end
    end;
pre_opt_is([#b_set{op=succeeded,dst=Dst,args=Args0}=I0|Is], Reached, Sub0, Acc) ->
    [Arg] = Args = sub_args(Args0, Sub0),
    I = I0#b_set{args=Args},
    case pre_is_safe_bool(Arg, Sub0) of
        true ->
            %% The preceding boolean operation can't fail. Get rid
            %% of this `succeeded` instruction.
            Sub = Sub0#{Dst=>#b_literal{val=true}},
            pre_opt_is(Is, Reached, Sub, Acc);
        false ->
            pre_opt_is(Is, Reached, Sub0, [I|Acc])
    end;
pre_opt_is([#b_set{dst=Dst,args=Args0}=I0|Is], Reached, Sub0, Acc) ->
    Args = sub_args(Args0, Sub0),
    I = I0#b_set{args=Args},
    case is_bool_expr(I) of
        true ->
            case pre_eval_op(I, Sub0) of
                none ->
                    Sub = Sub0#{Dst=>I},
                    pre_opt_is(Is, Reached, Sub, [I|Acc]);
                #b_var{}=Var ->
                    %% We must remove the 'succeeded' instruction that
                    %% follows since the variable it checks is gone.
                    [#b_set{op=succeeded,dst=SuccDst,args=[Dst]}] = Is,
                    Sub = Sub0#{Dst=>Var,SuccDst=>#b_literal{val=true}},
                    pre_opt_is([], Reached, Sub, Acc);
                #b_literal{}=Lit ->
                    Sub = Sub0#{Dst=>Lit},
                    pre_opt_is(Is, Reached, Sub, Acc)
            end;
        false ->
            pre_opt_is(Is, Reached, Sub0, [I|Acc])
        end;
pre_opt_is([], _Reached, Sub, Acc) ->
    {reverse(Acc),Sub}.

pre_opt_terminator(#b_br{bool=#b_literal{}}=Br, _Sub, _Blocks) ->
    Br;
pre_opt_terminator(#b_br{bool=Bool}=Br0, Sub, Blocks) ->
    case beam_ssa:normalize(Br0#b_br{bool=sub_arg(Bool, Sub)}) of
        Br0 ->
            Br0;
        #b_br{bool=#b_literal{val=true},succ=Next}=Br ->
            %% See if the terminator from the successor block
            %% can be incorporated into this block to give
            %% more opportunities for optimization.
            #b_blk{is=Is,last=Last} = map_get(Next, Blocks),
            case {Is,Last} of
                {[],#b_switch{}} ->
                    pre_opt_terminator(Last, Sub, Blocks);
                {_,_} ->
                    Br
            end
    end;
pre_opt_terminator(#b_ret{arg=Arg}=Ret, Sub, _Blocks) ->
    Ret#b_ret{arg=sub_arg(Arg, Sub)};
pre_opt_terminator(#b_switch{arg=Arg0,list=List}=Sw0, Sub, Blocks) ->
    Arg = sub_arg(Arg0, Sub),
    Sw = Sw0#b_switch{arg=Arg},
    case sort(List) of
        [{#b_literal{val=false},Fail},
         {#b_literal{val=true},Succ}] ->
            case pre_is_arg_bool(Arg, Sub) of
                false ->
                    pre_opt_sw(Sw, Fail, Succ, Sub, Blocks);
                true ->
                    beam_ssa:normalize(#b_br{bool=Arg,succ=Succ,fail=Fail})
            end;
        _ ->
            Sw
    end.

pre_opt_sw(#b_switch{arg=Arg,fail=Fail}=Sw, False, True, Sub, Blocks) ->
    case Sub of
        #{Arg:={true_or_any,PhiL}} ->
            #{Fail:=FailBlk,False:=FalseBlk,PhiL:=PhiBlk} = Blocks,
            case {FailBlk,FalseBlk,PhiBlk} of
                {#b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}},
                 #b_blk{is=[],last=#b_br{succ=PhiL,fail=PhiL}},
                 #b_blk{is=[#b_set{op=phi,args=PhiArgs}|_]}} ->
                    case keyfind(False, 2, PhiArgs) of
                        {#b_literal{val=Bool},False} when Bool =/= true ->
                            %% This is an `andalso` in a guard. The code
                            %% can be simplified to a two-way `br` because
                            %% the actual value of the variable does not
                            %% matter if it is not equal to `true`.
                            DummyDst = #b_var{name=0},
                            {#b_set{op={bif,'=:='},dst=DummyDst,
                                    args=[Arg,#b_literal{val=true}]},
                             #b_br{bool=DummyDst,succ=True,fail=False}};
                        {_,_} ->
                            Sw
                    end;
                {_,_,_} ->
                    Sw
            end;
        #{} ->
            Sw
    end.

pre_eval_op(#b_set{op={bif,Op},args=Args}, Sub) ->
    case pre_are_args_bool(Args, Sub) of
        true ->
            case {Op,Args} of
                {'and',[#b_literal{val=true},#b_var{}=Res]} -> Res;
                {'and',[#b_literal{val=false}=Res,#b_var{}]} -> Res;
                {'and',[#b_var{}=Res,#b_literal{val=true}]} -> Res;
                {'and',[#b_var{},#b_literal{val=false}=Res]} -> Res;
                {'or',[#b_literal{val=true}=Res,#b_var{}]} -> Res;
                {'or',[#b_literal{val=false},#b_var{}=Res]} -> Res;
                {'or',[#b_var{},#b_literal{val=true}=Res]} -> Res;
                {'or',[#b_var{}=Res,#b_literal{val=false}]} -> Res;
                _ -> none
            end;
        false ->
            none
    end.

all_same([{H,_}|T]) ->
    all(fun({E,_}) -> E =:= H end, T).

pre_is_phi_bool([{#b_literal{val=Lit},_}|As], Sub) ->
    is_boolean(Lit) andalso pre_is_phi_bool(As, Sub);
pre_is_phi_bool([{#b_var{}=A,_}|As], Sub) ->
    case Sub of
        #{A:=#b_set{}} ->
            pre_is_phi_bool(As, Sub);
        #{} ->
            false
    end;
pre_is_phi_bool([], _Sub) -> true.

pre_is_safe_bool(#b_literal{}, _Sub) ->
    true;
pre_is_safe_bool(Var, Sub) ->
    case Sub of
        #{Var:=#b_set{op={bif,is_function},
                      args=[_,Arity]}} ->
            case Arity of
                #b_literal{val=Lit} ->
                    is_integer(Lit) andalso Lit >= 0;
                #b_var{} ->
                    false
            end;
        #{Var:=#b_set{op={bif,Op},args=Args}} ->
            Arity = length(Args),
            erl_internal:bool_op(Op, Arity) andalso
                pre_are_args_bool(Args, Sub);
        #{} ->
            false
    end.

pre_are_args_bool([A|As], Sub) ->
    pre_is_arg_bool(A, Sub) andalso pre_are_args_bool(As, Sub);
pre_are_args_bool([], _Sub) -> true.

pre_is_arg_bool(#b_literal{val=Lit}, _Sub) ->
    is_boolean(Lit);
pre_is_arg_bool(#b_var{}=A, Sub) ->
    case Sub of
        #{A:=#b_set{}} ->
            true;
        #{} ->
            false
    end.

%%%
%%% Build a map from variable to definitions for boolean expressions
%%% phi nodes. This map will be used by collect_bool_vars() and by
%%% shortcut_branches().
%%%

interesting_defs(Blocks) ->
    interesting_defs(maps:to_list(Blocks), []).

interesting_defs([{L,#b_blk{is=Is}}|Bs], Acc) ->
    interesting_defs(Bs, interesting_defs_is(Is, L, Acc));
interesting_defs([], Acc) ->
    maps:from_list(Acc).

interesting_defs_is([#b_set{op={bif,_},dst=V}=I|Is], L, Acc) ->
    case is_bool_expr(I) of
        true ->
            interesting_defs_is(Is, L, [{V,{L,I}}|Acc]);
        false ->
            interesting_defs_is(Is, L, Acc)
    end;
interesting_defs_is([#b_set{op=phi,dst=V}=Set|Is], L, Acc) ->
    interesting_defs_is(Is, L, [{V,{L,Set}}|Acc]);
interesting_defs_is([#b_set{}|Is], L, Acc) ->
    interesting_defs_is(Is, L, Acc);
interesting_defs_is([], _L, Acc) -> Acc.

%%%
%%% Search for boolean expressions to optimize.
%%%
%%% The main purpose of this module is to optimize guards. A guard ends in the
%%% following instructions:
%%%
%%%      Bool = bif:'=:=' Var, literal true
%%%      br BoolVar, label Success, label Failure
%%%
%%% To make sure that we'll find the end of the guard instead of some
%%% interior '=:=' instruction we will visit the blocks in postorder.
%%%

bool_opt(Blocks, St) ->
    bool_opt(beam_ssa:rpo(Blocks), Blocks, St).

bool_opt([L|Ls], Blocks0, St0) ->
    {Blocks,St1} = bool_opt(Ls, Blocks0, St0),
    case Blocks of
        #{L:=#b_blk{is=[_|_]=Is,last=#b_br{bool=#b_var{}=Bool}=Br}} ->
            case last(Is) of
                #b_set{op={bif,'=:='},dst=Bool,
                       args=[#b_var{},#b_literal{val=true}]} ->
                    try
                        bool_opt_rewrite(Bool, L, Br, Blocks, St1)
                    catch
                        throw:not_possible ->
                            {Blocks,St1}
                    end;
                #b_set{} ->
                    {Blocks,St1}
            end;
        #{} ->
            %% Either this block was removed by a previous successful
            %% optimization, it is empty, or its terminator is not a
            %% two-way `br` instruction.
            {Blocks,St1}
    end;
bool_opt([], Blocks, St) ->
    {Blocks,St}.

bool_opt_rewrite(Bool, From, Br, Blocks0, St0) ->
    TreeVars = collect_bool_vars(Bool, St0),
    case TreeVars of
        [Bool] ->
            %% Only one variable means that there is nothing to
            %% optimize.  (The variable is either a function argument,
            %% or has been defined by an instruction such as `call` or
            %% `get_tuple_element`.)
            not_possible();
        [_|_] ->
            ok
    end,

    %% Find the common dominator block for all the blocks with boolean
    %% variables.
    Dom = bool_opt_dom(TreeVars, St0),

    %% Split out non-boolean instruction from the block that dominates
    %% all the boolean operators. Splitting will save some work, and
    %% it could also make more optimizations possible since phi nodes
    %% could be difficult to handle later when they have been included
    %% in the graph.
    {DomPreIs,Blocks1} = split_dom_block(Dom, Blocks0),

    %% Collect all blocks from the Dom block up to and including
    %% the From block.
    Bs = collect_digraph_blocks(Dom, From, Br, Blocks1),

    %% Build a digraph from the collected blocks.
    {Root,G0,St1} = build_digraph(Bs, Br, St0),

    %% Optimize the digraph.
    LDefs = digraph_bool_def(G0),
    St = St1#st{ldefs=LDefs},
    G1 = opt_digraph_top(Bool, G0, St),
    G = shortcut_branches(Root, G1, St),

    %% Make sure that every variable that is used will be defined
    %% on every path to its use.
    ensure_init(Root, G, G0),

    %% Delete the original blocks. This is important so that we will not
    %% try optimize the already optimized code. That would not work
    %% because the map of definitions in St#st.defs would not be updated
    %% to include the newly optimized blocks.
    DomBlk0 = map_get(Dom, Blocks1),
    Blocks2 = maps:without([L || {L,#b_blk{}} <- Bs], Blocks1),

    %% Convert the optimized digraph back to SSA code.
    Blocks3 = digraph_to_ssa([Root], G, Blocks2),

    %% Add a branch from the pre-sequence in the dominating block to
    %% the first block of the optimized code.
    DomBlk = DomBlk0#b_blk{is=DomPreIs,last=oneway_br(Root)},
    Blocks = Blocks3#{Dom => DomBlk},
    {Blocks,St#st{ldefs=#{}}}.

%%%
%%% Collect boolean variables recursively reachable from the root
%%% boolean variable.
%%%

collect_bool_vars(RootBool, St) ->
    #b_set{args=[#b_var{}=Var,#b_literal{}]} = get_def(RootBool, St),
    collect_bool_vars([Var], St, [RootBool]).

collect_bool_vars([V|Vs], St, Acc) ->
    case get_def(V, St) of
        #b_set{op=phi,anno=Anno,args=Args} ->
            {Vars,Ls} = collect_phi_args(Args, Anno),
            collect_bool_vars(Vars ++ Vs, St, Ls ++ Vars ++ Acc);
        #b_set{args=Args}=I ->
            %% This is a boolean expression.
            Vars = [Arg || #b_var{}=Arg <- Args],
            case is_rewritable_bool_op(I) of
                true ->
                    %% This is a bool op ('and', 'or', or
                    %% 'not'). Recursively collect more boolean
                    %% variables from its arguments.
                    collect_bool_vars(Vars ++ Vs, St, [V|Acc]);
                false ->
                    %% This is a comparison operator (such as `<`) or
                    %% type test. Don't visit its arguments
                    %% recursively.
                    collect_bool_vars(Vs, St, [V|Acc])
            end;
        none ->
            collect_bool_vars(Vs, St, Acc)
    end;
collect_bool_vars([], _St, Acc) ->
    ordsets:from_list(Acc).

is_rewritable_bool_op(#b_set{op={bif,Bif}}) ->
    %% `xor` is a bool op, but it is not practical to rewrite it.
    case Bif of
        'and' -> true;
        'or' -> true;
        'not' -> true;
        _ -> false
    end.

collect_phi_args(Args, Anno) ->
    case is_map_key(boolean_phi, Anno) of
        true ->
            Vars = [V || {#b_var{}=V,_} <- Args],
            case Vars of
                [_|_] ->
                    {Vars,[]};
                [] ->
                    %% This phi node only contains literal values.
                    %% Force the inclusion of referenced blocks.
                    Ls = [{block,L} || {_,L} <- Args],
                    {[],Ls}
            end;
        false ->
            %% We can't rewrite phi nodes that don't return
            %% a boolean value.
            {[],[]}
    end.

%%%
%%% Dominator utility functions.
%%%

bool_opt_dom(TreeVars, #st{defs=Defs,dom={DomBy,Num}}) ->
    Ls0 = foldl(fun({block,L}, A) ->
                        [L|A];
                   (V, A) ->
                        {L,_} = map_get(V, Defs),
                        [L|A]
                end, [], TreeVars),
    Ls = ordsets:from_list(Ls0),
    [Common|_] = beam_ssa:common_dominators(Ls, DomBy, Num),
    Common.

split_dom_block(L, Blocks0) ->
    #b_blk{is=Is} = Blk0 = map_get(L, Blocks0),
    {PreIs,TailIs} = split_dom_block_is(Is, []),
    Blk = Blk0#b_blk{is=TailIs},
    Blocks = Blocks0#{L:=Blk},
    {PreIs,Blocks}.

split_dom_block_is([#b_set{},#b_set{op=succeeded}]=Is, PreAcc) ->
    {reverse(PreAcc),Is};
split_dom_block_is([#b_set{}=I|Is]=Is0, PreAcc) ->
    case is_bool_expr(I) of
        true ->
            {reverse(PreAcc),Is0};
        false ->
            split_dom_block_is(Is, [I|PreAcc])
    end;
split_dom_block_is([], PreAcc) ->
    {reverse(PreAcc),[]}.

%%%
%%% Find and collect the blocks that should be converted to a digraph.
%%%

collect_digraph_blocks(FirstL, LastL, #b_br{succ=Succ,fail=Fail}, Blocks) ->
    Ws = gb_sets:singleton(FirstL),
    Seen = cerl_sets:from_list([Succ,Fail]),
    collect_digraph_blocks(Ws, LastL, Blocks, Seen, []).

collect_digraph_blocks(Ws0, LastL, Blocks, Seen0, Acc0) ->
    case gb_sets:is_empty(Ws0) of
        true ->
            Acc0;
        false ->
            {L,Ws1} = gb_sets:take_smallest(Ws0),
            Seen = cerl_sets:add_element(L, Seen0),
            Blk = map_get(L, Blocks),
            Acc = [{L,Blk}|Acc0],
            Ws = cdb_update_workset(L, Blk, LastL, Seen, Ws1),
            collect_digraph_blocks(Ws, LastL, Blocks, Seen, Acc)
    end.

cdb_update_workset(LastL, _Blk, LastL, _Seen, Ws) ->
    Ws;
cdb_update_workset(_L, Blk, _LastL, Seen, Ws) ->
    Successors = beam_ssa:successors(Blk),
    cdb_update_workset(Successors, Seen, Ws).

cdb_update_workset([L|Ls], Seen, Ws) ->
    case cerl_sets:is_element(L, Seen) of
        true ->
            cdb_update_workset(Ls, Seen, Ws);
        false ->
            cdb_update_workset(Ls, Seen, gb_sets:add_element(L, Ws))
    end;
cdb_update_workset([], _Seen, Ws) -> Ws.

%%%
%%% For the blocks from the dominating block up to the last block,
%%% build a digraph where each vertex is an instruction. This is just
%%% a more convenient way to represent the code, more suitable for
%%% the optimizations we are about to do.
%%%

build_digraph(Bs, #b_br{succ=Succ,fail=Fail}, St0) ->
    Ignore = ordsets:from_list([Succ,Fail]),
    G0 = dg_new(),
    {Map0,G1,St1} = build_mapping(Bs, #{}, G0, St0),
    {Map,G2} = add_external_vertices(Ignore, Map0, G1),
    {G,St} = build_digraph_1(Bs, G2, Map, St1),

    %% Find the root node now. After we have done optimizations,
    %% there may be more than one root node (that is, nodes without
    %% any incident vertices).
    [Root] = digraph_roots(G),
    {Root,G,St}.

build_mapping([{L,Blk}|Bs], Map0, G0, St0) ->
    {Vtx,St} = new_label(St0),
    Map = Map0#{L=>Vtx},
    Label = case Blk of
                #b_blk{is=[]} -> br;
                #b_blk{} -> initial
            end,
    G = dg_add_vertex(G0, Vtx, Label),
    build_mapping(Bs, Map, G, St);
build_mapping([], Map, G, St) ->
    {Map,G,St}.

add_external_vertices([V|Vs], Map0, G0) ->
    G = dg_add_vertex(G0, V, {external,#{}}),
    Map = Map0#{V=>V},
    add_external_vertices(Vs, Map, G);
add_external_vertices([], Map, G) ->
    {Map,G}.

build_digraph_1([{L,Blk}|Bs], G0, Map, St0) ->
    #b_blk{is=Is,last=Last} = Blk,
    Vtx = map_get(L, Map),
    {G,St} = build_digraph_is(Is, Last, Vtx, Map, G0, St0),
    build_digraph_1(Bs, G, Map, St);
build_digraph_1([], G, _Map, St) ->
    {G,St}.

build_digraph_is([#b_set{op=phi,args=Args0}=I0|Is], Last, Vtx, Map, G, St) ->
    Args = [{V,map_get(L, Map)} || {V,L} <- Args0],
    I = I0#b_set{args=Args},
    build_digraph_is_1(I, Is, Last, Vtx, Map, G, St);
build_digraph_is([#b_set{}=I|Is], Last, Vtx, Map, G, St) ->
    case beam_ssa:no_side_effect(I) of
        true ->
            build_digraph_is_1(I, Is, Last, Vtx, Map, G, St);
        false ->
            not_possible()
    end;
build_digraph_is([], Last, From, Map, G0, St) ->
    case Last of
        #b_br{bool=#b_literal{val=true},succ=To0,fail=To0} ->
            To = map_get(To0, Map),
            G = dg_add_edge(G0, From, To, next),
            {G,St};
        #b_br{bool=#b_var{}=Bool,succ=Succ0,fail=Fail0} ->
            #{Succ0:=Succ,Fail0:=Fail} = Map,
            case dg_vertex(G0, From) of
                #b_set{dst=Bool} ->
                    G = add_succ_fail_edges(From, Succ, Fail, G0),
                    {G,St};
                #b_set{} ->
                    %% Wrong variable being tested. This is rare.
                    not_possible();
                br ->
                    G1 = add_succ_fail_edges(From, Succ, Fail, G0),
                    G = dg_add_vertex(G1, From, {br,Bool}),
                    {G,St}
            end;
        _ ->
            not_possible()
    end.

build_digraph_is_1(I, Is, Last, Vtx, Map, G0, St0) ->
    G1 = dg_add_vertex(G0, Vtx, I),
    case Is of
        [] ->
            build_digraph_is(Is, Last, Vtx, Map, G1, St0);
        [_|_] ->
            {NextVtx,St} = new_label(St0),
            G2 = dg_add_vertex(G1, NextVtx, initial),
            G = dg_add_edge(G2, Vtx, NextVtx, next),
            build_digraph_is(Is, Last, NextVtx, Map, G, St)
    end.

%%%
%%% Optimize the graph, attempting to eliminating 'and', 'or', and 'not'
%%% instructions.
%%%

opt_digraph_top(Arg, G0, St) ->
    I = get_def(Arg, G0, St),
    #b_set{op={bif,'=:='},dst=Dst,
           args=[#b_var{}=Bool,#b_literal{val=true}]} = I,
    {br,Succ,Fail} = get_targets(Dst, G0, St),
    G1 = ensure_single_use(Dst, G0, St),
    G = convert_to_br_node(I, Succ, G1, St),
    redirect_test(Bool, {fail,Fail}, G, St).

do_opt_digraph([A|As], G0, St) ->
    I = get_def(A, G0, St),
    try opt_digraph_instr(I, G0, St) of
        G ->
            do_opt_digraph(As, G, St)
    catch
        throw:not_possible ->
            do_opt_digraph(As, G0, St)
    end;
do_opt_digraph([], G, _St) -> G.

opt_digraph_instr(#b_set{dst=Dst}=I, G0, St) ->
    %% We KNOW that this node has two outgoing edges (one labeled
    %% `succ` and one `fail`).
    {br,Succ,Fail} = get_targets(Dst, G0, St),
    G1 = ensure_single_use(Dst, G0, St),
    case I of
        #b_set{op={bif,'and'},args=Args} ->
            G2 = convert_to_br_node(I, Succ, G1, St),
            {First,Second} = order_args(Args, G2, St),
            G = redirect_test(First, {fail,Fail}, G2, St),
            redirect_test(Second, {fail,Fail}, G, St);
        #b_set{op={bif,'or'},args=Args} ->
            {First,Second} = order_args(Args, G1, St),

            %% Here we give up the optimization if the optimization
            %% would skip instructions that may fail. A possible
            %% future improvement would be to hoist the failing
            %% instructions so that they would always be executed.
            ensure_no_failing_instructions(First, Second, G1, St),

            G2 = convert_to_br_node(I, Succ, G1, St),
            G = redirect_test(First, {succ,Succ}, G2, St),
            redirect_test(Second, {fail,Fail}, G, St);
        #b_set{op={bif,'xor'}} ->
            %% Rewriting 'xor' is not practical. Fortunately,
            %% 'xor' is almost never used in practice.
            not_possible();
        #b_set{op={bif,'not'},args=[#b_var{}=Bool]} ->
            G = convert_to_br_node(I, Fail, G1, St),
            redirect_test(Bool, {fail,Succ}, G, St);
        #b_set{op=phi,dst=Bool} ->
            Vtx = get_vertex(Bool, St),
            G2 = del_out_edges(Vtx, G1),
            G = dg_add_edge(G2, Vtx, Succ, next),
            redirect_test(Bool, {fail,Fail}, G, St);
        #b_set{} ->
            G1
    end.

ensure_single_use(Bool, G, #st{uses=U}=St) ->
    case map_get(Bool, U) of
        [_] ->
            G;
        Uses ->
            Vtx = get_vertex(Bool, St),
            ensure_single_use_1(Bool, Vtx, Uses, G)
    end.

ensure_single_use_1(Bool, Vtx, Uses, G) ->
    Fail = case get_targets(Vtx, G) of
               {br,_,Fail0} -> Fail0;
               _ -> not_possible()
           end,
    case partition(fun({L,#b_set{}}) when L =:= Fail -> true;
                      (_) -> false
                   end, Uses) of
        {[_],[_]} ->
            case dg_vertex(G, Fail) of
                {external,Bs0} ->
                    %% The only other use of the variable Bool
                    %% is in the failure block. It can be
                    %% replaced with the literal `false`
                    %% in that block.
                    Bs = Bs0#{Bool => #b_literal{val=false}},
                    dg_add_vertex(G, Fail, {external,Bs});
                _ ->
                    not_possible()
            end;
        {_,_} ->
            not_possible()
    end.

convert_to_br_node(I, Target, G0, St) ->
    Vtx = get_vertex(I, St),
    G1 = del_out_edges(Vtx, G0),
    G = dg_add_vertex(G1, Vtx, br),
    dg_add_edge(G, Vtx, Target, next).


%% ensure_no_failing_instructions(First, Second, G, St) -> ok.
%%  Ensure that there are no instructions that can fail that would not
%%  be executed if right-hand side of the `or` would be skipped. That
%%  means that the `or` could succeed when it was supposed to
%%  fail. Example:
%%
%%    (element(1, T) =:= tag) or
%%    (element(10, T) =:= y)

ensure_no_failing_instructions(First, Second, G, St) ->
    Vs0 = covered(get_vertex(First, St), get_vertex(Second, St), G),
    Vs = [{V,dg_vertex(G, V)} || V <- Vs0],
    Failing = [P || {V,#b_set{op=succeeded}}=P <- Vs,
                    not eaten_by_phi(V, G)],
    case Failing of
        [] -> ok;
        [_|_] -> not_possible()
    end.

eaten_by_phi(V, G) ->
    {br,_,Fail} = get_targets(V, G),
    case dg_vertex(G, Fail) of
        br ->
            [To] = dg_out_neighbours(G, Fail),
            case dg_vertex(G, To) of
                #b_set{op=phi} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% order_args([Arg1,Arg2], G, St) -> {First,Second}.
%%  Order arguments for a boolean operator so that there is path in the
%%  digraph from the instruction referered to by the first operand to
%%  the instruction refered to by the second operand.

order_args([#b_var{}=VarA,#b_var{}=VarB], G, St) ->
    {VA,VB} = {get_vertex(VarA, St),get_vertex(VarB, St)},
    case dg_is_path(G, VA, VB) of
        true ->
            %% Core Erlang code generated by v3_core always
            %% has operands already in correct order.
            {VarA,VarB};
        false ->
            %% Core Erlang code generated by other frontends
            %% such as LFE may have the operands swapped.
            true = dg_is_path(G, VB, VA),       %Assertion.
            {VarB,VarB}
    end;
order_args(_Args, _G, _St) ->
    %% Literal operands. Can only happen if the Core Erlang optimization
    %% passes have been turned off.
    not_possible().

redirect_test(Bool, SuccFail, G0, St) ->
    V = get_vertex(Bool, St),
    I = get_def(Bool, G0, St),
    case I of
        #b_set{op=phi,args=Args} ->
            G = ensure_single_use(Bool, G0, St),
            redirect_phi(Bool, Args, SuccFail, G, St);
        #b_set{} ->
            G1 = redirect_test_1(V, SuccFail, G0),
            G = ensure_single_use(Bool, G1, St),
            do_opt_digraph([Bool], G, St)
    end.

redirect_test_1(V, SuccFail, G) ->
    case get_targets(V, G) of
        {br,_Succ,Fail} ->
            %% I have only seen this happen in code generated by LFE
            %% (in lfe_andor_SUITE.core and lfe_guard_SUITE.core)
            case SuccFail of
                {fail,Fail} -> G;
                {succ,_} -> not_possible()
            end;
        {br,Next} ->
            case SuccFail of
                {succ,Succ} ->
                    add_succ_fail_edges(V, Succ, Next, G);
                {fail,Fail} ->
                    add_succ_fail_edges(V, Next, Fail, G)
            end
    end.

redirect_phi(Phi, Args, SuccFail, G0, St) ->
    PhiVtx = get_vertex(Phi, St),
    G = dg_add_vertex(G0, PhiVtx, br),
    redirect_phi_1(PhiVtx, sort(Args), SuccFail, G, St).

redirect_phi_1(PhiVtx, [{#b_literal{val=false},FalseExit},
                        {#b_var{}=SuccBool,_BoolExit}],
             SuccFail, G0, St) ->
    BoolVtx = get_vertex(SuccBool, St),
    [FalseOut] = dg_out_edges(G0, FalseExit),
    G1 = dg_del_edge(G0, FalseOut),
    case SuccFail of
        {fail,Fail} ->
            G2 = dg_add_edge(G1, FalseExit, Fail, next),
            G = add_succ_fail_edges(BoolVtx, PhiVtx, FalseExit, G2),
            do_opt_digraph([SuccBool], G, St);
        {succ,Succ} ->
            G2 = dg_add_edge(G1, FalseExit, PhiVtx, next),
            G = add_succ_fail_edges(BoolVtx, Succ, PhiVtx, G2),
            do_opt_digraph([SuccBool], G, St)
    end;
redirect_phi_1(PhiVtx, [{#b_literal{val=true},TrueExit},
                        {#b_var{}=SuccBool,_BoolExit}],
             {fail,Fail}, G0, St) ->
    %% This was probably an `orelse` in the source code.
    BoolVtx = get_vertex(SuccBool, St),
    [TrueOut] = dg_out_edges(G0, TrueExit),
    G1 = dg_del_edge(G0, TrueOut),
    G2 = dg_add_edge(G1, TrueExit, PhiVtx, next),
    G = add_succ_fail_edges(BoolVtx, PhiVtx, Fail, G2),
    %% As as future improvement, we could follow TrueExit
    %% back to its originating boolean expression and
    %% optimize that too.
    do_opt_digraph([SuccBool], G, St);
redirect_phi_1(_PhiVtx, [{#b_literal{val=false},FalseExit},
                         {#b_literal{val=true},TrueExit}],
               SuccFail, G0, _St) ->
    case SuccFail of
        {fail,Fail} ->
            [FalseOut] = dg_out_edges(G0, FalseExit),
            G = dg_del_edge(G0, FalseOut),
            dg_add_edge(G, FalseExit, Fail, next);
        {succ,Succ} ->
            [TrueOut] = dg_out_edges(G0, TrueExit),
            G = dg_del_edge(G0, TrueOut),
            dg_add_edge(G, TrueExit, Succ, next)
    end;
redirect_phi_1(_PhiVtx, _Args, _SuccFail, _G, _St) ->
    not_possible().

digraph_bool_def(G) ->
    Vs = dg_vertices(G),
    Ds = [{Dst,Vtx} || {Vtx,#b_set{dst=Dst}} <- Vs],
    maps:from_list(Ds).

%%%
%%% Shortcut branches that branch to other branches.
%%%
%%% Shortcutting may eliminate problems with variables that
%%% are not defined on all paths to their use. For example,
%%% code such as the following can be made safe again:
%%%
%%%       ensure_written(Head, false) when not Head#head.ram_file -> ...
%%%
%%% Shortcutting also simplifies the conversion from the digraph
%%% back to the standard SSA format.
%%%

shortcut_branches(Vtx, G, St) ->
    Vs = reverse(dg_reverse_postorder(G, [Vtx])),
    do_shortcut_branches(Vs, G, St).

do_shortcut_branches([V|Vs], G0, St) ->
    case get_targets(V, G0) of
        {br,Succ0,Fail0} ->
            {SuccBs,FailBs} = eval_bs(V, G0, St),
            Succ = eval_instr(Succ0, G0, SuccBs),
            G1 = redirect_edge(V, Succ0, {succ,Succ}, G0),
            Fail = eval_instr(Fail0, G1, FailBs),
            G = redirect_edge(V, Fail0, {fail,Fail}, G1),
            do_shortcut_branches(Vs, G, St);
        {br,Next0} ->
            Next = eval_instr(Next0, G0, #{}),
            G = redirect_edge(V, Next0, {next,Next}, G0),
            do_shortcut_branches(Vs, G, St);
        none ->
            %% This is an external vertex.
            do_shortcut_branches(Vs, G0, St)
    end;
do_shortcut_branches([], G, _St) -> G.

redirect_edge(_From, To, {_Label,To}, G) ->
    G;
redirect_edge(From, To0, {Label,To}, G0) ->
    G = dg_del_edge(G0, {From,To0,Label}),
    dg_add_edge(G, From, To, Label).

eval_bs(Vtx, G, St) ->
    case dg_vertex(G, Vtx) of
        #b_set{op={bif,'=:='},args=[#b_var{}=Bool,#b_literal{val=true}]} ->
            case get_def(Bool, G, St) of
                #b_set{op=phi}=Phi ->
                    phi_bs(Phi);
                _ ->
                    {#{},#{}}
            end;
        _ ->
            {#{},#{}}
    end.

phi_bs(#b_set{op=phi,dst=PhiDst,args=PhiArgs}) ->
    Literals0 = [Lit || {#b_literal{}=Lit,_} <- PhiArgs],
    case length(Literals0) =:= length(PhiArgs) of
        true ->
            %% The values in the phi node are all literals.
            Literals = ordsets:from_list(Literals0),
            case partition(fun(#b_literal{val=Val}) ->
                                   Val =:= true
                           end, Literals) of
                {[True],[FailVal]} ->
                    %% As there is only two possible values, we can
                    %% predict the value of the phi node on both
                    %% branches.
                    SuccBs = #{PhiDst => True},
                    FailBs = #{PhiDst => FailVal},
                    {SuccBs,FailBs};
                {_,_} ->
                    {#{},#{}}
            end;
        false ->
            {#{},#{}}
    end.

eval_instr(Vtx, G, Bs) ->
    case dg_vertex(G, Vtx) of
        #b_set{} when map_size(Bs) =:= 0 ->
            %% With no bindings, eval_safe_bool_expr() is
            %% unlikely to do anything useful. If we would
            %% call it anyway, the time complexity would be
            %% quadratic, which would be slow for large
            %% graphs.
            Vtx;
        #b_set{}=I ->
            case is_safe_bool_expr(I) of
                true -> eval_safe_bool_expr(I, Vtx, G, Bs);
                false -> Vtx
            end;
        br ->
            %% We can shortcut this branch unless its
            %% target is a phi node.
            [Next] = dg_out_neighbours(G, Vtx),
            case dg_vertex(G, Next) of
                #b_set{op=phi} -> Vtx;
                _ -> eval_instr(Next, G, Bs)
            end;
        {br,#b_var{}} ->
            Vtx;
        {external,_} ->
            Vtx
    end.

eval_safe_bool_expr(#b_set{op={bif,Bif},dst=Dst,args=Args0}, Vtx, G, Bs) ->
    case get_targets(Vtx, G) of
        {br,Succ,Fail} ->
            True = #b_literal{val=true},
            False = #b_literal{val=false},
            Args = sub_args(Args0, Bs),
            case eval_bif(Bif, Args) of
                none ->
                    case {eval_instr(Succ, G, Bs#{Dst=>True}),
                          eval_instr(Fail, G, Bs#{Dst=>False})} of
                        {Same,Same} -> Same;
                        {_,_} -> Vtx
                    end;
                true ->
                    eval_instr(Succ, G, Bs#{Dst=>True});
                false ->
                    eval_instr(Fail, G, Bs#{Dst=>False})
            end;
        {br,_} ->
            Vtx
    end.

eval_bif(Bif, Args0) ->
    case eval_literal_args(Args0, []) of
        none ->
            none;
        Args ->
            %% We have already made sure that this expression can't
            %% fail; thus there is no need for a `try`.
            apply(erlang, Bif, Args)
    end.

eval_literal_args([#b_literal{val=Val}|As], Acc) ->
    eval_literal_args(As, [Val|Acc]);
eval_literal_args([_|_], _) ->
    none;
eval_literal_args([], Acc) ->
    reverse(Acc).

%%%
%%% Check that variables are initialized on all paths and abort
%%% the optimization if not.
%%%
%%% Expressions that use `or` and `not` may have added
%%% `bif:is_boolean` instructions at the end of the boolean
%%% expression. It can happen that the variables tested by
%%% `bif:is_boolean` are not initialized on all paths.
%%%

ensure_init(Root, G, G0) ->
    Vs = dg_vertices(G),

    %% Build an ordset of a all variables used by the code
    %% before the optimization.
    Used = ensure_init_used(G0),

    %% Build a map of all variables that are set by instructions in
    %% the digraph. Variables not included in this map have been
    %% defined by code before the code in the digraph.
    Vars = maps:from_list([{Dst,unset} ||
                              {_,#b_set{dst=Dst}} <- Vs]),
    RPO = dg_reverse_postorder(G, [Root]),
    ensure_init_1(RPO, Used, G, #{Root=>Vars}).

ensure_init_1([V|Vs], Used, G, InitMaps0) ->
    InitMaps = ensure_init_instr(V, Used, G, InitMaps0),
    ensure_init_1(Vs, Used, G, InitMaps);
ensure_init_1([], _, _, _) -> ok.

ensure_init_instr(Vtx, Used, G, InitMaps0) ->
    VarMap0 = map_get(Vtx, InitMaps0),
    case dg_vertex(G, Vtx) of
        #b_set{dst=Dst}=I ->
            do_ensure_init_instr(I, VarMap0, InitMaps0),
            OutVs = dg_out_neighbours(G, Vtx),
            VarMap = VarMap0#{Dst=>set},
            InitMaps = InitMaps0#{Vtx:=VarMap},
            ensure_init_successors(OutVs, G, VarMap, InitMaps);
        {external,_} ->
            %% We have reached the success or failure node.
            %% If the code we have been optimizing does not
            %% originate from a guard, it is possible that a
            %% variable set in the optimized code will be used
            %% here.
            case [V || {V,unset} <- maps:to_list(VarMap0)] of
                [] ->
                    InitMaps0;
                [_|_]=Unset0 ->
                    %% There are some variables that are not always
                    %% set when this node is reached. We must make
                    %% sure that they are not used at this node or
                    %% one of its successors.
                    Unset = ordsets:from_list(Unset0),
                    case ordsets:is_subset(Unset, Used) of
                        true ->
                            %% Note that all of the potentially unset
                            %% variables are only used once (otherwise
                            %% the optimization would have been
                            %% aborted earlier). Therefore, since all
                            %% variables are used in the optimized code,
                            %% they cannot be used in this node or in one
                            %% of its successors.
                            InitMaps0;
                        false ->
                            %% The original code probably did not
                            %% originate from a guard. One of the
                            %% potentially unset variables are not
                            %% used in the optimized code. That means
                            %% that it must be used at this node or in
                            %% one of its successors. (Or that it was
                            %% not used at all in the original code,
                            %% but that basically only happens in test
                            %% cases.)
                            not_possible()
                    end
            end;
        _ ->
            OutVs = dg_out_neighbours(G, Vtx),
            ensure_init_successors(OutVs, G, VarMap0, InitMaps0)
    end.

ensure_init_used(G) ->
    Vs = dg_vertices(G),
    ensure_init_used_1(Vs, G, []).

ensure_init_used_1([{Vtx,#b_set{dst=Dst}=I}|Vs], G, Acc0) ->
    Acc1 = [beam_ssa:used(I)|Acc0],
    case dg_out_degree(G, Vtx) of
        2 ->
            Acc = [[Dst]|Acc1],
            ensure_init_used_1(Vs, G, Acc);
        _ ->
            ensure_init_used_1(Vs, G, Acc1)
    end;
ensure_init_used_1([{_Vtx,{br,Bool}}|Vs], G, Acc) ->
    ensure_init_used_1(Vs, G, [[Bool]|Acc]);
ensure_init_used_1([_|Vs], G, Acc) ->
    ensure_init_used_1(Vs, G, Acc);
ensure_init_used_1([], _G, Acc) ->
    ordsets:union(Acc).

do_ensure_init_instr(#b_set{op=phi,args=Args},
                     _VarMap, InitMaps) ->
    _ = [ensure_init_used(Var, map_get(From, InitMaps)) ||
            {#b_var{}=Var,From} <- Args],
    ok;
do_ensure_init_instr(#b_set{}=I, VarMap, _InitMaps) ->
    Used = beam_ssa:used(I),
    _ = [ensure_init_used(Var, VarMap) || Var <- Used],
    ok.

ensure_init_used(Var, VarMap) ->
    case VarMap of
        #{Var:=unset} -> not_possible();
        #{Var:=set} -> ok;
        #{} -> ok
    end.

ensure_init_successors([To|Vs], G, Vars0, InitMaps0) ->
    case InitMaps0 of
        #{To:=Vars1} ->
            Vars = join_inits(Vars0, Vars1),
            InitMaps = InitMaps0#{To:=Vars},
            ensure_init_successors(Vs, G, Vars0, InitMaps);
        #{} ->
            InitMaps = InitMaps0#{To=>Vars0},
            ensure_init_successors(Vs, G, Vars0, InitMaps)
    end;
ensure_init_successors([], _, _, InitMaps) ->
    InitMaps.

join_inits(VarMap0, VarMap1) ->
    join_inits_1(maps:to_list(VarMap0), VarMap1).

join_inits_1([{V,State0}|Vs], VarMap) ->
    State1 = map_get(V, VarMap),
    State = case {State0,State1} of
                {set,set} -> set;
                {_,_} -> unset
            end,
    case State =:= State1 of
        true ->
            join_inits_1(Vs, VarMap);
        false ->
            join_inits_1(Vs, VarMap#{V:=State})
    end;
join_inits_1([], VarMap) ->
    VarMap.

%%%
%%% Transform the digraph back to standard SSA code.
%%%

digraph_to_ssa(Ls, G, Blocks0) ->
    Seen = cerl_sets:new(),
    {Blocks,_} = digraph_to_ssa(Ls, G, Blocks0, Seen),
    Blocks.

digraph_to_ssa([L|Ls], G, Blocks0, Seen0) ->
    Seen1 = cerl_sets:add_element(L, Seen0),
    {Blk,Successors0} = digraph_to_ssa_blk(L, G, Blocks0, []),
    Blocks1 = Blocks0#{L=>Blk},
    Successors = [S || S <- Successors0,
                       not cerl_sets:is_element(S, Seen1)],
    {Blocks,Seen} = digraph_to_ssa(Successors, G, Blocks1, Seen1),
    digraph_to_ssa(Ls, G, Blocks, Seen);
digraph_to_ssa([], _G, Blocks, Seen) ->
    {Blocks,Seen}.

digraph_to_ssa_blk(From, G, Blocks, Acc) ->
    case dg_vertex(G, From) of
        #b_set{dst=Dst}=I ->
            case get_targets(From, G) of
                {br,Succ,Fail} ->
                    %% This is a two-way branch that ends the current block.
                    Br = #b_br{bool=Dst,succ=Succ,fail=Fail},
                    Is = reverse(Acc, [I]),
                    Blk = #b_blk{is=Is,last=Br},
                    {Blk,beam_ssa:successors(Blk)};
                {br,Next} ->
                    case dg_in_degree(G, Next) of
                        1 ->
                            digraph_to_ssa_blk(Next, G, Blocks, [I|Acc]);
                        _ ->
                            %% The Next node has multiple incident edge. That
                            %% means that it can't be part of the current block,
                            %% but must start a new block.
                            Br = oneway_br(Next),
                            Is = reverse(Acc, [I]),
                            Blk = #b_blk{is=Is,last=Br},
                            {Blk,beam_ssa:successors(Blk)}
                    end
            end;
        br ->
            case Acc of
                [] ->
                    %% Create an empty block.
                    {br,Next} = get_targets(From, G),
                    Blk = #b_blk{is=[],last=oneway_br(Next)},
                    {Blk,beam_ssa:successors(Blk)};
                [_|_] ->
                    %% Finish up the block, and let the block
                    %% transfer control to the `br` node at From.
                    Br = oneway_br(From),
                    Is = reverse(Acc),
                    Blk = #b_blk{is=Is,last=Br},
                    {Blk,beam_ssa:successors(Blk)}
            end;
        {br,Bool} ->
            %% This is a two-way `br` instruction. The most common
            %% reason for its existence in the graph is that the root
            %% node only contained a phi instruction (which was taken
            %% out of the block before building the graph).
            [] = Acc,                           %Assertion.
            {br,Succ,Fail} = get_targets(From, G),
            Br = #b_br{bool=Bool,succ=Succ,fail=Fail},
            Blk = #b_blk{is=[],last=Br},
            {Blk,beam_ssa:successors(Blk)};
        {external,Sub} ->
            #b_blk{is=Is0} = Blk = map_get(From, Blocks),
            Is = [I#b_set{args=sub_args(Args0, Sub)} ||
                     #b_set{args=Args0}=I <- Is0],
            {Blk#b_blk{is=Is},[]}
    end.

%%%
%%% Helper functions follow.
%%%

%% get_def(Var, #st{}) -> #b_set{} | none.
%%  Find the definition for a variable. Only boolean
%%  expressions and phi nodes can be found.

get_def(#b_var{}=Bool, #st{defs=Defs}) ->
    case Defs of
        #{Bool:={_,Def}} ->
            Def;
        #{} ->
            none
    end.

%% get_def(Var, Graph, #st{}) -> #b_set{} | none.
%%  Find the definition for a variable, looking first in the digraph
%%  Graph. If it is not found there, look in the global map of
%%  interesting definitions from the entire functions.

get_def(Var, G, #st{ldefs=LDefs,defs=Defs}) ->
    case LDefs of
        #{Var:=Vtx} ->
            dg_vertex(G, Vtx);
        #{} ->
            %% Not in the graph. Returning definitions for phi nodes
            %% outside the graph is useful for shortcut_branches().
            case Defs of
                #{Var:={_,Def}} -> Def;
                #{} -> none
            end
    end.

add_succ_fail_edges(From, Succ, Fail, G0) ->
    G1 = dg_add_edge(G0, From, Succ, succ),
    G = dg_add_edge(G1, From, Fail, fail),
    case dg_out_edges(G0, From) of
        [{From,_,next}=E] -> dg_del_edge(G, E);
        [] -> G
    end.

get_vertex(#b_set{dst=Dst}, St) ->
    get_vertex(Dst, St);
get_vertex(#b_var{}=Var, #st{ldefs=LDefs}) ->
    map_get(Var, LDefs).

get_targets(Vtx, G) when is_integer(Vtx) ->
    case dg_out_edges(G, Vtx) of
        [{_,To,next}] ->
            {br,To};
        [{_,Succ,succ},{_,Fail,fail}] ->
            {br,Succ,Fail};
        [{_,Fail,fail},{_,Succ,succ}] ->
            {br,Succ,Fail};
        [] ->
            none
    end.

get_targets(#b_var{}=Var, G, #st{ldefs=LDefs}) ->
    get_targets(map_get(Var, LDefs), G).

del_out_edges(V, G) ->
    dg_del_edges(G, dg_out_edges(G, V)).

covered(From, To, G) ->
    Seen0 = gb_sets:empty(),
    {yes,Seen} = covered_1(From, To, G, Seen0),
    gb_sets:to_list(Seen).

covered_1(To, To, _G, Seen) ->
    {yes,Seen};
covered_1(From, To, G, Seen0) ->
    Vs0 = dg_out_neighbours(G, From),
    Vs = [V || V <- Vs0, not gb_sets:is_member(V, Seen0)],
    Seen = gb_sets:union(gb_sets:from_list(Vs), Seen0),
    case Vs of
        [] ->
            no;
        [_|_] ->
            covered_list(Vs, To, G, Seen, false)
    end.

covered_list([V|Vs], To, G, Seen0, AnyFound) ->
    case covered_1(V, To, G, Seen0) of
        {yes,Seen} ->
            covered_list(Vs, To, G, Seen, true);
        no ->
            covered_list(Vs, To, G, Seen0, AnyFound)
    end;
covered_list([], _, _, Seen, AnyFound) ->
    case AnyFound of
        true -> {yes,Seen};
        false -> no
    end.

digraph_roots(G) ->
    digraph_roots_1(dg_vertices(G), G).

digraph_roots_1([{V,_}|Vs], G) ->
    case dg_in_degree(G, V) of
        0 ->
            [V|digraph_roots_1(Vs, G)];
        _ ->
            digraph_roots_1(Vs, G)
    end;
digraph_roots_1([], _G) -> [].

not_possible() ->
    throw(not_possible).

new_label(#st{count=Count}=St) ->
    {Count,St#st{count=Count+1}}.

sub_args(Args, Sub) ->
    [sub_arg(Arg, Sub) || Arg <- Args].

sub_arg({#b_var{}=Arg,From}, Sub) when is_integer(From) ->
    {do_sub_arg(Arg, Sub),From};
sub_arg(#b_var{}=Arg, Sub) ->
    do_sub_arg(Arg, Sub);
sub_arg(#b_remote{mod=Mod,name=Name}=Rem, Sub) ->
    Rem#b_remote{mod=do_sub_arg(Mod, Sub),
                 name=do_sub_arg(Name, Sub)};
sub_arg(Arg, _Sub) -> Arg.

do_sub_arg(#b_var{}=Old, Sub) ->
    case Sub of
        #{Old:=#b_literal{}=New} -> New;
        #{Old:=#b_var{}=New} -> New;
        #{} -> Old
    end;
do_sub_arg(#b_literal{}=Old, _Sub) -> Old.

is_bool_expr(#b_set{op={bif,Op},args=Args}) ->
    Arity = length(Args),
    erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity) orelse
	erl_internal:bool_op(Op, Arity);
is_bool_expr(_) -> false.

%% Test whether the expression always succeeds and
%% always returns a boolean.
is_safe_bool_expr(#b_set{op={bif,Op},args=Args}) ->
    Arity = length(Args),
    erl_internal:comp_op(Op, Arity) orelse
	erl_internal:new_type_test(Op, Arity);
is_safe_bool_expr(#b_set{}) -> false.

oneway_br(To) ->
    #b_br{bool=#b_literal{val=true},succ=To,fail=To}.

%%%
%%% Digraph data type. Similar to the digraph module, but provides a
%%% functional API. The functional API allows us to revert to a
%%% previous version of the digraph when an optimization that may have
%%% damaged the digraph has failed.
%%%

-record(dg, {vs,
             in_es,
             out_es}).

dg_new() ->
    Vs = #{},
    EsMap = #{},
    #dg{vs=Vs,in_es=EsMap,out_es=EsMap}.

dg_add_vertex(Dg, V, Label) ->
    #dg{in_es=InEsMap0,out_es=OutEsMap0,vs=Vs0} = Dg,
    InEsMap = dg__init_edge_map(V, InEsMap0),
    OutEsMap = dg__init_edge_map(V, OutEsMap0),
    Vs = Vs0#{V=>Label},
    Dg#dg{vs=Vs,in_es=InEsMap,out_es=OutEsMap}.

dg_add_edge(Dg, From, To, Label) ->
    #dg{in_es=InEsMap0,out_es=OutEsMap0} = Dg,
    Name = {From,To,Label},
    InEsMap = dg__edge_map_add(To, Name, InEsMap0),
    OutEsMap = dg__edge_map_add(From, Name, OutEsMap0),
    Dg#dg{in_es=InEsMap,out_es=OutEsMap}.

dg_out_degree(#dg{out_es=OutEsMap}, V) ->
    length(map_get(V, OutEsMap)).

dg_out_edges(#dg{out_es=OutEsMap}, V) ->
    map_get(V, OutEsMap).

dg_out_neighbours(#dg{out_es=OutEsMap}, V) ->
    [To || {_,To,_} <- map_get(V, OutEsMap)].

dg_del_edge(Dg, {From,To,_}=E) ->
    #dg{in_es=InEsMap0,out_es=OutEsMap0} = Dg,
    InEsMap = dg__edge_map_del(To, E, InEsMap0),
    OutEsMap = dg__edge_map_del(From, E, OutEsMap0),
    Dg#dg{in_es=InEsMap,out_es=OutEsMap}.

dg_del_edges(G, Es) when is_list(Es) ->
    foldl(fun(E, A) -> dg_del_edge(A, E) end, G, Es).

dg_in_degree(#dg{in_es=InEsMap}, V) ->
    length(map_get(V, InEsMap)).

dg_vertex(#dg{vs=Vs}, V) ->
    map_get(V, Vs).

dg_vertices(#dg{vs=Vs}) ->
    maps:to_list(Vs).

dg_is_path(G, From, To) ->
    Seen = cerl_sets:new(),
    try
        _ = dg__is_path([From], To, G, Seen),
        false
    catch
        throw:true ->
            true
    end.

dg_reverse_postorder(G, Vs) ->
    Seen = cerl_sets:new(),
    {RPO,_} = dg__rpo(Vs, G, Seen, []),
    RPO.

dg__rpo([V|Vs], G, Seen0, Acc0) ->
    case cerl_sets:is_element(V, Seen0) of
        true ->
            dg__rpo(Vs, G, Seen0, Acc0);
        false ->
            Seen1 = cerl_sets:add_element(V, Seen0),
            Successors = dg_out_neighbours(G, V),
            {Acc,Seen} = dg__rpo(Successors, G, Seen1, Acc0),
            dg__rpo(Vs, G, Seen, [V|Acc])
    end;
dg__rpo([], _, Seen, Acc) ->
    {Acc,Seen}.

dg__is_path([To|_], To, _G, _Seen) ->
    throw(true);
dg__is_path([V|Vs], To, G, Seen0) ->
    case cerl_sets:is_element(V, Seen0) of
        true ->
            dg__is_path(Vs, To, G, Seen0);
        false ->
            Seen1 = cerl_sets:add_element(V, Seen0),
            Successors = dg_out_neighbours(G, V),
            Seen = dg__is_path(Successors, To, G, Seen1),
            dg__is_path(Vs, To, G, Seen)
    end;
dg__is_path([], _To, _G, Seen) ->
    Seen.

dg__init_edge_map(V, EsMap) ->
    case is_map_key(V, EsMap) of
        true ->
            EsMap;
        false ->
            EsMap#{V=>ordsets:new()}
    end.

dg__edge_map_add(V, E, EsMap) ->
    Es0 = map_get(V, EsMap),
    Es = ordsets:add_element(E, Es0),
    EsMap#{V:=Es}.

dg__edge_map_del(V, E, EsMap) ->
    Es0 = map_get(V, EsMap),
    Es = Es0 -- [E],
    EsMap#{V:=Es}.

-if(?DEBUG).

-spec dump(any()) -> any().
dump(G) ->
    dump_topsort(topsort(G), G),
    io:nl(),
    G.

topsort(G) ->
    Roots = digraph_roots(G),
    dg_reverse_postorder(G, Roots).

dump_topsort([V|Vs], G) ->
    Info = dg_vertex(G, V),
    Out = dg_out_neighbours(G, V),
    io:format("~p ~p ~w\n", [V,Info,Out]),
    dump_topsort(Vs, G);
dump_topsort([], _G) -> ok.

-endif.
