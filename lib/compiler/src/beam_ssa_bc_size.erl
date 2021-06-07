%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
%% Try to calculate the exact size of bitstring produced by a binary
%% comprehension such as:
%%
%%    << <<X:16>> || <<X:4>> <= Bs >>
%%
%% For this example, the size of the resulting binary (rounded up to
%% the nearest number of bytes) will be:
%%
%%    ((bit_size(Bs) div 4) * 16 + 7) div 8
%%
%% If the exact size can't be determined, such as for this comprehension:
%%
%%    << <<X:16>> || <<X:4>> <= Bs, X =/= 15 >>
%%
%% the default size of 256 bytes will be used as starting size of
%% the writable binary.
%%

-module(beam_ssa_bc_size).
-export([opt/1]).

-import(lists, [any/2,member/2,reverse/1,sort/1]).

-include("beam_ssa_opt.hrl").

-spec opt(st_map()) -> st_map().

opt(StMap) ->
    opt(maps:keys(StMap), StMap).

opt([Id|Ids], StMap0) ->
    StMap = opt_function(Id, StMap0),
    opt(Ids, StMap);
opt([], StMap) -> StMap.

opt_function(Id, StMap) ->
    #opt_st{anno=Anno,ssa=Linear0,cnt=Count0} = OptSt0 = map_get(Id, StMap),
    ParamInfo = maps:get(parameter_info, Anno, #{}),
    try opt_blks(Linear0, ParamInfo, StMap, unchanged, Count0, []) of
        {Linear,Count} ->
            OptSt = OptSt0#opt_st{ssa=Linear,cnt=Count},
            StMap#{Id := OptSt};
        none ->
            StMap
    catch
        Class:Error:Stack ->
            #b_local{name=#b_literal{val=Name},arity=Arity} = Id,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

opt_blks([{L,#b_blk{is=Is}=Blk}|Blks], ParamInfo, StMap, AnyChange, Count0, Acc0) ->
    case Is of
        [#b_set{op=bs_init_writable,dst=Dst}] ->
            Bs = #{st_map => StMap, Dst => {writable,#b_literal{val=0}}},
            try opt_writable(Bs, L, Blk, Blks, ParamInfo, Count0, Acc0) of
                {Acc,Count} ->
                    opt_blks(Blks, ParamInfo, StMap, changed, Count, Acc)
            catch
                throw:not_possible ->
                    opt_blks(Blks, ParamInfo, StMap, AnyChange, Count0, [{L,Blk}|Acc0])
            end;
        _ ->
            opt_blks(Blks, ParamInfo, StMap, AnyChange, Count0, [{L,Blk}|Acc0])
    end;
opt_blks([], _ParamInfo, _StMap, changed, Count, Acc) ->
    {reverse(Acc),Count};
opt_blks([], _ParamInfo, _StMap, unchanged, _Count, _Acc) ->
    none.

opt_writable(Bs0, L, Blk, Blks, ParamInfo, Count0, Acc0) ->
    case {Blk,Blks} of
        {#b_blk{last=#b_br{succ=Next,fail=Next}},
         [{Next,#b_blk{is=[#b_set{op=call,args=[_|Args],dst=Dst}=Call|_],
                       last=CallLast}}|_]} ->
            ensure_not_match_context(Call, ParamInfo),

            ArgTypes = maps:from_list([{Arg,{arg,Arg}} || Arg <- Args]),
            Bs = maps:merge(ArgTypes, Bs0),
            Result = map_get(Dst, call_size_func(Call, Bs)),
            {Expr,Annos} = make_expr_tree(Result),

            %% Note that we pass the generator call's terminator: should we
            %% need to raise a `bad_generator` exception, it needs to fail in
            %% the same manner as the generator itself.
            cg_size_calc(Expr, L, Blk, CallLast, Annos, Count0, Acc0);
        {_,_} ->
            throw(not_possible)
    end.

ensure_not_match_context(#b_set{anno=Anno,args=[_|Args]}, ParamInfo) ->
    case maps:get(bsm_info, Anno, []) of
        context_reused ->
            %% The generator is a match context. The optimization is
            %% not safe. Example:
            %%
            %%     f(<<B/binary>>) ->
            %%          << <<V>> || <<V>> <= B >>.
            throw(not_possible);
        _ ->
            case any(fun(V) ->
                             member(accepts_match_context,
                                    maps:get(V, ParamInfo, []))
                     end, Args) of
                true ->
                    %% Match context is passed from the calling function. Example:
                    %%    f0(<<B/binary>>) -> f1(B).
                    %%    f1(B) -> << <<V>> || <<V>> <= B >>.
                    throw(not_possible);
                false ->
                    ok
            end
    end.

%%%
%%% Traverse the SSA code of the binary comprehension functions to
%%% figure out the exact size for the writable binary. This algorithm
%%% is similar to how types are determined by beam_ssa_type, but here
%%% we only care about how many bits are matched of from the generators
%%% and how many bits are appended to the writable binary.
%%%

call_size_func(#b_set{anno=Anno,op=call,args=[Name|Args],dst=Dst}, Bs) ->
    StMap = map_get(st_map, Bs),
    case StMap of
        #{Name := #opt_st{ssa=Linear,args=Params}} ->
            NewBs0 = setup_call_bs(Params, Args, Bs, #{}),
            case any(fun({writable,_}) -> true;
                        (_) -> false
                     end, maps:values(NewBs0)) of
                false ->
                    %% Since the writable binary is not passed to the called function,
                    %% it can't have any effect on the size of the writable binary
                    %% and there is no need to analyze it.
                    Bs#{Dst => any};
                true ->
                    NewBs = NewBs0#{Name => self, st_map => StMap},
                    Map0 = #{0 => NewBs},
                    Result = calc_size(Linear, Map0),
                    Bs#{Dst => Result}
            end;
        #{} ->
            case Name of
                #b_remote{mod=#b_literal{val=erlang},
                          name=#b_literal{val=error},
                          arity=1} ->
                    capture_anno(Anno, Dst, Args, Bs#{Dst => exception});
                _ ->
                    Bs#{Dst => any}
            end
    end.

capture_anno(Anno, Dst, [ErrorTerm], Bs) ->
    case get_value(ErrorTerm, Bs) of
        {tuple,Elements} ->
            Ts = [get_value(E, Bs) || E <- Elements],
            capture_anno_1(Anno, Dst, Ts, Bs);
        _ ->
            Bs
    end.

capture_anno_1(Anno, Dst, [{nil_or_bad,Generator}|_], Bs) ->
    Bs#{Dst => {generator_anno,{Generator,Anno}}};
capture_anno_1(Anno, Dst, [{arg,Generator}|_], Bs) ->
    Bs#{Dst => {generator_anno,{Generator,Anno}}};
capture_anno_1(Anno, Dst, [_|T], Bs) ->
    capture_anno_1(Anno, Dst, T, Bs);
capture_anno_1(_, _, [], Bs) ->
    Bs.

setup_call_bs([V|Vs], [A0|As], OldBs, NewBs) ->
    A = case get_value(A0, OldBs) of
            #b_literal{}=Lit -> {arg,Lit};
            {writable,#b_literal{val=0}}=Wr -> Wr;
            {arg,_}=Arg -> Arg;
            _ -> any
        end,
    setup_call_bs(Vs, As, OldBs, NewBs#{V => A});
setup_call_bs([], [], #{}, NewBs) -> NewBs.

calc_size([{L,#b_blk{is=Is,last=Last}}|Blks], Map0) ->
    case maps:take(L, Map0) of
        {Bs0,Map1} ->
            Bs1 = calc_size_is(Is, Bs0),
            Map2 = update_successors(Last, Bs1, Map1),
            case get_ret(Last, Bs1) of
                none ->
                    calc_size(Blks, Map2);
                Ret ->
                    %% Save information about the function returned.
                    Map = Map2#{L => Ret},
                    calc_size(Blks, Map)
            end;
        error ->
            %% This block is unreachable.
            calc_size(Blks, Map0)
    end;
calc_size([], Map) ->
    case sort(maps:values(Map)) of
        [{call,_}=Call,{generator_anno,GenAnno}] ->
            {Call,GenAnno};
        _ ->
            throw(not_possible)
    end.

get_ret(#b_ret{arg=Arg}, Bs) ->
    case get_value(Arg, Bs) of
        exception ->
            none;
        {writable,#b_literal{val=0}} ->
            none;
        {generator_anno,_}=GenAnno ->
            GenAnno;
        Ret ->
            Ret
    end;
get_ret(_, _) -> none.

update_successors(#b_br{bool=Bool,succ=Succ,fail=Fail}, Bs0, Map0) ->
    case get_value(Bool, Bs0) of
        #b_literal{val=true} ->
            update_successor(Succ, Bs0, Map0);
        {succeeded,Var} ->
            Map = update_successor(Succ, Bs0, Map0),
            update_successor(Fail, maps:remove(Var, Bs0), Map);
        {'if',Var,TrueType,FalseType} ->
            Bs = maps:remove(Bool, Bs0),
            case Var of
                #b_var{} ->
                    Map = update_successor(Succ, Bs#{Var => TrueType}, Map0),
                    update_successor(Fail, Bs#{Var => FalseType}, Map);
                #b_literal{} ->
                    Bs
            end;
        any ->
            Map = update_successor(Succ, Bs0#{Bool := #b_literal{val=true}}, Map0),
            update_successor(Fail, Bs0#{Bool := #b_literal{val=false}}, Map)
    end;
update_successors(#b_switch{}, _Bs, _Map) ->
    %% A switch implies a filter, which means that we cannot calculate the
    %% exact size.
    throw(not_possible);
update_successors(#b_ret{}, _Bs, Map) -> Map.

update_successor(?EXCEPTION_BLOCK, _Bs, Map) ->
    Map;
update_successor(L, Bs, Map) ->
    case Map of
        #{L := OldBs} ->
            Map#{L := join_bs(OldBs, Bs)};
        #{} ->
            Map#{L => Bs}
    end.

calc_size_is([I|Is], Bs0) ->
    Bs = calc_size_instr(I, Bs0),
    calc_size_is(Is, Bs);
calc_size_is([], Bs) -> Bs.

calc_size_instr(#b_set{op=bs_add,args=[A,B,U],dst=Dst}, Bs) ->
    %% We must make sure that the value of bs_add only depends on literals
    %% and arguments passed from the function that created the writable
    %% binary.
    case {get_value(A, Bs),get_arg_value(B, Bs)} of
        {#b_literal{}=Lit,Val} ->
            Bs#{Dst => {expr,{{bif,'+'},[Lit,{{bif,'*'},[Val,U]}]}}};
        {{expr,Expr},Val} ->
            Bs#{Dst => {expr,{{bif,'+'},[Expr,{{bif,'*'},[Val,U]}]}}};
        {_,_} ->
            %% The value depends on a variable of which we know nothing.
            Bs#{Dst => any}
    end;
calc_size_instr(#b_set{op=bs_init,args=[#b_literal{val=private_append},
                                        Writable,Size,Unit],
                       dst=Dst}, Bs) ->
    case get_value(Size, Bs) of
        {arg,SizeOrigin} ->
            Expr = {{bif,'*'},[SizeOrigin,Unit]},
            update_writable(Dst, Writable, Expr, Bs);
        #b_literal{} ->
            Expr = {{bif,'*'},[Size,Unit]},
            update_writable(Dst, Writable, Expr, Bs);
        {expr,Expr} ->
            update_writable(Dst, Writable, Expr, Bs);
        _ ->
            Bs#{Dst => any}
    end;
calc_size_instr(#b_set{op=bs_match,args=[_Type,Ctx,_Flags,
                                         Size,Unit],dst=Dst}, Bs) ->
    case get_arg_value(Size, Bs) of
        none ->
            Bs#{Dst => any};
        Val ->
            update_match(Dst, Ctx, {{safe,{bif,'*'}},[Val,Unit]}, Bs)
    end;
calc_size_instr(#b_set{op=bs_start_match,args=[#b_literal{val=new},Arg],dst=Dst}, Bs) ->
    case get_arg_value(Arg, Bs) of
        none ->
            Bs#{Dst => any};
        Val ->
            Bs#{Dst => {match,{{bif,bit_size},[Val]},#b_literal{val=0}}}
    end;
calc_size_instr(#b_set{op=call,args=[Name|Args],dst=Dst}=I, Bs) ->
    if
        is_map_key(Name, Bs) ->
            Result0 = [get_value(A, Bs) || A <- Args],
            Result = [Val || Val <- Result0, Val =/= any],
            Bs#{Dst => {call,Result}};
        true ->
            call_size_func(I, Bs)
    end;
calc_size_instr(#b_set{op=get_tl,args=[Ctx],dst=Dst}, Bs) ->
    update_match(Dst, Ctx, #b_literal{val=1}, Bs);
calc_size_instr(#b_set{op=is_nonempty_list,args=[Arg],dst=Dst}, Bs) ->
    case get_arg_value(Arg, Bs) of
        none ->
            Bs#{Dst => any};
        Val ->
            NumElements = {{bif,length},[Val]},
            Match = {match,NumElements,#b_literal{val=0}},
            NoMatch = {nil_or_bad,Val},
            Bs#{Dst => {'if',Arg,Match,NoMatch}}
    end;
calc_size_instr(#b_set{op=put_tuple,args=Args,dst=Dst}, Bs) ->
    Bs#{Dst => {tuple,Args}};
calc_size_instr(#b_set{op={succeeded,_},args=[Arg],dst=Dst}, Bs) ->
    Bs#{Dst => {succeeded,Arg}};
calc_size_instr(#b_set{dst=Dst}, Bs) ->
    Bs#{Dst => any}.

update_writable(Dst, Writable, Expr, Bs) ->
    case get_value(Writable, Bs) of
        {writable,#b_literal{val=0}} ->
            Bs#{Dst => {writable,Expr}};
        _ ->
            Bs#{Dst => any}
    end.

update_match(Dst, Ctx, Increment, Bs) ->
    case get_value(Ctx, Bs) of
        {match,NumElements,Offset0} ->
            Offset = {{bif,'+'},[Offset0,Increment]},
            Bs#{Dst => {match,NumElements,Offset}};
        _ ->
            Bs#{Dst => any}
    end.

get_arg_value(#b_literal{}=Lit, _Bs) ->
    Lit;
get_arg_value(Name, Bs) ->
    case Bs of
        #{Name := {arg,Val}} -> Val;
        #{} -> none
    end.

get_value(Name, Bs) ->
    case Bs of
        #{Name := Value} -> Value;
        #{} -> Name
    end.

join_bs(LHS, RHS) ->
    if
        map_size(LHS) < map_size(RHS) ->
            join_bs_1(maps:keys(LHS), RHS, LHS);
        true ->
            join_bs_1(maps:keys(RHS), LHS, RHS)
    end.

%% Joins two maps of bindings, keeping the variables that are common to both maps.
join_bs_1([V | Vs], Bigger, Smaller) ->
    case {Bigger, Smaller} of
        {#{V := Same},#{V := Same}} ->
            join_bs_1(Vs, Bigger, Smaller);
        {#{V := _LHS},#{V := _RHS}} ->
            join_bs_1(Vs, Bigger, Smaller#{V := any});
        {#{}, #{V := _}} ->
            join_bs_1(Vs, Bigger, maps:remove(V, Smaller))
    end;
join_bs_1([], _Bigger, Smaller) -> Smaller.

%%%
%%% Turn the result of the traversal of the SSA code into an expression tree.
%%%

make_expr_tree({{call,Alloc0},GenAnno}) ->
    {Alloc1,Annos} = make_expr_tree_list(Alloc0, none, none, [GenAnno]),
    Alloc2 = opt_expr(Alloc1),
    Alloc = round_up_to_byte_size(Alloc2),
    {Alloc,maps:from_list(Annos)};
make_expr_tree(_) ->
    throw(not_possible).

make_expr_tree_list([{{call,List},GenAnno}|T], Match, none, Annos0) ->
    {BuildSize,Annos} = make_expr_tree_list(List, none, none, [GenAnno|Annos0]),
    make_expr_tree_list(T, Match, BuildSize, Annos);
make_expr_tree_list([{match,NumItems,N}|T], none, BuildSize, Annos) ->
    make_expr_tree_list(T, {NumItems,N}, BuildSize, Annos);
make_expr_tree_list([{writable,BuildSize}|T], Match, none, Annos) ->
    make_expr_tree_list(T, Match, BuildSize, Annos);
make_expr_tree_list([_|T], Match, BuildSize, Annos) ->
    make_expr_tree_list(T, Match, BuildSize, Annos);
make_expr_tree_list([], Match, BuildSize, Annos)
  when Match =/= none, BuildSize =/= none ->
    {NumItems,N} = Match,
    Expr = {{bif,'*'},[{{safe,{bif,'div'}},[NumItems,N]},BuildSize]},
    {Expr,Annos};
make_expr_tree_list([], _, _, Annos) ->
    {none,Annos}.

round_up_to_byte_size(Alloc0) ->
    Alloc = case divisible_by_eight(Alloc0) of
                true -> Alloc0;
                false -> {{bif,'+'},[Alloc0,#b_literal{val=7}]}
            end,
    opt_expr({{bif,'div'},[Alloc,#b_literal{val=8}]}).

divisible_by_eight({{bif,'*'},[Expr1,Expr2]}) ->
    divisible_by_eight(Expr1) orelse divisible_by_eight(Expr2);
divisible_by_eight(#b_literal{val=Val}) when Val rem 8 =:= 0 ->
    true;
divisible_by_eight(_) -> false.

%%%
%%% Optimize an expression tree.
%%%

opt_expr({Op,Args0}) ->
    Args = opt_expr_args(Args0),
    case literal_expr_args(Args, []) of
        none ->
            opt_expr_1(Op, Args);
        LitArgs ->
            Bif = case Op of
                      {safe,{bif,Bif0}} -> Bif0;
                      {bif,Bif0} -> Bif0
                  end,
            try apply(erlang, Bif, LitArgs) of
                Result ->
                    #b_literal{val=Result}
            catch
                error:_ ->
                    opt_expr_1(Op, Args)
            end
    end;
opt_expr(none) -> none.

opt_expr_1({safe,{bif,'div'}}=Op, Args) ->
    case Args of
        [Int,#b_literal{val=1}] ->
            Int;
        [_Int,#b_literal{val=N}] when N > 1 ->
            opt_expr_1({bif,'div'}, Args);
        [_,_] ->
            {Op,Args}
    end;
opt_expr_1({bif,'div'}=Op, [Numerator,#b_literal{val=Denominator}]=Args) ->
    try
        opt_expr_div(Numerator, Denominator)
    catch
        throw:not_possible ->
            try Denominator band (Denominator - 1) of
                0 ->
                    %% The denominator is a power of two.
                    Shift = round(math:log2(Denominator)),
                    {{bif,'bsr'},[Numerator,#b_literal{val=Shift}]};
                _ ->
                    {Op,Args}
            catch
                _:_ ->
                    {Op,Args}
            end
    end;
opt_expr_1({bif,'*'}, [{{safe,_},_},#b_literal{val=0}=Zero]) ->
    Zero;
opt_expr_1({bif,'*'}, [Factor,#b_literal{val=1}]) ->
    Factor;
opt_expr_1(Op, Args) ->
    {Op,Args}.

opt_expr_div({{bif,'*'},[A,B]}, Denominator) ->
    case B of
        #b_literal{val=Factor} when Factor rem Denominator =:= 0 ->
            {{bif,'*'},[A,#b_literal{val=Factor div Denominator}]};
        _ ->
            {{bif,'*'},[A,opt_expr_div(B, Denominator)]}
    end;
opt_expr_div(_, _) ->
    throw(not_possible).

opt_expr_args([A0|As]) ->
    A = case A0 of
            #b_literal{} -> A0;
            #b_var{} -> A0;
            _ -> opt_expr(A0)
        end,
    [A|opt_expr_args(As)];
opt_expr_args([]) -> [].

literal_expr_args([#b_literal{val=Val}|As], Acc) ->
    literal_expr_args(As, [Val|Acc]);
literal_expr_args([_|_], _) ->
    none;
literal_expr_args([], Acc) ->
    reverse(Acc).

%%%
%%% Given an expression tree, generate SSA code to calculate the number
%%% bytes to allocate for the writable binary.
%%%

cg_size_calc(Expr, L, #b_blk{is=Is0}=Blk0, CallLast, Annos, Count0, Acc0) ->
    [InitWr] = Is0,
    FailBlk0 = [],
    {Acc1,Alloc,NextBlk,FailBlk,Count} = cg_size_calc_1(L, Expr, Annos,
                                                        CallLast, FailBlk0,
                                                        Count0, Acc0),
    Is = [InitWr#b_set{args=[Alloc]}],
    Blk = Blk0#b_blk{is=Is},
    Acc = [{NextBlk,Blk}|FailBlk++Acc1],
    {Acc,Count}.

cg_size_calc_1(L, #b_literal{}=Alloc, _Annos, _CallLast, FailBlk, Count, Acc) ->
    {Acc,Alloc,L,FailBlk,Count};
cg_size_calc_1(L0, {Op0,Args0}, Annos, CallLast, FailBlk0, Count0, Acc0) ->
    {Args,Acc1,L,FailBlk1,Count1} = cg_atomic_args(Args0, L0, Annos, CallLast,
                                                   FailBlk0, Count0, Acc0, []),
    {BadGenL,FailBlk,Count2} = cg_bad_generator(Args, Annos, CallLast,
                                                FailBlk1, Count1),
    {Dst,Count3} = new_var('@ssa_tmp', Count2),
    case Op0 of
        {safe,Op} ->
            {OpDst,Count4} = new_var('@ssa_size', Count3),
            {[OpSuccL,OpFailL,PhiL,NextL],Count5} = new_blocks(4, Count4),
            I = #b_set{op=Op,args=Args,dst=OpDst},
            {Blk,Count} = cg_succeeded(I, OpSuccL, OpFailL, Count5),
            JumpBlk = #b_blk{is=[],last=cg_br(PhiL)},
            PhiIs = [#b_set{op=phi,
                            args=[{OpDst,OpSuccL},{#b_literal{val=0},OpFailL}],
                            dst=Dst}],
            PhiBlk = #b_blk{is=PhiIs,last=cg_br(NextL)},
            Acc = [{PhiL,PhiBlk},{OpSuccL,JumpBlk},
                   {OpFailL,JumpBlk},{L,Blk}|Acc1],
            {Acc,Dst,NextL,FailBlk,Count};
        _ ->
            {NextBlkL,Count4} = new_block(Count3),
            I = #b_set{op=Op0,args=Args,dst=Dst},
            {SuccBlk,Count} = cg_succeeded(I, NextBlkL, BadGenL, Count4),
            Acc = [{L,SuccBlk}|Acc1],
            {Acc,Dst,NextBlkL,FailBlk,Count}
    end.

cg_bad_generator([Arg|_], Annos, CallLast, FailBlk, Count) ->
    case Annos of
        #{Arg := Anno} ->
            cg_bad_generator_1(Anno, Arg, CallLast, FailBlk, Count);
        #{} ->
            case FailBlk of
                [{L,_}|_] ->
                    {L,FailBlk,Count};
                [] ->
                    cg_bad_generator_1(#{}, Arg, CallLast, FailBlk, Count)
            end
    end.

cg_bad_generator_1(Anno, Arg, CallLast, FailBlk, Count0) ->
    {L,Count1} = new_block(Count0),
    {TupleDst,Count2} = new_var('@ssa_tuple', Count1),
    {Ret,Count3} = new_var('@ssa_ret', Count2),
    MFA = #b_remote{mod=#b_literal{val=erlang},
                    name=#b_literal{val=error},
                    arity=1},
    TupleI = #b_set{op=put_tuple,
                    args=[#b_literal{val=bad_generator},Arg],
                    dst=TupleDst},
    CallI = #b_set{anno=Anno,op=call,args=[MFA,TupleDst],dst=Ret},
    Is = [TupleI,CallI],

    %% When the generator is called within try/catch, the `bad_generator` call
    %% must refer to the same landing pad or else we'll break optimizations
    %% that assume exceptions are always reflected in the control flow.
    Last = case CallLast of
               #b_br{fail=CatchLbl} when CatchLbl =/= ?EXCEPTION_BLOCK ->
                   #b_br{bool=#b_literal{val=true},
                         succ=CatchLbl,fail=CatchLbl};
               _ ->
                   #b_ret{arg=Ret}
           end,

    Blk = #b_blk{is=Is,last=Last},
    {L,[{L,Blk}|FailBlk],Count3}.

cg_succeeded(#b_set{dst=OpDst}=I, Succ, Fail, Count0) ->
    {Bool,Count} = new_var('@ssa_bool', Count0),
    SuccI = #b_set{op={succeeded,guard},args=[OpDst],dst=Bool},
    Blk = #b_blk{is=[I,SuccI],last=#b_br{bool=Bool,succ=Succ,fail=Fail}},
    {Blk,Count}.

cg_br(Target) ->
    #b_br{bool=#b_literal{val=true},succ=Target,fail=Target}.

cg_atomic_args([A|As], L, Annos, CallLast, FailBlk0, Count0, BlkAcc0, Acc) ->
    case A of
        #b_literal{} ->
            cg_atomic_args(As, L, Annos, CallLast, FailBlk0,
                           Count0, BlkAcc0, [A|Acc]);
        #b_var{} ->
            cg_atomic_args(As, L, Annos, CallLast, FailBlk0,
                           Count0, BlkAcc0, [A|Acc]);
        none ->
            throw(not_possible);
        _ ->
            {BlkAcc,Var,NextBlk,FailBlk,Count} =
                cg_size_calc_1(L, A, Annos, CallLast, FailBlk0,
                               Count0, BlkAcc0),
            cg_atomic_args(As, NextBlk, Annos, CallLast, FailBlk,
                               Count, BlkAcc, [Var|Acc])
    end;
cg_atomic_args([], NextBlk, _Annos, _CallLast, FailBlk, Count, BlkAcc, Acc) ->
    {reverse(Acc),BlkAcc,NextBlk,FailBlk,Count}.

new_var(Base, Count) ->
    {#b_var{name={Base,Count}},Count+1}.

new_blocks(N, Count) ->
    new_blocks(N, Count, []).

new_blocks(0, Count, Acc) ->
    {Acc,Count};
new_blocks(N, Count, Acc) ->
    new_blocks(N - 1, Count + 1, [Count|Acc]).

new_block(Count) ->
    {Count,Count+1}.
