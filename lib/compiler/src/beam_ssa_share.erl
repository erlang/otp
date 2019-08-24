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

%%
%% Share code for semantically equivalent blocks referred to
%% to by `br` and `switch` instructions.
%%
%% A similar optimization is done in beam_jump, but doing it here as
%% well is beneficial as it may enable other optimizations. If there
%% are many semantically equivalent clauses, this optimization can
%% substanstially decrease compilation times.
%%
%% block/2 is called from the liveness optimization pass in
%% beam_ssa_opt, as code sharing helps the liveness pass and vice
%% versa.
%%

-module(beam_ssa_share).
-export([module/2,block/2]).

-include("beam_ssa.hrl").

-import(lists, [keyfind/3,reverse/1,sort/1]).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,Module#b_module{body=Fs}}.

-spec block(Blk0, Blocks0) -> Blk when
      Blk0 :: beam_ssa:b_blk(),
      Blocks0 :: beam_ssa:block_map(),
      Blk :: beam_ssa:b_blk().

block(#b_blk{last=Last0}=Blk, Blocks) ->
    case share_terminator(Last0, Blocks) of
        none -> Blk;
        Last -> Blk#b_blk{last=beam_ssa:normalize(Last)}
    end.

%%%
%%% Local functions.
%%%

function(#b_function{anno=Anno,bs=Blocks0}=F) ->
    try
        PO = reverse(beam_ssa:rpo(Blocks0)),
        {Blocks1,Changed} = blocks(PO, Blocks0, false),
        Blocks = case Changed of
                     true ->
                         beam_ssa:trim_unreachable(Blocks1);
                     false ->
                         Blocks0
                 end,
        F#b_function{bs=Blocks}
    catch
        Class:Error:Stack ->
            #{func_info:={_,Name,Arity}} = Anno,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

blocks([L|Ls], Blocks, Changed) ->
    #b_blk{last=Last0} = Blk0 = map_get(L, Blocks),
    case block(Blk0, Blocks) of
        #b_blk{last=Last0} ->
            blocks(Ls, Blocks, Changed);
        #b_blk{}=Blk ->
            blocks(Ls, Blocks#{L:=Blk}, true)
    end;
blocks([], Blocks, Changed) ->
    {Blocks,Changed}.

share_terminator(#b_br{bool=#b_var{},succ=Succ0,fail=Fail0}=Br, Blocks) ->
    {Succ,SuccBlk} = shortcut_nonempty_block(Succ0, Blocks),
    {Fail,FailBlk} = shortcut_nonempty_block(Fail0, Blocks),
    case are_equivalent(Succ, SuccBlk, Fail, FailBlk, Blocks) of
        true ->
            %% The blocks are semantically equivalent.
            Br#b_br{succ=Succ,fail=Succ};
        false ->
            if
                Succ =:= Succ0, Fail =:= Fail0 ->
                    %% None of blocks were cut short.
                    none;
                true ->
                    %% One or both labels were cut short
                    %% to avoid jumping to an empty block.
                    Br#b_br{succ=Succ,fail=Fail}
            end
    end;
share_terminator(#b_switch{}=Sw, Blocks) ->
    share_switch(Sw, Blocks);
share_terminator(_Last, _Blocks) -> none.

%% Test whether the two blocks are semantically equivalent.  This
%% function is specially optimized to return `false` as fast as
%% possible if the blocks are not equivalent, as that is the common
%% case.

are_equivalent(_Succ, _, ?BADARG_BLOCK, _, _Blocks) ->
    %% ?BADARG_BLOCK is special. Sharing could be incorrect.
    false;
are_equivalent(_Succ, #b_blk{is=Is1,last=#b_ret{arg=RetVal1}=Ret1},
         _Fail, #b_blk{is=Is2,last=#b_ret{arg=RetVal2}=Ret2}, _Blocks) ->
    case {RetVal1,RetVal2} of
        {#b_literal{},#b_literal{}} ->
            case RetVal1 =:= RetVal2 of
                true ->
                    %% The return values are identical literals. We
                    %% only need to compare the canonicalized bodies.
                    Can1 = canonical_is(Is1),
                    Can2 = canonical_is(Is2),
                    Can1 =:= Can2;
                false ->
                    %% Non-equal literals.
                    false
            end;
        {#b_var{},#b_var{}} ->
            %% The return values are varibles. We must canonicalize
            %% the blocks (including returns) and compare them.
            Can1 = canonical_is(Is1 ++ [Ret1]),
            Can2 = canonical_is(Is2 ++ [Ret2]),
            Can1 =:= Can2;
        {_,_} ->
            %% One literal and one variable.
            false
    end;
are_equivalent(Succ,
         #b_blk{is=Is1,
                last=#b_br{bool=#b_literal{val=true},
                           succ=Target}},
         Fail,
         #b_blk{is=Is2,
                last=#b_br{bool=#b_literal{val=true},
                           succ=Target}},
         Blocks) ->
    %% Both blocks end with an unconditional branch to the
    %% same target block. If the target block has phi nodes,
    %% we must pick up the values from the phi nodes and
    %% compare them.
    #b_blk{is=Is} = map_get(Target, Blocks),
    Phis1 = canonical_terminator_phis(Is, Succ),
    Phis2 = canonical_terminator_phis(Is, Fail),
    case {Phis1,Phis2} of
        {[#b_set{args=[#b_literal{}]}|_],_} when Phis1 =/= Phis2 ->
            %% Different values are used in the phi nodes.
            false;
        {_,[#b_set{args=[#b_literal{}]}|_]} when Phis1 =/= Phis2 ->
            %% Different values are used in the phi nodes.
            false;
        {_,_} ->
            %% The values in the phi nodes are variables or identical
            %% literals. We must canonicalize the blocks and compare
            %% them.
            Can1 = canonical_is(Is1 ++ Phis1),
            Can2 = canonical_is(Is2 ++ Phis2),
            Can1 =:= Can2
    end;
are_equivalent(Succ0, #b_blk{is=Is1,last=#b_br{bool=#b_var{},fail=Same}},
         Fail0, #b_blk{is=Is2,last=#b_br{bool=#b_var{},fail=Same}},
         Blocks) ->
    %% Two-way branches with identical failure labels. First compare the
    %% canonicalized bodies of the blocks.
    case canonical_is(Is1) =:= canonical_is(Is2) of
        false ->
            %% Different bodies.
            false;
        true ->
            %% Bodies were equal. That is fairly uncommon, so to keep
            %% the code simple we will rewrite the `br` to a `switch`
            %% and let share_switch/2 do the work of following the
            %% branches.
            Sw = #b_switch{arg=#b_var{name=not_used},fail=Fail0,
                           list=[{#b_literal{},Succ0}]},
            #b_switch{fail=Fail,list=[{_,Succ}]} = share_switch(Sw, Blocks),
            Fail =:= Succ
    end;
are_equivalent(_, _, _, _, _) -> false.

share_switch(#b_switch{fail=Fail0,list=List0}=Sw, Blocks) ->
    Prep = share_prepare_sw([{value,Fail0}|List0], Blocks, 0, []),
    Res = do_share_switch(Prep, Blocks, []),
    [{_,Fail}|List] = [VL || {_,VL} <- sort(Res)],
    Sw#b_switch{fail=Fail,list=List}.

share_prepare_sw([{V,L0}|T], Blocks, N, Acc) ->
    {L,_Blk} = shortcut_nonempty_block(L0, Blocks),
    share_prepare_sw(T, Blocks, N+1, [{{L,#{}},{N,{V,L}}}|Acc]);
share_prepare_sw([], _, _, Acc) -> Acc.

do_share_switch(Prep, Blocks, Acc) ->
    Map = share_switch_1(Prep, Blocks, #{}),
    share_switch_2(maps:values(Map), Blocks, Acc).

share_switch_1([{Next0,Res}|T], Blocks, Map) ->
    {Can,Next} = canonical_block(Next0, Blocks),
    case Map of
        #{Can:=Ls} ->
            share_switch_1(T, Blocks, Map#{Can:=[{Next,Res}|Ls]});
        #{} ->
            share_switch_1(T, Blocks, Map#{Can=>[{Next,Res}]})
    end;
share_switch_1([], _Blocks, Map) -> Map.

share_switch_2([[{_,{N,Res}}]|T], Blocks, Acc) ->
    %% This block is not equivalent to any other block.
    share_switch_2(T, Blocks, [{N,Res}|Acc]);
share_switch_2([[{done,{_,{_,Common}}}|_]=Eqs|T], Blocks, Acc0) ->
    %% Two or more blocks are semantically equivalent, and all blocks
    %% are either terminated with a `ret` or a `br` to the same target
    %% block. Replace the labels in the `switch` for all of those
    %% blocks with the label for the first of the blocks.
    Acc = [{N,{V,Common}} || {done,{N,{V,_}}} <- Eqs] ++ Acc0,
    share_switch_2(T, Blocks, Acc);
share_switch_2([[{_,_}|_]=Prep|T], Blocks, Acc0) ->
    %% Two or more blocks are semantically equivalent, but they have
    %% different successful successor blocks. Now we must check
    %% recursively whether the successor blocks are equivalent too.
    Acc = do_share_switch(Prep, Blocks, Acc0),
    share_switch_2(T, Blocks, Acc);
share_switch_2([], _, Acc) -> Acc.

canonical_block({L,VarMap0}, Blocks) ->
    #b_blk{is=Is,last=Last0} = map_get(L, Blocks),
    case canonical_terminator(L, Last0, Blocks) of
        none ->
            %% The block has a terminator that we don't handle.
            {{none,L},done};
        {Last,done} ->
            %% The block ends with a `ret` or an unconditional `br` to
            %% another block.
            {Can,_VarMap} = canonical_is(Is ++ Last, VarMap0, []),
            {Can,done};
        {Last,Next} ->
            %% The block ends with a conditional branch.
            {Can,VarMap} = canonical_is(Is ++ Last, VarMap0, []),
            {Can,{Next,VarMap}}
    end.

%% Translate a sequence of instructions to a canonical representation. If the
%% canonical representation of two blocks compare equal, the blocks are
%% semantically equivalent. The following translations are done:
%%
%%    * Variables defined in the instruction sequence are replaced with
%%    {var,0}, {var,1}, and so on. Free variables are not changed.
%%
%%    * `location` annotations that would produce a `line` instruction are
%%    kept. All other annotations are cleared.
%%
%%    * Instructions are repackaged into tuples instead of into the
%%    usual records. The main reason is to avoid violating the types for
%%    the SSA records. We can simplify things a little by linking the
%%    instructions directly instead of putting them into a list.

canonical_is(Is) ->
    {Can,_} = canonical_is(Is, #{}, []),
    Can.

canonical_is([#b_set{op=Op,dst=Dst,args=Args0}=I|Is], VarMap0, Acc) ->
    Args = [canonical_arg(Arg, VarMap0) || Arg <-Args0],
    Var = {var,map_size(VarMap0)},
    VarMap = VarMap0#{Dst=>Var},
    LineAnno = case Op of
                   bs_match ->
                       %% The location annotation for a bs_match instruction
                       %% is only used in warnings, never to emit a `line`
                       %% instruction. Therefore, it should not be included.
                       [];
                   _ ->
                       %% The location annotation will be used in a `line`
                       %% instruction. It must be included.
                       beam_ssa:get_anno(location, I, none)
               end,
    canonical_is(Is, VarMap, {Op,LineAnno,Var,Args,Acc});
canonical_is([#b_ret{arg=Arg}], VarMap, Acc0) ->
    Acc1 = case Acc0 of
               {call,_Anno,Var,[#b_local{}|_]=Args,PrevAcc} ->
                   %% This is a tail-recursive call to a local function.
                   %% There will be no line instruction generated;
                   %% thus, the annotation is not significant.
                   {call,[],Var,Args,PrevAcc};
               _ ->
                   Acc0
           end,
    {{ret,canonical_arg(Arg, VarMap),Acc1},VarMap};
canonical_is([#b_br{bool=#b_var{}=Arg,fail=Fail}], VarMap, Acc) ->
    %% A previous buggy version of this code omitted the canonicalized
    %% argument in the return value. Unfortunately, that worked most
    %% of the time, except when `br` terminator referenced a variable
    %% defined in a previous block instead of in the same block.
    {{br,canonical_arg(Arg, VarMap),succ,Fail,Acc},VarMap};
canonical_is([#b_br{succ=Succ}], VarMap, Acc) ->
    {{br,Succ,Acc},VarMap};
canonical_is([], VarMap, Acc) ->
    {Acc,VarMap}.

canonical_terminator(_L, #b_ret{}=Ret, _Blocks) ->
    {[Ret],done};
canonical_terminator(L, #b_br{bool=#b_literal{val=true},succ=Succ}=Br, Blocks) ->
    #b_blk{is=Is} = map_get(Succ, Blocks),
    case canonical_terminator_phis(Is, L) of
        [] ->
            {[],Succ};
        [_|_]=Phis ->
            {Phis ++ [Br],done}
    end;
canonical_terminator(_L, #b_br{bool=#b_var{},succ=Succ}=Br, _Blocks) ->
    {[Br],Succ};
canonical_terminator(_, _, _) -> none.

canonical_terminator_phis([#b_set{op=phi,args=PhiArgs}=Phi|Is], L) ->
    {Value,L} = keyfind(L, 2, PhiArgs),
    [Phi#b_set{op=copy,args=[Value]}|canonical_terminator_phis(Is, L)];
canonical_terminator_phis([#b_set{op=peek_message}=I|_], L) ->
    %% We could get stuck into an infinite loop if we allowed the
    %% comparisons to continue into this block. Force an unequal
    %% compare with all other predecessors of this block.
    [I#b_set{op=copy,args=[#b_literal{val=L}]}];
canonical_terminator_phis(_, _) -> [].

canonical_arg(#b_var{}=Var, VarMap) ->
    case VarMap of
        #{Var:=CanonicalVar} ->
            CanonicalVar;
        #{} ->
            Var
    end;
canonical_arg(#b_remote{mod=Mod,name=Name}, VarMap) ->
    {remote,canonical_arg(Mod, VarMap),
     canonical_arg(Name, VarMap)};
canonical_arg(Other, _VarMap) -> Other.

%% Shortcut branches to empty blocks if safe.

shortcut_nonempty_block(L, Blocks) ->
    case map_get(L, Blocks) of
        #b_blk{is=[],last=#b_br{bool=#b_literal{val=true},succ=Succ}}=Blk ->
            %% This block is empty.
            case is_forbidden(Succ, Blocks) of
                false ->
                    shortcut_nonempty_block(Succ, Blocks);
                true ->
                    {L,Blk}
            end;
        #b_blk{}=Blk ->
            {L,Blk}
    end.

is_forbidden(L, Blocks) ->
    case map_get(L, Blocks) of
        #b_blk{is=[#b_set{op=phi}|_]} -> true;
        #b_blk{is=[#b_set{op=peek_message}|_]} -> true;
        #b_blk{} -> false
    end.
