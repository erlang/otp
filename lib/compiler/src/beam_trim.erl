%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

-module(beam_trim).
-moduledoc false.
-export([module/2]).

-import(lists, [any/2,reverse/1,reverse/2,seq/2,sort/1]).

-include("beam_asm.hrl").

-record(st,
        {safe :: sets:set(beam_asm:label()),    %Safe labels.
         fsz :: non_neg_integer()
        }).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
        St = #st{safe=safe_labels(Is0, []),fsz=0},
        Usage = none,
        Is = trim(Is0, Usage, St),
        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

trim([{init_yregs,_}=I|Is], none, St0) ->
    case usage(Is, St0) of
        none ->
            [I|trim(Is, none, St0)];
        {FrameSize,Us} ->
            St = St0#st{fsz=FrameSize},
            trim([I|Is], Us, St)
    end;
trim([{init_yregs,{list,Killed}}=I|Is0], [U|Us], St) ->
    FrameSize = St#st.fsz,

    %% Find out layout of the stack frame. Example of a layout:
    %%
    %%    [{kill,{y,0}},{dead,{y,1},{live,{y,2}},{kill,{y,3}}]
    %%
    %% That means that y0 and y3 are to be killed, that y1
    %% has been killed previously, and that y2 is live.
    Layout = frame_layout(FrameSize, Killed, U),

    %% Find a trim recipe.
    IsNotRecursive = is_not_recursive(Is0),
    case trim_recipe(Layout, IsNotRecursive, U) of
        none ->
            %% No recipe worked out. Use the original init_yregs/1
            %% instruction.
	    [I|trim(Is0, Us, St)];
        R ->
            %% Apply the recipe.
            {TrimInstr,Remap} = expand_recipe(R, FrameSize),
            Is = remap(Is0, Remap),
	    TrimInstr ++ trim(Is, none, St)
    end;
trim([I|Is], [_|Us], St) ->
    [I|trim(Is, Us, St)];
trim([I|Is], none, St) ->
    [I|trim(Is, none, St)];
trim([I|Is], [], St) ->
    [I|trim(Is, none, St)];
trim([], _, _) -> [].

%% is_not_recursive([Instruction]) -> true|false.
%%  Test whether the next call or apply instruction may
%%  do a recursive call. Return `true` if the call is
%%  definitely not recursive, and `false` otherwise.
is_not_recursive([{call_ext,_,Ext}|_]) ->
    case Ext of
        {extfunc,M,F,A} ->
            erl_bifs:is_pure(M, F, A);
        _ ->
            false
    end;
is_not_recursive([{block,_}|Is]) -> is_not_recursive(Is);
is_not_recursive([{line,_}|Is]) -> is_not_recursive(Is);
is_not_recursive(_) -> false.

%% trim_recipe([{kill,R}|{live,R}|{dead,R}]) -> Recipe | none.
%%      Recipe = {Kills,NumberToTrim,Moves}
%%      Kills = [{kill,Y}]
%%      Moves = [{move,SrcY,DstY}]
%%
%%  Calculate how to best trim the stack and kill the correct
%%  Y registers. Return the recipe that trims the most.

trim_recipe(Layout, IsNotRecursive, {Us,Ns}) ->
    UsedRegs = ordsets:union(Us, Ns),
    Recipes = construct_recipes(Layout, 0, [], []),
    NumOrigKills = length([I || {kill,_}=I <- Layout]),
    IsTooExpensive = is_too_expensive_fun(IsNotRecursive),
    Rs = [R || R <- Recipes,
               is_recipe_viable(R, UsedRegs),
               not is_too_expensive(R, NumOrigKills, IsTooExpensive)],
    case Rs of
        [] -> none;
        [R|_] -> R
    end.

construct_recipes([{kill,{y,Trim0}}|Ks], Trim0, Moves, Acc) ->
    Trim = Trim0 + 1,
    Recipe = {Ks,Trim,Moves},
    construct_recipes(Ks, Trim, Moves, [Recipe|Acc]);
construct_recipes([{dead,{y,Trim0}}|Ks], Trim0, Moves, Acc) ->
    Trim = Trim0 + 1,
    Recipe = {Ks,Trim,Moves},
    construct_recipes(Ks, Trim, Moves, [Recipe|Acc]);
construct_recipes([{live,{y,Trim0}=Src}|Ks0], Trim0, Moves0, Acc) ->
    case take_last_dead(Ks0) of
	none ->
            %% No more recipes are possible.
            Acc;
	{Dst,Ks} ->
	    Trim = Trim0 + 1,
	    Moves = [{move,Src,Dst}|Moves0],
            Recipe = {Ks,Trim,Moves},
            construct_recipes(Ks, Trim, Moves, [Recipe|Acc])
    end;
construct_recipes(_, _, _, Acc) ->
    Acc.

take_last_dead(L) ->
    take_last_dead_1(reverse(L)).

take_last_dead_1([{live,_}|Is]) ->
    take_last_dead_1(Is);
take_last_dead_1([{kill,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1([{dead,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1(_) -> none.

%% Is trimming too expensive?
is_too_expensive({Ks,_,Moves}, NumOrigKills, IsTooExpensive) ->
    NumKills = num_kills(Ks, 0),
    NumMoves = length(Moves),
    IsTooExpensive(NumKills, NumMoves, NumOrigKills).

num_kills([{kill,_}|T], Acc) ->
    num_kills(T, Acc+1);
num_kills([_|T], Acc) ->
    num_kills(T, Acc);
num_kills([], Acc) -> Acc.

is_too_expensive_fun(true) ->
    %% This call is not recursive (because it is a call to a BIF).
    %% Here we should avoid trimming if the trimming sequence is
    %% likely to be more expensive than the original sequence.
    fun(NumKills, NumMoves, NumOrigKills) ->
            Penalty =
                if
                    %% Slightly penalize the use of any `move`
                    %% instruction to avoid replacing two `kill`
                    %% instructions with a `move` and a `trim`.
                    NumMoves =/= 0 -> 1;
                    true -> 0
                end,
            1 + Penalty + NumKills + NumMoves > NumOrigKills
    end;
is_too_expensive_fun(false) ->
    %% This call **may** be recursive. In a recursive function that
    %% builds up a huge stack, having unused stack slots will be very
    %% expensive. Therefore, we want to be biased towards trimming.
    %% We will do that by not counting the `trim` instruction in
    %% the formula below.
    fun(NumKills, NumMoves, NumOrigKills) ->
            NumKills + NumMoves > NumOrigKills
    end.

is_recipe_viable({_,Trim,Moves}, UsedRegs) ->
    Moved = ordsets:from_list([Src || {move,Src,_} <- Moves]),
    Illegal = ordsets:from_list([Dst || {move,_,Dst} <- Moves]),
    Eliminated = [{y,N} || N <- seq(0, Trim - 1)],
    %% All eliminated registers that are also in the used set must be moved.
    UsedEliminated = ordsets:intersection(Eliminated, UsedRegs),
    case ordsets:is_subset(UsedEliminated, Moved) andalso
        ordsets:is_disjoint(Illegal, UsedRegs) of
        true ->
            UsedEliminated = Moved,                        %Assertion.
            true;
        _ ->
            false
    end.

expand_recipe({Layout,Trim,Moves}, FrameSize) ->
    Is = reverse(Moves, [{trim,Trim,FrameSize-Trim}]),
    Map = #{Src => Dst - Trim || {move,{y,Src},{y,Dst}} <- Moves},
    Remap = {Trim,Map},
    case [Y || {kill,Y} <- Layout] of
        [] ->
            {Is,Remap};
        [_|_]=Yregs ->
            {[{init_yregs,{list,Yregs}}|Is],Remap}
    end.

remap([{'%',Comment}=I0|Is], Remap) ->
    case Comment of
        {var_info,{y,_}=Var,Type} ->
            I = {'%',{var_info,remap_arg(Var, Remap),Type}},
            [I|remap(Is, Remap)];
        _ ->
            [I0|remap(Is, Remap)]
    end;
remap([{block,Bl0}|Is], Remap) ->
    Bl = remap_block(Bl0, Remap),
    I = {block,Bl},
    [I|remap(Is, Remap)];
remap([{bs_create_bin,Fail,Alloc,Live,Unit,Dst0,{list,Ss0}}|Is], Remap) ->
    Dst = remap_arg(Dst0, Remap),
    Ss = remap_args(Ss0, Remap),
    I = {bs_create_bin,Fail,Alloc,Live,Unit,Dst,{list,Ss}},
    [I|remap(Is, Remap)];
remap([{bs_get_tail,Src,Dst,Live}|Is], Remap) ->
    I = {bs_get_tail,remap_arg(Src, Remap),remap_arg(Dst, Remap),Live},
    [I|remap(Is, Remap)];
remap([{bs_start_match4,Fail,Live,Src,Dst}|Is], Remap) ->
    I = {bs_start_match4,Fail,Live,remap_arg(Src, Remap),remap_arg(Dst, Remap)},
    [I|remap(Is, Remap)];
remap([{bs_set_position,Src1,Src2}|Is], Remap) ->
    I = {bs_set_position,remap_arg(Src1, Remap),remap_arg(Src2, Remap)},
    [I|remap(Is, Remap)];
remap([{call_fun,_}=I|Is], Remap) ->
    [I|remap(Is, Remap)];
remap([{call_fun2,Tag,Arity,Func}=I|Is], Remap) ->
    I = {call_fun2,Tag,Arity,remap_arg(Func, Remap)},
    [I|remap(Is, Remap)];
remap([{call,_,_}=I|Is], Remap) ->
    [I|remap(Is, Remap)];
remap([{call_ext,_,_}=I|Is], Remap) ->
    [I|remap(Is, Remap)];
remap([{apply,_}=I|Is], Remap) ->
    [I|remap(Is, Remap)];
remap([{bif,Name,Fail,Ss,D}|Is], Remap) ->
    I = {bif,Name,Fail,remap_args(Ss, Remap),remap_arg(D, Remap)},
    [I|remap(Is, Remap)];
remap([{gc_bif,Name,Fail,Live,Ss,D}|Is], Remap) ->
    I = {gc_bif,Name,Fail,Live,remap_args(Ss, Remap),remap_arg(D, Remap)},
    [I|remap(Is, Remap)];
remap([{get_map_elements,Fail,M,{list,L0}}|Is], Remap) ->
    L = remap_args(L0, Remap),
    I = {get_map_elements,Fail,remap_arg(M, Remap),{list,L}},
    [I|remap(Is, Remap)];
remap([{init_yregs,{list,Yregs0}}|Is], Remap) ->
    Yregs = sort(remap_args(Yregs0, Remap)),
    I = {init_yregs,{list,Yregs}},
    [I|remap(Is, Remap)];
remap([{make_fun3,F,Index,OldUniq,Dst0,{list,Env0}}|Is], Remap) ->
    Env = remap_args(Env0, Remap),
    Dst = remap_arg(Dst0, Remap),
    I = {make_fun3,F,Index,OldUniq,Dst,{list,Env}},
    [I|remap(Is, Remap)];
remap([{update_record,Hint,Size,Src0,Dst0,{list,Updates0}}|Is], Remap) ->
    Updates = remap_args(Updates0, Remap),
    Src = remap_arg(Src0, Remap),
    Dst = remap_arg(Dst0, Remap),
    I = {update_record,Hint,Size,Src,Dst,{list,Updates}},
    [I|remap(Is, Remap)];
remap([{deallocate,N}|Is], {Trim,_}=Remap) ->
    I = {deallocate,N-Trim},
    [I|remap(Is, Remap)];
remap([{recv_marker_clear,Ref}|Is], Remap) ->
    I = {recv_marker_clear,remap_arg(Ref, Remap)},
    [I|remap(Is, Remap)];
remap([{recv_marker_reserve,Mark}|Is], Remap) ->
    I = {recv_marker_reserve,remap_arg(Mark, Remap)},
    [I|remap(Is, Remap)];
remap([{test,Name,Fail,Ss}|Is], Remap) ->
    I = {test,Name,Fail,remap_args(Ss, Remap)},
    [I|remap(Is, Remap)];
remap([{test,Name,Fail,Live,Ss,Dst}|Is], Remap) ->
    I = {test,Name,Fail,Live,remap_args(Ss, Remap),remap_arg(Dst, Remap)},
    [I|remap(Is, Remap)];
remap([return|_]=Is, _) ->
    Is;
remap([{line,_}=I|Is], Remap) ->
    [I|remap(Is, Remap)].

remap_block([{set,[{x,_}]=Ds,Ss0,Info}|Is], Remap) ->
    Ss = remap_args(Ss0, Remap),
    [{set,Ds,Ss,Info}|remap_block(Is, Remap)];
remap_block([{set,Ds0,Ss0,Info}|Is], Remap) ->
    Ds = remap_args(Ds0, Remap),
    Ss = remap_args(Ss0, Remap),
    [{set,Ds,Ss,Info}|remap_block(Is, Remap)];
remap_block([], _) -> [].

remap_args(Args, {Trim,Map}) ->
    [remap_arg(Arg, Trim, Map) || Arg <- Args].

remap_arg(Arg, {Trim,Map}) ->
    remap_arg(Arg, Trim, Map).

remap_arg(Arg, Trim, Map) ->
    case Arg of
        {y,Y} when Y < Trim ->
            {y,map_get(Y, Map)};
        {y,Y} ->
            {y,Y-Trim};
        #tr{r={y,Y},t=Type} when Y < Trim ->
            #tr{r={y,map_get(Y, Map)},t=Type};
        #tr{r={y,Y},t=Type} ->
            #tr{r={y,Y-Trim},t=Type};
        Other ->
            Other
    end.

%% safe_labels([Instruction], Accumulator) -> gb_set()
%%  Build a gb_set of safe labels. The code at a safe
%%  label does not depend on the values in a specific
%%  Y register, only that all Y registers are initialized
%%  so that it safe to scan the stack when an exception
%%  is generated.
%%
%%  In other words, code at a safe label will continue
%%  to work if Y registers have been renumbered and
%%  the size of the stack frame has changed.

safe_labels([{label,L}|Is], Acc) ->
    case is_safe_label(Is) of
        true -> safe_labels(Is, [L|Acc]);
        false -> safe_labels(Is, Acc)
    end;
safe_labels([_|Is], Acc) ->
    safe_labels(Is, Acc);
safe_labels([], Acc) -> sets:from_list(Acc, [{version, 2}]).

is_safe_label([{'%',_}|Is]) ->
    is_safe_label(Is);
is_safe_label([{line,_}|Is]) ->
    is_safe_label(Is);
is_safe_label([{badmatch,{Tag,_}}|_]) ->
    Tag =/= y;
is_safe_label([{case_end,{Tag,_}}|_]) ->
    Tag =/= y;
is_safe_label([{try_case_end,{Tag,_}}|_]) ->
    Tag =/= y;
is_safe_label([if_end|_]) ->
    true;
is_safe_label([{badrecord,{Tag,_}}|_]) ->
    Tag =/= y;
is_safe_label([{block,Bl}|Is]) ->
    is_safe_label_block(Bl) andalso is_safe_label(Is);
is_safe_label([{call_ext,_,{extfunc,M,F,A}}|_]) ->
    erl_bifs:is_exit_bif(M, F, A);
is_safe_label(_) -> false.

is_safe_label_block([{set,Ds,Ss,_}|Is]) ->
    IsYreg = fun(#tr{r={y,_}}) -> true;
                ({y,_}) -> true;
                (_) -> false
             end,
    %% This instruction is safe if the instruction
    %% neither reads or writes Y registers.
    not (any(IsYreg, Ss) orelse any(IsYreg, Ds)) andalso
        is_safe_label_block(Is);
is_safe_label_block([]) -> true.

%% frame_layout([Instruction], [{kill,_}], St) ->
%%      [{kill,Reg} | {live,Reg} | {dead,Reg}]
%%  Figure out the layout of the stack frame.

frame_layout(N, Killed, {U,_}) ->
    Dead0 = [{y,R} || R <- seq(0, N - 1)],
    Dead = ordsets:subtract(Dead0, ordsets:union(U, Killed)),
    Is = [[{R,{live,R}} || R <- U],
          [{R,{dead,R}} || R <- Dead],
          [{R,{kill,R}} || R <- Killed]],
    [I || {_,I} <- lists:merge(Is)].

%% usage([Instruction], SafeLabels) -> {FrameSize,[UsedYRegs]}
%%  Find out the frame size and usage information by looking at the
%%  code that follows.
%%
%%  Implicitly, also check that the instructions are a straight
%%  sequence of code that ends in a return. Any branches are
%%  to safe labels (i.e., the code at those labels don't depend
%%  on the contents of any Y register).

usage(Is0, St) ->
    Is = usage_1(Is0, []),
    do_usage(Is, St).

usage_1([{label,_}|_], Acc) ->
    Acc;
usage_1([I|Is], Acc) ->
    usage_1(Is, [I|Acc]);
usage_1([], Acc) ->
    Acc.

do_usage(Is0, #st{safe=Safe}) ->
    case Is0 of
        [return,{deallocate,N}|Is] ->
            Regs = [],
            case do_usage(Is, Safe, Regs, [], []) of
                none ->
                    none;
                Us ->
                    {N,Us}
            end;
        _ ->
            none
    end.

do_usage([{'%',_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{apply,_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{block,Blk}|Is], Safe, Regs0, Ns0, Acc) ->
    {Regs,Ns} = U = do_usage_blk(Blk, Regs0, Ns0),
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{bs_create_bin,Fail,_,_,_,Dst,{list,Args}}|Is], Safe, Regs0, Ns0, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            Regs1 = ordsets:del_element(Dst, Regs0),
            Regs = ordsets:union(Regs1, yregs(Args)),
            Ns = ordsets:union(yregs([Dst]), Ns0),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{bs_get_tail,Src,Dst,_}|Is], Safe, Regs0, Ns0, Acc) ->
    Regs1 = ordsets:del_element(Dst, Regs0),
    Regs = ordsets:union(Regs1, yregs([Src])),
    Ns = ordsets:union(yregs([Dst]), Ns0),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{bs_set_position,Src1,Src2}|Is], Safe, Regs0, Ns, Acc) ->
    Regs = ordsets:union(Regs0, yregs([Src1,Src2])),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{bs_start_match4,Fail,_Live,Src,Dst}|Is], Safe, Regs0, Ns, Acc) ->
    case (Fail =:= {atom,no_fail} orelse
          Fail =:= {atom,resume} orelse
          is_safe_branch(Fail, Safe)) of
        true ->
            Regs = ordsets:union(Regs0, yregs([Src,Dst])),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{call,_,_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{call_ext,_,_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{call_fun,_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{call_fun2,_,_,Ss}|Is], Safe, Regs0, Ns, Acc) ->
    Regs = ordsets:union(Regs0, yregs([Ss])),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{bif,_,Fail,Ss,Dst}|Is], Safe, Regs0, Ns0, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            Regs1 = ordsets:del_element(Dst, Regs0),
            Regs = ordsets:union(Regs1, yregs(Ss)),
            Ns = ordsets:union(yregs([Dst]), Ns0),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{gc_bif,_,Fail,_,Ss,Dst}|Is], Safe, Regs0, Ns0, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            Regs1 = ordsets:del_element(Dst, Regs0),
            Regs = ordsets:union(Regs1, yregs(Ss)),
            Ns = ordsets:union(yregs([Dst]), Ns0),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{get_map_elements,Fail,S,{list,List}}|Is], Safe, Regs0, Ns0, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            {Ss,Ds1} = beam_utils:split_even(List),
            Ds = yregs(Ds1),
            Regs1 = ordsets:subtract(Regs0, Ds),
            Regs = ordsets:union(Regs1, yregs([S|Ss])),
            Ns = ordsets:union(Ns0, Ds),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{init_yregs,{list,Ds}}|Is], Safe, Regs0, Ns0, Acc) ->
    Regs = ordsets:subtract(Regs0, Ds),
    Ns = ordsets:union(Ns0, Ds),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{make_fun3,_,_,_,Dst,{list,Ss}}|Is], Safe, Regs0, Ns0, Acc) ->
    Regs1 = ordsets:del_element(Dst, Regs0),
    Regs = ordsets:union(Regs1, yregs(Ss)),
    Ns = ordsets:union(yregs([Dst]), Ns0),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{update_record,_,_,Src,Dst,{list,Ss}}|Is], Safe, Regs0, Ns0, Acc) ->
    Regs1 = ordsets:del_element(Dst, Regs0),
    Regs = ordsets:union(Regs1, yregs([Src|Ss])),
    Ns = ordsets:union(yregs([Dst]), Ns0),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{line,_}|Is], Safe, Regs, Ns, Acc) ->
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{recv_marker_clear,Src}|Is], Safe, Regs0, Ns, Acc) ->
    Regs = ordsets:union(Regs0, yregs([Src])),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{recv_marker_reserve,Src}|Is], Safe, Regs0, Ns, Acc) ->
    Regs = ordsets:union(Regs0, yregs([Src])),
    U = {Regs,Ns},
    do_usage(Is, Safe, Regs, Ns, [U|Acc]);
do_usage([{test,_,Fail,Ss}|Is], Safe, Regs0, Ns, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            Regs = ordsets:union(Regs0, yregs(Ss)),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([{test,_,Fail,_,Ss,Dst}|Is], Safe, Regs0, Ns0, Acc) ->
    case is_safe_branch(Fail, Safe) of
        true ->
            Regs1 = ordsets:del_element(Dst, Regs0),
            Regs = ordsets:union(Regs1, yregs(Ss)),
            Ns = ordsets:union(yregs([Dst]), Ns0),
            U = {Regs,Ns},
            do_usage(Is, Safe, Regs, Ns, [U|Acc]);
        false ->
            none
    end;
do_usage([_I|_], _, _, _, _) ->
    none;
do_usage([], _Safe, _Regs, _Ns, Acc) -> Acc.

do_usage_blk([{set,Ds0,Ss,_}|Is], Regs0, Ns0) ->
    Ds = yregs(Ds0),
    {Regs1,Ns1} = do_usage_blk(Is, Regs0, Ns0),
    Regs2 = ordsets:subtract(Regs1, Ds),
    Regs = ordsets:union(Regs2, yregs(Ss)),
    Ns = ordsets:union(Ns1, Ds),
    {Regs,Ns};
do_usage_blk([], Regs, Ns) -> {Regs,Ns}.

is_safe_branch({f,0}, _Safe) ->
    true;
is_safe_branch({f,L}, Safe) ->
    sets:is_element(L, Safe).

yregs(Rs) ->
    ordsets:from_list(yregs_1(Rs)).

yregs_1([{y,_}=Y|Rs]) ->
    [Y|yregs_1(Rs)];
yregs_1([{tr,{y,_}=Y,_}|Rs]) ->
    [Y|yregs_1(Rs)];
yregs_1([_|Rs]) ->
    yregs_1(Rs);
yregs_1([]) -> [].
