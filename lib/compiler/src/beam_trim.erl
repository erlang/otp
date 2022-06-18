%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2021. All Rights Reserved.
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
-export([module/2]).

-import(lists, [any/2,reverse/1,reverse/2,sort/1]).

-include("beam_asm.hrl").

-record(st,
	{safe :: sets:set(beam_asm:label()) %Safe labels.
        }).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
        St = #st{safe=safe_labels(Is0, [])},
        Is = trim(Is0, St, []),
        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

trim([{init_yregs,{list,Kills0}}=I|Is0], St, Acc) ->
    Kills = [{kill,Y} || Y <- Kills0],
    try
        %% Find out the size and layout of the stack frame.
        %% Example of a layout:
        %%
        %%    [{kill,{y,0}},{dead,{y,1},{live,{y,2}},{kill,{y,3}}]
        %%
        %% That means that y0 and y3 are to be killed, that y1
        %% has been killed previously, and that y2 is live.
        {FrameSize,Layout} = frame_layout(Is0, Kills, St),

        %% Calculate all recipes that are not worse in terms
        %% of estimated execution time. The recipes are ordered
        %% in descending order from how much they trim.
        IsNotRecursive = is_not_recursive(Is0),
        Recipes = trim_recipes(Layout, IsNotRecursive),

        %% Try the recipes in order. A recipe may not work out because
        %% a register that was previously killed may be
        %% resurrected. If that happens, the next recipe, which trims
        %% less, will be tried.
        try_remap(Recipes, Is0, FrameSize)
    of
	{Is,TrimInstr} ->
            %% One of the recipes was applied.
	    trim(Is, St, reverse(TrimInstr)++Acc)
    catch
	not_possible ->
            %% No recipe worked out. Use the original init_yregs/1
            %% instruction.
	    trim(Is0, St, [I|Acc])
    end;
trim([I|Is], St, Acc) ->
    trim(Is, St, [I|Acc]);
trim([], _, Acc) ->
    reverse(Acc).

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

%% trim_recipes([{kill,R}|{live,R}|{dead,R}]) -> [Recipe].
%%      Recipe = {Kills,NumberToTrim,Moves}
%%      Kills = [{kill,Y}]
%%      Moves = [{move,SrcY,DstY}]
%%
%%  Calculate how to best trim the stack and kill the correct
%%  Y registers. Return a list of possible recipes. The best
%%  recipe (the one that trims the most) is first in the list.

trim_recipes(Layout, IsNotRecursive) ->
    Recipes = construct_recipes(Layout, 0, [], []),
    NumOrigKills = length([I || {kill,_}=I <- Layout]),
    IsTooExpensive = is_too_expensive_fun(IsNotRecursive),
    [R || R <- Recipes,
          not is_too_expensive(R, NumOrigKills, IsTooExpensive)].

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
construct_recipes([], _, _, Acc) -> Acc.

take_last_dead(L) ->
    take_last_dead_1(reverse(L)).

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

%% try_remap([Recipe], [Instruction], FrameSize) ->
%%           {[Instruction],[TrimInstruction]}.
%%  Try to renumber Y registers in the instruction stream. The
%%  first recipe that works will be used.
%%
%%  This function will issue a `not_possible` exception if none
%%  of the recipes were possible to apply.

try_remap([R|Rs], Is, FrameSize) ->
    {TrimInstr,Map} = expand_recipe(R, FrameSize),
    try
	{remap(Is, Map, []),TrimInstr}
    catch
	throw:not_possible ->
	    try_remap(Rs, Is, FrameSize)
    end;
try_remap([], _, _) -> throw(not_possible).

expand_recipe({Layout,Trim,Moves}, FrameSize) ->
    Is = reverse(Moves, [{trim,Trim,FrameSize-Trim}]),
    Map = create_map(Trim, Moves),
    case [Y || {kill,Y} <- Layout] of
        [] ->
            {Is,Map};
        [_|_]=Yregs ->
            {[{init_yregs,{list,Yregs}}|Is],Map}
    end.

create_map(Trim, []) ->
    fun({y,Y}) when Y < Trim ->
            throw(not_possible);
       ({y,Y}) ->
            {y,Y-Trim};
       (#tr{r={y,Y}}) when Y < Trim ->
            throw(not_possible);
       (#tr{r={y,Y},t=Type}) ->
            #tr{r={y,Y-Trim},t=Type};
       ({frame_size,N}) ->
            N - Trim;
       (Any) ->
            Any
    end;
create_map(Trim, Moves) ->
    Map0 = [{Src,Dst-Trim} || {move,{y,Src},{y,Dst}} <- Moves],
    Map = maps:from_list(Map0),
    IllegalTargets = sets:from_list([Dst || {move,_,{y,Dst}} <- Moves], [{version, 2}]),
    fun({y,Y0}) when Y0 < Trim ->
            case Map of
                #{Y0:=Y} -> {y,Y};
                #{} -> throw(not_possible)
            end;
       ({y,Y}) ->
            case sets:is_element(Y, IllegalTargets) of
                true -> throw(not_possible);
                false -> {y,Y-Trim}
            end;
       (#tr{r={y,Y0},t=Type}) when Y0 < Trim ->
            case Map of
                #{Y0:=Y} -> #tr{r={y,Y},t=Type};
                #{} -> throw(not_possible)
            end;
       (#tr{r={y,Y},t=Type}) ->
            case sets:is_element(Y, IllegalTargets) of
                true -> throw(not_possible);
                false -> #tr{r={y,Y-Trim},t=Type}
            end;
       ({frame_size,N}) ->
            N - Trim;
       (Any) ->
            Any
    end.

remap([{'%',Comment}=I|Is], Map, Acc) ->
    case Comment of
        {var_info,Var,Type} ->
            remap(Is, Map, [{'%',{var_info,Map(Var),Type}}|Acc]);
        _ ->
            remap(Is, Map, [I|Acc])
    end;
remap([{block,Bl0}|Is], Map, Acc) ->
    Bl = remap_block(Bl0, Map, []),
    remap(Is, Map, [{block,Bl}|Acc]);
remap([{bs_get_tail,Src,Dst,Live}|Is], Map, Acc) ->
    I = {bs_get_tail,Map(Src),Map(Dst),Live},
    remap(Is, Map, [I|Acc]);
remap([{bs_start_match4,Fail,Live,Src,Dst}|Is], Map, Acc) ->
    I = {bs_start_match4,Fail,Live,Map(Src),Map(Dst)},
    remap(Is, Map, [I|Acc]);
remap([{bs_set_position,Src1,Src2}|Is], Map, Acc) ->
    I = {bs_set_position,Map(Src1),Map(Src2)},
    remap(Is, Map, [I|Acc]);
remap([{call_fun,_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]);
remap([{call,_,_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]);
remap([{call_ext,_,_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]);
remap([{apply,_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]);
remap([{bif,Name,Fail,Ss,D}|Is], Map, Acc) ->
    I = {bif,Name,Fail,[Map(S) || S <- Ss],Map(D)},
    remap(Is, Map, [I|Acc]);
remap([{gc_bif,Name,Fail,Live,Ss,D}|Is], Map, Acc) ->
    I = {gc_bif,Name,Fail,Live,[Map(S) || S <- Ss],Map(D)},
    remap(Is, Map, [I|Acc]);
remap([{get_map_elements,Fail,M,{list,L0}}|Is], Map, Acc) ->
    L = [Map(E) || E <- L0],
    I = {get_map_elements,Fail,Map(M),{list,L}},
    remap(Is, Map, [I|Acc]);
remap([{init_yregs,{list,Yregs0}}|Is], Map, Acc) ->
    Yregs = sort([Map(Y) || Y <- Yregs0]),
    I = {init_yregs,{list,Yregs}},
    remap(Is, Map, [I|Acc]);
remap([{make_fun2,_,_,_,_}=I|T], Map, Acc) ->
    remap(T, Map, [I|Acc]);
remap([{make_fun3,F,Index,OldUniq,Dst0,{list,Env0}}|T], Map, Acc) ->
    Env = [Map(E) || E <- Env0],
    Dst = Map(Dst0),
    I = {make_fun3,F,Index,OldUniq,Dst,{list,Env}},
    remap(T, Map, [I|Acc]);
remap([{deallocate,N}|Is], Map, Acc) ->
    I = {deallocate,Map({frame_size,N})},
    remap(Is, Map, [I|Acc]);
remap([{recv_marker_clear,Ref}|Is], Map, Acc) ->
    I = {recv_marker_clear,Map(Ref)},
    remap(Is, Map, [I|Acc]);
remap([{recv_marker_reserve,Mark}|Is], Map, Acc) ->
    I = {recv_marker_reserve,Map(Mark)},
    remap(Is, Map, [I|Acc]);
remap([{swap,Reg1,Reg2}|Is], Map, Acc) ->
    I = {swap,Map(Reg1),Map(Reg2)},
    remap(Is, Map, [I|Acc]);
remap([{test,Name,Fail,Ss}|Is], Map, Acc) ->
    I = {test,Name,Fail,[Map(S) || S <- Ss]},
    remap(Is, Map, [I|Acc]);
remap([{test,Name,Fail,Live,Ss,Dst}|Is], Map, Acc) ->
    I = {test,Name,Fail,Live,[Map(S) || S <- Ss],Map(Dst)},
    remap(Is, Map, [I|Acc]);
remap([return|_]=Is, _, Acc) ->
    reverse(Acc, Is);
remap([{line,_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]).

remap_block([{set,Ds0,Ss0,Info}|Is], Map, Acc) ->
    Ds = [Map(D) || D <- Ds0],
    Ss = [Map(S) || S <- Ss0],
    remap_block(Is, Map, [{set,Ds,Ss,Info}|Acc]);
remap_block([], _, Acc) -> reverse(Acc).

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

frame_layout(Is, Kills, #st{safe=Safe}) ->
    N = frame_size(Is, Safe),
    IsKilled = fun(R) -> is_not_used(R, Is) end,
    {N,frame_layout_1(Kills, 0, N, IsKilled, [])}.

frame_layout_1([{kill,{y,Y}}=I|Ks], Y, N, IsKilled, Acc) ->
    frame_layout_1(Ks, Y+1, N, IsKilled, [I|Acc]);
frame_layout_1(Ks, Y, N, IsKilled, Acc) when Y < N ->
    R = {y,Y},
    I = case IsKilled(R) of
	    false -> {live,R};
	    true -> {dead,R}
	end,
    frame_layout_1(Ks, Y+1, N, IsKilled, [I|Acc]);
frame_layout_1([], Y, Y, _, Acc) ->
    frame_layout_2(Acc).

frame_layout_2([{live,_}|Is]) -> frame_layout_2(Is);
frame_layout_2(Is) -> reverse(Is).

%% frame_size([Instruction], SafeLabels) -> FrameSize
%%  Find out the frame size by looking at the code that follows.
%%
%%  Implicitly, also check that the instructions are a straight
%%  sequence of code that ends in a return. Any branches are
%%  to safe labels (i.e., the code at those labels don't depend
%%  on the contents of any Y register).

frame_size([{'%',_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{block,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{call_fun,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{call,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{call_ext,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{apply,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{bif,_,{f,L},_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{gc_bif,_,{f,L},_,_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{test,_,{f,L},_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{test,_,{f,L},_,_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{init_yregs,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{make_fun2,_,_,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{make_fun3,_,_,_,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{recv_marker_clear,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{recv_marker_reserve,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{get_map_elements,{f,L},_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{deallocate,N}|_], _) ->
    N;
frame_size([{line,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{bs_start_match4,Fail,_,_,_}|Is], Safe) ->
    case Fail of
        {f,L} -> frame_size_branch(L, Is, Safe);
        _ -> frame_size(Is, Safe)
    end;
frame_size([{bs_set_position,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{bs_get_tail,_,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{swap,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size(_, _) -> throw(not_possible).

frame_size_branch(0, Is, Safe) ->
    frame_size(Is, Safe);
frame_size_branch(L, Is, Safe) ->
    case sets:is_element(L, Safe) of
	false -> throw(not_possible);
	true -> frame_size(Is, Safe)
    end.

%% is_not_used(Y, [Instruction]) -> true|false.
%%  Test whether the value of Y is unused in the instruction sequence.
%%  Return true if the value of Y is not used, and false if it is used.
%%
%%  This function handles the same instructions as frame_size/2. It
%%  assumes that any labels in the instructions are safe labels.

is_not_used(Y, [{'%',_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{apply,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{bif,_,{f,_},Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is);
is_not_used(Y, [{block,Bl}|Is]) ->
    case is_not_used_block(Y, Bl) of
        used -> false;
        killed -> true;
        transparent -> is_not_used(Y, Is)
    end;
is_not_used(Y, [{bs_get_tail,Src,Dst,_}|Is]) ->
    is_not_used_ss_dst(Y, [Src], Dst, Is);
is_not_used(Y, [{bs_start_match4,_Fail,_Live,Src,Dst}|Is]) ->
    Y =/= Src andalso Y =/= Dst andalso
        is_not_used(Y, Is);
is_not_used(Y, [{bs_set_position,Src1,Src2}|Is]) ->
    Y =/= Src1 andalso Y =/= Src2 andalso
        is_not_used(Y, Is);
is_not_used(Y, [{call,_,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{call_ext,_,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{call_fun,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(_Y, [{deallocate,_}|_]) ->
    true;
is_not_used(Y, [{gc_bif,_,{f,_},_Live,Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is);
is_not_used(Y, [{get_map_elements,{f,_},S,{list,List}}|Is]) ->
    {Ss,Ds} = beam_utils:split_even(List),
    case is_y_member(Y, [S|Ss]) of
	true ->
	    false;
	false ->
            is_y_member(Y, Ds) orelse is_not_used(Y, Is)
    end;
is_not_used(Y, [{init_yregs,{list,Yregs}}|Is]) ->
    is_y_member(Y, Yregs) orelse is_not_used(Y, Is);
is_not_used(Y, [{line,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{make_fun2,_,_,_,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{make_fun3,_,_,_,Dst,{list,Env}}|Is]) ->
    is_not_used_ss_dst(Y, Env, Dst, Is);
is_not_used(Y, [{recv_marker_clear,Ref}|Is]) ->
    Y =/= Ref andalso is_not_used(Y, Is);
is_not_used(Y, [{recv_marker_reserve,Dst}|Is]) ->
    Y =/= Dst andalso is_not_used(Y, Is);
is_not_used(Y, [{swap,Reg1,Reg2}|Is]) ->
    Y =/= Reg1 andalso Y =/= Reg2 andalso is_not_used(Y, Is);
is_not_used(Y, [{test,_,_,Ss}|Is]) ->
    not is_y_member(Y, Ss) andalso is_not_used(Y, Is);
is_not_used(Y, [{test,_Op,{f,_},_Live,Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is).

is_not_used_block(Y, [{set,Ds,Ss,_}|Is]) ->
    case is_y_member(Y, Ss) of
        true ->
            used;
        false ->
            case is_y_member(Y, Ds) of
                true ->
                    killed;
                false ->
                    is_not_used_block(Y, Is)
            end
    end;
is_not_used_block(_Y, []) -> transparent.

is_not_used_ss_dst(Y, Ss, Dst, Is) ->
    not is_y_member(Y, Ss) andalso (Y =:= Dst orelse is_not_used(Y, Is)).

is_y_member({y,N0}, Ss) ->
    any(fun(#tr{r={y,N}}) ->
                N =:= N0;
           ({y,N}) ->
                N =:= N0;
           (_) ->
                false
        end, Ss).
