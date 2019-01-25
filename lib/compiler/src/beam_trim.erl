%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-import(lists, [any/2,member/2,reverse/1,reverse/2,splitwith/2,sort/1]).

-record(st,
	{safe :: cerl_sets:set(beam_asm:label()) %Safe labels.
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

trim([{kill,_}|_]=Is0, St, Acc) ->
    {Kills0,Is1} = splitwith(fun({kill,_}) -> true;
			       (_) -> false
			    end, Is0),
    Kills = sort(Kills0),
    try
        %% Find out the size and layout of the stack frame.
        %% Example of a layout:
        %%
        %%    [{kill,{y,0}},{dead,{y,1},{live,{y,2}},{kill,{y,3}}]
        %%
        %% That means that y0 and y3 are to be killed, that y1
        %% has been killed previously, and that y2 is live.
        {FrameSize,Layout} = frame_layout(Is1, Kills, St),

        %% Calculate all recipes that are not worse in terms
        %% of estimated execution time. The recipes are ordered
        %% in descending order from how much they trim.
        Recipes = trim_recipes(Layout),

        %% Try the recipes in order. A recipe may not work out because
        %% a register that was previously killed may be
        %% resurrected. If that happens, the next recipe, which trims
        %% less, will be tried.
        try_remap(Recipes, Is1, FrameSize)
    of
	{Is,TrimInstr} ->
            %% One of the recipes was applied.
	    trim(Is, St, reverse(TrimInstr)++Acc)
    catch
	not_possible ->
            %% No recipe worked out. Use the original kill
            %% instructions.
	    trim(Is1, St, reverse(Kills, Acc))
    end;
trim([I|Is], St, Acc) ->
    trim(Is, St, [I|Acc]);
trim([], _, Acc) ->
    reverse(Acc).

%% trim_recipes([{kill,R}|{live,R}|{dead,R}]) -> [Recipe].
%%      Recipe = {Kills,NumberToTrim,Moves}
%%      Kills = [{kill,Y}]
%%      Moves = [{move,SrcY,DstY}]
%%
%%  Calculate how to best trim the stack and kill the correct
%%  Y registers. Return a list of possible recipes. The best
%%  recipe (the one that trims the most) is first in the list.
%%  All of the recipes are no worse in estimated execution time
%%  than the original sequences of kill instructions.

trim_recipes(Layout) ->
    Cost = length([I || {kill,_}=I <- Layout]),
    trim_recipes_1(Layout, 0, [], {Cost,[]}).

trim_recipes_1([{kill,{y,Trim0}}|Ks], Trim0, Moves, Recipes0) ->
    Trim = Trim0 + 1,
    Recipes = save_recipe(Ks, Trim, Moves, Recipes0),
    trim_recipes_1(Ks, Trim, Moves, Recipes);
trim_recipes_1([{dead,{y,Trim0}}|Ks], Trim0, Moves, Recipes0) ->
    Trim = Trim0 + 1,
    Recipes = save_recipe(Ks, Trim, Moves, Recipes0),
    trim_recipes_1(Ks, Trim, Moves, Recipes);
trim_recipes_1([{live,{y,Trim0}=Src}|Ks0], Trim0, Moves0, Recipes0) ->
    case take_last_dead(Ks0) of
	none ->
            {_,RecipesList} = Recipes0,
            RecipesList;
	{Dst,Ks} ->
	    Trim = Trim0 + 1,
	    Moves = [{move,Src,Dst}|Moves0],
            Recipes = save_recipe(Ks, Trim, Moves, Recipes0),
            trim_recipes_1(Ks, Trim, Moves, Recipes)
    end;
trim_recipes_1([], _, _, {_,RecipesList}) ->
    RecipesList.

take_last_dead(L) ->
    take_last_dead_1(reverse(L)).

take_last_dead_1([{kill,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1([{dead,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1(_) -> none.

save_recipe(Ks, Trim, Moves, {MaxCost,Acc}=Recipes) ->
    case recipe_cost(Ks, Moves) of
        Cost when Cost =< MaxCost ->
            %% The price is right.
            {MaxCost,[{Ks,Trim,Moves}|Acc]};
	_Cost ->
            %% Too expensive.
            Recipes
    end.

recipe_cost(Ks, Moves) ->
    %% We estimate that a {move,{y,_},{y,_}} instruction is roughly twice as
    %% expensive as a {kill,{y,_}} instruction. A {trim,_} instruction is
    %% roughly as expensive as a {kill,{y,_}} instruction.

    recipe_cost_1(Ks, 1+2*length(Moves)).

recipe_cost_1([{kill,_}|Ks], Cost) ->
    recipe_cost_1(Ks, Cost+1);
recipe_cost_1([_|Ks], Cost) ->
    recipe_cost_1(Ks, Cost);
recipe_cost_1([], Cost) -> Cost.

%% try_remap([Recipe], [Instruction], FrameSize) ->
%%           {[Instruction],[TrimInstruction]}.
%%  Try to renumber Y registers in the instruction stream. The
%%  first rececipe that works will be used.
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
    Kills = [Kill || {kill,_}=Kill <- Layout],
    {Kills++reverse(Moves, [{trim,Trim,FrameSize-Trim}]),create_map(Trim, Moves)}.

create_map(Trim, []) ->
    fun({y,Y}) when Y < Trim -> throw(not_possible);
       ({y,Y}) -> {y,Y-Trim};
       ({frame_size,N}) -> N - Trim;
       (Any) -> Any
    end;
create_map(Trim, Moves) ->
    Map0 = [{Src,Dst-Trim} || {move,{y,Src},{y,Dst}} <- Moves],
    Map = maps:from_list(Map0),
    IllegalTargets = cerl_sets:from_list([Dst || {move,_,{y,Dst}} <- Moves]),
    fun({y,Y0}) when Y0 < Trim ->
            case Map of
                #{Y0:=Y} -> {y,Y};
                #{} -> throw(not_possible)
            end;
       ({y,Y}) ->
	    case cerl_sets:is_element(Y, IllegalTargets) of
		true -> throw(not_possible);
		false -> {y,Y-Trim}
	    end;
       ({frame_size,N}) -> N - Trim;
       (Any) -> Any
    end.

remap([{'%',_}=I|Is], Map, Acc) ->
    remap(Is, Map, [I|Acc]);
remap([{block,Bl0}|Is], Map, Acc) ->
    Bl = remap_block(Bl0, Map, []),
    remap(Is, Map, [{block,Bl}|Acc]);
remap([{bs_get_tail,Src,Dst,Live}|Is], Map, Acc) ->
    I = {bs_get_tail,Map(Src),Map(Dst),Live},
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
remap([{bs_init,Fail,Info,Live,Ss0,Dst0}|Is], Map, Acc) ->
    Ss = [Map(Src) || Src <- Ss0],
    Dst = Map(Dst0),
    I = {bs_init,Fail,Info,Live,Ss,Dst},
    remap(Is, Map, [I|Acc]);
remap([{bs_put=Op,Fail,Info,Ss}|Is], Map, Acc) ->
    I = {Op,Fail,Info,[Map(S) || S <- Ss]},
    remap(Is, Map, [I|Acc]);
remap([{kill,Y}|T], Map, Acc) ->
    remap(T, Map, [{kill,Map(Y)}|Acc]);
remap([{make_fun2,_,_,_,_}=I|T], Map, Acc) ->
    remap(T, Map, [I|Acc]);
remap([{deallocate,N}|Is], Map, Acc) ->
    I = {deallocate,Map({frame_size,N})},
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
safe_labels([], Acc) -> cerl_sets:from_list(Acc).

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
    IsYreg = fun({y,_}) -> true;
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
frame_size([{call_ext,_,_}=I|Is], Safe) ->
    case beam_jump:is_exit_instruction(I) of
	true -> throw(not_possible);
	false -> frame_size(Is, Safe)
    end;
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
frame_size([{bs_init,{f,L},_,_,_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{bs_put,{f,L},_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{kill,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{make_fun2,_,_,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{get_map_elements,{f,L},_,_}|Is], Safe) ->
    frame_size_branch(L, Is, Safe);
frame_size([{deallocate,N}|_], _) ->
    N;
frame_size([{line,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{bs_set_position,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([{bs_get_tail,_,_,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size(_, _) -> throw(not_possible).

frame_size_branch(0, Is, Safe) ->
    frame_size(Is, Safe);
frame_size_branch(L, Is, Safe) ->
    case cerl_sets:is_element(L, Safe) of
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
is_not_used(Y, [{bs_init,_,_,_,Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is);
is_not_used(Y, [{bs_put,{f,_},_,Ss}|Is]) ->
    not member(Y, Ss) andalso is_not_used(Y, Is);
is_not_used(Y, [{bs_set_position,Src1,Src2}|Is]) ->
    Y =/= Src1 andalso Y =/= Src2 andalso
        is_not_used(Y, Is);
is_not_used(Y, [{call,_,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{call_ext,_,_}=I|Is]) ->
    beam_jump:is_exit_instruction(I) orelse is_not_used(Y, Is);
is_not_used(Y, [{call_fun,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(_Y, [{deallocate,_}|_]) ->
    true;
is_not_used(Y, [{gc_bif,_,{f,_},_Live,Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is);
is_not_used(Y, [{get_map_elements,{f,_},S,{list,List}}|Is]) ->
    {Ss,Ds} = beam_utils:split_even(List),
    case member(Y, [S|Ss]) of
	true ->
	    false;
	false ->
            member(Y, Ds) orelse is_not_used(Y, Is)
    end;
is_not_used(Y, [{kill,Yreg}|Is]) ->
    Y =:= Yreg orelse is_not_used(Y, Is);
is_not_used(Y, [{line,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{make_fun2,_,_,_,_}|Is]) ->
    is_not_used(Y, Is);
is_not_used(Y, [{test,_,_,Ss}|Is]) ->
    not member(Y, Ss) andalso is_not_used(Y, Is);
is_not_used(Y, [{test,_Op,{f,_},_Live,Ss,Dst}|Is]) ->
    is_not_used_ss_dst(Y, Ss, Dst, Is).

is_not_used_block(Y, [{set,Ds,Ss,_}|Is]) ->
    case member(Y, Ss) of
        true ->
            used;
        false ->
            case member(Y, Ds) of
                true ->
                    killed;
                false ->
                    is_not_used_block(Y, Is)
            end
    end;
is_not_used_block(_Y, []) -> transparent.

is_not_used_ss_dst(Y, Ss, Dst, Is) ->
    not member(Y, Ss) andalso (Y =:= Dst orelse is_not_used(Y, Is)).
