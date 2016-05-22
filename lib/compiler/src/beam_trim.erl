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

-import(lists, [reverse/1,reverse/2,splitwith/2,sort/1]).

-record(st,
	{safe,					%Safe labels.
	 lbl					%Code at each label.
	 }).

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    %%ok = io:fwrite("~w: ~p\n", [?LINE,{Name,Arity}]),
    St = #st{safe=safe_labels(Is0, []),lbl=beam_utils:index_labels(Is0)},
    Is = trim(Is0, St, []),
    {function,Name,Arity,CLabel,Is}.

trim([{kill,_}|_]=Is0, St, Acc) ->
    {Kills0,Is1} = splitwith(fun({kill,_}) -> true;
			       (_) -> false
			    end, Is0),
    Kills = sort(Kills0),
    try
	{FrameSize,Layout} = frame_layout(Is1, Kills, St),
	Configs = trim_instructions(Layout),
	try_remap(Configs, Is1, FrameSize)
	of
	{Is,TrimInstr} ->
	    trim(Is, St, reverse(TrimInstr)++Acc)
    catch
	not_possible ->
	    trim(Is1, St, reverse(Kills, Acc))
    end;
trim([I|Is], St, Acc) ->
    trim(Is, St, [I|Acc]);
trim([], _, Acc) ->
    reverse(Acc).

%% trim_instructions([{kill,R}|{live,R}|{dead,R}]) -> {[Instruction],MapFun}
%%  Figure out the sequence of moves and trim to use.

trim_instructions(Layout) ->
    Cost = length([I || {kill,_}=I <- Layout]),
    trim_instructions_1(Layout, 0, [], {Cost,[]}).

trim_instructions_1([{kill,{y,Trim0}}|Ks], Trim0, Moves, Config0) ->
    Trim = Trim0 + 1,
    Config = save_config(Ks, Trim, Moves, Config0),
    trim_instructions_1(Ks, Trim, Moves, Config);
trim_instructions_1([{dead,{y,Trim0}}|Ks], Trim0, Moves, Config0) ->
    Trim = Trim0 + 1,
    Config = save_config(Ks, Trim, Moves, Config0),
    trim_instructions_1(Ks, Trim, Moves, Config);
trim_instructions_1([{live,{y,Trim0}=Src}|Ks0], Trim0, Moves0, Config0) ->
    case take_last_dead(Ks0) of
	none ->
	    {_,ConfigList} = Config0,
	    ConfigList;
	{Dst,Ks} ->
	    Trim = Trim0 + 1,
	    Moves = [{move,Src,Dst}|Moves0],
	    Config = save_config(Ks, Trim, Moves, Config0),
	    trim_instructions_1(Ks, Trim, Moves, Config)
    end;
trim_instructions_1([], _, _, {_,ConfigList}) ->
    ConfigList.

take_last_dead(L) ->
    take_last_dead_1(reverse(L)).

take_last_dead_1([{kill,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1([{dead,Reg}|Is]) ->
    {Reg,reverse(Is)};
take_last_dead_1(_) -> none.

save_config(Ks, Trim, Moves, {MaxCost,Acc}=Config) ->
    case config_cost(Ks, Moves) of
	Cost when Cost =< MaxCost ->
	    {MaxCost,[{Ks,Trim,Moves}|Acc]};
	_Cost ->
	    Config
    end.

config_cost(Ks, Moves) ->
    %% We estimate that a {move,{y,_},{y,_}} instruction is roughly twice as
    %% expensive as a {kill,{y,_}} instruction. A {trim,_} instruction is
    %% roughly as expensive as a {kill,{y,_}} instruction.

    config_cost_1(Ks, 1+2*length(Moves)).

config_cost_1([{kill,_}|Ks], Cost) ->
    config_cost_1(Ks, Cost+1);
config_cost_1([_|Ks], Cost) ->
    config_cost_1(Ks, Cost);
config_cost_1([], Cost) -> Cost.

expand_config({Layout,Trim,Moves}, FrameSize) ->
    Kills = [Kill || {kill,_}=Kill <- Layout],
    {Kills++reverse(Moves, [{trim,Trim,FrameSize-Trim}]),create_map(Trim, Moves)}.

create_map(Trim, []) ->
    fun({y,Y}) when Y < Trim -> throw(not_possible);
       ({y,Y}) -> {y,Y-Trim};
       ({frame_size,N}) -> N - Trim;
       (Any) -> Any
    end;
create_map(Trim, Moves) ->
    GbTree0 = [{Src,Dst-Trim} || {move,{y,Src},{y,Dst}} <- Moves],
    GbTree = gb_trees:from_orddict(sort(GbTree0)),
    IllegalTargets = gb_sets:from_list([Dst || {move,_,{y,Dst}} <- Moves]),
    fun({y,Y0}) when Y0 < Trim ->
	    case gb_trees:lookup(Y0, GbTree) of
		{value,Y} -> {y,Y};
		none -> throw(not_possible)
	    end;
       ({y,Y}) ->
	    case gb_sets:is_element(Y, IllegalTargets) of
		true -> throw(not_possible);
		false -> {y,Y-Trim}
	    end;
       ({frame_size,N}) -> N - Trim;
       (Any) -> Any
    end.

try_remap([C|Cs], Is, FrameSize) ->
    {TrimInstr,Map} = expand_config(C, FrameSize),
    try
	{remap(Is, Map, []),TrimInstr}
    catch
	throw:not_possible ->
	    try_remap(Cs, Is, FrameSize)
    end;
try_remap([], _, _) -> throw(not_possible).

remap([{block,Bl0}|Is], Map, Acc) ->
    Bl = remap_block(Bl0, Map, []),
    remap(Is, Map, [{block,Bl}|Acc]);
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
    
safe_labels([{label,L},{line,_},{badmatch,{Tag,_}}|Is], Acc) when Tag =/= y ->
    safe_labels(Is, [L|Acc]);
safe_labels([{label,L},{line,_},{case_end,{Tag,_}}|Is], Acc) when Tag =/= y ->
    safe_labels(Is, [L|Acc]);
safe_labels([{label,L},{line,_},if_end|Is], Acc) ->
    safe_labels(Is, [L|Acc]);
safe_labels([{label,L},
	     {block,[{set,[{x,0}],[{Tag,_}],move}]},
	     {line,_},
	     {call_ext,1,{extfunc,erlang,error,1}}|Is], Acc) when Tag =/= y ->
    safe_labels(Is, [L|Acc]);
safe_labels([_|Is], Acc) ->
    safe_labels(Is, Acc);
safe_labels([], Acc) -> gb_sets:from_list(Acc).

%% frame_layout([Instruction], [{kill,_}], St) ->
%%      [{kill,Reg} | {live,Reg} | {dead,Reg}]
%%  Figure out the layout of the stack frame.

frame_layout(Is, Kills, #st{safe=Safe,lbl=D}) ->
    N = frame_size(Is, Safe),
    IsKilled = fun(R) -> beam_utils:is_killed(R, Is, D) end,
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
frame_size([{deallocate,N}|_], _) -> N;
frame_size([{line,_}|Is], Safe) ->
    frame_size(Is, Safe);
frame_size([_|_], _) -> throw(not_possible).

frame_size_branch(0, Is, Safe) ->
    frame_size(Is, Safe);
frame_size_branch(L, Is, Safe) ->
    case gb_sets:is_member(L, Safe) of
	false -> throw(not_possible);
	true -> frame_size(Is, Safe)
    end.
