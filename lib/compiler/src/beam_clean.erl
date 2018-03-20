%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose : Clean up, such as removing unused labels and unused functions.

-module(beam_clean).

-export([module/2]).
-export([bs_clean_saves/1]).
-export([clean_labels/1]).
-import(lists, [foldl/3,reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,_}, Opts) ->
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs0],
    All = foldl(fun({function,_,_,Lbl,_}=Func,D) -> dict:store(Lbl, Func, D) end,
		dict:new(), Fs0),
    WorkList = rootset(Fs0, Exp, Attr),
    Used = find_all_used(WorkList, All, sets:from_list(WorkList)),
    Fs1 = remove_unused(Order, Used, All),
    {Fs2,Lc} = clean_labels(Fs1),
    Fs3 = bs_fix(Fs2),
    Fs = maybe_remove_lines(Fs3, Opts),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% Remove all bs_save2/2 instructions not referenced by a bs_restore2/2.

-spec bs_clean_saves([beam_utils:instruction()]) ->
                            [beam_utils:instruction()].

bs_clean_saves(Is) ->
    Needed = bs_restores(Is, []),
    bs_clean_saves_1(Is, gb_sets:from_list(Needed), []).

%% Determine the rootset, i.e. exported functions and
%% the on_load function (if any).

rootset(Fs, Root0, Attr) ->
    Root1 = case proplists:get_value(on_load, Attr) of
		undefined -> Root0;
		[OnLoad] -> [OnLoad|Root0]
	   end,
    Root = sofs:set(Root1, [function]),
    Map0 = [{{Name,Arity},Lbl} || {function,Name,Arity,Lbl,_} <- Fs],
    Map = sofs:relation(Map0, [{function,label}]),
    sofs:to_external(sofs:image(Map, Root)).

%% Remove the unused functions.

remove_unused([F|Fs], Used, All) ->
    case sets:is_element(F, Used) of
	false -> remove_unused(Fs, Used, All);
	true -> [dict:fetch(F, All)|remove_unused(Fs, Used, All)]
    end;
remove_unused([], _, _) -> [].
	    
%% Find all used functions.

find_all_used([F|Fs0], All, Used0) ->
    {function,_,_,_,Code} = dict:fetch(F, All),
    {Fs,Used} = update_work_list(Code, {Fs0,Used0}),
    find_all_used(Fs, All, Used);
find_all_used([], _All, Used) -> Used.

update_work_list([{call,_,{f,L}}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{make_fun2,{f,L},_,_,_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([_|Is], Sets) ->
    update_work_list(Is, Sets);
update_work_list([], Sets) -> Sets.

add_to_work_list(F, {Fs,Used}=Sets) ->
    case sets:is_element(F, Used) of
	true -> Sets;
	false -> {[F|Fs],sets:add_element(F, Used)}
    end.


%%%
%%% Coalesce adjacent labels. Renumber all labels to eliminate gaps.
%%% This cleanup will slightly reduce file size and slightly speed up loading.
%%%
%%% We also expand is_record/3 to a sequence of instructions. It is done
%%% here merely because this module will always be called even if optimization
%%% is turned off. We don't want to do the expansion in beam_asm because we
%%% want to see the expanded code in a .S file.
%%%

-type label() :: beam_asm:label().

-record(st, {lmap :: [{label(),label()}], %Translation tables for labels.
	     entry :: beam_asm:label(),   %Number of entry label.
	     lc :: non_neg_integer()      %Label counter
	     }).

-spec clean_labels([beam_utils:instruction()]) ->
                          {[beam_utils:instruction()],pos_integer()}.

clean_labels(Fs0) ->
    St0 = #st{lmap=[],entry=1,lc=1},
    {Fs1,#st{lmap=Lmap0,lc=Lc}} = function_renumber(Fs0, St0, []),
    Lmap = maps:from_list(Lmap0),
    Fs = function_replace(Fs1, Lmap, []),
    {Fs,Lc}.

function_renumber([{function,Name,Arity,_Entry,Asm0}|Fs], St0, Acc) ->
    {Asm,St} = renumber_labels(Asm0, [], St0),
    function_renumber(Fs, St, [{function,Name,Arity,St#st.entry,Asm}|Acc]);
function_renumber([], St, Acc) -> {Acc,St}.

renumber_labels([{bif,is_record,{f,_},
		  [Term,{atom,Tag}=TagAtom,{integer,Arity}],Dst}|Is0], Acc, St) ->
    ContLabel = 900000000+2*St#st.lc,
    FailLabel = ContLabel+1,
    Fail = {f,FailLabel},
    Tmp = Dst,
    Is = case is_record_tuple(Term, Tag, Arity) of
	     yes ->
		 [{move,{atom,true},Dst}|Is0];
	     no ->
		 [{move,{atom,false},Dst}|Is0];
	     maybe ->
		 [{test,is_tuple,Fail,[Term]},
		  {test,test_arity,Fail,[Term,Arity]},
		  {get_tuple_element,Term,0,Tmp},
		  {test,is_eq_exact,Fail,[Tmp,TagAtom]},
		  {move,{atom,true},Dst},
		  {jump,{f,ContLabel}},
		  {label,FailLabel},
		  {move,{atom,false},Dst},
		  {jump,{f,ContLabel}},	%Improves optimization by beam_dead.
		  {label,ContLabel}|Is0]
	 end,
    renumber_labels(Is, Acc, St);
renumber_labels([{test,is_record,{f,_}=Fail,
		  [Term,{atom,Tag}=TagAtom,{integer,Arity}]}|Is0], Acc, St) ->
    Tmp = {x,1022},
    Is = case is_record_tuple(Term, Tag, Arity) of
	     yes ->
		 Is0;
	     no ->
		 [{jump,Fail}|Is0];
	     maybe ->
		 [{test,is_tuple,Fail,[Term]},
		  {test,test_arity,Fail,[Term,Arity]},
		  {get_tuple_element,Term,0,Tmp},
		  {test,is_eq_exact,Fail,[Tmp,TagAtom]}|Is0]
	 end,
    renumber_labels(Is, Acc, St);
renumber_labels([{label,Old}|Is], [{label,New}|_]=Acc, #st{lmap=D0}=St) ->
    D = [{Old,New}|D0],
    renumber_labels(Is, Acc, St#st{lmap=D});
renumber_labels([{label,Old}|Is], Acc, St0) ->
    New = St0#st.lc,
    D = [{Old,New}|St0#st.lmap],
    renumber_labels(Is, [{label,New}|Acc], St0#st{lmap=D,lc=New+1});
renumber_labels([{func_info,_,_,_}=Fi|Is], Acc, St0) ->
    renumber_labels(Is, [Fi|Acc], St0#st{entry=St0#st.lc});
renumber_labels([I|Is], Acc, St0) ->
    renumber_labels(Is, [I|Acc], St0);
renumber_labels([], Acc, St) -> {Acc,St}.

is_record_tuple({x,_}, _, _) -> maybe;
is_record_tuple({y,_}, _, _) -> maybe;
is_record_tuple({literal,Tuple}, Tag, Arity)
  when element(1, Tuple) =:= Tag, tuple_size(Tuple) =:= Arity -> yes;
is_record_tuple(_, _, _) -> no.

function_replace([{function,Name,Arity,Entry,Asm0}|Fs], Dict, Acc) ->
    Asm = try
              Fb = fun(Old) -> throw({error,{undefined_label,Old}}) end,
              beam_utils:replace_labels(Asm0, [], Dict, Fb)
	  catch
	      throw:{error,{undefined_label,Lbl}=Reason} ->
		  io:format("Function ~s/~w refers to undefined label ~w\n",
			    [Name,Arity,Lbl]),
		  exit(Reason)
	  end,
    function_replace(Fs, Dict, [{function,Name,Arity,Entry,Asm}|Acc]);
function_replace([], _, Acc) -> Acc.

%%%
%%% Final fixup of bs_start_match2/5,bs_save2/bs_restore2 instructions for
%%% new bit syntax matching (introduced in R11B).
%%%
%%% Pass 1: Scan the code, looking for bs_restore2/2 instructions.
%%%
%%% Pass 2: Update bs_save2/2 and bs_restore/2 instructions. Remove
%%% any bs_save2/2 instruction whose save position are never referenced
%%% by any bs_restore2/2 instruction.
%%%
%%% Note this module can be invoked several times, so we must be careful
%%% not to touch instructions that have already been fixed up.
%%%

bs_fix(Fs) ->
    bs_fix(Fs, []).

bs_fix([{function,Name,Arity,Entry,Asm0}|Fs], Acc) ->
    Asm = bs_function(Asm0),
    bs_fix(Fs, [{function,Name,Arity,Entry,Asm}|Acc]);
bs_fix([], Acc) -> reverse(Acc).

bs_function(Is) ->
    Dict0 = bs_restores(Is, []),
    S0 = sofs:relation(Dict0, [{context,save_point}]),
    S1 = sofs:relation_to_family(S0),
    S = sofs:to_external(S1),
    Dict = make_save_point_dict(S, []),
    bs_replace(Is, Dict, []).

make_save_point_dict([{Ctx,Pts}|T], Acc0) ->
    Acc = make_save_point_dict_1(Pts, Ctx, 0, Acc0),
    make_save_point_dict(T, Acc);
make_save_point_dict([], Acc) ->
    gb_trees:from_orddict(ordsets:from_list(Acc)).

make_save_point_dict_1([H|T], Ctx, I, Acc) ->
    make_save_point_dict_1(T, Ctx, I+1, [{{Ctx,H},I}|Acc]);
make_save_point_dict_1([], Ctx, I, Acc) ->
    [{Ctx,I}|Acc].

%% Pass 1.
bs_restores([{bs_restore2,_,{Same,Same}}|Is], Dict) ->
    %% This save point is special. No explicit save is needed.
    bs_restores(Is, Dict);
bs_restores([{bs_restore2,_,{atom,start}}|Is], Dict) ->
    %% This instruction can occur if "compilation"
    %% started from a .S file.
    bs_restores(Is, Dict);
bs_restores([{bs_restore2,_,{_,_}=SavePoint}|Is], Dict) ->
    bs_restores(Is, [SavePoint|Dict]);
bs_restores([_|Is], Dict) ->
    bs_restores(Is, Dict);
bs_restores([], Dict) -> Dict.
    
%% Pass 2.
bs_replace([{test,bs_start_match2,F,Live,[Src,{context,Ctx}],CtxR}|T], Dict, Acc) ->
    Slots = case gb_trees:lookup(Ctx, Dict) of
		{value,Slots0} -> Slots0;
		none -> 0
	    end,
    I = {test,bs_start_match2,F,Live,[Src,Slots],CtxR},
    bs_replace(T, Dict, [I|Acc]);
bs_replace([{bs_save2,CtxR,{_,_}=SavePoint}|T], Dict, Acc) ->
    case gb_trees:lookup(SavePoint, Dict) of
	{value,N} ->
	    bs_replace(T, Dict, [{bs_save2,CtxR,N}|Acc]);
	none ->
	    bs_replace(T, Dict, Acc)
    end;
bs_replace([{bs_restore2,_,{atom,start}}=I|T], Dict, Acc) ->
    %% This instruction can occur if "compilation"
    %% started from a .S file.
    bs_replace(T, Dict, [I|Acc]);    
bs_replace([{bs_restore2,CtxR,{Same,Same}}|T], Dict, Acc) ->
    %% This save point refers to the point in the binary where the match
    %% started. It has a special name.
    bs_replace(T, Dict, [{bs_restore2,CtxR,{atom,start}}|Acc]);
bs_replace([{bs_restore2,CtxR,{_,_}=SavePoint}|T], Dict, Acc) ->
    N = gb_trees:get(SavePoint, Dict),
    bs_replace(T, Dict, [{bs_restore2,CtxR,N}|Acc]);
bs_replace([I|Is], Dict, Acc) ->
    bs_replace(Is, Dict, [I|Acc]);
bs_replace([], _, Acc) -> reverse(Acc).

bs_clean_saves_1([{bs_save2,_,{_,_}=SavePoint}=I|Is], Needed, Acc) ->
    case gb_sets:is_member(SavePoint, Needed) of
	false -> bs_clean_saves_1(Is, Needed, Acc);
	true -> bs_clean_saves_1(Is, Needed, [I|Acc])
    end;
bs_clean_saves_1([I|Is], Needed, Acc) ->
    bs_clean_saves_1(Is, Needed, [I|Acc]);
bs_clean_saves_1([], _, Acc) -> reverse(Acc).

%%%
%%% Remove line instructions if requested.
%%%

maybe_remove_lines(Fs, Opts) ->
    case proplists:get_bool(no_line_info, Opts) of
	false -> Fs;
	true -> remove_lines(Fs)
    end.

remove_lines([{function,N,A,Lbl,Is0}|T]) ->
    Is = remove_lines_fun(Is0),
    [{function,N,A,Lbl,Is}|remove_lines(T)];
remove_lines([]) -> [].

remove_lines_fun([{line,_}|Is]) ->
    remove_lines_fun(Is);
remove_lines_fun([{block,Bl0}|Is]) ->
    Bl = remove_lines_block(Bl0),
    [{block,Bl}|remove_lines_fun(Is)];
remove_lines_fun([I|Is]) ->
    [I|remove_lines_fun(Is)];
remove_lines_fun([]) -> [].

remove_lines_block([{set,_,_,{line,_}}|Is]) ->
    remove_lines_block(Is);
remove_lines_block([I|Is]) ->
    [I|remove_lines_block(Is)];
remove_lines_block([]) -> [].
