%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% Purpose : Clean up, such as removing unused labels and unused functions.

-module(beam_clean).

-export([module/2]).
-export([bs_clean_saves/1]).
-export([clean_labels/1]).
-import(lists, [map/2,foldl/3,reverse/1]).

module({Mod,Exp,Attr,Fs0,_}, _Opt) ->
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs0],
    All = foldl(fun({function,_,_,Lbl,_}=Func,D) -> dict:store(Lbl, Func, D) end,
		dict:new(), Fs0),
    WorkList = rootset(Fs0, Exp, Attr),
    Used = find_all_used(WorkList, All, sets:from_list(WorkList)),
    Fs1 = remove_unused(Order, Used, All),
    {Fs2,Lc} = clean_labels(Fs1),
    Fs = bs_fix(Fs2),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% Remove all bs_save2/2 instructions not referenced by a bs_restore2/2.
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
update_work_list([{call_last,_,{f,L},_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{call_only,_,{f,L}}|Is], Sets) ->
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

-record(st, {lmap,				%Translation tables for labels.
	     entry,				%Number of entry label.
	     lc					%Label counter
	     }).

clean_labels(Fs0) ->
    St0 = #st{lmap=[],lc=1},
    {Fs1,#st{lmap=Lmap0,lc=Lc}} = function_renumber(Fs0, St0, []),
    Lmap = gb_trees:from_orddict(ordsets:from_list(Lmap0)),
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
    Tmp = {x,1023},
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
	      replace(Asm0, [], Dict)
	  catch
	      throw:{error,{undefined_label,Lbl}=Reason} ->
		  io:format("Function ~s/~w refers to undefined label ~w\n",
			    [Name,Arity,Lbl]),
		  exit(Reason)
	  end,
    function_replace(Fs, Dict, [{function,Name,Arity,Entry,Asm}|Acc]);
function_replace([], _, Acc) -> Acc.

replace([{test,bs_match_string=Op,{f,Lbl},[Ctx,Bin0]}|Is], Acc, D) ->
    Bits = bit_size(Bin0),
    Bin = case Bits rem 8 of
	      0 -> Bin0;
	      Rem -> <<Bin0/bitstring,0:(8-Rem)>>
	  end,
    I = {test,Op,{f,label(Lbl, D)},[Ctx,Bits,{string,binary_to_list(Bin)}]},
    replace(Is, [I|Acc], D);
replace([{test,Test,{f,Lbl},Ops}|Is], Acc, D) ->
    replace(Is, [{test,Test,{f,label(Lbl, D)},Ops}|Acc], D);
replace([{test,Test,{f,Lbl},Live,Ops,Dst}|Is], Acc, D) ->
    replace(Is, [{test,Test,{f,label(Lbl, D)},Live,Ops,Dst}|Acc], D);
replace([{select_val,R,{f,Fail0},{list,Vls0}}|Is], Acc, D) ->
    Vls1 = map(fun ({f,L}) -> {f,label(L, D)};
		   (Other) -> Other end, Vls0),
    Fail = label(Fail0, D),
    case redundant_values(Vls1, Fail, []) of
	[] ->
	    %% Oops, no choices left. The loader will not accept that.
	    %% Convert to a plain jump.
	    replace(Is, [{jump,{f,Fail}}|Acc], D);
	Vls ->
	    replace(Is, [{select_val,R,{f,Fail},{list,Vls}}|Acc], D)
    end;
replace([{select_tuple_arity,R,{f,Fail},{list,Vls0}}|Is], Acc, D) ->
    Vls = map(fun ({f,L}) -> {f,label(L, D)};
		  (Other) -> Other end, Vls0),
    replace(Is, [{select_tuple_arity,R,{f,label(Fail, D)},{list,Vls}}|Acc], D);
replace([{'try',R,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{'try',R,{f,label(Lbl, D)}}|Acc], D);
replace([{'catch',R,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{'catch',R,{f,label(Lbl, D)}}|Acc], D);
replace([{jump,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{jump,{f,label(Lbl, D)}}|Acc], D);
replace([{loop_rec,{f,Lbl},R}|Is], Acc, D) ->
    replace(Is, [{loop_rec,{f,label(Lbl, D)},R}|Acc], D);
replace([{loop_rec_end,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{loop_rec_end,{f,label(Lbl, D)}}|Acc], D);
replace([{wait,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{wait,{f,label(Lbl, D)}}|Acc], D);
replace([{wait_timeout,{f,Lbl},To}|Is], Acc, D) ->
    replace(Is, [{wait_timeout,{f,label(Lbl, D)},To}|Acc], D);
replace([{bif,Name,{f,Lbl},As,R}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bif,Name,{f,label(Lbl, D)},As,R}|Acc], D);
replace([{gc_bif,Name,{f,Lbl},Live,As,R}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{gc_bif,Name,{f,label(Lbl, D)},Live,As,R}|Acc], D);
replace([{call,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call,Ar,{f,label(Lbl,D)}}|Acc], D);
replace([{call_last,Ar,{f,Lbl},N}|Is], Acc, D) ->
    replace(Is, [{call_last,Ar,{f,label(Lbl,D)},N}|Acc], D);
replace([{call_only,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call_only,Ar,{f,label(Lbl, D)}}|Acc], D);
replace([{make_fun2,{f,Lbl},U1,U2,U3}|Is], Acc, D) ->
    replace(Is, [{make_fun2,{f,label(Lbl, D)},U1,U2,U3}|Acc], D);
replace([{bs_init2,{f,Lbl},Sz,Words,R,F,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_init2,{f,label(Lbl, D)},Sz,Words,R,F,Dst}|Acc], D);
replace([{bs_init_bits,{f,Lbl},Sz,Words,R,F,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_init_bits,{f,label(Lbl, D)},Sz,Words,R,F,Dst}|Acc], D);
replace([{bs_put_integer,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_integer,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_utf8=I,{f,Lbl},Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{I,{f,label(Lbl, D)},Fl,Val}|Acc], D);
replace([{bs_put_utf16=I,{f,Lbl},Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{I,{f,label(Lbl, D)},Fl,Val}|Acc], D);
replace([{bs_put_utf32=I,{f,Lbl},Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{I,{f,label(Lbl, D)},Fl,Val}|Acc], D);
replace([{bs_put_binary,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_binary,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_float,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_float,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_add,{f,Lbl},Src,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_add,{f,label(Lbl, D)},Src,Dst}|Acc], D);
replace([{bs_append,{f,Lbl},_,_,_,_,_,_,_}=I0|Is], Acc, D) when Lbl =/= 0 ->
    I = setelement(2, I0, {f,label(Lbl, D)}),
    replace(Is, [I|Acc], D);
replace([{bs_utf8_size=I,{f,Lbl},Src,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{I,{f,label(Lbl, D)},Src,Dst}|Acc], D);
replace([{bs_utf16_size=I,{f,Lbl},Src,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{I,{f,label(Lbl, D)},Src,Dst}|Acc], D);
replace([I|Is], Acc, D) ->
    replace(Is, [I|Acc], D);
replace([], Acc, _) -> Acc.

label(Old, D) ->
    case gb_trees:lookup(Old, D) of
	{value,Val} -> Val;
	none -> throw({error,{undefined_label,Old}})
    end.
	    
redundant_values([_,{f,Fail}|Vls], Fail, Acc) ->
    redundant_values(Vls, Fail, Acc);
redundant_values([Val,Lbl|Vls], Fail, Acc) ->
    redundant_values(Vls, Fail, [Lbl,Val|Acc]);
redundant_values([], _, Acc) -> reverse(Acc).

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
bs_replace([{test,bs_start_match2,F,Live,[Src,Ctx],CtxR}|T], Dict, Acc) when is_atom(Ctx) ->
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
