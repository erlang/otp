%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: beam_clean.erl,v 1.1 2008/12/17 09:53:41 mikpe Exp $
%%
%% Purpose : Clean up, such as removing unused labels and unused functions.

-module(beam_clean).

-export([module/2]).
-import(lists, [member/2,map/2,foldl/3,mapfoldl/3,reverse/1]).

module({Mod,Exp,Attr,Fs0,_}, _Opt) ->
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs0],
    All = foldl(fun({function,_,_,Lbl,_}=Func,D) -> dict:store(Lbl, Func, D) end,
		dict:new(), Fs0),
    {WorkList,Used0} = exp_to_labels(Fs0, Exp),
    Used = find_all_used(WorkList, All, Used0),
    Fs1 = remove_unused(Order, Used, All),
    {Fs,Lc} = clean_labels(Fs1),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% Convert the export list ({Name,Arity} pairs) to a list of entry labels.

exp_to_labels(Fs, Exp) -> exp_to_labels(Fs, Exp, [], sets:new()).

exp_to_labels([{function,Name,Arity,Lbl,_}|Fs], Exp, Acc, Used) ->
    case member({Name,Arity}, Exp) of
	true -> exp_to_labels(Fs, Exp, [Lbl|Acc], sets:add_element(Lbl, Used));
	false -> exp_to_labels(Fs, Exp, Acc, Used)
    end;
exp_to_labels([], _, Acc, Used) -> {Acc,Used}.

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
update_work_list([{make_fun,{f,L},_,_}|Is], Sets) ->
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
%%% We also expand internal_is_record/3 to a sequence of instructions. It is done
%%% here merely because this module will always be called even if optimization
%%% is turned off. We don't want to do the expansion in beam_asm because we
%%% want to see the expanded code in a .S file.
%%%

-record(st, {lmap,				%Translation tables for labels.
	     entry,				%Number of entry label.
	     lc					%Label counter
	     }).

clean_labels(Fs0) ->
    St0 = #st{lmap=dict:new(),lc=1},
    {Fs1,#st{lmap=Lmap,lc=Lc}} = mapfoldl(fun function_renumber/2, St0, Fs0),
    {map(fun(F) -> function_replace(F, Lmap) end, Fs1),Lc}.

function_renumber({function,Name,Arity,_Entry,Asm0}, St0) ->
    {Asm,St} = renumber_labels(Asm0, [], St0),
    {{function,Name,Arity,St#st.entry,Asm},St}.

renumber_labels([{bif,internal_is_record,{f,_},
		  [Term,Tag,{integer,Arity}],Dst}|Is], Acc, St) ->
    ContLabel = 900000000+2*St#st.lc,
    FailLabel = ContLabel+1,
    Fail = {f,FailLabel},
    Tmp = Dst,
    renumber_labels([{test,is_tuple,Fail,[Term]},
		     {test,test_arity,Fail,[Term,Arity]},
		     {get_tuple_element,Term,0,Tmp},
		     {test,is_eq_exact,Fail,[Tmp,Tag]},
		     {move,{atom,true},Dst},
		     {jump,{f,ContLabel}},
		     {label,FailLabel},
		     {move,{atom,false},Dst},
		     {label,ContLabel}|Is], Acc, St);
renumber_labels([{test,internal_is_record,{f,_}=Fail,
		  [Term,Tag,{integer,Arity}]}|Is], Acc, St) ->
    Tmp = {x,1023},
    case Term of
	{Reg,_} when Reg == x; Reg == y ->
	    renumber_labels([{test,is_tuple,Fail,[Term]},
			     {test,test_arity,Fail,[Term,Arity]},
			     {get_tuple_element,Term,0,Tmp},
			     {test,is_eq_exact,Fail,[Tmp,Tag]}|Is], Acc, St);
	_ ->
	    renumber_labels([{jump,Fail}|Is], Acc, St)
    end;
renumber_labels([{label,Old}|Is], [{label,New}|_]=Acc, #st{lmap=D0}=St) ->
    D = dict:store(Old, New, D0),
    renumber_labels(Is, Acc, St#st{lmap=D});
renumber_labels([{label,Old}|Is], Acc, St0) ->
    New = St0#st.lc,
    D = dict:store(Old, New, St0#st.lmap),
    renumber_labels(Is, [{label,New}|Acc], St0#st{lmap=D,lc=New+1});
renumber_labels([{func_info,_,_,_}=Fi|Is], Acc, St0) ->
    renumber_labels(Is, [Fi|Acc], St0#st{entry=St0#st.lc});
renumber_labels([I|Is], Acc, St0) ->
    renumber_labels(Is, [I|Acc], St0);
renumber_labels([], Acc, St0) -> {Acc,St0}.

function_replace({function,Name,Arity,Entry,Asm0}, Dict) ->
    Asm = case catch replace(Asm0, [], Dict) of
	      {'EXIT',_}=Reason ->
		  exit(Reason);
	      {error,{undefined_label,Lbl}=Reason} ->
		  io:format("Function ~s/~w refers to undefined label ~w\n",
			    [Name,Arity,Lbl]),
		  exit(Reason);
	      Asm1 when list(Asm1) -> Asm1
	  end,
    {function,Name,Arity,Entry,Asm}.

replace([{test,Test,{f,Lbl},Ops}|Is], Acc, D) ->
    replace(Is, [{test,Test,{f,label(Lbl, D)},Ops}|Acc], D);
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
replace([{call,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call,Ar,{f,label(Lbl,D)}}|Acc], D);
replace([{call_last,Ar,{f,Lbl},N}|Is], Acc, D) ->
    replace(Is, [{call_last,Ar,{f,label(Lbl,D)},N}|Acc], D);
replace([{call_only,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call_only,Ar,{f,label(Lbl, D)}}|Acc], D);
replace([{make_fun,{f,Lbl},U1,U2}|Is], Acc, D) ->
    replace(Is, [{make_fun,{f,label(Lbl, D)},U1,U2}|Acc], D);
replace([{make_fun2,{f,Lbl},U1,U2,U3}|Is], Acc, D) ->
    replace(Is, [{make_fun2,{f,label(Lbl, D)},U1,U2,U3}|Acc], D);
replace([{bs_init2,{f,Lbl},Sz,Words,R,F,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_init2,{f,label(Lbl, D)},Sz,Words,R,F,Dst}|Acc], D);
replace([{bs_put_integer,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_integer,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_binary,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_binary,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_float,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_float,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_final,{f,Lbl},R}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_final,{f,label(Lbl, D)},R}|Acc], D);
replace([{bs_add,{f,Lbl},Src,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_add,{f,label(Lbl, D)},Src,Dst}|Acc], D);
replace([{bs_bits_to_bytes,{f,Lbl},Bits,Dst}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_bits_to_bytes,{f,label(Lbl, D)},Bits,Dst}|Acc], D);
replace([I|Is], Acc, D) ->
    replace(Is, [I|Acc], D);
replace([], Acc, _) -> Acc.

label(Old, D) ->
    case dict:find(Old, D) of
	{ok,Val} -> Val;
	error -> throw({error,{undefined_label,Old}})
    end.

redundant_values([_,{f,Fail}|Vls], Fail, Acc) ->
    redundant_values(Vls, Fail, Acc);
redundant_values([Val,Lbl|Vls], Fail, Acc) ->
    redundant_values(Vls, Fail, [Lbl,Val|Acc]);
redundant_values([], _, Acc) -> reverse(Acc).
