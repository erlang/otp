%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
-export([clean_labels/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,_}, Opts) ->
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs0],
    All = maps:from_list([{Lbl,Func} || {function,_,_,Lbl,_}=Func <- Fs0]),
    WorkList = rootset(Fs0, Exp, Attr),
    Used = find_all_used(WorkList, All, cerl_sets:from_list(WorkList)),
    Fs1 = remove_unused(Order, Used, All),
    {Fs2,Lc} = clean_labels(Fs1),
    Fs = maybe_remove_lines(Fs2, Opts),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

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
    case cerl_sets:is_element(F, Used) of
	false -> remove_unused(Fs, Used, All);
	true -> [map_get(F, All)|remove_unused(Fs, Used, All)]
    end;
remove_unused([], _, _) -> [].

%% Find all used functions.

find_all_used([F|Fs0], All, Used0) ->
    {function,_,_,_,Code} = map_get(F, All),
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
    case cerl_sets:is_element(F, Used) of
	true -> Sets;
	false -> {[F|Fs],cerl_sets:add_element(F, Used)}
    end.


%%%
%%% Coalesce adjacent labels. Renumber all labels to eliminate gaps.
%%% This cleanup will slightly reduce file size and slightly speed up
%%% loading.
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
