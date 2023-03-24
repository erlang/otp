%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
%% This pass implements transforms which allow for safe destructive
%% term updates.
%%
%% The optimization is done in three phases:
%%
%%   * The module is scanned and instructions suitable for
%%     transformation into a destructive form are idenitifed
%%     (find_applicable_instructions/2).
%%
%%   * When instructions are transformed to their destructive form,
%%     their arguments may have to be modified in order to be suitable
%%     for destructive update. For example, a literal <<>> may have to
%%     be converted to a heap allocated term created by a
%%     bs_writable_binary. Identifying such terms and literals is done
%%     by find_initial_values/3.
%%
%%   * Given a set of instructions and literals to patch, the third
%%     phase (patch_instructions/4) traverses the module and performs
%%     the needed modifications.
%%
%% Currently this module implements the following transforms allowing
%% for destructive update:
%%
%%   * Private append
%%
%%     When a binary is grown by appending data to it using
%%     `bs_create_bin`, a considerable performance improvement can be
%%     achieved if the append can be done using the destructive
%%     `private_append` instead of `append`. Using `private_append`
%%     flavor of `bs_create_bin` is only possible when the binary
%%     being extended has been created by `bs_writable_binary` or by
%%     another `bs_create_bin` using `private_append`. As
%%     `private_append` is destructive, an additional requirement is
%%     that there is only one live reference to the binary being being
%%     extended.
%%
%%     This optimization implements a new SSA optimization pass which
%%     finds suitable code sequences which iteratively grow a binaries
%%     starting with `<<>>` and rewrites them to start with an initial
%%     value created by `bs_writable_binary` and use `private_append`
%%     for `bs_create_bin`.
%%

-module(beam_ssa_destructive_update).
-moduledoc false.

-export([opt/2]).

-import(lists, [foldl/3, foldr/3, keysort/2, reverse/1]).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
ff(#b_local{name=#b_literal{val=N},arity=A}) ->
    io_lib:format("~p/~p", [N,A]).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.
opt(StMap, FuncDb) ->
    %% Ignore functions which are not in the function db (never
    %% called).
    Funs = [ F || F <- maps:keys(StMap), is_map_key(F, FuncDb)],

    %% Find instructions to transform from their 'functional' to their
    %% 'destructive' form.
    {Applicable,ValuesToTrack} = find_applicable_instructions(Funs, StMap),
    ?DP("Found applicable instructions:~n~s",
        [[io_lib:format("  in ~s\n~s",
                        [ff(F),
                         [io_lib:format("    ~p: ~p~n", [I,Info])
                          || I:=Info <- PerInstruction]])
          || F:=PerInstruction <- Applicable]]),
    ?DP("Found values to track:~n~s",
        [[io_lib:format("  ~p in ~s: ~p~n", [Var,ff(F),Info])
          || {F,Var,Info} <- ValuesToTrack]]),

    %% Find initial values.
    InitialsToPatch = find_initial_values(ValuesToTrack, StMap, FuncDb),

    %% Patch instructions and initial values.
    patch_instructions(Applicable, InitialsToPatch, StMap, FuncDb).

find_applicable_instructions(Funs, StMap) ->
    fai(Funs, #{}, [], StMap).

fai([F|Funs], Instructions0, Values0, StMap) ->
    #opt_st{ssa=Linear} = map_get(F, StMap),
    {Instructions,Values} = fai_blk(Linear, F, Instructions0, Values0),
    fai(Funs, Instructions, Values, StMap);
fai([], Instructions, Values, _StMap) ->
    {Instructions,Values}.

fai_blk([{_Lbl,#b_blk{is=Is}}|Linear], F, Instructions0, Values0) ->
    {Instructions,Values} = fai_is(Is, F, Instructions0, Values0),
    fai_blk(Linear, F, Instructions, Values);
fai_blk([], _, Instructions, Values) ->
    {Instructions,Values}.

fai_is([I|Is], F, Instructions0, Values0) ->
    {Instructions,Values} = fai_i(I, F, Instructions0, Values0),
    fai_is(Is, F, Instructions, Values);
fai_is([], _F, Instructions, Values) ->
    {Instructions,Values}.

fai_i(#b_set{dst=Dst, op=bs_create_bin,
             args=[#b_literal{val=append},
                   _,
                   Lit=#b_literal{val= <<>>}|_]},
      F, Instructions0, Values) ->
    %% Special case for when the first fragment is a literal <<>> as
    %% it won't be annotated as unique nor will it die with the
    %% instruction.
    Instructions =
        add_applicable_instruction(Dst,
                                   {appendable_binary,Dst,Lit},
                                   F, Instructions0),
    {Instructions,Values};
fai_i(#b_set{dst=Dst, op=bs_create_bin,
             args=[#b_literal{val=append},SegmentInfo,Var|_],
             anno=#{first_fragment_dies:=Dies}=Anno},
      F, Instructions0, Values0) ->
    case Dies andalso is_unique(Var, Anno)
        andalso is_appendable(Anno, SegmentInfo) of
        true ->
            Instructions =
                add_applicable_instruction(Dst,
                                           {appendable_binary,Dst,Var},
                                           F, Instructions0),
            Values = add_tracked_var(Var, init_writable, F, Values0),
            {Instructions,Values};
        false ->
            {Instructions0,Values0}
    end;
fai_i(_I, _F, Instructions, Values) ->
    {Instructions,Values}.

add_applicable_instruction(Instr, Info, F, Instructions0) ->
    PerFun0 = maps:get(F, Instructions0, #{}),
    PerInstruction = maps:get(Instr, PerFun0, []),
    PerFun = PerFun0#{Instr=>[Info|PerInstruction]},
    Instructions0#{F=>PerFun}.

add_tracked_var(Var, Info, F, Vars) ->
    [{F,Var,Info}|Vars].

-record(fiv_st,
        {
         funcdb,
         stmap,
         defsdb = #{},
         literals = #{},
         valuesdb = #{}
        }).

find_initial_values(ValuesToTrack, StMap, FuncDb) ->
    fiv(ValuesToTrack, #fiv_st{funcdb=FuncDb,stmap=StMap}).

fiv([{Fun,Dst,Kind}|Work], FivSt0=#fiv_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    {DefsInFun,FivSt} = fiv_defs_in_fun(Fun, Args, SSA, FivSt0),
    ValuesInFun = fiv_values_in_fun(Fun, FivSt),
    ?DP("*** tracking ~p in: ~s ***~n", [Kind, ff(Fun)]),
    fiv_track_value_in_fun([{Dst,{self,Kind}}], Fun, Work,
                           DefsInFun, ValuesInFun, FivSt);
fiv([{Fun,{track_call_argument,Callee,Element,Idx}}|Work],
    FivSt0=#fiv_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    ?DP("*** Tracking ~p of the ~p:th argument in call to ~s"
        " in the function ~s ***~n", [Element, Idx, ff(Callee), ff(Fun)]),
    {DefsInFun,FivSt1} = fiv_defs_in_fun(Fun, Args, SSA, FivSt0),
    ValuesInFun = fiv_values_in_fun(Fun, FivSt1),
    {Vars,FivSt} =
        fiv_get_call_arguments(Callee, Element, Idx, DefsInFun, Fun, FivSt1),
    ?DP("  Vars to track: ~p~n", [Vars]),
    fiv_track_value_in_fun(Vars, Fun, Work, DefsInFun, ValuesInFun, FivSt);
fiv([{Fun,{track_result,Element}}|Work], FivSt0=#fiv_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    {DefsInFun,FivSt1} = fiv_defs_in_fun(Fun, Args, SSA, FivSt0),
    ValuesInFun = fiv_values_in_fun(Fun, FivSt0),
    ?DP("*** Tracking ~p of the result of ~s ***~n", [Element, ff(Fun)]),
    {Results,FivSt} = fiv_get_results(SSA, Element, Fun, FivSt1),
    ?DP("values to track inside the function: ~p~n", [Results]),
    fiv_track_value_in_fun(Results, Fun, Work, DefsInFun, ValuesInFun, FivSt);
fiv([], FivSt) ->
    FivSt#fiv_st.literals.

patch_instructions(Applicable, InitialsToPatch, StMap0, FuncDb) ->
    ?DP("Instructions to patch:~n  ~p~n", [Applicable]),
    ?DP("Initial values to patch :~n  ~p~n", [InitialsToPatch]),
    %% Merge instructions and initial values so we only get one map
    %% per fuctions which is indexed on the variable.
    Merge =
        fun(A, B) ->
                maps:fold(fun(VarOrLbl, Info0, Acc) ->
                                  Info = Info0++maps:get(VarOrLbl, Acc, []),
                                  Acc#{VarOrLbl => Info}
                          end, A, B)
        end,
    Patches = maps:fold(fun(Fun, Initials, Acc) ->
                                InitialsInFun = Merge(Initials,
                                                      maps:get(Fun, Acc, #{})),
                                Acc#{Fun => InitialsInFun}
                        end, Applicable, InitialsToPatch),
    ?DP("Patches:~n  ~p~n", [Patches]),
    StMap = maps:fold(fun(Fun, Ps, StMapAcc) ->
                              OptSt=#opt_st{ssa=SSA0,cnt=Cnt0} =
                                  map_get(Fun, StMapAcc),
                              {SSA,Cnt} = patch_f(SSA0, Cnt0, Ps),
                              StMapAcc#{Fun => OptSt#opt_st{ssa=SSA,cnt=Cnt}}
                      end, StMap0, Patches),
    {StMap, FuncDb}.

is_unique(Var, Anno) ->
    ordsets:is_element(Var, maps:get(unique, Anno, [])).

is_appendable(Anno, #b_literal{val=[SegmentUnit|_]})
  when is_integer(SegmentUnit) ->
    case Anno of
        #{arg_types:=#{2:=#t_bitstring{appendable=true,size_unit=SizeUnit}}} ->
            SizeUnit rem SegmentUnit == 0;
        _ ->
            false
    end.

%% Find all variables which are returned and return them in a worklist
fiv_get_results(SSA, Element, Fun, FivSt) ->
    fiv_get_results(SSA, [], Element, Fun, FivSt).

fiv_get_results([{_,#b_blk{last=#b_ret{arg=#b_var{}=V}}}|Rest],
                Acc, Element, Fun, FivSt) ->
    fiv_get_results(Rest, [{V,Element}|Acc], Element, Fun, FivSt);
fiv_get_results([{Lbl,#b_blk{last=#b_ret{arg=#b_literal{val=Lit}}}}|Rest],
                Acc, Element, Fun, FivSt0) ->
    %% As tracking doesn't make any attempt to use type information to
    %% exclude execution paths not relevant when tracking an
    %% appendable binary, it can happen that we encounter literals
    %% which do not match the type of the element. We can safely stop
    %% the tracking in that case.
    Continue = case Element of
                   {tuple_element,_,_} ->
                       is_tuple(Lit);
                   {self,init_writable} ->
                       is_bitstring(Lit);
                   {hd,_} ->
                       is_list(Lit) andalso (Lit =/= [])
               end,
    FivSt = if Continue ->
                    fiv_add_literal(Fun, {ret,Lbl,Element}, FivSt0);
               true ->
                    FivSt0
            end,
    fiv_get_results(Rest, Acc, Element, Fun, FivSt);
fiv_get_results([_|Rest], Acc, Element, Fun, FivSt) ->
    fiv_get_results(Rest, Acc, Element, Fun, FivSt);
fiv_get_results([], Acc, _, _Fun, FivSt) ->
    {Acc, FivSt}.

fiv_track_value_in_fun([{#b_var{}=V,Element}|Rest], Fun, Work,
                       Defs, ValuesInFun, FivSt0)
  when is_map_key({V,Element}, ValuesInFun) ->
    %% We have already explored this value.
    ?DP("We have already explored ~p of ~p in ~s~n", [Element, V, ff(Fun)]),
    fiv_track_value_in_fun(Rest, Fun, Work, Defs, ValuesInFun, FivSt0);
fiv_track_value_in_fun([{#b_var{}=V,Element}|Rest], Fun, Work0, Defs,
                       ValuesInFun0, FivSt0=#fiv_st{}) ->
    ?DP("Tracking ~p of ~p in fun ~s~n", [Element, V, ff(Fun)]),
    ValuesInFun = ValuesInFun0#{{V,Element}=>visited},
    case Defs of
        #{V:=#b_set{dst=V,op=Op,args=Args}} ->
            case {Op,Args,Element} of
                {bs_create_bin,[#b_literal{val=append},_,Arg|_],
                 {self,init_writable}} ->
                    ?DP("value is created by append, adding ~p.~n",
                        [{Arg,self}]),
                    fiv_track_value_in_fun([{Arg,Element}|Rest], Fun, Work0,
                                           Defs, ValuesInFun, FivSt0);
                {bs_create_bin,[#b_literal{val=private_append},_,_|_],
                 {self,init_writable}} ->
                    ?DP("value is created by private_append.~n"),
                    %% If the code has already been rewritten to use
                    %% private_append, tracking the accumulator to
                    %% ensure that it is is writable has already
                    %% been seen to, so no need to track it.
                    fiv_track_value_in_fun(Rest, Fun, Work0,
                                           Defs, ValuesInFun, FivSt0);
                {bs_init_writable,_,{self,init_writable}} ->
                    ?DP("value is created by bs_init_writable.~n"),
                    %% bs_init_writable creates a writable binary, so
                    %% we are done.
                    fiv_track_value_in_fun(Rest, Fun, Work0,
                                           Defs, ValuesInFun, FivSt0);
                {call,[#b_local{}=Callee|_Args],_} ->
                    ?DP("value is created by local call to ~s.~n",
                        [ff(Callee)]),
                    fiv_track_value_into_call(Callee, Element, Fun, Rest, Work0,
                                              Defs, ValuesInFun, FivSt0);
                {call,[#b_remote{mod=#b_literal{val=erlang},
                                 name=#b_literal{val=error},
                                 arity=1}|_Args],_} ->
                    ?DP("value is from non-returning external call.~n"),
                    %% As erlang:error/1 never returns, we shouldn't
                    %% try to track this value.
                    fiv_track_value_in_fun(Rest, Fun, Work0,
                                           Defs, ValuesInFun, FivSt0);
                {get_hd,[List],_} ->
                    %% We only handle the case when the tracked value
                    %% is in the head field of a cons. This is due to
                    %% the type analyser always assuming that a cons
                    %% is part of a list and therefore we will never
                    %% be able to safely rewrite an accumulator in the
                    %% tail field of the cons, thus we will never
                    %% have to track it.
                    ?DP("value is created by a get_hd, adding ~p.~n",
                        [{List,{hd,Element}}]),
                    fiv_track_value_in_fun(
                      [{List,{hd,Element}}|Rest], Fun, Work0,
                      Defs, ValuesInFun, FivSt0);
                {get_tuple_element,[#b_var{}=Tuple,#b_literal{val=Idx}],_} ->
                    ?DP("value is created by a get_tuple_element, adding ~p.~n",
                        [{Tuple,{tuple_element,Idx,Element}}]),
                    fiv_track_value_in_fun(
                      [{Tuple,{tuple_element,Idx,Element}}|Rest], Fun, Work0,
                      Defs, ValuesInFun, FivSt0);
                {phi,_,_} ->
                    ?DP("value is created by a phi~n"),
                    {ToExplore,FivSt} = fiv_handle_phi(Fun, V, Args,
                                                       Element, FivSt0),
                    fiv_track_value_in_fun(ToExplore ++ Rest, Fun, Work0,
                                           Defs, ValuesInFun, FivSt);
                {put_tuple,_,_} ->
                    ?DP("value is created by a put tuple.~n"),
                    fiv_track_put_tuple(Args, Element, Rest, Fun, V, Work0,
                                        Defs, ValuesInFun, FivSt0);
                {put_list,_,_} ->
                    ?DP("value is created by a put list.~n"),
                    fiv_track_put_list(Args, Element, Rest, Fun, V, Work0,
                                       Defs, ValuesInFun, FivSt0);
                {_,_,_} ->
                    %% Above we have handled all operations through
                    %% which we are able to track the value to its
                    %% construction. All other operations are from
                    %% execution paths not reachable when the actual
                    %% type (at runtime) is a relevant bitstring.
                    %% Thus we can safely abort the tracking here.
                    %%
                    %% Note: That the bif element/2 is not handled is
                    %% not an omission. When the element index is
                    %% statically known, the CSE pass will convert the
                    %% bif to the instruction get_tuple_element. When
                    %% not known, the default action is correct. The
                    %% same reasoning applies to hd, tl, and map_get.
                    ?DP("value is created by unknown instruction.~n"),
                    fiv_track_value_in_fun(Rest, Fun, Work0,
                                           Defs, ValuesInFun, FivSt0)
            end;
        #{V:={arg,Idx}} ->
            ?DP("value is function argument.~n"),
            fiv_track_value_into_caller(Element, Idx, Rest, Fun, Work0, Defs,
                                        ValuesInFun, FivSt0)
    end;
fiv_track_value_in_fun([{#b_literal{},_}|Rest], Fun, Work,
                       Defs, ValuesInFun, FivSt) ->
    fiv_track_value_in_fun(Rest, Fun, Work, Defs, ValuesInFun, FivSt);
fiv_track_value_in_fun([], Fun, Work, _Defs, ValuesInFun,
                       FivSt0=#fiv_st{valuesdb=ValuesDb0}) ->
    %% We are done with this function. Store the result in the
    %% valuesdb and continue with the work list.
    FivSt = FivSt0#fiv_st{valuesdb=ValuesDb0#{Fun=>ValuesInFun}},
    fiv(Work, FivSt).

fiv_track_value_into_call(Callee, Element, CallerFun, CallerWork, GlobalWork0,
                          CallerDefs, CallerValuesInFun, FivSt0) ->
    GlobalWork = [{Callee, {track_result, Element}}|GlobalWork0],
    fiv_track_value_in_fun(CallerWork, CallerFun, GlobalWork,
                           CallerDefs, CallerValuesInFun, FivSt0).

fiv_track_value_into_caller(Element, ArgIdx,
                            CalledFunWorklist, CalledFun,
                            GlobalWorklist0,
                            CalledFunDefs, CalledFunValues,
                            FivSt0=#fiv_st{funcdb=FuncDb,stmap=StMap}) ->
    #func_info{in=Callers} = map_get(CalledFun, FuncDb),
    ?DP("Track into callers of ~s, tracking arg-idx:~p, ~p~n  callers:~s~n",
        [ff(CalledFun), ArgIdx, Element,
         string:join([ff(C) || C <- Callers],", ")]),
    %% The caller information in func_info does not remove a caller
    %% when it is inlined into another function (although the new
    %% caller is added), so we have to filter out functions which lack
    %% entries in the st_map (as they are dead, they have been removed
    %% from the stmap).
    Work = [{Caller,{track_call_argument,CalledFun,Element,ArgIdx}}
            || Caller <- Callers, is_map_key(Caller, StMap)],
    GlobalWorklist = Work ++ GlobalWorklist0,
    fiv_track_value_in_fun(CalledFunWorklist, CalledFun, GlobalWorklist,
                           CalledFunDefs, CalledFunValues, FivSt0).

fiv_track_put_tuple(FieldVars, {tuple_element,Idx,Element},
                    Work, Fun, Dst, GlobalWork,
                    Defs, ValuesInFun, FivSt0) ->
    %% The value we are tracking was constructed by a put tuple and we
    %% are interested in continuing the tracking of the field
    case lists:nth(Idx + 1, FieldVars) of
        ToTrack = #b_var{} ->
            fiv_track_value_in_fun([{ToTrack,Element}|Work], Fun, GlobalWork,
                                   Defs, ValuesInFun, FivSt0);
        #b_literal{val=Lit} ->
            FivSt = fiv_add_literal(Fun, {opargs,Dst,Idx,Lit,Element}, FivSt0),
            fiv_track_value_in_fun(Work, Fun, GlobalWork,
                                   Defs, ValuesInFun, FivSt)
    end;
fiv_track_put_tuple(_FieldVars, _,
                    Work, Fun, _Dst, GlobalWork,
                    Defs, ValuesInFun, DefSt) ->
    %% As the tracked element isn't a tuple element, this is an
    %% execution path which isn't type compatible, stop tracking.
    fiv_track_value_in_fun(Work, Fun, GlobalWork,
                           Defs, ValuesInFun, DefSt).

fiv_track_put_list([Hd,_Tl], {hd,Element},
                   Work, Fun, Dst, GlobalWork,
                   Defs, ValuesInFun, FivSt0) ->
    %% The value we are tracking was constructed by a put list and we
    %% are interested in continuing the tracking of the field. We only
    %% handle the case when the tracked value is in the head field of
    %% a cons. This is due to the type analyser always assuming that a
    %% cons is part of a list and therefore we will never be able to
    %% safely rewrite an accumulator in the tail field of the cons,
    %% thus we will never have to track it.
    case Hd of
        #b_var{} ->
            fiv_track_value_in_fun([{Hd,Element}|Work], Fun, GlobalWork,
                                   Defs, ValuesInFun, FivSt0);
        #b_literal{val=Lit} ->
            FivSt = fiv_add_literal(Fun, {opargs,Dst,0,Lit,Element}, FivSt0),
            fiv_track_value_in_fun(Work, Fun, GlobalWork, Defs,
                                   ValuesInFun, FivSt)
    end;
fiv_track_put_list([_Hd,_Tl], _, Work, Fun, _Dst, GlobalWork,
                   Defs, ValuesInFun, DefSt) ->
    %% As the tracked element isn't a list element, this is an
    %% execution path which isn't type compatible, stop tracking.
    fiv_track_value_in_fun(Work, Fun, GlobalWork, Defs, ValuesInFun, DefSt).

%% Find all calls to Callee and produce a work-list containing all
%% values which are used as the Idx:th argument.
fiv_get_call_arguments(Callee, Element, Idx, Defs, Fun, FivSt0) ->
    %% We traverse all defs inside the caller to find the calls.
    maps:fold(fun(_, #b_set{dst=Dst,op=call,args=[Target|Args]}, {Acc,FivSt})
                    when Callee =:= Target ->
                      {Values,FivSt1} =
                          fiv_gca(Args, Element, Idx, Fun, Dst, FivSt),
                      {Values ++ Acc, FivSt1};
                 (_, _, Acc) ->
                      Acc
              end, {[], FivSt0}, Defs).

fiv_gca(Args, Element, Idx, Fun, Dst, FivSt) ->
    fiv_gca(Args, 0, Element, Idx, Fun, Dst, FivSt).

fiv_gca([#b_var{}=V|_], I, Element, I, _Fun, _Dst, FivSt) ->
    %% This is the argument we are tracking.
    {[{V,Element}], FivSt};
fiv_gca([#b_literal{val=Lit}|_], I, {self,init_writable}, I, _Fun, _Dst, FivSt)
  when not is_bitstring(Lit)->
    %% As value tracking is done without type information, we can
    %% follow def chains which don't terminate in a bitstring. This is
    %% harmless, but we should ignore them and not, later on, try to
    %% patch them to a bs_writable_binary.
    {[], FivSt};
fiv_gca([#b_literal{val=Lit}|_], I, Element, I, Fun, Dst, FivSt) ->
    {[], fiv_add_literal(Fun, {opargs,Dst,I+1,Lit,Element}, FivSt)};
fiv_gca([_|Args], I, Element, Idx, Fun, Dst, FivSt) ->
    fiv_gca(Args, I + 1, Element, Idx, Fun, Dst, FivSt).

fiv_handle_phi(Fun, Dst, Args, Element, FivSt0) ->
    foldl(fun({#b_literal{val=Lit},Lbl}, {Acc,FivStAcc0}) ->
                  FivStAcc =
                      fiv_add_literal(Fun, {phi,Dst,Lbl,Lit,Element},
                                      FivStAcc0),
                  {Acc, FivStAcc};
             ({V=#b_var{},_Lbl}, {Acc,FivStAcc}) ->
                  ?DP("will explore ~p: ~p~n", [V,Element]),
                  {[{V,Element}|Acc],FivStAcc}
          end, {[],FivSt0}, Args).

%% Cache calculation of the defs for a function so we only do it once.
fiv_defs_in_fun(Fun, Args, SSA, FivSt=#fiv_st{defsdb=DefsDb}) ->
    case DefsDb of
        #{Fun:=Defs} ->
            {Defs, FivSt};
        #{} ->
            BlockMap = maps:from_list(SSA),
            Labels = maps:keys(BlockMap),
            Defs0 = beam_ssa:definitions(Labels, BlockMap),
            {Defs,_} = foldl(fun(Arg, {Acc,Idx}) ->
                                     {Acc#{Arg => {arg,Idx}}, Idx + 1}
                             end, {Defs0,0}, Args),
            {Defs, FivSt#fiv_st{defsdb=DefsDb#{Fun=>Defs}}}
    end.

%% Look up what we know about the values in Fun.
fiv_values_in_fun(Fun, #fiv_st{valuesdb=ValuesDb}) ->
    maps:get(Fun, ValuesDb, #{}).

fiv_add_literal(Fun, LitInfo, FivSt=#fiv_st{literals=Ls}) ->
    PerFun0 = maps:get(Fun, Ls, #{}),
    Key = element(2, LitInfo),
    PerKey = maps:get(Key, PerFun0, []),
    %% We only want to add the same literal once.
    ?DP("~s literal ~p in ~s~n",
        [case ordsets:is_element(LitInfo, PerKey) of
             true ->
                 "Ignoring already tracked";
             false ->
                 "Adding"
         end,LitInfo,ff(Fun)]),
    PerFun = PerFun0#{Key => ordsets:add_element(LitInfo, PerKey)},
    FivSt#fiv_st{literals=Ls#{Fun => PerFun}}.

patch_f(SSA0, Cnt0, Patches) ->
    patch_f(SSA0, Cnt0, Patches, [], []).

patch_f([{Lbl,Blk=#b_blk{is=Is0,last=Last0}}|Rest],
        Cnt0, PD0, Acc0, BlockAdditions0) ->
    {Last,Extra,Cnt2,PD} =
        case PD0 of
            #{ Lbl := Patches } ->
                {Last1,Extra0,Cnt1} = patch_ret(Last0, Patches, Cnt0),
                {Last1,reverse(Extra0),Cnt1,maps:remove(Lbl, PD0)};
            #{} ->
                {Last0,[],Cnt0,PD0}
        end,
    {Is,Cnt,BlockAdditions} = patch_is(Is0, PD, Cnt2, [], []),
    Acc = [{Lbl,Blk#b_blk{is=Is++Extra,last=Last}}|Acc0],
    patch_f(Rest, Cnt, PD, Acc, BlockAdditions++BlockAdditions0);
patch_f([], Cnt, _PD, Acc, BlockAdditions) ->
    ?DP("BlockAdditions: ~p~n", [BlockAdditions]),
    Linear = insert_block_additions(Acc, maps:from_list(BlockAdditions), []),
    ?DP("SSA-result:~n~p~n", [Linear]),
    {Linear, Cnt}.

patch_is([I0=#b_set{dst=Dst}|Rest], PD0, Cnt0, Acc, BlockAdditions0)
  when is_map_key(Dst, PD0) ->
    #{ Dst := Patches } = PD0,
    PD = maps:remove(Dst, PD0),
    case Patches of
        [{opargs,Dst,_,_,_}|_] ->
            OpArgs = [{Idx,Lit,Element}
                      || {opargs,D,Idx,Lit,Element} <- Patches, Dst =:= D],
            Ps = keysort(1, OpArgs),
            {Is,Cnt} = patch_opargs(I0, Ps, Cnt0),
            patch_is(Rest, PD, Cnt, Is++Acc, BlockAdditions0);
        [{appendable_binary,Dst,#b_literal{val= <<>>}=Lit}] ->
            %% Special case for when the first fragment is a literal
            %% <<>> and it has to be replaced with a bs_init_writable.
            #b_set{op=bs_create_bin,dst=Dst,args=Args0}=I0,
            [#b_literal{val=append},SegInfo,Lit|OtherArgs] = Args0,
            {V,Cnt} = new_var(Cnt0),
            Init = #b_set{op=bs_init_writable,dst=V,args=[#b_literal{val=256}]},
            I = I0#b_set{args=[#b_literal{val=private_append},
                               SegInfo,V|OtherArgs]},
            patch_is(Rest, PD, Cnt, [I,Init|Acc], BlockAdditions0);
        [{appendable_binary,Dst,_}] ->
            #b_set{op=bs_create_bin,dst=Dst,args=Args0}=I0,
            [#b_literal{val=append}|OtherArgs] = Args0,
            I = I0#b_set{args=[#b_literal{val=private_append}|OtherArgs]},
            patch_is(Rest, PD, Cnt0, [I|Acc], BlockAdditions0);
        [{phi,Dst,_,_,_}|_] ->
            {I, Extra, Cnt} = patch_phi(I0, Patches, Cnt0),
            patch_is(Rest, PD, Cnt, [I|Acc], Extra++BlockAdditions0)
    end;
patch_is([I|Rest], PD, Cnt, Acc, BlockAdditions) ->
    patch_is(Rest, PD, Cnt, [I|Acc], BlockAdditions);
patch_is([], _, Cnt, Acc, BlockAdditions) ->
    {reverse(Acc), Cnt, BlockAdditions}.

%% The only time when we patch a return is when it returns a
%% literal.
patch_ret(Last=#b_ret{arg=#b_literal{val=Lit}}, Patches, Cnt0)
  when is_list(Lit); is_tuple(Lit) ->
    Ps = keysort(1, [E || {ret,_,E} <- Patches]),
    ?DP("patch_appends_ret tuple or list :~n  lit: ~p~n  patches: ~p~n",
        [Lit, Ps]),
    {V,Extra,Cnt} = patch_literal_term(Lit, Ps, Cnt0),
    {Last#b_ret{arg=V}, Extra, Cnt};
patch_ret(Last=#b_ret{arg=#b_literal{val=Lit}}, [{ret,_,Element}], Cnt0) ->
    ?DP("patch_appends_ret other:~n  lit: ~p~n  element: ~p~n", [Lit, Element]),
    {V,Extra,Cnt} = patch_literal_term(Lit, Element, Cnt0),
    {Last#b_ret{arg=V}, Extra, Cnt}.

%% Should return the instructions in reversed order
patch_opargs(I0=#b_set{args=Args}, Patches0, Cnt0) ->
    ?DP("Patching args in ~p~nArgs: ~p~n Patches: ~p~n",
        [I0,Args,Patches0]),
    Patches = merge_arg_patches(Patches0),
    {PatchedArgs,Is,Cnt} = patch_opargs(Args, Patches, 0, [], [], Cnt0),
    {[I0#b_set{args=reverse(PatchedArgs)}|Is], Cnt}.

patch_opargs([#b_literal{val=Lit}|Args], [{Idx,Lit,Element}|Patches],
             Idx, PatchedArgs, Is, Cnt0) ->
    ?DP("Patching arg idx ~p~n  lit: ~p~n  elem: ~p~n", [Idx,Lit,Element]),
    {Arg,Extra,Cnt} = patch_literal_term(Lit, Element, Cnt0),
    patch_opargs(Args, Patches, Idx + 1, [Arg|PatchedArgs], Extra++Is, Cnt);
patch_opargs([Arg|Args], Patches, Idx, PatchedArgs, Is, Cnt) ->
    ?DP("Skipping arg idx ~p~n  arg: ~p~n  patches: ~p~n", [Idx,Arg,Patches]),
    patch_opargs(Args, Patches, Idx + 1, [Arg|PatchedArgs], Is, Cnt);
patch_opargs([], [], _, PatchedArgs, Is, Cnt) ->
    {PatchedArgs, Is, Cnt}.

%% The way find_initial_values work, we can end up with multiple
%% patches patching different parts of a tuple or pair. We merge them
%% here.
merge_arg_patches([{Idx,Lit,P0},{Idx,Lit,P1}|Patches]) ->
    P = case {P0, P1} of
            {{tuple_element,I0,E0},{tuple_element,I1,E1}} ->
                {tuple_elements,[{I0,E0},{I1,E1}]};
            {{tuple_elements,Es},{tuple_element,I,E}} ->
                {tuple_elements,[{I,E}|Es]};
            {_,_} ->
                [P0|merge_arg_patches([P1|Patches])]
        end,
    merge_arg_patches([{Idx,Lit,P}|Patches]);
merge_arg_patches([P|Patches]) ->
    [P|merge_arg_patches(Patches)];
merge_arg_patches([]) ->
    [].

patch_phi(I0=#b_set{op=phi,args=Args0}, Patches, Cnt0) ->
    L2P = foldl(fun(Phi={phi,_,Lbl,_,_}, Acc) ->
                        Acc#{Lbl => Phi}
                end, #{}, Patches),
    {Args, Extra, Cnt} =
        foldr(fun(Arg0={_,Lbl}, {ArgsAcc,ExtraAcc,CntAcc}) ->
                      case L2P of
                          #{Lbl := {phi,_,Lbl,Lit,Element}} ->
                              {Arg,Extra,Cnt1} =
                                  patch_literal_term(Lit, Element, CntAcc),
                              {[{Arg,Lbl}|ArgsAcc],
                               [{Lbl,Extra}|ExtraAcc], Cnt1};
                          _ ->
                              {[Arg0|ArgsAcc], ExtraAcc, CntAcc}
                      end
              end, {[],[],Cnt0}, Args0),
    I = I0#b_set{op=phi,args=Args},
    {I, Extra, Cnt}.

%% Should return the instructions in reversed order
patch_literal_term(Tuple, {tuple_elements,Elems}, Cnt) ->
    Es = [{tuple_element,I,E} || {I,E} <- keysort(1, Elems)],
    patch_literal_tuple(Tuple, Es, Cnt);
patch_literal_term(Tuple, Elements0, Cnt) when is_tuple(Tuple) ->
    Elements = if is_list(Elements0) -> Elements0;
                  true -> [Elements0]
               end,
    patch_literal_tuple(Tuple, Elements, Cnt);
patch_literal_term(<<>>, {self,init_writable}, Cnt0) ->
    {V,Cnt} = new_var(Cnt0),
    I = #b_set{op=bs_init_writable,dst=V,args=[#b_literal{val=256}]},
    {V,[I],Cnt};
patch_literal_term(Lit, {self,_}, Cnt) ->
    {#b_literal{val=Lit}, [], Cnt};
patch_literal_term([H0|T0], {hd,Element}, Cnt0) ->
    {H,Extra,Cnt1} = patch_literal_term(H0, Element, Cnt0),
    {T,[],Cnt1} = patch_literal_term(T0, [], Cnt1),
    {Dst,Cnt} = new_var(Cnt1),
    I = #b_set{op=put_list,dst=Dst,args=[H,T]},
    {Dst, [I|Extra], Cnt};
patch_literal_term([_|_]=Pair, Elems, Cnt) when is_list(Elems) ->
    [Elem] = [E || {hd,_}=E <- Elems],
    patch_literal_term(Pair, Elem, Cnt);
patch_literal_term(Lit, [], Cnt) ->
    {#b_literal{val=Lit}, [], Cnt}.

patch_literal_tuple(Tuple, Elements0, Cnt) ->
    ?DP("Will patch literal tuple~n  tuple:~p~n  elements: ~p~n",
        [Tuple,Elements0]),
    Elements = [ E || {tuple_element,_,_}=E <- Elements0],
    patch_literal_tuple(erlang:tuple_to_list(Tuple), Elements, [], [], 0, Cnt).

patch_literal_tuple([Lit|LitElements], [{tuple_element,Idx,Element}|Elements],
                    Patched, Extra, Idx, Cnt0) ->
    ?DP("patch_literal_tuple: idx:~p~n  Lit: ~p~n  patch: ~p~n",
        [Idx, Lit, Element]),
    {V,Exs,Cnt} = patch_literal_term(Lit, Element, Cnt0),
    patch_literal_tuple(LitElements, Elements, [V|Patched],
                        Exs ++ Extra, Idx + 1, Cnt);
patch_literal_tuple([Lit|LitElements], Patches, Patched, Extra, Idx, Cnt) ->
    ?DP("patch_literal_tuple: skipping idx:~p~n  Lit: ~p~n  patches: ~p~n", [Idx, Lit, Patches]),
    {T,[],Cnt} = patch_literal_term(Lit, [], Cnt),
    patch_literal_tuple(LitElements, Patches, [T|Patched], Extra, Idx + 1, Cnt);
patch_literal_tuple([], [], Patched, Extra, _, Cnt0) ->
    {V,Cnt} = new_var(Cnt0),
    I = #b_set{op=put_tuple,dst=V,args=reverse(Patched)},
    {V,[I|Extra],Cnt}.

new_var(Count) ->
    {#b_var{name=Count},Count+1}.

%% Done with an accumulator to reverse the reversed block order from
%% patch_appends_f/5.
insert_block_additions([Blk0={L,B=#b_blk{is=Is0}}|RevLinear],
                       Lbl2Addition, Acc) ->
    Blk = case Lbl2Addition of
              #{ L := Additions} ->
                  Is = Is0 ++ reverse(Additions),
                  {L,B#b_blk{is=Is}};
              _ ->
                  Blk0
          end,
    insert_block_additions(RevLinear, Lbl2Addition, [Blk|Acc]);
insert_block_additions([], _, Acc) ->
    Acc.
