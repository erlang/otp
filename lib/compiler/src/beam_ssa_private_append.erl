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
%%

%% When a binary is grown by appending data to it using
%% `bs_create_bin`, a considerable performance improvement can be
%% achieved if the append can be done using the destructive
%% `private_append` instead of `append`. Using `private_append` flavor
%% of `bs_create_bin` is only possible when the binary being extended
%% has been created by `bs_writable_binary` or by another
%% `bs_create_bin` using `private_append`. As `private_append` is
%% destructive, an additional requirement is that there is only one
%% live reference to the binary being being extended.

%% This optimization implements a new SSA optimization pass which
%% finds suitable code sequences which iteratively grow a binaries
%% starting with `<<>>` and rewrites them to start with an initial
%% value created by `bs_writable_binary` and use `private_append` for
%% `bs_create_bin`.

-module(beam_ssa_private_append).

-export([opt/2]).

-import(lists, [foldl/3, foldr/3, keysort/2, map/2, reverse/1]).

-include("beam_ssa_opt.hrl").
-include("beam_types.hrl").

%% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DP(FMT, ARGS), io:format(FMT, ARGS)).
-define(DP(FMT), io:format(FMT)).
-else.
-define(DP(FMT, ARGS), skip).
-define(DP(FMT), skip).
-endif.

-spec opt(st_map(), func_info_db()) -> {st_map(), func_info_db()}.
opt(StMap, FuncDb) ->
    %% Ignore functions which are not in the function db (never
    %% called) or are stubs for nifs.
    Funs = [ F || F <- maps:keys(StMap),
                  is_map_key(F, FuncDb), not is_nif(F, StMap)],
    private_append(Funs, StMap, FuncDb).

private_append(Funs, StMap0, FuncDb) ->
    Appends = maps:fold(fun(Fun, As, Acc) ->
                                [{Fun,A} || A <- As] ++ Acc
                        end, [], find_appends(Funs, StMap0, #{})),
    %% We now have to find where we create the binaries in order to
    %% patch them.
    Defs = find_defs(Appends, StMap0, FuncDb),
    StMap = patch_appends(Defs, Appends, StMap0),
    {StMap, FuncDb}.

find_appends([F|Funs], StMap, Found0) ->
    #opt_st{ssa=Linear} = map_get(F, StMap),
    Found = find_appends_blk(Linear, F, Found0),
    find_appends(Funs, StMap, Found);
find_appends([], _, Found) ->
    Found.

find_appends_blk([{_Lbl,#b_blk{is=Is}}|Linear], Fun, Found0) ->
    Found = find_appends_is(Is, Fun, Found0),
    find_appends_blk(Linear, Fun, Found);
find_appends_blk([], _, Found) ->
    Found.

find_appends_is([#b_set{dst=Dst, op=bs_create_bin,
                        args=[#b_literal{val=append},
                              _,
                              Lit=#b_literal{val= <<>>}|_]}|Is],
                Fun, Found0) ->
    %% Special case for when the first fragment is a literal <<>> as
    %% it won't be annotated as unique nor will it die with the
    %% instruction.
    AlreadyFound = maps:get(Fun, Found0, []),
    Found = Found0#{Fun => [{append,Dst,Lit}|AlreadyFound]},
    find_appends_is(Is, Fun, Found);
find_appends_is([#b_set{dst=Dst, op=bs_create_bin,
                        args=[#b_literal{val=append},SegmentInfo,Var|_],
                        anno=#{first_fragment_dies:=Dies}=Anno}|Is],
                Fun, Found0) ->
    case Dies andalso is_unique(Var, Anno)
        andalso is_appendable(Anno, SegmentInfo) of
        true ->
            AlreadyFound = maps:get(Fun, Found0, []),
            Found = Found0#{Fun => [{append,Dst,Var}|AlreadyFound]},
            find_appends_is(Is, Fun, Found);
        false ->
            find_appends_is(Is, Fun, Found0)
    end;
find_appends_is([_|Is], Fun, Found) ->
    find_appends_is(Is, Fun, Found);
find_appends_is([], _, Found) ->
    Found.

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

-record(def_st,
        {
         funcdb,
         stmap,
         defsdb = #{},
         literals = #{},
         valuesdb = #{}
        }).

find_defs(As, StMap, FuncDb) ->
    find_defs_1(As, #def_st{funcdb=FuncDb,stmap=StMap}).

find_defs_1([{Fun,{append,Dst,_Arg}}|Work], DefSt0=#def_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    {DefsInFun,DefSt} = defs_in_fun(Fun, Args, SSA, DefSt0),
    ValuesInFun = values_in_fun(Fun, DefSt),
    ?DP("*** append in: ~p ***~n", [Fun]),
    track_value_in_fun([{Dst,self}], Fun, Work,
                       DefsInFun, ValuesInFun, DefSt);
find_defs_1([{Fun,{track_call_argument,Callee,Element,Idx}}|Work],
            DefSt0=#def_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    ?DP("*** Tracking ~p of the ~p:th argument in call to ~p"
        " in the function ~p ***~n", [Element, Idx, Callee, Fun]),

    {DefsInFun,DefSt1} = defs_in_fun(Fun, Args, SSA, DefSt0),
    ValuesInFun = values_in_fun(Fun, DefSt1),
    {Vars,DefSt} =
        get_call_arguments(Callee, Element, Idx, DefsInFun, Fun, DefSt1),
    ?DP("  Vars to track: ~p~n", [Vars]),
    track_value_in_fun(Vars, Fun, Work, DefsInFun, ValuesInFun, DefSt);
find_defs_1([{Fun,{track_result,Element}}|Work],
            DefSt0=#def_st{stmap=StMap}) ->
    #{Fun:=#opt_st{ssa=SSA,args=Args}} = StMap,
    {DefsInFun,DefSt1} = defs_in_fun(Fun, Args, SSA, DefSt0),
    ValuesInFun = values_in_fun(Fun, DefSt0),
    ?DP("*** Tracking ~p of the result of ~p ***~n",
        [Fun, Element]),
    {Results,DefSt} = get_results(SSA, Element, Fun, DefSt1),
    ?DP("values to track inside the function: ~p~n", [Results]),
    track_value_in_fun(Results, Fun, Work, DefsInFun, ValuesInFun, DefSt);

find_defs_1([], DefSt) ->
    DefSt#def_st.literals.

%% Find all variables which are returned and return them in a worklist
get_results(SSA, Element, Fun, DefSt) ->
    get_results(SSA, [], Element, Fun, DefSt).

get_results([{_,#b_blk{last=#b_ret{arg=#b_var{}=V}}}|Rest],
            Acc, Element, Fun, DefSt) ->
    get_results(Rest, [{V,Element}|Acc], Element, Fun, DefSt);
get_results([{Lbl,#b_blk{last=#b_ret{arg=#b_literal{val=Lit}}}}|Rest],
            Acc, Element, Fun, DefSt0) ->
    %% As tracking doesn't make any attempt to use type information to
    %% exclude execution paths not relevant when tracking an
    %% appendable binary, it can happen that we encounter literals
    %% which do not match the type of the element. We can safely stop
    %% the tracking in that case.
    Continue = case Element of
                   {tuple_element,_,_} ->
                       is_tuple(Lit);
                   self ->
                       is_bitstring(Lit);
                   {hd,_} ->
                       is_list(Lit) andalso (Lit =/= [])
               end,
    DefSt = if Continue ->
                    add_literal(Fun, {ret,Lbl,Element}, DefSt0);
               true ->
                    DefSt0
            end,
    get_results(Rest, Acc, Element, Fun, DefSt);
get_results([_|Rest], Acc, Element, Fun, DefSt) ->
    get_results(Rest, Acc, Element, Fun, DefSt);
get_results([], Acc, _, _Fun, DefSt) ->
    {Acc, DefSt}.

track_value_in_fun([{#b_var{}=V,Element}|Rest], Fun, Work,
                   Defs, ValuesInFun, DefSt0)
  when is_map_key({V,Element}, ValuesInFun) ->
    %% We have already explored this value.
    ?DP("We have already explored ~p of ~p in ~p~n", [Element, V, Fun]),
    track_value_in_fun(Rest, Fun, Work, Defs, ValuesInFun, DefSt0);
track_value_in_fun([{#b_var{}=V,Element}|Rest], Fun, Work0, Defs,
                   ValuesInFun0, DefSt0=#def_st{}) ->
    ?DP("Tracking  ~p of ~p in fun ~p~n", [Element, V, Fun]),
    ValuesInFun = ValuesInFun0#{{V,Element}=>visited},
    case Defs of
        #{V:=#b_set{dst=V,op=Op,args=Args}} ->
            case {Op,Args,Element} of
                {bs_create_bin,[#b_literal{val=append},_,Arg|_],self} ->
                    track_value_in_fun([{Arg,self}|Rest], Fun, Work0,
                                       Defs, ValuesInFun, DefSt0);
                {bs_create_bin,[#b_literal{val=private_append},_,_|_],self} ->
                    %% If the code has already been rewritten to use
                    %% private_append, tracking the accumulator to
                    %% ensure that it is is writable has already
                    %% been seen to, so no need to track it.
                    track_value_in_fun(Rest, Fun, Work0,
                                       Defs, ValuesInFun, DefSt0);
                {bs_init_writable,_,self} ->
                    %% bs_init_writable creates a writable binary, so
                    %% we are done.
                    track_value_in_fun(Rest, Fun, Work0,
                                       Defs, ValuesInFun, DefSt0);
                {call,[#b_local{}=Callee|_Args],_} ->
                    track_value_into_call(Callee, Element, Fun, Rest, Work0,
                                          Defs, ValuesInFun, DefSt0);
                {call,[#b_remote{mod=#b_literal{val=erlang},
                                 name=#b_literal{val=error},
                                 arity=1}|_Args],_} ->
                    %% As erlang:error/1 never returns, we shouldn't
                    %% try to track this value.
                    track_value_in_fun(Rest, Fun, Work0,
                                       Defs, ValuesInFun, DefSt0);
                {get_hd,[List],_} ->
                    %% We only handle the case when the tracked value
                    %% is in the head field of a cons. This is due to
                    %% the type analyser always assuming that a cons
                    %% is part of a list and therefore we will never
                    %% be able to safely rewrite an accumulator in the
                    %% tail field of the cons, thus we will never
                    %% have to track it.
                    track_value_in_fun(
                      [{List,{hd,Element}}|Rest], Fun, Work0,
                      Defs, ValuesInFun, DefSt0);
                {get_tuple_element,[#b_var{}=Tuple,#b_literal{val=Idx}],_} ->
                    track_value_in_fun(
                      [{Tuple,{tuple_element,Idx,Element}}|Rest], Fun, Work0,
                      Defs, ValuesInFun, DefSt0);
                {phi,_,_} ->
                    {ToExplore,DefSt} = handle_phi(Fun, V, Args,
                                                   Element, DefSt0),
                    track_value_in_fun(ToExplore ++ Rest, Fun, Work0,
                                       Defs, ValuesInFun, DefSt);
                {put_tuple,_,_} ->
                    track_put_tuple(Args, Element, Rest, Fun, V, Work0,
                                    Defs, ValuesInFun, DefSt0);
                {put_list,_,_} ->
                    track_put_list(Args, Element, Rest, Fun, V, Work0,
                                   Defs, ValuesInFun, DefSt0);
                {_,_,_} ->
                    %% Above we have handled all operations through
                    %% which we are able to track the value to its
                    %% construction. All other operations are from
                    %% execution paths not reachable when the actual
                    %% type (at runtime) is a relevant bitstring.
                    %% Thus we can safely abort the tracking here.
                    track_value_in_fun(Rest, Fun, Work0,
                                       Defs, ValuesInFun, DefSt0)
            end;
        #{V:={arg,Idx}} ->
            track_value_into_caller(Element, Idx, Rest, Fun, Work0, Defs,
                                    ValuesInFun, DefSt0)
    end;
track_value_in_fun([{#b_literal{},_}|Rest], Fun, Work,
                   Defs, ValuesInFun, DefSt) ->
    track_value_in_fun(Rest, Fun, Work, Defs, ValuesInFun, DefSt);
track_value_in_fun([], Fun, Work, _Defs, ValuesInFun,
                   DefSt0=#def_st{valuesdb=ValuesDb0}) ->
    %% We are done with this function. Store the result in the
    %% valuesdb and continue with the work list.
    DefSt = DefSt0#def_st{valuesdb=ValuesDb0#{Fun=>ValuesInFun}},
    find_defs_1(Work, DefSt).

track_value_into_call(Callee, Element, CallerFun, CallerWork, GlobalWork0,
                      CallerDefs, CallerValuesInFun, DefSt0) ->
    GlobalWork = [{Callee, {track_result, Element}}|GlobalWork0],
    track_value_in_fun(CallerWork, CallerFun, GlobalWork,
                       CallerDefs, CallerValuesInFun, DefSt0).

track_value_into_caller(Element, ArgIdx,
                        CalledFunWorklist, CalledFun,
                        GlobalWorklist0,
                        CalledFunDefs, CalledFunValues,
                        DefSt0=#def_st{funcdb=FuncDb,stmap=StMap}) ->
    #func_info{in=Callers} = map_get(CalledFun, FuncDb),
    ?DP("Track into callers of ~p, tracking arg-idx:~p, ~p~n  callers:~p~n",
        [CalledFun, ArgIdx, Element, Callers]),
    %% The caller information in func_info does not remove a caller
    %% when it is inlined into another function (although the new
    %% caller is added), so we have to filter out functions which lack
    %% entries in the st_map (as they are dead, they have been removed
    %% from the stmap).
    Work = [ {Caller,{track_call_argument,CalledFun,Element,ArgIdx}}
             || Caller <- Callers, is_map_key(Caller, StMap)],
    GlobalWorklist = Work ++ GlobalWorklist0,
    track_value_in_fun(CalledFunWorklist, CalledFun, GlobalWorklist,
                       CalledFunDefs, CalledFunValues, DefSt0).

track_put_tuple(FieldVars, {tuple_element,Idx,Element},
                Work, Fun, Dst, GlobalWork,
                Defs, ValuesInFun, DefSt0) ->
    %% The value we are tracking was constructed by a put tuple and we
    %% are interested in continuing the tracking of the field
    case lists:nth(Idx + 1, FieldVars) of
        ToTrack = #b_var{} ->
            track_value_in_fun([{ToTrack,Element}|Work], Fun, GlobalWork,
                               Defs, ValuesInFun, DefSt0);
        #b_literal{val=Lit} ->
            DefSt = add_literal(Fun, {opargs,Dst,Idx,Lit,Element}, DefSt0),
            track_value_in_fun(Work, Fun, GlobalWork,
                               Defs, ValuesInFun, DefSt)
    end.

track_put_list([Hd,_Tl], {hd,Element},
               Work, Fun, Dst, GlobalWork,
               Defs, ValuesInFun, DefSt0) ->
    %% The value we are tracking was constructed by a put list and we
    %% are interested in continuing the tracking of the field. We only
    %% handle the case when the tracked value is in the head field of
    %% a cons. This is due to the type analyser always assuming that a
    %% cons is part of a list and therefore we will never be able to
    %% safely rewrite an accumulator in the tail field of the cons,
    %% thus we will never have to track it.
    case Hd of
        #b_var{} ->
            track_value_in_fun([{Hd,Element}|Work], Fun, GlobalWork,
                               Defs, ValuesInFun, DefSt0);
        #b_literal{val=Lit} ->
            DefSt = add_literal(Fun, {opargs,Dst,0,Lit,Element}, DefSt0),
            track_value_in_fun(Work, Fun, GlobalWork, Defs, ValuesInFun, DefSt)
    end.

%% Find all calls to Callee and produce a work-list containing all
%% values which are used as the Idx:th argument.
get_call_arguments(Callee, Element, Idx, Defs, Fun, DefSt0) ->
    %% We traverse all defs inside the caller to find the calls.
    maps:fold(fun(_, #b_set{dst=Dst,op=call,args=[Target|Args]}, {Acc,DefSt})
                    when Callee =:= Target ->
                      {Values,DefSt1} =
                          gca(Args, Element, Idx, Fun, Dst, DefSt),
                      {Values ++ Acc, DefSt1};
                 (_, _, Acc) ->
                      Acc
              end, {[], DefSt0}, Defs).

gca(Args, Element, Idx, Fun, Dst, DefSt) ->
    gca(Args, 0, Element, Idx, Fun, Dst, DefSt).

gca([#b_var{}=V|_], I, Element, I, _Fun, _Dst, DefSt) ->
    %% This is the argument we are tracking.
    {[{V,Element}], DefSt};
gca([#b_literal{val=Lit}|_], I, self, I, _Fun, _Dst, DefSt)
  when not is_bitstring(Lit)->
    %% As value tracking is done without type information, we can
    %% follow def chains which don't terminate in a bitstring. This is
    %% harmless, but we should ignore them and not, later on, try to
    %% patch them to a bs_writable_binary.
    {[], DefSt};
gca([#b_literal{val=Lit}|_], I, Element, I, Fun, Dst, DefSt) ->
    {[], add_literal(Fun, {opargs,Dst,I+1,Lit,Element}, DefSt)};
gca([_|Args], I, Element, Idx, Fun, Dst, DefSt) ->
    gca(Args, I + 1, Element, Idx, Fun, Dst, DefSt).

handle_phi(Fun, Dst, Args, Element, DefSt0) ->
    foldl(fun({#b_literal{val=Lit},Lbl}, {Acc,DefStAcc0}) ->
                  DefStAcc =
                      add_literal(Fun, {phi,Dst,Lbl,Lit,Element}, DefStAcc0),
                  {Acc, DefStAcc};
             ({V=#b_var{},_Lbl}, {Acc,DefStAcc}) ->
                  {[{V,Element}|Acc],DefStAcc}
          end, {[],DefSt0}, Args).

%% Cache calculation of the defs for a function so we only do it once.
defs_in_fun(Fun, Args, SSA, DefSt=#def_st{defsdb=DefsDb}) ->
    case DefsDb of
        #{Fun:=Defs} ->
            {Defs, DefSt};
        #{} ->
            BlockMap = maps:from_list(SSA),
            Labels = maps:keys(BlockMap),
            Defs0 = beam_ssa:definitions(Labels, BlockMap),
            {Defs,_} = foldl(fun(Arg, {Acc,Idx}) ->
                                     {Acc#{Arg => {arg,Idx}}, Idx + 1}
                             end, {Defs0,0}, Args),
            {Defs, DefSt#def_st{defsdb=DefsDb#{Fun=>Defs}}}
    end.


%% Look up what we know about the values in Fun.
values_in_fun(Fun, #def_st{valuesdb=ValuesDb}) ->
    maps:get(Fun, ValuesDb, #{}).

add_literal(Fun, LitInfo, DefSt=#def_st{literals=Ls}) ->
    Old = maps:get(Fun, Ls, []),
    DefSt#def_st{literals=Ls#{Fun => [LitInfo|Old]}}.

patch_appends(Bins, Appends, StMap0) ->
    ?DP("Appends:~n~p~n", [Appends]),
    ?DP("Bins:~n~p~n", [Bins]),

    %% Group by function
    Patches = foldl(fun({Fun,Append}, Acc) ->
                            Acc#{Fun => [Append|maps:get(Fun, Acc, [])] }
                    end, Bins, Appends),
    ?DP("Patches:~n~p~n", [Patches]),
    maps:fold(fun(Fun, Ps, StMapAcc) ->
                      OptSt=#opt_st{ssa=SSA0,cnt=Cnt0} =
                          map_get(Fun, StMapAcc),
                      {SSA,Cnt} = patch_appends_f(SSA0, Cnt0, Ps),
                      StMapAcc#{Fun => OptSt#opt_st{ssa=SSA,cnt=Cnt}}
              end, StMap0, Patches).

patch_appends_f(SSA0, Cnt0, Patches) ->
    ?DP("Will patch ~p~n", [Patches]),
    ?DP("SSA: ~p~n", [SSA0]),
    %% Group by PD
    PD = foldl(fun(P, Acc) ->
                       case P of
                           {opargs,Dst,_,_,_} -> ok;
                           {append,Dst,_} -> ok;
                           {phi,Dst,_,_,_} -> ok;
                           {ret,Dst,_} -> ok
                       end,
                       Set = ordsets:add_element(P, maps:get(Dst, Acc, [])),
                       Acc#{Dst => Set}
               end, #{}, Patches),
    ?DP("PD: ~p~n", [PD]),
    patch_appends_f(SSA0, Cnt0, PD, [], []).

patch_appends_f([{Lbl,Blk=#b_blk{is=Is0,last=Last0}}|Rest],
                Cnt0, PD0, Acc0, BlockAdditions0) ->
    {Last,Extra,Cnt2,PD} =
        case PD0 of
            #{ Lbl := Patches } ->
                {Last1,Extra0,Cnt1} = patch_appends_ret(Last0, Patches, Cnt0),
                {Last1, reverse(Extra0), Cnt1, maps:remove(Lbl, PD0)};
            #{} ->
                {Last0, [], Cnt0, PD0}
        end,
    {Is, Cnt, BlockAdditions} = patch_appends_is(Is0, PD, Cnt2, [], []),
    Acc = [{Lbl,Blk#b_blk{is=Is ++ Extra, last=Last}}|Acc0],
    patch_appends_f(Rest, Cnt, PD, Acc, BlockAdditions ++ BlockAdditions0 );
patch_appends_f([], Cnt, _PD, Acc, BlockAdditions) ->
    ?DP("BlockAdditions: ~p~n", [BlockAdditions]),
    Linear = insert_block_additions(Acc, maps:from_list(BlockAdditions), []),
    ?DP("SSA-result:~n~p~n", [Linear]),
    {Linear, Cnt}.

patch_appends_is([I0=#b_set{dst=Dst}|Rest], PD0, Cnt0, Acc, BlockAdditions0)
  when is_map_key(Dst, PD0) ->
    #{ Dst := Patches } = PD0,
    PD = maps:remove(Dst, PD0),
    ExtractOpargs = fun({opargs,D,Idx,Lit,Element}) when Dst =:= D ->
                            {Idx, Lit, Element}
                    end,
    case Patches of
        [{opargs,Dst,_,_,_}|_] ->
            Ps = keysort(1, map(ExtractOpargs, Patches)),
            {Is, Cnt} = patch_opargs(I0, Ps, Cnt0),
            patch_appends_is(Rest, PD, Cnt, Is++Acc, BlockAdditions0);
        [{append,Dst,#b_literal{val= <<>>}=Lit}] ->
            %% Special case for when the first fragment is a literal
            %% <<>> and it has to be replaced with a bs_init_writable.
            #b_set{op=bs_create_bin,dst=Dst,args=Args0}=I0,
            [#b_literal{val=append},SegInfo,Lit|OtherArgs] = Args0,
            {V,Cnt} = new_var(Cnt0),
            Init = #b_set{op=bs_init_writable,dst=V,args=[#b_literal{val=256}]},
            I = I0#b_set{args=[#b_literal{val=private_append},
                               SegInfo,V|OtherArgs]},
            patch_appends_is(Rest, PD, Cnt, [I,Init|Acc], BlockAdditions0);
        [{append,Dst,_}] ->
            #b_set{op=bs_create_bin,dst=Dst,args=Args0}=I0,
            [#b_literal{val=append}|OtherArgs] = Args0,
            I = I0#b_set{args=[#b_literal{val=private_append}|OtherArgs]},
            patch_appends_is(Rest, PD, Cnt0, [I|Acc], BlockAdditions0);
        [{phi,Dst,_,_,_}|_] ->
            {I, Extra, Cnt} = patch_phi(I0, Patches, Cnt0),
            patch_appends_is(Rest, PD, Cnt, [I|Acc], Extra ++ BlockAdditions0)
    end;
patch_appends_is([I|Rest], PD, Cnt, Acc, BlockAdditions) ->
    patch_appends_is(Rest, PD, Cnt, [I|Acc], BlockAdditions);
patch_appends_is([], _, Cnt, Acc, BlockAdditions) ->
    {reverse(Acc), Cnt, BlockAdditions}.

%% The only time when we patch a return is when it returns a
%% literal.
patch_appends_ret(Last=#b_ret{arg=#b_literal{val=Lit}}, Patches, Cnt0)
  when is_list(Lit); is_tuple(Lit) ->
    Ps = keysort(1, [E || {ret,_,E} <- Patches]),
    ?DP("patch_appends_ret tuple or list :~n  lit: ~p~n  patches: ~p~n", [Lit, Ps]),
    {V,Extra,Cnt} = patch_literal_term(Lit, Ps, Cnt0),
    {Last#b_ret{arg=V}, Extra, Cnt};
patch_appends_ret(Last=#b_ret{arg=#b_literal{val=Lit}},
                  [{ret,_,Element}],
                  Cnt0) ->
    ?DP("patch_appends_ret other:~n  lit: ~p~n  element: ~p~n", [Lit, Element]),
    {V,Extra,Cnt} = patch_literal_term(Lit, Element, Cnt0),
    {Last#b_ret{arg=V}, Extra, Cnt}.

%% Should return the instructions in reversed order
patch_opargs(I0=#b_set{args=Args}, Patches0, Cnt0) ->
    ?DP("Patching args in ~p~nArgs: ~p~n Patches: ~p~n",
        [I0,Args,Patches0]),
    Patches = merge_arg_patches(Patches0),
    {PatchedArgs, Is, Cnt} = patch_opargs(Args, Patches, 0, [], [], Cnt0),
    {[I0#b_set{args=reverse(PatchedArgs)}|Is], Cnt}.

patch_opargs([#b_literal{val=Lit}|Args], [{Idx,Lit,Element}|Patches],
             Idx, PatchedArgs, Is, Cnt0) ->
    ?DP("Patching arg idx ~p~n  lit: ~p~n  elem: ~p~n", [Idx, Lit, Element]),
    {Arg,Extra,Cnt} = patch_literal_term(Lit, Element, Cnt0),
    patch_opargs(Args, Patches, Idx + 1, [Arg|PatchedArgs], Extra ++ Is, Cnt);
patch_opargs([Arg|Args], Patches, Idx, PatchedArgs, Is, Cnt) ->
    ?DP("Skipping arg idx ~p~n  arg: ~p~n  patches: ~p~n",
              [Idx,Arg,Patches]),
    patch_opargs(Args, Patches, Idx + 1, [Arg|PatchedArgs], Is, Cnt);
patch_opargs([], [], _, PatchedArgs, Is, Cnt) ->
    {PatchedArgs, Is, Cnt}.

%% The way find_defs and find_appends work, we can end up with
%% multiple patches patching different parts of a tuple or pair. We
%% merge them here.
merge_arg_patches([{Idx,Lit,P0},{Idx,Lit,P1}|Patches]) ->
    P = case {P0, P1} of
            {{tuple_element,I0,E0},{tuple_element,I1,E1}} ->
                {tuple_elements,[{I0,E0},{I1,E1}]};
            {{tuple_elements,Es},{tuple_element,I,E}} ->
                {tuple_elements,[{I,E}|Es]}
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
        foldr(fun(Arg0={_, Lbl}, {ArgsAcc, ExtraAcc, CntAcc}) ->
                      case L2P of
                          #{Lbl := {phi,_,Lbl,Lit,Element}} ->
                              {Arg,Extra,Cnt1} =
                                  patch_literal_term(Lit, Element, CntAcc),
                              {[{Arg,Lbl}|ArgsAcc],
                               [{Lbl, Extra}|ExtraAcc], Cnt1};
                          _ ->
                              {[Arg0|ArgsAcc], ExtraAcc, CntAcc}
                      end
              end, {[], [], Cnt0}, Args0),
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
patch_literal_term(<<>>, self, Cnt0) ->
    {V,Cnt} = new_var(Cnt0),
    I = #b_set{op=bs_init_writable,dst=V,args=[#b_literal{val=256}]},
    {V, [I], Cnt};
patch_literal_term(Lit, self, Cnt) ->
    {#b_literal{val=Lit}, [], Cnt};
patch_literal_term([H0|T0], {hd,Element}, Cnt0) ->
    {H,Extra,Cnt1} = patch_literal_term(H0, Element, Cnt0),
    {T,[],Cnt1} = patch_literal_term(T0, [], Cnt1),
    {Dst,Cnt} = new_var(Cnt1),
    I = #b_set{op=put_list,dst=Dst,args=[H,T]},
    {Dst, [I|Extra], Cnt};
patch_literal_term(Lit, [], Cnt) ->
    {#b_literal{val=Lit}, [], Cnt}.

patch_literal_tuple(Tuple, Elements, Cnt) ->
    ?DP("Will patch literal tuple~n  tuple:~p~n  elements: ~p~n",
              [Tuple,Elements]),
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
    patch_literal_tuple(LitElements, Patches, [T|Patched],
                        Extra, Idx + 1, Cnt);
patch_literal_tuple([], [], Patched, Extra, _, Cnt0) ->
    {V,Cnt} = new_var(Cnt0),
    I = #b_set{op=put_tuple,dst=V,args=reverse(Patched)},
    {V, [I|Extra], Cnt}.

%% As beam_ssa_opt:new_var/2, but with a hard-coded base
new_var(Count) ->
    {#b_var{name={alias_opt,Count}},Count+1}.

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

%%%
%%% Predicate to check if a function is the stub for a nif.
%%%
-spec is_nif(func_id(), st_map()) -> boolean().

is_nif(F, StMap) ->
    #opt_st{ssa=[{0,#b_blk{is=Is}}|_]} = map_get(F, StMap),
    case Is of
        [#b_set{op=nif_start}|_] ->
            true;
        _ -> false
    end.
