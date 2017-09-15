%% -*- erlang-indent-level: 2 -*-
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

%%%----------------------------------------------------------------------
%%% File    : dialyzer_races.erl
%%% Author  : Maria Christakis <christakismaria@gmail.com>
%%% Description : Utility functions for race condition detection
%%%
%%% Created : 21 Nov 2008 by Maria Christakis <christakismaria@gmail.com>
%%%----------------------------------------------------------------------
-module(dialyzer_races).

%% Race Analysis

-export([store_race_call/5, race/1, get_race_warnings/2, format_args/4]).

%% Record Interfaces

-export([beg_clause_new/3, cleanup/1, end_case_new/1, end_clause_new/3,
         get_curr_fun/1, get_curr_fun_args/1, get_new_table/1,
         get_race_analysis/1, get_race_list/1, get_race_list_size/1,
	 get_race_list_and_size/1,
         let_tag_new/2, new/0, put_curr_fun/3, put_fun_args/2,
         put_race_analysis/2, put_race_list/3]).

-export_type([races/0, core_vars/0]).

-include("dialyzer.hrl").

%%% ===========================================================================
%%%
%%%  Definitions
%%%
%%% ===========================================================================

-define(local, 5).
-define(no_arg, no_arg).
-define(no_label, no_label).
-define(bypassed, bypassed).

-define(WARN_WHEREIS_REGISTER, warn_whereis_register).
-define(WARN_WHEREIS_UNREGISTER, warn_whereis_unregister).
-define(WARN_ETS_LOOKUP_INSERT, warn_ets_lookup_insert).
-define(WARN_MNESIA_DIRTY_READ_WRITE, warn_mnesia_dirty_read_write).
-define(WARN_NO_WARN, warn_no_warn).

%%% ===========================================================================
%%%
%%%  Local Types
%%%
%%% ===========================================================================

-type label_type()  :: label() | [label()] | {label()} | ?no_label.
-type args()        :: [label_type() | [string()]].
-type core_vars()   :: cerl:cerl() | ?no_arg | ?bypassed.
-type var_to_map1() :: core_vars() | [cerl:cerl()].
-type var_to_map2() :: cerl:cerl() | [cerl:cerl()] | ?bypassed.
-type core_args()   :: [core_vars()] | 'empty'.
-type op()          :: 'bind' | 'unbind'.

-type dep_calls()  :: 'whereis' | 'ets_lookup' | 'mnesia_dirty_read'.
-type warn_calls() :: 'register' | 'unregister' | 'ets_insert'
                    | 'mnesia_dirty_write'.
-type call()       :: 'whereis' | 'register' | 'unregister' | 'ets_new'
                    | 'ets_lookup' | 'ets_insert' | 'mnesia_dirty_read1'
                    | 'mnesia_dirty_read2' | 'mnesia_dirty_write1'
                    | 'mnesia_dirty_write2' | 'function_call'.
-type race_tag()   :: 'whereis_register' | 'whereis_unregister'
                    | 'ets_lookup_insert' | 'mnesia_dirty_read_write'.

%% The following type is similar to the raw_warning() type but has a
%% tag which is local to this module and is not propagated to outside
-type dial_race_warning() :: {race_warn_tag(), warning_info(), {atom(), [term()]}}.
-type race_warn_tag() :: ?WARN_WHEREIS_REGISTER | ?WARN_WHEREIS_UNREGISTER
                      | ?WARN_ETS_LOOKUP_INSERT | ?WARN_MNESIA_DIRTY_READ_WRITE.

-record(beg_clause, {arg        :: var_to_map1() | 'undefined',
                     pats       :: var_to_map1() | 'undefined',
                     guard      :: cerl:cerl() | 'undefined'}).
-record(end_clause, {arg        :: var_to_map1() | 'undefined',
                     pats       :: var_to_map1() | 'undefined',
                     guard      :: cerl:cerl() | 'undefined'}).
-record(end_case,   {clauses    :: [#end_clause{}]}).
-record(curr_fun,   {status     :: 'in' | 'out' | 'undefined',
                     mfa        :: dialyzer_callgraph:mfa_or_funlbl()
                                 | 'undefined',
                     label      :: label() | 'undefined',
                     def_vars   :: [core_vars()] | 'undefined',
                     arg_types  :: [erl_types:erl_type()] | 'undefined',
                     call_vars  :: [core_vars()] | 'undefined',
                     var_map    :: dict:dict() | 'undefined'}).
-record(dep_call,   {call_name  :: dep_calls(),
                     args       :: args() | 'undefined',
                     arg_types  :: [erl_types:erl_type()],
                     vars       :: [core_vars()],
                     state      :: dialyzer_dataflow:state(),
                     file_line  :: file_line(),
                     var_map    :: dict:dict() | 'undefined'}).
-record(fun_call,   {caller     :: dialyzer_callgraph:mfa_or_funlbl(),
                     callee     :: dialyzer_callgraph:mfa_or_funlbl(),
                     arg_types  :: [erl_types:erl_type()],
                     vars       :: [core_vars()]}).
-record(let_tag,    {var        :: var_to_map1(),
                     arg        :: var_to_map1()}).
-record(warn_call,  {call_name  :: warn_calls(),
                     args       :: args(),
                     var_map    :: dict:dict() | 'undefined'}).

-type case_tags()  :: 'beg_case' | #beg_clause{} | #end_clause{} | #end_case{}.
-type code()       :: [#dep_call{} | #fun_call{} | #warn_call{} |
                       #curr_fun{} | #let_tag{} | case_tags() | race_tag()].

-type table_var()  :: label() | ?no_label.
-type table()      :: {'named', table_var(), [string()]} | 'other' | 'no_t'.

-record(race_fun,   {mfa        :: mfa(),
                     args       :: args(),
                     arg_types  :: [erl_types:erl_type()],
                     vars       :: [core_vars()],
                     file_line  :: file_line(),
                     index      :: non_neg_integer(),
                     fun_mfa    :: dialyzer_callgraph:mfa_or_funlbl(),
                     fun_label  :: label()}).

-record(races, {curr_fun                :: dialyzer_callgraph:mfa_or_funlbl()
                                         | 'undefined',
                curr_fun_label          :: label() | 'undefined',
                curr_fun_args = 'empty' :: core_args(),
                new_table = 'no_t'      :: table(),
                race_list = []          :: code(),
                race_list_size = 0      :: non_neg_integer(),
                race_tags = []          :: [#race_fun{}],
                %% true for fun types and warning mode
                race_analysis = false   :: boolean(),
                race_warnings = []      :: [dial_race_warning()]}).

%%% ===========================================================================
%%%
%%%  Exported Types
%%%
%%% ===========================================================================

-opaque races() :: #races{}.

%%% ===========================================================================
%%%
%%%  Race Analysis
%%%
%%% ===========================================================================

-spec store_race_call(dialyzer_callgraph:mfa_or_funlbl(),
		      [erl_types:erl_type()], [core_vars()],
                      file_line(), dialyzer_dataflow:state()) ->
  dialyzer_dataflow:state().

store_race_call(Fun, ArgTypes, Args, FileLine, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  CurrFun = Races#races.curr_fun,
  CurrFunLabel = Races#races.curr_fun_label,
  RaceTags = Races#races.race_tags,
  CleanState = dialyzer_dataflow:state__records_only(State),
  {NewRaceList, NewRaceListSize, NewRaceTags, NewTable} =
    case CurrFun of
      {_Module, module_info, A} when A =:= 0 orelse A =:= 1 ->
        {[], 0, RaceTags, no_t};
      _Thing ->
        RaceList = Races#races.race_list,
        RaceListSize = Races#races.race_list_size,
        case Fun of
          {erlang, get_module_info, A} when A =:= 1 orelse A =:= 2 ->
            {[], 0, RaceTags, no_t};
          {erlang, register, 2} ->
            VarArgs = format_args(Args, ArgTypes, CleanState, register),
            RaceFun = #race_fun{mfa = Fun, args = VarArgs,
                                arg_types = ArgTypes, vars = Args,
                                file_line = FileLine, index = RaceListSize,
                                fun_mfa = CurrFun, fun_label = CurrFunLabel},
            {[#warn_call{call_name = register, args = VarArgs}|
              RaceList], RaceListSize + 1, [RaceFun|RaceTags], no_t};
          {erlang, unregister, 1} ->
            VarArgs = format_args(Args, ArgTypes, CleanState, unregister),
            RaceFun = #race_fun{mfa = Fun, args = VarArgs,
                                arg_types = ArgTypes, vars = Args,
                                file_line = FileLine, index = RaceListSize,
                                fun_mfa = CurrFun, fun_label = CurrFunLabel},
            {[#warn_call{call_name = unregister, args = VarArgs}|
              RaceList], RaceListSize + 1, [RaceFun|RaceTags], no_t};
	  {erlang, whereis, 1} ->
            VarArgs = format_args(Args, ArgTypes, CleanState, whereis),
	    {[#dep_call{call_name = whereis, args = VarArgs,
                        arg_types = ArgTypes, vars = Args,
                        state = CleanState, file_line = FileLine}|
              RaceList], RaceListSize + 1, RaceTags, no_t};
	  {ets, insert, 2} ->
            VarArgs = format_args(Args, ArgTypes, CleanState, ets_insert),
            RaceFun = #race_fun{mfa = Fun, args = VarArgs,
                                arg_types = ArgTypes, vars = Args,
                                file_line = FileLine, index = RaceListSize,
                                fun_mfa = CurrFun, fun_label = CurrFunLabel},
            {[#warn_call{call_name = ets_insert, args = VarArgs}|
              RaceList], RaceListSize + 1, [RaceFun|RaceTags], no_t};
          {ets, lookup, 2} ->
            VarArgs = format_args(Args, ArgTypes, CleanState, ets_lookup),
            {[#dep_call{call_name = ets_lookup, args = VarArgs,
                        arg_types = ArgTypes, vars = Args,
                        state = CleanState, file_line = FileLine}|
              RaceList], RaceListSize + 1, RaceTags, no_t};
	  {ets, new, 2} ->
	    VarArgs = format_args(Args, ArgTypes, CleanState, ets_new),
            [VarArgs1, VarArgs2, _, Options] = VarArgs,
            NewTable1 =
              case lists:member("'public'", Options) of
                true ->
                  case lists:member("'named_table'", Options) of
                    true ->
                      {named, VarArgs1, VarArgs2};
                    false -> other
                  end;
                false -> no_t
              end,
	    {RaceList, RaceListSize, RaceTags, NewTable1};
	  {mnesia, dirty_read, A} when A =:= 1 orelse A =:= 2 ->
            VarArgs =
              case A of
                1 ->
                  format_args(Args, ArgTypes, CleanState, mnesia_dirty_read1);
                2 ->
                  format_args(Args, ArgTypes, CleanState, mnesia_dirty_read2)
              end,
            {[#dep_call{call_name = mnesia_dirty_read, args = VarArgs,
                        arg_types = ArgTypes, vars = Args,
                        state = CleanState, file_line = FileLine}|RaceList],
	     RaceListSize + 1, RaceTags, no_t};
          {mnesia, dirty_write, A} when A =:= 1 orelse A =:= 2 ->
            VarArgs =
              case A of
                1 ->
                  format_args(Args, ArgTypes, CleanState, mnesia_dirty_write1);
                2 ->
                  format_args(Args, ArgTypes, CleanState, mnesia_dirty_write2)
              end,
            RaceFun = #race_fun{mfa = Fun, args = VarArgs,
                                arg_types = ArgTypes, vars = Args,
                                file_line = FileLine, index = RaceListSize,
                                fun_mfa = CurrFun, fun_label = CurrFunLabel},
            {[#warn_call{call_name = mnesia_dirty_write,
			 args = VarArgs}|RaceList],
	     RaceListSize + 1, [RaceFun|RaceTags], no_t};
          Int when is_integer(Int) ->
            {[#fun_call{caller = CurrFun, callee = Int, arg_types =  ArgTypes,
                        vars = Args}|RaceList],
	     RaceListSize + 1, RaceTags, no_t};
          _Other ->
            Callgraph = dialyzer_dataflow:state__get_callgraph(State),
            case digraph:vertex(dialyzer_callgraph:get_digraph(Callgraph),
                                Fun) of
              {Fun, confirmed} ->
                {[#fun_call{caller = CurrFun, callee = Fun,
                            arg_types = ArgTypes, vars = Args}|RaceList],
		 RaceListSize + 1, RaceTags, no_t};
              false ->
                {RaceList, RaceListSize, RaceTags, no_t}
            end
        end
    end,
  state__renew_info(NewRaceList, NewRaceListSize, NewRaceTags, NewTable, State).

-spec race(dialyzer_dataflow:state()) -> dialyzer_dataflow:state().

race(State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  RaceTags = Races#races.race_tags,
  RetState =
    case RaceTags of
      [] -> State;
      [#race_fun{mfa = Fun,
                 args = VarArgs, arg_types = ArgTypes,
                 vars = Args, file_line = FileLine,
                 index = Index, fun_mfa = CurrFun,
                 fun_label = CurrFunLabel}|T] ->
        Callgraph = dialyzer_dataflow:state__get_callgraph(State),
        {ok, [_Args, Code]} =
          dict:find(CurrFun, dialyzer_callgraph:get_race_code(Callgraph)),
        RaceList = lists:reverse(Code),
        RaceWarnTag =
          case Fun of
            {erlang, register, 2} -> ?WARN_WHEREIS_REGISTER;
            {erlang, unregister, 1} -> ?WARN_WHEREIS_UNREGISTER;
            {ets, insert, 2} -> ?WARN_ETS_LOOKUP_INSERT;
            {mnesia, dirty_write, _A} -> ?WARN_MNESIA_DIRTY_READ_WRITE
          end,
        State1 =
          state__renew_curr_fun(CurrFun,
          state__renew_curr_fun_label(CurrFunLabel,
          state__renew_race_list(lists:nthtail(length(RaceList) - Index,
					       RaceList), State))),
        DepList = fixup_race_list(RaceWarnTag, VarArgs, State1),
        {State2, RaceWarn} =
          get_race_warn(Fun, Args, ArgTypes, DepList, State),
        {File, Line} = FileLine,
        CurrMFA = dialyzer_dataflow:state__find_function(CurrFun, State),
        WarningInfo = {File, Line, CurrMFA},
        race(
          state__add_race_warning(
            state__renew_race_tags(T, State2), RaceWarn, RaceWarnTag,
            WarningInfo))
    end,
  state__renew_race_tags([], RetState).

fixup_race_list(RaceWarnTag, WarnVarArgs, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  CurrFun = Races#races.curr_fun,
  CurrFunLabel = Races#races.curr_fun_label,
  RaceList = Races#races.race_list,
  Callgraph = dialyzer_dataflow:state__get_callgraph(State),
  Digraph = dialyzer_callgraph:get_digraph(Callgraph),
  Calls = digraph:edges(Digraph),
  RaceTag =
    case RaceWarnTag of
      ?WARN_WHEREIS_REGISTER -> whereis_register;
      ?WARN_WHEREIS_UNREGISTER -> whereis_unregister;
      ?WARN_ETS_LOOKUP_INSERT -> ets_lookup_insert;
      ?WARN_MNESIA_DIRTY_READ_WRITE -> mnesia_dirty_read_write
    end,
  NewRaceList = [RaceTag|RaceList],
  CleanState = dialyzer_dataflow:state__cleanup(State),
  NewState = state__renew_race_list(NewRaceList, CleanState),
  DepList1 =
    fixup_race_forward_pullout(CurrFun, CurrFunLabel, Calls,
                               lists:reverse(NewRaceList), [], CurrFun,
                               WarnVarArgs, RaceWarnTag, dict:new(),
                               [], [], [], 2 * ?local, NewState),
  Parents = fixup_race_backward(CurrFun, Calls, Calls, [], ?local),
  UParents = lists:usort(Parents),
  Filtered = filter_parents(UParents, UParents, Digraph),
  NewParents =
    case lists:member(CurrFun, Filtered) of
      true -> Filtered;
      false -> [CurrFun|Filtered]
    end,
  DepList2 =
    fixup_race_list_helper(NewParents, Calls, CurrFun, WarnVarArgs,
                           RaceWarnTag, NewState),
  dialyzer_dataflow:dispose_state(CleanState),
  lists:usort(cleanup_dep_calls(DepList1 ++ DepList2)).

fixup_race_list_helper(Parents, Calls, CurrFun, WarnVarArgs, RaceWarnTag,
		       State) ->
  case Parents of
    [] -> [];
    [Head|Tail] ->
      Callgraph = dialyzer_dataflow:state__get_callgraph(State),
      Code =
        case dict:find(Head, dialyzer_callgraph:get_race_code(Callgraph)) of
          error -> [];
          {ok, [_A, C]} -> C
        end,
      {ok, FunLabel} = dialyzer_callgraph:lookup_label(Head, Callgraph),
      DepList1 =
        fixup_race_forward_pullout(Head, FunLabel, Calls, Code, [], CurrFun,
                                   WarnVarArgs, RaceWarnTag, dict:new(),
                                   [], [], [], 2 * ?local, State),
      DepList2 =
        fixup_race_list_helper(Tail, Calls, CurrFun, WarnVarArgs,
			       RaceWarnTag, State),
      DepList1 ++ DepList2
  end.

%%% ===========================================================================
%%%
%%%  Forward Analysis
%%%
%%% ===========================================================================

fixup_race_forward_pullout(CurrFun, CurrFunLabel, Calls, Code, RaceList,
                           InitFun, WarnVarArgs, RaceWarnTag, RaceVarMap,
                           FunDefVars, FunCallVars, FunArgTypes, NestingLevel,
                           State) ->
  TState = dialyzer_dataflow:state__duplicate(State),
  {DepList, NewCurrFun, NewCurrFunLabel, NewCalls,
   NewCode, NewRaceList, NewRaceVarMap, NewFunDefVars,
   NewFunCallVars, NewFunArgTypes, NewNestingLevel} =
    fixup_race_forward(CurrFun, CurrFunLabel, Calls, Code, RaceList,
                       InitFun, WarnVarArgs, RaceWarnTag, RaceVarMap,
                       FunDefVars, FunCallVars, FunArgTypes, NestingLevel,
                       cleanup_race_code(TState)),
  dialyzer_dataflow:dispose_state(TState),
  case NewCode of
    [] -> DepList;
    [#fun_call{caller = NewCurrFun, callee = Call, arg_types = FunTypes,
               vars = FunArgs}|Tail] ->
      Callgraph = dialyzer_dataflow:state__get_callgraph(State),
      OkCall = {ok, Call},
      {Name, Label} =
        case is_integer(Call) of
          true ->
            case dialyzer_callgraph:lookup_name(Call, Callgraph) of
              error -> {OkCall, OkCall};
              N -> {N, OkCall}
            end;
          false ->
            {OkCall, dialyzer_callgraph:lookup_label(Call, Callgraph)}
        end,
      {NewCurrFun1, NewCurrFunLabel1, NewCalls1, NewCode1, NewRaceList1,
       NewRaceVarMap1, NewFunDefVars1, NewFunCallVars1, NewFunArgTypes1,
       NewNestingLevel1} =
        case Label =:= error of
          true ->
            {NewCurrFun, NewCurrFunLabel, NewCalls, Tail, NewRaceList,
             NewRaceVarMap, NewFunDefVars, NewFunCallVars, NewFunArgTypes,
             NewNestingLevel};
          false ->
            {ok, Fun} = Name,
            {ok, Int} = Label,
            case dict:find(Fun, dialyzer_callgraph:get_race_code(Callgraph)) of
              error ->
                {NewCurrFun, NewCurrFunLabel, NewCalls, Tail, NewRaceList,
                 NewRaceVarMap, NewFunDefVars, NewFunCallVars, NewFunArgTypes,
                 NewNestingLevel};
              {ok, [Args, CodeB]} ->
                Races = dialyzer_dataflow:state__get_races(State),
                {RetCurrFun, RetCurrFunLabel, RetCalls, RetCode,
                 RetRaceList, RetRaceVarMap, RetFunDefVars, RetFunCallVars,
                 RetFunArgTypes, RetNestingLevel} =
                  fixup_race_forward_helper(NewCurrFun,
                      NewCurrFunLabel, Fun, Int, NewCalls, NewCalls,
                      [#curr_fun{status = out, mfa = NewCurrFun,
                                 label = NewCurrFunLabel,
                                 var_map = NewRaceVarMap,
                                 def_vars = NewFunDefVars,
                                 call_vars = NewFunCallVars,
                                 arg_types = NewFunArgTypes}|
                       Tail],
                      NewRaceList, InitFun, FunArgs, FunTypes, RaceWarnTag,
                      NewRaceVarMap, NewFunDefVars, NewFunCallVars,
                      NewFunArgTypes, NewNestingLevel, Args, CodeB,
                      Races#races.race_list),
                case RetCode of
                  [#curr_fun{}|_CodeTail] ->
                    {NewCurrFun, NewCurrFunLabel, RetCalls, RetCode,
                     RetRaceList, NewRaceVarMap, NewFunDefVars,
                     NewFunCallVars, NewFunArgTypes, RetNestingLevel};
                  _Else ->
                    {RetCurrFun, RetCurrFunLabel, RetCalls, RetCode,
                     RetRaceList, RetRaceVarMap, RetFunDefVars,
                     RetFunCallVars, RetFunArgTypes, RetNestingLevel}
                end
            end
        end,
      DepList ++
        fixup_race_forward_pullout(NewCurrFun1, NewCurrFunLabel1, NewCalls1,
                                   NewCode1, NewRaceList1, InitFun, WarnVarArgs,
                                   RaceWarnTag, NewRaceVarMap1, NewFunDefVars1,
                                   NewFunCallVars1, NewFunArgTypes1,
                                   NewNestingLevel1, State)
  end.

fixup_race_forward(CurrFun, CurrFunLabel, Calls, Code, RaceList,
                   InitFun, WarnVarArgs, RaceWarnTag, RaceVarMap,
                   FunDefVars, FunCallVars, FunArgTypes, NestingLevel,
                   State) ->
  case Code of
    [] ->
      {[], CurrFun, CurrFunLabel, Calls, Code, RaceList, RaceVarMap,
       FunDefVars, FunCallVars, FunArgTypes, NestingLevel};
    [Head|Tail] ->
      Callgraph = dialyzer_dataflow:state__get_callgraph(State),
      {NewRL, DepList, NewNL, Return} =
        case Head of
          #dep_call{call_name = whereis} ->
            case RaceWarnTag of
              WarnWhereis when WarnWhereis =:= ?WARN_WHEREIS_REGISTER orelse
                               WarnWhereis =:= ?WARN_WHEREIS_UNREGISTER ->
    	        {[Head#dep_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
          #dep_call{call_name = ets_lookup} ->
            case RaceWarnTag of
              ?WARN_ETS_LOOKUP_INSERT ->
                {[Head#dep_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
          #dep_call{call_name = mnesia_dirty_read} ->
            case RaceWarnTag of
              ?WARN_MNESIA_DIRTY_READ_WRITE ->
     	        {[Head#dep_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
	  #warn_call{call_name = RegCall} when RegCall =:= register orelse
                                               RegCall =:= unregister ->
            case RaceWarnTag of
              WarnWhereis when WarnWhereis =:= ?WARN_WHEREIS_REGISTER orelse
                               WarnWhereis =:= ?WARN_WHEREIS_UNREGISTER ->
     	        {[Head#warn_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
  	  #warn_call{call_name = ets_insert} ->
            case RaceWarnTag of
              ?WARN_ETS_LOOKUP_INSERT ->
                {[Head#warn_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
  	  #warn_call{call_name = mnesia_dirty_write} ->
            case RaceWarnTag of
              ?WARN_MNESIA_DIRTY_READ_WRITE ->
     	        {[Head#warn_call{var_map = RaceVarMap}|RaceList],
                 [], NestingLevel, false};
              _Other ->
                {RaceList, [], NestingLevel, false}
            end;
          #fun_call{caller = CurrFun, callee = InitFun} ->
            {RaceList, [], NestingLevel, false};
	  #fun_call{caller = CurrFun} ->
            {RaceList, [], NestingLevel - 1, false};
          beg_case ->
            {[Head|RaceList], [], NestingLevel, false};
          #beg_clause{} ->
            {[#beg_clause{}|RaceList], [], NestingLevel, false};
          #end_clause{} ->
            {[#end_clause{}|RaceList], [], NestingLevel, false};
          #end_case{} ->
            {[Head|RaceList], [], NestingLevel, false};
          #let_tag{} ->
            {RaceList, [], NestingLevel, false};
          #curr_fun{status = in, mfa = InitFun,
                    label = _InitFunLabel, var_map = _NewRVM,
                    def_vars = NewFDV, call_vars = NewFCV,
                    arg_types = _NewFAT} ->
            {[#curr_fun{status = out, var_map = RaceVarMap,
                        def_vars = NewFDV, call_vars = NewFCV}|
              RaceList], [], NestingLevel - 1, false};
          #curr_fun{status = in, def_vars = NewFDV,
                    call_vars = NewFCV} ->
            {[#curr_fun{status = out, var_map = RaceVarMap,
                        def_vars = NewFDV, call_vars = NewFCV}|
              RaceList],
             [], NestingLevel - 1, false};
          #curr_fun{status = out} ->
            {[#curr_fun{status = in, var_map = RaceVarMap}|RaceList], [],
             NestingLevel + 1, false};
          RaceTag ->
            PublicTables = dialyzer_callgraph:get_public_tables(Callgraph),
            NamedTables = dialyzer_callgraph:get_named_tables(Callgraph),
            WarnVarArgs1 =
              var_type_analysis(FunDefVars, FunArgTypes, WarnVarArgs,
                                RaceWarnTag, RaceVarMap,
                                dialyzer_dataflow:state__records_only(State)),
            {NewDepList, IsPublic, _Return} =
              get_deplist_paths(RaceList, WarnVarArgs1, RaceWarnTag,
                                RaceVarMap, 0, PublicTables, NamedTables),
            {NewHead, NewDepList1} =
              case RaceTag of
                whereis_register ->
                  {[#warn_call{call_name = register, args = WarnVarArgs,
                              var_map = RaceVarMap}],
                   NewDepList};
                 whereis_unregister ->
                  {[#warn_call{call_name = unregister, args = WarnVarArgs,
                              var_map = RaceVarMap}],
                   NewDepList};
                ets_lookup_insert ->
                  NewWarnCall =
                    [#warn_call{call_name = ets_insert, args = WarnVarArgs,
                                var_map = RaceVarMap}],
                  [Tab, Names, _, _] = WarnVarArgs,
                  case IsPublic orelse
                    compare_var_list(Tab, PublicTables, RaceVarMap)
                    orelse
                    length(Names -- NamedTables) < length(Names) of
                    true ->
                      {NewWarnCall, NewDepList};
                    false -> {NewWarnCall, []}
                  end;
                mnesia_dirty_read_write ->
                  {[#warn_call{call_name = mnesia_dirty_write,
                               args = WarnVarArgs, var_map = RaceVarMap}],
                   NewDepList}
              end,
            {NewHead ++ RaceList, NewDepList1, NestingLevel,
             is_last_race(RaceTag, InitFun, Tail, Callgraph)}
        end,
      {NewCurrFun, NewCurrFunLabel, NewCode, NewRaceList, NewRaceVarMap,
       NewFunDefVars, NewFunCallVars, NewFunArgTypes, NewNestingLevel,
       PullOut} =
        case Head of
          #fun_call{caller = CurrFun} ->
            case NewNL =:= 0 of
              true ->
                {CurrFun, CurrFunLabel, Tail, NewRL, RaceVarMap,
                 FunDefVars, FunCallVars, FunArgTypes, NewNL, false};
              false ->
                {CurrFun, CurrFunLabel, Code, NewRL, RaceVarMap,
                 FunDefVars, FunCallVars, FunArgTypes, NewNL, true}
            end;
          #beg_clause{arg = Arg, pats = Pats, guard = Guard} ->
            {RaceVarMap1, RemoveClause} =
              race_var_map_guard(Arg, Pats, Guard, RaceVarMap, bind),
            case RemoveClause of
              true ->
                {RaceList2,
                 #curr_fun{mfa = CurrFun2, label = CurrFunLabel2,
                           var_map = RaceVarMap2, def_vars = FunDefVars2,
                           call_vars = FunCallVars2, arg_types = FunArgTypes2},
                 Code2, NestingLevel2} =
                  remove_clause(NewRL,
                                #curr_fun{mfa = CurrFun, label = CurrFunLabel,
                                          var_map = RaceVarMap1,
                                          def_vars = FunDefVars,
                                          call_vars = FunCallVars,
                                          arg_types = FunArgTypes},
                                Tail, NewNL),
                {CurrFun2, CurrFunLabel2, Code2, RaceList2,
                 RaceVarMap2, FunDefVars2, FunCallVars2, FunArgTypes2,
                 NestingLevel2, false};
              false ->
                {CurrFun, CurrFunLabel, Tail, NewRL, RaceVarMap1,
                 FunDefVars, FunCallVars, FunArgTypes, NewNL, false}
            end;
          #end_clause{arg = Arg, pats = Pats, guard = Guard} ->
            {RaceVarMap1, _RemoveClause} =
              race_var_map_guard(Arg, Pats, Guard, RaceVarMap, unbind),
            {CurrFun, CurrFunLabel, Tail, NewRL, RaceVarMap1,
             FunDefVars, FunCallVars, FunArgTypes, NewNL,
             false};
          #end_case{clauses = Clauses} ->
            RaceVarMap1 =
              race_var_map_clauses(Clauses, RaceVarMap),
            {CurrFun, CurrFunLabel, Tail, NewRL, RaceVarMap1,
             FunDefVars, FunCallVars, FunArgTypes, NewNL,
             false};
          #let_tag{var = Var, arg = Arg} ->
            {CurrFun, CurrFunLabel, Tail, NewRL,
             race_var_map(Var, Arg, RaceVarMap, bind), FunDefVars,
             FunCallVars, FunArgTypes, NewNL, false};
          #curr_fun{mfa = CurrFun1, label = CurrFunLabel1,
                    var_map = RaceVarMap1, def_vars = FunDefVars1,
                    call_vars = FunCallVars1, arg_types = FunArgTypes1} ->
             case NewNL =:= 0 of
               true ->
                 {CurrFun, CurrFunLabel,
                  remove_nonlocal_functions(Tail, 1), NewRL, RaceVarMap,
                  FunDefVars, FunCallVars, FunArgTypes, NewNL, false};
               false ->
                 {CurrFun1, CurrFunLabel1, Tail, NewRL, RaceVarMap1,
                  FunDefVars1, FunCallVars1, FunArgTypes1, NewNL, false}
             end;
          _Thing ->
            {CurrFun, CurrFunLabel, Tail, NewRL, RaceVarMap,
             FunDefVars, FunCallVars, FunArgTypes, NewNL, false}
        end,
      case Return of
        true ->
          {DepList, NewCurrFun, NewCurrFunLabel, Calls,
           [], NewRaceList, NewRaceVarMap, NewFunDefVars,
           NewFunCallVars, NewFunArgTypes, NewNestingLevel};
        false ->
          NewNestingLevel1 =
            case NewNestingLevel =:= 0 of
              true -> NewNestingLevel + 1;
              false -> NewNestingLevel
            end,
          case PullOut of
            true ->
              {DepList, NewCurrFun, NewCurrFunLabel, Calls,
               NewCode, NewRaceList, NewRaceVarMap, NewFunDefVars,
               NewFunCallVars, NewFunArgTypes, NewNestingLevel1};
            false ->
              {RetDepList, NewCurrFun1,  NewCurrFunLabel1, NewCalls1,
               NewCode1, NewRaceList1, NewRaceVarMap1, NewFunDefVars1,
               NewFunCallVars1, NewFunArgTypes1, NewNestingLevel2} =
                fixup_race_forward(NewCurrFun, NewCurrFunLabel, Calls,
                                   NewCode, NewRaceList, InitFun, WarnVarArgs,
                                   RaceWarnTag, NewRaceVarMap, NewFunDefVars,
                                   NewFunCallVars, NewFunArgTypes,
                                   NewNestingLevel1, State),
              {DepList ++ RetDepList,  NewCurrFun1,  NewCurrFunLabel1,
               NewCalls1, NewCode1, NewRaceList1, NewRaceVarMap1,
               NewFunDefVars1, NewFunCallVars1, NewFunArgTypes1,
               NewNestingLevel2}
          end
      end
  end.

get_deplist_paths(RaceList, WarnVarArgs, RaceWarnTag, RaceVarMap, CurrLevel,
                  PublicTables, NamedTables) ->
  case RaceList of
    [] -> {[], false, true};
    [Head|Tail] ->
      case Head of
        #end_case{} ->
          {RaceList1, DepList1, IsPublic1, Continue1} =
            handle_case(Tail, WarnVarArgs, RaceWarnTag, RaceVarMap, CurrLevel,
                        PublicTables, NamedTables),
          case Continue1 of
            true ->
              {DepList2, IsPublic2, Continue2} =
                get_deplist_paths(RaceList1, WarnVarArgs, RaceWarnTag,
                                  RaceVarMap, CurrLevel, PublicTables,
                                  NamedTables),
              {DepList1 ++ DepList2, IsPublic1 orelse IsPublic2, Continue2};
            false -> {DepList1, IsPublic1, false}
          end;
        #beg_clause{} ->
          get_deplist_paths(fixup_before_case_path(Tail), WarnVarArgs,
                            RaceWarnTag, RaceVarMap, CurrLevel, PublicTables,
                            NamedTables);
        #curr_fun{status = in, var_map = RaceVarMap1} ->
          {DepList, IsPublic, Continue} =
            get_deplist_paths(Tail, WarnVarArgs, RaceWarnTag, RaceVarMap,
                              CurrLevel + 1, PublicTables, NamedTables),
          IsPublic1 =
            case RaceWarnTag of
              ?WARN_ETS_LOOKUP_INSERT ->
                [Tabs, Names, _, _] = WarnVarArgs,
                IsPublic orelse
                  lists:any(
                    fun (T) ->
                        compare_var_list(T, PublicTables, RaceVarMap1)
                    end, Tabs)
                  orelse
                  length(Names -- NamedTables) < length(Names);
              _ -> true
            end,
          {DepList, IsPublic1, Continue};
        #curr_fun{status = out, var_map = RaceVarMap1, def_vars = FunDefVars,
                  call_vars = FunCallVars} ->
          WarnVarArgs1 =
            var_analysis([format_arg(DefVar) || DefVar <- FunDefVars],
                         [format_arg(CallVar) || CallVar <- FunCallVars],
                         WarnVarArgs, RaceWarnTag),
          {WarnVarArgs2, Stop} =
            case RaceWarnTag of
              ?WARN_WHEREIS_REGISTER ->
                [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs1,
                Vars =
                  lists:flatten(
                    [find_all_bound_vars(V, RaceVarMap1) || V <- WVA1]),
                case {Vars, CurrLevel} of
                  {[], 0} ->
                    {WarnVarArgs, true};
                  {[], _} ->
                    {WarnVarArgs, false};
                  _ ->
                    {[Vars, WVA2, WVA3, WVA4], false}
                end;
              ?WARN_WHEREIS_UNREGISTER ->
                [WVA1, WVA2] = WarnVarArgs1,
                Vars =
                  lists:flatten(
                    [find_all_bound_vars(V, RaceVarMap1) || V <- WVA1]),
                case {Vars, CurrLevel} of
                  {[], 0} ->
                    {WarnVarArgs, true};
                  {[], _} ->
                    {WarnVarArgs, false};
                  _ ->
                    {[Vars, WVA2], false}
                end;
              ?WARN_ETS_LOOKUP_INSERT ->
                [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs1,
                Vars1 =
                  lists:flatten(
                    [find_all_bound_vars(V1, RaceVarMap1) || V1 <- WVA1]),
                Vars2 =
                  lists:flatten(
                    [find_all_bound_vars(V2, RaceVarMap1) || V2 <- WVA3]),
                case {Vars1, Vars2, CurrLevel} of
                  {[], _, 0} ->
                    {WarnVarArgs, true};
                  {[], _, _} ->
                    {WarnVarArgs, false};
                  {_, [], 0} ->
                    {WarnVarArgs, true};
                  {_, [], _} ->
                    {WarnVarArgs, false};
                  _ ->
                    {[Vars1, WVA2, Vars2, WVA4], false}
                end;
              ?WARN_MNESIA_DIRTY_READ_WRITE ->
                [WVA1, WVA2|T] = WarnVarArgs1,
                Vars =
                  lists:flatten(
                    [find_all_bound_vars(V, RaceVarMap1) || V <- WVA1]),
                case {Vars, CurrLevel} of
                  {[], 0} ->
                    {WarnVarArgs, true};
                  {[], _} ->
                    {WarnVarArgs, false};
                  _ ->
                    {[Vars, WVA2|T], false}
                end
            end,
          case Stop of
            true -> {[], false, false};
            false ->
              CurrLevel1 =
                case CurrLevel of
                  0 -> CurrLevel;
                  _ -> CurrLevel - 1
                end,
              get_deplist_paths(Tail, WarnVarArgs2, RaceWarnTag, RaceVarMap1,
                                CurrLevel1, PublicTables, NamedTables)
          end;
        #warn_call{call_name = RegCall, args = WarnVarArgs1,
                   var_map = RaceVarMap1} when RegCall =:= register orelse
                                               RegCall =:= unregister ->
          case compare_first_arg(WarnVarArgs, WarnVarArgs1, RaceVarMap1) of
            true -> {[], false, false};
            NewWarnVarArgs ->
              get_deplist_paths(Tail, NewWarnVarArgs, RaceWarnTag, RaceVarMap,
                                CurrLevel, PublicTables, NamedTables)
          end;
        #warn_call{call_name = ets_insert, args = WarnVarArgs1,
                   var_map = RaceVarMap1} ->
          case compare_ets_insert(WarnVarArgs, WarnVarArgs1, RaceVarMap1) of
            true -> {[], false, false};
            NewWarnVarArgs ->
              get_deplist_paths(Tail, NewWarnVarArgs, RaceWarnTag, RaceVarMap,
                                CurrLevel, PublicTables, NamedTables)
          end;
        #warn_call{call_name = mnesia_dirty_write, args = WarnVarArgs1,
                   var_map = RaceVarMap1} ->
          case compare_first_arg(WarnVarArgs, WarnVarArgs1, RaceVarMap1) of
            true -> {[], false, false};
            NewWarnVarArgs ->
              get_deplist_paths(Tail, NewWarnVarArgs, RaceWarnTag, RaceVarMap,
                                CurrLevel, PublicTables, NamedTables)
          end;
        #dep_call{var_map = RaceVarMap1} ->
          {DepList, IsPublic, Continue} =
            get_deplist_paths(Tail, WarnVarArgs, RaceWarnTag, RaceVarMap,
                              CurrLevel, PublicTables, NamedTables),
          {refine_race(Head, WarnVarArgs, RaceWarnTag, DepList, RaceVarMap1),
	   IsPublic, Continue}
     end
  end.

handle_case(RaceList, WarnVarArgs, RaceWarnTag, RaceVarMap, CurrLevel,
            PublicTables, NamedTables) ->
  case RaceList of
    [] -> {[], [], false, true};
    [Head|Tail] ->
      case Head of
        #end_clause{} ->
          {RestRaceList, DepList1, IsPublic1, Continue1} =
            do_clause(Tail, WarnVarArgs, RaceWarnTag, RaceVarMap, CurrLevel,
                      PublicTables, NamedTables),
          {RetRaceList, DepList2, IsPublic2, Continue2} =
            handle_case(RestRaceList, WarnVarArgs, RaceWarnTag, RaceVarMap,
                        CurrLevel, PublicTables, NamedTables),
          {RetRaceList, DepList1 ++ DepList2, IsPublic1 orelse IsPublic2,
           Continue1 orelse Continue2};
        beg_case -> {Tail, [], false, false}
      end
  end.

do_clause(RaceList, WarnVarArgs, RaceWarnTag, RaceVarMap, CurrLevel,
          PublicTables, NamedTables) ->
  {DepList, IsPublic, Continue} =
    get_deplist_paths(fixup_case_path(RaceList, 0), WarnVarArgs,
		      RaceWarnTag, RaceVarMap, CurrLevel,
                      PublicTables, NamedTables),
  {fixup_case_rest_paths(RaceList, 0), DepList, IsPublic, Continue}.

fixup_case_path(RaceList, NestingLevel) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel - 1, false};
          #end_case{} -> {NestingLevel + 1, false};
          #beg_clause{} ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> [];
        false -> [Head|fixup_case_path(Tail, NewNestingLevel)]
      end
  end.

%% Gets the race list before a case clause.
fixup_before_case_path(RaceList) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      case Head of
        #end_clause{} ->
          fixup_before_case_path(fixup_case_rest_paths(Tail, 0));
        beg_case -> Tail
      end
  end.

fixup_case_rest_paths(RaceList, NestingLevel) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel - 1, false};
          #end_case{} -> {NestingLevel + 1, false};
          #beg_clause{} ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> Tail;
        false -> fixup_case_rest_paths(Tail, NewNestingLevel)
      end
  end.

fixup_race_forward_helper(CurrFun, CurrFunLabel, Fun, FunLabel,
                          Calls, CallsToAnalyze, Code, RaceList,
                          InitFun, NewFunArgs, NewFunTypes,
                          RaceWarnTag, RaceVarMap, FunDefVars,
                          FunCallVars, FunArgTypes, NestingLevel,
                          Args, CodeB, StateRaceList) ->
  case Calls of
    [] ->
      {NewRaceList,
       #curr_fun{mfa = NewCurrFun, label = NewCurrFunLabel,
                 var_map = NewRaceVarMap, def_vars = NewFunDefVars,
                 call_vars = NewFunCallVars, arg_types = NewFunArgTypes},
       NewCode, NewNestingLevel} =
        remove_clause(RaceList,
        #curr_fun{mfa = CurrFun, label = CurrFunLabel, var_map = RaceVarMap,
                  def_vars = FunDefVars, call_vars = FunCallVars,
                  arg_types = FunArgTypes},
        Code, NestingLevel),
      {NewCurrFun, NewCurrFunLabel, CallsToAnalyze, NewCode, NewRaceList,
       NewRaceVarMap, NewFunDefVars, NewFunCallVars, NewFunArgTypes,
       NewNestingLevel};
    [Head|Tail] ->
      case Head of
        {InitFun, InitFun} when CurrFun =:= InitFun, Fun =:= InitFun ->
          NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
          NewRaceVarMap =
            race_var_map(Args, NewFunArgs, RaceVarMap, bind),
          RetC =
            fixup_all_calls(InitFun, InitFun, FunLabel, Args,
            CodeB ++
            [#curr_fun{status = out, mfa =  InitFun,
                       label = CurrFunLabel, var_map = RaceVarMap,
                       def_vars = FunDefVars, call_vars = FunCallVars,
                       arg_types = FunArgTypes}],
            Code, RaceVarMap),
          NewCode =
            fixup_all_calls(InitFun, InitFun, FunLabel, Args,
            CodeB ++
            [#curr_fun{status = out, mfa =  InitFun,
                       label = CurrFunLabel, var_map = NewRaceVarMap,
                       def_vars = Args, call_vars = NewFunArgs,
                       arg_types = NewFunTypes}],
            [#curr_fun{status = in, mfa = Fun,
                       label = FunLabel, var_map = NewRaceVarMap,
                       def_vars = Args, call_vars = NewFunArgs,
                       arg_types = NewFunTypes}|
            lists:reverse(StateRaceList)] ++
            RetC, NewRaceVarMap),
          {InitFun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
           NewRaceVarMap, Args, NewFunArgs, NewFunTypes, NestingLevel};
        {CurrFun, Fun} ->
          NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
          NewRaceVarMap = race_var_map(Args, NewFunArgs, RaceVarMap, bind),
          RetC =
            case Fun of
              InitFun ->
                fixup_all_calls(CurrFun, Fun, FunLabel, Args,
                  lists:reverse(StateRaceList) ++
                  [#curr_fun{status = out, mfa = CurrFun,
                             label = CurrFunLabel, var_map = RaceVarMap,
                             def_vars = FunDefVars, call_vars = FunCallVars,
                             arg_types = FunArgTypes}],
                  Code, RaceVarMap);
              _Other1 ->
                fixup_all_calls(CurrFun, Fun, FunLabel, Args,
                  CodeB ++
                  [#curr_fun{status = out, mfa = CurrFun,
                             label = CurrFunLabel, var_map = RaceVarMap,
                             def_vars = FunDefVars, call_vars = FunCallVars,
                             arg_types = FunArgTypes}],
                  Code, RaceVarMap)
            end,
          NewCode =
            case Fun of
              InitFun ->
                [#curr_fun{status = in, mfa = Fun,
                           label = FunLabel, var_map = NewRaceVarMap,
                           def_vars = Args, call_vars = NewFunArgs,
                           arg_types = NewFunTypes}|
                 lists:reverse(StateRaceList)] ++ RetC;
              _ ->
                [#curr_fun{status = in, mfa = Fun,
                           label = FunLabel, var_map = NewRaceVarMap,
                           def_vars = Args, call_vars = NewFunArgs,
                           arg_types = NewFunTypes}|CodeB] ++
                  RetC
            end,
          {Fun, FunLabel, NewCallsToAnalyze, NewCode, RaceList, NewRaceVarMap,
           Args, NewFunArgs, NewFunTypes, NestingLevel};
	{_TupleA, _TupleB} ->
	  fixup_race_forward_helper(CurrFun, CurrFunLabel, Fun, FunLabel,
            Tail, CallsToAnalyze, Code, RaceList, InitFun, NewFunArgs,
            NewFunTypes, RaceWarnTag, RaceVarMap, FunDefVars, FunCallVars,
            FunArgTypes, NestingLevel, Args, CodeB, StateRaceList)
      end
  end.

%%% ===========================================================================
%%%
%%%  Backward Analysis
%%%
%%% ===========================================================================

fixup_race_backward(CurrFun, Calls, CallsToAnalyze, Parents, Height) ->
  case Height =:= 0 of
    true -> Parents;
    false ->
      case Calls of
        [] ->
          case is_integer(CurrFun) orelse lists:member(CurrFun, Parents) of
            true -> Parents;
            false -> [CurrFun|Parents]
          end;
        [Head|Tail] ->
	  {Parent, TupleB} = Head,
	  case TupleB =:= CurrFun of
            true ->  % more paths are needed
              NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
              NewParents =
                fixup_race_backward(Parent, NewCallsToAnalyze,
				    NewCallsToAnalyze, Parents, Height - 1),
              fixup_race_backward(CurrFun, Tail, NewCallsToAnalyze, NewParents,
				  Height);
            false ->
              fixup_race_backward(CurrFun, Tail, CallsToAnalyze, Parents,
                                  Height)
          end
      end
  end.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

are_bound_labels(Label1, Label2, RaceVarMap) ->
  case dict:find(Label1, RaceVarMap) of
    error -> false;
    {ok, Labels} ->
      lists:member(Label2, Labels) orelse
        are_bound_labels_helper(Labels, Label1, Label2, RaceVarMap)
  end.

are_bound_labels_helper(Labels, OldLabel, CompLabel, RaceVarMap) ->
  case dict:size(RaceVarMap) of
    0 -> false;
    _ ->
      case Labels of
        [] -> false;
        [Head|Tail] ->
          NewRaceVarMap = dict:erase(OldLabel, RaceVarMap),
          are_bound_labels(Head, CompLabel, NewRaceVarMap) orelse
	    are_bound_labels_helper(Tail, Head, CompLabel, NewRaceVarMap)
      end
  end.

are_bound_vars(Vars1, Vars2, RaceVarMap) ->
  case is_list(Vars1) andalso is_list(Vars2) of
    true ->
      case Vars1 of
	[] -> false;
	[AHead|ATail] ->
	  case Vars2 of
	    [] -> false;
	    [PHead|PTail] ->
	      are_bound_vars(AHead, PHead, RaceVarMap) andalso
		are_bound_vars(ATail, PTail, RaceVarMap)
	  end
      end;
    false ->
      {NewVars1, NewVars2, IsList} =
	case is_list(Vars1) of
	  true ->
	    case Vars1 of
	      [Var1] -> {Var1, Vars2, true};
	      _Thing -> {Vars1, Vars2, false}
	    end;
	  false ->
	    case is_list(Vars2) of
	      true ->
		case Vars2 of
		  [Var2] -> {Vars1, Var2, true};
		  _Thing -> {Vars1, Vars2, false}
		end;
	      false -> {Vars1, Vars2, true}
	    end
	end,
      case IsList of
	true ->
	  case cerl:type(NewVars1) of
	    var ->
	      case cerl:type(NewVars2) of
		var ->
		  ALabel = cerl_trees:get_label(NewVars1),
		  PLabel = cerl_trees:get_label(NewVars2),
		  are_bound_labels(ALabel, PLabel, RaceVarMap) orelse
		    are_bound_labels(PLabel, ALabel, RaceVarMap);
		alias ->
		  are_bound_vars(NewVars1, cerl:alias_var(NewVars2),
				 RaceVarMap);
		values ->
		  are_bound_vars(NewVars1, cerl:values_es(NewVars2),
				 RaceVarMap);
		_Other -> false
	      end;
	    tuple ->
	      case cerl:type(NewVars2) of
		tuple ->
		  are_bound_vars(cerl:tuple_es(NewVars1),
				 cerl:tuple_es(NewVars2), RaceVarMap);
		alias ->
		  are_bound_vars(NewVars1, cerl:alias_var(NewVars2),
				 RaceVarMap);
		values ->
		  are_bound_vars(NewVars1, cerl:values_es(NewVars2),
				 RaceVarMap);
		_Other -> false
	      end;
	    cons ->
	      case cerl:type(NewVars2) of
		cons ->
		  are_bound_vars(cerl:cons_hd(NewVars1),
				 cerl:cons_hd(NewVars2), RaceVarMap)
		    andalso
		    are_bound_vars(cerl:cons_tl(NewVars1),
				   cerl:cons_tl(NewVars2), RaceVarMap);
		alias ->
		  are_bound_vars(NewVars1, cerl:alias_var(NewVars2),
				 RaceVarMap);
		values ->
		  are_bound_vars(NewVars1, cerl:values_es(NewVars2),
				 RaceVarMap);
		_Other -> false
	      end;
	    alias ->
	      case cerl:type(NewVars2) of
		alias ->
		  are_bound_vars(cerl:alias_var(NewVars1),
				 cerl:alias_var(NewVars2), RaceVarMap);
		_Other ->
		  are_bound_vars(cerl:alias_var(NewVars1),
				 NewVars2, RaceVarMap)
	      end;
	    values ->
	      case cerl:type(NewVars2) of
		values ->
		  are_bound_vars(cerl:values_es(NewVars1),
				 cerl:values_es(NewVars2), RaceVarMap);
		_Other ->
		  are_bound_vars(cerl:values_es(NewVars1),
				 NewVars2, RaceVarMap)
	      end;
	    _Other -> false
	  end;
	false -> false
      end
  end.

callgraph__renew_tables(Table, Callgraph) ->
  case Table of
    {named, NameLabel, Names} ->
      PTablesToAdd =
        case NameLabel of
          ?no_label -> [];
          _Other -> [NameLabel]
        end,
      NamesToAdd = filter_named_tables(Names),
      PTables = dialyzer_callgraph:get_public_tables(Callgraph),
      NTables = dialyzer_callgraph:get_named_tables(Callgraph),
      dialyzer_callgraph:put_public_tables(
        lists:usort(PTablesToAdd ++ PTables),
        dialyzer_callgraph:put_named_tables(
        NamesToAdd ++ NTables, Callgraph));
    _Other ->
      Callgraph
  end.

cleanup_clause_code(#curr_fun{mfa = CurrFun} = CurrTuple, Code,
                    NestingLevel, LocalNestingLevel) ->
  case Code of
    [] -> {CurrTuple, []};
    [Head|Tail] ->
      {NewLocalNestingLevel, NewNestingLevel, NewCurrTuple, Return} =
        case Head of
          beg_case ->
            {LocalNestingLevel, NestingLevel + 1, CurrTuple, false};
          #end_case{} ->
            {LocalNestingLevel, NestingLevel - 1, CurrTuple, false};
          #end_clause{} ->
            case NestingLevel =:= 0 of
              true ->
                {LocalNestingLevel, NestingLevel, CurrTuple, true};
              false ->
                {LocalNestingLevel, NestingLevel, CurrTuple, false}
            end;
          #fun_call{caller = CurrFun} ->
            {LocalNestingLevel - 1, NestingLevel, CurrTuple, false};
          #curr_fun{status = in} ->
            {LocalNestingLevel - 1, NestingLevel, Head, false};
          #curr_fun{status = out} ->
            {LocalNestingLevel + 1, NestingLevel, Head, false};
          Other when Other =/= #fun_call{} ->
            {LocalNestingLevel, NestingLevel, CurrTuple, false}
        end,
      case Return of
        true -> {NewCurrTuple, Tail};
        false ->
          cleanup_clause_code(NewCurrTuple, Tail, NewNestingLevel,
                              NewLocalNestingLevel)
      end
  end.

cleanup_dep_calls(DepList) ->
  case DepList of
    [] -> [];
    [#dep_call{call_name = CallName, arg_types = ArgTypes,
               vars = Vars, state = State, file_line = FileLine}|T] ->
      [#dep_call{call_name = CallName, arg_types = ArgTypes,
                 vars = Vars, state = State, file_line = FileLine}|
       cleanup_dep_calls(T)]
  end.

cleanup_race_code(State) ->
  Callgraph = dialyzer_dataflow:state__get_callgraph(State),
  dialyzer_dataflow:state__put_callgraph(
    dialyzer_callgraph:race_code_new(Callgraph), State).

filter_named_tables(NamesList) ->
  case NamesList of
    [] -> [];
    [Head|Tail] ->
      NewHead =
        case string:find(Head, "()", trailing) of
          nomatch -> [Head];
          _Other -> []
        end,
      NewHead ++ filter_named_tables(Tail)
  end.

filter_parents(Parents, NewParents, Digraph) ->
  case Parents of
    [] -> NewParents;
    [Head|Tail] ->
      NewParents1 = filter_parents_helper1(Head, Tail, NewParents, Digraph),
      filter_parents(Tail, NewParents1, Digraph)
  end.

filter_parents_helper1(First, Rest, NewParents, Digraph) ->
  case Rest of
    [] -> NewParents;
    [Head|Tail] ->
      NewParents1 = filter_parents_helper2(First, Head, NewParents, Digraph),
      filter_parents_helper1(First, Tail, NewParents1, Digraph)
  end.

filter_parents_helper2(Parent1, Parent2, NewParents, Digraph) ->
  case digraph:get_path(Digraph, Parent1, Parent2) of
    false ->
      case digraph:get_path(Digraph, Parent2, Parent1) of
        false -> NewParents;
        _Vertices -> NewParents -- [Parent1]
      end;
    _Vertices -> NewParents -- [Parent2]
  end.

find_all_bound_vars(Label, RaceVarMap) ->
  case dict:find(Label, RaceVarMap) of
    error -> [Label];
    {ok, Labels} ->
      lists:usort(Labels ++
                  find_all_bound_vars_helper(Labels, Label, RaceVarMap))
  end.

find_all_bound_vars_helper(Labels, Label, RaceVarMap) ->
  case dict:size(RaceVarMap) of
    0 -> [];
    _ ->
      case Labels of
        [] -> [];
        [Head|Tail] ->
          NewRaceVarMap = dict:erase(Label, RaceVarMap),
          find_all_bound_vars(Head, NewRaceVarMap) ++
	    find_all_bound_vars_helper(Tail, Head, NewRaceVarMap)
      end
  end.

fixup_all_calls(CurrFun, NextFun, NextFunLabel, Args, CodeToReplace,
                Code, RaceVarMap) ->
  case Code of
    [] -> [];
    [Head|Tail] ->
      NewCode =
        case Head of
          #fun_call{caller = CurrFun, callee = Callee,
                    arg_types = FunArgTypes, vars = FunArgs}
          when Callee =:= NextFun orelse Callee =:= NextFunLabel ->
            RaceVarMap1 = race_var_map(Args, FunArgs, RaceVarMap, bind),
            [#curr_fun{status = in, mfa = NextFun, label = NextFunLabel,
                       var_map = RaceVarMap1, def_vars = Args,
                       call_vars = FunArgs, arg_types = FunArgTypes}|
              CodeToReplace];
          _Other -> [Head]
        end,
      RetCode =
        fixup_all_calls(CurrFun, NextFun, NextFunLabel, Args, CodeToReplace,
                        Tail, RaceVarMap),
      NewCode ++ RetCode
  end.

is_last_race(RaceTag, InitFun, Code, Callgraph) ->
  case Code of
    [] -> true;
    [Head|Tail] ->
      case Head of
        RaceTag -> false;
        #fun_call{callee = Fun} ->
          FunName =
            case is_integer(Fun) of
              true ->
                case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
                  error -> Fun;
                  {ok, Name} -> Name
                end;
              false -> Fun
            end,
          Digraph = dialyzer_callgraph:get_digraph(Callgraph),
          case FunName =:= InitFun orelse
               digraph:get_path(Digraph, FunName, InitFun) of
            false -> is_last_race(RaceTag, InitFun, Tail, Callgraph);
            _Vertices -> false
          end;
        _Other -> is_last_race(RaceTag, InitFun, Tail, Callgraph)
      end
  end.

lists_key_member(Member, List, N) when is_integer(Member) ->
  case List of
    [] -> 0;
    [Head|Tail] ->
      NewN = N + 1,
      case Head of
        Member -> NewN;
        _Other -> lists_key_member(Member, Tail, NewN)
      end
  end;
lists_key_member(_M, _L, _N) ->
  0.

lists_key_member_lists(MemberList, List) ->
  case MemberList of
    [] -> 0;
    [Head|Tail] ->
      case lists_key_member(Head, List, 0) of
        0 -> lists_key_member_lists(Tail, List);
        Other -> Other
      end
  end.

lists_key_members_lists(MemberList, List) ->
  case MemberList of
    [] -> [];
    [Head|Tail] ->
      lists:usort(
        lists_key_members_lists_helper(Head, List, 1) ++
        lists_key_members_lists(Tail, List))
  end.

lists_key_members_lists_helper(Elem, List, N) when is_integer(Elem) ->
  case List of
    [] -> [];
    [Head|Tail] ->
      NewHead =
        case Head =:= Elem of
          true -> [N];
          false -> []
        end,
      NewHead ++ lists_key_members_lists_helper(Elem, Tail, N + 1)
  end;
lists_key_members_lists_helper(_Elem, _List, _N) ->
  [0].

lists_key_replace(N, List, NewMember) ->
  {Before, [_|After]} = lists:split(N - 1, List),
  Before ++ [NewMember|After].

lists_get(0, _List) -> ?no_label;
lists_get(N, List) -> lists:nth(N, List).

refine_race(RaceCall, WarnVarArgs, RaceWarnTag, DependencyList, RaceVarMap) ->
  case RaceWarnTag of
    WarnWhereis when WarnWhereis =:= ?WARN_WHEREIS_REGISTER orelse
                     WarnWhereis =:= ?WARN_WHEREIS_UNREGISTER ->
      case RaceCall of
        #dep_call{call_name = ets_lookup} ->
          DependencyList;
        #dep_call{call_name = mnesia_dirty_read} ->
          DependencyList;
        #dep_call{call_name = whereis, args = VarArgs} ->
          refine_race_helper(RaceCall, VarArgs, WarnVarArgs, RaceWarnTag,
                             DependencyList, RaceVarMap)
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      case RaceCall of
        #dep_call{call_name = whereis} ->
          DependencyList;
        #dep_call{call_name = mnesia_dirty_read} ->
          DependencyList;
        #dep_call{call_name = ets_lookup, args = VarArgs} ->
          refine_race_helper(RaceCall, VarArgs, WarnVarArgs, RaceWarnTag,
                             DependencyList, RaceVarMap)
      end;
    ?WARN_MNESIA_DIRTY_READ_WRITE ->
      case RaceCall of
        #dep_call{call_name = whereis} ->
          DependencyList;
        #dep_call{call_name = ets_lookup} ->
          DependencyList;
        #dep_call{call_name = mnesia_dirty_read, args = VarArgs} ->
          refine_race_helper(RaceCall, VarArgs, WarnVarArgs, RaceWarnTag,
                             DependencyList, RaceVarMap)
      end
  end.

refine_race_helper(RaceCall, VarArgs, WarnVarArgs, RaceWarnTag, DependencyList,
                   RaceVarMap) ->
  case compare_types(VarArgs, WarnVarArgs, RaceWarnTag, RaceVarMap) of
    true -> [RaceCall|DependencyList];
    false -> DependencyList
  end.

remove_clause(RaceList, CurrTuple, Code, NestingLevel) ->
  NewRaceList = fixup_case_rest_paths(RaceList, 0),
  {NewCurrTuple, NewCode} =
    cleanup_clause_code(CurrTuple, Code, 0, NestingLevel),
  ReturnTuple = {NewRaceList, NewCurrTuple, NewCode, NestingLevel},
  case NewRaceList of
    [beg_case|RTail] ->
      case NewCode of
        [#end_case{}|CTail] ->
          remove_clause(RTail, NewCurrTuple, CTail, NestingLevel);
        _Other -> ReturnTuple
      end;
    _Else -> ReturnTuple
  end.

remove_nonlocal_functions(Code, NestingLevel) ->
  case Code of
    [] -> [];
    [H|T] ->
      NewNL =
        case H of
          #curr_fun{status = in} ->
            NestingLevel + 1;
          #curr_fun{status = out} ->
            NestingLevel - 1;
          _Other ->
            NestingLevel
        end,
      case NewNL =:= 0 of
        true -> T;
        false -> remove_nonlocal_functions(T, NewNL)
      end
  end.

renew_curr_fun(CurrFun, Races) ->
  Races#races{curr_fun = CurrFun}.

renew_curr_fun_label(CurrFunLabel, Races) ->
  Races#races{curr_fun_label = CurrFunLabel}.

renew_race_list(RaceList, Races) ->
  Races#races{race_list = RaceList}.

renew_race_list_size(RaceListSize, Races) ->
  Races#races{race_list_size = RaceListSize}.

renew_race_tags(RaceTags, Races) ->
  Races#races{race_tags = RaceTags}.

renew_table(Table, Races) ->
  Races#races{new_table = Table}.

state__renew_curr_fun(CurrFun, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  dialyzer_dataflow:state__put_races(renew_curr_fun(CurrFun, Races), State).

state__renew_curr_fun_label(CurrFunLabel, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  dialyzer_dataflow:state__put_races(
    renew_curr_fun_label(CurrFunLabel, Races), State).

state__renew_race_list(RaceList, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  dialyzer_dataflow:state__put_races(renew_race_list(RaceList, Races), State).

state__renew_race_tags(RaceTags, State) ->
  Races = dialyzer_dataflow:state__get_races(State),
  dialyzer_dataflow:state__put_races(renew_race_tags(RaceTags, Races), State).

state__renew_info(RaceList, RaceListSize, RaceTags, Table, State) ->
  Callgraph = dialyzer_dataflow:state__get_callgraph(State),
  Races = dialyzer_dataflow:state__get_races(State),
  dialyzer_dataflow:state__put_callgraph(
    callgraph__renew_tables(Table, Callgraph),
    dialyzer_dataflow:state__put_races(
      renew_table(Table,
      renew_race_list(RaceList,
      renew_race_list_size(RaceListSize,
      renew_race_tags(RaceTags, Races)))), State)).

%%% ===========================================================================
%%%
%%%  Variable and Type Utilities
%%%
%%% ===========================================================================

any_args(StrList) ->
  case StrList of
    [] -> false;
    [Head|Tail] ->
      case string:find(Head, "()", trailing) of
        nomatch -> any_args(Tail);
        _Other -> true
      end
  end.

-spec bind_dict_vars(label(), label(), dict:dict()) -> dict:dict().

bind_dict_vars(Key, Label, RaceVarMap) ->
  case Key =:= Label of
    true -> RaceVarMap;
    false ->
      case dict:find(Key, RaceVarMap) of
	error -> dict:store(Key, [Label], RaceVarMap);
	{ok, Labels} ->
	  case lists:member(Label, Labels) of
	    true -> RaceVarMap;
	    false -> dict:store(Key, [Label|Labels], RaceVarMap)
	  end
      end
  end.

bind_dict_vars_list(Key, Labels, RaceVarMap) ->
  case Labels of
    [] -> RaceVarMap;
    [Head|Tail] ->
      bind_dict_vars_list(Key, Tail, bind_dict_vars(Key, Head, RaceVarMap))
  end.

compare_ets_insert(OldWarnVarArgs, NewWarnVarArgs, RaceVarMap) ->
  [Old1, Old2, Old3, Old4] = OldWarnVarArgs,
  [New1, New2, New3, New4] = NewWarnVarArgs,
  Bool =
    case any_args(Old2) of
      true -> compare_var_list(New1, Old1, RaceVarMap);
      false ->
        case any_args(New2) of
          true -> compare_var_list(New1, Old1, RaceVarMap);
          false -> compare_var_list(New1, Old1, RaceVarMap)
                     orelse (Old2 =:= New2)
        end
    end,
  case Bool of
    true ->
      case any_args(Old4) of
        true ->
          case compare_list_vars(Old3, ets_list_args(New3), [], RaceVarMap) of
            true -> true;
            Args3 -> lists_key_replace(3, OldWarnVarArgs, Args3)
          end;
        false ->
           case any_args(New4) of
             true ->
               case compare_list_vars(Old3, ets_list_args(New3), [],
                                      RaceVarMap) of
                 true -> true;
                 Args3 -> lists_key_replace(3, OldWarnVarArgs, Args3)
               end;
             false ->
               case compare_list_vars(Old3, ets_list_args(New3), [],
                                      RaceVarMap) of
                 true -> true;
                 Args3 ->
                   lists_key_replace(4,
                     lists_key_replace(3, OldWarnVarArgs, Args3), Old4 -- New4)
               end
           end
      end;
    false -> OldWarnVarArgs
  end.

compare_first_arg(OldWarnVarArgs, NewWarnVarArgs, RaceVarMap) ->
  [Old1, Old2|_OldT] = OldWarnVarArgs,
  [New1, New2|_NewT] = NewWarnVarArgs,
  case any_args(Old2) of
    true ->
      case compare_var_list(New1, Old1, RaceVarMap) of
        true -> true;
        false -> OldWarnVarArgs
      end;
    false ->
      case any_args(New2) of
        true ->
          case compare_var_list(New1, Old1, RaceVarMap) of
            true -> true;
            false -> OldWarnVarArgs
          end;
        false ->
          case compare_var_list(New1, Old1, RaceVarMap) of
            true -> true;
            false -> lists_key_replace(2, OldWarnVarArgs, Old2 -- New2)
          end
      end
  end.

compare_argtypes(ArgTypes, WarnArgTypes) ->
  lists:any(fun (X) -> lists:member(X, WarnArgTypes) end, ArgTypes).

%% Compares the argument types of the two suspicious calls.
compare_types(VarArgs, WarnVarArgs, RaceWarnTag, RaceVarMap) ->
  case RaceWarnTag of
    ?WARN_WHEREIS_REGISTER ->
      [VA1, VA2] = VarArgs,
      [WVA1, WVA2, _, _] = WarnVarArgs,
      case any_args(VA2) of
        true -> compare_var_list(VA1, WVA1, RaceVarMap);
        false ->
          case any_args(WVA2) of
            true -> compare_var_list(VA1, WVA1, RaceVarMap);
            false ->
              compare_var_list(VA1, WVA1, RaceVarMap) orelse
                compare_argtypes(VA2, WVA2)
          end
      end;
    ?WARN_WHEREIS_UNREGISTER ->
      [VA1, VA2] = VarArgs,
      [WVA1, WVA2] = WarnVarArgs,
      case any_args(VA2) of
        true -> compare_var_list(VA1, WVA1, RaceVarMap);
        false ->
          case any_args(WVA2) of
            true -> compare_var_list(VA1, WVA1, RaceVarMap);
            false ->
              compare_var_list(VA1, WVA1, RaceVarMap) orelse
                compare_argtypes(VA2, WVA2)
          end
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      [VA1, VA2, VA3, VA4] = VarArgs,
      [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs,
      Bool =
        case any_args(VA2) of
          true -> compare_var_list(VA1, WVA1, RaceVarMap);
          false ->
            case any_args(WVA2) of
              true -> compare_var_list(VA1, WVA1, RaceVarMap);
              false ->
                compare_var_list(VA1, WVA1, RaceVarMap) orelse
                  compare_argtypes(VA2, WVA2)
            end
        end,
      Bool andalso
        (case any_args(VA4) of
           true ->
             compare_var_list(VA3, WVA3, RaceVarMap);
           false ->
             case any_args(WVA4) of
               true ->
                 compare_var_list(VA3, WVA3, RaceVarMap);
               false ->
                 compare_var_list(VA3, WVA3, RaceVarMap) orelse
                   compare_argtypes(VA4, WVA4)
             end
         end);
    ?WARN_MNESIA_DIRTY_READ_WRITE ->
      [VA1, VA2|_] = VarArgs, %% Two or four elements
      [WVA1, WVA2|_] = WarnVarArgs,
      case any_args(VA2) of
        true -> compare_var_list(VA1, WVA1, RaceVarMap);
        false ->
          case any_args(WVA2) of
            true -> compare_var_list(VA1, WVA1, RaceVarMap);
            false ->
              compare_var_list(VA1, WVA1, RaceVarMap) orelse
                compare_argtypes(VA2, WVA2)
          end
      end
  end.

compare_list_vars(VarList1, VarList2, NewVarList1, RaceVarMap) ->
  case VarList1 of
    [] ->
      case NewVarList1 of
        [] -> true;
        _Other -> NewVarList1
      end;
    [Head|Tail] ->
      NewHead =
        case compare_var_list(Head, VarList2, RaceVarMap) of
          true -> [];
          false -> [Head]
        end,
      compare_list_vars(Tail, VarList2, NewHead ++ NewVarList1, RaceVarMap)
  end.

compare_vars(Var1, Var2, RaceVarMap) when is_integer(Var1), is_integer(Var2) ->
  Var1 =:= Var2 orelse
    are_bound_labels(Var1, Var2, RaceVarMap) orelse
    are_bound_labels(Var2, Var1, RaceVarMap);
compare_vars(_Var1, _Var2, _RaceVarMap) ->
  false.

-spec compare_var_list(label_type(), [label_type()], dict:dict()) -> boolean().

compare_var_list(Var, VarList, RaceVarMap) ->
  lists:any(fun (V) -> compare_vars(Var, V, RaceVarMap) end, VarList).

ets_list_args(MaybeList) ->
  case is_list(MaybeList) of
    true ->
      try [ets_tuple_args(T) || T <- MaybeList]
      catch _:_ -> [?no_label]
      end;
    false -> [ets_tuple_args(MaybeList)]
  end.

ets_list_argtypes(ListStr) ->
  ListStr1 = string:trim(ListStr, leading, "$["),
  string:trim(ListStr1, trailing, "$]$.$,").

ets_tuple_args(MaybeTuple) ->
  case is_tuple(MaybeTuple) of
    true -> element(1, MaybeTuple);
    false -> ?no_label
  end.

ets_tuple_argtypes2(TupleList, ElemList) ->
  case TupleList of
    [] -> ElemList;
    [H|T] ->
      ets_tuple_argtypes2(T,
                          ets_tuple_argtypes2_helper(H, [], 0) ++ ElemList)
  end.

ets_tuple_argtypes2_helper(TupleStr, ElemStr, NestingLevel) ->
  case TupleStr of
    [] -> [];
    [H|T] ->
      {NewElemStr, NewNestingLevel, Return} =
        case H of
          ${ when NestingLevel =:= 0 ->
            {ElemStr, NestingLevel + 1, false};
          ${ ->
            {[H|ElemStr], NestingLevel + 1, false};
          $[ ->
            {[H|ElemStr], NestingLevel + 1, false};
          $( ->
            {[H|ElemStr], NestingLevel + 1, false};
          $} ->
            {[H|ElemStr], NestingLevel - 1, false};
          $] ->
            {[H|ElemStr], NestingLevel - 1, false};
          $) ->
            {[H|ElemStr], NestingLevel - 1, false};
          $, when NestingLevel =:= 1 ->
            {lists:reverse(ElemStr), NestingLevel, true};
          _Other ->
            {[H|ElemStr], NestingLevel, false}
        end,
      case Return of
        true -> string:lexemes(NewElemStr, " |");
        false ->
          ets_tuple_argtypes2_helper(T, NewElemStr, NewNestingLevel)
      end
  end.

ets_tuple_argtypes1(Str, Tuple, TupleList, NestingLevel) ->
  case Str of
    [] -> TupleList;
    [H|T] ->
      {NewTuple, NewNestingLevel, Add} =
        case H of
          ${ ->
            {[H|Tuple], NestingLevel + 1, false};
          $} ->
            case NestingLevel of
              1 ->
                {[H|Tuple], NestingLevel - 1, true};
              _Else ->
                {[H|Tuple], NestingLevel - 1, false}
            end;
          _Other1 when NestingLevel =:= 0 ->
            {Tuple, NestingLevel, false};
          _Other2 ->
            {[H|Tuple], NestingLevel, false}
        end,
        case Add of
          true ->
            ets_tuple_argtypes1(T, [],
                                [lists:reverse(NewTuple)|TupleList],
                                NewNestingLevel);
          false ->
            ets_tuple_argtypes1(T, NewTuple, TupleList, NewNestingLevel)
        end
  end.

format_arg(?bypassed) -> ?no_label;
format_arg(Arg0) ->
  Arg = cerl:fold_literal(Arg0),
  case cerl:type(Arg) of
    var -> cerl_trees:get_label(Arg);
    tuple -> list_to_tuple([format_arg(A) || A <- cerl:tuple_es(Arg)]);
    cons -> [format_arg(cerl:cons_hd(Arg))|format_arg(cerl:cons_tl(Arg))];
    alias -> format_arg(cerl:alias_var(Arg));
    literal ->
      case cerl:is_c_nil(Arg) of
        true -> [];
        false -> ?no_label
      end;
    _Other -> ?no_label
  end.

-spec format_args([core_vars()], [erl_types:erl_type()],
                  dialyzer_dataflow:state(), call()) ->
  args().

format_args([], [], _State, _Call) ->
  [];
format_args(ArgList, TypeList, CleanState, Call) ->
  format_args_2(format_args_1(ArgList, TypeList, CleanState), Call).

format_args_1([Arg], [Type], CleanState) ->
  [format_arg(Arg), format_type(Type, CleanState)];
format_args_1([Arg|Args], [Type|Types], CleanState) ->
  List =
    case Arg =:= ?bypassed of
      true -> [?no_label, format_type(Type, CleanState)];
      false ->
        case cerl:is_literal(cerl:fold_literal(Arg)) of
          true -> [?no_label, format_cerl(Arg)];
          false -> [format_arg(Arg), format_type(Type, CleanState)]
        end
    end,
  List ++ format_args_1(Args, Types, CleanState).

format_args_2(StrArgList, Call) ->
  case Call of
    whereis ->
      lists_key_replace(2, StrArgList,
	string:lexemes(lists:nth(2, StrArgList), " |"));
    register ->
      lists_key_replace(2, StrArgList,
	string:lexemes(lists:nth(2, StrArgList), " |"));
    unregister ->
      lists_key_replace(2, StrArgList,
	string:lexemes(lists:nth(2, StrArgList), " |"));
    ets_new ->
      StrArgList1 = lists_key_replace(2, StrArgList,
	string:lexemes(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        string:lexemes(ets_list_argtypes(lists:nth(4, StrArgList1)), " |"));
    ets_lookup ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        string:lexemes(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        string:lexemes(lists:nth(4, StrArgList1), " |"));
    ets_insert ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        string:lexemes(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        ets_tuple_argtypes2(
        ets_tuple_argtypes1(lists:nth(4, StrArgList1), [], [], 0),
        []));
    mnesia_dirty_read1 ->
      lists_key_replace(2, StrArgList,
        [mnesia_tuple_argtypes(T) || T <- string:lexemes(
        lists:nth(2, StrArgList), " |")]);
    mnesia_dirty_read2 ->
      lists_key_replace(2, StrArgList,
        string:lexemes(lists:nth(2, StrArgList), " |"));
    mnesia_dirty_write1 ->
      lists_key_replace(2, StrArgList,
        [mnesia_record_tab(R) || R <- string:lexemes(
        lists:nth(2, StrArgList), " |")]);
    mnesia_dirty_write2 ->
      lists_key_replace(2, StrArgList,
        string:lexemes(lists:nth(2, StrArgList), " |"));
    function_call -> StrArgList
  end.

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []),
                       [{hook, dialyzer_utils:pp_hook()},
                        {noann, true},
                        {paper, 100000},
                        {ribbon, 100000}
                       ]).

format_type(Type, State) ->
  R = dialyzer_dataflow:state__get_records(State),
  erl_types:t_to_string(Type, R).

mnesia_record_tab(RecordStr) ->
  case erl_scan:string(RecordStr) of
    {ok, [{'#', _}, {atom, _, Name}|_], _} ->
      io_lib:write_string(atom_to_list(Name), $');
    _ -> RecordStr
  end.

mnesia_tuple_argtypes(TupleStr) ->
  TupleStr1 = string:trim(TupleStr, leading, "${"),
  [TupleStr2|_T] = string:lexemes(TupleStr1, " ,"),
  lists:flatten(string:lexemes(TupleStr2, " |")).

-spec race_var_map(var_to_map1(), var_to_map2(), dict:dict(), op()) ->
        dict:dict().

race_var_map(Vars1, Vars2, RaceVarMap, Op) ->
  case Vars1 =:= ?no_arg orelse Vars1 =:= ?bypassed
                         orelse Vars2 =:= ?bypassed of
    true -> RaceVarMap;
    false ->
      case is_list(Vars1) andalso is_list(Vars2) of
        true ->
          case Vars1 of
            [] -> RaceVarMap;
            [AHead|ATail] ->
              case Vars2 of
                [] -> RaceVarMap;
                [PHead|PTail] ->
                  NewRaceVarMap = race_var_map(AHead, PHead, RaceVarMap, Op),
                  race_var_map(ATail, PTail, NewRaceVarMap, Op)
              end
          end;
        false ->
          {NewVars1, NewVars2, Bool} =
            case is_list(Vars1) of
              true ->
                case Vars1 of
                  [Var1] -> {Var1, Vars2, true};
                  _Thing -> {Vars1, Vars2, false}
                end;
              false ->
                case is_list(Vars2) of
                  true ->
                    case Vars2 of
                      [Var2] -> {Vars1, Var2, true};
                      _Thing -> {Vars1, Vars2, false}
                    end;
                  false -> {Vars1, Vars2, true}
                end
            end,
          case Bool of
            true ->
              case cerl:type(NewVars1) of
                var ->
                  case cerl:type(NewVars2) of
                    var ->
                      ALabel = cerl_trees:get_label(NewVars1),
                      PLabel = cerl_trees:get_label(NewVars2),
                      case Op of
                        bind ->
                          TempRaceVarMap =
                            bind_dict_vars(ALabel, PLabel, RaceVarMap),
                          bind_dict_vars(PLabel, ALabel, TempRaceVarMap);
                        unbind ->
                          TempRaceVarMap =
                            unbind_dict_vars(ALabel, PLabel, RaceVarMap),
                          unbind_dict_vars(PLabel, ALabel, TempRaceVarMap)
                      end;
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
				   RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
				   RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                tuple ->
                  case cerl:type(NewVars2) of
                    tuple ->
                      race_var_map(cerl:tuple_es(NewVars1),
				   cerl:tuple_es(NewVars2), RaceVarMap, Op);
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
				   RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
				   RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                cons ->
                  case cerl:type(NewVars2) of
                    cons ->
                      NewRaceVarMap = race_var_map(cerl:cons_hd(NewVars1),
                        cerl:cons_hd(NewVars2), RaceVarMap, Op),
                      race_var_map(cerl:cons_tl(NewVars1),
                        cerl:cons_tl(NewVars2), NewRaceVarMap, Op);
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
				   RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
				   RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                alias ->
                  case cerl:type(NewVars2) of
                    alias ->
                      race_var_map(cerl:alias_var(NewVars1),
				   cerl:alias_var(NewVars2), RaceVarMap, Op);
                    _Other ->
                      race_var_map(cerl:alias_var(NewVars1),
                        NewVars2, RaceVarMap, Op)
                  end;
                values ->
                  case cerl:type(NewVars2) of
                    values ->
                      race_var_map(cerl:values_es(NewVars1),
				   cerl:values_es(NewVars2), RaceVarMap, Op);
                    _Other ->
                      race_var_map(cerl:values_es(NewVars1),
                        NewVars2, RaceVarMap, Op)
                  end;
                _Other -> RaceVarMap
              end;
            false -> RaceVarMap
          end
      end
  end.

race_var_map_clauses(Clauses, RaceVarMap) ->
  case Clauses of
    [] -> RaceVarMap;
    [#end_clause{arg = Arg, pats = Pats, guard = Guard}|T] ->
      {RaceVarMap1, _RemoveClause} =
        race_var_map_guard(Arg, Pats, Guard, RaceVarMap, bind),
      race_var_map_clauses(T, RaceVarMap1)
  end.

race_var_map_guard(Arg, Pats, Guard, RaceVarMap, Op) ->
  {NewRaceVarMap, RemoveClause} =
    case cerl:type(Guard) of
      call ->
        CallName = cerl:call_name(Guard),
        case cerl:is_literal(CallName) of
          true ->
            case cerl:concrete(CallName) of
              '=:=' ->
                [Arg1, Arg2] = cerl:call_args(Guard),
                {race_var_map(Arg1, Arg2, RaceVarMap, Op), false};
              '==' ->
                [Arg1, Arg2] = cerl:call_args(Guard),
                {race_var_map(Arg1, Arg2, RaceVarMap, Op), false};
              '=/=' ->
                case Op of
                  bind ->
                    [Arg1, Arg2] = cerl:call_args(Guard),
                    {RaceVarMap, are_bound_vars(Arg1, Arg2, RaceVarMap)};
                  unbind -> {RaceVarMap, false}
                end;
              _Other -> {RaceVarMap, false}
            end;
          false -> {RaceVarMap, false}
        end;
      _Other -> {RaceVarMap, false}
    end,
  {RaceVarMap1, RemoveClause1} =
    race_var_map_guard_helper1(Arg, Pats,
    race_var_map(Arg, Pats, NewRaceVarMap, Op), Op),
  {RaceVarMap1, RemoveClause orelse RemoveClause1}.

race_var_map_guard_helper1(Arg, Pats, RaceVarMap, Op) ->
  case Arg =:= ?no_arg orelse Arg =:= ?bypassed of
    true -> {RaceVarMap, false};
    false ->
      case cerl:type(Arg) of
        call ->
          case Pats of
            [NewPat] ->
              ModName = cerl:call_module(Arg),
              CallName = cerl:call_name(Arg),
              case cerl:is_literal(ModName) andalso
                cerl:is_literal(CallName) of
                true ->
                  case {cerl:concrete(ModName),
                        cerl:concrete(CallName)} of
                    {erlang, '=:='} ->
                      race_var_map_guard_helper2(Arg, NewPat, true,
                                                 RaceVarMap, Op);
                    {erlang, '=='} ->
                      race_var_map_guard_helper2(Arg, NewPat, true,
                                                 RaceVarMap, Op);
                    {erlang, '=/='} ->
                      race_var_map_guard_helper2(Arg, NewPat, false,
                                                 RaceVarMap, Op);
                    _Else -> {RaceVarMap, false}
                  end;
                false -> {RaceVarMap, false}
              end;
            _Other -> {RaceVarMap, false}
          end;
        _Other -> {RaceVarMap, false}
      end
  end.

race_var_map_guard_helper2(Arg, Pat0, Bool, RaceVarMap, Op) ->
  Pat = cerl:fold_literal(Pat0),
  case cerl:type(Pat) of
    literal ->
      [Arg1, Arg2] = cerl:call_args(Arg),
      case cerl:concrete(Pat) of
        Bool ->
          {race_var_map(Arg1, Arg2, RaceVarMap, Op), false};
        _Else ->
          case Op of
            bind ->
              {RaceVarMap, are_bound_vars(Arg1, Arg2, RaceVarMap)};
            unbind -> {RaceVarMap, false}
          end
      end;
    _Else -> {RaceVarMap, false}
  end.

unbind_dict_vars(Var, Var, RaceVarMap) ->
  RaceVarMap;
unbind_dict_vars(Var1, Var2, RaceVarMap) ->
  case dict:find(Var1, RaceVarMap) of
    error -> RaceVarMap;
    {ok, Labels} ->
      case Labels of
        [] -> dict:erase(Var1, RaceVarMap);
        _Else ->
          case lists:member(Var2, Labels) of
            true ->
              unbind_dict_vars(Var1, Var2,
                bind_dict_vars_list(Var1, Labels -- [Var2],
				    dict:erase(Var1, RaceVarMap)));
            false ->
              unbind_dict_vars_helper(Labels, Var1, Var2, RaceVarMap)
          end
      end
  end.

unbind_dict_vars_helper(Labels, Key, CompLabel, RaceVarMap) ->
  case dict:size(RaceVarMap) of
    0 -> RaceVarMap;
    _ ->
      case Labels of
        [] -> RaceVarMap;
        [Head|Tail] ->
          NewRaceVarMap =
            case are_bound_labels(Head, CompLabel, RaceVarMap) orelse
                 are_bound_labels(CompLabel, Head, RaceVarMap) of
              true ->
                bind_dict_vars_list(Key, Labels -- [Head],
				      dict:erase(Key, RaceVarMap));
              false -> RaceVarMap
            end,
          unbind_dict_vars_helper(Tail, Key, CompLabel, NewRaceVarMap)
      end
  end.

var_analysis(FunDefArgs, FunCallArgs, WarnVarArgs, RaceWarnTag) ->
  case RaceWarnTag of
    ?WARN_WHEREIS_REGISTER ->
      [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs,
      ArgNos = lists_key_members_lists(WVA1, FunDefArgs),
      [[lists_get(N, FunCallArgs) || N <- ArgNos], WVA2, WVA3, WVA4];
    ?WARN_WHEREIS_UNREGISTER ->
      [WVA1, WVA2] = WarnVarArgs,
      ArgNos = lists_key_members_lists(WVA1, FunDefArgs),
      [[lists_get(N, FunCallArgs) || N <- ArgNos], WVA2];
    ?WARN_ETS_LOOKUP_INSERT ->
      [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs,
      ArgNos1 = lists_key_members_lists(WVA1, FunDefArgs),
      ArgNos2 = lists_key_members_lists(WVA3, FunDefArgs),
      [[lists_get(N1, FunCallArgs) || N1 <- ArgNos1], WVA2,
       [lists_get(N2, FunCallArgs) || N2 <- ArgNos2], WVA4];
    ?WARN_MNESIA_DIRTY_READ_WRITE ->
      [WVA1, WVA2|T] = WarnVarArgs,
      ArgNos = lists_key_members_lists(WVA1, FunDefArgs),
      [[lists_get(N, FunCallArgs) || N <- ArgNos], WVA2|T]
  end.

var_type_analysis(FunDefArgs, FunCallTypes, WarnVarArgs, RaceWarnTag,
                  RaceVarMap, CleanState) ->
  FunVarArgs = format_args(FunDefArgs, FunCallTypes, CleanState, function_call),
  case RaceWarnTag of
    ?WARN_WHEREIS_REGISTER ->
      [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs,
      Vars = find_all_bound_vars(WVA1, RaceVarMap),
      case lists_key_member_lists(Vars, FunVarArgs) of
        0 -> [Vars, WVA2, WVA3, WVA4];
        N when is_integer(N) ->
          NewWVA2 = string:lexemes(lists:nth(N + 1, FunVarArgs), " |"),
          [Vars, NewWVA2, WVA3, WVA4]
      end;
    ?WARN_WHEREIS_UNREGISTER ->
      [WVA1, WVA2] = WarnVarArgs,
      Vars = find_all_bound_vars(WVA1, RaceVarMap),
      case lists_key_member_lists(Vars, FunVarArgs) of
        0 -> [Vars, WVA2];
        N when is_integer(N) ->
          NewWVA2 = string:lexemes(lists:nth(N + 1, FunVarArgs), " |"),
          [Vars, NewWVA2]
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      [WVA1, WVA2, WVA3, WVA4] = WarnVarArgs,
      Vars1 = find_all_bound_vars(WVA1, RaceVarMap),
      FirstVarArg =
        case lists_key_member_lists(Vars1, FunVarArgs) of
          0 -> [Vars1, WVA2];
          N1 when is_integer(N1) ->
            NewWVA2 = string:lexemes(lists:nth(N1 + 1, FunVarArgs), " |"),
            [Vars1, NewWVA2]
        end,
      Vars2 =
        lists:flatten(
          [find_all_bound_vars(A, RaceVarMap) || A <- ets_list_args(WVA3)]),
      case lists_key_member_lists(Vars2, FunVarArgs) of
        0 -> FirstVarArg ++ [Vars2, WVA4];
        N2 when is_integer(N2) ->
          NewWVA4 =
            ets_tuple_argtypes2(
            ets_tuple_argtypes1(lists:nth(N2 + 1, FunVarArgs), [], [], 0),
            []),
          FirstVarArg ++ [Vars2, NewWVA4]

      end;
    ?WARN_MNESIA_DIRTY_READ_WRITE ->
      [WVA1, WVA2|T] = WarnVarArgs,
      Arity =
        case T of
          [] -> 1;
          _Else -> 2
        end,
      Vars = find_all_bound_vars(WVA1, RaceVarMap),
      case lists_key_member_lists(Vars, FunVarArgs) of
        0 -> [Vars, WVA2|T];
        N when is_integer(N) ->
          NewWVA2 =
            case Arity of
              1 ->
                [mnesia_record_tab(R) || R <- string:lexemes(
                  lists:nth(2, FunVarArgs), " |")];
              2 ->
                string:lexemes(lists:nth(N + 1, FunVarArgs), " |")
            end,
          [Vars, NewWVA2|T]
      end
  end.

%%% ===========================================================================
%%%
%%%  Warning Format Utilities
%%%
%%% ===========================================================================

add_race_warning(Warn, #races{race_warnings = Warns} = Races) ->
  Races#races{race_warnings = [Warn|Warns]}.

get_race_warn(Fun, Args, ArgTypes, DepList, State) ->
  {M, F, _A} = Fun,
  case DepList of
    [] -> {State, no_race};
    _Other ->
      {State, {race_condition, [M, F, Args, ArgTypes, State, DepList]}}
  end.

-spec get_race_warnings(races(), dialyzer_dataflow:state()) ->
  {races(), dialyzer_dataflow:state()}.

get_race_warnings(#races{race_warnings = RaceWarnings}, State) ->
  get_race_warnings_helper(RaceWarnings, State).

get_race_warnings_helper(Warnings, State) ->
  case Warnings of
    [] ->
      {dialyzer_dataflow:state__get_races(State), State};
    [H|T] ->
      {RaceWarnTag, WarningInfo, {race_condition, [M, F, A, AT, S, DepList]}} = H,
      Reason =
        case RaceWarnTag of
          ?WARN_WHEREIS_REGISTER ->
            get_reason(lists:keysort(7, DepList),
                       "might fail due to a possible race condition "
                       "caused by its combination with ");
          ?WARN_WHEREIS_UNREGISTER ->
            get_reason(lists:keysort(7, DepList),
                       "might fail due to a possible race condition "
                       "caused by its combination with ");
          ?WARN_ETS_LOOKUP_INSERT ->
            get_reason(lists:keysort(7, DepList),
                       "might have an unintended effect due to " ++
                       "a possible race condition " ++
                       "caused by its combination with ");
          ?WARN_MNESIA_DIRTY_READ_WRITE ->
            get_reason(lists:keysort(7, DepList),
                       "might have an unintended effect due to " ++
                       "a possible race condition " ++
                       "caused by its combination with ")
        end,
      W =
        {?WARN_RACE_CONDITION, WarningInfo,
         {race_condition,
          [M, F, dialyzer_dataflow:format_args(A, AT, S), Reason]}},
      get_race_warnings_helper(T,
        dialyzer_dataflow:state__add_warning(W, State))
  end.

get_reason(DependencyList, Reason) ->
  case DependencyList of
    [] -> "";
    [#dep_call{call_name = Call, arg_types = ArgTypes, vars = Args,
               state = State, file_line = {File, Line}}|T] ->
      R =
        Reason ++
        case Call of
          whereis -> "the erlang:whereis";
          ets_lookup -> "the ets:lookup";
          mnesia_dirty_read -> "the mnesia:dirty_read"
        end ++
        dialyzer_dataflow:format_args(Args, ArgTypes, State) ++
        " call in " ++
        filename:basename(File) ++
        " on line " ++
        lists:flatten(io_lib:write(Line)),
      case T of
        [] -> R;
        _ -> get_reason(T, R ++ ", ")
      end
  end.

state__add_race_warning(State, RaceWarn, RaceWarnTag, WarningInfo) ->
  case RaceWarn of
    no_race -> State;
    _Else ->
      Races = dialyzer_dataflow:state__get_races(State),
      Warn = {RaceWarnTag, WarningInfo, RaceWarn},
      dialyzer_dataflow:state__put_races(add_race_warning(Warn, Races), State)
  end.

%%% ===========================================================================
%%%
%%%  Record Interfaces
%%%
%%% ===========================================================================

-spec beg_clause_new(var_to_map1(), var_to_map1(), cerl:cerl()) ->
   #beg_clause{}.

beg_clause_new(Arg, Pats, Guard) ->
  #beg_clause{arg = Arg, pats = Pats, guard = Guard}.

-spec cleanup(races()) -> races().

cleanup(#races{race_list = RaceList}) ->
  #races{race_list = RaceList}.

-spec end_case_new([#end_clause{}]) -> #end_case{}.

end_case_new(Clauses) ->
  #end_case{clauses = Clauses}.

-spec end_clause_new(var_to_map1(), var_to_map1(), cerl:cerl()) ->
   #end_clause{}.

end_clause_new(Arg, Pats, Guard) ->
  #end_clause{arg = Arg, pats = Pats, guard = Guard}.

-spec get_curr_fun(races()) -> dialyzer_callgraph:mfa_or_funlbl().

get_curr_fun(#races{curr_fun = CurrFun}) ->
  CurrFun.

-spec get_curr_fun_args(races()) -> core_args().

get_curr_fun_args(#races{curr_fun_args = CurrFunArgs}) ->
  CurrFunArgs.

-spec get_new_table(races()) -> table().

get_new_table(#races{new_table = Table}) ->
  Table.

-spec get_race_analysis(races()) -> boolean().

get_race_analysis(#races{race_analysis = RaceAnalysis}) ->
  RaceAnalysis.

-spec get_race_list(races()) -> code().

get_race_list(#races{race_list = RaceList}) ->
  RaceList.

-spec get_race_list_size(races()) -> non_neg_integer().

get_race_list_size(#races{race_list_size = RaceListSize}) ->
  RaceListSize.

-spec get_race_list_and_size(races()) -> {code(), non_neg_integer()}.

get_race_list_and_size(#races{race_list = RaceList,
			      race_list_size = RaceListSize}) ->
  {RaceList, RaceListSize}.

-spec let_tag_new(var_to_map1(), var_to_map1()) -> #let_tag{}.

let_tag_new(Var, Arg) ->
  #let_tag{var = Var, arg = Arg}.

-spec new() -> races().

new() -> #races{}.

-spec put_curr_fun(dialyzer_callgraph:mfa_or_funlbl(), label(), races()) ->
  races().

put_curr_fun(CurrFun, CurrFunLabel, Races) ->
  Races#races{curr_fun = CurrFun,
              curr_fun_label = CurrFunLabel,
              curr_fun_args = empty}.

-spec put_fun_args(core_args(), races()) -> races().

put_fun_args(Args, #races{curr_fun_args = CurrFunArgs} = Races) ->
  case CurrFunArgs of
    empty -> Races#races{curr_fun_args = Args};
    _Other -> Races
  end.

-spec put_race_analysis(boolean(), races()) ->
  races().

put_race_analysis(Analysis, Races) ->
  Races#races{race_analysis = Analysis}.

-spec put_race_list(code(), non_neg_integer(), races()) ->
  races().

put_race_list(RaceList, RaceListSize, Races) ->
  Races#races{race_list = RaceList, race_list_size = RaceListSize}.
