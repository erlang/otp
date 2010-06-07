%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File        : dialyzer_behaviours.erl
%%% Authors     : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Tools for analyzing proper behaviour usage.
%%%
%%% Created     : 28 Oct 2009 by Stavros Aronis <aronisstav@gmail.com>
%%%-------------------------------------------------------------------
%%% NOTE: This module is currently experimental -- do NOT rely on it!
%%%-------------------------------------------------------------------

-module(dialyzer_behaviours).

-export([check_callbacks/4, get_behaviours/2, get_behaviour_apis/1,
	 translate_behaviour_api_call/5, translatable_behaviours/1,
	 translate_callgraph/3]).

-export_type([behaviour/0, behaviour_api_dict/0]).

%%--------------------------------------------------------------------

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-type behaviour() :: atom().

-record(state, {plt        :: dialyzer_plt:plt(),
		codeserver :: dialyzer_codeserver:codeserver(),
		filename   :: file:filename(),
		behlines   :: [{behaviour(), non_neg_integer()}]}).

%%--------------------------------------------------------------------

-spec get_behaviours([module()], dialyzer_codeserver:codeserver()) ->
  {[behaviour()], [behaviour()]}.

get_behaviours(Modules, Codeserver) ->
  get_behaviours(Modules, Codeserver, [], []).

-spec check_callbacks(module(), [{cerl:cerl(), cerl:cerl()}],
		      dialyzer_plt:plt(),
		      dialyzer_codeserver:codeserver()) -> [dial_warning()].

check_callbacks(Module, Attrs, Plt, Codeserver) ->
  {Behaviours, BehLines} = get_behaviours(Attrs),
  case Behaviours of
    [] -> [];
    _ ->
      MFA = {Module,module_info,0},
      {_Var,Code} = dialyzer_codeserver:lookup_mfa_code(MFA, Codeserver),
      File = get_file(cerl:get_ann(Code)),
      State = #state{plt = Plt, codeserver = Codeserver, filename = File,
		     behlines = BehLines},
      Warnings = get_warnings(Module, Behaviours, State),
      [add_tag_file_line(Module, W, State) || W <- Warnings]
  end.

-spec translatable_behaviours(cerl:c_module()) -> behaviour_api_dict().

translatable_behaviours(Tree) ->
  Attrs = cerl:module_attrs(Tree),
  {Behaviours, _BehLines} = get_behaviours(Attrs),
  [{B, Calls} || B <- Behaviours, (Calls = behaviour_api_calls(B)) =/= []].

-spec get_behaviour_apis([behaviour()]) -> [mfa()].

get_behaviour_apis(Behaviours) ->
  get_behaviour_apis(Behaviours, []).

-spec translate_behaviour_api_call(dialyzer_races:mfa_or_funlbl(),
				   [erl_types:erl_type()],
				   [dialyzer_races:core_vars()],
				   module(),
				   behaviour_api_dict()) ->
				      {dialyzer_races:mfa_or_funlbl(),
				       [erl_types:erl_type()],
				       [dialyzer_races:core_vars()]}
					| 'plain_call'.

translate_behaviour_api_call(_Fun, _ArgTypes, _Args, _Module, []) ->
  plain_call;
translate_behaviour_api_call({Module, Fun, Arity}, ArgTypes, Args,
			     CallbackModule, BehApiInfo) ->
  case lists:keyfind(Module, 1, BehApiInfo) of
    false -> plain_call;
    {Module, Calls} ->
      case lists:keyfind({Fun, Arity}, 1, Calls) of
	false -> plain_call;
	{{Fun, Arity}, {CFun, CArity, COrder}} ->
	  {{CallbackModule, CFun, CArity},
	   [nth_or_0(N, ArgTypes, erl_types:t_any()) || N <-COrder],
	   [nth_or_0(N, Args, bypassed) || N <-COrder]}
      end
  end;
translate_behaviour_api_call(_Fun, _ArgTypes, _Args, _Module, _BehApiInfo) ->
  plain_call.

-spec translate_callgraph(behaviour_api_dict(), atom(),
			  dialyzer_callgraph:callgraph()) ->
			     dialyzer_callgraph:callgraph().

translate_callgraph([{Behaviour,_}|Behaviours], Module, Callgraph) ->
  UsedCalls = [Call || {_From, {M, _F, _A}} = Call <-
			 dialyzer_callgraph:get_behaviour_api_calls(Callgraph),
		       M =:= Behaviour],
  Calls = [{{Behaviour, API, Arity}, Callback} ||
	    {{API, Arity}, Callback} <- behaviour_api_calls(Behaviour)],
  DirectCalls = [{From, {Module, Fun, Arity}} ||
		  {From, To} <- UsedCalls,{API, {Fun, Arity, _Ord}} <- Calls,
		  To =:= API],
  NewCallgraph = dialyzer_callgraph:add_edges(DirectCalls, Callgraph),
  translate_callgraph(Behaviours, Module, NewCallgraph);
translate_callgraph([], _Module, Callgraph) ->
  Callgraph.

%%--------------------------------------------------------------------

get_behaviours(Attrs) ->
  BehaviourListsAndLine = [{cerl:concrete(L2), hd(cerl:get_ann(L2))} ||
		  {L1, L2} <- Attrs, cerl:is_literal(L1),
		  cerl:is_literal(L2), cerl:concrete(L1) =:= 'behaviour'],
  Behaviours = lists:append([Behs || {Behs,_} <- BehaviourListsAndLine]),
  BehLines = [{B,L} || {L1,L} <- BehaviourListsAndLine, B <- L1],
  {Behaviours, BehLines}.

get_warnings(Module, Behaviours, State) ->
  get_warnings(Module, Behaviours, State, []).

get_warnings(_, [], _, Acc) ->
  Acc;
get_warnings(Module, [Behaviour|Rest], State, Acc) ->
  Warnings = check_behaviour(Module, Behaviour, State),
  get_warnings(Module, Rest, State, Warnings ++ Acc).

check_behaviour(Module, Behaviour, State) ->
  try
    Callbacks = Behaviour:behaviour_info(callbacks),
    Fun = fun({_,_,_}) -> true;
	     (_)       -> false
	  end,
    case lists:any(Fun, Callbacks) of
      true ->  check_all_callbacks(Module, Behaviour, Callbacks, State);
      false -> []
    end
  catch
    _:_ -> []
  end.

check_all_callbacks(Module, Behaviour, Callbacks, State) ->
  check_all_callbacks(Module, Behaviour, Callbacks, State, []).

check_all_callbacks(_Module, _Behaviour, [], _State, Acc) ->
  Acc;
check_all_callbacks(Module, Behaviour, [{Fun, Arity, Spec}|Rest],
                    #state{codeserver = CServer} = State, Acc) ->
  Records = dialyzer_codeserver:get_records(CServer),
  ExpTypes = dialyzer_codeserver:get_exported_types(CServer),
  case parse_spec(Spec, ExpTypes, Records) of
    {ok, Fun, Type} ->
      RetType = erl_types:t_fun_range(Type),
      ArgTypes = erl_types:t_fun_args(Type),
      Warns = check_callback(Module, Behaviour, Fun, Arity, RetType,
			     ArgTypes, State#state.plt);
    Else ->
      Warns = [{invalid_spec, [Behaviour, Fun, Arity, reason_spec_error(Else)]}]
  end,
  check_all_callbacks(Module, Behaviour, Rest, State, Warns ++ Acc);
check_all_callbacks(Module, Behaviour, [{Fun, Arity}|Rest], State, Acc) ->
  Warns = {spec_missing, [Behaviour, Fun, Arity]},
  check_all_callbacks(Module, Behaviour, Rest, State, [Warns|Acc]).

parse_spec(String, ExpTypes, Records) ->
  case erl_scan:string(String) of
    {ok, Tokens, _} ->
      case erl_parse:parse(Tokens) of
	{ok, Form} ->
	  case Form of
	    {attribute, _, 'spec', {{Fun, _}, [TypeForm|_Constraint]}} ->
	      MaybeRemoteType = erl_types:t_from_form(TypeForm),
	      try
		Type = erl_types:t_solve_remote(MaybeRemoteType, ExpTypes,
                                                Records),
		{ok, Fun, Type}
	      catch
		throw:{error,Msg} -> {spec_remote_error, Msg}
	      end;
	    _Other -> not_a_spec
	  end;
	{error, {Line, _, Msg}} -> {spec_parser_error, Line, Msg}
      end;
    _Other ->
      lexer_error
  end.

reason_spec_error({spec_remote_error, Msg}) ->
  io_lib:format("Remote type solver error: ~s. Make sure the behaviour source is included in the analysis or the plt",[Msg]);
reason_spec_error(not_a_spec) ->
  "This is not a spec";
reason_spec_error({spec_parser_error, Line, Msg}) ->
  io_lib:format("~s line of the spec: ~s", [ordinal(Line),Msg]);
reason_spec_error(lexer_error) ->
  "Lexical error".

ordinal(1) -> "1st";
ordinal(2) -> "2nd";
ordinal(3) -> "3rd";
ordinal(N) when is_integer(N) -> io_lib:format("~wth",[N]).

check_callback(Module, Behaviour, Fun, Arity, XRetType, XArgTypes, Plt) ->
  LookupType = dialyzer_plt:lookup(Plt, {Module, Fun, Arity}),
  case LookupType of
    {value, {Type,Args}} ->
      Warn1 = case unifiable(Type, XRetType) of
		[] -> [];
		Offenders ->
		  [{callback_type_mismatch,
		   [Behaviour, Fun, Arity, erl_types:t_sup(Offenders)]}]
	      end,
      ZipArgs = lists:zip3(lists:seq(1, Arity), Args, XArgTypes),
      Warn2 = [{callback_arg_type_mismatch,
		[Behaviour, Fun, Arity, N,
		 erl_types:t_sup(Offenders)]} ||
		{Offenders, N} <- [check_callback_1(V) || V <- ZipArgs],
		Offenders =/= []],
      Warn1 ++ Warn2;
    _ -> [{callback_missing, [Behaviour, Fun, Arity]}]
  end.

check_callback_1({N, T1, T2}) ->
  {unifiable(T1, T2), N}.

unifiable(Type1, Type2) ->
  List1 = erl_types:t_elements(Type1),
  List2 = erl_types:t_elements(Type2),
  [T || T <- List1,
	lists:all(fun(T1) ->
		      erl_types:t_is_none(erl_types:t_inf(T, T1, opaque))
		  end, List2)].

add_tag_file_line(_Module, {Tag, [B|_R]} = Warn, State)
  when Tag =:= spec_missing;
       Tag =:= invalid_spec;
       Tag =:= callback_missing ->
  {B, Line} = lists:keyfind(B, 1, State#state.behlines),
  {?WARN_BEHAVIOUR, {State#state.filename, Line}, Warn};
add_tag_file_line(Module, {_Tag, [_B, Fun, Arity|_R]} = Warn, State) ->
  {_A, FunCode} =
    dialyzer_codeserver:lookup_mfa_code({Module, Fun, Arity},
					State#state.codeserver),
  Anns = cerl:get_ann(FunCode),
  FileLine = {get_file(Anns), get_line(Anns)},
  {?WARN_BEHAVIOUR, FileLine, Warn}.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([{file, File}|_]) -> File;
get_file([_|Tail]) -> get_file(Tail).

%%-----------------------------------------------------------------------------

get_behaviours([], _Codeserver, KnownAcc, UnknownAcc) ->
  {KnownAcc, UnknownAcc};
get_behaviours([M|Rest], Codeserver, KnownAcc, UnknownAcc) ->
  Tree = dialyzer_codeserver:lookup_mod_code(M, Codeserver),
  Attrs = cerl:module_attrs(Tree),
  {Behaviours, _BehLines} = get_behaviours(Attrs),
  {Known, Unknown} = call_behaviours(Behaviours),
  get_behaviours(Rest, Codeserver, Known ++ KnownAcc, Unknown ++ UnknownAcc).

call_behaviours(Behaviours) ->
  call_behaviours(Behaviours, [], []).
call_behaviours([], KnownAcc, UnknownAcc) ->
  {lists:reverse(KnownAcc), lists:reverse(UnknownAcc)};
call_behaviours([Behaviour|Rest], KnownAcc, UnknownAcc) ->
  try
    Callbacks = Behaviour:behaviour_info(callbacks),
    Fun = fun({_,_,_}) -> true;
	     (_)       -> false
	  end,
    case lists:any(Fun, Callbacks) of
      false -> call_behaviours(Rest, KnownAcc, [Behaviour | UnknownAcc]);
      true  -> call_behaviours(Rest, [Behaviour | KnownAcc], UnknownAcc)
    end
  catch
    _:_ -> call_behaviours(Rest, KnownAcc, [Behaviour | UnknownAcc])
  end.

%------------------------------------------------------------------------------

get_behaviour_apis([], Acc) ->
  Acc;
get_behaviour_apis([Behaviour | Rest], Acc) ->
  MFAs = [{Behaviour, Fun, Arity} ||
	   {{Fun, Arity}, _} <- behaviour_api_calls(Behaviour)],
  get_behaviour_apis(Rest, MFAs ++ Acc).

%------------------------------------------------------------------------------

nth_or_0(0, _List, Zero) ->
  Zero;
nth_or_0(N, List, _Zero) ->
  lists:nth(N, List).

%------------------------------------------------------------------------------

-type behaviour_api_dict()::[{behaviour(), behaviour_api_info()}].
-type behaviour_api_info()::[{original_fun(), replacement_fun()}].
-type original_fun()::{atom(), arity()}.
-type replacement_fun()::{atom(), arity(), arg_list()}.
-type arg_list()::[byte()].

-spec behaviour_api_calls(behaviour()) -> behaviour_api_info().

behaviour_api_calls(gen_server) ->
  [{{start_link, 3}, {init, 1, [2]}},
   {{start_link, 4}, {init, 1, [3]}},
   {{start, 3}, {init, 1, [2]}},
   {{start, 4}, {init, 1, [3]}},
   {{call, 2}, {handle_call, 3, [2, 0, 0]}},
   {{call, 3}, {handle_call, 3, [2, 0, 0]}},
   {{multi_call, 2}, {handle_call, 3, [2, 0, 0]}},
   {{multi_call, 3}, {handle_call, 3, [3, 0, 0]}},
   {{multi_call, 4}, {handle_call, 3, [3, 0, 0]}},
   {{cast, 2}, {handle_cast, 2, [2, 0]}},
   {{abcast, 2}, {handle_cast, 2, [2, 0]}},
   {{abcast, 3}, {handle_cast, 2, [3, 0]}}];
behaviour_api_calls(_Other) ->
  [].
