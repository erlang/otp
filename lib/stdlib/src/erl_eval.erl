%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(erl_eval).

%% Guard is_map/1 is not yet supported in HiPE.
-compile(no_native).

%% An evaluator for Erlang abstract syntax.

-export([exprs/2,exprs/3,exprs/4,expr/2,expr/3,expr/4,expr/5,
         expr_list/2,expr_list/3,expr_list/4]).
-export([new_bindings/0,bindings/1,binding/2,add_binding/3,del_binding/2]).

-export([is_constant_expr/1, partial_eval/1]).

%% Is used by standalone Erlang (escript).
%% Also used by shell.erl.
-export([match_clause/4]).

-export([check_command/2, fun_data/1]).

-import(lists, [reverse/1,foldl/3,member/2]).

-export_type([binding_struct/0]).

-type(expression() :: erl_parse:abstract_expr()).
-type(expressions() :: [erl_parse:abstract_expr()]).
-type(expression_list() :: [expression()]).
-type(clauses() :: [erl_parse:abstract_clause()]).
-type(name() :: term()).
-type(value() :: term()).
-type(bindings() :: [{name(), value()}]).
-type(binding_struct() :: orddict:orddict()).

-type(lfun_value_handler() :: fun((Name :: atom(),
                                   Arguments :: [term()]) ->
                                         Value :: value())).
-type(lfun_eval_handler() :: fun((Name :: atom(),
                                  Arguments :: expression_list(),
                                  Bindings :: binding_struct()) ->
                                        {value,
                                         Value :: value(),
                                         NewBindings :: binding_struct()})).
-type(local_function_handler() :: {value, lfun_value_handler()}
                                | {eval, lfun_eval_handler()}
                                | none).

-type(func_spec() :: {Module :: module(), Function :: atom()} | function()).
-type(nlfun_handler() :: fun((FuncSpec :: func_spec(),
                              Arguments :: [term()]) ->
                                    term())).
-type(non_local_function_handler() :: {value, nlfun_handler()}
                                    | none).

%% exprs(ExpressionSeq, Bindings)
%% exprs(ExpressionSeq, Bindings, LocalFuncHandler)
%% exprs(ExpressionSeq, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns:
%%	{value,Value,NewBindings}
%%    or {'EXIT', Reason}
%% Only exprs/2 checks the command by calling erl_lint. The reason is
%% that if there is a function handler present, then it is possible
%% that there are valid constructs in Expression to be taken care of
%% by a function handler but considered errors by erl_lint.

-spec(exprs(Expressions, Bindings) -> {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs(Exprs, Bs) ->
    case check_command(Exprs, Bs) of
        ok -> 
            exprs(Exprs, Bs, none, none, none);
        {error,{_Line,_Mod,Error}} ->
	    erlang:raise(error, Error, [{?MODULE,exprs,2}])
    end.

-spec(exprs(Expressions, Bindings, LocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs(Exprs, Bs, Lf) ->
    exprs(Exprs, Bs, Lf, none, none).

-spec(exprs(Expressions, Bindings, LocalFunctionHandler,
            NonLocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs(Exprs, Bs, Lf, Ef) ->
    exprs(Exprs, Bs, Lf, Ef, none).

exprs([E], Bs0, Lf, Ef, RBs) ->
    expr(E, Bs0, Lf, Ef, RBs);
exprs([E|Es], Bs0, Lf, Ef, RBs) ->
    RBs1 = none,
    {value,_V,Bs} = expr(E, Bs0, Lf, Ef, RBs1),
    exprs(Es, Bs, Lf, Ef, RBs).

%% expr(Expression, Bindings)
%% expr(Expression, Bindings, LocalFuncHandler)
%% expr(Expression, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns:
%%	 {value,Value,NewBindings}
%%    or {'EXIT', Reason}
%%
%% Only expr/2 checks the command by calling erl_lint. See exprs/2.

-spec(expr(Expression, Bindings) -> {value, Value, NewBindings} when
      Expression :: expression(),
      Bindings :: binding_struct(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr(E, Bs) ->
    case check_command([E], Bs) of
        ok -> 
            expr(E, Bs, none, none, none);
        {error,{_Line,_Mod,Error}} ->
	    erlang:raise(error, Error, [{?MODULE,expr,2}])
    end.

-spec(expr(Expression, Bindings, LocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expression :: expression(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr(E, Bs, Lf) ->
    expr(E, Bs, Lf, none, none).

-spec(expr(Expression, Bindings, LocalFunctionHandler,
           NonLocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expression :: expression(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr(E, Bs, Lf, Ef) ->
    expr(E, Bs, Lf, Ef, none).

%% Check a command (a list of expressions) by calling erl_lint.

check_command(Es, Bs) ->
    Opts = [bitlevel_binaries,binary_comprehension],
    case erl_lint:exprs_opt(Es, bindings(Bs), Opts) of
        {ok,_Ws} ->
            ok;
        {error,[{_File,[Error|_]}],_Ws} ->
            {error,Error}
    end.

%% Check whether a term F is a function created by this module.
%% Returns 'false' if not, otherwise {fun_data,Imports,Clauses}.

fun_data(F) when is_function(F) ->
    case erlang:fun_info(F, module) of
        {module,erl_eval} ->
            case erlang:fun_info(F, env) of
                {env,[{FBs,_FLf,_FEf,FCs}]} ->
                    {fun_data,FBs,FCs};
                {env,[{FBs,_FLf,_FEf,FCs,FName}]} ->
                    {named_fun_data,FBs,FName,FCs}
            end;
        _ ->
            false
    end;
fun_data(_T) ->
    false.

-spec(expr(Expression, Bindings, LocalFunctionHandler,
           NonLocalFunctionHandler, ReturnFormat) ->
             {value, Value, NewBindings} | Value when
      Expression :: expression(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      ReturnFormat :: none | value,
      Value :: value(),
      NewBindings :: binding_struct()).
expr({var,_,V}, Bs, _Lf, _Ef, RBs) ->
    case binding(V, Bs) of
	{value,Val} ->
            ret_expr(Val, Bs, RBs);
	unbound -> % Should not happen.
	    erlang:raise(error, {unbound,V}, stacktrace())
    end;
expr({char,_,C}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(C, Bs, RBs);
expr({integer,_,I}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(I, Bs, RBs);
expr({float,_,F}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(F, Bs, RBs);
expr({atom,_,A}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(A, Bs, RBs);
expr({string,_,S}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(S, Bs, RBs);
expr({nil, _}, Bs, _Lf, _Ef, RBs) ->
    ret_expr([], Bs, RBs);
expr({cons,_,H0,T0}, Bs0, Lf, Ef, RBs) ->
    {value,H,Bs1} = expr(H0, Bs0, Lf, Ef, none),
    {value,T,Bs2} = expr(T0, Bs0, Lf, Ef, none),
    ret_expr([H|T], merge_bindings(Bs1, Bs2), RBs);
expr({lc,_,E,Qs}, Bs, Lf, Ef, RBs) ->
    eval_lc(E, Qs, Bs, Lf, Ef, RBs);
expr({bc,_,E,Qs}, Bs, Lf, Ef, RBs) ->
    eval_bc(E, Qs, Bs, Lf, Ef, RBs);
expr({tuple,_,Es}, Bs0, Lf, Ef, RBs) ->
    {Vs,Bs} = expr_list(Es, Bs0, Lf, Ef),
    ret_expr(list_to_tuple(Vs), Bs, RBs);
expr({record_field,_,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, {undef_record,Name}, stacktrace());
expr({record_index,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, {undef_record,Name}, stacktrace());
expr({record,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, {undef_record,Name}, stacktrace());
expr({record,_,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, {undef_record,Name}, stacktrace());

%% map
expr({map,_,Binding,Es}, Bs0, Lf, Ef, RBs) ->
    {value, Map0, Bs1} = expr(Binding, Bs0, Lf, Ef, none),
    {Vs,Bs2} = eval_map_fields(Es, Bs0, Lf, Ef),
    _ = maps:put(k, v, Map0),			%Validate map.
    Map1 = lists:foldl(fun ({map_assoc,K,V}, Mi) ->
			       maps:put(K, V, Mi);
			   ({map_exact,K,V}, Mi) ->
			       maps:update(K, V, Mi)
		       end, Map0, Vs),
    ret_expr(Map1, merge_bindings(Bs2, Bs1), RBs);
expr({map,_,Es}, Bs0, Lf, Ef, RBs) ->
    {Vs,Bs} = eval_map_fields(Es, Bs0, Lf, Ef),
    ret_expr(lists:foldl(fun
		({map_assoc,K,V}, Mi) -> maps:put(K,V,Mi)
	    end, maps:new(), Vs), Bs, RBs);

expr({block,_,Es}, Bs, Lf, Ef, RBs) ->
    exprs(Es, Bs, Lf, Ef, RBs);
expr({'if',_,Cs}, Bs, Lf, Ef, RBs) ->
    if_clauses(Cs, Bs, Lf, Ef, RBs);
expr({'case',_,E,Cs}, Bs0, Lf, Ef, RBs) ->
    {value,Val,Bs} = expr(E, Bs0, Lf, Ef, none),
    case_clauses(Val, Cs, Bs, Lf, Ef, RBs);
expr({'try',_,B,Cases,Catches,AB}, Bs, Lf, Ef, RBs) ->
    try_clauses(B, Cases, Catches, AB, Bs, Lf, Ef, RBs);
expr({'receive',_,Cs}, Bs, Lf, Ef, RBs) ->
    receive_clauses(Cs, Bs, Lf, Ef, RBs);
expr({'receive',_, Cs, E, TB}, Bs0, Lf, Ef, RBs) ->
    {value,T,Bs} = expr(E, Bs0, Lf, Ef, none),
    receive_clauses(T, Cs, {TB,Bs}, Bs0, Lf, Ef, RBs);
expr({'fun',_Line,{function,Mod0,Name0,Arity0}}, Bs0, Lf, Ef, RBs) ->
    {[Mod,Name,Arity],Bs} = expr_list([Mod0,Name0,Arity0], Bs0, Lf, Ef),
    F = erlang:make_fun(Mod, Name, Arity),
    ret_expr(F, Bs, RBs);    
expr({'fun',_Line,{function,Name,Arity}}, _Bs0, _Lf, _Ef, _RBs) -> % R8
    %% Don't know what to do...
    erlang:raise(error, undef, [{erl_eval,Name,Arity}|stacktrace()]);
expr({'fun',Line,{clauses,Cs}} = Ex, Bs, Lf, Ef, RBs) ->
    %% Save only used variables in the function environment.
    %% {value,L,V} are hidden while lint finds used variables.
    {Ex1, _} = hide_calls(Ex, 0),
    {ok,Used} = erl_lint:used_vars([Ex1], Bs),
    En = orddict:filter(fun(K,_V) -> member(K,Used) end, Bs),
    Info = {En,Lf,Ef,Cs},
    %% This is a really ugly hack!
    F = 
    case length(element(3,hd(Cs))) of
        0 -> fun () -> eval_fun([], Info) end;
        1 -> fun (A) -> eval_fun([A], Info) end;
        2 -> fun (A,B) -> eval_fun([A,B], Info) end;
        3 -> fun (A,B,C) -> eval_fun([A,B,C], Info) end;
        4 -> fun (A,B,C,D) -> eval_fun([A,B,C,D], Info) end;
        5 -> fun (A,B,C,D,E) -> eval_fun([A,B,C,D,E], Info) end;
        6 -> fun (A,B,C,D,E,F) -> eval_fun([A,B,C,D,E,F], Info) end;
        7 -> fun (A,B,C,D,E,F,G) -> eval_fun([A,B,C,D,E,F,G], Info) end;
        8 -> fun (A,B,C,D,E,F,G,H) -> eval_fun([A,B,C,D,E,F,G,H], Info) end;
        9 -> fun (A,B,C,D,E,F,G,H,I) -> eval_fun([A,B,C,D,E,F,G,H,I], Info) end;
        10 -> fun (A,B,C,D,E,F,G,H,I,J) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J], Info) end;
        11 -> fun (A,B,C,D,E,F,G,H,I,J,K) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K], Info) end;
        12 -> fun (A,B,C,D,E,F,G,H,I,J,K,L) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L], Info) end;
        13 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M], Info) end;
        14 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N], Info) end;
        15 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Info) end;
        16 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Info) end;
        17 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], Info) end;
        18 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], Info) end;
        19 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], Info) end;
        20 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
           eval_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], Info) end;
	_Other ->
	    erlang:raise(error, {'argument_limit',{'fun',Line,Cs}},
			 stacktrace())
    end,
    ret_expr(F, Bs, RBs);
expr({named_fun,Line,Name,Cs} = Ex, Bs, Lf, Ef, RBs) ->
    %% Save only used variables in the function environment.
    %% {value,L,V} are hidden while lint finds used variables.
    {Ex1, _} = hide_calls(Ex, 0),
    {ok,Used} = erl_lint:used_vars([Ex1], Bs),
    En = orddict:filter(fun(K,_V) -> member(K,Used) end, Bs),
    Info = {En,Lf,Ef,Cs,Name},
    %% This is a really ugly hack!
    F =
    case length(element(3,hd(Cs))) of
        0 -> fun RF() -> eval_named_fun([], RF, Info) end;
        1 -> fun RF(A) -> eval_named_fun([A], RF, Info) end;
        2 -> fun RF(A,B) -> eval_named_fun([A,B], RF, Info) end;
        3 -> fun RF(A,B,C) -> eval_named_fun([A,B,C], RF, Info) end;
        4 -> fun RF(A,B,C,D) -> eval_named_fun([A,B,C,D], RF, Info) end;
        5 -> fun RF(A,B,C,D,E) -> eval_named_fun([A,B,C,D,E], RF, Info) end;
        6 -> fun RF(A,B,C,D,E,F) -> eval_named_fun([A,B,C,D,E,F], RF, Info) end;
        7 -> fun RF(A,B,C,D,E,F,G) ->
           eval_named_fun([A,B,C,D,E,F,G], RF, Info) end;
        8 -> fun RF(A,B,C,D,E,F,G,H) ->
           eval_named_fun([A,B,C,D,E,F,G,H], RF, Info) end;
        9 -> fun RF(A,B,C,D,E,F,G,H,I) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I], RF, Info) end;
        10 -> fun RF(A,B,C,D,E,F,G,H,I,J) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J], RF, Info) end;
        11 -> fun RF(A,B,C,D,E,F,G,H,I,J,K) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K], RF, Info) end;
        12 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L], RF, Info) end;
        13 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M], RF, Info) end;
        14 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N], RF, Info) end;
        15 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], RF, Info) end;
        16 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], RF, Info) end;
        17 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], RF, Info) end;
        18 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], RF, Info) end;
        19 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                          RF, Info) end;
        20 -> fun RF(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) ->
           eval_named_fun([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],
                          RF, Info) end;
        _Other ->
            erlang:raise(error, {'argument_limit',{named_fun,Line,Name,Cs}},
                         stacktrace())
    end,
    ret_expr(F, Bs, RBs);
expr({call,_,{remote,_,{atom,_,qlc},{atom,_,q}},[{lc,_,_E,_Qs}=LC | As0]}, 
     Bs0, Lf, Ef, RBs) when length(As0) =< 1 ->
    %% No expansion or evaluation of module name or function name.
    MaxLine = find_maxline(LC),
    {LC1, D} = hide_calls(LC, MaxLine),
    case qlc:transform_from_evaluator(LC1, Bs0) of
        {ok,{call,L,Remote,[QLC]}} ->
            QLC1 = unhide_calls(QLC, MaxLine, D),
            expr({call,L,Remote,[QLC1 | As0]}, Bs0, Lf, Ef, RBs);
        {not_ok,Error} ->
            ret_expr(Error, Bs0, RBs)
    end;
expr({call,L1,{remote,L2,{record_field,_,{atom,_,''},{atom,_,qlc}=Mod},
               {atom,_,q}=Func},
      [{lc,_,_E,_Qs} | As0]=As}, 
     Bs, Lf, Ef, RBs) when length(As0) =< 1 ->
    expr({call,L1,{remote,L2,Mod,Func},As}, Bs, Lf, Ef, RBs);
expr({call,_,{remote,_,Mod,Func},As0}, Bs0, Lf, Ef, RBs) ->
    {value,M,Bs1} = expr(Mod, Bs0, Lf, Ef, none),
    {value,F,Bs2} = expr(Func, Bs0, Lf, Ef, none),
    {As,Bs3} = expr_list(As0, merge_bindings(Bs1, Bs2), Lf, Ef),
    %% M could be a parameterized module (not an atom).
    case is_atom(M) andalso erl_internal:bif(M, F, length(As)) of
        true ->
            bif(F, As, Bs3, Ef, RBs);
        false ->
            do_apply(M, F, As, Bs3, Ef, RBs)
    end;
expr({call,_,{atom,_,Func},As0}, Bs0, Lf, Ef, RBs) ->
    case erl_internal:bif(Func, length(As0)) of
        true ->
            {As,Bs} = expr_list(As0, Bs0, Lf, Ef),
            bif(Func, As, Bs, Ef, RBs);
        false ->
            local_func(Func, As0, Bs0, Lf, Ef, RBs)
    end;
expr({call,_,Func0,As0}, Bs0, Lf, Ef, RBs) -> % function or {Mod,Fun}
    {value,Func,Bs1} = expr(Func0, Bs0, Lf, Ef, none),
    {As,Bs2} = expr_list(As0, Bs1, Lf, Ef),
    case Func of
	{M,F} when is_atom(M), is_atom(F) ->
	    erlang:raise(error, {badfun,Func}, stacktrace());
	_ ->
	    do_apply(Func, As, Bs2, Ef, RBs)
    end;
expr({'catch',_,Expr}, Bs0, Lf, Ef, RBs) ->
    Ref = make_ref(),
    case catch {Ref,expr(Expr, Bs0, Lf, Ef, none)} of
	{Ref,{value,V,Bs}} ->	  % Nothing was thrown (guaranteed).
            ret_expr(V, Bs, RBs);
	Other ->
            ret_expr(Other, Bs0, RBs)
    end;
expr({match,_,Lhs,Rhs0}, Bs0, Lf, Ef, RBs) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Lf, Ef, none),
    case match(Lhs, Rhs, Bs1) of
	{match,Bs} ->
            ret_expr(Rhs, Bs, RBs);
	nomatch ->
	    erlang:raise(error, {badmatch,Rhs}, stacktrace())
    end;
expr({op,_,Op,A0}, Bs0, Lf, Ef, RBs) ->
    {value,A,Bs} = expr(A0, Bs0, Lf, Ef, none),
    eval_op(Op, A, Bs, Ef, RBs);
expr({op,_,'andalso',L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    V = case L of
	    true ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none),
		R;
	    false -> false;
	    _ -> erlang:raise(error, {badarg,L}, stacktrace())
	end,
    ret_expr(V, Bs1, RBs);
expr({op,_,'orelse',L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    V = case L of
	    true -> true;
	    false ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none),
		R;
	    _ -> erlang:raise(error, {badarg,L}, stacktrace())
	end,
    ret_expr(V, Bs1, RBs);
expr({op,_,Op,L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    {value,R,Bs2} = expr(R0, Bs0, Lf, Ef, none),
    eval_op(Op, L, R, merge_bindings(Bs1, Bs2), Ef, RBs);
expr({bin,_,Fs}, Bs0, Lf, Ef, RBs) ->
    EvalFun = fun(E, B) -> expr(E, B, Lf, Ef, none) end,
    {value,V,Bs} = eval_bits:expr_grp(Fs, Bs0, EvalFun),
    ret_expr(V, Bs, RBs);
expr({remote,_,_,_}, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, {badexpr,':'}, stacktrace());
expr({value,_,Val}, Bs, _Lf, _Ef, RBs) ->    % Special case straight values.
    ret_expr(Val, Bs, RBs).

find_maxline(LC) ->
    put('$erl_eval_max_line', 0),
    F = fun(A) ->
                L = erl_anno:line(A),
                case is_integer(L) and (L > get('$erl_eval_max_line')) of
                    true -> put('$erl_eval_max_line', L);
                    false -> ok
                end end,
    _ = erl_parse:map_anno(F, LC),
    erase('$erl_eval_max_line').

hide_calls(LC, MaxLine) ->
    LineId0 = MaxLine + 1,
    {NLC, _, D} = hide(LC, LineId0, dict:new()),
    {NLC, D}.

%% v/1 and local calls are hidden.
hide({value,L,V}, Id, D) ->
    A = erl_anno:new(Id),
    {{atom,A,ok}, Id+1, dict:store(Id, {value,L,V}, D)};
hide({call,L,{atom,_,N}=Atom,Args}, Id0, D0) ->
    {NArgs, Id, D} = hide(Args, Id0, D0),
    C = case erl_internal:bif(N, length(Args)) of
            true ->
                {call,L,Atom,NArgs};
            false -> 
                A = erl_anno:new(Id),
                {call,A,{remote,L,{atom,L,m},{atom,L,f}},NArgs}
        end,
    {C, Id+1, dict:store(Id, {call,Atom}, D)};
hide(T0, Id0, D0) when is_tuple(T0) -> 
    {L, Id, D} = hide(tuple_to_list(T0), Id0, D0),
    {list_to_tuple(L), Id, D};
hide([E0 | Es0], Id0, D0) -> 
    {E, Id1, D1} = hide(E0, Id0, D0),
    {Es, Id, D} = hide(Es0, Id1, D1),
    {[E | Es], Id, D};
hide(E, Id, D) -> 
    {E, Id, D}.

unhide_calls({atom,A,ok}=E, MaxLine, D) ->
    L = erl_anno:line(A),
    if
        L > MaxLine ->
            dict:fetch(L, D);
        true ->
            E
    end;
unhide_calls({call,A,{remote,L,{atom,L,m},{atom,L,f}}=F,Args}, MaxLine, D) ->
    Line = erl_anno:line(A),
    if
        Line > MaxLine ->
            {call,Atom} = dict:fetch(Line, D),
            {call,L,Atom,unhide_calls(Args, MaxLine, D)};
        true ->
            {call,A,F,unhide_calls(Args, MaxLine, D)}
    end;
unhide_calls(T, MaxLine, D) when is_tuple(T) -> 
    list_to_tuple(unhide_calls(tuple_to_list(T), MaxLine, D));
unhide_calls([E | Es], MaxLine, D) -> 
    [unhide_calls(E, MaxLine, D) | unhide_calls(Es, MaxLine, D)];
unhide_calls(E, _MaxLine, _D) -> 
    E.

%% local_func(Function, Arguments, Bindings, LocalFuncHandler,
%%            ExternalFuncHandler, RBs) ->
%%	{value,Value,Bindings} | Value when
%%	LocalFuncHandler = {value,F} | {value,F,Eas} |
%%                         {eval,F}  | {eval,F,Eas}  | none.

local_func(Func, As0, Bs0, {value,F}, Ef, value) ->
    {As1,_Bs1} = expr_list(As0, Bs0, {value,F}, Ef),
    %% Make tail recursive calls when possible.
    F(Func, As1);
local_func(Func, As0, Bs0, {value,F}, Ef, RBs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F}, Ef),
    ret_expr(F(Func, As1), Bs1, RBs);
local_func(Func, As0, Bs0, {value,F,Eas}, Ef, RBs) ->
    Fun = fun(Name, Args) -> apply(F, [Name,Args|Eas]) end,
    local_func(Func, As0, Bs0, {value, Fun}, Ef, RBs);
local_func(Func, As, Bs, {eval,F}, _Ef, RBs) ->
    local_func2(F(Func, As, Bs), RBs);
local_func(Func, As, Bs, {eval,F,Eas}, _Ef, RBs) ->
    local_func2(apply(F, [Func,As,Bs|Eas]), RBs);
%% These two clauses are for backwards compatibility.
local_func(Func, As0, Bs0, {M,F}, Ef, RBs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {M,F}, Ef),
    ret_expr(M:F(Func,As1), Bs1, RBs);
local_func(Func, As, _Bs, {M,F,Eas}, _Ef, RBs) ->
    local_func2(apply(M, F, [Func,As|Eas]), RBs);
%% Default unknown function handler to undefined function.
local_func(Func, As0, _Bs0, none, _Ef, _RBs) ->
    erlang:raise(error, undef, [{erl_eval,Func,length(As0)}|stacktrace()]).

local_func2({value,V,Bs}, RBs) ->
    ret_expr(V, Bs, RBs);
local_func2({eval,F,As,Bs}, RBs) -> % This reply is not documented.
    %% The shell found F. erl_eval tries to do a tail recursive call,
    %% something the shell cannot do. Do not use Ef here.
    do_apply(F, As, Bs, none, RBs).

%% bif(Name, Arguments, RBs)
%%  Evaluate the Erlang auto-imported function Name. erlang:apply/2,3
%%  are "hidden" from the external function handler.

bif(apply, [erlang,apply,As], Bs, Ef, RBs) ->
    bif(apply, As, Bs, Ef, RBs);
bif(apply, [M,F,As], Bs, Ef, RBs) ->
    do_apply(M, F, As, Bs, Ef, RBs);
bif(apply, [F,As], Bs, Ef, RBs) ->
    do_apply(F, As, Bs, Ef, RBs);
bif(Name, As, Bs, Ef, RBs) ->
    do_apply(erlang, Name, As, Bs, Ef, RBs).

%% do_apply(MF, Arguments, Bindings, ExternalFuncHandler, RBs) ->
%%	{value,Value,Bindings} | Value when
%%	ExternalFuncHandler = {value,F} | none.
%% MF is a tuple {Module,Function} or a fun.

do_apply({M,F}=Func, As, Bs0, Ef, RBs)
  when tuple_size(M) >= 1, is_atom(element(1, M)), is_atom(F) ->
    case Ef of
        none when RBs =:= value ->
            %% Make tail recursive calls when possible.
            apply(M, F, As);
        none ->
            ret_expr(apply(M, F, As), Bs0, RBs);
        {value,Fun} when RBs =:= value ->
            Fun(Func, As);
        {value,Fun} ->
            ret_expr(Fun(Func, As), Bs0, RBs)
    end;
do_apply(Func, As, Bs0, Ef, RBs) ->
    Env = if
              is_function(Func) -> 
                  case {erlang:fun_info(Func, module),
                        erlang:fun_info(Func, env)} of
                      {{module,?MODULE},{env,Env1}} when Env1 =/= [] ->
                          {env,Env1};
                      _ ->
                          no_env
                  end;
              true -> 
                  no_env
          end,
    case {Env,Ef} of
        {{env,[{FBs,FLf,FEf,FCs}]},_} ->
            %% If we are evaluting within another function body 
            %% (RBs =/= none), we return RBs when this function body
            %% has been evalutated, otherwise we return Bs0, the
            %% bindings when evalution of this function body started.
            NRBs = if
                       RBs =:= none -> Bs0;
                       true -> RBs
                   end,
            case {erlang:fun_info(Func, arity), length(As)} of
                {{arity, Arity}, Arity} ->
                    eval_fun(FCs, As, FBs, FLf, FEf, NRBs);
                _ ->
                    erlang:raise(error, {badarity,{Func,As}},stacktrace())
            end;
        {{env,[{FBs,FLf,FEf,FCs,FName}]},_} ->
            NRBs = if
                       RBs =:= none -> Bs0;
                       true -> RBs
                   end,
            case {erlang:fun_info(Func, arity), length(As)} of
                {{arity, Arity}, Arity} ->
                    eval_named_fun(FCs, As, FBs, FLf, FEf, FName, Func, NRBs);
                _ ->
                    erlang:raise(error, {badarity,{Func,As}},stacktrace())
            end;
        {no_env,none} when RBs =:= value ->
            %% Make tail recursive calls when possible.
            apply(Func, As);
        {no_env,none} ->
            ret_expr(apply(Func, As), Bs0, RBs);
        {no_env,{value,F}} when RBs =:= value ->
            F(Func,As);
        {no_env,{value,F}} ->
            ret_expr(F(Func, As), Bs0, RBs)
    end.

do_apply(Mod, Func, As, Bs0, Ef, RBs) ->
    case Ef of
        none when RBs =:= value ->
            %% Make tail recursive calls when possible.
            apply(Mod, Func, As);
        none ->
            ret_expr(apply(Mod, Func, As), Bs0, RBs);
        {value,F} when RBs =:= value ->
            F({Mod,Func}, As);
        {value,F} ->
            ret_expr(F({Mod,Func}, As), Bs0, RBs)
    end.

%% eval_lc(Expr, [Qualifier], Bindings, LocalFunctionHandler, 
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value

eval_lc(E, Qs, Bs, Lf, Ef, RBs) ->
    ret_expr(lists:reverse(eval_lc1(E, Qs, Bs, Lf, Ef, [])), Bs, RBs).

eval_lc1(E, [{generate,_,P,L0}|Qs], Bs0, Lf, Ef, Acc0) ->
    {value,L1,_Bs1} = expr(L0, Bs0, Lf, Ef, none),
    CompFun = fun(Bs, Acc) -> eval_lc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_generate(L1, P, Bs0, Lf, Ef, CompFun, Acc0);
eval_lc1(E, [{b_generate,_,P,L0}|Qs], Bs0, Lf, Ef, Acc0) ->
    {value,Bin,_Bs1} = expr(L0, Bs0, Lf, Ef, none),
    CompFun = fun(Bs, Acc) -> eval_lc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_b_generate(Bin, P, Bs0, Lf, Ef, CompFun, Acc0);
eval_lc1(E, [F|Qs], Bs0, Lf, Ef, Acc) ->
    CompFun = fun(Bs) -> eval_lc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_filter(F, Bs0, Lf, Ef, CompFun, Acc);
eval_lc1(E, [], Bs, Lf, Ef, Acc) ->
    {value,V,_} = expr(E, Bs, Lf, Ef, none),
    [V|Acc].

%% eval_bc(Expr, [Qualifier], Bindings, LocalFunctionHandler, 
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value

eval_bc(E, Qs, Bs, Lf, Ef, RBs) ->
    ret_expr(eval_bc1(E, Qs, Bs, Lf, Ef, <<>>), Bs, RBs).

eval_bc1(E, [{b_generate,_,P,L0}|Qs], Bs0, Lf, Ef, Acc0) ->
    {value,Bin,_Bs1} = expr(L0, Bs0, Lf, Ef, none),
    CompFun = fun(Bs, Acc) -> eval_bc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_b_generate(Bin, P, Bs0, Lf, Ef, CompFun, Acc0);
eval_bc1(E, [{generate,_,P,L0}|Qs], Bs0, Lf, Ef, Acc0) ->
    {value,List,_Bs1} = expr(L0, Bs0, Lf, Ef, none),
    CompFun = fun(Bs, Acc) -> eval_bc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_generate(List, P, Bs0, Lf, Ef, CompFun, Acc0);
eval_bc1(E, [F|Qs], Bs0, Lf, Ef, Acc) ->
    CompFun = fun(Bs) -> eval_bc1(E, Qs, Bs, Lf, Ef, Acc) end,
    eval_filter(F, Bs0, Lf, Ef, CompFun, Acc);
eval_bc1(E, [], Bs, Lf, Ef, Acc) ->
    {value,V,_} = expr(E, Bs, Lf, Ef, none),
    <<Acc/bitstring,V/bitstring>>.

eval_generate([V|Rest], P, Bs0, Lf, Ef, CompFun, Acc) ->
    case match(P, V, new_bindings(), Bs0) of
	{match,Bsn} ->
	    Bs2 = add_bindings(Bsn, Bs0),
	    NewAcc = CompFun(Bs2, Acc),
	    eval_generate(Rest, P, Bs0, Lf, Ef, CompFun, NewAcc);
	nomatch -> 
	    eval_generate(Rest, P, Bs0, Lf, Ef, CompFun, Acc)
	end;
eval_generate([], _P, _Bs0, _Lf, _Ef, _CompFun, Acc) ->
    Acc;
eval_generate(Term, _P, _Bs0, _Lf, _Ef, _CompFun, _Acc) ->
    erlang:raise(error, {bad_generator,Term}, stacktrace()).

eval_b_generate(<<_/bitstring>>=Bin, P, Bs0, Lf, Ef, CompFun, Acc) ->
    Mfun = match_fun(Bs0),
    Efun = fun(Exp, Bs) -> expr(Exp, Bs, Lf, Ef, none) end,
    case eval_bits:bin_gen(P, Bin, new_bindings(), Bs0, Mfun, Efun) of
	{match, Rest, Bs1} ->
	    Bs2 = add_bindings(Bs1, Bs0),
	    NewAcc = CompFun(Bs2, Acc),
	    eval_b_generate(Rest, P, Bs0, Lf, Ef, CompFun, NewAcc);
	{nomatch, Rest} ->
	    eval_b_generate(Rest, P, Bs0, Lf, Ef, CompFun, Acc);
	done ->
	    Acc
    end;
eval_b_generate(Term, _P, _Bs0, _Lf, _Ef, _CompFun, _Acc) ->
    erlang:raise(error, {bad_generator,Term}, stacktrace()).

eval_filter(F, Bs0, Lf, Ef, CompFun, Acc) ->
    case erl_lint:is_guard_test(F) of
	true ->
	    case guard_test(F, Bs0, Lf, Ef) of
		{value,true,Bs1} -> CompFun(Bs1);
		{value,false,_} -> Acc
	    end;
	false ->
	    case expr(F, Bs0, Lf, Ef, none) of
		{value,true,Bs1} -> CompFun(Bs1);
		{value,false,_} -> Acc;
		{value,V,_} -> 
                    erlang:raise(error, {bad_filter,V}, stacktrace())
	    end
    end.

%% eval_map_fields([Field], Bindings, LocalFunctionHandler,
%%                 ExternalFuncHandler) ->
%%  {[{map_assoc | map_exact,Key,Value}],Bindings}

eval_map_fields(Fs, Bs, Lf, Ef) ->
    eval_map_fields(Fs, Bs, Lf, Ef, []).

eval_map_fields([{map_field_assoc,_,K0,V0}|Fs], Bs0, Lf, Ef, Acc) ->
    {value,K1,Bs1} = expr(K0, Bs0, Lf, Ef, none),
    {value,V1,Bs2} = expr(V0, Bs1, Lf, Ef, none),
    eval_map_fields(Fs, Bs2, Lf, Ef, [{map_assoc,K1,V1}|Acc]);
eval_map_fields([{map_field_exact,_,K0,V0}|Fs], Bs0, Lf, Ef, Acc) ->
    {value,K1,Bs1} = expr(K0, Bs0, Lf, Ef, none),
    {value,V1,Bs2} = expr(V0, Bs1, Lf, Ef, none),
    eval_map_fields(Fs, Bs2, Lf, Ef, [{map_exact,K1,V1}|Acc]);
eval_map_fields([], Bs, _Lf, _Ef, Acc) ->
    {lists:reverse(Acc),Bs}.


%% RBs is the bindings to return when the evalution of a function
%% (fun) has finished. If RBs =:= none, then the evalution took place
%% outside a function. If RBs =:= value, only the value (not the bindings)
%% is to be returned (to a compiled function).

ret_expr(V, _Bs, value) ->
    V;
ret_expr(V, Bs, none) ->
    {value,V,Bs};
ret_expr(V, _Bs, RBs) when is_list(RBs) ->
    {value,V,RBs}.

%% eval_fun(Arguments, {Bindings,LocalFunctionHandler,
%%                      ExternalFunctionHandler,Clauses}) -> Value
%% This function is called when the fun is called from compiled code
%% or from apply.

eval_fun(As, {Bs0,Lf,Ef,Cs}) ->
    eval_fun(Cs, As, Bs0, Lf, Ef, value).

eval_fun([{clause,_,H,G,B}|Cs], As, Bs0, Lf, Ef, RBs) ->
    case match_list(H, As, new_bindings(), Bs0) of
	{match,Bsn} ->                      % The new bindings for the head
	    Bs1 = add_bindings(Bsn, Bs0),   % which then shadow!
	    case guard(G, Bs1, Lf, Ef) of
		true -> exprs(B, Bs1, Lf, Ef, RBs);
		false -> eval_fun(Cs, As, Bs0, Lf, Ef, RBs)
	    end;
	nomatch ->
	    eval_fun(Cs, As, Bs0, Lf, Ef, RBs)
    end;
eval_fun([], As, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, function_clause, 
		 [{?MODULE,'-inside-an-interpreted-fun-',As}|stacktrace()]).


eval_named_fun(As, Fun, {Bs0,Lf,Ef,Cs,Name}) ->
    eval_named_fun(Cs, As, Bs0, Lf, Ef, Name, Fun, value).

eval_named_fun([{clause,_,H,G,B}|Cs], As, Bs0, Lf, Ef, Name, Fun, RBs) ->
    Bs1 = add_binding(Name, Fun, Bs0),
    case match_list(H, As, new_bindings(), Bs1) of
        {match,Bsn} ->                      % The new bindings for the head
            Bs2 = add_bindings(Bsn, Bs1),   % which then shadow!
            case guard(G, Bs2, Lf, Ef) of
                true -> exprs(B, Bs2, Lf, Ef, RBs);
                false -> eval_named_fun(Cs, As, Bs0, Lf, Ef, Name, Fun, RBs)
            end;
        nomatch ->
            eval_named_fun(Cs, As, Bs0, Lf, Ef, Name, Fun, RBs)
    end;
eval_named_fun([], As, _Bs, _Lf, _Ef, _Name, _Fun, _RBs) ->
    erlang:raise(error, function_clause,
                 [{?MODULE,'-inside-an-interpreted-fun-',As}|stacktrace()]).


%% expr_list(ExpressionList, Bindings)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Evaluate a list of expressions "in parallel" at the same level.

-spec(expr_list(ExpressionList, Bindings) -> {ValueList, NewBindings} when
      ExpressionList :: expression_list(),
      Bindings :: binding_struct(),
      ValueList :: [value()],
      NewBindings :: binding_struct()).
expr_list(Es, Bs) ->
    expr_list(Es, Bs, none, none).

-spec(expr_list(ExpressionList, Bindings, LocalFunctionHandler) ->
             {ValueList, NewBindings} when
      ExpressionList :: expression_list(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      ValueList :: [value()],
      NewBindings :: binding_struct()).
expr_list(Es, Bs, Lf) ->
    expr_list(Es, Bs, Lf, none).

-spec(expr_list(ExpressionList, Bindings, LocalFunctionHandler,
                NonLocalFunctionHandler) ->
             {ValueList, NewBindings} when
      ExpressionList :: expression_list(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      ValueList :: [value()],
      NewBindings :: binding_struct()).
expr_list(Es, Bs, Lf, Ef) ->
    expr_list(Es, [], Bs, Bs, Lf, Ef).    

expr_list([E|Es], Vs, BsOrig, Bs0, Lf, Ef) ->
    {value,V,Bs1} = expr(E, BsOrig, Lf, Ef, none),
    expr_list(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0), Lf, Ef);
expr_list([], Vs, _, Bs, _Lf, _Ef) ->
    {reverse(Vs),Bs}.

eval_op(Op, Arg1, Arg2, Bs, Ef, RBs) ->
    do_apply(erlang, Op, [Arg1,Arg2], Bs, Ef, RBs).

eval_op(Op, Arg, Bs, Ef, RBs) ->
    do_apply(erlang, Op, [Arg], Bs, Ef, RBs).

%% if_clauses(Clauses, Bindings, LocalFuncHandler, ExtFuncHandler, RBs)

if_clauses([{clause,_,[],G,B}|Cs], Bs, Lf, Ef, RBs) ->
    case guard(G, Bs, Lf, Ef) of
	true -> exprs(B, Bs, Lf, Ef, RBs);
	false -> if_clauses(Cs, Bs, Lf, Ef, RBs)
    end;
if_clauses([], _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, if_clause, stacktrace()).

%% try_clauses(Body, CaseClauses, CatchClauses, AfterBody, Bindings, 
%%             LocalFuncHandler, ExtFuncHandler, RBs)
%% When/if variable bindings between the different parts of a
%% try-catch expression are introduced this will have to be rewritten.
try_clauses(B, Cases, Catches, AB, Bs, Lf, Ef, RBs) ->
    try exprs(B, Bs, Lf, Ef, none) of
	{value,V,Bs1} when Cases =:= [] ->
	    ret_expr(V, Bs1, RBs);
	{value,V,Bs1} ->
	    case match_clause(Cases, [V], Bs1, Lf, Ef) of
		{B2,Bs2} ->
		    exprs(B2, Bs2, Lf, Ef, RBs);
		nomatch ->
		    erlang:raise(error, {try_clause,V}, stacktrace())
	    end
    catch
	Class:Reason when Catches =:= [] ->
	    %% Rethrow
	    erlang:raise(Class, Reason, stacktrace());
	Class:Reason ->
%%% 	    %% Set stacktrace
%%% 	    try erlang:raise(Class, Reason, stacktrace())
%%% 	    catch _:_ -> ok 
%%% 	    end,
            V = {Class,Reason,erlang:get_stacktrace()},
	    case match_clause(Catches, [V],Bs, Lf, Ef) of
		{B2,Bs2} ->
		    exprs(B2, Bs2, Lf, Ef, RBs);
		nomatch ->
		    erlang:raise(Class, Reason, stacktrace())
	    end
    after
	if AB =:= [] -> 
		Bs; % any
	   true ->
		exprs(AB, Bs, Lf, Ef, none)
	end
    end.

%% case_clauses(Value, Clauses, Bindings, LocalFuncHandler, ExtFuncHandler, 
%%              RBs)

case_clauses(Val, Cs, Bs, Lf, Ef, RBs) ->
    case match_clause(Cs, [Val], Bs, Lf, Ef) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf, Ef, RBs);
	nomatch ->
	    erlang:raise(error, {case_clause,Val}, stacktrace())
    end.

%%
%% receive_clauses(Clauses, Bindings, LocalFuncHnd,ExtFuncHnd, RBs)
%%
receive_clauses(Cs, Bs, Lf, Ef, RBs) ->
    receive_clauses(infinity, Cs, unused, Bs, Lf, Ef, RBs).
%%
%% receive_clauses(TimeOut, Clauses, TimeoutBody, Bindings, 
%%                 ExternalFuncHandler, LocalFuncHandler, RBs)
%%
receive_clauses(T, Cs, TB, Bs, Lf, Ef, RBs) ->
    F = fun (M) -> match_clause(Cs, [M], Bs, Lf, Ef) end,
    case prim_eval:'receive'(F, T) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf, Ef, RBs);
	timeout ->
	    {B, Bs1} = TB,
	    exprs(B, Bs1, Lf, Ef, RBs)
    end.

%% match_clause -> {Body, Bindings} or nomatch

-spec(match_clause(Clauses, ValueList, Bindings, LocalFunctionHandler) ->
             {Body, NewBindings} | nomatch when
      Clauses :: clauses(),
      ValueList :: [value()],
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      Body :: expression_list(),
      NewBindings :: binding_struct()).

match_clause(Cs, Vs, Bs, Lf) ->
    match_clause(Cs, Vs, Bs, Lf, none).

match_clause([{clause,_,H,G,B}|Cs], Vals, Bs, Lf, Ef) ->
    case match_list(H, Vals, Bs) of
	{match, Bs1} ->
	    case guard(G, Bs1, Lf, Ef) of
		true -> {B, Bs1};
		false -> match_clause(Cs, Vals, Bs, Lf, Ef)
	    end;
	nomatch -> match_clause(Cs, Vals, Bs, Lf, Ef)
    end;
match_clause([], _Vals, _Bs, _Lf, _Ef) ->
    nomatch.

%% guard(GuardTests, Bindings, LocalFuncHandler, ExtFuncHandler) -> bool()
%%  Evaluate a guard.  We test if the guard is a true guard.

guard(L=[G|_], Bs0, Lf, Ef) when is_list(G) ->
    guard1(L, Bs0, Lf, Ef);
guard(L, Bs0, Lf, Ef) ->
    guard0(L, Bs0, Lf, Ef).

%% disjunction of guard conjunctions
guard1([G|Gs], Bs0, Lf, Ef) when is_list(G) ->
    case guard0(G, Bs0, Lf, Ef) of
	true ->
	    true;
	false ->
	    guard1(Gs, Bs0, Lf, Ef)
    end;
guard1([], _Bs, _Lf, _Ef) -> false.

%% guard conjunction
guard0([G|Gs], Bs0, Lf, Ef) ->
    case erl_lint:is_guard_test(G) of
	true ->
	    case guard_test(G, Bs0, Lf, Ef) of
                {value,true,Bs} -> guard0(Gs, Bs, Lf, Ef);
                {value,false,_} -> false
	    end;
	false ->
	    erlang:raise(error, guard_expr, stacktrace())
    end;
guard0([], _Bs, _Lf, _Ef) -> true.

%% guard_test(GuardTest, Bindings, LocalFuncHandler, ExtFuncHandler) ->
%%	{value,bool(),NewBindings}.
%%  Evaluate one guard test. Never fails, returns bool().

guard_test({call,L,{atom,Ln,F},As0}, Bs0, Lf, Ef) ->
    TT = type_test(F),
    G = {call,L,{atom,Ln,TT},As0},
    expr_guard_test(G, Bs0, Lf, Ef);
guard_test({call,L,{remote,Lr,{atom,Lm,erlang},{atom,Lf,F}},As0},
           Bs0, Lf, Ef) ->
    TT = type_test(F),
    G = {call,L,{remote,Lr,{atom,Lm,erlang},{atom,Lf,TT}},As0},
    expr_guard_test(G, Bs0, Lf, Ef);
guard_test(G, Bs0, Lf, Ef) ->
    expr_guard_test(G, Bs0, Lf, Ef).

expr_guard_test(G, Bs0, Lf, Ef) ->
    try {value,true,_} = expr(G, Bs0, Lf, Ef, none)
    catch error:_ -> {value,false,Bs0} end.
    
type_test(integer) -> is_integer;
type_test(float) -> is_float;
type_test(number) -> is_number;
type_test(atom) -> is_atom;
type_test(list) -> is_list;
type_test(tuple) -> is_tuple;
type_test(pid) -> is_pid;
type_test(reference) -> is_reference;
type_test(port) -> is_port;
type_test(function) -> is_function;
type_test(binary) -> is_binary;
type_test(record) -> is_record;
type_test(map) -> is_map;
type_test(Test) -> Test.


%% match(Pattern, Term, Bindings) ->
%%	{match,NewBindings} | nomatch
%%      or erlang:error({illegal_pattern, Pattern}).
%%  Try to match Pattern against Term with the current bindings.

match(Pat, Term, Bs) ->
    match(Pat, Term, Bs, Bs).

%% Bs are the bindings that are augmented with new bindings. BBs are
%% the bindings used for "binsize" variables (in <<X:Y>>, Y is a
%% binsize variable).

match(Pat, Term, Bs, BBs) ->
    case catch match1(Pat, Term, Bs, BBs) of
	invalid ->
	    erlang:raise(error, {illegal_pattern,Pat}, stacktrace());
	Other ->
	    Other
    end.

string_to_conses([], _, Tail) -> Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

match1({atom,_,A0}, A, Bs, _BBs) ->
    case A of
	A0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({integer,_,I0}, I, Bs, _BBs) ->
    case I of
	I0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({float,_,F0}, F, Bs, _BBs) ->
    case F of
	F0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({char,_,C0}, C, Bs, _BBs) ->
    case C of
	C0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({var,_,'_'}, _, Bs, _BBs) ->		%Anonymous variable matches
    {match,Bs};					% everything, no new bindings
match1({var,_,Name}, Term, Bs, _BBs) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,_} ->
	    throw(nomatch);
	unbound ->
	    {match,add_binding(Name, Term, Bs)}
    end;
match1({match,_,Pat1,Pat2}, Term, Bs0, BBs) ->
    {match, Bs1} = match1(Pat1, Term, Bs0, BBs),
    match1(Pat2, Term, Bs1, BBs);
match1({string,_,S0}, S, Bs, _BBs) ->
    case S of
	S0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({nil,_}, Nil, Bs, _BBs) ->
    case Nil of
	[] -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({cons,_,H,T}, [H1|T1], Bs0, BBs) ->
    {match,Bs} = match1(H, H1, Bs0, BBs),
    match1(T, T1, Bs, BBs);
match1({cons,_,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({tuple,_,Elts}, Tuple, Bs, BBs) 
         when length(Elts) =:= tuple_size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs, BBs);
match1({tuple,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({map,_,Fs}, #{}=Map, Bs, BBs) ->
    match_map(Fs, Map, Bs, BBs);
match1({map,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({bin, _, Fs}, <<_/bitstring>>=B, Bs0, BBs) ->
    eval_bits:match_bits(Fs, B, Bs0, BBs,
			 match_fun(BBs),
			 fun(E, Bs) -> expr(E, Bs, none, none, none) end);
match1({bin,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({op,_,'++',{nil,_},R}, Term, Bs, BBs) ->
    match1(R, Term, Bs, BBs);
match1({op,_,'++',{cons,Li,{integer,L2,I},T},R}, Term, Bs, BBs) ->
    match1({cons,Li,{integer,L2,I},{op,Li,'++',T,R}}, Term, Bs, BBs);
match1({op,_,'++',{cons,Li,{char,L2,C},T},R}, Term, Bs, BBs) ->
    match1({cons,Li,{char,L2,C},{op,Li,'++',T,R}}, Term, Bs, BBs);
match1({op,_,'++',{string,Li,L},R}, Term, Bs, BBs) ->
    match1(string_to_conses(L, Li, R), Term, Bs, BBs);
match1({op,Line,Op,A}, Term, Bs, BBs) ->
    case partial_eval({op,Line,Op,A}) of
	{op,Line,Op,A} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs)
    end;
match1({op,Line,Op,L,R}, Term, Bs, BBs) ->
    case partial_eval({op,Line,Op,L,R}) of
	{op,Line,Op,L,R} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs)
    end;
match1(_, _, _Bs, _BBs) ->
    throw(invalid).

match_fun(BBs) ->
    fun(match, {L,R,Bs}) -> match1(L, R, Bs, BBs);
       (binding, {Name,Bs}) -> binding(Name, Bs);
       (add_binding, {Name,Val,Bs}) -> add_binding(Name, Val, Bs)
    end.

match_tuple([E|Es], Tuple, I, Bs0, BBs) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0, BBs),
    match_tuple(Es, Tuple, I+1, Bs, BBs);
match_tuple([], _, _, Bs, _BBs) ->
    {match,Bs}.

match_map([{map_field_exact, _, K, V}|Fs], Map, Bs0, BBs) ->
    Vm = try
	{value, Ke, _} = expr(K, BBs),
	maps:get(Ke,Map)
    catch error:_ ->
	throw(nomatch)
    end,
    {match, Bs} = match1(V, Vm, Bs0, BBs),
    match_map(Fs, Map, Bs, BBs);
match_map([], _, Bs, _) ->
    {match, Bs}.

%% match_list(PatternList, TermList, Bindings) ->
%%	{match,NewBindings} | nomatch
%%  Try to match a list of patterns against a list of terms with the
%%  current bindings.

match_list(Ps, Ts, Bs) ->
    match_list(Ps, Ts, Bs, Bs).

match_list([P|Ps], [T|Ts], Bs0, BBs) ->
    case match(P, T, Bs0, BBs) of
	{match,Bs1} -> match_list(Ps, Ts, Bs1, BBs);
	nomatch -> nomatch
    end;
match_list([], [], Bs, _BBs) ->
    {match,Bs};
match_list(_, _, _Bs, _BBs) ->
    nomatch.

%% new_bindings()
%% bindings(Bindings)
%% binding(Name, Bindings)
%% add_binding(Name, Value, Bindings)
%% del_binding(Name, Bindings)

-spec(new_bindings() -> binding_struct()).
new_bindings() -> orddict:new().

-spec(bindings(BindingStruct :: binding_struct()) -> bindings()).
bindings(Bs) -> orddict:to_list(Bs).

-spec(binding(Name, BindingStruct) -> {value, value()} | unbound when
      Name :: name(),
      BindingStruct :: binding_struct()).
binding(Name, Bs) ->
    case orddict:find(Name, Bs) of
	{ok,Val} -> {value,Val};
	error -> unbound
    end.

-spec(add_binding(Name, Value, BindingStruct) -> binding_struct() when
      Name :: name(),
      Value :: value(),
      BindingStruct :: binding_struct()).
add_binding(Name, Val, Bs) -> orddict:store(Name, Val, Bs).

-spec(del_binding(Name, BindingStruct) -> binding_struct() when
      Name :: name(),
      BindingStruct :: binding_struct()).
del_binding(Name, Bs) -> orddict:erase(Name, Bs).

add_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) -> orddict:store(Name, Val, Bs) end,
	  Bs2, orddict:to_list(Bs1)).

merge_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) ->
		  case orddict:find(Name, Bs) of
		      {ok,Val} -> Bs;		%Already with SAME value
		      {ok,V1} -> 
			  erlang:raise(error, {badmatch,V1}, stacktrace());
		      error -> orddict:store(Name, Val, Bs)
		  end end,
	  Bs2, orddict:to_list(Bs1)).

%% del_bindings(Bs1, Bs2) -> % del all in Bs1 from Bs2
%%     orddict:fold(
%%       fun (Name, Val, Bs) ->
%% 	      case orddict:find(Name, Bs) of
%% 		  {ok,Val} -> orddict:erase(Name, Bs);
%% 		  {ok,V1} -> erlang:raise(error,{badmatch,V1},stacktrace());
%% 		  error -> Bs
%% 	      end
%%       end, Bs2, Bs1).
%%----------------------------------------------------------------------------
%%
%% Evaluate expressions:
%% constants and 
%% op A
%% L op R
%% Things that evaluate to constants are accepted
%% and guard_bifs are allowed in constant expressions
%%----------------------------------------------------------------------------

is_constant_expr(Expr) ->
    case eval_expr(Expr) of
        {ok, X} when is_number(X) -> true;
        _ -> false
    end.

eval_expr(Expr) ->
    case catch ev_expr(Expr) of
        X when is_integer(X) -> {ok, X};
        X when is_float(X) -> {ok, X};
        X when is_atom(X) -> {ok,X};
        {'EXIT',Reason} -> {error, Reason};
        _ -> {error, badarg}
    end.

partial_eval(Expr) ->
    Line = line(Expr),
    case catch ev_expr(Expr) of
	X when is_integer(X) -> ret_expr(Expr,{integer,Line,X});
	X when is_float(X) -> ret_expr(Expr,{float,Line,X});
	X when is_atom(X) -> ret_expr(Expr,{atom,Line,X});
	_ ->
	    Expr
    end.

ev_expr({op,_,Op,L,R}) -> erlang:Op(ev_expr(L), ev_expr(R));
ev_expr({op,_,Op,A}) -> erlang:Op(ev_expr(A));
ev_expr({integer,_,X}) -> X;
ev_expr({char,_,X})    -> X;
ev_expr({float,_,X})   -> X;
ev_expr({atom,_,X})    -> X;
ev_expr({tuple,_,Es}) ->
    list_to_tuple([ev_expr(X) || X <- Es]);
ev_expr({nil,_}) -> [];
ev_expr({cons,_,H,T}) -> [ev_expr(H) | ev_expr(T)].
%%ev_expr({call,Line,{atom,_,F},As}) ->
%%    true = erl_internal:guard_bif(F, length(As)),
%%    apply(erlang, F, [ev_expr(X) || X <- As]);
%%ev_expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,F}},As}) ->
%%    true = erl_internal:guard_bif(F, length(As)),
%%    apply(erlang, F, [ev_expr(X) || X <- As]);

ret_expr(_Old, New) ->
    %%    io:format("~w: reduced ~s => ~s~n",
    %%	      [line(Old), erl_pp:expr(Old), erl_pp:expr(New)]),
    New.

line(Expr) -> element(2, Expr).

%% {?MODULE,expr,3} is still the stacktrace, despite the
%% fact that expr() now takes two, three or four arguments...
stacktrace() -> [{?MODULE,expr,3}].
