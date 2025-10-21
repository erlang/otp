%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-moduledoc """
The Erlang meta interpreter.

This module provides an interpreter for Erlang expressions. The expressions are
in the abstract syntax as returned by `m:erl_parse`, the Erlang parser, or
`m:io`.

## Local Function Handler

During evaluation of a function, no calls can be made to local functions. An
undefined function error would be generated. However, the optional argument
`LocalFunctionHandler` can be used to define a function that is called when
there is a call to a local function. The argument can have the following
formats:

- **`{value,Func}`** - This defines a local function handler that is called
  with:

  ```erlang
  Func(Name, Arguments)
  ```

  `Name` is the name of the local function (an atom) and `Arguments` is a list
  of the _evaluated_ arguments. The function handler returns the value of the
  local function. In this case, the current bindings cannot be accessed. To
  signal an error, the function handler calls [`exit/1`](`exit/1`) with a
  suitable exit value.

- **`{eval,Func}`** - This defines a local function handler that is called with:

  ```erlang
  Func(Name, Arguments, Bindings)
  ```

  `Name` is the name of the local function (an atom), `Arguments` is a list of
  the _unevaluated_ arguments, and `Bindings` are the current variable bindings.
  The function handler returns:

  ```erlang
  {value,Value,NewBindings}
  ```

  `Value` is the value of the local function and `NewBindings` are the updated
  variable bindings. In this case, the function handler must itself evaluate all
  the function arguments and manage the bindings. To signal an error, the
  function handler calls [`exit/1`](`exit/1`) with a suitable exit value.

- **`none`** - There is no local function handler.

## Non-Local Function Handler

The optional argument `NonLocalFunctionHandler` can be used to define a function
that is called in the following cases:

- A functional object (fun) is called.
- A built-in function is called.
- A function is called using the `M:F` syntax, where `M` and `F` are atoms or
  expressions.
- An operator `Op/A` is called (this is handled as a call to function
  `erlang:Op/A`).

Exceptions are calls to `erlang:apply/2,3`; neither of the function handlers are
called for such calls. The argument can have the following formats:

- **`{value,Func}`** - This defines a non-local function handler. The function
  may be called with two arguments:

  ```erlang
  Func(FuncSpec, Arguments)
  ```

  or three arguments:

  ```erlang
  Func(Anno, FuncSpec, Arguments)
  ```

  `Anno` is the [`erl_anno:anno()`](`t:erl_anno:anno/0`) of the node, `FuncSpec`
  is the name of the function of the form `{Module,Function}` or a fun, and
  `Arguments` is a list of the _evaluated_ arguments. The function handler
  returns the value of the function. To signal an error, the function handler
  calls [`exit/1`](`exit/1`) with a suitable exit value.

- **`none`** - There is no non-local function handler.

> #### Note {: .info }
>
> For calls such as `erlang:apply(Fun, Args)` or
> `erlang:apply(Module, Function, Args)`, the call of the non-local function
> handler corresponding to the call to `erlang:apply/2,3` itself
> (`Func({erlang, apply}, [Fun, Args])` or
> `Func({erlang, apply}, [Module, Function, Args])`) never takes place.
>
> The non-local function handler _is_ however called with the evaluated
> arguments of the call to `erlang:apply/2,3`: `Func(Fun, Args)` or
> `Func({Module, Function}, Args)` (assuming that `{Module, Function}` is not
> `{erlang, apply}`).
>
> Calls to functions defined by evaluating fun expressions `"fun ... end"` are
> also hidden from non-local function handlers.

The non-local function handler argument is probably not used as frequently as
the local function handler argument. A possible use is to call
[`exit/1`](`exit/1`) on calls to functions that for some reason are not allowed
to be called.
""".

-compile(nowarn_deprecated_catch).

%% An evaluator for Erlang abstract syntax.

-export([exprs/2,exprs/3,exprs/4,expr/2,expr/3,expr/4,expr/5,
         expr_list/2,expr_list/3,expr_list/4]).
-export([new_bindings/0,new_bindings/1,bindings/1,binding/2,
         add_binding/3,del_binding/2]).
-export([extended_parse_exprs/1, extended_parse_term/1]).
-export([is_constant_expr/1, partial_eval/1, eval_str/1]).

%% Is used by standalone Erlang (escript).
%% Also used by shell.erl.
-export([match_clause/4]).

-export([check_command/2, fun_data/1]).

-import(lists, [all/2,any/2,foldl/3,member/2,reverse/1]).

-export_type([binding_struct/0]).

-type(expression() :: erl_parse:abstract_expr()).
-doc "As returned by `erl_parse:parse_exprs/1` or `io:parse_erl_exprs/2`.".
-type(expressions() :: [erl_parse:abstract_expr()]).
-type(expression_list() :: [expression()]).
-type(clauses() :: [erl_parse:abstract_clause()]).
-type(name() :: term()).
-type(value() :: term()).
-type(bindings() :: [{name(), value()}]).
-doc """
A binding structure. It is either a `map` or an `orddict`. `erl_eval` will
always return the same type as the one given.
""".
-type(binding_struct() :: orddict:orddict() | map()).

-type(lfun_value_handler() :: fun((Name :: atom(),
                                   Arguments :: [term()]) ->
                                         Value :: value())).
-type(lfun_eval_handler() :: fun((Name :: atom(),
                                  Arguments :: expression_list(),
                                  Bindings :: binding_struct()) ->
                                        {value,
                                         Value :: value(),
                                         NewBindings :: binding_struct()})).
-doc """
Further described in section
[Local Function Handler](`m:erl_eval#module-local-function-handler`) in this module
""".
-type(local_function_handler() :: {value, lfun_value_handler()}
                                | {eval, lfun_eval_handler()}
                                | none).

-type(func_spec() :: {Module :: module(), Function :: atom()} | function()).
-type(nlfun_handler() :: fun((FuncSpec :: func_spec(),
                              Arguments :: [term()]) -> term())
                       | fun((Anno :: erl_anno:anno(), FuncSpec :: func_spec(),
                              Arguments :: [term()]) -> term())).
-doc """
Further described in section
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.
""".
-type(non_local_function_handler() :: {value, nlfun_handler()}
                                    | none).

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

empty_fun_used_vars() -> #{}.

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

-doc(#{equiv => exprs(Expressions, Bindings, none)}).
-spec(exprs(Expressions, Bindings) -> {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs(Exprs, Bs) ->
    case check_command(Exprs, Bs) of
        ok ->
            exprs(Exprs, Bs, none, none, none, empty_fun_used_vars());
        {error,{_Location,_Mod,Error}} ->
	    erlang:raise(error, Error, ?STACKTRACE)
    end.

-doc(#{equiv => exprs(Expressions, Bindings, LocalFunctionHandler, none)}).
-spec(exprs(Expressions, Bindings, LocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs(Exprs, Bs, Lf) ->
    exprs(Exprs, Bs, Lf, none, none, empty_fun_used_vars()).

-doc """
Evaluates `Expressions` with the set of bindings `Bindings`, where `Expressions`
is a sequence of expressions (in abstract syntax) of a type that can be returned
by `io:parse_erl_exprs/2`.

For an explanation of when and how to use arguments
`LocalFunctionHandler` and `NonLocalFunctionHandler`, see sections
[Local Function Handler](`m:erl_eval#module-local-function-handler`) and
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.

Returns `{value, Value, NewBindings}`
""".
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
    exprs(Exprs, Bs, Lf, Ef, none, empty_fun_used_vars()).

-spec(exprs(Expressions, Bindings, LocalFunctionHandler,
            NonLocalFunctionHandler, ReturnFormat, FunUsedVars) ->
             {value, Value, NewBindings} when
      Expressions :: expressions(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      ReturnFormat :: none | value,
      FunUsedVars :: erl_lint:fun_used_vars(),
      Value :: value(),
      NewBindings :: binding_struct()).
exprs([E], Bs0, Lf, Ef, RBs, FUVs) ->
    expr(E, Bs0, Lf, Ef, RBs, FUVs);
exprs([E|Es], Bs0, Lf, Ef, RBs, FUVs) ->
    RBs1 = none,
    {value,_V,Bs} = expr(E, Bs0, Lf, Ef, RBs1, FUVs),
    exprs(Es, Bs, Lf, Ef, RBs, FUVs).

%% maybe_match_exprs(Expression, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns one of:
%%	 {success,Value}
%%	 {failure,Value}
%%  or raises an exception.

maybe_match_exprs([{maybe_match,Anno,Lhs,Rhs0}|Es], Bs0, Lf, Ef) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Lf, Ef, none),
    case match(Lhs, Rhs, Anno, Bs1, Bs1, Ef) of
	{match,Bs} ->
            case Es of
                [] ->
                    {success,Rhs};
                [_|_] ->
                    maybe_match_exprs(Es, Bs, Lf, Ef)
            end;
	nomatch ->
            {failure,Rhs}
    end;
maybe_match_exprs([E], Bs0, Lf, Ef) ->
    {value,V,_Bs} = expr(E, Bs0, Lf, Ef, none),
    {success,V};
maybe_match_exprs([E|Es], Bs0, Lf, Ef) ->
    {value,_V,Bs} = expr(E, Bs0, Lf, Ef, none),
    maybe_match_exprs(Es, Bs, Lf, Ef).

%% expr(Expression, Bindings)
%% expr(Expression, Bindings, LocalFuncHandler)
%% expr(Expression, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns:
%%	 {value,Value,NewBindings}
%%    or {'EXIT', Reason}
%%
%% Only expr/2 checks the command by calling erl_lint. See exprs/2.

-doc(#{equiv => expr(Expression, Bindings, none)}).
-spec(expr(Expression, Bindings) -> {value, Value, NewBindings} when
      Expression :: expression(),
      Bindings :: binding_struct(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr(E, Bs) ->
    case check_command([E], Bs) of
        ok ->
            expr(E, Bs, none, none, none);
        {error,{_Location,_Mod,Error}} ->
	    erlang:raise(error, Error, ?STACKTRACE)
    end.

-doc(#{equiv => expr(Expression, Bindings, LocalFunctionHandler, none)}).
-spec(expr(Expression, Bindings, LocalFunctionHandler) ->
             {value, Value, NewBindings} when
      Expression :: expression(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr(E, Bs, Lf) ->
    expr(E, Bs, Lf, none, none).

-doc(#{equiv => expr(Expression, Bindings, LocalFunctionHandler,
                     NonLocalFunctionHandler, none)}).
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

-doc false.
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

-doc false.
fun_data(F) when is_function(F) ->
    case erlang:fun_info(F, module) of
        {module,?MODULE} ->
            case erlang:fun_info(F, env) of
                {env,[{_FAnno,FBs,_FLf,_FEf,_FUVs,FCs}]} ->
                    {fun_data,FBs,FCs};
                {env,[{_FAnno,FBs,_FLf,_FEf,_FUVs,FCs,FName}]} ->
                    {named_fun_data,FBs,FName,FCs}
            end;
        _ ->
            false
    end;
fun_data(_T) ->
    false.

-doc """
Evaluates `Expression` with the set of bindings `Bindings`. `Expression` is an
expression in abstract syntax.

For an explanation of when and how to use arguments `LocalFunctionHandler` and
`NonLocalFunctionHandler`, see sections
[Local Function Handler](`m:erl_eval#module-local-function-handler`) and
[Non-Local Function Handler](`m:erl_eval#module-non-local-function-handler`) in this
module.

Returns `{value, Value, NewBindings}` by default. If `ReturnFormat` is `value`,
only `Value` is returned.
""".
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
expr(Expr, Bs, Lf, Ef, Rbs) ->
    expr(Expr, Bs, Lf, Ef, Rbs, empty_fun_used_vars()).

-spec(expr(Expression, Bindings, LocalFunctionHandler,
           NonLocalFunctionHandler, ReturnFormat, FunUsedVars) ->
             {value, Value, NewBindings} | Value when
      Expression :: expression(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      NonLocalFunctionHandler :: non_local_function_handler(),
      ReturnFormat :: none | value,
      FunUsedVars :: erl_lint:fun_used_vars(),
      Value :: value(),
      NewBindings :: binding_struct()).
expr({var,Anno,V}, Bs, _Lf, Ef, RBs, _FUVs) ->
    case binding(V, Bs) of
	{value,Val} ->
            ret_expr(Val, Bs, RBs);
	unbound -> % Cannot not happen if checked by erl_lint
            apply_error({unbound,V}, ?STACKTRACE, Anno, Bs, Ef, RBs)
    end;
expr({char,_,C}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr(C, Bs, RBs);
expr({integer,_,I}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr(I, Bs, RBs);
expr({float,_,F}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr(F, Bs, RBs);
expr({atom,_,A}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr(A, Bs, RBs);
expr({string,_,S}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr(S, Bs, RBs);
expr({nil, _}, Bs, _Lf, _Ef, RBs, _FUVs) ->
    ret_expr([], Bs, RBs);
expr({cons,Anno,H0,T0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,H,Bs1} = expr(H0, Bs0, Lf, Ef, none, FUVs),
    {value,T,Bs2} = expr(T0, Bs0, Lf, Ef, none, FUVs),
    ret_expr([H|T], merge_bindings(Bs1, Bs2, Anno, Ef), RBs);
expr({lc,_,E,Qs}, Bs, Lf, Ef, RBs, FUVs) ->
    eval_lc(E, Qs, Bs, Lf, Ef, RBs, FUVs);
expr({bc,_,E,Qs}, Bs, Lf, Ef, RBs, FUVs) ->
    eval_bc(E, Qs, Bs, Lf, Ef, RBs, FUVs);
expr({mc,_,E,Qs}, Bs, Lf, Ef, RBs, FUVs) ->
    eval_mc(E, Qs, Bs, Lf, Ef, RBs, FUVs);
expr({tuple,_,Es}, Bs0, Lf, Ef, RBs, FUVs) ->
    {Vs,Bs} = expr_list(Es, Bs0, Lf, Ef, FUVs),
    ret_expr(list_to_tuple(Vs), Bs, RBs);
expr({record_field,Anno,_,Name,_}, Bs, _Lf, Ef, RBs, _FUVs) ->
    apply_error({undef_record,Name}, ?STACKTRACE, Anno, Bs, Ef, RBs);
expr({record_index,Anno,Name,_}, Bs, _Lf, Ef, RBs, _FUVs) ->
    apply_error({undef_record,Name}, ?STACKTRACE, Anno, Bs, Ef, RBs);
expr({record,Anno,Name,_}, Bs, _Lf, Ef, RBs, _FUVs) ->
    apply_error({undef_record,Name}, ?STACKTRACE, Anno, Bs, Ef, RBs);
expr({record,Anno,_,Name,_}, Bs, _Lf, Ef, RBs, _FUVs) ->
    apply_error({undef_record,Name}, ?STACKTRACE, Anno, Bs, Ef, RBs);

%% map
expr({map,Anno,Binding,Es}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value, Map0, Bs1} = expr(Binding, Bs0, Lf, Ef, none, FUVs),
    {Vs,Bs2} = eval_map_fields(Es, Bs0, Lf, Ef, FUVs),
    _ = maps:put(k, v, Map0),			%Validate map.
    Map1 = lists:foldl(fun ({map_assoc,K,V}, Mi) ->
			       maps:put(K, V, Mi);
			   ({map_exact,K,V}, Mi) ->
			       maps:update(K, V, Mi)
		       end, Map0, Vs),
    ret_expr(Map1, merge_bindings(Bs2, Bs1, Anno, Ef), RBs);
expr({map,_,Es}, Bs0, Lf, Ef, RBs, FUVs) ->
    {Vs,Bs} = eval_map_fields(Es, Bs0, Lf, Ef, FUVs),
    ret_expr(lists:foldl(fun
		({map_assoc,K,V}, Mi) -> maps:put(K,V,Mi)
	    end, maps:new(), Vs), Bs, RBs);

expr({block,_,Es}, Bs, Lf, Ef, RBs, FUVs) ->
    exprs(Es, Bs, Lf, Ef, RBs, FUVs);
expr({'if',Anno,Cs}, Bs, Lf, Ef, RBs, FUVs) ->
    if_clauses(Cs, Anno, Bs, Lf, Ef, RBs, FUVs);
expr({'case',Anno,E,Cs}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,Val,Bs} = expr(E, Bs0, Lf, Ef, none, FUVs),
    case_clauses(Val, Cs, Anno, Bs, Lf, Ef, RBs, FUVs);
expr({'try',Anno,B,Cases,Catches,AB}, Bs, Lf, Ef, RBs, FUVs) ->
    try_clauses(B, Cases, Catches, AB, Anno, Bs, Lf, Ef, RBs, FUVs);
expr({'receive',_,Cs}, Bs, Lf, Ef, RBs, FUVs) ->
    receive_clauses(Cs, Bs, Lf, Ef, RBs, FUVs);
expr({'receive',_, Cs, E, TB}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,T,Bs} = expr(E, Bs0, Lf, Ef, none, FUVs),
    receive_clauses(T, Cs, {TB,Bs}, Bs0, Lf, Ef, RBs, FUVs);
expr({'fun',_Anno,{function,Mod0,Name0,Arity0}}, Bs0, Lf, Ef, RBs, FUVs) ->
    {[Mod,Name,Arity],Bs} = expr_list([Mod0,Name0,Arity0], Bs0, Lf, Ef, FUVs),
    F = erlang:make_fun(Mod, Name, Arity),
    ret_expr(F, Bs, RBs);
expr({'fun',Anno,{function,Name,Arity}}, Bs0, Lf, Ef, RBs, FUVs) -> % R8
    case erl_internal:bif(Name, Arity) of
        true ->
            %% Auto-imported BIF. Create an external fun.
            ret_expr(fun erlang:Name/Arity, Bs0, RBs);
        false ->
            %% A local function assumed to be defined in the shell.
            %% Create a wrapper fun that will call the local fun.
            %% Calling the fun will succeed if the local fun is
            %% defined when the call is made.
            Args = [{var,Anno,list_to_atom("@arg" ++ [V])} ||
                       V <- lists:seq($a, $a+Arity-1)],
            H = Args,
            G = [{atom,Anno,true}],
            B = [{call,Anno,{atom,Anno,Name},Args}],
            Cs = [{clause,Anno,H,G,B}],
            expr({'fun',Anno,{clauses,Cs}}, Bs0, Lf, Ef, RBs, FUVs)
    end;
expr({'fun',Anno,{clauses,Cs}} = Ex, Bs, Lf, Ef, RBs, FUVs) ->
    {En,NewFUVs} = fun_used_bindings(Ex, Cs, Bs, FUVs),
    Info = {Anno,En,Lf,Ef,NewFUVs,Cs},

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
            L = erl_anno:location(Anno),
            Reason = {'argument_limit',{'fun',L,to_terms(Cs)}},
            apply_error(Reason, ?STACKTRACE, Anno, Bs, Ef, RBs)
    end,
    ret_expr(F, Bs, RBs);
expr({named_fun,Anno,Name,Cs} = Ex, Bs, Lf, Ef, RBs, FUVs) ->
    {En,NewFUVs} = fun_used_bindings(Ex, Cs, Bs, FUVs),
    Info = {Anno,En,Lf,Ef,NewFUVs,Cs,Name},

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
            L = erl_anno:location(Anno),
            Reason = {'argument_limit',{named_fun,L,Name,to_terms(Cs)}},
            apply_error(Reason, ?STACKTRACE, Anno, Bs, Ef, RBs)
    end,
    ret_expr(F, Bs, RBs);
expr({call,_,{remote,_,{atom,_,qlc},{atom,_,q}},[{lc,_,_E,_Qs}=LC | As0]},
     Bs0, Lf, Ef, RBs, FUVs) when length(As0) =< 1 ->
    %% No expansion or evaluation of module name or function name.
    MaxLine = find_maxline(LC),
    {LC1, D} = hide_calls(LC, MaxLine),
    case qlc:transform_from_evaluator(LC1, Bs0) of
        {ok,{call,A,Remote,[QLC]}} ->
            QLC1 = unhide_calls(QLC, MaxLine, D),
            expr({call,A,Remote,[QLC1 | As0]}, Bs0, Lf, Ef, RBs, FUVs);
        {not_ok,Error} ->
            ret_expr(Error, Bs0, RBs)
    end;
expr({call,A1,{remote,A2,{record_field,_,{atom,_,''},{atom,_,qlc}=Mod},
               {atom,_,q}=Func},
      [{lc,_,_E,_Qs} | As0]=As},
     Bs, Lf, Ef, RBs, FUVs) when length(As0) =< 1 ->
    expr({call,A1,{remote,A2,Mod,Func},As}, Bs, Lf, Ef, RBs, FUVs);
expr({call,Anno,{remote,_,Mod,Func},As0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,M,Bs1} = expr(Mod, Bs0, Lf, Ef, none, FUVs),
    {value,F,Bs2} = expr(Func, Bs0, Lf, Ef, none, FUVs),
    {As,Bs3} = expr_list(As0, merge_bindings(Bs1, Bs2, Anno, Ef), Lf, Ef, FUVs),
    %% M could be a parameterized module (not an atom).
    case is_atom(M) andalso erl_internal:bif(M, F, length(As)) of
        true ->
            bif(F, As, Anno, Bs3, Ef, RBs);
        false ->
            do_apply(M, F, As, Anno, Bs3, Ef, RBs)
    end;
expr({call,Anno,{atom,_,Func},As0}, Bs0, Lf, Ef, RBs, FUVs) ->
    case erl_internal:bif(Func, length(As0)) of
        true ->
            {As,Bs} = expr_list(As0, Bs0, Lf, Ef),
            bif(Func, As, Anno, Bs, Ef, RBs);
        false ->
            local_func(Func, As0, Anno, Bs0, Lf, Ef, RBs, FUVs)
    end;
expr({call,Anno,Func0,As0}, Bs0, Lf, Ef, RBs, FUVs) -> % function or {Mod,Fun}
    {value,Func,Bs1} = expr(Func0, Bs0, Lf, Ef, none, FUVs),
    {As,Bs2} = expr_list(As0, Bs1, Lf, Ef, FUVs),
    case Func of
	{M,F} when is_atom(M), is_atom(F) ->
            apply_error({badfun,Func}, ?STACKTRACE, Anno, Bs0, Ef, RBs);
	_ ->
	    do_apply(Func, As, Anno, Bs2, Ef, RBs)
    end;
expr({'catch',_,Expr}, Bs0, Lf, Ef, RBs, FUVs) ->
    try expr(Expr, Bs0, Lf, Ef, none, FUVs) of
        {value,V,Bs} ->
            ret_expr(V, Bs, RBs)
    catch
        throw:Term ->
            ret_expr(Term, Bs0, RBs);
        exit:Reason ->
            ret_expr({'EXIT',Reason}, Bs0, RBs);
        error:Reason:Stacktrace ->
            ret_expr({'EXIT',{Reason,Stacktrace}}, Bs0, RBs)
    end;
expr({match,Anno,Lhs,Rhs0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Lf, Ef, none, FUVs),
    case match(Lhs, Rhs, Anno, Bs1, Bs1, Ef) of
	{match,Bs} ->
            ret_expr(Rhs, Bs, RBs);
	nomatch -> apply_error({badmatch,Rhs}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
    end;
expr({'maybe',_,Es}, Bs, Lf, Ef, RBs, _FUVs) ->
    {_,Val} = maybe_match_exprs(Es, Bs, Lf, Ef),
    ret_expr(Val, Bs, RBs);
expr({'maybe',Anno,Es,{'else',_,Cs}}, Bs0, Lf, Ef, RBs, FUVs) ->
    case maybe_match_exprs(Es, Bs0, Lf, Ef) of
        {success,Val} ->
            ret_expr(Val, Bs0, RBs);
        {failure,Val} ->
            case match_clause(Cs, [Val], Bs0, Lf, Ef) of
                {B, Bs} ->
                    exprs(B, Bs, Lf, Ef, RBs, FUVs);
                nomatch ->
                    apply_error({else_clause,Val}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
            end
    end;
expr({op,Anno,Op,A0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,A,Bs} = expr(A0, Bs0, Lf, Ef, none, FUVs),
    eval_op(Op, A, Anno, Bs, Ef, RBs);
expr({op,Anno,'andalso',L0,R0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none, FUVs),
    V = case L of
	    true ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none, FUVs),
		R;
	    false -> false;
	    _ -> apply_error({badarg,L}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
	end,
    ret_expr(V, Bs1, RBs);
expr({op,Anno,'orelse',L0,R0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none, FUVs),
    V = case L of
	    true -> true;
	    false ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none, FUVs),
		R;
	    _ -> apply_error({badarg,L}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
	end,
    ret_expr(V, Bs1, RBs);
expr({op,Anno,Op,L0,R0}, Bs0, Lf, Ef, RBs, FUVs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none, FUVs),
    {value,R,Bs2} = expr(R0, Bs0, Lf, Ef, none, FUVs),
    eval_op(Op, L, R, Anno, merge_bindings(Bs1, Bs2, Anno, Ef), Ef, RBs);
expr({bin,_,Fs}, Bs0, Lf, Ef, RBs, FUVs) ->
    EvalFun = fun(E, B) -> expr(E, B, Lf, Ef, none, FUVs) end,
    ErrorFun = fun(A, R, S) -> apply_error(R, S, A, Bs0, Ef, RBs) end,
    {value,V,Bs} = eval_bits:expr_grp(Fs, Bs0, EvalFun, ErrorFun),
    ret_expr(V, Bs, RBs);
expr({remote,Anno,_,_}, Bs0, _Lf, Ef, RBs, _FUVs) ->
    apply_error({badexpr,':'}, ?STACKTRACE, Anno, Bs0, Ef, RBs).

apply_error(Reason, Stack, Anno, Bs0, Ef, RBs) ->
    do_apply(erlang, raise, [error, Reason, Stack], Anno, Bs0, Ef, RBs).

find_maxline(LC) ->
    put('$erl_eval_max_line', 0),
    F = fun(A) ->
                case erl_anno:is_anno(A) of
                    true ->
                        L = erl_anno:line(A),
                        case
                            is_integer(L) and (L > get('$erl_eval_max_line'))
                        of
                            true -> put('$erl_eval_max_line', L);
                            false -> ok
                        end;
                    false -> ok
                end
        end,
    _ = erl_parse:map_anno(F, LC),
    erase('$erl_eval_max_line').

fun_used_bindings(Fun, Cs, Bs, FUVs) ->
    {Used,InnerFUVs} =
        case FUVs of
            %% If this clause is in our fun used vars tree,
            %% then we do not need to compute to traverse it again.
            #{Cs := UsedAndFUVs} ->
                UsedAndFUVs;

            #{} ->
                %% Save only used variables in the function environment.
                AllUsedVars = erl_lint:used_vars([Fun], bindings(Bs)),

                %% At the root we should see only a single function,
                %% so we extract its used vars and its tree out.
                [{_,UsedAndFUVs}] = maps:to_list(AllUsedVars),
                UsedAndFUVs
        end,

    {filter_bindings(fun(K,_V) -> member(K,Used) end, Bs),InnerFUVs}.

hide_calls(LC, MaxLine) ->
    LineId0 = MaxLine + 1,
    {NLC, _, D} = hide(LC, LineId0, maps:new()),
    {NLC, D}.

%% Local calls are hidden from qlc so they are not expanded.
hide({call,A,{atom,_,N}=Atom,Args}, Id0, D0) ->
    {NArgs, Id, D} = hide(Args, Id0, D0),
    C = case erl_internal:bif(N, length(Args)) of
            true ->
                {call,A,Atom,NArgs};
            false ->
                Anno = erl_anno:new(Id),
                {call,Anno,{remote,A,{atom,A,m},{atom,A,f}},NArgs}
        end,
    {C, Id+1, maps:put(Id, {call,Atom}, D)};
hide(T0, Id0, D0) when is_tuple(T0) ->
    {L, Id, D} = hide(tuple_to_list(T0), Id0, D0),
    {list_to_tuple(L), Id, D};
hide([E0 | Es0], Id0, D0) ->
    {E, Id1, D1} = hide(E0, Id0, D0),
    {Es, Id, D} = hide(Es0, Id1, D1),
    {[E | Es], Id, D};
hide(E, Id, D) ->
    {E, Id, D}.

unhide_calls({call,Anno,{remote,A,{atom,A,m},{atom,A,f}}=F,Args},
             MaxLine, D) ->
    Line = erl_anno:line(Anno),
    if
        Line > MaxLine ->
            {call,Atom} = map_get(Line, D),
            {call,A,Atom,unhide_calls(Args, MaxLine, D)};
        true ->
            {call,Anno,F,unhide_calls(Args, MaxLine, D)}
    end;
unhide_calls(T, MaxLine, D) when is_tuple(T) ->
    list_to_tuple(unhide_calls(tuple_to_list(T), MaxLine, D));
unhide_calls([E | Es], MaxLine, D) ->
    [unhide_calls(E, MaxLine, D) | unhide_calls(Es, MaxLine, D)];
unhide_calls(E, _MaxLine, _D) ->
    E.

%% local_func(Function, Arguments, Anno, Bindings, LocalFuncHandler,
%%            ExternalFuncHandler, RBs, FunUsedVars) ->
%%	{value,Value,Bindings} | Value when
%%	LocalFuncHandler = {value,F} | {value,F,Eas} |
%%                         {eval,F}  | {eval,F,Eas}  | none.

local_func(Func, As0, _Anno, Bs0, {value,F}, Ef, value, FUVs) ->
    {As1,_Bs1} = expr_list(As0, Bs0, {value,F}, Ef, FUVs),
    %% Make tail recursive calls when possible.
    F(Func, As1);
local_func(Func, As0, _Anno, Bs0, {value,F}, Ef, RBs, FUVs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F}, Ef, FUVs),
    ret_expr(F(Func, As1), Bs1, RBs);
local_func(Func, As0, Anno, Bs0, {value,F,Eas}, Ef, RBs, FUVs) ->
    Fun = fun(Name, Args) -> apply(F, [Name,Args|Eas]) end,
    local_func(Func, As0, Anno, Bs0, {value, Fun}, Ef, RBs, FUVs);
local_func(Func, As, Anno, Bs, {eval,F}, _Ef, RBs, _FUVs) ->
    local_func2(F(Func, As, Bs), Anno, RBs);
local_func(Func, As, Anno, Bs, {eval,F,Eas}, _Ef, RBs, _FUVs) ->
    local_func2(apply(F, [Func,As,Bs|Eas]), Anno, RBs);
%% These two clauses are for backwards compatibility.
local_func(Func, As0, _Anno, Bs0, {M,F}, Ef, RBs, FUVs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {M,F}, Ef, FUVs),
    ret_expr(M:F(Func,As1), Bs1, RBs);
local_func(Func, As, Anno, _Bs, {M,F,Eas}, _Ef, RBs, _FUVs) ->
    local_func2(apply(M, F, [Func,As|Eas]), Anno, RBs);
%% Default unknown function handler to undefined function.
local_func(Func, As0, Anno, Bs0, none, Ef, RBs, _FUVs) ->
    apply_error(undef, [{?MODULE,Func,length(As0)}|?STACKTRACE], Anno, Bs0, Ef, RBs).

local_func2({value,V,Bs}, _Anno, RBs) ->
    ret_expr(V, Bs, RBs);
local_func2({eval,F,As,Bs}, Anno, RBs) -> % This reply is not documented.
    %% The shell found F. erl_eval tries to do a tail recursive call,
    %% something the shell cannot do. Do not use Ef here.
    do_apply(F, As, Anno, Bs, none, RBs).

%% bif(Name, Arguments, RBs)
%%  Evaluate the Erlang auto-imported function Name. erlang:apply/2,3
%%  are "hidden" from the external function handler.

bif(apply, [erlang,apply,As], Anno, Bs, Ef, RBs) ->
    bif(apply, As, Anno, Bs, Ef, RBs);
bif(apply, [M,F,As], Anno, Bs, Ef, RBs) ->
    do_apply(M, F, As, Anno, Bs, Ef, RBs);
bif(apply, [F,As], Anno, Bs, Ef, RBs) ->
    do_apply(F, As, Anno, Bs, Ef, RBs);
bif(Name, As, Anno, Bs, Ef, RBs) ->
    do_apply(erlang, Name, As, Anno, Bs, Ef, RBs).

%% do_apply(Func, Arguments, Bindings, ExternalFuncHandler, RBs) ->
%%	{value,Value,Bindings} | Value when
%%	ExternalFuncHandler = {value,F} | none,
%%  Func = fun()

do_apply(Func, As, Anno, Bs0, Ef, RBs) ->
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
        {{env,[{FAnno,FBs,FLf,FEf,FFUVs,FCs}]},_} ->
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
                    eval_fun(FCs, As, FAnno, FBs, FLf, FEf, NRBs, FFUVs);
                _ ->
                    apply_error({badarity,{Func,As}}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
            end;
        {{env,[{FAnno,FBs,FLf,FEf,FFUVs,FCs,FName}]},_} ->
            NRBs = if
                       RBs =:= none -> Bs0;
                       true -> RBs
                   end,
            case {erlang:fun_info(Func, arity), length(As)} of
                {{arity, Arity}, Arity} ->
                    eval_named_fun(FCs, As, FAnno, FBs, FLf, FEf, FName, Func, NRBs, FFUVs);
                _ ->
                    apply_error({badarity,{Func,As}}, ?STACKTRACE, Anno, Bs0, Ef, RBs)
            end;
        {no_env,none} when RBs =:= value ->
            %% Make tail recursive calls when possible.
            apply(Func, As);
        {no_env,none} ->
            ret_expr(apply(Func, As), Bs0, RBs);
        {no_env,{value,F}} when RBs =:= value ->
            do_apply(F, Anno, Func, As);
        {no_env,{value,F}} ->
            ret_expr(do_apply(F, Anno, Func, As), Bs0, RBs)
    end.

do_apply(Mod, Func, As, Anno, Bs0, Ef, RBs) ->
    case Ef of
        none when RBs =:= value ->
            %% Make tail recursive calls when possible.
            apply(Mod, Func, As);
        none ->
            ret_expr(apply(Mod, Func, As), Bs0, RBs);
        {value,F} when RBs =:= value ->
            do_apply(F, Anno, {Mod,Func}, As);
        {value,F} ->
            ret_expr(do_apply(F, Anno, {Mod,Func}, As), Bs0, RBs)
    end.

do_apply(F, Anno, FunOrModFun, Args) when is_function(F, 3) ->
    F(Anno, FunOrModFun, Args);
do_apply(F, _Anno, FunOrModFun, Args) when is_function(F, 2) ->
    F(FunOrModFun, Args).

%% eval_lc(Expr, [Qualifier], Bindings, LocalFunctionHandler,
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value

eval_lc(E, Qs, Bs, Lf, Ef, RBs, FUVs) ->
    ret_expr(lists:reverse(eval_lc1(E, Qs, Bs, Lf, Ef, FUVs, [])), Bs, RBs).

eval_lc1(E, [{zip, Anno, Gens}|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    {VarList, Bs1} = convert_gen_values(Gens, [], Bs0, Lf, Ef, FUVs),
    eval_zip(E, [{zip, Anno, VarList}|Qs], Bs1, Lf, Ef, FUVs, Acc0, fun eval_lc1/7);
eval_lc1(E, [Q|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    case is_generator(Q) of
        true ->
            CF = fun(Bs, Acc) -> eval_lc1(E, Qs, Bs, Lf, Ef, FUVs, Acc) end,
            eval_generator(Q, Bs0, Lf, Ef, FUVs, Acc0, CF);
        false ->
            CF = fun(Bs) -> eval_lc1(E, Qs, Bs, Lf, Ef, FUVs, Acc0) end,
            eval_filter(Q, Bs0, Lf, Ef, CF, FUVs, Acc0)
    end;
eval_lc1(E, [], Bs, Lf, Ef, FUVs, Acc) ->
    {value,V,_} = expr(E, Bs, Lf, Ef, none, FUVs),
    [V|Acc].

%% convert values for generator vars from abstract form to flattened lists
convert_gen_values([{Generate, Anno, P, L0}|Qs], Acc, Bs0, Lf, Ef,FUVs)
    when Generate =:= generate;
         Generate =:= generate_strict;
         Generate =:= b_generate;
         Generate =:= b_generate_strict ->
    {value,L1,_Bs1} = expr(L0, Bs0, Lf, Ef, none, FUVs),
    convert_gen_values(Qs, [{Generate, Anno, P, L1}|Acc], Bs0, Lf, Ef, FUVs);
convert_gen_values([{Generate, Anno, P, Map0}|Qs], Acc, Bs0, Lf, Ef,FUVs)
    when Generate =:= m_generate;
         Generate =:= m_generate_strict ->
    {map_field_exact,_,K,V} = P,
    {value,Map,_Bs1} = expr(Map0, Bs0, Lf, Ef, none, FUVs),
    Iter = case is_map(Map) of
               true ->
                   maps:iterator(Map);
               false ->
                   %% Validate iterator.
                   try maps:foreach(fun(_, _) -> ok end, Map) of
                       _ ->
                           Map
                   catch
                       _:_ ->
                           apply_error({bad_generator,Map}, ?STACKTRACE,
                                       Anno, Bs0, Ef, none)
                   end
           end,
    convert_gen_values(Qs, [{Generate, Anno, {tuple, Anno, [K, V]}, Iter}|Acc],
                       Bs0, Lf, Ef, FUVs);
convert_gen_values([], Acc, Bs0, _Lf, _Ef, _FUVs) ->
    {reverse(Acc), Bs0}.

bind_all_generators(Gens, Bs0, Lf, Ef, FUVs, StrictPats) ->
    Bs1 = new_bindings(Bs0),
    Bs2 = case is_map(Bs1) of
              true -> Bs1#{strict_pats => StrictPats};
              false -> orddict:store(strict_pats, StrictPats, Bs1)
    end,
    bind_all_generators1(Gens, [], Bs2, Lf, Ef, FUVs, continue).

bind_all_generators1([{Generate, Anno, P, <<_/bitstring>>=Bin}|Qs],
                     Acc, Bs0, Lf, Ef, FUVs, ContinueSkip)
  when Generate =:= b_generate;
       Generate =:= b_generate_strict ->
    Mfun = match_fun(Bs0, Ef),
    Efun = fun(Exp, Bs) -> expr(Exp, Bs, Lf, Ef, none) end,
    ErrorFun = fun(A, R, S) -> apply_error(R, S, A, Bs0, Ef, none) end,
    case {eval_bits:bin_gen(P, Bin, new_bindings(Bs0), Bs0, Mfun,
                            Efun, ErrorFun),
          Generate} of
        {{match, Rest, Bs1}, _} ->
            Bs2 = zip_add_bindings(Bs1, Bs0),
            case {Bs2, Generate, ContinueSkip} of
                {nomatch, b_generate, _} ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, Rest}|Acc],
                                         Bs0, Lf, Ef, FUVs, skip);
                {nomatch_strict, _, _} ->
                    {Acc, error};
                {nomatch, b_generate_strict, _} ->
                    {Acc, error};
                {_, b_generate, skip} ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, Rest}|Acc],
                                         Bs0, Lf, Ef, FUVs, ContinueSkip);
                _ ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, Rest}|Acc],
                                         Bs2, Lf, Ef, FUVs, ContinueSkip)
            end;
        {{nomatch, Rest}, b_generate} ->
            bind_all_generators1(Qs, [{b_generate, Anno, P, Rest}|Acc],
                                 Bs0, Lf, Ef, FUVs, skip);
        {{nomatch, _Rest}, b_generate_strict} ->
            {Acc, error};
        {done, b_generate} when Bin =/= <<>> ->
            {[], done};
        {done, b_generate_strict} when Bin =/= <<>> ->
            {Acc, error};
        {done, b_generate} ->
            case ContinueSkip of
                continue -> {[], done};
                skip -> {[], skip}
            end
    end;
bind_all_generators1([{Generate, Anno, P, [H|T]}|Qs],
                     Acc, Bs0, Lf, Ef, FUVs, ContinueSkip)
  when Generate =:= generate;
       Generate =:= generate_strict ->
    case {match(P, H, Anno, new_bindings(Bs0), Bs0, Ef), Generate} of
        {{match,Bsn}, _} ->
            Bs2 = zip_add_bindings(Bsn, Bs0),
            case {Bs2, Generate, ContinueSkip} of
                {nomatch, generate, _} ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, T}|Acc],
                                         Bs0, Lf, Ef, FUVs, skip);
                {nomatch_strict, _, _} ->
                    {Acc, error};
                {nomatch, generate_strict, _} ->
                    {Acc, error};
                {_, generate, skip} ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, T}|Acc],
                                         Bs0, Lf, Ef, FUVs, skip);
                _ ->
                    bind_all_generators1(Qs,[{Generate, Anno, P, T}|Acc],
                                         Bs2, Lf, Ef, FUVs, ContinueSkip)
            end;
        {nomatch, generate} ->
            %% match/6 returns nomatch. Skip this value
            bind_all_generators1(Qs, [{generate, Anno, P, T}|Acc],
                                 Bs0, Lf, Ef, FUVs, skip);
        {nomatch, generate_strict} ->
            {Acc, error}
    end;
bind_all_generators1([{Generate, Anno, P, Iter0}|Qs],
                     Acc, Bs0, Lf, Ef, FUVs, ContinueSkip)
  when Generate =:= m_generate;
       Generate =:= m_generate_strict->
    case maps:next(Iter0) of
        {K,V,Iter} ->
            case {match(P, {K,V}, Anno, new_bindings(Bs0), Bs0, Ef), Generate} of
                {{match,Bsn}, _} ->
                    Bs2 = zip_add_bindings(Bsn, Bs0),
                    case {Bs2, Generate, ContinueSkip} of
                        {nomatch, m_generate, _} ->
                            bind_all_generators1(Qs,[{Generate, Anno, P, Iter}|Acc],
                                                 Bs0, Lf, Ef, FUVs, skip);
                        {nomatch_strict, _, _} ->
                            {Acc, error};
                        {nomatch, m_generate_strict, _} ->
                            {Acc, error};
                        {_, m_generate, skip} ->
                            bind_all_generators1(Qs, [{Generate, Anno, P, Iter}|Acc],
                                                 Bs0, Lf, Ef, FUVs, skip);
                        _ -> bind_all_generators1(Qs,[{Generate, Anno, P, Iter}|Acc],
                                                  Bs2, Lf, Ef, FUVs, ContinueSkip)
                    end;
                {nomatch, m_generate} ->
                    bind_all_generators1(Qs, [{m_generate, Anno, P, Iter}|Acc],
                                         Bs0, Lf, Ef, FUVs, skip);
                {nomatch, m_generate_strict} ->
                    {Acc, error}
            end;
        none ->
            case ContinueSkip of
                continue -> {[], done};
                skip -> {[], skip}
            end
    end;
bind_all_generators1([{generate,_,_,[]}|_], _, _, _, _, _,_) ->
    %% no more values left for a var, time to return
    {[],done};
bind_all_generators1([{generate_strict,_,_,[]}|_], _, _, _, _, _,_) ->
    %% no more values left for a var, time to return
    {[],done};
bind_all_generators1([{generate, _Anno, _P, _Term}|_Qs], Acc, _Bs0,_Lf, _Ef, _FUVs,_) ->
    {Acc, error};
bind_all_generators1([{generate_strict, _Anno, _P, _Term}|_Qs], Acc, _Bs0,_Lf, _Ef, _FUVs,_) ->
    {Acc, error};
bind_all_generators1([{b_generate, _Anno, _P, _Term}|_Qs], Acc, _Bs0,_Lf, _Ef, _FUVs,_) ->
    {Acc, error};
bind_all_generators1([{b_generate_strict, _Anno, _P, _Term}|_Qs], Acc, _Bs0,_Lf, _Ef, _FUVs,_) ->
    {Acc, error};
bind_all_generators1([], [_H|_T] = Acc, Bs0, _Lf, _Ef, _FUVs, continue) ->
    %% all vars are bind for this round
    {Acc, Bs0};
bind_all_generators1([], [_H|_T] = Acc, _Bs0,_Lf, _Ef, _FUVs, skip) ->
    {Acc, skip}.

check_bad_generators([{Generate,_,_,V}|T], Env, Acc)
    when Generate =:= generate;
         Generate =:= generate_strict ->
    check_bad_generators(T, Env, [V|Acc]);
check_bad_generators([{Generate,_,_,Iter0}|T], Env, Acc)
    when Generate =:= m_generate;
         Generate =:= m_generate_strict ->
    case maps:next(Iter0) of
        none -> check_bad_generators(T, Env, [#{}|Acc]);
        _ -> check_bad_generators(T, Env, [#{K => V || K := V <- Iter0}|Acc])
    end;
check_bad_generators([{Generate,_,P,<<_/bitstring>>=Bin}|T], {Bs0, Lf, Ef}, Acc)
    when Generate =:= b_generate;
         Generate =:= b_generate_strict ->
    Mfun = match_fun(Bs0, Ef),
    Efun = fun(Exp, Bs) -> expr(Exp, Bs, Lf, Ef, none) end,
    ErrorFun = fun(A, R, S) -> apply_error(R, S, A, Bs0, Ef, none) end,
    case eval_bits:bin_gen(P, Bin, new_bindings(Bs0), Bs0, Mfun, Efun, ErrorFun) of
        done ->
            check_bad_generators(T, {Bs0, Lf, Ef}, [<<>>|Acc]);
        _ ->
            check_bad_generators(T, {Bs0, Lf, Ef}, [Bin|Acc])
    end;
check_bad_generators([{b_generate,_,_,Term}|T], Env, Acc) ->
    check_bad_generators(T, Env, [Term|Acc]);
check_bad_generators([{b_generate_strict,_,_,Term}|T], Env, Acc) ->
    check_bad_generators(T, Env, [Term|Acc]);
check_bad_generators([], _, Acc)->
    case any(fun is_generator_end/1, Acc) of
        false ->
            %% None of the generators has reached its end.
            {ok, list_to_tuple(reverse(Acc))};
        true ->
            case all(fun(V) -> is_generator_end(V) end, Acc) of
                true ->
                    %% All generators have reached their end.
                    {ok, list_to_tuple(reverse(Acc))};
                false ->
                    {error, {bad_generators,list_to_tuple(reverse(Acc))}}
            end
    end.

is_generator_end([]) -> true;
is_generator_end(<<>>) -> true;
is_generator_end(Other) -> Other =:= #{}.

get_vars(Lit) ->
    get_vars(Lit, []).

get_vars({tuple,_,Es}, Vs) -> get_list_vars(Es, Vs);
get_vars({var,_,V}, Vs) -> ordsets:add_element(V, Vs);
get_vars([{bin_element,_,V,_,_}|BinElements], Vs0) ->
    Vs1 = get_vars(V, Vs0),
    get_list_vars(BinElements, Vs1);
get_vars([_|_]=Ls, Vs) -> get_list_vars(Ls, Vs);
get_vars(_, Vs) -> Vs.

get_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> get_vars(L, Vs0) end, Vs, Ls).

get_strict_patterns([{Generate,_,P,_}|Qs], Acc)
  when Generate =:= generate_strict;
       Generate =:= m_generate_strict ->
    Vars = get_vars(P),
    get_strict_patterns(Qs, Vars ++ Acc);
get_strict_patterns([{b_generate_strict,_,P,_}|Qs], Acc) ->
    Vars = case P of
        {bin,_,BinElements} ->
            get_vars(BinElements, Acc);
        _ -> []
    end,
    get_strict_patterns(Qs, Vars ++ Acc);
get_strict_patterns([_|Qs], Acc) ->
    get_strict_patterns(Qs, Acc);
get_strict_patterns([], Acc) ->
    sets:to_list(sets:from_list(Acc)).

%% eval_bc(Expr, [Qualifier], Bindings, LocalFunctionHandler,
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value

eval_bc(E, Qs, Bs, Lf, Ef, RBs, FUVs) ->
    ret_expr(eval_bc1(E, Qs, Bs, Lf, Ef, FUVs, <<>>), Bs, RBs).

eval_bc1(E, [{zip, Anno, Gens}|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    {VarList, Bs1} = convert_gen_values(Gens, [], Bs0, Lf, Ef, FUVs),
    eval_zip(E, [{zip, Anno, VarList}|Qs], Bs1, Lf, Ef, FUVs, Acc0, fun eval_bc1/7);
eval_bc1(E, [Q|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    case is_generator(Q) of
        true ->
            CF = fun(Bs, Acc) -> eval_bc1(E, Qs, Bs, Lf, Ef, FUVs, Acc) end,
            eval_generator(Q, Bs0, Lf, Ef, FUVs, Acc0, CF);
        false ->
            CF = fun(Bs) -> eval_bc1(E, Qs, Bs, Lf, Ef, FUVs, Acc0) end,
            eval_filter(Q, Bs0, Lf, Ef, CF, FUVs, Acc0)
    end;
eval_bc1(E, [], Bs, Lf, Ef, FUVs, Acc) ->
    {value,V,_} = expr(E, Bs, Lf, Ef, none, FUVs),
    <<Acc/bitstring,V/bitstring>>.

%% eval_mc(Expr, [Qualifier], Bindings, LocalFunctionHandler,
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value

eval_mc(E, Qs, Bs, Lf, Ef, RBs, FUVs) ->
    L = eval_mc1(E, Qs, Bs, Lf, Ef, FUVs, []),
    Map = maps:from_list(reverse(L)),
    ret_expr(Map, Bs, RBs).

eval_mc1(E, [{zip, Anno, Gens}|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    {VarList, Bs1} = convert_gen_values(Gens, [], Bs0, Lf, Ef, FUVs),
    eval_zip(E, [{zip, Anno, VarList}|Qs], Bs1, Lf, Ef, FUVs, Acc0, fun eval_mc1/7);
eval_mc1(E, [Q|Qs], Bs0, Lf, Ef, FUVs, Acc0) ->
    case is_generator(Q) of
        true ->
            CF = fun(Bs, Acc) -> eval_mc1(E, Qs, Bs, Lf, Ef, FUVs, Acc) end,
            eval_generator(Q, Bs0, Lf, Ef, FUVs, Acc0, CF);
        false ->
            CF = fun(Bs) -> eval_mc1(E, Qs, Bs, Lf, Ef, FUVs, Acc0) end,
            eval_filter(Q, Bs0, Lf, Ef, CF, FUVs, Acc0)
    end;
eval_mc1({map_field_assoc,Lfa,K0,V0}, [], Bs, Lf, Ef, FUVs, Acc) ->
    {value,KV,_} = expr({tuple,Lfa,[K0,V0]}, Bs, Lf, Ef, none, FUVs),
    [KV|Acc].

eval_zip(E, [{zip, Anno, VarList}|Qs], Bs0, Lf, Ef, FUVs, Acc0, Fun) ->
    Gens = case check_bad_generators(VarList, {Bs0, Lf, Ef}, []) of
               {ok, Acc} -> Acc;
               {error, Reason} ->
                   apply_error(Reason, ?STACKTRACE, Anno, Bs0, Ef, none)
           end,
    StrictPats = get_strict_patterns(VarList, []),
    {Rest, Bs1} = bind_all_generators(VarList, Bs0, Lf, Ef, FUVs, StrictPats),
    case {Rest, Qs, Bs1} of
        {_, _, error} -> apply_error({bad_generators,Gens}, ?STACKTRACE, Anno, Bs0, Ef, none);
        {[], [], _} -> Acc0;
        {[], _, _} -> Acc0;
        {_,_,done} -> Acc0;
        {_, _, skip} ->
            eval_zip(E, [{zip, Anno, reverse(Rest)}|Qs], Bs0, Lf, Ef, FUVs, Acc0, Fun);
        {_, _, _} ->
            Acc1 = Fun(E, Qs, add_bindings(Bs1, Bs0), Lf, Ef, FUVs, Acc0),
            eval_zip(E, [{zip, Anno, reverse(Rest)}|Qs], Bs0, Lf, Ef, FUVs, Acc1, Fun)
    end.

eval_generator({Generate,Anno,P,L0}, Bs0, Lf, Ef, FUVs, Acc0, CompFun)
  when Generate =:= generate;
       Generate =:= generate_strict ->
    {value,L1,_Bs1} = expr(L0, Bs0, Lf, Ef, none, FUVs),
    eval_generate(L1, P, Anno, Bs0, Lf, Ef, CompFun,
                  Generate =:= generate, Acc0);
eval_generator({Generate,Anno,P,Bin0}, Bs0, Lf, Ef, FUVs, Acc0, CompFun)
  when Generate =:= b_generate;
       Generate =:= b_generate_strict ->
    {value,Bin,_Bs1} = expr(Bin0, Bs0, Lf, Ef, none, FUVs),
    eval_b_generate(Bin, P, Anno, Bs0, Lf, Ef, CompFun,
                    Generate =:= b_generate, Acc0);
eval_generator({Generate,Anno,P,Map0}, Bs0, Lf, Ef, FUVs, Acc0, CompFun)
  when Generate =:= m_generate;
       Generate =:= m_generate_strict ->
    {map_field_exact,_,K,V} = P,
    {value,Map,_Bs1} = expr(Map0, Bs0, Lf, Ef, none, FUVs),
    Iter = case is_map(Map) of
               true ->
                   maps:iterator(Map);
               false ->
                   %% Validate iterator.
                   try maps:foreach(fun(_, _) -> ok end, Map) of
                       _ ->
                           Map
                   catch
                       _:_ ->
                           apply_error({bad_generator,Map}, ?STACKTRACE,
                                       Anno, Bs0, Ef, none)
                   end
           end,
    eval_m_generate(Iter, {tuple,Anno,[K,V]}, Anno, Bs0, Lf, Ef, CompFun,
                    Generate =:= m_generate, Acc0).

eval_generate([V|Rest], P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc) ->
    case match(P, V, Anno, new_bindings(Bs0), Bs0, Ef) of
	{match,Bsn} ->
            Bs2 = add_bindings(Bsn, Bs0),
            NewAcc = CompFun(Bs2, Acc),
            eval_generate(Rest, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, NewAcc);
        nomatch when Relaxed ->
            eval_generate(Rest, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc);
        nomatch ->
            apply_error({badmatch, V}, ?STACKTRACE, Anno, Bs0, Ef, none)
    end;
eval_generate([], _P, _Anno, _Bs0, _Lf, _Ef, _CompFun, _Relaxed, Acc) ->
    Acc;
eval_generate(Term, _P, Anno, Bs0, _Lf, Ef, _CompFun, _Relaxed, _Acc) ->
    apply_error({bad_generator,Term}, ?STACKTRACE, Anno, Bs0, Ef, none).

eval_b_generate(<<_/bitstring>>=Bin, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc) ->
    Mfun = match_fun(Bs0, Ef),
    Efun = fun(Exp, Bs) -> expr(Exp, Bs, Lf, Ef, none) end,
    ErrorFun = fun(A, R, S) -> apply_error(R, S, A, Bs0, Ef, none) end,
    case eval_bits:bin_gen(P, Bin, new_bindings(Bs0), Bs0, Mfun, Efun, ErrorFun) of
        {match, Rest, Bs1} ->
            Bs2 = add_bindings(Bs1, Bs0),
            NewAcc = CompFun(Bs2, Acc),
            eval_b_generate(Rest, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, NewAcc);
        {nomatch, Rest} when Relaxed ->
            eval_b_generate(Rest, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc);
        {nomatch, _Rest} ->
            apply_error({badmatch, Bin}, ?STACKTRACE, Anno, Bs0, Ef, none);
        done when not Relaxed, Bin =/= <<>> ->
            apply_error({badmatch, Bin}, ?STACKTRACE, Anno, Bs0, Ef, none);
        done ->
            Acc
    end;
eval_b_generate(Term, _P, Anno, Bs0, _Lf, Ef, _CompFun, _Relaxed, _Acc) ->
    apply_error({bad_generator,Term}, ?STACKTRACE, Anno, Bs0, Ef, none).

eval_m_generate(Iter0, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc0) ->
    case maps:next(Iter0) of
        {K,V,Iter} ->
            case match(P, {K,V}, Anno, new_bindings(Bs0), Bs0, Ef) of
                {match,Bsn} ->
                    Bs2 = add_bindings(Bsn, Bs0),
                    Acc = CompFun(Bs2, Acc0),
                    eval_m_generate(Iter, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc);
                nomatch when Relaxed ->
                    eval_m_generate(Iter, P, Anno, Bs0, Lf, Ef, CompFun, Relaxed, Acc0);
                nomatch ->
                    apply_error({badmatch, {K,V}}, ?STACKTRACE, Anno, Bs0, Ef, none)
            end;
        none ->
            Acc0
    end.

eval_filter(F, Bs0, Lf, Ef, CompFun, FUVs, Acc) ->
    case erl_lint:is_guard_test(F) of
	true ->
	    case guard_test(F, Bs0, Lf, Ef) of
		{value,true,Bs1} -> CompFun(Bs1);
		{value,false,_} -> Acc
	    end;
	false ->
	    case expr(F, Bs0, Lf, Ef, none, FUVs) of
		{value,true,Bs1} -> CompFun(Bs1);
		{value,false,_} -> Acc;
		{value,V,_} ->
                    apply_error({bad_filter,V}, ?STACKTRACE, element(2, F), Bs0, Ef, none)
	    end
    end.

is_generator({generate,_,_,_}) -> true;
is_generator({generate_strict,_,_,_}) -> true;
is_generator({b_generate,_,_,_}) -> true;
is_generator({b_generate_strict,_,_,_}) -> true;
is_generator({m_generate,_,_,_}) -> true;
is_generator({m_generate_strict,_,_,_}) -> true;
is_generator(_) -> false.

%% eval_map_fields([Field], Bindings, LocalFunctionHandler,
%%                 ExternalFuncHandler) ->
%%  {[{map_assoc | map_exact,Key,Value}],Bindings}

eval_map_fields(Fs, Bs, Lf, Ef, FUVs) ->
    eval_map_fields(Fs, Bs, Lf, Ef, FUVs, []).

eval_map_fields([{map_field_assoc,_,K0,V0}|Fs], Bs0, Lf, Ef, FUVs, Acc) ->
    {value,K1,Bs1} = expr(K0, Bs0, Lf, Ef, none, FUVs),
    {value,V1,Bs2} = expr(V0, Bs1, Lf, Ef, none, FUVs),
    eval_map_fields(Fs, Bs2, Lf, Ef, FUVs, [{map_assoc,K1,V1}|Acc]);
eval_map_fields([{map_field_exact,_,K0,V0}|Fs], Bs0, Lf, Ef, FUVs, Acc) ->
    {value,K1,Bs1} = expr(K0, Bs0, Lf, Ef, none, FUVs),
    {value,V1,Bs2} = expr(V0, Bs1, Lf, Ef, none, FUVs),
    eval_map_fields(Fs, Bs2, Lf, Ef, FUVs, [{map_exact,K1,V1}|Acc]);
eval_map_fields([], Bs, _Lf, _Ef, _FUVs, Acc) ->
    {lists:reverse(Acc),Bs}.


%% RBs is the bindings to return when the evalution of a function
%% (fun) has finished. If RBs =:= none, then the evalution took place
%% outside a function. If RBs =:= value, only the value (not the bindings)
%% is to be returned (to a compiled function).

ret_expr(V, _Bs, value) ->
    V;
ret_expr(V, Bs, none) ->
    {value,V,Bs};
ret_expr(V, _Bs, RBs) when is_list(RBs); is_map(RBs) ->
    {value,V,RBs}.

%% eval_fun(Arguments, {Anno,Bindings,LocalFunctionHandler,
%%                      ExternalFunctionHandler,FunUsedVars,Clauses}) -> Value
%% This function is called when the fun is called from compiled code
%% or from apply.

eval_fun(As, {Anno,Bs0,Lf,Ef,FUVs,Cs}) ->
    eval_fun(Cs, As, Anno, Bs0, Lf, Ef, value, FUVs).

eval_fun([{clause,_,H,G,B}|Cs], As, Anno, Bs0, Lf, Ef, RBs, FUVs) ->
    case match_list(H, As, Anno, new_bindings(Bs0), Bs0, Ef) of
	{match,Bsn} ->                      % The new bindings for the head
	    Bs1 = add_bindings(Bsn, Bs0),   % which then shadow!
	    case guard(G, Bs1, Lf, Ef) of
		true -> exprs(B, Bs1, Lf, Ef, RBs, FUVs);
		false -> eval_fun(Cs, As, Anno, Bs0, Lf, Ef, RBs, FUVs)
	    end;
	nomatch ->
	    eval_fun(Cs, As, Anno, Bs0, Lf, Ef, RBs, FUVs)
    end;
eval_fun([], As, Anno, Bs, _Lf, Ef, RBs, _FUVs) ->
    Stack = [{?MODULE,'-inside-an-interpreted-fun-',As}|?STACKTRACE],
    apply_error(function_clause, Stack, Anno, Bs, Ef, RBs).


eval_named_fun(As, Fun, {Anno,Bs0,Lf,Ef,FUVs,Cs,Name}) ->
    eval_named_fun(Cs, As, Anno, Bs0, Lf, Ef, Name, Fun, value, FUVs).

eval_named_fun([{clause,_,H,G,B}|Cs], As, Anno, Bs0, Lf, Ef, Name, Fun, RBs, FUVs) ->
    Bs1 = add_binding(Name, Fun, Bs0),
    case match_list(H, As, Anno, new_bindings(Bs0), Bs1, Ef) of
        {match,Bsn} ->                      % The new bindings for the head
            Bs2 = add_bindings(Bsn, Bs1),   % which then shadow!
            case guard(G, Bs2, Lf, Ef) of
                true -> exprs(B, Bs2, Lf, Ef, RBs, FUVs);
                false -> eval_named_fun(Cs, As, Anno, Bs0, Lf, Ef, Name, Fun, RBs, FUVs)
            end;
        nomatch ->
            eval_named_fun(Cs, As, Anno, Bs0, Lf, Ef, Name, Fun, RBs, FUVs)
    end;
eval_named_fun([], As, Anno, Bs, _Lf, Ef, _Name, _Fun, RBs, _FUVs) ->
    Stack = [{?MODULE,'-inside-an-interpreted-fun-',As}|?STACKTRACE],
    apply_error(function_clause, Stack, Anno, Bs, Ef, RBs).


%% expr_list(ExpressionList, Bindings)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Evaluate a list of expressions "in parallel" at the same level.

-doc(#{equiv => expr_list(ExpressionList, Bindings, none)}).
-spec(expr_list(ExpressionList, Bindings) -> {ValueList, NewBindings} when
      ExpressionList :: expression_list(),
      Bindings :: binding_struct(),
      ValueList :: [value()],
      NewBindings :: binding_struct()).
expr_list(Es, Bs) ->
    expr_list(Es, Bs, none, none, empty_fun_used_vars()).

-doc(#{equiv => expr_list(ExpressionList, Bindings, LocalFunctionHandler, none)}).
-spec(expr_list(ExpressionList, Bindings, LocalFunctionHandler) ->
             {ValueList, NewBindings} when
      ExpressionList :: expression_list(),
      Bindings :: binding_struct(),
      LocalFunctionHandler :: local_function_handler(),
      ValueList :: [value()],
      NewBindings :: binding_struct()).
expr_list(Es, Bs, Lf) ->
    expr_list(Es, Bs, Lf, none, empty_fun_used_vars()).

-doc """
Evaluates a list of expressions in parallel, using the same initial bindings for
each expression. Attempts are made to merge the bindings returned from each
evaluation.

This function is useful in `LocalFunctionHandler`, see section
[Local Function Handler](`m:erl_eval#module-local-function-handler`) in this module.

Returns `{ValueList, NewBindings}`.
""".
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
    expr_list(Es, Bs, Lf, Ef, empty_fun_used_vars()).

expr_list(Es, Bs, Lf, Ef, FUVs) ->
    expr_list(Es, [], Bs, Bs, Lf, Ef, FUVs).

expr_list([E|Es], Vs, BsOrig, Bs0, Lf, Ef, FUVs) ->
    {value,V,Bs1} = expr(E, BsOrig, Lf, Ef, none, FUVs),
    expr_list(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0, element(2, E), Ef), Lf, Ef, FUVs);
expr_list([], Vs, _, Bs, _Lf, _Ef, _FUVs) ->
    {reverse(Vs),Bs}.

eval_op(Op, Arg1, Arg2, Anno, Bs, Ef, RBs) ->
    do_apply(erlang, Op, [Arg1,Arg2], Anno, Bs, Ef, RBs).

eval_op(Op, Arg, Anno, Bs, Ef, RBs) ->
    do_apply(erlang, Op, [Arg], Anno, Bs, Ef, RBs).

%% if_clauses(Clauses, Anno, Bindings, LocalFuncHandler, ExtFuncHandler, RBs)

if_clauses([{clause,_,[],G,B}|Cs], Anno, Bs, Lf, Ef, RBs, FUVs) ->
    case guard(G, Bs, Lf, Ef) of
	true -> exprs(B, Bs, Lf, Ef, RBs, FUVs);
	false -> if_clauses(Cs, Anno, Bs, Lf, Ef, RBs, FUVs)
    end;
if_clauses([], Anno, Bs, _Lf, Ef, RBs, _FUVs) ->
    apply_error(if_clause, ?STACKTRACE, Anno, Bs, Ef, RBs).

%% try_clauses(Body, CaseClauses, CatchClauses, AfterBody, Anno, Bindings,
%%             LocalFuncHandler, ExtFuncHandler, RBs)

try_clauses(B, Cases, Catches, AB, Anno, Bs, Lf, Ef, RBs, FUVs) ->
    check_stacktrace_vars(Catches, Anno, Bs, Ef, RBs),
    try exprs(B, Bs, Lf, Ef, none, FUVs) of
	{value,V,Bs1} when Cases =:= [] ->
	    ret_expr(V, Bs1, RBs);
	{value,V,Bs1} ->
	    case match_clause(Cases, [V], Bs1, Lf, Ef) of
		{B2,Bs2} ->
		    exprs(B2, Bs2, Lf, Ef, RBs, FUVs);
		nomatch ->
                    apply_error({try_clause, V}, ?STACKTRACE, Anno, Bs, Ef, RBs)
	    end
    catch
	Class:Reason:Stacktrace when Catches =:= [] ->
	    erlang:raise(Class, Reason, Stacktrace);
	Class:Reason:Stacktrace ->
            V = {Class,Reason,Stacktrace},
	    case match_clause(Catches, [V], Bs, Lf, Ef) of
		{B2,Bs2} ->
		    exprs(B2, Bs2, Lf, Ef, RBs, FUVs);
		nomatch ->
		    erlang:raise(Class, Reason, Stacktrace)
	    end
    after
	if AB =:= [] ->
		Bs; % any
	   true ->
		exprs(AB, Bs, Lf, Ef, none, FUVs)
	end
    end.


check_stacktrace_vars([{clause,_,[{tuple,_,[_,_,STV]}],_,_}|Cs], Anno, Bs, Ef, RBs) ->
    case STV of
        {var,_,V} ->
            case binding(V, Bs) of
                {value, _} ->
                    apply_error(stacktrace_bound, ?STACKTRACE, Anno, Bs, Ef, RBs);
                unbound ->
                    check_stacktrace_vars(Cs, Anno, Bs, Ef, RBs)
            end;
        _ ->
            Reason = {illegal_stacktrace_variable,STV},
            apply_error(Reason, ?STACKTRACE, Anno, Bs, Ef, RBs)
    end;
check_stacktrace_vars([], _Anno, _Bs, _Ef, _RBs) ->
    ok.

%% case_clauses(Value, Clauses, Anno, Bindings, LocalFuncHandler,
%%              ExtFuncHandler, RBs)

case_clauses(Val, Cs, Anno, Bs, Lf, Ef, RBs, FUVs) ->
    case match_clause(Cs, [Val], Bs, Lf, Ef) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf, Ef, RBs, FUVs);
	nomatch ->
	    apply_error({case_clause,Val}, ?STACKTRACE, Anno, Bs, Ef, RBs)
    end.

%%
%% receive_clauses(Clauses, Bindings, LocalFuncHnd, ExtFuncHnd, RBs)
%%
receive_clauses(Cs, Bs, Lf, Ef, RBs, FUVs) ->
    receive_clauses(infinity, Cs, unused, Bs, Lf, Ef, RBs, FUVs).
%%
%% receive_clauses(TimeOut, Clauses, TimeoutBody, Bindings,
%%                 ExternalFuncHandler, LocalFuncHandler, RBs)
%%
receive_clauses(T, Cs, TB, Bs, Lf, Ef, RBs, FUVs) ->
    F = fun (M) -> match_clause(Cs, [M], Bs, Lf, Ef) end,
    case prim_eval:'receive'(F, T) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf, Ef, RBs, FUVs);
	timeout ->
	    {B, Bs1} = TB,
	    exprs(B, Bs1, Lf, Ef, RBs, FUVs)
    end.

%% match_clause -> {Body, Bindings} or nomatch

-doc false.
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

match_clause([{clause,Anno,H,G,B}|Cs], Vals, Bs, Lf, Ef) ->
    case match_list(H, Vals, Anno, Bs, Bs, Ef) of
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
            apply_error(guard_expr, ?STACKTRACE, element(2, G), Bs0, Ef, none)
    end;
guard0([], _Bs, _Lf, _Ef) -> true.

%% guard_test(GuardTest, Bindings, LocalFuncHandler, ExtFuncHandler) ->
%%	{value,bool(),NewBindings}.
%%  Evaluate one guard test. Never fails, returns bool().

guard_test({call,A,{atom,Ln,F},As0}, Bs0, Lf, Ef) ->
    TT = type_test(F),
    G = {call,A,{atom,Ln,TT},As0},
    expr_guard_test(G, Bs0, Lf, Ef);
guard_test({call,A,{remote,Ar,{atom,Am,erlang},{atom,Af,F}},As0},
           Bs0, Lf, Ef) ->
    TT = type_test(F),
    G = {call,A,{remote,Ar,{atom,Am,erlang},{atom,Af,TT}},As0},
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

%% match(Pattern, Term, Anno, NewBindings, Bindings, ExternalFunHnd) ->
%%      {match,NewBindings} | nomatch
%%      or erlang:error({illegal_pattern, Pattern}).
%%
%% Try to match Pattern against Term with the current bindings.
%% Bs are the bindings that are augmented with new bindings. BBs are
%% the bindings used for "binsize" variables (in <<X:Y>>, Y is a
%% binsize variable).

match(Pat, Term, Anno, Bs, BBs, Ef) ->
    case catch match1(Pat, Term, Bs, BBs, Ef) of
	invalid ->
            apply_error({illegal_pattern,to_term(Pat)}, ?STACKTRACE, Anno, Bs, Ef, none);
	Other ->
	    Other
    end.

string_to_conses([], _, Tail) -> Tail;
string_to_conses([E|Rest], Anno, Tail) ->
    {cons, Anno, {integer, Anno, E}, string_to_conses(Rest, Anno, Tail)}.

match1({atom,_,A0}, A, Bs, _BBs, _Ef) ->
    case A of
	A0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({integer,_,I0}, I, Bs, _BBs, _Ef) ->
    case I of
	I0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({float,_,F0}, F, Bs, _BBs, _Ef) ->
    case F of
	F0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({char,_,C0}, C, Bs, _BBs, _Ef) ->
    case C of
	C0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({var,_,'_'}, _, Bs, _BBs, _Ef) ->		%Anonymous variable matches
    {match,Bs};					% everything, no new bindings
match1({var,_,Name}, Term, Bs, _BBs, _Ef) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,_} ->
	    throw(nomatch);
	unbound ->
	    {match,add_binding(Name, Term, Bs)}
    end;
match1({match,_,Pat1,Pat2}, Term, Bs0, BBs, Ef) ->
    {match, Bs1} = match1(Pat1, Term, Bs0, BBs, Ef),
    match1(Pat2, Term, Bs1, BBs, Ef);
match1({string,_,S0}, S, Bs, _BBs, _Ef) ->
    case S of
	S0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({nil,_}, Nil, Bs, _BBs, _Ef) ->
    case Nil of
	[] -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({cons,_,H,T}, [H1|T1], Bs0, BBs, Ef) ->
    {match,Bs} = match1(H, H1, Bs0, BBs, Ef),
    match1(T, T1, Bs, BBs, Ef);
match1({cons,_,_,_}, _, _Bs, _BBs, _Ef) ->
    throw(nomatch);
match1({tuple,_,Elts}, Tuple, Bs, BBs, Ef)
         when length(Elts) =:= tuple_size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs, BBs, Ef);
match1({tuple,_,_}, _, _Bs, _BBs, _Ef) ->
    throw(nomatch);
match1({map,_,Fs}, #{}=Map, Bs, BBs, Ef) ->
    match_map(Fs, Map, Bs, BBs, Ef);
match1({map,_,_}, _, _Bs, _BBs, _Ef) ->
    throw(nomatch);
match1({bin, _, Fs}, <<_/bitstring>>=B, Bs0, BBs, Ef) ->
    EvalFun = fun(E, Bs) ->
                      case erl_lint:is_guard_expr(E) of
                          true -> ok;
                          false -> throw(invalid)
                      end,
                      try
                          expr(E, Bs, none, none, none)
                      catch
                          error:{unbound, _} ->
                              throw(invalid)
                      end
              end,
    ErrorFun = fun(A, R, S) -> apply_error(R, S, A, Bs0, Ef, none) end,
    eval_bits:match_bits(Fs, B, Bs0, BBs, match_fun(BBs, Ef), EvalFun, ErrorFun);
match1({bin,_,_}, _, _Bs, _BBs, _Ef) ->
    throw(nomatch);
match1({op,_,'++',{nil,_},R}, Term, Bs, BBs, Ef) ->
    match1(R, Term, Bs, BBs, Ef);
match1({op,_,'++',{cons,Ai,{integer,A2,I},T},R}, Term, Bs, BBs, Ef) ->
    match1({cons,Ai,{integer,A2,I},{op,Ai,'++',T,R}}, Term, Bs, BBs, Ef);
match1({op,_,'++',{cons,Ai,{char,A2,C},T},R}, Term, Bs, BBs, Ef) ->
    match1({cons,Ai,{char,A2,C},{op,Ai,'++',T,R}}, Term, Bs, BBs, Ef);
match1({op,_,'++',{string,Ai,L},R}, Term, Bs, BBs, Ef) ->
    match1(string_to_conses(L, Ai, R), Term, Bs, BBs, Ef);
match1({op,Anno,Op,A}, Term, Bs, BBs, Ef) ->
    case partial_eval({op,Anno,Op,A}) of
	{op,Anno,Op,A} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs, Ef)
    end;
match1({op,Anno,Op,L,R}, Term, Bs, BBs, Ef) ->
    case partial_eval({op,Anno,Op,L,R}) of
	{op,Anno,Op,L,R} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs, Ef)
    end;
match1(_, _, _Bs, _BBs, _Ef) ->
    throw(invalid).

match_fun(BBs, Ef) ->
    fun(match, {L,R,Bs}) -> match1(L, R, Bs, BBs, Ef);
       (binding, {Name,Bs}) -> binding(Name, Bs);
       (add_binding, {Name,Val,Bs}) -> add_binding(Name, Val, Bs)
    end.

match_tuple([E|Es], Tuple, I, Bs0, BBs, Ef) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0, BBs, Ef),
    match_tuple(Es, Tuple, I+1, Bs, BBs, Ef);
match_tuple([], _, _, Bs, _BBs, _Ef) ->
    {match,Bs}.

match_map([{map_field_exact, _, K, V}|Fs], Map, Bs0, BBs, Ef) ->
    Vm = try
	{value, Ke, _} = expr(K, BBs),
	maps:get(Ke,Map)
    catch error:_ ->
	throw(nomatch)
    end,
    {match, Bs} = match1(V, Vm, Bs0, BBs, Ef),
    match_map(Fs, Map, Bs, BBs, Ef);
match_map([], _, Bs, _, _) ->
    {match, Bs}.

%% match_list(PatternList, TermList, Anno, NewBindings, Bindings, ExternalFunHnd) ->
%%	{match,NewBindings} | nomatch
%%  Try to match a list of patterns against a list of terms with the
%%  current bindings.

match_list([P|Ps], [T|Ts], Anno, Bs0, BBs, Ef) ->
    case match(P, T, Anno, Bs0, BBs, Ef) of
	{match,Bs1} -> match_list(Ps, Ts, Anno, Bs1, BBs, Ef);
	nomatch -> nomatch
    end;
match_list([], [], _Anno, Bs, _BBs, _Ef) ->
    {match,Bs};
match_list(_, _, _Anno, _Bs, _BBs, _Ef) ->
    nomatch.

%% new_bindings()
%% bindings(Bindings)
%% binding(Name, Bindings)
%% add_binding(Name, Value, Bindings)
%% del_binding(Name, Bindings)

-doc "Returns an empty binding structure.".
-spec(new_bindings() -> binding_struct()).
new_bindings() -> orddict:new().

-doc "Returns the list of bindings contained in the binding structure.".
-spec(bindings(BindingStruct :: binding_struct()) -> bindings()).
bindings(Bs) when is_map(Bs) -> maps:to_list(Bs);
bindings(Bs) when is_list(Bs) -> orddict:to_list(Bs).

-doc "Returns the binding of `Name` in `BindingStruct`.".
-spec(binding(Name, BindingStruct) -> {value, value()} | unbound when
      Name :: name(),
      BindingStruct :: binding_struct()).
binding(Name, Bs) when is_map(Bs) ->
    case maps:find(Name, Bs) of
        {ok,Val} -> {value,Val};
        error -> unbound
    end;
binding(Name, Bs) when is_list(Bs) ->
    case orddict:find(Name, Bs) of
	{ok,Val} -> {value,Val};
	error -> unbound
    end.

-doc """
Adds binding `Name=Value` to `BindingStruct`. Returns an updated binding
structure.
""".
-spec(add_binding(Name, Value, BindingStruct) -> binding_struct() when
      Name :: name(),
      Value :: value(),
      BindingStruct :: binding_struct()).
add_binding(Name, Val, Bs) when is_map(Bs) -> maps:put(Name, Val, Bs);
add_binding(Name, Val, Bs) when is_list(Bs) -> orddict:store(Name, Val, Bs).

-doc """
Removes the binding of `Name` in `BindingStruct`. Returns an updated binding
structure.
""".
-spec(del_binding(Name, BindingStruct) -> binding_struct() when
      Name :: name(),
      BindingStruct :: binding_struct()).
del_binding(Name, Bs) when is_map(Bs) -> maps:remove(Name, Bs);
del_binding(Name, Bs) when is_list(Bs) -> orddict:erase(Name, Bs).

zip_add_bindings(Bs1, Bs2) when is_map(Bs1), is_map(Bs2) ->
    zip_add_bindings_map(maps:keys(Bs1), Bs1, Bs2);
zip_add_bindings(Bs1, Bs2) when is_list(Bs1), is_list(Bs2) ->
    zip_add_bindings1(orddict:to_list(Bs1), Bs2).

zip_add_bindings_map([Key | Keys], Bs1, Bs2) ->
    case {Bs1, Bs2} of
        {#{Key := Same}, #{Key := Same}} -> zip_add_bindings_map(Keys, Bs1, Bs2);
        {_, #{Key := _}} ->
            #{strict_pats := StrictPats} = Bs2,
            case lists:member(Key, StrictPats) of
                true -> nomatch_strict;
                false -> nomatch
            end;
        {#{Key := Value},_} -> zip_add_bindings_map(Keys, Bs1, Bs2#{Key => Value})
    end;
zip_add_bindings_map([], _, Bs2) ->
    Bs2.

zip_add_bindings1([{Name,Val}|Bs1], Bs2) ->
    case orddict:find(Name, Bs2) of
        {ok, Val} ->
            zip_add_bindings1(Bs1, Bs2);
        {ok, _Value} ->
            {ok, StrictPats} = orddict:find(strict_pats, Bs2),
            case lists:member(Name, StrictPats) of
                true -> nomatch_strict;
                false -> nomatch
            end;
        error ->
            zip_add_bindings1(Bs1, orddict:store(Name, Val, Bs2))
    end;
zip_add_bindings1([], Bs2) ->
    Bs2.

add_bindings(Bs1, Bs2) when is_map(Bs1), is_map(Bs2) ->
    maps:merge(Bs2, Bs1);
add_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) -> orddict:store(Name, Val, Bs) end,
	  Bs2, orddict:to_list(Bs1)).

merge_bindings(Bs1, Bs2, Anno, Ef) when is_map(Bs1), is_map(Bs2) ->
    maps:merge_with(fun
	(_K, V, V) -> V;
	(_K, _, V) -> apply_error({badmatch,V}, ?STACKTRACE, Anno, Bs1, Ef, none)
    end, Bs2, Bs1);
merge_bindings(Bs1, Bs2, Anno, Ef) ->
    foldl(fun ({Name,Val}, Bs) ->
		  case orddict:find(Name, Bs) of
		      {ok,Val} -> Bs;		%Already with SAME value
		      {ok,V1} ->
			  apply_error({badmatch,V1}, ?STACKTRACE, Anno, Bs1, Ef, none);
		      error -> orddict:store(Name, Val, Bs)
		  end end,
	  Bs2, orddict:to_list(Bs1)).

-doc false.
-spec(new_bindings(binding_struct()) -> binding_struct()).
new_bindings(Bs) when is_map(Bs) -> maps:new();
new_bindings(Bs) when is_list(Bs) -> orddict:new().

filter_bindings(Fun, Bs) when is_map(Bs) -> maps:filter(Fun, Bs);
filter_bindings(Fun, Bs) when is_list(Bs) -> orddict:filter(Fun, Bs).

to_terms(Abstrs) ->
    [to_term(Abstr) || Abstr <- Abstrs].

to_term(Abstr) ->
    erl_parse:anno_to_term(Abstr).

%% `Tokens' is assumed to have been scanned with the 'text' option.
%% The annotations of the returned expressions are locations.
%%
%% Can handle pids, ports, references, and external funs ("items").
%% Known items are represented by variables in the erl_parse tree, and
%% the items themselves are stored in the returned bindings.

-doc false.
-spec extended_parse_exprs(Tokens) ->
                {'ok', ExprList} | {'error', ErrorInfo} when
      Tokens :: [erl_scan:token()],
      ExprList :: [erl_parse:abstract_expr()],
      ErrorInfo :: erl_parse:error_info().

extended_parse_exprs(Tokens) ->
    Ts = tokens_fixup(Tokens),
    case erl_parse:parse_exprs(Ts) of
        {ok, Exprs0} ->
            Exprs = expr_fixup(Exprs0),
            {ok, reset_expr_anno(Exprs)};
        _ErrorInfo ->
            erl_parse:parse_exprs(reset_token_anno(Ts))
    end.

tokens_fixup([]) -> [];
tokens_fixup([T|Ts]=Ts0) ->
    try token_fixup(Ts0) of
        {NewT, NewTs} ->
            [NewT|tokens_fixup(NewTs)]
    catch
        _:_ ->
            [T|tokens_fixup(Ts)]
    end.

token_fixup(Ts) ->
    {AnnoL, NewTs, FixupTag} = unscannable(Ts),
    String = lists:append([erl_anno:text(A) || A <- AnnoL]),
    _ = validate_tag(FixupTag, String),
    NewAnno = erl_anno:set_text(fixup_text(FixupTag), hd(AnnoL)),
    {{string, NewAnno, String}, NewTs}.

unscannable([{'#', A1}, {var, A2, 'Fun'}, {'<', A3}, {atom, A4, _},
             {'.', A5}, {float, A6, _}, {'>', A7}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7], Ts, function};
unscannable([{'#', A1}, {var, A2, 'Fun'}, {'<', A3}, {atom, A4, _},
             {'.', A5}, {atom, A6, _}, {'.', A7}, {integer, A8, _},
             {'>', A9}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7, A8, A9], Ts, function};
unscannable([{'<', A1}, {float, A2, _}, {'.', A3}, {integer, A4, _},
             {'>', A5}|Ts]) ->
    {[A1, A2, A3, A4, A5], Ts, pid};
unscannable([{'#', A1}, {var, A2, 'Port'}, {'<', A3}, {float, A4, _},
             {'>', A5}|Ts]) ->
    {[A1, A2, A3, A4, A5], Ts, port};
unscannable([{'#', A1}, {var, A2, 'Ref'}, {'<', A3}, {float, A4, _},
             {'.', A5}, {float, A6, _}, {'>', A7}|Ts]) ->
    {[A1, A2, A3, A4, A5, A6, A7], Ts, reference}.

expr_fixup({string,A,S}=T) ->
    try string_fixup(A, S, T) of
        Expr -> Expr
    catch
        _:_ -> T
    end;
expr_fixup(Tuple) when is_tuple(Tuple) ->
    L = expr_fixup(tuple_to_list(Tuple)),
    list_to_tuple(L);
expr_fixup([E0|Es0]) ->
    E = expr_fixup(E0),
    Es = expr_fixup(Es0),
    [E|Es];
expr_fixup(T) ->
    T.

string_fixup(Anno, String, Token) ->
    Text = erl_anno:text(Anno),
    FixupTag = fixup_tag(Text, String),
    fixup_ast(FixupTag, Anno, String, Token).

reset_token_anno(Tokens) ->
    [setelement(2, T, (reset_anno())(element(2, T))) || T <- Tokens].

reset_expr_anno(Exprs) ->
    [erl_parse:map_anno(reset_anno(), E) || E <- Exprs].

reset_anno() ->
    fun(A) -> erl_anno:new(erl_anno:location(A)) end.

fixup_ast(pid, A, _S, T) ->
    {call,A,{remote,A,{atom,A,erlang},{atom,A,list_to_pid}},[T]};
fixup_ast(port, A, _S, T) ->
    {call,A,{remote,A,{atom,A,erlang},{atom,A,list_to_port}},[T]};
fixup_ast(reference, A, _S, T) ->
    {call,A,{remote,A,{atom,A,erlang},{atom,A,list_to_ref}},[T]};
fixup_ast(function, A, S, _T) ->
    {Module, Function, Arity} = fixup_mfa(S),
    {'fun',A,{function,{atom,A,Module},{atom,A,Function},{integer,A,Arity}}}.

fixup_text(function)  -> "function";
fixup_text(pid)       -> "pid";
fixup_text(port)      -> "port";
fixup_text(reference) -> "reference".

fixup_tag("function",  "#"++_) -> function;
fixup_tag("pid",       "<"++_) -> pid;
fixup_tag("port",      "#"++_) -> port;
fixup_tag("reference", "#"++_) -> reference.

fixup_mfa(S) ->
    {ok, [_, _, _,
          {atom, _, Module}, _,
          {atom, _, Function}, _,
          {integer, _, Arity}|_], _} = erl_scan:string(S),
    {Module, Function, Arity}.

validate_tag(pid, String) -> erlang:list_to_pid(String);
validate_tag(port, String) -> erlang:list_to_port(String);
validate_tag(reference, String) -> erlang:list_to_ref(String);
validate_tag(function, String) ->
    {Module, Function, Arity} = fixup_mfa(String),
    erlang:make_fun(Module, Function, Arity).

%%% End of extended_parse_exprs.

%% `Tokens' is assumed to have been scanned with the 'text' option.
%%
%% Can handle pids, ports, references, and external funs.

-doc false.
-spec extended_parse_term(Tokens) ->
                {'ok', Term} | {'error', ErrorInfo} when
      Tokens :: [erl_scan:token()],
      Term :: term(),
      ErrorInfo :: erl_parse:error_info().

extended_parse_term(Tokens) ->
    case extended_parse_exprs(Tokens) of
        {ok, [Expr]} ->
            try normalise(Expr) of
                Term ->
                    {ok, Term}
            catch
                _:_ ->
                    Loc = erl_anno:location(element(2, Expr)),
                    {error,{Loc,?MODULE,"bad term"}}
            end;
        {ok, [_,Expr|_]} ->
                Loc = erl_anno:location(element(2, Expr)),
                {error,{Loc,?MODULE,"bad term"}};
        {error, _} = Error ->
            Error
    end.

%% From erl_parse.
normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
normalise({map,_,Pairs}) ->
    maps:from_list(lists:map(fun
		%% only allow '=>'
		({map_field_assoc,_,K,V}) ->
                                     {normalise(K),normalise(V)}
	    end, Pairs));
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;   %Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
%% Special case for #...<>
normalise({call,_,{remote,_,{atom,_,erlang},{atom,_,Fun}},[{string,_,S}]}) when
        Fun =:= list_to_ref; Fun =:= list_to_port; Fun =:= list_to_pid ->
    erlang:Fun(S);
normalise({'fun',_,{function,{atom,_,M},{atom,_,F},{integer,_,A}}}) ->
    %% Since "#Fun<M.F.A>" is recognized, "fun M:F/A" should be too.
    fun M:F/A.

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

%%----------------------------------------------------------------------------
%%
%% Evaluate expressions:
%% constants and
%% op A
%% L op R
%% Things that evaluate to constants are accepted
%% and guard_bifs are allowed in constant expressions
%%----------------------------------------------------------------------------

-doc false.
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

-doc false.
partial_eval(Expr) ->
    Anno = anno(Expr),
    case catch ev_expr(Expr) of
	X when is_integer(X) -> ret_expr(Expr,{integer,Anno,X});
	X when is_float(X) -> ret_expr(Expr,{float,Anno,X});
	X when is_atom(X) -> ret_expr(Expr,{atom,Anno,X});
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
%%ev_expr({call,Anno,{atom,_,F},As}) ->
%%    true = erl_internal:guard_bif(F, length(As)),
%%    apply(erlang, F, [ev_expr(X) || X <- As]);
%%ev_expr({call,Anno,{remote,_,{atom,_,erlang},{atom,_,F}},As}) ->
%%    true = erl_internal:guard_bif(F, length(As)),
%%    apply(erlang, F, [ev_expr(X) || X <- As]);

%% eval_str(InStr) -> {ok, OutStr} | {error, ErrStr'}
%%   InStr must represent a body
%%   Note: If InStr is a binary it has to be a Latin-1 string.
%%   If you have a UTF-8 encoded binary you have to call
%%   unicode:characters_to_list/1 before the call to eval_str().

-define(result(F,D), lists:flatten(io_lib:format(F, D))).

-doc false.
-spec eval_str(string() | unicode:latin1_binary()) ->
                      {'ok', string()} | {'error', string()}.

eval_str(Str) when is_list(Str) ->
    case erl_scan:tokens([], Str, 0) of
	{more, _} ->
	    {error, "Incomplete form (missing .<cr>)??"};
	{done, {ok, Toks, _}, Rest} ->
	    case all_white(Rest) of
		true ->
		    case erl_parse:parse_exprs(Toks) of
			{ok, Exprs} ->
			    case catch erl_eval:exprs(Exprs, erl_eval:new_bindings()) of
				{value, Val, _} ->
				    {ok, Val};
				Other ->
				    {error, ?result("*** eval: ~p", [Other])}
			    end;
			{error, {_Location, Mod, Args}} ->
                            Msg = ?result("*** ~ts",[Mod:format_error(Args)]),
                            {error, Msg}
		    end;
		false ->
		    {error, ?result("Non-white space found after "
				    "end-of-form :~ts", [Rest])}
		end
    end;
eval_str(Bin) when is_binary(Bin) ->
    eval_str(binary_to_list(Bin)).

all_white([$\s|T]) -> all_white(T);
all_white([$\n|T]) -> all_white(T);
all_white([$\t|T]) -> all_white(T);
all_white([])      -> true;
all_white(_)       -> false.

ret_expr(_Old, New) ->
    %%    io:format("~w: reduced ~s => ~s~n",
    %%	      [line(Old), erl_pp:expr(Old), erl_pp:expr(New)]),
    New.

anno(Expr) -> element(2, Expr).
