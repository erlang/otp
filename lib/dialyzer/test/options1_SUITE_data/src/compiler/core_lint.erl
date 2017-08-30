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
%%     $Id: core_lint.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%% Purpose : Do necessary checking of Core Erlang code.

%% Check Core module for errors.  Seeing this module is used in the
%% compiler after optimisations wedone more checking than would be
%% necessary after just parsing.  Don't check all constructs.
%%
%% We check the following:
%%
%% All referred functions, called and exported, are defined.
%% Format of export list.
%% Format of attributes
%% Used variables are defined.
%% Variables in let and funs.
%% Patterns case clauses.
%% Values only as multiple values/variables/patterns.
%% Return same number of values as requested
%% Correct number of arguments
%%
%% Checks to add:
%%
%% Consistency of values/variables
%% Consistency of function return values/calls.
%%
%% We keep the names defined variables and functions in a ordered list
%% of variable names and function name/arity pairs.

-module(core_lint).


-export([module/1,module/2,format_error/1]).

-import(lists, [reverse/1,all/2,foldl/3]).
-import(ordsets, [add_element/2,is_element/2,union/2]).
%-import(ordsets, [subtract/2]).

-include("core_parse.hrl").

%% Define the lint state record.

-record(lint, {module=[],			%Current module
	       func=[],				%Current function
	       errors=[],			%Errors
	       warnings=[]}).			%Warnings

%% Keep track of defined
-record(def, {vars=[],
	      funs=[]}).

%%-deftype retcount() -> any | unknown | int().

%% format_error(Error)
%%  Return a string describing the error.

format_error(invalid_exports) -> "invalid exports";
format_error(invalid_attributes) -> "invalid attributes";
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({undefined_function,{F1,A1},{F2,A2}}) ->
    io_lib:format("undefined function ~w/~w in ~w/~w", [F1,A1,F2,A2]);
format_error({illegal_expr,{F,A}}) ->
    io_lib:format("illegal expression in ~w/~w", [F,A]);
format_error({illegal_guard,{F,A}}) ->
    io_lib:format("illegal guard expression in ~w/~w", [F,A]);
format_error({illegal_pattern,{F,A}}) ->
    io_lib:format("illegal pattern in ~w/~w", [F,A]);
format_error({illegal_try,{F,A}}) ->
    io_lib:format("illegal try expression in ~w/~w", [F,A]);
format_error({pattern_mismatch,{F,A}}) ->
    io_lib:format("pattern count mismatch in ~w/~w", [F,A]);
format_error({return_mismatch,{F,A}}) ->
    io_lib:format("return count mismatch in ~w/~w", [F,A]);
format_error({arg_mismatch,{F,A}}) ->
    io_lib:format("argument count mismatch in ~w/~w", [F,A]);
format_error({unbound_var,N,{F,A}}) ->
    io_lib:format("unbound variable ~s in ~w/~w", [N,F,A]);
format_error({duplicate_var,N,{F,A}}) ->
    io_lib:format("duplicate variable ~s in ~w/~w", [N,F,A]);
format_error({not_var,{F,A}}) ->
    io_lib:format("expecting variable in ~w/~w", [F,A]);
format_error({not_pattern,{F,A}}) ->
    io_lib:format("expecting pattern in ~w/~w", [F,A]);
format_error({not_bs_pattern,{F,A}}) ->
    io_lib:format("expecting bit syntax pattern in ~w/~w", [F,A]).

%% module(CoreMod) ->
%% module(CoreMod, [CompileOption]) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}

module(M) -> module(M, []).

module(#c_module{name=M,exports=Es,attrs=As,defs=Ds}, _Opts) ->
    Defined = defined_funcs(Ds),
    St0 = #lint{module=M#c_atom.val},
    St1 = check_exports(Es, St0),
    St2 = check_attrs(As, St1),
    St3 = module_defs(Ds, Defined, St2),
    St4 = check_state(Es, Defined, St3),
    return_status(St4).

%% defined_funcs([FuncDef]) -> [Fname].

defined_funcs(Fs) ->
    foldl(fun (#c_def{name=#c_fname{id=I,arity=A}}, Def) ->
		  add_element({I,A}, Def)
	  end, [], Fs).

%% return_status(State) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = reverse(St#lint.warnings),
    case reverse(St#lint.errors) of
	[] -> {ok,[{St#lint.module,Ws}]};
	Es -> {error,[{St#lint.module,Es}],[{St#lint.module,Ws}]}
    end.

%% add_error(ErrorDescriptor, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'
%%  Note that we don't use line numbers here.

add_error(E, St) -> St#lint{errors=[{none,core_lint,E}|St#lint.errors]}.

%%add_warning(W, St) -> St#lint{warnings=[{none,core_lint,W}|St#lint.warnings]}.

check_exports(Es, St) ->
    case all(fun (#c_fname{id=Name,arity=Arity}) when
		       atom(Name), integer(Arity) -> true;
		 (_) -> false
	     end, Es) of
	true -> St;
	false -> add_error(invalid_exports, St)
    end.

check_attrs(As, St) ->
    case all(fun (#c_def{name=#c_atom{},val=V}) -> core_lib:is_literal(V);
		 (_) -> false
	     end, As) of
	true -> St;
	false -> add_error(invalid_attributes, St)
    end.

check_state(Es, Defined, St) ->
    foldl(fun (#c_fname{id=N,arity=A}, St1) ->
		  F = {N,A},
		  case is_element(F, Defined) of
		      true -> St1;
		      false -> add_error({undefined_function,F}, St)
		  end
	  end, St, Es).
%     Undef = subtract(Es, Defined),
%     St1 = foldl(fun (F, St) -> add_error({undefined_function,F}, St) end,
% 		St0, Undef),
%     St1.

%% module_defs(CoreBody, Defined, State) -> State.

module_defs(B, Def, St) ->
    %% Set top level function name.
    foldl(fun (Func, St0) ->
		  #c_fname{id=F,arity=A} = Func#c_def.name,
		  St1 = St0#lint{func={F,A}},
		  function(Func, Def, St1)
	  end, St, B).

%% functions([Fdef], Defined, State) -> State.

functions(Fs, Def, St0) ->
    foldl(fun (F, St) -> function(F, Def, St) end, St0, Fs).

%% function(CoreFunc, Defined, State) -> State.

function(#c_def{name=#c_fname{},val=B}, Def, St) ->
    %% Body must be a fun!
    case B of
	#c_fun{} -> expr(B, Def, any, St);
	_ -> add_error({illegal_expr,St#lint.func}, St)
    end.

%% body(Expr, Defined, RetCount, State) -> State.

body(#c_values{es=Es}, Def, Rt, St) ->
    return_match(Rt, length(Es), expr_list(Es, Def, St));
body(E, Def, Rt, St0) ->
    St1 = expr(E, Def, Rt, St0),
    case core_lib:is_simple_top(E) of
	true -> return_match(Rt, 1, St1);
	false -> St1
    end.

%% guard(Expr, Defined, State) -> State.
%%  Guards are boolean expressions with test wrapped in a protected.

guard(Expr, Def, St) -> gexpr(Expr, Def, 1, St).

%% guard_list([Expr], Defined, State) -> State.

%% guard_list(Es, Def, St0) ->
%%     foldl(fun (E, St) -> guard(E, Def, St) end, St0, Es).

%% gbody(Expr, Defined, RetCount, State) -> State.

gbody(#c_values{es=Es}, Def, Rt, St) ->
    return_match(Rt, length(Es), gexpr_list(Es, Def, St));
gbody(E, Def, Rt, St0) ->
    St1 = gexpr(E, Def, Rt, St0),
    case core_lib:is_simple_top(E) of
	true -> return_match(Rt, 1, St1);
	false -> St1
    end.

gexpr(#c_var{name=N}, Def, _Rt, St) -> expr_var(N, Def, St);
gexpr(#c_int{}, _Def, _Rt, St) -> St;
gexpr(#c_float{}, _Def, _Rt, St) -> St;
gexpr(#c_atom{}, _Def, _Rt, St) -> St;
gexpr(#c_char{}, _Def, _Rt, St) -> St;
gexpr(#c_string{}, _Def, _Rt, St) -> St;
gexpr(#c_nil{}, _Def, _Rt, St) -> St;
gexpr(#c_cons{hd=H,tl=T}, Def, _Rt, St) ->
    gexpr_list([H,T], Def, St);
gexpr(#c_tuple{es=Es}, Def, _Rt, St) ->
    gexpr_list(Es, Def, St);
gexpr(#c_binary{segments=Ss}, Def, _Rt, St) ->
    gbitstr_list(Ss, Def, St);
gexpr(#c_seq{arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = gexpr(Arg, Def, any, St0),		%Ignore values
    gbody(B, Def, Rt, St1);
gexpr(#c_let{vars=Vs,arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = gbody(Arg, Def, let_varcount(Vs), St0), %This is a guard body
    {Lvs,St2} = variable_list(Vs, St1),
    gbody(B, union(Lvs, Def), Rt, St2);
gexpr(#c_call{module=#c_atom{val=erlang},
	      name=#c_atom{},
	      args=As}, Def, 1, St) ->
    gexpr_list(As, Def, St);
gexpr(#c_primop{name=N,args=As}, Def, _Rt, St0) when record(N, c_atom) ->
    gexpr_list(As, Def, St0);
gexpr(#c_try{arg=E,vars=[#c_var{name=X}],body=#c_var{name=X},
	     evars=[#c_var{},#c_var{},#c_var{}],handler=#c_atom{val=false}},
      Def, Rt, St) ->
    gbody(E, Def, Rt, St);
gexpr(_, _, _, St) ->
    add_error({illegal_guard,St#lint.func}, St).

%% gexpr_list([Expr], Defined, State) -> State.

gexpr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> gexpr(E, Def, 1, St) end, St0, Es).

%% gbitstr_list([Elem], Defined, State) -> State.

gbitstr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> gbitstr(E, Def, St) end, St0, Es).

gbitstr(#c_bitstr{val=V,size=S,unit=U,type=T,flags=Fs}, Def, St0) ->
    St1 = bit_type(U, T, Fs, St0),
    gexpr_list([V,S], Def, St1).

%% expr(Expr, Defined, RetCount, State) -> State.

expr(#c_var{name=N}, Def, _Rt, St) -> expr_var(N, Def, St);
expr(#c_int{}, _Def, _Rt, St) -> St;
expr(#c_float{}, _Def, _Rt, St) -> St;
expr(#c_atom{}, _Def, _Rt, St) -> St;
expr(#c_char{}, _Def, _Rt, St) -> St;
expr(#c_string{}, _Def, _Rt, St) -> St;
expr(#c_nil{}, _Def, _Rt, St) -> St;
expr(#c_cons{hd=H,tl=T}, Def, _Rt, St) ->
    expr_list([H,T], Def, St);
expr(#c_tuple{es=Es}, Def, _Rt, St) ->
    expr_list(Es, Def, St);
expr(#c_binary{segments=Ss}, Def, _Rt, St) ->
    bitstr_list(Ss, Def, St);
expr(#c_fname{id=I,arity=A}, Def, _Rt, St) ->
    expr_fname({I,A}, Def, St);
expr(#c_fun{vars=Vs,body=B}, Def, Rt, St0) ->
    {Vvs,St1} = variable_list(Vs, St0),
    return_match(Rt, 1, body(B, union(Vvs, Def), any, St1));
expr(#c_seq{arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = expr(Arg, Def, any, St0),		%Ignore values
    body(B, Def, Rt, St1);
expr(#c_let{vars=Vs,arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = body(Arg, Def, let_varcount(Vs), St0), %This is a body
    {Lvs,St2} = variable_list(Vs, St1),
    body(B, union(Lvs, Def), Rt, St2);
expr(#c_letrec{defs=Fs,body=B}, Def0, Rt, St0) ->
    Def1 = union(defined_funcs(Fs), Def0),	%All defined stuff
    St1 = functions(Fs, Def1, St0),
    body(B, Def1, Rt, St1#lint{func=St0#lint.func});
expr(#c_case{arg=Arg,clauses=Cs}, Def, Rt, St0) ->
    Pc = case_patcount(Cs),
    St1 = body(Arg, Def, Pc, St0),
    clauses(Cs, Def, Pc, Rt, St1);
expr(#c_receive{clauses=Cs,timeout=T,action=A}, Def, Rt, St0) ->
    St1 = expr(T, Def, 1, St0),
    St2 = body(A, Def, Rt, St1),
    clauses(Cs, Def, 1, Rt, St2);
expr(#c_apply{op=Op,args=As}, Def, _Rt, St0) ->
    St1 = apply_op(Op, Def, length(As), St0),
    expr_list(As, Def, St1);
expr(#c_call{module=M,name=N,args=As}, Def, _Rt, St0) ->
    St1 = expr(M, Def, 1, St0),
    St2 = expr(N, Def, 1, St1),
    expr_list(As, Def, St2);
expr(#c_primop{name=N,args=As}, Def, _Rt, St0) when record(N, c_atom) ->
    expr_list(As, Def, St0);
expr(#c_catch{body=B}, Def, Rt, St) ->
    return_match(Rt, 1, body(B, Def, 1, St));
expr(#c_try{arg=A,vars=Vs,body=B,evars=Evs,handler=H}, Def, Rt, St0) ->
    St1 = case length(Evs) of
	      2 -> St0;
	      _ -> add_error({illegal_try,St0#lint.func}, St0)
	  end,
    St2 = body(A, Def, let_varcount(Vs), St1),
    {Ns,St3} = variable_list(Vs, St2),
    St4 = body(B, union(Ns, Def), Rt, St3),
    {Ens,St5} = variable_list(Evs, St4),
    body(H, union(Ens, Def), Rt, St5);
expr(_, _, _, St) ->
    %%io:fwrite("clint: ~p~n", [Other]),
    add_error({illegal_expr,St#lint.func}, St).

%% expr_list([Expr], Defined, State) -> State.

expr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> expr(E, Def, 1, St) end, St0, Es).

%% bitstr_list([Elem], Defined, State) -> State.

bitstr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> bitstr(E, Def, St) end, St0, Es).

bitstr(#c_bitstr{val=V,size=S,unit=U,type=T,flags=Fs}, Def, St0) ->
    St1 = bit_type(U, T, Fs, St0),
    expr_list([V,S], Def, St1).

%% apply_op(Op, Defined, ArgCount, State) -> State.
%%  A apply op is either an fname or an expression.

apply_op(#c_fname{id=I,arity=A}, Def, Ac, St0) ->
    St1 = expr_fname({I,A}, Def, St0),
    arg_match(Ac, A, St1);
apply_op(E, Def, _, St) -> expr(E, Def, 1, St).	%Hard to check

%% expr_var(VarName, Defined, State) -> State.

expr_var(N, Def, St) ->
    case is_element(N, Def) of
	true -> St;
	false -> add_error({unbound_var,N,St#lint.func}, St)
    end.

%% expr_fname(Fname, Defined, State) -> State.

expr_fname(Fname, Def, St) ->
    case is_element(Fname, Def) of
	true -> St;
	false -> add_error({undefined_function,Fname,St#lint.func}, St)
    end.

%% let_varcount([Var]) -> int().

let_varcount([]) -> any;			%Ignore values
let_varcount(Es) -> length(Es).

%% case_patcount([Clause]) -> int().

case_patcount([#c_clause{pats=Ps}|_]) -> length(Ps).

%% clauses([Clause], Defined, PatCount, RetCount, State) -> State.

clauses(Cs, Def, Pc, Rt, St0) ->
    foldl(fun (C, St) -> clause(C, Def, Pc, Rt, St) end, St0, Cs).

%% clause(Clause, Defined, PatCount, RetCount, State) -> State.

clause(#c_clause{pats=Ps,guard=G,body=B}, Def0, Pc, Rt, St0) ->
    St1 = pattern_match(Pc, length(Ps), St0),
    {Pvs,St2} = pattern_list(Ps, Def0, St1),
    Def1 = union(Pvs, Def0),
    St3 = guard(G, Def1, St2),
    body(B, Def1, Rt, St3).

%% variable(Var, [PatVar], State) -> {[VarName],State}.

variable(#c_var{name=N}, Ps, St) ->
    case is_element(N, Ps) of
	true -> {[],add_error({duplicate_var,N,St#lint.func}, St)};
	false -> {[N],St}
    end;
variable(_, Def, St) -> {Def,add_error({not_var,St#lint.func}, St)}.

%% variable_list([Var], State) -> {[Var],State}.
%% variable_list([Var], [PatVar], State) -> {[Var],State}.

variable_list(Vs, St) -> variable_list(Vs, [], St).

variable_list(Vs, Ps, St) ->
    foldl(fun (V, {Ps0,St0}) ->
		  {Vvs,St1} = variable(V, Ps0, St0),
		  {union(Vvs, Ps0),St1}
	  end, {Ps,St}, Vs).

%% pattern(Pattern, Defined, State) -> {[PatVar],State}.
%% pattern(Pattern, Defined, [PatVar], State) -> {[PatVar],State}.
%%  Patterns are complicated by sizes in binaries.  These are pure
%%  input variables which create no bindings.  We, therefor, need to
%%  carry around the original defined variables to get the correct
%%  handling.

%% pattern(P, Def, St) -> pattern(P, Def, [], St).

pattern(#c_var{name=N}, Def, Ps, St) ->
    pat_var(N, Def, Ps, St);
pattern(#c_int{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_float{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_atom{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_char{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_string{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_nil{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_cons{hd=H,tl=T}, Def, Ps, St) ->
    pattern_list([H,T], Def, Ps, St);
pattern(#c_tuple{es=Es}, Def, Ps, St) ->
    pattern_list(Es, Def, Ps, St);
pattern(#c_binary{segments=Ss}, Def, Ps, St) ->
    pat_bin(Ss, Def, Ps, St);
pattern(#c_alias{var=V,pat=P}, Def, Ps, St0) ->
    {Vvs,St1} = variable(V, Ps, St0),
    pattern(P, Def, union(Vvs, Ps), St1);
pattern(_, _, Ps, St) -> {Ps,add_error({not_pattern,St#lint.func}, St)}.

pat_var(N, _Def, Ps, St) ->
    case is_element(N, Ps) of
	true -> {Ps,add_error({duplicate_var,N,St#lint.func}, St)};
	false -> {add_element(N, Ps),St}
    end.

%% pat_bin_list([Elem], Defined, [PatVar], State) -> {[PatVar],State}.

pat_bin(Es, Def, Ps0, St0) ->
    foldl(fun (E, {Ps,St}) -> pat_segment(E, Def, Ps, St) end, {Ps0,St0}, Es).

pat_segment(#c_bitstr{val=V,size=S,unit=U,type=T,flags=Fs}, Def, Ps, St0) ->
    St1 = bit_type(U, T, Fs, St0),
    St2 = pat_bit_expr(S, T, Def, St1),
    pattern(V, Def, Ps, St2);
pat_segment(_, _, Ps, St) ->
    {Ps,add_error({not_bs_pattern,St#lint.func}, St)}.

%% pat_bit_expr(SizePat, Type, Defined, State) -> State.
%%  Check the Size pattern, this is an input!  Be a bit tough here.

pat_bit_expr(#c_int{val=I}, _, _, St) when I >= 0 -> St;
pat_bit_expr(#c_var{name=N}, _, Def, St) ->
    expr_var(N, Def, St);
pat_bit_expr(#c_atom{val=all}, binary, _Def, St) -> St;
pat_bit_expr(_, _, _, St) ->
    add_error({illegal_expr,St#lint.func}, St).

bit_type(Unit, Type, Flags, St) ->
    U = core_lib:literal_value(Unit),
    T = core_lib:literal_value(Type),
    Fs = core_lib:literal_value(Flags),
    case erl_bits:set_bit_type(default, [T,{unit,U}|Fs]) of
	{ok,_,_} -> St;
	{error,E} -> add_error({E,St#lint.func}, St)
    end.

%% pattern_list([Var], Defined, State) -> {[PatVar],State}.
%% pattern_list([Var], Defined, [PatVar], State) -> {[PatVar],State}.

pattern_list(Pats, Def, St) -> pattern_list(Pats, Def, [], St).

pattern_list(Pats, Def, Ps0, St0) ->
    foldl(fun (P, {Ps,St}) -> pattern(P, Def, Ps, St) end, {Ps0,St0}, Pats).

%% pattern_match(Required, Supplied, State) -> State.
%%  Check that the required number of patterns match the supplied.

pattern_match(N, N, St) -> St;
pattern_match(_Req, _Sup, St) ->
    add_error({pattern_mismatch,St#lint.func}, St).

%% return_match(Required, Supplied, State) -> State.
%%  Check that the required number of return values match the supplied.

return_match(any, _Sup, St) -> St;
return_match(_Req, unknown, St) -> St;
return_match(N, N, St) -> St;
return_match(_Req, _Sup, St) ->
    add_error({return_mismatch,St#lint.func}, St).

%% arg_match(Required, Supplied, State) -> State.

arg_match(_Req, unknown, St) -> St;
arg_match(N, N, St) -> St;
arg_match(_Req, _Sup, St) ->
    add_error({arg_mismatch,St#lint.func}, St).
