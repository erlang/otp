%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose : Do necessary checking of Core Erlang code.

%% Check Core module for errors.  Seeing this module is used in the
%% compiler after optimisations we do more checking than would be
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
%% Consistency of values/variables
%% Consistency of function return values/calls.
%%
%% We keep the names defined variables and functions in a ordered list
%% of variable names and function name/arity pairs.

-module(core_lint).

-export([module/1,module/2,format_error/1]).

-import(lists, [reverse/1,all/2,foldl/3]).
-import(ordsets, [add_element/2,is_element/2,union/2]).

-include("core_parse.hrl").

%%-----------------------------------------------------------------------
%% Types used in this module

-type fa()       :: {atom(), arity()}.

-type err_desc() :: 'invalid_attributes' | 'invalid_exports'
                  | {'arg_mismatch', fa()} | {'bittype_unit', fa()}
                  | {'illegal_expr', fa()} | {'illegal_guard', fa()}
                  | {'illegal_pattern', fa()} | {'illegal_try', fa()}
                  | {'not_bs_pattern', fa()} | {'not_pattern', fa()}
                  | {'not_var', fa()} | {'pattern_mismatch', fa()}
                  | {'return_mismatch', fa()} | {'undefined_function', fa()}
                  | {'duplicate_var', cerl:var_name(), fa()}
                  | {'unbound_var', cerl:var_name(), fa()}
                  | {'undefined_function', fa(), fa()}
                  | {'tail_segment_not_at_end', fa()}.

-type error()    :: {'none', module(), err_desc()}.
-type warning()  :: {module(), term()}.

%%-----------------------------------------------------------------------
%% Define the lint state record.

-record(lint, {module       :: module(),		% Current module
	       func         :: fa() | 'undefined',	% Current function
	       errors  = [] :: [error()],		% Errors
	       warnings= [] :: [warning()]}).		% Warnings

%%----------------------------------------------------------------------

%% format_error(Error)
%%  Return a string describing the error.

-spec format_error(err_desc()) -> [char() | list()].

format_error(invalid_attributes) -> "invalid attributes";
format_error(invalid_exports) -> "invalid exports";
format_error({arg_mismatch,{F,A}}) ->
    io_lib:format("argument count mismatch in ~w/~w", [F,A]);
format_error({bittype_unit,{F,A}}) ->
    io_lib:format("unit without size in bit syntax pattern/expression in ~w/~w", [F,A]);
format_error({illegal_expr,{F,A}}) ->
    io_lib:format("illegal expression in ~w/~w", [F,A]);
format_error({illegal_guard,{F,A}}) ->
    io_lib:format("illegal guard expression in ~w/~w", [F,A]);
format_error({illegal_pattern,{F,A}}) ->
    io_lib:format("illegal pattern in ~w/~w", [F,A]);
format_error({illegal_try,{F,A}}) ->
    io_lib:format("illegal try expression in ~w/~w", [F,A]);
format_error({not_bs_pattern,{F,A}}) ->
    io_lib:format("expecting bit syntax pattern in ~w/~w", [F,A]);
format_error({not_pattern,{F,A}}) ->
    io_lib:format("expecting pattern in ~w/~w", [F,A]);
format_error({not_var,{F,A}}) ->
    io_lib:format("expecting variable in ~w/~w", [F,A]);
format_error({pattern_mismatch,{F,A}}) ->
    io_lib:format("pattern count mismatch in ~w/~w", [F,A]);
format_error({return_mismatch,{F,A}}) ->
    io_lib:format("return count mismatch in ~w/~w", [F,A]);
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({duplicate_var,N,{F,A}}) ->
    io_lib:format("duplicate variable ~s in ~w/~w", [N,F,A]);
format_error({unbound_var,N,{F,A}}) ->
    io_lib:format("unbound variable ~s in ~w/~w", [N,F,A]);
format_error({undefined_function,{F1,A1},{F2,A2}}) ->
    io_lib:format("undefined function ~w/~w in ~w/~w", [F1,A1,F2,A2]);
format_error({tail_segment_not_at_end,{F,A}}) ->
    io_lib:format("binary tail segment not at end in ~w/~w", [F,A]).

-type ret() :: {'ok', [{module(), [warning(),...]}]}
             | {'error', [{module(), [error(),...]}],
		         [{module(), [warning(),...]}]}.

-spec module(cerl:c_module()) -> ret().
         
module(M) -> module(M, []).

-spec module(cerl:c_module(), [compile:option()]) -> ret().

module(#c_module{name=M,exports=Es,attrs=As,defs=Ds}, _Opts) ->
    Defined = defined_funcs(Ds),
    St0 = #lint{module=M#c_literal.val},
    St1 = check_exports(Es, St0),
    St2 = check_attrs(As, St1),
    St3 = module_defs(Ds, Defined, St2),
    St4 = check_state(Es, Defined, St3),
    return_status(St4).

%% defined_funcs([FuncDef]) -> [Fname].

defined_funcs(Fs) ->
    foldl(fun ({#c_var{name={_I,_A}=IA},_}, Def) ->
		  add_element(IA, Def)
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

add_error(E, St) -> St#lint{errors=[{none,?MODULE,E}|St#lint.errors]}.

%%add_warning(W, St) -> St#lint{warnings=[{none,core_lint,W}|St#lint.warnings]}.

check_exports(Es, St) ->
    case all(fun (#c_var{name={Name,Arity}})
		 when is_atom(Name), is_integer(Arity) -> true;
		 (_) -> false
	     end, Es) of
	true -> St;
	false -> add_error(invalid_exports, St)
    end.

check_attrs(As, St) ->
    case all(fun ({#c_literal{},#c_literal{}}) -> true;
		 (_) -> false
	     end, As) of
	true -> St;
	false -> add_error(invalid_attributes, St)
    end.

check_state(Es, Defined, St) ->
    foldl(fun (#c_var{name={_N,_A}=F}, St1) ->
		  case is_element(F, Defined) of
		      true -> St1;
		      false -> add_error({undefined_function,F}, St)
		  end
	  end, St, Es).

%% module_defs(CoreBody, Defined, State) -> State.

module_defs(B, Def, St) ->
    %% Set top level function name.
    foldl(fun (Func, St0) ->
		  {#c_var{name={_F,_A}=FA},_} = Func,
		  St1 = St0#lint{func=FA},
		  function(Func, Def, St1)
	  end, St, B).

%% functions([Fdef], Defined, State) -> State.

functions(Fs, Def, St0) ->
    foldl(fun (F, St) -> function(F, Def, St) end, St0, Fs).

%% function(CoreFunc, Defined, State) -> State.

function({#c_var{name={_,_}},B}, Def, St) ->
    %% Body must be a fun!
    case B of
	#c_fun{} -> expr(B, Def, 1, St);
	_ -> add_error({illegal_expr,St#lint.func}, St)
    end.

%% body(Expr, Defined, RetCount, State) -> State.

body(#c_values{es=Es}, Def, Rt, St) ->
    return_match(Rt, length(Es), expr_list(Es, Def, St));
body(E, Def, Rt, St0) ->
    St1 = expr(E, Def, Rt, St0),
    case is_simple_top(E) of
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
    case is_simple_top(E) of
	true -> return_match(Rt, 1, St1);
	false -> St1
    end.

gexpr(#c_var{name=N}, Def, Rt, St) when is_atom(N); is_integer(N) ->
    return_match(Rt, 1, expr_var(N, Def, St));
gexpr(#c_literal{}, _Def, Rt, St) ->
    return_match(Rt, 1, St);
gexpr(#c_cons{hd=H,tl=T}, Def, Rt, St) ->
    return_match(Rt, 1, gexpr_list([H,T], Def, St));
gexpr(#c_tuple{es=Es}, Def, Rt, St) ->
    return_match(Rt, 1, gexpr_list(Es, Def, St));
gexpr(#c_map{es=Es}, Def, Rt, St) ->
    return_match(Rt, 1, gexpr_list(Es, Def, St));
gexpr(#c_map_pair{key=K,val=V}, Def, Rt, St) ->
    return_match(Rt, 1, gexpr_list([K,V], Def, St));
gexpr(#c_binary{segments=Ss}, Def, Rt, St) ->
    return_match(Rt, 1, gbitstr_list(Ss, Def, St));
gexpr(#c_seq{arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = gexpr(Arg, Def, 1, St0),
    return_match(Rt, 1, gbody(B, Def, Rt, St1));
gexpr(#c_let{vars=Vs,arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = gbody(Arg, Def, let_varcount(Vs), St0), %This is a guard body
    {Lvs,St2} = variable_list(Vs, St1),
    gbody(B, union(Lvs, Def), Rt, St2);
gexpr(#c_call{module=#c_literal{val=erlang},name=#c_literal{val=is_record},
              args=[Arg,#c_literal{val=Tag},#c_literal{val=Size}]},
      Def, Rt, St) when is_atom(Tag), is_integer(Size) ->
    return_match(Rt, 1, gexpr(Arg, Def, 1, St));
gexpr(#c_call{module=#c_literal{val=erlang},name=#c_literal{val=is_record}},
      _Def, Rt, St) ->
    return_match(Rt, 1, add_error({illegal_guard,St#lint.func}, St));
gexpr(#c_call{module=#c_literal{val=erlang},name=#c_literal{val=Name},args=As},
      Def, Rt, St0) when is_atom(Name) ->
    St1 = return_match(Rt, 1, St0),
    case is_guard_bif(Name, length(As)) of
        true ->
            gexpr_list(As, Def, St1);
        false ->
            add_error({illegal_guard,St1#lint.func}, St1)
    end;
gexpr(#c_primop{name=#c_literal{val=A},args=As}, Def, _Rt, St0) when is_atom(A) ->
    gexpr_list(As, Def, St0);
gexpr(#c_try{arg=E,vars=[#c_var{name=X}],body=#c_var{name=X},
	     evars=[#c_var{},#c_var{}],handler=#c_literal{val=false}},
      Def, Rt, St) ->
    gbody(E, Def, Rt, St);
gexpr(#c_case{arg=Arg,clauses=Cs}, Def, Rt, St0) ->
    PatCount = case_patcount(Cs),
    St1 = gbody(Arg, Def, PatCount, St0),
    clauses(Cs, Def, PatCount, Rt, St1);
gexpr(_Core, _, _, St) ->
    %%io:fwrite("clint gexpr: ~p~n", [_Core]),
    add_error({illegal_guard,St#lint.func}, St).

%% gexpr_list([Expr], Defined, State) -> State.

gexpr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> gexpr(E, Def, 1, St) end, St0, Es).

%% gbitstr_list([Elem], Defined, State) -> State.

gbitstr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> gbitstr(E, Def, St) end, St0, Es).

gbitstr(#c_bitstr{val=V,size=S}, Def, St) ->
    gexpr_list([V,S], Def, St).

%% is_guard_bif(Name, Arity) -> Boolean.

is_guard_bif(Name, Arity) ->
    erl_internal:guard_bif(Name, Arity)
        orelse erl_internal:arith_op(Name, Arity)
        orelse erl_internal:bool_op(Name, Arity)
        orelse erl_internal:comp_op(Name, Arity).

%% expr(Expr, Defined, RetCount, State) -> State.

expr(#c_var{name={_,_}=FA}, Def, Rt, St) ->
    return_match(Rt, 1, expr_fname(FA, Def, St));
expr(#c_var{name=N}, Def, Rt, St) ->
    return_match(Rt, 1, expr_var(N, Def, St));
expr(#c_literal{}, _Def, Rt, St) ->
    return_match(Rt, 1, St);
expr(#c_cons{hd=H,tl=T}, Def, Rt, St) ->
    return_match(Rt, 1, expr_list([H,T], Def, St));
expr(#c_tuple{es=Es}, Def, Rt, St) ->
    return_match(Rt, 1, expr_list(Es, Def, St));
expr(#c_map{es=Es}, Def, Rt, St) ->
    return_match(Rt, 1, expr_list(Es, Def, St));
expr(#c_map_pair{key=K,val=V}, Def, Rt, St) ->
    return_match(Rt, 1, expr_list([K,V], Def, St));
expr(#c_binary{segments=Ss}, Def, Rt, St) ->
    return_match(Rt, 1, bitstr_list(Ss, Def, St));
expr(#c_fun{vars=Vs,body=B}, Def, Rt, St0) ->
    {Vvs,St1} = variable_list(Vs, St0),
    return_match(Rt, 1, body(B, union(Vvs, Def), 1, St1));
expr(#c_seq{arg=Arg,body=B}, Def, Rt, St0) ->
    St1 = expr(Arg, Def, 1, St0),
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
expr(#c_apply{op=Op,args=As}, Def, Rt, St0) ->
    St1 = apply_op(Op, Def, length(As), St0),
    return_match(Rt, 1, expr_list(As, Def, St1));
expr(#c_call{module=#c_literal{val=erlang},name=#c_literal{val=Name},args=As},
     Def, Rt, St0) when is_atom(Name) ->
    St1 = expr_list(As, Def, St0),
    case erl_bifs:is_exit_bif(erlang, Name, length(As)) of
        true -> St1;
        false -> return_match(Rt, 1, St1)
    end;
expr(#c_call{module=M,name=N,args=As}, Def, _Rt, St0) ->
    St1 = expr(M, Def, 1, St0),
    St2 = expr(N, Def, 1, St1),
    expr_list(As, Def, St2);
expr(#c_primop{name=#c_literal{val=A},args=As}, Def, Rt, St0) when is_atom(A) ->
    St1 = expr_list(As, Def, St0),
    case A of
        match_fail -> St1;
        _ -> return_match(Rt, 1, St1)
    end;
expr(#c_catch{body=B}, Def, Rt, St) ->
    return_match(Rt, 1, body(B, Def, 1, St));
expr(#c_try{arg=A,vars=Vs,body=B,evars=Evs,handler=H}, Def, Rt, St0) ->
    St1 = case Evs of
	      [_, _, _] -> St0;
	      _ -> add_error({illegal_try,St0#lint.func}, St0)
	  end,
    St2 = body(A, Def, let_varcount(Vs), St1),
    {Ns,St3} = variable_list(Vs, St2),
    St4 = body(B, union(Ns, Def), Rt, St3),
    {Ens,St5} = variable_list(Evs, St4),
    body(H, union(Ens, Def), Rt, St5);
expr(_Other, _, _, St) ->
    %%io:fwrite("clint expr: ~p~n", [_Other]),
    add_error({illegal_expr,St#lint.func}, St).

%% expr_list([Expr], Defined, State) -> State.

expr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> expr(E, Def, 1, St) end, St0, Es).

%% bitstr_list([Elem], Defined, State) -> State.

bitstr_list(Es, Def, St0) ->
    foldl(fun (E, St) -> bitstr(E, Def, St) end, St0, Es).

bitstr(#c_bitstr{val=V,size=S}, Def, St) ->
    expr_list([V,S], Def, St).

%% apply_op(Op, Defined, ArgCount, State) -> State.
%%  A apply op is either an fname or an expression.

apply_op(#c_var{name={_I,A}=IA}, Def, Ac, St0) ->
    St1 = expr_fname(IA, Def, St0),
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
%%  input variables which create no bindings.  We, therefore, need to
%%  carry around the original defined variables to get the correct
%%  handling.

%% pattern(P, Def, St) -> pattern(P, Def, [], St).

pattern(#c_var{name=N}, Def, Ps, St) ->
    pat_var(N, Def, Ps, St);
pattern(#c_literal{}, _Def, Ps, St) -> {Ps,St};
pattern(#c_cons{hd=H,tl=T}, Def, Ps, St) ->
    pattern_list([H,T], Def, Ps, St);
pattern(#c_tuple{es=Es}, Def, Ps, St) ->
    pattern_list(Es, Def, Ps, St);
pattern(#c_map{es=Es}, Def, Ps, St) ->
    pattern_list(Es, Def, Ps, St);
pattern(#c_map_pair{op=#c_literal{val=exact},key=K,val=V}, Def, Ps, St) ->
    %% The key is an input.
    pat_map_expr(K, Def, St),
    pattern_list([V],Def,Ps,St);
pattern(#c_binary{segments=Ss}, Def, Ps, St0) ->
    St = pat_bin_tail_check(Ss, St0),
    pat_bin(Ss, Def, Ps, St);
pattern(#c_alias{var=V,pat=P}, Def, Ps, St0) ->
    {Vvs,St1} = variable(V, Ps, St0),
    pattern(P, Def, union(Vvs, Ps), St1);
pattern(_Other, _, Ps, St) ->
    %%io:fwrite("clint pattern: ~p~n", [_Other]),
    {Ps,add_error({not_pattern,St#lint.func}, St)}.

pat_var(N, _Def, Ps, St) ->
    case is_element(N, Ps) of
	true -> {Ps,add_error({duplicate_var,N,St#lint.func}, St)};
	false -> {add_element(N, Ps),St}
    end.

%% pat_bin_list([Elem], Defined, [PatVar], State) -> {[PatVar],State}.

pat_bin(Es, Def0, Ps0, St0) ->
    {Ps,_,St} = foldl(fun (E, {Ps,Def,St}) ->
			      pat_segment(E, Def, Ps, St)
		      end, {Ps0,Def0,St0}, Es),
    {Ps,St}.

pat_segment(#c_bitstr{val=V,size=S,type=T}, Def0, Ps0, St0) ->
    St1 = pat_bit_expr(S, T, Def0, St0),
    {Ps,St2} = pattern(V, Def0, Ps0, St1),
    Def = case V of
	      #c_var{name=Name} -> add_element(Name, Def0);
	      _ -> Def0
	  end,
    {Ps,Def,St2};
pat_segment(_, Def, Ps, St) ->
    {Ps,Def,add_error({not_bs_pattern,St#lint.func}, St)}.

%% pat_bin_tail_check([Elem], State) -> State.
%%  There must be at most one tail segment (a size-less segment of
%%  type binary) and it must occur at the end.

pat_bin_tail_check([#c_bitstr{size=#c_literal{val=all}}], St) ->
    %% Size-less field is OK at the end of the list of segments.
    St;
pat_bin_tail_check([#c_bitstr{size=#c_literal{val=all}}|_], St) ->
    add_error({tail_segment_not_at_end,St#lint.func}, St);
pat_bin_tail_check([_|Ss], St) ->
    pat_bin_tail_check(Ss, St);
pat_bin_tail_check([], St) -> St.

%% pat_bit_expr(SizePat, Type, Defined, State) -> State.
%%  Check the Size pattern, this is an input!  Because of optimizations,
%%  we must allow any kind of constant and literal here.

pat_bit_expr(#c_var{name=N}, _, Def, St) -> expr_var(N, Def, St);
pat_bit_expr(#c_literal{}, _, _, St) -> St;
pat_bit_expr(#c_binary{}, _, _Def, St) ->
    %% Literal binaries may be expressed as a bit syntax construction
    %% expression if such expression is more compact than the literal.
    %% Example: <<0:100000000>>
    St;
pat_bit_expr(_, _, _, St) ->
    add_error({illegal_expr,St#lint.func}, St).

pat_map_expr(#c_var{name=N}, Def, St) -> expr_var(N, Def, St);
pat_map_expr(#c_literal{}, _Def, St) -> St;
pat_map_expr(_, _, St) -> add_error({illegal_expr,St#lint.func}, St).

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
return_match(N, N, St) -> St;
return_match(_Req, _Sup, St) ->
    add_error({return_mismatch,St#lint.func}, St).

%% arg_match(Required, Supplied, State) -> State.

arg_match(N, N, St) -> St;
arg_match(_Req, _Sup, St) ->
    add_error({arg_mismatch,St#lint.func}, St).

%% Only check if the top-level is a simple.
-spec is_simple_top(cerl:cerl()) -> boolean().

is_simple_top(#c_var{}) -> true;
is_simple_top(#c_cons{}) -> true;
is_simple_top(#c_tuple{}) -> true;
is_simple_top(#c_binary{}) -> true;
is_simple_top(#c_literal{}) -> true;
is_simple_top(_) -> false.
