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
%%     $Id: core_lib.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
%% Purpose: Core Erlang abstract syntax functions.

-module(core_lib).

-export([get_anno/1,set_anno/2]).
-export([is_atomic/1,is_literal/1,is_literal_list/1,
	 is_simple/1,is_simple_list/1,is_simple_top/1]).
-export([literal_value/1,make_literal/1]).
-export([make_values/1]).
-export([map/2, fold/3, mapfold/3]).
-export([is_var_used/2]).

%% -compile([export_all]).

-include("core_parse.hrl").

%% get_anno(Core) -> Anno.
%% set_anno(Core, Anno) -> Core.
%%  Generic get/set annotation.

get_anno(C) -> element(2, C).
set_anno(C, A) -> setelement(2, C, A).

%% is_atomic(Expr) -> true | false.

is_atomic(#c_char{}) -> true;
is_atomic(#c_int{}) -> true;
is_atomic(#c_float{}) -> true;
is_atomic(#c_atom{}) -> true;
is_atomic(#c_string{}) -> true;
is_atomic(#c_nil{}) -> true;
is_atomic(#c_fname{}) -> true;
is_atomic(_) -> false.

%% is_literal(Expr) -> true | false.

is_literal(#c_cons{hd=H,tl=T}) ->
    case is_literal(H) of
	true -> is_literal(T);
	false -> false
    end;
is_literal(#c_tuple{es=Es}) -> is_literal_list(Es);
is_literal(#c_binary{segments=Es}) -> is_lit_bin(Es);
is_literal(E) -> is_atomic(E).

is_literal_list(Es) -> lists:all(fun is_literal/1, Es).

is_lit_bin(Es) ->
    lists:all(fun (#c_bitstr{val=E,size=S}) ->
		      is_literal(E) and is_literal(S)
	      end, Es).

%% is_simple(Expr) -> true | false.

is_simple(#c_var{}) -> true;
is_simple(#c_cons{hd=H,tl=T}) ->
    case is_simple(H) of
	true -> is_simple(T);
	false -> false
    end;
is_simple(#c_tuple{es=Es}) -> is_simple_list(Es);
is_simple(#c_binary{segments=Es}) -> is_simp_bin(Es);
is_simple(E) -> is_atomic(E).

is_simple_list(Es) -> lists:all(fun is_simple/1, Es).

is_simp_bin(Es) ->
    lists:all(fun (#c_bitstr{val=E,size=S}) ->
		      is_simple(E) and is_simple(S)
	      end, Es).

%% is_simple_top(Expr) -> true | false.
%%  Only check if the top-level is a simple.

is_simple_top(#c_var{}) -> true;
is_simple_top(#c_cons{}) -> true;
is_simple_top(#c_tuple{}) -> true;
is_simple_top(#c_binary{}) -> true;
is_simple_top(E) -> is_atomic(E).

%% literal_value(LitExpr) -> Value.
%%  Return the value of LitExpr.

literal_value(#c_char{val=C}) -> C;
literal_value(#c_int{val=I}) -> I;
literal_value(#c_float{val=F}) -> F;
literal_value(#c_atom{val=A}) -> A;
literal_value(#c_string{val=S}) -> S;
literal_value(#c_nil{}) -> [];
literal_value(#c_cons{hd=H,tl=T}) ->
    [literal_value(H)|literal_value(T)];
literal_value(#c_tuple{es=Es}) ->
    list_to_tuple(literal_value_list(Es)).

literal_value_list(Vals) -> lists:map(fun literal_value/1, Vals).

%% make_literal(Value) -> LitExpr.
%%  Make a literal expression from an Erlang value.

make_literal(I) when integer(I) -> #c_int{val=I};
make_literal(F) when float(F) -> #c_float{val=F};
make_literal(A) when atom(A) -> #c_atom{val=A};
make_literal([]) -> #c_nil{};
make_literal([H|T]) ->
    #c_cons{hd=make_literal(H),tl=make_literal(T)};
make_literal(T) when tuple(T) ->
    #c_tuple{es=make_literal_list(tuple_to_list(T))}.

make_literal_list(Vals) -> lists:map(fun make_literal/1, Vals).

%% make_values([CoreExpr] | CoreExpr) -> #c_values{} | CoreExpr.
%%  Make a suitable values structure, expr or values, depending on
%%  Expr.

make_values([E]) -> E;
make_values([H|_]=Es) -> #c_values{anno=get_anno(H),es=Es};
make_values([]) -> #c_values{es=[]};
make_values(E) -> E.

%% map(MapFun, CoreExpr) -> CoreExpr.
%%  This function traverses the core parse format, at each level
%%  applying the submited argument function, assumed to do the real
%%  work.
%%
%%  The "eager" style, where each component of a construct are
%%  descended to before the construct itself, admits that some
%%  companion functions (the F:s) may be made simpler, since it may be
%%  safely assumed that no lower illegal instanced will be
%%  created/uncovered by actions on the current level.

map(F, #c_tuple{es=Es}=R) ->
    F(R#c_tuple{es=map_list(F, Es)});
map(F, #c_cons{hd=Hd, tl=Tl}=R) ->
    F(R#c_cons{hd=map(F, Hd),
	       tl=map(F, Tl)});
map(F, #c_values{es=Es}=R) ->
    F(R#c_values{es=map_list(F, Es)});

map(F, #c_alias{var=Var, pat=Pat}=R) ->
    F(R#c_alias{var=map(F, Var),
		pat=map(F, Pat)});

map(F, #c_module{defs=Defs}=R) ->
    F(R#c_module{defs=map_list(F, Defs)});
map(F, #c_def{val=Val}=R) ->
    F(R#c_def{val=map(F, Val)});

map(F, #c_fun{vars=Vars, body=Body}=R) ->
    F(R#c_fun{vars=map_list(F, Vars),
	      body=map(F, Body)});
map(F, #c_let{vars=Vs, arg=Arg, body=Body}=R) ->
    F(R#c_let{vars=map_list(F, Vs),
	      arg=map(F, Arg),
	      body=map(F, Body)});
map(F, #c_letrec{defs=Fs,body=Body}=R) ->
    F(R#c_letrec{defs=map_list(F, Fs),
		 body=map(F, Body)});
map(F, #c_seq{arg=Arg, body=Body}=R) ->
    F(R#c_seq{arg=map(F, Arg),
	      body=map(F, Body)});
map(F, #c_case{arg=Arg, clauses=Clauses}=R) ->
    F(R#c_case{arg=map(F, Arg),
	       clauses=map_list(F, Clauses)});
map(F, #c_clause{pats=Ps, guard=Guard, body=Body}=R) ->
    F(R#c_clause{pats=map_list(F, Ps),
		 guard=map(F, Guard),
		 body=map(F, Body)});
map(F, #c_receive{clauses=Cls, timeout=Tout, action=Act}=R) ->
    F(R#c_receive{clauses=map_list(F, Cls),
		  timeout=map(F, Tout),
		  action=map(F, Act)});
map(F, #c_apply{op=Op,args=Args}=R) ->
    F(R#c_apply{op=map(F, Op),
		args=map_list(F, Args)});
map(F, #c_call{module=M,name=N,args=Args}=R) ->
    F(R#c_call{module=map(F, M),
	       name=map(F, N),
	       args=map_list(F, Args)});
map(F, #c_primop{name=N,args=Args}=R) ->
    F(R#c_primop{name=map(F, N),
		 args=map_list(F, Args)});
map(F, #c_try{arg=Expr,vars=Vars,body=Body,evars=Evars,handler=Handler}=R) ->
    F(R#c_try{arg=map(F, Expr),
	      vars=map(F, Vars),
	      body=map(F, Body),
	      evars=map(F, Evars),
	      handler=map(F, Handler)});
map(F, #c_catch{body=Body}=R) ->
    F(R#c_catch{body=map(F, Body)});
map(F, T) -> F(T).				%Atomic nodes.

map_list(F, L) -> lists:map(fun (E) -> map(F, E) end, L).

%% fold(FoldFun, Accumulator, CoreExpr) -> Accumulator.
%%  This function traverses the core parse format, at each level
%%  applying the submited argument function, assumed to do the real
%%  work, and keeping the accumulated result in the A (accumulator)
%%  argument.

fold(F, Acc, #c_tuple{es=Es}=R) ->
    F(R, fold_list(F, Acc, Es));
fold(F, Acc, #c_cons{hd=Hd, tl=Tl}=R) ->
    F(R, fold(F, fold(F, Acc, Hd), Tl));
fold(F, Acc, #c_values{es=Es}=R) ->
    F(R, fold_list(F, Acc, Es));

fold(F, Acc, #c_alias{pat=P,var=V}=R) ->
    F(R, fold(F, fold(F, Acc, P), V));

fold(F, Acc, #c_module{defs=Defs}=R) ->
    F(R, fold_list(F, Acc, Defs));
fold(F, Acc, #c_def{val=Val}=R) ->
    F(R, fold(F, Acc, Val));

fold(F, Acc, #c_fun{vars=Vars, body=Body}=R) ->
    F(R, fold(F, fold_list(F, Acc, Vars), Body));
fold(F, Acc, #c_let{vars=Vs, arg=Arg, body=Body}=R) ->
    F(R, fold(F, fold(F, fold_list(F, Acc, Vs), Arg), Body));
fold(F, Acc, #c_letrec{defs=Fs,body=Body}=R) ->
    F(R, fold(F, fold_list(F, Acc, Fs), Body));
fold(F, Acc, #c_seq{arg=Arg, body=Body}=R) ->
    F(R, fold(F, fold(F, Acc, Arg), Body));
fold(F, Acc, #c_case{arg=Arg, clauses=Clauses}=R) ->
    F(R, fold_list(F, fold(F, Acc, Arg), Clauses));
fold(F, Acc, #c_clause{pats=Ps,guard=G,body=B}=R) ->
    F(R, fold(F, fold(F, fold_list(F, Acc, Ps), G), B));
fold(F, Acc, #c_receive{clauses=Cl, timeout=Ti, action=Ac}=R) ->
    F(R, fold_list(F, fold(F, fold(F, Acc, Ac), Ti), Cl));
fold(F, Acc, #c_apply{op=Op, args=Args}=R) ->
    F(R, fold_list(F, fold(F, Acc, Op), Args));
fold(F, Acc, #c_call{module=Mod,name=Name,args=Args}=R) ->
    F(R, fold_list(F, fold(F, fold(F, Acc, Mod), Name), Args));
fold(F, Acc, #c_primop{name=Name,args=Args}=R) ->
    F(R, fold_list(F, fold(F, Acc, Name), Args));
fold(F, Acc, #c_try{arg=E,vars=Vs,body=Body,evars=Evs,handler=H}=R) ->
    NewB = fold(F, fold_list(F, fold(F, Acc, E), Vs), Body),
    F(R, fold(F, fold_list(F, NewB, Evs), H));
fold(F, Acc, #c_catch{body=Body}=R) ->
    F(R, fold(F, Acc, Body));
fold(F, Acc, T) ->				%Atomic nodes
    F(T, Acc).

fold_list(F, Acc, L) ->
    lists:foldl(fun (E, A) -> fold(F, A, E) end, Acc, L).

%% mapfold(MapfoldFun, Accumulator, CoreExpr) -> {CoreExpr,Accumulator}.
%%  This function traverses the core parse format, at each level
%%  applying the submited argument function, assumed to do the real
%%  work, and keeping the accumulated result in the A (accumulator)
%%  argument.

mapfold(F, Acc0, #c_tuple{es=Es0}=R) ->
    {Es1,Acc1} = mapfold_list(F, Acc0, Es0),
    F(R#c_tuple{es=Es1}, Acc1);
mapfold(F, Acc0, #c_cons{hd=H0,tl=T0}=R) ->
    {H1,Acc1} = mapfold(F, Acc0, H0),
    {T1,Acc2} = mapfold(F, Acc1, T0),
    F(R#c_cons{hd=H1,tl=T1}, Acc2);
mapfold(F, Acc0, #c_values{es=Es0}=R) ->
    {Es1,Acc1} = mapfold_list(F, Acc0, Es0),
    F(R#c_values{es=Es1}, Acc1);

mapfold(F, Acc0, #c_alias{pat=P0,var=V0}=R) ->
    {P1,Acc1} = mapfold(F, Acc0, P0),
    {V1,Acc2} = mapfold(F, Acc1, V0),
    F(R#c_alias{pat=P1,var=V1}, Acc2);

mapfold(F, Acc0, #c_module{defs=D0}=R) ->
    {D1,Acc1} = mapfold_list(F, Acc0, D0),
    F(R#c_module{defs=D1}, Acc1);
mapfold(F, Acc0, #c_def{val=V0}=R) ->
    {V1,Acc1} = mapfold(F, Acc0, V0),
    F(R#c_def{val=V1}, Acc1);

mapfold(F, Acc0, #c_fun{vars=Vs0, body=B0}=R) ->
    {Vs1,Acc1} = mapfold_list(F, Acc0, Vs0),
    {B1,Acc2} = mapfold(F, Acc1, B0),
    F(R#c_fun{vars=Vs1,body=B1}, Acc2);
mapfold(F, Acc0, #c_let{vars=Vs0, arg=A0, body=B0}=R) ->
    {Vs1,Acc1} = mapfold_list(F, Acc0, Vs0),
    {A1,Acc2} = mapfold(F, Acc1, A0),
    {B1,Acc3} = mapfold(F, Acc2, B0),
    F(R#c_let{vars=Vs1,arg=A1,body=B1}, Acc3);
mapfold(F, Acc0, #c_letrec{defs=Fs0,body=B0}=R) ->
    {Fs1,Acc1} = mapfold_list(F, Acc0, Fs0),
    {B1,Acc2} = mapfold(F, Acc1, B0),
    F(R#c_letrec{defs=Fs1,body=B1}, Acc2);
mapfold(F, Acc0, #c_seq{arg=A0, body=B0}=R) ->
    {A1,Acc1} = mapfold(F, Acc0, A0),
    {B1,Acc2} = mapfold(F, Acc1, B0),
    F(R#c_seq{arg=A1,body=B1}, Acc2);
mapfold(F, Acc0, #c_case{arg=A0,clauses=Cs0}=R) ->
    {A1,Acc1} = mapfold(F, Acc0, A0),
    {Cs1,Acc2} = mapfold_list(F, Acc1, Cs0),
    F(R#c_case{arg=A1,clauses=Cs1}, Acc2);
mapfold(F, Acc0, #c_clause{pats=Ps0,guard=G0,body=B0}=R) ->
    {Ps1,Acc1} = mapfold_list(F, Acc0, Ps0),
    {G1,Acc2} = mapfold(F, Acc1, G0),
    {B1,Acc3} = mapfold(F, Acc2, B0),
    F(R#c_clause{pats=Ps1,guard=G1,body=B1}, Acc3);
mapfold(F, Acc0, #c_receive{clauses=Cs0,timeout=T0,action=A0}=R) ->
    {T1,Acc1} = mapfold(F, Acc0, T0),
    {Cs1,Acc2} = mapfold_list(F, Acc1, Cs0),
    {A1,Acc3} = mapfold(F, Acc2, A0),
    F(R#c_receive{clauses=Cs1,timeout=T1,action=A1}, Acc3);
mapfold(F, Acc0, #c_apply{op=Op0, args=As0}=R) ->
    {Op1,Acc1} = mapfold(F, Acc0, Op0),
    {As1,Acc2} = mapfold_list(F, Acc1, As0),
    F(R#c_apply{op=Op1,args=As1}, Acc2);
mapfold(F, Acc0, #c_call{module=M0,name=N0,args=As0}=R) ->
    {M1,Acc1} = mapfold(F, Acc0, M0),
    {N1,Acc2} = mapfold(F, Acc1, N0),
    {As1,Acc3} = mapfold_list(F, Acc2, As0),
    F(R#c_call{module=M1,name=N1,args=As1}, Acc3);
mapfold(F, Acc0, #c_primop{name=N0, args=As0}=R) ->
    {N1,Acc1} = mapfold(F, Acc0, N0),
    {As1,Acc2} = mapfold_list(F, Acc1, As0),
    F(R#c_primop{name=N1,args=As1}, Acc2);
mapfold(F, Acc0, #c_try{arg=E0,vars=Vs0,body=B0,evars=Evs0,handler=H0}=R) ->
    {E1,Acc1} = mapfold(F, Acc0, E0),
    {Vs1,Acc2} = mapfold_list(F, Acc1, Vs0),
    {B1,Acc3} = mapfold(F, Acc2, B0),
    {Evs1,Acc4} = mapfold_list(F, Acc3, Evs0),
    {H1,Acc5} = mapfold(F, Acc4, H0),
    F(R#c_try{arg=E1,vars=Vs1,body=B1,evars=Evs1,handler=H1}, Acc5);
mapfold(F, Acc0, #c_catch{body=B0}=R) ->
    {B1,Acc1} = mapfold(F, Acc0, B0),
    F(R#c_catch{body=B1}, Acc1);
mapfold(F, Acc, T) ->				%Atomic nodes
	F(T, Acc).

mapfold_list(F, Acc, L) ->
    lists:mapfoldl(fun (E, A) -> mapfold(F, A, E) end, Acc, L).

%% is_var_used(VarName, Expr) -> true | false.
%%  Test if the variable VarName is used in Expr.

is_var_used(V, B) -> vu_body(V, B).

vu_body(V, #c_values{es=Es}) ->
    vu_expr_list(V, Es);
vu_body(V, Body) ->
    vu_expr(V, Body).

vu_expr(V, #c_var{name=V2}) -> V =:= V2;
vu_expr(V, #c_cons{hd=H,tl=T}) ->
    case vu_expr(V, H) of
	true -> true;
	false -> vu_expr(V, T)
    end;
vu_expr(V, #c_tuple{es=Es}) ->
    vu_expr_list(V, Es);
vu_expr(V, #c_binary{segments=Ss}) ->
    vu_seg_list(V, Ss);
vu_expr(V, #c_fun{vars=Vs,body=B}) ->
    %% Variables in fun shadow previous variables
    case vu_var_list(V, Vs) of
	true -> false;
	false -> vu_body(V, B)
    end;
vu_expr(V, #c_let{vars=Vs,arg=Arg,body=B}) ->
    case vu_body(V, Arg) of
	true -> true;
	false ->
	    %% Variables in let shadow previous variables.
	    case vu_var_list(V, Vs) of
		true -> false;
		false -> vu_body(V, B)
	    end
    end;
vu_expr(V, #c_letrec{defs=Fs,body=B}) ->
    case lists:any(fun (#c_def{val=Fb}) -> vu_body(V, Fb) end, Fs) of
	true -> true;
	false -> vu_body(V, B)
    end;
vu_expr(V, #c_seq{arg=Arg,body=B}) ->
    case vu_expr(V, Arg) of
	true -> true;
	false -> vu_body(V, B)
    end;
vu_expr(V, #c_case{arg=Arg,clauses=Cs}) ->
    case vu_expr(V, Arg) of
	true -> true;
	false -> vu_clauses(V, Cs)
    end;
vu_expr(V, #c_receive{clauses=Cs,timeout=T,action=A}) ->
    case vu_clauses(V, Cs) of
	true -> true;
	false ->
	    case vu_expr(V, T) of
		true -> true;
		false -> vu_body(V, A)
	    end
    end;
vu_expr(V, #c_apply{op=Op,args=As}) ->
    vu_expr_list(V, [Op|As]);
vu_expr(V, #c_call{module=M,name=N,args=As}) ->
    vu_expr_list(V, [M,N|As]);
vu_expr(V, #c_primop{args=As}) ->		%Name is an atom
    vu_expr_list(V, As);
vu_expr(V, #c_catch{body=B}) ->
    vu_body(V, B);
vu_expr(V, #c_try{arg=E,vars=Vs,body=B,evars=Evs,handler=H}) ->
    case vu_body(V, E) of
	true -> true;
	false ->
	    %% Variables shadow previous ones.
	    case case vu_var_list(V, Vs) of
		     true -> false;
		     false -> vu_body(V, B)
		 end of
		true -> true;
		false ->
		    case vu_var_list(V, Evs) of
			true -> false;
			false -> vu_body(V, H)
		    end
	    end
    end;
vu_expr(_, _) -> false.				%Everything else

vu_expr_list(V, Es) ->
    lists:any(fun(E) -> vu_expr(V, E) end, Es).

vu_seg_list(V, Ss) ->
    lists:any(fun (#c_bitstr{val=Val,size=Size}) ->
		      case vu_expr(V, Val) of
			  true -> true;
			  false -> vu_expr(V, Size)
		      end
	      end, Ss).

%% vu_clause(VarName, Clause) -> true | false.
%% vu_clauses(VarName, [Clause]) -> true | false.
%%  Have to get the pattern results right.

vu_clause(V, #c_clause{pats=Ps,guard=G,body=B}) ->
    case vu_pattern_list(V, Ps) of
	{true,_Shad} -> true;			%It is used
	{false,true} -> false;			%Shadowed
	{false,false} ->			%Not affected
	    case vu_expr(V, G) of
		true -> true;
		false ->vu_body(V, B)
	    end
    end.

vu_clauses(V, Cs) ->
    lists:any(fun(C) -> vu_clause(V, C) end, Cs).

%% vu_pattern(VarName, Pattern) -> {Used,Shadow}.
%% vu_pattern_list(VarName, [Pattern]) -> {Used,Shadow}.
%%  Binaries complicate patterns as a variable can both be properly
%%  used, in a bit segment size, and shadow.  They can also do both.

%%vu_pattern(V, Pat) -> vu_pattern(V, Pat, {false,false}).

vu_pattern(V, #c_var{name=V2}, St) ->
    setelement(2, St, V =:= V2);
vu_pattern(V, #c_cons{hd=H,tl=T}, St0) ->
    case vu_pattern(V, H, St0) of
	{true,true}=St1 -> St1;			%Nothing more to know
	St1 -> vu_pattern(V, T, St1)
    end;
vu_pattern(V, #c_tuple{es=Es}, St) ->
    vu_pattern_list(V, Es, St);
vu_pattern(V, #c_binary{segments=Ss}, St) ->
    vu_pat_seg_list(V, Ss, St);
vu_pattern(V, #c_alias{var=Var,pat=P}, St0) ->
    case vu_pattern(V, Var, St0) of
	{true,true}=St1 -> St1;
	St1 -> vu_pattern(V, P, St1)
    end;
vu_pattern(_, _, St) -> St.

vu_pattern_list(V, Ps) -> vu_pattern_list(V, Ps, {false,false}).

vu_pattern_list(V, Ps, St0) ->
    lists:foldl(fun(P, St) -> vu_pattern(V, P, St) end, St0, Ps).

vu_pat_seg_list(V, Ss, St) ->
    lists:foldl(fun (#c_bitstr{val=Val,size=Size}, St0) ->
			case vu_pattern(V, Val, St0) of
			    {true,true}=St1 -> St1;
			    {_Used,Shad} -> {vu_expr(V, Size),Shad}
			end
		end, St, Ss).

%% vu_var_list(VarName, [Var]) -> true | false.

vu_var_list(V, Vs) ->
    lists:any(fun (#c_var{name=V2}) -> V =:= V2 end, Vs).
