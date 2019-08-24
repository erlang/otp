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
%%     $Id: sys_expand_pmod.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
-module(sys_expand_pmod).

%% Expand function definition forms of parameterized module. We assume
%% all record definitions, imports, queries, etc., have been expanded
%% away. Any calls on the form 'foo(...)' must be calls to local
%% functions. Auto-generated functions (module_info,...) have not yet
%% been added to the function definitions, but are listed in 'defined'
%% and 'exports'. The 'new/N' function is neither added to the
%% definitions nor to the 'exports'/'defines' lists yet.

-export([forms/4]).

-record(pmod, {parameters, exports, defined, predef}).

%% TODO: more abstract handling of predefined/static functions.

forms(Fs0, Ps, Es0, Ds0) ->
    PreDef = [{module_info,0},{module_info,1}],
    forms(Fs0, Ps, Es0, Ds0, PreDef).

forms(Fs0, Ps, Es0, Ds0, PreDef) ->
    St0 = #pmod{parameters=Ps,exports=Es0,defined=Ds0, predef=PreDef},
    {Fs1, St1} = forms(Fs0, St0),
    Es1 = update_function_names(Es0, St1),
    Ds1 = update_function_names(Ds0, St1),
    Fs2 = update_forms(Fs1, St1),
    {Fs2,Es1,Ds1}.

%% This is extremely simplistic for now; all functions get an extra
%% parameter, whether they need it or not, except for static functions.

update_function_names(Es, St) ->
    [update_function_name(E, St) || E <- Es].

update_function_name(E={F,A}, St) ->
    case ordsets:is_element(E, St#pmod.predef) of
	true -> E;
	false -> {F, A + 1}
    end.

update_forms([{function,L,N,A,Cs}|Fs],St) ->
    [{function,L,N,A+1,Cs}|update_forms(Fs,St)];
update_forms([F|Fs],St) ->
    [F|update_forms(Fs,St)];
update_forms([],_St) ->
    [].

%% Process the program forms.

forms([F0|Fs0],St0) ->
    {F1,St1} = form(F0,St0),
    {Fs1,St2} = forms(Fs0,St1),
    {[F1|Fs1],St2};
forms([], St0) ->
    {[], St0}.

%% Only function definitions are of interest here. State is not updated.
form({function,Line,Name0,Arity0,Clauses0},St) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0, St),
    {{function,Line,Name,Arity,Clauses},St};
%% Pass anything else through
form(F,St) -> {F,St}.

function(Name, Arity, Clauses0, St) ->
    Clauses1 = clauses(Clauses0,St),
    {Name,Arity,Clauses1}.

clauses([C|Cs],St) ->
    {clause,L,H,G,B} = clause(C,St),
    T = {tuple,L,[{var,L,V} || V <- ['_'|St#pmod.parameters]]},
    [{clause,L,H++[{match,L,T,{var,L,'THIS'}}],G,B}|clauses(Cs,St)];
clauses([],_St) -> [].

clause({clause,Line,H0,G0,B0},St) ->
    H1 = head(H0,St),
    G1 = guard(G0,St),
    B1 = exprs(B0,St),
    {clause,Line,H1,G1,B1}.

head(Ps,St) -> patterns(Ps,St).

patterns([P0|Ps],St) ->
    P1 = pattern(P0,St),
    [P1|patterns(Ps,St)];
patterns([],_St) -> [].

string_to_conses([], _Line, Tail) ->
    Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

pattern({var,Line,V},_St) -> {var,Line,V};
pattern({match,Line,L0,R0},St) ->
    L1 = pattern(L0,St),
    R1 = pattern(R0,St),
    {match,Line,L1,R1};
pattern({integer,Line,I},_St) -> {integer,Line,I};
pattern({char,Line,C},_St) -> {char,Line,C};
pattern({float,Line,F},_St) -> {float,Line,F};
pattern({atom,Line,A},_St) -> {atom,Line,A};
pattern({string,Line,S},_St) -> {string,Line,S};
pattern({nil,Line},_St) -> {nil,Line};
pattern({cons,Line,H0,T0},St) ->
    H1 = pattern(H0,St),
    T1 = pattern(T0,St),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0},St) ->
    Ps1 = pattern_list(Ps0,St),
    {tuple,Line,Ps1};
pattern({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
pattern({op,_Line,'++',{nil,_},R},St) ->
    pattern(R,St);
pattern({op,_Line,'++',{cons,Li,{char,C2,I},T},R},St) ->
    pattern({cons,Li,{char,C2,I},{op,Li,'++',T,R}},St);
pattern({op,_Line,'++',{cons,Li,{integer,L2,I},T},R},St) ->
    pattern({cons,Li,{integer,L2,I},{op,Li,'++',T,R}},St);
pattern({op,_Line,'++',{string,Li,L},R},St) ->
    pattern(string_to_conses(L, Li, R),St);
pattern({op,Line,Op,A},_St) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R},_St) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs],St) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1,St)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1,St),S2,T2} | pattern_grp(Fs,St)];
pattern_grp([],_St) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when atom(Atom), integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].

pattern_list([P0|Ps],St) ->
    P1 = pattern(P0,St),
    [P1|pattern_list(Ps,St)];
pattern_list([],_St) -> [].

guard([G0|Gs],St) when list(G0) ->
    [guard0(G0,St) | guard(Gs,St)];
guard(L,St) ->
    guard0(L,St).

guard0([G0|Gs],St) ->
    G1 = guard_test(G0,St),
    [G1|guard0(Gs,St)];
guard0([],_St) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0},St) ->
    case erl_internal:type_test(F, length(As0)) of
	true ->
	    As1 = gexpr_list(As0,St),
	    {call,Line,{atom,La,F},As1};
	_ ->
	    gexpr(Expr,St)
    end;
guard_test(Any,St) ->
    gexpr(Any,St).

gexpr({var,L,V},_St) ->
    {var,L,V};
% %% alternative implementation of accessing module parameters
%     case index(V,St#pmod.parameters) of
% 	N when N > 0 ->
% 	    {call,L,{remote,L,{atom,L,erlang},{atom,L,element}},
% 	     [{integer,L,N+1},{var,L,'THIS'}]};
% 	_ ->
% 	    {var,L,V}
%     end;
gexpr({integer,Line,I},_St) -> {integer,Line,I};
gexpr({char,Line,C},_St) -> {char,Line,C};
gexpr({float,Line,F},_St) -> {float,Line,F};
gexpr({atom,Line,A},_St) -> {atom,Line,A};
gexpr({string,Line,S},_St) -> {string,Line,S};
gexpr({nil,Line},_St) -> {nil,Line};
gexpr({cons,Line,H0,T0},St) ->
    H1 = gexpr(H0,St),
    T1 = gexpr(T0,St),
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0},St) ->
    Es1 = gexpr_list(Es0,St),
    {tuple,Line,Es1};
gexpr({call,Line,{atom,La,F},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{atom,La,F},As1}
    end;
% Pre-expansion generated calls to erlang:is_record/3 must also be handled
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,is_record}},As0},St)
  when length(As0) == 3 ->
    As1 = gexpr_list(As0,St),
    {call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,is_record}},As1};
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
% Unfortunately, writing calls as {M,F}(...) is also allowed.
gexpr({call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As0},St) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0,St),
		{call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As1}
    end;
gexpr({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0},St) ->
    case erl_internal:arith_op(Op, 1) or
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0,St),
		{op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0},St) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0,St),
	    R1 = gexpr(R0,St),
	    {op,Line,Op,L1,R1}
    end.

gexpr_list([E0|Es],St) ->
    E1 = gexpr(E0,St),
    [E1|gexpr_list(Es,St)];
gexpr_list([],_St) -> [].

exprs([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|exprs(Es,St)];
exprs([],_St) -> [].

expr({var,L,V},_St) ->
    {var,L,V};
%     case index(V,St#pmod.parameters) of
% 	N when N > 0 ->
% 	    {call,L,{remote,L,{atom,L,erlang},{atom,L,element}},
% 	     [{integer,L,N+1},{var,L,'THIS'}]};
% 	_ ->
% 	    {var,L,V}
%     end;
expr({integer,Line,I},_St) -> {integer,Line,I};
expr({float,Line,F},_St) -> {float,Line,F};
expr({atom,Line,A},_St) -> {atom,Line,A};
expr({string,Line,S},_St) -> {string,Line,S};
expr({char,Line,C},_St) -> {char,Line,C};
expr({nil,Line},_St) -> {nil,Line};
expr({cons,Line,H0,T0},St) ->
    H1 = expr(H0,St),
    T1 = expr(T0,St),
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0},St) ->
    Qs1 = lc_quals(Qs0,St),
    E1 = expr(E0,St),
    {lc,Line,E1,Qs1};
expr({tuple,Line,Es0},St) ->
    Es1 = expr_list(Es0,St),
    {tuple,Line,Es1};
expr({block,Line,Es0},St) ->
    Es1 = exprs(Es0,St),
    {block,Line,Es1};
expr({'if',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0},St) ->
    E1 = expr(E0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0},St) ->
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0},St) ->
    To1 = expr(To0,St),
    ToEs1 = exprs(ToEs0,St),
    Cs1 = icr_clauses(Cs0,St),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0},St) ->
    Es1 = exprs(Es0,St),
    Scs1 = icr_clauses(Scs0,St),
    Ccs1 = icr_clauses(Ccs0,St),
    As1 = exprs(As0,St),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body,Info},St) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0,St),
	    {'fun',Line,{clauses,Cs1},Info};
	{function,F,A} ->
	    {F1,A1} = update_function_name({F,A},St),
	    if A1 == A ->
		    {'fun',Line,{function,F,A},Info};
	       true ->
		    %% Must rewrite local fun-name to a fun that does a
		    %% call with the extra THIS parameter.
		    As = make_vars(A, Line),
		    As1 = As ++ [{var,Line,'THIS'}],
		    Call = {call,Line,{atom,Line,F1},As1},
		    Cs = [{clause,Line,As,[],[Call]}],
		    {'fun',Line,{clauses,Cs},Info}
	    end;
	{function,M,F,A} ->			%This is an error in lint!
	    {'fun',Line,{function,M,F,A},Info}
    end;
expr({named_fun,Loc,Name,Cs,Info},St) ->
    {named_fun,Loc,Name,fun_clauses(Cs, St),Info};
expr({call,Lc,{atom,_,new}=Name,As0},#pmod{parameters=Ps}=St)
  when length(As0) =:= length(Ps) ->
    %% The new() function does not take a 'THIS' argument (it's static).
    As1 = expr_list(As0,St),
    {call,Lc,Name,As1};
expr({call,Lc,{atom,_,module_info}=Name,As0},St)
  when length(As0) == 0; length(As0) == 1 ->
    %% The module_info/0 and module_info/1 functions are also static.
    As1 = expr_list(As0,St),
    {call,Lc,Name,As1};
expr({call,Lc,{atom,Lf,F},As0},St) ->
    %% Local function call - needs THIS parameter.
    As1 = expr_list(As0,St),
    {call,Lc,{atom,Lf,F},As1 ++ [{var,0,'THIS'}]};
expr({call,Line,F0,As0},St) ->
    %% Other function call
    F1 = expr(F0,St),
    As1 = expr_list(As0,St),
    {call,Line,F1,As1};
expr({'catch',Line,E0},St) ->
    E1 = expr(E0,St),
    {'catch',Line,E1};
expr({match,Line,P0,E0},St) ->
    E1 = expr(E0,St),
    P1 = pattern(P0,St),
    {match,Line,P1,E1};
expr({bin,Line,Fs},St) ->
    Fs2 = pattern_grp(Fs,St),
    {bin,Line,Fs2};
expr({op,Line,Op,A0},St) ->
    A1 = expr(A0,St),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0},St) ->
    L1 = expr(L0,St),
    R1 = expr(R0,St),
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0},St) ->
    M1 = expr(M0,St),
    F1 = expr(F0,St),
    {remote,Line,M1,F1}.

expr_list([E0|Es],St) ->
    E1 = expr(E0,St),
    [E1|expr_list(Es,St)];
expr_list([],_St) -> [].

icr_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|icr_clauses(Cs,St)];
icr_clauses([],_St) -> [].

lc_quals([{generate,Line,P0,E0}|Qs],St) ->
    E1 = expr(E0,St),
    P1 = pattern(P0,St),
    [{generate,Line,P1,E1}|lc_quals(Qs,St)];
lc_quals([E0|Qs],St) ->
    E1 = expr(E0,St),
    [E1|lc_quals(Qs,St)];
lc_quals([],_St) -> [].

fun_clauses([C0|Cs],St) ->
    C1 = clause(C0,St),
    [C1|fun_clauses(Cs,St)];
fun_clauses([],_St) -> [].

% %% Return index from 1 upwards, or 0 if not in the list.
%
% index(X,Ys) -> index(X,Ys,1).
%
% index(X,[X|Ys],A) -> A;
% index(X,[Y|Ys],A) -> index(X,Ys,A+1);
% index(X,[],A) -> 0.

make_vars(N, L) ->
    make_vars(1, N, L).

make_vars(N, M, L) when N =< M ->
    V = list_to_atom("X"++integer_to_list(N)),
    [{var,L,V} | make_vars(N + 1, M, L)];
make_vars(_, _, _) ->
    [].
