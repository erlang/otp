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
%%     $Id$
%%
-module(erl_id_trans).

%% An identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    forms(Forms).

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = form(F0),
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute,Line,module,Mod}) ->
    {attribute,Line,module,Mod};
form({attribute,Line,file,{File,Line}}) ->	%This is valid anywhere.
    {attribute,Line,file,{File,Line}};
form({attribute,Line,export,Es0}) ->
    Es1 = farity_list(Es0),
    {attribute,Line,export,Es1};
form({attribute,Line,import,{Mod,Is0}}) ->
    Is1 = farity_list(Is0),
    {attribute,Line,import,{Mod,Is1}};
form({attribute,Line,export_type,Es0}) ->
    Es1 = farity_list(Es0),
    {attribute,Line,export_type,Es1};
form({attribute,Line,optional_callbacks,Es0}) ->
    try farity_list(Es0) of
        Es1 ->
            {attribute,Line,optional_callbacks,Es1}
    catch
        _:_ ->
            {attribute,Line,optional_callbacks,Es0}
    end;
form({attribute,Line,compile,C}) ->
    {attribute,Line,compile,C};
form({attribute,Line,record,{Name,Defs0}}) ->
    Defs1 = record_defs(Defs0),
    {attribute,Line,record,{Name,Defs1}};
form({attribute,Line,asm,{function,N,A,Code}}) ->
    {attribute,Line,asm,{function,N,A,Code}};
form({attribute,Line,type,{N,T,Vs}}) ->
    T1 = type(T),
    Vs1 = variable_list(Vs),
    {attribute,Line,type,{N,T1,Vs1}};
form({attribute,Line,opaque,{N,T,Vs}}) ->
    T1 = type(T),
    Vs1 = variable_list(Vs),
    {attribute,Line,opaque,{N,T1,Vs1}};
form({attribute,Line,spec,{{N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Line,spec,{{N,A},FTs1}};
form({attribute,Line,spec,{{M,N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Line,spec,{{M,N,A},FTs1}};
form({attribute,Line,callback,{{N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Line,callback,{{N,A},FTs1}};
form({attribute,Line,Attr,Val}) ->		%The general attribute.
    {attribute,Line,Attr,Val};
form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
%% Extra forms from the parser.
form({error,E}) -> {error,E};
form({warning,W}) -> {warning,W};
form({eof,Line}) -> {eof,Line}.

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name,Arity}|Fas]) ->
    [{Name,Arity}|farity_list(Fas)];
farity_list([]) -> [].

%% -type variable_list([Var]) -> [Var]

variable_list([{var,Line,Var}|Vs]) ->
    [{var,Line,Var}|variable_list(Vs)];
variable_list([]) -> [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field,Line,{atom,La,A},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Line,{atom,La,A},Val1}|record_defs(Is)];
record_defs([{record_field,Line,{atom,La,A}}|Is]) ->
    [{record_field,Line,{atom,La,A}}|record_defs(Is)];
record_defs([{typed_record_field,{record_field,Line,{atom,La,A},Val0},Type}|
             Is]) ->
    Val1 = expr(Val0),
    Type1 = type(Type),
    [{typed_record_field,{record_field,Line,{atom,La,A},Val1},Type1}|
     record_defs(Is)];
record_defs([{typed_record_field,{record_field,Line,{atom,La,A}},Type}|Is]) ->
    Type1 = type(Type),
    [{typed_record_field,{record_field,Line,{atom,La,A}},Type1}|
     record_defs(Is)];
record_defs([]) -> [].

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause,Line,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Line,H1,G1,B1}.

%% -type head([Pattern]) -> [Pattern].

head(Ps) -> patterns(Ps).

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({match,Line,L0,R0}) ->
    L1 = pattern(L0),
    R1 = pattern(R0),
    {match,Line,L1,R1};
pattern({integer,Line,I}) -> {integer,Line,I};
pattern({char,Line,C}) -> {char,Line,C};
pattern({float,Line,F}) -> {float,Line,F};
pattern({atom,Line,A}) -> {atom,Line,A};
pattern({string,Line,S}) -> {string,Line,S};
pattern({nil,Line}) -> {nil,Line};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
pattern({map,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {map,Line,Ps1};
pattern({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = pattern(V),
    {map_field_exact,Line,Ke,Ve};
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record,Line,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Line,Name,Pfs1};
pattern({record_index,Line,Name,Field0}) ->
    Field1 = pattern(Field0),
    {record_index,Line,Name,Field1};
pattern({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
pattern({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
pattern({op,Line,Op,A}) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R}) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1),S2,T2} | pattern_grp(Fs)];
pattern_grp([]) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].



%% -type pattern_list([Pattern]) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{var,La,'_'},P1}|pattern_fields(Pfs)];
pattern_fields([]) -> [].

%% -type guard([GuardTest]) -> [GuardTest].

guard([G0|Gs]) when is_list(G0) ->
    [guard0(G0) | guard(Gs)];
guard(L) ->
    guard0(L).

guard0([G0|Gs]) ->
    G1 =  guard_test(G0),
    [G1|guard0(Gs)];
guard0([]) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
	true -> 
	    As1 = gexpr_list(As0),
	    {call,Line,{atom,La,F},As1};
	_ ->
	    gexpr(Expr)
    end;
guard_test(Any) ->
    gexpr(Any).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Line,V}) -> {var,Line,V};
gexpr({integer,Line,I}) -> {integer,Line,I};
gexpr({char,Line,C}) -> {char,Line,C};
gexpr({float,Line,F}) -> {float,Line,F};
gexpr({atom,Line,A}) -> {atom,Line,A};
gexpr({string,Line,S}) -> {string,Line,S};
gexpr({nil,Line}) -> {nil,Line};
gexpr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = gexpr_list([Map0|Es0]),
    {map,Line,Map1,Es1};
gexpr({map,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {map,Line,Es1};
gexpr({map_field_assoc,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_assoc,Line,Ke,Ve};
gexpr({map_field_exact,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_exact,Line,Ke,Ve};
gexpr({cons,Line,H0,T0}) ->
    H1 = gexpr(H0),
    T1 = gexpr(T0),				%They see the same variables
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
gexpr({record_index,Line,Name,Field0}) ->
    Field1 = gexpr(Field0),
    {record_index,Line,Name,Field1};
gexpr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = gexpr(Rec0),
    Field1 = gexpr(Field0),
    {record_field,Line,Rec1,Name,Field1};
gexpr({record,Line,Name,Inits0}) ->
    Inits1 = grecord_inits(Inits0),
    {record,Line,Name,Inits1};
gexpr({call,Line,{atom,La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{atom,La,F},As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
gexpr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0),
		{op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {op,Line,Op,L1,R1};
gexpr({op,Line,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {op,Line,Op,L1,R1}
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|grecord_inits(Is)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|grecord_inits(Is)];
grecord_inits([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Line,V}) -> {var,Line,V};
expr({integer,Line,I}) -> {integer,Line,I};
expr({float,Line,F}) -> {float,Line,F};
expr({atom,Line,A}) -> {atom,Line,A};
expr({string,Line,S}) -> {string,Line,S};
expr({char,Line,C}) -> {char,Line,C};
expr({nil,Line}) -> {nil,Line};
expr({cons,Line,H0,T0}) ->
    H1 = expr(H0),
    T1 = expr(T0),				%They see the same variables
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {lc,Line,E1,Qs1};
expr({bc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {bc,Line,E1,Qs1};
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
expr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = exprs([Map0|Es0]),
    {map,Line,Map1,Es1};
expr({map,Line,Es0}) ->
    Es1 = exprs(Es0),
    {map,Line,Es1};
expr({map_field_assoc,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_assoc,Line,Ke,Ve};
expr({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_exact,Line,Ke,Ve};
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0}) ->
    Field1 = expr(Field0),
    {record_index,Line,Name,Field1};
expr({record,Line,Name,Inits0}) ->
    Inits1 = record_inits(Inits0),
    {record,Line,Name,Inits1};
expr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
expr({record,Line,Rec0,Name,Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record,Line,Rec1,Name,Upds1};
expr({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}) ->
    Es1 = exprs(Es0),
    Scs1 = icr_clauses(Scs0),
    Ccs1 = icr_clauses(Ccs0),
    As1 = exprs(As0),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body}) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0),
	    {'fun',Line,{clauses,Cs1}};
	{function,F,A} ->
	    {'fun',Line,{function,F,A}};
	{function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    %% R10B-6: fun M:F/A. (Backward compatibility)
	    {'fun',Line,{function,M,F,A}};
	{function,M0,F0,A0} ->
	    %% R15: fun M:F/A with variables.
	    M = expr(M0),
	    F = expr(F0),
	    A = expr(A0),
	    {'fun',Line,{function,M,F,A}}
    end;
expr({named_fun,Loc,Name,Cs}) ->
    {named_fun,Loc,Name,fun_clauses(Cs)};
expr({call,Line,F0,As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    {call,Line,F1,As1};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Line,P1,E1};
expr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote,Line,M1,F1}.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_inits(Is)];
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([{b_generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|lc_bc_quals(Qs)];
lc_bc_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].

function_type_list([{type,Line,bounded_fun,[Ft,Fc]}|Fts]) ->
    Ft1 = function_type(Ft),
    Fc1 = function_constraint(Fc),
    [{type,Line,bounded_fun,[Ft1,Fc1]}|function_type_list(Fts)];
function_type_list([Ft|Fts]) ->
    [function_type(Ft)|function_type_list(Fts)];
function_type_list([]) -> [].

function_type({type,Line,'fun',[{type,Lt,product,As},B]}) ->
    As1 = type_list(As),
    B1 = type(B),
    {type,Line,'fun',[{type,Lt,product,As1},B1]}.

function_constraint([C|Cs]) ->
    C1 = constraint(C),
    [C1|function_constraint(Cs)];
function_constraint([]) -> [].

constraint({type,Line,constraint,[{atom,L,A},[V,T]]}) ->
    V1 = type(V),
    T1 = type(T),
    {type,Line,constraint,[{atom,L,A},[V1,T1]]}.

type({ann_type,Line,[{var,Lv,V},T]}) ->
    T1 = type(T),
    {ann_type,Line,[{var,Lv,V},T1]};
type({atom,Line,A}) ->
    {atom,Line,A};
type({integer,Line,I}) ->
    {integer,Line,I};
type({op,Line,Op,T}) ->
    T1 = type(T),
    {op,Line,Op,T1};
type({op,Line,Op,L,R}) ->
    L1 = type(L),
    R1 = type(R),
    {op,Line,Op,L1,R1};
type({type,Line,binary,[M,N]}) ->
    M1 = type(M),
    N1 = type(N),
    {type,Line,binary,[M1,N1]};
type({type,Line,'fun',[]}) ->
    {type,Line,'fun',[]};
type({type,Line,'fun',[{type,Lt,any},B]}) ->
    B1 = type(B),
    {type,Line,'fun',[{type,Lt,any},B1]};
type({type,Line,range,[L,H]}) ->
    L1 = type(L),
    H1 = type(H),
    {type,Line,range,[L1,H1]};
type({type,Line,map,any}) ->
    {type,Line,map,any};
type({type,Line,map,Ps}) ->
    Ps1 = map_pair_types(Ps),
    {type,Line,map,Ps1};
type({type,Line,record,[{atom,La,N}|Fs]}) ->
    Fs1 = field_types(Fs),
    {type,Line,record,[{atom,La,N}|Fs1]};
type({remote_type,Line,[{atom,Lm,M},{atom,Ln,N},As]}) ->
    As1 = type_list(As),
    {remote_type,Line,[{atom,Lm,M},{atom,Ln,N},As1]};
type({type,Line,tuple,any}) ->
    {type,Line,tuple,any};
type({type,Line,tuple,Ts}) ->
    Ts1 = type_list(Ts),
    {type,Line,tuple,Ts1};
type({type,Line,union,Ts}) ->
    Ts1 = type_list(Ts),
    {type,Line,union,Ts1};
type({var,Line,V}) ->
    {var,Line,V};
type({user_type,Line,N,As}) ->
    As1 = type_list(As),
    {user_type,Line,N,As1};
type({type,Line,N,As}) ->
    As1 = type_list(As),
    {type,Line,N,As1}.

map_pair_types([{type,Line,map_field_assoc,[K,V]}|Ps]) ->
    K1 = type(K),
    V1 = type(V),
    [{type,Line,map_field_assoc,[K1,V1]}|map_pair_types(Ps)];
map_pair_types([{type,Line,map_field_exact,[K,V]}|Ps]) ->
    K1 = type(K),
    V1 = type(V),
    [{type,Line,map_field_exact,[K1,V1]}|map_pair_types(Ps)];
map_pair_types([]) -> [].

field_types([{type,Line,field_type,[{atom,La,A},T]}|Fs]) ->
    T1 = type(T),
    [{type,Line,field_type,[{atom,La,A},T1]}|field_types(Fs)];
field_types([]) -> [].

type_list([T|Ts]) ->
    T1 = type(T),
    [T1|type_list(Ts)];
type_list([]) -> [].
