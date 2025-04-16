%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
-module(erl_id_trans).

-moduledoc """
This module performs an identity parse transformation of Erlang code.

It is included as an example for users who wants to write their own
parse transformers. If option `{parse_transform,Module}` is passed
to the compiler, a user-written function `parse_transform/2`
is called by the compiler before the code is checked for errors.

Before the function `parse_transform/2` is called, the Erlang
Compiler checks if the parse transformation can handle abstract code
with column numbers: If the function `parse_transform_info/0`
is implemented and returns a map where the key `error_location` is
associated with the value `line`, the compiler removes
column numbers from the abstract code before calling the parse
transform. Otherwise, the compiler passes the abstract code on
without modification.

## Parse Transformations

Parse transformations are used if a programmer wants to use
Erlang syntax, but with different semantics. The original Erlang
code is then transformed into other Erlang code.

> #### Note {: .info }
>
> Programmers are strongly advised not to engage in parse
> transformations. No support is offered for problems encountered.
>

## See Also

`m:erl_parse` and `m:compile`.
""".

%% An identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2, parse_transform_info/0]).

-doc "Performs an identity transformation on Erlang forms, as an example.".
-spec parse_transform(Forms, Options) -> NewForms when
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      NewForms :: Forms,
      Options :: [compile:option()].
parse_transform(Forms, _Options) ->
    forms(Forms).

-doc "Returns information about the parse transform itself.".
-doc(#{since => <<"OTP 24.0">>}).
-spec parse_transform_info() -> #{ 'error_location' => 'column' | 'line' }.
parse_transform_info() ->
    #{error_location => column}.

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
form({attribute,Anno,module,Mod}) ->
    {attribute,Anno,module,Mod};
form({attribute,Anno,file,{File,Line}}) ->	%This is valid anywhere.
    {attribute,Anno,file,{File,Line}};
form({attribute,Anno,export,Es0}) ->
    Es1 = farity_list(Es0),
    {attribute,Anno,export,Es1};
form({attribute,Anno,import,{Mod,Is0}}) ->
    Is1 = farity_list(Is0),
    {attribute,Anno,import,{Mod,Is1}};
form({attribute,Anno,export_type,Es0}) ->
    Es1 = farity_list(Es0),
    {attribute,Anno,export_type,Es1};
form({attribute,Anno,optional_callbacks,Es0}) ->
    try farity_list(Es0) of
        Es1 ->
            {attribute,Anno,optional_callbacks,Es1}
    catch
        _:_ ->
            {attribute,Anno,optional_callbacks,Es0}
    end;
form({attribute,Anno,compile,C}) ->
    {attribute,Anno,compile,C};
form({attribute,Anno,record,{Name,Defs0}}) ->
    Defs1 = record_defs(Defs0),
    {attribute,Anno,record,{Name,Defs1}};
form({attribute,Anno,asm,{function,N,A,Code}}) ->
    {attribute,Anno,asm,{function,N,A,Code}};
form({attribute,Anno,type,{N,T,Vs}}) ->
    T1 = type(T),
    Vs1 = variable_list(Vs),
    {attribute,Anno,type,{N,T1,Vs1}};
form({attribute,Anno,opaque,{N,T,Vs}}) ->
    T1 = type(T),
    Vs1 = variable_list(Vs),
    {attribute,Anno,opaque,{N,T1,Vs1}};
form({attribute,Anno,spec,{{N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Anno,spec,{{N,A},FTs1}};
form({attribute,Anno,spec,{{M,N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Anno,spec,{{M,N,A},FTs1}};
form({attribute,Anno,callback,{{N,A},FTs}}) ->
    FTs1 = function_type_list(FTs),
    {attribute,Anno,callback,{{N,A},FTs1}};
form({attribute,Anno,Attr,Val}) ->		%The general attribute.
    {attribute,Anno,Attr,Val};
form({function,Anno,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Anno,Name,Arity,Clauses};
%% Extra forms from the parser.
form({error,E}) -> {error,E};
form({warning,W}) -> {warning,W};
form({eof,Location}) -> {eof,Location}.

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name,Arity}|Fas]) ->
    [{Name,Arity}|farity_list(Fas)];
farity_list([]) -> [].

%% -type variable_list([Var]) -> [Var]

variable_list([{var,Anno,Var}|Vs]) ->
    [{var,Anno,Var}|variable_list(Vs)];
variable_list([]) -> [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field,Anno,{atom,Aa,A},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Anno,{atom,Aa,A},Val1}|record_defs(Is)];
record_defs([{record_field,Anno,{atom,Aa,A}}|Is]) ->
    [{record_field,Anno,{atom,Aa,A}}|record_defs(Is)];
record_defs([{typed_record_field,{record_field,Anno,{atom,Aa,A},Val0},Type}|
             Is]) ->
    Val1 = expr(Val0),
    Type1 = type(Type),
    [{typed_record_field,{record_field,Anno,{atom,Aa,A},Val1},Type1}|
     record_defs(Is)];
record_defs([{typed_record_field,{record_field,Anno,{atom,Aa,A}},Type}|Is]) ->
    Type1 = type(Type),
    [{typed_record_field,{record_field,Anno,{atom,Aa,A}},Type1}|
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

clause({clause,Anno,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Anno,H1,G1,B1}.

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

pattern({var,Anno,V}) -> {var,Anno,V};
pattern({match,Anno,L0,R0}) ->
    L1 = pattern(L0),
    R1 = pattern(R0),
    {match,Anno,L1,R1};
pattern({integer,Anno,I}) -> {integer,Anno,I};
pattern({char,Anno,C}) -> {char,Anno,C};
pattern({float,Anno,F}) -> {float,Anno,F};
pattern({atom,Anno,A}) -> {atom,Anno,A};
pattern({string,Anno,S}) -> {string,Anno,S};
pattern({nil,Anno}) -> {nil,Anno};
pattern({cons,Anno,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Anno,H1,T1};
pattern({tuple,Anno,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Anno,Ps1};
pattern({map,Anno,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {map,Anno,Ps1};
pattern({map_field_exact,Anno,K,V}) ->
    Ke = expr(K),
    Ve = pattern(V),
    {map_field_exact,Anno,Ke,Ve};
pattern({record,Anno,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Anno,Name,Pfs1};
pattern({record_index,Anno,Name,Field0}) ->
    Field1 = pattern(Field0),
    {record_index,Anno,Name,Field1};
pattern({record_field,Anno,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Anno,Rec1,Name,Field1};
pattern({record_field,Anno,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Anno,Rec1,Field1};
pattern({bin,Anno,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Anno,Fs2};
pattern({op,Anno,Op,A}) ->
    {op,Anno,Op,A};
pattern({op,Anno,Op,L,R}) ->
    {op,Anno,Op,L,R}.

pattern_grp([{bin_element,Anno,E1,S1,T1} | Fs]) ->
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
    [{bin_element,Anno,expr(E1),S2,T2} | pattern_grp(Fs)];
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

pattern_fields([{record_field,Af,{atom,Aa,F},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Af,{atom,Aa,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Af,{var,Aa,'_'},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Af,{var,Aa,'_'},P1}|pattern_fields(Pfs)];
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

guard_test(Expr={call,Anno,{atom,Aa,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
	true ->
	    As1 = gexpr_list(As0),
	    {call,Anno,{atom,Aa,F},As1};
	_ ->
	    gexpr(Expr)
    end;
guard_test(Any) ->
    gexpr(Any).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Anno,V}) -> {var,Anno,V};
gexpr({integer,Anno,I}) -> {integer,Anno,I};
gexpr({char,Anno,C}) -> {char,Anno,C};
gexpr({float,Anno,F}) -> {float,Anno,F};
gexpr({atom,Anno,A}) -> {atom,Anno,A};
gexpr({string,Anno,S}) -> {string,Anno,S};
gexpr({nil,Anno}) -> {nil,Anno};
gexpr({map,Anno,Map0,Es0}) ->
    [Map1|Es1] = gexpr_list([Map0|Es0]),
    {map,Anno,Map1,Es1};
gexpr({map,Anno,Es0}) ->
    Es1 = gexpr_list(Es0),
    {map,Anno,Es1};
gexpr({map_field_assoc,Anno,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_assoc,Anno,Ke,Ve};
gexpr({map_field_exact,Anno,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_exact,Anno,Ke,Ve};
gexpr({cons,Anno,H0,T0}) ->
    H1 = gexpr(H0),
    T1 = gexpr(T0),				%They see the same variables
    {cons,Anno,H1,T1};
gexpr({tuple,Anno,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Anno,Es1};
gexpr({record_index,Anno,Name,Field0}) ->
    Field1 = gexpr(Field0),
    {record_index,Anno,Name,Field1};
gexpr({record_field,Anno,Rec0,Name,Field0}) ->
    Rec1 = gexpr(Rec0),
    Field1 = gexpr(Field0),
    {record_field,Anno,Rec1,Name,Field1};
gexpr({record,Anno,Name,Inits0}) ->
    Inits1 = grecord_inits(Inits0),
    {record,Anno,Name,Inits1};
gexpr({call,Anno,{atom,Aa,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Anno,{atom,Aa,F},As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Anno,{remote,Aa,{atom,Ab,erlang},{atom,Ac,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Anno,{remote,Aa,{atom,Ab,erlang},{atom,Ac,F}},As1}
    end;
gexpr({bin,Anno,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Anno,Fs2};
gexpr({op,Anno,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) or
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0),
		{op,Anno,Op,A1}
    end;
gexpr({op,Anno,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {op,Anno,Op,L1,R1};
gexpr({op,Anno,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {op,Anno,Op,L1,R1}
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

grecord_inits([{record_field,Af,{atom,Aa,F},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Af,{atom,Aa,F},Val1}|grecord_inits(Is)];
grecord_inits([{record_field,Af,{var,Aa,'_'},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Af,{var,Aa,'_'},Val1}|grecord_inits(Is)];
grecord_inits([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Anno,V}) -> {var,Anno,V};
expr({integer,Anno,I}) -> {integer,Anno,I};
expr({float,Anno,F}) -> {float,Anno,F};
expr({atom,Anno,A}) -> {atom,Anno,A};
expr({string,Anno,S}) -> {string,Anno,S};
expr({char,Anno,C}) -> {char,Anno,C};
expr({nil,Anno}) -> {nil,Anno};
expr({cons,Anno,H0,T0}) ->
    H1 = expr(H0),
    T1 = expr(T0),				%They see the same variables
    {cons,Anno,H1,T1};
expr({lc,Anno,E0,Qs0}) ->
    Qs1 = comprehension_quals(Qs0),
    E1 = expr(E0),
    {lc,Anno,E1,Qs1};
expr({bc,Anno,E0,Qs0}) ->
    Qs1 = comprehension_quals(Qs0),
    E1 = expr(E0),
    {bc,Anno,E1,Qs1};
expr({mc,Anno,E0,Qs0}) ->
    Qs1 = comprehension_quals(Qs0),
    E1 = expr(E0),
    {mc,Anno,E1,Qs1};
expr({tuple,Anno,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Anno,Es1};
expr({map,Anno,Map0,Es0}) ->
    [Map1|Es1] = exprs([Map0|Es0]),
    {map,Anno,Map1,Es1};
expr({map,Anno,Es0}) ->
    Es1 = exprs(Es0),
    {map,Anno,Es1};
expr({map_field_assoc,Anno,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_assoc,Anno,Ke,Ve};
expr({map_field_exact,Anno,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_exact,Anno,Ke,Ve};
expr({record_index,Anno,Name,Field0}) ->
    Field1 = expr(Field0),
    {record_index,Anno,Name,Field1};
expr({record,Anno,Name,Inits0}) ->
    Inits1 = record_inits(Inits0),
    {record,Anno,Name,Inits1};
expr({record_field,Anno,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Anno,Rec1,Name,Field1};
expr({record,Anno,Rec0,Name,Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record,Anno,Rec1,Name,Upds1};
expr({record_field,Anno,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Anno,Rec1,Field1};
expr({block,Anno,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Anno,Es1};
expr({'if',Anno,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Anno,Cs1};
expr({'case',Anno,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Anno,E1,Cs1};
expr({'receive',Anno,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Anno,Cs1};
expr({'receive',Anno,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Anno,Cs1,To1,ToEs1};
expr({'try',Anno,Es0,Scs0,Ccs0,As0}) ->
    Es1 = exprs(Es0),
    Scs1 = icr_clauses(Scs0),
    Ccs1 = icr_clauses(Ccs0),
    As1 = exprs(As0),
    {'try',Anno,Es1,Scs1,Ccs1,As1};
expr({'fun',Anno,Body}) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0),
	    {'fun',Anno,{clauses,Cs1}};
	{function,F,A} ->
	    {'fun',Anno,{function,F,A}};
	{function,M0,F0,A0} ->
	    M = expr(M0),
	    F = expr(F0),
	    A = expr(A0),
	    {'fun',Anno,{function,M,F,A}}
    end;
expr({named_fun,Anno,Name,Cs}) ->
    {named_fun,Anno,Name,fun_clauses(Cs)};
expr({call,Anno,F0,As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    {call,Anno,F1,As1};
expr({'catch',Anno,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Anno,E1};
expr({'maybe',MaybeAnno,Es0}) ->
    Es = exprs(Es0),
    {'maybe',MaybeAnno,Es};
expr({'maybe',MaybeAnno,Es0,{'else',ElseAnno,Cs0}}) ->
    Es = exprs(Es0),
    Cs = clauses(Cs0),
    {'maybe',MaybeAnno,Es,{'else',ElseAnno,Cs}};
expr({maybe_match,Anno,P0,E0}) ->
    E = expr(E0),
    P = pattern(P0),
    {maybe_match,Anno,P,E};
expr({match,Anno,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Anno,P1,E1};
expr({bin,Anno,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Anno,Fs2};
expr({op,Anno,Op,A0}) ->
    A1 = expr(A0),
    {op,Anno,Op,A1};
expr({op,Anno,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Anno,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Anno,M0,F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote,Anno,M1,F1}.

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

record_inits([{record_field,Af,{atom,Aa,F},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Af,{atom,Aa,F},Val1}|record_inits(Is)];
record_inits([{record_field,Af,{var,Aa,'_'},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Af,{var,Aa,'_'},Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Af,{atom,Aa,F},Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field,Af,{atom,Aa,F},Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

%% -type comprehension_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

comprehension_quals([{generate,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{generate_strict,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate_strict,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{b_generate,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{b_generate_strict,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate_strict,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{m_generate,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{m_generate,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{m_generate_strict,Anno,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{m_generate_strict,Anno,P1,E1}|comprehension_quals(Qs)];
comprehension_quals([{zip,Anno,Gens0}|Qs]) ->
    Gens1 = comprehension_quals(Gens0),
    [{zip,Anno,Gens1}|comprehension_quals(Qs)];
comprehension_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|comprehension_quals(Qs)];
comprehension_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].

function_type_list([{type,Anno,bounded_fun,[Ft,Fc]}|Fts]) ->
    Ft1 = function_type(Ft),
    Fc1 = function_constraint(Fc),
    [{type,Anno,bounded_fun,[Ft1,Fc1]}|function_type_list(Fts)];
function_type_list([Ft|Fts]) ->
    [function_type(Ft)|function_type_list(Fts)];
function_type_list([]) -> [].

function_type({type,Anno,'fun',[{type,At,product,As},B]}) ->
    As1 = type_list(As),
    B1 = type(B),
    {type,Anno,'fun',[{type,At,product,As1},B1]}.

function_constraint([C|Cs]) ->
    C1 = constraint(C),
    [C1|function_constraint(Cs)];
function_constraint([]) -> [].

constraint({type,Anno,constraint,[{atom,Annoa,A},[V,T]]}) ->
    V1 = type(V),
    T1 = type(T),
    {type,Anno,constraint,[{atom,Annoa,A},[V1,T1]]}.

type({ann_type,Anno,[{var,Av,V},T]}) ->
    T1 = type(T),
    {ann_type,Anno,[{var,Av,V},T1]};
type({atom,Anno,A}) ->
    {atom,Anno,A};
type({integer,Anno,I}) ->
    {integer,Anno,I};
type({char,Anno,C}) ->
    {char,Anno,C};
type({op,Anno,Op,T}) ->
    T1 = type(T),
    {op,Anno,Op,T1};
type({op,Anno,Op,L,R}) ->
    L1 = type(L),
    R1 = type(R),
    {op,Anno,Op,L1,R1};
type({type,Anno,binary,[M,N]}) ->
    M1 = type(M),
    N1 = type(N),
    {type,Anno,binary,[M1,N1]};
type({type,Anno,'fun',[]}) ->
    {type,Anno,'fun',[]};
type({type,Anno,'fun',[{type,At,any},B]}) ->
    B1 = type(B),
    {type,Anno,'fun',[{type,At,any},B1]};
type({type,Anno,range,[L,H]}) ->
    L1 = type(L),
    H1 = type(H),
    {type,Anno,range,[L1,H1]};
type({type,Anno,map,any}) ->
    {type,Anno,map,any};
type({type,Anno,map,Ps}) ->
    Ps1 = map_pair_types(Ps),
    {type,Anno,map,Ps1};
type({type,Anno,record,[{atom,Aa,N}|Fs]}) ->
    Fs1 = field_types(Fs),
    {type,Anno,record,[{atom,Aa,N}|Fs1]};
type({remote_type,Anno,[{atom,Am,M},{atom,An,N},As]}) ->
    As1 = type_list(As),
    {remote_type,Anno,[{atom,Am,M},{atom,An,N},As1]};
type({type,Anno,tuple,any}) ->
    {type,Anno,tuple,any};
type({type,Anno,tuple,Ts}) ->
    Ts1 = type_list(Ts),
    {type,Anno,tuple,Ts1};
type({type,Anno,union,Ts}) ->
    Ts1 = type_list(Ts),
    {type,Anno,union,Ts1};
type({var,Anno,V}) ->
    {var,Anno,V};
type({user_type,Anno,N,As}) ->
    As1 = type_list(As),
    {user_type,Anno,N,As1};
type({type,Anno,N,As}) ->
    As1 = type_list(As),
    {type,Anno,N,As1}.

map_pair_types([{type,Anno,map_field_assoc,[K,V]}|Ps]) ->
    K1 = type(K),
    V1 = type(V),
    [{type,Anno,map_field_assoc,[K1,V1]}|map_pair_types(Ps)];
map_pair_types([{type,Anno,map_field_exact,[K,V]}|Ps]) ->
    K1 = type(K),
    V1 = type(V),
    [{type,Anno,map_field_exact,[K1,V1]}|map_pair_types(Ps)];
map_pair_types([]) -> [].

field_types([{type,Anno,field_type,[{atom,Aa,A},T]}|Fs]) ->
    T1 = type(T),
    [{type,Anno,field_type,[{atom,Aa,A},T1]}|field_types(Fs)];
field_types([]) -> [].

type_list([T|Ts]) ->
    T1 = type(T),
    [T1|type_list(Ts)];
type_list([]) -> [].
