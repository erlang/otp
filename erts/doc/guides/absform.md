<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# The Abstract Format

This section describes the standard representation of parse trees for Erlang
programs as Erlang terms. This representation is known as the _abstract format_.
Functions dealing with such parse trees are
[`compile:forms/1,2`](`compile:forms/1`) and functions in the following modules:

- `m:epp`
- `m:erl_eval`
- `m:erl_lint`
- `m:erl_parse`
- `m:erl_pp`
- `m:io`

The functions are also used as input and output for parse transforms, see the
`m:compile` module.

We use the function `Rep` to denote the mapping from an Erlang source construct
`C` to its abstract format representation `R`, and write `R = Rep(C)`.

The word `ANNO` in this section represents an annotation, and denotes among
other things the number of the line in the source file where the construction
occurred. See `m:erl_anno` for details. Several instances of `ANNO` in the same
construction can denote different annotations.

As operators are not terms in their own right, when operators are mentioned
below, the representation of an operator is to be taken to be the atom with a
printname consisting of the same characters as the operator.

## Module Declarations and Forms

A module declaration consists of a sequence of forms, which are either function
declarations or attributes.

- If D is a module declaration consisting of the forms `F_1`, ..., `F_k`, then
  Rep(D) = `[Rep(F_1), ..., Rep(F_k)]`.
- If F is an attribute `-export([Fun_1/A_1, ..., Fun_k/A_k])`, then Rep(F) =
  `{attribute,ANNO,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}`.
- If F is an attribute `-import(Mod,[Fun_1/A_1, ..., Fun_k/A_k])`, then Rep(F) =
  `{attribute,ANNO,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}`.
- If F is an attribute `-module(Mod)`, then Rep(F) =
  `{attribute,ANNO,module,Mod}`.
- If F is an attribute `-file(File,Line)`, then Rep(F) =
  `{attribute,ANNO,file,{File,Line}}`.
- If F is a function declaration `Name Fc_1 ; ... ; Name Fc_k`, where each
  `Fc_i` is a function clause with a pattern sequence of the same length
  `Arity`, then Rep(F) =
  `{function,ANNO,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}`.
- If F is a function specification `-Spec Name Ft_1; ...; Ft_k`, where `Spec` is
  either the atom `spec` or the atom `callback`, and each `Ft_i` is a possibly
  constrained function type with an argument sequence of the same length
  `Arity`, then Rep(F) =
  `{attribute,ANNO,Spec,{{Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}`.
- If F is a function specification `-spec Mod:Name Ft_1; ...; Ft_k`, where each
  `Ft_i` is a possibly constrained function type with an argument sequence of
  the same length `Arity`, then Rep(F) =
  `{attribute,ANNO,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}`.
- If F is a record declaration `-record(Name,{V_1, ..., V_k})`, where each `V_i`
  is a record field, then Rep(F) =
  `{attribute,ANNO,record,{Name,[Rep(V_1), ..., Rep(V_k)]}}`. For Rep(V), see
  below.
- If F is a type declaration `-Type Name(V_1, ..., V_k) :: T`, where `Type` is
  either the atom `type` or the atom `opaque`, each `V_i` is a type variable,
  and `T` is a type, then Rep(F) =
  `{attribute,ANNO,Type,{Name,Rep(T),[Rep(V_1), ..., Rep(V_k)]}}`.
- If F is a wild attribute `-A(T)`, then Rep(F) = `{attribute,ANNO,A,T}`.

### Record Fields

Each field in a record declaration can have an optional, explicit, default
initializer expression, and an optional type.

- If V is `A`, then Rep(V) = `{record_field,ANNO,Rep(A)}`.
- If V is `A = E`, where `E` is an expression, then Rep(V) =
  `{record_field,ANNO,Rep(A),Rep(E)}`.
- If V is `A :: T`, where `T` is a type, then Rep(V) =
  `{typed_record_field,{record_field,ANNO,Rep(A)},Rep(T)}`.
- If V is `A = E :: T`, where `E` is an expression and `T` is a type, then
  Rep(V) = `{typed_record_field,{record_field,ANNO,Rep(A),Rep(E)},Rep(T)}`.

### Representation of Parse Errors and End-of-File

In addition to the representations of forms, the list that represents a module
declaration (as returned by functions in `m:epp` and `m:erl_parse`) can contain
the following:

- Tuples `{error,E}` and `{warning,W}`, denoting syntactically incorrect forms
  and warnings.
- `{eof,LOCATION}`, denoting an end-of-stream encountered before a complete form
  had been parsed. The word `LOCATION` represents a location, and denotes the
  number of the last line, and possibly the number of the last column on that
  line, in the source file. See `m:erl_anno` for details.

See [`the form_info/0`](`t:erl_parse:form_info/0`) type in `m:erl_parse` for
more details about these values.

## Atomic Literals

There are five kinds of atomic literals, which are represented in the same way
in patterns, expressions, and guards:

- If L is an atom literal, then Rep(L) = `{atom,ANNO,L}`.
- If L is a character literal, then Rep(L) = `{char,ANNO,L}`.
- If L is a float literal, then Rep(L) = `{float,ANNO,L}`.
- If L is an integer literal, then Rep(L) = `{integer,ANNO,L}`.
- If L is a string literal consisting of the characters `C_1`, ..., `C_k`, then
  Rep(L) = `{string,ANNO,[C_1, ..., C_k]}`.

Notice that negative integer and float literals do not occur as such; they are
parsed as an application of the unary negation operator.

## Patterns

If Ps is a sequence of patterns `P_1, ..., P_k`, then Rep(Ps) =
`[Rep(P_1), ..., Rep(P_k)]`. Such sequences occur as the list of arguments to a
function or fun.

Individual patterns are represented as follows:

- If P is an atomic literal `L`, then Rep(P) = Rep(L).
- If P is a bitstring pattern `<<P_1:Size_1/TSL_1, ..., P_k:Size_k/TSL_k>>`,
  where each `Size_i` is an expression that can be evaluated to an integer, and
  each `TSL_i` is a type specificer list, then Rep(P) =
  `{bin,ANNO,[{bin_element,ANNO,Rep(P_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}`.
  For Rep(TSL), see below. An omitted `Size_i` is represented by `default`. An
  omitted `TSL_i` is represented by `default`.
- If P is a compound pattern `P_1 = P_2`, then Rep(P) =
  `{match,ANNO,Rep(P_1),Rep(P_2)}`.
- If P is a cons pattern `[P_h | P_t]`, then Rep(P) =
  `{cons,ANNO,Rep(P_h),Rep(P_t)}`.
- If P is a map pattern `#{A_1, ..., A_k}`, where each `A_i` is an association
  `P_i_1 := P_i_2`, then Rep(P) = `{map,ANNO,[Rep(A_1), ..., Rep(A_k)]}`. For
  Rep(A), see below.
- If P is a nil pattern `[]`, then Rep(P) = `{nil,ANNO}`.
- If P is an operator pattern `P_1 Op P_2`, where `Op` is a binary operator
  (this is either an occurrence of `++` applied to a literal string or character
  list, or an occurrence of an expression that can be evaluated to a number at
  compile time), then Rep(P) = `{op,ANNO,Op,Rep(P_1),Rep(P_2)}`.
- If P is an operator pattern `Op P_0`, where `Op` is a unary operator (this is
  an occurrence of an expression that can be evaluated to a number at compile
  time), then Rep(P) = `{op,ANNO,Op,Rep(P_0)}`.
- If P is a parenthesized pattern `( P_0 )`, then Rep(P) = `Rep(P_0)`, that is,
  parenthesized patterns cannot be distinguished from their bodies.
- If P is a record field index pattern `#Name.Field`, where `Field` is an atom,
  then Rep(P) = `{record_index,ANNO,Name,Rep(Field)}`.
- If P is a record pattern `#Name{Field_1=P_1, ..., Field_k=P_k}`, where each
  `Field_i` is an atom or `_`, then Rep(P) =
  `{record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(P_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(P_k)}]}`.
- If P is a tuple pattern `{P_1, ..., P_k}`, then Rep(P) =
  `{tuple,ANNO,[Rep(P_1), ..., Rep(P_k)]}`.
- If P is a universal pattern `_`, then Rep(P) = `{var,ANNO,'_'}`.
- If P is a variable pattern `V`, then Rep(P) = `{var,ANNO,A}`, where A is an
  atom with a printname consisting of the same characters as `V`.

Notice that every pattern has the same source form as some expression, and is
represented in the same way as the corresponding expression.

## Expressions

A body B is a non-empty sequence of expressions `E_1, ..., E_k`, and Rep(B) =
`[Rep(E_1), ..., Rep(E_k)]`.

An expression E is one of the following:

- If E is an atomic literal `L`, then Rep(E) = Rep(L).
- If E is a bitstring comprehension `<<E_0 || Q_1, ..., Q_k>>`, where each `Q_i`
  is a qualifier, then Rep(E) = `{bc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}`.
  For Rep(Q), see below.
- If E is a bitstring constructor `<<E_1:Size_1/TSL_1, ..., E_k:Size_k/TSL_k>>`,
  where each `Size_i` is an expression and each `TSL_i` is a type specificer
  list, then Rep(E) =
  `{bin,ANNO,[{bin_element,ANNO,Rep(E_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(E_k),Rep(Size_k),Rep(TSL_k)}]}`.
  For Rep(TSL), see below. An omitted `Size_i` is represented by `default`. An
  omitted `TSL_i` is represented by `default`.
- If E is a block expression `begin B end`, where `B` is a body, then Rep(E) =
  `{block,ANNO,Rep(B)}`.
- If E is a case expression `case E_0 of Cc_1 ; ... ; Cc_k end`, where `E_0` is
  an expression and each `Cc_i` is a case clause, then Rep(E) =
  `{'case',ANNO,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}`.
- If E is a catch expression `catch E_0`, then Rep(E) =
  `{'catch',ANNO,Rep(E_0)}`.
- If E is a cons skeleton `[E_h | E_t]`, then Rep(E) =
  `{cons,ANNO,Rep(E_h),Rep(E_t)}`.
- If E is a fun expression `fun Name/Arity`, then Rep(E) =
  `{'fun',ANNO,{function,Name,Arity}}`.
- If E is a fun expression `fun Module:Name/Arity`, then Rep(E) =
  `{'fun',ANNO,{function,Rep(Module),Rep(Name),Rep(Arity)}}`.
- If E is a fun expression `fun Fc_1 ; ... ; Fc_k end`, where each `Fc_i` is a
  function clause, then Rep(E) =
  `{'fun',ANNO,{clauses,[Rep(Fc_1), ..., Rep(Fc_k)]}}`.
- If E is a fun expression `fun Name Fc_1 ; ... ; Name Fc_k end`, where `Name`
  is a variable and each `Fc_i` is a function clause, then Rep(E) =
  `{named_fun,ANNO,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}`.
- If E is a function call `E_0(E_1, ..., E_k)`, then Rep(E) =
  `{call,ANNO,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}`.
- If E is a function call `E_m:E_0(E_1, ..., E_k)`, then Rep(E) =
  `{call,ANNO,{remote,ANNO,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}`.
- If E is an if expression `if Ic_1 ; ... ; Ic_k end`, where each `Ic_i` is an
  if clause, then Rep(E) = `{'if',ANNO,[Rep(Ic_1), ..., Rep(Ic_k)]}`.
- If E is a list comprehension `[E_0 || Q_1, ..., Q_k]`, where each `Q_i` is a
  qualifier, then Rep(E) = `{lc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}`. For
  Rep(Q), see below.
- If E is a map comprehension `#{E_0 || Q_1, ..., Q_k}`, where `E_0` is an
  association `K => V` and each `Q_i` is a qualifier, then Rep(E) =
  `{mc,ANNO,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}`. For Rep(E_0) and Rep(Q), see
  below.
- If E is a map creation `#{A_1, ..., A_k}`, where each `A_i` is an association
  `E_i_1 => E_i_2`, then Rep(E) = `{map,ANNO,[Rep(A_1), ..., Rep(A_k)]}`. For
  Rep(A), see below.
- If E is a map update `E_0#{A_1, ..., A_k}`, where each `A_i` is an association
  `E_i_1 => E_i_2` or `E_i_1 := E_i_2`, then Rep(E) =
  `{map,ANNO,Rep(E_0),[Rep(A_1), ..., Rep(A_k)]}`. For Rep(A), see below.
- If E is a match operator expression `P = E_0`, where `P` is a pattern, then
  Rep(E) = `{match,ANNO,Rep(P),Rep(E_0)}`.
- If E is a conditional match operator expression `P ?= E_0`, where `P` is a
  pattern, then Rep(E) = `{maybe_match,ANNO,Rep(P),Rep(E_0)}`.
- If E is a maybe expression `maybe B end`, where `B` is a body then Rep(E) =
  `{'maybe',ANNO,Rep(B)}`.
- If E is a maybe expression `maybe B else Ec_1 ; ... ; Ec_k end`, where `B` is
  a body and each `Ec_i` is an else clause then Rep(E) =
  `{'maybe',ANNO,Rep(B),{'else',ANNO,[Rep(Ec_1), ..., Rep(Ec_k)]}}`.
- If E is nil, `[]`, then Rep(E) = `{nil,ANNO}`.
- If E is an operator expression `E_1 Op E_2`, where `Op` is a binary operator
  other than match operator `=`, then Rep(E) = `{op,ANNO,Op,Rep(E_1),Rep(E_2)}`.
- If E is an operator expression `Op E_0`, where `Op` is a unary operator, then
  Rep(E) = `{op,ANNO,Op,Rep(E_0)}`.
- If E is a parenthesized expression `( E_0 )`, then Rep(E) = `Rep(E_0)`, that
  is, parenthesized expressions cannot be distinguished from their bodies.
- If E is a receive expression `receive Cc_1 ; ... ; Cc_k end`, where each
  `Cc_i` is a case clause, then Rep(E) =
  `{'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)]}`.
- If E is a receive expression `receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end`,
  where each `Cc_i` is a case clause, `E_0` is an expression, and `B_t` is a
  body, then Rep(E) =
  `{'receive',ANNO,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}`.
- If E is a record creation `#Name{Field_1=E_1, ..., Field_k=E_k}`, where each
  `Field_i` is an atom or `_`, then Rep(E) =
  `{record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]}`.
- If E is a record field access `E_0#Name.Field`, where `Field` is an atom, then
  Rep(E) = `{record_field,ANNO,Rep(E_0),Name,Rep(Field)}`.
- If E is a record field index `#Name.Field`, where `Field` is an atom, then
  Rep(E) = `{record_index,ANNO,Name,Rep(Field)}`.
- If E is a record update `E_0#Name{Field_1=E_1, ..., Field_k=E_k}`, where each
  `Field_i` is an atom, then Rep(E) =
  `{record,ANNO,Rep(E_0),Name,[{record_field,ANNO,Rep(Field_1),Rep(E_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(E_k)}]}`.
- If E is a tuple skeleton `{E_1, ..., E_k}`, then Rep(E) =
  `{tuple,ANNO,[Rep(E_1), ..., Rep(E_k)]}`.
- If E is a try expression `try B catch Tc_1 ; ... ; Tc_k end`, where `B` is a
  body and each `Tc_i` is a catch clause, then Rep(E) =
  `{'try',ANNO,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],[]}`.
- If E is a try expression
  `try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n end`, where `B` is a body,
  each `Cc_i` is a case clause, and each `Tc_j` is a catch clause, then Rep(E) =
  `{'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]}`.
- If E is a try expression `try B after A end`, where `B` and `A` are bodies,
  then Rep(E) = `{'try',ANNO,Rep(B),[],[],Rep(A)}`.
- If E is a try expression `try B of Cc_1 ; ... ; Cc_k after A end`, where `B`
  and `A` are a bodies, and each `Cc_i` is a case clause, then Rep(E) =
  `{'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[],Rep(A)}`.
- If E is a try expression `try B catch Tc_1 ; ... ; Tc_k after A end`, where
  `B` and `A` are bodies, and each `Tc_i` is a catch clause, then Rep(E) =
  `{'try',ANNO,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],Rep(A)}`.
- If E is a try expression
  `try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n after A end`, where `B`
  and `A` are a bodies, each `Cc_i` is a case clause, and each `Tc_j` is a catch
  clause, then Rep(E) =
  `{'try',ANNO,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)}`.
- If E is a variable `V`, then Rep(E) = `{var,ANNO,A}`, where `A` is an atom
  with a printname consisting of the same characters as `V`.

### Qualifiers

A qualifier Q is one of the following:

- If Q is a filter `E`, where `E` is an expression, then Rep(Q) = `Rep(E)`.
- If Q is a list generator `P <- E`, where `P` is a pattern and `E` is an
  expression, then Rep(Q) = `{generate,ANNO,Rep(P),Rep(E)}`.
- If Q is a bitstring generator `P <= E`, where `P` is a pattern and `E` is an
  expression, then Rep(Q) = `{b_generate,ANNO,Rep(P),Rep(E)}`.
- If Q is a map generator `P <- E`, where `P` is an association pattern
  `P_1 := P_2` and `E` is an expression, then Rep(Q) =
  `{m_generate,ANNO,Rep(P),Rep(E)}`. For Rep(P), see below.

### Bitstring Element Type Specifiers

A type specifier list TSL for a bitstring element is a sequence of type
specifiers `TS_1 - ... - TS_k`, and Rep(TSL) = `[Rep(TS_1), ..., Rep(TS_k)]`.

- If TS is a type specifier `A`, where `A` is an atom, then Rep(TS) = `A`.
- If TS is a type specifier `A:Value`, where `A` is an atom and `Value` is an
  integer, then Rep(TS) = `{A,Value}`.

### Associations

An association A is one of the following:

- If A is an association `K => V`, then Rep(A) =
  `{map_field_assoc,ANNO,Rep(K),Rep(V)}`.
- If A is an association `K := V`, then Rep(A) =
  `{map_field_exact,ANNO,Rep(K),Rep(V)}`.

## Clauses

There are function clauses, if clauses, case clauses, and catch clauses.

A clause C is one of the following:

- If C is a case clause `P -> B`, where `P` is a pattern and `B` is a body, then
  Rep(C) = `{clause,ANNO,[Rep(P)],[],Rep(B)}`.
- If C is a case clause `P when Gs -> B`, where `P` is a pattern, `Gs` is a
  guard sequence, and `B` is a body, then Rep(C) =
  `{clause,ANNO,[Rep(P)],Rep(Gs),Rep(B)}`.
- If C is a catch clause `P -> B`, where `P` is a pattern and `B` is a body,
  then Rep(C) = `{clause,ANNO,[Rep({throw,P,_})],[],Rep(B)}`, that is, a catch
  clause with an explicit exception class `throw` and with or without an
  explicit stacktrace variable `_` cannot be distinguished from a catch clause
  without an explicit exception class and without an explicit stacktrace
  variable.
- If C is a catch clause `X : P -> B`, where `X` is an atomic literal or a
  variable pattern, `P` is a pattern, and `B` is a body, then Rep(C) =
  `{clause,ANNO,[Rep({X,P,_})],[],Rep(B)}`, that is, a catch clause with an
  explicit exception class and with an explicit stacktrace variable `_` cannot
  be distinguished from a catch clause with an explicit exception class and
  without an explicit stacktrace variable.
- If C is a catch clause `X : P : S -> B`, where `X` is an atomic literal or a
  variable pattern, `P` is a pattern, `S` is a variable, and `B` is a body, then
  Rep(C) = `{clause,ANNO,[Rep({X,P,S})],[],Rep(B)}`.
- If C is a catch clause `P when Gs -> B`, where `P` is a pattern, `Gs` is a
  guard sequence, and `B` is a body, then Rep(C) =
  `{clause,ANNO,[Rep({throw,P,_})],Rep(Gs),Rep(B)}`, that is, a catch clause
  with an explicit exception class `throw` and with or without an explicit
  stacktrace variable `_` cannot be distinguished from a catch clause without an
  explicit exception class and without an explicit stacktrace variable.
- If C is a catch clause `X : P when Gs -> B`, where `X` is an atomic literal or
  a variable pattern, `P` is a pattern, `Gs` is a guard sequence, and `B` is a
  body, then Rep(C) = `{clause,ANNO,[Rep({X,P,_})],Rep(Gs),Rep(B)}`, that is, a
  catch clause with an explicit exception class and with an explicit stacktrace
  variable `_` cannot be distinguished from a catch clause with an explicit
  exception class and without an explicit stacktrace variable.
- If C is a catch clause `X : P : S when Gs -> B`, where `X` is an atomic
  literal or a variable pattern, `P` is a pattern, `Gs` is a guard sequence, `S`
  is a variable, and `B` is a body, then Rep(C) =
  `{clause,ANNO,[Rep({X,P,S})],Rep(Gs),Rep(B)}`.
- If C is a function clause `( Ps ) -> B`, where `Ps` is a pattern sequence and
  `B` is a body, then Rep(C) = `{clause,ANNO,Rep(Ps),[],Rep(B)}`.
- If C is a function clause `( Ps ) when Gs -> B`, where `Ps` is a pattern
  sequence, `Gs` is a guard sequence and `B` is a body, then Rep(C) =
  `{clause,ANNO,Rep(Ps),Rep(Gs),Rep(B)}`.
- If C is an if clause `Gs -> B`, where `Gs` is a guard sequence and `B` is a
  body, then Rep(C) = `{clause,ANNO,[],Rep(Gs),Rep(B)}`.

## Guards

A guard sequence Gs is a sequence of guards `G_1; ...; G_k`, and Rep(Gs) =
`[Rep(G_1), ..., Rep(G_k)]`. If the guard sequence is empty, then Rep(Gs) =
`[]`.

A guard G is a non-empty sequence of guard tests `Gt_1, ..., Gt_k`, and Rep(G) =
`[Rep(Gt_1), ..., Rep(Gt_k)]`.

A guard test Gt is one of the following:

- If Gt is an atomic literal `L`, then Rep(Gt) = Rep(L).
- If Gt is a bitstring constructor
  `<<Gt_1:Size_1/TSL_1, ..., Gt_k:Size_k/TSL_k>>`, where each `Size_i` is a
  guard test and each `TSL_i` is a type specificer list, then Rep(Gt) =
  `{bin,ANNO,[{bin_element,ANNO,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ..., {bin_element,ANNO,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}`.
  For Rep(TSL), see above. An omitted `Size_i` is represented by `default`. An
  omitted `TSL_i` is represented by `default`.
- If Gt is a cons skeleton `[Gt_h | Gt_t]`, then Rep(Gt) =
  `{cons,ANNO,Rep(Gt_h),Rep(Gt_t)}`.
- If Gt is a function call `A(Gt_1, ..., Gt_k)`, where `A` is an atom, then
  Rep(Gt) = `{call,ANNO,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}`.
- If Gt is a function call `A_m:A(Gt_1, ..., Gt_k)`, where `A_m` is the atom
  `erlang` and `A` is an atom or an operator, then Rep(Gt) =
  `{call,ANNO,{remote,ANNO,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}`.
- If Gt is a map creation `#{A_1, ..., A_k}`, where each `A_i` is an association
  `Gt_i_1 => Gt_i_2`, then Rep(Gt) = `{map,ANNO,[Rep(A_1), ..., Rep(A_k)]}`. For
  Rep(A), see above.
- If Gt is a map update `Gt_0#{A_1, ..., A_k}`, where each `A_i` is an
  association `Gt_i_1 => Gt_i_2` or `Gt_i_1 := Gt_i_2`, then Rep(Gt) =
  `{map,ANNO,Rep(Gt_0),[Rep(A_1), ..., Rep(A_k)]}`. For Rep(A), see above.
- If Gt is nil, `[]`, then Rep(Gt) = `{nil,ANNO}`.
- If Gt is an operator guard test `Gt_1 Op Gt_2`, where `Op` is a binary
  operator other than match operator `=`, then Rep(Gt) =
  `{op,ANNO,Op,Rep(Gt_1),Rep(Gt_2)}`.
- If Gt is an operator guard test `Op Gt_0`, where `Op` is a unary operator,
  then Rep(Gt) = `{op,ANNO,Op,Rep(Gt_0)}`.
- If Gt is a parenthesized guard test `( Gt_0 )`, then Rep(Gt) = `Rep(Gt_0)`,
  that is, parenthesized guard tests cannot be distinguished from their bodies.
- If Gt is a record creation `#Name{Field_1=Gt_1, ..., Field_k=Gt_k}`, where
  each `Field_i` is an atom or `_`, then Rep(Gt) =
  `{record,ANNO,Name,[{record_field,ANNO,Rep(Field_1),Rep(Gt_1)}, ..., {record_field,ANNO,Rep(Field_k),Rep(Gt_k)}]}`.
- If Gt is a record field access `Gt_0#Name.Field`, where `Field` is an atom,
  then Rep(Gt) = `{record_field,ANNO,Rep(Gt_0),Name,Rep(Field)}`.
- If Gt is a record field index `#Name.Field`, where `Field` is an atom, then
  Rep(Gt) = `{record_index,ANNO,Name,Rep(Field)}`.
- If Gt is a tuple skeleton `{Gt_1, ..., Gt_k}`, then Rep(Gt) =
  `{tuple,ANNO,[Rep(Gt_1), ..., Rep(Gt_k)]}`.
- If Gt is a variable pattern `V`, then Rep(Gt) = `{var,ANNO,A}`, where A is an
  atom with a printname consisting of the same characters as `V`.

Notice that every guard test has the same source form as some expression, and is
represented in the same way as the corresponding expression.

## Types

- If T is an annotated type `A :: T_0`, where `A` is a variable, then Rep(T) =
  `{ann_type,ANNO,[Rep(A),Rep(T_0)]}`.
- If T is an atom, a character, or an integer literal L, then Rep(T) = Rep(L).
- If T is a bitstring type `<<_:M,_:_*N>>`, where `M` and `N` are singleton
  integer types, then Rep(T) = `{type,ANNO,binary,[Rep(M),Rep(N)]}`.
- If T is the empty list type `[]`, then Rep(T) = `{type,ANNO,nil,[]}`, that is,
  the empty list type `[]` cannot be distinguished from the predefined type
  `t:nil/0`.
- If T is a fun type `fun()`, then Rep(T) = `{type,ANNO,'fun',[]}`.
- If T is a fun type `fun((...) -> T_0)`, then Rep(T) =
  `{type,ANNO,'fun',[{type,ANNO,any},Rep(T_0)]}`.
- If T is a fun type `fun(Ft)`, where `Ft` is a function type, then Rep(T) =
  `Rep(Ft)`. For Rep(Ft), see below.
- If T is an integer range type `L .. H`, where `L` and `H` are singleton
  integer types, then Rep(T) = `{type,ANNO,range,[Rep(L),Rep(H)]}`.
- If T is a map type `t:map/0`, then Rep(T) = `{type,ANNO,map,any}`.
- If T is a map type `#{A_1, ..., A_k}`, where each `A_i` is an association
  type, then Rep(T) = `{type,ANNO,map,[Rep(A_1), ..., Rep(A_k)]}`. For Rep(A),
  see below.
- If T is an operator type `T_1 Op T_2`, where `Op` is a binary operator (this
  is an occurrence of an expression that can be evaluated to an integer at
  compile time), then Rep(T) = `{op,ANNO,Op,Rep(T_1),Rep(T_2)}`.
- If T is an operator type `Op T_0`, where `Op` is a unary operator (this is an
  occurrence of an expression that can be evaluated to an integer at compile
  time), then Rep(T) = `{op,ANNO,Op,Rep(T_0)}`.
- If T is `( T_0 )`, then Rep(T) = `Rep(T_0)`, that is, parenthesized types
  cannot be distinguished from their bodies.
- If T is a predefined (or built-in) type `N(T_1, ..., T_k)`, then Rep(T) =
  `{type,ANNO,N,[Rep(T_1), ..., Rep(T_k)]}`.
- If T is a record type `#Name{F_1, ..., F_k}`, where each `F_i` is a record
  field type, then Rep(T) =
  `{type,ANNO,record,[Rep(Name),Rep(F_1), ..., Rep(F_k)]}`. For Rep(F), see
  below.
- If T is a remote type `M:N(T_1, ..., T_k)`, then Rep(T) =
  `{remote_type,ANNO,[Rep(M),Rep(N),[Rep(T_1), ..., Rep(T_k)]]}`.
- If T is a tuple type `t:tuple/0`, then Rep(T) = `{type,ANNO,tuple,any}`.
- If T is a tuple type `{T_1, ..., T_k}`, then Rep(T) =
  `{type,ANNO,tuple,[Rep(T_1), ..., Rep(T_k)]}`.
- If T is a type union `T_1 | ... | T_k`, then Rep(T) =
  `{type,ANNO,union,[Rep(T_1), ..., Rep(T_k)]}`.
- If T is a type variable `V`, then Rep(T) = `{var,ANNO,A}`, where `A` is an
  atom with a printname consisting of the same characters as `V`. A type
  variable is any variable except underscore (`_`).
- If T is a user-defined type `N(T_1, ..., T_k)`, then Rep(T) =
  `{user_type,ANNO,N,[Rep(T_1), ..., Rep(T_k)]}`.

### Function Types

A function type Ft is one of the following:

- If Ft is a constrained function type `Ft_1 when Fc`, where `Ft_1` is a
  function type and `Fc` is a function constraint, then Rep(T) =
  `{type,ANNO,bounded_fun,[Rep(Ft_1),Rep(Fc)]}`. For Rep(Fc), see below.
- If Ft is a function type `(T_1, ..., T_n) -> T_0`, where each `T_i` is a type,
  then Rep(Ft) =
  `{type,ANNO,'fun',[{type,ANNO,product,[Rep(T_1), ..., Rep(T_n)]},Rep(T_0)]}`.

### Function Constraints

A function constraint Fc is a non-empty sequence of constraints `C_1, ..., C_k`,
and Rep(Fc) = `[Rep(C_1), ..., Rep(C_k)]`.

- If C is a constraint `V :: T`, where `V` is a type variable and `T` is a type,
  then Rep(C) =
  `{type,ANNO,constraint,[{atom,ANNO,is_subtype},[Rep(V),Rep(T)]]}`.

### Association Types

- If A is an association type `K => V`, where `K` and `V` are types, then Rep(A)
  = `{type,ANNO,map_field_assoc,[Rep(K),Rep(V)]}`.
- If A is an association type `K := V`, where `K` and `V` are types, then Rep(A)
  = `{type,ANNO,map_field_exact,[Rep(K),Rep(V)]}`.

### Record Field Types

- If F is a record field type `Name :: Type`, where `Type` is a type, then
  Rep(F) = `{type,ANNO,field_type,[Rep(Name),Rep(Type)]}`.

## The Abstract Format after Preprocessing

The compilation option `debug_info` can be specified to the compiler to have the
abstract code stored in the `abstract_code` chunk in the Beam file (for
debugging purposes).

As from Erlang/OTP R9C, the `abstract_code` chunk contains
`{raw_abstract_v1,AbstractCode}`, where `AbstractCode` is the abstract code as
described in this section.

In OTP releases before R9C, the abstract code after some more processing was
stored in the Beam file. The first element of the tuple would be either
`abstract_v1` (in OTP R7B) or `abstract_v2` (in OTP R8B).
