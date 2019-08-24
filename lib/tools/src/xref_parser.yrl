%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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

Nonterminals 
xref statements statement expr constants constant const
assign_op prefix_op add_op mult_op count_op restr_op path_op cast_op 
regexp regatom regint regvar regstr
variable id type.

Terminals 
edge vertex var atom decl cast 'of' string integer
'(' ')' '[' ']' ',' '+' '-' '*' '|' '||' '|||' '=' ':=' '#' '{' '}' ':' '/'.

Rootsymbol xref.

Endsymbol '$end'.

xref -> statements : '$1'.

assign_op -> '=' : tmp.
assign_op -> ':=' : user.
add_op    -> '+' : union.
add_op    -> '-' : difference.
mult_op   -> '*' : intersection.
count_op  -> '#' : '#'.
restr_op  -> '|' : '|'.
restr_op  -> '||' : '||'.
restr_op  -> '|||' : '|||'.
path_op   -> 'of' : 'of'.
cast_op   -> '(' cast ')' : value_of('$2').
prefix_op -> id : '$1'.

Left  200 add_op.
Left  300 mult_op.
Left  400 count_op.
Left  500 restr_op.
Left  600 path_op.
Unary 700 cast_op.
Unary 700 prefix_op.

statements -> statement : ['$1'].
statements -> expr : ['$1'].
statements -> statement ',' statements : ['$1' | '$3'].

statement -> variable assign_op expr : {assign, '$2', '$1', '$3'}.

expr -> '[' constant constants ']' type : type({list, ['$2' | '$3']}, '$5').
expr -> '{' constant constants '}' type : type({tuple, ['$2' | '$3']}, '$5').
expr -> constant type : type('$1', '$2').
expr -> variable : {variable, '$1'}.
expr -> expr add_op expr : {set, '$2', '$1', '$3'}.
expr -> expr mult_op expr : {set, '$2', '$1', '$3'}.
expr -> count_op expr : prefix('$1', '$2').
expr -> expr restr_op expr : {restr, '$2', '$1', '$3'}.
expr -> expr path_op expr : {path, '$1', '$3'}.
expr -> cast_op expr : {type, {convert, '$1'}, '$2'}.
expr -> prefix_op expr : prefix('$1', '$2').
expr -> regexp : '$1'.
expr -> '(' expr ')' : '$2'.

constants -> '$empty' : [].
constants -> ',' constant constants : ['$2' | '$3'].

constant -> const : '$1'.
    
const -> id : {constant, unknown, vertex, '$1'}.
const -> edge : value_of('$1').
const -> vertex : value_of('$1').

regexp -> regstr type : regexp(atom, '$1', '$2').
regexp -> regatom ':' regatom '/' regint type : 
                     regexp(func, {'$1', '$3', '$5'}, '$6').

regatom -> regstr : '$1'.
regatom -> id : {atom, '$1'}.
regatom -> regvar : '$1'.

regint -> regstr : '$1'.
regint -> integer : {integer, value_of('$1')}.
regint -> regvar : '$1'.

regstr -> string : check_regexp(value_of('$1')).
regvar -> variable : check_regexp_variable('$1').

id       -> atom    : value_of('$1').
variable -> var     : value_of('$1').

type     -> decl     : value_of('$1').
type     -> '$empty' : unknown.

Erlang code.

-export([t2s/1]).

-import(lists, [concat/1, flatten/1]).

%%% Syntax of the parse tree:
%%% Start = [Statement]
%%% Statement = {assign, AOp, VarName, Expr}
%%%           | Expr
%%% AOp = tmp | user
%%% Expr = Constants | Variable | Unary | Binary | RegExpr
%%% Constants = {list, [Constant]}  % not empty list
%%%           | {tuple, [Constant]}
%%%           | Constant % only to avoid [ and ] in error messages...
%%% Constant = {constant, 'Fun', vertex, MFA} | 
%%%            {constant, AtomType, vertex, atom()} |
%%%            {constant, 'Fun', edge, {MFA, MFA}} | 
%%%            {constant, AtomType, edge, {atom(), atom()}}
%%% Variable = {variable, VarName}
%%% VarName = atom()
%%% Unary = {set, SetUOp, Expr} 
%%%       | {graph, GraphUOp, Expr}
%%%       | {type, {TypeOp, Type}, Expr}
%%%       | {numeric, NumOp, Expr, Expr}
%%% SetUOp = range | domain | weak | strict
%%% GraphUOp = components | condensation | closure
%%% Binary = {set, SetBOp, Expr, Expr}
%%%        | {restr, RestrOp, Expr, Expr}
%%%        | {path, Expr, Expr}
%%% SetBOp = union | intersection | difference
%%% RestrOp = '|' | '||' | '|||'
%%% TypeOp = type | convert
%%% NumOp = '#'
%%% RegExpr = {regexpr, RExpr, Type}
%%% RExpr = string() | {AtomReg, AtomReg, IntReg}
%%% AtomReg = string() | atom() | variable()
%%% IntReg = string() | integer()
%%% MFA = {atom(), atom(), integer()}
%%% Type = 'Rel' | 'App' | 'Mod' | 'Fun'
%%%      | 'Lin' | 'LLin' | 'XLin' | 'ELin' | 'XXL'
%%% AtomType = unknown | 'Rel' | 'App' | 'Mod'

value_of(Token) ->
    element(3, Token).

prefix(Op, Expr) ->
    case is_prefix_op(Op) of
	false ->
	    return_error(0, ["invalid_operator", Op]);
	UOp ->
	    {UOp, Op, Expr}
    end.

is_prefix_op(range) -> set;
is_prefix_op(domain) -> set;
is_prefix_op(weak) -> set;
is_prefix_op(strict) -> set;
is_prefix_op(components) -> graph;
is_prefix_op(condensation) -> graph;
is_prefix_op(closure) -> graph;
is_prefix_op('#') -> numeric;
is_prefix_op(_) -> false.

check_regexp(String) ->
    case re:compile(String, [unicode]) of
	{ok, _Expr} ->
	    {regexpr, String};
	{error, {ErrString, Position}} ->
	    return_error(Position, ["invalid_regexp", String, ErrString])
    end.

check_regexp_variable('_') ->
    variable;
check_regexp_variable(Var) ->
    return_error(0, ["invalid_regexp_variable", Var]).

regexp(func, RExpr, unknown) ->
    {regexpr, RExpr, 'Fun'};
regexp(_, RExpr, unknown) ->
    return_error(0, ["missing_type", t2s({regexpr, RExpr, unknown})]);
regexp(Kind, RExpr, Type) ->
    E = {type, {type, Type}, {regexpr, RExpr, Type}},
    case Type of
	'Fun' when Kind =:= func -> E;
	'Mod' when Kind =:= atom -> E;
	'App' when Kind =:= atom -> E;
	'Rel' when Kind =:= atom -> E;
	_Else -> return_error(0, ["type_mismatch", t2s(E)])
    end.

type(Expr, unknown) ->
    Expr;
type(Expr, Type) ->
    {type, {type, Type}, type_constants(Expr, Type, Expr)}.

type_constants({list, L}, Type, E) ->
    {list, type_constants(L, Type, E)};
type_constants({tuple, L}, Type, E) ->
    {tuple, type_constants(L, Type, E)};
type_constants([C | Cs], Type, E) ->
    [type_constants(C, Type, E) | type_constants(Cs, Type, E)];
type_constants([], _Type, _E) ->
    [];
type_constants({constant, unknown, OType, Con}, 'Rel', _E) ->
    {constant, 'Rel', OType, Con};
type_constants({constant, unknown, OType, Con}, 'App', _E) ->
    {constant, 'App', OType, Con};
type_constants({constant, unknown, OType, Con}, 'Mod', _E) ->
    {constant, 'Mod', OType, Con};
type_constants(C={constant, Type, _OType, _Con}, Type, _E) ->
    C;
type_constants(_C, Type, E) ->
    return_error(0, ["type_mismatch", t2s({type, {type, Type}, E})]).

t2s(T) ->
    concat(flatten(e2s(T, 0))).

%% Does not handle list of statements.
e2s({assign, VarType, Name, E}, P) ->
    [left(P, 100), Name, name_it(VarType), e2s(E, 100), right(P, 100)];
e2s({constant, 'Fun', vertex, MFA}, _P) ->
    mfa2s(MFA);
e2s({constant, _Type, vertex, A}, _P) ->
    [c2s(A)];
e2s({constant, 'Fun', edge, {MFA1,MFA2}}, _P) ->
    [mfa2s(MFA1),' -> ',mfa2s(MFA2)];
e2s({constant, _Type, edge, {A1,A2}}, _P) ->
    [c2s(A1),' -> ',c2s(A2)];
e2s({variable, Name}, _P) ->
    [Name];
e2s({list, E}, _P) ->
    ['[', e2s(E, 0), ']'];
e2s({tuple, E}, _P) ->
    ['{', e2s(E, 0), '}'];
e2s({type, {convert, Type}, E}, P) ->
    [left(P, 700), '(',Type,') ', e2s(E, 700), right(P, 700)];
e2s({type, {type, Type}, E}, P) ->
    [left(P, 700), e2s(E, 700), ' : ', Type, right(P, 700)];
e2s({set, Op, E}, P) ->
    [left(P, 700), name_it(Op), ' ', e2s(E, 700), right(P, 700)];
e2s({graph, Op, E}, P) ->
    [left(P, 700), name_it(Op), ' ', e2s(E, 700), right(P, 700)];
e2s({numeric, Op, E}, P) ->
    [left(P, 400), name_it(Op), ' ', e2s(E, 400), right(P, 400)];
e2s({set, Op, E1, E2}, P) ->
    P1 = prio(Op),
    [left(P, P1), e2s(E1, P1),name_it(Op),e2s(E2, P1+50), right(P, P1)];
e2s({path, E1, E2}, P) ->
    P1 = 600,
    [left(P, P1), e2s(E1, P1),' of ',e2s(E2, P1+50), right(P, P1)];
e2s({regexpr, Expr={regexpr,_}, _Type}, _P) ->
    [re(Expr)];
e2s({regexpr, {M,F,A}, _Type}, _P) ->
    [re(M),':',re(F),'/', re(A)];
e2s({restr, Op, E1, E2}, P) ->
    P1 = 500,
    [left(P, P1), e2s(E1, P1),name_it(Op),e2s(E2, P1+50), right(P, P1)];
e2s([], _P) ->
    [];
e2s([E], P) ->
    e2s(E, P);
e2s([E | Es], P) ->
    [e2s(E, P),', ',e2s(Es, P)].

mfa2s({M,F,A}) ->
    [c2s(M),':',c2s(F),'/',A].

c2s(C) ->
    [S] = io_lib:format("~tp", [C]),
    list_to_atom(S).

re(variable) -> ['_'];
re({atom, Atom}) -> [Atom];
re({integer, Int}) -> [Int];
re({regexpr, Str}) -> ['"',erlang:list_to_atom(Str),'"'].

left(P1, P2) when P1 > P2 -> ['('];
left(_P1, _P2) -> [].

right(P1, P2) when P1 > P2 -> [')'];
right(_P1, _P2) -> [].

prio(intersection) -> 300;
prio(difference)   -> 200;
prio(union)        -> 200.

name_it(tmp)           -> ' = ';
name_it(user)          -> ' := ';
name_it('|')           -> ' | ';
name_it('||')          -> ' || ';
name_it('|||')         -> ' ||| ';
name_it(union)         -> ' + ';
name_it(intersection)  -> ' * ';
name_it(difference)    -> ' - ';
name_it(Name) -> Name.   
