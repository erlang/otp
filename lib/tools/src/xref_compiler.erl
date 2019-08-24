%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2015. All Rights Reserved.
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

-module(xref_compiler).

-include("xref.hrl").

%-define(debug, true).

-ifdef(debug).
-define(FORMAT(P, A), io:format(P, A)).
-define(CALL(F), F).
-else.
-define(FORMAT(P, A), ok).
-define(CALL(F), ok).
-endif.

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([compile/2]).

-export([update_graph_counter/3]).

-export([format_error/1]).

-import(lists,
	[concat/1, foldl/3, nthtail/2, reverse/1, sort/1, sublist/2]).

-import(sofs,
	[composite/2, difference/2, empty_set/0, from_term/1,
	 intersection/2, is_empty_set/1, multiple_relative_product/2,
	 projection/2, relation/1, relation_to_family/1,
	 restriction/2, specification/2, substitution/2,
	 to_external/1, union/2, union_of_family/1]).

%%
%%  Exported functions
%%

compile(Chars, Table) ->
    case xref_scanner:scan(Chars) of
	{ok, Tokens}  ->
	    case xref_parser:parse(Tokens) of
		{ok, ParseTree} ->
		    ?FORMAT("ParseTree ~p~n", [ParseTree]),
		    case catch statements(ParseTree, Table) of
			E={error, _, _} ->
			    E;
			{ok, UV, P} ->
			    %% User variables to be.
			    Table1 = user_vars(UV, Table),
			    ?CALL(statistics(runtime)),
			    Reply = i(P, Table1),
			    ?CALL({_, Time} = statistics(runtime)),
			    ?FORMAT("Result in ~p ms~n",[Time]),
			    Reply
		    end;
		{error, {Line, _Module, Error}} ->
		    error({parse_error, Line, Error})
	    end;
	{error, Info, Line} ->
	    error({parse_error, Line, Info})
    end.

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({parse_error, Line, Error}) ->
    format_parse_error(Error, format_line(Line));
format_error({variable_reassigned, Expr}) ->
    io_lib:format("Variable assigned more than once: ~ts~n", [Expr]);
format_error({unknown_variable, Name}) ->
    io_lib:format("Variable ~tp used before set~n", [Name]);
format_error({type_error, Expr}) ->
    io_lib:format("Operator applied to argument(s) of different or "
		  "invalid type(s): ~ts~n", [Expr]);
format_error({type_mismatch, Expr1, Expr2}) ->
    io_lib:format("Constants of different types: ~ts, ~ts~n",
		  [Expr1, Expr2]);
format_error({unknown_constant, Constant}) ->
    io_lib:format("Unknown constant ~ts~n", [Constant]);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%%
%%  Local functions
%%

user_vars([{{user,Name}, Val} | UV], Table) ->
    user_vars(UV, dict:store(Name, Val, Table));
user_vars([_V | UV], Table) ->
    user_vars(UV, Table);
user_vars([], Table) ->
    Table.

statements(Stmts, Table) ->
    statements(Stmts, Table, [], []).

statements([Stmt={assign, VarType, Name, E} | Stmts0], Table, L, UV) ->
    case dict:find(Name, Table) of
	{ok, _} ->
	    throw_error({variable_reassigned, xref_parser:t2s(Stmt)});
	error ->
	    {Type, OType, NewE} = t_expr(E, Table),
	    Val = #xref_var{name = Name, vtype = VarType,
			    otype = OType, type = Type},
	    NewTable = dict:store(Name, Val, Table),
	    Stmts = if Stmts0 =:= [] -> [{variable, Name}]; true -> Stmts0 end,
	    Variable = {VarType, Name},
	    Put = {put, Variable, NewE},
	    statements(Stmts, NewTable, [Put | L], [{Variable,Val} | UV])
    end;
statements([Expr], Table, L, UV) ->
    {Type, OType, NewE} = t_expr(Expr, Table),
    E1 = un_familiarize(Type, OType, NewE),
    NE = case {Type, OType} of
	     %% Edges with empty sets of line numbers are removed.
	     {{line, _}, edge} ->
		 {relation_to_family, E1};
	     {_Type, edge_closure} ->
		 %% Fake a closure usage, just to make sure it is destroyed.
		 E2 = {fun graph_access/2, E1, E1},
                 {fun(_E) -> 'closure()' end, E2};
	     _Else -> E1
	 end,
    {ok, UV, stats(L, NE)}.

stats([{put, V, X} | Ss], E) ->
    stats(Ss, {put, V, X, E});
stats([], E) ->
    E.

t_expr(E, Table) ->
    {expr, Type, OType, E1} = check_expr(E, Table),
    ?FORMAT("TExpr:~n~p~n",[E1]),
    E2 = convert(E1),
    ?FORMAT("After conversion:~n~p~n",[E2]),
    {Type, OType, E2}.

%%% check_expr/2 translates Expr in xref_parser.yrl into TExpr:
%%%
%%% TExpr = {expr, Type, ObjectType, Expr}
%%% Expr = {constants, [Constant]}
%%%      | {variable, {VarType, VarName}}
%%%      | {call, Call, Expr}
%%%      | {call, Call, Expr, Expr}
%%%      | {call, restriction, integer(), Expr, Expr}
%%%      | {convert, ObjectType, Type, Type}
%%%      | {convert, Type, Type}
%%% Constant = atom() | {atom(), atom()} | MFA | {MFA, MFA}
%%% Call = atom() % function in the sofs module
%%%      | fun()
%%% Type = {line, LineType} | function | module | application | release
%%%      | number
%%% LineType = line | local_call | external_call | export_call | all_line_call
%%% VarType = predef | user | tmp
%%% ObjectType = vertex | vertex_set | edge | edge_set | edge_closure | path
%%%            | number
%%% MFA = {atom(), atom(), integer()}

%% -> TExpr
check_expr({list, L}, Table) ->
    check_constants(L, Table);
check_expr({tuple, L}, Table) ->
    {expr, Type, vertex, _Consts} = check_constants(L, Table),
    Cs = reverse(constant_vertices(L, [])),
    {expr, Type, path, {constants, Cs}};
check_expr({variable, Name}, Table) ->
    case dict:find(Name, Table) of
	{ok, #xref_var{vtype = VarType, otype = OType, type = Type}} ->
	    V0 = {variable, {VarType, Name}},
	    V = case {VarType, Type, OType} of
		    {predef, release, _} -> V0;
		    {predef, application, _} -> V0;
		    {predef, module, _} -> V0;
		    {predef, function, vertex} -> V0;
		    {predef, function, edge} -> {call, union_of_family, V0};
		    _Else  -> V0
	    end,
	    {expr, Type, OType, V};
	error ->
	    throw_error({unknown_variable, Name})
    end;
check_expr({type, {type, _Type}, E}, Table) ->
    check_expr(E, Table);
check_expr(Expr={type, {convert, NewType0}, E}, Table) ->
    NewType = what_type(NewType0),
    {expr, OldType, OType, NE} = check_expr(E, Table),
    ok = check_conversion(OType, OldType, NewType, Expr),
    {expr, NewType, OType, {convert, OType, OldType, NewType, NE}};
check_expr(Expr={set, SOp, E}, Table) ->
    {expr, Type, OType0, E1} = check_expr(E, Table),
    OType = case {OType0, SOp} of
		{edge, range} -> vertex;
		{edge, domain} -> vertex;
		{edge, weak} -> edge;
		{edge, strict} -> edge;
		{edge_set, range} -> vertex_set;
		{edge_set, domain} -> vertex_set;
		{edge_set, weak} -> edge_set;
		{edge_set, strict} -> edge_set;
		_ ->
		    throw_error({type_error, xref_parser:t2s(Expr)})
	    end,
    Op = set_op(SOp),
    NE = function_vertices_to_family(Type, OType, {call, Op, E1}),
    {expr, Type, OType, NE};
check_expr(Expr={graph, Op, E}, Table) ->
    {expr, Type, NOType, E1} = check_expr(E, Table),
    case Type of
	{line, _LineType} ->
	    throw_error({type_error, xref_parser:t2s(Expr)});
	_Else ->
	    ok
    end,
    OType =
	case {NOType, Op} of
	    {edge, components} -> vertex_set;
	    {edge, condensation} -> edge_set;
	    {edge, closure} -> edge_closure;
	    {edge_closure, components} -> vertex_set;
	    {edge_closure, condensation} -> edge_set;
	    {edge_closure, closure} -> edge_closure;
	    %% Neither need nor want these ones:
	    %% {edge_set, closure} -> edge_set_closure;
	    %% {edge_set, components} -> vertex_set_set;
	    _ ->
		throw_error({type_error, xref_parser:t2s(Expr)})
	end,
    E2 = {convert, NOType, edge_closure, E1},
    NE = case Op of
	     closure -> E2;
	     _Op -> use_of_closure(Op, E2)
	 end,
    {expr, Type, OType, NE};
check_expr(Expr={numeric, '#', E}, Table) ->
    {expr, Type, OType, E1} = check_expr(E, Table),
    case OType of
	vertex -> ok;
	vertex_set -> ok;
	edge -> ok;
	edge_set -> ok;
	_Else -> throw_error({type_error, xref_parser:t2s(Expr)})
    end,
    NE = {convert, OType, Type, number, E1},
    {expr, number, number, {call, no_elements, NE}};
check_expr(Expr={set, SOp, E1, E2}, Table) ->
    %% sets and numbers...
    {expr, Type1, OType1, NE1} = check_expr(E1, Table),
    {expr, Type2, OType2, NE2} = check_expr(E2, Table),
    OType = case {OType1, OType2} of
		{vertex, vertex} -> vertex;
		{edge, edge} -> edge;
		{number, number} -> number;
		_ -> throw_error({type_error, xref_parser:t2s(Expr)})
	    end,
    case OType of
	number ->
	    {expr, number, number, {call, ari_op(SOp), NE1, NE2}};
	_Else -> % set
	    {Type, NewE1, NewE2} =
		case {type_ord(Type1), type_ord(Type2)} of
		    {T1, T2} when T1 =:= T2 ->
			%% Example: if Type1 = {line, line} and
			%% Type2 = {line, export_line}, then this is not
			%% correct, but works:
			{Type1, NE1, NE2};
		    {T1, T2} when T1 < 2; T2 < 2 ->
			throw_error({type_error, xref_parser:t2s(Expr)});
		    {T1, T2} when T1 > T2 ->
			{Type2, {convert, OType, Type1, Type2, NE1}, NE2};
		    {T1, T2} when T1 < T2 ->
			{Type1, NE1, {convert, OType, Type2, Type1, NE2}}
		end,
	    Op = set_op(SOp, Type, OType),
	    {expr, Type, OType, {call, Op, NewE1, NewE2}}
    end;
check_expr(Expr={restr, ROp, E1, E2}, Table) ->
    {expr, Type1, OType1, NE1} = check_expr(E1, Table),
    {expr, Type2, OType2, NE2} = check_expr(E2, Table),
    case {Type1, Type2} of
	{{line, _LineType1}, _Type2} ->
	    throw_error({type_error, xref_parser:t2s(Expr)});
	{_Type1, {line, _LineType2}} ->
	    throw_error({type_error, xref_parser:t2s(Expr)});
	_ ->
	    ok
    end,
    case {OType1, OType2} of
	{edge, vertex} when ROp =:= '|||' ->
	    {expr, _, _, R1} = restriction('|', E1, Type1, NE1, Type2, NE2),
	    {expr, _, _, R2} = restriction('||', E1, Type1, NE1, Type2, NE2),
	    {expr, Type1, edge, {call, intersection, R1, R2}};
	{edge, vertex} ->
	    restriction(ROp, E1, Type1, NE1, Type2, NE2);
	{edge_closure, vertex} when ROp =:= '|||' ->
	    {expr, _, _, R1} =
		closure_restriction('|', Type1, Type2, OType2, NE1, NE2),
	    {expr, _, _, R2} =
		closure_restriction('||', Type1, Type2, OType2, NE1, NE2),
	    {expr, Type1, edge, {call, intersection, R1, R2}};
	{edge_closure, vertex} ->
	    closure_restriction(ROp, Type1, Type2, OType2, NE1, NE2);
	_ ->
	    throw_error({type_error, xref_parser:t2s(Expr)})
    end;
check_expr(Expr={path, E1, E2}, Table) ->
    {expr, Type1, OType1a, E1a} = check_expr(E1, Table),
    {expr, Type2, OType2, E2a} = check_expr(E2, Table),
    case {Type1, Type2} of
	{{line, _LineType1}, _Type2} ->
	    throw_error({type_error, xref_parser:t2s(Expr)});
	{_Type1, {line, _LineType2}} ->
	    throw_error({type_error, xref_parser:t2s(Expr)});
	_Else ->
	    ok
    end,
    E2b = {convert, OType2, Type2, Type1, E2a},
    {OType1, NE1} = path_arg(OType1a, E1a),
    NE2 = case {OType1, OType2} of
	      {path, edge} -> {convert, OType2, edge_closure, E2b};
	      {path, edge_closure} when Type1 =:= Type2 -> E2b;
	      _ -> throw_error({type_error, xref_parser:t2s(Expr)})
	  end,
    {expr, Type1, path, use_of_closure(path, NE2, NE1)};
check_expr({regexpr, RExpr, Type0}, _Table) ->
    %% Using the "universal" variables is not optimal as regards speed,
    %% but it is simple...
    Type = what_type(Type0),
    V = case Type of
	    function -> v;
	    module -> 'M';
	    application -> 'A';
	    release -> 'R'
	end,
    Var = {variable, {predef, V}},
    Call = {call, fun(E, V2) -> xref_utils:regexpr(E, V2) end,
	    {constants, RExpr}, Var},
    {expr, Type, vertex, Call};
check_expr(C={constant, _Type, _OType, _C}, Table) ->
    check_constants([C], Table).

path_arg(edge, E={constants, C}) ->
    case to_external(C) of
	[{V1,V2}] -> {path, {constants, [V1, V2]}};
	_ -> {edge, E}
    end;
path_arg(OType, E) ->
    {OType, E}.

check_conversion(OType, Type1, Type2, Expr) ->
    case conversions(OType, Type1, Type2) of
	ok -> ok;
	not_ok -> throw_error({type_error, xref_parser:t2s(Expr)})
    end.

%% Allowed conversions.
conversions(_OType, {line, LineType}, {line, LineType}) -> ok;
conversions(edge, {line, _}, {line, all_line_call}) -> ok;
conversions(edge, From, {line, Line})
                 when is_atom(From), Line =/= all_line_call -> ok;
conversions(vertex, From, {line, line}) when is_atom(From) -> ok;
conversions(vertex, From, To) when is_atom(From), is_atom(To) -> ok;
conversions(edge, From, To) when is_atom(From), is_atom(To) -> ok;
%% "Extra":
conversions(edge, {line, Line}, To)
                 when is_atom(To), Line =/= all_line_call -> ok;
conversions(vertex, {line, line}, To) when is_atom(To) -> ok;
conversions(_OType, _From, _To) -> not_ok.

set_op(union, {line, _LineType}, edge) -> family_union;
set_op(intersection, {line, _LineType}, edge) -> family_intersection;
set_op(difference, {line, _LineType}, edge) -> family_difference;
set_op(union, function, vertex) -> family_union;
set_op(intersection, function, vertex) -> family_intersection;
set_op(difference, function, vertex) -> family_difference;
set_op(SOp, _Type, _OType) -> SOp.

set_op(weak) -> weak_relation;
set_op(strict) -> strict_relation;
set_op(Op) -> Op.

ari_op(union) -> fun(X, Y) -> X + Y end;
ari_op(intersection) -> fun(X, Y) -> X * Y end;
ari_op(difference) -> fun(X, Y) -> X - Y end.

restriction(ROp, E1, Type1, NE1, Type2, NE2) ->
    {Column, _} = restr_op(ROp),
    case NE1 of
	{call, union_of_family, _E} when ROp =:= '|' ->
	    restriction(Column, Type1, E1, Type2, NE2);
	{call, union_of_family, _E} when ROp =:= '||' ->
	    E1p = {inverse, E1},
	    restriction(Column, Type1, E1p, Type2, NE2);
	_ ->
	    NE2a = {convert, vertex, Type2, Type1, NE2},
	    NE2b = family_to_function_vertices(Type1, vertex, NE2a),
	    {expr, Type1, edge, {call, restriction, Column, NE1, NE2b}}
    end.

restriction(Column, Type1, VE, Type2, E2) when Type1 =:= function ->
    M = {convert, vertex, Type2, module, E2},
    Restr = {call, union_of_family, {call, restriction, VE, M}},
    C = {convert, vertex, Type2, Type1, E2},
    F = family_to_function_vertices(Type1, vertex, C),
    {expr, Type1, edge, {call, restriction, Column, Restr, F}}.

closure_restriction(Op, Type1, Type2, OType2, E1, E2) ->
    {_, Fun} = restr_op(Op),
    E2a = {convert, OType2, Type2, Type1, E2},
    E2b = family_to_function_vertices(Type1, vertex, E2a),
    {expr, Type1, edge, use_of_closure(Fun, E1, E2b)}.

restr_op('|')  -> {1, call};
restr_op('||') -> {2, use}.

%% Closures (digraphs) must be deleted, but not too soon. A wrapper
%% is inserted here for every use of a closure, to make sure that a
%% 'save' and an 'unput' instruction are inserted for every digraph, in
%% particular the temporary ones. The 'unput' instruction must occur
%% _after_ the call to the function that uses the digraph (the default
%% is that it is inserted _before_ the call).
use_of_closure(Op, C) ->
    access_of_closure(C, {call, fun(X) -> xref_utils:Op(X) end, C}).

use_of_closure(Op, C, E) ->
    access_of_closure(C, {call, fun(X, Y) -> xref_utils:Op(X, Y) end, C, E}).

access_of_closure(C, E) ->
    {call, fun graph_access/2, C, E}.

check_constants(Cs=[C={constant, Type0, OType, _Con} | Cs1], Table) ->
    check_mix(Cs1, Type0, OType, C),
    Types = case Type0 of
		unknown -> ['Rel', 'App', 'Mod'];
		T -> [T]
	    end,
    case split(Types, Cs, Table) of
	[{TypeToBe, _Cs}] ->
            S = from_term([Con || {constant, _T, _OT, Con} <- Cs]),
	    Type = what_type(TypeToBe),
	    E = function_vertices_to_family(Type, OType, {constants, S}),
	    {expr, Type, OType, E};
	[{Type1, [C1|_]}, {Type2, [C2|_]} | _] ->
	    throw_error({type_mismatch,
			 make_vertex(Type1, C1),
			 make_vertex(Type2, C2)})
    end.

check_mix([C={constant, 'Fun', OType, _Con} | Cs], 'Fun', OType, _C0) ->
    check_mix(Cs, 'Fun', OType, C);
check_mix([C={constant, Type, OType, _Con} | Cs], Type0, OType, _C0)
         when Type =/= 'Fun', Type0 =/= 'Fun' ->
    check_mix(Cs, Type, OType, C);
check_mix([C | _], _Type0, _OType0, C0) ->
    throw_error({type_mismatch, xref_parser:t2s(C0), xref_parser:t2s(C)});
check_mix([], _Type0, _OType0, _C0) ->
    ok.

split(Types, Cs, Table) ->
    Vs = from_term(constant_vertices(Cs, [])),
    split(Types, Vs, empty_set(), unknown, Table, []).

split([Type | Types], Vs, AllSoFar, _Type, Table, L) ->
    S0 = known_vertices(Type, Vs, Table),
    S = difference(S0, AllSoFar),
    case is_empty_set(S) of
	true ->
	    split(Types, Vs, AllSoFar, Type, Table, L);
	false ->
	    All = union(AllSoFar, S0),
	    split(Types, Vs, All, Type, Table,
		  [{Type, to_external(S)} | L])
    end;
split([], Vs, All, Type, _Table, L) ->
    case to_external(difference(Vs, All)) of
	[] -> L;
	[C|_] -> throw_error({unknown_constant, make_vertex(Type, C)})
    end.

make_vertex(Type, C) ->
    xref_parser:t2s({constant, Type, vertex, C}).

constant_vertices([{constant, _Type, edge, {A,B}} | Cs], L) ->
    constant_vertices(Cs, [A, B | L]);
constant_vertices([{constant, _Type, vertex, V} | Cs], L) ->
    constant_vertices(Cs, [V | L]);
constant_vertices([], L) ->
    L.

known_vertices('Fun', Cs, T) ->
    M = projection(1, Cs),
    F = union_of_family(restriction(fetch_value(v, T), M)),
    union(bifs(Cs), intersection(Cs, F));
known_vertices('Mod', Cs, T) ->
    intersection(Cs, fetch_value('M', T));
known_vertices('App', Cs, T) ->
    intersection(Cs, fetch_value('A', T));
known_vertices('Rel', Cs, T) ->
    intersection(Cs, fetch_value('R', T)).

bifs(Cs) ->
    specification({external,
                   fun({M,F,A}) -> xref_utils:is_builtin(M, F, A) end},
                  Cs).

function_vertices_to_family(function, vertex, E) ->
    {call, partition_family, 1, E};
function_vertices_to_family(_Type, _OType, E) ->
    E.

family_to_function_vertices(function, vertex, E) ->
    {call, union_of_family, E};
family_to_function_vertices(_Type, _OType, E) ->
    E.

-define(Q(E), {quote, E}).

convert({inverse, {variable, Variable}}) ->
    {get, {inverse, var_name(Variable)}};
convert({variable, Variable}) ->
    {get, var_name(Variable)};
convert({convert, FromOType, ToOType, E}) ->
    convert(convert(E), FromOType, ToOType);
convert({convert, OType, FromType, ToType, E}) ->
    convert(convert(E), OType, FromType, ToType);
convert({call, Op, E}) ->
    {Op, convert(E)};
convert({call, Op, E1, E2}) ->
    {Op, convert(E1), convert(E2)};
convert({call, Op, E1, E2, E3}) ->
    {Op, convert(E1), convert(E2), convert(E3)};
convert({constants, Constants}) ->
    ?Q(Constants);
convert(I) when is_integer(I) ->
    ?Q(I).

var_name({predef, VarName}) -> VarName;
var_name(Variable) -> Variable.

convert(E, OType, OType) ->
    E;
convert(E, edge, edge_closure) ->
    {fun(S) -> xref_utils:closure(S) end, E}.

convert(E, OType, FromType, number) ->
    un_familiarize(FromType, OType, E);
convert(E, OType, FromType, ToType) ->
    case {type_ord(FromType), type_ord(ToType)} of
        {FT, To} when FT =:= To ->
            E;
	{FT, ToT} when FT > ToT ->
            special(OType, FromType, ToType, E);
	{FT, ToT} when FT < ToT ->
            general(OType, FromType, ToType, E)
    end.

-define(T(V), {tmp, V}).

general(_ObjectType, FromType, ToType, X) when FromType =:= ToType ->
    X;
general(edge, {line, _LineType}, ToType, LEs) ->
    VEs = {projection, ?Q({external, fun({V1V2,_Ls}) -> V1V2 end}), LEs},
    general(edge, function, ToType, VEs);
general(edge, function, ToType, VEs) ->
    MEs = {projection,
	   ?Q({external, fun({{M1,_,_},{M2,_,_}}) -> {M1,M2} end}),
	   VEs},
    general(edge, module, ToType, MEs);
general(edge, module, ToType, MEs) ->
    AEs = {image, {get, me2ae}, MEs},
    general(edge, application, ToType, AEs);
general(edge, application, release, AEs) ->
    {image, {get, ae}, AEs};
general(vertex, {line, _LineType}, ToType, L) ->
    V = {partition_family, ?Q(1), {domain, L}},
    general(vertex, function, ToType, V);
general(vertex, function, ToType, V) ->
    M = {domain, V},
    general(vertex, module, ToType, M);
general(vertex, module, ToType, M) ->
    A = {image, {get, m2a}, M},
    general(vertex, application, ToType, A);
general(vertex, application, release, A) ->
    {image, {get, a2r}, A}.

special(_ObjectType, FromType, ToType, X) when FromType =:= ToType ->
    X;
special(edge, {line, _LineType}, {line, all_line_call}, Calls) ->
   {put, ?T(mods),
       {projection,
	?Q({external, fun({{{M1,_,_},{M2,_,_}},_}) -> {M1,M2} end}),
	Calls},
       {put, ?T(def_at),
           {union, {image, {get, def_at},
                           {union, {domain, {get, ?T(mods)}},
                                   {range, {get, ?T(mods)}}}}},
           {fun funs_to_lines/2,
	           {get, ?T(def_at)}, Calls}}};
special(edge, function, {line, LineType}, VEs) ->
    Var = if
	      LineType =:= line -> call_at;
	      LineType =:= export_call -> e_call_at;
	      LineType =:= local_call -> l_call_at;
	      LineType =:= external_call -> x_call_at
	  end,
    line_edges(VEs, Var);
special(edge, module, ToType, MEs) ->
    VEs = {image,
	   {projection,
	    ?Q({external, fun(FE={{M1,_,_},{M2,_,_}}) -> {{M1,M2},FE} end}),
	    {union,
	     {image, {get, e},
	      {projection, ?Q({external, fun({M1,_M2}) -> M1 end}), MEs}}}},
	   MEs},
    special(edge, function, ToType, VEs);
special(edge, application, ToType, AEs) ->
    MEs = {inverse_image, {get, me2ae}, AEs},
    special(edge, module, ToType, MEs);
special(edge, release, ToType, REs) ->
    AEs = {inverse_image, {get, ae}, REs},
    special(edge, application, ToType, AEs);
special(vertex, function, {line, _LineType}, V) ->
    {restriction,
       {union_of_family, {restriction, {get, def_at}, {domain, V}}},
       {union_of_family, V}};
special(vertex, module, ToType, M) ->
    V = {restriction, {get, v}, M},
    special(vertex, function, ToType, V);
special(vertex, application, ToType, A) ->
    M = {inverse_image, {get, m2a}, A},
    special(vertex, module, ToType, M);
special(vertex, release, ToType, R) ->
    A = {inverse_image, {get, a2r}, R},
    special(vertex, application, ToType, A).

line_edges(VEs, CallAt) ->
    {put, ?T(ves), VEs,
        {put, ?T(m1),
             {projection, ?Q({external, fun({{M1,_,_},_}) -> M1 end}),
	      {get, ?T(ves)}},
	     {image, {projection, ?Q({external, fun(C={VV,_L}) -> {VV,C} end}),
		      {union, {image, {get, CallAt}, {get, ?T(m1)}}}},
		     {get, ?T(ves)}}}}.

%% {(((v1,l1),(v2,l2)),l) :
%%       (v1,l1) in DefAt and (v2,l2) in DefAt and ((v1,v2),L) in CallAt}
funs_to_lines(DefAt, CallAt) ->
    T1 = multiple_relative_product({DefAt, DefAt}, projection(1, CallAt)),
    T2 = composite(substitution(1, T1), CallAt),
    Fun = fun({{{V1,V2},{L1,L2}},Ls}) -> {{{V1,L1},{V2,L2}},Ls} end,
    projection({external, Fun}, T2).

what_type('Rel')         -> release;
what_type('App')         -> application;
what_type('Mod')         -> module;
what_type('Fun')         -> function;
what_type('Lin')         -> {line, line};
what_type('LLin')        -> {line, local_call};
what_type('XLin')        -> {line, external_call};
what_type('ELin')        -> {line, export_call};
what_type('XXL')         -> {line, all_line_call}.

type_ord({line, all_line_call}) -> 0;
type_ord({line, _LT})           -> 1;
type_ord(function)              -> 2;
type_ord(module)                -> 3;
type_ord(application)           -> 4;
type_ord(release)               -> 5.

%% While evaluating, sets of vertices are represented as families.
%% Sets of edges are not families, but plain sets (this might change).
%% Calls (with line numbers) are "straightened" out here, but will be
%% families again shortly, unless just counted.
un_familiarize(function, vertex, E) ->
    {union_of_family, E};
un_familiarize({line, _}, edge, E) ->
    {family_to_relation, E};
un_familiarize(_Type, _OType, E) ->
    E.

%% Expressions are evaluated using a stack and tail recursion.
%% Common subexpressions are evaluated once only, using a table for
%% storing temporary results.
%% (Using a table _and_ a stack is perhaps not a very good way of
%% doing things.)
i(E, Table) ->
    Start = 1,
    {N, _NE, _NI, NT} = find_nodes(E, Start, dict:new()),
    {Vs, UVs0, L} = save_vars(dict:to_list(NT), NT, [], [], []),

    VarsToSave = to_external(relation_to_family(relation(Vs))),
    Fun = fun({NN,S}, D) ->
		  dict:store(NN, {extra,S,dict:fetch(NN, D)}, D)
	  end,
    D = foldl(Fun, dict:from_list(L), VarsToSave),

    UVs = reverse(sort(UVs0)),
    {_D, Is0} = make_instructions(N, UVs, D),
    Is = insert_unput(Is0),
    ?FORMAT("Instructions:~n~p~n~n~n", [Is]),
    %% Well, compiles _and_ evaluates...
    evaluate(Is, Table, []).

%% Traverses the expression tree in postorder, giving a unique number
%% to each node. A table is created, and common subexpressions found.
find_nodes(E={quote,_}, I, T) ->
    find_node(E, I, T);
find_nodes({get, Var}, I, T) ->
    find_node({var,Var}, I, T);
find_nodes({put, Var, E1, E2}, I, T) ->
    {_NE1_N, NE1, I1, T1} = find_nodes(E1, I, T),
    %% Now NE1 is considered used once, which is wrong. Fixed below.
    NT = dict:store({var, Var}, NE1, T1),
    find_nodes(E2, I1, NT);
find_nodes(Tuple, I, T) when is_tuple(Tuple) ->
    [Tag0 | L] = tuple_to_list(Tuple),
    Fun = fun(A, {L0, I0, T0}) ->
		  {NA, _E, NI, NT} = find_nodes(A, I0, T0),
		  {[NA | L0], NI, NT}
	  end,
    {NL, NI, T1} = foldl(Fun, {[], I, T}, L),
    Tag = case Tag0 of
	      _ when is_function(Tag0) ->
		  Tag0;
	      _ when is_atom(Tag0) ->
		  Arity = length(NL),
		  fun sofs:Tag0/Arity
	  end,
    find_node({apply, Tag, NL}, NI, T1).

find_node(E, I, T) ->
    case dict:find(E, T) of
        {ok, {reuse, N}} ->
	    {N, E, I, T};
	{ok, N} when is_integer(N) ->
	    {N, E, I, dict:store(E, {reuse, N}, T)};
	{ok, E1} ->
	    find_node(E1, I, T);
	error ->
 	    {I, E, I+1, dict:store(E, I, T)}
    end.

%% Creates save instructions for those values (stored on the stack while
%% evaluating) that are to be used after the result has been popped.
save_vars([{I, {reuse,N}} | DL], D, Vs, UVs, L) ->
    save_vars(DL, D, [{N, {save, {tmp, N}}} | Vs], UVs, [{N, I} | L]);
save_vars([{I, N} | DL], D, Vs, UVs, L) when is_integer(N) ->
    save_vars(DL, D, Vs, UVs, [{N, I} | L]);
save_vars([{{var,V={user,_}}, I} | DL], D, Vs, UVs, L) ->
    N = case dict:fetch(I, D) of
	    {reuse, N0} -> N0;
	    N0 -> N0
	end,
    save_vars(DL, D, [{N, {save, V}} | Vs], [N | UVs], L);
save_vars([{{var,{tmp,_}}, _I} | DL], D, Vs, UVs, L) ->
    save_vars(DL, D, Vs, UVs, L);
save_vars([], _D, Vs, UVs, L) ->
    {Vs, UVs, L}.

%% Traverses the expression again, this time using more or less the
%% inverse of the table created by find_nodes. The first time a node
%% is visited, its children are traversed, the following times a
%% get instructions are inserted (using the saved value).
make_instructions(N, UserVars, D) ->
    {D1, Is0} = make_instrs(N, D, []),
    %% Assignments the results of which are not used by the final
    %% expression are handled here. Instructions are created for user
    %% variables only (assignment of a closure is handled properly
    %% without further action).
    make_more_instrs(UserVars, D1, Is0).

make_more_instrs([UV | UVs], D, Is) ->
    case dict:find(UV, D) of
	error ->
	    make_more_instrs(UVs, D, Is);
	_Else ->
	    {ND, NIs} = make_instrs(UV, D, Is),
	    make_more_instrs(UVs, ND, [pop | NIs])
    end;
make_more_instrs([], D, Is) ->
    {D, Is}.

make_instrs(N, D, Is) ->
    case dict:find(N, D) of
	{ok, {extra, Save, Val}} ->
	    {D1, Is1} = make_instr(Val, D, Is),
	    {dict:erase(N, D1), Save ++ Is1};
	{ok, Val} ->
	    {D1, Is1} = make_instr(Val, D, Is),
	    {dict:erase(N, D1), Is1};
	error ->
	    {D, [{get, {tmp, N}} | Is]}
    end.

make_instr({var, V}, D, Is) ->
    {D, [{get, V} | Is]};
make_instr(Q = {quote, _T}, D, Is) ->
    {D, [Q | Is]};
make_instr({apply, MF, Ns}, D, Is) ->
    Fun = fun(N, {D0, Is0}) -> make_instrs(N, D0, Is0) end,
    {D1, Is1} = foldl(Fun, {D, Is}, Ns),
    {D1, [{apply, MF, length(Ns)} | Is1]}.

%% Makes sure that temporary results are removed from the table as soon
%% as they are no longer needed.
%% Assignments may create extra save instructions, which are removed here.
insert_unput(L) ->
    insert_unput(L, dict:new(), []).

insert_unput([I={get, V={tmp, _}} | Is], D, L) ->
    case dict:find(V, D) of
        {ok, _} -> insert_unput(Is, D, [I | L]);
        error ->   insert_unput(Is, dict:store(V, [], D), [I,  {unput, V} | L])
    end;
insert_unput([I={save, V={tmp,_}} | Is], D, L) ->
    case dict:find(V, D) of
	{ok, _} ->
	    insert_unput(Is, dict:erase(V, D), [I | L]);
	error ->
	    %% Extra save removed.
	    insert_unput(Is, dict:erase(V, D), L)
    end;
insert_unput([I | Is], D, L) ->
    insert_unput(Is, D, [I | L]);
insert_unput([], _D, L) ->
    L.

graph_access(_G, V) ->
    %% _G may have been deleted by an unput already
    V.

evaluate([{apply, MF, NoAs} | P], T, S) ->
    Args = sublist(S, NoAs),
    NewS = nthtail(NoAs, S),
    ?FORMAT("Applying ~p/~p~n", [MF,NoAs]),
    evaluate(P, T, [apply(MF, Args) | NewS]);
evaluate([{quote, Val} | P], T, S) ->
    evaluate(P, T, [Val | S]);
evaluate([{get, Var} | P], T, S) when is_atom(Var) -> % predefined
    Value = fetch_value(Var, T),
    Val = case Value of
	      {R, _} -> R; % relation
	      _ -> Value   % simple set
	  end,
    evaluate(P, T, [Val | S]);
evaluate([{get, {inverse, Var}} | P], T, S) -> % predefined, inverse
    {_, R} = fetch_value(Var, T),
    evaluate(P, T, [R | S]);
evaluate([{get, {user, Var}} | P], T, S) ->
    Val = fetch_value(Var, T),
    evaluate(P, T, [Val | S]);
evaluate([{get, Var} | P], T, S) -> % tmp
    evaluate(P, T, [dict:fetch(Var, T) | S]);
evaluate([{save, Var={tmp, _}} | P], T, S=[Val | _]) ->
    T1 = update_graph_counter(Val, +1, T),
    evaluate(P, dict:store(Var, Val, T1), S);
evaluate([{save, {user, Name}} | P], T, S=[Val | _]) ->
    #xref_var{vtype = user, otype = OType, type = Type} = dict:fetch(Name, T),
    NewVar = #xref_var{name = Name, value = Val,
		       vtype = user, otype = OType, type = Type},
    T1 = update_graph_counter(Val, +1, T),
    NT = dict:store(Name, NewVar, T1),
    evaluate(P, NT, S);
evaluate([{unput, Var} | P], T, S) ->
    T1 = update_graph_counter(dict:fetch(Var, T), -1, T),
    evaluate(P, dict:erase(Var, T1), S);
evaluate([pop | P], T, [_ | S]) ->
    evaluate(P, T, S);
evaluate([], T, [R]) ->
    {T, R}.

%% (PossibleGraph, 1 | -1, dict:dict()) -> dict:dict()
%% Use the same table for everything... Here: Reference counters for digraphs.
update_graph_counter(Value, Inc, T) ->
    case catch digraph:info(Value) of
	Info when is_list(Info) ->
	    case dict:find(Value, T) of
		{ok, 1} when Inc =:= -1 ->
		    true = digraph:delete(Value),
		    dict:erase(Value, T);
		{ok, C} ->
		    dict:store(Value, C+Inc, T);
		error when Inc =:= 1 ->
		    dict:store(Value, 1, T)
	    end;
	_EXIT ->
	    T
    end.

fetch_value(V, D) ->
    #xref_var{value = Value} = dict:fetch(V, D),
    Value.

format_parse_error(["invalid_regexp", String, Error], Line) ->
    io_lib:format("Invalid regular expression \"~ts\"~s: ~ts~n",
		  [String, Line, lists:flatten(Error)]);
format_parse_error(["invalid_regexp_variable", Var], Line) ->
    io_lib:format("Invalid wildcard variable ~tp~s "
		  "(only '_' is allowed)~n", [Var, Line]);
format_parse_error(["missing_type", Expr], Line) ->
    io_lib:format("Missing type of regular expression ~ts~s~n",
		  [Expr, Line]);
format_parse_error(["type_mismatch", Expr], Line) ->
    io_lib:format("Type does not match structure of constant~s: ~ts~n",
		  [Line, Expr]);
format_parse_error(["invalid_operator", Op], Line) ->
    io_lib:format("Invalid operator ~tp~s~n", [Op, Line]);
format_parse_error(Error, Line) ->
    io_lib:format("Parse error~s: ~ts~n", [Line, lists:flatten(Error)]).

format_line(?XREF_END_LINE) ->
    " at end of string";
format_line(0) ->
    "";
format_line(Line) when is_integer(Line) ->
    concat([" on line ", Line]).

throw_error(Reason) ->
    throw(error(Reason)).

error(Reason) ->
    {error, ?MODULE, Reason}.
