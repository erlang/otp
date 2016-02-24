%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2015. All Rights Reserved.
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

%%% A stripped version of erl_parse.yrl.
%%%
%%% A type for the abstract format with *local* types has been added.
%%% The type of the abstract format is not up-to-date, but it does not
%%% matter since the purpose of the type is to stress the conversion
%%% of type forms to erl_type().

-module(big_local_type).

-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,tokens/1,tokens/2]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).

-export_type([abstract_clause/0, abstract_expr/0, abstract_form/0,
              error_info/0]).

%% Start of Abstract Format

-type line() :: erl_anno:line().

-export_type([af_module/0, af_export/0, af_import/0, af_fa_list/0,
              af_compile/0, af_file/0, af_record_decl/0,
              af_field_decl/0, af_wild_attribute/0,
              af_record_update/1, af_catch/0, af_local_call/0,
              af_remote_call/0, af_args/0, af_local_function/0,
              af_remote_function/0, af_list_comprehension/0,
              af_binary_comprehension/0, af_template/0,
              af_qualifier_seq/0, af_qualifier/0, af_generator/0,
              af_filter/0, af_block/0, af_if/0, af_case/0, af_try/0,
              af_clause_seq/0, af_catch_clause_seq/0, af_receive/0,
              af_local_fun/0, af_remote_fun/0, af_fun/0, af_query/0,
              af_query_access/0, af_clause/0,
              af_catch_clause/0, af_catch_pattern/0, af_catch_class/0,
              af_body/0, af_guard_seq/0, af_guard/0, af_guard_test/0,
              af_record_access/1, af_guard_call/0,
              af_remote_guard_call/0, af_pattern/0, af_literal/0,
              af_atom/0, af_lit_atom/1, af_integer/0, af_float/0,
              af_string/0, af_match/1, af_variable/0,
              af_anon_variable/0, af_tuple/1, af_nil/0, af_cons/1,
              af_bin/1, af_binelement/1, af_binelement_size/0,
              af_binary_op/1, af_binop/0, af_unary_op/1, af_unop/0]).

-type abstract_form() :: af_module()
                       | af_export()
                       | af_import()
                       | af_compile()
                       | af_file()
                       | af_record_decl()
                       | af_wild_attribute()
                       | af_function_decl().

-type af_module() :: {attribute, line(), module, module()}.

-type af_export() :: {attribute, line(), export, af_fa_list()}.

-type af_import() :: {attribute, line(), import, af_fa_list()}.

-type af_fa_list() :: [{function(), arity()}].

-type af_compile() :: {attribute, line(), compile, any()}.

-type af_file() :: {attribute, line(), file, {string(), line()}}.

-type af_record_decl() ::
        {attribute, line(), record, af_record_name(), [af_field_decl()]}.

-type af_field_decl() :: {record_field, line(), af_atom()}
                       | {record_field, line(), af_atom(), abstract_expr()}.

%% Types and specs, among other things...
-type af_wild_attribute() :: {attribute, line(), af_atom(), any()}.

-type af_function_decl() ::
        {function, line(), function(), arity(), af_clause_seq()}.

-type abstract_expr() :: af_literal()
                       | af_match(abstract_expr())
                       | af_variable()
                       | af_tuple(abstract_expr())
                       | af_nil()
                       | af_cons(abstract_expr())
                       | af_bin(abstract_expr())
                       | af_binary_op(abstract_expr())
                       | af_unary_op(abstract_expr())
                       | af_record_access(abstract_expr())
                       | af_record_update(abstract_expr())
                       | af_record_index()
                       | af_record_field(abstract_expr())
                       | af_catch()
                       | af_local_call()
                       | af_remote_call()
                       | af_list_comprehension()
                       | af_binary_comprehension()
                       | af_block()
                       | af_if()
                       | af_case()
                       | af_try()
                       | af_receive()
                       | af_local_fun()
                       | af_remote_fun()
                       | af_fun()
                       | af_query()
                       | af_query_access().

-type af_record_update(T) :: {record,
                              line(),
                              abstract_expr(),
                              af_record_name(),
                              [af_record_field(T)]}.

-type af_catch() :: {'catch', line(), abstract_expr()}.

-type af_local_call() :: {call, line(), af_local_function(), af_args()}.

-type af_remote_call() :: {call, line(), af_remote_function(), af_args()}.

-type af_args() :: [abstract_expr()].

-type af_local_function() :: abstract_expr().

-type af_remote_function() ::
        {remote, line(), abstract_expr(), abstract_expr()}.

-type af_list_comprehension() ::
        {lc, line(), af_template(), af_qualifier_seq()}.

-type af_binary_comprehension() ::
        {bc, line(), af_template(), af_qualifier_seq()}.

-type af_template() :: abstract_expr().

-type af_qualifier_seq() :: [af_qualifier()].

-type af_qualifier() :: af_generator() | af_filter().

-type af_generator() :: {generate, line(), af_pattern(), abstract_expr()}
                      | {b_generate, line(), af_pattern(), abstract_expr()}.

-type af_filter() :: abstract_expr().

-type af_block() :: {block, line(), af_body()}.

-type af_if() :: {'if', line(), af_clause_seq()}.

-type af_case() :: {'case', line(), abstract_expr(), af_clause_seq()}.

-type af_try() :: {'try',
                   line(),
                   af_body(),
                   af_clause_seq(),
                   af_catch_clause_seq(),
                   af_body()}.

-type af_clause_seq() :: [af_clause(), ...].

-type af_catch_clause_seq() :: [af_clause(), ...].

-type af_receive() ::
        {'receive', line(), af_clause_seq()}
      | {'receive', line(), af_clause_seq(), abstract_expr(), af_body()}.

-type af_local_fun() :: {'fun', line(), {function, function(), arity()}}.

-type af_remote_fun() ::
        {'fun', line(), {function, module(), function(), arity()}}
      | {'fun', line(), {function, af_atom(), af_atom(), af_integer()}}.

-type af_fun() :: {'fun', line(), {clauses, af_clause_seq()}}.

-type af_query() :: {'query', line(), af_list_comprehension()}.

-type af_query_access() ::
        {record_field, line(), abstract_expr(), af_field_name()}.

-type abstract_clause() :: af_clause() | af_catch_clause().

-type af_clause() ::
        {clause, line(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_catch_clause() ::
        {clause, line(), [af_catch_pattern()], af_guard_seq(), af_body()}.

-type af_catch_pattern() ::
        {af_catch_class(), af_pattern(), af_anon_variable()}.

-type af_catch_class() ::
        af_variable()
      | af_lit_atom(throw) | af_lit_atom(error) | af_lit_atom(exit).

-type af_body() :: [abstract_expr(), ...].

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() :: af_literal()
                       | af_variable()
                       | af_tuple(af_guard_test())
                       | af_nil()
                       | af_cons(af_guard_test())
                       | af_bin(af_guard_test())
                       | af_binary_op(af_guard_test())
                       | af_unary_op(af_guard_test())
                       | af_record_access(af_guard_test())
                       | af_record_index()
                       | af_record_field(af_guard_test())
                       | af_guard_call()
                       | af_remote_guard_call().

-type af_record_access(T) ::
        {record, line(), af_record_name(), [af_record_field(T)]}.

-type af_guard_call() :: {call, line(), function(), [af_guard_test()]}.

-type af_remote_guard_call() ::
        {call, line(), atom(), af_lit_atom(erlang), [af_guard_test()]}.

-type af_pattern() :: af_literal()
                    | af_match(af_pattern())
                    | af_variable()
                    | af_anon_variable()
                    | af_tuple(af_pattern())
                    | af_nil()
                    | af_cons(af_pattern())
                    | af_bin(af_pattern())
                    | af_binary_op(af_pattern())
                    | af_unary_op(af_pattern())
                    | af_record_index()
                    | af_record_field(af_pattern()).

-type af_literal() :: af_atom() | af_integer() | af_float() | af_string().

-type af_atom() :: af_lit_atom(atom()).

-type af_lit_atom(A) :: {atom, line(), A}.

-type af_integer() :: {integer, line(), non_neg_integer()}.

-type af_float() :: {float, line(), float()}.

-type af_string() :: {string, line(), [byte()]}.

-type af_match(T) :: {match, line(), T, T}.

-type af_variable() :: {var, line(), atom()}.

-type af_anon_variable() :: {var, line(), '_'}.

-type af_tuple(T) :: {tuple, line(), [T]}.

-type af_nil() :: {nil, line()}.

-type af_cons(T) :: {cons, line, T, T}.

-type af_bin(T) :: {bin, line(), [af_binelement(T)]}.

-type af_binelement(T) :: {bin_element,
                           line(),
                           T,
                           af_binelement_size(),
                           type_specifier_list()}.

-type af_binelement_size() :: default | abstract_expr().

-type af_binary_op(T) :: {op, line(), T, af_binop(), T}.

-type af_binop() :: '/' | '*' | 'div' | 'rem' | 'band' | 'and' | '+' | '-'
                  | 'bor' | 'bxor' | 'bsl' | 'bsr' | 'or' | 'xor' | '++'
                  | '--' | '==' | '/=' | '=<' | '<'  | '>=' | '>' | '=:='
                  | '=/='.

-type af_unary_op(T) :: {op, line(), af_unop(), T}.

-type af_unop() :: '+' | '*' | 'bnot' | 'not'.

%% See also lib/stdlib/{src/erl_bits.erl,include/erl_bits.hrl}.
-type type_specifier_list() :: default | [type_specifier(), ...].

-type type_specifier() :: af_type()
                        | af_signedness()
                        | af_endianness()
                        | af_unit().

-type af_type() :: integer
                 | float
                 | binary
                 | bytes
                 | bitstring
                 | bits
                 | utf8
                 | utf16
                 | utf32.

-type af_signedness() :: signed | unsigned.

-type af_endianness() :: big | little | native.

-type af_unit() :: {unit, 1..256}.

-type af_record_index() ::
        {record_index, line(), af_record_name(), af_field_name()}.

-type af_record_field(T) :: {record_field, line(), af_field_name(), T}.

-type af_record_name() :: atom().

-type af_field_name() :: atom().

%% End of Abstract Format

-type error_description() :: term().
-type error_info() :: {erl_anno:line(), module(), error_description()}.
-type token() :: {Tag :: atom(), Line :: erl_anno:line()}.

%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

-define(mkop2(L, OpPos, R),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).

-define(mkop1(OpPos, A),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,A}
        end).

%% keep track of line info in tokens
-define(line(Tup), element(2, Tup)).

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo} when
      Tokens :: [token()],
      AbsForm :: abstract_form(),
      ErrorInfo :: error_info().
parse_form([{'-',L1},{atom,L2,spec}|Tokens]) ->
    parse([{'-',L1},{'spec',L2}|Tokens]);
parse_form([{'-',L1},{atom,L2,callback}|Tokens]) ->
    parse([{'-',L1},{'callback',L2}|Tokens]);
parse_form(Tokens) ->
    parse(Tokens).

-spec parse_exprs(Tokens) -> {ok, ExprList} | {error, ErrorInfo} when
      Tokens :: [token()],
      ExprList :: [abstract_expr()],
      ErrorInfo :: error_info().
parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,_} = Err -> Err
    end.

-spec parse_term(Tokens) -> {ok, Term} | {error, ErrorInfo} when
      Tokens :: [token()],
      Term :: term(),
      ErrorInfo :: error_info().
parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    try normalise(Expr) of
		Term -> {ok,Term}
	    catch
		_:_R -> {error,{?line(Expr),?MODULE,"bad term"}}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{?line(E2),?MODULE,"bad term"}};
	{error,_} = Err -> Err
    end.

%%  Convert between the abstract form of a term and a term.

-spec normalise(AbsTerm) -> Data when
      AbsTerm :: abstract_expr(),
      Data :: term().
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
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

%%  Generate a list of tokens representing the abstract term.

-spec tokens(AbsTerm) -> Tokens when
      AbsTerm :: abstract_expr(),
      Tokens :: [token()].
tokens(Abs) ->
    tokens(Abs, []).

-spec tokens(AbsTerm, MoreTokens) -> Tokens when
      AbsTerm :: abstract_expr(),
      MoreTokens :: [token()],
      Tokens :: [token()].
tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, ?line(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = ?line(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, ?line(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

-type pre_op() :: 'catch' | '+' | '-' | 'bnot' | 'not' | '#'.

-spec preop_prec(pre_op()) -> {0 | 600 | 700, 100 | 700 | 800}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

-spec func_prec() -> {800,700}.

func_prec() -> {800,700}.

-spec max_prec() -> 1000.

max_prec() -> 1000.

parse(T) ->
    bar:foo(T).
