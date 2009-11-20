%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
Nonterminals
add_op attribute basic_type bif_test
case_expr clause_body
clause_guard clause_head comp_op cr_clause cr_clauses expr expr_tail
exprs farity farity_list form formal_parameter_list function
function_call function_clause guard guard_call guard_expr
guard_expr_list guard_exprs guard_expr_tail guard_expr_tuple
guard_parameter_list
guard_tests guard_test if_clause if_clauses if_expr list match_expr
mult_op parameter_list pattern patterns pattern_list pattern_tail pattern_tuple
prefix_op receive_expr send_expr tuple.

Terminals
'!' '(' ')' '*' '+' ',' '-' '->' '/' '/=' ':' ';' '<' '=' '=/=' '=:='
'=<' '==' '>' '>=' '[' ']' 'after' 'band' 'begin' 'bnot'
'bor' 'bsl' 'bsr' 'bxor' 'case' 'catch' 'div' 'end' 'if' 'of'
'receive' 'rem' 'when' '{' '|' '}' atom float integer string var.
% 'receive' 'rem' 'true' 'when' '{' '|' '}' atom float integer string var.

Rootsymbol form.

Endsymbol dot.

Unary 0 'catch'.
Right 200 '='.
Right 200 '!'.
Left 300 add_op.
Left 400 mult_op.
Unary 500 prefix_op.


add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.


basic_type -> atom : '$1'.
basic_type -> float : '$1'.
basic_type -> integer : '$1'.
basic_type -> string : '$1'.
basic_type -> var : '$1'.
% basic_type -> 'true' : {atom, element(2, '$1'), 'true'}.


pattern -> basic_type : '$1'.
pattern -> pattern_list : '$1'.
pattern -> pattern_tuple : '$1'.

pattern_list -> '[' ']' : {nil, element(2, '$1')}.
pattern_list -> '[' pattern pattern_tail ']' :
   {cons, element(2, '$1'), '$2', '$3'}.

pattern_tail -> '|' pattern : '$2'.
pattern_tail -> ',' pattern pattern_tail :
   {cons, element(2, '$2'), '$2', '$3'}.
pattern_tail -> '$empty' : {nil, 0}.

pattern_tuple -> '{' '}' : {tuple, element(2, '$1'), []}.
pattern_tuple -> '{' patterns '}' : {tuple, element(2, '$1'), '$2'}.

patterns -> pattern : ['$1'].
patterns -> pattern ',' patterns : ['$1' | '$3'].


expr -> basic_type : '$1'.
expr -> list : '$1'.
expr -> tuple : '$1'.
expr -> function_call : '$1'.

expr -> expr add_op expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
expr -> expr mult_op expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
expr -> prefix_op expr:
   case '$2' of
       {float, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {float, Pos, -N};
	       {'+', _} ->
		   {float, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {float, Pos, N}}
	   end;
       {integer, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {integer, Pos, -N};
	       {'+', _} ->
		   {integer, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {integer, Pos, N}}
	   end;
       _ ->
	   {Op, Pos} = '$1',
	   {arith, Pos, Op, '$2'}
   end.

expr -> '(' expr ')' : '$2'.
expr -> 'begin' exprs 'end' : {block, element(2, '$1'), '$2'}.
expr -> 'catch' expr : {'catch', element(2, '$1'), '$2'}.

expr -> case_expr : '$1'.
expr -> if_expr : '$1'.
expr -> receive_expr : '$1'.
expr -> match_expr : '$1'.
expr -> send_expr : '$1'.


list -> '[' ']' : {nil, element(2, '$1')}.
list -> '[' expr expr_tail ']' : {cons, element(2, '$1'), '$2', '$3'}.

expr_tail -> '|' expr : '$2'.
expr_tail -> ',' expr expr_tail : {cons, element(2, '$2'), '$2', '$3'}.
expr_tail -> '$empty' : {nil, 0}.

tuple -> '{' '}' : {tuple, element(2, '$1'), []}.
tuple -> '{' exprs '}' : {tuple, element(2, '$1'), '$2'}.


function_call -> atom '(' parameter_list ')' :
   case erl_parse:erlang_bif(element(3, '$1'), length('$3')) of
       true ->
	   {bif, element(2, '$1'), element(3, '$1'), '$3'};
       false ->
	   {call, element(2, '$1'), [], element(3, '$1'), '$3'}
   end.
function_call -> atom ':' atom '(' parameter_list ')' :
   {call, element(2, '$1'), element(3, '$1'), element(3, '$3'), '$5'}.

parameter_list -> exprs : '$1'.
parameter_list -> '$empty' : [].


case_expr -> 'case' expr 'of' cr_clauses 'end' :
   {'case', element(2, '$1'), '$2', '$4'}.

cr_clause -> pattern clause_guard clause_body :
   {clause, element(2, '$1'), ['$1'], '$2', '$3'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

if_expr -> 'if' if_clauses 'end' : {'if', element(2, '$1'), '$2'}.

if_clause -> guard clause_body : {clause, element(2, hd('$2')), '$1', '$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

receive_expr -> 'receive' 'after' expr clause_body 'end' :
   {'receive', element(2, '$1'), [], '$3', '$4'}.
receive_expr -> 'receive' cr_clauses 'end' :
   {'receive', element(2, '$1'), '$2'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
   {'receive', element(2, '$1'), '$2', '$4', '$5'}.


match_expr -> expr '=' expr :
   case erl_parse:is_term('$1') of
       true ->
	   {match, element(2, '$1'), '$1', '$3'};
       false ->
	   throw({error, {element(2, '$1'), yecc, "illegal lhs in match **"}})
   end.

send_expr -> expr '!' expr :
   Pos = element(2, '$1'),
   {send, Pos, '$1', '$3'}.


exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].


guard_expr -> basic_type : '$1'.
guard_expr -> guard_expr_list : '$1'.
guard_expr -> guard_expr_tuple : '$1'.
guard_expr -> guard_call : '$1'.
guard_expr -> '(' guard_expr ')' : '$2'.
guard_expr -> guard_expr add_op guard_expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
guard_expr -> guard_expr mult_op guard_expr :
   {Op, Pos} = '$2',
   {arith, Pos, Op, '$1', '$3'}.
guard_expr -> prefix_op guard_expr:
   case '$2' of
       {float, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {float, Pos, -N};
	       {'+', _} ->
		   {float, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {float, Pos, N}}
	   end;
       {integer, Pos, N} ->
	   case '$1' of
	       {'-', _} ->
		   {integer, Pos, -N};
	       {'+', _} ->
		   {integer, Pos, N};
	       {Op, Pos1} ->
		   {arith, Pos1, Op, {integer, Pos, N}}
	   end;
       _ ->
	   {Op, Pos} = '$1',
	   {arith, Pos, Op, '$2'}
   end.

guard_expr_list -> '[' ']' : {nil, element(2, '$1')}.
guard_expr_list -> '[' guard_expr guard_expr_tail ']' :
   {cons, element(2, '$1'), '$2', '$3'}.

guard_expr_tail -> '|' guard_expr : '$2'.
guard_expr_tail -> ',' guard_expr guard_expr_tail :
 {cons, element(2, '$2'), '$2', '$3'}.
guard_expr_tail -> '$empty' : {nil, 0}.

guard_expr_tuple -> '{' '}' : {tuple, element(2, '$1'), []}.
guard_expr_tuple -> '{' guard_exprs '}' : {tuple, element(2, '$1'), '$2'}.

guard_exprs -> guard_expr : ['$1'].
guard_exprs -> guard_expr ',' guard_exprs : ['$1' | '$3'].


guard_call -> atom '(' guard_parameter_list ')' :
   case erl_parse:erlang_guard_bif(element(3, '$1'), length('$3')) of
       true ->
	   {bif, element(2, '$1'), element(3, '$1'), '$3'};
       false ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.

guard_parameter_list -> guard_exprs : '$1'.
guard_parameter_list -> '$empty' : [].


bif_test -> atom '(' guard_parameter_list ')' :
   case erl_parse:erlang_guard_test(element(3, '$1'), length('$3')) of
       true ->
	   {test, element(2, '$1'), element(3, '$1'), '$3'};
       false ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.


guard_test -> bif_test : '$1'.
guard_test -> guard_expr comp_op guard_expr :
   {Op, Pos} = '$2',
   {comp, Pos, Op, '$1', '$3'}.

guard_tests -> guard_test : ['$1'].
guard_tests -> guard_test ',' guard_tests : ['$1' | '$3'].

% guard -> 'true' : [].
guard -> atom :
   case '$1' of
       {atom, _, true} ->
           [];
       _ ->
	   throw({error, {element(2, '$1'), yecc, "illegal test in guard **"}})
   end.
guard -> guard_tests : '$1'.


function_clause -> clause_head clause_guard clause_body :
   {Name, Line, Arity, Parameters} = '$1',
   {function, Line, Name, Arity,
    [{clause, element(2, hd('$3')), Parameters, '$2', '$3'}]}.

clause_head -> atom '(' formal_parameter_list ')' :
   {element(3, '$1'), element(2, '$1'), length('$3'), '$3'}.

formal_parameter_list -> patterns : '$1'.
formal_parameter_list -> '$empty' : [].

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.


function -> function_clause : '$1'.
function -> function_clause ';' function :
   case '$1' of
       {function, Pos1, Name1, Arity1, [Clause]} ->
	   case '$3' of
	       {function, _, Name1, Arity2, Clauses} ->
		   if
		       Arity1 /= Arity2 ->
			   throw({error, {Pos1, yecc,
				  io_lib:format('arity conflict in definition of ~w',
						[Name1])}});
		       true ->
			   {function, Pos1, Name1, Arity1, [Clause | Clauses]}
		   end;
	       _ ->
		   throw({error, {Pos1, yecc,
			  io_lib:format('missing final dot in def of ~w/~w',
					[Name1, Arity1])}})
	   end
   end.


attribute -> atom : element(3, '$1').
attribute -> '[' farity_list ']' : '$2'.

farity_list -> farity : ['$1'].
farity_list -> farity ',' farity_list : ['$1' | '$3'].

farity -> atom '/' integer : {element(3, '$1'), element(3, '$3')}.


form -> '-' atom '(' attribute ')' :
   {attribute, element(2, '$2'), element(3, '$2'), '$4'}.
form -> function : '$1'.
