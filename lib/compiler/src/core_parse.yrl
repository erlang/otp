%% -*-Erlang-*-
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
%% Core Erlang YECC parser grammar

%% Have explicit productions for annotated phrases named anno_XXX.
%% This just does an XXX and adds the annotation.

Expect 0.

Nonterminals

module_definition module_export module_attribute module_defs
exported_names exported_name
attribute_list attribute
function_definition function_definitions

constant constants atomic_constant tuple_constant cons_constant tail_constant
other_pattern atomic_pattern tuple_pattern cons_pattern tail_pattern
binary_pattern segment_patterns segment_pattern

expression single_expression
literal literals atomic_literal tuple_literal cons_literal tail_literal fun_literal
nil tuple cons tail
binary segments segment

let_expr let_vars letrec_expr case_expr fun_expr
function_name
application_expr call_expr primop_expr arg_list
receive_expr timeout try_expr
sequence catch_expr
variable clause clause_pattern

map_expr anno_map_expr map_pairs anno_map_pair map_pair map_pair_assoc map_pair_exact
map_pattern map_pair_patterns map_pair_pattern

annotation anno_atom anno_fun anno_expression anno_expressions
anno_variable anno_variables anno_pattern anno_patterns
anno_function_name
anno_literal
anno_segment anno_segment_pattern
anno_clause anno_clauses.

Terminals

%% Separators

'(' ')' '{' '}' '[' ']' '|' ',' '->' '=' '/' '<' '>' ':' '-|' '#'
'~' '=>' ':='

%% Keywords (atoms are assumed to always be single-quoted).

'module' 'attributes' 'do' 'let' 'in' 'letrec'
'apply' 'call' 'primop'
'case' 'of' 'end' 'when' 'fun' 'try' 'catch' 'receive' 'after'

%% Literal tokens (provided by the tokeniser):

char integer float atom string var.

%% Literal tokens NOT provided by the tokenise:

nil -> '[' ']' : {nil,tok_line('$1')}. 

%% Declare the start rule for parsing

Rootsymbol module_definition.


%% Grammar

module_definition ->
    'module' atom module_export module_attribute module_defs 'end' :
	#c_module{name=#c_literal{val=tok_val('$2')},exports='$3',
		  attrs='$4',defs='$5'}.
module_definition ->
    '(' 'module' atom module_export module_attribute module_defs 'end'
	'-|' annotation ')' :
        #c_module{anno='$9',name=#c_literal{val=tok_val('$3')},exports='$4',
		  attrs='$5',defs='$6'}.

module_export -> '[' ']' : [].
module_export -> '[' exported_names ']' : '$2'.

exported_names -> exported_name ',' exported_names : ['$1' | '$3'].
exported_names -> exported_name : ['$1'].

exported_name -> anno_function_name : '$1'.

module_attribute -> 'attributes' '[' ']' : [].
module_attribute -> 'attributes' '[' attribute_list ']' : '$3'.

attribute_list -> attribute ',' attribute_list : ['$1' | '$3'].
attribute_list -> attribute : ['$1'].

attribute -> anno_atom '=' anno_literal :
	{'$1','$3'}.

anno_atom -> atom :
         cerl:c_atom(tok_val('$1')).
anno_atom -> '(' atom '-|' annotation ')' :
         cerl:ann_c_atom('$4', tok_val('$2')).

anno_literal -> literal : '$1'.
anno_literal -> '(' literal '-|' annotation ')' : cerl:set_ann('$2', '$4').

module_defs -> function_definitions : '$1'.

annotation -> '[' ']' : [].
annotation -> '[' constants ']' : '$2'.

function_definitions ->
    function_definition function_definitions : ['$1' | '$2'].
function_definitions ->
    '$empty' : [].

function_definition ->
    anno_function_name '=' anno_fun :
	{'$1','$3'}.

anno_fun -> '(' fun_expr '-|' annotation ')' :
	cerl:set_ann('$2', '$4').
anno_fun -> fun_expr : '$1'.

%% Constant terms for annotations and attributes.
%%  These are represented by straight unabstract Erlang.

constant -> atomic_constant : '$1'.
constant -> tuple_constant : '$1'.
constant -> cons_constant : '$1'.

constants -> constant ',' constants : ['$1' | '$3'].
constants -> constant : ['$1'].

atomic_constant -> char : tok_val('$1').
atomic_constant -> integer : tok_val('$1').
atomic_constant -> float : tok_val('$1').
atomic_constant -> atom : tok_val('$1').
atomic_constant -> string : tok_val('$1').
atomic_constant -> nil : [].

tuple_constant -> '{' '}' : {}.
tuple_constant -> '{' constants '}' : list_to_tuple('$2').

cons_constant -> '[' constant tail_constant : ['$2'|'$3'].

tail_constant -> ']' : [].
tail_constant -> '|' constant ']' : '$2'.
tail_constant -> ',' constant tail_constant : ['$2'|'$3'].

%% Patterns
%%  We have to be a little sneaky here as we would like to be able to
%%  do:
%%  V = {a}
%%  ( V = {a} -| <anno> )
%%  ( V -| <anno> ) = {a}
%%  V = ( {a} -| <anno> )
%%  ( ( V -| <anno> ) = ( {a} -| <anno> ) -| <anno> )

anno_pattern -> '(' other_pattern '-|' annotation ')' :
	cerl:set_ann('$2', '$4').
anno_pattern -> other_pattern : '$1'.
anno_pattern -> anno_variable : '$1'.

anno_patterns -> anno_pattern ',' anno_patterns : ['$1' | '$3'].
anno_patterns -> anno_pattern : ['$1'].

other_pattern -> atomic_pattern : '$1'.
other_pattern -> tuple_pattern : '$1'.
other_pattern -> map_pattern : '$1'.
other_pattern -> cons_pattern : '$1'.
other_pattern -> binary_pattern : '$1'.
other_pattern -> anno_variable '=' anno_pattern :
	#c_alias{var='$1',pat='$3'}.

atomic_pattern -> atomic_literal : '$1'.

tuple_pattern -> '{' '}' : c_tuple([]).
tuple_pattern -> '{' anno_patterns '}' : c_tuple('$2').

map_pattern -> '~' '{' '}' '~' : c_map_pattern([]).
map_pattern -> '~' '{' map_pair_patterns '}' '~' :
		   c_map_pattern('$3').
map_pattern -> '~' '{' map_pair_patterns '|' anno_map_expr '}' '~' :
		   ann_c_map_pattern('$5', '$3').

map_pair_patterns -> map_pair_pattern : ['$1'].
map_pair_patterns -> map_pair_pattern ',' map_pair_patterns : ['$1' | '$3'].

map_pair_pattern -> anno_expression ':=' anno_pattern :
			#c_map_pair{op=#c_literal{val=exact},
				    key='$1',val='$3'}.
map_pair_pattern -> '(' anno_expression ':=' anno_pattern '-|' annotation ')' :
			#c_map_pair{anno='$6',op=#c_literal{val=exact},
				    key='$2',val='$4'}.

cons_pattern -> '[' anno_pattern tail_pattern :
		    c_cons('$2', '$3').

tail_pattern -> ']' : #c_literal{val=[]}.
tail_pattern -> '|' anno_pattern ']' : '$2'.
tail_pattern -> ',' anno_pattern tail_pattern :
		    c_cons('$2', '$3').

binary_pattern -> '#' '{' '}' '#' : #c_binary{segments=[]}.
binary_pattern -> '#' '{' segment_patterns '}' '#' : #c_binary{segments='$3'}.

segment_patterns -> anno_segment_pattern ',' segment_patterns : ['$1' | '$3'].
segment_patterns -> anno_segment_pattern : ['$1'].

anno_segment_pattern -> segment_pattern : '$1'.
anno_segment_pattern -> '(' segment_pattern '-|' annotation ')' :
      cerl:set_ann('$2', '$4').

segment_pattern -> '#' '<' anno_pattern '>' '(' anno_expressions ')':
	case '$6' of
	    [S,U,T,Fs] ->
		#c_bitstr{val='$3',size=S,unit=U,type=T,flags=Fs};
	    true ->
		return_error(tok_line('$1'),
			     "expected 4 arguments in binary segment")
	end.

variable -> var : #c_var{name=tok_val('$1')}.

anno_variables -> anno_variable ',' anno_variables : ['$1' | '$3'].
anno_variables -> anno_variable : ['$1'].

anno_variable -> variable : '$1'.
anno_variable -> '(' variable '-|' annotation ')' :
	cerl:set_ann('$2', '$4').

%% Expressions
%%  Must split expressions into two levels as nested value expressions
%%  are illegal.

anno_expression -> expression : '$1'.
anno_expression -> '(' expression '-|' annotation ')' :
	cerl:set_ann('$2', '$4').

anno_expressions -> anno_expression ',' anno_expressions : ['$1' | '$3'].
anno_expressions -> anno_expression : ['$1'].

expression -> '<' '>' : #c_values{es=[]}.
expression -> '<' anno_expressions '>' : #c_values{es='$2'}.
expression -> single_expression : '$1'.

single_expression -> atomic_literal : '$1'.
single_expression -> tuple : '$1'.
single_expression -> cons : '$1'.
single_expression -> binary : '$1'.
single_expression -> variable : '$1'.
single_expression -> function_name : '$1'.
single_expression -> fun_literal : '$1'.
single_expression -> fun_expr : '$1'.
single_expression -> let_expr : '$1'.
single_expression -> letrec_expr : '$1'.
single_expression -> case_expr : '$1'.
single_expression -> receive_expr : '$1'.
single_expression -> application_expr : '$1'.
single_expression -> call_expr : '$1'.
single_expression -> primop_expr : '$1'.
single_expression -> try_expr : '$1'.
single_expression -> sequence : '$1'.
single_expression -> catch_expr : '$1'.
single_expression -> map_expr : '$1'.

literal -> atomic_literal : '$1'.
literal -> tuple_literal : '$1'.
literal -> cons_literal : '$1'.

literals -> literal ',' literals : ['$1' | '$3'].
literals -> literal : ['$1'].

atomic_literal -> char : #c_literal{val=tok_val('$1')}.
atomic_literal -> integer : #c_literal{val=tok_val('$1')}.
atomic_literal -> float : #c_literal{val=tok_val('$1')}.
atomic_literal -> atom : #c_literal{val=tok_val('$1')}.
atomic_literal -> string : #c_literal{val=tok_val('$1')}.
atomic_literal -> nil : #c_literal{val=[]}.

tuple_literal -> '{' '}' : c_tuple([]).
tuple_literal -> '{' literals '}' : c_tuple('$2').

cons_literal -> '[' literal tail_literal : c_cons('$2', '$3').

tail_literal -> ']' : #c_literal{val=[]}.
tail_literal -> '|' literal ']' : '$2'.
tail_literal -> ',' literal tail_literal : c_cons('$2', '$3').

fun_literal -> 'fun' atom ':' atom '/' integer :
	#c_literal{val = erlang:make_fun(tok_val('$2'), tok_val('$4'), tok_val('$6'))}.

tuple -> '{' '}' : c_tuple([]).
tuple -> '{' anno_expressions '}' : c_tuple('$2').

map_expr -> '~' '{' '}' '~' : c_map([]).
map_expr -> '~' '{' map_pairs '}' '~' : c_map('$3').
map_expr -> '~' '{' map_pairs '|' anno_variable '}' '~' : ann_c_map([], '$5', '$3').
map_expr -> '~' '{' map_pairs '|' anno_map_expr '}' '~' : ann_c_map([], '$5', '$3').

anno_map_expr -> map_expr : '$1'.
anno_map_expr -> '(' map_expr '-|' annotation ')' : cerl:set_ann('$2', '$4').

map_pairs -> anno_map_pair : ['$1'].
map_pairs -> anno_map_pair ',' map_pairs : ['$1' | '$3'].

anno_map_pair -> map_pair : '$1'.
anno_map_pair -> '(' map_pair '-|' annotation ')' : cerl:set_ann('$2', '$4').

map_pair -> map_pair_assoc : '$1'.
map_pair -> map_pair_exact : '$1'.

map_pair_assoc -> anno_expression '=>' anno_expression :
		#c_map_pair{op=#c_literal{val=assoc},key='$1',val='$3'}.
map_pair_exact -> anno_expression ':=' anno_expression :
		#c_map_pair{op=#c_literal{val=exact},key='$1',val='$3'}.

cons -> '[' anno_expression tail : c_cons('$2', '$3').

tail -> ']' : #c_literal{val=[]}.
tail -> '|' anno_expression ']' : '$2'.
tail -> ',' anno_expression tail : c_cons('$2', '$3').

binary -> '#' '{' '}' '#' : #c_literal{val = <<>>}.
binary -> '#' '{' segments '}' '#' : make_binary('$3').

segments -> anno_segment ',' segments : ['$1' | '$3'].
segments -> anno_segment : ['$1'].

anno_segment -> segment : '$1'.
anno_segment -> '(' segment '-|' annotation ')' : cerl:set_ann('$2', '$4').

segment -> '#' '<' anno_expression '>' '(' anno_expressions ')':
	case '$6' of
	    [S,U,T,Fs] ->
		#c_bitstr{val='$3',size=S,unit=U,type=T,flags=Fs};
	    true ->
		return_error(tok_line('$1'),
			     "expected 4 arguments in binary segment")
	end.

function_name -> atom '/' integer :
	#c_var{name={tok_val('$1'),tok_val('$3')}}.

anno_function_name -> function_name : '$1'.
anno_function_name -> '(' function_name '-|' annotation ')' :
	cerl:set_ann('$2', '$4').

let_vars -> anno_variable : ['$1'].
let_vars -> '<' '>' : [].
let_vars -> '<' anno_variables '>' : '$2'.

sequence -> 'do' anno_expression anno_expression :
	#c_seq{arg='$2',body='$3'}.

fun_expr -> 'fun' '(' ')' '->' anno_expression :
	#c_fun{vars=[],body='$5'}.
fun_expr -> 'fun' '(' anno_variables ')' '->' anno_expression :
	#c_fun{vars='$3',body='$6'}.

let_expr -> 'let' let_vars '=' anno_expression 'in' anno_expression :
	#c_let{vars='$2',arg='$4',body='$6'}.

letrec_expr -> 'letrec' function_definitions 'in' anno_expression :
	#c_letrec{defs='$2',body='$4'}.

case_expr -> 'case' anno_expression 'of' anno_clauses 'end' :
	#c_case{arg='$2',clauses='$4'}.

anno_clauses -> anno_clause anno_clauses : ['$1' | '$2'].
anno_clauses -> anno_clause : ['$1'].

anno_clause -> clause : '$1'.
anno_clause -> '(' clause '-|' annotation ')' :
	cerl:set_ann('$2', '$4').

clause -> clause_pattern 'when' anno_expression '->' anno_expression :
	#c_clause{pats='$1',guard='$3',body='$5'}.

clause_pattern -> anno_pattern : ['$1'].
clause_pattern -> '<' '>' : [].
clause_pattern -> '<' anno_patterns '>' : '$2'.

application_expr -> 'apply' anno_expression arg_list :
	#c_apply{op='$2',args='$3'}.

call_expr ->
    'call' anno_expression ':' anno_expression arg_list :
	#c_call{module='$2',name='$4',args='$5'}.

primop_expr -> 'primop' anno_expression arg_list :
	#c_primop{name='$2',args='$3'}.

arg_list -> '(' ')' : [].
arg_list -> '(' anno_expressions ')' : '$2'.

try_expr ->
    'try' anno_expression 'of' let_vars '->' anno_expression
	'catch' let_vars '->' anno_expression :
	Len = length('$8'),
        if Len =:= 2; Len =:= 3 ->
		#c_try{arg='$2',vars='$4',body='$6',evars='$8',handler='$10'};
	   true ->
		return_error(tok_line('$7'),
			     "expected 2 or 3 exception variables in 'try'")
	end.

catch_expr -> 'catch' anno_expression : #c_catch{body='$2'}.

receive_expr -> 'receive' timeout : 
	{T,A} = '$2',
	#c_receive{clauses=[],timeout=T,action=A}.
receive_expr -> 'receive' anno_clauses timeout : 
	{T,A} = '$3',
	#c_receive{clauses='$2',timeout=T,action=A}.

timeout ->
    'after' anno_expression '->' anno_expression : {'$2','$4'}.

%% ====================================================================== %%

Header
"%% This file was automatically generated from the file \"core_parse.yrl\"."
"%%"
"%% Copyright Ericsson AB 1999-2009. All Rights Reserved."
"%%"
"%% Licensed under the Apache License, Version 2.0 (the \"License\"); you may"
"%% not use this file except in compliance with the License. You may obtain"
"%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>"
"%%"
"%% Unless required by applicable law or agreed to in writing, software"
"%% distributed under the License is distributed on an \"AS IS\" BASIS,"
"%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
"%% See the License for the specific language governing permissions and"
"%% limitations under the License."
"".

Erlang code.

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("core_parse.hrl").

-import(cerl, [ann_c_map/3,ann_c_map_pattern/2,c_cons/2,c_map/1,
	       c_map_pattern/1,c_tuple/1]).

tok_val(T) -> element(3, T).
tok_line(T) -> element(2, T).

%% make_binary([#c_bitstr{}]) -> #c_binary{} | #c_literal{}
%%  Create either #c_binary{} or #c_literal{} from the binary segments.
%%  In certain contexts, such as keys for maps, only literals and
%%  variables are allowed, so we must not create a #c_binary{}
%%  record in those situation.
%%
%%  To keep this function simple, we use a crude heuristic. We will
%%  assume that Core Erlang has been produced by core_pp. If the
%%  segments *could* have been output from a literal binary by
%%  core_pp, we will create a #c_literal{}. Otherwise we will create a
%%  #c_binary{} record.

make_binary(Segs) ->
    try make_lit_bin(<<>>, Segs) of
	Bs when is_bitstring(Bs) ->
	    #c_literal{val=Bs}
    catch
	throw:impossible ->
	    #c_binary{segments=Segs}
    end.

make_lit_bin(Acc, [#c_bitstr{val=I0,size=Sz0,unit=U0,type=Type0,flags=F0}|T]) ->
    I = get_lit_val(I0),
    Sz = get_lit_val(Sz0),
    U = get_lit_val(U0),
    Type = get_lit_val(Type0),
    F = get_lit_val(F0),
    if
	is_integer(I), U =:= 1, Type =:= integer, F =:= [unsigned,big] ->
	    ok;
	true ->
	    throw(impossible)
    end,
    if
	0 =< Sz, Sz =< 8, T =:= [] ->
	    <<Acc/binary,I:Sz>>;
	Sz =:= 8 ->
	    make_lit_bin(<<Acc/binary,I:8>>, T);
	true ->
	    throw(impossible)
    end;
make_lit_bin(Acc, []) -> Acc.

get_lit_val(#c_literal{val=Val}) -> Val;
get_lit_val(_) -> throw(impossible).

%% vim: syntax=erlang
