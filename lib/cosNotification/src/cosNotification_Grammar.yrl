%%--------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File    : cosNotification_Grammar.yrl
%% Purpose : Implement the constraint grammar for CosNotification filters.
%%--------------------------------------------------------------------

Nonterminals  
	'<toplevel>' '<constraint>' '<expr>' '<bool>' '<bool_or>' '<Ident>'
	'<bool_and>' '<bool_compare>' '<expr_in>' '<expr_twiddle>' '<term>' 
	'<factor_not>' '<factor>' '<Component>' '<CompExt>' '<CompDot>' '<UnionVal>'.

Terminals 
%	'dbslsh' 'bslshd' 
	'bslsh' 'ident' 'string'
	'_length' '_d''_type_id' '_repos_id' 
	'not' 'or' 'and' 'num'
	'in' '~' '.' 'dollar'
	'ADDOP' 'RELOP' 'MULOP' 'default' 'exist'
	'TRUE' 'FALSE'
	'(' ')' '[' ']' 'int'.

Left 100 'or'.
Left 200 'and'.
%Nonassoc 300 'RELOP'.          % '==', '!=', '<', '>', '<=', '=>'
Left 300 'RELOP'.
%Nonassoc 400 'in'.
Left 400 'in'.
%Nonassoc 500 '~'.
Left 500 '~'.
Left 600 'ADDOP'.               % '+', '-'
Left 700 'MULOP'.               % '*', '/'
Unary 800 'not'.
Unary 900 'exist'.
Unary 900 'default'.
%Unary 900 'u-'.                 % unary minus

Rootsymbol    '<toplevel>'.
Endsymbol     '$end'.

'<toplevel>' -> '$empty' : '$empty'.
'<toplevel>' -> '<constraint>' : '$1'.

'<constraint>' -> '<bool>' : '$1'.
  
'<bool>' -> '<bool_or>' : '$1'.

'<bool_or>' -> '<bool_or>' 'or' '<bool_and>' : {'or', '$1', '$3'}.
'<bool_or>' -> '<bool_and>' : '$1'.

'<bool_and>' -> '<bool_and>' 'and' '<bool_compare>' : {'and', '$1', '$3'}. 
'<bool_and>' -> '<bool_compare>' : '$1'.

'<bool_compare>' -> '<expr_in>' 'RELOP' '<expr_in>' : {element(2, '$2'), '$1', '$3'}. 
'<bool_compare>' -> '<expr_in>' : '$1'.

'<expr_in>' -> '<expr_twiddle>' : '$1'.
'<expr_in>' -> '<expr_twiddle>' 'in' '<Ident>' : {'in', '$1', '$3'}.
'<expr_in>' -> '<expr_twiddle>' 'in' 'dollar' '<Component>' : {'in', '$1', examin_comp({'component', '$4'})}.

'<expr_twiddle>' -> '<expr>' : '$1'. 
'<expr_twiddle>' -> '<expr>' '~' '<expr>' : {'~', '$1', '$3'}.

'<expr>' -> '<term>' : '$1'.
'<expr>' -> '<expr>' 'ADDOP' '<term>' : {element(2, '$2'), '$1', '$3'}.

'<term>' -> '<factor_not>' : '$1'.
'<term>' -> '<term>' 'MULOP' '<factor_not>' : {element(2, '$2'), '$1', '$3'}.

'<factor_not>' -> '<factor>' : '$1'.
'<factor_not>' -> 'not' '<factor>' : {'not', '$2'}.

'<factor>' -> '(' '<bool_or>' ')' : '$2'. 
'<factor>' -> 'num' : element(2, '$1').
'<factor>' -> 'int' : element(2, '$1').
'<factor>' -> 'string' : element(2, '$1').
'<factor>' -> 'TRUE' : 'true'.
'<factor>' -> 'FALSE' : 'false'.
'<factor>' -> 'ADDOP' 'num' : create_unary(element(2, '$1'), element(2, '$2')).
'<factor>' -> 'ADDOP' 'int' : create_unary(element(2, '$1'), element(2, '$2')).
'<factor>' -> '<Ident>' : list_to_atom('$1').
'<factor>' -> 'dollar' '<Component>' : examin_comp({component, '$2'}).
'<factor>' -> 'default' 'dollar' '<Component>' : examin_comp({'default_component', '$3'}).
'<factor>' -> 'exist' 'dollar' '<Component>' : examin_comp({'exist_component', '$3'}).

%% The following rules are used to create Components. The format used is:
%% [...]
'<Component>' -> '.' '<CompDot>' : '$2'.
'<Component>' -> '[' 'int' ']' '<CompExt>' : [{'arrindex', element(2, '$2')} | '$4']. %% CompArray
'<Component>' -> '(' '<Ident>' ')' '<CompExt>' : [{'associd', '$2'} | '$4']. %%CompAssoc
'<Component>' ->  '<Ident>' '<CompExt>' : [{'varid', '$1'} | '$2']. %% run-time variable
'<Component>' ->  '$empty' : [].

'<CompExt>' ->  '.' '<CompDot>' : '$2'.
'<CompExt>' ->  '[' 'int' ']' '<CompExt>' : [{'arrindex', element(2, '$2')} | '$4']. %% CompArray
'<CompExt>' ->  '(' '<Ident>' ')' '<CompExt>' : [{'associd', '$2'} | '$4']. %%CompAssoc
'<CompExt>' ->  '$empty' : [].

'<CompDot>' -> '<Ident>' '<CompExt>' : [{'dotid', '$1'} | '$2']. 
'<CompDot>' -> 'int' '<CompExt>' : [{'dotint', element(2, '$1')} | '$2']. %% ComPos
'<CompDot>' -> '(' '<UnionVal>' ')' '<CompExt>' : ['$2' | '$4']. %% UnionPos
'<CompDot>' -> '_length'           : ['_length'].   %% arrays or sequences ONLY
'<CompDot>' -> '_d'                : ['_d'].        %% discriminated unions ONLY
'<CompDot>' -> '_type_id'          : ['_type_id'].  %% ok if info can be obtained
'<CompDot>' -> '_repos_id'         : ['_repos_id']. %% ok if info can be obtained

'<Ident>' -> 'ident' : element(2, '$1').
'<Ident>' -> 'bslsh' 'ident' : element(2, '$2').

'<UnionVal>' ->  'int' : {'uint', element(2, '$1')}.
'<UnionVal>' ->  'ADDOP' 'int' : {'uint', create_unary(element(2, '$1'), element(2, '$2'))}.
'<UnionVal>' ->  'string' : {'ustr', element(2, '$1')}.
'<UnionVal>' ->   '$empty': 'default'.

Erlang code.
%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : cosNotification_Grammar.erl
%% Purpose : THIS FILE HAS BEEN GENERATED. DO NOT EDIT!!!!
%%----------------------------------------------------------------------

-include("CosNotification_Definitions.hrl").

create_unary('+', Val) when is_number(Val) -> Val;
create_unary('-', Val) when is_number(Val) -> -Val;
create_unary(_, _) -> return_error(0, "syntax error").

examin_comp({T, []}) ->
	{T, '$empty'};
examin_comp(V) ->
	V.

