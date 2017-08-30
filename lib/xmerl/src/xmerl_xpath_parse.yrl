%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%% Description  : Yecc spec for XPATH grammar
%%    This version of the parser is based on the XPATH spec:
%%    http://www.w3.org/TR/1999/REC-xpath-19991116 (XPATH version 1.0)


Nonterminals
	'LocationPath'
	'AbsoluteLocationPath'
	'RelativeLocationPath'
	'Step'
%%	'AxisSpecifier'
	'NodeTest'
	'Predicate'
	'PredicateExpr'
	'AbbreviatedAbsoluteLocationPath'
	'AbbreviatedRelativeLocationPath'
	'AbbreviatedStep'
%%	'AbbreviatedAxisSpecifier'
	'Expr'
	'PrimaryExpr'
	'FunctionCall'
	'Argument'
	'UnionExpr'
	'PathExpr'
	'FilterExpr'
	'OrExpr'
	'AndExpr'
	'EqualityExpr'
	'RelationalExpr'
	'AdditiveExpr'
	'MultiplicativeExpr'
	'UnaryExpr'
%%	'Operator'
%%	'OperatorName'
	'MultiplyOperator'
	'NameTest'
	'<PredicateList>'
	'<PredicateMember>'
	'<ArgumentList>'
	'<ArgumentMember>'
	.

Terminals
	'number'
	'axis'
	'node_type'
	'literal'
	'prefix_test'
	'var_reference'
	'function_name'
	'name'
	'processing-instruction'
	'wildcard'
	'(' ')' '[' ']' '.' '..' '@' ',' '::'	
	'and' 'or' 'mod' 'div'
	'/' '//' '|' '+' '-' '=' '!=' '<' '<=' '>' '>='
	'*'
	.

Rootsymbol 'Expr'.

Endsymbol '$end' .

Left 100 'or' .
Left 200 'and' .
Left 300 '=' .
Left 300 '!=' .
Left 400 '<' .
Left 400 '>=' .
Left 400 '>' .
Left 400 '<=' .
Unary 500 '-' .

Expect 2.

%%------------------------------------------------------------
%% Clauses
%%

%% [1]
'LocationPath' -> 'RelativeLocationPath' : {path, rel, '$1'} .
'LocationPath' -> 'AbsoluteLocationPath' : {path, abs, '$1'}.

%% [2]
'AbsoluteLocationPath' -> '/' 'RelativeLocationPath' : '$2' .
'AbsoluteLocationPath' -> '/' : '/' .

%% [3]
'RelativeLocationPath' -> 'AbbreviatedAbsoluteLocationPath' : '$1' .
'RelativeLocationPath' -> 'Step' : '$1' .
'RelativeLocationPath' -> 'RelativeLocationPath' '/' 'Step' : 
	{refine, '$1', '$3'} .
'RelativeLocationPath' -> 'AbbreviatedRelativeLocationPath' : '$1' .

%% [4]
'Step' -> 'axis' '::' 'NodeTest' '<PredicateList>' 
	: {step, {value('$1'), '$3', '$4'}} .
'Step' -> 'axis' '::' 'NodeTest' 
	: {step, {value('$1'), '$3', []}} .
'Step' -> '@' 'name' '<PredicateList>' 
	: {step, {value('$1'), '$2', '$3'}} .
'Step' -> '@' 'name' 
	: {step, {'attribute', '$2', []}} .
'Step' -> 'NodeTest' '<PredicateList>'
	: {step, {'child', '$1', '$2'}} .
'Step' -> 'NodeTest' 
	: {step, {'child', '$1', []}} .
'Step' -> 'AbbreviatedStep' 
	: {abbrev_step, '$1'} .


'<PredicateList>' -> '<PredicateMember>' : lists:reverse('$1') .


'<PredicateMember>' -> '<PredicateMember>' 'Predicate'
	: ['$2'|'$1'] .
'<PredicateMember>' -> 'Predicate' : ['$1'] .


%% [5]
%% 'AxisSpecifier' -> 'axis' '::' : '$1' .
%% 'AxisSpecifier' -> 'AbbreviatedAxisSpecifier' : '$1' .


%% [7]
'NodeTest' -> 'NameTest' : '$1' .
'NodeTest' -> 'node_type' '(' ')' : {node_type, value('$1')} .
'NodeTest' -> 'processing-instruction' '(' ')' : {node_type, value('$1')} .
'NodeTest' -> 'processing-instruction' '(' 'literal' ')' 
	: {processing_instruction, value('$3')} .


%% [8]
'Predicate' -> '[' 'PredicateExpr' ']' : {pred, '$2'} .

%% [9]
'PredicateExpr' -> 'Expr' : '$1' .

%% [10] 
'AbbreviatedAbsoluteLocationPath'  -> '//' 'RelativeLocationPath'
	: {'//', '$2'} .

%% [11] 
'AbbreviatedRelativeLocationPath' -> 'RelativeLocationPath' '//' 'Step'
	: {'$1', '//', '$3'} .

%% [12]
'AbbreviatedStep' -> '.' : '$1' .
'AbbreviatedStep' -> '..' : '$1' .

%% [13]
%% 'AbbreviatedAxisSpecifier' ->  '$empty' : 'child' .
%% 'AbbreviatedAxisSpecifier' ->  '@' : '$1' .

%% [14]
'Expr' -> 'OrExpr' : '$1' .

%% [15]
'PrimaryExpr' -> 'var_reference' : {variable_reference, value('$1')} .
'PrimaryExpr' -> '(' Expr ')' : '$2' .
'PrimaryExpr' -> 'literal' : {literal, value('$1')} .
'PrimaryExpr' -> 'number' : {number, value('$1')} .
'PrimaryExpr' -> 'FunctionCall' : '$1' .


%% [16]
'FunctionCall' -> 'function_name' '(' ')' : {function_call, value('$1'), []} .
'FunctionCall' -> 'function_name' '(' '<ArgumentList>' ')'
	: {function_call, value('$1'), '$3'} .

'<ArgumentList>' -> '<ArgumentMember>' : lists:reverse('$1') .

'<ArgumentMember>' -> '<ArgumentMember>' ',' 'Argument'
	: ['$3'|'$1'] .
'<ArgumentMember>' -> 'Argument' : ['$1'] .


%% [17]
'Argument' -> 'Expr' : '$1' .


%% [18]
'UnionExpr' -> 'PathExpr' : '$1' .
'UnionExpr' -> 'UnionExpr' '|' 'PathExpr' : {path, union, {'$1', '$3'}} .


%% [19]
'PathExpr' -> 'LocationPath' : '$1' .
'PathExpr' -> 'FilterExpr' : '$1' .
'PathExpr' -> 'FilterExpr' '/' 'RelativeLocationPath' : {refine, '$1', '$3'} .
'PathExpr' -> 'FilterExpr' '//' 'RelativeLocationPath' : {'$1', '//', '$3'} .

%% [20]
'FilterExpr' -> 'PrimaryExpr' : '$1' .
'FilterExpr' -> 'FilterExpr' 'Predicate' : {path, filter, {'$1', '$2'}} .


%% [21]
'OrExpr' -> 'AndExpr' : '$1' .
'OrExpr' -> 'OrExpr' 'or' 'AndExpr' 
	: {bool, 'or', '$1', '$3'} .


%% [22]
'AndExpr' -> 'EqualityExpr' : '$1' .
'AndExpr' -> 'AndExpr' 'and' 'EqualityExpr' 
	: {bool, 'and', '$1', '$3'} .

%% [23]
'EqualityExpr' -> 'RelationalExpr' : '$1' .
'EqualityExpr' -> 'EqualityExpr' '=' 'RelationalExpr' 
	: {comp, '=', '$1', '$3'} .
'EqualityExpr' -> 'EqualityExpr' '!=' 'RelationalExpr' 
	: {comp, '!=', '$1', '$3'} .

%%[24]
'RelationalExpr' -> 'AdditiveExpr' : '$1' .
'RelationalExpr' -> 'RelationalExpr' '<' 'AdditiveExpr' 
	: {comp, '<', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '>' 'AdditiveExpr' 
	: {comp, '>', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '<=' 'AdditiveExpr' 
	: {comp, '<=', '$1', '$3'} .
'RelationalExpr' -> 'RelationalExpr' '>=' 'AdditiveExpr' 
	: {comp, '>=', '$1', '$3'} .


%% [25]
'AdditiveExpr' -> 'MultiplicativeExpr' : '$1' .
'AdditiveExpr' -> 'AdditiveExpr' '+' 'MultiplicativeExpr'
	: {arith, '+', '$1', '$3'} .
'AdditiveExpr' -> 'AdditiveExpr' '-' 'MultiplicativeExpr'
	: {arith, '-', '$1', '$3'} .


%% [26]
'MultiplicativeExpr' -> 'UnaryExpr' : '$1' .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'MultiplyOperator' 'UnaryExpr'
	: {arith, '$2', '$1', '$3'} .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'div' 'UnaryExpr' 
	: {arith, 'div', '$1', '$3'} .
'MultiplicativeExpr' -> 'MultiplicativeExpr' 'mod' 'UnaryExpr'
	: {arith, 'mod', '$1', '$3'} .


%% [27]
'UnaryExpr' -> 'UnionExpr' : '$1' .
'UnaryExpr' -> '-' UnaryExpr : {'negative', '$2'} .



%% [32]
%% 'Operator' -> 'OperatorName' : '$1' .
%% 'Operator' -> 'MultiplyOperator' : '$1' .
%% 'Operator' -> '/' : '$1' .
%% 'Operator' -> '//' : '$1' .
%% 'Operator' -> '|' : '$1' .
%% 'Operator' -> '+' : '$1' .
%% 'Operator' -> '-' : '$1' .
%% 'Operator' -> '=' : '$1' .
%% 'Operator' -> '!=' : '$1' .
%% 'Operator' -> '<' : '$1' .
%% 'Operator' -> '<=' : '$1' .
%% 'Operator' -> '>' : '$1' .
%% 'Operator' -> '>=' : '$1' .

%% [33]
%% 'OperatorName' -> 'and' : '$1' .
%% 'OperatorName' -> 'mod' : '$1' .
%% 'OperatorName' -> 'div' : '$1' .

%% [34]
'MultiplyOperator' -> '*' : '*' .


%% [37]
'NameTest' -> 'wildcard' : {wildcard, value('$1')} .
'NameTest' -> 'prefix_test' : {prefix_test, value('$1')} .
'NameTest' -> 'name' : {name, value('$1')} .



Erlang code.

% token({Token, _Line}) ->
% 	Token;
% token({Token, _Line, _Value}) ->
% 	Token.

value({Token, _Line}) ->
	Token;
value({_Token, _Line, Value}) ->
	Value.
