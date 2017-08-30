%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%------------------------------------------------------------
%% Yecc spec for IDL
%% 
%% 
%% 
%% Implementation Detail:
%% OorM_ means OneORMany and is used instead of 
%%       the "+" BNF notation
%% ZorM_ means ZeroORMany and is used instead of 
%%       the "*" BNF notation
%%
%%	All the reverse/1 calls are because yecc+lists naturally leads
%%	to reversed lists, which then have to be reversed. Maybe fix
%%	this?
%%
%% Implementation history
%%
%%	The IDL language supported is not the complete IDL. We skipped
%%	the multiple declarator syntax allowed (i.e. typedef long T1,
%%	T2). This also applies to attributes members in structs,
%%	unions and exceptions, and to case labels in unions. The cases
%%	where IDL has been altered is marked with comments containing
%%	NIY.
%%
%%	Above is chaging. Whenever we change a clause, we put (FIXED) in
%%	its comment.
%% 
%%------------------------------------------------------------





Nonterminals
	'<op_type_spec>'
	'<enumerator>'
	'<switch_body>'
	'OorM_<case>'
	'<member_list>'
	'<struct_type>'
	'<unsigned_int>'
	'<constr_type_spec>'
	'<shift_expr>'
	'<or_expr>'
	'<inheritance_spec>'
	'ZorM_<param_dcl>'
	'Opt_<context_expr>'
	'<attr_dcl>'
	'<array_declarator>'
	'<element_spec>'
	'<signed_int>'
	'<primary_expr>'
	'<interface_dcl>'
	'ZorM_<string_literal>'
	'Opt_<raises_expr>'
	'<integer_type>'
	'<signed_long_int>'
	'<literal>'
	'<export>'
	'<forward_dcl>'
	'OorM_<definition>'
	'<base_type_spec>'
	'<op_dcl>'
	'<const_exp>'
	'<case>'
	'<any_type>'
	'<signed_short_int>'
	'<unary_expr>'
	'<context_expr>'
	'ZorM_<scoped_name>'
	'<switch_type_spec>'
	'<complex_declarator>'
	'<declarators>'
	'OorM_<member>'
	'<interface>'
	'<parameter_dcls>'
	'<op_attribute>'
	'<positive_int_const>'
	'OorM_<fixed_array_size>'
	'<sequence_type>'
	'<case_label>'
	'<octet_type>'
	'<type_dcl>'
	'<module>'
	'<specification>'
	'<declarator>'
	'<boolean_type>'
	'<union_type>'
	'<add_expr>'
	'<interface_body>'
	'<except_dcl>'
	'<fixed_array_size>'
	'<unsigned_short_int>'
	'<boolean_literal>'
	'<and_expr>'
	'Opt_<inheritance_spec>'
	'<scoped_name>'
	'<param_type_spec>'
	'ZorM_<member>'
	'<char_type>'
	'<const_dcl>'
	'<param_dcl>'
	'ZorM_<simple_declarator>'
	'ZorM_<declarator>'
	'<const_type>'
	'<definition>'
	'<param_attribute>'
	'<simple_declarator>'
	'Opt_readonly'
	'<simple_type_spec>'
	'<enum_type>'
	'<type_spec>'
	'OorM_<case_label>'
	'<floating_pt_type>'
	'<template_type_spec>'
	'<mult_expr>'
	'<xor_expr>'
	'<string_type>'
	'<raises_expr>'
	'Opt_<op_attribute>'
	'ZorM_<enumerator>'
	'<member>'
	'<unsigned_long_int>'
	'<type_declarator>'
	'<unary_operator>'
	'ZorM_<export>'
	'<interface_header>'
	'OE_preproc'				% NON standard
	'OE_pragma'				% NON standard
	'Ugly_pragmas'				% NON standard
	'ZorM_<integer_literal>'
	'<fixed_pt_type>'
	'<fixed_pt_const_type>'
	'<constr_forward_decl>'
	.


Terminals
	'#'
	'in'
	'['
	'interface'
	'('
	'case'
	'union'
	'struct'
	'<character_literal>'
	'<wcharacter_literal>'
	')'
	']'
	'any'
	'long'
	'float'
	'out'
	'*'
	'^'
	'enum'
	'double'
	'+'
	'context'
	'oneway'
	'sequence'
	','
	'FALSE'
	'<identifier>'
	'{'
	'readonly'
	':'
	'-'
	'void'
	';'
	'char'
	'wchar'          %% WCHAR
	'|'
	'inout'
	'}'
	'attribute'
	'<'
	'octet'
	'/'
	'TRUE'
	'~'
	'='
	'>'
	'switch'
	'unsigned'
	'typedef'
	'>>'
	'const'
	'<string_literal>'
	'<wstring_literal>'
	'raises'
	'string'
	'wstring'
	'fixed'
	'default'
	'short'
	'%'
	'<<'
	'module'
	'exception'
	'boolean'
	'<integer_literal>'
	'<fixed_pt_literal>'
	'<floating_pt_literal>'
	'&'
	'::'
	'Object'
	.


Rootsymbol '<specification>'.


Expect 9.


%%------------------------------------------------------------
%% Clauses
%%

%% Handling of pragmas.
%% Pragma prefix, id and version are not standard.

%% pragma prefix, or codeopt
OE_pragma -> '#'  '<integer_literal>' '<identifier>' 
	           '<identifier>' '<string_literal>' '#'
	: #pragma{type='$4', to=followed, apply='$5'} .

%% pragma id
OE_pragma -> '#'  '<integer_literal>' '<identifier>' 
	           '<identifier>' '<identifier>' '<string_literal>' '#'
	: #pragma{type='$4', to='$5', apply='$6'} .

%% pragma version
OE_pragma -> '#'  '<integer_literal>' '<identifier>' 
	           '<identifier>' '<identifier>' '<floating_pt_literal>' '#'
	: #pragma{type='$4', to='$5', apply=ic_options:float_to_version('$6')} .







%% Ugly pragmas
Ugly_pragmas -> '$empty' : [].
Ugly_pragmas -> 'Ugly_pragmas' 'OE_pragma' : ['$2'|'$1'].



%% (0) Handling of preprocessor stuff. 

OE_preproc -> '#' '#' .

OE_preproc -> '#' '<integer_literal>' '<string_literal>' 
	'ZorM_<integer_literal>' '#'
	: case '$4' of
	     [] ->
		case '$2' of
		     {_,_,"1"} ->
			 #preproc{cat=line_nr, id='$3', aux='$4'};
		   	_ ->
		     []
	        end;	
	     _ ->	 
	        #preproc{cat=line_nr, id='$3', aux='$4'}
          end.

%% (0b) Non-standard
'ZorM_<integer_literal>' -> '$empty' : [] .
'ZorM_<integer_literal>' -> '<integer_literal>' 'ZorM_<integer_literal>'
	: ['$1' | '$2'] .

%% (1)
'<specification>' -> 'OorM_<definition>' : reverse('$1') .


%% Added clause
'OorM_<definition>' -> '<definition>' : ['$1'] .
'OorM_<definition>' -> 'OorM_<definition>' '<definition>' 
: ['$2' | '$1'] .
		       

%% (2)
'<definition>' -> '<type_dcl>' ';' : '$1' .
'<definition>' -> '<const_dcl>' ';' : '$1' .
'<definition>' -> '<except_dcl>' ';' : '$1' .
'<definition>' -> '<interface>' ';' : '$1' .
'<definition>' -> '<module>' ';' : '$1' .
'<definition>' -> 'OE_preproc'  : '$1' .
'<definition>' -> 'OE_pragma'  : '$1' . 


%% (3)
'<module>' -> 'module' '<identifier>' '{' 'OorM_<definition>' '}'
: #module{ id='$2', body=reverse('$4')}.


%% (4)
'<interface>' -> '<interface_dcl>' : '$1' .
'<interface>' -> '<forward_dcl>' : '$1' .


%% (5)
'<interface_dcl>' -> '<interface_header>' '{' '<interface_body>' '}' 
  : #interface{id=element(1, '$1'), inherit=element(2, '$1'), 
	       body=lists:reverse('$3')} .


%% (6)
'<forward_dcl>' -> 'interface' '<identifier>'
: #forward{id='$2'} .


%% (7)
'<interface_header>' -> 'interface' '<identifier>' 'Opt_<inheritance_spec>'
: {'$2', '$3'} .


%% (8)
'<interface_body>' -> 'ZorM_<export>' : '$1' .


%% Added clause
'ZorM_<export>' -> '$empty' : [] .
'ZorM_<export>' -> 'ZorM_<export>' '<export>' 
  %% Complicated because <export> might be a list (of type defs for instance)
  : if  is_list('$2') -> '$2' ++ '$1';
        true       -> ['$2' | '$1']
    end .


%% (9)
'<export>' -> '<type_dcl>' ';' : '$1' .
'<export>' -> '<const_dcl>' ';' : '$1' .
'<export>' -> '<except_dcl>' ';' : '$1' .
'<export>' -> '<attr_dcl>' ';' : '$1' .
'<export>' -> '<op_dcl>' ';' : '$1' .
'<export>' -> 'OE_preproc'  : '$1' .
'<export>' -> 'OE_pragma'  : '$1' . 

%% Added clause
'Opt_<inheritance_spec>' -> '$empty' : [].
'Opt_<inheritance_spec>' -> '<inheritance_spec>' : '$1'.

%% (10)
'<inheritance_spec>' -> ':' '<scoped_name>' 'ZorM_<scoped_name>' 
  : ['$2' | reverse('$3')] .


%% Added clause
'ZorM_<scoped_name>' -> '$empty' : [] .
'ZorM_<scoped_name>' -> 'ZorM_<scoped_name>' ',' '<scoped_name>' 
  : ['$3' | '$1'] .


%% (11)
'<scoped_name>' -> '<identifier>' : ic_symtab:scoped_id_new('$1') .
'<scoped_name>' -> '::' '<identifier>' : ic_symtab:scoped_id_new_global('$2') .
'<scoped_name>' -> '<scoped_name>' '::' '<identifier>' 
  : ic_symtab:scoped_id_add('$1', '$3') .


%% (12)
'<const_dcl>' -> 'const' '<const_type>' '<identifier>' '=' '<const_exp>' 
  : #const{type='$2', id='$3', val='$5'} .


%% (13)
'<const_type>' -> '<integer_type>' : '$1' .
'<const_type>' -> '<char_type>' : '$1' .
'<const_type>' -> '<boolean_type>' : '$1' .
'<const_type>' -> '<floating_pt_type>' : '$1' .
'<const_type>' -> '<string_type>' : '$1' .
'<const_type>' -> '<fixed_pt_const_type>' : '$1' .
'<const_type>' -> '<scoped_name>' : '$1' .
'<const_type>' -> '<octet_type>' : '$1' .


%% (14)
'<const_exp>' -> '<or_expr>' : '$1' .


%% (15)
'<or_expr>' -> '<xor_expr>' : '$1' .
'<or_expr>' -> '<or_expr>' '|' '<xor_expr>' : {'or', '$1', '$3'} .


%% (16)
'<xor_expr>' -> '<and_expr>' : '$1' .
'<xor_expr>' -> '<xor_expr>' '^' '<and_expr>' : {'xor', '$1', '$3'} .


%% (17)
'<and_expr>' -> '<shift_expr>' : '$1' .
'<and_expr>' -> '<and_expr>' '&' '<shift_expr>' : {'and', '$1', '$3'} .


%% (18)
'<shift_expr>' -> '<add_expr>' : '$1' .
'<shift_expr>' -> '<shift_expr>' '>>' '<add_expr>' : {'rshift', '$1', '$3'} .
'<shift_expr>' -> '<shift_expr>' '<<' '<add_expr>' : {'lshift', '$1', '$3'} .


%% (19)
'<add_expr>' -> '<mult_expr>' : '$1' .
'<add_expr>' -> '<add_expr>' '+' '<mult_expr>' : {'+', '$1', '$3'} .
'<add_expr>' -> '<add_expr>' '-' '<mult_expr>' : {'-', '$1', '$3'} .


%% (20)
'<mult_expr>' -> '<unary_expr>' : '$1' .
'<mult_expr>' -> '<mult_expr>' '*' '<unary_expr>' : {'*', '$1', '$3'} .
'<mult_expr>' -> '<mult_expr>' '/' '<unary_expr>' : {'/', '$1', '$3'} .
'<mult_expr>' -> '<mult_expr>' '%' '<unary_expr>' : {'%', '$1', '$3'} .


%% (21)
'<unary_expr>' -> '<unary_operator>' '<primary_expr>' : {'$1', '$2'} .
'<unary_expr>' -> '<primary_expr>' : '$1' .


%% (22)
'<unary_operator>' -> '-' : '$1' .
'<unary_operator>' -> '+' : '$1' .
'<unary_operator>' -> '~' : '$1' .


%% (23)
'<primary_expr>' -> '<scoped_name>' : '$1' .
'<primary_expr>' -> '<literal>' : '$1' .
'<primary_expr>' -> '(' '<const_exp>' ')' : '$2' .


%% (24)
'<literal>' -> '<integer_literal>' : '$1' .
'<literal>' -> '<wstring_literal>' : '$1' .
'<literal>' -> '<string_literal>' : '$1' .
'<literal>' -> '<character_literal>' : '$1' .
'<literal>' -> '<wcharacter_literal>' : '$1' .
'<literal>' -> '<fixed_pt_literal>' : '$1' .
'<literal>' -> '<floating_pt_literal>' : '$1' .
'<literal>' -> '<boolean_literal>' : '$1' .


%% (25)
'<boolean_literal>' -> 'TRUE' : '$1' .
'<boolean_literal>' -> 'FALSE' : '$1' .


%% (26)
'<positive_int_const>' -> '<const_exp>' : '$1' .


%% (27)
'<type_dcl>' -> 'typedef' '<type_declarator>' : '$2' .
'<type_dcl>' -> '<struct_type>' : '$1' .
'<type_dcl>' -> '<union_type>' : '$1' .
'<type_dcl>' -> '<enum_type>' : '$1' .
'<type_dcl>' -> '<constr_forward_decl>' : '$1' .

%% (28) NIY multiple declarators (FIXED)
'<type_declarator>' -> '<type_spec>' '<declarators>'
  : #typedef{type='$1', id='$2'} . %%%ic:unfold(#typedef{type='$1', id='$2'}) .
%%'<type_declarator>' -> '<type_spec>' '<declarator>'
%%  : #typedef{type='$1', id='$2'} . 

%% (29)
'<type_spec>' -> '<simple_type_spec>' : '$1' .
'<type_spec>' -> '<constr_type_spec>' : '$1' .


%% (30)
'<simple_type_spec>' -> '<base_type_spec>' : '$1' .
'<simple_type_spec>' -> '<template_type_spec>' : '$1' .
'<simple_type_spec>' -> '<scoped_name>' : '$1' .


%% (31)
'<base_type_spec>' -> '<floating_pt_type>' : '$1' .
'<base_type_spec>' -> '<integer_type>' : '$1' .
'<base_type_spec>' -> '<char_type>' : '$1' .
'<base_type_spec>' -> '<boolean_type>' : '$1' .
'<base_type_spec>' -> '<octet_type>' : '$1' .
'<base_type_spec>' -> '<any_type>' : '$1' .
'<base_type_spec>' -> 'Object' : '$1' .  %% NON Standard, isn't a base type


%% (32)
'<template_type_spec>' -> '<sequence_type>' : '$1' .
'<template_type_spec>' -> '<string_type>' : '$1' .
'<template_type_spec>' -> '<fixed_pt_type>' : '$1' .


%% (33)
'<constr_type_spec>' -> '<struct_type>' : '$1' .
'<constr_type_spec>' -> '<union_type>' : '$1' .
'<constr_type_spec>' -> '<enum_type>' : '$1' .


%% (34)
'<declarators>' -> '<declarator>' 'ZorM_<declarator>'
: ['$1' | reverse('$2')] .

%% Added clause
'ZorM_<declarator>' -> '$empty' : [] .
'ZorM_<declarator>' -> 'ZorM_<declarator>' ',' '<declarator>'
: ['$3' | '$1'] .


%% (35)
'<declarator>' -> '<simple_declarator>' : '$1' .
'<declarator>' -> '<complex_declarator>' : '$1' .


%% (36)
'<simple_declarator>' -> '<identifier>' : '$1' .


%% (37)
'<complex_declarator>' -> '<array_declarator>' : '$1' .


%% (38)
'<floating_pt_type>' -> 'float' : '$1' .
'<floating_pt_type>' -> 'double' : '$1' .


%% (39)
'<integer_type>' -> '<signed_int>' : '$1' .
'<integer_type>' -> '<unsigned_int>' : {'unsigned', '$1'} .


%% (40)
'<signed_int>' -> '<signed_long_int>' : '$1' .
'<signed_int>' -> '<signed_short_int>' : '$1' .


%% (41)
'<signed_long_int>' -> 'long' : '$1' .
'<signed_long_int>' -> 'long' 'long': {'long long', element(2,'$2')} .


%% (42)
'<signed_short_int>' -> 'short' : '$1' .


%% (43)
'<unsigned_int>' -> '<unsigned_long_int>' : '$1' .
'<unsigned_int>' -> '<unsigned_short_int>' : '$1' .


%% (44)
'<unsigned_long_int>' -> 'unsigned' 'long' : '$2' .
'<unsigned_long_int>' -> 'unsigned' 'long' 'long' : {'long long', element(2,'$2')} . %% ULLONG


%% (45)
'<unsigned_short_int>' -> 'unsigned' 'short' : '$2' .


%% (46)
'<char_type>' -> 'char' : '$1' .
'<char_type>' -> 'wchar' : '$1' .    %% WCHAR


%% (47)
'<boolean_type>' -> 'boolean' : '$1' .


%% (48)
'<octet_type>' -> 'octet' : '$1' .


%% (49)
'<any_type>' -> 'any' : '$1' .

%%
'<fixed_pt_const_type>' -> 'fixed' : '$1'.

%% (50) NIY: unfolding of struct decls (FIXED)
%%'<struct_type>' -> 'struct' '<identifier>' '{' '<member_list>' '}'
%%  : #struct{id='$2', body=ic:unfold('$4')} .
'<struct_type>' -> 'struct' '<identifier>' '{' '<member_list>' '}'
  : #struct{id='$2', body='$4'} .


%% (51)
'<member_list>' -> 'OorM_<member>' : reverse('$1') .


%% Added clause
%%'OorM_<member>' -> '<member>' : ['$1'] .
%%'OorM_<member>' -> 'OorM_<member>' '<member>'
%%  : ['$2' | '$1'] .

'OorM_<member>' -> '<member>' : '$1' .
'OorM_<member>' -> 'OorM_<member>' '<member>'
  : '$2' ++ '$1' .



%% (52) NIY: member multiple declarators (FIXED)
%%'<member>' -> '<type_spec>' '<declarators>' ';'
%%  : #member{type='$1', id='$2'} .

'<member>' -> 'Ugly_pragmas' '<type_spec>' '<declarators>' 'Ugly_pragmas' ';' 'Ugly_pragmas'
  : '$1' ++ '$4' ++ '$6' ++ [#member{type='$2', id='$3'}] .


%% (53) NIY: unfolding of union cases (FIXED)
%%'<union_type>' -> 'union' '<identifier>' 'switch' 
%%    '(' '<switch_type_spec>' ')' '{' '<switch_body>' '}'
%%  : #union{id='$2', type='$5', body=ic:unfold('$8')} .
'<union_type>' -> 'union' '<identifier>' 'switch' 
    '(' '<switch_type_spec>' ')' '{' '<switch_body>' '}'
  : #union{id='$2', type='$5', body='$8'} .


%% (54)
'<switch_type_spec>' -> '<integer_type>' : '$1' .
'<switch_type_spec>' -> '<char_type>' : '$1' .
'<switch_type_spec>' -> '<boolean_type>' : '$1' .
'<switch_type_spec>' -> '<enum_type>' : '$1' .
'<switch_type_spec>' -> '<scoped_name>' : '$1' .


%% (55)
'<switch_body>' -> 'OorM_<case>' : reverse(lists:flatten('$1')) .

%%'<switch_body>' -> 'OorM_<case>' : '$1' .


%% Added clause
'OorM_<case>' -> '<case>' : ['$1'] .
'OorM_<case>' -> 'OorM_<case>' '<case>' : ['$2' | '$1'] .


%% (56) NIY thing: multiple case labels (FIXED)
%%'<case>' -> 'OorM_<case_label>' '<element_spec>' ';'
%%  : '$2'#case_dcl{label=reverse('$1')} .

'<case>' -> 
	'Ugly_pragmas' 'OorM_<case_label>' 
	'Ugly_pragmas' '<element_spec>' 
	'Ugly_pragmas' ';' 'Ugly_pragmas'
  : '$1' ++ '$3' ++ '$5' ++ '$7' ++ [ '$4'#case_dcl{label=reverse('$2')} ] .
	

%% Added clause
%%'OorM_<case_label>' -> '<case_label>' : ['$1'] .
%%'OorM_<case_label>' -> 'OorM_<case_label>' '<case_label>' : ['$2' | '$1'] .

'OorM_<case_label>' -> 'Ugly_pragmas' '<case_label>' 'Ugly_pragmas' 
	: '$1' ++ ['$2'] ++ '$3' .
'OorM_<case_label>' -> 'OorM_<case_label>' 'Ugly_pragmas' '<case_label>' 'Ugly_pragmas'
	: '$2' ++ ['$3'|'$1'] ++ '$4'.


%% (57)
'<case_label>' -> 'case' '<const_exp>' ':' : '$2' .
'<case_label>' -> 'default' ':' : '$1' .


%% (58)
'<element_spec>' -> '<type_spec>' '<declarator>'
: #case_dcl{type='$1', id='$2'} .


%% (59)
%%'<enum_type>' -> 'enum' '<identifier>' 
%%'{' '<enumerator>' 'ZorM_<enumerator>' '}'
%%: #enum{id='$2', body=['$4' | reverse('$5')]} .

'<enum_type>' -> 'enum' '<identifier>' 
'{' 'Ugly_pragmas' '<enumerator>' 'Ugly_pragmas' 'ZorM_<enumerator>' 'Ugly_pragmas' '}'
: #enum{id='$2', body='$4'++'$6'++'$8'++['$5' | reverse('$7')]} .



%% Added clause
%%'ZorM_<enumerator>' -> '$empty' : [] .
%%'ZorM_<enumerator>' -> 'ZorM_<enumerator>' ',' '<enumerator>' : ['$3' | '$1'] .

'ZorM_<enumerator>' -> '$empty' : [] .
'ZorM_<enumerator>' -> 'ZorM_<enumerator>' 'Ugly_pragmas' ',' 'Ugly_pragmas' '<enumerator>' 
	: '$2'++'$4'++['$5' | '$1'] .

%% (60)
'<enumerator>' -> '<identifier>' : #enumerator{id='$1'} .


%% (61)
'<sequence_type>' -> 'sequence' '<' '<simple_type_spec>' ',' 
	'<positive_int_const>' '>' 
  : #sequence{type='$3', length='$5'} .
'<sequence_type>' -> 'sequence' '<' '<simple_type_spec>' '>' 
  : #sequence{type='$3'} .


%% (62)
'<string_type>' -> 'string' '<' '<positive_int_const>' '>' 
  : #string{length='$3'} .
'<string_type>' -> 'string' : #string{} .

'<string_type>' -> 'wstring' '<' '<positive_int_const>' '>'   %% WSTRING 
  : #wstring{length='$3'} .
'<string_type>' -> 'wstring' : #wstring{} .                   %% WSTRING


%% (63)
'<array_declarator>' -> '<identifier>' 'OorM_<fixed_array_size>'
  : #array{id='$1', size=reverse('$2')} .


%% Added clause
'OorM_<fixed_array_size>' -> '<fixed_array_size>' : ['$1'] .
'OorM_<fixed_array_size>' -> 'OorM_<fixed_array_size>' '<fixed_array_size>' 
  : ['$2' | '$1'] .


%% (64)
'<fixed_array_size>' -> '[' '<positive_int_const>' ']' : '$2' .


%% (65) NIY: multiple attribute declarators (FIXED)
'<attr_dcl>' -> 'Opt_readonly' 'attribute' '<param_type_spec>' 
    '<simple_declarator>' 'ZorM_<simple_declarator>' 
  : #attr{readonly='$1', type='$3', id=['$4' | reverse('$5')]} .
%%  : ic:unfold(#attr{readonly='$1', type='$3', id=['$4' | reverse('$5')]}) .
%%'<attr_dcl>' -> 'Opt_readonly' 'attribute' '<param_type_spec>' 
%%    '<simple_declarator>'


%% (66) NIY: unfolding of exception bodies (FIXED)
%%'<except_dcl>' -> 'exception' '<identifier>' '{' 'ZorM_<member>' '}'
%%  : #except{id='$2', body=ic:unfold('$4')} .
'<except_dcl>' -> 'exception' '<identifier>' '{' 'ZorM_<member>' '}'
  : #except{id='$2', body=reverse('$4')} .

%% (67)
'<op_dcl>' -> 'Opt_<op_attribute>' '<op_type_spec>' '<identifier>' '<parameter_dcls>' 'Opt_<raises_expr>' 'Opt_<context_expr>'
  : #op{oneway='$1', type='$2', id='$3', params='$4', raises='$5', ctx='$6'} .

%% Added clause
'Opt_<op_attribute>' -> '$empty' : nil.
'Opt_<op_attribute>' -> '<op_attribute>' : '$1'.

%% (68)
'<op_attribute>' -> 'oneway' : '$1' .


%% (69)
'<op_type_spec>' -> '<param_type_spec>' : '$1' .
'<op_type_spec>' -> 'void' : '$1' .


%% (70) Rewritten
%'<parameter_dcls>' -> '(' '<param_dcl>' 'ZorM_<param_dcl>' ')'
%  : ['$2' | reverse('$3')] .
%'<parameter_dcls>' -> '(' ')' : [] .

'<parameter_dcls>' -> '(' 'Ugly_pragmas' '<param_dcl>' 'ZorM_<param_dcl>' ')'
  : '$2' ++ ['$3' | reverse('$4')] .
'<parameter_dcls>' -> '(' 'Ugly_pragmas' ')' : '$2' .


%% Added clause
%'ZorM_<param_dcl>' -> '$empty' : [] .
%'ZorM_<param_dcl>' -> 'ZorM_<param_dcl>' ',' '<param_dcl>' : ['$3' | '$1'] .


'ZorM_<param_dcl>' -> 'Ugly_pragmas' : '$1' .
'ZorM_<param_dcl>' -> 'ZorM_<param_dcl>' 'Ugly_pragmas' ',' 'Ugly_pragmas' '<param_dcl>' 'Ugly_pragmas' 
	: '$2' ++ '$4' ++ '$6' ++ ['$5' | '$1'] .




%% (71)
'<param_dcl>' -> '<param_attribute>' '<param_type_spec>' '<simple_declarator>'
  : #param{inout='$1', type='$2', id='$3'} .


%% (72)
'<param_attribute>' -> 'in' : '$1' .
'<param_attribute>' -> 'out' : '$1' .
'<param_attribute>' -> 'inout' : '$1' .


%% Added clause
'Opt_<raises_expr>' -> '$empty' : [] .
'Opt_<raises_expr>' -> '<raises_expr>' : '$1' .

%% (73)
'<raises_expr>' -> 'raises' '(' '<scoped_name>' 'ZorM_<scoped_name>' ')'
  : ['$3'| reverse('$4')] .


%% Added clause
'Opt_<context_expr>' -> '$empty' : [] .
'Opt_<context_expr>' -> '<context_expr>' : '$1'.

%% (74)
'<context_expr>' -> 'context' '(' '<string_literal>' 'ZorM_<string_literal>'')'
  : ['$3' | reverse('$4')] .



%% (75)
'<param_type_spec>' -> '<base_type_spec>' : '$1' .
'<param_type_spec>' -> '<string_type>' : '$1' .
'<param_type_spec>' -> '<scoped_name>' : '$1' .


%% (96)
'<fixed_pt_type>' -> 'fixed' '<' '<positive_int_const>' ',' '<positive_int_const>' '>'
  : #fixed{digits='$3',scale='$5'} .

%% (99)
'<constr_forward_decl>' -> 'struct' '<identifier>' : #constr_forward{id='$2', tk=tk_struct} .
'<constr_forward_decl>' -> 'union'  '<identifier>' : #constr_forward{id='$2', tk=tk_union} .

%% Added clause
'ZorM_<string_literal>' -> '$empty' : [] .
'ZorM_<string_literal>' -> 'ZorM_<string_literal>' ',' '<string_literal>' 
  : ['$3' | '$1'] .

%% Added clause
'ZorM_<simple_declarator>' -> '$empty' : [] .
'ZorM_<simple_declarator>' -> 'ZorM_<simple_declarator>' ',' 
'<simple_declarator>' : ['$3' | '$1'] .

%% Added clause
%%'ZorM_<member>' -> '$empty' : [] .
%%'ZorM_<member>' -> 'ZorM_<member>' '<member>' : ['$2' | '$1'] .

'ZorM_<member>' -> 'Ugly_pragmas' : '$1' .
'ZorM_<member>' -> 'ZorM_<member>' '<member>' : '$2' ++ '$1' .


%% Added clause
'Opt_readonly' -> '$empty' : nil.
'Opt_readonly' -> 'readonly' : '$1'.



Erlang code.
%%-----------------------------------------------------------



