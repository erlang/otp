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

%% Description  : Token scanner for XPATH grammar

%%%----------------------------------------------------------------------
%%%
%%% The XPATH grammar is a bit tricky, due to operator overloading.
%%% This version of the scanner is based on the XPATH spec:
%%% http://www.w3.org/TR/1999/REC-xpath-19991116 (XPATH version 1.0)
%%%
%%% Quote from the spec:
%%%
%%%  "The following special tokenization rules must be applied in the order
%%%  specified to disambiguate the ExprToken grammar:
%%%
%%%  o If there is a preceding token and the preceding token is not one of
%%%    @, ::. (, [, or an Operator, then a * must be recognized as a 
%%%    MultiplyOperator and an NCName must be recognized as an OperatorName
%%%  o If the character following an NCName (possible after intervening
%%%    ExprWhiteSpace) is (, then the token must be recognized as a NodeType
%%%    or a FunctionName.
%%%  o If the two characters following an NCName (possible after intervening
%%%    ExprWhiteSpace) are ::, then the token must be recognized as an 
%%%    AxisName.
%%%  o Otherwise, the token must not be recognized as a MultiplyOperator, an
%%%    OperatorName, a NodeType, a FunctionName, or an AxisName."
%%%----------------------------------------------------------------------

-module(xmerl_xpath_scan).


%% main API
-export([tokens/1]).

%% exported helper functions
-export([scan_number/1]).

-include("xmerl.hrl").

-define(L, 1).


tokens(Str) ->
    tokens(strip_ws(Str), []).

tokens([], Acc) ->
    lists:reverse([{'$end', ?L, '$end'}|Acc]);
tokens(Str, Acc) ->
    case scan_token(Str, Acc) of
	{rescan, NewStr} ->
	    tokens(NewStr, Acc);
	{Token, T} ->
	    tokens(strip_ws(T), [Token|Acc])
    end.

%% Expr Tokens
scan_token("(" ++ T, _A) ->  {{'(', ?L, '('}, T};
scan_token(")" ++ T, _A) ->  {{')', ?L, ')'}, T};
scan_token("[" ++ T, _A) ->  {{'[', ?L, '['}, T};
scan_token("]" ++ T, _A) ->  {{']', ?L, ']'}, T};
scan_token(".." ++ T, _A) -> {rescan,"parent::node()" ++ T} ;
						% {{'..',?L,'..'}, T};
scan_token("@" ++ T, _A) ->  {rescan,"attribute::" ++ T};
						% {{'@',?L,'@'},T};
scan_token("," ++ T, _A) ->  {{',', ?L, ','}, T};
scan_token("::" ++ T, _A) -> {{'::', ?L, '::'}, T};

%% operators
scan_token("//" ++ T, _A) -> {rescan,"/descendant-or-self::node()/" ++ T};
						% {{'//',?L,'//'},T};
scan_token("/" ++ T, _A) ->  {{'/', ?L, '/'}, T};
scan_token("|" ++ T, _A) ->  {{'|', ?L, '|'}, T};
scan_token("+" ++ T, _A) ->  {{'+', ?L, '+'}, T};
scan_token("-" ++ T, _A) ->  {{'-', ?L, '-'}, T};
scan_token("=" ++ T, _A) ->  {{'=', ?L, '='}, T};
scan_token("!=" ++ T, _A) -> {{'!=', ?L, '!='}, T};
scan_token("<=" ++ T, _A) -> {{'<=', ?L, '<='}, T};
scan_token("<" ++ T, _A) ->  {{'<', ?L, '<'}, T};
scan_token(">=" ++ T, _A) -> {{'>=', ?L, '>='}, T};
scan_token(">" ++ T, _A) ->  {{'>', ?L, '>'}, T};

scan_token("*" ++ T, A) ->
    Tok = 
	case A of
	    [{X,_,_}|_] ->
		case special_token(X) of
		    false ->
			{'*', ?L, '*'};
		    true ->
			{'wildcard', ?L, 'wildcard'}
		end;
	    _ ->
		{'wildcard', ?L, 'wildcard'}
	end,
    {Tok, T};

%% numbers
scan_token(Str = [H|_], _A) when H >= $0, H =< $9 ->
    scan_number(Str);
scan_token(Str = [$., H|_], A) when H >= $0, H =< $9 ->
    scan_number(Str, A);
scan_token("." ++ T, _A) ->
%    {{'.', ?L, '.'}, T};
    {rescan, "self::node()" ++ T};

%% Variable Reference
scan_token([$$|T], _A) ->
    {{Prefix, Local}, T1} = scan_name(T),
    case Prefix of
	[] ->
	    {{var_reference, ?L, list_to_atom(Local)}, T1};
	_ ->
	    {{var_reference, ?L, list_to_atom(Prefix++":"++Local)}, T1}
    end;

scan_token([H|T], _A) when H == $" ; H == $' ->
    {Literal, T1} = scan_literal(T, H, []),
    {{literal, ?L, Literal}, T1};

scan_token(T, A) ->
    {{Prefix, Local}, T1} = scan_name(T),
    case A of
	[{X,_,_}|_] ->
	    case special_token(X) of
		false ->
		    operator_name(Prefix, Local, T1);
		true ->
		    other_name(Prefix, Local, strip_ws(T1))
	    end;
	_ ->
	    other_name(Prefix, Local, T1)
    end.

operator_name([], "and", T) ->	{{'and', ?L, 'and'}, T};
operator_name([], "or", T) ->	{{'or', ?L, 'or'}, T};
operator_name([], "mod", T) ->	{{'mod', ?L, 'mod'}, T};
operator_name([], "div", T) ->	{{'div', ?L, 'div'}, T}.


other_name(Prefix, [], "*" ++ T) ->
    %% [37] NameTest ::= '*' | NCName ':' '*' | QName
    {{prefix_test, ?L, Prefix}, T};
other_name(Prefix, Local, T = "(" ++ _) ->
    node_type_or_function_name(Prefix, Local, T);
other_name(Prefix, Local, T = "::" ++ _) ->
    axis(Prefix, Local, T);
other_name([], Local, T) ->
    {{name, ?L, {list_to_atom(Local),              [], Local}}, T};
other_name(Prefix, Local, T) ->
    {{name, ?L, {list_to_atom(Prefix++":"++Local), Prefix, Local}}, T}.



%% node types
node_type_or_function_name([], "comment", T) ->
    {{node_type, ?L, comment}, T};
node_type_or_function_name([], "text", T) ->
    {{node_type, ?L, text}, T};
node_type_or_function_name([], "processing-instruction", T) ->
    {{'processing-instruction', ?L, 'processing-instruction'}, T};
node_type_or_function_name([], "node", T) ->
    {{node_type, ?L, node}, T};
node_type_or_function_name(Prefix, Local, T) ->
    {{function_name, ?L, list_to_atom(Prefix ++ Local)}, T}.


%% axis names
axis([], "ancestor-or-self", T) ->	{{axis, ?L, ancestor_or_self}, T};
axis([], "ancestor", T) ->		{{axis, ?L, ancestor}, T};
axis([], "attribute", T) ->		{{axis, ?L, attribute}, T};
axis([], "child", T) ->			{{axis, ?L, child}, T};
axis([], "descendant-or-self", T) ->	{{axis, ?L, descendant_or_self}, T};
axis([], "descendant", T) ->		{{axis, ?L, descendant}, T};
axis([], "following-sibling", T) ->	{{axis, ?L, following_sibling}, T};
axis([], "following", T) ->		{{axis, ?L, following}, T};
axis([], "namespace", T) ->		{{axis, ?L, namespace}, T};
axis([], "parent", T) ->		{{axis, ?L, parent}, T};
axis([], "preceding-sibling", T) ->	{{axis, ?L, preceding_sibling}, T};
axis([], "preceding", T) ->		{{axis, ?L, preceding}, T};
axis([], "self", T) ->			{{axis, ?L, self}, T}.




scan_literal([H|T], H, Acc) ->
    {lists:reverse(Acc), T};
scan_literal([H|T], Delim, Acc) ->
    scan_literal(T, Delim, [H|Acc]).


scan_name([H1, H2 | T]) when H1 == $: ; H1 == $_ ->
    if ?whitespace(H2) ->
	    exit({invalid_name, [H1, H2, '...']});
       true ->
	    scan_prefix(T, [H2, H1])
    end;
scan_name([H|T]) ->
    case xmerl_lib:is_letter(H) of
	true ->
	    scan_prefix(T, [H]);
	false ->
	    exit({invalid_name, lists:sublist([H|T], 1, 6)})
    end;
scan_name(Str) ->
    exit({invalid_name, lists:sublist(Str, 1, 6)}).

scan_prefix([], Acc) ->
    {{[], lists:reverse(Acc)}, []};
scan_prefix(Str = [H|_], Acc) when ?whitespace(H) ->
    {{[], lists:reverse(Acc)}, Str};
scan_prefix(T = "::" ++ _, Acc) ->
    %% This is the next token
    {{[], lists:reverse(Acc)}, T};
scan_prefix(":" ++ T, Acc) ->
    {LocalPart, T1} = scan_local_part(T, []),
    Prefix = lists:reverse(Acc),
    {{Prefix, LocalPart}, T1};
scan_prefix(Str = [H|T], Acc) ->
    case xmerl_lib:is_namechar(H) of
	true ->
	    scan_prefix(T, [H|Acc]);
	false ->
	    {{[], lists:reverse(Acc)}, Str}
    end.

scan_local_part([], Acc) ->
    {lists:reverse(Acc), []};
scan_local_part(Str = [H|_], Acc) when ?whitespace(H) ->
    {lists:reverse(Acc), Str};
scan_local_part(Str = [H|T], Acc) ->
    case xmerl_lib:is_namechar(H) of
	true ->
	    scan_local_part(T, [H|Acc]);
	false ->
	    {lists:reverse(Acc), Str}
    end.


scan_number(T) ->
    scan_number(T, []).

scan_number([], Acc) ->
    {{number, ?L, list_to_integer(lists:reverse(Acc))}, []};
scan_number("." ++ T, []) ->
    {Digits, T1} = scan_digits(T, ".0"),
    Number = list_to_float(Digits),
    {{number, ?L, Number}, T1};
scan_number("." ++ T, Acc) ->
    {Digits, T1} = scan_digits(T, "." ++ Acc),
    Number = list_to_float(Digits),
    {{number, ?L, Number}, T1};
scan_number([H|T], Acc) when H >= $0, H =< $9 ->
    scan_number(T, [H|Acc]);
scan_number(T, Acc) ->
    {{number, ?L, list_to_integer(lists:reverse(Acc))}, T}.

scan_digits([], Acc) ->
    {lists:reverse(Acc), []};
scan_digits([H|T], Acc) when H >= $0, H =< $9 ->
    scan_digits(T, [H|Acc]);
scan_digits(T, Acc) ->
    {lists:reverse(Acc), T}.


strip_ws([H|T]) when ?whitespace(H) ->
    strip_ws(T);
strip_ws(T) ->
    T.


%% special_token('@') -> true;
special_token('::') -> true;
special_token(',') -> true;
special_token('(') -> true;
special_token('[') -> true;
special_token('/') -> true;
%% special_token('//') -> true;
special_token('|') -> true;
special_token('+') -> true;
special_token('-') -> true;
special_token('=') -> true;
special_token('!=') -> true;
special_token('<') -> true;
special_token('<=') -> true;
special_token('>') -> true;
special_token('>=') -> true;
special_token('and') -> true;
special_token('or') -> true;
special_token('mod') -> true;
special_token('div') -> true;
special_token(_) -> false.
