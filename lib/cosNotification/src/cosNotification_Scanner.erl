%%----------------------------------------------------------------------
%%
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
%%
%%----------------------------------------------------------------------
%% File    : cosNotification_Scanner.erl
%% Purpose : Scan and pre-process a grammar.
%%----------------------------------------------------------------------

-module('cosNotification_Scanner').

-export([scan/1]).

scan(Str) ->
    RSL = scan(Str, 1, [], any),
    {ok, lists:reverse(RSL)}.


%% Guard macros used at top scan functions only
-define(is_number(X), X >= $0, X =< $9).
-define(is_upper(X), X >= $A, X =< $Z).
-define(is_lower(X), X >= $a, X =< $z).

%%----------------------------------------------------------------------
%% scan
%%
%% A-Z
scan([X|Str], Line, Out, Type) when ?is_upper(X) ->
    scan_name(Str, [X], Line, Out, Type);
%% a-z
scan([X|Str], Line, Out, Type) when ?is_lower(X) ->
    scan_name(Str, [X], Line, Out, Type);
%% 0-9
scan([X|Str], Line, Out, Type) when ?is_number(X) ->
    scan_number(Str, [X], Line, Out, Type);

%% RELOP:s == != <= >= > <
scan([$=,$= | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '=='} | Out], any);
scan([$!,$= | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '!='} | Out], any);
scan([$<,$= | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '<='} | Out], any);
scan([$>,$= | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '>='} | Out], any);
scan([$> | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '>'} | Out], any);
scan([$< | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'RELOP', '<'} | Out], any);

%% ADDOP:s + -
scan([$+ | Str], Line, Out, Type) ->
    scan(Str, Line, [{'ADDOP', '+'} | Out], Type);
scan([$- | Str], Line, Out, Type) ->
    scan(Str, Line, [{'ADDOP', '-'} | Out], Type);

%% MULOP:s * /
scan([$* | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'MULOP', '*'} | Out], any);
scan([$/ | Str], Line, Out, _Type) ->
    scan(Str, Line, [{'MULOP', '/'} | Out], any);

%% TAB
scan([9| T], Line, Out, Type) -> scan(T, Line, Out, Type);
%% SP
scan([32| T], Line, Out, Type) -> scan(T, Line, Out, Type);
%% CR
scan([$\r|Str], Line, Out, Type) ->
    scan(Str, Line, Out, Type);
%% LF
scan([$\n|Str], Line, Out, Type) ->
    scan(Str, Line+1, Out, Type);
%% \\
scan([92, 92 | Str], Line, Out, Type) ->
    scan(Str, Line, [{'dbslsh', Line} | Out], Type);
%% \'
scan([92, 39 | Str], Line, Out, Type) ->
    scan(Str, Line, [{'bslshd', Line} | Out], Type);
%% '\'
scan([92 | Str], Line, Out, Type) ->
    scan(Str, Line, [{'bslsh', Line} | Out], Type);
%% '_'
scan([$_ | Str], Line, Out, dollar) -> 
    scan_name(Str, [$_], Line, Out, dollar);
%% '$'
scan([$$, 92 | Str], Line, Out, _Type) -> 
    scan(Str, Line, [{'bslsh', Line}, {'dollar', Line} | Out], dollar);
scan([$$ | Str], Line, Out, _Type) -> 
    scan(Str, Line, [{'dollar', Line} | Out], dollar);
scan([$"|Str], Line, Out, Type) ->
    scan_const(char, Str, [], Line, Out, Type);
scan([$'|Str], Line, Out, Type) ->
    scan_const(string, Str, [], Line, Out, Type);

%% Writing '+.<CompDot>' is not allowed ('+' or '-' are only allowed
%% as unary for <UnionVal> (within a component) which must be en integer).
scan([$. | Str], Line, [{'ADDOP', Op}|Out], _) ->
    scan_frac(Str, [$.], Line, [{'ADDOP', Op}|Out], any);
%% Must be a <CompDot>
scan([$. | Str], Line, Out, dollar) ->
    scan(Str, Line, [{'.',Line} | Out], dollar);
%% Number
scan([$. | Str], Line, Out, Type) ->
    scan_frac(Str, [$.], Line, Out, Type);
scan([C|Str], Line, Out, Type) ->
    scan(Str, Line, [{list_to_atom([C]), Line} | Out], Type);
	    
scan([], _Line, Out, _Type) ->
    Out.

%%----------------------------------------------------------------------
%% scan_name
%%

scan_number([X|Str], Accum, Line, Out, Type) when ?is_number(X) ->
    scan_number(Str, [X|Accum], Line, Out, Type);
scan_number([X|Str], Accum, Line, Out, dollar) when X==$. ->
    scan(Str, Line, [{'.', Line}, 
		     {'int', list_to_integer(lists:reverse(Accum))} | Out], dollar);
scan_number([X|Str], Accum, Line, Out, Type) when X==$. ->
    scan_frac(Str, [X|Accum], Line, Out, Type);
scan_number([X|Str], Accum, Line, Out, Type) when X==$e ->
    scan_exp(Str, [X|Accum], Line, Out, Type);
scan_number([X|Str], Accum, Line, Out, Type) when X==$E ->
    scan_exp(Str, [X|Accum], Line, Out, Type);
scan_number(Str, Accum, Line, Out, Type) ->
    scan(Str, Line, [{'int', list_to_integer(lists:reverse(Accum))} | Out], Type).


%% Floating point number scan.
%%
%%	Non trivial scan. A float consists of an integral part, a
%%	decimal point, a fraction part, an e or E and a signed integer
%%	exponent. Either the integer part or the fraction part but not
%%	both may be missing, and either the decimal point or the
%%	exponent part but not both may be missing. The exponent part
%%	must consist of an e or E and a possibly signed exponent.
%%
%%	Analysis shows that "1." ".7" "1e2" ".5e-3" "1.7e2" "1.7e-2"
%%	is allowed and "1" ".e9" is not. The sign is only allowed just
%%	after an e or E. The scanner reads a number as an integer
%%	until it encounters a "." so the integer part only error case
%%	will not be caught in the scanner (but rather in expression
%%	evaluation)

scan_frac([$e | _Str], [$.], _Line, _Out, _Type) ->
    {error, "illegal_float"};
scan_frac([$E | _Str], [$.], _Line, _Out, _Type) ->
    {error, "illegal_float"};
scan_frac(Str, Accum, Line, Out, Type) ->
    scan_frac2(Str, Accum, Line, Out, Type).

scan_frac2([X|Str], Accum, Line, Out, Type) when ?is_number(X) ->
    scan_frac2(Str, [X|Accum], Line, Out, Type);
scan_frac2([X|Str], Accum, Line, Out, Type) when X==$e ->
    scan_exp(Str, [X|Accum], Line, Out, Type);
scan_frac2([X|Str], Accum, Line, Out, Type) when X==$E ->
    scan_exp(Str, [X|Accum], Line, Out, Type);
%% Since '.2' is allowed, we add '0' in front to be sure (erlang do not allow
%% list_to_float(".2") and list_to_float("0.2") eq. list_to_float("00.2")).
scan_frac2(Str, Accum, Line, Out, Type) ->
    scan(Str, Line, [{'num', list_to_float([$0|lists:reverse(Accum)])} | Out], Type).

scan_exp([X|Str], Accum, Line, Out, Type) when X==$- ->
    scan_exp2(Str, [X|Accum], Line, Out, Type);
scan_exp(Str, Accum, Line, Out, Type) ->
    scan_exp2(Str, Accum, Line, Out, Type).

scan_exp2([X|Str], Accum, Line, Out, Type) when ?is_number(X) ->
    scan_exp2(Str, [X|Accum], Line, Out, Type);
%% Since '.2' is allowed, we add '0' in front to be sure (erlang do not allow
%% list_to_float(".2")).
scan_exp2(Str, Accum, Line, Out, Type) ->
    scan(Str, Line, [{'num', list_to_float([$0|lists:reverse(Accum)])} | Out], Type).


scan_name([X|Str], Accum, Line, Out, Type) when ?is_upper(X) ->
    scan_name(Str, [X|Accum], Line, Out, Type);
scan_name([X|Str], Accum, Line, Out, Type) when ?is_lower(X) ->
    scan_name(Str, [X|Accum], Line, Out, Type);
scan_name([X|Str], Accum, Line, Out, Type) when ?is_number(X) ->
    scan_name(Str, [X|Accum], Line, Out, Type);
scan_name([$_|Str], Accum, Line, Out, dollar) ->
    scan_name(Str, [$_|Accum], Line, Out, dollar);
scan_name(S, Accum, Line, [{bslsh,LL} | Out], Type) ->
    %% An escaped identifier.
    L = lists:reverse(Accum),
    scan(S, Line, [{'ident', L}, {bslsh,LL} | Out], Type);
scan_name(S, Accum, Line, Out, Type) ->
    L = lists:reverse(Accum),
    {X, NewType} = case check_name(L) of
		       false -> 
			   {{'ident', L}, Type};
		       _ -> 
			   {{list_to_atom(L), Line}, any}
		   end,
    scan(S, Line, [X | Out], NewType).

%% Shall scan a constant
scan_const(char, [$" | Rest], Accum, Line, Out, Type) ->
    scan(Rest, Line, 
	 [{'ident', list_to_atom(lists:reverse(Accum))} | Out], Type);
scan_const(char, [], _Accum, _Line, Out, _Type) -> %% Bad string
%    {error, "bad_string"};
    Out;
scan_const(string, [$' | Rest], Accum, Line, Out, Type) ->
    scan(Rest, Line, 
	 [{'string', lists:reverse(Accum)} | Out], Type);
scan_const(Mode, [$\\, C | Rest], Accum, Line, Out, Type) ->
    case escaped_char(C) of
	error ->
	    %% Bad escape character
	    %% {error, "bad_escape_character"};
	    scan_const(Mode, Rest, [C | Accum], Line, Out, Type);
	EC ->
	    scan_const(Mode, Rest, [EC | Accum], Line, Out, Type)
    end;
scan_const(Mode, [C | Rest], Accum, Line, Out, Type) ->
    scan_const(Mode, Rest, [C | Accum], Line, Out, Type).

%% Escaped character. Escaped chars are repr as two characters in the
%% input list of letters and this is translated into one char.
escaped_char($n) -> $\n;
escaped_char($t) -> $\t;
escaped_char($v) -> $\v;
escaped_char($b) -> $\b;
escaped_char($r) -> $ ;
escaped_char($f) -> $\f;
escaped_char($a) -> $\a;
escaped_char($\\) -> $\\;
escaped_char($?) -> $?;
escaped_char($') -> $';
escaped_char($") -> $";
%% Error
escaped_char(_Other) -> error.


check_name("exist") ->     true;
check_name("default") ->   true;
check_name("_length") ->   true;
check_name("_d") ->        true;
check_name("_type_id") ->  true;
check_name("_repos_id") -> true;
check_name("not") ->       true;
check_name("or") ->        true;
check_name("and") ->       true;
check_name("FALSE") ->     true;
check_name("TRUE") ->      true;
check_name("in") ->        true;
check_name(_) ->           false.
    

