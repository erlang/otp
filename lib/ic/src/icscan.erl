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
%%
-module(icscan).


-export([scan/2]).

-include("ic.hrl").


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

-import(lists, [reverse/1]).


scan(G, File) ->
    PL = call_preproc(G, File),
    call_scan(G, PL).

call_preproc(G, File) ->
    case ic_options:get_opt(G, use_preproc) of
	true -> 
	    icpreproc:preproc(G, File);
	false ->
	    case catch file:read_file(File) of
		{ok, Bin} ->
		    binary_to_list(Bin);
		Other ->
		    exit(Other)
	    end
    end.

call_scan(G, PL) ->
    BE = ic_options:get_opt(G, be),
    RSL = scan(G, BE, PL, 1, []),
    lists:reverse(RSL).


%% Guard macros used at top scan functions only
-define(is_number(X), X >= $0 , X =< $9).
-define(is_upper(X), X >= $A , X =< $Z).
-define(is_lower(X), X >= $a, X =< $z).
-define(is_hex_uc(X), X >= $A , X =< $F).
-define(is_hex_lc(X), X >= $a , X =< $f).
-define(is_octal(X), X >=$0, X =< $7).

%% Handle:
%%  const wchar aWChar = L'X';
scan(G, BE, [$L, $'|Str], Line, Out) ->
    scan_const(G, BE, wchar, Str, [], Line, Out);
scan(G, BE, [$L, $"|Str], Line, Out) ->
    scan_const(G, BE, wstring, Str, [], Line, Out);
scan(G, BE, [$_, X|Str], Line, Out) when ?is_upper(X) ->
    scan_name(G, BE, Str, [X], false, Line, Out);
scan(G, BE, [$_, X|Str], Line, Out) when ?is_lower(X) ->
    scan_name(G, BE, Str, [X], false, Line, Out);
scan(G, BE, [X|Str], Line, Out) when ?is_upper(X) ->
    scan_name(G, BE, Str, [X], true, Line, Out);
scan(G, BE, [X|Str], Line, Out) when ?is_lower(X) ->
    scan_name(G, BE, Str, [X], true, Line, Out);
scan(G, BE, [X|Str], Line, Out) when ?is_number(X) ->
    scan_number(G, BE, Str, [X], Line, Out);
scan(G, BE, [9| T], Line, Out) -> scan(G, BE, T, Line, Out);
scan(G, BE, [32| T], Line, Out) -> scan(G, BE, T, Line, Out);
scan(G, BE, [$\r|Str], Line, Out) ->
    scan(G, BE, Str, Line, Out);
scan(G, BE, [$\n|Str], Line, Out) ->
    scan(G, BE, Str, Line+1, Out);
scan(G, BE, [$:, $: | Str], Line, Out) ->
    scan(G, BE, Str, Line, [{'::', Line} | Out]);
scan(G, BE, [$/, $/ | Str], Line, Out) ->
    Rest = skip_to_nl(Str),
    scan(G, BE, Rest, Line, Out);
scan(G, BE, [$/, $* | Str], Line, Out) ->
    Rest = skip_comment(Str),
    scan(G, BE, Rest, Line, Out);
scan(G, BE, [$", $\\|Str], Line, Out) ->
    scan_const(G, BE, string, [$\\|Str], [], Line, Out);
scan(G, BE, [$"|Str], Line, Out) ->
    scan_const(G, BE, string, Str, [], Line, Out);
scan(G, BE, [$', $\\|Str], Line, Out) ->
    scan_const(G, BE, char, [$\\|Str], [], Line, Out);
scan(G, BE, [$'|Str], Line, Out) ->
    scan_const(G, BE, char, Str, [], Line, Out);
scan(G, BE, [$\\|Str], Line, Out) ->
    scan_const(G, BE, escaped, [$\\|Str], [], Line, Out);
scan(G, BE, [$. | Str], Line, Out) ->
    scan_frac(G, BE, Str, [$.], Line, Out);
scan(G, BE, [$# | Str], Line, Out) ->
    scan_preproc(G, BE, Str, Line, Out);
scan(G, BE, [$<, $< | Str], Line, Out) ->
    scan(G, BE, Str, Line, [{'<<', Line} | Out]);
scan(G, BE, [$>, $> | Str], Line, Out) ->
    scan(G, BE, Str, Line, [{'>>', Line} | Out]);
scan(G, BE, [C|Str], Line, Out) ->
    scan(G, BE, Str, Line, [{list_to_atom([C]), Line} | Out]);
	    
scan(_G, _BE, [], _Line, Out) ->
    Out.


scan_number(G, BE, [X|Str], [$0], Line, Out) when X == $X ; X ==$x ->
    case Str of
	[D|_TmpStr] when ?is_number(D); ?is_hex_uc(D); ?is_hex_lc(D) ->
	    {Num,Rest} = scan_hex_number(Str,0),
	    scan(G, BE, Rest, Line, [{'<integer_literal>', Line, 
				  integer_to_list(Num)} | Out]);
	[D|TmpStr] -> 
	    scan(G, BE, TmpStr, Line, [{list_to_atom([D]), Line} | Out])
    end;
scan_number(G, BE, Str, [$0], Line, Out) ->
    %% If an integer literal starts with a 0 it may indicate that
    %% it is represented as an octal number. But, it can also be a fixed
    %% type which must use padding to match a fixed typedef. For example:
    %% typedef fixed<5,2> fixed52;
    %% 123.45d, 123.00d and 023.00d is all valid fixed values.
    %% Naturally, a float can be defined as 0.14 or 00.14.
    case pre_scan_number(Str, [], octal) of
	octal ->
	    {Num, Rest} = scan_octal_number(Str,0),
	    scan(G, BE, Rest, Line, [{'<integer_literal>', Line, 
				  integer_to_list(Num)} | Out]);
	{fixed, Fixed, Rest} ->
	    scan(G, BE, Rest, Line, [{'<fixed_pt_literal>', Line, Fixed} | Out]);
	float ->
	    %% Not very likely that someone defines a constant as 00.14 but ... 
	    NewStr = remove_leading_zeroes(Str),
	    scan(G, BE, NewStr, Line, Out)
    end;
scan_number(G, BE, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_number(G, BE, Str, [X|Accum], Line, Out);
scan_number(G, BE, [X|Str], Accum, Line, Out) when X==$. ->
    scan_frac(G, BE, Str, [X|Accum], Line, Out);
scan_number(G, BE, [X|Str], Accum, Line, Out) when X==$e ; X==$e ->
    scan_exp(G, BE, Str, [X|Accum], Line, Out);
scan_number(G, BE, [X|Str], Accum, Line, Out) when X==$D ; X==$d ->
    scan(G, BE, Str, Line, [{'<fixed_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]);
scan_number(G, BE, Str, Accum, Line, Out) ->
    scan(G, BE, Str, Line, [{'<integer_literal>', Line,
			 (lists:reverse(Accum))} | Out]).


remove_leading_zeroes([$0|Rest]) ->
    remove_leading_zeroes(Rest);
remove_leading_zeroes(L) ->
    L.

scan_hex_number([X|Rest],Acc) when X >=$a, X =< $f ->
    scan_hex_number(Rest,(Acc bsl 4) + (X - $a + 10)); 
scan_hex_number([X|Rest],Acc) when X >=$A, X =< $F ->
    scan_hex_number(Rest,(Acc bsl 4) + (X - $A + 10)); 
scan_hex_number([X|Rest],Acc) when X >=$0, X =< $9 ->
    scan_hex_number(Rest,(Acc bsl 4) + (X-$0));
scan_hex_number(Rest,Acc) -> 
    {Acc,Rest}.

pre_scan_number([$d|Rest], Acc, _) ->
    {fixed, [$0|lists:reverse(Acc)], Rest};
pre_scan_number([$D|Rest], Acc, _) ->
    {fixed, [$0|lists:reverse(Acc)], Rest};
pre_scan_number([$.|Rest], Acc, _) ->
    %% Actually, we don't know if it's a float since it can be a fixed.
    pre_scan_number(Rest, [$.|Acc], float);
pre_scan_number([X|_], _Acc, _) when X == $E ; X ==$e  ->
    %% Now we now it's a float.
    float;
pre_scan_number([X|Rest], Acc, Type) when ?is_number(X) ->
    pre_scan_number(Rest, [X|Acc], Type);
pre_scan_number(_Rest, _Acc, Type) ->
    %% At this point we know it's a octal or float.
    Type.

scan_octal_number([X|Rest],Acc) when ?is_octal(X) ->
    scan_octal_number(Rest,(Acc bsl 3) + (X-$0)); 
scan_octal_number(Rest,Acc) -> 
    {Acc, Rest}.

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

scan_frac(G, _BE, [$e | _Str], [$.], Line, _Out) ->
    ic_error:fatal_error(G, {illegal_float, Line});
scan_frac(G, _BE, [$E | _Str], [$.], Line, _Out) ->
    ic_error:fatal_error(G, {illegal_float, Line});
scan_frac(G, BE, Str, Accum, Line, Out) ->
    scan_frac2(G, BE, Str, Accum, Line, Out).

scan_frac2(G, BE, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_frac2(G, BE, Str, [X|Accum], Line, Out);
scan_frac2(G, BE, [X|Str], Accum, Line, Out) when X==$e ; X==$E ->
    scan_exp(G, BE, Str, [X|Accum], Line, Out);
%% The following case is for fixed (e.g. 123.45d).
scan_frac2(G, BE, [X|Str], Accum, Line, Out) when X==$d ; X==$D ->
    scan(G, BE, Str, Line, [{'<fixed_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]);
scan_frac2(G, BE, Str, Accum, Line, Out) ->
    scan(G, BE, Str, Line, [{'<floating_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]).

scan_exp(G, BE, [X|Str], Accum, Line, Out) when X==$- ->
    scan_exp2(G, BE, Str, [X|Accum], Line, Out);
scan_exp(G, BE, Str, Accum, Line, Out) ->
    scan_exp2(G, BE, Str, Accum, Line, Out).

scan_exp2(G, BE, [X|Str], Accum, Line, Out) when ?is_number(X) ->
    scan_exp2(G, BE, Str, [X|Accum], Line, Out);
scan_exp2(G, BE, Str, Accum, Line, Out) ->
    scan(G, BE, Str, Line, [{'<floating_pt_literal>', Line,
			 (lists:reverse(Accum))} | Out]).


scan_name(G, BE, [X|Str], Accum, TypeCheck, Line, Out) when ?is_upper(X) ->
    scan_name(G, BE, Str, [X|Accum], TypeCheck, Line, Out);
scan_name(G, BE, [X|Str], Accum, TypeCheck, Line, Out) when ?is_lower(X) ->
    scan_name(G, BE, Str, [X|Accum], TypeCheck, Line, Out);
scan_name(G, BE, [X|Str], Accum, TypeCheck, Line, Out) when ?is_number(X) ->
    scan_name(G, BE, Str, [X|Accum], TypeCheck, Line, Out);
scan_name(G, BE, [$_|Str], Accum, TypeCheck, Line, Out) ->
    scan_name(G, BE, Str, [$_|Accum], TypeCheck, Line, Out);
scan_name(G, BE, S, Accum, false, Line, Out) ->
    %% The CORBA 2.3 specification allows the user to override typechecking:
    %% typedef string _native;
    %% interface i {
    %%     void foo(in _native VT);
    %% };
    %% BUT, the IFR-id remains the same ("IDL:native:1.0") etc. The reason for
    %% this is that one don't have to re-write a large chunk of IDL- and
    %% application-code.
    scan(G, BE, S, Line, [{'<identifier>', Line, lists:reverse(Accum)} | Out]);
scan_name(G, BE, S, Accum, _, Line, Out) ->
    L = lists:reverse(Accum),
    X = case is_reserved(L, BE) of
	    undefined -> 
		{'<identifier>', Line, L};
	    Yes -> 
		{Yes, Line}
	end,
    scan(G, BE, S, Line, [X | Out]).

%% Shall scan a constant
scan_const(G, BE, string, [$" | Rest], Accum, Line, [{'<string_literal>', _, Str}|Out]) ->
    scan(G, BE, Rest, Line, 
	 [{'<string_literal>', Line, Str ++ lists:reverse(Accum)} | Out]);
scan_const(G, BE, string, [$" | Rest], Accum, Line, Out) ->
    scan(G, BE, Rest, Line, 
	 [{'<string_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, BE, wstring, [$" | Rest], Accum, Line, [{'<wstring_literal>', _,Wstr}|Out]) -> %% WSTRING
    scan(G, BE, Rest, Line, 
	 [{'<wstring_literal>', Line, Wstr ++ lists:reverse(Accum)} | Out]);
scan_const(G, BE, wstring, [$" | Rest], Accum, Line, Out) -> %% WSTRING
    scan(G, BE, Rest, Line, 
	 [{'<wstring_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, _BE, string, [], _Accum, Line, Out) -> %% Bad string
    ic_error:error(G, {bad_string, Line}),
    Out;
scan_const(G, _BE, wstring, [], _Accum, Line, Out) -> %% Bad WSTRING
    ic_error:error(G, {bad_string, Line}),
    Out;
scan_const(G, BE, char, [$' | Rest], Accum, Line, Out) ->
    scan(G, BE, Rest, Line, 
	 [{'<character_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, BE, wchar, [$' | Rest], Accum, Line, Out) -> %% WCHAR
    scan(G, BE, Rest, Line, 
	 [{'<wcharacter_literal>', Line, lists:reverse(Accum)} | Out]);
scan_const(G, BE, Mode, [$\\, C | Rest], Accum, Line, Out) ->
    case escaped_char(C) of
	error ->
	    ic_error:error(G, {bad_escape_character, Line, C}), %% Bad escape character
	    scan_const(G, BE, Mode, Rest, [C | Accum], Line, Out);
	octal ->
	    {Num,Rest2} = scan_octal_number([C|Rest], 0),
	    scan_const(G, BE, Mode, Rest2, [Num|Accum], Line, Out); 
	hexadecimal ->
	    {Num,Rest2} = scan_hex_number(Rest, 0),
	    if
		Num > 255 -> %% 16#FF
		    ic_error:error(G, {bad_escape_character, Line, C}),
		    scan_const(G, BE, Mode, Rest, [C | Accum], Line, Out);
		true ->
		    scan_const(G, BE, Mode, Rest2, [Num|Accum], Line, Out)
	    end;
	unicode ->
	    {Num,Rest2} = scan_hex_number(Rest, 0),
	    if
		Num > 65535 -> %% 16#FFFF
		    ic_error:error(G, {bad_escape_character, Line, C}),
		    scan_const(G, BE, Mode, Rest, [C | Accum], Line, Out);
		true ->
		    scan_const(G, BE, Mode, Rest2, [Num|Accum], Line, Out)
	    end;
	EC ->
	    scan_const(G, BE, Mode, Rest, [EC | Accum], Line, Out)
    end;
scan_const(G, BE, Mode, [C | Rest], Accum, Line, Out) ->
    scan_const(G, BE, Mode, Rest, [C | Accum], Line, Out).


%%
%% Preprocessor output handling
%%
%%	gcc outputs a line with line number, file name (within \") and
%%	one or more integer flags. The scanner scans the line number,
%%	the id and all integers up to nl.
%%
%% NOTE: This will have to be enhanced in order to eat #pragma
%%
scan_preproc(G, BE, Str, Line, Out) ->
    {List, Rest} = scan_to_nl(strip(Str), []),
    NewLine = get_new_line_nr(strip(List), Line+1, []),
    case scan_number(G, BE, List, [], Line, [{'#', Line} | Out]) of
	L when is_list(L) ->
	    scan(G, BE, Rest, NewLine, [{'#', Line} | L])
    end.

get_new_line_nr([C|R], Line, Acc) when C>=$0, C=<$9 ->
    get_new_line_nr(R, Line, [C|Acc]);
get_new_line_nr(_, Line, []) -> Line;		% No line nr found
get_new_line_nr(_, _, Acc) -> list_to_integer(reverse(Acc)).

scan_to_nl([], Acc) -> {reverse(Acc), []};
scan_to_nl([$\n|Str], Acc) -> {reverse(Acc), Str};
scan_to_nl([$\r|R], Acc) -> scan_to_nl(R, Acc);
scan_to_nl([C|R], Acc) -> scan_to_nl(R, [C|Acc]).

strip([$ |R]) -> strip(R);
strip(L) -> L.

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
escaped_char($x) -> hexadecimal;
escaped_char($u) -> unicode;
escaped_char(X) when ?is_octal(X) -> octal;
%% Error
escaped_char(_Other) -> error.

skip_to_nl([]) -> [];
skip_to_nl([$\n | Str]) ->[$\n | Str];
skip_to_nl([_|Str]) ->
    skip_to_nl(Str).

skip_comment([$\\, _ | Str]) ->
    skip_comment(Str);
skip_comment([$*, $/ | Str]) -> Str;
skip_comment([_|Str]) -> 
    skip_comment(Str).


%%----------------------------------------------------------------------
%% Shall separate keywords from identifiers and numbers

%% Fill in the ets of reserved words
is_reserved("Object", _) ->     'Object';
is_reserved("in", _) ->          in;
is_reserved("interface", _) ->   interface;
is_reserved("case", _) ->       'case';
is_reserved("union", _) ->       union;
is_reserved("struct", _) ->      struct;
is_reserved("any", _) ->         any;
is_reserved("long", _) ->        long;
is_reserved("float", _) ->       float;
is_reserved("out", _) ->         out;
is_reserved("enum", _) ->        enum;
is_reserved("double", _) ->      double;
is_reserved("context", _) ->     context;
is_reserved("oneway", _) ->      oneway;
is_reserved("sequence", _) ->    sequence;
is_reserved("FALSE", _) ->      'FALSE';
is_reserved("readonly", _) ->    readonly;
is_reserved("char", _) ->        char;
is_reserved("wchar", _) ->       wchar;
is_reserved("void", _) ->        void;
is_reserved("inout", _) ->       inout;
is_reserved("attribute", _) ->   attribute;
is_reserved("octet", _) ->       octet;
is_reserved("TRUE", _) ->       'TRUE';
is_reserved("switch", _) ->      switch;
is_reserved("unsigned", _) ->    unsigned;
is_reserved("typedef", _) ->     typedef;
is_reserved("const", _) ->       const;
is_reserved("raises", _) ->      raises;
is_reserved("string", _) ->      string;
is_reserved("wstring", _) ->     wstring;
is_reserved("default", _) ->     default;
is_reserved("short", _) ->       short;
is_reserved("module", _) ->      module;
is_reserved("exception", _) ->   exception;
is_reserved("boolean", _) ->     boolean;
%% --- New keywords Introduced in CORBA-2.3.1 ---
%% For now we cannot add these for all backends right now since it would cause
%% some problems for at least one customer.
is_reserved("fixed", BE) ->       check_be(BE, fixed);
%is_reserved("abstract", BE) ->    check_be(BE, abstract);
%is_reserved("custom", BE) ->      check_be(BE, custom);
%is_reserved("factory", BE) ->     check_be(BE, factory);
%is_reserved("local", BE) ->       check_be(BE, local);
%is_reserved("native", BE) ->      check_be(BE, native);
%is_reserved("private", BE) ->     check_be(BE, private);
%is_reserved("public", BE) ->      check_be(BE, public);
%is_reserved("supports", BE) ->    check_be(BE, supports);
%is_reserved("truncatable", BE) -> check_be(BE, truncatable);
%is_reserved("ValueBase", BE) ->   check_be(BE, 'ValueBase');
%is_reserved("valuetype", BE) ->   check_be(BE, valuetype);
is_reserved(_, _) -> undefined.

check_be(erl_corba, KeyWord) ->
    KeyWord;
check_be(_, _) ->
    undefined.

