%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: core_scan.erl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
%% Purpose: Scanner for Core Erlang.

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase
%%
%% Many punctuation characters region have special meaning.  Must
%% watch using × \327, bvery close to x \170

-module(core_scan).

-export([string/1,string/2,tokens/3,format_error/1]).

-import(lists, [reverse/1]).

%% tokens(Continuation, CharList, StartPos) ->
%%	{done, {ok, [Tok], EndPos}, Rest} |
%%	{done, {error,{ErrorPos,core_scan,What}, EndPos}, Rest} |
%%	{more, Continuation'}
%%  This is the main function into the re-entrant scanner. It calls the
%%  re-entrant pre-scanner until this says done, then calls scan/1 on
%%  the result.
%%
%%  The continuation has the form:
%%      {RestChars,CharsSoFar,CurrentPos,StartPos}

tokens([], Chars, Pos) ->			%First call
    tokens({[],[],Pos,Pos}, Chars, Pos);
tokens({Chars,SoFar0,Cp,Sp}, MoreChars, _) ->
    In = Chars ++ MoreChars,
    case pre_scan(In, SoFar0, Cp) of
	{done,_,[],Ep} ->			%Found nothing
	    {done,{eof,Ep},[]};
	{done,_,SoFar1,Ep} ->			%Got complete tokens
	    Res = case scan(reverse(SoFar1), Sp) of
		      {ok,Toks} -> {ok,Toks,Ep};
		      {error,E} -> {error,E,Ep}
		  end,
	    {done,Res,[]};
	{more,Rest,SoFar1,Cp1} ->		%Missing end token
	    {more,{Rest,SoFar1,Cp1,Sp}};
	Other ->				%An error has occurred
	    {done,Other,[]}
    end.

%% string([Char]) ->
%% string([Char], StartPos) ->
%%    {ok, [Tok], EndPos} |
%%    {error,{Pos,core_scan,What}, EndPos}

string(Cs) -> string(Cs, 1).

string(Cs, Sp) ->
    %% Add an 'eof' to always get correct handling.
    case string_pre_scan(Cs, [], Sp) of
	{done,_,SoFar,Ep} ->			%Got tokens
	    case scan(reverse(SoFar), Sp) of
		{ok,Toks} -> {ok,Toks,Ep};
		{error,E} -> {error,E,Ep}
	    end;
	Other -> Other				%An error has occurred
    end.

%% string_pre_scan(Cs, SoFar0, StartPos) ->
%%      {done,Rest,SoFar,EndPos} | {error,E,EndPos}.

string_pre_scan(Cs, SoFar0, Sp) ->
    case pre_scan(Cs, SoFar0, Sp) of
	{done,Rest,SoFar1,Ep} ->		%Got complete tokens
	    {done,Rest,SoFar1,Ep};
	{more,Rest,SoFar1,Ep} ->		%Missing end token
	    string_pre_scan(Rest ++ eof, SoFar1, Ep);
	Other -> Other				%An error has occurred
    end.

%% format_error(Error)
%%  Return a string describing the error.

format_error({string,Quote,Head}) ->
    ["unterminated " ++ string_thing(Quote) ++
     " starting with " ++ io_lib:write_string(Head,Quote)];
format_error({illegal,Type}) -> io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) -> io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";
format_error(Other) -> io_lib:write(Other).

string_thing($') -> "atom";
string_thing($") -> "string".

%% Re-entrant pre-scanner.
%%
%% If the input list of characters is insufficient to build a term the
%% scanner returns a request for more characters and a continuation to be
%% used when trying to build a term with more characters. To indicate
%% end-of-file the input character list should be replaced with 'eof'
%% as an empty list has meaning.
%%
%% When more characters are need inside a comment, string or quoted
%% atom, which can become rather long, instead of pushing the
%% characters read so far back onto RestChars to be reread, a special
%% reentry token is returned indicating the middle of a construct.
%% The token is the start character as an atom, '%', '"' and '\''.

%% pre_scan([Char], SoFar, StartPos) ->
%%	{done,RestChars,ScannedChars,NewPos} |
%%	{more,RestChars,ScannedChars,NewPos} |
%%	{error,{ErrorPos,core_scan,Description},NewPos}.
%%  Main pre-scan function. It has been split into 2 functions because of
%%  efficiency, with a good indexing compiler it would be unnecessary.

pre_scan([C|Cs], SoFar, Pos) ->
    pre_scan(C, Cs, SoFar, Pos);
pre_scan([], SoFar, Pos) ->
    {more,[],SoFar,Pos};
pre_scan(eof, SoFar, Pos) ->
    {done,eof,SoFar,Pos}.

%% pre_scan(Char, [Char], SoFar, Pos)

pre_scan($$, Cs0, SoFar0, Pos) ->
    case pre_char(Cs0, [$$|SoFar0]) of
	{Cs,SoFar} ->
	    pre_scan(Cs, SoFar, Pos);
	more ->
	    {more,[$$|Cs0],SoFar0, Pos};
	error ->
	    pre_error(char, Pos, Pos)
    end;
pre_scan($', Cs, SoFar, Pos) ->
    pre_string(Cs, $', '\'', Pos, [$'|SoFar], Pos);
pre_scan({'\'',Sp}, Cs, SoFar, Pos) ->		%Re-entering quoted atom
    pre_string(Cs, $', '\'', Sp, SoFar, Pos);
pre_scan($", Cs, SoFar, Pos) ->
    pre_string(Cs, $", '"', Pos, [$"|SoFar], Pos);
pre_scan({'"',Sp}, Cs, SoFar, Pos) ->		%Re-entering string
    pre_string(Cs, $", '"', Sp, SoFar, Pos);
pre_scan($%, Cs, SoFar, Pos) ->
    pre_comment(Cs, SoFar, Pos);
pre_scan('%', Cs, SoFar, Pos) ->		%Re-entering comment
    pre_comment(Cs, SoFar, Pos);
pre_scan($\n, Cs, SoFar, Pos) ->
    pre_scan(Cs, [$\n|SoFar], Pos+1);
pre_scan(C, Cs, SoFar, Pos) ->
    pre_scan(Cs, [C|SoFar], Pos).

%% pre_string([Char], Quote, Reent, StartPos, SoFar, Pos)

pre_string([Q|Cs], Q, _, _, SoFar, Pos) ->
    pre_scan(Cs, [Q|SoFar], Pos);
pre_string([$\n|Cs], Q, Reent, Sp, SoFar, Pos) ->
    pre_string(Cs, Q, Reent, Sp, [$\n|SoFar], Pos+1);
pre_string([$\\|Cs0], Q, Reent, Sp, SoFar0, Pos) ->
    case pre_escape(Cs0, SoFar0) of
	{Cs,SoFar} ->
	    pre_string(Cs, Q, Reent, Sp, SoFar, Pos);
	more ->
	    {more,[{Reent,Sp},$\\|Cs0],SoFar0,Pos};
	error ->
	    pre_string_error(Q, Sp, SoFar0, Pos)
    end;
pre_string([C|Cs], Q, Reent, Sp, SoFar, Pos) ->
    pre_string(Cs, Q, Reent, Sp, [C|SoFar], Pos);
pre_string([], _, Reent, Sp, SoFar, Pos) ->
    {more,[{Reent,Sp}],SoFar,Pos};
pre_string(eof, Q, _, Sp, SoFar, Pos) ->
    pre_string_error(Q, Sp, SoFar, Pos).

pre_string_error(Q, Sp, SoFar, Pos) ->
    S = reverse(string:substr(SoFar, 1, string:chr(SoFar, Q)-1)),
    pre_error({string,Q,string:substr(S, 1, 16)}, Sp, Pos).

pre_char([C|Cs], SoFar) -> pre_char(C, Cs, SoFar);
pre_char([], _) -> more;
pre_char(eof, _) -> error.

pre_char($\\, Cs, SoFar) ->
    pre_escape(Cs, SoFar);
pre_char(C, Cs, SoFar) ->
    {Cs,[C|SoFar]}.

pre_escape([$^|Cs0], SoFar) ->
    case Cs0 of
	[C3|Cs] ->
	    {Cs,[C3,$^,$\\|SoFar]};
	[] -> more;
	eof -> error
    end;
pre_escape([C|Cs], SoFar) ->
    {Cs,[C,$\\|SoFar]};
pre_escape([], _) -> more;
pre_escape(eof, _) -> error.

%% pre_comment([Char], SoFar, Pos)
%%  Comments are replaced by one SPACE.

pre_comment([$\n|Cs], SoFar, Pos) ->
    pre_scan(Cs, [$\n,$\s|SoFar], Pos+1);	%Terminate comment
pre_comment([_|Cs], SoFar, Pos) ->
    pre_comment(Cs, SoFar, Pos);
pre_comment([], SoFar, Pos) ->
    {more,['%'],SoFar,Pos};
pre_comment(eof, Sofar, Pos) ->
    pre_scan(eof, [$\s|Sofar], Pos).

pre_error(E, Epos, Pos) ->
    {error,{Epos,core_scan,E}, Pos}.

%% scan(CharList, StartPos)
%%  This takes a list of characters and tries to tokenise them.
%%
%%  The token list is built in reverse order (in a stack) to save appending
%%  and then reversed when all the tokens have been collected. Most tokens
%%  are built in the same way.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,core_scan,What}}

scan(Cs, Pos) ->
    scan1(Cs, [], Pos).

%% scan1(Characters, TokenStack, Position)
%%  Scan a list of characters into tokens.

scan1([$\n|Cs], Toks, Pos) ->            	        %Skip newline
    scan1(Cs, Toks, Pos+1);
scan1([C|Cs], Toks, Pos) when C >= $\000, C =< $\s -> 	%Skip control chars
    scan1(Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $\200, C =< $\240 ->
    scan1(Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $a, C =< $z ->	%Keywords
    scan_key_word(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $ß, C =< $ÿ, C /= $÷ ->
    scan_key_word(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $A, C =< $Z ->	%Variables
    scan_variable(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $À, C =< $Þ, C /= $× ->
    scan_variable(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $0, C =< $9 ->	%Numbers
    scan_number(C, Cs, Toks, Pos);
scan1([$-,C|Cs], Toks, Pos) when C >= $0, C =< $9 ->	%Signed numbers
    scan_signed_number($-, C, Cs, Toks, Pos);
scan1([$+,C|Cs], Toks, Pos) when C >= $0, C =< $9 ->	%Signed numbers
    scan_signed_number($+, C, Cs, Toks, Pos);
scan1([$_|Cs], Toks, Pos) ->				%_ variables
    scan_variable($_, Cs, Toks, Pos);
scan1([$$|Cs0], Toks, Pos) ->				%Character constant
    {C,Cs,Pos1} = scan_char(Cs0, Pos),
    scan1(Cs, [{char,Pos,C}|Toks], Pos1);
scan1([$'|Cs0], Toks, Pos) ->				%Atom (always quoted)
    {S,Cs1,Pos1} = scan_string(Cs0, $', Pos),
    case catch list_to_atom(S) of
	A when atom(A) ->
	    scan1(Cs1, [{atom,Pos,A}|Toks], Pos1);
	_Error -> scan_error({illegal,atom}, Pos)
    end;
scan1([$"|Cs0], Toks, Pos) ->				%String
    {S,Cs1,Pos1} = scan_string(Cs0, $", Pos),
    scan1(Cs1, [{string,Pos,S}|Toks], Pos1);
%% Punctuation characters and operators, first recognise multiples.
scan1("->" ++ Cs, Toks, Pos) ->
    scan1(Cs, [{'->',Pos}|Toks], Pos);
scan1("-|" ++ Cs, Toks, Pos) ->
    scan1(Cs, [{'-|',Pos}|Toks], Pos);
scan1([C|Cs], Toks, Pos) ->				%Punctuation character
    P = list_to_atom([C]),
    scan1(Cs, [{P,Pos}|Toks], Pos);
scan1([], Toks0, _) ->
    Toks = reverse(Toks0),
    {ok,Toks}.

%% scan_key_word(FirstChar, CharList, Tokens, Pos)
%% scan_variable(FirstChar, CharList, Tokens, Pos)

scan_key_word(C, Cs0, Toks, Pos) ->
    {Wcs,Cs} = scan_name(Cs0, []),
    case catch list_to_atom([C|reverse(Wcs)]) of
	Name when atom(Name) ->
	    scan1(Cs, [{Name,Pos}|Toks], Pos);
	_Error -> scan_error({illegal,atom}, Pos)
    end.

scan_variable(C, Cs0, Toks, Pos) ->
    {Wcs,Cs} = scan_name(Cs0, []),
    case catch list_to_atom([C|reverse(Wcs)]) of
	Name when atom(Name) ->
	    scan1(Cs, [{var,Pos,Name}|Toks], Pos);
	_Error -> scan_error({illegal,var}, Pos)
    end.

%% scan_name(Cs) -> lists:splitwith(fun (C) -> name_char(C) end, Cs).

scan_name([C|Cs], Ncs) ->
    case name_char(C) of
	true -> scan_name(Cs, [C|Ncs]);
	false -> {Ncs,[C|Cs]}			%Must rebuild here, sigh!
    end;
scan_name([], Ncs) ->
    {Ncs,[]}.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $ß, C =< $ÿ, C /= $÷ -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $À, C =< $Þ, C /= $× -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

%% scan_string(CharList, QuoteChar, Pos) -> {StringChars,RestChars,NewPos}.

scan_string(Cs, Q, Pos) ->
    scan_string(Cs, [], Q, Pos).

scan_string([Q|Cs], Scs, Q, Pos) ->
    {reverse(Scs),Cs,Pos};
scan_string([$\n|Cs], Scs, Q, Pos) ->
    scan_string(Cs, [$\n|Scs], Q, Pos+1);
scan_string([$\\|Cs0], Scs, Q, Pos) ->
    {C,Cs,Pos1} = scan_escape(Cs0, Pos),
    scan_string(Cs, [C|Scs], Q, Pos1);
scan_string([C|Cs], Scs, Q, Pos) ->
    scan_string(Cs, [C|Scs], Q, Pos).

%% scan_char(Chars, Pos) -> {Char,RestChars,NewPos}.
%%  Read a single character from a character constant. The pre-scan
%%  phase has checked for errors here.

scan_char([$\\|Cs], Pos) ->
    scan_escape(Cs, Pos);
scan_char([$\n|Cs], Pos) ->                  %Newline
    {$\n,Cs,Pos+1};
scan_char([C|Cs], Pos) ->
    {C,Cs,Pos}.

scan_escape([O1,O2,O3|Cs], Pos) when            %\<1-3> octal digits
    O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,Cs,Pos};
scan_escape([O1,O2|Cs], Pos) when
    O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,Cs,Pos};
scan_escape([O1|Cs], Pos) when
    O1 >= $0, O1 =< $7 ->
    {O1 - $0,Cs,Pos};
scan_escape([$^,C|Cs], Pos) ->			%\^X -> CTL-X
    Val = C band 31,
    {Val,Cs,Pos};
%scan_escape([$\n,C1|Cs],Pos) ->
%    {C1,Cs,Pos+1};
%scan_escape([C,C1|Cs],Pos) when C >= $\000, C =< $\s ->
%    {C1,Cs,Pos};
scan_escape([$\n|Cs],Pos) ->
    {$\n,Cs,Pos+1};
scan_escape([C0|Cs],Pos) ->
    C = escape_char(C0),
    {C,Cs,Pos}.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.

%% scan_number(Char, CharList, TokenStack, Pos)
%%  We can handle simple radix notation:
%%    <digit>#<digits>		- the digits read in that base
%%    <digits>			- the digits in base 10
%%    <digits>.<digits>
%%    <digits>.<digits>E+-<digits>
%%
%%  Except for explicitly based integers we build a list of all the
%%  characters and then use list_to_integer/1 or list_to_float/1 to
%%  generate the value.

%%  SPos == Start position
%%  CPos == Current position

scan_number(C, Cs0, Toks, Pos) ->
    {Ncs,Cs,Pos1} = scan_integer(Cs0, [C], Pos),
    scan_after_int(Cs, Ncs, Toks, Pos, Pos1).

scan_signed_number(S, C, Cs0, Toks, Pos) ->
    {Ncs,Cs,Pos1} = scan_integer(Cs0, [C,S], Pos),
    scan_after_int(Cs, Ncs, Toks, Pos, Pos1).

scan_integer([C|Cs], Stack, Pos) when C >= $0, C =< $9 ->
    scan_integer(Cs, [C|Stack], Pos);
scan_integer(Cs, Stack, Pos) ->
    {Stack,Cs,Pos}.

scan_after_int([$.,C|Cs0], Ncs0, Toks, SPos, CPos) when C >= $0, C =< $9 ->
    {Ncs,Cs,CPos1} = scan_integer(Cs0, [C,$.|Ncs0], CPos),
    scan_after_fraction(Cs, Ncs, Toks, SPos, CPos1);
scan_after_int([$#|Cs], Ncs, Toks, SPos, CPos) ->
    case list_to_integer(reverse(Ncs)) of
	Base when Base >= 2, Base =< 16 ->
	    scan_based_int(Cs, 0, Base, Toks, SPos, CPos);
	Base ->
	    scan_error({base,Base}, CPos)
    end;
scan_after_int(Cs, Ncs, Toks, SPos, CPos) ->
    N = list_to_integer(reverse(Ncs)),
    scan1(Cs, [{integer,SPos,N}|Toks], CPos).

scan_based_int([C|Cs], SoFar, Base, Toks, SPos, CPos) when
    C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    scan_based_int(Cs, Next, Base, Toks, SPos, CPos);
scan_based_int([C|Cs], SoFar, Base, Toks, SPos, CPos) when
    C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    scan_based_int(Cs, Next, Base, Toks, SPos, CPos);
scan_based_int([C|Cs], SoFar, Base, Toks, SPos, CPos) when
    C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    scan_based_int(Cs, Next, Base, Toks, SPos, CPos);
scan_based_int(Cs, SoFar, _, Toks, SPos, CPos) ->
    scan1(Cs, [{integer,SPos,SoFar}|Toks], CPos).

scan_after_fraction([$E|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$E|Ncs], Toks, SPos, CPos);
scan_after_fraction([$e|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$E|Ncs], Toks, SPos, CPos);
scan_after_fraction(Cs, Ncs, Toks, SPos, CPos) ->
    case catch list_to_float(reverse(Ncs)) of
	N when float(N) ->
	    scan1(Cs, [{float,SPos,N}|Toks], CPos);
	_Error -> scan_error({illegal,float}, SPos)
    end.

%% scan_exponent(CharList, NumberCharStack, TokenStack, StartPos, CurPos)
%%  Generate an error here if E{+|-} not followed by any digits.

scan_exponent([$+|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, [$+|Ncs], Toks, SPos, CPos);
scan_exponent([$-|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, [$-|Ncs], Toks, SPos, CPos);
scan_exponent(Cs, Ncs, Toks, SPos, CPos) ->
    scan_exponent1(Cs, Ncs, Toks, SPos, CPos).

scan_exponent1([C|Cs0], Ncs0, Toks, SPos, CPos) when C >= $0, C =< $9 ->
    {Ncs,Cs,CPos1} = scan_integer(Cs0, [C|Ncs0], CPos),
    case catch list_to_float(reverse(Ncs)) of
	N when float(N) ->
	    scan1(Cs, [{float,SPos,N}|Toks], CPos1);
	_Error -> scan_error({illegal,float}, SPos)
    end;
scan_exponent1(_, _, _, _, CPos) ->
    scan_error(float, CPos).

scan_error(In, Pos) ->
    {error,{Pos,core_scan,In}}.
