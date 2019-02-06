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
%% The Initial Developer of the Original Code is Ericsson Utvecklings
%% AB. Portions created by Ericsson are Copyright 1999, Ericsson
%% Utvecklings AB. All Rights Reserved.''
%%
%% @private
%% @copyright Richard Carlsson 2001-2003. Portions created by Ericsson
%% are Copyright 1999, Ericsson Utvecklings AB. All Rights Reserved.
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end

%% @doc Tokeniser for EDoc. Based on the Erlang standard library module
%% {@link //stdlib/erl_scan}.

-module(edoc_scanner).

%% NOTE: the interface to this module is ancient and should be updated.
%% Please do not regard these exported functions as stable. Their
%% behaviour is described in the documentation of the module `erl_scan'.
%%
%% Since there are no `full stop' tokens in EDoc specifications, the
%% `tokens' function *always* returns `{more, Continuation}' unless an
%% error occurs.

-export([string/1,string/2,format_error/1]).

-import(lists, [reverse/1]).

string(Cs) -> string(Cs, 1).

string(Cs, StartPos) ->
    case scan(Cs, StartPos) of
	{ok,Toks} -> {ok,Toks,StartPos};
	{error,E} -> {error,E,StartPos}
    end.

%% format_error(Error)
%%  Return a string describing the error.

format_error({string,Quote,Head}) ->
    ["unterminated string starting with " ++ io_lib:write_string(Head,Quote)];
format_error({illegal,Type}) -> io_lib:fwrite("illegal ~w", [Type]);
format_error(char) -> "unterminated character";
format_error(scan) -> "premature end";
format_error({base,Base}) -> io_lib:fwrite("illegal base '~w'", [Base]);
format_error(float) -> "bad float";

format_error(Other) -> io_lib:write(Other).

%% Reserved words, not atoms:
reserved('where') -> true;
reserved(_) -> false.

%% scan(CharList, StartPos)
%%  This takes a list of characters and tries to tokenise them.
%%
%%  The token list is built in reverse order (in a stack) to save appending
%%  and then reversed when all the tokens have been collected. Most tokens
%%  are built in the same way.
%%
%%  Returns:
%%	{ok,[Tok]}
%%	{error,{ErrorPos,edoc_scanner,What}}

scan(Cs, Pos) ->
    scan1(Cs, [], Pos).

%% scan1(Characters, TokenStack, Position)
%%  Scan a list of characters into tokens.

scan1([$\n|Cs], Toks, Pos) ->            	        % Newline
    scan1(Cs, Toks, Pos+1);
scan1([C|Cs], Toks, Pos) when C >= 0, C =< $  -> 	% Skip blanks
    scan1(Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $a, C =< $z ->	% Unquoted atom
    scan_atom(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $\337, C =< $\377, C /= $\367 ->
    scan_atom(C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Numbers
    scan_number(C, Cs, Toks, Pos);
scan1([$-,C| Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Signed numbers
    scan_signed_number($-, C, Cs, Toks, Pos);
scan1([$+,C| Cs], Toks, Pos) when C >= $0, C =< $9 ->	% Signed numbers
    scan_signed_number($+, C, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $A, C =< $Z ->	% Variables
    scan_variable(C, Cs, Toks, Pos);
scan1([$_|Cs], Toks, Pos) ->				% Variables
    scan_variable($_, Cs, Toks, Pos);
scan1([C|Cs], Toks, Pos) when C >= $\300, C =< $\336, C /= $\327 ->
    scan_variable(C, Cs, Toks, Pos);
scan1([$$|Cs], Toks, Pos) ->			% Character constant
    case scan_char_const(Cs, Toks, Pos) of
	{ok, Result} ->
	    {ok, Result};
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, char}, Pos)
    end;
scan1([$'|Cs0], Toks, Pos) ->				% Quoted atom
    case scan_string(Cs0, $', Pos) of
	{S,Cs1,Pos1} ->
	    case catch list_to_atom(S) of
		A when is_atom(A) ->
		    scan1(Cs1, [{atom,Pos,A}|Toks], Pos1);
		_Error -> scan_error({illegal,atom}, Pos)
	    end;
	{error, premature_end} ->
	    scan_error({string,$',Cs0}, Pos);
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, atom}, Pos)
    end;
scan1([$"|Cs0], Toks, Pos) ->				% String
    case scan_string(Cs0, $", Pos) of
	{S,Cs1,Pos1} ->
	    case Toks of
		[{string, Pos0, S0} | Toks1] ->
		    scan1(Cs1, [{string, Pos0, S0 ++ S} | Toks1],
			  Pos1);
		_ ->
		    scan1(Cs1, [{string,Pos,S}|Toks], Pos1)
	    end;
	{error, premature_end} ->
	    scan_error({string,$",Cs0}, Pos);
	{error, truncated_char} ->
	    scan_error(char, Pos);
	{error, illegal_character} ->
	    scan_error({illegal, string}, Pos)
    end;
%% Punctuation characters and operators, first recognise multiples.
scan1([$=,$>|Cs], Toks, Pos) ->
    scan1(Cs, [{'=>',Pos}|Toks], Pos);
scan1([$<,$<|Cs], Toks, Pos) ->
    scan1(Cs, [{'<<',Pos}|Toks], Pos);
scan1([$>,$>|Cs], Toks, Pos) ->
    scan1(Cs, [{'>>',Pos}|Toks], Pos);
scan1([$-,$>|Cs], Toks, Pos) ->
    scan1(Cs, [{'->',Pos}|Toks], Pos);
scan1([$:,$=|Cs], Toks, Pos) ->
    scan1(Cs, [{':=',Pos}|Toks], Pos);
scan1([$:,$:|Cs], Toks, Pos) ->
    scan1(Cs, [{'::',Pos}|Toks], Pos);
scan1([$/,$/|Cs], Toks, Pos) ->
    scan1(Cs, [{'//',Pos}|Toks], Pos);
scan1([$.,$.,$.|Cs], Toks, Pos) ->
    scan1(Cs, [{'...',Pos}|Toks], Pos);
scan1([$.,$.|Cs], Toks, Pos) ->
    scan1(Cs, [{'..',Pos}|Toks], Pos);
scan1([C|Cs], Toks, Pos) -> % Punctuation character
    P = list_to_atom([C]),
    scan1(Cs, [{P,Pos}|Toks], Pos);
scan1([], Toks0, _Pos) ->
    Toks = reverse(Toks0),
    {ok,Toks}.

%% Note that `_' is not accepted as a variable token.
scan_variable(C, Cs, Toks, Pos) ->
    {Wcs,Cs1} = scan_name(Cs, []),
    W = [C|reverse(Wcs)],
    case W of
	"_" ->
            scan1(Cs1, [{an_var,Pos,'_'}|Toks], Pos);
	_ ->
	    case catch list_to_atom(W) of
		A when is_atom(A) ->
		    scan1(Cs1, [{var,Pos,A}|Toks], Pos);
		_ ->
		    scan_error({illegal,variable}, Pos)
	    end
    end.

scan_atom(C, Cs, Toks, Pos) ->
    {Wcs,Cs1} = scan_name(Cs, []),
    W = [C|reverse(Wcs)],
    case catch list_to_atom(W) of
	A when is_atom(A) ->
	    case reserved(A) of
		true ->
		    scan1(Cs1, [{A,Pos}|Toks], Pos);
		false ->
		    scan1(Cs1, [{atom,Pos,A}|Toks], Pos)
	    end;
	_ ->
	    scan_error({illegal,token}, Pos)
    end.

%% scan_name(Cs) -> lists:splitwith(fun (C) -> name_char(C) end, Cs).

scan_name([C|Cs], Ncs) ->
    case name_char(C) of
	true ->
	    scan_name(Cs, [C|Ncs]);
	false ->
	    {Ncs,[C|Cs]}		% Must rebuild here, sigh!
    end;
scan_name([], Ncs) ->
    {Ncs,[]}.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $\337, C =< $\377, C /= $\367 -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $\300, C =< $\336, C /= $\327 -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

%% scan_string(CharList, QuoteChar, Pos) ->
%%	{StringChars,RestChars, NewPos}

scan_string(Cs, Quote, Pos) ->
    scan_string(Cs, [], Quote, Pos).

scan_string([Quote|Cs], Scs, Quote, Pos) ->
    {reverse(Scs),Cs,Pos};
scan_string([], _Scs, _Quote, _Pos) ->
    {error, premature_end};
scan_string(Cs0, Scs, Quote, Pos) ->
    case scan_char(Cs0, Pos) of
	{C,Cs,Pos1} ->
	    %% Only build the string here
	    scan_string(Cs, [C|Scs], Quote, Pos1);
	Error ->
	    Error
    end.

%% Note that space characters are not allowed
scan_char_const([$\040 | _Cs0], _Toks, _Pos) ->
    {error, illegal_character};
scan_char_const(Cs0, Toks, Pos) ->
    case scan_char(Cs0, Pos) of
	{C,Cs,Pos1} ->
	    scan1(Cs, [{char,Pos,C}|Toks], Pos1);
	Error ->
	    Error
    end.

%% {Character,RestChars,NewPos} = scan_char(Chars, Pos)
%% Read a single character from a string or character constant. The
%% pre-scan phase has checked for errors here.
%% Note that control characters are not allowed.

scan_char([$\\|Cs], Pos) ->
    scan_escape(Cs, Pos);
scan_char([C | _Cs], _Pos) when C =< 16#1f ->
    {error, illegal_character};
scan_char([C|Cs], Pos) ->
    {C,Cs,Pos};
scan_char([], _Pos) ->
    {error, truncated_char}.

%% The following conforms to Standard Erlang escape sequences.

-define(HEX(C), C >= $0 andalso C =< $9 orelse
                C >= $A andalso C =< $F orelse
                C >= $a andalso C =< $f).

-define(UNICODE(C),
         (C >= 0 andalso C < 16#D800 orelse
          C > 16#DFFF andalso C < 16#FFFE orelse
          C > 16#FFFF andalso C =< 16#10FFFF)).

scan_escape([O1, O2, O3 | Cs], Pos) when        % \<1-3> octal digits
  O1 >= $0, O1 =< $3, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    Val = (O1*8 + O2)*8 + O3 - 73*$0,
    {Val,Cs,Pos};
scan_escape([O1, O2 | Cs], Pos) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7 ->
    Val = (O1*8 + O2) - 9*$0,
    {Val,Cs,Pos};
scan_escape([O1 | Cs], Pos) when
  O1 >= $0, O1 =< $7 ->
    {O1 - $0,Cs,Pos};
scan_escape([$x, ${ | Cs], Pos) ->
    scan_hex(Cs, Pos, []);
scan_escape([$x, H1, H2 | Cs], Pos) when ?HEX(H1), ?HEX(H2) ->
    Val = (H1*16 + H2) - 17*$0,
    {Val,Cs,Pos};
scan_escape([$^, C | Cs], Pos) ->    % \^X -> CTL-X
    if C >= $\100, C =< $\137 ->
	    {C - $\100,Cs,Pos};
       true -> {error, illegal_control_character}
    end;
scan_escape([C | Cs], Pos) ->
    case escape_char(C) of
	C1 when C1 > $\000 -> {C1,Cs,Pos};
	_ -> {error, undefined_escape_sequence}
    end;
scan_escape([], _Pos) ->
    {error, truncated_char}.

scan_hex([C | Cs], Pos, HCs) when ?HEX(C) ->
    scan_hex(Cs, Pos, [C | HCs]);
scan_hex([$} | Cs], Pos, HCs) ->
    case catch erlang:list_to_integer(lists:reverse(HCs), 16) of
        Val when ?UNICODE(Val) ->
            {Val,Cs,Pos};
        _ ->
            {error, undefined_escape_sequence}
    end;
scan_hex(_Cs, _Pos, _HCs) ->
    {error, undefined_escape_sequence}.

%% Note that we return $\000 for undefined escapes.
escape_char($b) -> $\010;		% \b = BS
escape_char($d) -> $\177;		% \d = DEL
escape_char($e) -> $\033;		% \e = ESC
escape_char($f) -> $\014;		% \f = FF
escape_char($n) -> $\012;		% \n = LF
escape_char($r) -> $\015;		% \r = CR
escape_char($s) -> $\040;		% \s = SPC
escape_char($t) -> $\011;		% \t = HT
escape_char($v) -> $\013;		% \v = VT
escape_char($\\) -> $\134;		% \\ = \
escape_char($') -> $\047;		% \' = '
escape_char($") -> $\042;		% \" = "
escape_char(_C) -> $\000.

%% scan_number(Char, CharList, TokenStack, Pos)
%%  We handle sign and radix notation:
%%    [+-]<digits>		- the digits in base [+-]10
%%    [+-]<digits>.<digits>
%%    [+-]<digits>.<digits>E+-<digits>
%%    [+-]<digits>#<digits>	- the digits read in base [+-]B
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
    {Ncs,Cs,Pos1} = scan_integer(Cs0, [C, S], Pos),
    scan_after_int(Cs, Ncs, Toks, Pos, Pos1).

scan_integer([C|Cs], Stack, Pos) when C >= $0, C =< $9 ->
    scan_integer(Cs, [C|Stack], Pos);
scan_integer(Cs, Stack, Pos) ->
    {Stack,Cs,Pos}.

scan_after_int([$.,C|Cs0], Ncs0, Toks, SPos, CPos) when C >= $0, C =< $9 ->
    {Ncs,Cs,CPos1} = scan_integer(Cs0, [C,$.|Ncs0], CPos),
    scan_after_fraction(Cs, Ncs, Toks, SPos, CPos1);
scan_after_int(Cs, Ncs, Toks, SPos, CPos) ->
    N = list_to_integer(reverse(Ncs)),
    scan1(Cs, [{integer,SPos,N}|Toks], CPos).

scan_after_fraction([$E|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$E|Ncs], Toks, SPos, CPos);
scan_after_fraction([$e|Cs], Ncs, Toks, SPos, CPos) ->
    scan_exponent(Cs, [$e|Ncs], Toks, SPos, CPos);
scan_after_fraction(Cs, Ncs, Toks, SPos, CPos) ->
    case catch list_to_float(reverse(Ncs)) of
	N when is_float(N) ->
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
	N when is_float(N) ->
	    scan1(Cs, [{float,SPos,N}|Toks], CPos1);
	_Error -> scan_error({illegal,float}, SPos)
    end;
scan_exponent1(_, _, _, _, CPos) ->
    scan_error(float, CPos).

scan_error(In, Pos) ->
    {error,{Pos,edoc_scanner,In}}.
