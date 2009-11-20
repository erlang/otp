%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
%%%*********************************************************************
%%% 
%%%   Description:      This file contains io functions adapted to the 
%%%                     TV requirements.
%%%
%%%*********************************************************************

-module(tv_io_lib).


-export([format/2]).

-export([write/1,write/2]).
-export([write_atom/1,write_string/2]).

-export([char_list/1,deep_char_list/1,printable_list/1]).



%% Interface calls to sub-modules.

format(Format, Args) ->
    tv_io_lib_format:fwrite(Format, Args).


%% write(Term)
%% write(Term, Depth)
%% write(Term, Depth, Pretty)
%%  Return a (non-flattened) list of characters giving a printed
%%  representation of the term.

write(Term) -> write(Term, -1).

write(_Term, 0) -> "...";
write(Term, _D) when is_integer(Term) -> integer_to_list(Term);
write(Term, _D) when is_float(Term) -> tv_io_lib_format:fwrite_g(Term);
write(Atom, _D) when is_atom(Atom) -> write_atom(Atom);
write(Term, _D) when is_port(Term) -> "#Port";
write(Term, _D) when is_pid(Term) -> pid_to_list(Term);
write(Term, _D) when is_reference(Term) -> "#Ref";
write(Term, _D) when is_binary(Term) -> "#Bin";
write(Term, _D) when is_bitstring(Term) -> "#Bitstr";
write([], _D) -> "[]";
write({}, _D) -> "{}";
write([H|T], D) ->
    if
	D =:= 1 -> "[...]";
	true ->
	    [$[,[write(H, D-1)|write_tail(T, D-1)],$]]
    end;
write(F, _D) when is_function(F) ->
    {module,M} = erlang:fun_info(F, module),
    ["#Fun<",atom_to_list(M),">"];
write(T, D) when is_tuple(T) ->
    if
	D =:= 1 -> "{...}";
	true ->
	    [${,
	     [write(element(1, T), D-1)|write_tail(tl(tuple_to_list(T)), D-1)],
	     $}]
    end.

%% write_tail(List, Depth)
%%  Test the terminating case first as this looks better with depth.

write_tail([], _D) -> "";
write_tail(_List, 1) -> "|...";
write_tail([H|T], D) ->
    [$,,write(H, D-1)|write_tail(T, D-1)];
write_tail(Other, D) ->
    [$|,write(Other, D-1)].

%% write_atom(Atom) -> [Char]
%%  Generate the list of characters needed to print an atom.

write_atom(Atom) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
	true ->
	    write_string(Chars, $');
	false ->
	    Chars
    end.


write_string(S, Q) ->
    [Q|write_string1(S, Q)].

write_string1([], Q) ->
    [Q];
write_string1([C|Cs], Q) ->
    write_char(C, Q, write_string1(Cs, Q)).


write_char(Q, Q, Tail) ->			%Must check this first
    [$\\,Q|Tail];
write_char($\\, _, Tail) ->			%In printable character range
    [$\\,$\\|Tail];
write_char(C, _, Tail) when C >= $ , C =< $~ ->
    [C|Tail];
write_char(C, _, Tail) when C >= 128+$ , C =< 255 ->
    [C|Tail];
write_char($\n, _Q, Tail) ->			%\n = LF
    [$\\,$n|Tail];
write_char($\r, _, Tail) ->			%\r = CR
    [$\\,$r|Tail];
write_char($\t, _, Tail) ->			%\t = TAB
    [$\\,$t|Tail];
write_char($\v, _, Tail) ->			%\v = VT
    [$\\,$v|Tail];
write_char($\b, _, Tail) ->			%\b = BS
    [$\\,$b|Tail];
write_char($\f, _, Tail) ->			%\f = FF
    [$\\,$f|Tail];
write_char($\e, _, Tail) ->			%\e = ESC
    [$\\,$e|Tail];
write_char($\d, _, Tail) ->			%\d = DEL
    [$\\,$d|Tail];
write_char(C, _, Tail) when C < $  ->
    C1 = (C bsr 3) + $0,
    C2 = (C band 7) + $0,
    [$\\,$0,C1,C2|Tail];
write_char(C, _, Tail) when C > $~ ->
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3|Tail].

%% quote_atom(Atom, CharList)
%%  Return 'true' if atom with chars in CharList needs to be quoted, else
%%  return 'false'.

quote_atom(Atom, Cs0) ->
    case erl_scan:reserved_word(Atom) of
	true -> true;
	false ->
	    case Cs0 of
		[C|Cs] when C >= $a, C =< $z ->
		    quote_atom(Cs);
		_ -> true
	    end
    end.

quote_atom([C|Cs]) when C >= $a, C =< $z ->
    quote_atom(Cs);
quote_atom([C|Cs]) when C >= $A, C =< $Z ->
    quote_atom(Cs);
quote_atom([C|Cs]) when C >= $0, C =< $9 ->
    quote_atom(Cs);
quote_atom([$_|Cs]) ->
    quote_atom(Cs);
quote_atom([$@|Cs]) ->
    quote_atom(Cs);
quote_atom([_|_]) ->
    true;
quote_atom([]) ->
    false.

%% char_list(CharList)
%% deep_char_list(CharList)
%%  Return true if CharList is a (possibly deep) list of characters, else
%%  false.

char_list([C|Cs]) when is_integer(C), C >= 0, C =< 255 ->
    char_list(Cs);
char_list([]) -> true;
char_list(_Other) -> false.			%Everything else is false

deep_char_list(Cs) ->
    deep_char_list(Cs, []).

deep_char_list([C|Cs], More) when is_list(C) ->
    deep_char_list(C, [Cs|More]);
deep_char_list([C|Cs], More) when is_integer(C), C >= 0, C =< 255 ->
    deep_char_list(Cs, More);
deep_char_list([], [Cs|More]) ->
    deep_char_list(Cs, More);
deep_char_list([], []) -> true;
deep_char_list(_Other, _More) ->	     %Everything else is false
    false.

%% printable_list([Char]) -> bool()
%%  Return true if CharList is a list of printable characters, else
%%  false.

printable_list([C|Cs]) when is_integer(C), C >= $ , C =< 255 ->
    printable_list(Cs);
printable_list([$\n|Cs]) ->
    printable_list(Cs);
printable_list([$\r|Cs]) ->
    printable_list(Cs);
printable_list([$\t|Cs]) ->
    printable_list(Cs);
printable_list([$\v|Cs]) ->
    printable_list(Cs);
printable_list([$\b|Cs]) ->
    printable_list(Cs);
printable_list([$\f|Cs]) ->
    printable_list(Cs);
printable_list([$\e|Cs]) ->
    printable_list(Cs);
printable_list([]) -> true;
printable_list(_Other) -> false.	     %Everything else is false


