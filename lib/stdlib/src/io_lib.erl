%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

%% This module is a library of useful i/o functions. It is hoped that the
%% functions defined in it are basic enough to be used without modification
%% as components of more complex utilities.
%%
%% It is completely self-contained and uses no other modules. Its own
%% utilities are exported.
%%
%% Most of the code here is derived from the original prolog versions and
%% from similar code written by Joe Armstrong and myself.
%%
%% This module has been split into separate modules:
%% io_lib        - basic write and utilities
%% io_lib_format - formatted output
%% io_lib_fread  - formatted input
%% io_lib_pretty - term prettyprinter

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
%% watch using × \327, very close to x \170

-module(io_lib).

-export([fwrite/2,fread/2,fread/3,format/2]).
-export([scan_format/2,unscan_format/1,build_text/1]).
-export([print/1,print/4,indentation/2]).

-export([write/1,write/2,write/3,nl/0,format_prompt/1,format_prompt/2]).
-export([write_atom/1,write_string/1,write_string/2,write_latin1_string/1,
         write_latin1_string/2, write_char/1, write_latin1_char/1]).

-export([write_atom_as_latin1/1, write_string_as_latin1/1,
         write_string_as_latin1/2, write_char_as_latin1/1]).

-export([quote_atom/2, char_list/1, latin1_char_list/1,
	 deep_char_list/1, deep_latin1_char_list/1,
	 printable_list/1, printable_latin1_list/1, printable_unicode_list/1]).

%% Utilities for collecting characters.
-export([collect_chars/3, collect_chars/4,
	 collect_line/2, collect_line/3, collect_line/4,
	 get_until/3, get_until/4]).

%% The following functions were used by Yecc's include-file.
-export([write_unicode_string/1, write_unicode_char/1,
         deep_unicode_char_list/1]).

-export_type([chars/0, latin1_string/0, continuation/0,
              fread_error/0, fread_item/0, format_spec/0]).

%%----------------------------------------------------------------------

-type chars() :: [char() | chars()].
-type latin1_string() :: [unicode:latin1_char()].
-type depth() :: -1 | non_neg_integer().

-opaque continuation() :: {Format :: string(),
                           Stack :: chars(),
                           Nchars :: non_neg_integer(),
                           Results :: [term()]}.

-type fread_error() :: 'atom'
                     | 'based'
                     | 'character'
                     | 'float'
                     | 'format'
                     | 'input'
                     | 'integer'
                     | 'string'
                     | 'unsigned'.

-type fread_item() :: string() | atom() | integer() | float().

-type format_spec() ::
        #{
           control_char := char(),
           args         := [any()],
           width        := 'none' | integer(),
           adjust       := 'left' | 'right',
           precision    := 'none' | integer(),
           pad_char     := char(),
           encoding     := 'unicode' | 'latin1',
           strings      := boolean()
         }.

%%----------------------------------------------------------------------

%% Interface calls to sub-modules.

-spec fwrite(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

fwrite(Format, Args) ->
    format(Format, Args).

-spec fread(Format, String) -> Result when
      Format :: string(),
      String :: string(),
      Result :: {'ok', InputList :: [fread_item()], LeftOverChars :: string()}
              | {'more', RestFormat :: string(),
                 Nchars :: non_neg_integer(),
                 InputStack :: chars()}
              | {'error', {'fread', What :: fread_error()}}.

fread(Chars, Format) ->
    io_lib_fread:fread(Chars, Format).

-spec fread(Continuation, CharSpec, Format) -> Return when
      Continuation :: continuation() | [],
      CharSpec :: string() | eof,
      Format :: string(),
      Return :: {'more', Continuation1 :: continuation()}
              | {'done', Result, LeftOverChars :: string()},
      Result :: {'ok', InputList :: [fread_item()]}
              | 'eof'
              | {'error', {'fread', What :: fread_error()}}.

fread(Cont, Chars, Format) ->
    io_lib_fread:fread(Cont, Chars, Format).

-spec format(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

format(Format, Args) ->
    case catch io_lib_format:fwrite(Format, Args) of
	{'EXIT',_} ->
	    erlang:error(badarg, [Format, Args]);
	Other ->
	    Other
    end.

-spec scan_format(Format, Data) -> FormatList when
      Format :: io:format(),
      Data :: [term()],
      FormatList :: [char() | format_spec()].

scan_format(Format, Args) ->
    try io_lib_format:scan(Format, Args)
    catch
        _:_ -> erlang:error(badarg, [Format, Args])
    end.

-spec unscan_format(FormatList) -> {Format, Data} when
      FormatList :: [char() | format_spec()],
      Format :: io:format(),
      Data :: [term()].

unscan_format(FormatList) ->
    io_lib_format:unscan(FormatList).

-spec build_text(FormatList) -> chars() when
      FormatList :: [char() | format_spec()].

build_text(FormatList) ->
    io_lib_format:build(FormatList).

-spec print(Term) -> chars() when
      Term :: term().

print(Term) ->
    io_lib_pretty:print(Term).

-spec print(Term, Column, LineLength, Depth) -> chars() when
      Term :: term(),
      Column :: non_neg_integer(),
      LineLength :: non_neg_integer(),
      Depth :: depth().

print(Term, Column, LineLength, Depth) ->
    io_lib_pretty:print(Term, Column, LineLength, Depth).

-spec indentation(String, StartIndent) -> integer() when
      String :: string(),
      StartIndent :: integer().

indentation(Chars, Current) ->
    io_lib_format:indentation(Chars, Current).


%% Format an IO-request prompt (handles formatting errors safely).
%% Atoms, binaries, and iolists (or unicode:charlist()) can be used
%% as-is, and will be printed without any additional quotes.

-spec format_prompt(term()) -> chars().

format_prompt(Prompt) ->
    format_prompt(Prompt, latin1).

-spec format_prompt(term(), atom()) -> chars().

format_prompt({format,Format,Args}, _Encoding) ->
    do_format_prompt(Format, Args);
format_prompt(Prompt, Encoding)
  when is_list(Prompt); is_atom(Prompt); is_binary(Prompt) ->
    do_format_prompt(add_modifier(Encoding, "s"), [Prompt]);
format_prompt(Prompt, Encoding) ->
    do_format_prompt(add_modifier(Encoding, "p"), [Prompt]).

do_format_prompt(Format, Args) ->
    case catch io_lib:format(Format, Args) of
	{'EXIT',_} -> "???";
	List -> List
    end.

add_modifier(latin1, C) ->
    "~"++C;
add_modifier(_, C) ->
    "~t"++C.

%% write(Term)
%% write(Term, Depth)
%% write(Term, Depth, Pretty)
%%  Return a (non-flattened) list of characters giving a printed
%%  representation of the term. write/3 is for backward compatibility.

-spec write(Term) -> chars() when
      Term :: term().

write(Term) -> write(Term, -1).

-spec write(term(), depth(), boolean()) -> chars().

write(Term, D, true) ->
    io_lib_pretty:print(Term, 1, 80, D);
write(Term, D, false) ->
    write(Term, D).

-spec write(Term, Depth) -> chars() when
      Term :: term(),
      Depth :: depth();
           (Term, Options) -> chars() when
      Term :: term(),
      Options :: [Option],
      Option :: {'depth', Depth}
              | {'encoding', 'latin1' | 'utf8' | 'unicode'},
      Depth :: depth().

write(Term, Options) when is_list(Options) ->
    Depth = get_option(depth, Options, -1),
    Encoding = get_option(encoding, Options, epp:default_encoding()),
    write1(Term, Depth, Encoding);
write(Term, Depth) ->
    write1(Term, Depth, latin1).

write1(_Term, 0, _E) -> "...";
write1(Term, _D, _E) when is_integer(Term) -> integer_to_list(Term);
write1(Term, _D, _E) when is_float(Term) -> io_lib_format:fwrite_g(Term);
write1(Atom, _D, latin1) when is_atom(Atom) -> write_atom_as_latin1(Atom);
write1(Atom, _D, _E) when is_atom(Atom) -> write_atom(Atom);
write1(Term, _D, _E) when is_port(Term) -> write_port(Term);
write1(Term, _D, _E) when is_pid(Term) -> pid_to_list(Term);
write1(Term, _D, _E) when is_reference(Term) -> write_ref(Term);
write1(<<_/bitstring>>=Term, D, _E) -> write_binary(Term, D);
write1([], _D, _E) -> "[]";
write1({}, _D, _E) -> "{}";
write1([H|T], D, E) ->
    if
	D =:= 1 -> "[...]";
	true ->
	    [$[,[write1(H, D-1, E)|write_tail(T, D-1, E, $|)],$]]
    end;
write1(F, _D, _E) when is_function(F) ->
    erlang:fun_to_list(F);
write1(Term, D, E) when is_map(Term) ->
    write_map(Term, D, E);
write1(T, D, E) when is_tuple(T) ->
    if
	D =:= 1 -> "{...}";
	true ->
	    [${,
	     [write1(element(1, T), D-1, E)|
              write_tail(tl(tuple_to_list(T)), D-1, E, $,)],
	     $}]
    end.

%% write_tail(List, Depth, CharacterBeforeDots)
%%  Test the terminating case first as this looks better with depth.

write_tail([], _D, _E, _S) -> "";
write_tail(_, 1, _E, S) -> [S | "..."];
write_tail([H|T], D, E, S) ->
    [$,,write1(H, D-1, E)|write_tail(T, D-1, E, S)];
write_tail(Other, D, E, S) ->
    [S,write1(Other, D-1, E)].

write_port(Port) ->
    erlang:port_to_list(Port).

write_ref(Ref) ->
    erlang:ref_to_list(Ref).

write_map(Map, D, E) when is_integer(D) ->
    [$#,${,write_map_body(maps:to_list(Map), D, E),$}].

write_map_body(_, 0, _E) -> "...";
write_map_body([], _, _E) -> [];
write_map_body([{K,V}], D, E) -> write_map_assoc(K, V, D, E);
write_map_body([{K,V}|KVs], D, E) ->
    [write_map_assoc(K, V, D, E),$, | write_map_body(KVs, D-1, E)].

write_map_assoc(K, V, D, E) ->
    [write1(K, D - 1, E),"=>",write1(V, D-1, E)].

write_binary(B, D) when is_integer(D) ->
    [$<,$<,write_binary_body(B, D),$>,$>].

write_binary_body(<<>>, _D) ->
    "";
write_binary_body(_B, 1) ->
    "...";
write_binary_body(<<X:8>>, _D) ->
    [integer_to_list(X)];
write_binary_body(<<X:8,Rest/bitstring>>, D) ->
    [integer_to_list(X),$,|write_binary_body(Rest, D-1)];
write_binary_body(B, _D) ->
    L = bit_size(B),
    <<X:L>> = B,
    [integer_to_list(X),$:,integer_to_list(L)].

get_option(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
	false -> Default;
	{Key, Value} -> Value;
	_ -> Default
    end.

%%% There are two functions to write Unicode atoms:
%%% - they both escape control characters < 160;
%%% - write_atom() never escapes characters >= 160;
%%% - write_atom_as_latin1() also escapes characters >= 255.

%% write_atom(Atom) -> [Char]
%%  Generate the list of characters needed to print an atom.

-spec write_atom(Atom) -> chars() when
      Atom :: atom().

write_atom(Atom) ->
    write_possibly_quoted_atom(Atom, fun write_string/2).

-spec write_atom_as_latin1(Atom) -> latin1_string() when
      Atom :: atom().

write_atom_as_latin1(Atom) ->
    write_possibly_quoted_atom(Atom, fun write_string_as_latin1/2).

write_possibly_quoted_atom(Atom, PFun) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
	true ->
            PFun(Chars, $');   %'
	false ->
	    Chars
    end.

%% quote_atom(Atom, CharList)
%%  Return 'true' if atom with chars in CharList needs to be quoted, else
%%  return 'false'. Notice that characters >= 160 are always quoted.

-spec quote_atom(atom(), chars()) -> boolean().

quote_atom(Atom, Cs0) ->
    case erl_scan:reserved_word(Atom) of
	true -> true;
	false ->
	    case Cs0 of
		[C|Cs] when C >= $a, C =< $z ->
		    not name_chars(Cs);
		[C|Cs] when C >= $ß, C =< $ÿ, C =/= $÷ ->
		    not name_chars(Cs);
		_ -> true
	    end
    end.

name_chars([C|Cs]) ->
    case name_char(C) of
	true -> name_chars(Cs);
	false -> false
    end;
name_chars([]) -> true.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

%%% There are two functions to write Unicode strings:
%%% - they both escape control characters < 160;
%%% - write_string() never escapes characters >= 160;
%%% - write_string_as_latin1() also escapes characters >= 255.

%% write_string([Char]) -> [Char]
%%  Generate the list of characters needed to print a string.

-spec write_string(String) -> chars() when
      String :: string().

write_string(S) ->
    write_string(S, $").   %"

-spec write_string(string(), char()) -> chars().

write_string(S, Q) ->
    [Q|write_string1(unicode_as_unicode, S, Q)].

%% Backwards compatibility.
write_unicode_string(S) ->
    write_string(S).

-spec write_latin1_string(Latin1String) -> latin1_string() when
      Latin1String :: latin1_string().

write_latin1_string(S) ->
    write_latin1_string(S, $").   %"

-spec write_latin1_string(latin1_string(), char()) -> latin1_string().

write_latin1_string(S, Q) ->
    [Q|write_string1(latin1, S, Q)].

-spec write_string_as_latin1(String) -> latin1_string() when
      String :: string().

write_string_as_latin1(S) ->
    write_string_as_latin1(S, $").   %"

-spec write_string_as_latin1(string(), char()) -> latin1_string().

write_string_as_latin1(S, Q) ->
    [Q|write_string1(unicode_as_latin1, S, Q)].

write_string1(_,[], Q) ->
    [Q];
write_string1(Enc,[C|Cs], Q) ->
    string_char(Enc,C, Q, write_string1(Enc,Cs, Q)).

string_char(_,Q, Q, Tail) -> [$\\,Q|Tail];	%Must check these first!
string_char(_,$\\, _, Tail) -> [$\\,$\\|Tail];
string_char(_,C, _, Tail) when C >= $\s, C =< $~ ->
    [C|Tail];
string_char(latin1,C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char(unicode_as_unicode,C, _, Tail) when C >= $\240 ->
    [C|Tail];
string_char(unicode_as_latin1,C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char(unicode_as_latin1,C, _, Tail) when C >= $\377 ->
    "\\x{"++erlang:integer_to_list(C, 16)++"}"++Tail;
string_char(_,$\n, _, Tail) -> [$\\,$n|Tail];	%\n = LF
string_char(_,$\r, _, Tail) -> [$\\,$r|Tail];	%\r = CR
string_char(_,$\t, _, Tail) -> [$\\,$t|Tail];	%\t = TAB
string_char(_,$\v, _, Tail) -> [$\\,$v|Tail];	%\v = VT
string_char(_,$\b, _, Tail) -> [$\\,$b|Tail];	%\b = BS
string_char(_,$\f, _, Tail) -> [$\\,$f|Tail];	%\f = FF
string_char(_,$\e, _, Tail) -> [$\\,$e|Tail];	%\e = ESC
string_char(_,$\d, _, Tail) -> [$\\,$d|Tail];	%\d = DEL
string_char(_,C, _, Tail) when C < $\240->	%Other control characters.
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3|Tail].

%%% There are two functions to write a Unicode character:
%%% - they both escape control characters < 160;
%%% - write_char() never escapes characters >= 160;
%%% - write_char_as_latin1() also escapes characters >= 255.

%% write_char(Char) -> [char()].
%%  Generate the list of characters needed to print a character constant.
%%  Must special case SPACE, $\s, here.

-spec write_char(Char) -> chars() when
      Char :: char().

write_char($\s) -> "$\\s";			%Must special case this.
write_char(C) when is_integer(C), C >= $\000 ->
    [$$|string_char(unicode_as_unicode, C, -1, [])].

%% Backwards compatibility.
write_unicode_char(C) ->
    write_char(C).

-spec write_latin1_char(Latin1Char) -> latin1_string() when
      Latin1Char :: unicode:latin1_char().

write_latin1_char(Lat1) when is_integer(Lat1), Lat1 >= $\000, Lat1 =< $\377  ->
    [$$|string_char(latin1, Lat1, -1, [])].

-spec write_char_as_latin1(Char) -> latin1_string() when
      Char :: char().

write_char_as_latin1(Uni) when is_integer(Uni), Uni >= $\000 ->
    [$$|string_char(unicode_as_latin1,Uni, -1, [])].

%% latin1_char_list(CharList)
%% deep_latin1_char_list(CharList)
%%  Return true if CharList is a (possibly deep) list of Latin-1
%%  characters, else false.

-spec latin1_char_list(Term) -> boolean() when
      Term :: term().

latin1_char_list([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    latin1_char_list(Cs);
latin1_char_list([]) -> true;
latin1_char_list(_) -> false.			%Everything else is false

-spec char_list(Term) -> boolean() when
      Term :: term().

char_list([C|Cs]) when is_integer(C), C >= 0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    char_list(Cs);
char_list([]) -> true;
char_list(_) -> false.			%Everything else is false

-spec deep_latin1_char_list(Term) -> boolean() when
      Term :: term().

deep_latin1_char_list(Cs) ->
    deep_latin1_char_list(Cs, []).

deep_latin1_char_list([C|Cs], More) when is_list(C) ->
    deep_latin1_char_list(C, [Cs|More]);
deep_latin1_char_list([C|Cs], More) when is_integer(C), C >= $\000, C =< $\377 ->
    deep_latin1_char_list(Cs, More);
deep_latin1_char_list([], [Cs|More]) ->
    deep_latin1_char_list(Cs, More);
deep_latin1_char_list([], []) -> true;
deep_latin1_char_list(_, _More) ->			%Everything else is false
    false.

-spec deep_char_list(Term) -> boolean() when
      Term :: term().

deep_char_list(Cs) ->
    deep_char_list(Cs, []).

deep_char_list([C|Cs], More) when is_list(C) ->
    deep_char_list(C, [Cs|More]);
deep_char_list([C|Cs], More)
  when is_integer(C), C >= 0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    deep_char_list(Cs, More);
deep_char_list([], [Cs|More]) ->
    deep_char_list(Cs, More);
deep_char_list([], []) -> true;
deep_char_list(_, _More) ->		%Everything else is false
    false.

deep_unicode_char_list(Term) ->
    deep_char_list(Term).

%% printable_latin1_list([Char]) -> boolean()
%%  Return true if CharList is a list of printable Latin1 characters, else
%%  false.

-spec printable_latin1_list(Term) -> boolean() when
      Term :: term().

printable_latin1_list([C|Cs]) when is_integer(C), C >= $\040, C =< $\176 ->
    printable_latin1_list(Cs);
printable_latin1_list([C|Cs]) when is_integer(C), C >= $\240, C =< $\377 ->
    printable_latin1_list(Cs);
printable_latin1_list([$\n|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\r|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\t|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\v|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\b|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\f|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([$\e|Cs]) -> printable_latin1_list(Cs);
printable_latin1_list([]) -> true;
printable_latin1_list(_) -> false.			%Everything else is false

%% printable_list([Char]) -> boolean()
%%  Return true if CharList is a list of printable characters, else
%%  false. The notion of printable in Unicode terms is somewhat floating.
%%  Everything that is not a control character and not invalid unicode
%%  will be considered printable. 
%%  What the user has noted as printable characters is what actually 
%%  specifies when this function will return true. If the VM is started
%%  with +pc latin1, only the latin1 range will be deemed as printable
%%  if on the other hand +pc unicode is given, all characters in the Unicode
%%  character set are deemed printable. latin1 is default.

-spec printable_list(Term) -> boolean() when
      Term :: term().

printable_list(L) ->
    %% There will be more alternatives returns from io:printable range 
    %% in the future. To not have a catch-all clause is deliberate.
    case io:printable_range() of
	latin1 ->
	    printable_latin1_list(L);
	unicode ->
	    printable_unicode_list(L)
    end.

-spec printable_unicode_list(Term) -> boolean() when
      Term :: term().

printable_unicode_list([C|Cs]) when is_integer(C), C >= $\040, C =< $\176 ->
    printable_unicode_list(Cs);
printable_unicode_list([C|Cs])
  when is_integer(C), C >= 16#A0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    printable_unicode_list(Cs);
printable_unicode_list([$\n|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\r|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\t|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\v|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\b|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\f|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([$\e|Cs]) -> printable_unicode_list(Cs);
printable_unicode_list([]) -> true;
printable_unicode_list(_) -> false.		%Everything else is false

%% List = nl()
%%  Return a list of characters to generate a newline.

-spec nl() -> string().

nl() ->
    "\n".

%%
%% Utilities for collecting characters in input files
%%

count_and_find_utf8(Bin,N) ->
    cafu(Bin,N,0,0,none).

cafu(<<>>,_N,Count,_ByteCount,SavePos) ->
    {Count,SavePos};
cafu(<<_/utf8,Rest/binary>>, 0, Count, ByteCount, _SavePos) ->
    cafu(Rest,-1,Count+1,0,ByteCount);
cafu(<<_/utf8,Rest/binary>>, N, Count, _ByteCount, SavePos) when N < 0 ->
    cafu(Rest,-1,Count+1,0,SavePos);
cafu(<<_/utf8,Rest/binary>> = Whole, N, Count, ByteCount, SavePos) ->
    Delta = byte_size(Whole) - byte_size(Rest),
    cafu(Rest,N-1,Count+1,ByteCount+Delta,SavePos);
cafu(_Other,_N,Count,_ByteCount,SavePos) -> % Non Utf8 character at end
    {Count,SavePos}.

%% collect_chars(State, Data, Count). New in R9C.
%%  Returns:
%%      {stop,Result,RestData}
%%      NewState
%%% BC (with pre-R13).
collect_chars(Tag, Data, N) ->
    collect_chars(Tag, Data, latin1, N).

%% Now we are aware of encoding...    
collect_chars(start, Data, unicode, N) when is_binary(Data) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,eof}
    end;
collect_chars(start, Data, latin1, N) when is_binary(Data) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,eof}
    end;
collect_chars(start,Data,_,N) when is_list(Data) ->
    collect_chars_list([], N, Data);
collect_chars(start, eof, _,_) ->
    {stop,eof,eof};
collect_chars({binary,Stack,_N}, eof, _,_) ->
    {stop,binrev(Stack),eof};
collect_chars({binary,Stack,N}, Data,unicode, _) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),eof}
    end;
collect_chars({binary,Stack,N}, Data,latin1, _) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),eof}
    end;
collect_chars({list,Stack,N}, Data, _,_) ->
    collect_chars_list(Stack, N, Data);
%% collect_chars(Continuation, MoreChars, Count)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}

collect_chars([], Chars, _, N) ->
    collect_chars1(N, Chars, []);
collect_chars({Left,Sofar}, Chars, _, _N) ->
    collect_chars1(Left, Chars, Sofar).

collect_chars1(N, Chars, Stack) when N =< 0 ->
    {done,lists:reverse(Stack, []),Chars};
collect_chars1(N, [C|Rest], Stack) ->
    collect_chars1(N-1, Rest, [C|Stack]);
collect_chars1(_N, eof, []) ->
    {done,eof,[]};
collect_chars1(_N, eof, Stack) ->
    {done,lists:reverse(Stack, []),[]};
collect_chars1(N, [], Stack) ->
    {more,{N,Stack}}.

collect_chars_list(Stack, 0, Data) ->
    {stop,lists:reverse(Stack, []),Data};
collect_chars_list(Stack, _N, eof) ->
    {stop,lists:reverse(Stack, []),eof};
collect_chars_list(Stack, N, []) ->
    {list,Stack,N};
collect_chars_list(Stack,N, [H|T]) ->
    collect_chars_list([H|Stack], N-1, T).

%% collect_line(Continuation, MoreChars)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}
%%
%%  XXX Can be removed when compatibility with pre-R12B-5 nodes
%%  is no longer required.
%% 
collect_line([], Chars) ->
    collect_line1(Chars, []);
collect_line({SoFar}, More) ->
    collect_line1(More, SoFar).

collect_line1([$\r, $\n|Rest], Stack) ->
    collect_line1([$\n|Rest], Stack);
collect_line1([$\n|Rest], Stack) ->
    {done,lists:reverse([$\n|Stack], []),Rest};
collect_line1([C|Rest], Stack) ->
    collect_line1(Rest, [C|Stack]);
collect_line1(eof, []) ->
    {done,eof,[]};
collect_line1(eof, Stack) ->
    {done,lists:reverse(Stack, []),[]};
collect_line1([], Stack) ->
    {more,{Stack}}.

%% collect_line(State, Data, _). New in R9C.
%%  Returns:
%%	{stop,Result,RestData}
%%	NewState
%%% BC (with pre-R13).
collect_line(Tag, Data, Any) -> 
    collect_line(Tag, Data, latin1, Any).

%% Now we are aware of encoding...    
collect_line(start, Data, Encoding, _) when is_binary(Data) ->
    collect_line_bin(Data, Data, [], Encoding);
collect_line(start, Data, _, _) when is_list(Data) ->
    collect_line_list(Data, []);
collect_line(start, eof, _, _) ->
    {stop,eof,eof};
collect_line(Stack, Data, Encoding, _) when is_binary(Data) ->
    collect_line_bin(Data, Data, Stack, Encoding);
collect_line(Stack, Data, _, _) when is_list(Data) ->
    collect_line_list(Data, Stack);
collect_line([B|_]=Stack, eof, _, _) when is_binary(B) ->
    {stop,binrev(Stack),eof};
collect_line(Stack, eof, _, _) ->
    {stop,lists:reverse(Stack, []),eof}.


collect_line_bin(<<$\n,T/binary>>, Data, Stack0, _) ->
    N = byte_size(Data) - byte_size(T),
    <<Line:N/binary,_/binary>> = Data,
    case Stack0 of
	[] ->
	    {stop,Line,T};
	[<<$\r>>|Stack] when N =:= 1 ->
	    {stop,binrev(Stack, [$\n]),T};
	_ ->
	    {stop,binrev(Stack0, [Line]),T}
    end;
collect_line_bin(<<$\r,$\n,T/binary>>, Data, Stack, _) ->
    N = byte_size(Data) - byte_size(T) - 2,
    <<Line:N/binary,_/binary>> = Data,
    {stop,binrev(Stack, [Line,$\n]),T};
collect_line_bin(<<$\r>>, Data0, Stack, _) ->
    N = byte_size(Data0) - 1,
    <<Data:N/binary,_/binary>> = Data0,
    [<<$\r>>,Data|Stack];
collect_line_bin(<<_,T/binary>>, Data, Stack, Enc) ->
    collect_line_bin(T, Data, Stack, Enc);
collect_line_bin(<<>>, Data, Stack, _) ->
    %% Need more data here.
    [Data|Stack].

collect_line_list([$\n|T], [$\r|Stack]) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([$\n|T], Stack) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([H|T], Stack) ->
    collect_line_list(T, [H|Stack]);
collect_line_list([], Stack) ->
    Stack.

%% Translator function to emulate a new (R9C and later) 
%% I/O client when you have an old one.
%%
%% Implements a middleman that is get_until server and get_chars client.

%%% BC (with pre-R13).
get_until(Any,Data,Arg) ->
    get_until(Any,Data,latin1,Arg).

%% Now we are aware of encoding...    
get_until(start, Data, Encoding, XtraArg) ->
    get_until([], Data, Encoding, XtraArg);
get_until(Cont, Data, Encoding, {Mod, Func, XtraArgs}) ->
    Chars = if is_binary(Data), Encoding =:= unicode ->
		    unicode:characters_to_list(Data,utf8);
	       is_binary(Data) ->
		    binary_to_list(Data);
	       true ->
		    Data
	    end,
    case apply(Mod, Func, [Cont,Chars|XtraArgs]) of
	{done,Result,Buf} ->
	    {stop,if is_binary(Data), 
		     is_list(Result), 
		     Encoding =:= unicode ->
			  unicode:characters_to_binary(Result,unicode,unicode);
		     is_binary(Data), 
		     is_list(Result) ->
			  erlang:iolist_to_binary(Result);
%%		     is_list(Data),
%%		     is_list(Result),
%% 		     Encoding =:= latin1 ->
%% 			  % Should check for only latin1, but skip that for
%% 			  % efficiency reasons.
%% 			  [ exit({cannot_convert, unicode, latin1}) || 
%% 			      X <- List, X > 255 ];
		     true ->
			  Result
		  end,
	     Buf};
	{more,NewCont} ->
	    NewCont
    end.

binrev(L) ->
    list_to_binary(lists:reverse(L, [])).

binrev(L, T) ->
    list_to_binary(lists:reverse(L, T)).
