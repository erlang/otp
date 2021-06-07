%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
-module(io_lib_format).

%% Formatting functions of io library.

-export([fwrite/2,fwrite/3,fwrite_g/1,indentation/2,scan/2,unscan/1,
         build/1, build/2]).

%%  Format the arguments in Args after string Format. Just generate
%%  an error if there is an error in the arguments.
%%
%%  To do the printing command correctly we need to calculate the
%%  current indentation for everything before it. This may be very
%%  expensive, especially when it is not needed, so we first determine
%%  if, and for how long, we need to calculate the indentations. We do
%%  this by first collecting all the control sequences and
%%  corresponding arguments, then counting the print sequences and
%%  then building the output.  This method has some drawbacks, it does
%%  two passes over the format string and creates more temporary data,
%%  and it also splits the handling of the control characters into two
%%  parts.

-spec fwrite(Format, Data) -> io_lib:chars() when
      Format :: io:format(),
      Data :: [term()].

fwrite(Format, Args) ->
    build(scan(Format, Args)).

-spec fwrite(Format, Data, Options) -> io_lib:chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: io_lib:chars_limit().

fwrite(Format, Args, Options) ->
    build(scan(Format, Args), Options).

%% Build the output text for a pre-parsed format list.

-spec build(FormatList) -> io_lib:chars() when
      FormatList :: [char() | io_lib:format_spec()].

build(Cs) ->
    build(Cs, []).

-spec build(FormatList, Options) -> io_lib:chars() when
      FormatList :: [char() | io_lib:format_spec()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: io_lib:chars_limit().

build(Cs, Options) ->
    CharsLimit = get_option(chars_limit, Options, -1),
    Res1 = build_small(Cs),
    {P, S, W, Other} = count_small(Res1),
    case P + S + W of
        0 ->
            Res1;
        NumOfLimited ->
            RemainingChars = sub(CharsLimit, Other),
            build_limited(Res1, P, NumOfLimited, RemainingChars, 0)
    end.

%% Parse all control sequences in the format string.

-spec scan(Format, Data) -> FormatList when
      Format :: io:format(),
      Data :: [term()],
      FormatList :: [char() | io_lib:format_spec()].

scan(Format, Args) when is_atom(Format) ->
    scan(atom_to_list(Format), Args);
scan(Format, Args) when is_binary(Format) ->
    scan(binary_to_list(Format), Args);
scan(Format, Args) ->
    collect(Format, Args).

%% Revert a pre-parsed format list to a plain character list and a
%% list of arguments.

-spec unscan(FormatList) -> {Format, Data} when
      FormatList :: [char() | io_lib:format_spec()],
      Format :: io:format(),
      Data :: [term()].

unscan(Cs) ->
    {print(Cs), args(Cs)}.

args([#{args := As} | Cs]) ->
    As ++ args(Cs);
args([_C | Cs]) ->
    args(Cs);
args([]) ->
    [].

print([#{control_char := C, width := F, adjust := Ad, precision := P,
         pad_char := Pad, encoding := Encoding, strings := Strings} | Cs]) ->
    print(C, F, Ad, P, Pad, Encoding, Strings) ++ print(Cs);
print([C | Cs]) ->
    [C | print(Cs)];
print([]) ->
    [].

print(C, F, Ad, P, Pad, Encoding, Strings) ->
    [$~] ++ print_field_width(F, Ad) ++ print_precision(P, Pad) ++
        print_pad_char(Pad) ++ print_encoding(Encoding) ++
        print_strings(Strings) ++ [C].

print_field_width(none, _Ad) -> "";
print_field_width(F, left) -> integer_to_list(-F);
print_field_width(F, right) -> integer_to_list(F).

print_precision(none, $\s) -> "";
print_precision(none, _Pad) -> ".";  % pad must be second dot
print_precision(P, _Pad) -> [$. | integer_to_list(P)].

print_pad_char($\s) -> ""; % default, no need to make explicit
print_pad_char(Pad) -> [$., Pad].

print_encoding(unicode) -> "t";
print_encoding(latin1) -> "".

print_strings(false) -> "l";
print_strings(true) -> "".

collect([$~|Fmt0], Args0) ->
    {C,Fmt1,Args1} = collect_cseq(Fmt0, Args0),
    [C|collect(Fmt1, Args1)];
collect([C|Fmt], Args) ->
    [C|collect(Fmt, Args)];
collect([], []) -> [].

collect_cseq(Fmt0, Args0) ->
    {F,Ad,Fmt1,Args1} = field_width(Fmt0, Args0),
    {P,Fmt2,Args2} = precision(Fmt1, Args1),
    {Pad,Fmt3,Args3} = pad_char(Fmt2, Args2),
    Spec0 = #{width => F,
              adjust => Ad,
              precision => P,
              pad_char => Pad,
              encoding => latin1,
              strings => true},
    {Spec1,Fmt4} = modifiers(Fmt3, Spec0),
    {C,As,Fmt5,Args4} = collect_cc(Fmt4, Args3),
    Spec2 = Spec1#{control_char => C, args => As},
    {Spec2,Fmt5,Args4}.

modifiers([$t|Fmt], Spec) ->
    modifiers(Fmt, Spec#{encoding => unicode});
modifiers([$l|Fmt], Spec) ->
    modifiers(Fmt, Spec#{strings => false});
modifiers(Fmt, Spec) ->
    {Spec, Fmt}.

field_width([$-|Fmt0], Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F,left,Fmt,Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F,right,Fmt,Args}.

precision([$.|Fmt], Args) ->
    field_value(Fmt, Args);
precision(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([$*|Fmt], [A|Args]) when is_integer(A) ->
    {A,Fmt,Args};
field_value([C|Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([C|Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10*F + (C - $0));
field_value(Fmt, Args, F) ->		%Default case
    {F,Fmt,Args}.

pad_char([$.,$*|Fmt], [Pad|Args]) -> {Pad,Fmt,Args};
pad_char([$.,Pad|Fmt], Args) -> {Pad,Fmt,Args};
pad_char(Fmt, Args) -> {$\s,Fmt,Args}.

%% collect_cc([FormatChar], [Argument]) ->
%%	{Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w|Fmt], [A|Args]) -> {$w,[A],Fmt,Args};
collect_cc([$p|Fmt], [A|Args]) -> {$p,[A],Fmt,Args};
collect_cc([$W|Fmt], [A,Depth|Args]) -> {$W,[A,Depth],Fmt,Args};
collect_cc([$P|Fmt], [A,Depth|Args]) -> {$P,[A,Depth],Fmt,Args};
collect_cc([$s|Fmt], [A|Args]) -> {$s,[A],Fmt,Args};
collect_cc([$e|Fmt], [A|Args]) -> {$e,[A],Fmt,Args};
collect_cc([$f|Fmt], [A|Args]) -> {$f,[A],Fmt,Args};
collect_cc([$g|Fmt], [A|Args]) -> {$g,[A],Fmt,Args};
collect_cc([$b|Fmt], [A|Args]) -> {$b,[A],Fmt,Args};
collect_cc([$B|Fmt], [A|Args]) -> {$B,[A],Fmt,Args};
collect_cc([$x|Fmt], [A,Prefix|Args]) -> {$x,[A,Prefix],Fmt,Args};
collect_cc([$X|Fmt], [A,Prefix|Args]) -> {$X,[A,Prefix],Fmt,Args};
collect_cc([$+|Fmt], [A|Args]) -> {$+,[A],Fmt,Args};
collect_cc([$#|Fmt], [A|Args]) -> {$#,[A],Fmt,Args};
collect_cc([$c|Fmt], [A|Args]) -> {$c,[A],Fmt,Args};
collect_cc([$~|Fmt], Args) when is_list(Args) -> {$~,[],Fmt,Args};
collect_cc([$n|Fmt], Args) when is_list(Args) -> {$n,[],Fmt,Args};
collect_cc([$i|Fmt], [A|Args]) -> {$i,[A],Fmt,Args}.

%% count_small([ControlC]) -> Count.
%%  Count the number of big (pPwWsS) print requests and
%%  number of characters of other print (small) requests.

count_small(Cs) ->
    count_small(Cs, #{p => 0, s => 0, w => 0, other => 0}).

count_small([#{control_char := $p}|Cs], #{p := P} = Cnts) ->
    count_small(Cs, Cnts#{p := P + 1});
count_small([#{control_char := $P}|Cs], #{p := P} = Cnts) ->
    count_small(Cs, Cnts#{p := P + 1});
count_small([#{control_char := $w}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([#{control_char := $W}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([#{control_char := $s}|Cs], #{w := W} = Cnts) ->
    count_small(Cs, Cnts#{w := W + 1});
count_small([S|Cs], #{other := Other} = Cnts) when is_list(S);
                                                   is_binary(S) ->
    count_small(Cs, Cnts#{other := Other + io_lib:chars_length(S)});
count_small([C|Cs], #{other := Other} = Cnts) when is_integer(C) ->
    count_small(Cs, Cnts#{other := Other + 1});
count_small([], #{p := P, s := S, w := W, other := Other}) ->
    {P, S, W, Other}.

%% build_small([Control]) -> io_lib:chars().
%%  Interpret the control structures, but only the small ones.
%%  The big ones are saved for later.
%% build_limited([Control], NumberOfPps, NumberOfLimited,
%%               CharsLimit, Indentation)
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build_small([#{control_char := C, args := As, width := F, adjust := Ad,
               precision := P, pad_char := Pad, encoding := Enc}=CC | Cs]) ->
    case control_small(C, As, F, Ad, P, Pad, Enc) of
        not_small -> [CC | build_small(Cs)];
        S -> lists:flatten(S) ++ build_small(Cs)
    end;
build_small([C|Cs]) -> [C|build_small(Cs)];
build_small([]) -> [].

build_limited([#{control_char := C, args := As, width := F, adjust := Ad,
                 precision := P, pad_char := Pad, encoding := Enc,
                 strings := Str} | Cs], NumOfPs0, Count0, MaxLen0, I) ->
    MaxChars = if
                   MaxLen0 < 0 -> MaxLen0;
                   true -> MaxLen0 div Count0
               end,
    S = control_limited(C, As, F, Ad, P, Pad, Enc, Str, MaxChars, I),
    NumOfPs = decr_pc(C, NumOfPs0),
    Count = Count0 - 1,
    MaxLen = if
                 MaxLen0 < 0 -> % optimization
                     MaxLen0;
                 true ->
                     Len = io_lib:chars_length(S),
                     sub(MaxLen0, Len)
             end,
    if
	NumOfPs > 0 -> [S|build_limited(Cs, NumOfPs, Count,
                                        MaxLen, indentation(S, I))];
	true -> [S|build_limited(Cs, NumOfPs, Count, MaxLen, I)]
    end;
build_limited([$\n|Cs], NumOfPs, Count, MaxLen, _I) ->
    [$\n|build_limited(Cs, NumOfPs, Count, MaxLen, 0)];
build_limited([$\t|Cs], NumOfPs, Count, MaxLen, I) ->
    [$\t|build_limited(Cs, NumOfPs, Count, MaxLen, ((I + 8) div 8) * 8)];
build_limited([C|Cs], NumOfPs, Count, MaxLen, I) ->
    [C|build_limited(Cs, NumOfPs, Count, MaxLen, I+1)];
build_limited([], _, _, _, _) -> [].

decr_pc($p, Pc) -> Pc - 1;
decr_pc($P, Pc) -> Pc - 1;
decr_pc(_, Pc) -> Pc.

%%  Calculate the indentation of the end of a string given its start
%%  indentation. We assume tabs at 8 cols.

-spec indentation(String, StartIndent) -> integer() when
      String :: io_lib:chars(),
      StartIndent :: integer().

indentation([$\n|Cs], _I) -> indentation(Cs, 0);
indentation([$\t|Cs], I) -> indentation(Cs, ((I + 8) div 8) * 8);
indentation([C|Cs], I) when is_integer(C) ->
    indentation(Cs, I+1);
indentation([C|Cs], I) ->
    indentation(Cs, indentation(C, I));
indentation([], I) -> I.

%% control_small(FormatChar, [Argument], FieldWidth, Adjust, Precision,
%%               PadChar, Encoding) -> String
%% control_limited(FormatChar, [Argument], FieldWidth, Adjust, Precision,
%%                 PadChar, Encoding, StringP, ChrsLim, Indentation) -> String
%%  These are the dispatch functions for the various formatting controls.

control_small($s, [A], F, Adj, P, Pad, latin1=Enc) when is_atom(A) ->
    L = iolist_to_chars(atom_to_list(A)),
    string(L, F, Adj, P, Pad, Enc);
control_small($s, [A], F, Adj, P, Pad, unicode=Enc) when is_atom(A) ->
    string(atom_to_list(A), F, Adj, P, Pad, Enc);
control_small($e, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_e(A, F, Adj, P, Pad);
control_small($f, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_f(A, F, Adj, P, Pad);
control_small($g, [A], F, Adj, P, Pad, _Enc) when is_float(A) ->
    fwrite_g(A, F, Adj, P, Pad);
control_small($b, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control_small($B, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control_small($x, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A),
                                                         is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
control_small($x, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true);
control_small($X, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A),
                                                         is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
control_small($X, [A,Prefix], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false);
control_small($+, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control_small($#, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    Base = base(P),
    Prefix = [integer_to_list(Base), $#],
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control_small($c, [A], F, Adj, P, Pad, unicode) when is_integer(A) ->
    char(A, F, Adj, P, Pad);
control_small($c, [A], F, Adj, P, Pad, _Enc) when is_integer(A) ->
    char(A band 255, F, Adj, P, Pad);
control_small($~, [], F, Adj, P, Pad, _Enc) -> char($~, F, Adj, P, Pad);
control_small($n, [], F, Adj, P, Pad, _Enc) -> newline(F, Adj, P, Pad);
control_small($i, [_A], _F, _Adj, _P, _Pad, _Enc) -> [];
control_small(_C, _As, _F, _Adj, _P, _Pad, _Enc) -> not_small.

control_limited($s, [L0], F, Adj, P, Pad, latin1=Enc, _Str, CL, _I) ->
    L = iolist_to_chars(L0, F, CL),
    string(L, limit_field(F, CL), Adj, P, Pad, Enc);
control_limited($s, [L0], F, Adj, P, Pad, unicode=Enc, _Str, CL, _I) ->
    L = cdata_to_chars(L0, F, CL),
    uniconv(string(L, limit_field(F, CL), Adj, P, Pad, Enc));
control_limited($w, [A], F, Adj, P, Pad, Enc, _Str, CL, _I) ->
    Chars = io_lib:write(A, [{depth, -1}, {encoding, Enc}, {chars_limit, CL}]),
    term(Chars, F, Adj, P, Pad);
control_limited($p, [A], F, Adj, P, Pad, Enc, Str, CL, I) ->
    print(A, -1, F, Adj, P, Pad, Enc, Str, CL, I);
control_limited($W, [A,Depth], F, Adj, P, Pad, Enc, _Str, CL, _I)
           when is_integer(Depth) ->
    Chars = io_lib:write(A, [{depth, Depth}, {encoding, Enc}, {chars_limit, CL}]),
    term(Chars, F, Adj, P, Pad);
control_limited($P, [A,Depth], F, Adj, P, Pad, Enc, Str, CL, I)
           when is_integer(Depth) ->
    print(A, Depth, F, Adj, P, Pad, Enc, Str, CL, I).

-ifdef(UNICODE_AS_BINARIES).
uniconv(C) ->
    unicode:characters_to_binary(C,unicode).
-else.
uniconv(C) ->
    C.
-endif.
%% Default integer base
base(none) ->
    10;
base(B) when is_integer(B) ->
    B.

%% term(TermList, Field, Adjust, Precision, PadChar)
%%  Output the characters in a term.
%%  Adjust the characters within the field if length less than Max padding
%%  with PadChar.

term(T, none, _Adj, none, _Pad) -> T;
term(T, none, Adj, P, Pad) -> term(T, P, Adj, P, Pad);
term(T, F, Adj, P0, Pad) ->
    L = io_lib:chars_length(T),
    P = erlang:min(L, case P0 of none -> F; _ -> min(P0, F) end),
    if
	L > P ->
	    adjust(chars($*, P), chars(Pad, F-P), Adj);
	F >= P ->
	    adjust(T, chars(Pad, F-L), Adj)
    end.

%% print(Term, Depth, Field, Adjust, Precision, PadChar, Encoding,
%%       Indentation)
%% Print a term. Field width sets maximum line length, Precision sets
%% initial indentation.

print(T, D, none, Adj, P, Pad, E, Str, ChLim, I) ->
    print(T, D, 80, Adj, P, Pad, E, Str, ChLim, I);
print(T, D, F, Adj, none, Pad, E, Str, ChLim, I) ->
    print(T, D, F, Adj, I+1, Pad, E, Str, ChLim, I);
print(T, D, F, right, P, _Pad, Enc, Str, ChLim, _I) ->
    Options = [{chars_limit, ChLim},
               {column, P},
               {line_length, F},
               {depth, D},
               {encoding, Enc},
               {strings, Str}],
    io_lib_pretty:print(T, Options).

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    term(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_e(Fl, Fd, P) ->
    signbit(Fl) ++ abs_float_e(abs(Fl), Fd, P).

abs_float_e(_Fl, {Ds,E}, P) ->
    case float_man(Ds, 1, P-1) of
	{[$0|Fs],true} -> [[$1|Fs]|float_exp(E)];
	{Fs,false} -> [Fs|float_exp(E-1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Char],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs,C} = float_man(Ds, Dc),
    {[$.|Cs],C};
float_man([D|Ds], I, Dc) ->
    case float_man(Ds, I-1, Dc) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false};
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], I, Dc) ->				%Pad with 0's
    {lists:duplicate(I, $0) ++ [$.|lists:duplicate(Dc, $0)],false}.

float_man([D|_], 0) when D >= $5 -> {[],true};
float_man([_|_], 0) -> {[],false};
float_man([D|Ds], Dc) ->
    case float_man(Ds, Dc-1) of
	{Cs,true} when D =:= $9 -> {[$0|Cs],true};
	{Cs,true} -> {[D+1|Cs],false}; 
	{Cs,false} -> {[D|Cs],false}
    end;
float_man([], Dc) -> {lists:duplicate(Dc, $0),false}.	%Pad with 0's

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e,$+|integer_to_list(E)];
float_exp(E) ->
    [$e|integer_to_list(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) ->		%Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    term(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) ->
    signbit(Fl) ++ abs_float_f(abs(Fl), Fd, P).

abs_float_f(Fl, {Ds,E}, P) when E =< 0 ->
    abs_float_f(Fl, {lists:duplicate(-E+1, $0)++Ds,1}, P);	%Prepend enough 0's
abs_float_f(_Fl, {Ds,E}, P) ->
    case float_man(Ds, E, P) of
	{Fs,true} -> "1" ++ Fs;			%Handle carry
	{Fs,false} -> Fs
    end.

%% signbit(Float) -> [$-] | []

signbit(Fl) when Fl < 0.0 -> [$-];
signbit(Fl) when Fl > 0.0 -> [];
signbit(Fl) ->
    case <<Fl/float>> of
        <<1:1,_:63>> -> [$-];
        _ -> []
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) ->
    float_data(float_to_list(Fl), []).

float_data([$e|E], Ds) ->
    {lists:reverse(Ds),list_to_integer(E)+1};
float_data([D|Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D|Ds]);
float_data([_|Cs], Ds) ->
    float_data(Cs, Ds).

%%  Returns a correctly rounded string that converts to Float when
%%  read back with list_to_float/1.
%%
%%  When abs(Float) < float(1 bsl 53) the shortest such string is
%%  returned, and otherwise the shortest such string using scientific
%%  notation is returned. That is, scientific notation is used if and
%%  only if scientific notation results in a shorter string than
%%  normal notation when abs(Float) < float(1 bsl 53), and scientific
%%  notation is used unconditionally if abs(Float) >= float(1 bsl
%%  53). See comment in insert_decimal/2 for an explanation for why
%%  float(1 bsl 53) is chosen as cutoff point.
%%
%%  The algorithm that is used to find the decimal number that is
%%  represented by the returned String is described in "Ryu: Fast
%%  Float-to-String Conversion" in Proceedings of 39th ACM SIGPLAN
%%  Conference on Programming Language Design and Implementation.
%%  https://dl.acm.org/doi/pdf/10.1145/3192366.3192369

-spec fwrite_g(float()) -> string().
fwrite_g(Float) ->
    case sign_mantissa_exponent(Float) of
        {0, 0, 0} -> "0.0";
        {1, 0, 0} -> "-0.0";
        {S, M, E} when E < 2047 ->
            {Place, Digits} = 
                case is_small_int(M, E) of
                    {int, M1, E1} ->
                        compute_shortest_int(M1, E1);
                    not_int ->
                        fwrite_g_1(M, E)
                end,
            DigitList = insert_decimal(Place, Digits, Float),
            insert_minus(S, DigitList)
    end.

-define(BIG_POW, (1 bsl 52)).
-define(DECODE_CORRECTION, 1075).

sign_mantissa_exponent(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    {S, M , BE}.

is_small_int(M, E) ->
    M2 = ?BIG_POW bor M,
    E2 = E - ?DECODE_CORRECTION,
    case E2 > 0 orelse E2 < -52 of
        true ->
            %% f = m2 * 2^e2 >= 2^53 is an integer.
            %% Ignore this case for now.
            %% or f < 1
            not_int;
        _ -> 
            %% Since 2^52 <= m2 < 2^53 and 0 <= -e2 <= 52: 1 <= f = m2 / 2^-e2 < 2^53.
            %% Test if the lower -e2 bits of the significand are 0, i.e. whether the fraction is 0.
            Mask = (1 bsl -E2) - 1,
            Fraction = M2 band Mask,
            case Fraction of
                0 ->
                %% f is an integer in the range [1, 2^53).
                %% Note: mantissa might contain trailing (decimal) 0's.
                    {int, M2 bsr -E2, 0};
                _ ->
                    not_int
            end
    end.

%%  For small integers in the range [1, 2^53), v.mantissa might contain trailing (decimal) zeros.
compute_shortest_int(M, E) when M rem 10 =:= 0 ->
    Q = M div 10,
    compute_shortest_int(Q, E + 1);
compute_shortest_int(M, E) ->
    {E, integer_to_list(M)}.

fwrite_g_1(M, E) ->
    {Mf, Ef} = decode(M, E),
    Shift = mmshift(M, E),
    Mv = 4 * Mf,
    {Q, Vm, Vr, Vp, E10} = convert_to_decimal(Ef, Mv, Shift),
    Accept = M rem 2 == 0,
    {VmIsTrailingZero, VrIsTrailingZero, Vp1} = bounds(Mv, Q, Vp, Accept, Ef, Shift),
    {D1, E1} = compute_shortest(Vm, Vr, Vp1, VmIsTrailingZero, VrIsTrailingZero, Accept),
    {E1 + E10, integer_to_list(D1)}.

decode(Mantissa, 0) ->
    {Mantissa, 1 - ?DECODE_CORRECTION - 2};
decode(Mantissa, Exponent) ->
    {Mantissa + ?BIG_POW, Exponent - ?DECODE_CORRECTION - 2}.

mmshift(0, E) when E > 1 ->
    0;
mmshift(_M, _E) ->
    1.

convert_to_decimal(E2, Mv, Shift) when E2 >= 0 ->
    Q = max(0, ((E2 * 78913) bsr 18) - 1),
    Mul = io_lib_format_ryu_table:inv_value(Q),
    K = io_lib_format_ryu_table:pow5_inv_bitcount() + pow5bits(Q) - 1,
    I = -E2 + Q + K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, I, Mul),
    {Q, Vm, Vr, Vp, Q};

convert_to_decimal(E2, Mv, Shift) when E2 < 0 ->
    Q = max(0, ((-E2 * 732923) bsr 20) - 1),
    I = -E2 - Q,
    K = pow5bits(I) - io_lib_format_ryu_table:pow5_bitcount(),
    From_file = io_lib_format_ryu_table:value(I),
    J = Q - K,
    {Vm, Vr, Vp} = mulShiftAll(Mv, Shift, J, From_file),
    E10 = E2 + Q,
    {Q, Vm, Vr, Vp, E10}.

pow5bits(E) ->
    ((E * 1217359) bsr 19) + 1.

mulShiftAll(Mv, Shift, J, Mul) ->
    A = mulShift64(Mv - 1 - Shift, Mul, J),
    B = mulShift64(Mv, Mul, J),
    C = mulShift64(Mv + 2,Mul, J),
    {A, B, C}.

mulShift64(M, Mul, J) ->
    (M * Mul) bsr J.

bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21, Mv rem 5 =:= 0 ->
    {false, multipleOfPowerOf5(Mv, Q) , Vp};
bounds(Mv, Q, Vp, true, E2, Shift) when E2 >= 0, Q =< 21 ->
    {multipleOfPowerOf5(Mv - 1 - Shift, Q), false , Vp};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 >= 0, Q =< 21 ->
    {false, false , Vp - vpmodifier(multipleOfPowerOf5(Mv + 2, Q))};
bounds(_Mv, Q, Vp, true, E2, Shift) when E2 < 0, Q =< 1 ->
    {Shift =:= 1, true, Vp};
bounds(_Mv, Q, Vp, false, E2, _Shift) when E2 < 0, Q =< 1 ->
    {false, true, Vp - 1};
bounds(Mv, Q, Vp, _Accept, E2, _Shift) when E2 < 0, Q < 63 ->
    {false, (Mv band ((1 bsl Q) -1 )) =:= 0, Vp};
bounds(_Mv, _Q, Vp, _Accept, _E2, _Shift) ->
    {false, false, Vp}.

multipleOfPowerOf5(Value, Q) ->
    pow5factor(Value) >= Q.

pow5factor(Val) ->
    pow5factor(Val div 5, 0).

pow5factor(Val, Count) when (Val rem 5) /= 0->
    Count;
pow5factor(Val, Count) ->
    pow5factor(Val div 5, Count + 1).

vpmodifier(true) ->
    1;
vpmodifier(false) ->
    0.

compute_shortest(Vm, Vr, Vp, false, false, _Accept) ->
    {Vm1, Vr1, Removed, RoundUp} =
        general_case(Vm, Vr, Vp, 0, false),
    Output = Vr1 + handle_normal_output_mod(Vr1, Vm1, RoundUp),
    {Output, Removed};
compute_shortest(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, Accept) ->
    {Vm1, Vr1, Removed, LastRemovedDigit} = 
        handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, 0, 0),
    Output = Vr1 + handle_zero_output_mod(Vr1, Vm1, Accept, VmIsTrailingZero, LastRemovedDigit),
    {Output, Removed}.

general_case(Vm, Vr, Vp, Removed, RoundUp) when (Vp div 100) =< (Vm div 100)->
    general_case_10(Vm, Vr, Vp, Removed, RoundUp);
general_case(Vm, Vr, Vp, Removed, _RU) ->
    VmD100 = Vm div 100,
    VrD100 = Vr div 100,
    VpD100 = Vp div 100,
    RoundUp = ((Vr rem 100) >= 50),
    general_case_10(VmD100, VrD100, VpD100, 2 + Removed, RoundUp).

general_case_10(Vm, Vr, Vp, Removed, RoundUp) 
    when (Vp div 10) =< (Vm div 10)->
    {Vm, Vr, Removed, RoundUp};
general_case_10(Vm, Vr, Vp, Removed, _RU) ->
    VmD10 = Vm div 10,
    VrD10 = Vr div 10,
    VpD10 = Vp div 10,
    RoundUp = ((Vr rem 10) >= 5),
    general_case_10(VmD10, VrD10, VpD10, 1 + Removed, RoundUp).

handle_normal_output_mod(Vr, Vm, RoundUp) when (Vm =:= Vr) or RoundUp ->
    1;
handle_normal_output_mod(_Vr, _Vm, _RoundUp) ->
    0.

handle_trailing_zeros(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit)
    when (Vp div 10) =< (Vm div 10)->
    vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit);
handle_trailing_zeros(Vm, Vr, Vp, VmIsTrailingZero, VrIsTrailingZero, Removed, LastRemovedDigit) ->
    VmTZ = VmIsTrailingZero and ((Vm rem 10) =:= 0),
    VrTZ = VrIsTrailingZero and (LastRemovedDigit =:= 0),
    handle_trailing_zeros(Vm div 10, Vr div 10, Vp div 10, VmTZ, VrTZ, 1 + Removed, Vr rem 10).

vmIsTrailingZero(Vm, Vr, _Vp, false = _VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    handle_50_dotdot_0(Vm, Vr, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, _Vp, _VmTZ, VrTZ, Removed, LastRemovedDigit) when (Vm rem 10) /= 0 ->
    handle_50_dotdot_0(Vm, Vr, VrTZ, Removed, LastRemovedDigit);
vmIsTrailingZero(Vm, Vr, Vp, VmTZ, VrTZ, Removed, LastRemovedDigit) ->
    vmIsTrailingZero(Vm div 10, Vr div 10, Vp div 10, VmTZ, LastRemovedDigit == 0 andalso VrTZ, 1 + Removed, Vr rem 10).

handle_50_dotdot_0(Vm, Vr, true, Removed, 5) when (Vr rem 2) =:= 0 ->
    {Vm, Vr, Removed, 4};
handle_50_dotdot_0(Vm, Vr, _VrTZ, Removed, LastRemovedDigit) ->
    {Vm, Vr, Removed, LastRemovedDigit}.

handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, LastRemovedDigit) when LastRemovedDigit >= 5 ->
    1;
handle_zero_output_mod(Vr, Vm, Accept, VmTZ, _LastRemovedDigit) when Vr =:= Vm, ((not Accept) or (not VmTZ)) ->
    1;
handle_zero_output_mod(_Vr, _Vm, _Accept, _VmTZ, _LastRemovedDigit) ->
    0.

insert_decimal(Place, S, Float) ->
    L = length(S),
    Exp = Place + L - 1,
    ExpL = integer_to_list(Exp),
    ExpCost = length(ExpL) + 2,
    if
        Place < 0 ->
            if
                Exp >= 0 ->
                    {S0, S1} = lists:split(L + Place, S),
                    S0 ++ "." ++ S1;
                2 - Place - L =< ExpCost ->
                    "0." ++ lists:duplicate(-Place - L, $0) ++ S;
                true ->
                    insert_exp(ExpL, S)
            end;
        true ->
            Dot = if L =:= 1 -> 1; true -> 0 end,
            if
                %% All integers in the range [-2^53, 2^53] can
                %% be stored without loss of precision in an
                %% IEEE 754 64-bit double but 2^53+1 cannot be
                %% stored in an IEEE 754 64-bit double without
                %% loss of precision (float((1 bsl 53)+1) =:=
                %% float(1 bsl 53)). It thus makes sense to
                %% show floats that are >= 2^53 or <= -2^53 in
                %% scientific notation to indicate that the
                %% number is so large that there could be loss
                %% in precion when adding or subtracting 1.
                %%
                %% https://stackoverflow.com/questions/1848700/biggest-integer-that-can-be-stored-in-a-double?answertab=votes#tab-top
                ExpCost + Dot >= Place + 2 andalso abs(Float) < float(1 bsl 53) ->
                    S ++ lists:duplicate(Place, $0) ++ ".0";
                true ->
                    insert_exp(ExpL, S)
            end
    end.


insert_exp(ExpL, [C]) ->
    [C] ++ ".0e" ++ ExpL;
insert_exp(ExpL, [C | S]) ->
    [C] ++ "." ++ S ++ "e" ++ ExpL.

insert_minus(0, Digits) ->
    Digits;
insert_minus(1, Digits) ->
    [$-] ++ Digits.

%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4, 
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E = if A < 1.0e-1 -> -2;
	   A < 1.0e0  -> -1;
	   A < 1.0e1  -> 0;
	   A < 1.0e2  -> 1;
	   A < 1.0e3  -> 2;
	   A < 1.0e4  -> 3;
	   true       -> fwrite_f
	end,
    if  P =< 1, E =:= -1;
	P-1 > E, E >= -1 ->
	    fwrite_f(Fl, F, Adj, P-1-E, Pad);
	P =< 1 ->
	    fwrite_e(Fl, F, Adj, 2, Pad);
	true ->
	    fwrite_e(Fl, F, Adj, P, Pad)
    end.


iolist_to_chars(Cs, F, CharsLimit) when CharsLimit < 0; CharsLimit >= F ->
    iolist_to_chars(Cs);
iolist_to_chars(Cs, _, CharsLimit) ->
    limit_iolist_to_chars(Cs, sub(CharsLimit, 3), [], normal). % three dots

iolist_to_chars([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    [C | iolist_to_chars(Cs)];
iolist_to_chars([I|Cs]) ->
    [iolist_to_chars(I) | iolist_to_chars(Cs)];
iolist_to_chars([]) ->
    [];
iolist_to_chars(B) when is_binary(B) ->
    binary_to_list(B).

limit_iolist_to_chars(Cs, 0, S, normal) ->
    L = limit_iolist_to_chars(Cs, 4, S, final),
    case iolist_size(L) of
        N when N < 4 -> L;
        4 -> "..."
    end;
limit_iolist_to_chars(_Cs, 0, _S, final) -> [];
limit_iolist_to_chars([C|Cs], Limit, S, Mode) when C >= $\000, C =< $\377 ->
    [C | limit_iolist_to_chars(Cs, Limit - 1, S, Mode)];
limit_iolist_to_chars([I|Cs], Limit, S, Mode) ->
    limit_iolist_to_chars(I, Limit, [Cs|S], Mode);
limit_iolist_to_chars([], _Limit, [], _Mode) ->
    [];
limit_iolist_to_chars([], Limit, [Cs|S], Mode) ->
    limit_iolist_to_chars(Cs, Limit, S, Mode);
limit_iolist_to_chars(B, Limit, S, Mode) when is_binary(B) ->
    case byte_size(B) of
        Sz when Sz > Limit ->
            {B1, B2} = split_binary(B, Limit),
            [binary_to_list(B1) | limit_iolist_to_chars(B2, 0, S, Mode)];
        Sz ->
            [binary_to_list(B) | limit_iolist_to_chars([], Limit-Sz, S, Mode)]
    end.

cdata_to_chars(Cs, F, CharsLimit) when CharsLimit < 0; CharsLimit >= F ->
    cdata_to_chars(Cs);
cdata_to_chars(Cs, _, CharsLimit) ->
    limit_cdata_to_chars(Cs, sub(CharsLimit, 3), normal). % three dots

cdata_to_chars([C|Cs]) when is_integer(C), C >= $\000 ->
    [C | cdata_to_chars(Cs)];
cdata_to_chars([I|Cs]) ->
    [cdata_to_chars(I) | cdata_to_chars(Cs)];
cdata_to_chars([]) ->
    [];
cdata_to_chars(B) when is_binary(B) ->
    case catch unicode:characters_to_list(B) of
        L when is_list(L) -> L;
        _ -> binary_to_list(B)
    end.

limit_cdata_to_chars(Cs, 0, normal) ->
    L = limit_cdata_to_chars(Cs, 4, final),
    case string:length(L) of
        N when N < 4 -> L;
        4 -> "..."
    end;
limit_cdata_to_chars(_Cs, 0, final) -> [];
limit_cdata_to_chars(Cs, Limit, Mode) ->
    case string:next_grapheme(Cs) of
        {error, <<C,Cs1/binary>>} ->
            %% This is how ~ts handles Latin1 binaries with option
            %% chars_limit.
            [C | limit_cdata_to_chars(Cs1, Limit - 1, Mode)];
        {error, [C|Cs1]} -> % not all versions of module string return this
            [C | limit_cdata_to_chars(Cs1, Limit - 1, Mode)];
        [] ->
            [];
        [GC|Cs1] ->
            [GC | limit_cdata_to_chars(Cs1, Limit - 1, Mode)]
    end.

limit_field(F, CharsLimit) when CharsLimit < 0; F =:= none ->
    F;
limit_field(F, CharsLimit) ->
    max(3, min(F, CharsLimit)).

%% string(String, Field, Adjust, Precision, PadChar)

string(S, none, _Adj, none, _Pad, _Enc) -> S;
string(S, F, Adj, none, Pad, Enc) ->
    string_field(S, F, Adj, io_lib:chars_length(S), Pad, Enc);
string(S, none, _Adj, P, Pad, Enc) ->
    string_field(S, P, left, io_lib:chars_length(S), Pad, Enc);
string(S, F, Adj, P, Pad, Enc) when F >= P ->
    N = io_lib:chars_length(S),
    if F > P ->
	    if N > P ->
		    adjust(flat_trunc(S, P, Enc), chars(Pad, F-P), Adj);
	       N < P ->
		    adjust([S|chars(Pad, P-N)], chars(Pad, F-P), Adj);
	       true -> % N == P
		    adjust(S, chars(Pad, F-P), Adj)
	    end;
       true -> % F == P
	    string_field(S, F, Adj, N, Pad, Enc)
    end.

string_field(S, F, _Adj, N, _Pad, Enc) when N > F ->
    flat_trunc(S, F, Enc);
string_field(S, F, Adj, N, Pad, _Enc) when N < F ->
    adjust(S, chars(Pad, F-N), Adj);
string_field(S, _, _, _, _, _) -> % N == F
    S.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase)
%% -> [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
	    S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
	    term([$-|S], F, Adj, none, Pad);
       true ->
	    S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
	    term(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase)
%% -> [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
	    S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
	    term([$-,Prefix|S], F, Adj, none, Pad);
       true ->
	    S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
	    term([Prefix|S], F, Adj, none, Pad)
    end.

%% char(Char, Field, Adjust, Precision, PadChar) -> chars().

char(C, none, _Adj, none, _Pad) -> [C];
char(C, F, _Adj, none, _Pad) -> chars(C, F);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(F, right, _P, _Pad) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data|Pad];
adjust(Data, Pad, right) -> [Pad|Data].

%% Flatten and truncate a deep list to at most N elements.

flat_trunc(List, N, latin1) when is_integer(N), N >= 0 ->
    {S, _} = lists:split(N, lists:flatten(List)),
    S;
flat_trunc(List, N, unicode) when is_integer(N), N >= 0 ->
    string:slice(List, 0, N).

%% A deep version of lists:duplicate/2

chars(_C, 0) ->
    [];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C,C];
chars(C, 3) ->
    [C,C,C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C,S|S].

%chars(C, N, Tail) ->
%    [chars(C, N)|Tail].

%% Lowercase conversion

cond_lowercase(String, true) ->
    lowercase(String);
cond_lowercase(String,false) ->
    String.

lowercase([H|T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H-$A+$a)|lowercase(T)];
lowercase([H|T]) ->
    [H|lowercase(T)];
lowercase([]) ->
    [].

%% Make sure T does change sign.
sub(T, _) when T < 0 -> T;
sub(T, E) when T >= E -> T - E;
sub(_, _) -> 0.

get_option(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
	false -> Default;
	{Key, Value} -> Value;
	_ -> Default
    end.
