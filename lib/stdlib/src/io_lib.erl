%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-moduledoc """
I/O library functions.

This module contains functions for converting to and from strings (lists of
characters). They are used for implementing the functions in the `m:io` module.
There is no guarantee that the character lists returned from some of the
functions are flat, they can be deep lists. Function `lists:flatten/1` can be
used for flattening deep lists.
""".

-compile(nowarn_deprecated_catch).

-export([fwrite/2,fwrite/3,fread/2,fread/3,format/2,format/3]).
-export([bfwrite/2, bfwrite/3, bformat/2, bformat/3]).
-export([scan_format/2,unscan_format/1,build_text/1,build_text/2]).
-export([print/1,print/4,indentation/2]).

-export([write/1,write/2,write/3,write/5,bwrite/2]).
-export([nl/0,format_prompt/1,format_prompt/2]).
-export([write_binary/3]).
-export([write_atom/1,write_string/1,write_string/2,write_latin1_string/1,
         write_latin1_string/2, write_char/1, write_latin1_char/1,
         bwrite_string/3
        ]).

-export([write_atom_as_latin1/1, write_string_as_latin1/1,
         write_string_as_latin1/2, write_char_as_latin1/1]).

-export([quote_atom/2, char_list/1, latin1_char_list/1,
	 deep_char_list/1, deep_latin1_char_list/1,
	 printable_list/1, printable_latin1_list/1, printable_unicode_list/1]).

%% Utilities for collecting characters mostly used by group
-export([collect_chars/1, collect_chars/3, collect_chars/4, collect_chars_eager/4,
	 collect_line/1, collect_line/3, collect_line/4, collect_line_no_eol/4,
	 get_until/1, get_until/3, get_until/4]).

%% The following functions were used by Yecc's include-file.
-export([write_unicode_string/1, write_unicode_char/1,
         deep_unicode_char_list/1]).

-export([limit_term/2]).

-export([chars_length/1]).

-export([write_bin/5, write_string_bin/3, write_binary_bin/4]).

-export_type([chars/0, latin1_string/0, continuation/0,
              fread_error/0, fread_item/0, format_spec/0, chars_limit/0]).

-dialyzer([{nowarn_function,
            [string_bin_escape_unicode/6,
             string_bin_escape_latin1/6]},
           no_improper_lists]).

%%----------------------------------------------------------------------

-doc "An possibly deep list containing only `t:char/0`s.".
-type chars() :: [char() | chars()].
-type latin1_string() :: [unicode:latin1_char()].
-type depth() :: -1 | non_neg_integer().

-doc "A continuation as returned by `fread/3`.".
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

-doc """
A map describing the contents of a format string.

- `control_char` is the type of control sequence: `$P`, `$w`, and so on.
- `args` is a list of the arguments used by the control sequence, or an empty
  list if the control sequence does not take any arguments.
- `width` is the field width.
- `adjust` is the adjustment.
- `precision` is the precision of the printed argument.
- `pad_char` is the padding character.
- `encoding` is set to `true` if translation modifier `t` is present.
- `strings` is set to `false` if modifier `l` is present.
- `maps_order` is set to `undefined` by default, `ordered` if modifier `k` is
  present, or `reversed` or `CmpFun` if modifier `K` is present.
""".
-type format_spec() ::
        #{
           control_char := char(),
           args         := [any()],
           width        := 'none' | integer(),
           adjust       := 'left' | 'right',
           precision    := 'none' | integer(),
           pad_char     := char(),
           encoding     := 'unicode' | 'latin1',
           strings      := boolean(),
           % `maps_order` has been added since OTP26 and is optional
           maps_order   => maps:iterator_order()
         }.

%%----------------------------------------------------------------------

%% Interface calls to sub-modules.

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format`.

For a detailed description of the available formatting options, see
[`io:fwrite/1,2,3`](`io:fwrite/1`). If the format string or argument list
contains an error, a fault is generated.

If and only if the Unicode translation modifier is used in the format string
(that is, `~ts` or `~tc`), the resulting list can contain characters beyond the
ISO Latin-1 character range (that is, numbers > 255). If so, the result is still
an ordinary Erlang `t:string/0`, and can well be used in any context where
Unicode data is allowed.
""".
-spec fwrite(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

fwrite(Format, Args) ->
    format(Format, Args).

-type chars_limit() :: integer().

-doc """
Returns a character list that represents `Data` formatted in accordance with
`Format` in the same way as `fwrite/2` and `format/2`, but takes an extra
argument, a list of options.

Valid option:

- **`{chars_limit, CharsLimit}`** - A soft limit on the number of characters
  returned. When the number of characters is reached, remaining structures are
  replaced by "`...`". `CharsLimit` defaults to -1, which means no limit on the
  number of characters returned.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec fwrite(Format, Data, Options) -> chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

fwrite(Format, Args, Options) ->
    format(Format, Args, Options).


-doc(#{equiv => bfwrite(Format, Data, [])}).
-doc(#{since => <<"OTP 28.0">>}).
-spec bfwrite(Format, Data) -> unicode:unicode_binary() when
      Format :: io:format(),
      Data :: [term()].

bfwrite(F, A) ->
    bformat(F, A, []).

-doc("""
   Binary variant of `fwrite/3`

   Returns a UTF-8 encoded binary string.
   """).
-doc(#{since => <<"OTP 28.0">>}).
-spec bfwrite(Format, Data, Options) -> unicode:unicode_binary() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

bfwrite(F, A, Opts) ->
    bformat(F, A, Opts).

-doc """
Tries to read `String` in accordance with the control sequences in `Format`.

For a detailed description of the available formatting options, see `io:fread/3`.
It is assumed that `String` contains whole lines.

The function returns:

- **`{ok, InputList, LeftOverChars}`** - The string was read. `InputList` is the
  list of successfully matched and read items, and `LeftOverChars` are the input
  characters not used.

- **`{more, RestFormat, Nchars, InputStack}`** - The string was read, but more
  input is needed to complete the original format string. `RestFormat` is the
  remaining format string, `Nchars` is the number of characters scanned, and
  `InputStack` is the reversed list of inputs matched up to that point.

- **`{error, What}`** - The read operation failed and parameter `What` gives a
  hint about the error.

_Example:_

```erlang
3> io_lib:fread("~f~f~f", "15.6 17.3e-6 24.5").
{ok,[15.6,1.73e-5,24.5],[]}
```
""".

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

-doc """
This is the re-entrant formatted reader. The continuation of the first call to
the functions must be `[]`.

For a complete description of how the re-entrant input scheme works,
see Armstrong, Virding, Williams: 'Concurrent Programming in
Erlang', Chapter 13.

The function returns:

- **`{done, Result, LeftOverChars}`** - The input is complete. The result is one
  of the following:

  - **`{ok, InputList}`** - The string was read. `InputList` is the list of
    successfully matched and read items, and `LeftOverChars` are the remaining
    characters.

  - **`eof`** - End of file was encountered. `LeftOverChars` are the input
    characters not used.

  - **`{error, What}`** - An error occurred and parameter `What` gives a hint
    about the error.

- **`{more, Continuation}`** - More data is required to build a term.
  `Continuation` must be passed to [`fread/3`](`fread/3`) when more data becomes
  available.
""".
-spec fread(Continuation, CharSpec, Format) -> Return when
      Continuation :: continuation() | [],
      CharSpec :: string() | 'eof',
      Format :: string(),
      Return :: {'more', Continuation1 :: continuation()}
              | {'done', Result, LeftOverChars :: string()},
      Result :: {'ok', InputList :: [fread_item()]}
              | 'eof'
              | {'error', {'fread', What :: fread_error()}}.

fread(Cont, Chars, Format) ->
    io_lib_fread:fread(Cont, Chars, Format).

-doc(#{equiv => fwrite(Format, Data)}).
-spec format(Format, Data) -> chars() when
      Format :: io:format(),
      Data :: [term()].

format(Format, Args) ->
    try io_lib_format:fwrite(Format, Args)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc(#{equiv => fwrite(Format, Data, Options)}).
-doc(#{since => <<"OTP 21.0">>}).
-spec format(Format, Data, Options) -> chars() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

format(Format, Args, Options) ->
    try io_lib_format:fwrite(Format, Args, Options)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.


-doc(#{equiv => bfwrite(Format, Data, [])}).
-doc(#{since => <<"OTP 28.0">>}).
-spec bformat(Format, Data) -> unicode:unicode_binary() when
      Format :: io:format(),
      Data :: [term()].

bformat(Format, Args) ->
    try io_lib_format:fwrite_bin(Format, Args)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc(#{equiv => bfwrite(Format, Data, Options)}).
-doc(#{since => <<"OTP 28.0">>}).
-spec bformat(Format, Data, Options) -> unicode:unicode_binary() when
      Format :: io:format(),
      Data :: [term()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

bformat(Format, Args, Options) ->
    try io_lib_format:fwrite_bin(Format, Args, Options)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc """
Returns a list corresponding to the specified format string, where control
sequences have been replaced with corresponding tuples. This list can be passed
to:

- `build_text/1` to have the same effect as [`format(Format, Args)`](`format/2`)
- `unscan_format/1` to get the corresponding pair of `Format` and `Args` (with
  every `*` and corresponding argument expanded to numeric values)

A typical use of this function is to replace unbounded-size control sequences
like `~w` and `~p` with the depth-limited variants `~W` and `~P` before
formatting to text in, for example, a logger.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec scan_format(Format, Data) -> FormatList when
      Format :: io:format(),
      Data :: [term()],
      FormatList :: [char() | format_spec()].

scan_format(Format, Args) ->
    try io_lib_format:scan(Format, Args)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [Format, Args])
    end.

-doc "For details, see `scan_format/2`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec unscan_format(FormatList) -> {Format, Data} when
      FormatList :: [char() | format_spec()],
      Format :: io:format(),
      Data :: [term()].

unscan_format(FormatList) ->
    io_lib_format:unscan(FormatList).

-doc "For details, see `scan_format/2`.".
-doc(#{since => <<"OTP 18.0">>}).
-spec build_text(FormatList) -> chars() when
      FormatList :: [char() | format_spec()].

build_text(FormatList) ->
    try io_lib_format:build(FormatList)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [FormatList])
    end.

-doc false.
-spec build_text(FormatList, Options) -> chars() when
      FormatList :: [char() | format_spec()],
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit},
      CharsLimit :: chars_limit().

build_text(FormatList, Options) ->
    try io_lib_format:build(FormatList, Options)
    catch
        C:R:S ->
            test_modules_loaded(C, R, S),
            erlang:error(badarg, [FormatList, Options])
    end.

%% Failure to load a module must not be labeled as badarg.
%% C, R, and S are included so that the original error, which could be
%% a bug in io_lib_format, can be found by tracing on
%% test_modules_loaded/3.
test_modules_loaded(_C, _R, _S) ->
    Modules = [io_lib_format, io_lib_pretty, string, unicode],
    case code:ensure_modules_loaded(Modules) of
        ok -> ok;
        Error -> erlang:error(Error)
    end.

-doc(#{equiv => print(Term, 1, 80, -1)}).
-spec print(Term) -> chars() when
      Term :: term().

print(Term) ->
    io_lib_pretty:print(Term).

-doc """
Returns a list of characters that represents `Term`, but breaks representations
longer than one line into many lines and indents each line sensibly.

Also tries to detect and output lists of printable characters as strings.

- `Column` is the starting column; defaults to 1.
- `LineLength` is the maximum line length; defaults to 80.
- `Depth` is the maximum print depth; defaults to -1, which means no limitation.
""".
-spec print(Term, Column, LineLength, Depth) -> chars() when
      Term :: term(),
      Column :: non_neg_integer(),
      LineLength :: non_neg_integer(),
      Depth :: depth().

print(Term, Column, LineLength, Depth) ->
    io_lib_pretty:print(Term, Column, LineLength, Depth).

-doc "Returns the indentation if `String` has been printed, starting at `StartIndent`.".
-spec indentation(String, StartIndent) -> integer() when
      String :: string(),
      StartIndent :: integer().

indentation(Chars, Current) ->
    io_lib_format:indentation(Chars, Current).


%% Format an IO-request prompt (handles formatting errors safely).
%% Atoms, binaries, and iolists (or unicode:charlist()) can be used
%% as-is, and will be printed without any additional quotes.

-doc false.
-spec format_prompt(term()) -> chars().

format_prompt(Prompt) ->
    format_prompt(Prompt, latin1).

-doc false.
-spec format_prompt(term(), atom()) -> chars().

format_prompt({format,Format,Args}, _Encoding) ->
    do_format_prompt(Format, Args);
format_prompt(Prompt, Encoding)
  when is_list(Prompt); is_atom(Prompt); is_binary(Prompt) ->
    do_format_prompt(add_modifier(Encoding, "s"), [Prompt]);
format_prompt(Prompt, Encoding) ->
    do_format_prompt(add_modifier(Encoding, "p"), [Prompt]).

do_format_prompt(Format, Args) ->
    case catch format(Format, Args) of
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

-doc(#{equiv => write(Term, -1)}).
-spec write(Term) -> chars() when
      Term :: term().

write(Term) ->
    write1(Term, -1, latin1, undefined).

-doc false.
-spec write(term(), depth(), boolean()) -> chars().

write(Term, D, true) ->
    io_lib_pretty:print(Term, 1, 80, D);
write(Term, D, false) ->
    write(Term, D).

-doc """
write(Term, DepthOrOptions)

Returns a character list that represents `Term`. Option `Depth` controls the
depth of the structures written.

When the specified depth is reached, everything below this level is replaced by
"`...`".

`Depth` defaults to -1, which means no limitation. Option `CharsLimit` puts a
soft limit on the number of characters returned. When the number of characters is
reached, remaining structures are replaced by "`...`". `CharsLimit` defaults to -1,
which means no limit on the number of characters returned.

_Example:_

```erlang
1> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9})).
"{1,[2],[3],[4,5],6,7,8,9}"
2> lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9}, 5)).
"{1,[2],[3],[...],...}"
3> lists:flatten(io_lib:write({[1,2,3],[4,5],6,7,8,9}, [{chars_limit,20}])).
"{[1,2|...],[4|...],...}"
```
""".
-spec write(Term, Depth) -> chars() when
      Term :: term(),
      Depth :: depth();
           (Term, Options) -> chars() when
      Term :: term(),
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit}
              | {'depth', Depth}
              | {'encoding', 'latin1' | 'utf8' | 'unicode'}
              | {'maps_order', maps:iterator_order()},
      CharsLimit :: chars_limit(),
      Depth :: depth().

write(Term, Options) when is_list(Options) ->
    Depth = get_option(depth, Options, -1),
    Encoding = get_option(encoding, Options, epp:default_encoding()),
    CharsLimit = get_option(chars_limit, Options, -1),
    MapsOrder = get_option(maps_order, Options, undefined),
    write(Term, Depth, Encoding, MapsOrder, CharsLimit);
write(Term, Depth) ->
    write(Term, [{depth, Depth}, {encoding, latin1}]).

-doc false.
write(Term, Depth, Encoding, MapsOrder, CharsLimit) ->
    if
        Depth =:= 0; CharsLimit =:= 0 ->
            "...";
        is_integer(CharsLimit), CharsLimit < 0, is_integer(Depth) ->
            write1(Term, Depth, Encoding, MapsOrder);
        is_integer(CharsLimit), CharsLimit > 0 ->
            RecDefFun = fun(_, _) -> no end,
            If = io_lib_pretty:intermediate
                 (Term, Depth, CharsLimit, RecDefFun, Encoding, _Str=false, MapsOrder),
            io_lib_pretty:write(If)
    end.

-doc """
Behaves as `write/2` but returns a UTF-8 encoded binary string.
""".
-doc(#{since => <<"OTP 28.0">>}).
-spec bwrite(Term, Options) -> unicode:unicode_binary() when
      Term :: term(),
      Options :: [Option],
      Option :: {'chars_limit', CharsLimit}
              | {'depth', Depth}
              | {'encoding', 'latin1' | 'utf8' | 'unicode'}
              | {'maps_order', maps:iterator_order()},
      CharsLimit :: chars_limit(),
      Depth :: depth().

bwrite(Term, Options) when is_list(Options) ->
    Depth = get_option(depth, Options, -1),
    %% InEncoding = How should we display atoms
    InEncoding = get_option(encoding, Options, epp:default_encoding()),
    CharsLimit = get_option(chars_limit, Options, -1),
    MapsOrder = get_option(maps_order, Options, undefined),
    {S, _Sz} = write_bin(Term, Depth, InEncoding, MapsOrder, CharsLimit),
    S.

-doc false.
-spec write_bin(Term, Depth, InEncoding, MapsOrder, CharsLimit) ->
          {unicode:unicode_binary(), Sz::integer()} when
      Term :: term(),
      Depth :: depth(),
      InEncoding :: 'latin1' | 'utf8' | 'unicode',
      MapsOrder :: maps:iterator_order() | undefined,
      CharsLimit :: chars_limit().

write_bin(Term, Depth, InEncoding, MapsOrder, CharsLimit) ->
    if
        Depth =:= 0; CharsLimit =:= 0 ->
            {<<"...">>, 3};
        is_integer(CharsLimit), CharsLimit < 0, is_integer(Depth) ->
            write_bin1(Term, Depth, InEncoding, MapsOrder, 0, <<>>);
        is_integer(CharsLimit), CharsLimit > 0 ->
            RecDefFun = fun(_, _) -> no end,
            If = io_lib_pretty:intermediate(Term, Depth, CharsLimit, RecDefFun,
                                            {InEncoding, utf8}, _Str=false, MapsOrder),
            {_, Len, _, _} = If,
            Bin = io_lib_pretty:write(If, {unicode,utf8}),
            {Bin, Len}
    end.

write_bin1(_Term, 0, _Enc, _O, Sz, Acc) ->
    {<<Acc/binary, "...">>, Sz+3};
write_bin1(Term, _D, _Enc, _O, Sz, Acc) when is_integer(Term) ->
    Int = integer_to_binary(Term),
    {<<Acc/binary, Int/binary>>, byte_size(Int) + Sz};
write_bin1(Term, _D, _Enc, _O, Sz, Acc) when is_float(Term) ->
    Float = float_to_binary(Term, [short]),
    {<<Acc/binary, Float/binary>>, byte_size(Float)+Sz};
write_bin1(Atom, _D, latin1, _O, Sz, Acc) when is_atom(Atom) ->
    Str = unicode:characters_to_binary(write_atom_as_latin1(Atom)),
    {<<Acc/binary, Str/binary>>, byte_size(Str)+Sz};
write_bin1(Atom, _D, _Enc, _O, Sz, Acc) when is_atom(Atom) ->
    Str = write_atom(Atom),
    {<<Acc/binary, (unicode:characters_to_binary(Str))/binary>>, length(Str)+Sz};
write_bin1(Term, _D, _Enc, _O, Sz, Acc) when is_port(Term) ->
    Str = (list_to_binary(erlang:port_to_list(Term))),
    {<<Acc/binary, Str/binary>>, byte_size(Str)+Sz};
write_bin1(Term, _D, _Enc, _O, Sz, Acc) when is_pid(Term) ->
    Str = (list_to_binary(pid_to_list(Term))),
    {<<Acc/binary, Str/binary>>, byte_size(Str)+Sz};
write_bin1(Term, _D, _Enc, _O, Sz, Acc) when is_reference(Term) ->
    Str = (list_to_binary(erlang:ref_to_list(Term))),
    {<<Acc/binary, Str/binary>>, byte_size(Str)+Sz};
write_bin1(<<_/bitstring>>=Term, D, _Enc, _O, Sz, Acc) ->
    write_binary_bin0(Term, D, Sz, Acc);
write_bin1([], _D, _Enc, _O, Sz, Acc) ->
    {<<Acc/binary, "[]">>, Sz+2};
write_bin1({}, _D, _Enc, _O, Sz, Acc) ->
    {<<Acc/binary, "{}">>, Sz+2};
write_bin1(F, _D, _Enc, _O, Sz, Acc) when is_function(F) ->
    Str = (list_to_binary(erlang:fun_to_list(F))),
    {<<Acc/binary, Str/binary>>, byte_size(Str) + Sz};
write_bin1([H|T], D, Enc, O, Sz, Acc) ->
    if
	D =:= 1 -> {<<Acc/binary, "[...]">>, Sz+5};
	true ->
            {Head, Sz1} = write_bin1(H, D-1, Enc, O, Sz+1, <<Acc/binary, $[>>),
            write_tail_bin(T, D-1, Enc, O, Sz1, Head)
    end;
write_bin1(T, D, Enc, O, Sz, Acc) when is_tuple(T) ->
    if
	D =:= 1 -> {<<Acc/binary, "{...}">>, Sz+5};
	true ->
            {First, Sz1} = write_bin1(element(1, T), D-1, Enc, O, Sz+1, <<Acc/binary, ${>>),
            write_tuple_bin(T, 2, D-1, Enc, O, Sz1, First)
    end;
write_bin1(Term, 1, _Enc, _O, Sz, Acc) when is_map(Term) ->
    {<<Acc/binary, "#{}">>, Sz+3};
write_bin1(Map, D, Enc, O, Sz, Acc) when is_map(Map), is_integer(D) ->
    I = maps:iterator(Map, O),
    case maps:next(I) of
        {K, V, NextI} ->
            D0 = D - 1,
            {Start,Sz1} = write_map_assoc_bin(K, V, D0, Enc, O, Sz+2, <<Acc/binary, $#, ${>>),
            write_map_body_bin(NextI, D0, D0, Enc, O, Sz1, Start);
        none ->
            {~"#{}", 3}
    end.

write_tail_bin([], _D, _Enc, _O, Sz, Acc) -> {<<Acc/binary, $]>>, Sz+1};
write_tail_bin(_, 1, _Enc, _O, Sz, Acc) -> {<<Acc/binary, "|...]">>, Sz+1};
write_tail_bin([H|T], D, Enc, O, Sz, Acc) ->
    {Head, Sz1} = write_bin1(H, D-1, Enc, O, Sz+1, <<Acc/binary,$,>>),
    write_tail_bin(T, D-1, Enc, O, Sz1, Head);
write_tail_bin(Other, D, Enc, O, Sz, Acc) ->
    {Tail, Sz1} = write_bin1(Other, D-1, Enc, O, Sz+1, <<Acc/binary, $|>>),
    {<<Tail/binary, $]>>, Sz1+1}.

write_tuple_bin(T, I, _D, _Enc, _O, Sz, Acc) when I > tuple_size(T) ->
    {<<Acc/binary, $}>>, Sz+1};
write_tuple_bin(_, _I, 1, _Enc, _O, Sz, Acc) ->
    {<<Acc/binary, ",...}">>, Sz+4};
write_tuple_bin(T, I, D, Enc, O, Sz, Acc) ->
    {Elem, Sz1} = write_bin1(element(I, T), D-1, Enc, O, Sz+1, <<Acc/binary, $,>>),
    write_tuple_bin(T, I+1, D-1, Enc, O, Sz1, Elem).

write_map_body_bin(_, 1, _D0, _Enc, _O, Sz, Acc) ->
    {<<Acc/binary, ",...}">>, Sz+5};
write_map_body_bin(I, D, D0, Enc, O, Sz, Acc) ->
    case maps:next(I) of
        {K, V, NextI} ->
            {W, Sz1} = write_map_assoc_bin(K, V, D0, Enc, O, Sz+1, <<Acc/binary, $,>>),
            write_map_body_bin(NextI, D - 1, D0, Enc, O, Sz1, W);
        none ->
            {<<Acc/binary, "}">>, Sz+1}
    end.
write_map_assoc_bin(K, V, D, Enc, O, Sz, Acc) ->
    {KBin, Sz1} = write_bin1(K, D, Enc, O, Sz, Acc),
    write_bin1(V, D, Enc, O, Sz1 + 4, <<KBin/binary, " => ">>).

write_binary_bin0(B, D, Sz, Acc) ->
    {S, _} = write_binary_bin(B, D, -1, Acc),
    {S, byte_size(S)+Sz}.

-doc false.
-spec write_binary_bin(Bin, Depth, T, Acc) -> {unicode:unicode_binary(), binary()} when
      Bin :: binary(),
      Depth :: integer(),
      T :: integer(),
      Acc :: unicode:unicode_binary().

write_binary_bin(B, D, T, Acc) when is_integer(T) ->
    write_binary_body_bin(B, D, tsub(T, 4), <<Acc/binary, "<<" >>).

write_binary_body_bin(<<>> = B, _D, _T, Acc) ->
    {<<Acc/binary, ">>" >>, B};
write_binary_body_bin(<<_/bitstring>>=B, D, T, Acc) when D =:= 1; T =:= 0->
    {<<Acc/binary, "...>>">>, B};
write_binary_body_bin(<<X:8>>, _D, _T, Acc) ->
    {<<Acc/binary, (integer_to_binary(X))/binary, ">>">>, <<>>};
write_binary_body_bin(<<X:8,Rest/bitstring>>, D, T, Acc) ->
    IntBin = integer_to_binary(X),
    write_binary_body_bin(Rest, D-1, tsub(T, byte_size(IntBin) + 1),
                          <<Acc/binary, IntBin/binary, $,>>);
write_binary_body_bin(B, _D, _T, Acc) ->
    L = bit_size(B),
    <<X:L>> = B,
    {<<Acc/binary, (integer_to_binary(X))/binary, $:,
       (integer_to_binary(L))/binary,">>">>,
     <<>>}.


write1(_Term, 0, _E, _O) -> "...";
write1(Term, _D, _E, _O) when is_integer(Term) -> integer_to_list(Term);
write1(Term, _D, _E, _O) when is_float(Term) -> io_lib_format:fwrite_g(Term);
write1(Atom, _D, latin1, _O) when is_atom(Atom) -> write_atom_as_latin1(Atom);
write1(Atom, _D, _E, _O) when is_atom(Atom) -> write_atom(Atom);
write1(Term, _D, _E, _O) when is_port(Term) -> erlang:port_to_list(Term);
write1(Term, _D, _E, _O) when is_pid(Term) -> pid_to_list(Term);
write1(Term, _D, _E, _O) when is_reference(Term) -> erlang:ref_to_list(Term);
write1(<<_/bitstring>>=Term, D, _E, _O) -> write_binary(Term, D);
write1([], _D, _E, _O) -> "[]";
write1({}, _D, _E, _O) -> "{}";
write1(F, _D, _E, _O) when is_function(F) -> erlang:fun_to_list(F);
write1([H|T], D, E, O) ->
    if
	D =:= 1 -> "[...]";
	true ->
	    [$[,[write1(H, D-1, E, O)|write_tail(T, D-1, E, O)],$]]
    end;
write1(Term, D, E, O) when is_map(Term) ->
    write_map(Term, D, E, O);
write1(T, D, E, O) when is_tuple(T) ->
    if
	D =:= 1 -> "{...}";
	true ->
	    [${,
	     [write1(element(1, T), D-1, E, O)|write_tuple(T, 2, D-1, E, O)],
	     $}]
    end.

%% write_tail(List, Depth, Encoding)
%%  Test the terminating case first as this looks better with depth.

write_tail([], _D, _E, _O) -> "";
write_tail(_, 1, _E, _O) -> [$| | "..."];
write_tail([H|T], D, E, O) ->
    [$,,write1(H, D-1, E, O)|write_tail(T, D-1, E, O)];
write_tail(Other, D, E, O) ->
    [$|,write1(Other, D-1, E, O)].

write_tuple(T, I, _D, _E, _O) when I > tuple_size(T) -> "";
write_tuple(_, _I, 1, _E, _O) -> [$, | "..."];
write_tuple(T, I, D, E, O) ->
    [$,,write1(element(I, T), D-1, E, O)|write_tuple(T, I+1, D-1, E, O)].

write_map(_, 1, _E, _O) -> "#{}";
write_map(Map, D, E, O) when is_integer(D) ->
    I = maps:iterator(Map, O),
    case maps:next(I) of
        {K, V, NextI} ->
            D0 = D - 1,
            W = write_map_assoc(K, V, D0, E, O),
            [$#,${,[W | write_map_body(NextI, D0, D0, E, O)],$}];
        none -> "#{}"
    end.

write_map_body(_, 1, _D0, _E, _O) -> ",...";
write_map_body(I, D, D0, E, O) ->
    case maps:next(I) of
        {K, V, NextI} ->
            W = write_map_assoc(K, V, D0, E, O),
            [$,,W|write_map_body(NextI, D - 1, D0, E, O)];
        none -> ""
    end.

write_map_assoc(K, V, D, E, O) ->
    [write1(K, D, E, O)," => ",write1(V, D, E, O)].

write_binary(B, D) when is_integer(D) ->
    {S, _} = write_binary(B, D, -1),
    S.

-doc false.
write_binary(B, D, T) when is_integer(T) ->
    {S, Rest} = write_binary_body(B, D, tsub(T, 4), []),
    {[$<,$<,lists:reverse(S),$>,$>], Rest}.

write_binary_body(<<>> = B, _D, _T, Acc) ->
    {Acc, B};
write_binary_body(B, D, T, Acc) when D =:= 1; T =:= 0->
    {["..."|Acc], B};
write_binary_body(<<X:8>>, _D, _T, Acc) ->
    {[integer_to_list(X)|Acc], <<>>};
write_binary_body(<<X:8,Rest/bitstring>>, D, T, Acc) ->
    S = integer_to_list(X),
    write_binary_body(Rest, D-1, tsub(T, length(S) + 1), [$,,S|Acc]);
write_binary_body(B, _D, _T, Acc) ->
    L = bit_size(B),
    <<X:L>> = B,
    {[integer_to_list(L),$:,integer_to_list(X)|Acc], <<>>}.

%% Make sure T does not change sign.
tsub(T, _) when T < 0 -> T;
tsub(T, E) when T >= E -> T - E;
tsub(_, _) -> 0.

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

-doc "Returns the list of characters needed to print atom `Atom`.".
-spec write_atom(Atom) -> chars() when
      Atom :: atom().

write_atom(Atom) ->
    write_possibly_quoted_atom(Atom, fun write_string/2).

-doc """
Returns the list of characters needed to print atom `Atom`. Non-Latin-1
characters are escaped.
""".
-doc(#{since => <<"OTP 20.0">>}).
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

-doc false.
-spec quote_atom(atom(), chars()) -> boolean().

quote_atom(Atom, Cs0) ->
    case erl_scan:reserved_word(Atom) of
	true -> true;
	false ->
	    case Cs0 of
		[C|Cs] when is_integer(C), C >= $a, C =< $z ->
		    not name_chars(Cs);
		[C|Cs] when is_integer(C), C >= $ß, C =< $ÿ, C =/= $÷ ->
		    not name_chars(Cs);
		[C|_] when is_integer(C) -> true;
                [] -> true
	    end
    end.

name_chars([C|Cs]) when is_integer(C) ->
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

-doc "Returns the list of characters needed to print `String` as a string.".
-spec write_string(String) -> chars() when
      String :: string().

write_string(S) ->
    write_string(S, $").   %"

-doc false.
-spec write_string(string(), char()) -> chars().

write_string(S, Q) ->
    [Q|write_string1(unicode_as_unicode, S, Q)].

-doc "Returns the UTF-8 encoded binary `String` surrounded by `Qoute`.".
-doc(#{since => <<"OTP 28.0">>}).
-spec bwrite_string(String, Qoute, InEnc) -> unicode:unicode_binary() when
      String :: string() | binary(),
      Qoute  :: integer() | [],
      InEnc  :: 'unicode' | 'latin1'.  %% In case of binary input

bwrite_string(S, Q, InEnc) ->
    {Bin, _Sz} = write_string_bin(S, Q, InEnc),
    Bin.

-doc false.
-spec write_string_bin(String, Qoute, InEnc) -> {unicode:unicode_binary(), Sz::integer()} when
      String :: string() | binary(),
      Qoute  :: integer() | [],
      InEnc  :: 'unicode' | 'latin1'.  %% In case of binary input

write_string_bin(S, Q, _InEnc) when is_list(S) ->
    Escaped = write_string(S,Q),
    Sz = chars_length(Escaped),
    Bin = unicode:characters_to_binary(Escaped),
    true = is_binary(Bin),
    {Bin, Sz};
write_string_bin(S, Q, latin1) when is_binary(S) ->
    Escaped = string_bin_escape_latin1(S, S, Q, [], 0, 0),
    Sz = iolist_size(Escaped) + if Q == [] -> 0; true -> 2 end,
    Bin = unicode:characters_to_binary([Q,Escaped,Q], latin1, utf8),
    true = is_binary(Bin),
    {Bin, Sz};
write_string_bin(S, Q, unicode) when is_binary(S) ->
    Escaped = string_bin_escape_unicode(S, S, Q, [], 0, 0),
    Bin = case Q of
              [] when is_binary(Escaped) -> Escaped;
              _ -> unicode:characters_to_binary([Q,Escaped,Q])
          end,
    {Bin, string:length(Bin)}.

string_bin_escape_latin1(<<Byte, Rest/binary>>, Orig, Q, Acc, Skip0, Len) ->
    case needs_escape(Byte, Q) of
        false ->
            string_bin_escape_latin1(Rest, Orig, Q, Acc, Skip0, Len+1);
        Escape when Len =:= 0 ->
            Skip = Skip0 + Len + 1,
            string_bin_escape_latin1(Rest, Orig, Q, [Acc | Escape], Skip, 0);
        Escape ->
            Skip = Skip0 + Len + 1,
            Part = binary_part(Orig, Skip0, Len),
            string_bin_escape_latin1(Rest, Orig, Q, [Acc, Part | Escape], Skip, 0)
    end;
string_bin_escape_latin1(_, Orig, _, _Acc, 0, _) ->
    Orig;
string_bin_escape_latin1(_, Orig, _, Acc, Skip, Len) ->
    case Len =:= 0 of
        true -> Acc;
        false -> [Acc | binary_part(Orig, Skip, Len)]
    end.

string_bin_escape_unicode(<<Byte, Rest/binary>>, Orig, Q, Acc, Skip0, Len) when Byte > 127 ->
    string_bin_escape_unicode(Rest, Orig, Q, Acc, Skip0, Len+1);
string_bin_escape_unicode(<<Byte, Rest/binary>>, Orig, Q, Acc, Skip0, Len) ->
    case needs_escape(Byte, Q) of
        false ->
            string_bin_escape_unicode(Rest, Orig, Q, Acc, Skip0, Len+1);
        Escape when Len =:= 0 ->
            Skip = Skip0 + Len + 1,
            string_bin_escape_unicode(Rest, Orig, Q, [Acc | Escape], Skip, 0);
        Escape ->
            Skip = Skip0 + Len + 1,
            Part = binary_part(Orig, Skip0, Len),
            string_bin_escape_unicode(Rest, Orig, Q, [Acc, Part | Escape], Skip, 0)
    end;
string_bin_escape_unicode(_, Orig, _, _Acc, 0, _) ->
    Orig;
string_bin_escape_unicode(_, Orig, _, Acc, Skip, Len) ->
    case Len =:= 0 of
        true -> Acc;
        false -> [Acc | binary_part(Orig, Skip, Len)]
    end.

needs_escape(Q, Q)   -> [$\\, Q];
needs_escape($\\, _) -> [$\\, $\\];
needs_escape(C, _) when C >= $\s,   C =< $~ ->
    false;
needs_escape($\n, _) -> [$\\, $n];
needs_escape($\r, _) -> [$\\, $r];
needs_escape($\t, _) -> [$\\, $t];
needs_escape($\v, _) -> [$\\, $v];
needs_escape($\b, _) -> [$\\, $b];
needs_escape($\f, _) -> [$\\, $f];
needs_escape($\e, _) -> [$\\, $e];
needs_escape($\d, _) -> [$\\, $d];
needs_escape(C, _) when C >= $\240, C =< $\377 ->
    false;
needs_escape(C, _) when C < $\240 ->
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3].

%% Backwards compatibility.
-doc false.
write_unicode_string(S) ->
    write_string(S).

-doc "Returns the list of characters needed to print `Latin1String` as a string.".
-doc(#{since => <<"OTP R16B">>}).
-spec write_latin1_string(Latin1String) -> latin1_string() when
      Latin1String :: latin1_string().

write_latin1_string(S) ->
    write_latin1_string(S, $").   %"

-doc false.
-spec write_latin1_string(latin1_string(), char()) -> latin1_string().

write_latin1_string(S, Q) ->
    [Q|write_string1(latin1, S, Q)].

-doc """
Returns the list of characters needed to print `String` as a string. Non-Latin-1
characters are escaped.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_string_as_latin1(String) -> latin1_string() when
      String :: string().

write_string_as_latin1(S) ->
    write_string_as_latin1(S, $").   %"

-doc false.
-spec write_string_as_latin1(string(), char()) -> latin1_string().

write_string_as_latin1(S, Q) ->
    [Q|write_string1(unicode_as_latin1, S, Q)].

write_string1(_,[], Q) ->
    [Q];
write_string1(Enc,[C|Cs], Q) when is_integer(C) ->
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

-doc """
Returns the list of characters needed to print a character constant in the
Unicode character set.
""".
-spec write_char(Char) -> chars() when
      Char :: char().

write_char($\s) -> "$\\s";			%Must special case this.
write_char(C) when is_integer(C), C >= $\000 ->
    [$$|string_char(unicode_as_unicode, C, -1, [])].

%% Backwards compatibility.
-doc false.
write_unicode_char(C) ->
    write_char(C).

-doc """
Returns the list of characters needed to print a character constant in the ISO
Latin-1 character set.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_latin1_char(Latin1Char) -> latin1_string() when
      Latin1Char :: unicode:latin1_char().

write_latin1_char(Lat1) when is_integer(Lat1), Lat1 >= $\000, Lat1 =< $\377  ->
    [$$|string_char(latin1, Lat1, -1, [])].

-doc """
Returns the list of characters needed to print a character constant in the
Unicode character set. Non-Latin-1 characters are escaped.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec write_char_as_latin1(Char) -> latin1_string() when
      Char :: char().

write_char_as_latin1(Uni) when is_integer(Uni), Uni >= $\000 ->
    [$$|string_char(unicode_as_latin1,Uni, -1, [])].

%% latin1_char_list(CharList)
%% deep_latin1_char_list(CharList)
%%  Return true if CharList is a (possibly deep) list of Latin-1
%%  characters, else false.

-doc """
Returns `true` if `Term` is a flat list of characters in the ISO Latin-1 range,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec latin1_char_list(Term) -> boolean() when
      Term :: term().

latin1_char_list([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    latin1_char_list(Cs);
latin1_char_list([]) -> true;
latin1_char_list(_) -> false.			%Everything else is false

-doc """
Returns `true` if `Term` is a flat list of characters in the Unicode range,
otherwise `false`.
""".
-spec char_list(Term) -> boolean() when
      Term :: term().

char_list([C|Cs]) when is_integer(C), C >= 0, C < 16#D800;
       is_integer(C), C > 16#DFFF, C < 16#FFFE;
       is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    char_list(Cs);
char_list([]) -> true;
char_list(_) -> false.			%Everything else is false

-doc """
Returns `true` if `Term` is a, possibly deep, list of characters in the ISO
Latin-1 range, otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
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

-doc """
Returns `true` if `Term` is a, possibly deep, list of characters in the Unicode
range, otherwise `false`.
""".
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

-doc false.
deep_unicode_char_list(Term) ->
    deep_char_list(Term).

%% printable_latin1_list([Char]) -> boolean()
%%  Return true if CharList is a list of printable Latin1 characters, else
%%  false.

-doc """
Returns `true` if `Term` is a flat list of printable ISO Latin-1 characters,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
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

-doc """
Returns `true` if `Term` is a flat list of printable characters, otherwise
`false`.

What is a printable character in this case is determined by startup flag `+pc`
to the Erlang VM; see `io:printable_range/0` and
[`erl(1)`](`e:erts:erl_cmd.md`).
""".
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

-doc """
Returns `true` if `Term` is a flat list of printable Unicode characters,
otherwise `false`.
""".
-doc(#{since => <<"OTP R16B">>}).
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

-doc "Returns a character list that represents a new line character.".
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
-doc false.
collect_chars(Tag, Data, N) ->
    collect_chars(Tag, Data, latin1, N).

%% Now we are aware of encoding...    
-doc false.
collect_chars(start, Data, unicode, N) when is_binary(Data), is_integer(N) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,<<>>}
    end;
collect_chars(start, Data, latin1, N) when is_binary(Data), is_integer(N) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,<<>>}
    end;
collect_chars(start,Data,_,N) when is_list(Data), is_integer(N) ->
    collect_chars_list([], N, Data);
collect_chars(start, eof, _,_) ->
    {stop,eof,eof};
collect_chars({binary,[<<>>],_N}, eof, _,_) ->
    {stop,eof,eof};
collect_chars({binary,Stack,_N}, eof, _,_) ->
    {stop,binrev(Stack),eof};
collect_chars({binary,Stack,N}, Data,unicode, _) when is_integer(N) ->
    {Size,Npos} = count_and_find_utf8(Data,N),
    if Size > N ->
	    {B1,B2} = split_binary(Data, Npos),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),<<>>}
    end;
collect_chars({binary,Stack,N}, Data,latin1, _) when is_integer(N) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),<<>>}
    end;
collect_chars({list,Stack,N}, Data, _,_) when is_integer(N) ->
    collect_chars_list(Stack, N, Data);

%% collect_chars(Continuation, MoreChars, Count)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}

collect_chars([], Chars, _, N) when is_integer(N) ->
    collect_chars1(N, Chars, []);
collect_chars({Left,Sofar}, Chars, _, _N) when is_integer(Left) ->
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
collect_chars_list([], _N, eof) ->
    {stop,eof,eof};
collect_chars_list(Stack, _N, eof) ->
    {stop,lists:reverse(Stack, []),eof};
collect_chars_list(Stack, N, []) ->
    {list,Stack,N};
collect_chars_list(Stack,N, [H|T]) ->
    collect_chars_list([H|Stack], N-1, T).

%% Fetch the number of remaining bytes
-doc false.
collect_chars({_, _, N}) ->
    N.

%% A special collect_chars that never returns more_chars,
%% instead it eagerly stops collecting if it has received
%% any characters at all.
-doc false.
collect_chars_eager(State, Chars, Encoding, N) ->
    case collect_chars(State, Chars, Encoding, N) of
        {list, Stack, _N} when Stack =/= [] ->
            {stop, lists:reverse(Stack), []};
        {binary, Stack, _N} when Stack =/= [<<>>] ->
            {stop, binrev(Stack), []};
        Else ->
            Else
    end.

%% collect_line(State, Data, _). New in R9C.
%%  Returns:
%%	{stop,Result,RestData}
%%	NewState
%%% BC (with pre-R13).
-doc false.
collect_line(Tag, Data, Any) -> 
    collect_line(Tag, Data, latin1, Any).

%% A special variant of collect line that trims the last newline
%% used by io:get_password/0,1
-doc false.
collect_line_no_eol(Tag, Data, Encoding, Any) ->
    case collect_line(Tag, Data, Encoding, Any) of
        {stop, Line, Rest} when Line =/= eof ->
            {stop, string:trim(Line), Rest};
        Else -> Else
    end.

%% Now we are aware of encoding...    
-doc false.
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

%% Return the number of remaing bytes, 0 for unknown.
-doc false.
collect_line(_State) ->
    0.

%% Translator function to emulate a new (R9C and later) 
%% I/O client when you have an old one.
%%
%% Implements a middleman that is get_until server and get_chars client.

%%% BC (with pre-R13).
-doc false.
get_until(Any,Data,Arg) ->
    get_until(Any,Data,latin1,Arg).

%% Now we are aware of encoding...    
-doc false.
get_until(start, Data, Encoding, XtraArg) ->
    %% We use the type of the initial data as an indicator of what
    %% the final result should be cast to. We cannot use the final
    %% data as that might be eof and then we have no idea what to
    %% convert to.
    get_until({is_binary(Data), []}, Data, Encoding, XtraArg);
get_until({IsDataBinary, Cont}, Data, Encoding, {Mod, Func, XtraArgs}) ->
    Chars = if is_binary(Data), Encoding =:= unicode ->
		    unicode:characters_to_list(Data,utf8);
	       is_binary(Data) ->
		    binary_to_list(Data);
	       true ->
		    Data
	    end,
    case apply(Mod, Func, [Cont,Chars|XtraArgs]) of
	{done,Result,Buf} ->
	    {stop,if IsDataBinary, 
		     is_list(Result), 
		     Encoding =:= unicode ->
			  unicode:characters_to_binary(Result,unicode,unicode);
		     IsDataBinary, 
		     is_list(Result) ->
			  erlang:iolist_to_binary(Result);
%%		     IsDataBinary,
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
	    {IsDataBinary, NewCont}
    end.

binrev(L) ->
    list_to_binary(lists:reverse(L, [])).

binrev(L, T) ->
    list_to_binary(lists:reverse(L, T)).

%% Return the number of remaing bytes, 0 for unknown.
-doc false.
get_until(_State) ->
    0.

-doc false.
-spec limit_term(term(), depth()) -> term().

%% The intention is to mimic the depth limitation of io_lib:write()
%% and io_lib_pretty:print(). The leaves ('...') should never be
%% seen when printed with the same depth. Bitstrings are never
%% truncated, which is OK as long as they are not sent to other nodes.
limit_term(Term, Depth) when is_integer(Depth), Depth >= -1 ->
    try test_limit(Term, Depth) of
        ok -> Term
    catch
        throw:limit ->
            limit(Term, Depth)
    end.

limit(_, 0) -> '...';
limit([H|T]=L, D) ->
    if
	D =:= 1 -> ['...'];
	true ->
            case printable_list(L) of
                true -> L;
                false ->
                    [limit(H, D-1)|limit_tail(T, D-1)]
            end
    end;
limit(Term, D) when is_map(Term) ->
    limit_map(Term, D);
limit({}=T, _D) -> T;
limit(T, D) when is_tuple(T) ->
    if
	D =:= 1 -> {'...'};
	true ->
            list_to_tuple([limit(element(1, T), D-1)|
                           limit_tuple(T, 2, D-1)])
    end;
limit(<<_/bitstring>>=Term, D) -> limit_bitstring(Term, D);
limit(Term, _D) -> Term.

limit_tail([], _D) -> [];
limit_tail(_, 1) -> ['...'];
limit_tail([H|T], D) ->
    [limit(H, D-1)|limit_tail(T, D-1)];
limit_tail(Other, D) ->
    limit(Other, D-1).

limit_tuple(T, I, _D) when I > tuple_size(T) -> [];
limit_tuple(_, _I, 1) -> ['...'];
limit_tuple(T, I, D) ->
    [limit(element(I, T), D-1)|limit_tuple(T, I+1, D-1)].

%% Cannot limit maps properly since there is no guarantee that
%% maps:from_list() creates a map with the same internal ordering of
%% the selected associations as in Map. Instead of subtracting one
%% from the depth as the map associations are traversed (as is done
%% for tuples and lists), the same depth is applied to each and every
%% (returned) association.
limit_map(Map, D) ->
    %% Keep one extra association to make sure the final ',...' is included.
    limit_map_body(maps:iterator(Map), D + 1, D, []).

limit_map_body(_I, 0, _D0, Acc) ->
    maps:from_list(Acc);
limit_map_body(I, D, D0, Acc) ->
    case maps:next(I) of
        {K, V, NextI} ->
            limit_map_body(NextI, D-1, D0, [limit_map_assoc(K, V, D0) | Acc]);
        none ->
            maps:from_list(Acc)
    end.

limit_map_assoc(K, V, D) ->
    %% Keep keys as are to avoid creating duplicated keys.
    {K, limit(V, D - 1)}.

limit_bitstring(B, _D) -> B. % Keeps all printable binaries.

test_limit(_, 0) -> throw(limit);
test_limit([H|T]=L, D) when is_integer(D) ->
    if
	D =:= 1 -> throw(limit);
	true ->
            case printable_list(L) of
                true -> ok;
                false ->
                    test_limit(H, D-1),
                    test_limit_tail(T, D-1)
            end
    end;
test_limit(Term, D) when is_map(Term) ->
    test_limit_map(Term, D);
test_limit({}, _D) -> ok;
test_limit(T, D) when is_tuple(T) ->
    test_limit_tuple(T, 1, tuple_size(T), D);
test_limit(<<_/bitstring>>=Term, D) -> test_limit_bitstring(Term, D);
test_limit(_Term, _D) -> ok.

test_limit_tail([], _D) -> ok;
test_limit_tail(_, 1) -> throw(limit);
test_limit_tail([H|T], D) ->
    test_limit(H, D-1),
    test_limit_tail(T, D-1);
test_limit_tail(Other, D) ->
    test_limit(Other, D-1).

test_limit_tuple(_T, I, Sz, _D) when I > Sz -> ok;
test_limit_tuple(_, _, _, 1) -> throw(limit);
test_limit_tuple(T, I, Sz, D) ->
    test_limit(element(I, T), D-1),
    test_limit_tuple(T, I+1, Sz, D-1).

test_limit_map(Map, D) ->
    test_limit_map_body(maps:iterator(Map), D).

test_limit_map_body(_I, 0) -> throw(limit); % cannot happen
test_limit_map_body(I, D) ->
    case maps:next(I) of
        {K, V, NextI} ->
            test_limit_map_assoc(K, V, D),
            test_limit_map_body(NextI, D-1);
        none ->
            ok
    end.

test_limit_map_assoc(K, V, D) ->
    test_limit(K, D - 1),
    test_limit(V, D - 1).

test_limit_bitstring(_, _) -> ok.

-doc false.
-spec chars_length(chars()) -> non_neg_integer().
%% Optimized for deep lists S such that deep_latin1_char_list(S) is
%% true. No binaries allowed! It is assumed that $\r is never followed
%% by $\n if S is an iolist() (string:length() assigns such a
%% sub-sequence length 1).
chars_length(S) ->
    try
        %% true = deep_latin1_char_list(S),
        iolist_size(S)
    catch
        _:_ ->
            string:length(S)
    end.
