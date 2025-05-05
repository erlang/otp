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
-module(io).
-moduledoc """
Standard I/O server interface functions.

This module provides an interface to standard Erlang I/O servers. The output
functions all return `ok` if they are successful, or exit if they are not.

All functions in this module have an optional parameter
[`IoDevice`](`t:device/0`). If included, it must be the pid of a process that
handles the I/O protocols. Normally, it is an [`IoDevice`](`t:device/0`) returned by
`file:open/2`. If no [`IoDevice`](`t:device/0`) is given,
[`standard_io`](`t:standard_io/0`) is used.

For a description of the I/O protocols, see section
[The Erlang I/O Protocol](io_protocol.md) in the User's Guide.

> #### Warning {: .warning }
>
> The data supplied to function `put_chars/2` is to be in
> the `t:unicode:chardata/0` format. This means that programs supplying binaries
> to this function must convert them to UTF-8 before trying to output the data
> on an I/O device.
>
> If an I/O device is set in binary mode, functions
> [`get_chars/2,3`](`get_chars/2`) and [`get_line/1,2`](`get_line/1`) can return
> binaries instead of lists. The binaries are encoded in UTF-8.
>
> To work with binaries in ISO Latin-1 encoding, use the `m:file` module
> instead.
>
> For conversion functions between character encodings, see the `m:unicode`
> module.

## Error Information

The `ErrorInfo` mentioned in this module is the standard `ErrorInfo` structure
that is returned from all I/O modules. It has the following format:

```erlang
{ErrorLocation, Module, ErrorDescriptor}
```

A string that describes the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```
""".

-compile(nowarn_deprecated_catch).

-export([put_chars/1,put_chars/2,nl/0,nl/1,
	 get_chars/2,get_chars/3,get_line/1,get_line/2,
	 get_password/0, get_password/1,
	 setopts/1, setopts/2, getopts/0, getopts/1]).
-export([write/1,write/2,read/1,read/2,read/3,read/4]).
-export([columns/0,columns/1,rows/0,rows/1]).
-export([fwrite/1,fwrite/2,fwrite/3,fread/2,fread/3,
	 format/1,format/2,format/3]).
-export([scan_erl_exprs/1,scan_erl_exprs/2,scan_erl_exprs/3,scan_erl_exprs/4,
	 scan_erl_form/1,scan_erl_form/2,scan_erl_form/3,scan_erl_form/4,
	 parse_erl_exprs/1,parse_erl_exprs/2,parse_erl_exprs/3,
         parse_erl_exprs/4,parse_erl_form/1,parse_erl_form/2,
         parse_erl_form/3,parse_erl_form/4]).
-export([request/1,request/2,requests/1,requests/2]).
%% Implemented in native code
-export([printable_range/0]).

-export_type([device/0, format/0, server_no_data/0,
              standard_io/0, standard_error/0, user/0]).

%%-------------------------------------------------------------------------

-doc """
The default standard I/O device assigned to a process. This device is used when
no `IoDevice` argument is specified in the function calls in this module.

It is sometimes desirable to use an explicit `IoDevice` argument that
refers to the default I/O device. This is the case with functions that can
access either a file or the default I/O device. The atom `standard_io` has this
special meaning. The following example illustrates this:

```erlang
27> io:read('enter>').
enter>foo.
{ok,foo}
28> io:read(standard_io, 'enter>').
enter>bar.
{ok,bar}
```

By default all I/O sent to `standard_io` will end up in the [`user`](`t:user/0`)
I/O device of the node that spawned the calling process.

`standard_io` is an alias for [`group_leader/0`](`erlang:group_leader/0`), so in
order to change where the default input/output requests are sent you can change
the group leader of the current process using
[`group_leader(NewGroupLeader, self())`](`erlang:group_leader/2`).
""".
-type standard_io() :: standard_io.
-doc """
The I/O device `standard_error` can be used to direct output to whatever the
current operating system considers a suitable I/O device for error output. This
can be useful when standard output is redirected.

Example on a Unix-like operating system:

```text
$ erl -noinput -eval 'io:format(standard_error,"Error: ~s~n",["error 11"]),'\
'init:stop().' > /dev/null
Error: error 11
```
""".
-type standard_error() :: standard_error.
-doc """
An I/O device that can be used to interact with the node local `stdout` and
`stdin`. This can be either a terminal, a pipe, a file, or a combination.

Use `getopts/1` to get more information about the I/O device.

See [The Interactive Shell](unicode_usage.md#the-interactive-shell) and
[Escripts and non-interactive I/O](unicode_usage.md#escripts-and-non-interactive-i-o)
in the Using Unicode In Erlang User's Guide for details on how Unicode is
handled by `user`.
""".
-type user() :: user.
-doc """
An I/O device, either `t:standard_io/0`, `t:standard_error/0`, `t:user/0`, a `t:file:io_server/0`,
a registered name, or any pid handling I/O protocols.
""".
-type device() :: atom() | pid() | file:io_server() | standard_io() | standard_error() | user().
-type prompt() :: atom() | unicode:chardata().

%% ErrorDescription is whatever the I/O-server sends.
-doc "What the I/O server sends when there is no data.".
-type server_no_data() :: {'error', ErrorDescription :: term()} | 'eof'.

%%-------------------------------------------------------------------------
%% Needs to be inlined for error_info to be correct
-compile({inline,[o_request/2]}).
o_request(Function, OrigArgs) ->
    {Io, Request} =
        if
            Function =:= format; Function =:= fwrite ->
                case OrigArgs of
                    [Format] ->
                        {default_output(), {format, Format, []}};
                    [Format, Args] ->
                        {default_output(), {format, Format, Args}};
                    [D, Format, Args] ->
                        {D, {format, Format, Args}}
                end;
            Function =:= put_chars ->
                case OrigArgs of
                    [Chars] ->
                        {default_output(), {put_chars, unicode, Chars}};
                    [D, Chars] ->
                        {D, {put_chars, unicode, Chars}};
                    [D, Encoding, Chars] ->
                        {D, {put_chars, Encoding, Chars}}
                end;
            Function =:= nl ->
                case OrigArgs of
                    [] ->
                        {default_output(), nl};
                    [D] ->
                        {D, nl}
                end;
            Function =:= write ->
                case OrigArgs of
                    [Term] ->
                        {default_output(), {write, Term}};
                    [D, Term] ->
                        {D, {write, Term}}
                end
        end,
    ErrorRef = make_ref(),
    case request(Io, Request, ErrorRef) of
        {ErrorRef, Reason} ->
            %% We differentiate between errors that are created by this module
            erlang:error(conv_reason(Reason), OrigArgs,
                         [{error_info,#{cause => {?MODULE, Reason},
                                        module => erl_stdlib_errors}}]);
        {error, Reason} ->
            %% and the errors we get from the Device
            erlang:error(conv_reason(Reason), OrigArgs,
                         [{error_info,#{cause => {device, Reason},
                                        module => erl_stdlib_errors}}]);
        Other ->
            Other
    end.

%%
%% User interface.
%%

%% Request what the user considers printable characters
-doc """
Returns the user-requested range of printable Unicode characters.

The user can request a range of characters that are to be considered printable
in heuristic detection of strings by the shell and by the formatting functions.
This is done by supplying `+pc <range>` when starting Erlang.

The only valid values for `<range>` are `latin1` and `unicode`. `latin1` means
that only code points < 256 (except control characters, and so on) are
considered printable. `unicode` means that all printable characters in all
Unicode character ranges are considered printable by the I/O functions.

By default, Erlang is started so that only the `latin1` range of characters
indicate that a list of integers is a string.

The simplest way to use the setting is to call `io_lib:printable_list/1`, which
uses the return value of this function to decide if a list is a string of
printable characters.

> #### Note {: .info }
>
> In a future release, this function may return more values and ranges. To avoid
> compatibility problems, it is recommended to use function
> `io_lib:printable_list/1`.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec printable_range() -> 'unicode' | 'latin1'.
printable_range() ->
    erlang:nif_error(undefined).

%% Put chars takes mixed *unicode* list from R13 onwards.
-doc(#{equiv => put_chars(standard_io, CharData)}).
-spec put_chars(CharData) -> 'ok' when
      CharData :: unicode:chardata().

put_chars(Chars) ->
    o_request(?FUNCTION_NAME, [Chars]).

-doc """
Writes the characters of `CharData` to the [`IoDevice`](`t:device/0`).

If you want to write latin1 encoded bytes to the [`IoDevice`](`t:device/0`) you should use
`file:write/2` instead.
""".
-spec put_chars(IoDevice, CharData) -> 'ok' when
      IoDevice :: device(),
      CharData :: unicode:chardata().

put_chars(Io, Chars) ->
    o_request(?FUNCTION_NAME, [Io, Chars]).

-doc(#{equiv => nl(standard_io)}).
-spec nl() -> 'ok'.

nl() ->
    o_request(?FUNCTION_NAME, []).

-doc "Writes new line to the standard output (`IoDevice`).".
-spec nl(IoDevice) -> 'ok' when
      IoDevice :: device().

nl(Io) ->
    o_request(?FUNCTION_NAME, [Io]).

-doc(#{equiv => columns(standard_io)}).
-spec columns() -> {'ok', pos_integer()} | {'error', 'enotsup'}.

columns() ->
    columns(default_output()).

-doc """
Retrieves the number of columns of the [`IoDevice`](`t:device/0`) (that is, the width of a
terminal).

The function succeeds for terminal devices and returns `{error, enotsup}` for
all other I/O devices.
""".
-spec columns(IoDevice) -> {'ok', pos_integer()} | {'error', 'enotsup'} when
      IoDevice :: device().

columns(Io) ->
    case request(Io, {get_geometry,columns}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.

-doc(#{equiv => rows(standard_io)}).
-spec rows() -> {'ok', pos_integer()} | {'error', 'enotsup'}.

rows() ->
    rows(default_output()).

-doc """
Retrieves the number of rows of [`IoDevice`](`t:device/0`) (that is, the height of a terminal).

The function only succeeds for terminal devices, for all other I/O devices the
function returns `{error, enotsup}`.
""".
-spec rows(IoDevice) -> {'ok', pos_integer()} | {'error', 'enotsup'} when
      IoDevice :: device().

rows(Io) ->
    case request(Io,{get_geometry,rows}) of
	N  when is_integer(N), N > 0 ->
	    {ok,N};
	_ ->
	    {error,enotsup}
    end.

-doc(#{equiv => get_chars(standard_io, Prompt, Count)}).
-spec get_chars(Prompt, Count) -> Data | server_no_data() when
      Prompt :: prompt(),
      Count :: non_neg_integer(),
      Data :: string() | unicode:unicode_binary().

get_chars(Prompt, N) ->
    get_chars(default_input(), Prompt, N).

-doc """
Reads `Count` characters from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

The function returns:

- **`Data`** - The input characters. If the I/O device supports Unicode, the
  data can represent codepoints > 255 (the `latin1` range). If the I/O server is
  set to deliver binaries, they are encoded in UTF-8 (regardless of whether the
  I/O device supports Unicode). If you want the data to be returned as a latin1
  encoded binary you should use `file:read/2` instead.

- **`eof`** - End of file was encountered.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.
""".
-spec get_chars(IoDevice, Prompt, Count) -> Data | server_no_data() when
      IoDevice :: device(),
      Prompt :: prompt(),
      Count :: non_neg_integer(),
      Data :: string() | unicode:unicode_binary().

get_chars(Io, Prompt, N) when is_integer(N), N >= 0 ->
    request(Io, {get_chars,unicode,Prompt,N}).

-doc(#{equiv => get_line(standard_io, Prompt)}).
-spec get_line(Prompt) -> Data | server_no_data() when
      Prompt :: prompt(),
      Data :: string() | unicode:unicode_binary().

get_line(Prompt) ->
    get_line(default_input(), Prompt).

-doc """
Reads a line from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

The function returns:

- **`Data`** - The characters in the line terminated by a line feed (or end of
  file). If the I/O device supports Unicode, the data can represent codepoints >
  255 (the `latin1` range). If the I/O server is set to deliver binaries, they
  are encoded in UTF-8 (regardless of if the I/O device supports Unicode). If
  you want the data to be returned as a latin1 encoded binary you should use
  `file:read_line/1` instead.

- **`eof`** - End of file was encountered.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.
""".
-spec get_line(IoDevice, Prompt) -> Data | server_no_data() when
      IoDevice :: device(),
      Prompt :: prompt(),
      Data :: string() | unicode:unicode_binary().

get_line(Io, Prompt) ->
    request(Io, {get_line,unicode,Prompt}).

-doc """
Reads a password from `t:user/0`. Works just as `get_line/2` except that
the typed characters are not printed to the terminal.

In order for this function to work, the `m:shell` must be in `{noshell, raw}`
mode. See `shell:start_interactive/1` for details on what that means.

*Example*:

```
#!/usr/bin/env escript
%%! -noshell

main(_) ->
    ok = shell:start_interactive({noshell, raw}),
    try
        io:get_password()
    after
        shell:start_interactive({noshell, cooked})
    end.
```
""".
-doc #{ since => ~"OTP 28.0" }.
-spec get_password() -> Data | server_no_data() when
    Data :: string() | unicode:unicode_binary().
get_password() ->
    get_password(user).

-doc false.
get_password(Io) ->
    request(Io, {get_password,unicode}).

-type encoding()   :: 'latin1' | 'unicode' | 'utf8' | 'utf16' | 'utf32'
                    | {'utf16', 'big' | 'little'} | {'utf32','big' | 'little'}.
-type expand_fun() :: fun((string()) -> {'yes'|'no', string(), list()}).
-type option()   :: {'binary', boolean()}
                    | {'echo', boolean()}
                    | {'expand_fun', expand_fun()}
                    | {'encoding', encoding()}
                    | {atom(), term()}.
-type getopt() :: {'terminal' | 'stdin' | 'stdout' | 'stderr', boolean()} | option().

-doc(#{equiv => getopts(standard_io)}).
-spec getopts() -> [getopt()] | {'error', Reason} when
      Reason :: term().

getopts() ->
    getopts(default_input()).

-doc """
Requests all available options and their current values for a [`IoDevice`](`t:device/0`).

For example:

```erlang
1> {ok,F} = file:open("/dev/null",[read]).
{ok,<0.42.0>}
2> io:getopts(F).
[{binary,false},{encoding,latin1}]
```

Here the file I/O server returns all available options for a file, which are the
expected ones, `encoding` and `binary`. However, the standard shell has some
more options:

```erlang
3> io:getopts().
[{expand_fun,#Fun<group.0.120017273>},
 {echo,true},
 {binary,false},
 {encoding,unicode},
 {terminal,true},
 {stdout,true},
 {stderr,true},
 {stdin,true}]
```

This example is, as can be seen, run in an environment where the terminal
supports Unicode input and output.

The `stdin`, `stdout` and `stderr` options are read only and indicates
whether the stream is a terminal or not. When it is a terminal, most systems that
Erlang runs on allows the use of [ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code)
to control what the terminal inputs or outputs.

`terminal` is an alias for `stdout`.

See `setopts/2` for a description of the other options.
""".
-spec getopts(IoDevice) -> [getopt()] | {'error', Reason} when
      IoDevice :: device(),
      Reason :: term().

getopts(Io) ->
    request(Io, getopts).

-type setopt() :: 'binary' | 'list' | option().

-doc(#{equiv => setopts(standard_io, Opts)}).
-spec setopts(Opts) -> 'ok' | {'error', Reason} when
      Opts :: [setopt()],
      Reason :: term().

setopts(Opts) ->
    setopts(default_input(), Opts).

-doc """
Set options for [`IoDevice`](`t:device/0`). Possible options and values vary
depending on the I/O device.

For a list of supported options and their current values on a specific I/O
device, use function `getopts/1`.

The options and values supported by the OTP I/O devices are as follows:

- **`binary`, `list`, or `{binary, boolean()}`** - If set in binary mode
  (`binary` or `{binary, true}`), the I/O server sends binary data (encoded in
  UTF-8) as answers to the `get_line`, `get_chars`, and, if possible,
  `get_until` requests (for details, see section
  [The Erlang I/O Protocol](io_protocol.md)) in the User's Guide). The immediate
  effect is that [`get_chars/2,3`](`get_chars/2`) and
  [`get_line/1,2`](`get_line/1`) return UTF-8 binaries instead of lists of
  characters for the affected I/O device.

  By default, all I/O devices in OTP are set in `list` mode. However, the I/O
  functions can handle any of these modes and so should other, user-written,
  modules behaving as clients to I/O servers.

  This option is supported by the `t:standard_io/0`, `t:user/0` and `t:file:io_server/0`
   I/O servers.

- **`{echo, boolean()}`** - Denotes if the terminal is to echo input. Only
  supported for the standard shell I/O server (`group.erl`)

- **`{expand_fun, expand_fun()}`** - Provides a function for tab-completion
  (expansion) like the Erlang shell. This function is called when the user
  presses the _Tab_ key. The expansion is active when calling line-reading
  functions, such as [`get_line/1,2`](`get_line/1`).

  The function is called with the current line, up to the cursor, as a reversed
  string. It is to return a three-tuple: `{yes|no, string(), list()}`. The first
  element gives a beep if `no`, otherwise the expansion is silent; the second is
  a string that will be entered at the cursor position; the third is a list of
  possible expansions. If this list is not empty, it is printed below the
  current input line. The list of possible expansions can be formatted in
  different ways to make more advanced expansion suggestions more readable to
  the user, see `edlin_expand:expand/2` for documentation of that.

  Trivial example (beep on anything except empty line, which is expanded to
  `"quit"`):

  ```erlang
  fun("") -> {yes, "quit", []};
     (_) -> {no, "", ["quit"]} end
  ```

  This option is only supported by the standard shell (`group.erl`).

- **`{line_history, true | false}`** - Specifies if `get_line` and `get_until`
  I/O requests should be saved in the `m:shell` history buffer.

  This option is only supported by the standard shell (`group.erl`).

- **`{log, none | output | input | all}`** - Tells the I/O server that it should log
  I/O requests. Requests will be logged at [`info` level](`t:logger:level/0`) to the
  `[otp, kernel, io, input | output | ctrl]` domain with the following report:

  ```erl
  #{ request := IoRequest, server := pid(), server_name => term() }.
  ```

  It is important to note that extra care should be taken so that these log reports are not
  logged to `t:standard_io/0` as that may cause the system to enter an infinite loop.

  Example:

  ```
  1> logger:set_primary_config(level, info).
  ok
  2> logger:add_handler(stdout, logger_std_h, #{ config => #{ file => "stdout.log" }}).
  ok
  3> io:setopts(user, [{log, output}]).
  ok
  4> io:format(user, "Hello~n", []).
  Hello
  ok
  5> file:read_file("stdout.log").
  {ok,<<"2024-11-14T09:53:49.275085+01:00 info: <0.89.0> wrote to user, Hello\n">>}
  ```

  Not all I/O servers support this option. Use `io:getopts/1` to check if it is available.

  > #### Note {: .info }
  >
  > The I/O servers in Erlang/OTP will set the [logger domain](`logger_filters:domain/2`)
  > to `[otp, kernel, io, input | output]`. The default `m:logger` handler will not print
  > this domain, so you need to enable it. This can be done by adding a new filter like this:
  >
  > ```erl
  > logger:add_handler_filter(default, io_domain,
  >    {fun logger_filters:domain/2, {log,sub,[otp,kernel,io]}}).
  > ```

- **`{encoding, latin1 | unicode}`** - Specifies how characters are input or
  output from or to the I/O device, implying that, for example, a terminal is
  set to handle Unicode input and output or a file is set to handle UTF-8 data
  encoding.

  The option _does not_ affect how data is returned from the I/O functions or
  how it is sent in the I/O protocol, it only affects how the I/O device is to
  handle Unicode characters to the "physical" device.

  The standard shell is set for `unicode` or `latin1` encoding when the system
  is started. The encoding is set with the help of the `LANG` or `LC_CTYPE`
  environment variables on Unix-like system or by other means on other systems.
  So, the user can input Unicode characters and the I/O device is in
  `{encoding, unicode}` mode if the I/O device supports it. The mode can be
  changed, if the assumption of the runtime system is wrong, by setting this
  option.

  > #### Note {: .info }
  >
  > Prior to OTP 26.0, when Erlang was started with the `-oldshell` or
  > `-noshell` flags (for example, in an `escript`), the default encoding for
  > [`standard_io`](`t:standard_io/0`) was set to `latin1`, meaning that any
  > characters > codepoint 255 were escaped and that input was expected to be
  > plain 8-bit ISO Latin-1. As of OTP 26.0, [`standard_io`](`t:standard_io/0`)
  > always defaults to `unicode` if its supported, otherwise `latin1`.
  >
  > If you want to send raw bytes on [`standard_io`](`t:standard_io/0`), you now
  > always need to explicitly set the encoding to `latin1`; otherwise, code
  > points 128-255 will be converted to UTF-8. This is best done by setting the
  > kernel configuration parameter
  > [standard_io_encoding](`e:kernel:kernel_app.md#standard_io_encoding`) to
  > `latin1`.

  Files can also be set in `{encoding, unicode}`, meaning that data is written
  and read as UTF-8. More encodings are possible for files, see below.

  `{encoding, unicode | latin1}` is supported by both the standard shell
  (`group.erl` including `werl` on Windows), the 'oldshell' (`user.erl`), and
  the file I/O servers.

- **`{encoding, utf8 | utf16 | utf32 | {utf16,big} | {utf16,little} | {utf32,big} | {utf32,little}}`** -
  For disk files, the encoding can be set to various UTF variants. This has the
  effect that data is expected to be read as the specified encoding from the
  file, and the data is written in the specified encoding to the disk file.

  `{encoding, utf8}` has the same effect as `{encoding, unicode}` on files.

  The extended encodings are only supported on disk files (opened by function
  `file:open/2`).
""".
-spec setopts(IoDevice, Opts) -> 'ok' | {'error', Reason} when
      IoDevice :: device(),
      Opts :: [setopt()],
      Reason :: term().

setopts(Io, Opts) ->
    request(Io, {setopts, Opts}).

%% Writing and reading Erlang terms.

-doc(#{equiv => write(standard_io, Term)}).
-spec write(Term) -> 'ok' when
      Term :: term().

write(Term) ->
    o_request(?FUNCTION_NAME, [Term]).

-doc "Writes term `Term` to [`IoDevice`](`t:device/0`).".
-spec write(IoDevice, Term) -> 'ok' when
      IoDevice :: device(),
      Term :: term().

write(Io, Term) ->
    o_request(?FUNCTION_NAME, [Io, Term]).


-doc(#{equiv => read(standard_io, Prompt)}).
-spec read(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: {'ok', Term :: term()}
              | server_no_data()
              | {'error', ErrorInfo},
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info().

read(Prompt) ->
    read(default_input(), Prompt).

-doc """
Reads a term `Term` from the standard input (`IoDevice`), prompting it with
`Prompt`.

The function returns:

- **`{ok, Term}`** - The parsing was successful.

- **`eof`** - End of file was encountered.

- **`{error, ErrorInfo}`** - The parsing failed.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.
""".
-spec read(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: {'ok', Term :: term()}
              | server_no_data()
              | {'error', ErrorInfo},
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info().

read(Io, Prompt) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[1]}) of
	{ok,Toks,_EndLine} ->
	    erl_parse:parse_term(Toks);
	{error,E,_EndLine} ->
	    {error,E};
	{eof,_EndLine} ->
	    eof;
	Other ->
	    Other
    end.

-doc(#{equiv => read(IoDevice, Prompt, StartLocation, [])}).
-spec read(IoDevice, Prompt, StartLocation) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Result :: {'ok', Term :: term(), EndLocation :: erl_anno:location()}
              | {'eof', EndLocation :: erl_anno:location()}
              | server_no_data()
              | {'error', ErrorInfo, ErrorLocation :: erl_anno:location()},
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info().

read(Io, Prompt, Pos0) ->
    read(Io, Prompt, Pos0, []).

-doc """
Reads a term `Term` from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

Reading starts at location `StartLocation`. Argument `Options` is passed on as
argument `Options` of function `erl_scan:tokens/4`.

The function returns:

- **`{ok, Term, EndLocation}`** - The parsing was successful.

- **`{eof, EndLocation}`** - End of file was encountered.

- **`{error, ErrorInfo, ErrorLocation}`** - The parsing failed.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec read(IoDevice, Prompt, StartLocation, Options) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Options :: erl_scan:options(),
      Result :: {'ok', Term :: term(), EndLocation :: erl_anno:location()}
              | {'eof', EndLocation :: erl_anno:location()}
              | server_no_data()
              | {'error', ErrorInfo, ErrorLocation :: erl_anno:location()},
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info().

read(Io, Prompt, Pos0, Options) ->
    Args = [Pos0,Options],
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,Args}) of
	{ok,Toks,EndLocation} ->
            case erl_parse:parse_term(Toks) of
                {ok,Term} -> {ok,Term,EndLocation};
                {error,ErrorInfo} -> {error,ErrorInfo,EndLocation}
            end;
	{error,_E,_EndLocation} = Error ->
	    Error;
	{eof,_EndLocation} = Eof ->
	    Eof;
	Other ->
	    Other
    end.

%% Formatted writing and reading.

conv_reason(arguments) -> badarg;
conv_reason(terminated) -> terminated;
conv_reason(calling_self) -> calling_self;
conv_reason({no_translation,_,_}) -> no_translation;
conv_reason(_Reason) -> badarg.

-type format() :: atom() | string() | binary().

-doc(#{equiv => fwrite(Format, [])}).
-spec fwrite(Format) -> 'ok' when
      Format :: format().

fwrite(Format) ->
    o_request(?FUNCTION_NAME, [Format]).

-doc(#{equiv => fwrite(standard_io, Format, Data)}).
-spec fwrite(Format, Data) -> 'ok' when
      Format :: format(),
      Data :: [term()].

fwrite(Format, Args) ->
    o_request(?FUNCTION_NAME, [Format, Args]).

-doc """
Writes the items in `Data` on the [`IoDevice`](`t:device/0`) in accordance with `Format`.

`Format` contains plain characters that are copied to
the output device, and control sequences for formatting, see below. If `Format`
is an atom or a binary, it is first converted to a list with the aid of
[`atom_to_list/1`](`atom_to_list/1`) or
[`binary_to_list/1`](`binary_to_list/1`). Example:

```erlang
1> io:fwrite("Hello world!~n", []).
Hello world!
ok
```

The general format of a control sequence is `~F.P.PadModC`.

The character `C` determines the type of control sequence to be used. It is the
only required field. All of `F`, `P`, `Pad`, and `Mod` are optional. For
example, to use a `#` for `Pad` but use the default values for `F` and `P`, you
can write `~..#C`.

- `F` is the `field width` of the printed argument. A negative value means that
  the argument is left-justified within the field, otherwise right-justified. If
  no field width is specified, the required print width is used. If the field
  width specified is too small, the whole field is filled with `*` characters.
- `P` is the `precision` of the printed argument. A default value is used if no
  precision is specified. The interpretation of precision depends on the control
  sequences. Unless otherwise specified, argument `within` is used to determine
  print width.
- `Pad` is the padding character. This is the character used to pad the printed
  representation of the argument so that it conforms to the specified field
  width and precision. Only one padding character can be specified and, whenever
  applicable, it is used for both the field width and precision. The default
  padding character is `' '` (space).
- `Mod` is the control sequence modifier. This is one or more characters that
  change the interpretation of `Data`.

  The current modifiers are:

  - **`t`** - For Unicode translation.

  - **`l`** - For stopping `p` and `P` from detecting printable characters.

  - **`k`** - For use with `p`, `P`, `w`, and `W` to format maps in map-key
    `ordered` order (see `t:maps:iterator_order/0`).

  - **`K`** - Similar to `k`, for formatting maps in map-key order, but takes an
    extra argument that specifies the `t:maps:iterator_order/0`.

    For example:

    ```erlang
    > M = #{ a => 1, b => 2 }.
    #{a => 1,b => 2}
    > io:format("~Kp~n", [reversed, M]).
    #{b => 2,a => 1}
    ok
    ```

If `F`, `P`, or `Pad` is a `*` character, the next argument in `Data` is used as
the value. For example:

```erlang
1> io:fwrite("~*.*.0f~n",[9, 5, 3.14159265]).
003.14159
ok
```

To use a literal `*` character as `Pad`, it must be passed as an argument:

```erlang
2> io:fwrite("~*.*.*f~n",[9, 5, $*, 3.14159265]).
**3.14159
ok
```

_Available control sequences:_

- **`~`** - Character `~` is written.

- **`c`** - The argument is a number that is interpreted as an ASCII code. The
  precision is the number of times the character is printed and defaults to the
  field width, which in turn defaults to 1. Example:

  ```erlang
  1> io:fwrite("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]).
  |     aaaaa|bbbbb     |ccccc|
  ok
  ```

  If the Unicode translation modifier (`t`) is in effect, the integer argument
  can be any number representing a valid Unicode codepoint, otherwise it is to
  be an integer less than or equal to 255, otherwise it is masked with 16#FF:

  ```erlang
  2> io:fwrite("~tc~n",[1024]).
  \x{400}
  ok
  3> io:fwrite("~c~n",[1024]).
  ^@
  ok
  ```

- **`f`** - The argument is a float that is written as `[-]ddd.ddd`, where the
  precision is the number of digits after the decimal point. The default
  precision is 6 and it cannot be < 1.

- **`e`** - The argument is a float that is written as `[-]d.ddde+-ddd`, where
  the precision is the number of digits written. The default precision is 6 and
  it cannot be < 2.

- **`g`** - The argument is a float that is written as `f`, if it is >= 0.1 and
  < 10000.0. Otherwise, it is written in the `e` format. The precision is the
  number of significant digits. It defaults to 6 and is not to be < 2. If the
  absolute value of the float does not allow it to be written in the `f` format
  with the desired number of significant digits, it is also written in the `e`
  format.

- **`s`** - Prints the argument with the string syntax. The argument is, if no
  Unicode translation modifier is present, an `t:iolist/0`, a `t:binary/0`, or
  an `t:atom/0`. If the Unicode translation modifier (`t`) is in effect, the
  argument is [`unicode:chardata()`](`t:unicode:chardata/0`), meaning that
  binaries are in UTF-8. The characters are printed without quotes. The string
  is first truncated by the specified precision and then padded and justified to
  the specified field width. The default precision is the field width.

  This format can be used for printing any object and truncating the output so
  it fits a specified field:

  ```erlang
  1> io:fwrite("|~10w|~n", [{hey, hey, hey}]).
  |**********|
  ok
  2> io:fwrite("|~10s|~n", [io_lib:write({hey, hey, hey})]).
  |{hey,hey,h|
  3> io:fwrite("|~-10.8s|~n", [io_lib:write({hey, hey, hey})]).
  |{hey,hey  |
  ok
  ```

  A list with integers > 255 is considered an error if the Unicode translation
  modifier is not specified:

  ```erlang
  4> io:fwrite("~ts~n",[[1024]]).
  \x{400}
  ok
  5> io:fwrite("~s~n",[[1024]]).
  ** exception error: bad argument
       in function  io:format/3
          called as io:format(<0.53.0>,"~s~n",[[1024]])
  ```

- **`w`** - Writes data with the standard syntax. This is used to output Erlang
  terms. Atoms are printed within quotes if they contain embedded non-printable
  characters. Atom characters > 255 are escaped unless the Unicode translation
  modifier (`t`) is used. Floats are printed accurately as the shortest,
  correctly rounded string.

- **`p`**{: #tilde_p } - Writes the data with standard syntax in the same way as `~w`, but
  breaks terms whose printed representation is longer than one line into many
  lines and indents each line sensibly. Left-justification is not supported. It
  also tries to detect flat lists of printable characters and output these as
  strings. For example:

  ```erlang
  1> T = [{attributes,[[{id,age,1.50000},{mode,explicit},
  {typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
  {typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}].
  ...
  2> io:fwrite("~w~n", [T]).
  [{attributes,[[{id,age,1.5},{mode,explicit},{typename,
  [73,78,84,69,71,69,82]}],[{id,cho},{mode,explicit},{typena
  me,'Cho'}]]},{typename,'Person'},{tag,{'PRIVATE',3}},{mode
  ,implicit}]
  ok
  3> io:fwrite("~62p~n", [T]).
  [{attributes,[[{id,age,1.5},
                 {mode,explicit},
                 {typename,"INTEGER"}],
                [{id,cho},{mode,explicit},{typename,'Cho'}]]},
   {typename,'Person'},
   {tag,{'PRIVATE',3}},
   {mode,implicit}]
  ok
  ```

  The field width specifies the maximum line length. It defaults to 80. The
  precision specifies the initial indentation of the term. It defaults to the
  number of characters printed on this line in the _same_ call to `write/1` or
  [`format/1,2,3`](`format/1`). For example, using `T` above:

  ```erlang
  4> io:fwrite("Here T = ~62p~n", [T]).
  Here T = [{attributes,[[{id,age,1.5},
                          {mode,explicit},
                          {typename,"INTEGER"}],
                         [{id,cho},
                          {mode,explicit},
                          {typename,'Cho'}]]},
            {typename,'Person'},
            {tag,{'PRIVATE',3}},
            {mode,implicit}]
  ok
  ```

  As from Erlang/OTP 21.0, a field width of value `0` can be used for specifying
  that a line is infinitely long, which means that no line breaks are inserted.
  For example:

  ```erlang
  5> io:fwrite("~0p~n", [lists:seq(1, 30)]).
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
  ok
  ```

  When the modifier `l` is specified, no detection of printable character lists
  takes place, for example:

  ```erlang
  6> S = [{a,"a"}, {b, "b"}],
     io:fwrite("~15p~n", [S]).
  [{a,"a"},
   {b,"b"}]
  ok
  7> io:fwrite("~15lp~n", [S]).
  [{a,[97]},
   {b,[98]}]
  ok
  ```

  The Unicode translation modifier `t` specifies how to treat characters outside
  the Latin-1 range of codepoints, in atoms, strings, and binaries. For example,
  printing an atom containing a character > 255:

  ```erlang
  8> io:fwrite("~p~n",[list_to_atom([1024])]).
  '\x{400}'
  ok
  9> io:fwrite("~tp~n",[list_to_atom([1024])]).
  'Ѐ'
  ok
  ```

  By default, Erlang only detects lists of characters in the Latin-1 range as
  strings, but the `+pc unicode` flag can be used to change this (see
  `printable_range/0` for details). For example:

  ```erlang
  10> io:fwrite("~p~n",[[214]]).
  "Ö"
  ok
  11> io:fwrite("~p~n",[[1024]]).
  [1024]
  ok
  12> io:fwrite("~tp~n",[[1024]]).
  [1024]
  ok
  ```

  but if Erlang was started with `+pc unicode`:

  ```erlang
  13> io:fwrite("~p~n",[[1024]]).
  [1024]
  ok
  14> io:fwrite("~tp~n",[[1024]]).
  "Ѐ"
  ok
  ```

  Similarly, binaries that look like UTF-8 encoded strings are output with the
  binary string syntax if the `t` modifier is specified:

  ```erlang
  15> io:fwrite("~p~n", [<<208,128>>]).
  <<208,128>>
  ok
  16> io:fwrite("~tp~n", [<<208,128>>]).
  <<"Ѐ"/utf8>>
  ok
  17> io:fwrite("~tp~n", [<<128,128>>]).
  <<128,128>>
  ok
  ```

- **`W`** - Writes data in the same way as `~w`, but takes an extra argument
  that is the maximum depth to which terms are printed. Anything below this
  depth is replaced with `...`. For example, using `T` above:

  ```erlang
  8> io:fwrite("~W~n", [T,9]).
  [{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
  [{id,cho},{mode,...},{...}]]},{typename,'Person'},
  {tag,{'PRIVATE',3}},{mode,implicit}]
  ok
  ```

  If the maximum depth is reached, it cannot be read in the resultant output.
  Also, the `,...` form in a tuple denotes that there are more elements in the
  tuple but these are below the print depth.

- **`P`** - Writes data in the same way as `~p`, but takes an extra argument
  that is the maximum depth to which terms are printed. Anything below this
  depth is replaced with `...`, for example:

  ```erlang
  9> io:fwrite("~62P~n", [T,9]).
  [{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],
                [{id,cho},{mode,...},{...}]]},
   {typename,'Person'},
   {tag,{'PRIVATE',3}},
   {mode,implicit}]
  ok
  ```

- **`B`** - Writes an integer in base 2-36, the default base is 10. A leading
  dash is printed for negative integers.

  The precision field selects base, for example:

  ```erlang
  1> io:fwrite("~.16B~n", [31]).
  1F
  ok
  2> io:fwrite("~.2B~n", [-19]).
  -10011
  ok
  3> io:fwrite("~.36B~n", [5*36+35]).
  5Z
  ok
  ```

- **`X`** - Like `B`, but takes an extra argument that is a prefix to insert
  before the number, but after the leading dash, if any.

  The prefix can be a possibly deep list of characters or an atom. Example:

  ```erlang
  1> io:fwrite("~X~n", [31,"10#"]).
  10#31
  ok
  2> io:fwrite("~.16X~n", [-31,"0x"]).
  -0x1F
  ok
  ```

- **`#`** - Like `B`, but prints the number with an Erlang style `#`\-separated
  base prefix. Example:

  ```erlang
  1> io:fwrite("~.10#~n", [31]).
  10#31
  ok
  2> io:fwrite("~.16#~n", [-31]).
  -16#1F
  ok
  ```

- **`b`** - Like `B`, but prints lowercase letters.

- **`x`** - Like `X`, but prints lowercase letters.

- **`+`** - Like `#`, but prints lowercase letters.

- **`n`** - Writes a new line.

- **`i`** - Ignores the next term.

The function returns:

- **`ok`** - The formatting succeeded.

If an error occurs, there is no output. Example:

```erlang
1> io:fwrite("~s ~w ~i ~w ~c ~n",['abc def', 'abc def', {foo, 1},{foo, 1}, 65]).
abc def 'abc def'  {foo,1} A
ok
2> io:fwrite("~s", [65]).
** exception error: bad argument
     in function  io:format/3
        called as io:format(<0.53.0>,"~s","A")
```

In this example, an attempt was made to output the single character 65 with the
aid of the string formatting directive `"~s"`.
""".
-spec fwrite(IoDevice, Format, Data) -> 'ok' when
      IoDevice :: device(),
      Format :: format(),
      Data :: [term()].

fwrite(Io, Format, Args) ->
    o_request(?FUNCTION_NAME, [Io, Format, Args]).

-doc(#{equiv => fread(standard_io, Prompt, Format)}).
-spec fread(Prompt, Format) -> Result when
      Prompt :: prompt(),
      Format :: format(),
      Result :: {'ok', Terms :: [term()]} | 'eof' | {'error', What :: term()}.

fread(Prompt, Format) ->
    fread(default_input(), Prompt, Format).

-doc """
Reads characters from [`IoDevice`](`t:device/0`), prompting it with `Prompt`. Interprets the
characters in accordance with `Format`.

`Format` can contain the following:

- Whitespace characters (_Space_, _Tab_, and _Newline_) that cause input to be
  read to the next non-whitespace character.
- Ordinary characters that must match the next input character.
- Control sequences, which have the general format `~*FMC`, where:

  - Character `*` is an optional return suppression character. It provides a
    method to specify a field that is to be omitted.
  - `F` is the `field width` of the input field.
  - `M` is an optional translation modifier (of which `t` is the only supported,
    meaning Unicode translation).
  - `C` determines the type of control sequence.

  Unless otherwise specified, leading whitespace is ignored for all control
  sequences. An input field cannot be more than one line wide.

  _Available control sequences:_

  - **`~`** - A single `~` is expected in the input.

  - **`d`** - A decimal integer is expected.

  - **`u`** - An unsigned integer in base 2-36 is expected. The field width
    parameter is used to specify base. Leading whitespace characters are not
    skipped.

  - **`-`** - An optional sign character is expected. A sign character `-` gives
    return value `-1`. Sign character `+` or none gives `1`. The field width
    parameter is ignored. Leading whitespace characters are not skipped.

  - **`#`** - An integer in base 2-36 with Erlang-style base prefix (for
    example, `"16#ffff"`) is expected.

  - **`f`** - A floating point number is expected. It must follow the Erlang
    floating point number syntax.

  - **`s`** - A string of non-whitespace characters is read. If a field width
    has been specified, this number of characters are read and all trailing
    whitespace characters are stripped. An Erlang string (list of characters) is
    returned.

    If Unicode translation is in effect (`~ts`), characters > 255 are accepted,
    otherwise not. With the translation modifier, the returned list can as a
    consequence also contain integers > 255:

    ```erlang
    1> io:fread("Prompt> ","~s").
    Prompt> <Characters beyond latin1 range not printable in this medium>
    {error,{fread,string}}
    2> io:fread("Prompt> ","~ts").
    Prompt> <Characters beyond latin1 range not printable in this medium>
    {ok,[[1091,1085,1080,1094,1086,1076,1077]]}
    ```

  - **`a`** - Similar to `s`, but the resulting string is converted into an
    atom.

  - **`c`** - The number of characters equal to the field width are read
    (default is 1) and returned as an Erlang string. However, leading and
    trailing whitespace characters are not omitted as they are with `s`. All
    characters are returned.

    The Unicode translation modifier works as with `s`:

    ```erlang
    1> io:fread("Prompt> ","~c").
    Prompt> <Character beyond latin1 range not printable in this medium>
    {error,{fread,string}}
    2> io:fread("Prompt> ","~tc").
    Prompt> <Character beyond latin1 range not printable in this medium>
    {ok,[[1091]]}
    ```

  - **`l`** - Returns the number of characters that have been scanned up to that
    point, including whitespace characters.

  The function returns:

  - **`{ok, Terms}`** - The read was successful and `Terms` is the list of
    successfully matched and read items.

  - **`eof`** - End of file was encountered.

  - **`{error, FreadError}`** - The reading failed and `FreadError` gives a hint
    about the error.

  - **`{error, ErrorDescription}`** - The read operation failed and parameter
    `ErrorDescription` gives a hint about the error.

_Examples:_

```erlang
20> io:fread('enter>', "~f~f~f").
enter>1.9 35.5e3 15.0
{ok,[1.9,3.55e4,15.0]}
21> io:fread('enter>', "~10f~d").
enter>     5.67899
{ok,[5.678,99]}
22> io:fread('enter>', ":~10s:~10c:").
enter>:   alan   :   joe    :
{ok, ["alan", "   joe    "]}
```
""".
-spec fread(IoDevice, Prompt, Format) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Format :: format(),
      Result :: {'ok', Terms :: [term()]}
              | {'error', {'fread', FreadError :: io_lib:fread_error()}}
              | server_no_data().

fread(Io, Prompt, Format) ->
    request(Io, {fread,Prompt,Format}).

-doc(#{equiv => format(Format, [])}).
-spec format(Format) -> 'ok' when
      Format :: format().
format(Format) ->
    o_request(?FUNCTION_NAME, [Format]).

-doc(#{equiv => format(standard_io, Format, Data)}).
-spec format(Format, Data) -> 'ok' when
      Format :: format(),
      Data :: [term()].

format(Format, Args) ->
    o_request(?FUNCTION_NAME, [Format, Args]).

-doc(#{equiv => fwrite(IoDevice, Format, Data)}).
-spec format(IoDevice, Format, Data) -> 'ok' when
      IoDevice :: device(),
      Format :: format(),
      Data :: [term()].

format(Io, Format, Args) ->
    o_request(?FUNCTION_NAME, [Io, Format, Args]).

%% Scanning Erlang code.

-doc(#{equiv => scan_erl_exprs(standard_io, Prompt)}).
-spec scan_erl_exprs(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | server_no_data().
 
scan_erl_exprs(Prompt) ->
    scan_erl_exprs(default_input(), Prompt, 1).

-doc(#{equiv => scan_erl_exprs(Device, Prompt, 1)}).
-spec scan_erl_exprs(Device, Prompt) -> Result when
      Device :: device(),
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_exprs(Io, Prompt) ->
    scan_erl_exprs(Io, Prompt, 1).

-doc(#{equiv => scan_erl_exprs(Device, Prompt, StartLocation, [])}).
-spec scan_erl_exprs(Device, Prompt, StartLocation) -> Result when
      Device :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_exprs(Io, Prompt, Pos0) ->
    scan_erl_exprs(Io, Prompt, Pos0, []).

-doc """
Reads data from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

Reading starts at location `StartLocation`. Argument `Options` is passed on as
argument `Options` of function `erl_scan:tokens/4`. The data is tokenized
as if it were a sequence of Erlang expressions until a final dot (`.`) is
reached. This token is also returned.

The function returns:

- **`{ok, Tokens, EndLocation}`** - The tokenization succeeded.

- **`{eof, EndLocation}`** - End of file was encountered by the tokenizer.

- **`eof`** - End of file was encountered by the I/O server.

- **`{error, ErrorInfo, ErrorLocation}`** - An error occurred while tokenizing.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.

_Example:_

```erlang
23> io:scan_erl_exprs('enter>').
enter>abc(), "hey".
{ok,[{atom,1,abc},{'(',1},{')',1},{',',1},{string,1,"hey"},{dot,1}],2}
24> io:scan_erl_exprs('enter>').
enter>1.0er.
{error,{1,erl_scan,{illegal,float}},2}
```
""".
-doc(#{since => <<"OTP R16B">>}).
-spec scan_erl_exprs(Device, Prompt, StartLocation, Options) -> Result when
      Device :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Options :: erl_scan:options(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_exprs(Io, Prompt, Pos0, Options) ->
    request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0,Options]}).

-doc(#{equiv => scan_erl_form(standard_io, Prompt)}).
-spec scan_erl_form(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_form(Prompt) ->
    scan_erl_form(default_input(), Prompt, 1).

-doc(#{equiv => scan_erl_form(IoDevice, Prompt, 1)}).
-spec scan_erl_form(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_form(Io, Prompt) ->
    scan_erl_form(Io, Prompt, 1).

-doc(#{equiv => scan_erl_form(IoDevice, Prompt, StartLocation, [])}).
-spec scan_erl_form(IoDevice, Prompt, StartLocation) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_form(Io, Prompt, Pos0) ->
    scan_erl_form(Io, Prompt, Pos0, []).

-doc """
Reads data from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

Starts reading at location `StartLocation` (`1`). Argument `Options` is passed
on as argument `Options` of function `erl_scan:tokens/4`. The data is tokenized
as if it was an Erlang form (one of the valid Erlang expressions in an Erlang
source file) until a final dot (`.`) is reached. This last token is also
returned.

The return values are the same as for [`scan_erl_exprs/4`](`scan_erl_exprs/4`).
""".
-doc(#{since => <<"OTP R16B">>}).
-spec scan_erl_form(IoDevice, Prompt, StartLocation, Options) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Options :: erl_scan:options(),
      Result :: erl_scan:tokens_result() | server_no_data().

scan_erl_form(Io, Prompt, Pos0, Options) ->
    request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0,Options]}).

%% Parsing Erlang code.

-type parse_ret() :: {'ok',
                      ExprList :: [erl_parse:abstract_expr()],
                      EndLocation :: erl_anno:location()}
                   | {'eof', EndLocation :: erl_anno:location()}
                   | {'error',
                      ErrorInfo :: erl_scan:error_info()
                                 | erl_parse:error_info(),
                      ErrorLocation :: erl_anno:location()}
                   | server_no_data().

-doc(#{equiv => parse_erl_exprs(standard_io, Prompt)}).
-spec parse_erl_exprs(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: parse_ret().

parse_erl_exprs(Prompt) ->
    parse_erl_exprs(default_input(), Prompt, 1).

-doc(#{equiv => parse_erl_exprs(IoDevice, Prompt, 1)}).
-spec parse_erl_exprs(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: parse_ret().

parse_erl_exprs(Io, Prompt) ->
    parse_erl_exprs(Io, Prompt, 1).

-doc(#{equiv => parse_erl_exprs(IoDevice, Prompt, StartLocation, [])}).
-spec parse_erl_exprs(IoDevice, Prompt, StartLocation) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Result :: parse_ret().

parse_erl_exprs(Io, Prompt, Pos0) ->
    parse_erl_exprs(Io, Prompt, Pos0, []).

-doc """
Reads data from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

Starts reading at location `StartLocation`. Argument `Options` is passed
on as argument `Options` of function `erl_scan:tokens/4`. The data is tokenized
and parsed as if it was a sequence of Erlang expressions until a final dot (`.`)
is reached.

The function returns:

- **`{ok, ExprList, EndLocation}`** - The parsing was successful.

- **`{eof, EndLocation}`** - End of file was encountered by the tokenizer.

- **`eof`** - End of file was encountered by the I/O server.

- **`{error, ErrorInfo, ErrorLocation}`** - An error occurred while tokenizing
  or parsing.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.

Example:

```erlang
25> io:parse_erl_exprs('enter>').
enter>abc(), "hey".
{ok, [{call,1,{atom,1,abc},[]},{string,1,"hey"}],2}
26> io:parse_erl_exprs('enter>').
enter>abc("hey".
{error,{1,erl_parse,["syntax error before: ",["'.'"]]},2}
```
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_erl_exprs(IoDevice, Prompt, StartLocation, Options) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Options :: erl_scan:options(),
      Result :: parse_ret().

parse_erl_exprs(Io, Prompt, Pos0, Options) ->
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,[Pos0,Options]}) of
	{ok,Toks,EndPos} ->
	    case erl_parse:parse_exprs(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

-type parse_form_ret() :: {'ok',
                           AbsForm :: erl_parse:abstract_form(),
                           EndLocation :: erl_anno:location()}
                        | {'eof', EndLocation :: erl_anno:location()}
                        | {'error',
                           ErrorInfo :: erl_scan:error_info()
                                      | erl_parse:error_info(),
                           ErrorLocation :: erl_anno:location()}
                        | server_no_data().

-doc(#{equiv => parse_erl_form(standard_io, Prompt)}).
-spec parse_erl_form(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: parse_form_ret().

parse_erl_form(Prompt) ->
    parse_erl_form(default_input(), Prompt, 1).

-doc(#{equiv => parse_erl_form(IoDevice, Prompt, 1)}).
-spec parse_erl_form(IoDevice, Prompt) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      Result :: parse_form_ret().

parse_erl_form(Io, Prompt) ->
    parse_erl_form(Io, Prompt, 1).

-doc(#{equiv => parse_erl_form(IoDevice, Prompt, StartLocation, [])}).
-spec parse_erl_form(IoDevice, Prompt, StartLocation) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Result :: parse_form_ret().

parse_erl_form(Io, Prompt, Pos0) ->
    parse_erl_form(Io, Prompt, Pos0, []).

-doc """
Reads data from [`IoDevice`](`t:device/0`), prompting it with `Prompt`.

Starts reading at location `StartLocation`. Argument `Options` is passed
on as argument `Options` of function `erl_scan:tokens/4`. The data is tokenized
and parsed as if it was an Erlang form (one of the valid Erlang expressions in
an Erlang source file) until a final dot (`.`) is reached.

The function returns:

- **`{ok, AbsForm, EndLocation}`** - The parsing was successful.

- **`{eof, EndLocation}`** - End of file was encountered by the tokenizer.

- **`eof`** - End of file was encountered by the I/O server.

- **`{error, ErrorInfo, ErrorLocation}`** - An error occurred while tokenizing
  or parsing.

- **`{error, ErrorDescription}`** - Other (rare) error condition, such as
  `{error, estale}` if reading from an NFS file system.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec parse_erl_form(IoDevice, Prompt, StartLocation, Options) -> Result when
      IoDevice :: device(),
      Prompt :: prompt(),
      StartLocation :: erl_anno:location(),
      Options :: erl_scan:options(),
      Result :: parse_form_ret().

parse_erl_form(Io, Prompt, Pos0, Options) ->
    Args = [Pos0, Options],
    case request(Io, {get_until,unicode,Prompt,erl_scan,tokens,Args}) of
	{ok,Toks,EndPos} ->
	    case erl_parse:parse_form(Toks) of
		{ok,Exprs} -> {ok,Exprs,EndPos};
		{error,E} -> {error,E,EndPos}
	    end;
	Other ->
	    Other
    end.

%% Miscellaneous functions.

-doc false.
request(Request) ->
    request(default_output(), Request).

-doc false.
request(Name, Request) ->
    request(Name, Request, error).
request(standard_io, Request, ErrorTag) ->
    request(group_leader(), Request, ErrorTag);
request(Pid, Request, ErrorTag) when is_pid(Pid) ->
    execute_request(Pid, io_request(Pid, Request), ErrorTag);
request(Name, Request, ErrorTag) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {ErrorTag, arguments};
	Pid ->
	    request(Pid, Request, ErrorTag)
    end.

execute_request(Pid, _Tuple, ErrorTag) when Pid =:= self() ->
    {ErrorTag, calling_self};
execute_request(Pid, {Convert,Converted}, ErrorTag) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {io_request,self(),Mref,Converted},

    receive
	{io_reply, Mref, Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    if
		Convert ->
		    convert_binaries(Reply);
		true ->
		    Reply
	    end;
	{'DOWN', Mref, _, _, _} ->
	    receive
		{'EXIT', Pid, _What} -> true
	    after 0 -> true
	    end,
	    {ErrorTag,terminated}
    end.

-doc false.
requests(Requests) ->				%Requests as atomic action
    requests(default_output(), Requests).

-doc false.
requests(standard_io, Requests) ->              %Requests as atomic action
    requests(group_leader(), Requests);
requests(Pid, Requests) when is_pid(Pid) ->
    {Convert, Converted} = io_requests(Pid, Requests),
    execute_request(Pid,{Convert,{requests,Converted}},error);
requests(Name, Requests) when is_atom(Name) ->
    case whereis(Name) of
	undefined ->
	    {error, arguments};
	Pid ->
	    requests(Pid, Requests)
    end.


default_input() ->
    group_leader().

default_output() ->
    group_leader().

%% io_requests(Requests)
%%  Transform requests into correct i/o server messages. Only handle the
%%  one we KNOW must be changed, others, including incorrect ones, are
%%  passed straight through. Perform a flatten on the request list.

io_requests(Pid, Rs) ->
    io_requests(Pid, Rs, [], []).

io_requests(Pid, [{requests,Rs1}|Rs], Cont, Tail) ->
    io_requests(Pid, Rs1, [Rs|Cont], Tail);
io_requests(Pid, [R], [], _Tail) ->
    {Conv,Request} = io_request(Pid, R),
    {Conv,[Request]};
io_requests(Pid, [R|Rs], Cont, Tail) ->
    {_,Request} = io_request(Pid, R),
    {Conv,Requests} = io_requests(Pid, Rs, Cont, Tail),
    {Conv,[Request|Requests]};
io_requests(Pid, [], [Rs|Cont], Tail) ->
    io_requests(Pid, Rs, Cont, Tail);
io_requests(_Pid, [], [], _Tail) -> 
    {false,[]}.

bc_req(Pid, Req0, MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    %% The most common case. A modern i/o server.
	    {false,Req0};
	false ->
	    %% Backward compatibility only. Unlikely to ever happen.
	    case tuple_to_list(Req0) of
		[Op,_Enc] ->
		    {MaybeConvert,Op};
		[Op,_Enc|T] ->
		    Req = list_to_tuple([Op|T]),
		    {MaybeConvert,Req}
	    end
    end.

io_request(Pid, {write,Term}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,write,[Term]},false);
io_request(Pid, {format,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,format,[Format,Args]},false);
io_request(Pid, {fwrite,Format,Args}) ->
    bc_req(Pid,{put_chars,unicode,io_lib,fwrite,[Format,Args]},false);
io_request(Pid, nl) ->
    bc_req(Pid,{put_chars,unicode,io_lib:nl()},false);
io_request(Pid, {put_chars,Enc,Chars}=Request0) 
  when is_list(Chars), node(Pid) =:= node() ->
    %% Convert to binary data if the I/O server is guaranteed to be new
    Request =
	case catch unicode:characters_to_binary(Chars,Enc) of
	    Binary when is_binary(Binary) ->
		{put_chars,Enc,Binary};
	    _ ->
		Request0
	end,
    {false,Request};
io_request(Pid, {put_chars,Enc,Chars}=Request0) 
  when is_list(Chars) ->
    case net_kernel:dflag_unicode_io(Pid) of
	true ->
	    case catch unicode:characters_to_binary(Chars,Enc,unicode) of
		Binary when is_binary(Binary) ->
		    {false,{put_chars,unicode,Binary}};
		_ ->
		    {false,Request0}
	    end;
	false ->
	    %% Convert back to old style put_chars message...
	    case catch unicode:characters_to_binary(Chars,Enc,latin1) of
		Binary when is_binary(Binary) ->
		    {false,{put_chars,Binary}};
		_ ->
		    {false,{put_chars,Chars}}
	    end
    end;
io_request(Pid, {fread,Prompt,Format}) ->
    bc_req(Pid,{get_until,unicode,Prompt,io_lib,fread,[Format]},true);
io_request(Pid, {get_until,Enc,Prompt,M,F,A}) ->
    bc_req(Pid,{get_until,Enc,Prompt,M,F,A},true);
io_request(Pid, {get_chars,Enc,Prompt,N}) ->
    bc_req(Pid,{get_chars,Enc,Prompt,N},true);
io_request(Pid, {get_line,Enc,Prompt}) ->
    bc_req(Pid,{get_line,Enc,Prompt},true);
io_request(Pid, {get_password,Enc}) ->
    bc_req(Pid,{get_password, Enc},true);
io_request(_Pid, R) ->				%Pass this straight through
    {false,R}.

convert_binaries(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(Bin,latin1,unicode);
convert_binaries(Else) ->
    Else.
