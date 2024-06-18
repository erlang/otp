%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-module(epp).
-moduledoc """
An Erlang code preprocessor.

The Erlang code preprocessor includes functions that are used by the `m:compile`
module to preprocess macros and include files before the parsing takes place.

The Erlang source file _encoding_{: #encoding } is selected by a comment in one
of the first two lines of the source file. The first string matching the regular
expression `coding\s*[:=]\s*([-a-zA-Z0-9])+` selects the encoding. If the
matching string is not a valid encoding, it is ignored. The valid encodings are
`Latin-1` and `UTF-8`, where the case of the characters can be chosen freely.

_Examples:_

```erlang
%% coding: utf-8
```

```erlang
%% For this file we have chosen encoding = Latin-1
```

```erlang
%% -*- coding: latin-1 -*-
```

## Error Information

`ErrorInfo` is the standard `ErrorInfo` structure that is returned from all I/O
modules. The format is as follows:

```erlang
{ErrorLine, Module, ErrorDescriptor}
```

A string describing the error is obtained with the following call:

```erlang
Module:format_error(ErrorDescriptor)
```

## See Also

`m:erl_parse`
""".

%% An Erlang code preprocessor.

-export([open/1,open/2,open/3,close/1,format_error/1]).
-export([scan_erl_form/1,parse_erl_form/1,macro_defs/1]).
-export([scan_file/1, scan_file/2, parse_file/1, parse_file/2, parse_file/3]).
-export([default_encoding/0, encoding_to_string/1,
         read_encoding_from_binary/1, read_encoding_from_binary/2,
         set_encoding/1, set_encoding/2, read_encoding/1, read_encoding/2]).

-export([interpret_file_attribute/1]).
-export([normalize_typed_record_fields/1,restore_typed_record_fields/1]).

-include_lib("kernel/include/file.hrl").

%%------------------------------------------------------------------------

-export_type([source_encoding/0]).

-type macros() :: [atom() | {atom(), term()} | {atom(), term(), 'redefine'}].
-doc "Handle to the `epp` server.".
-type epp_handle() :: pid().
-type source_encoding() :: latin1 | utf8.

-type ifdef() :: 'ifdef' | 'ifndef' | 'if' | 'else'.

-type name() :: atom().
-type argspec() :: 'none'                       %No arguments
                 | non_neg_integer().           %Number of arguments
-type argnames() :: [atom()].
-type tokens() :: [erl_scan:token()].
-type predef() :: 'undefined' | {'none', tokens()}.
-type userdef() :: {argspec(), {argnames(), tokens()}}.
-type used() :: {name(), argspec()}.

-type function_name_type() :: 'undefined'
			    | {atom(),non_neg_integer()}
			    | tokens().

-type warning_info() :: {erl_anno:location(), module(), term()}.

-define(DEFAULT_ENCODING, utf8).

%% Epp state record.
-record(epp, {file :: file:io_device()
                    | 'undefined',              %Current file
	      location=1,         		%Current location
              delta=0 :: non_neg_integer(),     %Offset from Location (-file)
              name="" :: file:name(),           %Current file name
              name2="" :: file:name(),          %-"-, modified by -file
              istk=[] :: [ifdef()],             %Ifdef stack
              sstk=[] :: [#epp{}],              %State stack
              path=[] :: [file:name()],         %Include-path
              macs = #{}		        %Macros (don't care locations)
	            :: #{name() => predef() | [userdef()]},
              uses = #{}			%Macro use structure
	            :: #{name() => [{argspec(), [used()]}]},
              default_encoding = ?DEFAULT_ENCODING :: source_encoding(),
              pre_opened = false :: boolean(),
              in_prefix = true :: boolean(),
              erl_scan_opts = [] :: [_],
              features = [] :: [atom()],
              else_reserved = false :: boolean(),
              fname = [] :: function_name_type(),
              deterministic = false :: boolean()
	     }).

%% open(Options)
%% open(FileName, IncludePath)
%% open(FileName, IncludePath, PreDefMacros)
%% close(Epp)
%% scan_erl_form(Epp)
%% parse_erl_form(Epp)
%% scan_file(Epp)
%% scan_file(FileName, Options)
%% parse_file(Epp)
%% parse_file(FileName, Options)
%% parse_file(FileName, IncludePath, PreDefMacros)
%% macro_defs(Epp)

-doc "Equivalent to `epp:open([{name, FileName}, {includes, IncludePath}])`.".
-spec open(FileName, IncludePath) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      Epp :: epp_handle(),
      ErrorDescriptor :: term().

open(Name, Path) ->
    open(Name, Path, []).

-doc """
Equivalent to
`epp:open([{name, FileName}, {includes, IncludePath}, {macros, PredefMacros}])`.
""".
-spec open(FileName, IncludePath, PredefMacros) ->
	{'ok', Epp} | {'error', ErrorDescriptor} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      PredefMacros :: macros(),
      Epp :: epp_handle(),
      ErrorDescriptor :: term().

open(Name, Path, Pdm) ->
    open([{name, Name}, {includes, Path}, {macros, Pdm}]).

-doc """
Opens a file for preprocessing.

If you want to change the file name of the implicit -file() attributes inserted
during preprocessing, you can do with `{source_name, SourceName}`. If unset it
will default to the name of the opened file.

Setting `{deterministic, Enabled}` will additionally reduce the file name of the
implicit -file() attributes inserted during preprocessing to only the basename
of the path.

If `extra` is specified in `Options`, the return value is `{ok, Epp, Extra}`
instead of `{ok, Epp}`.

The option `location` is forwarded to the Erlang token scanner, see
[`erl_scan:tokens/3,4`](`erl_scan:tokens/3`).

The `{compiler_internal,term()}` option is forwarded to the Erlang token
scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`).
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec open(Options) ->
		  {'ok', Epp} | {'ok', Epp, Extra} | {'error', ErrorDescriptor} when
      Options :: [{'default_encoding', DefEncoding :: source_encoding()} |
		  {'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'source_name', SourceName :: file:name()} |
		  {'deterministic', Enabled :: boolean()} |
		  {'macros', PredefMacros :: macros()} |
		  {'name',FileName :: file:name()} |
		  {'location',StartLocation :: erl_anno:location()} |
		  {'fd',FileDescriptor :: file:io_device()} |
		  'extra' |
                  {'compiler_internal', [term()]}],
      Epp :: epp_handle(),
      Extra :: [{'encoding', source_encoding() | 'none'}],
      ErrorDescriptor :: term().

open(Options) ->
    case proplists:get_value(name, Options) of
        undefined ->
            erlang:error(badarg);
        Name ->
            Self = self(),
            Epp = spawn(fun() -> server(Self, Name, Options) end),
            Extra = proplists:get_bool(extra, Options),
            case epp_request(Epp) of
                {ok, Pid, Encoding} when Extra ->
                    {ok, Pid, [{encoding, Encoding}]};
                {ok, Pid, _} ->
                    {ok, Pid};
                {ok, Pid} when Extra ->
                    {ok, Pid, []};
                Other ->
                    Other
            end
    end.

-doc "Closes the preprocessing of a file.".
-spec close(Epp) -> 'ok' when
      Epp :: epp_handle().

close(Epp) ->
    %% Make sure that close is synchronous as a courtesy to test
    %% cases that test for resource leaks.
    Ref = erlang:monitor(process, Epp),
    R = epp_request(Epp, close),
    receive {'DOWN',Ref,_,_,_} -> ok end,
    R.

-doc """
Returns the raw tokens of the next Erlang form from the opened Erlang source
file. A tuple `{eof, Line}` is returned at the end of the file. The first form
corresponds to an implicit attribute `-file(File,1).`, where `File` is the file
name.
""".
-doc(#{since => <<"OTP R13B03">>}).
-spec scan_erl_form(Epp) ->
    {'ok', Tokens} | {error, ErrorInfo} |
    {'warning',WarningInfo} | {'eof',Line} when
      Epp :: epp_handle(),
      Tokens :: erl_scan:tokens(),
      Line :: erl_anno:line(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      WarningInfo :: warning_info().

scan_erl_form(Epp) ->
    epp_request(Epp, scan_erl_form).

-doc """
Returns the next Erlang form from the opened Erlang source file. Tuple
`{eof, Location}` is returned at the end of the file. The first form corresponds
to an implicit attribute `-file(File,1).`, where `File` is the file name.
""".
-spec parse_erl_form(Epp) ->
    {'ok', AbsForm} | {error, ErrorInfo} |
    {'warning',WarningInfo} | {'eof',Location} when
      Epp :: epp_handle(),
      AbsForm :: erl_parse:abstract_form(),
      Location :: erl_anno:location(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      WarningInfo :: warning_info().

parse_erl_form(Epp) ->
    case epp_request(Epp, scan_erl_form) of
	{ok,Toks} ->
	    erl_parse:parse_form(Toks);
	Other ->
	    Other
    end.

-doc false.
macro_defs(Epp) ->
    epp_request(Epp, macro_defs).

%% format_error(ErrorDescriptor) -> String
%%  Return a string describing the error.

-doc """
Takes an `ErrorDescriptor` and returns a string that describes the error or
warning. This function is usually called implicitly when processing an
`ErrorInfo` structure (see section [Error Information](`m:epp#module-error-information`)).
""".
-doc(#{since => <<"OTP R14B03">>}).
-spec format_error(ErrorDescriptor) -> io_lib:chars() when
      ErrorDescriptor :: term().

format_error(Error) ->
    case format_error_1(Error) of
        {Format, Args} when is_list(Args) ->
            io_lib:format(Format, Args);
        Bin when is_binary(Bin) ->
            unicode:characters_to_list(Bin);
        List when is_list(List) ->
            List
    end.

format_error_1(cannot_parse) ->
    ~"cannot parse file, giving up";
format_error_1({bad,W}) ->
    {~"badly formed '~s'", [W]};
format_error_1({duplicated_argument, Arg}) ->
    {~"argument '~ts' already used", [Arg]};
format_error_1(missing_parenthesis) ->
    {~"badly formed define: missing closing right parenthesis",[]};
format_error_1(missing_comma) ->
    ~"badly formed define: missing comma";
format_error_1(premature_end) ->
    ~"premature end";
format_error_1({call,What}) ->
    {~"illegal macro call '~ts'",[What]};
format_error_1({undefined,M,none}) ->
    {~"undefined macro '~ts'", [M]};
format_error_1({undefined,M,A}) ->
    {~"undefined macro '~ts/~p'", [M,A]};
format_error_1({depth,What}) ->
    {~"~s too deep",[What]};
format_error_1({mismatch,M}) ->
    {~"argument mismatch for macro '~ts'", [M]};
format_error_1({arg_error,M}) ->
    {~"badly formed argument for macro '~ts'", [M]};
format_error_1({redefine,M}) ->
    {~"redefining macro '~ts'", [M]};
format_error_1({redefine_predef,M}) ->
    {~"redefining predefined macro '~s'", [M]};
format_error_1({circular,M,none}) ->
    {~"circular macro '~ts'", [M]};
format_error_1({circular,M,A}) ->
    {~"circular macro '~ts/~p'", [M,A]};
format_error_1({include,W,F}) ->
    {~"can't find include ~s \"~ts\"", [W,F]};
format_error_1({Tag, invalid, Alternative}) when Tag =:= moduledoc; Tag =:= doc ->
    {~"invalid ~s tag, only ~s allowed", [Tag, Alternative]};
format_error_1({Tag, W, Filename}) when Tag =:= moduledoc; Tag =:= doc ->
    {~"can't find ~s ~s \"~ts\"", [Tag, W, Filename]};
format_error_1({illegal,How,What}) ->
    {~"~s '-~s'", [How,What]};
format_error_1({illegal_function,Macro}) ->
    {~"?~s can only be used within a function", [Macro]};
format_error_1({illegal_function_usage,Macro}) ->
    {~"?~s must not begin a form", [Macro]};
format_error_1(elif_after_else) ->
    "'elif' following 'else'";
format_error_1({'NYI',What}) ->
    {~"not yet implemented '~s'", [What]};
format_error_1({error,Term}) ->
    {~"-error(~tp).", [Term]};
format_error_1({warning,Term}) ->
    {~"-warning(~tp).", [Term]};
format_error_1(ftr_after_prefix) ->
    ~"feature directive not allowed after exports or record definitions";
format_error_1(E) -> file:format_error(E).

-doc """
Preprocesses an Erlang source file returning a list of the lists of raw tokens
of each form. Notice that the tuple `{eof, Line}` returned at the end of the
file is included as a "form", and any failures to scan a form are included in
the list as tuples `{error, ErrorInfo}`.
""".
-doc(#{since => <<"OTP 24.0">>}).
-spec scan_file(FileName, Options) ->
        {'ok', [Form], Extra} | {error, OpenError} when
      FileName :: file:name(),
      Options :: [{'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'source_name', SourceName :: file:name()} |
		  {'macros', PredefMacros :: macros()} |
		  {'default_encoding', DefEncoding :: source_encoding()}],
      Form :: erl_scan:tokens() | {'error', ErrorInfo} | {'eof', Loc},
      Loc :: erl_anno:location(),
      ErrorInfo :: erl_scan:error_info(),
      Extra :: [{'encoding', source_encoding() | 'none'}],
      OpenError :: file:posix() | badarg | system_limit.

scan_file(Ifile, Options) ->
    case open([{name, Ifile}, extra | Options]) of
	{ok,Epp,Extra} ->
	    Forms = scan_file(Epp),
	    close(Epp),
	    {ok,Forms,Extra};
	{error,E} ->
	    {error,E}
    end.

-doc false.
scan_file(Epp) ->
    case scan_erl_form(Epp) of
	{ok,Toks} ->
            [Toks|scan_file(Epp)];
	{error,E} ->
	    [{error,E}|scan_file(Epp)];
	{eof,Location} ->
	    [{eof,Location}]
    end.

-doc """
Equivalent to
`epp:parse_file(FileName, [{includes, IncludePath}, {macros, PredefMacros}])`.
""".
-spec parse_file(FileName, IncludePath, PredefMacros) ->
                {'ok', [Form]} | {error, OpenError} when
      FileName :: file:name(),
      IncludePath :: [DirectoryName :: file:name()],
      Form :: erl_parse:abstract_form()
            | {'error', ErrorInfo}
            | {'eof',Location},
      PredefMacros :: macros(),
      Location :: erl_anno:location(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Path, Predefs) ->
    parse_file(Ifile, [{includes, Path}, {macros, Predefs}]).

-doc """
Preprocesses and parses an Erlang source file. Notice that tuple
`{eof, Location}` returned at the end of the file is included as a "form".

If you want to change the file name of the implicit -file() attributes inserted
during preprocessing, you can do with `{source_name, SourceName}`. If unset it
will default to the name of the opened file.

If `extra` is specified in `Options`, the return value is `{ok, [Form], Extra}`
instead of `{ok, [Form]}`.

The option `location` is forwarded to the Erlang token scanner, see
[`erl_scan:tokens/3,4`](`erl_scan:tokens/3`).

The `{compiler_internal,term()}` option is forwarded to the Erlang token
scanner, see [`{compiler_internal,term()}`](`m:erl_scan#compiler_interal`).
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec parse_file(FileName, Options) ->
        {'ok', [Form]} | {'ok', [Form], Extra} | {error, OpenError} when
      FileName :: file:name(),
      Options :: [{'includes', IncludePath :: [DirectoryName :: file:name()]} |
		  {'source_name', SourceName :: file:name()} |
		  {'macros', PredefMacros :: macros()} |
		  {'default_encoding', DefEncoding :: source_encoding()} |
		  {'location',StartLocation :: erl_anno:location()} |
                  {'reserved_word_fun', Fun :: fun((atom()) -> boolean())} |
                  {'features', [Feature :: atom()]} |
		  'extra' |
                  {'compiler_internal', [term()]}],
      Form :: erl_parse:abstract_form()
            | {'error', ErrorInfo}
            | {'eof',Location},
      Location :: erl_anno:location(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      Extra :: [{'encoding', source_encoding() | 'none'}],
      OpenError :: file:posix() | badarg | system_limit.

parse_file(Ifile, Options) ->
    case open([{name, Ifile} | Options]) of
	{ok,Epp} ->
	    Forms = parse_file(Epp),
	    close(Epp),
	    {ok, Forms};
	{ok,Epp,Extra} ->
	    Forms = parse_file(Epp),
            Epp ! {get_features, self()},
            Ftrs = receive {features, X} -> X end,
	    close(Epp),
	    {ok, Forms, [{features, Ftrs} | Extra]};
	{error,E} ->
	    {error,E}
    end.

-doc false.
-spec parse_file(Epp) -> [Form] when
      Epp :: epp_handle(),
      Form :: erl_parse:abstract_form() | {'error', ErrorInfo} |
	      {'warning',WarningInfo} | {'eof',Location},
      Location :: erl_anno:location(),
      ErrorInfo :: erl_scan:error_info() | erl_parse:error_info(),
      WarningInfo :: warning_info().

parse_file(Epp) ->
    case parse_erl_form(Epp) of
	{ok,Form} ->
            [Form|parse_file(Epp)];
	{error,E} ->
	    [{error,E}|parse_file(Epp)];
	{warning,W} ->
	    [{warning,W}|parse_file(Epp)];
	{eof,Location} ->
	    [{eof,Location}]
    end.

-doc "Returns the default encoding of Erlang source files.".
-doc(#{since => <<"OTP R16B">>}).
-spec default_encoding() -> source_encoding().

default_encoding() ->
    ?DEFAULT_ENCODING.

-doc """
Returns a string representation of an encoding. The string is recognized by
[`read_encoding/1,2`](`read_encoding/1`),
[`read_encoding_from_binary/1,2`](`read_encoding_from_binary/1`), and
[`set_encoding/1,2`](`set_encoding/1`) as a valid encoding.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec encoding_to_string(Encoding) -> string() when
      Encoding :: source_encoding().

encoding_to_string(latin1) -> "coding: latin-1";
encoding_to_string(utf8) -> "coding: utf-8".

-doc(#{equiv => read_encoding/2}).
-doc(#{since => <<"OTP R16B">>}).
-spec read_encoding(FileName) -> source_encoding() | none when
      FileName :: file:name().

read_encoding(Name) ->
    read_encoding(Name, []).

-doc """
Read the [encoding](`m:epp#encoding`) from a file. Returns the read encoding, or
`none` if no valid encoding is found.

Option `in_comment_only` is `true` by default, which is correct for Erlang
source files. If set to `false`, the encoding string does not necessarily have
to occur in a comment.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec read_encoding(FileName, Options) -> source_encoding() | none when
      FileName :: file:name(),
      Options :: [Option],
      Option :: {in_comment_only, boolean()}.

read_encoding(Name, Options) ->
    InComment = proplists:get_value(in_comment_only, Options, true),
    case file:open(Name, [read]) of
        {ok,File} ->
            try read_encoding_from_file(File, InComment)
            after ok = file:close(File)
            end;
        _Error ->
            none
    end.

-doc """
Reads the [encoding](`m:epp#encoding`) from an I/O device and sets the encoding
of the device accordingly. The position of the I/O device referenced by `File`
is not affected. If no valid encoding can be read from the I/O device, the
encoding of the I/O device is set to the default encoding.

Returns the read encoding, or `none` if no valid encoding is found.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec set_encoding(File) -> source_encoding() | none when
      File :: io:device(). % pid(); raw files don't work

set_encoding(File) ->
    set_encoding(File, ?DEFAULT_ENCODING).

-doc """
Reads the [encoding](`m:epp#encoding`) from an I/O device and sets the encoding
of the device accordingly. The position of the I/O device referenced by `File`
is not affected. If no valid encoding can be read from the I/O device, the
encoding of the I/O device is set to the [encoding](`m:epp#encoding`) specified
by `Default`.

Returns the read encoding, or `none` if no valid encoding is found.
""".
-doc(#{since => <<"OTP 17.0">>}).
-spec set_encoding(File, Default) -> source_encoding() | none when
      Default :: source_encoding(),
      File :: io:device(). % pid(); raw files don't work

set_encoding(File, Default) ->
    Encoding = read_encoding_from_file(File, true),
    Enc = case Encoding of
              none -> Default;
              Encoding -> Encoding
          end,
    ok = io:setopts(File, [{encoding, Enc}]),
    Encoding.

-doc(#{equiv => read_encoding_from_binary/2}).
-doc(#{since => <<"OTP R16B">>}).
-spec read_encoding_from_binary(Binary) -> source_encoding() | none when
      Binary :: binary().

-define(ENC_CHUNK, 32).
-define(N_ENC_CHUNK, 16). % a total of 512 bytes

read_encoding_from_binary(Binary) ->
    read_encoding_from_binary(Binary, []).

-doc """
Read the [encoding](`m:epp#encoding`) from a binary. Returns the read encoding,
or `none` if no valid encoding is found.

Option `in_comment_only` is `true` by default, which is correct for Erlang
source files. If set to `false`, the encoding string does not necessarily have
to occur in a comment.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec read_encoding_from_binary(Binary, Options) ->
                                       source_encoding() | none when
      Binary :: binary(),
      Options :: [Option],
      Option :: {in_comment_only, boolean()}.

read_encoding_from_binary(Binary, Options) ->
    InComment = proplists:get_value(in_comment_only, Options, true),
    try
        com_nl(Binary, fake_reader(0), 0, InComment)
    catch
        throw:no ->
            none
    end.

fake_reader(N) ->
    fun() when N =:= ?N_ENC_CHUNK ->
            throw(no);
       () ->
            {<<>>, fake_reader(N+1)}
    end.

-spec read_encoding_from_file(File, InComment) -> source_encoding() | none when
      File :: io:device(),
      InComment :: boolean().

read_encoding_from_file(File, InComment) ->
    {ok, Pos0} = file:position(File, cur),
    Opts = io:getopts(File),
    Encoding0 = lists:keyfind(encoding, 1, Opts),
    Binary0 = lists:keyfind(binary, 1, Opts),
    ok = io:setopts(File, [binary, {encoding, latin1}]),
    try
        {B, Fun} = (reader(File, 0))(),
        com_nl(B, Fun, 0, InComment)
    catch
        throw:no ->
            none
    after
        {ok, Pos0} = file:position(File, Pos0),
        ok = io:setopts(File, [Binary0, Encoding0])
    end.

reader(Fd, N) ->
    fun() when N =:= ?N_ENC_CHUNK ->
            throw(no);
       () ->
            case file:read(Fd, ?ENC_CHUNK) of
                eof ->
                    {<<>>, reader(Fd, N+1)};
                {ok, Bin} ->
                    {Bin, reader(Fd, N+1)};
                {error, _} ->
                    throw(no) % ignore errors
            end
    end.

com_nl(_, _, 2, _) ->
    throw(no);
com_nl(B, Fun, N, false=Com) ->
    com_c(B, Fun, N, Com);
com_nl(B, Fun, N, true=Com) ->
    com(B, Fun, N, Com).

com(<<"\n",B/binary>>, Fun, N, Com) ->
    com_nl(B, Fun, N+1, Com);
com(<<"%", B/binary>>, Fun, N, Com) ->
    com_c(B, Fun, N, Com);
com(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
    com(B, Fun, N, Com);
com(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com(B, Fun1, N, Com).

com_c(<<"c",B/binary>>, Fun, N, Com) ->
    com_oding(B, Fun, N, Com);
com_c(<<"\n",B/binary>>, Fun, N, Com) ->
    com_nl(B, Fun, N+1, Com);
com_c(<<_:1/unit:8,B/binary>>, Fun, N, Com) ->
    com_c(B, Fun, N, Com);
com_c(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_c(B, Fun1, N, Com).

com_oding(<<"oding",B/binary>>, Fun, N, Com) ->
    com_sep(B, Fun, N, Com);
com_oding(B, Fun, N, Com) when byte_size(B) >= length("oding") ->
    com_c(B, Fun, N, Com);
com_oding(B, Fun, N, Com) ->
    {B1, Fun1} = Fun(),
    com_oding(list_to_binary([B, B1]), Fun1, N, Com).

com_sep(<<":",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_sep(<<"=",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_sep(<<"\s",B/binary>>, Fun, N, Com) ->
    com_sep(B, Fun, N, Com);
com_sep(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_sep(B, Fun1, N, Com);
com_sep(B, Fun, N, Com) ->
    com_c(B, Fun, N, Com).

com_space(<<"\s",B/binary>>, Fun, N, Com) ->
    com_space(B, Fun, N, Com);
com_space(<<>>, Fun, N, Com) ->
    {B, Fun1} = Fun(),
    com_space(B, Fun1, N, Com);
com_space(B, Fun, N, _Com) ->
    com_enc(B, Fun, N, [], []).

com_enc(<<C:1/unit:8,B/binary>>, Fun, N, L, Ps) when C >= $a, C =< $z;
                                                     C >= $A, C =< $Z;
                                                     C >= $0, C =< $9 ->
    com_enc(B, Fun, N, [C | L], Ps);
com_enc(<<>>, Fun, N, L, Ps) ->
    case Fun() of
        {<<>>, _} ->
            com_enc_end([L | Ps]);
        {B, Fun1} ->
            com_enc(B, Fun1, N, L, Ps)
    end;
com_enc(<<"-",B/binary>>, Fun, N, L, Ps) ->
    com_enc(B, Fun, N, [], [L | Ps]);
com_enc(_B, _Fun, _N, L, Ps) ->
    com_enc_end([L | Ps]).

com_enc_end(Ps0) ->
    Ps = lists:reverse([lists:reverse(lowercase(P)) || P <- Ps0]),
    com_encoding(Ps).

com_encoding(["latin","1"|_]) ->
    latin1;
com_encoding(["utf","8"|_]) ->
    utf8;
com_encoding(_) ->
    throw(no). % Don't try any further

lowercase(S) ->
    unicode:characters_to_list(string:lowercase(S)).

-doc false.
normalize_typed_record_fields([]) ->
    {typed, []};
normalize_typed_record_fields(Fields) ->
    normalize_typed_record_fields(Fields, [], false).

normalize_typed_record_fields([], NewFields, Typed) ->
    case Typed of
	true -> {typed, lists:reverse(NewFields)};
	false -> not_typed
    end;
normalize_typed_record_fields([{typed_record_field,Field,_}|Rest],
			      NewFields, _Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], true);
normalize_typed_record_fields([Field|Rest], NewFields, Typed) ->
    normalize_typed_record_fields(Rest, [Field|NewFields], Typed).

-doc false.
restore_typed_record_fields([]) ->
    [];
restore_typed_record_fields([{attribute,A,record,{Record,_NewFields}},
                             {attribute,A,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    [{attribute,A,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([{attribute,A,type,{{record,Record},Fields,[]}}|
                             Forms]) ->
    %% This clause is due to the compiler's 'E' option.
    %% Record information kept by erl_expand_records.
    [{attribute,A,record,{Record,Fields}}|
     restore_typed_record_fields(Forms)];
restore_typed_record_fields([Form|Forms]) ->
    [Form|restore_typed_record_fields(Forms)].

server(Pid, Name, Options) ->
    process_flag(trap_exit, true),
    St = #epp{},
    case proplists:get_value(fd, Options) of
        undefined ->
            case file:open(Name, [read]) of
                {ok,File} ->
                    init_server(Pid, Name, Options, St#epp{file = File});
                {error,E} ->
                    epp_reply(Pid, {error,E})
            end;
        Fd ->
            init_server(Pid, Name, Options, St#epp{file = Fd, pre_opened=true})
    end.

init_server(Pid, FileName, Options, St0) ->
    SourceName = proplists:get_value(source_name, Options, FileName),
    Pdm = proplists:get_value(macros, Options, []),
    Features = proplists:get_value(features, Options, []),
    Internal = proplists:get_value(compiler_internal, Options, []),
    ParseChecks = proplists:get_bool(ssa_checks, Internal),
    Ms0 = predef_macros(SourceName, Features),
    case user_predef(Pdm, Ms0) of
	{ok,Ms1} ->
            DefEncoding = proplists:get_value(default_encoding, Options,
                                              ?DEFAULT_ENCODING),
            Encoding = set_encoding(St0#epp.file, DefEncoding),
            epp_reply(Pid, {ok,self(),Encoding}),
            %% ensure directory of current source file is
            %% first in path
            Path = [filename:dirname(FileName) |
                    proplists:get_value(includes, Options, [])],
            {ok,{_,ResWordFun0}} =
                erl_features:keyword_fun([], fun erl_scan:f_reserved_word/1),
            ResWordFun =
                proplists:get_value(reserved_word_fun, Options,
                                    ResWordFun0),
            %% the default location is 1 for backwards compatibility, not {1,1}
            AtLocation = proplists:get_value(location, Options, 1),

            Deterministic = proplists:get_value(deterministic, Options, false),
            St = St0#epp{delta=0, name=SourceName, name2=SourceName,
			 path=Path, location=AtLocation, macs=Ms1,
			 default_encoding=DefEncoding,
                         erl_scan_opts =
                             [{text_fun, keep_ftr_keywords()},
                              {reserved_word_fun, ResWordFun}]
                         ++ if ParseChecks ->
                                    [{compiler_internal,[ssa_checks]}];
                               true -> []
                            end,
                         features = Features,
                         else_reserved = ResWordFun('else'),
                         deterministic = Deterministic},
            From = wait_request(St),
            Anno = erl_anno:new(AtLocation),
            enter_file_reply(From, file_name(SourceName), Anno,
			     AtLocation, code, Deterministic),
            wait_req_scan(St);
	{error,E} ->
	    epp_reply(Pid, {error,E})
    end.

%% Return a function that keeps quoted atoms that are keywords in
%% configurable features.  Need in erl_lint to avoid warning about
%% them.
keep_ftr_keywords() ->
    Features = erl_features:configurable(),
    Keywords = lists:flatmap(fun erl_features:keywords/1, Features),
    F = fun(Atom) -> atom_to_list(Atom) ++ "'" end,
    Strings = lists:map(F, Keywords),
    fun(atom, [$'|S]) -> lists:member(S, Strings);
       (_, _) -> false
    end.

%% predef_macros(FileName) -> Macrodict
%%  Initialise the macro dictionary with the default predefined macros,
%%  FILE, LINE, MODULE as undefined, MACHINE and MACHINE value.

predef_macros(File, EnabledFeatures0) ->
    Machine = list_to_atom(erlang:system_info(machine)),
    Anno = line1(),
    OtpVersion = list_to_integer(erlang:system_info(otp_release)),
    AvailableFeatures =
        [Ftr || Ftr <- erl_features:all(),
                maps:get(status, erl_features:info(Ftr)) /= rejected],
    PermanentFeatures =
        [Ftr || Ftr <- erl_features:all(),
                maps:get(status, erl_features:info(Ftr)) == permanent],
    EnabledFeatures = EnabledFeatures0 ++ PermanentFeatures,
    Defs = [{'FILE', 	           {none,[{string,Anno,File}]}},
	    {'FUNCTION_NAME',      undefined},
	    {'FUNCTION_ARITY',     undefined},
	    {'LINE',		   {none,[{integer,Anno,1}]}},
	    {'MODULE',	           undefined},
	    {'MODULE_STRING',      undefined},
	    {'BASE_MODULE',	   undefined},
	    {'BASE_MODULE_STRING', undefined},
	    {'MACHINE',	           {none,[{atom,Anno,Machine}]}},
	    {Machine,	           {none,[{atom,Anno,true}]}},
	    {'OTP_RELEASE',	   {none,[{integer,Anno,OtpVersion}]}},
            {'FEATURE_AVAILABLE',  [ftr_macro(AvailableFeatures)]},
            {'FEATURE_ENABLED', [ftr_macro(EnabledFeatures)]}
	   ],
    maps:from_list(Defs).

%% Make macro definition from a list of features.  The macro takes one
%% argument and returns true when argument is available as a feature.
ftr_macro(Features) ->
    Anno = line1(),
    Arg = 'X',
    Fexp = fun(Ftr) -> [{'(', Anno},
                        {var, Anno, Arg},
                        {')', Anno},
                        {'==', Anno},
                        {atom, Anno, Ftr}]
           end,
    Body =
        case Features of
            [] -> [{atom, Anno, false}];
            [Ftr| Ftrs] ->
                [{'(', Anno}|
                 lists:foldl(fun(F, Expr) ->
                                     Fexp(F) ++ [{'orelse', Anno} | Expr]
                            end,
                            Fexp(Ftr) ++ [{')', Anno}],
                            Ftrs)]
        end,
    {1, {[Arg], Body}}.

%% user_predef(PreDefMacros, Macros) ->
%%	{ok,MacroDict} | {error,E}
%%  Add the predefined macros to the macros dictionary. A macro without a
%%  value gets the value 'true'.

user_predef([{M,Val,redefine}|Pdm], Ms) when is_atom(M) ->
    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
    user_predef(Pdm, Ms#{M=>{none,Exp}});
user_predef([{M,Val}|Pdm], Ms) when is_atom(M) ->
    case Ms of
	#{M:=Defs} when is_list(Defs) ->
	     %% User defined macros.
	    {error,{redefine,M}};
	#{M:=_Defs} ->
	    %% Predefined macros.
	    {error,{redefine_predef,M}};
	_ ->
	    Exp = erl_parse:tokens(erl_parse:abstract(Val)),
	    user_predef(Pdm, Ms#{M=>[{none,{none,Exp}}]})
    end;
user_predef([M|Pdm], Ms) when is_atom(M) ->
    user_predef([{M,true}|Pdm], Ms);
user_predef([Md|_Pdm], _Ms) -> {error,{bad,Md}};
user_predef([], Ms) -> {ok,Ms}.

%% wait_request(EppState) -> RequestFrom
%% wait_req_scan(EppState)
%% wait_req_skip(EppState, SkipIstack)
%%  Handle requests, processing trivial requests directly. Either return
%%  requestor or scan/skip tokens.

wait_request(St) ->
    receive
	{epp_request,From,scan_erl_form} -> From;
        {get_features, From} ->
            From ! {features, St#epp.features},
            wait_request(St);
	{epp_request,From,macro_defs} ->
	    %% Return the old format to avoid any incompability issues.
	    Defs = [{{atom,K},V} || K := V <- St#epp.macs],
	    epp_reply(From, Defs),
	    wait_request(St);
	{epp_request,From,close} ->
	    close_file(St),
	    epp_reply(From, ok),
	    exit(normal);
	{'EXIT',_,R} ->
	    exit(R);
	Other ->
	    io:fwrite("Epp: unknown '~w'\n", [Other]),
	    wait_request(St)
    end.

close_file(#epp{pre_opened = true}) ->
    ok;
close_file(#epp{pre_opened = false, file = File}) ->
    ok = file:close(File).

wait_req_scan(St) ->
    From = wait_request(St),
    scan_toks(From, St).

wait_req_skip(St, Sis) ->
    From = wait_request(St),
    skip_toks(From, St, Sis).

%% enter_file(FileName, IncludeToken, From, EppState)
%% leave_file(From, EppState)
%%  Handle entering and leaving included files. Notify caller when the
%%  current file is changed. Note it is an error to exit a file if we are
%%  in a conditional. These functions never return.

enter_file(_NewName, Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{loc(Inc),epp,{depth,"include"}}}),
    wait_req_scan(St);
enter_file(NewName, Inc, From, St) ->
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
            Loc = start_loc(St#epp.location),
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E} ->
	    epp_reply(From, {error,{loc(Inc),epp,{include,file,NewName}}}),
	    wait_req_scan(St)
    end.

%% enter_file2(File, FullName, From, EppState, AtLocation) -> EppState.
%%  Set epp to use this file and "enter" it.

enter_file2(NewF, Pname, From, St0, AtLocation) ->
    Anno = erl_anno:new(AtLocation),
    enter_file_reply(From, Pname, Anno, AtLocation, code, St0#epp.deterministic),
    #epp{macs = Ms0,
         default_encoding = DefEncoding,
         in_prefix = InPrefix,
         erl_scan_opts = ScanOpts,
         else_reserved = ElseReserved,
         features = Ftrs,
         deterministic = Deterministic} = St0,
    Ms = Ms0#{'FILE':={none,[{string,Anno,source_name(St0,Pname)}]}},
    %% update the head of the include path to be the directory of the new
    %% source file, so that an included file can always include other files
    %% relative to its current location (this is also how C does it); note
    %% that the directory of the parent source file (the previous head of
    %% the path) must be dropped, otherwise the path used within the current
    %% file will depend on the order of file inclusions in the parent files
    Path = [filename:dirname(Pname) | tl(St0#epp.path)],
    _ = set_encoding(NewF, DefEncoding),
    #epp{file=NewF,location=AtLocation,name=Pname,name2=Pname,delta=0,
         sstk=[St0|St0#epp.sstk],path=Path,macs=Ms,
         in_prefix = InPrefix,
         features = Ftrs,
         erl_scan_opts = ScanOpts,
         else_reserved = ElseReserved,
         default_encoding=DefEncoding,
         deterministic=Deterministic}.

enter_file_reply(From, Name, LocationAnno, AtLocation, Where, Deterministic) ->
    Anno0 = erl_anno:new(AtLocation),
    Anno = case Where of
               code -> Anno0;
               generated -> erl_anno:set_generated(true, Anno0)
           end,
    Rep = {ok, [{'-',Anno},{atom,Anno,file},{'(',Anno},
		{string,Anno,source_name(Deterministic,Name)},{',',Anno},
		{integer,Anno,get_line(LocationAnno)},{')',LocationAnno},
                {dot,Anno}]},
    epp_reply(From, Rep).

%% Flatten filename to a string. Must be a valid filename.

file_name([C | T]) when is_integer(C), C > 0 ->
    [C | file_name(T)];
file_name([H|T]) ->
    file_name(H) ++ file_name(T);
file_name([]) ->
    [];
file_name(N) when is_atom(N) ->
    atom_to_list(N).

leave_file(From, St) ->
    case St#epp.istk of
	[I|Cis] ->
	    epp_reply(From,
		      {error,{St#epp.location,epp,
                              {illegal,"unterminated",I}}}),
	    leave_file(wait_request(St),St#epp{istk=Cis});
	[] ->
	    case St#epp.sstk of
		[OldSt|Sts] ->
		    close_file(St),
                    #epp{location=OldLoc, delta=Delta, name=OldName,
                         name2=OldName2} = OldSt,
                    CurrLoc = add_line(OldLoc, Delta),
                    Anno = erl_anno:new(CurrLoc),
		    Ms0 = St#epp.macs,
                    InPrefix = St#epp.in_prefix,
                    Ftrs = St#epp.features,
                    ElseReserved = St#epp.else_reserved,
                    ScanOpts = St#epp.erl_scan_opts,
		    Ms = Ms0#{'FILE':={none,[{string,Anno,source_name(St,OldName2)}]}},
                    NextSt = OldSt#epp{sstk=Sts,macs=Ms,uses=St#epp.uses,
                                       in_prefix = InPrefix,
                                       features = Ftrs,
                                       else_reserved = ElseReserved,
                                       erl_scan_opts = ScanOpts},
		    enter_file_reply(From, OldName, Anno, CurrLoc, code, St#epp.deterministic),
                    case OldName2 =:= OldName of
                        true ->
                            ok;
                        false ->
                            NFrom = wait_request(NextSt),
                            OldAnno = erl_anno:new(OldLoc),
                            enter_file_reply(NFrom, OldName2, OldAnno,
                                             CurrLoc, generated, St#epp.deterministic)
                        end,
                    wait_req_scan(NextSt);
		[] ->
		    epp_reply(From, {eof,St#epp.location}),
		    wait_req_scan(St)
	    end
    end.

%% scan_toks(From, EppState)
%% scan_toks(Tokens, From, EppState)

scan_toks(From, St) ->
    #epp{file = File, location = Loc, erl_scan_opts = ScanOpts} = St,
    case io:scan_erl_form(File, '', Loc, ScanOpts) of
        {ok,Toks,Cl} ->
            scan_toks(Toks, From, St#epp{location=Cl});
        {error,E,Cl} ->
            epp_reply(From, {error,E}),
            wait_req_scan(St#epp{location=Cl});
        {eof,Cl} ->
            leave_file(From, St#epp{location=Cl});
        {error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
            leave_file(wait_request(St), St)	%This serious, just exit!
    end.

scan_toks([{'-',_Lh},{atom,_Ld,feature}=Feature|Toks], From, St) ->
    scan_feature(Toks, Feature, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,define}=Define|Toks], From, St) ->
    scan_define(Toks, Define, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,undef}=Undef|Toks], From, St) ->
    scan_undef(Toks, Undef, From, leave_prefix(St));
scan_toks([{'-',_Lh},{atom,_Ld,error}=Error|Toks], From, St) ->
    scan_err_warn(Toks, Error, From, leave_prefix(St));
scan_toks([{'-',_Lh},{atom,_Ld,warning}=Warn|Toks], From, St) ->
    scan_err_warn(Toks, Warn, From, leave_prefix(St));
scan_toks([{'-',_Lh},{atom,_Li,include}=Inc|Toks], From, St) ->
    scan_include(Toks, Inc, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,D}=Doc | [{'(', _},{'{',_} | _] = Toks], From, St)
  when D =:= doc; D =:= moduledoc ->
    scan_filedoc(coalesce_strings(Toks), Doc, From, St);
scan_toks([{'-',_Lh},{atom,_Ld,D}=Doc | [{'{',_} | _] = Toks], From, St)
  when D =:= doc; D =:= moduledoc ->
    scan_filedoc(coalesce_strings(Toks), Doc, From, St);
scan_toks([{'-',_Lh},{atom,_Li,include_lib}=IncLib|Toks], From, St) ->
    scan_include_lib(Toks, IncLib, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifdef}=IfDef|Toks], From, St) ->
    scan_ifdef(Toks, IfDef, From, St);
scan_toks([{'-',_Lh},{atom,_Li,ifndef}=IfnDef|Toks], From, St) ->
    scan_ifndef(Toks, IfnDef, From, St);
scan_toks([{'-',_Lh},{atom,_Le,'else'}=Else|Toks], From, St) ->
    scan_else(Toks, Else, From, St);
%% conditionally allow else as a keyword
scan_toks([{'-',_Lh},{'else',_Le}=Else|Toks], From, St)
  when St#epp.else_reserved ->
    scan_else(Toks, Else, From, St);
scan_toks([{'-',_Lh},{'if',_Le}=If|Toks], From, St) ->
    scan_if(Toks, If, From, St);
scan_toks([{'-',_Lh},{atom,_Le,elif}=Elif|Toks], From, St) ->
    scan_elif(Toks, Elif, From, St);
scan_toks([{'-',_Lh},{atom,_Le,endif}=Endif|Toks], From, St) ->
    scan_endif(Toks, Endif, From, St);
scan_toks([{'-',_Lh},{atom,_Lf,file}=FileToken|Toks0], From, St) ->
    case catch expand_macros(Toks0, St) of
	Toks1 when is_list(Toks1) ->
            scan_file(Toks1, FileToken, From, St);
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end;
scan_toks(Toks0, From, St) ->
    case catch expand_macros(Toks0, St#epp{fname=Toks0}) of
	Toks1 when is_list(Toks1) ->
            InPrefix =
                St#epp.in_prefix
                andalso case Toks1 of
                            [] -> true;
                            [{'-', _Loc}, Tok | _] ->
                                in_prefix(Tok);
                            _ ->
                                false
                        end,
	    epp_reply(From, {ok,Toks1}),
	    wait_req_scan(St#epp{in_prefix = InPrefix,
                                 macs=scan_module(Toks1, St#epp.macs)});
	{error,ErrL,What} ->
	    epp_reply(From, {error,{ErrL,epp,What}}),
	    wait_req_scan(St)
    end.

%% First we parse either ({file, "filename"}) or {file, "filename"} and
%% return proper errors if syntax is incorrect. Only literal strings are allowed.
scan_filedoc([{'(', _},{'{',_}, {atom, _,file},
              {',', _}, {string, _, _} = DocFilename,
              {'}', _},{')',_},{dot,_} = Dot], DocType, From, St) ->
    scan_filedoc_content(DocFilename, Dot, DocType, From, St);
scan_filedoc([{'(', _},{'{',_}, {atom, _,file} | _] = Toks, DocType, From, St) ->
    T = find_mismatch(['(','{',atom,',',string,'}',')',dot], Toks, DocType),
    epp_reply(From, {error,{loc(T),epp,{bad,DocType}}}),
    wait_req_scan(St);
scan_filedoc([{'(', _},{'{',_}, T | _], DocType, From, St) ->
    epp_reply(From, {error,{loc(T),epp,{DocType, invalid, file}}}),
    wait_req_scan(St);
scan_filedoc([{'{',_}, {atom, _,file},
              {',', _}, {string, _, _} = DocFilename,
              {'}', _},{dot,_} = Dot], DocType, From, St) ->
    scan_filedoc_content(DocFilename, Dot, DocType, From, St);
scan_filedoc([{'{',_}, {atom, _,file} | _] = Toks, {atom,_,DocType}, From, St) ->
    T = find_mismatch(['{',{atom, file},',',string,'}',dot], Toks, DocType),
    epp_reply(From, {error,{loc(T),epp,{bad,DocType}}}),
    wait_req_scan(St);
scan_filedoc([{'{',_}, T | _], {atom,_,DocType}, From, St) ->
    epp_reply(From, {error,{loc(T),epp,{DocType, invalid, file}}}),
    wait_req_scan(St).

%% Reads the content of the file and rewrites the AST as if
%% the content had been written in-place.
scan_filedoc_content({string, _A, DocFilename}, Dot,
                     {atom,DocLoc,Doc}, From, #epp{name = CurrentFilename} = St) ->
    %% The head of the path is the dir where the current file is
    Cwd = hd(St#epp.path),
    case file:path_open([Cwd], DocFilename, [read, binary]) of
        {ok, NewF, Pname} ->
            case file:read_file_info(NewF) of
                {ok, #file_info{ size = Sz }} ->
                    {ok, Bin} = file:read(NewF, Sz),
                    ok = file:close(NewF),
                    StartLoc = start_loc(St#epp.location),
                    %% Enter a new file for this doc entry
                    enter_file_reply(From, Pname, erl_anno:new(StartLoc), StartLoc,
                                     code, St#epp.deterministic),
                    epp_reply(From, {ok,
                                     [{'-',StartLoc}, {atom, StartLoc, Doc}]
                                     ++ [{string, StartLoc, unicode:characters_to_list(Bin)}, {dot,StartLoc}]}),
                    %% Restore the previous file
                    enter_file_reply(From, CurrentFilename,
                                     erl_anno:new(loc(Dot)), loc(Dot), code,
                                     St#epp.deterministic),
                    wait_req_scan(St);
                {error, _} ->
                    ok = file:close(NewF),
                    epp_reply(From, {error,{DocLoc,epp,{Doc, file, DocFilename}}}),
                    wait_req_scan(St)
            end;
        {error, enoent} ->
            epp_reply(From, {warning,{DocLoc,epp,{Doc, file, DocFilename}}}),
            epp_reply(From, {ok,
                             [{'-',DocLoc}, {atom, DocLoc, Doc}]
                             ++ [{string, DocLoc, ""}, {dot,DocLoc}]}),
            wait_req_scan(St);
        {error, _} ->
            epp_reply(From, {error,{DocLoc,epp,{Doc, file, DocFilename}}}),
            wait_req_scan(St)
    end.

%% Determine whether we have passed the prefix where a -feature
%% directive is allowed.
in_prefix({atom, _, Atom}) ->
    %% These directives are allowed inside the prefix
    lists:member(Atom, ['module', 'feature',
                        'if', 'else', 'elif', 'endif', 'ifdef', 'ifndef',
                        'define', 'undef', 'include', 'include_lib',
                        'moduledoc', 'doc']);
in_prefix(_T) ->
    false.

leave_prefix(#epp{} = St) ->
    St#epp{in_prefix = false}.

scan_module([{'-',_Ah},{atom,_Am,module},{'(',_Al}|Ts], Ms) ->
    scan_module_1(Ts, Ms);
scan_module([{'-',_Ah},{atom,_Am,extends},{'(',_Al}|Ts], Ms) ->
    scan_extends(Ts, Ms);
scan_module(_Ts, Ms) -> Ms.

scan_module_1([{atom,_,_}=A,{',',Anno}|Ts], Ms) ->
    %% Parameterized modules.
    scan_module_1([A,{')',Anno}|Ts], Ms);
scan_module_1([{atom,Anno,A}=ModAtom,{')',_Ar}|_Ts], Ms0) ->
    ModString = atom_to_list(A),
    Ms = Ms0#{'MODULE':={none,[ModAtom]}},
    Ms#{'MODULE_STRING':={none,[{string,Anno,ModString}]}};
scan_module_1(_Ts, Ms) -> Ms.

scan_extends([{atom,Anno,A}=ModAtom,{')',_Ar}|_Ts], Ms0) ->
    ModString = atom_to_list(A),
    Ms = Ms0#{'BASE_MODULE':={none,[ModAtom]}},
    Ms#{'BASE_MODULE_STRING':={none,[{string,Anno,ModString}]}};
scan_extends(_Ts, Ms) -> Ms.

scan_err_warn([{'(',_}|_]=Toks0, {atom,_,Tag}=Token, From, St) ->
    try expand_macros(Toks0, St) of
	Toks when is_list(Toks) ->
	    case erl_parse:parse_term(Toks) of
		{ok,Term} ->
		    epp_reply(From, {Tag,{loc(Token),epp,{Tag,Term}}});
		{error,_} ->
		    epp_reply(From, {error,{loc(Token),epp,{bad,Tag}}})
	    end
    catch
	_:_ ->
	    epp_reply(From, {error,{loc(Token),epp,{bad,Tag}}})
    end,
    wait_req_scan(St);
scan_err_warn(Toks, {atom,_,Tag}=Token, From, St) ->
    T = no_match(Toks, Token),
    epp_reply(From, {error,{loc(T),epp,{bad,Tag}}}),
    wait_req_scan(St).

%% scan a feature directive
scan_feature([{'(', _Ap}, {atom, _Am, Ftr},
              {',', _}, {atom, _, Ind}, {')', _}, {dot, _}],
             Feature, From, St)
  when St#epp.in_prefix,
       (Ind =:= enable
        orelse Ind =:= disable) ->
    case update_features(St, Ind, Ftr, loc(Feature)) of
        {ok, St1} ->
            scan_toks(From, St1);
        {error, {{Mod, Reason}, ErrLoc}} ->
            epp_reply(From, {error, {ErrLoc, Mod, Reason}}),
            wait_req_scan(St)
    end;
scan_feature([{'(', _Ap}, {atom, _Am, _Ind},
              {',', _}, {atom, _, _Ftr}, {')', _}, {dot, _}| _Toks],
             Feature, From, St) when not St#epp.in_prefix ->
    epp_reply(From, {error, {loc(Feature), epp,
                             ftr_after_prefix}}),
    wait_req_scan(St);
scan_feature(Toks, {atom, _, Tag} = Token, From, St) ->
    T = no_match(Toks, Token),
    epp_reply(From, {error,{loc(T),epp,{bad,Tag}}}),
    wait_req_scan(St).

update_features(St0, Ind, Ftr, Loc) ->
    Ftrs0 = St0#epp.features,
    ScanOpts0 = St0#epp.erl_scan_opts,
    KeywordFun =
        case proplists:get_value(reserved_word_fun, ScanOpts0) of
            undefined -> fun erl_scan:f_reserved_word/1;
            Fun -> Fun
        end,
    case erl_features:keyword_fun(Ind, Ftr, Ftrs0, KeywordFun) of
        {error, Reason} ->
            {error, {Reason, Loc}};
        {ok, {Ftrs1, ResWordFun1}} ->
            Macs0 = St0#epp.macs,
            Macs1 = Macs0#{'FEATURE_ENABLED' => [ftr_macro(Ftrs1)]},
            ScanOpts1 = proplists:delete(reserved_word_fun, ScanOpts0),
            St = St0#epp{erl_scan_opts =
                             [{reserved_word_fun, ResWordFun1}| ScanOpts1],
                         features = Ftrs1,
                         else_reserved = ResWordFun1('else'),
                         macs = Macs1},
            {ok, St}
    end.

%% scan_define(Tokens, DefineToken, From, EppState)

scan_define([{'(',_Ap},{Type,_Am,_}=Mac|Toks], Def, From, St)
  when Type =:= atom; Type =:= var ->
    scan_define_1(Toks, Mac, Def, From, St);
scan_define(Toks, Def, From, St) ->
    T = find_mismatch(['(', var_or_atom], Toks, Def),
    epp_reply(From, {error,{loc(T),epp,{bad,define}}}),
    wait_req_scan(St).

scan_define_1([{',',_}=Comma|Toks], Mac,_Def, From, St) ->
    case catch macro_expansion(Toks, Comma) of
        Expansion when is_list(Expansion) ->
	    scan_define_2(none, {none,Expansion}, Mac, From, St);
        {error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St)
    end;
scan_define_1([{'(',_Ac}=T|Toks], Mac, _Def, From, St) ->
    case catch macro_pars(Toks, [], T) of
        {ok,{As,_}=MacroDef} ->
            Len = length(As),
	    scan_define_2(Len, MacroDef, Mac, From, St);
	{error,ErrL,What} ->
            epp_reply(From, {error,{ErrL,epp,What}}),
            wait_req_scan(St)
    end;
scan_define_1(Toks, _Mac, Def, From, St) ->
    T = no_match(Toks, Def),
    epp_reply(From, {error,{loc(T),epp,{bad,define}}}),
    wait_req_scan(St).

scan_define_2(Arity, Def, {_,_,Key}=Mac, From, #epp{macs=Ms}=St) ->
    case Ms of
	#{Key:=Defs} when is_list(Defs) ->
	    %% User defined macros: can be overloaded
	    case proplists:is_defined(Arity, Defs) of
		true ->
		    epp_reply(From, {error,{loc(Mac),epp,{redefine,Key}}}),
		    wait_req_scan(St);
		false ->
		    scan_define_cont(From, St, Key, Defs, Arity, Def)
	    end;
	#{Key:=_} ->
	    %% Predefined macros: cannot be overloaded
	    epp_reply(From, {error,{loc(Mac),epp,{redefine_predef,Key}}}),
	    wait_req_scan(St);
	_ ->
	    scan_define_cont(From, St, Key, [], Arity, Def)
    end.

%%% Detection of circular macro expansions (which would either keep
%%% the compiler looping forever, or run out of memory):
%%% When a macro is defined, we store the names of other macros it
%%% uses in St#epp.uses. If any macro is undef'ed, that information
%%% becomes invalid, so we redo it for all remaining macros.
%%% The circularity detection itself is done when a macro is expanded:
%%% the information from St#epp.uses is traversed, and if a circularity
%%% is detected, an error message is thrown.

scan_define_cont(F, #epp{macs=Ms0}=St, M, Defs, Arity, Def) ->
    Ms = Ms0#{M=>[{Arity,Def}|Defs]},
    try macro_uses(Def) of
        U ->
	    Uses0 = St#epp.uses,
	    Val = [{Arity,U}|case Uses0 of
				 #{M:=UseList} -> UseList;
				 _ -> []
			     end],
	    Uses = Uses0#{M=>Val},
            scan_toks(F, St#epp{uses=Uses,macs=Ms})
    catch
        {error, Location, Reason} ->
            epp_reply(F, {error,{Location,epp,Reason}}),
            wait_req_scan(St)
    end.

macro_uses({_Args, Tokens}) ->
    Uses0 = macro_ref(Tokens),
    lists:usort(Uses0).

macro_ref([]) ->
    [];
macro_ref([{'?', _}, {'?', _} | Rest]) ->
    macro_ref(Rest);
macro_ref([{'?', _}, {atom, _, A}=Atom | Rest]) ->
    Lm = loc(Atom),
    Arity = count_args(Rest, Lm, A),
    [{A,Arity} | macro_ref(Rest)];
macro_ref([{'?', _}, {var, _, A}=Var | Rest]) ->
    Lm = loc(Var),
    Arity = count_args(Rest, Lm, A),
    [{A,Arity} | macro_ref(Rest)];
macro_ref([_Token | Rest]) ->
    macro_ref(Rest).

%% scan_undef(Tokens, UndefToken, From, EppState)

scan_undef([{'(',_Alp},{atom,_Am,M},{')',_Arp},{dot,_Ad}], _Undef, From, St) ->
    Macs = maps:remove(M, St#epp.macs),
    Uses = maps:remove(M, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef([{'(',_Alp},{var,_Am,M},{')',_Arp},{dot,_Ad}], _Undef, From,St) ->
    Macs = maps:remove(M, St#epp.macs),
    Uses = maps:remove(M, St#epp.uses),
    scan_toks(From, St#epp{macs=Macs, uses=Uses});
scan_undef(Toks, Undef, From, St) ->
    T = find_mismatch(['(',var_or_atom,')',dot], Toks, Undef),
    epp_reply(From, {error,{loc(T),epp,{bad,undef}}}),
    wait_req_scan(St).

%% scan_include(Tokens, IncludeToken, From, St)

scan_include(Tokens0, Inc, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_include1(Tokens, Inc, From, St).

scan_include1([{'(',_Alp},{string,_Af,NewName0}=StringT,{')',_Arp},{dot,_Ad}],
              _Inc, From, St) ->
    NewName = expand_var(NewName0),
    enter_file(NewName, StringT, From, St);
scan_include1(Toks, Inc, From, St) ->
    T = find_mismatch(['(',string,')',dot], Toks, Inc),
    epp_reply(From, {error,{loc(T),epp,{bad,include}}}),
    wait_req_scan(St).

%% scan_include_lib(Tokens, IncludeToken, From, EppState)
%%  For include_lib we first test if we can find the file through the
%%  normal search path, if not we assume that the first directory name
%%  is a library name, find its true directory and try with that.

expand_lib_dir(Name) ->
    try
	[App|Path] = filename:split(Name),
	LibDir = code:lib_dir(list_to_atom(App)),
	{ok,fname_join([LibDir|Path])}
    catch
	_:_ ->
	    error
    end.

scan_include_lib(Tokens0, Inc, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_include_lib1(Tokens, Inc, From, St).

scan_include_lib1([{'(',_Alp},{string,_Af,_NewName0},{')',_Arp},{dot,_Ad}],
                  Inc, From, St)
  when length(St#epp.sstk) >= 8 ->
    epp_reply(From, {error,{loc(Inc),epp,{depth,"include_lib"}}}),
    wait_req_scan(St);
scan_include_lib1([{'(',_Alp},{string,_Af,NewName0}=N,{')',_Arp},{dot,_Ad}],
                  _Inc, From, St) ->
    NewName = expand_var(NewName0),
    Loc = start_loc(St#epp.location),
    case file:path_open(St#epp.path, NewName, [read]) of
	{ok,NewF,Pname} ->
	    wait_req_scan(enter_file2(NewF, Pname, From, St, Loc));
	{error,_E1} ->
	    case expand_lib_dir(NewName) of
		{ok,Header} ->
		    case file:open(Header, [read]) of
			{ok,NewF} ->
			    wait_req_scan(enter_file2(NewF, Header, From,
                                                      St, Loc));
			{error,_E2} ->
			    epp_reply(From,
				      {error,{loc(N),epp,
                                              {include,lib,NewName}}}),
			    wait_req_scan(St)
		    end;
		error ->
		    epp_reply(From, {error,{loc(N),epp,
                                            {include,lib,NewName}}}),
		    wait_req_scan(St)
	    end
    end;
scan_include_lib1(Toks, Inc, From, St) ->
    T = find_mismatch(['(',string,')',dot], Toks, Inc),
    epp_reply(From, {error,{loc(T),epp,{bad,include_lib}}}),
    wait_req_scan(St).

%% scan_ifdef(Tokens, IfdefToken, From, EppState)
%% scan_ifndef(Tokens, IfdefToken, From, EppSate)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if[n]def test and then treat as undefined macro.

scan_ifdef([{'(',_Alp},{atom,_Am,M},{')',_Arp},{dot,_Ad}], _IfD, From, St) ->
    case is_macro_defined(M, St) of
        true ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
        false ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef([{'(',_Alp},{var,_Am,M},{')',_Arp},{dot,_Ad}], _IfD, From, St) ->
    case is_macro_defined(M, St) of
        true ->
	    scan_toks(From, St#epp{istk=[ifdef|St#epp.istk]});
        false ->
	    skip_toks(From, St, [ifdef])
    end;
scan_ifdef(Toks, IfDef, From, St) ->
    T = find_mismatch(['(',var_or_atom,')',dot], Toks, IfDef),
    epp_reply(From, {error,{loc(T),epp,{bad,ifdef}}}),
    wait_req_skip(St, [ifdef]).

scan_ifndef([{'(',_Alp},{atom,_Am,M},{')',_Arp},{dot,_Ad}], _IfnD, From, St) ->
    case is_macro_defined(M, St) of
        true ->
	    skip_toks(From, St, [ifndef]);
        false ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef([{'(',_Alp},{var,_Am,M},{')',_Arp},{dot,_Ad}], _IfnD, From, St) ->
    case is_macro_defined(M, St) of
        true ->
	    skip_toks(From, St, [ifndef]);
        false ->
	    scan_toks(From, St#epp{istk=[ifndef|St#epp.istk]})
    end;
scan_ifndef(Toks, IfnDef, From, St) ->
    T = find_mismatch(['(',var_or_atom,')',dot], Toks, IfnDef),
    epp_reply(From, {error,{loc(T),epp,{bad,ifndef}}}),
    wait_req_skip(St, [ifndef]).

is_macro_defined(Name, #epp{macs=Macs}) ->
    case Macs of
        #{Name := undefined} -> false;
        #{Name := _Def} -> true;
        #{} -> false
    end.

%% scan_else(Tokens, ElseToken, From, EppState)
%%  If we are in an if body then convert to else and skip, if we are in an
%%  else or not in anything report an error.

scan_else([{dot,_Ad}], Else, From, St) ->
    case St#epp.istk of
	['else'|Cis] ->
	    epp_reply(From, {error,{loc(Else),
                                    epp,{illegal,"repeated",'else'}}}),
	    wait_req_skip(St#epp{istk=Cis}, ['else']);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, ['else']);
	[] ->
	    epp_reply(From, {error,{loc(Else),epp,
                                    {illegal,"unbalanced",'else'}}}),
	    wait_req_scan(St)
    end;
scan_else(Toks, Else, From, St) ->
    T = no_match(Toks, Else),
    epp_reply(From, {error,{loc(T),epp,{bad,'else'}}}),
    wait_req_scan(St).

%% scan_if(Tokens, IfToken, From, EppState)
%%  Handle the conditional parsing of a file.

scan_if([{'(',_}|_]=Toks, If, From, St) ->
    try eval_if(Toks, St) of
	true ->
	    scan_toks(From, St#epp{istk=['if'|St#epp.istk]});
	_ ->
	    skip_toks(From, St, ['if'])
    catch
	throw:Error0 ->
	    Error = case Error0 of
			{_,erl_parse,_} ->
			    {error,Error0};
                        {error,ErrL,What} ->
                            {error,{ErrL,epp,What}};
			_ ->
			    {error,{loc(If),epp,Error0}}
		    end,
	    epp_reply(From, Error),
	    wait_req_skip(St, ['if'])
    end;
scan_if(Toks, If, From, St) ->
    T = no_match(Toks, If),
    epp_reply(From, {error,{loc(T),epp,{bad,'if'}}}),
    wait_req_skip(St, ['if']).

eval_if(Toks0, St) ->
    Toks = expand_macros(Toks0, St),
    Es1 = case erl_parse:parse_exprs(Toks) of
	      {ok,Es0} -> Es0;
	      {error,E} -> throw(E)
	  end,
    Es = evaluate_builtins(Es1, St),
    assert_guard_expr(Es),
    Bs = erl_eval:new_bindings(),
    LocalFun = fun(_Name, _Args) ->
		       error(badarg)
	       end,
    try erl_eval:exprs(Es, Bs, {value,LocalFun}) of
	{value,Res,_} ->
	    Res
    catch
	_:_ ->
	    false
    end.

assert_guard_expr([E]) ->
    case erl_lint:is_guard_expr(E) of
	false ->
	    throw({bad,'if'});
	true ->
	    ok
    end;
assert_guard_expr(_) ->
    throw({bad,'if'}).

%% evaluate_builtins(AbstractForm0, #epp{}) -> AbstractForm.
%%   Evaluate call to special functions for the preprocessor.
evaluate_builtins({call,_,{atom,_,defined},[N0]}, #epp{macs=Macs}) ->
    %% Evaluate defined(Symbol).
    N = case N0 of
	    {var,_,N1} -> N1;
	    {atom,_,N1} -> N1;
	    _ -> throw({bad,'if'})
	end,
    {atom,erl_anno:new(0),maps:is_key(N, Macs)};
evaluate_builtins([H|T], St) ->
    [evaluate_builtins(H, St)|evaluate_builtins(T, St)];
evaluate_builtins(Tuple, St) when is_tuple(Tuple) ->
    list_to_tuple(evaluate_builtins(tuple_to_list(Tuple), St));
evaluate_builtins(Other, _) ->
    Other.

%% scan_elif(Tokens, EndifToken, From, EppState)
%%  Handle the conditional parsing of a file.
%%  Report a badly formed if test and then treat as false macro.

scan_elif(_Toks, Elif, From, St) ->
    case St#epp.istk of
	['else'|Cis] ->
	    epp_reply(From, {error,{loc(Elif),
                                    epp,{illegal,"unbalanced",'elif'}}}),
	    wait_req_skip(St#epp{istk=Cis}, ['else']);
	[_I|Cis] ->
	    skip_toks(From, St#epp{istk=Cis}, ['elif']);
	[] ->
	    epp_reply(From, {error,{loc(Elif),epp,
                                    {illegal,"unbalanced",elif}}}),
	    wait_req_scan(St)
    end.

%% scan_endif(Tokens, EndifToken, From, EppState)
%%  If we are in an if body then exit it, else report an error.

scan_endif([{dot,_Ad}], Endif, From, St) ->
    case St#epp.istk of
	[_I|Cis] ->
	    scan_toks(From, St#epp{istk=Cis});
	[] ->
	    epp_reply(From, {error,{loc(Endif),epp,
                                    {illegal,"unbalanced",endif}}}),
	    wait_req_scan(St)
    end;
scan_endif(Toks, Endif, From, St) ->
    T = no_match(Toks, Endif),
    epp_reply(From, {error,{loc(T),epp,{bad,endif}}}),
    wait_req_scan(St).

%% scan_file(Tokens, FileToken, From, EppState)
%%  Set the current file and line to the given file and line.
%%  Note that the line of the attribute itself is kept.

scan_file(Tokens0, Tf, From, St) ->
    Tokens = coalesce_strings(Tokens0),
    scan_file1(Tokens, Tf, From, St).

scan_file1([{'(',_Alp},{string,_As,Name},{',',_Ac},{integer,_Ai,Ln},{')',_Arp},
            {dot,_Ad}], Tf, From, St) ->
    Anno = erl_anno:new(Ln),
    enter_file_reply(From, Name, Anno, loc(Tf), generated, St#epp.deterministic),
    Ms0 = St#epp.macs,
    Ms = Ms0#{'FILE':={none,[{string,line1(),source_name(St,Name)}]}},
    Locf = loc(Tf),
    NewLoc = new_location(Ln, St#epp.location, Locf),
    Delta = get_line(element(2, Tf))-Ln + St#epp.delta,
    wait_req_scan(St#epp{name2=Name,location=NewLoc,delta=Delta,macs=Ms});
scan_file1(Toks, Tf, From, St) ->
    T = find_mismatch(['(', string, ',', integer, ')', dot], Toks, Tf),
    epp_reply(From, {error,{loc(T),epp,{bad,file}}}),
    wait_req_scan(St).

new_location(Ln, Le, Lf) when is_integer(Lf) ->
    Ln+(Le-Lf);
new_location(Ln, {Le,_}, {Lf,_}) ->
    {Ln+(Le-Lf),1}.

%% skip_toks(From, EppState, SkipIstack)
%%  Skip over forms until current conditional has been exited. Handle
%%  nested conditionals and repeated 'else's.

skip_toks(From, St, [I|Sis]) ->
    ElseReserved = St#epp.else_reserved,
    case io:scan_erl_form(St#epp.file, '', St#epp.location, St#epp.erl_scan_opts) of
	{ok,[{'-',_Ah},{atom,_Ai,ifdef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifdef,I|Sis]);
	{ok,[{'-',_Ah},{atom,_Ai,ifndef}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [ifndef,I|Sis]);
	{ok,[{'-',_Ah},{'if',_Ai}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, ['if',I|Sis]);
	{ok,[{'-',_Ah},{atom,_Ae,'else'}=Else|_Toks],Cl}->
	    skip_else(Else, From, St#epp{location=Cl}, [I|Sis]);
        %% conditionally allow else as reserved word
	{ok,[{'-',_Ah},{'else',_Ae}=Else|_Toks],Cl} when ElseReserved ->
	    skip_else(Else, From, St#epp{location=Cl}, [I|Sis]);
	{ok,[{'-',_Ah},{atom,_Ae,'elif'}=Elif|Toks],Cl}->
	    skip_elif(Toks, Elif, From, St#epp{location=Cl}, [I|Sis]);
	{ok,[{'-',_Ah},{atom,_Ae,endif}|_Toks],Cl} ->
	    skip_toks(From, St#epp{location=Cl}, Sis);
	{ok,_Toks,Cl} ->
	    skip_toks(From, St#epp{location=Cl}, [I|Sis]);
	{error,E,Cl} ->
	    case E of
		{_,file_io_server,invalid_unicode} ->
		    %% The compiler needs to know that there was
		    %% invalid unicode characters in the file
		    %% (and there is no point in continuing anyway
		    %% since io server process has terminated).
		    epp_reply(From, {error,E}),
		    leave_file(wait_request(St), St);
		_ ->
		    %% Some other invalid token, such as a bad floating
		    %% point number. Just ignore it.
		    skip_toks(From, St#epp{location=Cl}, [I|Sis])
	    end;
	{eof,Cl} ->
	    leave_file(From, St#epp{location=Cl,istk=[I|Sis]});
	{error,_E} ->
            epp_reply(From, {error,{St#epp.location,epp,cannot_parse}}),
	    leave_file(wait_request(St), St)	%This serious, just exit!
    end;
skip_toks(From, St, []) ->
    scan_toks(From, St).

skip_else(Else, From, St, ['else'|Sis]) ->
    epp_reply(From, {error,{loc(Else),epp,{illegal,"repeated",'else'}}}),
    wait_req_skip(St, ['else'|Sis]);
skip_else(_Else, From, St, ['elif'|Sis]) ->
    skip_toks(From, St, ['else'|Sis]);
skip_else(_Else, From, St, [_I]) ->
    scan_toks(From, St#epp{istk=['else'|St#epp.istk]});
skip_else(_Else, From, St, Sis) ->
    skip_toks(From, St, Sis).

skip_elif(_Toks, Elif, From, St, ['else'|_]=Sis) ->
    epp_reply(From, {error,{loc(Elif),epp,elif_after_else}}),
    wait_req_skip(St, Sis);
skip_elif(Toks, Elif, From, St, [_I]) ->
    scan_if(Toks, Elif, From, St);
skip_elif(_Toks, _Elif, From, St, Sis) ->
    skip_toks(From, St, Sis).

%% macro_pars(Tokens, ArgStack, Token)
%% macro_expansion(Tokens, Token)
%%  Extract the macro parameters and the expansion from a macro definition.

macro_pars([{')',_Ap}=Par|Ex], Args, _T0) ->
    {ok, {lists:reverse(Args), macro_pars_end(Ex, Par)}};
macro_pars([{var,_,Name}=T|Ex], Args, _T0) ->
    check_macro_arg(Name, Args, T),
    macro_pars_cont(Ex, [Name|Args], T);
macro_pars(Toks, _Args, T0) ->
    T = no_match(Toks, T0),
    throw({error,loc(T),{bad,define}}).

macro_pars_cont([{')',_Ap}=Par|Ex], Args, _T0) ->
    {ok, {lists:reverse(Args), macro_pars_end(Ex, Par)}};
macro_pars_cont([{',',_Ad},{var,_,Name}=T|Ex], Args, _T0) ->
    check_macro_arg(Name, Args, T),
    macro_pars_cont(Ex, [Name|Args], T);
macro_pars_cont(Toks, _Args, T0) ->
    T = no_match(Toks, T0),
    throw({error,loc(T),{bad,define}}).

macro_pars_end([{',',_Ad}=Comma|Ex], _T0) ->
    macro_expansion(Ex, Comma);
macro_pars_end(Toks, T0) ->
    T = no_match(Toks, T0),
    throw({error,loc(T),missing_comma}).

macro_expansion([{')',_Ap},{dot,_Ad}], _T0) -> [];
macro_expansion([{dot,_}=Dot], _T0) ->
    throw({error,loc(Dot),missing_parenthesis});
macro_expansion([T|Ts], _T0) ->
    [T|macro_expansion(Ts, T)];
macro_expansion([], T0) -> throw({error,loc(T0),premature_end}).

check_macro_arg(Name, Args, T) ->
    case lists:member(Name, Args) of
        true ->
            throw({error,loc(T),{duplicated_argument,Name}});
        false ->
            ok
    end.

%% expand_macros(Tokens, St)
%%  Expand the macros in a list of tokens, making sure that an expansion
%%  gets the same location as the macro call.

expand_macros(MacT, M, Toks, St) ->
    #epp{macs=Ms,uses=U} = St,
    Lm = loc(MacT),
    Anno = element(2, MacT),
    case expand_macro1(Lm, M, Toks, Ms) of
	{ok,{none,Exp}} ->
	    check_uses([{M,none}], [], U, Lm),
	    Toks1 = expand_macros(expand_macro(Exp, Anno, [], #{}), St),
	    expand_macros(Toks1++Toks, St);
	{ok,{As,Exp}} ->
	    check_uses([{M,length(As)}], [], U, Lm),
	    {Bs,Toks1} = bind_args(Toks, Lm, M, As, #{}),
	    expand_macros(expand_macro(Exp, Anno, Toks1, Bs), St)
    end.

expand_macro1(Lm, M, Toks, Ms) ->
    Arity = count_args(Toks, Lm, M),
    case Ms of
	#{M:=undefined} ->
	    %% Predefined macro without definition.
            throw({error,Lm,{undefined,M,Arity}});
	#{M:=[{none,Def}]} ->
            {ok,Def};
	#{M:=Defs} when is_list(Defs) ->
	    case proplists:get_value(Arity, Defs) of
                undefined ->
                    throw({error,Lm,{mismatch,M}});
                Def ->
                    {ok,Def}
            end;
        #{M:=PreDef} ->
	    %% Predefined macro.
            {ok,PreDef};
        _ ->
	    %% Macro not found.
            throw({error,Lm,{undefined,M,Arity}})
    end.

check_uses([], _Anc, _U, _Lm) ->
    ok;
check_uses([M|Rest], Anc, U, Lm) ->
    case lists:member(M, Anc) of
	true ->
	    {Name,Arity} = M,
	    throw({error,Lm,{circular,Name,Arity}});
	false ->
	    L = get_macro_uses(M, U),
	    check_uses(L, [M|Anc], U, Lm),
	    check_uses(Rest, Anc, U, Lm)
    end.

get_macro_uses({M,Arity}, U) ->
    case U of
	#{M:=L} ->
	    proplists:get_value(Arity, L, proplists:get_value(none, L, []));
	_ ->
	    []
    end.

%% Macro expansion
%% Note: io:scan_erl_form() does not return comments or white spaces.
expand_macros([{'?',_Aq},{atom,_Am,M}=MacT|Toks], St) ->
    expand_macros(MacT, M, Toks, St);
%% Special macros
expand_macros([{'?',_Aq},{var,Lm,'FUNCTION_NAME'}=Token|Toks], St0) ->
    St = update_fun_name(Token, St0),
    case St#epp.fname of
	undefined ->
	    [{'?',_Aq},Token];
	{Name,_} ->
	    [{atom,Lm,Name}]
    end ++ expand_macros(Toks, St);
expand_macros([{'?',_Aq},{var,Lm,'FUNCTION_ARITY'}=Token|Toks], St0) ->
    St = update_fun_name(Token, St0),
    case St#epp.fname of
	undefined ->
	    [{'?',_Aq},Token];
	{_,Arity} ->
	    [{integer,Lm,Arity}]
    end ++ expand_macros(Toks, St);
expand_macros([{'?',_Aq},{var,Lm,'LINE'}=Tok|Toks], St) ->
    Line = erl_scan:line(Tok),
    [{integer,Lm,Line}|expand_macros(Toks, St)];
expand_macros([{'?',_Aq},{var,_Am,M}=MacT|Toks], St) ->
    expand_macros(MacT, M, Toks, St);
%% Illegal macros
expand_macros([{'?',_Aq},Token|_Toks], _St) ->
    T = case erl_scan:text(Token) of
            Text when is_list(Text) ->
                Text;
            undefined ->
                Symbol = erl_scan:symbol(Token),
                io_lib:fwrite(<<"~tp">>, [Symbol])
        end,
    throw({error,loc(Token),{call,[$?|T]}});
expand_macros([T|Ts], St) ->
    [T|expand_macros(Ts, St)];
expand_macros([], _St) -> [].

%% bind_args(Tokens, MacroLocation, MacroName, ArgumentVars, Bindings)
%%  Collect the arguments to a macro call.

bind_args([{'(',_Alp},{')',_Arp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
bind_args([{'(',_Alp}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
bind_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

macro_args([{')',_Arp}|Toks], _Lm, _M, [], Bs) ->
    {Bs,Toks};
macro_args([{',',_Ac}|Toks0], Lm, M, [A|As], Bs) ->
    {Arg,Toks1} = macro_arg(Toks0, [], []),
    macro_args(Toks1, Lm, M, As, store_arg(Lm, M, A, Arg, Bs));
macro_args([], Lm, M, _As, _Bs) ->
    throw({error,Lm,{arg_error,M}}); % Cannot happen.
macro_args(_Toks, Lm, M, _As, _Bs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

store_arg(L, M, _A, [], _Bs) ->
    throw({error,L,{mismatch,M}});
store_arg(_L, _M, A, Arg, Bs) ->
    Bs#{A=>Arg}.

%% count_args(Tokens, MacroLine, MacroName)
%%  Count the number of arguments in a macro call.
count_args([{'(', _Alp},{')',_Arp}|_Toks], _Lm, _M) ->
    0;
count_args([{'(', _Alp},{',',_Ac}|_Toks], Lm, M) ->
    throw({error,Lm,{arg_error,M}});
count_args([{'(',_Alp}|Toks0], Lm, M) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, 1);
count_args(_Toks, _Lm, _M) ->
    none.

count_args([{')',_Arp}|_Toks], _Lm, _M, NbArgs) ->
    NbArgs;
count_args([{',',_Ac},{')',_Arp}|_Toks], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args([{',',_Ac}|Toks0], Lm, M, NbArgs) ->
    {_Arg,Toks1} = macro_arg(Toks0, [], []),
    count_args(Toks1, Lm, M, NbArgs+1);
count_args([], Lm, M, _NbArgs) ->
    throw({error,Lm,{arg_error,M}});
count_args(_Toks, Lm, M, _NbArgs) ->
    throw({error,Lm,{mismatch,M}}). % Cannot happen.

%% macro_arg([Tok], [ClosePar], [ArgTok]) -> {[ArgTok],[RestTok]}.
%%  Collect argument tokens until we hit a ',' or a ')'. We know a
%%  enough about syntax to recognise "open parentheses" and keep
%%  scanning until matching "close parenthesis".

macro_arg([{',',Lc}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{',',Lc}|Toks]};
macro_arg([{')',Lrp}|Toks], [], Arg) ->
    {lists:reverse(Arg),[{')',Lrp}|Toks]};
macro_arg([{'(',Llp}|Toks], E, Arg) ->
    macro_arg(Toks, [')'|E], [{'(',Llp}|Arg]);
macro_arg([{'<<',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, ['>>'|E], [{'<<',Lls}|Arg]);
macro_arg([{'[',Lls}|Toks], E, Arg) ->
    macro_arg(Toks, [']'|E], [{'[',Lls}|Arg]);
macro_arg([{'{',Llc}|Toks], E, Arg) ->
    macro_arg(Toks, ['}'|E], [{'{',Llc}|Arg]);
macro_arg([{'begin',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'begin',Lb}|Arg]);
macro_arg([{'if',Li}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'if',Li}|Arg]);
macro_arg([{'case',Lc}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'case',Lc}|Arg]);
macro_arg([{'fun',Lc}|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'fun',Lc}|Arg]);
macro_arg([{'fun',_}=Fun,{var,_,_}=Name|[{'(',_}|_]=Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [Name,Fun|Arg]);
macro_arg([{'maybe',Lb}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'maybe',Lb}|Arg]);
macro_arg([{'receive',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'receive',Lr}|Arg]);
macro_arg([{'try',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'try',Lr}|Arg]);
macro_arg([{'cond',Lr}|Toks], E, Arg) ->
    macro_arg(Toks, ['end'|E], [{'cond',Lr}|Arg]);
macro_arg([{Rb,Lrb}|Toks], [Rb|E], Arg) ->	%Found matching close
    macro_arg(Toks, E, [{Rb,Lrb}|Arg]);
macro_arg([T|Toks], E, Arg) ->
    macro_arg(Toks, E, [T|Arg]);
macro_arg([], _E, Arg) ->
    {lists:reverse(Arg),[]}.

%% expand_macro(MacroDef, MacroTokenAnno, RestTokens, Bindings)
%% expand_arg(Argtokens, MacroTokens, TokenAnno, RestTokens, Bindings)
%%  Insert the macro expansion replacing macro parameters with their
%%  argument values, inserting the location of first the macro call
%%  and then the macro arguments, i.e. simulate textual expansion.

expand_macro([{var,_Av,V}|Ts], Anno, Rest, Bs) ->
    case Bs of
	#{V:=Val} ->
	    expand_arg(Val, Ts, Anno, Rest, Bs);
	_ ->
	    [{var,Anno,V}|expand_macro(Ts, Anno, Rest, Bs)]
    end;
expand_macro([{'?', _}, {'?', _}, {var,_Av,V}|Ts], Anno, Rest, Bs) ->
    case Bs of
	#{V:=Val} ->
            expand_arg(stringify(Val, Anno), Ts, Anno, Rest, Bs);
	_ ->
	    [{var,Anno,V}|expand_macro(Ts, Anno, Rest, Bs)]
    end;
expand_macro([T|Ts], Anno, Rest, Bs) ->
    [setelement(2, T, Anno)|expand_macro(Ts, Anno, Rest, Bs)];
expand_macro([], _Anno, Rest, _Bs) -> Rest.

expand_arg([A|As], Ts, _Anno, Rest, Bs) ->
    %% It is not obvious that the annotation of arguments should
    %% replace _Anno.
    NextAnno = element(2, A),
    [A|expand_arg(As, Ts, NextAnno, Rest, Bs)];
expand_arg([], Ts, Anno, Rest, Bs) ->
    expand_macro(Ts, Anno, Rest, Bs).

%%%
%%% Here follows support for the ?FUNCTION_NAME and ?FUNCTION_ARITY
%%% macros. Since the parser has not been run yet, we don't know the
%%% name and arity of the current function. Therefore, we will need to
%%% scan the beginning of the current form to extract the name and
%%% arity of the function.
%%%

update_fun_name(Token, #epp{fname=Toks0}=St) when is_list(Toks0) ->
    %% ?FUNCTION_NAME or ?FUNCTION_ARITY is used for the first time in
    %% a function.  First expand macros (except ?FUNCTION_NAME and
    %% ?FUNCTION_ARITY) in the form.

    Toks1 = (catch expand_macros(Toks0, St#epp{fname=undefined})),

    %% Now extract the name and arity from the stream of tokens, and store
    %% the result in the #epp{} record so we don't have to do it
    %% again.

    case Toks1 of
	[{atom,_,Name},{'(',_}|Toks] ->
	    %% This is the beginning of a function definition.
	    %% Scan the token stream up to the matching right
	    %% parenthesis and count the number of arguments.
	    FA = update_fun_name_1(Toks, 1, {Name,0}, St),
	    St#epp{fname=FA};
	[{'?',_}|_] ->
	    %% ?FUNCTION_NAME/?FUNCTION_ARITY used at the beginning
	    %% of a form. Does not make sense.
	    {var,_,Macro} = Token,
	    throw({error,loc(Token),{illegal_function_usage,Macro}});
	_ when is_list(Toks1) ->
	    %% Not the beginning of a function (an attribute or a
	    %% syntax error).
	    {var,_,Macro} = Token,
	    throw({error,loc(Token),{illegal_function,Macro}});
	_ ->
	    %% A macro expansion error. Return a dummy value and
	    %% let the caller notice and handle the error.
	    St#epp{fname={'_',0}}
    end;
update_fun_name(_Token, St) ->
    St.

update_fun_name_1([Tok|Toks], L, FA, St) ->
    case classify_token(Tok) of
	comma ->
	    if
		L =:= 1 ->
		    {Name,Arity} = FA,
		    update_fun_name_1(Toks, L, {Name,Arity+1}, St);
		true ->
		    update_fun_name_1(Toks, L, FA, St)
	    end;
	left ->
	    update_fun_name_1(Toks, L+1, FA, St);
	right when L =:= 1 ->
	    FA;
	right ->
	    update_fun_name_1(Toks, L-1, FA, St);
	other ->
	    case FA of
		{Name,0} ->
		    update_fun_name_1(Toks, L, {Name,1}, St);
		{_,_} ->
		    update_fun_name_1(Toks, L, FA, St)
	    end
    end;
update_fun_name_1([], _, FA, _) ->
    %% Syntax error, but never mind.
    FA.

classify_token({C,_}) -> classify_token_1(C);
classify_token(_) -> other.

classify_token_1(',') -> comma;
classify_token_1('(') -> left;
classify_token_1('{') -> left;
classify_token_1('[') -> left;
classify_token_1('<<') -> left;
classify_token_1(')') -> right;
classify_token_1('}') -> right;
classify_token_1(']') -> right;
classify_token_1('>>') -> right;
classify_token_1(_) -> other.


%%% stringify(Ts, Anno) returns a list of one token: a string which when
%%% tokenized would yield the token list Ts.

%% erl_scan:text(T) is not backward compatible with this.
%% Note that escaped characters will be replaced by themselves.
token_src({dot, _}) ->
    ".";
token_src({X, _}) when is_atom(X) ->
    atom_to_list(X);
token_src({var, _, X}) ->
    atom_to_list(X);
token_src({char,_,C}) ->
    io_lib:write_char(C);
token_src({string, _, X}) ->
    io_lib:write_string(X);
token_src({_, _, X}) ->
    io_lib:format("~w", [X]).

stringify1([]) ->
    [];
stringify1([T | Tokens]) ->
    [io_lib:format(" ~ts", [token_src(T)]) | stringify1(Tokens)].

stringify(Ts, Anno) ->
    [$\s | S] = lists:flatten(stringify1(Ts)),
    [{string, Anno, S}].

coalesce_strings([{string,A,S} | Tokens]) ->
    coalesce_strings(Tokens, A, [S]);
coalesce_strings([T | Tokens]) ->
    [T | coalesce_strings(Tokens)];
coalesce_strings([]) ->
    [].

coalesce_strings([{string,_,S}|Tokens], A, S0) ->
    coalesce_strings(Tokens, A, [S | S0]);
coalesce_strings(Tokens, A, S) ->
    [{string,A,lists:append(lists:reverse(S))} | coalesce_strings(Tokens)].

find_mismatch([Tag|Tags], [{Tag,_A}=T|Ts], _T0) ->
    find_mismatch(Tags, Ts, T);
find_mismatch([Tag|Tags], [{Tag,_A,_V}=T|Ts], _T0) ->
    find_mismatch(Tags, Ts, T);
find_mismatch([var_or_atom|Tags], [{var,_A,_V}=T|Ts], _T0) ->
    find_mismatch(Tags, Ts, T);
find_mismatch([var_or_atom|Tags], [{atom,_A,_N}=T|Ts], _T0) ->
    find_mismatch(Tags, Ts, T);
find_mismatch([{Tag,Value}|Tags], [{Tag,_A,Value}=T|Ts], _T0) ->
    find_mismatch(Tags, Ts, T);
find_mismatch(_, Ts, T0) ->
    no_match(Ts, T0).

no_match([T|_], _T0) ->
    T;
no_match(_, T0) ->
    T0.

%% epp_request(Epp)
%% epp_request(Epp, Request)
%% epp_reply(From, Reply)
%%  Handle communication with the epp.

epp_request(Epp) ->
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_request(Epp, Req) ->
    Epp ! {epp_request,self(),Req},
    wait_epp_reply(Epp, erlang:monitor(process, Epp)).

epp_reply(From, Rep) ->
    From ! {epp_reply,self(),Rep},
    ok.

wait_epp_reply(Epp, Mref) ->
    receive
	{epp_reply,Epp,Rep} ->
	    erlang:demonitor(Mref, [flush]),
	    Rep;
	{'DOWN',Mref,_,_,E} ->
	    receive {epp_reply,Epp,Rep} -> Rep
	    after 0 -> exit(E)
	    end
    end.

expand_var([$$ | _] = NewName) ->
    case catch expand_var1(NewName) of
	{ok, ExpName} ->
	    ExpName;
	_ ->
	    NewName
    end;
expand_var(NewName) ->
    NewName.

expand_var1(NewName) ->
    [[$$ | Var] | Rest] = filename:split(NewName),
    Value = os:getenv(Var),
    true = Value =/= false,
    {ok, fname_join([Value | Rest])}.

fname_join(["." | [_|_]=Rest]) ->
    fname_join(Rest);
fname_join(Components) ->
    filename:join(Components).

loc(Token) ->
    erl_scan:location(Token).

add_line(Line, Offset) when is_integer(Line) ->
    Line+Offset;
add_line({Line, Column}, Offset) ->
    {Line+Offset, Column}.

start_loc(Line) when is_integer(Line) ->
    1;
start_loc({_Line, _Column}) ->
    {1, 1}.

line1() ->
    erl_anno:new(1).

get_line(Anno) ->
    erl_anno:line(Anno).

%% epp has always output -file attributes when entering and leaving
%% included files (-include, -include_lib). Starting with R11B the
%% -file attribute is also recognized in the input file. This is
%% mainly aimed at yecc, the parser generator, which uses the -file
%% attribute to get correct lines in messages referring to code
%% supplied by the user (actions etc in .yrl files).
%%
%% In a perfect world (read: perfectly implemented applications such
%% as Xref, Cover, Debugger, etc.) it would not be necessary to
%% distinguish -file attributes from epp and the input file. The
%% Debugger for example could have one window for each referred file,
%% each window with its own set of breakpoints etc. The line numbers
%% of the abstract code would then point into different windows
%% depending on the -file attribute. [Note that if, as is the case for
%% yecc, code has been copied into the file, then it is possible that
%% the copied code differs from the one referred to by the -file
%% attribute, which means that line numbers can mismatch.] In practice
%% however it is very rare with Erlang functions in included files, so
%% only one window is used per module. This means that the line
%% numbers of the abstract code have to be adjusted to refer to the
%% top-most source file. The function interpret_file_attributes/1
%% below interprets the -file attribute and returns forms where line
%% numbers refer to the top-most file. The -file attribute forms that
%% have been output by epp (corresponding to -include and
%% -include_lib) are kept, but the user's -file attributes are
%% removed. This seems sufficient for now.
%%
%% It turns out to be difficult to distinguish -file attributes in the
%% input file from the ones added by epp unless some action is taken.
%% The solution employed is to let epp label the annotation of user
%% supplied -file attributes as 'generated'.

-doc false.
interpret_file_attribute(Forms) ->
    interpret_file_attr(Forms, 0, []).

interpret_file_attr([{attribute,Anno,file,{File,Line}}=Form | Forms],
                    Delta, Fs) ->
    L = get_line(Anno),
    Generated = erl_anno:generated(Anno),
    if
        Generated ->
            %% -file attribute
            interpret_file_attr(Forms, (L + Delta) - Line, Fs);
        not Generated ->
            %% -include or -include_lib
            % true = L =:= Line,
            case Fs of
                [_, File | Fs1] -> % end of included file
                    [Form | interpret_file_attr(Forms, 0, [File | Fs1])];
                _ -> % start of included file
                    [Form | interpret_file_attr(Forms, 0, [File | Fs])]
            end
    end;
interpret_file_attr([Form0 | Forms], Delta, Fs) ->
    F = fun(Anno) ->
                Line = erl_anno:line(Anno),
                erl_anno:set_line(Line + Delta, Anno)
        end,
    Form = erl_parse:map_anno(F, Form0),
    [Form | interpret_file_attr(Forms, Delta, Fs)];
interpret_file_attr([], _Delta, _Fs) ->
    [].

-spec source_name(#epp{} | boolean(), file:filename_all()) -> file:filename_all().
source_name(Deterministic, Name) when is_boolean(Deterministic) ->
    case Deterministic of
        true -> filename:basename(Name);
        false -> Name
    end;
source_name(St, Name) ->
    source_name(St#epp.deterministic, Name).
