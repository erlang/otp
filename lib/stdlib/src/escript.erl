%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

-module(escript).
-moduledoc """
This module provides functions to create and inspect escripts.

See the [escript](`e:erts:escript_cmd.md`) program documentation
for more details on how to use escripts.
""".

%% Useful functions that can be called from scripts.
-export([script_name/0, create/2, extract/2]).

%% Internal API.
-export([start/0, start/1]).

%%-----------------------------------------------------------------------

-define(SHEBANG,  "/usr/bin/env escript").
-define(COMMENT,  "This is an -*- erlang -*- file").
-define(BUNDLE_HEADER, ".EB\n").
-define(NEW_ESCRIPT_TERM, escript_app_files).

%%-----------------------------------------------------------------------

-type mode()   :: 'compile' | 'debug' | 'interpret' | 'run'.
-type source() :: 'archive' | 'beam' | 'text'.

-record(state, {file         :: file:filename(),
                module       :: module(),
                forms_or_bin,
                source       :: source() | 'undefined',
                n_errors     :: non_neg_integer(),
                mode         :: mode(),
                exports_main :: boolean(),
                has_records  :: boolean()}).

-doc "The initial `#!` line.

For example:

```text
#!/usr/bin/env escript
```
".
-type shebang() :: string().
-type comment() :: string().
-doc "Any arguments that should be passed to [erl](`e:erts:erl_cmd.md`) when starting.".
-type emu_args() :: string().

-record(sections, {type,
		   shebang  :: shebang() | 'undefined',
		   comment  :: comment() | 'undefined',
		   emu_args :: emu_args() | 'undefined',
		   body}).

-record(extract_options, {compile_source}).

-type section() ::
        'shebang'
      | {'shebang', shebang() | default | undefined}
      | 'comment'
      | {'comment', comment() | default | undefined}
      | {'emu_args', emu_args() | undefined}
      | {'source', file:filename() | binary()}
      | {'beam', file:filename() | binary()}
      | {'files', [file:filename() | {file:filename(), binary()}]}
      | {'modules', [file:filename() | binary()]}.
-type legacy_archive() :: {'archive', binary()}.


%%-----------------------------------------------------------------------

%% Create a complete escript file with both header and body
-doc """"
Creates an escript from a list of sections.

The sections can be specified in any order. An escript begins with an optional
`Header` followed by a mandatory `Body`. If the header is present, it always
begins with a `shebang`, possibly followed by a `comment` and `emu_args`. The
`shebang` defaults to `"/usr/bin/env escript"`. The `comment` defaults to
`"This is an -*- erlang -*- file"`. The created escript can either be returned
as a binary or written to file.

> #### Change {: .info }
>
> Support for archive files is an experimental feature, and as such can be
> subject to incompatible changes.
>
> In Erlang/OTP 28, the archive support in `m:erl_prim_loader` and the
> the code server has been removed. Archives can be still be used for escripts,
> but the internal implementation of archives has changed as well a how
> to create an escript containing multiple file.
>
> `escript` scripts that use archive files must use
> `escript:extract/2` to read data files from its archive, as reading files
> from archives using `code:lib_dir/2` and `m:erl_prim_loader` is no longer
> supported.

As an example of how the function can be used, we create an
interpreted escript that uses `emu_args` to set some emulator flag. In
this case, it sets the number of schedulers with `+S3`. We also
extract the different sections from the newly created script:

```erlang
> Source = ~"""
%% Demo
main(_Args) ->
    io:format("~p\n", [erlang:system_info(schedulers)]).
""", ok.
> {ok, Bin} = escript:create(binary, [shebang, comment, {emu_args, "+S3"},
                                      {source, Source}]).
{ok,<<"#!/usr/bin/env escript\n%% This is an -*- erlang -*- file\n%%!+S3"...>>}
> file:write_file("demo.escript", Bin).
ok
> os:cmd("escript demo.escript").
"3\n"
> escript:extract("demo.escript", []).
{ok,[{shebang,default}, {comment,default}, {emu_args,"+S3"},
     {source,<<"%% Demo\nmain(_Args) ->\n    io:format(erlang:system_info(schedu"...>>}]}
```

An escript containing a compiled BEAM file can be created as follows:

```erlang
> file:write_file("demo.erl",
                  ["%% demo.erl\n-module(demo).\n-export([main/1]).\n\n", Source]).
ok
> {ok, demo, BeamCode} = compile:file("demo.erl", [binary]).
{ok,demo,
    <<70,79,82,49,0,0,2,208,66,69,65,77,65,116,111,109,0,0,0,
      79,0,0,0,9,4,100,...>>}
> escript:create("demo.escript", [shebang, {beam, BeamCode}]).
ok
> escript:extract("demo.escript", []).
{ok,[{shebang,undefined}, {comment,undefined}, {emu_args,undefined},
     {beam,<<70,79,82,49,0,0,3,68,66,69,65,77,65,116,
             111,109,0,0,0,83,0,0,0,9,...>>}]}
> os:cmd("escript demo.beam").
"true"
```

An escript containing both Erlang source files and BEAM code can be created as follows:

```erlang
> {ok, SourceCode} = file:read_file("demo.erl"), ok.
ok
> escript:create("demo.escript",
                 [shebang,
                  {modules, [BeamCode]},
                  {files, ["demo.erl"]}]).
ok
> escript:extract("demo.escript", []).
{ok,[{shebang,default},
     {comment,undefined},
     {emu_args,undefined},
     {modules,[<<70,79,82,49,0,0,2,144,66,69,65,77,65,116,85,
                 56,0,0,0,82,...>>]},
     {files,[{"demo.erl",
              <<"%% demo.erl\n-module(demo).\n-export([main/1]).\n\n%% Demo\nmain(_Args) -"...>>}]}]}
```
"""".
-spec create(file:filename() | binary(), [section()]) ->
		    ok | {ok, binary()} | {error, term()}.

create(File, Options) when is_list(Options) ->
    try
	S = prepare(Options, #sections{}),
	BinList =
	    [Section || Section <- [S#sections.shebang,
				    S#sections.comment,
				    S#sections.emu_args,
				    S#sections.body],
			Section =/= undefined],
	case File of
	    binary ->
		{ok, list_to_binary(BinList)};
	    _ ->
		case file:write_file(File, BinList) of
		    ok ->
			ok;
		    {error, Reason} ->
			{error, {Reason, File}}
		end
	end
    catch
	throw:PrepareReason ->
	    {error, PrepareReason}
    end.

prepare([H | T], S) ->
    case H of
	{shebang, undefined} ->
	    prepare(T, S);
	shebang ->
	    prepare(T, S#sections{shebang = "#!" ++ ?SHEBANG ++ "\n"});
	{shebang, default} ->
	    prepare(T, S#sections{shebang = "#!" ++ ?SHEBANG ++ "\n"});
	{shebang, Shebang} when is_list(Shebang) ->
	    prepare(T, S#sections{shebang = "#!" ++ Shebang ++ "\n"});
	{comment, undefined} ->
	    prepare(T, S);
	comment ->
	    prepare(T, S#sections{comment = "%% " ++ ?COMMENT ++ "\n"});
	{comment, default} ->
	    prepare(T, S#sections{comment = "%% " ++ ?COMMENT ++ "\n"});
	{comment, Comment} when is_list(Comment) ->
	    prepare(T, S#sections{comment = "%% " ++ Comment ++ "\n"});
	{emu_args, undefined} ->
	    prepare(T, S);
	{emu_args, Args} when is_list(Args) ->
	    prepare(T, S#sections{emu_args = "%%!" ++ Args ++ "\n"});
	{archive = Type, ZipFiles, ZipOptions}
	  when is_list(ZipFiles), is_list(ZipOptions) ->
	    File = "dummy.zip",
	    case zip:create(File, ZipFiles, ZipOptions ++ [memory]) of
		{ok, {File, ZipBin}} ->
                    prepare([{Type, ZipBin} | T], S);
		{error, Reason} ->
		    throw({Reason, H})
	    end;
	{modules=Type, Files} when is_list(Files) ->
            prepare_bundle(Type, Files, T, S);
	{files=Type, Files} when is_list(Files) ->
            prepare_bundle(Type, Files, T, S);
	{Type, File} when is_list(File) ->
	    case file:read_file(File) of
		{ok, Bin} ->
		    prepare(T, S#sections{type = Type, body = Bin});
		{error, Reason} ->
		    throw({Reason, H})
	    end;
	{Type, Bin} when is_binary(Bin) ->
	    prepare(T, S#sections{type = Type, body = Bin});
	_ ->
	    throw({badarg, H})
    end;
prepare([], #sections{body = undefined}) ->
    throw(missing_body);
prepare([], #sections{type = bundle, body = BodyMap} = S) ->
    Modules = case BodyMap of
                  #{modules := Mods} -> Mods;
                  #{} -> throw(no_modules)
              end,
    Files = maps:get(files, BodyMap, []),
    S#sections{body=beam_bundle(Modules, Files)};
prepare([], #sections{type = Type} = S) when Type =:= source; Type =:= beam ->
    S;
prepare([], #sections{type = Type}) ->
    throw({illegal_type, Type});
prepare(BadOptions, _) ->
    throw({badarg, BadOptions}).

prepare_bundle(Type, Files, T, #sections{body=undefined}=S) ->
    prepare_bundle(Type, Files, T, S#sections{type=bundle, body=#{}});
prepare_bundle(Type, Files0, T, #sections{type=bundle, body=Body}=S)
  when is_map(Body) ->
    Files = case Type of
                modules -> read_beams(Files0);
                files -> read_files(Files0)
            end,
    prepare(T, S#sections{body=Body#{Type => Files}}).

read_files(Files) ->
    [case FileOrBin of
         {Name, Binary} when is_list(Name), is_binary(Binary) ->
             {Name, Binary};
         Name when is_list(Name) ->
             case file:read_file(Name) of
                 {ok, Bin} ->
                     {Name, Bin};
                 {error, Reason} ->
                     throw({Reason, FileOrBin})
             end;
         _ ->
             throw({illegal_file, FileOrBin})
     end || FileOrBin <- Files].

read_beams(Files) ->
    [if
         is_binary(FileOrBin) ->
             FileOrBin;
         is_list(FileOrBin) ->
             case file:read_file(FileOrBin) of
                 {ok, Bin} ->
                     Bin;
                 {error, Reason} ->
                     throw({Reason, FileOrBin})
             end;
         true ->
             throw({illegal_file, FileOrBin})
     end || FileOrBin <- Files].

beam_bundle(Beams, OtherFiles) ->
    Packed = term_to_binary(Beams, [{compressed,9}]),
    PackedSize = byte_size(Packed),
    OtherArchive = term_to_binary(OtherFiles, [{compressed,9}]),
    OtherArchiveSize = byte_size(OtherArchive),
    <<?BUNDLE_HEADER,PackedSize:32,Packed/binary,
      OtherArchiveSize:32,OtherArchive/binary>>.

-type section_name() :: shebang | comment | emu_args | body .
-type extract_option() :: compile_source | {section, [section_name()]}.
-doc """
Parses an escript and extracts its sections.

This is the reverse of `create/2`.

All sections are returned even if they do not exist in the escript. If a
particular section happens to have the same value as the default value, the
extracted value is set to the atom `default`. If a section is missing, the
extracted value is set to the atom `undefined`.

> #### Change {: .info }
>
> Support for archive files is an experimental feature, and as such can be
> subject to incompatible changes.
>
> `escript:extract/2` now returns BEAM files and other files separately
> in `{modules, [...]}` and `{files, [...]}` tuples, respectively,
> when the input is an escript created by Erlang/OTP 28 or later.
>
> If the escript was created by Erlang/OTP 27 or earlier, a zip archive
> will still be returned in an `{archive, ZipArchive}` tuple.

Option `compile_source` only affects the result if the escript contains `source`
code. In this case the Erlang code is automatically compiled and
`{source, BeamCode}` is returned instead of `{source, SourceCode}`.

Example:

```erlang
> escript:create("demo.escript",
                 [shebang,
                  {modules, [BeamCode]},
                  {files, ["demo.erl"]}]).
ok
> escript:extract("demo.escript", []).
{ok,[{shebang,default},
     {comment,undefined},
     {emu_args,undefined},
     {modules,[<<70,79,82,49,0,0,2,144,66,69,65,77,65,116,85,
                 56,0,0,0,82,...>>]},
     {files,[{"demo.erl",
              <<"%% demo.erl\n-module(demo).\n-export([main/1]).\n\n%% Demo\nmain(_Args) -"...>>}]}]}
```

`escript:extract/2` in Erlang/OTP 28 can extract the sections of an
escript created by Erlang/OTP 27 and earlier:

```
> escript:extract("demo_otp27.escript", []).
{ok,[{{archive,<<80,75,3,4,20,0,0,0,8,0,118,7,98,60,105,
                152,61,93,107,0,0,0,118,0,...>>}
     {emu_args,undefined}]}
```
""".
-spec extract(file:filename(), [extract_option()]) ->
        {ok, [section() | legacy_archive()]} | {error, term()}.

extract(File, Options) when is_list(File), is_list(Options) ->
    try
	EO = parse_extract_options(Options,
				   #extract_options{compile_source = false}),
	{HeaderSz, StartLine, Fd, Sections} =
	    parse_header(File, not EO#extract_options.compile_source),
	Type = Sections#sections.type,
	case {Type, EO#extract_options.compile_source} of
	    {source, true} ->
		Bin = compile_source(Type, File, Fd, StartLine, HeaderSz);
	    {_, _} ->
		ok = file:close(Fd),
		case file:read_file(File) of
		    {ok, <<_Header:HeaderSz/binary, Bin/binary>>} ->
			ok;
		    {error, ReadReason} ->
			Bin = get_rid_of_compiler_warning,
			throw(ReadReason)
		end
	end,
	return_sections(Sections, Bin)
    catch
	throw:Reason ->
	    {error, Reason}
    end.

parse_extract_options([H | T], EO) ->
    case H of
	compile_source ->
	    EO2 = EO#extract_options{compile_source = true},
	    parse_extract_options(T, EO2);
	_ ->
	    throw({badarg, H})
    end;
parse_extract_options([], EO) ->
    EO.

compile_source(Type, File, Fd, StartLine, HeaderSz) ->
    {text, _Module, Forms, _HasRecs, _Mode} =
	do_parse_file(Type, File, Fd, StartLine, HeaderSz, false),
    ok = file:close(Fd),
    case compile:forms(Forms, [return_errors, debug_info]) of
	{ok, _, BeamBin} ->
	    BeamBin;
	{error, Errors, Warnings} ->
	    throw({compile, [{errors, format_errors(Errors)},
			     {warnings, format_errors(Warnings)}]})
    end.

format_errors(CompileErrors) ->
    [lists:flatten([File, ":", integer_to_list(LineNo), ": ",
		    Mod:format_error(Error)]) ||
	{File, FileErrors} <- CompileErrors,
	{LineNo, Mod, Error} <- FileErrors].

return_sections(S, Bin) ->
    {ok, [normalize_section(shebang,  S#sections.shebang),
	  normalize_section(comment,  S#sections.comment),
	  normalize_section(emu_args, S#sections.emu_args)] ++
	  normalize_section(S#sections.type, Bin)}.

normalize_section(Name, undefined) ->
    {Name, undefined};
normalize_section(shebang, "#!" ++ Chars) ->
    Chopped = string:trim(Chars, trailing, "$\n"),
    Stripped = string:trim(Chopped, both),
    if
	Stripped =:= ?SHEBANG ->
	    {shebang, default};
	true ->
	    {shebang, Stripped}
    end;
normalize_section(comment, Chars) ->
    Chopped = string:trim(Chars, trailing, "$\n"),
    Stripped = string:trim(string:trim(Chopped, leading, "$%"), both),
    if
	Stripped =:= ?COMMENT ->
	    {comment, default};
	true ->
	    {comment, Stripped}
    end;
normalize_section(emu_args, "%%!" ++ Chars) ->
    Chopped = string:trim(Chars, trailing, "$\n"),
    Stripped = string:trim(Chopped, both),
    {emu_args, Stripped};
normalize_section(archive, Bin) ->
    case Bin of
        <<?BUNDLE_HEADER,BeamSize:32,PackedBeam:BeamSize/binary,
          OtherSize:32,PackedOther:OtherSize/binary>> ->
            Beams = binary_to_term(PackedBeam, [safe]),
            OtherArchive = binary_to_term(PackedOther, [safe]),
            [{modules, Beams},
             {files, OtherArchive}];
        _ ->
            [{archive, Bin}]
    end;
normalize_section(Name, Chars) ->
    [{Name, Chars}].

-doc """
Returns the name of the escript that is executed.

If the function is invoked outside the context of an escript,
the behavior is undefined.
""".
-spec script_name() -> string().

script_name() ->
    [ScriptName|_] = init:get_plain_arguments(),
    ScriptName.

%%
%% Internal API.
%%

-doc hidden.
-spec start() -> no_return().
start() ->
    start([]).

-doc hidden.
-spec start([string()]) -> no_return().
start(EscriptOptions) ->
    try
        %% Commands run using -run or -s are run in a process
        %% trap_exit set to false. Because this behaviour is
        %% surprising for users of escript, make sure to reset
        %% trap_exit to false.
        process_flag(trap_exit, false),
        case init:get_plain_arguments() of
            [File|Args] ->
                parse_and_run(File, Args, EscriptOptions);
            [] ->
                io:format(standard_error, "escript: Missing filename\n", []),
                my_halt(127)
        end
    catch
        throw:Str ->
            put_chars(io_lib:format("escript: ~ts\n", [Str])),
            my_halt(127);
        _:Reason:Stk ->
            put_chars(io_lib:format("escript: Internal error: ~tp\n", [Reason])),
            put_chars(io_lib:format("~tp\n", [Stk])),
            my_halt(127)
    end.

-spec parse_and_run(_, _, _) -> no_return().

parse_and_run(File, Args, Options) ->
    CheckOnly = lists:member("s", Options),
    {Source, Module, FormsOrBin, HasRecs, Mode} =
	parse_file(File, CheckOnly),
    Mode2 =
        case lists:member("d", Options) of
            true  ->
		debug;
            false ->
		case lists:member("c", Options) of
		    true  ->
			compile;
		    false ->
			case lists:member("i", Options) of
			    true  -> interpret;
			    false -> Mode
			end
		end
        end,
    if
        is_list(FormsOrBin) ->
            case Mode2 of
                interpret ->
                    interpret(FormsOrBin, HasRecs, File, Args);
                compile ->
                    case compile:forms(FormsOrBin, [report]) of
                        {ok, Module, BeamBin} ->
                            {module, Module} = code:load_binary(Module, File, BeamBin),
                            run(Module, Args);
                        _Other ->
                            fatal("There were compilation errors.")
                    end;
                debug ->
                    case compile:forms(FormsOrBin, [report, debug_info]) of
                        {ok,Module,BeamBin} ->
                            {module, Module} = code:load_binary(Module, File, BeamBin),
                            debug(Module, {Module, File, File, BeamBin}, Args);
                        _Other ->
                            fatal("There were compilation errors.")
                    end
            end;
        is_binary(FormsOrBin) ->
            case Source of
                archive ->
                    case handle_archive(File, FormsOrBin) of
                        ok when CheckOnly ->
			    case code:ensure_loaded(Module) of
				{module, _} ->
				    case erlang:function_exported(Module, main, 1) of
					true ->
					    my_halt(0);
					false ->
					    Text = lists:concat(["Function ", Module,
								 ":main/1 is not exported"]),
					    fatal(Text)
				    end;
				_ ->
				    Text = lists:concat(["Cannot load module ", Module,
							 " from archive"]),
				    fatal(Text)
			    end;
			ok ->
                            case Mode2 of
                                run   -> run(Module, Args);
                                debug -> debug(Module, Module, Args)
                            end;
            {error, Reason} ->
                fatal(Reason)
                    end;
                beam ->
                    case Mode2 of
                        run ->
                            {module, Module} = code:load_binary(Module, File, FormsOrBin),
                            run(Module, Args);
                        debug ->
			    [Base | Rest] = lists:reverse(filename:split(File)),
			    Base2 = filename:basename(Base, code:objfile_extension()),
			    Rest2 =
				case Rest of
				    ["ebin" | Top] -> ["src" | Top];
				    _ -> Rest
				end,
			    SrcFile = filename:join(lists:reverse([Base2 ++ ".erl" | Rest2])),
			    debug(Module, {Module, SrcFile, File, FormsOrBin}, Args)
                    end
            end
    end.

handle_archive(_File, <<?BUNDLE_HEADER,BeamSize:32,Beams0:BeamSize/binary,
                       OtherSize:32,OtherFiles:OtherSize/binary>>) ->
    Beams = name_beams(binary_to_term(Beams0, [safe])),
    UnpackedFiles = binary_to_term(OtherFiles, [safe]),
    AppFiles = [{list_to_atom(filename:basename(filename:rootname(Name))), Bin} ||
                   {Name, Bin} <:- UnpackedFiles,
                   filename:extension(Name) =:= ".app"],
    persistent_term:put(?NEW_ESCRIPT_TERM, AppFiles),
    {ok,Prepared} = code:prepare_loading(Beams),
    ok = code:finish_loading(Prepared),
    ok;
handle_archive(File, Archive) ->
    {ok, {Beams, AppFiles}} = extract_archive(File, Archive),
    persistent_term:put(?NEW_ESCRIPT_TERM, AppFiles),
    {ok, Prepared} = code:prepare_loading(Beams),
    ok = code:finish_loading(Prepared),
    ok.

name_beams([Beam | Beams]) ->
    Info = beam_lib:info(Beam),
    {module, Mod} = lists:keyfind(module, 1, Info),
    File = atom_to_list(Mod) ++ ".erl",
    [{Mod,File,Beam}|name_beams(Beams)];
name_beams([]) -> [].

extract_archive(File, Archive) ->
    zip:foldl(fun do_extract_archive/4, {[], []}, {File, Archive}).

do_extract_archive(Name, _, Get, {BeamAcc, AppFileAcc}) ->
    case filename:extension(Name) of
        ".beam" ->
            BeamFile = filename:basename(Name),
            Mod = list_to_atom(filename:rootname(BeamFile)),
            {[{Mod, BeamFile, Get()} | BeamAcc], AppFileAcc};
        ".app" ->
            App = list_to_atom(filename:rootname(filename:basename(Name))),
            {BeamAcc, [{App, Get()} | AppFileAcc]};
        _ ->
            {BeamAcc, AppFileAcc}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_file(File, CheckOnly) ->
    {HeaderSz, StartLine, Fd, Sections} =
	parse_header(File, false),
    do_parse_file(Sections#sections.type,
		  File, Fd, StartLine, HeaderSz, CheckOnly).

do_parse_file(Type, File, Fd, StartLine, HeaderSz, CheckOnly) ->
    S = initial_state(File),
    #state{mode = Mode,
	   source = Source,
	   module = Module,
	   forms_or_bin = FormsOrBin,
	   has_records = HasRecs} =
	case Type of
	    archive ->
		%% Archive file
		ok = file:close(Fd),
		parse_archive(S, File, HeaderSz);
	    beam ->
		%% Beam file
		ok = file:close(Fd),
		parse_beam(S, File, HeaderSz, CheckOnly);
	    source ->
		%% Source code
		parse_source(S, File, Fd, StartLine, HeaderSz, CheckOnly)
        end,
    {Source, Module, FormsOrBin, HasRecs, Mode}.

initial_state(File) ->
    #state{file = File,
	   n_errors = 0,
	   mode = compile,
	   exports_main = false,
	   has_records = false}.

%% Skip header and make a heuristic guess about the script type
parse_header(File, KeepFirst) ->
    LineNo = 1,
    {ok, Fd} =
        case file:open(File, [read]) of
            {ok, Fd0} ->
                {ok, Fd0};
            {error, R} ->
                fatal(lists:concat([file:format_error(R), ": '", File, "'"]))
        end,

    %% Skip shebang on first line
    {ok, HeaderSz0} = file:position(Fd, cur),
    Line1 = get_line(Fd),
    case classify_line(Line1) of
        shebang ->
            find_first_body_line(Fd, HeaderSz0, LineNo, KeepFirst,
				 #sections{shebang = Line1});
	archive ->
	    {HeaderSz0, LineNo, Fd,
	     #sections{type = archive}};
	beam ->
            {HeaderSz0, LineNo, Fd,
	     #sections{type = beam}};
        comment ->
            find_first_body_line(Fd, HeaderSz0, LineNo, KeepFirst,
				 #sections{comment = Line1});
	_ ->
            find_first_body_line(Fd, HeaderSz0, LineNo, KeepFirst,
				 #sections{})
    end.

find_first_body_line(Fd, HeaderSz0, LineNo, KeepFirst, Sections) ->
    {ok, HeaderSz1} = file:position(Fd, cur),
    %% Look for special comment on second line
    Line2 = get_line(Fd),
    {ok, HeaderSz2} = file:position(Fd, cur),
    case Sections of
        #sections{shebang=undefined,comment=Comment} when is_list(Comment) ->
            {HeaderSz1, LineNo, Fd,
             Sections#sections{type = guess_type(Line2)}};
        #sections{shebang=undefined} when KeepFirst ->
            %% No shebang and no comment. Use the entire file.
            {HeaderSz0, LineNo, Fd,
             Sections#sections{type = guess_type(Line2)}};
        #sections{shebang=undefined} ->
            %% No shebang. Skip the first line.
            {HeaderSz1, LineNo, Fd,
             Sections#sections{type = guess_type(Line2)}};
        #sections{} ->
            case classify_line(Line2) of
                emu_args ->
                    %% Skip special comment on second line
                    Line3 = get_line(Fd),
                    {HeaderSz2, LineNo + 2, Fd,
                     Sections#sections{type = guess_type(Line3),
                                       comment = undefined,
                                       emu_args = Line2}};
                comment ->
                    %% Look for special comment on third line
                    Line3 = get_line(Fd),
                    {ok, HeaderSz3} = file:position(Fd, cur),
                    Line3Type = classify_line(Line3),
                    if
                        Line3Type =:= emu_args ->
                            %% Skip special comment on third line
                            Line4 = get_line(Fd),
                            {HeaderSz3, LineNo + 3, Fd,
                             Sections#sections{type = guess_type(Line4),
                                               comment = Line2,
                                               emu_args = Line3}};
                        true ->
                            %% Skip shebang on first line and comment on second
                            {HeaderSz2, LineNo + 2, Fd,
                             Sections#sections{type = guess_type(Line3),
                                               comment = Line2}}
                    end;
                _ ->
                    %% Just skip shebang on first line
                    {HeaderSz1, LineNo + 1, Fd,
                     Sections#sections{type = guess_type(Line2)}}
            end
    end.

classify_line(Line) ->
    case Line of
        "#!" ++ _ -> shebang;
        "PK" ++ _ -> archive;
        ".EB" ++ _ -> archive;
        "FOR1" ++ _ -> beam;
        "%%!" ++ _ -> emu_args;
        "%" ++ _ -> comment;
        _ -> undefined
   end.

guess_type(Line) ->
    case classify_line(Line) of
	archive -> archive;
	beam    -> beam;
	_       -> source
    end.

get_line(P) ->
    case io:get_line(P, '') of
        eof ->
            fatal("Premature end of file reached");
        Line ->
            Line
    end.

parse_archive(S, File, HeaderSz) ->
    case file:read_file(File) of
	{ok, <<_Header:HeaderSz/binary, Bin/binary>>} ->
	    Mod =
		case init:get_argument(escript) of
		    {ok, [["main", M]]} ->
			%% Use explicit module name
			list_to_atom(M);
		    _ ->
			%% Use escript name without extension as module name
			RevBase = lists:reverse(filename:basename(File)),
			RevBase2 =
			    case lists:dropwhile(fun(X) -> X =/= $. end, RevBase) of
                                [$. | Rest] -> Rest;
                                [] -> RevBase
                            end,
                        list_to_atom(lists:reverse(RevBase2))
		end,
	    S#state{source = archive,
		    mode = run,
		    module = Mod,
		    forms_or_bin = Bin};
	{ok, _} ->
	    fatal("Illegal archive format");
	{error, Reason} ->
	    fatal(file:format_error(Reason))
    end.


parse_beam(S, File, HeaderSz, CheckOnly) ->
    {ok, <<_Header:HeaderSz/binary, Bin/binary>>} =
        file:read_file(File),
    case beam_lib:chunks(Bin, [exports]) of
	{ok, {Module, [{exports, Exports}]}} ->
	    case CheckOnly of
		true ->
		    case lists:member({main, 1}, Exports) of
			true ->
			    my_halt(0);
			false ->
			    Text = lists:concat(["Function ", Module, ":main/1 is not exported"]),
			    fatal(Text)
		    end;
		false ->
		    S#state{source = beam,
			    mode = run,
			    module = Module,
			    forms_or_bin = Bin}
	    end;
	{error, beam_lib, Reason} when is_tuple(Reason) ->
            fatal(element(1, Reason))
    end.

parse_source(S, File, Fd, StartLine, HeaderSz, CheckOnly) ->
    {PreDefMacros, Module} = pre_def_macros(File),
    IncludePath = [],
    %% Read the encoding on the second line, if there is any:
    {ok, _} = file:position(Fd, 0),
    _ = io:get_line(Fd, ''),
    Encoding = epp:set_encoding(Fd),
    {ok, _} = file:position(Fd, HeaderSz),
    case epp:open([{fd, Fd}, {name, File}, {location, {StartLine, 1}},
                   {includes, IncludePath}, {macros, PreDefMacros}]) of
        {ok, Epp} ->
            _ = [io:setopts(Fd, [{encoding,Encoding}]) ||
                    Encoding =/= none],
            {ok, FileForm} = epp:parse_erl_form(Epp),
            OptModRes = epp:parse_erl_form(Epp),
            S2 = S#state{source = text, module = Module},
            S3 =
                case OptModRes of
                    {ok, {attribute,_, module, M} = Form} ->
                        epp_parse_file(Epp, S2#state{module = M}, [Form, FileForm]);
                    {ok, _} ->
                        ModForm = {attribute,a1(),module, Module},
                        epp_parse_file2(Epp, S2, [ModForm, FileForm], OptModRes);
                    {error, _} ->
                        epp_parse_file2(Epp, S2, [FileForm], OptModRes);
                    {eof, LastLine} ->
                        S#state{forms_or_bin = [FileForm, {eof, LastLine}]}
                end,
            ok = epp:close(Epp),
            ok = file:close(Fd),
	    check_source(S3, CheckOnly);
	{error, Reason} ->
	    io:format(standard_error, "escript: ~tp\n", [Reason]),
	    fatal("Preprocessor error")
    end.

check_source(S, CheckOnly) ->
    case S of
	#state{n_errors = Nerrs} when Nerrs =/= 0 ->
	    fatal("There were compilation errors.");
	#state{exports_main = ExpMain,
	       forms_or_bin = [FileForm2, ModForm2 | Forms]} ->
	    %% Optionally add export of main/1
	    Forms2 =
		case ExpMain of
		    false -> [{attribute, a0(), export, [{main,1}]} | Forms];
		    true  -> Forms
		end,
	    Forms3 = [FileForm2, ModForm2 | Forms2],
	    case CheckOnly of
		true ->
		    %% Strong validation and halt
		    case compile:forms(Forms3, [report,strong_validation]) of
			{ok,_} ->
			    my_halt(0);
			_Other ->
			    fatal("There were compilation errors.")
		    end;
		false ->
		    S#state{forms_or_bin = Forms3}
	    end
    end.

pre_def_macros(File) ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Unique = erlang:unique_integer([positive]),
    Replace = fun(Char) ->
		      case Char of
			  $\. -> $\_;
                          _ -> Char
                      end
              end,
    CleanBase = lists:map(Replace, filename:basename(File)),
    ModuleStr =
	CleanBase ++ "__" ++
        "escript__" ++
        integer_to_list(MegaSecs) ++ "__" ++
	integer_to_list(Secs) ++ "__" ++
	integer_to_list(MicroSecs) ++ "__" ++
	integer_to_list(Unique),
    Module = list_to_atom(ModuleStr),
    PreDefMacros = [{'MODULE', Module, redefine},
                    {'MODULE_STRING', ModuleStr, redefine}],
    {PreDefMacros, Module}.

epp_parse_file(Epp, S, Forms) ->
    Parsed = epp:parse_erl_form(Epp),
    epp_parse_file2(Epp, S, Forms, Parsed).

epp_parse_file2(Epp, S, Forms, {ok, {attribute, _, mode, native}}) ->
    %% Native mode is no longer supported, just ignore it.
    epp_parse_file(Epp, S, Forms);
epp_parse_file2(Epp, S, Forms, Parsed) ->
    %% io:format(standard_error, "~p\n", [Parsed]),
    case Parsed of
        {ok, Form} ->
            case Form of
                {attribute,_,record, _} ->
                    S2 = S#state{has_records = true},
                    epp_parse_file(Epp, S2, [Form | Forms]);
                {attribute,Ln,mode,NewMode} ->
                    S2 = S#state{mode = NewMode},
                    if
                        NewMode =:= compile; NewMode =:= interpret; NewMode =:= debug ->
                            epp_parse_file(Epp, S2, [Form | Forms]);
                        true ->
                            Args = lists:flatten(io_lib:format("illegal mode attribute: ~p", [NewMode])),
                            io:format(standard_error, "~ts:~s: ~s\n", [S#state.file,pos(Ln),Args]),
                            Error = {error,{Ln,erl_parse,Args}},
                            Nerrs= S#state.n_errors + 1,
                            epp_parse_file(Epp, S2#state{n_errors = Nerrs}, [Error | Forms])
                    end;
                {attribute,_,export,Fs} ->
                    case lists:member({main,1}, Fs) of
                        false ->
                            epp_parse_file(Epp, S, [Form | Forms]);
                        true ->
                            epp_parse_file(Epp, S#state{exports_main = true}, [Form | Forms])
                    end;
                _ ->
                    epp_parse_file(Epp, S, [Form | Forms])
            end;
        {error,{Ln,Mod,Args}} = Form ->
            io:format(standard_error, "~ts:~s: ~ts\n",
                      [S#state.file,pos(Ln),Mod:format_error(Args)]),
            epp_parse_file(Epp, S#state{n_errors = S#state.n_errors + 1}, [Form | Forms]);
        {eof, LastLine} ->
            S#state{forms_or_bin = lists:reverse([{eof, LastLine} | Forms])}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluate script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec debug(_, _, _) -> no_return().

debug(Module, AbsMod, Args) ->
    case hidden_apply(debugger, debugger, start, []) of
	{ok, _} ->
	    case hidden_apply(debugger, int, i, [AbsMod]) of
		{module, _} ->
		    hidden_apply(debugger, debugger, auto_attach, [[init]]),
		    run(Module, Args);
		error ->
		    Text = lists:concat(["Cannot load the code for ", Module, " into the debugger"]),
		    fatal(Text)
	    end;
	_ ->
	    fatal("Cannot start the debugger")
    end.

-spec run(_, _) -> no_return().

run(Module, Args) ->
    try
        Module:main(Args),
        my_halt(0)
    catch
        Class:Reason:StackTrace ->
            fatal(format_exception(Class, Reason, StackTrace))
    end.

-spec interpret(_, _, _, _) -> no_return().

interpret(Forms, HasRecs,  File, Args) ->
    %% Basic validation before execution
    case erl_lint:module(Forms) of
	{ok,Ws} ->
	    report_warnings(Ws);
	{error,Es,Ws} ->
	    report_errors(Es),
	    report_warnings(Ws),
	    fatal("There were compilation errors.")
    end,
    %% Optionally expand records
    Forms2 =
	case HasRecs of
	    false -> Forms;
	    true  -> erl_expand_records:module(Forms, [])
	end,
    Dict = parse_to_map(Forms2),
    ArgsA = erl_parse:abstract(Args, 0),
    Anno = a0(),
    Call = {call,Anno,{atom,Anno,main},[ArgsA]},
    try
        _ = erl_eval:expr(Call,
                          erl_eval:new_bindings(),
                          {value,fun(I, J) ->
                                         code_handler(I, J, Dict, File)
                                 end}),
        my_halt(0)
    catch
        Class:Reason:StackTrace ->
            fatal(format_exception(Class, Reason, StackTrace))
    end.

report_errors(Errors) ->
    lists:foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
                      ({F,Eds}) -> list_errors(F, Eds) end,
                  Errors).

list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite(standard_error, "~ts:~s: ~ts\n", [F,pos(Line),Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:format(standard_error, "~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

pos({Line,Col}) ->
    io_lib:format("~w:~w", [Line,Col]);
pos(Line) ->
    io_lib:format("~w", [Line]).

report_warnings(Ws0) ->
    Ws1 = lists:flatmap(fun({{F,_L},Eds}) -> format_message(F, Eds);
                           ({F,Eds}) -> format_message(F, Eds) end,
                  Ws0),
    Ws = ordsets:from_list(Ws1),
    lists:foreach(fun({_,Str}) -> io:put_chars(standard_error, Str) end, Ws).

format_message(F, [{Line,Mod,E}|Es]) ->
    M = {{F,Line},io_lib:format("~ts:~s: Warning: ~ts\n", [F,pos(Line),Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(F, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~ts: Warning: ~ts\n", [F,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(_, []) -> [].

parse_to_map(L) -> parse_to_map(L, maps:new()).

parse_to_map([{function,_,Name,Arity,Clauses}|T], Map0) ->
    Map = maps:put({local, Name,Arity}, Clauses, Map0),
    parse_to_map(T, Map);
parse_to_map([{attribute,_,import,{Mod,Funcs}}|T], Map0) ->
    Map = lists:foldl(fun(I, D) ->
                              maps:put({remote,I}, Mod, D)
                       end, Map0, Funcs),
    parse_to_map(T, Map);
parse_to_map([_|T], Map) ->
    parse_to_map(T, Map);
parse_to_map([], Map) ->
    Map.

code_handler(local, [file], _, File) ->
    File;
code_handler(Name, Args, Map, File) ->
    %%io:format(standard_error, "code handler=~p~n",[{Name, Args}]),
    Arity = length(Args),
    case maps:find({local,Name,Arity}, Map) of
        {ok, Cs} ->
            LF = {value,fun(I, J) -> code_handler(I, J, Map, File) end},
            case erl_eval:match_clause(Cs, Args,erl_eval:new_bindings(),LF) of
                {Body, Bs} ->
                    eval_exprs(Body, Bs, LF, none, none);
                nomatch ->
                    erlang:error({function_clause,[{local,Name,Args}]})
            end;
        error ->
            case maps:find({remote,{Name,Arity}}, Map) of
                {ok, Mod} ->
                    %% io:format(standard_error, "Calling:~p~n",[{Mod,Name,Args}]),
                    apply(Mod, Name, Args);
                error ->
                    io:format(standard_error, "Script does not export ~tw/~w\n", [Name,Arity]),
                    my_halt(127)
            end
    end.

eval_exprs([E], Bs0, Lf, Ef, _RBs) ->
    RBs1 = value,
    erl_eval:expr(E, Bs0, Lf, Ef, RBs1);
eval_exprs([E|Es], Bs0, Lf, Ef, RBs) ->
    RBs1 = none,
    {value,_V,Bs} = erl_eval:expr(E, Bs0, Lf, Ef, RBs1),
    eval_exprs(Es, Bs, Lf, Ef, RBs).

format_exception(Class, Reason, StackTrace) ->
    Enc = encoding(),
    P = case Enc of
            latin1 -> "P";
            _ -> "tP"
        end,
    PF = fun(Term, I) ->
                 io_lib:format("~." ++ integer_to_list(I) ++ P, [Term, 50])
         end,
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    erl_error:format_exception(1, Class, Reason, StackTrace, StackFun, PF, Enc).

encoding() ->
    case io:getopts() of
        {error, _}=_Err ->
            latin1;
        Opts ->
            case lists:keyfind(encoding, 1, Opts) of
                false -> latin1;
                {encoding, Encoding} -> Encoding
            end
    end.

put_chars(String) ->
    try
        io:put_chars(standard_error, String)
    catch
        _:_ ->
            display_err(lists:flatten(String))
    end.

display_err(String) ->
    Port = open_port({fd,2,2}, [out,binary]),
    Port ! {self(), {command, list_to_binary(String)}},
    port_close(Port).

a0() ->
    anno(0).

a1() ->
    anno(1).

anno(L) ->
    erl_anno:new(L).

fatal(Str) ->
    throw(Str).

-spec my_halt(_) -> no_return().
my_halt(Reason) ->
    erlang:halt(Reason).

hidden_apply(App, M, F, Args) ->
    try
	apply(fun() -> M end(), F, Args)
    catch
	error:undef:StackTrace ->
	    case StackTrace of
		[{M,F,Args,_} | _] ->
		    Arity = length(Args),
		    Text = io_lib:format("Call to ~w:~w/~w in application ~w failed.\n",
					 [M, F, Arity, App]),
		    fatal(Text);
		Stk ->
		    erlang:raise(error, undef, Stk)
	    end
    end.
