%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @copyright 2001-2007 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @version {@version}
%% @end
%% =====================================================================

%% TODO: check weirdness in name generation for @spec f(TypeName, ...) -> ...
%% TODO: option for ignoring functions matching some pattern ('..._test_'/0)
%% TODO: @private_type tag, opaque unless generating private docs?
%% TODO: document the record type syntax
%% TODO: some 'skip' option for ignoring particular modules/packages?
%% TODO: intermediate-level packages: document even if no local sources.
%% TODO: multiline comment support (needs modified comment representation)
%% TODO: config-file for default settings
%% TODO: config: locations of all local docdirs; generate local doc-index page
%% TODO: config: URL:s of offline packages/apps
%% TODO: config: default stylesheet
%% TODO: config: default header/footer, etc.
%% TODO: offline linkage
%% TODO: including source code, explicitly and/or automatically

%% @doc EDoc - the Erlang program documentation generator.
%%
%% This module provides the main user interface to EDoc.
%% <ul>
%%   <li><a href="overview-summary.html">EDoc User Manual</a></li>
%%   <li><a href="overview-summary.html#Running_EDoc">Running EDoc</a></li>
%% </ul>

-module(edoc).

-export([packages/1, packages/2, files/1, files/2,
	 application/1, application/2, application/3,
	 toc/1, toc/2, toc/3,
	 run/3,
	 file/1, file/2,
	 read/1, read/2,
	 layout/1, layout/2,
	 get_doc/1, get_doc/2, get_doc/3,
	 read_comments/1, read_comments/2,
	 read_source/1, read_source/2]).

-compile({no_auto_import,[error/1]}).

-include("edoc.hrl").


%% @spec (Name::filename()) -> ok
%% @equiv file(Name, [])
%% @deprecated See {@link file/2} for details.

file(Name) ->
    file(Name, []).

%% @spec file(filename(), proplist()) -> ok 
%%
%% @type filename() = //kernel/file:filename()
%% @type proplist() = [term()]
%%
%% @deprecated This is part of the old interface to EDoc and is mainly
%% kept for backwards compatibility. The preferred way of generating
%% documentation is through one of the functions {@link application/2},
%% {@link packages/2} and {@link files/2}.
%%
%% @doc Reads a source code file and outputs formatted documentation to
%% a corresponding file.
%%
%% Options:
%% <dl>
%%  <dt>{@type {dir, filename()@}}
%%  </dt>
%%  <dd>Specifies the output directory for the created file. (By
%%      default, the output is written to the directory of the source
%%      file.)
%%  </dd>
%%  <dt>{@type {source_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the expected suffix of the input file. The default
%%      value is `".erl"'.
%%  </dd>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix for the created file. The default value is
%%      `".html"'.
%%  </dd>
%% </dl>
%%
%% See {@link get_doc/2} and {@link layout/2} for further
%% options.
%%
%% For running EDoc from a Makefile or similar, see
%% {@link edoc_run:file/1}.
%%
%% @see read/2

%% NEW-OPTIONS: source_suffix, file_suffix, dir
%% INHERIT-OPTIONS: read/2

file(Name, Options) ->
    Text = read(Name, Options),
    SrcSuffix = proplists:get_value(source_suffix, Options,
				    ?DEFAULT_SOURCE_SUFFIX),
    BaseName = filename:basename(Name, SrcSuffix),
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Dir = proplists:get_value(dir, Options, filename:dirname(Name)),
    edoc_lib:write_file(Text, Dir, BaseName ++ Suffix).


%% TODO: better documentation of files/1/2, packages/1/2, application/1/2/3

%% @spec (Files::[filename() | {package(), [filename()]}]) -> ok
%% @equiv packages(Packages, [])

files(Files) ->
    files(Files, []).

%% @spec (Files::[filename() | {package(), [filename()]}],
%%        Options::proplist()) -> ok
%% @doc Runs EDoc on a given set of source files. See {@link run/3} for
%% details, including options.
%% @equiv run([], Files, Options)

files(Files, Options) ->
    run([], Files, Options).

%% @spec (Packages::[package()]) -> ok
%% @equiv packages(Packages, [])

packages(Packages) ->
    packages(Packages, []).

%% @spec (Packages::[package()], Options::proplist()) -> ok
%% @type package() = atom() | string()
%%
%% @doc Runs EDoc on a set of packages. The `source_path' option is used
%% to locate the files; see {@link run/3} for details, including
%% options. This function automatically appends the current directory to
%% the source path.
%%
%% @equiv run(Packages, [], Options)

packages(Packages, Options) ->
    run(Packages, [], Options  ++ [{source_path, [?CURRENT_DIR]}]).

%% @spec (Application::atom()) -> ok
%% @equiv application(Application, [])

application(App) ->
    application(App, []).

%% @spec (Application::atom(), Options::proplist()) -> ok
%% @doc Run EDoc on an application in its default app-directory. See
%% {@link application/3} for details.
%% @see application/1

application(App, Options) when is_atom(App) ->
    case code:lib_dir(App) of
 	Dir when is_list(Dir) ->
 	    application(App, Dir, Options);
 	_ ->
	    edoc_report:report("cannot find application directory for '~s'.",
                               [App]),
 	    exit(error)
    end.

%% @spec (Application::atom(), Dir::filename(), Options::proplist())
%%        -> ok
%% @doc Run EDoc on an application located in the specified directory.
%% Tries to automatically set up good defaults. Unless the user
%% specifies otherwise:
%% <ul>
%%   <li>The `doc' subdirectory will be used as the target directory, if
%%   it exists; otherwise the application directory is used.
%%   </li>
%%   <li>The source code is assumed to be located in the `src'
%%   subdirectory, if it exists, or otherwise in the application
%%   directory itself.
%%   </li>
%%   <li>The {@link run/3. `subpackages'} option is turned on. All found
%%   source files will be processed.
%%   </li>
%%   <li>The `include' subdirectory is automatically added to the
%%   include path. (Only important if {@link read_source/2.
%%   preprocessing} is turned on.)
%%   </li>
%% </ul>
%%
%% See {@link run/3} for details, including options.
%%
%% @see application/2

application(App, Dir, Options) when is_atom(App) ->
    Src = edoc_lib:try_subdir(Dir, ?SOURCE_DIR),
    Overview = filename:join(edoc_lib:try_subdir(Dir, ?EDOC_DIR),
			     ?OVERVIEW_FILE),
    Opts = Options ++ [{source_path, [Src]},
		       subpackages,
		       {title, io_lib:fwrite("The ~s application", [App])},
		       {overview, Overview},
		       {dir, filename:join(Dir, ?EDOC_DIR)},
		       {includes, [filename:join(Dir, "include")]}],
    Opts1 = set_app_default(App, Dir, Opts),
    %% Recursively document all subpackages of '' - i.e., everything.
    run([''], [], [{application, App} | Opts1]).

%% Try to set up a default application base URI in a smart way if the
%% user has not specified it explicitly.

set_app_default(App, Dir0, Opts) ->
    case proplists:get_value(app_default, Opts) of
	undefined ->
	    AppName = atom_to_list(App),
	    Dir = edoc_lib:simplify_path(filename:absname(Dir0)),
	    AppDir = case filename:basename(Dir) of
			 AppName ->
			     filename:dirname(Dir);
			 _ ->
			     ?APP_DEFAULT
		     end,
	    [{app_default, AppDir} | Opts];
	_ ->
	    Opts
    end.

%% If no source files are found for a (specified) package, no package
%% documentation will be generated either (even if there is a
%% package-documentation file). This is the way it should be. For
%% specified files, use empty package (unless otherwise specified). The
%% assumed package is always used for creating the output. If the actual
%% module or package of the source differs from the assumption gathered
%% from the path and file name, a warning should be issued (since links
%% are likely to be incorrect).

opt_defaults() ->
    [packages].

opt_negations() ->
    [{no_preprocess, preprocess},
     {no_subpackages, subpackages},
     {no_report_missing_types, report_missing_types},
     {no_packages, packages}].

%% @spec run(Packages::[package()],
%%           Files::[filename() | {package(), [filename()]}],
%%           Options::proplist()) -> ok
%% @doc Runs EDoc on a given set of source files and/or packages. Note
%% that the doclet plugin module has its own particular options; see the
%% `doclet' option below.
%% 
%% Also see {@link layout/2} for layout-related options, and
%% {@link get_doc/2} for options related to reading source
%% files.
%%
%% Options:
%% <dl>
%%  <dt>{@type {app_default, string()@}}
%%  </dt>
%%  <dd>Specifies the default base URI for unknown applications.
%%  </dd>
%%  <dt>{@type {application, App::atom()@}}
%%  </dt>
%%  <dd>Specifies that the generated documentation describes the
%%      application `App'. This mainly affects generated references.
%%  </dd>
%%  <dt>{@type {dir, filename()@}}
%%  </dt>
%%  <dd>Specifies the target directory for the generated documentation.
%%  </dd>
%%  <dt>{@type {doc_path, [string()]@}}
%%  </dt>
%%  <dd>Specifies a list of URI:s pointing to directories that contain
%%      EDoc-generated documentation. URI without a `scheme://' part are
%%      taken as relative to `file://'. (Note that such paths must use
%%      `/' as separator, regardless of the host operating system.)
%%  </dd>
%%  <dt>{@type {doclet, Module::atom()@}}
%%  </dt>
%%  <dd>Specifies a callback module to be used for creating the
%%      documentation. The module must export a function `run(Cmd, Ctxt)'.
%%      The default doclet module is {@link edoc_doclet}; see {@link
%%      edoc_doclet:run/2} for doclet-specific options.
%%  </dd>
%%  <dt>{@type {exclude_packages, [package()]@}}
%%  </dt>
%%  <dd>Lists packages to be excluded from the documentation. Typically
%%      used in conjunction with the `subpackages' option.
%%  </dd>
%%  <dt>{@type {file_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the suffix used for output files. The default value is
%%      `".html"'. Note that this also affects generated references.
%%  </dd>
%%  <dt>{@type {new, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', any existing `edoc-info' file in the
%%      target directory will be ignored and overwritten. The default
%%      value is `false'.
%%  </dd>
%%  <dt>{@type {packages, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', it it assumed that packages (module
%%      namespaces) are being used, and that the source code directory
%%      structure reflects this. The default value is `true'. (Usually,
%%      this does the right thing even if all the modules belong to the
%%      top-level "empty" package.) `no_packages' is an alias for
%%      `{packages, false}'. See the `subpackages' option below for
%%      further details.
%%
%%      If the source code is organized in a hierarchy of
%%      subdirectories although it does not use packages, use
%%      `no_packages' together with the recursive-search `subpackages'
%%      option (on by default) to automatically generate documentation
%%      for all the modules.
%%  </dd>
%%  <dt>{@type {source_path, [filename()]@}}
%%  </dt>
%%  <dd>Specifies a list of file system paths used to locate the source
%%      code for packages.
%%  </dd>
%%  <dt>{@type {source_suffix, string()@}}
%%  </dt>
%%  <dd>Specifies the expected suffix of input files. The default
%%      value is `".erl"'.
%%  </dd>
%%  <dt>{@type {subpackages, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', all subpackages of specified packages
%%      will also be included in the documentation. The default value is
%%      `false'. `no_subpackages' is an alias for `{subpackages,
%%      false}'. See also the `exclude_packages' option.
%%
%%      Subpackage source files are found by recursively searching
%%      for source code files in subdirectories of the known source code
%%      root directories. (Also see the `source_path' option.) Directory
%%      names must begin with a lowercase letter and contain only
%%      alphanumeric characters and underscore, or they will be ignored.
%%      (For example, a subdirectory named `test-files' will not be
%%      searched.)
%%  </dd>
%% </dl>
%%
%% @see files/2
%% @see packages/2
%% @see application/2

%% NEW-OPTIONS: source_path, application
%% INHERIT-OPTIONS: init_context/1
%% INHERIT-OPTIONS: expand_sources/2
%% INHERIT-OPTIONS: target_dir_info/5
%% INHERIT-OPTIONS: edoc_lib:find_sources/3
%% INHERIT-OPTIONS: edoc_lib:run_doclet/2
%% INHERIT-OPTIONS: edoc_lib:get_doc_env/4

run(Packages, Files, Opts0) ->
    Opts = expand_opts(Opts0),
    Ctxt = init_context(Opts),
    Dir = Ctxt#context.dir,
    Path = proplists:append_values(source_path, Opts),
    Ss = sources(Path, Packages, Opts),
    {Ss1, Ms} = expand_sources(expand_files(Files) ++ Ss, Opts),
    Ps = [P || {_, P, _, _} <- Ss1],
    App = proplists:get_value(application, Opts, ?NO_APP),
    {App1, Ps1, Ms1} = target_dir_info(Dir, App, Ps, Ms, Opts),
    %% The "empty package" is never included in the list of packages.
    Ps2 = edoc_lib:unique(lists:sort(Ps1)) -- [''],
    Ms2 = edoc_lib:unique(lists:sort(Ms1)),
    Fs = package_files(Path, Ps2),
    Env = edoc_lib:get_doc_env(App1, Ps2, Ms2, Opts),
    Ctxt1 = Ctxt#context{env = Env},
    Cmd = #doclet_gen{sources = Ss1,
		      app = App1,
		      packages = Ps2,
		      modules = Ms2,
		      filemap = Fs
		     },
    F = fun (M) ->
		M:run(Cmd, Ctxt1)
	end,
    edoc_lib:run_doclet(F, Opts).

expand_opts(Opts0) ->
    proplists:substitute_negations(opt_negations(),
				   Opts0 ++ opt_defaults()).

%% NEW-OPTIONS: dir
%% DEFER-OPTIONS: run/3

init_context(Opts) ->
    #context{dir = proplists:get_value(dir, Opts, ?CURRENT_DIR),
	     opts = Opts
	    }.

%% INHERIT-OPTIONS: edoc_lib:find_sources/3

sources(Path, Packages, Opts) ->
    lists:foldl(fun (P, Xs) ->
			edoc_lib:find_sources(Path, P, Opts) ++ Xs
		end,
		[], Packages).

package_files(Path, Packages) ->
    Name = ?PACKAGE_FILE,    % this is hard-coded for now
    D = lists:foldl(fun (P, D) ->
			    F = edoc_lib:find_file(Path, P, Name),
			    dict:store(P, F, D)
		    end,
		    dict:new(), Packages),
    fun (P) ->
	    case dict:find(P, D) of
		{ok, F} -> F;
		error -> ""
	    end
    end.

%% Expand user-specified sets of files.

expand_files([{P, Fs1} | Fs]) ->
    [{P, filename:basename(F), filename:dirname(F)} || F <- Fs1]
	++ expand_files(Fs);
expand_files([F | Fs]) ->
    [{'', filename:basename(F), filename:dirname(F)} |
     expand_files(Fs)];
expand_files([]) ->
    [].

%% Create the (assumed) full module names. Keep only the first source
%% for each module, but preserve the order of the list.

%% NEW-OPTIONS: source_suffix, packages
%% DEFER-OPTIONS: run/3

expand_sources(Ss, Opts) ->
    Suffix = proplists:get_value(source_suffix, Opts,
				 ?DEFAULT_SOURCE_SUFFIX),
    Ss1 = case proplists:get_bool(packages, Opts) of
	      true -> Ss;
	      false -> [{'',F,D} || {_P,F,D} <- Ss]
	  end,
    expand_sources(Ss1, Suffix, sets:new(), [], []).

expand_sources([{P, F, D} | Fs], Suffix, S, As, Ms) ->
    M = list_to_atom(packages:concat(P, filename:rootname(F, Suffix))),
    case sets:is_element(M, S) of
	true ->
	    expand_sources(Fs, Suffix, S, As, Ms);
	false ->
	    S1 = sets:add_element(M, S),
	    expand_sources(Fs, Suffix, S1, [{M, P, F, D} | As],
			   [M | Ms])
    end;
expand_sources([], _Suffix, _S, As, Ms) ->
    {lists:reverse(As), lists:reverse(Ms)}.

%% NEW-OPTIONS: new

target_dir_info(Dir, App, Ps, Ms, Opts) ->
    case proplists:get_bool(new, Opts) of
	true ->
	    {App, Ps, Ms};
	false ->
	    {App1, Ps1, Ms1} = edoc_lib:read_info_file(Dir),
	    {if App == ?NO_APP -> App1;
		true -> App
	     end,
	     Ps ++ Ps1,
	     Ms ++ Ms1}
    end.


%% @hidden   Not official yet

toc(Dir) ->
    toc(Dir, []).

%% @equiv toc(Dir, Paths, [])
%% @hidden   Not official yet

%% NEW-OPTIONS: doc_path

toc(Dir, Opts) ->
    Paths = proplists:append_values(doc_path, Opts)
	++ edoc_lib:find_doc_dirs(),
    toc(Dir, Paths, Opts).

%% @doc Create a meta-level table of contents.
%% @hidden   Not official yet

%% INHERIT-OPTIONS: init_context/1
%% INHERIT-OPTIONS: edoc_lib:run_doclet/2
%% INHERIT-OPTIONS: edoc_lib:get_doc_env/4

toc(Dir, Paths, Opts0) ->
    Opts = expand_opts(Opts0 ++ [{dir, Dir}]),
    Ctxt = init_context(Opts),
    Env = edoc_lib:get_doc_env('', [], [], Opts),
    Ctxt1 = Ctxt#context{env = Env},
    F = fun (M) ->
		M:run(#doclet_toc{paths=Paths}, Ctxt1)
	end,
    edoc_lib:run_doclet(F, Opts).


%% @spec read(File::filename()) -> string()
%% @equiv read(File, [])

read(File) ->
    read(File, []).

%% @spec read(File::filename(), Options::proplist()) -> string()
%%
%% @doc Reads and processes a source file and returns the resulting
%% EDoc-text as a string. See {@link get_doc/2} and {@link layout/2} for
%% options.
%%
%% @see file/2

%% INHERIT-OPTIONS: get_doc/2, layout/2

read(File, Opts) ->
    {_ModuleName, Doc} = get_doc(File, Opts),
    layout(Doc, Opts).


%% @spec layout(Doc::edoc_module()) -> string()
%% @equiv layout(Doc, [])

layout(Doc) ->
    layout(Doc, []).

%% @spec layout(Doc::edoc_module(), Options::proplist()) -> string()
%%
%% @doc Transforms EDoc module documentation data to text. The default
%% layout creates an HTML document.
%%
%% Options:
%% <dl>
%%  <dt>{@type {layout, Module::atom()@}}
%%  </dt>
%%  <dd>Specifies a callback module to be used for formatting. The
%%      module must export a function `module(Doc, Options)'. The
%%      default callback module is {@link edoc_layout}; see {@link
%%      edoc_layout:module/2} for layout-specific options.
%%  </dd>
%% </dl>
%%
%% @see layout/1
%% @see run/3
%% @see read/2
%% @see file/2

%% INHERIT-OPTIONS: edoc_lib:run_layout/2

layout(Doc, Opts) ->
    F = fun (M) ->
		M:module(Doc, Opts)
	end,
    edoc_lib:run_layout(F, Opts).


%% @spec (File) ->  [comment()]
%% @type comment() = {Line, Column, Indentation, Text}
%% where
%%   Line = integer(),
%%   Column = integer(),
%%   Indentation = integer(),
%%   Text = [string()]
%% @equiv read_comments(File, [])

read_comments(File) ->
    read_comments(File, []).

%% @spec read_comments(File::filename(), Options::proplist()) ->
%%           [comment()]
%%
%% @doc Extracts comments from an Erlang source code file. See the
%% module {@link //syntax_tools/erl_comment_scan} for details on the
%% representation of comments. Currently, no options are avaliable.

read_comments(File, _Opts) ->
    erl_comment_scan:file(File).


%% @spec (File) -> [syntaxTree()]
%% @equiv read_source(File, [])

read_source(Name) ->
    read_source(Name, []).

%% @spec read_source(File::filename(), Options::proplist()) ->
%%           [syntaxTree()]
%%
%% @type syntaxTree() = //syntax_tools/erl_syntax:syntaxTree()
%%
%% @doc Reads an Erlang source file and returns the list of "source code
%% form" syntax trees.
%%
%% Options:
%% <dl>
%%  <dt>{@type {preprocess, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', the source file will be read via the
%%      Erlang preprocessor (`epp'). The default value is `false'.
%%      `no_preprocess' is an alias for `{preprocess, false}'.
%%
%%      Normally, preprocessing is not necessary for EDoc to work, but
%%      if a file contains too exotic definitions or uses of macros, it
%%      will not be possible to read it without preprocessing. <em>Note:
%%      comments in included files will not be available to EDoc, even
%%      with this option enabled.</em>
%%  </dd>
%%  <dt>{@type {includes, Path::[string()]@}}
%%  </dt>
%%  <dd>Specifies a list of directory names to be searched for include
%%      files, if the `preprocess' option is turned on. Also used with
%%      the `@headerfile' tag. The default value is the empty list. The
%%      directory of the source file is always automatically appended to
%%      the search path.
%%  </dd>
%%  <dt>{@type {macros, [{atom(), term()@}]@}}
%%  </dt>
%%  <dd>Specifies a list of pre-defined Erlang preprocessor (`epp')
%%      macro definitions, used if the `preprocess' option is turned on.
%%      The default value is the empty list.</dd>
%% </dl>
%%  <dt>{@type {report_missing_types, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', warnings are issued for missing types.
%%      The default value is `false'.
%%      `no_report_missing_types' is an alias for
%%      `{report_missing_types, false}'.
%%  </dd>
%%
%% @see get_doc/2
%% @see //syntax_tools/erl_syntax

%% NEW-OPTIONS: [no_]preprocess (preprocess -> includes, macros)

read_source(Name, Opts0) ->
    Opts = expand_opts(Opts0),
    case read_source_1(Name, Opts) of
	{ok, Forms} ->
	    check_forms(Forms, Name),
	    Forms;
	{error, R} ->
	    edoc_report:error({"error reading file '~s'.",
                               [edoc_lib:filename(Name)]}),
	    exit({error, R})
    end.

read_source_1(Name, Opts) ->
    case proplists:get_bool(preprocess, Opts) of
	true ->
	    read_source_2(Name, Opts);
	false ->
	    epp_dodger:quick_parse_file(Name, Opts ++ [{no_fail, false}])
    end.

read_source_2(Name, Opts) ->
    Includes = proplists:append_values(includes, Opts)
	++ [filename:dirname(Name)],
    Macros = proplists:append_values(macros, Opts),
    epp:parse_file(Name, Includes, Macros).

check_forms(Fs, Name) ->
    Fun = fun (F) ->
	     case erl_syntax:type(F) of
		 error_marker ->
		     case erl_syntax:error_marker_info(F) of
			 {L, M, D} ->
			     edoc_report:error(L, Name, {format_error, M, D});
			 Other ->
			     edoc_report:report(Name, "unknown error in "
                                                "source code: ~w.", [Other])
		     end,
		     exit(error);
		 _ ->
		     ok
	     end
	  end,
    lists:foreach(Fun, Fs).


%% @spec get_doc(File::filename()) -> {ModuleName, edoc_module()}
%% @equiv get_doc(File, [])

get_doc(File) ->
    get_doc(File, []).

%% @spec get_doc(File::filename(), Options::proplist()) ->
%%           {ModuleName, edoc_module()}
%%	ModuleName = atom()
%%
%% @type edoc_module(). The EDoc documentation data for a module,
%% expressed as an XML document in {@link //xmerl. XMerL} format. See
%% the file <a href="../priv/edoc.dtd">`edoc.dtd'</a> for details.
%%
%% @doc Reads a source code file and extracts EDoc documentation data.
%% Note that without an environment parameter (see {@link get_doc/3}),
%% hypertext links may not be correct.
%%
%% Options:
%% <dl>
%%  <dt>{@type {def, Macros@}}
%%  </dt>
%%  <dd><ul>
%%       <li>`Macros' = {@type Macro | [Macro]}</li>
%%       <li>`Macro' = {@type {Name::atom(), Text::string()@}}</li>
%%      </ul>
%%    Specifies a set of EDoc macro definitions. See
%%    <a href="overview-summary.html#Macro_expansion">Inline macro expansion</a>
%%    for details.
%%  </dd>
%%  <dt>{@type {hidden, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of hidden functions will
%%      also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {private, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', documentation of private functions will
%%      also be included. The default value is `false'.
%%  </dd>
%%  <dt>{@type {todo, boolean()@}}
%%  </dt>
%%  <dd>If the value is `true', To-Do notes written using `@todo' or
%%  `@TODO' tags will be included in the documentation. The default
%%  value is `false'.
%%  </dd>
%% </dl>
%%
%% See {@link read_source/2}, {@link read_comments/2} and {@link
%% edoc_lib:get_doc_env/4} for further options.
%%
%% @see get_doc/3
%% @see run/3
%% @see edoc_extract:source/5
%% @see read/2
%% @see layout/2

%% INHERIT-OPTIONS: get_doc/3
%% INHERIT-OPTIONS: edoc_lib:get_doc_env/4

get_doc(File, Opts) ->
    Env = edoc_lib:get_doc_env(Opts),
    get_doc(File, Env, Opts).

%% @spec get_doc(File::filename(), Env::edoc_lib:edoc_env(),
%%        Options::proplist()) -> {ModuleName, edoc_module()}
%%     ModuleName = atom()
%%
%% @doc Like {@link get_doc/2}, but for a given environment
%% parameter. `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/4}.

%% INHERIT-OPTIONS: read_source/2, read_comments/2, edoc_extract:source/5
%% DEFER-OPTIONS: get_doc/2

get_doc(File, Env, Opts) ->
    edoc_extract:source(File, Env, Opts).
