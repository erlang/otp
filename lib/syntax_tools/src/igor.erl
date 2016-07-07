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
%% @copyright 1998-2014 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

%% @doc Igor: the Module Merger and Renamer.
%%
%% The program Igor merges the source code of one or more Erlang
%% modules into a single module, which can then replace the original set
%% of modules. Igor is also able to rename a set of (possibly
%% interdependent) modules, without joining them into a single
%% module.
%%
%% The main user interface consists of the functions {@link merge/3} and
%% {@link rename/3}. See also the function {@link parse_transform/2}.
%%
%% A note of warning: Igor cannot do anything about the case when the
%% name of a remote function is passed to the built-in functions
%% `apply' and `spawn' <em>unless</em> the module
%% and function names are explicitly stated in the call, as in e.g.
%% `apply(lists, reverse, [Xs])'. In all other cases, Igor
%% leaves such calls unchanged, and warns the user that manual editing
%% might be necessary.
%%
%% Also note that Erlang records will be renamed as necessary to
%% avoid non-equivalent definitions using the same record name. This
%% does not work if the source code accesses the name field of such
%% record tuples by `element/2' or similar methods. Always
%% use the record syntax to handle record tuples, if possible.
%%
%% Disclaimer: the author of this program takes no responsibility for
%% the correctness of the produced output, or for any effects of its
%% execution. In particular, the author may not be held responsible
%% should Igor include the code of a deceased madman in the result.
%%
%% For further information on Igors in general, see e.g. "Young
%% Frankenstein", Mel Brooks, 1974, and "The Fifth Elephant", Terry
%% Pratchett, 1999.
%% @end
%% =====================================================================


%% This program is named after the character Igor, assistant to Dr.
%% Frankenstein, in the 1939 film "Son of Frankenstein" (with Boris
%% Karloff playing The Monster for the last time; Igor was played by
%% Bela Lugosi). Igor's job (in the film) was mainly to bring reasonably
%% fresh parts of various human corpses to the good Doctor, for his
%% purpose of reanimating them in the shape of a new formidable, living
%% creature.
%%
%% Merging code is done by joining the sources, possibly changing the
%% order of declarations as necessary, renaming functions and records to
%% avoid name clashes, and changing remote calls to local calls where
%% possible. Stub modules may be automatically generated to redirect any
%% calls that still use the old names. Indirectly, code merging can be
%% used to simply rename a set of modules.
%%
%% What Igor does not do is to optimise the resulting code, which
%% typically can benefit from techniques such as inlining, constant
%% folding, specialisation, etc. This task is left to the Doctor.
%% (Luckily, Igor can call on Inga to do some cleanup; cf. 'erl_tidy'.)

%% TODO: FIXME: don't remove module qualifier if name is (auto-)imported!
%% TODO: handle merging of parameterized modules (somehow).
%% TODO: check for redefinition of macros; check equivalence; comment out.
%% TODO: {export, [E]}, E = atom() | {atom(), atom(), integer()}.
%% TODO: improve documentation. 
%% TODO: optionally rename all functions from specified (or all) modules.

-module(igor).

-export([create_stubs/2, merge/2, merge/3, merge_files/3, merge_files/4,
	 merge_sources/3, parse_transform/2, rename/2, rename/3]).

-include_lib("kernel/include/file.hrl").


%% =====================================================================
%% Global Constants

-define(NOTE_HEADER, "Note from Igor: ").
-define(COMMENT_PREFIX, "% ").
-define(COMMENT_BAR,
	"======================="
	"======================="
	"=======================").
-define(NOTE_PREFIX, "%! ").
-define(KILL_PREFIX, "%<<< ").
-define(DEFAULT_INCLUDES, ["."]).
-define(DEFAULT_MACROS, []).
-define(DEFAULT_SUFFIX, ".erl").
-define(DEFAULT_BACKUP_SUFFIX, ".bak").
-define(DEFAULT_DIR, "").
-define(DEFAULT_STUB_DIR, "stubs").
-define(TIDY_OPTS, [quiet]).

%% This may also be used in patterns. R must not be an integer, i.e.,
%% the structure must be distinct from function names.

-define(record_name(R), {record, R}).

%% =====================================================================

%% Data structure for module information

-record(module, {name        :: atom(),
		 vars = none :: [atom()] | 'none',
		 functions   :: ordsets:ordset({atom(), arity()}),
		 exports     :: ordsets:ordset({atom(), arity()})
			      | ordsets:ordset({{atom(), arity()}, term()}),
		 aliases     :: ordsets:ordset({{atom(), arity()},
						{atom(), {atom(), arity()}}}),
		 attributes  :: ordsets:ordset({atom(), term()}),
		 records     :: [{atom(), [{atom(), term()}]}]
		}).

%% The default pretty-printing function.

default_printer(Tree, Options) ->
    erl_prettypr:format(Tree, Options).

%% =====================================================================

-type option() :: atom() | {atom(), term()}.

-type attribute()      :: {atom(), term()}.
-type moduleName()     :: atom().
-type functionName()   :: {atom(), arity()}.
-type functionPair()   :: {functionName(), {moduleName(), functionName()}}.
-type stubDescriptor() :: {moduleName(), [functionPair()], [attribute()]}.

-type notes() :: 'always' | 'yes' | 'no'.

%% =====================================================================
%% @spec parse_transform(Forms::[syntaxTree()], Options::[term()]) ->
%%           [syntaxTree()]
%%
%% @type syntaxTree() = erl_syntax:syntaxTree(). An abstract syntax
%% tree. See the {@link erl_syntax} module for details.
%%
%% @doc Allows Igor to work as a component of the Erlang compiler.
%% Including the term `{parse_transform, igor}' in the
%% compile options when compiling an Erlang module (cf.
%% `compile:file/2'), will call upon Igor to process the
%% source code, allowing automatic inclusion of other source files. No
%% files are created or overwritten when this function is used.
%%
%% Igor will look for terms `{igor, List}' in the compile
%% options, where `List' is a list of Igor-specific options,
%% as follows:
%% <dl>
%%  <dt>`{files, [filename()]}'</dt>
%%    <dd>The value specifies a list of source files to be merged with
%%    the file being compiled; cf. `merge_files/4'.</dd>
%% </dl>
%%
%% See `merge_files/4' for further options. Note, however,
%% that some options are preset by this function and cannot be
%% overridden by the user; in particular, all cosmetic features are
%% turned off, for efficiency. Preprocessing is turned on.
%%
%% @see merge_files/4
%% @see //compiler/compile:file/2

-spec parse_transform(erl_syntax:forms(), [option()]) ->
        [erl_syntax:syntaxTree()].

parse_transform(Forms, Options) ->
    M = get_module_info(Forms),
    Name = M#module.name,
    Opts = proplists:append_values(igor, Options),
    Files = proplists:append_values(files, Opts),
    %% We turn off all features that are only cosmetic, and make sure to
    %% turn on preservation of `file' attributes.
    Opts1 = [{comments, false},
	     {notes, no},
	     {no_imports, true},
	     {file_attributes, yes},
	     {preprocess, true},
	     {export, [Name]}
	     | Opts],
    {T, _} = merge_files(Name, [Forms], Files, Opts1),
    verbose("done.", Opts1),
    erl_syntax:revert_forms(T).


%% =====================================================================
%% @spec merge(Name::atom(), Files::[filename()]) -> [filename()]
%% @equiv merge(Name, Files, [])

-spec merge(atom(), [file:filename()]) -> [file:filename(),...].

merge(Name, Files) ->
    merge(Name, Files, []).

%% =====================================================================
%% @spec merge(Name::atom(), Files::[filename()], Options::[term()]) ->
%%           [filename()]
%%
%% @type filename() = file:filename()
%%
%% @doc Merges source code files to a single file. `Name'
%% specifies the name of the resulting module - not the name of the
%% output file. `Files' is a list of file names and/or module
%% names of source modules to be read and merged (see
%% `merge_files/4' for details). All the input modules must
%% be distinctly named.
%%
%% The resulting source code is written to a file named
%% "`<em>Name</em>.erl'" in the current directory, unless
%% otherwise specified by the options `dir' and
%% `outfile' described below.
%%
%% Examples:
%% <ul>
%%   <li>given a module `m' in file "`m.erl'"
%%   which uses the standard library module `lists', calling
%%   `igor:merge(m, [m, lists])' will create a new file
%%   "`m.erl' which contains the code from `m' and
%%   exports the same functions, and which includes the referenced code
%%   from the `lists' module. The original file will be
%%   renamed to "`m.erl.bak'".</li>
%%
%%   <li>given modules `m1' and `m2', in
%%   corresponding files, calling `igor:merge(m, [m1, m2])'
%%   will create a file "`m.erl'" which contains the code
%%   from `m1' and `m2' and exports the functions
%%   of `m1'.</li>
%% </ul>
%%
%% Stub module files are created for those modules that are to be
%% exported by the target module (see options `export',
%% `stubs' and `stub_dir').
%%
%% The function returns the list of file names of all created
%% modules, including any automatically created stub modules. The file
%% name of the target module is always first in the list.
%%
%% Note: If you get a "syntax error" message when trying to merge
%% files (and you know those files to be correct), then try the
%% `preprocess' option. It typically means that your code
%% contains too strange macros to be handled without actually performing
%% the preprocessor expansions.
%% 
%% Options:
%% <dl>
%%   <dt>`{backup_suffix, string()}'</dt>
%%
%%     <dd>Specifies the file name suffix to be used when a backup file
%%     is created; the default value is `".bak"'.</dd>
%%
%%   <dt>`{backups, boolean()}'</dt>
%%
%%     <dd>If the value is `true', existing files will be
%%     renamed before new files are opened for writing. The new names
%%     are formed by appending the string given by the
%%     `backup_suffix' option to the original name. The
%%     default value is `true'.</dd>
%%
%%   <dt>`{dir, filename()}'</dt>
%%
%%     <dd>Specifies the name of the directory in which the output file
%%     is to be written. An empty string is interpreted as the current
%%     directory. By default, the current directory is used.</dd>
%%
%%   <dt>`{outfile, filename()}'</dt>
%%
%%     <dd>Specifies the name of the file (without suffix) to which the
%%     resulting source code is to be written. By default, this is the
%%     same as the `Name' argument.</dd>
%%
%%   <dt>`{preprocess, boolean()}'</dt>
%%
%%     <dd>If the value is `true', preprocessing will be done
%%     when reading the source code. See `merge_files/4' for
%%     details.</dd>
%%
%%   <dt>`{printer, Function}'</dt>
%%     <dd><ul>
%%       <li>`Function = (syntaxTree()) -> string()'</li>
%%     </ul>
%%     Specifies a function for prettyprinting Erlang syntax trees.
%%     This is used for outputting the resulting module definition, as
%%     well as for creating stub files. The function is assumed to
%%     return formatted text for the given syntax tree, and should raise
%%     an exception if an error occurs. The default formatting function
%%     calls `erl_prettypr:format/2'.</dd>
%%
%%   <dt>`{stub_dir, filename()}'</dt>
%%
%%     <dd>Specifies the name of the directory to which any generated
%%     stub module files are written. The default value is
%%     `"stubs"'.</dd>
%%
%%   <dt>`{stubs, boolean()}'</dt>
%%
%%     <dd>If the value is `true', stub module files will be
%%     automatically generated for all exported modules that do not have
%%     the same name as the target module. The default value is
%%     `true'.</dd>
%%
%%   <dt>`{suffix, string()}'</dt>
%%
%%     <dd>Specifies the suffix to be used for the output file names;
%%     the default value is `".erl"'.</dd>
%% </dl>
%%
%% See `merge_files/4' for further options.
%%
%% @see merge/2
%% @see merge_files/4

%% The defaults for 'merge' are also used for 'create_stubs'.

-define(DEFAULT_MERGE_OPTS,
	[{backup_suffix, ?DEFAULT_BACKUP_SUFFIX},
	 backups, 
	 {dir, ?DEFAULT_DIR},
	 {printer, fun default_printer/2},
	 {stub_dir, ?DEFAULT_STUB_DIR},
	 stubs,
	 {suffix, ?DEFAULT_SUFFIX},
	 {verbose, false}]).

-spec merge(atom(), [file:filename()], [option()]) -> [file:filename(),...].

merge(Name, Files, Opts) ->
    Opts1 = Opts ++ ?DEFAULT_MERGE_OPTS,
    {Sources, Enc} = merge_files1(Files, Opts1),
    {Tree, Stubs} = merge_sources(Name, Sources, Opts1),
    Dir = proplists:get_value(dir, Opts1, ""),
    Filename = proplists:get_value(outfile, Opts1, Name),
    Encoding = [{encoding, Enc} || Enc =/= none],
    File = write_module(Tree, Filename, Dir, Encoding ++ Opts1),
    [File | maybe_create_stubs(Stubs, Opts1)].


%% =====================================================================
%% @spec merge_files(Name::atom(), Files::[filename()],
%%                   Options::[term()]) ->
%%           {syntaxTree(), [stubDescriptor()]}
%% @equiv merge_files(Name, [], Files, Options)

-spec merge_files(atom(), [file:filename()], [option()]) ->
        {erl_syntax:syntaxTree(), [stubDescriptor()]}.

merge_files(Name, Files, Options) ->
    merge_files(Name, [], Files, Options).


%% =====================================================================
%% @spec merge_files(Name::atom(), Sources::[Forms],
%%                   Files::[filename()], Options::[term()]) ->
%%           {syntaxTree(), [stubDescriptor()]}
%%
%%     Forms = syntaxTree() | [syntaxTree()]
%%
%% @doc Merges source code files and syntax trees to a single syntax
%% tree. This is a file-reading front end to
%% `merge_sources/3'. `Name' specifies the name of
%% the resulting module - not the name of the output file.
%% `Sources' is a list of syntax trees and/or lists of
%% "source code form" syntax trees, each entry representing a module
%% definition. `Files' is a list of file names and/or module
%% names of source modules to be read and included. All the input
%% modules must be distinctly named.
%%
%% If a name in `Files' is not the name of an existing
%% file, Igor assumes it represents a module name, and tries to locate
%% and read the corresponding source file. The parsed files are appended
%% to `Sources' and passed on to
%% `merge_sources/3', i.e., entries in `Sources'
%% are listed before entries read from files.
%%
%% If no exports are listed by an `export' option (see
%% `merge_sources/3' for details), then if `Name'
%% is also the name of one of the input modules, that module will be
%% exported; otherwise, the first listed module will be exported. Cf.
%% the examples under `merge/3'.
%%
%% The result is a pair `{Tree, Stubs}', where
%% `Tree' represents the source code that is the result of
%% merging all the code in `Sources' and `Files',
%% and `Stubs' is a list of stub module descriptors (see
%% `merge_sources/3' for details).
%%
%% Options:
%% <dl>
%%   <dt>`{comments, boolean()}'</dt>
%%
%%     <dd>If the value is `true', source code comments in
%%     the original files will be preserved in the output. The default
%%     value is `true'.</dd>
%%
%%   <dt>`{find_src_rules, [{string(), string()}]}'</dt>
%%
%%     <dd>Specifies a list of rules for associating object files with
%%     source files, to be passed to the function
%%     `filename:find_src/2'. This can be used to change the
%%     way Igor looks for source files. If this option is not specified,
%%     the default system rules are used. The first occurrence of this
%%     option completely overrides any later in the option list.</dd>
%%
%%   <dt>`{includes, [filename()]}'</dt>
%%
%%     <dd>Specifies a list of directory names for the Erlang
%%     preprocessor, if used, to search for include files (cf. the
%%     `preprocess' option). The default value is the empty
%%     list. The directory of the source file and the current directory
%%     are automatically appended to the list.</dd>
%%
%%   <dt>`{macros, [{atom(), term()}]}'</dt>
%%
%%     <dd>Specifies a list of "pre-defined" macro definitions for the
%%     Erlang preprocessor, if used (cf. the `preprocess'
%%     option). The default value is the empty list.</dd>
%%
%%   <dt>`{preprocess, boolean()}'</dt>
%%
%%     <dd>If the value is `false', Igor will read source
%%     files without passing them through the Erlang preprocessor
%%     (`epp'), in order to avoid expansion of preprocessor
%%     directives such as `-include(...).',
%%     `-define(...).' and `-ifdef(...)', and
%%     macro calls such as `?LINE' and `?MY_MACRO(x,
%%     y)'. The default value is `false', i.e.,
%%     preprocessing is not done. (See the module
%%     `epp_dodger' for details.)
%%
%%     Notes: If a file contains too exotic definitions or uses of
%%     macros, it will not be possible to read it without preprocessing.
%%     Furthermore, Igor does not currently try to sort out multiple
%%     inclusions of the same file, or redefinitions of the same macro
%%     name. Therefore, when preprocessing is turned off, it may become
%%     necessary to edit the resulting source code, removing such
%%     re-inclusions and redefinitions.</dd>
%% </dl>
%%
%% See `merge_sources/3' for further options.
%%
%% @see merge/3
%% @see merge_files/3
%% @see merge_sources/3
%% @see //stdlib/filename:find_src/2
%% @see epp_dodger

-spec merge_files(atom(), erl_syntax:forms(), [file:filename()], [option()]) ->
        {erl_syntax:syntaxTree(), [stubDescriptor()]}.

merge_files(Name, Trees, Files, Opts) ->
    {Sources, _Encoding} = merge_files1(Files, Opts),
    merge_sources(Name, Trees ++ Sources, Opts).

merge_files1([], _) ->
    report_error("no files to merge."),
    exit(badarg);
merge_files1(Files, Opts) ->
    Opts1 = Opts ++ [{includes, ?DEFAULT_INCLUDES},
		     {macros, ?DEFAULT_MACROS},
		     {preprocess, false},
		     comments],
    SourceEncodings = [read_module(F, Opts1) || F <- Files],
    {Sources, [Encoding | _]} = lists:unzip(SourceEncodings),
    {Sources, Encoding}.


%% =====================================================================
%% @spec merge_sources(Name::atom(), Sources::[Forms],
%%                     Options::[term()]) ->
%%           {syntaxTree(), [stubDescriptor()]}
%%
%%     Forms = syntaxTree() | [syntaxTree()]
%%
%% @type stubDescriptor() = {ModuleName, Functions, [Attribute]}
%%	    ModuleName = atom()
%%	    Functions = [{FunctionName, {ModuleName, FunctionName}}]
%%	    FunctionName = {atom(), integer()}
%%	    Attribute = {atom(), term()}.
%%
%%      A stub module descriptor contains the module name, a list of
%%      exported functions, and a list of module attributes. Each
%%      function is described by its name (which includes its arity),
%%      and the corresponding module and function that it calls. (The
%%      arities should always match.) The attributes are simply
%%      described by key-value pairs.
%%
%% @doc Merges syntax trees to a single syntax tree. This is the main
%% code merging "engine". `Name' specifies the name of the
%% resulting module. `Sources' is a list of syntax trees of
%% type `form_list' and/or lists of "source code form" syntax
%% trees, each entry representing a module definition. All the input
%% modules must be distinctly named.
%%
%% Unless otherwise specified by the options, all modules are assumed
%% to be at least "static", and all except the target module are assumed
%% to be "safe". See the `static' and `safe'
%% options for details.
%%
%% If `Name' is also the name of one of the input modules,
%% the code from that module will occur at the top of the resulting
%% code, and no extra "header" comments will be added. In other words,
%% the look of that module will be preserved.
%%
%% The result is a pair `{Tree, Stubs}', where
%% `Tree' represents the source code that is the result of
%% merging all the code in `Sources', and `Stubs'
%% is a list of stub module descriptors (see below).
%%
%% `Stubs' contains one entry for each exported input
%% module (cf. the `export' option), each entry describing a
%% stub module that redirects calls of functions in the original module
%% to the corresponding (possibly renamed) functions in the new module.
%% The stub descriptors can be used to automatically generate stub
%% modules; see `create_stubs/2'.
%%
%% Options:
%% <dl>
%%   <dt>`{export, [atom()]}'</dt>
%%
%%     <dd>Specifies a list of names of input modules whose interfaces
%%     should be exported by the output module. A stub descriptor is
%%     generated for each specified module, unless its name is
%%     `Name'. If no modules are specified, then if
%%     `Name' is also the name of an input module, that
%%     module will be exported; otherwise the first listed module in
%%     `Sources' will be exported. The default value is the
%%     empty list.</dd>
%%
%%   <dt>`{export_all, boolean()}'</dt>
%%
%%     <dd>If the value is `true', this is equivalent to
%%     listing all of the input modules in the `export'
%%     option. The default value is `false'.</dd>
%%
%% <dt>`{file_attributes, Preserve}'</dt>
%%     <dd><ul>
%%       <li>`Preserve = yes | comment | no'</li>
%%     </ul>
%%     If the value is `yes', all file attributes
%%     `-file(...)' in the input sources will be preserved in
%%     the resulting code. If the value is `comment', they
%%     will be turned into comments, but remain in their original
%%     positions in the code relative to the other source code forms. If
%%     the value is `no', all file attributes will be removed
%%     from the code, unless they have attached comments, in which case
%%     they will be handled as in the `comment' case. The
%%     default value is `no'.</dd>
%%
%% <dt>`{no_banner, boolean()}'</dt>
%%
%%     <dd>If the value is `true', no banner comment will be
%%     added at the top of the resulting module, even if the target
%%     module does not have the same name as any of the input modules.
%%     Instead, Igor will try to preserve the look of the module whose
%%     code is at the top of the output. The default value is
%%     `false'.</dd>
%%
%% <dt>`{no_headers, boolean()}'</dt>
%%
%%     <dd>If the value is `true', no header comments will be
%%     added to the resulting module at the beginning of each section of
%%     code that originates from a particular input module. The default
%%     value is `false', which means that section headers are
%%     normally added whenever more than two or more modules are
%%     merged.</dd>
%%
%% <dt>`{no_imports, boolean()}'</dt>
%%
%%     <dd>If the value is `true', all
%%     `-import(...)' declarations in the original code will
%%     be expanded in the result; otherwise, as much as possible of the
%%     original import declarations will be preserved. The default value
%%     is `false'.</dd>
%%
%% <dt>`{notes, Notes}'</dt>
%%     <dd><ul>
%%       <li>`Notes = always | yes | no'</li>
%%     </ul>
%%     If the value is `yes', comments will be inserted where
%%     important changes have been made in the code. If the value is
%%     `always', <em>all</em> changes to the code will be
%%     commented. If the value is `no', changes will be made
%%     without comments. The default value is `yes'.</dd>
%%
%% <dt>`{redirect, [{atom(), atom()}]}'</dt>
%%
%%     <dd>Specifies a list of pairs of module names, representing a
%%     mapping from old names to new. <em>The set of old names may not
%%     include any of the names of the input modules.</em> All calls to
%%     the listed old modules will be rewritten to refer to the
%%     corresponding new modules. <em>The redirected calls will not be
%%     further processed, even if the new destination is in one of the
%%     input modules.</em> This option mainly exists to support module
%%     renaming; cf. `rename/3'. The default value is the
%%     empty list.</dd>
%%
%% <dt>`{safe, [atom()]}'</dt>
%%
%%     <dd>Specifies a list of names of input modules such that calls to
%%     these "safe" modules may be turned into direct local calls, that
%%     do not test for code replacement. Typically, this can be done for
%%     e.g. standard library modules. If a module is "safe", it is per
%%     definition also "static" (cf. below). The list may be empty. By
%%     default, all involved modules <em>except the target module</em>
%%     are considered "safe".</dd>
%%
%% <dt>`{static, [atom()]}'</dt>
%%
%%     <dd>Specifies a list of names of input modules which will be
%%     assumed never to be replaced (reloaded) unless the target module
%%     is also first replaced. The list may be empty. The target module
%%     itself (which may also be one of the input modules) is always
%%     regarded as "static", regardless of the value of this option. By
%%     default, all involved modules are assumed to be static.</dd>
%%
%% <dt>`{tidy, boolean()}'</dt>
%%
%%     <dd>If the value is `true', the resulting code will be
%%     processed using the `erl_tidy' module, which removes
%%     unused functions and does general code cleanup. (See
%%     `erl_tidy:module/2' for additional options.) The
%%     default value is `true'.</dd>
%%
%% <dt>`{verbose, boolean()}'</dt>
%%
%%     <dd>If the value is `true', progress messages will be
%%     output while the program is running; the default value is
%%     `false'.</dd>
%% </dl>
%%
%% Note: The distinction between "static" and "safe" modules is
%% necessary in order not to break the semantics of dynamic code
%% replacement. A "static" source module will not be replaced unless the
%% target module also is. Now imagine a state machine implemented by
%% placing the code for each state in a separate module, and suppose
%% that we want to merge this into a single target module, marking all
%% source modules as static. At each point in the original code where a
%% call is made from one of the modules to another (i.e., the state
%% transitions), code replacement is expected to be detected. Then, if
%% we in the merged code do not check at these points if the
%% <em>target</em> module (the result of the merge) has been replaced,
%% we can not be sure in general that we will be able to do code
%% replacement of the merged state machine - it could run forever
%% without detecting the code change. Therefore, all such calls must
%% remain remote-calls (detecting code changes), but may call the target
%% module directly.
%%
%% If we are sure that this kind of situation cannot ensue, we may
%% specify the involved modules as "safe", and all calls between them
%% will become local. Note that if the target module itself is specified
%% as safe, "remote" calls to itself will be turned into local calls.
%% This would destroy the code replacement properties of e.g. a typical
%% server loop.
%%
%% @see create_stubs/2
%% @see rename/3
%% @see erl_tidy:module/2

%% Currently, there is no run-time support in Erlang for detecting
%% whether some module has been changed since the current module was
%% loaded. Therefore, if a source module is specified as non-static, not
%% much will be gained from merging: a call to a non-static module will
%% remain a remote call using the old module name, even when it is
%% performed from within the merged code. If that module is specified as
%% exported, the old name could then refer to an auto-generated stub,
%% redirecting the call back to the corresponding function in the target
%% module. This could possibly be useful in some cases, but efficiency
%% is not improved by such a transformation. If support for efficient
%% testing for module updates is added to Erlang in future versions,
%% code merging will be able to use local calls even for non-static
%% source modules, opening the way for compiler optimisations over the
%% module boundaries.

%% Data structure for merging environment.

-record(merge, {target     :: atom(),
		sources    :: ordsets:ordset(atom()),
		export     :: ordsets:ordset(atom()),
		static     :: ordsets:ordset(atom()),
		safe       :: ordsets:ordset(atom()),
		preserved  :: boolean(),
		no_headers :: boolean(),
		notes      :: notes(),
		redirect   :: dict:dict(atom(), atom()),
		no_imports :: ordsets:ordset(atom()),
		options	   :: [option()]
	       }).

-spec merge_sources(atom(), [erl_syntax:forms()], [option()]) ->
        {erl_syntax:syntaxTree(), [stubDescriptor()]}.

merge_sources(Name, Sources, Opts) ->
    %% Prepare the options and the inputs.
    Opts1 = Opts ++ [{export_all, false},
		     {file_attributes, no},
		     {no_imports, false},
		     {notes, yes},
		     tidy,
		     {verbose, false}],
    Trees = case Sources of
		[] ->
		    report_error("no sources to merge."),
		    exit(badarg);
		_ ->
		    [if is_list(M) -> erl_syntax:form_list(M);
			true -> M
		     end
		     || M <- Sources]
	    end,
    %% There must be at least one module to work with.
    Modules = [get_module_info(T) || T <- Trees],
    merge_sources_1(Name, Modules, Trees, Opts1).

%% Data structure for keeping state during transformation.

-record(state, {export :: sets:set({atom(), arity()})}).

state__add_export(Name, Arity, S) ->
    S#state{export = sets:add_element({Name, Arity},
				      S#state.export)}.

merge_sources_1(Name, Modules, Trees, Opts) ->
    %% Get the (nonempty) list of source module names, in the given
    %% order. Multiple occurrences of the same source module name are
    %% not accepted.
    Ns = [M#module.name || M <- Modules],
    case duplicates(Ns) of
	[] ->
	    ok;
	Ns1 ->
	    report_error("same module names repeated in input: ~p.",
			 [Ns1]),
	    exit(error)
    end,
    Sources = ordsets:from_list(Ns),
    All = ordsets:add_element(Name, Sources),

    %% Initialise the merging environment from the given options.
    %%
    %% If the `export' option is the empty list, then if the target
    %% module is the same as one of the sources, that module will be
    %% exported; otherwise the first listed source module is exported.
    %% This simplifies use in most cases, and guarantees that the
    %% generated module has a well-defined interface. If `export_all' is
    %% `true', we expand it here by including the set of source module
    %% names.
    Es = case proplists:append_values(export, Opts) of
	     [] ->
		 case ordsets:is_element(Name, Sources) of
		     true ->
			 [Name];
		     false ->
			 [hd(Ns)]
		 end;
	     Es1 when is_list(Es1) ->
		 ordsets:from_list(Es1)
	 end,
    Export = case proplists:get_bool(export_all, Opts) of
		 false ->
		     Es;
		 true ->
		     ordsets:union(Sources, Es)
	     end,
    check_module_names(Export, Sources, "declared as exported"),
    verbose("modules exported from `~w': ~p.", [Name, Export], Opts),

    %% The target module is always "static". (Particularly useful when
    %% the target is the same as one of the source modules). It is
    %% however not "safe" by default. If no modules are explicitly
    %% specified as static, it is assumed that *all* are static.
    Static0 = ordsets:from_list(proplists:append_values(static, Opts)),
    Static = case proplists:is_defined(static, Opts) of
		 false ->
		     All;
		 true ->
		     ordsets:add_element(Name, Static0)
	     end,
    check_module_names(Static, All, "declared 'static'"),
    verbose("static modules: ~p.", [Static], Opts),

    %% If no modules are explicitly specified as "safe", it is assumed
    %% that *all* source modules are "safe" except the target module and
    %% those explicitly specified as "static".
    Safe = case proplists:is_defined(safe, Opts) of
	       false ->
		   ordsets:subtract(Sources,
				    ordsets:add_element(Name, Static0));
	       true ->
		   ordsets:from_list(
		     proplists:append_values(safe, Opts))
	   end,
    check_module_names(Safe, All, "declared 'safe'"),
    verbose("safe modules: ~p.", [Safe], Opts),

    Preserved = (ordsets:is_element(Name, Sources)
		 andalso ordsets:is_element(Name, Export))
	orelse proplists:get_bool(no_banner, Opts),
    NoHeaders = proplists:get_bool(no_headers, Opts),
    Notes = proplists:get_value(notes, Opts, always),
    Rs = proplists:append_values(redirect, Opts),
    Redirect = case is_atom_map(Rs) of
		   true ->
		       Ms = ordsets:from_list([M || {M, _} <- Rs]),
		       case ordsets:intersection(Sources, Ms) of
			   [] ->
			       ok;
			   Ms1 ->
			       report_error("cannot redirect calls to "
					    "modules in input set: ~p.",
					    [Ms1]),
			       exit(error)
		       end,
		       dict:from_list(Rs);
		   false ->
		       report_error("bad value for `redirect' option: "
				    "~P.",
				    [Rs, 10]),
		       exit(error)
	       end,
    NoImports = case proplists:get_bool(no_imports, Opts) of
		    true ->
			ordsets:from_list(Sources ++
					  dict:fetch_keys(Redirect));
		    false ->
			ordsets:from_list(dict:fetch_keys(Redirect))
		end,
    Env = #merge{target = Name,
		 sources = Sources,
		 export = Export,
		 safe = Safe,
		 static = Static,
		 preserved = Preserved,
		 no_headers = NoHeaders,
		 notes = Notes,
		 redirect = Redirect,
		 no_imports = NoImports,
		 options = Opts},
    merge_sources_2(Env, Modules, Trees, Opts).

is_atom_map([{A1, A2} | As]) when is_atom(A1), is_atom(A2) ->
    is_atom_map(As);
is_atom_map([]) ->
    true;
is_atom_map(_) ->
    false.

check_module_names(Names, Sources, Txt) ->
    case Names -- Sources of
	[] ->
	    ok;
	Xs ->
	    report_error("unknown modules ~s: ~p.", [Txt, Xs]),
	    exit(error)
    end.

%% This function performs all the stages of the actual merge:

merge_sources_2(Env, Modules, Trees, Opts) ->
    %% Compute the merged name space and the list of renamings.
    {Names, Renaming} = merge_namespaces(Modules, Env),
    
    %% Merge the source module descriptions, computing a structure
    %% describing the resulting module, and a table of aliases which
    %% must be expanded.
    {Module, Expansions} = merge_info(Modules, Names, Renaming,
				      Env),
    
    %% Merge the actual source code, also returning the "original
    %% header" (for the first code section in the output).
    St = #state{export = sets:new()},
    {Tree, Header, St1} = merge_code(Trees, Modules, Expansions,
				     Renaming, Env, St),

    %% Filter out unwanted program forms and add a preamble to the code,
    %% making a complete module.
    Tree1 = erl_syntax:form_list([make_preamble(Module, Header,
						Env, St1),
				  filter_forms(Tree, Env)]),
    
    %% Tidy the final syntax tree (removing unused functions) and return
    %% it together with the list of stub descriptors.
    {tidy(Tree1, Opts), make_stubs(Modules, Renaming, Env)}.

make_preamble(Module, Header, Env, St) ->
    Name = Module#module.name,
    Vars = Module#module.vars,
    Extras = ordsets:from_list(sets:to_list(St#state.export)),
    Exports = make_exports(Module#module.exports, Extras),
    Imports = make_imports(Module#module.aliases),
    Attributes = make_attributes(Module#module.attributes),
    erl_syntax:form_list(module_header(Header, Name, Vars, Env)
			 ++ Exports
			 ++ Imports
			 ++ Attributes).

%% If the target preserves one of the source modules, we do not generate
%% a new header, but use the original.

module_header(Forms, Name, Vars, Env) ->
    case Env#merge.preserved of
	true ->
	    update_header(Forms, Name, Vars);
	false ->
	    [comment([?COMMENT_BAR,
		      "This module was formed by merging "
		      "the following modules:",
		      ""]
		     ++ [lists:flatten(io_lib:fwrite("\t\t`~w'",
						     [M]))
			 || M <- Env#merge.sources]
		     ++ ["",
			 timestamp(),
			 ""]),
	     erl_syntax:attribute(erl_syntax:atom('module'),
				  [erl_syntax:atom(Name)])]
    end.

update_header(Fs, Name, Vars) ->
    [M | Fs1] = lists:reverse(Fs),
    Ps = if Vars =:= none -> [];
	    true -> [erl_syntax:list([erl_syntax:variable(V)
				      || V <- Vars])]
	 end,
    M1 = rewrite(M, erl_syntax:attribute(erl_syntax:atom('module'),
					 [erl_syntax:atom(Name) | Ps])),
    lists:reverse([M1 | Fs1]).

%% Some functions may have been noted as necessary to export (because of
%% how they are called) even though the user did not specify that the
%% modules in which these functions originated should be part of the
%% interface of the resulting module.

make_exports(Exports, Extras) ->
    case ordsets:subtract(Extras, Exports) of
	[] ->
	    [make_export(Exports)];
	Es ->
	    [make_export(Exports),
	     comment(["** The following exports "
		      "are not official: **"]),
	     make_export(Es)]
    end.

make_export(Names) ->
    Es = [erl_syntax:arity_qualifier(erl_syntax:atom(F),
				     erl_syntax:integer(A))
	  || {F, A} <- Names],
    if Es =:= [] ->
	    comment(["** Nothing is officially exported "
		    "from this module! **"]);
       true ->
	    erl_syntax:attribute(erl_syntax:atom('export'),
				 [erl_syntax:list(Es)])
    end.

%% Any aliases that cannot be expressed using `import' (i.e. those not
%% on the form `{F, {M, F}}') are ignored.

make_imports(As) ->
    %% First remove any auto-imports and "non-proper" imports from
    %% the list.
    As1 = [A || {F, {_M, F}} = A <- As, not is_auto_import(F)],
    [make_import(M, Fs) || {M, Fs} <- group_imports(As1)].

make_import(Module, Names) ->
    Is = [erl_syntax:arity_qualifier(erl_syntax:atom(F),
				     erl_syntax:integer(A))
	  || {F, A} <- Names],
    erl_syntax:attribute(erl_syntax:atom('import'),
			 [erl_syntax:atom(Module),
			  erl_syntax:list(Is)]).

%% Group aliases by module.

group_imports(Imports) ->
    dict:to_list(
      lists:foldl(
	fun ({F, {M, F}}, D) ->
		case dict:find(M, D) of
		    {ok, V} ->
			V1 = ordsets:add_element(F, V),
			dict:store(M, V1, D);
		    error ->
			dict:store(M, [F], D)
		end
	end,
	dict:new(), Imports)).


%% ---------------------------------------------------------------------
%% Making stub descriptors
%%
%% These are generated for all exported modules that are not the target
%% module.

make_stubs(Modules, Renaming, Env) ->
    make_stubs_1(Modules, Renaming, Env).

make_stubs_1([M | Ms], Renaming, Env) ->
    Name = M#module.name,
    if Name =/= Env#merge.target ->
	    case ordsets:is_element(Name, Env#merge.export) of
		true ->
		    [make_stub(M, Renaming(Name), Env)
		     | make_stubs_1(Ms, Renaming, Env)];
		false ->
		    make_stubs_1(Ms, Renaming, Env)
	    end;
       true ->
	    make_stubs_1(Ms, Renaming, Env)
    end;
make_stubs_1([], _, _) ->
    [].

make_stub(M, Map, Env) ->
    Target = Env#merge.target,
    Es = [{F, {Target, Map(F)}} || F <- M#module.exports],
    {M#module.name, Es, M#module.attributes}.


%% ---------------------------------------------------------------------
%% Removing and/or out-commenting program forms. The returned form
%% sequence tree is not necessarily flat.

-type atts()      :: 'delete' | 'kill'.
-type file_atts() :: 'delete' | 'keep' | 'kill'.

-record(filter, {records         :: sets:set(atom()),
		 file_attributes :: file_atts(),
		 attributes      :: atts()}).

filter_forms(Tree, Env) ->
    Forms = erl_syntax:form_list_elements(
	      erl_syntax:flatten_form_list(Tree)),
    erl_syntax:form_list(filter_forms_1(Forms, Env)).

filter_forms_1(Forms, Env) ->
    {Fs, _} = filter_forms_2(Forms, Env),
    lists:reverse(Fs).

filter_forms_2(Forms, Env) ->
    FileAttrsOpt = proplists:get_value(file_attributes,
				       Env#merge.options, comment),
    %% Sanity check and translation of option value:
    FileAttrs = case FileAttrsOpt of
		    yes -> keep;
		    no -> delete;
		    comment -> kill;
		    _ ->
			report_error("invalid value for option "
				     "`file_attributes': ~w.",
				     [FileAttrsOpt]),
			exit(error)
		end,
    Attrs = if length(Env#merge.sources) =:= 1 ->
		    delete;    %% keeping the originals looks weird
	       true ->
		    kill
	    end,
    S = #filter{records = sets:new(),
		file_attributes = FileAttrs,
		attributes = Attrs},
    lists:foldl(
      fun (F, {Fs, S0}) ->
	      case filter_form(F, S0) of
		  {keep, S1} ->
		      {[F | Fs], S1};    % keep
		  {kill, S1} ->
		      {[kill_form(F) | Fs], S1};    % kill
		  {delete, S1} ->
		      %% Remove, or kill if it has comments (only
		      %% top-level comments are examined).
		      case erl_syntax:has_comments(F) of
			  false ->
			      {Fs, S1};
			  true ->
			      {[kill_form(F) | Fs], S1}
		      end
	      end
      end,
      {[], S}, Forms).

filter_form(F, S) ->
    case erl_syntax_lib:analyze_form(F) of
	{attribute, {'file', _}} ->
	    {S#filter.file_attributes, S};
	{attribute, {'module', _}} ->
	    {delete, S};
	{attribute, {'export', _}} ->
	    {delete, S};
	{attribute, {'import', _}} ->
	    {delete, S};
	{attribute, {'record', {R, _}}} ->
	    Records = S#filter.records,
	    case sets:is_element(R, Records) of
		true ->
		    {kill, S};    % already defined above
		false ->
		    S1 = S#filter{records =
				  sets:add_element(R, Records)},
		    {keep, S1}
	    end;
	{attribute, preprocessor} ->
	    {keep, S};    %% keep all preprocessor attributes
	{attribute, _} ->
	    {S#filter.attributes, S};    %% handle all other attributes
	{error_marker, _} ->
	    {delete, S};
	{warning_marker, _} ->
	    {delete, S};
	eof_marker ->
	    {delete, S};    % these must be deleted!
	_ ->
	    {keep, S}    % keep all other Erlang forms
    end.

%% This out-comments (kills) a program form. Any top-level pre-comments
%% are moved out, to avoid "nested" comments.

kill_form(F) ->
    F1 = erl_syntax:set_precomments(F, []),
    F2 = erl_syntax_lib:to_comment(F1, ?KILL_PREFIX),
    erl_syntax:set_precomments(F2, erl_syntax:get_precomments(F)).


%% ---------------------------------------------------------------------
%% Merging the name spaces of a set of modules. Returns the final set
%% (see module `sets') of names and a total renaming function (atom())
%% -> ({atom(), integer()}) -> {atom(), integer()}.
%%
%% Names are added in two passes, in order to avoid renaming the
%% interface functions whenever possible: all exported functions are
%% added to the name space before any nonexported are added, and
%% "exported" modules are taken before any other. Thus, the order is:
%%
%%   - exported functions of exported modules
%%   - exported functions of nonexported modules
%%   - internal functions of exported modules
%%   - internal functions of nonexported modules
%%
%% In fact, only the first group is important, but there might be some
%% point in establishing the above order, for better readability of the
%% final code.

merge_namespaces(Modules, Env) ->
    Export = Env#merge.export,
    Split = fun (M) ->
		    ordsets:is_element(M#module.name, Export)
	    end,
    {M1, M2} = split_list(Split, Modules),
    R = dict:new(),
    Acc = {sets:new(), R},
    {M3, Acc1} = merge_namespaces_1(M1, Acc),

    %% Detect and warn about renamed interface functions
    {_, Maps0} = Acc1,
    case [{M, dict:to_list(Map)}
	  || {M, Map} <- dict:to_list(Maps0), dict:size(Map) =/= 0] of
	[] ->
	    ok;
	Fs ->
	    report_warning("interface functions renamed:\n\t~p.", [Fs])
    end,
    {M4, Acc2} = merge_namespaces_1(M2, Acc1),
    Ms = M3 ++ M4,
    Acc3 = merge_namespaces_2(Ms, Acc2),
    {{Names, Maps}, _} = merge_namespaces_3(Ms, Acc3),
    {Names, make_renaming_function(Maps)}.

%% Adding exported names. (Note that the list gets a new temporary
%% format also containing the exports.) This first step initialises the
%% Maps "dict-of-dicts" structure.

merge_namespaces_1(Modules, Acc) ->
    lists:mapfoldl(
      fun (Module, {Names, Maps}) ->
	      Exports = sets:from_list(Module#module.exports),
	      M = Module#module.name,
	      {Names1, Map} = add_function_renamings(M, Exports, Names,
						     dict:new()),
	      Maps1 = dict:store(M, Map, Maps),
	      {{Module, Exports}, {Names1, Maps1}}
      end,
      Acc, Modules).

%% Adding nonexported names.

merge_namespaces_2(Modules, Acc) ->
    lists:foldl(
      fun ({Module, Exports}, {Names, Maps}) ->
	      Other = sets:subtract(
			sets:from_list(Module#module.functions),
			Exports),
	      M = Module#module.name,
	      Map = dict:fetch(M, Maps),
	      {Names1, Map1} = add_function_renamings(M, Other, Names,
						      Map),
	      Maps1 = dict:store(M, Map1, Maps),
	      {Names1, Maps1}
      end,
      Acc, Modules).

%% Adding record names. We need to keep a global
%% "record-definition-to-new-record-name" mapping RMap while doing this.

merge_namespaces_3(Modules, Acc) ->
    lists:foldl(
      fun ({Module, _Exports}, {{Names, Maps}, RMap}) ->
	      Records = Module#module.records,
	      M = Module#module.name,
	      Map = dict:fetch(M, Maps),
	      {Names1, Map1, RMap1} = add_record_renamings(M, Records,
							   Names, Map,
							   RMap),
	      Maps1 = dict:store(M, Map1, Maps),
	      {{Names1, Maps1}, RMap1}
      end,
      {Acc, dict:new()}, Modules).

%% This takes the set of added function names together with the existing
%% name set, creates new function names where necessary, and returns the
%% final name set together with the list of renamings.

add_function_renamings(Module, New, Names, Map) ->
    Clashes = sets:to_list(sets:intersection(New, Names)),
    lists:foldl(
      fun (F = {_, A}, {Names, Map}) when is_integer(A) ->
	      F1 = new_function_name(Module, F, Names),
	      {sets:add_element(F1, Names), dict:store(F, F1, Map)}
      end,
      {sets:union(New, Names), Map}, Clashes).

%% This is similar to the above, but for record names. Note that we add
%% both the record name and the whole definition to the namespace.

add_record_renamings(Module, Records, Names, Map, RMap) ->
    lists:foldl(
      fun (N = {R, Fs}, {Names, Map, RMap}) ->
	      case sets:is_element(?record_name(R), Names) of
		  true ->
		      %% The name is already in use.
		      case sets:is_element(?record_name(N), Names) of
			  true ->
			      %% We have seen this definition before;
			      %% make sure we use the same name.
			      {R1, _} = remap_record_name(N, RMap),
			      Map1 = dict:store(?record_name(R),
						?record_name(R1), Map),
			      {Names, Map1, RMap};
			  false ->
			      %% Redefinition of existing name. Create
			      %% new name and set up renamings.
			      N1 = {R1, _} = new_record_name(Module, R,
							     Fs, Names),
			      Map1 = dict:store(?record_name(R),
						?record_name(R1), Map),
			      RMap1 = dict:store(N, N1, RMap),
			      Names1 = sets:add_element(?record_name(N1),
							Names),
			      {Names1, Map1, RMap1}
		      end;
		  false ->
		      %% A previously unused record name.
		      Names1 = sets:add_element(?record_name(R), Names),
		      Names2 = sets:add_element(?record_name(N), Names1),
		      {Names2, Map, RMap}
	      end
      end,
      {Names, Map, RMap}, Records).

remap_record_name(N, Map) ->
    case dict:find(N, Map) of
	{ok, N1} -> N1;
	error -> N
    end.

%% This hides the implementation of the record namespace. Since Map
%% yields identity for non-remapped names, the remapped names must be
%% stored in wrapped form.

map_record_name(R, Map) ->
    ?record_name(R1) = Map(?record_name(R)),
    R1.

%% When we rename a function, we want the new name to be as close as
%% possible to the old, and as informative as possible. Therefore, we
%% first prefix it with the name of the originating module, followed by
%% two underscore characters, and then if there still is a name clash,
%% we suffix the name by "_N", where N is the smallest possible positive
%% integer that does not cause a clash.

new_function_name(M, {F, A}, Names) ->
    Base = atom_to_list(M) ++ "__" ++ atom_to_list(F),
    Name = {list_to_atom(Base), A},
    case sets:is_element(Name, Names) of
	false ->
	    Name;
	true ->
	    new_function_name(1, A, Base, Names)
    end.

new_function_name(N, Arity, Base, Names) ->
    Name = {list_to_atom(Base ++ "_" ++ integer_to_list(N)),
	    Arity},
    case sets:is_element(Name, Names) of
	false ->
	    Name;
	true ->
	    %% Increment counter and try again.
	    new_function_name(N + 1, Arity, Base, Names)
    end.

%% This is pretty much the same as new_function_name, for now.

new_record_name(M, R, Fs, Names) ->
    Base = atom_to_list(M) ++ "__" ++ atom_to_list(R),
    Name = {list_to_atom(Base), Fs},
    case sets:is_element(?record_name(Name), Names) of
	false ->
	    Name;
	true ->
	    new_record_name_1(1, Base, Fs, Names)
    end.

new_record_name_1(N, Base, Fs, Names) ->
    Name = {list_to_atom(Base ++ "_" ++ integer_to_list(N)), Fs},
    case sets:is_element(?record_name(Name), Names) of
	false ->
	    Name;
	true ->
	    %% Increment counter and try again.
	    new_record_name_1(N + 1, Base, Fs, Names)
    end.

%% This returns a *total* function from the set of module names to the
%% set of *total* operators on function names, yielding identity for all
%% function names that are not specified in the given partial map
%% (ModuleName -> (Name -> Name)).

make_renaming_function(Maps) ->
    fun (Module) ->
	    case dict:find(Module, Maps) of
		{ok, Map} ->
		    fun (Name) ->
			    case dict:find(Name, Map) of
				{ok, Name1} ->
				    Name1;    % renamed
				error ->
				    Name    % identity
			    end
		    end;
		error ->
		    %% Other module - yield identity map.
		    fun (Name) -> Name end
	    end
    end.


%% ---------------------------------------------------------------------
%% Merging module info records into a target module record, and finding
%% necessary alias expansions. Returns `{Module, Expansions}' where
%% `Expansions' has type `dict(ModuleName, dict(Alias, FullName))'

merge_info(Modules, Names, Renaming, Env) ->
    Forbid = sets:from_list(Env#merge.no_imports),
    Expansions = alias_expansions(Modules, Names, Forbid),
    Module = merge_info_1(Modules, Renaming, Expansions, Env),
    {Module, Expansions}.

merge_info_1(Modules, Renaming, Expansions, Env) ->
    lists:foldl(
      fun (M, A) ->
	      Name = M#module.name,
	      Map = Renaming(Name),
	      Functions = join_functions(Map,
					 M#module.functions,
					 A#module.functions),
	      Exports = join_exports(Env, Name, Map,
				     M#module.exports,
				     A#module.exports),
	      Aliases = join_aliases(Name, Expansions,
				     M#module.aliases,
				     A#module.aliases),
	      Attributes = join_attributes(Env, Name,
					   M#module.attributes,
					   A#module.attributes),
	      Records = join_records(Map,
				     M#module.records,
				     A#module.records),
	      A#module{functions = Functions,
		       exports = Exports,
		       aliases = Aliases,
		       attributes = Attributes,
		       records = Records}
      end,
      #module{name = Env#merge.target,
	      functions = ordsets:new(),
	      exports = ordsets:new(),
	      aliases = ordsets:new(),
	      attributes = ordsets:new(),
	      records = ordsets:new()},
      Modules).

%% Functions must be renamed before including.

join_functions(Map, Source, Target) ->
    ordsets:union(ordsets:from_list([Map(A) || A <- Source]),
		  Target).

%% Exports also need renaming, and are kept only if their originating
%% modules are exported.

join_exports(Env, Name, Map, Source, Target) ->
    case ordsets:is_element(Name, Env#merge.export) of
	true ->
	    ordsets:union(ordsets:from_list([Map(F)
					     || F <- Source]),
			  Target);
	false ->
	    Target
    end.

%% Aliases never need renaming; instead we always expand uses which
%% could cause name clashes. We must then remove the expanded names from
%% the imports of the target.

join_aliases(Name, Expansions, Source, Target) ->
    As = case dict:find(Name, Expansions) of
	     {ok, As1} ->
		 ordsets:from_list(dict:to_list(As1));
	     error ->
		 []
	 end,
    ordsets:union(ordsets:subtract(Source, As), Target).

%% We only propagate attributes if the number of source modules is 1 or
%% the source module has the same name as the resulting module.

join_attributes(Env, Name, Source, Target) ->
    if Env#merge.target =:= Name ->
	    ordsets:union(Source, Target);
       true ->
	    if length(Env#merge.sources) =:= 1 ->
		    ordsets:union(Source, Target);
	       true ->
		    Target
	    end
    end.

%% The final record info in itself is not used at present, but we
%% compute the join anyway. We apply renaming to records much like we do
%% to functions, but records have a separate namespace.

join_records(Map, Source, Target) ->
    Renamed = [{map_record_name(R, Map), Fs} || {R, Fs} <- Source],
    ordsets:union(ordsets:from_list(Renamed), Target).

%% This finds aliases that are in conflict or are for other reasons
%% necessary to expand while transforming the code later. It is assumed
%% that each module is in itself correct, and thus does not contain
%% conflicting definitions of the same alias.
%%
%% We could of course simply say that *all* aliases, without exception,
%% should be expanded, but such a big change in the style of the code
%% should not be done unless the user explicitly specifies it.
%%
%% The returned `Expansions' is a dictionary (module `dict') mapping
%% each module name in `Modules' to a dictionary which maps those
%% aliases to be expanded for that module to their corresponding full
%% names.
%%
%% Aliases are filtered according to the following rules:
%%
%%	1. If a name is defined (in some source module) as an alias of a
%%	name `M:...', where `M' is any of the source modules(*), then
%%	the definition of that alias should be removed, and all its uses
%%	(in the same module as the definition) be expanded.
%%
%%	2. Then, if a name is defined (in some source module) as an
%%	alias, but the name occurs in the name space of the resulting
%%	module, then the definition should be removed and all uses (in
%%	the same module) expanded.
%%
%%	3. Finally, if a name has two or more distinct alias definitions
%%	in the source modules, then all definitions of that alias should
%%	be removed and all uses (in all modules) expanded. (We remove
%%	all definitions mainly for symmetry.)
%%
%%	(*) It is actually possible for an alias to refer to the module
%%	in which it is itself defined. However, since we also in this
%%	case want to expand all uses, we don't have to do any extra work
%%	to handle it.

%% The filtering is done in two stages.

alias_expansions(Modules, Names, Forbid) ->
    Table = alias_expansions_1(Modules, Forbid, Names),
    alias_expansions_2(Modules, Table).

%% First consider each alias in isolation.

alias_expansions_1(Modules, Forbid, Names) ->
    lists:foldl(
      fun (M, T) ->
	      Map = lists:foldl(
		      fun ({A, F}, T1) ->
			      case keep_alias(A, F, Forbid, Names)
				  of
				  true ->
				      T1;
				  false ->
				      dict:store(A, F, T1)
			      end
		      end,
		      dict:new(), M#module.aliases),
	      dict:store(M#module.name, Map, T)
      end,
      dict:new(), Modules).

keep_alias(A, {M, _}, Forbid, Names) ->
    case sets:is_element(M, Forbid) of
	true ->
	    false;
	false ->
	    not sets:is_element(A, Names)
    end.

%% In this second stage, we resolve any conflicts that may remain
%% because of distinct source modules still containing distinct alias
%% definitions of the same name - in that case we remove *all* of them
%% (mainly for symmetry).

alias_expansions_2(Modules, Table) ->
    %% Get the set of all alias definitions in all modules (collapsing
    %% duplicated but equivalent definitions).
    Aliases = lists:foldl(
		fun (M, A) ->
			ordsets:union(A, M#module.aliases)
		end,
		ordsets:new(), Modules),
    
    %% Get the set of names with multiple (distinct) definitions.
    Names = duplicates([F || {F, _} <- Aliases]),
    
    %% Go through all aliases in all source modules and add necessary
    %% entries to the expansion-table. We expect that there is an entry
    %% in the table here for each module.
    lists:foldl(
      fun (M, T) ->
	      N = M#module.name,
	      lists:foldl(
		fun ({A, F}, T1) ->
			case ordsets:is_element(A, Names) of
			    true ->
				T2 = dict:fetch(N, T1),
				dict:store(N,
					   dict:store(A, F, T2),
					   T1);
			    false ->
				T1
			end
		end,
		T, M#module.aliases)
      end,
      Table, Modules).


%% ---------------------------------------------------------------------
%% Merging the source code.

-type map_fun() :: fun(({atom(), integer()}) -> {atom(), integer()}).

%% Data structure for code transformation environment.

-record(code, {module     :: atom(),
	       target     :: atom(),
	       sources    :: sets:set(atom()),
	       static     :: sets:set(atom()),
	       safe       :: sets:set(atom()),
	       preserved  :: boolean(),
	       no_headers :: boolean(),
	       notes      :: notes(),
	       map        :: map_fun() | 'undefined',
	       renaming   :: fun((atom()) -> map_fun()),
	       expand     :: dict:dict({atom(), integer()},
                                       {atom(), {atom(), integer()}})
                           | 'undefined',
	       redirect	  :: dict:dict(atom(), atom())
	      }).

%% `Trees' must be a list of syntax trees of type `form_list'. The
%% result is a pair `{Result, Header}' where `Result' is a `form_list'
%% tree representing the merged code, and if the `preserved' flag is
%% set, `Header' is the list of forms up to and including the first
%% `-module(...)' declaration, but stripped of any `-file(...)'
%% attributes - otherwise `Header' is an empty list.

merge_code(Trees, Modules, Expansions, Renaming, Env, St) ->
    Env1 = #code{target = Env#merge.target,
		 sources = sets:from_list(Env#merge.sources),
		 static = sets:from_list(Env#merge.static),
		 safe = sets:from_list(Env#merge.safe),
		 preserved = Env#merge.preserved,
		 no_headers = Env#merge.no_headers,
		 notes = Env#merge.notes,
		 redirect = Env#merge.redirect,
		 renaming = Renaming},
    Code = order_code(Modules, Trees, Env1),
    {Code1, Header} = case Env1#code.preserved of
			  true ->
			      take_header(Code);
			  false ->
			      {Code, erl_syntax:form_list([])}
		      end,
    {Forms, St1} = merge_code_1(Code1, Expansions, Env1, St),
    Tree = erl_syntax:form_list(Forms),
    {Tree, Header, St1}.

merge_code_1(Code, Expansions, Env, St) ->
      lists:foldr(
	fun ({Module, T}, {Acc, St0}) ->
		M = Module#module.name,
		Expand = case dict:find(M, Expansions) of
			     {ok, Dict} -> Dict;
			     error -> dict:new()
			 end,
		Env1 = Env#code{module = M,
				map = (Env#code.renaming)(M),
				expand = Expand},
		{T1, St1} = transform(T, Env1, St0),
		{[section_header(M, T1, Env1) | Acc], St1}
	end,
	{[], St}, Code).

%% Pair module info and source code, in the order we want, and flatten
%% the form lists. If the name of the target is the same as one of the
%% source modules, and the result should preserve the original module,
%% the code for that module should be first in the output.

order_code(Modules, Trees, Env) ->
    order_code(Modules, Trees, {}, [], Env).

order_code([M | Ms], [T | Ts], First, Rest, Env) ->
    T1 = erl_syntax:flatten_form_list(T),
    case (M#module.name =:= Env#code.target) and
	Env#code.preserved of
	true ->
	    order_code(Ms, Ts, {M, T1}, Rest, Env);
	false ->
	    order_code(Ms, Ts, First, [{M, T1} | Rest], Env)
    end;
order_code([], [], First, Rest, _Env) ->
    Rest1 = lists:reverse(Rest),
    case First of
	{} ->
	    Rest1;
	M ->
	    [M | Rest1]
    end.

%% Extracting the "original" header (the `-module(...)' declaration is
%% sure to exist).

take_header([{M, T} | Ms]) ->
    Fs = erl_syntax:form_list_elements(T),
    {Header, Fs1} = take_header_1(Fs, []),
    T1 = erl_syntax:form_list(Fs1),
    {[{M, T1} | Ms], Header}.

take_header_1([F | Fs], As) ->
    case erl_syntax_lib:analyze_form(F) of
	{'attribute', {'module', _}} ->
	    {lists:reverse([F | As]), Fs};    % done
	{'attribute', {'file', _}} ->
	    take_header_1(Fs, As);    % discard
	_ ->
	    take_header_1(Fs, [F | As])    % keep
    end.

section_header(Name, Tree, Env) ->
    N = sets:size(Env#code.sources),
    if N > 1, Name =/= Env#code.target, Env#code.notes =/= no,
       Env#code.no_headers =/= true ->
	    Text = io_lib:fwrite("The following code stems "
				 "from module `~w'.", [Name]),
	    Header = comment([?COMMENT_BAR, "",
			      lists:flatten(Text), ""]),
	    erl_syntax:form_list([Header, Tree]);
       true ->
	    Tree
    end.

transform(Tree, Env, St) ->
    case erl_syntax:type(Tree) of
	application ->
	    transform_application(Tree, Env, St);
	attribute ->
	    transform_attribute(Tree, Env, St);
  	function ->
  	    transform_function(Tree, Env, St);
 	implicit_fun ->
 	    transform_implicit_fun(Tree, Env, St);
  	record_expr ->
  	    transform_record(Tree, Env, St);
  	record_index_expr ->
  	    transform_record(Tree, Env, St);
  	record_access ->
  	    transform_record(Tree, Env, St);
	_ ->
	    default_transform(Tree, Env, St)
    end.

default_transform(Tree, Env, St) ->
    case erl_syntax:subtrees(Tree) of
	[] ->
	    {Tree, St};
	Gs ->
	    {Gs1, St1} = transform_1(Gs, Env, St),
	    Tree1 = rewrite(Tree, erl_syntax:make_tree(
				    erl_syntax:type(Tree),
				    Gs1)),
	    {Tree1, St1}
    end.

transform_1([G | Gs], Env, St) ->
    {G1, St1} = transform_list(G, Env, St),
    {Gs1, St2} = transform_1(Gs, Env, St1),
    {[G1 | Gs1], St2};
transform_1([], _Env, St) ->
    {[], St}.

transform_list([T | Ts], Env, St) ->
    {T1, St1} = transform(T, Env, St),
    {Ts1, St2} = transform_list(Ts, Env, St1),
    {[T1 | Ts1], St2};
transform_list([], _Env, St) ->
    {[], St}.

%% Renaming function definitions

transform_function(T, Env, St) ->
    {T1, St1} = default_transform(T, Env, St),
    F = erl_syntax_lib:analyze_function(T1),
    {V, Text} = case (Env#code.map)(F) of
		    F ->
			%% Not renamed
			{none, []};
		    {Atom, _Arity} ->
			%% Renamed
			Cs = erl_syntax:function_clauses(T1),
			N = rename_atom(
			      erl_syntax:function_name(T1),
			      Atom),
			T2 = erl_syntax:function(N, Cs),
			{{value, T2}, renaming_note(Atom)}
		end,
    {maybe_modified(V, T1, 2, Text, Env), St1}.

renaming_note(Name) ->
    [lists:flatten(io_lib:fwrite("renamed function to `~w'",
				 [Name]))].

rename_atom(Node, Atom) ->
    rewrite(Node, erl_syntax:atom(Atom)).

%% Renaming "implicit fun" expressions (done quietly).

transform_implicit_fun(T, Env, St) ->
    {T1, St1} = default_transform(T, Env, St),
    {V, Text} = case erl_syntax:type(erl_syntax:implicit_fun_name(T1)) of
		    arity_qualifier ->
			F = erl_syntax_lib:analyze_implicit_fun(T1),
			case (Env#code.map)(F) of
			    F ->
				%% Not renamed
				{none, []};
			    {Atom, Arity} ->
				%% Renamed
				N = rewrite(
				      erl_syntax:implicit_fun_name(T1),
				      erl_syntax:arity_qualifier(
					erl_syntax:atom(Atom),
					erl_syntax:integer(Arity))),
				T2 = erl_syntax:implicit_fun(N),
				{{value, T2}, ["function was renamed"]}
			end;
		    module_qualifier ->
			{none, []}
		end,
    {maybe_modified_quiet(V, T1, 2, Text, Env), St1}.

%% Transforming function applications

transform_application(T, Env, St) ->
    %% We transform the arguments first, so we can concentrate on the
    %% application itself after that point.
    {As, St1} = transform_list(
		  erl_syntax:application_arguments(T),
		  Env, St),
    F = erl_syntax:application_operator(T),

    %% See if the operator is an explicit function name.
    %% (Usually, this will be the case.)
    case catch {ok, erl_syntax_lib:analyze_function_name(F)} of
	{ok, Name} ->
	    transform_application_1(Name, F, As, T, Env, St1);
	syntax_error ->
	    %% Oper is not a function name, but might be any other
	    %% expression - we just visit it and reassemble the
	    %% application.
	    %% We do not handle applications of tuples `{M, F}'.
	    {F1, St2} = transform(F, Env, St1),
	    {rewrite(T, erl_syntax:application(F1, As)), St2};
	{'EXIT', R} ->
	    exit(R);
	R ->
	    throw(R)
    end.

%% At this point we should have an explicit function name, which might
%% or might not be qualified by a module name.

transform_application_1(Name, F, As, T, Env, St) ->
    %% Before doing anything else, we must unfold any uses of aliases
    %% whose definitions have been killed.
    Arity = length(As),
    {Name1, F1} = expand_operator(Name, Arity, F, Env),
    F2 = maybe_modified_quiet(F1, F, 7, ["unfolded alias"], Env),
    {V, St1} = transform_application_2(Name1, Arity, F2, As, Env,
				       St),
    T1 = rewrite(T, erl_syntax:application(F2, As)),
    T3 = case V of
	     none ->
		 T1;
	     {value, {T2, Depth, Message}} ->
		 maybe_modified_quiet({value, T2}, T1, Depth,
				      Message, Env)
	 end,
    {T3, St1}.

%% Here, Name has been expanded if necessary (if so, this is also
%% reflected by F), and As have been transformed. We should return
%% `{none, State}' if no further rewriting is necessary, and otherwise
%% `{{value, {Tree, Depth, Message}}, State}', where `Depth' and
%% `Message' are to be passed to `maybe_modified'.

transform_application_2(Name, Arity, F, As, Env, St)
  when is_atom(Name) ->
    transform_atom_application(Name, Arity, F, As, Env, St);
transform_application_2({M, N}, Arity, F, As, Env, St)
  when is_atom(M), is_atom(N) ->
    transform_qualified_application(M, N, Arity, F, As, Env, St);
transform_application_2(_Name, _Arity, _F, _As, _Env, St) ->
    {none, St}.    % strange name - do nothing.

expand_operator(Name, Arity, _F, Env) when is_atom(Name) ->
    %% An unqualified function name - must be looked up. However, we
    %% must first check that it is not an auto-imported name - these
    %% have precedence over normal imports. We do a sanity check on the
    %% found arity.
    case is_auto_import({Name, Arity}) of
	true ->
	    {Name, none};    % auto-import - never expand.
	false ->
	    case dict:find({Name, Arity}, Env#code.expand) of
		{ok, {M, {N, A}}} when A =:= Arity ->
		    %% Expand to a qualified name.
		    F1 = erl_syntax:module_qualifier(
			   erl_syntax:atom(M),
			   erl_syntax:atom(N)),
		    {{M, N}, {value, F1}};
		error ->
		    %% Not in the table - leave it unchanged
		    {Name, none}
	    end
    end;
expand_operator(Name, _Arity, _F, _Env) ->
    %% Qualified function name - leave it unchanged
    {Name, none}.

%% Transforming an application of a named function without module
%% qualifier (often misleadingly called "local" applications). Note that
%% since the `apply', `spawn' and `spawn_link' functions are implicitly
%% imported (from module `erlang'), applications of these names cannot
%% refer to functions defined in the source code.

transform_atom_application(Name, Arity, F, As, Env, St) ->
    %% Catch applications of `apply' and `spawn'.
    case {Name, Arity} of
	{'apply', 2} ->
	    warning_apply_2(Env#code.module, Env#code.target),
	    {none, St};
	{'apply', 3} ->
	    transform_apply_call(F, As, Env, St);
	{'spawn', 3} ->
	    transform_spawn_call(F, As, Env, St);
	{'spawn', 4} ->
	    transform_spawn_call(F, As, Env, St);
	{'spawn_link', 3} ->
	    transform_spawn_call(F, As, Env, St);
	{'spawn_link', 4} ->
	    transform_spawn_call(F, As, Env, St);
	_ ->
	    %% A simple call of an unqualified function name - just
	    %% remap the name as necessary. Auto-imported names may not
	    %% be changed - the call never refers to a local function.
	    %% We do a sanity check on the arity.
	    case is_auto_import({Name, Arity}) of
		true ->
		    {none, St};    % auto-import - do not change.
		false ->
		    case (Env#code.map)({Name, Arity}) of
			{N, A} when N =:= Name, A =:= Arity ->
			    %% Not changed.
			    {none, St};
			{N, A} when A =:= Arity ->
			    %% The callee (in the current module)
			    %% was renamed.
			    F1 = rewrite(F, erl_syntax:atom(N)),
			    T = erl_syntax:application(F1, As),
			    V = {T, 2, ["callee was renamed"]},
			    {{value, V}, St}
		    end
	    end
    end.

%% Transforming an application of an explicitly named function qualified
%% with an (also explicit) module name. (Often called "remote"
%% applications.)

transform_qualified_application(Module, Name, Arity, F, As, Env, St) ->
    %% Catch applications of `apply' and `spawn'.
    case {Module, Name, Arity} of
	{'erlang', 'apply', 2} ->
	    warning_apply_2(Env#code.module, Env#code.target),
	    {none, St};
	{'erlang', 'apply', 3} ->
	    transform_apply_call(F, As, Env, St);
	{'erlang', 'spawn', 3} ->
	    transform_spawn_call(F, As, Env, St);
	{'erlang', 'spawn', 4} ->
	    transform_spawn_call(F, As, Env, St);
	{'erlang', 'spawn_link', 3} ->
	    transform_spawn_call(F, As, Env, St);
	{'erlang', 'spawn_link', 4} ->
	    transform_spawn_call(F, As, Env, St);
	_ ->
	    case erlang:is_builtin(Module, Name, Arity) of
		false ->
		    transform_qualified_application_1(
		      Module, Name, Arity, F, As, Env, St);
		true ->
		    {none, St}
	    end
    end.

transform_qualified_application_1(Module, Name, Arity, F, As, Env,
				  St) ->
    MakeLocal = fun (N) ->
			F1 = rewrite(F, erl_syntax:atom(N)),
			erl_syntax:application(F1, As)
		end,
    MakeRemote = fun () ->
			 erl_syntax:application(F, As)
		 end,
    MakeDynamic = fun(M, N) ->
			  F1 = erl_syntax:module_qualifier(
				 erl_syntax:atom(M),
				 erl_syntax:atom(N)),
			  F2 = rewrite(F, F1),
			  erl_syntax:application(F2, As)
		  end,
    localise(Module, Name, Arity, MakeLocal, MakeRemote,
	     MakeDynamic, 3, Env, St).

%% For an `apply/3' call, if we know the called module and function
%% names, and the number of arguments, then we can rewrite it to a
%% direct remote call - and if we do not, there is nothing we can
%% change.

transform_apply_call(F, As, Env, St) ->
    [Module, Name, List] = As,
    case (erl_syntax:type(Module) =:= atom)
	and (erl_syntax:type(Name) =:= atom)
	and erl_syntax:is_proper_list(List) of
	true ->
	    transform_apply_call_1(Module, Name, List, F, As, Env,
				   St);
	false ->
	    %% We can't get enough information about the
	    %% arguments to the `apply' call, so we do nothing
	    %% but warn.
	    warning_unsafe_call(apply, Env#code.module,
				Env#code.target),
	    {none, St}
    end.

%% Rewrite the apply-call to a static qualified call and handle that
%% instead.

transform_apply_call_1(Module, Name, List, F, _As, Env, St) ->
    F1 = rewrite(F, erl_syntax:module_qualifier( Module, Name)),
    As1 = erl_syntax:list_elements(List),
    M = erl_syntax:atom_value(Module),
    N = erl_syntax:atom_value(Name),
    A = length(As1),
    transform_qualified_application_1(M, N, A, F1, As1, Env, St).

%% `spawn' and `spawn_link' (with arity 3 or 4) are very much like
%% `apply/3', but there could be an extra `Node' argument. Note that `F'
%% below can represent both `spawn' and `spawn_link'.

transform_spawn_call(F, As, Env, St) ->
    case As of
	[Module, Name, List] ->
	    MakeSpawn = fun (As1) ->
				erl_syntax:application(F, As1)
			end,
	    transform_spawn_call_1(Module, Name, List, MakeSpawn,
				   Env, St);
	[Node, Module, Name, List] ->
	    MakeSpawn = fun (As1) ->
				erl_syntax:application(
				  F, [Node | As1])
			end,
	    transform_spawn_call_1(Module, Name, List, MakeSpawn,
				   Env, St)
    end.

%% Here, we can treat all dynamic-lookup spawns like `spawn/3'.

transform_spawn_call_1(Module, Name, List, MakeSpawn, Env, St) ->
    case (erl_syntax:type(Module) =:= atom)
	and (erl_syntax:type(Name) =:= atom)
	and erl_syntax:is_proper_list(List)
	of
	true ->
	    transform_spawn_call_2(Module, Name, List, MakeSpawn,
				   Env, St);
	_ ->
	    %% We can't get enough information about the arguments to
	    %% the `spawn' call, so we do nothing but warn.
	    warning_unsafe_call(spawn, Env#code.module,
				Env#code.target),
	    {none, St}
    end.

transform_spawn_call_2(Module, Name, List, MakeSpawn, Env, St) ->
    As = erl_syntax:list_elements(List),
    Arity = length(As),
    MakeLocal = fun (N) ->
			%% By using `spawn-a-fun', we do not have to
			%% force the callee to be exported.
			A = rewrite(Name, erl_syntax:atom(N)),
			B = erl_syntax:application(A, As),
			C = erl_syntax:clause([], [B]),
			F = erl_syntax:fun_expr([C]),
			MakeSpawn([F])
		end,
    MakeRemote = fun () ->
			 MakeSpawn([Module, Name, List])
		 end,
    MakeDynamic = fun (M, N) ->
			  F = rewrite(Name, erl_syntax:atom(N)),
			  MakeSpawn([erl_syntax:atom(M), F, List])
		  end,
    localise(erl_syntax:atom_value(Module),
	     erl_syntax:atom_value(Name),
	     Arity, MakeLocal, MakeRemote, MakeDynamic,
	     4, Env, St).

%% MakeLocal = (atom()) -> syntaxTree()
%% MakeRemote = () -> syntaxTree()
%% MakeDynamic = (atom(), atom()) -> syntaxTree()
%% localise(...) -> {none, state()} | {{value, V}, State}

localise(Module, Name, Arity, MakeLocal, MakeRemote, MakeDynamic,
	 Depth, Env, St) ->
    %% Is the callee in one of the source modules?
    case sets:is_element(Module, Env#code.sources) of
	false ->
	    case dict:find(Module, Env#code.redirect) of
		{ok, Module1} ->
		    T = MakeDynamic(Module1, Name),
		    V = {T, Depth, ["redirected call"]},
		    {{value, V}, St};
		error ->
		    {none, St}    % Nothing needs doing.
	    end;
	true ->
	    %% Remap the name of the callee, as necessary. Do a sanity
	    %% check on the arity.
	    Map = (Env#code.renaming)(Module),
	    Name1 = case Map({Name, Arity}) of
			{N, A} when A =:= Arity ->
			    N
		    end,

	    %% See if the callee module is "safe" and/or "static".
	    Safe = sets:is_element(Module, Env#code.safe),
	    Static = (sets:is_element(Module, Env#code.static)
		      or Safe),

	    %% Select what kind of code to generate for the call:
	    case Static of
		false ->
		    %% (This also implies that the called module is not
		    %% the target module - which is always "static" -
		    %% and that it is not "safe".) The called module
		    %% could be replaced dynamically, independent of the
		    %% target module, so we must protect the localised
		    %% call. We strip all comments from the localised
		    %% code, to avoid getting the same comments twice.
		    L = MakeLocal(Name1),
		    L1 = erl_syntax_lib:strip_comments(L),
		    R = MakeRemote(),
		    {T, Text} = protect_call(Module, L1, R),
		    V = {T, Depth, Text},
		    {{value, V}, St};
		true ->
		    %% In this case, the called module is never replaced
		    %% unless the target module also is. (N.B.: These
		    %% might be the same module.)
		    case Safe of
			false ->
			    %% The normal code replacement semantics
			    %% must be preserved here, so the generated
			    %% call must be qualified with the name of
			    %% the target module. (We assume this is
			    %% efficiently compiled even if we do not
			    %% insert an explicit "latest version"
			    %% test.)
			    Target = Env#code.target,
			    case Module =:= Target of
				true ->
				    %% Already calling the target module
				    %% - do not insert irritating notes.
				    {none, St};
				false ->
				    %% We must ensure that the function
				    %% is exported.
				    St1 = state__add_export(Name1,
							    Arity, St),
				    T = MakeDynamic(Target, Name1),
				    Text = ["localised call"],
				    V = {T, Depth, Text},
				    {{value, V}, St1}
			    end;
			true ->
			    %% The call is regarded as safe to localise
			    %% completely. Code replacement will in
			    %% general not be detected (except for
			    %% spawn/apply).
			    T = MakeLocal(Name1),
			    Text = ["localised safe call"],
			    V = {T, Depth, Text},
			    {{value, V}, St}
		    end
	    end
    end.

%%% %% This creates a test on whether there is a later loaded version of
%%% %% Module: if not, select the `Local' expression, otherwise the `Remote'
%%% %% expression. We knowingly duplicate code here, to allow better
%%% %% optimisations, but we never duplicate work.
%%%
%%% protect_call(Module, Local, Remote) ->
%%%     T = erl_syntax:if_expr(
%%% 	  [erl_syntax:clause([erl_syntax:application(
%%% 				erl_syntax:atom('not_replaced'),
%%% 				[erl_syntax:atom(Module)])],
%%% 			     [Local]),
%%% 	   erl_syntax:clause([erl_syntax:atom('true')],
%%% 			     [Remote])]),
%%%     {T, ["localised dynamic call"]}.

%% This "protects" a localised call by letting it remain a remote call.

protect_call(_Module, _Local, Remote) ->
    {Remote, ["dynamic call"]}.

%% Renaming record declarations

transform_attribute(T, Env, St) ->
    {T1, St1} = TSt1 = default_transform(T, Env, St),
    case erl_syntax_lib:analyze_attribute(T1) of
	{record, {R, _}} ->
	    F = fun(R) ->
			[_ | As] = erl_syntax:attribute_arguments(T1),
			erl_syntax:attribute(
			  erl_syntax:attribute_name(T1),
			  [erl_syntax:atom(R) | As])
		end,
	    {V, Text} = rename_record(R, F, Env),
	    {maybe_modified(V, T1, 2, Text, Env), St1};
	_ ->
	    TSt1
    end.

%% This handles renaming of records.

transform_record(T, Env, St) ->
    {T1, St1} = TSt1 = default_transform(T, Env, St),
    X = case catch erl_syntax_lib:analyze_record_expr(T1) of
	    {record_expr, {R, _}} ->
		F = fun (R) ->
			    erl_syntax:record_expr(
			      erl_syntax:record_expr_argument(T1),
			      erl_syntax:atom(R),
			      erl_syntax:record_expr_fields(T1))
		    end,
		{R, F};
	    {record_index_expr, {R, _}} ->
		F = fun (R) ->
			    erl_syntax:record_index_expr(
			      erl_syntax:atom(R),
			      erl_syntax:record_index_expr_field(T1))
		    end,
		{R, F};
	    {record_access, {R, _}} ->
		F = fun (R) ->
			    erl_syntax:record_access(
			      erl_syntax:record_access_argument(T1),
			      erl_syntax:atom(R),
			      erl_syntax:record_access_field(T1))
		    end,
		{R, F};
	    _Type ->
		false
	end,
    case X of
	{R1, F1} ->
	    {V, Text} = rename_record(R1, F1, Env),
	    {maybe_modified(V, T1, 1, Text, Env), St1};
	false ->
	    TSt1
    end.

rename_record(R, F, Env) ->
    case map_record_name(R, Env#code.map) of
	R ->
	    %% Not renamed
	    {none, []};
	R1 ->
	    %% Renamed
	    {{value, F(R1)}, ["record was renamed"]}
    end.

%% Maybe-rewriting Node, adding modification notes.

%% This is for non-primary modifications; they are not commented unless
%% the `notes' option is set to `always'.

maybe_modified_quiet(V, Node, Depth, Message, Env) ->
    case Env#code.notes of
	always ->
	    maybe_modified_1(V, Node, Depth, Message, yes);
	_ ->
	    maybe_modified_1(V, Node, Depth, Message, no)
    end.

%% This is for important notes; they are only disabled if the `notes'
%% option is set to `no'.

maybe_modified(V, Node, Depth, Message, Env) ->
    maybe_modified_1(V, Node, Depth, Message, Env#code.notes).

maybe_modified_1(none, Node, _Depth, _Message, _Notes) ->
    Node;
maybe_modified_1({value, Node1}, Node, Depth, Message, Notes) ->
    case Notes of
	no ->
	    rewrite(Node, Node1);
	_ ->
	    Code = erl_syntax:comment_text(
		     erl_syntax_lib:to_comment(
		       erl_syntax_lib:strip_comments(
			 erl_syntax_lib:limit(Node, Depth)),
		       "\040\040")),
	    erl_syntax:add_precomments(
	      [comment_note(Message ++
			    ["Original code:" | Code])],
	      rewrite(Node, Node1))
    end.


%% =====================================================================
%% @spec create_stubs(Stubs::[stubDescriptor()], Options::[term()]) ->
%%           [string()]
%%
%% @doc Creates stub module source files corresponding to the given stub
%% descriptors. The returned value is the list of names of the created
%% files. See `merge_sources/3' for more information about
%% stub descriptors.
%%
%% Options:
%% <dl>
%%   <dt>`{backup_suffix, string()}'</dt>
%%   <dt>`{backups, boolean()}'</dt>
%%   <dt>`{printer, Function}'</dt>
%%   <dt>`{stub_dir, filename()}'</dt>
%%   <dt>`{suffix, string()}'</dt>
%%   <dt>`{verbose, boolean()}'</dt>
%% </dl>
%% 
%% See `merge/3' for details on these options.
%%
%% @see merge/3
%% @see merge_sources/3

-spec create_stubs([stubDescriptor()], [option()]) -> [string()].

create_stubs(Stubs, Opts) ->
    Opts1 = Opts ++ ?DEFAULT_MERGE_OPTS,
    lists:foldl(fun (S, Fs) ->
			F = create_stub(S, Opts1),
			[F | Fs]
		end,
		[], Stubs).

maybe_create_stubs(Stubs, Opts) ->
    case proplists:get_bool(stubs, Opts) of
	true ->
	    create_stubs(Stubs, Opts);
	false ->
	    []
    end.

create_stub({Name, Fs, Attrs}, Opts) ->
    Defs = [stub_function(F) || F <- Fs],
    Exports = [F || {F, _} <- Fs],
    Forms = stub_header(Name, Exports, Attrs) ++ Defs,
    Dir = proplists:get_value(stub_dir, Opts, ""),
    verbose("creating stub file for module `~w'.", [Name], Opts),
    write_module(erl_syntax:form_list(Forms), Name, Dir, Opts).

%% We just follow the arity specifications naively when we create the
%% stub funcion - it is not our responsibility to check them.

stub_function({{F, A}, {M, {F1, A1}}}) ->
    Vs = var_list(A),
    Vs1 = var_list(A1),
    R = erl_syntax:module_qualifier(erl_syntax:atom(M),
				    erl_syntax:atom(F1)),
    Call = erl_syntax:application(R, Vs1),
    erl_syntax:function(erl_syntax:atom(F),
			[erl_syntax:clause(Vs, [], [Call])]).

var_list(N) ->
    var_list(N, 1).

var_list(N, I) when N > 0 ->
    [erl_syntax:variable("X" ++ integer_to_list(I))
     | var_list(N - 1, I + 1)];
var_list(0, _) ->
    [].

stub_header(Name, Exports, Attrs) ->
    [comment([?COMMENT_BAR,
	      io_lib:fwrite("This is an automatically "
			    "generated stub interface\n"
			    "for the module `~w'.",
			    [Name]),
	      "",
	      timestamp(),
	      ""]),
     erl_syntax:attribute(erl_syntax:atom('module'),
			  [erl_syntax:atom(Name)]),
     make_export(Exports)]
	++ make_attributes(Attrs).


%% =====================================================================

-type renamings() :: [{atom(), atom()}].

%% =====================================================================
%% @spec rename(Files::[filename()], Renamings) -> [string()]
%% @equiv rename(Files, Renamings, [])

-spec rename([file:filename()], renamings()) -> [string()].

rename(Files, Renamings) ->
    rename(Files, Renamings, []).

%% =====================================================================
%% @spec rename(Files::[filename()], Renamings, Options::[term()]) ->
%%           [string()]
%%
%%     Renamings = [{atom(), atom()}]
%%
%% @doc Renames a set of possibly interdependent source code modules.
%% `Files' is a list of file names of source modules to be
%% processed. `Renamings' is a list of pairs of <em>module
%% names</em>, representing a mapping from old names to new. The
%% returned value is the list of output file names.
%%
%% Each file in the list will be read and processed separately. For
%% every file, each reference to some module M, such that there is an
%% entry `{<em>M</em>, <em>M1</em>}' in
%% `Renamings', will be changed to the corresponding M1.
%% Furthermore, if a file F defines module M, and there is an entry
%% `{<em>M</em>, <em>M1</em>}' in `Renamings', a
%% new file named `<em>M1</em>.erl' will be created in the
%% same directory as F, containing the source code for module M, renamed
%% to M1. If M does not have an entry in `Renamings', the
%% module is not renamed, only updated, and the resulting source code is
%% written to `<em>M</em>.erl' (typically, this overwrites
%% the original file). The `suffix' option (see below) can be
%% used to change the default "`.erl'" suffix for the
%% generated files.
%%
%% Stub modules will automatically be created (see the
%% `stubs' and `stub_dir' options below) for each
%% module that is renamed. These can be used to redirect any calls still
%% using the old module names. The stub files are created in the same
%% directory as the source file (typically overwriting the original
%% file).
%%
%% Options:
%% <dl>
%%   <dt>`{backup_suffix, string()}'</dt>
%%   <dt>`{backups, boolean()}'</dt>
%%   <dt>`{printer, Function}'</dt>
%%   <dt>`{stubs, boolean()}'</dt>
%%   <dt>`{suffix, string()}'</dt>
%% </dl>
%% See `merge/3' for details on these options.
%%
%% <dl>
%%   <dt>`{comments, boolean()}'</dt>
%%   <dt>`{preprocess, boolean()}'</dt>
%% </dl>
%% See `merge_files/4' for details on these options.
%%
%% <dl>
%%   <dt>`{no_banner, boolean()}'</dt>
%% </dl>
%% For the `rename' function, this option is
%% `true' by default. See `merge_sources/3' for
%% details.
%%
%% <dl>
%%   <dt>`{tidy, boolean()}'</dt>
%% </dl>
%% For the `rename' function, this option is
%% `false' by default. See `merge_sources/3' for
%% details.
%%
%% <dl>
%%   <dt>`{no_headers, boolean()}'</dt>
%%   <dt>`{stub_dir, filename()}'</dt>
%% </dl>
%% These options are preset by the `rename' function and
%% cannot be overridden by the user.
%%
%% See `merge_sources/3' for further options.
%%
%% @see merge/3
%% @see merge_sources/3
%% @see merge_files/4

-spec rename([file:filename()], renamings(), [term()]) -> [string()].

rename(Files, Renamings, Opts) ->
    Dict = case is_atom_map(Renamings) of
	       true ->
		   dict:from_list(Renamings);
	       false ->
		   report_error("bad module renaming: ~P.",
				[Renamings, 10]),
		   exit(error)
	   end,
    %% We disable *all* automatic source code lookup, for safety: you
    %% are only allowed to do renaming on a module if you give its path.
    Opts1 = [{find_src_rules, []}]
	++ Opts ++ [{backup_suffix, ?DEFAULT_BACKUP_SUFFIX},
		    backups, 
		    {printer, fun default_printer/2},
		    stubs,
		    {suffix, ?DEFAULT_SUFFIX},
		    comments, 
		    {preprocess, false},
		    {tidy, false},
		    no_banner,
		    {notes, no},
		    {verbose, false}],
    lists:flatmap(fun (F) -> rename_file(F, Dict, Opts1) end, Files).

rename_file(File, Dict, Opts) ->
    {S, Enc} = read_module(File, Opts),
    %% Try to avoid *two* coding: comments:
    Encoding = [{encoding, Enc} ||
                   Enc =/= none,
                   not proplists:get_bool(comments, Opts)],
    M = get_module_info(S),
    Name = M#module.name,
    Name1 = case dict:find(Name, Dict) of
		{ok, N} -> N;
		error -> Name
	    end,
    %% We convert the dictionary to a new list to ensure that we use the
    %% exact same renaming for redirections. We must remove the current
    %% module from the redirection set.
    Dict1 = dict:erase(Name, Dict),
    Opts1 = [no_headers,
	     {export, [Name]},
	     {static, [Name]},
	     {redirect, dict:to_list(Dict1)}] ++ Encoding ++ Opts,
    {Tree, Stubs} = merge_sources(Name1, [S], Opts1),
    Dir = filename:dirname(filename(File)),
    File1 = write_module(Tree, Name1, Dir, Opts++Encoding),

    %% We create the stub file in the same directory as the source file
    %% and the target file.
    [File1 | maybe_create_stubs(Stubs, [{stub_dir, Dir} | Opts1])].


%% ---------------------------------------------------------------------
%% Initialise a module-info record with data about the module
%% represented by the syntax tree (or list of "forms"). Listed exports
%% are guaranteed to be in the set of function names.

get_module_info(Forms) ->
    L = case catch {ok, erl_syntax_lib:analyze_forms(Forms)} of
	    {ok, L1} ->
		L1;
	    syntax_error ->
		report_error("syntax error in input."),
		erlang:error(badarg);
	    {'EXIT', R} ->
		exit(R);
	    R ->
		throw(R)
	end,
    {Name, Vars} =
	case lists:keyfind(module, 1, L) of
	    {module, {_N, _Vs} = NVs} ->
		NVs;
	    {module, N} ->
		{N, none};
	    false ->
		report_error("in source code: module name missing."),
		exit(error)
	end,
    case lists:keyfind(errors, 1, L) of
	{errors, Ds} when Ds =/= [] ->
	    report_errors(Ds, Name),
	    exit(error);
	_ ->
	    ok
    end,
    case lists:keyfind(warnings, 1, L) of
	{warnings, Ds1} when Ds1 =/= [] ->
	    report_warnings(Ds1, Name); 
	_ ->
	    ok
    end,
    Functions = case lists:keyfind(functions, 1, L) of
		    {functions, Fs} ->
			ordsets:from_list(Fs);
		    _ ->
			[]
		end,
    Exports = case lists:keyfind(exports, 1, L) of
		  {exports, Es} ->
		      ordsets:from_list(Es);
		  _ ->
		      []
	      end,
    Imports = case lists:keyfind(imports, 1, L) of
		  {imports, Is} ->
		      expand_imports(Is, Name);
		  _ ->
		      []
	      end,
    Attributes = case lists:keyfind(attributes, 1, L) of
		     {attributes, As} ->
			 ordsets:from_list(As);
		     _ ->
			 []
		 end,
    Records = case lists:keyfind(records, 1, L) of
		  {records, Rs} ->
		      fold_record_fields(Rs);
		  _ ->
		      []
	      end,
    check_records(Records, Name),
    #module{name = Name,
	    vars = Vars,
	    functions = Functions,
	    exports = ordsets:intersection(Exports, Functions),
	    aliases = Imports,
	    attributes = Attributes,
	    records = Records}.

fold_record_fields(Rs) ->
    [{N, [fold_record_field(F) || F <- Fs]} || {N, Fs} <- Rs].

fold_record_field({_Name, {none, _Type}} = None) ->
    None;
fold_record_field({Name, {F, Type}}) ->
    case erl_syntax:is_literal(F) of
	true ->
	    {Name, {value, erl_syntax:concrete(F)}, Type};
	false ->
	    %% The default value for the field is not a constant, so we
	    %% represent it by a hash value instead. (We don't want to
	    %% do this in the general case.)
	    {Name, {hash, erlang:phash(F, 16#ffffff)}, Type}
    end;
%% The following two clauses handle code before Erlang/OTP 19.0.
fold_record_field({_Name, none} = None) ->
    None;
fold_record_field({Name, F}) ->
    case erl_syntax:is_literal(F) of
	true ->
	    {Name, {value, erl_syntax:concrete(F)}};
	false ->
	    %% The default value for the field is not a constant, so we
	    %% represent it by a hash value instead. (We don't want to
	    %% do this in the general case.)
	    {Name, {hash, erlang:phash(F, 16#ffffff)}}
    end.

report_errors([D | Ds], Name) ->
    report_error("error: " ++ error_text(D, Name)),
    report_errors(Ds, Name);
report_errors([], _) ->
    ok.

report_warnings([D | Ds], Name) ->
    report_warning(error_text(D, Name)),
    report_errors(Ds, Name);
report_warnings([], _) ->
    ok.

error_text(D, Name) ->
    case D of
	{L, M, E} when is_integer(L), is_atom(M) ->
	    case catch M:format_error(E) of
		S when is_list(S) ->
		    io_lib:fwrite("`~w', line ~w: ~ts.",
				  [Name, L, S]);
		_ ->
		    error_text_1(D, Name)
	    end;
	_E ->
	    error_text_1(D, Name)
    end.

error_text_1(D, Name) ->
    io_lib:fwrite("error: `~w', ~P.", [Name, D, 15]).

check_records(Rs, Name) ->
    case duplicates([N || {N, _} <- Rs]) of
	[] ->
	    ok;
	Ns ->
	    report_error("in module `~w': "
			 "multiply defined records: ~p.",
			 [Name, Ns]),
	    exit(error)
    end.

expand_imports(Is, Name) ->
    Fs = ordsets:from_list(lists:append([[{M, F} || F <- Fs]
					 || {M, Fs} <- Is])),
    As = erl_syntax_lib:function_name_expansions(Fs),
    case duplicates([N || {N, _} <- As]) of
	[] ->
	    ordsets:from_list(As);
	Ns ->
	    report_error("in module `~w': "
			 "multiply imported functions: ~p.",
			 [Name, Ns]),
	    exit(error)
    end.


%% ---------------------------------------------------------------------
%% File handling

%% open_output_file(filename()) -> filedescriptor()

open_output_file(FName) ->
    case catch file:open(FName, [write]) of
	{ok, FD} ->
	    FD;
	{error, _} = Error ->
	    error_open_output(FName),
	    exit(Error);
	{'EXIT', R} ->
	    error_open_output(FName),
	    exit(R);
	R ->
	    error_open_output(FName),
	    exit(R)
    end.

output_encoding(FD, Opts) ->
    case proplists:get_value(encoding, Opts) of
        undefined ->
            ok = io:setopts(FD, [{encoding, epp:default_encoding()}]);
        Encoding ->
            ok = io:setopts(FD, [{encoding, Encoding}]),
            EncS = epp:encoding_to_string(Encoding),
            ok = io:fwrite(FD, <<"%% ~s\n">>, [EncS])
    end.

%% read_module(Name, Options) -> {syntaxTree(), epp:source_encoding()}
%%
%% This also tries to locate the real source file, if "Name" does not
%% point directly to a particular file.

read_module(Name, Options) ->
    case file_type(Name) of
	{value, _} ->
	    read_module_1(Name, Options);
	none ->
	    Rules = proplists:get_value(find_src_rules, Options),
	    case find_src(Name, Rules) of
		{error, _} ->
		    %% It seems that we have no file - go on anyway,
		    %% just to get a decent error message.
		    read_module_1(Name, Options);
		{Name1, _} ->
		    read_module_1(Name1 ++ ".erl", Options)
	    end
    end.

read_module_1(Name, Options) ->
    verbose("reading module `~ts'.", [filename(Name)], Options),
    {Forms, Enc} = read_module_2(Name, Options),
    case proplists:get_bool(comments, Options) of
	false ->
	    {Forms, Enc};
	true ->
	    Comments = erl_comment_scan:file(Name),
	    {erl_recomment:recomment_forms(Forms, Comments), Enc}
    end.

read_module_2(Name, Options) ->
    case read_module_3(Name, Options) of
	{ok, Forms} ->
	    check_forms(Forms, Name), 
            Enc = epp:read_encoding(Name),
	    {Forms, Enc};
	{error, _} = Error ->
	    error_read_file(Name),
	    exit(Error)
    end.

read_module_3(Name, Options) ->
    case proplists:get_bool(preprocess, Options) of
	false ->
	    epp_dodger:parse_file(Name);
	true ->
	    read_module_4(Name, Options)
    end.

read_module_4(Name, Options) ->
    Includes = proplists:append_values(includes, Options)
	++ [filename:dirname(Name) | ?DEFAULT_INCLUDES],
    Macros = proplists:append_values(macros, Options)
	++ ?DEFAULT_MACROS,
    epp:parse_file(Name, Includes, Macros).

check_forms([F | Fs], File) ->
    case erl_syntax:type(F) of
	error_marker ->
	    S = case erl_syntax:error_marker_info(F) of
		    {_, M, D} ->
			M:format_error(D);
		    _ ->
			"unknown error"
		end,
	    report_error("in file `~ts' at line ~w:\n  ~ts",
			 [filename(File), erl_syntax:get_pos(F), S]),
	    exit(error);
	_ ->
	    check_forms(Fs, File)
    end;
check_forms([], _) ->
    ok.

find_src(Name, undefined) ->
    filename:find_src(filename(Name));
find_src(Name, Rules) ->
    filename:find_src(filename(Name), Rules).

%% file_type(filename()) -> {value, Type} | none

file_type(Name) ->
    case catch file:read_file_info(Name) of
	{ok, Env} ->
	    {value, Env#file_info.type};
	{error, enoent} ->
	    none;
	{error, _} = Error ->
	    error_read_file_info(Name),
	    exit(Error);
	{'EXIT', R} ->
	    error_read_file_info(Name),
	    exit(R);
	R ->
	    error_read_file_info(Name),
	    throw(R)
    end.

%% Create the target directory and make a backup file if necessary, then
%% open the file, output the text and close the file safely. Returns the
%% file name.

write_module(Tree, Name, Dir, Opts) ->
    Name1 = filename(Name),
    Dir1 = filename(Dir),
    Base = if Dir1 =:= "" ->
		   Name1;
	      true ->
		   case file_type(Dir1) of
		       {value, directory} ->
			   ok;
		       {value, _} ->
			   report_error("`~ts' is not a directory.",
					[Dir1]),
			   exit(error);
		       none ->
			   case file:make_dir(Dir1) of
			       ok ->
				   verbose("created directory `~ts'.",
					   [Dir1], Opts),
				   ok;
			       E ->
				   report_error("failed to create "
						"directory `~ts'.",
						[Dir1]),
				   exit({make_dir, E})
			   end
		   end,
		   filename:join(Dir1, Name1)
	   end,
    Suffix = proplists:get_value(suffix, Opts, ""),
    File = Base ++ Suffix,
    case proplists:get_bool(backups, Opts) of
	true ->
	    backup_file(File, Opts);
	false ->
	    ok
    end,
    Printer = proplists:get_value(printer, Opts),
    FD = open_output_file(File),
    ok = output_encoding(FD, Opts),
    verbose("writing to file `~ts'.", [File], Opts),
    V = (catch {ok, output(FD, Printer, Tree, Opts)}),
    ok = file:close(FD),
    case V of
	{ok, _} ->
	    File;
	{'EXIT', R} ->
	    error_write_file(File),
	    exit(R);
	R ->
	    error_write_file(File),
	    throw(R)
    end.

output(FD, Printer, Tree, Opts) ->
    io:put_chars(FD, Printer(Tree, Opts)),
    io:nl(FD).

%% If the file exists, rename it by appending the given suffix to the
%% file name.

backup_file(Name, Opts) ->
    case file_type(Name) of
	{value, regular} ->
	    backup_file_1(Name, Opts);
	{value, _} ->
	    error_backup_file(Name),
	    exit(error);
	none ->
	    ok
    end.

%% The file should exist and be a regular file here.

backup_file_1(Name, Opts) ->
    Name1 = filename(Name),
    Suffix = proplists:get_value(backup_suffix, Opts, ""),
    Dest = filename:join(filename:dirname(Name1),
			 filename:basename(Name1) ++ Suffix),
    case catch file:rename(Name1, Dest) of
	ok ->
	    verbose("made backup of file `~ts'.", [Name1], Opts);
	{error, R} ->
	    error_backup_file(Name1),
	    exit({error, R});
	{'EXIT', R} ->
	    error_backup_file(Name1),
	    exit(R);
	R ->
	    error_backup_file(Name1),
	    throw(R)
    end.


%% =====================================================================
%% Utility functions

%% The form sequence returned by 'erl_tidy:module' is flat, even if the
%% given tree is not.

tidy(Tree, Opts) ->
    case proplists:get_bool(tidy, Opts) of
	true ->
	    verbose("tidying final module.", Opts),
	    erl_tidy:module(Tree, ?TIDY_OPTS);
	false ->
	    Tree
    end.

make_attributes(As) ->
    [make_attribute(A) || A <- As].

make_attribute({Name, Term}) ->
    erl_syntax:attribute(erl_syntax:atom(Name),
			 [erl_syntax:abstract(Term)]).

is_auto_import({F, A}) ->
    erl_internal:bif(F, A).

timestamp() ->
    {{Yr, Mth, Dy}, {Hr, Mt, Sc}} = erlang:localtime(),
    lists:flatten(io_lib:fwrite("Created by Igor "
				"~w-~2.2.0w-~2.2.0w, "
				"~2.2.0w:~2.2.0w:~2.2.0w.",
				[Yr, Mth, Dy, Hr, Mt, Sc])).

filename([C | T]) when is_integer(C), C > 0 ->
    [C | filename(T)];
filename([H|T]) ->
    filename(H) ++ filename(T);
filename([]) ->
    [];
filename(N) when is_atom(N) ->
    atom_to_list(N);
filename(N) ->
    report_error("bad filename: `~P'.", [N, 25]),
    exit(error).

duplicates(Xs) ->
    ordsets:from_list(Xs -- ordsets:from_list(Xs)).

split_list(F, L) ->
    split_list(L, F, [], []).

split_list([H | T], F, A1, A2) ->
    case F(H) of
	true ->
	    split_list(T, F, [H | A1], A2);
	false ->
	    split_list(T, F, A1, [H | A2])
    end;
split_list([], _, A1, A2) ->
    {lists:reverse(A1), lists:reverse(A2)}.

rewrite(Source, Target) ->
    erl_syntax:copy_attrs(Source, Target).

comment_note([L | Ls]) ->
    comment([?NOTE_HEADER ++ L | Ls], ?NOTE_PREFIX).

comment(Txt) ->
    comment(Txt, ?COMMENT_PREFIX).

comment(Txt, Prefix) ->
    erl_syntax:comment(prefix_lines(split_lines(Txt), Prefix)).

prefix_lines([L | Ls], Prefix) ->
    [Prefix ++ L | prefix_lines(Ls, Prefix)];
prefix_lines([], _) ->
    [].

split_lines(Ls) ->
    split_lines(Ls, []).

split_lines([L | Ls], Ls1) ->
    split_lines(Ls, split_lines(L, [], Ls1));
split_lines([], Ls1) ->
    lists:reverse(Ls1).

split_lines([$\r, $\n | Cs], Cs1, Ls) ->
    split_lines_1(Cs, Cs1, Ls);
split_lines([$\r | Cs], Cs1, Ls) ->
    split_lines_1(Cs, Cs1, Ls);
split_lines([$\n | Cs], Cs1, Ls) ->
    split_lines_1(Cs, Cs1, Ls);
split_lines([C | Cs], Cs1, Ls) ->
    split_lines(Cs, [C | Cs1], Ls);
split_lines([], Cs, Ls) ->
    [lists:reverse(Cs) | Ls].

split_lines_1(Cs, Cs1, Ls) ->
    split_lines(Cs, [], [lists:reverse(Cs1) | Ls]).


%% =====================================================================
%% Reporting

warning_unsafe_call(Name, Module, Target) ->
    report_warning("call to `~w' in module `~w' "
		   "possibly unsafe in `~s'.", [Name, Module, Target]).

warning_apply_2(Module, Target) ->
    report_warning("call to `apply/2' in module `~w' "
		   "possibly unsafe in `~s'.", [Module, Target]).

error_open_output(Name) ->
    report_error("cannot open file `~ts' for output.", [filename(Name)]).

error_read_file(Name) ->
    report_error("error reading file `~ts'.", [filename(Name)]).

error_read_file_info(Name) ->
    report_error("error getting file info: `~ts'.", [filename(Name)]).

error_write_file(Name) ->
    report_error("error writing to file `~ts'.", [filename(Name)]).

error_backup_file(Name) ->
    report_error("could not create backup of file `~ts'.",
		 [filename(Name)]).

verbose(S, Opts) ->
    verbose(S, [], Opts).

verbose(S, Vs, Opts) ->
    case proplists:get_bool(verbose, Opts) of
	true ->
	    report(S, Vs);
	false ->
	    ok
    end.

report_error(S) ->
    report_error(S, []).

report_error(S, Vs) ->
    report(S, Vs).

report_warning(S) ->
    report_warning(S, []).

report_warning(S, Vs) ->
    report("warning: " ++ S, Vs).

% report(S) ->
%     report(S, []).

report(S, Vs) ->
    io:fwrite(lists:concat([?MODULE, ": ", S, "\n"]), Vs).

