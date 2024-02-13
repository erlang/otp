%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2024. All Rights Reserved.
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
-module(cover).
-moduledoc """
A Coverage Analysis Tool for Erlang

The module `cover` provides a set of functions for coverage analysis of Erlang
programs, counting how many times each _executable line_ of code is executed
when a program is run. An executable line contains an Erlang expression such as
a matching or a function call. A blank line or a line containing a comment,
function head or pattern in a `case` or `receive` statement is not executable.

Coverage analysis can be used to verify test cases, making sure all relevant
code is covered, and may also be helpful when looking for bottlenecks in the
code.

Before any analysis can take place, the involved modules must be _cover
compiled_. This means that some extra information is added to the module before
it is compiled into a binary which then is loaded. The source file of the module
is not affected and no `.beam` file is created. If the runtime system supports
coverage natively, Cover will automatically use that functionality to lower the
execution overhead for cover-compiled code.

Each time a function in a Cover compiled module is called, information about the
call is added to an internal database of Cover. The coverage analysis is
performed by examining the contents of the Cover database. The output `Answer`
is determined by two parameters, `Level` and `Analysis`.

- `Level = module`

  `Answer = {Module,Value}`, where `Module` is the module name.

- `Level = function`

  `Answer = [{Function,Value}]`, one tuple for each function in the module. A
  function is specified by its module name `M`, function name `F` and arity `A`
  as a tuple `{M,F,A}`.

- `Level = clause`

  `Answer = [{Clause,Value}]`, one tuple for each clause in the module. A clause
  is specified by its module name `M`, function name `F`, arity `A` and position
  in the function definition `C` as a tuple `{M,F,A,C}`.

- `Level = line`

  `Answer = [{Line,Value}]`, one tuple for each executable line in the module. A
  line is specified by its module name `M` and line number in the source file
  `N` as a tuple `{M,N}`.

- `Analysis = coverage`

  `Value = {Cov,NotCov}` where `Cov` is the number of executable lines in the
  module, function, clause or line that have been executed at least once and
  `NotCov` is the number of executable lines that have not been executed.

- `Analysis = calls`

  `Value = Calls` which is the number of times the module, function, or clause
  has been called. In the case of line level analysis, `Calls` is the number of
  times the line has been executed.

_Distribution_

Cover can be used in a distributed Erlang system. One of the nodes in the system
must then be selected as the _main node_, and all Cover commands must be
executed from this node. The error reason `not_main_node` is returned if an
interface function is called on one of the remote nodes.

Use `cover:start/1` and `cover:stop/1` to add or remove nodes. The same Cover
compiled code will be loaded on each node, and analysis will collect and sum up
coverage data results from all nodes.

To only collect data from remote nodes without stopping `cover` on those nodes,
use `cover:flush/1`

If the connection to a remote node goes down, the main node will mark it as
lost. If the node comes back it will be added again. If the remote node was
alive during the disconnected period, cover data from before and during this
period will be included in the analysis.

## SEE ALSO

code(3), compile(3)
""".

%%
%% This module implements the Erlang coverage tool.
%% 
%% ARCHITECTURE
%%
%% The coverage tool consists of one process on each node involved in
%% coverage analysis. The process is registered as 'cover_server'
%% (?SERVER).  The cover_server on the 'main' node is in charge, and
%% it monitors the cover_servers on all remote nodes. When it gets a
%% 'DOWN' message for another cover_server, it marks the node as
%% 'lost'. If a nodeup is received for a lost node the main node
%% ensures that the cover compiled modules are loaded again. If the
%% remote node was alive during the disconnected period, cover data
%% for this period will also be included in the analysis.
%%
%% The cover_server process on the main node is implemented by the
%% functions init_main/1 and main_process_loop/1. The cover_server on
%% the remote nodes are implemented by the functions init_remote/2 and
%% remote_process_loop/1.
%%
%% COUNTERS
%%
%% The 'counters' modules is used for counting how many time each line
%% executed. Each cover-compiled module will have its own array of
%% counters.
%%
%% The counter reference for module Module is stored in a persistent
%% term with the key {cover,Module}.
%%
%% When the cover:local_only/0 function has been called, the reference
%% for the counter array will be compiled into each cover-compiled
%% module directly (instead of retrieving it from a persistent term).
%% That will be faster, but the resulting code can be only be used on
%% the main node.
%%
%% TABLES
%%
%% Each node has one table: ?COVER_MAPPING_TABLE.
%%
%% ?COVER_MAPPING_TABLE maps from a #bump{} record to an index in the
%% counter array for the module. It is used both during instrumentation
%% of cover-compiled modules and when collecting the counter values.
%%
%% The main node owns the table ?COLLECTION_TABLE. The counter data
%% is consolidated into this table from the counters on both the main
%% node and from remote nodes. This consolidation is done when a
%% remote node is stopped with cover:stop/1 or just before starting an
%% analysis.
%%
%% The main node also has a table named ?BINARY_TABLE. This table
%% contains the abstract code code for each cover-compiled
%% module. This is necessary so that the code can be loaded on remote
%% nodes that are started after the compilation.
%%
%% PARALLELISM
%%
%% To take advantage of SMP when doing the cover analysis both the data 
%% collection and analysis has been parallelized. One process is spawned for
%% each node when collecting data, and on the remote node when collecting data
%% one process is spawned per module. 
%% 
%% When analyzing data it is possible to issue multiple
%% analyse(_to_file)/X calls at once. They are, however, all calls
%% (for backwards compatibility reasons), so the user of cover will
%% have to spawn several processes to to the calls (or use
%% async_analyse_to_file/X).
%%

%% External exports
-export([start/0, start/1,
	 compile/1, compile/2, compile_module/1, compile_module/2,
	 compile_directory/0, compile_directory/1, compile_directory/2,
	 compile_beam/1, compile_beam_directory/0, compile_beam_directory/1,
	 analyse/0, analyse/1, analyse/2, analyse/3,
	 analyze/0, analyze/1, analyze/2, analyze/3,
	 analyse_to_file/0,
	 analyse_to_file/1, analyse_to_file/2, analyse_to_file/3,
	 analyze_to_file/0,
	 analyze_to_file/1, analyze_to_file/2, analyze_to_file/3,
	 async_analyse_to_file/1,async_analyse_to_file/2,
	 async_analyse_to_file/3, async_analyze_to_file/1,
	 async_analyze_to_file/2, async_analyze_to_file/3,
	 export/1, export/2, import/1,
	 modules/0, imported/0, imported_modules/0, which_nodes/0, is_compiled/1,
	 reset/1, reset/0,
	 flush/1,
	 stop/0, stop/1,
         local_only/0]).
-export([remote_start/1,get_main_node/0]).

%% Used internally to ensure we upgrade the code to the latest version.
-export([main_process_loop/1,remote_process_loop/1]).

-record(main_state, {compiled=[],           % [{Module,File}]
		     imported=[],           % [{Module,File,ImportFile}]
		     stopper,               % undefined | pid()
                     local_only=false,      % true | false
		     nodes=[],              % [Node]
		     lost_nodes=[]}).       % [Node]

-record(remote_data, {module,
                      file,
                      code,
                      mapping,
                      clauses}).

-record(remote_state, {compiled=[],         % [{Module,File}]
		       main_node}).         % atom()

-record(bump, {module   = '_',              % atom()
	       function = '_',              % atom()
	       arity    = '_',              % integer()
	       clause   = '_',              % integer()
	       line     = '_'               % integer()
	      }).
-define(BUMP_REC_NAME,bump).
-define(CHUNK_SIZE, 20000).

-define(COVER_MAPPING_TABLE, 'cover_internal_mapping_table').
-define(BINARY_TABLE, 'cover_binary_code_table').
-define(COLLECTION_TABLE, 'cover_collected_remote_data_table').

-define(TAG, cover_compiled).
-define(SERVER, cover_server).

-define(SPAWN_DBG(Tag,Value),put(Tag,Value)).
-define(STYLESHEET, "styles.css").
-define(TOOLS_APP, tools).

-include_lib("stdlib/include/ms_transform.hrl").

%%%----------------------------------------------------------------------
%%% External exports
%%%----------------------------------------------------------------------

-doc """
Starts the Cover server which owns the Cover internal database. This function is
called automatically by the other functions in the module.
""".
-spec start() -> {'ok', pid()} | {'error', Reason} when
      Reason :: {'already_started', pid()}
              | term().

start() ->
    case whereis(?SERVER) of
	undefined ->
	    Starter = self(),
	    Pid = spawn(fun() -> 
				?SPAWN_DBG(start,[]),
				init_main(Starter) 
			end),
	    Ref = erlang:monitor(process,Pid),
	    Return = 
		receive 
		    {?SERVER,started} -> 
			{ok,Pid};
		    {?SERVER,{error,Error}} -> 
			{error,Error};
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			{error,Info}
		end,
	    erlang:demonitor(Ref),
	    Return;
	Pid ->
	    {error,{already_started,Pid}}
    end.

-doc """
Starts a Cover server on the each of given nodes, and loads all cover compiled
modules. This call will fail if `cover:local_only/0` has been called.
""".
-spec start(Nodes) -> {'ok', StartedNodes}
                    | {'error', 'not_main_node'}
                    | {'error', 'local_only'} when
      Nodes :: node() | [node()],
      StartedNodes :: [node()].

start(Node) when is_atom(Node) ->
    start([Node]);
start(Nodes) ->
    call({start_nodes,remove_myself(Nodes,[])}).

-doc """
Only support running Cover on the local node. This function must be called
before any modules have been compiled or any nodes added. When running in this
mode, modules will be Cover compiled in a more efficient way, but the resulting
code will only work on the same node they were compiled on.
""".
-doc(#{since => <<"OTP 22.0">>}).
-spec local_only() -> 'ok' | {'error', 'too_late'}.

local_only() ->
    call(local_only).

-type compile_result() :: {'ok', Module :: module()}
                        | {'error', file:filename()}
                        | {'error', 'not_main_node'}.
-type mod_file() :: (Module :: module()) | (File :: file:filename()).
-type mod_files() :: mod_file() | [mod_file()].
-type option() :: {'i', Dir :: file:filename()}
                | {'d', Macro :: atom()}
                | {'d', Macro :: atom(), Value :: term()}
                | 'export_all'.

-doc(#{equiv => compile_module/2}).
-spec compile(ModFiles) -> Result | [Result] when
      ModFiles :: mod_files(),
      Result :: compile_result().

compile(ModFile) ->
    compile_module(ModFile, []).

-doc(#{equiv => compile_module/2}).
-spec compile(ModFiles, Options) -> Result | [Result] when
      ModFiles :: mod_files(),
      Options :: [option()],
      Result :: compile_result().

compile(ModFile, Options) ->
    compile_module(ModFile, Options).

-doc(#{equiv => compile_module/2}).
-spec compile_module(ModFiles) -> Result | [Result] when
      ModFiles :: mod_files(),
      Result :: compile_result().

compile_module(ModFile) when is_atom(ModFile);
			     is_list(ModFile) ->
    compile_module(ModFile, []).

-doc """
Compiles a module for Cover analysis. The module is given by its module name
`Module` or by its file name `File`. The `.erl` extension may be omitted. If the
module is located in another directory, the path has to be specified.

`Options` is a list of compiler options which defaults to `[]`. Only options
defining include file directories and macros are passed to `compile:file/2`,
everything else is ignored.

If the module is successfully Cover compiled, the function returns
`{ok, Module}`. Otherwise the function returns `{error, File}`. Errors and
warnings are printed as they occur.

If a list of `ModFiles` is given as input, a list of `Result` will be returned.
The order of the returned list is undefined.

Note that the internal database is (re-)initiated during the compilation,
meaning any previously collected coverage data for the module will be lost.
""".
-spec compile_module(ModFiles, Options) -> Result | [Result] when
      ModFiles :: mod_files(),
      Options :: [option()],
      Result :: compile_result().

compile_module(ModFile, Options) when is_atom(ModFile);
				      is_list(ModFile), is_integer(hd(ModFile)) ->
    [R] = compile_module([ModFile], Options),
    R;
compile_module(ModFiles, Options) when is_list(Options) ->
    AbsFiles =
	[begin
	     File =
		 case ModFile of
		     _ when is_atom(ModFile) -> atom_to_list(ModFile);
		     _ when is_list(ModFile) -> ModFile
		 end,
	     WithExt = case filename:extension(File) of
			   ".erl" ->
			       File;
			   _ ->
			       File++".erl"
		       end,
	     filename:absname(WithExt)
	 end || ModFile <- ModFiles],
    compile_modules(AbsFiles, Options).

-type file_error() :: 'eacces' | 'enoent'.

-doc(#{equiv => compile_directory/2}).
-spec compile_directory() -> [Result] | {'error', Reason} when
      Reason :: file_error(),
      Result :: compile_result().

compile_directory() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    compile_directory(Dir, []);
	Error ->
	    Error
    end.

-doc(#{equiv => compile_directory/2}).
-spec compile_directory(Dir) -> [Result] | {'error', Reason} when
      Dir :: file:filename(),
      Reason :: file_error(),
      Result :: compile_result().

compile_directory(Dir) when is_list(Dir) ->
    compile_directory(Dir, []).


-doc """
Compiles all modules (`.erl` files) in a directory `Dir` for Cover analysis the
same way as [`compile_module/1,2`](`compile_module/1`) and returns a list with
the return values.

`Dir` defaults to the current working directory.

The function returns `{error, eacces}` if the directory is not readable or
`{error, enoent}` if the directory does not exist.
""".
-spec compile_directory(Dir, Options) -> [Result] | {'error', Reason} when
      Dir :: file:filename(),
      Options :: [option()],
      Reason :: file_error(),
      Result :: compile_result().

compile_directory(Dir, Options) when is_list(Dir), is_list(Options) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    ErlFiles = [filename:join(Dir, File) ||
			   File <- Files,
			   filename:extension(File) =:= ".erl"],
	    compile_modules(ErlFiles, Options);
	Error ->
	    Error
    end.

compile_modules(Files,Options) ->
    Options2 = filter_options(Options),
    call({compile, Files, Options2}).

filter_options(Options) ->
    lists:filter(fun(Option) ->
                         case Option of
                             {i, Dir} when is_list(Dir) -> true;
                             {d, _Macro} -> true;
                             {d, _Macro, _Value} -> true;
                             export_all -> true;
                             tuple_calls -> true;
                             {feature,_,enable} -> true; %FIXME: To be removed.
                             {feature,_,disable} -> true; %FIXME: To be removed.
                             _ -> false
                         end
                 end,
                 Options).

-type beam_mod_file() :: (Module :: module()) | (BeamFile :: file:filename()).
-type beam_mod_files() :: beam_mod_file() | [beam_mod_file()].
-type compile_beam_rsn() ::
        'non_existing'
      | {'no_abstract_code', BeamFile :: file:filename()}
      | {'encrypted_abstract_code', BeamFile :: file:filename()}
      | {'already_cover_compiled', 'no_beam_found', module()}
      | {{'missing_backend', module()}, BeamFile :: file:filename()}
      | {'no_file_attribute', BeamFile :: file:filename()}
      | 'not_main_node'.

-type compile_beam_result() :: {'ok', module()}
                             | {'error', BeamFile :: file:filename()}
                             | {'error', Reason :: compile_beam_rsn()}.

-doc """
Does the same as [`compile/1,2`](`compile/1`), but uses an existing `.beam` file
as base, that is, the module is not compiled from source. Thus
[`compile_beam/1`](`compile_beam/1`) is faster than `compile/1,2`.

Note that the existing `.beam` file must contain _abstract code_, that is, it
must have been compiled with the [`debug_info`](`compile:file/2`) option. If
not, the error reason `{no_abstract_code, BeamFile}` is returned. If the
abstract code is encrypted, and no key is available for decrypting it, the error
reason `{encrypted_abstract_code, BeamFile}` is returned.

If only the module name (that is, not the full name of the `.beam` file) is
given to this function, the `.beam` file is found by calling
`code:which(Module)`. If no `.beam` file is found, the error reason
`non_existing` is returned. If the module is already cover compiled with
[`compile_beam/1`](`compile_beam/1`), the `.beam` file will be picked from the
same location as the first time it was compiled. If the module is already cover
compiled with [`compile/1,2`](`compile/1`), there is no way to find the correct
`.beam` file, so the error reason
`{already_cover_compiled, no_beam_found, Module}` is returned.

`{error, BeamFile}` is returned if the compiled code cannot be loaded on the
node.

If a list of `ModFiles` is given as input, a list of `Result` will be returned.
The order of the returned list is undefined.
""".
-spec compile_beam(ModFiles) -> Result | [Result] when
      ModFiles :: beam_mod_files(),
      Result :: compile_beam_result().

compile_beam(ModFile0) when is_atom(ModFile0);
			    is_list(ModFile0), is_integer(hd(ModFile0)) ->
    case compile_beams([ModFile0]) of
	[{error,{non_existing,_}}] ->
	    %% Backwards compatibility
	    {error,non_existing};
	[Result] ->
	    Result
    end;
compile_beam(ModFiles) when is_list(ModFiles) ->
    compile_beams(ModFiles).

-doc(#{equiv => compile_beam_directory/1}).
-spec compile_beam_directory() -> [Result] | {'error', Reason} when
      Reason :: file_error(),
      Result :: compile_beam_result().

compile_beam_directory() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    compile_beam_directory(Dir);
	Error ->
	    Error
    end.

-doc """
Compiles all modules (`.beam` files) in a directory `Dir` for Cover analysis the
same way as `compile_beam/1` and returns a list with the return values.

`Dir` defaults to the current working directory.

The function returns `{error, eacces}` if the directory is not readable or
`{error, enoent}` if the directory does not exist.
""".
-spec compile_beam_directory(Dir) ->
                    [Result] | {'error', Reason} when
      Dir :: file:filename(),
      Reason :: file_error(),
      Result :: compile_beam_result().

compile_beam_directory(Dir) when is_list(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    BeamFiles =  [filename:join(Dir, File) ||
			     File <- Files,
			     filename:extension(File) =:= ".beam"],
	    compile_beams(BeamFiles);
	Error ->
	    Error
    end.

compile_beams(ModFiles0) ->
    ModFiles = get_mods_and_beams(ModFiles0,[]),
    call({compile_beams,ModFiles}).

get_mods_and_beams([Module|ModFiles],Acc) when is_atom(Module) ->
    case code:which(Module) of
	non_existing ->
	    get_mods_and_beams(ModFiles,[{error,{non_existing,Module}}|Acc]);
	File ->
	    get_mods_and_beams([{Module,File}|ModFiles],Acc)
    end;
get_mods_and_beams([File|ModFiles],Acc) when is_list(File) ->
    {WithExt,WithoutExt}
	= case filename:rootname(File,".beam") of
	      File ->
		  {File++".beam",File};
	      Rootname ->
		  {File,Rootname}
	      end,
    AbsFile = filename:absname(WithExt),
    Module = list_to_atom(filename:basename(WithoutExt)),
    get_mods_and_beams([{Module,AbsFile}|ModFiles],Acc);
get_mods_and_beams([{Module,File}|ModFiles],Acc) ->
    %% Check for duplicates
    case lists:keyfind(Module,2,Acc) of
	{ok,Module,File} ->
	    %% Duplicate, but same file so ignore
	    get_mods_and_beams(ModFiles,Acc);
	{ok,Module,_OtherFile} ->
	    %% Duplicate and different file - error
	    get_mods_and_beams(ModFiles,[{error,{duplicate,Module}}|Acc]);
	_ ->
	    get_mods_and_beams(ModFiles,[{ok,Module,File}|Acc])
    end;
get_mods_and_beams([],Acc) ->
    lists:reverse(Acc).

-type analyse_item() ::
        (Line :: {M :: module(), N :: non_neg_integer()})
      | (Clause :: {M :: module(), F :: atom(), A :: arity(),
                    C :: non_neg_integer()})
      | (Function :: {M :: module(), F :: atom(), A :: arity()}). % mfa()
-type analyse_value() :: {Cov :: non_neg_integer(), NotCov :: non_neg_integer()}
                       | Calls :: non_neg_integer().
-type analyse_ok() :: [{Module :: module(), Value :: analyse_value()}]
                    | [{Item :: analyse_item(), Value :: analyse_value()}].
-type analyse_fail() :: [{'not_cover_compiled', module()}].
-type analysis() :: 'coverage' | 'calls'.
-type level() :: 'line' | 'clause' | 'function' | 'module'.
-type modules() :: module() | [module()].
-type one_result() ::
        {'ok', {Module :: module(), Value :: analyse_value()}}
      | {'ok', [{Item :: analyse_item(), Value :: analyse_value()}]}
      | {'error', {'not_cover_compiled', module()}}.

-define(is_analysis(__A__),
	(__A__=:=coverage orelse __A__=:=calls)).
-define(is_level(__L__),
	(__L__=:=line orelse __L__=:=clause orelse
	 __L__=:=function orelse __L__=:=module)).

-doc(#{equiv => analyse/3}).
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse() -> {'result', analyse_ok(), analyse_fail()} |
                   {'error', 'not_main_node'}.

analyse() ->
    analyse('_').

-dialyzer({no_contracts, analyse/1}).
%% modules() :: module() | [module()]. module() is an alias for
%% atom(), which overlaps with analysis() and level(). That is,
%% modules named 'calls' &c must be placed in a list.
-doc(#{equiv => analyse/3}).
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse(Analysis) -> {'result', analyse_ok(), analyse_fail()} |
                           {'error', 'not_main_node'} when
                  Analysis :: analysis();
             (Level) -> {'result', analyse_ok(), analyse_fail()} |
                        {'error', 'not_main_node'} when
                  Level :: level();
             (Modules) -> OneResult |
                          {'result', analyse_ok(), analyse_fail()} |
                          {'error', 'not_main_node'} when
                  Modules :: modules(),
                  OneResult :: one_result().

analyse(Analysis) when ?is_analysis(Analysis) ->
    analyse('_', Analysis);
analyse(Level) when ?is_level(Level) ->
    analyse('_', Level);
analyse(Module) ->
    analyse(Module, coverage).

-dialyzer({no_contracts,analyse/2}). %% See comment analyse/1.
-doc(#{equiv => analyse/3}).
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse(Analysis, Level) -> {'result', analyse_ok(), analyse_fail()} |
                                  {'error', 'not_main_node'} when 
                  Analysis :: analysis(),
                  Level :: level();
             (Modules, Analysis) -> OneResult |
                                    {'result', analyse_ok(), analyse_fail()} |
                                    {'error', 'not_main_node'} when
                  Analysis :: analysis(),
                  Modules :: modules(),
                  OneResult :: one_result();
             (Modules, Level) -> OneResult |
                                 {'result', analyse_ok(), analyse_fail()} |
                                 {'error', 'not_main_node'} when
                  Level :: level(),
                  Modules :: modules(),
                  OneResult :: one_result().

analyse(Analysis, Level) when ?is_analysis(Analysis) andalso
			      ?is_level(Level) ->
    analyse('_', Analysis, Level);
analyse(Module, Analysis) when ?is_analysis(Analysis) ->
    analyse(Module, Analysis, function);
analyse(Module, Level) when ?is_level(Level) ->
    analyse(Module, coverage, Level).

-doc """
Performs analysis of one or more Cover compiled modules, as specified by
`Analysis` and `Level` (see above), by examining the contents of the internal
database.

`Analysis` defaults to `coverage` and `Level` defaults to `function`.

If `Modules` is an atom (one module), the return will be `OneResult`, else the
return will be `{result, Ok, Fail}`.

If `Modules` is not given, all modules that have data in the cover data table,
are analysed. Note that this includes both cover compiled modules and imported
modules.

If a given module is not Cover compiled, this is indicated by the error reason
`{not_cover_compiled, Module}`.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse(Modules, Analysis, Level) ->
                     OneResult |
                     {'result', analyse_ok(), analyse_fail()} |
                     {'error', 'not_main_node'} when
      Analysis :: analysis(),
      Level :: level(),
      Modules :: modules(),
      OneResult :: one_result().

analyse(Module, Analysis, Level) when ?is_analysis(Analysis),
				      ?is_level(Level) ->
    call({{analyse, Analysis, Level}, Module}).

-doc false.
analyze() -> analyse( ).
-doc false.
analyze(Module) -> analyse(Module).
-doc false.
analyze(Module, Analysis) -> analyse(Module, Analysis).
-doc false.
analyze(Module, Analysis, Level) -> analyse(Module, Analysis, Level).

%% Kept for backwards compatibility:
%% analyse_to_file(Modules, OutFile) ->
%% analyse_to_file(Modules, OutFile, Options) -> {ok,OutFile} | {error,Error}

-doc(#{equiv => analyse_to_file/2}).
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse_to_file() -> {'result', analyse_file_ok(), analyse_file_fail()} |
                           {'error', 'not_main_node'}.

analyse_to_file() ->
    analyse_to_file('_').

-type analyse_option() :: 'html'
                        | {'outfile', OutFile :: file:filename()}
                        | {'outdir', OutDir :: file:filename()}.
-type analyse_answer() :: {'ok', OutFile :: file:filename()} |
                          {'error', analyse_rsn()}.
-type analyse_file_ok() :: [OutFile :: file:filename()].
-type analyse_file_fail() :: [analyse_rsn()].
-type analyse_rsn() :: {'not_cover_compiled', Module :: module()} |
                       {'file', File :: file:filename(), Reason :: term()} |
                       {'no_source_code_found', Module :: module()}.

-dialyzer({no_contracts, analyse_to_file/1}).
%% The option list [html] overlaps with module list [html].
-doc(#{equiv => analyse_to_file/2}).
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse_to_file(Modules) -> Answer |
                                  {'result',
                                   analyse_file_ok(), analyse_file_fail()} |
                                  {'error', 'not_main_node'} when
                          Modules :: modules(),
                          Answer :: analyse_answer();
                     (Options) -> {'result',
                                   analyse_file_ok(), analyse_file_fail()} |
                                  {'error', 'not_main_node'} when
                          Options :: [analyse_option()].

analyse_to_file(Arg) ->
    case is_options(Arg) of
	true ->
	    analyse_to_file('_',Arg);
	false ->
	    analyse_to_file(Arg,[])
    end.

-doc """
Makes copies of the source file for the given modules, where it for each
executable line is specified how many times it has been executed.

The output file `OutFile` defaults to `Module.COVER.out`, or `Module.COVER.html`
if the option `html` was used.

If `Modules` is an atom (one module), the return will be `Answer`, else the
return will be a list, `{result, Ok, Fail}`.

If `Modules` is not given, all modules that have da ta in the cover data table,
are analysed. Note that this includes both cover compiled modules and imported
modules.

If a module is not Cover compiled, this is indicated by the error reason
`{not_cover_compiled, Module}`.

If the source file and/or the output file cannot be opened using `file:open/2`,
the function returns `{error, {file, File, Reason}}` where `File` is the file
name and `Reason` is the error reason.

If a module was cover compiled from the `.beam` file, that is, using
`compile_beam/1` or
[`compile_beam_directory/0,1` ](`compile_beam_directory/0`),it is assumed that
the source code can be found in the same directory as the `.beam` file, in
`../src` relative to that directory, or using the source path in
`Module:module_info(compile)`. When using the latter, two paths are examined:
first the one constructed by joining `../src` and the tail of the compiled path
below a trailing `src` component, then the compiled path itself. If no source
code is found, this is indicated by the error reason
`{no_source_code_found, Module}`.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec analyse_to_file(Modules, Options) ->
                             Answer |
                             {'result',
                              analyse_file_ok(), analyse_file_fail()} |
                             {'error', 'not_main_node'} when
      Modules :: modules(),
      Options :: [analyse_option()],
      Answer :: analyse_answer().

analyse_to_file(Module, OutFile) when is_list(OutFile), is_integer(hd(OutFile)) ->
    %% Kept for backwards compatibility
    analyse_to_file(Module, [{outfile,OutFile}]);
analyse_to_file(Module, Options) when is_list(Options) ->
    call({{analyse_to_file, Options}, Module}).

-doc false.
analyse_to_file(Module, OutFile, Options) when is_list(OutFile) ->
    %% Kept for backwards compatibility
    analyse_to_file(Module,[{outfile,OutFile}|Options]).

-doc false.
analyze_to_file() -> analyse_to_file().
-doc false.
analyze_to_file(Module) -> analyse_to_file(Module).
-doc false.
analyze_to_file(Module, OptOrOut) -> analyse_to_file(Module, OptOrOut).
-doc false.
analyze_to_file(Module, OutFile, Options) -> 
    analyse_to_file(Module, OutFile, Options).

-doc(#{equiv => async_analyse_to_file/3}).
-doc(#{since => <<"OTP R14B02">>}).
-spec async_analyse_to_file(Module) -> pid() when
      Module :: module().

async_analyse_to_file(Module) ->
    do_spawn(?MODULE, analyse_to_file, [Module]).

-dialyzer({no_contracts, async_analyse_to_file/2}).
%% The types file:filename() (string()) and ['html'] has something in
%% common, namely [].
-doc(#{equiv => async_analyse_to_file/3}).
-doc(#{since => <<"OTP R14B02">>}).
-spec async_analyse_to_file(Module, OutFile) -> pid() when
                                Module :: module(),
                                OutFile :: file:filename();
                           (Module, Options) -> pid() when
                                Module :: module(),
                                Options :: [Option],
                                Option :: 'html'.

async_analyse_to_file(Module, OutFileOrOpts) ->
    do_spawn(?MODULE, analyse_to_file, [Module, OutFileOrOpts]).

-doc """
This function works exactly the same way as
[`analyse_to_file`](`analyse_to_file/1`) except that it is asynchronous instead
of synchronous. The spawned process will link with the caller when created. If
an error of type `analyse_rsn()` occurs while doing the cover analysis the
process will crash with the same error reason as
[`analyse_to_file`](`analyse_to_file/1`) would return.
""".
-doc(#{since => <<"OTP R14B02">>}).
-spec async_analyse_to_file(Module, OutFile, Options) -> pid() when
      Module :: module(),
      OutFile :: file:filename(),
      Options :: [Option],
      Option :: 'html'.

async_analyse_to_file(Module, OutFile, Options) ->
    do_spawn(?MODULE, analyse_to_file, [Module, OutFile, Options]).

is_options([html]) ->
    true; % this is not 100% safe - could be a module named html...
is_options([html|Opts]) ->
    is_options(Opts);
is_options([{Opt,_}|_]) when Opt==outfile; Opt==outdir ->
    true;
is_options(_) ->
    false.

do_spawn(M,F,A) ->
    spawn_link(fun() ->
		  case apply(M,F,A) of
		      {ok, _} ->
			  ok;
		      {error, Reason} ->
			  exit(Reason)
		  end
	  end).

-doc false.
async_analyze_to_file(Module) ->
    async_analyse_to_file(Module).
-doc false.
async_analyze_to_file(Module, OutFileOrOpts) ->
    async_analyse_to_file(Module, OutFileOrOpts).
-doc false.
async_analyze_to_file(Module, OutFile, Options) ->
    async_analyse_to_file(Module, OutFile, Options).

outfilename(undefined, Module, HTML) ->
    outfilename(Module, HTML);
outfilename(OutDir, Module, HTML) ->
    filename:join(OutDir, outfilename(Module, HTML)).

outfilename(Module, true) ->
    atom_to_list(Module)++".COVER.html";
outfilename(Module, false) ->
    atom_to_list(Module)++".COVER.out".

-type export_reason() :: {'not_cover_compiled', Module :: module()} |
                         {'cant_open_file',
                          ExportFile :: file:filename(), FileReason :: term()} |
                         'not_main_node'.

-doc(#{equiv => export/2}).
-spec export(File) -> 'ok' | {'error', Reason} when
      File :: file:filename(),
      Reason :: export_reason().

export(File) ->
    export(File, '_').

-doc """
Exports the current coverage data for `Module` to the file `ExportFile`. It is
recommended to name the `ExportFile` with the extension `.coverdata`, since
other filenames cannot be read by the web based interface to cover.

If `Module` is not given, data for all Cover compiled or earlier imported
modules is exported.

This function is useful if coverage data from different systems is to be merged.

See also `import/1`.
""".
-spec export(File, Module) -> 'ok' | {'error', Reason} when
      File :: file:filename(),
      Module :: module(),
      Reason :: export_reason().

export(File, Module) ->
    call({export,File,Module}).

-doc """
Imports coverage data from the file `ExportFile` created with
[`export/1,2`](`export/1`). Any analysis performed after this will include the
imported data.

Note that when compiling a module _all existing coverage data is removed_,
including imported data. If a module is already compiled when data is imported,
the imported data is _added_ to the existing coverage data.

Coverage data from several export files can be imported into one system. The
coverage data is then added up when analysing.

Coverage data for a module cannot be imported from the same file twice unless
the module is first reset or compiled. The check is based on the filename, so
you can easily fool the system by renaming your export file.

See also [`export/1,2`](`export/1`).
""".
-spec import(ExportFile) -> 'ok' | {'error', Reason} when
      ExportFile :: file:filename(),
      Reason :: {'cant_open_file', ExportFile, FileReason :: term()} |
                'not_main_node'.

import(File) ->
    call({import,File}).

-doc "Returns a list with all modules that are currently Cover compiled.".
-spec modules() -> [module()] | {'error', 'not_main_node'}.

modules() ->
   call(modules).

-doc "Returns a list with all modules for which there are imported data.".
-spec imported_modules() -> [module()] | {'error', 'not_main_node'}.

imported_modules() ->
   call(imported_modules).

-doc "Returns a list with all imported files.".
-spec imported() -> [file:filename()] |  {'error', 'not_main_node'}.

imported() ->
   call(imported).

-doc """
Returns a list with all nodes that are part of the coverage analysis. Note that
the current node is not returned. This node is always part of the analysis.
""".
-spec which_nodes() -> [node()].

which_nodes() ->
   call(which_nodes).

-doc """
Returns `{file, File}` if the module `Module` is Cover compiled, or `false`
otherwise. `File` is the `.erl` file used by
[`compile_module/1,2`](`compile_module/1`) or the `.beam` file used by
`compile_beam/1`.
""".
-spec is_compiled(Module) -> {'file', File :: file:filename()} |
                             'false' |
                             {'error', 'not_main_node'} when
      Module :: module().

is_compiled(Module) when is_atom(Module) ->
    call({is_compiled, Module}).

-doc """
Resets all coverage data for a Cover compiled module `Module` in the Cover
database on all nodes. If the argument is omitted, the coverage data will be
reset for all modules known by Cover.

If `Module` is not Cover compiled, the function returns
`{error, {not_cover_compiled, Module}}`.
""".
-spec reset(Module) -> 'ok' |
                       {'error', 'not_main_node'} |
                       {'error', {'not_cover_compiled', Module}} when
      Module :: module().

reset(Module) when is_atom(Module) ->
    call({reset, Module}).

-doc(#{equiv => reset/1}).
-spec reset() -> 'ok' | {'error', 'not_main_node'}.

reset() ->
    call(reset).

-doc "Stops the Cover server and unloads all Cover compiled code.".
-spec stop() -> 'ok' | {'error', 'not_main_node'}.

stop() ->
    call(stop).

-doc """
Stops the Cover server and unloads all Cover compiled code on the given nodes.
Data stored in the Cover database on the remote nodes is fetched and stored on
the main node.
""".
-spec stop(Nodes) -> 'ok' | {'error', 'not_main_node'} when
      Nodes :: node() | [node()].

stop(Node) when is_atom(Node) ->
    stop([Node]);
stop(Nodes) ->
    call({stop,remove_myself(Nodes,[])}).

-doc """
Fetch data from the Cover database on the remote nodes and stored on the main
node.
""".
-doc(#{since => <<"OTP R16B">>}).
-spec flush(Nodes) -> 'ok' | {'error', 'not_main_node'} when
      Nodes :: node() | [node()].

flush(Node) when is_atom(Node) ->
    flush([Node]);
flush(Nodes) ->
    call({flush,remove_myself(Nodes,[])}).

%% Used by test_server only. Not documented.
-doc false.
get_main_node() ->
    call(get_main_node).

call(Request) ->
    Ref = erlang:monitor(process,?SERVER),
    receive {'DOWN', Ref, _Type, _Object, noproc} -> 
	    erlang:demonitor(Ref),
            {ok,_} = start(),
	    call(Request)
    after 0 ->
	    ?SERVER ! {self(),Request},
	    Return = 
		receive 
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			exit(Info);
		    {?SERVER,Reply} -> 
			Reply
		end,
	    erlang:demonitor(Ref, [flush]),
	    Return
    end.

reply(From, Reply) ->
    From ! {?SERVER,Reply},
    ok.

is_from(From) ->
    is_pid(From).

remote_call(Node,Request) ->
    Ref = erlang:monitor(process,{?SERVER,Node}),
    receive {'DOWN', Ref, _Type, _Object, noproc} -> 
	    erlang:demonitor(Ref),
	    {error,node_dead}
    after 0 ->
	    {?SERVER,Node} ! Request,
	    Return = 
		receive 
		    {'DOWN', Ref, _Type, _Object, _Info} -> 
			case Request of
			    {remote,stop} -> ok;
			    _ -> {error,node_dead}
			end;
		    {?SERVER,Reply} -> 
			Reply
		end,
	    erlang:demonitor(Ref, [flush]),
	    Return
    end.
    
remote_reply(Proc,Reply) when is_pid(Proc) ->
    Proc ! {?SERVER,Reply},
    ok;
remote_reply(MainNode,Reply) ->
    {?SERVER,MainNode} ! {?SERVER,Reply},
    ok.

%%%----------------------------------------------------------------------
%%% cover_server on main node
%%%----------------------------------------------------------------------

init_main(Starter) ->
    try register(?SERVER,self()) of
        true ->
            ?COVER_MAPPING_TABLE = ets:new(?COVER_MAPPING_TABLE,
                                           [ordered_set, public, named_table]),
            ?BINARY_TABLE = ets:new(?BINARY_TABLE, [set, public, named_table]),
            ?COLLECTION_TABLE = ets:new(?COLLECTION_TABLE, [set, public,
                                                            named_table]),
            ok = net_kernel:monitor_nodes(true),
            Starter ! {?SERVER,started},
            main_process_loop(#main_state{})
    catch
        error:badarg ->
            %% The server's already registered; either report that it's already
            %% started or try again if it died before we could find its pid.
            case whereis(?SERVER) of
                undefined ->
                    init_main(Starter);
                Pid ->
                    Starter ! {?SERVER, {error, {already_started, Pid}}}
            end
    end.

-doc false.
main_process_loop(State) ->
    receive
        {From, local_only} ->
            case State of
                #main_state{compiled=[],nodes=[]} ->
                    reply(From, ok),
                    main_process_loop(State#main_state{local_only=true});
                #main_state{} ->
                    reply(From, {error,too_late}),
                    main_process_loop(State)
            end;

	{From, {start_nodes,Nodes}} ->
            case State#main_state.local_only of
                false ->
                    {StartedNodes,State1} = do_start_nodes(Nodes, State),
                    reply(From, {ok,StartedNodes}),
                    main_process_loop(State1);
                true ->
                    reply(From, {error,local_only}),
                    main_process_loop(State)
            end;

	{From, {compile, Files, Options}} ->
	    {R,S} = do_compile(Files, Options, State),
	    reply(From,R),
	    %% This module (cover) could have been reloaded. Make
	    %% sure we run the new code.
	    ?MODULE:main_process_loop(S);

	{From, {compile_beams, ModsAndFiles}} ->
	    {R,S} = do_compile_beams(ModsAndFiles,State),
	    reply(From,R),
	    %% This module (cover) could have been reloaded. Make
	    %% sure we run the new code.
	    ?MODULE:main_process_loop(S);

	{From, {export,OutFile,Module}} ->
	    spawn(fun() ->
			  ?SPAWN_DBG(export,{OutFile, Module}),
			  do_export(Module, OutFile, From, State)
		  end),
	    main_process_loop(State);
	
	{From, {import,File}} ->
	    case file:open(File,[read,binary,raw]) of
		{ok,Fd} ->
		    Imported = do_import_to_table(Fd,File,
						  State#main_state.imported),
		    reply(From, ok),
		    ok = file:close(Fd),
		    main_process_loop(State#main_state{imported=Imported});
		{error,Reason} ->
		    reply(From, {error, {cant_open_file,File,Reason}}),
		    main_process_loop(State)
	    end;

	{From, modules} ->
	    %% Get all compiled modules which are still loaded
	    {LoadedModules,Compiled} = 
		get_compiled_still_loaded(State#main_state.nodes,
					  State#main_state.compiled),
	    
	    reply(From, LoadedModules),
	    main_process_loop(State#main_state{compiled=Compiled});

	{From, imported_modules} ->
	    %% Get all modules with imported data
	    ImportedModules = lists:map(fun({Mod,_File,_ImportFile}) -> Mod end,
					State#main_state.imported),
	    reply(From, ImportedModules),
	    main_process_loop(State);

	{From, imported} ->
	    %% List all imported files
	    reply(From, get_all_importfiles(State#main_state.imported,[])),
	    main_process_loop(State);

	{From, which_nodes} ->
	    %% List all imported files
	    reply(From, State#main_state.nodes),
	    main_process_loop(State);

	{From, reset} ->
	    lists:foreach(
	      fun({Module,_File}) -> 
		      do_reset_main_node(Module,State#main_state.nodes)
	      end, 
	      State#main_state.compiled),
	    reply(From, ok),
	    main_process_loop(State#main_state{imported=[]});

	{From, {stop,Nodes}} ->
	    remote_collect('_',Nodes,true),
	    reply(From, ok),
	    Nodes1 = State#main_state.nodes--Nodes,
	    LostNodes1 = State#main_state.lost_nodes--Nodes,
	    main_process_loop(State#main_state{nodes=Nodes1,
					       lost_nodes=LostNodes1});

	{From, {flush,Nodes}} ->
	    remote_collect('_',Nodes,false),
	    reply(From, ok),
	    main_process_loop(State);

	{From, stop} ->
	    lists:foreach(
	      fun(Node) -> 
		      remote_call(Node,{remote,stop})
	      end,
	      State#main_state.nodes),
	    reload_originals(State#main_state.compiled),
            ets:delete(?COVER_MAPPING_TABLE),
            ets:delete(?BINARY_TABLE),
            ets:delete(?COLLECTION_TABLE),
            delete_all_counters(),
            unregister(?SERVER),
	    reply(From, ok);

	{From, {{analyse, Analysis, Level}, '_'}} ->
	    R = analyse_all(Analysis, Level, State),
	    reply(From, R),
	    main_process_loop(State);

	{From, {{analyse, Analysis, Level}, Modules}} when is_list(Modules) ->
	    R = analyse_list(Modules, Analysis, Level, State),
	    reply(From, R),
	    main_process_loop(State);

	{From, {{analyse, Analysis, Level}, Module}} ->
	    S = try 
		    Loaded = is_loaded(Module, State),
		    spawn(fun() ->
				  ?SPAWN_DBG(analyse,{Module,Analysis, Level}),
				  do_parallel_analysis(
				    Module, Analysis, Level, 
				    Loaded, From, State)
			  end),
		    State
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {{analyse_to_file, Opts},'_'}} ->
	    R = analyse_all_to_file(Opts, State),
	    reply(From,R),
	    main_process_loop(State);

	{From, {{analyse_to_file, Opts},Modules}} when is_list(Modules) ->
	    R = analyse_list_to_file(Modules, Opts, State),
	    reply(From,R),
	    main_process_loop(State);

	{From, {{analyse_to_file, Opts},Module}} ->
	    S = try 
		    Loaded = is_loaded(Module, State),
		    spawn_link(fun() ->
				  ?SPAWN_DBG(analyse_to_file,{Module,Opts}),
				  do_parallel_analysis_to_file(
				    Module, Opts, Loaded, From, State)
			  end),
		    State
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {is_compiled, Module}} ->
	    S = try is_loaded(Module, State) of
		    {loaded, File} ->
			reply(From,{file, File}),
			State;
		    {imported,_File,_ImportFiles} ->
			reply(From,false),
			State
		catch throw:Reason ->
			reply(From,false),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {reset, Module}} ->
	    S = try 
		    Loaded = is_loaded(Module,State),
		    R = case Loaded of
			    {loaded, _File} ->
				do_reset_main_node(
				  Module, State#main_state.nodes);
			    {imported, _File, _} ->
				do_reset_collection_table(Module)
			end,
		    Imported = 
			remove_imported(Module,
					State#main_state.imported),
		    reply(From, R),
		    State#main_state{imported=Imported}
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);		    
	
	{'DOWN', _MRef, process, {?SERVER,Node}, _Info} ->
	    %% A remote cover_server is down, mark as lost
	    {Nodes,Lost} =
		case lists:member(Node,State#main_state.nodes) of
		    true ->
			N = State#main_state.nodes--[Node],
			L = [Node|State#main_state.lost_nodes],
			{N,L};
		    false -> % node stopped
			{State#main_state.nodes,State#main_state.lost_nodes}
		end,
	    main_process_loop(State#main_state{nodes=Nodes,lost_nodes=Lost});

	{nodeup,Node} ->
	    State1 =
		case lists:member(Node,State#main_state.lost_nodes) of
		    true ->
			sync_compiled(Node,State);
		    false ->
			State
	    end,
	    main_process_loop(State1);

	{nodedown,_} ->
	    %% Will be taken care of when 'DOWN' message arrives
	    main_process_loop(State);
	
	{From, get_main_node} ->
	    reply(From, node()),
	    main_process_loop(State);

	get_status ->
	    io:format("~tp~n",[State]),
	    main_process_loop(State)
    end.

%%%----------------------------------------------------------------------
%%% cover_server on remote node
%%%----------------------------------------------------------------------

init_remote(Starter,MainNode) ->
    register(?SERVER,self()),
    ?COVER_MAPPING_TABLE = ets:new(?COVER_MAPPING_TABLE,
                                   [ordered_set, public, named_table]),
    Starter ! {self(),started},
    remote_process_loop(#remote_state{main_node=MainNode}).



-doc false.
remote_process_loop(State) ->
    receive 
	{remote,load_compiled,Compiled} ->
	    Compiled1 = load_compiled(Compiled,State#remote_state.compiled),
	    remote_reply(State#remote_state.main_node, ok),
	    ?MODULE:remote_process_loop(State#remote_state{compiled=Compiled1});

	{remote,unload,UnloadedModules} ->
	    unload(UnloadedModules),
	    Compiled = 
		update_compiled(UnloadedModules, State#remote_state.compiled),
	    remote_reply(State#remote_state.main_node, ok),
	    remote_process_loop(State#remote_state{compiled=Compiled});

	{remote,reset,Module} ->
	    reset_counters(Module),
	    remote_reply(State#remote_state.main_node, ok),
	    remote_process_loop(State);

	{remote,collect,Module,CollectorPid} ->
	    self() ! {remote,collect,Module,CollectorPid, ?SERVER};

	{remote,collect,Modules0,CollectorPid,From} ->
	    Modules = case Modules0 of
			  '_' -> [M || {M,_} <- State#remote_state.compiled];
			  _ -> Modules0
		      end,
            spawn(fun() ->
                          ?SPAWN_DBG(remote_collect, 
                                     {Modules, CollectorPid, From}),
                          do_collect(Modules, CollectorPid, From)
                  end),
	    remote_process_loop(State);

	{remote,stop} ->
	    reload_originals(State#remote_state.compiled),
	    ets:delete(?COVER_MAPPING_TABLE),
            delete_all_counters(),
            unregister(?SERVER),
	    ok; % not replying since 'DOWN' message will be received anyway

	{remote,get_compiled} ->
	    remote_reply(State#remote_state.main_node,
			 State#remote_state.compiled),
	    remote_process_loop(State);

	{From, get_main_node} ->
	    remote_reply(From, State#remote_state.main_node),
	    remote_process_loop(State);

	get_status ->
	    io:format("~tp~n",[State]),
	    remote_process_loop(State);

	M ->
	    io:format("WARNING: remote cover_server received\n~p\n",[M]),
	    case M of
		{From,_} ->
		    case is_from(From) of
			true ->
			    reply(From,{error,not_main_node});
		        false ->
			    ok
		    end;
		_ ->
		    ok
	    end,
	    remote_process_loop(State)
	    
    end.

do_collect(Modules, CollectorPid, From) ->
    _ = pmap(fun(Module) ->
                     send_counters(Module, CollectorPid)
             end, Modules),
    CollectorPid ! done,
    remote_reply(From, ok).

send_chunk(CollectorPid,Chunk) ->
    CollectorPid ! {chunk,Chunk,self()},
    receive continue -> ok end.

get_downs([]) ->
    ok;
get_downs(Mons) ->
    receive
	{'DOWN', Ref, _Type, Pid, _Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    get_downs(lists:delete({Pid,Ref},Mons));
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    get_downs(Mons)
	    end
    end.

reload_originals(Compiled) ->
    _ = pmap(fun do_reload_original/1, [M || {M,_} <- Compiled]),
    ok.

do_reload_original(Module) ->
    case code:which(Module) of
	?TAG ->
	    _ = code:purge(Module),     % remove code marked as 'old'
	    _ = code:delete(Module),    % mark cover compiled code as 'old'
	    %% Note: original beam code must be loaded before the cover
	    %% compiled code is purged, in order to for references to
	    %% 'fun M:F/A' and %% 'fun F/A' funs to be correct (they
	    %% refer to (M:)F/A in the *latest* version  of the module)
            _ = code:load_file(Module), % load original code
	    _ = code:purge(Module);     % remove cover compiled code
	_ ->
	    ignore
    end.

load_compiled([Data|Compiled],Acc) ->
    %% Make sure the #bump{} records and counters are available *before*
    %% compiling and loading the code.
    #remote_data{module=Module,file=File,code=Beam,
                 mapping=InitialMapping} = Data,
    ets:insert(?COVER_MAPPING_TABLE, InitialMapping),
    maybe_create_counters(Module, true),

    Sticky = case code:is_sticky(Module) of
                 true ->
                     code:unstick_mod(Module),
                     true;
                 false ->
                     false
             end,
    NewAcc = case code:load_binary(Module, ?TAG, Beam) of
                 {module,Module} ->
                     add_compiled(Module, File, Acc);
                 _  ->
                     do_clear(Module),
                     Acc
             end,
    case Sticky of
        true -> code:stick_mod(Module);
        false -> ok
    end,
    load_compiled(Compiled,NewAcc);
load_compiled([],Acc) ->
    Acc.

unload([Module|Modules]) ->
    do_clear(Module),
    do_reload_original(Module),
    unload(Modules);
unload([]) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%--Handling of remote nodes--------------------------------------------

do_start_nodes(Nodes, State) ->
    ThisNode = node(),
    StartedNodes =
	lists:foldl(
	  fun(Node,Acc) ->
		  case rpc:call(Node,cover,remote_start,[ThisNode]) of
		      {ok,_RPid} ->
			  erlang:monitor(process,{?SERVER,Node}),
			  [Node|Acc];
		      Error ->
			  io:format("Could not start cover on ~w: ~tp\n",
				    [Node,Error]),
			  Acc
		  end
	  end,
	  [],
	  Nodes),

    %% In case some of the compiled modules have been unloaded they
    %% should not be loaded on the new node.
    {_LoadedModules,Compiled} =
	get_compiled_still_loaded(State#main_state.nodes,
				  State#main_state.compiled),
    remote_load_compiled(StartedNodes, Compiled, State),

    State1 =
	State#main_state{nodes = State#main_state.nodes ++ StartedNodes,
			 compiled = Compiled},
    {StartedNodes, State1}.

%% start the cover_server on a remote node
-doc false.
remote_start(MainNode) ->
    case whereis(?SERVER) of
	undefined ->
	    Starter = self(),
	    Pid = spawn(fun() -> 
				?SPAWN_DBG(remote_start,{MainNode}),
				init_remote(Starter,MainNode) 
			end),
	    Ref = erlang:monitor(process,Pid),
	    Return = 
		receive 
		    {Pid,started} -> 
			{ok,Pid};
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			{error,Info}
		end,
	    erlang:demonitor(Ref),
	    Return;
	Pid ->
	    {error,{already_started,Pid}}
    end.

%% If a lost node comes back, ensure that main and remote node has the
%% same cover compiled modules. Note that no action is taken if the
%% same {Mod,File} eksists on both, i.e. code change is not handled!
sync_compiled(Node,State) ->
    #main_state{compiled=Compiled0,nodes=Nodes,lost_nodes=Lost}=State,
    State1 =
	case remote_call(Node,{remote,get_compiled}) of
	    {error,node_dead} ->
		{_,S} = do_start_nodes([Node],State),
		S;
	    {error,_} ->
		State;
	    RemoteCompiled ->
		{_,Compiled} =  get_compiled_still_loaded(Nodes,Compiled0),
		Unload = [UM || {UM,_}=U <- RemoteCompiled,
			       false == lists:member(U,Compiled)],
		remote_unload([Node],Unload),
		Load = [L || L <- Compiled,
			     false == lists:member(L,RemoteCompiled)],
		remote_load_compiled([Node], Load, State),
		State#main_state{compiled=Compiled, nodes=[Node|Nodes]}
	end,
    State1#main_state{lost_nodes=Lost--[Node]}.

%% Load a set of cover compiled modules on remote nodes,
%% We do it ?MAX_MODS modules at a time so that we don't
%% run out of memory on the cover_server node. 
-define(MAX_MODS, 10).
remote_load_compiled(Nodes, Compiled, #main_state{local_only=LocalOnly}) ->
    case LocalOnly of
        true ->
            ok;
        false ->
            remote_load_compiled(Nodes, Compiled, [], 0)
    end.

remote_load_compiled(_Nodes, [], [], _ModNum) ->
    ok;
remote_load_compiled(Nodes, Compiled, Acc, ModNum) 
  when Compiled == []; ModNum == ?MAX_MODS ->
    RemoteLoadData = get_downs_r(Acc),
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,load_compiled,RemoteLoadData})
      end,
      Nodes),
    remote_load_compiled(Nodes, Compiled, [], 0);
remote_load_compiled(Nodes, [MF | Rest], Acc, ModNum) ->
    remote_load_compiled(
      Nodes, Rest,
      [spawn_job_r(fun() -> get_data_for_remote_loading(MF) end) | Acc],
      ModNum + 1).

spawn_job_r(Fun) ->
    spawn_monitor(fun() -> exit(Fun()) end).

get_downs_r([]) ->
    [];
get_downs_r(Mons) ->
    receive
	{'DOWN', Ref, _Type, Pid, #remote_data{}=R} ->
	    [R|get_downs_r(lists:delete({Pid,Ref},Mons))];
	{'DOWN', Ref, _Type, Pid, Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    %% Something went really wrong - don't hang!
		    exit(Reason);
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    get_downs_r(Mons)
	    end
    end.


%% Read all data needed for loading a cover compiled module on a remote node
%% Binary is the beam code for the module and InitialTable is the initial
%% data to insert in ?COVER_TABLE.
get_data_for_remote_loading({Module,File}) ->
    [{Module,Code}] = ets:lookup(?BINARY_TABLE, Module),
    %%! The InitialTable list will be long if the module is big - what to do??
    Mapping = counters_mapping_table(Module),

    #remote_data{module=Module,file=File,code=Code,
                 mapping=Mapping,clauses=[]}.

%% Unload modules on remote nodes
remote_unload(Nodes,UnloadedModules) ->
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,unload,UnloadedModules})
      end,
      Nodes).    

%% Reset one or all modules on remote nodes
remote_reset(Module,Nodes) ->
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,reset,Module})
      end,
      Nodes).        

%% Collect data from remote nodes - used for analyse or stop(Node)
remote_collect(Modules,Nodes,Stop) ->
    _ = pmap(
          fun(Node) -> 
                  ?SPAWN_DBG(remote_collect, 
                             {Modules, Nodes, Stop}),
                  do_collection(Node, Modules, Stop)
          end, Nodes),
    ok.

do_collection(Node, Module, Stop) ->
    CollectorPid = spawn(fun collector_proc/0),
    case remote_call(Node,{remote,collect,Module,CollectorPid, self()}) of
	{error,node_dead} ->
	    CollectorPid ! done,
	    ok;
	ok when Stop ->
	    remote_call(Node,{remote,stop});
	ok ->
	    ok
    end.

%% Process which receives chunks of data from remote nodes - either when
%% analysing or when stopping cover on the remote nodes.
collector_proc() ->
    ?SPAWN_DBG(collector_proc, []),
    receive 
	{chunk,Chunk,From} ->
	    insert_in_collection_table(Chunk),
	    From ! continue,
	    collector_proc();
	done ->
	    ok
    end.

insert_in_collection_table([{Key,Val}|Chunk]) ->
    insert_in_collection_table(Key,Val),
    insert_in_collection_table(Chunk);
insert_in_collection_table([]) ->
    ok.

insert_in_collection_table(Key,Val) ->
    case ets:member(?COLLECTION_TABLE,Key) of
	true ->
	    _ = ets:update_counter(?COLLECTION_TABLE, Key,Val),
            ok;
	false ->
	    %% Make sure that there are no race conditions from ets:member
	    case ets:insert_new(?COLLECTION_TABLE,{Key,Val}) of
		false ->
		    insert_in_collection_table(Key,Val);
		_ ->
		    ok
	    end
    end.


remove_myself([Node|Nodes],Acc) when Node=:=node() ->
    remove_myself(Nodes,Acc);
remove_myself([Node|Nodes],Acc) ->
    remove_myself(Nodes,[Node|Acc]);
remove_myself([],Acc) ->
    Acc.

%%%--Handling of modules state data--------------------------------------

analyse_info(_Module,[]) ->
    ok;
analyse_info(Module,Imported) ->
    imported_info("Analysis",Module,Imported).

export_info(_Module,[]) ->
    ok;
export_info(_Module,_Imported) ->
    %% Do not print that the export includes imported modules
    ok.

export_info([]) ->
    ok;
export_info(_Imported) ->
    %% Do not print that the export includes imported modules
    ok.

get_all_importfiles([{_M,_F,ImportFiles}|Imported],Acc) ->
    NewAcc = do_get_all_importfiles(ImportFiles,Acc),
    get_all_importfiles(Imported,NewAcc);
get_all_importfiles([],Acc) ->
    Acc.

do_get_all_importfiles([ImportFile|ImportFiles],Acc) ->
    case lists:member(ImportFile,Acc) of
	true ->
	    do_get_all_importfiles(ImportFiles,Acc);
	false ->
	    do_get_all_importfiles(ImportFiles,[ImportFile|Acc])
    end;
do_get_all_importfiles([],Acc) ->
    Acc.

imported_info(Text,Module,Imported) ->
    case lists:keysearch(Module,1,Imported) of
	{value,{Module,_File,ImportFiles}} ->
	    io:format("~ts includes data from imported files\n~tp\n",
		      [Text,ImportFiles]);
	false ->
	    ok
    end.
    
    

add_imported(Module, File, ImportFile, Imported) ->
    add_imported(Module, File, filename:absname(ImportFile), Imported, []).

add_imported(M, F1, ImportFile, [{M,_F2,ImportFiles}|Imported], Acc) ->
    case lists:member(ImportFile,ImportFiles) of
	true ->
	    io:fwrite("WARNING: Module ~w already imported from ~tp~n"
		      "Not importing again!~n",[M,ImportFile]),
	    dont_import;
	false ->
	    NewEntry = {M, F1, [ImportFile | ImportFiles]},
	    {ok, lists:reverse([NewEntry | Acc]) ++ Imported}
    end;
add_imported(M, F, ImportFile, [H|Imported], Acc) ->
    add_imported(M, F, ImportFile, Imported, [H|Acc]);
add_imported(M, F, ImportFile, [], Acc) ->
    {ok, lists:reverse([{M, F, [ImportFile]} | Acc])}.
    
%% Removes a module from the list of imported modules and writes a warning
%% This is done when a module is compiled.
remove_imported(Module,Imported) ->
    case lists:keysearch(Module,1,Imported) of
	{value,{Module,_,ImportFiles}} ->
	    io:fwrite("WARNING: Deleting data for module ~w imported from~n"
		      "~tp~n",[Module,ImportFiles]),
	    lists:keydelete(Module,1,Imported);
	false ->
	    Imported
    end.

%% Adds information to the list of compiled modules, preserving time order
%% and without adding duplicate entries.
add_compiled(Module, File1, [{Module,_File2}|Compiled]) ->
    [{Module,File1}|Compiled];
add_compiled(Module, File, [H|Compiled]) ->
    [H|add_compiled(Module, File, Compiled)];
add_compiled(Module, File, []) ->
    [{Module,File}].

are_loaded([Module|Modules], State, Loaded, Imported, Error) ->
    try is_loaded(Module,State) of
	{loaded,File} ->
	    are_loaded(Modules, State, [{Module,File}|Loaded], Imported, Error);
	{imported,File,_} ->
	    are_loaded(Modules, State, Loaded, [{Module,File}|Imported], Error)
    catch throw:_ ->
	    are_loaded(Modules, State, Loaded, Imported,
		       [{not_cover_compiled,Module}|Error])
    end;
are_loaded([], _State, Loaded, Imported, Error) ->
    {Loaded, Imported, Error}.

is_loaded(Module, State) ->
    case get_file(Module, State#main_state.compiled) of
	{ok, File} ->
	    case code:which(Module) of
		?TAG -> {loaded, File};
		_ -> throw(unloaded)
	    end;
	false ->
	    case get_file(Module,State#main_state.imported) of
		{ok,File,ImportFiles} ->
		    {imported, File, ImportFiles};
		false ->
		    throw(not_loaded)
	    end
    end.

get_file(Module, [{Module, File}|_T]) ->
    {ok, File};
get_file(Module, [{Module, File, ImportFiles}|_T]) ->
    {ok, File, ImportFiles};
get_file(Module, [_H|T]) ->
    get_file(Module, T);
get_file(_Module, []) ->
    false.

get_beam_file(Module,?TAG,Compiled) ->
    {value,{Module,File}} = lists:keysearch(Module,1,Compiled),
    case filename:extension(File) of
	".erl" -> {error,no_beam};
	".beam" -> {ok,File}
    end;
get_beam_file(_Module,BeamFile,_Compiled) ->
    {ok,BeamFile}.

get_modules(Compiled) ->
    lists:map(fun({Module, _File}) -> Module end, Compiled).

update_compiled([Module|Modules], [{Module,_File}|Compiled]) ->
    update_compiled(Modules, Compiled);
update_compiled(Modules, [H|Compiled]) ->
    [H|update_compiled(Modules, Compiled)];
update_compiled(_Modules, []) ->
    [].

%% Get all compiled modules which are still loaded, and possibly an
%% updated version of the Compiled list.
get_compiled_still_loaded(Nodes,Compiled0) ->
    %% Find all Cover compiled modules which are still loaded
    CompiledModules = get_modules(Compiled0),
    LoadedModules = lists:filter(fun(Module) ->
					 case code:which(Module) of
					     ?TAG -> true;
					     _ -> false
					 end
				 end,
				 CompiledModules),

    %% If some Cover compiled modules have been unloaded, update the database.
    UnloadedModules = CompiledModules--LoadedModules,
    Compiled = 
	case UnloadedModules of
	    [] ->
		Compiled0;
	    _ ->
		lists:foreach(fun(Module) -> do_clear(Module) end,
			      UnloadedModules),
		remote_unload(Nodes,UnloadedModules),
		update_compiled(UnloadedModules, Compiled0)
	end,
    {LoadedModules,Compiled}.


%%%--Compilation---------------------------------------------------------

do_compile_beams(ModsAndFiles, State) ->
    Result0 = pmap(fun({ok,Module,File}) ->
			  do_compile_beam(Module, File, State);
		     (Error) ->
			  Error
		  end,
		  ModsAndFiles),
    Compiled = [{M,F} || {ok,M,F} <- Result0],
    remote_load_compiled(State#main_state.nodes, Compiled, State),
    fix_state_and_result(Result0,State,[]).

do_compile_beam(Module,BeamFile0,State) ->
    case get_beam_file(Module,BeamFile0,State#main_state.compiled) of
	{ok,BeamFile} ->
            LocalOnly = State#main_state.local_only,
	    UserOptions = get_compile_options(Module,BeamFile),
	    case do_compile_beam1(Module,BeamFile,
                                  UserOptions,LocalOnly) of
		{ok, Module} ->
		    {ok,Module,BeamFile};
		error ->
		    {error, BeamFile};
		{error,Reason} -> % no abstract code or no 'file' attribute
		    {error, {Reason, BeamFile}}
	    end;
	{error,no_beam} ->
	    %% The module has first been compiled from .erl, and now
	    %% someone tries to compile it from .beam
	    {error,{already_cover_compiled,no_beam_found,Module}}
    end.

fix_state_and_result([{ok,Module,BeamFile}|Rest],State,Acc) ->
    Compiled = add_compiled(Module,BeamFile,State#main_state.compiled),
    Imported = remove_imported(Module,State#main_state.imported),
    NewState = State#main_state{compiled=Compiled,imported=Imported},
    fix_state_and_result(Rest,NewState,[{ok,Module}|Acc]);
fix_state_and_result([Error|Rest],State,Acc) ->
    fix_state_and_result(Rest,State,[Error|Acc]);
fix_state_and_result([],State,Acc) ->
    {lists:reverse(Acc),State}.


do_compile(Files, Options, State) ->
    LocalOnly = State#main_state.local_only,
    Result0 = pmap(fun(File) ->
			   do_compile1(File, Options, LocalOnly)
		   end,
		   Files),
    Compiled = [{M,F} || {ok,M,F} <- Result0],
    remote_load_compiled(State#main_state.nodes, Compiled, State),
    fix_state_and_result(Result0,State,[]).

do_compile1(File, Options, LocalOnly) ->
    case do_compile2(File, Options, LocalOnly) of
	{ok, Module} ->
	    {ok,Module,File};
	error ->
	    {error,File}
    end.

%% do_compile2(File, Options) -> {ok,Module} | error
do_compile2(File, UserOptions, LocalOnly) ->
    Options = [debug_info,binary,report_errors,report_warnings] ++ UserOptions,
    case compile:file(File, Options) of
	{ok, Module, Binary} ->
	    do_compile_beam1(Module,Binary,UserOptions,LocalOnly);
	error ->
	    error
    end.

%% Beam is a binary or a .beam file name
do_compile_beam1(Module,Beam,UserOptions,LocalOnly) ->
    %% Clear database
    do_clear(Module),

    %% Extract the abstract format.
    case get_abstract_code(Module, Beam) of
	{error,_}=Error ->
            Error;
	{ok,{raw_abstract_v1,Code}} ->
            Forms0 = epp:interpret_file_attribute(Code),
	    case find_main_filename(Forms0) of
		{ok,MainFile} ->
		    do_compile_beam2(Module,Beam,UserOptions,
                                     Forms0,MainFile,LocalOnly);
		Error ->
		    Error
	    end;
	{ok,{_VSN,_Code}} ->
	    %% Wrong version of abstract code. Just report that there
	    %% is no abstract code.
	    {error,no_abstract_code}
    end.

get_abstract_code(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
	{ok, {Module, [{abstract_code, AbstractCode}]}} ->
            case AbstractCode of
                no_abstract_code=E -> {error, E};
                _ -> {ok,AbstractCode}
            end;
	{error,beam_lib,{key_missing_or_invalid,_,_}} ->
	    {error,encrypted_abstract_code};
	{error,beam_lib,{missing_backend,_,Backend}} ->
	    {error,{missing_backend,Backend}}
    end.

do_compile_beam2(Module,Beam,UserOptions,Forms0,MainFile,LocalOnly) ->
    init_counter_mapping(Module),

    %% Instrument the abstract code by inserting
    %% calls to update the counters.
    Forms = transform(Forms0, Module, MainFile, LocalOnly),

    %% Create counters.
    maybe_create_counters(Module, not LocalOnly),

    %% We need to recover the source from the compilation
    %% info otherwise the newly compiled module will have
    %% source pointing to the current directory
    SourceInfo = get_source_info(Module, Beam),

    %% Compile and load the result.
    %% It's necessary to check the result of loading since it may
    %% fail, for example if Module resides in a sticky directory.
    Options0 = SourceInfo ++ UserOptions,
    Options = [report_errors,force_line_counters|Options0],

    {ok, Module, Binary} = compile:forms(Forms, Options),

    case code:load_binary(Module, ?TAG, Binary) of
	{module, Module} ->
	    %% Store binary code so it can be loaded on remote nodes.
	    ets:insert(?BINARY_TABLE, {Module, Binary}),
	    {ok, Module};
	_Error ->
	    do_clear(Module),
	    error
    end.

get_source_info(Module, Beam) ->
    Compile = get_compile_info(Module, Beam),
    case lists:keyfind(source, 1, Compile) of
        { source, _ } = Tuple -> [Tuple];
        false -> []
    end.

get_compile_options(Module, Beam) ->
    Compile = get_compile_info(Module, Beam),
    case lists:keyfind(options, 1, Compile) of
        {options, Options } -> filter_options(Options);
        false -> []
    end.

get_compile_info(Module, Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
	{ok, {Module, [{compile_info, Compile}]}} ->
		Compile;
	_ ->
		[]
    end.

transform(Code, Module, _MainFile, LocalOnly) ->
    {ok,MungedForms0} = sys_coverage:cover_transform(Code, fun counter_index/5),
    patch_code(Module, MungedForms0, LocalOnly).

%% Helpfunction which returns the first found file-attribute, which can
%% be interpreted as the name of the main erlang source file.
find_main_filename([{attribute,_,file,{MainFile,_}}|_]) ->
    {ok,MainFile};
find_main_filename([_|Rest]) ->
    find_main_filename(Rest);
find_main_filename([]) ->
    {error, no_file_attribute}.

%%%--Counters------------------------------------------------------------

init_counter_mapping(Mod) ->
    true = ets:insert_new(?COVER_MAPPING_TABLE, {Mod,0}),
    ok.

counter_index(Mod, F, A, C, Line) ->
    Key = #bump{module=Mod,function=F,arity=A,
                clause=C,line=Line},
    case ets:lookup(?COVER_MAPPING_TABLE, Key) of
        [] ->
            Index = ets:update_counter(?COVER_MAPPING_TABLE,
                                       Mod, {2,1}),
            true = ets:insert(?COVER_MAPPING_TABLE, {Key,Index}),
            Index;
        [{Key,Index}] ->
            Index
    end.

%% Create the counter array and store as a persistent term.
maybe_create_counters(Mod, true) ->
    case has_native_coverage() of
        false ->
            Cref = create_counters(Mod),
            Key = {?MODULE,Mod},
            persistent_term:put(Key, Cref),
            ok;
        true ->
            ok
    end;
maybe_create_counters(_Mod, false) ->
    ok.

create_counters(Mod) ->
    Size0 = ets:lookup_element(?COVER_MAPPING_TABLE, Mod, 2),
    Size = max(1, Size0),                       %Size must not be 0.
    Cref = counters:new(Size, [write_concurrency]),
    ets:insert(?COVER_MAPPING_TABLE, {{counters,Mod},Cref}),
    Cref.

patch_code(Mod, Forms, Local) ->
    case has_native_coverage() of
        true ->
            _ = catch code:reset_coverage(Mod),
            Forms;
        false when Local =:= false ->
            A = erl_anno:new(0),
            AbstrKey = {tuple,A,[{atom,A,?MODULE},{atom,A,Mod}]},
            patch_code1(Forms, {distributed,AbstrKey});
        false when Local =:= true ->
            Cref = create_counters(Mod),
            AbstrCref = cid_to_abstract(Cref),
            patch_code1(Forms, {local_only,AbstrCref})
    end.

%% Go through the abstract code and replace 'executable_line' forms
%% with the actual code to increment the counters.
patch_code1({executable_line,_Anno,Index}, {distributed,AbstrKey}) ->
    %% Replace with counters:add(persistent_term:get(Key), Index, 1).
    %% This code will work on any node.
    A = element(2, AbstrKey),
    GetCref = {call,A,{remote,A,{atom,A,persistent_term},{atom,A,get}},
               [AbstrKey]},
    {call,A,{remote,A,{atom,A,counters},{atom,A,add}},
     [GetCref,{integer,A,Index},{integer,A,1}]};
patch_code1({executable_line,_Anno,Index}, {local_only,AbstrCref}) ->
    %% Replace with counters:add(Cref, Index, 1). This code
    %% will only work on the local node.
    A = element(2, AbstrCref),
    {call,A,{remote,A,{atom,A,counters},{atom,A,add}},
     [AbstrCref,{integer,A,Index},{integer,A,1}]};
patch_code1({clauses,Cs}, Key) ->
    {clauses,[patch_code1(El, Key) || El <- Cs]};
patch_code1({attribute, _, _, _} = Attribute, _Key) ->
    Attribute;
patch_code1([_|_]=List, Key) ->
    [patch_code1(El, Key) || El <- List];
patch_code1(Tuple, Key) when tuple_size(Tuple) >= 3 ->
    Acc = [element(2, Tuple),element(1, Tuple)],
    patch_code_tuple(3, tuple_size(Tuple), Tuple, Key, Acc);
patch_code1(Other, _Key) ->
    Other.

patch_code_tuple(I, Size, Tuple, Key, Acc) when I =< Size ->
    El = patch_code1(element(I, Tuple), Key),
    patch_code_tuple(I + 1, Size, Tuple, Key, [El|Acc]);
patch_code_tuple(_I, _Size, _Tuple, _Key, Acc) ->
    list_to_tuple(lists:reverse(Acc)).

%% Don't try this at home! Assumes knowledge of the internal
%% representation of a counter ref.
cid_to_abstract(Cref0) ->
    A = erl_anno:new(0),
    %% Disable dialyzer warning for breaking opacity.
    Cref = binary_to_term(term_to_binary(Cref0)),
    {write_concurrency,Ref} = Cref,
    {tuple,A,[{atom,A,write_concurrency},{integer,A,Ref}]}.

%% Called on the remote node. Collect and send counters to
%% the main node. Also zero the counters.
send_counters(Mod, CollectorPid) ->
    Process = fun(Chunk) -> send_chunk(CollectorPid, Chunk) end,
    move_counters(Mod, Process).

%% Called on the main node. Collect the counters and consolidate
%% them into the collection table. Also zero the counters.
move_counters(Mod) ->
    Process = fun insert_in_collection_table/1,
    move_counters(Mod, Process).

move_counters(Mod, Process) ->
    Move = case has_native_coverage() of
               true ->
                   native_move(Mod);
               false ->
                   standard_move(Mod)
           end,
    Pattern = {#bump{module=Mod,_='_'},'_'},
    Matches = ets:match_object(?COVER_MAPPING_TABLE, Pattern, ?CHUNK_SIZE),
    move_counters1(Matches, Move, Process).

move_counters1({Mappings,Continuation}, Move, Process) ->
    Moved = [Move(Item) || Item <- Mappings],
    Process(Moved),
    move_counters1(ets:match_object(Continuation), Move, Process);
move_counters1('$end_of_table', _Move, _Process) ->
    ok.

counters_mapping_table(Mod) ->
    Mapping = counters_mapping(Mod),
    case has_native_coverage() of
        false ->
            Cref = get_counters_ref(Mod),
            #{size:=Size} = counters:info(Cref),
            [{Mod,Size}|Mapping];
        true ->
            Mapping
    end.

get_counters_ref(Mod) ->
    ets:lookup_element(?COVER_MAPPING_TABLE, {counters,Mod}, 2).

counters_mapping(Mod) ->
    Pattern = {#bump{module=Mod,_='_'},'_'},
    ets:match_object(?COVER_MAPPING_TABLE, Pattern).

clear_counters(Mod) ->
    _ = persistent_term:erase({?MODULE,Mod}),
    ets:delete(?COVER_MAPPING_TABLE, Mod),
    Pattern = {#bump{module=Mod,_='_'},'_'},
    _ = ets:match_delete(?COVER_MAPPING_TABLE, Pattern),
    ok.

standard_move(Mod) ->
    Cref = get_counters_ref(Mod),
    fun({Key,Index}) ->
            Count = counters:get(Cref, Index),
            ok = counters:sub(Cref, Index, Count),
            {Key,Count}
    end.

native_move(Mod) ->
    Coverage = maps:from_list(code:get_coverage(line, Mod)),
    _ = code:reset_coverage(Mod),
    fun({#bump{line=Line}=Key,_Index}) ->
                   case Coverage of
                       #{Line := false} ->
                           {Key,0};
                       #{Line := true} ->
                           {Key,1};
                       #{Line := N} when is_integer(N), N >= 0 ->
                           {Key,N};
                       #{} ->
                           {Key,0}
                   end
           end.

%% Reset counters (set counters to 0).
reset_counters(Mod) ->
    case has_native_coverage() of
        true ->
            _ = catch code:reset_coverage(Mod),
            ok;
        false ->
            Pattern = {#bump{module=Mod,_='_'},'$1'},
            MatchSpec = [{Pattern,[],['$1']}],
            Matches = ets:select(?COVER_MAPPING_TABLE,
                                 MatchSpec, ?CHUNK_SIZE),
            Cref = get_counters_ref(Mod),
            reset_counters1(Matches, Cref)
    end.

reset_counters1({Indices,Continuation}, Cref) ->
    _ = [counters:put(Cref, N, 0) || N <- Indices],
    reset_counters1(ets:select(Continuation), Cref);
reset_counters1('$end_of_table', _Cref) ->
    ok.

delete_all_counters() ->
    _ = [persistent_term:erase(Key) || {?MODULE,_}=Key <- persistent_term:get()],
    ok.

%%%--Analysis------------------------------------------------------------

%% Collect data for all modules
collect(Nodes) ->
    Modules = [Module || {Module,_} <- ets:tab2list(?BINARY_TABLE)],
    collect_modules(Modules, Nodes).

%% Collect data for a list of modules
collect(Modules0, Nodes) ->
    Modules = [Module || Module <- Modules0, ets:member(?BINARY_TABLE, Module)],
    collect_modules(Modules, Nodes).

collect_modules(Modules, Nodes) ->
    Mon1 = spawn_monitor(fun() -> pmap(fun move_modules/1, Modules) end),
    Mon2 = spawn_monitor(fun() -> remote_collect('_', Nodes, false) end),
    get_downs([Mon1,Mon2]).

%% Collect data for one module
collect_module(Module, #main_state{nodes=Nodes}) ->
    move_modules(Module),
    remote_collect([Module], Nodes, false).

%% When analysing, the data from the local ?COVER_TABLE is moved to the
%% ?COLLECTION_TABLE. Resetting data in ?COVER_TABLE
move_modules(Module) when is_atom(Module) ->
    move_counters(Module).

%% Given a .beam file, find the .erl file. Look first in same directory as
%% the .beam file, then in ../src, then in compile info.
find_source(Module, File0) ->
    try
        Root = filename:rootname(File0, ".beam"),
        Root == File0 andalso throw(File0),  %% not .beam
        %% Look for .erl in pwd.
        File = Root ++ ".erl",
        throw_file(File),
        %% Not in pwd: look in ../src.
        BeamDir = filename:dirname(File),
        Base = filename:basename(File),
        throw_file(filename:join([BeamDir, "..", "src", Base])),
        %% Not in ../src: look for source path in compile info, but
        %% first look relative the beam directory.
        Info =
            try lists:keyfind(source, 1, Module:module_info(compile))
            catch error:undef ->
                    %% The module might have been imported
                    %% and the beam not available
                    throw({beam, File0})
            end,
        false == Info andalso throw({beam, File0}),  %% stripped
        {source, SrcFile} = Info,
        throw_file(splice(BeamDir, SrcFile)),  %% below ../src
        throw_file(SrcFile),                   %% or absolute
        %% No success means that source is either not under ../src or
        %% its relative path differs from that of compile info. (For
        %% example, compiled under src/x but installed under src/y.)
        %% An option to specify an arbitrary source path explicitly is
        %% probably a better solution than either more heuristics or a
        %% potentially slow filesystem search.
        {beam, File0}
    catch
        Path -> Path
    end.

throw_file(Path) ->
    false /= Path andalso filelib:is_file(Path) andalso throw(Path).

%% Splice the tail of a source path, starting from the last "src"
%% component, onto the parent of a beam directory, or return false if
%% no "src" component is found.
%%
%% Eg. splice("/path/to/app-1.0/ebin", "/compiled/path/to/app/src/x/y.erl")
%%        --> "/path/to/app-1.0/ebin/../src/x/y.erl"
%%
%% This handles the case of source in subdirectories of ../src with
%% beams that have moved since compilation.
%%
splice(BeamDir, SrcFile) ->
    case lists:splitwith(fun(C) -> C /= "src" end, revsplit(SrcFile)) of
        {T, [_|_]} ->  %% found src component
            filename:join([BeamDir, "..", "src" | lists:reverse(T)]);
        {_, []} ->     %% or not
            false
    end.

revsplit(Path) ->
    lists:reverse(filename:split(Path)).

analyse_list(Modules, Analysis, Level, State) ->
    {LoadedMF, ImportedMF, Error} = are_loaded(Modules, State, [], [], []),
    Loaded = [M || {M,_} <- LoadedMF],
    Imported = [M || {M,_} <- ImportedMF],
    collect(Loaded, State#main_state.nodes),
    All = Loaded ++ Imported,
    Fun = fun(Module) ->
		  do_analyse(Module, Analysis, Level)
	  end,
    {result, lists:flatten(pmap(Fun, All)), Error}.

analyse_all(Analysis, Level, State) ->
    collect(State#main_state.nodes),
    All = ets:tab2list(?BINARY_TABLE),
    Fun = fun({Module,_}) ->
		  do_analyse(Module, Analysis, Level)
	  end,
    {result, lists:flatten(pmap(Fun, All)), []}.

do_parallel_analysis(Module, Analysis, Level, Loaded, From, State) ->
    analyse_info(Module,State#main_state.imported),
    _ = case Loaded of
	    {loaded, _File} ->
		collect_module(Module, State);
	    _ ->
                ok
	end,
    R = do_analyse(Module, Analysis, Level),
    reply(From, {ok,R}).

%% do_analyse(Module, Analysis, Level, Clauses)-> {ok,Answer} | {error,Error}
%%   Clauses = [{Module,Function,Arity,Clause,Lines}]
do_analyse(Module, Analysis, line) ->
    Pattern = {#bump{module=Module},'_'},
    Bumps = ets:match_object(?COLLECTION_TABLE, Pattern),
    Fun = case Analysis of
	      coverage ->
		  fun({#bump{line=L}, 0}) ->
			  {{Module,L}, {0,1}};
		     ({#bump{line=L}, _N}) ->
			  {{Module,L}, {1,0}}
		  end;
	      calls ->
		  fun({#bump{line=L}, N}) ->
			  {{Module,L}, N}
		  end
	  end,
    lists:keysort(1, lists:map(Fun, Bumps));
do_analyse(Module, Analysis, clause) ->
    Pattern = {#bump{module=Module},'_'},
    Bumps = lists:keysort(1,ets:match_object(?COLLECTION_TABLE, Pattern)),
    analyse_clause(Analysis,Bumps);
do_analyse(Module, Analysis, function) ->
    ClauseResult = do_analyse(Module, Analysis, clause),
    merge_clauses(ClauseResult, merge_fun(Analysis));
do_analyse(Module, Analysis, module) ->
    FunctionResult = do_analyse(Module, Analysis, function),
    Result = merge_functions(FunctionResult, merge_fun(Analysis)),
    {Module,Result}.

analyse_clause(_,[]) ->
    [];
analyse_clause(coverage,
	       [{#bump{module=M,function=F,arity=A,clause=C},_}|_]=Bumps) ->
    analyse_clause_cov(Bumps,{M,F,A,C},0,0,[]);
analyse_clause(calls,Bumps) ->
    analyse_clause_calls(Bumps,{x,x,x,x},[]).

analyse_clause_cov([{#bump{module=M,function=F,arity=A,clause=C},N}|Bumps],
		   {M,F,A,C}=Clause,Ls,NotCov,Acc) ->
    analyse_clause_cov(Bumps,Clause,Ls+1,if N==0->NotCov+1; true->NotCov end,Acc);
analyse_clause_cov([{#bump{module=M1,function=F1,arity=A1,clause=C1},_}|_]=Bumps,
		    Clause,Ls,NotCov,Acc) ->
    analyse_clause_cov(Bumps,{M1,F1,A1,C1},0,0,[{Clause,{Ls-NotCov,NotCov}}|Acc]);
analyse_clause_cov([],Clause,Ls,NotCov,Acc) ->
    lists:reverse(Acc,[{Clause,{Ls-NotCov,NotCov}}]).

analyse_clause_calls([{#bump{module=M,function=F,arity=A,clause=C},_}|Bumps],
		     {M,F,A,C}=Clause,Acc) ->
    analyse_clause_calls(Bumps,Clause,Acc);
analyse_clause_calls([{#bump{module=M1,function=F1,arity=A1,clause=C1},N}|Bumps],
		     _Clause,Acc) ->
    analyse_clause_calls(Bumps,{M1,F1,A1,C1},[{{M1,F1,A1,C1},N}|Acc]);
analyse_clause_calls([],_Clause,Acc) ->
    lists:reverse(Acc).

merge_fun(coverage) ->
    fun({Cov1,NotCov1}, {Cov2,NotCov2}) ->
	    {Cov1+Cov2, NotCov1+NotCov2}
    end;
merge_fun(calls) ->
    fun(Calls1, Calls2) ->
	    Calls1+Calls2
    end.

merge_clauses(Clauses, MFun) -> merge_clauses(Clauses, MFun, []).
merge_clauses([{{M,F,A,_C1},R1},{{M,F,A,C2},R2}|Clauses], MFun, Result) ->
    merge_clauses([{{M,F,A,C2},MFun(R1,R2)}|Clauses], MFun, Result);
merge_clauses([{{M,F,A,_C},R}|Clauses], MFun, Result) ->
    merge_clauses(Clauses, MFun, [{{M,F,A},R}|Result]);
merge_clauses([], _Fun, Result) ->
    lists:reverse(Result).

merge_functions([{_MFA,R}|Functions], MFun) ->
    merge_functions(Functions, MFun, R);
merge_functions([],_MFun) ->         % There are no clauses.
    {0,0}.                           % No function can be covered or notcov.

merge_functions([{_MFA,R}|Functions], MFun, Result) ->
    merge_functions(Functions, MFun, MFun(Result, R));
merge_functions([], _MFun, Result) ->
    Result.

analyse_list_to_file(Modules, Opts, State) ->
    {LoadedMF, ImportedMF, Error} = are_loaded(Modules, State, [], [], []),
    collect([M || {M,_} <- LoadedMF], State#main_state.nodes),
    OutDir = proplists:get_value(outdir,Opts),
    HTML = lists:member(html,Opts),
    Fun = fun({Module,File}) ->
		  OutFile = outfilename(OutDir,Module,HTML),
		  do_analyse_to_file(Module,File,OutFile,HTML,State)
	  end,
    {Ok,Error1} = split_ok_error(pmap(Fun, LoadedMF++ImportedMF),[],[]),
    {result,Ok,Error ++ Error1}.

analyse_all_to_file(Opts, State) ->
    collect(State#main_state.nodes),
    AllModules = get_all_modules(State),
    OutDir = proplists:get_value(outdir,Opts),
    HTML = lists:member(html,Opts),
    Fun = fun({Module,File}) ->
		  OutFile = outfilename(OutDir,Module,HTML),
		  do_analyse_to_file(Module,File,OutFile,HTML,State)
	  end,
    {Ok,Error} = split_ok_error(pmap(Fun, AllModules),[],[]),
    {result,Ok,Error}.

get_all_modules(State) ->
    get_all_modules(State#main_state.compiled ++ State#main_state.imported,[]).
get_all_modules([{Module,File}|Rest],Acc) ->
    get_all_modules(Rest,[{Module,File}|Acc]);
get_all_modules([{Module,File,_}|Rest],Acc) ->
    case lists:keymember(Module,1,Acc) of
	true -> get_all_modules(Rest,Acc);
	false -> get_all_modules(Rest,[{Module,File}|Acc])
    end;
get_all_modules([],Acc) ->
    Acc.

split_ok_error([{ok,R}|Result],Ok,Error) ->
    split_ok_error(Result,[R|Ok],Error);
split_ok_error([{error,R}|Result],Ok,Error) ->
    split_ok_error(Result,Ok,[R|Error]);
split_ok_error([],Ok,Error) ->
    {Ok,Error}.

do_parallel_analysis_to_file(Module, Opts, Loaded, From, State) ->
    File = case Loaded of
	       {loaded, File0} ->
		   collect_module(Module, State),
		   File0;
	       {imported, File0, _} ->
		   File0
	   end,
    HTML = lists:member(html,Opts),
    OutFile =
	case proplists:get_value(outfile,Opts) of
	    undefined ->
		outfilename(proplists:get_value(outdir,Opts),Module,HTML);
	    F ->
		F
	end,
    reply(From, do_analyse_to_file(Module,File,OutFile,HTML,State)).

do_analyse_to_file(Module,File,OutFile,HTML,State) ->
    case find_source(Module, File) of
	{beam,_BeamFile} ->
	    {error,{no_source_code_found,Module}};
	ErlFile ->
	    analyse_info(Module,State#main_state.imported),
	    do_analyse_to_file1(Module,OutFile,ErlFile,HTML)
    end.

%% do_analyse_to_file1(Module,OutFile,ErlFile) -> {ok,OutFile} | {error,Error}
%%   Module = atom()
%%   OutFile = ErlFile = string()
do_analyse_to_file1(Module, OutFile, ErlFile, HTML) ->
    case file:open(ErlFile, [read,raw,read_ahead]) of
	{ok, InFd} ->
	    case file:open(OutFile, [write,raw,delayed_write]) of
		{ok, OutFd} ->
                    Enc = encoding(ErlFile),
		    if HTML ->
                            Header = create_header(OutFile, Enc),
                            H1Bin = unicode:characters_to_binary(Header,Enc,Enc),
                            ok = file:write(OutFd,H1Bin);
		       true -> ok
		    end,
		    
		    %% Write some initial information to the output file
		    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
                   Timestamp =
                       io_lib:format("~p-~s-~s at ~s:~s:~s",
                                     [Y,
                                      string:pad(integer_to_list(Mo), 2, leading, $0),
                                      string:pad(integer_to_list(D),  2, leading, $0),
                                      string:pad(integer_to_list(H),  2, leading, $0),
                                      string:pad(integer_to_list(Mi), 2, leading, $0),
                                      string:pad(integer_to_list(S),  2, leading, $0)]),

                   OutFileInfo =
                       if HTML ->
                            create_footer(ErlFile, Timestamp);
                          true ->
                            ["File generated from ",ErlFile," by COVER ",
                             Timestamp, "\n\n",
                             "**************************************"
                             "**************************************"
                             "\n\n"]
                          end,

                   H2Bin = unicode:characters_to_binary(OutFileInfo,Enc,Enc),
                   ok = file:write(OutFd, H2Bin),

		    Pattern = {#bump{module=Module,line='$1',_='_'},'$2'},
		    MS = [{Pattern,[{is_integer,'$1'},{'>','$1',0}],[{{'$1','$2'}}]}],
                    CovLines0 =
                        lists:keysort(1, ets:select(?COLLECTION_TABLE, MS)),
                    CovLines = merge_dup_lines(CovLines0),
		    print_lines(Module, CovLines, InFd, OutFd, 1, HTML),
		    
		    if HTML ->
                           ok = file:write(OutFd, close_html());
		       true -> ok
		    end,

		    ok = file:close(OutFd),
		    ok = file:close(InFd),

		    {ok, OutFile};

		{error, Reason} ->
		    {error, {file, OutFile, Reason}}
	    end;

	{error, Reason} ->
	    {error, {file, ErlFile, Reason}}
    end.

merge_dup_lines(CovLines) ->
    merge_dup_lines(CovLines, []).
merge_dup_lines([{L, N}|T], [{L, NAcc}|TAcc]) ->
    merge_dup_lines(T, [{L, NAcc + N}|TAcc]);
merge_dup_lines([{L, N}|T], Acc) ->
    merge_dup_lines(T, [{L, N}|Acc]);
merge_dup_lines([], Acc) ->
    lists:reverse(Acc).

print_lines(Module, CovLines, InFd, OutFd, L, HTML) ->
    case file:read_line(InFd) of
	eof ->
	    ignore;
	{ok,RawLine} ->
	    Line = escape_lt_and_gt(RawLine,HTML),
	    case CovLines of
	       [{L,N}|CovLines1] ->
                    if N=:=0, HTML=:=true ->
                           MissedLine = table_row("miss", Line, L, N),
                           ok = file:write(OutFd, MissedLine);
                       HTML=:=true ->
                           HitLine = table_row("hit", Line, L, N),
                           ok = file:write(OutFd, HitLine);
                       N < 1000000 ->
                           Str = string:pad(integer_to_list(N), 6, leading, $\s),
                           ok = file:write(OutFd, [Str,fill1(),Line]);
                       N < 10000000 ->
                           Str = integer_to_list(N),
                           ok = file:write(OutFd, [Str,fill2(),Line]);
                       true ->
                           Str = integer_to_list(N),
                           ok = file:write(OutFd, [Str,fill3(),Line])
                    end,
		    print_lines(Module, CovLines1, InFd, OutFd, L+1, HTML);
		_ ->                            %Including comment lines
        NonCoveredContent =
                    if HTML -> table_row(Line, L);
                    true -> [tab(),Line]
                    end,
		    ok = file:write(OutFd, NonCoveredContent),
		    print_lines(Module, CovLines, InFd, OutFd, L+1, HTML)
	    end
    end.

tab() ->  "        |  ".
fill1() ->      "..|  ".
fill2() ->       ".|  ".
fill3() ->        "|  ".

%% HTML sections
create_header(OutFile, Enc) ->
    ["<!doctype html>\n"
    "<html>\n"
    "<head>\n"
    "<meta charset=\"",html_encoding(Enc),"\">\n"
    "<title>",OutFile,"</title>\n"
    "<style>"] ++
    read_stylesheet() ++
    ["</style>\n",
    "</head>\n"
    "<body>\n"
    "<h1><code>",OutFile,"</code></h1>\n"].

create_footer(ErlFile, Timestamp) ->
    ["<footer><p>File generated from <code>",ErlFile,
    "</code> by <a href=\"http://erlang.org/doc/man/cover.html\">cover</a> at ",
    Timestamp,"</p></footer>\n<table>\n<tbody>\n"].

close_html() ->
    ["</tbody>\n",
     "<thead>\n",
     "<tr>\n",
     "<th>Line</th>\n",
     "<th>Hits</th>\n",
     "<th>Source</th>\n",
     "</tr>\n",
     "</thead>\n",
     "</table>\n",
     "</body>\n"
     "</html>\n"].

table_row(CssClass, Line, L, N) ->
    ["<tr class=\"",CssClass,"\">\n", table_data(Line, L, N)].
table_row(Line, L) ->
    ["<tr>\n", table_data(Line, L, "")].

table_data(Line, L, N) ->
   LineNoNL = Line -- "\n",
   ["<td class=\"line\" id=\"L",integer_to_list(L),"\">",
    "<a href=\"#L",integer_to_list(L),"\">",
    integer_to_list(L),
    "</a></td>\n",
   "<td class=\"hits\">",maybe_integer_to_list(N),"</td>\n",
   "<td class=\"source\"><code>",LineNoNL,"</code></td>\n</tr>\n"].

maybe_integer_to_list(0) -> "<pre style=\"display: inline;\">:-(</pre>";
maybe_integer_to_list(N) when is_integer(N) -> integer_to_list(N);
maybe_integer_to_list(_) -> "".

read_stylesheet() ->
    PrivDir = code:priv_dir(?TOOLS_APP),
    {ok, Css} = file:read_file(filename:join(PrivDir, ?STYLESHEET)),
    [Css].

%%%--Export--------------------------------------------------------------
do_export(Module, OutFile, From, State) ->
    case file:open(OutFile,[write,binary,raw,delayed_write]) of
	{ok,Fd} ->
	    Reply = 
		case Module of
		    '_' ->
			export_info(State#main_state.imported),
			collect(State#main_state.nodes),
			do_export_table(State#main_state.compiled,
					State#main_state.imported,
					Fd);
		    _ ->
			export_info(Module,State#main_state.imported),
			try is_loaded(Module, State) of
			    {loaded, File} ->
				collect_module(Module, State),
				do_export_table([{Module,File}],[],Fd);
			    {imported, File, ImportFiles} ->
				%% don't know if I should allow this - 
				%% export a module which is only imported
				Imported = [{Module,File,ImportFiles}],
				do_export_table([],Imported,Fd)
			catch throw:_ ->
				{error,{not_cover_compiled,Module}}
			end
		end,
	    ok = file:close(Fd),
	    reply(From, Reply);
	{error,Reason} ->
	    reply(From, {error, {cant_open_file,OutFile,Reason}})

    end.

do_export_table(Compiled, Imported, Fd) ->
    ModList = merge(Imported,Compiled),
    write_module_data(ModList,Fd).

merge([{Module,File,_ImportFiles}|Imported],ModuleList) ->
    case lists:keymember(Module,1,ModuleList) of
	true ->
	    merge(Imported,ModuleList);
	false ->
	    merge(Imported,[{Module,File}|ModuleList])
    end;
merge([],ModuleList) ->
    ModuleList.

write_module_data([{Module,File}|ModList], Fd) ->
    write({file,Module,File}, Fd),
    ModuleData = ets:match_object(?COLLECTION_TABLE, {#bump{module=Module},'_'}),
    do_write_module_data(ModuleData, Fd),
    write_module_data(ModList, Fd);
write_module_data([], _Fd) ->
    ok.

do_write_module_data([H|T],Fd) ->
    write(H,Fd),
    do_write_module_data(T,Fd);
do_write_module_data([],_Fd) ->
    ok.

write(Element,Fd) ->
    Bin = term_to_binary(Element,[compressed]),
    case byte_size(Bin) of
	Size when Size > 255 ->
	    SizeBin = term_to_binary({'$size',Size}),
	    ok = file:write(Fd, <<(byte_size(SizeBin)):8,SizeBin/binary,Bin/binary>>);
	Size ->
	    ok = file:write(Fd,<<Size:8,Bin/binary>>)
    end,
    ok.    

%%%--Import--------------------------------------------------------------
do_import_to_table(Fd,ImportFile,Imported) ->
    do_import_to_table(Fd,ImportFile,Imported,[]).
do_import_to_table(Fd,ImportFile,Imported,DontImport) ->
    case get_term(Fd) of
	{file,Module,File} ->
	    case add_imported(Module, File, ImportFile, Imported) of
		{ok,NewImported} ->
		    do_import_to_table(Fd,ImportFile,NewImported,DontImport);
		dont_import ->
		    do_import_to_table(Fd,ImportFile,Imported,
				       [Module|DontImport])
	    end;
	{Key=#bump{module=Module},Val} ->
	    case lists:member(Module,DontImport) of
		false ->
		    insert_in_collection_table(Key,Val);
		true ->
		    ok
	    end,
	    do_import_to_table(Fd,ImportFile,Imported,DontImport);
	eof ->
	    Imported
    end.
	    

get_term(Fd) ->
    case file:read(Fd,1) of
	{ok,<<Size1:8>>} ->
	    {ok,Bin1} = file:read(Fd,Size1),
	    case binary_to_term(Bin1) of
		{'$size',Size2} -> 
		    {ok,Bin2} = file:read(Fd,Size2),
		    binary_to_term(Bin2);
		Term ->
		    Term
	    end;
	eof ->
	    eof
    end.

%%%--Reset---------------------------------------------------------------

%% Reset main node and all remote nodes
do_reset_main_node(Module,Nodes) ->
    reset_counters(Module),
    do_reset_collection_table(Module),
    remote_reset(Module,Nodes).

do_reset_collection_table(Module) ->
    ets:match_delete(?COLLECTION_TABLE, {#bump{module=Module},'_'}).

do_clear(Module) ->
    clear_counters(Module),
    case lists:member(?COLLECTION_TABLE, ets:all()) of
	true ->
	    %% We're on the main node
            ets:match_delete(?BINARY_TABLE, {Module,'_'}),
	    ets:match_delete(?COLLECTION_TABLE, {#bump{module=Module},'_'});
	false ->
	    ok
    end.

not_loaded(Module, unloaded, State) ->
    do_clear(Module),
    remote_unload(State#main_state.nodes,[Module]),
    Compiled = update_compiled([Module],
			       State#main_state.compiled),
    State#main_state{ compiled = Compiled };
not_loaded(_Module,_Else, State) ->
    State.



%%%--Div-----------------------------------------------------------------

escape_lt_and_gt(Rawline,HTML) when HTML =/= true ->
    Rawline;
escape_lt_and_gt(Rawline,_HTML) ->
    escape_lt_and_gt1(Rawline,[]).

escape_lt_and_gt1([$<|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$t,$l,$&|Acc]);
escape_lt_and_gt1([$>|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$t,$g,$&|Acc]);
escape_lt_and_gt1([$&|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$p,$m,$a,$&|Acc]);
escape_lt_and_gt1([],Acc) ->
    lists:reverse(Acc);
escape_lt_and_gt1([H|T],Acc) ->
    escape_lt_and_gt1(T,[H|Acc]).

%%%--Internal functions for parallelization------------------------------
pmap(Fun,List) ->
    NTot = length(List),
    NProcs = erlang:system_info(schedulers) * 2,
    NPerProc = (NTot div NProcs) + 1,
    Mons = pmap_spawn(Fun,NPerProc,List,[]),
    pmap_collect(Mons,[]).

pmap_spawn(_,_,[],Mons) ->
    Mons;
pmap_spawn(Fun,NPerProc,List,Mons) ->
    {L1,L2} = if length(List)>=NPerProc -> lists:split(NPerProc,List);
		 true -> {List,[]} % last chunk
	      end,
    Mon =
	spawn_monitor(
	  fun() ->
		  exit({pmap_done,lists:map(Fun,L1)})
	  end),
    pmap_spawn(Fun,NPerProc,L2,[Mon|Mons]).

pmap_collect([],Acc) ->
    lists:append(Acc);
pmap_collect(Mons,Acc) ->
    receive
	{'DOWN', Ref, process, Pid, {pmap_done,Result}} ->
	    pmap_collect(lists:delete({Pid,Ref},Mons),[Result|Acc]);
	{'DOWN', Ref, process, Pid, Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    %% Something went really wrong - don't hang!
		    exit(Reason);
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    pmap_collect(Mons,Acc)
	    end
    end.

%%%-----------------------------------------------------------------
%%% Decide which encoding to use when analyzing to file.
%%% The target file contains the file path, so if either the file name
%%% encoding or the encoding of the source file is utf8, then we need
%%% to use utf8.
encoding(File) ->
    case file:native_name_encoding() of
        latin1 ->
            case epp:read_encoding(File) of
                none ->
                    epp:default_encoding();
                E ->
                    E
            end;
        utf8 ->
            utf8
    end.

html_encoding(latin1) ->
    "iso-8859-1";
html_encoding(utf8) ->
    "utf-8".

has_native_coverage() ->
    code:coverage_support().
