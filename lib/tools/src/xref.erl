%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2020. All Rights Reserved.
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

-module(xref).

-behaviour(gen_server).

%% External exports
-export([start/1, start/2, stop/1]).

-export([m/1, d/1,
	 add_release/2, add_release/3,
	 add_application/2, add_application/3, 
	 add_module/2, add_module/3,
	 add_directory/2, add_directory/3,
	 replace_module/3, replace_module/4,
	 replace_application/3, replace_application/4,
	 remove_module/2, remove_application/2, remove_release/2,
	 get_library_path/1, set_library_path/2, set_library_path/3,
	 q/2, q/3, info/1, info/2, info/3, 
	 update/1, update/2, 
	 forget/1, forget/2, variables/1, variables/2,
	 analyze/2, analyze/3, analyse/2, analyse/3,
	 get_default/1, get_default/2, 
	 set_default/2, set_default/3]).

-export([format_error/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(lists, [keydelete/3, keysearch/3]).

-import(sofs, [to_external/1, is_sofs_set/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-type application() :: atom().
-type call() :: {atom(), atom()} | funcall().
-type constant() :: xmfa() | module() | application() | release().
-type directory() :: atom() | file:filename().
-type file() :: file:filename().
-type file_error() :: atom().
-type funcall() :: {xmfa(), xmfa()}.
-type function_name() :: atom().
-type library() :: atom().
-type library_path() :: path() | 'code_path'.
-type mode() :: 'functions' | 'modules'.
-type path() :: [file()].
-type release() :: atom().
-type string_position() :: pos_integer().
-type variable() :: atom().
-type xarity() :: arity() | -1.
-type xmfa() :: {module(), function_name(), xarity()}.
-type xref() :: atom() | pid().

-spec m(FileOrModule) -> [DebugInfoResult] |
                   [NoDebugInfoResult] |
                   {'error', module(), Reason} when
      FileOrModule :: file:filename() | module(),
      DebugInfoResult :: {'deprecated', [funcall()]}
                       | {'undefined', [funcall()]}
                       | {'unused', [mfa()]},
      NoDebugInfoResult :: {'deprecated', [xmfa()]}
                         | {'undefined', [xmfa()]},
      Reason :: {'cover_compiled', Module}
              | {'file_error', file(), file_error()}
              | {'interpreted', Module}
              | {'invalid_filename', term()}
              | {'no_such_module', Module}
              | beam_lib:chnk_rsn().

%% No user variables have been assigned digraphs, so there is no
%% need to call xref_base:delete/1.
m(Module) when is_atom(Module) ->
    case xref_utils:find_beam(Module) of
	{ok, File} ->
	    Fun = fun(S) -> 
                          xref_base:add_module(S, File, {builtins,true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end;
	Error -> Error
    end;
m(File) ->
    case xref_utils:split_filename(File, ".beam") of
	false ->
	    {error, xref_base, {invalid_filename, File}};
	{Dir, BaseName} ->
	    BeamFile = filename:join(Dir, BaseName),
	    Fun = fun(S) -> 
                          xref_base:add_module(S, BeamFile, {builtins, true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end
    end.

-spec d(Directory) -> [DebugInfoResult] |
                      [NoDebugInfoResult] |
                      {'error', module(), Reason} when
      Directory :: directory(),
      DebugInfoResult :: {'deprecated', [funcall()]}
                       | {'undefined', [funcall()]}
                       | {'unused', [mfa()]},
      NoDebugInfoResult :: {'deprecated', [xmfa()]}
                         | {'undefined', [xmfa()]},
      Reason :: {'file_error', file(), file_error()}
              | {'invalid_filename', term()}
              | {'unrecognized_file', file()}
              | beam_lib:chnk_rsn().

d(Directory) ->
    Fun = fun(S) ->
                  xref_base:add_directory(S, Directory, {builtins, true})
          end,
    Fun1 = fun(S) ->
		   case Fun(S) of
		       {ok, [], _S} -> 
			   no_modules;
		       Reply -> 
			   Reply
		   end
	   end,
    case catch do_functions_analysis(Fun1) of
	no_modules ->
	    catch do_modules_analysis(Fun);
	Result -> 
	    Result
    end.

-spec start(NameOrOptions) ->
                   {'ok', pid()} |
                   {'error', {'already_started', pid()}} when
      NameOrOptions :: Name | Options,
      Name :: atom(),
      Options :: Option | [Option],
      Option :: {'xref_mode', mode()} | term().

start(Name) when is_atom(Name) ->
    start(Name, []);
start(Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start(xref, Args, Opts).

-spec start(Name, Options) ->
                   {'ok', pid()} |
                   {'error', {'already_started', pid()}} when
      Name :: atom(),
      Options :: Option | [Option],
      Option :: {'xref_mode', mode()} | term().

start(Name, Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start({local, Name}, xref, Args, Opts);
start(Name, Opt) ->
    start(Name, [Opt]).

split_args(Opts) ->
    case keysearch(xref_mode, 1, Opts) of
	{value, Mode} ->
	    {[Mode], keydelete(xref_mode, 1, Opts)};
	false ->
	    {[], Opts}
    end.

-spec stop(XrefServer) -> 'stopped' when
      XrefServer :: xref().

stop(Name) ->
    try gen_server:call(Name, stop, infinity)
    after catch unregister(Name) % ensure the name is gone
    end.

-spec add_release(XrefServer, Directory) ->
                         {'ok', release()} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'release_clash', {release(), directory(), directory()}}
              | add_dir_rsn().

add_release(Name, Dir) ->
    gen_server:call(Name, {add_release, Dir}, infinity).

-spec add_release(XrefServer, Directory, Options) ->
                         {'ok', release()} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'name', release()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'release_clash', {release(), directory(), directory()}}
              | add_dir_rsn().

add_release(Name, Dir, Options) ->
    gen_server:call(Name, {add_release, Dir, Options}, infinity).

-spec add_application(XrefServer, Directory) ->
                             {'ok', application()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().

add_application(Name, Dir) ->
    gen_server:call(Name, {add_application, Dir}, infinity).

-spec add_application(XrefServer, Directory, Options) ->
                             {'ok', application()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'name', application()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().

add_application(Name, Dir, Options) ->
    gen_server:call(Name, {add_application, Dir, Options}, infinity).

-type add_mod_rsn() ::
        {'file_error', file(), file_error()}
      | {'invalid_filename', term()}
      | {'invalid_options', term()}
      | {'module_clash', {module(), file(), file()}}
      | {'no_debug_info', file()}
      | beam_lib:chnk_rsn().

-spec add_module(XrefServer, File) -> 
                             {'ok', module()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      File :: file:filename(),
      Reason :: add_mod_rsn().

add_module(Name, File) ->
    gen_server:call(Name, {add_module, File}, infinity).

-spec add_module(XrefServer, File, Options) -> 
                             {'ok', module()} |
                             {'error', module(), Reason} when
      XrefServer :: xref(),
      File :: file:filename(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: add_mod_rsn().

add_module(Name, File, Options) ->
    gen_server:call(Name, {add_module, File, Options}, infinity).

-type add_dir_rsn() ::
        {'file_error', file(), file_error()}
      | {'invalid_filename', term()}
      | {'invalid_options', term()}
      | {'unrecognized_file', file()}
      | beam_lib:chnk_rsn().

-spec add_directory(XrefServer, Directory) ->
                           {'ok', Modules} |
                           {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Modules :: [module()],
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | add_dir_rsn().
      
add_directory(Name, Dir) ->
    gen_server:call(Name, {add_directory, Dir}, infinity).

-spec add_directory(XrefServer, Directory, Options) ->
                           {'ok', Modules} |
                           {'error', module(), Reason} when
      XrefServer :: xref(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'recurse', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'recurse'
              | 'verbose'
              | 'warnings',
      Modules :: [module()],
      Reason :: add_dir_rsn().

add_directory(Name, Dir, Options) ->
    gen_server:call(Name, {add_directory, Dir, Options}, infinity).

-spec replace_module(XrefServer, Module, File) ->
                                 {'ok', Module} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Module :: module(),
      File :: file(),
      Reason :: {'module_mismatch', Module, ReadModule :: module()}
              | {'no_such_module', Module}
              | add_mod_rsn().

replace_module(Name, Module, File) ->
    gen_server:call(Name, {replace_module, Module, File}, infinity).

-spec replace_module(XrefServer, Module, File, Options) ->
                                 {'ok', Module} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Module :: module(),
      File :: file(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'verbose'
              | 'warnings',
      Reason :: {'module_mismatch', Module, ReadModule :: module()}
              | {'no_such_module', Module}
              | add_mod_rsn().

replace_module(Name, Module, File, Options) ->
    gen_server:call(Name, {replace_module, Module, File, Options}, infinity).

-spec replace_application(XrefServer, Application, Directory) ->
                                 {'ok', Application} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Application :: application(),
      Directory :: directory(),
      Reason :: {'no_such_application', Application}
              | add_dir_rsn().

replace_application(Name, App, Dir) ->
    gen_server:call(Name, {replace_application, App, Dir}, infinity).

-spec replace_application(XrefServer, Application, Directory, Options) ->
                                 {'ok', Application} |
                                 {'error', module(), Reason} when
      XrefServer :: xref(),
      Application :: application(),
      Directory :: directory(),
      Options :: Option | [Option],
      Option :: {'builtins', boolean()}
              | {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'builtins'
              | 'verbose'
              | 'warnings',
      Reason :: {'application_clash', {application(), directory(), directory()}}
              | {'no_such_application', Application}
              | add_dir_rsn().

replace_application(Name, App, Dir, Options) ->
    gen_server:call(Name, {replace_application, App, Dir, Options}, infinity).

-spec remove_module(XrefServer, Modules) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Modules :: module() | [module()],
      Reason :: {'no_such_module', module()}.

remove_module(Name, Mod) ->
    gen_server:call(Name, {remove_module, Mod}, infinity).

-spec remove_application(XrefServer, Applications) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Applications :: application() | [application()],
      Reason :: {'no_such_application', application()}.

remove_application(Name, App) ->
    gen_server:call(Name, {remove_application, App}, infinity).

-spec remove_release(XrefServer, Releases) ->
                                'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Releases :: release() | [release()],
      Reason :: {'no_such_release', release()}.

remove_release(Name, Rel) ->
    gen_server:call(Name, {remove_release, Rel}, infinity).

-spec get_library_path(XrefServer) -> {'ok', LibraryPath} when
      XrefServer :: xref(),
      LibraryPath :: library_path().

get_library_path(Name) ->
    gen_server:call(Name, get_library_path, infinity).

-spec set_library_path(XrefServer, LibraryPath) ->
                              'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      LibraryPath :: library_path(),
      Reason :: {'file_error', file(), file_error()}
              | {'invalid_options', term()}
              | {'invalid_path', term()}.

set_library_path(Name, Path) ->
    gen_server:call(Name, {set_library_path, Path}, infinity).

-spec set_library_path(XrefServer, LibraryPath, Options) ->
                              'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      LibraryPath :: library_path(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Reason :: {'invalid_options', term()}
              | {'invalid_path', term()}.

set_library_path(Name, Path, Options) ->
    gen_server:call(Name, {set_library_path, Path, Options}, infinity).

-type info() :: {'application', Application :: [application()]}
              | {'builtins', boolean()}
              | {'directory', directory()}
              | {'library_path', library_path()}
              | {'mode', mode()}
              | {'no_analyzed_modules', non_neg_integer()}
              | {'no_applications', non_neg_integer()}
              | {'no_calls', {NoResolved :: non_neg_integer(),
                              NoUnresolved :: non_neg_integer()}}
              | {'no_function_calls',
                 {NoLocal :: non_neg_integer(),
                  NoResolvedExternal :: non_neg_integer(),
                  NoUnresolved :: non_neg_integer()}}
              | {'no_functions', {NoLocal :: non_neg_integer(),
                                  NoExternal :: non_neg_integer()}}
              | {'no_inter_function_calls', non_neg_integer()}
              | {'no_releases', non_neg_integer()}
              | {'release', Release :: [release()]}
              | {'version', Version :: [non_neg_integer()]}.

-spec info(XrefServer) -> [Info] when
      XrefServer :: xref(),
      Info :: info().

info(Name) ->
    gen_server:call(Name, info, infinity).

-spec info(XrefServer, Category) ->
                  [{Item, [Info]}] |
                  {'error', module(), {'no_such_info', Category}} when
      XrefServer :: xref(),
      Category :: 'modules' | 'applications' | 'releases' | 'libraries',
      Item :: module() | application() | release() | library(),
      Info :: info().

info(Name, What) ->
    gen_server:call(Name, {info, What}, infinity).

-spec info(XrefServer, Category, Items) -> 
                  [{Item, [Info]}] |
                  {'error', module(), Reason} when
      XrefServer :: xref(),
      Category :: 'modules' | 'applications' | 'releases' | 'libraries',
      Items :: Item | [Item],
      Item :: module() | application() | release() | library(),
      Info :: info(),
      Reason :: {'no_such_application', Item}
              | {'no_such_info', Category}
              | {'no_such_library', Item}
              | {'no_such_module', Item}
              | {'no_such_release', Item}.

info(Name, What, Qual) ->
    gen_server:call(Name, {info, What, Qual}, infinity).

-spec update(XrefServer) ->
                    {'ok', Modules} |
                    {'error', module(), Reason} when
      XrefServer :: xref(),
      Modules :: [module()],
      Reason :: {'module_mismatch', module(), ReadModule :: module()}
              | add_mod_rsn(). % except 'invalid_options'

update(Name) ->
    gen_server:call(Name, update, infinity).

-spec update(XrefServer, Options) ->
                    {'ok', Modules} |
                    {'error', module(), Reason} when
      XrefServer :: xref(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()}
              | {'warnings', boolean()}
              | 'verbose'
              | 'warnings',
      Modules :: [module()],
      Reason :: {'module_mismatch', module(), ReadModule :: module()}
              | add_mod_rsn().

update(Name, Options) ->
    gen_server:call(Name, {update, Options}, infinity).

-spec forget(XrefServer) -> 'ok' when
      XrefServer :: xref().

forget(Name) ->
    gen_server:call(Name, forget, infinity).

-spec forget(XrefServer, Variables) -> 'ok' | {'error', module(), Reason} when
      XrefServer :: xref(),
      Variables :: variable() | [variable()],
      Reason :: {'not_user_variable', term()}.

forget(Name, Variable) ->
    gen_server:call(Name, {forget, Variable}, infinity).

-spec variables(XrefServer) -> {'ok', [VariableInfo]} when
      XrefServer :: xref(),
      VariableInfo :: {'predefined', [variable()]}
                    | {'user', [variable()]}.

variables(Name) ->
    gen_server:call(Name, variables, infinity).

-spec variables(XrefServer, Options) -> {'ok', [VariableInfo]} when
      XrefServer :: xref(),
      Options :: Option | [Option],
      Option :: 'predefined'
              | 'user'
              | {'verbose', boolean()}
              | 'verbose',
      VariableInfo :: {'predefined', [variable()]}
                    | {'user', [variable()]}.

variables(Name, Options) ->
    gen_server:call(Name, {variables, Options}, infinity).

-spec analyse(XrefServer, Analysis) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyse(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

-spec analyse(XrefServer, Analysis, Options) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyse(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

-type app_spec() :: application() | [application()].
-type depr_flag() :: 'next_version'
                   | 'next_major_release'
                   | 'eventually'.
-type func_spec() :: xmfa() | [xmfa()].
-type mod_spec() :: module() | [module()].
-type rel_spec() :: release() | [release()].
-type analysis() :: 'undefined_function_calls'
                  | 'undefined_functions'
                  | 'locals_not_used'
                  | 'exports_not_used'
                  | 'deprecated_function_calls'
                  | {'deprecated_function_calls', DeprFlag :: depr_flag()}
                  | 'deprecated_functions'
                  | {'deprecated_functions', DeprFlag :: depr_flag()}
                  | {'call', FuncSpec :: func_spec()}
                  | {'use', FuncSpec :: func_spec()}
                  | {'module_call', ModSpec :: mod_spec()}
                  | {'module_use', ModSpec :: mod_spec()}
                  | {'application_call', AppSpec :: app_spec()}
                  | {'application_use', AppSpec :: app_spec()}
                  | {'release_call', RelSpec :: rel_spec()}
                  | {'release_use', RelSpec :: rel_spec()}.

-type analyze_rsn() :: {'invalid_options', term()}
                     | {'parse_error', string_position(), term()}
                     | {'unavailable_analysis', term()}
                     | {'unknown_analysis', term()}
                     | {'unknown_constant', string()}
                     | {'unknown_variable', variable()}.

-spec analyze(XrefServer, Analysis) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyze(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

-spec analyze(XrefServer, Analysis, Options) ->
                     {'ok', Answer} |
                     {'error', module(), Reason} when
      XrefServer :: xref(),
      Analysis :: analysis(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: [term()],
      Reason :: analyze_rsn().
      
analyze(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

-type component() :: [constant()].
-type define_at() :: {xmfa(), LineNumber :: non_neg_integer()}.
-type answer() :: 'false'
                | [constant()]
                | [(Call :: call()) |
                   (ComponentCall :: {component(), component()})]
                | [Component :: component()]
                | non_neg_integer()
                | [DefineAt :: define_at()]
                | [CallAt :: {funcall(), LineNumbers :: [non_neg_integer()]}]
                | [AllLines :: {{define_at(), define_at()},
                                LineNumbers :: [non_neg_integer()]}].
-type q_rsn() :: {'invalid_options', term()}
               | {'parse_error', string_position(), term()}
               | {'type_error', string()}
               | {'type_mismatch', string(), string()}
               | {'unknown_analysis', term()}
               | {'unknown_constant', string()}
               | {'unknown_variable', variable()}
               | {'variable_reassigned', string()}.

-spec q(XrefServer, Query) ->
               {'ok', Answer} |
               {'error', module(), Reason} when
      XrefServer :: xref(),
      Query :: string() | atom(),
      Answer :: answer(),
      Reason :: q_rsn().

q(Name, Q) ->
    gen_server:call(Name, {qry, Q}, infinity).

-spec q(XrefServer, Query, Options) ->
               {'ok', Answer} |
               {'error', module(), Reason} when
      XrefServer :: xref(),
      Query :: string() | atom(),
      Options :: Option | [Option],
      Option :: {'verbose', boolean()} | 'verbose',
      Answer :: answer(),
      Reason :: q_rsn().

q(Name, Q, Options) ->
    gen_server:call(Name, {qry, Q, Options}, infinity).

-spec get_default(XrefServer) -> [{Option, Value}] when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean().

get_default(Name) ->
    gen_server:call(Name, get_default, infinity).

-spec get_default(XrefServer, Option) ->
                         {'ok', Value} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      Reason :: {'invalid_options', term()}. % 'invalid_option'

get_default(Name, Option) ->
    gen_server:call(Name, {get_default, Option}, infinity).

-spec set_default(XrefServer, OptionValues) ->
                         'ok' |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      OptionValues :: OptionValue | [OptionValue],
      OptionValue :: {Option, Value},
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      Reason :: {'invalid_options', term()}.

set_default(Name, OptionValues) ->
    gen_server:call(Name, {set_default, OptionValues}, infinity).

-spec set_default(XrefServer, Option, Value) ->
                         {'ok', OldValue} |
                         {'error', module(), Reason} when
      XrefServer :: xref(),
      Option :: 'builtins' | 'recurse' | 'verbose' | 'warnings',
      Value :: boolean(),
      OldValue :: boolean(),
      Reason :: {'invalid_options', term()}.

set_default(Name, Option, Value) ->
    gen_server:call(Name, {set_default, Option, Value}, infinity).

-spec format_error(Error) -> io_lib:chars() when
      Error :: {'error', module(), Reason :: term()}.

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%%%----------------------------------------------------------------------
%%%Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Args) ->
    case xref_base:new(Args) of
	{ok, S} ->
	    {ok, S};
	{error, _Module, Reason} ->
	    {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({add_release, Dir}, _From, State) ->
    case xref_base:add_release(State, Dir) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_release, Dir, Options}, _From, State) ->
    case xref_base:add_release(State, Dir, Options) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir}, _From, State) ->
    case xref_base:add_application(State, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir, Options}, _From, State) ->
    case xref_base:add_application(State, Dir, Options) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_module, File}, _From, State) ->
    case xref_base:add_module(State, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({add_module, File, Options}, _From, State) ->
    case xref_base:add_module(State, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({replace_application, Appl, Dir}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_application, Appl, Dir, Opts}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir, Opts) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_module, Mod}, _From, State) ->
    case xref_base:remove_module(State, Mod) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_application, Appl}, _From, State) ->
    case xref_base:remove_application(State, Appl) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_release, Rel}, _From, State) ->
    case xref_base:remove_release(State, Rel) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir}, _From, State) ->
    case xref_base:add_directory(State, Dir) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir, Options}, _From, State) ->
    case xref_base:add_directory(State, Dir, Options) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(get_library_path, _From, State) ->
    Path = xref_base:get_library_path(State),
    {reply, Path, State};
handle_call({set_library_path, Path}, _From, State) ->
    case xref_base:set_library_path(State, Path) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_library_path, Path, Options}, _From, State) ->
    case xref_base:set_library_path(State, Path, Options) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File}, _From, State) ->
    case xref_base:replace_module(State, Module, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File, Options}, _From, State) ->
    case xref_base:replace_module(State, Module, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(info, _From, State) ->
    {reply, xref_base:info(State), State};
handle_call({info, What}, _From, State) ->
    {reply, xref_base:info(State, What), State};
handle_call({info, What, Qual}, _From, State) ->
    {reply, xref_base:info(State, What, Qual), State};
handle_call(update, _From, State) ->
    case xref_base:update(State) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({update, Options}, _From, State) ->
    case xref_base:update(State, Options) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(forget, _From, State) ->
    {ok, NewState} = xref_base:forget(State),
    {reply, ok, NewState};
handle_call({forget, Variable}, _From, State) ->
    case xref_base:forget(State, Variable) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(variables, _From, State) ->
    %% The reason the ok-Error pattern is broken for variables, q and
    %% analyze is that the state may have been updated even if an
    %% error occurs.
    {Reply, NewState} = xref_base:variables(State),
    {reply, Reply, NewState};
handle_call({variables, Options}, _From, State) ->
    {Reply, NewState} = xref_base:variables(State, Options),
    {reply, Reply, NewState};
handle_call({analyze, What}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What),
    {reply, unsetify(Reply), NewState};
handle_call({analyze, What, Options}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What, Options),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q, Options}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q, Options),
    {reply, unsetify(Reply), NewState};
handle_call(get_default, _From, State) ->
    Reply = xref_base:get_default(State),
    {reply, Reply, State};
handle_call({get_default, Option}, _From, State) ->
    Reply = xref_base:get_default(State, Option),
    {reply, Reply, State};
handle_call({set_default, OptionValues}, _From, State) ->
    case xref_base:set_default(State, OptionValues) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_default, Option, Value}, _From, State) ->
    case xref_base:set_default(State, Option, Value) of
	{ok, OldValue, NewState} ->
	    {reply, {ok, OldValue}, NewState};
	Error ->
	    {reply, Error, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2 
%% Returns: {noreply, State} | 
%% {noreply, State, Timeout} | 
%% {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_functions_analysis(FFun) ->
    {ok, State} = xref_base:new(),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_function_calls),
    {Unused, State5} = do_analysis(State4, locals_not_used),
    {Deprecated, _} = do_analysis(State5, deprecated_function_calls),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}, 
     {unused,to_external(Unused)}].

do_modules_analysis(FFun) ->
    {ok, State} = xref_base:new({xref_mode, modules}),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_functions),
    {Deprecated, _} = do_analysis(State4, deprecated_functions),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}].

do_analysis(State, Analysis) ->
    case xref_base:analyze(State, Analysis) of
	{{ok, Reply}, NewState} ->
	    {Reply, NewState};
	{Error, _} ->
	    throw(Error)
    end.

unsetify(Reply={ok, X}) ->
    case is_sofs_set(X) of
	true -> {ok, to_external(X)};
	false -> Reply
    end;
unsetify(Reply) ->
    Reply.
