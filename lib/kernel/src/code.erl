%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(code).

-include_lib("kernel/include/logger.hrl").

%% This is the interface module to the code server. It also contains
%% some implementation details.  See also related modules: code_*.erl
%% in this directory.

-export([objfile_extension/0, 
	 set_path/1, 
	 get_path/0, 
	 load_file/1,
	 ensure_loaded/1,
	 ensure_modules_loaded/1,
	 load_abs/1,
	 load_abs/2,
	 load_binary/3,
	 load_native_partial/2,
	 load_native_sticky/3,
	 atomic_load/1,
	 prepare_loading/1,
	 finish_loading/1,
	 delete/1,
	 purge/1,
	 soft_purge/1,
	 is_loaded/1,
	 all_loaded/0,
	 stop/0,
	 root_dir/0,
	 lib_dir/0,
	 lib_dir/1,
	 lib_dir/2,
	 compiler_dir/0,
	 priv_dir/1,
	 stick_dir/1,
	 unstick_dir/1,
	 stick_mod/1,
	 unstick_mod/1,
	 is_sticky/1,
	 get_object_code/1,
	 add_path/1,
	 add_pathsz/1,
	 add_paths/1,
	 add_pathsa/1,
	 add_patha/1,
	 add_pathz/1,
	 del_path/1,
	 replace_path/2,
	 rehash/0,
	 start_link/0,
	 which/1,
	 where_is_file/1,
	 where_is_file/2,
	 set_primary_archive/4,
	 clash/0,
         module_status/1,
         modified_modules/0,
         get_mode/0]).

-deprecated({rehash,0,next_major_release}).

-export_type([load_error_rsn/0, load_ret/0]).
-export_type([prepared_code/0]).

-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------------
%% Some types for basic exported functions of this module
%%----------------------------------------------------------------------------

-type load_error_rsn() :: 'badfile'
                        | 'nofile'
                        | 'not_purged'
                        | 'on_load_failure'
                        | 'sticky_directory'.
-type load_ret() :: {'error', What :: load_error_rsn()}
                  | {'module', Module :: module()}.
-type loaded_ret_atoms() :: 'cover_compiled' | 'preloaded'.
-type loaded_filename() :: (Filename :: file:filename()) | loaded_ret_atoms().

-define(PREPARED, '$prepared$').
-opaque prepared_code() ::
	  {?PREPARED,[{module(),{binary(),string(),_}}]}.


%%% BIFs

-export([get_chunk/2, is_module_native/1, make_stub_module/3, module_md5/1]).

-spec get_chunk(Bin, Chunk) ->
                       binary() | undefined when
      Bin :: binary(),
      Chunk :: string().

get_chunk(_, _) ->
    erlang:nif_error(undef).

-spec is_module_native(Module) -> true | false | undefined when
      Module :: module().

is_module_native(_) ->
    erlang:nif_error(undef).

-spec make_stub_module(LoaderState, Beam, Info) -> module() when
      LoaderState :: binary(),
      Beam :: binary(),
      Info :: {list(), list(), binary()}.

make_stub_module(_, _, _) ->
    erlang:nif_error(undef).

-spec module_md5(binary()) -> binary() | undefined.

module_md5(_) ->
    erlang:nif_error(undef).

%%% End of BIFs

%%----------------------------------------------------------------------------
%% User interface
%%----------------------------------------------------------------------------

-spec objfile_extension() -> nonempty_string().
objfile_extension() ->
    init:objfile_extension().

-spec load_file(Module) -> load_ret() when
      Module :: module().
load_file(Mod) when is_atom(Mod) ->
    call({load_file,Mod}).

-spec ensure_loaded(Module) -> {module, Module} | {error, What} when
      Module :: module(),
      What :: embedded | badfile | nofile | on_load_failure.
ensure_loaded(Mod) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
        true -> {module, Mod};
        false -> call({ensure_loaded,Mod})
    end.

%% XXX File as an atom is allowed only for backwards compatibility.
-spec load_abs(Filename) -> load_ret() when
      Filename :: file:filename().
load_abs(File) when is_list(File); is_atom(File) ->
    Mod = list_to_atom(filename:basename(File)),
    call({load_abs,File,Mod}).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_abs(Filename :: loaded_filename(), Module :: module()) -> load_ret().
load_abs(File, M) when (is_list(File) orelse is_atom(File)), is_atom(M) ->
    call({load_abs,File,M}).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_binary(Module, Filename, Binary) ->
                         {module, Module} | {error, What} when
      Module :: module(),
      Filename :: loaded_filename(),
      Binary :: binary(),
      What :: badarg | load_error_rsn().
load_binary(Mod, File, Bin)
  when is_atom(Mod), (is_list(File) orelse is_atom(File)), is_binary(Bin) ->
    call({load_binary,Mod,File,Bin}).

-spec load_native_partial(Module :: module(), Binary :: binary()) -> load_ret().
load_native_partial(Mod, Bin) when is_atom(Mod), is_binary(Bin) ->
    call({load_native_partial,Mod,Bin}).

-spec load_native_sticky(Module :: module(), Binary :: binary(), WholeModule :: 'false' | binary()) -> load_ret().
load_native_sticky(Mod, Bin, WholeModule)
  when is_atom(Mod), is_binary(Bin),
       (is_binary(WholeModule) orelse WholeModule =:= false) ->
    call({load_native_sticky,Mod,Bin,WholeModule}).

-spec delete(Module) -> boolean() when
      Module :: module().
delete(Mod) when is_atom(Mod) -> call({delete,Mod}).

-spec purge(Module) -> boolean() when
      Module :: module().
purge(Mod) when is_atom(Mod) -> call({purge,Mod}).

-spec soft_purge(Module) -> boolean() when
      Module :: module().
soft_purge(Mod) when is_atom(Mod) -> call({soft_purge,Mod}).

-spec is_loaded(Module) -> {'file', Loaded} | false when
      Module :: module(),
      Loaded :: loaded_filename().
is_loaded(Mod) when is_atom(Mod) -> call({is_loaded,Mod}).

-spec get_object_code(Module) -> {Module, Binary, Filename} | error when
      Module :: module(),
      Binary :: binary(),
      Filename :: file:filename().
get_object_code(Mod) when is_atom(Mod) -> call({get_object_code, Mod}).

-spec all_loaded() -> [{Module, Loaded}] when
      Module :: module(),
      Loaded :: loaded_filename().
all_loaded() -> call(all_loaded).

-spec stop() -> no_return().
stop() -> call(stop).

-spec root_dir() -> file:filename().
root_dir() -> call({dir,root_dir}).

-spec lib_dir() -> file:filename().
lib_dir() -> call({dir,lib_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-spec lib_dir(Name) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom().
lib_dir(App) when is_atom(App) ; is_list(App) -> call({dir,{lib_dir,App}}).

-spec lib_dir(Name, SubDir) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom(),
      SubDir :: atom().
lib_dir(App, SubDir) when is_atom(App), is_atom(SubDir) -> call({dir,{lib_dir,App,SubDir}}).

-spec compiler_dir() -> file:filename().
compiler_dir() -> call({dir,compiler_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-spec priv_dir(Name) -> file:filename() | {'error', 'bad_name'} when
      Name :: atom().
priv_dir(App) when is_atom(App) ; is_list(App) -> call({dir,{priv_dir,App}}).

-spec stick_dir(Dir) -> 'ok' | 'error' when
      Dir :: file:filename().
stick_dir(Dir) when is_list(Dir) -> call({stick_dir,Dir}).

-spec unstick_dir(Dir) -> 'ok' | 'error' when
      Dir :: file:filename().
unstick_dir(Dir) when is_list(Dir) -> call({unstick_dir,Dir}).

-spec stick_mod(Module :: module()) -> 'true'.
stick_mod(Mod) when is_atom(Mod) -> call({stick_mod,Mod}).

-spec unstick_mod(Module :: module()) -> 'true'.
unstick_mod(Mod) when is_atom(Mod) -> call({unstick_mod,Mod}).

-spec is_sticky(Module) -> boolean() when
      Module :: module().
is_sticky(Mod) when is_atom(Mod) -> call({is_sticky,Mod}).

-spec set_path(Path) -> 'true' | {'error', What} when
      Path :: [Dir :: file:filename()],
      What :: 'bad_directory'.
set_path(PathList) when is_list(PathList) -> call({set_path,PathList}).

-spec get_path() -> Path when
      Path :: [Dir :: file:filename()].
get_path() -> call(get_path).

-type add_path_ret() :: 'true' | {'error', 'bad_directory'}.
-spec add_path(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_path(Dir) when is_list(Dir) -> call({add_path,last,Dir}).

-spec add_pathz(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_pathz(Dir) when is_list(Dir) -> call({add_path,last,Dir}).

-spec add_patha(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_patha(Dir) when is_list(Dir) -> call({add_path,first,Dir}).

-spec add_paths(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_paths(Dirs) when is_list(Dirs) -> call({add_paths,last,Dirs}).

-spec add_pathsz(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsz(Dirs) when is_list(Dirs) -> call({add_paths,last,Dirs}).

-spec add_pathsa(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsa(Dirs) when is_list(Dirs) -> call({add_paths,first,Dirs}).

-spec del_path(NameOrDir) -> boolean() | {'error', What} when
      NameOrDir :: Name | Dir,
      Name :: atom(),
      Dir :: file:filename(),
      What :: 'bad_name'.
del_path(Name) when is_list(Name) ; is_atom(Name) -> call({del_path,Name}).

-spec replace_path(Name, Dir) -> 'true' | {'error', What} when
      Name:: atom(),
      Dir :: file:filename(),
      What :: 'bad_directory' | 'bad_name' | {'badarg',_}.
replace_path(Name, Dir) when (is_atom(Name) orelse is_list(Name)),
			     (is_atom(Dir) orelse is_list(Dir)) ->
    call({replace_path,Name,Dir}).

-spec rehash() -> 'ok'.
rehash() ->
    cache_warning(),
    ok.

-spec get_mode() -> 'embedded' | 'interactive'.
get_mode() -> call(get_mode).

%%%
%%% Loading of several modules in parallel.
%%%

-spec ensure_modules_loaded([Module]) ->
   'ok' | {'error',[{Module,What}]} when
      Module :: module(),
      What :: badfile | nofile | on_load_failure.

ensure_modules_loaded(Modules) when is_list(Modules) ->
    case prepare_ensure(Modules, []) of
	Ms when is_list(Ms) ->
	    ensure_modules_loaded_1(Ms);
	error ->
	    error(function_clause, [Modules])
    end.

ensure_modules_loaded_1(Ms0) ->
    Ms = lists:usort(Ms0),
    {Prep,Error0} = load_mods(Ms),
    {OnLoad,Normal} = partition_on_load(Prep),
    Error1 = case finish_loading(Normal, true) of
		 ok -> Error0;
		 {error,Err} -> Err ++ Error0
	     end,
    ensure_modules_loaded_2(OnLoad, Error1).

ensure_modules_loaded_2([{M,_}|Ms], Errors) ->
    case ensure_loaded(M) of
	{module,M} ->
	    ensure_modules_loaded_2(Ms, Errors);
	{error,Err} ->
	    ensure_modules_loaded_2(Ms, [{M,Err}|Errors])
    end;
ensure_modules_loaded_2([], []) ->
    ok;
ensure_modules_loaded_2([], [_|_]=Errors) ->
    {error,Errors}.

prepare_ensure([M|Ms], Acc) when is_atom(M) ->
    case erlang:module_loaded(M) of
	true ->
	    prepare_ensure(Ms, Acc);
	false ->
	    prepare_ensure(Ms, [M|Acc])
    end;
prepare_ensure([], Acc) ->
    Acc;
prepare_ensure(_, _) ->
    error.

-spec atomic_load(Modules) -> 'ok' | {'error',[{Module,What}]} when
      Modules :: [Module | {Module, Filename, Binary}],
      Module :: module(),
      Filename :: file:filename(),
      Binary :: binary(),
      What :: 'badfile' | 'nofile' | 'on_load_not_allowed' | 'duplicated' |
	      'not_purged' | 'sticky_directory' | 'pending_on_load'.

atomic_load(Modules) ->
    case do_prepare_loading(Modules) of
	{ok,Prep} ->
	    finish_loading(Prep, false);
	{error,_}=Error ->
	    Error;
	badarg ->
	    error(function_clause, [Modules])
    end.

-spec prepare_loading(Modules) ->
           {'ok',Prepared} | {'error',[{Module,What}]} when
      Modules :: [Module | {Module, Filename, Binary}],
      Module :: module(),
      Filename :: file:filename(),
      Binary :: binary(),
      Prepared :: prepared_code(),
      What :: 'badfile' | 'nofile' | 'on_load_not_allowed' | 'duplicated'.

prepare_loading(Modules) ->
    case do_prepare_loading(Modules) of
	{ok,Prep} ->
	    {ok,{?PREPARED,Prep}};
	{error,_}=Error ->
	    Error;
	badarg ->
	    error(function_clause, [Modules])
    end.

-spec finish_loading(Prepared) -> 'ok' | {'error',[{Module,What}]} when
      Prepared :: prepared_code(),
      Module :: module(),
      What :: 'not_purged' | 'sticky_directory' | 'pending_on_load'.

finish_loading({?PREPARED,Prepared}=Arg) when is_list(Prepared) ->
    case verify_prepared(Prepared) of
	ok ->
	    finish_loading(Prepared, false);
	error ->
	    error(function_clause, [Arg])
    end.

partition_load([Item|T], Bs, Ms) ->
    case Item of
	{M,File,Bin} when is_atom(M) andalso
			  is_list(File) andalso
			  is_binary(Bin) ->
	    partition_load(T, [Item|Bs], Ms);
	M when is_atom(M) ->
	    partition_load(T, Bs, [Item|Ms]);
	_ ->
	    error
    end;
partition_load([], Bs, Ms) ->
    {Bs,Ms}.

do_prepare_loading(Modules) ->
    case partition_load(Modules, [], []) of
	{ModBins,Ms} ->
	    case prepare_loading_1(ModBins, Ms) of
		{error,_}=Error ->
		    Error;
		Prep when is_list(Prep) ->
		    {ok,Prep}
	    end;
	error ->
	    badarg
    end.

prepare_loading_1(ModBins, Ms) ->
    %% erlang:finish_loading/1 *will* detect duplicates.
    %% However, we want to detect all errors that can be detected
    %% by only examining the input data before call the LastAction
    %% fun.
    case prepare_check_uniq(ModBins, Ms) of
	ok ->
	    prepare_loading_2(ModBins, Ms);
	Error ->
	    Error
    end.

prepare_loading_2(ModBins, Ms) ->
    {Prep0,Error0} = load_bins(ModBins),
    {Prep1,Error1} = load_mods(Ms),
    case Error0 ++ Error1 of
	[] ->
	    prepare_loading_3(Prep0 ++ Prep1);
	[_|_]=Error ->
	    {error,Error}
    end.

prepare_loading_3(Prep) ->
    case partition_on_load(Prep) of
	{[_|_]=OnLoad,_} ->
	    Error = [{M,on_load_not_allowed} || {M,_} <- OnLoad],
	    {error,Error};
	{[],_} ->
	    Prep
    end.

prepare_check_uniq([{M,_,_}|T], Ms) ->
    prepare_check_uniq(T, [M|Ms]);
prepare_check_uniq([], Ms) ->
    prepare_check_uniq_1(lists:sort(Ms), []).

prepare_check_uniq_1([M|[M|_]=Ms], Acc) ->
    prepare_check_uniq_1(Ms, [{M,duplicated}|Acc]);
prepare_check_uniq_1([_|Ms], Acc) ->
    prepare_check_uniq_1(Ms, Acc);
prepare_check_uniq_1([], []) ->
    ok;
prepare_check_uniq_1([], [_|_]=Errors) ->
    {error,Errors}.

partition_on_load(Prep) ->
    P = fun({_,{PC,_,_}}) ->
		erlang:has_prepared_code_on_load(PC)
	end,
    lists:partition(P, Prep).

verify_prepared([{M,{Prep,Name,_Native}}|T])
  when is_atom(M), is_list(Name) ->
    try erlang:has_prepared_code_on_load(Prep) of
	false ->
	    verify_prepared(T);
	_ ->
	    error
    catch
	error:_ ->
	    error
    end;
verify_prepared([]) ->
    ok;
verify_prepared(_) ->
    error.

finish_loading(Prepared0, EnsureLoaded) ->
    Prepared = [{M,{Bin,File}} || {M,{Bin,File,_}} <- Prepared0],
    Native0 = [{M,Code} || {M,{_,_,Code}} <- Prepared0,
			   Code =/= undefined],
    case call({finish_loading,Prepared,EnsureLoaded}) of
	ok ->
	    finish_loading_native(Native0);
	{error,Errors}=E when EnsureLoaded ->
	    S0 = sofs:relation(Errors),
	    S1 = sofs:domain(S0),
	    R0 = sofs:relation(Native0),
	    R1 = sofs:drestriction(R0, S1),
	    Native = sofs:to_external(R1),
	    finish_loading_native(Native),
	    E;
	{error,_}=E ->
	    E
    end.

finish_loading_native([{Mod,Code}|Ms]) ->
    _ = load_native_partial(Mod, Code),
    finish_loading_native(Ms);
finish_loading_native([]) ->
    ok.

load_mods([]) ->
    {[],[]};
load_mods(Mods) ->
    Path = get_path(),
    F = prepare_loading_fun(),
    {ok,{Succ,Error0}} = erl_prim_loader:get_modules(Mods, F, Path),
    Error = [case E of
		 badfile -> {M,E};
		 _ -> {M,nofile}
	     end || {M,E} <- Error0],
    {Succ,Error}.

load_bins([]) ->
    {[],[]};
load_bins(BinItems) ->
    F = prepare_loading_fun(),
    do_par(F, BinItems).

-type prep_fun_type() :: fun((module(), file:filename(), binary()) ->
				    {ok,_} | {error,_}).

-spec prepare_loading_fun() -> prep_fun_type().

prepare_loading_fun() ->
    GetNative = get_native_fun(),
    fun(Mod, FullName, Beam) ->
	    case erlang:prepare_loading(Mod, Beam) of
		{error,_}=Error ->
		    Error;
		Prepared ->
		    {ok,{Prepared,FullName,GetNative(Beam)}}
	    end
    end.

get_native_fun() ->
    Architecture = erlang:system_info(hipe_architecture),
    try hipe_unified_loader:chunk_name(Architecture) of
	ChunkTag ->
	    fun(Beam) -> code:get_chunk(Beam, ChunkTag) end
    catch _:_ ->
	    fun(_) -> undefined end
    end.

do_par(Fun, L) ->
    {_,Ref} = spawn_monitor(do_par_fun(Fun, L)),
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Res
    end.

-spec do_par_fun(prep_fun_type(), list()) -> fun(() -> no_return()).

do_par_fun(Fun, L) ->
    fun() ->
	    _ = [spawn_monitor(do_par_fun_2(Fun, Item)) ||
		    Item <- L],
	    exit(do_par_recv(length(L), [], []))
    end.

-spec do_par_fun_2(prep_fun_type(),
		   {module(),file:filename(),binary()}) ->
			  fun(() -> no_return()).

do_par_fun_2(Fun, Item) ->
    fun() ->
	    {Mod,Filename,Bin} = Item,
	    try Fun(Mod, Filename, Bin) of
		{ok,Res} ->
		    exit({good,{Mod,Res}});
		{error,Error} ->
		    exit({bad,{Mod,Error}})
	    catch
		_:Error ->
		    exit({bad,{Mod,Error}})
	    end
    end.

do_par_recv(0, Good, Bad) ->
    {Good,Bad};
do_par_recv(N, Good, Bad) ->
    receive
	{'DOWN',_,process,_,{good,Res}} ->
	    do_par_recv(N-1, [Res|Good], Bad);
	{'DOWN',_,process,_,{bad,Res}} ->
	    do_par_recv(N-1, Good, [Res|Bad])
    end.

%%-----------------------------------------------------------------

call(Req) ->
    code_server:call(Req).

-spec start_link() -> {'ok', pid()}.
start_link() ->
    do_start().
    
%%-----------------------------------------------------------------
%% In the init phase, code must not use any modules not yet loaded,
%% either pre_loaded (e.g. init) or first in the script (e.g.
%% erlang).  Therefore, keep the modules used in init phase to a
%% minimum, and make sure they are loaded before init is called.
%% Try to call these modules from do_start instead.
%% file is used in init - this is ok; file has been started before
%% us, so the module is loaded.
%%-----------------------------------------------------------------

do_start() ->
    maybe_warn_for_cache(),
    load_code_server_prerequisites(),

    {ok,[[Root0]]} = init:get_argument(root),
    Mode = start_get_mode(),
    Root = filename:join([Root0]),	    % Normalize.
    Res = code_server:start_link([Root,Mode]),

    maybe_stick_dirs(Mode),

    %% Quietly load native code for all modules loaded so far.
    Architecture = erlang:system_info(hipe_architecture),
    load_native_code_for_all_loaded(Architecture),
    Res.

%% Make sure that all modules that the code_server process calls
%% (directly or indirectly) are loaded. Otherwise the code_server
%% process will deadlock.

load_code_server_prerequisites() ->
    %% Please keep the alphabetical order.
    Needed = [binary,
	      ets,
	      filename,
	      gb_sets,
	      gb_trees,
	      hipe_unified_loader,
	      lists,
	      os,
	      unicode],
    _ = [M = M:module_info(module) || M <- Needed],
    ok.

maybe_stick_dirs(interactive) ->
    case init:get_argument(nostick) of
	{ok,[[]]} ->
	    ok;
	_ ->
	    do_stick_dirs()
    end;
maybe_stick_dirs(_) ->
    ok.

do_stick_dirs() ->
    do_s(compiler),
    do_s(stdlib),
    do_s(kernel).

do_s(Lib) ->
    case lib_dir(Lib) of
	{error, _} ->
	    ok;
	Dir ->
	    %% The return value is intentionally ignored. Missing
	    %% directories is not a fatal error. (In embedded systems,
	    %% there is usually no compiler directory.)
	    _ = stick_dir(filename:append(Dir, "ebin")),
	    ok
    end.

start_get_mode() ->
    case init:get_argument(mode) of
	{ok, [FirstMode | Rest]} ->
	    case Rest of
		[] ->
		    ok;
		_ ->
		    ?LOG_WARNING("Multiple -mode given to erl, using the first, ~p",
				 [FirstMode])
	    end,
	    case FirstMode of
		["embedded"] ->
		    embedded;
		_ ->
		    interactive
	    end;
	_ ->
	    interactive
    end.

%% Find out which version of a particular module we would
%% load if we tried to load it, unless it's already loaded.
%% In that case return the name of the file which contains
%% the loaded object code

-spec which(Module) -> Which when
      Module :: module(),
      Which :: file:filename() | loaded_ret_atoms() | non_existing.
which(Module) when is_atom(Module) ->
    case is_loaded(Module) of
	false ->
            which(Module, get_path());
	{file, File} ->
	    File
    end.

which(Module, Path) when is_atom(Module) ->
    File = atom_to_list(Module) ++ objfile_extension(),
    where_is_file(Path, File).

%% Search the code path for a specific file. Try to locate
%% it in the code path cache if possible.

-spec where_is_file(Filename) -> non_existing | Absname when
      Filename :: file:filename(),
      Absname :: file:filename().
where_is_file(File) when is_list(File) ->
    Path = get_path(),
    where_is_file(Path, File).

%% To avoid unnecessary work when looking at many modules, this also
%% accepts pairs of directories and pre-fetched contents in the path
-spec where_is_file(Path :: [Dir|{Dir,Files}], Filename :: file:filename()) ->
          'non_existing' | file:filename() when
      Dir :: file:filename(), Files :: [file:filename()].

where_is_file([], _) ->
    non_existing;
where_is_file([{Path, Files}|Tail], File) ->
    where_is_file(Tail, File, Path, Files);
where_is_file([Path|Tail], File) ->
    case erl_prim_loader:list_dir(Path) of
	{ok,Files} ->
            where_is_file(Tail, File, Path, Files);
	_Error ->
	    where_is_file(Tail, File)
    end.

where_is_file(Tail, File, Path, Files) ->
    case lists:member(File, Files) of
        true ->
            filename:append(Path, File);
        false ->
            where_is_file(Tail, File)
    end.

-spec set_primary_archive(ArchiveFile :: file:filename(),
			  ArchiveBin :: binary(),
			  FileInfo :: file:file_info(),
			  ParserFun :: fun())
			 -> 'ok' | {'error', atom()}.

set_primary_archive(ArchiveFile0, ArchiveBin, #file_info{} = FileInfo,
		    ParserFun)
  when is_list(ArchiveFile0), is_binary(ArchiveBin) ->
    ArchiveFile = filename:absname(ArchiveFile0),
    case call({set_primary_archive, ArchiveFile, ArchiveBin, FileInfo,
	       ParserFun}) of
	{ok, []} ->
	    ok;
	{ok, _Mode, Ebins} ->
	    %% Prepend the code path with the ebins found in the archive
	    Ebins2 = [filename:join([ArchiveFile, E]) || E <- Ebins],
	    add_pathsa(Ebins2); % Returns ok
	{error, _Reason} = Error ->
	    Error
    end.
    
%% Search the entire path system looking for name clashes

-spec clash() -> 'ok'.

clash() ->
    Path = get_path(),
    Struct = lists:flatten(build(Path)),
    Len = length(search(Struct)),
    io:format("** Found ~w name clashes in code paths ~n", [Len]).

%% Internal for clash/0

search([]) -> [];
search([{Dir, File} | Tail]) ->
    case lists:keyfind(File, 2, Tail) of
	false -> 
	    search(Tail);
	{Dir2, File} ->
	    io:format("** ~ts hides ~ts~n",
		      [filename:join(Dir, File),
		       filename:join(Dir2, File)]),
	    [clash | search(Tail)]
    end.

build([]) -> [];
build([Dir|Tail]) ->
    Files = filter(objfile_extension(), Dir,
		   erl_prim_loader:list_dir(Dir)),
    [decorate(Files, Dir) | build(Tail)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
    [{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, Dir, error) ->
    io:format("** Bad path can't read ~ts~n", [Dir]), [];
filter(Ext, _, {ok,Files}) -> 
    filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen, [File|Tail]) ->
    case has_ext(Ext, Extlen, File) of
	true -> [File | filter2(Ext, Extlen, Tail)];
	false -> filter2(Ext, Extlen, Tail)
    end.

has_ext(Ext, Extlen, File) ->
    L = length(File),
    case catch lists:nthtail(L - Extlen, File) of
	Ext -> true;
	_ -> false
    end.

%%%
%%% Warning for deprecated code path cache.
%%%

maybe_warn_for_cache() ->
    case init:get_argument(code_path_cache) of
	{ok, _} ->
	    cache_warning();
	error ->
	    ok
    end.

cache_warning() ->
    W = "The code path cache functionality has been removed",
    error_logger:warning_report(W).

%%%
%%% Silently load native code for all modules loaded so far.
%%%

load_native_code_for_all_loaded(undefined) ->
    ok;
load_native_code_for_all_loaded(Architecture) ->
    try hipe_unified_loader:chunk_name(Architecture) of
	ChunkTag ->
	    Loaded = all_loaded(),
	    _ = spawn(fun() -> load_all_native(Loaded, ChunkTag) end),
	    ok
    catch
	_:_ ->
	    ok
    end.

load_all_native(Loaded, ChunkTag) ->
    catch load_all_native_1(Loaded, ChunkTag).

load_all_native_1([{_,preloaded}|T], ChunkTag) ->
    load_all_native_1(T, ChunkTag);
load_all_native_1([{Mod,BeamFilename}|T], ChunkTag) ->
    case code:is_module_native(Mod) of
	false ->
	    %% prim_file is faster than file and the file server may
	    %% not be started yet.
	    {ok,Beam} = prim_file:read_file(BeamFilename),
	    case code:get_chunk(Beam, ChunkTag) of
		undefined ->
		    ok;
		NativeCode when is_binary(NativeCode) ->
		    _ = load_native_partial(Mod, NativeCode),
		    ok
	    end;
	true -> ok
    end,
    load_all_native_1(T, ChunkTag);
load_all_native_1([], _) ->
    ok.

%% Returns the status of the module in relation to object file on disk.
-spec module_status(Module :: module()) -> not_loaded | loaded | modified | removed.
module_status(Module) ->
    module_status(Module, code:get_path()).

%% Note that we don't want to go via which/1, since it doesn't look at the
%% disk contents at all if the module is already loaded.
module_status(Module, PathFiles) ->
    case code:is_loaded(Module) of
        false -> not_loaded;
        {file, preloaded} -> loaded;
        {file, cover_compiled} ->
            %% cover compilation loads directly to memory and does not
            %% create a beam file, so report 'modified' if a file exists
            case which(Module, PathFiles) of
                non_existing -> removed;
                _File -> modified
            end;
        {file, []} -> loaded;  % no beam file - generated code
        {file, OldFile} when is_list(OldFile) ->
            %% we don't care whether or not the file is in the same location
            %% as when last loaded, as long as it can be found in the path
            case which(Module, PathFiles) of
                non_existing -> removed;
                Path ->
                    case module_changed_on_disk(Module, Path) of
                        true -> modified;
                        false -> loaded
                    end
            end
    end.

%% Detects actual code changes only, e.g. to decide whether a module should
%% be reloaded; does not care about file timestamps or compilation time
module_changed_on_disk(Module, Path) ->
    MD5 = erlang:get_module_info(Module, md5),
    case erlang:system_info(hipe_architecture) of
        undefined ->
            %% straightforward, since native is not supported
            MD5 =/= beam_file_md5(Path);
        Architecture ->
            case code:is_module_native(Module) of
                true ->
                    %% MD5 is for native code, so we check only the native
                    %% code on disk, ignoring the beam code
                    MD5 =/= beam_file_native_md5(Path, Architecture);
                _ ->
                    %% MD5 is for beam code, so check only the beam code on
                    %% disk, even if the file contains native code as well
                    MD5 =/= beam_file_md5(Path)
            end
    end.

beam_file_md5(Path) ->
    case beam_lib:md5(Path) of
        {ok,{_Mod,MD5}} -> MD5;
        _ -> undefined
    end.

beam_file_native_md5(Path, Architecture) ->
    try
        get_beam_chunk(Path, hipe_unified_loader:chunk_name(Architecture))
    of
        NativeCode when is_binary(NativeCode) ->
            erlang:md5(NativeCode)
    catch
        _:_ -> undefined
    end.

get_beam_chunk(Path, Chunk) ->
    {ok, {_, [{_, Bin}]}} = beam_lib:chunks(Path, [Chunk]),
    Bin.

%% Returns a list of all modules modified on disk.
-spec modified_modules() -> [module()].
modified_modules() ->
    PathFiles = path_files(),
    [M || {M, _} <- code:all_loaded(),
          module_status(M, PathFiles) =:= modified].

%% prefetch the directory contents of code path directories
path_files() ->
    path_files(code:get_path()).

path_files([]) ->
    [];
path_files([Path|Tail]) ->
    case erl_prim_loader:list_dir(Path) of
        {ok, Files} ->
            [{Path,Files} | path_files(Tail)];
        _Error ->
            path_files(Tail)
    end.
