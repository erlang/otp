%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-include("eep48.hrl").

%% This is the interface module to the code server. It also contains
%% some implementation details.  See also related modules: code_*.erl
%% in this directory.

-export([objfile_extension/0,
	 set_path/1, set_path/2,
	 get_path/0,
	 load_file/1,
	 ensure_loaded/1,
	 ensure_modules_loaded/1,
	 load_abs/1,
	 load_abs/2,
	 load_binary/3,
	 atomic_load/1,
	 prepare_loading/1,
	 finish_loading/1,
	 delete/1,
	 purge/1,
	 soft_purge/1,
	 is_loaded/1,
	 all_loaded/0,
         all_available/0,
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
	 add_paths/1, add_paths/2,
     add_path/1, add_path/2,
	 add_pathsa/1, add_pathsa/2,
     add_pathsz/1, add_pathsz/2,
	 add_patha/1, add_patha/2,
	 add_pathz/1, add_pathz/2,
	 del_path/1,
     del_paths/1,
     clear_cache/0,
	 replace_path/2,replace_path/3,
	 start_link/0,
	 which/1,
         get_doc/1,
         get_doc/2,
	 where_is_file/1,
	 where_is_file/2,
	 set_primary_archive/4,
	 clash/0,
         module_status/0,
         module_status/1,
         modified_modules/0,
         get_mode/0]).

-removed({rehash,0,"the code path cache feature has been removed"}).
-removed({is_module_native,1,"HiPE has been removed"}).

-export_type([load_error_rsn/0, load_ret/0]).
-export_type([prepared_code/0]).
-export_type([module_status/0]).

-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------------
%% Some types for basic exported functions of this module
%%----------------------------------------------------------------------------

-define(is_cache(T), T =:= cache orelse T =:= nocache).
-type cache() :: cache | nocache.
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

-export([get_chunk/2, module_md5/1]).

-spec get_chunk(Bin, Chunk) ->
                       binary() | undefined when
      Bin :: binary(),
      Chunk :: string().

get_chunk(<<"FOR1", _/bits>>=Beam, Chunk) ->
    get_chunk_1(Beam, Chunk);
get_chunk(Beam, Chunk) ->
    %% Corrupt header or compressed module, decompress it before passing it to
    %% the loader and let the BIF signal any errors.
    get_chunk_1(try_decompress(Beam), Chunk).

get_chunk_1(Beam, Chunk) ->
    try
        erts_internal:beamfile_chunk(Beam, Chunk)
    catch
        error:Reason ->
            {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
                (catch erlang:error(new_stacktrace, [Beam, Chunk])),
            erlang:raise(error, Reason, [{Mod,get_chunk,L,Loc}|Rest])
    end.

-spec module_md5(binary()) -> binary() | undefined.

module_md5(<<"FOR1", _/bits>>=Beam) ->
    module_md5_1(Beam);
module_md5(Beam) ->
    %% Corrupt header or compressed module, decompress it before passing it to
    %% the loader and let the BIF signal any errors.
    module_md5_1(try_decompress(Beam)).

module_md5_1(Beam) ->
    try
        erts_internal:beamfile_module_md5(Beam)
    catch
        error:Reason ->
            {'EXIT',{new_stacktrace,[{Mod,_,L,Loc}|Rest]}} =
                (catch erlang:error(new_stacktrace, [Beam])),
            erlang:raise(error, Reason, [{Mod,module_md5,L,Loc}|Rest])
    end.

try_decompress(Bin0) ->
    try zlib:gunzip(Bin0) of
        Decompressed -> Decompressed
    catch
        _:_ -> Bin0
    end.

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
    case get_object_code(Mod) of
        error -> {error,nofile};
        {Mod,Binary,File} -> load_module(Mod, File, Binary, false)
    end.

-spec ensure_loaded(Module) -> {module, Module} | {error, What} when
      Module :: module(),
      What :: embedded | badfile | nofile | on_load_failure.
ensure_loaded(Mod) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
        true -> {module, Mod};
        false ->
            case call({get_object_code_for_loading, Mod}) of
                {module, Mod} -> {module, Mod};
                {error, What} -> {error, What};
                {Binary,File,Ref} ->
                    case erlang:prepare_loading(Mod, Binary) of
                        {error,_}=Error ->
                            call({load_error, Ref, Mod, Error});
                        Prepared ->
                            call({load_module, Prepared, Mod, File, false, Ref})
                    end
            end
    end.

%% XXX File as an atom is allowed only for backwards compatibility.
-spec load_abs(Filename) -> load_ret() when
      Filename :: file:filename().
load_abs(File) when is_list(File); is_atom(File) ->
    load_abs(File, list_to_atom(filename:basename(File))).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_abs(Filename :: loaded_filename(), Module :: module()) -> load_ret().
load_abs(File, M) when (is_list(File) orelse is_atom(File)), is_atom(M) ->
    case modp(File) of
        true ->
            FileName0 = lists:concat([File, objfile_extension()]),
            FileName = code_server:absname(FileName0),
            case erl_prim_loader:get_file(FileName) of
                {ok,Bin,_} ->
                    load_module(M, FileName, Bin, false);
                error ->
                    {error, nofile}
            end;
        false ->
            {error,badarg}
    end.

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_binary(Module, Filename, Binary) ->
                         {module, Module} | {error, What} when
      Module :: module(),
      Filename :: loaded_filename(),
      Binary :: binary(),
      What :: badarg | load_error_rsn().
load_binary(Mod, File, Bin)
  when is_atom(Mod), (is_list(File) orelse is_atom(File)), is_binary(Bin) ->
    case modp(File) of
        true -> load_module(Mod, File, Bin, true);
        false -> {error,badarg}
    end.

load_module(Mod, File, Bin, Purge) ->
    case erlang:prepare_loading(Mod, Bin) of
        {error,_}=Error ->
            Error;
        Prepared ->
            call({load_module, Prepared, Mod, File, Purge, false})
    end.

modp(Atom) when is_atom(Atom) -> true;
modp(List) when is_list(List) -> int_list(List);
modp(_)                       -> false.

int_list([H|T]) when is_integer(H) -> int_list(T);
int_list([_|_])                    -> false;
int_list([])                       -> true.

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
is_loaded(Mod) when is_atom(Mod) ->
    code_server:is_loaded(Mod).

-spec get_object_code(Module) -> {Module, Binary, Filename} | error when
      Module :: module(),
      Binary :: binary(),
      Filename :: file:filename().
get_object_code(Mod) when is_atom(Mod) -> call({get_object_code, Mod}).

-spec all_loaded() -> [{Module, Loaded}] when
      Module :: module(),
      Loaded :: loaded_filename().
all_loaded() -> call(all_loaded).

-spec all_available() -> [{Module, Filename, Loaded}] when
      Module :: string(),
      Filename :: loaded_filename(),
      Loaded :: boolean().
all_available() ->
    case code:get_mode() of
        interactive ->
            all_available(get_path(), #{});
        embedded ->
            all_available([], #{})
    end.
all_available([Path|Tail], Acc) ->
    case erl_prim_loader:list_dir(Path) of
        {ok, Files} ->
            all_available(Tail, all_available(Path, Files, Acc));
        _Error ->
            all_available(Tail, Acc)
    end;
all_available([], AllModules) ->
    AllLoaded = [{atom_to_list(M),Path,true} || {M,Path} <- all_loaded()],
    AllAvailable =
        maps:fold(
          fun(File, Path, Acc) ->
                  [{filename:rootname(File), filename:append(Path, File), false} | Acc]
          end, [], AllModules),
    OrderFun = fun F({A,_,_},{B,_,_}) ->
                       F(A,B);
                   F(A,B) ->
                       A =< B
               end,
    lists:umerge(OrderFun, lists:sort(OrderFun, AllLoaded), lists:sort(OrderFun, AllAvailable)).

all_available(Path, [File | T], Acc) ->
    case filename:extension(File) of
        ".beam" ->
            case maps:is_key(File, Acc) of
                false ->
                    all_available(Path, T, Acc#{ File => Path });
                true ->
                    all_available(Path, T, Acc)
            end;
        _Else ->
                    all_available(Path, T, Acc)
    end;
all_available(_Path, [], Acc) ->
    Acc.

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
is_sticky(Mod) when is_atom(Mod) ->
    code_server:is_sticky(Mod).

-type set_path_ret() :: 'true' | {'error', 'bad_directory'}.
-spec set_path(Path) -> set_path_ret() when
      Path :: [Dir :: file:filename()].
set_path(PathList) -> set_path(PathList, nocache).

-spec set_path(Path, cache()) -> set_path_ret() when
      Path :: [Dir :: file:filename()].
set_path(PathList, Cache) when is_list(PathList), ?is_cache(Cache) ->
    call({set_path,PathList,Cache}).

-spec get_path() -> Path when
      Path :: [Dir :: file:filename()].
get_path() -> call(get_path).

-type add_path_ret() :: 'true' | {'error', 'bad_directory'}.
-spec add_path(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_path(Dir) -> add_path(Dir, nocache).

-spec add_path(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_path(Dir, Cache) when is_list(Dir), ?is_cache(Cache) -> call({add_path,last,Dir,Cache}).

-spec add_pathz(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_pathz(Dir) -> add_pathz(Dir, nocache).

-spec add_pathz(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_pathz(Dir, Cache) when is_list(Dir), ?is_cache(Cache) -> call({add_path,last,Dir,Cache}).

-spec add_patha(Dir) -> add_path_ret() when
      Dir :: file:filename().
add_patha(Dir) -> add_patha(Dir, nocache).

-spec add_patha(Dir, cache()) -> add_path_ret() when
      Dir :: file:filename().
add_patha(Dir, Cache) when is_list(Dir), ?is_cache(Cache) -> call({add_path,first,Dir,Cache}).

-spec add_paths(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_paths(Dirs) -> add_paths(Dirs, nocache).

-spec add_paths(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_paths(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) -> call({add_paths,last,Dirs,Cache}).

-spec add_pathsz(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsz(Dirs) -> add_pathsz(Dirs, nocache).

-spec add_pathsz(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsz(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) -> call({add_paths,last,Dirs,Cache}).

-spec add_pathsa(Dirs) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsa(Dirs) -> add_pathsa(Dirs, nocache).

-spec add_pathsa(Dirs, cache()) -> 'ok' when
      Dirs :: [Dir :: file:filename()].
add_pathsa(Dirs, Cache) when is_list(Dirs), ?is_cache(Cache) -> call({add_paths,first,Dirs,Cache}).

-spec del_path(NameOrDir) -> boolean() | {'error', What} when
      NameOrDir :: Name | Dir,
      Name :: atom(),
      Dir :: file:filename(),
      What :: 'bad_name'.
del_path(Name) when is_list(Name) ; is_atom(Name) -> call({del_path,Name}).

-spec del_paths(NamesOrDirs) -> 'ok' when
      NamesOrDirs :: [Name | Dir],
      Name :: atom(),
      Dir :: file:filename().
del_paths(Dirs) when is_list(Dirs) -> call({del_paths,Dirs}).

-type replace_path_ret() :: 'true' |
                            {'error', 'bad_directory' | 'bad_name' | {'badarg',_}}.
-spec replace_path(Name, Dir) -> replace_path_ret() when
      Name:: atom(),
      Dir :: file:filename().
replace_path(Name, Dir) ->
    replace_path(Name, Dir, nocache).

-spec replace_path(Name, Dir, cache()) -> replace_path_ret() when
      Name:: atom(),
      Dir :: file:filename().
replace_path(Name, Dir, Cache) when (is_atom(Name) orelse is_list(Name)),
                 (is_atom(Dir) orelse is_list(Dir)), ?is_cache(Cache) ->
    call({replace_path,Name,Dir,Cache}).

-spec get_mode() -> 'embedded' | 'interactive'.
get_mode() -> call(get_mode).

-spec clear_cache() -> ok.
clear_cache() -> call(clear_cache).

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

ensure_modules_loaded_2([{M,{Prepared,File}}|Ms], Errors) ->
    case call({load_module, Prepared, M, File, false, true}) of
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
    P = fun({_,{PC,_}}) ->
		erlang:has_prepared_code_on_load(PC)
	end,
    lists:partition(P, Prep).

verify_prepared([{M,{Prep,Name}}|T])
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

finish_loading(Prepared, EnsureLoaded) ->
    call({finish_loading,Prepared,EnsureLoaded}).

load_mods([]) ->
    {[],[]};
load_mods(Mods) ->
    F = fun(Mod) ->
        case get_object_code(Mod) of
            {Mod, Beam, File} -> prepare_loading(Mod, File, Beam);
            error -> {error, nofile}
        end
    end,
    do_par(F, Mods).

load_bins([]) ->
    {[],[]};
load_bins(BinItems) ->
    F = fun({Mod, File, Beam}) -> prepare_loading(Mod, File, Beam) end,
    do_par(F, BinItems).

-spec prepare_loading(module(), file:filename(), binary()) ->
                     {ok,_} | {error,_}.

prepare_loading(Mod, FullName, Beam) ->
    case erlang:prepare_loading(Mod, Beam) of
	{error,_}=Error ->
	    Error;
	Prepared ->
	    {ok,{Prepared,FullName}}
    end.

do_par(Fun, L) ->
    {_,Ref} = spawn_monitor(do_par_fun(Fun, L)),
    receive
	{'DOWN',Ref,process,_,Res} ->
	    Res
    end.

-type par_fun_type() :: fun((module() | {module(), file:filename(), binary()}) ->
                            {ok,_} | {error,_}).

-spec do_par_fun(par_fun_type(), list()) -> fun(() -> no_return()).

do_par_fun(Fun, L) ->
    fun() ->
	_ = [spawn_monitor(do_par_fun_each(Fun, Item)) || Item <- L],
	exit(do_par_recv(length(L), [], []))
    end.

-spec do_par_fun_each(par_fun_type(), term()) ->
			  fun(() -> no_return()).

do_par_fun_each(Fun, Mod) when is_atom(Mod) ->
    do_par_fun_each(Fun, Mod, Mod);
do_par_fun_each(Fun, {Mod, _, _} = Item) ->
    do_par_fun_each(Fun, Mod, Item).

do_par_fun_each(Fun, Mod, Item) ->
    fun() ->
        try Fun(Item) of
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

    Res.

%% Make sure that all modules that the code_server process calls
%% (directly or indirectly) are loaded. Otherwise the code_server
%% process will deadlock.

load_code_server_prerequisites() ->
    %% Please keep the alphabetical order.
    Needed = [beam_lib,
              binary,
	      ets,
	      filename,
	      gb_sets,
	      gb_trees,
	      lists,
	      os,
	      unicode],
    _ = [M = M:module_info(module) || M <- Needed],
    _ = erl_features:enabled(),
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
      Which :: loaded_filename() | non_existing.
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

-spec get_doc(Mod) -> {ok, Res} | {error, Reason} when
      Mod :: module(),
      Res :: #docs_v1{},
      Reason :: non_existing | missing | file:posix().
get_doc(Mod) when is_atom(Mod) ->
    get_doc(Mod, #{sources => [eep48, debug_info]}).

get_doc(Mod, #{sources:=[Source|Sources]}=Options) ->
    GetDoc = fun(Fn) -> R = case Source of
            debug_info -> get_doc_chunk_from_ast(Fn);
            eep48 -> get_doc_chunk(Fn, Mod)
        end,
        case R of
            {error, missing} -> get_doc(Mod, Options#{sources=>Sources});
            _ -> R
        end
    end,
    case which(Mod) of
        preloaded ->
            case code:lib_dir(erts) of
                {error, _} -> {error, missing};
                ErtsDir ->
                    ErtsEbinDir =
                        case filelib:is_dir(filename:join([ErtsDir,"ebin"])) of
                            true -> filename:join([ErtsDir,"ebin"]);
                            false -> filename:join([ErtsDir,"preloaded","ebin"])
                        end,
                    Fn = filename:join([ErtsEbinDir, atom_to_list(Mod) ++ ".beam"]),
                    GetDoc(Fn)
            end;
        Error when is_atom(Error) ->
            {error, Error};
        Fn ->
            GetDoc(Fn)
    end;
get_doc(_, #{sources:=[]}) ->
    {error, missing}.

get_doc_chunk(Filename, Mod) when is_atom(Mod) ->
    case beam_lib:chunks(Filename, ["Docs"]) of
        {error,beam_lib,{missing_chunk,_,_}} ->
            get_doc_chunk(Filename, atom_to_list(Mod));
        {error,beam_lib,{file_error,_Filename,_Err}} ->
            get_doc_chunk(Filename, atom_to_list(Mod));
        {ok, {Mod, [{"Docs",Bin}]}} ->
            {ok,binary_to_term(Bin)}
    end;
get_doc_chunk(Filename, Mod) ->
    RootDir = code:root_dir(),
    case filename:dirname(Filename) of
        Filename ->
            {error,missing};
        RootDir ->
            {error,missing};
        Dir ->
            ChunkFile = filename:join([Dir,"doc","chunks",Mod ++ ".chunk"]),
            case file:read_file(ChunkFile) of
                {ok, Bin} ->
                    {ok, binary_to_term(Bin)};
                {error,enoent} ->
                    get_doc_chunk(Dir, Mod);
                {error,Reason} ->
                    {error,Reason}
            end
    end.

get_doc_chunk_from_ast(Filename) ->
    case beam_lib:chunks(Filename, [abstract_code]) of
        {error,beam_lib,{missing_chunk,_,_}} ->
            {error,missing};
        {error,beam_lib,{file_error,_,_}} ->
            {error, missing};
        {ok, {_Mod, [{abstract_code,
                      {raw_abstract_v1, AST}}]}} ->
            Docs = get_function_docs_from_ast(AST),
            Types = get_type_docs_from_ast(AST),
            {ok, #docs_v1{ anno = 0, beam_language = erlang,
                           module_doc = none,
                           metadata = #{ generated => true, otp_doc_vsn => ?CURR_DOC_VERSION},
                           docs = Docs++Types }};
        {ok, {_Mod, [{abstract_code,no_abstract_code}]}} ->
            {error,missing};
        Error ->
            Error
    end.

get_type_docs_from_ast(AST) ->
    lists:flatmap(fun(E) -> get_type_docs_from_ast(E, AST) end, AST).
get_type_docs_from_ast({attribute, Anno, type, {TypeName, _, Ps}}=Meta, _) ->
    Arity = length(Ps),
    Signature = io_lib:format("~p/~p",[TypeName,Arity]),
    [{{type, TypeName, Arity},Anno,[unicode:characters_to_binary(Signature)],none,#{signature => [Meta]}}];
get_type_docs_from_ast(_, _) ->
    [].

get_function_docs_from_ast(AST) ->
    lists:flatmap(fun(E) -> get_function_docs_from_ast(E, AST) end, AST).
get_function_docs_from_ast({function,Anno,Name,Arity,_Code}, AST) ->
    Signature = io_lib:format("~p/~p",[Name,Arity]),
    Specs = lists:filter(
               fun({attribute,_Ln,spec,{FA,_}}) ->
                       case FA of
                           {F,A} ->
                               F =:= Name andalso A =:= Arity;
                           {_, F, A} ->
                               F =:= Name andalso A =:= Arity
                       end;
                  (_) -> false
               end, AST),
    SpecMd = case Specs of
                 [S] -> #{ signature => [S] };
                 [] -> #{}
             end,
    [{{function, Name, Arity}, Anno,
      [unicode:characters_to_binary(Signature)], none, SpecMd}];
get_function_docs_from_ast(_, _) ->
    [].

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

-type module_status() :: not_loaded | loaded | modified | removed.

%% Returns the list of all loaded modules and their current status
-spec module_status() -> [{module(), module_status()}].
module_status() ->
    module_status([M || {M, _} <- all_loaded()]).

%% Returns the status of the module in relation to object file on disk.
-spec module_status (Module :: module() | [module()]) ->
          module_status() | [{module(), module_status()}].
module_status(Modules) when is_list(Modules) ->
    PathFiles = path_files(),
    [{M, module_status(M, PathFiles)} || M <- Modules];
module_status(Module) ->
    module_status(Module, code:get_path()).

%% Note that we don't want to go via which/1, since it doesn't look at the
%% disk contents at all if the module is already loaded.
module_status(Module, PathFiles) ->
    case is_loaded(Module) of
        false -> not_loaded;
        {file, preloaded} -> loaded;
        {file, cover_compiled} ->
            %% Cover compilation loads directly to memory and does not
            %% create a beam file, so report 'modified' if a file exists.
            case which(Module, PathFiles) of
                non_existing -> removed;
                _File -> modified
            end;
        {file, []} -> loaded;  % no beam file - generated code
        {file, [_|_]} ->
            %% We don't care whether or not the file is in the same location
            %% as when last loaded, as long as it can be found in the path.
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
    MD5 =/= beam_file_md5(Module, Path).

beam_file_md5(Module, Path) ->
    case do_beam_file_md5(Path) of
        MD5 when is_binary(MD5) ->
            MD5;
        undefined ->
            %% This module is probably embedded in an archive.
            case get_object_code(Module) of
                {Module, Code, _Path} ->
                    do_beam_file_md5(Code);
                error ->
                    undefined
            end
    end.

do_beam_file_md5(PathOrCode) ->
    case beam_lib:md5(PathOrCode) of
        {ok,{_Mod,MD5}} -> MD5;
        _ -> undefined
    end.

%% Returns a list of all modules modified on disk.
-spec modified_modules() -> [module()].
modified_modules() ->
    [M || {M, modified} <- module_status()].

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
