%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(code).

%% This is the interface module to the code server. It also contains
%% some implementation details.  See also related modules: code_*.erl
%% in this directory.

-export([objfile_extension/0, 
	 set_path/1, 
	 get_path/0, 
	 load_file/1,
	 ensure_loaded/1,
	 load_abs/1,
	 load_abs/2,
	 load_binary/3,
	 load_native_partial/2,
	 load_native_sticky/3,
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
	 start_link/0, start_link/1,
	 which/1,
	 where_is_file/1,
	 where_is_file/2,
	 set_primary_archive/4,
	 clash/0,
     get_mode/0]).

-export_type([load_error_rsn/0, load_ret/0]).

-include_lib("kernel/include/file.hrl").

%%----------------------------------------------------------------------------
%% Some types for basic exported functions of this module
%%----------------------------------------------------------------------------

-type load_error_rsn() :: 'badfile'
                        | 'native_code'
                        | 'nofile'
                        | 'not_purged'
                        | 'on_load'
                        | 'sticky_directory'.
-type load_ret() :: {'error', What :: load_error_rsn()}
                  | {'module', Module :: module()}.
-type loaded_ret_atoms() :: 'cover_compiled' | 'preloaded'.
-type loaded_filename() :: (Filename :: file:filename()) | loaded_ret_atoms().

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

-spec make_stub_module(Module, Beam, Info) -> Module when
      Module :: module(),
      Beam :: binary(),
      Info :: {list(), list()}.

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
      What :: embedded | badfile | native_code | nofile | on_load.
ensure_loaded(Mod) when is_atom(Mod) -> 
    call({ensure_loaded,Mod}).

%% XXX File as an atom is allowed only for backwards compatibility.
-spec load_abs(Filename) -> load_ret() when
      Filename :: file:filename().
load_abs(File) when is_list(File); is_atom(File) -> call({load_abs,File,[]}).

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
      What :: 'bad_directory' | 'bad_path'.
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
rehash() -> call(rehash).

-spec get_mode() -> 'embedded' | 'interactive'.
get_mode() -> call(get_mode).

%%-----------------------------------------------------------------

call(Req) ->
    code_server:call(code_server, Req).

-spec start_link() -> {'ok', pid()} | {'error', 'crash'}.
start_link() ->
    start_link([stick]).

-spec start_link(Flags :: [atom()]) -> {'ok', pid()} | {'error', 'crash'}.
start_link(Flags) ->
    do_start(Flags).
    
%%-----------------------------------------------------------------
%% In the init phase, code must not use any modules not yet loaded,
%% either pre_loaded (e.g. init) or first in the script (e.g.
%% erlang).  Therefore, keep the modules used in init phase to a
%% minimum, and make sure they are loaded before init is called.
%% Try to call these modules from do_start instead.
%% file is used in init - this is ok; file has been started before
%% us, so the module is loaded.
%%-----------------------------------------------------------------

do_start(Flags) ->
    load_code_server_prerequisites(),

    Mode = get_mode(Flags),
    case init:get_argument(root) of 
	{ok,[[Root0]]} ->
	    Root = filename:join([Root0]), % Normalize.  Use filename
	    case code_server:start_link([Root,Mode]) of
		{ok,_Pid} = Ok2 ->
		    if
			Mode =:= interactive ->
			    case lists:member(stick, Flags) of
				true -> do_stick_dirs();
				_    -> ok
			    end;
			true ->
			    ok
		    end,
		    %% Quietly load native code for all modules loaded so far
		    catch load_native_code_for_all_loaded(),
		    Ok2;
		Other ->
		    Other
	    end;
	Other ->
	    error_logger:error_msg("Can not start code server ~w ~n", [Other]),
	    {error, crash}
    end.

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

get_mode(Flags) ->
    case lists:member(embedded, Flags) of
	true ->
	    embedded;
	_Otherwise -> 
	    case init:get_argument(mode) of
		{ok,[["embedded"]]} ->
		    embedded;
		{ok,[["minimal"]]} ->
		    minimal;
		_Else ->
		    interactive
	    end
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
	    which2(Module);
	{file, File} ->
	    File
    end.

which2(Module) ->
    Base = atom_to_list(Module),
    File = filename:basename(Base) ++ objfile_extension(),
    Path = get_path(),
    which(File, filename:dirname(Base), Path).

-spec which(file:filename(), file:filename(), [file:filename()]) ->
        'non_existing' | file:filename().

which(_, _, []) ->
    non_existing;
which(File, Base, [Directory|Tail]) ->
    Path = if
	       Base =:= "." -> Directory;
	       true -> filename:join(Directory, Base)
	   end,
    case erl_prim_loader:list_dir(Path) of
	{ok,Files} ->
	    case lists:member(File,Files) of
		true ->
		    filename:append(Path, File);
		false ->
		    which(File, Base, Tail)
	    end;
	_Error ->
	    which(File, Base, Tail)
    end.

%% Search the code path for a specific file. Try to locate
%% it in the code path cache if possible.

-spec where_is_file(Filename) -> non_existing | Absname when
      Filename :: file:filename(),
      Absname :: file:filename().
where_is_file(File) when is_list(File) ->
    case call({is_cached,File}) of
	no ->
	    Path = get_path(),
	    which(File, ".", Path);
	Dir ->
	    filename:join(Dir, File)
    end.

-spec where_is_file(Path :: file:filename(), Filename :: file:filename()) ->
        file:filename() | 'non_existing'.

where_is_file(Path, File) when is_list(Path), is_list(File) ->
    CodePath = get_path(),
    if
	Path =:= CodePath ->
	    case call({is_cached, File}) of
		no ->
		    which(File, ".", Path);
		Dir ->
		    filename:join(Dir, File)
	    end;
	true ->
	    which(File, ".", Path)
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

-spec load_native_code_for_all_loaded() -> ok.
load_native_code_for_all_loaded() ->
    Architecture = erlang:system_info(hipe_architecture),
    ChunkName = hipe_unified_loader:chunk_name(Architecture),
    lists:foreach(fun({Module, BeamFilename}) ->
        case code:is_module_native(Module) of
            false ->
                case beam_lib:chunks(BeamFilename, [ChunkName]) of
                    {ok,{_,[{_,Bin}]}} when is_binary(Bin) ->
                        load_native_partial(Module, Bin);
                    {error, beam_lib, _} -> ok
                end;
            true -> ok
        end
    end, all_loaded()).
