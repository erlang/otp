%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
	 set_primary_archive/2,
	 clash/0]).

-include_lib("kernel/include/file.hrl").

%% User interface.
%%
%% objfile_extension()		-> ".beam"
%% set_path(Dir*)		-> true
%% get_path()			-> Dir*
%% add_path(Dir)		-> true | {error, What}
%% add_patha(Dir)		-> true | {error, What}
%% add_pathz(Dir)		-> true | {error, What}
%% add_paths(DirList)           -> true | {error, What}
%% add_pathsa(DirList)          -> true | {error, What}
%% add_pathsz(DirList)          -> true | {error, What}
%% del_path(Dir)		-> true | {error, What}
%% replace_path(Name,Dir)       -> true | {error, What}
%% load_file(File)		-> {error,What} | {module, Mod}
%% load_abs(File)		-> {error,What} | {module, Mod}
%% load_abs(File,Mod)		-> {error,What} | {module, Mod}
%% load_binary(Mod,File,Bin)    -> {error,What} | {module,Mod}
%% ensure_loaded(Module)	-> {error,What} | {module, Mod}
%% delete(Module)
%% purge(Module)  kills all procs running old code
%% soft_purge(Module)           -> true | false
%% is_loaded(Module)		-> {file, File} | false
%% all_loaded()			-> {Module, File}*
%% get_object_code(Mod)         -> error | {Mod, Bin, Filename}
%% stop()			-> true
%% root_dir()                   
%% compiler_dir()
%% lib_dir()
%% priv_dir(Name)
%% stick_dir(Dir)               -> ok | error
%% unstick_dir(Dir)             -> ok | error
%% is_sticky(Module)            -> true | false
%% which(Module)                -> Filename
%% set_primary_archive((FileName, Bin)  -> ok | {error, Reason}
%% clash() ->                   -> print out

%%----------------------------------------------------------------------------
%% Some types for basic exported functions of this module
%%----------------------------------------------------------------------------

-type load_error_rsn() :: 'badfile' | 'native_code' | 'nofile' | 'not_purged'
		        | 'sticky_directory'.	% for some functions only
-type load_ret() :: {'error', load_error_rsn()} | {'module', atom()}.
-type loaded_ret_atoms() :: 'cover_compiled' | 'preloaded'.
-type loaded_filename() :: file:filename() | loaded_ret_atoms().

%%----------------------------------------------------------------------------
%% User interface
%%----------------------------------------------------------------------------

-spec objfile_extension() -> file:filename().
objfile_extension() ->
    init:objfile_extension().

-spec load_file(Module :: atom()) -> load_ret(). 
load_file(Mod) when is_atom(Mod) ->
    call({load_file,Mod}).

-spec ensure_loaded(Module :: atom()) -> load_ret().
ensure_loaded(Mod) when is_atom(Mod) -> 
    call({ensure_loaded,Mod}).

%% XXX File as an atom is allowed only for backwards compatibility.
-spec load_abs(Filename :: file:filename()) -> load_ret().
load_abs(File) when is_list(File); is_atom(File) -> call({load_abs,File,[]}).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_abs(Filename :: loaded_filename(), Module :: atom()) -> load_ret().
load_abs(File,M) when (is_list(File) orelse is_atom(File)), is_atom(M) ->
    call({load_abs,File,M}).

%% XXX Filename is also an atom(), e.g. 'cover_compiled'
-spec load_binary(Module :: atom(), Filename :: loaded_filename(), Binary :: binary()) -> load_ret().
load_binary(Mod,File,Bin)
  when is_atom(Mod), (is_list(File) orelse is_atom(File)), is_binary(Bin) ->
    call({load_binary,Mod,File,Bin}).

-spec load_native_partial(Module :: atom(), Binary :: binary()) -> load_ret().
load_native_partial(Mod,Bin) when is_atom(Mod), is_binary(Bin) ->
    call({load_native_partial,Mod,Bin}).

-spec load_native_sticky(Module :: atom(), Binary :: binary(), WholeModule :: 'false' | binary()) -> load_ret().
load_native_sticky(Mod,Bin,WholeModule)
  when is_atom(Mod), is_binary(Bin),
       (is_binary(WholeModule) orelse WholeModule =:= false) ->
    call({load_native_sticky,Mod,Bin,WholeModule}).

-spec delete(Module :: atom()) -> boolean().
delete(Mod) when is_atom(Mod) -> call({delete,Mod}).

-spec purge/1 :: (Module :: atom()) -> boolean().
purge(Mod) when is_atom(Mod) -> call({purge,Mod}).

-spec soft_purge(Module :: atom()) -> boolean().
soft_purge(Mod) when is_atom(Mod) -> call({soft_purge,Mod}).

-spec is_loaded(Module :: atom()) -> {'file', loaded_filename()} | 'false'.
is_loaded(Mod) when is_atom(Mod) -> call({is_loaded,Mod}).

-spec get_object_code(Module :: atom()) -> {atom(), binary(), file:filename()} | 'error'.
get_object_code(Mod) when is_atom(Mod) -> call({get_object_code, Mod}).

-spec all_loaded() -> [{atom(), loaded_filename()}].
all_loaded() -> call(all_loaded).

-spec stop() -> no_return().
stop() -> call(stop).

-spec root_dir() -> file:filename().
root_dir() -> call({dir,root_dir}).

-spec lib_dir() -> file:filename().
lib_dir() -> call({dir,lib_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-spec lib_dir(App :: atom()) -> file:filename() | {'error', 'bad_name'}.
lib_dir(App) when is_atom(App) ; is_list(App) -> call({dir,{lib_dir,App}}).

-spec lib_dir(App :: atom(), SubDir :: atom()) -> file:filename() | {'error', 'bad_name'}.
lib_dir(App, SubDir) when is_atom(App), is_atom(SubDir) -> call({dir,{lib_dir,App,SubDir}}).

-spec compiler_dir() -> file:filename().
compiler_dir() -> call({dir,compiler_dir}).

%% XXX is_list() is for backwards compatibility -- take out in future version
-spec priv_dir(Appl :: atom()) -> file:filename() | {'error', 'bad_name'}.
priv_dir(App) when is_atom(App) ; is_list(App) -> call({dir,{priv_dir,App}}).

-spec stick_dir(Directory :: file:filename()) -> 'ok' | 'error'.
stick_dir(Dir) when is_list(Dir) -> call({stick_dir,Dir}).

-spec unstick_dir(Directory :: file:filename()) -> 'ok' | 'error'.
unstick_dir(Dir) when is_list(Dir) -> call({unstick_dir,Dir}).

-spec stick_mod(Module :: atom()) -> 'true'.
stick_mod(Mod) when is_atom(Mod) -> call({stick_mod,Mod}).

-spec unstick_mod(Module :: atom()) -> 'true'.
unstick_mod(Mod) when is_atom(Mod) -> call({unstick_mod,Mod}).

-spec is_sticky(Module :: atom()) -> boolean().
is_sticky(Mod) when is_atom(Mod) -> call({is_sticky,Mod}).

-spec set_path(Directories :: [file:filename()]) -> 'true' | {'error', term()}.
set_path(PathList) when is_list(PathList) -> call({set_path,PathList}).

-spec get_path() -> [file:filename()].
get_path() -> call(get_path).

-spec add_path(Directory :: file:filename()) -> 'true' | {'error', term()}.
add_path(Dir) when is_list(Dir) -> call({add_path,last,Dir}).

-spec add_pathz(Directory :: file:filename()) -> 'true' | {'error', term()}.
add_pathz(Dir) when is_list(Dir) -> call({add_path,last,Dir}).

-spec add_patha(Directory :: file:filename()) -> 'true' | {'error', term()}.
add_patha(Dir) when is_list(Dir) -> call({add_path,first,Dir}).

-spec add_paths(Directories :: [file:filename()]) -> 'ok'.
add_paths(Dirs) when is_list(Dirs) -> call({add_paths,last,Dirs}).

-spec add_pathsz(Directories :: [file:filename()]) -> 'ok'.
add_pathsz(Dirs) when is_list(Dirs) -> call({add_paths,last,Dirs}).

-spec add_pathsa(Directories :: [file:filename()]) -> 'ok'.
add_pathsa(Dirs) when is_list(Dirs) -> call({add_paths,first,Dirs}).

%% XXX Contract's input argument differs from add_path/1 -- why?
-spec del_path(Name :: file:filename() | atom()) -> boolean() | {'error', 'bad_name'}.
del_path(Name) when is_list(Name) ; is_atom(Name) -> call({del_path,Name}).

-type replace_path_error() :: {'error', 'bad_directory' | 'bad_name' | {'badarg',_}}.
-spec replace_path(Name:: atom(), Dir :: file:filename()) -> 'true' | replace_path_error().
replace_path(Name, Dir) when (is_atom(Name) or is_list(Name)) and
			     (is_atom(Dir) or is_list(Dir)) ->
    call({replace_path,Name,Dir}).

-spec rehash() -> 'ok'.
rehash() -> call(rehash).

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
    %% The following module_info/1 calls are here to ensure
    %% that the modules are loaded prior to their use elsewhere in 
    %% the code_server.
    %% Otherwise a deadlock may occur when the code_server is starting.
    code_server:module_info(module),
    packages:module_info(module),
    catch hipe_unified_loader:load_hipe_modules(),
    gb_sets:module_info(module),
    gb_trees:module_info(module),

    ets:module_info(module),
    os:module_info(module),
    filename:module_info(module),
    lists:module_info(module),

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
		    Ok2;
		Other ->
		    Other
	    end;
	Other ->
	    error_logger:error_msg("Can not start code server ~w ~n",[Other]),
	    {error, crash}
    end.

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
	    stick_dir(filename:append(Dir, "ebin")),
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

-type which_ret_atoms() :: loaded_ret_atoms() | 'non_existing'.

-spec which(Module :: atom()) -> file:filename() | which_ret_atoms().

which(Module) when is_atom(Module) ->
    case is_loaded(Module) of
	false ->
	    which2(Module);
	{file, File} ->
	    File
    end.

which2(Module) ->
    Base = to_path(Module),
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

-spec where_is_file(Filename :: file:filename()) ->
        'non_existing' | file:filename().

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

-spec set_primary_archive(ArchiveFile :: file:filename(), ArchiveBin :: binary()) -> 'ok' | {'error', atom()}.

set_primary_archive(ArchiveFile0, ArchiveBin) when is_list(ArchiveFile0), is_binary(ArchiveBin) ->
    ArchiveFile = filename:absname(ArchiveFile0),
    case call({set_primary_archive, ArchiveFile, ArchiveBin}) of
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
	    io:format("** ~s hides ~s~n",
		      [filename:join(Dir, File),
		       filename:join(Dir2, File)]),
	    [clash | search(Tail)]
    end.

build([]) -> [];
build([Dir|Tail]) ->
    Files = filter(objfile_extension(), Dir, file:list_dir(Dir)),
    [decorate(Files, Dir) | build(Tail)].

decorate([], _) -> [];
decorate([File|Tail], Dir) ->
    [{Dir, File} | decorate(Tail, Dir)].

filter(_Ext, Dir, {error,_}) ->     
    io:format("** Bad path can't read ~s~n", [Dir]), [];
filter(Ext, _, {ok,Files}) -> 
    filter2(Ext, length(Ext), Files).

filter2(_Ext, _Extlen, []) -> [];
filter2(Ext, Extlen,[File|Tail]) ->
    case has_ext(Ext,Extlen, File) of 
	true -> [File | filter2(Ext, Extlen, Tail)];
	false -> filter2(Ext, Extlen, Tail)
    end.

has_ext(Ext, Extlen,File) ->
    L = length(File),
    case catch lists:nthtail(L - Extlen, File) of
	Ext -> true;
	_ -> false
    end.

to_path(X) ->
    filename:join(packages:split(X)).
