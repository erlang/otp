%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
-module(code_server).

%% This file holds the server part of the code_server.

-export([start_link/1,
	 call/2,
	 system_continue/3, 
	 system_terminate/4,
	 system_code_change/4,
	 error_msg/2, info_msg/2
	]).

-include_lib("kernel/include/file.hrl").

-import(lists, [foreach/2]).

-define(ANY_NATIVE_CODE_LOADED, any_native_code_loaded).

-record(state, {supervisor,
		root,
		path,
		moddb,
		namedb,
		cache = no_cache,
		mode = interactive,
		on_load = []}).
-type state() :: #state{}.

start_link(Args) ->
    Ref = make_ref(),
    Parent = self(),
    Init = fun() -> init(Ref, Parent, Args) end,
    spawn_link(Init),
    receive 
	{Ref,Res} -> Res
    end.


%% -----------------------------------------------------------
%% Init the code_server process.
%% -----------------------------------------------------------

init(Ref, Parent, [Root,Mode0]) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),

    Db = ets:new(code, [private]),
    foreach(fun (M) ->
		    %% Pre-loaded modules are always sticky.
		    ets:insert(Db, [{M,preloaded},{{sticky,M},true}])
	    end, erlang:pre_loaded()),
    ets:insert(Db, init:fetch_loaded()),

    Mode = 
	case Mode0 of
	    minimal -> interactive;
	    _       -> Mode0
	end,

    IPath =
	case Mode of
	    interactive ->
		LibDir = filename:append(Root, "lib"),
		{ok,Dirs} = erl_prim_loader:list_dir(LibDir),
		{Paths,_Libs} = make_path(LibDir, Dirs),
		UserLibPaths = get_user_lib_dirs(),
		["."] ++ UserLibPaths ++ Paths;
	    _ ->
		[]
	end,

    Path = add_loader_path(IPath, Mode),
    State0 = #state{root = Root,
		    path = Path,
		    moddb = Db,
		    namedb = init_namedb(Path),
		    mode = Mode},

    State =
	case init:get_argument(code_path_cache) of
	    {ok, _} -> 
		create_cache(State0);
	    error -> 
		State0
	end,

    put(?ANY_NATIVE_CODE_LOADED, false),

    Parent ! {Ref,{ok,self()}},
    loop(State#state{supervisor = Parent}).

get_user_lib_dirs() ->
    case os:getenv("ERL_LIBS") of
	LibDirs0 when is_list(LibDirs0) ->
	    Sep =
		case os:type() of
		    {win32, _} -> $;;
		    _          -> $:
		end,
	    LibDirs = split_paths(LibDirs0, Sep, [], []),
	    get_user_lib_dirs_1(LibDirs);
	false ->
	    []
    end.

get_user_lib_dirs_1([Dir|DirList]) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok, Dirs} ->
	    {Paths,_Libs} = make_path(Dir, Dirs),
	    %% Only add paths trailing with ./ebin.
	    [P || P <- Paths, filename:basename(P) =:= "ebin"] ++
		get_user_lib_dirs_1(DirList);
	error ->
	    get_user_lib_dirs_1(DirList)
    end;
get_user_lib_dirs_1([]) -> [].


split_paths([S|T], S, Path, Paths) ->
    split_paths(T, S, [], [lists:reverse(Path) | Paths]);
split_paths([C|T], S, Path, Paths) ->
    split_paths(T, S, [C|Path], Paths);
split_paths([], _S, Path, Paths) ->
    lists:reverse(Paths, [lists:reverse(Path)]).

call(Name, Req) ->
    Name ! {code_call, self(), Req},
    receive 
	{?MODULE, Reply} ->
	    Reply
    end.

reply(Pid, Res) ->
    Pid ! {?MODULE, Res}.

loop(#state{supervisor=Supervisor}=State0) ->
    receive 
	{code_call, Pid, Req} ->
	    case handle_call(Req, {Pid, call}, State0) of
		{reply, Res, State} ->
		    _ = reply(Pid, Res),
		    loop(State);
		{noreply, State} ->
		    loop(State);
		{stop, Why, stopped, State} ->
		    system_terminate(Why, Supervisor, [], State)
	    end;
	{'EXIT', Supervisor, Reason} ->
	    system_terminate(Reason, Supervisor, [], State0);
	{system, From, Msg} ->
	    handle_system_msg(running,Msg, From, Supervisor, State0);
	{'DOWN',Ref,process,_,Res} ->
	    State = finish_on_load(Ref, Res, State0),
	    loop(State);
	_Msg ->
	    loop(State0)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

handle_system_msg(SysState, Msg, From, Parent, Misc) ->
    case do_sys_cmd(SysState, Msg, Parent, Misc) of
	{suspended, Reply, NMisc} ->
	    gen_reply(From, Reply),
	    suspend_loop(suspended, Parent, NMisc);
	{running, Reply, NMisc} ->
	    gen_reply(From, Reply),
	    system_continue(Parent, [], NMisc)
    end.

gen_reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%-----------------------------------------------------------------
%% When a process is suspended, it can only respond to system
%% messages.
%%-----------------------------------------------------------------
suspend_loop(SysState, Parent, Misc) ->
    receive
	{system, From, Msg} ->
	    handle_system_msg(SysState, Msg, From, Parent, Misc);
	{'EXIT', Parent, Reason} ->
	    system_terminate(Reason, Parent, [], Misc)
    end.

do_sys_cmd(_, suspend, _Parent, Misc) ->
    {suspended, ok, Misc};
do_sys_cmd(_, resume, _Parent, Misc) ->
    {running, ok, Misc};
do_sys_cmd(SysState, get_status, Parent, Misc) ->
    Status = {status, self(), {module, ?MODULE},
	      [get(), SysState, Parent, [], Misc]},
    {SysState, Status, Misc};
do_sys_cmd(SysState, {debug, _What}, _Parent, Misc) ->
    {SysState,ok,Misc};
do_sys_cmd(suspended, {change_code, Module, Vsn, Extra}, _Parent, Misc0) ->
    {Res, Misc} = 
	case catch ?MODULE:system_code_change(Misc0, Module, Vsn, Extra)  of
	    {ok, _} = Ok -> Ok;
	    Else -> {{error, Else}, Misc0}
	end,
    {suspended, Res, Misc};
do_sys_cmd(SysState, Other, _Parent, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Misc}.

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(_Reason, _Parent, _Debug, _State) ->
    %% error_msg("~p terminating: ~p~n ", [?MODULE, Reason]),
    exit(shutdown).

-spec system_code_change(state(), module(), term(), term()) -> {'ok', state()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

%%
%% The gen_server call back functions.
%%

handle_call({stick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, true, S),S};

handle_call({unstick_dir,Dir}, {_From,_Tag}, S) ->
    {reply,stick_dir(Dir, false, S),S};

handle_call({stick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, true, S),S};

handle_call({unstick_mod,Mod}, {_From,_Tag}, S) ->
    {reply,stick_mod(Mod, false, S),S};

handle_call({dir,Dir}, {_From,_Tag}, S) ->
    Root = S#state.root,
    Resp = do_dir(Root,Dir,S#state.namedb),
    {reply,Resp,S};

handle_call({load_file,Mod}, Caller, St) ->
    case modp(Mod) of
	false ->
	    {reply,{error,badarg},St};
	true ->
	    load_file(Mod, Caller, St)
    end;

handle_call({add_path,Where,Dir0}, {_From,_Tag},
	    #state{cache=Cache0,namedb=Namedb,path=Path0}=S) ->
    case Cache0 of
	no_cache ->
	    {Resp,Path} = add_path(Where, Dir0, Path0, Namedb),
	    {reply,Resp,S#state{path=Path}};
	_ ->
	    Dir = absname(Dir0), %% Cache always expands the path 
	    {Resp,Path} = add_path(Where, Dir, Path0, Namedb),
	    Cache = update_cache([Dir], Where, Cache0),
	    {reply,Resp,S#state{path=Path,cache=Cache}}
    end;

handle_call({add_paths,Where,Dirs0}, {_From,_Tag},
	    #state{cache=Cache0,namedb=Namedb,path=Path0}=S) ->
    case Cache0 of
	no_cache ->
	    {Resp,Path} = add_paths(Where, Dirs0, Path0, Namedb),
	    {reply,Resp,S#state{path=Path}};
	_ ->
	    %% Cache always expands the path 
	    Dirs = [absname(Dir) || Dir <- Dirs0], 
	    {Resp,Path} = add_paths(Where, Dirs, Path0, Namedb),
	    Cache=update_cache(Dirs,Where,Cache0),
	    {reply,Resp,S#state{cache=Cache,path=Path}}
    end;

handle_call({set_path,PathList}, {_From,_Tag},
	    #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path,NewDb} = set_path(PathList, Path0, Namedb),
    {reply,Resp,rehash_cache(S#state{path=Path,namedb=NewDb})};

handle_call({del_path,Name}, {_From,_Tag},
	    #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path} = del_path(Name, Path0, Namedb),
    {reply,Resp,rehash_cache(S#state{path=Path})};

handle_call({replace_path,Name,Dir}, {_From,_Tag},
	    #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path} = replace_path(Name, Dir, Path0, Namedb),
    {reply,Resp,rehash_cache(S#state{path=Path})};

handle_call(rehash, {_From,_Tag}, S0) ->
    S = create_cache(S0),
    {reply,ok,S};

handle_call(get_path, {_From,_Tag}, S) ->
    {reply,S#state.path,S};

%% Messages to load, delete and purge modules/files.
handle_call({load_abs,File,Mod}, Caller, S) ->
    case modp(File) of
	false ->
	    {reply,{error,badarg},S};
	true ->
	    load_abs(File, Mod, Caller, S)
    end;

handle_call({load_binary,Mod,File,Bin}, Caller, S) ->
    do_load_binary(Mod, File, Bin, Caller, S);

handle_call({load_native_partial,Mod,Bin}, {_From,_Tag}, S) ->
    Result = (catch hipe_unified_loader:load(Mod, Bin)),
    Status = hipe_result_to_status(Result),
    {reply,Status,S};

handle_call({load_native_sticky,Mod,Bin,WholeModule}, {_From,_Tag}, S) ->
    Result = (catch hipe_unified_loader:load_module(Mod, Bin, WholeModule)),
    Status = hipe_result_to_status(Result),
    {reply,Status,S};

handle_call({ensure_loaded,Mod0}, Caller, St0) ->
    Fun = fun (M, St) ->
		  case erlang:module_loaded(M) of
		      true ->
			  {reply,{module,M},St};
		      false when St#state.mode =:= interactive ->
			  load_file(M, Caller, St);
		      false -> 
			  {reply,{error,embedded},St}
		  end
	  end,
    do_mod_call(Fun, Mod0, {error,badarg}, St0);

handle_call({delete,Mod0}, {_From,_Tag}, S) ->
    Fun = fun (M, St) ->
		  case catch erlang:delete_module(M) of
		      true ->
			  ets:delete(St#state.moddb, M),
			  {reply,true,St};
		      _ -> 
			  {reply,false,St}
		  end
	  end,
    do_mod_call(Fun, Mod0, false, S);

handle_call({purge,Mod0}, {_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) ->
			{reply,do_purge(M),St}
		end, Mod0, false, St0);

handle_call({soft_purge,Mod0}, {_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) ->
			{reply,do_soft_purge(M),St}
		end, Mod0, true, St0);

handle_call({is_loaded,Mod0}, {_From,_Tag}, St0) ->
    do_mod_call(fun (M, St) ->
			{reply,is_loaded(M, St#state.moddb),St}
		end, Mod0, false, St0);

handle_call(all_loaded, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply,all_loaded(Db),S};

handle_call({get_object_code,Mod0}, {_From,_Tag}, St0) ->
    Fun = fun(M, St) ->
		  Path = St#state.path,
		  case mod_to_bin(Path, atom_to_list(M)) of
		      {_,Bin,FName} -> {reply,{M,Bin,FName},St};
		      Error -> {reply,Error,St}
		  end
	  end,
    do_mod_call(Fun, Mod0, error, St0);

handle_call({is_sticky, Mod}, {_From,_Tag}, S) ->
    Db = S#state.moddb,
    {reply, is_sticky(Mod,Db), S};

handle_call(stop,{_From,_Tag}, S) ->
    {stop,normal,stopped,S};

handle_call({is_cached,_File}, {_From,_Tag}, S=#state{cache=no_cache}) ->
    {reply, no, S};

handle_call({set_primary_archive, File, ArchiveBin, FileInfo, ParserFun}, {_From,_Tag}, S=#state{mode=Mode}) ->
    case erl_prim_loader:set_primary_archive(File, ArchiveBin, FileInfo, ParserFun) of
	{ok, Files} ->
	    {reply, {ok, Mode, Files}, S};
	{error, _Reason} = Error ->
	    {reply, Error, S}
    end;

handle_call({is_cached,File}, {_From,_Tag}, S=#state{cache=Cache}) ->
    ObjExt = objfile_extension(),
    Ext = filename:extension(File),
    Type = case Ext of
	       ObjExt -> obj;
	       ".app" -> app;
	       _ -> undef
	   end,
    if Type =:= undef -> 
	    {reply, no, S};
       true ->
	    Key = {Type,list_to_atom(filename:rootname(File, Ext))},
	    case ets:lookup(Cache, Key) of
		[] -> 
		    {reply, no, S};
		[{Key,Dir}] ->
		    {reply, Dir, S}
	    end
    end;

handle_call(get_mode, {_From,_Tag}, S=#state{mode=Mode}) ->
    {reply, Mode, S};

handle_call(Other,{_From,_Tag}, S) ->			
    error_msg(" ** Codeserver*** ignoring ~w~n ",[Other]),
    {noreply,S}.

do_mod_call(Action, Module, _Error, St) when is_atom(Module) ->
    Action(Module, St);
do_mod_call(Action, Module, Error, St) ->
    try list_to_atom(Module) of
	Atom when is_atom(Atom) ->
	    Action(Atom, St)
    catch
	error:badarg ->
	    {reply,Error,St}
    end.

%% --------------------------------------------------------------
%% Cache functions 
%% --------------------------------------------------------------

create_cache(St = #state{cache = no_cache}) ->
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St);
create_cache(St) ->
    rehash_cache(St).

rehash_cache(St = #state{cache = no_cache}) ->
    St;
rehash_cache(St = #state{cache = OldCache}) ->
    ets:delete(OldCache), 
    Cache = ets:new(code_cache, [protected]),
    rehash_cache(Cache, St).

rehash_cache(Cache, St = #state{path = Path}) ->
    Exts = [{obj,objfile_extension()}, {app,".app"}],
    {Cache,NewPath} = locate_mods(lists:reverse(Path), first, Exts, Cache, []),
    St#state{cache = Cache, path=NewPath}.

update_cache(Dirs, Where, Cache0) ->
    Exts = [{obj,objfile_extension()}, {app,".app"}],
    {Cache, _} = locate_mods(Dirs, Where, Exts, Cache0, []),
    Cache.

locate_mods([Dir0|Path], Where, Exts, Cache, Acc) ->
    Dir = absname(Dir0), %% Cache always expands the path 
    case erl_prim_loader:list_dir(Dir) of
	{ok, Files} -> 
	    Cache = filter_mods(Files, Where, Exts, Dir, Cache),
	    locate_mods(Path, Where, Exts, Cache, [Dir|Acc]);
	error ->
	    locate_mods(Path, Where, Exts, Cache, Acc)
    end;
locate_mods([], _, _, Cache, Path) ->
    {Cache,Path}.

filter_mods([File|Rest], Where, Exts, Dir, Cache) ->
    Ext = filename:extension(File),
    Root = list_to_atom(filename:rootname(File, Ext)),
    case lists:keyfind(Ext, 2, Exts) of
	{Type, _} ->
	    Key = {Type,Root},
	    case Where of
		first ->
		    true = ets:insert(Cache, {Key,Dir});
		last ->
		    case ets:lookup(Cache, Key) of
			[] ->
			    true = ets:insert(Cache, {Key,Dir});
			_ ->
			    ignore
		    end
	    end;
	false ->
	    ok
    end,
    filter_mods(Rest, Where, Exts, Dir, Cache);
filter_mods([], _, _, _, Cache) ->
    Cache.

%% --------------------------------------------------------------
%% Path handling functions.
%% --------------------------------------------------------------

%%
%% Create the initial path. 
%%
make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, [], []).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
		     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split(BaseName, "-") of
	[_, _|_] = Toks ->
	    VsnStr = lists:last(Toks),
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
		    Name = join(lists:sublist(Toks, length(Toks)-1),"-"),
		    {Name,VsnNum,FullName};
		false ->
		    {FullName,[0],FullName}
	    end;
	_ ->
	    {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
	true ->
	    {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
	_  ->
	    false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true; 
		  (_)                       -> false
	      end, Cs).

split(Cs, S) ->
    split1(Cs, S, []).

split1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, Toks);
	false -> split2(S, Seps, Toks, [C])
    end;
split1([], _Seps, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, [lists:reverse(Cs)|Toks]);
	false -> split2(S, Seps, Toks, [C|Cs])
    end;
split2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).
   
join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keyfind(Name, 1, Acc) of
	{_, NV, OldFullName} when NV =:= NumVsn ->
	    case filename:extension(OldFullName) =:= ArchiveExt of
		false ->
		    choose(Bs,Acc, ArchiveExt);
		true ->
		    Acc2 = lists:keystore(Name, 1, Acc, New),
		    choose(Bs,Acc2, ArchiveExt)
	    end;
	{_, _, _} ->
	    choose(Bs,Acc, ArchiveExt);
	false ->
	    choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Ebin = filename:append(Dir,"ebin"),
    %% First try with /ebin
    case erl_prim_loader:read_file_info(Ebin) of
	{ok,#file_info{type=directory}} ->
	    make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
	_ ->
	    %% Second try with archive
	    Ext = archive_extension(),
	    Base = filename:basename(Dir, Ext),
	    Ebin2 = filename:join([filename:dirname(Dir), Base ++ Ext, Base, "ebin"]),
	    Ebins = 
		case split(Base, "-") of
		    [_, _|_] = Toks ->
			AppName = join(lists:sublist(Toks, length(Toks)-1),"-"),
			Ebin3 = filename:join([filename:dirname(Dir), Base ++ Ext, AppName, "ebin"]),
			[Ebin3, Ebin2, Dir];
		    _ ->
			[Ebin2, Dir]
		end,
	    try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle, Bs)
    end.

try_ebin_dirs([Ebin | Ebins],BundleDir,Tail,Res,Bundle,Bs) ->
    case erl_prim_loader:read_file_info(Ebin) of
	{ok,#file_info{type=directory}} -> 
	    make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
	_ ->
	    try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle,Bs)
    end;
try_ebin_dirs([],BundleDir,Tail,Res,_Bundle,Bs) ->
    make_path(BundleDir,Tail,Res,Bs).


%%
%% Add the erl_prim_loader path.
%% 
%%
add_loader_path(IPath0,Mode) ->
    {ok,PrimP0} = erl_prim_loader:get_path(),
    case Mode of
        embedded ->
            strip_path(PrimP0, Mode);  % i.e. only normalize
        _ ->
            Pa0 = get_arg(pa),
            Pz0 = get_arg(pz),

            Pa = patch_path(Pa0),
            Pz = patch_path(Pz0),
	    PrimP = patch_path(PrimP0),
	    IPath = patch_path(IPath0),

            P = exclude_pa_pz(PrimP,Pa,Pz),
            Path0 = strip_path(P, Mode),
            Path = add(Path0, IPath, []),
            add_pa_pz(Path,Pa,Pz)
    end.

patch_path(Path) ->
    case check_path(Path) of
	{ok, NewPath} -> NewPath;
	{error, _Reason} -> Path
    end.	    

%% As the erl_prim_loader path includes the -pa and -pz
%% directories they have to be removed first !!
exclude_pa_pz(P0,Pa,Pz) ->
    P1 = excl(Pa, P0),
    P = excl(Pz, lists:reverse(P1)),
    lists:reverse(P).

excl([], P) -> 
    P;
excl([D|Ds], P) ->
    excl(Ds, lists:delete(D, P)).

%%
%% Keep only 'valid' paths in code server.
%% Only if mode is interactive, in an embedded
%% system we can't rely on file.
%%

strip_path([P0|Ps], Mode) ->
    P = filename:join([P0]), % Normalize
    case check_path([P]) of
	{ok, [NewP]} ->
	    [NewP|strip_path(Ps, Mode)];
	_ when Mode =:= embedded ->
	    [P|strip_path(Ps, Mode)];
	_ ->
	    strip_path(Ps, Mode)
    end;
strip_path(_, _) ->
    [].
    
%%
%% Add only non-existing paths.
%% Also delete other versions of directories,
%% e.g. .../test-3.2/ebin should exclude .../test-*/ebin (and .../test/ebin).
%% Put the Path directories first in resulting path.
%%
add(Path,["."|IPath],Acc) ->
    RPath = add1(Path,IPath,Acc),
    ["."|lists:delete(".",RPath)];
add(Path,IPath,Acc) ->
    add1(Path,IPath,Acc).

add1([P|Path],IPath,Acc) ->
    case lists:member(P,Acc) of
	true ->
	    add1(Path,IPath,Acc); % Already added
	false ->
	    IPath1 = exclude(P,IPath),
	    add1(Path,IPath1,[P|Acc])
    end;
add1(_,IPath,Acc) ->
    lists:reverse(Acc) ++ IPath.

add_pa_pz(Path0, Patha, Pathz) ->
    {_,Path1} = add_paths(first,Patha,Path0,false),
    {_,Path2} = add_paths(first,Pathz,lists:reverse(Path1),false),
    lists:reverse(Path2).

get_arg(Arg) ->
    case init:get_argument(Arg) of
	{ok, Values} ->
	    lists:append(Values);
	_ ->
	    []
    end.

%%
%% Exclude other versions of Dir or duplicates.
%% Return a new Path.
%%
exclude(Dir,Path) ->
    Name = get_name(Dir),
    [D || D <- Path, 
	  D =/= Dir, 
	  get_name(D) =/= Name].

%%
%% Get the "Name" of a directory. A directory in the code server path
%% have the following form: .../Name-Vsn or .../Name
%% where Vsn is any sortable term (the newest directory is sorted as
%% the greatest term).
%%
%%
get_name(Dir) ->
    get_name2(get_name1(Dir), []).

get_name1(Dir) ->
    case lists:reverse(filename:split(Dir)) of
	["ebin",DirName|_] -> DirName;
	[DirName|_]        -> DirName;
	_                  -> ""        % No name !
    end.

get_name2([$-|_],Acc) -> lists:reverse(Acc);
get_name2([H|T],Acc)  -> get_name2(T,[H|Acc]);
get_name2(_,Acc)      -> lists:reverse(Acc).

check_path(Path) ->
    PathChoice = init:code_path_choice(),
    ArchiveExt = archive_extension(),
    do_check_path(Path, PathChoice, ArchiveExt, []).
    
do_check_path([], _PathChoice, _ArchiveExt, Acc) -> 
    {ok, lists:reverse(Acc)};
do_check_path([Dir | Tail], PathChoice, ArchiveExt, Acc) ->
    case catch erl_prim_loader:read_file_info(Dir) of
	{ok, #file_info{type=directory}} -> 
	    do_check_path(Tail, PathChoice, ArchiveExt, [Dir | Acc]);
	_ when PathChoice =:= strict ->
	    %% Be strict. Only use dir as explicitly stated
	    {error, bad_directory};
	_ when PathChoice =:= relaxed ->
	    %% Be relaxed
	    case catch lists:reverse(filename:split(Dir)) of
		{'EXIT', _} ->
		    {error, bad_directory};
		["ebin", App] ->
		    Dir2 = filename:join([App ++ ArchiveExt, App, "ebin"]),
		    case erl_prim_loader:read_file_info(Dir2) of
			{ok, #file_info{type = directory}} ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Dir2 | Acc]);
			_ ->
			    {error, bad_directory}
		    end;    
		["ebin", App, OptArchive | RevTop] ->
		    Ext = filename:extension(OptArchive),
		    Base = filename:basename(OptArchive, Ext),
		    Dir2 = 
			if
			    Ext =:= ArchiveExt, Base =:= App ->
				%% .../app-vsn.ez/app-vsn/ebin
				Top = lists:reverse(RevTop),
				filename:join(Top ++ [App, "ebin"]);
			    Ext =:= ArchiveExt ->
				%% .../app-vsn.ez/xxx/ebin
				{error, bad_directory};
			    true ->
				%% .../app-vsn/ebin
				Top = lists:reverse([OptArchive | RevTop]),
				filename:join(Top ++ [App ++ ArchiveExt, App, "ebin"])
			end,
		    case erl_prim_loader:read_file_info(Dir2) of
			{ok, #file_info{type = directory}} ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Dir2 | Acc]);
			_ ->
			    {error, bad_directory}
		    end;    
		_ ->
		    {error, bad_directory}
	    end
    end.

%%
%% Add new path(s).
%%
add_path(Where,Dir,Path,NameDb) when is_atom(Dir) ->
    add_path(Where,atom_to_list(Dir),Path,NameDb);
add_path(Where,Dir0,Path,NameDb) when is_list(Dir0) ->
    case int_list(Dir0) of
	true ->
	    Dir = filename:join([Dir0]), % Normalize
	    case check_path([Dir]) of
		{ok, [NewDir]} ->
		    {true, do_add(Where,NewDir,Path,NameDb)};
		Error ->
		    {Error, Path}
	    end;
	false ->
	    {{error, bad_directory}, Path}
    end;
add_path(_,_,Path,_) ->
    {{error, bad_directory}, Path}.


%%
%% If the new directory is added first or if the directory didn't exist
%% the name-directory table must be updated.
%% If NameDb is false we should NOT update NameDb as it is done later
%% then the table is created :-)
%%
do_add(first,Dir,Path,NameDb) ->
    update(Dir, NameDb),
    [Dir|lists:delete(Dir,Path)];
do_add(last,Dir,Path,NameDb) ->
    case lists:member(Dir,Path) of
	true ->
	    Path;
	false ->
	    maybe_update(Dir, NameDb),
	    Path ++ [Dir]
    end.

%% Do not update if the same name already exists !
maybe_update(Dir, NameDb) ->
    (lookup_name(get_name(Dir), NameDb) =:= false) andalso update(Dir, NameDb).

update(_Dir, false) ->
    true;
update(Dir, NameDb) ->
    replace_name(Dir, NameDb).

%%
%% Set a completely new path.
%%
set_path(NewPath0, OldPath, NameDb) ->
    NewPath = normalize(NewPath0),
    case check_path(NewPath) of
	{ok, NewPath2} ->
	    ets:delete(NameDb),
	    NewDb = init_namedb(NewPath2),
	    {true, NewPath2, NewDb};
	Error ->
	    {Error, OldPath, NameDb}
    end.

%%
%% Normalize the given path.
%% The check_path function catches erroneous path,
%% thus it is ignored here.
%%
normalize([P|Path]) when is_atom(P) ->
    normalize([atom_to_list(P)|Path]);
normalize([P|Path]) when is_list(P) ->
    case int_list(P) of
	true  -> [filename:join([P])|normalize(Path)];
	false -> [P|normalize(Path)]
    end;
normalize([P|Path]) ->
    [P|normalize(Path)];
normalize([]) ->
    [];
normalize(Other) ->
    Other.

%% Handle a table of name-directory pairs.
%% The priv_dir/1 and lib_dir/1 functions will have
%% an O(1) lookup.
init_namedb(Path) ->
    Db = ets:new(code_names,[private]),
    init_namedb(lists:reverse(Path), Db),
    Db.
    
init_namedb([P|Path], Db) ->
    insert_name(P, Db),
    init_namedb(Path, Db);
init_namedb([], _) ->
    ok.

-ifdef(NOTUSED).
clear_namedb([P|Path], Db) ->
    delete_name_dir(P, Db),
    clear_namedb(Path, Db);
clear_namedb([], _) ->
    ok.
-endif.

insert_name(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name -> insert_name(Name, Dir, Db)
    end.

insert_name(Name, Dir, Db) ->
    AppDir = del_ebin(Dir),
    {Base, SubDirs} = archive_subdirs(AppDir),
    ets:insert(Db, {Name, AppDir, Base, SubDirs}),
    true.

archive_subdirs(AppDir) ->
    IsDir =
	fun(RelFile) ->
		File = filename:join([AppDir, RelFile]),
		case erl_prim_loader:read_file_info(File) of
		    {ok, #file_info{type = directory}} ->
			false;
		    _ ->
			true
		end
	end,
    {Base, ArchiveDirs} = all_archive_subdirs(AppDir),
    {Base, lists:filter(IsDir, ArchiveDirs)}.

all_archive_subdirs(AppDir) ->
    Ext = archive_extension(),
    Base = filename:basename(AppDir),
    Dirs = 
	case split(Base, "-") of
	    [_, _|_] = Toks ->
		Base2 = join(lists:sublist(Toks, length(Toks)-1), "-"),
		[Base2, Base];
	    _ ->
		[Base]
	end,
    try_archive_subdirs(AppDir ++ Ext, Base, Dirs).

try_archive_subdirs(Archive, Base, [Dir | Dirs]) ->
    ArchiveDir = filename:join([Archive, Dir]),
    case erl_prim_loader:list_dir(ArchiveDir) of
	{ok, Files} ->
	    IsDir =
		fun(RelFile) ->
			File = filename:join([ArchiveDir, RelFile]),
			case erl_prim_loader:read_file_info(File) of
			    {ok, #file_info{type = directory}} ->
				true;
			    _ ->
				false
			end
		end,
	    {Dir, lists:filter(IsDir, Files)};
	_ ->
	    try_archive_subdirs(Archive, Base, Dirs)
    end;
try_archive_subdirs(_Archive, Base, []) ->
    {Base, []}.

%%
%% Delete a directory from Path.
%% Name can be either the the name in .../Name[-*] or
%% the complete directory name.
%%
del_path(Name0,Path,NameDb) ->
    case catch filename:join([to_list(Name0)]) of
	{'EXIT',_} ->
	    {{error,bad_name},Path};
	Name ->
	    case del_path1(Name,Path,NameDb) of
		Path -> % Nothing has changed
		    {false,Path};
		NewPath ->
		    {true,NewPath}
	    end
    end.

del_path1(Name,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    delete_name(Name, NameDb),
	    insert_old_shadowed(Name, Path, NameDb),
	    Path;
	_ when Name =:= P ->
	    case delete_name_dir(Name, NameDb) of
		true -> insert_old_shadowed(get_name(Name), Path, NameDb);
		false -> ok
	    end,
	    Path;
	_ ->
	    [P|del_path1(Name,Path,NameDb)]
    end;
del_path1(_,[],_) ->
    [].

insert_old_shadowed(Name, [P|Path], NameDb) ->
    case get_name(P) of
	Name -> insert_name(Name, P, NameDb);
	_    -> insert_old_shadowed(Name, Path, NameDb)
    end;
insert_old_shadowed(_, [], _) ->
    ok.

%%
%% Replace an old occurrence of an directory with name .../Name[-*].
%% If it does not exist, put the new directory last in Path.
%%
replace_path(Name,Dir,Path,NameDb) ->
    case catch check_pars(Name,Dir) of
	{ok,N,D} ->
	    {true,replace_path1(N,D,Path,NameDb)};
	{'EXIT',_} ->
	    {{error,{badarg,[Name,Dir]}},Path};
	Error ->
	    {Error,Path}
    end.

replace_path1(Name,Dir,[P|Path],NameDb) ->
    case get_name(P) of
	Name ->
	    insert_name(Name, Dir, NameDb),
	    [Dir|Path];
	_ ->
	    [P|replace_path1(Name,Dir,Path,NameDb)]
    end;
replace_path1(Name, Dir, [], NameDb) ->
    insert_name(Name, Dir, NameDb),
    [Dir].

check_pars(Name,Dir) ->
    N = to_list(Name),
    D = filename:join([to_list(Dir)]), % Normalize
    case get_name(Dir) of
	N ->
	    case check_path([D]) of
		{ok, [NewD]} ->
		    {ok,N,NewD};
		Error ->
		    Error
	    end;
	_ ->
	    {error,bad_name}
    end.

del_ebin(Dir) ->
    case filename:basename(Dir) of
	"ebin" -> 
	    Dir2 = filename:dirname(Dir),
	    Dir3 = filename:dirname(Dir2),
	    Ext = archive_extension(),
	    case filename:extension(Dir3) of
		E when E =:= Ext ->
		    %% Strip archive extension
		    filename:join([filename:dirname(Dir3), 
				   filename:basename(Dir3, Ext)]);
		_ ->
		    Dir2
	    end;
	_ ->
	    Dir
    end.

replace_name(Dir, Db) ->
    case get_name(Dir) of
	Dir ->
	    false;
	Name ->
	    delete_name(Name, Db),
	    insert_name(Name, Dir, Db)
    end.

delete_name(Name, Db) ->
    ets:delete(Db, Name).

delete_name_dir(Dir, Db) ->
    case get_name(Dir) of
	Dir  -> false;
	Name ->
	    Dir0 = del_ebin(Dir),
	    case lookup_name(Name, Db) of
		{ok, Dir0, _Base, _SubDirs} ->
		    ets:delete(Db, Name), 
		    true;
		_ -> false
	    end
    end.

lookup_name(Name, Db) ->
    case ets:lookup(Db, Name) of
	[{Name, Dir, Base, SubDirs}] -> {ok, Dir, Base, SubDirs};
	_ -> false
    end.


%%
%% Fetch a directory.
%%
do_dir(Root,lib_dir,_) ->
    filename:append(Root, "lib");
do_dir(Root,root_dir,_) ->
    Root;
do_dir(_Root,compiler_dir,NameDb) ->
    case lookup_name("compiler", NameDb) of
	{ok, Dir, _Base, _SubDirs} -> Dir;
	_  -> ""
    end;
do_dir(_Root,{lib_dir,Name},NameDb) ->
    case catch lookup_name(to_list(Name), NameDb) of
	{ok, Dir, _Base, _SubDirs} -> Dir;
	_         -> {error, bad_name}
    end;
do_dir(_Root,{lib_dir,Name,SubDir0},NameDb) ->
    SubDir = atom_to_list(SubDir0),
    case catch lookup_name(to_list(Name), NameDb) of
	{ok, Dir, Base, SubDirs} ->
	    case lists:member(SubDir, SubDirs) of
		true ->
		    %% Subdir is in archive
		    filename:join([Dir ++ archive_extension(),
				   Base,
				   SubDir]);
		false ->
		    %% Subdir is regular directory
		    filename:join([Dir, SubDir])
	    end;
	_  -> 
	    {error, bad_name}
    end;
do_dir(_Root,{priv_dir,Name},NameDb) ->
    do_dir(_Root,{lib_dir,Name,priv},NameDb);
do_dir(_, _, _) ->
    'bad request to code'.

stick_dir(Dir, Stick, St) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok,Listing} ->
	    Mods = get_mods(Listing, objfile_extension()),
	    Db = St#state.moddb,
	    case Stick of
		true ->
		    foreach(fun (M) -> ets:insert(Db, {{sticky,M},true}) end, Mods);
		false ->
		    foreach(fun (M) -> ets:delete(Db, {sticky,M}) end, Mods)
	    end;
	Error -> 
	    Error
    end.

stick_mod(M, Stick, St) ->
    Db = St#state.moddb,
    case Stick of
	true ->
	    ets:insert(Db, {{sticky,M},true});
	false ->
	    ets:delete(Db, {sticky,M})
    end.

get_mods([File|Tail], Extension) ->
    case filename:extension(File) of
	Extension ->
	    [list_to_atom(filename:basename(File, Extension)) |
	     get_mods(Tail, Extension)];
	_ ->
	    get_mods(Tail, Extension)
    end;
get_mods([], _) -> [].

is_sticky(Mod, Db) ->
    erlang:module_loaded(Mod) andalso (ets:lookup(Db, {sticky, Mod}) =/= []).

add_paths(Where,[Dir|Tail],Path,NameDb) ->
    {_,NPath} = add_path(Where,Dir,Path,NameDb),
    add_paths(Where,Tail,NPath,NameDb);
add_paths(_,_,Path,_) ->
    {ok,Path}.

do_load_binary(Module, File, Binary, Caller, St) ->
    case modp(Module) andalso modp(File) andalso is_binary(Binary) of
	true ->
	    case erlang:module_loaded(to_atom(Module)) of
		true -> do_purge(Module);
		false -> ok
	    end,
	    try_load_module(File, Module, Binary, Caller, St);
	false ->
	    {reply,{error,badarg},St}
    end.

modp(Atom) when is_atom(Atom) -> true;
modp(List) when is_list(List) -> int_list(List);
modp(_)                       -> false.

load_abs(File, Mod0, Caller, St) ->
    Ext = objfile_extension(),
    FileName0 = lists:concat([File, Ext]),
    FileName = absname(FileName0),
    Mod = if Mod0 =:= [] ->
		  list_to_atom(filename:basename(FileName0, Ext));
	     true ->
		  Mod0
	  end,
    case erl_prim_loader:get_file(FileName) of
	{ok,Bin,_} ->
	    try_load_module(FileName, Mod, Bin, Caller, St);
	error ->
	    {reply,{error,nofile},St}
    end.

try_load_module(Mod, Dir, Caller, St) ->
    File = filename:append(Dir, to_list(Mod) ++
			   objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    {reply,error,St};
	{ok,Binary,FName} ->
	    try_load_module(absname(FName), Mod, Binary, Caller, St)
    end.

try_load_module(File, Mod, Bin, {From,_}=Caller, St0) ->
    M = to_atom(Mod),
    case pending_on_load(M, From, St0) of
	no ->
	    try_load_module_1(File, M, Bin, Caller, St0);
	{yes,St} ->
	    {noreply,St}
    end.

try_load_module_1(File, Mod, Bin, Caller, #state{moddb=Db}=St) ->
    case is_sticky(Mod, Db) of
	true ->                         %% Sticky file reject the load
	    error_msg("Can't load module that resides in sticky dir\n",[]),
	    {reply,{error,sticky_directory},St};
	false ->
	    case catch load_native_code(Mod, Bin) of
		{module,Mod} = Module ->
		    ets:insert(Db, {Mod,File}),
		    {reply,Module,St};
		no_native ->
		    case erlang:load_module(Mod, Bin) of
			{module,Mod} = Module ->
			    ets:insert(Db, {Mod,File}),
			    post_beam_load(Mod),
			    {reply,Module,St};
			{error,on_load} ->
			    handle_on_load(Mod, File, Caller, St);
			{error,What} = Error ->
			    error_msg("Loading of ~ts failed: ~p\n", [File, What]),
			    {reply,Error,St}
		    end;
		Error ->
		    error_msg("Native loading of ~ts failed: ~p\n",
			      [File,Error]),
		    {reply,ok,St}
	    end
    end.

load_native_code(Mod, Bin) ->
    %% During bootstrapping of Open Source Erlang, we don't have any hipe
    %% loader modules, but the Erlang emulator might be hipe enabled.
    %% Therefore we must test for that the loader modules are available
    %% before trying to to load native code.
    case erlang:module_loaded(hipe_unified_loader) of
	false ->
	    no_native;
	true ->
	    Result = hipe_unified_loader:load_native_code(Mod, Bin),
	    case Result of
		{module,_} ->
		    put(?ANY_NATIVE_CODE_LOADED, true);
		_ ->
		    ok
	    end,
	    Result
    end.

hipe_result_to_status(Result) ->
    case Result of
	{module,_} ->
	    put(?ANY_NATIVE_CODE_LOADED, true),
	    Result;
	_ ->
	    {error,Result}
    end.

post_beam_load(Mod) ->
    %% post_beam_load/1 can potentially be very expensive because it
    %% blocks multi-scheduling; thus we want to avoid the call if we
    %% know that it is not needed.
    case get(?ANY_NATIVE_CODE_LOADED) of
	true -> hipe_unified_loader:post_beam_load(Mod);
	false -> ok
    end.

int_list([H|T]) when is_integer(H) -> int_list(T);
int_list([_|_])                    -> false;
int_list([])                       -> true.

load_file(Mod0, {From,_}=Caller, St0) ->
    Mod = to_atom(Mod0),
    case pending_on_load(Mod, From, St0) of
	no -> load_file_1(Mod, Caller, St0);
	{yes,St} -> {noreply,St}
    end.

load_file_1(Mod, Caller, #state{path=Path,cache=no_cache}=St) ->
    case mod_to_bin(Path, Mod) of
	error ->
	    {reply,{error,nofile},St};
	{Mod,Binary,File} ->
	    try_load_module(File, Mod, Binary, Caller, St)
    end;
load_file_1(Mod, Caller, #state{cache=Cache}=St0) ->
    Key = {obj,Mod},
    case ets:lookup(Cache, Key) of
	[] -> 
	    St = rehash_cache(St0),
	    case ets:lookup(St#state.cache, Key) of
		[] -> 
		    {reply,{error,nofile},St};
		[{Key,Dir}] ->
		    try_load_module(Mod, Dir, Caller, St)
	    end;
	[{Key,Dir}] ->
	    try_load_module(Mod, Dir, Caller, St0)
    end.

mod_to_bin([Dir|Tail], Mod) ->
    File = filename:append(Dir, to_list(Mod) ++ objfile_extension()),
    case erl_prim_loader:get_file(File) of
	error -> 
	    mod_to_bin(Tail, Mod);
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end;
mod_to_bin([], Mod) ->
    %% At last, try also erl_prim_loader's own method
    File = to_list(Mod) ++ objfile_extension(),
    case erl_prim_loader:get_file(File) of
	error -> 
	    error;     % No more alternatives !
	{ok,Bin,FName} ->
	    {Mod,Bin,absname(FName)}
    end.

absname(File) ->
    case erl_prim_loader:get_cwd() of
	{ok,Cwd} -> absname(File, Cwd);
	_Error -> File
    end.

absname(Name, AbsBase) ->
    case filename:pathtype(Name) of
	relative ->
	    filename:absname_join(AbsBase, Name);
	absolute ->
	    %% We must flatten the filename before passing it into join/1,
	    %% or we will get slashes inserted into the wrong places.
	    filename:join([filename:flatten(Name)]);
	volumerelative ->
	    absname_vr(filename:split(Name), filename:split(AbsBase), AbsBase)
    end.

%% Handles volumerelative names (on Windows only).

absname_vr(["/"|Rest1], [Volume|_], _AbsBase) ->
    %% Absolute path on current drive.
    filename:join([Volume|Rest1]);
absname_vr([[X, $:]|Rest1], [[X|_]|_], AbsBase) ->
    %% Relative to current directory on current drive.
    absname(filename:join(Rest1), AbsBase);
absname_vr([[X, $:]|Name], _, _AbsBase) ->
    %% Relative to current directory on another drive.
    Dcwd =
	case erl_prim_loader:get_cwd([X, $:]) of
	    {ok, Dir}  -> Dir;
	    error -> [X, $:, $/]
    end,
    absname(filename:join(Name), Dcwd).


%% do_purge(Module)
%%  Kill all processes running code from *old* Module, and then purge the
%%  module. Return true if any processes killed, else false.

do_purge(Mod0) ->
    Mod = to_atom(Mod0),
    case erlang:check_old_code(Mod) of
	false ->
	    false;
	true ->
	    Res = check_proc_code(erlang:processes(), Mod, true),
	    try
		erlang:purge_module(Mod)
	    catch
		_:_ -> ignore
	    end,
	    Res
    end.

%% do_soft_purge(Module)
%% Purge old code only if no procs remain that run old code.
%% Return true in that case, false if procs remain (in this
%% case old code is not purged)

do_soft_purge(Mod0) ->
    Mod = to_atom(Mod0),
    case erlang:check_old_code(Mod) of
	false ->
	    true;
	true ->
	    case check_proc_code(erlang:processes(), Mod, false) of
		false ->
		    false;
		true ->
		    try
			erlang:purge_module(Mod)
		    catch
			_:_ -> ignore
		    end,
		    true
	    end
    end.

%%
%% check_proc_code(Pids, Mod, Hard) - Send asynchronous
%%   requests to all processes to perform a check_process_code
%%   operation. Each process will check their own state and
%%   reply with the result. If 'Hard' equals
%%   - true, processes that refer 'Mod' will be killed. If
%%     any processes were killed true is returned; otherwise,
%%     false.
%%   - false, and any processes refer 'Mod', false will
%%     returned; otherwise, true.
%%
%%   Requests will be sent to all processes identified by
%%   Pids at once, but without allowing GC to be performed.
%%   Check process code operations that are aborted due to
%%   GC need, will be restarted allowing GC. However, only
%%   ?MAX_CPC_GC_PROCS outstanding operation allowing GC at
%%   a time will be allowed. This in order not to blow up
%%   memory wise.
%%
%%   We also only allow ?MAX_CPC_NO_OUTSTANDING_KILLS
%%   outstanding kills. This both in order to avoid flooding
%%   our message queue with 'DOWN' messages and limiting the
%%   amount of memory used to keep references to all
%%   outstanding kills.
%%

%% We maybe should allow more than two outstanding
%% GC requests, but for now we play it safe...
-define(MAX_CPC_GC_PROCS, 2).
-define(MAX_CPC_NO_OUTSTANDING_KILLS, 10).

-record(cpc_static, {hard, module, tag}).

-record(cpc_kill, {outstanding = [],
		   no_outstanding = 0,
		   waiting = [],
		   killed = false}).

check_proc_code(Pids, Mod, Hard) ->
    Tag = erlang:make_ref(),
    CpcS = #cpc_static{hard = Hard,
		       module = Mod,
		       tag = Tag},
    check_proc_code(CpcS, cpc_init(CpcS, Pids, 0), 0, [], #cpc_kill{}, true).

check_proc_code(#cpc_static{hard = true}, 0, 0, [],
		#cpc_kill{outstanding = [], waiting = [], killed = Killed},
		true) ->
    %% No outstanding requests. We did a hard check, so result is whether or
    %% not we killed any processes...
    Killed;
check_proc_code(#cpc_static{hard = false}, 0, 0, [], _KillState, Success) ->
    %% No outstanding requests and we did a soft check...
    Success;
check_proc_code(#cpc_static{hard = false, tag = Tag} = CpcS, NoReq0, NoGcReq0,
		[], _KillState, false) ->
    %% Failed soft check; just cleanup the remaining replies corresponding
    %% to the requests we've sent...
    {NoReq1, NoGcReq1} = receive
			     {check_process_code, {Tag, _P, GC}, _Res} ->
				 case GC of
				     false -> {NoReq0-1, NoGcReq0};
				     true -> {NoReq0, NoGcReq0-1}
				 end
			 end,
    check_proc_code(CpcS, NoReq1, NoGcReq1, [], _KillState, false);
check_proc_code(#cpc_static{tag = Tag} = CpcS, NoReq0, NoGcReq0, NeedGC0,
		KillState0, Success) ->

    %% Check if we should request a GC operation
    {NoGcReq1, NeedGC1} = case NoGcReq0 < ?MAX_CPC_GC_PROCS of
			      GcOpAllowed when GcOpAllowed == false;
					       NeedGC0 == [] ->
				  {NoGcReq0, NeedGC0};
			      _ ->
				  {NoGcReq0+1, cpc_request_gc(CpcS,NeedGC0)}
			  end,

    %% Wait for a cpc reply or 'DOWN' message
    {NoReq1, NoGcReq2, Pid, Result, KillState1} = cpc_recv(Tag,
							   NoReq0,
							   NoGcReq1,
							   KillState0),

    %% Check the result of the reply
    case Result of
	aborted ->
	    %% Operation aborted due to the need to GC in order to
	    %% determine if the process is referring the module.
	    %% Schedule the operation for restart allowing GC...
	    check_proc_code(CpcS, NoReq1, NoGcReq2, [Pid|NeedGC1], KillState1,
			    Success);
	false ->
	    %% Process not referring the module; done with this process...
	    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1, KillState1,
			    Success);
	true ->
	    %% Process referring the module...
	    case CpcS#cpc_static.hard of
		false ->
		    %% ... and soft check. The whole operation failed so
		    %% no point continuing; clean up and fail...
		    check_proc_code(CpcS, NoReq1, NoGcReq2, [], KillState1,
				    false);
		true ->
		    %% ... and hard check; schedule kill of it...
		    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1,
				    cpc_sched_kill(Pid, KillState1), Success)
	    end;
	'DOWN' ->
	    %% Handled 'DOWN' message
	    check_proc_code(CpcS, NoReq1, NoGcReq2, NeedGC1,
			    KillState1, Success)
    end.

cpc_recv(Tag, NoReq, NoGcReq, #cpc_kill{outstanding = []} = KillState) ->
    receive
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end;
cpc_recv(Tag, NoReq, NoGcReq,
	 #cpc_kill{outstanding = [R0, R1, R2, R3, R4 | _]} = KillState) ->
    receive
	{'DOWN', R, process, _, _} when R == R0;
					R == R1;
					R == R2;
					R == R3;
					R == R4 ->
	    cpc_handle_down(NoReq, NoGcReq, R, KillState); 
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end;
cpc_recv(Tag, NoReq, NoGcReq, #cpc_kill{outstanding = [R|_]} = KillState) ->
    receive
	{'DOWN', R, process, _, _} ->
	    cpc_handle_down(NoReq, NoGcReq, R, KillState); 
	{check_process_code, {Tag, Pid, GC}, Res} ->
	    cpc_handle_cpc(NoReq, NoGcReq, GC, Pid, Res, KillState)
    end.

cpc_handle_down(NoReq, NoGcReq, R, #cpc_kill{outstanding = Rs,
					     no_outstanding = N} = KillState) ->
    {NoReq, NoGcReq, undefined, 'DOWN',
     cpc_sched_kill_waiting(KillState#cpc_kill{outstanding = cpc_list_rm(R, Rs),
					       no_outstanding = N-1})}.

cpc_list_rm(R, [R|Rs]) ->
    Rs;
cpc_list_rm(R0, [R1|Rs]) ->
    [R1|cpc_list_rm(R0, Rs)].

cpc_handle_cpc(NoReq, NoGcReq, false, Pid, Res, KillState) ->
    {NoReq-1, NoGcReq, Pid, Res, KillState};
cpc_handle_cpc(NoReq, NoGcReq, true, Pid, Res, KillState) ->
    {NoReq, NoGcReq-1, Pid, Res, KillState}.

cpc_sched_kill_waiting(#cpc_kill{waiting = []} = KillState) ->
    KillState;
cpc_sched_kill_waiting(#cpc_kill{outstanding = Rs,
				 no_outstanding = N,
				 waiting = [P|Ps]} = KillState) ->
    R = erlang:monitor(process, P),
    exit(P, kill),
    KillState#cpc_kill{outstanding = [R|Rs],
		       no_outstanding = N+1,
		       waiting = Ps,
		       killed = true}.

cpc_sched_kill(Pid, #cpc_kill{no_outstanding = N, waiting = Pids} = KillState)
  when N >= ?MAX_CPC_NO_OUTSTANDING_KILLS ->
    KillState#cpc_kill{waiting = [Pid|Pids]};
cpc_sched_kill(Pid,
	       #cpc_kill{outstanding = Rs, no_outstanding = N} = KillState) ->
    R = erlang:monitor(process, Pid),
    exit(Pid, kill),
    KillState#cpc_kill{outstanding = [R|Rs],
		       no_outstanding = N+1,
		       killed = true}.

cpc_request(#cpc_static{tag = Tag, module = Mod}, Pid, AllowGc) ->
    erlang:check_process_code(Pid, Mod, [{async, {Tag, Pid, AllowGc}},
					 {allow_gc, AllowGc}]).

cpc_request_gc(CpcS, [Pid|Pids]) ->
    cpc_request(CpcS, Pid, true),
    Pids.

cpc_init(_CpcS, [], NoReqs) ->
    NoReqs;
cpc_init(CpcS, [Pid|Pids], NoReqs) ->
    cpc_request(CpcS, Pid, false),
    cpc_init(CpcS, Pids, NoReqs+1).

% end of check_proc_code() implementation.

is_loaded(M, Db) ->
    case ets:lookup(Db, M) of
	[{M,File}] -> {file,File};
	[] -> false
    end.

%% -------------------------------------------------------
%% The on_load functionality.
%% -------------------------------------------------------

handle_on_load(Mod, File, {From,_}, #state{on_load=OnLoad0}=St0) ->
    Fun = fun() ->
		  Res = erlang:call_on_load_function(Mod),
		  exit(Res)
	  end,
    {_,Ref} = spawn_monitor(Fun),
    OnLoad = [{Ref,Mod,File,[From]}|OnLoad0],
    St = St0#state{on_load=OnLoad},
    {noreply,St}.

pending_on_load(_, _, #state{on_load=[]}) ->
    no;
pending_on_load(Mod, From, #state{on_load=OnLoad0}=St) ->
    case lists:keymember(Mod, 2, OnLoad0) of
	false ->
	    no;
	true ->
	    OnLoad = pending_on_load_1(Mod, From, OnLoad0),
	    {yes,St#state{on_load=OnLoad}}
    end.

pending_on_load_1(Mod, From, [{Ref,Mod,File,Pids}|T]) ->
    [{Ref,Mod,File,[From|Pids]}|T];
pending_on_load_1(Mod, From, [H|T]) ->
    [H|pending_on_load_1(Mod, From, T)];
pending_on_load_1(_, _, []) -> [].

finish_on_load(Ref, OnLoadRes, #state{on_load=OnLoad0,moddb=Db}=State) ->
    case lists:keyfind(Ref, 1, OnLoad0) of
	false ->
	    %% Since this process in general silently ignores messages
	    %% it doesn't understand, it should also ignore a 'DOWN'
	    %% message with an unknown reference.
	    State;
	{Ref,Mod,File,WaitingPids} ->
	    finish_on_load_1(Mod, File, OnLoadRes, WaitingPids, Db),
	    OnLoad = [E || {R,_,_,_}=E <- OnLoad0, R =/= Ref],
	    State#state{on_load=OnLoad}
    end.

finish_on_load_1(Mod, File, OnLoadRes, WaitingPids, Db) ->
    Keep = OnLoadRes =:= ok,
    erlang:finish_after_on_load(Mod, Keep),
    Res = case Keep of
	      false ->
		  _ = finish_on_load_report(Mod, OnLoadRes),
		  {error,on_load_failure};
	      true ->
		  ets:insert(Db, {Mod,File}),
		  {module,Mod}
	  end,
    _ = [reply(Pid, Res) || Pid <- WaitingPids],
    ok.

finish_on_load_report(_Mod, Atom) when is_atom(Atom) ->
    %% No error reports for atoms.
    ok;
finish_on_load_report(Mod, Term) ->
    %% Play it very safe here. The error_logger module and
    %% modules it depend on may not be loaded yet and there
    %% would be a dead-lock if we called it directly
    %% from the code_server process.
    spawn(fun() ->
		  F = "The on_load function for module "
		      "~s returned ~P\n",

		  %% Express the call as an apply to simplify
		  %% the ext_mod_dep/1 test case.
		  E = error_logger,
		  E:warning_msg(F, [Mod,Term,10])
	  end).

%% -------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------

all_loaded(Db) ->
    all_l(Db, ets:slot(Db, 0), 1, []).

all_l(_Db, '$end_of_table', _, Acc) ->
    Acc;
all_l(Db, ModInfo, N, Acc) ->
    NewAcc = strip_mod_info(ModInfo,Acc),
    all_l(Db, ets:slot(Db, N), N + 1, NewAcc).


strip_mod_info([{{sticky,_},_}|T], Acc) -> strip_mod_info(T, Acc);
strip_mod_info([H|T], Acc)              -> strip_mod_info(T, [H|Acc]);
strip_mod_info([], Acc)                 -> Acc.

% error_msg(Format) ->
%     error_msg(Format,[]).
error_msg(Format, Args) ->
    Msg = {notify,{error, group_leader(), {self(), Format, Args}}},
    error_logger ! Msg,
    ok.

info_msg(Format, Args) ->
    Msg = {notify,{info_msg, group_leader(), {self(), Format, Args}}},
    error_logger ! Msg,
    ok.

objfile_extension() ->
    init:objfile_extension().

archive_extension() ->
    init:archive_extension().

to_list(X) when is_list(X) -> X;
to_list(X) when is_atom(X) -> atom_to_list(X).

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X).
