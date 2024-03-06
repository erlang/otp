%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
-module(code_server).

%% This file holds the server part of the code_server.

-export([start_link/1,
	 call/1, absname/1,
	 is_loaded/1, is_sticky/1,
	 system_code_change/4,
	 error_msg/2, info_msg/2
	]).

-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-import(lists, [foreach/2]).
-define(moddb, code_server).

-type on_load_action() ::
	fun((term(), state()) -> {'reply',term(),state()} |
				 {'noreply',state()}).

-type on_load_item() :: {{pid(),reference()},module(),
			 [{pid(),on_load_action()}]}.

-record(state, {supervisor :: pid(),
		root :: file:name_all(),
		path :: [{file:name_all(), cache | nocache}],
		moddb :: ets:table(),
		namedb :: ets:table(),
		mode = interactive :: 'interactive' | 'embedded',
		on_load = [] :: [on_load_item()],
                loading = #{} :: #{module() => [pid()]}}).
-type state() :: #state{}.

-spec start_link([term()]) -> {'ok', pid()}.
start_link(Args) ->
    Ref = make_ref(),
    Parent = self(),
    Init = fun() -> init(Ref, Parent, Args) end,
    spawn_link(Init),
    receive 
	{Ref,Res} -> Res
    end.

is_loaded(Mod) ->
    case ets:lookup(?moddb, Mod) of
        [{Mod,File}] -> {file,File};
        [] -> false
    end.

is_sticky(Mod) ->
    is_sticky(Mod, ?moddb).

%% -----------------------------------------------------------
%% Init the code_server process.
%% -----------------------------------------------------------

init(Ref, Parent, [Root,Mode]) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),

    Db = ets:new(?moddb, [named_table, protected]),
    foreach(fun (M) ->
		    %% Pre-loaded modules are always sticky.
		    ets:insert(Db, [{M,preloaded},{{sticky,M},true}])
	    end, erlang:pre_loaded()),
    Loaded0 = init:fetch_loaded(),
    Loaded = [{M,filename:join([P])} || {M,P} <- Loaded0], %Normalize.
    ets:insert(Db, Loaded),

    IPath =
	case Mode of
	    interactive ->
		LibDir = filename:append(Root, "lib"),
		{ok,Dirs} = erl_prim_loader:list_dir(LibDir),
		Paths = make_path(LibDir, Dirs),
		UserLibPaths = get_user_lib_dirs(),
		["."] ++ UserLibPaths ++ Paths;
	    _ ->
		[]
	end,

    Path = add_loader_path(IPath, Mode),
    State = #state{supervisor = Parent,
		   root = Root,
		   path = Path,
		   moddb = Db,
		   namedb = create_namedb(Path, Root),
		   mode = Mode},

    Parent ! {Ref,{ok,self()}},
    loop(State).

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
	    Paths = make_path(Dir, Dirs),
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

-spec call(term()) -> term().
call(Req) ->
    Ref = erlang:monitor(process, ?MODULE),
    ?MODULE ! {code_call, self(), Req},
    receive 
	{?MODULE, Reply} ->
            erlang:demonitor(Ref,[flush]),
	    Reply;
        {'DOWN',Ref,process,_,_} ->
            exit({'DOWN',code_server,Req})
    end.

reply(Pid, Res) ->
    Pid ! {?MODULE, Res}.

loop(#state{supervisor=Supervisor}=State0) ->
    receive 
	{code_call, Pid, Req} ->
	    case handle_call(Req, Pid, State0) of
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
	{'DOWN',Ref,process,Pid,Res} ->
	    State = finish_on_load({Pid,Ref}, Res, State0),
	    loop(State);
        {{'LOADER_DOWN', Info}, _Ref, process, _Pid, _Res} ->
            State = loader_down(State0, Info),
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

handle_call({stick_dir,Dir}, _From, S) ->
    {reply,stick_dir(Dir, true, S),S};

handle_call({unstick_dir,Dir}, _From, S) ->
    {reply,stick_dir(Dir, false, S),S};

handle_call({stick_mod,Mod}, _From, S) ->
    {reply,stick_mod(Mod, true, S),S};

handle_call({unstick_mod,Mod}, _From, S) ->
    {reply,stick_mod(Mod, false, S),S};

handle_call({dir,Dir}, _From, S) ->
    Root = S#state.root,
    Resp = do_dir(Root,Dir,S#state.namedb),
    {reply,Resp,S};

handle_call({add_path,Where,Dir0,Cache}, _From,
	    #state{namedb=Namedb,path=Path0}=S) ->
    {Resp,Path} = add_path(Where, Dir0, Path0, Cache, Namedb),
    {reply,Resp,S#state{path=Path}};

handle_call({add_paths,Where,Dirs0,Cache}, _From,
	    #state{namedb=Namedb,path=Path0}=S) ->
    {Resp,Path} = add_paths(Where, Dirs0, Path0, Cache, Namedb),
    {reply,Resp,S#state{path=Path}};

handle_call({set_path,PathList,Cache}, _From,
	    #state{root=Root,path=Path0,namedb=Namedb}=S) ->
    {Resp,Path,NewDb} = set_path(PathList, Path0, Cache, Namedb, Root),
    {reply,Resp,S#state{path=Path,namedb=NewDb}};

handle_call({del_path,Name}, _From,
	    #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path} = del_path(Name, Path0, Namedb),
    {reply,Resp,S#state{path=Path}};

handle_call({del_paths,Names}, _From,
            #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path} = del_paths(Names, Path0, Namedb),
    {reply,Resp,S#state{path=Path}};

handle_call({replace_path,Name,Dir,Cache}, _From,
	    #state{path=Path0,namedb=Namedb}=S) ->
    {Resp,Path} = replace_path(Name, Dir, Path0, Cache, Namedb),
    {reply,Resp,S#state{path=Path}};

handle_call(get_path, _From, S) ->
    {reply,[P || {P, _Cache} <- S#state.path],S};

handle_call(clear_cache, _From, S) ->
    Path = [{P, if is_atom(Cache) -> Cache; true -> cache end} ||
            {P, Cache} <- S#state.path],
    {reply,ok,S#state{path=Path}};

handle_call({load_module,PC,Mod,File,Purge,EnsureLoaded}, From, S)
  when is_atom(Mod) ->
    case Purge andalso erlang:module_loaded(Mod) of
	true -> do_purge(Mod);
	false -> ok
    end,
    try_finish_module(File, Mod, PC, EnsureLoaded, From, S);

handle_call({load_error,Ref,Mod,Error}, _From, S) ->
    reply_loading(Ref, Mod, Error, S);

handle_call({delete,Mod}, _From, St) when is_atom(Mod) ->
    case catch erlang:delete_module(Mod) of
	true ->
	    ets:delete(St#state.moddb, Mod),
	    {reply,true,St};
	_ ->
	    {reply,false,St}
    end;

handle_call({purge,Mod}, _From, St) when is_atom(Mod) ->
    {reply,do_purge(Mod),St};

handle_call({soft_purge,Mod}, _From, St) when is_atom(Mod) ->
    {reply,do_soft_purge(Mod),St};

handle_call(all_loaded, _From, S) ->
    Db = S#state.moddb,
    {reply,all_loaded(Db),S};

handle_call({get_object_code,Mod}, _From, St0) when is_atom(Mod) ->
    case get_object_code(St0, Mod) of
	{Bin,FName,St1} -> {reply,{Mod,Bin,FName},St1};
	{error,St1} -> {reply,error,St1}
    end;

handle_call({get_object_code_for_loading,Mod}, From, St0) when is_atom(Mod) ->
    case erlang:module_loaded(Mod) of
        true -> {reply, {module, Mod}, St0};
        false when St0#state.mode =:= interactive ->
            %% Handles pending on_load events first. If the code is being
            %% loaded, finish before adding more entries to the queue.
            Action = fun(_, St1) ->
                case erlang:module_loaded(Mod) of
                    true -> {reply, {module, Mod}, St1};
                    false -> get_object_code_for_loading(St1, Mod, From)
                end
            end,
            handle_pending_on_load(Action, Mod, From, St0);
        false -> {reply, {error,embedded}, St0}
    end;

handle_call(stop,_From, S) ->
    {stop,normal,stopped,S};

handle_call({set_primary_archive, File, ArchiveBin, FileInfo, ParserFun},
	    _From, S=#state{mode=Mode}) ->
    case erl_prim_loader:set_primary_archive(File, ArchiveBin, FileInfo,
					     ParserFun) of
	{ok, Files} ->
	    {reply, {ok, Mode, Files}, S};
	{error, _Reason} = Error ->
	    {reply, Error, S}
    end;

handle_call(get_mode, _From, S=#state{mode=Mode}) ->
    {reply, Mode, S};

handle_call({finish_loading,Prepared,EnsureLoaded}, _From, S) ->
    {reply,finish_loading(Prepared, EnsureLoaded, S),S};

handle_call(Other,_From, S) ->
    error_msg(" ** Codeserver*** ignoring ~w~n ",[Other]),
    {noreply,S}.

%% --------------------------------------------------------------
%% Path handling functions.
%% --------------------------------------------------------------

%%
%% Create the initial path. 
%%
make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, []).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
		     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split_base(BaseName) of
	{Name, VsnStr} ->
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
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

make_path(_, [], Res) ->
    Res;
make_path(BundleDir, [Bundle|Tail], Res) ->
    Dir = filename:append(BundleDir, Bundle),
    Ebin = filename:append(Dir, "ebin"),
    %% First try with /ebin
    case is_dir(Ebin) of
	true ->
	    make_path(BundleDir, Tail, [Ebin|Res]);
	false ->
	    %% Second try with archive
	    Ext = archive_extension(),
	    Base = filename:basename(Bundle, Ext),
	    Ebin2 = filename:join([BundleDir, Base ++ Ext, Base, "ebin"]),
	    Ebins = 
		case split_base(Base) of
		    {AppName,_} ->
			Ebin3 = filename:join([BundleDir, Base ++ Ext,
					       AppName, "ebin"]),
			[Ebin3, Ebin2, Dir];
		    _ ->
			[Ebin2, Dir]
		end,
	    case try_ebin_dirs(Ebins) of
		{ok,FoundEbin} ->
		    make_path(BundleDir, Tail, [FoundEbin|Res]);
		error ->
		    make_path(BundleDir, Tail, Res)
	    end
    end.

try_ebin_dirs([Ebin|Ebins]) ->
    case is_dir(Ebin) of
	true -> {ok,Ebin};
	false -> try_ebin_dirs(Ebins)
    end;
try_ebin_dirs([]) ->
    error.


%%
%% Add the erl_prim_loader path.
%%
add_loader_path(IPath0,Mode) ->
    {ok,PrimP0} = erl_prim_loader:get_path(),

    %% All boot paths except for "." are cached by default but this can be disabled.
    %% -pa and -pz are never cached by default.
    case Mode of
        embedded ->
            cache_path(strip_path(PrimP0, Mode));  % i.e. only normalize
        _ ->
            Pa0 = get_arg(pa),
            Pz0 = get_arg(pz),

            Pa = patch_path(Pa0),
            Pz = patch_path(Pz0),
            PrimP = patch_path(PrimP0),
            IPath = patch_path(IPath0),

            Path0 = exclude_pa_pz(PrimP,Pa,Pz),
            Path1 = strip_path(Path0, Mode),
            Path2 = merge_path(Path1, IPath, []),
            Path3 = cache_path(Path2),
            add_pa_pz(Path3,Pa,Pz)
    end.

cache_path(Path) ->
    Default = cache_boot_paths(),
    [{P, do_cache_path(P, Default)} || P <- Path].

do_cache_path(".", _) -> nocache;
do_cache_path(_, Default) -> Default.

cache_boot_paths() ->
    case init:get_argument(cache_boot_paths) of
        {ok,[["false"]]} -> nocache;
        _ -> cache
    end.

patch_path(Path) ->
    case check_path(Path) of
	{ok, NewPath} -> NewPath;
	{error, _Reason} -> Path
    end.	    

%% As the erl_prim_loader path includes the -pa and -pz
%% directories they have to be removed first !!
exclude_pa_pz(P0,Pa,[]) ->
    P0 -- Pa;
exclude_pa_pz(P0,Pa,Pz) ->
    lists:reverse(lists:reverse(P0 -- Pa) -- Pz).

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
merge_path(Path,["."|IPath],Acc) ->
    RPath = merge_path1(Path,IPath,Acc),
    ["."|lists:delete(".",RPath)];
merge_path(Path,IPath,Acc) ->
    merge_path1(Path,IPath,Acc).

merge_path1([P|Path],IPath,Acc) ->
    case lists:member(P,Acc) of
	true ->
	    merge_path1(Path,IPath,Acc); % Already added
	false ->
	    IPath1 = exclude(P,IPath),
	    merge_path1(Path,IPath1,[P|Acc])
    end;
merge_path1(_,IPath,Acc) ->
    lists:reverse(Acc) ++ IPath.

add_pa_pz(Path0, Patha, Pathz) ->
    {_,Path1} = add_paths(first,Patha,Path0,nocache,false),
    case Pathz of
        [] ->
            Path1;
        _ ->
            {_,Path2} = add_paths(first,Pathz,lists:reverse(Path1),nocache,false),
            lists:reverse(Path2)
    end.

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
    get_name_from_splitted(filename:split(Dir)).

get_name_from_splitted([DirName,"ebin"]) ->
    discard_after_hyphen(DirName);
get_name_from_splitted([DirName]) ->
    discard_after_hyphen(DirName);
get_name_from_splitted([_|T]) ->
    get_name_from_splitted(T);
get_name_from_splitted([]) ->
    "".						%No name.

discard_after_hyphen("-"++_) ->
    [];
discard_after_hyphen([H|T]) ->
    [H|discard_after_hyphen(T)];
discard_after_hyphen([]) ->
    [].

split_base(BaseName) ->
    case split(BaseName, "-") of
	[_, _|_] = Toks ->
	    Vsn = lists:last(Toks),
	    AllButLast = lists:droplast(Toks),
	    {join(AllButLast, "-"),Vsn};
	[_|_] ->
	    BaseName
    end.

check_path(Path) ->
    PathChoice = init:code_path_choice(),
    ArchiveExt = archive_extension(),
    do_check_path(Path, PathChoice, ArchiveExt, []).
    
do_check_path([], _PathChoice, _ArchiveExt, Acc) -> 
    {ok, lists:reverse(Acc)};
do_check_path([Dir | Tail], PathChoice, ArchiveExt, Acc) ->
    case is_dir(Dir) of
	true ->
	    do_check_path(Tail, PathChoice, ArchiveExt, [Dir | Acc]);
	false when PathChoice =:= strict ->
	    %% Be strict. Only use dir as explicitly stated
	    {error, bad_directory};
	false when PathChoice =:= relaxed ->
	    %% Be relaxed
	    case catch lists:reverse(filename:split(Dir)) of
		{'EXIT', _} ->
		    {error, bad_directory};
		["ebin", App] ->
		    Dir2 = filename:join([App ++ ArchiveExt, App, "ebin"]),
		    case is_dir(Dir2) of
			true ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Dir2 | Acc]);
			false ->
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
		    case is_dir(Dir2) of
			true ->
			    do_check_path(Tail, PathChoice, ArchiveExt, [Dir2 | Acc]);
			false ->
			    {error, bad_directory}
		    end;    
		_ ->
		    {error, bad_directory}
	    end
    end.

%%
%% Add new path(s).
%%
add_path(Where,Dir,Path,Cache,NameDb) when is_atom(Dir) ->
    add_path(Where,atom_to_list(Dir),Path,Cache,NameDb);
add_path(Where,Dir0,Path,Cache,NameDb) when is_list(Dir0) ->
    case int_list(Dir0) of
	true ->
	    Dir = filename:join([Dir0]), % Normalize
	    case check_path([Dir]) of
		{ok, [NewDir]} ->
		    {true, do_add(Where,NewDir,Path,Cache,NameDb)};
		Error ->
		    {Error, Path}
	    end;
	false ->
	    {{error, bad_directory}, Path}
    end;
add_path(_,_,Path,_,_) ->
    {{error, bad_directory}, Path}.


%%
%% If the new directory is added first or if the directory didn't exist
%% the name-directory table must be updated.
%% If NameDb is false we should NOT update NameDb as it is done later
%% then the table is created :-)
%%
do_add(first,Dir,Path,Cache,NameDb) ->
    update(Dir, NameDb),
    [{Dir, Cache}|lists:keydelete(Dir,1,Path)];
do_add(last,Dir,Path,Cache,NameDb) ->
    case lists:keymember(Dir,1,Path) of
	true ->
	    lists:keyreplace(Dir,1,Path,{Dir,Cache});
	false ->
	    maybe_update(Dir, NameDb),
	    Path ++ [{Dir,Cache}]
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
set_path(NewPath0, OldPath, Cache, NameDb, Root) ->
    NewPath = normalize(NewPath0),
    case check_path(NewPath) of
	{ok, NewPath2} ->
	    ets:delete(NameDb),
            NewPath3 = [{P, Cache} || P <- NewPath2],
	    NewDb = create_namedb(NewPath3, Root),
	    {true, NewPath3, NewDb};
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
create_namedb(Path, Root) ->
    Db = ets:new(code_names,[named_table, public]),
    init_namedb(lists:reverse(Path), Db),

    case lookup_name("erts", Db) of
        {ok, _, _, _} ->
            %% erts is part of code path
            ok;
        false ->
            %% No erts in code path, check if this is a source
            %% repo and if so use that.
            ErtsDir = filename:join(Root, "erts"),
            case erl_prim_loader:read_file_info(ErtsDir) of
                error ->
                    ok;
                _ ->
                    do_insert_name("erts", ErtsDir, Db)
            end
    end,
    Db.

init_namedb([{P, _Cache}|Path], Db) ->
    insert_dir(P, Db),
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

%% Dir must be a complete pathname (not only a name).
insert_dir(Dir, Db) ->
    Splitted = filename:split(Dir),
    case get_name_from_splitted(Splitted) of
	Name when Name /= "ebin", Name /= "." ->
	    Name;
	_ ->
	    SplittedAbsName = filename:split(absname(Dir)),
	    Name = get_name_from_splitted(SplittedAbsName)
    end,
    AppDir = filename:join(del_ebin_1(Splitted)),
    do_insert_name(Name, AppDir, Db).

insert_name(Name, Dir, Db) ->
    AppDir = del_ebin(Dir),
    do_insert_name(Name, AppDir, Db).

do_insert_name(Name, AppDir, Db) ->
    {Base, SubDirs} = archive_subdirs(AppDir),
    ets:insert(Db, {Name, AppDir, Base, SubDirs}),
    true.

archive_subdirs(AppDir) ->
    Base = filename:basename(AppDir),
    Dirs = case split_base(Base) of
	       {Name, _} -> [Name, Base];
	    _ -> [Base]
	end,
    Ext = archive_extension(),
    try_archive_subdirs(AppDir ++ Ext, Base, Dirs).

try_archive_subdirs(Archive, Base, [Dir | Dirs]) ->
    ArchiveDir = filename:append(Archive, Dir),
    case erl_prim_loader:list_dir(ArchiveDir) of
	{ok, Files} ->
	    IsDir = fun(RelFile) ->
			    File = filename:append(ArchiveDir, RelFile),
			    is_dir(File)
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

del_path1(Name,[{P, Cache}|Path],NameDb) ->
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
	    [{P, Cache}|del_path1(Name,Path,NameDb)]
    end;
del_path1(_,[],_) ->
    [].

insert_old_shadowed(Name, [{P, _Cache}|Path], NameDb) ->
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
replace_path(Name,Dir,Path,Cache,NameDb) ->
    case catch check_pars(Name,Dir) of
	{ok,N,D} ->
	    {true,replace_path1(N,D,Path,Cache,NameDb)};
	{'EXIT',_} ->
	    {{error,{badarg,[Name,Dir]}},Path};
	Error ->
	    {Error,Path}
    end.

replace_path1(Name,Dir,[{P, _}=Pair|Path],Cache,NameDb) ->
    case get_name(P) of
	Name ->
	    insert_name(Name, Dir, NameDb),
	    [{Dir, Cache}|Path];
	_ ->
	    [Pair|replace_path1(Name,Dir,Path,Cache,NameDb)]
    end;
replace_path1(Name, Dir, [], Cache, NameDb) ->
    insert_name(Name, Dir, NameDb),
    [{Dir, Cache}].

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
    filename:join(del_ebin_1(filename:split(Dir))).

del_ebin_1([Parent,App,"ebin"]) ->
    case filename:basename(Parent) of
	[] ->
	    %% Parent is the root directory
	    [Parent,App];
	_ ->
	    Ext = archive_extension(),
	    case filename:basename(Parent, Ext) of
		Parent ->
		    %% Plain directory.
		    [Parent,App];
		Archive ->
		    %% Archive.
		    [Archive]
	    end
    end;
del_ebin_1(Path = [_App,"ebin"]) ->
    del_ebin_1(filename:split(absname(filename:join(Path))));
del_ebin_1(["ebin"]) ->
    del_ebin_1(filename:split(absname("ebin")));
del_ebin_1([H|T]) ->
    [H|del_ebin_1(T)];
del_ebin_1([]) ->
    [].

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

add_paths(Where,[Dir|Tail],Path,Cache,NameDb) ->
    {_,NPath} = add_path(Where,Dir,Path,Cache,NameDb),
    add_paths(Where,Tail,NPath,Cache,NameDb);
add_paths(_,_,Path,_,_) ->
    {ok,Path}.

del_paths([Name | Names],Path,NameDb) ->
    {_,NPath} = del_path(Name, Path, NameDb),
    del_paths(Names,NPath,NameDb);
del_paths(_,Path,_) ->
    {ok,Path}.

try_finish_module(File, Mod, PC, EnsureLoaded, From, St) ->
    Action = case EnsureLoaded of
        false ->
            fun(_, S) -> try_finish_module_1(File, Mod, PC, From, false, S) end;
        _ ->
            fun(_, S0) ->
                case erlang:module_loaded(Mod) of
                    true ->
                        reply_loading(EnsureLoaded, Mod, {module, Mod}, S0);
                    false when S0#state.mode =:= interactive ->
                        try_finish_module_1(File, Mod, PC, From, EnsureLoaded, S0);
                    false ->
                        reply_loading(EnsureLoaded, Mod, {error, embedded}, S0)
                end
            end
    end,
    handle_pending_on_load(Action, Mod, From, St).

try_finish_module_1(File, Mod, PC, From, EnsureLoaded, #state{moddb=Db}=St) ->
    case is_sticky(Mod, Db) of
	true ->                         %% Sticky file reject the load
	    error_msg("Can't load module '~w' that resides in sticky dir\n",[Mod]),
            reply_loading(EnsureLoaded, Mod, {error,sticky_directory}, St);
	false ->
	    try_finish_module_2(File, Mod, PC, From, EnsureLoaded, St)
    end.

try_finish_module_2(File, Mod, PC, From, EnsureLoaded, St0) ->
    Action = fun(Result, #state{moddb=Db}=St1) ->
        case Result of
            {module, _} -> ets:insert(Db, {Mod, File});
            {error, on_load_failure} -> ok;
            {error, What} -> error_msg("Loading of ~ts failed: ~p\n", [File, What])
        end,
        reply_loading(EnsureLoaded, Mod, Result, St1)
    end,
    Res = case erlang:finish_loading([PC]) of
        ok ->
            {module,Mod};
        {Error,[Mod]} ->
            {error,Error}
    end,
    handle_on_load(Res, Action, Mod, From, St0).

int_list([H|T]) when is_integer(H) -> int_list(T);
int_list([_|_])                    -> false;
int_list([])                       -> true.

get_object_code(#state{path=Path} = St, Mod) when is_atom(Mod) ->
    ModStr = atom_to_list(Mod),
    case erl_prim_loader:is_basename(ModStr) of
        true ->
            case mod_to_bin(Path, ModStr ++ objfile_extension(), []) of
                {Binary, File, NewPath} ->
                    {Binary, File, St#state{path=NewPath}};

                {error, NewPath} ->
                    {error, St#state{path=NewPath}}
            end;

        false ->
            {error, St}
    end.

get_object_code_for_loading(St0, Mod, From) ->
    case wait_loading(St0, Mod, From) of
        {true, St1} -> {noreply, St1};
        false ->
            case get_object_code(St0, Mod) of
                {Bin, FName, St1} ->
                    {Ref, St2} = monitor_loader(St1, Mod, From, Bin, FName),
                    {reply, {Bin, FName, Ref}, St2};
                {error, St1} ->
                    {reply, {error, nofile}, St1}
            end
    end.

monitor_loader(#state{loading = Loading0} = St, Mod, Pid, Bin, FName) ->
    Tag = {'LOADER_DOWN', {Mod, Bin, FName}},
    Ref = erlang:monitor(process, Pid, [{tag, Tag}]),
    Loading = Loading0#{Mod => []},
    {Ref, St#state{loading = Loading}}.

wait_loading(#state{loading = Loading0} = St, Mod, Pid) ->
    case Loading0 of
        #{Mod := Waiting} ->
            Loading = Loading0#{Mod := [Pid | Waiting]},
            {true, St#state{loading = Loading}};
        _ ->
            false
    end.

reply_loading(Ref, Mod, Reply, #state{loading = Loading0} = St)
  when is_reference(Ref) ->
    {Waiting, Loading} = maps:take(Mod, Loading0),
    _ = [reply(Pid, Reply) || Pid <- Waiting],
    erlang:demonitor(Ref, [flush]),
    {reply, Reply, St#state{loading = Loading}};
reply_loading(Ref, _Mod, Reply, St) when is_boolean(Ref) ->
    {reply, Reply, St}.

loader_down(#state{loading = Loading0} = St, {Mod, Bin, FName}) ->
    case Loading0 of
        #{Mod := [First | Rest]} ->
            Tag = {'LOADER_DOWN', {Mod, Bin, FName}},
            Ref = erlang:monitor(process, First, [{tag, Tag}]),
            Loading = Loading0#{Mod := Rest},
            _ = reply(First, {Bin, FName, Ref}),
            St#state{loading = Loading};
        #{Mod := []} ->
            Loading = maps:remove(Mod, Loading0),
            St#state{loading = Loading};
        #{} ->
            %% Rogue message, unknown messages are silently dropped in code server
            St
    end.

mod_to_bin([{Dir, Cache0}|Tail], ModFile, Acc) ->
    case with_cache(Cache0, Dir, ModFile) of
        {true, Cache1} ->
            File = filename:append(Dir, ModFile),

            case erl_prim_loader:get_file(File) of
                error ->
                    mod_to_bin(Tail, ModFile, [{Dir, Cache1} | Acc]);

                {ok,Bin,_} ->
                    Path = lists:reverse(Acc, [{Dir, Cache1} | Tail]),

                    case filename:pathtype(File) of
                        absolute -> {Bin, File, Path};
                        _ -> {Bin, absname(File), Path}
                    end
            end;
        {false, Cache1} ->
            mod_to_bin(Tail, ModFile, [{Dir, Cache1} | Acc])
    end;
mod_to_bin([], ModFile, Acc) ->
    %% At last, try also erl_prim_loader's own method
    case erl_prim_loader:get_file(ModFile) of
        error ->
            {error, lists:reverse(Acc)};     % No more alternatives !
        {ok,Bin,FName} ->
            {Bin, absname(FName), lists:reverse(Acc)}
    end.

with_cache(nocache, _Dir, _ModFile) ->
    {true, nocache};
with_cache(cache, Dir, ModFile) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Entries} -> with_cache(maps:from_keys(Entries, []), Dir, ModFile);
        error -> {false, cache}
    end;
with_cache(Cache, _Dir, ModFile) when is_map(Cache) ->
    {is_map_key(ModFile, Cache), Cache}.

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

do_purge(Mod) ->
    {_WasOld, DidKill} = erts_code_purger:purge(Mod),
    DidKill.

do_soft_purge(Mod) ->
    erts_code_purger:soft_purge(Mod).

is_dir(Path) ->
    case erl_prim_loader:read_file_info(Path) of
	{ok,#file_info{type=directory}} -> true;
	_ -> false
    end.

%%%
%%% Loading of multiple modules in parallel.
%%%

finish_loading(Prepared, EnsureLoaded, #state{moddb=Db}=St) ->
    Ps = [fun(L) -> finish_loading_ensure(L, EnsureLoaded) end,
	  fun(L) -> abort_if_pending_on_load(L, St) end,
	  fun(L) -> abort_if_sticky(L, Db) end,
	  fun(L) -> do_finish_loading(L, St) end],
    run(Ps, Prepared).

finish_loading_ensure(Prepared, true) ->
    {ok,[P || {M,_}=P <- Prepared, not erlang:module_loaded(M)]};
finish_loading_ensure(Prepared, false) ->
    {ok,Prepared}.

abort_if_pending_on_load(L, #state{on_load=[]}) ->
    {ok,L};
abort_if_pending_on_load(L, #state{on_load=OnLoad}) ->
    Pending = [{M,pending_on_load} ||
		  {M,_} <- L,
		  lists:keymember(M, 2, OnLoad)],
    case Pending of
	[] -> {ok,L};
	[_|_] -> {error,Pending}
    end.

abort_if_sticky(L, Db) ->
    Sticky = [{M,sticky_directory} || {M,_} <- L, is_sticky(M, Db)],
    case Sticky of
	[] -> {ok,L};
	[_|_] -> {error,Sticky}
    end.

do_finish_loading(Prepared, #state{moddb=Db}) ->
    MagicBins = [B || {_,{B,_}} <- Prepared],
    case erlang:finish_loading(MagicBins) of
	ok ->
	    MFs = [{M,F} || {M,{_,F}} <- Prepared],
	    true = ets:insert(Db, MFs),
	    ok;
	{Reason,Ms} ->
	    {error,[{M,Reason} || M <- Ms]}
    end.

run([F], Data) ->
    F(Data);
run([F|Fs], Data0) ->
    case F(Data0) of
	{ok,Data} ->
	    run(Fs, Data);
	{error,_}=Error ->
	    Error
    end.

%% -------------------------------------------------------
%% The on_load functionality.
%% -------------------------------------------------------

handle_on_load({error,on_load}, Action, Mod, From, St0) ->
    #state{on_load=OnLoad0} = St0,
    Fun = fun() ->
		  Res = erlang:call_on_load_function(Mod),
		  exit(Res)
	  end,
    PidRef = spawn_monitor(Fun),
    PidAction = {From,Action},
    OnLoad = [{PidRef,Mod,[PidAction]}|OnLoad0],
    St = St0#state{on_load=OnLoad},
    {noreply,St};
handle_on_load(Res, Action, _, _, St) ->
    Action(Res, St).

handle_pending_on_load(Action, Mod, From, #state{on_load=OnLoad0}=St) ->
    case lists:keyfind(Mod, 2, OnLoad0) of
	false ->
	    Action({module, Mod}, St);
	{{From,_Ref},Mod,_Pids} ->
	    %% The on_load function tried to make an external
	    %% call to its own module. That would be a deadlock.
	    %% Fail the call. (The call is probably from error_handler,
	    %% and it will ignore the actual error reason and cause
	    %% an undef exception.)
	    {reply,{error,deadlock},St};
	{_,_,_} ->
	    OnLoad = handle_pending_on_load_1(Mod, {From,Action}, OnLoad0),
	    {noreply,St#state{on_load=OnLoad}}
    end.

handle_pending_on_load_1(Mod, From, [{PidRef,Mod,Pids}|T]) ->
    [{PidRef,Mod,[From|Pids]}|T];
handle_pending_on_load_1(Mod, From, [H|T]) ->
    [H|handle_pending_on_load_1(Mod, From, T)];
handle_pending_on_load_1(_, _, []) -> [].

finish_on_load(PidRef, OnLoadRes, #state{on_load=OnLoad0}=St0) ->
    case lists:keyfind(PidRef, 1, OnLoad0) of
	false ->
	    %% Since this process in general silently ignores messages
	    %% it doesn't understand, it should also ignore a 'DOWN'
	    %% message with an unknown reference.
	    St0;
	{PidRef,Mod,Waiting} ->
	    St = finish_on_load_1(Mod, OnLoadRes, Waiting, St0),
	    OnLoad = [E || {R,_,_}=E <- OnLoad0, R =/= PidRef],
	    St#state{on_load=OnLoad}
    end.

finish_on_load_1(Mod, OnLoadRes, Waiting, St) ->
    Keep = OnLoadRes =:= ok,
    erts_code_purger:finish_after_on_load(Mod, Keep),
    Res = case Keep of
	      false ->
		  _ = finish_on_load_report(Mod, OnLoadRes),
		  {error,on_load_failure};
	      true ->
		  {module,Mod}
	  end,
    finish_on_load_2(Waiting, Res, St).

finish_on_load_2([{Pid,Action}|T], Res, St0) ->
    case Action(Res, St0) of
	{reply,Rep,St} ->
	    _ = reply(Pid, Rep),
	    finish_on_load_2(T, Res, St);
	{noreply,St} ->
	    finish_on_load_2(T, Res, St)
    end;
finish_on_load_2([], _, St) ->
    St.

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
		      "~s returned:~n~P\n",

		  %% Express the call as an apply to simplify
		  %% the ext_mod_dep/1 test case.
		  E = error_logger,
		  E:warning_msg(F, [Mod,Term,10])
	  end).

%% -------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------

all_loaded(Db) ->
    Ms = ets:fun2ms(fun({M,_}=T) when is_atom(M) -> T end),
    ets:select(Db, Ms).

-spec error_msg(io:format(), [term()]) -> 'ok'.
error_msg(Format, Args) ->
    %% This is equal to calling logger:error/3 which we don't want to
    %% do from code_server. We don't want to call logger:timestamp()
    %% either.
    _ = try
            logger ! {log,error,Format,Args,
                      #{pid=>self(),
                        gl=>group_leader(),
                        time=>os:system_time(microsecond),
                        error_logger=>#{tag=>error}}}
        catch _:_ ->
                erlang:display({?MODULE,error}),
                erlang:display({Format,Args})
        end,
    ok.

-spec info_msg(io:format(), [term()]) -> 'ok'.
info_msg(Format, Args) ->
    %% This is equal to calling logger:info/3 which we don't want to
    %% do from code_server. We don't want to call logger:timestamp()
    %% either.
    catch logger ! {log,info,Format,Args,
                    #{pid=>self(),
                      gl=>group_leader(),
                      time=>os:system_time(microsecond),
                      error_logger=>#{tag=>info_msg}}},
    ok.

objfile_extension() ->
    init:objfile_extension().

archive_extension() ->
    init:archive_extension().

to_list(X) when is_list(X) -> X;
to_list(X) when is_atom(X) -> atom_to_list(X).
