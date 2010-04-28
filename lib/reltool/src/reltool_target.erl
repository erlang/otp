%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

-module(reltool_target).

%% Public
-export([
         gen_config/2,
         gen_app/1,
         gen_rel/2,
         gen_rel_files/2,
         gen_boot/1,
         gen_script/4,
         gen_spec/1,
         eval_spec/3,
         gen_target/2,
         install/2
        ]).

-include("reltool.hrl").
-include_lib("kernel/include/file.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hardcoded internals about the kernel application
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Mandatory modules are modules that must be loaded before processes
%% can be started. These are a collection of modules from the kernel
%% and stdlib applications. Nowadays, error_handler dynamically loads
%% almost every module. The error_handler self must still be there
%% though.

mandatory_modules() ->
    [error_handler].

%% Kernel processes are specially treated by the init process. If a
%% kernel process terminates the whole system terminates.

kernel_processes(KernelApp) ->
    [
     {kernelProcess, heart, {heart, start, []}},
     {kernelProcess, error_logger , {error_logger, start_link, []}},
     {kernelProcess,
      application_controller,
      {application_controller, start, [KernelApp]}}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a config file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_config(Sys, InclDefs) ->
    {ok, do_gen_config(Sys, InclDefs)}.

do_gen_config(#sys{root_dir          	= RootDir,
		   lib_dirs          	= LibDirs,
		   mod_cond          	= ModCond,
		   incl_cond         	= AppCond,
		   apps              	= Apps,
		   boot_rel          	= BootRel,
		   rels              	= Rels,
		   emu_name          	= EmuName,
		   profile           	= Profile,
		   incl_sys_filters  	= InclSysFiles,
		   excl_sys_filters  	= ExclSysFiles,
		   incl_app_filters  	= InclAppFiles,
		   excl_app_filters  	= ExclAppFiles,
		   incl_archive_filters = InclArchiveDirs,
		   excl_archive_filters = ExclArchiveDirs,
		   archive_opts      	= ArchiveOpts,
		   relocatable       	= Relocatable,
		   rel_app_type        	= RelAppType,
		   embedded_app_type   	= InclAppType,
		   app_file          	= AppFile,
		   debug_info        	= DebugInfo},
	      InclDefs) ->
    ErtsItems =
        case lists:keysearch(erts, #app.name, Apps) of
            {value, Erts} ->
                [{erts, do_gen_config(Erts, InclDefs)}];
            false ->
                []
        end,
    AppsItems =
        [do_gen_config(A, InclDefs)
	 || A <- Apps,
	    A#app.name =/= ?MISSING_APP_NAME,
	    A#app.name =/= erts,
	    not A#app.is_escript],
    EscriptItems = [{escript,
		     A#app.active_dir,
		     emit(incl_cond, A#app.incl_cond, undefined, InclDefs)}
		    || A <- Apps, A#app.is_escript],
    DefaultRels = reltool_utils:default_rels(),
    RelsItems =
	[{rel, R#rel.name, R#rel.vsn, do_gen_config(R, InclDefs)} ||
	    R <- Rels],
    DefaultRelsItems =
	[{rel, R#rel.name, R#rel.vsn, do_gen_config(R, InclDefs)} ||
	    R <- DefaultRels],
    RelsItems2 =
	case InclDefs of
	    true  -> RelsItems;
	    false -> RelsItems -- DefaultRelsItems
	end,
    X = fun(List) -> [Re || #regexp{source = Re} <- List] end,
    {sys,
     emit(root_dir, RootDir, code:root_dir(), InclDefs) ++
     emit(lib_dirs, LibDirs, ?DEFAULT_LIBS, InclDefs) ++
     EscriptItems ++
     emit(mod_cond, ModCond, ?DEFAULT_MOD_COND, InclDefs) ++
     emit(incl_cond, AppCond, ?DEFAULT_INCL_COND, InclDefs) ++
     ErtsItems ++
     lists:flatten(AppsItems) ++
     emit(boot_rel, BootRel, ?DEFAULT_REL_NAME, InclDefs) ++
     RelsItems2 ++
     emit(emu_name, EmuName, ?DEFAULT_EMU_NAME, InclDefs) ++
     emit(relocatable, Relocatable, ?DEFAULT_RELOCATABLE, InclDefs) ++
     emit(profile, Profile, ?DEFAULT_PROFILE, InclDefs) ++
     emit(incl_sys_filters, X(InclSysFiles), reltool_utils:choose_default(incl_sys_filters, Profile, InclDefs), InclDefs) ++
     emit(excl_sys_filters, X(ExclSysFiles), reltool_utils:choose_default(excl_sys_filters, Profile, InclDefs), InclDefs) ++
     emit(incl_app_filters, X(InclAppFiles), reltool_utils:choose_default(incl_app_filters, Profile, InclDefs), InclDefs) ++
     emit(excl_app_filters, X(ExclAppFiles), reltool_utils:choose_default(excl_app_filters, Profile, InclDefs), InclDefs) ++
     emit(incl_archive_filters, X(InclArchiveDirs), ?DEFAULT_INCL_ARCHIVE_FILTERS, InclDefs) ++
     emit(excl_archive_filters, X(ExclArchiveDirs), ?DEFAULT_EXCL_ARCHIVE_FILTERS, InclDefs) ++
     emit(archive_opts, ArchiveOpts, ?DEFAULT_ARCHIVE_OPTS, InclDefs) ++
     emit(rel_app_type, RelAppType, ?DEFAULT_REL_APP_TYPE, InclDefs) ++
     emit(embedded_app_type, InclAppType, reltool_utils:choose_default(embedded_app_type, Profile, InclDefs), InclDefs) ++
     emit(app_file, AppFile, ?DEFAULT_APP_FILE, InclDefs) ++
     emit(debug_info, DebugInfo, ?DEFAULT_DEBUG_INFO, InclDefs)};
do_gen_config(#app{name = Name,
		   mod_cond = ModCond,
		   incl_cond  = AppCond,
		   debug_info = DebugInfo,
		   app_file = AppFile,
		   incl_app_filters = InclAppFiles,
		   excl_app_filters = ExclAppFiles,
		   incl_archive_filters = InclArchiveDirs,
		   excl_archive_filters = ExclArchiveDirs,
		   archive_opts = ArchiveOpts,
		   use_selected_vsn  = UseSelected,
		   vsn = Vsn,
		   mods = Mods,
		   is_included = IsIncl},
	      InclDefs) ->
    AppConfig =
	[
	 emit(mod_cond, ModCond, undefined, InclDefs),
	 emit(incl_cond, AppCond, undefined, InclDefs),
	 emit(debug_info, DebugInfo, undefined, InclDefs),
	 emit(app_file, AppFile, undefined, InclDefs),
	 emit(incl_app_filters, InclAppFiles, undefined, InclDefs),
	 emit(excl_app_filters, ExclAppFiles, undefined, InclDefs),
	 emit(incl_archive_filters, InclArchiveDirs, undefined, InclDefs),
	 emit(excl_archive_filters, ExclArchiveDirs, undefined, InclDefs),
	 emit(archive_opts, ArchiveOpts, undefined, InclDefs),
	 if
	     IsIncl, InclDefs -> [{vsn, Vsn}];
	     UseSelected      -> [{vsn, Vsn}];
	     true             -> []
	 end,
	 [do_gen_config(M, InclDefs) || M <- Mods]
	],
    case lists:flatten(AppConfig) of
	FlatAppConfig when FlatAppConfig =/= []; IsIncl ->
	    [{app, Name, FlatAppConfig}];
	[] ->
	    []
    end;
do_gen_config(#mod{name = Name,
		   incl_cond = AppCond,
		   debug_info = DebugInfo,
		   is_included = IsIncl},
	      InclDefs) ->
    ModConfig =
	[
	 emit(incl_cond,  AppCond, undefined, InclDefs),
	 emit(debug_info, DebugInfo, undefined, InclDefs)
	],
    case lists:flatten(ModConfig) of
	FlatModConfig when FlatModConfig =/= []; IsIncl ->
	    [{mod, Name, FlatModConfig}];
	_ ->
	    []
    end;
do_gen_config(#rel{name = _Name,
		   vsn = _Vsn,
		   rel_apps = RelApps},
	      InclDefs) ->
    [do_gen_config(RA, InclDefs) || RA <- RelApps];
do_gen_config(#rel_app{name = Name,
		       app_type = Type,
		       incl_apps = InclApps},
	      _InclDefs) ->
    case {Type, InclApps} of
        {undefined, []} -> Name;
        {undefined, _}  -> {Name, InclApps};
        {_, []}         -> {Name, Type};
        {_, _}          -> {Name, Type, InclApps}
    end;
do_gen_config({Tag, Val}, InclDefs) ->
    emit(Tag, Val, undefined, InclDefs);
do_gen_config([], _InclDefs) ->
    [];
do_gen_config([H | T], InclDefs) ->
    lists:flatten([do_gen_config(H, InclDefs), do_gen_config(T, InclDefs)]).

emit(Tag, Val, Default, InclDefs) ->
    %% io:format("~p(~p):\n\t~p\n\t~p\n",
    %%           [Tag, Val =/= Default, Val, Default]),
    if
        Val == undefined -> [];
        InclDefs         -> [{Tag, Val}];
        Val =/= Default  -> [{Tag, Val}];
        true             -> []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of an app file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_app(#app{name = Name,
             info = #app_info{description = Desc,
                              id = Id,
                              vsn = Vsn,
                              modules = Mods,
                              maxP = MaxP,
                              maxT = MaxT,
                              registered = Regs,
                              incl_apps = InclApps,
                              applications = ReqApps,
                              env = Env,
                              mod = StartMod,
                              start_phases = StartPhases}}) ->
    StartMod2 =
        case StartMod =:= undefined of
            true  -> [];
            false -> [{mod, StartMod}]
        end,
    {application, Name,
     [{description, Desc},
      {vsn, Vsn},
      {id, Id},
      {modules, Mods},
      {registered, Regs},
      {applications, ReqApps},
      {included_applications, InclApps},
      {env, Env},
      {start_phases, StartPhases},
      {maxT, MaxT},
      {maxP, MaxP} |
      StartMod2]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a rel file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_rel(Rel, Sys) ->
    try
	MergedApps = merge_apps(Rel, Sys),
	{ok, do_gen_rel(Rel, Sys, MergedApps)}
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

do_gen_rel(#rel{name = RelName, vsn = RelVsn},
	   #sys{apps = Apps},
	   MergedApps) ->
    ErtsName = erts,
    case lists:keysearch(ErtsName, #app.name, Apps) of
	{value, Erts} ->
	    {release,
	     {RelName, RelVsn},
	     {ErtsName, Erts#app.vsn},
	     [strip_rel_info(App) || App <- MergedApps]};
	false ->
	    reltool_utils:throw_error("Mandatory application ~p is "
				      "not included",
                                      [ErtsName])
    end.

strip_rel_info(#app{name = Name,
		    vsn = Vsn,
		    app_type = Type,
		    info = #app_info{incl_apps = InclApps}})
  when Type =/= undefined ->
    case {Type, InclApps} of
        {permanent, []} -> {Name, Vsn};
        {permanent, _}  -> {Name, Vsn, InclApps};
        {_, []}         -> {Name, Vsn, Type};
        {_, _}          -> {Name, Vsn, Type, InclApps}
    end.

merge_apps(#rel{name = RelName,
		rel_apps = RelApps},
	   #sys{apps = Apps,
		rel_app_type = RelAppType,
		embedded_app_type = EmbAppType}) ->
    Mandatory = [kernel, stdlib],
    MergedApps = do_merge_apps(RelName, Mandatory, Apps, permanent, []),
    MergedApps2 = do_merge_apps(RelName, RelApps, Apps, RelAppType, MergedApps),
    Embedded =
	[A#app.name || A <- Apps,
		       EmbAppType =/= undefined,
		       A#app.is_included,
		       A#app.name =/= erts,
		       A#app.name =/= ?MISSING_APP_NAME,
		       not lists:keymember(A#app.name, #app.name, MergedApps2)],
    MergedApps3 = do_merge_apps(RelName, Embedded, Apps, EmbAppType, MergedApps2),
    sort_apps(MergedApps3).

do_merge_apps(RelName, [#rel_app{name = Name} = RA | RelApps], Apps, RelAppType, Acc) ->
    case is_already_merged(Name, RelApps, Acc) of
	true ->
	    do_merge_apps(RelName, RelApps, Apps, RelAppType, Acc);
	false ->
	    {value, App} = lists:keysearch(Name, #app.name, Apps),
	    MergedApp = merge_app(RelName, RA, RelAppType, App),
	    MoreNames = (MergedApp#app.info)#app_info.applications,
	    Acc2 = [MergedApp | Acc],
	    do_merge_apps(RelName, MoreNames ++ RelApps, Apps, RelAppType, Acc2)
    end;
do_merge_apps(RelName, [Name | RelApps], Apps, RelAppType, Acc) ->
  case is_already_merged(Name, RelApps, Acc) of
	true ->
	  do_merge_apps(RelName, RelApps, Apps, RelAppType, Acc);
	false ->
	  RelApp = init_rel_app(Name, Apps),
	  do_merge_apps(RelName, [RelApp | RelApps], Apps, RelAppType, Acc)
  end;
do_merge_apps(_RelName, [], _Apps, _RelAppType, Acc) ->
    lists:reverse(Acc).

init_rel_app(Name, Apps) ->
    {value, App} = lists:keysearch(Name, #app.name, Apps),
    Info = App#app.info,
    #rel_app{name = Name,
	     app_type = undefined,
	     incl_apps = Info#app_info.incl_apps}.

merge_app(RelName,
	      #rel_app{name = Name,
		       app_type = Type,
		       incl_apps = InclApps},
	      RelAppType,
	      App) ->
    Type2 =
        case {Type, App#app.app_type} of
            {undefined, undefined} -> RelAppType;
            {undefined, AppAppType} -> AppAppType;
            {_, _} -> Type
        end,
    Info = App#app.info,
    case InclApps -- Info#app_info.incl_apps of
        [] ->
	    App#app{app_type = Type2, info = Info#app_info{incl_apps = InclApps}};
        BadIncl ->
            reltool_utils:throw_error("~p: These applications are "
				      "used by release ~s but are "
				      "missing as included_applications "
				      "in the app file: ~p",
                                      [Name, RelName, BadIncl])
    end.

is_already_merged(Name, [Name | _], _MergedApps) ->
    true;
is_already_merged(Name, [#rel_app{name = Name} | _], _MergedApps) ->
    true;
is_already_merged(Name, [_ | RelApps], MergedApps) ->
    is_already_merged(Name, RelApps, MergedApps);
is_already_merged(Name, [], [#app{name = Name} | _MergedApps]) ->
    true;
is_already_merged(Name, [] = RelApps, [_ | MergedApps]) ->
    is_already_merged(Name, RelApps, MergedApps);
is_already_merged(_Name, [], []) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a boot file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_boot({script, {_, _}, _} = Script) ->
    {ok, term_to_binary(Script)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate the contents of a script file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_script(Rel, Sys, PathFlag, Variables) ->
    try
	MergedApps = merge_apps(Rel, Sys),
        do_gen_script(Rel, Sys, MergedApps, PathFlag, Variables)
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

do_gen_script(#rel{name = RelName, vsn = RelVsn},
              #sys{apps = Apps},
	      MergedApps,
              PathFlag,
              Variables) ->
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    Preloaded = [Mod#mod.name || Mod <- Erts#app.mods],
    Mandatory = mandatory_modules(),
    Early = Mandatory ++ Preloaded,
    {value, KernelApp} = lists:keysearch(kernel, #app.name, MergedApps),
    InclApps = [I || #app{info = #app_info{incl_apps = I}} <- MergedApps],

    %% Create the script
    DeepList =
        [
         %% Register preloaded modules
         {preLoaded, lists:sort(Preloaded)},
         {progress, preloaded},

         %% Load mandatory modules
         {path, create_mandatory_path(MergedApps, PathFlag, Variables)},
         {primLoad, lists:sort(Mandatory)},
         {kernel_load_completed},
         {progress, kernel_load_completed},

         %% Load remaining modules
         [load_app_mods(A, Early, PathFlag, Variables) || A <- MergedApps],
         {progress, modules_loaded},

         %% Start kernel processes
         {path, create_path(MergedApps, PathFlag, Variables)},
         kernel_processes(gen_app(KernelApp)),
         {progress, init_kernel_started},

         %% Load applications
         [{apply, {application, load, [gen_app(A)]}} ||
             A = #app{name = Name, app_type = Type} <- MergedApps,
             Name =/= kernel,
             Type =/= none],
         {progress, applications_loaded},

         %% Start applications
         [{apply, {application, start_boot, [Name, Type]}} ||
             #app{name = Name, app_type = Type} <- MergedApps,
             Type =/= none,
             Type =/= load,
             not lists:member(Name, InclApps)],

         %% Apply user specific customizations
         {apply, {c, erlangrc, []}},
         {progress, started}
        ],
    {ok, {script, {RelName, RelVsn}, lists:flatten(DeepList)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_app_mods(#app{mods = Mods} = App, Mand, PathFlag, Variables) ->
    Path = cr_path(App, PathFlag, Variables),
    PartNames =
        lists:sort([{packages:split(M),M} ||
                       #mod{name = M} <- Mods,
                       not lists:member(M, Mand)]),
    SplitMods =
        lists:foldl(
          fun({Parts,M}, [{Last, Acc}|Rest]) ->
                  [_|Tail] = lists:reverse(Parts),
                  case lists:reverse(Tail) of
                      Subs when Subs == Last ->
                          [{Last,[M|Acc]}|Rest];
                      Subs ->
                          [{Subs, [M]}|[{Last,Acc}|Rest]]
                  end
          end,
          [{[],
            []}],
          PartNames),
    lists:foldl(
      fun({Subs,Ms}, Cmds) ->
              [{path, [filename:join([Path | Subs])]},
               {primLoad, lists:sort(Ms)} | Cmds]
      end,
      [],
      SplitMods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: sort_apps(Apps) -> {ok, Apps'} | throw({error, Error})
%% Types: Apps = {{Name, Vsn}, #application}]
%% Purpose: Sort applications according to dependencies among
%%          applications.  If order doesn't matter, use the same
%%          order as in the original list.
%% Alg. written by Ulf Wiger 970917 (etxuwig@etxb.ericsson.se)
%% Mod. by mbj
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_apps(Apps) ->
    sort_apps(Apps, [], [], []).

sort_apps([#app{name = Name, info = Info} = App | Apps],
	  Missing,
	  Circular,
	  Visited) ->
    {Uses, Apps1, NotFnd1} =
	find_all(Name, Info#app_info.applications, Apps, Visited, [], []),
    {Incs, Apps2, NotFnd2} =
	find_all(Name,
		 lists:reverse(Info#app_info.incl_apps),
		 Apps1,
		 Visited,
		 [],
		 []),

    Missing1 = NotFnd1 ++ NotFnd2 ++ Missing,
    case Uses ++ Incs of
        [] ->
            %% No more app that must be started before this one is
            %% found; they are all already taken care of (and present
            %% in Visited list)
            [App | sort_apps(Apps, Missing1, Circular, [Name | Visited])];
        L ->
            %% The apps in L must be started before the app.
            %% Check if we have already taken care of some app in L,
            %% in that case we have a circular dependency.
            NewCircular = [N1 || N1 <- L, N2 <- Visited, N1 =:= N2],
            Circular1 = case NewCircular of
                            [] -> Circular;
                            _ -> [Name | NewCircular] ++ Circular
                        end,
            %% L must be started before N, try again, with all apps
            %% in L added before N.
            Apps3 = del_apps(NewCircular, L ++ [App | Apps2]),
            sort_apps(Apps3, Missing1, Circular1, [Name | Visited])
    end;
sort_apps([], [], [], _) ->
    [];
sort_apps([], Missing, [], _) ->
    %% this has already been checked before, but as we have the info...
    reltool_utils:throw_error("Undefined applications: ~p",
			      [make_set(Missing)]);
sort_apps([], [], Circular, _) ->
    reltool_utils:throw_error("Circular dependencies: ~p",
			      [make_set(Circular)]);
sort_apps([], Missing, Circular, _) ->
    reltool_utils:throw_error("Circular dependencies: ~p"
                              "Undefined applications: ~p\n",
                              [make_set(Circular), make_set(Missing)]).

find_all(CheckingApp, [Name | Names], Apps, Visited, Found, NotFound) ->
    case lists:keysearch(Name, #app.name, Apps) of
        {value, #app{info = Info} = App} ->
            %% It is OK to have a dependecy like
            %% X includes Y, Y uses X.
            case lists:member(CheckingApp, Info#app_info.incl_apps) of
                true ->
                    case lists:member(Name, Visited) of
                        true ->
                            find_all(CheckingApp,
				     Names,
				     Apps,
				     Visited,
				     Found,
				     NotFound);
                        false ->
                            find_all(CheckingApp,
				     Names,
				     Apps,
				     Visited,
				     Found,
				     [Name | NotFound])
                    end;
                false ->
                    find_all(CheckingApp,
			     Names,
			     Apps -- [App],
			     Visited,
			     [App|Found],
			     NotFound)
            end;
        false ->
            case lists:member(Name, Visited) of
                true ->
                    find_all(CheckingApp,
			     Names,
			     Apps,
			     Visited,
			     Found,
			     NotFound);
                false ->
                    find_all(CheckingApp,
			     Names,
			     Apps,
			     Visited,
			     Found,
			     [Name|NotFound])
            end
    end;
find_all(_CheckingApp, [], Apps, _Visited, Found, NotFound) ->
    {Found, Apps, NotFound}.

del_apps([Name | Names], Apps) ->
    del_apps(Names, lists:keydelete(Name, #app.name, Apps));
del_apps([], Apps) ->
    Apps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create the load path used in the generated script.
%% If PathFlag is true a script intended to be used as a complete
%% system (e.g. in an embbeded system), i.e. all applications are
%% located under $ROOT/lib.
%% Otherwise all paths are set according to dir per application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Create the complete path.
create_path(Apps, PathFlag, Variables) ->
    make_set([cr_path(App, PathFlag, Variables) || App <- Apps]).

%% Create the path to a specific application.
%% (The otp_build flag is only used for OTP internal system make)
cr_path(#app{label = Label}, true, []) ->
    filename:join(["$ROOT", "lib", Label, "ebin"]);
cr_path(#app{name = Name, vsn = Vsn, label = Label, active_dir = Dir},
	true,
	Variables) ->
    Tail = [Label, "ebin"],
    case variable_dir(Dir, atom_to_list(Name), Vsn, Variables) of
        {ok, VarDir} ->
            filename:join([VarDir] ++ Tail);
        _ ->
            filename:join(["$ROOT", "lib"] ++ Tail)
    end;
cr_path(#app{name = Name}, otp_build, _) ->
    filename:join(["$ROOT", "lib", atom_to_list(Name), "ebin"]);
cr_path(#app{active_dir = Dir}, _, _) ->
    filename:join([Dir, "ebin"]).

variable_dir(Dir, Name, Vsn, [{Var,Path} | Variables]) ->
    case lists:prefix(Path, Dir) of
        true ->
            D0 = strip_prefix(Path, Dir),
            case strip_name_ebin(D0, Name, Vsn) of
                {ok, D} ->
                    {ok, filename:join(["\$" ++ Var] ++ D)};
                _ ->
                    %% We know at least that we are located
                    %% under the variable dir.
                    {ok, filename:join(["\$" ++ Var] ++ D0)}
            end;
        false ->
            variable_dir(Dir, Name, Vsn, Variables)
    end;
variable_dir(_Dir, _, _, []) ->
    false.

strip_prefix(Path, Dir) ->
    L = length(filename:split(Path)),
    lists:nthtail(L, filename:split(Dir)).

strip_name_ebin(Dir, Name, Vsn) ->
    FullName = Name ++ "-" ++ Vsn,
    case lists:reverse(Dir) of
        ["ebin", Name     | D] -> {ok, lists:reverse(D)};
        ["ebin", FullName | D] -> {ok, lists:reverse(D)};
        _                      -> false
    end.

%% Create the path to the kernel and stdlib applications.
create_mandatory_path(Apps, PathFlag, Variables) ->
    Mandatory = [kernel, stdlib],
    make_set(lists:map(fun(#app{name = Name} = App) ->
                               case lists:member(Name, Mandatory) of
                                   true ->
                                       cr_path(App, PathFlag, Variables);
                                   false ->
                                       ""
                               end
                       end,
                       Apps)).

make_set([]) ->
    [];
make_set([""|T]) -> % Ignore empty items.
    make_set(T);
make_set([H|T]) ->
    [H | [ Y || Y<- make_set(T),
                Y =/= H]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate rel, script and boot files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_rel_files(Sys, TargetDir) ->
    try
        Spec = spec_rel_files(Sys),
        eval_spec(Spec, Sys#sys.root_dir, TargetDir)
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

spec_rel_files(#sys{rels = Rels} = Sys) ->
    lists:append([do_spec_rel_files(R, Sys) || R <- Rels]).

do_spec_rel_files(#rel{name = RelName} = Rel,  Sys) ->
    RelFile = RelName ++ ".rel",
    ScriptFile = RelName ++ ".script",
    BootFile = RelName ++ ".boot",
    MergedApps = merge_apps(Rel, Sys),
    GenRel = do_gen_rel(Rel, Sys, MergedApps),
    PathFlag = true,
    Variables = [],
    {ok, Script} = do_gen_script(Rel, Sys, MergedApps, PathFlag, Variables),
    {ok, BootBin} = gen_boot(Script),
    Date = date(),
    Time = time(),
    RelIoList = io_lib:format("%% rel generated at ~w ~w\n~p.\n\n",
                              [Date, Time, GenRel]),
    ScriptIoList = io_lib:format("%% script generated at ~w ~w\n~p.\n\n",
                                 [Date, Time, Script]),
    [
     {write_file, RelFile, RelIoList},
     {write_file, ScriptFile, ScriptIoList},
     {write_file, BootFile, BootBin}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate a complete target system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_target(Sys, TargetDir) ->
    try
        Spec = do_gen_spec(Sys),
        eval_spec(Spec, Sys#sys.root_dir, TargetDir)
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

gen_spec(Sys) ->
    try
        {ok, do_gen_spec(Sys)}
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

do_gen_spec(#sys{root_dir = RootDir,
                 incl_sys_filters = InclRegexps,
                 excl_sys_filters = ExclRegexps,
                 relocatable = Relocatable,
                 apps = Apps} = Sys) ->
    {create_dir, _, SysFiles} = spec_dir(RootDir),
    {ExclRegexps2, SysFiles2} =
	strip_sys_files(Relocatable, SysFiles, Apps, ExclRegexps),
    RelFiles = spec_rel_files(Sys),
    {InclRegexps2, BinFiles} =
	spec_bin_files(Sys, SysFiles, SysFiles2, RelFiles, InclRegexps),
    LibFiles = spec_lib_files(Sys),
    {BootVsn, StartFile} = spec_start_file(Sys),
    SysFiles3 =
        [
         {create_dir, "releases",
	  [StartFile,
	   {create_dir,BootVsn, RelFiles}]},
	 {create_dir, "bin", BinFiles}
        ] ++ SysFiles2,
    SysFiles4 = filter_spec(SysFiles3, InclRegexps2, ExclRegexps2),
    SysFiles5 = SysFiles4 ++ [{create_dir, "lib", LibFiles}],
    check_sys(["bin", "erts", "lib"], SysFiles5),
    SysFiles5.

strip_sys_files(Relocatable, SysFiles, Apps, ExclRegexps) ->
    ExclRegexps2 =
	case Relocatable of
	    true ->
		ExtraExcl = ["^erts.*/bin/.*src\$"],
		reltool_utils:decode_regexps(excl_sys_filters,
					     {add, ExtraExcl},
					     ExclRegexps);
	    false ->
		ExclRegexps
	end,
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    FilterErts =
        fun(Spec) ->
		File = element(2, Spec),
		case File of
		    "erts" ->
			reltool_utils:throw_error("This system is not installed. "
						  "The directory ~s is missing.",
				    [Erts#app.label]);
		    _ when File =:= Erts#app.label ->
			replace_dyn_erl(Relocatable, Spec);
                    "erts-" ++ _ ->
			false;
                    _ ->
                        true
                end
        end,
    SysFiles2 = lists:zf(FilterErts, SysFiles),
    SysFiles3 = lists:foldl(fun(F, Acc) -> lists:keydelete(F, 2, Acc) end,
			    SysFiles2,
			    ["releases", "lib", "bin"]),
    {ExclRegexps2, SysFiles3}.

replace_dyn_erl(false, _ErtsSpec) ->
    true;
replace_dyn_erl(true, {create_dir, ErtsDir, ErtsFiles}) ->
    [{create_dir, _, BinFiles}] =
	safe_lookup_spec("bin", ErtsFiles),
    case lookup_spec("dyn_erl", BinFiles) of
        [] ->
            case lookup_spec("erl.ini", BinFiles) of
                [] ->
                    true;
                [{copy_file, ErlIni}] ->
                    %% Remove Windows .ini file
                    BinFiles2 = lists:keydelete(ErlIni, 2, BinFiles),
                    ErtsFiles2 =
			lists:keyreplace("bin",
					 2,
					 ErtsFiles,
					 {create_dir, "bin", BinFiles2}),
                    {true, {create_dir, ErtsDir, ErtsFiles2}}
            end;
        [{copy_file, DynErlExe}] ->
            %% Replace erl with dyn_erl
            ErlExe = "erl" ++ filename:extension(DynErlExe),
            BinFiles2 = lists:keydelete(DynErlExe, 2, BinFiles),
            DynErlExe2 = filename:join([ErtsDir, "bin", DynErlExe]),
            BinFiles3 = lists:keyreplace(ErlExe,
					 2,
					 BinFiles2,
					 {copy_file, ErlExe, DynErlExe2}),
            ErtsFiles2 = lists:keyreplace("bin",
					  2,
					  ErtsFiles,
					  {create_dir, "bin", BinFiles3}),
            {true, {create_dir, ErtsDir, ErtsFiles2}}
    end.

spec_bin_files(Sys, AllSysFiles, StrippedSysFiles, RelFiles, InclRegexps) ->
    [{create_dir, ErtsLabel, ErtsFiles}] =
	safe_lookup_spec("erts", StrippedSysFiles),
    [{create_dir, _, BinFiles}] = safe_lookup_spec("bin", ErtsFiles),
    ErtsBin = filename:join([ErtsLabel, "bin"]),
    Escripts = spec_escripts(Sys, ErtsBin, BinFiles),
    Map = fun({copy_file, File}) ->
                  {copy_file, File, filename:join([ErtsBin, File])};
             ({copy_file, NewFile, OldFile}) ->
                  {_, OldFile2} =
		      abs_to_rel_path(ErtsBin,
				      filename:join([ErtsBin, OldFile])),
                  {copy_file, NewFile, OldFile2}
          end,

    %% Do only copy those bin files from erts/bin that also exists in bin
    [{create_dir, _, OldBinFiles}] = safe_lookup_spec("bin", AllSysFiles),
    GoodNames = [F || {copy_file, F} <- OldBinFiles,
		      not lists:suffix(".boot", F),
		      not lists:suffix(".script", F)],
    BinFiles2 = [Map(S) || S <- BinFiles,
			   lists:member(element(2, S), GoodNames)],
    BootFiles = [F || F <- RelFiles, lists:suffix(".boot", element(2, F))],
    [{write_file, _, BootRel}] =
	safe_lookup_spec(Sys#sys.boot_rel ++ ".boot", BootFiles),
    BootFiles2 = lists:keystore("start.boot",
				2,
				BootFiles,
				{write_file, "start.boot", BootRel}),
    MakeRegexp =
	fun(File) -> "^bin/" ++ element(2, File) ++ "(|.escript)\$" end,
    ExtraIncl = lists:map(MakeRegexp, Escripts),
    InclRegexps2 = reltool_utils:decode_regexps(incl_sys_filters,
						{add, ExtraIncl},
						InclRegexps),
    {InclRegexps2, Escripts ++ BinFiles2 ++ BootFiles2}.

spec_escripts(#sys{apps = Apps}, ErtsBin, BinFiles) ->
    Filter = fun(#app{is_escript = IsEscript,
		      is_included = IsIncl,
                      is_pre_included = IsPre,
		      name = Name,
		      active_dir = File}) ->
                     if
                         Name =:= ?MISSING_APP_NAME ->
                             false;
                         not IsEscript ->
                             false;
                         IsIncl; IsPre ->
                             {true, do_spec_escript(File, ErtsBin, BinFiles)};
                         true ->
                             false
                     end
             end,
    lists:flatten(lists:zf(Filter, Apps)).

do_spec_escript(File, ErtsBin, BinFiles) ->
    [{copy_file, EscriptExe}] = safe_lookup_spec("escript", BinFiles),
    EscriptExt = ".escript",
    Base = filename:basename(File, EscriptExt),
    ExeExt = filename:extension(EscriptExe),
    [{copy_file, Base ++ EscriptExt, File},
     {copy_file, Base ++ ExeExt, filename:join([ErtsBin, EscriptExe])}].

check_sys(Mandatory, SysFiles) ->
    lists:foreach(fun(M) -> do_check_sys(M, SysFiles) end, Mandatory).

do_check_sys(Prefix, Specs) ->
    case lookup_spec(Prefix, Specs) of
        [] ->
            reltool_utils:throw_error("Mandatory system directory ~s "
				      "is not included",
                                      [Prefix]);
        _ ->
            ok
    end.

spec_start_file(#sys{boot_rel = BootRelName,
                     rels = Rels,
                     apps = Apps}) ->
    {value, Erts} = lists:keysearch(erts, #app.name, Apps),
    {value, BootRel} = lists:keysearch(BootRelName, #rel.name, Rels),
    Data = Erts#app.vsn ++ " " ++ BootRel#rel.vsn ++ "\n",
    {BootRel#rel.vsn, {write_file, "start_erl.data", Data}}.

lookup_spec(Prefix, Specs) ->
    lists:filter(fun(S) -> lists:prefix(Prefix, element(2, S)) end, Specs).

safe_lookup_spec(Prefix, Specs) ->
    case lookup_spec(Prefix, Specs) of
        [] ->
	    %% io:format("lookup fail ~s:\n\t~p\n", [Prefix, Specs]),
            reltool_utils:throw_error("Mandatory system file ~s is "
				      "not included", [Prefix]);
        Match ->
            Match
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Specify applications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

spec_lib_files(#sys{apps = Apps} = Sys) ->
    Filter = fun(#app{is_escript = IsEscript, is_included = IsIncl,
                      is_pre_included = IsPre, name = Name}) ->
                     if
                         Name =:= ?MISSING_APP_NAME ->
                             false;
                         IsEscript ->
                             false;
                         IsIncl; IsPre ->
                             true;
                         true ->
                             false
                     end
             end,
    SelectedApps = lists:filter(Filter, Apps),
    check_apps([kernel, stdlib], SelectedApps),
    lists:flatten([spec_app(App, Sys) || App <- SelectedApps]).

check_apps([Mandatory | Names], Apps) ->
    case lists:keymember(Mandatory, #app.name, Apps) of
        false ->
            reltool_utils:throw_error("Mandatory application ~p is "
				      "not included in ~p",
                                      [Mandatory, Apps]);
        true ->
            check_apps(Names, Apps)
    end;
check_apps([], _) ->
    ok.

spec_app(#app{name              = Name,
              mods              = Mods,
              active_dir        = SourceDir,
              incl_app_filters  = AppInclRegexps,
              excl_app_filters  = AppExclRegexps} = App,
         #sys{incl_app_filters  = SysInclRegexps,
              excl_app_filters  = SysExclRegexps,
              debug_info        = SysDebugInfo} = Sys) ->
    %% List files recursively
    {create_dir, _, AppFiles} = spec_dir(SourceDir),

    %% Replace ebin
    AppUpFilename = atom_to_list(Name) ++ ".appup",
    EbinDir = filename:join([SourceDir, "ebin"]),
    OptAppUpFileSpec = spec_opt_copy_file(EbinDir, AppUpFilename),
    OptAppFileSpec = spec_app_file(App, Sys, EbinDir),
    ModSpecs = [spec_mod(M, SysDebugInfo) || M <- Mods,
					     M#mod.is_included,
					     M#mod.exists],
    NewEbin = {create_dir,
	       "ebin",
	       OptAppUpFileSpec ++ OptAppFileSpec ++ ModSpecs},
    AppFiles2 = lists:keystore("ebin", 2, AppFiles, NewEbin),

    %% Apply file filter
    InclRegexps = reltool_utils:default_val(AppInclRegexps, SysInclRegexps),
    ExclRegexps = reltool_utils:default_val(AppExclRegexps, SysExclRegexps),
    AppFiles3 = filter_spec(AppFiles2, InclRegexps, ExclRegexps),

    %% Regular top directory and/or archive
    spec_archive(App, Sys, AppFiles3).

spec_archive(#app{label               = Label,
                  active_dir          = SourceDir,
                  incl_archive_filters = AppInclArchiveDirs,
                  excl_archive_filters = AppExclArchiveDirs,
                  archive_opts        = AppArchiveOpts},
             #sys{root_dir            = RootDir,
                  incl_archive_filters = SysInclArchiveDirs,
                  excl_archive_filters = SysExclArchiveDirs,
                  archive_opts        = SysArchiveOpts},
             Files) ->
    InclArchiveDirs =
	reltool_utils:default_val(AppInclArchiveDirs, SysInclArchiveDirs),
    ExclArchiveDirs =
	reltool_utils:default_val(AppExclArchiveDirs, SysExclArchiveDirs),
    ArchiveOpts =
	reltool_utils:default_val(AppArchiveOpts, SysArchiveOpts),
    Match = fun(F) -> match(element(2, F), InclArchiveDirs, ExclArchiveDirs) end,
    case lists:filter(Match, Files) of
        [] ->
            %% Nothing to archive
            [spec_create_dir(RootDir, SourceDir, Label, Files)];
        ArchiveFiles ->
            OptDir =
                case Files -- ArchiveFiles of
                    [] ->
                        [];
                    ExternalFiles ->
                        [spec_create_dir(RootDir,
					 SourceDir,
					 Label,
					 ExternalFiles)]
                end,
            ArchiveOpts =
		reltool_utils:default_val(AppArchiveOpts, SysArchiveOpts),
            ArchiveDir =
		spec_create_dir(RootDir, SourceDir, Label, ArchiveFiles),
            [{archive, Label ++ ".ez", ArchiveOpts, [ArchiveDir]} | OptDir]
    end.

spec_dir(Dir) ->
    Base = filename:basename(Dir),
    case erl_prim_loader:read_file_info(Dir) of
        {ok, #file_info{type = directory}} ->
            case erl_prim_loader:list_dir(Dir) of
                {ok, Files} ->
                    %% Directory
                    {create_dir,
		     Base,
		     [spec_dir(filename:join([Dir, F])) || F <- Files]};
                error ->
                    reltool_utils:throw_error("list dir ~s failed", [Dir])
            end;
        {ok, #file_info{type = regular}} ->
            %% Plain file
            {copy_file, Base};
        _ ->
            reltool_utils:throw_error("read file info ~s failed", [Dir])
    end.

spec_mod(Mod, DebugInfo) ->
    File = atom_to_list(Mod#mod.name) ++ code:objfile_extension(),
    case reltool_utils:default_val(Mod#mod.debug_info, DebugInfo) of
        keep ->
            {copy_file, File};
        strip ->
            {strip_beam, File}
    end.

spec_app_file(#app{name = Name,
                   info = Info,
                   mods = Mods,
                   app_file = AppFile} = App,
              #sys{app_file = SysAppFile},
              EbinDir) ->
    AppFilename = atom_to_list(Name) ++ ".app",
    case reltool_utils:default_val(AppFile, SysAppFile) of
        keep ->
            %% Copy if it exists
            spec_opt_copy_file(EbinDir, AppFilename);
        strip ->
            %% Remove non-included modules
            %% Generate new file
            ModNames = [M#mod.name || M <- Mods,
                                      M#mod.is_included,
                                      lists:member(M#mod.name,
                                                   Info#app_info.modules)],
            App2 = App#app{info = Info#app_info{modules = ModNames}},
            Contents = gen_app(App2),
            AppIoList = io_lib:format("%% app generated at ~w ~w\n~p.\n\n",
                                      [date(), time(), Contents]),
            [{write_file, AppFilename, AppIoList}];
        all ->
            %% Include all included modules
            %% Generate new file
            ModNames = [M#mod.name || M <- Mods, M#mod.is_included],
            App2 = App#app{info = Info#app_info{modules = ModNames}},
            Contents = gen_app(App2),
            AppIoList = io_lib:format("%% app generated at ~w ~w\n~p.\n\n",
                                      [date(), time(), Contents]),
            [{write_file, AppFilename, AppIoList}]

    end.

spec_opt_copy_file(DirName, BaseName) ->
    case filelib:is_regular(filename:join([DirName, BaseName]),
			    erl_prim_loader) of
        true -> [{copy_file, BaseName}];
        false -> []
    end.

spec_create_dir(RootDir, SourceDir, BaseDir, Files) ->
    LibDir = filename:join([RootDir, "lib"]),
    case abs_to_rel_path(LibDir, SourceDir) of
        {relative, Dir} ->  {create_dir, Dir, Files};
        {absolute, Dir} ->  {create_dir, BaseDir, Dir, Files}
    end.

abs_to_rel_path(RootDir, SourcePath) ->
    R = filename:split(RootDir),
    S = filename:split(SourcePath),
    abs_to_rel_path(R, S, SourcePath).

abs_to_rel_path([H | R], [H | S], SourcePath) ->
    abs_to_rel_path(R, S, SourcePath);
abs_to_rel_path([], S, _SourcePath) ->
    {relative, filename:join(S)};
abs_to_rel_path(_, _, SourcePath) ->
    {absolute, SourcePath}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluate specification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_spec(Spec, SourceDir, TargetDir) ->
    SourceDir2 = filename:absname(SourceDir),
    TargetDir2 = filename:absname(TargetDir),
    try
        case filelib:is_dir(TargetDir2) of
            true ->
                do_eval_spec(Spec, SourceDir2, SourceDir2, TargetDir2),
                ok;
            false ->
                {error, TargetDir2 ++ ": " ++ file:format_error(enoent)}
        end
    catch
        throw:{error, Text} ->
            cleanup_spec(Spec, TargetDir2),
            {error, Text}
    end.

do_eval_spec(List, OrigSourceDir, SourceDir, TargetDir) when is_list(List) ->
    lists:foreach(fun(F) ->
			  do_eval_spec(F, OrigSourceDir, SourceDir, TargetDir)
		  end,
		  List);
%% do_eval_spec({source_dir, SourceDir2, Spec}, OrigSourceDir, _SourceDir, TargetDir) ->
%%     %% Source dir is absolute or relative the original source dir
%%     SourceDir3 = filename:join([OrigSourceDir, SourceDir2]),
%%     do_eval_spec(Spec, OrigSourceDir, SourceDir3, TargetDir);
do_eval_spec({create_dir, Dir, Files}, OrigSourceDir, SourceDir, TargetDir) ->
    SourceDir2 = filename:join([SourceDir, Dir]),
    TargetDir2 = filename:join([TargetDir, Dir]),
    reltool_utils:create_dir(TargetDir2),
    do_eval_spec(Files, OrigSourceDir, SourceDir2, TargetDir2);
do_eval_spec({create_dir, Dir, OldDir, Files},
	     OrigSourceDir,
	     _SourceDir,
	     TargetDir) ->
    SourceDir2 = filename:join([OrigSourceDir, OldDir]),
    TargetDir2 = filename:join([TargetDir, Dir]),
    reltool_utils:create_dir(TargetDir2),
    do_eval_spec(Files, SourceDir2, SourceDir2, TargetDir2);
do_eval_spec({archive, Archive, Options, Files},
	     OrigSourceDir,
	     SourceDir,
	     TargetDir) ->
    TmpSpec = {create_dir, "tmp", Files},
    TmpDir = filename:join([TargetDir, "tmp"]),
    reltool_utils:create_dir(TmpDir),
    do_eval_spec(Files, OrigSourceDir, SourceDir, TmpDir),

    ArchiveFile = filename:join([TargetDir, Archive]),
    Files2 = [element(2, F) || F <- Files],
    Res = zip:create(ArchiveFile, Files2, [{cwd, TmpDir} | Options]),

    cleanup_spec(TmpSpec, TargetDir),
    case Res of
        {ok, _} ->
            ok;
        {error, Reason} ->
            reltool_utils:throw_error("create archive ~s failed: ~p",
				      [ArchiveFile, Reason])
    end;
do_eval_spec({copy_file, File}, _OrigSourceDir, SourceDir, TargetDir) ->
    SourceFile = filename:join([SourceDir, File]),
    TargetFile = filename:join([TargetDir, File]),
    reltool_utils:copy_file(SourceFile, TargetFile);
do_eval_spec({copy_file, File, OldFile},
	     OrigSourceDir,
	     _SourceDir,
	     TargetDir) ->
    SourceFile = filename:join([OrigSourceDir, OldFile]),
    TargetFile = filename:join([TargetDir, File]),
    reltool_utils:copy_file(SourceFile, TargetFile);
do_eval_spec({write_file, File, IoList},
	     _OrigSourceDir,
	     _SourceDir,
	     TargetDir) ->
    TargetFile = filename:join([TargetDir, File]),
    reltool_utils:write_file(TargetFile, IoList);
do_eval_spec({strip_beam, File}, _OrigSourceDir, SourceDir, TargetDir) ->
    SourceFile = filename:join([SourceDir, File]),
    TargetFile = filename:join([TargetDir, File]),
    BeamBin = reltool_utils:read_file(SourceFile),
    {ok, {_, BeamBin2}} = beam_lib:strip(BeamBin),
    reltool_utils:write_file(TargetFile, BeamBin2).

cleanup_spec(List, TargetDir) when is_list(List) ->
    lists:foreach(fun(F)-> cleanup_spec(F, TargetDir) end, List);
%% cleanup_spec({source_dir, _SourceDir, Spec}, TargetDir) ->
%%     cleanup_spec(Spec, TargetDir);
cleanup_spec({create_dir, Dir, Files}, TargetDir) ->
    TargetDir2 = filename:join([TargetDir, Dir]),
    cleanup_spec(Files, TargetDir2),
    file:del_dir(TargetDir2);
cleanup_spec({create_dir, Dir, _OldDir, Files}, TargetDir) ->
    TargetDir2 = filename:join([TargetDir, Dir]),
    cleanup_spec(Files, TargetDir2),
    file:del_dir(TargetDir2);
cleanup_spec({archive, Archive, _Options, Files}, TargetDir) ->
    TargetFile = filename:join([TargetDir, Archive]),
    file:delete(TargetFile),
    TmpDir = filename:join([TargetDir, "tmp"]),
    cleanup_spec(Files, TmpDir),
    file:del_dir(TmpDir);
cleanup_spec({copy_file, File}, TargetDir) ->
    TargetFile = filename:join([TargetDir, File]),
    file:delete(TargetFile);
cleanup_spec({copy_file, NewFile, _OldFile}, TargetDir) ->
    TargetFile = filename:join([TargetDir, NewFile]),
    file:delete(TargetFile);
cleanup_spec({write_file, File, _IoList}, TargetDir) ->
    TargetFile = filename:join([TargetDir, File]),
    file:delete(TargetFile);
cleanup_spec({strip_beam, File}, TargetDir) ->
    TargetFile = filename:join([TargetDir, File]),
    file:delete(TargetFile).

filter_spec(List, InclRegexps, ExclRegexps) ->
    do_filter_spec("", List, InclRegexps, ExclRegexps).

do_filter_spec(Path, List, InclRegexps, ExclRegexps) when is_list(List) ->
    lists:zf(fun(File) ->
		     do_filter_spec(Path, File, InclRegexps, ExclRegexps)
	     end,
	     List);
%% do_filter_spec(Path, {source_dir, _SourceDir, Spec}, InclRegexps, ExclRegexps) ->
%%     do_filter_spec(Path, Spec, InclRegexps, ExclRegexps);
do_filter_spec(Path, {create_dir, Dir, Files}, InclRegexps, ExclRegexps) ->
    Path2 = opt_join(Path, Dir),
    case do_filter_spec(Path2, Files, InclRegexps, ExclRegexps) of
        [] ->
            case match(Path2, InclRegexps, ExclRegexps) of
                true ->
                    {true, {create_dir, Dir, []}};
                false ->
                    false
            end;
        Files2 when is_list(Files2) ->
            {true, {create_dir, Dir, Files2}}
    end;
do_filter_spec(Path,
	       {create_dir, NewDir, OldDir, Files},
	       InclRegexps,
	       ExclRegexps) ->
    Path2 = opt_join(Path, NewDir),
    case do_filter_spec(Path2, Files, InclRegexps, ExclRegexps) of
        [] ->
            case match(Path2, InclRegexps, ExclRegexps) of
                true ->
                    {true, {create_dir, NewDir, OldDir, []}};
                false ->
                    false
            end;
        Files2 when is_list(Files2) ->
            {true, {create_dir, NewDir, OldDir, Files2}}
    end;
do_filter_spec(Path,
	       {archive, Archive, Options, Files},
	       InclRegexps,
	       ExclRegexps) ->
    case do_filter_spec(Path, Files, InclRegexps, ExclRegexps) of
        [] ->
            case match(Path, InclRegexps, ExclRegexps) of
                true ->
                    {true, {archive, Archive, Options, []}};
                false ->
                    false
            end;
        Files2 when is_list(Files2) ->
            {true, {archive, Archive, Options, Files2}}
    end;
do_filter_spec(Path, {copy_file, File}, InclRegexps, ExclRegexps) ->
    Path2 = opt_join(Path, File),
    match(Path2, InclRegexps, ExclRegexps);
do_filter_spec(Path,
	       {copy_file, NewFile, _OldFile},
	       InclRegexps,
	       ExclRegexps) ->
    Path2 = opt_join(Path, NewFile),
    match(Path2, InclRegexps, ExclRegexps);
do_filter_spec(Path, {write_file, File, _IoList}, InclRegexps, ExclRegexps) ->
    Path2 = opt_join(Path, File),
    match(Path2, InclRegexps, ExclRegexps);
do_filter_spec(Path, {strip_beam, File}, InclRegexps, ExclRegexps) ->
    Path2 = opt_join(Path, File),
    match(Path2, InclRegexps, ExclRegexps).

opt_join([], File) ->
    File;
opt_join(Path, File) ->
    filename:join([Path, File]).

match(String, InclRegexps, ExclRegexps) ->
    match(String, InclRegexps) andalso not match(String, ExclRegexps).

%% Match at least one regexp
match(_String, []) ->
    false;
match(String, [#regexp{source = _, compiled = MP} | Regexps]) ->
    %% io:format("Regexp: ~p ~p\n", [String, Regexp]),
    case re:run(String, MP, [{capture, none}]) of
        nomatch -> match(String, Regexps);
        match   -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Old style installation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

install(RelName, TargetDir) ->
    try
        do_install(RelName, TargetDir)
    catch
        throw:{error, Text} ->
            {error, Text}
    end.

do_install(RelName, TargetDir) ->
    TargetDir2 = filename:absname(TargetDir),
    RelDir = filename:join([TargetDir2, "releases"]),
    DataFile = filename:join([RelDir, "start_erl.data"]),
    Bin = reltool_utils:read_file(DataFile),
    case string:tokens(binary_to_list(Bin), " \n") of
        [ErlVsn, RelVsn | _] ->
            ErtsBinDir = filename:join([TargetDir2, "erts-" ++ ErlVsn, "bin"]),
            BinDir = filename:join([TargetDir2, "bin"]),
	    case os:type() of
		{win32, _} ->
		    NativeRootDir = filename:nativename(TargetDir2),
		    %% NativeBinDir =
		    %% filename:nativename(filename:join([BinDir, "win32"])),
		    NativeBinDir = filename:nativename(BinDir),
		    IniData = ["[erlang]\r\n",
			       "Bindir=", NativeBinDir, "\r\n",
			       "Progname=erl\r\n",
			       "Rootdir=", NativeRootDir, "\r\n"],
		    IniFile = filename:join([BinDir, "erl.ini"]),
                    ok = file:write_file(IniFile, IniData);
		_ ->
		    subst_src_scripts(start_scripts(),
				      ErtsBinDir,
				      BinDir,
				      [{"FINAL_ROOTDIR", TargetDir2},
				       {"EMU", "beam"}],
				      [preserve])
	    end,
            RelFile = filename:join([RelDir, RelVsn, RelName ++ ".rel"]),
            ok = release_handler:create_RELEASES(TargetDir2, RelFile),
            ok;
        _ ->
            reltool_utils:throw_error("~s: Illegal data file syntax", [DataFile])
    end.

subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) ->
    Fun = fun(Script) ->
		  subst_src_script(Script, SrcDir, DestDir, Vars, Opts)
	  end,
    lists:foreach(Fun, Scripts).

subst_src_script(Script, SrcDir, DestDir, Vars, Opts) ->
    subst_file(filename:join([SrcDir, Script ++ ".src"]),
               filename:join([DestDir, Script]),
               Vars,
               Opts).

subst_file(Src, Dest, Vars, Opts) ->
    Bin = reltool_utils:read_file(Src),
    Chars = subst(binary_to_list(Bin), Vars),
    reltool_utils:write_file(Dest, Chars),
    case lists:member(preserve, Opts) of
        true ->
            FileInfo = reltool_utils:read_file_info(Src),
            reltool_utils:write_file_info(Dest, FileInfo);
        false ->
            ok
    end.

%% subst(Str, Vars)
%% Vars = [{Var, Val}]
%% Var = Val = string()
%% Substitute all occurrences of %Var% for Val in Str, using the list
%% of variables in Vars.
%%
subst(Str, Vars) ->
    subst(Str, Vars, []).

subst([$%, C | Rest], Vars, Result) when $A =< C, C =< $Z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C | Rest], Vars, Result) when $a =< C, C =< $z ->
    subst_var([C| Rest], Vars, Result, []);
subst([$%, C | Rest], Vars, Result) when  C == $_ ->
    subst_var([C| Rest], Vars, Result, []);
subst([C| Rest], Vars, Result) ->
    subst(Rest, Vars, [C| Result]);
subst([], _Vars, Result) ->
    lists:reverse(Result).

subst_var([$%| Rest], Vars, Result, VarAcc) ->
    Key = lists:reverse(VarAcc),
    case lists:keysearch(Key, 1, Vars) of
        {value, {Key, Value}} ->
            subst(Rest, Vars, lists:reverse(Value, Result));
        false ->
            subst(Rest, Vars, [$% | VarAcc ++ [$% | Result]])
    end;
subst_var([C| Rest], Vars, Result, VarAcc) ->
    subst_var(Rest, Vars, Result, [C| VarAcc]);
subst_var([], Vars, Result, VarAcc) ->
    subst([], Vars, [VarAcc ++ [$% | Result]]).

start_scripts() ->
    ["erl", "start", "start_erl"].
