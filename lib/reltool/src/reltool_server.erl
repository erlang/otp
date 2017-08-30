%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(reltool_server).

%% Public
-export([
         start_link/0, start_link/1,
         get_config/3, load_config/2, save_config/4,
         get_rel/2, get_script/2,
         reset_config/1, undo_config/1,
         get_mod/2,
         get_app/2, set_app/2,
         get_apps/2, set_apps/2,
         get_sys/1, set_sys/2,
         get_status/1,
         gen_rel_files/2, gen_target/2, gen_spec/1
        ]).

%% Internal
-export([init/1, loop/1]).

%% sys callback functions
-export([
         system_continue/3,
         system_terminate/4,
         system_code_change/4
        ]).

-include("reltool.hrl").

-record(state,
        {options,
         parent_pid,
         common,
         sys,
         old_sys,
         status,
         old_status,
	 app_tab,
	 old_app_tab,
	 mod_tab,
	 old_mod_tab,
	 mod_used_by_tab}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client

start_link() ->
    start_link([]).

start_link(Options) ->
    proc_lib:start_link(?MODULE,
			init,
			[[{parent, self()} | Options]],
			infinity,
			[]).

get_config(Pid, InclDef, InclDeriv) ->
    reltool_utils:call(Pid, {get_config, InclDef, InclDeriv}).

load_config(Pid, FilenameOrConfig) ->
    reltool_utils:call(Pid, {load_config, FilenameOrConfig}).

save_config(Pid, Filename, InclDef, InclDeriv) ->
    reltool_utils:call(Pid, {save_config, Filename, InclDef, InclDeriv}).

reset_config(Pid) ->
    reltool_utils:call(Pid, reset_config).

undo_config(Pid) ->
    reltool_utils:call(Pid, undo_config).

get_rel(Pid, RelName) ->
    reltool_utils:call(Pid, {get_rel, RelName}).

get_script(Pid, RelName) ->
    reltool_utils:call(Pid, {get_script, RelName}).

get_mod(Pid, ModName) ->
    reltool_utils:call(Pid, {get_mod, ModName}).

get_app(Pid, AppName) ->
    reltool_utils:call(Pid, {get_app, AppName}).

set_app(Pid, App) ->
    reltool_utils:call(Pid, {set_app, App}).

get_apps(Pid, Kind) ->
    reltool_utils:call(Pid, {get_apps, Kind}).

set_apps(Pid, Apps) ->
    reltool_utils:call(Pid, {set_apps, Apps}).

get_sys(Pid) ->
    reltool_utils:call(Pid, get_sys).

set_sys(Pid, Sys) ->
    reltool_utils:call(Pid, {set_sys, Sys}).

get_status(Pid) ->
    reltool_utils:call(Pid, get_status).

gen_rel_files(Pid, Dir) ->
    reltool_utils:call(Pid, {gen_rel_files, Dir}).

gen_target(Pid, Dir) ->
    reltool_utils:call(Pid, {gen_target, Dir}).

gen_spec(Pid) ->
    reltool_utils:call(Pid, gen_spec).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server

init([{parent,Parent}|_] = Options) ->
    try
        do_init(Options)
    catch
	throw:{error,Reason} ->
	    proc_lib:init_ack(Parent,{error,Reason});
        error:Reason ->
            exit({Reason, erlang:get_stacktrace()})
    end.

do_init(Options) ->
    AppTab = ets:new(reltool_apps1, [public, ordered_set, {keypos,#app.name}]),
    OldAppTab = ets:new(reltool_apps2, [public, ordered_set, {keypos,#app.name}]),
    ModTab = ets:new(reltool_mods1, [public, ordered_set, {keypos,#mod.name}]),
    OldModTab = ets:new(reltool_mods2, [public, ordered_set, {keypos,#mod.name}]),
    ModUsesTab = ets:new(reltool_mod_uses, [public, bag, {keypos, 1}]),
    S = #state{options = Options,
	       app_tab = AppTab,
	       old_app_tab = OldAppTab,
	       mod_tab = ModTab,
	       old_mod_tab = OldModTab,
	       mod_used_by_tab = ModUsesTab},

    S2 = parse_options(S),
    {S3, Apps, Status2} = refresh(S2),
    Status3 =  analyse(S3, Apps, Status2),
    %% Set old_xxx equal to xxx to allow undo=nop
    FakeBackup = {ets:tab2list(S3#state.app_tab),ets:tab2list(S3#state.mod_tab)},
    S4 = save_old(S3, S3, FakeBackup, Status3),
    #state{parent_pid = Parent, sys=Sys, common=C} = S4,
    proc_lib:init_ack(Parent, {ok, self(), C, Sys#sys{apps=undefined}}),
    loop(S4).

parse_options(S) ->
    Sys = default_sys(),
    C = #common{sys_debug = [],
		wx_debug = 0,
		trap_exit = true},
    parse_options(S#state.options, S, C, Sys).

default_sys() ->
    #sys{root_dir          = reltool_utils:root_dir(),
	 lib_dirs          = reltool_utils:erl_libs(),
	 escripts          = [],
	 incl_cond         = ?DEFAULT_INCL_COND,
	 mod_cond          = ?DEFAULT_MOD_COND,
	 apps              = ?DEFAULT_APPS,
	 boot_rel          = ?DEFAULT_REL_NAME,
	 rels              = reltool_utils:default_rels(),
	 emu_name          = ?DEFAULT_EMU_NAME,
	 profile           = ?DEFAULT_PROFILE,
	 incl_sys_filters  = dec_re(incl_sys_filters,
				    ?DEFAULT_INCL_SYS_FILTERS,
				    []),
	 excl_sys_filters  = dec_re(excl_sys_filters,
				    ?DEFAULT_EXCL_SYS_FILTERS,
				    []),
	 incl_app_filters  = dec_re(incl_app_filters,
				    ?DEFAULT_INCL_APP_FILTERS,
				    []),
	 excl_app_filters  = dec_re(excl_app_filters,
				    ?DEFAULT_EXCL_APP_FILTERS,
				    []),
	 relocatable       = ?DEFAULT_RELOCATABLE,
	 rel_app_type      = ?DEFAULT_REL_APP_TYPE,
	 embedded_app_type = ?DEFAULT_EMBEDDED_APP_TYPE,
	 app_file          = ?DEFAULT_APP_FILE,
	 incl_archive_filters = dec_re(incl_archive_filters,
				       ?DEFAULT_INCL_ARCHIVE_FILTERS,
				       []),
	 excl_archive_filters = dec_re(excl_archive_filters,
				       ?DEFAULT_EXCL_ARCHIVE_FILTERS,
				       []),
	 archive_opts      = ?DEFAULT_ARCHIVE_OPTS,
	 debug_info        = ?DEFAULT_DEBUG_INFO}.

dec_re(Key, Regexps, Old) ->
    reltool_utils:decode_regexps(Key, Regexps, Old).

parse_options([{Key, Val} | KeyVals], S, C, Sys) ->
    case Key of
        parent ->
            parse_options(KeyVals, S#state{parent_pid = Val}, C, Sys);
        sys_debug ->
            parse_options(KeyVals, S, C#common{sys_debug = Val}, Sys);
        wx_debug ->
            parse_options(KeyVals, S, C#common{wx_debug = Val}, Sys);
        trap_exit ->
            parse_options(KeyVals, S, C#common{trap_exit = Val}, Sys);
        config ->
            Sys2 = read_config(Sys, Val),
            parse_options(KeyVals, S, C, Sys2);
        sys ->
            Sys2 = read_config(Sys, {sys, Val}),
            parse_options(KeyVals, S, C, Sys2);
        _ ->
	    reltool_utils:throw_error("Illegal option: ~p", [{Key, Val}])
    end;
parse_options([], S, C, Sys) ->
    S#state{common = C, sys = Sys};
parse_options(KeyVals, _S, _C, _Sys) ->
    reltool_utils:throw_error("Illegal option: ~p", [KeyVals]).

loop(#state{sys = Sys} = S) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg,
				  From,
				  S#state.parent_pid,
				  ?MODULE,
				  (S#state.common)#common.sys_debug,
				  S);
        {call, ReplyTo, Ref, {get_config, InclDef, InclDeriv}} ->
            Reply = do_get_config(S, InclDef, InclDeriv),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {load_config, SysConfig}} ->
	    Fun = fun() -> do_load_config(S, SysConfig) end,
	    {S3, Status2} = config_and_refresh(S, Fun),
            reltool_utils:reply(ReplyTo, Ref, Status2),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, {save_config, Filename, InclDef, InclDeriv}} ->
            Reply = do_save_config(S, Filename, InclDef, InclDeriv),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, reset_config} ->
	    Fun = fun() -> parse_options(S) end,
	    {S3, Status2} = config_and_refresh(S, Fun),
            reltool_utils:reply(ReplyTo, Ref, Status2),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, undo_config} ->
            S2 = S#state{sys = S#state.old_sys,
                         old_sys = Sys,
			 status = S#state.old_status,
			 old_status = S#state.status,
			 app_tab = S#state.old_app_tab,
			 old_app_tab = S#state.app_tab,
			 mod_tab = S#state.old_mod_tab,
			 old_mod_tab = S#state.mod_tab},
            reltool_utils:reply(ReplyTo, Ref, ok),
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, {get_rel, RelName}} ->
            Reply =
                case lists:keysearch(RelName, #rel.name, Sys#sys.rels) of
                    {value, Rel} ->
                        reltool_target:gen_rel(Rel, sys_all_apps(S));
                    false ->
                        {error, "No such release: " ++ RelName}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_script, RelName}} ->
            Reply =
                case lists:keysearch(RelName, #rel.name, Sys#sys.rels) of
                    {value, Rel} ->
                        PathFlag = true,
                        Vars = [],
                        reltool_target:gen_script(Rel, sys_all_apps(S),
						  PathFlag, Vars);
                    false ->
                        {error, "No such release: " ++ RelName}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_mod, ModName}} ->
            Reply =
                case ets:lookup(S#state.mod_tab, ModName) of
                    [M] ->
                        {ok, M};
                    [] ->
                        {ok, missing_mod(ModName, ?MISSING_APP_NAME)}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_app, AppName}} when is_atom(AppName) ->
            Reply =
                case ets:lookup(S#state.app_tab,AppName) of
                    [App] ->
                        {ok, App};
                    [] ->
                        {error, "No such application: " ++
			 atom_to_list(AppName)}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_app, App}} ->
	    Fun = fun() -> do_set_apps(S, [App]) end,
	    {S3, Status2} = config_and_refresh(S, Fun),
	    Reply =
		case Status2 of
		    {ok, Warnings} ->
			[App2] = ets:lookup(S3#state.app_tab,App#app.name),
			{ok, App2, Warnings};
		    {error, _} ->
			Status2
		end,
	    reltool_utils:reply(ReplyTo, Ref, Reply),
	    ?MODULE:loop(S3);
        {call, ReplyTo, Ref, {get_apps, Kind}} ->
            AppNames =
                case Kind of
                    whitelist ->
			%% Pre-included
			ets:select(S#state.app_tab,
				   [{#app{is_pre_included=true,_='_'},
				     [],
				     ['$_']}]);
		    blacklist ->
			%% Pre-excluded
			ets:select(S#state.app_tab,
				   [{#app{is_pre_included=false,_='_'},
				     [],
				     ['$_']}]);
		    source ->
			%% Not included and not pre-excluded
			ets:select(S#state.app_tab,
				   [{#app{is_included='$1',
					  is_pre_included='$2',
					  _='_'},
				     [{'=/=','$1',true},
				      {'=/=','$2',false}],
				     ['$_']}]);
                    derived ->
			%% Included, but not pre-included
			ets:select(S#state.app_tab,
				   [{#app{is_included='$1',
					  is_pre_included='$2',
					  _='_'},
				     [{'=:=','$1',true},
				      {'=/=','$2',true}],
				     ['$_']}])
                end,
            reltool_utils:reply(ReplyTo, Ref, {ok, AppNames}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_apps, Apps}} ->
	    Fun = fun() -> do_set_apps(S, Apps) end,
	    {S3, Status2} = config_and_refresh(S, Fun),
            reltool_utils:reply(ReplyTo, Ref, Status2),
	    ?MODULE:loop(S3);
        {call, ReplyTo, Ref, get_sys} ->
            reltool_utils:reply(ReplyTo, Ref, {ok, Sys#sys{apps = undefined}}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_sys, Sys2}} ->
	    Fun = fun() -> S#state{sys =  Sys2#sys{apps = Sys#sys.apps}} end,
	    {S3, Status} = config_and_refresh(S, Fun),
            reltool_utils:reply(ReplyTo, Ref, Status),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, get_status} ->
            reltool_utils:reply(ReplyTo, Ref, S#state.status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_rel_files, Dir}} ->
            Status =
                case reltool_target:gen_rel_files(sys_all_apps(S), Dir) of
                    ok ->
                        {ok, []};
                    {error, Reason} ->
                        {error, Reason}
                end,
            reltool_utils:reply(ReplyTo, Ref, Status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_target, Dir}} ->
            Reply = reltool_target:gen_target(sys_all_apps(S), Dir),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, gen_spec} ->
            Reply = reltool_target:gen_spec(sys_all_apps(S)),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {'EXIT', Pid, Reason} when Pid =:= S#state.parent_pid ->
            exit(Reason);
        {call, ReplyTo, Ref, Msg} when is_pid(ReplyTo), is_reference(Ref) ->
            error_logger:format("~w~w got unexpected call:\n\t~p\n",
                                [?MODULE, self(), Msg]),
            reltool_utils:reply(ReplyTo, Ref, {error, {invalid_call, Msg}}),
            ?MODULE:loop(S);
        Msg ->
            error_logger:format("~w~w got unexpected message:\n\t~p\n",
                                [?MODULE, self(), Msg]),
            ?MODULE:loop(S)
    end.


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_set_apps(#state{sys = Sys} = S, ChangedApps) ->
    %% Create new list of configured applications
    SysApps = app_update_config(ChangedApps, Sys#sys.apps),
    S#state{sys = Sys#sys{apps = SysApps}}.

%% Re-create the #sys.apps list by
%% 1) taking configurable fields from the changed #app records and
%%    create new default records
%% 2) removing #app records if no configurable fields are set
%% 3) keeping #app records that are not changed
app_update_config([#app{name=Name,is_escript={inlined,Escript}}|_],_SysApps) ->
    reltool_utils:throw_error("Application ~w is inlined in ~w. Can not change "
			      "configuration for an inlined application.",
			      [Name,Escript]);
app_update_config([Config|Configs],SysApps) ->
    NewSysApps =
	case app_set_config_only(Config) of
	    {delete,Name} ->
		lists:keydelete(Name,#app.name,SysApps);
	    New ->
		lists:ukeymerge(#app.name,[New],SysApps)
	end,
    app_update_config(Configs,NewSysApps);
app_update_config([],SysApps) ->
    SysApps.

app_set_config_only(#app{mods=ConfigMods} = Config) ->
    app_set_config_only(mod_set_config_only(ConfigMods),Config).

app_set_config_only([],#app{name                 = Name,
			    incl_cond            = undefined,
			    mod_cond             = undefined,
			    use_selected_vsn     = undefined,
			    debug_info           = undefined,
			    app_file             = undefined,
			    app_type             = undefined,
			    incl_app_filters     = undefined,
			    excl_app_filters     = undefined,
			    incl_archive_filters = undefined,
			    excl_archive_filters = undefined,
			    archive_opts         = undefined,
			    is_escript           = false})->
    {delete,Name};
app_set_config_only(Mods,#app{name                 = Name,
			      incl_cond            = InclCond,
			      mod_cond             = ModCond,
			      use_selected_vsn     = UseSelectedVsn,
			      debug_info           = DebugInfo,
			      app_file             = AppFile,
			      app_type             = AppType,
			      incl_app_filters     = InclAppFilters,
			      excl_app_filters     = ExclAppFilters,
			      incl_archive_filters = InclArchiveFilters,
			      excl_archive_filters = ExclArchiveFilters,
			      archive_opts         = ArchiveOpts,
			      vsn                  = Vsn,
			      is_escript           = IsEscript,
			      label                = Label,
			      info                 = Info,
			      active_dir           = ActiveDir,
			      sorted_dirs          = SortedDirs}) ->
    App = (default_app(Name))#app{incl_cond            = InclCond,
				  mod_cond             = ModCond,
				  use_selected_vsn     = UseSelectedVsn,
				  debug_info           = DebugInfo,
				  app_file             = AppFile,
				  app_type             = AppType,
				  incl_app_filters     = InclAppFilters,
				  excl_app_filters     = ExclAppFilters,
				  incl_archive_filters = InclArchiveFilters,
				  excl_archive_filters = ExclArchiveFilters,
				  archive_opts         = ArchiveOpts,
				  vsn                  = Vsn,
				  mods                 = Mods},

    if IsEscript ->
	    %% Some fields shall only be set if it is an escript, e.g. label
	    %% must never be set for any other applications since that will
	    %% prevent refreshing.
	    App#app{is_escript  = IsEscript,
		    active_dir  = ActiveDir,
		    sorted_dirs = SortedDirs,
		    label       = Label,
		    info        = Info};
       UseSelectedVsn =:= dir ->
	    %% Must not loose active_dir if it is configured to be used
	    App#app{active_dir = ActiveDir,
		    sorted_dirs = [ActiveDir]};
       true ->
	    App
    end.

mod_set_config_only(ConfigMods) ->
    [#mod{name       = Name,
	  incl_cond  = InclCond,
	  debug_info = DebugInfo} ||
	#mod{name       = Name,
	     incl_cond  = InclCond,
	     debug_info = DebugInfo} <- ConfigMods,
	(InclCond =/= undefined) orelse (DebugInfo =/= undefined)].


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyse(#state{sys=Sys} = S, Apps, Status) ->
    %% Create a list of {RelName,AppName}, one element for each
    %% AppName that needs to be included for the given release.
    RelApps = apps_in_rels(Sys#sys.rels, Apps),

    %% Initiate is_pre_included and is_included for all applications
    %% based on #sys.incl_cond, #app.incl_cond and if the application
    %% is included in a release (rel spec - see apps_in_rels above).
    %% Then initiate the same for each module, and check that there
    %% are no duplicated module names (in different applications)
    %% where we can not decide which one to use.
    %% Write all #app to app_tab and all #mod to mod_tab.
    Status2 = apps_init_is_included(S, Apps, RelApps, Status),

    %% For each application that is not (directly or indirectly) part
    %% of a release, but still has #app.is_included==true, propagate
    %% is_included to the dependencies specified in the .app files.
    app_propagate_is_included(S),

    %% For each module that has #mod.is_included==true, propagate
    %% is_included to the modules it uses.
    mod_propagate_is_included(S),

    %% Insert reverse dependencies - i.e. for each
    %% #mod{name=Mod, uses_mods=[UsedMod]},
    %% insert an entry {UsedMod,Mod} in mod_used_by_tab.
    propagate_is_used_by(S),

    %% Set the above reverse dependencies in #mod records
    %% (used_by_mods) and accumulate in #app records.
    %% Make sure #app.is_included is always true if some
    %% #mod.is_included==true for at least one module in the app.
    %% Set status=missing|ok for #app and #mod - indicates if module
    %% (.beam file) is missing in file system.
    app_recap_dependencies(S),

    %% Check that the boot_rel exists.
    %% Check that all applications that are listed in a 'rel' spec are
    %% also really included in the target release.
    %% Check that all mandatory applications are included in all rels.
    verify_config(S, RelApps, Status2).

apps_in_rels(Rels, Apps) ->
    AllRelApps =
	lists:foldl(fun(Rel, RelApps) ->
			    MoreRelApps = apps_in_rel(Rel, Apps),
			    MoreRelApps ++ RelApps
		    end,
		    [],
		    Rels),
    lists:reverse(AllRelApps).

apps_in_rel(#rel{name = RelName, rel_apps = RelApps}, Apps) ->
    Mandatory = [{RelName, kernel}, {RelName, stdlib}],
    Explicit0 = [{RelName, AppName} || #rel_app{name=AppName} <- RelApps],
    Explicit = Mandatory ++ Explicit0,
    Deps =
	[{RelName, AppName} ||
	    RA <- RelApps,
	    AppName <-
		case lists:keyfind(RA#rel_app.name,
				   #app.name,
				   Apps) of
		    App=#app{info = #app_info{applications = AA}} ->
			%% Included applications in rel shall overwrite included
			%% applications in .app. I.e. included applications in
			%% .app shall only be used if it is not defined in rel.
			IA = case RA#rel_app.incl_apps of
				 undefined ->
				     (App#app.info)#app_info.incl_apps;
				 RelIA ->
				     RelIA
			     end,
			AA ++ IA;
		    false ->
			reltool_utils:throw_error(
			  "Release ~tp uses non existing "
			  "application ~w",
			  [RelName,RA#rel_app.name])
		end,
	    not lists:keymember(AppName, 2, Explicit)],
    more_apps_in_rels(Deps, Apps, Explicit).

more_apps_in_rels([{RelName, AppName} = RA | RelApps], Apps, Acc) ->
    case lists:member(RA, Acc) of
	true ->
	    more_apps_in_rels(RelApps, Apps, Acc);
	false ->
	    case lists:keyfind(AppName, #app.name, Apps) of
		#app{info = #app_info{applications = AA, incl_apps=IA}} ->
		    Extra = [{RelName, N} || N <- AA++IA],
		    Acc2 = more_apps_in_rels(Extra, Apps, [RA | Acc]),
		    more_apps_in_rels(RelApps, Apps, Acc2);
		false ->
		    reltool_utils:throw_error(
		      "Release ~tp uses non existing application ~w",
		      [RelName,AppName])
	    end
    end;
more_apps_in_rels([], _Apps, Acc) ->
    Acc.

apps_init_is_included(S, Apps, RelApps, Status) ->
    lists:foldl(fun(App, AccStatus) ->
			app_init_is_included(S, App, RelApps, AccStatus)
		end,
		Status,
		Apps).

app_init_is_included(#state{app_tab = AppTab, mod_tab = ModTab, sys=Sys},
		     #app{name = AppName, mods = Mods} = A,
		     RelApps,
		     Status) ->
    AppCond =
        case A#app.incl_cond of
            undefined -> Sys#sys.incl_cond;
            _         -> A#app.incl_cond
        end,
    ModCond =
        case A#app.mod_cond of
            undefined -> Sys#sys.mod_cond;
            _         -> A#app.mod_cond
        end,
    Rels = [RelName || {RelName, AN} <- RelApps, AN =:= AppName],
    {Default, IsPreIncl, IsIncl, Status2} =
        case {AppCond, Rels} of
            {include, _} ->
		{undefined, true, true, Status};
            {exclude, []} ->
		{undefined, false, false, Status};
            {exclude, [RelName | _]} -> % App is included in at least one rel
		reltool_utils:throw_error(
		  "Application ~w is used in release ~tp and cannot be excluded",
		  [AppName,RelName]);
            {derived, []} ->
		{undefined, undefined, undefined, Status};
            {derived, [_ | _]} -> % App is included in at least one rel
		{true, undefined, true, Status}
        end,
    {Mods2,Status3} = lists:mapfoldl(fun(Mod,Acc) ->
					     mod_init_is_included(ModTab,
								  Mod,
								  ModCond,
								  AppCond,
								  Default,
								  Acc)
				     end,
				     Status2,
				     Mods),
    A2 = A#app{mods = Mods2,
	       is_pre_included = IsPreIncl,
	       is_included = IsIncl,
	       rels = Rels},
    ets:insert(AppTab, A2),
    Status3.

mod_init_is_included(ModTab, M, ModCond, AppCond, Default, Status) ->
    %% print(M#mod.name, hipe, "incl_cond -> ~w\n", [AppCond]),
    IsIncl =
        case AppCond of
            include ->
                case M#mod.incl_cond of
                    include ->
                        true;
                    exclude ->
                        false;
		    derived ->
			undefined;
                    undefined ->
                        %% print(M#mod.name, hipe, "mod_cond -> ~w\n",
			%%       [ModCond]),
                        case ModCond of
                            all     -> true;
                            app     -> false_to_undefined(M#mod.is_app_mod);
                            ebin    -> false_to_undefined(M#mod.is_ebin_mod);
                            derived -> Default;
                            none    -> false
                        end
                end;
            exclude ->
                false;
            derived ->
                case M#mod.incl_cond of
                    include ->
                        true;
                    exclude ->
                        false;
		    derived ->
			undefined;
                    undefined ->
                        Default
                end
        end,

    M2 = M#mod{is_pre_included = IsIncl, is_included = IsIncl},

    Status2 =
	case ets:lookup(ModTab,M#mod.name) of
	    [Existing] ->
		case {Existing#mod.is_included,IsIncl} of
		    {false,_} ->
			ets:insert(ModTab, M2),
			reltool_utils:add_warning(
			  "Module ~w exists in applications ~w and ~w. "
			  "Using module from application ~w.",
			  [M#mod.name, Existing#mod.app_name,
			   M#mod.app_name, M#mod.app_name],
			  Status);
		    {_,false} ->
			%% Don't insert in ModTab - using Existing
			reltool_utils:add_warning(
			  "Module ~w exists in applications ~w and ~w. "
			  "Using module from application ~w.",
			  [M#mod.name, Existing#mod.app_name,
			   M#mod.app_name,Existing#mod.app_name],
			  Status);
		    {_,_} ->
			reltool_utils:throw_error(
			  "Module ~w potentially included by two different "
			  "applications: ~w and ~w.",
			  [M#mod.name,Existing#mod.app_name,M#mod.app_name])
		end;
	    [] ->
		ets:insert(ModTab, M2),
		Status
	end,

    %% print(M#mod.name, hipe, "~p -> ~w\n", [M2, IsIncl]),
    {M2,Status2}.

false_to_undefined(Bool) ->
    case Bool of
        false -> undefined;
        _     -> Bool
    end.

get_no_rel_apps_and_dependencies(S) ->
    ets:select(S#state.app_tab, [{#app{name='$1',
				       is_included=true,
				       info=#app_info{applications='$2',
						      incl_apps='$3',
						      _='_'},
				       rels=[],
				       _='_'},
				  [],
				  [{{'$1','$2','$3'}}]}]).

app_propagate_is_included(S) ->
    lists:foreach(
      fun({AppName,DepNames1,DepNames2}) ->
	      app_mark_is_included(S,AppName,DepNames1++DepNames2)
      end,
      get_no_rel_apps_and_dependencies(S)).

app_mark_is_included(#state{app_tab=AppTab, mod_tab=ModTab, sys=Sys}=S,UsedByName,[AppName|AppNames]) ->
    case ets:lookup(AppTab, AppName) of
	[A] ->
	    case A#app.is_included of
		undefined ->
		    %% Not yet marked => mark and propagate
		    A2 =
			case A#app.incl_cond of
			    include ->
				A#app{is_pre_included = true,
				      is_included = true};
			    exclude ->
				A#app{is_pre_included = false,
				      is_included = false};
			    AppInclCond when AppInclCond==undefined;
					     AppInclCond==derived  ->
				A#app{is_included = true}
			end,
		    ets:insert(AppTab, A2),

		    ModCond =
			case A#app.mod_cond of
			    undefined -> Sys#sys.mod_cond;
			    _         -> A#app.mod_cond
			end,
		    Filter =
			fun(M) ->
				case ModCond of
				    all     -> true;
				    app     -> M#mod.is_app_mod;
				    ebin    -> M#mod.is_ebin_mod;
				    derived -> false;
				    none    -> false
				end
			end,
		    Mods = lists:filter(Filter, A#app.mods),
		    %% Mark the modules of this app, but no need to go
		    %% recursive on modules since this is done in
		    %% mod_mark_is_included.
		    [case M#mod.is_included of
			 undefined ->
			     M2 =
				 case M#mod.incl_cond of
				     include ->
					 M#mod{is_pre_included = true,
					       is_included = true};
				     exclude ->
					 M#mod{is_pre_included = false,
					       is_included = false};
				     ModInclCond when ModInclCond==undefined;
						      ModInclCond==derived  ->
					 M#mod{is_included = true}
				 end,
			     ets:insert(ModTab, M2);
			 _ ->
			     ok
		     end || M <- Mods],

		    %% Go recursive on dependencies
		    #app{info=#app_info{applications=DepNames1,
					incl_apps=DepNames2}} = A,
		    app_mark_is_included(S,AppName,DepNames1++DepNames2);
		_ ->
		    %% Already marked
		    ok
	    end;
	[] ->
	    %% Missing app
	    reltool_utils:throw_error(
	      "Application ~tp uses non existing application ~w",
	      [UsedByName,AppName])
    end,
    app_mark_is_included(S, UsedByName, AppNames);
app_mark_is_included(_S, _UsedByName, []) ->
    ok.

%% Return the list for {ModName, UsesModNames} for all modules where
%% #mod.is_included==true.
get_all_mods_and_dependencies(S) ->
    ets:select(S#state.mod_tab, [{#mod{name='$1',
				       uses_mods='$2',
				       is_included=true,
				       _='_'},
				  [],
				  [{{'$1','$2'}}]}]).

mod_propagate_is_included(S) ->
    case lists:flatmap(
	   fun({ModName,UsesModNames}) ->
		   mod_mark_is_included(S,ModName,UsesModNames,[])
	   end,
	   get_all_mods_and_dependencies(S)) of
	[] ->
	    ok;
	MissingMods ->
	    MissingApp = default_app(?MISSING_APP_NAME, "missing"),
	    MissingApp2 = MissingApp#app{label = ?MISSING_APP_TEXT,
					 info = missing_app_info(""),
					 mods = MissingMods,
					 status = missing,
					 uses_mods = []},
	    ets:insert(S#state.app_tab, MissingApp2),
	    ok
    end.

mod_mark_is_included(#state{app_tab=AppTab, mod_tab=ModTab, sys=Sys} = S,
		     UsedByName, [ModName | ModNames], Acc) ->
    Acc3 =
        case ets:lookup(ModTab, ModName) of
            [M] ->
                case M#mod.is_included of
                    undefined ->
                        %% Not yet marked => mark and propagate
                        M2 =
                            case M#mod.incl_cond of
                                include ->
                                    M#mod{is_pre_included = true,
					  is_included = true};
                                exclude ->
                                    M#mod{is_pre_included = false,
					  is_included = false};
				ModInclCond when ModInclCond==undefined;
						 ModInclCond==derived  ->
                                    M#mod{is_included = true}
                            end,
                        ets:insert(ModTab, M2),
                        [A] = ets:lookup(AppTab, M2#mod.app_name),
                        Acc2 =
                            case A#app.is_included of
                                undefined ->
                                    ModCond =
                                        case A#app.mod_cond of
                                            undefined -> Sys#sys.mod_cond;
                                            _         -> A#app.mod_cond
                                        end,
                                    Filter =
                                        fun(M3) ->
                                                case ModCond of
                                                    all     -> true;
                                                    app     -> M3#mod.is_app_mod;
                                                    ebin    -> M3#mod.is_ebin_mod;
                                                    derived -> false;
                                                    none    -> false
                                                end
                                        end,
                                    Mods = lists:filter(Filter, A#app.mods),
                                    A2 = A#app{is_included = true},
                                    ets:insert(AppTab, A2),
                                    mod_mark_is_included(S,
							 ModName,
							 [M3#mod.name ||
							     M3 <- Mods],
							 Acc);
				_ ->
				    %% Already marked true or false
                                    Acc
                            end,
                        mod_mark_is_included(S, ModName, M2#mod.uses_mods, Acc2);
		    _ ->
                        %% Already marked true or false
                        Acc
                end;
            [] ->
                M = missing_mod(ModName, ?MISSING_APP_NAME),
                M2 = M#mod{is_included = true},
                ets:insert(ModTab, M2),
                [M2 | Acc]
        end,
    mod_mark_is_included(S, UsedByName, ModNames, Acc3);
mod_mark_is_included(_S, _UsedByName, [], Acc) ->
    Acc.

propagate_is_used_by(S) ->
    lists:foreach(
      fun({Mod,UsesMods}) ->
	      lists:foreach(
		fun(UsedMod) ->
			ets:insert(S#state.mod_used_by_tab,{UsedMod,Mod})
		end,
		UsesMods)
      end,
      get_all_mods_and_dependencies(S)).


app_recap_dependencies(S) ->
    ets:foldl(fun(App,_) -> app_recap_dependencies(S,App) end,
	      ok, S#state.app_tab).

app_recap_dependencies(S, #app{mods = Mods, is_included = IsIncl} = A) ->
    {Mods2, IsIncl2} = mod_recap_dependencies(S, A, Mods, [], IsIncl),
    AppStatus =
        case lists:keymember(missing, #mod.status, Mods2) of
            true  -> missing;
            false -> ok
        end,
    UsesMods = [M#mod.uses_mods || M <- Mods2, M#mod.is_included =:= true],
    UsesMods2 = lists:usort(lists:flatten(UsesMods)),
    UsesApps = [M#mod.app_name || ModName <- UsesMods2,
				  M <- ets:lookup(S#state.mod_tab, ModName)],
    UsesApps2 = lists:usort(UsesApps),
    UsedByMods = [M#mod.used_by_mods || M <- Mods2, M#mod.is_included =:= true],
    UsedByMods2 = lists:usort(lists:flatten(UsedByMods)),
    UsedByApps = [M#mod.app_name || ModName <- UsedByMods2,
				    M <- ets:lookup(S#state.mod_tab, ModName)],
    UsedByApps2 = lists:usort(UsedByApps),

    A2 = A#app{mods = Mods2,
               status = AppStatus,
               uses_mods = UsesMods2,
               used_by_mods = UsedByMods2,
               uses_apps = UsesApps2,
               used_by_apps = UsedByApps2,
               is_included = IsIncl2},
    ets:insert(S#state.app_tab,A2),
    ok.

mod_recap_dependencies(S, A, [#mod{name = ModName}=M1 | Mods], Acc, IsIncl) ->
    case ets:lookup(S#state.mod_tab, ModName) of
	[M2] when M2#mod.app_name=:=A#app.name ->
	    ModStatus = do_get_status(M2),
	    %% print(M2#mod.name, hipe, "status -> ~w\n", [ModStatus]),
	    {IsIncl2, M3} =
		case M2#mod.is_included of
		    true ->
			UsedByMods =
			    [N || {_, N} <- ets:lookup(S#state.mod_used_by_tab,
						       ModName)],
			{true, M2#mod{status = ModStatus, used_by_mods = UsedByMods}};
		    _    ->
			{IsIncl, M2#mod{status = ModStatus, used_by_mods = []}}
		end,
	    ets:insert(S#state.mod_tab, M3),
	    mod_recap_dependencies(S, A, Mods, [M3 | Acc], IsIncl2);
	[_] when A#app.is_included==false; M1#mod.incl_cond==exclude ->
	    %% App is explicitely excluded so it is ok that the module
	    %% record does not exist for this module in this
	    %% application.
	    mod_recap_dependencies(S, A, Mods, [M1 | Acc], IsIncl);
	[M2] ->
	    %% A module is potensially included by multiple
	    %% applications. This is not allowed!
	    reltool_utils:throw_error(
	      "Module ~w potentially included by two different applications: "
	      "~w and ~w.", [ModName,A#app.name, M2#mod.app_name])
    end;
mod_recap_dependencies(_S, _A, [], Acc, IsIncl) ->
    {lists:reverse(Acc), IsIncl}.

do_get_status(M) ->
    if
        M#mod.exists =:= false, M#mod.is_included =/= false ->
            missing;
        true ->
            ok
    end.

verify_config(#state{app_tab=AppTab, sys=#sys{boot_rel = BootRel, rels = Rels}},
	      RelApps, Status) ->
    case lists:keymember(BootRel, #rel.name, Rels) of
        true ->
	    Status2 = lists:foldl(fun(RA, Acc) ->
					  check_app(AppTab, RA, Acc) end,
				  Status,
				  RelApps),
	    lists:foldl(fun(#rel{name = RelName}, Acc)->
				check_rel(RelName, RelApps, Acc)
			end,
			Status2,
			Rels);
        false ->
	    reltool_utils:throw_error(
	      "Release ~tp is mandatory (used as boot_rel)",[BootRel])
    end.

check_app(AppTab, {RelName, AppName}, Status) ->
    case ets:lookup(AppTab, AppName) of
	[#app{is_pre_included=IsPreIncl, is_included=IsIncl}]
	  when IsPreIncl; IsIncl ->
	    Status;
	_ ->
	    reltool_utils:throw_error(
	      "Release ~tp uses non included application ~w",[RelName,AppName])
    end.

check_rel(RelName, RelApps, Status) ->
    EnsureApp =
        fun(AppName, Acc) ->
                case lists:member({RelName, AppName}, RelApps) of
                    true ->
                        Acc;
                    false ->
			reltool_utils:throw_error(
			  "Mandatory application ~w is not included in "
			  "release ~tp", [AppName,RelName])
                end
        end,
    Mandatory = [kernel, stdlib],
    lists:foldl(EnsureApp, Status, Mandatory).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

refresh_app(#app{name = AppName,
                 is_escript = IsEscript,
                 active_dir = ActiveDir,
                 label = OptLabel,
                 mods = Mods,
                 status = AppStatus} = App,
            Force,
            Status) ->
    if
        Force; OptLabel =:= undefined ->
            {AppInfo, EbinMods, Status3} =
                case IsEscript of
                    false ->

                        %% Add info from .app file
                        Base = get_base(AppName, ActiveDir),
                        DefaultVsn = get_vsn_from_dir(AppName,Base),
                        Ebin = filename:join([ActiveDir, "ebin"]),
                        AppFile =
			    filename:join([Ebin,
					   atom_to_list(AppName) ++ ".app"]),
                        {AI, Status2} =
			    read_app_info(AppFile,
					  AppFile,
					  AppName,
                                          ActiveDir,
                                          AppStatus,
					  DefaultVsn,
					  Status),

			%% And read all modules from ebin and create
			%% #mod record with dependencies (uses_mods).
                        {AI, read_ebin_mods(Ebin, AppName), Status2};
                    _ ->
                        {App#app.info, Mods, Status}
                end,

            %% Add non-existing modules - i.e. create default #mod
            %% records for all modules that are listed in .app file
            %% but do not exist in ebin.
	    AppInfoMods = lists:usort(AppInfo#app_info.modules),
	    Status4 =
		case AppInfo#app_info.modules -- AppInfoMods of
		    [] ->
			Status3;
		    DuplicatedMods  ->
			lists:foldl(
			  fun(M,S) ->
				  reltool_utils:add_warning(
				    "Module ~w duplicated in app file for "
				    "application ~w.", [M, AppName], S)
			  end,
			  Status3,
			  DuplicatedMods)
		end,
	    AppModNames =
		case AppInfo#app_info.mod of
		    {StartModName, _} ->
			case lists:member(StartModName, AppInfoMods) of
			    true  -> AppInfoMods;
			    false -> [StartModName | AppInfoMods]
			end;
		    undefined ->
			AppInfoMods
		end,
	    MissingMods = add_missing_mods(AppName, EbinMods, AppModNames),

            %% Add optional user config for each module.
	    %% The #mod records that are already in the #app record at
	    %% this point do only contain user defined configuration
	    %% (set by parse_options/4). So here we merge with the
	    %% default records from above.
            Mods2 = add_mod_config(MissingMods ++ EbinMods, Mods),

            %% Set app flag for each module in app file, i.e. the flag
            %% which indicates if the module is listed in the .app
            %% file or not. The start module also get the flag set to true.
            Mods3 = set_mod_flags(Mods2, AppModNames),

	    %% Finally, set label and update the #app record
            AppVsn = AppInfo#app_info.vsn,
            AppLabel =
                case AppVsn of
                    "" -> atom_to_list(AppName);
                    _  -> atom_to_list(AppName) ++ "-" ++ AppVsn
                end,
            App2 = App#app{vsn = AppVsn,
                           label = AppLabel,
                           info = AppInfo,
                           mods = lists:keysort(#mod.name, Mods3)},
            {App2, Status4};
        true ->
            {App, Status}
    end.

missing_app_info(Vsn) ->
    #app_info{vsn = Vsn}.

read_app_info(_AppFileOrBin, _AppFile, erts, _ActiveDir, _AppStatus, DefaultVsn, Status) ->
    {missing_app_info(DefaultVsn), Status};
read_app_info(_AppFileOrBin, _AppFile, _AppName, undefined, missing, DefaultVsn, Status) ->
    {missing_app_info(DefaultVsn), Status};
read_app_info(AppFileOrBin, AppFile, AppName, _ActiveDir, _AppStatus, DefaultVsn, Status) ->
    EnoentText = file:format_error(enoent),
    case reltool_utils:prim_consult(AppFileOrBin) of
        {ok,  [{application, AppName, Info}]} ->
            AI = #app_info{vsn = DefaultVsn},
            parse_app_info(AppFile, Info, AI, Status);
        {ok, _BadApp} ->
            {missing_app_info(DefaultVsn),
	     reltool_utils:add_warning("~w: Illegal contents in app file ~tp, "
				       "application tuple with arity 3 expected.",
				       [AppName,AppFile],
				       Status)};
        {error, Text} when Text =:= EnoentText ->
	    {missing_app_info(DefaultVsn),
             reltool_utils:add_warning("~w: Missing app file ~tp.",
                                       [AppName,AppFile],
                                       Status)};
        {error, Text} ->
            {missing_app_info(DefaultVsn),
	     reltool_utils:add_warning("~w: Cannot parse app file ~tp (~tp).",
				       [AppName,AppFile,Text],
				       Status)}
    end.

parse_app_info(File, [{Key, Val} | KeyVals], AI, Status) ->
    case Key of
        description ->
	    parse_app_info(File, KeyVals, AI#app_info{description = Val},
			   Status);
        id ->
	    parse_app_info(File, KeyVals, AI#app_info{id = Val}, Status);
        vsn ->
	    parse_app_info(File, KeyVals, AI#app_info{vsn = Val}, Status);
        modules ->
	    parse_app_info(File, KeyVals, AI#app_info{modules = Val}, Status);
        maxP ->
	    parse_app_info(File, KeyVals, AI#app_info{maxP = Val}, Status);
        maxT ->
	    parse_app_info(File, KeyVals, AI#app_info{maxT = Val}, Status);
        registered ->
	    parse_app_info(File, KeyVals, AI#app_info{registered = Val},
			   Status);
        included_applications ->
	    parse_app_info(File, KeyVals, AI#app_info{incl_apps = Val}, Status);
        applications ->
	    parse_app_info(File, KeyVals, AI#app_info{applications = Val},
			   Status);
        env ->
	    parse_app_info(File, KeyVals, AI#app_info{env = Val}, Status);
        mod ->
	    parse_app_info(File, KeyVals, AI#app_info{mod = Val}, Status);
        start_phases ->
	    parse_app_info(File, KeyVals, AI#app_info{start_phases = Val},
			   Status);
	runtime_dependencies ->
	    parse_app_info(File, KeyVals, AI#app_info{runtime_dependencies = Val},
			   Status);
        _ ->
	    Status2 =
		reltool_utils:add_warning("Unexpected item ~p in app file ~tp.",
					  [Key,File],
					  Status),
	    parse_app_info(File, KeyVals, AI, Status2)
    end;
parse_app_info(_, [], AI, Status) ->
    {AI, Status}.

read_ebin_mods(Ebin, AppName) ->
    case erl_prim_loader:list_dir(Ebin) of
        {ok, Files} ->
            Ext = code:objfile_extension(),
            InitMod = fun(F) ->
                              File = filename:join([Ebin, F]),
                              init_mod(AppName, File, File, Ext)
                      end,
            Files2 = [F || F <- Files, filename:extension(F) =:= Ext],
            pmap(InitMod, Files2);
        error ->
            []
    end.

pmap(Fun, List) ->
    lists:map(Fun, List).
    %% N = erlang:system_info(schedulers) * 2,
    %% pmap(Fun, List, 0, N, 0, [], []).

%% -record(pmap_res, {count, ref, res}).
%% -record(pmap_wait, {count, ref, pid}).
%%
%% pmap(Fun, [H | T], N, Max, Count, WaitFor, Results) when N < Max ->
%%     Ref = make_ref(),
%%     Parent = self(),
%%     Count2 = Count + 1,
%%     Pid = spawn_link(fun() -> Parent ! #pmap_res{count = Count2, ref = Ref, res = Fun(H)}, unlink(Parent) end),
%%     PW = #pmap_wait{count = Count2, pid = Pid, ref = Ref},
%%     pmap(Fun, T, N + 1, Max, Count2, [PW | WaitFor], Results);
%% pmap(_Fun, [], _N, _Max, _Count, [], Results) ->
%%     %% Sort results and return them in the same orderas the original list
%%     [PR#pmap_res.res || PR <- lists:keysort(#pmap_res.count, Results)];
%% pmap(Fun, List, N, Max, Count, WaitFor, Results) ->
%%     receive
%%      #pmap_res{ref = Ref} = PR ->
%%          WaitFor2 = lists:keydelete(Ref, #pmap_wait.ref, WaitFor),
%%          pmap(Fun, List, N - 1, Max, Count, WaitFor2, [PR | Results]);
%%      {'EXIT', Reason} ->
%%          exit(Reason)
%%     end.

init_mod(AppName, File, FileOrBin, Ext) ->
    UsesMods = xref_mod(FileOrBin),
    Base = filename:basename(File, Ext),
    ModName = list_to_atom(Base),
    #mod{name = ModName,
         app_name = AppName,
         incl_cond = undefined,
         is_ebin_mod = true,
         uses_mods = UsesMods,
         exists = true}.

xref_mod({Base, Bin}) when is_binary(Bin) ->
    Dir = filename:absname("reltool_server.tmp"),
    ok = reltool_utils:recursive_delete(Dir),
    ok = file:make_dir(Dir),
    File = filename:join([Dir, Base]),
    ok = file:write_file(File, Bin),
    Res = xref_mod(File),
    ok = reltool_utils:recursive_delete(Dir),
    Res;
xref_mod(File) when is_list(File) ->
    {ok, Pid} = xref:start([{xref_mode, modules}]),
    link(Pid),
    ok = xref:set_default(Pid, [{verbose,false}, {warnings, false}]),
    ok = xref:set_library_path(Pid, []),
    {ok, _} = xref:add_module(Pid, File, []),
    {ok, UnknownMods} = xref:q(Pid, "UM", []),
    %% {ok, ExportedFuns} = xref:q(Pid, "X", []),
    %% io:format("Unres: ~p\n", [xref:variables(Pid, [predefined])]),
    %% io:format("Q: ~p\n", [xref:q(Pid, "XU", [])]),
    Ref = erlang:monitor(process, Pid),
    unlink(Pid),
    xref:stop(Pid),
    wait_for_processto_die(Ref, Pid, File),
    UnknownMods.

wait_for_processto_die(Ref, Pid, File) ->
    receive
	{'DOWN', Ref, _Type, _Object, _Info} ->
	    ok
    after timer:seconds(30) ->
	    error_logger:error_msg("~w(~w): Waiting for process ~w to die ~tp\n",
				   [?MODULE, ?LINE, Pid, File]),
	    wait_for_processto_die(Ref, Pid, File)
    end.

add_missing_mods(AppName, EbinMods, AppModNames) ->
    EbinModNames = [M#mod.name || M <- EbinMods],
    MissingModNames = AppModNames -- EbinModNames,
    [missing_mod(ModName, AppName) || ModName <- MissingModNames].

missing_mod(ModName, AppName) ->
    %% io:format("Missing: ~w -> ~w\n", [AppName, ModName]),
    #mod{name = ModName,
         app_name = AppName,
         incl_cond = undefined,
         is_ebin_mod = false,
         exists = false,
         status = missing,
         uses_mods = []}.

add_mod_config(Mods, ModConfigs) ->
    AddConfig =
        fun(Config, Acc) ->
                case lists:keyfind(Config#mod.name, #mod.name, Mods) of
                    #mod{} = M ->
                        M2 = M#mod{incl_cond = Config#mod.incl_cond},
                        lists:keystore(Config#mod.name, #mod.name, Acc, M2);
                    false ->
                        Config2 = Config#mod{uses_mods = [], exists = false},
                        [Config2 | Acc]
                end
        end,
    lists:foldl(AddConfig, Mods, ModConfigs).

set_mod_flags(Mods, AppModNames) ->
    SetFlags =
        fun(#mod{name = N} = M) ->
                M#mod{is_app_mod = lists:member(N, AppModNames)}
        end,
    lists:map(SetFlags, Mods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_get_config(S, InclDef, InclDeriv) ->
    AppTab = S#state.app_tab,
    Sys =
        case InclDeriv of
            false ->
		%% Only the apps that exist in #sys.apps shall be
		%% included,and they shall be minimized
		Apps = [shrink_app(App) ||
			   #app{name=Name} <- (S#state.sys)#sys.apps,
			   App <- ets:lookup(AppTab,Name)],
		(S#state.sys)#sys{apps=Apps};
            true  ->
		sys_all_apps(S)
        end,
    reltool_target:gen_config(Sys, InclDef).

shrink_app(A) ->
    Mods = [M#mod{is_app_mod = undefined,
                  is_ebin_mod = undefined,
                  uses_mods = undefined,
                  exists = false} ||
               M <- A#app.mods,
               M#mod.incl_cond =/= undefined],
    if
	A#app.is_escript ->
	    A#app{vsn = undefined,
		  label = undefined,
		  info = undefined,
		  mods = [],
		  uses_mods = undefined};
	true ->
            {Dir, Dirs, OptVsn} =
                case A#app.use_selected_vsn of
                    undefined ->
			{undefined, [], undefined};
                    vsn ->
			{undefined, [], A#app.vsn};
                    dir ->
			{A#app.active_dir, [A#app.active_dir], undefined}
                end,
            A#app{active_dir = Dir,
		  sorted_dirs = Dirs,
		  vsn = OptVsn,
		  label = undefined,
		  info = undefined,
		  mods = Mods,
		  uses_mods = undefined}
    end.


do_save_config(S, Filename, InclDef, InclDeriv) ->
    {ok, Config} = do_get_config(S, InclDef, InclDeriv),
    IoList = io_lib:format("%% config generated at ~w ~w\n~p.\n\n",
                           [date(), time(), Config]),
    file:write_file(Filename, IoList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_load_config(S, SysConfig) ->
    S#state{sys = read_config(default_sys(), SysConfig)}.

read_config(OldSys, Filename) when is_list(Filename) ->
    case file:consult(Filename) of
        {ok, [SysConfig | _]} ->
            read_config(OldSys, SysConfig);
        {ok, Content} ->
	    reltool_utils:throw_error("Illegal file content: ~p",[Content]);
        {error, Reason} ->
	    reltool_utils:throw_error("Illegal config file ~tp: ~ts",
				      [Filename,file:format_error(Reason)])
    end;
read_config(OldSys, {sys, KeyVals}) ->
    NewSys = decode(OldSys#sys{apps = [], rels = []}, KeyVals),
    Apps = [A#app{mods = lists:sort(A#app.mods)} || A <- NewSys#sys.apps],
    Rels =
	case NewSys#sys.rels of
	    []   -> reltool_utils:default_rels();
	    Rs -> Rs
	end,
    NewSys2 = NewSys#sys{apps = lists:sort(Apps),
			 rels = lists:sort(Rels)},
    case lists:keymember(NewSys2#sys.boot_rel, #rel.name, NewSys2#sys.rels) of
	true ->
	    NewSys2;
	false ->
	    reltool_utils:throw_error(
	      "Release ~tp is mandatory (used as boot_rel)",
	      [NewSys2#sys.boot_rel])
    end;
read_config(_OldSys, BadConfig) ->
    reltool_utils:throw_error("Illegal content: ~p", [BadConfig]).

decode(#sys{apps = Apps} = Sys, [{erts = Name, AppKeyVals} | SysKeyVals])
  when is_atom(Name), is_list(AppKeyVals) ->
    App = default_app(Name),
    App2= decode(App, AppKeyVals),
    decode(Sys#sys{apps = [App2 | Apps]}, SysKeyVals);
decode(#sys{apps = Apps} = Sys, [{app, Name, AppKeyVals} | SysKeyVals])
  when is_atom(Name), is_list(AppKeyVals) ->
    App = default_app(Name),
    App2 = decode(App, AppKeyVals),
    decode(Sys#sys{apps = [App2 | Apps]}, SysKeyVals);
decode(#sys{apps = Apps, escripts = Escripts} = Sys,
       [{escript, File0, AppKeyVals} | SysKeyVals])
  when is_list(File0), is_list(AppKeyVals) ->
    File = filename:absname(File0),
    App = default_escript_app(File),
    App2 = decode(App, AppKeyVals),
    decode(Sys#sys{apps = [App2 | Apps], escripts = [File | Escripts]},
	   SysKeyVals);
decode(#sys{rels = Rels} = Sys, [{rel, Name, Vsn, RelApps} | SysKeyVals])
  when is_list(Name), is_list(Vsn), is_list(RelApps) ->
    Rel = #rel{name = Name, vsn = Vsn, rel_apps = []},
    Rel2 = decode(Rel, RelApps),
    decode(Sys#sys{rels = [Rel2 | Rels]}, SysKeyVals);
decode(#sys{} = Sys, [{Key, Val} | KeyVals]) ->
    Sys3 =
        case Key of
            root_dir when is_list(Val) ->
                Sys#sys{root_dir = Val};
            lib_dirs when is_list(Val) ->
                Sys#sys{lib_dirs = Val};
            mod_cond when Val =:= all;
			  Val =:= app;
                          Val =:= ebin;
			  Val =:= derived;
                          Val =:= none ->
                Sys#sys{mod_cond = Val};
            incl_cond when Val =:= include;
			   Val =:= exclude;
                           Val =:= derived ->
                Sys#sys{incl_cond = Val};
            boot_rel when is_list(Val) ->
                Sys#sys{boot_rel = Val};
            emu_name when is_list(Val) ->
                Sys#sys{emu_name = Val};
	    profile when Val =:= development;
			 Val =:= embedded;
			 Val =:= standalone ->
		InclSys = reltool_utils:choose_default(incl_sys_filters, Val, false),
		ExclSys = reltool_utils:choose_default(excl_sys_filters, Val, false),
		InclApp = reltool_utils:choose_default(incl_app_filters, Val, false),
		ExclApp = reltool_utils:choose_default(excl_app_filters, Val, false),
		AppType = reltool_utils:choose_default(embedded_app_type, Val, false),
		Sys#sys{profile = Val,
			incl_sys_filters = dec_re(incl_sys_filters,
						  InclSys,
						  Sys#sys.incl_sys_filters),
			excl_sys_filters = dec_re(excl_sys_filters,
						  ExclSys,
						  Sys#sys.excl_sys_filters),
			incl_app_filters = dec_re(incl_app_filters,
						  InclApp,
						  Sys#sys.incl_app_filters),
			excl_app_filters = dec_re(excl_app_filters,
						  ExclApp,
						  Sys#sys.excl_app_filters),
			embedded_app_type = AppType};
	    excl_lib when Val =:= otp_root ->
		Sys#sys{excl_lib=Val};
            incl_sys_filters ->
                Sys#sys{incl_sys_filters =
			    dec_re(Key, Val, Sys#sys.incl_sys_filters)};
            excl_sys_filters ->
                Sys#sys{excl_sys_filters =
			    dec_re(Key, Val, Sys#sys.excl_sys_filters)};
            incl_app_filters ->
                Sys#sys{incl_app_filters =
			    dec_re(Key, Val, Sys#sys.incl_app_filters)};
            excl_app_filters ->
                Sys#sys{excl_app_filters =
			    dec_re(Key, Val, Sys#sys.excl_app_filters)};
            incl_archive_filters ->
                Sys#sys{incl_archive_filters =
			    dec_re(Key, Val, Sys#sys.incl_archive_filters)};
            excl_archive_filters ->
                Sys#sys{excl_archive_filters =
			    dec_re(Key, Val, Sys#sys.excl_archive_filters)};
            archive_opts when is_list(Val) ->
                Sys#sys{archive_opts = Val};
            relocatable when Val =:= true; Val =:= false ->
                Sys#sys{relocatable = Val};
            rel_app_type when Val =:= permanent;
			      Val =:= transient;
			      Val =:= temporary;
			      Val =:= load;
			      Val =:= none ->
                Sys#sys{rel_app_type = Val};
	    embedded_app_type when Val =:= permanent;
				   Val =:= transient;
				   Val =:= temporary;
				   Val =:= load;
				   Val =:= none;
				   Val =:= undefined ->
                Sys#sys{embedded_app_type = Val};
            app_file when Val =:= keep; Val =:= strip; Val =:= all ->
                Sys#sys{app_file = Val};
            debug_info when Val =:= keep; Val =:= strip ->
                Sys#sys{debug_info = Val};
            _ ->
		reltool_utils:throw_error("Illegal option: ~p", [{Key, Val}])
        end,
    decode(Sys3, KeyVals);
decode(#app{} = App, [{Key, Val} | KeyVals]) ->
    App2 =
        case Key of
            mod_cond when Val =:= all;
			  Val =:= app;
			  Val =:= ebin;
			  Val =:= derived;
			  Val =:= none ->
                App#app{mod_cond = Val};
            incl_cond when Val =:= include;
			   Val =:= exclude;
			   Val =:= derived ->
                App#app{incl_cond = Val};

            debug_info when Val =:= keep;
			    Val =:= strip ->
                App#app{debug_info = Val};
            app_file when Val =:= keep;
			  Val =:= strip;
			  Val =:= all ->
                App#app{app_file = Val};
            app_type when Val =:= permanent;
			  Val =:= transient;
			  Val =:= temporary;
                          Val =:= load;
			  Val =:= none;
			  Val =:= undefined ->
                App#app{app_type = Val};
            incl_app_filters ->
                App#app{incl_app_filters =
			    dec_re(Key, Val, App#app.incl_app_filters)};
            excl_app_filters ->
                App#app{excl_app_filters =
			    dec_re(Key, Val, App#app.excl_app_filters)};
            incl_archive_filters ->
                App#app{incl_archive_filters =
			    dec_re(Key, Val, App#app.incl_archive_filters)};
            excl_archive_filters ->
                App#app{excl_archive_filters =
			    dec_re(Key, Val, App#app.excl_archive_filters)};
            archive_opts when is_list(Val) ->
                App#app{archive_opts = Val};
            vsn when is_list(Val), App#app.use_selected_vsn=:=undefined ->
		App#app{use_selected_vsn = vsn, vsn = Val};
	    lib_dir when is_list(Val), App#app.use_selected_vsn=:=undefined ->
		case filelib:is_dir(Val) of
		    true ->
			Dir = reltool_utils:normalize_dir(Val),
			App#app{use_selected_vsn = dir,
				active_dir = Dir,
				sorted_dirs = [Dir]};
		    false ->
			reltool_utils:throw_error("Illegal lib dir for ~w: ~p",
						  [App#app.name, Val])
		end;
	    SelectVsn when SelectVsn=:=vsn; SelectVsn=:=lib_dir ->
		reltool_utils:throw_error("Mutual exclusive options "
					  "'vsn' and 'lib_dir'",[]);
            _ ->
		reltool_utils:throw_error("Illegal option: ~p", [{Key, Val}])
        end,
    decode(App2, KeyVals);
decode(#app{mods = Mods} = App, [{mod, Name, ModKeyVals} | AppKeyVals]) ->
    Mod = decode(#mod{name = Name}, ModKeyVals),
    decode(App#app{mods = [Mod | Mods]}, AppKeyVals);
decode(#mod{} = Mod, [{Key, Val} | KeyVals]) ->
    Mod2 =
        case Key of
            incl_cond when Val =:= include; Val =:= exclude; Val =:= derived ->
                Mod#mod{incl_cond = Val};
            debug_info when Val =:= keep; Val =:= strip ->
                Mod#mod{debug_info = Val};
            _ ->
		reltool_utils:throw_error("Illegal option: ~p", [{Key, Val}])
        end,
    decode(Mod2, KeyVals);
decode(#rel{rel_apps = RelApps} = Rel, [RelApp | KeyVals]) ->
    {ValidTypesAssigned, RA} =
        case RelApp of
            Name when is_atom(Name) ->
                {true, #rel_app{name = Name}};
            {Name, InclApps} when is_atom(Name), is_list(InclApps) ->
		VI = lists:all(fun erlang:is_atom/1, InclApps),
                {VI, #rel_app{name = Name, incl_apps = InclApps}};
            {Name, Type} when is_atom(Name) ->
                {is_type(Type), #rel_app{name = Name, app_type = Type}};
            {Name, Type, InclApps} when is_atom(Name), is_list(InclApps) ->
		VT = is_type(Type),
		VI = lists:all(fun erlang:is_atom/1, InclApps),
		{VT andalso VI,
                 #rel_app{name = Name, app_type = Type, incl_apps = InclApps}};
            _ ->
                {false, #rel_app{}}
        end,
    case ValidTypesAssigned of
	true ->
            decode(Rel#rel{rel_apps = RelApps ++ [RA]}, KeyVals);
        false ->
	    reltool_utils:throw_error("Illegal option: ~p", [RelApp])
    end;
decode(Acc, []) ->
    Acc;
decode(_Acc, KeyVal) ->
    reltool_utils:throw_error("Illegal option: ~p", [KeyVal]).

is_type(Type) ->
    case Type of
        undefined -> true;
        permanent -> true;
        transient -> true;
        temporary -> true;
        load      -> true;
        none      -> true;
        _         -> false
    end.

split_escript_name(File) when is_list(File) ->
    Label = filename:basename(File, ".escript"),
    {list_to_atom("*escript* " ++ Label), Label}.

default_escript_app(File) ->
    {Name, Label} = split_escript_name(File),
    App = default_app(Name, File),
    App#app{is_escript = true,
	    label = Label,
	    info = missing_app_info("")}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Apps is a list of #app records - sorted on #app.name - containing
%% only the apps that have specific configuration (e.g. in the config
%% file)
refresh(#state{sys=Sys} = S) ->
    RootDir = filename:absname(Sys#sys.root_dir),
    LibDirs = [filename:absname(D) || D <- Sys#sys.lib_dirs],
    Escripts = [filename:absname(E) || E <- Sys#sys.escripts],

    %% Read all lib dirs and return sorted [{AppName,Dir}]
    SourceDirs = libs_to_dirs(RootDir, LibDirs),

    %% Create #app records for all apps in SourceDirs, and merge with
    %% list of apps from config.
    MergedApps = merge_app_dirs(SourceDirs, Sys#sys.apps),

    %% For each escript, find all related files and convert to #app
    %% and #mod records
    {AllApps, Status2} = escripts_to_apps(Escripts, MergedApps, {ok,[]}),

    %% Make sure correct version of each application is used according
    %% to the user configuration.
    %% Then find all modules and their dependencies and set user
    %% configuration per module if it exists.
    {RefreshedApps, Status3} = refresh_apps(Sys#sys.apps, AllApps, [],
					    true, Status2),

    %% Make sure erts exists in app list and has a version (or warn)
    {PatchedApps, Status4} = patch_erts_version(RootDir, RefreshedApps, Status3),

    %% Update #sys and return
    Escripts2 = [A#app.active_dir || A <- PatchedApps, A#app.is_escript],
    Sys2 = Sys#sys{root_dir = RootDir,
		   lib_dirs = LibDirs,
		   escripts = Escripts2},
    {S#state{sys=Sys2}, PatchedApps, Status4}.

patch_erts_version(RootDir, Apps, Status) ->
    AppName = erts,
    case lists:keyfind(AppName, #app.name, Apps) of
        #app{vsn = Vsn} = Erts ->
            LocalRoot = code:root_dir(),
            if
                LocalRoot =:= RootDir, Vsn =:= "" ->
                    Vsn2 = erlang:system_info(version),
                    Erts2 = Erts#app{vsn = Vsn2, label = "erts-" ++ Vsn2},
                    Apps2 = lists:keystore(AppName, #app.name, Apps, Erts2),
                    {Apps2, Status};
                Vsn =:= "" ->
                    {Apps, reltool_utils:add_warning("erts has no version",[],
						     Status)};
                true ->
                    {Apps, Status}
            end;
        false ->
	    reltool_utils:throw_error(
	      "erts cannot be found in the root directory ~tp", [RootDir])
    end.

libs_to_dirs(RootDir, LibDirs) ->
    case file:list_dir(RootDir) of
        {ok, RootFiles} ->
            RootLibDir = filename:join([RootDir, "lib"]),
            AllLibDirs = [RootLibDir | LibDirs],
            case AllLibDirs -- lists:usort(AllLibDirs) of
                [] ->
                    Fun = fun(Base) ->
                                  AppDir = filename:join([RootLibDir, Base]),
                                  case filelib:is_dir(filename:join([AppDir,
								     "ebin"]),
						      erl_prim_loader) of
                                      true ->
                                          AppDir;
                                      false ->
                                          filename:join([RootDir,
							 Base,
							 "preloaded"])
                                  end
                          end,
                    ErtsFiles = [{erts, Fun(F)} || F <- RootFiles,
						   lists:prefix("erts", F)],
                    app_dirs2(AllLibDirs, [ErtsFiles]);
                [Duplicate | _] ->
		    reltool_utils:throw_error("Duplicate library: ~tp",[Duplicate])
            end;
        {error, Reason} ->
	    reltool_utils:throw_error("Missing root library ~tp: ~ts",
				      [RootDir,file:format_error(Reason)])
    end.

app_dirs2([Lib | Libs], Acc) ->
    case file:list_dir(Lib) of
        {ok, Files} ->
            Filter =
                fun(Base) ->
                        AppDir = filename:join([Lib, Base]),
                        EbinDir = filename:join([AppDir, "ebin"]),
                        case filelib:is_dir(EbinDir, erl_prim_loader) of
                            true ->
				Name = find_app_name(Base,EbinDir),
                                case Name of
                                    erts -> false;
                                    _    -> {true, {Name, AppDir}}
                                end;
                            false ->
                                false
                        end
                end,
            Files2 = lists:zf(Filter, Files),
            app_dirs2(Libs, [Files2 | Acc]);
        {error, Reason} ->
	    reltool_utils:throw_error("Illegal library ~tp: ~ts",
				      [Lib, file:format_error(Reason)])
    end;
app_dirs2([], Acc) ->
    lists:sort(lists:append(Acc)).

find_app_name(Base,EbinDir) ->
    {ok,EbinFiles} = erl_prim_loader:list_dir(EbinDir),
    AppFile =
	case [F || F <- EbinFiles, filename:extension(F)=:=".app"] of
	    [AF] ->
		AF;
	    _ ->
		undefined
	end,
    find_app_name1(Base,AppFile).

find_app_name1(Base,undefined) ->
    {Name,_} = reltool_utils:split_app_name(Base),
    Name;
find_app_name1(_Base,AppFile) ->
    list_to_atom(filename:rootname(AppFile)).

get_vsn_from_dir(AppName,Base) ->
    Prefix = atom_to_list(AppName) ++ "-",
    case lists:prefix(Prefix,Base) of
	true ->
	    lists:nthtail(length(Prefix),Base);
	false ->
	    ""
    end.


escripts_to_apps([Escript | Escripts], Apps, Status) ->
    {EscriptAppName, _Label} = split_escript_name(Escript),
    Ext = code:objfile_extension(),

    %% First find all .app files and associate the app name to the app
    %% label - this is in order to now which application a module
    %% belongs to in the next round.
    AppFun = fun(FullName, _GetInfo, _GetBin, AppFiles) ->
		     Components = filename:split(FullName),
		     case Components of
			 [AppLabel, "ebin", File] ->
			     case filename:extension(File) of
				 ".app" ->
				     [{AppLabel,File}|AppFiles];
				 _ ->
				     AppFiles
			     end;
			 _ ->
			     AppFiles
		     end
	     end,
    AppFiles =
	case reltool_utils:escript_foldl(AppFun, [], Escript) of
	    {ok, AF} ->
		AF;
	    {error, Reason1} ->
		reltool_utils:throw_error("Illegal escript ~tp: ~p",
					  [Escript,Reason1])
	end,

    %% Next, traverse all files...
    Fun = fun(FullName, _GetInfo, GetBin, {FileAcc, StatusAcc}) ->
                  Components = filename:split(FullName),
                  case Components of
                      [AppLabel, "ebin", File] ->
                          case filename:extension(File) of
                              ".app" ->
				  AppName =
				      list_to_atom(filename:rootname(File)),
                                  DefaultVsn =
				      get_vsn_from_dir(AppName,AppLabel),
                                  AppFileName =
				      filename:join([Escript, FullName]),
                                  Dir = filename:join([Escript, AppName]),
                                  {Info, StatusAcc2} =
                                      read_app_info(GetBin(),
						    AppFileName,
						    AppName,
                                                    Dir,
                                                    ok,
						    DefaultVsn,
						    Status),
                                  {[{AppName, app, Dir, Info} | FileAcc],
				   StatusAcc2};
                              E when E =:= Ext ->
				  AppFile =
				      proplists:get_value(AppLabel,AppFiles),
				  AppName = find_app_name1(AppLabel,AppFile),
                                  Mod = init_mod(AppName,
						 File,
						 {File, GetBin()},
						 Ext),
                                  Dir = filename:join([Escript, AppName]),
                                  {[{AppName, mod, Dir, Mod} | FileAcc],
				   StatusAcc};
                              _ ->
                                  {FileAcc, StatusAcc}
                          end;
                      ["."] ->
                          Bin = GetBin(),
                          {ok, {ModName, _}} = beam_lib:version(Bin),
                          ModStr = atom_to_list(ModName) ++ Ext,
                          Mod = init_mod(EscriptAppName,
					 ModStr,
					 {ModStr, GetBin()},
					 Ext),
                          {[{EscriptAppName, mod, Escript, Mod} | FileAcc],
			   StatusAcc};
                      [File] ->
                          case filename:extension(File) of
                              E when E =:= Ext ->
                                  Mod = init_mod(EscriptAppName,
						 File,
						 {File, GetBin()},
						 Ext),
                                  {[{EscriptAppName, mod, File, Mod} | FileAcc],
				   StatusAcc};
                              _ ->
                                  {FileAcc, StatusAcc}
                          end;
                      _ ->
                          {FileAcc, StatusAcc}
                  end
          end,

    case reltool_utils:escript_foldl(Fun, {[], Status}, Escript) of
	{ok, {Files, Status2}} ->
	    EscriptApp =
		case lists:keyfind(EscriptAppName,#app.name,Apps) of
		    false -> default_escript_app(Escript);
		    EA    -> EA
		end,
	    {Apps2, Status3} =
		escript_files_to_apps(EscriptAppName,
				      lists:sort(Files),
				      [EscriptApp],
				      Apps,
				      Status2),
	    escripts_to_apps(Escripts, Apps2, Status3);
	{error, Reason2} ->
	    reltool_utils:throw_error("Illegal escript ~tp: ~p",
				      [Escript,Reason2])
    end;
escripts_to_apps([], Apps, Status) ->
    {Apps, Status}.

%% Assume that all files for an app are in consecutive order
%% Assume the app info is before the mods
escript_files_to_apps(EscriptAppName,
		      [{AppName, Type, Dir, ModOrInfo} | Files],
		      Acc,
		      Apps,
		      Status) ->
    {NewAcc,Status3} =
	case Type of
	    mod ->
		case Acc of
		    [App | Acc2] when App#app.name =:= ModOrInfo#mod.app_name ->
			Mods = lists:ukeymerge(#mod.name,
					       [ModOrInfo],
					       App#app.mods),
			{[App#app{mods = Mods} | Acc2], Status};
		    Acc ->
			{NewApp, Status2} = init_escript_app(AppName,
							     EscriptAppName,
							     Dir,
							     missing_app_info(""),
							     [ModOrInfo],
							     Apps,
							     Status),
			{[NewApp | Acc], Status2}
		end;
           app ->
		{App, Status2} = init_escript_app(AppName,
						  EscriptAppName,
						  Dir,
						  ModOrInfo,
						  [],
						  Apps,
						  Status),
		{[App | Acc], Status2}
	end,
    escript_files_to_apps(EscriptAppName, Files, NewAcc, Apps, Status3);
escript_files_to_apps(_EscriptAppName, [], Acc, Apps, Status) ->
    {lists:ukeymerge(#app.name, lists:reverse(Acc), Apps), Status}.

init_escript_app(AppName, EscriptAppName, Dir, Info, Mods, Apps, Status) ->
    App1 = default_app(AppName, Dir),
    IsEscript =
	if AppName=:=EscriptAppName -> true;
	   true -> {inlined, EscriptAppName}
	end,
    InclCond = (lists:keyfind(EscriptAppName,#app.name,Apps))#app.incl_cond,
    App2 = App1#app{is_escript = IsEscript,
		    label = filename:basename(Dir, ".escript"),
		    info = Info,
		    mods = Mods,
		    active_dir = Dir,
		    sorted_dirs = [Dir],
		    incl_cond = InclCond},% inlined apps inherit incl from escript
    case lists:keymember(AppName, #app.name, Apps) of
        true ->
	    reltool_utils:throw_error(
	      "~w: Application name clash. Escript ~tp contains application ~tp.",
	      [AppName,Dir,AppName]);
        false ->
            {App2, Status}
    end.

merge_app_dirs([{Name, Dir} | Rest], Apps) ->
    App =
        case lists:keyfind(Name, #app.name, Apps) of
            false ->
		default_app(Name, Dir);
            OldApp ->
		SortedDirs = lists:umerge(fun reltool_utils:app_dir_test/2,
					  [Dir], OldApp#app.sorted_dirs),
                OldApp#app{sorted_dirs = SortedDirs}
        end,
    Apps2 = lists:ukeymerge(#app.name, [App], Apps),
    merge_app_dirs(Rest, Apps2);
merge_app_dirs([], Apps) ->
    set_active_dirs(Apps).

%% First dir, i.e. the one with highest version, is set to active dir,
%% unless a specific dir is given in config
set_active_dirs([#app{use_selected_vsn = dir} = App | Apps]) ->
    [App | set_active_dirs(Apps)];
set_active_dirs([#app{sorted_dirs = [ActiveDir|_]} = App | Apps]) ->
    [App#app{active_dir = ActiveDir} | set_active_dirs(Apps)];
set_active_dirs([#app{sorted_dirs = []} = App | Apps]) ->
    [App#app{active_dir = undefined} | set_active_dirs(Apps)];
set_active_dirs([]) ->
    [].


default_app(Name, Dir) ->
    App = default_app(Name),
    App#app{active_dir = Dir,
            sorted_dirs = [Dir]}.

default_app(Name) ->
    #app{name = Name,
         is_escript = false,
         sorted_dirs = [],
         mods = [],
         status = missing}.



refresh_apps(ConfigApps, [New | NewApps], Acc, Force, Status) ->
    {New2, Status3} =
	case lists:keymember(New#app.name,#app.name,ConfigApps) of
	    true ->
		%% There is user defined config for this application, make
		%% sure that the application exists and that correct
		%% version is used. Set active directory.
		{Info, ActiveDir, Status2} = ensure_app_info(New, Status),
		OptLabel =
		    case Info#app_info.vsn =:= New#app.vsn of
			true -> New#app.label;
			false -> undefined % Cause refresh
		    end,
		refresh_app(New#app{label = OptLabel,
				    active_dir = ActiveDir,
				    vsn = Info#app_info.vsn,
				    info = Info},
			    Force,
			    Status2);
	    false ->
		%% There is no user defined config for this
		%% application. This means that the app is found in the
		%% lib dirs, and that the highest version shall be
		%% used. I.e. the active_dir and vsn are already correct
		%% from merge_app_dirs.
		refresh_app(New, Force, Status)
	end,
    refresh_apps(ConfigApps, NewApps, [New2 | Acc], Force, Status3);
refresh_apps(_ConfigApps, [], Acc, _Force, Status) ->
    {lists:reverse(Acc), Status}.

ensure_app_info(#app{is_escript = IsEscript, active_dir = Dir, info = Info},
		Status)
  when IsEscript=/=false ->
    %% Escript or application which is inlined in an escript
    {Info, Dir, Status};
ensure_app_info(#app{name = Name, sorted_dirs = []} = App, Status) ->
    Reason = "~w: Missing application directory.",
    case App of
        #app{incl_cond = exclude, status = missing, active_dir = Dir} ->
            Status2 = reltool_utils:add_warning(Reason, [Name], Status),
            {missing_app_info(""), Dir, Status2};
        _ ->
            reltool_utils:throw_error(Reason, [Name])
    end;
ensure_app_info(#app{name = Name,
		     vsn = Vsn,
		     use_selected_vsn = UseSelectedVsn,
		     active_dir = ActiveDir,
		     sorted_dirs = Dirs,
                     info = undefined,
                     status = AppStatus},
		Status) ->
    ReadInfo =
        fun(Dir, StatusAcc) ->
                Base = get_base(Name, Dir),
                Ebin = filename:join([Dir, "ebin"]),
                DefaultVsn = get_vsn_from_dir(Name,Base),
                AppFile = filename:join([Ebin, atom_to_list(Name) ++ ".app"]),
                read_app_info(AppFile, AppFile, Name, ActiveDir,
                              AppStatus, DefaultVsn, StatusAcc)
        end,
    {AllInfo, Status2} = lists:mapfoldl(ReadInfo, Status, Dirs),
    AllVsns = [I#app_info.vsn || I <- AllInfo],
    Status3 =
        case AllVsns -- lists:usort(AllVsns) of
            [] ->
                %% No redundant info
                Status2;
            [BadVsn | _] ->
		reltool_utils:throw_error(
		  "~w: Application version clash. "
		  "Multiple directories contain version ~tp.",
		  [Name,BadVsn])
        end,
    FirstInfo = hd(AllInfo),
    FirstDir = hd(Dirs),
    if
        UseSelectedVsn =:= dir ->
	    if ActiveDir =:= FirstDir ->
		    {FirstInfo, FirstDir, Status3};
	       true ->
		    Info = find_dir(ActiveDir, AllInfo, Dirs),
		    {Info, ActiveDir, Status3}
	    end;
        UseSelectedVsn =:= vsn ->
	    if Vsn =:= FirstInfo#app_info.vsn ->
		    {FirstInfo, FirstDir, Status3};
	       true ->
		    case find_vsn(Vsn, AllInfo, Dirs) of
			{Info, VsnDir} ->
			    {Info, VsnDir, Status3};
			false ->
			    reltool_utils:throw_error(
			      "~w: No application directory contains "
			      "selected version ~tp", [Name,Vsn])
		    end
	    end;
	true ->
            {FirstInfo, FirstDir, Status3}
    end;
ensure_app_info(#app{active_dir = Dir, info = Info}, Status) ->
    {Info, Dir, Status}.

find_vsn(Vsn, [#app_info{vsn = Vsn} = Info | _], [Dir | _]) ->
    {Info, Dir};
find_vsn(Vsn, [_ | MoreInfo], [_ | MoreDirs]) ->
    find_vsn(Vsn, MoreInfo, MoreDirs);
find_vsn(_, [], []) ->
    false.

find_dir(Dir, [Info | _], [Dir | _]) ->
    Info;
find_dir(Dir, [_ | MoreInfo], [_ | MoreDirs]) ->
    find_dir(Dir, MoreInfo, MoreDirs).

get_base(Name, Dir) ->
    case Name of
        erts ->
            case filename:basename(Dir) of
                "preloaded" ->
                    filename:basename(filename:dirname(Dir));
                TmpBase ->
                    TmpBase
            end;
        _ ->
            filename:basename(Dir)
    end.

sys_all_apps(#state{app_tab=AppTab, sys=Sys}) ->
    Sys#sys{apps = ets:match_object(AppTab,'_')}.

config_and_refresh(OldS, Fun) ->
    try
	S = Fun(),
	{S2, Apps, Status2} = refresh(S),
	%% Analyse will write to app_tab and mod_tab, so we first
	%% backup these tables and clear them
	Backup = backup(OldS),
	try
	    Status3 = analyse(S2, Apps, Status2),
	    S3 = save_old(OldS, S2, Backup, Status3),
	    {S3, Status3}
	catch throw:{error,_} = Error1 ->
		restore(Backup,OldS),
		throw(Error1)
	end
    catch throw:{error,_} = Error2 ->
	    {OldS, Error2}
    end.


backup(S) ->
    Apps = ets:tab2list(S#state.app_tab),
    Mods = ets:tab2list(S#state.mod_tab),
    ets:delete_all_objects(S#state.app_tab),
    ets:delete_all_objects(S#state.mod_tab),
    ets:delete_all_objects(S#state.mod_used_by_tab), %tmp tab, no backup needed
    {Apps,Mods}.

restore({Apps,Mods}, S) ->
    insert_all(S#state.app_tab,Apps),
    insert_all(S#state.mod_tab,Mods).

save_old(#state{status=OldStatus,sys=OldSys},NewS,{OldApps,OldMods},NewStatus) ->
    ets:delete_all_objects(NewS#state.old_app_tab),
    ets:delete_all_objects(NewS#state.old_mod_tab),
    insert_all(NewS#state.old_app_tab,OldApps),
    insert_all(NewS#state.old_mod_tab,OldMods),
    NewS#state{old_sys=OldSys,
	       old_status=OldStatus,
	       status=NewStatus}.

insert_all(Tab,Items) ->
    lists:foreach(fun(Item) -> ets:insert(Tab,Item) end, Items).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(_Parent, _Debug, S) ->
    ?MODULE:loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S,_Module,_OldVsn,_Extra) ->
    {ok, S}.
