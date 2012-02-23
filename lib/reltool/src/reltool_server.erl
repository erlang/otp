%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
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
         old_status}).

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

init(Options) ->
    try
        do_init(Options)
    catch
        error:Reason ->
            exit({Reason, erlang:get_stacktrace()})
    end.

do_init(Options) ->
    AppTab = ets:new(reltool_apps, [public, ordered_set, {keypos, #app.name}]),
    ModTab = ets:new(reltool_mods, [public, ordered_set, {keypos, #mod.name}]),
    OldAppTab = ets:new(reltool_apps, [public, ordered_set, {keypos, #app.name}]),
    OldModTab = ets:new(reltool_mods, [public, ordered_set, {keypos, #mod.name}]),
    ModUsesTab = ets:new(reltool_mod_uses, [public, bag, {keypos, 1}]),
    InitialC = #common{app_tab = AppTab,
		       mod_tab = ModTab,
		       old_app_tab = OldAppTab,
		       old_mod_tab = OldModTab,
		       mod_used_by_tab = ModUsesTab},

    {S, Status} = parse_options(InitialC, Options),
    %%! Check status before returning ok?

    proc_lib:init_ack(S#state.parent_pid, {ok, self(), S#state.common}),

    %% This will do exit if it fails
    {S2, _Status2} = refresh_and_analyse_no_rollback(S, Status),
    %%! what to do about warnings?
    loop(S2).

parse_options(C, Opts) ->
    Sys = default_sys(),
    C2 = C#common{sys_debug = [],
		  wx_debug = 0,
		  trap_exit = true},
    S = #state{options = Opts},
    parse_options(Opts, S, C2, Sys, {ok, []}).

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

parse_options([{Key, Val} | KeyVals], S, C, Sys, Status) ->
    case Key of
        parent ->
            parse_options(KeyVals, S#state{parent_pid = Val}, C, Sys, Status);
        sys_debug ->
            parse_options(KeyVals, S, C#common{sys_debug = Val}, Sys, Status);
        wx_debug ->
            parse_options(KeyVals, S, C#common{wx_debug = Val}, Sys, Status);
        trap_exit ->
            parse_options(KeyVals, S, C#common{trap_exit = Val}, Sys, Status);
        config ->
            {Sys2, Status2} = read_config(Sys, Val, Status),
            parse_options(KeyVals, S, C, Sys2, Status2);
        sys ->
            {Sys2, Status2} = read_config(Sys, {sys, Val}, Status),
            parse_options(KeyVals, S, C, Sys2, Status2);
        _ ->
            Text = lists:flatten(io_lib:format("~p", [{Key, Val}])),
            Status2 =
		reltool_utils:return_first_error(Status,
						 "Illegal option: " ++ Text),
            parse_options(KeyVals, S, C, Sys, Status2)
    end;
parse_options([], S, C, Sys, Status) ->
    {S#state{common = C, sys = Sys}, Status};
parse_options(KeyVals, S, C, Sys, Status) ->
    Text = lists:flatten(io_lib:format("~p", [KeyVals])),
    Status2 = reltool_utils:return_first_error(Status,
					       "Illegal options: " ++ Text),
    {S#state{common = C, sys = Sys}, Status2}.

loop(#state{common = C, sys = Sys} = S) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg,
				  From,
				  S#state.parent_pid,
				  ?MODULE,
				  C#common.sys_debug,
				  S);
        {call, ReplyTo, Ref, {get_config, InclDef, InclDeriv}} ->
            Reply = do_get_config(S, InclDef, InclDeriv),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {load_config, SysConfig}} ->
	    {S2, Status} = do_load_config(S, SysConfig),
	    {S3, Status2} = refresh_and_analyse(S, S2, Status),
            reltool_utils:reply(ReplyTo, Ref, Status2),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, {save_config, Filename, InclDef, InclDeriv}} ->
            Reply = do_save_config(S, Filename, InclDef, InclDeriv),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, reset_config} ->
	    {S2, Status} = parse_options(C, S#state.options),
	    {S3, Status2} = refresh_and_analyse(S, S2, Status),
            reltool_utils:reply(ReplyTo, Ref, Status2),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, undo_config} ->
	    C2 = C#common{app_tab = C#common.old_app_tab,
			  old_app_tab = C#common.app_tab,
			  mod_tab = C#common.old_mod_tab,
			  old_mod_tab = C#common.mod_tab},
            S2 = S#state{common = C2,
			 sys = S#state.old_sys,
                         old_sys = Sys,
			 status = S#state.old_status,
			 old_status = S#state.status},
            reltool_utils:reply(ReplyTo, Ref, ok),
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, {get_rel, RelName}} ->
            Sys = S#state.sys,
            Reply =
                case lists:keysearch(RelName, #rel.name, Sys#sys.rels) of
                    {value, Rel} ->
                        reltool_target:gen_rel(Rel, sys_all_apps(C,Sys));
                    false ->
                        {error, "No such release: " ++ RelName}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_script, RelName}} ->
            Sys = S#state.sys,
            Reply =
                case lists:keysearch(RelName, #rel.name, Sys#sys.rels) of
                    {value, Rel} ->
                        PathFlag = true,
                        Vars = [],
                        reltool_target:gen_script(Rel, sys_all_apps(C,Sys),
						  PathFlag, Vars);
                    false ->
                        {error, "No such release: " ++ RelName}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_mod, ModName}} ->
            Reply =
                case ets:lookup(C#common.mod_tab, ModName) of
                    [M] ->
                        {ok, M};
                    [] ->
                        {ok, missing_mod(ModName, ?MISSING_APP_NAME)}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {get_app, AppName}} when is_atom(AppName) ->
            Reply =
                case ets:lookup(C#common.app_tab,AppName) of
                    [App] ->
                        {ok, App};
                    [] ->
                        {error, "No such application: " ++
			 atom_to_list(AppName)}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_app, App}} ->
	    {S2, Status} = do_set_apps(S, [App]),
	    {S3, Status2} = refresh_and_analyse(S, S2, Status),
	    Reply =
		case Status2 of
		    {ok, Warnings} ->
			[App2] = ets:lookup(C#common.app_tab,App#app.name),
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
			ets:select(C#common.app_tab,
				   [{#app{is_pre_included=true,_='_'},
				     [],
				     ['$_']}]);
		    blacklist ->
			%% Pre-excluded
			ets:select(C#common.app_tab,
				   [{#app{is_pre_included=false,_='_'},
				     [],
				     ['$_']}]);
		    source ->
			%% Not included and not pre-excluded
			ets:select(C#common.app_tab,
				   [{#app{is_included='$1',
					  is_pre_included='$2',
					  _='_'},
				     [{'=/=','$1',true},
				      {'=/=','$2',false}],
				     ['$_']}]);
                    derived ->
			%% Included, but not pre-included
			ets:select(C#common.app_tab,
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
	    {S2, Status} = do_set_apps(S, Apps),
	    {S3, Status2} = refresh_and_analyse(S, S2, Status),
            reltool_utils:reply(ReplyTo, Ref, Status2),
	    ?MODULE:loop(S3);
        {call, ReplyTo, Ref, get_sys} ->
            reltool_utils:reply(ReplyTo, Ref, {ok, Sys#sys{apps = undefined}}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_sys, Sys2}} ->
	    S2 = S#state{sys =  Sys2#sys{apps = Sys#sys.apps}},
	    {S3, Status} = refresh_and_analyse(S, S2, {ok,[]}),
            reltool_utils:reply(ReplyTo, Ref, Status),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, get_status} ->
            reltool_utils:reply(ReplyTo, Ref, S#state.status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_rel_files, Dir}} ->
            Status =
                case reltool_target:gen_rel_files(sys_all_apps(C,Sys), Dir) of
                    ok ->
                        {ok, []};
                    {error, Reason} ->
                        {error, Reason}
                end,
            reltool_utils:reply(ReplyTo, Ref, Status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_target, Dir}} ->
            Reply = reltool_target:gen_target(sys_all_apps(C,Sys), Dir),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, gen_spec} ->
            Reply = reltool_target:gen_spec(sys_all_apps(C,Sys)),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {'EXIT', Pid, Reason} when Pid =:= S#state.parent_pid ->
            exit(Reason);
        {call, ReplyTo, Ref, Msg} when is_pid(ReplyTo), is_reference(Ref) ->
            error_logger:format("~p~p got unexpected call:\n\t~p\n",
                                [?MODULE, self(), Msg]),
            reltool_utils:reply(ReplyTo, Ref, {error, {invalid_call, Msg}}),
            ?MODULE:loop(S);
        Msg ->
            error_logger:format("~p~p got unexpected message:\n\t~p\n",
                                [?MODULE, self(), Msg]),
            ?MODULE:loop(S)
    end.


 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_set_apps(#state{sys = Sys} = S, ChangedApps) ->
    %% Create new list of configured applications
    {SysApps,Status} = app_update_config(ChangedApps, Sys#sys.apps, {ok,[]}),
    {S#state{sys = Sys#sys{apps = SysApps}}, Status}.

%% Re-create the #sys.apps list by
%% 1) taking configurable fields from the changed #app records and
%%    create new default records
%% 2) removing #app records if no configurable fields are set
%% 3) keeping #app records that are not changed
app_update_config([#app{name=Name,is_escript={inlined,Escript}}|Configs],
		  SysApps,Status) ->
    Text =
	lists:flatten(
	  io_lib:format("Application ~p is inlined in ~p. Can not change "
			"configuration for an inlined application.",
			[Name,Escript])),
    Status2 = reltool_utils:return_first_error(Status, Text),
    app_update_config(Configs,SysApps,Status2);
app_update_config([Config|Configs],SysApps,Status) ->
    NewSysApps =
	case app_set_config_only(Config) of
	    {delete,Name} ->
		lists:keydelete(Name,#app.name,SysApps);
	    New ->
		lists:ukeymerge(#app.name,[New],SysApps)
	end,
    app_update_config(Configs,NewSysApps,Status);
app_update_config([],SysApps,Status) ->
    {SysApps,Status}.

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

    %% Some fields shall only be set if it is an escript, e.g. label
    %% must never be set for any other applications since that will
    %% prevent refreshing.
    if IsEscript ->
	    App#app{is_escript  = IsEscript,
		    active_dir  = ActiveDir,
		    sorted_dirs = SortedDirs,
		    label       = Label,
		    info        = Info};
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

analyse(#state{common=C, sys=Sys}, Apps, Status) ->
    %% Create a list of {RelName,AppName}, one element for each
    %% AppName that needs to be included for the given release.
    {RelApps, Status2} = apps_in_rels(Sys#sys.rels, Apps, Status),

    %% Initiate is_pre_included and is_included for all applications
    %% based on #sys.incl_cond, #app.incl_cond and if the application
    %% is included in a release (rel spec - see apps_in_rels above).
    %% Then initiate the same for each module, and check that there
    %% are no duplicated module names (in different applications)
    %% where we can not decide which one to use.
    %% Write all #app to app_tab and all #mod to mod_tab.
    Status3 = apps_init_is_included(C, Sys, Apps, RelApps, Status2),

    %% For each module that has #mod.is_included==true, propagate
    %% is_included to the modules it uses.
    propagate_is_included(C, Sys),

    %% Insert reverse dependencies - i.e. for each
    %% #mod{name=Mod, uses_mods=[UsedMod]},
    %% insert an entry {UsedMod,Mod} in mod_used_by_tab.
    propagate_is_used_by(C),

    %% Set the above reverse dependencies in #mod records
    %% (used_by_mods) and accumulate in #app records.
    %% Make sure #app.is_included is always true if some
    %% #mod.is_included==true for at least one module in the app.
    %% Set status=missing|ok for #app and #mod - indicates if module
    %% (.beam file) is missing in file system.
    Status4 = app_recap_dependencies(C, Status3),

    %% Check that the boot_rel exists.
    %% Check that all applications that are listed in a 'rel' spec are
    %% also really included in the target release.
    %% Check that all mandatory applications are included in all rels.
    verify_config(C, Sys, RelApps, Status4).

apps_in_rels(Rels, Apps, Status) ->
    {AllRelApps, Status2} =
	lists:foldl(fun(Rel, {RelApps, S}) ->
			    {MoreRelApps, S2} = apps_in_rel(Rel, Apps, S),
			    {MoreRelApps ++ RelApps, S2}
		    end,
		    {[], Status},
		    Rels),
    {lists:reverse(AllRelApps), Status2}.

apps_in_rel(#rel{name = RelName, rel_apps = RelApps}, Apps, Status) ->
    Mandatory = [{RelName, kernel}, {RelName, stdlib}],
    Other = [{RelName, AppName} ||
		RA <- RelApps,
		AppName <- [RA#rel_app.name | RA#rel_app.incl_apps],
		not lists:keymember(AppName, 2, Mandatory)],
    more_apps_in_rels(Mandatory ++ Other, Apps, [], Status).

more_apps_in_rels([{RelName, AppName} = RA | RelApps], Apps, Acc, Status) ->
    case lists:member(RA, Acc) of
	true ->
	    more_apps_in_rels(RelApps, Apps, Acc, Status);
	false ->
	    case lists:keyfind(AppName, #app.name, Apps) of
		#app{info = #app_info{applications = InfoApps}} ->
		    Extra = [{RelName, N} || N <- InfoApps],
		    {Acc2, Status2} =
			more_apps_in_rels(Extra, Apps, [RA | Acc], Status),
		    more_apps_in_rels(RelApps, Apps, Acc2, Status2);
		false ->
		    Text = lists:concat(["Release ", RelName,
					 " uses non existing application ",
					 AppName]),
		    Status2 = reltool_utils:return_first_error(Status, Text),
		    more_apps_in_rels(RelApps, Apps, Acc, Status2)
	    end
    end;
more_apps_in_rels([], _Apps, Acc, Status) ->
    {Acc, Status}.


apps_init_is_included(C, Sys, Apps, RelApps, Status) ->
    lists:foldl(fun(App, AccStatus) ->
			app_init_is_included(C, Sys, App, RelApps, AccStatus)
		end,
		Status,
		Apps).

app_init_is_included(C,
		     Sys,
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
		Text = lists:concat(["Application ", AppName, " is used "
				     "in release ", RelName, " and cannot "
				     "be excluded"]),
		TmpStatus = reltool_utils:return_first_error(Status, Text),
		{undefined, false, false, TmpStatus};
            {derived, []} ->
		{undefined, undefined, undefined, Status};
            {derived, [_ | _]} -> % App is included in at least one rel
		{true, undefined, true, Status}
        end,
    {Mods2,Status3} = lists:mapfoldl(fun(Mod,Acc) ->
					     mod_init_is_included(C,
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
    ets:insert(C#common.app_tab, A2), %%! Set mods to only mod names here????
    Status3.

mod_init_is_included(C, M, ModCond, AppCond, Default, Status) ->
    %% print(M#mod.name, hipe, "incl_cond -> ~p\n", [AppCond]),
    IsIncl =
        case AppCond of
            include ->
                case M#mod.incl_cond of
                    include ->
                        true;
                    exclude ->
                        false;
                    undefined ->
                        %% print(M#mod.name, hipe, "mod_cond -> ~p\n",
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
                    undefined ->
                        Default
                end
        end,

    M2 = M#mod{is_pre_included = IsIncl, is_included = IsIncl},

    Status2 =
	case ets:lookup(C#common.mod_tab,M#mod.name) of
	    [Existing] ->
		case {Existing#mod.is_included,IsIncl} of
		    {false,_} ->
			Warning =
			    lists:concat(
			      ["Module ",M#mod.name,
			       " exists in applications ", Existing#mod.app_name,
			      " and ", M#mod.app_name,
			       ". Using module from application ",
			       M#mod.app_name, "."]),
			ets:insert(C#common.mod_tab, M2),
			reltool_utils:add_warning(Status,Warning);
		    {_,false} ->
			Warning =
			    lists:concat(
			      ["Module ",M#mod.name,
			       " exists in applications ", Existing#mod.app_name,
			      " and ", M#mod.app_name,
			       ". Using module from application ",
			       Existing#mod.app_name, "."]),

			%% Don't insert in mod_tab - using Existing
			reltool_utils:add_warning(Status,Warning);
		    {_,_} ->
			Error =
			    lists:concat(
			      ["Module ",M#mod.name,
			       " potentially included by ",
			       "two different applications: ",
			       Existing#mod.app_name, " and ",
			       M#mod.app_name, "."]),
			%% Don't insert in mod_tab - using Existing
			reltool_utils:return_first_error(Status,Error)
		end;
	    [] ->
		ets:insert(C#common.mod_tab, M2),
		Status
	end,

    %% print(M#mod.name, hipe, "~p -> ~p\n", [M2, IsIncl]),
    {M2,Status2}.

false_to_undefined(Bool) ->
    case Bool of
        false -> undefined;
        _     -> Bool
    end.

%% Return the list for {ModName, UsesModNames} for all modules where
%% #mod.is_included==true.
get_all_mods_and_dependencies(C) ->
    ets:select(C#common.mod_tab, [{#mod{name='$1',
					uses_mods='$2',
					is_included=true,
					_='_'},
				   [],
				   [{{'$1','$2'}}]}]).

propagate_is_included(C, Sys) ->
    case lists:flatmap(
	   fun({ModName,UsesModNames}) ->
		   mod_mark_is_included(C,Sys,ModName,UsesModNames,[])
	   end,
	   get_all_mods_and_dependencies(C)) of
	[] ->
	    ok;
	MissingMods ->
	    MissingApp = default_app(?MISSING_APP_NAME, "missing"),
	    MissingApp2 = MissingApp#app{label = ?MISSING_APP_TEXT,
					 info = missing_app_info(""),
					 mods = MissingMods,
					 status = missing,
					 uses_mods = []},
	    ets:insert(C#common.app_tab, MissingApp2),
	    ok
    end.

mod_mark_is_included(C, Sys, UsedByName, [ModName | ModNames], Acc) ->
    Acc3 =
        case ets:lookup(C#common.mod_tab, ModName) of
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
                                    M#mod{is_pre_included = true,
					  is_included = true};
                                undefined ->
                                    M#mod{is_included = true}
                            end,
                        ets:insert(C#common.mod_tab, M2),
                        [A] = ets:lookup(C#common.app_tab, M2#mod.app_name),
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
                                    ets:insert(C#common.app_tab, A2),
                                    mod_mark_is_included(C,
							 Sys,
							 ModName,
							 [M3#mod.name ||
							     M3 <- Mods],
							 Acc);
				_ ->
				    %% Already marked true or false
                                    Acc
                            end,
                        mod_mark_is_included(C,
					     Sys,
					     ModName,
					     M2#mod.uses_mods,
					     Acc2);
		    _ ->
                        %% Already marked true or false
                        Acc
                end;
            [] ->
                M = missing_mod(ModName, ?MISSING_APP_NAME),
                M2 = M#mod{is_included = true},
                ets:insert(C#common.mod_tab, M2),
                [M2 | Acc]
        end,
    mod_mark_is_included(C, Sys, UsedByName, ModNames, Acc3);
mod_mark_is_included(_C, _Sys, _UsedByName, [], Acc) ->
    Acc.

propagate_is_used_by(C) ->
    lists:foreach(
      fun({Mod,UsesMods}) ->
	      lists:foreach(
		fun(UsedMod) ->
			ets:insert(C#common.mod_used_by_tab,{UsedMod,Mod})
		end,
		UsesMods)
      end,
      get_all_mods_and_dependencies(C)).


app_recap_dependencies(C, Status0) ->
    ets:foldl(fun(App,Status) ->
		      app_recap_dependencies(C,App,Status)
	      end,
	      Status0,
	      C#common.app_tab).

app_recap_dependencies(C, #app{mods = Mods, is_included = IsIncl} = A, Status) ->
    {Mods2, IsIncl2, Status2} =
	mod_recap_dependencies(C, A, Mods, [], IsIncl, Status),
    AppStatus =
        case lists:keymember(missing, #mod.status, Mods2) of
            true  -> missing;
            false -> ok
        end,
    UsesMods = [M#mod.uses_mods || M <- Mods2, M#mod.is_included =:= true],
    UsesMods2 = lists:usort(lists:flatten(UsesMods)),
    UsesApps = [M#mod.app_name || ModName <- UsesMods2,
				  M <- ets:lookup(C#common.mod_tab, ModName)],
    UsesApps2 = lists:usort(UsesApps),
    UsedByMods = [M#mod.used_by_mods || M <- Mods2, M#mod.is_included =:= true],
    UsedByMods2 = lists:usort(lists:flatten(UsedByMods)),
    UsedByApps = [M#mod.app_name || ModName <- UsedByMods2,
				    M <- ets:lookup(C#common.mod_tab, ModName)],
    UsedByApps2 = lists:usort(UsedByApps),

    A2 = A#app{mods = Mods2,
               status = AppStatus,
               uses_mods = UsesMods2,
               used_by_mods = UsedByMods2,
               uses_apps = UsesApps2,
               used_by_apps = UsedByApps2,
               is_included = IsIncl2},
    ets:insert(C#common.app_tab,A2),
    Status2.

mod_recap_dependencies(C, A, [#mod{name = ModName}=M1 | Mods], Acc, IsIncl, Status) ->
    case ets:lookup(C#common.mod_tab, ModName) of
	[M2] when M2#mod.app_name=:=A#app.name ->
	    ModStatus = do_get_status(M2),
	    %% print(M2#mod.name, hipe, "status -> ~p\n", [ModStatus]),
	    {IsIncl2, M3} =
		case M2#mod.is_included of
		    true ->
			UsedByMods =
			    [N || {_, N} <- ets:lookup(C#common.mod_used_by_tab,
						       ModName)],
			{true, M2#mod{status = ModStatus, used_by_mods = UsedByMods}};
		    _    ->
			{IsIncl, M2#mod{status = ModStatus, used_by_mods = []}}
		end,
	    ets:insert(C#common.mod_tab, M3),
	    mod_recap_dependencies(C, A, Mods, [M3 | Acc], IsIncl2, Status);
	[_] when A#app.is_included==false; M1#mod.incl_cond==exclude -> %!!! incl_cond could be read from #sys.app.mods
	    %% App is explicitely excluded so it is ok that the module
	    %% record does not exist for this module in this
	    %% application.
	    mod_recap_dependencies(C, A, Mods, [M1 | Acc], IsIncl, Status);
	[M2] ->
	    %% A module is potensially included by multiple
	    %% applications. This is not allowed!
	    Error =
		lists:concat(
		  ["Module ",ModName,
		   " potentially included by two different applications: ",
		   A#app.name, " and ", M2#mod.app_name, "."]),
	    Status2 = reltool_utils:return_first_error(Status,Error),
	    mod_recap_dependencies(C, A, Mods, [M1 | Acc], IsIncl, Status2)
    end;
mod_recap_dependencies(_C, _A, [], Acc, IsIncl, Status) ->
    {lists:reverse(Acc), IsIncl, Status}.

do_get_status(M) ->
    if
        M#mod.exists =:= false, M#mod.is_included =/= false ->
            missing;
        true ->
            ok
    end.

verify_config(C, #sys{boot_rel = BootRel, rels = Rels}, RelApps, Status) ->
    case lists:keymember(BootRel, #rel.name, Rels) of
        true ->
	    Status2 = lists:foldl(fun(RA, Acc) ->
					  check_app(C, RA, Acc) end,
				  Status,
				  RelApps),
	    lists:foldl(fun(#rel{name = RelName}, Acc)->
				check_rel(RelName, RelApps, Acc)
			end,
			Status2,
			Rels);
        false ->
	    Text = lists:concat(["Release ", BootRel,
				 " is mandatory (used as boot_rel)"]),
	    reltool_utils:return_first_error(Status, Text)
    end.

check_app(C, {RelName, AppName}, Status) ->
    case ets:lookup(C#common.app_tab, AppName) of
	[#app{is_pre_included=IsPreIncl, is_included=IsIncl}]
	  when IsPreIncl; IsIncl ->
	    Status;
	_ ->
	    Text = lists:concat(["Release ", RelName,
				 " uses non included application ",
				 AppName]),
	    reltool_utils:return_first_error(Status, Text)
    end.

check_rel(RelName, RelApps, Status) ->
    EnsureApp =
        fun(AppName, Acc) ->
                case lists:member({RelName, AppName}, RelApps) of
                    true ->
                        Acc;
                    false ->
			Text = lists:concat(["Mandatory application ",
					     AppName,
					     " is not included in release ",
					     RelName]),
			reltool_utils:return_first_error(Acc, Text)
                end
        end,
    Mandatory = [kernel, stdlib],
    lists:foldl(EnsureApp, Status, Mandatory).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

refresh_app(#app{name = AppName,
                 is_escript = IsEscript,
                 active_dir = ActiveDir,
                 label = OptLabel,
                 mods = Mods} = App,
            Force,
            Status) ->
    if
        Force; OptLabel =:= undefined ->
            {AppInfo, EbinMods, Status3} =
                case IsEscript of
                    false ->

                        %% Add info from .app file
                        Base = get_base(AppName, ActiveDir),
                        {_, DefaultVsn} = reltool_utils:split_app_name(Base),
                        Ebin = filename:join([ActiveDir, "ebin"]),
                        AppFile =
			    filename:join([Ebin,
					   atom_to_list(AppName) ++ ".app"]),
                        {AI, Status2} =
			    read_app_info(AppFile,
					  AppFile,
					  AppName,
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
	    AppInfoMods = AppInfo#app_info.modules,
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
	    %% (set by parse_options/1). So here we merge with the
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
            {App2, Status3};
        true ->
            {App, Status}
    end.

missing_app_info(Vsn) ->
    #app_info{vsn = Vsn}.

read_app_info(_AppFileOrBin, _AppFile, erts, DefaultVsn, Status) ->
    {missing_app_info(DefaultVsn), Status};
read_app_info(AppFileOrBin, AppFile, AppName, DefaultVsn, Status) ->
    EnoentText = file:format_error(enoent),
    case reltool_utils:prim_consult(AppFileOrBin) of
        {ok,  [{application, AppName, Info}]} ->
            AI = #app_info{vsn = DefaultVsn},
            parse_app_info(AppFile, Info, AI, Status);
        {ok, _BadApp} ->
            Text = lists:concat([AppName,
				 ": Illegal contents in app file ", AppFile,
				 ", application tuple with arity 3 expected."]),
            {missing_app_info(DefaultVsn),
	     reltool_utils:add_warning(Status, Text)};
        {error, Text} when Text =:= EnoentText ->
	    Text2 = lists:concat([AppName,
				  ": Missing app file ", AppFile, "."]),
	    {missing_app_info(DefaultVsn),
	     reltool_utils:add_warning(Status, Text2)};
        {error, Text} ->
            Text2 = lists:concat([AppName,
				  ": Cannot parse app file ",
				  AppFile, " (", Text, ")."]),
            {missing_app_info(DefaultVsn),
	     reltool_utils:add_warning(Status, Text2)}
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
        _ ->
	    String = lists:concat(["Unexpected item ",
				   Key, "in app file ", File]),
	    parse_app_info(File, KeyVals, AI,
			   reltool_utils:add_warning(Status, String))
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
	    error_logger:error_msg("~p(~p): Waiting for process ~p to die ~p\n",
				   [?MODULE, ?LINE, Pid, File]),
	    wait_for_processto_die(Ref, Pid, File)
    end.

add_missing_mods(AppName, EbinMods, AppModNames) ->
    EbinModNames = [M#mod.name || M <- EbinMods],
    MissingModNames = AppModNames -- EbinModNames,
    [missing_mod(ModName, AppName) || ModName <- MissingModNames].

missing_mod(ModName, AppName) ->
    %% io:format("Missing: ~p -> ~p\n", [AppName, ModName]),
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
    AppTab = (S#state.common)#common.app_tab,
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
		sys_all_apps(S#state.common,S#state.sys)
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
			{shrinked, [], undefined};
                    false ->
			{shrinked, [], undefined};
                    true ->
			{A#app.active_dir, [A#app.active_dir], A#app.vsn}
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
    {NewSys, Status} = read_config(default_sys(), SysConfig, {ok, []}),
    {S#state{sys = NewSys}, Status}.

read_config(OldSys, Filename, Status) when is_list(Filename) ->
    case file:consult(Filename) of
        {ok, [SysConfig | _]} ->
            read_config(OldSys, SysConfig, Status);
        {ok, Content} ->
            Text = lists:flatten(io_lib:format("~p", [Content])),
            {OldSys,
	     reltool_utils:return_first_error(Status,
					      "Illegal file content: " ++
					      Text)};
        {error, Reason} ->
            Text = file:format_error(Reason),
            {OldSys,
	     reltool_utils:return_first_error(Status,
					      "Illegal config file " ++
					      Filename ++ ": " ++ Text)}
    end;
read_config(OldSys, {sys, KeyVals}, Status) ->
    {NewSys, Status2} =
	decode(OldSys#sys{apps = [], rels = []}, KeyVals, Status),
    case Status2 of
	{ok, _Warnings} -> % BUGBUG: handle warnings
	    Apps = [A#app{mods = lists:sort(A#app.mods)} ||
		       A <- NewSys#sys.apps],
	    case NewSys#sys.rels of
		[]   -> Rels = reltool_utils:default_rels();
		Rels -> ok
	    end,
	    NewSys2 = NewSys#sys{apps = lists:sort(Apps),
				 rels = lists:sort(Rels)},
	    case lists:keymember(NewSys2#sys.boot_rel,
				 #rel.name,
				 NewSys2#sys.rels) of
		true ->
		    {NewSys2, Status2};
		false ->
		    Text2 = lists:concat(["Release " ++ NewSys2#sys.boot_rel,
					  " is mandatory (used as boot_rel)"]),
		    {OldSys, reltool_utils:return_first_error(Status2, Text2)}
	    end;
	{error, _} ->
            %% Keep old state
	    {OldSys, Status2}
    end;
read_config(OldSys, BadConfig, Status) ->
    Text = lists:flatten(io_lib:format("~p", [BadConfig])),
    {OldSys,
     reltool_utils:return_first_error(Status, "Illegal content: " ++ Text)}.

decode(#sys{apps = Apps} = Sys, [{erts = Name, AppKeyVals} | SysKeyVals],
       Status)
  when is_atom(Name), is_list(AppKeyVals) ->
    App = default_app(Name),
    {App2, Status2} = decode(App, AppKeyVals, Status),
    decode(Sys#sys{apps = [App2 | Apps]}, SysKeyVals, Status2);
decode(#sys{apps = Apps} = Sys, [{app, Name, AppKeyVals} | SysKeyVals], Status)
  when is_atom(Name), is_list(AppKeyVals) ->
    App = default_app(Name),
    {App2, Status2} = decode(App, AppKeyVals, Status),
    decode(Sys#sys{apps = [App2 | Apps]}, SysKeyVals, Status2);
decode(#sys{apps = Apps, escripts = Escripts} = Sys,
       [{escript, File0, AppKeyVals} | SysKeyVals], Status)
  when is_list(File0), is_list(AppKeyVals) ->
    File = filename:absname(File0),
    App = default_escript_app(File),
    {App2, Status2} = decode(App, AppKeyVals, Status),
    decode(Sys#sys{apps = [App2 | Apps], escripts = [File | Escripts]},
	   SysKeyVals,
	   Status2);
decode(#sys{rels = Rels} = Sys, [{rel, Name, Vsn, RelApps} | SysKeyVals],
       Status)
  when is_list(Name), is_list(Vsn), is_list(RelApps) ->
    Rel = #rel{name = Name, vsn = Vsn, rel_apps = []},
    {Rel2, Status2} = decode(Rel, RelApps, Status),
    decode(Sys#sys{rels = [Rel2 | Rels]}, SysKeyVals, Status2);
decode(#sys{} = Sys, [{Key, Val} | KeyVals], Status) ->
    {Sys3, Status3} =
        case Key of
            root_dir when is_list(Val) ->
                {Sys#sys{root_dir = Val}, Status};
            lib_dirs when is_list(Val) ->
                {Sys#sys{lib_dirs = Val}, Status};
            mod_cond when Val =:= all;
			  Val =:= app;
                          Val =:= ebin;
			  Val =:= derived;
                          Val =:= none ->
                {Sys#sys{mod_cond = Val}, Status};
            incl_cond when Val =:= include;
			   Val =:= exclude;
                           Val =:= derived ->
                {Sys#sys{incl_cond = Val}, Status};
            boot_rel when is_list(Val) ->
                {Sys#sys{boot_rel = Val}, Status};
            emu_name when is_list(Val) ->
                {Sys#sys{emu_name = Val}, Status};
	    profile when Val =:= development;
			 Val =:= embedded;
			 Val =:= standalone ->
		InclSys = reltool_utils:choose_default(incl_sys_filters, Val, false),
		ExclSys = reltool_utils:choose_default(excl_sys_filters, Val, false),
		InclApp = reltool_utils:choose_default(incl_app_filters, Val, false),
		ExclApp = reltool_utils:choose_default(excl_app_filters, Val, false),
		AppType = reltool_utils:choose_default(embedded_app_type, Val, false),
		{Sys#sys{profile = Val,
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
			 embedded_app_type = AppType},
                 Status};
            incl_sys_filters ->
                {Sys#sys{incl_sys_filters =
			 dec_re(Key,
				Val,
				Sys#sys.incl_sys_filters)},
		 Status};
            excl_sys_filters ->
                {Sys#sys{excl_sys_filters =
			 dec_re(Key,
				Val,
				Sys#sys.excl_sys_filters)},
		 Status};
            incl_app_filters ->
                {Sys#sys{incl_app_filters =
			 dec_re(Key,
				Val,
				Sys#sys.incl_app_filters)},
		 Status};
            excl_app_filters ->
                {Sys#sys{excl_app_filters =
			 dec_re(Key,
				Val,
				Sys#sys.excl_app_filters)},
		 Status};
            incl_archive_filters ->
                {Sys#sys{incl_archive_filters =
			 dec_re(Key,
				Val,
				Sys#sys.incl_archive_filters)},
		 Status};
            excl_archive_filters ->
                {Sys#sys{excl_archive_filters =
			 dec_re(Key,
				Val,
				Sys#sys.excl_archive_filters)},
		 Status};
            archive_opts when is_list(Val) ->
                {Sys#sys{archive_opts = Val}, Status};
            relocatable when Val =:= true; Val =:= false ->
                {Sys#sys{relocatable = Val}, Status};
            rel_app_type when Val =:= permanent;
			      Val =:= transient;
			      Val =:= temporary;
			      Val =:= load;
			      Val =:= none ->
                {Sys#sys{rel_app_type = Val}, Status};
	    embedded_app_type when Val =:= permanent;
				   Val =:= transient;
				   Val =:= temporary;
				   Val =:= load;
				   Val =:= none;
				   Val =:= undefined ->
                {Sys#sys{embedded_app_type = Val}, Status};
            app_file when Val =:= keep; Val =:= strip; Val =:= all ->
                {Sys#sys{app_file = Val}, Status};
            debug_info when Val =:= keep; Val =:= strip ->
                {Sys#sys{debug_info = Val}, Status};
            _ ->
                Text = lists:flatten(io_lib:format("~p", [{Key, Val}])),
                {Sys, reltool_utils:return_first_error(Status,
						       "Illegal option: " ++
						       Text)}
        end,
    decode(Sys3, KeyVals, Status3);
decode(#app{} = App, [{Key, Val} | KeyVals], Status) ->
    {App2, Status2} =
        case Key of
            mod_cond when Val =:= all;
			  Val =:= app;
			  Val =:= ebin;
			  Val =:= derived;
			  Val =:= none ->
                {App#app{mod_cond = Val}, Status};
            incl_cond when Val =:= include;
			   Val =:= exclude;
			   Val =:= derived ->
                {App#app{incl_cond = Val}, Status};

            debug_info when Val =:= keep;
			    Val =:= strip ->
                {App#app{debug_info = Val}, Status};
            app_file when Val =:= keep;
			  Val =:= strip;
			  Val =:= all ->
                {App#app{app_file = Val}, Status};
            app_type when Val =:= permanent;
			  Val =:= transient;
			  Val =:= temporary;
                          Val =:= load;
			  Val =:= none;
			  Val =:= undefined ->
                {App#app{app_type = Val}, Status};
            incl_app_filters ->
                {App#app{incl_app_filters =
			 dec_re(Key,
				Val,
				App#app.incl_app_filters)},
		 Status};
            excl_app_filters ->
                {App#app{excl_app_filters =
			 dec_re(Key,
				Val,
				App#app.excl_app_filters)},
		 Status};
            incl_archive_filters ->
                {App#app{incl_archive_filters =
			 dec_re(Key,
				Val,
				App#app.incl_archive_filters)},
		 Status};
            excl_archive_filters ->
                {App#app{excl_archive_filters =
			 dec_re(Key,
				Val,
				App#app.excl_archive_filters)},
		 Status};
            archive_opts when is_list(Val) ->
                {App#app{archive_opts = Val}, Status};
            vsn when is_list(Val) ->
                {App#app{use_selected_vsn = true, vsn = Val}, Status};
            _ ->
                Text = lists:flatten(io_lib:format("~p", [{Key, Val}])),
                {App, reltool_utils:return_first_error(Status,
						       "Illegal option: " ++ Text)}
        end,
    decode(App2, KeyVals, Status2);
decode(#app{mods = Mods} = App, [{mod, Name, ModKeyVals} | AppKeyVals],
       Status) ->
    {Mod, Status2} = decode(#mod{name = Name}, ModKeyVals, Status),
    decode(App#app{mods = [Mod | Mods]}, AppKeyVals, Status2);
decode(#mod{} = Mod, [{Key, Val} | KeyVals], Status) ->
    {Mod2, Status2} =
        case Key of
            incl_cond when Val =:= include; Val =:= exclude; Val =:= derived ->
                {Mod#mod{incl_cond = Val}, Status};
            debug_info when Val =:= keep; Val =:= strip ->
                {Mod#mod{debug_info = Val}, Status};
            _ ->
                Text = lists:flatten(io_lib:format("~p", [{Key, Val}])),
                {Mod,
		 reltool_utils:return_first_error(Status,
						  "Illegal option: " ++ Text)}
        end,
    decode(Mod2, KeyVals, Status2);
decode(#rel{rel_apps = RelApps} = Rel, [RelApp | KeyVals], Status) ->
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
                {false, #rel_app{incl_apps = []}}
        end,
    case ValidTypesAssigned of
	true ->
            decode(Rel#rel{rel_apps = RelApps ++ [RA]}, KeyVals, Status);
        false ->
            Text = lists:flatten(io_lib:format("~p", [RelApp])),
            Status2 =
		reltool_utils:return_first_error(Status,
						 "Illegal option: " ++ Text),
            decode(Rel, KeyVals, Status2)
    end;
decode(Acc, [], Status) ->
    {Acc, Status};
decode(Acc, KeyVal, Status) ->
    Text = lists:flatten(io_lib:format("~p", [KeyVal])),
    {Acc, reltool_utils:return_first_error(Status, "Illegal option: " ++ Text)}.

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
refresh(#state{sys=Sys} = S, Status) ->
    RootDir = filename:absname(Sys#sys.root_dir),
    LibDirs = [filename:absname(D) || D <- Sys#sys.lib_dirs],
    Escripts = [filename:absname(E) || E <- Sys#sys.escripts],

    %% Read all lib dirs and return sorted [{AppName,Dir}]
    {SourceDirs, Status2} = libs_to_dirs(RootDir, LibDirs, Status),

    %% Create #app records for all apps in SourceDirs, and merge with
    %% list of apps from config.
    MergedApps = merge_app_dirs(SourceDirs, Sys#sys.apps),

    %% For each escript, find all related files and convert to #app
    %% and #mod records
    {AllApps, Status3} = escripts_to_apps(Escripts, MergedApps, Status2),

    %% Make sure correct version of each application is used according
    %% to the user configuration.
    %% Then find all modules and their dependencies and set user
    %% configuration per module if it exists.
    {RefreshedApps, Status4} = refresh_apps(Sys#sys.apps, AllApps, [],
					    true, Status3),

    %% Make sure erts exists in app list and has a version (or warn)
    {PatchedApps, Status5} = patch_erts_version(RootDir, RefreshedApps, Status4),

    %% Update #sys and return
    Escripts2 = [A#app.active_dir || A <- PatchedApps, A#app.is_escript],
    Sys2 = Sys#sys{root_dir = RootDir,
		   lib_dirs = LibDirs,
		   escripts = Escripts2},
    {S#state{sys=Sys2}, PatchedApps, Status5}.

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
                    {Apps, reltool_utils:add_warning(Status,
						     "erts has no version")};
                true ->
                    {Apps, Status}
            end;
        false ->
            Text = "erts cannot be found in the root directory " ++ RootDir,
            Status2 = reltool_utils:return_first_error(Status, Text),
            {Apps, Status2}
    end.

libs_to_dirs(RootDir, LibDirs, Status) ->
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
                    app_dirs2(AllLibDirs, [ErtsFiles], Status);
                [Duplicate | _] ->
                    {[],
		     reltool_utils:return_first_error(Status,
						      "Duplicate library: " ++
						      Duplicate)}
            end;
        {error, Reason} ->
            Text = file:format_error(Reason),
            {[], reltool_utils:return_first_error(Status,
						  "Missing root library " ++
						  RootDir ++ ": " ++ Text)}
    end.

app_dirs2([Lib | Libs], Acc, Status) ->
    case file:list_dir(Lib) of
        {ok, Files} ->
            Filter =
                fun(Base) ->
                        AppDir = filename:join([Lib, Base]),
                        EbinDir = filename:join([AppDir, "ebin"]),
                        case filelib:is_dir(EbinDir, erl_prim_loader) of
                            true ->
                                {Name, _Vsn} =
				    reltool_utils:split_app_name(Base),
                                case Name of
                                    erts -> false;
                                    _    -> {true, {Name, AppDir}}
                                end;
                            false ->
                                false
                        end
                end,
            Files2 = lists:zf(Filter, Files),
            app_dirs2(Libs, [Files2 | Acc], Status);
        {error, Reason} ->
            Text = file:format_error(Reason),
            {[], reltool_utils:return_first_error(Status,
						  "Illegal library " ++
						  Lib ++ ": " ++ Text)}
    end;
app_dirs2([], Acc, Status) ->
    {lists:sort(lists:append(Acc)), Status}.

escripts_to_apps([Escript | Escripts], Apps, Status) ->
    {EscriptAppName, _Label} = split_escript_name(Escript),
    Ext = code:objfile_extension(),
    Fun = fun(FullName, _GetInfo, GetBin, {FileAcc, StatusAcc}) ->
                  Components = filename:split(FullName),
                  case Components of
                      [AppLabel, "ebin", File] ->
                          case filename:extension(File) of
                              ".app" ->
                                  {AppName, DefaultVsn} =
				      reltool_utils:split_app_name(AppLabel),
                                  AppFileName =
				      filename:join([Escript, FullName]),
                                  {Info, StatusAcc2} =
                                      read_app_info(GetBin(),
						    AppFileName,
						    AppName,
						    DefaultVsn,
						    Status),
                                  Dir = filename:join([Escript, AppName]),
                                  {[{AppName, app, Dir, Info} | FileAcc],
				   StatusAcc2};
                              E when E =:= Ext ->
                                  {AppName, _} =
				      reltool_utils:split_app_name(AppLabel),
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
	{error, Reason} ->
	    Text = lists:flatten(io_lib:format("~p", [Reason])),
	    {[], reltool_utils:return_first_error(Status,
						  "Illegal escript " ++
						  Escript ++ ": " ++ Text)}
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
            Error = lists:concat([AppName, ": Application name clash. ",
                                  "Escript ", Dir," contains application ",
				  AppName, "."]),
            {App2, reltool_utils:return_first_error(Status, Error)};
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

%% First dir, i.e. the one with highest version, is set to active dir
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
ensure_app_info(#app{name = Name, sorted_dirs = []}, Status) ->
    Error = lists:concat([Name, ": Missing application directory."]),
    Status2 = reltool_utils:return_first_error(Status, Error),
    {missing_app_info(""), undefined, Status2};
ensure_app_info(#app{name = Name,
		     vsn = Vsn,
		     sorted_dirs = Dirs,
		     info = undefined},
		Status) ->
    ReadInfo =
        fun(Dir, StatusAcc) ->
                Base = get_base(Name, Dir),
                Ebin = filename:join([Dir, "ebin"]),
                {_, DefaultVsn} = reltool_utils:split_app_name(Base),
                AppFile = filename:join([Ebin, atom_to_list(Name) ++ ".app"]),
                read_app_info(AppFile, AppFile, Name, DefaultVsn, StatusAcc)
        end,
    {AllInfo, Status2} = lists:mapfoldl(ReadInfo, Status, Dirs),
    AllVsns = [I#app_info.vsn || I <- AllInfo],
    Status3 =
        case AllVsns -- lists:usort(AllVsns) of
            [] ->
                %% No redundant info
                Status2;
            [BadVsn | _] ->
                Error2 =
		    lists:concat([Name, ": Application version clash. ",
				  "Multiple directories contains version \"",
				  BadVsn, "\"."]),
                reltool_utils:return_first_error(Status2, Error2)
        end,
    FirstInfo = hd(AllInfo),
    FirstDir = hd(Dirs),
    if
        Vsn =:= undefined ->
            {FirstInfo, FirstDir, Status3};
        Vsn =:= FirstInfo#app_info.vsn ->
            {FirstInfo, FirstDir, Status3};
        true ->
            case find_vsn(Vsn, AllInfo, Dirs) of
                {Info, VsnDir} ->
                    {Info, VsnDir, Status3};
                false ->
                    Error3 =
			lists:concat([Name,
				      ": No application directory contains ",
				      "selected version \"",
				      Vsn, "\"."]),
                    Status4 = reltool_utils:return_first_error(Status3, Error3),
                    {FirstInfo, FirstDir, Status4}
            end
    end;
ensure_app_info(#app{active_dir = Dir, info = Info}, Status) ->
    {Info, Dir, Status}.

find_vsn(Vsn, [#app_info{vsn = Vsn} = Info | _], [Dir | _]) ->
    {Info, Dir};
find_vsn(Vsn, [_ | MoreInfo], [_ | MoreDirs]) ->
    find_vsn(Vsn, MoreInfo, MoreDirs);
find_vsn(_, [], []) ->
    false.

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

sys_all_apps(C,Sys) ->
    Sys#sys{apps = all_apps(C)}.

all_apps(C) ->
    ets:match_object(C#common.app_tab,'_').

refresh_and_analyse_no_rollback(#state{common=C} = S, {ok,_} = Status) ->
    case refresh(S, Status) of
	{S2, Apps, {ok, _}=Status2} ->
	    case analyse(S2, Apps, Status2) of
		{ok, _} = Status3 ->
		    %% Set old_xxx is equal to xxx
		    FakeBackup = {ets:tab2list(C#common.app_tab),
				  ets:tab2list(C#common.mod_tab)},
		    save_old(S2, S2, FakeBackup, Status3);
		{error,Reason} ->
		    exit(Reason)
	    end;
	{_,_,{error,Reason}} ->
	    exit(Reason)
    end;
refresh_and_analyse_no_rollback(_,{error,Reason}) ->
    exit(Reason).

refresh_and_analyse(OldS, S, {ok,_}=Status) ->
    case refresh(S, Status) of
	{S2, Apps, {ok,_}=Status2} ->
	    %% Analyse will write to app_tab and mod_tab, so we first
	    %% backup these tables and clear them
	    Backup = backup(OldS),
	    case analyse(S2, Apps, Status2) of
		{ok, _} = Status3 ->
		    save_old(OldS, S2, Backup, Status3);
		Status3 ->
		    restore(Backup,OldS),
		    {OldS,Status3}
	    end;
	{_, _, Status2} ->
	    {OldS, Status2}
    end;
refresh_and_analyse(OldS, _S, Status) ->
    {OldS,Status}.


backup(#state{common=C}) ->
    Apps = ets:tab2list(C#common.app_tab),
    Mods = ets:tab2list(C#common.mod_tab),
    ets:delete_all_objects(C#common.app_tab),
    ets:delete_all_objects(C#common.mod_tab),
    ets:delete_all_objects(C#common.mod_used_by_tab), %tmp tab, no backup needed
    {Apps,Mods}.

restore({Apps,Mods}, #state{common=C}) ->
    insert_all(C#common.app_tab,Apps),
    insert_all(C#common.mod_tab,Mods).

save_old(#state{status=OldStatus,sys=OldSys},#state{common=C}=NewS,
	 {OldApps,OldMods},NewStatus) ->
    ets:delete_all_objects(C#common.old_app_tab),
    ets:delete_all_objects(C#common.old_mod_tab),
    insert_all(C#common.old_app_tab,OldApps),
    insert_all(C#common.old_mod_tab,OldMods),
    {NewS#state{old_sys=OldSys,
		old_status=OldStatus,
		status=NewStatus},
     NewStatus}.

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
