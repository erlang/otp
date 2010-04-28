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
    {S, Status} = parse_options(Options),
    #state{parent_pid = ParentPid, common = C, sys = Sys} = S,

    %% process_flag(trap_exit, (S#state.common)#common.trap_exit),
    proc_lib:init_ack(ParentPid,
		      {ok, self(), C, Sys#sys{apps = undefined}}),
    {S2, Status2} = refresh(S, true, Status),
    {S3, Status3} =
	analyse(S2#state{old_sys = S2#state.sys}, Status2),
    case Status3 of
	{ok, _Warnings} -> % BUGBUG: handle warnings
	    loop(S3#state{status = Status3, old_status = {ok, []}});
	{error, Reason} ->
	    exit(Reason)
    end.

parse_options(Opts) ->
    AppTab = ets:new(reltool_apps, [public, ordered_set, {keypos, #app.name}]),
    ModTab = ets:new(reltool_mods, [public, ordered_set, {keypos, #mod.name}]),
    ModUsesTab = ets:new(reltool_mod_uses, [public, bag, {keypos, 1}]),
    Sys = #sys{root_dir          = reltool_utils:root_dir(),
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
               debug_info        = ?DEFAULT_DEBUG_INFO},
    C2 = #common{sys_debug = [],
                 wx_debug = 0,
                 trap_exit = true,
                 app_tab = AppTab,
                 mod_tab = ModTab,
                 mod_used_by_tab = ModUsesTab},
    S = #state{options = Opts},
    parse_options(Opts, S, C2, Sys, {ok, []}).

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
            {S2, Reply} = do_load_config(S, SysConfig),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, {save_config, Filename, InclDef, InclDeriv}} ->
            Reply = do_save_config(S, Filename, InclDef, InclDeriv),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, reset_config} ->
            {S2, Status} = parse_options(S#state.options),
            S3 = shrink_sys(S2),
            {S4, Status2} = refresh(S3, true, Status),
            {S5, Status3} = analyse(S4#state{old_sys = S4#state.sys}, Status2),
            S6 =
                case Status3 of
                    {ok, _Warnings} ->
                        S5#state{status = Status3, old_status = S#state.status};
                    {error, _} ->
			%% Keep old state
                        S
                end,
            reltool_utils:reply(ReplyTo, Ref, Status3),
            ?MODULE:loop(S6);
        {call, ReplyTo, Ref, undo_config} ->
            reltool_utils:reply(ReplyTo, Ref, ok),
            S2 = S#state{sys = S#state.old_sys,
                         old_sys = S#state.sys,
                         status = S#state.old_status,
                         old_status = S#state.status},
            ?MODULE:loop(S2);
        {call, ReplyTo, Ref, {get_rel, RelName}} ->
            Sys = S#state.sys,
            Reply =
                case lists:keysearch(RelName, #rel.name, Sys#sys.rels) of
                    {value, Rel} ->
                        reltool_target:gen_rel(Rel, Sys);
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
                        reltool_target:gen_script(Rel, Sys, PathFlag, Vars);
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
                case lists:keysearch(AppName, #app.name, Sys#sys.apps) of
                    {value, App} ->
                        {ok, App};
                    false ->
                        {error, "No such application: " ++
			 atom_to_list(AppName)}
                end,
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_app, App}} ->
            {S2, Status} = do_set_app(S, App, {ok, []}),
            {S3, Status2} = analyse(S2, Status),
            case Status2 of
                {ok, Warnings} ->
                    App2 = ?KEYSEARCH(App#app.name,
                                      #app.name,
                                      (S3#state.sys)#sys.apps),
                    reltool_utils:reply(ReplyTo, Ref, {ok, App2, Warnings}),
                    ?MODULE:loop(S3);
                {error, Reason} ->
		    %% Keep old state
                    reltool_utils:reply(ReplyTo, Ref, {error, Reason}),
                    ?MODULE:loop(S)
            end;
        {call, ReplyTo, Ref, {get_apps, Kind}} ->
            AppNames =
                case Kind of
                    whitelist ->
                        [A ||
                            A <- Sys#sys.apps,
                            A#app.is_pre_included =:= true];
                    blacklist ->
                        [A ||
                            A <- Sys#sys.apps,
                            A#app.is_pre_included =:= false];
                    source ->
                        [A ||
                            A <- Sys#sys.apps,
                            A#app.is_included =/= true,
                            A#app.is_pre_included =/= false];
                    derived ->
                        [A ||
                            A <- Sys#sys.apps,
                            A#app.is_included =:= true,
                            A#app.is_pre_included =/= true]
                end,
            reltool_utils:reply(ReplyTo, Ref, {ok, AppNames}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_apps, Apps}} ->
            {S2, Status} =
		lists:foldl(fun(A, {X, Y}) -> do_set_app(X, A, Y) end,
			    {S, {ok, []}},
			    Apps),
            {S3, Status2} = analyse(S2, Status),
            reltool_utils:reply(ReplyTo, Ref, Status2),
            ?MODULE:loop(S3);
        {call, ReplyTo, Ref, get_sys} ->
            reltool_utils:reply(ReplyTo, Ref, {ok, Sys#sys{apps = undefined}}),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {set_sys, Sys2}} ->
            S2 = S#state{sys = Sys2#sys{apps = Sys#sys.apps}},
            Force =
                (Sys2#sys.root_dir =/= Sys#sys.root_dir) orelse
                (Sys2#sys.lib_dirs =/= Sys#sys.lib_dirs) orelse
                (Sys2#sys.escripts =/= Sys#sys.escripts),
            {S3, Status} = refresh(S2, Force, {ok, []}),
	    {S4, Status2} =
		analyse(S3#state{old_sys = S#state.sys}, Status),
	    {S5, Status3} =
		case Status2 of
		    {ok, _Warnings} -> % BUGBUG: handle warnings
			{S4#state{status = Status2,
				  old_status = S#state.status},
			 Status2};
		    {error, _} ->
			%% Keep old state
			{S, Status2}
		end,
            reltool_utils:reply(ReplyTo, Ref, Status3),
            ?MODULE:loop(S5);
        {call, ReplyTo, Ref, get_status} ->
            reltool_utils:reply(ReplyTo, Ref, S#state.status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_rel_files, Dir}} ->
            Status =
                case reltool_target:gen_rel_files(S#state.sys, Dir) of
                    ok ->
                        {ok, []};
                    {error, Reason} ->
                        {error, Reason}
                end,
            reltool_utils:reply(ReplyTo, Ref, Status),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, {gen_target, Dir}} ->
            Reply = reltool_target:gen_target(S#state.sys, Dir),
            reltool_utils:reply(ReplyTo, Ref, Reply),
            ?MODULE:loop(S);
        {call, ReplyTo, Ref, gen_spec} ->
            Reply = reltool_target:gen_spec(S#state.sys),
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

do_set_app(#state{sys = Sys} = S, App, Status) ->
    AppName = App#app.name,
    {App2, Status2} = refresh_app(App, false, Status),
    Apps = Sys#sys.apps,
    Apps2 = lists:keystore(AppName, #app.name, Apps, App2),
    Escripts = [A#app.active_dir || A <- Apps2, A#app.is_escript],
    Sys2 = Sys#sys{apps = Apps2, escripts = Escripts},
    {S#state{sys = Sys2}, Status2}.

analyse(#state{common = C,
	       sys = #sys{apps = Apps0, rels = Rels} = Sys} = S,
	Status) ->
    Apps = lists:keydelete(?MISSING_APP_NAME, #app.name, Apps0),
    ets:delete_all_objects(C#common.app_tab),
    ets:delete_all_objects(C#common.mod_tab),
    ets:delete_all_objects(C#common.mod_used_by_tab),
    MissingApp = default_app(?MISSING_APP_NAME, "missing"),
    ets:insert(C#common.app_tab, MissingApp),

    {RevRelApps, Status2} = apps_in_rels(Rels, Apps, Status),
    RelApps2 = lists:reverse(RevRelApps),
    {Apps2, Status3} =
	lists:mapfoldl(fun(App, Acc) ->
			       app_init_is_included(C, Sys, App, RelApps2, Acc)
		       end,
		  Status2,
		  Apps),
    Apps3 =
        case app_propagate_is_included(C, Sys, Apps2, []) of
            [] ->
                Apps2;
            MissingMods ->
                %% io:format("Missing mods: ~p\n", [MissingMods]),
                MissingApp2 = MissingApp#app{label = ?MISSING_APP_TEXT,
                                             info = missing_app_info(""),
                                             mods = MissingMods,
                                             status = missing,
                                             uses_mods = []},
                [MissingApp2 | Apps2]
        end,
    app_propagate_is_used_by(C, Apps3),
    Apps4 = read_apps(C, Sys, Apps3, []),
    %% io:format("Missing app: ~p\n",
    %%           [lists:keysearch(?MISSING_APP_NAME, #app.name, Apps4)]),
    Sys2 = Sys#sys{apps = Apps4},

    case verify_config(RelApps2, Sys2, Status3) of
	{ok, _Warnings} = Status4 ->
	    {S#state{sys = Sys2}, Status4};
	{error, _} = Status4 ->
            {S, Status4}
    end.

apps_in_rels(Rels, Apps, Status) ->
    lists:foldl(fun(Rel, {RelApps, S}) ->
			{MoreRelApps, S2} = apps_in_rel(Rel, Apps, S),
			{MoreRelApps ++ RelApps, S2}
		end,
		{[], Status},
		Rels).

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
	    case lists:keysearch(AppName, #app.name, Apps) of
		{value, #app{info = #app_info{applications = InfoApps}}} ->
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
    A2 = A#app{is_pre_included = IsPreIncl,
	       is_included = IsIncl,
	       rels = Rels},
    ets:insert(C#common.app_tab, A2),
    lists:foreach(fun(Mod) ->
			  mod_init_is_included(C,
					       Mod,
					       ModCond,
					       AppCond,
					       Default)
		  end,
		  Mods),
    {A2, Status2}.

mod_init_is_included(C, M, ModCond, AppCond, Default) ->
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
    %% print(M#mod.name, hipe, "~p -> ~p\n", [M2, IsIncl]),
    ets:insert(C#common.mod_tab, M2).

false_to_undefined(Bool) ->
    case Bool of
        false -> undefined;
        _     -> Bool
    end.

app_propagate_is_included(C, Sys, [#app{mods = Mods} = A | Apps], Acc) ->
    Acc2 = mod_propagate_is_included(C, Sys, A, Mods, Acc),
    app_propagate_is_included(C, Sys, Apps, Acc2);
app_propagate_is_included(_C, _Sys, [], Acc) ->
    Acc.

mod_propagate_is_included(C, Sys, A, [#mod{name = ModName} | Mods], Acc) ->
    [M2] = ets:lookup(C#common.mod_tab, ModName),
    %% print(ModName, file, "Maybe Prop ~p -> ~p\n",
    %%       [M2, M2#mod.is_included]),
    %% print(ModName, filename, "Maybe Prop ~p -> ~p\n",
    %%       [M2, M2#mod.is_included]),
    Acc2 =
        case M2#mod.is_included of
            true ->
                %% Propagate include mark
                mod_mark_is_included(C, Sys, ModName, M2#mod.uses_mods, Acc);
            false ->
                Acc;
            undefined ->
                Acc
        end,
    mod_propagate_is_included(C, Sys, A, Mods, Acc2);
mod_propagate_is_included(_C, _Sys, _A, [], Acc) ->
    Acc.

mod_mark_is_included(C, Sys, UsedByName, [ModName | ModNames], Acc) ->
    Acc3 =
        case ets:lookup(C#common.mod_tab, ModName) of
            [M] ->
                %% print(UsedByName, file, "Maybe Mark ~p -> ~p\n",
		%%       [M, M#mod.is_included]),
                %% print(UsedByName, filename, "Maybe Mark ~p -> ~p\n",
		%%       [M, M#mod.is_included]),
                case M#mod.is_included of
                    true ->
                        %% Already marked
                        Acc;
                    false ->
                        %% Already marked
                        Acc;
                    undefined ->
                        %% Mark and propagate
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
                        %% io:format("Propagate mod: ~p -> ~p (~p)\n",
			%%           [UsedByName, ModName, M#mod.incl_cond]),
                        [A] = ets:lookup(C#common.app_tab, M2#mod.app_name),
                        Acc2 =
                            case A#app.is_included of
                                true ->
                                    Acc;
                                false ->
                                    Acc;
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
                                    %% io:format("Propagate app: ~p ~p -> ~p\n",
                                    %% [UsedByName, A#app.name,
				    %% [M3#mod.name || M3 <- Mods]]),
                                    A2 = A#app{is_included = true},
                                    ets:insert(C#common.app_tab, A2),
                                    mod_mark_is_included(C,
							 Sys,
							 ModName,
							 [M3#mod.name ||
							     M3 <- Mods],
							 Acc)
                            end,
                        mod_mark_is_included(C,
					     Sys,
					     ModName,
					     M2#mod.uses_mods,
					     Acc2)
                end;
            [] ->
                M = missing_mod(ModName, ?MISSING_APP_NAME),
                M2 = M#mod{is_included = true},
                ets:insert(C#common.mod_tab, M2),
                ets:insert(C#common.mod_used_by_tab, {UsedByName, ModName}),
                [M2 | Acc]
        end,
    mod_mark_is_included(C, Sys, UsedByName, ModNames, Acc3);
mod_mark_is_included(_C, _Sys, _UsedByName, [], Acc) ->
    Acc.

app_propagate_is_used_by(C, [#app{mods = Mods, name = Name} | Apps]) ->
    case Name =:= ?MISSING_APP_NAME of
        true -> ok;
        false -> ok
    end,
    mod_propagate_is_used_by(C, Mods),
    app_propagate_is_used_by(C, Apps);
app_propagate_is_used_by(_C, []) ->
    ok.

mod_propagate_is_used_by(C, [#mod{name = ModName} | Mods]) ->
    [M] = ets:lookup(C#common.mod_tab, ModName),
    case M#mod.is_included of
        true ->
            [ets:insert(C#common.mod_used_by_tab, {UsedModName, ModName}) ||
                UsedModName <- M#mod.uses_mods];
        false ->
            ignore;
        undefined ->
            ignore
    end,
     mod_propagate_is_used_by(C, Mods);
mod_propagate_is_used_by(_C, []) ->
    ok.

read_apps(C, Sys, [#app{mods = Mods, is_included = IsIncl} = A | Apps], Acc) ->
    {Mods2, IsIncl2} = read_apps(C, Sys, A, Mods, [], IsIncl),
    Status =
        case lists:keysearch(missing, #mod.status, Mods2) of
            {value, _} -> missing;
            false      -> ok
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
               status = Status,
               uses_mods = UsesMods2,
               used_by_mods = UsedByMods2,
               uses_apps = UsesApps2,
               used_by_apps = UsedByApps2,
               is_included = IsIncl2},
    read_apps(C, Sys, Apps, [A2 | Acc]);
read_apps(_C, _Sys, [], Acc) ->
    lists:reverse(Acc).

read_apps(C, Sys, A, [#mod{name = ModName} | Mods], Acc, IsIncl) ->
    [M2] = ets:lookup(C#common.mod_tab, ModName),
    Status = do_get_status(M2),
    %% print(M2#mod.name, hipe, "status -> ~p\n", [Status]),
    {IsIncl2, M3} =
        case M2#mod.is_included of
            true ->
                UsedByMods =
		    [N || {_, N} <- ets:lookup(C#common.mod_used_by_tab,
					       ModName)],
                {true, M2#mod{status = Status, used_by_mods = UsedByMods}};
            _    ->
                {IsIncl, M2#mod{status = Status, used_by_mods = []}}
        end,
    ets:insert(C#common.mod_tab, M3),
    read_apps(C, Sys, A, Mods, [M3 | Acc], IsIncl2);
read_apps(_C, _Sys, _A, [], Acc, IsIncl) ->
    {lists:reverse(Acc), IsIncl}.

do_get_status(M) ->
    if
        M#mod.exists =:= false, M#mod.is_included =/= false ->
            missing;
        true ->
            ok
    end.

shrink_sys(#state{sys = #sys{apps = Apps} = Sys} = S) ->
    Apps2 = lists:zf(fun filter_app/1, Apps),
    S#state{sys = Sys#sys{apps = Apps2}}.

filter_app(A) ->
    Mods = [M#mod{is_app_mod = undefined,
                  is_ebin_mod = undefined,
                  uses_mods = undefined,
                  exists = false} ||
               M <- A#app.mods,
               M#mod.incl_cond =/= undefined],
    if
	A#app.is_escript ->
	    {true, A#app{vsn = undefined,
                         label = undefined,
                         info = undefined,
                         mods = [],
                         uses_mods = undefined}};
        Mods =:= [],
        A#app.mod_cond =:= undefined,
        A#app.incl_cond =:= undefined,
        A#app.use_selected_vsn =:= undefined ->
            false;
        true ->
            {Dir, Dirs} =
                case A#app.use_selected_vsn of
                    undefined ->
			{shrinked, []};
                    false ->
			{shrinked, []};
                    true ->
			{A#app.active_dir, [A#app.active_dir]};
		    _ when A#app.is_escript ->
			{A#app.active_dir, [A#app.active_dir]}
                end,
            OptVsn =
                case A#app.use_selected_vsn of
                    undefined -> undefined;
                    false -> undefined;
                    true -> A#app.vsn
                end,
            {true, A#app{active_dir = Dir,
                         sorted_dirs = Dirs,
                         vsn = OptVsn,
                         label = undefined,
                         info = undefined,
                         mods = Mods,
                         uses_mods = undefined}}
    end.

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
                        {AI, read_ebin_mods(Ebin, AppName), Status2};
                    true ->
                        {App#app.info, Mods, Status}
                end,

            %% Add non-existing modules
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

            %% Add optional user config for each module
            Mods2 = add_mod_config(MissingMods ++ EbinMods, Mods),

            %% Set app flag for each module in app file
            Mods3 = set_mod_flags(Mods2, AppModNames),
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
                case lists:keysearch(Config#mod.name, #mod.name, Mods) of
                    {value, M} ->
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
    S2 =
        case InclDeriv of
            false -> shrink_sys(S);
            true  -> S
        end,
    reltool_target:gen_config(S2#state.sys, InclDef).

do_save_config(S, Filename, InclDef, InclDeriv) ->
    {ok, Config} = do_get_config(S, InclDef, InclDeriv),
    IoList = io_lib:format("%% config generated at ~w ~w\n~p.\n\n",
                           [date(), time(), Config]),
    file:write_file(Filename, IoList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_load_config(S, SysConfig) ->
    OldSys = S#state.sys,
    S2 = shrink_sys(S),
    ShrinkedSys = S2#state.sys,
    {NewSys, Status} =
	read_config(ShrinkedSys#sys{apps = []}, SysConfig, {ok, []}),
    case Status of
        {ok, _Warnings} ->
            Force = false,
            {MergedSys, Status2} = merge_config(OldSys, NewSys, Force, Status),
            {S3, Status3} =
		analyse(S2#state{sys = MergedSys, old_sys = OldSys}, Status2),
            S4 =
                case Status3 of
                    {ok, _Warnings2} ->
                        S3#state{status = Status3, old_status = S#state.status};
                    {error, _} ->
			%% Keep old state
                        S
                end,
            {S4, Status3};
        {error, _} ->
            %% Keep old state
            {S, Status}
    end.

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
	    case lists:keysearch(NewSys2#sys.boot_rel,
				 #rel.name,
				 NewSys2#sys.rels) of
		{value, _} ->
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
       [{escript, File, AppKeyVals} | SysKeyVals], Status)
  when is_list(File), is_list(AppKeyVals) ->
    {Name, Label} = split_escript_name(File),
    App = default_app(Name, File),
    App2 = App#app{is_escript = true,
		   label = Label,
		   info = missing_app_info(""),
		   active_dir = File,
		   sorted_dirs = [File]},
    {App3, Status2} = decode(App2, AppKeyVals, Status),
    decode(Sys#sys{apps = [App3 | Apps], escripts = [File | Escripts]},
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
            app_file when Val =:= keep; Val =:= strip, Val =:= all ->
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
    RA =
        case RelApp of
            Name when is_atom(Name) ->
                #rel_app{name = Name, app_type = undefined, incl_apps = []};
            {Name, Type} when is_atom(Name) ->
                #rel_app{name = Name, app_type = Type, incl_apps = []};
            {Name, InclApps} when is_atom(Name), is_list(InclApps) ->
                #rel_app{name = Name,
			 app_type = undefined,
			 incl_apps = InclApps};
            {Name, Type, InclApps} when is_atom(Name), is_list(InclApps) ->
                #rel_app{name = Name, app_type = Type, incl_apps = InclApps};
            _ ->
                #rel_app{incl_apps = []}
        end,
    IsType = is_type(RA#rel_app.app_type),
    NonAtoms = [IA || IA <- RA#rel_app.incl_apps, not is_atom(IA)],
    if
        IsType, NonAtoms =:= [] ->
            decode(Rel#rel{rel_apps = RelApps ++ [RA]}, KeyVals, Status);
        true ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

refresh(#state{sys = Sys} = S, Force, Status) ->
    {Sys2, Status2} = merge_config(Sys, Sys#sys{apps = []}, Force, Status),
    {S#state{sys = Sys2}, Status2}.

merge_config(OldSys, NewSys, Force, Status) ->
    RootDir = filename:absname(NewSys#sys.root_dir),
    LibDirs = [filename:absname(D) || D <- NewSys#sys.lib_dirs],
    Escripts = [filename:absname(E) || E <- NewSys#sys.escripts],
    {SourceDirs, Status2} =
        libs_to_dirs(RootDir, LibDirs, Status),
    MergedApps = merge_app_dirs(SourceDirs, NewSys#sys.apps, OldSys#sys.apps),
    {AllApps, Status3} =
        escripts_to_apps(Escripts, MergedApps, OldSys#sys.apps, Status2),
    {RefreshedApps, Status4} =
        refresh_apps(OldSys#sys.apps, AllApps, [], Force, Status3),
    {PatchedApps, Status5} =
	patch_erts_version(RootDir, RefreshedApps, Status4),
    Escripts2 = [A#app.active_dir || A <- PatchedApps, A#app.is_escript],
    NewSys2 = NewSys#sys{root_dir = RootDir,
                         lib_dirs = LibDirs,
                         escripts = Escripts2,
                         apps = PatchedApps},
    {NewSys2, Status5}.

verify_config(RelApps, #sys{boot_rel = BootRel, rels = Rels, apps = Apps}, Status) ->
    case lists:keymember(BootRel, #rel.name, Rels) of
        true ->
	    Status2 = lists:foldl(fun(RA, Acc) ->
					  check_app(RA, Apps, Acc) end,
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

check_app({RelName, AppName}, Apps, Status) ->
    case lists:keysearch(AppName, #app.name, Apps) of
	{value, App} when App#app.is_pre_included ->
	    Status;
	{value, App} when App#app.is_included ->
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

patch_erts_version(RootDir, Apps, Status) ->
    AppName = erts,
    case lists:keysearch(AppName, #app.name, Apps) of
        {value, Erts} ->
            LocalRoot = code:root_dir(),
            Vsn = Erts#app.vsn,
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

escripts_to_apps([Escript | Escripts], Apps, OldApps, Status) ->
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
	    {Apps2, Status3} =
		files_to_apps(Escript,
			      lists:sort(Files),
			      Apps,
			      Apps,
			      OldApps,
			      Status2),
	    escripts_to_apps(Escripts, Apps2, OldApps, Status3);
	{error, Reason} ->
	    Text = lists:flatten(io_lib:format("~p", [Reason])),
	    {[], reltool_utils:return_first_error(Status,
						  "Illegal escript " ++
						  Escript ++ ": " ++ Text)}
    end;
escripts_to_apps([], Apps, _OldApps, Status) ->
    {Apps, Status}.

%% Assume that all files for an app are in consecutive order
%% Assume the app info is before the mods
files_to_apps(Escript,
	      [{AppName, Type, Dir, ModOrInfo} | Files] = AllFiles,
	      Acc,
	      Apps,
	      OldApps,
	      Status) ->
    case Type of
        mod ->
            case Acc of
                [] ->
                    Info = missing_app_info(""),
                    {NewApp, Status2} =
			merge_escript_app(AppName,
					  Dir,
					  Info,
					  [ModOrInfo],
					  Apps,
					  OldApps,
					  Status),
                    files_to_apps(Escript,
				  AllFiles,
				  [NewApp | Acc],
				  Apps,
				  OldApps, Status2);
                [App | Acc2] when App#app.name =:= ModOrInfo#mod.app_name ->
                    App2 = App#app{mods = [ModOrInfo | App#app.mods]},
                    files_to_apps(Escript,
				  Files,
				  [App2 | Acc2],
				  Apps,
				  OldApps,
				  Status);
                [App | Acc2] ->
                    PrevApp = App#app{mods = lists:keysort(#mod.name,
							   App#app.mods)},
                    Info = missing_app_info(""),
                    {NewApp, Status2} =
			merge_escript_app(AppName,
					  Dir,
					  Info,
					  [ModOrInfo],
					  Apps,
					  OldApps,
					  Status),
                    files_to_apps(Escript,
				  Files,
				  [NewApp, PrevApp | Acc2],
				  Apps,
				  OldApps,
				  Status2)
            end;
        app ->
            {App, Status2} =
		merge_escript_app(AppName, Dir, ModOrInfo, [], Apps, OldApps,
				  Status),
	    files_to_apps(Escript, Files, [App | Acc], Apps, OldApps, Status2)
    end;
files_to_apps(_Escript, [], Acc, _Apps, _OldApps, Status) ->
    {lists:keysort(#app.name, Acc), Status}.

merge_escript_app(AppName, Dir, Info, Mods, Apps, OldApps, Status) ->
    case lists:keysearch(AppName, #app.name, OldApps) of
        {value, App} ->
            ok;
        false ->
	    App = default_app(AppName, Dir)
    end,
    App2 = App#app{is_escript = true,
		   label = filename:basename(Dir, ".escript"),
		   info = Info,
		   mods = Mods,
		   active_dir = Dir,
		   sorted_dirs = [Dir]},
    case lists:keysearch(AppName, #app.name, Apps) of
        {value, _} ->
            Error = lists:concat([AppName, ": Application name clash. ",
                                  "Escript ", Dir," contains application ",
				  AppName, "."]),
            {App2, reltool_utils:return_first_error(Status, Error)};
        false ->
            {App2, Status}
    end.

merge_app_dirs([{Name, Dir} | Rest], [App | Apps], OldApps)
  when App#app.name =:= Name ->
    %% Add new dir to app
    App2 = App#app{sorted_dirs = [Dir | App#app.sorted_dirs]},
    merge_app_dirs(Rest, [App2 | Apps], OldApps);
merge_app_dirs([{Name, Dir} | Rest], Apps, OldApps) ->
    %% Initate app
    Apps2 = sort_app_dirs(Apps),
    Apps4 =
        case lists:keysearch(Name, #app.name, Apps) of
            false ->
                case lists:keysearch(Name, #app.name, OldApps) of
                    {value, OldApp} when OldApp#app.active_dir =:= Dir ->
                        [OldApp | Apps2];
                    {value, OldApp} ->
                        App =
                            case filter_app(OldApp) of
                                {true, NewApp} ->
                                    NewApp#app{active_dir = Dir,
					       sorted_dirs = [Dir]};
                                false ->
                                    default_app(Name, Dir)
                            end,
                        [App | Apps2];
                    false ->
                        App = default_app(Name, Dir),
                        [App | Apps2]
                end;
            {value, OldApp} ->
                Apps3 = lists:keydelete(Name, #app.name, Apps2),
                App = OldApp#app{sorted_dirs = [Dir | OldApp#app.sorted_dirs]},
                [App | Apps3]
        end,
    merge_app_dirs(Rest, Apps4, OldApps);
merge_app_dirs([], Apps, _OldApps) ->
    Apps2 = sort_app_dirs(Apps),
    lists:reverse(Apps2).

sort_app_dirs([#app{sorted_dirs = Dirs} = App | Acc]) ->
    SortedDirs = lists:sort(fun reltool_utils:app_dir_test/2, Dirs),
    case SortedDirs of
        [ActiveDir | _] -> ok;
        [] -> ActiveDir = undefined
    end,
    [App#app{active_dir = ActiveDir, sorted_dirs = SortedDirs} | Acc];
sort_app_dirs([]) ->
    [].

default_app(Name, Dir) ->
    App = default_app(Name),
    App#app{active_dir = Dir,
            sorted_dirs = [Dir]}.

default_app(Name) ->
    #app{name = Name,
         is_escript = false,
         use_selected_vsn = undefined,
         active_dir = undefined,
         sorted_dirs = [],
         vsn = undefined,
         label = undefined,
         info = undefined,
         mods = [],

         mod_cond = undefined,
         incl_cond = undefined,

         status = missing,
         uses_mods = undefined,
         is_pre_included = undefined,
         is_included = undefined,
	 rels = undefined}.

%% Assume that the application are sorted
refresh_apps([Old | OldApps], [New | NewApps], Acc, Force, Status)
  when New#app.name =:= Old#app.name ->
    {Info, ActiveDir, Status2} = ensure_app_info(New, Status),
    OptLabel =
        case Info#app_info.vsn =:= New#app.vsn of
            true -> New#app.label;
            false -> undefined % Cause refresh
        end,
    {Refreshed, Status3} =
        refresh_app(New#app{label = OptLabel,
                            active_dir = ActiveDir,
                            vsn = Info#app_info.vsn,
                            info = Info},
                    Force,
                    Status2),
    refresh_apps(OldApps, NewApps, [Refreshed | Acc], Force, Status3);
refresh_apps([Old | OldApps], [New | NewApps], Acc, Force, Status)
  when New#app.name < Old#app.name ->
    %% No old app version exists. Use new as is.
    %% BUGBUG: Issue warning if the active_dir is not defined
    {New2, Status2} = refresh_app(New, Force, Status),
    refresh_apps([Old | OldApps], NewApps, [New2 | Acc], Force, Status2);
refresh_apps([Old | OldApps], [New | NewApps], Acc, Force, Status)
  when New#app.name > Old#app.name ->
    %% No new version. Remove the old.
    Status2 =
        case Old#app.name =:= ?MISSING_APP_NAME of
            true ->
                Status;
            false ->
                Warning =
		    lists:concat([Old#app.name,
				  ": The source dirs does not ",
				  "contain the application anymore."]),
                reltool_utils:add_warning(Status, Warning)
        end,
    refresh_apps(OldApps, [New | NewApps], Acc, Force, Status2);
refresh_apps([], [New | NewApps], Acc, Force, Status) ->
    %% No old app version exists. Use new as is.
    {New2, Status2} = refresh_app(New, Force, Status),
    refresh_apps([], NewApps, [New2 | Acc], Force, Status2);
refresh_apps([Old | OldApps], [], Acc, Force, Status) ->
    %% No new version. Remove the old.
    Status2 =
        case Old#app.name =:= ?MISSING_APP_NAME of
            true ->
                Status;
            false ->
                Warning =
		    lists:concat([Old#app.name,
				  ": The source dirs does not "
				  "contain the application anymore."]),
                reltool_utils:add_warning(Status, Warning)
        end,
    refresh_apps(OldApps, [], Acc, Force, Status2);
refresh_apps([], [], Acc, _Force, Status) ->
    {lists:reverse(Acc), Status}.

ensure_app_info(#app{is_escript = true, active_dir = Dir, info = Info},
		Status) ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sys callbacks

system_continue(_Parent, _Debug, S) ->
    ?MODULE:loop(S).

system_terminate(Reason, _Parent, _Debug, _S) ->
    exit(Reason).

system_code_change(S,_Module,_OldVsn,_Extra) ->
    {ok, S}.
