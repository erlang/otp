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

-define(APPLICATION,      reltool).
-define(MISSING_APP_NAME, '*MISSING*').
-define(MISSING_APP_TEXT, "*MISSING*").

-type file()             :: string().
-type dir()              :: string().
%% app      - Include all modules in app file
%% ebin     - Include all modules on ebin directory
%% derived  - Include only those modules that others are dependent on
-type mod_cond()         :: all | app | ebin | derived | none.
-type incl_cond()        :: include | exclude | derived.
-type debug_info()       :: keep | strip.
-type app_file()         :: keep | strip | all.
-type re_regexp()        :: string(). % re:regexp()
-type regexps()          :: [re_regexp()] |
			    {add, [re_regexp()]} |
			    {del, [re_regexp()]} .
-type incl_sys_filters() :: regexps().
-type excl_sys_filters() :: regexps().
-type incl_app_filters() :: regexps().
-type excl_app_filters() :: regexps().
-type incl_archive_filters() :: regexps().
-type excl_archive_filters() :: regexps().
-type archive_opt()      :: term(). % zip:create()
-type root_dir()         :: dir().
-type lib_dir()          :: dir().
-type profile()          :: development | embedded | standalone.
-type relocatable()      :: boolean().
-type escript_file()     :: file().
-type mod_name()         :: atom().
-type app_name()         :: atom().
-type app_vsn()          :: string(). % e.g. "4.7"
-type app_label()        :: string().% e.g. "mnesia" or "mnesia-4.7"
-type app_type()         :: permanent | transient | temporary | load | none.
-type incl_app()         :: app_name().
-type emu_name()         :: string().
-type rel_name()         :: string().
-type rel_vsn()          :: string().
-type boot_rel()         :: rel_name().
-type rel_app()          :: app_name()
                          | {app_name(), app_type()}
                          | {app_name(), [incl_app()]}
                          | {app_name(), app_type(), [incl_app()]}.
-type mod()              :: {incl_cond, incl_cond()}
                          | {debug_info, debug_info()}.
-type app()              :: {vsn, app_vsn()}
                          | {mod, mod_name(), mod()}
                          | {mod_cond, mod_cond()}
                          | {incl_cond, incl_cond()}
                          | {app_file, app_file()}
                          | {debug_info, debug_info()}
                          | {incl_app_filters, incl_app_filters()}
                          | {excl_app_filters, excl_app_filters()}
                          | {incl_archive_filters, incl_archive_filters()}
                          | {excl_archive_filters, excl_archive_filters()}.
-type escript()          :: {incl_cond, incl_cond()}.
-type sys()              :: {mod_cond, mod_cond()}
                          | {incl_cond, incl_cond()}
                          | {debug_info, debug_info()}
                          | {app_file, app_file()}
                          | {profile, profile()}
                          | {incl_sys_filters, incl_sys_filters()}
                          | {excl_sys_filters, excl_sys_filters()}
                          | {incl_app_filters, incl_app_filters()}
                          | {excl_app_filters, excl_app_filters()}
                          | {incl_archive_filters, incl_archive_filters()}
                          | {excl_archive_filters, excl_archive_filters()}
                          | {archive_opts, [archive_opt()]}
                          | {root_dir, root_dir()}
                          | {lib_dirs, [lib_dir()]}
                          | {boot_rel, boot_rel()}
                          | {rel, rel_name(), rel_vsn(), [rel_app()]}
                          | {relocatable, relocatable()}
                          | {erts, app()}
                          | {escript, escript_file(), [escript()]}
                          | {app, app_name(), [app()]}.
-type config()           :: {sys, [sys()]}.
-type option()           :: {wx_debug, term()} |
			    {trap_exit, boolean()} |
			    config() |
			    {config, config() | file()}.
-type options()          :: [option()].
-type server_pid()       :: pid().
-type window_pid()       :: pid().
-type server()           :: server_pid() | options().
-type rel_file()         :: term().
-type script_file()      :: term().
-type reason()           :: string().

-type base_dir()         :: dir().
-type base_file()        :: file().
-type top_dir()          :: file().
-type top_file()         :: file().
-type target_spec()      :: [target_spec()]
                         | {create_dir, base_dir(), [target_spec()]}
                         | {create_dir, base_dir(), top_dir(), [target_spec()]}
                         | {archive, base_file(), [archive_opt()], [target_spec()]}
                         | {copy_file, base_file()}
                         | {copy_file, base_file(), top_file()}
                         | {write_file, base_file(), iolist()}
                         | {strip_beam_file, base_file()}.
-type target_dir()      :: dir().
-type incl_defaults()   :: boolean().
-type incl_derived()    :: boolean().
-type ets_tab()         :: term().
-type status()          :: missing | ok.

-record(common,
        {
          sys_debug 	  :: term(),
          wx_debug  	  :: term(),
          trap_exit 	  :: boolean(),
          app_tab   	  :: ets_tab(),
          mod_tab   	  :: ets_tab(),
          mod_used_by_tab :: ets_tab()
         }).


-record(mod,
        {%% Static
          name        :: mod_name(),
          app_name    :: app_name(),
          incl_cond   :: incl_cond() | undefined,
          debug_info  :: debug_info() | undefined,
          is_app_mod  :: boolean(),
          is_ebin_mod :: boolean(),
          uses_mods   :: [mod_name()],
          exists      :: boolean(),

          %% Dynamic
          status          :: status(),
          used_by_mods    :: [mod_name()],
          is_pre_included :: boolean() | undefined,
          is_included     :: boolean() | undefined
         }).

-record(app_info,
        {
          description  = ""        :: string(),
          id           = ""        :: string(),
          vsn          = ""        :: app_vsn(),
          modules      = []        :: [mod_name()],
          maxP         = infinity  :: integer() | infinity,
          maxT         = infinity  :: integer() | infinity,
          registered   = []        :: [atom()],
          incl_apps    = []        :: [app_name()],
          applications = []        :: [app_name()],
          env          = []        :: [{atom(), term()}],
          mod          = undefined :: {mod_name(), [term()]} | undefined,
          start_phases = undefined :: [{atom(), term()}] | undefined
         }).

-record(app,
        {%% Static info
          name             :: app_name(),
          is_escript       :: boolean(),
          use_selected_vsn :: boolean() | undefined,
          active_dir       :: dir(),
          sorted_dirs      :: [dir()],
          vsn              :: app_vsn(),
          label            :: app_label(),
          info             :: #app_info{} | undefined,
          mods             :: [#mod{}],

          %% Static source cond
          mod_cond  :: mod_cond()  | undefined,
          incl_cond :: incl_cond() | undefined,

          %% Static target cond
          debug_info            :: debug_info() | undefined,
          app_file              :: app_file() | undefined,
          app_type              :: app_type() | undefined,
          incl_app_filters      :: incl_app_filters(),
          excl_app_filters      :: excl_app_filters(),
          incl_archive_filters  :: incl_archive_filters(),
          excl_archive_filters  :: excl_archive_filters(),
          archive_opts          :: [archive_opt()],

          %% Dynamic
          status          :: status(),
          uses_mods       :: [mod_name()],
          used_by_mods    :: [mod_name()],
          uses_apps       :: [app_name()],
          used_by_apps    :: [app_name()],
          is_pre_included :: boolean(),
          is_included     :: boolean(),
          rels            :: [rel_name()]
         }).

-record(rel_app,
        {
          name      :: app_name(),
          app_type  :: app_type(),
          incl_apps :: [incl_app()]
        }).

-record(rel,
        {
          name     :: rel_name(),
          vsn      :: rel_vsn(),
          rel_apps :: [#rel_app{}]
         }).

-record(sys,
        {
          %% Sources
          root_dir  :: dir(),
          lib_dirs  :: [dir()],
          escripts  :: [file()],
          mod_cond  :: mod_cond(),
          incl_cond :: incl_cond(),
          apps      :: [#app{}],

          %% Target cond
          boot_rel 	       :: boot_rel(),
          rels     	       :: [#rel{}],
          emu_name 	       :: emu_name(),
          profile  	       :: profile(),
          incl_sys_filters     :: incl_sys_filters(),
          excl_sys_filters     :: excl_sys_filters(),
          incl_app_filters     :: incl_app_filters(),
          excl_app_filters     :: excl_app_filters(),
          incl_archive_filters :: incl_archive_filters(),
          excl_archive_filters :: excl_archive_filters(),
          archive_opts         :: [archive_opt()],
          relocatable          :: boolean(),
          rel_app_type         :: app_type(),
          embedded_app_type    :: app_type() | undefined,
          app_file             :: app_file(),
          debug_info           :: debug_info()
         }).

-record(regexp, {source, compiled}).

-define(ERR_IMAGE,    0).
-define(WARN_IMAGE,   1).
-define(QUEST_IMAGE,  2).
-define(TICK_IMAGE,   3).
-define(CROSS_IMAGE,  4).
-define(SOURCE_IMAGE, 5).

-define(KEYSEARCH(Key, Pos, List),
        reltool_utils:safe_keysearch(Key, Pos, List, ?MODULE, ?LINE)).

-define(DEFAULT_LIBS,              []).
-define(DEFAULT_APPS,              []).
-define(DEFAULT_INCL_COND,         derived).
-define(DEFAULT_MOD_COND,          all).
-define(DEFAULT_REL_NAME,          "start_clean").
-define(DEFAULT_EMU_NAME,          "beam").
-define(DEFAULT_PROFILE,           development).
-define(DEFAULT_RELOCATABLE,       true).
-define(DEFAULT_REL_APP_TYPE,      permanent).
-define(DEFAULT_EMBEDDED_APP_TYPE, undefined).
-define(DEFAULT_APP_FILE,          keep).
-define(DEFAULT_DEBUG_INFO,        keep).

-define(DEFAULT_INCL_ARCHIVE_FILTERS, [".*"]).
-define(DEFAULT_EXCL_ARCHIVE_FILTERS, ["^include\$", "^priv\$"]).
-define(DEFAULT_ARCHIVE_OPTS,      []).

-define(DEFAULT_INCL_SYS_FILTERS,    [".*"]).
-define(DEFAULT_EXCL_SYS_FILTERS,    []).
-define(DEFAULT_INCL_APP_FILTERS,    [".*"]).
-define(DEFAULT_EXCL_APP_FILTERS,    []).

-define(EMBEDDED_INCL_SYS_FILTERS,   ["^bin",
				      "^erts",
				      "^lib",
				      "^releases"]).
-define(EMBEDDED_EXCL_SYS_FILTERS,
	["^bin/(erlc|dialyzer|typer)(|\\.exe)\$",
	 "^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)\$",
	 "^erts.*/bin/.*(debug|pdb)"]).
-define(EMBEDDED_INCL_APP_FILTERS,    ["^ebin",
				       "^include",
				       "^priv"]).
-define(EMBEDDED_EXCL_APP_FILTERS,    []).
-define(EMBEDDED_APP_TYPE,            load).

-define(STANDALONE_INCL_SYS_FILTERS,  ["^bin/(erl|epmd)(|\\.exe|\\.ini)\$",
				       "^bin/start(|_clean).boot\$",
				       "^erts.*/bin",
				       "^lib\$"]).
-define(STANDALONE_EXCL_SYS_FILTERS,
	["^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)\$",
	 "^erts.*/bin/(start|escript|to_erl|run_erl)(|\\.exe)\$",
	 "^erts.*/bin/.*(debug|pdb)"]).
-define(STANDALONE_INCL_APP_FILTERS,  ["^ebin",
				     "^priv"]).
-define(STANDALONE_EXCL_APP_FILTERS,  ["^ebin/.*\\.appup\$"]).
