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

-define(APPLICATION,      reltool).
-define(MISSING_APP_NAME, '*MISSING*').
-define(MISSING_APP_TEXT, "*MISSING*").

-type file()             :: file:filename().
-type dir()              :: file:filename().
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
-type escript_app_name() :: app_name().
-type mod_name()         :: atom().
-type app_name()         :: atom().
-type app_vsn()          :: string(). % e.g. "4.7"
-type app_label()        :: string(). % e.g. "mnesia" or "mnesia-4.7"
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
			  | {lib_dir, lib_dir()}
                          | {mod, mod_name(), [mod()]}
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
			  | {excl_lib, excl_lib()}
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
-type option()           :: {wx_debug, term()}
			  | {trap_exit, boolean()}
			  | config()
			  | {config, config() | file()}.
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
-type target_dir()       :: dir().
-type incl_defaults()    :: boolean().
-type incl_derived()     :: boolean().
-type status()           :: missing | ok.
-type excl_lib()         :: otp_root.

-record(common,
        {
          sys_debug 	  :: term(),
          wx_debug  	  :: term(),
          trap_exit 	  :: boolean()
	}).

%% Types '$1','$2' and '_' are needed for match specs to ets:select
-record(mod,
        { %% Static
          name        :: '$1' | mod_name(),
          app_name    :: '_'  | app_name(),
          incl_cond   :: '_'  | incl_cond() | undefined,
          debug_info  :: '_'  | debug_info() | undefined,
          is_app_mod  :: '_'  | boolean() | undefined,
          is_ebin_mod :: '_'  | boolean() | undefined,
          uses_mods   :: '$2' | [mod_name()] | undefined,
          exists      :: '_'  | boolean() | undefined,
          %% Dynamic
          status = ok       :: '_' | status(),
          used_by_mods = [] :: '_' | [mod_name()],
          is_pre_included   :: '_' | boolean() | undefined,
          is_included       :: '_' | boolean() | undefined
	}).

-record(app_info,
        {
          description  = ""        :: '_' | string(),
          id           = ""        :: '_' | string(),
          vsn          = ""        :: '_' | app_vsn(),
          modules      = []        :: '_' | [mod_name()],
          maxP         = infinity  :: '_' | integer() | infinity,
          maxT         = infinity  :: '_' | integer() | infinity,
          registered   = []        :: '_' | [atom()],
          incl_apps    = []        :: '_' | '$3' | [app_name()],
          applications = []        :: '_' | '$2' | [app_name()],
          env          = []        :: '_' | [{atom(), term()}],
          mod          = undefined :: '_' | {mod_name(), [term()]} | undefined,
          start_phases = undefined :: '_' | [{atom(), term()}] | undefined,
          runtime_dependencies = [] :: '_' | [string()]
	}).

-record(regexp, {source, compiled}).

%% Types '$1','$2' and '_' are needed for match specs to ets:select
-record(app,
        { %% Static info
          name             :: '_' | app_name(),
          is_escript       :: '_' | boolean() | {inlined, escript_app_name()},
          use_selected_vsn :: '_' | vsn | dir | undefined,
          active_dir       :: '_' | dir() | undefined,
          sorted_dirs      :: '_' | [dir()],
          vsn              :: '_' | app_vsn() | undefined,
          label            :: '_' | app_label() | undefined,
          info             :: '_' | #app_info{} | undefined,
          mods             :: '_' | [#mod{}],

          %% Static source cond
          mod_cond  :: '_' | mod_cond()  | undefined,
          incl_cond :: '_' | incl_cond() | undefined,

          %% Static target cond
          debug_info            :: '_' | debug_info() | undefined,
          app_file              :: '_' | app_file() | undefined,
          app_type              :: '_' | app_type() | undefined,
          incl_app_filters      :: '_' | [#regexp{}] | undefined,
          excl_app_filters      :: '_' | [#regexp{}] | undefined,
          incl_archive_filters  :: '_' | [#regexp{}] | undefined,
          excl_archive_filters  :: '_' | [#regexp{}] | undefined,
          archive_opts          :: '_' | [archive_opt()] | undefined,

          %% Dynamic
          status          :: '_' | status(),
          uses_mods       :: '_' | [mod_name()] | undefined,
          used_by_mods    :: '_' | [mod_name()] | undefined,
          uses_apps       :: '_' | [app_name()] | undefined,
          used_by_apps    :: '_' | [app_name()] | undefined,
          is_pre_included :: '_' | '$2' | boolean() | undefined,
          is_included     :: '_' | '$1' | boolean() | undefined,
          rels            :: '_' | [rel_name()] | undefined
	}).

-record(rel_app,
        {
          name           :: app_name(),
          app_type       :: app_type() | undefined,
          incl_apps      :: [incl_app()] | undefined
        }).

-record(rel,
        {
          name     :: rel_name(),
          vsn      :: rel_vsn(),
          rel_apps :: [#rel_app{}]
	}).

-record(sys,
        { %% Sources
          root_dir  :: dir(),
          lib_dirs  :: [dir()],
          escripts  :: [file()],
          mod_cond  :: mod_cond(),
          incl_cond :: incl_cond(),
          apps      :: [#app{}] | undefined,

          %% Target cond
          boot_rel 	       :: boot_rel(),
          rels     	       :: [#rel{}],
          emu_name 	       :: emu_name(),
          profile  	       :: profile(),
	  excl_lib             :: excl_lib() | undefined,
          incl_sys_filters     :: [#regexp{}],
          excl_sys_filters     :: [#regexp{}],
          incl_app_filters     :: [#regexp{}],
          excl_app_filters     :: [#regexp{}],
          incl_archive_filters :: [#regexp{}],
          excl_archive_filters :: [#regexp{}],
          archive_opts         :: [archive_opt()],
          relocatable          :: boolean(),
          rel_app_type         :: app_type(),
          embedded_app_type    :: app_type() | undefined,
          app_file             :: app_file(),
          debug_info           :: debug_info()
	}).

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
-define(DEFAULT_ARCHIVE_OPTS,         []).

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
