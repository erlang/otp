%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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
-define(MISSING_APP,      '*MISSING*').
-define(MISSING_APP_TEXT, "*MISSING*").

-record(common,
        {
          sys_debug,         % term()
          wx_debug,          % term()
          trap_exit,         % bool()
          app_tab,           % ets_tab()
          mod_tab,           % ets_tab()
          mod_used_by_tab    % ets_tab()
         }).

-record(sys,
        {
          %% Sources
          root_dir,         % directory()
          lib_dirs,         % [directory()]
          escripts,         % [file()]
          mod_cond,         % all | app | ebin | derived | none
          incl_cond,        % include | exclude | derived
          apps,             % [#app{}]

          %% Target cond
          boot_rel,         	% string()
          rels,             	% [#rel{}]
          emu_name,         	% string()
          profile,          	% standalone | development | embedded
          incl_sys_filters,     % [regexp()]
          excl_sys_filters,     % [regexp()]
          incl_app_filters,     % [regexp()]
          excl_app_filters,     % [regexp()]
          incl_archive_filters, % [regexp()]
          excl_archive_filters, % [regexp()]
          archive_opts,         % [zip:create()]
          relocatable,          % bool()
          app_type,             % permanent | transient | temporary | load | none
          app_file,             % keep | strip | all
          debug_info            % keep | strip
         }).           

-record(rel,
        {
          name,             % string()
          vsn,              % string()
          rel_apps          % [#rel_app{}]
         }).

-record(rel_app,
        {
          name,             % atom()
          app_type,         % permanent | transient | temporary | load | none
          incl_apps         % [atom()]
         }).

-record(app,
        {%% Static info
          name,            % atom()
          is_escript,      % bool()
          use_selected_vsn,% bool() | undefined
          active_dir,      % dir_name()
          sorted_dirs,     % [dir_name()]
          vsn,             % string() e.g. "4.7"
          label,           % string() e.g. "mnesia" or "mnesia-4.7"  
          info,            % #app_info{} | undefined
          mods,            % [#mod{}]

          %% Static source cond
          mod_cond,        % all | app | ebin | derived | none | undefined
          incl_cond,       % include | exclude | derived | undefined

          %% Static target cond
          debug_info,           % keep | strip | undefined
          app_file,             % keep | strip | all | undefined
          app_type,             % permanent | transient | temporary | load | none
          incl_app_filters,     % [regexp()]
          excl_app_filters,     % [regexp()]
          incl_archive_filters, % [regexp()]
          excl_archive_filters, % [regexp()]
          archive_opts,         % [zip_create_opt()]

          %% Dynamic
          status,          % missing | ok
          uses_mods,       % [atom()]
          used_by_mods,    % [atom()]
          uses_apps,       % [atom()]
          used_by_apps,    % [atom()]
          is_pre_included, % bool()
          is_included      % bool()
         }). 

-record(mod,
        {%% Static
          name,            % atom()
          app_name,        % atom()
          incl_cond,       % include | exclude | derived | undefined
          debug_info,      % keep | strip | undefined
          is_app_mod,      % bool(),
          is_ebin_mod,     % bool(),
          uses_mods,       % [module()]
          exists,          % bool()
          %% Dynamic
          status,          % missing | ok
          used_by_mods,    % [atom()]
          is_pre_included, % bool() | undefined
          is_included      % bool() | undefined
         }).

%% app      - Include all modules in app file
%% ebin     - Include all modules on ebin directory
%% derived  - Include only those modules that others are dependent on

-record(app_info,
        {
          description = "",
          id = "",
          vsn = "",
          modules = [],
          maxP = infinity,
          maxT = infinity,
          registered = [],
          incl_apps = [],
          applications = [],
          env = [],
          mod = undefined,
          start_phases = undefined
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
-define(DEFAULT_APP_TYPE,          permanent).
-define(DEFAULT_APP_FILE,          keep).
-define(DEFAULT_DEBUG_INFO,        keep).

-define(DEFAULT_INCL_ARCHIVE_FILTERS, [".*"]).
-define(DEFAULT_EXCL_ARCHIVE_FILTERS, ["^include$", "^priv$"]).
-define(DEFAULT_ARCHIVE_OPTS,      []).

-define(DEFAULT_INCL_SYS_FILTERS,    [".*"]).
-define(DEFAULT_EXCL_SYS_FILTERS,    []).
-define(DEFAULT_INCL_APP_FILTERS,    [".*"]).
-define(DEFAULT_EXCL_APP_FILTERS,    []).

-define(EMBEDDED_INCL_SYS_FILTERS,   ["^bin", 
				    "^erts",
				    "^lib",
				    "^releases"]).
-define(EMBEDDED_EXCL_SYS_FILTERS,    ["^bin/(erlc|dialyzer|typer)(|\\.exe)$",
				     "^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$",
				     "^erts.*/bin/.*(debug|pdb)"]).
-define(EMBEDDED_INCL_APP_FILTERS,    ["^ebin",
				     "^priv", 
				     "^include"]).
-define(EMBEDDED_EXCL_APP_FILTERS,    []).

-define(STANDALONE_INCL_SYS_FILTERS,  ["^bin/(erl|epmd)(|\\.exe|\\.ini)$",
				     "^bin/start(|_clean).boot$",
				     "^erts.*/bin",
				     "^lib$"]).
-define(STANDALONE_EXCL_SYS_FILTERS,  ["^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$",
				     "^erts.*/bin/(start|escript|to_erl|run_erl)(|\\.exe)$", 
				     "^erts.*/bin/.*(debug|pdb)"]).
-define(STANDALONE_INCL_APP_FILTERS,  ["^ebin", 
				     "^priv"]).
-define(STANDALONE_EXCL_APP_FILTERS,  ["^ebin/.*\\.appup$"]).
