%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose:
%% 
%% Test:
%% ts:run().
%% ts:run(snmp, [batch]).
%% ts:run(snmp, snmp_manager_config_test, [batch]).
%% 
%%----------------------------------------------------------------------
-module(snmp_manager_config_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/src/manager/snmpm_usm.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% -compile(export_all).

-export([
	all/0,groups/0,init_per_group/2,end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

	

	 simple_start_and_stop/1,
	 start_without_mandatory_opts1/1,
	 start_without_mandatory_opts2/1,
	 start_with_all_valid_opts/1,
	 start_with_unknown_opts/1,
	 start_with_incorrect_opts/1,
	 start_with_invalid_manager_conf_file1/1,
	 start_with_invalid_users_conf_file1/1,
	 start_with_invalid_agents_conf_file1/1,
	 start_with_invalid_usm_conf_file1/1,
         start_with_create_db_and_dir_opt/1,

	

	
	 simple_system_op/1,

	
	 register_user_using_file/1,
	 register_user_using_function/1,
	 register_user_failed_using_function1/1,

	
	 register_agent_using_file/1,
	 register_agent_using_function/1,
	 register_agent_failed_using_function1/1,

	
	 register_usm_user_using_file/1,
	 register_usm_user_using_function/1,
	 register_usm_user_failed_using_function1/1,
	 update_usm_user_info/1, 

	
	 create_and_increment/1,

	
	 stats_create_and_increment/1,

	
	 otp_7219/1, 
	 
	 otp_8395_1/1, 
	 otp_8395_2/1, 
	 otp_8395_3/1, 
	 otp_8395_4/1

	]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%======================================================================
%% External functions
%%======================================================================

init_per_testcase(Case, Config) when is_list(Config) ->
    p("init_per_testcase -> Case: ~p", [Case]),
    SnmpPrivDir = ?config(priv_dir, Config),
    p("init_per_testcase -> SnmpPrivDir: ~p", [SnmpPrivDir]),
    SuiteDir = atom_to_list(?MODULE),
    SuiteTopDir = filename:join(SnmpPrivDir, SuiteDir),
    case file:make_dir(SuiteTopDir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	{error, Reason} ->
	    ?FAIL({failed_creating, SuiteTopDir, Reason})
    end,
    p("init_per_testcase -> SuiteTopDir: ~p", [SuiteTopDir]),
    CaseDir = atom_to_list(Case),
    ?line ok = 
	file:make_dir(CaseTopDir = filename:join(SuiteTopDir, CaseDir)),
    p("init_per_testcase -> CaseTopDir: ~p", [CaseTopDir]),
    ?line ok = 
	file:make_dir(MgrTopDir  = filename:join(CaseTopDir, "manager/")),
    ?line ok = 
	file:make_dir(MgrConfDir = filename:join(MgrTopDir,   "conf/")),
    MgrDbDir = filename:join(MgrTopDir, "db/"),
    case Case of
	start_with_create_db_and_dir_opt ->
	    ok;
	_ ->
	    ?line ok = file:make_dir(MgrDbDir)
    end,
    ?line ok = 
	file:make_dir(MgrLogDir  = filename:join(MgrTopDir,   "log/")),
    [{case_top_dir,     CaseTopDir},
     {manager_dir,      MgrTopDir},
     {manager_conf_dir, MgrConfDir},
     {manager_db_dir,   MgrDbDir},
     {manager_log_dir,  MgrLogDir} | Config].


end_per_testcase(Case, Config) when is_list(Config) ->
    p("end_per_testcase -> Case: ~p", [Case]),
    %% The cleanup is removed due to some really discusting NFS behaviour...
    %% CaseTopDir = ?config(manager_dir, Config),
    %% ?line ok = ?DEL_DIR(CaseTopDir),
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================
% all(doc) ->
%     "The top snmp manager config test case";
all() -> 
[{group, start_and_stop}, {group, normal_op},
 {group, tickets}].

groups() -> 
    [{start_and_stop, [],
      [simple_start_and_stop, 
       start_without_mandatory_opts1,
       start_without_mandatory_opts2,
       start_with_all_valid_opts, start_with_unknown_opts,
       start_with_incorrect_opts,
       start_with_create_db_and_dir_opt,
       start_with_invalid_manager_conf_file1,
       start_with_invalid_users_conf_file1,
       start_with_invalid_agents_conf_file1,
       start_with_invalid_usm_conf_file1]},
     {normal_op, [],
      [{group, system}, 
       {group, agents}, 
       {group, users},
       {group, usm_users}, 
       {group, counter},
       {group, stats_counter}]},
     {system, [], [simple_system_op]},
     {users, [],
      [register_user_using_file, 
       register_user_using_function,
       register_user_failed_using_function1]},
     {agents, [],
      [register_agent_using_file,
       register_agent_using_function,
       register_agent_failed_using_function1]},
     {usm_users, [],
      [register_usm_user_using_file,
       register_usm_user_using_function,
       register_usm_user_failed_using_function1,
       update_usm_user_info]},
     {counter, [], [create_and_increment]},
     {stats_counter, [], [stats_create_and_increment]},
     {tickets, [], [otp_7219, {group, otp_8395}]},
     {otp_8395, [],
      [otp_8395_1, otp_8395_2, otp_8395_3, otp_8395_4]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.



%%======================================================================
%% Test functions
%%======================================================================



%% 
%% ---
%% 

simple_start_and_stop(suite) -> [];
simple_start_and_stop(doc) ->
    "Start the snmp manager config process with the \n"
	"minimum setof options (config dir).";
simple_start_and_stop(Conf) when is_list(Conf) ->
    put(tname,ssas),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?line {ok, _Pid} = snmpm_config:start_link(Opts),
    ?line ok = snmpm_config:stop(),

    ok.


%% 
%% ---
%% 

start_without_mandatory_opts1(suite) -> [];
start_without_mandatory_opts1(doc) ->
    "Start the snmp manager config process with some of the \n"
	"mandatory options missing.";
start_without_mandatory_opts1(Conf) when is_list(Conf) ->
    put(tname,swomo1),
    put(verbosity,trace),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),
    

    %% config, but no dir:
    p("config option, but no dir"),
    Opts = [{priority, normal}, 
	    {config, [{verbosity, trace}, {db_dir, DbDir}]}, {mibs, []}],
    ?line {error, {missing_mandatory,dir}} = config_start(Opts),

    p("done"),
    ok.


%% 
%% ---
%% 

start_without_mandatory_opts2(suite) -> [];
start_without_mandatory_opts2(doc) ->
    "Start the snmp manager config process with some of the \n"
	"mandatory options missing.";
start_without_mandatory_opts2(Conf) when is_list(Conf) ->
    put(tname,swomo2),
    put(verbosity,trace),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),

    write_manager_conf(ConfDir),
    

    %% Second set of options (no config):
    p("no config option"),
    Opts = [{priority, normal}, 
	    {mibs, []}],
    ?line {error, {missing_mandatory,config,[dir, db_dir]}} = 
	config_start(Opts),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_all_valid_opts(suite) -> [];
start_with_all_valid_opts(doc) ->
    "Start the snmp manager config process with the \n"
	"complete set of all the valid options.";
start_with_all_valid_opts(Conf) when is_list(Conf) ->
    put(tname,swavo),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),
    LogDir  = ?config(manager_log_dir, Conf),
    StdMibDir = filename:join(code:priv_dir(snmp), "mibs") ++ "/",

    write_manager_conf(ConfDir),
    

    %% Third set of options (no versions):
    p("all options"),
    NetIfOpts  = [{module,    snmpm_net_if}, 
		  {verbosity, trace},
		  {options,   [{recbuf,   30000},
			       {bind_to,  false},
			       {no_reuse, false}]}],
    ServerOpts = [{timeout, 10000}, {verbosity, trace}],
    NoteStoreOpts = [{timeout, 20000}, {verbosity, trace}],
    ConfigOpts = [{dir, ConfDir}, {verbosity, trace},
                  {db_dir, DbDir}, {db_init_error, create}],
    Mibs = [join(StdMibDir, "SNMP-NOTIFICATION-MIB"),
	    join(StdMibDir, "SNMP-USER-BASED-SM-MIB")],
    Prio = normal,
    ATL  = [{type,   read_write}, 
	    {dir,    LogDir}, 
	    {size,   {10,10240}},
	    {repair, true}],
    Vsns = [v1,v2,v3],
    Opts = [{config,          ConfigOpts},
	    {net_if,          NetIfOpts},
	    {server,          ServerOpts},
	    {note_store,      NoteStoreOpts},
	    {audit_trail_log, ATL},
	    {priority,        Prio}, 
	    {mibs,            Mibs},
	    {versions,        Vsns}],
    ?line {ok, _Pid} = config_start(Opts),
    ?line ok = config_stop(),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_unknown_opts(suite) -> [];
start_with_unknown_opts(doc) ->
    "Start the snmp manager config process when some of\n"
	"the options are unknown.";
start_with_unknown_opts(Conf) when is_list(Conf) ->
    put(tname,swuo),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),
    LogDir  = ?config(manager_log_dir, Conf),
    StdMibDir = filename:join(code:priv_dir(snmp), "mibs") ++ "/",

    write_manager_conf(ConfDir),
    

    %% Third set of options (no versions):
    p("all options"),
    NetIfOpts  = [{module,    snmpm_net_if}, 
		  {verbosity, trace},
		  {options,   [{recbuf,   30000},
			       {bind_to,  false},
			       {no_reuse, false}]}],
    ServerOpts = [{timeout, 10000}, {verbosity, trace}],
    NoteStoreOpts = [{timeout, 20000}, {verbosity, trace}],
    ConfigOpts = [{dir, ConfDir}, {verbosity, trace}, {db_dir, DbDir}],
    Mibs = [join(StdMibDir, "SNMP-NOTIFICATION-MIB"),
	    join(StdMibDir, "SNMP-USER-BASED-SM-MIB")],
    Prio = normal,
    ATL  = [{type,   read_write}, 
	    {dir,    LogDir}, 
	    {size,   {10,10240}},
	    {repair, true}],
    Vsns = [v1,v2,v3],
    Opts = [{config,          ConfigOpts},
	    {net_if,          NetIfOpts},
	    {server,          ServerOpts},
	    {note_store,      NoteStoreOpts},
	    {audit_trail_log, ATL},
	    {unknown_option,  "dummy value"},
	    {priority,        Prio}, 
	    {mibs,            Mibs},
	    {versions,        Vsns}],
    ?line {ok, _Pid} = config_start(Opts),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_incorrect_opts(suite) -> [];
start_with_incorrect_opts(doc) ->
    "Start the snmp manager config process when some of\n"
	"the options has incorrect values.";
start_with_incorrect_opts(Conf) when is_list(Conf) -> 
    put(tname,swio),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),
    LogDir  = ?config(manager_log_dir, Conf),
    StdMibDir = filename:join(code:priv_dir(snmp), "mibs") ++ "/",

    write_manager_conf(ConfDir),
    
    ConfigOpts = [{verbosity,trace}, {dir, ConfDir}, {db_dir, DbDir}],

    p("net-if - incorrect module"),
    NetIfOpts1 = [{module, snmpm_user}],  %% Behaviour check will fail
    Opts01     = [{config, ConfigOpts}, {versions, [v1]}, 
		   {net_if, NetIfOpts1}],
    ?line {error, Reason01} = config_start(Opts01),
    p("net-if (module) res: ~p", [Reason01]),
    
    p("net-if - incorrect verbosity"),
    NetIfOpts2  = [{verbosity, invalid_verbosity}],
    Opts02      = [{config, ConfigOpts}, {versions, [v1]}, 
		   {net_if, NetIfOpts2}],
    ?line {error, Reason02} = config_start(Opts02),
    p("net-if (verbosity) res: ~p", [Reason02]),

    p("net-if - incorrect options"),
    NetIfOpts3 = [{options, invalid_options}],
    Opts03     = [{config, ConfigOpts}, {versions, [v1]}, 
		  {net_if, NetIfOpts3}],
    ?line {error, Reason03} = config_start(Opts03),
    p("net-if (options) res: ~p", [Reason03]),
		   
    p("server - incorrect timeout (1)"),
    ServerOpts1 = [{timeout, invalid_timeout}],
    Opts08      = [{config, ConfigOpts}, {versions, [v1]}, 
		   {server, ServerOpts1}],
    ?line {error, Reason08} = config_start(Opts08),
    p("server (timeout) res: ~p", [Reason08]),

    p("server - incorrect timeout (2)"),
    ServerOpts2 = [{timeout, 0}],
    Opts09      = [{config, ConfigOpts}, {versions, [v1]}, 
		   {server, ServerOpts2}],
    ?line {error, Reason09} = config_start(Opts09),
    p("server (timeout) res: ~p", [Reason09]),

    p("server - incorrect timeout (3)"),
    ServerOpts3 = [{timeout, -1000}],
    Opts10      = [{config, ConfigOpts}, 
		   {versions, [v1]}, 
		   {server, ServerOpts3}],
    ?line {error, Reason10} = config_start(Opts10),
    p("server (timeout) res: ~p", [Reason10]),

    p("server - incorrect verbosity"),
    ServerOpts4 = [{verbosity, invalid_verbosity}],
    Opts11      = [{config, ConfigOpts}, 
		   {versions, [v1]}, 
		   {server, ServerOpts4}],
    ?line {error, Reason11} = config_start(Opts11),
    p("server (verbosity) res: ~p", [Reason11]),

    p("note-store - incorrect timeout (1)"),
    NoteStoreOpts1 = [{timeout, invalid_timeout}],
    Opts12         = [{config, ConfigOpts}, 
		      {versions, [v1]}, 
		      {note_store, NoteStoreOpts1}],
    ?line {error, Reason12} = config_start(Opts12),
    p("note-store (timeout) res: ~p", [Reason12]),

    p("note-store - incorrect timeout (2)"),
    NoteStoreOpts2 = [{timeout, 0}],
    Opts13         = [{config, ConfigOpts}, 
		      {versions, [v1]}, 
		      {note_store, NoteStoreOpts2}],
    ?line {error, Reason13} = config_start(Opts13),
    p("note-store (timeout) res: ~p", [Reason13]),

    p("note-store - incorrect timeout (3)"),
    NoteStoreOpts3 = [{timeout, -2000}],
    Opts14         = [{config, ConfigOpts}, 
		      {versions, [v1]}, 
		      {note_store, NoteStoreOpts3}],
    ?line {error, Reason14} = config_start(Opts14),
    p("note-store (timeout) res: ~p", [Reason14]),

    p("note-store - incorrect verbosity"),
    NoteStoreOpts4 = [{timeout, 20000}, {verbosity, invalid_verbosity}],
    Opts15         = [{config, ConfigOpts}, 
		      {versions, [v1]}, 
		      {note_store, NoteStoreOpts4}],
    ?line {error, Reason15} = config_start(Opts15),
    p("note-store (verbosity) res: ~p", [Reason15]),

    p("config - incorrect dir (1)"),
    ConfigOpts1 = [{dir, invalid_dir}],
    Opts16      = [{config, ConfigOpts1}, 
		   {versions, [v1]}],
    ?line {error, Reason16} = config_start(Opts16),
    p("config (dir) res: ~p", [Reason16]),

    p("config - incorrect dir (2)"),
    ConfigOpts2 = [{dir, "/invalid/dir"}],
    Opts17      = [{config, ConfigOpts2}, 
		   {versions, [v1]}],
    ?line {error, Reason17} = config_start(Opts17),
    p("config (dir) res: ~p", [Reason17]),

    p("config - incorrect verbosity"),
    ConfigOpts3 = [{dir, ConfDir}, {verbosity, invalid_verbosity}],
    Opts18      = [{config, ConfigOpts3}, 
		   {versions, [v1]}],
    ?line {error, Reason18} = config_start(Opts18),
    p("config (verbosity) res: ~p", [Reason18]),

    p("mibs - incorrect mibs (1)"),
    Mibs1  = invalid_mibs,
    Opts19 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {mibs, Mibs1}],
    ?line {error, Reason19} = config_start(Opts19),
    p("mibs (mibs) res: ~p", [Reason19]),

    p("mibs - incorrect mibs (2)"),
    Mibs2  = [join(StdMibDir, "INVALID-MIB")],
    Opts20 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {mibs, Mibs2}],
    ?line {error, Reason20} = config_start(Opts20),
    p("mibs (mibs) res: ~p", [Reason20]),

    p("prio - incorrect prio"),
    Prio1 = invalid_prio,
    Opts21 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {priority, Prio1}],
    ?line {error, Reason21} = config_start(Opts21),
    p("prio (prio) res: ~p", [Reason21]),

    p("atl - incorrect type"),
    ATL1  = [{type,   invalid_type}, 
	     {dir,    LogDir}, 
	     {size,   {10,10240}},
	     {repair, true}],
    Opts22 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL1}],
    ?line {error, Reason22} = config_start(Opts22),
    p("atl (type) res: ~p", [Reason22]),

    p("atl - incorrect dir (1)"),
    ATL2  = [{type,   read_write}, 
	     {dir,    invalid_dir}, 
	     {size,   {10,10240}},
	     {repair, true}],
    Opts23 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL2}],
    ?line {error, Reason23} = config_start(Opts23),
    p("atl (dir) res: ~p", [Reason23]),

    p("atl - incorrect dir (2)"),
    ATL3  = [{type,   read_write}, 
	     {dir,    "/invalid/dir"}, 
	     {size,   {10,10240}},
	     {repair, true}],
    Opts24 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL3}],
    ?line {error, Reason24} = config_start(Opts24),
    p("atl (dir) res: ~p", [Reason24]),

    p("atl - incorrect size (1)"),
    ATL4  = [{type,   read_write}, 
	     {dir,    LogDir}, 
	     {size,   invalid_size},
	     {repair, true}],
    Opts25 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL4}],
    ?line {error, Reason25} = config_start(Opts25),
    p("atl (size) res: ~p", [Reason25]),

    p("atl - incorrect size (2)"),
    ATL5  = [{type,   read_write}, 
	     {dir,    LogDir}, 
	     {size,   {10,invalid_file_size}},
	     {repair, true}],
    Opts26 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL5}],
    ?line {error, Reason26} = config_start(Opts26),
    p("atl (size) res: ~p", [Reason26]),

    p("atl - incorrect size (3)"),
    ATL6  = [{type,   read_write}, 
	     {dir,    LogDir}, 
	     {size,   {invalid_file_num,10240}},
	     {repair, true}],
    Opts27 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL6}],
    ?line {error, Reason27} = config_start(Opts27),
    p("atl (size) res: ~p", [Reason27]),

    p("atl - incorrect repair"),
    ATL7  = [{type,   read_write}, 
	     {dir,    LogDir}, 
	     {size,   {10,10240}},
	     {repair, invalid_repair}],
    Opts28 = [{config, ConfigOpts}, 
	      {versions, [v1]}, 
	      {audit_trail_log, ATL7}],
    ?line {error, Reason28} = config_start(Opts28),
    p("atl (repair) res: ~p", [Reason28]),

    p("version - incorrect versions (1)"),
    Vsns1  = invalid_vsns,
    Opts29 = [{config, ConfigOpts}, 
	      {versions, Vsns1}],
    ?line {error, Reason29} = config_start(Opts29),
    p("versions (versions) res: ~p", [Reason29]),

    p("version - incorrect versions (2)"),
    Vsns2  = [v1,v2,v3,v9],
    Opts30 = [{config, ConfigOpts}, 
	      {versions, Vsns2}],
    ?line {error, Reason30} = config_start(Opts30),
    p("versions (versions) res: ~p", [Reason30]),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_invalid_manager_conf_file1(suite) -> [];
start_with_invalid_manager_conf_file1(doc) ->
    "Start with invalid manager config file (1).";
start_with_invalid_manager_conf_file1(Conf) when is_list(Conf) -> 
    put(tname,swimcf),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    %% --
    p("write manager config file with invalid IP address (1)"),
    write_manager_conf(ConfDir, 
		       "arne-anka", "4001", "500", "\"bmkEngine\""),
    ?line {error, Reason11} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason11]),
    ?line {failed_reading, _, _, 1, {parse_error, _}} = Reason11,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid IP address (2)"),
    write_manager_conf(ConfDir, 
		       "arne_anka", "4001", "500", "\"bmkEngine\""),
    ?line {error, Reason12} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason12]),
    ?line {failed_check, _, _, 2, {bad_address, _}} = Reason12,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid IP address (3)"),
    write_manager_conf(ConfDir, 
		       "9999", "4001", "500", "\"bmkEngine\""),
    ?line {error, Reason13} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason13]),
    ?line {failed_check, _, _, 2, {bad_address, _}} = Reason13,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid port (2)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "kalle-anka", "500", "\"bmkEngine\""),
    ?line {error, Reason21} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason21]),
    ?line {failed_reading, _, _, 2, {parse_error, _}} = Reason21,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid port (1)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "-1", "500", "\"bmkEngine\""),
    ?line {error, Reason22} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason22]),
    io:format("Reason22: ~p~n", [Reason22]),
   ?line {failed_check, _, _, 3, {bad_port, _}} = Reason22,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid port (3)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "\"kalle-anka\"", "500", "\"bmkEngine\""),
    ?line {error, Reason23} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason23]),
    ?line {failed_check, _, _, 3, {bad_port, _}} = Reason23,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid EngineID (1)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "500", "bmkEngine"),
    ?line {error, Reason31} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason31]),
    ?line {failed_check, _, _, 5, {invalid_string, _}} = Reason31,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid EngineID (2)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "500", "{1,2,3}"),
    ?line {error, Reason32} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason32]),
    ?line {failed_check, _, _, 5, {invalid_string, _}} = Reason32,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid EngineID (3)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "500", "10101"),
    ?line {error, Reason33} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason33]),
    ?line {failed_check, _, _, 5, {invalid_string, _}} = Reason33,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid MMS (1)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "483", "\"bmkEngine\""),
    ?line {error, Reason41} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason41]),
    ?line {failed_check, _, _, 4, {invalid_integer, _}} = Reason41,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid MMS (2)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "-1", "\"bmkEngine\""),
    ?line {error, Reason42} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason42]),
    ?line {failed_check, _, _, 4, {invalid_integer, _}} = Reason42,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid MMS (3)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "\"kalle-anka\"", "\"bmkEngine\""),
    ?line {error, Reason43} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason43]),
    ?line {failed_check, _, _, 4, {invalid_integer, _}} = Reason43,
    await_config_not_running(),

    %% --
    p("write manager config file with invalid MMS (4)"),
    write_manager_conf(ConfDir, 
		       "[134,138,177,189]", "4001", "kalle_anka", "\"bmkEngine\""),
    ?line {error, Reason44} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason44]),
    ?line {failed_check, _, _, 4, {invalid_integer, _}} = Reason44,
    await_config_not_running(),

    %% --
    p("write manager config file with unknown option"),
    write_manager_conf(ConfDir, 
		       "{kalle, anka}."),
    ?line {error, Reason51} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason51]),
    ?line {failed_check, _, _, 1, {unknown_config, _}} = Reason51,
    await_config_not_running(),

    %% --
    p("write manager config file with unknown option"),
    write_manager_conf(ConfDir, 
		       "kalle_anka."),
    ?line {error, Reason52} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason52]),
    ?line {failed_check, _, _, 1, {unknown_config, _}} = Reason52,
    await_config_not_running(),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_invalid_users_conf_file1(suite) -> [];
start_with_invalid_users_conf_file1(doc) ->
    "Start with invalid users config file.";
start_with_invalid_users_conf_file1(Conf) when is_list(Conf) -> 
    put(tname,swiucf),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir   = ?config(manager_db_dir, Conf),

    verify_dir_existing(conf, ConfDir),
    verify_dir_existing(db,   DbDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    write_manager_conf(ConfDir),

    %% --
    p("write users config file with invalid module (1)"),
    write_users_conf(ConfDir, [{"kalle", "kalle", "dummy"}]),
    ?line {error, Reason11} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason11]),
    ?line {failed_check, _, _, _, {bad_module, kalle}} = Reason11,
    await_config_not_running(),

    %% --
    p("write users config file with invalid module (1)"),
    write_users_conf(ConfDir, [{"kalle", "snmpm", "dummy"}]),
    ?line {error, Reason12} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason12]),
    ?line {failed_check, _, _, _, {bad_module, _}} = Reason12,
    await_config_not_running(),

    %% --
    p("write users config file with invalid module (2)"),
    write_users_conf(ConfDir, [{"kalle1", "10101", "dummy"}]),
    ?line {error, Reason13} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason13]),
    ?line {failed_check, _, _, _, {bad_module, _}} = Reason13,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user tuple (1)"),
    write_users_conf2(ConfDir, "{kalle, snmpm_user_default}."),
    ?line {error, Reason21} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason21]),
    ?line {failed_check, _, _, _, {bad_user_config, _}} = Reason21,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user tuple (2)"),
    write_users_conf2(ConfDir, "{kalle, snmpm_user_default, kalle, [], olle}."),
    ?line {error, Reason22} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason22]),
    ?line {failed_check, _, _, _, {bad_user_config, _}} = Reason22,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user tuple (3)"),
    write_users_conf2(ConfDir, "snmpm_user_default."),
    ?line {error, Reason23} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason23]),
    ?line {failed_check, _, _, _, {bad_user_config, _}} = Reason23,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user tuple (4)"),
    write_users_conf2(ConfDir, "[kalle, snmpm_user_default, kalle]."),
    ?line {error, Reason24} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason24]),
    ?line {failed_check, _, _, _, {bad_user_config, _}} = Reason24,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user agent default config (1)"),
    write_users_conf2(ConfDir, "{kalle, snmpm_user_default, kalle, olle}."),
    ?line {error, Reason31} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason31]),
    ?line {failed_check, _, _, _, {bad_default_agent_config, _}} = Reason31,
    await_config_not_running(),

    %% --
    p("write users config file with invalid user agent default config (2)"),
    write_users_conf2(ConfDir, "{kalle, snmpm_user_default, kalle, [olle]}."),
    ?line {error, Reason32} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason32]),
    %% ?line {failed_check, _, _, _, {bad_default_agent_config, _}} = Reason32,
    case Reason32 of
	{failed_check, _, _, _, {bad_default_agent_config, _}} ->
	    ok;
	{A, B, C, D} ->
	    exit({bad_error, A, B, C, D})
    end,
    await_config_not_running(),

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_invalid_agents_conf_file1(suite) -> [];
start_with_invalid_agents_conf_file1(doc) ->
    "Start with invalid agents config file.";
start_with_invalid_agents_conf_file1(Conf) when is_list(Conf) -> 
    put(tname, swiacf),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir   = ?config(manager_db_dir, Conf),

    verify_dir_existing(conf, ConfDir),
    verify_dir_existing(db,   DbDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    write_manager_conf(ConfDir),

    write_users_conf(ConfDir, [{"swiacf", "snmpm_user_default", "dummy"}]),
    
    Agent0 = {"swiacf", "\"targ-hobbes\"", "\"comm1\"", 
	      "[192,168,0,100]", "162", "\"bmkEngine\"", "1500", "484", "v1",
	      "any", "\"initial\"", "noAuthNoPriv"},

    %% --
    p("[test 11] write agents config file with invalid user (1)"),
    Agent11 = setelement(1, Agent0, "kalle-anka"),
    write_agents_conf(ConfDir, [Agent11]),
    case config_start(Opts) of
	{error, Reason11} ->
	    p("start failed (as expected): ~p", [Reason11]),
	    ?line {failed_reading, _, _, _, {parse_error, _}} = Reason11,
	    await_config_not_running();
	OK_11 ->
	    exit({error, {unexpected_success, "11", OK_11}})
    end,

    %% --
    p("[test 21] write agents config file with invalid target name (1)"),
    Agent21 = setelement(2, Agent0, "targ-hobbes"),
    write_agents_conf(ConfDir, [Agent21]),
    case config_start(Opts) of
	{error, Reason21} ->
	    p("start failed (as expected): ~p", [Reason21]),
	    ?line {failed_reading, _, _, _, {parse_error, _}} = Reason21,
	    await_config_not_running();
	OK_21 ->
	    exit({error, {unexpected_success, "21", OK_21}})
    end,

    %% --
    p("[test 22] write agents config file with invalid target name (2)"),
    Agent22 = setelement(2, Agent0, "targ_hobbes"),
    write_agents_conf(ConfDir, [Agent22]),
    case config_start(Opts) of
	{error, Reason22} ->
	    p("start failed (as expected): ~p", [Reason22]),
	    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason22,
	    await_config_not_running();
	OK_22 ->
	    exit({error, {unexpected_success, "22", OK_22}})
    end,

    %% --
    p("[test 23] write agents config file with invalid target name (3)"),
    Agent23 = setelement(2, Agent0, "10101"),
    write_agents_conf(ConfDir, [Agent23]),
    case config_start(Opts) of
	{error, Reason23} ->
	    p("start failed (as expected): ~p", [Reason23]),
	    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason23,
	    await_config_not_running();
	OK_23 ->
	    exit({error, {unexpected_success, "23", OK_23}})
    end,

    %% --
    p("[test 31] write agents config file with invalid community (1)"),
    Agent31 = setelement(3, Agent0, "targ-hobbes"),
    write_agents_conf(ConfDir, [Agent31]),
    case config_start(Opts) of
	{error, Reason31} ->
	    p("start failed (as expected): ~p", [Reason31]),
	    ?line {failed_reading, _, _, _, {parse_error, _}} = Reason31,
	    await_config_not_running();
	OK_31 ->
	    exit({error, {unexpected_success, "31", OK_31}})
    end,

    %% --
    p("[test 32] write agents config file with invalid community (2)"),
    Agent32 = setelement(3, Agent0, "targ_hobbes"),
    write_agents_conf(ConfDir, [Agent32]),
    case config_start(Opts) of
	{error, Reason32} ->
	    p("start failed (as expected): ~p", [Reason32]),
	    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason32,
	    await_config_not_running();
	OK_32 ->
	    exit({error, {unexpected_success, "32", OK_32}})
    end,

    %% --
    p("[test 33] write agents config file with invalid community (3)"),
    Agent33 = setelement(3, Agent0, "10101"),
    write_agents_conf(ConfDir, [Agent33]),
    case config_start(Opts) of
	{error, Reason33} ->
	    p("start failed (as expected): ~p", [Reason33]),
	    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason33,
	    await_config_not_running();
	OK_33 ->
	    exit({error, {unexpected_success, "33", OK_33}})
    end,

    %% --
    p("[test 51] write agents config file with invalid ip (1)"),
    Agent51 = setelement(4, Agent0, "kalle_anka"),
    write_agents_conf(ConfDir, [Agent51]),
    case config_start(Opts) of
	{error, Reason51} ->
	    p("start failed (as expected): ~p", [Reason51]),
	    ?line {failed_check, _, _, _, {bad_domain, _}} = Reason51,
	    await_config_not_running();
	OK_51 ->
	    exit({error, {unexpected_success, "51", OK_51}})
    end,

    %% --
    p("[test 52] write agents config file with invalid ip (2)"),
    Agent52 = setelement(4, Agent0, "10101"),
    write_agents_conf(ConfDir, [Agent52]),
    case config_start(Opts) of
	{error, Reason52} ->
	    p("start failed (as expected): ~p", [Reason52]),
	    ?line {failed_check, _, _, _, {bad_address, _}} = Reason52,
	    await_config_not_running();
	OK_52 ->
	    exit({error, {unexpected_success, "52", OK_52}})
    end,

    %% --
    p("[test 53] write agents config file with invalid ip (3)"),
    Agent53 = setelement(4, Agent0, "[192,168,0]"),
    write_agents_conf(ConfDir, [Agent53]),
    case config_start(Opts) of
	{error, Reason53} ->
	    p("start failed (as expected): ~p", [Reason53]),
	    ?line {failed_check, _, _, _, {bad_address, _}} = Reason53,
	    await_config_not_running();
	OK_53 ->
	    exit({error, {unexpected_success, "53", OK_53}})
    end,

    %% --
    p("[test 54] write agents config file with invalid ip (4)"),
    Agent54 = setelement(4, Agent0, "[192,168,0,100,99]"),
    write_agents_conf(ConfDir, [Agent54]),
    case config_start(Opts) of
	{error, Reason54} ->
	    p("start failed (as expected): ~p", [Reason54]),
	    ?line {failed_check, _, _, _, {bad_address, _}} = Reason54,
	    await_config_not_running();
	OK_54 ->
	    exit({error, {unexpected_success, "54", OK_54}})
    end,

    %% --
    p("[test 55] write agents config file with invalid ip (5)"),
    Agent55 = setelement(4, Agent0, "[192,168,0,arne]"),
    write_agents_conf(ConfDir, [Agent55]),
    ?line {error, Reason55} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason55]),
    ?line {failed_check, _, _, _, {bad_address, _}} = Reason55,
    await_config_not_running(),

    %% --
    p("[test 61] write agents config file with invalid port (1)"),
    Agent61 = setelement(5, Agent0, "kalle_anka"),
    write_agents_conf(ConfDir, [Agent61]),
    ?line {error, Reason61} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason61]),
    ?line {failed_check, _, _, _, {bad_address, _}} = Reason61,
    await_config_not_running(),

    %% --
    p("[test 62] write agents config file with invalid port (2)"),
    Agent62 = setelement(5, Agent0, "-1"),
    write_agents_conf(ConfDir, [Agent62]),
    ?line {error, Reason62} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason62]),
    ?line {failed_check, _, _, _, {bad_address, _}} = Reason62,
    await_config_not_running(),

    %% --
    p("[test 63] write agents config file with invalid port (3)"),
    Agent63 = setelement(5, Agent0, "\"100\""),
    write_agents_conf(ConfDir, [Agent63]),
    ?line {error, Reason63} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason63]),
    ?line {failed_check, _, _, _, {bad_address, _}} = Reason63,
    await_config_not_running(),

    %% --
    p("[test 71] write agents config file with invalid engine-id (1)"),
    Agent71 = setelement(6, Agent0, "kalle_anka"),
    write_agents_conf(ConfDir, [Agent71]),
    ?line {error, Reason71} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason71]),
    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason71,
    await_config_not_running(),

    %% --
    p("[test 72] write agents config file with invalid engine-id (2)"),
    Agent72 = setelement(6, Agent0, "10101"),
    write_agents_conf(ConfDir, [Agent72]),
    ?line {error, Reason72} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason72]),
    ?line {failed_check, _, _, _, {invalid_string, _}} = Reason72,
    await_config_not_running(),

    %% --
    p("[test 81] write agents config file with invalid timeout (1)"),
    Agent81 = setelement(7, Agent0, "kalle_anka"),
    write_agents_conf(ConfDir, [Agent81]),
    ?line {error, Reason81} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason81]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason81,
    await_config_not_running(),

    %% --
    p("[test 82] write agents config file with invalid timeout (2)"),
    Agent82 = setelement(7, Agent0, "-1"),
    write_agents_conf(ConfDir, [Agent82]),
    ?line {error, Reason82} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason82]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason82,
    await_config_not_running(),

    %% --
    p("[test 83] write agents config file with invalid timeout (3)"),
    Agent83 = setelement(7, Agent0, "{1000, 1, 10, kalle}"),
    write_agents_conf(ConfDir, [Agent83]),
    ?line {error, Reason83} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason83]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason83,
    await_config_not_running(),

    %% --
    p("[test 84] write agents config file with invalid timeout (4)"),
    Agent84 = setelement(7, Agent0, "{1000, -1, 10, 10}"),
    write_agents_conf(ConfDir, [Agent84]),
    ?line {error, Reason84} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason84]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason84,
    await_config_not_running(),

    %% --
    p("[test 85] write agents config file with invalid timeout (5)"),
    Agent85 = setelement(7, Agent0, "{1000, 1, -100, 10}"),
    write_agents_conf(ConfDir, [Agent85]),
    ?line {error, Reason85} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason85]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason85,
    await_config_not_running(),

    %% --
    p("[test 86] write agents config file with invalid timeout (6)"),
    Agent86 = setelement(7, Agent0, "{1000, 1, 100, -1}"),
    write_agents_conf(ConfDir, [Agent86]),
    ?line {error, Reason86} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason86]),
    ?line {failed_check, _, _, _, {invalid_timer, _}} = Reason86,
    await_config_not_running(),

    %% --
    p("[test 91] write agents config file with invalid max-message-size (1)"),
    Agent91 = setelement(8, Agent0, "483"),
    write_agents_conf(ConfDir, [Agent91]),
    ?line {error, Reason91} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason91]),
    ?line {failed_check, _, _, _, {invalid_packet_size, _}} = Reason91,
    await_config_not_running(),

    %% --
    p("[test 92] write agents config file with invalid max-message-size (2)"),
    Agent92 = setelement(8, Agent0, "kalle_anka"),
    write_agents_conf(ConfDir, [Agent92]),
    ?line {error, Reason92} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason92]),
    ?line {failed_check, _, _, _, {invalid_packet_size, _}} = Reason92,
    await_config_not_running(),

    %% --
    p("[test A1] write agents config file with invalid version (1)"),
    AgentA1 = setelement(9, Agent0, "1"),
    write_agents_conf(ConfDir, [AgentA1]),
    ?line {error, ReasonA1} = config_start(Opts),
    p("start failed (as expected): ~p", [ReasonA1]),
    ?line {failed_check, _, _, _, {bad_version, _}} = ReasonA1,
    await_config_not_running(),

    %% --
    p("[test A2] write agents config file with invalid version (2)"),
    AgentA2 = setelement(9, Agent0, "v30"),
    write_agents_conf(ConfDir, [AgentA2]),
    ?line {error, ReasonA2} = config_start(Opts),
    p("start failed (as expected): ~p", [ReasonA2]),
    ?line {failed_check, _, _, _, {bad_version, _}} = ReasonA2,
    await_config_not_running(),

    %% --
    p("[test B1] write agents config file with invalid sec-model (1)"),
    AgentB1 = setelement(10, Agent0, "\"any\""),
    write_agents_conf(ConfDir, [AgentB1]),
    ?line {error, ReasonB1} = config_start(Opts),
    p("start failed (as expected): ~p", [ReasonB1]),
    ?line {failed_check, _, _, _, {invalid_sec_model, _}} = ReasonB1,
    await_config_not_running(),

    %% --
    p("[test B2] write agents config file with invalid sec-model (2)"),
    AgentB2 = setelement(10, Agent0, "v3"),
    write_agents_conf(ConfDir, [AgentB2]),
    ?line {error, ReasonB2} = config_start(Opts),
    p("start failed (as expected): ~p", [ReasonB2]),
    ?line {failed_check, _, _, _, {invalid_sec_model, _}} = ReasonB2,
    await_config_not_running(),

    %% --
    p("[test C1] write agents config file with invalid sec-name (1)"),
    AgentC1 = setelement(11, Agent0, "initial"),
    write_agents_conf(ConfDir, [AgentC1]),
    case config_start(Opts) of
	{error, ReasonC1} ->
	    p("start failed (as expected): ~p", [ReasonC1]),
	    ?line {failed_check, _, _, _, {bad_sec_name, _}} = ReasonC1,
	    await_config_not_running();
	OK_C1 ->
	    exit({error, {unexpected_success, "C1", OK_C1}})
    end,

    %% --
    p("[test C2] write agents config file with invalid sec-name (2)"),
    AgentC2 = setelement(11, Agent0, "10101"),
    write_agents_conf(ConfDir, [AgentC2]),
    case config_start(Opts) of
	{error, ReasonC2} ->
	    p("start failed (as expected): ~p", [ReasonC2]),
	    ?line {failed_check, _, _, _, {bad_sec_name, _}} = ReasonC2,
	    await_config_not_running();
	OK_C2 ->
	    exit({error, {unexpected_success, "C2", OK_C2}})
    end,

    %% --
    p("[test D1] write agents config file with invalid sec-level (1)"),
    AgentD1 = setelement(12, Agent0, "\"noAuthNoPriv\""),
    write_agents_conf(ConfDir, [AgentD1]),
    case config_start(Opts) of
	{error, ReasonD1} ->
	    p("start failed (as expected): ~p", [ReasonD1]),
	    ?line {failed_check, _, _, _, {invalid_sec_level, _}} = ReasonD1,
	    await_config_not_running();
	OK_D1 ->
	    exit({error, {unexpected_success, "D1", OK_D1}})
    end,

    %% --
    p("[test D2] write agents config file with invalid sec-level (2)"),
    AgentD2 = setelement(12, Agent0, "99"),
    write_agents_conf(ConfDir, [AgentD2]),
    case config_start(Opts) of
	{error, ReasonD2} ->
	    p("start failed (as expected): ~p", [ReasonD2]),
	    ?line {failed_check, _, _, _, {invalid_sec_level, _}} = ReasonD2,
	    await_config_not_running();
	OK_D2 ->
	    exit({error, {unexpected_success, "D2", OK_D2}})
    end,

    %% --
    p("[test E1] write agents config file with invalid agent (1)"),
    write_agents_conf2(ConfDir, "{swiacf, \"targ-hobbes\"}."),
    case config_start(Opts) of
	{error, ReasonE1} ->
	    p("start failed (as expected): ~p", [ReasonE1]),
	    ?line {failed_check, _, _, _, {bad_agent_config, _}} = ReasonE1,
	    await_config_not_running();
	OK_E1 ->
	    exit({error, {unexpected_success, "E1", OK_E1}})
    end,

    p("done"),
    ok.


%% 
%% ---
%% 

start_with_invalid_usm_conf_file1(suite) -> [];
start_with_invalid_usm_conf_file1(doc) ->
    "Start with invalid usm config file.";
start_with_invalid_usm_conf_file1(Conf) when is_list(Conf) -> 
    put(tname,swiusmcf),
    p("start"),
    process_flag(trap_exit, true),

    case ?CRYPTO_START() of
        ok ->
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?SKIP({unsupported_encryption, Reason});
                yes ->
                    ok
            end;
        {error, Reason} ->
            ?SKIP({failed_starting_crypto, Reason})
    end,
     
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    Opts = [{versions, [v1,v2,v3]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    write_manager_conf(ConfDir),

    write_users_conf(ConfDir, [{"swiacf", "snmpm_user_default", "dummy"}]),
    
    Usm0 = {"\"bmkEngine\"", "\"swiusmcf\"", 
	    "usmNoAuthProtocol", "[]",
	    "usmNoPrivProtocol", "[]"},

    Usm1 = {"\"bmkEngine\"", "\"swiusmcf\"", "\"kalle\"", 
	    "usmNoAuthProtocol", "[]",
	    "usmNoPrivProtocol", "[]"},

    %% --
    p("[test 11] write usm config file with invalid engine-id (1)"),
    Usm11 = setelement(1, Usm0, "kalle-anka"),
    write_usm_conf(ConfDir, [Usm11]),
    ?line {error, Reason11} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason11]),
    ?line {failed_reading, _, _, _, {parse_error, _}} = Reason11,
    await_config_not_running(),

    %% --
    p("[test 12] write usm config file with invalid engine-id (2)"),
    Usm12 = setelement(1, Usm0, "kalle_anka"),
    write_usm_conf(ConfDir, [Usm12]),
    ?line {error, Reason12} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason12]),
    ?line {failed_check, _, _, _, {bad_usm_engine_id, _}} = Reason12,
    await_config_not_running(),

    %% --
    p("[test 13] write usm config file with invalid engine-id (3)"),
    Usm13 = setelement(1, Usm1, "10101"),
    write_usm_conf(ConfDir, [Usm13]),
    ?line {error, Reason13} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason13]),
    ?line {failed_check, _, _, _, {bad_usm_engine_id, _}} = Reason13,
    await_config_not_running(),

    %% --
    p("[test 21] write usm config file with invalid user-name (1)"),
    Usm21 = setelement(2, Usm0, "kalle_anka"),
    write_usm_conf(ConfDir, [Usm21]),
    ?line {error, Reason21} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason21]),
    ?line {failed_check, _, _, _, {bad_usm_user_name, _}} = Reason21,
    await_config_not_running(),

    %% --
    p("[test 22] write usm config file with invalid user-name (1)"),
    Usm22 = setelement(2, Usm1, "10101"),
    write_usm_conf(ConfDir, [Usm22]),
    ?line {error, Reason22} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason22]),
    ?line {failed_check, _, _, _, {bad_usm_user_name, _}} = Reason22,
    await_config_not_running(),

    %% --
    p("[test 31] write usm config file with invalid sec-name (1)"),
    Usm31 = setelement(3, Usm1, "kalle_anka"),
    write_usm_conf(ConfDir, [Usm31]),
    ?line {error, Reason31} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason31]),
    ?line {failed_check, _, _, _, {bad_usm_sec_name, _}} = Reason31,
    await_config_not_running(),

    %% --
    p("[test 32] write usm config file with invalid sec-name (2)"),
    Usm32 = setelement(3, Usm1, "10101"),
    write_usm_conf(ConfDir, [Usm32]),
    ?line {error, Reason32} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason32]),
    ?line {failed_check, _, _, _, {bad_usm_sec_name, _}} = Reason32,
    await_config_not_running(),

    %% --
    p("[test 41] write usm config file with invalid auth-protocol (1)"),
    Usm41 = setelement(3, Usm0, "\"usmNoAuthProtocol\""),
    write_usm_conf(ConfDir, [Usm41]),
    ?line {error, Reason41} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason41]),
    ?line {failed_check, _, _, _, {invalid_auth_protocol, _}} = Reason41,
    await_config_not_running(),

    %% --
    p("[test 42] write usm config file with invalid auth-protocol (2)"),
    Usm42 = setelement(3, Usm0, "kalle"),
    write_usm_conf(ConfDir, [Usm42]),
    ?line {error, Reason42} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason42]),
    ?line {failed_check, _, _, _, {invalid_auth_protocol, _}} = Reason42,
    await_config_not_running(),

    %% --
    p("[test 43] write usm config file with invalid auth-protocol (3)"),
    Usm43 = setelement(3, Usm0, "10101"),
    write_usm_conf(ConfDir, [Usm43]),
    ?line {error, Reason43} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason43]),
    ?line {failed_check, _, _, _, {invalid_auth_protocol, _}} = Reason43,
    await_config_not_running(),

    %% --
    p("[test 51] write usm config file with invalid auth-key (1)"),
    Usm51 = setelement(3, Usm0, "usmHMACMD5AuthProtocol"),
    write_usm_conf(ConfDir, [Usm51]),
    ?line {error, Reason51} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason51]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _, _}} = Reason51,
    await_config_not_running(),

    %% --
    p("[test 52] write usm config file with invalid auth-key (2)"),
    Usm52 = setelement(4, Usm51, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]"),
    write_usm_conf(ConfDir, [Usm52]),
    ?line {error, Reason52} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason52]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _, 15}} = Reason52,
    await_config_not_running(),

    %% --
    p("[test 53] write usm config file with invalid auth-key (3)"),
    Usm53 = setelement(4, Usm51, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7]"),
    write_usm_conf(ConfDir, [Usm53]),
    ?line {error, Reason53} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason53]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _, 17}} = Reason53,
    await_config_not_running(),

    %% --
    p("[test 54] write usm config file with invalid auth-key (4)"),
    Usm54 = setelement(4, Usm51, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,kalle]"),
    write_usm_conf(ConfDir, [Usm54]),
    ?line maybe_start_crypto(),  %% Make sure it's started...
    ?line {error, Reason54} = config_start(Opts),
    ?line ok = maybe_stop_crypto(),
    p("start failed (as expected): ~p", [Reason54]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _}} = Reason54,
    await_config_not_running(),

    %% --
    p("[test 55] write usm config file with invalid auth-key (5)"),
    Usm55 = setelement(4, Usm51, "arne_anka"),
    write_usm_conf(ConfDir, [Usm55]),
    ?line {error, Reason55} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason55]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _}} = Reason55,
    await_config_not_running(),

    %% --
    p("[test 56] write usm config file with invalid auth-key (6)"),
    Usm56 = setelement(4, Usm51, "10101"),
    write_usm_conf(ConfDir, [Usm56]),
    ?line {error, Reason56} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason56]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _}} = Reason56,
    await_config_not_running(),

    %% --
    p("[test 57] write usm config file with invalid auth-key (7)"),
    Usm57 = setelement(3, Usm0, "usmHMACSHAAuthProtocol"),
    write_usm_conf(ConfDir, [Usm57]),
    ?line {error, Reason57} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason57]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _, _}} = Reason57,
    await_config_not_running(),

    %% --
    p("[test 58] write usm config file with invalid auth-key (8)"),
    Usm58 = setelement(4, Usm57, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]"),
    write_usm_conf(ConfDir, [Usm58]),
    ?line {error, Reason58} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason58]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _, 16}} = Reason58,
    await_config_not_running(),

    %% --
    p("[test 59] write usm config file with invalid auth-key (9)"),
    Usm59 = setelement(4, Usm57, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,ka]"),
    write_usm_conf(ConfDir, [Usm59]),
    ?line ok = maybe_start_crypto(),
    ?line {error, Reason59} = config_start(Opts),
    ?line ok = maybe_stop_crypto(),
    p("start failed (as expected): ~p", [Reason59]),
    ?line {failed_check, _, _, _, {invalid_auth_key, _}} = Reason59,
    await_config_not_running(),

    %% --
    %% <CRYPTO-MODIFICATIONS>
    %% The crypto application do no longer need to be started
    %% explicitly (all of it is as of R14 implemented with NIFs).
    case (catch crypto:version()) of
	{'EXIT', {undef, _}} ->
	    p("[test 5A] write usm config file with valid auth-key "
	      "when crypto not started (10)"),
	    Usm5A = setelement(4, 
			       Usm57, 
			       "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]"),
	    write_usm_conf(ConfDir, [Usm5A]),
	    ?line {error, Reason5A} = config_start(Opts),
	    p("start failed (as expected): ~p", [Reason5A]),
	    ?line {failed_check, _, _, _, {unsupported_crypto, _}} = Reason5A,
	    await_config_not_running();
	_ ->
	    %% This function is only present in version 2.0 or greater.
	    %% The crypto app no longer needs to be explicitly started
	    ok
    end,
    %% </CRYPTO-MODIFICATIONS>

    %% --
    p("[test 61] write usm config file with invalid priv-protocol (1)"),
    Usm61 = setelement(5, Usm0, "\"usmNoPrivProtocol\""),
    write_usm_conf(ConfDir, [Usm61]),
    ?line {error, Reason61} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason61]),
    ?line {failed_check, _, _, _, {invalid_priv_protocol, _}} = Reason61,
    await_config_not_running(),

    %% --
    p("[test 62] write usm config file with invalid priv-protocol (2)"),
    Usm62 = setelement(5, Usm0, "kalle"),
    write_usm_conf(ConfDir, [Usm62]),
    ?line {error, Reason62} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason62]),
    ?line {failed_check, _, _, _, {invalid_priv_protocol, _}} = Reason62,
    await_config_not_running(),

    %% --
    p("[test 63] write usm config file with invalid priv-protocol (3)"),
    Usm63 = setelement(5, Usm0, "10101"),
    write_usm_conf(ConfDir, [Usm63]),
    ?line {error, Reason63} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason63]),
    ?line {failed_check, _, _, _, {invalid_priv_protocol, _}} = Reason63,
    await_config_not_running(),

    %% --
    p("[test 71] write usm config file with invalid priv-key (1)"),
    Usm71 = setelement(5, Usm0, "usmDESPrivProtocol"),
    write_usm_conf(ConfDir, [Usm71]),
    ?line {error, Reason71} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason71]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _, _}} = Reason71,
    await_config_not_running(),

    %% --
    p("[test 72] write usm config file with invalid priv-key (2)"),
    Usm72 = setelement(6, Usm71, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]"),
    write_usm_conf(ConfDir, [Usm72]),
    ?line {error, Reason72} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason72]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _, 15}} = Reason72,
    await_config_not_running(),

    %% --
    p("[test 73] write usm config file with invalid priv-key (3)"),
    Usm73 = setelement(6, Usm71, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7]"),
    write_usm_conf(ConfDir, [Usm73]),
    ?line {error, Reason73} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason73]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _, 17}} = Reason73,
    await_config_not_running(),

    %% --
    p("[test 74] write usm config file with invalid priv-key (4)"),
    Usm74 = setelement(6, Usm71, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,kalle]"),
    write_usm_conf(ConfDir, [Usm74]),
    ?line ok = maybe_start_crypto(),
    ?line {error, Reason74} = config_start(Opts),
    ?line ok = maybe_stop_crypto(),
    p("start failed (as expected): ~p", [Reason74]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _}} = Reason74,
    await_config_not_running(),

    %% --
    p("[test 75] write usm config file with invalid priv-key (5)"),
    Usm75 = setelement(6, Usm71, "arne_anka"),
    write_usm_conf(ConfDir, [Usm75]),
    ?line {error, Reason75} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason75]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _}} = Reason75,
    await_config_not_running(),

    %% --
    p("[test 76] write usm config file with invalid priv-key (6)"),
    Usm76 = setelement(6, Usm71, "10101"),
    write_usm_conf(ConfDir, [Usm76]),
    ?line {error, Reason76} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason76]),
    ?line {failed_check, _, _, _, {invalid_priv_key, _}} = Reason76,
    await_config_not_running(),

    %% --
    %% <CRYPTO-MODIFICATIONS>
    %% The crypto application do no longer need to be started
    %% explicitly (all of it is as of R14 implemented with NIFs).
    case (catch crypto:version()) of
	{'EXIT', {undef, _}} ->
	    p("[test 77] write usm config file with valid priv-key "
	      "when crypto not started (7)"),
	    Usm77 = setelement(6, Usm71, "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]"),
	    write_usm_conf(ConfDir, [Usm77]),
	    ?line {error, Reason77} = config_start(Opts),
	    p("start failed (as expected): ~p", [Reason77]),
	    ?line {failed_check, _, _, _, {unsupported_crypto, _}} = Reason77,
	    await_config_not_running();
	_ ->
	    %% This function is only present in version 2.0 or greater.
	    %% The crypto app no longer needs to be explicitly started
	    ok
    end,
    %% </CRYPTO-MODIFICATIONS>

    %% --
    p("[test 78] write usm config file with invalid usm (1)"),
    write_usm_conf2(ConfDir, "{\"bmkEngine\", \"swiusmcf\"}."),
    ?line {error, Reason81} = config_start(Opts),
    p("start failed (as expected): ~p", [Reason81]),
    ?line {failed_check, _, _, _, {bad_usm_config, _}} = Reason81,
    await_config_not_running(),
   
    p("done"),
    ok.


%% 
%% ---
%% 

start_with_create_db_and_dir_opt(suite) -> [];
start_with_create_db_and_dir_opt(doc) ->
    "Start the snmp manager config process with the\n"
        "create_db_and_dir option.";
start_with_create_db_and_dir_opt(Conf) when is_list(Conf) ->
    put(tname, swcdado),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),
    true = not filelib:is_dir(DbDir) and not filelib:is_file(DbDir),
    write_manager_conf(ConfDir),

    p("verify nonexistent db_dir"),
    ConfigOpts01 = [{verbosity,trace}, {dir, ConfDir}, {db_dir, DbDir}],
    {error, Reason01} = config_start([{config, ConfigOpts01}]),
    p("nonexistent db_dir res: ~p", [Reason01]),
    {invalid_conf_db_dir, _, not_found} = Reason01,

    p("verify nonexistent db_dir gets created"),
    ConfigOpts02 = [{db_init_error, create_db_and_dir} | ConfigOpts01],
    {ok, _Pid} = config_start([{config, ConfigOpts02}]),
    true = filelib:is_dir(DbDir),
    p("verified: nonexistent db_dir was correctly created"),
    ok = config_stop(),

    p("done"),
    ok.

%% 
%% ---
%% 


simple_system_op(suite) -> [];
simple_system_op(doc) -> 
    "Access some of the known system info and some \n"
	"system info that does not exist.";
simple_system_op(Conf) when is_list(Conf) ->
    put(tname,sso),
    p("start"),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    p("start config"),
    ?line {ok, _Pid}         = config_start(Opts),
    
    p("retreive various configs"),
    ?line {ok, _Time}        = snmpm_config:system_start_time(),
    ?line {ok, _EngineId}    = snmpm_config:get_engine_id(),
    ?line {ok, _MMS}         = snmpm_config:get_engine_max_message_size(),

    p("attempt to retreive nonexisting"),
    ?line {error, not_found} = snmpm_config:system_info(kalle),
    
    ?line ok = config_stop(),
    await_config_not_running(),

    p("done"),
    ok.


%% 
%% ---
%% 



%% 
%% ---
%% 

register_user_using_file(suite) -> [];
register_user_using_file(doc) ->
    "Register user using the 'users.conf' file.";
register_user_using_file(Conf) when is_list(Conf) -> 
    put(tname,ruufi),
    p("start"),
    process_flag(trap_exit, true),
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%% 
%% ---
%% 

register_user_using_function(suite) -> [];
register_user_using_function(doc) ->
    "Register user using the API (function).";
register_user_using_function(Conf) when is_list(Conf) -> 
    put(tname,ruufu),
    p("start"),
    process_flag(trap_exit, true),
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%% 
%% ---
%% 

register_user_failed_using_function1(suite) -> [];
register_user_failed_using_function1(doc) ->
    "Register user failed using incorrect arguments to API (function).";
register_user_failed_using_function1(Conf) when is_list(Conf) -> 
    put(tname,rufufu1),
    p("start"),
    process_flag(trap_exit, true),
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%% 
%% ---
%% 



%% 
%% ---
%% 

register_agent_using_file(suite) -> [];
register_agent_using_file(doc) ->
    "Register agents using the 'agents'conf' file.";
register_agent_using_file(Conf) when is_list(Conf) -> 
    put(tname,raufi),
    p("start"),
    process_flag(trap_exit, true),
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],


    %% --
    p("write manager config file"),
    write_manager_conf(ConfDir),

    %% --
    p("write users config file"),
    UserId1 = raufi1,
    UserId1Str = str(UserId1),
    UserId2 = raufi2,
    UserId2Str = str(UserId2),
    User1 = {UserId1Str, "snmpm_user_default", "dummy1"},
    User2 = {UserId2Str, "snmpm_user_default", "dummy2", "[{version, v1}]"},
    write_users_conf(ConfDir, [User1, User2]),

    %% --
    p("write agents config file"),
    AgentAddr1 = [192,168,0,101],
    AgentAddr1Str = str(AgentAddr1),
    AgentPort1 = 162,
    AgentPort1Str = str(AgentPort1),
    EngineID1 = "bmkEngine1",
    EngineID1Str = str(EngineID1),
    MMS1 = 1024,
    MMS1Str = str(MMS1),
    AgentAddr2 = [192,168,0,102],
    AgentAddr2Str = str(AgentAddr2),
    AgentPort2 = 162,
    AgentPort2Str = str(AgentPort2),
    EngineID2 = "bmkEngine2",
    EngineID2Str = str(EngineID2),
    MMS2 = 512,
    MMS2Str = str(MMS2),
    Agent1Str = {UserId1Str, "\"targ-hobbes1\"", "\"comm\"", 
		 AgentAddr1Str, AgentPort1Str, EngineID1Str, 
		 "1000", MMS1Str, "v1",
		 "any", "\"initial\"", "noAuthNoPriv"},
    Agent2Str = {UserId2Str, "\"targ-hobbes2\"", "\"comm\"", 
		 AgentAddr2Str, AgentPort2Str, EngineID2Str, 
		 "1500", MMS2Str, "v1",
		 "any", "\"initial\"", "noAuthNoPriv"},
    write_agents_conf(ConfDir, [Agent1Str, Agent2Str]),

    %% --
    p("start the config process"),
    ?line {ok, _Pid} = config_start(Opts),

    %% --
    p("which agents"),
    ?line [_, _] = All = snmpm_config:which_agents(),
    p("all agents: ~n   ~p", [All]),
    ?line [A1]         = snmpm_config:which_agents(UserId1),
    p("agents belonging to ~w: ~n   ~p", [UserId1, A1]),
    ?line [A2]         = snmpm_config:which_agents(UserId2),
    p("agents belonging to ~w: ~n   ~p", [UserId2, A2]),

    %% --
    p("All info for agent <~w,~w>", [AgentAddr1, AgentPort1]),
    ?line {ok, AllInfo1} = 
	snmpm_config:agent_info(AgentAddr1, AgentPort1, all),
    p("all agent info for agent: ~n   ~p", [AllInfo1]),
    
    %% --
    p("EngineID (~p) for agent <~w,~w>", [EngineID1, AgentAddr1, AgentPort1]),
    ?line {ok, EngineID1} = 
	snmpm_config:agent_info(AgentAddr1, AgentPort1, engine_id),
    
    
    %% --
    p("All info for agent <~w,~w>", [AgentAddr2, AgentPort2]),
    ?line {ok, AllInfo2} = 
	snmpm_config:agent_info(AgentAddr2, AgentPort2, all),
    p("all agent info for agent: ~n   ~p", [AllInfo2]),
    
    %% --
    p("EngineID (~p) for agent <~w,~w>", [EngineID2, AgentAddr2, AgentPort2]),
    ?line {ok, EngineID2} = 
	snmpm_config:agent_info(AgentAddr2, AgentPort2, engine_id),

    %% --
    ?line {ok, MMS2} = 
	snmpm_config:agent_info(AgentAddr2, AgentPort2, max_message_size),
    NewMMS21 = 2048,
    p("try update agent info max-message-size to ~w for agent <~w,~w>", 
      [NewMMS21, AgentAddr2, AgentPort2]),
    ?line ok = snmpm_config:update_agent_info(UserId2, AgentAddr2, AgentPort2,
					      max_message_size, NewMMS21),
    ?line {ok, NewMMS21} = 
	snmpm_config:agent_info(AgentAddr2, AgentPort2, max_message_size),

    %% --
    p("try (and fail) to update agent info max-message-size to ~w "
      "for agent <~w,~w> " 
      "with user ~w (not owner)", 
      [NewMMS21, AgentAddr2, AgentPort2, UserId1]),
    ?line {error, Reason01} = 
	snmpm_config:update_agent_info(UserId1, AgentAddr2, AgentPort2,
				       max_message_size, NewMMS21),
    p("expected failure. Reason01: ~p", [Reason01]), 
    ?line {ok, NewMMS21} = 
	snmpm_config:agent_info(AgentAddr2, AgentPort2, max_message_size),

    %% --
    NewMMS22 = 400,
    p("try (and fail) to update agent info max-message-size to ~w "
      "for agent <~w,~w>", 
      [NewMMS22, AgentAddr2, AgentPort2]),
    ?line {error, Reason02} = 
	snmpm_config:update_agent_info(UserId1, AgentAddr2, AgentPort2,
				       max_message_size, NewMMS22),
    p("expected failure. Reason02: ~p", [Reason02]), 

    %% --
    p("stop config process"),
    ?line ok = snmpm_config:stop(),
    await_config_not_running(),

    %% --
    p("done"),
    ok.


%% 
%% ---
%% 

register_agent_using_function(suite) -> [];
register_agent_using_function(doc) ->
    "Register agents using the API (function).";
register_agent_using_function(Conf) when is_list(Conf) -> 
    put(tname,raufu),
    p("start"),
    process_flag(trap_exit, true),
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%% 
%% ---
%% 

register_agent_failed_using_function1(suite) -> [];
register_agent_failed_using_function1(doc) ->
    "Register agents failng using the API (function) with incorrect "
	"config (1).";
register_agent_failed_using_function1(Conf) when is_list(Conf) -> 
    put(tname,rafuf1),
    p("start"),
    process_flag(trap_exit, true),
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%%
%% ---
%% 



%% 
%% ---
%% 

register_usm_user_using_file(suite) -> [];
register_usm_user_using_file(doc) ->
    "Register usm user using the 'usm.conf' file.";
register_usm_user_using_file(Conf) when is_list(Conf) -> 
    put(tname,ruuufi),
    p("start"),
    process_flag(trap_exit, true),

    case ?CRYPTO_START() of
        ok ->
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?SKIP({unsupported_encryption, Reason});
                yes ->
                    ok
            end;
        {error, Reason} ->
            ?SKIP({failed_starting_crypto, Reason})
    end,
     
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir   = ?config(manager_db_dir, Conf),

    Opts = [{versions, [v3]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    %% --
    p("write manager config file"),
    write_manager_conf(ConfDir),

    %% --
    p("write usm user config file"),
    SecEngineID = "loctzp's engine",
    SecName1    = "samu_auth1",
    UserName1   = SecName1,
    UsmUser1 = {"\"" ++ SecEngineID ++ "\"", 
		"\"" ++ UserName1 ++ "\"", 
		"\"" ++ SecName1 ++ "\"", 
		"usmHMACMD5AuthProtocol", "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]",
		"usmNoPrivProtocol", "[]"},

    SecName2    = "samu_auth2",
    UserName2   = "samu",
    UsmUser2 = {"\"" ++ SecEngineID ++ "\"", 
		"\"" ++ UserName2 ++ "\"", 
		"\"" ++ SecName2 ++ "\"", 
		"usmHMACMD5AuthProtocol", "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]",
		"usmNoPrivProtocol", "[]"},
    write_usm_conf(ConfDir, [UsmUser1, UsmUser2]),

    %% --
    p("start the config process"),
    ?line {ok, _Pid} = config_start(Opts),

    %% --
    p("lookup 1 (ok)"),
    ?line {ok, #usm_user{name = UserName1} = User1} = 
	snmpm_config:get_usm_user_from_sec_name(SecEngineID, SecName1),
    p("User: ~p", [User1]),

    p("lookup 2 (ok)"),
    ?line {ok, #usm_user{name = UserName2} = User2} = 
	snmpm_config:get_usm_user_from_sec_name(SecEngineID, SecName2),
    p("User: ~p", [User2]),

    p("lookup 3 (error)"),
    ?line {error, not_found} = 
	snmpm_config:get_usm_user_from_sec_name(SecEngineID, SecName2 ++ "_1"),

    %% --
    p("stop config process"),
    ?line ok = snmpm_config:stop(),
    await_config_not_running(),

    %% --
    p("done"),
    ok.


%% 
%% ---
%% 

register_usm_user_using_function(suite) -> [];
register_usm_user_using_function(doc) ->
    "Register usm user using the API (function).";
register_usm_user_using_function(Conf) when is_list(Conf) -> 
    put(tname,ruuufu),
    p("start"),
    process_flag(trap_exit, true),

    case ?CRYPTO_START() of
        ok ->
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?SKIP({unsupported_encryption, Reason});
                yes ->
                    ok
            end;
        {error, Reason} ->
            ?SKIP({failed_starting_crypto, Reason})
    end,
     
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    Opts = [{versions, [v3]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    %% --
    p("write manager config file"),
    write_manager_conf(ConfDir),

    %% --
    p("start the config process"),
    ?line {ok, _Pid} = config_start(Opts),

    %% --
    p("register usm user's"),
    EngineID   = "loctzp's engine",

    p("register user 1 (ok)"),
    UserName1  = "samu_auth1",
    SecName1   = UserName1, 
    UsmConfig1 = [{sec_name, SecName1},
		  {auth,     usmHMACMD5AuthProtocol},
		  {auth_key, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]},
		  {priv,     usmNoPrivProtocol}],
    ?line ok = snmpm_config:register_usm_user(EngineID, UserName1, UsmConfig1),
    p("try register user 1 again (error)"),
    ?line {error, {already_registered, EngineID, UserName1}} = 
	snmpm_config:register_usm_user(EngineID, UserName1, UsmConfig1),
    
    p("register user 2 (ok)"),
    UserName2  = "samu_auth2",
    SecName2   = UserName2, 
    UsmConfig2 = [{auth,     usmHMACMD5AuthProtocol},
		  {auth_key, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]},
		  {priv,     usmNoPrivProtocol}],
    ?line ok = snmpm_config:register_usm_user(EngineID, UserName2, UsmConfig2),
    
    p("register user 3 (ok)"),
    UserName3  = "samu3",
    SecName3   = "samu_auth3",
    UsmConfig3 = [{sec_name, SecName3},
		  {auth,     usmHMACMD5AuthProtocol},
		  {auth_key, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6]},
		  {priv,     usmNoPrivProtocol}],
    ?line ok = snmpm_config:register_usm_user(EngineID, UserName3, UsmConfig3),
    
    p("lookup 1 (ok)"),
    ?line {ok, #usm_user{name = UserName1} = User1} = 
	snmpm_config:get_usm_user_from_sec_name(EngineID, SecName1),
    p("User: ~p", [User1]),

    p("lookup 2 (ok)"),
    ?line {ok, #usm_user{name = UserName2} = User2} = 
	snmpm_config:get_usm_user_from_sec_name(EngineID, SecName2),
    p("User: ~p", [User2]),

    p("lookup 3 (ok)"),
    ?line {ok, #usm_user{name = UserName3} = User3} = 
	snmpm_config:get_usm_user_from_sec_name(EngineID, SecName3),
    p("User: ~p", [User3]),

    p("lookup 4 (error)"),
    ?line {error, not_found} = 
	snmpm_config:get_usm_user_from_sec_name(EngineID, SecName3 ++ "_1"),

    %% --
    p("stop config process"),
    ?line ok = snmpm_config:stop(),
    await_config_not_running(),

    %% --
    p("done"),
    ok.


%% 
%% ---
%% 

register_usm_user_failed_using_function1(suite) -> [];
register_usm_user_failed_using_function1(doc) ->
    "Register usm user failed using incorrect arguments to API (function).";
register_usm_user_failed_using_function1(Conf) when is_list(Conf) -> 
    put(tname,ruufufu1),
    p("start"),
    process_flag(trap_exit, true),

    case ?CRYPTO_START() of
        ok ->
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?SKIP({unsupported_encryption, Reason});
                yes ->
                    ok
            end;
        {error, Reason} ->
            ?SKIP({failed_starting_crypto, Reason})
    end,
     
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%%
%% ---
%% 

update_usm_user_info(suite) -> [];
update_usm_user_info(doc) ->
    "Update usm user info.";
update_usm_user_info(Conf) when is_list(Conf) -> 
    put(tname,ruufufu1),
    p("start"),
    process_flag(trap_exit, true),

    case ?CRYPTO_START() of
        ok ->
            case ?CRYPTO_SUPPORT() of
                {no, Reason} ->
                    ?SKIP({unsupported_encryption, Reason});
                yes ->
                    ok
            end;
        {error, Reason} ->
            ?SKIP({failed_starting_crypto, Reason})
    end,
     
    _ConfDir = ?config(manager_conf_dir, Conf),
    _DbDir = ?config(manager_db_dir, Conf),
    ?SKIP(not_yet_implemented).


%%
%% ---
%% 



%% 
%% ---
%% 

create_and_increment(suite) -> [];
create_and_increment(doc) ->
    "Craete and increment counters.";
create_and_increment(Conf) when is_list(Conf) -> 
    put(tname,cai),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?line {ok, _Pid} = snmpm_config:start_link(Opts),

    %% Random init
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()),

    StartVal = random:uniform(2147483647),
    IncVal   = 42, 
    EndVal   = StartVal + IncVal,

    ?line StartVal = snmpm_config:cre_counter(test_id, StartVal),
    ?line EndVal   = snmpm_config:incr_counter(test_id, IncVal),

    ?line ok = snmpm_config:stop(),
    await_config_not_running(),
    ok.


%% 
%% ---
%% 



%% 
%% ---
%% 

stats_create_and_increment(suite) -> [];
stats_create_and_increment(doc) ->
    "Create and increment statistics counters.";
stats_create_and_increment(Conf) when is_list(Conf) -> 
    put(tname,scai),
    p("start"),
    process_flag(trap_exit, true),
    
    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{versions, [v1]}, 
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    ?line {ok, _Pid} = snmpm_config:start_link(Opts),

    p("stats table (1): ~p", [ets:tab2list(snmpm_stats_table)]),
    ?line 0  = snmpm_config:maybe_cre_stats_counter(stats1, 0),
    p("stats table (2): ~p", [ets:tab2list(snmpm_stats_table)]),
    ?line ok = snmpm_config:maybe_cre_stats_counter(stats1, 0),
    p("stats table (3): ~p", [ets:tab2list(snmpm_stats_table)]),
    ?line 1  = snmpm_config:maybe_cre_stats_counter(stats2, 1),
    p("stats table (4): ~p", [ets:tab2list(snmpm_stats_table)]),
    ?line 10 = snmpm_config:cre_stats_counter(stats3, 10),
    p("stats table (5): ~p", [ets:tab2list(snmpm_stats_table)]),

    Stats1Inc = fun() -> snmpm_config:incr_stats_counter(stats1, 1) end,
    ?line 10 = loop(10, -1, Stats1Inc),
    p("stats table (6): ~p", [ets:tab2list(snmpm_stats_table)]),
    
    ?line ok = snmpm_config:reset_stats_counter(stats1),

    ?line 10 = loop(10, -1, Stats1Inc),

    ?line ok = snmpm_config:stop(),
    await_config_not_running(),
    ok.


loop(0, Acc, _) ->
    Acc;
loop(N, _, F) when (N > 0) andalso is_function(F) ->
    Acc = F(),
    loop(N-1, Acc, F).


%%======================================================================
%% Ticket test-cases
%%======================================================================



otp_7219(suite) ->
    [];
otp_7219(doc) ->
    "Test-case for ticket OTP-7219";
otp_7219(Config) when is_list(Config) ->
    put(tname, otp7219),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Config),
    DbDir   = ?config(manager_db_dir, Config),

    p("write manager configuration"),
    write_manager_conf(ConfDir),

    Opts1 = [{versions, [v1]}, 
	     {inform_request_behaviour, user}, 
	     {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    p("start manager config"),
    ?line {ok, _Pid1} = snmpm_config:start_link(Opts1),

    p("get some manager config"),
    {ok, {user, _}} = snmpm_config:system_info(net_if_irb),

    p("stop manager config"),
    ?line ok = snmpm_config:stop(),
    await_config_not_running(),

    IRB_TO = 15322, 
    Opts2 = [{versions, [v1]}, 
	     {inform_request_behaviour, {user, IRB_TO}}, 
	     {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],

    p("start manager config"),
    ?line {ok, _Pid2} = snmpm_config:start_link(Opts2),

    p("get some manager config"),
    {ok, {user, IRB_TO}} = snmpm_config:system_info(net_if_irb),

    p("stop manager config"),
    ?line ok = snmpm_config:stop(),
    await_config_not_running(),

    p("done"),
    ok.




otp_8395_1(suite) -> [];
otp_8395_1(doc) ->
    "OTP-8395(1)";
otp_8395_1(Conf) when is_list(Conf) ->
    put(tname, otp_8395_1),
    p("start"),
    process_flag(trap_exit, true),
    otp8395(Conf, false, ok),
    ok.

otp_8395_2(suite) -> [];
otp_8395_2(doc) ->
    "OTP-8395(2)";
otp_8395_2(Conf) when is_list(Conf) ->
    put(tname, otp_8395_2),
    p("start"),
    process_flag(trap_exit, true),
    otp8395(Conf, true, ok),
    ok.

otp_8395_3(suite) -> [];
otp_8395_3(doc) ->
    "OTP-8395(3)";
otp_8395_3(Conf) when is_list(Conf) ->
    put(tname, otp_8395_3),
    p("start"),
    process_flag(trap_exit, true),
    otp8395(Conf, gurka, error),
    ok.

otp8395(Conf, SeqNoVal, Expect) ->
    ConfDir   = ?config(manager_conf_dir, Conf),
    DbDir     = ?config(manager_db_dir, Conf),
    LogDir    = ?config(manager_log_dir, Conf),
    StdMibDir = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    
    write_manager_conf(ConfDir),
    
    %% Third set of options (no versions):
    p("all options"),
    NetIfOpts  = [{module,    snmpm_net_if}, 
		  {verbosity, trace},
		  {options,   [{recbuf,   30000},
			       {bind_to,  false},
			       {no_reuse, false}]}],
    ServerOpts = [{timeout, 10000}, {verbosity, trace}],
    NoteStoreOpts = [{timeout, 20000}, {verbosity, trace}],
    ConfigOpts = [{dir, ConfDir}, {verbosity, trace}, {db_dir, DbDir}],
    Mibs = [join(StdMibDir, "SNMP-NOTIFICATION-MIB"),
	    join(StdMibDir, "SNMP-USER-BASED-SM-MIB")],
    Prio = normal,
    ATL  = [{type,   read_write}, 
	    {dir,    LogDir}, 
	    {size,   {10,10240}},
	    {repair, true},
	    {seqno,  SeqNoVal}],
    Vsns = [v1,v2,v3],
    Opts = [{config,          ConfigOpts},
	    {net_if,          NetIfOpts},
	    {server,          ServerOpts},
	    {note_store,      NoteStoreOpts},
	    {audit_trail_log, ATL},
	    {priority,        Prio}, 
	    {mibs,            Mibs},
	    {versions,        Vsns}],
    
    case config_start(Opts) of
	{ok, _Pid} when (Expect =:= ok) ->
	    ?line ok = config_stop(),
	    ok;
	{ok, _Pid} when (Expect =/= ok) ->
	    config_stop(),
	    exit({unexpected_started_config, SeqNoVal});
	_Error when (Expect =/= ok) ->
	    ok; 
	Error when (Expect =:= ok) ->
	    exit({unexpected_failed_starting_config, SeqNoVal, Error})
    end,
    p("done"),
    ok.


otp_8395_4(suite) -> [];
otp_8395_4(doc) ->
    "OTP-8395(4)";
otp_8395_4(Conf) when is_list(Conf) ->
    put(tname, otp_8395_4),
    p("start"),
    process_flag(trap_exit, true),

    snmp:print_version_info(),

    ConfDir   = ?config(manager_conf_dir, Conf),
    DbDir     = ?config(manager_db_dir, Conf),
    LogDir    = ?config(manager_log_dir, Conf),
    StdMibDir = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    
    write_manager_conf(ConfDir),
    
    %% Third set of options (no versions):
    p("all options"),
    NetIfOpts  = [{module,    snmpm_net_if}, 
		  {verbosity, trace},
		  {options,   [{recbuf,   30000},
			       {bind_to,  false},
			       {no_reuse, false}]}],
    ServerOpts = [{timeout, 10000}, {verbosity, trace}],
    NoteStoreOpts = [{timeout, 20000}, {verbosity, trace}],
    ConfigOpts = [{dir, ConfDir}, {verbosity, trace}, {db_dir, DbDir}],
    Mibs = [join(StdMibDir, "SNMP-NOTIFICATION-MIB"),
	    join(StdMibDir, "SNMP-USER-BASED-SM-MIB")],
    Prio = normal,
    ATL  = [{type,   read_write}, 
	    {dir,    LogDir}, 
	    {size,   {10,10240}},
	    {repair, true},
	    {seqno,  true}],
    Vsns = [v1,v2,v3],
    Opts = [{config,          ConfigOpts},
	    {net_if,          NetIfOpts},
	    {server,          ServerOpts},
	    {note_store,      NoteStoreOpts},
	    {audit_trail_log, ATL},
	    {priority,        Prio}, 
	    {mibs,            Mibs},
	    {versions,        Vsns}],
    
    ?line {ok, _Pid} = config_start(Opts),
    
    Counter   = otp_8395_4, 
    Initial   = 10,
    Increment = 2, 
    Max       = 20,
    
    %% At this call the counter does *not* exist. The call creates
    %% it with the initial value!

    Val1 = Initial, 
    Val1 = otp8395_incr_counter(Counter, Initial, Increment, Max),

    %% Now it exist, make sure another call does the expected increment
    
    Val2 = Initial + Increment, 
    Val2 = otp8395_incr_counter(Counter, Initial, Increment, Max),

    ?line ok = config_stop(),

    p("done"),
    ok.
    

otp8395_incr_counter(Counter, Initial, Increment, Max) ->
    snmpm_config:increment_counter(Counter, Initial, Increment, Max).


%%======================================================================
%% Internal functions
%%======================================================================

await_config_not_running() ->
    await_not_running(snmpm_config, 5).

await_not_running(Name, 0) ->
    p("await_not_running -> done waiting for ~w to die - try kill it", [Name]),
    %% Ok, we tried it the nice way, now use brute force
    await_killed(Name, 5);
await_not_running(Name, N) when N > 0 ->
    p("await_not_running -> is process ~w still running (~w)", [Name, N]),
    case erlang:whereis(Name) of
	undefined ->
	    p("await_not_running -> no such (~w) process - sleep some",[Name]),
	    ?SLEEP(1000),
	    p("await_not_running -> no such (~w) process - done", [Name]),
	    ok;
	Pid when is_pid(Pid) ->
	    p("~w process still running", [Name]),
	    ?SLEEP(500),
	    await_not_running(Name, N-1)
    end.
    
await_killed(Name, 0) ->
    p("await_killed -> could not kill ~w => giving up", [Name]),
    exit({error, {failed_terminating, Name}});
await_killed(Name, N) when N > 0 ->
    p("await_killed -> is process ~w still running (~w)", [Name, N]),
    case whereis(Name) of
	undefined ->
	    p("await_killed -> no such (~w) process - sleep some", [Name]),
	    ?SLEEP(1000),
	    p("await_killed -> no such (~w) process - done", [Name]),
	    ok;
	Pid when is_pid(Pid) ->
	    p("await_killed -> ~w still running - try kill it", [Name]),
	    exit(Pid, kill),
	    ?SLEEP(1000),
	    await_killed(Name, N-1)
    end.
    

config_start(Opts) ->
    (catch snmpm_config:start_link(Opts)).

config_stop() ->
    (catch snmpm_config:stop()).


%% ------

join(Dir, File) ->
    filename:join(Dir, File).


%% ------

write_manager_conf(Dir) ->
    Port = "5000",
    MMS  = "484",
    EngineID = "\"mgrEngine\"",
    Str = lists:flatten(
	    io_lib:format("%% Minimum manager config file\n"
			  "{port,             ~s}.\n"
			  "{max_message_size, ~s}.\n"
			  "{engine_id,        ~s}.\n",
			  [Port, MMS, EngineID])),
    write_manager_conf(Dir, Str).

write_manager_conf(Dir, IP, Port, MMS, EngineID) ->
    Str = lists:flatten(
	    io_lib:format("{address,          ~s}.\n"
			  "{port,             ~s}.\n"
			  "{max_message_size, ~s}.\n"
			  "{engine_id,        ~s}.\n",
			  [IP, Port, MMS, EngineID])),
    write_manager_conf(Dir, Str).

write_manager_conf(Dir, Str) ->
    write_conf_file(Dir, "manager.conf", Str).


write_users_conf(Dir, Users) ->
    F = fun({UserId, UserMod, UserData}) -> %% Old format
		lists:flatten(
		  io_lib:format("{~s, ~s, ~s, ~s}.~n", 
				[UserId, UserMod, UserData, "[]"]));
	   ({UserId, UserMod, UserData, DefaultAgentConfig}) -> %% New format
		lists:flatten(
		  io_lib:format("{~s, ~s, ~s, ~s}.~n", 
				[UserId, UserMod, UserData, DefaultAgentConfig]))
	end,
    Str = lists:flatten([F(User) || User <- Users]),
    write_conf_file(Dir, "users.conf", Str).

write_users_conf2(Dir, Str) ->
    write_conf_file(Dir, "users.conf", Str).


write_agents_conf(Dir, Agents) ->
    F = fun({UserId,
	     TargetName, Comm, 
	     Ip, Port, EngineID, 
	     Timeout, MMS, 
	     Version, SecModel, SecName, SecLevel}) ->
		lists:flatten(
		  io_lib:format("{~s, ~n"
				" ~s, ~s, ~n"
				" ~s, ~s, ~s, ~n"
				" ~s, ~s, ~n"
				" ~s, ~s, ~s, ~s}.~n", 
				[UserId, 
				 TargetName, Comm, 
				 Ip, Port, EngineID, 
				 Timeout, MMS, 
				 Version, SecModel, SecName, SecLevel]))
	end,
    Str = lists:flatten([F(Agent) || Agent <- Agents]),
    write_conf_file(Dir, "agents.conf", Str).

write_agents_conf2(Dir, Str) ->
    write_conf_file(Dir, "agents.conf", Str).


write_usm_conf(Dir, Usms) ->
    F = fun({EngineID, UserName, SecName, AuthP, AuthKey, PrivP, PrivKey}) ->
		lists:flatten(
		  io_lib:format("{~s, ~s, ~s, ~n"
				" ~s, ~s, ~n"
				" ~s, ~s}.~n", 
				[EngineID, UserName, SecName, 
				 AuthP, AuthKey, 
				 PrivP, PrivKey]));
	   ({EngineID, UserName, AuthP, AuthKey, PrivP, PrivKey}) ->
		lists:flatten(
		  io_lib:format("{~s, ~s, ~n"
				" ~s, ~s, ~n"
				" ~s, ~s}.~n", 
				[EngineID, UserName, 
				 AuthP, AuthKey, 
				 PrivP, PrivKey]));
	   (Usm) ->
		exit({invalid_usm, Usm})
	end,
    Str = lists:flatten([F(Usm) || Usm <- Usms]),
    write_conf_file(Dir, "usm.conf", Str).

write_usm_conf2(Dir, Str) ->
    write_conf_file(Dir, "usm.conf", Str).


write_conf_file(Dir, File, Str) ->
    case file:open(filename:join(Dir, File), write) of
	{ok, Fd} ->
	    ?line ok = io:format(Fd, "~s", [Str]),
	    file:close(Fd);
	{error, Reason} ->
	    Info = 
		[{dir, Dir, case (catch file:read_file_info(Dir)) of
				{ok, FI} -> 
				    FI;
				_ ->
				    undefined
			    end},
		 {file, File}], 
	    exit({failed_writing_conf_file, Info, Reason})
    end.


maybe_start_crypto() ->
    case (catch crypto:version()) of
	{'EXIT', {undef, _}} ->
	    %% This is the version of crypto before the NIFs...
	    ?CRYPTO_START();
	_ ->
	    %% No need to start this version of crypto..
	    ok
    end.

maybe_stop_crypto() ->
    case (catch crypto:version()) of
	{'EXIT', {undef, _}} ->
	    %% This is the version of crypto before the NIFs...
	    crypto:stop();
	_ ->
	    %% There is nothing to stop in this version of crypto..
	    ok
    end.


%% ------

verify_dir_existing(DirName, Dir) ->
    case file:read_file_info(Dir) of
	{ok, _} ->
	    ok;
	{error, Reason} ->
	    exit({non_existing_dir, DirName, Dir, Reason})
    end.


%% ------

str(X) ->
    lists:flatten(io_lib:format("~w", [X])).


%% ------

p(F) ->
    p(F, []).

p(F, A) ->
    p(get(tname), F, A).

p(TName, F, A) ->
    io:format("*** [~s] ***"
              " ~w -> " ++ F ++ "~n", [formated_timestamp(),TName|A]).

formated_timestamp() ->
    snmp_test_lib:formated_timestamp().
