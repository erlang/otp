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
%% Purpose: Various (snmp manager) user related tests
%%----------------------------------------------------------------------
-module(snmp_manager_user_test).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
 

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% -compile(export_all).

-export([
	 all/0,groups/0,init_per_group/2,end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
	 
	 
	 simple_register_and_unregister1/1,
	 simple_register_and_unregister2/1,
	 simple_register_and_unregister3/1,
	 register_and_crash1/1,
	 register_and_crash2/1,
	 register_and_crash3/1,
	 register_request_and_crash1/1,
	 register_request_and_crash2/1,
	 register_request_and_crash3/1,
	 simple_register_monitor_and_unregister1/1,
	 simple_register_monitor_and_unregister2/1,
	 simple_register_monitor_and_unregister3/1,
	 register_monitor_and_crash1/1, 
	 register_monitor_and_crash2/1, 
	 register_monitor_and_crash3/1, 
	 register_monitor_and_crash4/1, 
	 register_monitor_and_crash5/1, 
	 register_monitor_request_and_crash1/1,
	 register_monitor_request_and_crash2/1,
	 register_monitor_request_and_crash3/1,
	 register_monitor_request_and_crash4/1, 

	
	 otp7902/1

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
    ?line ok = 
	file:make_dir(MgrDbDir   = filename:join(MgrTopDir,   "db/")),
    ?line ok = 
	file:make_dir(MgrLogDir  = filename:join(MgrTopDir,   "log/")),
    [{suite_top_dir,    SuiteTopDir},
     {case_top_dir,     CaseTopDir},
     {manager_dir,      MgrTopDir},
     {manager_conf_dir, MgrConfDir},
     {manager_db_dir,   MgrDbDir},
     {manager_log_dir,  MgrLogDir} | Config].


end_per_testcase(Case, Config) when is_list(Config) ->
    p("end_per_testcase -> Case: ~p", [Case]),
%     MgrTopDir = ?config(manager_dir, Config),
%     ?DEL_DIR(MgrTopDir),
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================

all() -> 
[{group, register_user}, {group, tickets}].

groups() -> 
    [{register_user, [],
  [simple_register_and_unregister1,
   simple_register_and_unregister2,
   simple_register_and_unregister3, register_and_crash1,
   register_and_crash2, register_and_crash3,
   register_request_and_crash1,
   register_request_and_crash2,
   register_request_and_crash3,
   simple_register_monitor_and_unregister1,
   simple_register_monitor_and_unregister2,
   simple_register_monitor_and_unregister3,
   register_monitor_and_crash1,
   register_monitor_and_crash2,
   register_monitor_and_crash3,
   register_monitor_and_crash4,
   register_monitor_and_crash5,
   register_monitor_request_and_crash1,
   register_monitor_request_and_crash2,
   register_monitor_request_and_crash3,
   register_monitor_request_and_crash4]},
 {tickets, [], [otp7902]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.






%%======================================================================
%% Test functions
%%======================================================================

simple_register_and_unregister1(suite) -> [];
simple_register_and_unregister1(doc) ->
    "Start a user, register and unregister the user.";
simple_register_and_unregister1(Conf) when is_list(Conf) ->
    put(tname,srar1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),
    
    ?line ok = unregister_user(Pid),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_and_unregister2(suite) -> [];
simple_register_and_unregister2(doc) ->
    "Start a single user process, "
	"register 2 users (per that process) and unregister the 2 users.";
simple_register_and_unregister2(Conf) when is_list(Conf) ->
    put(tname,srar2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    
    ?line ok = unregister_user(Pid, Id1),
    ?line ok = unregister_user(Pid, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_and_unregister3(suite) -> [];
simple_register_and_unregister3(doc) ->
    "Start 2 user processes, "
	"register one users per process and unregister the 2 users.";
simple_register_and_unregister3(Conf) when is_list(Conf) ->
    put(tname,srar2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid1 = start_user(),
    ?line Pid2 = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid1, Id1),
    ?line ok = register_user(Pid2, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    
    ?line ok = unregister_user(Pid1, Id1),
    ?line ok = unregister_user(Pid2, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid1),
    ?line stop_user(Pid2),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash1(suite) -> [];
register_and_crash1(doc) ->
    "Start a user, register and crash user.";
register_and_crash1(Conf) when is_list(Conf) ->
    put(tname,racau1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?line [Id] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash2(suite) -> [];
register_and_crash2(doc) ->
    "Start a single user process, "
	"register 2 users (per that process) and crash the process.";
register_and_crash2(Conf) when is_list(Conf) ->
    put(tname,racau2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else1 ->
			   ?FAIL({invalid_users, Else1})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?line Users3 = case which_users() of
		       [Id1, Id2] = U3 ->
			   U3;
		       [Id2, Id1] = U4 ->
			   U4;
		       Else2 ->
			   ?FAIL({invalid_users, Else2})
		   end,
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_and_crash3(suite) -> [];
register_and_crash3(doc) ->
    "Start 2 user processes, "
	"register one user per process and "
	"crash the first user process.";
register_and_crash3(Conf) when is_list(Conf) ->
    %%     put(tname,rac3),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash1(suite) -> [];
register_request_and_crash1(doc) ->
    "Start a single user process, "
	"register user, send request and crash user.";
register_request_and_crash1(Conf) when is_list(Conf) ->
    %%     put(tname,rrac1),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash2(suite) -> [];
register_request_and_crash2(doc) ->
    "Start a single user process, "
	"register 2 users (per that single user process), "
	"send a request for each user and crash the single user process.";
register_request_and_crash2(Conf) when is_list(Conf) ->
    %%     put(tname,rrac2),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_request_and_crash3(suite) -> [];
register_request_and_crash3(doc) ->
    "Start 2 user processes, "
	"register one user per process, "
	"send a request for each user and crash the first user process.";
register_request_and_crash3(Conf) when is_list(Conf) ->
    %%     put(tname,rrac3),
    %%     p("start"),
    %%     process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister1(suite) -> [];
simple_register_monitor_and_unregister1(doc) ->
    "Start a user, register-link and unregister the user.";
simple_register_monitor_and_unregister1(Conf) when is_list(Conf) ->
    put(tname,srlau1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    p("start user"),
    ?line Pid = start_user(),

    p("get users (=0)"),
    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    p("register monitored user"),
    ?line ok = register_user_monitor(Pid, Id),

    p("get users (=1)"),
    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),
    
    p("unregister monitored user"),
    ?line unregister_user(Pid),

    p("get users (=0)"),
    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("start user"),
    ?line stop_user(Pid),

    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister2(suite) -> [];
simple_register_monitor_and_unregister2(doc) ->
    "Start a single user process, "
	"register-link 2 users (per that process) and "
	"unregister the 2 users.";
simple_register_monitor_and_unregister2(Conf) when is_list(Conf) ->
    put(tname,srlau2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),
    
    ?line ok = unregister_user(Pid, Id1),
    ?line ok = unregister_user(Pid, Id2),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

simple_register_monitor_and_unregister3(suite) -> [];
simple_register_monitor_and_unregister3(doc) ->
    "Start a single user process, "
	"register one user and register-monitor one user "
	"(per that process) and "
	"unregister the 2 users.";
simple_register_monitor_and_unregister3(Conf) when is_list(Conf) ->
    put(tname,srlau3),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),
    
    ?line unregister_user(Pid),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid),

    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash1(suite) -> [];
register_monitor_and_crash1(doc) ->
    "Start a user, register-monitor and crash the user.";
register_monitor_and_crash1(Conf) when is_list(Conf) ->
    put(tname,rlac1),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id),

    ?line [Id] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash2(suite) -> [];
register_monitor_and_crash2(doc) ->
    "Start a single user process, "
	"register-monitor 2 users (per that process) "
	"and crash the single user process.";
register_monitor_and_crash2(Conf) when is_list(Conf) ->
    put(tname,rlac2),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash3(suite) -> [];
register_monitor_and_crash3(doc) ->
    "Start a single user process, "
	"register-monitor one user and register one user, "
	"crash the single user process.";
register_monitor_and_crash3(Conf) when is_list(Conf) -> 
    process_flag(trap_exit, true),
    put(tname,rlac3),

    %% <CONDITIONAL-SKIP>
    %% The point of this is to catch machines running 
    %% SLES9 (2.6.5)
    LinuxVersionVerify = 
	fun() ->
		case os:cmd("uname -m") of
		    "i686" ++ _ ->
%% 			io:format("found an i686 machine, "
%% 				  "now check version~n", []),
			case os:version() of
			    {2, 6, Rev} when Rev >= 16 ->
				true;
			    {2, Min, _} when Min > 6 ->
				true;
			    {Maj, _, _} when Maj > 2 ->
				true;
			    _ ->
				false
			end;
		    _ ->
			true
		end
	end,
    Skippable = [{unix, [{linux, LinuxVersionVerify}]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Conf, Condition),
    %% </CONDITIONAL-SKIP>

    p("start"),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    ?line Pid = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user(Pid, Id1),
    ?line ok = register_user_monitor(Pid, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid),

    ?SLEEP(1000),

    ?line [Id1] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash4(suite) -> [];
register_monitor_and_crash4(doc) ->
    "Start 2 user processes, "
	"register-monitor one user per process "
	"and crash the first user process.";
register_monitor_and_crash4(Conf) when is_list(Conf) ->
    put(tname,rlac4),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("start manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = make_ref(), 
    Id2 = make_ref(), 

    p("start user processes"),
    ?line Pid1 = start_user(),
    ?line Pid2 = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid1, Id1),
    ?line ok = register_user_monitor(Pid2, Id2),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       Else ->
			   ?FAIL({invalid_users, Else})
		   end,
    p("Users2: ~p", [Users2]),

    ?line ok = simulate_crash(Pid1),

    ?SLEEP(1000),

    ?line [Id2] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line stop_user(Pid2),

    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_and_crash5(suite) -> [];
register_monitor_and_crash5(doc) ->
    "OTP-7961: "
	"Start 2 user processes, "
	"register-monitor a user for per process, "
	"let each user register an agent "
	"and crash the first user process.";
register_monitor_and_crash5(Conf) when is_list(Conf) ->
    put(tname,rlac4),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("start manager"),
    ?line ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    Id1 = gurka, %% make_ref(), 
    Id2 = tomat, %% make_ref(), 

    p("start user processes"),
    ?line Pid1 = start_user(),
    ?line Pid2 = start_user(),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = register_user_monitor(Pid1, Id1),
    ?line ok = register_user_monitor(Pid2, Id2),

    LocalHost = snmp_test_lib:localhost(),

    TargetName1 = "kalle1",
    Address1    = LocalHost,
    Port1       = 5001,
    EngineId1   = "agentEngineId-1",

    TargetName2 = "kalle2",
    Address2 = LocalHost,
    Port2       = 5002,
    EngineId2   = "agentEngineId-2",

    ?line ok = register_agent(Pid1, 
			      Id1, TargetName1, [{address,   Address1},
						 {port,      Port1},
						 {engine_id, EngineId1}]),
    ?line ok = register_agent(Pid2, 
			      Id2, TargetName2, [{address,   Address2},
						 {port,      Port2},
						 {engine_id, EngineId2}]),

    ?line Users2 = case which_users() of
		       [Id1, Id2] = U1 ->
			   U1;
		       [Id2, Id1] = U2 ->
			   U2;
		       U3 ->
			   ?FAIL({invalid_users, U3})
		   end,
    p("Users2: ~p", [Users2]),

    p("verify all agent(s): expect 2"),
    ?line Agents1 = case which_agents() of
			[TargetName1, TargetName2] = A1 ->
			    A1;
			[TargetName2, TargetName1] = A2 ->
			    A2;
		       A3 ->
			   ?FAIL({invalid_agents, A3})
		   end,
    p("Agents1: ~p", [Agents1]),

    ?line ok = simulate_crash(Pid1),

    p("wait some time"),
    ?SLEEP(1000),

    ?line [Id2] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line [TargetName2] = Agents2 = which_agents(),
    p("Agents2: ~p", [Agents2]),
    
    ?line stop_user(Pid2),

    p("stop manager"),
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

register_monitor_request_and_crash1(suite) -> [];
register_monitor_request_and_crash1(doc) ->
    "Start a single user process, "
	"register-monitor one user, "
	"send request and crash the user.";
register_monitor_request_and_crash1(Conf) when is_list(Conf) ->
    %% put(tname,rlrac1),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash2(suite) -> [];
register_monitor_request_and_crash2(doc) ->
    "Start a single user process, "
	"register-monitor 2 user (per that one process), "
	"send a request for each user and crash the single user process.";
register_monitor_request_and_crash2(Conf) when is_list(Conf) ->
    %% put(tname,rlrac2),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash3(suite) -> [];
register_monitor_request_and_crash3(doc) ->
    "Start a single user process, "
	"register-monitor one user and register one user, "
	"send a request for each user and crash the single user process.";
register_monitor_request_and_crash3(Conf) when is_list(Conf) ->
    %% put(tname,rlrac3),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).


%% ------------------------------------------------------------------

register_monitor_request_and_crash4(suite) -> [];
register_monitor_request_and_crash4(doc) ->
    "Start 2 user processes, "
	"register-monitor one user and register one user on the "
	"first user process and do the same for the other user process, "
	"then for each user, send a request and "
	"crash the first user process.";
register_monitor_request_and_crash4(Conf) when is_list(Conf) ->
    %% put(tname,rlrac4),
    %% p("start"),
    %% process_flag(trap_exit, true),
    ?SKIP(not_yet_implemented).



%% ------------------------------------------------------------------

otp7902(suite) -> [];
otp7902(doc) ->
    "OTP-7902 - Start old user and make sure it wors.";
otp7902(Conf) when is_list(Conf) ->
    put(tname, otp7902),
    p("start"),
    process_flag(trap_exit, true),

    ConfDir = ?config(manager_conf_dir, Conf),
    DbDir = ?config(manager_db_dir, Conf),

    write_manager_conf(ConfDir),

    Opts = [{server, [{verbosity, trace}]},
            {net_if, [{verbosity, trace}]},
            {note_store, [{verbosity, trace}]},
            {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
 
    p("try starting manager"),
    ok = snmpm:start_link(Opts),
 
    ?SLEEP(1000),

    ?line [] = Users1 = which_users(),
    p("Users1: ~p", [Users1]),
    
    ?line ok = snmp_manager_user_old:start(),

    ?line [_] = Users2 = which_users(),
    p("Users2: ~p", [Users2]),
    
    ?line ok = snmp_manager_user_old:stop(),

    ?line [] = Users3 = which_users(),
    p("Users3: ~p", [Users3]),
    
    ?line ok = snmpm:stop(),

    p("end"),
    ok.


%% ------------------------------------------------------------------

start_user() ->
    {ok, Pid} = snmp_manager_user_test_lib:start_link(),
    Pid.

stop_user(Pid) ->
    snmp_manager_user_test_lib:stop(Pid).

simulate_crash(Pid) ->
    snmp_manager_user_test_lib:simulate_crash(Pid, simulate_crash),
    receive
	{'EXIT', Pid, simulate_crash} ->
	    ok;
	{'EXIT', Pid, Whatever} ->
	    {ok, Whatever}
    after 5000 ->
	    {error, timeout}
    end.

register_user(Pid, Id) ->
    snmp_manager_user_test_lib:register(Pid, Id).

register_user_monitor(Pid, Id) ->
    snmp_manager_user_test_lib:register_monitor(Pid, Id).

unregister_user(Pid) ->
    case snmp_manager_user_test_lib:unregister(Pid) of
	{ok, Res} ->
	    case [R || R <- Res, R =/= ok] of
		[] ->
		    ok;
		Errs ->
		    {error, Errs}
	    end;
	Error ->
	    Error
    end.

unregister_user(Pid, Id) ->
    snmp_manager_user_test_lib:unregister(Pid, Id).


register_agent(Pid, Id, TargetName, Config) ->
    snmp_manager_user_test_lib:register_agent(Pid, Id, TargetName, Config).


%% ------

which_users() ->
    snmpm:which_users().

which_agents() ->
    snmpm:which_agents().


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

write_manager_conf(Dir, Str) ->
    write_conf_file(Dir, "manager.conf", Str).


write_conf_file(Dir, File, Str) ->
    ?line {ok, Fd} = file:open(filename:join(Dir, File), write),
    ?line ok = io:format(Fd, "~s", [Str]),
    file:close(Fd).


%% ------

p(F) ->
    p(F, []).
 
p(F, A) ->
    p(get(tname), F, A).
 
p(TName, F, A) ->
    io:format("~w -> " ++ F ++ "~n", [TName|A]).

