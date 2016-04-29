%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Test suite of the agent mib-server.
%%          Some of these tests should really be in a mib-storage suite.
%%----------------------------------------------------------------------

-module(snmp_agent_mibs_test).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------

-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/include/SNMP-COMMUNITY-MIB.hrl").
-include_lib("snmp/include/SNMP-VIEW-BASED-ACM-MIB.hrl").
-include_lib("snmp/include/SNMP-USER-BASED-SM-MIB.hrl").
-include("snmp_test_data/Test2.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/0,
	 groups/0,

	 init_per_suite/1, 
	 end_per_suite/1, 

	 init_per_group/2,
	 end_per_group/2, 

         init_per_testcase/2, 
	 end_per_testcase/2,

	 start_and_stop/1,
	
	 size_check_ets1/1,
	 size_check_ets2/1,
	 size_check_ets2_bad_file1/1,
	 size_check_ets3/1,
	 size_check_ets3_bad_file1/1,
	 size_check_dets/1,
	 size_check_mnesia/1,
	 load_unload/1,
	 me_lookup/1,
	 which_mib/1,
	 cache_test/1

	]).


%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================

init_per_suite(Config0) when is_list(Config0) ->

    ?DBG("init_per_suite -> entry with"
         "~n   Config0: ~p", [Config0]),

    Config1 = snmp_test_lib:init_suite_top_dir(?MODULE, Config0), 

    ?DBG("init_per_suite -> done when"
         "~n   Config1: ~p", [Config1]),

    Config1.

end_per_suite(Config) when is_list(Config) ->

    ?DBG("end_per_suite -> entry with"
         "~n   Config: ~p", [Config]),

    Config.


init_per_testcase(Case, Config0) when is_list(Config0) ->
    Config1    = snmp_test_lib:fix_data_dir(Config0),
    CaseTopDir = snmp_test_lib:init_testcase_top_dir(Case, Config1), 
    DbDir      = join(CaseTopDir, "db_dir/"),
    ?line ok   = file:make_dir(DbDir),
    init_per_testcase2(Case, [{db_dir,       DbDir}, 
			      {case_top_dir, CaseTopDir} | Config1]).

init_per_testcase2(size_check_ets2_bad_file1, Config) when is_list(Config) ->
    DbDir = ?config(db_dir, Config),
    %% Create a ad file
    ok = file:write_file(join(DbDir, "snmpa_symbolic_store.db"), 
			 "calvin and hoppes play chess"),
    Config;
init_per_testcase2(size_check_ets3_bad_file1, Config) when is_list(Config) ->
    DbDir = ?config(db_dir, Config),
    %% Create a ad file
    ok = file:write_file(join(DbDir, "snmpa_symbolic_store.db"), 
			 "calvin and hoppes play chess"),
    Config;
init_per_testcase2(size_check_mnesia, Config) when is_list(Config) ->
    DbDir = ?config(db_dir, Config),
    mnesia_start([{dir, DbDir}]),
    Config;
init_per_testcase2(cache_test, Config) when is_list(Config) ->
    Min = timer:minutes(5), 
    Timeout = 
	case lists:keysearch(tc_timeout, 1, Config) of
	    {value, {tc_timeout, TcTimeout}} when TcTimeout < Min ->
		Min; 
	    {value, {tc_timeout, TcTimeout}} ->
		TcTimeout; 
	    _ ->
		Min
	end,
    Dog = test_server:timetrap(Timeout), 
    [{watchdog, Dog} | Config];
init_per_testcase2(_Case, Config) when is_list(Config) ->
    Config.

%% end_per_testcase(EtsCase, Config) 
%%   when (is_list(Config) andalso 
%% 	((EtsCase =:= size_check_ets2) orelse 
%% 	 (EtsCase =:= size_check_ets3))) ->
%%     Dir = ?config(ets_dir, Config),
%%     ?line ok = ?DEL_DIR(Dir),
%%     lists:keydelete(ets_dir, 1, Config);
%% end_per_testcase(size_check_dets, Config) when is_list(Config) ->
%%     Dir = ?config(dets_dir, Config),
%%     ?line ok = ?DEL_DIR(Dir),
%%     lists:keydelete(dets_dir, 1, Config);
end_per_testcase(size_check_mnesia, Config) when is_list(Config) ->
    mnesia_stop(),
    %% Dir = ?config(db_dir, Config),
    %% ?line ok = ?DEL_DIR(Dir),
    %% lists:keydelete(mnesia_dir, 1, Config);
    Config;
end_per_testcase(cache_test, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    Config;
end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


%%======================================================================
%% Test case definitions
%%======================================================================

all() -> 
    cases().

groups() -> 
    [{size_check, [],
      [
       size_check_ets1,            % Plain ets
       size_check_ets2,            % ets with a file
       size_check_ets2_bad_file1,  % ets with a bad file
       size_check_ets3,            % ets with a checksummed file
       size_check_ets3_bad_file1,  % ets with bad file (checksummed) 
       size_check_dets,            % Plain dets
       size_check_mnesia           % Plain mnesia
      ]
     }].


init_per_group(GroupName, Config) ->
    snmp_test_lib:init_group_top_dir(GroupName, Config).

end_per_group(_GroupName, Config) ->
    %% Do we really need to do this?
    %% lists:keydelete(snmp_group_top_dir, 1, Config).
    Config.


cases() -> 
    [
     start_and_stop, 
     load_unload, 
     {group, size_check},
     me_lookup, 
     which_mib, 
     cache_test
    ].


%%======================================================================
%% Test functions
%%======================================================================

start_and_stop(suite) -> [];
start_and_stop(Config) when is_list(Config) ->
    Prio      = normal,
    Verbosity = trace,

    ?line sym_start(Prio, Verbosity),
    ?line MibsPid = mibs_start(Prio, Verbosity),

    ?line mibs_info(MibsPid),

    ?line mibs_stop(MibsPid),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

load_unload(suite) -> [];
load_unload(Config) when is_list(Config) ->
    ?DBG("load_unload -> start", []),

    Prio       = normal,
    Verbosity  = log,
    MibDir     = ?config(data_dir, Config),

    ?DBG("load_unload -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("load_unload -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("load_unload -> load one not already loaded mib", []),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, []),
    ?line ok = load_mibs(MibsPid, MibDir, ["Test2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["Test2"]),
    
    ?DBG("load_unload -> try load one *already loaded* mib", []),
    EMib = join(MibDir, "Test2"), 
    ?line {error, {'load aborted at', EMib, already_loaded}} = 
	load_mibs(MibsPid, MibDir, ["Test2"]),

    ?DBG("load_unload -> load 2 not already loaded mibs", []),
    ?line ok = load_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, 
				  ["Test2", "TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> unload one loaded mib", []),
    ?line ok = unload_mibs(MibsPid, ["Test2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> try unload two loaded mibs and one not loaded", []),
    ?line {error, {'unload aborted at', "Test2", not_loaded}} = 
	   unload_mibs(MibsPid, ["TestTrap","Test2","TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrapv2"]),
    
    ?DBG("load_unload -> unload the remaining loaded mib", []),
    ?line ok = unload_mibs(MibsPid, ["TestTrapv2"]),
    ?line ok = verify_loaded_mibs(MibsPid, MibDir, []),
    
    ?DBG("load_unload -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("load_unload -> stop symbolic store", []),
    ?line sym_stop(),

    ?DBG("load_unload -> done", []),
    ok.


%% ---------------------------------------------------------------------


size_check_ets1(suite) ->
    [];
size_check_ets1(Config) when is_list(Config) ->
    MibStorage = [{module, snmpa_mib_storage_ets}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_ets2(suite) ->
    [];
size_check_ets2(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir, Dir}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_ets2_bad_file1(suite) ->
    [];
size_check_ets2_bad_file1(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    %% Ensure that the bad file does not cause any problems (action = clear)
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,    Dir}, 
			     {action, clear}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_ets3(suite) ->
    [];
size_check_ets3(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,      Dir}, 
			     {checksum, true}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_ets3_bad_file1(suite) ->
    [];
size_check_ets3_bad_file1(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    %% Ensure that the bad file does not cause any problems (action = clear)
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,      Dir}, 
			     {action,   clear}, 
			     {checksum, true}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_dets(suite) ->
    [];
size_check_dets(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),
    MibStorage = [{module,  snmpa_mib_storage_dets}, 
		  {options, [{dir, Dir}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

size_check_mnesia(suite) ->
    [];
size_check_mnesia(Config) when is_list(Config) ->
    MibStorage = [{module,  snmpa_mib_storage_mnesia}, 
		  {options, [{nodes, [node()]}]}], 
    do_size_check([{mib_storage, MibStorage}|Config]).

do_size_check(Config) ->
    ?DBG("do_size_check -> start with"
	 "~n   Config: ~p", [Config]),
    Prio      = normal,
    Verbosity = trace,

    MibStorage = ?config(mib_storage, Config),
    ?DBG("do_size_check -> MibStorage: ~p", [MibStorage]),
    MibDir     = ?config(data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    
    ?DBG("do_size_check -> start symbolic store", []),
    ?line sym_start(Prio, MibStorage, Verbosity),
    ?DBG("do_size_check -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, MibStorage, Verbosity),

    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("do_size_check -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("do_size_check -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?SLEEP(2000),
    ?DBG("do_size_check -> display mem usage", []),
    ?line display_memory_usage(MibsPid),
    
    ?DBG("do_size_check -> unload std mibs", []),
    ?line unload_mibs(MibsPid, StdMibs),
    ?DBG("do_size_check -> unload mibs", []),
    ?line unload_mibs(MibsPid, Mibs),

    ?DBG("do_size_check -> stop mib server", []),
    ?line mibs_stop(MibsPid),
    ?DBG("do_size_check -> stop symbolic store", []),
    ?line sym_stop(),

    ?DBG("do_size_check -> done", []),
    ok.


%% ---------------------------------------------------------------------

me_lookup(suite) -> [];
me_lookup(Config) when is_list(Config) ->
    Prio       = normal,
    Verbosity  = trace,
    MibDir     = ?config(data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       %% "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("me_lookup -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("me_lookup -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("me_lookup -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("me_lookup -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("me_lookup -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ?line ok = me_lookup(MibsPid, ?snmpTrapCommunity_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ?line ok = me_lookup(MibsPid, ?vacmViewSpinLock_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-USER-BASED-SM-MIB", 
	 [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = me_lookup(MibsPid, ?usmStatsNotInTimeWindows_instance),
    
    ?DBG("me_lookup -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("me_lookup -> stop symbolic store", []),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

which_mib(suite) -> [];
which_mib(Config) when is_list(Config) ->
    Prio       = normal,
    Verbosity  = trace,
    MibDir     = ?config(data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Mibs    = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs = ["OTP-SNMPEA-MIB",
	       "SNMP-COMMUNITY-MIB",
	       "SNMP-FRAMEWORK-MIB",
	       "SNMP-MPD-MIB",
	       "SNMP-NOTIFICATION-MIB",
	       "SNMP-TARGET-MIB",
	       %% "SNMP-USER-BASED-SM-MIB",
	       "SNMP-VIEW-BASED-ACM-MIB",
	       "SNMPv2-MIB",
	       "SNMPv2-TC",
	       "SNMPv2-TM"],

    ?DBG("which_mib -> start symbolic store", []),
    ?line sym_start(Prio, Verbosity),

    ?DBG("which_mib -> start mib server", []),
    ?line MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("which_mib -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("which_mib -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("which_mib -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ?line ok = which_mib(MibsPid, ?snmpTrapCommunity_instance, 
			 "SNMP-COMMUNITY-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ?line ok = which_mib(MibsPid, ?vacmViewSpinLock_instance, 
			 "SNMP-VIEW-BASED-ACM-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-USER-BASED-SM-MIB (not loaded)", 
	 [?usmStatsNotInTimeWindows_instance]),
    ?line {error, _} = which_mib(MibsPid, ?usmStatsNotInTimeWindows_instance,
				"SNMP-USER-BASED-SM-MIB"),
    
    ?DBG("which_mib -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("which_mib -> stop symbolic store", []),
    ?line sym_stop(),

    ok.


%% ---------------------------------------------------------------------

cache_test(suite) -> [];
cache_test(Config) when is_list(Config) ->
    ?DBG("cache_test -> start", []),
    Prio       = normal,
    Verbosity  = trace,
    MibStorage = [{module, snmpa_mib_storage_ets}],
    MibDir     = ?config(data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Mibs       = ["Test2", "TestTrap", "TestTrapv2"],
    StdMibs    = ["OTP-SNMPEA-MIB",
		  "SNMP-COMMUNITY-MIB",
		  "SNMP-FRAMEWORK-MIB",
		  "SNMP-MPD-MIB",
		  "SNMP-NOTIFICATION-MIB",
		  "SNMP-TARGET-MIB",
		  %% "SNMP-USER-BASED-SM-MIB",
		  "SNMP-VIEW-BASED-ACM-MIB",
		  "SNMPv2-MIB",
		  "SNMPv2-TC",
		  "SNMPv2-TM"],

    ?DBG("cache_test -> start symbolic store", []),
    ?line sym_start(Prio, MibStorage, Verbosity),

    ?DBG("cache_test -> start mib server", []),
    GcLimit   = 2, 
    Age       = timer:seconds(10), 
    CacheOpts = [{autogc, false}, {age, Age}, {gclimit, GcLimit}],
    ?line MibsPid = mibs_start(Prio, MibStorage, [], Verbosity, CacheOpts), 
    
    ?DBG("cache_test -> load mibs", []),
    ?line load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("cache_test -> load std mibs", []),
    ?line load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("cache_test -> do a simple walk to populate the cache", []),
    ?line ok = walk(MibsPid),
     
    {ok, Sz1} = snmpa_mib:which_cache_size(MibsPid),
    ?DBG("cache_test -> Size1: ~p", [Sz1]),

    ?DBG("cache_test -> sleep 5 secs", []),
    ?SLEEP(timer:seconds(5)),

    ?DBG("cache_test -> perform gc, expect nothing", []),
    {ok, 0} = snmpa_mib:gc_cache(MibsPid),

    ?DBG("cache_test -> sleep 10 secs", []),
    ?SLEEP(timer:seconds(10)),

    ?DBG("cache_test -> perform gc, expect GcLimit", []),
    GcLimit1 = GcLimit + 1, 
    {ok, GcLimit1} = snmpa_mib:gc_cache(MibsPid, Age, GcLimit1),

    Sz2 = Sz1 - GcLimit1, 
    {ok, Sz2} = snmpa_mib:which_cache_size(MibsPid),
    ?DBG("cache_test -> Size2: ~p", [Sz2]),

    ?DBG("cache_test -> enable cache autogc", []),
    ?line ok = snmpa_mib:enable_cache_autogc(MibsPid),

    ?DBG("cache_test -> wait 65 seconds to allow gc to happen", []),
    ?SLEEP(timer:seconds(65)),
    Sz3 = Sz2 - GcLimit, 
    {ok, Sz3} = snmpa_mib:which_cache_size(MibsPid),
    ?DBG("cache_test -> Size3: ~p", [Sz3]),

    ?DBG("cache_test -> "
	 "wait 2 minutes to allow gc to happen, expect empty cache", []),
    ?SLEEP(timer:minutes(2)),
    {ok, 0} = snmpa_mib:which_cache_size(MibsPid),

    ?DBG("cache_test -> stop mib server", []),
    ?line mibs_stop(MibsPid),

    ?DBG("cache_test -> stop symbolic store", []),
    ?line sym_stop(),
    ok.

walk(MibsPid) ->
    MibView = snmpa_acm:get_root_mib_view(),
    do_walk(MibsPid, ?snmpTrapCommunity_instance, MibView),
    do_walk(MibsPid, ?vacmViewSpinLock_instance, MibView),
    do_walk(MibsPid, ?usmStatsNotInTimeWindows_instance, MibView),
    do_walk(MibsPid, ?tDescr_instance, MibView).
    

do_walk(MibsPid, Oid, MibView) ->
    io:format("do_walk -> entry with"
	      "~n   Oid: ~p"
	      "~n", [Oid]),
    case snmpa_mib:next(MibsPid, Oid, MibView) of
	{table, _, _, #me{oid = Oid}} ->
	    ok;
	{table, _, _, #me{oid = Next}} ->
	    do_walk(MibsPid, Next, MibView);
	{variable, #me{oid = Oid}, _} ->
	    ok;
	{variable, #me{oid = Next}, _} ->
	    do_walk(MibsPid, Next, MibView)
    end.


%%======================================================================
%% Internal functions
%%======================================================================

%% -- Mnesia functions

mnesia_start(Opts) ->
    mnesia_start(Opts, [node()]).

mnesia_start(Opts, Nodes) ->
    ?DBG("mnesia_start -> load mnesia", []),
    ?line ok = application:load(mnesia),
    F = fun({Key, Val}) ->
		?DBG("mnesia_start -> set mnesia env: ~n~p -> ~p", [Key,Val]),
		?line application_controller:set_env(mnesia, Key, Val)
	end,
    lists:foreach(F, Opts),
    ?DBG("mnesia_start -> create mnesia schema on ~p", [Nodes]),
    ?line ok = mnesia:create_schema(Nodes),
    ?DBG("mnesia_start -> start mnesia", []),
    ?line ok = application:start(mnesia),
    ok.

mnesia_stop() ->
    ?DBG("mnesia_stop -> stop mnesia", []),
    application:stop(mnesia),
    ?DBG("mnesia_stop -> unload mnesia", []),
    application:unload(mnesia),
    ok.
    
%% - Symbolic Store mini interface

sym_start(Prio, Verbosity) ->
    sym_start(Prio, mib_storage(), Verbosity).

sym_start(Prio, MibStorage, Verbosity) ->
    Opts = [{mib_storage, MibStorage}, {verbosity,Verbosity}],
    {ok, _Pid} = snmpa_symbolic_store:start_link(Prio, Opts),
    ok.

sym_stop() ->
    ok = snmpa_symbolic_store:stop().

sym_info() ->
    snmpa_symbolic_store:info().


%% -- MIB server mini interface 
		   
mibs_start(Prio, Verbosity) when is_atom(Prio) andalso is_atom(Verbosity) ->
    mibs_start(Prio, mib_storage(), [], Verbosity).

mibs_start(Prio, MibStorage, Verbosity) 
  when is_atom(Prio) andalso is_atom(Verbosity) ->
    mibs_start(Prio, MibStorage, [], Verbosity).

mibs_start(Prio, MibStorage, Mibs, Verbosity) 
  when is_atom(Prio)       andalso 
       is_list(Mibs)       andalso 
       is_atom(Verbosity) ->
    mibs_start(Prio, MibStorage, Mibs, Verbosity, []).

mibs_start(Prio, MibStorage, Mibs, Verbosity, CacheOpts) 
  when is_atom(Prio)       andalso 
       is_list(Mibs)       andalso 
       is_atom(Verbosity)  andalso 
       is_list(CacheOpts) ->
    Opts = [{mib_storage, MibStorage}, 
	    {verbosity,   Verbosity}, 
	    {cache,       CacheOpts}],
    {ok, Pid} = snmpa_mib:start_link(Prio, Mibs, Opts),
    Pid.

mibs_stop(Pid) ->
    ok = snmpa_mib:stop(Pid).

mibs_info(Pid) ->
    snmpa_mib:info(Pid).

load_mibs(Pid, Dir, Mibs0) ->
    Mibs = [join(Dir, Mib) || Mib <- Mibs0],
    Res = snmpa_mib:load_mibs(Pid, Mibs),
    %% ?DBG("load_mibs -> "
    %% 	 "~n   Res: ~p", [Res]),
    Res.

unload_mibs(Pid, Mibs) ->
    Res = snmpa_mib:unload_mibs(Pid, Mibs),
    %% ?DBG("unload_mibs -> "
    %% 	 "~n   Res: ~p", [Res]),
    Res.

verify_loaded_mibs(Pid, Dir, ExpectedMibs0) ->
    ExpectedMibs = [join(Dir, Mib) || Mib <- ExpectedMibs0],
    case snmpa_mib:info(Pid, loaded_mibs) of
	ExpectedMibs ->
	    ok;
	LoadedMibs0 ->
	    ?DBG("verify_loaded_mibs -> LoadedMibs0: ~p", [LoadedMibs0]),
	    LoadedMibs = [filename:rootname(FN, ".bin") || FN <- LoadedMibs0],
	    ?DBG("verify_loaded_mibs -> LoadedMibs: ~p", [LoadedMibs]),
	    ExpNotLoadedMibs = ExpectedMibs -- LoadedMibs,
	    LoadedNotExpMibs = LoadedMibs -- ExpectedMibs,
	    ?DBG("verify_loaded_mibs -> "
		 "~n   ExpNotLoadedMibs: ~p"
		 "~n   LoadedNotExpMibs: ~p", 
		 [ExpNotLoadedMibs, LoadedNotExpMibs]),
	    case ExpNotLoadedMibs of
		[] ->
		    case LoadedNotExpMibs of
			[] ->
			    ok;
			_ ->
			    {error, {unexpected_loaded_mibs, LoadedNotExpMibs}}
		    end;
		_ ->
		    case LoadedNotExpMibs of
			[] ->
			    {error, {not_loaded_mibs, ExpNotLoadedMibs}};
			_ ->
			    {error, {unexpected_mibs, 
				     ExpNotLoadedMibs, LoadedNotExpMibs}}
		    end
	    end
	
    end.

me_lookup(Pid, Oid) ->    
    case snmpa_mib:lookup(Pid, Oid) of
	{variable, #me{oid = Oid}} ->
	    ok;
	{variable, #me{oid = OtherOid}} ->
	    case lists:reverse(Oid) of
		[0|Rest] ->
		    case lists:reverse(Rest) of
			OtherOid ->
			    ok;
			AnotherOid ->
			    {error, {invalid_oid, Oid, AnotherOid}}
		    end;
		_ ->
		    {error, {invalid_oid, Oid, OtherOid}}
	    end;
	{table_column, _ME, _TableEntryOid} ->
            ok;
        {subagent, SubAgentPid, _SANextOid} ->
            {error, {subagent, SubAgentPid}};
        {false, Reason} ->
            {error, Reason};
        Else ->
            {error, Else}
    end.

			    
which_mib(Pid, Oid, Mib1) ->    
    case snmpa_mib:which_mib(Pid, Oid) of
	{ok, Mib2} when is_atom(Mib2) ->
	    Mib3 = atom_to_list(Mib2),
	    which_mib(Mib1, Mib3);
	{ok, Mib2} ->
	    which_mib(Mib1, Mib2);
        {error, Reason} ->
            {error, Reason};
        Else ->
            {error, Else}
    end.

which_mib(M, M) ->
    ok;
which_mib(M1, M2) ->
    {error, {invalid_mib, M1, M2}}.


%% Default mib-storage
mib_storage() ->
    [{module, snmpa_mib_storage_ets}].


%% -- 

display_memory_usage(MibsPid) ->
    SymInfo     = sym_info(),
    SymProcSize = key1search(process_memory, SymInfo),
    DbSize      = key1search(db_memory,      SymInfo),
    MibsInfo    = mibs_info(MibsPid),
    TreeSize    = key1search(tree_size_bytes,  MibsInfo),
    MibsProcMem = key1search(process_memory,   MibsInfo),
    MibDbSize   = key1search([db_memory,mib],  MibsInfo),
    NodeDbSize  = key1search([db_memory,node], MibsInfo),
    TreeDbSize  = key1search([db_memory,tree], MibsInfo),
    ?INF("Symbolic store memory usage: "
	"~n   Process memory size: ~p"
	"~n   Db size:             ~p"
	"~n"
	"~nMib server memory usage: "
	"~n   Tree size:           ~p"
	"~n   Process memory size: ~p"
	"~n   Mib db size:         ~p"
	"~n   Node db size:        ~p"
	"~n   Tree db size:        ~p"
	"~n", 
	[SymProcSize, DbSize,
	TreeSize, MibsProcMem, MibDbSize, NodeDbSize, TreeDbSize]).
    
key1search([], Res) ->
    Res;
key1search([Key|Keys], List) when is_atom(Key) andalso is_list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    key1search(Keys, Val);
	false ->
	    undefined
    end;
key1search(Key, List) when is_atom(Key) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    undefined
    end.

join(Dir, File) ->
    filename:join(Dir, File).
