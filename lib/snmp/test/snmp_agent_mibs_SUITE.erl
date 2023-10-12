%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2022. All Rights Reserved.
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

-module(snmp_agent_mibs_SUITE).


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
         suite/0, all/0, groups/0,
         init_per_suite/1,    end_per_suite/1,
         init_per_group/2,    end_per_group/2, 
         init_per_testcase/2, end_per_testcase/2,

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


-define(ALIB, snmp_agent_test_lib).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     start_and_stop, 
     load_unload, 
     {group, size_check},
     me_lookup, 
     which_mib, 
     cache_test
    ].

groups() -> 
    [
     {size_check, [], size_check_cases()}
    ].

size_check_cases() ->
    [
     size_check_ets1,            % Plain ets
     size_check_ets2,            % ets with a file
     size_check_ets2_bad_file1,  % ets with a bad file
     size_check_ets3,            % ets with a checksummed file
     size_check_ets3_bad_file1,  % ets with bad file (checksummed) 
     size_check_dets,            % Plain dets
     size_check_mnesia           % Plain mnesia
    ].



%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->

    ?DBG("init_per_suite -> entry with"
         "~n   Config0: ~p", [Config0]),

    case snmp_test_lib:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            Config2 = ?LIB:init_suite_top_dir(?MODULE, Config1), 

            %% We need a monitor on this node also
            snmp_test_sys_monitor:start(),

            ?DBG("init_per_suite -> done when"
                 "~n   Config: ~p", [Config]),

            Config2
    end.

end_per_suite(Config) when is_list(Config) ->

    ?DBG("end_per_suite -> entry with"
         "~n   Config: ~p", [Config]),

    snmp_test_sys_monitor:stop(),

    ?LIB:end_per_suite(Config).



%%
%% -----
%%

init_per_group(GroupName, Config) ->
    ?LIB:init_group_top_dir(GroupName, Config).

end_per_group(_GroupName, Config) ->
    Config.




%%
%% -----
%%

init_per_testcase(Case, Config0) when is_list(Config0) ->
    snmp_test_global_sys_monitor:reset_events(),
    Config1    = ?LIB:fix_data_dir(Config0),
    CaseTopDir = ?LIB:init_testcase_top_dir(Case, Config1), 
    DbDir      = join(CaseTopDir, "db_dir/"),
    ok   = file:make_dir(DbDir),
    init_per_testcase2(Case, [{db_dir,       DbDir}, 
			      {case_top_dir, CaseTopDir} | Config1]).

init_per_testcase2(size_check_ets2_bad_file1, Config) when is_list(Config) ->
    DbDir = ?config(db_dir, Config),
    %% Create a bad file
    ok = file:write_file(join(DbDir, "snmpa_symbolic_store.db"), 
			 "calvin and hoppes play chess"),
    Factor = ?config(snmp_factor, Config),
    ct:timetrap(?MINS(1 + (Factor div 2))),
    Config;
init_per_testcase2(size_check_ets3_bad_file1, Config) when is_list(Config) ->
    DbDir = ?config(db_dir, Config),
    %% Create a bad file
    ok = file:write_file(join(DbDir, "snmpa_symbolic_store.db"), 
			 "calvin and hoppes play chess"),
    Factor = ?config(snmp_factor, Config),
    ct:timetrap(?MINS(1 + (Factor div 2))),
    Config;
init_per_testcase2(size_check_mnesia, Config) when is_list(Config) ->
    Factor = ?config(snmp_factor, Config),
    ct:timetrap(?MINS(1 + (Factor div 2))),
    Config;
init_per_testcase2(cache_test, Config) when is_list(Config) ->
    Factor = ?config(snmp_factor, Config),
    ct:timetrap(?MINS(10 + (Factor div 2))),
    Config;
init_per_testcase2(_Case, Config) when is_list(Config) ->
    Factor = ?config(snmp_factor, Config),
    ct:timetrap(?MINS(1 + (Factor div 3))),
    Config.


end_per_testcase(Case, Config) when is_list(Config) ->
    ?IPRINT("system events during test: "
            "~n   ~p", [snmp_test_global_sys_monitor:events()]),
    end_per_testcase1(Case, Config).

end_per_testcase1(size_check_mnesia, Config) when is_list(Config) ->
    mnesia_stop(),
    Config;
end_per_testcase1(cache_test, Config) when is_list(Config) ->
    Config;
end_per_testcase1(_Case, Config) when is_list(Config) ->
    Config.


%%======================================================================
%% Test functions
%%======================================================================

start_and_stop(Config) when is_list(Config) ->
    tc_try(start_and_start,
           fun() -> do_start_and_stop(Config) end).

do_start_and_stop(_Config) ->
    Prio      = normal,
    Verbosity = trace,

    sym_start(Prio, Verbosity),
    MibsPid = mibs_start(Prio, Verbosity),

    mibs_info(MibsPid),

    mibs_stop(MibsPid),
    sym_stop(),

    ok.


%% ---------------------------------------------------------------------

load_unload(Config) when is_list(Config) ->
    tc_try(load_unload,
           fun() -> do_load_unload(Config) end).

do_load_unload(Config) ->
    ?DBG("load_unload -> start", []),

    Prio       = normal,
    Verbosity  = log,
    MibDir     = ?config(data_dir, Config),

    ?DBG("load_unload -> start symbolic store", []),
    sym_start(Prio, Verbosity),

    ?DBG("load_unload -> start mib server", []),
    MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("load_unload -> load one not already loaded mib", []),
    ok = verify_loaded_mibs(MibsPid, MibDir, []),
    ok = load_mibs(MibsPid, MibDir, ["Test2"]),
    ok = verify_loaded_mibs(MibsPid, MibDir, ["Test2"]),
    
    ?DBG("load_unload -> try load one *already loaded* mib", []),
    EMib = join(MibDir, "Test2"), 
    {error, {'load aborted at', EMib, already_loaded}} =
	load_mibs(MibsPid, MibDir, ["Test2"]),

    ?DBG("load_unload -> load 2 not already loaded mibs", []),
    ok = load_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    ok = verify_loaded_mibs(MibsPid, MibDir,
				  ["Test2", "TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> unload one loaded mib", []),
    ok = unload_mibs(MibsPid, ["Test2"]),
    ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrap", "TestTrapv2"]),
    
    ?DBG("load_unload -> try unload two loaded mibs and one not loaded", []),
    {error, {'unload aborted at', "Test2", not_loaded}} =
	   unload_mibs(MibsPid, ["TestTrap","Test2","TestTrapv2"]),
    ok = verify_loaded_mibs(MibsPid, MibDir, ["TestTrapv2"]),
    
    ?DBG("load_unload -> unload the remaining loaded mib", []),
    ok = unload_mibs(MibsPid, ["TestTrapv2"]),
    ok = verify_loaded_mibs(MibsPid, MibDir, []),
    
    ?DBG("load_unload -> stop mib server", []),
    mibs_stop(MibsPid),

    ?DBG("load_unload -> stop symbolic store", []),
    sym_stop(),

    ?DBG("load_unload -> done", []),
    ok.


%% ---------------------------------------------------------------------


size_check_ets1(Config) when is_list(Config) ->
    MibStorage = [{module, snmpa_mib_storage_ets}], 
    do_size_check(size_check_ets1,
                  [{mib_storage, MibStorage}|Config]).

size_check_ets2(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir, Dir}]}], 
    do_size_check(size_check_ets2,
                  [{mib_storage, MibStorage}|Config]).

size_check_ets2_bad_file1(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    %% Ensure that the bad file does not cause any problems (action = clear)
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,    Dir}, 
			     {action, clear}]}], 
    do_size_check(size_check_ets2_bad_file1,
                  [{mib_storage, MibStorage}|Config]).

size_check_ets3(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,      Dir}, 
			     {checksum, true}]}], 
    do_size_check(size_check_ets3,
                  [{mib_storage, MibStorage}|Config]).

size_check_ets3_bad_file1(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),    
    %% Ensure that the bad file does not cause any problems (action = clear)
    MibStorage = [{module,  snmpa_mib_storage_ets}, 
		  {options, [{dir,      Dir}, 
			     {action,   clear}, 
			     {checksum, true}]}], 
    do_size_check(size_check_ets3_bad_file1,
                  [{mib_storage, MibStorage}|Config]).

size_check_dets(Config) when is_list(Config) ->
    Dir = ?config(db_dir, Config),
    MibStorage = [{module,  snmpa_mib_storage_dets}, 
		  {options, [{dir, Dir}]}], 
    do_size_check(size_check_dets,
                  [{mib_storage, MibStorage}|Config]).

size_check_mnesia(Config) when is_list(Config) ->
    MibStorage = [{module,  snmpa_mib_storage_mnesia}, 
                  {options, [{nodes, []}]}],
    DbDir      = ?config(db_dir, Config),
    Init       = fun() -> mnesia_start([{dir, DbDir}]), ok end,
    do_size_check(size_check_mnesia,
                  Init,
                  [{mib_storage, MibStorage}|Config]).

do_size_check(Name, Config) ->
    Init = fun() -> ok end,
    do_size_check(Name, Init, Config).

do_size_check(Name, Init, Config) ->
    tc_try(Name, Init, fun() -> do_size_check(Config) end).


do_size_check(Config) ->
    ?IPRINT("do_size_check -> start with"
            "~n   Config: ~p", [Config]),
    Prio      = normal,
    Verbosity = trace,

    MibStorage = ?config(mib_storage, Config),
    ?IPRINT("do_size_check -> MibStorage: ~p", [MibStorage]),
    MibDir     = ?config(data_dir, Config),
    StdMibDir  = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    
    ?IPRINT("do_size_check -> start symbolic store", []),
    sym_start(Prio, MibStorage, Verbosity),
    ?IPRINT("do_size_check -> start mib server", []),
    MibsPid = mibs_start(Prio, MibStorage, Verbosity),

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

    ?IPRINT("do_size_check -> load std mibs", []),
    load_mibs(MibsPid, StdMibDir, StdMibs),
    ?IPRINT("do_size_check -> load (own) mibs", []),
    load_mibs(MibsPid, MibDir, Mibs),

    ?SLEEP(2000),
    ?IPRINT("do_size_check -> display mem usage", []),
    display_memory_usage(MibsPid),
    
    ?IPRINT("do_size_check -> unload (own) mibs", []),
    unload_mibs(MibsPid, Mibs),
    ?IPRINT("do_size_check -> unload std mibs", []),
    unload_mibs(MibsPid, StdMibs),

    ?IPRINT("do_size_check -> stop mib server", []),
    mibs_stop(MibsPid),
    ?IPRINT("do_size_check -> stop symbolic store", []),
    sym_stop(),

    ?IPRINT("do_size_check -> done", []),
    ok.


%% ---------------------------------------------------------------------

me_lookup(Config) when is_list(Config) ->
    tc_try(me_lookup,
           fun() -> do_me_lookup(Config) end).

do_me_lookup(Config) ->
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
    sym_start(Prio, Verbosity),

    ?DBG("me_lookup -> start mib server", []),
    MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("me_lookup -> load mibs", []),
    load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("me_lookup -> load std mibs", []),
    load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("me_lookup -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ok = me_lookup(MibsPid, ?snmpTrapCommunity_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ok = me_lookup(MibsPid, ?vacmViewSpinLock_instance),
    
    ?DBG("me_lookup -> find ~w from SNMP-USER-BASED-SM-MIB", 
	 [?usmStatsNotInTimeWindows_instance]),
    {error, _} = me_lookup(MibsPid, ?usmStatsNotInTimeWindows_instance),
    
    ?DBG("me_lookup -> stop mib server", []),
    mibs_stop(MibsPid),

    ?DBG("me_lookup -> stop symbolic store", []),
    sym_stop(),

    ok.


%% ---------------------------------------------------------------------

which_mib(Config) when is_list(Config) ->
    tc_try(which_mib,
           fun() -> do_which_mib(Config) end).

do_which_mib(Config) ->
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
    sym_start(Prio, Verbosity),

    ?DBG("which_mib -> start mib server", []),
    MibsPid = mibs_start(Prio, Verbosity),
    
    ?DBG("which_mib -> load mibs", []),
    load_mibs(MibsPid, MibDir, Mibs),
    ?DBG("which_mib -> load std mibs", []),
    load_mibs(MibsPid, StdMibDir, StdMibs),

    ?DBG("which_mib -> find ~w from SNMP-COMMUNITY-MIB", 
	 [?snmpTrapCommunity_instance]),
    ok = which_mib(MibsPid, ?snmpTrapCommunity_instance,
			 "SNMP-COMMUNITY-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-VIEW-BASED-ACM-MIB", 
	 [?vacmViewSpinLock_instance]),
    ok = which_mib(MibsPid, ?vacmViewSpinLock_instance,
			 "SNMP-VIEW-BASED-ACM-MIB"),
    
    ?DBG("which_mib -> find ~w from SNMP-USER-BASED-SM-MIB (not loaded)", 
	 [?usmStatsNotInTimeWindows_instance]),
    {error, _} = which_mib(MibsPid, ?usmStatsNotInTimeWindows_instance,
				"SNMP-USER-BASED-SM-MIB"),
    
    ?DBG("which_mib -> stop mib server", []),
    mibs_stop(MibsPid),

    ?DBG("which_mib -> stop symbolic store", []),
    sym_stop(),

    ok.


%% ---------------------------------------------------------------------

cache_test(Config) when is_list(Config) ->
    tc_try(cache_test,
           fun() -> do_cache_test(Config) end).

do_cache_test(Config) ->
    ?IPRINT("cache_test -> start"),
    Prio       = normal,
    %% Verbosity  = trace,
    Verbosity  = info,
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
		  "SNMP-USER-BASED-SM-MIB",
		  "SNMP-VIEW-BASED-ACM-MIB",
		  "SNMPv2-MIB",
		  "SNMPv2-TC",
		  "SNMPv2-TM"],

    ?IPRINT("cache_test -> start symbolic store"),
    sym_start(Prio, MibStorage, silence), % Verbosity),

    ?IPRINT("cache_test -> start mib server"),
    GcLimit   = 3,
    Age       = timer:seconds(10),
    CacheOpts = [{autogc,    false},
                 {age,       Age},
                 {gclimit,   GcLimit},
                 {gcverbose, true}],
    MibsPid = mibs_start(Prio, MibStorage, [], Verbosity, CacheOpts),
    
    ?NPRINT("Info before load mibs: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> load mibs"),
    load_mibs(MibsPid, MibDir, Mibs),

    ?NPRINT("Info before load std mibs: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> load std mibs"),
    load_mibs(MibsPid, StdMibDir, StdMibs),

    ?NPRINT("Info (after mibs load but) before populate: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> populate the cache"),
    ok = populate(MibsPid),

    ?NPRINT("Info after populate: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    Sz1 = cache_sz_verify(1, MibsPid, any),

    ?IPRINT("cache_test -> sleep 5 secs"),
    ?SLEEP(timer:seconds(5)),

    _ = cache_gc_verify(1, MibsPid),

    ?NPRINT("Info after 5 sec sleep: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> sleep 10 secs"),
    ?SLEEP(timer:seconds(10)),

    ?NPRINT("Info after 10 sec sleep: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    GcLimit1 = cache_gc_verify(2, MibsPid, Age, GcLimit + 1),

    Sz2 = cache_sz_verify(2, MibsPid, Sz1 - GcLimit1),


    ?IPRINT("cache_test -> subscribe to GC events"),
    ok = snmpa_mib:subscribe_gc_events(MibsPid),

    ?IPRINT("cache_test -> enable cache autogc"),
    ok = snmpa_mib:enable_cache_autogc(MibsPid),

    ?IPRINT("cache_test -> wait 65 seconds to allow gc to happen"),
    ?SLEEP(timer:seconds(65)),

    ?NPRINT("Info after 65 sec sleep: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> [1] flush expected GC events"),
    {NumEvents1, TotGC1} = cache_flush_gc_events(MibsPid),
    ?IPRINT("cache_test -> GC events: "
            "~n      Number of Events:    ~p"
            "~n      Total elements GCed: ~p", [NumEvents1, TotGC1]),

    _ = cache_sz_verify(3, MibsPid, Sz2 - GcLimit),

    ?IPRINT("cache_test -> "
            "wait 2 minutes to allow gc to happen, expect empty cache"),
    ?SLEEP(timer:minutes(2)),

    ?NPRINT("Info after 2 min sleep: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    _ = cache_sz_verify(4, MibsPid, 0),


    ?IPRINT("cache_test -> change gclimit to infinity"),
    snmpa_mib:update_cache_gclimit(MibsPid, infinity),

    ?IPRINT("cache_test -> change age to ~w mins", [3]),
    snmpa_mib:update_cache_age(MibsPid, ?MINS(3)),

    ?IPRINT("cache_test -> [2] flush expected GC events"),
    {NumEvents2, TotGC2} = cache_flush_gc_events(MibsPid),
    ?IPRINT("cache_test -> GC events: "
            "~n      Number of Events:    ~p"
            "~n      Total elements GCed: ~p", [NumEvents2, TotGC2]),

    ?IPRINT("cache_test -> populate the cache again"),
    populate(MibsPid),

    ?IPRINT("cache_test -> validate cache size"),
    {ok, Sz4} = snmpa_mib:which_cache_size(MibsPid),
    if (Sz4 > 0) ->
            ?IPRINT("cache_test -> expected cache size: ~w > 0", [Sz4]);
       true      ->
            ?EPRINT("cache_test -> cache *not* populated"),
            ?FAIL(cache_not_populated)
    end,    

    ?NPRINT("Info after poulated: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> wait 2 mins - before tuching some entries"),
    ?SLEEP(?MINS(2)),

    %% There should not be anything GC:ed

    receive
        {MibsPid, gc_result, {ok, NGC1}} ->
            ?EPRINT("cache_test -> unexpected GC of ~w elements", [NGC1]),
            exit({unexpected_gc_result, NGC1})
    after 0 ->
            ok
    end,

    ?IPRINT("cache_test -> touch some elements again (update the cache)"),
    populate_lookup(MibsPid),

    ?IPRINT("cache_test -> await partial GC"),
    NumGC2 =
        receive
            {MibsPid, gc_result, {ok, NGC2}} 
              when (NGC2 > 0) andalso (Sz4 > NGC2) ->
                ?NPRINT("cache_test -> "
                        "received partial GC result of ~w elements", [NGC2]),
                NGC2
        end,

    ?NPRINT("Info after partial GC: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),


    ?IPRINT("cache_test -> await final GC"),
    receive
        {MibsPid, gc_result, {ok, NGC3}} 
          when (NGC3 > 0) andalso ((Sz4 - NumGC2) =:= NGC3) ->
            ?NPRINT("cache_test -> "
                    "received final GC result of ~w elements", [NGC3]),
            NGC3;
        Any ->
            ?EPRINT("cache_test -> unexpected message: "
                    "~n      ~p", [Any]),
            ?FAIL({unexpected, Any})
    end,

    ?NPRINT("Info after final GC: "
            "~n      ~p", [snmpa_mib:info(MibsPid)]),

    ?IPRINT("cache_test -> validate cache size (expect empty)"),
    {ok, Sz5} = snmpa_mib:which_cache_size(MibsPid),
    if (Sz5 =:= 0) ->
            ?IPRINT("cache_test -> expected cache size: 0");
       true      ->
            ?EPRINT("cache_test -> cache *not* empty (~w)", [Sz5]),
            ?FAIL({cache_populated, Sz5})
    end,


    ?IPRINT("cache_test -> stop mib server"),
    mibs_stop(MibsPid),

    ?IPRINT("cache_test -> stop symbolic store"),
    sym_stop(),

    ?IPRINT("cache_test -> end"),
    ok.

populate(MibsPid) ->
    %% Make some lookups
    populate_lookup(MibsPid),
    %% Make some walk's
    populate_walk(MibsPid).

populate_lookup(MibsPid) ->    
    {variable, _} = snmpa_mib:lookup(MibsPid, ?snmpTrapCommunity_instance),
    {variable, _} = snmpa_mib:lookup(MibsPid, ?vacmViewSpinLock_instance),
    {variable, _} = snmpa_mib:lookup(MibsPid, ?usmStatsNotInTimeWindows_instance),
    {variable, _} = snmpa_mib:lookup(MibsPid, ?tDescr_instance),
    ok.

populate_walk(MibsPid) ->
    MibView = snmpa_acm:get_root_mib_view(),
    walk(MibsPid, ?snmpTrapCommunity_instance,        MibView),
    walk(MibsPid, ?vacmViewSpinLock_instance,         MibView),
    walk(MibsPid, ?usmStatsNotInTimeWindows_instance, MibView),
    walk(MibsPid, ?tDescr_instance,                   MibView),
    ok.

walk(MibsPid, Oid, MibView) ->
    ?IPRINT("walk -> entry with"
            "~n      Oid: ~p", [Oid]),
    do_walk(MibsPid, Oid, MibView).

do_walk(MibsPid, Oid, MibView) ->
    ?IPRINT("do_walk -> entry with"
            "~n   Oid: ~p"
            "~n", [Oid]),
    case snmpa_mib:next(MibsPid, Oid, MibView) of
	{table, _, _, #me{oid = Oid}} ->
            ?IPRINT("do_walk -> done for table (~p)", [Oid]),
	    ok;
	{table, _, _, #me{oid = Next}} ->
            ?IPRINT("do_walk -> table next ~p", [Next]),
	    do_walk(MibsPid, Next, MibView);
	{variable, #me{oid = Oid}, _} ->
            ?IPRINT("do_walk -> done for variable (~p)", [Oid]),
	    ok;
	{variable, #me{oid = Next}, _} ->
            ?IPRINT("do_walk -> variable next ~p", [Next]),
	    do_walk(MibsPid, Next, MibView)
    end.


cache_gc_verify(ID, MibsPid) ->
    GC = fun() -> snmpa_mib:gc_cache(MibsPid) end,
    cache_gc_verify(ID, GC, 0).

cache_gc_verify(ID, MibsPid, Age, ExpectedGcLimit) ->
    GC = fun() -> snmpa_mib:gc_cache(MibsPid, Age, ExpectedGcLimit) end,
    cache_gc_verify(ID, GC, ExpectedGcLimit).

cache_gc_verify(ID, GC, ExpectedGc) ->
    ?IPRINT("cache_gc_verify -> [~w] perform gc, expect ~w", [ID, ExpectedGc]),
    case GC() of
        {ok, ExpectedGc} ->
            ?IPRINT("cache_gc_verify -> [~w] gc => ok", [ID]),
            ExpectedGc;
        {ok, OtherGc} ->
            ?IPRINT("cache_gc_verify -> [~w] invalid GC limit: "
                    "~n      Expected: ~p"
                    "~n      Got:      ~p",
                    [ID, ExpectedGc, OtherGc]),
            exit({ID, invalid_gc_limit, {ExpectedGc, OtherGc}});
        Unexpected ->
            ?IPRINT("cache_gc_verify -> [~w] unexpected: "
                    "~n      ~p",
                    [ID, Unexpected]),
            exit({ID, unexpected, Unexpected})
    end.


cache_sz_verify(ID, MibsPid, ExpectedSz) ->
    ?IPRINT("cache_sz_verify -> [~w] expect size ~w", [ID, ExpectedSz]),
    case snmpa_mib:which_cache_size(MibsPid) of
        {ok, ExpectedSz} ->
            ?IPRINT("cache_sz_verify -> [~w] sz => ok", [ID]),
            ExpectedSz;
        {ok, UnexpectedSz} when (ExpectedSz =:= any) ->
            ?IPRINT("cache_sz_verify -> [~w] sz => ok (~w)", [ID, UnexpectedSz]),
            UnexpectedSz;
        {ok, UnexpectedSz} ->
            ?IPRINT("cache_sz_verify -> [~w] invalid size: "
                    "~n      Expected: ~p"
                    "~n      Got:      ~p",
                    [ID, ExpectedSz, UnexpectedSz]),
            exit({ID, invalid_size, {ExpectedSz, UnexpectedSz}});
        Unexpected ->
            ?IPRINT("cache_sz_verify -> [~w] unexpected: "
                    "~n      ~p",
                    [ID, Unexpected]),
            exit({ID, unexpected, Unexpected})
    end.


cache_flush_gc_events(MibServer) ->
    cache_flush_gc_events(MibServer, 0, 0).

cache_flush_gc_events(MibServer, NumEvents, TotGC) ->
    receive
        {MibServer, gc_result, {ok, NumGC}} ->
            ?IPRINT("cache_flush_gc_events -> GC event ~w (~w)",
                    [NumGC, NumEvents]),
            cache_flush_gc_events(MibServer, NumEvents+1, TotGC+NumGC)
    after 0 ->
            if
                (NumEvents =:= 0) andalso (TotGC =:= 0) ->
                    ?IPRINT("cache_flush_gc_events -> no GC events"),
                    exit(no_gc_events);
                true ->
                    {NumEvents, TotGC}
            end
    end.


%%======================================================================
%% Internal functions
%%======================================================================

%% -- Mnesia functions

mnesia_start(Opts) ->
    mnesia_start(Opts, [node()]).

mnesia_start(Opts, Nodes) ->
    %% We can accept mnesia being loaded but *not* started.
    %% If its started it *may* contain data that will invalidate
    %% this test case.
    ?IPRINT("mnesia_start -> try load mnesia when:"
            "~n   Loaded:  ~p"
            "~n   Running: ~p", [apps_loaded(), apps_running()]),
    ok = case application:load(mnesia) of
                   ok ->
                       ok;
                   {error, {already_loaded, mnesia}} ->
                       ok;
                   {error, _} = ERROR ->
                       ERROR
               end,
    F = fun({Key, Val}) ->
		?IPRINT("mnesia_start -> try set mnesia env: "
                        "~n   ~p -> ~p", [Key, Val]),
		application_controller:set_env(mnesia, Key, Val)
	end,
    lists:foreach(F, Opts),
    ?IPRINT("mnesia_start -> create mnesia schema on ~p", [Nodes]),
    case mnesia:create_schema(Nodes) of
              ok ->
                  ok;
              {error, {_, {already_exist, _}}} ->
                  throw({skip, "Mnesia schema already exists"});
              {error, SchemaReason} ->
                  throw({skip,
                         ?F("Failed create mnesia schema: ~p", [SchemaReason])})
          end,
    ?IPRINT("mnesia_start -> start mnesia", []),
    case application:start(mnesia) of
              ok ->
                  ok;
              {error, {already_started, mnesia}} ->
                  throw({skip, "Mnesia already started"});
              {error, StartReason} ->
                  throw({skip,
                         ?F("Failed starting mnesia: ~p", [StartReason])})
          end,
    ?IPRINT("mnesia_start -> mnesia started", []),
    ok.

mnesia_stop() ->
    ?IPRINT("mnesia_stop -> try stop mnesia when:"
            "~n   Loaded:  ~p"
            "~n   Running: ~p", [apps_loaded(), apps_running()]),
    application:stop(mnesia),
    ?IPRINT("mnesia_stop -> try unload mnesia when"
            "~n   Loaded:  ~p"
            "~n   Running: ~p", [apps_loaded(), apps_running()]),
    application:unload(mnesia),
    ?IPRINT("mnesia_stop -> done when:"
            "~n   Loaded:  ~p"
            "~n   Running: ~p", [apps_loaded(), apps_running()]),
    ok.

apps_loaded() ->
    [App || {App, _, _} <- application:loaded_applications()].

apps_running() ->
    [App || {App, _, _} <- application:which_applications()].


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
    Res = snmpa_mib:load_mibs(Pid, Mibs, false),
    %% ?DBG("load_mibs -> "
    %% 	 "~n   Res: ~p", [Res]),
    Res.

unload_mibs(Pid, Mibs) ->
    Res = snmpa_mib:unload_mibs(Pid, Mibs, false),
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

tc_try(Name, TC) ->
    tc_try(Name, fun() -> ok end, TC).

tc_try(Name, Init, TC)
  when is_atom(Name) andalso is_function(Init, 0) andalso is_function(TC, 0) ->
    Pre = fun() ->
                  {ok, Peer, Node} = ?START_PEER(atom_to_list(Name)),
                  ok = run_on(Node, Init),
                  {Peer, Node}
          end,
    Case = fun({_Peer, Node}) ->
                   monitor_node(Node, true),
                   Pid = spawn_link(Node, TC),
                   receive
                       %% No test case could/would provoke a nodedown => SKIP
                       {nodedown, Node} = N ->
                           ?NPRINT("unexpected node down ~p", [Node]),
                           exit({skip, N});
                       {'EXIT', Pid, normal} ->
                           monitor_node(Node, false),                           
                           ok;
                       {'EXIT', Pid, ok} ->
                           monitor_node(Node, false),                           
                           ok;
                       %% This is a node down.
                       %% Its just a race that this came before the actual
                       %% 'nodedown'.
                       %% Also, there is no normal test case that could provoke
                       %% a node down, so this is *not* our fault => SKIP
                       {'EXIT', Pid, noconnection = Reason} ->
                           ?NPRINT("unexpected '~p' termination", [Reason]),
                           monitor_node(Node, false), % Just in case
                           exit({skip, Reason});
                       {'EXIT', Pid, Reason} ->
                           monitor_node(Node, false),
                           exit(Reason)
                   end
           end,
    Post = fun({Peer, Node}) ->
                   receive
                       {nodedown, Node} ->
                           ?NPRINT("node ~p (already) stopped", [Node]),
                           ok
                   after 0 ->
                           monitor_node(Node, true),
                           ?NPRINT("try stop node ~p", [Node]),
                           peer:stop(Peer),
                           receive
                               {nodedown, Node} ->
                                   ?NPRINT("node ~p stopped", [Node]),
                                   ok
                           end
                   end
           end,
    ?TC_TRY(Name, Pre, Case, Post).
    
run_on(Node, F) when is_atom(Node) andalso is_function(F, 0) ->
    monitor_node(Node, true),
    Pid = spawn_link(Node, F),
    receive
        {nodedown, Node} = N ->
            exit(N);
        {'EXIT', Pid, normal} ->
            monitor_node(Node, false),                           
            ok;
        {'EXIT', Pid, Reason} ->
            monitor_node(Node, false),                           
            Reason
    end.


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
    ?IPRINT("Symbolic store memory usage: "
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
