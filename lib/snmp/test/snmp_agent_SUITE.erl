%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2023. All Rights Reserved.
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

-module(snmp_agent_SUITE).

-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2, 

	 %% major_tcs - misc
	 app_info/1, 
	 info_test/1, 
         create_local_db_dir/1,

	 %% major_tcs - test_v1
	 simple/1, 
	 db_notify_client/1, 
	 v1_processing/1, 
	 big/1, 
	 big2/1,
	 loop_mib_1/1,
	 api/1, 
	 subagent/1, 
	 mnesia/1, 
	 sa_register/1, 
	 v1_trap/1, 
	 sa_error/1, 
	 next_across_sa/1, 
	 undo/1,
	 sparse_table/1, 
	 cnt_64/1, 
	 opaque/1, 
	 change_target_addr_config/1, 

	 %% major_tcs - test_v1 - multiple_reqs
	 mul_get/1, 
	 mul_get_err/1, 
	 mul_next/1, 
	 mul_next_err/1,
	 mul_set/1, 
	 mul_set_err/1, 

	 %% major_tcs - test_v1 - reported_bugs
	 otp_1128/1, 
	 otp_1129/1, 
	 otp_1131/1, 
	 otp_1162/1, 
	 otp_1222/1,
	 otp_1298/1, 
	 otp_1331/1, 
	 otp_1338/1, 
	 otp_1342/1, 
	 otp_1366/1, 
	 otp_2776/1,
	 otp_2979/1, 
	 otp_3187/1, 
	 otp_3725/1, 

	 %% major_tcs - test_v1 - standard_mibs
	 snmp_standard_mib/1, 
	 snmp_community_mib/1,
	 snmp_framework_mib/1, 
	 snmp_target_mib/1,
	 snmp_notification_mib/1, 
	 snmp_view_based_acm_mib/1, 

	 %% major_tcs - test_v2
	 simple_2/1, 
	 v2_processing/1, 
	 big_2/1, 
	 big2_2/1, 
	 loop_mib_2/1,
	 api_2/1, 
	 subagent_2/1, 
	 mnesia_2/1, 
	 sa_register_2/1, 
	 v2_trap/1, 
	 sa_error_2/1,
	 next_across_sa_2/1, 
	 undo_2/1, 
	 v2_types/1, 
	 implied/1,
	 sparse_table_2/1, 
	 cnt_64_2/1, 
	 opaque_2/1, 
	 v2_caps/1,

	 %% major_tcs - test_v2 - multiple_reqs_2
	 mul_get_2/1, 
	 mul_get_err_2/1, 
	 mul_next_2/1, 
	 mul_next_err_2/1,
	 mul_set_2/1, 
	 mul_set_err_2/1, 

	 %% major_tcs - test_v2 - v2_inform
	 v2_inform_i/1, 

	 %% major_tcs - test_v2 - reported_bugs_2
	 otp_1128_2/1, 
	 otp_1129_2/1, 
	 otp_1131_2/1, 
	 otp_1162_2/1,
	 otp_1222_2/1, 
	 otp_1298_2/1, 
	 otp_1331_2/1, 
	 otp_1338_2/1,
	 otp_1342_2/1, 
	 otp_1366_2/1, 
	 otp_2776_2/1, 
	 otp_2979_2/1, 
	 otp_3187_2/1, 

	 %% major_tcs - test_v2 - standard_mibs_2
	 snmpv2_mib_2/1, 
	 snmp_community_mib_2/1,
	 snmp_framework_mib_2/1, 
	 snmp_target_mib_2/1,
	 snmp_notification_mib_2/1, 
	 snmp_view_based_acm_mib_2/1,

	 %% major_tcs - test_v1_v2
	 simple_bi/1, 

	 %% major_tcs - test_v3
	 simple_3/1, 
	 v3_processing/1, 
	 big_3/1, 
	 big2_3/1, 
	 api_3/1,
	 subagent_3/1, 
	 mnesia_3/1, 
	 loop_mib_3/1, 
	 sa_register_3/1, 
	 v3_trap/1, 
	 sa_error_3/1,
	 next_across_sa_3/1, 
	 undo_3/1, 
	 v2_types_3/1, 
	 implied_3/1, 
	 sparse_table_3/1, 
	 cnt_64_3/1,
	 opaque_3/1, 
	 v2_caps_3/1, 

	 %% major_tcs - test_v3 - multiple_reqs_3
	 mul_get_3/1, 
	 mul_get_err_3/1, 
	 mul_next_3/1, 
	 mul_next_err_3/1, 
	 mul_set_3/1,
	 mul_set_err_3/1,

	 %% major_tcs - test_v3 - v3_inform
	 v3_inform_i/1, 

	 %% major_tcs - test_v3 - reported_bugs_3
	 otp_1128_3/1, 
	 otp_1129_3/1, 
	 otp_1131_3/1, 
	 otp_1162_3/1,
	 otp_1222_3/1, 
	 otp_1298_3/1, 
	 otp_1331_3/1, 
	 otp_1338_3/1,
	 otp_1342_3/1, 
	 otp_1366_3/1, 
	 otp_2776_3/1, 
	 otp_2979_3/1, 
	 otp_3187_3/1,
	 otp_3542/1,

	 %% major_tcs - test_v3 - standard_mibs_3
	 snmpv2_mib_3/1, 
	 snmp_framework_mib_3/1, 
	 snmp_mpd_mib_3/1,
	 snmp_target_mib_3/1, 
	 snmp_notification_mib_3/1,
	 snmp_view_based_acm_mib_3/1, 
	 snmp_user_based_sm_mib_3/1,

	 %% major_tcs - test_v3 - v3_security
	 v3_crypto_basic/1, 
	 v3_md5_auth/1, 
	 v3_sha_auth/1,
	 v3_sha224_auth/1,
	 v3_sha256_auth/1,
	 v3_sha384_auth/1,
	 v3_sha512_auth/1,
	 v3_des_priv/1, 

	 %% major_tcs - test_multi_threaded, test_multi_threaded_ext
	 multi_threaded/1, 
	 mt_trap/1, 
	 
	 %% major_tcs - mib_storage - mib_storage_ets
	 mse_simple/1, 
	 mse_v1_processing/1, 
	 mse_big/1, 
	 mse_big2/1,
	 mse_loop_mib/1, 
	 mse_api/1, 
	 mse_sa_register/1, 
	 mse_v1_trap/1,
	 mse_sa_error/1, 
	 mse_next_across_sa/1, 
	 mse_undo/1,
	 mse_standard_mib/1, 
	 mse_community_mib/1, 
	 mse_framework_mib/1,
	 mse_target_mib/1, 
	 mse_notification_mib/1,
	 mse_view_based_acm_mib/1, 
	 mse_sparse_table/1, 
	 mse_me_of/1,
	 mse_mib_of/1, 

	 %% major_tcs - mib_storage - mib_storage_dets
	 msd_simple/1, 
	 msd_v1_processing/1, 
	 msd_big/1, 
	 msd_big2/1,
	 msd_loop_mib/1, 
	 msd_api/1, 
	 msd_sa_register/1, 
	 msd_v1_trap/1,
	 msd_sa_error/1, 
	 msd_next_across_sa/1, 
	 msd_undo/1,
	 msd_standard_mib/1, 
	 msd_community_mib/1, 
	 msd_framework_mib/1,
	 msd_target_mib/1, 
	 msd_notification_mib/1,
	 msd_view_based_acm_mib/1, 
	 msd_sparse_table/1, 
	 msd_me_of/1,
	 msd_mib_of/1, 

	 %% major_tcs - mib_storage - mib_storage_mnesia
	 msm_simple/1, 
	 msm_v1_processing/1, 
	 msm_big/1, 
	 msm_big2/1,
	 msm_loop_mib/1, 
	 msm_api/1, 
	 msm_sa_register/1, 
	 msm_v1_trap/1,
	 msm_sa_error/1, 
	 msm_next_across_sa/1, 
	 msm_undo/1,
	 msm_standard_mib/1, 
	 msm_community_mib/1, 
	 msm_framework_mib/1,
	 msm_target_mib/1, 
	 msm_notification_mib/1,
	 msm_view_based_acm_mib/1, 
	 msm_sparse_table/1, 
	 msm_me_of/1,
	 msm_mib_of/1, 

	 %% major_tcs - mib_storage - mse_size_check
	 mse_size_check/1, 

	 %% major_tcs - mib_storage - msd_size_check
	 msd_size_check/1, 

	 %% major_tcs - mib_storage - msm_size_check
	 msm_size_check/1, 

	 %% major_tcs - mib_storage - varm_mib_storage_dets
	 msd_varm_mib_start/1, 

	 %% major_tcs - mib_storage - varm_mib_storage_mnesia
	 msm_varm_mib_start/1, 

	 %% major_tcs - tickets1 - otp4394
	 otp_4394/1, 

	 %% major_tcs - tickets1 - otp7157
	 otp_7157/1,

         %% major_tcs - tickets1 - otp16092
         otp_16092_simple_start_and_stop1/1,
         otp_16092_simple_start_and_stop2/1,
         otp_16092_simple_start_and_stop3/1,
         otp_16092_simple_start_and_stop4/1,

	 %% tickets2
	 otp8395/1, 
	 otp9884/1,
         otp16649_1/1,
         otp16649_2/1,
         otp16649_3/1,
         otp16649_4/1,
         otp16649_5/1,
         otp16649_6/1,
         otp16649_7/1
	]).

%% Internal exports
-export([dummy_manager_init/2, 
	 v3_sync/1, 
	 v3_inform_sync/1, 
	 v2_caps_i/1, 
	 v1_proc/0, 
	 v2_proc/0, 
	 big_test/0, 
	 big_test_2/0, 
	 simple_standard_test/0, 
	 db_notify_client_test/0, 
	 notify/2, 
	 multi_threaded_test/0, 
	 mt_trap_test/2, 
	 types_v2_test/0, 
	 implied_test/1, 
	 sparse_table_test/0, 
	 cnt_64_test/1, 
	 opaque_test/0, 
	 api_test/1, 
	 unreg_test/0, 
	 load_test/0, 
	 load_test_sa/0, 
	 api_test2/0, 
	 api_test3/0, 
	 do_mul_get/0, 
	 do_mul_get_err/0, 
	 do_mul_next/0, 
	 do_mul_next_err/0, 
	 do_mul_set/0, 
	 do_mul_set_err/0, 
	 sa_mib/0, 
	 ma_trap1/1, 
	 ma_trap2/1, 
	 ma_v2_2_v1_trap/1, 
	 ma_v2_2_v1_trap2/1, 
	 sa_trap1/1, 
	 sa_trap2/1, 
	 sa_trap3/1, 
	 ma_v2_trap1/1, 
	 ma_v2_trap2/1, 
	 ma_v2_inform1/1, 
	 ma_v2_inform2/1, 
	 ma_v2_inform3/1, 
	 delivery_targets/3,  
	 delivery_info/4, 
	 ma_v1_2_v2_trap/1, 
	 ma_v1_2_v2_trap2/1, 
	 sa_v1_2_v2_trap1/1, 
	 sa_v1_2_v2_trap2/1, 
	 sa_v1_2_v2_trap3/1, 
	 sa_errs_bad_value/0, 
	 sa_errs_gen_err/0, 
	 sa_too_big/0, 
	 next_across_sa_test/0, 
	 undo_test/0, 
	 bad_return/0, 
	 standard_mib_a/0, 
	 std_mib_read/0, 
	 std_mib_write/0, 
	 std_mib_init/0, 
	 std_mib_finish/0, 
	 standard_mib_test_finish/0, 
	 std_mib_asn_err/0, 
	 snmpv2_mib_test_finish/0, 
	 std_mib_a/0, 
	 std_mib_b/1, 
	 std_mib_c/1, 
	 snmpv2_mib_a/0, 
	 snmp_community_mib_test/0, 
	 snmp_framework_mib_test/0, 
	 snmp_mpd_mib_a/0, 
	 snmp_mpd_mib_b/0, 
	 snmp_mpd_mib_c/1, 
	 snmp_target_mib_test/0, 
	 snmp_notification_mib_test/0, 
	 do_set/1, 
	 add_row/1, 
	 del_row/1, 
	 use_no_rights/0, 
	 use_rights/0, 
	 usm_add_user1/0, 
	 usm_use_user/0, 
	 usm_key_change1/2, 
	 usm_key_change2/4, 
	 usm_key_change3/4, 
	 usm_read/0, 
	 usm_del_user/0, 
	 usm_bad/0, 
	 loop_mib_1_test/0,
	 loop_mib_2_test/0,
	 loop_mib_3_test/0,
	 otp_1129_i/1, 
	 otp_1162_test/0, 
	 otp_1131_test/0, 
	 otp_1222_test/0, 
	 otp_1298_test/0, 
	 otp_1331_test/0, 
	 otp_1338_test/0, 
	 otp_1342_test/0, 
	 otp_1366_test/0, 
	 otp_1128_test/0, 
	 otp_2776_test/0, 
	 otp_2979_test/0, 
	 otp_3542_test/0, 
	 otp_3725_test/1, 
	 otp_4394_test/0, 
	 otp_7157_test/1, 
	 otp9884_backup/4, 
	 agent_log_validation/0, 
	 mnesia_init/1, 
	 mnesia_start/0, 
	 mnesia_stop/0, 
	 start_standalone_agent/1, 
	 stop_standalone_agent/1, 
	 start_standalone_manager/1, 
	 stop_standalone_manager/1, 
	 do_info/1
	]).

-define(application, snmp).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("snmp_test_lib.hrl").
-define(SNMP_USE_V3, true).
-include_lib("snmp/include/snmp_types.hrl").
-include_lib("snmp/src/agent/snmpa_atl.hrl").

-define(ALIB, snmp_agent_test_lib).

-define(klas1, [1,3,6,1,2,1,7]).
-define(klas2, [1,3,6,1,2,1,9]).
-define(klas3, [1,3,6,1,2,1,8,1]).
-define(klas4, [1,3,6,1,2,1,8,4]).
-define(sa, [1,3,6,1,4,1,193,2]).
-define(system, [1,3,6,1,2,1,1]).
-define(snmp, [1,3,6,1,2,1,11]).
-define(sysDescr_instance, [1,3,6,1,2,1,1,1,0]).
-define(sysObjectID_instance, [1,3,6,1,2,1,1,2,0]).
-define(sysUpTime_instance, [1,3,6,1,2,1,1,3,0]).
-define(snmpTraps, [1,3,6,1,6,3,1,1,5]).
-define(ericsson, [1,3,6,1,4,1,193]).
-define(testTrap, [1,3,6,1,2,1,15,0]).
-define(xDescr, [1,3,6,1,2,1,17,1]).
-define(xDescr2, [1,3,6,1,2,1,17,2]).

-define(active, 1).
-define(notInService, 2).
-define(notReady, 3).
-define(createAndGo, 4).
-define(createAndWait, 5).
-define(destroy, 6).

-define(TRAP_UDP, 5000).

-define(MGR_PORT,       5000).
-define(MGR_MMS,        1024).
-define(MGR_ENGINE_ID,  "mgrEngine").


-define(tooBigStr, "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff").

-define(str(X), snmp_pdus:bits_to_str(X)).

-define(break(), begin io:format(user, "break at line ~w: pid: ~p\n",
				 [?LINE, self()]),
		       receive cont -> ok end
		 end).


-define(v1_2(V1,V2),
	       case get(vsn) of
		   v1 -> V1;
		   _ -> V2
	       end).
		        
-define(v1_2_3(V1,V2,V3),
	       case get(vsn) of
		   v1 -> V1;
		   v2 -> V2;
		   _  -> V3
	       end).


-define(expect1(What), 
	?ALIB:expect(?MODULE, ?LINE, What)).
-define(expect2(What, ExpVBs), 
	?ALIB:expect(?MODULE, ?LINE, What, ExpVBs)).
-define(expect3(Err, Idx, ExpVBs), 
	?ALIB:expect(?MODULE, ?LINE, Err, Idx, ExpVBs)).
-define(expect4(Err, Idx, ExpVBs, To), 
	?ALIB:expect(?MODULE, ?LINE, Err, Idx, ExpVBs, To)).
-define(expect5(Type, Ent, Gen, Spec, ExpVBs), 
	?ALIB:expect(?MODULE, ?LINE, Type, Ent, Gen, Spec, ExpVBs)).
-define(expect6(Type, Ent, Gen, Spec, ExpVBs, To), 
	?ALIB:expect(?MODULE, ?LINE, Type, Ent, Gen, Spec, ExpVBs, To)).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    %% This is a temporary measure to ensure that we can 
    %% test the socket backend without effecting *all*
    %% applications on *all* machines.
    %% This flag is set only for *one* host.
    %% case ?TEST_INET_BACKENDS() of
    case ?TEST_INET_BACKENDS() of
        true ->
            [
             {group, inet_backend_default},
             {group, inet_backend_inet},
             {group, inet_backend_socket}
            ];
        _ ->
            [
             {group, inet_backend_default}
            ]
    end.

groups() -> 
    [
     {inet_backend_default,          [], inet_backend_default_cases()},
     {inet_backend_inet,             [], inet_backend_inet_cases()},
     {inet_backend_socket,           [], inet_backend_socket_cases()},

     {all,                           [], all_cases()},
     {major_tcs,                     [], major_cases()},
     {mib_storage,                   [], mib_storage_cases()}, 
     {mib_storage_ets,               [], mib_storage_ets_cases()},
     {mib_storage_dets,              [], mib_storage_dets_cases()},
     {mib_storage_mnesia,            [], mib_storage_mnesia_cases()},
     {mib_storage_size_check_ets,    [], mse_size_check_cases()},
     {mib_storage_size_check_dets,   [], msd_size_check_cases()},
     {mib_storage_size_check_mnesia, [], msm_size_check_cases()},
     {mib_storage_varm_dets,         [], varm_mib_storage_dets_cases()},
     {mib_storage_varm_mnesia,       [], varm_mib_storage_mnesia_cases()},
     {misc,                          [], misc_cases()}, 
     {test_v1,                       [], v1_cases()},
     {test_v1_ipv6,                  [], v1_cases_ipv6()},
     {test_v2,                       [], v2_cases()},
     {test_v2_ipv6,                  [], v2_cases_ipv6()},
     {test_v1_v2,                    [], v1_v2_cases()},
     {test_v1_v2_ipv6,               [], v1_v2_cases()},
     {test_v3,                       [], v3_cases()},
     {test_v3_ipv6,                  [], v3_cases_ipv6()},
     {test_multi_threaded,           [], mt_cases()},
     {test_multi_threaded_ext,       [], mt_cases()},
     {multiple_reqs,                 [], mul_cases()},
     {multiple_reqs_2,               [], mul_cases_2()},
     {multiple_reqs_3,               [], mul_cases_3()},
     {v2_inform,                     [], v2_inform_cases()}, 
     {v3_inform,                     [], v3_inform_cases()}, 
     {v3_security,                   [], v3_security_cases()}, 
     {standard_mibs,                 [], standard_mibs_cases()}, 
     {standard_mibs_ipv6,            [], standard_mibs_cases_ipv6()},
     {standard_mibs_2,               [], standard_mibs2_cases()}, 
     {standard_mibs_3,               [], standard_mibs3_cases()}, 
     {reported_bugs,                 [], reported_bugs_cases()}, 
     {reported_bugs_2,               [], reported_bugs2_cases()}, 
     {reported_bugs_3,               [], reported_bugs3_cases()}, 
     {tickets1,                      [], tickets1_cases()}, 
     {otp4394,                       [], [otp_4394]},
     {otp7157,                       [], [otp_7157]},
     {otp16092,                      [], otp16092_cases()},

     %% These all belong to group 'tickets2'
     {tickets2,                      [], tickets2_cases()}, 
     {otp16649,                      [], otp16649_cases()},
     {otp16649_ipv4,                 [], otp16649_gen_cases()},
     {otp16649_ipv6,                 [], otp16649_gen_cases()}
    ].


inet_backend_default_cases() ->
    [{all, [], all_cases()}].

inet_backend_inet_cases() ->
    [{all, [], all_cases()}].

inet_backend_socket_cases() ->
    [{all, [], all_cases()}].


%% The agent and the nodes of the agent and manager
%% is started at group init and then lives until
%% group end!
major_cases() ->
    [
     {group, misc}, 
     {group, test_v1}, 
     {group, test_v2},
     {group, test_v1_v2}, 
     {group, test_v3},
     {group, test_v1_ipv6},
     {group, test_v2_ipv6},
     {group, test_v1_v2_ipv6},
     {group, test_v3_ipv6},
     {group, test_multi_threaded}, 
     {group, test_multi_threaded_ext}, 
     {group, mib_storage},
     {group, tickets1}
    ].
    
all_cases() -> 
    [
     %% The agent and the nodes of the agent and manager
     %% is started at group init and then lives until
     %% group end!
     {group, major_tcs},

     %% The test cases in the 'tickets2' group
     %% require control of start and stop of agents and inodes!
     {group, tickets2}
    ].


tickets1_cases() ->
    [
     {group, otp4394}, 
     {group, otp7157}, 
     {group, otp16092}
    ].


tickets2_cases() ->
    [
     otp8395, 
     otp9884,
     {group, otp16649}
    ].

otp16649_cases() ->
    [
     {group, otp16649_ipv4},
     {group, otp16649_ipv6}
    ].

otp16649_gen_cases() ->
    [
     otp16649_1,
     otp16649_2,
     otp16649_3,
     otp16649_4,
     otp16649_5,
     otp16649_6,
     otp16649_7
    ].



%%
%% -----
%%

init_per_suite(Config0) when is_list(Config0) ->

    ?IPRINT("init_per_suite -> entry with"
            "~n   Config:                ~p"
            "~n   Nodes:                 ~p"
            "~n   explicit inet backend: ~p"
            "~n   test inet backends:    ~p",
            [Config0, erlang:nodes(),
             ?EXPLICIT_INET_BACKEND(),
             ?TEST_INET_BACKENDS()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            Config2   = ?LIB:init_suite_top_dir(?MODULE, Config1), 
            Config3   = ?LIB:fix_data_dir(Config2),

            %% Mib-dirs
            MibDir    = ?LIB:lookup(data_dir, Config3),
            StdMibDir = join([code:priv_dir(snmp), "mibs"]),

            Config4 = [{mib_dir, MibDir}, {std_mib_dir, StdMibDir} | Config3],

            %% We need a monitor on this node also
            snmp_test_sys_monitor:start(),

            snmp_test_mgr_counter_server:start(), 

            ?IPRINT("init_per_suite -> end when"
                    "~n      Config: ~p"
                    "~n      Nodes:  ~p", [Config4, erlang:nodes()]),

            Config4
    end.

end_per_suite(Config0) when is_list(Config0) ->

    ?IPRINT("end_per_suite -> entry with"
            "~n   Config0: ~p"
            "~n   Nodes:   ~p", [Config0, erlang:nodes()]),

    case snmp_test_mgr_counter_server:stop() of
    	{ok, Counters} ->
    	    ?IPRINT("end_per_suite -> successfully stopped counter server"
                    "~n      Counters: ~p", [Counters]);
    
    	{error, Reason} ->
    	    ?IPRINT("end_per_suite -> failed stopping counter server"
                    "~n      Reason: ~p", [Reason])
    end,
    snmp_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    ?IPRINT("end_per_suite -> end when"
            "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.



%%
%% -----
%%

init_per_group(GroupName, Config0) ->
    ?IPRINT("init_per_group -> entry with"
            "~n      GroupName: ~p"
            "~n      Config:    ~p"
            "~n   when"
            "~n      Nodes:     ~p",
            [GroupName, Config0, nodes()]),

    Config1 = init_per_group2(GroupName, Config0),

    ?IPRINT("init_per_group -> done when"
            "~n      GroupName: ~p"
            "~n      Config:    ~p"
            "~n      Nodes:     ~p",
            [GroupName, Config1, nodes()]),

    Config1.


init_per_group2(inet_backend_default = _GroupName, Config0) ->
    Config1 = [{socket_create_opts, []} | Config0],
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            ?LIB:init_group_top_dir(default, Config1);
        false ->
            %% For a "standard" test (that is if we do not run the "extended"
            %% inet backends test) then we should always run this group!
            %% So, if we have an extended test, *then* (and only then) 
            %% check the factor.
            case ?TEST_INET_BACKENDS() of
                true ->
                    case lists:keysearch(snmp_factor, 1, Config0) of
                        {value, {snmp_factor, Factor}} when (Factor < 3) ->
                            ?LIB:init_group_top_dir(default, Config1);
                        _ ->
                            {skip, "Machine too slow"}
                    end;
                _ ->
                    ?LIB:init_group_top_dir(default, Config1)
            end                    
    end;
init_per_group2(inet_backend_inet = _GroupName, Config0) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            case lists:keysearch(snmp_factor, 1, Config0) of
                {value, {snmp_factor, Factor}} when (Factor < 5) ->
                    Config1 = [{socket_create_opts, [{inet_backend, inet}]} |
                               Config0],
                    ?LIB:init_group_top_dir(inet, Config1);
                _ ->
                    {skip, "Machine too slow"}
            end
    end;
init_per_group2(inet_backend_socket = _GroupName, Config0) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the *default* group should be run!
            {skip, "explicit inet backend"};
        false ->
            %% Always run this unless a backend has been explicitly
            %% configured (since this is really what we want to test).
            Config1 = [{socket_create_opts, [{inet_backend, socket}]} |
                       Config0],
            ?LIB:init_group_top_dir(socket, Config1)
    end;
init_per_group2(major_tcs = GroupName, Config) ->
    init_all(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(otp7157 = GroupName, Config) -> 
    otp_7157_init(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(otp4394 = GroupName, Config) -> 
    otp_4394_init(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(v2_inform = GroupName, Config) -> 
    init_v2_inform(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(v3_inform = GroupName, Config) -> 
    init_v3_inform(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(multiple_reqs = GroupName, Config) -> 
    init_mul(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(multiple_reqs_2 = GroupName, Config) -> 
    init_mul(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(multiple_reqs_3 = GroupName, Config) -> 
    init_mul(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(test_multi_threaded = GroupName, Config) -> 
    init_mt(snmp_test_lib:init_group_top_dir(GroupName, Config), true);
init_per_group2(test_multi_threaded_ext = GroupName, Config) -> 
    init_mt(snmp_test_lib:init_group_top_dir(GroupName, Config), extended);
init_per_group2(test_v3 = GroupName, Config) -> 
    case snmp_test_lib:crypto_start() of
        ok ->
            init_v3(snmp_test_lib:init_group_top_dir(GroupName, Config));
        _ ->
            {skip, "Crypto did not start"}
    end;
init_per_group2(test_v1_v2 = GroupName, Config) -> 
    init_v1_v2(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(test_v2 = GroupName, Config) -> 
    init_v2(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(test_v1 = GroupName, Config) -> 
    init_v1(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(test_v1_ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName, Config, fun init_v1/1);
init_per_group2(test_v2_ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName, Config, fun init_v2/1);
init_per_group2(test_v1_v2_ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName, Config, fun init_v1_v2/1);
init_per_group2(test_v3_ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName, Config, fun init_v3/1);
init_per_group2(misc = GroupName, Config) -> 
    init_misc(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(mib_storage_varm_mnesia = GroupName, Config) -> 
    init_varm_mib_storage_mnesia(snmp_test_lib:init_group_top_dir(GroupName, 
								  Config));
init_per_group2(mib_storage_varm_dets = GroupName, Config) -> 
    init_varm_mib_storage_dets(
      snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(mib_storage_size_check_mnesia = GroupName, Config) -> 
       case snmp_test_lib:crypto_start() of
           ok ->
               init_size_check_msm(snmp_test_lib:init_group_top_dir(GroupName, Config));
           _ ->
               {skip, "Crypto did not start"}
       end;
init_per_group2(mib_storage_size_check_dets = GroupName, Config) -> 
       case snmp_test_lib:crypto_start() of
           ok ->
               init_size_check_msm(snmp_test_lib:init_group_top_dir(GroupName, Config));
           _ ->
               {skip, "Crypto did not start"}
       end;
init_per_group2(mib_storage_size_check_ets = GroupName, Config) -> 
    case snmp_test_lib:crypto_start() of
        ok ->
            init_size_check_msm(snmp_test_lib:init_group_top_dir(GroupName, Config));
        _ ->
            {skip, "Crypto did not start"}
    end;
init_per_group2(mib_storage_mnesia = GroupName, Config) -> 
    init_mib_storage_mnesia(snmp_test_lib:init_group_top_dir(GroupName, 
							     Config));
init_per_group2(mib_storage_dets = GroupName, Config) -> 
    init_mib_storage_dets(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(mib_storage_ets = GroupName, Config) -> 
    init_mib_storage_ets(snmp_test_lib:init_group_top_dir(GroupName, Config));
init_per_group2(otp16649_ipv4 = GroupName, Config) -> 
    Config2 = [{ip,       ?LOCALHOST(inet)},
               {ipfamily, inet},
               {tdomain,  transportDomainUdpIpv4} |
               lists:keydelete(ip, 1, Config)],
    snmp_test_lib:init_group_top_dir(GroupName, Config2);
init_per_group2(otp16649_ipv6 = GroupName, Config) ->
    init_per_group_ipv6(GroupName,
                        [{tdomain,  transportDomainUdpIpv6} | Config],
                        fun(C) -> C end);
init_per_group2(GroupName, Config) ->
    snmp_test_lib:init_group_top_dir(GroupName, Config).

init_per_group_ipv6(GroupName, Config, Init) ->
    %% <OS-CONDITIONAL-SKIP>
    %% This is a highly questionable test.
    %% But until we have time to figure out what IPv6 issues
    %% are actually causing the failures...
    OSSkipable = [{unix, 
                   [
		    %% We only have the one NetBSD machine,
		    %% and UDP on IPv6 is very glitchy, so ...
		    {netbsd, fun(_) -> true end},
                    {darwin, fun(V) when (V > {9, 8, 0}) ->
				     %% This version is OK: No Skip
				     false;
				(_) ->
				     %% This version is *not* ok: Skip
				     true
                             end}
                   ]
                  }],
    %% </OS-CONDITIONAL-SKIP>
    case ?OS_BASED_SKIP(OSSkipable) of
        true ->
            {skip, "Host *may* not *properly* support IPV6"};
        false ->
            %% Even if this host supports IPv6 we don't use it unless its
            %% one of the configured/supported IPv6 hosts...
            case ?HAS_SUPPORT_IPV6() of
                true ->
                    Init(
                      snmp_test_lib:init_group_top_dir(
                        GroupName,
                        [{ipfamily, inet6},
                         {ip, ?LOCALHOST(inet6)}
                         | lists:keydelete(ip, 1, Config)]));
                false ->
                    {skip, "Host does not support IPv6"}
            end
    end.


end_per_group(GroupName, Config) ->
    ?IPRINT("end_per_group -> entry with"
            "~n      GroupName: ~p"
            "~n      Config:    ~p"
            "~n   when"
            "~n      Nodes:     ~p",
            [GroupName, Config, nodes()]),

    Config1 = end_per_group2(GroupName, Config),

    ?IPRINT("end_per_group -> done when"
            "~n      Nodes:     ~p",
            [nodes()]),

    Config1.

end_per_group2(major_tcs, Config) ->
    finish_all(Config);
end_per_group2(otp7157, Config) -> 
    otp_7157_finish(Config);
end_per_group2(otp4394, Config) -> 
    otp_4394_finish(Config);
end_per_group2(v2_inform, Config) -> 
    finish_v2_inform(Config);
end_per_group2(v3_inform, Config) -> 
    finish_v3_inform(Config);
end_per_group2(multiple_reqs, Config) -> 
    finish_mul(Config);
end_per_group2(multiple_reqs_2, Config) -> 
    finish_mul(Config);
end_per_group2(multiple_reqs_3, Config) -> 
    finish_mul(Config);
end_per_group2(test_multi_threaded, Config) -> 
    finish_mt(Config);
end_per_group2(test_multi_threaded_ext, Config) -> 
    finish_mt(Config);
end_per_group2(test_v3_ipv6, Config) ->
    finish_v3(Config);
end_per_group2(test_v1_v2_ipv6, Config) ->
    finish_v1_v2(Config);
end_per_group2(test_v2_ipv6, Config) ->
    finish_v2(Config);
end_per_group2(test_v1_ipv6, Config) ->
    finish_v1(Config);
end_per_group2(test_v3, Config) ->
    finish_v3(Config);
end_per_group2(test_v1_v2, Config) ->
    finish_v1_v2(Config);
end_per_group2(test_v2, Config) ->
    finish_v2(Config);
end_per_group2(test_v1, Config) ->
    finish_v1(Config);
end_per_group2(misc, Config) ->
    finish_misc(Config);
end_per_group2(mib_storage_varm_mnesia, Config) ->
    finish_varm_mib_storage_mnesia(Config);
end_per_group2(mib_storage_varm_dets, Config) ->
    finish_varm_mib_storage_dets(Config);
end_per_group2(mib_storage_size_check_mnesia, Config) ->
    finish_size_check_msm(Config);
end_per_group2(mib_storage_size_check_dets, Config) ->
    finish_size_check_msd(Config);
end_per_group2(mib_storage_size_check_ets, Config) ->
    finish_size_check_mse(Config);
end_per_group2(mib_storage_mnesia, Config) ->
    finish_mib_storage_mnesia(Config);
end_per_group2(mib_storage_dets, Config) ->
    finish_mib_storage_dets(Config);
end_per_group2(mib_storage_ets, Config) ->
    finish_mib_storage_ets(Config);
end_per_group2(_GroupName, Config) ->
    Config.



%%
%% ----- Init Per TestCase -----
%%

%% T is in number of minutes
wd_start(T, Config) ->
    Factor = case ?config(snmp_factor, Config) of
                 F when (F > 0) ->
                     F-1;
                 _ ->
                     0
             end,
    Dog = ?WD_START(?MINS(T + Factor)),
    [{watchdog, Dog} | Config ].
    
    
wd_stop(Config) ->
    Dog = ?config(watchdog, Config),
    ?WD_STOP(Dog),
    lists:keydelete(Dog, 2, Config).
    
init_per_testcase(Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase -> entry with"
            "~n   Config: ~p"
            "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    Result = init_per_testcase1(Case, Config),

    snmp_test_global_sys_monitor:reset_events(),

    ?IPRINT("init_per_testcase -> done when"
            "~n      Result: ~p"
            "~n      Nodes:  ~p", [Result, erlang:nodes()]),
    Result.

init_per_testcase1(otp8395 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp8395({init, init_per_testcase2(Case, Config)});
init_per_testcase1(otp9884 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp9884({init, init_per_testcase2(Case, Config)});
init_per_testcase1(otp16649_1 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_1_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_2 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_2_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_3 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_3_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_4 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_4_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_5 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_5_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_6 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_6_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp16649_7 = Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    otp16649_7_init(init_per_testcase2(Case, Config));
init_per_testcase1(otp_7157 = _Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [_Case, Config]),
    wd_start(1, Config);
init_per_testcase1(Case, Config) 
  when ((Case =:= otp_16092_simple_start_and_stop1)  orelse
        (Case =:= otp_16092_simple_start_and_stop2)  orelse
        (Case =:= otp_16092_simple_start_and_stop3)  orelse
        (Case =:= otp_16092_simple_start_and_stop4)) andalso
       is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [_Case, Config]),
    init_per_testcase2(Case, wd_start(1, Config));
init_per_testcase1(v2_inform_i = _Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [_Case, Config]),
    wd_start(10, Config);
init_per_testcase1(v3_inform_i = _Case, Config) when is_list(Config) ->
    ?DBG("init_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [_Case, Config]),
    wd_start(10, Config);
init_per_testcase1(v3_des_priv = _Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase1 -> entry with"
            "~n   Case:   ~p"
            "~n   Config: ~p", [_Case, Config]),
    case ?config(label, Config) of
        docker ->
            ?IPRINT("Running in docker => SKIP"),
            {skip, "Behaves badly when run in a docker"};
        _ ->
            %% <OS-CONDITIONAL-SKIP>
            %% This is intended to catch Yellow Dog Linux release 6.2 (2.6.29)
            LinuxVersionVerify = 
                fun() ->
                        case string:to_lower(os:cmd("uname -m")) of
                            "ppc" ++ _ ->
                                case file:read_file_info("/etc/issue") of
                                    {ok, _} ->
                                        case string:to_lower(
                                               os:cmd("grep -i yellow /etc/issue")) of
                                            "yellow dog " ++ _ ->
                                                case os:version() of
                                                    {2, 6, 29} ->
                                                        ?IPRINT("(PPC Linux) "
                                                                "kernel version check: "
                                                                "{2, 6, 29} => SKIP"),
                                                        true;
                                                    V ->
                                                        ?IPRINT("(PPC Linux) "
                                                                "kernel version check: "
                                                                "~p != {2, 6, 29} => "
                                                                "*NO* SKIP", [V]),
                                                        false
                                                end;
                                            _ -> % Maybe plain Debian or Ubuntu
                                                ?IPRINT("(PPC Linux) => *NO* SKIP"),
                                                false
                                        end;
                                    _ ->
                                        ?IPRINT("(PPC Linux) Unknown distro => "
                                                "*NO* SKIP"),
                                        false
                                end;
                            _ ->
                                ?IPRINT("(Linux) Not PPC => *NO* SKIP"),
                                false
                        end
                end,
            OSSkippable = [{unix, [{linux, LinuxVersionVerify}]}],
            %% </OS-CONDITIONAL-SKIP>
            case ?OS_BASED_SKIP(OSSkippable) of
                true ->
                    {skip, "Host *may* not *properly* handle this test case"};
                false ->
                    wd_start(6, Config)
            end
    end;
init_per_testcase1(_Case, Config) when is_list(Config) ->
    ?IPRINT("init_per_testcase -> entry with"
            "~n   Case:   ~p"
            "~n   Config: ~p", [_Case, Config]),
    wd_start(6, Config).

init_per_testcase2(Case, Config) ->

    ?DBG("init_per_testcase2 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),

    CaseTopDir = snmp_test_lib:init_testcase_top_dir(Case, Config), 

    %% Create agent top-dir(s)
    AgentTopDir = join([CaseTopDir, agent]),
    ok = file:make_dir(AgentTopDir),
    AgentConfDir = join([AgentTopDir, config]),
    ok = file:make_dir(AgentConfDir),
    AgentDbDir = join([AgentTopDir, db]),
    ok = file:make_dir(AgentDbDir),
    AgentLogDir = join([AgentTopDir, log]),
    ok = file:make_dir(AgentLogDir),

    %% Create sub-agent top-dir(s)
    SubAgentTopDir = join([CaseTopDir, sub_agent]),
    ok = file:make_dir(SubAgentTopDir),

    %% Create manager top-dir(s)
    ManagerTopDir = join([CaseTopDir, manager]),
    ok = file:make_dir(ManagerTopDir),

    [{case_top_dir,      CaseTopDir}, 
     {agent_top_dir,     AgentTopDir}, 
     {agent_conf_dir,    AgentConfDir}, 
     {agent_db_dir,      AgentDbDir}, 
     {agent_log_dir,     AgentLogDir}, 
     {sub_agent_top_dir, SubAgentTopDir}, 
     {manager_top_dir,   ManagerTopDir} | Config].


%% ---- End Per TestCase ---- 

end_per_testcase(Case, Config) when is_list(Config) ->

    ?IPRINT("end_per_testcase -> entry with"
            "~n   Config:        ~p"
            "~n   Nodes:         ~p",
            [Config, erlang:nodes()]),

    ?IPRINT("system events during test: "
            "~n   ~p", [snmp_test_global_sys_monitor:events()]),

    %% On some hosts, this operation can take a long time.
    %% So long, that the timetrap expires and the test case
    %% will be "failed".
    %% So, wrap it in a process and for a successful test case,
    %% give it 30 seconds, then kill it. If the test case has
    %% already failed, we will want to get as much of the logs
    %% as possible. So, set no timeout (infinity) and let the
    %% test framework take care of things...
    %% But also, *don't* bother with this unless the test case
    %% has failed!
    case ?config(tc_status, Config) of
        ok ->
            ok;
        _ ->
            To   = ?SECS(30),
            Flag = process_flag(trap_exit, true),
            Pid  = spawn_link(fun() -> display_log(Config), exit(normal) end),
            receive
                {'EXIT', Pid, _} ->
                    process_flag(trap_exit, Flag),
                    ok
            after To ->
                    ?WPRINT("Display Log process fail to complete in time"
                            "(~w msec): kill it", [To]),
                    process_flag(trap_exit, Flag),
                    exit(Pid, kill)
            end
        end,

    Result = end_per_testcase1(Case, Config),

    ?IPRINT("end_per_testcase -> done with"
            "~n   Result: ~p"
            "~n   Nodes:  ~p", [Result, erlang:nodes()]),
    Result.

end_per_testcase1(otp8395, Config) when is_list(Config) ->
    otp8395({fin, Config});
end_per_testcase1(otp9884, Config) when is_list(Config) ->
    otp9884({fin, Config});
end_per_testcase1(otp16649_1, Config) when is_list(Config) ->
    otp16649_1_fin(Config);
end_per_testcase1(otp16649_2, Config) when is_list(Config) ->
    otp16649_2_fin(Config);
end_per_testcase1(otp16649_3, Config) when is_list(Config) ->
    otp16649_3_fin(Config);
end_per_testcase1(otp16649_4, Config) when is_list(Config) ->
    otp16649_4_fin(Config);
end_per_testcase1(otp16649_5, Config) when is_list(Config) ->
    otp16649_5_fin(Config);
end_per_testcase1(otp16649_6, Config) when is_list(Config) ->
    otp16649_6_fin(Config);
end_per_testcase1(otp16649_7, Config) when is_list(Config) ->
    otp16649_7_fin(Config);
end_per_testcase1(_Case, Config) when is_list(Config) ->
    ?DBG("end_per_testcase1 -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [_Case, Config]),
    wd_stop(Config).





%%%-----------------------------------------------------------------
%%% The test case structure is as follows:
%%%
%%% init_all - starts mnesia, 
%%%      
%%%    init_v1 - starts agent
%%%       simple
%%%       big  - e.g. starts/stops subagent, load/unloads mibs
%%%       init_mul
%%%          mul_get
%%%          mul_set
%%%          <etc>
%%%       finish_mul
%%%       <etc>
%%%    finish_v1
%%%
%%%    init_v2 - starts agent
%%%    finish_v2
%%%      
%%%    init_bilingual - starts agent
%%%    finish_bilingual
%%%      
%%% finish_all
%%%
%%% There is still one problem with these testsuites.  If one test
%%% fails, it may not be possible to run some other cases, as it
%%% may have e.g. created some row or loaded some table, that it
%%% didn't undo (since it failed).
%%%-----------------------------------------------------------------

init_all(Conf) ->
    ?DISPLAY_SUITE_INFO(), 
    ?ALIB:init_all(Conf).

finish_all(Conf) ->
    ?ALIB:finish_all(Conf).

start_v1_agent(Config) ->
    ?ALIB:start_v1_agent(Config).

start_v1_agent(Config, Opts) ->
    ?ALIB:start_v1_agent(Config, Opts).

start_v2_agent(Config) ->
    ?ALIB:start_v2_agent(Config).

start_v2_agent(Config, Opts) ->
    ?ALIB:start_v2_agent(Config, Opts).

%% start_v3_agent(Config) ->
%%     ?ALIB:start_v3_agent(Config).

start_v3_agent(Config, Opts) ->
    ?ALIB:start_v3_agent(Config, Opts).

start_bilingual_agent(Config) ->
    ?ALIB:start_bilingual_agent(Config).

start_multi_threaded_agent(Config, MT) when is_list(Config) ->
    [{multi_threaded, MT} | ?ALIB:start_mt_agent(Config, MT)].

stop_agent(Config) ->
    ?ALIB:stop_agent(Config).


create_tables(SaNode) ->
    %% {atomic, ok} = mnesia:create_table([{name, friendsTable2},
    %%     				      {ram_copies, [SaNode]},
    %%     				      {snmp, [{key, integer}]},
    %%     				      {attributes, [a1,a2,a3]}]),
    mnesia_create_table_or_fail([{name, friendsTable2},
                                 {ram_copies, [SaNode]},
                                 {snmp, [{key, integer}]},
                                 {attributes, [a1,a2,a3]}]),
    %% {atomic, ok} = mnesia:create_table([{name, kompissTable2},
    %%     				      {ram_copies, [SaNode]},
    %%     				      {snmp, [{key, integer}]},
    %%     				      {attributes, [a1,a2,a3]}]),
    mnesia_create_table_or_fail([{name, kompissTable2},
                                 {ram_copies, [SaNode]},
                                 {snmp, [{key, integer}]},
                                 {attributes, [a1,a2,a3]}]),
    %% {atomic, ok} = mnesia:create_table([{name, snmp_variables},
    %%     				      {attributes, [a1,a2]}]),
    mnesia_create_table_or_fail([{name, snmp_variables},
                                 {attributes, [a1,a2]}]),
    ok.

mnesia_create_table_or_fail(Args) ->
    ?IPRINT("mnesia_create_table_or_fail -> "
            "try create table ~p", [proplists:get_value(name, Args)]),
    case mnesia:create_table(Args) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, Table}}  ->
            ?EPRINT("mnesia_create_table_or_fail -> "
                    "table ~p already exists", [Table]),
            %% ?SKIP({table_already_exist, Table});
            ?FAIL({table_already_exist, Table});
        {aborted, Error}  ->
            ?EPRINT("mnesia_create_table_or_fail -> "
                    "failed creating table: "
                    "~n      ~p", [Error]),
            ?FAIL({failed_create_table, Error})
    end.


delete_tables() ->
    %% mnesia:delete_table(friendsTable2),
    mnesia_delete_table(friendsTable2),
    %% mnesia:delete_table(kompissTable2),
    mnesia_delete_table(kompissTable2),
    %% mnesia:delete_table(snmp_variables).
    mnesia_delete_table(snmp_variables).

mnesia_delete_table(Tab) ->
    try mnesia:delete_table(Tab) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            ?EPRINT("Table delete aborted:"
                    "~n      Table:  ~w: "
                    "~n      Reason: ~p", [Tab, Reason]),
            {error, Reason}
    catch
        C:E:S ->
            ?EPRINT("Failed delete table ~w: "
                    "~n      Class: ~p"
                    "~n      Error: ~p"
                    "~n      Stack: ~p", [Tab, C, E, S]),
            {error, {C, E, S}}
    end.

%% Tables are created in runtime!
delete_mib_storage_mnesia_tables() ->
    mnesia:delete_table(snmpa_mib_data),
    mnesia:delete_table(snmpa_mib_tree),
    mnesia:delete_table(snmpa_symbolic_store).


%%-----------------------------------------------------------------
%% A test case is always one of:
%%   - v1 specific case
%%   - v2 specific case
%%   - v1 and v2 case
%% All v1 specific cases are prefixed with v1_, and all v2 with
%% v2_.  E.g. v1_trap/v2_trap.
%%
%% All other cases are shared. However, the testserver uses the name
%% of the case to generate a file for that case.  The same case cannot
%% be used in different configurations in the same suite.  Therefore
%% all these functions exists in two variants, the base function
%% <base>, and a second version <base>_2.  There may be several
%% versions as well, <base>_N.
%%-----------------------------------------------------------------

mib_storage_cases() ->
    [
     {group, mib_storage_ets}, 
     {group, mib_storage_dets},
     {group, mib_storage_mnesia},
     {group, mib_storage_size_check_ets},
     {group, mib_storage_size_check_dets},
     {group, mib_storage_size_check_mnesia},
     {group, mib_storage_varm_dets},
     {group, mib_storage_varm_mnesia}
    ].
    
mib_storage_ets_cases() -> 
    [
     mse_simple, 
     mse_v1_processing, 
     mse_big, 
     mse_big2,
     mse_loop_mib, 
     mse_api, 
     mse_sa_register, 
     mse_v1_trap,
     mse_sa_error, 
     mse_next_across_sa, 
     mse_undo,
     mse_standard_mib, 
     mse_community_mib, 
     mse_framework_mib,
     mse_target_mib, 
     mse_notification_mib,
     mse_view_based_acm_mib, 
     mse_sparse_table, 
     mse_me_of,
     mse_mib_of
    ].

mib_storage_dets_cases() -> 
    [
     msd_simple, 
     msd_v1_processing, 
     msd_big, 
     msd_big2,
     msd_loop_mib, 
     msd_api, 
     msd_sa_register, 
     msd_v1_trap,
     msd_sa_error, 
     msd_next_across_sa, 
     msd_undo,
     msd_standard_mib, 
     msd_community_mib, 
     msd_framework_mib,
     msd_target_mib, 
     msd_notification_mib,
     msd_view_based_acm_mib, 
     msd_sparse_table, 
     msd_me_of,
     msd_mib_of
    ].

mib_storage_mnesia_cases() -> 
    [
     msm_simple, 
     msm_v1_processing, 
     msm_big, 
     msm_big2,
     msm_loop_mib, 
     msm_api, 
     msm_sa_register, 
     msm_v1_trap,
     msm_sa_error, 
     msm_next_across_sa, 
     msm_undo,
     msm_standard_mib, 
     msm_community_mib, 
     msm_framework_mib,
     msm_target_mib, 
     msm_notification_mib,
     msm_view_based_acm_mib, 
     msm_sparse_table, 
     msm_me_of,
     msm_mib_of
    ].

mse_size_check_cases() -> 
    [mse_size_check].

msd_size_check_cases() -> 
    [msd_size_check].

msm_size_check_cases() -> 
    [msm_size_check].

varm_mib_storage_dets_cases() -> 
    [msd_varm_mib_start].

varm_mib_storage_mnesia_cases() -> 
    [msm_varm_mib_start].

init_mib_storage_ets(Config) when is_list(Config) ->
    ?IPRINT("init_mib_storage_ets -> entry"),
    MibStorage = {mib_storage, [{module, snmpa_mib_storage_ets}]},
    init_ms(Config, [MibStorage]).

init_mib_storage_dets(Config) when is_list(Config) ->
    ?IPRINT("init_mib_storage_dets -> entry"),
    AgentDbDir = ?GCONF(agent_db_dir, Config),
    MibStorage = {mib_storage, [{module,  snmpa_mib_storage_dets}, 
				{options, [{dir, AgentDbDir}]}]},
    init_ms(Config, [MibStorage]).

init_mib_storage_mnesia(Config) when is_list(Config) ->
    ?IPRINT("init_mib_storage_mnesia -> entry"),
    AgentNode = ?GCONF(snmp_master, Config),
    MibStorage = {mib_storage, [{module, snmpa_mib_storage_mnesia}, 
				{options, [{nodes, [AgentNode]}]}]},
    init_ms(Config, [MibStorage]).

init_ms(Config, Opts) when is_list(Config) ->
    ?IPRINT("init_ms -> entry with"
            "~n   Config: ~p"
            "~n   Opts:   ~p", [Config, Opts]),
    SaNode       = ?GCONF(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?GCONF(agent_conf_dir, Config),
    MgrDir       = ?GCONF(mgr_dir, Config),
    Ip           = ?GCONF(ip, Config),
    config([v1], MgrDir, AgentConfDir,
		 tuple_to_list(Ip), tuple_to_list(Ip)),
    MasterAgentVerbosity = {agent_verbosity, trace},
    MibsVerbosity        = {mib_server,      [{verbosity, trace}]},
    SymStoreVerbosity    = {symbolic_store,  [{verbosity, trace}]},
    Opts1 = [MasterAgentVerbosity, MibsVerbosity, SymStoreVerbosity | Opts],
    [{vsn, v1} | start_v1_agent(Config, Opts1)].

%% init_size_check_mse(Config) when is_list(Config) ->
%%     MibStorage = {mib_storage, [{module, snmpa_mib_storage_ets}]},
%%     init_size_check_ms(Config, [MibStorage]).

%% init_size_check_msd(Config) when is_list(Config) ->
%%     AgentDbDir = ?GCONF(agent_db_dir, Config),
%%     MibStorage = {mib_storage, [{module,  snmpa_mib_storage_dets}, 
%% 				{options, [{dir, AgentDbDir}]}]},
%%     init_size_check_ms(Config, [MibStorage]).

init_size_check_msm(Config) when is_list(Config) ->
    AgentNode = ?GCONF(snmp_master, Config),
    MibStorage = {mib_storage, [{module, snmpa_mib_storage_mnesia}, 
				{options, [{nodes, [AgentNode]}]}]},
    init_size_check_ms(Config, [MibStorage]).

init_size_check_ms(Config, Opts) when is_list(Config) ->
    SaNode = ?GCONF(snmp_sa, Config),
    %% We are using v3 here, so crypto must be supported or else...
    case ?CRYPTO_START() of
	ok ->
	    case ?CRYPTO_SUPPORT() of
		{no, Reason} ->
                    ?WPRINT("crypto support not sufficient:"
                            "~n      ~p", [Reason]),
		    ?SKIP({unsupported_encryption, Reason});
		yes ->
                    ?IPRINT("crypto started"),
		    ok
	    end;
	{error, Reason} ->
            ?IPRINT("crypto not started:"
                    "~n      ~p", [Reason]),
	    ?SKIP({failed_starting_crypto, Reason})
    end,
    create_tables(SaNode),
    AgentConfDir = ?GCONF(agent_conf_dir, Config),
    MgrDir       = ?GCONF(mgr_dir, Config),
    Ip           = ?GCONF(ip, Config),
    ok = config([v3], MgrDir, AgentConfDir,
		      tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v3} | start_v3_agent(Config, Opts)].

init_varm_mib_storage_dets(Config) when is_list(Config) ->
    ?IPRINT("init_varm_mib_storage_dets -> entry"),
    SaNode       = ?GCONF(snmp_sa, Config),
    create_tables(SaNode),
    AgentDbDir   = ?GCONF(agent_db_dir, Config),
    AgentConfDir = ?GCONF(agent_conf_dir, Config),
    MgrDir       = ?GCONF(mgr_dir, Config),
    Ip           = ?GCONF(ip, Config),
    config([v1], MgrDir, AgentConfDir,
		 tuple_to_list(Ip), tuple_to_list(Ip)),
    MibStorage = {mib_storage, [{module,  snmpa_mib_storage_dets}, 
				{options, [{dir, AgentDbDir}]}]},
    MasterAgentVerbosity = {agent_verbosity, trace},
    MibsVerbosity        = {mib_server,      [{verbosity, trace}]},
    SymStoreVerbosity    = {symbolic_store,  [{verbosity, trace}]},
    Opts = [MibStorage, 
	    MasterAgentVerbosity, 
	    MibsVerbosity, 
	    SymStoreVerbosity],
    [{vsn, v1}, {agent_opts, Opts} | Config].

init_varm_mib_storage_mnesia(Config) when is_list(Config) ->
    ?IPRINT("init_varm_mib_storage_mnesia -> entry"),
    SaNode       = ?GCONF(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?GCONF(agent_conf_dir, Config),
    MgrDir       = ?GCONF(mgr_dir, Config),
    Ip           = ?GCONF(ip, Config),
    config([v1], MgrDir, AgentConfDir,
		 tuple_to_list(Ip), tuple_to_list(Ip)),
    AgentNode = ?GCONF(snmp_master, Config),
    MibStorage = {mib_storage, [{module, snmpa_mib_storage_mnesia}, 
				{options, [{nodes, [AgentNode]}]}]},
    MasterAgentVerbosity = {agent_verbosity, trace},
    MibsVerbosity        = {mib_server,      [{verbosity, trace}]},
    SymStoreVerbosity    = {symbolic_store,  [{verbosity, trace}]},
    Opts = [MibStorage,
	    MasterAgentVerbosity,
	    MibsVerbosity,
	    SymStoreVerbosity],
    [{vsn, v1}, {agent_opts, Opts} | Config].

finish_mib_storage_ets(Config) when is_list(Config) ->
    ?IPRINT("finish_mib_storage_ets -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_mib_storage_ets -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_mib_storage_ets -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_mib_storage_ets -> done"),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_mib_storage_dets(Config) when is_list(Config) ->
    ?IPRINT("finish_mib_storage_dets -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_mib_storage_dets -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_mib_storage_dets -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_mib_storage_dets -> done"),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_mib_storage_mnesia(Config) when is_list(Config) ->
    ?IPRINT("finish_mib_storage_mnesia -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_mib_storage_mnesia -> "
            "try delete (mib storage) mnesia tables"),
    delete_mib_storage_mnesia_tables(),
    ?IPRINT("finish_mib_storage_mnesia -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_mib_storage_mnesia -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_mib_storage_mnesia -> done"),
    C2 = lists:keydelete(vsn, 1, C1),
    lists:keydelete(agent_opts, 1, C2).

finish_varm_mib_storage_dets(Config) when is_list(Config) ->
    ?IPRINT("finish_varm_mib_storage_dets -> try delete mnesia tables"),
    delete_tables(),
    %% C1 = stop_agent(Config), % In case something went wrong...
    ?IPRINT("finish_varm_mib_storage_dets -> try delete files"),
    delete_files(Config),
    ?IPRINT("finish_varm_mib_storage_dets -> done"),
    C2 = lists:keydelete(vsn, 1, Config),
    lists:keydelete(agent_opts, 1, C2).

finish_varm_mib_storage_mnesia(Config) when is_list(Config) ->
    ?IPRINT("finish_varm_mib_storage_mnesia -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_varm_mib_storage_mnesia -> "
            "try delete (mib storage) mnesia tables"),
    delete_mib_storage_mnesia_tables(),
    %% C1 = stop_agent(Config), % In case something went wrong...
    ?IPRINT("finish_varm_mib_storage_mnesia -> try delete files"),
    delete_files(Config),
    ?IPRINT("finish_varm_mib_storage_mnesia -> done"),
    C2 = lists:keydelete(vsn, 1, Config),
    lists:keydelete(agent_opts, 1, C2).

finish_size_check_mse(Config) when is_list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_msd(Config) when is_list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_msm(Config) when is_list(Config) ->
    finish_size_check_ms(Config).

finish_size_check_ms(Config) when is_list(Config) ->
    ?IPRINT("finish_size_check_ms -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_size_check_ms -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_size_check_ms -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_size_check_ms -> done"),
    lists:keydelete(vsn, 1, C1).


%% These are just interface functions to fool the test server
mse_simple(X)         -> ?P(mse_simple), simple(X).
mse_v1_processing(X)  -> ?P(mse_v1_processing), v1_processing(X).
mse_big(X)            -> ?P(mse_big), big(X).
mse_big2(X)           -> ?P(mse_big2), big2(X).
mse_loop_mib(X)       -> ?P(mse_loop_mib), loop_mib_1(X).
mse_api(X)            -> ?P(mse_api), api(X).
mse_sa_register(X)    -> ?P(mse_sa_register), sa_register(X).
mse_v1_trap(X)        -> ?P(mse_v1_trap), v1_trap(X).
mse_sa_error(X)       -> ?P(mse_sa_error), sa_error(X).
mse_next_across_sa(X) -> ?P(mse_next_across_sa), next_across_sa(X).
mse_undo(X)           -> ?P(mse_undo), undo(X).
mse_standard_mib(X)   -> ?P(mse_standard_mib), snmp_standard_mib(X).
mse_community_mib(X)  -> ?P(mse_community_mib), snmp_community_mib(X).
mse_framework_mib(X)  -> ?P(mse_framework_mib), snmp_framework_mib(X).
mse_target_mib(X)       -> ?P(mse_target_mib), snmp_target_mib(X).
mse_notification_mib(X) -> ?P(mse_notification_mib), snmp_notification_mib(X).
mse_view_based_acm_mib(X) -> ?P(mse_view_based_acm_mib), snmp_view_based_acm_mib(X).
mse_sparse_table(X)   -> ?P(mse_sparse_table), sparse_table(X).
mse_me_of(X)          -> ?P(mse_me_of), ms_me_of(X).
mse_mib_of(X)         -> ?P(mse_mib_of), ms_mib_of(X).

msd_simple(X)         -> ?P(msd_simple), simple(X).
msd_v1_processing(X)  -> ?P(msd_v1_processing), v1_processing(X).
msd_big(X)            -> ?P(msd_big), big(X).
msd_big2(X)           -> ?P(msd_big2), big2(X).
msd_loop_mib(X)       -> ?P(msd_loop_mib), loop_mib_1(X).
msd_api(X)            -> ?P(msd_api), api(X).
msd_sa_register(X)    -> ?P(msd_sa_register), sa_register(X).
msd_v1_trap(X)        -> ?P(msd_v1_trap), v1_trap(X).
msd_sa_error(X)       -> ?P(msd_sa_error), sa_error(X).
msd_next_across_sa(X) -> ?P(msd_next_across_sa), next_across_sa(X).
msd_undo(X)           -> ?P(msd_undo), undo(X).
msd_standard_mib(X)   -> ?P(msd_standard_mib), snmp_standard_mib(X).
msd_community_mib(X)  -> ?P(msd_community_mib), snmp_community_mib(X).
msd_framework_mib(X)  -> ?P(msd_framework_mib), snmp_framework_mib(X).
msd_target_mib(X)       -> ?P(msd_target_mib), snmp_target_mib(X).
msd_notification_mib(X) -> ?P(msd_notification_mib), snmp_notification_mib(X).
msd_view_based_acm_mib(X) -> ?P(msd_view_based_acm_mib), snmp_view_based_acm_mib(X).
msd_sparse_table(X)   -> ?P(msd_sparse_table), sparse_table(X).
msd_me_of(X)          -> ?P(msd_me_of), ms_me_of(X).
msd_mib_of(X)         -> ?P(msd_mib_of), ms_mib_of(X).

msm_simple(X)         -> ?P(msm_simple), simple(X).
msm_v1_processing(X)  -> ?P(msm_v1_processing), v1_processing(X).
msm_big(X)            -> ?P(msm_big2), big(X).
msm_big2(X)           -> ?P(msm_loop_mib), big2(X).
msm_loop_mib(X)       -> ?P(msm_loop_mib), loop_mib_1(X).
msm_api(X)            -> ?P(msm_api), api(X).
msm_sa_register(X)    -> ?P(msm_sa_register), sa_register(X).
msm_v1_trap(X)        -> ?P(msm_v1_trap), v1_trap(X).
msm_sa_error(X)       -> ?P(msm_sa_error), sa_error(X).
msm_next_across_sa(X) -> ?P(msm_next_across_sa), next_across_sa(X).
msm_undo(X)           -> ?P(msm_undo), undo(X).
msm_standard_mib(X)   -> ?P(msm_standard_mib), snmp_standard_mib(X).
msm_community_mib(X)  -> ?P(msm_community_mib), snmp_community_mib(X).
msm_framework_mib(X)  -> ?P(msm_framework_mib), snmp_framework_mib(X).
msm_target_mib(X)       -> ?P(msm_target_mib), snmp_target_mib(X).
msm_notification_mib(X) -> ?P(msm_notification_mib), snmp_notification_mib(X).
msm_view_based_acm_mib(X) -> ?P(msm_view_based_acm_mib), snmp_view_based_acm_mib(X).
msm_sparse_table(X)   -> ?P(msm_sparse_table), sparse_table(X).
msm_me_of(X)          -> ?P(msm_me_of), ms_me_of(X).
msm_mib_of(X)         -> ?P(msm_mib_of), ms_mib_of(X).


mse_size_check(X)     -> ?P(mse_size_check), ms_size_check(X).
msd_size_check(X)     -> ?P(msd_size_check), ms_size_check(X).
msm_size_check(X)     -> ?P(msm_size_check), ms_size_check(X).

msd_varm_mib_start(X) -> 
    ?P(msd_varm_mib_start), 
    varm_mib_start(X).

msm_varm_mib_start(X) -> 
    %% <CONDITIONAL-SKIP>
    %% This is a bit radioactive but...
    Skippable = [win32],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(msm_varm_mib_start), 
    varm_mib_start(X).

ms_size_check(Config) when is_list(Config) ->
    ?P(ms_size_check),
    init_case(Config),
    ?IPRINT("mib server size check..."),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),
    load_master_std("OTP-SNMPEA-MIB"),
    load_master_std("SNMP-COMMUNITY-MIB"),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    load_master_std("SNMP-MPD-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-USER-BASED-SM-MIB"),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    load_master_std("SNMPv2-MIB"),
    load_master_std("SNMPv2-TM"),

    ?SLEEP(2000),

    display_memory_usage(),

    unload_master("OTP-SNMPEA-MIB"),
    unload_master("SNMP-COMMUNITY-MIB"),
    unload_master("SNMP-FRAMEWORK-MIB"),
    unload_master("SNMP-MPD-MIB"),
    unload_master("SNMP-NOTIFICATION-MIB"),
    unload_master("SNMP-TARGET-MIB"),
    unload_master("SNMP-USER-BASED-SM-MIB"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    unload_master("SNMPv2-MIB"),
    unload_master("SNMPv2-TM"),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),

    ok.


varm_mib_start(Config) when is_list(Config) ->
    ?P(varm_mib_start),
    ?IPRINT("varm_mib_start -> entry"),
    init_case(Config),

    %% Start the agent
    Opts    = ?GCONF(agent_opts, Config),
    Config1 = start_v1_agent(Config, Opts),

    %% Sleep some in order for the agent to start properly
    ?DBG("varm_mib_start -> sleep some (before loading mobs)", []),
    ?SLEEP(5000),

    %% Load all the mibs
    HardwiredMibs = loaded_mibs(),
    ?DBG("varm_mib_start -> load all mibs", []),
    load_master_std("SNMP-COMMUNITY-MIB"),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),

    %% Unload the hardwired mibs
    ?DBG("varm_mib_start -> sleep some (before unloading hardwired mibs)", []),
    ?SLEEP(1000),
    ?DBG("varm_mib_start -> unload (hardwired) mibs", []),
    unload_mibs(HardwiredMibs),    %% unload hardwired

    ?DBG("varm_mib_start -> sleep some (before stopping agent)", []),
    ?SLEEP(1000),

    %% Stop the agent (without deleting the stored files)
    ?DBG("varm_mib_start -> stop the agent", []),
    Config2 = stop_agent(Config1),

    %% Sleep some in order for the agent to stop properly
    ?DBG("varm_mib_start -> sleep some (before re-starting the agent)", []),
    ?SLEEP(5000),

    %% Start the agent (again)
    ?DBG("varm_mib_start -> start the agent", []),
    Config3 = start_v1_agent(Config2, Opts),

    ?DBG("varm_mib_start -> sleep some (before starting tests)", []),
    ?SLEEP(5000),

    %% Perform the test(s)
    ?DBG("varm_mib_start -> perform the tests", []),
    try_test(snmp_community_mib_test),
    try_test(snmp_framework_mib_test),
    try_test(snmp_target_mib_test),
    try_test(snmp_notification_mib_test),

    %% Stop the agent (without deleting the stored files)
    ?DBG("varm_mib_start -> stop the agent", []),
    stop_agent(Config3),
    ok.


-define(snmpTrapCommunity_instance, [1,3,6,1,6,3,18,1,4,0]).
-define(vacmViewSpinLock_instance, [1,3,6,1,6,3,16,1,5,1,0]).
-define(usmStatsNotInTimeWindows_instance, [1,3,6,1,6,3,15,1,1,2,0]).

ms_me_of(Config) when is_list(Config) ->
    ?P(ms_me_of),
    init_case(Config),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),
    load_master_std("OTP-SNMPEA-MIB"),
    load_master_std("SNMP-COMMUNITY-MIB"),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    load_master_std("SNMP-MPD-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),

    ?SLEEP(2000),

    display_memory_usage(),


    ?DBG("ms_me_of -> find ~w from SNMP-COMMUNITY-MIB",
	[?snmpTrapCommunity_instance]),
    ok = me_of(?snmpTrapCommunity_instance),
    
    ?DBG("ms_me_of -> find ~w from SNMP-VIEW-BASED-ACM-MIB",
         [?vacmViewSpinLock_instance]),
    ok = me_of(?vacmViewSpinLock_instance),
    
    ?DBG("ms_me_of -> find ~w from SNMP-USER-BASED-SM-MIB",
         [?usmStatsNotInTimeWindows_instance]),
    {error, _} = me_of(?usmStatsNotInTimeWindows_instance),
    

    unload_master("OTP-SNMPEA-MIB"),
    unload_master("SNMP-COMMUNITY-MIB"),
    unload_master("SNMP-FRAMEWORK-MIB"),
    unload_master("SNMP-MPD-MIB"),
    unload_master("SNMP-NOTIFICATION-MIB"),
    unload_master("SNMP-TARGET-MIB"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),

    ok.

me_of(Oid) ->
    case snmpa:me_of(Oid) of
	{ok, #me{oid = Oid}} ->
            ok;
	{ok, #me{oid = OtherOid}} ->
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
	{error, Reason} ->
	    {error, Reason};
	Else ->
	    {error, Else}
    end.


ms_mib_of(Config) when is_list(Config) ->
    ?P(ms_mib_of),
    init_case(Config),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),
    load_master_std("OTP-SNMPEA-MIB"),
    load_master_std("SNMP-COMMUNITY-MIB"),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    load_master_std("SNMP-MPD-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),

    ?SLEEP(2000),

    display_memory_usage(),


    ?DBG("ms_mib_of -> find ~w from SNMP-COMMUNITY-MIB",
	[?snmpTrapCommunity_instance]),
    ok = mib_of(?snmpTrapCommunity_instance, 'SNMP-COMMUNITY-MIB'),
    
    ?DBG("ms_mib_of -> find ~w from SNMP-VIEW-BASED-ACM-MIB",
         [?vacmViewSpinLock_instance]),
    ok = mib_of(?vacmViewSpinLock_instance, 'SNMP-VIEW-BASED-ACM-MIB'),
    
    ?DBG("ms_mib_of -> find ~w from SNMP-USER-BASED-SM-MIB",
         [?usmStatsNotInTimeWindows_instance]),
    {error, _} = mib_of(?usmStatsNotInTimeWindows_instance,
			      'SNMP-USER-BASED-SM-MIB'),
    

    unload_master("OTP-SNMPEA-MIB"),
    unload_master("SNMP-COMMUNITY-MIB"),
    unload_master("SNMP-FRAMEWORK-MIB"),
    unload_master("SNMP-MPD-MIB"),
    unload_master("SNMP-NOTIFICATION-MIB"),
    unload_master("SNMP-TARGET-MIB"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),

    ok.

mib_of(Oid, ExpectedMibName) ->
    ?DBG("mib_of -> entry with"
	 "~n   Oid:          ~p"
	 "~n   ExpectedMibName: ~p", [Oid, ExpectedMibName]),
    %% case snmpa:mib_of(Oid) of
    MibOf = snmpa:mib_of(Oid),
    ?DBG("mib_of -> MibOf: ~n~p", [MibOf]),
    case MibOf of
	{ok, ExpectedMibName} ->
            ok;
	{ok, OtherMibName} ->
	    {error, {invalid_mib, ExpectedMibName, OtherMibName}};
	{error, Reason} ->
	    {error, Reason};
	Else ->
	    ?DBG("mib_of -> Else: ~n~p", [Else]),
	    {error, Else}
    end.



init_misc(Config) ->
    init_v1(Config).

finish_misc(Config) ->
    finish_v1(Config).

misc_cases() -> 
    [
     app_info, 
     info_test,
     create_local_db_dir
    ].

app_info(Config) when is_list(Config) ->
    ?P(app_info),
    SnmpDir   = app_dir(snmp),
    SslDir    = app_dir(ssl),
    CryptoDir = app_dir(crypto),
    Attr = snmp:module_info(attributes),
    AppVsn = 
	case lists:keysearch(app_vsn, 1, Attr) of
	    {value, {app_vsn, V}} ->
		V;
	    false ->
		"undefined"
	end,
    io:format("Root dir: ~ts~n"
	      "SNMP:   Application dir: ~ts~n"
	      "        Application ver: ~ts~n"
	      "SSL:    Application dir: ~ts~n"
	      "CRYPTO: Application dir: ~ts~n",
	      [code:root_dir(), SnmpDir, AppVsn, SslDir, CryptoDir]),
    ok.

app_dir(App) ->
    case code:lib_dir(App) of
	D when is_list(D) ->
	    filename:basename(D);
	{error, _Reason} ->
	    "undefined"
    end.

create_local_db_dir(Config) when is_list(Config) ->
    Pre = fun() ->
                  DataDir    = snmp_test_lib:lookup(data_dir, Config),
                  T          = {erlang:unique_integer([positive]),
                                erlang:unique_integer([positive]),
                                erlang:unique_integer([positive])},
                  [As,Bs,Cs] = [integer_to_list(I) || I <- tuple_to_list(T)],
                  DbDir      = filename:join([DataDir, As, Bs, Cs]),
                  ?IPRINT("try ensuring db-dir does not exist"),
                  try del_dir(DbDir, 3) of
                      ok ->
                          ok
                  catch
                      C:E:S ->
                          ?WPRINT("Failed pre db-dir delete: "
                                  "~n   Class: ~p"
                                  "~n   Error: ~p"
                                  "~n   Stack: ~p", [C, E, S]),
                          throw({skip, "Failed pre db-dir cleanup"})
                  end,
                  {ok, Peer, Node} = ?START_PEER(""),
                  {DbDir, Peer, Node}
          end,
    Case = fun do_create_local_db_dir/1,
    Post = fun({DbDir, Peer, Node}) ->
                   ?IPRINT("try stop node ~p", [Node]),
                   peer:stop(Peer),
                   ?IPRINT("try delete db-dir"),
                   try del_dir(DbDir, 3)
                   catch
                       C:E:S ->
                           ?WPRINT("Failed post db-dir delete: "
                                   "~n   DbDir  ~s"
                                   "~n   Class: ~p"
                                   "~n   Error: ~p"
                                   "~n   Stack: ~p", [DbDir, C, E, S]),
                           ok
                   end
           end,
    ?TC_TRY(create_local_db_dir, Pre, Case, Post).

do_create_local_db_dir({DbDir, _Peer, Node}) ->
    ?P(create_local_db_dir),
    %% first start with a nonexisting DbDir
    Fun1 = fun() ->
                   false = filelib:is_dir(DbDir),
                   process_flag(trap_exit,true),
                   {error, {error, {failed_open_dets, {file_error, _, _}}}} =
                       snmpa_local_db:start_link(normal, DbDir,
                                                 [{verbosity, trace}]),
                   false = filelib:is_dir(DbDir),
                   {ok, not_found}
           end,
    {ok, not_found} = nodecall(Node, Fun1),
    %% now start with a nonexisting DbDir but pass the
    %% create_local_db_dir option as well
    Fun2 = fun() ->
                   false = filelib:is_dir(DbDir),
                   process_flag(trap_exit,true),
                   {ok, _Pid} =
                       snmpa_local_db:start_link(normal, DbDir,
                                                 create_db_and_dir, [{verbosity,trace}]),
                   snmpa_local_db:stop(),
                   true = filelib:is_dir(DbDir),
                   {ok, found}
           end,
    {ok, found} = nodecall(Node, Fun2),
    ok.

nodecall(Node, Fun) ->
    Parent = self(),
    Ref    = make_ref(),
    Pid    = spawn_link(Node,
                        fun() ->
                                Res = Fun(),
                                unlink(Parent),
                                Parent ! {Ref, Res}
                        end),
    receive
        %% Just so we are not left hanging
        {'EXIT', Pid, Reason} ->
            Reason;
        {Ref, Res} ->
            Res
    end.

del_dir(_Dir, 0) ->
    ok;
del_dir(Dir, Depth) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            lists:map(fun(F) ->
                              Nm = filename:join(Dir,F),
                              ok = file:delete(Nm)
                      end, Files),
            ok = file:del_dir(Dir),
            del_dir(filename:dirname(Dir), Depth-1);
        false ->
            ok
    end.

%v1_cases() -> [loop_mib_1];
v1_cases() -> 
    [
     simple, 
     db_notify_client, 
     v1_processing, 
     big, 
     big2,
     loop_mib_1,
     api, 
     subagent, 
     mnesia, 
     {group, multiple_reqs},
     sa_register, 
     v1_trap, 
     sa_error, 
     next_across_sa, 
     undo,
     {group, reported_bugs}, 
     {group, standard_mibs},
     sparse_table, 
     cnt_64, 
     opaque, 
     change_target_addr_config
    ].  

v1_cases_ipv6() ->
    [
     simple,
     v1_processing,
     loop_mib_1,
%%     big,
%%     big2,
     api,
     subagent,
%%     mnesia,
%%     {group, multiple_reqs},
     sa_register,
%%     v1_trap, % sends v1 trap
%%     sa_error,
     next_across_sa,
     undo,
%%     {group, reported_bugs},
     {group, standard_mibs_ipv6},
     sparse_table,
%%     cnt_64, % sends v1 trap
     opaque
%%     change_target_addr_config % sends v1 trap
    ].

init_v1(Config) when is_list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    IpFamily = config_ipfamily(Config),
    config(
	    [v1], MgrDir, AgentConfDir,
	    tuple_to_list(Ip), tuple_to_list(Ip), IpFamily),
    [{vsn, v1} | start_v1_agent(Config)].

finish_v1(Config) when is_list(Config) ->
    ?IPRINT("finish_v1 -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_v1 -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_v1 -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_v1 -> done"),
    lists:keydelete(vsn, 1, C1).


v2_cases() -> 
    [
     simple_2, 
     v2_processing, 
     big_2, 
     big2_2, 
     loop_mib_2,
     api_2, 
     subagent_2, 
     mnesia_2, 
     {group, multiple_reqs_2},
     sa_register_2, 
     v2_trap, 
     {group, v2_inform}, 
     sa_error_2,
     next_across_sa_2, 
     undo_2, 
     {group, reported_bugs_2},
     {group, standard_mibs_2}, 
     v2_types, 
     implied,
     sparse_table_2, 
     cnt_64_2, 
     opaque_2, 
     v2_caps
    ].

v2_cases_ipv6() ->
    [
     simple_2,
     v2_processing,
%%     big_2,
%%     big2_2,
     loop_mib_2,
     api_2,
     subagent_2,
%%     mnesia_2,
%%     {group, multiple_reqs_2},
     sa_register_2,
     v2_trap,
     {group, v2_inform},
%%     sa_error_2,
     next_across_sa_2,
     undo_2,
%%     {group, reported_bugs_2},
     {group, standard_mibs_2},
     v2_types,
     implied,
     sparse_table_2,
     cnt_64_2,
     opaque_2,
     v2_caps
    ].

init_v2(Config) when is_list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    IpFamily = config_ipfamily(Config),
    config(
      [v2], MgrDir, AgentConfDir,
      tuple_to_list(Ip), tuple_to_list(Ip), IpFamily),
    [{vsn, v2} | start_v2_agent(Config)].

finish_v2(Config) when is_list(Config) ->
    ?IPRINT("finish_v2 -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_v2 -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_v2 -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_v2 -> done"),
    lists:keydelete(vsn, 1, C1).


v1_v2_cases() -> 
    [simple_bi].

init_v1_v2(Config) when is_list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    IpFamily = config_ipfamily(Config),
    config([v1,v2], MgrDir, AgentConfDir, 
	   tuple_to_list(Ip), tuple_to_list(Ip), IpFamily),
    [{vsn, bilingual} | start_bilingual_agent(Config)].

finish_v1_v2(Config) when is_list(Config) ->
    ?IPRINT("finish_v1_v2 -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_v1_v2 -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_v1_v2 -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_v1_v2 -> done"),
    lists:keydelete(vsn, 1, C1).


v3_cases() -> 
    [
     simple_3, 
     v3_processing, 
     big_3, 
     big2_3, 
     api_3,
     subagent_3, 
     mnesia_3, 
     loop_mib_3, 
     {group, multiple_reqs_3},
     sa_register_3, 
     v3_trap, 
     {group, v3_inform}, 
     sa_error_3,
     next_across_sa_3, 
     undo_3, 
     {group, reported_bugs_3},
     {group, standard_mibs_3}, 
     {group, v3_security},
     v2_types_3, 
     implied_3, 
     sparse_table_3, 
     cnt_64_3,
     opaque_3, 
     v2_caps_3
    ].

v3_cases_ipv6() ->
    [
     simple_3,
     v3_processing,
%%     big_3,
%%     big2_3,
     api_3,
     subagent_3,
%%     mnesia_3,
     loop_mib_3,
%%     {group, multiple_reqs_3},
     sa_register_3,
     v3_trap,
     {group, v3_inform},
%%     sa_error_3,
     next_across_sa_3,
     undo_3,
%%     {group, reported_bugs_3},
     {group, standard_mibs_3},
     {group, v3_security},
     v2_types_3,
     implied_3,
     sparse_table_3,
     cnt_64_3,
     opaque_3,
     v2_caps_3
    ].

init_v3(Config) when is_list(Config) ->
    %% Make sure crypto works, otherwise start_agent will fail
    %% and we will be stuck with a bunch of mnesia tables for
    %% the rest of this suite...
    ?DBG("start_agent -> start crypto app",[]),
    case ?CRYPTO_START() of
	ok ->
            ?IPRINT("crypto started - check support"),
	    case ?CRYPTO_SUPPORT() of
		{no, Reason} ->
                    ?WPRINT("crypto support not sufficient:"
                            "~n      ~p", [Reason]),
		    ?SKIP({unsupported_encryption, Reason});
		yes ->
                    ?IPRINT("crypto supported"),
		    ok
	    end;
	{error, Reason} ->
            ?IPRINT("crypto not started:"
                    "~n      ~p", [Reason]),
	    ?SKIP({failed_starting_crypto, Reason})
    end,
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    IpFamily = config_ipfamily(Config),
    ok =
	config(
	  [v3], MgrDir, AgentConfDir,
	  tuple_to_list(Ip), tuple_to_list(Ip), IpFamily),
    Opts =
	[{master_agent_verbosity, trace},
	 {agent_verbosity, trace},
	 {net_if_verbosity, trace}],
    [{vsn, v3} | start_v3_agent(Config, Opts)].

finish_v3(Config) when is_list(Config) ->
    ?IPRINT("finish_v3 -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_v3 -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_v3 -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_v3 -> done"),
    lists:keydelete(vsn, 1, C1).


mt_cases() ->
    [
     multi_threaded, 
     mt_trap
    ].

init_mt(Config, MT) when is_list(Config) ->
    SaNode = ?config(snmp_sa, Config),
    create_tables(SaNode),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir   = ?config(mgr_dir, Config),
    Ip       = ?config(ip, Config),
    ok = config([v2], MgrDir, AgentConfDir,
		      tuple_to_list(Ip), tuple_to_list(Ip)),
    [{vsn, v2} | start_multi_threaded_agent(Config, MT)].

finish_mt(Config) when is_list(Config) ->
    ?IPRINT("finish_mt -> try delete mnesia tables"),
    delete_tables(),
    ?IPRINT("finish_mt -> try stop agent"),
    C1 = stop_agent(Config),
    ?IPRINT("finish_mt -> try delete files"),
    delete_files(C1),
    ?IPRINT("finish_mt -> done"),
    lists:keydelete(vsn, 1, C1).

%% This one *must* be run first in each case.
init_case(Config) ->
    ?ALIB:init_case(Config).


load_master(Mib) ->
    ?DBG("load_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mib(snmp_master_agent, Mib),	% Unload for safety
    ok = snmpa:load_mib(snmp_master_agent, join(get(mib_dir), Mib)).

load_master_std(Mib) ->
    ?DBG("load_master_std -> entry with"
	"~n   Mib: ~p", [Mib]),
    snmpa:unload_mib(snmp_master_agent, Mib),	% Unload for safety
    ok = snmpa:load_mib(snmp_master_agent, join(get(std_mib_dir), Mib)).

unload_master(Mib) ->
    ?DBG("unload_master -> entry with"
	"~n   Mib: ~p", [Mib]),
    ok = snmpa:unload_mib(snmp_master_agent, Mib).

loaded_mibs() ->
    ?DBG("loaded_mibs -> entry",[]),
    Info = snmpa:info(snmp_master_agent),
    {value, {mib_server,  MibInfo}} = lists:keysearch(mib_server, 1, Info),
    {value, {loaded_mibs, Mibs}}    = lists:keysearch(loaded_mibs, 1, MibInfo),
    [atom_to_list(Mib) || {Mib,_,_} <- Mibs].

unload_mibs(Mibs) ->
    ?DBG("unload_mibs -> entry with"
	"~n   Mibs: ~p", [Mibs]),
    ok = snmpa:unload_mibs(snmp_master_agent, Mibs).

start_subagent(SaNode, RegTree, Mib) ->
    ?ALIB:start_subagent(SaNode, RegTree, Mib).

stop_subagent(SA) ->
    ?ALIB:stop_subagent(SA).


%%-----------------------------------------------------------------
%% This function takes care of the old OTP-SNMPEA-MIB.
%% Unfortunately, the testcases were written to use the data in the
%% internal tables, and these table are now obsolete and not used
%% by the agent.  Therefore, we emulate them by using
%% OLD-SNMPEA-MIB, which uses the default impl. of all tables.
%%
%% These two rows must exist in intCommunityTable
%%    {[147,214,36,45], "public", 2, readWrite}.
%%    {[147,214,36,45], "standard trap", 2, read}.
%% (But with the manager's IP address)
%%
%%-----------------------------------------------------------------
init_old() ->
    snmpa_local_db:table_create_row(intCommunityTable,
				    get(mip) ++ [6 | "public"],
				    {get(mip), "public", 2, 2}),
    snmpa_local_db:table_create_row(intCommunityTable,
				    get(mip) ++ [13 | "standard trap"],
				    {get(mip), "standard trap", 2, 1}),
    snmpa_local_db:variable_set(intAgentIpAddress, [127,0,0,1]).
    
				    

simple(Config) when is_list(Config) ->
    ?P(simple),
    init_case(Config),
    
    try_test(simple_standard_test),

    ?IPRINT("done"),
    ok.

simple_2(X) -> ?P(simple_2), simple(X).

simple_bi(Config) when is_list(Config) ->
    ?P(simple_bi),
    init_case(Config),

    put(vsn, v1), % First, try v1 manager
    try_test(simple_standard_test),
    
    put(vsn, v2), % Then, try v2 manager
    try_test(simple_standard_test).
    
simple_3(X) ->
    ?P(simple_3), simple(X).

big(Config) when is_list(Config) ->
    ?P(big),
    %% put(sname, {?MODULE, big}),
    %% put(verbosity, trace),

    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("Starting subagent..."),
    pong = net_adm:ping(SaNode),
    
    {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?DBG("big -> SA: ~p", [SA]),
    load_master("OLD-SNMPEA-MIB"),
    init_old(),

    snmpa:dump_mibs(),
    snmpa:dump_mibs("dumped_mibs.txt"),
    io:format("Local DB: ~n~p~n", [snmpa_local_db:print()]),

    try_test(big_test),

    stop_subagent(SA),
    unload_master("OLD-SNMPEA-MIB").

big_2(X) -> ?P(big_2), big(X).

big_3(X) -> ?P(big_3), big(X).

     
big2(Config) when is_list(Config) ->
    ?P(big2), 
    %% This is exactly the same tests as 'big', but with the
    %% v2 equivalent of the mibs.
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("Starting subagent..."),
    pong = net_adm:ping(SaNode),
    
    {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1-v2"),
    load_master("OLD-SNMPEA-MIB-v2"),
    init_old(),
    try_test(big_test),
    stop_subagent(SA),
    unload_master("OLD-SNMPEA-MIB-v2").

big2_2(X) -> ?P(big2_2), big2(X).

big2_3(X) -> ?P(big2_3), big2(X).
    

multi_threaded(Config) when is_list(Config) ->
    ?P(multi_threaded), 
    init_case(Config),
    
    load_master("Test1"),
    try_test(multi_threaded_test),
    unload_master("Test1").

mt_trap(Config) when is_list(Config) ->
    ?P(mt_trap), 
    init_case(Config),
    MA = whereis(snmp_master_agent),
    MT = ?config(multi_threaded, Config),
    
    load_master("Test1"),
    load_master("TestTrapv2"),
    try_test(mt_trap_test, [MA, MT]),
    unload_master("TestTrapv2"),
    unload_master("Test1"),
    ok.

v2_types(Config) when is_list(Config) ->
    ?P(v2_types), 
    init_case(Config),

    load_master("Test1"),
    try_test(types_v2_test),
    unload_master("Test1").

v2_types_3(X) -> ?P(v2_types_3), v2_types(X).
    

implied(Config) when is_list(Config) ->
    ?P(implied), 
    init_case(Config),
    MA = whereis(snmp_master_agent),

    load_master("Test1"),
    try_test(implied_test,[MA]),
    unload_master("Test1").

implied_3(X) -> ?P(implied_3), implied(X).
    

sparse_table(Config) when is_list(Config) ->
    ?P(sparse_table), 
    init_case(Config),

    load_master("Test1"),
    try_test(sparse_table_test),
    unload_master("Test1").

sparse_table_2(X) -> ?P(sparse_table_2), sparse_table(X).

sparse_table_3(X) -> ?P(sparse_table_3), sparse_table(X).

cnt_64(Config) when is_list(Config) ->
    ?P(cnt_64), 
    init_case(Config),
    MA = whereis(snmp_master_agent),

    load_master("Test1"),
    try_test(cnt_64_test, [MA]),
    unload_master("Test1").

cnt_64_2(X) -> ?P(cnt_64_2), cnt_64(X).

cnt_64_3(X) -> ?P(cnt_64_3), cnt_64(X).

opaque(Config) when is_list(Config) ->
    ?P(opaque), 
    init_case(Config),

    load_master("Test1"),
    try_test(opaque_test),
    unload_master("Test1").

opaque_2(X) -> ?P(opaque_2), opaque(X).

opaque_3(X) -> ?P(opaque_2), opaque(X).


change_target_addr_config(Config) when is_list(Config) ->
    ?P(change_target_addr_config), 
    ?IPRINT("change_target_addr_config -> entry"),
    init_case(Config),

    put(sname,snmp_suite),
    put(verbosity,trace),

    MA = whereis(snmp_master_agent),

    ?IPRINT("change_target_addr_config -> load TestTrap"),
    load_master("TestTrap"),

    ?IPRINT("change_target_addr_config -> set trace verbosity for local_db"),
    snmpa:verbosity(local_db,trace),

    %% First send some traps that will arrive att the original manager
    ?IPRINT("change_target_addr_config -> send trap"),
    try_test(ma_trap1, [MA]),

    ?IPRINT("change_target_addr_config -> set silence verbosity for local_db"),
    snmpa:verbosity(local_db, silence),

    %% Start new dummy listener
    ?IPRINT("change_target_addr_config -> start dummy manager"),
    {ok,Pid,NewPort} = dummy_manager_start(MA),
    
    %% Reconfigure
    ?IPRINT("change_target_addr_config -> reconfigure"),
    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_addr_conf(AgentConfDir, NewPort),
    snmp_target_mib:reconfigure(AgentConfDir),

    %% Send the trap again
    ?IPRINT("change_target_addr_config -> send trap again"),
    catch dummy_manager_send_trap2(Pid),

    ?IPRINT("change_target_addr_config -> await trap ack"),
    catch dummy_manager_await_trap2_ack(),

    ?IPRINT("change_target_addr_config -> stop dummy manager"),
    ok = dummy_manager_stop(Pid),

    ?IPRINT("change_target_addr_config -> reset target address config"),
    reset_target_addr_conf(AgentConfDir),

    ?IPRINT("change_target_addr_config -> unload TestTrap"),
    unload_master("TestTrap").


dummy_manager_start(MA) ->
    ?DBG("dummy_manager_start -> entry",[]),
    Pid = spawn(get(mgr_node), ?MODULE, dummy_manager_init, [self(), MA]),
    ?DBG("dummy_manager_start -> Pid: ~p",[Pid]),
    await_dummy_manager_started(Pid).

await_dummy_manager_started(Pid) ->
    receive
	{dummy_manager_started,Pid,Port} ->
	    ?DBG("dummy_manager_start -> acknowledge received with"
		"~n   Port: ~p",[Port]),
	    {ok,Pid,Port};

	{'EXIT', Pid, Reason} ->
            ?EPRINT("dummy manager terminated: "
                    "~n      ~p", [Reason]),
	    {error, Pid, Reason};

	_O ->
	    ?NPRINT("dummy_manager_start -> received unknown message:"
                    "~n   ~p", [_O]),
	    await_dummy_manager_started(Pid)
    end.

dummy_manager_stop(Pid) ->
    ?DBG("dummy_manager_stop -> entry with Pid: ~p",[Pid]),
    Pid ! stop,
    receive
	{dummy_manager_stopping, Pid} -> 
	    ?DBG("dummy_manager_stop -> acknowledge received",[]),
	    ok
    after 10000 ->
	    ?EPRINT("dummy_manager_stop -> timeout"),
	    timeout
    end.

dummy_manager_send_trap2(Pid) ->
    ?DBG("dummy_manager_send_trap2 -> entry",[]),
    Pid ! {send_trap,testTrap2}.

dummy_manager_await_trap2_ack() ->
    ?DBG("dummy_manager_await_trap2 -> entry",[]),
    receive
	{received_trap, _Trap} ->
	    ?IPRINT("dummy_manager_await_trap2 -> received trap: ~p", [_Trap]),
	    %% Note: 
	    %% Without this sleep the v2_inform_i testcase fails! There
	    %% is no relation between these two test cases as far as I
	    %% able to figure out...
	    ?SLEEP(60000),
	    ok;
	_O ->
	    ?WPRINT("dummy_manager_await_trap2 -> unexpected message: ~p",[_O]),
	    ok
    after 10000 ->
	    ?EPRINT("dummy_manager_await_trap2 -> timeout",[]),
	    timeout
    end.

dummy_manager_init(Parent,MA) ->
    ?DBG("dummy_manager_init -> entry with"
	   "~n   Parent: ~p"
	   "~n   MA:     ~p",[Parent,MA]),
    {ok,S} = gen_udp:open(0,[{recbuf,65535}]),
    ?DBG("dummy_manager_init -> S: ~p",[S]),
    {ok,Port} = inet:port(S),
    ?DBG("dummy_manager_init -> Port: ~p",[Port]),
    Parent ! {dummy_manager_started,self(),Port},
    dummy_manager_loop(Parent,S,MA).

dummy_manager_loop(P,S,MA) ->
    ?IPRINT("dummy_manager_loop -> ready for receive"),
    receive
	{send_trap,Trap} ->
	    ?IPRINT("dummy_manager_loop -> received trap send request"
                    "~n   Trap: ~p", [Trap]),
	    snmpa:send_trap(MA, Trap, "standard trap"),
	    dummy_manager_loop(P,S,MA);
	{udp, _UdpId, _Ip, _UdpPort, Bytes} ->
	    ?IPRINT("dummy_manager_loop -> received upd message"
                    "~n   from: ~p:~p"
                    "~n   size: ~p",
                    [_Ip, _UdpPort, dummy_manager_message_sz(Bytes)]),
	    R = dummy_manager_handle_message(Bytes),
	    ?DBG("dummy_manager_loop -> R: ~p", [R]),
	    P ! R,
	    dummy_manager_loop(P, S, MA);
	stop ->
	    ?DBG("dummy_manager_loop -> received stop request",[]),
	    P ! {dummy_manager_stopping, self()},
	    gen_udp:close(S),
	    exit(normal);
	_O ->
	    ?WPRINT("dummy_manager_loop -> received unknown message:"
                    "~n   ~p", [_O]),
	    dummy_manager_loop(P, S, MA)
    end.

%% -ifdef(snmp_log).
dummy_manager_message_sz(B) when is_binary(B) ->
    byte_size(B);
dummy_manager_message_sz(L) when is_list(L) ->
    length(L);
dummy_manager_message_sz(_) ->
    undefined.
%% -endif.

dummy_manager_handle_message(Bytes) ->
    case (catch snmp_pdus:dec_message(Bytes)) of
	{'EXIT',Reason} ->
	    ?EPRINT("dummy_manager_handle_message -> "
		   "failed decoding message only:"
                    "~n      ~p", [Reason]),
	    {error, Reason};
	M ->
	    ?DBG("dummy_manager_handle_message -> decoded message:"
		   "~n   ~p", [M]),
	    {received_trap, M}
    end.


api(Config) when is_list(Config) ->
    ?P(api),
    init_case(Config),

    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(api_test, [node()]),
    unload_master("OLD-SNMPEA-MIB").

api_2(X) -> ?P(api_2), api(X).

api_3(X) -> ?P(api_3), api(X).


subagent(Config) when is_list(Config) ->
    ?P(subagent), 
    {SaNode, _MgrNode, MibDir} = init_case(Config),

    ?NPRINT("try start subagent..."),
    {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?NPRINT("try test case load_test_sa..."),
    try_test(load_test_sa),
    
    ?NPRINT("Testing unregister subagent..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmpa, unregister_subagent, [MA, SA]),
    ?NPRINT("try test case unreg_test..."),
    try_test(unreg_test),

    ?NPRINT("Loading previous subagent mib in master and testing..."),
    ok = snmpa:load_mib(MA, join(MibDir, "Klas1")),
    ?NPRINT("try test case load_test..."),
    try_test(load_test),

    ?NPRINT("Unloading previous subagent mib in master and testing..."),
    ok = snmpa:unload_mib(MA, join(MibDir, "Klas1")),
    ?NPRINT("try test case unreg_test..."),
    try_test(unreg_test),

    ?NPRINT("Testing register subagent..."),
    rpc:call(SaNode, snmpa, register_subagent, [MA, ?klas1, SA]),
    ?NPRINT("try test case load_test_sa..."),
    try_test(load_test_sa),

    ?NPRINT("try stop subagent..."),
    stop_subagent(SA),
    ?NPRINT("try test case unreg_test..."),
    try_test(unreg_test),

    ?NPRINT("done"),
    ok.
    
subagent_2(X) -> ?P(subagent_2), subagent(X).

subagent_3(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(subagent_3), 
    subagent(X).


mnesia(Config) when is_list(Config) ->
    ?P(mnesia), 
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("Starting subagent with mnesia impl..."),
    {ok, SA} = start_subagent(SaNode, ?klas2, "Klas2"),
    load_master("OLD-SNMPEA-MIB"),
    init_old(),

    try_test(big_test_2),

    ?NPRINT("Testing unregister subagent..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmpa, unregister_subagent, [MA, SA]),
    try_test(unreg_test),
    unload_master("OLD-SNMPEA-MIB"),
    stop_subagent(SA).

mnesia_2(X) -> ?P(mnesia_2), mnesia(X).

mnesia_3(X) -> ?P(mnesia_3), mnesia(X).


mul_cases() -> 
    [
     mul_get, 
     mul_get_err, 
     mul_next, 
     mul_next_err,
     mul_set,
     mul_set_err
    ].


mul_cases_2() -> 
    [
     mul_get_2, 
     mul_get_err_2, 
     mul_next_2, 
     mul_next_err_2,
     mul_set_2,
     mul_set_err_2
    ].


mul_cases_3() ->
    [
     mul_get_3, 
     mul_get_err_3, 
     mul_next_3, 
     mul_next_err_3, 
     mul_set_3,
     mul_set_err_3
    ].
    

init_mul(Config) when is_list(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),
    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    [{mul_sub, SA} | Config].

finish_mul(Config) when is_list(Config) ->
    init_case(Config),
    
    SA = ?config(mul_sub, Config),
    
    unload_master("OLD-SNMPEA-MIB"),
    stop_subagent(SA),
    lists:keydelete(mul_sub, 1, Config).
    
mul_get(Config) when is_list(Config) ->
    ?P(mul_get), 
    init_case(Config),
    
    ?NPRINT("Testing multiple get..."),
    try_test(do_mul_get).

mul_get_2(X) -> ?P(mul_get_2), mul_get(X).

mul_get_3(X) -> ?P(mul_get_3), mul_get(X).

	     
mul_get_err(Config) when is_list(Config) ->
    ?P(mul_get_err), 
    init_case(Config),
    
    ?NPRINT("Testing multiple get with error..."),
    try_test(do_mul_get_err).

mul_get_err_2(X) -> ?P(mul_get_err_2), mul_get_err(X).

mul_get_err_3(X) -> ?P(mul_get_err_3), mul_get_err(X).

	     
mul_next(Config) when is_list(Config) ->
    ?P(mul_next), 
    init_case(Config),
    
    ?NPRINT("Testing multiple next..."),
    try_test(do_mul_next).

mul_next_2(X) -> ?P(mul_next_2), mul_next(X).

mul_next_3(X) -> ?P(mul_next_3), mul_next(X).

	     
mul_next_err(Config) when is_list(Config) ->
    ?P(mul_next_err), 
    init_case(Config),
    
    ?NPRINT("Testing multiple next..."),
    try_test(do_mul_next_err).

mul_next_err_2(X) -> ?P(mul_next_err_2), mul_next_err(X).

mul_next_err_3(X) -> ?P(mul_next_err_3), mul_next_err(X).

	     
mul_set(Config) when is_list(Config) ->
    ?P(mul_set), 
    init_case(Config),
    
    ?NPRINT("Testing multiple set..."),
    try_test(do_mul_set).

mul_set_2(X) -> ?P(mul_set_2), mul_set(X).

mul_set_3(X) -> ?P(mul_set_3), mul_set(X).

	     
mul_set_err(Config) when is_list(Config) ->
    ?P(mul_set_err), 
    init_case(Config),
    
    ?NPRINT("Testing multiple set with error..."),
    try_test(do_mul_set_err).

mul_set_err_2(X) -> ?P(mul_set_err_2), mul_set_err(X).

mul_set_err_3(X) -> ?P(mul_set_err_3), mul_set_err(X).


sa_register(Config) when is_list(Config) ->
    ?P(sa_register), 
    {SaNode, _MgrNode, MibDir} = init_case(Config),

    ?DBG("sa_register -> start subagent", []),
    ?NPRINT("start subagent..."),
    {ok, SA} = start_subagent(SaNode, ?klas1, "Klas1"),

    ?DBG("sa_register -> unregister subagent", []),
    ?NPRINT("Testing unregister subagent (2)..."),
    MA = whereis(snmp_master_agent),
    rpc:call(SaNode, snmpa, unregister_subagent, [MA, ?klas1]),
    try_test(unreg_test),

    ?NPRINT("Unloading Klas1..."),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:unload_mib(SA, join(MibDir, "Klas1")),

    ?NPRINT("Loading SA-MIB..."),
    ?DBG("sa_register -> unload mibs", []),
    snmpa:load_mib(SA, join(MibDir, "SA-MIB")),
    
    ?NPRINT("register subagent..."),
    ?DBG("sa_register -> register subagent", []),
    rpc:call(SaNode, snmpa, register_subagent, [MA, ?sa, SA]),

    try_test(sa_mib),

    ?NPRINT("stop subagent..."),
    ?DBG("sa_register -> stop subagent", []),
    stop_subagent(SA).
    
sa_register_2(X) -> ?P(sa_register_2), sa_register(X).

sa_register_3(X) -> ?P(sa_register_3), sa_register(X).


v1_trap(Config) when is_list(Config) ->
    ?P(v1_trap), 
    trap1(Config).

trap1(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("start subagent..."),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    ?NPRINT("Testing trap sending from master agent..."),
    MA = whereis(snmp_master_agent),

    ?NPRINT("load TestTrap & TestTrapv2..."), 
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    ?NPRINT("Testing trap sending from master-agent..."),
    try_test(ma_trap1, [MA]),
    try_test(ma_trap2, [MA]),
    try_test(ma_v2_2_v1_trap, [MA]),
    try_test(ma_v2_2_v1_trap2, [MA]),

    ?NPRINT("Testing trap sending from subagent..."),
    try_test(sa_trap1, [SA]),
    try_test(sa_trap2, [SA]),
    try_test(sa_trap3, [SA]),
    
    ?NPRINT("unload TestTrap & TestTrapv2..."), 
    unload_master("TestTrap"),
    unload_master("TestTrapv2"),

    ?NPRINT("stop subagent..."),
    stop_subagent(SA).

v2_trap(Config) when is_list(Config) ->
    ?P(v2_trap), 
    trap2(Config).

trap2(Config) ->
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("start subagent..."),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    ?NPRINT("Testing trap sending from master agent..."),
    MA = whereis(snmp_master_agent),

    ?NPRINT("load TestTrap & TestTrapv2..."), 
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    ?NPRINT("Testing trap sending from master-agent..."),
    try_test(ma_v2_trap1, [MA]),
    try_test(ma_v2_trap2, [MA]),
    try_test(ma_v1_2_v2_trap, [MA]),
    try_test(ma_v1_2_v2_trap2, [MA]),

    try_test(sa_mib),

    ?NPRINT("Testing trap sending from subagent..."),
    try_test(sa_v1_2_v2_trap1, [SA]),
    try_test(sa_v1_2_v2_trap2, [SA]),
    try_test(sa_v1_2_v2_trap3, [SA]),
    
    ?NPRINT("unload TestTrap & TestTrapv2..."), 
    unload_master("TestTrap"),
    unload_master("TestTrapv2"),

    ?NPRINT("stop subagent..."),
    stop_subagent(SA).

v3_trap(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(v3_trap), 
    trap2(Config).


v3_inform_cases() ->
    [
     v3_inform_i
    ].

init_v3_inform(X) ->
    init_v2_inform(X).

finish_v3_inform(X) ->
    finish_v2_inform(X).


init_v2_inform(Config) when is_list(Config) ->
    _Dir = ?config(agent_conf_dir, Config),
    %% snmp_internal_mib:configure(Dir),
    Config.

finish_v2_inform(Config) when is_list(Config) ->
    _Dir = ?config(agent_conf_dir, Config),
    %% snmp_internal_mib:configure(Dir),
    Config.

v2_inform_cases() ->
    [
     v2_inform_i
    ].

v2_inform_i(Config) when is_list(Config) ->
    ?P(v2_inform_i), 
    inform_i(Config).

inform_i(Config) ->
    init_case(Config),

    MA = whereis(snmp_master_agent),

    ?NPRINT("load TestTrap & TestTrapv2..."), 
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    ?NPRINT("Testing inform sending from master agent...  "
            "~n   NOTE! This test takes a few minutes (10) to complete."),

    try_test(ma_v2_inform1, [MA]),
    try_test(ma_v2_inform2, [MA]),
    try_test(ma_v2_inform3, [MA]),

    ?NPRINT("unload TestTrap & TestTrapv2..."), 
    unload_master("TestTrap"),
    unload_master("TestTrapv2"),
    ok.

v3_inform_i(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(v3_inform_i), 
    inform_i(X).


sa_error(Config) when is_list(Config) ->
    ?P(sa_error), 
    {SaNode, _MgrNode, _MibDir} = init_case(Config),

    ?NPRINT("load OLD-SNMPEA-MIB..."), 
    load_master("OLD-SNMPEA-MIB"),
    init_old(),

    ?NPRINT("start subagent..."),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    ?NPRINT("Testing sa bad value (is_set_ok)..."),
    try_test(sa_errs_bad_value),

    ?NPRINT("Testing sa gen err (set)..."),
    try_test(sa_errs_gen_err),

    ?NPRINT("Testing too big..."),
    try_test(sa_too_big),

    ?NPRINT("unload OLD-SNMPEA-MIB..."), 
    unload_master("OLD-SNMPEA-MIB"),

    ?NPRINT("stop subagent..."),
    stop_subagent(SA).

sa_error_2(X) -> 
    ?P(sa_error_2), 
    sa_error(X).

sa_error_3(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(sa_error_3), 
    sa_error(X).


next_across_sa(Config) when is_list(Config) ->
    ?P(next_across_sa), 
    {SaNode, _MgrNode, MibDir} = init_case(Config),
    MA = whereis(snmp_master_agent),

    ?NPRINT("start subagent (1)..."),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    ?NPRINT("Loading another subagent mib (Klas1)..."),
    ok = snmpa:load_mib(SA, MibDir ++ "Klas1"),

    ?NPRINT("register subagent..."), 
    rpc:call(SaNode, snmpa, register_subagent, [MA, ?klas1, SA]),

    ?NPRINT("Load test subagent..."),
    try_test(load_test_sa),
    
    ?NPRINT("Testing next across subagent (endOfMibView from SA)..."),
    try_test(next_across_sa_test),

    ?NPRINT("Unloading mib (Klas1)"),
    snmpa:unload_mib(SA, join(MibDir, "Klas1")),
    rpc:call(SaNode, snmpa, unregister_subagent, [MA, ?klas1]),
    try_test(unreg_test),

    ?NPRINT("Starting another subagent (2) "),
    {ok, SA2} = start_subagent(SaNode, ?klas1, "Klas1"),
    ?NPRINT("Testing next across subagent (wrong prefix from SA)..."),
    try_test(next_across_sa_test),
    
    ?NPRINT("stop subagent (1)..."),
    stop_subagent(SA),

    ?NPRINT("stop subagent (2)..."),
    stop_subagent(SA2).

next_across_sa_2(X) -> 
    ?P(next_across_sa_2), 
    next_across_sa(X).

next_across_sa_3(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(next_across_sa_3), 
    next_across_sa(X).


undo(Config) when is_list(Config) ->
    ?P(undo), 
    {SaNode, _MgrNode, MibDir} = init_case(Config),

    MA = whereis(snmp_master_agent),

    ?NPRINT("start subagent (1)..."),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),

    ?NPRINT("Load Klas3 & Klas4..."),
    ok = snmpa:load_mib(MA, join(MibDir, "Klas3")),
    ok = snmpa:load_mib(MA, join(MibDir, "Klas4")),

    ?NPRINT("Testing undo phase at master agent..."),
    try_test(undo_test),
    try_test(api_test2),

    ?NPRINT("Unload Klas3..."),
    ok = snmpa:unload_mib(MA, join(MibDir, "Klas3")),

    ?NPRINT("Testing bad return values from instrum. funcs..."),
    try_test(bad_return),

    ?NPRINT("Unload Klas4..."),
    ok = snmpa:unload_mib(MA, join(MibDir, "Klas4")),

    ?NPRINT("Testing undo phase at subagent..."),
    ok = snmpa:load_mib(SA, join(MibDir, "Klas3")),
    ok = snmpa:load_mib(SA, join(MibDir, "Klas4")),
    ok = snmpa:register_subagent(MA, ?klas3, SA),
    ok = snmpa:register_subagent(MA, ?klas4, SA),
    try_test(undo_test),
    try_test(api_test3),

    ?NPRINT("Testing undo phase across master/subagents..."),
    try_test(undo_test),
    try_test(api_test3),

    ?NPRINT("stop subagent..."), 
    stop_subagent(SA).

undo_2(X) -> 
    ?P(undo_2), 
    undo(X).

undo_3(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(undo_3), 
    undo(X).

%% Req. Test2
v1_processing(Config) when is_list(Config) ->
    ?P(v1_processing), 
    ?DBG("v1_processing -> entry", []),
    init_case(Config),

    ?NPRINT("Load Test2..."), 
    load_master("Test2"),
    
    try_test(v1_proc),
    
    ?NPRINT("Unload Test2..."), 
    unload_master("Test2").

%% Req. Test2
v2_processing(Config) when is_list(Config) ->
    ?P(v2_processing), 
    init_case(Config),

    ?NPRINT("Load Test2..."), 
    load_master("Test2"),

    try_test(v2_proc),

    ?NPRINT("Unload Test2..."), 
    unload_master("Test2").

%% Req. Test2
v3_processing(Config) when is_list(Config) ->
    ?P(v3_processing), 
    init_case(Config),

    ?NPRINT("Load Test2..."), 
    load_master("Test2"),

    try_test(v2_proc), % same as v2!

    ?NPRINT("Unload Test2..."), 
    unload_master("Test2").


%% We'll try get/set/trap and inform for all the auth & priv protocols.
%% For informs, the mgr is auth-engine. The agent has to sync.  This is
%% accomplished by the first inform sent.  That one will generate a
%% report, which makes it in sync.  The notification-generating
%% application times out, and send again.  This time it'll work.

v3_security_cases() ->
    [
     v3_crypto_basic, 
     v3_md5_auth, 
     v3_sha_auth,
     v3_sha224_auth,
     v3_sha256_auth,
     v3_sha384_auth,
     v3_sha512_auth,
     v3_des_priv
    ].


v3_crypto_basic(_Config) ->
    ?P(v3_crypto_basic), 
    EID = [0,0,0,0,0,0,0,0,0,0,0,2],
    %% From rfc2274 appendix A.3.1
    KMd5_1 = snmp:passwd2localized_key(md5, "maplesyrup", EID),
    [16#52,16#6f,16#5e,16#ed,16#9f,16#cc,16#e2,16#6f,
	   16#89,16#64,16#c2,16#93,16#07,16#87,16#d8,16#2b] =
	KMd5_1,
    %% From rfc2274 appendix A.3.2
    KSHA_1 = snmp:passwd2localized_key(sha, "maplesyrup", EID),
    [16#66,16#95,16#fe,16#bc,16#92,16#88,16#e3,16#62,16#82,16#23,
	   16#5f,16#c7,16#15,16#1f,16#12,16#84,16#97,16#b3,16#8f,16#3f] = 
	KSHA_1,
    %% From rfc2274, appendix A.5.1
    KMd5_2 = snmp:passwd2localized_key(md5, "newsyrup", EID),
    [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#88,16#05,16#61,16#51,16#41,16#67,16#6c,16#c9,
	   16#19,16#61,16#74,16#e7,16#42,16#a3,16#25,16#51] =
	snmp_user_based_sm_mib:mk_key_change(md5, KMd5_1, KMd5_2, 16,
					     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    %% From rfc2274, appendix A.5.2
    KSHA_2 = snmp:passwd2localized_key(sha, "newsyrup", EID),
    [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#9c,16#10,16#17,16#f4,
	   16#fd,16#48,16#3d,16#2d,16#e8,16#d5,16#fa,16#db,
	   16#f8,16#43,16#92,16#cb,16#06,16#45,16#70,16#51] =
	snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1, KSHA_2, 20,
			     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    KSHA_1t = lists:sublist(KSHA_1, 16),
    KSHA_2t = lists:sublist(KSHA_2, 16),
    [16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,
	   16#7e,16#f8,16#d8,16#a4,16#c9,16#cd,16#b2,16#6b,
	   16#47,16#59,16#1c,16#d8,16#52,16#ff,16#88,16#b5] =
	snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1t, KSHA_2t, 16,
					     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),

    %% Try with correct random
    Kc1 = snmp_user_based_sm_mib:mk_key_change(md5, KMd5_1, KMd5_2),
    KMd5_2 = snmp_user_based_sm_mib:extract_new_key(md5, KMd5_1, Kc1),
    Kc2 = snmp_user_based_sm_mib:mk_key_change(sha, KSHA_1, KSHA_2),
    KSHA_2 = snmp_user_based_sm_mib:extract_new_key(sha, KSHA_1, Kc2),
    ok.
    


v3_md5_auth(Config) when is_list(Config) ->
    ?P(v3_md5_auth), 
    init_case(Config),

    ?NPRINT("Testing MD5 authentication...takes a few seconds..."),
    
    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authMD5", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authMD5"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_sha_auth(Config) when is_list(Config) ->
    ?P(v3_sha_auth), 
    init_case(Config),

    ?NPRINT("Testing SHA authentication...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authSHA", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authSHA"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_sha224_auth(Config) when is_list(Config) ->
    ?P(v3_sha224_auth), 
    init_case(Config),

    ?NPRINT("Testing SHA224 authentication...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authSHA224", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authSHA224"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_sha256_auth(Config) when is_list(Config) ->
    ?P(v3_sha256_auth), 
    init_case(Config),

    ?NPRINT("Testing SHA256 authentication...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authSHA256", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authSHA256"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_sha384_auth(Config) when is_list(Config) ->
    ?P(v3_sha384_auth), 
    init_case(Config),

    ?NPRINT("Testing SHA authentication...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authSHA384", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authSHA384"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_sha512_auth(Config) when is_list(Config) ->
    ?P(v3_sha512_auth), 
    init_case(Config),

    ?NPRINT("Testing SHA512 authentication...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "authSHA512", authNoPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authNoPriv}, {user, "authSHA512"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

v3_des_priv(Config) when is_list(Config) ->
    ?P(v3_des_priv), 
    init_case(Config),

    ?NPRINT("Testing DES encryption...takes a few seconds..."),

    AgentConfDir = ?config(agent_conf_dir, Config),
    rewrite_target_params_conf(AgentConfDir, "privDES", authPriv),
    snmp_target_mib:reconfigure(AgentConfDir),

    MA = whereis(snmp_master_agent),

    snmp_user_based_sm_mib:usmUserTable(print),

    load_master("Test2"),
    load_master("TestTrap"),
    load_master("TestTrapv2"),

    try_test(v3_sync, [[{v2_proc, []},
			{ma_v2_trap1, [MA]},
			{v3_inform_sync, [MA]}]],
	     [{sec_level, authPriv}, {user, "privDES"}]),

    unload_master("TestTrapv2"),
    unload_master("TestTrap"),
    unload_master("Test2"),
    reset_target_params_conf(AgentConfDir).

%% -define(usmStatsNotInTimeWindows_instance, [1,3,6,1,6,3,15,1,1,2,0]).

%% Make sure mgr is in sync with agent
v3_sync(Funcs) ->
    ?DBG("v3_sync -> entry with Funcs: ~p",[Funcs]),
    g([[sysDescr, 0]]),
    ?expect2(report, [{?usmStatsNotInTimeWindows_instance, any}]),
    g([[sysDescr, 0]]),
    ?expect1([{[sysDescr,0], any}]),
    lists:foreach(fun({Func, Args}) -> apply(?MODULE, Func, Args) end, Funcs).

v3_inform_sync(MA) ->
    ?DBG("v3_sync -> entry with MA: ~p => Send notification",[MA]),
    snmpa:send_notification(MA, testTrapv22, no_receiver,
				 "standard inform", []),
    %% Make sure agent is in sync with mgr...
    ?DBG("v3_sync -> wait some time: ",[]),
    ?SLEEP(20000), % more than 1500*10 in target_addr.conf
    ?DBG("v3_sync -> await response",[]),
    ?expect2({inform, true},
                   [{[sysUpTime, 0], any},
                    {[snmpTrapOID, 0], ?system ++ [0,1]}]).


v2_caps(Config) when is_list(Config) ->
    ?P(v2_caps), 
    init_case(Config),

    try_test(v2_caps_i, [node()]).

v2_caps_3(X) -> ?P(v2_caps_3), v2_caps(X).


v2_caps_i(Node) ->
    Idx = rpc:call(Node, snmpa, add_agent_caps, [[1,2,3,4,5], "test cap"]),
    g([[sysORID, Idx], [sysORDescr, Idx]]),
    ?expect1([{[sysORID, Idx], [1,2,3,4,5]},
		    {[sysORDescr, Idx], "test cap"}]),
    rpc:call(Node, snmpa, del_agent_caps, [Idx]),
    g([[sysORID, Idx]]),
    ?expect1([{[sysORID, Idx], noSuchInstance}]).
    

%% Req. Test2
v1_proc() ->
    ?DBG("v1_proc -> entry", []),
    %% According to RFC1157.
    %% Template: <Section>:<list no>
    v1_get_p(),
    v1_get_next_p(),
    v1_set_p().
    
    
v1_get_p() ->
    %% 4.1.2:1
    g([[test2]]),
    ?expect3(noSuchName, 1, [{[test2], 'NULL'}]),
    g([[tDescr]]),
    ?expect3(noSuchName, 1, [{[tDescr], 'NULL'}]),
    g([[tDescr2,0]]),
    ?expect3(noSuchName, 1, [{[tDescr2,0], 'NULL'}]),
    g([[tDescr3,0]]),
    ?expect3(noSuchName, 1, [{[tDescr3,0], 'NULL'}]),
    g([[tDescr4,0]]),
    ?expect3(noSuchName, 1, [{[tDescr4,0], 'NULL'}]),
    g([[sysDescr, 0], [tDescr,0]]), % Outside mibview
    ?expect3(noSuchName, 2, [{[sysDescr, 0], 'NULL'},
				   {[tDescr,0], 'NULL'}]),
    g([[sysDescr,3]]),
    ?expect3(noSuchName, 1, [{[sysDescr, 3], 'NULL'}]),
    
    %% 4.1.2:2
    g([[tTable]]),
    ?expect3(noSuchName, 1, [{[tTable], 'NULL'}]),
    g([[tEntry]]),
    ?expect3(noSuchName, 1, [{[tEntry], 'NULL'}]),
    
    %% 4.1.2:3
    g([[tTooBig, 0]]),
    ?expect3(tooBig, 0, [{[tTooBig, 0], 'NULL'}]),

    %% 4.1.2:4
    g([[tGenErr1, 0]]),
    ?expect3(genErr, 1, [{[tGenErr1, 0], 'NULL'}]),
    g([[tGenErr2, 0]]),
    ?expect3(genErr, 1, [{[tGenErr2, 0], 'NULL'}]),
    g([[sysDescr, 0], [tGenErr3, 0]]),
    ?expect3(genErr, 2, [{[sysDescr, 0], 'NULL'},
			       {[tGenErr3, 0], 'NULL'}]).
    
    
v1_get_next_p() ->
    %% 4.1.3:1
    gn([[1,3,7,1]]),
    ?expect3(noSuchName, 1, [{[1,3,7,1], 'NULL'}]),
    gn([[tDescr2]]),
    ?expect3(tooBig, 0, any),
    
    %% 4.1.3:2
    gn([[tTooBig]]),
    io:format("We currently don't handle tooBig correct!!!\n"),

    ?expect3(tooBig, 0, any),

    %% 4.1.3:3
    gn([[tGenErr1]]),

    ?expect3(genErr, 1, any),
    gn([[tGenErr2]]),

    ?expect3(genErr, 1, any),
    gn([[sysDescr], [tGenErr3]]),

    ?expect3(genErr, 2, any).
    
v1_set_p() ->
    %% 4.1.5:1
    s([{[1,3,7,0], i, 4}]),
    ?expect3(noSuchName, 1, [{[1,3,7,0], 4}]),
    s([{[tDescr,0], s, "outside mibview"}]),
    ?expect3(noSuchName, 1, [{[tDescr,0], "outside mibview"}]),
    s([{[tDescr3,0], s, "read-only"}]),
    ?expect3(noSuchName, 1, [{[tDescr3,0], "read-only"}]),
    s([{[tDescr3], s, "noSuchObject"}]),
    ?expect3(noSuchName, 1, [{[tDescr3], "noSuchObject"}]),
    s([{[tDescr3,1], s, "noSuchInstance"}]),
    ?expect3(noSuchName, 1, [{[tDescr3,1], "noSuchInstance"}]),
    s([{[tDescr2,0], s, "inconsistentName"}]),
    ?expect3(noSuchName, 1, [{[tDescr2,0], "inconsistentName"}]),

    %% 4.1.5:2
    s([{[tDescr2, 0], i, 4}]),
    ?expect3(badValue, 1, [{[tDescr2, 0], 4}]),
    s([{[tDescr2, 0], s, "badValue"}]),
    ?expect3(badValue, 1, [{[tDescr2, 0], "badValue"}]),
    
    %% 4.1.5:3
    %% The standard is quite incorrect here.  The resp pdu was too big.  In
    %% the resp pdu, we have the original vbs.  In the tooBig pdu we still
    %% have to original vbs => the tooBig pdu is too big as well!!!  It
    %% may not get it to the manager, unless the agent uses 'NULL' instead
    %% of the std-like original value.
    s([{[tTooBig, 0], s, ?tooBigStr}]),
    %% according to std:
%    ?expect3(tooBig, 0, [{[tTooBig, 0], ?tooBigStr}]),
    ?expect3(tooBig, 0, [{[tTooBig, 0], 'NULL'}]),
    
    %% 4.1.5:4
    s([{[tDescr2, 0], s, "is_set_ok_fail"}]),
    ?expect3(genErr, 1, [{[tDescr2, 0], "is_set_ok_fail"}]),
    s([{[tDescr2, 0], s, "commit_fail"}]),
    ?expect3(genErr, 1, [{[tDescr2, 0], "commit_fail"}]).
    
%% Req. Test2
v2_proc() ->
    %% According to RFC1905.
    %% Template: <Section>:<list no>
    ?DBG("v2_proc -> entry",[]),
    v2_get_p(),
    v2_get_next_p(),
    v2_get_bulk_p(),
    v2_set_p().

v2_get_p() ->
    %% 4.2.1:2
    ?DBG("v2_get_p -> entry",[]),
    g([[test2]]),
    ?expect1([{[test2], noSuchObject}]),
    g([[tDescr]]),
    ?expect1([{[tDescr], noSuchObject}]),
    g([[tDescr4,0]]),
    ?expect1([{[tDescr4,0], noSuchObject}]),
    g([[sysDescr, 0], [tDescr,0]]), % Outside mibview
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {[tDescr,0], noSuchObject}]),
    g([[tTable]]),
    ?expect1([{[tTable], noSuchObject}]),
    g([[tEntry]]),
    ?expect1([{[tEntry], noSuchObject}]),
    
    %% 4.2.1:3
    g([[tDescr2,0]]), %% instrum ret noSuchName!!!
    ?expect1([{[tDescr2,0], noSuchInstance}]),
    g([[tDescr3,0]]),
    ?expect1([{[tDescr3,0], noSuchInstance}]),
    g([[sysDescr,3]]),
    ?expect1([{[sysDescr, 3], noSuchInstance}]),
    g([[tIndex,1]]),
    ?expect1([{[tIndex, 1], noSuchInstance}]),

    %% 4.2.1 - any other error: genErr
    g([[tGenErr1, 0]]),
    ?expect3(genErr, 1, [{[tGenErr1, 0], 'NULL'}]),
    g([[tGenErr2, 0]]),
    ?expect3(genErr, 1, [{[tGenErr2, 0], 'NULL'}]),
    g([[sysDescr, 0], [tGenErr3, 0]]),
    ?expect3(genErr, 2, [{[sysDescr, 0], 'NULL'},
			       {[tGenErr3, 0], 'NULL'}]),
    
    %% 4.2.1 - tooBig
    g([[tTooBig, 0]]),
    ?expect3(tooBig, 0, []).

    
v2_get_next_p() ->
    %% 4.2.2:2
    ?DBG("v2_get_next_p -> entry",[]),
    gn([[1,3,7,1]]),
    ?expect1([{[1,3,7,1], endOfMibView}]),
    gn([[sysDescr], [1,3,7,1]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[1,3,7,1], endOfMibView}]),
    gn([[tCnt2, 1]]),
    ?expect1([{[tCnt2,2], 100}]),
    gn([[tCnt2, 2]]),
    ?expect1([{[tCnt2,2], endOfMibView}]),
    
    %% 4.2.2 - any other error: genErr
    gn([[tGenErr1]]),
    ?expect3(genErr, 1, [{[tGenErr1], 'NULL'}]),
    gn([[tGenErr2]]),
    ?expect3(genErr, 1, [{[tGenErr2], 'NULL'}]),
    gn([[sysDescr], [tGenErr3]]),
    ?expect3(genErr, 2, [{[sysDescr], 'NULL'},
			       {[tGenErr3], 'NULL'}]),
    
    %% 4.2.2 - tooBig
    gn([[tTooBig]]),
    ?expect3(tooBig, 0, []).

v2_get_bulk_p() ->
    %% 4.2.3
    ?DBG("v2_get_bulk_p -> entry",[]),
    gb(1, 1, []),
    ?expect1([]),
    gb(-1, 1, []),
    ?expect1([]),
    gb(-1, -1, []),
    ?expect1([]),
    gb(-1, -1, []),
    ?expect1([]),
    gb(2, 0, [[sysDescr], [1,3,7,1]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[1,3,7,1], endOfMibView}]),
    gb(1, 2, [[sysDescr], [1,3,7,1]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[1,3,7,1], endOfMibView}]),
    gb(0, 2, [[sysDescr], [1,3,7,1]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[1,3,7,1], endOfMibView},
		    {[sysObjectID, 0], [1,2,3]},
		    {[1,3,7,1], endOfMibView}]),
    
    gb(2, 2, [[sysDescr], [1,3,7,1], [sysDescr], [1,3,7,1]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[1,3,7,1], endOfMibView},
		    {[sysDescr, 0], "Erlang SNMP agent"},		      
		    {[1,3,7,1], endOfMibView},
		    {[sysObjectID, 0], [1,2,3]},
		    {[1,3,7,1], endOfMibView}]),
    
    gb(1, 2, [[sysDescr], [sysDescr], [tTooBig]]),
    ?expect1([{[sysDescr, 0], "Erlang SNMP agent"},
		    {[sysDescr, 0], "Erlang SNMP agent"}]),
    
    gb(1,12, [[tDescr2], [sysDescr]]), % next one after tDescr2 is tTooBig.
    ?expect1([]),
    
    gb(2,2, [[sysDescr], [sysObjectID], [tGenErr1], [sysDescr]]),
    ?expect3(genErr, 3, [{[sysDescr], 'NULL'},
			       {[sysObjectID], 'NULL'},
			       {[tGenErr1], 'NULL'},
			       {[sysDescr], 'NULL'}]),
    gb(0, 2, [[tCnt2, 1]]),
    ?expect1([{[tCnt2,2], 100},
		    {[tCnt2,2], endOfMibView}]).
    
    
v2_set_p() ->
    %% 4.2.5:1
    ?DBG("v2_set_p -> entry",[]),
    s([{[1,3,7,0], i, 4}]),
    ?expect3(noAccess, 1, [{[1,3,7,0], 4}]),
    s([{[tDescr,0], s, "outside mibview"}]),
    ?expect3(noAccess, 1, [{[tDescr,0], "outside mibview"}]),
    
    %% 4.2.5:2
    s([{[1,3,6,1,0], s, "noSuchObject"}]),
    ?expect3(notWritable, 1, [{[1,3,6,1,0], "noSuchObject"}]),
    
    %% 4.2.5:3
    s([{[tDescr2, 0], i, 4}]),
    ?expect3(wrongType, 1, [{[tDescr2, 0], 4}]),
    s([{[tDescr2, 0], s, "badValue"}]),
    ?expect3(badValue, 1, [{[tDescr2, 0], "badValue"}]),

    %% 4.2.5:4
    s([{[tStr, 0], s, ""}]),
    ?expect3(wrongLength, 1, [{[tStr, 0], ""}]),
    s([{[tStr, 0], s, "12345"}]),
    ?expect3(wrongLength, 1, [{[tStr, 0], "12345"}]),
    
    %% 4.2.5:5 - N/A

    %% 4.2.5:6
    s([{[tInt1, 0], i, 0}]),
    ?expect3(wrongValue, 1, [{[tInt1, 0], 0}]),
    s([{[tInt1, 0], i, 5}]),
    ?expect3(wrongValue, 1, [{[tInt1, 0], 5}]),
    s([{[tInt2, 0], i, 0}]),
    ?expect3(wrongValue, 1, [{[tInt2, 0], 0}]),
    s([{[tInt2, 0], i, 5}]),
    ?expect3(wrongValue, 1, [{[tInt2, 0], 5}]),
    s([{[tInt3, 0], i, 5}]),
    ?expect3(wrongValue, 1, [{[tInt3, 0], 5}]),
    
    %% 4.2.5:7
    s([{[tDescrX, 1, 1], s, "noCreation"}]),
    ?expect3(noCreation, 1, [{[tDescrX, 1, 1], "noCreation"}]),

    %% 4.2.5:8
    s([{[tDescrX, 1, 2], s, "inconsistentName"}]),
    ?expect3(inconsistentName, 1,
		   [{[tDescrX, 1, 2], "inconsistentName"}]),
    
    %% 4.2.5:9
    s([{[tCnt, 1, 2], i, 5}]),
    ?expect3(notWritable, 1, [{[tCnt, 1, 2], 5}]),
    s([{[tDescr3,0], s, "read-only"}]),
    ?expect3(notWritable, 1, [{[tDescr3,0], "read-only"}]),

    %% 4.2.5:10
    s([{[tDescr2,0], s, "inconsistentValue"}]),
    ?expect3(inconsistentValue, 1,
		   [{[tDescr2,0], "inconsistentValue"}]),
    
    %% 4.2.5:11
    s([{[tDescr2,0], s, "resourceUnavailable"}]),
    ?expect3(resourceUnavailable, 1,
		   [{[tDescr2,0],"resourceUnavailable"}]),
    
    %% 4.2.5:12
    s([{[tDescr2, 0], s, "is_set_ok_fail"}]),
    ?expect3(genErr, 1, [{[tDescr2, 0], "is_set_ok_fail"}]).
    
    %% commitFailed and undoFailed is tested by the 'undo' case.
    

%% Req. OLD-SNMPEA-MIB
table_test() ->
    io:format("Testing simple get, next and set on communityTable...~n"),

    Key1c3 = [intCommunityViewIndex,get(mip),is("public")],
    Key2c3 = [intCommunityViewIndex,get(mip),is("standard trap")],
    Key1c4 = [intCommunityAccess,get(mip),is("public")],
    EndKey = [intCommunityEntry,[9],get(mip),is("public")],
    gn([[intCommunityEntry]]),
    ?expect1([{Key1c3, 2}]),
    gn([[intCommunityTable]]),
    ?expect1([{Key1c3, 2}]),
    gn([[community]]),
    ?expect1([{Key1c3, 2}]),
    gn([[otpSnmpeaMIB]]),
    ?expect1([{Key1c3, 2}]),
    gn([[ericsson]]),
    ?expect1([{Key1c3, 2}]),
    gn([Key1c3]),
    ?expect1([{Key2c3, 2}]),
    gn([Key2c3]),
    ?expect1([{Key1c4, 2}]),
    gn([EndKey]),
    AgentIp = [intAgentIpAddress,0], 
    ?expect1([{AgentIp, any}]),
    g([Key1c3]),
    ?expect1([{Key1c3, 2}]),
    g([EndKey]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{EndKey, noSuchObject}])),

    io:format("Testing row creation/deletion on communityTable...~n"),
    NewKeyc3 = [intCommunityViewIndex,get(mip),is("test")],
    NewKeyc4 = [intCommunityAccess,get(mip),is("test")],
    NewKeyc5 = [intCommunityStatus,get(mip),is("test")],
    s([{NewKeyc5, ?createAndGo}]),
    ?expect3(?v1_2(badValue, inconsistentValue), 1, any),
    s([{NewKeyc5, ?createAndGo}, {NewKeyc3, 2}, {NewKeyc4, 2}]),
    ?expect1([{NewKeyc5, ?createAndGo},{NewKeyc3, 2}, {NewKeyc4, 2}]),
    g([NewKeyc4]),
    ?expect1([{NewKeyc4, 2}]),
    s([{NewKeyc5, ?destroy}]),
    ?expect1([{NewKeyc5, ?destroy}]),
    s([{NewKeyc4, 2}]),
    ?expect3(?v1_2(noSuchName, inconsistentName), 1, [{NewKeyc4, 2}]),
    s([{NewKeyc5, ?createAndWait}]),
    ?expect1([{NewKeyc5, ?createAndWait}]),
    g([NewKeyc5]),
    ?expect1([{NewKeyc5, ?notReady}]),
    s([{NewKeyc4, 2}]),
    ?expect1([{NewKeyc4, 2}]),
    g([NewKeyc5]),
    ?expect1([{NewKeyc5, ?notReady}]),
    s([{NewKeyc3, 2}]),
    ?expect1([{NewKeyc3, 2}]),
    g([NewKeyc5]),
    ?expect1([{NewKeyc5, ?notInService}]),
    s([{NewKeyc5, ?active}]),
    ?expect1([{NewKeyc5, ?active}]),
    s([{NewKeyc5, ?destroy}]),
    ?expect1([{NewKeyc5, ?destroy}]),
    s([{NewKeyc3, 3}]),
    ?expect3(?v1_2(noSuchName, inconsistentName), 1, [{NewKeyc3, 3}]),
    otp_1128_test(),
    ok.

%% Req. system group
simple_standard_test() ->
    ?DBG("simple_standard_test -> entry",[]),
    gn([[1,1]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2,1]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[1,3,6,1,2,1,1]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    gn([[sysDescr]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    g([[sysDescr,0]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"}]),
    g([[sysDescr]]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{[sysDescr], noSuchObject}])),
    g([[1,6,7,0]]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{[1,6,7,0], noSuchObject}])),
    gn([[1,13]]),
    ?v1_2(?expect3(noSuchName,1, any),
		?expect1([{[1,13], endOfMibView}])),
    s([{[sysLocation, 0], "new_value"}]),
    ?expect1([{[sysLocation, 0], "new_value"}]),
    g([[sysLocation, 0]]),
    ?expect1([{[sysLocation, 0], "new_value"}]),
    io:format("Testing noSuchName and badValue...~n"),
    s([{[sysServices,0], 3}]),
    ?expect3(?v1_2(noSuchName, notWritable), 1, any),
    s([{[sysLocation, 0], i, 3}]),
    ?expect3(?v1_2(badValue, wrongType), 1, any),
    ?DBG("simple_standard_test -> done",[]),
    ok.

%% This is run in the agent node
db_notify_client(Config) when is_list(Config) ->
    ?P(db_notify_client), 
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("db_notify_client -> case initiated: "
	 "~n   SaNode:  ~p"
	 "~n   MgrNode: ~p"
	 "~n   MibDir:  ~p", [_SaNode, _MgrNode, _MibDir]),
    ?DBG("db_notify_client -> maximize verbosity", []),
    snmpa_local_db:verbosity(trace),
    Self = self(), 
    ?DBG("db_notify_client -> register self (~p) notify client", [Self]),
    snmpa_local_db:register_notify_client(Self, ?MODULE),

    %% This call (to the manager) will issue to set operations, so
    %% we expect to receive to notify(insert) calls.
    try_test(db_notify_client_test),

    ?DBG("db_notify_client -> await first notify",[]),
    receive 
	{db_notify_test_reply, insert} -> 
	    ?DBG("db_notify_client -> first notify received",[]),
	    ok
    after 10000 ->
	    ?FAIL({timeout, waiting_for_first_event})
    end,
    
    ?DBG("db_notify_client -> await second notify",[]),
    receive 
	{db_notify_test_reply, insert} -> 
	    ?DBG("db_notify_client -> second notify received",[]),
	    ok
    after 10000 ->
	    ?FAIL({timeout, waiting_for_second_event})
    end,

    ?DBG("db_notify_client -> unregister self (~p) notify client", [Self]),
    snmpa_local_db:unregister_notify_client(Self),
    ?DBG("db_notify_client -> minimize verbosity", []),
    snmpa_local_db:verbosity(silence),

    ?DBG("db_notify_client -> done", []),
    ok.


%% This is run in the manager node
db_notify_client_test() ->
    ?DBG("set first new sysLocation",[]),
    s([{[sysLocation, 0], "new_value"}]),
    ?expect1([{[sysLocation, 0], "new_value"}]),

    ?DBG("set second new sysLocation",[]),
    s([{[sysLocation, 0], "new_value"}]),
    ?expect1([{[sysLocation, 0], "new_value"}]).

%% Callback function
notify(Pid, What) -> 
    ?DBG("notify(~p,~p) -> called",[Pid,What]),
    Pid ! {db_notify_test_reply, What}.


%% Req: system group, OLD-SNMPEA-MIB, Klas1
big_test() ->

    ?DBG("big_test -> testing simple next/get/set @ master agent...",[]),
    simple_standard_test(),
    
    ?DBG("big_test -> testing simple next/get/set @ subagent...",[]),
    gn([[klas1]]),
    ?expect1([{[fname,0], ""}]),
    g([[fname,0]]),
    ?expect1([{[fname,0], ""}]),
    s([{[fname,0], s, "test set"}]),
    ?expect1([{[fname,0], "test set"}]),
    g([[fname,0]]),
    ?expect1([{[fname,0], "test set"}]),
    
    ?DBG("big_test -> "
	"testing next from last instance in master to subagent...",[]),
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[fname,0], "test set"}]),
    gn([[1,1], [?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {[fname,0], "test set"}]),
    s([{[fname,0], s, ""}]),
    ?expect1([{[fname,0], ""}]),
    
    table_test(),

    ?DBG("big_test -> adding one row in subagent table",[]),
    _FTab = [friendsEntry],
    s([{[friendsEntry, [2, 3]], s, "kompis3"},
       {[friendsEntry, [3, 3]], i, ?createAndGo}]),
    ?expect1([{[friendsEntry, [2, 3]], "kompis3"},
		    {[friendsEntry, [3, 3]], ?createAndGo}]),
    g([[friendsEntry, [2, 3]],
       [friendsEntry, [3, 3]]]),
    ?expect1([{[friendsEntry, [2, 3]], "kompis3"},
		    {[friendsEntry, [3, 3]], ?active}]),
    s([{[friendsEntry, [3, 3]], i, ?destroy}]),
    ?expect1([{[friendsEntry, [3, 3]], ?destroy}]),
    
    otp_1131_test(),

    ?DBG("big_test -> adding two rows in subagent table with special INDEX",
       []),
    s([{[kompissEntry, [1, 3]], s, "kompis3"},
       {[kompissEntry, [2, 3]], i, ?createAndGo}]),
    ?expect1([{[kompissEntry, [1, 3]], "kompis3"},
		    {[kompissEntry, [2, 3]], ?createAndGo}]),
    g([[kompissEntry, [1, 3]],
       [kompissEntry, [2, 3]]]),
    ?expect1([{[kompissEntry, [1, 3]], "kompis3"},
		    {[kompissEntry, [2, 3]], ?active}]),
    gn([[kompissEntry, [1]],
	[kompissEntry, [2]]]),
    ?expect1([{[kompissEntry, [1, 3]], "kompis3"},
		    {[kompissEntry, [2, 3]], ?active}]),
    s([{[kompissEntry, [1, 2]], s, "kompis3"},
       {[kompissEntry, [2, 2]], i, ?createAndGo}]),
    ?expect1([{[kompissEntry, [1, 2]], "kompis3"},
		    {[kompissEntry, [2, 2]], ?createAndGo}]),
    gn([[kompissEntry, [1, 1]],
	[kompissEntry, [2, 1]]]),
    ?expect1([{[kompissEntry, [1, 2]], "kompis3"},
		    {[kompissEntry, [2, 2]], ?active}]),
    s([{[kompissEntry, [2, 3]], i, ?destroy}]),
    ?expect1([{[kompissEntry, [2, 3]], ?destroy}]),
    s([{[kompissEntry, [2, 2]], i, ?destroy}]),
    ?expect1([{[kompissEntry, [2, 2]], ?destroy}]),
    ?DBG("big_test -> done",[]),
    ok.

%% Req. system group, Klas2, OLD-SNMPEA-MIB
big_test_2() ->
    ?NPRINT("Testing simple next/get/set @ master agent (2)..."),
    simple_standard_test(),
    
    ?NPRINT("Testing simple next/get/set @ subagent (2)..."),
    gn([[klas2]]),
    ?expect1([{[fname2,0], ""}]),
    g([[fname2,0]]),
    ?expect1([{[fname2,0], ""}]),
    s([{[fname2,0], s, "test set"}]),
    ?expect1([{[fname2,0], "test set"}]),
    g([[fname2,0]]),
    ?expect1([{[fname2,0], "test set"}]),

    otp_1298_test(),

    ?NPRINT("Testing next from last object in master to subagent (2)..."),
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[fname2,0], "test set"}]),
    gn([[1,1], [?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {[fname2,0], "test set"}]),
    
    table_test(),
    
    ?NPRINT("Adding one row in subagent table (2)"),
    _FTab = [friendsEntry2],
    s([{[friendsEntry2, [2, 3]], s, "kompis3"},
       {[friendsEntry2, [3, 3]], i, ?createAndGo}]),
    ?expect1([{[friendsEntry2, [2, 3]], "kompis3"},
		    {[friendsEntry2, [3, 3]], ?createAndGo}]),
    g([[friendsEntry2, [2, 3]],
       [friendsEntry2, [3, 3]]]),
    ?expect1([{[friendsEntry2, [2, 3]], "kompis3"},
		    {[friendsEntry2, [3, 3]], ?active}]),
    s([{[friendsEntry2, [3, 3]], i, ?destroy}]),
    ?expect1([{[friendsEntry2, [3, 3]], ?destroy}]),
    
    ?NPRINT("Adding two rows in subagent table with special INDEX (2)"),
    s([{[kompissEntry2, [1, 3]], s, "kompis3"},
       {[kompissEntry2, [2, 3]], i, ?createAndGo}]),
    ?expect1([{[kompissEntry2, [1, 3]], "kompis3"},
		    {[kompissEntry2, [2, 3]], ?createAndGo}]),
    g([[kompissEntry2, [1, 3]],
       [kompissEntry2, [2, 3]]]),
    ?expect1([{[kompissEntry2, [1, 3]], "kompis3"},
		    {[kompissEntry2, [2, 3]], ?active}]),
    gn([[kompissEntry2, [1]],
	[kompissEntry2, [2]]]),
    ?expect1([{[kompissEntry2, [1, 3]], "kompis3"},
		    {[kompissEntry2, [2, 3]], ?active}]),
    s([{[kompissEntry2, [1, 2]], s, "kompis3"},
       {[kompissEntry2, [2, 2]], i, ?createAndGo}]),
    ?expect1([{[kompissEntry2, [1, 2]], "kompis3"},
		    {[kompissEntry2, [2, 2]], ?createAndGo}]),
    gn([[kompissEntry2, [1, 1]],
	[kompissEntry2, [2, 1]]]),
    ?expect1([{[kompissEntry2, [1, 2]], "kompis3"},
		    {[kompissEntry2, [2, 2]], ?active}]),
    s([{[kompissEntry2, [2, 3]], i, ?destroy}]),
    ?expect1([{[kompissEntry2, [2, 3]], ?destroy}]),
    s([{[kompissEntry2, [2, 2]], i, ?destroy}]),
    ?expect1([{[kompissEntry2, [2, 2]], ?destroy}]),
    ok.

%% Req. Test1
multi_threaded_test() ->
    ?NPRINT("Testing multi threaded agent..."),
    g([[multiStr,0]]),
    Pid = get_multi_pid(),
    g([[sysUpTime,0]]),
    ?expect1([{[sysUpTime,0], any}]),
    s([{[sysLocation, 0], s, "pelle"}]),
    ?expect1([{[sysLocation, 0], "pelle"}]),
    Pid ! continue,
    ?expect1([{[multiStr,0], "ok"}]),
    
    s([{[multiStr, 0], s, "block"}]),
    Pid2 = get_multi_pid(),    
    g([[sysUpTime,0]]),
    ?expect1([{[sysUpTime,0], any}]),
    g([[multiStr,0]]),
    Pid3 = get_multi_pid(),
    g([[sysUpTime,0]]),
    ?expect1([{[sysUpTime,0], any}]),
    s([{[sysLocation, 0], s, "kalle"}]),
    Pid3 ! continue,
    ?expect1([{[multiStr,0], "ok"}]),
    Pid2 ! continue,
    ?expect1([{[multiStr,0], "block"}]),
    ?expect1([{[sysLocation,0], "kalle"}]).

%% Req. Test1, TestTrapv2
mt_trap_test(MA, MT) ->
    ?NPRINT("Testing trap-sending with multi threaded (~w) agent...", [MT]),
    ?IPRINT("mt_trap_test(01) -> issue testTrapv22 (standard trap)", []),
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?IPRINT("mt_trap_test(02) -> await v2trap", []),
    ?expect2(v2trap, [{[sysUpTime, 0],   any},
			    {[snmpTrapOID, 0], ?system ++ [0,1]}]),

    %% multi-threaded = true
    %% This will *lock* the 'main thread' of a multi-threaded agent,
    %% the worker state will be 'busy'. Therefore when a new request
    %% arrives a new *temporary* worker will be spawned.
    ?IPRINT("mt_trap_test(03) -> issue mtTrap (standard trap)", []),
    snmpa:send_trap(MA, mtTrap, "standard trap"),
    Pid = get_multi_pid(),
    ?IPRINT("mt_trap_test(04) -> multi pid: ~p. Now request sysUpTime...", [Pid]),
    g([[sysUpTime,0]]),

    ?IPRINT("mt_trap_test(06) -> await sysUpTime", []),
    ?expect1([{[sysUpTime,0], any}]),

    %% This will *only* work if multi-threaded is 'true', not 'extended'
    %% since in the latter case all notifications are serialized through
    %% a dedicated worker, which is now locked (see above).

    if
        (MT =:= true) ->
            ?IPRINT("mt_trap_test(07) -> issue testTrapv22 (standard trap)", []),
            snmpa:send_trap(MA, testTrapv22, "standard trap"),
            ?IPRINT("mt_trap_test(08) -> await v2trap", []),
            ?expect2(v2trap,
                           [{[sysUpTime, 0],   any}, 
                            {[snmpTrapOID, 0], ?system ++ [0,1]}]);
        true ->
            ok
    end,

    ?IPRINT("mt_trap_test(09) -> send continue to multi-pid", []),
    Pid ! continue,

    ?IPRINT("mt_trap_test(10) -> await v2trap", []),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?testTrap ++ [2]},
			    {[multiStr,0], "ok"}]),
    ?IPRINT("mt_trap_test(11) -> done", []),
    ok.

    
get_multi_pid() ->
    get_multi_pid(10).
get_multi_pid(0) ->
    ?FAIL(no_global_name);
get_multi_pid(N) ->
    ?SLEEP(1000),
    case global:whereis_name(snmp_multi_tester) of
	Pid when is_pid(Pid) -> Pid;
	_ -> get_multi_pid(N-1)
    end.

%% Req. Test1
types_v2_test() ->
    ?NPRINT("Testing v2 types..."),

    s([{[bits1,0], 2#10}]),
    ?expect1([{[bits1,0], ?str(2#10)}]),
    g([[bits1,0]]),
    ?expect1([{[bits1,0], ?str(2#101)}]),
    
    s([{[bits2,0], 2#11000000110}]),
    ?expect1([{[bits2,0], ?str(2#11000000110)}]),
    g([[bits2,0]]),
    ?expect1([{[bits2,0], ?str(2#11000000110)}]),
    
    g([[bits3,0]]),
    ?expect3(genErr, 1, any),
    
    g([[bits4,0]]),
    ?expect3(genErr, 1, any),
    
    s([{[bits1,0], s, [2#10]}]),
    ?expect3(?v1_2(badValue, wrongValue), 1, any),

    s([{[bits2,0], 2#11001001101010011}]),
    ?expect3(?v1_2(badValue, wrongValue), 1, any).
    

%% Req. Test1
implied_test(MA) ->
    ?IPRINT("implied_test -> start"),

    ?NPRINT("Testing IMPLIED..."),

    snmpa:verbosity(MA, trace),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = "apa",
    Idx2 = "qq",
    ?DBG("implied_test -> (send) create row 1 '~s' in table 1",[Idx1]),
    s([{[testStatus, Idx1], i, ?createAndGo}, {[testDescr, Idx1],s,"row 1"}]),
    ?expect1([{[testStatus, Idx1], ?createAndGo},
		    {[testDescr, Idx1], "row 1"}]),
    ?DBG("implied_test -> (send) create row 2 '~s' in table 1",[Idx2]),
    s([{[testStatus, Idx2], i, ?createAndGo}, {[testDescr, Idx2],s,"row 2"}]),
    ?expect1([{[testStatus, Idx2], ?createAndGo},
		    {[testDescr, Idx2], "row 2"}]),
    ?DBG("implied_test -> get-next(testDescr)",[]),
    gn([[testDescr]]),
    ?expect1([{[testDescr,Idx1], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr) of row 1",[]),
    gn([[testDescr,Idx1]]),
    ?expect1([{[testDescr,Idx2], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 1",[Idx1]),
    s([{[testStatus, Idx1], i, ?destroy}]),
    ?expect1([{[testStatus, Idx1], ?destroy}]),
    ?DBG("implied_test -> (send) delete row 2 '~s' from table 1",[Idx2]),
    s([{[testStatus, Idx2], i, ?destroy}]),
    ?expect1([{[testStatus, Idx2], ?destroy}]),

    %% Try the same in other table
    Idx3 = [1, "apa"],
    Idx4 = [1, "qq"],
    ?DBG("implied_test -> (send) create row 1 '~s' in table 2",[Idx3]),
    s([{[testStatus2, Idx3], i, ?createAndGo}, {[testDescr2,Idx3],s,"row 1"}]),
    ?expect1([{[testStatus2, Idx3], ?createAndGo},
		    {[testDescr2, Idx3], "row 1"}]),
    ?DBG("implied_test -> (send) create row 2 '~s' in table 2",[Idx4]),
    s([{[testStatus2, Idx4], i, ?createAndGo}, {[testDescr2,Idx4],s,"row 2"}]),
    ?expect1([{[testStatus2, Idx4], ?createAndGo},
		    {[testDescr2, Idx4], "row 2"}]),
    ?DBG("implied_test -> get-next(testDescr2)",[]),
    gn([[testDescr2]]),
    ?expect1([{[testDescr2,Idx3], "row 1"}]),
    ?DBG("implied_test -> get-next(testDescr2) of row 1",[]),
    gn([[testDescr2,Idx3]]),
    ?expect1([{[testDescr2,Idx4], "row 2"}]),

    % Delete the rows
    ?DBG("implied_test -> (send) delete row 1 '~s' from table 2",[Idx3]),
    s([{[testStatus2, Idx3], i, ?destroy}]),
    ?expect1([{[testStatus2, Idx3], ?destroy}]),
    ?DBG("implied_test -> (send) delete row 2 '~s' from table 2",[Idx4]),
    s([{[testStatus2, Idx4], i, ?destroy}]),
    ?expect1([{[testStatus2, Idx4], ?destroy}]),

    snmpa:verbosity(MA, log),

    ?IPRINT("implied_test -> done", []),
    ok.
    
    

%% Req. Test1
sparse_table_test() ->
    ?NPRINT("Testing sparse table..."),

    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    s([{[sparseStatus, Idx1], i, ?createAndGo},
       {[sparseDescr, Idx1], s, "row 1"}]),
    ?expect1([{[sparseStatus, Idx1], ?createAndGo},
		    {[sparseDescr, Idx1], "row 1"}]),
    s([{[sparseStatus, Idx2], i, ?createAndGo},
       {[sparseDescr, Idx2], s, "row 2"}]),
    ?expect1([{[sparseStatus, Idx2], ?createAndGo},
		    {[sparseDescr, Idx2], "row 2"}]),
    ?v1_2(gn([[sparseIndex], [sparseDescr,Idx1], [sparseDescr,Idx2],
	      [sparseStatus,Idx1], [sparseStatus,Idx2]]),
	  gb(0,5,[[sparseIndex]])),
    ?expect1([{[sparseDescr,Idx1], "row 1"},
		    {[sparseDescr,Idx2], "row 2"},
		    {[sparseStatus,Idx1], ?active},
		    {[sparseStatus,Idx2], ?active},
		    {[sparseStr,0], "slut"}]),
    %% Delete the rows
    s([{[sparseStatus, Idx1], i, ?destroy}]),
    ?expect1([{[sparseStatus, Idx1], ?destroy}]),
    s([{[sparseStatus, Idx2], i, ?destroy}]),
    ?expect1([{[sparseStatus, Idx2], ?destroy}]).


%% Req. Test1
cnt_64_test(MA) ->
    ?IPRINT("start cnt64 test (~p)", [MA]),
    snmpa:verbosity(MA, trace),
    ?IPRINT("start cnt64 test"),

    ?NPRINT("Testing Counter64, and at the same time, "
            "RowStatus is not last column"),
    
    ?DBG("get cnt64",[]),
    g([[cnt64,0]]),
    ?DBG("await response",[]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{[cnt64,0],18446744073709551615}])),
    ?DBG("get-next cnt64",[]),
    gn([[cnt64]]),
    ?DBG("await response",[]),
    ?v1_2(?expect1([{[cnt64Str,0], "after cnt64"}]),
		?expect1([{[cnt64,0],18446744073709551615}])),
    ?DBG("send cntTrap",[]),
    snmpa:send_trap(MA,cntTrap,"standard trap",[
						{sysContact,  "pelle"},
						{cnt64,       10},
						{sysLocation, "here"}
					       ]),
    ?DBG("await response",[]),
    ?v1_2(?expect5(trap, [test], 6, 1, [{[sysContact,0], "pelle"},
					      {[sysLocation,0], "here"}]),
		?expect2(v2trap, [{[sysUpTime, 0], any},
				  {[snmpTrapOID, 0], ?testTrap ++ [1]},
				  {[sysContact,0], "pelle"},
				  {[cnt64,0], 10},
				  {[sysLocation,0], "here"}])),
    
    %% Create two rows, check that they are get-nexted in correct order.
    Idx1 = 1,
    Idx2 = 2,
    ?DBG("create row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?expect1([{[cntStatus, Idx1], ?createAndGo}]),
    ?DBG("create row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?createAndGo}]),
    ?DBG("await response",[]),
    ?expect1([{[cntStatus, Idx2], ?createAndGo}]),

    ?DBG("get-next (cntIndex)",[]),
    gn([[cntIndex]]),
    ?DBG("await response",[]),
    ?v1_2(?expect1([{[cntStatus,Idx1], ?active}]),
		?expect1([{[cntCnt,Idx1], 0}])),
    % Delete the rows
    ?DBG("delete row (cntStatus): ~p",[Idx1]),
    s([{[cntStatus, Idx1], i, ?destroy}]),
    ?DBG("await response",[]),
    ?expect1([{[cntStatus, Idx1], ?destroy}]),
    ?DBG("delete row (cntStatus): ~p",[Idx2]),
    s([{[cntStatus, Idx2], i, ?destroy}]),
    ?DBG("await response",[]),
    ?expect1([{[cntStatus, Idx2], ?destroy}]),
    catch snmpa:verbosity(MA, log),
    ?DBG("done",[]),
    ok.

%% Req. Test1
opaque_test() ->
    ?NPRINT("Testing Opaque datatype..."),
    g([[opaqueObj,0]]),
    ?expect1([{[opaqueObj,0], "opaque-data"}]).
    
%% Req. OLD-SNMPEA-MIB
api_test(MaNode) ->
    {value, OID} = rpc:call(MaNode, snmpa, name_to_oid,
				  [intAgentIpAddress]),
    {value, intAgentIpAddress} = rpc:call(MaNode, snmpa,
						oid_to_name, [OID]),
    false = rpc:call(MaNode, snmpa, name_to_oid, [intAgentIpAddres]),
    false = rpc:call(MaNode, snmpa, oid_to_name,
			   [[1,5,32,3,54,3,3,34,4]]),
    {value, 2} = rpc:call(MaNode, snmpa, enum_to_int,
				[intViewType, excluded]),
    {value, excluded} = rpc:call(MaNode, snmpa, int_to_enum,
				       [intViewType, 2]),
    false = rpc:call(MaNode, snmpa, enum_to_int, [intViewType, exclude]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [intAgentIpAddress, exclude]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [intAgentIpAddre, exclude]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [intViewType, 3]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [intAgentIpAddress, 2]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [intAgentIpAddre, 2]),
    {value, active} = rpc:call(MaNode, snmpa,
				     int_to_enum, ['RowStatus', ?active]),
    {value, ?destroy} = rpc:call(MaNode, snmpa,
				       enum_to_int, ['RowStatus', destroy]),
    false = rpc:call(MaNode, snmpa,
			   enum_to_int, ['RowStatus', xxxdestroy]),
    false = rpc:call(MaNode, snmpa,
			   enum_to_int, ['xxRowStatus', destroy]),
    false = rpc:call(MaNode, snmpa, int_to_enum, ['RowStatus', 25]),
    false = rpc:call(MaNode, snmpa, int_to_enum, ['xxRowStatus', 1]),
    case snmp:date_and_time() of
	      List when is_list(List), length(List) == 8 -> ok;
	      List when is_list(List), length(List) == 11 -> ok
    end.

%% Req. Klas3
api_test2() ->
    g([[fname3,0]]),
    ?expect1([{[fname3,0], "ok"}]),
    g([[fname4,0]]),
    ?expect1([{[fname4,0], 1}]).

api_test3() ->
    g([[fname3,0]]),
    ?expect1([{[fname3,0], "ok"}]).
    
    
unreg_test() ->
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[snmpInPkts, 0], any}]).

load_test() ->
    gn([[?v1_2(sysServices, sysORLastChange),0]]),
    ?expect1([{[fname,0], ""}]).

%% Req. Klas1
load_test_sa() ->
    gn([[?v1_2(sysServices,sysORLastChange), 0]]),
    ?expect1([{[fname,0], any}]).
    
%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_get() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?expect1([{[fname,0], "test set"}]),
    g([[sysDescr,0], Key1c4, [fname,0],Key1c3,[sysName,0]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {Key1c4, 2},
		    {[fname,0], "test set"},
		    {Key1c3, 2},
		    {[sysName,0], "test"}]),
    g([[1,3,7,1], Key1c4, [sysDescr,0], [1,3,7,2], Key1c3, [sysDescr,0]]),
    ?v1_2(?expect3(noSuchName, [1,4], any),
		?expect1([{[1,3,7,1], noSuchObject},
			  {Key1c4, 2},
			  {[sysDescr,0], "Erlang SNMP agent"},
			  {[1,3,7,2], noSuchObject},
			  {Key1c3, 2},
			  {[sysDescr,0], "Erlang SNMP agent"}])).

%% Req. v1, system group, Klas1, OLD-SNMPEA-MIB, *ej* Klas3.
do_mul_get_err() ->
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?expect1([{[fname,0], "test set"}]),
    g([[sysDescr,0],Key1c4,[fname,0], Key1c3, [sysName,2]]),
    ?v1_2(?expect3(noSuchName, 5, any),
		?expect1([{[sysDescr,0], "Erlang SNMP agent"},
			  {Key1c4, 2},
			  {[fname,0], "test set"},
			  {Key1c3, 2},
			  {[sysName,2], noSuchInstance}])),
    g([[sysDescr,0],Key1c4,[fname3,0], Key1c3, [sysName,1]]),
    ?v1_2(?expect3(noSuchName, [3,5], any),
		?expect1([{[sysDescr,0], "Erlang SNMP agent"},
			  {Key1c4, 2},
			  {[fname3,0], noSuchObject},
			  {Key1c3, 2},
			  {[sysName,1], noSuchInstance}])).


%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?expect1([{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [fname],Key1c3s,[sysName]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {Key1c4, 2}, {[fname,0], "test set"},
		    {Key1c3, 2}, {[sysName,0], "test"}]).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_next_err() ->
    Key1c3s = [intCommunityEntry,[3],get(mip),is("publi")],
    Key1c4s = [intCommunityEntry,[4],get(mip),is("publi")],
    Key1c3 = [intCommunityEntry,[3],get(mip),is("public")],
    Key1c4 = [intCommunityEntry,[4],get(mip),is("public")],
    s([{[fname,0], s, "test set"}]),
    ?expect1([{[fname,0], "test set"}]),
    gn([[sysDescr], Key1c4s, [1,3,6,999], [fname],[1,3,90], Key1c3s,[sysName]]),
    ?v1_2(?expect3(noSuchName, [3,5], any),
		?expect1([{[sysDescr,0], "Erlang SNMP agent"},
			  {Key1c4, 2},
			  {[1,3,6,999], endOfMibView},
			  {[fname,0], "test set"},
			  {[1,3,90], endOfMibView},
			  {Key1c3, 2},
			  {[sysName,0], "test"}])).


%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set() ->
    ?NPRINT("Adding one row in subagent table, and one in master table"),
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{[friendsEntry, [2, 3]], "kompis3"},
       {NewKeyc3, 2},
       {[sysLocation,0], "new_value"},
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?expect1([{[friendsEntry, [2, 3]], "kompis3"},
		    {NewKeyc3, 2},
		    {[sysLocation,0], "new_value"},
		    {NewKeyc5, ?createAndGo},
		    {NewKeyc4, 2},
		    {[friendsEntry, [3, 3]], ?createAndGo}]),
    g([[friendsEntry, [2, 3]],
       [sysLocation,0],
       [friendsEntry, [3, 3]]]),
    ?expect1([{[friendsEntry, [2, 3]], "kompis3"},
		    {[sysLocation,0], "new_value"},
		    {[friendsEntry, [3, 3]], ?active}]),
    g([NewKeyc4]),
    ?expect1([{NewKeyc4, 2}]),
    s([{[friendsEntry, [3, 3]], ?destroy},
       {NewKeyc5, ?destroy}]),
    ?expect1([{[friendsEntry, [3, 3]], ?destroy},
		    {NewKeyc5, ?destroy}]).

%% Req. system group, Klas1, OLD-SNMPEA-MIB
do_mul_set_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    ?NPRINT("Adding one row in subagent table, and one in master table"),
    s([{[friendsEntry, [2, 3]], s, "kompis3"},
       {NewKeyc3, 2},
       {[sysUpTime,0], 45},   % sysUpTime (readOnly)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2},
       {[friendsEntry, [3, 3]], ?createAndGo}]),
    ?expect3(?v1_2(noSuchName, notWritable), 3, any),
    g([[friendsEntry, [2, 3]]]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{[friendsEntry, [2,3]], noSuchInstance}])),
    g([NewKeyc4]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{NewKeyc4, noSuchInstance}])).

%% Req. SA-MIB
sa_mib() ->
    g([[sa, [2,0]]]),
    ?expect1([{[sa, [2,0]], 3}]),
    s([{[sa, [1,0]], s, "sa_test"}]),
    ?expect1([{[sa, [1,0]], "sa_test"}]),
    ok.

ma_trap1(MA) ->
    ok = snmpa:send_trap(MA, testTrap2, "standard trap"),
    ?expect5(trap, [system], 6, 1, [{[system, [4,0]],
					   "{mbj,eklas}@erlang.ericsson.se"}]),
    ok = snmpa:send_trap(MA, testTrap1, "standard trap"),
    ?expect5(trap, [1,2,3] , 1, 0, [{[system, [4,0]],
					   "{mbj,eklas}@erlang.ericsson.se"}]),
    ok.

ma_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?expect5(trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]),
    ok.

ma_v2_2_v1_trap(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?expect5(trap, [system], 6, 1, [{[system, [4,0]], "pelle"}]),
    ok.    

ma_v2_2_v1_trap2(MA) ->
    snmpa:send_trap(MA,linkUp,"standard trap",[{ifIndex, [1], 1},
					      {ifAdminStatus, [1], 1},
					      {ifOperStatus, [1], 2}]),
    ?expect5(trap, [1,2,3], 3, 0, [{[ifIndex, 1], 1},
					 {[ifAdminStatus, 1], 1},
					 {[ifOperStatus, 1], 2}]),
    ok.

sa_trap1(SA) ->
    %% io:format("sa_trap1 -> entry with"
    %% 	      "~n   SA:       ~p"
    %% 	      "~n   node(SA): ~p"
    %% 	      "~n   self():   ~p"
    %% 	      "~n   node():   ~p"
    %% 	      "~n", [SA, node(SA), self(), node()]),
    _VRes  = (catch snmpa:verbosity(SA, {subagents, trace})),
    %% io:format("sa_trap1 -> SA verbosity set: "
    %% 	      "~n   VRes: ~p"
    %% 	      "~n", [VRes]),
    _TSRes = (catch snmpa:send_trap(SA, saTrap, "standard trap")),
    %% io:format("sa_trap1 -> SA trap send: "
    %% 	      "~n   TSRes: ~p"
    %% 	      "~n", [TSRes]),
    ?expect5(trap, [ericsson], 6, 1, [{[system, [4,0]],
					     "{mbj,eklas}@erlang.ericsson.se"},
					    {[sa, [1,0]], "sa_test"}]),
    snmpa:verbosity(SA, {subagents, silence}),
    ok.

sa_trap2(SA) ->
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?expect5(trap, [ericsson], 6, 1, [{[system, [4,0]], "pelle"},
					    {[sa, [1,0]], "sa_test"}]),
    ok.

sa_trap3(SA) ->
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?expect5(trap, [ericsson], 6, 2, [{[system, [4,0]],
					     "{mbj,eklas}@erlang.ericsson.se"},
					    {[sa, [1,0]], "sa_test"},
					    {[intViewSubtree,4],[1,2,3,4]}]),
    ok.

ma_v2_trap1(MA) ->
    ?DBG("ma_v2_traps -> entry with MA = ~p => "
	   "send standard trap: testTrapv22",[MA]),
    snmpa:send_trap(MA, testTrapv22, "standard trap"),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?system ++ [0,1]}]),
    ?DBG("ma_v2_traps -> send standard trap: testTrapv21",[]),
    snmpa:send_trap(MA, testTrapv21, "standard trap"),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?snmp ++ [1]}]),
    ok.

ma_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrapv22,"standard trap",[{sysContact,"pelle"}]),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
                      {[snmpTrapOID, 0], ?system ++ [0,1]},
                      {[system, [4,0]], "pelle"}]).

%% Note: This test case takes a while... actually a couple of minutes.
ma_v2_inform1(MA) ->
    ?IPRINT("begin ma_v2_inform1 (MA: ~p)", [MA]), 

    CmdExpectInform = 
	fun(_No, Response) ->
		?expect2({inform, Response},
			 [{[sysUpTime, 0], any}, 
			  {[snmpTrapOID, 0], ?system ++ [0,1]}])
	end,

    CmdExp = 
	fun(ok) -> 
		ok;
	   ({ok, _Val}) ->
		?DBG("ma_v2_inform -> [cmd2] Val: ~p", [_Val]),
		ok;
	   ({error, Id, Extra}) ->
		{error, {unexpected, Id, Extra}};
	   (Error) ->
		Error
	end,

    Cmd01 = 
	fun() -> 
		snmpa:send_notification(MA, testTrapv22, no_receiver, 
					"standard inform", []),
		ok
	end,
    Cmd02 = 
	fun() ->
		Res = CmdExpectInform(1, true),
		CmdExp(Res)
	end,

    Tag03 = tag11, 
    Cmd03 = 
	fun() ->
		snmpa:send_notification(MA, testTrapv22, {Tag03, self()},
					"standard inform", []),
		ok
	end,
    Cmd04 = 
	fun() ->
		Res = CmdExpectInform(2, true),
		CmdExp(Res)
	end,
    CmdSnmpTargets = 
	fun(T) ->
		receive
		    {snmp_targets, T, [_Addr]} ->
			?DBG("ma_v2_inform1 -> "
			     "received expected snmp_targets "
			     "~n   with receiver: ~p", [_Addr]),
			ok;
		    {snmp_targets, T, Addrs} ->
			?EPRINT("ma_v2_inform1 -> "
                                "received unexpected snmp_targets"
                                "~n   with receivers: ~n   ~p",[Addrs]),
			{error, {bad_addrs, Addrs}}
		after
		    5000 ->
			?EPRINT("ma_v2_inform1 -> "
                                "timeout awaiting snmp_targets [~w]",[T]),
			{error, snmp_targets_timeout}
		end
	end,
    Cmd05 = fun() -> CmdSnmpTargets(Tag03) end,
    Cmd06 = 
	fun() ->
		receive
		    {snmp_notification, Tag03, {got_response, _Addr}} ->
			?DBG("ma_v2_inform1 -> "
			     "received expected snmp_notification "
			     "[with manager response] from: ~n   ~p", [_Addr]),
			ok;
		    {snmp_notification, Tag03, {no_response, _Addr}} ->
			?EPRINT("ma_v2_inform1 -> "
                                "received unexpected snmp_notification "
                                "[without manager response] from: ~n   ~p",
                                [_Addr]),
			{error, no_response}
		after
		    20000 ->
			?EPRINT("ma_v2_inform1 -> "
                                "timeout awaiting snmp_notification [~p]: "
                                "~n   Message Queue: "
                                "~n      ~p",
                                [Tag03,
                                 process_info(self(), messages)]),
			{error, snmp_notification_timeout}
		end
	end,

    Tag07 = tag12,
    Cmd07 = 
	fun() ->
		snmpa:send_notification(MA, testTrapv22, {Tag07, self()},
					"standard inform", []),
		ok
	end,
    Cmd08 = 
	fun() ->
		Res = CmdExpectInform(3, false),
		CmdExp(Res)
	end,
    Cmd09 = 
	fun() -> 
		CmdSnmpTargets(Tag07) 
	end,
    Cmd10 = 
	fun() ->
		receive
		    {snmp_notification, Tag07, {got_response, _Addr}} ->
			?EPRINT("ma_v2_inform1 -> "
                                "received unexpected snmp_notification "
                                "[with manager response] from: ~n   ~p", [_Addr]),
			{error, got_response};
		    {snmp_notification, Tag07, {no_response, _Addr}} ->
			?DBG("ma_v2_inform1 -> "
			     "received expected snmp_notification "
			     "[without manager response] from: ~n   ~p",
			     [_Addr]),
			ok
		after
		    240000 ->
			?EPRINT("ma_v2_inform1 -> "
                                "timeout awaiting snmp_notification [~p]: "
                                "~n   Message Queue: "
                                "~n      ~p",
                                [Tag07,
                                 process_info(self(), messages)]),
			{error, snmp_notification_timeout}
		end
	end,

    Commands = 
	[
	 { 1, "Send notification [no receiver]", Cmd01},
	 { 2, "Expect inform [send response]",   Cmd02},
	 { 3, "Send notification [tag1]",        Cmd03},
	 { 4, "Expect inform [send response]",   Cmd04},
	 { 5, "Expect snmp_targets message [from trap sender]", Cmd05},
	 { 6, "Expect snmp_notification [got_response] message [from trap sender]", Cmd06},
	 { 7, "Send notification [tag2]",        Cmd07},
	 { 8, "Expect inform [don't send response]", Cmd08},
	 { 9, "Expect snmp_targets message [from trap sender]", Cmd09},
	 {10, "Expect snmp_notification [no_response] message [from trap sender]",  Cmd10}
	],

    command_handler(Commands).
		   
    
%% Note:  This test case takes a while... actually a couple of minutes.
ma_v2_inform2(MA) ->
    ?IPRINT("begin ma_v2_inform2 (MA: ~p)", [MA]), 

    CmdExpectInform = 
	fun(_No, Response) ->
		?expect2({inform, Response},
			 [{[sysUpTime, 0], any}, 
			  {[snmpTrapOID, 0], ?system ++ [0,1]}])
	end,

    CmdExp = 
	fun(ok) -> 
		ok;
	   ({ok, _Val}) ->
		?DBG("ma_v2_inform -> [cmd2] Val: ~p", [_Val]),
		ok;
	   ({error, Id, Extra}) ->
		{error, {unexpected, Id, Extra}};
	   (Error) ->
		Error
	end,

    %% Await callback(s)
    CmdAwaitDeliveryCallback = 
	fun(Kind, Ref, Tag) ->
		?IPRINT("ma_v2_inform2:"
                        "CmdAwaitDeliveryCallback -> entry with"
                        "~n   Kind: ~p"
                        "~n   Ref:  ~p"
                        "~n   Tag:  ~p", [Kind, Ref, Tag]),
		receive
		    {Kind, Ref, ok} ->
			?IPRINT("ma_v2_inform2:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "received expected result: ok"
                                "~n", [Tag, Ref]),
			ok;
		    {Kind, Ref, Error} ->
			?IPRINT("ma_v2_inform2:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "received unexpected result: "
                                "~n   Error: ~p"
                                "~n", [Tag, Ref, Error]),
			{error, {unexpected_response, Error}}
		after
		    240000 ->
			?EPRINT("ma_v2_inform2:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "timeout awaiting got_response for "
                                "snmp_notification [~p]: "
                                "~n   Message Queue: "
                                "~n      ~p",
                                [Tag, Ref, Tag,
                                 process_info(self(), messages)]),
			{error, snmp_notification_timeout}
		end
	end,
	
    Tag11   = tag21, 
    Ref11   = make_ref(), 
    Extra11 = [{tag,      Tag11}, 
	       {ref,      Ref11}, 
	       {recv,     self()}, 
	       {targets,  []}, 
	       {address,  dummy}, 
	       {expected_delivery_result, got_response}], 
    Recv11  = #snmpa_notification_delivery_info{tag   = Tag11,
						mod   = ?MODULE,
						extra = Extra11},
    Cmd11 = 
	fun() ->
		snmpa:send_notification(MA, testTrapv22, 
					Recv11,
					"standard inform", []),
		ok
	end,
    Cmd12 = 
	fun() ->
		Res = CmdExpectInform(4, true),
		CmdExp(Res)
	end,

    Cmd13 = fun() -> CmdAwaitDeliveryCallback(targets, Ref11, Tag11) end,
    Cmd14 = fun() -> CmdAwaitDeliveryCallback(info,    Ref11, Tag11) end,
			
    Commands = 
	[
	 {11, "Send notification [tag3]",                    Cmd11},
	 {12, "Expect notification message [tag3]",          Cmd12}, 
	 {13, "Expect targets message [tag3]",               Cmd13}, 
	 {14, "Expect notification response message [tag3]", Cmd14}
	],

    command_handler(Commands).
		   
    
%% Note:  This test case takes a while... actually a couple of minutes.
ma_v2_inform3(MA) ->
    ?IPRINT("begin ma_v2_inform3 (MA: ~p)", [MA]), 

    CmdExpectInform = 
	fun(_No, Response) ->
		?DBG("ma_v2_inform3:CmdExpectInform -> ~p: "
                     "~n      ~p", [_No, Response]),
		?expect2({inform, Response},
			 [{[sysUpTime, 0], any}, 
			  {[snmpTrapOID, 0], ?system ++ [0,1]}])
	end,

    CmdExp = 
	fun(ok) -> 
		ok;
	   ({ok, _Val}) ->
		?DBG("ma_v2_inform3:CmdExp -> Val: ~p", [_Val]),
		ok;
	   ({error, Id, Extra}) ->
		{error, {unexpected, Id, Extra}};
	   (Error) ->
		Error
	end,

    %% Await callback(s)
    CmdAwaitDeliveryCallback = 
	fun(Kind, Ref, Tag) ->
		?IPRINT("ma_v2_inform3:"
                        "CmdAwaitDeliveryCallback -> entry with"
                        "~n   Kind: ~p"
                        "~n   Ref:  ~p"
                        "~n   Tag:  ~p", [Kind, Ref, Tag]),
		receive
		    {Kind, Ref, ok} ->
			?IPRINT("ma_v2_inform3:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "received expected result: ok"
                                "~n", [Tag, Ref]),
			ok;
		    {Kind, Ref, Error} ->
			?IPRINT("ma_v2_inform3:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "received unexpected result: "
                                "~n   Error: ~p"
                                "~n", [Tag, Ref, Error]),
			{error, {unexpected_response, Error}}
		after
		    240000 ->
			?EPRINT("ma_v2_inform3:"
                                "CmdAwaitDeliveryCallback(~p,~p) -> "
                                "timeout awaiting got_response for "
                                "snmp_notification [~p]: "
                                "~n   Message Queue: "
                                "~n      ~p",
                                [Kind, Ref, Tag,
                                 process_info(self(), messages)]),
			{error, snmp_notification_timeout}
		end
	end,
	
    Tag15   = tag31, 
    Ref15   = make_ref(), 
    Extra15 = [{tag,      Tag15}, 
	       {ref,      Ref15}, 
	       {recv,     self()}, 
	       {targets,  []}, 
	       {address,  dummy}, 
	       {expected_delivery_result, no_response}], 
    Recv15  = #snmpa_notification_delivery_info{tag   = Tag15,
						mod   = ?MODULE,
						extra = Extra15},
    Cmd15 = 
	fun() ->
		snmpa:send_notification(MA, testTrapv22, 
					Recv15,
					"standard inform", []),
		ok
	end,
    Cmd16 = 
	fun() ->
		Res = CmdExpectInform(5, false),
		CmdExp(Res)
	end,
    
    Cmd17 = fun() -> CmdAwaitDeliveryCallback(targets, Ref15, Tag15) end,
    Cmd18 = fun() -> CmdAwaitDeliveryCallback(info,    Ref15, Tag15) end,
    
    Commands = 
	[
	 {15, "Send notification [" ++ atom_to_list(Tag15) ++ "]", Cmd15},
	 {16, "Expect notification message [" ++ atom_to_list(Tag15) ++ "]", Cmd16}, 
	 {17, "Expect targets message [" ++ atom_to_list(Tag15) ++ "]", Cmd17}, 
	 {18, "Expect notification (no) response message [" ++ atom_to_list(Tag15) ++ "]", Cmd18}
	],

    command_handler(Commands).
		   

%% snmpa_notification_delivery_info_receiver callback function
delivery_targets(Tag, Addresses, Extra) ->
    io:format("~w:delivery_targets -> entry with"
	      "~n   Tag:       ~p"
	      "~n   Addresses: ~p"
	      "~n   Extra:     ~p"
	      "~n", [?MODULE, Tag, Addresses, Extra]),
    {value, {_, Pid}} = lists:keysearch(recv, 1, Extra),
    {value, {_, Ref}} = lists:keysearch(ref,  1, Extra),
    case lists:keysearch(tag,  1, Extra) of
	{value, {_, Tag}} ->
	    Pid ! {targets, Ref, ok};
	{value, {_, OtherTag}} ->
	    Pid ! {targets, Ref, {error, {wrong_tag, Tag, OtherTag}}}
    end,
    ok.

%% snmpa_notification_delivery_info_receiver callback function
delivery_info(Tag, Address, DeliveryResult, Extra) ->
    io:format("~w:delivery_info -> entry with"
	      "~n   Tag:            ~p"
	      "~n   Address:        ~p"
	      "~n   DeliveryResult: ~p"
	      "~n   Extra:          ~p"
	      "~n", [?MODULE, Tag, Address, DeliveryResult, Extra]),
    {value, {_, Pid}} = lists:keysearch(recv, 1, Extra),
    {value, {_, Ref}} = lists:keysearch(ref,  1, Extra),
    case lists:keysearch(tag,  1, Extra) of
	{value, {_, Tag}} ->
	    Pid ! {info, Ref, ok};
	{value, {_, OtherTag}} ->
	    Pid ! {info, Ref, {error, {wrong_tag, Tag, OtherTag}}}
    end,
    ok.


command_handler([]) ->    
    ok;
command_handler([{_No, _Desc, Cmd}|Rest]) ->
    ?IPRINT("command_handler -> command ~w: ~n   ~s", [_No, _Desc]),
    %% case (catch Cmd()) of
    %%     ok ->
    %%         ?IPRINT("command_handler -> ~w: ok", [_No]),
    %%         command_handler(Rest);
    %%     {error, Reason} ->
    %%         ?EPRINT("command_handler -> ~w error: ~n~p", [_No, Reason]),
    %%         ?FAIL(Reason);
    %%     Error ->
    %%         ?EPRINT("command_handler -> ~w unexpected: ~n~p", [_No, Error]),
    %%         ?FAIL({unexpected_command_result, Error})
    %% end.
    try Cmd() of
        ok ->
            ?IPRINT("command_handler -> ~w: ok", [_No]),
            command_handler(Rest);
        {error, Reason} ->
            ?IPRINT("command_handler -> command ~w error", [_No]),
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("command_handler -> ~w error: ~n~p", [_No, Reason]),
                    ?FAIL(Reason);
                true ->
                    ?WPRINT("command_handler -> "
                            "failed when we got system events: "
                            "~n   Reason:     ~p"
                            "~n   Sys Events: ~p"
                            "~n", [Reason, SysEvs]),
                    ?SKIP([{reason, Reason}, {system_events, SysEvs}])
            end;
        Error ->
            ?IPRINT("command_handler -> command ~w unexpected", [_No]),
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("command_handler -> "
                            "~w unexpected: ~n~p", [_No, Error]),
                    ?FAIL({unexpected_command_result, Error});
                true ->
                    ?WPRINT("command_handler -> "
                            "unexpected when we got system events: "
                            "~n   Unexpected: ~p"
                            "~n   Sys Events: ~p"
                            "~n", [Error, SysEvs]),
                    ?SKIP([{unexpected, Error}, {system_events, SysEvs}])
            end
    catch
        C:E:S ->
            ?IPRINT("command_handler -> command ~w caught", [_No]),
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("command_handler -> ~w caught: "
                            "~n   Class: ~p"
                            "~n   Error: ~p"
                            "~n   Stack: ~p", [_No, C, E, S]),
                    ?FAIL({catched_command_result, {C, E, S}});
                true ->
                    ?WPRINT("command_handler -> "
                            "caught when we got system events: "
                            "~n   Caught: "
                            "~n      Class:   ~p"
                            "~n      Error:   ~p"
                            "~n      Stack:   ~p"
                            "~n   Sys Events: ~p"
                            "~n", [C, E, S, SysEvs]),
                    ?SKIP([{caught, {C, E, S}}, {system_events, SysEvs}])
            end
    end.
    

ma_v1_2_v2_trap(MA) ->
    snmpa:send_trap(MA,linkDown,"standard trap",[{ifIndex, [1], 1}]),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?snmpTraps ++ [3]},
			    {[ifIndex, 1], 1},
			    {[snmpTrapEnterprise, 0], [1,2,3]}]).

    
ma_v1_2_v2_trap2(MA) ->
    snmpa:send_trap(MA,testTrap2,"standard trap",[{sysContact,"pelle"}]),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?system ++ [0,1]},
			    {[system, [4,0]], "pelle"},
			    {[snmpTrapEnterprise, 0], ?system}]).
    

sa_v1_2_v2_trap1(SA) ->
    snmpa:verbosity(SA, {subagents, trace}),
    snmpa:send_trap(SA, saTrap, "standard trap"),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			    {[system, [4,0]],
			     "{mbj,eklas}@erlang.ericsson.se"},
			    {[sa, [1,0]], "sa_test"},
			    {[snmpTrapEnterprise, 0], ?ericsson}]),
    snmpa:verbosity(SA, {subagents, silence}),
    ok.

sa_v1_2_v2_trap2(SA) ->
    snmpa:verbosity(SA, {subagents, trace}),
    snmpa:send_trap(SA, saTrap, "standard trap",[{sysContact,"pelle"}]),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?ericsson ++ [0, 1]},
			    {[system, [4,0]], "pelle"},
			    {[sa, [1,0]], "sa_test"},
			    {[snmpTrapEnterprise, 0], ?ericsson}]),
    snmpa:verbosity(SA, {subagents, silence}),
    ok.


sa_v1_2_v2_trap3(SA) ->
    snmpa:verbosity(SA, {subagents, trace}),
    snmpa:send_trap(SA, saTrap2, "standard trap",
			 [{intViewSubtree, [4], [1,2,3,4]}]),
    ?expect2(v2trap, [{[sysUpTime, 0], any},
			    {[snmpTrapOID, 0], ?ericsson ++ [0, 2]},
			    {[system, [4,0]],
			     "{mbj,eklas}@erlang.ericsson.se"},
			    {[sa, [1,0]], "sa_test"},
			    {[intViewSubtree,4],[1,2,3,4]},
			    {[snmpTrapEnterprise, 0], ?ericsson}]),
    snmpa:verbosity(SA, {subagents, silence}),
    ok.
			     

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_bad_value() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 5}, % badValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?expect3(badValue, 2, any),
    s([{NewKeyc3, 2},
       {[sa, [2,0]], 6}, % wrongValue (i is_set_ok)
       {NewKeyc5, ?createAndGo},
       {NewKeyc4, 2}]),
    ?expect3(?v1_2(badValue, wrongValue), 2, any),
    g([NewKeyc4]),
    ?v1_2(?expect3(noSuchName, 1, any),
		?expect1([{NewKeyc4, noSuchInstance}])).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_errs_gen_err() ->
    NewKeyc3 = [intCommunityEntry,[3],get(mip),is("test")],
    NewKeyc4 = [intCommunityEntry,[4],get(mip),is("test")],
    NewKeyc5 = [intCommunityEntry,[5],get(mip),is("test")],
    s([{NewKeyc3, 2},{NewKeyc4, 2},
       {NewKeyc5, ?createAndGo}, {[sa, [3,0]], 5}]),
    ?expect3(genErr, 4, any),
% The row might have been added; we don't know.
% (as a matter of fact we do - it is added, because the agent
% first sets its own vars, and then th SAs. Lets destroy it.
    s([{NewKeyc5, ?destroy}]),
    ?expect1([{NewKeyc5, ?destroy}]).

%% Req. SA-MIB, OLD-SNMPEA-MIB
sa_too_big() ->
    g([[sa, [4,0]]]),
    ?expect1(tooBig).

%% Req. Klas1, system group, snmp group (v1/v2)
next_across_sa_test() ->
    gn([[sysDescr],[klas1,5]]),
    ?expect1([{[sysDescr,0], "Erlang SNMP agent"},
		    {[snmpInPkts, 0], any}]).

%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]). -> noError
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]). -> {badValue, 2}
%% snmp_test_mgr:s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]). -> {genErr, 1}
%% snmp_test_mgr:s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]). -> {genErr, 2}
%% Req. Klas3, Klas4
undo_test() ->
    s([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    ?expect1([{[fStatus3, 1], 4}, {[fname3,0], "ok"}]),
    s([{[fStatus3, 1], 4}, {[fname3,0], "hoj"}]),
    ?expect3(?v1_2(badValue, inconsistentValue), 2, any),
    s([{[fStatus3, 3], 4}, {[fname3,0], "hoj"}]),
    ?expect3(?v1_2(genErr, undoFailed), 1, any),
    s([{[fStatus3, 4], 4}, {[fname3,0], "ok"}]),
    ?expect3(?v1_2(genErr, commitFailed), 1, any),
% unfortunately we don't know if we'll get undoFailed or commitFailed.
% it depends on which order the agent traverses the varbind list.
%    s([{[fStatus3, 4], 4}, {[fname3,0], "ufail"}]),
%    expect(5, ?v1_2(genErr, undoFailed), 1, any),
    s([{[fStatus3, 1], 4}, {[fname3,0], "xfail"}]),
    ?expect3(genErr, 2, any).
    
%% Req. Klas3, Klas4
bad_return() ->
    g([[fStatus4,4],
       [fName4,4]]),
    ?expect3(genErr, 2, any),
    g([[fStatus4,5],
       [fName4,5]]),
    ?expect3(genErr, 1, any),
    g([[fStatus4,6],
       [fName4,6]]),
    ?expect3(genErr, 2, any),
    gn([[fStatus4,7],
       [fName4,7]]),
    ?expect3(genErr, 2, any),
    gn([[fStatus4,8],
       [fName4,8]]),
    ?expect3(genErr, 1, any),
    gn([[fStatus4,9],
       [fName4,9]]),
    ?expect3(genErr, 2, any).


%%%-----------------------------------------------------------------
%%% Test the implementation of standard mibs.
%%% We should *at least* try to GET all variables, just to make
%%% sure the instrumentation functions work.
%%% Note that many of the functions in the standard mib is
%%% already tested by the normal tests.
%%%-----------------------------------------------------------------

standard_mibs_cases() ->
    [
     snmp_standard_mib, 
     snmp_community_mib,
     snmp_framework_mib, 
     snmp_target_mib,
     snmp_notification_mib, 
     snmp_view_based_acm_mib
    ].
    
standard_mibs_cases_ipv6() ->
    [
     %% snmp_standard_mib, % Sending v1 traps does not work over IPv6
     snmp_community_mib,
     snmp_framework_mib,
     snmp_target_mib,
     snmp_notification_mib,
     snmp_view_based_acm_mib
    ].

%%-----------------------------------------------------------------
%% For this test, the agent is configured for v1.
%% o  Test the counters and control objects in SNMP-STANDARD-MIB
%%-----------------------------------------------------------------
snmp_standard_mib(Config) when is_list(Config) ->
    ?P(snmp_standard_mib), 
    init_case(Config),
    ?DBG("snmp_standard_mib -> std_mib_init", []),
    try_test(std_mib_init),

    ?DBG("snmp_standard_mib -> std_mib_a", []),
    InBadVsns = try_test(std_mib_a),
    put(vsn, v2),

    ?DBG("snmp_standard_mib -> std_mib_read", []),
    try_test(std_mib_read),
    put(vsn, v1),

    ?DBG("snmp_standard_mib -> std_mib_b (~w)", [InBadVsns]),
    Bad = try_test(std_mib_b, [InBadVsns]),

    ?DBG("snmp_standard_mib -> std_mib_read (community: 'bad community')", []),
    try_test(std_mib_read, [], [{community, "bad community"}]),

    ?DBG("snmp_standard_mib -> std_mib_write (community: 'public')", []),
    try_test(std_mib_write, [], [{community, "public"}]),

    ?DBG("snmp_standard_mib -> std_mib_asn_err", []),
    try_test(std_mib_asn_err),

    ?DBG("snmp_standard_mib -> std_mib_c (~w)", [Bad]),
    try_test(std_mib_c, [Bad]),

    ?DBG("snmp_standard_mib -> std_mib_a", []),
    try_test(standard_mib_a),
    
    ?DBG("snmp_standard_mib -> std_mib_finish", []),
    try_test(std_mib_finish),

    ?DBG("snmp_standard_mib -> std_mib_test_finish", []),
    try_test(standard_mib_test_finish, [], [{community, "bad community"}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_a() ->
    [OutPkts]  = get_req(2, [[snmpOutPkts,0]]),
    [OutPkts2] = get_req(3, [[snmpOutPkts,0]]),
    OutPkts2   = OutPkts + 1,
    %% There are some more counters we could test here, but it's not that
    %% important, since they are removed from SNMPv2-MIB.
    ok.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_init() ->
    %% disable authentication failure traps.  (otherwise w'd get many of
    %% them - this is also a test to see that it works).
    s([{[snmpEnableAuthenTraps,0], 2}]),
    ?expect1([{[snmpEnableAuthenTraps, 0], 2}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_finish() ->
    %% enable again
    s([{[snmpEnableAuthenTraps,0], 1}]),
    ?expect1([{[snmpEnableAuthenTraps, 0], 1}]).

%% Req. SNMP-STANDARD-MIB
standard_mib_test_finish() ->
    %% force a authenticationFailure (should result in a trap)
    std_mib_write(),
    %% check that we got a trap
    ?expect5(trap, [1,2,3], 4, 0, []).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_read() ->
    ?DBG("std_mib_read -> entry", []),
    g([[sysUpTime,0]]), % try a bad <something>; msg dropped, no reply
    ?DBG("std_mib_read -> await timeout (i.e. no reply)", []),
    ?expect1(timeout). % make sure we don't get a trap!


%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_write() ->
    ?DBG("std_mib_write -> entry", []),
    s([{[sysLocation, 0], "new_value"}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_asn_err() ->
    snmp_test_mgr:send_bytes([48,99,67,12,0,0,0,0,0,0,5]).


standard_mibs2_cases() ->
    [
     snmpv2_mib_2, 
     snmp_community_mib_2,
     snmp_framework_mib_2, 
     snmp_target_mib_2,
     snmp_notification_mib_2, 
     snmp_view_based_acm_mib_2
    ].
    

%%-----------------------------------------------------------------
%% For this test, the agent is configured for v2 and v3.
%% o  Test the counters and control objects in SNMPv2-MIB
%%-----------------------------------------------------------------
snmpv2_mib_2(Config) when is_list(Config) ->
    ?P(snmpv2_mib_2), 
    ?IPRINT("snmpv2_mib_2 -> start"),
    init_case(Config),

    ?DBG("snmpv2_mib_2 -> standard mib init",[]),
    try_test(std_mib_init),

    ?DBG("snmpv2_mib_2 -> get number of (so far) bad versions",[]),
    InBadVsns = try_test(std_mib_a),

    ?DBG("snmpv2_mib_2 -> make a bad version read",[]),
    put(vsn, v1),
    try_test(std_mib_read),

    ?DBG("snmpv2_mib_2 -> bad version read",[]),
    put(vsn, v2),
    Bad = try_test(std_mib_b, [InBadVsns]),

    ?DBG("snmpv2_mib_2 -> read with bad community",[]),
    try_test(std_mib_read, [], [{community, "bad community"}]),

    ?DBG("snmpv2_mib_2 -> write with public community",[]),
    try_test(std_mib_write, [], [{community, "public"}]),

    ?DBG("snmpv2_mib_2 -> asn err", []),
    try_test(std_mib_asn_err),

    ?DBG("snmpv2_mib_2 -> check counters",[]),
    try_test(std_mib_c, [Bad]),

    ?DBG("snmpv2_mib_2 -> get some counters",[]),
    try_test(snmpv2_mib_a),
    
    ?DBG("snmpv2_mib_2 -> enable auth traps, and await some",[]),
    try_test(std_mib_finish),

    ?DBG("snmpv2_mib_2 -> force auth failure, and await trap, "
         "then disable auth traps", []),
    try_test(snmpv2_mib_test_finish, [], [{community, "bad community"}]),
    
    ?IPRINT("snmpv2_mib_2 -> done"),
    ok.
    

standard_mibs3_cases() ->
    [
     snmpv2_mib_3, 
     snmp_framework_mib_3, 
     snmp_mpd_mib_3,
     snmp_target_mib_3, 
     snmp_notification_mib_3,
     snmp_view_based_acm_mib_3, 
     snmp_user_based_sm_mib_3
    ].

    
%% Req. SNMPv2-MIB
snmpv2_mib_3(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(snmpv2_mib_3), 
    init_case(Config),

    InBadVsns = try_test(std_mib_a),
    put(vsn, v1),
    try_test(std_mib_read),
    put(vsn, v3),
    _Bad = try_test(std_mib_b, [InBadVsns]),
    try_test(snmpv2_mib_a),

    try_test(std_mib_finish).
    
-define(authenticationFailure, [1,3,6,1,6,3,1,1,5,5]).

%% Req. SNMPv2-MIB
snmpv2_mib_test_finish() ->
    %% force a authenticationFailure
    ?DBG("ma_v2_inform -> write to std mib",[]),
    std_mib_write(),

    %% check that we got a trap
    ?DBG("ma_v2_inform -> await trap",[]),
    ?expect2(v2trap, [{[sysUpTime,0], any},
			    {[snmpTrapOID,0], ?authenticationFailure}]),

    %% and the the inform
    ?DBG("ma_v2_inform -> await inform",[]),
    ?expect2({inform,true}, [{[sysUpTime,0], any},
				   {[snmpTrapOID,0],?authenticationFailure}]).

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_a() ->
    [InPkts] = get_req(2, [[snmpInPkts,0]]),
    [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    InPkts2 = InPkts + 1,

    [InBadVsns] = get_req(4, [[snmpInBadVersions,0]]),
    InBadVsns.

%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_b(InBadVsns) ->
    [InBadVsns2] = get_req(1, [[snmpInBadVersions,0]]),
    InBadVsns2 = InBadVsns + 1,
    [InPkts] = get_req(2, [[snmpInPkts,0]]),
    [InPkts2] = get_req(3, [[snmpInPkts,0]]),
    InPkts2 = InPkts + 1,
    [InBadCommunityNames, InBadCommunityUses, InASNErrs] =
	get_req(4, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    {InBadCommunityNames, InBadCommunityUses, InASNErrs}.
    
%% Req. SNMP-STANDARD-MIB | SNMPv2-MIB
std_mib_c({InBadCommunityNames, InBadCommunityUses, InASNErrs}) ->
    [InBadCommunityNames2, InBadCommunityUses2, InASNErrs2] =
	get_req(1, [[snmpInBadCommunityNames,0],
		    [snmpInBadCommunityUses,0],
		    [snmpInASNParseErrs, 0]]),
    InBadCommunityNames2 = InBadCommunityNames + 1,
    InBadCommunityUses2 = InBadCommunityUses + 1,
    InASNErrs2 = InASNErrs + 1.

%% Req. SNMPv2-MIB
snmpv2_mib_a() ->
    [SetSerial] = get_req(2, [[snmpSetSerialNo,0]]),
    s([{[snmpSetSerialNo,0], SetSerial}, {[sysLocation, 0], "val2"}]),
    ?expect1([{[snmpSetSerialNo,0], SetSerial},
		    {[sysLocation, 0], "val2"}]),
    s([{[sysLocation, 0], "val3"}, {[snmpSetSerialNo,0], SetSerial}]),
    ?expect3(inconsistentValue, 2,
		   [{[sysLocation, 0], "val3"},
		    {[snmpSetSerialNo,0], SetSerial}]),
    ["val2"] = get_req(5, [[sysLocation,0]]),
    ok.
    
    
%%-----------------------------------------------------------------
%% o  Bad community uses/name is tested already
%%    in SNMPv2-MIB and STANDARD-MIB.
%% o  Test add/deletion of rows.
%%-----------------------------------------------------------------
snmp_community_mib(Config) when is_list(Config) ->
    ?P(snmp_community_mib), 
    init_case(Config),
    load_master_std("SNMP-COMMUNITY-MIB"),
    try_test(snmp_community_mib_test),
    unload_master("SNMP-COMMUNITY-MIB").

snmp_community_mib_2(X) -> ?P(snmp_community_mib_2), snmp_community_mib(X).

%% Req. SNMP-COMMUNITY-MIB
snmp_community_mib_test() ->
    ?NPRINT("NOT YET IMPLEMENTED"),
    nyi.


%%-----------------------------------------------------------------
%% o  Test engine boots / time
%%-----------------------------------------------------------------
snmp_framework_mib(Config) when is_list(Config) ->
    ?P(snmp_framework_mib), 
    init_case(Config),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    try_test(snmp_framework_mib_test),
    unload_master("SNMP-FRAMEWORK-MIB").

snmp_framework_mib_2(X) -> ?P(snmp_framework_mib_2), snmp_framework_mib(X).

snmp_framework_mib_3(Config) when is_list(Config) ->
    ?P(snmp_framework_mib_3), 
    init_case(Config),
    try_test(snmp_framework_mib_test).


%% Req. SNMP-FRAMEWORK-MIB
%% snmpEngineTime (in number of seconds).
%% In theory, the Engine Time diff of the engine, should be exactly
%% the same as the number of seconds we sleep (5 in this case).
%% But because, on some (slow or/and high loaded) hosts, the actual
%% time we sleep could be a lot larger (due to, for instance, scheduling).
%% Therefore we must take that into account when we check if the 
%% Engine Time diff (between the two checks) is acceptably.
snmp_framework_mib_test() ->

    ?IPRINT("transports: "
            "~n      ~p"
            "~ninfo: "
            "~n      ~p",
            [
             rpc:call(get(master_node), snmpa, which_transports, []),
             rpc:call(get(master_node), snmpa, info, [])
            ]),

    Sleep = 5,
    ["agentEngine"] = get_req(1, [[snmpEngineID,0]]),
    T1 = snmp_misc:now(ms),
    [EngineTime] = get_req(2, [[snmpEngineTime,0]]),
    T2 = snmp_misc:now(ms),
    ?SLEEP(?SECS(Sleep)),
    T3 = snmp_misc:now(ms),
    [EngineTime2] = get_req(3, [[snmpEngineTime,0]]),
    T4 = snmp_misc:now(ms),

    %% Ok, we tried to sleep 5 seconds, but how long did we actually sleep
    ASleep         = ((T3-T2) + 500) div 1000,
    EngineTimeDiff = EngineTime2 - EngineTime,
    HighEngineTime = EngineTime + ASleep + 2,
    LowEngineTime  = EngineTime + ASleep - 1,

    ?IPRINT("snmp_framework_mib -> time(s): "
            "~n   EngineTime 1:                         ~p"
            "~n      Time to acquire:                   ~w msec"
            "~n   EngineTime 2:                         ~p"
            "~n      Time to acquire:                   ~w msec"
            "~n   => Total time to acquire:             ~w msec"
            "~n      Sleep between get(snmpEngineTime): ~w (~w) sec"
            "~n      Engine Time Diff:                  ~w sec"
            "~n      Success if:"
            "~n           ~w (low) =< Engine Time 2 =< ~w (high)",
            [EngineTime, T2-T1, 
             EngineTime2, T4-T3,
             T4-T1, ASleep, Sleep, EngineTimeDiff, LowEngineTime, HighEngineTime]),

    %% In our environment, get'ing the snmpEngineTime should only take
    %% a couple of milli sec (less then 100).
    %% So just as a sanity check, we check that it its < 1000ms.
    %% We do have a couple of machines that are "unstable"...

    ?IPRINT("check that all snmpEngineTime acquire < 1000"),
    if
        ((T2 - T1) >= 1000) ->
            ?WPRINT("Failed snmpEngineTime 1 sanity check: "
                    "~n      Time to acquire ~w >= 1000", [T2-T1]),
            ?SKIP({acquire, snmpEngineTime, 1, T1, T2});
        ((T4 - T3) >= 1000) ->
            ?WPRINT("Failed snmpEngineTime 2 sanity check: "
                    "~n      Time to acquire ~w >= 1000", [T4-T3]),
            ?SKIP({acquire, snmpEngineTime, 2, T3, T4});
        true ->
            ok
    end,

    if
        (HighEngineTime < EngineTime2) ->
            ?EPRINT("snmp_framework_mib -> (High) Engine Time diff (~w) too large: "
                    "~n      ~w < ~w",
                    [EngineTimeDiff, HighEngineTime, EngineTime2]),
            ?FAIL({too_large_diff, EngineTime, EngineTime2});
        (LowEngineTime > EngineTime2) ->
             ?EPRINT("snmp_framework_mib -> (Low) Engine Time diff (~w) too large: "
                    "~n      ~w > ~w",
                    [EngineTimeDiff, LowEngineTime, EngineTime2]),
            ?FAIL({too_large_diff, EngineTime, EngineTime2});
        true -> 
            ok
    end,

    T5 = snmp_misc:now(ms),
    case get_req(4, [[snmpEngineBoots,0]]) of
	      [Boots] when is_integer(Boots) -> 
                  T6 = snmp_misc:now(ms),
                  ?IPRINT("snmp_framework_mib -> "
                          "~n   boots: ~p"
                          "~n      Time to acquire: ~w ms", [Boots, T6-T5]),
		  ok;
	      Else -> 
                  ?EPRINT("snmp_framework_mib -> failed get proper boots:"
                          "~n   ~p", [Else]),
		  ?FAIL({invalid_boots, Else})
	  end,
    ok.

%%-----------------------------------------------------------------
%% o  Test the counters
%%-----------------------------------------------------------------
snmp_mpd_mib_3(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(snmp_mpd_mib_3), 
    init_case(Config),
    UnknownPDUHs = try_test(snmp_mpd_mib_a),
    try_test(snmp_mpd_mib_b, [], [{context_engine_id, "bad engine"}]),
    try_test(snmp_mpd_mib_c, [UnknownPDUHs]).
    

%% Req. SNMP-MPD-MIB
snmp_mpd_mib_a() ->
    [UnknownSecs, InvalidMsgs] =
	get_req(1, [[snmpUnknownSecurityModels,0],
		    [snmpInvalidMsgs,0]]),
    Pdu = #pdu{type = 'get-request',
	       request_id = 23,
	       error_status = noError,
	       error_index = 0,
	       varbinds = []},
    SPdu = #scopedPdu{contextEngineID = "agentEngine",
		      contextName = "",
		      data = Pdu},
    SPDUBytes = snmp_pdus:enc_scoped_pdu(SPdu),
    V3Hdr1 = #v3_hdr{msgID = 21,
		     msgMaxSize = 484,
		     msgFlags = [7],
		     msgSecurityModel = 23,  % bad sec model
		     msgSecurityParameters = []},
    V3Hdr2 = #v3_hdr{msgID = 21,
		     msgMaxSize = 484,
		     msgFlags = [6], % bad flag combination
		     msgSecurityModel = 3,
		     msgSecurityParameters = []},
    Message1 = #message{version = 'version-3', vsn_hdr = V3Hdr1,
			data = SPDUBytes},
    Message2 = #message{version = 'version-3', vsn_hdr = V3Hdr2,
			data = SPDUBytes},
    MsgBytes1 = snmp_pdus:enc_message_only(Message1),
    MsgBytes2 = snmp_pdus:enc_message_only(Message2),
    snmp_test_mgr:send_bytes(MsgBytes1),
    snmp_test_mgr:send_bytes(MsgBytes2),

    [UnknownSecs2, InvalidMsgs2, UnknownPDUHs] =
	get_req(1, [[snmpUnknownSecurityModels,0],
		    [snmpInvalidMsgs,0],
		    [snmpUnknownPDUHandlers, 0]]),
    UnknownSecs2 = UnknownSecs + 1,
    InvalidMsgs2 = InvalidMsgs + 1,
    UnknownPDUHs.

-define(snmpUnknownPDUHandlers_instance, [1,3,6,1,6,3,11,2,1,3,0]).
snmp_mpd_mib_b() ->
    g([[sysUpTime,0]]),
    ?expect2(report, [{?snmpUnknownPDUHandlers_instance, any}]).
    

snmp_mpd_mib_c(UnknownPDUHs) ->
    [UnknownPDUHs2] = get_req(1, [[snmpUnknownPDUHandlers, 0]]),
    UnknownPDUHs2 = UnknownPDUHs + 1,
    ok.


snmp_target_mib(Config) when is_list(Config) ->
    ?P(snmp_target_mib), 
    init_case(Config),
    load_master_std("SNMP-TARGET-MIB"),
    try_test(snmp_target_mib_test),
    unload_master("SNMP-TARGET-MIB").

snmp_target_mib_2(X) -> ?P(snmp_target_mib_2), snmp_target_mib(X).

snmp_target_mib_3(X) -> ?P(snmp_target_mib_3), snmp_target_mib(X).

snmp_target_mib_test() ->
    ?NPRINT("NOT YET IMPLEMENTED"),
    nyi.

snmp_notification_mib(Config) when is_list(Config) ->
    ?P(snmp_notification_mib), 
    init_case(Config),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    try_test(snmp_notification_mib_test),
    unload_master("SNMP-NOTIFICATION-MIB").

snmp_notification_mib_2(X) -> ?P(snmp_notification_mib_2), 
			      snmp_notification_mib(X).

snmp_notification_mib_3(X) -> ?P(snmp_notification_mib_3), 
			      snmp_notification_mib(X).

snmp_notification_mib_test() ->
    ?NPRINT("NOT YET IMPLEMENTED"),
    nyi.


%%-----------------------------------------------------------------
%% o  add/delete views and try them
%% o  try boundaries
%%-----------------------------------------------------------------
snmp_view_based_acm_mib(Config) when is_list(Config) ->
    ?P(snmp_view_based_acm_mib), 
    init_case(Config),

    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    load_master("Test2"),
    snmp_view_based_acm_mib(),
    unload_master("Test2"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB").

snmp_view_based_acm_mib_2(X) -> 
    ?P(snmp_view_based_acm_mib_2), 
    snmp_view_based_acm_mib(X).

snmp_view_based_acm_mib_3(X) -> 
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(snmp_view_based_acm_mib_3), 
    snmp_view_based_acm_mib(X).

snmp_view_based_acm_mib() ->
    snmpa:verbosity(net_if,trace),
    snmpa:verbosity(master_agent,trace),
    ?IPRINT("start snmp_view_based_acm_mib test"),
    %% The user "no-rights" is present in USM, and is mapped to security
    %% name 'no-rights", which is not present in VACM.
    %% So, we'll add rights for it, try them and delete them.
    %% We'll give "no-rights" write access to tDescr.0 and read access
    %% to tDescr2.0
    %% These are the options we'll use to the mgr
    Opts = [{user, "no-rights"}, {community, "no-rights"}],
    %% Find the valid secmodel, and one invalid secmodel.
    {SecMod, InvSecMod} = 
	case get(vsn) of
	    v1 -> {?SEC_V1, ?SEC_V2C};
	    v2 -> {?SEC_V2C, ?SEC_USM};
	    v3 -> {?SEC_USM, ?SEC_V1}
	end,
    ?DBG("assign rights for 'no-rights'",[]),
    try_test(use_no_rights, [], Opts),

    %% Now, add a mapping from "no-rights" -> "no-rights-group"
    GRow1Status = [vacmSecurityToGroupStatus,[SecMod, 9,"no-rights"]],
    GRow1 = 
	[{[vacmGroupName, [SecMod, 9,"no-rights"]], "no-rights-group"},
	 {GRow1Status, ?createAndGo}],
    ?DBG("set '~p'",[GRow1]),
    try_test(do_set, [GRow1]),

    ?DBG("assign rights for 'no-rights'",[]),
    try_test(use_no_rights, [], Opts),

    %% Create a mapping for another sec model, and make sure it doesn't
    %% give us access
    GRow2Status = [vacmSecurityToGroupStatus,[InvSecMod, 9,"no-rights"]],
    GRow2 = [{[vacmGroupName, [InvSecMod, 9, "no-rights"]], "initial"},
	     {GRow2Status, ?createAndGo}],

    ?DBG("set '~p'",[GRow2]),
    try_test(do_set, [GRow2]),

    ?DBG("assign rights for 'no-rights'",[]),
    try_test(use_no_rights, [], Opts),

    %% Delete that row
    try_test(del_row, [GRow2Status]),
    
    RVName = "rv_name",
    WVName = "wv_name",

    %% Access row
    ARow1Idx = [15 | "no-rights-group"] ++ [0, ?SEC_ANY, 1],
    ARow1Status = [vacmAccessStatus, ARow1Idx],
    ARow1 = [{[vacmAccessContextMatch, ARow1Idx], 1},
	     {[vacmAccessReadViewName, ARow1Idx], RVName},
	     {[vacmAccessWriteViewName, ARow1Idx], WVName},
	     {ARow1Status, ?createAndGo}],
    
    %% This access row would give access, if InvSecMod was valid.
    ARow2Idx = [15 | "no-rights-group"] ++ [0, InvSecMod, 1],
    ARow2Status = [vacmAccessStatus, ARow2Idx],
    ARow2 = [{[vacmAccessContextMatch, ARow2Idx], 1},
	     {[vacmAccessReadViewName, ARow2Idx], "internet"},
	     {[vacmAccessWriteViewName, ARow2Idx], "internet"},
	     {ARow2Status, ?createAndGo}],
    
    try_test(do_set, [ARow2]),

    try_test(use_no_rights, [], Opts),

    %% Delete that row
    try_test(del_row, [ARow2Status]),
    

    %% Add valid row
    try_test(do_set, [ARow1]),

    try_test(use_no_rights, [], Opts),

    %% Create the view family
    VRow1Idx = mk_ln(RVName) ++ mk_ln(?xDescr),         % object access
    VRow2Idx = mk_ln(RVName) ++ mk_ln(?xDescr2 ++ [0]), % instance access
    VRow3Idx = mk_ln(WVName) ++ mk_ln(?xDescr),         % object access
    VRow4Idx = mk_ln(WVName) ++ mk_ln(?xDescr ++ [0]),  % instance access
    VRow1Status = [vacmViewTreeFamilyStatus, VRow1Idx],
    VRow2Status = [vacmViewTreeFamilyStatus, VRow2Idx],
    VRow3Status = [vacmViewTreeFamilyStatus, VRow3Idx],
    VRow4Status = [vacmViewTreeFamilyStatus, VRow4Idx],
    
    try_test(add_row, [VRow1Status]),
    try_test(add_row, [VRow2Status]),
    try_test(add_row, [VRow3Status]),

    %% We're supposed to have access now...
    try_test(use_rights, [], Opts),

    %% Change Row3 to Row4
    try_test(del_row, [VRow3Status]),
    try_test(add_row, [VRow4Status]),

    %% We should still have access...
    try_test(use_rights, [], Opts),

    %% Delete rows
    try_test(del_row, [GRow1Status]),
    
    try_test(use_no_rights, [], Opts),

    %% Delete rest of rows
    try_test(del_row, [ARow1Status]),
    try_test(del_row, [VRow1Status]),
    try_test(del_row, [VRow2Status]),
    try_test(del_row, [VRow4Status]),

    try_test(use_no_rights, [], Opts),
    snmpa:verbosity(master_agent,log).

do_set(Row) ->
    s(Row),
    ?expect1(Row).
    
add_row(RowStatus) ->
    s([{RowStatus, ?createAndGo}]),
    ?expect1([{RowStatus, ?createAndGo}]).

del_row(RowStatus) ->
    s([{RowStatus, ?destroy}]),
    ?expect1([{RowStatus, ?destroy}]).
    
    

use_no_rights() ->
    g([[xDescr,0]]),
    ?v1_2_3(?expect3(noSuchName, 1, any),
	    ?expect1([{[xDescr,0], noSuchObject}]),
	    ?expect3(authorizationError, 1, any)),
    g([[xDescr2,0]]),
    ?v1_2_3(?expect3(noSuchName, 1, any),
	    ?expect1([{[xDescr2,0], noSuchObject}]),
	    ?expect3(authorizationError, 1, any)),
    gn([[xDescr]]),
    ?v1_2_3(?expect3(noSuchName, 1, any),
	    ?expect1([{[xDescr], endOfMibView}]),
	    ?expect3(authorizationError, 1, any)),
    s([{[xDescr,0], "tryit"}]),
    ?v1_2_3(?expect3(noSuchName, 1, any),
	    ?expect3(noAccess, 1, any),
	    ?expect3(authorizationError, 1, any)).


use_rights() ->
    g([[xDescr,0]]),
    ?expect1([{[xDescr,0], any}]),
    g([[xDescr2,0]]),
    ?expect1([{[xDescr2,0], any}]),
    s([{[xDescr,0], "tryit"}]),
    ?expect3(noError, 0, any),
    g([[xDescr,0]]),
    ?expect1([{[xDescr,0], "tryit"}]).

mk_ln(X) ->
    [length(X) | X].


%%-----------------------------------------------------------------
%% o  add/delete users and try them
%% o  test all secLevels
%% o  test all combinations of protocols
%% o  try bad ops; check counters
%%-----------------------------------------------------------------
snmp_user_based_sm_mib_3(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [{unix, [darwin]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(snmp_user_based_sm_mib_3), 
    init_case(Config),

    _AgentDir = ?config(agent_conf_dir, Config),
    load_master_std("SNMP-USER-BASED-SM-MIB"),

    %% The newUser used here already has VACM access.
    
    %% Add a new user in the simplest way; just createAndGo
    try_test(v3_sync, [[{usm_add_user1, []}]],
	[{sec_level, authPriv}, {user, "privDES"}]),

    %% Try to use the new user
    load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    unload_master("Test2"),

    ShaKey1 = snmp:passwd2localized_key(sha, "new sha password", "agentEngine"),
    DesKey1 = lists:sublist(ShaKey1, 16),

    %% Change the new user's keys - 1
    try_test(v3_sync, [[{usm_key_change1, [ShaKey1, DesKey1]}]],
	[{sec_level, authPriv}, {user, "newUser"}]),

    %% Try to use the new keys
    MgrDir = ?config(mgr_dir, Config),
    rewrite_usm_mgr(MgrDir, ShaKey1, DesKey1),
    load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    unload_master("Test2"),

    ShaKey2 = snmp:passwd2localized_key(sha, "newer password", "agentEngine"),
    DesKey2 = lists:sublist(ShaKey2, 16),

    %% Change the new user's keys - 2
    try_test(v3_sync,
	      [[{usm_key_change2, [ShaKey1, DesKey1, ShaKey2, DesKey2]}]],
	      [{sec_level, authPriv}, {user, "newUser"}]),

    %% Try to use the new keys
    reset_usm_mgr(MgrDir),
    rewrite_usm_mgr(MgrDir, ShaKey2, DesKey2),
    load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	      [{sec_level, authPriv}, {user, "newUser"}]),
    unload_master("Test2"),
    reset_usm_mgr(MgrDir),

    %% Change the new user's keys - 3
    try_test(v3_sync,
	      [[{usm_key_change3, [ShaKey2, DesKey2, ShaKey1, DesKey1]}]],
	      [{sec_level, authPriv}, {user, "privDES"}]),

    %% Try to use the new keys
    rewrite_usm_mgr(MgrDir, ShaKey1, DesKey1),
    load_master("Test2"),
    try_test(v3_sync, [[{usm_use_user, []}]],
	[{sec_level, authPriv}, {user, "newUser"}]),
    unload_master("Test2"),
    reset_usm_mgr(MgrDir),

    %% Try some read requests
    try_test(v3_sync, [[{usm_read, []}]],
		   [{sec_level, authPriv}, {user, "privDES"}]),

    %% Delete the new user
    try_test(v3_sync, [[{usm_del_user, []}]],
		   [{sec_level, authPriv}, {user, "privDES"}]),

    %% Try some bad requests
    try_test(v3_sync, [[{usm_bad, []}]],
		   [{sec_level, authPriv}, {user, "privDES"}]),

    unload_master("SNMP-USER-BASED-SM-MIB").

-define(usmUserSecurityName, [1,3,6,1,6,3,15,1,2,2,1,3]).

usm_add_user1() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    RowPointer = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs1  = [{[usmUserCloneFrom, NewRowIndex], RowPointer},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    s(Vbs1),
    ?expect1(Vbs1),
    ok.
    
usm_use_user() ->
    v2_proc().


%% Change own public keys
usm_key_change1(ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							"passwd_shaxxxxxxxxxx",
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							"passwd_desxxxxxx",
							DesKey),
    Vbs1 = [{[usmUserAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs1),
    ?expect1(Vbs1).
    
%% Change own private keys
usm_key_change2(OldShaKey, OldDesKey, ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldShaKey,
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldDesKey,
							DesKey),
    Vbs1 = [{[usmUserOwnAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserOwnPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs1),
    ?expect1(Vbs1).
    
%% Change other's public keys
usm_key_change3(OldShaKey, OldDesKey, ShaKey, DesKey) ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    ShaKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldShaKey,
							ShaKey),
    DesKeyChange = snmp_user_based_sm_mib:mk_key_change(sha,
							OldDesKey,
							DesKey),
    Vbs1 = [{[usmUserOwnAuthKeyChange, NewRowIndex], ShaKeyChange}],
    s(Vbs1),
    ?expect3(noAccess, 1, any),
    Vbs2 = [{[usmUserOwnPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs2),
    ?expect3(noAccess, 1, any),
    
    
    Vbs3 = [{[usmUserAuthKeyChange, NewRowIndex], ShaKeyChange},
	    {[usmUserPrivKeyChange, NewRowIndex], DesKeyChange}],
    s(Vbs3),
    ?expect1(Vbs3),
    ok.

usm_read() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    g([[usmUserSecurityName, NewRowIndex],
	     [usmUserCloneFrom, NewRowIndex],
	     [usmUserAuthKeyChange, NewRowIndex],
	     [usmUserOwnAuthKeyChange, NewRowIndex],
	     [usmUserPrivKeyChange, NewRowIndex],
	     [usmUserOwnPrivKeyChange, NewRowIndex]]),
    ?expect1([{[usmUserSecurityName, NewRowIndex], "newUser"},
		    {[usmUserCloneFrom, NewRowIndex], [0,0]},
		    {[usmUserAuthKeyChange, NewRowIndex], ""},
		    {[usmUserOwnAuthKeyChange, NewRowIndex], ""},
		    {[usmUserPrivKeyChange, NewRowIndex], ""},
		    {[usmUserOwnPrivKeyChange, NewRowIndex], ""}]),
    ok.
    
    

usm_del_user() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    Vbs1  = [{[usmUserStatus, NewRowIndex], ?destroy}],
    s(Vbs1),
    ?expect1(Vbs1),
    ok.

-define(usmUserCloneFrom, [1,3,6,1,6,3,15,1,2,2,1,4]).

-define(usmNoAuthProtocol, [1,3,6,1,6,3,10,1,1,1]).

-define(usmHMACMD5AuthProtocol, [1,3,6,1,6,3,10,1,1,2]).

-define(usmHMACSHAAuthProtocol, [1,3,6,1,6,3,10,1,1,3]).

-define(usmNoPrivProtocol, [1,3,6,1,6,3,10,1,2,1]).

-define(usmDESPrivProtocol, [1,3,6,1,6,3,10,1,2,2]).

usm_bad() ->
    NewRowIndex = [11,"agentEngine", 7, "newUser"],
    RowPointer1 = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDOS"],
    Vbs1  = [{[usmUserCloneFrom, NewRowIndex], RowPointer1},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    s(Vbs1),
    ?expect3(inconsistentName, 1, any),

    RowPointer2 = ?usmUserCloneFrom ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs2  = [{[usmUserCloneFrom, NewRowIndex], RowPointer2},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    s(Vbs2),
    ?expect3(wrongValue, 1, any),

    RowPointer3 = ?usmUserSecurityName ++ [11|"agentEngine"] ++ [7|"privDES"],
    Vbs3  = [{[usmUserCloneFrom, NewRowIndex], RowPointer3},
	     {[usmUserStatus, NewRowIndex], ?createAndGo}],
    s(Vbs3),
    ?expect1(Vbs3),
    s([{[usmUserAuthProtocol, NewRowIndex], ?usmNoAuthProtocol}]),
    ?expect3(inconsistentValue, 1, any),
    s([{[usmUserAuthProtocol, NewRowIndex], ?usmHMACMD5AuthProtocol}]),
    ?expect3(inconsistentValue, 1, any),
    s([{[usmUserAuthProtocol, NewRowIndex], ?usmDESPrivProtocol}]),
    ?expect3(wrongValue, 1, any),
    s([{[usmUserPrivProtocol, NewRowIndex], ?usmHMACSHAAuthProtocol}]),
    ?expect3(wrongValue, 1, any),

    Vbs4  = [{[usmUserStatus, NewRowIndex], ?destroy}],
    s(Vbs4),
    ?expect1(Vbs4),
    ok.
    

%%-----------------------------------------------------------------
%% Loop through entire MIB, to make sure that all instrum. funcs
%% works.
%% Load all std mibs that are not loaded by default.
%%-----------------------------------------------------------------
loop_mib_1(Config) when is_list(Config) ->
    ?P(loop_mib_1),
    ?IPRINT("loop_mib_1 -> initiate case"),

    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("loop_mib_1 -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p",[_SaNode, _MgrNode, _MibDir]),
    ?DBG("loop_mib_1 -> load mib SNMP-COMMUNITY-MIB",[]),
    load_master_std("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib_1 -> load mib SNMP-MPD-MIB",[]),
    load_master_std("SNMP-MPD-MIB"),
    ?DBG("loop_mib_1 -> load mib SNMP-TARGET-MIB",[]),
    load_master_std("SNMP-TARGET-MIB"),
    ?DBG("loop_mib_1 -> load mib SNMP-NOTIFICATION-MIB",[]),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib_1 -> load mib SNMP-FRAMEWORK-MIB",[]),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib_1 -> load mib SNMP-VIEW-BASED-ACM-MIB",[]),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    ?DBG("loop_mib_1 -> try",[]),

    try_test(loop_mib_1_test),

    ?DBG("loop_mib_1 -> unload mib SNMP-COMMUNITY-MIB",[]),
    unload_master("SNMP-COMMUNITY-MIB"),
    ?DBG("loop_mib_1 -> unload mib SNMP-MPD-MIB",[]),
    unload_master("SNMP-MPD-MIB"),
    ?DBG("loop_mib_1 -> unload mib SNMP-TARGET-MIB",[]),
    unload_master("SNMP-TARGET-MIB"),
    ?DBG("loop_mib_1 -> unload mib SNMP-NOTIFICATION-MIB",[]),
    unload_master("SNMP-NOTIFICATION-MIB"),
    ?DBG("loop_mib_1 -> unload mib SNMP-FRAMEWORK-MIB",[]),
    unload_master("SNMP-FRAMEWORK-MIB"),
    ?DBG("loop_mib_1 -> unload mib SNMP-VIEW-BASED-ACM-MIB",[]),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    %% snmpa:verbosity(master_agent,log),
    %% snmpa:verbosity(mib_server,silence),
    ?IPRINT("loop_mib_1 -> done"),
    ok.
    

loop_mib_2(Config) when is_list(Config) ->
    ?P(loop_mib_2), 
    ?IPRINT("loop_mib_2 -> initiate case"),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("do_loop_mib_2 -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p", [_SaNode, _MgrNode, _MibDir]),
    ?DBG("loop_mib_2 -> load mibs",[]),
    load_master_std("SNMP-COMMUNITY-MIB"),
    load_master_std("SNMP-MPD-MIB"),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    load_master_std("SNMP-FRAMEWORK-MIB"),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),

    try_test(loop_mib_2_test),

    ?DBG("loop_mib_2 -> unload mibs",[]),
    unload_master("SNMP-COMMUNITY-MIB"),
    unload_master("SNMP-MPD-MIB"),
    unload_master("SNMP-TARGET-MIB"),
    unload_master("SNMP-NOTIFICATION-MIB"),
    unload_master("SNMP-FRAMEWORK-MIB"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    ?IPRINT("loop_mib_2 -> done"),
    ok.


loop_mib_3(Config) when is_list(Config) ->
    ?P(loop_mib_3), 
    ?IPRINT("loop_mib_3 -> initiate case"),
    {_SaNode, _MgrNode, _MibDir} = init_case(Config),
    ?DBG("loop_mib_3 -> ~n"
	   "\tSaNode:  ~p~n"
	   "\tMgrNode: ~p~n"
	   "\tMibDir:  ~p", [_SaNode, _MgrNode, _MibDir]),
    ?DBG("loop_mib_3 -> load mibs",[]),
    load_master_std("SNMP-TARGET-MIB"),
    load_master_std("SNMP-NOTIFICATION-MIB"),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    load_master_std("SNMP-USER-BASED-SM-MIB"),

    try_test(loop_mib_3_test),

    ?DBG("loop_mib_3 -> unload mibs",[]),
    unload_master("SNMP-TARGET-MIB"),
    unload_master("SNMP-NOTIFICATION-MIB"),
    unload_master("SNMP-VIEW-BASED-ACM-MIB"),
    unload_master("SNMP-USER-BASED-SM-MIB"),
    ?IPRINT("loop_mib_3 -> done"),
    ok.


%% Req. As many mibs all possible
loop_mib_1_test() ->
    ?IPRINT("loop_mib_1_test -> entry"),
    N = loop_it_1([1,1], 0),
    ?IPRINT("found ~w variables", [N]),
    N = if N < 100 -> 100;
		 true -> N
	      end.

loop_it_1(Oid, N) ->
    ?IPRINT("loop_it_1_test -> entry with"
            "~n      Oid: ~p"
            "~n      N:   ~p", [Oid, N]),
    case get_next_req([Oid]) of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = [#varbind{oid   = NOid,
				      value = _Value}]} when NOid > Oid ->
	    ?IPRINT("loop_it_1_test -> "
                    "expected intermediate (get-next) result: "
                    "~n   NOid:  ~p"
                    "~n   Value: ~p", [NOid, _Value]),
	    [_Value2] = get_req(1, [NOid]), % must not be same
	    ?IPRINT("loop_it_1_test -> expected intermediate (get) result: "
                    "~n   Value2: ~p", [_Value2]),
	    loop_it_1(NOid, N+1);

	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_1_test -> unexpected (get-response) vbs: "
                    "~n      Vbs: ~p", [Vbs]),
	    ?FAIL({unexpected_vbs,
                         [{get_next_oid, Oid}, 
                          {counter,      N},
                          {varbinds,     Vbs}]});

	#pdu{type         = 'get-response', 
	     error_status = noSuchName, 
	     error_index = 1,
	     varbinds    = [_]} ->
	    ?IPRINT("loop_it_1_test -> done: ~p", [N]),
	    N;

	#pdu{type         = 'get-response', 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_1_test -> unexpected (get-response) pdu: "
                    "~n      Err: ~p"
                    "~n      Idx: ~p"
                    "~n      Vbs: ~p", [Err, Idx, Vbs]),
	    ?FAIL({unexpected_pdu,
                         [{get_next_oid, Oid},
                          {counter,      N},
                          {error_status, Err},
                          {error_index,  Idx},
                          {varbinds,     Vbs}]});

	#pdu{type         = Type, 
	     error_status = Err, 
	     error_index  = Idx,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_1_test -> unexpected pdu: "
                    "~n      Type: ~p"
                    "~n      Err:  ~p"
                    "~n      Idx:  ~p"
                    "~n      Vbs:  ~p", [Type, Err, Idx, Vbs]),
	    ?FAIL({unexpected_pdu,
                         [{get_next_oid, Oid},
                          {counter,      N},
                          {type,         Type},
                          {error_status, Err},
                          {error_index,  Idx},
                          {varbinds,     Vbs}]});

	{error, Reason} ->
            %% Regardless of the error here (its usually timeout),
            %% if we have had system events we skip since the results
            %% in those cases are simply not reliable.
            %% There is just no point in trying to analyze the reason.
            ?IPRINT("loop_it_1_test -> receive error: "
                    "~n      ~p", [Reason]),
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("loop_it_1_test -> error: "
                            "~n      ~p", [Reason]),
                    ?FAIL([{get_next_oid, Oid},
                                 {counter,      N},
                                 {reason,       Reason}]);

                        true ->
                    ?WPRINT("loop_it_1_test -> "
                            "error when we got system events: "
                            "~n   Reason:     ~p"
                            "~n   Sys Events: ~p"
                            "~n", [Reason, SysEvs]),
                    ?SKIP([{reason, Reason}, {system_events, SysEvs}])
            end
    end.
	    

%% Req. As many mibs as possible
loop_mib_2_test() ->
    ?IPRINT("loop_mib_2_test -> entry"),
    N = loop_it_2([1,1], 0),
    ?IPRINT("found ~w variables", [N]),
    N = if N < 100 -> 100;
		 true -> N
	      end.

loop_it_2(Oid, N) ->
    ?IPRINT("loop_it_2 -> entry with"
            "~n   Oid: ~p"
            "~n   N:   ~p", [Oid, N]),
    case get_next_req([Oid]) of
	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = [#varbind{oid = _NOid, value = endOfMibView}]} ->
	    ?IPRINT("loop_it_2 -> done: "
                    "~n   NOid: ~p", [_NOid]),
	    N;

	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = [#varbind{oid   = NOid,
				      value = _Value}]} when NOid > Oid ->
	    ?IPRINT("loop_it_2 -> "
                    "expected intermediate (get-next) result: "
                    "~n   NOid:  ~p"
                    "~n   Value: ~p", [NOid, _Value]),
	    [_Value2] = get_req(1, [NOid]), % must not be same
	    ?IPRINT("loop_it_2 -> expected intermediate (get) result: "
                    "~n   Value2: ~p", [_Value2]),
	    loop_it_2(NOid, N+1);

	#pdu{type         = 'get-response', 
	     error_status = noError, 
	     error_index  = 0,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_2 -> unexpected (get-response) vbs: "
                    "~n      Vbs: ~p", [Vbs]),
	    ?FAIL({unexpected_vbs,
                         [{get_next_oid, Oid},
                          {counter,      N},
                          {varbinds,     Vbs}]});

	#pdu{type         = 'get-response', 
	     error_status = ES, 
	     error_index  = EI,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_2 -> unexpected (get-response) pdu: "
                    "~n      ES:  ~p"
                    "~n      EI:  ~p"
                    "~n      Vbs: ~p", [ES, EI, Vbs]),
	    ?FAIL({unexpected_pdu,
                         [{get_next_oid, Oid},
                          {counter,      N},
                          {error_status, ES}, 
                          {error_index,  EI},
                          {varbinds,     Vbs}]});

	#pdu{type         = Type, 
	     error_status = ES, 
	     error_index  = EI,
	     varbinds     = Vbs} ->
            ?EPRINT("loop_it_2 -> unexpected pdu: "
                    "~n      Type: ~p"
                    "~n      ES:   ~p"
                    "~n      EI:   ~p"
                    "~n      Vbs:  ~p", [Type, ES, EI, Vbs]),
	    ?FAIL({unexpected_pdu,
                         [{get_next_oid, Oid},
                          {counter,      N},
                          {type,         Type}, 
                          {error_status, ES}, 
                          {error_index,  EI},
                          {varbinds,     Vbs}]});

	{error, Reason} ->
            %% Regardless of the error here (its usually timeout),
            %% if we have had system events we skip since the results
            %% in those cases are simply not reliable.
            %% There is just no point in trying to analyze the reason.
            ?IPRINT("loop_it_2 -> receive error: "
                    "~n      ~p", [Reason]),
            SysEvs = snmp_test_global_sys_monitor:events(),
            if
                (SysEvs =:= []) ->
                    ?EPRINT("loop_it_2 -> error *without* system events: "
                            "~n   Oid:    ~p"
                            "~n   N:      ~p"
                            "~n   Reason: ~p", [Oid, N,Reason]),
                    ?FAIL([{get_next_oid, Oid},
                           {counter,      N},
                           {reason,       Reason}]);

                true ->
                    ?WPRINT("loop_it_2 -> "
                            "error *with* system events: "
                            "~n   Oid:        ~p"
                            "~n   N:          ~p"
                            "~n   Reason:     ~p"
                            "~n   Sys Events: ~p"
                            "~n", [Oid, N, Reason, SysEvs]),
                    ?SKIP([{reason, Reason}, {system_events, SysEvs}])
            end
    end.
	    
loop_mib_3_test() ->
    ?DBG("loop_mib_3_test -> entry",[]),
    loop_mib_2_test().


%%%-----------------------------------------------------------------
%%% Testing of reported bugs and other tickets.
%%%-----------------------------------------------------------------

reported_bugs_cases() ->
    [
     otp_1128, 
     otp_1129, 
     otp_1131, 
     otp_1162, 
     otp_1222,
     otp_1298, 
     otp_1331, 
     otp_1338, 
     otp_1342, 
     otp_1366, 
     otp_2776,
     otp_2979, 
     otp_3187, 
     otp_3725
    ].

reported_bugs2_cases() ->
    [
     otp_1128_2, 
     otp_1129_2, 
     otp_1131_2, 
     otp_1162_2,
     otp_1222_2, 
     otp_1298_2, 
     otp_1331_2, 
     otp_1338_2,
     otp_1342_2, 
     otp_1366_2, 
     otp_2776_2, 
     otp_2979_2, 
     otp_3187_2
    ].

reported_bugs3_cases() ->
    [
     otp_1128_3, 
     otp_1129_3, 
     otp_1131_3, 
     otp_1162_3,
     otp_1222_3, 
     otp_1298_3, 
     otp_1331_3, 
     otp_1338_3,
     otp_1342_3, 
     otp_1366_3, 
     otp_2776_3, 
     otp_2979_3, 
     otp_3187_3,
     otp_3542
    ].
   
    
%%-----------------------------------------------------------------
%% Ticket: OTP-1128
%% Slogan: Bug in handling of createAndWait set-requests.
%%-----------------------------------------------------------------
otp_1128(Config) when is_list(Config) ->
    ?P(otp_1128), 
    init_case(Config),

    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(otp_1128_test),
    unload_master("OLD-SNMPEA-MIB"),
    ok.

otp_1128_2(X) -> ?P(otp_1128_2), otp_1128(X).

otp_1128_3(X) -> ?P(otp_1128_3), otp_1128(X).

otp_1128_test() ->
    io:format("Testing bug reported in ticket OTP-1128...~n"),

    NewKeyc3 = [intCommunityViewIndex,get(mip),is("test")],
    NewKeyc4 = [intCommunityAccess,get(mip),is("test")],
    NewKeyc5 = [intCommunityStatus,get(mip),is("test")],

    s([{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    ?expect1([{NewKeyc5, ?createAndWait}, {NewKeyc4, 2}]),
    g([NewKeyc5]),
    ?expect1([{NewKeyc5, ?notReady}]),
    s([{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    ?expect1([{NewKeyc5, ?active}, {NewKeyc3, 2}]),
    g([NewKeyc5]),
    ?expect1([{NewKeyc5, ?active}]),
    s([{NewKeyc5, ?destroy}]),
    ?expect1([{NewKeyc5, ?destroy}]),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1129, OTP-1169
%% Slogan: snmpa:int_to_enum crashes on bad oids
%%-----------------------------------------------------------------
otp_1129(Config) when is_list(Config) ->
    ?P(otp_1129), 
    init_case(Config),
    load_master("Klas3"),
    try_test(otp_1129_i, [node()]),
    unload_master("Klas3"),
    ok.

otp_1129_2(X) -> ?P(otp_1129_2), otp_1129(X).

otp_1129_3(X) -> ?P(otp_1129_3), otp_1129(X).

otp_1129_i(MaNode) ->
    io:format("Testing bug reported in ticket OTP-1129...~n"),
    false = rpc:call(MaNode, snmpa, int_to_enum, [iso, 1]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [isox, 1]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1131
%% Slogan: Agent crashes / erlang node halts if RowIndex in a
%%         setrequest is of bad type, e.g. an INDEX {INTEGER},
%%         and RowIdenx [3,2].
%%-----------------------------------------------------------------
otp_1131(Config) when is_list(Config) ->
    ?P(otp_1131), 
    init_case(Config),

    load_master("Klas1"),
    try_test(otp_1131_test),
    unload_master("Klas1").

otp_1131_2(X) -> ?P(otp_1131_2), otp_1131(X).

otp_1131_3(X) -> 
    %% <CONDITIONAL-SKIP>
    %% This is intended to catch Montavista Linux 4.0/ppc (2.6.5)
    %% Montavista Linux looks like a Debian distro (/etc/issue)
    LinuxVersionVerify = 
	fun() ->
		case string:to_lower(os:cmd("uname -m")) of
		    "ppc" ++ _ ->
			case file:read_file_info("/etc/issue") of
			    {ok, _} ->
				case string:to_lower(
                                       os:cmd("grep -i montavista /etc/issue")) of
				    "montavista" ++ _ ->
					case os:version() of
					    {2, 6, 10} ->
                                                ?IPRINT("(PPC Linux) kernel version check: "
                                                        "{2, 6, 10} => SKIP"),
						true;
					    V ->
                                                ?IPRINT("(PPC Linux) kernel version check: "
                                                        "~p != {2, 6, 10} => *NO* SKIP", [V]),
						false
					end;
				    _ -> % Maybe plain Debian or Ubuntu
                                        ?IPRINT("(PPC Linux) Not MontaVista => *NO* SKIP"),
					false
				end;
			    _ ->
				%% Not a Debian based distro
                                ?IPRINT("(PPC Linux) Unknown distro => *NO* SKIP"),
				false
			end;
		    _ ->
                        ?IPRINT("(Linux) Not PPC => *NO* SKIP"),
			false
		end
	end,
    Skippable = [{unix, [darwin, {linux, LinuxVersionVerify}]}],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(X, Condition),
    %% </CONDITIONAL-SKIP>

    ?P(otp_1131_3), 
    otp_1131(X).

otp_1131_test() ->
    io:format("Testing bug reported in ticket OTP-1131...~n"),
    s([{[friendsEntry, [2, 3, 1]], s, "kompis3"},
       {[friendsEntry, [3, 3, 1]], i, ?createAndGo}]),
    ?expect3(?v1_2(noSuchName, noCreation), 2, any),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1162
%% Slogan: snmp_agent can't handle wrongValue from instrum.func
%%-----------------------------------------------------------------
otp_1162(Config) when is_list(Config) ->
    ?P(otp_1162), 
    {SaNode, _MgrNode, _MibDir} = init_case(Config),
    {ok, SA} = start_subagent(SaNode, ?sa, "SA-MIB"),
    try_test(otp_1162_test),
    stop_subagent(SA).

otp_1162_2(X) -> ?P(otp_1162_2), otp_1162(X).

otp_1162_3(X) -> ?P(otp_1162_3), otp_1162(X).

otp_1162_test() ->
    s([{[sa, [2,0]], 6}]), % wrongValue (i is_set_ok)
    ?expect3(?v1_2(badValue, wrongValue), 1, any),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1222
%% Slogan: snmp agent crash if faulty index is returned from instrum
%%-----------------------------------------------------------------
otp_1222(Config) when is_list(Config) ->
    ?P(otp_1222), 
    init_case(Config),
    load_master("Klas3"),
    load_master("Klas4"),
    try_test(otp_1222_test),
    unload_master("Klas3"),
    unload_master("Klas4"),
    ok.

otp_1222_2(X) -> ?P(otp_1222_2), otp_1222(X).

otp_1222_3(X) -> ?P(otp_1222_3), otp_1222(X).

otp_1222_test() ->
    io:format("Testing bug reported in ticket OTP-1222...~n"),
    s([{[fStatus4,1], 4}, {[fName4,1], 1}]),
    ?expect3(genErr, 0, any),
    s([{[fStatus4,2], 4}, {[fName4,2], 1}]),
    ?expect3(genErr, 0, any),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1298
%% Slogan: Negative INTEGER values are treated as positive.
%%-----------------------------------------------------------------
otp_1298(Config) when is_list(Config) ->
    ?P(otp_1298), 
    init_case(Config),

    load_master("Klas2"),
    try_test(otp_1298_test),
    unload_master("Klas2"),
    ok.

otp_1298_2(X) -> ?P(otp_1298_2), otp_1298(X).

otp_1298_3(X) -> ?P(otp_1298_3), otp_1298(X).

otp_1298_test() ->
    io:format("Testing bug reported in ticket OTP-1298...~n"),
    s([{[fint,0], -1}]),
    ?expect1([{[fint,0], -1}]),
    ok.
    

%%-----------------------------------------------------------------
%% Ticket: OTP-1331
%% Slogan: snmp_generic should return noError when deleting non-ex row
%%-----------------------------------------------------------------
otp_1331(Config) when is_list(Config) ->
    ?P(otp_1331), 
    init_case(Config),
    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(otp_1331_test),
    unload_master("OLD-SNMPEA-MIB"),
    ok.

otp_1331_2(X) -> ?P(otp_1331_2), otp_1331(X).

otp_1331_3(X) -> ?P(otp_1331_3), otp_1331(X).

otp_1331_test() ->
    NewKeyc5 = [intCommunityStatus,[127,32,0,0],is("test")],
    s([{NewKeyc5, ?destroy}]),
    ?expect1([{NewKeyc5, ?destroy}]),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1338
%% Slogan: snmp bug in initialisation of default values for mnesia tabs
%%-----------------------------------------------------------------
otp_1338(Config) when is_list(Config) ->
    ?P(otp_1338), 
    init_case(Config),

    load_master("Klas2"),
    try_test(otp_1338_test),
    unload_master("Klas2").

otp_1338_2(X) -> ?P(otp_1338_2), otp_1338(X).

otp_1338_3(X) -> ?P(otp_1338_3), otp_1338(X).

otp_1338_test() ->
    s([{[kStatus2, 7], i, ?createAndGo}]),
    ?expect1([{[kStatus2, 7], ?createAndGo}]),
    g([[kName2, 7]]),
    ?expect1([{[kName2, 7], "JJJ"}]).


%%-----------------------------------------------------------------
%% Ticket: OTP-1342
%% Slogan: default impl of snmp table can't handle bad index access,
%%         Set when INDEX is read-write gets into an infinite loop!
%%-----------------------------------------------------------------
otp_1342(Config) when is_list(Config) ->
    ?P(otp_1342), 
    init_case(Config),
    load_master("Klas4"),
    try_test(otp_1342_test),
    unload_master("Klas4"),
    ok.

otp_1342_2(X) -> ?P(otp_1342_2), otp_1342(X).

otp_1342_3(X) -> ?P(otp_1342_3), otp_1342(X).

otp_1342_test() ->
    s([{[fIndex5, 1], i, 1},
       {[fName5, 1], i, 3},
       {[fStatus5, 1], i, ?createAndGo}]),
    ?expect3(?v1_2(noSuchName, noCreation), 3, any),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-1366
%% Slogan: snmp traps not sent to all managers
%% Note: NYI! We need a way to tell the test server that we need
%%       mgrs on two different machines.
%%-----------------------------------------------------------------
otp_1366(Config) when is_list(Config) ->
    ?P(otp_1366), 
    init_case(Config),
    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(otp_1366_test),
    unload_master("OLD-SNMPEA-MIB"),
    ok.

otp_1366_2(X) -> ?P(otp_1366_2), otp_1366(X).

otp_1366_3(X) -> ?P(otp_1366_3), otp_1366(X).

otp_1366_test() ->
    ?NPRINT("NOT YET IMPLEMENTED"),
    'NYI'.


%%-----------------------------------------------------------------
%% Ticket: OTP-2776
%% Slogan: snmp:validate_date_and_time() fails when time is 00:00
%%-----------------------------------------------------------------
otp_2776(Config) when is_list(Config) ->
    ?P(otp_2776), 
    init_case(Config),
    try_test(otp_2776_test).
 
otp_2776_2(X) -> ?P(otp_2776_2), otp_2776(X).

otp_2776_3(X) -> ?P(otp_2776_3), otp_2776(X).
 
otp_2776_test() ->
  ?NPRINT("Testing bug reported in ticket OTP-2776..."),
 
  Dt01_valid   = [19,98,9,1,1,0,23,0,43,0,0],
  Dt02_valid   = [19,98,9,1,0,0,0,0,43,0,0],  % This is what is fixed: 00:00
  Dt03_valid   = [19,98,2,28,1,0,23,0,43,0,0],
  Dt04_invalid = [19,98,2,29,1,0,23,0,43,0,0],
  Dt05_valid   = [19,96,2,29,1,0,23,0,43,0,0],
  Dt06_valid   = [20,0,2,29,1,0,23,0,43,0,0],
  Dt07_invalid = [19,96,2,30,1,0,23,0,43,0,0], % This is also fixed: 30/2
  Dt08_valid   = [19,98,4,30,1,0,23,0,43,0,0],
  Dt09_invalid = [19,98,4,31,1,0,23,0,43,0,0], % This is also fixed: 31/4
  Dt10_invalid = [], 
  Dt11_invalid = [kalle,hobbe], 
  L = [{ 1, true,  Dt01_valid},
       { 2, true,  Dt02_valid},
       { 3, true,  Dt03_valid},
       { 4, false, Dt04_invalid},
       { 5, true,  Dt05_valid},
       { 6, true,  Dt06_valid},
       { 7, false, Dt07_invalid},
       { 8, true,  Dt08_valid},
       { 9, false, Dt09_invalid},
       {10, false, Dt10_invalid},
       {11, false, Dt11_invalid}],
  
  ok = validate_dat(L).
 

validate_dat(L) -> validate_dat(L,[]).
 
validate_dat([],V) -> 
  Fun = fun({_,X}) -> case X of
                        ok -> false;
                        _  -> true
                      end
        end,
  validate_dat1( lists:reverse( lists:filter(Fun,V) ) );
validate_dat([{Id,E,Dat}|T],V) ->
  validate_dat(T,[validate_dat2(Id,E,Dat) | V]).
 
validate_dat1([]) -> ok;
validate_dat1(L)  -> {error,L}.
 
validate_dat2(Id, E, Dat) ->
  Res = case {E,snmp:validate_date_and_time(Dat)} of
          {E,E} -> ok;
          {E,A} -> {E,A}
        end,
  {Id, Res}.


%%-----------------------------------------------------------------
%% Ticket: OTP-2979
%% Slogan: get-next on more than 1 column in an empty table
%%         returns bad response.
%%-----------------------------------------------------------------
otp_2979(Config) when is_list(Config) ->
    ?P(otp_2979), 
    init_case(Config),
    load_master("Test1"),
    init_old(),
    try_test(otp_2979_test),
    unload_master("Test1").

otp_2979_2(X) -> ?P(otp_2979_2), otp_2979(X).

otp_2979_3(X) -> ?P(otp_2979_3), otp_2979(X).

otp_2979_test() ->
    gn([[sparseDescr], [sparseStatus]]),
    ?expect1([{[sparseStr,0], "slut"},
		    {[sparseStr,0], "slut"}]),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-3187
%% Slogan: get-next on vacmAccessTable for columns > 5 returns
%%         endOfTable - should return value.
%%-----------------------------------------------------------------
otp_3187(Config) when is_list(Config) ->
    ?P(otp_3187), 
    init_case(Config),
    load_master_std("SNMP-VIEW-BASED-ACM-MIB"),
    otp_3187_test(),
    unload_master("SNMP-VIEW-BASED-ACM-MIB").

otp_3187_2(X) -> ?P(otp_3187_2), otp_3187(X).

otp_3187_3(X) -> ?P(otp_3187_3), otp_3187(X).

otp_3187_test() ->
    Elements =
       snmp_view_based_acm_mib:vacmAccessTable(get_next,[],[4,5,6]),
    lists:foreach(fun(E) ->
			   if E == endOfTable ->
					?FAIL(endOfTable);
				       true -> ok
				end
		   end, Elements).

%%-----------------------------------------------------------------
%% Ticket: OTP-3542
%% Slogan: 
%%-----------------------------------------------------------------
otp_3542(Config) when is_list(Config) ->
    ?P(otp_3542), 
    init_case(Config),
    try_test(otp_3542_test).

otp_3542_test() ->
    io:format("SNMP v3 discovery...~n"),
    Res = snmp_test_mgr:d(),
    io:format("SNMP v3 discovery result: ~p~n",[Res]).


%%-----------------------------------------------------------------
%% Ticket: OTP-3725
%% Slogan: Slow response time on snmpa:int_to_enum
%%-----------------------------------------------------------------
otp_3725(Config) when is_list(Config) ->
    ?P(otp_3725), 
    init_case(Config),

    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(otp_3725_test, [node()]),
    unload_master("OLD-SNMPEA-MIB").

%% Req. OLD-SNMPEA-MIB
otp_3725_test(MaNode) ->
    io:format("Testing feature requested in ticket OTP-3725...~n"),
    rpc:call(MaNode,snmpa,verbosity,[symbolic_store,trace]),
    Db = rpc:call(MaNode, snmpa, get_symbolic_store_db, []),
    ?DBG("otp_3725_test -> Db = ~p", [Db]),

    {value, OID} = rpc:call(MaNode, snmpa, name_to_oid,
				  [Db, intAgentIpAddress]),
    ?DBG("otp_3725_test -> name_to_oid for ~p: ~p", [intAgentIpAddress,OID]),
    {value, intAgentIpAddress} = rpc:call(MaNode, snmpa, oid_to_name,
						[Db,OID]),
    ?DBG("otp_3725_test -> oid_to_name for ~p: ~p", [OID, intAgentIpAddress]),
    false = rpc:call(MaNode, snmpa, name_to_oid, [Db, intAgentIpAddres]),
    false = rpc:call(MaNode, snmpa, oid_to_name,
			   [Db, [1,5,32,3,54,3,3,34,4]]),
    {value, 2} = rpc:call(MaNode, snmpa, enum_to_int,
				[Db, intViewType, excluded]),
    {value, excluded} = rpc:call(MaNode, snmpa, int_to_enum,
				       [Db, intViewType, 2]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [Db, intViewType, exclude]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [Db, intAgentIpAddress, exclude]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [Db, intAgentIpAddre, exclude]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [Db, intViewType, 3]),
    false = rpc:call(MaNode, snmpa, int_to_enum,
			   [Db, intAgentIpAddress, 2]),
    false = rpc:call(MaNode, snmpa, int_to_enum,
			   [Db, intAgentIpAddre, 2]),
    {value, active} = rpc:call(MaNode, snmpa, int_to_enum,
				     [Db, 'RowStatus', ?active]),
    {value, ?destroy} = rpc:call(MaNode, snmpa, enum_to_int,
				       [Db, 'RowStatus', destroy]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [Db, 'RowStatus', xxxdestroy]),
    false = rpc:call(MaNode, snmpa, enum_to_int,
			   [Db, 'xxRowStatus', destroy]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [Db, 'RowStatus', 25]),
    false = rpc:call(MaNode, snmpa, int_to_enum, [Db, 'xxRowStatus', 1]),
    ok.


%%-----------------------------------------------------------------
%% Ticket: OTP-4394
%% Slogan: Target mib tag list check invalid
%%-----------------------------------------------------------------

otp_4394_init(Config) when is_list(Config) ->
    ?DBG("otp_4394_init -> entry with"
	   "~n   Config: ~p", [Config]),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir       = ?config(mgr_dir,        Config),
    Ip           = ?config(ip,             Config),
    otp_4394_config(AgentConfDir, MgrDir, Ip),
    MasterAgentVerbosity = {master_agent_verbosity, trace},
    NetIfVerbosity       = {net_if_verbosity,       trace},
    Opts = [MasterAgentVerbosity, NetIfVerbosity],
    [{vsn, v1} | start_v1_agent(Config, Opts)].

otp_4394_config(AgentConfDir, MgrDir, Ip0) ->
    ?DBG("otp_4394_config -> entry with"
	   "~n   AgentConfDir: ~p"
	   "~n   MgrDir:       ~p"
	   "~n   Ip0:          ~p", [AgentConfDir, MgrDir, Ip0]),
    Vsn = [v1],
    Ip = tuple_to_list(Ip0), 
    snmp_config:write_agent_snmp_files(AgentConfDir, Vsn, Ip,
					     ?TRAP_UDP, Ip, 4000, 
					     "OTP-4394 test"),
    case update_usm(Vsn, AgentConfDir) of
	true ->
	    copy_file(join(AgentConfDir, "usm.conf"),
			    join(MgrDir, "usm.conf")),
	    update_usm_mgr(Vsn, MgrDir);
	false ->
	    ok
    end,
    C1 = {"a", "all-rights", "initial", "", "pc"},
    C2 = {"c", "secret", "secret_name", "", "secret_tag"},
    write_community_conf(AgentConfDir, [C1, C2]),
    update_vacm(Vsn, AgentConfDir),
    Ta1 = {"shelob v1", 
	   [134,138,177,177], 5000, 1500, 3, %% Use Ip and modify
	   "pc1", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0], 
	   [],
	   2048},
    Ta2 = {"bifur v1", 
	   [134,138,177,75], 5000, 1500, 3, %% Use Ip
	   "pc2", 
	   "target_v1", "", 
	   %% [255,255,255,255,0,0],
	   [], 2048},
    write_target_addr_conf(AgentConfDir, [Ta1, Ta2]),
    write_target_params_conf(AgentConfDir, Vsn),
    write_notify_conf(AgentConfDir),
    ok.
    
otp_4394_finish(Config) when is_list(Config) ->
    ?DBG("finish_otp_4394 -> entry", []),
    C1 = stop_agent(Config),
    delete_files(C1),
    erase(mgr_node),
    lists:keydelete(vsn, 1, C1).

otp_4394(Config) ->
    ?P(otp_4394), 
    ?DBG("otp_4394 -> entry", []),
    init_case(Config),
    try_test(otp_4394_test),
    ?DBG("otp_4394 -> done", []),
    ok.

otp_4394_test() ->
    ?DBG("otp_4394_test -> entry", []),
    gn([[1,1]]),
    Res = 
	case snmp_test_mgr:expect(1, [{[sysDescr,0],  "Erlang SNMP agent"}]) of
	    {error, 1, _, {_, [timeout]}} ->
		?DBG("otp_4394_test -> expected result: timeout", []),
		ok;
	    Else ->
		Else
	end,
    ?DBG("otp_4394_test -> done with: ~p", [Res]),
    Res.


%%-----------------------------------------------------------------
%% Ticket: OTP-7157
%% Slogan: Target mib tag list check invalid
%%-----------------------------------------------------------------

otp_7157_init(Config) when is_list(Config) ->
    %% <CONDITIONAL-SKIP>
    Skippable = [win32],
    Condition = fun() -> ?OS_BASED_SKIP(Skippable) end,
    ?NON_PC_TC_MAYBE_SKIP(Config, Condition),
    %% </CONDITIONAL-SKIP>

    ?DBG("init_otp_7157 -> entry with"
	   "~n   Config: ~p", [Config]),
    AgentConfDir = ?config(agent_conf_dir, Config),
    MgrDir = ?config(mgr_dir, Config),
    Ip = ?config(ip, Config),
    config([v2], MgrDir, AgentConfDir,
		 tuple_to_list(Ip), tuple_to_list(Ip)),
    MasterAgentVerbosity = {master_agent_verbosity, trace},
    NetIfVerbosity       = {net_if_verbosity,       trace},
    Opts = [MasterAgentVerbosity, NetIfVerbosity],
    [{vsn, v2} | start_v2_agent(Config, Opts)].


otp_7157_finish(Config) when is_list(Config) ->
    ?DBG("finish_otp_7157 -> entry", []),
    C1 = stop_agent(Config),
    delete_files(C1),
    erase(mgr_node),
    lists:keydelete(vsn, 1, C1).

otp_7157(Config) ->
    ?P(otp_7157), 
    ?DBG("otp_7157 -> entry", []),
    init_case(Config),
    MA = whereis(snmp_master_agent),
    load_master("Test1"),
    try_test(otp_7157_test, [MA]),
    unload_master("Test1"),
    ?DBG("otp_7157 -> done", []),
    ok.

%% ts:run(snmp, snmp_agent_test, [batch]).
otp_7157_test(MA) ->
    ?IPRINT("start otp_7157_test test (~p)", [MA]),
    snmpa:verbosity(MA, trace),
    ?IPRINT("start otp_7157_test test"),

    ?NPRINT("Testing that varbinds in traps/notifications are not reordered"),
    
    ?DBG("send cntTrap",[]),
    snmpa:send_trap(MA, cntTrap, "standard trap"),

    ?DBG("await response",[]),
    %% We don't really care about the values, just the vb order.
    ok = ?expect2(v2trap, [{[sysUpTime,   0], any},
				 {[snmpTrapOID, 0], any},
				 {[sysContact,  0], any},
				 {[cnt64,       0], any},
				 {[sysLocation, 0], any}]),

    ?DBG("done", []),
    ok.



%%-----------------------------------------------------------------
%% Ticket: OTP-16092
%% Slogan: Extra socket options
%%         We perform simple start and stop tests with and without
%%         this option.
%%-----------------------------------------------------------------

otp16092_cases() ->
    [
     otp_16092_simple_start_and_stop1, % default
     otp_16092_simple_start_and_stop2, % []
     otp_16092_simple_start_and_stop3, % bad => ignored
     otp_16092_simple_start_and_stop4  % invalid content
    ].

otp_16092_simple_start_and_stop1(Config) ->
    otp_16092_simple_start_and_stop(?FUNCTION_NAME,
                                    Config, default, success).


otp_16092_simple_start_and_stop2(Config) ->
    otp_16092_simple_start_and_stop(?FUNCTION_NAME,
                                    Config, [], success).


otp_16092_simple_start_and_stop3(Config) ->
    otp_16092_simple_start_and_stop(?FUNCTION_NAME,
                                    Config,
                                    'this-should-be-ignored',
                                    success).


otp_16092_simple_start_and_stop4(Config) ->
    otp_16092_simple_start_and_stop(?FUNCTION_NAME,
                                    Config,
                                    ['this-should-fail'],
                                    failure).

otp_16092_simple_start_and_stop(CaseName, Config, ESO, Expected)
  when is_atom(CaseName) ->
    Pre  = fun() ->
                   [D|_] = lists:reverse(atom_to_list(CaseName)),
                   Digit = [D],
                           
                   ?NPRINT("try start agent node"),
                   {ok, Peer, Node} = ?START_PEER(Digit ++ "-agent"),

                   ?NPRINT("try write config to file"),
                   ConfDir   = ?config(agent_conf_dir, Config),
                   DbDir     = ?config(agent_db_dir,   Config),
                   Vsns      = [v1],
                   IP        = tuple_to_list(?config(ip, Config)),
                   ManagerIP = IP,
                   TrapPort  = ?TRAP_UDP,
                   AgentIP   = IP,
                   AgentPort = 4000,
                   SysName   = "test",
                   ok = snmp_config:write_agent_snmp_files(ConfDir, Vsns,
                                                           ManagerIP, TrapPort,
                                                           AgentIP, AgentPort,
                                                           SysName),

                   #{eso       => ESO,
                     vsns      => Vsns,
                     db_dir    => DbDir,
                     conf_opts => [{dir,        ConfDir},
                                   {force_load, false}, 
                                   {verbosity,  trace}],
                     node      => Node,
                     peer      => Peer,
                     expected  => Expected}
           end,
    Case = fun(State) ->
                   do_otp_16092_simple_start_and_stop(State)
           end,
    Post = fun(#{node := Node,
                 peer := Peer}) ->
                   ?NPRINT("try stop agent node ~p", [Node]),
                   ?STOP_PEER(Peer)
           end,
    ?TC_TRY(CaseName, Pre, Case, Post).

do_otp_16092_simple_start_and_stop(#{eso       := ESO,
                                     vsns      := Vsns,
                                     db_dir    := DbDir,
                                     conf_opts := ConfOpts,
                                     node      := Node,
                                     expected  := Expected} = _State) ->
    NiOpts =
        case ESO of
            default ->
                [{verbosity, trace}];
            _ -> 
                [{verbosity, trace}, {options, [{extra_sock_opts, ESO}]}]
        end,

    Opts = [{agent_type, master},
            {versions,   Vsns},
            {db_dir,     DbDir},
            {config,     ConfOpts},
            {net_if,     NiOpts}],

    otp16092_try_start_and_stop_agent(Node, Opts, Expected),

    ?NPRINT("done"),
    ok.

otp16092_try_start_and_stop_agent(Node, Opts, Expected) ->
    ?IPRINT("try start snmp (agent) supervisor (on ~p) - expect ~p", 
            [Node, Expected]),
    case start_standalone_agent(Node, Opts) of
        {ok, Pid} when is_pid(Pid) andalso (Expected =:= success) ->
            ?IPRINT("Expected success starting snmp (agent) supervisor"),
            ?SLEEP(1000),
            stop_standalone_agent(Pid),
            ok;
        {ok, Pid} when is_pid(Pid) andalso (Expected =:= failure) ->
            ?EPRINT("Unexpected success starting snmp (agent) supervisor: (~p)",
                    [Pid]),
            ?SLEEP(1000),
            stop_standalone_agent(Pid),
            ?FAIL('unexpected-start-success');

        {error,
         shutdown,
         {failed_to_start_child, snmpa_agent_sup,
          {shutdown,
           {failed_to_start_child, snmpa_agent,
            {net_if_error, Reason}}}}} when (Expected =:= failure) ->
            ?IPRINT("Expected (shutdown, net-if) error starting "
                    "snmp (agent) supervisor (on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;
        {error, {shutdown, Reason}} when (Expected =:= failure) ->
            ?IPRINT("Expected (shutdown) error starting "
                    "snmp (agent) supervisor (on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;
        {error, Reason} when (Expected =:= failure) ->
            ?IPRINT("Expected error starting snmp (agent) supervisor (on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;

        {badrpc, 
         {'EXIT',
          {shutdown,
           {failed_to_start_child, snmpa_agent_sup,
            {shutdown,
             {failed_to_start_child, snmpa_agent,
              {net_if_error, Reason}}}}}}} when (Expected =:= failure) ->
            ?IPRINT("Expected (badrpc, shutdown, net-if) error starting "
                    "snmp (agent) supervisor (on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;
        {badrpc, {'EXIT', {shutdown, Reason}}} when (Expected =:= failure) ->
            ?IPRINT("Expected (badrpc, shutdown) error starting "
                    "snmp (agent) supervisor (on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;
        {badrpc, {'EXIT', Reason}} when (Expected =:= failure) ->
            ?IPRINT("Expected (badrpc) error starting snmp (agent) supervisor "
                      "(on ~p):"
                    "~n   ~p", [Node, Reason]),
            ok;

        {badrpc, Reason} = BADRPC ->
            ?WPRINT("Bad RPC to node ~p failed:"
                    "~n   ~p", [Node, Reason]),
            ?SKIP({BADRPC, Node})

    end,
    ok.



%%-----------------------------------------------------------------
%% Extra test cases
%% These cases are started in the new way
%%-----------------------------------------------------------------

otp8395({init, Config}) when is_list(Config) ->
    ?DBG("otp8395(init) -> entry with"
	 "~n   Config: ~p", [Config]),

    %% --
    %% Start nodes
    %%
    {ok, AgentPeer, AgentNode}     = ?START_PEER("-agent"),
    {ok, ManagerPeer, ManagerNode} = ?START_PEER("-manager"),

    %% -- 
    %% Mnesia init
    %% 

    AgentDbDir = ?config(agent_db_dir, Config),
    AgentMnesiaDir = join([AgentDbDir, "mnesia"]),
    mnesia_init(AgentNode, AgentMnesiaDir),

    mnesia_create_schema(AgentNode, [AgentNode]),

    mnesia_start(AgentNode),

    %% --
    %% Host & IP
    %% 

    AgentHost    = ?HOSTNAME(AgentNode),
    %% SubAgentHost = ?HPSTNAME(SubAgentNode), 
    ManagerHost  = ?HOSTNAME(ManagerNode),

    IpFamily          = inet,
    Host              = snmp_test_lib:hostname(), 
    Ip                = ?LOCALHOST(),
    {ok, AgentIP0}    = snmp_misc:ip(AgentHost),
    AgentIP           = tuple_to_list(AgentIP0), 
    %% {ok, SubAgentIP0} = snmp_misc:ip(SubAgentHost),
    %% SubAgentIP        = tuple_to_list(SubAgentIP0), 
    {ok, ManagerIP0}  = snmp_misc:ip(ManagerHost),
    ManagerIP         = tuple_to_list(ManagerIP0),


    %% --
    %% Write agent config
    %% 

    Vsns           = [v1], 
    AgentConfDir   = ?config(agent_conf_dir, Config),
    ManagerConfDir = ?config(manager_top_dir, Config),
    config(Vsns, ManagerConfDir, AgentConfDir, ManagerIP, AgentIP, IpFamily),


    %% --
    %% Start the agent
    %% 

    Config2 = start_agent([{host,          Host}, 
			   {ip,            Ip}, 
			   {ipfamily,      IpFamily},
			   {agent_peer,    AgentPeer},
			   {agent_node,    AgentNode}, 
			   {agent_host,    AgentHost}, 
			   {agent_ip,      AgentIP}, 
			   %% {sub_agent_node, SubAgentNode}, 
			   %% {sub_agent_host, SubAgentHost}, 
			   %% {sub_agent_ip,   SubAgentIP},
			   {manager_peer,  ManagerPeer},
			   {manager_node,  ManagerNode},
			   {manager_host,  ManagerHost}, 
			   {manager_ip,    ManagerIP}|Config]),

    %% -- 
    %% Create watchdog 
    %% 

    wd_start(1, Config2);

otp8395({fin, Config}) when is_list(Config) ->
    ?DBG("otp8395(fin) -> entry with"
	 "~n   Config: ~p", [Config]),

    AgentNode   = ?config(agent_node, Config),

    %% -
    %% Stop agent (this is the nice way to do it, 
    %% so logs and files can be closed in the proper way).
    %% 

    AgentSup = ?config(agent_sup, Config),
    ?DBG("otp8395(fin) -> stop (stand-alone) agent: ~p", [AgentSup]),
    stop_standalone_agent(AgentSup), 

    %% - 
    %% Stop mnesia
    %% 
    ?DBG("otp8395(fin) -> stop mnesia", []),
    mnesia_stop(AgentNode),


    %% - 
    %% Stop the agent node
    %% 

    ?DBG("otp8395(fin) -> stop agent node", []),
    ?STOP_PEER(?config(agent_peer, Config)),

    %% - 
    %% Stop the manager node
    %% 

    ?DBG("otp8395(fin) -> stop manager node", []),
    ?STOP_PEER(?config(manager_peer, Config)),

    wd_stop(Config);

otp8395(doc) ->
    "OTP-8395 - ATL with sequence numbering. ";

otp8395(Config) when is_list(Config) ->
    ?DBG("otp8395 -> entry with"
	 "~n   Config: ~p", [Config]),

    ?SLEEP(1000),

    %% This is just to dirty trick for the ***old*** test-code
    put(mgr_node, ?config(manager_node, Config)), 
    put(mgr_dir,  ?config(manager_top_dir, Config)),
    put(mib_dir,  ?config(mib_dir, Config)),
    put(vsn, v1), 
    put(master_host, ?config(agent_host, Config)),
    put(ipfamily, ?config(ipfamily, Config)),
    try_test(simple_standard_test),

    ?SLEEP(1000),
    AgentNode   = ?config(agent_node, Config),
    AgentLogDir = ?config(agent_log_dir, Config),
    OutFile     = join([AgentLogDir, "otp8395.txt"]),
    {ok, _LogInfo} = rpc:call(AgentNode, snmpa, log_info, []),
    ?DBG("otp8395 -> LogInfo: ~p", [_LogInfo]), 

    %% SyncRes = rpc:call(AgentNode, snmp, log_sync, [?audit_trail_log_name]),
    %% ?DBG("otp8395 -> SyncRes: ~p", [SyncRes]), 

    ok = agent_log_validation(AgentNode),
    _LTTRes = 
	rpc:call(AgentNode, snmpa, log_to_txt, [AgentLogDir, [], OutFile]), 
    ?DBG("otp8395 -> LTTRes: ~p", [_LTTRes]), 

    ?SLEEP(1000),
    ?DBG("otp8395 -> done", []),
    ok.


%%-----------------------------------------------------------------

otp9884({init, Config}) when is_list(Config) ->
    ?DBG("otp9884(init) -> entry with"
	 "~n   Config: ~p", [Config]),
    init_v1_agent([{fname, otp9884}, {ipfamily, inet} | Config]);

otp9884({fin, Config}) when is_list(Config) ->
    ?DBG("otp9884(fin) -> entry with"
	 "~n   Config: ~p", [Config]),
    fin_v1_agent(Config);

otp9884(doc) ->
    "OTP-9884 - Simultaneous backup(s) call should not work. ";

otp9884(Config) when is_list(Config) ->
    ?DBG("otp9884 -> entry with"
	 "~n   Config: ~p", [Config]),

    AgentNode = ?config(agent_node, Config),
    [AgentBkpDir1, AgentBkpDir2] = ?config(agent_backup_dirs, Config),
    Self = self(), 
    timer:apply_after(1000,
		      ?MODULE,
                      otp9884_backup,
                      [AgentNode, Self, first,  AgentBkpDir1]),
    timer:apply_after(1000,
		      ?MODULE,
                      otp9884_backup,
                      [AgentNode, Self, second, AgentBkpDir2]),
    otp9884_await_backup_started(),
    otp9884_await_backup_completion(undefined, undefined),

    ?DBG("otp9884 -> done", []),
    ok.


otp9884_backup(Node, Pid, Tag, Dir) ->
    ?IPRINT("[~w] backup - await continue", [Tag]),
    Pid ! {otp9884_backup_started, Tag, self()},
    receive
        {otp9884_backup_continue, Tag, Pid} ->
            ok
    end,
    ?IPRINT("[~w] backup start", [Tag]),
    Res = rpc:call(Node, snmpa, backup, [Dir]),
    ?IPRINT("[~w] backup result: ~p", [Tag, Res]),
    Pid ! {otp9884_backup_complete, Tag, Res}.


otp9884_await_backup_started() ->
    otp9884_await_backup_started(undefined, undefined).

otp9884_await_backup_started(First, Second)
  when is_pid(First) andalso is_pid(Second) ->
    ?IPRINT("otp9884_await_backup_started -> order first continue"),
    First  ! {otp9884_backup_continue, first, self()},
    ?IPRINT("otp9884_await_backup_started -> order second continue"),
    Second ! {otp9884_backup_continue, second, self()},
    ok;
otp9884_await_backup_started(First, Second) ->
    receive
        {otp9884_backup_started, first, Pid} when (First =:= undefined) ->
            ?IPRINT("otp9884_await_backup_started -> received started from first"),
            otp9884_await_backup_started(Pid, Second);
        {otp9884_backup_started, second, Pid} when (Second =:= undefined) ->
            ?IPRINT("otp9884_await_backup_started -> received started from second"),
            otp9884_await_backup_started(First, Pid)
    end.
    
otp9884_await_backup_completion(ok, Second) 
  when ((Second =/= ok) andalso (Second =/= undefined)) ->
    ?IPRINT("otp9884_await_backup_completion -> "
            "first backup succeed and second failed (~p)", [Second]),
    ok;
otp9884_await_backup_completion(First, ok) 
  when ((First =/= ok) andalso (First =/= undefined)) ->
    ?IPRINT("otp9884_await_backup_completion -> "
            "second backup succeed and first failed (~p)", [First]),
    ok;
otp9884_await_backup_completion(First, Second) 
  when (((First =:= undefined) andalso (Second =:= undefined)) 
	orelse 
	((First =:= undefined) andalso (Second =/= undefined)) 
	orelse 
	((First =/= undefined) andalso (Second =:= undefined))) ->
    ?IPRINT("otp9884_await_backup_completion -> await complete messages"),
    receive
	{otp9884_backup_complete, first, Res} ->
	    ?IPRINT("otp9884_await_backup_completion -> "
                    "received complete message for first: ~p", [Res]),
	    otp9884_await_backup_completion(Res, Second);
	{otp9884_backup_complete, second, Res} ->
	    ?IPRINT("otp9884_await_backup_completion -> "
                    "received complete message for second: ~p", [Res]),
	    otp9884_await_backup_completion(First, Res)
    after 10000 ->
	    %% we have waited long enough
            ?EPRINT("otp9884_await_backup_completion -> timeout"),
	    throw({error, {timeout, First, Second}})
    end;
otp9884_await_backup_completion(First, Second) ->
    ?EPRINT("Bad Completion: "
            "~n      First:  ~p"
            "~n      Second: ~p", [First, Second]),
    throw({error, {bad_completion, First, Second}}).


%%-----------------------------------------------------------------

otp16649_1_init(Config) ->
    AgentPreTransports = [{4000, req_responder},
                          {4001, trap_sender}],
    otp16649_init(1, AgentPreTransports, Config).

otp16649_1_fin(Config) ->
    otp16649_fin(1, Config).

otp16649_1(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_1(Config) when is_list(Config) ->
    otp16649(1, Config).


otp16649_2_init(Config) ->
    AgentPreTransports = [{4000,   req_responder},
                          {system, trap_sender}],
    otp16649_init(2, AgentPreTransports, Config).

otp16649_2_fin(Config) ->
    otp16649_fin(2, Config).

otp16649_2(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_2(Config) when is_list(Config) ->
    otp16649(2, Config).


otp16649_3_init(Config) ->
    AgentPreTransports = [{4000, req_responder},
                          {0,    trap_sender}],
    otp16649_init(3, AgentPreTransports, Config).

otp16649_3_fin(Config) ->
    otp16649_fin(3, Config).

otp16649_3(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_3(Config) when is_list(Config) ->
    otp16649(3, Config).


otp16649_4_init(Config) ->
    AgentPreTransports = [{4000,        req_responder},
                          {{4000,4010}, trap_sender, [{no_reuse, true}]}],
    otp16649_init(4, AgentPreTransports, Config).

otp16649_4_fin(Config) ->
    otp16649_fin(4, Config).

otp16649_4(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_4(Config) when is_list(Config) ->
    otp16649(4, Config).


otp16649_5_init(Config) ->
    AgentPreTransports = [{4000,        req_responder},
                          {[{4000,4010}], trap_sender, [{no_reuse, true}]}],
    otp16649_init(5, AgentPreTransports, Config).

otp16649_5_fin(Config) ->
    otp16649_fin(5, Config).

otp16649_5(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_5(Config) when is_list(Config) ->
    otp16649(5, Config).


otp16649_6_init(Config) ->
    AgentPreTransports = [{4000,        req_responder},
                          {[4000,4001,4002], trap_sender, [{no_reuse, true}]}],
    otp16649_init(6, AgentPreTransports, Config).

otp16649_6_fin(Config) ->
    otp16649_fin(5, Config).

otp16649_6(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_6(Config) when is_list(Config) ->
    otp16649(6, Config).


otp16649_7_init(Config) ->
    AgentPreTransports = [{4000,        req_responder},
                          {[4000,{5100,5110}], trap_sender,
                           [{no_reuse, true}]}],
    otp16649_init(7, AgentPreTransports, Config).

otp16649_7_fin(Config) ->
    otp16649_fin(7, Config).

otp16649_7(doc) ->
    "OTP-16649 - Multiple transports.";
otp16649_7(Config) when is_list(Config) ->
    otp16649(7, Config).


otp16649(N, Config) ->
    ?IPRINT("otp16649 -> entry with"
            "~n   N:      ~w"
            "~n   Config: ~p", [N, Config]),

    AgentNode   = ?config(agent_node, Config),
    ManagerNode = ?config(manager_node, Config),

    AInfo = rpc:call(AgentNode, snmpa, info, []),

    ?IPRINT("Agent Info: "
            "~n      ~p", [AInfo]),

    {value, {_, AgentRawTransports}} =
        lists:keysearch(agent_raw_transports, 1, Config),
    {value, {_, NetIF}}                =
        lists:keysearch(net_if, 1, AInfo),
    {value, {_, TIs}}                =
        lists:keysearch(transport_info, 1, NetIF),
    
    if (length(AgentRawTransports) =:= length(TIs)) ->
            ok;
       true ->
            ?IPRINT("Invalid transports: "
                    "~n   Number of raw transports: ~w"
                    "~n   Number of transports:     ~w",
                    [length(AgentRawTransports), length(TIs)]),
            exit({invalid_num_transports,
                  length(AgentRawTransports), length(TIs)})
    end,

    ?IPRINT("validate transports"),
    otp16649_validate_transports(AgentRawTransports, TIs),

    ?IPRINT("which req-responder port-no"),
    AgentReqPortNo = otp16649_which_req_port_no(TIs),

    ?IPRINT("which trap-sender port-no"),
    AgentTrapPortNo = otp16649_which_trap_port_no(TIs),

    ?IPRINT("(mgr) register user"),
    ok = otp16649_mgr_reg_user(ManagerNode),

    ?IPRINT("(mgr) register agent"),
    TargetBase = "otp16649-agent-",
    ReqTarget  = TargetBase ++ "req",
    TrapTarget = TargetBase ++ "trap",

    ok = otp16649_mgr_reg_agent(ManagerNode,
                                      ?config(ipfamily, Config),
                                      ?config(tdomain, Config),
                                      ReqTarget, AgentReqPortNo),
    ok = otp16649_mgr_reg_agent(ManagerNode,
                                      ?config(ipfamily, Config),
                                      ?config(tdomain, Config),
                                      TrapTarget, AgentTrapPortNo),

    ?IPRINT("(mgr) simple (sync) get request"),
    Oids     = [?sysObjectID_instance, ?sysDescr_instance, ?sysUpTime_instance],
    ok = case otp16649_mgr_get_req(ManagerNode, Oids) of
                   {ok, {noError, 0, ReplyOids}, _} ->
                       ?IPRINT("(mgr) simple (sync) successful reply: "
                               "~n      ~p", [ReplyOids]),
                       ok;
                   {ok, InvalidReply, _} ->
                       ?IPRINT("(mgr) simple (sync) invalid reply: "
                               "~n      ~p", [InvalidReply]),
                       ok;
                   {error, Reason} ->
                       ?IPRINT("(mgr) simple (sync) error: "
                               "~n      ~p", [Reason]),
                       error
               end,

    ?IPRINT("load TestTrap..."), 
    MibDir = ?config(mib_dir, Config),
    ok = otp16649_agent_load_mib(AgentNode, MibDir, "TestTrap"),

    ?IPRINT("(agent) send trap (testTrap2)"),
    ok = otp16649_agent_send_trap(AgentNode, testTrap2),

    TDomain = ?config(tdomain, Config),

    receive
        {handle_trap, From_v1, TrapTarget,
         {?system, 6, 1, _, Vbs_v1}}
        when is_pid(From_v1) andalso
             is_list(Vbs_v1) andalso
             (TDomain =:= transportDomainUdpIpv4) ->
            ?IPRINT("received expected (v1) handle trap callback message: "
                    "~n      ~p", [Vbs_v1]),
            ok

    after 5000 ->
            case TDomain of
                transportDomainUdpIpv4 ->
                    ?IPRINT("TIMEOUT"),
                    exit(timeout);
                transportDomainUdpIpv6 ->
                    ?IPRINT("expected timeout - "
                            "v1 trap's can only be sent on IPv4 domains"),
                    ok
            end
    end,


    ?IPRINT("load TestTrapv2..."), 
    ok = otp16649_agent_load_mib(AgentNode, MibDir, "TestTrapv2"),

    ?IPRINT("(agent) send trap (testTrapv22)"),
    ok = otp16649_agent_send_trap(AgentNode, testTrapv22),

    receive
        {handle_trap, From_v2, TrapTarget,
         {noError, 0, Vbs_v2}} when is_pid(From_v2) andalso
                                    is_list(Vbs_v2) ->
            ?IPRINT("received expected (v2) handle trap callback message: "
                    "~n      ~p", [Vbs_v2]),
            ok

    after 5000 ->
            ?IPRINT("TIMEOUT"),
            exit(timeout)
    end,

    ?IPRINT("done"),
    ok.


otp16649_init(N, AgentPreTransports, Config) ->
    ?IPRINT("otp16649_init -> entry with"
            "~n   N:                  ~w"
            "~n   AgentPreTransports: ~w"
            "~n   Config:             ~p", [N, AgentPreTransports, Config]),

    %% --
    %% Start nodes
    %%

    {ok, AgentPeer, AgentNode} = ?START_PEER("-agent"),
    {ok, ManagerPeer, ManagerNode} = ?START_PEER("-manager"),

    %% --
    %% Misc
    %%

    AgentHost         = ?HOSTNAME(AgentNode),
    ManagerHost       = ?HOSTNAME(ManagerNode),

    ?IPRINT("otp16649_init -> "
            "~n      AgentHost:   ~p"
            "~n      ManagerHost: ~p",
            [AgentHost, ManagerHost]),

    Host              = snmp_test_lib:hostname(), 
    Ip                = ?config(ip, Config),
    %% We should really "extract" the address from the hostnames,
    %% but because on some OSes (Ubuntu) adds 12.7.0.1.1 to its hosts file,
    %% this does not work. We want a "proper" address.
    %% Also, since both nodes (agent and manager) are both started locally,
    %% we can use 'Ip' for both!
    %% {ok, AgentIP}     = snmp_misc:ip(AgentHost),
    %% {ok, ManagerIP0}  = snmp_misc:ip(ManagerHost),
    AgentIP           = Ip,
    ManagerIP0        = Ip,
    ManagerIP         = tuple_to_list(ManagerIP0),
    ?IPRINT("otp16649_init -> "
            "~n      Host:       ~p"
            "~n      Ip:         ~p"
            "~n      AgentIP:    ~p"
            "~n      ManagerIP0: ~p"
            "~n      ManagerIP:  ~p",
            [Host, Ip, AgentIP, ManagerIP0, ManagerIP]),


    %% --
    %% Write agent config
    %% 

    AgentConfDir        = ?config(agent_conf_dir, Config),
    Vsns                = [v1,v2],
    TransportDomain     = ?config(tdomain, Config),
    F = fun({PortInfo, Kind}) ->
                #{addr => {AgentIP, PortInfo}, kind => Kind};
           ({PortInfo, Kind, Opts}) ->
                #{addr => {AgentIP, PortInfo}, kind => Kind, opts => Opts}
        end,
    AgentPreTransports2 = [F(T) || T <- AgentPreTransports],

    ?IPRINT("write agent config files"),
    ok = snmp_config:write_agent_snmp_files(
                 AgentConfDir, Vsns,
                 TransportDomain, {ManagerIP, ?MGR_PORT}, AgentPreTransports2,
                 "test"),

    ?IPRINT("start agent"),
    Config2 = start_agent([{host,          Host},
			   {agent_peer,    AgentPeer},
			   {agent_node,    AgentNode},
			   {agent_host,    AgentHost},
			   {agent_ip,      AgentIP},
			   {manager_peer,  ManagerPeer},
			   {manager_node,  ManagerNode},
			   {manager_host,  ManagerHost},
			   {manager_ip,    ManagerIP}|Config]),



    %% --
    %% Write manager config
    %%

    ?IPRINT("create manager dirs"),
    MgrTopDir  = ?config(manager_top_dir, Config),
    MgrDbDir   = filename:join(MgrTopDir,  "db/"),
    MgrConfDir = filename:join(MgrTopDir,  "conf/"),
    ok   = file:make_dir(MgrConfDir),
    MgrDbDir   = filename:join(MgrTopDir,  "db/"),
    ok   = file:make_dir(MgrDbDir),
    MgrLogDir  = filename:join(MgrTopDir,  "log/"),
    ok   = file:make_dir(MgrLogDir),

    ?IPRINT("write manager config files"),
    MgrTransports = [{TransportDomain, {ManagerIP0, ?MGR_PORT}}],
    ok = snmp_config:write_manager_snmp_files(
                 MgrConfDir,
                 MgrTransports,
                 ?MGR_MMS, ?MGR_ENGINE_ID),
    
    Config3 = [{manager_db_dir,   MgrDbDir},
               {manager_conf_dir, MgrConfDir},
               {manager_log_dir,  MgrLogDir} | Config2],
               
    ?IPRINT("start manager"),
    ok = start_manager(Config3),

    ?DBG("otp16649_init -> done when"
         "~n   Config2: ~p", [Config3]),
    [{agent_raw_transports, AgentPreTransports} | Config3].


otp16649_fin(N, Config) when is_integer(N) ->
    ?IPRINT("otp16649_fin -> entry with"
            "~n   N:      ~p"
            "~n   Config: ~p", [N, Config]),

    ManagerNode = ?config(manager_node, Config),

    %% -
    %% Stop agent (this is the nice way to do it, 
    %% so logs and files can be closed in the proper way).
    %%

    AgentTopSup = ?config(agent_sup, Config),
    stop_standalone_agent(AgentTopSup),


    %% -
    %% Stop manager
    %%

    stop_standalone_manager(ManagerNode),


    %%
    %% Stop the manager node
    %%

    ?DBG("otp16649_fin -> stop manager node", []),
    peer:stop(?config(manager_peer, Config)),


    %%
    %% Stop the agent node
    %%

    ?DBG("otp16649_fin -> stop agent node", []),
    peer:stop(?config(agent_peer, Config)),

    ?DBG("otp16649_fin -> done", []),
    Config1 = lists:keydelete(manager_node, 1, Config),
    lists:keydelete(agent_node, 1, Config1).


otp16649_validate_transports([], []) ->
    ok;
otp16649_validate_transports([AgentRawTransport|AgentRawTransports],
                             [TI|TIs]) ->
    ?IPRINT("validate transport:"
            "~n   AgentRawTransport: ~p"
            "~n   TI:                ~p",  [AgentRawTransport, TI]),
    otp16649_validate_transport(AgentRawTransport, TI),
    otp16649_validate_transports(AgentRawTransports, TIs).

otp16649_validate_transport({PortInfo, Kind}, #{taddress       := {_, PortNo},
                                                transport_kind := Kind}) ->
    ?IPRINT("validate ~w transport:"
            "~n   PortNo:   ~w"
            "~n   PortInfo: ~p",  [Kind, PortNo, PortInfo]),
    otp16649_validate_port(PortInfo, PortNo);
otp16649_validate_transport({_, ConfKind}, #{taddress       := {_, PortNo},
                                             transport_kind := ActualKind}) ->
    exit({invalid_transport_kind, {PortNo, ConfKind, ActualKind}});
otp16649_validate_transport({PortInfo, Kind, _}, #{taddress       := {_, PortNo},
                                                   transport_kind := Kind}) ->
    ?IPRINT("validate ~w transport:"
            "~n   PortNo:   ~w"
            "~n   PortInfo: ~p",  [Kind, PortNo, PortInfo]),
    otp16649_validate_port(PortInfo, PortNo);
otp16649_validate_transport({_, ConfKind, _}, #{taddress       := {_, PortNo},
                                                transport_kind := ActualKind}) ->
    exit({invalid_transport_kind, {PortNo, ConfKind, ActualKind}}).

otp16649_validate_port(PortNo, PortNo) when is_integer(PortNo) ->
    ok;
otp16649_validate_port(0, PortNo) when is_integer(PortNo) ->
    ok;
otp16649_validate_port(system, PortNo) when is_integer(PortNo) ->
    ok;
otp16649_validate_port(Range, PortNo) when is_tuple(Range) ->
    case otp16649_validate_port_range(Range, PortNo) of
        ok ->
            ok;
        error ->
            exit({invalid_transport_port_no, {Range, PortNo}})
    end;
otp16649_validate_port(Ranges, PortNo) when is_list(Ranges) ->
    case otp16649_validate_port_ranges(Ranges, PortNo) of
        ok ->
            ok;
        error ->
            exit({invalid_transport_port_no, {Ranges, PortNo}})
    end.


otp16649_validate_port_range({Min, Max}, PortNo)
  when is_integer(Min) andalso
       is_integer(Max) andalso
       is_integer(PortNo) andalso
       (Min =< PortNo) andalso
       (PortNo =< Max) ->
    ok;
otp16649_validate_port_range(_Range, _PortNo) ->
    error.

otp16649_validate_port_ranges([], _PortNo) ->
    error;
otp16649_validate_port_ranges([PortNo|_], PortNo) when is_integer(PortNo) ->
    ok;
otp16649_validate_port_ranges([Range|Ranges], PortNo) when is_tuple(Range) ->
    case otp16649_validate_port_range(Range, PortNo) of
        ok ->
            ok;
        error ->
            otp16649_validate_port_ranges(Ranges, PortNo)
    end;
otp16649_validate_port_ranges([_|Ranges], PortNo) ->
    otp16649_validate_port_ranges(Ranges, PortNo).


otp16649_which_req_port_no(TIs) ->
    ?IPRINT("otp16649_which_req_port_no -> entry with"
            "~n      TIs: ~p", [TIs]),
    otp16649_which_port_no(TIs, req_responder).

otp16649_which_trap_port_no(TIs) ->
    ?IPRINT("otp16649_which_trap_port_no -> entry with"
            "~n      TIs: ~p", [TIs]),
    otp16649_which_port_no(TIs, trap_sender).

otp16649_which_port_no([], Kind) ->
    exit({no_transport_port_no, Kind});
otp16649_which_port_no([#{taddress       := {_, PortNo},
                          transport_kind := Kind}|_], Kind) ->
    PortNo;
otp16649_which_port_no([_|TIs], Kind) ->
    otp16649_which_port_no(TIs, Kind).


otp16649_mgr_reg_user(Node) ->
    rpc:call(Node, snmpm, register_user,
             [otp16649, snmp_otp16649_user, self()]).

otp16649_mgr_reg_agent(Node, IPFam, TDomain, Target, PortNo) ->
    ?IPRINT("otp16649_mgr_reg_agent -> entry with"
            "~n      Node:    ~p"
            "~n      IPFam:   ~p"
            "~n      TDomain: ~p"
            "~n      Target:  ~p"
            "~n      PortNo:  ~p",
            [Node, IPFam, TDomain, Target, PortNo]),
    Localhost  = ?LOCALHOST(IPFam),
    Config     = [{address,   Localhost},
                  {port,      PortNo},
                  {version,   v1},
                  {tdomain,   TDomain},
                  {engine_id, "agentEngine"}],
    rpc:call(Node, snmpm, register_agent,
              [otp16649, Target, Config]).

otp16649_mgr_get_req(Node, Oids) ->
    TargetName = "otp16649-agent-req",
    rpc:call(Node, snmpm, sync_get2, [otp16649, TargetName, Oids]).

otp16649_agent_load_mib(Node, MibDir, Mib) ->
    rpc:call(Node, snmpa, unload_mib, [snmp_master_agent, Mib]), % For safety
    MibPath = join(MibDir, Mib),
    rpc:call(Node, snmpa, load_mib, [snmp_master_agent, MibPath]).

otp16649_agent_send_trap(Node, Trap) ->
    rpc:call(Node, snmpa, send_trap,
             [snmp_master_agent, Trap, "standard trap"]).


%%-----------------------------------------------------------------

agent_log_validation(Node) ->
    rpc:call(Node, ?MODULE, agent_log_validation, []).

agent_log_validation() ->
    put(sname, otp8308),
    put(verbosity, trace),
    snmp_log:validate(?audit_trail_log_name, true).

mnesia_init(Node, Dir) ->
    rpc:call(Node, ?MODULE, mnesia_init, [Dir]).

mnesia_init(Dir) ->
    ok = application:load(mnesia),
    application_controller:set_env(mnesia, dir, Dir).

mnesia_create_schema(Node, Nodes) ->
    rpc:call(Node, mnesia, create_schema, [Nodes]).
    
mnesia_start(Node) ->
    rpc:call(Node, application, start, [mnesia]).

mnesia_start() ->
    application:start(mnesia).

mnesia_stop(Node) ->
    rpc:call(Node, application, stop, [mnesia]).

mnesia_stop() ->
    application:start(mnesia).

    
start_agent(Config) ->
    start_agent(Config, []).

start_agent(Config, Opts) ->

    %% Directories
    ConfDir     = ?config(agent_conf_dir,     Config),
    DbDir       = ?config(agent_db_dir,       Config),
    LogDir      = ?config(agent_log_dir,      Config),
    InetBackend = ?config(socket_create_opts, Config),
 
    Vsns = [v1], 

    AgentConfig = process_agent_options(ConfDir, DbDir, LogDir, Vsns,
                                        InetBackend, Opts),
    
    %% Nodes
    AgentNode = ?config(agent_node, Config),
    
    process_flag(trap_exit, true),

    {ok, AgentTopSup} = start_standalone_agent(AgentNode, AgentConfig),

    [{agent_sup, AgentTopSup} | Config].
    

process_agent_options(ConfDir, DbDir, LogDir, Vsns, InetBackend, Opts) ->
    ?IPRINT("process_agent_options ->"
            "~n   ConfDir:     ~p"
            "~n   DbDir:       ~p"
            "~n   LogDir:      ~p"
            "~n   Vsns:        ~p"
            "~n   InetBackend: ~p"
            "~n   Opts:        ~p",
            [ConfDir, DbDir, LogDir, Vsns, InetBackend, Opts]),
    Defaults = 
	[{agent_type,      master},
	 {agent_verbosity, trace},
	 {priority,        normal},
	 {versions,        Vsns},
	 {db_dir,          DbDir},
	 {mib_storage,     ets},
	 {local_db, [{repair,    true},
		     {auto_save, 5000},
		     {verbosity, log}]},
	 {error_report_module, snmpa_error_logger},
	 {config, [{dir,        ConfDir},
		   {force_load, true},
		   {verbosity,  trace}]},
	 {multi_threaded, true},
	 {mib_server, [{mibentry_override,  false},
		       {trapentry_override, false},
		       {verbosity,          info}]},
	 {target_cache,   [{verbosity,info}]},
	 {symbolic_store, [{verbosity,log}]},
	 {note_store, [{timeout,30000}, {verbosity,log}]},
	 {net_if, [{module,    snmpa_net_if},
		   {verbosity, trace},
		   {options,   InetBackend ++ [{bind_to,   false},
                                               {no_reuse,  false},
                                               {req_limit, infinity}]}]},
	 {audit_trail_log, [{type,   read_write},
			    {dir,    LogDir},
			    {size,   {10240,20}},
			    {repair, true},
			    {seqno,  true}]}],
    
    process_options(Defaults, Opts).

process_options(Defaults, _Opts) ->
    %% process_options(Defaults, Opts, []).
    Defaults.


start_standalone_agent(Node, Config)  ->
    rpc:call(Node, ?MODULE, start_standalone_agent, [Config]).

start_standalone_agent(Config)  ->
    case snmpa_supervisor:start_link(normal, Config) of
        {ok, AgentTopSup} ->
            unlink(AgentTopSup),
            {ok, AgentTopSup};
        {error, {already_started, AgentTopSup}} ->
            {ok, AgentTopSup};
        {error, _} = ERROR ->
            ERROR
    end.

stop_standalone_agent(Pid) when (node(Pid) =/= node()) ->
    MRef = erlang:monitor(process, Pid),
    rpc:call(node(Pid), ?MODULE, stop_standalone_agent, [Pid]),
    receive
	{'DOWN', MRef, process, Pid, _Info} ->
	    ?DBG("received expected DOWN message "
		 "regarding snmp agent supervisor: "
		 "~n   Info: ~p", [_Info]),
	    ok
    after 5000 ->
	    ?DBG("no DOWN message "
		 "regarding snmp agent supervisor within time", []),
	    ok
    end;
stop_standalone_agent(Pid) ->
    ?DBG("attempting to terminate agent top-supervisor: ~p", [Pid]),
    nkill(Pid, kill).


start_manager(Config) ->
    Node    = ?config(manager_node, Config),
    ConfDir = ?config(manager_conf_dir, Config),
    DbDir   = ?config(manager_db_dir, Config),

    Opts = [{server, [{verbosity, trace}]},
	    {net_if, [{verbosity, trace}]},
	    {note_store, [{verbosity, trace}]},
	    {config, [{verbosity, trace}, {dir, ConfDir}, {db_dir, DbDir}]}],
    ok = start_standalone_manager(Node, Opts).


start_standalone_manager(Node, Config) ->
    rpc:call(Node, ?MODULE, start_standalone_manager, [Config]).

start_standalone_manager(Config)  ->
    snmpm:start(Config).


stop_standalone_manager(Node) when (Node =/= node()) ->
    rpc:call(Node, snmpm, stop, []);
stop_standalone_manager(_) ->
    snmpm:stop().


nkill(Pid, Reason) ->
    nkill(Pid, Reason, 10).

nkill(Pid, Reason, N) when N > 0 ->
    case (catch erlang:process_info(Pid)) of
	Info when is_list(Info) ->
	    ?DBG("Info for process to kill: "
		 "~n   Info: ~p", [Info]),
	    exit(Pid, Reason),
	    ?SLEEP(1000),
	    nkill(Pid, Reason, N-1);
	_ ->
	    ?DBG("No process info => already dead?", []),
	    ok
    end.

    
%%-----------------------------------------------------------------
%% Slogan: info test
%%-----------------------------------------------------------------

info_test(Config) when is_list(Config) ->
    ?P(info_test), 
    init_case(Config),

    load_master("OLD-SNMPEA-MIB"),
    init_old(),
    try_test(do_info, [node()]),
    unload_master("OLD-SNMPEA-MIB").
    
do_info(MaNode) ->
    Info = rpc:call(MaNode, snmpa, info, []),
    ?DBG("info_test1 -> Info: ~n~p", [Info]),
    Keys = [vsns, 
	    stats_counters, 
	    {agent, [process_memory, db_memory]}, 
	    {net_if, [process_memory, transport_info, reqs]}, 
	    {note_store, [process_memory, db_memory]}, 
	    {symbolic_store, [process_memory, db_memory]}, 
	    {local_db, [process_memory, db_memory]}, 
	    {mib_server, [process_memory, 
			  loaded_mibs, 
			  subagents, 
			  tree_size_bytes, 
			  db_memory]}], 
    verify_info(Info, Keys),
    ok.

verify_info([], []) ->
    ok;
verify_info([], Keys) ->
    ?FAIL({remaining_info_keys, Keys});
verify_info(Info0, [Key|Keys]) ->
    Info = verify_info1(Info0, Key),
    verify_info(Info, Keys).

verify_info1(Info0, Key) when is_atom(Key) ->
    case lists:keydelete(Key, 1, Info0) of
	Info0 ->
	    ?FAIL({missing_info, Key});
	Info ->
	    Info
    end;
verify_info1(Info0, {Key, SubKeys}) when is_atom(Key) andalso is_list(SubKeys) ->
    case lists:keysearch(Key, 1, Info0) of
	false ->
	    ?FAIL({missing_info, Key});	    
	{value, {Key, SubInfo}} ->
	    case verify_subinfo(SubInfo, SubKeys) of
		ok ->
		    lists:keydelete(Key, 1, Info0);
		{error, MissingSubKeyOrKeys} ->
		    ?FAIL({missing_info, {Key, MissingSubKeyOrKeys}})
	    end
    end.

verify_subinfo(_, []) ->
    ok;
verify_subinfo([], Keys) ->
    {error, Keys};
verify_subinfo(Info0, [Key|Keys]) ->
    case lists:keydelete(Key, 1, Info0) of
	Info0 ->
	    {error, Key};
	Info ->
	    verify_subinfo(Info, Keys)
    end.
   
%% Index String - string used in index
is(S) -> [length(S) | S].

try_test(Func) ->
    ?NPRINT("try test ~w...", [Func]),
    ?ALIB:try_test(?MODULE, Func).

try_test(Func, A) ->
    ?NPRINT("try test ~w...", [Func]),     
    ?ALIB:try_test(?MODULE, Func, A).

try_test(Func, A, Opts) ->
    ?NPRINT("try test ~w...", [Func]),     
    ?ALIB:try_test(?MODULE, Func, A, Opts).


%% Test manager wrapper functions:
g(Oids)          -> snmp_test_mgr:g(Oids).
%%gn()             -> snmp_test_mgr:gn().
gn(OidsOrN)      -> snmp_test_mgr:gn(OidsOrN).
gb(NR, MR, Oids) -> snmp_test_mgr:gb(NR, MR, Oids).
s(VAV)           -> snmp_test_mgr:s(VAV).
   
get_req(Id, Vars) ->
    ?ALIB:get_req(Id, Vars).

get_next_req(Vars) ->
    ?ALIB:get_next_req(Vars).


%%%-----------------------------------------------------------------
%%% Configuration
%%%-----------------------------------------------------------------
delete_files(Config) ->
    ?ALIB:delete_files(Config).

config(Vsns, MgrDir, AgentDir, MIp, AIp) ->
    ?ALIB:config(Vsns, MgrDir, AgentDir, MIp, AIp).

config(Vsns, MgrDir, AgentDir, MIp, AIp, IpFamily) ->
    ?ALIB:config(Vsns, MgrDir, AgentDir, MIp, AIp, IpFamily).

update_usm(Vsns, Dir) ->
    ?ALIB:update_usm(Vsns, Dir).
    
update_usm_mgr(Vsns, Dir) ->
    ?ALIB:update_usm_mgr(Vsns, Dir).

rewrite_usm_mgr(Dir, ShaKey, DesKey) -> 
    ?ALIB:rewrite_usm_mgr(Dir, ShaKey, DesKey).

reset_usm_mgr(Dir) ->
    ?ALIB:reset_usm_mgr(Dir).


update_vacm(Vsn, Dir) ->
    ?ALIB:update_vacm(Vsn, Dir).

write_community_conf(Dir, Conf) ->
    ?ALIB:write_community_conf(Dir, Conf).
    
write_target_addr_conf(Dir, Conf) ->
    ?ALIB:write_target_addr_conf(Dir, Conf).


rewrite_target_addr_conf(Dir, NewPort) ->
    ?ALIB:rewrite_target_addr_conf(Dir, NewPort).


reset_target_addr_conf(Dir) ->
    ?ALIB:reset_target_addr_conf(Dir).

write_target_params_conf(Dir, Vsns) ->
    ?ALIB:write_target_params_conf(Dir, Vsns).

rewrite_target_params_conf(Dir, SecName, SecLevel) -> 
    ?ALIB:rewrite_target_params_conf(Dir, SecName, SecLevel).

reset_target_params_conf(Dir) ->
    ?ALIB:reset_target_params_conf(Dir).

write_notify_conf(Dir) -> 
    ?ALIB:write_notify_conf(Dir).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

copy_file(From, To) ->
    ?ALIB:copy_file(From, To).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_log(Config) ->
    case lists:keysearch(agent_log_dir, 1, Config) of
	{value, {_, Dir}} ->
	    case lists:keysearch(snmp_master, 1, Config) of
		{value, {_, Node}} ->
		    LogDir  = Dir, 
		    Mibs    = [], 
		    OutFile = join(LogDir, "snmpa_log.txt"), 
		    ?IPRINT("~n"
                            "========================="
                            "  < Audit Trail Log >  "
                            "========================="
                            "~n"),
		    rcall(Node, snmpa, log_to_txt, [LogDir, Mibs, OutFile]),
		    rcall(Node, snmpa, log_to_io, [LogDir, Mibs]),
		    ?IPRINT("~n"
                            "========================="
                            " < / Audit Trail Log > "
                            "========================="
                            "~n");
		false ->
		    ?IPRINT("display_log -> no agent node found"),
		    ok
	    end;
	false ->
	    ?IPRINT("display_log -> no agent log dir found: "
                    "~n   ~p", [Config]),
	    ok
    end.


%% ------

display_memory_usage() ->
    Info  = snmpa:info(snmp_master_agent),
    AMU   = display_agent_memory_usage(Info),
    NIMU  = display_net_if_memory_usage(Info),
    NSMU  = display_note_store_memory_usage(Info),
    SSMU  = display_symbolic_store_memory_usage(Info),
    LDBMU = display_local_db_memory_usage(Info),
    MSMU  = display_mib_server_memory_usage(Info),
    ?NPRINT("Memory usage: ~n" ++ AMU ++ NIMU ++ NSMU ++ SSMU ++ LDBMU ++ MSMU),
    ok.

display_agent_memory_usage(Info) ->
    AgentInfo = lists_key1search(agent, Info),
    ProcMem   = 
	lists_key1search([process_memory,master_agent], AgentInfo),
    WProcMem  = 
	lists_key1search([process_memory,worker], AgentInfo),
    SWProcMem = 
	lists_key1search([process_memory,set_worker], AgentInfo),
    TabSize  = 
	lists_key1search([db_memory,agent], AgentInfo),
    CCSize   = 
	lists_key1search([db_memory,community_cache], AgentInfo),
    VacmSize = 
	lists_key1search([db_memory,vacm], AgentInfo),
    ?F("  Agent memory usage: "
       "~n    Master process memory size:     ~p"
       "~n    Worker process memory size:     ~p"
       "~n    Set-worker process memory size: ~p"
       "~n    Agent tab size:                 ~p"
       "~n    Community cache size:           ~p"
       "~n    Vacm tab size:                  ~p"
       "~n",
       [ProcMem, WProcMem, SWProcMem, 
        TabSize, CCSize, VacmSize]).

display_net_if_memory_usage(Info) ->
    NiInfo     = lists_key1search(net_if, Info),
    ProcMem    = lists_key1search(process_memory, NiInfo),
    ?F("  Net if memory usage: "
       "~n    Process memory size:            ~p"
       "~n", [ProcMem]).

display_note_store_memory_usage(Info) ->
    NsInfo     = lists_key1search(note_store, Info),
    ProcMem    = lists_key1search([process_memory,notes], NsInfo),
    ProcTmrMem = lists_key1search([process_memory,timer], NsInfo),
    TabSize    = lists_key1search([db_memory,notes],      NsInfo),
    ?F("  Note store memory usage: "
       "~n    Notes process memory size:      ~p"
       "~n    Timer process memory size:      ~p"
       "~n    Notes tab size:                 ~p"
       "~n",
       [ProcMem, ProcTmrMem, TabSize]).
 
  display_symbolic_store_memory_usage(Info) -> 
    SsInfo  = lists_key1search(symbolic_store, Info),
    ProcMem = lists_key1search(process_memory, SsInfo),
    DbMem   = lists_key1search(db_memory,      SsInfo),
    ?F("  Symbolic store memory usage: "
       "~n    Process memory size:            ~p"
       "~n    DB size:                        ~p"
       "~n",
       [ProcMem, DbMem]).
    
display_local_db_memory_usage(Info) ->
    LdInfo   = lists_key1search(local_db, Info),
    ProcMem  = lists_key1search(process_memory,   LdInfo),
    EtsSize  = lists_key1search([db_memory,ets],  LdInfo),
    DetsSize = lists_key1search([db_memory,dets], LdInfo),
    ?F("  Local DB memory usage: "
       "~n    Process memory size:            ~p"
       "~n    DB [ets] size:                  ~p"
       "~n    DB [dets] size:                 ~p"
       "~n",
       [ProcMem, EtsSize, DetsSize]).

display_mib_server_memory_usage(Info) ->
    MibInfo    = lists_key1search(mib_server, Info),
    ProcMem    = lists_key1search(process_memory,   MibInfo),
    TreeSize   = lists_key1search(tree_size_bytes,  MibInfo),
    MibDbSize  = lists_key1search([db_memory,mib],  MibInfo),
    NodeDbSize = lists_key1search([db_memory,node], MibInfo),
    TreeDbSize = lists_key1search([db_memory,tree], MibInfo),
    ?F("  MIB server memory usage: "
       "~n    Process memory size:            ~p"
       "~n    Tree size:                      ~p"
       "~n    Mib db size:                    ~p"
       "~n    Node db size:                   ~p"
       "~n    Tree db size:                   ~p"
       "~n", 
       [ProcMem, TreeSize, MibDbSize, NodeDbSize, TreeDbSize]).
    
lists_key1search([], Res) ->
    Res;
lists_key1search([Key|Keys], List) when is_atom(Key) andalso is_list(List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    lists_key1search(Keys, Val);
	false ->
	    undefined
    end;
lists_key1search(Key, List) when is_atom(Key) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    undefined
    end.


%% ------

init_v1_agent(Config) -> 
    %% -- 
    %% Start nodes
    %%
    {ok, AgentPeer, AgentNode} = ?START_PEER("-agent"),

    %% We don't use a manager in this test but the (common) config 
    %% function takes an argument that is derived from this
    {ok, ManagerPeer, ManagerNode} = ?START_PEER("-manager"),

    %% -- 
    %% Mnesia init
    %% 

    AgentDbDir = ?config(agent_db_dir, Config),
    AgentMnesiaDir = join([AgentDbDir, "mnesia"]),
    mnesia_init(AgentNode, AgentMnesiaDir),

    mnesia_create_schema(AgentNode, [AgentNode]),

    mnesia_start(AgentNode),

    %% --
    %% Host & IP
    %% 

    AgentHost   = ?HOSTNAME(AgentNode),
    ManagerHost = ?HOSTNAME(ManagerNode),

    Host              = snmp_test_lib:hostname(), 
    IpFamily          = config_ipfamily(Config),
    Ip                = ?LOCALHOST(IpFamily),
    {ok, AgentIP0}    = snmp_misc:ip(AgentHost, IpFamily),
    AgentIP           = tuple_to_list(AgentIP0), 
    {ok, ManagerIP0}  = snmp_misc:ip(ManagerHost, IpFamily),
    ManagerIP         = tuple_to_list(ManagerIP0),


    %% --
    %% Write agent config
    %% 

    Vsns           = [v1], 
    ManagerConfDir = ?config(manager_top_dir, Config),
    AgentConfDir   = ?config(agent_conf_dir, Config),
    AgentTopDir    = ?config(agent_top_dir, Config),
    AgentBkpDir1   = join([AgentTopDir, backup1]),
    AgentBkpDir2   = join([AgentTopDir, backup2]),
    ok = file:make_dir(AgentBkpDir1),
    ok = file:make_dir(AgentBkpDir2),
    AgentBkpDirs = [AgentBkpDir1, AgentBkpDir2], 
    config(Vsns, ManagerConfDir, AgentConfDir, ManagerIP, AgentIP, IpFamily),


    %% --
    %% Start the agent
    %% 

    Config2 = start_agent([{host,              Host}, 
			   {ip,                Ip},
			   {agent_peer,        AgentPeer},
			   {agent_node,        AgentNode}, 
			   {agent_host,        AgentHost}, 
			   {agent_ip,          AgentIP}, 
			   {agent_backup_dirs, AgentBkpDirs},
                           
                           %% We need this here since without it
                           %% fin_v1_agent will not be able to stop!
                           {manager_peer, ManagerPeer},
                           {manager_node, ManagerNode}|Config]),

    %% -- 
    %% Create watchdog 
    %% 

    wd_start(1, Config2).

fin_v1_agent(Config) ->
    AgentNode   = ?config(agent_node, Config),

    %% -
    %% Stop agent (this is the nice way to do it, 
    %% so logs and files can be closed in the proper way).
    %% 

    AgentSup = ?config(agent_sup, Config),
    stop_standalone_agent(AgentSup), 

    %% - 
    %% Stop mnesia
    %% 
    mnesia_stop(AgentNode),


    %% - 
    %% Stop the agent node
    %%
    peer:stop(?config(agent_peer, Config)),


    %%     SubAgentNode = ?config(sub_agent_node, Config),
    %%     stop_node(SubAgentNode),


    %% - 
    %% Stop the manager node
    %% 
    peer:stop(?config(manager_peer, Config)),

    wd_stop(Config).


config_ipfamily(Config) ->
    case ?config(ipfamily, Config) of
	undefined ->
	    inet;
	Value ->
	    Value
    end.


%% ------

join(Parts) ->
    filename:join(Parts).

join(Dir, File) ->
    filename:join(Dir, File).


%% ------

rcall(Node, Mod, Func, Args) ->
    case rpc:call(Node, Mod, Func, Args) of
	{badrpc, nodedown} ->
	    ?FAIL({rpc_failure, Node});
	Else ->
	    Else
    end.
