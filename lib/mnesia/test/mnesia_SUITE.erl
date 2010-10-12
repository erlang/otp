%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
%%

%%
-module(mnesia_SUITE).
-author('hakan@erix.ericsson.se').
-compile([export_all]).
-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() -> 
[{group, light}, {group, medium}, {group, heavy},
 clean_up_suite].

groups() -> 
    [{light, [],
  [{group, install}, {group, nice}, {group, evil},
   {mnesia_frag_test, [{group, light}]}, {group, qlc},
   {group, registry}, {group, config}, {group, examples}]},
 {install, [], [{mnesia_install_test, all}]},
 {nice, [], [{mnesia_nice_coverage_test, all}]},
 {evil, [], [{mnesia_evil_coverage_test, all}]},
 {qlc, [], [{mnesia_qlc_test, all}]},
 {registry, [], [{mnesia_registry_test, all}]},
 {config, [], [{mnesia_config_test, all}]},
 {examples, [], [{mnesia_examples_test, all}]},
 {medium, [],
  [{group, install}, {group, atomicity},
   {group, isolation}, {group, durability},
   {group, recovery}, {group, consistency},
   {mnesia_frag_test, [{group, medium}]}]},
 {atomicity, [], [{mnesia_atomicity_test, all}]},
 {isolation, [], [{mnesia_isolation_test, all}]},
 {durability, [], [{mnesia_durability_test, all}]},
 {recovery, [], [{mnesia_recovery_test, all}]},
 {consistency, [], [{mnesia_consistency_test, all}]},
 {heavy, [], [{group, measure}]},
 {measure, [], [{mnesia_measure_test, all}]},
 {prediction, [],
  [{mnesia_measure_test, [{group, prediction}]}]},
 {fairness, [],
  [{mnesia_measure_test, [{group, fairness}]}]},
 {benchmarks, [],
  [{mnesia_measure_test, [{group, benchmarks}]}]},
 {consumption, [],
  [{mnesia_measure_test, [{group, consumption}]}]},
 {scalability, [],
  [{mnesia_measure_test, [{group, scalability}]}]},
 {otp_r4b, [],
  [{mnesia_config_test, access_module},
   {mnesia_config_test, dump_log_load_regulation},
   {mnesia_config_test, embedded_mnemosyne},
   {mnesia_config_test, ignore_fallback_at_startup},
   {mnesia_config_test, max_wait_for_decision},
   {mnesia_consistency_test, consistency_after_restore},
   {mnesia_evil_backup, restore},
   {mnesia_evil_coverage_test, offline_set_master_nodes},
   {mnesia_evil_coverage_test, record_name},
   {mnesia_evil_coverage_test, user_properties},
   {mnesia_registry_test, all}, {group, otp_2363}]},
 {otp_2363, [],
  [{mnesia_dirty_access_test,
    dirty_index_match_object_disc_only},
   {mnesia_dirty_access_test, dirty_index_read_disc_only},
   {mnesia_dirty_access_test,
    dirty_index_update_bag_disc_only},
   {mnesia_dirty_access_test,
    dirty_index_update_set_disc_only},
   {mnesia_evil_coverage_test,
    create_live_table_index_disc_only}]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
silly() ->
    mnesia_install_test:silly().
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   



   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%








clean_up_suite(doc) -> ["Not a test case only kills mnesia and nodes, that where" 
			"started during the tests"];
clean_up_suite(suite) ->
    [];
clean_up_suite(Config) when is_list(Config)->
    mnesia:kill(),
    Slaves = mnesia_test_lib:lookup_config(nodenames, Config),
    Nodes = lists:delete(node(), Slaves),
    rpc:multicall(Nodes, erlang, halt, []),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    


