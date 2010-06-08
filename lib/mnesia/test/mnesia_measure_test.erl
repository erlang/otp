%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(mnesia_measure_test).
-author('hakan@erix.ericsson.se').
-compile([export_all]).

-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

fin_per_testcase(Func, Conf) ->
    mnesia_test_lib:fin_per_testcase(Func, Conf).

-define(init(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					   delete_schema],
					  N, Config, ?FILE, ?LINE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(doc) ->
    ["Measure various aspects of Mnesia",
     "Verify that Mnesia has predictable response times,",
     "that the transaction system has fair algoritms,",
     "resource consumption, scalabilitym system limits etc.",
     "Perform some benchmarks."];
all(suite) ->
    [
     prediction,
     consumption,
     scalability,
     benchmarks
    ].
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prediction(doc) ->
    ["The system must have predictable response times.",
     "The maintenance of the system should not impact on the",
     "availability. Make sure that the response times does not vary too",
     "much from the undisturbed normal usage.",
     "Verify that deadlocks never occurs."];
prediction(suite) ->
    [
     reader_disturbed_by_node_down,
     writer_disturbed_by_node_down,
     reader_disturbed_by_node_up,
     writer_disturbed_by_node_up,
     reader_disturbed_by_schema_ops,
     writer_disturbed_by_schema_ops,
     reader_disturbed_by_checkpoint,
     writer_disturbed_by_checkpoint,
     reader_disturbed_by_dump_log,
     writer_disturbed_by_dump_log,
     reader_disturbed_by_backup,
     writer_disturbed_by_backup,
     reader_disturbed_by_restore,
     writer_disturbed_by_restore,
     fairness
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fairness(doc) ->
    ["Verify that the transaction system behaves fair, even under intense",
     "stress. Combine different access patterns (transaction profiles)",
     "in order to verify that concurrent applications gets a fair share",
     "of the database resource. Verify that starvation never may occur."];
fairness(suite) ->
    [
     reader_competing_with_reader,
     reader_competing_with_writer,
     writer_competing_with_reader,
     writer_competing_with_writer
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
consumption(doc) ->
    ["Measure the resource consumption and publish the outcome. Make",
     "sure that resources are released after failures."];
consumption(suite) ->
    [
     measure_resource_consumption,
     determine_resource_leakage
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scalability(doc) ->
    ["Try out where the system limits are. We must at least meet the",
     "documented system limits.",
     "Redo the performance meters for various configurations and load,",
     "especially near system limits."];
scalability(suite) ->
    [
     determine_system_limits,
     performance_at_min_config,
     performance_at_max_config,
     performance_at_full_load,
     resource_consumption_at_min_config,
     resource_consumption_at_max_config,
     resource_consumption_at_full_load
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
benchmarks(doc) ->
    ["Measure typical database operations and publish them. Try to",
     "verify that new releases of Mnesia always outperforms old",
     "releases, or at least that the meters does not get worse."];
benchmarks(suite) ->
    [
     meter,
     cost,
     dbn_meters,
     measure_all_api_functions,
     tpcb,
     mnemosyne_vs_mnesia_kernel
    ].

dbn_meters(suite) -> [];
dbn_meters(Config) when is_list(Config) ->
    _Nodes = ?init(3, Config),
    ?match(ok, mnesia_dbn_meters:start()),
    ok.

tpcb(suite) ->
    [
     ram_tpcb,
     disc_tpcb,
     disc_only_tpcb
    ].

tpcb(ReplicaType, Config) ->
    HarakiriDelay = {tc_timeout, timer:minutes(20)},
    Nodes = ?acquire_nodes(2, Config ++ [HarakiriDelay]),
    Args = [{n_branches, 2},
	    {n_drivers_per_node, 1},
	    {replica_nodes, Nodes},
	    {driver_nodes, [hd(Nodes)]},
	    {use_running_mnesia, true},
	    {use_sticky_locks, true},
	    {replica_type, ReplicaType}],
    ?match({ok, _}, mnesia_tpcb:start(Args)),
    ?verify_mnesia(Nodes, []).

ram_tpcb(suite) -> [];
ram_tpcb(Config) when is_list(Config) ->
    tpcb(ram_copies, Config).

disc_tpcb(suite) -> [];
disc_tpcb(Config) when is_list(Config) ->
    tpcb(disc_copies, Config).

disc_only_tpcb(suite) -> [];
disc_only_tpcb(Config) when is_list(Config) ->
    tpcb(disc_only_copies, Config).

meter(suite) ->
    [
     ram_meter,
     disc_meter,
     disc_only_meter
    ].

ram_meter(suite) -> [];
ram_meter(Config) when is_list(Config) ->
    HarakiriDelay = [{tc_timeout, timer:minutes(20)}],
    Nodes = ?init(3, Config ++ HarakiriDelay),
    ?match(ok, mnesia_meter:go(ram_copies, Nodes)).

disc_meter(suite) -> [];
disc_meter(Config) when is_list(Config) ->
    HarakiriDelay = [{tc_timeout, timer:minutes(20)}],
    Nodes = ?init(3, Config ++ HarakiriDelay),
    ?match(ok, mnesia_meter:go(disc_copies, Nodes)).

disc_only_meter(suite) -> [];
disc_only_meter(Config) when is_list(Config) ->
    HarakiriDelay = [{tc_timeout, timer:minutes(20)}],
    Nodes = ?init(3, Config ++ HarakiriDelay),
    ?match(ok, mnesia_meter:go(disc_only_copies, Nodes)).

cost(suite) -> [];
cost(Config) when is_list(Config) ->
    Nodes = ?init(3, Config),
    ?match(ok, mnesia_cost:go(Nodes)),
    file:delete("MNESIA_COST").
