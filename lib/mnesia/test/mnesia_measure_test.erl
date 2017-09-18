%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(mnesia_measure_test).
-author('hakan@erix.ericsson.se').

-export([init_per_testcase/2, end_per_testcase/2,
         init_per_group/2, end_per_group/2,
         all/0, groups/0]).

-export([cost/1, dbn_meters/1,
         ram_tpcb/1, disc_tpcb/1, disc_only_tpcb/1,
         ram_meter/1, disc_meter/1, disc_only_meter/1]).


-include("mnesia_test_lib.hrl").

init_per_testcase(Func, Conf) ->
    mnesia_test_lib:init_per_testcase(Func, Conf).

end_per_testcase(Func, Conf) ->
    mnesia_test_lib:end_per_testcase(Func, Conf).

-define(init(N, Config),
	mnesia_test_lib:prepare_test_case([{init_test_case, [mnesia]},
					   delete_schema],
					  N, Config, ?FILE, ?LINE)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [{group, benchmarks}].

groups() -> 
    [{benchmarks, [],
      [{group, meter}, cost, dbn_meters,
       {group, tpcb}]},
     {tpcb, [], [ram_tpcb, disc_tpcb, disc_only_tpcb]},
     {meter, [], [ram_meter, disc_meter, disc_only_meter]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dbn_meters(suite) -> [];
dbn_meters(Config) when is_list(Config) ->
    _Nodes = ?init(3, Config),
    ?match(ok, mnesia_dbn_meters:start()),
    ok.


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
