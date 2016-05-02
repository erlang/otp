%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mnesia_examples_test).
-author('hakan@erix.ericsson.se').
-compile([export_all]).
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
    [bup, company, meter, {group, tpcb}].

groups() -> 
    [{tpcb, [],
      [replica_test, sticky_replica_test, dist_test,
       conflict_test, frag_test, frag2_test, remote_test,
       remote_frag2_test]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bup(doc) -> ["Run the backup examples in bup.erl"];
bup(suite) -> [];
bup(Config) when is_list(Config) ->
    Nodes = ?init(3, Config),
    opt_net_load(bup),
    ?match(ok, bup:test(Nodes)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
company(doc) ->
    ["Run the company examples in company.erl and company_o.erl"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replica_test(suite) -> [];
replica_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(replica_test, ram_copies))).

sticky_replica_test(suite) -> [];
sticky_replica_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(sticky_replica_test, ram_copies))).

dist_test(suite) -> [];
dist_test(Config) when is_list(Config) ->
    ?init(3, [{tc_timeout, timer:minutes(10)} | Config]),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(dist_test, ram_copies))).

conflict_test(suite) -> [];
conflict_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(conflict_test, ram_copies))).

frag_test(suite) -> [];
frag_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(frag_test, ram_copies))).

frag2_test(suite) -> [];
frag2_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(frag2_test, ram_copies))).

remote_test(suite) -> [];
remote_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(remote_test, ram_copies))).

remote_frag2_test(suite) -> [];
remote_frag2_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(config(remote_frag2_test, ram_copies))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meter(doc) ->
    ["Run the meter example in mnesia_meter.erl"];
meter(suite) ->
    [];
meter(Config) when is_list(Config) ->
    [N | _] = ?init(3, Config),
    opt_net_load(mnesia_meter),
    ?match(ok, mnesia_meter:go(ram_copies, [N])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config(Test, Type) ->
    Config0 = mnesia_tpcb:config(Test, Type),
    %% Cut the times, the idea is to test the example and configuration
    %% not running the test a long time
    Config1 = lists:keyreplace(stop_after, 1, Config0, {stop_after, 6000}),
    lists:keyreplace(report_interval, 1, Config1, {report_interval, 1000}).

opt_net_load(ExampleMod) ->
    opt_net_load([node() | nodes()], ExampleMod, ok).

opt_net_load([Node | Nodes], ExampleMod, Res) ->
    case rpc:call(Node, ?MODULE, opt_load, [ExampleMod]) of
	{module, ExampleMod} ->
	    opt_net_load(Nodes, ExampleMod, Res);
	{error, Reason} ->
	    Error = {opt_net_load, ExampleMod, Node, Reason},
	    opt_net_load(Nodes, ExampleMod, {error, Error});
	{badrpc, Reason} ->
	    Error = {opt_net_load, ExampleMod, Node, Reason},
	    opt_net_load(Nodes, ExampleMod, {error, Error})
    end;
opt_net_load([], _ExampleMod, Res) ->
    Res.

opt_load(Mod) ->
    case code:is_loaded(Mod) of
	{file, _} ->
	    {module, Mod};
	false ->
	    Abs = filename:join([code:lib_dir(mnesia), examples, Mod]),
	    code:load_abs(Abs)
    end.
