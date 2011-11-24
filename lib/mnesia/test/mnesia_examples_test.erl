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
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(replica_test, ram_copies))).

sticky_replica_test(suite) -> [];
sticky_replica_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(sticky_replica_test, ram_copies))).

dist_test(suite) -> [];
dist_test(Config) when is_list(Config) ->
    ?init(3, [{tc_timeout, timer:minutes(10)} | Config]),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(dist_test, ram_copies))).

conflict_test(suite) -> [];
conflict_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(conflict_test, ram_copies))).

frag_test(suite) -> [];
frag_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(frag_test, ram_copies))).

frag2_test(suite) -> [];
frag2_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(frag2_test, ram_copies))).

remote_test(suite) -> [];
remote_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(remote_test, ram_copies))).

remote_frag2_test(suite) -> [];
remote_frag2_test(Config) when is_list(Config) ->
    ?init(3, Config),
    opt_net_load(mnesia_tpcb),
    ?match({ok, _}, mnesia_tpcb:start(mnesia_tpcb:config(remote_frag2_test, ram_copies))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meter(doc) ->
    ["Run the meter example in mnesia_meter.erl"];
meter(suite) ->
    [];
meter(Config) when is_list(Config) ->
    [N | _] = ?init(3, Config),
    opt_net_load(mnesia_meter),
    ?match(ok, mnesia_meter:go(ram_copies, [N])).


