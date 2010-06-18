-module(opaque_tests_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2,
         init_per_testcase/2, fin_per_testcase/2]).

-export([array/1, crash/1, dict/1, ets/1, gb_sets/1, inf_loop1/1, 
         int/1, mixed_opaque/1, my_digraph/1, my_queue/1, opaque/1, 
         queue/1, rec/1, timer/1, union/1, wings/1, zoltan_kis1/1, 
         zoltan_kis2/1, zoltan_kis3/1, zoltan_kis4/1, zoltan_kis5/1, 
         zoltan_kis6/1]).

-define(default_timeout, ?t:minutes(1)).
-define(dialyzer_options, ?config(dialyzer_options, Config)).
-define(datadir, ?config(data_dir, Config)).
-define(privdir, ?config(priv_dir, Config)).

groups() -> [].

init_per_group(_GroupName, Config) -> Config.

end_per_group(_GroupName, Config) -> Config.

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{dialyzer_options, [{warnings,[no_unused,no_return]}]}, {watchdog, Dog} | Config].

fin_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    ?t:timetrap_cancel(Dog),
    ok.

all() ->
    [array,crash,dict,ets,gb_sets,inf_loop1,int,mixed_opaque,my_digraph,
     my_queue,opaque,queue,rec,timer,union,wings,zoltan_kis1,zoltan_kis2,
     zoltan_kis3,zoltan_kis4,zoltan_kis5,zoltan_kis6].

array(Config) when is_list(Config) ->
    ?line run(Config, {array, dir}),
    ok.

crash(Config) when is_list(Config) ->
    ?line run(Config, {crash, dir}),
    ok.

dict(Config) when is_list(Config) ->
    ?line run(Config, {dict, dir}),
    ok.

ets(Config) when is_list(Config) ->
    ?line run(Config, {ets, dir}),
    ok.

gb_sets(Config) when is_list(Config) ->
    ?line run(Config, {gb_sets, dir}),
    ok.

inf_loop1(Config) when is_list(Config) ->
    ?line run(Config, {inf_loop1, file}),
    ok.

int(Config) when is_list(Config) ->
    ?line run(Config, {int, dir}),
    ok.

mixed_opaque(Config) when is_list(Config) ->
    ?line run(Config, {mixed_opaque, dir}),
    ok.

my_digraph(Config) when is_list(Config) ->
    ?line run(Config, {my_digraph, dir}),
    ok.

my_queue(Config) when is_list(Config) ->
    ?line run(Config, {my_queue, dir}),
    ok.

opaque(Config) when is_list(Config) ->
    ?line run(Config, {opaque, dir}),
    ok.

queue(Config) when is_list(Config) ->
    ?line run(Config, {queue, dir}),
    ok.

rec(Config) when is_list(Config) ->
    ?line run(Config, {rec, dir}),
    ok.

timer(Config) when is_list(Config) ->
    ?line run(Config, {timer, dir}),
    ok.

union(Config) when is_list(Config) ->
    ?line run(Config, {union, dir}),
    ok.

wings(Config) when is_list(Config) ->
    ?line run(Config, {wings, dir}),
    ok.

zoltan_kis1(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis1, file}),
    ok.

zoltan_kis2(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis2, file}),
    ok.

zoltan_kis3(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis3, file}),
    ok.

zoltan_kis4(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis4, file}),
    ok.

zoltan_kis5(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis5, file}),
    ok.

zoltan_kis6(Config) when is_list(Config) ->
    ?line run(Config, {zoltan_kis6, file}),
    ok.

run(Config, TestCase) ->
    case run_test(Config, TestCase) of
        ok -> ok;
        {fail, Reason} ->
            ?t:format("~s",[Reason]),
            fail()
    end.

run_test(Config, {TestCase, Kind}) ->
    Dog = ?config(watchdog, Config),
    Options = ?dialyzer_options,
    Dir = ?datadir,
    OutDir = ?privdir,
    case dialyzer_test:dialyzer_test(Options, TestCase, Kind,
                                     Dir, OutDir, Dog) of
        same -> ok;
        {differ, DiffList} ->
            {fail,
               io_lib:format("\nTest ~p failed:\n~p\n",
                            [TestCase, DiffList])}
    end.

fail() ->
    io:format("failed\n"),
    ?t:fail().
