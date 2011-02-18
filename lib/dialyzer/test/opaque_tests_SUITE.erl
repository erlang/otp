%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(opaque_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([opaque_tests_SUITE_consistency/1, array/1, crash/1, dict/1, 
         ets/1, gb_sets/1, inf_loop1/1, int/1, mixed_opaque/1, 
         my_digraph/1, my_queue/1, opaque/1, queue/1, rec/1, timer/1, 
         union/1, wings/1, zoltan_kis1/1, zoltan_kis2/1, zoltan_kis3/1, 
         zoltan_kis4/1, zoltan_kis5/1, zoltan_kis6/1]).

suite() ->
  [{timetrap, {minutes, 1}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, [{warnings,[no_unused,no_return]}]}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [opaque_tests_SUITE_consistency,array,crash,dict,ets,gb_sets,inf_loop1,int,
   mixed_opaque,my_digraph,my_queue,opaque,queue,rec,timer,union,wings,
   zoltan_kis1,zoltan_kis2,zoltan_kis3,zoltan_kis4,zoltan_kis5,zoltan_kis6].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

opaque_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

array(Config) ->
  case dialyze(Config, array) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

crash(Config) ->
  case dialyze(Config, crash) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

dict(Config) ->
  case dialyze(Config, dict) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

ets(Config) ->
  case dialyze(Config, ets) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

gb_sets(Config) ->
  case dialyze(Config, gb_sets) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

inf_loop1(Config) ->
  case dialyze(Config, inf_loop1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

int(Config) ->
  case dialyze(Config, int) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mixed_opaque(Config) ->
  case dialyze(Config, mixed_opaque) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

my_digraph(Config) ->
  case dialyze(Config, my_digraph) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

my_queue(Config) ->
  case dialyze(Config, my_queue) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

opaque(Config) ->
  case dialyze(Config, opaque) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

queue(Config) ->
  case dialyze(Config, queue) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

rec(Config) ->
  case dialyze(Config, rec) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

timer(Config) ->
  case dialyze(Config, timer) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

union(Config) ->
  case dialyze(Config, union) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

wings(Config) ->
  case dialyze(Config, wings) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis1(Config) ->
  case dialyze(Config, zoltan_kis1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis2(Config) ->
  case dialyze(Config, zoltan_kis2) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis3(Config) ->
  case dialyze(Config, zoltan_kis3) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis4(Config) ->
  case dialyze(Config, zoltan_kis4) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis5(Config) ->
  case dialyze(Config, zoltan_kis5) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

zoltan_kis6(Config) ->
  case dialyze(Config, zoltan_kis6) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

