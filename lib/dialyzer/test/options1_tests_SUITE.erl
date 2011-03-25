%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(options1_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([options1_tests_SUITE_consistency/1, compiler/1]).

suite() ->
  [{timetrap, {minutes, 30}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, [{include_dirs,["my_include"]},
                               {defines,[{'COMPILER_VSN',42}]},
                               {warnings,[no_improper_lists]}]}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [options1_tests_SUITE_consistency,compiler].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

options1_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

compiler(Config) ->
  case dialyze(Config, compiler) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

