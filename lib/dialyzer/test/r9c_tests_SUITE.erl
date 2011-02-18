%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(r9c_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([r9c_tests_SUITE_consistency/1, asn1/1, inets/1, mnesia/1]).

suite() ->
  [{timetrap, {minutes, 20}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, [{defines,[{vsn,42}]}]}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [r9c_tests_SUITE_consistency,asn1,inets,mnesia].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

r9c_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

asn1(Config) ->
  case dialyze(Config, asn1) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

inets(Config) ->
  case dialyze(Config, inets) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

mnesia(Config) ->
  case dialyze(Config, mnesia) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

