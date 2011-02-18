%% ATTENTION!
%% This is an automatically generated file. Do not edit.
%% Use './remake' script to refresh it if needed.
%% All Dialyzer options should be defined in dialyzer_options
%% file.

-module(user_tests_SUITE).

-include("ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([user_tests_SUITE_consistency/1, broken_dialyzer/1, 
         gcpFlowControl/1, qlc_error/1, spvcOrig/1, wsp_pdu/1]).

suite() ->
  [{timetrap, {minutes, 3}}].

init_per_suite() ->
  [{timetrap, ?plt_timeout}].
init_per_suite(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    fail -> {skip, "Plt creation/check failed."};
    ok -> [{dialyzer_options, []}|Config]
  end.

end_per_suite(_Config) ->
  ok.

all() ->
  [user_tests_SUITE_consistency,broken_dialyzer,gcpFlowControl,qlc_error,
   spvcOrig,wsp_pdu].

dialyze(Config, TestCase) ->
  Opts = ?config(dialyzer_options, Config),
  Dir = ?config(data_dir, Config),
  OutDir = ?config(priv_dir, Config),
  dialyzer_common:check(TestCase, Opts, Dir, OutDir).

user_tests_SUITE_consistency(Config) ->
  Dir = ?config(data_dir, Config),
  case dialyzer_common:new_tests(Dir, all()) of
    []  -> ok;
    New -> ct:fail({missing_tests,New})
  end.

broken_dialyzer(Config) ->
  case dialyze(Config, broken_dialyzer) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

gcpFlowControl(Config) ->
  case dialyze(Config, gcpFlowControl) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

qlc_error(Config) ->
  case dialyze(Config, qlc_error) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

spvcOrig(Config) ->
  case dialyze(Config, spvcOrig) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

wsp_pdu(Config) ->
  case dialyze(Config, wsp_pdu) of
    'same' -> 'same';
    Error  -> ct:fail(Error)
  end.

