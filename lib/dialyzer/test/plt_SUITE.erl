%% This suite is the only hand made and simply
%% checks if we can build a plt.

-module(plt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0, build_plt/1]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_plt].

build_plt(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    ok   -> ok;
    fail -> ct:fail(plt_build_fail)
  end.
