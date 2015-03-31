%% This suite is the only hand made and simply
%% checks if we can build and update a plt.

-module(plt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0, build_plt/1, update_plt/1]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_plt, update_plt].

build_plt(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    ok   -> ok;
    fail -> ct:fail(plt_build_fail)
  end.

%%% [James Fish:]
%%% If a function is removed from a module and the module has previously
%%% been added to a PLT, the function will not be removed from PLT when
%%% the PLT is checked. This results in dialyzer failing to produce a
%%% callgraph warning when doing success typings analysis if the remove
%%% function is still called in another module
%%% As the function is not removed from the PLT a prior warning, such as a
%%% contract types warning, might be emitted when the removed function
%%% nolonger exists.
update_plt(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Prog1 = <<"-module(plt_gc).
               -export([one/0]).
               one() ->
                  one.">>,
    {ok, Beam} = compile(Config, Prog1, plt_gc, []),

    ErlangBeam = case code:where_is_file("erlang.beam") of
                     non_existing ->
                         filename:join([code:root_dir(),
                                        "erts", "preloaded", "ebin",
                                        "erlang.beam"]);
                     EBeam ->
                         EBeam
                 end,
    Plt = filename:join(PrivDir, "plt_gc.plt"),
    Opts = [{check_plt, true}, {from, byte_code}],
    [] = dialyzer:run([{analysis_type, plt_build},
                       {files, [Beam, ErlangBeam]},
                       {output_plt, Plt}] ++ Opts),

    Prog2 = <<"-module(plt_gc).
               -export([two/0]).
               two() ->
                  two.">>,
    {ok, Beam} = compile(Config, Prog2, plt_gc, []),

    Test = <<"-module(test).
              -export([test/0]).
              -spec test() -> test.
              test() ->
                  plt_gc:one().">>,
    {ok, TestBeam} = compile(Config, Test, test, []),
    [{warn_callgraph, _, {call_to_missing, [plt_gc, one, 0]}}] =
        dialyzer:run([{analysis_type, succ_typings},
                      {files, [TestBeam]},
                      {init_plt, Plt}] ++ Opts),
    ok.

compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = ?config(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.
