%% This suite is the only hand made and simply
%% checks if we can build and update a plt.

-module(plt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0, build_plt/1, beam_tests/1, update_plt/1,
         local_fun_same_as_callback/1,
         remove_plt/1, run_plt_check/1, run_succ_typings/1]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_plt, beam_tests, update_plt, run_plt_check,
          remove_plt, run_succ_typings, local_fun_same_as_callback].

build_plt(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    ok   -> ok;
    fail -> ct:fail(plt_build_fail)
  end.

beam_tests(Config) when is_list(Config) ->
    Prog = <<"
              -module(no_auto_import).

              %% Copied from erl_lint_SUITE.erl, clash6

              -export([size/1]).

              size([]) ->
                  0;
              size({N,_}) ->
                  N;
              size([_|T]) ->
                  1+size(T).
             ">>,
    Opts = [no_auto_import],
    {ok, BeamFile} = compile(Config, Prog, no_auto_import, Opts),
    [] = run_dialyzer(plt_build, [BeamFile], []),
    ok.

run_plt_check(Config) when is_list(Config) ->
    Mod1 = <<"
	      -module(run_plt_check1).
	     ">>,

    Mod2A = <<"
	       -module(run_plt_check2).
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1, run_plt_check1, []),
    {ok, BeamFile2} = compile(Config, Mod2A, run_plt_check2, []),
    [] = run_dialyzer(plt_build, [BeamFile1, BeamFile2], []),

    Mod2B = <<"
	       -module(run_plt_check2).

	       -export([call/1]).

	       call(X) -> run_plt_check1:call(X).
	     ">>,

    {ok, BeamFile2} = compile(Config, Mod2B, run_plt_check2, []),

    % callgraph warning as run_plt_check2:call/1 makes a call to unexported
    % function run_plt_check1:call/1.
    [_] = run_dialyzer(plt_check, [], []),

    ok.

run_succ_typings(Config) when is_list(Config) ->
    Mod1A = <<"
	       -module(run_succ_typings1).

	       -export([call/0]).

	       call() -> a.
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1A, run_succ_typings1, []),
    [] = run_dialyzer(plt_build, [BeamFile1], []),

    Mod1B = <<"
	       -module(run_succ_typings1).

	       -export([call/0]).

	       call() -> b.
	     ">>,

    Mod2 = <<"
	      -module(run_succ_typings2).

	      -export([call/0]).

	      -spec call() -> b.
	      call() -> run_succ_typings1:call().
	     ">>,

    {ok, BeamFile1} = compile(Config, Mod1B, run_succ_typings1, []),
    {ok, BeamFile2} = compile(Config, Mod2, run_succ_typings2, []),
    % contract types warning as run_succ_typings2:call/0 makes a call to
    % run_succ_typings1:call/0, which returns a (not b) in the PLT.
    [_] = run_dialyzer(succ_typings, [BeamFile2], [{check_plt, false}]),
    % warning not returned as run_succ_typings1 is updated in the PLT.
    [] = run_dialyzer(succ_typings, [BeamFile2], [{check_plt, true}]),

    ok.

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

%%% If a behaviour module contains an non-exported function with the same name
%%% as one of the behaviour's callbacks, the callback info was inadvertently
%%% deleted from the PLT as the dialyzer_plt:delete_list/2 function was cleaning
%%% up the callback table. This bug was reported by Brujo Benavides.

local_fun_same_as_callback(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Prog1 =
      <<"-module(bad_behaviour).
         -callback bad() -> bad.
         -export([publicly_bad/0]).

         %% @doc This function is here just to avoid the 'unused' warning for bad/0
         publicly_bad() -> bad().

         %% @doc This function overlaps with the callback with the same name, and
         %%      that was an issue for dialyzer since it's a private function.
         bad() -> bad.">>,
    {ok, Beam} = compile(Config, Prog1, bad_behaviour, []),

    ErlangBeam = case code:where_is_file("erlang.beam") of
                     non_existing ->
                         filename:join([code:root_dir(),
                                        "erts", "preloaded", "ebin",
                                        "erlang.beam"]);
                     EBeam ->
                         EBeam
                 end,
    Plt = filename:join(PrivDir, "plt_bad_behaviour.plt"),
    Opts = [{check_plt, true}, {from, byte_code}],
    [] = dialyzer:run([{analysis_type, plt_build},
                       {files, [Beam, ErlangBeam]},
                       {output_plt, Plt}] ++ Opts),

    Prog2 =
        <<"-module(bad_child).
           -behaviour(bad_behaviour).

           -export([bad/0]).

           %% @doc This function incorreclty implements bad_behaviour.
           bad() -> not_bad.">>,
    {ok, TestBeam} = compile(Config, Prog2, bad_child, []),

    [{warn_behaviour, _,
      {callback_type_mismatch,
       [bad_behaviour,bad,0,"'not_bad'","'bad'"]}}] =
        dialyzer:run([{analysis_type, succ_typings},
                      {files, [TestBeam]},
                      {init_plt, Plt}] ++ Opts),
    ok.
  
%%% [James Fish:]
%%% Dialyzer always asserts that files and directories passed in its
%%% options exist. Therefore it is not possible to remove a beam/module
%%% from a PLT when the beam file no longer exists. Dialyzer should not to
%%% check files exist on disk when removing from the PLT.
remove_plt(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Prog1 = <<"-module(m1).
               -export([t/0]).
               t() ->
                  m2:a(a).">>,
    {ok, Beam1} = compile(Config, Prog1, m1, []),

    Prog2 = <<"-module(m2).
               -export([a/1]).
               a(A) when is_integer(A) -> A.">>,
    {ok, Beam2} = compile(Config, Prog2, m2, []),

    Plt = filename:join(PrivDir, "remove.plt"),
    Opts = [{check_plt, true}, {from, byte_code}],

    [{warn_return_no_exit, _, {no_return,[only_normal,t,0]}},
     {warn_failing_call, _, {call, [m2,a,"('a')",_,_,_,_,_]}}] =
        dialyzer:run([{analysis_type, plt_build},
                      {files, [Beam1, Beam2]},
                      {get_warnings, true},
                      {output_plt, Plt}] ++ Opts),

    [] = dialyzer:run([{init_plt, Plt},
                       {files, [Beam2]},
                       {analysis_type, plt_remove}]),

    [] =  dialyzer:run([{analysis_type, succ_typings},
                        {files, [Beam1]},
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

run_dialyzer(Analysis, Files, Opts) ->
    dialyzer:run([{analysis_type, Analysis},
		  {files, Files},
		  {from, byte_code} |
		  Opts]).
