%% This suite is the only hand made and simply
%% checks if we can build and update a plt.

-module(plt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0, build_plt/1, beam_tests/1, update_plt/1,
         local_fun_same_as_callback/1,
         remove_plt/1, run_plt_check/1, run_succ_typings/1,
         bad_dialyzer_attr/1, merge_plts/1, bad_record_type/1]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_plt, beam_tests, update_plt, run_plt_check,
          remove_plt, run_succ_typings, local_fun_same_as_callback,
          bad_dialyzer_attr, merge_plts, bad_record_type].

build_plt(Config) ->
  OutDir = ?config(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    ok   -> ok;
    fail -> ct:fail(plt_build_fail)
  end.

beam_tests(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Plt = filename:join(PrivDir, "beam_tests.plt"),
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
    [] = run_dialyzer(plt_build, [BeamFile], [{output_plt, Plt}]),
    ok.

run_plt_check(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Plt = filename:join(PrivDir, "run_plt_check.plt"),
    Mod1 = <<"
	      -module(run_plt_check1).
	     ">>,

    Mod2A = <<"
	       -module(run_plt_check2).
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1, run_plt_check1, []),
    {ok, BeamFile2} = compile(Config, Mod2A, run_plt_check2, []),
    [] = run_dialyzer(plt_build, [BeamFile1, BeamFile2], [{output_plt, Plt}]),

    Mod2B = <<"
	       -module(run_plt_check2).

	       -export([call/1]).

	       call(X) -> run_plt_check1:call(X).
	     ">>,

    {ok, BeamFile2} = compile(Config, Mod2B, run_plt_check2, []),

    % callgraph warning as run_plt_check2:call/1 makes a call to unexported
    % function run_plt_check1:call/1.
    [_] = run_dialyzer(plt_check, [], [{init_plt, Plt}]),

    ok.

run_succ_typings(Config) when is_list(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Plt = filename:join(PrivDir, "run_succ_typings.plt"),
    Mod1A = <<"
	       -module(run_succ_typings1).

	       -export([call/0]).

	       call() -> a.
	      ">>,

    {ok, BeamFile1} = compile(Config, Mod1A, run_succ_typings1, []),
    [] = run_dialyzer(plt_build, [BeamFile1], [{output_plt, Plt}]),

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
    [_] = run_dialyzer(succ_typings, [BeamFile2],
                       [{check_plt, false}, {init_plt, Plt}]),
    % warning not returned as run_succ_typings1 is updated in the PLT.
    [] = run_dialyzer(succ_typings, [BeamFile2],
                      [{check_plt, true}, {init_plt, Plt}]),

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

%% ERL-283, OTP-13979. As of OTP-14323 this test no longer does what
%% it is designed to do--the linter stops every attempt to run the
%% checks of Dialyzer's on bad dialyzer attributes. For the time
%% being, the linter's error message are checked instead. The test
%% needs to be updated when/if the Dialyzer can analyze Core Erlang
%% without compiling abstract code.
bad_dialyzer_attr(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Source = lists:concat([dial, ".erl"]),
    Filename = filename:join(PrivDir, Source),
    ok = dialyzer_common:check_plt(PrivDir),
    PltFilename = dialyzer_common:plt_file(PrivDir),
    Opts = [{files, [Filename]},
            {check_plt, false},
            {from, src_code},
            {init_plt, PltFilename}],

    Prog1 = <<"-module(dial).
               -dialyzer({no_return, [undef/0]}).">>,
    ok = file:write_file(Filename, Prog1),
    {dialyzer_error,
     "Analysis failed with error:\n" ++ Str1} =
        (catch dialyzer:run(Opts)),
    S1 = string:find(Str1, "dial.erl:2: function undef/0 undefined"),
    true = is_list(S1),

    Prog2 = <<"-module(dial).
               -dialyzer({no_return, [{undef,1,2}]}).">>,
    ok = file:write_file(Filename, Prog2),
    {dialyzer_error,
     "Analysis failed with error:\n" ++ Str2} =
        (catch dialyzer:run(Opts)),
    S2 = string:find(Str2, "dial.erl:2: badly formed dialyzer "
                           "attribute: {no_return,{undef,1,2}}"),
    true = is_list(S2),

    ok.

merge_plts(Config) ->
    %% A few checks of merging PLTs.
    fun() ->
            {Mod1, Mod2} = types(),
            {BeamFiles, Plt1, Plt2} = create_plts(Mod1, Mod2, Config),

            {dialyzer_error,
             "Could not merge PLTs since they are not disjoint"++_} =
                (catch run_dialyzer(succ_typings, BeamFiles,
                                    [{plts, [Plt1, Plt1]}])),
            [{warn_contract_types,_,_}] =
                run_dialyzer(succ_typings, BeamFiles,
                             [{warnings, [unknown]},
                              {plts, [Plt1, Plt2]}])
    end(),

    fun() ->
            {Mod1, Mod2} = callbacks(),
            {BeamFiles, Plt1, Plt2} = create_plts(Mod1, Mod2, Config),

            {dialyzer_error,
             "Could not merge PLTs since they are not disjoint"++_} =
                (catch run_dialyzer(succ_typings, BeamFiles,
                                    [{plts, [Plt1, Plt1]}])),
            [] =
                run_dialyzer(succ_typings, BeamFiles,
                             [{warnings, [unknown]},
                              {plts, [Plt1, Plt2]}])
    end(),

    ok.

types() ->
    Mod1 = <<"-module(merge_plts_1).
              -export([f/0]).
              -export_type([t/0]).
              -type t() :: merge_plts_2:t().
              -spec f() -> t().
              f() -> 1. % Not an atom().
	      ">>,
    Mod2 = <<"-module(merge_plts_2).
              -export_type([t/0]).
              -type t() :: atom().
	     ">>,
    {Mod1, Mod2}.

callbacks() -> % A very shallow test.
    Mod1 = <<"-module(merge_plts_1).
              -callback t() -> merge_plts_2:t().
	      ">>,
    Mod2 = <<"-module(merge_plts_2).
              -export_type([t/0]).
              -type t() :: atom().
	     ">>,
    {Mod1, Mod2}.

create_plts(Mod1, Mod2, Config) ->
    PrivDir = ?config(priv_dir, Config),
    Plt1 = filename:join(PrivDir, "merge_plts_1.plt"),
    Plt2 = filename:join(PrivDir, "merge_plts_2.plt"),
    ErlangBeam = erlang_beam(),

    {ok, BeamFile1} = compile(Config, Mod1, merge_plts_1, []),
    [] = run_dialyzer(plt_build, [ErlangBeam,BeamFile1], [{output_plt,Plt1}]),

    {ok, BeamFile2} = compile(Config, Mod2, merge_plts_2, []),
    [] = run_dialyzer(plt_build, [BeamFile2], [{output_plt, Plt2}]),
    {[BeamFile1, BeamFile2], Plt1, Plt2}.

%% End of merge_plts().

bad_record_type(Config) ->
    PrivDir = ?config(priv_dir, Config),
    Source = lists:concat([bad_record_type, ".erl"]),
    Filename = filename:join(PrivDir, Source),
    PltFilename = dialyzer_common:plt_file(PrivDir),

    Opts = [{files, [Filename]},
            {check_plt, false},
            {from, src_code},
            {init_plt, PltFilename}],

    Prog = <<"-module(bad_record_type).
              -export([r/0]).
              -record(r, {f = 3 :: integer()}).
              -spec r() -> #r{f :: atom()}.
              r() ->
                  #r{}.">>,
    ok = file:write_file(Filename, Prog),
    {dialyzer_error,
     "Analysis failed with error:\n" ++ Str} =
        (catch dialyzer:run(Opts)),
    P = string:str(Str,
                    "bad_record_type.erl:4: Illegal declaration of #r{f}"),
    true = P > 0,
    ok.

erlang_beam() ->
    case code:where_is_file("erlang.beam") of
        non_existing ->
            filename:join([code:root_dir(),
                           "erts", "preloaded", "ebin",
                           "erlang.beam"]);
        EBeam ->
            EBeam
    end.

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
