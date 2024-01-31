-module(cplt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("dialyzer/src/dialyzer.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0,
         build_plt/1, beam_tests/1, update_plt/1,
         local_fun_same_as_callback/1,
         remove_plt/1, run_plt_check/1, run_succ_typings/1,
         bad_dialyzer_attr/1, merge_plts/1, bad_record_type/1,
         letrec_rvals/1,
         missing_plt_file/1,
         build_xdg_plt/1,
         mod_dep_from_behaviour/1,
         mod_dep_from_record_definition_field_value_default_used/1,
         mod_dep_from_record_definition_field_value_default_unused/1,
         mod_dep_from_record_definition_field_type/1,
         mod_dep_from_overloaded_callback/1,
         mod_dep_from_exported_overloaded_fun_spec/1,
         mod_dep_from_unexported_overloaded_fun_spec/1,
         mod_dep_from_callback_constraint/1,
         mod_dep_from_unexported_fun_spec_constraint/1,
         mod_dep_from_exported_fun_spec_constraint/1,
         mod_dep_from_exported_type/1,
         mod_dep_from_callback_return/1,
         mod_dep_from_callback_args/1,
         mod_dep_from_unexported_opaque_type_args/1,
         mod_dep_from_exported_opaque_type_args/1,
         mod_dep_from_unexported_opaque_type/1,
         mod_dep_from_exported_opaque_type/1,
         mod_dep_from_unexported_type_args/1,
         mod_dep_from_exported_type_args/1,
         mod_dep_from_unexported_fun_spec_args/1,
         mod_dep_from_exported_fun_spec_args/1,
         mod_dep_from_unexported_fun_spec_return/1,
         mod_dep_from_exported_fun_spec_return/1,
         mod_dep_from_unexported_type/1,
         adding_warning_apps_after_a_run_without_them_causes_any_new_warnings_to_be_reported/1,
         removing_warning_apps_after_a_run_with_them_causes_any_warnings_for_the_removed_apps_not_to_be_reported/1,
         removing_legal_warnings_with_existing_stored_warnings_in_plt_does_not_result_in_old_warnings_being_printed/1,
         adding_legal_warnings_with_existing_stored_warnings_in_plt_results_in_new_warnings_being_printed/1
        ]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_plt, build_xdg_plt, beam_tests, update_plt, run_plt_check,
          remove_plt, run_succ_typings, local_fun_same_as_callback,
          bad_dialyzer_attr, merge_plts, bad_record_type,
          letrec_rvals,
          missing_plt_file,
          mod_dep_from_behaviour,
          mod_dep_from_record_definition_field_value_default_used,
          mod_dep_from_record_definition_field_value_default_unused,
          mod_dep_from_record_definition_field_type,
          mod_dep_from_overloaded_callback,
          mod_dep_from_exported_overloaded_fun_spec,
          mod_dep_from_unexported_overloaded_fun_spec,
          mod_dep_from_callback_constraint,
          mod_dep_from_unexported_fun_spec_constraint,
          mod_dep_from_exported_fun_spec_constraint,
          mod_dep_from_exported_type,
          mod_dep_from_callback_return,
          mod_dep_from_callback_args,
          mod_dep_from_unexported_opaque_type_args,
          mod_dep_from_exported_opaque_type_args,
          mod_dep_from_unexported_opaque_type,
          mod_dep_from_exported_opaque_type,
          mod_dep_from_unexported_type_args,
          mod_dep_from_exported_type_args,
          mod_dep_from_unexported_fun_spec_args,
          mod_dep_from_exported_fun_spec_args,
          mod_dep_from_unexported_fun_spec_return,
          mod_dep_from_exported_fun_spec_return,
          mod_dep_from_unexported_type,
          adding_warning_apps_after_a_run_without_them_causes_any_new_warnings_to_be_reported,
          removing_warning_apps_after_a_run_with_them_causes_any_warnings_for_the_removed_apps_not_to_be_reported,
          removing_legal_warnings_with_existing_stored_warnings_in_plt_does_not_result_in_old_warnings_being_printed,
          adding_legal_warnings_with_existing_stored_warnings_in_plt_results_in_new_warnings_being_printed
          ].

build_plt(Config) ->
  OutDir = proplists:get_value(priv_dir, Config),
  case dialyzer_common:check_plt(OutDir) of
    ok   -> ok;
    fail -> ct:fail(plt_build_fail)
  end.

build_xdg_plt(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    %% We change the $HOME of the emulator to run this test
    HomeEnv = case os:type() of
                  {win32, _} ->
                      [Drive | Path] = filename:split(TestHome),
                      [{"APPDATA", filename:join(TestHome,"AppData")},
                       {"HOMEDRIVE", Drive}, {"HOMEPATH", filename:join(Path)}];
                  _ ->
                      [{"HOME", TestHome}]
              end,

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),

    erpc:call(
      Node,
      fun() ->
        ?assertMatch([], dialyzer:run(
                           [{analysis_type, plt_build},
                            {apps, [erts]},
                            {warnings, [no_unknown]}])),
        ?assertMatch(
           {ok,_}, file:read_file(
                     filename:join(
                       filename:basedir(user_cache, "erlang"),
                       ".dialyzer_plt")))
      end),

    peer:stop(Peer).

beam_tests(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Plt = filename:join(PrivDir, "beam_tests.plt"),
    Src = <<"
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
    {ok, BeamFile} = compile(Config, Src, no_auto_import, Opts),
    [] = run_dialyzer(plt_build, [BeamFile], [{output_plt, Plt}]),
    ok.

run_plt_check(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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

    %% callgraph warning as run_plt_check2:call/1 makes a call to unexported
    %% function run_plt_check1:call/1.
    [_] = run_dialyzer(plt_check, [], [{init_plt, Plt}]),

    ok.

run_succ_typings(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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
    %% contract types warning as run_succ_typings2:call/0 makes a call to
    %% run_succ_typings1:call/0, which returns a (not b) in the PLT.
    [_] = run_dialyzer(succ_typings, [BeamFile2],
                       [{check_plt, false}, {init_plt, Plt}]),
    %% warning not returned as run_succ_typings1 is updated in the PLT.
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
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 = <<"-module(plt_gc).
               -export([one/0]).
               one() ->
                  one.">>,
    {ok, Beam} = compile(Config, Prog1, plt_gc, []),

    ErlangBeam = case code:where_is_file("erlang.beam") of
                     non_existing ->
                         filename:join([code:root_dir(),
                                        "erts", "ebin", "erlang.beam"]);
                     EBeam ->
                         EBeam
                 end,
    Plt = filename:join(PrivDir, "plt_gc.plt"),
    Opts = [{check_plt, true}, {from, byte_code}],
    [] = dialyzer:run([{analysis_type, plt_build},
                       {files, [Beam, ErlangBeam]},
                       {output_plt, Plt},
                       {warnings, [no_unknown]}] ++ Opts),

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
    [{warn_callgraph, {_Filename, {5,19}}, {call_to_missing, [plt_gc,one,0]}}] =
        dialyzer:run([{analysis_type, succ_typings},
                      {files, [TestBeam]},
                      {init_plt, Plt}] ++ Opts),
    ok.


%%% If a behaviour module contains an non-exported function with the same name
%%% as one of the behaviour's callbacks, the callback info was inadvertently
%%% deleted from the PLT as the dialyzer_plt:delete_list/2 function was cleaning
%%% up the callback table. This bug was reported by Brujo Benavides.

local_fun_same_as_callback(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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
                       {output_plt, Plt},
                       {warnings, [no_unknown]}] ++ Opts),

    Prog2 =
        <<"-module(bad_child).
           -behaviour(bad_behaviour).

           -export([bad/0]).

           %% @doc This function incorrectly implements bad_behaviour.
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
    PrivDir = proplists:get_value(priv_dir, Config),
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
                      {output_plt, Plt},
                      {warnings, [no_unknown]}] ++ Opts),

    [] = dialyzer:run([{init_plt, Plt},
                       {files, [Beam2]},
                       {analysis_type, plt_remove},
                       {warnings, [no_unknown]}]),

    [] =  dialyzer:run([{analysis_type, succ_typings},
                        {files, [Beam1]},
                        {init_plt, Plt},
                        {warnings, [no_unknown]}] ++ Opts),
    ok.

%% ERL-283, OTP-13979. As of OTP-14323 this test no longer does what
%% it is designed to do--the linter stops every attempt to run the
%% checks of Dialyzer's on bad dialyzer attributes. For the time
%% being, the linter's error message are checked instead. The test
%% needs to be updated when/if the Dialyzer can analyze Core Erlang
%% without compiling abstract code.
bad_dialyzer_attr(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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
    S1 = string:find(Str1, "dial.erl:2:17: function undef/0 undefined"),
    true = is_list(S1),

    Prog2 = <<"-module(dial).
               -dialyzer({no_return, [{undef,1,2}]}).">>,
    ok = file:write_file(Filename, Prog2),
    {dialyzer_error,
     "Analysis failed with error:\n" ++ Str2} =
        (catch dialyzer:run(Opts)),
    S2 = string:find(Str2, "dial.erl:2:17: badly formed dialyzer "
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

callbacks() -> %% A very shallow test.
    Mod1 = <<"-module(merge_plts_1).
              -callback t() -> merge_plts_2:t().
	      ">>,
    Mod2 = <<"-module(merge_plts_2).
              -export_type([t/0]).
              -type t() :: atom().
	     ">>,
    {Mod1, Mod2}.

create_plts(Mod1, Mod2, Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
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
    PrivDir = proplists:get_value(priv_dir, Config),
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
                    "bad_record_type.erl:4:16: Illegal declaration of #r{f}"),
    true = P > 0,
    ok.

letrec_rvals(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Plt = filename:join(PrivDir, "letrec_rvals.plt"),
    Prog = <<"
-module(letrec_rvals).

-export([demo_fun/1]).

demo_fun(_Arg) ->
    case ok of
        _ ->
            _Res = _Arg,
            [ ok || _ <- [] ]
    end,
    _Res.

handle_info() ->
    case chids_to_audit() of
        {ChIds, St2} ->
            [ ChId || ChId <- ChIds ],
            ok
    end,
    check_done(St2).

chids_to_audit() ->
    some_module:get_audit_list().

check_done(_) ->
    ok.
    ">>,
    {ok, BeamFile} = compile(Config, Prog, letrec_rvals, []),
    [] = run_dialyzer(plt_build, [BeamFile], [{output_plt, Plt}]),
    ok.

%% GH-4501
missing_plt_file(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    PltFile = filename:join(PrivDir, "missing_plt_file.plt"),
    Prog2 = <<"-module(missing_plt_file2).
              t() -> foo.">>,
    {ok, BeamFile2} = compile(Config, Prog2, missing_plt_file2, []),

    true = missing_plt_file_1(Config, PltFile, BeamFile2),
    true = missing_plt_file_2(Config, PltFile, BeamFile2),
    true = missing_plt_file_3(),
    ok.

missing_plt_file_1(Config, PltFile, BeamFile2) ->
    BeamFile = create(Config, PltFile),
    ok = file:delete(BeamFile),
    try succ(PltFile, BeamFile2), false
    catch throw:{dialyzer_error, _} -> true
    end.

missing_plt_file_2(Config, PltFile, BeamFile2) ->
    BeamFile = create(Config, PltFile),
    ok = file:delete(BeamFile),

    Cmd = "dialyzer -q --plt " ++ PltFile ++ " " ++ BeamFile2,
    io:format("Cmd `~p'\n", [Cmd]),
    "\ndialyzer: File not found: " ++ _ = os:cmd(Cmd),

    try check(PltFile, BeamFile2), false
    catch throw:{dialyzer_error, _} -> true
    end.

missing_plt_file_3() ->
    try dialyzer_cplt:from_file("no_such_file"), false
    catch throw:{dialyzer_error, _} -> true
    end.

create(Config, PltFile) ->
    Prog = <<"-module(missing_plt_file).
              t() -> foo.">>,
    {ok, BeamFile} = compile(Config, Prog, missing_plt_file, []),
    Files = [BeamFile],
    _ = file:delete(PltFile),
    [] = dialyzer:run([{files,Files},
                       {output_plt, PltFile},
                       {analysis_type, plt_build},
                       {warnings, [no_unknown]}]),
    BeamFile.

succ(PltFile, BeamFile2) ->
    Files = [BeamFile2],
    dialyzer:run([{files, Files},
                  {plts,[PltFile]},
                  {analysis_type, succ_typings}]).

check(PltFile, _BeamFile2) ->
    dialyzer:run([{plts,[PltFile]},
                  {analysis_type, plt_check}]).

mod_dep_from_record_definition_field_value_default_unused(Config) ->
    DependerSrc = <<"
      -module(depender).

      -record(my_record,
        { num_field = type_deps:get_num() :: number(),
          str_field,
          bool_field
        }).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, []}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_record_definition_field_value_default_used(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export([f/0]).

      -record(my_record,
        { num_field = type_deps:get_num() :: number(),
          str_field,
          bool_field
        }).

      f() -> #my_record{str_field = \"foo\", bool_field = true}. % type_deps:get_num() used implicitly here
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_behaviour(Config) ->
    DependerSrc = <<"
      -module(depender).
      -behaviour(type_deps).
      -export([quux/1]).

      quux(N) -> N + 1. % Depends on behaviour module to check the callback implementation
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_record_definition_field_type(Config) ->
    DependerSrc = <<"
      -module(depender).

      -record(my_record,
        { num_field = 1 :: type_deps:number_like(),
          str_field,
          bool_field
        }).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_overloaded_callback(Config) ->
    DependerSrc1 = <<"
      -module(depender).

      -callback f(string()) -> string()
                ; (type_deps:number_like()) -> type_deps:number_like().
      ">>,
    DependerSrc2 = <<"
      -module(depender).

      -callback f(type_deps:number_like()) -> type_deps:number_like()
                ; (string()) -> string().
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc1, ExpectedTypeDepsInPlt),
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc2, ExpectedTypeDepsInPlt).

mod_dep_from_exported_overloaded_fun_spec(Config) ->
    DependerSrc1 = <<"
      -module(depender).
      -export([f/1]).

      -spec f({a, atom()}) -> atom()
            ; ({n, type_deps:number_like()}) -> type_deps:number_like().
      f({a, X}) when is_atom(X) -> X;
      f({n, X}) when is_number(X) -> X.
      ">>,
    DependerSrc2 = <<"
      -module(depender).
      -export([f/1]).

      -spec f({n, type_deps:number_like()}) -> type_deps:number_like()
            ; ({a, atom()}) -> atom().
      f({n, X}) when is_number(X) -> X;
      f({a, X}) when is_atom(X) -> X.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc1, ExpectedTypeDepsInPlt),
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc2, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_overloaded_fun_spec(Config) ->
    DependerSrc1 = <<"
      -module(depender).
      -compile({nowarn_unused_function, [f/1]}).

      -spec f({a, atom()}) -> atom()
            ; ({n, type_deps:number_like()}) -> type_deps:number_like().
      f({a, X}) when is_atom(X) -> X;
      f({n, X}) when is_number(X) -> X.
      ">>,
    DependerSrc2 = <<"
      -module(depender).
      -compile({nowarn_unused_function, [f/1]}).

      -spec f({n, type_deps:number_like()}) -> type_deps:number_like()
            ; ({a, atom()}) -> atom().
      f({n, X}) when is_number(X) -> X;
      f({a, X}) when is_atom(X) -> X.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc1, ExpectedTypeDepsInPlt),
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc2, ExpectedTypeDepsInPlt).

mod_dep_from_callback_constraint(Config) ->
    DependerSrc1 = <<"
      -module(depender).

      -callback f(X) -> string() when X :: type_deps:number_like().
      ">>,
    DependerSrc2 = <<"
      -module(depender).

      -callback f(X :: type_deps:number_like()) -> string().
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc1, ExpectedTypeDepsInPlt),
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc2, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_fun_spec_constraint(Config) ->
    DependerSrc = <<"
      -module(depender).
      -compile({nowarn_unused_function, [f/1]}).

      -spec f(N) -> number() when N :: type_deps:number_like().
      f(N) -> N.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_fun_spec_constraint(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export([f/1]).

      -spec f(N) -> number() when N :: type_deps:number_like().
      f(N) -> N.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_callback_return(Config) ->
    DependerSrc = <<"
      -module(depender).

      -callback f(string()) -> type_deps:number_like().
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_callback_args(Config) ->
    DependerSrc = <<"
      -module(depender).

      -callback f(type_deps:number_like()) -> string().
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_opaque_type(Config) ->
    DependerSrc = <<"
      -module(depender).

      -opaque my_type() :: {string(), type_deps:number_like()}.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_opaque_type(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export_type([my_type/0]).

      -opaque my_type() :: {string(), type_deps:number_like()}.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_opaque_type_args(Config) ->
    DependerSrc = <<"
      -module(depender).

      -type my_type() :: type_deps:my_opaque(number()).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_opaque_type_args(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export_type([my_type/0]).

      -type my_type() :: type_deps:my_opaque(number()).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_type_args(Config) ->
    DependerSrc = <<"
      -module(depender).

      -type my_type() :: {string(), type_deps:number_like()}.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_type_args(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export_type([my_type/0]).

      -type my_type() :: {string(), type_deps:number_like()}.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_fun_spec_args(Config) ->
    DependerSrc = <<"
      -module(depender).
      -compile({nowarn_unused_function, [f/1]}).

      -spec f(type_deps:number_like()) -> number().
      f(N) -> N.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_fun_spec_args(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export([f/1]).

      -spec f(N :: type_deps:number_like()) -> number().
      f(N) -> N.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_fun_spec_return(Config) ->
    DependerSrc = <<"
      -module(depender).
      -compile({nowarn_unused_function, [f/0]}).

      -spec f() -> type_deps:number_like().
      f() -> 1.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_fun_spec_return(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export([f/0]).

      -spec f() -> type_deps:number_like().
      f() -> 1.
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_unexported_type(Config) ->
    DependerSrc = <<"
      -module(depender).

      -type my_type() :: type_deps:list_like(number()).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

mod_dep_from_exported_type(Config) ->
    DependerSrc = <<"
      -module(depender).
      -export_type([my_type/0]).

      -type my_type() :: type_deps:list_like(number()).
      ">>,
    ExpectedTypeDepsInPlt = [{depender, []}, {type_deps, [depender]}],
    ok = check_plt_deps(Config, ?FUNCTION_NAME, DependerSrc, ExpectedTypeDepsInPlt).

check_plt_deps(Config, TestName, DependerSrc, ExpectedTypeDepsInPltUnsorted) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(TestName) ++ ".plt"),
    {ok, DepsBeamFile} = compile(Config, type_deps, []),
    {ok, DependerBeamFile} = compile(Config, DependerSrc, depender, []),
    [] = run_dialyzer(plt_build, [DependerBeamFile, DepsBeamFile], [{output_plt, PltFile}]),
    {_ResPlt, #plt_info{mod_deps = DepsByModule}} = dialyzer_cplt:plt_and_info_from_file(PltFile),

    ActualTypeDepsInPlt =
      lists:sort(dict:to_list(dict:erase(erlang, DepsByModule))),
    ExpectedTypeDepsInPlt =
      lists:usort(ExpectedTypeDepsInPltUnsorted),

    ?assertEqual(
      ExpectedTypeDepsInPlt,
      ActualTypeDepsInPlt,
      {missing, ExpectedTypeDepsInPlt -- ActualTypeDepsInPlt,
       extra, ActualTypeDepsInPlt -- ExpectedTypeDepsInPlt}).

erlang_beam() ->
    case code:where_is_file("erlang.beam") of
        non_existing ->
            filename:join([code:root_dir(),
                           "erts", "preloaded", "ebin",
                           "erlang.beam"]);
        EBeam ->
            EBeam
    end.

%% Builds the named module using the source in the cplt_SUITE_data dir
compile(Config, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    DataDir = proplists:get_value(data_dir,Config),
    SrcFilename = filename:join([DataDir, Source]),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(SrcFilename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

%% Builds the named module using the literal source given
compile(Config, Prog, Module, CompileOpts) ->
    Source = lists:concat([Module, ".erl"]),
    PrivDir = proplists:get_value(priv_dir,Config),
    Filename = filename:join([PrivDir, Source]),
    ok = file:write_file(Filename, Prog),
    Opts = [{outdir, PrivDir}, debug_info | CompileOpts],
    {ok, Module} = compile:file(Filename, Opts),
    {ok, filename:join([PrivDir, lists:concat([Module, ".beam"])])}.

run_dialyzer(Analysis, Files, Opts) ->
    dialyzer:run([{analysis_type, Analysis},
                  {files, Files},
                  {from, byte_code},
                  {warnings, [no_unknown]} |
                  Opts]).

m_src_without_warning() -> <<"
  -module(m).
  -export([updt/3]).

  updt(X, K, V) -> X#{ K => V }.
  ">>.

m_src_with_warning() -> <<"
  -module(m).
  -export([updt/3]).

  -spec updt(list(), term(), term()) -> list(). % Warning: Spec is wrong! Function takes a map, not a list
  updt(X, K, V) -> X#{ K => V }.
  ">>.

adding_warning_apps_after_a_run_without_them_causes_any_new_warnings_to_be_reported(Config) ->
    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".plt"),
    Opts = [{init_plt, [PltFile]}],
    OptsNoContractWarn = [{init_plt, [PltFile]}, {warnings, [no_contracts]}],
    {ok, Beam} = compile(Config, m_src_with_warning(), m, []),
    [] = run_dialyzer(incremental, [Beam], OptsNoContractWarn),
    ?assertMatch(
      [{warn_contract_types,
          {_, {5,4}},
          {invalid_contract,[m,updt,3,{[1],true},"([any()],term(),term()) -> [any()]","(map(),_,_) -> map()"]}}
      ],
      run_dialyzer(incremental, [Beam], Opts)).

removing_warning_apps_after_a_run_with_them_causes_any_warnings_for_the_removed_apps_not_to_be_reported(Config) ->
    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".plt"),
    Opts = [{init_plt, [PltFile]}],
    OptsNoContractWarn = [{init_plt, [PltFile]}, {warnings, [no_contracts]}],
    {ok, Beam} = compile(Config, m_src_with_warning(), m, []),
    ?assertMatch(
      [{warn_contract_types,
          {_, {5,4}},
          {invalid_contract,[m,updt,3,{[1],true},"([any()],term(),term()) -> [any()]","(map(),_,_) -> map()"]}}
      ],
      run_dialyzer(incremental, [Beam], Opts)),
    ?assertEqual(
       [],
       run_dialyzer(incremental, [Beam], OptsNoContractWarn)).

removing_legal_warnings_with_existing_stored_warnings_in_plt_does_not_result_in_old_warnings_being_printed(Config) ->
    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".plt"),
    Opts = [{init_plt, [PltFile]}],
    {ok, BeamFileBefore} = compile(Config, m_src_with_warning(), m, []),
    ?assertMatch(
      [{warn_contract_types,
          {_, {5,4}},
          {invalid_contract,[m,updt,3,{[1],true},"([any()],term(),term()) -> [any()]","(map(),_,_) -> map()"]}}
      ],
      run_dialyzer(incremental, [BeamFileBefore], Opts)),
    {ok, BeamFileAfter} = compile(Config, m_src_without_warning(), m, []),
    ?assertEqual(
       [],
       run_dialyzer(incremental, [BeamFileAfter], Opts)).

adding_legal_warnings_with_existing_stored_warnings_in_plt_results_in_new_warnings_being_printed(Config) ->
    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".plt"),
    Opts = [{init_plt, [PltFile]}],
    {ok, BeamFileAfter} = compile(Config, m_src_without_warning(), m, []),
    ?assertEqual(
       [],
       run_dialyzer(incremental, [BeamFileAfter], Opts)),
    {ok, BeamFileBefore} = compile(Config, m_src_with_warning(), m, []),
    ?assertMatch(
       [{warn_contract_types,
        {_, {5,4}},
        {invalid_contract,[m,updt,3,{[1],true},"([any()],term(),term()) -> [any()]","(map(),_,_) -> map()"]}}
       ],
       run_dialyzer(incremental, [BeamFileBefore], Opts)).
