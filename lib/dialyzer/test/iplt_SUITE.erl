-module(iplt_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("dialyzer/src/dialyzer.hrl").
-include("dialyzer_test_constants.hrl").

-export([suite/0, all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         beam_tests/1,
         local_fun_same_as_callback/1,
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
         adding_legal_warnings_with_existing_stored_warnings_in_plt_results_in_new_warnings_being_printed/1,
         reading_from_one_plt_and_writing_to_another_does_not_mutate_the_input_plt/1,
         reading_from_and_writing_to_one_plt_mutates_it/1,
         beams_with_no_debug_info_are_rejected/1
        ]).

suite() ->
  [{timetrap, ?plt_timeout}].

all() -> [build_xdg_plt, beam_tests, 
          local_fun_same_as_callback,
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
          adding_legal_warnings_with_existing_stored_warnings_in_plt_results_in_new_warnings_being_printed,
          reading_from_one_plt_and_writing_to_another_does_not_mutate_the_input_plt,
          reading_from_and_writing_to_one_plt_mutates_it,
          beams_with_no_debug_info_are_rejected
          ].

init_per_testcase(_TestCase, Config) ->
  % Always run from a clean default PLT, so tests aren't dependent on what
  % happens to be in the default user cache when they start
  _ = file:delete(dialyzer_iplt:get_default_iplt_filename()),
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

build_xdg_plt(Config) ->
    TestHome = filename:join(?config(priv_dir, Config), ?FUNCTION_NAME),

    %% We change the $HOME of the emulator to run this test
    HomeEnv = case os:type() of
                  {win32, _} ->
                      [Drive | Path] = filename:split(TestHome),
                      [{"APPDATA", filename:join(TestHome,"AppData")},
                       {"HOMEDRIVE", Drive}, {"HOMEPATH", Path}];
                  _ ->
                      [{"HOME", TestHome}]
              end,

    {ok, Peer, Node} = ?CT_PEER(#{ env => HomeEnv }),

    erpc:call(
      Node,
      fun() ->
        ?assertMatch([], dialyzer:run(
                           [{analysis_type, incremental},
                            {apps, [erts]},
                            {warnings, [no_unknown]}])),
        ?assertMatch(
           {ok,_}, file:read_file(
                     filename:join(
                       filename:basedir(user_cache, "erlang"),
                       ".dialyzer_iplt")))
      end),

    peer:stop(Peer).

beam_tests(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Plt = filename:join(PrivDir, "beam_tests.iplt"),
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
    [] = run_dialyzer(incremental, [BeamFile], [{output_plt, Plt}]),
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
                                        "erts", "ebin",
                                        "erlang.beam"]);
                     EBeam ->
                         EBeam
                 end,
    Plt = filename:join(PrivDir, "plt_bad_behaviour.iplt"),
    Opts = [{from, byte_code}, {warnings, [no_unknown]}],
    [] = dialyzer:run([{analysis_type, incremental},
                       {files, [Beam, ErlangBeam]},
                       {output_plt, Plt}] ++ Opts),

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
        dialyzer:run([{analysis_type, incremental},
                      {files, [TestBeam, Beam, ErlangBeam]},
                      {init_plt, Plt}] ++ Opts),
    ok.



letrec_rvals(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Plt = filename:join(PrivDir, "letrec_rvals.iplt"),
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
    _ = run_dialyzer(incremental, [BeamFile], [{output_plt, Plt}]),
    ok.

missing_plt_file(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    PltFile = filename:join(PrivDir, "missing_plt_file.iplt"),
    Prog2 = <<"-module(missing_plt_file2).
              t() -> foo.">>,
    {ok, BeamFile2} = compile(Config, Prog2, missing_plt_file2, []),

    true = missing_plt_file_1(Config, PltFile, BeamFile2),
    true = missing_plt_file_2(Config, PltFile, BeamFile2),
    ok.

missing_plt_file_1(Config, PltFile, BeamFile2) ->
    BeamFile = create1(Config, PltFile),
    ok = file:delete(BeamFile),
    try incr(PltFile, BeamFile2), true
    catch throw:{dialyzer_error, _} -> false
    end.

create1(Config, PltFile) ->
    Prog = <<"-module(missing_plt_file).
              t() -> foo.">>,
    {ok, BeamFile} = compile(Config, Prog, missing_plt_file, []),
    Files = [BeamFile],
    _ = file:delete(PltFile),
    _ = dialyzer:run([{files,Files},
                       {init_plt, PltFile},
                       {output_plt, PltFile},
                       {analysis_type, incremental}]),
    BeamFile.

missing_plt_file_2(Config, PltFile, BeamFile2) ->
    BeamFile = create2(Config, PltFile),
    ok = file:delete(BeamFile),
    try incr(PltFile, BeamFile2), true
    catch throw:{dialyzer_error, _} -> false
    end.

create2(Config, PltFile) ->
    Prog = <<"-module(missing_plt_file).
              t() -> foo.">>,
    {ok, BeamFile} = compile(Config, Prog, missing_plt_file, []),
    Files = [BeamFile],
    _ = file:delete(PltFile),
    _ = dialyzer:run([{files,Files},
                       {output_plt, PltFile},
                       {analysis_type, incremental}]),
    BeamFile.

incr(PltFile, BeamFile2) ->
    Files = [BeamFile2],
    dialyzer:run([{files, Files},
                  {plts,[PltFile]},
                  {analysis_type, incremental}]).

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
    PltFile = filename:join(PrivDir, atom_to_list(TestName) ++ ".iplt"),
    {ok, DepsBeamFile} = compile(Config, type_deps, []),
    {ok, DependerBeamFile} = compile(Config, DependerSrc, depender, []),
    [] = run_dialyzer(incremental,
                      [DependerBeamFile, DepsBeamFile],
                      [{init_plt, PltFile}, {output_plt, PltFile}, {warnings, [no_unknown]}]),
    {_ResPlt, #iplt_info{mod_deps = DepsByModule}} = dialyzer_iplt:plt_and_info_from_file(PltFile),

    ActualTypeDepsInPlt =
      lists:sort(dict:to_list(dict:erase(erlang, DepsByModule))),
    ExpectedTypeDepsInPlt =
      lists:usort(ExpectedTypeDepsInPltUnsorted),

    ?assertEqual(
      ExpectedTypeDepsInPlt,
      ActualTypeDepsInPlt,
      {missing, ExpectedTypeDepsInPlt -- ActualTypeDepsInPlt,
       extra, ActualTypeDepsInPlt -- ExpectedTypeDepsInPlt}).

%% Builds the named module using the source in the iplt_SUITE_data dir
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
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    Opts = [{init_plt, [PltFile]}],
    OptsNoContractWarn = [{init_plt, [PltFile]}, {warnings, [no_contracts]}],
    {ok, Beam} = compile(Config, m_src_with_warning(), m, []),
    ?assertEqual(
       [],
       run_dialyzer(incremental, [Beam], OptsNoContractWarn)),
    ?assertMatch(
       [{warn_contract_types,
         {_, {5,4}},
         {invalid_contract,[m,updt,3,{[1],true},"([any()],term(),term()) -> [any()]","(map(),_,_) -> map()"]}}
       ],
       run_dialyzer(incremental, [Beam], Opts)).

removing_warning_apps_after_a_run_with_them_causes_any_warnings_for_the_removed_apps_not_to_be_reported(Config) ->
    PrivDir = ?config(priv_dir, Config),
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
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
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
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
    PltFile = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
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

reading_from_one_plt_and_writing_to_another_does_not_mutate_the_input_plt(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).
         -export([bar/0]).

         -spec bar() -> ok.
         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt1 = filename:join(PrivDir, "prog1.iplt"),
    ?assertMatch([], dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt1},
                       {from, byte_code},
                       {warnings, [no_unknown]}])),

    % Now PLT v1 should exist
    Plt1ContentsBefore = dialyzer_plt:get_all_contracts(dialyzer_iplt:from_file(Plt1)),
    ?assertMatch({ok, {contract,_,_,_}},
                 maps:find({foo,bar,0}, Plt1ContentsBefore)),
    ?assertMatch(error,
                 maps:find({foo,baz,0}, Plt1ContentsBefore)),

    Prog2 =
      <<"-module(foo).
         -export([bar/0, baz/0]).

         -spec bar() -> ok.
         bar() -> ok.

         -spec baz() -> ok.
         baz() -> ok.">>,
    {ok, Beam2} = compile(Config, Prog2, foo, []),

    Plt2 = filename:join(PrivDir, "prog2.iplt"),
    ?assertMatch([], dialyzer:run([{analysis_type, incremental},
                       {files, [Beam2]},
                       {init_plt, Plt1},
                       {output_plt, Plt2},
                       {from, byte_code},
                       {warnings, [no_unknown]}])),

    % Now PLT v1 should be the same, but PLT v2 should have the changes in it
    Plt1ContentsAfter = dialyzer_plt:get_all_contracts(dialyzer_iplt:from_file(Plt1)),
    ?assertMatch({ok, {contract,_,_,_}},
                 maps:find({foo,bar,0}, Plt1ContentsAfter)),
    ?assertMatch(error,
                 maps:find({foo,baz,0}, Plt1ContentsAfter)),

    Plt2Contents = dialyzer_plt:get_all_contracts(dialyzer_iplt:from_file(Plt2)),
    ?assertMatch({ok, {contract,_,_,_}},
                 maps:find({foo,bar,0}, Plt2Contents)),
    ?assertMatch({ok, {contract,_,_,_}},
                 maps:find({foo,baz,0}, Plt2Contents)).

reading_from_and_writing_to_one_plt_mutates_it(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Prog1 =
      <<"-module(foo).
         -export([bar/0]).

         -spec bar() -> ok.
         bar() -> ok.">>,
    {ok, Beam1} = compile(Config, Prog1, foo, []),

    Plt = filename:join(PrivDir, "mutate.iplt"),
    ?assertMatch([], dialyzer:run([{analysis_type, incremental},
                       {files, [Beam1]},
                       {init_plt, Plt},
                       {from, byte_code},
                       {warnings, [no_unknown]}])),

    % Now PLT should exist after running incremental mode
    PltContentsBefore = dialyzer_plt:get_all_contracts(dialyzer_iplt:from_file(Plt)),
    ?assertMatch({ok, {contract,_,_,_}},
                 maps:find({foo,bar,0}, PltContentsBefore)),
    ?assertMatch(error,
                 maps:find({foo,baz,0}, PltContentsBefore)),

    Prog2 =
      <<"-module(foo).
         -export([bar/0, baz/0]).

         -spec bar() -> ok.
         bar() -> ok.

         -spec baz() -> ok.
         baz() -> ok.">>,
    {ok, Beam2} = compile(Config, Prog2, foo, []),

    ?assertMatch([], dialyzer:run([{analysis_type, incremental},
                       {files, [Beam2]},
                       {init_plt, Plt},
                       {output_plt, Plt},
                       {from, byte_code},
                       {warnings, [no_unknown]}])),

    % Now PLT should have been mutated to contain the new version of module 'foo'
    PltContentsAfter = dialyzer_plt:get_all_contracts(dialyzer_iplt:from_file(Plt)),
    ?assertMatch({ok, {contract,_,_,_}}, maps:find({foo,bar,0}, PltContentsAfter)),
    ?assertMatch({ok, {contract,_,_,_}}, maps:find({foo,baz,0}, PltContentsAfter)).

beams_with_no_debug_info_are_rejected(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Plt = filename:join(PrivDir, atom_to_list(?FUNCTION_NAME) ++ ".iplt"),
    Src = <<"
        -module(my_list).

        -export([my_size/1]).

        -spec my_size(list()) -> non_neg_integer().
        my_size([]) ->
            0;
        my_size({N,_}) ->
            N;
        my_size([_|T]) ->
            1+my_size(T).
    ">>,
    SrcFilename = filename:join([PrivDir, "my_list.erl"]),
    ok = file:write_file(SrcFilename, Src),
    Opts = [{outdir, PrivDir}], % No debug info enabled
    {ok, Module} = compile:file(SrcFilename, Opts),
    BeamFile = filename:join([PrivDir, lists:concat([Module, ".beam"])]),

    ?assertThrow(
       {dialyzer_error, "Could not compute MD5 for .beam (debug_info error) - did you forget to set the debug_info compilation option? " ++ _},
       run_dialyzer(incremental, [BeamFile], [{output_plt, Plt}])).
