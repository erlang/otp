%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(ct_doctest_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([api_branches/1, module_result_modes/1,
         docs_filtering_and_error_formatting/1, parser_prompt_parsing/1,
         runtime_failure_matching/1, parse_rewrite_helpers/1, file_support/1,
         external_parser/1, module/1, type_and_callback_docs/1, verbose_option/1,
         skipped_blocks_option/1,
         integration_smoke/1]).

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [api_branches,
     module_result_modes,
     docs_filtering_and_error_formatting,
     parser_prompt_parsing,
     runtime_failure_matching,
     parse_rewrite_helpers,
     file_support,
     external_parser,
     module,
     type_and_callback_docs,
     verbose_option,
     skipped_blocks_option,
     integration_smoke].

init_per_suite(Config) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Files = lists:sort(filelib:wildcard(filename:join(DataDir,
                                                      "ct_doctest_*_mod.erl"))),
    Modules = [compile_fixture(File, PrivDir) || File <- Files],
    true = code:add_patha(PrivDir),
    [{fixture_modules, Modules} | Config].

end_per_suite(Config) ->
    Modules = proplists:get_value(fixture_modules, Config, []),
    lists:foreach(fun(Module) ->
                          code:purge(Module),
                          code:delete(Module)
                  end, Modules),
    ok.

api_branches(_Config) ->
    {error, _} = ct_doctest:module(ct_doctest_missing_mod),
    {error, unsupported_format} = ct_doctest:module(ct_doctest_unsupported_format_mod).

module_result_modes(_Config) ->
    ok = ct_doctest:module(ct_doctest_none_mod),
    {comment, _} = ct_doctest:module(ct_doctest_no_tests_mod),
    expect_error_count(ct_doctest_module_doc_parse_error_mod, [], 1,
                       ["A test failed in moduledoc on line 0",
                        "syntax error before: ')'"]).

docs_filtering_and_error_formatting(_Config) ->
    expect_error_count(ct_doctest_module_doc_value_error_mod, [], 1,
                       ["A test failed in moduledoc",
                        "no match of right hand side value 2"]),
    expect_error_count(ct_doctest_function_parse_error_mod, [], 1,
                       ["A test failed in function f/0 on line 0",
                        "syntax error before: ')'"]),
    expect_error_count(ct_doctest_function_value_error_mod, [], 1,
                       ["A test failed in function f/0",
                        "no match of right hand side value 4"]),
    expect_error_count(ct_doctest_bad_line_numbers_mod, [], 1,
                       ["A test failed in moduledoc",
                        "Bad prompt number 3; expected 2"]).

parser_prompt_parsing(_Config) ->
    expect_error_count(ct_doctest_prompt_parser_mod, [], 3,
                       ["A test failed in function h/0",
                        "Bad prompt number 3; expected 2",
                        "A test failed in function g/0",
                        "Bad prompt number 2; expected 1",
                        "A test failed in function f/0"]),
    ok = ct_doctest:module(ct_doctest_non_erlang_block_mod).

runtime_failure_matching(_Config) ->
    ok = ct_doctest:module(ct_doctest_failure_match_mod),
    expect_error_count(ct_doctest_failure_unexpected_success_mod, [], 1,
                       ["A test failed in function f/0",
                        "Expected failure got ok"]),
    expect_exception(ct_doctest_failure_mismatch_mod, [], error, badarg).

parse_rewrite_helpers(_Config) ->
    expect_error_count(ct_doctest_parse_rewrite_mod, [{verbose, true}], 2,
                       ["A test failed in function h/0",
                        "A test failed in function g/0",
                        "illegal_pattern"]),
    expect_error_count(ct_doctest_scan_error_mod, [], 1,
                       ["A test failed in function f/0 on line 0",
                        "unterminated string"]).

file_support(Config) ->
    DataDir = ?config(data_dir, Config),
    Bindings = #{'Prebound' => hello},
    ParseErrorFile = filename:join(DataDir, "doctest_parse_error.md"),
    ok = ct_doctest:file(filename:join(DataDir, "doctest_ok.md"), Bindings, []),
    expect_error_count(filename:join(DataDir, "doctest_fail.md"), [], 1),
    expect_error_count(ParseErrorFile, [], 1),
    {error, enoent} = ct_doctest:file(filename:join(DataDir, "missing_*.md"), []).

module(_Config) ->

    ExpectedSubstrings = ["unknown:2: unterminated atom starting with 'ok.\\n'",
                          "unknown:2: syntax error before: ok",
                          "test.erl:2: function f/0 undefined"],

    expect_error_count(ct_doctest_module_mod, [], 3, ExpectedSubstrings).

external_parser(Config) ->
    DataDir = ?config(data_dir, Config),
    ParserOpt = {parser, fun ct_doctest_external_parser_mod:parse_doc/1},
    ok = ct_doctest:module(ct_doctest_external_parser_mod, [ParserOpt]),
    ok = ct_doctest:file(filename:join(DataDir, "custom_parser.txt"),
                         [ParserOpt]),
    expect_error_count(ct_doctest_external_parser_mod, [{parser, not_a_fun}], 1,
                       ["Invalid parser provided: not_a_fun"]),
    expect_error_count(ct_doctest_external_parser_mod,
                       [{parser, fun(_) -> {error, bad_parser} end}], 1,
                       ["Parser returned an error: bad_parser"]),
    expect_error_count(ct_doctest_external_parser_mod,
                       [{parser, fun(_) -> bad_result end}], 1,
                       ["Parser returned invalid result: bad_result"]),
    expect_error_count(ct_doctest_external_parser_mod,
                       [{parser, fun(_) -> [not_binary] end}], 1,
                       ["Invalid code block: not_binary"]),
    expect_exception(filename:join(DataDir, "doctest_ok.md"),
                     [{parser, fun(_) -> erlang:error(boom) end}],
                     error, boom).

type_and_callback_docs(_Config) ->
    ok = ct_doctest:module(ct_doctest_type_callback_mod),
    expect_error_count(ct_doctest_type_callback_value_error_mod, [], 2,
                       ["A test failed in type sample/0",
                        "A test failed in callback sample_cb/1"]).

verbose_option(Config) ->
    DataDir = ?config(data_dir, Config),
    Bindings = [{'Prebound', hello}],
    ok = ct_doctest:module(ct_doctest_type_callback_mod, [{verbose, true}]),
    expect_error_count(ct_doctest_type_callback_value_error_mod,
                       [{verbose, true}], 2,
                       ["A test failed in type sample/0",
                        "A test failed in callback sample_cb/1"]),
    ok = ct_doctest:module(ct_doctest_skipped_block_mod,
                           [{skipped_blocks, 1}, {verbose, true}]),
    ok = ct_doctest:file(filename:join(DataDir, "doctest_ok.md"), Bindings,
                         [{verbose, true}]).

skipped_blocks_option(_Config) ->
    ok = ct_doctest:module(ct_doctest_skipped_block_mod),
    ok = ct_doctest:module(ct_doctest_skipped_block_mod, [{skipped_blocks, false}]),
    ok = ct_doctest:module(ct_doctest_skipped_block_mod,
                           [{skipped_blocks, 1}]),
    %% Wrong skipped_blocks count should raise an error.
    try ct_doctest:module(ct_doctest_skipped_block_mod, [{skipped_blocks, 0}]) of
        _ -> ct:fail(expected_error)
    catch
        error:{unexpected_skipped_blocks, 0, 1} -> ok
    end.

integration_smoke(_Config) ->
    Bindings = [{moduledoc, [{'Prebound', hello}]}],
    ct_doctest:module(ct_doctest, Bindings, [{skipped_blocks, 8},
                                             {verbose, true}]).

compile_fixture(File, OutDir) ->
    Module = list_to_atom(filename:basename(File, ".erl")),
    {ok, Module} = compile:file(File, [debug_info,
                                       {outdir, OutDir},
                                       report_errors,
                                       report_warnings]),
    {module, Module} = code:load_abs(filename:join(OutDir, atom_to_list(Module))),
    Module.

expect_error_count(Module, Bindings, ExpectedErrors) ->
    expect_error_count(Module, Bindings, ExpectedErrors, []).
expect_error_count(Module, Bindings, ExpectedErrors, ExpectedSubstrings) ->
    ct:capture_start(),
    try run_target(Module, Bindings) of
        Result ->
            ct:fail({expected_error_count, ExpectedErrors, got_result, Result})
    catch
        error:{ExpectedErrors, errors} ->
            ct:capture_stop(),
            OutputData = ct:capture_get(),
            lists:foreach(fun(Expected) ->
                                case string:find(OutputData, Expected) of
                                    nomatch ->
                                        ct:fail({expected_output, Expected, got, OutputData});
                                    _ ->
                                        ok
                                end
                          end, ExpectedSubstrings),
            ok;
        Class:Reason:Stacktrace ->
            ct:fail({unexpected_exception, Module,
                     {Class, Reason}, Stacktrace})
    end.

expect_exception(Module, Bindings, Class, Reason) ->
    try run_target(Module, Bindings) of
        Result ->
            ct:fail({expected_exception, {Class, Reason}, got_result, Result})
    catch
        Class:Reason ->
            ok;
        OtherClass:OtherReason:Stacktrace ->
            ct:fail({unexpected_exception, Module,
                     {OtherClass, OtherReason}, Stacktrace})
    end.

run_target(Target, []) when is_atom(Target) ->
    ct_doctest:module(Target);
run_target(Target, Options) when is_atom(Target) ->
    ct_doctest:module(Target, Options);
run_target(Target, []) ->
    ct_doctest:file(Target);
run_target(Target, Options) ->
    ct_doctest:file(Target, Options).
