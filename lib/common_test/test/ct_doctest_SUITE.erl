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
         runtime_failure_matching/1, parse_rewrite_helpers/1,
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
    {error, _} = ct_doctest:test(ct_doctest_missing_mod, []),
    {error, unsupported_format} = ct_doctest:test(ct_doctest_unsupported_format_mod, []).

module_result_modes(_Config) ->
    ok = ct_doctest:test(ct_doctest_none_mod, []),
    {comment, _} = ct_doctest:test(ct_doctest_no_tests_mod, []),
    expect_error_count(ct_doctest_module_doc_parse_error_mod, [], 1).

docs_filtering_and_error_formatting(_Config) ->
    expect_error_count(ct_doctest_module_doc_value_error_mod, [], 1),
    expect_error_count(ct_doctest_function_parse_error_mod, [], 1),
    expect_error_count(ct_doctest_function_value_error_mod, [], 1),
    expect_error_count(ct_doctest_bad_line_numbers_mod, [], 1).

parser_prompt_parsing(_Config) ->
    expect_error_count(ct_doctest_prompt_parser_mod, [], 3),
    ok = ct_doctest:test(ct_doctest_non_erlang_block_mod, []).

runtime_failure_matching(_Config) ->
    ok = ct_doctest:test(ct_doctest_failure_match_mod, []),
    expect_error_count(ct_doctest_failure_unexpected_success_mod, [], 1),
    expect_exception(ct_doctest_failure_mismatch_mod, [], error, badarg).

parse_rewrite_helpers(_Config) ->
    ok = ct_doctest:test(ct_doctest_parse_rewrite_mod, []),
    expect_error_count(ct_doctest_scan_error_mod, [], 1).

integration_smoke(_Config) ->
    Bindings = [{module_doc,
                 erl_eval:add_binding('Prebound', hello, erl_eval:new_bindings())}],
    ct_doctest:test(ct_doctest, Bindings).

compile_fixture(File, OutDir) ->
    Module = list_to_atom(filename:basename(File, ".erl")),
    {ok, Module} = compile:file(File, [debug_info,
                                       {outdir, OutDir},
                                       report_errors,
                                       report_warnings]),
    {module, Module} = code:load_abs(filename:join(OutDir, atom_to_list(Module))),
    Module.

expect_error_count(Module, Bindings, ExpectedErrors) ->
    try ct_doctest:test(Module, Bindings) of
        Result ->
            ct:fail({expected_error_count, ExpectedErrors, got_result, Result})
    catch
        error:{ExpectedErrors, errors} ->
            ok;
        Class:Reason:Stacktrace ->
            ct:fail({unexpected_exception, Module,
                     {Class, Reason}, Stacktrace})
    end.

expect_exception(Module, Bindings, Class, Reason) ->
    try ct_doctest:test(Module, Bindings) of
        Result ->
            ct:fail({expected_exception, {Class, Reason}, got_result, Result})
    catch
        Class:Reason ->
            ok;
        OtherClass:OtherReason:Stacktrace ->
            ct:fail({unexpected_exception, Module,
                     {OtherClass, OtherReason}, Stacktrace})
    end.
