%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

%%
%% Purpose: Whitebox tests for the string-parameter validation in
%% odbc:param_query/3,4. The validation runs client-side in
%% fix_params/1 and rejects bad declared maximums BEFORE any port I/O, so
%% these tests need no database connection: a dummy pid is enough, the
%% error is returned before the connection is used.
%%

-module(odbc_param_limit_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("odbc/src/odbc_internal.hrl").

%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [max_length_cap,
     max_length_cap_all_string_types,
     at_cap_passes_validation,
     invalid_max_length,
     value_exceeds_max_length,
     describe_roundtrip_sentinel].

init_per_suite(Config) ->
    %% Only the client-side validation is exercised; the odbc application
    %% must be loadable but no driver/DB is required.
    case application:load(odbc) of
        ok -> Config;
        {error, {already_loaded, odbc}} -> Config;
        {error, _} -> {skip, "odbc application not available"}
    end.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% A dummy connection reference. Validation rejects over-cap / invalid
%% parameters before odbc:param_query/4 ever calls the connection, so the
%% pid is never contacted on the rejection paths.
dummy_ref() ->
    self().

%%--------------------------------------------------------------------
max_length_cap() ->
    [{doc, "A declared maximum above ?MAX_PARAM_STRING_LEN is rejected with "
      "{max_length_too_large, Type, Max, Limit} and does not reach the port."}].
max_length_cap(_Config) ->
    Limit = ?MAX_PARAM_STRING_LEN,
    1073741822 = Limit,
    Over = Limit + 1,
    {error, {max_length_too_large, sql_varchar, Over, Limit}} =
        odbc:param_query(dummy_ref(), "INSERT", [{{sql_varchar, Over}, [<<>>]}]),
    ok.

max_length_cap_all_string_types() ->
    [{doc, "The cap is enforced for all five sized string parameter types."}].
max_length_cap_all_string_types(_Config) ->
    Limit = ?MAX_PARAM_STRING_LEN,
    Over = Limit + 1,
    Types = [sql_char, sql_varchar, sql_wchar, sql_wvarchar, sql_wlongvarchar],
    lists:foreach(
      fun(Type) ->
              {error, {max_length_too_large, Type, Over, Limit}} =
                  odbc:param_query(dummy_ref(), "INSERT",
                                   [{{Type, Over}, [<<>>]}])
      end, Types),
    ok.

at_cap_passes_validation() ->
    [{doc, "A declared maximum exactly at ?MAX_PARAM_STRING_LEN passes "
      "validation (it is not a max_length_too_large error); the call then "
      "fails later because the dummy reference is not a live connection. "
      "This guards against the cap being set one too low and rejecting a "
      "legitimate width."}].
at_cap_passes_validation(_Config) ->
    Limit = ?MAX_PARAM_STRING_LEN,
    %% Short timeout: we only care that validation let it through, not that
    %% the (non-existent) connection answers.
    Result = odbc:param_query(dummy_ref(), "INSERT",
                              [{{sql_varchar, Limit}, ["ok"]}], 100),
    case Result of
        {error, {max_length_too_large, _, _, _}} ->
            ct:fail("value at the cap was wrongly rejected as too large");
        {error, _Other} ->
            %% Passed validation, failed later on the dummy connection. Good.
            ok
    end.

invalid_max_length() ->
    [{doc, "A non-integer or negative declared maximum is rejected with "
      "{invalid_max_length, Type, Max}."}].
invalid_max_length(_Config) ->
    {error, {invalid_max_length, sql_char, -1}} =
        odbc:param_query(dummy_ref(), "INSERT", [{{sql_char, -1}, [<<>>]}]),
    {error, {invalid_max_length, sql_char, foo}} =
        odbc:param_query(dummy_ref(), "INSERT", [{{sql_char, foo}, [<<>>]}]),
    ok.

value_exceeds_max_length() ->
    [{doc, "A value longer than the declared maximum is rejected with "
      "{value_exceeds_max_length, Type, Max, Len}."}].
value_exceeds_max_length(_Config) ->
    {error, {value_exceeds_max_length, sql_varchar, 5, 7}} =
        odbc:param_query(dummy_ref(), "INSERT", [{{sql_varchar, 5}, ["toolong"]}]),
    ok.

describe_roundtrip_sentinel() ->
    [{doc, "Driver 'max/unlimited' sentinel widths that describe_table/2,3 can "
      "report for SQL Server VARCHAR(MAX) (2^31 - 1) and NVARCHAR(MAX) "
      "(2^30 - 1) exceed the cap and are rejected. These are not usable as a "
      "literal bound width on a 32-bit SQLLEN platform, so rejecting them is "
      "not an incompatibility (they could not be honored before either)."}].
describe_roundtrip_sentinel(_Config) ->
    Limit = ?MAX_PARAM_STRING_LEN,
    VarcharMax = (1 bsl 31) - 1,         %% SQL Server VARCHAR(MAX)
    NVarcharMax = (1 bsl 30) - 1,        %% SQL Server NVARCHAR(MAX)
    true = VarcharMax > Limit,
    true = NVarcharMax > Limit,
    {error, {max_length_too_large, sql_varchar, VarcharMax, Limit}} =
        odbc:param_query(dummy_ref(), "INSERT",
                         [{{sql_varchar, VarcharMax}, [<<>>]}]),
    {error, {max_length_too_large, sql_wvarchar, NVarcharMax, Limit}} =
        odbc:param_query(dummy_ref(), "INSERT",
                         [{{sql_wvarchar, NVarcharMax}, [<<>>]}]),
    ok.
