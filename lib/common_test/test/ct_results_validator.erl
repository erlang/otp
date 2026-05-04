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

-module(ct_results_validator).

-include("ct_results_parser.hrl").

-export([validate_total/1,
         validate_tests/3,
         validate_test_cases_total/1,
         validate_test_cases/2,
         validate_old_runs/2]).

-define(DOT_SPLIT, "(?<!\\.)\\.(?!\\.)").

%%%-----------------------------------------------------------------
%%% EXPORTED FUNCTIONS
%%%-----------------------------------------------------------------

validate_total(Tests) ->
    Total = #total{ok = 0,
                   failed = 0,
                   skipped = 0,
                   user_skipped = 0,
                   auto_skipped = 0,
                   missing_suites = 0},
    validate_total(Tests, Total).

%%%-----------------------------------------------------------------

validate_tests([], [#total{}], _) ->
    ok;
validate_tests([#test{test_name = TestName,
                      label = Label,
                      start_date = Date1,
                      ok = Ok,
                      failed = Failed,
                      skipped = Skipped,
                      user_skipped = UserSkipped,
                      auto_skipped = AutoSkipped,
                      missing_suites = MissingSuites,
                      node = Node
                     } = Expected | Tests1],
               [#test{test_name = TestName,
                      suite_log_link = SuiteLink,
                      label = Label,
                      start_date = Date2,
                      ok = Ok,
                      failed = Failed,
                      skipped = Skipped,
                      user_skipped = UserSkipped,
                      auto_skipped = AutoSkipped,
                      missing_suites = MissingSuites,
                      node = Node,
                      ct_log_link = CtLogLink,
                      old_runs_link = OldRunsLink
                     } | Tests2], false) ->
    ok = compare_date(Date1, Date2),
    ok = validate_links(Expected, SuiteLink, CtLogLink, OldRunsLink),
    validate_tests(Tests1, Tests2, false);
validate_tests([#test{test_name = TestName,
                      ok = Ok,
                      failed = Failed,
                      skipped = Skipped,
                      user_skipped = UserSkipped,
                      auto_skipped = AutoSkipped,
                      missing_suites = MissingSuites
                     } = Expected | Tests1],
               [#test{test_name = TestName,
                      suite_log_link = SuiteLink,
                      ok = Ok,
                      failed = Failed,
                      skipped = Skipped,
                      user_skipped = UserSkipped,
                      auto_skipped = AutoSkipped,
                      missing_suites = MissingSuites
                     } | Tests2], true) ->
    ok = validate_links(Expected, SuiteLink),
    validate_tests(Tests1, Tests2, true);
validate_tests(Expected, Actual, Mode) ->
    ct:pal("validate_tests: UNMATCHED CLAUSE~n"
           "  Expected: ~p~n"
           "  Actual: ~p~n"
           "  Mode: ~p", [Expected, Actual, Mode]),
    error({unmatched_validate_tests, Expected, Actual, Mode}).

%%%-----------------------------------------------------------------

validate_test_cases_total(TestCases) ->
    Total = #test_cases_total{ok = 0,
                              failed = 0,
                              skipped = 0,
                              total = 0
                             },
    validate_test_cases_total(TestCases, Total).

%%%-----------------------------------------------------------------

validate_test_cases([], [#test_cases_total{}]) ->
    ok;
validate_test_cases([#test_case{num = Num,
                                module = Mod,
                                group = Group,
                                tc = Tc,
                                tc_link = TcLink1,
                                top_log = TopLog1,
                                end_log = EndLog1,
                                result = Result,
                                comment = undefined} | Cases1],
                    [#test_case{num = Num,
                                module = Mod,
                                group = Group,
                                tc = Tc,
                                tc_link = TcLink2,
                                top_log = TopLog2,
                                end_log = EndLog2,
                                result = Result,
                                comment = undefined} | Cases2]) ->
    ok = compare_links(TcLink1, TcLink2),
    ok = compare_links(TopLog1, TopLog2),
    ok = compare_links(EndLog1, EndLog2),
    validate_test_cases(Cases1, Cases2);
validate_test_cases([#test_case{num = Num,
                                module = Mod,
                                group = Group,
                                tc = Tc,
                                tc_link = TcLink1,
                                top_log = TopLog1,
                                end_log = EndLog1,
                                result = Result,
                                comment = Comment1} | Cases1],
                    [#test_case{num = Num,
                                module = Mod,
                                group = Group,
                                tc = Tc,
                                tc_link = TcLink2,
                                top_log = TopLog2,
                                end_log = EndLog2,
                                result = Result,
                                comment = Comment2} | Cases2]) ->
    true = (string:jaro_similarity(Comment1, Comment2) > 0.9),
    ok = compare_links(TcLink1, TcLink2),
    ok = compare_links(TopLog1, TopLog2),
    ok = compare_links(EndLog1, EndLog2),
    validate_test_cases(Cases1, Cases2);
validate_test_cases([Expected | _], [Actual | _]) ->
    ct:pal("validate_test_cases: UNMATCHED CLAUSE~n"
           "  Expected: ~p~n"
           "  Actual: ~p", [Expected, Actual]),
    error({unmatched_validate_test_cases, Expected, Actual});
validate_test_cases(Expected, Actual) ->
    ct:pal("validate_test_cases: UNMATCHED CLAUSE (list mismatch)~n"
           "  Expected: ~p~n"
           "  Actual: ~p", [Expected, Actual]),
    error({unmatched_validate_test_cases, Expected, Actual}).

%%%-----------------------------------------------------------------

validate_old_runs([], []) ->
    ok;
validate_old_runs([#old_run{start_date = StartDate1,
                            node = Node,
                            label = Label,
                            tests = Tests,
                            test_names = TestNames,
                            total = Total,
                            ok = Ok,
                            failed = Failed,
                            skipped = Skipped,
                            user_skipped = UserSkipped,
                            auto_skipped = AutoSkipped,
                            missing_suites = MissingSuites} = OldRun | OldRuns1],
                  [#old_run{start_date = StartDate2,
                            link = Link,
                            node = Node,
                            label = Label,
                            tests = Tests,
                            test_names = TestNames,
                            total = Total,
                            ok = Ok,
                            failed = Failed,
                            skipped = Skipped,
                            user_skipped = UserSkipped,
                            auto_skipped = AutoSkipped,
                            missing_suites = MissingSuites} | OldRuns2]) ->
    ok = compare_date(StartDate1, StartDate2),
    LinkParts = string:split(Link, "/", all),
    ok = validate_old_run_link_parts(OldRun, LinkParts),
    validate_old_runs(OldRuns1, OldRuns2);
validate_old_runs(Expected, Actual) ->
    ct:pal("validate_old_runs: UNMATCHED CLAUSE~n"
           "  Expected: ~p~n"
           "  Actual: ~p", [Expected, Actual]),
    error({unmatched_validate_old_runs, Expected, Actual}).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

validate_total([#total{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                       missing_suites = MS, elapsed_time = undefined}],
               #total{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                      missing_suites = MS, elapsed_time = undefined}) ->
    ok;
validate_total([#total{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                       missing_suites = MS, elapsed_time = ET1}],
               #total{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                      missing_suites = MS, elapsed_time = ET2}) ->
    true = (abs(timer:now_diff(ET1, ET2)) =< 5000),
    ok;
validate_total([#test{ok = Ok, failed = F, skipped = S, user_skipped = US, auto_skipped = AS,
                      missing_suites = MS, elapsed_time = ET} | Tests],
               #total{ok = TOk, failed = TF, skipped = TS, user_skipped = TUS, auto_skipped = TAS,
                      missing_suites = TMS, elapsed_time = TET} = Total0) ->
    Total = Total0#total{ok = TOk + Ok,
                         failed = TF + F,
                         skipped = TS + S,
                         user_skipped = TUS + US,
                         auto_skipped = TAS + AS,
                         missing_suites = TMS + MS,
                         elapsed_time = add_time(TET, ET)},
    validate_total(Tests, Total).

%%%-----------------------------------------------------------------

validate_links(Expected, SuiteLink) ->
    SuiteLinkParts = string:split(SuiteLink, "/", all),
    3 = length(SuiteLinkParts),
    ok = validate_link_parts(Expected, SuiteLinkParts).

%%%-----------------------------------------------------------------

validate_links(Expected, SuiteLink, CtLogLink, OldRunsLink) ->
    SuiteLinkParts = string:split(SuiteLink, "/", all),
    4 = length(SuiteLinkParts),
    ok = validate_link_parts(Expected, SuiteLinkParts),

    CtLogLinkParts = string:split(CtLogLink, "/", all),
    2 = length(CtLogLinkParts),
    ok = validate_link_parts(Expected, CtLogLinkParts),

    case OldRunsLink of
        undefined ->
            ok;
        _ ->
            OldRunsLinkParts = string:split(OldRunsLink, "/", all),
            1 = length(OldRunsLinkParts),
            ok = validate_link_parts(Expected, OldRunsLinkParts)
    end.

%%%-----------------------------------------------------------------

validate_test_cases_total([#test_cases_total{ok = Ok, failed = F, skipped = S, total = Tot,
                                             time = T, elapsed_time = ET} | []],
                          #test_cases_total{ok = Ok, failed = F, skipped = S, time = T, total = Tot})
  when ET /= undefined ->
    ok;
validate_test_cases_total([#test_case{time = Time, tc = Tc} | Cases],
                          #test_cases_total{time = TotalTime0} = Total0)
  when Tc == init_per_suite; Tc == init_per_group; Tc == end_per_group; Tc == end_per_suite ->
    TotalTime = add_time(TotalTime0, Time),
    Total = Total0#test_cases_total{time = TotalTime},
    validate_test_cases_total(Cases, Total);
validate_test_cases_total([#test_case{result = Result, time = Time} | Cases],
                          #test_cases_total{time = TotalTime0} = Total0) ->
    Total1 = update_total(Result, Total0),
    TotalTime = add_time(TotalTime0, Time),
    Total = Total1#test_cases_total{time = TotalTime},
    validate_test_cases_total(Cases, Total);
validate_test_cases_total(Cases, Total) ->
    ct:pal("validate_test_cases_total: UNMATCHED CLAUSE~n"
           "  Cases: ~p~n"
           "  Total: ~p", [Cases, Total]),
    error({unmatched_validate_test_cases_total, Cases, Total}).

%%%-----------------------------------------------------------------

add_time(undefined, undefined) ->
    undefined;
add_time(undefined, T) ->
    T;
add_time({M1, S1, U1}, {M2, S2, U2}) ->
    {M1 + M2 + (S1 + S2 + (U1 + U2) div 1_000_000) div 1_000_000,
     (S1 + S2 + (U1 + U2) div 1_000_000) rem 1_000_000,
     (U1 + U2) rem 1_000_000}.

%%%-----------------------------------------------------------------

update_total(ok, #test_cases_total{ok = Ok, total = Tot, result = undefined} = Total) ->
    Total#test_cases_total{ok = Ok + 1, total = Tot + 1, result = ok};
update_total(Skipped, #test_cases_total{skipped = S, total = Tot, result = undefined} = Total)
  when Skipped =:= user_skipped; Skipped =:= auto_skipped ->
    Total#test_cases_total{skipped = S + 1, total = Tot + 1, result = skipped};
update_total(ok, #test_cases_total{ok = Ok, total = Tot} = Total) ->
    Total#test_cases_total{ok = Ok + 1, total = Tot + 1};
update_total(Skipped, #test_cases_total{skipped = S, total = Tot} = Total)
  when Skipped =:= user_skipped; Skipped =:= auto_skipped ->
    Total#test_cases_total{skipped = S + 1, total = Tot + 1};
update_total(failed, #test_cases_total{failed = F, total = Tot} = Total) ->
    Total#test_cases_total{failed = F + 1, total = Tot + 1, result = failed}.

%%%-----------------------------------------------------------------

validate_link_parts(_Expected, []) ->
    ok;
validate_link_parts(#test{start_date = ExpectedDate, node = ExpectedNode} = Expected,
                    ["ct_run." ++ Part | Rest]) ->
    [Node, Date] = string:split(Part, "."),
    ExpectedNode = list_to_atom(Node),
    ParsedDate = parse_link_date(Date),
    compare_date(ExpectedDate, ParsedDate),
    validate_link_parts(Expected, Rest);
validate_link_parts(#test{start_date = ExpectedDate} = Expected, ["run." ++ Date | Rest]) ->
    ParsedDate = parse_link_date(Date),
    compare_date(ExpectedDate, ParsedDate),
    validate_link_parts(Expected, Rest);
validate_link_parts(#test{test_name = TestName} = Expected, [Part | Rest]) when Rest /= [] ->
    ok = compare_links(TestName ++ ".logs", Part),
    validate_link_parts(Expected, Rest);
validate_link_parts(_Expected, ["suite.log.html" | []]) ->
    ok;
validate_link_parts(_Expected, ["ctlog.html" | []]) ->
    ok;
validate_link_parts(_Expected, ["all_runs.html" | []]) ->
    ok.

%%%-----------------------------------------------------------------

validate_old_run_link_parts(#old_run{start_date = StartDate, node = ExpectedNode},
                            ["ct_run." ++ Part, "index.html"]) ->
    [Node, Date] = string:split(Part, "."),
    ExpectedNode = list_to_atom(Node),
    ParsedDate = parse_link_date(Date),
    compare_date(StartDate, ParsedDate).

%%%-----------------------------------------------------------------

compare_links(undefined, undefined) ->
    ok;
compare_links(Link1, Link2) when is_list(Link1), is_list(Link2) ->
    Parts1 = re:split(Link1, ?DOT_SPLIT, [{return, list}]),
    Parts2 = re:split(Link2, ?DOT_SPLIT, [{return, list}]),
    compare_link_parts(Parts1, Parts2).

%%%-----------------------------------------------------------------

compare_link_parts([L1, L2, L3, _, L4], [L1, L2, L3, _, _, L4]) ->
    ok;
compare_link_parts([L1, L2, L3, _, L4], [L1, L2, L3, _, L4]) ->
    ok;
compare_link_parts([L1, L2, _, L3], [L1, L2, _, L3]) ->
    ok;
compare_link_parts([L1, L2, _, L3], [L1, L2, L3]) ->
    ok;
compare_link_parts([L1, L2, L3], [L1, L2, _, L3]) ->
    ok;
compare_link_parts([L1, L2, L3], [L1, L2, L3]) ->
    ok.

%%%-----------------------------------------------------------------

compare_date(Date, Date) ->
    ok;
compare_date(Date1, Date2) ->
    S1 = calendar:datetime_to_gregorian_seconds(Date1),
    S2 = calendar:datetime_to_gregorian_seconds(Date2),
    case abs(S2 - S1) of
        %% We expect no more than a few seconds to pass before generation of
        %% expected Date and actual from the test execution
        Diff when Diff =< 30 -> ok
    end.

%%%-----------------------------------------------------------------

parse_link_date(Date) ->
    {ok, [Year, Month, Day, Hour, Minute, Second], _} = io_lib:fread("~d-~d-~d_~d.~d.~d", Date),
    {{Year, Month, Day}, {Hour, Minute, Second}}.
