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

-module(ct_results_parser).

-export([parse_index_html_file/1,
         parse_suite_log_file/1,
         parse_old_runs_file/1]).

-include("ct_results_parser.hrl").

-define(NEXT_ROW(Text), string:find(Text, "<tr class")).
-define(NEXT_COL(Text), string:find(Text, "<td")).

%%%-----------------------------------------------------------------
%%% EXPORTED FUNCTIONS
%%%-----------------------------------------------------------------

parse_index_html_file(Path) ->
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Table} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        [#test{start_date = StartDate} | _] = Tests = parse_tests(Table, []),
        {ok, _} = collect_until("<tfoot>\n", file:read_line(Fd), Fd, []),
        {ok, Footer} = collect_until("</tfoot>\n", file:read_line(Fd), Fd, []),
        case StartDate of
            undefined ->
                Total = parse_total(lists:flatten(Footer), 6, #total{}),
                lists:append(Tests, [Total]);
            _ ->
                Total = parse_total(lists:flatten(Footer), 10, #total{}),
                lists:append(Tests, [Total])
        end
    after
        file:close(Fd)
    end.

%%%-----------------------------------------------------------------

parse_suite_log_file(Path) ->
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, File} = collect_until("<h1>", file:read_line(Fd), Fd, []),
        Last = lists:last(File),
        [_, TestName0] = string:split(Last, "<i>"),
        [TestName, _] = string:split(TestName0, "</i>"),
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Table} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        TestCases = parse_test_cases(lists:flatten(Table), undefined, []),
        {ok, _} = collect_until("<tfoot>\n", file:read_line(Fd), Fd, []),
        {ok, Footer} = collect_until("</tfoot>\n", file:read_line(Fd), Fd, []),
        Total = parse_test_cases_total(lists:flatten(Footer), #test_cases_total{}),
        [#suite_test_name{test_name = TestName}] ++ TestCases ++ [Total]
    after
        file:close(Fd)
    end.

%%%-----------------------------------------------------------------

parse_old_runs_file(Path) ->
    {ok, Fd} = file:open(Path, [read]),
    try
        {ok, _} = collect_until("<tbody>\n", file:read_line(Fd), Fd, []),
        {ok, Table} = collect_until("</tbody>\n", file:read_line(Fd), Fd, []),
        parse_old_runs(lists:flatten(Table), [])
    after
        file:close(Fd)
    end.

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

collect_until(Expected, {ok, Line}, Fd, Acc) ->
    case string:prefix(Line, Expected) of
        nomatch -> collect_until(Expected, file:read_line(Fd), Fd, [Line | Acc]);
        _ -> {ok, lists:reverse([Line | Acc])}
    end;
collect_until(_Expected, eof, _Fd, _Acc) ->
    {error, eof};
collect_until(_Expected, {error, Other}, _Fd, _Acc) ->
    {error, Other}.

%%%-----------------------------------------------------------------

parse_tests([], Tests) ->
    lists:reverse(Tests);
parse_tests(["<tr class=" ++ _ | Rest], Tests) ->
    parse_tests(Rest, [#test{} | Tests]);
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{test_name = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    [Name, _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{test_name = Name, suite_log_link = Link} | Tests]);
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{ct_log_link = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    ["CT Log", _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{ct_log_link = Link} | Tests]);
parse_tests(["<td align=center><b>" ++ Line | Rest], [Test | Tests]) ->
    [Label, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{label = list_to_atom(Label)} | Tests]);
parse_tests(["<td>" ++ Line | Rest], [#test{start_date = undefined} = Test | Tests]) ->
    [Date, _] = string:split(Line, "<"),
    {ok, [_DayOfWeek, MonthName, Day, Year, Hour, Minute, Second], _} =
        io_lib:fread("~s ~s ~d ~d ~d:~d:~d", Date),
    Month = month_name_to_number(MonthName),
    DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    parse_tests(Rest, [Test#test{start_date = DateTime} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{ok = undefined} = Test | Tests]) ->
    [Ok, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{ok = list_to_integer(Ok)} | Tests]);
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{failed = undefined} = Test | Tests]) ->
    Line = filter_font_color(Line0),
    [Failed, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{failed = list_to_integer(Failed)} | Tests]);
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{skipped = undefined} = Test0 | Tests]) ->
    Line = filter_font_color(Line0),
    [All, _] = string:split(Line, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Test = Test0#test{skipped = list_to_integer(string:trim(Skipped)),
                      user_skipped = list_to_integer(UserSkipped),
                      auto_skipped = list_to_integer(AutoSkipped)},
    parse_tests(Rest, [Test | Tests]);
parse_tests(["<td align=right>" ++ Line0 | Rest], [#test{missing_suites = undefined} = Test | Tests]) ->
    Line = filter_font_color(Line0),
    [MissingSuites, _] = string:split(Line, "<"),
    parse_tests(Rest, [Test#test{missing_suites = list_to_integer(MissingSuites)} | Tests]);
parse_tests(["<td align=right>" ++ Line | Rest], [#test{node = undefined, elapsed_time = undefined} = Test | Tests]) ->
    [NodeOrElapsedTime, _] = string:split(Line, "<"),
    case string:find(NodeOrElapsedTime, "@") of
        nomatch ->
            parse_tests(Rest, [Test#test{elapsed_time = parse_timestamp(NodeOrElapsedTime)} | Tests]);
        _ ->
            parse_tests(Rest, [Test#test{node = list_to_atom(NodeOrElapsedTime)} | Tests])
    end;
parse_tests(["<td><a href=\"" ++ Line0 | Rest], [#test{old_runs_link = undefined} = Test | Tests]) ->
    [Link, Line1] = string:split(Line0, "\">"),
    ["Old Runs", _] = string:split(Line1, "<"),
    parse_tests(Rest, [Test#test{old_runs_link = Link} | Tests]);
parse_tests(["<td>none</td>\n" | Rest], [#test{old_runs_link = undefined} = Test | Tests]) ->
    parse_tests(Rest, [Test | Tests]);
parse_tests(["</tr>\n" | Rest], Tests) ->
    parse_tests(Rest, Tests);
parse_tests(["</tbody>\n"], Tests) ->
    lists:reverse(Tests);
parse_tests([Line | _Rest], Tests) ->
    ct:pal("parse_tests: UNMATCHED CLAUSE~n"
           "  Line: ~p~n"
           "  Tests: ~p", [Line, Tests]),
    error({unmatched_parse_tests, Line, Tests}).

%%%-----------------------------------------------------------------

parse_total(nomatch, _Cols, Total) ->
    Total;
parse_total("<tr class=" ++ Rest, Cols, Total) ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td><b>Total</b></td>" ++ Rest, Cols, Total) when Cols =:= 6; Cols =:= 10 ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td>&nbsp;</td>" ++ Rest, Cols, Total) when Cols =:= 6; Cols =:= 10 ->
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{ok = undefined} = Total) ->
    [Ok, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{ok = list_to_integer(Ok)});
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{failed = undefined} = Total) ->
    [Failed, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{failed = list_to_integer(Failed)});
parse_total("<td align=right>" ++ Rest0, Cols, #total{skipped = undefined} = Total0) ->
    [All, Rest] = string:split(Rest0, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Total = Total0#total{skipped = list_to_integer(string:trim(Skipped)),
                         user_skipped = list_to_integer(UserSkipped),
                         auto_skipped = list_to_integer(AutoSkipped)},
    parse_total(?NEXT_COL(Rest), Cols, Total);
parse_total("<td align=right><b>" ++ Rest0, Cols, #total{missing_suites = undefined} = Total) ->
    [MissingSuites, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{missing_suites = list_to_integer(MissingSuites)});
parse_total("<td align=right><b>" ++ Rest0, 6 = Cols, #total{elapsed_time = undefined} = Total) ->
    [ElapsedTime, Rest] = string:split(Rest0, "</b>"),
    parse_total(?NEXT_COL(Rest), Cols, Total#total{elapsed_time = parse_timestamp(ElapsedTime)});
parse_total(Line, Cols, Total) ->
    ct:pal("parse_total: UNMATCHED CLAUSE~n"
           "  Line: ~p~n"
           "  Cols: ~p~n"
           "  Total: ~p", [Line, Cols, Total]),
    error({unmatched_parse_total, Line, Cols, Total}).

%%%-----------------------------------------------------------------

parse_test_cases(nomatch, _, Cases) ->
    lists:reverse(Cases);
parse_test_cases("<tr class=" ++ Rest, _, Cases) ->
    parse_test_cases(?NEXT_COL(Rest), 1, [#test_case{} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 1 = Col, [Case | Cases]) ->
    case string:take(Rest0, lists:seq($0, $9)) of
        {[], Rest} ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case | Cases]);
        {Num, Rest} ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{num = list_to_integer(Num)} | Cases])
    end;
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 2 = Col, [Case | Cases]) ->
    [Mod, Rest] = string:split(Rest0, "<"),
    parse_test_cases(string:find(Rest, "<td>"), Col + 1, [Case#test_case{module = Mod} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 3 = Col, [Case | Cases]) ->
    case string:split(Rest0, "<") of
        [[], Rest] ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case | Cases]);
        [Group, Rest] ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{group = Group} | Cases])
    end;
parse_test_cases("<td><a href=\"" ++ Rest0, 4 = Col, [Case | Cases]) ->
    [Link, Rest1] = string:split(Rest0, "\">"),
    [Name, Rest] = string:split(Rest1, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{tc = list_to_atom(Name), tc_link = Link} | Cases]);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 4 = Col, [Case | Cases]) ->
    [Name, Rest] = string:split(Rest0, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{tc = list_to_atom(Name)} | Cases]);
parse_test_cases("<td><a href=\"" ++ Rest0, 5 = Col, [Case | Cases]) ->
    [Link1, Rest1] = string:split(Rest0, "\">"),
    "<a href=\"" ++ Rest2 = string:find(Rest1, "<a href=\""),
    [Link2, Rest] = string:split(Rest2, "\">"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{top_log = Link1, end_log = Link2} | Cases]);
parse_test_cases("<td><font color=\"black\">< ></font></td>" ++ Rest, 5 = Col, Cases) ->
    parse_test_cases(?NEXT_COL(Rest), Col + 1, Cases);
parse_test_cases("<td><font color=\"black\">" ++ Rest0, 6 = Col, [Case | Cases]) ->
    [Time, Rest] = string:split(Rest0, "<"),
    parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{time = parse_timestamp(Time)} | Cases]);
parse_test_cases("<td><font color=" ++ Rest0, 7 = Col, [Case | Cases]) ->
    [Color, Rest1] = string:split(Rest0, ">"),
    [Result0, Rest] = string:split(Rest1, "<"),
    Result1 = string:lowercase(Result0),
    case list_to_atom(Result1) of
        skipped when Color =:= "\"#FF8000\"" ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{result = user_skipped} | Cases]);
        skipped when Color =:= "\"#FFA64D\"" ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{result = auto_skipped} | Cases]);
        Result ->
            parse_test_cases(?NEXT_COL(Rest), Col + 1, [Case#test_case{result = Result} | Cases])
    end;
parse_test_cases("<td>" ++ Rest0, 8 = _Col, [Case | Cases]) ->
    case string:split(Rest0, "</td>") of
        [[], Rest] ->
            parse_test_cases(?NEXT_ROW(Rest), 1, [Case | Cases]);
        [Comment0, Rest] ->
            Comment1 = string:split(Comment0, "<br />"),
            Comment = [filter_font_color(C) || C <- Comment1],
            parse_test_cases(?NEXT_ROW(Rest), 1, [Case#test_case{comment = Comment} | Cases])
    end;
parse_test_cases(Line, Col, Cases) ->
    ct:pal("parse_test_cases: UNMATCHED CLAUSE~n"
           "  Line: ~p~n"
           "  Col: ~p~n"
           "  Cases: ~p", [Line, Col, Cases]),
    error({unmatched_parse_test_cases, Line, Col, Cases}).

%%%-----------------------------------------------------------------

parse_test_cases_total(nomatch, TCTotal) ->
    TCTotal;
parse_test_cases_total("<tr>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td><b>TOTAL</b></td>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td></td>" ++ Rest, TCTotal) ->
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total("<td>" ++ Rest0, #test_cases_total{time = undefined} = TCTotal) ->
    [Time, Rest] = string:split(Rest0, "<"),
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal#test_cases_total{time = parse_timestamp(Time)});
parse_test_cases_total("<td><b>" ++ Rest0, #test_cases_total{result = undefined} = TCTotal) ->
    [Result0, Rest] = string:split(Rest0, "<"),
    Result = string:lowercase(Result0),
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal#test_cases_total{result = list_to_atom(Result)});
parse_test_cases_total("<td>" ++ Rest0, #test_cases_total{ok = undefined} = TCTotal0) ->
    [Ok, Rest1] = string:split(Rest0, " Ok, "),
    [Failed, Rest2] = string:split(Rest1, " Failed"),
    {Skipped, Rest3} = case Rest2 of
        ", " ++ Rest2a ->
            [S, R] = string:split(Rest2a, " Skipped of "),
            {list_to_integer(S), R};
        " of " ++ R ->
            {0, R}
    end,
    [Total, Rest4] = string:split(Rest3, "<br>Elapsed Time: "),
    [ElapsedTime, Rest] = string:split(Rest4, "</td>"),
    TCTotal = TCTotal0#test_cases_total{ok = list_to_integer(Ok),
                                        failed = list_to_integer(Failed),
                                        skipped = Skipped,
                                        total = list_to_integer(Total),
                                        elapsed_time = parse_timestamp(ElapsedTime)},
    parse_test_cases_total(?NEXT_COL(Rest), TCTotal);
parse_test_cases_total(Line, TCTotal) ->
    ct:pal("parse_test_cases_total: UNMATCHED CLAUSE~n"
           "  Line: ~p~n"
           "  TCTotal: ~p", [Line, TCTotal]),
    error({unmatched_parse_test_cases_total, Line, TCTotal}).

%%%-----------------------------------------------------------------

parse_old_runs(nomatch, Runs) ->
    lists:reverse(Runs);
parse_old_runs("<tr class=" ++ Rest, Runs) ->
    parse_old_runs(?NEXT_COL(Rest), [#old_run{} | Runs]);
parse_old_runs("<td><a href=\"" ++ Rest0, [#old_run{link = undefined} = Run | Runs]) ->
    [Link, Rest1] = string:split(Rest0, "\">"),
    [Date, Rest] = string:split(Rest1, "<"),
    {ok, [_DayOfWeek, MonthName, Day, Year, Hour, Minute, Second], _} =
        io_lib:fread("~s ~s ~d ~d ~d:~d:~d", Date),
    Month = month_name_to_number(MonthName),
    DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{link = Link, start_date = DateTime} | Runs]);
parse_old_runs("<td align=center>" ++ Rest0, [#old_run{node = undefined} = Run | Runs]) ->
    [Node, Rest] = string:split(Rest0, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{node = list_to_atom(Node)} | Runs]);
parse_old_runs("<td align=center><b>" ++ Rest0, [#old_run{label = undefined} = Run | Runs]) ->
    [Label, Rest] = string:split(Rest0, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{label = list_to_atom(Label)} | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{tests = undefined} = Run | Runs]) ->
    [Tests, Rest] = string:split(Rest0, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{tests = list_to_integer(Tests)} | Runs]);
parse_old_runs("<td align=center title='" ++ Rest0, [#old_run{test_names = undefined} = Run | Runs]) ->
    [TestNames, Rest] = string:split(Rest0, "'"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{test_names = TestNames} | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{total = undefined} = Run | Runs]) ->
    [Total, Rest] = string:split(Rest0, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{total = list_to_integer(Total)} | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{ok = undefined} = Run | Runs]) ->
    [Ok, Rest] = string:split(Rest0, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{ok = list_to_integer(Ok)} | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{failed = undefined} = Run | Runs]) ->
    Line = filter_font_color(Rest0),
    [Failed, Rest] = string:split(Line, "<"),
    parse_old_runs(?NEXT_COL(Rest), [Run#old_run{failed = list_to_integer(Failed)} | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{skipped = undefined} = Run0 | Runs]) ->
    Line = filter_font_color(Rest0),
    [All, Rest] = string:split(Line, "<"),
    [Skipped, Other0] = string:split(All, "("),
    [UserSkipped, Other1] = string:split(Other0, "/"),
    [AutoSkipped, _] = string:split(Other1, ")"),
    Run = Run0#old_run{skipped = list_to_integer(string:trim(Skipped)),
                       user_skipped = list_to_integer(UserSkipped),
                       auto_skipped = list_to_integer(AutoSkipped)},
    parse_old_runs(?NEXT_COL(Rest), [Run | Runs]);
parse_old_runs("<td align=right>" ++ Rest0, [#old_run{missing_suites = undefined} = Run | Runs]) ->
    Line = filter_font_color(Rest0),
    [MissingSuites, Rest] = string:split(Line, "<"),
    parse_old_runs(?NEXT_ROW(Rest), [Run#old_run{missing_suites = list_to_integer(MissingSuites)} | Runs]);
parse_old_runs(Line, Runs) ->
    ct:pal("parse_old_runs: UNMATCHED CLAUSE~n"
           "  Line: ~p~n"
           "  Runs: ~p", [Line, Runs]),
    error({unmatched_parse_old_runs, Line, Runs}).

%%%-----------------------------------------------------------------

filter_font_color(String0) ->
    String1 = re:replace(String0, "<font color=[^>]+>", ""),
    re:replace(String1, "</font>", "", [{return, list}]).

%%%-----------------------------------------------------------------

month_name_to_number("Jan") -> 1;
month_name_to_number("Feb") -> 2;
month_name_to_number("Mar") -> 3;
month_name_to_number("Apr") -> 4;
month_name_to_number("May") -> 5;
month_name_to_number("Jun") -> 6;
month_name_to_number("Jul") -> 7;
month_name_to_number("Aug") -> 8;
month_name_to_number("Sep") -> 9;
month_name_to_number("Oct") -> 10;
month_name_to_number("Nov") -> 11;
month_name_to_number("Dec") -> 12.

%%%-----------------------------------------------------------------

parse_timestamp(Str) ->
    {ok, [Seconds], _} = io_lib:fread("~fs", Str),
    MicroSecs = round(Seconds * 1_000_000),
    Megas = MicroSecs div 1_000_000_000_000,
    Secs = (MicroSecs div 1_000_000) rem 1_000_000,
    Micros = MicroSecs rem 1_000_000,
    {Megas, Secs, Micros}.
