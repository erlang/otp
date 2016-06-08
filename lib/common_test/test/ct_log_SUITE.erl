%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_log_SUITE
%%%
%%% Description: Test that ct:log, ct:pal and io:format print to
%%% the test case log file as expected, with or without special HTML
%%% characters being escaped. 
%%%
%%%-------------------------------------------------------------------
-module(ct_log_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.    

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group,print_and_verify}].

groups() -> 
    [{print_and_verify,[sequence],[print,verify]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% 
print(Config) ->
    TcLogFile = proplists:get_value(tc_logfile, Config),
    Pid = self(),
    String = atom_to_list(?MODULE),

    %% START mark
    io:format("LOGGING START~n"),

    %% io:format
    io:format("1. Printing nothing~n", []),
    io:format("2. Printing a string: ~s~n", [String]),
    io:format("3. Printing a string: ~p~n", [String]),
    io:format("4. Printing a tuple: ~w~n", [{module,?MODULE}]),
    io:format("5. Printing a pid: ~w~n", [Pid]),
    io:format("6. Printing HTML: <pre>~s</pre>~n", [String]),

    %% --- API ---
    %% pal(Format) ->
    %%   = ct:pal(default, 50, Format, []).
    %% pal(X1, X2) -> ok
    %%   X1 = Category | Importance | Format
    %%   X2 = Format | FormatArgs
    %% pal(X1, X2, X3) -> ok
    %%   X1 = Category | Importance
    %%   X2 = Importance | Format
    %%   X3 = Format | FormatArgs
    %% pal(Category, Importance, Format, FormatArgs) -> ok
    %% ------
    ct:pal("1. Printing nothing"),
    ct:pal("2. Printing nothing", []),
    ct:pal("3. Printing a string: ~s", [String]),
    ct:pal("4. Printing a string: ~p", [String]),
    ct:pal("5. Printing a tuple: ~w", [{module,?MODULE}]),
    ct:pal("6. Printing a pid: ~w", [Pid]),
    ct:pal("7. Printing HTML: <pre>~s</pre>", [String]),
    ct:pal(ct_internal, "8. Printing with category"),
    ct:pal(ct_internal, "9. Printing with ~s", ["category"]),
    ct:pal(50, "10. Printing with importance"),
    ct:pal(50, "11. Printing with ~s", ["importance"]),
    ct:pal(ct_internal, 50, "12. Printing with ~s", ["category and importance"]),

    %% --- API ---
    %% log(Format) -> ok
    %%   = ct:log(default, 50, Format, [], []).
    %% log(X1, X2) -> ok
    %%   X1 = Category | Importance | Format
    %%   X2 = Format | FormatArgs
    %% log(X1, X2, X3) -> ok
    %%   X1 = Category | Importance
    %%   X2 = Importance | Format
    %%   X3 = Format | FormatArgs | Opts
    %% log(X1, X2, X3, X4) -> ok
    %%   X1 = Category | Importance
    %%   X2 = Importance | Format
    %%   X3 = Format | FormatArgs
    %%   X4 = FormatArgs | Opts
    %% log(Category, Importance, Format, FormatArgs, Opts) -> ok
    %% ------
    ct:log("1. Printing nothing"),
    ct:log("2. Printing nothing", []),
    ct:log("3. Printing a string: ~s", [String]),
    ct:log("4. Printing a string: ~p", [String]),
    ct:log("5. Printing a tuple: ~w", [{module,?MODULE}]),
    ct:log("6. Printing a pid: ~w", [Pid]),
    ct:log("7. Printing HTML: <pre>~s</pre>", [String]),
    ct:log("8. Printing a pid escaped: ~w", [Pid], [esc_chars]),
    ct:log("9. Printing a string escaped: ~p", [String], [esc_chars]),
    ct:log("10. Printing HTML escaped: <pre>~s</pre>", [String], [esc_chars]),
    ct:log("11. Printing a string, no css: ~s", [String], [no_css]),
    ct:log("12. Printing a pid escaped, no css: ~w", [Pid],
	   [esc_chars, no_css]),
    ct:log(ct_internal, "13. Printing with category"),
    ct:log(ct_internal, "14. Printing with ~s", ["category"]),
    ct:log(ct_internal, "15. Printing with ~s, no_css", ["category"],
	   [no_css]),
    ct:log(50, "16. Printing with importance"),
    ct:log(50, "17. Printing with ~s", ["importance"]),
    ct:log(50, "18. Printing with ~s, no_css", ["importance"], [no_css]),
    ct:log(ct_internal, 50, "19. Printing with category and importance"),
    ct:log(ct_internal, 50, "20. Printing with ~s", ["category and importance"]),
    ct:log(ct_internal, 50, "21. Printing a pid escaped with ~s, no_css: ~w",
	   ["category and importance",Pid], [esc_chars,no_css]),

    %% END mark
    ct:log("LOGGING END", [], [no_css]),
    {save_config,[{the_logfile,TcLogFile},{the_pid,Pid},{the_string,String}]}.


verify(Config) ->
    {print,SavedCfg} = proplists:get_value(saved_config, Config),
    TcLogFile = proplists:get_value(the_logfile, SavedCfg),
    Pid = proplists:get_value(the_pid, SavedCfg),
    StrPid = lists:flatten(io_lib:format("~p",[Pid])),
    EscPid = "&lt;" ++ string:substr(StrPid, 2, length(StrPid)-2) ++ "&gt;", 
    String = proplists:get_value(the_string, SavedCfg),
    ct:log("Read from prev testcase: ~p & ~p", [TcLogFile,Pid]),
    {ok,Dev} = file:open(TcLogFile, [read]),
    ok = read_until(Dev, "LOGGING START\n"),
    
    %% io:format
    match_line(Dev, "1. Printing nothing", []),
    read_nl(Dev),
    match_line(Dev, "2. Printing a string: ~s", [String]),
    read_nl(Dev),
    match_line(Dev, "3. Printing a string: ~p", [String]),
    read_nl(Dev),
    match_line(Dev, "4. Printing a tuple: ~w", [{module,?MODULE}]),
    read_nl(Dev),
    match_line(Dev, "5. Printing a pid: ~s", [EscPid]),
    read_nl(Dev),
    match_line(Dev, "6. Printing HTML: &lt;pre&gt;~s&lt;/pre&gt;", [String]),
    read_nl(Dev),
    %% ct:pal
    read_header(Dev),
    match_line(Dev, "1. Printing nothing", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "2. Printing nothing", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "3. Printing a string: ~s", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "4. Printing a string: ~p", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "5. Printing a tuple: ~w", [{module,?MODULE}]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "6. Printing a pid: ~s", [EscPid]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "7. Printing HTML: &lt;pre&gt;~s&lt;/pre&gt;", [String]),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "8. Printing with category", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "9. Printing with ~s", ["category"]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "10. Printing with importance", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "11. Printing with ~s", ["importance"]),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "12. Printing with ~s", ["category and importance"]),
    read_footer(Dev),
    %% ct:log
    read_header(Dev),
    match_line(Dev, "1. Printing nothing", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "2. Printing nothing", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "3. Printing a string: ~s", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "4. Printing a string: ~p", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "5. Printing a tuple: ~w", [{module,?MODULE}]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "6. Printing a pid: ~w", [Pid]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "7. Printing HTML: <pre>~s</pre>", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "8. Printing a pid escaped: ~s", [EscPid]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "9. Printing a string escaped: ~p", [String]),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "10. Printing HTML escaped: &lt;pre&gt;~s&lt;/pre&gt;",
	       [String]),
    read_footer(Dev),
    match_line(Dev, "11. Printing a string, no css: ~s", [String]),
    match_line(Dev, "12. Printing a pid escaped, no css: ~s", [EscPid]),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "13. Printing with category", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "14. Printing with ~s", ["category"]),
    read_footer(Dev),
    match_line(Dev, "15. Printing with ~s, no_css", ["category"]),
    read_header(Dev),
    match_line(Dev, "16. Printing with importance", []),
    read_footer(Dev),
    read_header(Dev),
    match_line(Dev, "17. Printing with ~s", ["importance"]),
    read_footer(Dev),
    match_line(Dev, "18. Printing with ~s, no_css", ["importance"]),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "19. Printing with category and importance", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\""),
    match_line(Dev, "20. Printing with ~s", ["category and importance"]),
    read_footer(Dev),
    match_line(Dev, "21. Printing a pid escaped with ~s, no_css: ~s",
	       ["category and importance",EscPid]),

    file:close(Dev),
    ok.

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------

read_until(Dev, Pat) ->
    case file:read_line(Dev) of
	{ok,Pat} ->
	    file:read_line(Dev),		% \n
	    ok;
	eof ->
	    file:close(Dev),
	    {error,{not_found,Pat}};
	_ ->
	    read_until(Dev, Pat)
    end.

match_line(Dev, Format, Args) ->
    Pat = lists:flatten(io_lib:format(Format, Args)),
    Line = element(2, file:read_line(Dev)),
    case re:run(Line, Pat) of
	{match,_} ->
	    ok;
	nomatch ->
	    ct:pal("ERROR! No match for ~p.\nLine = ~p", [Pat,Line]),
	    file:close(Dev),
	    ct:fail({mismatch,Pat,Line})
    end.

read_header(Dev) ->
    read_header(Dev, "\"default\"").
    
read_header(Dev, Cat) ->
    file:read_line(Dev),			% \n
    "</pre>\n" = element(2, file:read_line(Dev)),
    {match,_} =
	re:run(element(2, file:read_line(Dev)), "<div class="++Cat++"><pre><b>"
	       "\\*\\*\\* User \\d{4}-\\d{2}-\\d{2} "
	       "\\d{2}:\\d{2}:\\d{2}.\\d{1,} \\*\\*\\*</b>").

read_footer(Dev) ->
    "</pre></div>\n" = element(2, file:read_line(Dev)),
    "<pre>\n" = element(2, file:read_line(Dev)).

read_nl(Dev) ->
    file:read_line(Dev).
    
	
