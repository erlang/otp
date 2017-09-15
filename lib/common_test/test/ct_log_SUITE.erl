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

    %% ct:pal
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

    ct:pal("13. Printing with heading", [],
           [{heading,"This is a heading"}]),
    ct:pal(ct_internal, "14. Printing with category and heading", [],
           [{heading,"This is a heading"}]),
    ct:pal(50, "15. Printing with importance and heading", [],
           [{heading,"This is a heading"}]),
    ct:pal(ct_internal, 50, "16. Printing with category, importance and heading", [],
           [{heading,"This is a heading"}]),

    %% ct:log
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

    ct:log("22. Printing with heading", [],
           [{heading,"This is a heading"}]),
    ct:log(ct_internal, "23. Printing with category and heading", [],
           [{heading,"This is a heading"}]),
    ct:log(50, "24. Printing with importance and heading", [],
           [{heading,"This is a heading"}]),
    ct:log(ct_internal, 50, "25. Printing with category, importance and heading", [],
           [{heading,"This is a heading"}]),

    %% END mark
    ct:log("LOGGING END", [], [no_css]),


    %% ct:print
    ct:print("1. Does this show??"),
    ct:print("2. Does this ~s", ["show??"]),
    ct:print("3. Is this a non-html pid?? ~w", [self()]),
    ct:print(ct_internal, "4. Printing with category"),
    ct:print(ct_internal, "5. Printing with ~s", ["category"]),
    ct:print(50, "6. Printing with importance"),
    ct:print(50, "7. Printing with ~s", ["importance"]),
    ct:print(ct_internal, 50, "8. Printing with ~s", ["category and importance"]),
    ct:print("9. Printing with heading", [],
           [{heading,"This is a heading"}]),
    ct:print(ct_internal, "10. Printing with category and heading", [],
           [{heading,"This is a heading"}]),
    ct:print(50, "11. Printing with importance and heading", [],
           [{heading,"This is a heading"}]),
    ct:print(ct_internal, 50, "12. Printing with category, importance and heading", [],
           [{heading,"This is a heading"}]),

    {save_config,[{the_logfile,TcLogFile},{the_pid,Pid},{the_string,String}]}.


verify(Config) ->
    {print,SavedCfg} = proplists:get_value(saved_config, Config),
    TcLogFile = proplists:get_value(the_logfile, SavedCfg),
    Pid = proplists:get_value(the_pid, SavedCfg),
    StrPid = lists:flatten(io_lib:format("~p",[Pid])),
    EscPid = "&lt;" ++ string:slice(StrPid, 1, length(StrPid)-2) ++ "&gt;", 
    String = proplists:get_value(the_string, SavedCfg),
    ct:log("Read from prev testcase: ~p & ~p", [TcLogFile,Pid]),
    {ok,Dev} = file:open(TcLogFile, [read]),
    ok = read_until(Dev, "LOGGING START\n"),
    
    ct:pal("VERIFYING LOG ENTRIES...", []),

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
    read_header(Dev, "\"default\"", "This is a heading"),
    match_line(Dev, "13. Printing with heading", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\"", "This is a heading"),
    match_line(Dev, "14. Printing with category and heading", []),
    read_footer(Dev),
    read_header(Dev, "\"default\"", "This is a heading"),
    match_line(Dev, "15. Printing with importance and heading", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\"", "This is a heading"),
    match_line(Dev, "16. Printing with category, importance and heading", []),
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
    read_header(Dev, "\"default\"", "This is a heading"),
    match_line(Dev, "22. Printing with heading", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\"", "This is a heading"),
    match_line(Dev, "23. Printing with category and heading", []),
    read_footer(Dev),
    read_header(Dev, "\"default\"", "This is a heading"),
    match_line(Dev, "24. Printing with importance and heading", []),
    read_footer(Dev),
    read_header(Dev, "\"ct_internal\"", "This is a heading"),
    match_line(Dev, "25. Printing with category, importance and heading", []),
    read_footer(Dev),
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

    %% for debugging purposes:
    ct:pal("L: ~tp", [Line], [no_css]),

    case re:run(Line, Pat) of
	{match,_} ->
	    ok;
	nomatch ->
	    ct:pal("ERROR! No match for ~p", [Pat]),
	    file:close(Dev),
	    ct:fail({mismatch,Pat,Line})
    end.

read_header(Dev) ->
    read_header(Dev, "\"default\"", "User").
    
read_header(Dev, Cat) ->
    read_header(Dev, Cat, "User").

read_header(Dev, Cat, Heading) ->
    file:read_line(Dev),			% \n
    "</pre>\n" = element(2, file:read_line(Dev)),
    {ok,Hd} = file:read_line(Dev),

    %% for debugging purposes:
    ct:pal("H: ~tp", [Hd], [no_css]),
    
    Pat = "<div class="++Cat++"><pre><b>"++
          "\\*\\*\\* "++Heading++" \\d{4}-\\d{2}-\\d{2} "++
          "\\d{2}:\\d{2}:\\d{2}.\\d{1,} \\*\\*\\*</b>",

    case re:run(Hd, Pat) of
        {match,_} ->
            ok;
        _ ->
            ct:pal("ERROR! No match for ~p", [Pat]),
	    file:close(Dev),
	    ct:fail({mismatch,Pat,Hd})
    end.

read_footer(Dev) ->
    "</pre></div>\n" = element(2, file:read_line(Dev)),
    "<pre>\n" = element(2, file:read_line(Dev)),
    %% for debugging purposes:
    ct:pal("F: </pre></div><pre>", [], [no_css]).

read_nl(Dev) ->
    file:read_line(Dev).
    
	
