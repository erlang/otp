%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%% Purpose : Produces reports in HTML from the outcome of test suite runs.

-module(ts_reports).

-export([make_index/0, make_master_index/2, make_progress_index/2]).
-export([count_cases/1, year/0, current_time/0]).

-include_lib("kernel/include/file.hrl").
-include("ts.hrl").

-import(filename, [basename/1, rootname/1]).
-import(ts_lib, [error/1]).


%% Make master index page which points out index pages for all platforms.

make_master_index(Dir, Vars) ->
    IndexName = filename:join(Dir, "index.html"),
    {ok, Index0} = make_master_index1(directories(Dir), master_header(Vars)),
    Index = [Index0|master_footer()],
    io:put_chars("Updating " ++ IndexName ++ "... "),
    ok = ts_lib:force_write_file(IndexName, Index),
    io:put_chars("done\n").

make_master_index1([Dir|Rest], Result) ->
    NewResult = 
	case catch read_variables(Dir) of
	    {'EXIT',{{bad_installation,Reason},_}} ->
		io:put_chars("Failed to read " ++ filename:join(Dir,?variables)++
			     ": " ++ Reason ++ " - Ignoring this directory\n"),
		Result;
	    Vars ->
		Platform = ts_lib:var(platform_label, Vars),
		case make_index(Dir, Vars, false) of
		    {ok, Summary} ->
			make_master_index(Platform, Dir, Summary, Result);
		    {error, _} ->
			Result
		end
	end,
    make_master_index1(Rest, NewResult);
make_master_index1([], Result) ->
    {ok, Result}.

make_progress_index(Dir, Vars) ->
    IndexName = filename:join(Dir, "index.html"),
    io:put_chars("Updating " ++ IndexName ++ "... "),
    Index0=progress_header(Vars),
    ts_lib:force_delete(IndexName),
    Dirs=find_progress_runs(Dir),
    Index1=[Index0|make_progress_links(Dirs, [])],
    IndexF=[Index1|progress_footer()],
    ok = ts_lib:force_write_file(IndexName, IndexF),
    io:put_chars("done\n").

find_progress_runs(Dir) ->
    case file:list_dir(Dir) of
	{ok, Dirs0} ->
	    Dirs1= [filename:join(Dir,X) || X <- Dirs0, 
			 filelib:is_dir(filename:join(Dir,X))],
	    lists:sort(Dirs1);
	_ ->
	    []
    end.

name_from_vars(Dir, Platform) ->
    VarFile=filename:join([Dir, Platform, "variables"]),
    case file:consult(VarFile) of
	{ok, Vars} ->
	    ts_lib:var(platform_id, Vars);
	_Other ->
	    Platform
    end.

make_progress_links([], Acc) ->
    Acc;
make_progress_links([RDir|Rest], Acc) ->
    Dir=filename:basename(RDir),
    Platforms=[filename:basename(X) ||
		  X <- find_progress_runs(RDir)],
    PlatformLinks=["<A HREF=\""++filename:join([Dir,X,"index.html"])
		   ++"\">"++name_from_vars(RDir, X)++"</A><BR>" ||
		      X <- Platforms],
    LinkName=Dir++"/index.html",
    Link =
    [
     "<TR valign=top>\n",
     "<TD><A HREF=\"", LinkName, "\">", Dir, "</A></TD>", "\n",
     "<TD>", PlatformLinks, "</TD>", "\n"
    ],
    make_progress_links(Rest, [Link|Acc]).

read_variables(Dir) ->
    case file:consult(filename:join(Dir, ?variables)) of
	{ok, Vars} -> Vars;
	{error, Reason} ->
	    erlang:error({bad_installation,file:format_error(Reason)}, [Dir])
    end.

make_master_index(Platform, Dirname, {Succ, Fail, UserSkip,AutoSkip}, Result) ->
    Link = filename:join(filename:basename(Dirname), "index.html"),
    FailStr =
	if Fail > 0 ->  
		["<FONT color=\"red\">",
		 integer_to_list(Fail),"</FONT>"];
	   true ->
		integer_to_list(Fail)
	end,
    AutoSkipStr =
	if AutoSkip > 0 ->
		["<FONT color=\"brown\">",
		 integer_to_list(AutoSkip),"</FONT>"];
	   true -> integer_to_list(AutoSkip)
	end,
    [Result,
     "<TR valign=top>\n",
     "<TD><A HREF=\"", Link, "\">", Platform, "</A></TD>", "\n",
     make_row(integer_to_list(Succ), false),
     make_row(FailStr, false),
     make_row(integer_to_list(UserSkip), false),
     make_row(AutoSkipStr, false),
     "</TR>\n"].

%% Make index page which points out individual test suites for a single platform.

make_index() ->
    {ok, Pwd} = file:get_cwd(),
    Vars = read_variables(Pwd),
    make_index(Pwd, Vars, true).

make_index(Dir, Vars, IncludeLast) ->
    IndexName = filename:absname("index.html", Dir),
    io:put_chars("Updating " ++ IndexName ++ "... "),
    case catch make_index1(Dir, IndexName, Vars, IncludeLast) of
	{'EXIT', Reason} ->
	    io:put_chars("CRASHED!\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	{error, Reason} ->
	    io:put_chars("FAILED\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	{ok, Summary} ->
	    io:put_chars("done\n"),
	    {ok, Summary};
	Err ->
	    io:format("Unknown internal error. Please report.\n(Err: ~p, ID: 1)",
		      [Err]),
	    {error, Err}
    end.

make_index1(Dir, IndexName, Vars, IncludeLast) ->
    Logs0 = ts_lib:interesting_logs(Dir),
    Logs = 
	case IncludeLast of
	    true  -> add_last_name(Logs0);
	    false -> Logs0
	end,
    {ok, {Index0, Summary}} = make_index(Logs, header(Vars), 0, 0, 0, 0, 0),
    Index = [Index0|footer()],
    case ts_lib:force_write_file(IndexName, Index) of
	ok ->
	    {ok, Summary};
	{error, Reason} ->
	    error({index_write_error, Reason})
    end.

make_index([Name|Rest], Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt) ->
    case ts_lib:last_test(Name) of
	false ->
	    %% Silently skip.
	    make_index(Rest, Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt);
	Last ->
	    case count_cases(Last) of
		{Succ, Fail, USkip, ASkip} ->
		    Cov = 
			case file:read_file(filename:join(Last,?cover_total)) of
			    {ok,Bin} -> 
				TotCoverage = binary_to_term(Bin),
				io_lib:format("~w %",[TotCoverage]);
			    _error -> 
				""
			end,
		    Link = filename:join(basename(Name), basename(Last)),
		    JustTheName = rootname(basename(Name)),
		    NotBuilt = not_built(JustTheName),
		    NewResult = [Result, make_index1(JustTheName,
						     Link, Succ, Fail, USkip, ASkip, 
						     NotBuilt, Cov, false)],
		    make_index(Rest, NewResult, TotSucc+Succ, TotFail+Fail, 
			       UserSkip+USkip, AutoSkip+ASkip, TotNotBuilt+NotBuilt);
		error ->
		    make_index(Rest, Result, TotSucc, TotFail, UserSkip, AutoSkip,
			       TotNotBuilt)
	    end
    end;
make_index([], Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt) ->
    {ok, {[Result|make_index1("Total", no_link,
			      TotSucc, TotFail, UserSkip, AutoSkip, 
			      TotNotBuilt, "", true)],
	  {TotSucc, TotFail, UserSkip, AutoSkip}}}.

make_index1(SuiteName, Link, Success, Fail, UserSkip, AutoSkip, NotBuilt, Coverage, Bold) ->
    Name = test_suite_name(SuiteName),
    FailStr =
	if Fail > 0 ->  
		["<FONT color=\"red\">",
		 integer_to_list(Fail),"</FONT>"];
	   true ->
		integer_to_list(Fail)
	end,
    AutoSkipStr =
	if AutoSkip > 0 ->
		["<FONT color=\"brown\">",
		 integer_to_list(AutoSkip),"</FONT>"];
	   true -> integer_to_list(AutoSkip)
	end,
    ["<TR valign=top>\n",
     "<TD>",
     case Link of
	 no_link ->
	     ["<B>", Name|"</B>"];
	 _Other  ->
	     CrashDumpName = SuiteName ++ "_erl_crash.dump",
	     CrashDumpLink = 
		 case filelib:is_file(CrashDumpName) of
		     true -> 
			 ["&nbsp;<A HREF=\"", CrashDumpName, 
			  "\">(CrashDump)</A>"];
		     false ->
			 ""
		 end,
	     LogFile = filename:join(Link, ?suitelog_name ++ ".html"),
	     ["<A HREF=\"", LogFile, "\">", Name, "</A>\n", CrashDumpLink, 
	      "</TD>\n"]
     end,
     make_row(integer_to_list(Success), Bold),
     make_row(FailStr, Bold),
     make_row(integer_to_list(UserSkip), Bold),
     make_row(AutoSkipStr, Bold),
     make_row(integer_to_list(NotBuilt), Bold),
     make_row(Coverage, Bold),
     "</TR>\n"].

make_row(Row, true) ->
    ["<TD ALIGN=right><B>", Row|"</B></TD>"];
make_row(Row, false) ->
    ["<TD ALIGN=right>", Row|"</TD>"].

not_built(BaseName) ->
    Dir = filename:join("..", BaseName++"_test"), 
    Erl = length(filelib:wildcard(filename:join(Dir,"*_SUITE.erl"))),
    Beam = length(filelib:wildcard(filename:join(Dir,"*_SUITE.beam"))),
    Erl-Beam.


%% Add the log file directory for the very last test run (according to
%% last_name).

add_last_name(Logs) ->
    case file:read_file("last_name") of
	{ok, Bin} ->
	    Name = filename:dirname(lib:nonl(binary_to_list(Bin))),
	    case lists:member(Name, Logs) of
		true  -> Logs;
		false -> [Name|Logs]
	    end;
	_ ->
	    Logs
    end.

term_to_text(Term) ->
    lists:flatten(io_lib:format("~p.\n", [Term])).

test_suite_name(Name) ->
    ts_lib:initial_capital(Name) ++ " suite".

directories(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    [filename:join(Dir, X) || X <- Files,
			      filelib:is_dir(filename:join(Dir, X))].


%%% Headers and footers.

header(Vars) ->
    Platform = ts_lib:var(platform_id, Vars),
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
     "<HTML>\n",
     "<HEAD>\n",
     "<TITLE>Test Results for ", Platform, "</TITLE>\n",
     "</HEAD>\n",

     body_tag(),

     "<!-- ---- DOCUMENT TITLE  ---- -->\n",

     "<CENTER>\n",
     "<H1>Test Results for ", Platform, "</H1>\n",
     "</CENTER>\n",

     "<!-- ---- CONTENT ---- -->\n",
     "<CENTER>\n",

     "<TABLE border=3 cellpadding=5>\n",
     "<th><B>Family</B></th>\n",
     "<th>Successful</th>\n",
     "<th>Failed</th>\n",
     "<th>User Skipped</th>\n"
     "<th>Auto Skipped</th>\n"
     "<th>Missing Suites</th>\n"
     "<th>Coverage</th>\n"
     "\n"].

footer() ->
    ["</TABLE>\n"
     "</CENTER>\n"
     "<P><CENTER>\n"
     "<HR>\n"
     "<P><FONT SIZE=-1>\n"
     "Copyright &copy; ", year(),
     " <A HREF=\"http://erlang.ericsson.se\">Open Telecom Platform</A><BR>\n"
     "Updated: <!date>", current_time(), "<!/date><BR>\n"
     "</FONT>\n"
     "</CENTER>\n"
     "</body>\n"
     "</HTML>\n"].

progress_header(Vars) ->
    Release = ts_lib:var(erl_release, Vars),
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
     "<HTML>\n",
     "<HEAD>\n",
     "<TITLE>", Release, " Progress Test Results</TITLE>\n",
     "</HEAD>\n",

     body_tag(),

     "<!-- ---- DOCUMENT TITLE ---- -->\n",

     "<CENTER>\n",
     "<H1>", Release, " Progress Test Results</H1>\n",
     "<TABLE border=3 cellpadding=5>\n",
     "<th><b>Test Run</b></th><th>Platforms</th>\n"].

progress_footer() ->
    ["</TABLE>\n",
     "</CENTER>\n",
     "<P><CENTER>\n",
     "<HR>\n",
     "<P><FONT SIZE=-1>\n",
     "Copyright &copy; ", year(),
     " <A HREF=\"http://erlang.ericsson.se\">Open Telecom Platform</A><BR>\n",
     "Updated: <!date>", current_time(), "<!/date><BR>\n",
     "</FONT>\n",
     "</CENTER>\n",
     "</body>\n",
     "</HTML>\n"].

master_header(Vars) ->
    Release = ts_lib:var(erl_release, Vars),
    Vsn = erlang:system_info(version),
    ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n"
     "<!-- autogenerated by '"++atom_to_list(?MODULE)++"'. -->\n"
     "<HTML>\n",
     "<HEAD>\n",
     "<TITLE>", Release, " Test Results (", Vsn, ")</TITLE>\n",
     "</HEAD>\n",

     body_tag(),

     "<!-- ---- DOCUMENT TITLE ---- -->\n",

     "<CENTER>\n",
     "<H1>", Release, " Test Results (", Vsn, ")</H1>\n",
     "</CENTER>\n",

     "<!-- ---- CONTENT ---- -->\n",

     "<CENTER>\n",
     
     "<TABLE border=3 cellpadding=5>\n",
     "<th><b>Platform</b></th>\n",
     "<th>Successful</th>\n",
     "<th>Failed</th>\n",
     "<th>User Skipped</th>\n"
     "<th>Auto Skipped</th>\n"
     "\n"].

master_footer() ->
    ["</TABLE>\n",
     "</CENTER>\n",
     "<P><CENTER>\n",
     "<HR>\n",
     "<P><FONT SIZE=-1>\n",
     "Copyright &copy; ", year(), 
     " <A HREF=\"http://erlang.ericsson.se\">Open Telecom Platform</A><BR>\n",
     "Updated: <!date>", current_time(), "<!/date><BR>\n",
     "</FONT>\n",
     "</CENTER>\n",
     "</body>\n",
     "</HTML>\n"].

body_tag() ->
    "<body bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\""
	"vlink=\"#800080\" alink=\"#FF0000\">".

year() ->
    {Y, _, _} = date(),
    integer_to_list(Y).

current_time() ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:local_time(),
    Weekday = weekday(calendar:day_of_the_week(Y, Mon, D)),
    lists:flatten(io_lib:format("~s ~s ~p ~2.2.0w:~2.2.0w:~2.2.0w ~w",
				[Weekday, month(Mon), D, H, Min, S, Y])).

weekday(1) -> "Mon";
weekday(2) -> "Tue";
weekday(3) -> "Wed";
weekday(4) -> "Thu";
weekday(5) -> "Fri";
weekday(6) -> "Sat";
weekday(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Count test cases in the given directory (a directory of the type
%% run.1997-08-04_09.58.52).

count_cases(Dir) ->
    SumFile = filename:join(Dir, ?run_summary),
    case read_summary(SumFile, [summary]) of
	{ok, [{Succ,Fail,Skip}]} ->
	    {Succ,Fail,Skip,0};
	{ok, [Summary]} ->
	    Summary;
	{error, _} ->
	    LogFile = filename:join(Dir, ?suitelog_name),
	    case file:read_file(LogFile) of
		{ok, Bin} ->
		    Summary = count_cases1(binary_to_list(Bin), {0, 0, 0, 0}),
		    write_summary(SumFile, Summary),
		    Summary;
		{error, _Reason} ->
		    io:format("\nFailed to read ~p (skipped)\n", [LogFile]),
		    error
	    end
    end.

write_summary(Name, Summary) ->
    File = [term_to_text({summary, Summary})],
    ts_lib:force_write_file(Name, File).

% XXX: This function doesn't do what the writer expect. It can't handle
% the case if there are several different keys and I had to add a special
% case for the empty file. The caller also expect just one tuple as
% a result so this function is written way to general for no reason.
% But it works sort of. /kgb

read_summary(Name, Keys) ->
    case file:consult(Name) of
	{ok, []} ->
	    {error, "Empty summary file"};
	{ok, Terms} ->
	    {ok, lists:map(fun(Key) -> {value, {_, Value}} = 
					   lists:keysearch(Key, 1, Terms),
				       Value end,
			   Keys)};
	{error, Reason} ->
	    {error, Reason}
    end.

count_cases1("=failed" ++ Rest, {Success, _Fail, UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Count, UserSkip,AutoSkip});
count_cases1("=successful" ++ Rest, {_Success, Fail, UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Count, Fail, UserSkip,AutoSkip});
count_cases1("=skipped" ++ Rest, {Success, Fail, _UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, Count,AutoSkip});
count_cases1("=user_skipped" ++ Rest, {Success, Fail, _UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, Count,AutoSkip});
count_cases1("=auto_skipped" ++ Rest, {Success, Fail, UserSkip,_AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, UserSkip,Count});
count_cases1([], Counters) ->
    Counters;
count_cases1(Other, Counters) ->
    count_cases1(skip_to_nl(Other), Counters).

get_number([$\s|Rest]) ->
    get_number(Rest);
get_number([Digit|Rest]) when $0 =< Digit, Digit =< $9 ->
    get_number(Rest, Digit-$0).

get_number([Digit|Rest], Acc) when $0 =< Digit, Digit =< $9 ->
    get_number(Rest, Acc*10+Digit-$0);
get_number([$\n|Rest], Acc) ->
    {Rest, Acc};
get_number([_|Rest], Acc) ->
    get_number(Rest, Acc).

skip_to_nl([$\n|Rest]) ->
    Rest;
skip_to_nl([_|Rest]) ->
    skip_to_nl(Rest);
skip_to_nl([]) ->
    [].
