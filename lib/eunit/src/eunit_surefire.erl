%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: $
%%
%% @author Mickaël Rémond <mremond@process-one.net>
%% @copyright 2009 Mickaël Rémond, Paul Guyot
%% @see eunit
%% @doc Surefire reports for EUnit (Format used by Maven and Atlassian
%% Bamboo for example to integrate test results). Based on initial code
%% from Paul Guyot.
%%
%% Example: Generate XML result file in the current directory:
%% ```eunit:test([fib, eunit_examples],
%%               [{report,{eunit_surefire,[{dir,"."}]}}]).'''

-module(eunit_surefire).

-behaviour(eunit_listener).

-define(NODEBUG, true).
-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/0, start/1]).

-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3,
	 terminate/2]).

%% ============================================================================
%% MACROS
%% ============================================================================
-define(XMLDIR, ".").
-define(INDENT, <<"  ">>).
-define(NEWLINE, <<"\n">>).

%% ============================================================================
%% TYPES
%% ============================================================================
-type(chars() :: [char() | any()]). % chars()

%% ============================================================================
%% RECORDS
%% ============================================================================
-record(testcase,
	{
	  name :: chars(),
	  description :: chars(),
	  result :: ok | {failed, tuple()} | {aborted, tuple()} | {skipped, tuple()},
	  time :: integer(),
	  output :: binary()
	 }).
-record(testsuite,
	{
	  name = <<>> :: binary(),
	  time = 0 :: integer(),
	  output = <<>> :: binary(),
	  succeeded = 0 :: integer(),
	  failed = 0 :: integer(),
	  aborted = 0 :: integer(),
	  skipped = 0 :: integer(),
	  testcases = [] :: [#testcase{}]
    }).
-record(state, {verbose = false,
		indent = 0,
		xmldir = ".",
		testsuite = #testsuite{}
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    XMLDir = proplists:get_value(dir, Options, ?XMLDIR),
    St = #state{verbose = proplists:get_bool(verbose, Options),
		xmldir = XMLDir,
		testsuite = #testsuite{}},
    receive
	{start, _Reference} ->
	    St
    end.

terminate({ok, _Data}, St) ->
    TestSuite = St#state.testsuite,
    XmlDir = St#state.xmldir,
    write_report(TestSuite, XmlDir),
    ok;
terminate({error, Reason}, _St) ->
    io:fwrite("Internal error: ~P.\n", [Reason, 25]),
    sync_end(error).

sync_end(Result) ->
    receive
	{stop, Reference, ReplyTo} ->
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

handle_begin(group, Data, St) ->
    NewId = proplists:get_value(id, Data),
    case NewId of
	[] ->
	    St;
	[_GroupId] ->
	    Desc = proplists:get_value(desc, Data),
	    TestSuite = St#state.testsuite,
	    NewTestSuite = TestSuite#testsuite{name = Desc},
	    St#state{testsuite=NewTestSuite};
	%% Surefire format is not hierarchic: Ignore subgroups:
	_ ->
	    St
    end;
handle_begin(test, _Data, St) ->
    St.
handle_end(group, Data, St) ->
    %% Retrieve existing test suite:
    case proplists:get_value(id, Data) of
	[] ->
	    St;
	[_GroupId|_] ->
	    TestSuite = St#state.testsuite,

	    %% Update TestSuite data:
	    Time = proplists:get_value(time, Data),
	    Output = proplists:get_value(output, Data),
	    NewTestSuite = TestSuite#testsuite{ time = Time, output = Output },
	    St#state{testsuite=NewTestSuite}
    end;
handle_end(test, Data, St) ->
    %% Retrieve existing test suite:
    TestSuite = St#state.testsuite,

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
		       proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Result = proplists:get_value(status, Data),
    Time = proplists:get_value(time, Data),
    Output = proplists:get_value(output, Data),
    TestCase = #testcase{name = Name, description = Desc,
			 time = Time,output = Output},
    NewTestSuite = add_testcase_to_testsuite(Result, TestCase, TestSuite),
    St#state{testsuite=NewTestSuite}.

%% Cancel group does not give information on the individual cancelled test case
%% We ignore this event
handle_cancel(group, _Data, St) ->
    St;
handle_cancel(test, Data, St) ->
    %% Retrieve existing test suite:
    TestSuite = St#state.testsuite,

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
		       proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Reason = proplists:get_value(reason, Data),
    TestCase = #testcase{
      name = Name, description = Desc,
      result = {skipped, Reason}, time = 0,
      output = <<>>},
    NewTestSuite = TestSuite#testsuite{
		     skipped = TestSuite#testsuite.skipped+1,
		     testcases=[TestCase|TestSuite#testsuite.testcases] },
    St#state{testsuite=NewTestSuite}.

format_name({Module, Function, Arity}, Line) ->
    lists:flatten([atom_to_list(Module), ":", atom_to_list(Function), "/",
		   integer_to_list(Arity), "_", integer_to_list(Line)]).
format_desc(undefined) ->
    "";
format_desc(Desc) when is_binary(Desc) ->
    binary_to_list(Desc);
format_desc(Desc) when is_list(Desc) ->
    Desc.

%% Add testcase to testsuite depending on the result of the test.
add_testcase_to_testsuite(ok, TestCaseTmp, TestSuite) ->
    TestCase = TestCaseTmp#testcase{ result = ok },
    TestSuite#testsuite{
      succeeded = TestSuite#testsuite.succeeded+1,
      testcases=[TestCase|TestSuite#testsuite.testcases] };
add_testcase_to_testsuite({error, Exception}, TestCaseTmp, TestSuite) ->
    case Exception of
	{error,{AssertionException,_},_} when
	AssertionException == assertion_failed;
	AssertionException == assertMatch_failed;
	AssertionException == assertEqual_failed;
	AssertionException == assertException_failed;
	AssertionException == assertCmd_failed;
	AssertionException == assertCmdOutput_failed
	->
	    TestCase = TestCaseTmp#testcase{ result = {failed, Exception} },
	    TestSuite#testsuite{
	      failed = TestSuite#testsuite.failed+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] };
	_ ->
	    TestCase = TestCaseTmp#testcase{ result = {aborted, Exception} },
	    TestSuite#testsuite{
	      aborted = TestSuite#testsuite.aborted+1,
	      testcases = [TestCase|TestSuite#testsuite.testcases] }
    end.

%% ----------------------------------------------------------------------------
%% Write a report to the XML directory.
%% This function opens the report file, calls write_report_to/2 and closes the file.
%% ----------------------------------------------------------------------------
write_report(#testsuite{name = Name} = TestSuite, XmlDir) ->
    Filename = filename:join(XmlDir, lists:flatten(["TEST-", escape_suitename(Name)], ".xml")),
    case file:open(Filename, [write, raw]) of
        {ok, FileDescriptor} ->
            try
                write_report_to(TestSuite, FileDescriptor)
            after
                file:close(FileDescriptor)
            end;
        {error, _Reason} = Error -> throw(Error)
    end.

%% ----------------------------------------------------------------------------
%% Actually write a report.
%% ----------------------------------------------------------------------------
write_report_to(TestSuite, FileDescriptor) ->
    write_header(FileDescriptor),
    write_start_tag(TestSuite, FileDescriptor),
    write_testcases(lists:reverse(TestSuite#testsuite.testcases), FileDescriptor),
    write_end_tag(FileDescriptor).

%% ----------------------------------------------------------------------------
%% Write the XML header.
%% ----------------------------------------------------------------------------
write_header(FileDescriptor) ->
    file:write(FileDescriptor, [<<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>">>, ?NEWLINE]).

%% ----------------------------------------------------------------------------
%% Write the testsuite start tag, with attributes describing the statistics
%% of the test suite.
%% ----------------------------------------------------------------------------
write_start_tag(
        #testsuite{
            name = Name,
            time = Time,
            succeeded = Succeeded,
            failed = Failed,
            skipped = Skipped,
            aborted = Aborted},
        FileDescriptor) ->
    Total = Succeeded + Failed + Skipped + Aborted,
    StartTag = [
        <<"<testsuite tests=\"">>, integer_to_list(Total),
        <<"\" failures=\"">>, integer_to_list(Failed),
        <<"\" errors=\"">>, integer_to_list(Aborted),
        <<"\" skipped=\"">>, integer_to_list(Skipped),
        <<"\" time=\"">>, format_time(Time),
        <<"\" name=\"">>, escape_attr(Name),
        <<"\">">>, ?NEWLINE],
    file:write(FileDescriptor, StartTag).

%% ----------------------------------------------------------------------------
%% Recursive function to write the test cases.
%% ----------------------------------------------------------------------------
write_testcases([], _FileDescriptor) -> void;
write_testcases([TestCase| Tail], FileDescriptor) ->
    write_testcase(TestCase, FileDescriptor),
    write_testcases(Tail, FileDescriptor).

%% ----------------------------------------------------------------------------
%% Write the testsuite end tag.
%% ----------------------------------------------------------------------------
write_end_tag(FileDescriptor) ->
    file:write(FileDescriptor, [<<"</testsuite>">>, ?NEWLINE]).

%% ----------------------------------------------------------------------------
%% Write a test case, as a testcase tag.
%% If the test case was successful and if there was no output, we write an empty
%% tag.
%% ----------------------------------------------------------------------------
write_testcase(
        #testcase{
            name = Name,
            description = Description,
            result = Result,
            time = Time,
            output = Output},
        FileDescriptor) ->
    DescriptionAttr = case Description of
			  <<>> -> [];
			  [] -> [];
			  _ -> [<<" description=\"">>, escape_attr(Description), <<"\"">>]
		      end,
    StartTag = [
        ?INDENT, <<"<testcase time=\"">>, format_time(Time),
        <<"\" name=\"">>, escape_attr(Name), <<"\"">>,
        DescriptionAttr],
    ContentAndEndTag = case {Result, Output} of
        {ok, []} -> [<<"/>">>, ?NEWLINE];
        {ok, <<>>} -> [<<"/>">>, ?NEWLINE];
        _ -> [<<">">>, ?NEWLINE, format_testcase_result(Result), format_testcase_output(Output), ?INDENT, <<"</testcase>">>, ?NEWLINE]
    end,
    file:write(FileDescriptor, [StartTag, ContentAndEndTag]).

%% ----------------------------------------------------------------------------
%% Format the result of the test.
%% Failed tests are represented with a failure tag.
%% Aborted tests are represented with an error tag.
%% Skipped tests are represented with a skipped tag.
%% ----------------------------------------------------------------------------
format_testcase_result(ok) -> [<<>>];
format_testcase_result({failed, {error, {Type, _}, _} = Exception}) when is_atom(Type) ->
    [?INDENT, ?INDENT, <<"<failure type=\"">>, escape_attr(atom_to_list(Type)), <<"\">">>, ?NEWLINE,
    <<"::">>, escape_text(eunit_lib:format_exception(Exception)),
    ?INDENT, ?INDENT, <<"</failure>">>, ?NEWLINE];
format_testcase_result({failed, Term}) ->
    [?INDENT, ?INDENT, <<"<failure type=\"unknown\">">>, ?NEWLINE,
    escape_text(io_lib:write(Term)),
    ?INDENT, ?INDENT, <<"</failure>">>, ?NEWLINE];
format_testcase_result({aborted, {Class, _Term, _Trace} = Exception}) when is_atom(Class) ->
    [?INDENT, ?INDENT, <<"<error type=\"">>, escape_attr(atom_to_list(Class)), <<"\">">>, ?NEWLINE,
    <<"::">>, escape_text(eunit_lib:format_exception(Exception)),
    ?INDENT, ?INDENT, <<"</error>">>, ?NEWLINE];
format_testcase_result({aborted, Term}) ->
    [?INDENT, ?INDENT, <<"<error type=\"unknown\">">>, ?NEWLINE,
    escape_text(io_lib:write(Term)),
    ?INDENT, ?INDENT, <<"</error>">>, ?NEWLINE];
format_testcase_result({skipped, {abort, Error}}) when is_tuple(Error) ->
    [?INDENT, ?INDENT, <<"<skipped type=\"">>, escape_attr(atom_to_list(element(1, Error))), <<"\">">>, ?NEWLINE,
    escape_text(eunit_lib:format_error(Error)),
    ?INDENT, ?INDENT, <<"</skipped>">>, ?NEWLINE];
format_testcase_result({skipped, {Type, Term}}) when is_atom(Type) ->
    [?INDENT, ?INDENT, <<"<skipped type=\"">>, escape_attr(atom_to_list(Type)), <<"\">">>, ?NEWLINE,
    escape_text(io_lib:write(Term)),
    ?INDENT, ?INDENT, <<"</skipped>">>, ?NEWLINE];
format_testcase_result({skipped, timeout}) ->
    [?INDENT, ?INDENT, <<"<skipped type=\"timeout\"/>">>, ?NEWLINE];
format_testcase_result({skipped, Term}) ->
    [?INDENT, ?INDENT, <<"<skipped type=\"unknown\">">>, ?NEWLINE,
    escape_text(io_lib:write(Term)),
    ?INDENT, ?INDENT, <<"</skipped>">>, ?NEWLINE].

%% ----------------------------------------------------------------------------
%% Format the output of a test case in xml.
%% Empty output is simply the empty string.
%% Other output is inside a <system-out> xml tag.
%% ----------------------------------------------------------------------------
format_testcase_output([]) -> [];
format_testcase_output(Output) ->
    [?INDENT, ?INDENT, <<"<system-out>">>, escape_text(Output), ?NEWLINE, ?INDENT, ?INDENT, <<"</system-out>">>, ?NEWLINE].

%% ----------------------------------------------------------------------------
%% Return the time in the SECS.MILLISECS format.
%% ----------------------------------------------------------------------------
format_time(Time) ->
    format_time_s(lists:reverse(integer_to_list(Time))).
format_time_s([Digit]) -> ["0.00", Digit];
format_time_s([Digit1, Digit2]) -> ["0.0", Digit2, Digit1];
format_time_s([Digit1, Digit2, Digit3]) -> ["0.", Digit3, Digit2, Digit1];
format_time_s([Digit1, Digit2, Digit3 | Tail]) -> [lists:reverse(Tail), $., Digit3, Digit2, Digit1].

%% ----------------------------------------------------------------------------
%% Escape a suite's name to generate the filename.
%% Remark: we might overwrite another testsuite's file.
%% ----------------------------------------------------------------------------
escape_suitename([Head | _T] = List) when is_list(Head) ->
    escape_suitename(lists:flatten(List));
escape_suitename(Binary) when is_binary(Binary) ->
    escape_suitename(binary_to_list(Binary));
escape_suitename("module '" ++ String) ->
    escape_suitename(String);
escape_suitename(String) ->
    escape_suitename(String, []).

escape_suitename(Binary, Acc) when is_binary(Binary) -> escape_suitename(binary_to_list(Binary), Acc);
escape_suitename([], Acc) -> lists:reverse(Acc);
escape_suitename([$  | Tail], Acc) -> escape_suitename(Tail, [$_ | Acc]);
escape_suitename([$' | Tail], Acc) -> escape_suitename(Tail, Acc);
escape_suitename([$/ | Tail], Acc) -> escape_suitename(Tail, [$: | Acc]);
escape_suitename([$\\ | Tail], Acc) -> escape_suitename(Tail, [$: | Acc]);
escape_suitename([Char | Tail], Acc) when Char < $! -> escape_suitename(Tail, Acc);
escape_suitename([Char | Tail], Acc) when Char > $~ -> escape_suitename(Tail, Acc);
escape_suitename([Char | Tail], Acc) -> escape_suitename(Tail, [Char | Acc]).

%% ----------------------------------------------------------------------------
%% Escape text for XML text nodes.
%% Replace < with &lt;, > with &gt; and & with &amp;
%% ----------------------------------------------------------------------------
escape_text(Text) when is_binary(Text) -> escape_text(binary_to_list(Text));
escape_text(Text) -> escape_xml(lists:flatten(Text), [], false).


%% ----------------------------------------------------------------------------
%% Escape text for XML attribute nodes.
%% Replace < with &lt;, > with &gt; and & with &amp;
%% ----------------------------------------------------------------------------
escape_attr(Text) when is_binary(Text) -> escape_attr(binary_to_list(Text));
escape_attr(Text) -> escape_xml(lists:flatten(Text), [], true).

escape_xml([], Acc, _ForAttr) -> lists:reverse(Acc);
escape_xml([$< | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $t, $l, $& | Acc], ForAttr);
escape_xml([$> | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $t, $g, $& | Acc], ForAttr);
escape_xml([$& | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $p, $m, $a, $& | Acc], ForAttr);
escape_xml([$" | Tail], Acc, true) -> escape_xml(Tail, [$;, $t, $o, $u, $q, $& | Acc], true); % "
escape_xml([Char | Tail], Acc, ForAttr) when is_integer(Char) -> escape_xml(Tail, [Char | Acc], ForAttr).
