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
%% @author Mickaël Rémond <mickael.remond@process-one.net>
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
	  result :: ok
                  | {failed, tuple()}
                  | {aborted, tuple()}
                  | {skipped, term()}
                  | undefined,
	  time :: integer(),
	  output :: binary()
	 }).
-record(testsuite,
	{
          id = 0 :: integer(),
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
		testsuites = [] :: [#testsuite{}]
	       }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    XMLDir = proplists:get_value(dir, Options, ?XMLDIR),
    St = #state{verbose = proplists:get_bool(verbose, Options),
		xmldir = XMLDir,
		testsuites = []},
    receive
	{start, _Reference} ->
	    St
    end.

terminate({ok, _Data}, St) ->
    TestSuites = St#state.testsuites,
    XmlDir = St#state.xmldir,
    write_reports(TestSuites, XmlDir),
    ok;
terminate({error, _Reason}, _St) ->
    %% Don't report any errors here, since eunit_tty takes care of that.
    %% Just terminate.
    ok.

handle_begin(Kind, Data, St) when Kind == group; Kind == test ->
    %% Run this code both for groups and tests; test is a bit
    %% surprising: This is a workaround for the fact that we don't get
    %% a group (handle_begin(group, ...) for testsuites (modules)
    %% which only have one test case.  In that case we get a test case
    %% with an id comprised of just one integer - the group id.
    NewId = proplists:get_value(id, Data),
    case NewId of
	[] ->
	    St;
	[GroupId] ->
	    Desc = proplists:get_value(desc, Data),
            TestSuite = #testsuite{id = GroupId, name = Desc},
	    St#state{testsuites=store_suite(TestSuite, St#state.testsuites)};
	%% Surefire format is not hierarchic: Ignore subgroups:
	_ ->
	    St
    end.
handle_end(group, Data, St) ->
    %% Retrieve existing test suite:
    case proplists:get_value(id, Data) of
	[] ->
	    St;
	[GroupId|_] ->
            TestSuites = St#state.testsuites,
	    TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

	    %% Update TestSuite data:
	    Time = proplists:get_value(time, Data),
	    Output = proplists:get_value(output, Data),
	    NewTestSuite = TestSuite#testsuite{ time = Time, output = Output },
	    St#state{testsuites=store_suite(NewTestSuite, TestSuites)}
    end;
handle_end(test, Data, St) ->
    %% Retrieve existing test suite:
    [GroupId|_] = proplists:get_value(id, Data),
    TestSuites = St#state.testsuites,
    TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

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
    St#state{testsuites=store_suite(NewTestSuite, TestSuites)}.

%% Cancel group does not give information on the individual cancelled test case
%% We ignore this event...
handle_cancel(group, Data, St) ->
    %% ...except when it tells us that a fixture setup or cleanup failed.
    case proplists:get_value(reason, Data) of
        {abort, {SomethingFailed, Exception}}
          when SomethingFailed =:= setup_failed;
               SomethingFailed =:= cleanup_failed ->
            [GroupId|_] = proplists:get_value(id, Data),
            TestSuites = St#state.testsuites,
            TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

            %% We don't have any proper name.  Let's give all the
            %% clues that we have.
            Name = case SomethingFailed of
                       setup_failed -> "fixture setup ";
                       cleanup_failed -> "fixture cleanup "
                   end
                ++ io_lib:format("~w", [proplists:get_value(id, Data)]),
            Desc = format_desc(proplists:get_value(desc, Data)),
            TestCase = #testcase{
              name = Name, description = Desc,
              time = 0, output = <<>>},
            NewTestSuite = add_testcase_to_testsuite({error, Exception}, TestCase, TestSuite),
            St#state{testsuites=store_suite(NewTestSuite, TestSuites)};
        _ ->
            St
    end;
handle_cancel(test, Data, St) ->
    %% Retrieve existing test suite:
    [GroupId|_] = proplists:get_value(id, Data),
    TestSuites = St#state.testsuites,
    TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

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
    St#state{testsuites=store_suite(NewTestSuite, TestSuites)}.

format_name({Module, Function, _Arity}, Line) ->
    lists:flatten([atom_to_list(Module), ":", integer_to_list(Line), " ",
                   atom_to_list(Function)]).
format_desc(undefined) ->
    "";
format_desc(Desc) when is_binary(Desc) ->
    binary_to_list(Desc);
format_desc(Desc) when is_list(Desc) ->
    Desc.

lookup_suite_by_group_id(GroupId, TestSuites) ->
    #testsuite{} = lists:keyfind(GroupId, #testsuite.id, TestSuites).

store_suite(#testsuite{id=GroupId} = TestSuite, TestSuites) ->
    lists:keystore(GroupId, #testsuite.id, TestSuites, TestSuite).

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
write_reports(TestSuites, XmlDir) ->
    lists:foreach(fun(TestSuite) -> write_report(TestSuite, XmlDir) end,
                  TestSuites).

write_report(#testsuite{name = Name} = TestSuite, XmlDir) ->
    Filename = filename:join(XmlDir, lists:flatten(["TEST-", escape_suitename(Name)], ".xml")),
    case file:open(Filename, [write,{encoding,utf8}]) of
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
    io:format(FileDescriptor, "~ts~ts", [<<"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>">>, ?NEWLINE]).

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
    io:format(FileDescriptor, "~ts", [StartTag]).

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
    io:format(FileDescriptor, "~ts~ts", [<<"</testsuite>">>, ?NEWLINE]).

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
			  [] -> [];
			  _ -> [<<" (">>, escape_attr(Description), <<")">>]
		      end,
    StartTag = [
        ?INDENT, <<"<testcase time=\"">>, format_time(Time),
        <<"\" name=\"">>, escape_attr(Name), DescriptionAttr, <<"\"">>],
    ContentAndEndTag = case {Result, Output} of
        {ok, <<>>} -> [<<"/>">>, ?NEWLINE];
        _ -> [<<">">>, ?NEWLINE, format_testcase_result(Result), format_testcase_output(Output), ?INDENT, <<"</testcase>">>, ?NEWLINE]
    end,
    io:format(FileDescriptor, "~ts~ts", [StartTag, ContentAndEndTag]).

%% ----------------------------------------------------------------------------
%% Format the result of the test.
%% Failed tests are represented with a failure tag.
%% Aborted tests are represented with an error tag.
%% Skipped tests are represented with a skipped tag.
%% ----------------------------------------------------------------------------
format_testcase_result(ok) -> [<<>>];
format_testcase_result({failed, {error, {Type, _}, _} = Exception}) when is_atom(Type) ->
    [?INDENT, ?INDENT, <<"<failure type=\"">>, escape_attr(atom_to_list(Type)), <<"\">">>, ?NEWLINE,
    <<"::">>, escape_text(eunit_lib:format_exception(Exception, 100)),
    ?INDENT, ?INDENT, <<"</failure>">>, ?NEWLINE];
format_testcase_result({failed, Term}) ->
    [?INDENT, ?INDENT, <<"<failure type=\"unknown\">">>, ?NEWLINE,
    escape_text(io_lib:write(Term)),
    ?INDENT, ?INDENT, <<"</failure>">>, ?NEWLINE];
format_testcase_result({aborted, {Class, _Term, _Trace} = Exception}) when is_atom(Class) ->
    [?INDENT, ?INDENT, <<"<error type=\"">>, escape_attr(atom_to_list(Class)), <<"\">">>, ?NEWLINE,
    <<"::">>, escape_text(eunit_lib:format_exception(Exception, 100)),
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
escape_suitename(Binary) when is_binary(Binary) ->
    escape_suitename(binary_to_list(Binary));
escape_suitename("module '" ++ String) ->
    escape_suitename(String);
escape_suitename(String) ->
    escape_suitename(String, []).

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
escape_text(Text) -> escape_xml(to_utf8(lists:flatten(Text)), [], false).


%% ----------------------------------------------------------------------------
%% Escape text for XML attribute nodes.
%% Replace < with &lt;, > with &gt; and & with &amp;
%% ----------------------------------------------------------------------------
escape_attr(Text) when is_binary(Text) -> escape_attr(binary_to_list(Text));
escape_attr(Text) -> escape_xml(to_utf8(lists:flatten(Text)), [], true).

escape_xml([], Acc, _ForAttr) -> lists:reverse(Acc);
escape_xml([$< | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $t, $l, $& | Acc], ForAttr);
escape_xml([$> | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $t, $g, $& | Acc], ForAttr);
escape_xml([$& | Tail], Acc, ForAttr) -> escape_xml(Tail, [$;, $p, $m, $a, $& | Acc], ForAttr);
escape_xml([$" | Tail], Acc, true) -> escape_xml(Tail, [$;, $t, $o, $u, $q, $& | Acc], true); % "
escape_xml([Char | Tail], Acc, ForAttr) when is_integer(Char) -> escape_xml(Tail, [Char | Acc], ForAttr).

%% the input may be utf8 or latin1; the resulting list is unicode
to_utf8(Desc) when is_binary(Desc) ->
	case unicode:characters_to_list(Desc) of
		{_,_,_} -> unicode:characters_to_list(Desc, latin1);
		X -> X
	end;
to_utf8(Desc) when is_list(Desc) ->
	try
		to_utf8(list_to_binary(Desc))
	catch
		_:_ ->
			Desc
	end.
