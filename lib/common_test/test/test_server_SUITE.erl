%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
%%% Author: Lukas Larsson <lukas@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(test_server_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("test_server_test_lib.hrl").
-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%% @spec suite() -> Info
suite() ->
    [{ct_hooks,[ts_install_cth,test_server_test_lib]}].


%% @spec init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_suite(Config) ->
    [{path_dirs,[proplists:get_value(data_dir,Config)]} | Config].

%% @spec end_per_suite(Config) -> _
end_per_suite(_Config) ->
    io:format("TEST_SERVER_FRAMEWORK: ~p",[os:getenv("TEST_SERVER_FRAMEWORK")]),
    ok.

%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_group(_GroupName, Config) ->
    Config.

%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
end_per_group(_GroupName, _Config) ->
    ok.

%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
init_per_testcase(_TestCase, Config) ->
    Config.

%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
end_per_testcase(test_server_unicode, _Config) ->
    [_,Host] = string:lexemes(atom_to_list(node()), "@"),
    N1 = list_to_atom("test_server_tester_latin1" ++ "@" ++ Host),
    N2 = list_to_atom("test_server_tester_utf8" ++ "@" ++ Host),
    test_server:stop_node(N1),
    test_server:stop_node(N2),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @spec: groups() -> [Group]
groups() ->
    [].

%% @spec all() -> GroupsAndTestCases | {skip,Reason}
all() ->
    [test_server_SUITE, test_server_parallel01_SUITE,
     test_server_conf02_SUITE, test_server_conf01_SUITE,
     test_server_skip_SUITE, test_server_shuffle01_SUITE,
     test_server_break_SUITE, test_server_cover_SUITE,
     test_server_unicode].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%           ok | exit() | {skip,Reason} | {comment,Comment} |
%%           {save_config,Config1} | {skip_and_save,Reason,Config1}
test_server_SUITE(Config) ->
%    rpc:call(Node,dbg, tracer,[]),
%    rpc:call(Node,dbg, p,[all,c]),
%    rpc:call(Node,dbg, tpl,[test_server_ctrl,x]),
    run_test_server_tests("test_server_SUITE",
			  [{test_server_SUITE,skip_case7,"SKIPPED!"}],
			  40, 1, 32, 21, 9, 1, 11, 2, 27, Config).

test_server_parallel01_SUITE(Config) ->
    run_test_server_tests("test_server_parallel01_SUITE", [],
			  37, 0, 19, 19, 0, 0, 0, 0, 37, Config).

test_server_shuffle01_SUITE(Config) ->
    run_test_server_tests("test_server_shuffle01_SUITE", [],
			  130, 0, 0, 76, 0, 0, 0, 0, 130, Config).

test_server_skip_SUITE(Config) ->
    run_test_server_tests("test_server_skip_SUITE", [],
			  3, 0, 1, 0, 1, 0, 3, 0, 0, Config).

test_server_conf01_SUITE(Config) ->
    run_test_server_tests("test_server_conf01_SUITE", [],
			  24, 0, 12, 12, 0, 0, 0, 0, 24, Config).

test_server_conf02_SUITE(Config) ->
    run_test_server_tests("test_server_conf02_SUITE", [],
			  26, 0, 12, 12, 0, 0, 0, 0, 26, Config).

test_server_break_SUITE(Config) ->
    run_test_server_tests("test_server_break_SUITE", [],
			  8, 2, 6, 4, 0, 0, 0, 2, 6, Config).

test_server_cover_SUITE(Config) ->
    case test_server:is_cover() of
	true ->
	    {skip, "Cover already running"};
	false ->
	    PrivDir = ?config(priv_dir,Config),

	    %% Test suite has two test cases
	    %%   tc1 calls cover_helper:foo/0
	    %%   tc2 calls cover_helper:bar/0
	    %% Each function in cover_helper is one line.
	    %%
	    %% First test run skips tc2, so only cover_helper:foo/0 is executed.
	    %% Cover file specifies to include cover_helper in this test run.
	    CoverFile1 = filename:join(PrivDir,"t1.cover"),
	    CoverSpec1 = {include,[cover_helper]},
	    file:write_file(CoverFile1,io_lib:format("~p.~n",[CoverSpec1])),
	    run_test_server_tests("test_server_cover_SUITE",
				  [{test_server_cover_SUITE,tc2,"SKIPPED!"}],
				  4, 0, 2, 1, 1, 0, 1, 0, 3,
				  CoverFile1, Config),

	    %% Next test run skips tc1, so only cover_helper:bar/0 is executed.
	    %% Cover file specifies cross compilation of cover_helper
	    CoverFile2 = filename:join(PrivDir,"t2.cover"),
	    CoverSpec2 = {cross,[{t1,[cover_helper]}]},
	    file:write_file(CoverFile2,io_lib:format("~p.~n",[CoverSpec2])),
	    run_test_server_tests("test_server_cover_SUITE",
				  [{test_server_cover_SUITE,tc1,"SKIPPED!"}],
				  4, 0, 2, 1, 1, 0, 1, 0, 3, CoverFile2, Config),

	    %% Cross cover analyse
	    WorkDir = ?config(work_dir,Config),
	    WC = filename:join([WorkDir,"test_server_cover_SUITE.logs","run.*"]),
	    [D2,D1|_] = lists:reverse(lists:sort(filelib:wildcard(WC))),
	    TagDirs = [{t1,D1},{t2,D2}],
	    test_server_ctrl:cross_cover_analyse(details,TagDirs),

	    %% Check that cover log shows only what is really included
	    %% in the test and cross cover log show the accumulated
	    %% result.
	    {ok,Cover1} = file:read_file(filename:join(D1,"cover.log")),
	    [{cover_helper,{1,1,_}}] = binary_to_term(Cover1),
	    {ok,Cover2} = file:read_file(filename:join(D2,"cover.log")),
	    [] = binary_to_term(Cover2),
	    {ok,Cross} = file:read_file(filename:join(D1,"cross_cover.log")),
	    [{cover_helper,{2,0,_}}] = binary_to_term(Cross),
	    ok
    end.

test_server_unicode(Config) ->
    run_test_server_tests("test_server_unicode_SUITE", [],
			  5, 0, 3, 3, 0, 0, 0, 0, 5, Config),

    %% Create and run two test suites - one with filename and content
    %% in latin1 (if the default filename mode is latin1) and one with
    %% filename and content in utf8.  Both have name and content
    %% including letters äöå.  Check that all logs are generated with
    %% utf8 encoded filenames.
    case file:native_name_encoding() of
	utf8 ->
	    ok;
	latin1 ->
	    generate_and_run_unicode_test(Config,latin1)
    end,
    generate_and_run_unicode_test(Config,utf8).

%%%-----------------------------------------------------------------
run_test_server_tests(SuiteName, Skip, NCases, NFail, NExpected, NSucc,
		      NUsrSkip, NAutoSkip, 
		      NActualSkip, NActualFail, NActualSucc, Config) ->
    run_test_server_tests(SuiteName, Skip, NCases, NFail, NExpected, NSucc,
			  NUsrSkip, NAutoSkip,
			  NActualSkip, NActualFail, NActualSucc, false, Config).

run_test_server_tests(SuiteName, Skip, NCases, NFail, NExpected, NSucc,
		      NUsrSkip, NAutoSkip,
		      NActualSkip, NActualFail, NActualSucc, Cover, Config) ->
    Node = proplists:get_value(node, Config),
    Encoding = rpc:call(Node,file,native_name_encoding,[]),
    WorkDir = proplists:get_value(work_dir, Config),
    LogDir = filename:join(WorkDir, SuiteName++".logs"),
    LogDirUri = test_server_ctrl:uri_encode(LogDir, Encoding),
    ct:log("<a href=\"file://~s\">Test case log files</a>\n", [LogDirUri]),

    {ok,_Pid} = rpc:call(Node,test_server_ctrl, start, []),
    case Cover of
	false ->
	    ok;
	_ ->
	    rpc:call(Node,test_server_ctrl,cover,[Cover,details])
    end,
    rpc:call(Node,
	     test_server_ctrl,add_dir_with_skip,
	     [SuiteName, 
	      [proplists:get_value(data_dir,Config)],SuiteName,
	      Skip]),

    until(fun() ->
		  rpc:call(Node,test_server_ctrl,jobs,[]) =:= []
	  end),
    
    rpc:call(Node,test_server_ctrl, stop, []),

    LogDir1 = translate_filename(LogDir,Encoding),
    LastRunDir = get_latest_run_dir(LogDir1),
    LastSuiteLog = filename:join(LastRunDir,"suite.log"),
    {ok,Data} =	test_server_test_lib:parse_suite(LastSuiteLog),
    check([{"Number of cases",NCases,Data#suite.n_cases},
	   {"Number failed",NFail,Data#suite.n_cases_failed},
	   {"Number expected",NExpected,Data#suite.n_cases_expected},
	   {"Number successful",NSucc,Data#suite.n_cases_succ},
	   {"Number user skipped",NUsrSkip,Data#suite.n_cases_user_skip},
	   {"Number auto skipped",NAutoSkip,Data#suite.n_cases_auto_skip}], ok),
    {NActualSkip,NActualFail,NActualSucc} = 
	lists:foldl(fun(#tc{ result = skip },{S,F,Su}) ->
			     {S+1,F,Su};
		       (#tc{ result = auto_skip },{S,F,Su}) ->
			    {S+1,F,Su};
		       (#tc{ result = ok },{S,F,Su}) ->
			    {S,F,Su+1};
		       (#tc{ result = failed },{S,F,Su}) ->
			    {S,F+1,Su}
		    end,{0,0,0},Data#suite.cases),
    Data.

translate_filename(Filename,EncodingOnTestNode) ->
    case {file:native_name_encoding(),EncodingOnTestNode} of
	{X,X} -> Filename;
	{utf8,latin1} -> list_to_binary(Filename);
	{latin1,utf8} -> unicode:characters_to_binary(Filename)
    end.

get_latest_run_dir(Dir) ->
    %% For the time being, filelib:wildcard cannot take a binary
    %% argument, so we avoid using this here.
    case file:list_dir(Dir) of
	{ok,Files} ->
	    {ok,RE} = re:compile(<<"^run.[1-2][-_\.0-9]*$">>),
	    RunDirs = lists:filter(
			fun(F) ->
				L = l(F),
				case re:run(F,RE) of
				    {match,[{0,L}]} -> true;
				    _ -> false
				end
			end, Files),
	    case RunDirs of
		[] ->
		    Dir;
		[H|T] ->
		    filename:join(Dir,get_latest_dir(T,H))
	    end;
	_ ->
	    Dir
    end.

l(X) when is_binary(X) -> size(X);
l(X) when is_list(X) -> length(X).

get_latest_dir([H|T],Latest) when H>Latest ->
    get_latest_dir(T,H);
get_latest_dir([_|T],Latest) ->
    get_latest_dir(T,Latest);
get_latest_dir([],Latest) ->
    Latest.

check([{Str,Same,Same}|T], Status) ->
    io:format("~s: ~p\n", [Str,Same]),
    check(T, Status);
check([{Str,Expected,Actual}|T], _) ->
    io:format("~s: expected ~p, actual ~p\n", [Str,Expected,Actual]),
    check(T, error);
check([], ok) -> ok;
check([], error) -> ?t:fail().

until(Fun) ->
    case Fun() of
	true ->
	    ok;
	false ->
	    timer:sleep(100),
	    until(Fun)
    end.

generate_and_run_unicode_test(Config0,Encoding) ->
    DataDir = ?config(data_dir,Config0),
    Suite = create_unicode_test_suite(DataDir,Encoding),

    %% We cannot run this test on default node since it must be
    %% started with correct file name mode (+fnu/+fnl).
    %% OBS: the node are stopped by end_per_testcase/2
    Config1 = lists:keydelete(node,1,Config0),
    Config2 = lists:keydelete(work_dir,1,Config1),
    NodeName = list_to_atom("test_server_tester_" ++ atom_to_list(Encoding)),
    Config = start_node(Config2,NodeName,erts_switch(Encoding)),

    %% Compile the suite
    Node = proplists:get_value(node,Config),
    {ok,Mod} = rpc:call(Node,compile,file,[Suite,[report,{outdir,DataDir}]]),
    ModStr = atom_to_list(Mod),

    %% Clean logdir
    LogDir0 = filename:join(DataDir,ModStr++".logs"),
    LogDir = translate_filename(LogDir0,Encoding),
    rm_dir(LogDir),

    %% Run the test
    run_test_server_tests(ModStr, [], 3, 0, 1, 1, 0, 0, 0, 0, 3, Config),

    %% Check that all logs are created with utf8 encoded filenames
    true = filelib:is_dir(LogDir),

    RunDir = get_latest_run_dir(LogDir),
    true = filelib:is_dir(RunDir),

    LowerModStr = string:lowercase(ModStr),
    SuiteHtml = translate_filename(LowerModStr++".src.html",Encoding),
    true = filelib:is_regular(filename:join(RunDir,SuiteHtml)),

    TCLog = translate_filename(LowerModStr++".tc_äöå.html",Encoding),
    true = filelib:is_regular(filename:join(RunDir,TCLog)),
    ok.

%% Same as test_server_test_lib:start_slave, but starts a peer with
%% additional arguments.
%% The reason for this is that we need to start nodes with +fnu/+fnl,
%% and that will not work well with a slave node since slave nodes run
%% remote file system on master - i.e. they will use same file name
%% mode as the master.
start_node(Config,Name,Args) ->
    [_,Host] = string:lexemes(atom_to_list(node()), "@"),
    ct:log("Trying to start ~w@~s~n",[Name,Host]),
    case test_server:start_node(Name, peer, [{args,Args}]) of
	{error,Reason} ->
	    test_server:fail(Reason);
	{ok,Node} ->
	    ct:log("Node ~p started~n", [Node]),
	    test_server_test_lib:prepare_tester_node(Node,Config)
    end.

create_unicode_test_suite(Dir,Encoding) ->
    ModStr = "test_server_"++atom_to_list(Encoding)++"_äöå_SUITE",
    File = filename:join(Dir,ModStr++".erl"),
    Suite =
	["%% -*- ",epp:encoding_to_string(Encoding)," -*-\n",
	 "-module(",ModStr,").\n"
	 "\n"
	 "-export([all/1, init_per_suite/1, end_per_suite/1]).\n"
	 "-export([init_per_testcase/2, end_per_testcase/2]).\n"
	 "-export([tc_äöå/1]).\n"
	 "\n"
	 "-include_lib(\"common_test/include/ct.hrl\").\n"
	 "\n"
	 "all(suite) ->\n"
	 "    [tc_äöå].\n"
	 "\n"
	 "init_per_suite(Config) ->\n"
	 "    Config.\n"
	 "\n"
	 "end_per_suite(_Config) ->\n"
	 "    ok.\n"
	 "\n"
	 "init_per_testcase(_Case,Config) ->\n"
	 "    init_timetrap(500,Config).\n"
	 "\n"
	 "init_timetrap(T,Config) ->\n"
	 "    Dog = ?t:timetrap(T),\n"
	 "    [{watchdog, Dog}|Config].\n"
	 "\n"
	 "end_per_testcase(_Case,Config) ->\n"
	 "    cancel_timetrap(Config).\n"
	 "\n"
	 "cancel_timetrap(Config) ->\n"
	 "    Dog=?config(watchdog, Config),\n"
	 "    ?t:timetrap_cancel(Dog),\n"
	 "    ok.\n"
	 "\n"
	 "tc_äöå(Config) when is_list(Config) ->\n"
	 "    true = filelib:is_dir(?config(priv_dir,Config)),\n"
	 "    ok.\n"],
    {ok,Fd} = file:open(raw_filename(File,Encoding),[write,{encoding,Encoding}]),
    io:put_chars(Fd,Suite),
    ok = file:close(Fd),
    File.

raw_filename(Name,latin1) -> list_to_binary(Name);
raw_filename(Name,utf8)   -> unicode:characters_to_binary(Name).

rm_dir(Dir) ->
    case file:list_dir(Dir) of
	{error,enoent} ->
	    ok;
	{ok,Files} ->
	    rm_files([filename:join(Dir, F) || F <- Files]),
	    file:del_dir(Dir)
    end.

rm_files([F | Fs]) ->
    case file:read_file_info(F) of
	{ok,#file_info{type=directory}} ->
	    rm_dir(F),
	    rm_files(Fs);
	{ok,_Regular} ->
	    case file:delete(F) of
		ok ->
		    rm_files(Fs);
		{error,Errno} ->
		    exit({del_failed,F,Errno})
	    end
    end;
rm_files([]) ->
    ok.

erts_switch(latin1) -> "+fnl";
erts_switch(utf8)   -> "+fnu".
