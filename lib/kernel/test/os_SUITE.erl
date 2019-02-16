%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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
-module(os_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([space_in_cwd/1, quoting/1, cmd_unicode/1, 
         null_in_command/1, space_in_name/1, bad_command/1,
	 find_executable/1, unix_comment_in_command/1, deep_list_command/1,
         large_output_command/1, background_command/0, background_command/1,
         message_leak/1, close_stdin/0, close_stdin/1, max_size_command/1,
         perf_counter_api/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [space_in_cwd, quoting, cmd_unicode, null_in_command,
     space_in_name, bad_command,
     find_executable, unix_comment_in_command, deep_list_command,
     large_output_command, background_command, message_leak,
     close_stdin, max_size_command, perf_counter_api].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TC, Config)
  when TC =:= background_command; TC =:= close_stdin ->
    case os:type() of
        {win32, _} ->
            {skip,"Should not work on windows"};
        _ ->
            Config
    end;
init_per_testcase(_TC,Config) ->
    Config.

end_per_testcase(_,_Config) ->
    ok.

%% Test that executing a command in a current working directory
%% with space in its name works.
space_in_cwd(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    Dirname = filename:join(PrivDir, "cwd with space"),
    ok = file:make_dir(Dirname),
    ok = file:set_cwd(Dirname),

    %% Using `more' gives the almost the same result on both Unix and Windows.

    Cmd = case os:type() of
	      {win32, _} ->
		  "more";
	      {unix, _} ->
		  "more </dev/null"
	  end,

    case os:cmd(Cmd) of
	[] -> ok;				% Unix.
	"\r\n" -> ok;			% Windows.
	Other ->
	    ct:fail({unexpected, Other})
    end,

    ct:sleep(5),
    [] = receive_all(),
    ok.

%% Test that various ways of quoting arguments work.
quoting(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Echo = filename:join(DataDir, "my_echo"),

    comp("one", os:cmd(Echo ++ " one")),
    comp("one::two", os:cmd(Echo ++ " one two")),
    comp("one two", os:cmd(Echo ++ " \"one two\"")),
    comp("x::one two::y", os:cmd(Echo ++ " x \"one two\" y")),
    comp("x::one two", os:cmd(Echo ++ " x \"one two\"")),
    comp("one two::y", os:cmd(Echo ++ " \"one two\" y")),
    comp("x::::y", os:cmd(Echo ++ " x \"\" y")),
    ct:sleep(5),
    [] = receive_all(),
    ok.


%% Test that unicode arguments work.
cmd_unicode(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Echo = filename:join(DataDir, "my_echo"),

    comp("one", os:cmd(Echo ++ " one")),
    comp("one::two", os:cmd(Echo ++ " one two")),
    comp("åäö::ϼΩ", os:cmd(Echo ++ " åäö " ++ [1020, 937])),
    ct:sleep(5),
    [] = receive_all(),
    ok.

null_in_command(Config) ->
    {Ok, Error} = case os:type() of
                      {win32,_} -> {"dir", "di\0r"};
                      _ -> {"ls", "l\0s"}
                  end,
    true = is_list(try os:cmd(Ok) catch Class0:_ -> Class0 end),
    error = try os:cmd(Error) catch Class1:_ -> Class1 end,
    ok.

%% Test that program with a space in its name can be executed.
space_in_name(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    DataDir = proplists:get_value(data_dir, Config),
    Spacedir = filename:join(PrivDir, "program files"),
    Ext = case os:type() of
	      {win32,_} -> ".exe";
	      _ -> ""
	  end,
    OrigEcho = filename:join(DataDir, "my_echo" ++ Ext),
    Echo0 = filename:join(Spacedir, "my_echo" ++ Ext),

    %% Copy the `my_echo' program to a directory whose name contains a space.

    ok = file:make_dir(Spacedir),
    {ok, Bin} = file:read_file(OrigEcho),
    ok = file:write_file(Echo0, Bin),
    Echo = filename:nativename(Echo0),
    ok = file:change_mode(Echo, 8#777),	% Make it executable on Unix.

    %% Run the echo program.
    %% Quoting on windows depends on if the full path of the executable
    %% contains special characters. Paths when running common_tests always
    %% include @, why Windows would always fail if we do not double the
    %% quotes (this is the behaviour of cmd.exe, not Erlang's idea).
    Quote = case os:type() of
                {win32,_} ->
		    case (Echo -- "&<>()@^|") =:= Echo of
		        true -> "\"";
			false -> "\"\""
	 	    end;
		_ ->
		    "\""
	    end,
    comp("", os:cmd(Quote ++ Echo ++ Quote)),
    comp("a::b::c", os:cmd(Quote ++ Echo ++ Quote ++ " a b c")),
    ct:sleep(5),
    [] = receive_all(),
    ok.

%% Check that a bad command doesn't crasch the server or the emulator (it used to).
bad_command(Config) when is_list(Config) ->
    catch os:cmd([a|b]),
    catch os:cmd({bad, thing}),

    %% This should at least not crash (on Unix it typically returns
    %% a message from the shell).
    os:cmd("xxxxx"),

    ok.

find_executable(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    DataDir = filename:join(proplists:get_value(data_dir, Config), "win32"),
	    ok = file:set_cwd(filename:join([DataDir, "current"])),
	    Bin = filename:join(DataDir, "bin"),
	    Abin = filename:join(DataDir, "abin"),
	    UsrBin = filename:join([DataDir, "usr", "bin"]),
	    {ok, Current} = file:get_cwd(),

	    Path = lists:concat([Bin, ";", Abin, ";", UsrBin]),
	    io:format("Path = ~s", [Path]),

	    %% Search for programs in Bin (second element in PATH).
	    find_exe(Abin, "my_ar", ".exe", Path),
	    find_exe(Abin, "my_ascii", ".com", Path),
	    find_exe(Abin, "my_adb", ".bat", Path),
	    %% OTP-3626 find names of executables given with extension
	    find_exe(Abin, "my_ar.exe", "", Path),
	    find_exe(Abin, "my_ascii.com", "", Path),
	    find_exe(Abin, "my_adb.bat", "", Path),
	    find_exe(Abin, "my_ar.EXE", "", Path),
	    find_exe(Abin, "my_ascii.COM", "", Path),
	    find_exe(Abin, "MY_ADB.BAT", "", Path),

	    %% Search for programs in Abin (second element in PATH).
	    find_exe(Abin, "my_ar", ".exe", Path),
	    find_exe(Abin, "my_ascii", ".com", Path),
	    find_exe(Abin, "my_adb", ".bat", Path),

	    %% Search for programs in the current working directory.
	    find_exe(Current, "my_program", ".exe", Path),
	    find_exe(Current, "my_command", ".com", Path),
	    find_exe(Current, "my_batch", ".bat", Path),
	    ok;
	{unix, _}  ->
	    DataDir = proplists:get_value(data_dir, Config),

	    %% Smoke test.
	    case ct:get_progname() of
		"erl" ->
		    ErlPath = os:find_executable("erl"),
		    true = is_list(ErlPath),
		    true = filelib:is_regular(ErlPath);
		_ ->
		    %% Don't bother -- the progname could include options.
		    ok
	    end,

	    %% Never return a directory name.
	    false = os:find_executable("unix", [DataDir]),
	    ok
    end.

find_exe(Where, Name, Ext, Path) ->
    Expected = filename:join(Where, Name++Ext),
    case os:find_executable(Name, Path) of
	Expected ->
	    ok;
	Name when is_list(Name) ->
	    case filename:absname(Name) of
		Expected ->
		    ok;
		Other ->
		    io:format("Expected ~p; got (converted to absolute) ~p",
			      [Expected, Other]),
		    ct:fail(failed)
	    end;
	Other ->
	    io:format("Expected ~p; got ~p", [Expected, Other]),
	    ct:fail(failed)
    end.

%% OTP-1805: Test that os:cmd(\ls #\) works correctly (used to hang).
unix_comment_in_command(Config) when is_list(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    ok = file:set_cwd(Priv),
    _ = os:cmd("ls #"),			% Any result is ok.
    ct:sleep(5),
    [] = receive_all(),
    ok.

%% Check that a deep list in command works equally on unix and on windows.
deep_list_command(Config) when is_list(Config) ->
    %% As a 'io_lib' module description says: "There is no guarantee that the
    %% character lists returned from some of the functions are flat, they can
    %% be deep lists."
    %% That's why os:cmd/1 can have arguments that are deep lists.
    %% It is not a problem for unix, but for windows it is (in R15B02 for ex.).
    Echo = os:cmd([$e, $c, "ho"]),
    true = erlang:is_list(Echo),
    %% FYI: [$e, $c, "ho"] =:= io_lib:format("ec~s", ["ho"])
    ok.

%% Test to make sure that the correct data is
%% received when doing large commands.
large_output_command(Config) when is_list(Config) ->
    %% Maximum allowed on windows is 8192, so we test well below that
    AAA = lists:duplicate(7000, $a),
    comp(AAA,os:cmd("echo " ++ AAA)).

%% Test that it is possible on unix to start a background task using os:cmd.
background_command() ->
    [{timetrap, {seconds, 5}}].
background_command(_Config) ->
    %% This testcase fails when the os:cmd takes
    %% longer then the 5 second timeout
    os:cmd("sleep 10&").

%% Test that message does not leak to the calling process
message_leak(_Config) ->
    process_flag(trap_exit, true),

    os:cmd("echo hello"),
    [] = receive_all(),

    case os:type() of
        {unix, _} ->
            os:cmd("for i in $(seq 1 100); do echo hello; done&"),
            [] = receive_all();
        _ ->
            ok % Cannot background on non-unix
    end,

    process_flag(trap_exit, false).

%% Test that os:cmd closes stdin of the program that is executed
close_stdin() ->
    [{timetrap, {seconds, 5}}].
close_stdin(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Fds = filename:join(DataDir, "my_fds"),

    "-1" = os:cmd(Fds).

max_size_command(_Config) ->

    Res20 = os:cmd("cat /dev/zero", #{ max_size => 20 }),
    20 = length(Res20),

    Res0 = os:cmd("cat /dev/zero", #{ max_size => 0 }),
    0 = length(Res0),

    Res32768 = os:cmd("cat /dev/zero", #{ max_size => 32768 }),
    32768 = length(Res32768),

    ResHello = string:trim(os:cmd("echo hello", #{ max_size => 20 })),
    5 = length(ResHello).

%% Test that the os:perf_counter api works as expected
perf_counter_api(_Config) ->

    true = is_integer(os:perf_counter()),
    true = os:perf_counter() > 0,

    Conv = fun(T1, T2) ->
                   erlang:convert_time_unit(T2 - T1, perf_counter, nanosecond)
           end,

    do_perf_counter_test([], Conv, 120000000, 80000000),
    do_perf_counter_test([1000], fun(T1, T2) -> T2 - T1 end, 120, 80).

do_perf_counter_test(CntArgs, Conv, Upper, Lower) ->
    %% We run the test multiple times to try to get a somewhat
    %% stable value... what does this test? That the
    %% calculate_perf_counter_unit in sys_time.c works somewhat ok.
    do_perf_counter_test(CntArgs, Conv, Upper, Lower, 10).

do_perf_counter_test(CntArgs, _Conv, Upper, Lower, 0) ->
    ct:fail("perf_counter_test ~p ~p ~p",[CntArgs, Upper, Lower]);
do_perf_counter_test(CntArgs, Conv, Upper, Lower, Iters) ->

    T1 = apply(os, perf_counter, CntArgs),
    timer:sleep(100),
    T2 = apply(os, perf_counter, CntArgs),
    TsDiff = Conv(T1, T2),
    ct:log("T1: ~p~n"
           "T2: ~p~n"
           "TsDiff: ~p~n",
           [T1,T2,TsDiff]),

    if
        TsDiff < Upper, TsDiff > Lower ->
            ok;
        true ->
            do_perf_counter_test(CntArgs, Conv, Upper, Lower, Iters-1)
    end.

%% Util functions

comp(Expected, Got) ->
    case strip_nl(Got) of
	Expected ->
	    ok;
	Other ->
	    ok = io:format("Expected: ~ts\n", [Expected]),
	    ok = io:format("Got:      ~ts\n", [Other]),
	    ct:fail(failed)
    end.

%% strips \n and \r\n from end of string

strip_nl([$\r, $\n]) -> [];
strip_nl([$\n])      -> [];
strip_nl([H|T])      -> [H|strip_nl(T)];
strip_nl([])         -> [].

receive_all() ->
    receive
	X -> [X|receive_all()]
    after 0 -> []
    end.
