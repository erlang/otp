%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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
-module(os_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([space_in_cwd/1, quoting/1, cmd_unicode/1, space_in_name/1, bad_command/1,
	 find_executable/1, unix_comment_in_command/1, deep_list_command/1, evil/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [space_in_cwd, quoting, cmd_unicode, space_in_name, bad_command,
     find_executable, unix_comment_in_command, deep_list_command, evil].

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


space_in_cwd(doc) ->
    "Test that executing a command in a current working directory "
	"with space in its name works.";
space_in_cwd(suite) -> [];
space_in_cwd(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line Dirname = filename:join(PrivDir, "cwd with space"),
    ?line ok = file:make_dir(Dirname),
    ?line ok = file:set_cwd(Dirname),

    %% Using `more' gives the almost the same result on both Unix and Windows.

    Cmd = case os:type() of
	      {win32, _} ->
		  "more";
	      {unix, _} ->
		  "more </dev/null"
	  end,

    ?line case os:cmd(Cmd) of
	      [] -> ok;				% Unix.
	      "\r\n" -> ok;			% Windows.
	      Other ->
		  ?line test_server:fail({unexpected, Other})
	  end,

    ?t:sleep(5),
    ?line [] = receive_all(),
    ok.

quoting(doc) -> "Test that various ways of quoting arguments work.";
quoting(suite) -> [];
quoting(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Echo = filename:join(DataDir, "my_echo"),

    ?line comp("one", os:cmd(Echo ++ " one")),
    ?line comp("one::two", os:cmd(Echo ++ " one two")),
    ?line comp("one two", os:cmd(Echo ++ " \"one two\"")),
    ?line comp("x::one two::y", os:cmd(Echo ++ " x \"one two\" y")),
    ?line comp("x::one two", os:cmd(Echo ++ " x \"one two\"")),
    ?line comp("one two::y", os:cmd(Echo ++ " \"one two\" y")),
    ?line comp("x::::y", os:cmd(Echo ++ " x \"\" y")),
    ?t:sleep(5),
    ?line [] = receive_all(),
    ok.


cmd_unicode(doc) -> "Test that unicode arguments work.";
cmd_unicode(suite) -> [];
cmd_unicode(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Echo = filename:join(DataDir, "my_echo"),

    ?line comp("one", os:cmd(Echo ++ " one")),
    ?line comp("one::two", os:cmd(Echo ++ " one two")),
    ?line comp("åäö::ϼΩ", os:cmd(Echo ++ " åäö " ++ [1020, 937])),
    ?t:sleep(5),
    ?line [] = receive_all(),
    ok.


space_in_name(doc) ->
    "Test that program with a space in its name can be executed.";
space_in_name(suite) -> [];
space_in_name(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line DataDir = ?config(data_dir, Config),
    ?line Spacedir = filename:join(PrivDir, "program files"),
    Ext = case os:type() of
	      {win32,_} -> ".exe";
	      _ -> ""
	  end,
    ?line OrigEcho = filename:join(DataDir, "my_echo" ++ Ext),
    ?line Echo0 = filename:join(Spacedir, "my_echo" ++ Ext),

    %% Copy the `my_echo' program to a directory whose name contains a space.

    ?line ok = file:make_dir(Spacedir),
    ?line {ok, Bin} = file:read_file(OrigEcho),
    ?line ok = file:write_file(Echo0, Bin),
    ?line Echo = filename:nativename(Echo0),
    ?line ok = file:change_mode(Echo, 8#777),	% Make it executable on Unix.

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
    ?line comp("", os:cmd(Quote ++ Echo ++ Quote)),
    ?line comp("a::b::c", os:cmd(Quote ++ Echo ++ Quote ++ " a b c")),
    ?t:sleep(5),
    ?line [] = receive_all(),
    ok.

bad_command(doc) ->
    "Check that a bad command doesn't crasch the server or the emulator (it used to).";
bad_command(suite) -> [];
bad_command(Config) when is_list(Config) ->
    ?line catch os:cmd([a|b]),
    ?line catch os:cmd({bad, thing}),

    %% This should at least not crash (on Unix it typically returns
    %% a message from the shell).
    ?line os:cmd("xxxxx"),

    ok.

find_executable(suite) -> [];
find_executable(doc) -> [];
find_executable(Config) when is_list(Config) ->
    case os:type() of
	{win32, _} ->
	    ?line DataDir = filename:join(?config(data_dir, Config), "win32"),
	    ?line ok = file:set_cwd(filename:join([DataDir, "current"])),
	    ?line Bin = filename:join(DataDir, "bin"),
	    ?line Abin = filename:join(DataDir, "abin"),
	    ?line UsrBin = filename:join([DataDir, "usr", "bin"]),
	    ?line {ok, Current} = file:get_cwd(),

	    ?line Path = lists:concat([Bin, ";", Abin, ";", UsrBin]),
	    ?line io:format("Path = ~s", [Path]),

	    %% Search for programs in Bin (second element in PATH).
	    ?line find_exe(Abin, "my_ar", ".exe", Path),
	    ?line find_exe(Abin, "my_ascii", ".com", Path),
	    ?line find_exe(Abin, "my_adb", ".bat", Path),
	    %% OTP-3626 find names of executables given with extension
	    ?line find_exe(Abin, "my_ar.exe", "", Path),
	    ?line find_exe(Abin, "my_ascii.com", "", Path),
	    ?line find_exe(Abin, "my_adb.bat", "", Path),
	    ?line find_exe(Abin, "my_ar.EXE", "", Path),
	    ?line find_exe(Abin, "my_ascii.COM", "", Path),
	    ?line find_exe(Abin, "MY_ADB.BAT", "", Path),

	    %% Search for programs in Abin (second element in PATH).
	    ?line find_exe(Abin, "my_ar", ".exe", Path),
	    ?line find_exe(Abin, "my_ascii", ".com", Path),
	    ?line find_exe(Abin, "my_adb", ".bat", Path),

	    %% Search for programs in the current working directory.
	    ?line find_exe(Current, "my_program", ".exe", Path),
	    ?line find_exe(Current, "my_command", ".com", Path),
	    ?line find_exe(Current, "my_batch", ".bat", Path),
	    ok;
	{unix, _}  ->
	    DataDir = ?config(data_dir, Config),

	    %% Smoke test.
	    case lib:progname() of
		erl ->
		    ?line ErlPath = os:find_executable("erl"),
		    ?line true = is_list(ErlPath),
		    ?line true = filelib:is_regular(ErlPath);
		_ ->
		    %% Don't bother -- the progname could include options.
		    ok
	    end,

	    %% Never return a directory name.
	    ?line false = os:find_executable("unix", [DataDir]),
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
		    test_server:fail()
	    end;
	Other ->
	    io:format("Expected ~p; got ~p", [Expected, Other]),
	    test_server:fail()
    end.

unix_comment_in_command(doc) ->
    "OTP-1805: Test that os:cmd(\"ls #\") works correctly (used to hang).";
unix_comment_in_command(suite) -> [];
unix_comment_in_command(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(20)),
    ?line Priv = ?config(priv_dir, Config),
    ?line ok = file:set_cwd(Priv),
    ?line _ = os:cmd("ls #"),			% Any result is ok.
    ?t:sleep(5),
    ?line [] = receive_all(),
    ?line test_server:timetrap_cancel(Dog),
    ok.

deep_list_command(doc) ->
    "Check that a deep list in command works equally on unix and on windows.";
deep_list_command(suite) -> [];
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


-define(EVIL_PROCS, 100).
-define(EVIL_LOOPS, 100).
-define(PORT_CREATOR, os_cmd_port_creator).
evil(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:minutes(5)),
    Parent = self(),
    Ps = lists:map(fun (N) ->
			   spawn_link(fun () ->
					      evil_loop(Parent, ?EVIL_LOOPS,N)
				      end)
		   end, lists:seq(1, ?EVIL_PROCS)),
    Devil = spawn_link(fun () -> devil(hd(Ps), hd(lists:reverse(Ps))) end),
    lists:foreach(fun (P) -> receive {P, done} -> ok end end, Ps),
    unlink(Devil),
    exit(Devil, kill),
    test_server:timetrap_cancel(Dog),
    ok.

devil(P1, P2) ->
    erlang:display({?PORT_CREATOR, whereis(?PORT_CREATOR)}),
    (catch ?PORT_CREATOR ! lists:seq(1,1000000)),
    (catch ?PORT_CREATOR ! lists:seq(1,666)),
    (catch ?PORT_CREATOR ! grrrrrrrrrrrrrrrr),
    (catch ?PORT_CREATOR ! {'EXIT', P1, buhuuu}),
    (catch ?PORT_CREATOR ! {'EXIT', hd(erlang:ports()), buhuuu}),
    (catch ?PORT_CREATOR ! {'EXIT', P2, arggggggg}),
    receive after 500 -> ok end,
    (catch exit(whereis(?PORT_CREATOR), kill)),
    (catch ?PORT_CREATOR ! ">8|"),
    receive after 500 -> ok end,
    (catch exit(whereis(?PORT_CREATOR), diiiiiiiiiiiiiiiiiiiie)),
    receive after 100 -> ok end,
    devil(P1, P2).

evil_loop(Parent, Loops, N) ->
    Res = integer_to_list(N),
    evil_loop(Parent, Loops, Res, "echo " ++ Res).

evil_loop(Parent, 0, _Res, _Cmd) ->
    Parent ! {self(), done};
evil_loop(Parent, Loops, Res, Cmd) ->
    comp(Res, os:cmd(Cmd)),
    evil_loop(Parent, Loops-1, Res, Cmd).

comp(Expected, Got) ->
    case strip_nl(Got) of
	Expected ->
	    ok;
	Other ->
	    ok = io:format("Expected: ~ts\n", [Expected]),
	    ok = io:format("Got:      ~ts\n", [Other]),
	    test_server:fail()
    end.

%% Like lib:nonl/1, but strips \r as well as \n.

strip_nl([$\r, $\n]) -> [];
strip_nl([$\n])      -> [];
strip_nl([H|T])      -> [H|strip_nl(T)];
strip_nl([])         -> [].

receive_all() ->
    receive
	X -> [X|receive_all()]
    after 0 -> []
    end.
