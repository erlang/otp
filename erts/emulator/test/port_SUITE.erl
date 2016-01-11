%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
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

-module(port_SUITE).

%%%
%%% Author: Bjorn Gustavsson; iter_max_ports contributed by Peter Hogfeldt.
%%%

%%
%% There are a lot of things to test with open_port(Name, Settings).
%%
%%   Name can be
%%
%%       {spawn, Command}
%%         which according to The Book and the manual page starts an
%%         external program. That is not true. It might very well be
%%         a linked-in program (the notion of 'linked-in driver' is
%%         silly, since any driver is 'linked-in').
%%	  [Spawn of external program is tested.]
%%
%%       Atom
%%         Read all contents of Atom, or write to it.
%%
%%       {fd, In, Out}
%%       Open file descriptors In and Out. [Not tested]
%%
%%   PortSettings can be
%%
%%       {packet, N}
%%         N is 1, 2 or 4.
%%
%%       stream (default)
%%         Without packet length.
%%
%%       use_stdio (default for spawned ports)
%%         The spawned process use file descriptors 0 and 1 for I/O.
%%
%%       nouse_stdio					[Not tested]
%%         Use filedescriptors 3 and 4.  This option is probably only
%%	   meaningful on Unix.
%%
%%       in (default for Atom)
%%         Input only (from Erlang's point of view).
%%
%%       out
%%         Output only (from Erlang's point of view).
%%
%%       binary
%%         The port is a binary port, i.e. messages received and sent
%%         to a port are binaries.
%%
%%       eof
%%         Port is not closed on eof and will not send an exit signal,
%%         instead it will send a {Port, eof} to the controlling process
%%         (output can still be sent to the port (??)).
%%


-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 init_per_suite/1, end_per_suite/1,
	 stream_small/1, stream_big/1,
	 basic_ping/1, slow_writes/1, bad_packet/1, bad_port_messages/1,
	 mul_basic/1, mul_slow_writes/1,
	 dying_port/1, port_program_with_path/1,
	 open_input_file_port/1, open_output_file_port/1,
         count_fds/1,
	 iter_max_ports/1, eof/1, input_only/1, output_only/1,
	 name1/1,
	 t_binary/1, parallell/1, t_exit/1,
	 env/1, huge_env/1, bad_env/1, cd/1, exit_status/1,
	 tps_16_bytes/1, tps_1K/1, line/1, stderr_to_stdout/1,
	 otp_3906/1, otp_4389/1, win_massive/1, win_massive_client/1,
	 mix_up_ports/1, otp_5112/1, otp_5119/1, otp_6224/1,
	 exit_status_multi_scheduling_block/1, ports/1,
	 spawn_driver/1, spawn_executable/1, close_deaf_port/1,
	 port_setget_data/1,
	 unregister_name/1, parallelism_option/1]).

-export([do_iter_max_ports/2]).

%% Internal exports.
-export([tps/3]).
-export([otp_3906_forker/5, otp_3906_start_forker_starter/4]).
-export([env_slave_main/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [otp_6224, {group, stream}, basic_ping, slow_writes,
     bad_packet, bad_port_messages, {group, options},
     {group, multiple_packets}, parallell, dying_port,
     port_program_with_path, open_input_file_port,
     open_output_file_port, name1, env, huge_env, bad_env, cd,
     exit_status, iter_max_ports, count_fds, t_exit, {group, tps}, line,
     stderr_to_stdout, otp_3906, otp_4389, win_massive,
     mix_up_ports, otp_5112, otp_5119,
     exit_status_multi_scheduling_block, ports, spawn_driver,
     spawn_executable, close_deaf_port, unregister_name,
     port_setget_data,
     parallelism_option].

groups() -> 
    [{stream, [], [stream_small, stream_big]},
     {options, [], [t_binary, eof, input_only, output_only]},
     {multiple_packets, [], [mul_basic, mul_slow_writes]},
     {tps, [], [tps_16_bytes, tps_1K]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


-define(DEFAULT_TIMEOUT, ?t:minutes(5)).

init_per_testcase(Case, Config) ->
    [{testcase, Case} |Config].

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    ignore_cores:init(Config).

end_per_suite(Config) when is_list(Config) ->
    ignore_cores:fini(Config).


-define(WIN_MASSIVE_PORT, 50000).

%% Tests that you can open a massive amount of ports (sockets)
%% on a Windows machine given the correct environment.
win_massive(Config) when is_list(Config) ->
    case os:type() of
	{win32,_} ->
	    do_win_massive();
	_ ->
	    {skip,"Only on Windows."}
    end.

do_win_massive() ->
    Dog = test_server:timetrap(test_server:seconds(360)),
    SuiteDir = filename:dirname(code:which(?MODULE)),
    Ports = " +Q 8192",
    {ok, Node} = 
	test_server:start_node(win_massive,
			       slave,
			       [{args, " -pa " ++ SuiteDir ++ Ports}]),
    ok = rpc:call(Node,?MODULE,win_massive_client,[3000]),
    test_server:stop_node(Node),
    test_server:timetrap_cancel(Dog),
    ok.
    
win_massive_client(N) ->
    {ok,P}=gen_tcp:listen(?WIN_MASSIVE_PORT,[{reuseaddr,true}]), 
    L = win_massive_loop(P,N),
    Len = length(L),
    lists:foreach(fun(E) ->
			  gen_tcp:close(E)
		  end,
		  L),
    case Len div 2 of
	N ->
	    ok;
	_Else ->
	    {too_few, Len}
    end.

win_massive_loop(_,0) ->
    [];
win_massive_loop(P,N) ->
    case (catch gen_tcp:connect("localhost",?WIN_MASSIVE_PORT,[])) of
	{ok,A} ->
	    case (catch gen_tcp:accept(P)) of
		{ok,B} ->
		    %erlang:display(N),
		    [A,B|win_massive_loop(P,N-1)];
		_Else ->
		    [A]
	    end;
	_Else0 ->
	    []
    end.
	    
    



%% Test that we can send a stream of bytes and get it back.
%% We will send only a small amount of data, to avoid deadlock.

stream_small(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    stream_ping(Config, 512, "", []),
    stream_ping(Config, 1777, "", []),
    stream_ping(Config, 1777, "-s512", []),
    test_server:timetrap_cancel(Dog),
    ok.

%% Send big amounts of data (much bigger than the buffer size in port test).
%% This will deadlock the emulator if the spawn driver haven't proper
%% non-blocking reads and writes.

stream_big(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(180)),
    stream_ping(Config, 43755, "", []),
    stream_ping(Config, 100000, "", []),
    stream_ping(Config, 77777, " -s40000", []),
    test_server:timetrap_cancel(Dog),
    ok.

%% Sends packet with header size of 1, 2, and 4, with packets of various
%% sizes.

basic_ping(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(120)),
    ping(Config, sizes(1), 1, "", []),
    ping(Config, sizes(2), 2, "", []),
    ping(Config, sizes(4), 4, "", []),
    test_server:timetrap_cancel(Dog),
    ok.

%% Let the port program insert delays between characters sent back to
%% Erlang, to test that the Erlang emulator can handle a packet coming in
%% small chunks rather than all at once.

slow_writes(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(20)),
    ping(Config, [8], 4, "-s1", []),
    ping(Config, [10], 2, "-s2", []),
    test_server:timetrap_cancel(Dog),
    ok.

bad_packet(doc) ->
    ["Test that we get {'EXIT', Port, einval} if we try to send a bigger "
     "packet than the packet header allows."];
bad_packet(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    PortTest = port_test(Config),
    process_flag(trap_exit, true),

    bad_packet(PortTest, 1, 256),
    bad_packet(PortTest, 1, 257),
    bad_packet(PortTest, 2, 65536),
    bad_packet(PortTest, 2, 65537),

    test_server:timetrap_cancel(Dog),
    ok.

bad_packet(PortTest, HeaderSize, PacketSize) ->
    P = open_port({spawn, PortTest}, [{packet, HeaderSize}]),
    P ! {self(), {command, make_zero_packet(PacketSize)}},
    receive
	{'EXIT', P, einval} -> ok;
	Other -> test_server:fail({unexpected_message, Other})
    end.

make_zero_packet(0) -> [];
make_zero_packet(N) when N rem 2 == 0 ->
    P = make_zero_packet(N div 2),
    [P|P];
make_zero_packet(N) ->
    P = make_zero_packet(N div 2),
    [0, P|P].

%% Test sending bad messages to a port.
bad_port_messages(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    PortTest = port_test(Config),
    process_flag(trap_exit, true),

    bad_message(PortTest, {a,b}),
    bad_message(PortTest, {a}),
    bad_message(PortTest, {self(),{command,bad_command}}),
    bad_message(PortTest, {self(),{connect,no_pid}}),

    test_server:timetrap_cancel(Dog),
    ok.

bad_message(PortTest, Message) ->    
    P = open_port({spawn,PortTest}, []),
    P ! Message,
    receive
	{'EXIT',P,badsig} -> ok;
	Other -> test_server:fail({unexpected_message, Other})
    end.

%% Tests various options (stream and {packet, Number} are implicitly
%% tested in other test cases).


%% Tests the 'binary' option for a port.

t_binary(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(300)),

    %% Packet mode.
    ping(Config, sizes(1), 1, "", [binary]),
    ping(Config, sizes(2), 2, "", [binary]),
    ping(Config, sizes(4), 4, "", [binary]),

    %% Stream mode.
    stream_ping(Config, 435, "", [binary]),
    stream_ping(Config, 43755, "", [binary]),
    stream_ping(Config, 100000, "", [binary]),

    test_server:timetrap_cancel(Dog),
    ok.

name1(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    PortTest = port_test(Config),
    Command = lists:concat([PortTest, " "]),
    P = open_port({spawn, Command}, []),
    register(myport, P),
    P = whereis(myport),
    Text = "hej",
    myport ! {self(), {command, Text}},
    receive
        {P, {data, Text}} ->
            ok
    end,
    myport ! {self(), close},
    receive
        {P, closed} -> ok
    end,
    undefined = whereis(myport),
    test_server:timetrap_cancel(Dog),
    ok.

%% Test that the 'eof' option works.

eof(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    PortTest = port_test(Config),
    Command = lists:concat([PortTest, " -h0 -q"]),
    P = open_port({spawn, Command}, [eof]),
    receive
        {P, eof} ->
            ok
    end,
    P ! {self(), close},
    receive
        {P, closed} -> ok
    end,
    test_server:timetrap_cancel(Dog),
    ok.

%% Tests that the 'in' option for a port works.

input_only(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(300)),
    expect_input(Config, [0, 1, 10, 13, 127, 128, 255], 1, "", [in]),
    expect_input(Config, [0, 1, 255, 2048], 2, "", [in]),
    expect_input(Config, [0, 1, 255, 2048], 4, "", [in]),
    expect_input(Config, [0, 1, 10, 13, 127, 128, 255],
                 1, "", [in, binary]),
    test_server:timetrap_cancel(Dog),
    ok.

%% Tests that the 'out' option for a port works.

output_only(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    Dir = ?config(priv_dir, Config),

    %% First we test that the port program gets the data
    Filename = filename:join(Dir, "output_only_stream"),
    Data = random_packet(35777, "echo"),
    output_and_verify(Config, ["-h0 -o", Filename], Data),
    Wait_time = 500,
    test_server:sleep(Wait_time),
    {ok, Written} = file:read_file(Filename),
    Data = binary_to_list(Written),

    %% Then we test that any writes to stdout from
    %% the port program is not sent to erlang
    output_and_verify(Config, ["-h0"], Data),

    test_server:timetrap_cancel(Dog),
    ok.

output_and_verify(Config, Options, Data) ->
    PortTest = port_test(Config),
    Command = lists:concat([PortTest, " " | Options]),
    Port = open_port({spawn, Command}, [out]),
    Port ! {self(), {command, Data}},
    Port ! {self(), close},
    receive
        {Port, closed} -> ok;
        Msg -> ct:fail({received_unexpected_message, Msg})
    end.

%% Test that receiving several packages written in the same
%% write operation works.


%% Basic test of receiving multiple packages, written in
%% one operation by the other end.
mul_basic(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(600)),
    expect_input(Config, [0, 1, 255, 10, 13], 1, "", []),
    expect_input(Config, [0, 10, 13, 1600, 32767, 65535], 2, "", []),
    expect_input(Config, [10, 70000], 4, "", []),
    test_server:timetrap_cancel(Dog),
    ok.

%% Test reading a buffer consisting of several packets, some
%% of which might be incomplete.  (The port program builds
%% a buffer with several packets, but writes it in chunks with
%% delays in between.)

mul_slow_writes(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(250)),
    expect_input(Config, [0, 20, 255, 10, 1], 1, "-s64", []),
    test_server:timetrap_cancel(Dog),
    ok.

%% Runs several port tests in parallell.  Each individual test
%% finishes in about 5 seconds.  Running in parallell, all tests
%% should also finish in about 5 seconds.

parallell(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(300)),
    Testers = [
	fun() -> stream_ping(Config, 1007, "-s100", []) end,
	fun() -> stream_ping(Config, 10007, "-s1000", []) end,
	fun() -> stream_ping(Config, 10007, "-s1000", []) end,

	fun() -> expect_input(Config, [21, 22, 23, 24, 25], 1,
		    "-s10", [in]) end,

	fun() -> ping(Config, [10], 1, "-d", []) end,
	fun() -> ping(Config, [20000], 2, "-d", []) end,
	fun() -> ping(Config, [101], 1, "-s10", []) end,
	fun() -> ping(Config, [1001], 2, "-s100", []) end,
	fun() -> ping(Config, [10001], 4, "-s1000", []) end,

	fun() -> ping(Config, [501, 501], 2, "-s100", []) end,
	fun() -> ping(Config, [11, 12, 13, 14, 11], 1, "-s5", []) end],
    process_flag(trap_exit, true),
    Pids = lists:map(fun fun_spawn/1, Testers),
    wait_for(Pids),
    test_server:timetrap_cancel(Dog),
    ok.

wait_for([]) ->
    ok;
wait_for(Pids) ->
    io:format("Waiting for ~p", [Pids]),
    receive
	{'EXIT', Pid, normal} ->
	    wait_for(lists:delete(Pid, Pids));
	Other ->
	    test_server:fail({bad_exit, Other})
    end.

%% Tests starting port programs that terminate by themselves.
%% This used to cause problems on Windows.

dying_port(suite) -> [];
dying_port(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(150)),
    process_flag(trap_exit, true),

    P1 = make_dying_port(Config),
    P2 = make_dying_port(Config),
    P3 = make_dying_port(Config),
    P4 = make_dying_port(Config),
    P5 = make_dying_port(Config),

    %% This should be big enough to be sure to block in the write.
    Garbage = random_packet(16384),

    P1 ! {self(), {command, Garbage}},
    P3 ! {self(), {command, Garbage}},
    P5 ! {self(), {command, Garbage}},

    wait_for_port_exit(P1),
    wait_for_port_exit(P2),
    wait_for_port_exit(P3),
    wait_for_port_exit(P4),
    wait_for_port_exit(P5),

    test_server:timetrap_cancel(Dog),
    ok.

wait_for_port_exit(Port) ->
    receive
	{'EXIT', Port, _} ->
	    ok
    end.

make_dying_port(Config) when is_list(Config) ->
    PortTest = port_test(Config),
    Command = lists:concat([PortTest, " -h0 -d -q"]),
    open_port({spawn, Command}, [stream]).

%% Tests that port program with complete path (but without any
%% .exe extension) can be started, even if there is a file with
%% the same name but without the extension in the same directory.
%% (In practice, the file with the same name could be a Unix
%% executable.)
%%
%% This used to failed on Windows (the .exe extension had to be
%% explicitly given).
%%
%% This testcase works on Unix, but is not very useful.

port_program_with_path(suite) -> [];
port_program_with_path(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    
    %% Create a copy of the port test program in a directory not
    %% included in PATH (i.e. in priv_dir), with the name 'my_port_test.exe'.
    %% Also, place a file named 'my_port_test' in the same directory.
    %% This used to confuse the CreateProcess() call in spawn driver.
    %% (On Unix, there will be a single file created, which will be
    %% a copy of the port program.)
    
    PortTest = os:find_executable("port_test", DataDir),
    io:format("os:find_executable(~p, ~p) returned ~p",
	      ["port_test", DataDir, PortTest]),
    {ok, PortTestPgm} = file:read_file(PortTest),
    NewName = filename:join(PrivDir, filename:basename(PortTest)),
    RedHerring = filename:rootname(NewName),
    ok = file:write_file(RedHerring, "I'm just here to confuse.\n"),
    ok = file:write_file(NewName, PortTestPgm),
    ok = file:write_file_info(NewName, #file_info{mode=8#111}),
    PgmWithPathAndNoExt = filename:rootname(NewName),
    
    %% Open the port using the path to the copied port test program,
    %% but without the .exe extension, and verified that it was started.
    %%
    %% If the bug is present the open_port call will fail with badarg.
    
    Command = lists:concat([PgmWithPathAndNoExt, " -h2"]),
    P = open_port({spawn, Command}, [{packet, 2}]),
    Message = "echo back to me",
    P ! {self(), {command, Message}},
    receive
        {P, {data, Message}} ->
            ok
    end,
    test_server:timetrap_cancel(Dog),
    ok.


%% Tests that files can be read using open_port(Filename, [in]).
%% This used to fail on Windows.
open_input_file_port(suite) -> [];
open_input_file_port(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    PrivDir = ?config(priv_dir, Config),
    
    %% Create a file with the file driver and read it back using
    %% open_port/2.

    MyFile1 = filename:join(PrivDir, "my_input_file"),
    FileData1 = "An input file",
    ok = file:write_file(MyFile1, FileData1),
    case open_port(MyFile1, [in]) of
	InputPort when is_port(InputPort) ->
	    receive
		{InputPort, {data, FileData1}} ->
		    ok
	    end
    end,
    test_server:timetrap_cancel(Dog),
    ok.

%% Tests that files can be written using open_port(Filename, [out]).
open_output_file_port(suite) -> [];
open_output_file_port(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    PrivDir = ?config(priv_dir, Config),

    %% Create a file with open_port/2 and read it back with
    %% the file driver.

    MyFile2 = filename:join(PrivDir, "my_output_file"),
    FileData2_0 = "A file created ",
    FileData2_1 = "with open_port/2.\n",
    FileData2 = FileData2_0 ++ FileData2_1,
    OutputPort = open_port(MyFile2, [out]),
    OutputPort ! {self(), {command, FileData2_0}},
    OutputPort ! {self(), {command, FileData2_1}},
    OutputPort ! {self(), close},
    {ok, Bin} = file:read_file(MyFile2),
    FileData2 = binary_to_list(Bin),

    test_server:timetrap_cancel(Dog),
    ok.

%% Tests that all appropriate fd's have been closed in the port program
count_fds(suite) -> [];
count_fds(Config) when is_list(Config) ->
    case os:type() of
        {unix, _} ->
            PrivDir = proplists:get_value(priv_dir, Config),
            Filename = filename:join(PrivDir, "my_fd_counter"),

            RunTest = fun(PortOpts) ->
                              PortTest = port_test(Config),
                              Command = lists:concat([PortTest, " -n -f -o", Filename]),
                              Port = open_port({spawn, Command}, PortOpts),
                              Port ! {self(), close},
                              receive
                                  {Port, closed} -> ok
                              end,
                              test_server:sleep(500),
                              {ok, Written} = file:read_file(Filename),
                              Written
                      end,
                    <<4:32/native>> = RunTest([out, nouse_stdio]),
                    <<4:32/native>> = RunTest([in, nouse_stdio]),
                    <<5:32/native>> = RunTest([in, out, nouse_stdio]),
            <<3:32/native>> = RunTest([out, use_stdio]),
            <<3:32/native>> = RunTest([in, use_stdio]),
            <<3:32/native>> = RunTest([in, out, use_stdio]),
            <<3:32/native>> = RunTest([in, out, use_stdio, stderr_to_stdout]),
            <<3:32/native>> = RunTest([out, use_stdio, stderr_to_stdout]);
        _ ->
            {skip, "Skipped on windows"}
    end.

%%
%% Open as many ports as possible. Do this several times and check
%% that we get the same number of ports every time.
%%

iter_max_ports(suite) -> [];
iter_max_ports(Config) when is_list(Config) ->
    %% The child_setup program might dump core if we get out of memory.
    %% This is hard to do anything about and is harmless. We run this test
    %% in a working directory with an ignore_core_files file which will make
    %% the search for core files ignore cores generated by this test.
    %%
    Config2 = ignore_cores:setup(?MODULE, iter_max_ports, Config, true),
    try
	iter_max_ports_test(Config2)
    after
	ignore_cores:restore(Config2)
    end.
    
    
iter_max_ports_test(Config) ->
    Dog = test_server:timetrap(test_server:minutes(30)),
    PortTest = port_test(Config),
    Command = lists:concat([PortTest, " -h0 -q"]),
    Iters = case os:type() of
		      {win32,_} -> 4;
		      _ -> 10
		  end,
    %% Run on a different node in order to limit the effect if this test fails.
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(test_iter_max_socks,slave,
				       [{args,"+Q 2048 -pa " ++ Dir}]),
    L = rpc:call(Node,?MODULE,do_iter_max_ports,[Iters, Command]),
    test_server:stop_node(Node),

    io:format("Result: ~p",[L]),
    all_equal(L),
    all_equal(L),
    test_server:timetrap_cancel(Dog),
    {comment, "Max ports: " ++ integer_to_list(hd(L))}.

do_iter_max_ports(N, Command) when N > 0 ->
    [max_ports(Command)| do_iter_max_ports(N-1, Command)];
do_iter_max_ports(_, _) ->
    [].

all_equal([E,E|T]) ->
    all_equal([E|T]);
all_equal([_]) -> ok;
all_equal([]) -> ok.

max_ports(Command) ->
    test_server:sleep(500),
    Ps = open_ports({spawn, Command}, [eof]),
    N = length(Ps),
    close_ports(Ps),
    io:format("Got ~p ports\n",[N]),
    N.

close_ports([P|Ps]) ->
    P ! {self(), close},
    receive
	{P,closed} ->
	    ok
    end,
    close_ports(Ps);
close_ports([]) ->
    ok.

open_ports(Name, Settings) ->
    case os:type() of
        {unix, freebsd} ->
            %% FreeBsd has issues with sendmsg/recvmsg in fork
            %% implementation and we therefor have to spawn
            %% slower to make sure that we always hit the same
            %% make roof.
            test_server:sleep(10);
        _ ->
            test_server:sleep(5)
    end,
    case catch open_port(Name, Settings) of
	P when is_port(P) ->
	    [P| open_ports(Name, Settings)];
	{'EXIT', {Code, _}} ->
	    case Code of
		enfile ->
		    [];
		emfile ->
		    [];
		system_limit ->
		    [];
		enomem ->
		    [];
		Other ->
		    test_server:fail({open_ports, Other})
	    end;
	Other ->
	    test_server:fail({open_ports, Other})
    end.

%% Tests that exit(Port, Term) works (has been known to crash the emulator).

t_exit(suite) -> [];
t_exit(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Pid = fun_spawn(fun suicide_port/1, [Config]),
    receive
	{'EXIT', Pid, die} ->
	    ok;
	Other ->
	    test_server:fail({bad_message, Other})
    end.

suicide_port(Config) when is_list(Config) ->
    Port = port_expect(Config, [], 0, "", []),
    exit(Port, die),
    receive after infinity -> ok end.


tps_16_bytes(doc) -> "";
tps_16_bytes(suite) -> [];
tps_16_bytes(Config) when is_list(Config) ->
    tps(16, Config).

tps_1K(doc) -> "";
tps_1K(suite) -> [];
tps_1K(Config) when is_list(Config) ->
    tps(1024, Config).

tps(Size, Config) ->
    Dog = test_server:timetrap(test_server:seconds(300)),
    PortTest = port_test(Config),
    Packet = list_to_binary(random_packet(Size, "e")),
    Port = open_port({spawn, PortTest}, [binary, {packet, 2}]),
    Transactions = 10000,
    {Elapsed, ok} = test_server:timecall(?MODULE, tps,
					       [Port, Packet, Transactions]),
    test_server:timetrap_cancel(Dog),
    {comment, integer_to_list(trunc(Transactions/Elapsed+0.5)) ++ " transactions/s"}.

tps(_Port, _Packet, 0) -> ok;
tps(Port, Packet, N) ->
    port_command(Port, Packet),
    receive
	{Port, {data, Packet}} ->
	    tps(Port, Packet, N-1);
	Other ->
	    test_server:fail({bad_message, Other})
    end.

%% Line I/O test
line(Config) when is_list(Config) ->
    Siz = 110,
    Dog = test_server:timetrap(test_server:seconds(300)),
    Packet1 = random_packet(Siz),
    Packet2 = random_packet(Siz div 2),
    %% Test that packets are split into lines
    port_expect(Config,[{lists:append([Packet1, io_lib:nl(), Packet2,
					     io_lib:nl()]),
			       [{eol, Packet1}, {eol, Packet2}]}],
		      0, "", [{line,Siz}]),
    %% Test the same for binaries
    port_expect(Config,[{lists:append([Packet1, io_lib:nl(), Packet2,
					     io_lib:nl()]),
			       [{eol, Packet1}, {eol, Packet2}]}],
		      0, "", [{line,Siz},binary]),
    %% Test that too long lines get split
    port_expect(Config,[{lists:append([Packet1, io_lib:nl(), Packet1,
					     Packet2, io_lib:nl()]),
			       [{eol, Packet1}, {noeol, Packet1},
				{eol, Packet2}]}], 0, "", [{line,Siz}]),
    %% Test that last output from closing port program gets received.
    L1 = lists:append([Packet1, io_lib:nl(), Packet2]),
    S1 = lists:flatten(io_lib:format("-l~w", [length(L1)])),
    io:format("S1 = ~w, L1 = ~w~n", [S1,L1]),
    port_expect(Config,[{L1,
			       [{eol, Packet1}, {noeol, Packet2}, eof]}], 0, 
		      S1, [{line,Siz},eof]),
    %% Test that lonely <CR> Don't get treated as newlines
    port_expect(Config,[{lists:append([Packet1, [13], Packet2,
					     io_lib:nl()]),
			       [{noeol, Packet1}, {eol, [13 |Packet2]}]}],
		      0, "", [{line,Siz}]),
    %% Test that packets get built up to lines (delayed output from
    %% port program)
    port_expect(Config,[{Packet2,[]},
			      {lists:append([Packet2, io_lib:nl(),
					     Packet1, io_lib:nl()]),
			       [{eol, lists:append(Packet2, Packet2)},
				{eol, Packet1}]}], 0, "-d", [{line,Siz}]),
    %% Test that we get badarg if trying both packet and line
    bad_argument(Config, [{packet, 5}, {line, 5}]),
    test_server:timetrap_cancel(Dog),
    ok.

%%% Redirection of stderr test
stderr_to_stdout(suite) ->
    [];
stderr_to_stdout(doc) ->
    "Test that redirection of standard error to standard output works.";
stderr_to_stdout(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    %% See that it works
    Packet = random_packet(10),
    port_expect(Config,[{Packet,[Packet]}], 0, "-e -l10",
		      [stderr_to_stdout]),
    %% stream_ping(Config, 10, "-e", [stderr_to_stdout]),
    %% See that it doesn't always happen (will generate garbage on stderr)
    port_expect(Config,[{Packet,[eof]}], 0, "-e -l10", [line,eof]),
    test_server:timetrap_cancel(Dog),
    ok.


bad_argument(Config, ArgList) ->
    PortTest = port_test(Config),
    case catch open_port({spawn, PortTest}, ArgList) of
	{'EXIT', {badarg, _}} ->
	    ok
    end.
    

%% 'env' option
%% (Can perhaps be made smaller by calling the other utility functions
%% in this module.)
env(suite) ->
    [];
env(doc) ->
    ["Test that the 'env' option works"];
env(Config)  when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    Priv = ?config(priv_dir, Config),
    Temp = filename:join(Priv, "env_fun.bin"),

    PluppVal = "dirty monkey",
    env_slave(Temp, [{"plupp",PluppVal}]),

    Long = "LongAndBoringEnvName",
    os:putenv(Long, "nisse"),

    env_slave(Temp, [{"plupp",PluppVal},
	    {"DIR_PLUPP","###glurfrik"}],
	fun() ->
		PluppVal = os:getenv("plupp"),
		"###glurfrik" = os:getenv("DIR_PLUPP"),
		"nisse" = os:getenv(Long)
	end),


    env_slave(Temp, [{"must_define_something","some_value"},
	    {"certainly_not_existing",false},
	    {"ends_with_equal", "value="},
	    {Long,false},
	    {"glurf","a glorfy string"}]),

    %% A lot of non existing variables (mingled with existing)
    NotExistingList = [{lists:flatten(io_lib:format("V~p_not_existing",[X])),false} 
                        ||  X <- lists:seq(1,150)],
    ExistingList = [{lists:flatten(io_lib:format("V~p_existing",[X])),"a_value"} 
                        ||  X <- lists:seq(1,150)],
    env_slave(Temp, lists:sort(ExistingList ++ NotExistingList)),

    test_server:timetrap_cancel(Dog),
    ok.

env_slave(File, Env) ->
    F = fun() ->
		lists:foreach(fun({Name,Val}) ->
				      Val = os:getenv(Name)
			      end, Env)
	end,
    env_slave(File, Env, F).

env_slave(File, Env, Body) ->
    file:write_file(File, term_to_binary(Body)),
    Program = atom_to_list(lib:progname()),
    Dir = filename:dirname(code:which(?MODULE)),
    Cmd = Program ++ " -pz " ++ Dir ++
	" -noinput -run " ++ ?MODULE_STRING ++ " env_slave_main " ++
	File ++ " -run erlang halt",
    Port = open_port({spawn, Cmd}, [{env,Env},{line,256}]),
    receive
	{Port,{data,{eol,"ok"}}} ->
	    ok;
	{Port,{data,{eol,Error}}} ->
	    io:format("~p\n", [Error]),
	    test_server:fail();
	Other ->
	    test_server:fail(Other)
    end.

env_slave_main([File]) ->
    {ok,Body0} = file:read_file(File),
    Body = binary_to_term(Body0),
    case Body() of
	{'EXIT',Reason} ->
	    io:format("Error: ~p\n", [Reason]);
	_ ->
	    io:format("ok\n")
    end,
    init:stop().


%% 'env' option
%%   Test bad environments.
bad_env(Config) when is_list(Config) ->
    try_bad_env([abbb]),
    try_bad_env([{"key","value"}|{"another","value"}]),
    try_bad_env([{"key","value","value2"}]),
    try_bad_env([{"key",[a,b,c]}]),
    try_bad_env([{"key",value}]),
    try_bad_env({a,tuple}),
    try_bad_env(42),
    try_bad_env([a|b]),
    try_bad_env(self()),
    ok.

try_bad_env(Env) ->
    try open_port({spawn,"ls"}, [{env,Env}])
    catch
	error:badarg -> ok
    end.

%% Test that we can handle a very very large environment gracefully.
huge_env(Config) when is_list(Config) ->
    Vars = case os:type() of
                {win32,_} -> 500;
                _ ->
                   %% We create a huge environment,
                   %% 20000 variables is about 25MB
                   %% which seems to be the limit on Linux.
                   20000
            end,
    Env = [{[$a + I div (25*25*25*25) rem 25,
              $a + I div (25*25*25) rem 25,
              $a + I div (25*25) rem 25,
              $a+I div 25 rem 25, $a+I rem 25],
            lists:duplicate(100,$a+I rem 25)}
           || I <- lists:seq(1,Vars)],
    try erlang:open_port({spawn,"ls"},[exit_status, {env, Env}]) of
        P ->
            receive
                {P, {exit_status,N}} = M ->
                    %% We test that the exit status is an integer, this means
                    %% that the child program has started. If we get an atom
                    %% something went wrong in the driver which is not ok.
                    ct:log("Got ~p",[M]),
                    true = is_integer(N)
            end
    catch E:R ->
            %% Have to catch the error here, as printing the stackdump
            %% in the ct log is way to heavy for some test machines.
            ct:fail("Open port failed ~p:~p",[E,R])
    end.



%% 'cd' option
%% (Can perhaps be made smaller by calling the other utility functions
%% in this module.)
cd(suite) ->
    [];
cd(doc) ->
    ["Test that the 'cd' option works"];
cd(Config)  when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),

    Program = atom_to_list(lib:progname()),
    DataDir = ?config(data_dir, Config),
    TestDir = filename:join(DataDir, "dir"),
    Cmd = Program ++ " -pz " ++ DataDir ++
        " -noshell -s port_test pwd -s erlang halt",
    _ = open_port({spawn, Cmd},
	[{cd, TestDir},
	    {line, 256}]),
    receive
	{_, {data, {eol, String}}} ->
	    case filename_equal(String, TestDir) of
		true ->
		    ok;
		false ->
		    test_server:fail({cd, String})
	    end;
	Other2 ->
	    test_server:fail({env, Other2})
    end,
    _ = open_port({spawn, Cmd},
		  [{cd, unicode:characters_to_binary(TestDir)},
		   {line, 256}]),
    receive
	{_, {data, {eol, String2}}} ->
	    case filename_equal(String2, TestDir) of
		true ->
		    ok;
		false ->
		    test_server:fail({cd, String2})
	    end;
	Other3 ->
	    test_server:fail({env, Other3})
    end,

    test_server:timetrap_cancel(Dog),
    ok.

filename_equal(A, B) ->
    case os:type() of
	{win32, _} ->
	    win_filename_equal(A, B);
	_ ->
	    A == B
    end.

win_filename_equal([], []) ->
    true;
win_filename_equal([], _) ->
    false;
win_filename_equal(_, []) ->
    false;
win_filename_equal([C1 | Rest1], [C2 | Rest2]) ->
    case tolower(C1) == tolower(C2) of
	true ->
	    win_filename_equal(Rest1, Rest2);
	false ->
	    false
    end.

tolower(C) when C >= $A, C =< $Z ->
    C + 32;
tolower(C) ->
    C.

otp_3906(suite) ->
    [];
otp_3906(doc) ->
    ["Tests that child process deaths are managed correctly when there are "
     " a large amount of concurrently dying children. See ticket OTP-3906."];
otp_3906(Config)  when is_list(Config) ->
    case os:type() of
	{unix, OSName} ->
	    otp_3906(Config, OSName);
	_ ->
	    {skipped, "Only run on Unix systems"}
    end.

-define(OTP_3906_CHILDREN,     1000).
-define(OTP_3906_EXIT_STATUS,  17).
-define(OTP_3906_PROGNAME,     "otp_3906").
-define(OTP_3906_TICK_TIMEOUT, 5000).
-define(OTP_3906_OSP_P_ERLP,   10).
-define(OTP_3906_MAX_CONC_OSP, 50).

otp_3906(Config, OSName) ->
    DataDir = filename:dirname(proplists:get_value(data_dir,Config)),
    {ok, Variables} = file:consult(
			      filename:join([DataDir,"..","..",
					     "test_server","variables"])),
    case lists:keysearch('CC', 1, Variables) of
	{value,{'CC', CC}} ->
	    SuiteDir = filename:dirname(code:which(?MODULE)),
	    PrivDir = ?config(priv_dir, Config),
	    Prog = otp_3906_make_prog(CC, PrivDir),
	    {ok, Node} = test_server:start_node(otp_3906,
						slave,
						[{args, " -pa " ++ SuiteDir},
						 {linked, false}]),
	    OP = process_flag(priority, max),
	    OTE = process_flag(trap_exit, true),
	    FS = spawn_link(Node,
			    ?MODULE,
			    otp_3906_start_forker_starter,
			    [?OTP_3906_CHILDREN, [], self(), Prog]),
	    Result = receive
			 {'EXIT', _ForkerStarter, Reason} ->
			     {failed, Reason};
			 {emulator_pid, EmPid} ->
			     case otp_3906_wait_result(FS, 0, 0) of
				 {succeded,
				  ?OTP_3906_CHILDREN,
				  ?OTP_3906_CHILDREN} ->
				     succeded;
				 {succeded, Forked, Exited} ->
				     otp_3906_list_defunct(EmPid, OSName),
				     {failed,
				      {mismatch,
				       {forked, Forked},
				       {exited, Exited}}};
				 Res ->
				     otp_3906_list_defunct(EmPid, OSName),
				     Res
			     end
		     end,
	    process_flag(trap_exit, OTE),
	    process_flag(priority, OP),
	    test_server:stop_node(Node),
	    case Result of
		succeded ->
		    ok;
		_ ->
		    test_server:fail(Result)
	    end;
	_ ->
	    {skipped, "No C compiler found"}
    end.

otp_3906_list_defunct(EmPid, OSName) ->
    % Guess ps switches to use and what to grep for (could be improved)
    {Switches, Zombie} = case OSName of
			     BSD when BSD == darwin;
				      BSD == openbsd;
				      BSD == netbsd;
				      BSD == freebsd ->
				 {"-ajx", "Z"};
			     _ ->
				 {"-ef", "[dD]efunct"}
			 end,
    test_server:format("Emulator pid: ~s~n"
		       "Listing of zombie processes:~n"
		       "~s~n",
		       [EmPid,
			otp_3906_htmlize(os:cmd("ps "
						++ Switches
						++ " | grep "
						++ Zombie))]).

otp_3906_htmlize([]) ->
    [];
otp_3906_htmlize([C | Cs]) ->
    case [C] of
	"<" -> "&lt;" ++ otp_3906_htmlize(Cs);
	">" -> "&gt;" ++ otp_3906_htmlize(Cs);
	_ ->   [C | otp_3906_htmlize(Cs)]
    end.

otp_3906_make_prog(CC, PrivDir) ->
    SrcFileName = filename:join(PrivDir, ?OTP_3906_PROGNAME ++ ".c"),
    TrgtFileName = filename:join(PrivDir, ?OTP_3906_PROGNAME),
    {ok, SrcFile} = file:open(SrcFileName, write),
    io:format(SrcFile,
	      "int           ~n"
	      "main(void)    ~n"
	      "{             ~n"
	      "   return ~p; ~n"
	      "}             ~n",
	      [?OTP_3906_EXIT_STATUS]),
    file:close(SrcFile),
    os:cmd(CC ++ " " ++ SrcFileName ++ " -o " ++ TrgtFileName),
    TrgtFileName.


otp_3906_wait_result(ForkerStarter, F, E) ->
    receive
	{'EXIT', ForkerStarter, Reason} ->
	    {failed, {Reason, {forked, F}, {exited, E}}};
	forked ->
	    otp_3906_wait_result(ForkerStarter, F+1, E);
	exited ->
	    otp_3906_wait_result(ForkerStarter, F, E+1);
	tick ->
	    otp_3906_wait_result(ForkerStarter, F, E);
	succeded ->
	    {succeded, F, E}
    after
	?OTP_3906_TICK_TIMEOUT ->
	    unlink(ForkerStarter),
	    exit(ForkerStarter, timeout),
	    {failed, {timeout, {forked, F}, {exited, E}}}
    end.

otp_3906_collect([], _) ->
    done;
otp_3906_collect(RefList, Sup) ->
    otp_3906_collect(otp_3906_collect_one(RefList, Sup), Sup).

otp_3906_collect_one(RefList, Sup) ->
    receive
	Ref when is_reference(Ref) ->
	    Sup ! tick,
	    lists:delete(Ref, RefList)
    end.
    
otp_3906_start_forker(N, Sup, Prog) ->
    Ref = make_ref(),
    spawn_opt(?MODULE,
	      otp_3906_forker,
	      [N, self(), Ref, Sup, Prog],
	      [link, {priority, max}]),
    Ref.

otp_3906_start_forker_starter(N, RefList, Sup, Prog) ->
    process_flag(priority, max),
    EmPid = os:getpid(),
    Sup ! {emulator_pid, EmPid},
    otp_3906_forker_starter(N, RefList, Sup, Prog).

otp_3906_forker_starter(0, RefList, Sup, _) ->
    otp_3906_collect(RefList, Sup),
    unlink(Sup),
    Sup ! succeded;
otp_3906_forker_starter(N, RefList, Sup, Prog)
  when length(RefList) >= ?OTP_3906_MAX_CONC_OSP ->
    otp_3906_forker_starter(N, otp_3906_collect_one(RefList, Sup), Sup, Prog);
otp_3906_forker_starter(N, RefList, Sup, Prog)
  when is_integer(N), N > ?OTP_3906_OSP_P_ERLP ->
    otp_3906_forker_starter(N-?OTP_3906_OSP_P_ERLP,
			    [otp_3906_start_forker(?OTP_3906_OSP_P_ERLP,
						   Sup,
						   Prog)|RefList],
			    Sup,
			    Prog);
otp_3906_forker_starter(N, RefList, Sup, Prog) when is_integer(N) ->
    otp_3906_forker_starter(0,
			    [otp_3906_start_forker(N,
						   Sup,
						   Prog)|RefList],
			    Sup,
			    Prog).

otp_3906_forker(0, Parent, Ref, _, _) ->
    unlink(Parent),
    Parent ! Ref;
otp_3906_forker(N, Parent, Ref, Sup, Prog) ->
    Port = erlang:open_port({spawn, Prog}, [exit_status, in]),
    Sup ! forked,
    receive
	{Port, {exit_status, ?OTP_3906_EXIT_STATUS}} ->
	    Sup ! exited,
	    otp_3906_forker(N-1, Parent, Ref, Sup, Prog);
	{Port, Res} ->
	    exit(Res);
	Other ->
	    exit(Other)
    end.


otp_4389(suite) -> [];
otp_4389(doc) -> [];
otp_4389(Config)  when is_list(Config) ->
    case os:type() of
	{unix, _} ->
	    Dog = test_server:timetrap(test_server:seconds(240)),
	    TCR = self(),
	    case get_true_cmd() of
		True when is_list(True) ->
		    lists:foreach(
			    fun (P) ->
				    receive
					      {P, ok} ->  ok;
					      {P, Err} -> ?t:fail(Err)
					  end
			    end,
			    lists:map(
			      fun(_) ->
				      spawn_link(
					fun() ->
						process_flag(trap_exit, true),
						case catch open_port({spawn, True},
								     [stream,exit_status]) of
						    P when is_port(P) ->
							receive
							    {P,{exit_status,_}} ->
								TCR ! {self(),ok};
							    {'EXIT',_,{R2,_}} when R2 == emfile;
										   R2 == eagain;
                                                                                   R2 == enomem ->
								TCR ! {self(),ok};
							    Err2 ->
								TCR ! {self(),{msg,Err2}}
							end;
						    {'EXIT',{R1,_}} when R1 == emfile;
									 R1 == eagain;
                                                                         R1 == enomem ->
							TCR ! {self(),ok};
						    Err1 ->
							TCR ! {self(), {open_port,Err1}}
						end
					end)
			      end,
			      lists:duplicate(1000,[]))),
		    test_server:timetrap_cancel(Dog),
		    {comment,
		     "This test case doesn't always fail when the bug that "
		     "it tests for is present (it is most likely to fail on"
		     " a multi processor machine). If the test case fails it"
		     " will fail by deadlocking the emulator."};
		_ ->
		    {skipped, "\"true\" command not found"}
	    end;
	_ ->
	    {skip,"Only run on Unix"}
    end.

get_true_cmd() ->
    DoFileExist = fun (FileName) ->
 			  case file:read_file_info(FileName) of
 			      {ok, _} -> throw(FileName);
 			      _ -> not_found
 			  end
 		  end,
    catch begin
 	      %% First check in /usr/bin and /bin
 	      DoFileExist("/usr/bin/true"),
 	      DoFileExist("/bin/true"),
 	      %% Try which
 	      case filename:dirname(os:cmd("which true")) of
 		  "." -> not_found;
 		  TrueDir -> filename:join(TrueDir, "true")
 	      end
 	  end.

%% 'exit_status' option
exit_status(suite) ->
    [];
exit_status(doc) ->
    ["Test that the 'exit_status' option works"];
exit_status(Config)  when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    port_expect(Config,[{"x",
			       [{exit_status, 5}]}],
		      1, "", [exit_status]),
    test_server:timetrap_cancel(Dog),
    ok.

spawn_driver(suite) ->
    [];
spawn_driver(doc) ->
    ["Test spawning a driver specifically"];
spawn_driver(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "echo_drv"),
    Port = erlang:open_port({spawn_driver, "echo_drv"}, []),
    Port ! {self(), {command, "Hello port!"}},
    receive 
	      {Port, {data, "Hello port!"}} = Msg1 -> 
		  io:format("~p~n", [Msg1]),
		  ok; 
	      Other ->
		  test_server:fail({unexpected, Other})
	  end,
    Port ! {self(), close},
    receive {Port, closed} -> ok end,

    Port2 = erlang:open_port({spawn_driver, "echo_drv -Hello port?"}, 
				   []),
    receive 
	      {Port2, {data, "Hello port?"}} = Msg2 -> 
		  io:format("~p~n", [Msg2]),
		  ok; 
	      Other2 ->
		  test_server:fail({unexpected2, Other2})
	  end,
    Port2 ! {self(), close},
    receive {Port2, closed} -> ok end,
    {'EXIT',{badarg,_}} = (catch erlang:open_port({spawn_driver, "ls"}, [])),
    {'EXIT',{badarg,_}} = (catch erlang:open_port({spawn_driver, "cmd"}, [])),
    {'EXIT',{badarg,_}} = (catch erlang:open_port({spawn_driver, os:find_executable("erl")}, [])),
    test_server:timetrap_cancel(Dog),
    ok.

parallelism_option(suite) ->
    [];
parallelism_option(doc) ->
    ["Test parallelism option of open_port"];
parallelism_option(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line ok = load_driver(Path, "echo_drv"),
    ?line Port = erlang:open_port({spawn_driver, "echo_drv"},
				  [{parallelism, true}]),
    ?line {parallelism, true} = erlang:port_info(Port, parallelism),
    ?line Port ! {self(), {command, "Hello port!"}},
    ?line receive 
	      {Port, {data, "Hello port!"}} = Msg1 -> 
		  io:format("~p~n", [Msg1]),
		  ok; 
	      Other ->
		  test_server:fail({unexpected, Other})
	  end,
    ?line Port ! {self(), close},
    ?line receive {Port, closed} -> ok end,

    ?line Port2 = erlang:open_port({spawn_driver, "echo_drv -Hello port?"}, 
				   [{parallelism, false}]),
    ?line {parallelism, false} = erlang:port_info(Port2, parallelism),
    ?line receive 
	      {Port2, {data, "Hello port?"}} = Msg2 -> 
		  io:format("~p~n", [Msg2]),
		  ok; 
	      Other2 ->
		  test_server:fail({unexpected2, Other2})
	  end,
    ?line Port2 ! {self(), close},
    ?line receive {Port2, closed} -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

spawn_executable(suite) ->
    [];
spawn_executable(doc) ->
    ["Test spawning an executable specifically"];
spawn_executable(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    DataDir = ?config(data_dir, Config),
    EchoArgs1 = filename:join([DataDir,"echo_args"]),
    ExactFile1 = filename:nativename(os:find_executable(EchoArgs1)),
    [ExactFile1] = run_echo_args(DataDir,[]),
    [ExactFile1] = run_echo_args(DataDir,[binary]),
    ["echo_args"] = run_echo_args(DataDir,["echo_args"]),
    ["echo_args"] = run_echo_args(DataDir,[binary, "echo_args"]),
    ["echo_arguments"] = run_echo_args(DataDir,["echo_arguments"]),
    ["echo_arguments"] = run_echo_args(DataDir,[binary, "echo_arguments"]),
    [ExactFile1,"hello world","dlrow olleh"] = 
	run_echo_args(DataDir,[ExactFile1,"hello world","dlrow olleh"]),
    [ExactFile1] = run_echo_args(DataDir,[default]),
    [ExactFile1] = run_echo_args(DataDir,[binary, default]),
    [ExactFile1,"hello world","dlrow olleh"] = 
	run_echo_args(DataDir,[switch_order,ExactFile1,"hello world",
			       "dlrow olleh"]),
    [ExactFile1,"hello world","dlrow olleh"] = 
	run_echo_args(DataDir,[binary,switch_order,ExactFile1,"hello world",
			       "dlrow olleh"]),
    [ExactFile1,"hello world","dlrow olleh"] =
	run_echo_args(DataDir,[default,"hello world","dlrow olleh"]),

    [ExactFile1,"hello world","dlrow olleh"] = 
	run_echo_args_2("\""++ExactFile1++"\" "++"\"hello world\" \"dlrow olleh\""),
    [ExactFile1,"hello world","dlrow olleh"] =
	run_echo_args_2(unicode:characters_to_binary("\""++ExactFile1++"\" "++"\"hello world\" \"dlrow olleh\"")),

    PrivDir = ?config(priv_dir, Config),
    SpaceDir =filename:join([PrivDir,"With Spaces"]),
    file:make_dir(SpaceDir),
    Executable = filename:basename(ExactFile1),
    file:copy(ExactFile1,filename:join([SpaceDir,Executable])),
    ExactFile2 = filename:nativename(filename:join([SpaceDir,Executable])),
    chmodplusx(ExactFile2),
    io:format("|~s|~n",[ExactFile2]),
    [ExactFile2] = run_echo_args(SpaceDir,[]),
    ["echo_args"] = run_echo_args(SpaceDir,["echo_args"]),
    ["echo_arguments"] = run_echo_args(SpaceDir,["echo_arguments"]),
    [ExactFile2,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,[ExactFile2,"hello world","dlrow olleh"]),
    [ExactFile2,"hello world","dlrow olleh"] =
	run_echo_args(SpaceDir,[binary, ExactFile2,"hello world","dlrow olleh"]),

    [ExactFile2,"hello \"world\"","\"dlrow\" olleh"] =
	run_echo_args(SpaceDir,[binary, ExactFile2,"hello \"world\"","\"dlrow\" olleh"]),
    [ExactFile2,"hello \"world\"","\"dlrow\" olleh"] =
	run_echo_args(SpaceDir,[binary, ExactFile2,"hello \"world\"","\"dlrow\" olleh"]),

    [ExactFile2] = run_echo_args(SpaceDir,[default]),
    [ExactFile2,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,[switch_order,ExactFile2,"hello world",
			       "dlrow olleh"]),
    [ExactFile2,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,[default,"hello world","dlrow olleh"]),
    [ExactFile2,"hello world","dlrow olleh"] = 
	run_echo_args_2("\""++ExactFile2++"\" "++"\"hello world\" \"dlrow olleh\""),
    [ExactFile2,"hello world","dlrow olleh"] =
	run_echo_args_2(unicode:characters_to_binary("\""++ExactFile2++"\" "++"\"hello world\" \"dlrow olleh\"")),

    ExeExt = 
	case string:to_lower(lists:last(string:tokens(ExactFile2,"."))) of
	    "exe" ->
		".exe";
	    _ ->
		""
	end,
    Executable2 = "spoky name"++ExeExt,
    file:copy(ExactFile1,filename:join([SpaceDir,Executable2])),
    ExactFile3 = filename:nativename(filename:join([SpaceDir,Executable2])),
    chmodplusx(ExactFile3),
    [ExactFile3] = run_echo_args(SpaceDir,Executable2,[]),
    ["echo_args"] = run_echo_args(SpaceDir,Executable2,["echo_args"]),
    ["echo_arguments"] = run_echo_args(SpaceDir,Executable2,["echo_arguments"]),
    [ExactFile3,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,Executable2,[ExactFile3,"hello world","dlrow olleh"]),
    [ExactFile3] = run_echo_args(SpaceDir,Executable2,[default]),
    [ExactFile3,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,Executable2,
		      [switch_order,ExactFile3,"hello world",
		       "dlrow olleh"]),
    [ExactFile3,"hello world","dlrow olleh"] = 
	run_echo_args(SpaceDir,Executable2,
		      [default,"hello world","dlrow olleh"]),
    [ExactFile3,"hello world","dlrow olleh"] = 
	run_echo_args_2("\""++ExactFile3++"\" "++"\"hello world\" \"dlrow olleh\""),
    [ExactFile3,"hello world","dlrow olleh"] =
	run_echo_args_2(unicode:characters_to_binary("\""++ExactFile3++"\" "++"\"hello world\" \"dlrow olleh\"")),
    {'EXIT',{enoent,_}} = (catch run_echo_args(SpaceDir,"fnurflmonfi",
						     [default,"hello world",
						      "dlrow olleh"])),

    NonExec = "kronxfrt"++ExeExt,
    file:write_file(filename:join([SpaceDir,NonExec]),
			  <<"Not an executable">>),
    {'EXIT',{eacces,_}} = (catch run_echo_args(SpaceDir,NonExec,
						     [default,"hello world",
						      "dlrow olleh"])),
    {'EXIT',{enoent,_}} = (catch open_port({spawn_executable,"cmd"},[])),
    {'EXIT',{enoent,_}} = (catch open_port({spawn_executable,"sh"},[])),
    case os:type() of
	{win32,_} ->
	    test_bat_file(SpaceDir);
	{unix,_} ->
	    test_sh_file(SpaceDir)
    end,
    test_server:timetrap_cancel(Dog),
    ok.

unregister_name(Config) when is_list(Config) ->
    true = register(crash, open_port({spawn, "sleep 100"}, [])),
    true = unregister(crash).

test_bat_file(Dir) ->
    FN = "tf.bat",
    Full = filename:join([Dir,FN]),
    D = [<<"@echo off\r\n">>,
	 <<"echo argv[0]:^|%0^|\r\n">>,
	 <<"if \"%1\" == \"\" goto done\r\n">>,
	 <<"echo argv[1]:^|%1^|\r\n">>,
	 <<"if \"%2\" == \"\" goto done\r\n">>,
	 <<"echo argv[2]:^|%2^|\r\n">>,
	 <<"if \"%3\" == \"\" goto done\r\n">>,
	 <<"echo argv[3]:^|%3^|\r\n">>,
	 <<"if \"%4\" == \"\" goto done\r\n">>,
	 <<"echo argv[4]:^|%4^|\r\n">>,
	 <<"if \"%5\" == \"\" goto done\r\n">>,
	 <<"echo argv[5]:^|%5^|\r\n">>,
	 <<"\r\n">>,
	 <<":done\r\n">>,
	 <<"\r\n">>],
    file:write_file(Full,list_to_binary(D)),
    EF = filename:basename(FN),
    [DN,"hello","world"] = 
	run_echo_args(Dir,FN,
		      [default,"hello","world"]),
    %% The arg0 argumant should be ignored when running batch files
    [DN,"hello","world"] = 
	run_echo_args(Dir,FN,
		      ["knaskurt","hello","world"]),
    EF = filename:basename(DN),
    ok.

test_sh_file(Dir) ->
    FN = "tf.sh",
    Full = filename:join([Dir,FN]),
    D = [<<"#! /bin/sh\n">>,
	 <<"echo 'argv[0]:|'$0'|'\n">>,
	 <<"i=1\n">>,
	 <<"while [ '!' -z \"$1\" ]; do\n">>,
	 <<"    echo 'argv['$i']:|'\"$1\"'|'\n">>,
	 <<"    shift\n">>,
	 <<"    i=`expr $i + 1`\n">>,
	 <<"done\n">>],
    file:write_file(Full,list_to_binary(D)),
    chmodplusx(Full),
    [Full,"hello","world"] = 
	run_echo_args(Dir,FN,
		      [default,"hello","world"]),
    [Full,"hello","world of spaces"] = 
	run_echo_args(Dir,FN,
		      [default,"hello","world of spaces"]),
    file:write_file(filename:join([Dir,"testfile1"]),<<"testdata1">>),
    file:write_file(filename:join([Dir,"testfile2"]),<<"testdata2">>),
    Pattern = filename:join([Dir,"testfile*"]),
    L = filelib:wildcard(Pattern),
    2 = length(L),
    [Full,"hello",Pattern] = 
	run_echo_args(Dir,FN,
		      [default,"hello",Pattern]),
     ok.

    

chmodplusx(Filename) ->
    case file:read_file_info(Filename) of
	{ok,FI} ->
	    FI2 = FI#file_info{mode = ((FI#file_info.mode) bor 8#00100)},
	    file:write_file_info(Filename,FI2);
	_ ->
	    ok
    end.

run_echo_args_2(FullnameAndArgs) ->
    Port = open_port({spawn,FullnameAndArgs},[eof]),
    Data = collect_data(Port),
    Port ! {self(), close},
    receive {Port, closed} -> ok end,
    parse_echo_args_output(Data).
    

run_echo_args(Where,Args) ->
    run_echo_args(Where,"echo_args",Args).
run_echo_args(Where,Prog,Args) ->
    {Binary, ArgvArg} = pack_argv(Args),
    Command0 = filename:join([Where,Prog]),
    Command = case Binary of
		  true -> unicode:characters_to_binary(Command0);
		  false -> Command0
	      end,
    Port = open_port({spawn_executable,Command},ArgvArg++[eof]),
    Data = collect_data(Port),
    Port ! {self(), close},
    receive {Port, closed} -> ok end,
    parse_echo_args_output(Data).

pack_argv([binary|Args]) ->
    {true, pack_argv(Args, true)};
pack_argv(Args) ->
    {false, pack_argv(Args, false)}.

pack_argv(Args, Binary) ->
    case Args of
	[] ->
	    [];
	[default|T] ->
	    [{args,[make_bin(Arg,Binary) || Arg <- T]}];
	[switch_order,H|T] ->
	    [{args,[make_bin(Arg,Binary) || Arg <- T]},{arg0,make_bin(H,Binary)}];
	[H|T] ->
	    [{arg0,make_bin(H,Binary)},{args,[make_bin(Arg,Binary) || Arg <- T]}]
    end.

make_bin(Str, false) -> Str;
make_bin(Str, true) ->  unicode:characters_to_binary(Str).

collect_data(Port) ->
    receive
	{Port, {data, Data}} ->
	    Data ++ collect_data(Port);
	{Port, eof} ->
	    []
    end.

parse_echo_args_output(Data) ->
    [lists:last(string:tokens(S,"|")) || S <- string:tokens(Data,"\r\n")].

mix_up_ports(suite) ->
    [];
mix_up_ports(doc) ->
    ["Test that the emulator does not mix up ports when the port table wraps"];
mix_up_ports(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "echo_drv"),
    Port = erlang:open_port({spawn, "echo_drv"}, []),
    Port ! {self(), {command, "Hello port!"}},
    receive 
	      {Port, {data, "Hello port!"}} = Msg1 -> 
		  io:format("~p~n", [Msg1]),
		  ok; 
	      Other ->
		  test_server:fail({unexpected, Other})
	  end,
    Port ! {self(), close},
    receive {Port, closed} -> ok end,
    loop(start, done,
	       fun(P) ->
		       Q = 
			   (catch erlang:open_port({spawn, "echo_drv"}, [])),
%%		       io:format("~p ", [Q]),
		       if is_port(Q) ->
			       Q;
			  true ->
			       io:format("~p~n", [P]),
			       done
		       end
	       end),
    Port ! {self(), {command, "Hello again port!"}},
    receive 
	      Msg2 ->
		  test_server:fail({unexpected, Msg2})
	  after 1000 ->
		  ok
	  end,
    test_server:timetrap_cancel(Dog),
    ok.

loop(Stop, Stop, Fun) when is_function(Fun) ->
    ok;
loop(Start, Stop, Fun) when is_function(Fun) ->
    loop(Fun(Start), Stop, Fun).


otp_5112(suite) ->
    [];
otp_5112(doc) ->
    ["Test that link to connected process is taken away when port calls",
     "driver_exit() also when the port index has wrapped"];
otp_5112(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "exit_drv"),
    Port = otp_5112_get_wrapped_port(),
    ?t:format("Max ports: ~p~n",[max_ports()]),
    ?t:format("Port: ~p~n",[Port]),
    {links, Links1} = process_info(self(),links),
    ?t:format("Links1: ~p~n",[Links1]),
    true = lists:member(Port, Links1),
    Port ! {self(), {command, ""}},
    ?line wait_until(fun () -> lists:member(Port, erlang:ports()) == false end),
    {links, Links2} = process_info(self(),links),
    ?t:format("Links2: ~p~n",[Links2]),
    false = lists:member(Port, Links2), %% This used to fail
    test_server:timetrap_cancel(Dog),
    ok.

otp_5112_get_wrapped_port() ->
    P1 = erlang:open_port({spawn, "exit_drv"}, []),
    case port_ix(P1) < max_ports() of
	      true ->
		  ?t:format("Need to wrap port index (~p)~n", [P1]),
		  otp_5112_wrap_port_ix([P1]),
		  P2 = erlang:open_port({spawn, "exit_drv"}, []),
		  false = port_ix(P2) < max_ports(),
		  P2;
	      false ->
		  ?t:format("Port index already wrapped (~p)~n", [P1]),
		  P1
	  end.

otp_5112_wrap_port_ix(Ports) ->
    case (catch erlang:open_port({spawn, "exit_drv"}, [])) of
	      Port when is_port(Port) ->
		  otp_5112_wrap_port_ix([Port|Ports]);
	      _ ->
		  %% Port table now full; empty port table
		  lists:foreach(fun (P) ->  P ! {self(), close} end,
				      Ports),
		  ok
	  end.


otp_5119(suite) ->
    [];
otp_5119(doc) ->
    ["Test that port index is not unnecessarily wrapped"];
otp_5119(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "exit_drv"),
    PI1 = port_ix(otp_5119_fill_empty_port_tab([])),
    Port2 = erlang:open_port({spawn, "exit_drv"}, []),
    PI2 = port_ix(Port2),
    {PortIx1, PortIx2} = case PI2 > PI1 of
	true ->
	    {PI1, PI2};
	false ->
	    {port_ix(otp_5119_fill_empty_port_tab([Port2])),
		port_ix(erlang:open_port({spawn, "exit_drv"}, []))}
    end,
    MaxPorts = max_ports(),
    ?t:format("PortIx1 = ~p ~p~n", [PI1, PortIx1]),
    ?t:format("PortIx2 = ~p ~p~n", [PI2, PortIx2]),
    ?t:format("MaxPorts = ~p~n", [MaxPorts]),
    true = PortIx2 > PortIx1,
    true = PortIx2 =< PortIx1 + MaxPorts,
    test_server:timetrap_cancel(Dog),
    ok.

otp_5119_fill_empty_port_tab(Ports) ->
    case (catch erlang:open_port({spawn, "exit_drv"}, [])) of
	      Port when is_port(Port) ->
		  otp_5119_fill_empty_port_tab([Port|Ports]);
	      _ ->
		  %% Port table now full; empty port table
		  lists:foreach(fun (P) ->  P ! {self(), close} end,
				      Ports),
		  [LastPort|_] = Ports,
		  LastPort
	  end.

max_ports() ->
    erlang:system_info(port_limit).

port_ix(Port) when is_port(Port) ->
    ["#Port",_,PortIxStr] = string:tokens(erlang:port_to_list(Port),
						"<.>"),
    list_to_integer(PortIxStr).


otp_6224(doc) -> ["Check that port command failure doesn't crash the emulator"];
otp_6224(suite) -> [];
otp_6224(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(10)),
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "failure_drv"),
    Go = make_ref(),
    Failer = spawn(fun () ->
				 receive Go -> ok end,
				 Port = open_port({spawn, "failure_drv"},
							[]),
				 Port ! {self(), {command, "Fail, please!"}},
				 otp_6224_loop()
			 end),
    Mon = erlang:monitor(process, Failer),
    Failer ! Go,
    receive
	      {'DOWN', Mon, process, Failer, Reason} ->
		  case Reason of
			    {driver_failed, _} -> ok;
			    driver_failed -> ok;
			    _ -> ?t:fail({unexpected_exit_reason,
						Reason})
			end
	  end,
    test_server:timetrap_cancel(Dog),
    ok.
    
otp_6224_loop() ->
    receive _ -> ok after 0 -> ok end,
    otp_6224_loop().


-define(EXIT_STATUS_MSB_MAX_PROCS, 64).
-define(EXIT_STATUS_MSB_MAX_PORTS, 300).

exit_status_multi_scheduling_block(doc) -> [];
exit_status_multi_scheduling_block(suite) -> [];
exit_status_multi_scheduling_block(Config) when is_list(Config) ->
    Repeat = 3,
    case ?t:os_type() of
	      {unix, _} ->
		  Dog = ?t:timetrap(test_server:minutes(2*Repeat)),
		  SleepSecs = 6,
		  try
		      lists:foreach(fun (_) ->
					    exit_status_msb_test(Config,
								 SleepSecs)
				    end,
				    lists:seq(1, Repeat))
		  after
		      %% Wait for the system to recover (regardless
		      %% of success or not) otherwise later testcases
		      %% may unnecessarily fail.
		      ?t:timetrap_cancel(Dog),
		      receive after SleepSecs+500 -> ok end
		  end;
	      _ -> {skip, "Not implemented for this OS"}
	  end.

exit_status_msb_test(Config, SleepSecs) when is_list(Config) ->
    %%
    %% We want to start port programs from as many schedulers as possible
    %% and we want these port programs to terminate while multi-scheduling
    %% is blocked.
    %% 
    NoSchedsOnln = erlang:system_info(schedulers_online),
    Parent = self(),
    ?t:format("SleepSecs = ~p~n", [SleepSecs]),
    PortProg = "sleep " ++ integer_to_list(SleepSecs),
    Start = erlang:monotonic_time(micro_seconds),
    NoProcs = case NoSchedsOnln of
			NProcs when NProcs < ?EXIT_STATUS_MSB_MAX_PROCS ->
			    NProcs;
			_ ->
			    ?EXIT_STATUS_MSB_MAX_PROCS
		    end,
    NoPortsPerProc = case 20*NoProcs of
			       TNPorts when TNPorts < ?EXIT_STATUS_MSB_MAX_PORTS -> 20;
			       _ -> ?EXIT_STATUS_MSB_MAX_PORTS div NoProcs
			   end,
    ?t:format("NoProcs = ~p~nNoPortsPerProc = ~p~n",
		    [NoProcs, NoPortsPerProc]),
    ProcFun
	= fun () ->
		  PrtSIds = lists:map(
			      fun (_) ->
				      erlang:yield(),
				      case catch open_port({spawn, PortProg},
							   [exit_status]) of
					  Prt when is_port(Prt) ->
					      {Prt,
					       erlang:system_info(scheduler_id)};
					  {'EXIT', {Err, _}} when Err == eagain;
								  Err == emfile;
                                                                  Err == enomem ->
					      noop;
					  {'EXIT', Err} when Err == eagain;
							     Err == emfile;
                                                             Err == enomem ->
					      noop;
					  Error ->
					      ?t:fail(Error)
				      end
			      end,
			      lists:seq(1, NoPortsPerProc)),
		  SIds = lists:filter(fun (noop) -> false;
					  (_) -> true
				      end,
				      lists:map(fun (noop) -> noop;
						    ({_, SId}) -> SId
						end,
						PrtSIds)),
		  process_flag(scheduler, 0),
		  Parent ! {self(), started, SIds},
		  lists:foreach(
		    fun (noop) ->
			    noop;
			({Port, _}) ->
			    receive
				{Port, {exit_status, 0}} ->
				    ok;
				{Port, {exit_status, Status}} when Status > 128 ->
				    %% Sometimes happens when we have created
				    %% too many ports.
				    ok;
				{Port, {exit_status, _}} = ESMsg ->
				    {Port, {exit_status, 0}} = ESMsg
			    end
		    end,
		    PrtSIds),
		  Parent ! {self(), done}
	  end,
    Procs = lists:map(fun (N) ->
				    spawn_opt(ProcFun,
					      [link,
					       {scheduler,
						(N rem NoSchedsOnln)+1}])
			    end,
			    lists:seq(1, NoProcs)),
    SIds = lists:map(fun (P) ->
				   receive {P, started, SIds} -> SIds end
			   end,
			   Procs),
    StartedTime = (erlang:monotonic_time(micro_seconds) - Start)/1000000,
    ?t:format("StartedTime = ~p~n", [StartedTime]),
    true = StartedTime < SleepSecs,
    erlang:system_flag(multi_scheduling, block),
    lists:foreach(fun (P) -> receive {P, done} -> ok end end, Procs),
    DoneTime = (erlang:monotonic_time(micro_seconds) - Start)/1000000,
    ?t:format("DoneTime = ~p~n", [DoneTime]),
    true = DoneTime > SleepSecs,
    ok = verify_multi_scheduling_blocked(),
    erlang:system_flag(multi_scheduling, unblock),
    case {length(lists:usort(lists:flatten(SIds))), NoSchedsOnln} of
	      {N, N} ->
		  ok;
	      {N, M} ->
		  ?t:fail("Failed to create ports on all"
				++ integer_to_list(M) ++ " available"
				"schedulers. Only created ports on "
				++ integer_to_list(N) ++ " schedulers.")
	  end.

save_sid(SIds) ->
    SId = erlang:system_info(scheduler_id),
    case lists:member(SId, SIds) of
	true -> SIds;
	false -> [SId|SIds]
    end.

sid_proc(SIds) ->
    NewSIds = save_sid(SIds),
    receive
	{From, want_sids} ->
	    From ! {self(), sids, NewSIds}
    after 0 ->
	    sid_proc(NewSIds)
    end.

verify_multi_scheduling_blocked() ->
    Procs = lists:map(fun (_) ->
				    spawn_link(fun () -> sid_proc([]) end)
			    end,
			    lists:seq(1, 3*erlang:system_info(schedulers_online))),
    receive after 1000 -> ok end,
    SIds = lists:map(fun (P) ->
				   P ! {self(), want_sids},
				   receive {P, sids, PSIds} -> PSIds end
			   end,
			   Procs),
    1 = length(lists:usort(lists:flatten(SIds))),
    ok.
		      
    
%%% Pinging functions.

stream_ping(Config, Size, CmdLine, Options) ->
    Data = random_packet(Size),
    port_expect(Config, [{Data, [Data]}], 0, CmdLine, Options).

ping(Config, Sizes, HSize, CmdLine, Options) ->
    Actions = lists:map(fun(Size) ->
				[$p|Packet] = random_packet(Size, "ping"),
				{[$p|Packet], [[$P|Packet]]}
				end,
			Sizes),
    port_expect(Config, Actions, HSize, CmdLine, Options).

%% expect_input(Sizes, HSize, CmdLine, Options)
%%
%% Sizes = Size of packets to generated.
%% HSize = Header size: 1, 2, or 4
%% CmdLine = Additional command line options.
%% Options = Addtional port options.

expect_input(Config, Sizes, HSize, CmdLine, Options) ->
    expect_input1(Config, Sizes, {HSize, CmdLine, Options}, [], []).

expect_input1(Config, [0|Rest], Params, Expect, ReplyCommand) ->
    expect_input1(Config, Rest, Params, [""|Expect], ["x0"|ReplyCommand]);
expect_input1(Config, [Size|Rest], Params, Expect, ReplyCommand) ->
    Packet = random_packet(Size),
    Fmt = io_lib:format("~c~p", [hd(Packet), Size]),
    expect_input1(Config, Rest, Params, [Packet|Expect], [Fmt|ReplyCommand]);
expect_input1(Config, [], {HSize, CmdLine0, Options}, Expect, ReplyCommand) ->
    CmdLine = build_cmd_line(CmdLine0, ReplyCommand, []),
    port_expect(Config, [{false, lists:reverse(Expect)}],
		HSize, CmdLine, Options).

build_cmd_line(FixedCmdLine, [Cmd|Rest], []) ->
    build_cmd_line(FixedCmdLine, Rest, [Cmd]);
build_cmd_line(FixedCmdLine, [Cmd|Rest], Result) ->
    build_cmd_line(FixedCmdLine, Rest, [Cmd, $:|Result]);
build_cmd_line(FixedCmdLine, [], Result) ->
    lists:flatten([FixedCmdLine, " -r", Result, " -n"]).

%% port_expect(Actions, HSize, CmdLine, Options)
%%
%% Actions = [{Send, ExpectList}|Rest]
%% HSize = 0 (stream), or 1, 2, 4   (header size aka "packet bytes")
%% CmdLine = Command line for port_test.  Don't include -h<digit>.
%% Options = Options for open_port/2.  Don't include {packet, Number} or
%%           or stream.
%%
%% Send = false | list()
%% ExpectList = List of lists or binaries.
%%
%% Returns the port.

port_expect(Config, Actions, HSize, CmdLine, Options0) ->
%    io:format("port_expect(~p, ~p, ~p, ~p)",
%		[Actions, HSize, CmdLine, Options0]),
    PortTest = port_test(Config),
    Cmd = lists:concat([PortTest, " -h", HSize, " ", CmdLine]),
    PortType =
	case HSize of
	    0 -> stream;
	    _ -> {packet, HSize}
	end,
    Options = [PortType|Options0],
    io:format("open_port({spawn, ~p}, ~p)", [Cmd, Options]),
    Port = open_port({spawn, Cmd}, Options),
    port_expect(Port, Actions, Options),
    Port.

port_expect(Port, [{Send, Expects}|Rest], Options) when is_list(Expects) ->
    port_send(Port, Send),
    IsBinaryPort = lists:member(binary, Options),
    Receiver =
	case {lists:member(stream, Options), line_option(Options)} of
	    {false, _} -> fun receive_all/2;
	    {true,false}  -> fun stream_receive_all/2;
	    {_, true} -> fun receive_all/2
	end,
    Receiver(Port, maybe_to_binary(Expects, IsBinaryPort)),
    port_expect(Port, Rest, Options);
port_expect(_, [], _) ->
    ok.

%%% Check for either line or {line,N} in option list
line_option([{line,_}|_]) ->
    true;
line_option([line|_]) ->
    true;
line_option([_|T]) ->
    line_option(T);
line_option([]) ->
    false.

any_list_to_binary({Atom, List}) ->
    {Atom, list_to_binary(List)};
any_list_to_binary(List) ->
    list_to_binary(List).

maybe_to_binary(Expects, true) ->
    lists:map(fun any_list_to_binary/1, Expects);
maybe_to_binary(Expects, false) ->
    Expects.

port_send(_Port, false) -> ok;
port_send(Port, Send) when is_list(Send) ->
%    io:format("port_send(~p, ~p)", [Port, Send]),
    Port ! {self(), {command, Send}}.

receive_all(Port, [Expect|Rest]) ->
%    io:format("receive_all(~p, [~p|Rest])", [Port, Expect]),
    receive
	{Port, {data, Expect}} ->
	    io:format("Received ~s", [format(Expect)]),
	    ok;
	{Port, {data, Other}} ->
	    io:format("Received ~s; expected ~s",
		      [format(Other), format(Expect)]),
	    test_server:fail(bad_message);
	Other ->
	    %% (We're not yet prepared for receiving both 'eol' and
	    %% 'exit_status'; remember that they may appear in any order.)
	    case {Expect, Rest, Other} of
		{eof, [], {Port, eof}} ->
		    io:format("Received soft EOF.",[]),
		    ok;
		{{exit_status, S}, [], {Port, {exit_status, S}}} ->
		    io:format("Received exit status ~p.",[S]),
		    ok;
		_ ->
%%%	            io:format("Unexpected message: ~s", [format(Other)]),
		    io:format("Unexpected message: ~w", [Other]),
		    test_server:fail(unexpected_message)
	    end
    end,
    receive_all(Port, Rest);
receive_all(_Port, []) ->
    ok.

stream_receive_all(Port, [Expect]) ->
    stream_receive_all1(Port, Expect).

stream_receive_all1(_Port, Empty) when is_binary(Empty), size(Empty) == 0 ->
    ok;
stream_receive_all1(_Port, []) ->
    ok;
stream_receive_all1(Port, Expect) ->
    receive
	{Port, {data, Data}} ->
	    Remaining = compare(Data, Expect),
	    stream_receive_all1(Port, Remaining);
	Other ->
	    test_server:fail({bad_message, Other})
    end.

compare(B1, B2) when is_binary(B1), is_binary(B2), byte_size(B1) =< byte_size(B2) ->
    case split_binary(B2, size(B1)) of
	{B1,Remaining} ->
	    Remaining;
	_Other ->
	    test_server:fail(nomatch)
    end;
compare(B1, B2) when is_binary(B1), is_binary(B2) ->
    test_server:fail(too_much_data);
compare([X|Rest1], [X|Rest2]) ->
    compare(Rest1, Rest2);
compare([_|_], [_|_]) ->
    test_server:fail(nomatch);
compare([], Remaining) ->
    Remaining;
compare(_Data, []) ->
    test_server:fail(too_much_data).

maybe_to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
maybe_to_list(List) ->
    List.

format({Eol,List}) ->
    io_lib:format("tuple<~w,~s>",[Eol, maybe_to_list(List)]);
format(List) when is_list(List) ->
    case list_at_least(50, List) of
	true ->
	    io_lib:format("\"~-50s...\"", [List]);
	false ->
	    io_lib:format("~p", [List])
    end;
format(Bin) when is_binary(Bin), size(Bin) >= 50 ->
    io_lib:format("binary<~-50s...>", [binary_to_list(Bin, 1, 50)]);
format(Bin) when is_binary(Bin) ->
    io_lib:format("binary<~s>", [binary_to_list(Bin)]).


list_at_least(Number, [_|Rest]) when Number > 0 ->
    list_at_least(Number-1, Rest);
list_at_least(Number, []) when Number > 0 ->
    false;
list_at_least(0, _List) -> true.


%%% Utility functions.

random_packet(Size) ->
    random_packet(Size, "").

random_packet(Size, Prefix) ->
    build_packet(Size-length(Prefix), lists:reverse(Prefix), random_char()).

build_packet(0, Result, _NextChar) ->
    lists:reverse(Result);
build_packet(Left, Result, NextChar0) ->
    NextChar =
	if
	    NextChar0 >= 126 ->
		33;
	    true ->
		NextChar0+1
	end,
    build_packet(Left-1, [NextChar0|Result], NextChar).

sizes() ->
    [10, 13, 64, 127, 128, 255, 256, 1023, 1024,
	32767, 32768, 65535, 65536].

sizes(Header_Size) ->
    sizes(Header_Size, sizes(), []).

sizes(1, [Packet_Size|Rest], Result) when Packet_Size < 256 ->
    sizes(1, Rest, [Packet_Size|Result]);
sizes(2, [Packet_Size|Rest], Result) when Packet_Size < 65536 ->
    sizes(2, Rest, [Packet_Size|Result]);
sizes(4, [Packet_Size|Rest], Result) ->
    sizes(4, Rest, [Packet_Size|Result]);
sizes(_, _, Result) ->
    Result.

random_char() ->
    random_char("abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789").

random_char(Chars) ->
    lists:nth(uniform(length(Chars)), Chars).

uniform(N) ->
    case rand:export_seed() of
	undefined ->
	    rand:seed(exsplus),
	    io:format("Random seed = ~p\n", [rand:export_seed()]);
	_ ->
	    ok
    end,
    rand:uniform(N).

fun_spawn(Fun) ->
    fun_spawn(Fun, []).

fun_spawn(Fun, Args) ->
    spawn_link(erlang, apply, [Fun, Args]).

port_test(Config) when is_list(Config) ->
    filename:join(?config(data_dir, Config), "port_test").


ports(doc) -> "Test that erlang:ports/0 returns a consistent snapshot of ports";
ports(suite) -> [];
ports(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    ok = load_driver(Path, "exit_drv"),

    receive after 1000 -> ok end, % Wait for other ports to stabilize

    OtherPorts = erlang:ports(),
    io:format("Other ports: ~p\n",[OtherPorts]),
    MaxPorts = 1024 - length(OtherPorts),

    TrafficPid = spawn_link(fun() -> ports_traffic(MaxPorts) end),

    ports_snapshots(100, TrafficPid, OtherPorts),
    TrafficPid ! {self(),die},
    receive {TrafficPid, dead} -> ok end,
    ok.

ports_snapshots(0, _, _) ->
    ok;
ports_snapshots(Iter, TrafficPid, OtherPorts) ->

    TrafficPid ! start,    
    receive after 1 -> ok end,

    Snapshot = erlang:ports(),

    TrafficPid ! {self(), stop},
    receive {TrafficPid, EventList, TrafficPorts} -> ok end,
    
    %%io:format("Snapshot=~p\n", [Snapshot]),
    ports_verify(Snapshot, OtherPorts ++ TrafficPorts, EventList),

    ports_snapshots(Iter-1, TrafficPid, OtherPorts).


ports_traffic(MaxPorts) ->
    ports_traffic_stopped(MaxPorts, {[],0}).

ports_traffic_stopped(MaxPorts, {PortList, PortCnt}) ->
    receive
	start ->
	    %%io:format("Traffic started in ~p\n",[self()]),
	    ports_traffic_started(MaxPorts, {PortList, PortCnt}, []);
	{Pid,die} ->
	    lists:foreach(fun(Port)-> erlang:port_close(Port) end,
				PortList),
	    Pid ! {self(),dead}
    end.

ports_traffic_started(MaxPorts, {PortList, PortCnt}, EventList) ->
    receive 
	{Pid, stop} ->
	    %%io:format("Traffic stopped in ~p\n",[self()]),
	    Pid ! {self(), EventList, PortList},
	    ports_traffic_stopped(MaxPorts, {PortList, PortCnt})

    after 0 ->
	    ports_traffic_do(MaxPorts, {PortList, PortCnt}, EventList)
    end.

ports_traffic_do(MaxPorts, {PortList, PortCnt}, EventList) ->
    N = uniform(MaxPorts),
    case N > PortCnt of
	true -> % Open port	    
	    P = open_port({spawn, "exit_drv"}, []),
	    %%io:format("Created port ~p\n",[P]),
	    ports_traffic_started(MaxPorts, {[P|PortList], PortCnt+1},
				  [{open,P}|EventList]);
	
	false -> % Close port
	    P = lists:nth(N, PortList),
	    %%io:format("Close port ~p\n",[P]),
	    true = erlang:port_close(P),
	    ports_traffic_started(MaxPorts, {lists:delete(P,PortList), PortCnt-1},
				  [{close,P}|EventList])
    end.

ports_verify(Ports, PortsAfter, EventList) ->    
    %%io:format("Candidate=~p\nEvents=~p\n", [PortsAfter, EventList]),
    case lists:sort(Ports) =:= lists:sort(PortsAfter) of
	true ->
	    io:format("Snapshot of ~p ports verified ok.\n",[length(Ports)]),
	    ok;
	false ->
	    %% Note that we track the event list "backwards", undoing open/close:
	    case EventList of
		[{open,P} | Tail] ->
		    ports_verify(Ports, lists:delete(P,PortsAfter), Tail);		    

		[{close,P} | Tail] ->
		    ports_verify(Ports, [P | PortsAfter], Tail);		    

		[] ->
		    test_server:fail("Inconsistent snapshot from erlang:ports()")
	    end
    end.

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.


close_deaf_port(doc) -> ["Send data to port program that does not read it, then close port."
			 "Primary targeting Windows to test threaded_handle_closer in sys.c"];
close_deaf_port(suite) -> [];
close_deaf_port(Config) when is_list(Config) ->
    Dog = test_server:timetrap(test_server:seconds(100)),
    DataDir = ?config(data_dir, Config),
    DeadPort = os:find_executable("dead_port", DataDir),
    Port = open_port({spawn,DeadPort++" 60"},[]),
    erlang:port_command(Port,"Hello, can you hear me!?!?"),
    port_close(Port),

    Res = close_deaf_port_1(0, DeadPort),
    io:format("Waiting for OS procs to terminate...\n"),
    receive after 5*1000 -> ok end,
    test_server:timetrap_cancel(Dog),
    Res.

close_deaf_port_1(200, _) ->
    ok;
close_deaf_port_1(N, Cmd) ->
    Timeout = integer_to_list(rand:uniform(5*1000)),
    try open_port({spawn_executable,Cmd},[{args,[Timeout]}]) of
    	Port ->
	    erlang:port_command(Port,"Hello, can you hear me!?!?"),
	    port_close(Port),
	    close_deaf_port_1(N+1, Cmd)
    catch
    	_:eagain ->
	    {comment, "Could not spawn more than " ++ integer_to_list(N) ++ " OS processes."}
    end.

%% Test undocumented port_set_data/2 and port_get_data/1
%% Hammer from multiple processes a while
%% and then abrubtly close the port (OTP-12208).
port_setget_data(Config) when is_list(Config) ->
    ok = load_driver(?config(data_dir, Config), "echo_drv"),
    Port = erlang:open_port({spawn_driver, "echo_drv"}, []),

    NSched = erlang:system_info(schedulers_online),
    HeapData = {1,2,3,<<"A heap binary">>,fun()->"This is fun"end,
	       list_to_binary(lists:seq(1,100))},
    PRs = lists:map(fun(I) ->
			    spawn_opt(fun() -> port_setget_data_hammer(Port,HeapData,false,1) end,
				      [monitor, {scheduler, I rem NSched}])
		    end,
		    lists:seq(1,10)),
    receive after 100 -> ok end,
    Papa = self(),
    lists:foreach(fun({Pid,_}) -> Pid ! {Papa,prepare_for_close} end, PRs),
    lists:foreach(fun({Pid,_}) ->
			  receive {Pid,prepare_for_close} -> ok end
		  end,
		  PRs),
    port_close(Port),
    lists:foreach(fun({Pid,Ref}) ->
			  receive {'DOWN', Ref, process, Pid, normal} -> ok end
		  end,
		  PRs),
    ok.

port_setget_data_hammer(Port, HeapData, IsSet0, N) ->
    Rand = rand:uniform(3),
    IsSet1 = try case Rand of
		     1 -> true = erlang:port_set_data(Port, atom), true;
		     2 -> true = erlang:port_set_data(Port, HeapData), true;
		     3 -> case erlang:port_get_data(Port) of
			      atom -> true;
			      HeapData -> true;
			      undefined -> false=IsSet0
			  end
		 end
    catch
	error:badarg ->
	    true = get(prepare_for_close),
	    io:format("~p did ~p rounds before port closed\n", [self(), N]),
	    exit(normal)
    end,
    receive {Papa, prepare_for_close} ->
	    put(prepare_for_close, true),
	    Papa ! {self(),prepare_for_close}
    after 0 ->
	    ok
    end,
    port_setget_data_hammer(Port, HeapData, IsSet1, N+1).


wait_until(Fun) ->
    case catch Fun() of
	true ->
	    ok;
	_ ->
	    receive after 100 -> ok end,
	    wait_until(Fun)
    end.
