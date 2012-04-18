%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(port_bif_SUITE).


-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, command/1,
	 command_e_1/1, command_e_2/1, command_e_3/1, command_e_4/1,
	 port_info1/1, port_info2/1,
	 connect/1, control/1, echo_to_busy/1]).

-export([do_command_e_1/1, do_command_e_2/1, do_command_e_4/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [command, {group, port_info}, connect, control,
     echo_to_busy].

groups() -> 
    [{command_e, [],
      [command_e_1, command_e_2, command_e_3, command_e_4]},
     {port_info, [], [port_info1, port_info2]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_testcase(_Func, Config) when is_list(Config) ->
    Dog=test_server:timetrap(test_server:minutes(10)),
    [{watchdog, Dog}|Config].
end_per_testcase(_Func, Config) when is_list(Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).

command(Config) when is_list(Config) ->
    ?line load_control_drv(Config),

    ?line P = open_port({spawn, control_drv}, []),
    ?line do_command(P, "hello"),
    ?line do_command(P, <<"hello">>),
    ?line do_command(P, sub_bin(<<"1234kalle">>)),
    ?line do_command(P, unaligned_sub_bin(<<"blurf">>)),
    ?line do_command(P, ["bl"|unaligned_sub_bin(<<"urf">>)]),
    ?line true = erlang:port_close(P),
    ok.

do_command(P, Data) ->
    true = erlang:port_command(P, Data),
    receive
	{P,{data,Data}} ->
	    ok;
	{P,{data,Data0}} ->
	    case {list_to_binary(Data0),list_to_binary([Data])} of
		{B,B} -> ok;
		_ -> test_server:fail({unexpected_data,Data0})
	    end;
	Other ->
	    test_server:fail({unexpected_message,Other})
    end.



%% port_command/2: badarg 1st arg
command_e_1(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    ?line _ = spawn_link(?MODULE, do_command_e_1, [Program]),
    ?line receive
	      {'EXIT', Pid, {badarg, _}} when is_pid(Pid) ->
		  ok;
	      Other ->
		  ?line test_server:fail(Other)
	  after 10000 ->
		  ?line test_server:fail(timeout)
	  end,
    ok.

do_command_e_1(Program) ->
    ?line _ = open_port({spawn, Program}, []),
    ?line erlang:port_command(apple, "plock"),
    exit(survived).

%% port_command/2: badarg 2nd arg
command_e_2(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    ?line _ = spawn_link(?MODULE, do_command_e_2, [Program]),
    ?line receive
	      {'EXIT', Pid, {badarg, _}} when is_pid(Pid) ->
		  ok;
	      Other ->
		  ?line test_server:fail(Other)
	  after 10000 ->
		  ?line test_server:fail(timeout)
	  end,
    ok.

do_command_e_2(Program) ->
    ?line P = open_port({spawn, Program}, []),
    ?line erlang:port_command(P, 1),
    exit(survived).

%% port_command/2: Posix signals trapped
command_e_3(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    ?line P = open_port({spawn, Program}, [{packet, 1}]),
    ?line Data = lists:duplicate(257, $a),
    ?line erlang:port_command(P, Data),
    ?line receive
	      {'EXIT', Port, einval} when is_port(Port) ->
		  ok;
	      Other ->
		  test_server:fail(Other)
	  after 10000 ->
		  test_server:fail(timeout)
	  end,
    ok.

%% port_command/2: Posix exit signals not trapped
command_e_4(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    ?line _ = spawn_link(?MODULE, do_command_e_4, [Program]),
    ?line receive
	      {'EXIT', Pid, {einval, _}} when is_pid(Pid) ->
		  ok;
	      Other ->
		  ?line test_server:fail(Other)
	  after 10000 ->
		  ?line test_server:fail(timeout)
	  end,
    ok.

do_command_e_4(Program) ->
    ?line P = open_port({spawn, Program}, [{packet, 1}]),
    ?line Data = lists:duplicate(257, $a),
    ?line erlang:port_command(P, Data),
    exit(survived).


%% Tests the port_info/1 BIF
port_info1(Config) when is_list(Config) ->
    ?line load_control_drv(Config),
    Me=self(),
    ?line P = open_port({spawn, control_drv}, []),
    ?line A1 = erlang:port_info(P),
    ?line false = lists:keysearch(registered_name, 1, A1),
    ?line register(myport, P),
    ?line A = erlang:port_info(P),
    ?line {value,{registered_name,myport}}=
	lists:keysearch(registered_name, 1, A),
    ?line {value,{name,"control_drv"}}=lists:keysearch(name, 1, A),
    ?line {value,{links,[Me]}}=lists:keysearch(links, 1, A),
    ?line {value,{id,_IdNum}}=lists:keysearch(id, 1, A),
    ?line {value,{connected,_}}=lists:keysearch(connected, 1, A),
    ?line {value,{input,0}}=lists:keysearch(input, 1, A),
    ?line {value,{output,0}}=lists:keysearch(output, 1, A),
    ?line true=erlang:port_close(P),
    ok.

%% Tests erlang:port_info/2"
port_info2(Config) when is_list(Config) ->
    ?line load_control_drv(Config),

    ?line P = open_port({spawn,control_drv}, [binary]),
    ?line [] = erlang:port_info(P, registered_name),
    ?line register(myport, P),
    ?line {registered_name, myport} = erlang:port_info(P, registered_name),

    ?line {name, "control_drv"}=erlang:port_info(P, name),
    ?line {id, _IdNum} = erlang:port_info(P, id),
    Me=self(),
    ?line {links, [Me]} = erlang:port_info(P, links),
    ?line {connected, Me} = erlang:port_info(P, connected),
    ?line {input, 0}=erlang:port_info(P, input),
    ?line {output,0}=erlang:port_info(P, output),

    ?line erlang:port_control(P, $i, "abc"),
    ?line receive
	      {P,{data,<<"abc">>}} -> ok
	  end,
    ?line {input,3} = erlang:port_info(P, input),
    ?line {output,0} = erlang:port_info(P, output),

    ?line Bin = list_to_binary(lists:duplicate(2047, 42)),
    ?line output_test(P, Bin, 3, 0),
    
    ?line true = erlang:port_close(P),
    ok.

output_test(_, _, Input, Output) when Output > 16#1fffffff ->
    io:format("~p bytes received\n", [Input]);
output_test(P, Bin, Input0, Output0) ->
    erlang:port_command(P, Bin),
    receive
	{P,{data,Bin}} -> ok;
	Other ->
	    io:format("~p", [Other]),
	    ?line ?t:fail()
    end,
    Input = Input0 + size(Bin),
    Output = Output0 + size(Bin),
    {input,Input} = erlang:port_info(P, input),
    {output,Output} = erlang:port_info(P, output),

    %% We can't test much here, but hopefully a debug-built emulator will crasch
    %% if there is something wrong with the heap allocation.
    case erlang:statistics(io) of
	{{input,In},{output,Out}} when is_integer(In), is_integer(Out) ->
	    ok
    end,
    output_test(P, Bin, Input, Output).

%% Tests the port_connect/2 BIF.
connect(Config) when is_list(Config) ->
    ?line load_control_drv(Config),

    ?line P = open_port({spawn, control_drv}, []),
    register(myport, P),

    ?line true = erlang:port_connect(myport, self()),

    %% Connect the port to another process.

    Data = "hello, world",
    Parent = self(),
    ?line Rec =
	fun(Me) -> receive
		       {P,{data,Data}} ->
			   Parent ! connect_ok,
			   Me(Me)
		   end
	end,
    ?line RecPid = spawn_link(fun() -> Rec(Rec) end),
    ?line true = erlang:port_connect(P, RecPid),
    ?line unlink(P),

    %% Send a command to the port and make sure that the
    %% other process receives the echo.

    ?line erlang:port_command(P, Data),
    ?line receive
	      connect_ok -> ok
	  end,

    %% Tests some errors.

    ?line {'EXIT',{badarg, _}}=(catch erlang:port_connect(self(), self())),
    ?line {'EXIT',{badarg, _}}=(catch erlang:port_connect(self(), P)),
    ?line {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, P)),
    ?line {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, xxxx)),
    ?line {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, [])),

    ?line process_flag(trap_exit, true),
    ?line exit(P, you_should_die),
    ?line receive
	      {'EXIT',RecPid,you_should_die} -> ok;
	      Other -> ?line ?t:fail({bad_message,Other})
	  end,

    %% Done.
    ok.

%% Tests port_control/3
control(Config) when is_list(Config) ->
    ?line load_control_drv(Config),
    ?line P = open_port({spawn, control_drv}, []),

    %% Test invalid (out-of-range) arguments.

    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(self(), 1, [])),

    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, -1, [])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, -34887348739733833, [])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 16#100000000, [])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, a, [])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 'e', dum)),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, dum)),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, fun(X) -> X end)),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e,
							     [fun(X) -> X end])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e,
							     [1|fun(X) -> X end])),

    %% Test errors detected by the driver.

    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 177, [])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 155,
							     random_packet(1024))),

    %% Test big op codes.

    register(myport, P),
    ?line test_op(myport, 256),
    ?line test_op(P, 256),
    ?line test_op(P, 16#0033A837),
    ?line test_op(P, 16#0ab37938),
    ?line test_op(P, 16#eab37938),
    ?line test_op(P, 16#ffffFFFF),

    %% Test the echo function of the driver.

    ?line echo(P, 0),
    ?line echo(P, 1),
    ?line echo(P, 10),
    ?line echo(P, 13),
    ?line echo(P, 63),
    ?line echo(P, 64),
    ?line echo(P, 65),
    ?line echo(P, 127),
    ?line echo(P, 1023),
    ?line echo(P, 1024),
    ?line echo(P, 11243),
    ?line echo(P, 70000),

    %% Done.

    ?line true=erlang:port_close(myport),
    ok.

test_op(P, Op) ->
    R = port_control(P, Op, []),
    <<Op:32>> = list_to_binary(R).

echo_to_busy(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line load_control_drv(Config),
    ?line P = open_port({spawn, control_drv}, []),
    ?line erlang:port_control(P, $b, [1]),	% Set to busy.
    Self = self(),
    ?line Echoer = spawn(fun() -> echoer(P, Self) end),
    ?line receive after 500 -> ok end,
    ?line erlang:port_control(P, $b, [0]),	% Set to not busy.
    ?line receive 
	      {Echoer, done} ->
		  ok;
	      {Echoer, Other} ->
		  test_server:fail(Other);
	      Other ->
		  test_server:fail({unexpected_message, Other})
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

echoer(P, ReplyTo) ->
    Msg = random_packet(73),
    true = erlang:port_connect(P, self()),
    erlang:port_command(P, Msg),
    receive
	{P, {data, Msg}} ->
	    ReplyTo ! {self(), done};
	Other ->
	    ReplyTo ! {self(), {bad_message, Other}}
    end.

echo(P, Size) ->
    io:format("Echo test, size ~w", [Size]),
    Packet = random_packet(Size),
    Packet = erlang:port_control(P, $e, Packet),
    Bin = list_to_binary(Packet),
    Packet = erlang:port_control(P, $e, Bin),
    Packet = erlang:port_control(P, $e, sub_bin(Bin)),
    Packet = erlang:port_control(P, $e, unaligned_sub_bin(Bin)),
    Packet = erlang:port_control(P, $e, [unaligned_sub_bin(Bin)]).

load_control_drv(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    ?line ok = load_driver(DataDir, "control_drv").

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
	ok -> ok;
	{error, Error} = Res ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    Res
    end.

random_packet(Size) ->
    random_packet(Size, "", random_char()).

random_packet(0, Result, _NextChar) ->
    Result;
random_packet(Left, Result, NextChar0) ->
    NextChar =
	if
	    NextChar0 >= 126 ->
		33;
	    true ->
		NextChar0+1
	end,
    random_packet(Left-1, [NextChar0|Result], NextChar).

random_char() ->
    random_char("abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789").

random_char(Chars) ->
    lists:nth(uniform(length(Chars)), Chars).

uniform(N) ->
    case get(random_seed) of
	undefined ->
	    {X, Y, Z} = time(),
	    random:seed(X, Y, Z);
	_ ->
	    ok
    end,
    random:uniform(N).

unaligned_sub_bin(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

sub_bin(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B.

id(I) -> I.
    
