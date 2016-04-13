%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(port_bif_SUITE).


-export([all/0, suite/0, groups/0,
         command/1,
	 command_e_1/1, command_e_2/1, command_e_3/1, command_e_4/1,
	 port_info1/1, port_info2/1,
	 port_info_os_pid/1, port_info_race/1,
	 connect/1, control/1, echo_to_busy/1]).

-export([do_command_e_1/1, do_command_e_2/1, do_command_e_4/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 10}}].

all() -> 
    [command, {group, port_info}, connect, control,
     echo_to_busy].

groups() -> 
    [{command_e, [],
      [command_e_1, command_e_2, command_e_3, command_e_4]},
     {port_info, [],
      [port_info1, port_info2, port_info_os_pid, port_info_race]}].

command(Config) when is_list(Config) ->
    load_control_drv(Config),

    P = open_port({spawn, control_drv}, []),
    do_command(P, "hello"),
    do_command(P, <<"hello">>),
    do_command(P, sub_bin(<<"1234kalle">>)),
    do_command(P, unaligned_sub_bin(<<"blurf">>)),
    do_command(P, ["bl"|unaligned_sub_bin(<<"urf">>)]),
    true = erlang:port_close(P),
    ok.

do_command(P, Data) ->
    true = erlang:port_command(P, Data),
    receive
	{P,{data,Data}} ->
	    ok;
	{P,{data,Data0}} ->
	    case {list_to_binary(Data0),list_to_binary([Data])} of
		{B,B} -> ok;
		_ -> ct:fail({unexpected_data,Data0})
	    end;
	Other ->
	    ct:fail({unexpected_message,Other})
    end.



%% port_command/2: badarg 1st arg
command_e_1(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    _ = spawn_link(?MODULE, do_command_e_1, [Program]),
    receive
        {'EXIT', Pid, {badarg, _}} when is_pid(Pid) ->
            ok;
        Other ->
            ct:fail(Other)
    after 10000 ->
            ct:fail(timeout)
    end,
    ok.

do_command_e_1(Program) ->
    _ = open_port({spawn, Program}, []),
    erlang:port_command(apple, "plock"),
    exit(survived).

%% port_command/2: badarg 2nd arg
command_e_2(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    _ = spawn_link(?MODULE, do_command_e_2, [Program]),
    receive
        {'EXIT', Pid, {badarg, _}} when is_pid(Pid) ->
            ok;
        Other ->
            ct:fail(Other)
    after 10000 ->
            ct:fail(timeout)
    end,
    ok.

do_command_e_2(Program) ->
    P = open_port({spawn, Program}, []),
    erlang:port_command(P, 1),
    exit(survived).

%% port_command/2: Posix signals trapped
command_e_3(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    P = open_port({spawn, Program}, [{packet, 1}]),
    Data = lists:duplicate(257, $a),
    erlang:port_command(P, Data),
    receive
        {'EXIT', Port, einval} when is_port(Port) ->
            ok;
        Other ->
            ct:fail(Other)
    after 10000 ->
            ct:fail(timeout)
    end,
    ok.

%% port_command/2: Posix exit signals not trapped
command_e_4(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Program = filename:join(DataDir, "port_test"),

    process_flag(trap_exit, true),
    _ = spawn_link(?MODULE, do_command_e_4, [Program]),
    receive
        {'EXIT', Pid, {einval, _}} when is_pid(Pid) ->
            ok;
        Other ->
            ct:fail(Other)
    after 10000 ->
            ct:fail(timeout)
    end,
    ok.

do_command_e_4(Program) ->
    P = open_port({spawn, Program}, [{packet, 1}]),
    Data = lists:duplicate(257, $a),
    erlang:port_command(P, Data),
    exit(survived).


%% Tests the port_info/1 BIF
port_info1(Config) when is_list(Config) ->
    load_control_drv(Config),
    Me=self(),
    P = open_port({spawn, control_drv}, []),
    A1 = erlang:port_info(P),
    false = lists:keysearch(registered_name, 1, A1),
    register(myport, P),
    A = erlang:port_info(P),
    {value,{registered_name,myport}}= lists:keysearch(registered_name, 1, A),
    {value,{name,"control_drv"}}=lists:keysearch(name, 1, A),
    {value,{links,[Me]}}=lists:keysearch(links, 1, A),
    {value,{id,_IdNum}}=lists:keysearch(id, 1, A),
    {value,{connected,_}}=lists:keysearch(connected, 1, A),
    {value,{input,0}}=lists:keysearch(input, 1, A),
    {value,{output,0}}=lists:keysearch(output, 1, A),
    {value,{os_pid,undefined}}=lists:keysearch(os_pid, 1, A),  % linked-in driver doesn't have a OS pid
    true=erlang:port_close(P),
    ok.

%% Tests erlang:port_info/2"
port_info2(Config) when is_list(Config) ->
    load_control_drv(Config),

    P = open_port({spawn,control_drv}, [binary]),
    [] = erlang:port_info(P, registered_name),
    register(myport, P),
    {registered_name, myport} = erlang:port_info(P, registered_name),

    {name, "control_drv"}=erlang:port_info(P, name),
    {id, _IdNum} = erlang:port_info(P, id),
    Me=self(),
    {links, [Me]} = erlang:port_info(P, links),
    {connected, Me} = erlang:port_info(P, connected),
    {input, 0}=erlang:port_info(P, input),
    {output,0}=erlang:port_info(P, output),
    {os_pid, undefined}=erlang:port_info(P, os_pid),  % linked-in driver doesn't have a OS pid

    erlang:port_control(P, $i, "abc"),
    receive
        {P,{data,<<"abc">>}} -> ok
    end,
    {input,3} = erlang:port_info(P, input),
    {output,0} = erlang:port_info(P, output),

    Bin = list_to_binary(lists:duplicate(2047, 42)),
    output_test(P, Bin, 3, 0),
    
    true = erlang:port_close(P),
    ok.

%% Tests the port_info/1,2 os_pid option BIF
port_info_os_pid(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} ->
	    do_port_info_os_pid();
	_ ->
	    {skip,"Only on Unix."}
    end.

do_port_info_os_pid() ->
    P = open_port({spawn, "echo $$"}, [eof]),
    A = erlang:port_info(P),
    {os_pid, InfoOSPid} = erlang:port_info(P, os_pid),
    EchoPidStr = receive
	    {P, {data, EchoPidStr0}} -> EchoPidStr0
	    after 10000 -> ct:fail(timeout)
    end,
    {ok, [EchoPid], []} = io_lib:fread("~u\n", EchoPidStr),
    {value,{os_pid, InfoOSPid}}=lists:keysearch(os_pid, 1, A),
    EchoPid = InfoOSPid,
    true = erlang:port_close(P),
    ok.

port_info_race(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Program = filename:join(DataDir, "port_test"),
    Top = self(),
    P1 = open_port({spawn,Program}, [{packet,1}]),
    P2 = open_port({spawn,Program}, [{packet,1}]),
    Info1 = erlang:port_info(P1),
    Info2 = erlang:port_info(P2),
    F = fun Loop(Port, _, 0) ->
                Top ! {ok,Port};
            Loop(Port, Info, N) ->
                Info = erlang:port_info(Port),
                Loop(Port, Info, N - 1)
        end,
    spawn_link(fun () -> F(P1, Info1, 1000) end),
    spawn_link(fun () -> F(P2, Info2, 1000) end),
    receive {ok,P1} -> ok end,
    receive {ok,P2} -> ok end,
    true = erlang:port_close(P1),
    true = erlang:port_close(P2),
    ok.

output_test(_, _, Input, Output) when Output > 16#1fffffff ->
    io:format("~p bytes received\n", [Input]);
output_test(P, Bin, Input0, Output0) ->
    erlang:port_command(P, Bin),
    receive
        {P,{data,Bin}} -> ok;
        Other ->
            ct:fail("~p", [Other])
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
    load_control_drv(Config),

    P = open_port({spawn, control_drv}, []),
    register(myport, P),

    true = erlang:port_connect(myport, self()),

    %% Connect the port to another process.

    Data = "hello, world",
    Parent = self(),
    Rec = fun(Me) ->
	    receive
		{P,{data,Data}} ->
		    Parent ! connect_ok,
		    Me(Me)
	    end
    end,
    RecPid = spawn_link(fun() -> Rec(Rec) end),
    true = erlang:port_connect(P, RecPid),
    unlink(P),

    %% Send a command to the port and make sure that the
    %% other process receives the echo.

    erlang:port_command(P, Data),
    receive
        connect_ok -> ok
    end,

    %% Tests some errors.

    {'EXIT',{badarg, _}}=(catch erlang:port_connect(self(), self())),
    {'EXIT',{badarg, _}}=(catch erlang:port_connect(self(), P)),
    {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, P)),
    {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, xxxx)),
    {'EXIT',{badarg, _}}=(catch erlang:port_connect(P, [])),

    process_flag(trap_exit, true),
    exit(P, you_should_die),
    receive
        {'EXIT',RecPid,you_should_die} -> ok;
        Other -> ct:fail({bad_message,Other})
    end,

    %% Done.
    ok.

%% Tests port_control/3
control(Config) when is_list(Config) ->
    load_control_drv(Config),
    P = open_port({spawn, control_drv}, []),

    %% Test invalid (out-of-range) arguments.

    {'EXIT', {badarg, _}} = (catch erlang:port_control(self(), 1, [])),

    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, -1, [])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, -34887348739733833, [])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 16#100000000, [])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, a, [])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 'e', dum)),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, dum)),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, fun(X) -> X end)),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, [fun(X) -> X end])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, $e, [1|fun(X) -> X end])),

    %% Test errors detected by the driver.

    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 177, [])),
    {'EXIT', {badarg, _}} = (catch erlang:port_control(P, 155, random_packet(1024))),

    %% Test big op codes.

    register(myport, P),
    test_op(myport, 256),
    test_op(P, 256),
    test_op(P, 16#0033A837),
    test_op(P, 16#0ab37938),
    test_op(P, 16#eab37938),
    test_op(P, 16#ffffFFFF),

    %% Test the echo function of the driver.

    echo(P, 0),
    echo(P, 1),
    echo(P, 10),
    echo(P, 13),
    echo(P, 63),
    echo(P, 64),
    echo(P, 65),
    echo(P, 127),
    echo(P, 1023),
    echo(P, 1024),
    echo(P, 11243),
    echo(P, 70000),

    %% Done.

    true = erlang:port_close(myport),
    ok.

test_op(P, Op) ->
    R = port_control(P, Op, []),
    <<Op:32>> = list_to_binary(R).

echo_to_busy(Config) when is_list(Config) ->
    ct:timetrap({seconds, 10}),
    load_control_drv(Config),
    P = open_port({spawn, control_drv}, []),
    erlang:port_control(P, $b, [1]),	% Set to busy.
    Self = self(),
    Echoer = spawn(fun() -> echoer(P, Self) end),
    receive after 500 -> ok end,
    erlang:port_control(P, $b, [0]),	% Set to not busy.
    receive
        {Echoer, done} ->
            ok;
        {Echoer, Other} ->
            ct:fail(Other);
        Other ->
            ct:fail({unexpected_message, Other})
    end,
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
    DataDir = proplists:get_value(data_dir, Config),
    erl_ddll:start(),
    ok = load_driver(DataDir, "control_drv").

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
    rand:uniform(N).

unaligned_sub_bin(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin.

sub_bin(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B.

id(I) -> I.
