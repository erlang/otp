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

-module(busy_port_SUITE).

-export([all/1, io_to_busy/1, message_order/1, send_3/1, 
	 system_monitor/1, no_trap_exit/1,
	 no_trap_exit_unlinked/1, trap_exit/1, multiple_writers/1,
	 hard_busy_driver/1, soft_busy_driver/1]).

-include("test_server.hrl").

%% Internal exports.
-export([init/2]).

all(suite) -> {req, [dynamic_loading],
	       [io_to_busy, message_order, send_3, 
		system_monitor, no_trap_exit,
		no_trap_exit_unlinked, trap_exit, multiple_writers,
		hard_busy_driver, soft_busy_driver]}.

%% Tests I/O operations to a busy port, to make sure a suspended send
%% operation is correctly restarted.  This used to crash Beam.

io_to_busy(suite) -> [];
io_to_busy(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(30)),

    ?line start_busy_driver(Config),
    ?line process_flag(trap_exit, true),
    ?line Writer = fun_spawn(fun writer/0),
    ?line Generator = fun_spawn(fun() -> generator(100, Writer) end),
    ?line wait_for([Writer, Generator]),

    ?line test_server:timetrap_cancel(Dog),
    ok.

generator(N, Writer) ->
    generator(N, Writer, lists:duplicate(128, 42)).

generator(0, Writer, _Data) ->
    Writer ! stop,
    erlang:garbage_collect(),
    receive after 2000 -> ok end,

    %% Calling process_info(Pid, current_function) on a suspended process
    %% used to crash Beam.
    {current_function, {erlang, send, 2}} =
	process_info(Writer, current_function),
    unlock_slave();
generator(N, Writer, Data) ->
    Writer ! {exec, Data},
    generator(N-1, Writer, [42|Data]).

writer() ->
    {Owner, Port} = get_slave(),
    Port ! {Owner, {connect, self()}},
    X = {a, b, c, d},
    forget({element(1, X), element(2, X), element(3, X), element(4, X)}),
    writer_loop(Port).

writer_loop(Port) ->
    receive
	{exec, Data} ->
	    Port ! {self(), {command, Data}},
	    writer_loop(Port);
	stop ->
	    erlang:garbage_collect()
    end.

forget(_) ->
    ok.

%% Test the interaction of busy ports and message sending.
%% This used to cause the wrong message to be received.

message_order(suite) -> {req, dynamic_loading};
message_order(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),

    ?line start_busy_driver(Config),
    ?line Self = self(),
    ?line Busy = fun_spawn(fun () -> send_to_busy_1(Self) end),
    ?line receive after 1000 -> ok end,
    ?line Busy ! first,
    ?line Busy ! second,
    ?line receive after 1 -> ok end,
    ?line unlock_slave(),
    ?line Busy ! third,
    ?line receive
	      {Busy, first} ->
		  ok;
	      Other ->
		  test_server:fail({unexpected_message, Other})
	  end,

    ?line test_server:timetrap_cancel(Dog),
    ok.

send_to_busy_1(Parent) ->
    {Owner, Slave} = get_slave(),
    Slave ! {Owner, {command, "set_me_busy"}},
    Slave ! {Owner, {command, "hello"}},
    Slave ! {Owner, {command, "hello again"}},
    receive
	Message ->
	    Parent ! {self(), Message}
    end.

%% Test the bif send/3
send_3(suite) -> {req,dynamic_loading};
send_3(doc) -> ["Test the BIF send/3"];
send_3(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    %%
    ?line start_busy_driver(Config),
    ?line {Owner,Slave} = get_slave(),
    ?line ok = erlang:send(Slave, {Owner,{command,"set busy"}}, 
			   [nosuspend]),
    ?line nosuspend = erlang:send(Slave, {Owner,{command,"busy"}},
				  [nosuspend]),
    ?line unlock_slave(),
    ?line ok = erlang:send(Slave, {Owner,{command,"not busy"}}, 
			   [nosuspend]),
    ?line ok = command(stop),
    %%
    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Test the erlang:system_monitor(Pid, [busy_port])
system_monitor(suite) -> {req,dynamic_loading};
system_monitor(doc) -> ["Test erlang:system_monitor({Pid,[busy_port]})."];
system_monitor(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Self = self(),
    %%
    ?line OldMonitor = erlang:system_monitor(Self, [busy_port]),
    ?line {Self,[busy_port]} = erlang:system_monitor(),
    ?line Void = make_ref(),
    ?line start_busy_driver(Config),
    ?line {Owner,Slave} = get_slave(),
    ?line Master = command(get_master),
    ?line Parent = self(),
    ?line Busy = 
	spawn_link(
	  fun() ->
		  Slave ! {Owner,{command,"set busy"}},
		  receive {Parent,alpha} -> ok end,
		  Slave ! {Owner,{command,"busy"}},
		  Slave ! {Owner,{command,"free"}},
		  Parent ! {self(),alpha},
		  command(lock),
		  receive {Parent,beta} -> ok end,
		  command({port_command,"busy"}),
		  command({port_command,"free"}),
		  Parent ! {self(),beta}
	  end),
    ?line Void = rec(Void),
    ?line Busy ! {self(),alpha},
    ?line {monitor,Busy,busy_port,Slave} = rec(Void),
    ?line unlock_slave(),
    ?line {Busy,alpha} = rec(Void),
    ?line Void = rec(Void),
    ?line Busy ! {self(), beta},
    ?line {monitor,Owner,busy_port,Slave} = rec(Void),
    ?line Master ! {Owner, {command, "u"}},
    ?line {Busy,beta} = rec(Void),
    ?line Void = rec(Void),
    ?line NewMonitor = erlang:system_monitor(OldMonitor),
    ?line OldMonitor = erlang:system_monitor(),
    ?line OldMonitor = erlang:system_monitor(OldMonitor),
    %%
    ?line test_server:timetrap_cancel(Dog),
    ok.



rec(Tag) ->
    receive X -> X after 1000 -> Tag end.




%% Assuming the following scenario,
%%
%%  +---------------+		       +-----------+
%%  | process with  |		       |           |
%%  | no trap_exit  |------------------| busy port |
%%  | (suspended)   |		       |	   |
%%  +---------------+		       +-----------+
%%
%% tests that the suspended process is killed if the port is killed.

no_trap_exit(suite) -> [];
no_trap_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line process_flag(trap_exit, true),
    ?line Pid = fun_spawn(fun no_trap_exit_process/3,
			  [self(), linked, Config]),
    ?line receive
	      {Pid, port_created, Port} ->
		  io:format("Process ~w created port ~w", [Pid, Port]),
		  ?line exit(Port, die);
	      Other1 ->
		  test_server:fail({unexpected_message, Other1})
	  end,
    ?line receive
	      {'EXIT', Pid, die} ->
		  ok;
	      Other2 ->
		  test_server:fail({unexpected_message, Other2})
	  end,

    ?line test_server:timetrap_cancel(Dog),
    ok.

%% The same scenario as above, but the port has been explicitly
%% unlinked from the process.

no_trap_exit_unlinked(suite) -> [];
no_trap_exit_unlinked(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line process_flag(trap_exit, true),
    ?line Pid = fun_spawn(fun no_trap_exit_process/3,
			  [self(), unlink, Config]),
    ?line receive
	      {Pid, port_created, Port} ->
		  io:format("Process ~w created port ~w", [Pid, Port]),
		  ?line exit(Port, die);
	      Other1 ->
		  test_server:fail({unexpected_message, Other1})
	  end,
    ?line receive
	      {'EXIT', Pid, normal} ->
		  ok;
	      Other2 ->
		  test_server:fail({unexpected_message, Other2})
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

no_trap_exit_process(ResultTo, Link, Config) ->
    ?line load_busy_driver(Config),
    ?line _Master = open_port({spawn, "busy_drv master"}, [eof]),
    ?line Slave = open_port({spawn, "busy_drv slave"}, [eof]),
    ?line case Link of
	      linked -> ok;
	      unlink -> unlink(Slave)
	  end,
    ?line Slave ! {self(), {command, "lock port"}},
    ?line ResultTo ! {self(), port_created, Slave},
    ?line Slave ! {self(), {command, "suspend me"}},
    ok.

%% Assuming the following scenario,
%%
%%  +---------------+		       +-----------+
%%  | process with  |		       |           |
%%  | trap_exit     |------------------| busy port |
%%  | (suspended)   |		       |	   |
%%  +---------------+		       +-----------+
%%
%% tests that the suspended process is scheduled runnable and
%% receives an 'EXIT' message if the port is killed.

trap_exit(suite) -> [];
trap_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Pid = fun_spawn(fun busy_port_exit_process/2, [self(), Config]),
    ?line receive
	      {Pid, port_created, Port} ->
		  io:format("Process ~w created port ~w", [Pid, Port]),
		  ?line unlink(Pid),
		  ?line {status, suspended} = process_info(Pid, status),
		  ?line exit(Port, die);
	      Other1 ->
		  test_server:fail({unexpected_message, Other1})
	  end,
    ?line receive
	      {Pid, ok} ->
		  ok;
	      Other2 ->
		  test_server:fail({unexpected_message, Other2})
	  end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

busy_port_exit_process(ResultTo, Config) ->
    ?line process_flag(trap_exit, true),
    ?line load_busy_driver(Config),
    ?line _Master = open_port({spawn, "busy_drv master"}, [eof]),
    ?line Slave = open_port({spawn, "busy_drv slave"}, [eof]),
    ?line Slave ! {self(), {command, "lock port"}},
    ?line ResultTo ! {self(), port_created, Slave},
    ?line Slave ! {self(), {command, "suspend me"}},
    receive
	{'EXIT', Slave, die} ->
	    ResultTo ! {self(), ok};
	Other ->
	    ResultTo ! {self(), {unexpected_message, Other}}
    end.

%% Tests that several processes suspended by a write to a busy port
%% will start running as soon as the port becamomes ready.
%% This should work even if some of the processes have terminated
%% in the meantime.

multiple_writers(suite) -> [];
multiple_writers(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line start_busy_driver(Config),
    ?line process_flag(trap_exit, true),

    %% Start the waiters and make sure they have blocked.
    ?line W1 = fun_spawn(fun quick_writer/0),
    ?line W2 = fun_spawn(fun quick_writer/0),
    ?line W3 = fun_spawn(fun quick_writer/0),
    ?line W4 = fun_spawn(fun quick_writer/0),
    ?line W5 = fun_spawn(fun quick_writer/0),
    ?line test_server:sleep(500),		% Make sure writers have blocked.

    %% Kill two of the processes.
    exit(W1, kill),
    receive {'EXIT', W1, killed} -> ok end,
    exit(W3, kill),
    receive {'EXIT', W3, killed} -> ok end,

    %% Unlock the port.  The surviving processes should be become runnable.
    ?line unlock_slave(),
    ?line wait_for([W2, W4, W5]),
    
    ?line test_server:timetrap_cancel(Dog),
    ok.

quick_writer() ->
    {Owner, Port} = get_slave(),
    Port ! {Owner, {command, "port to busy"}},
    Port ! {Owner, {command, "lock me"}},
    ok.

hard_busy_driver(Config) when is_list(Config) ->
    hs_test(Config, true).

soft_busy_driver(Config) when is_list(Config) ->
    hs_test(Config, false).

hs_test(Config, HardBusy) when is_list(Config) ->
    ?line Me = self(),
    ?line DrvName = case HardBusy of
			true -> 'hard_busy_drv';
			false -> 'soft_busy_drv'
		    end,
    ?line erl_ddll:start(),
    ?line Path = ?config(data_dir, Config),
    case erl_ddll:load_driver(Path, DrvName) of
	ok -> ok;
	{error, Error} ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ?line ?t:fail()
    end,

    ?line Port = open_port({spawn, DrvName}, []),
    
    NotSuspended = fun (Proc) ->
			   chk_not_value({status,suspended},
					 process_info(Proc, status))
		   end,
    NotBusyEnd = fun (Proc, Res, Time) ->
			 receive
			     {Port, caller, Proc} -> ok
			 after
			     500 -> exit(missing_caller_message)
			 end,
			 chk_value({return, true}, Res),
			 chk_range(0, Time, 100)
		 end,    
    ForceEnd = fun (Proc, Res, Time) ->
		       case HardBusy of
			   false ->
			       NotBusyEnd(Proc, Res, Time);
			   true ->
			       chk_value({error, notsup}, Res),
			       chk_range(0, Time, 100),
			       receive
				   Msg -> exit({unexpected_msg, Msg})
			       after
				   500 -> ok
			       end
		       end
	       end,
    BadArg = fun (_Proc, Res, Time) ->
		     chk_value({error, badarg}, Res),
		     chk_range(0, Time, 100)
	     end,

    %% Not busy

    %% Not busy; nosuspend option
    ?line hs_busy_pcmd(Port, [nosuspend], NotSuspended, NotBusyEnd),

    %% Not busy; force option
    ?line hs_busy_pcmd(Port, [force], NotSuspended, ForceEnd),

    %% Not busy; force and nosuspend option
    ?line hs_busy_pcmd(Port, [force, nosuspend], NotSuspended, ForceEnd),

    %% Not busy; no option
    ?line hs_busy_pcmd(Port, [], NotSuspended, NotBusyEnd),

    %% Not busy; bad option
    ?line hs_busy_pcmd(Port, [bad_option], NotSuspended, BadArg),


    %% Make busy
    ?line erlang:port_control(Port, $B, []),


    %% Busy; nosuspend option
    ?line hs_busy_pcmd(Port, [nosuspend], NotSuspended,
		       fun (_Proc, Res, Time) ->
			       chk_value({return, false}, Res),
			       chk_range(0, Time, 100),
			       receive
				   Msg -> exit({unexpected_msg, Msg})
			       after
				   500 -> ok
			       end
		       end),

    %% Busy; force option
    ?line hs_busy_pcmd(Port, [force], NotSuspended, ForceEnd),

    %% Busy; force and nosuspend option
    ?line hs_busy_pcmd(Port, [force, nosuspend], NotSuspended, ForceEnd),

    %% Busy; bad option
    ?line hs_busy_pcmd(Port, [bad_option], NotSuspended, BadArg),

    %% no option on busy port
    ?line hs_busy_pcmd(Port, [],
		       fun (Proc) ->
			       receive after 1000 -> ok end,
			       chk_value({status,suspended},
					 process_info(Proc, status)),

			       %% Make not busy
			       erlang:port_control(Port, $N, [])
		       end,
		       fun (_Proc, Res, Time) ->
			       chk_value({return, true}, Res),
			       chk_range(1000, Time, 2000)
		       end),

    ?line true = erlang:port_close(Port),
    ?line ok = erl_ddll:unload_driver(DrvName),
    ?line ok = erl_ddll:stop(),
    ?line ok.

hs_busy_pcmd(Prt, Opts, StartFun, EndFun) ->
    Tester = self(),
    P = spawn_link(fun () ->
			   erlang:yield(),
			   Tester ! {self(), doing_port_command},
			   Start = os:timestamp(),
			   Res = try {return,
				      erlang:port_command(Prt, [], Opts)}
				 catch Exception:Error -> {Exception, Error}
				 end,
			   End = os:timestamp(),
			   Time = round(timer:now_diff(End, Start)/1000),
			   Tester ! {self(), port_command_result, Res, Time}
		   end),
    receive
	{P, doing_port_command} ->
	    ok
    end,
    StartFun(P),
    receive
	{P, port_command_result, Res, Time} ->
	    EndFun(P, Res, Time)
    end.

%%% Utilities.

chk_range(Min, Val, Max) when Min =< Val, Val =< Max ->
    ok;
chk_range(Min, Val, Max) ->
    exit({bad_range, Min, Val, Max}).

chk_value(Exp, Exp) ->
    ok;
chk_value(Exp, Val) ->
    exit({unexpected_value, Val, expected, Exp}).

chk_not_value(NotExp, NotExp) ->
    exit({unexpected_not_value, NotExp});
chk_not_value(_, _) ->
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

fun_spawn(Fun) ->
    fun_spawn(Fun, []).

fun_spawn(Fun, Args) ->
    spawn_link(erlang, apply, [Fun, Args]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% These routines provide a port which will become busy when the
%% the first message is sent to it.  The unlock_slave/0 function can
%% be called (from another process) to make the port non-busy.
%%
%% Typical usage:
%%
%% start_busy_driver(Config)		Load driver; start server.
%%
%% 		        P r o c e s s   O n e
%% {Owner, Port} = get_slave()	O	Obtain port and its owner.
%% Port ! {Owner, {command, List}}	Send to port (will not block
%%					but port will become busy).
%% Port ! {Owner, {command, List}}	Will block the process.
%%
%% 		        P r o c e s s   T w o
%% unlock_slave()			Set port to non-busy.  Process One
%%				        will continue executing.  Further
%%					writes to the port will not block.
%%
%% Any process can call busy_drv:lock() to lock the port again.
%%
%% Note: This module must be used in an installed test suite (outside of
%% clearcase).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_busy_driver(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line erl_ddll:start(),
    case erl_ddll:load_driver(DataDir, "busy_drv") of
	ok -> ok;
	{error, Error} ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ?line ?t:fail()
    end.

%%% Interface functions.

start_busy_driver(Config) when is_list(Config) ->
    ?line Pid = spawn_link(?MODULE, init, [Config, self()]),
    ?line receive
	      {Pid, started} ->
		  ok;
	      Other ->
		  test_server:fail({unexpected_message, Other})
	  end.

unlock_slave() ->
    command(unlock).

get_slave() ->
    ?line command(get_slave).

%% Internal functions.

command(Msg) ->
    ?line whereis(busy_drv_server) ! {self(), Msg},
    ?line receive
	      {busy_drv_reply, Reply} ->
		  Reply
    end.

%%% Server.

init(Config, ReplyTo) ->
    register(busy_drv_server, self()),
    load_busy_driver(Config),
    Driver = "busy_drv",
    Master = open_port({spawn, Driver++" master"}, []),
    Slave = open_port({spawn, Driver++" slave"}, []),
    ReplyTo ! {self(), started},
    loop(Master, Slave).

loop(Master, Slave) ->
    receive
	{Pid, get_master} ->
	    Pid ! {busy_drv_reply, Master},
	    loop(Master, Slave);
	{Pid, get_slave} ->
	    Pid ! {busy_drv_reply, {self(), Slave}},
	    loop(Master, Slave);
	{Pid, unlock} ->
	    Master ! {self(), {command, "u"}},
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, lock} ->
	    Master ! {self(), {command, "l"}},
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, {port_command,Data}} ->
	    erlang:port_command(Slave, Data),
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, stop} ->
	    Pid ! {busy_drv_reply, ok}
    end.
