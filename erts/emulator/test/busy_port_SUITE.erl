%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(busy_port_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,end_per_testcase/2,
	 io_to_busy/1, message_order/1, send_3/1, 
	 system_monitor/1, no_trap_exit/1,
	 no_trap_exit_unlinked/1, trap_exit/1, multiple_writers/1,
	 hard_busy_driver/1, soft_busy_driver/1]).

-compile(export_all).

-include_lib("test_server/include/test_server.hrl").

%% Internal exports.
-export([init/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [io_to_busy, message_order, send_3, system_monitor,
     no_trap_exit, no_trap_exit_unlinked, trap_exit,
     multiple_writers, hard_busy_driver, soft_busy_driver,
     scheduling_delay_busy,scheduling_delay_busy_nosuspend,
     scheduling_busy_link].

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

end_per_testcase(_Case, Config) when is_list(Config) ->
    case whereis(busy_drv_server) of
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    Ref = monitor(process, Pid),
	    unlink(Pid),
	    exit(Pid, kill),
	    receive
		{'DOWN',Ref,process,Pid,_} ->
		    ok
	    end
    end,
    Config.

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
    case process_info(Writer, [status,current_function]) of
	[{status,suspended},{current_function,{erlang,send,2}}] -> ok;
	[{status,suspended},{current_function,{erlang,bif_return_trap,_}}] -> ok
    end,
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
    (catch port_command(Slave, "set_me_busy")),
    (catch port_command(Slave, "hello")),
    (catch port_command(Slave, "hello again")),
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
    receive after 100 -> ok end, % ensure command reached port
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
		  (catch port_command(Slave, "set busy")),
		  receive {Parent,alpha} -> ok end,
		  (catch port_command(Slave, "busy")),
		  (catch port_command(Slave, "free")),
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
    ?line port_command(Master, "u"),
    ?line {Busy,beta} = rec(Void),
    ?line Void = rec(Void),
    ?line _NewMonitor = erlang:system_monitor(OldMonitor),
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
    ?line (catch port_command(Slave, "lock port")),
    ?line ResultTo ! {self(), port_created, Slave},
    ?line (catch port_command(Slave, "suspend me")),
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
    ?line (catch port_command(Slave, "lock port")),
    ?line ResultTo ! {self(), port_created, Slave},
    ?line (catch port_command(Slave, "suspend me")),
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
    (catch port_command(Port, "port to busy")),
    (catch port_command(Port, "lock me")),
    ok.

hard_busy_driver(Config) when is_list(Config) ->
    hs_test(Config, true).

soft_busy_driver(Config) when is_list(Config) ->
    hs_test(Config, false).

hs_test(Config, HardBusy) when is_list(Config) ->
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
			   Start = erlang:monotonic_time(micro_seconds),
			   Res = try {return,
				      port_command(Prt, [], Opts)}
				 catch Exception:Error -> {Exception, Error}
				 end,
			   End = erlang:monotonic_time(micro_seconds),
			   Time = round((End - Start)/1000),
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

scheduling_delay_busy(Config) ->
    
    Scenario = 
	[{1,{spawn,[{var,drvname},undefined]}},
	 {2,{call,[{var,1},open_port]}},
	 {3,{spawn,[{var,2},{var,1}]}},
	 {0,{ack,[{var,1},{busy,1,250}]}},
	 {0,{cast,[{var,3},{command,2}]}},
	 [{0,{cast,[{var,3},{command,I}]}} 
	  || I <- lists:seq(3,50)],
	 {0,{cast,[{var,3},take_control]}},
	 {0,{cast,[{var,1},{new_owner,{var,3}}]}},
	 {0,{cast,[{var,3},close]}},
	 {0,{timer,sleep,[300]}},
	 {0,{erlang,port_command,[{var,2},<<$N>>,[force]]}},
	 [{0,{cast,[{var,1},{command,I}]}} 
	  || I <- lists:seq(101,127)]
	 ,{10,{call,[{var,3},get_data]}}
	 ],

    Validation = [{seq,10,lists:seq(1,50)}],

    port_scheduling(Scenario,Validation,?config(data_dir,Config)).

scheduling_delay_busy_nosuspend(Config) ->

    Scenario = 
	[{1,{spawn,[{var,drvname},undefined]}},
	 {2,{call,[{var,1},open_port]}},
	 {0,{cast,[{var,1},{command,1,100}]}},
	 {0,{cast,[{var,1},{busy,2}]}},
	 {0,{timer,sleep,[200]}}, % ensure reached port
	 {10,{call,[{var,1},{command,3,[nosuspend]}]}},
	 {0,{timer,sleep,[200]}},
	 {0,{erlang,port_command,[{var,2},<<$N>>,[force]]}},
	 {0,{cast,[{var,1},close]}},
	 {20,{call,[{var,1},get_data]}}
	 ],

    Validation = [{eq,10,nosuspend},{seq,20,[1,2]}],

    port_scheduling(Scenario,Validation,?config(data_dir,Config)).

scheduling_busy_link(Config) ->
    
    Scenario = 
	[{1,{spawn,[{var,drvname},undefined]}},
	 {2,{call,[{var,1},open_port]}},
	 {3,{spawn,[{var,2},{var,1}]}},
	 {0,{cast,[{var,1},unlink]}},
	 {0,{cast,[{var,1},{busy,1}]}},
	 {0,{cast,[{var,1},{command,2}]}},
	 {0,{cast,[{var,1},link]}},
	 {0,{timer,sleep,[1000]}},
	 {0,{ack,[{var,3},take_control]}},
	 {0,{cast,[{var,1},{new_owner,{var,3}}]}},
	 {0,{cast,[{var,3},close]}},
	 {10,{call,[{var,3},get_data]}},
	 {20,{call,[{var,1},get_exit]}}
	 ],

    Validation = [{seq,10,[1]},
		  {seq,20,[{'EXIT',noproc}]}],

    port_scheduling(Scenario,Validation,?config(data_dir,Config)).

process_init(DrvName,Owner) ->
    process_flag(trap_exit,true),
    process_loop(DrvName,Owner, {[],[]}).

process_loop(DrvName,undefined,Data) when is_list(DrvName) ->
    process_loop(DrvName,[binary],Data);
process_loop(DrvName,PortOpts,Data) when is_list(DrvName) ->
    receive
	{call,open_port,P} ->
	    Port = open_port({spawn, DrvName}, PortOpts),
	    send(P,Port),
	    process_loop(Port,self(),Data)
    end;
process_loop(Port,undefined,Data) ->
    receive
	{cast,{new_owner,Pid}} ->
	    pal("NewOwner: ~p",[Pid]),
	    process_loop(Port,Pid,Data)
    end;
process_loop(Port,Owner,{Data,Exit} = DE) ->
    receive
	{Port,connected} ->
	    pal("Connected",[]),
	    process_loop(Port,undefined,DE);	
	{Port,{data,NewData}} ->
	    pal("Got: ~p",[NewData]),
	    receive
		{Port,closed} ->
		    process_loop(Port,Owner,{Data ++ [NewData],Exit})
	    after 2000 ->
		    exit(did_not_get_port_close)
	    end;
	{'EXIT',Port,Reason} = Exit ->
	    pal("Exit: ~p",[Exit]),
	    process_loop(Port,Owner,{Data, Exit ++ [[{'EXIT',Reason}]]});
	{'EXIT',_Port,_Reason} = Exit ->
	    pal("Exit: ~p",[Exit]);
	{call,Msg,P} ->
	    case handle_msg(Msg,Port,Owner,DE) of
		{Reply,NewOwner,NewData} ->
		    send(P,Reply),
		    process_loop(Port,NewOwner,NewData);
		Reply ->
		    send(P,Reply),
		    process_loop(Port,Owner,DE)
	    end;
	{ack,Msg,P} ->
	    send(P,ok),
	    case handle_msg(Msg,Port,Owner,DE) of
		{_Reply,NewOwner,NewData} ->
		    process_loop(Port,NewOwner,NewData);
		_Reply ->
		    process_loop(Port,Owner,DE)
	    end;
	{cast,Msg} when is_atom(Msg) orelse element(1,Msg) /= new_owner ->
	    case handle_msg(Msg,Port,Owner,DE) of
		{_Reply,NewOwner,NewData} ->
		    process_loop(Port,NewOwner,NewData);
		_ ->
		    process_loop(Port,Owner,DE)
	    end
    end.

handle_msg({busy,Value,Delay},Port,Owner,_Data) ->
    pal("Long busy: ~p",[Value]),
    send(Port,{Owner,{command,<<$L,Value:32,(round(Delay/100))>>}});
handle_msg({busy,Value},Port,Owner,_Data)  ->
    pal("Busy: ~p",[Value]),
    send(Port,{Owner,{command,<<$B,Value:32>>}});
handle_msg({command,Value},Port,Owner,_Data)  ->
    pal("Short: ~p",[Value]),
    send(Port,{Owner,{command,<<$C,Value:32>>}});
handle_msg({command,Value,Delay},Port,Owner,_Data) when is_integer(Delay) ->
    pal("Long: ~p",[Value]),
    send(Port,{Owner,{command,<<$D,Value:32,(round(Delay/100))>>}});
handle_msg({command,Value,Opts},Port,Owner,_Data)  ->
    pal("Short Opt: ~p",[Value]),
    send(Port,{Owner,{command,<<$C,Value:32>>}},Opts);
handle_msg({command,Value,Opts,Delay},Port,Owner,_Data)  ->
    pal("Long Opt: ~p",[Value]),
    send(Port,{Owner,{command,<<$D,Value:32,(round(Delay/100))>>}},Opts);
handle_msg(take_control,Port,Owner,Data)  ->
    pal("Connect: ~p",[self()]),
    send(Port,{Owner, {connect, self()}}),
    {undefined,self(),Data};
handle_msg(unlink,Port,_Owner,_Data) ->
    pal("Unlink:",[]),
    erlang:unlink(Port);
handle_msg(link,Port,_Owner,_Data) ->
    pal("Link:",[]),
    erlang:link(Port);
handle_msg(close,Port,Owner,_Data)  ->
    pal("Close",[]),
    send(Port,{Owner,close});
handle_msg(get_data,Port,_Owner,{[],_Exit})  ->
    %% Wait for data if it has not arrived yet
    receive
	{Port,{data,Data}} ->
	    Data
    after 2000 ->
	    pal("~p",[erlang:process_info(self())]),
	    exit(did_not_get_port_data)
    end;
handle_msg(get_data,_Port,Owner,{Data,Exit})  ->
    pal("GetData",[]),
    {hd(Data),Owner,{tl(Data),Exit}};
handle_msg(get_exit,Port,_Owner,{_Data,[]})  ->
    %% Wait for exit if it has not arrived yet
    receive
	{'EXIT',Port,Reason} ->
	    [{'EXIT',Reason}]
    after 2000 ->
	    pal("~p",[erlang:process_info(self())]),
	    exit(did_not_get_port_exit)
    end;
handle_msg(get_exit,_Port,Owner,{Data,Exit}) ->
    {hd(Exit),Owner,{Data,tl(Exit)}}.

    

call(Pid,Msg) ->
    pal("call(~p,~p)",[Pid,Msg]),
    send(Pid,{call,Msg,self()}),
    receive
	Ret ->
	    Ret
    end.
ack(Pid,Msg) ->
    pal("ack(~p,~p)",[Pid,Msg]),
    send(Pid,{ack,Msg,self()}),
    receive
	Ret ->
	    Ret
    end.

cast(Pid,Msg) ->
    pal("cast(~p,~p)",[Pid,Msg]),
    send(Pid,{cast,Msg}).

send(Pid,Msg) ->
    erlang:send(Pid,Msg).
send(Prt,Msg,Opts) ->
    erlang:send(Prt,Msg,Opts).


port_scheduling(Scenario,Validation,Path) ->
    DrvName = "scheduling_drv",
    erl_ddll:start(),
    case erl_ddll:load_driver(Path, DrvName) of
	ok -> ok;
	{error, Error} ->
	    io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ?line ?t:fail()
    end,

    Data = run_scenario(lists:flatten(Scenario),[{drvname,DrvName}]),
    ok = validate_scenario(Data,Validation).


run_scenario([{V,{Module,Cmd,Args}}|T],Vars) ->
    Res = run_command(Module,Cmd,
		      replace_args(Args,Vars)),
    run_scenario(T,[{V,Res}|Vars]);
run_scenario([{V,{Cmd,Args}}|T],Vars) ->
    run_scenario([{V,{?MODULE,Cmd,Args}}|T],Vars);
run_scenario([],Vars) ->
    Vars.

run_command(_M,spawn,{Args,Opts}) ->
    Pid = spawn_opt(fun() -> apply(?MODULE,process_init,Args) end,[link|Opts]),
    pal("spawn(~p): ~p",[Args,Pid]),
    Pid;
run_command(M,spawn,Args) ->
    run_command(M,spawn,{Args,[]});
run_command(Mod,Func,Args) ->
    erlang:display({{Mod,Func,Args}, erlang:system_time(micro_seconds)}),
    apply(Mod,Func,Args).

validate_scenario(Data,[{print,Var}|T]) ->
    pal("Val: ~p",[proplists:get_value(Var,Data)]),
    validate_scenario(Data,T);
validate_scenario(Data,[{eq,Var,Value}|T]) ->
    case proplists:get_value(Var,Data) of
	Value ->
	    validate_scenario(Data,T);
	Else ->
	    exit({eq_return,Value,Else})
    end;
validate_scenario(Data,[{neq,Var,Value}|T]) ->
    case proplists:get_value(Var,Data) of
	Value ->
	    exit({neq_return,Value});
	_Else ->
	    validate_scenario(Data,T)
    end;
validate_scenario(Data,[{seq,Var,Seq}|T]) ->
    try
	validate_sequence(proplists:get_value(Var,Data),Seq)
    catch _:{validate_sequence,NotFound} ->
	    exit({validate_sequence,NotFound,Data})
    end,
    validate_scenario(Data,T);
validate_scenario(_,[]) ->
    ok.

validate_sequence(Data,Validation) when is_binary(Data) ->
    validate_sequence(binary_to_list(Data),Validation);	    
validate_sequence([H|R],[H|T]) ->
    validate_sequence(R,T);
validate_sequence([_|R],Seq) ->
    validate_sequence(R,Seq);
validate_sequence(_,[]) ->
    ok;
validate_sequence([],NotFound) ->
    exit({validate_sequence,NotFound}).

replace_args({var,Var},Vars) ->
    proplists:get_value(Var,Vars);
replace_args([H|T],Vars) ->
    [replace_args(H,Vars)|replace_args(T,Vars)];
replace_args([],_Vars) ->
    [];
replace_args(Tuple,Vars) when is_tuple(Tuple) ->
    list_to_tuple(replace_args(tuple_to_list(Tuple),Vars));
replace_args(Else,_Vars) ->
    Else.

pal(_F,_A) -> ok.
%pal(Format,Args) ->
%    ct:pal("~p "++Format,[self()|Args]).
%    erlang:display(lists:flatten(io_lib:format("~p "++Format,[self()|Args]))).
			

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
	    port_command(Master, "u"),
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, lock} ->
	    port_command(Master, "l"),
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, {port_command,Data}} ->
	    erlang:port_command(Slave, Data),
	    Pid ! {busy_drv_reply, ok},
	    loop(Master, Slave);
	{Pid, stop} ->
	    Pid ! {busy_drv_reply, ok}
    end.
