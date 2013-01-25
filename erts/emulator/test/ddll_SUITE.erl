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

-module(ddll_SUITE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Checks if the dynamic driver and linker loader works.
%%%
%%% These tests can only be run installed (outside clearcase).
%%%
%%% XXX In this suite is missing test cases for reference counts
%%% and that drivers are unloaded when their processes die.
%%% (For me to add :-)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, ddll_test/1, errors/1,
	 reference_count/1,
	 kill_port/1, dont_kill_port/1]).
-export([unload_on_process_exit/1, delayed_unload_with_ports/1, 
	 unload_due_to_process_exit/1,
	 no_unload_due_to_process_exit/1, no_unload_due_to_process_exit_2/1,
	 unload_reload_thingie/1, unload_reload_thingie_2/1,
	 unload_reload_thingie_3/1, reload_pending/1, reload_pending_kill/1,
	 load_fail_init/1,
	 reload_pending_fail_init/1,
	 more_error_codes/1, forced_port_killing/1, 
	 no_trap_exit_and_kill_ports/1,
	 monitor_demonitor/1, monitor_demonitor_load/1, new_interface/1, 
	 lock_driver/1]).

% Private exports
-export([echo_loader/2, nice_echo_loader/2 ,properties/1, load_and_unload/1]).

-import(ordsets, [subtract/2]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [ddll_test, errors, reference_count, kill_port,
     dont_kill_port, properties, load_and_unload,
     unload_on_process_exit, delayed_unload_with_ports,
     unload_due_to_process_exit,
     no_unload_due_to_process_exit,
     no_unload_due_to_process_exit_2, unload_reload_thingie,
     unload_reload_thingie_2, unload_reload_thingie_3,
     reload_pending, load_fail_init,
     reload_pending_fail_init, reload_pending_kill,
     more_error_codes, forced_port_killing,
     no_trap_exit_and_kill_ports, monitor_demonitor,
     monitor_demonitor_load, new_interface, lock_driver].

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


unload_on_process_exit(suite) ->
    [];
unload_on_process_exit(doc) ->
    ["Check that the driver is unloaded on process exit"];
unload_on_process_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Parent = self(),
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      erl_ddll:try_load(Path, echo_drv, []),
			      Parent ! gone,
			      receive go -> ok end,
			      erl_ddll:loaded_drivers(),
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    ?line false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Pid ! go,
    ?line receive
	gone -> ok
    end,
    ?line true = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Pid ! go,
    ?line receive
	{'DOWN', Ref, process, Pid, banan} ->
	    ok
    end,
    receive after 500 -> ok end,
    ?line false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    ?line test_server:timetrap_cancel(Dog),
    ok.

delayed_unload_with_ports(suite) ->
    [];
delayed_unload_with_ports(doc) ->
    ["Check that the driver is unloaded when the last port is closed"];
delayed_unload_with_ports(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:try_load(Path, echo_drv, []),
    ?line erl_ddll:try_load(Path, echo_drv, []),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    ?line 1 = erl_ddll:info(echo_drv, port_count),
    ?line Port2 = open_port({spawn, echo_drv}, [eof]),
    ?line 2 = erl_ddll:info(echo_drv, port_count),
    ?line {ok,pending_process} = erl_ddll:try_unload(echo_drv,[{monitor, pending_driver}]),
    ?line {ok,pending_driver,Ref} = erl_ddll:try_unload(echo_drv,[{monitor, pending_driver}]),
    ?line ok = receive _ -> false after 0 -> ok end,
    ?line Port ! {self(), close},
    ?line ok = receive {Port,closed} -> ok after 1000 -> false end,
    ?line 1 = erl_ddll:info(echo_drv, port_count),
    ?line Port2 ! {self(), close},
    ?line ok = receive {Port2,closed} -> ok after 1000 -> false end,
    ?line ok = receive {'DOWN', Ref, driver, echo_drv, unloaded} -> ok after 1000 -> false end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

unload_due_to_process_exit(suite) ->
    [];
unload_due_to_process_exit(doc) ->
    ["Check that the driver with ports is unloaded on process exit"];
unload_due_to_process_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

no_unload_due_to_process_exit(suite) ->
    [];
no_unload_due_to_process_exit(doc) ->
    ["Check that a driver with driver loaded in another process is not unloaded on process exit"];
no_unload_due_to_process_exit(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line ok = unload_expect_fast(echo_drv,[]),
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

no_unload_due_to_process_exit_2(suite) ->
    [];
no_unload_due_to_process_exit_2(doc) ->
    ["Check that a driver with open ports in another process is not unloaded on process exit"];
no_unload_due_to_process_exit_2(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line erlang:port_close(Port),
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

unload_reload_thingie(suite) ->
    [];
unload_reload_thingie(doc) ->
    ["Check delayed unload and reload"];
unload_reload_thingie(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded_only}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      spawn(F3),
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok,pending_driver,Ref3} = erl_ddll:try_unload(echo_drv,[{monitor,pending}]),
    ?line Ref4 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ?line ok = receive {'DOWN',Ref4, driver,echo_drv,load_cancelled} -> ok after 1000 -> false end,
    ?line {ok,already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line ok = receive {'UP',Ref3, driver,echo_drv,unload_cancelled} -> ok after 1000 -> false end,
    ?line Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line [{Parent,1}] = erl_ddll:info(echo_drv, processes),
    ?line 0 = erl_ddll:info(echo_drv, port_count),
    ?line ok = unload_expect_fast(echo_drv,[{monitor,pending}]),
    ?line ok = receive 
		   {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
	       after 300 -> error 
	       end,
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

unload_reload_thingie_2(suite) ->
    [];
unload_reload_thingie_2(doc) ->
    ["Check delayed unload and reload"];
unload_reload_thingie_2(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded_only}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      spawn(F3),
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok,pending_driver,Ref3} = erl_ddll:try_load(Path,echo_drv,[{monitor,pending_driver},{reload,pending_driver}]),
    ?line Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive {'DOWN',Ref4, driver,echo_drv,unloaded} -> ok after 1000 -> false end,
    ?line ok = receive {'UP',Ref3, driver,echo_drv,loaded} -> ok after 1000 -> false end,
    ?line [{Parent,1}] = erl_ddll:info(echo_drv, processes),
    ?line 0 = erl_ddll:info(echo_drv, port_count),
    ?line ok = receive 
		   {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
	       after 300 -> error 
	       end,
    ?line ok = unload_expect_fast(echo_drv,[{monitor,pending}]),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

unload_reload_thingie_3(suite) ->
    [];
unload_reload_thingie_3(doc) ->
    ["Check delayed unload and reload failure"];
unload_reload_thingie_3(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      spawn(F3),
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok,pending_driver,Ref3} = erl_ddll:try_load(filename:join([Path,"skrumpf"]),echo_drv,[{monitor,pending_driver},{reload,pending_driver}]),
    ?line Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive 
		   {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
	       after 300 -> error 
	       end,
    ?line ok = receive {'DOWN',Ref4,driver,echo_drv,unloaded} -> ok after 300 -> false end,
    ?line ok = receive 
		   {'DOWN',Ref3, driver,echo_drv,{load_failure,_}} -> ok 
	       after 1000 -> false 
	       end,
    ?line {'EXIT',_} = (catch erl_ddll:info(echo_drv, port_count)),
    ?line {error, not_loaded} = erl_ddll:try_unload(echo_drv,[{monitor,pending}]),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

reload_pending(suite) -> [];
reload_pending(doc) -> ["Reload a driver that is pending on a user"];
reload_pending(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      Parent ! opened,
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ?line receive opened -> ok end,
    ?line {error, pending_process} = 
	erl_ddll:try_load(Path, echo_drv,
			  [{reload,pending_driver},
			   {monitor,pending_driver}]),
    ?line {ok, pending_process, Ref3} = 
	erl_ddll:try_load(Path, echo_drv,
			  [{reload,pending},
			   {monitor,pending}]),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive Y -> {error, Y} after 300 -> ok end,
    ?line erlang:port_close(Port),
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line ok = receive {'UP', Ref3, driver, echo_drv, loaded} -> ok after 300 -> error end,
    [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    ?line ok = receive Z -> {error, Z} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

load_fail_init(suite) -> [];
load_fail_init(doc) -> ["Tests failure in the init in driver struct."];
load_fail_init(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line PathFailing = ?config(priv_dir, Config),
    ?line [_|_] = AllFailInits = filelib:wildcard("echo_drv_fail_init.*",Path),
    ?line lists:foreach(fun(Name) ->
				Src = filename:join([Path,Name]),
				Ext = filename:extension(Name),
				Dst =filename:join([PathFailing,"echo_drv"++Ext]),
				file:delete(Dst),
				{ok,_} = file:copy(Src,Dst)
			end,
			AllFailInits),
    ?line [_|_] = filelib:wildcard("echo_drv.*",PathFailing),
    ?line {error, driver_init_failed} = erl_ddll:try_load(PathFailing, 
							  echo_drv,
							  [{monitor,pending}]),
    ?line ok = receive XX ->
		       {unexpected,XX}
	       after 300 ->
		       ok
	       end,
    ?line test_server:timetrap_cancel(Dog),
    ok.


reload_pending_fail_init(suite) -> [];
reload_pending_fail_init(doc) -> ["Reload a driver that is pending but init fails"];
reload_pending_fail_init(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line PathFailing = ?config(priv_dir, Config),
    ?line [_|_] = AllFailInits = filelib:wildcard("echo_drv_fail_init.*",Path),
    ?line lists:foreach(fun(Name) ->
				Src = filename:join([Path,Name]),
				Ext = filename:extension(Name),
				Dst =filename:join([PathFailing,"echo_drv"++Ext]),
				file:delete(Dst),
				{ok,_} = file:copy(Src,Dst)
			end,
			AllFailInits),
    ?line [_|_] = filelib:wildcard("echo_drv.*",PathFailing),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      Parent ! opened,
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ?line receive opened -> ok end,
    ?line {ok, pending_process, Ref3} = 
	erl_ddll:try_load(PathFailing, echo_drv,
			  [{reload,pending},
			   {monitor,pending}]),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive Y -> {error, Y} after 300 -> ok end,
    ?line erlang:port_close(Port),
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line ok = receive {'DOWN', Ref3, driver, echo_drv, {load_failure,driver_init_failed}} -> ok  after 300 -> error end,
    ?line {'EXIT',{badarg,_}} = (catch erl_ddll:info(echo_drv,processes)),

    ?line ok = receive Z -> {error, Z} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

reload_pending_kill(suite) -> [];
reload_pending_kill(doc) -> ["Reload a driver with kill_ports option "
			     "that is pending on a user"];
reload_pending_kill(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line OldFlag = process_flag(trap_exit,true),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      process_flag(trap_exit,true),
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, [{driver_options,[kill_ports]}]),
			      spawn(F3),
			      receive go -> ok end,
			      Port = open_port({spawn, echo_drv}, [eof]),
			      Port2 = open_port({spawn, echo_drv}, [eof]),
			      Parent ! opened,
			      receive go -> ok end,
			      receive 
				  {'EXIT', Port2, driver_unloaded} ->
				      Parent ! first_exit 
			      end,
			      receive 
				  {'EXIT', Port, driver_unloaded} ->
				      Parent ! second_exit 
			      end,
			      receive go -> ok end,
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, [{driver_options,[kill_ports]}]),
    ?line {error,inconsistent} = erl_ddll:try_load(Path, echo_drv, []),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ?line receive opened -> ok end,
    ?line {error, pending_process} = 
	erl_ddll:try_load(Path, echo_drv,
			  [{driver_options,[kill_ports]},
			   {reload,pending_driver},
			   {monitor,pending_driver}]),
    ?line {ok, pending_process, Ref3} = 
	erl_ddll:try_load(Path, echo_drv,
			  [{driver_options,[kill_ports]},
			   {reload,pending},
			   {monitor,pending}]),
    ?line ok =  receive 
		    {'EXIT', Port, driver_unloaded} ->
			ok 
		after 300 -> error
		end,
    Pid ! go,
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line ok = receive {'UP', Ref3, driver, echo_drv, loaded} -> ok after 300 -> error end,
    ?line [_,_] = erl_ddll:info(echo_drv,processes),
    ?line ok = receive first_exit -> ok after 300 -> error end,
    ?line ok = receive second_exit -> ok after 300 -> error end,
    ?line 0 = erl_ddll:info(echo_drv,port_count), 
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive Y -> {error, Y} after 300 -> ok end,
    ?line Port2 = open_port({spawn, echo_drv}, [eof]),
    ?line true = is_port(Port2),
    [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    ?line 1 = erl_ddll:info(echo_drv,port_count), 
    ?line erlang:port_close(Port2),
    ?line ok = receive {'EXIT', Port2, normal} -> ok after 300 -> error end,
    ?line 0 = erl_ddll:info(echo_drv,port_count), 
    ?line [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    ?line Port3 = open_port({spawn, echo_drv}, [eof]),
    ?line {ok, pending_driver, Ref4} = 
	erl_ddll:try_unload(echo_drv,[{monitor,pending_driver}]),
    ?line ok =  receive 
		    {'EXIT', Port3, driver_unloaded} ->
			ok 
		after 300 -> error
		end,
    ?line ok = receive {'DOWN', Ref4, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    io:format("Port = ~w, Port2 = ~w, Port3 = ~w~n",[Port,Port2,Port3]),
    ?line ok = receive Z -> {error, Z} after 300 -> ok end,
    ?line process_flag(trap_exit,OldFlag),
    ?line test_server:timetrap_cancel(Dog),
    ok.


more_error_codes(suite) ->
    [];
more_error_codes(doc) ->
    ["Some more error code checking"];
more_error_codes(Config) when is_list(Config) ->
    ?line {error,Err} = erl_ddll:try_load("./echo_dr",echo_dr,[]),
    ?line true = is_list(erl_ddll:format_error(Err)),
    ?line true = is_list(erl_ddll:format_error(not_loaded)),
    ok.

forced_port_killing(suite) ->
    [];
forced_port_killing(doc) ->
    ["Check kill_ports option to try_unload "];
forced_port_killing(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line OldFlag=process_flag(trap_exit,true),
    ?line Parent = self(),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line spawn(F3),
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    ?line Port2 = open_port({spawn, echo_drv}, [eof]),
    ?line {ok, pending_driver, Ref1} = 
	erl_ddll:try_unload(echo_drv,[{monitor,pending_driver},kill_ports]),
    ?line ok = receive 
		   {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
	       after 300 -> error 
	       end,
    ?line ok = receive {'EXIT',Port,driver_unloaded} -> ok after 300 -> false end,
    ?line ok = receive {'EXIT',Port2,driver_unloaded} -> ok after 300 -> false end,
    ?line ok = receive {'DOWN',Ref1, driver, echo_drv, unloaded} -> ok after 300 -> false end,
    ?line process_flag(trap_exit,OldFlag),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

no_trap_exit_and_kill_ports(suite) ->
    [];
no_trap_exit_and_kill_ports(doc) ->
    ["Check delayed unload and reload with no trap_exit"];
no_trap_exit_and_kill_ports(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line Parent = self(),
    ?line OldFlag=process_flag(trap_exit,true),
    ?line F3 = fun() -> 
		       Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
		       receive X -> Parent ! {got,X} end 
	       end,
    ?line Pid = spawn(fun() ->
			      process_flag(trap_exit,false),
			      receive go -> ok end,
			      {ok, loaded} = erl_ddll:try_load(Path, echo_drv, 
							       [{driver_options,[kill_ports]}]),
			      spawn(F3),
			      receive go -> ok end,
			      _Port = open_port({spawn, echo_drv}, [eof]),
			      _Port2 = open_port({spawn, echo_drv}, [eof]),
			      exit(banan)
		      end),
    ?line Ref = erlang:monitor(process,Pid),
    Pid ! go,
    ?line {ok,Ref2} = receive 
			  R when is_reference(R) -> {ok,R}; 
			  Other -> {error, Other} 
		      after 500 -> {error, timeout} 
		      end,
    ?line {error, inconsistent} = erl_ddll:try_load(Path, echo_drv, []),
    ?line MyPort = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ?line ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ?line ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ?line ok = receive {'EXIT',MyPort,driver_unloaded} -> ok after 300 -> error end,
    ?line process_flag(trap_exit,OldFlag),
    ?line test_server:timetrap_cancel(Dog),
    ok.

monitor_demonitor(suite) ->
    [];
monitor_demonitor(doc) ->
    ["Check monitor and demonitor of drivers"];
monitor_demonitor(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line erl_ddll:try_load(Path, echo_drv, []),
    ?line Ref = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line Self = self(),
    ?line [{Self,1}] = erl_ddll:info(echo_drv,awaiting_unload),
    ?line true = erl_ddll:demonitor(Ref),
    ?line [] = erl_ddll:info(echo_drv,awaiting_unload),
    ?line erl_ddll:try_unload(echo_drv,[]),
    ?line ok = receive _ -> error after 300 -> ok end,
    ?line test_server:timetrap_cancel(Dog),
    ok.

monitor_demonitor_load(suite) ->
    [];
monitor_demonitor_load(doc) ->
    ["Check monitor/demonitor of driver loading"];
monitor_demonitor_load(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line {ok,loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    ?line Ref = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ?line ok = receive {'UP',Ref,driver,echo_drv,loaded} -> ok after 500 -> error end,
    ?line {ok, pending_driver} = erl_ddll:try_unload(echo_drv,[]),
    ?line Ref2 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ?line ok = receive {'DOWN',Ref2,driver,echo_drv,load_cancelled} -> ok after 0 -> error end,
    ?line {ok,already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ?line {ok, pending_driver} = 
	erl_ddll:try_load(Path, echo_drv, [{reload,pending_driver}]),
    ?line Ref3 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ?line Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line ok = receive _ -> error after 300 -> ok end,
    ?line Self = self(),
    ?line [{Self,1}] = erl_ddll:info(echo_drv,awaiting_load),
    ?line true = erl_ddll:demonitor(Ref3),
    ?line [] = erl_ddll:info(echo_drv,awaiting_load),
    ?line erlang:port_close(Port),
    ?line ok = receive {'DOWN',Ref4,driver,echo_drv,unloaded} -> ok after 300 -> error end,
    ?line ok = receive _ -> error after 300 -> ok end,
    ?line ok = unload_expect_fast(echo_drv,[]),
    ?line test_server:timetrap_cancel(Dog),
    ok.

new_interface(suite) ->
    [];
new_interface(doc) ->
    ["Test the new load/unload/reload interface"];
new_interface(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    % Typical scenario
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line Port = open_port({spawn, echo_drv}, [eof]),
    ?line ok = erl_ddll:unload(echo_drv),
    ?line Port ! {self(), {command, "text"}},
    ?line ok = receive 
		   {Port, {data, "text"}} -> ok;
		   _ -> error
	       after 
		   1000 -> error
	       end,
    ?line Ref = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line ok = receive X -> {error, X} after 300 -> ok end,
    ?line erlang:port_close(Port),
    ?line ok = receive {'DOWN', Ref, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    % More than one user
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line Ref2 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line Port2 = open_port({spawn, echo_drv}, [eof]),
    ?line ok = erl_ddll:unload(echo_drv),
    ?line Port2 ! {self(), {command, "text"}},
    ?line ok = receive 
		   {Port2, {data, "text"}} -> ok;
		   _ -> error
	       after 
		   1000 -> error
	       end,
    ?line ok = erl_ddll:unload(echo_drv),
    ?line Port2 ! {self(), {command, "text"}},
    ?line ok = receive 
		   {Port2, {data, "text"}} -> ok;
		   _ -> error
	       after 
		   1000 -> error
	       end,
    ?line ok = erl_ddll:unload(echo_drv),
    ?line Port2 ! {self(), {command, "text"}},
    ?line ok = receive 
		   {Port2, {data, "text"}} -> ok;
		   _ -> error
	       after 
		   1000 -> error
	       end,
    ?line ok = receive X2 -> {error, X2} after 300 -> ok end,
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line ok = receive {'UP', Ref2, driver, echo_drv, unload_cancelled} -> ok after 300 -> error end,
    ?line Ref3 = erl_ddll:monitor(driver,{echo_drv,unloaded_only}),
    ?line erlang:port_close(Port2),
    ?line ok = receive X3 -> {error, X3} after 300 -> ok end,
    ?line ok = erl_ddll:unload(echo_drv),
    ?line ok = receive {'DOWN', Ref3, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    ?line test_server:timetrap_cancel(Dog),
    ok.
    
    


ddll_test(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    %?line {error,{already_started,ErlDdllPid}} = erl_ddll:start(),
    %?line ErlDdllPid = whereis(ddll_server),

    %% Load the echo driver and verify that it was loaded.
    {ok,L1,L2}=load_echo_driver(Path),

    %% Verify that the driver works.

     ?line Port = open_port({spawn, echo_drv}, [eof]),
     ?line {hej, "hopp",4711,123445567436543653} = 
 	erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
     ?line {hej, "hopp",4711,123445567436543653} = 
 	erlang:port_call(Port,47,{hej, "hopp",4711,123445567436543653}),
     ?line Port ! {self(), {command, "text"}},
     ?line 1 = receive 
 		  {Port, {data, "text"}} -> 1;
 		  _Other -> 2
 	      after 
 		  1000 -> 2
 	      end,
     ?line Port ! {self(), close},
     ?line receive {Port, closed} -> ok end,

%%     %% Unload the driver and verify that it was unloaded.
     ok = unload_echo_driver(L1,L2),

%%     %?line {error, {already_started, _}} = erl_ddll:start(),

    ?line test_server:timetrap_cancel(Dog),
    ok.

%% Tests errors having to do with bad drivers.

errors(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    ?line {ok, L1} = erl_ddll:loaded_drivers(),

    ?line {error, {open_error, _}} = erl_ddll:load_driver(Path, bad_name),
    ?line {error, driver_init_failed} = erl_ddll:load_driver(Path, initfail_drv),
    ?line {error, bad_driver_name} = erl_ddll:load_driver(Path, wrongname_drv),

    %% We assume that there is a statically linked driver named "ddll":
    ?line {error, linked_in_driver} = erl_ddll:unload_driver(efile),
    ?line {error, not_loaded} = erl_ddll:unload_driver("__pucko_driver__"),
	
    case os:type() of
	{unix, _} ->
	    ?line {error, no_driver_init} =
		erl_ddll:load_driver(Path, noinit_drv);
	_ ->
	    ok
    end,

    ?line {ok, L1} = erl_ddll:loaded_drivers(),

    ?line test_server:timetrap_cancel(Dog),
    ok.

reference_count(doc) ->
    ["Check that drivers are unloaded when their reference count ",
     "reaches zero, and that they cannot be unloaded while ",
     "they are still referenced."];
reference_count(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    Pid1=spawn_link(?MODULE, echo_loader, [Path, self()]),
    receive
	{Pid1, echo_loaded} -> ok
    after 2000 -> test_server:fail("echo_loader failed to start.")
    end,

    Pid1 ! {self(), die},
    ?line test_server:sleep(200),   % Give time to unload.
    % Verify that the driver was automaticly unloaded when the
    % process died.
    ?line {error, not_loaded}=erl_ddll:unload_driver(echo_drv),
    
    ?line test_server:timetrap_cancel(Dog),
    ok.

% Loads the echo driver, send msg to started, sits and waits to
% get a signal to die, then unloads the driver and terminates.
echo_loader(Path, Starter) ->
    ?line {ok, L1, L2}=load_echo_driver(Path),
    ?line Starter ! {self(), echo_loaded},
    receive
	{Starter, die} ->
	    ?line unload_echo_driver(L1,L2)
    end.

% Loads the echo driver, send msg to started, sits and waits to
% get a signal to die, then unloads the driver and terminates.
nice_echo_loader(Path, Starter) ->
    ?line {ok, L1, L2}=load_nice_echo_driver(Path),
    ?line Starter ! {self(), echo_loaded},
    receive
	{Starter, die} ->
	    ?line unload_echo_driver(L1,L2)
    end.


kill_port(doc) ->
    ["Test that a port that uses a driver is killed when the ",
     "process that loaded the driver dies."];
kill_port(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    ?line Pid1=spawn(?MODULE, echo_loader, [Path, self()]),
    ?line receive
	      {Pid1, echo_loaded} ->
		  ok
	  after 3000 ->
		  ?line exit(Pid1, kill),
		  ?line test_server:fail("echo_loader failed to start.")
	  end,

    % Spawn off a port that uses the driver.
    ?line Port = open_port({spawn, echo_drv}, [eof]),

    % Kill the process / unload the driver.
    ?line process_flag(trap_exit, true),
    ?line exit(Pid1, kill),
    ?line test_server:sleep(200),    % Give some time to unload.
    ?line {error, not_loaded} = erl_ddll:unload_driver(echo_drv),

    % See if the port is killed.
    receive
	{'EXIT', Port, Reason} ->
	    io:format("Port exited with reason ~w", [Reason])
    after 5000 ->
	    ?line test_server:fail("Echo port did not terminate.")
    end,

    %% Cleanup and exit.
    ?line test_server:timetrap_cancel(Dog),
    ok.

dont_kill_port(doc) ->
    ["Test that a port that uses a driver is not killed when the ",
     "process that loaded the driver dies and it's nicely opened."];
dont_kill_port(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    ?line Pid1=spawn(?MODULE, nice_echo_loader, [Path, self()]),
    ?line receive
	      {Pid1, echo_loaded} ->
		  ok
	  after 3000 ->
		  ?line exit(Pid1, kill),
		  ?line test_server:fail("echo_loader failed to start.")
	  end,

    % Spawn off a port that uses the driver.
    ?line Port = open_port({spawn, echo_drv}, [eof]),

    % Kill the process / unload the driver.
    ?line process_flag(trap_exit, true),
    ?line exit(Pid1, kill),
    ?line test_server:sleep(200),    % Give some time to unload.
    ?line {hej, "hopp",4711,123445567436543653} = 
	erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
    ?line [] = erl_ddll:info(echo_drv,processes),
    %% unload should work with no owner
    ?line ok = erl_ddll:unload_driver(echo_drv), %Kill ports while at it

    % See if the port is killed.
    receive
	{'EXIT', Port, Reason} ->
	    io:format("Port exited with reason ~w", [Reason])
    after 5000 ->
	    ?line test_server:fail("Echo port did not terminate.")
    end,

    %% Cleanup and exit.
    ?line test_server:timetrap_cancel(Dog),
    ok.

properties(doc) -> ["Test that a process that loaded a driver ",
		    "is the only process that can unload it."];
properties(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),

    % Let another process load the echo driver.
    Pid=spawn_link(?MODULE, echo_loader, [Path, self()]),
    receive
	{Pid, echo_loaded} -> ok
    after 2000 -> test_server:fail("echo_loader failed to start.")
    end,

    % Try to unload the driver from this process (the wrong one).
    ?line {error, _} = erl_ddll:unload_driver(echo_drv),
    ?line {ok, Drivers} = erl_ddll:loaded_drivers(),
    ?line case lists:member("echo_drv", Drivers) of
	      true ->
		  ok;
	      false ->
		  test_server:fail("Unload from wrong process "
				   "succeeded.")
	  end,

    % Unload the driver and terminate dummy process.
    ?line Pid ! {self(), die},
    ?line test_server:sleep(200),   % Give time to unload.
    ?line test_server:timetrap_cancel(Dog),
    ok.

load_and_unload(doc) -> ["Load two drivers and unload them in load order."];
load_and_unload(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(60)),
    ?line Path = ?config(data_dir, Config),
    ?line {ok, Loaded_drivers1} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:load_driver(Path, echo_drv),
    ?line ok = erl_ddll:load_driver(Path, dummy_drv),
    ?line ok = erl_ddll:unload_driver(echo_drv),
    ?line ok = erl_ddll:unload_driver(dummy_drv),
    ?line {ok, Loaded_drivers2} = erl_ddll:loaded_drivers(),
    ?line Set1 = ordsets:from_list(Loaded_drivers1),
    ?line Set2 = ordsets:from_list(Loaded_drivers2),
    ?line io:format("~p == ~p\n", [Loaded_drivers1, Loaded_drivers2]),
    ?line [] = ordsets:to_list(ordsets:subtract(Set2, Set1)),

    ?line test_server:timetrap_cancel(Dog),
    ok.    

lock_driver(suite) ->
    [];
lock_driver(doc) ->
    ["Check multiple calls to driver_lock_driver"];
lock_driver(Config) when is_list(Config) ->
    ?line Dog = test_server:timetrap(test_server:seconds(10)),
    ?line Path = ?config(data_dir, Config),
    ?line {ok, _} = erl_ddll:try_load(Path, lock_drv, []),
    ?line Port1 = open_port({spawn, lock_drv}, [eof]),
    ?line Port2 = open_port({spawn, lock_drv}, [eof]),
    ?line true = erl_ddll:info(lock_drv,permanent),
    ?line erlang:port_close(Port1),
    ?line erlang:port_close(Port2),
    ?line test_server:timetrap_cancel(Dog),
    ok.
    

% Load and unload the echo_drv driver.
% Make sure that the driver doesn't exist before we load it,
% and that it exists before we unload it.
load_echo_driver(Path) ->
    ?line {ok, L1} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:load_driver(Path, echo_drv),
    ?line {ok, L2} = erl_ddll:loaded_drivers(),
    ?line ["echo_drv"] = ordsets:to_list(subtract(ordsets:from_list(L2), 
						  ordsets:from_list(L1))),
    {ok,L1,L2}.

load_nice_echo_driver(Path) ->
    ?line {ok, L1} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:load(Path, echo_drv),
    ?line {ok, L2} = erl_ddll:loaded_drivers(),
    ?line ["echo_drv"] = ordsets:to_list(subtract(ordsets:from_list(L2), 
						  ordsets:from_list(L1))),
    {ok,L1,L2}.

unload_echo_driver(L1,L2) ->
    ?line {ok, L2} = erl_ddll:loaded_drivers(),
    ?line ok = erl_ddll:unload_driver(echo_drv),
    ?line {ok, L3} = erl_ddll:loaded_drivers(),
    ?line [] = ordsets:to_list(subtract(ordsets:from_list(L3),
					ordsets:from_list(L1))),
    ok.

unload_expect_fast(Driver,XFlags) ->
    {ok, pending_driver, Ref} = 
	erl_ddll:try_unload(Driver,
			    [{monitor,pending_driver}]++XFlags),
    receive
	{'DOWN', Ref, driver, Driver, unloaded} ->
	    case lists:member(atom_to_list(Driver),element(2,erl_ddll:loaded_drivers())) of
		true ->
		    {error, {still_there, Driver}};
		false ->
		    ok
	    end
    after 1000 ->
	    {error,{unable_to_unload, Driver}}
    end.
