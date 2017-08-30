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


-export([all/0, suite/0,
         ddll_test/1, errors/1, reference_count/1,
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

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 10}}].

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

%% Check that the driver is unloaded on process exit
unload_on_process_exit(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Parent = self(),
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        erl_ddll:try_load(Path, echo_drv, []),
                        Parent ! gone,
                        receive go -> ok end,
                        erl_ddll:loaded_drivers(),
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Pid ! go,
    receive
        gone -> ok
    end,
    true = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    Pid ! go,
    receive
        {'DOWN', Ref, process, Pid, banan} ->
            ok
    end,
    receive after 500 -> ok end,
    false = lists:member("echo_drv",element(2,erl_ddll:loaded_drivers())),
    ok.

%% Check that the driver is unloaded when the last port is closed
delayed_unload_with_ports(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:try_load(Path, echo_drv, []),
    erl_ddll:try_load(Path, echo_drv, []),
    Port = open_port({spawn, echo_drv}, [eof]),
    1 = erl_ddll:info(echo_drv, port_count),
    Port2 = open_port({spawn, echo_drv}, [eof]),
    2 = erl_ddll:info(echo_drv, port_count),
    {ok,pending_process} = erl_ddll:try_unload(echo_drv,[{monitor, pending_driver}]),
    {ok,pending_driver,Ref} = erl_ddll:try_unload(echo_drv,[{monitor, pending_driver}]),
    ok = receive _ -> false after 0 -> ok end,
    Port ! {self(), close},
    ok = receive {Port,closed} -> ok after 1000 -> false end,
    1 = erl_ddll:info(echo_drv, port_count),
    Port2 ! {self(), close},
    ok = receive {Port2,closed} -> ok after 1000 -> false end,
    ok = receive {'DOWN', Ref, driver, echo_drv, unloaded} -> ok after 1000 -> false end,
    ok.

%% Check that the driver with ports is unloaded on process exit
unload_due_to_process_exit(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
                        spawn(F3),
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        _Port2 = open_port({spawn, echo_drv}, [eof]),
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok.

%% Check that a driver with driver loaded in another process is not unloaded on process exit
no_unload_due_to_process_exit(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
                        spawn(F3),
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        _Port2 = open_port({spawn, echo_drv}, [eof]),
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive X -> {error, X} after 300 -> ok end,
    ok = unload_expect_fast(echo_drv,[]),
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok.

%% Check that a driver with open ports in another process is not unloaded on process exit
no_unload_due_to_process_exit_2(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
                        spawn(F3),
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        _Port2 = open_port({spawn, echo_drv}, [eof]),
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive X -> {error, X} after 300 -> ok end,
    erlang:port_close(Port),
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok.

%% Check delayed unload and reload
unload_reload_thingie(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded_only}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        spawn(F3),
                        receive go -> ok end,
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok,pending_driver,Ref3} = erl_ddll:try_unload(echo_drv,[{monitor,pending}]),
    Ref4 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ok = receive {'DOWN',Ref4, driver,echo_drv,load_cancelled} -> ok after 1000 -> false end,
    {ok,already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    ok = receive {'UP',Ref3, driver,echo_drv,unload_cancelled} -> ok after 1000 -> false end,
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    [{Parent,1}] = erl_ddll:info(echo_drv, processes),
    0 = erl_ddll:info(echo_drv, port_count),
    ok = unload_expect_fast(echo_drv,[{monitor,pending}]),
    ok = receive 
             {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
         after 300 -> error 
         end,
    ok = receive X -> {error, X} after 300 -> ok end,
    ok.

%% Check delayed unload and reload
unload_reload_thingie_2(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded_only}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        spawn(F3),
                        receive go -> ok end,
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok,pending_driver,Ref3} = erl_ddll:try_load(Path, echo_drv,
                                                 [{monitor,pending_driver},{reload,pending_driver}]),
    Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive {'DOWN',Ref4, driver,echo_drv,unloaded} -> ok after 1000 -> false end,
    ok = receive {'UP',Ref3, driver,echo_drv,loaded} -> ok after 1000 -> false end,
    [{Parent,1}] = erl_ddll:info(echo_drv, processes),
    0 = erl_ddll:info(echo_drv, port_count),
    ok = receive 
             {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
         after 300 -> error 
         end,
    ok = unload_expect_fast(echo_drv,[{monitor,pending}]),
    ok = receive X -> {error, X} after 300 -> ok end,
    ok.

%% Check delayed unload and reload failure
unload_reload_thingie_3(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
                        receive go -> ok end,
                        _Port = open_port({spawn, echo_drv}, [eof]),
                        spawn(F3),
                        receive go -> ok end,
                        exit(banan)
                end),
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok,pending_driver,Ref3} = erl_ddll:try_load(filename:join([Path,"skrumpf"]), echo_drv,
                                                 [{monitor,pending_driver},{reload,pending_driver}]),
    Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive 
             {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
         after 300 -> error 
         end,
    ok = receive {'DOWN',Ref4,driver,echo_drv,unloaded} -> ok after 300 -> false end,
    ok = receive 
             {'DOWN',Ref3, driver,echo_drv,{load_failure,_}} -> ok 
         after 1000 -> false 
         end,
    {'EXIT',_} = (catch erl_ddll:info(echo_drv, port_count)),
    {error, not_loaded} = erl_ddll:try_unload(echo_drv,[{monitor,pending}]),
    ok = receive X -> {error, X} after 300 -> ok end,
    ok.

%% Reload a driver that is pending on a user
reload_pending(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
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
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    receive opened -> ok end,
    {error, pending_process} = 
    erl_ddll:try_load(Path, echo_drv,
                      [{reload,pending_driver},
                       {monitor,pending_driver}]),
    {ok, pending_process, Ref3} = 
    erl_ddll:try_load(Path, echo_drv,
                      [{reload,pending},
                       {monitor,pending}]),
    ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive Y -> {error, Y} after 300 -> ok end,
    erlang:port_close(Port),
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok = receive {'UP', Ref3, driver, echo_drv, loaded} -> ok after 300 -> error end,
    [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    ok = receive Z -> {error, Z} after 300 -> ok end,
    ok.

%% Tests failure in the init in driver struct.
load_fail_init(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    PathFailing = proplists:get_value(priv_dir, Config),
    [_|_] = AllFailInits = filelib:wildcard("echo_drv_fail_init.*",Path),
    lists:foreach(fun(Name) ->
                          Src = filename:join([Path,Name]),
                          Ext = filename:extension(Name),
                          Dst =filename:join([PathFailing,"echo_drv"++Ext]),
                          file:delete(Dst),
                          {ok,_} = file:copy(Src,Dst)
                  end,
                  AllFailInits),
    [_|_] = filelib:wildcard("echo_drv.*",PathFailing),
    {error, driver_init_failed} = erl_ddll:try_load(PathFailing, 
                                                    echo_drv,
                                                    [{monitor,pending}]),
    ok = receive XX ->
                     {unexpected,XX}
         after 300 ->
                   ok
         end,
    ok.


%% Reload a driver that is pending but init fails
reload_pending_fail_init(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    PathFailing = proplists:get_value(priv_dir, Config),
    [_|_] = AllFailInits = filelib:wildcard("echo_drv_fail_init.*",Path),
    lists:foreach(fun(Name) ->
                          Src = filename:join([Path,Name]),
                          Ext = filename:extension(Name),
                          Dst =filename:join([PathFailing,"echo_drv"++Ext]),
                          file:delete(Dst),
                          {ok,_} = file:copy(Src,Dst)
                  end,
                  AllFailInits),
    [_|_] = filelib:wildcard("echo_drv.*",PathFailing),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
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
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    receive opened -> ok end,
    {ok, pending_process, Ref3} = 
    erl_ddll:try_load(PathFailing, echo_drv,
                      [{reload,pending},
                       {monitor,pending}]),
    ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive Y -> {error, Y} after 300 -> ok end,
    erlang:port_close(Port),
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok = receive {'DOWN', Ref3, driver, echo_drv, {load_failure,driver_init_failed}} -> ok  after 300 -> error end,
    {'EXIT',{badarg,_}} = (catch erl_ddll:info(echo_drv,processes)),

    ok = receive Z -> {error, Z} after 300 -> ok end,
    ok.

%% Reload a driver with kill_ports option that is pending on a user
reload_pending_kill(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit,true),
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
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
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {ok, already_loaded} = erl_ddll:try_load(Path, echo_drv, [{driver_options,[kill_ports]}]),
    {error,inconsistent} = erl_ddll:try_load(Path, echo_drv, []),
    Port = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    receive opened -> ok end,
    {error, pending_process} = 
    erl_ddll:try_load(Path, echo_drv,
                      [{driver_options,[kill_ports]},
                       {reload,pending_driver},
                       {monitor,pending_driver}]),
    {ok, pending_process, Ref3} = 
    erl_ddll:try_load(Path, echo_drv,
                      [{driver_options,[kill_ports]},
                       {reload,pending},
                       {monitor,pending}]),
    ok =  receive 
              {'EXIT', Port, driver_unloaded} ->
                  ok 
          after 300 -> error
          end,
    Pid ! go,
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok = receive {'UP', Ref3, driver, echo_drv, loaded} -> ok after 300 -> error end,
    [_,_] = erl_ddll:info(echo_drv,processes),
    ok = receive first_exit -> ok after 300 -> error end,
    ok = receive second_exit -> ok after 300 -> error end,
    0 = erl_ddll:info(echo_drv,port_count), 
    ok = receive X -> {error, X} after 300 -> ok end,
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive Y -> {error, Y} after 300 -> ok end,
    Port2 = open_port({spawn, echo_drv}, [eof]),
    true = is_port(Port2),
    [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    1 = erl_ddll:info(echo_drv,port_count), 
    erlang:port_close(Port2),
    ok = receive {'EXIT', Port2, normal} -> ok after 300 -> error end,
    0 = erl_ddll:info(echo_drv,port_count), 
    [{Parent,1}] = erl_ddll:info(echo_drv,processes),
    Port3 = open_port({spawn, echo_drv}, [eof]),
    {ok, pending_driver, Ref4} = 
    erl_ddll:try_unload(echo_drv,[{monitor,pending_driver}]),
    ok =  receive 
              {'EXIT', Port3, driver_unloaded} ->
                  ok 
          after 300 -> error
          end,
    ok = receive {'DOWN', Ref4, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    io:format("Port = ~w, Port2 = ~w, Port3 = ~w~n",[Port,Port2,Port3]),
    ok = receive Z -> {error, Z} after 300 -> ok end,
    process_flag(trap_exit,OldFlag),
    ok.


%% Some more error code checking
more_error_codes(Config) when is_list(Config) ->
    {error,Err} = erl_ddll:try_load("./echo_dr",echo_dr,[]),
    true = is_list(erl_ddll:format_error(Err)),
    true = is_list(erl_ddll:format_error(not_loaded)),
    ok.

%% Check kill_ports option to try_unload
forced_port_killing(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    OldFlag=process_flag(trap_exit,true),
    Parent = self(),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    {ok, loaded} = erl_ddll:try_load(Path, echo_drv, []),
    spawn(F3),
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    Port = open_port({spawn, echo_drv}, [eof]),
    Port2 = open_port({spawn, echo_drv}, [eof]),
    {ok, pending_driver, Ref1} = 
    erl_ddll:try_unload(echo_drv,[{monitor,pending_driver},kill_ports]),
    ok = receive 
             {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok 
         after 300 -> error 
         end,
    ok = receive {'EXIT',Port,driver_unloaded} -> ok after 300 -> false end,
    ok = receive {'EXIT',Port2,driver_unloaded} -> ok after 300 -> false end,
    ok = receive {'DOWN',Ref1, driver, echo_drv, unloaded} -> ok after 300 -> false end,
    process_flag(trap_exit,OldFlag),
    ok = receive X -> {error, X} after 300 -> ok end,
    ok.

%% Check delayed unload and reload with no trap_exit
no_trap_exit_and_kill_ports(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    Parent = self(),
    OldFlag=process_flag(trap_exit,true),
    F3 = fun() -> 
                 Parent ! erl_ddll:monitor(driver,{echo_drv,unloaded}), 
                 receive X -> Parent ! {got,X} end 
         end,
    Pid = spawn(fun() ->
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
    Ref = erlang:monitor(process,Pid),
    Pid ! go,
    {ok,Ref2} = receive 
                    R when is_reference(R) -> {ok,R}; 
                    Other -> {error, Other} 
                after 500 -> {error, timeout} 
                end,
    {error, inconsistent} = erl_ddll:try_load(Path, echo_drv, []),
    MyPort = open_port({spawn, echo_drv}, [eof]),
    Pid ! go,
    ok = receive {'DOWN', Ref, process, Pid, banan} -> ok after 300 -> error end,
    ok = receive {got,{'DOWN', Ref2, driver, echo_drv, unloaded}} -> ok after 300 -> error end,
    ok = receive {'EXIT',MyPort,driver_unloaded} -> ok after 300 -> error end,
    process_flag(trap_exit,OldFlag),
    ok.

%% Check monitor and demonitor of drivers
monitor_demonitor(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    erl_ddll:try_load(Path, echo_drv, []),
    Ref = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    Self = self(),
    [{Self,1}] = erl_ddll:info(echo_drv,awaiting_unload),
    true = erl_ddll:demonitor(Ref),
    [] = erl_ddll:info(echo_drv,awaiting_unload),
    erl_ddll:try_unload(echo_drv,[]),
    ok = receive _ -> error after 300 -> ok end,
    ok.

%% Check monitor/demonitor of driver loading
monitor_demonitor_load(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    {ok,loaded} = erl_ddll:try_load(Path, echo_drv, []),
    Port = open_port({spawn, echo_drv}, [eof]),
    Ref = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ok = receive {'UP',Ref,driver,echo_drv,loaded} -> ok after 500 -> error end,
    {ok, pending_driver} = erl_ddll:try_unload(echo_drv,[]),
    Ref2 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    ok = receive {'DOWN',Ref2,driver,echo_drv,load_cancelled} -> ok after 0 -> error end,
    {ok,already_loaded} = erl_ddll:try_load(Path, echo_drv, []),
    {ok, pending_driver} = 
    erl_ddll:try_load(Path, echo_drv, [{reload,pending_driver}]),
    Ref3 = erl_ddll:monitor(driver,{echo_drv,loaded}),
    Ref4 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ok = receive _ -> error after 300 -> ok end,
    Self = self(),
    [{Self,1}] = erl_ddll:info(echo_drv,awaiting_load),
    true = erl_ddll:demonitor(Ref3),
    [] = erl_ddll:info(echo_drv,awaiting_load),
    erlang:port_close(Port),
    ok = receive {'DOWN',Ref4,driver,echo_drv,unloaded} -> ok after 300 -> error end,
    ok = receive _ -> error after 300 -> ok end,
    ok = unload_expect_fast(echo_drv,[]),
    ok.

%% Test the new load/unload/reload interface
new_interface(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    % Typical scenario
    ok = erl_ddll:load(Path, echo_drv),
    Port = open_port({spawn, echo_drv}, [eof]),
    ok = erl_ddll:unload(echo_drv),
    Port ! {self(), {command, "text"}},
    ok = receive 
             {Port, {data, "text"}} -> ok;
             _ -> error
         after 
             1000 -> error
         end,
    Ref = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ok = receive X -> {error, X} after 300 -> ok end,
    erlang:port_close(Port),
    ok = receive {'DOWN', Ref, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    % More than one user
    ok = erl_ddll:load(Path, echo_drv),
    Ref2 = erl_ddll:monitor(driver,{echo_drv,unloaded}),
    ok = erl_ddll:load(Path, echo_drv),
    ok = erl_ddll:load(Path, echo_drv),
    Port2 = open_port({spawn, echo_drv}, [eof]),
    ok = erl_ddll:unload(echo_drv),
    Port2 ! {self(), {command, "text"}},
    ok = receive 
             {Port2, {data, "text"}} -> ok;
             _ -> error
         after 
             1000 -> error
         end,
    ok = erl_ddll:unload(echo_drv),
    Port2 ! {self(), {command, "text"}},
    ok = receive 
             {Port2, {data, "text"}} -> ok;
             _ -> error
         after 
             1000 -> error
         end,
    ok = erl_ddll:unload(echo_drv),
    Port2 ! {self(), {command, "text"}},
    ok = receive 
             {Port2, {data, "text"}} -> ok;
             _ -> error
         after 
             1000 -> error
         end,
    ok = receive X2 -> {error, X2} after 300 -> ok end,
    ok = erl_ddll:load(Path, echo_drv),
    ok = receive {'UP', Ref2, driver, echo_drv, unload_cancelled} -> ok after 300 -> error end,
    Ref3 = erl_ddll:monitor(driver,{echo_drv,unloaded_only}),
    erlang:port_close(Port2),
    ok = receive X3 -> {error, X3} after 300 -> ok end,
    ok = erl_ddll:unload(echo_drv),
    ok = receive {'DOWN', Ref3, driver, echo_drv, unloaded} -> ok after 300 -> error end,
    ok.




ddll_test(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    %{error,{already_started,ErlDdllPid}} = erl_ddll:start(),
    %ErlDdllPid = whereis(ddll_server),

    %% Load the echo driver and verify that it was loaded.
    {ok,L1,L2}=load_echo_driver(Path),

    %% Verify that the driver works.

    Port = open_port({spawn, echo_drv}, [eof]),
    {hej, "hopp",4711,123445567436543653} = 
    erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
    {hej, "hopp",4711,123445567436543653} = 
    erlang:port_call(Port,47,{hej, "hopp",4711,123445567436543653}),
    Port ! {self(), {command, "text"}},
    1 = receive 
            {Port, {data, "text"}} -> 1;
            _Other -> 2
        after 
            1000 -> 2
        end,
    Port ! {self(), close},
    receive {Port, closed} -> ok end,

    %%     %% Unload the driver and verify that it was unloaded.
    ok = unload_echo_driver(L1,L2),

    %%     %{error, {already_started, _}} = erl_ddll:start(),
    ok.

%% Tests errors having to do with bad drivers.

errors(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    {ok, L1} = erl_ddll:loaded_drivers(),

    {error, {open_error, _}} = erl_ddll:load_driver(Path, bad_name),
    {error, driver_init_failed} = erl_ddll:load_driver(Path, initfail_drv),
    {error, bad_driver_name} = erl_ddll:load_driver(Path, wrongname_drv),

    %% We assume that there is a statically linked driver named "ddll":
    {error, linked_in_driver} = erl_ddll:unload_driver(efile),
    {error, not_loaded} = erl_ddll:unload_driver("__pucko_driver__"),

    case os:type() of
        {unix, _} ->
            {error, no_driver_init} =
            erl_ddll:load_driver(Path, noinit_drv);
        _ ->
            ok
    end,

    {ok, L1} = erl_ddll:loaded_drivers(),
    ok.

%% Check that drivers are unloaded when their reference count
%% reaches zero, and that they cannot be unloaded while
%% they are still referenced.
reference_count(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    Pid1=spawn_link(?MODULE, echo_loader, [Path, self()]),
    receive
        {Pid1, echo_loaded} -> ok
    after 2000 -> ct:fail("echo_loader failed to start.")
    end,

    Pid1 ! {self(), die},
    test_server:sleep(200),   % Give time to unload.
    % Verify that the driver was automaticly unloaded when the
    % process died.
    {error, not_loaded}=erl_ddll:unload_driver(echo_drv),
    ok.

% Loads the echo driver, send msg to started, sits and waits to
% get a signal to die, then unloads the driver and terminates.
echo_loader(Path, Starter) ->
    {ok, L1, L2}=load_echo_driver(Path),
    Starter ! {self(), echo_loaded},
    receive
        {Starter, die} ->
            unload_echo_driver(L1,L2)
    end.

% Loads the echo driver, send msg to started, sits and waits to
% get a signal to die, then unloads the driver and terminates.
nice_echo_loader(Path, Starter) ->
    {ok, L1, L2}=load_nice_echo_driver(Path),
    Starter ! {self(), echo_loaded},
    receive
        {Starter, die} ->
            unload_echo_driver(L1,L2)
    end.


%% Test that a port that uses a driver is killed when the
%% process that loaded the driver dies.
kill_port(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    Pid1=spawn(?MODULE, echo_loader, [Path, self()]),
    receive
        {Pid1, echo_loaded} ->
            ok
    after 3000 ->
              exit(Pid1, kill),
              ct:fail("echo_loader failed to start.")
    end,

    % Spawn off a port that uses the driver.
    Port = open_port({spawn, echo_drv}, [eof]),

    % Kill the process / unload the driver.
    process_flag(trap_exit, true),
    exit(Pid1, kill),
    test_server:sleep(200),    % Give some time to unload.
    {error, not_loaded} = erl_ddll:unload_driver(echo_drv),

    % See if the port is killed.
    receive
        {'EXIT', Port, Reason} ->
            io:format("Port exited with reason ~w", [Reason])
    after 5000 ->
              ct:fail("Echo port did not terminate.")
    end,
    ok.

%% Test that a port that uses a driver is not killed when the
%% process that loaded the driver dies and it's nicely opened.
dont_kill_port(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    %% Spawn a process that loads the driver (and holds a reference
    %% to it).
    Pid1=spawn(?MODULE, nice_echo_loader, [Path, self()]),
    receive
        {Pid1, echo_loaded} ->
            ok
    after 3000 ->
              exit(Pid1, kill),
              ct:fail("echo_loader failed to start.")
    end,

    % Spawn off a port that uses the driver.
    Port = open_port({spawn, echo_drv}, [eof]),

    % Kill the process / unload the driver.
    process_flag(trap_exit, true),
    exit(Pid1, kill),
    test_server:sleep(200),    % Give some time to unload.
    {hej, "hopp",4711,123445567436543653} = 
    erlang:port_call(Port,{hej, "hopp",4711,123445567436543653}),
    [] = erl_ddll:info(echo_drv,processes),
    %% unload should work with no owner
    ok = erl_ddll:unload_driver(echo_drv), %Kill ports while at it

    % See if the port is killed.
    receive
        {'EXIT', Port, Reason} ->
            io:format("Port exited with reason ~w", [Reason])
    after 5000 ->
              ct:fail("Echo port did not terminate.")
    end,
    ok.

%% Test that a process that loaded a driver
%% is the only process that can unload it.
properties(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),

    % Let another process load the echo driver.
    Pid=spawn_link(?MODULE, echo_loader, [Path, self()]),
    receive
        {Pid, echo_loaded} -> ok
    after 2000 -> ct:fail("echo_loader failed to start.")
    end,

    % Try to unload the driver from this process (the wrong one).
    {error, _} = erl_ddll:unload_driver(echo_drv),
    {ok, Drivers} = erl_ddll:loaded_drivers(),
    case lists:member("echo_drv", Drivers) of
        true ->
            ok;
        false ->
            ct:fail("Unload from wrong process succeeded.")
    end,

    % Unload the driver and terminate dummy process.
    Pid ! {self(), die},
    test_server:sleep(200),   % Give time to unload.
    ok.

%% Load two drivers and unload them in load order.
load_and_unload(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    {ok, Loaded_drivers1} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:load_driver(Path, echo_drv),
    ok = erl_ddll:load_driver(Path, dummy_drv),
    ok = erl_ddll:unload_driver(echo_drv),
    ok = erl_ddll:unload_driver(dummy_drv),
    {ok, Loaded_drivers2} = erl_ddll:loaded_drivers(),
    Set1 = ordsets:from_list(Loaded_drivers1),
    Set2 = ordsets:from_list(Loaded_drivers2),
    io:format("~p == ~p\n", [Loaded_drivers1, Loaded_drivers2]),
    [] = ordsets:to_list(ordsets:subtract(Set2, Set1)),
    ok.    

%% Check multiple calls to driver_lock_driver
lock_driver(Config) when is_list(Config) ->
    Path = proplists:get_value(data_dir, Config),
    {ok, _} = erl_ddll:try_load(Path, lock_drv, []),
    Port1 = open_port({spawn, lock_drv}, [eof]),
    Port2 = open_port({spawn, lock_drv}, [eof]),
    true = erl_ddll:info(lock_drv,permanent),
    erlang:port_close(Port1),
    erlang:port_close(Port2),
    ok.


% Load and unload the echo_drv driver.
% Make sure that the driver doesn't exist before we load it,
% and that it exists before we unload it.
load_echo_driver(Path) ->
    {ok, L1} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:load_driver(Path, echo_drv),
    {ok, L2} = erl_ddll:loaded_drivers(),
    ["echo_drv"] = ordsets:to_list(subtract(ordsets:from_list(L2), 
                                            ordsets:from_list(L1))),
    {ok,L1,L2}.

load_nice_echo_driver(Path) ->
    {ok, L1} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:load(Path, echo_drv),
    {ok, L2} = erl_ddll:loaded_drivers(),
    ["echo_drv"] = ordsets:to_list(subtract(ordsets:from_list(L2), 
                                            ordsets:from_list(L1))),
    {ok,L1,L2}.

unload_echo_driver(L1,L2) ->
    {ok, L2} = erl_ddll:loaded_drivers(),
    ok = erl_ddll:unload_driver(echo_drv),
    {ok, L3} = erl_ddll:loaded_drivers(),
    [] = ordsets:to_list(subtract(ordsets:from_list(L3),
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
