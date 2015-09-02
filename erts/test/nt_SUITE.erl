%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
%%% Purpose: Test NT specific utilities
-module(nt_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,init_per_testcase/2,
	 end_per_testcase/2,nt/1,handle_eventlog/2,
	 middleman/1,service_basic/1, service_env/1, user_env/1, synced/1, 
	 service_prio/1, 
	 logout/1, debug/1, restart/1, restart_always/1,stopaction/1,
	 shutdown_io/0,do_shutdown_io/0]).
-define(TEST_TIMEOUT, ?t:seconds(180)).

-define(TEST_SERVICES, [1,2,3,4,5,6,7,8,9,10,11]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case os:type() of
	{win32, nt} ->
	    [nt, service_basic, service_env, user_env, synced,
	     service_prio, logout, debug, restart, restart_always,
	     stopaction];
	_ -> [nt]
    end.

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


init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(?TEST_TIMEOUT),
    [{watchdog, Dog} | Config].

end_per_testcase(_Func, Config) ->
    lists:foreach(fun(X) -> 
			  catch remove_service("test_service_" ++ 
					 integer_to_list(X)) end,
		  ?TEST_SERVICES),
    Dog = ?config(watchdog, Config),
    catch test_server:timetrap_cancel(Dog),
    ok.    

erlsrv() ->
    "\"" ++ os:find_executable(erlsrv) ++ "\"".


recv_prog_output(Port) -> 
    receive
	{Port, {data, {eol,Data}}} ->
	    %%io:format("Got data: ~s~n", [Data]),
	    [ Data | recv_prog_output(Port)];
	_X ->
	    %%io:format("Got data: ~p~n", [_X]),
	    Port ! {self(), close},
	    receive
		_ ->
		    []
	    end
    end.

	    
%%% X == parameters to erlsrv
%%% returns command output without stderr
do_command(X) ->
    %%io:format("Command: ~s~n", [erlsrv() ++ " " ++ X]),
    Port = open_port({spawn, erlsrv() ++ " " ++ X}, [stream, {line, 100}, eof, in]), 
    Res = recv_prog_output(Port),
    case Res of
	[] ->
	    failed;
	_Y ->
	    %%io:format("~p~n",[_Y]),
	    ok
    end.


create_service(Name) ->
    ok = do_command("add " ++ Name).

start_service(Name) ->
    ok = do_command("start " ++ Name).

stop_service(Name) ->
    ok = do_command("stop " ++ Name).

remove_service(Name) ->
    ok = do_command("remove " ++ Name).
do_wait_for_it(_,0) ->
    false;
do_wait_for_it(FullName,N) ->
    case net_adm:ping(FullName) of
	pong ->
	    true;
	_ ->
	    receive 
	    after 1000 ->
		    do_wait_for_it(FullName,N-1)
	    end
    end.

wait_for_node(Name) ->
    FullName = make_full_name(Name),
    do_wait_for_it(FullName,30).

make_full_name(Name) ->
    [_,Suffix] = string:tokens(atom_to_list(node()),"@"),
    list_to_atom(Name ++ "@" ++ Suffix).
    

%%% The following tests are only run on NT:

service_basic(doc) ->
    ["Check some basic (cosmetic) service parameters"];
service_basic(suite) -> [];
service_basic(Config) when is_list(Config) ->
    ?line Name = "test_service_20",
    ?line IntName = Name++"_internal",
    ?line Service = [{servicename,Name},
		     {args, ["-setcookie", 
		      atom_to_list(erlang:get_cookie())]},
		     {internalservicename,IntName},
		     {comment,"Epic comment"}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line S2 = erlsrv:get_service(Name),
    ?line {value,{comment,"Epic comment"}} = lists:keysearch(comment,1,S2),
    ?line {value,{internalservicename,IntName}} =
	lists:keysearch(internalservicename,1,S2),
    ?line S3 = lists:keyreplace(comment,1,S2,{comment,"Basic comment"}),
    ?line S4 = lists:keyreplace(internalservicename,1,S3,
				{internalservicename,"WillNotHappen"}),
    ?line ok = erlsrv:store_service(S4),
    ?line S5 = erlsrv:get_service(Name),
    ?line {value,{comment,"Basic comment"}} = lists:keysearch(comment,1,S5),
    ?line {value,{internalservicename,IntName}} =
	lists:keysearch(internalservicename,1,S5),
    ?line NewName = "test_service_21",
    ?line S6 = erlsrv:new_service(NewName,S5,[]), % should remove 
						  % internalservicename
    ?line ok = erlsrv:store_service(S6),
    ?line S7 = erlsrv:get_service(NewName),
    ?line {value,{comment,"Basic comment"}} = lists:keysearch(comment,1,S7),
    ?line {value,{internalservicename,[$t,$e,$s,$t | _]}} =
	lists:keysearch(internalservicename,1,S7),
    ?line remove_service(Name),
    ?line remove_service(NewName),
    ok.

service_env(doc) ->
    ["Check that service name and executable is in the environment of the " ++ 
     "erlang process created by erlsrv."];
service_env(suite) -> [];
service_env(Config) when is_list(Config) ->
    ?line Name = "test_service_2",
    ?line Service = [{servicename,Name},
		     {args, ["-setcookie", 
		      atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line Name = rpc:call(make_full_name(Name),os,getenv,
			  ["ERLSRV_SERVICE_NAME"]),
    ?line "erlsrv.exe" = filename:basename(
			   hd(
			     string:tokens(
			       rpc:call(make_full_name(Name),
					os,
					getenv,
					["ERLSRV_EXECUTABLE"]),
			       "\""))),
    ?line remove_service(Name),
    ok.
user_env(doc) ->
    ["Check that the user defined environment is ADDED to the service's"++
     " normal dito."];
user_env(suite) -> [];
user_env(Config) when is_list(Config) ->
    ?line Name = "test_service_3",
    ?line Service = [{servicename,Name},{env,[{"HUBBA","BUBBA"}]},
		     {args, ["-setcookie", 
		      atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line true = rpc:call(make_full_name(Name),os,getenv,
			  ["SystemDrive"]) =/= false,
    ?line "BUBBA" = rpc:call(make_full_name(Name),os,getenv,["HUBBA"]),
    ?line remove_service(Name),
    ok.
synced(doc) -> 
    ["Check that services are stopped and started syncronous and that"++
     " failed stopactions kill the erlang machine anyway."];
synced(suite) -> [];
synced(Config) when is_list(Config) ->
    ?line Name0 = "test_service_4",
    ?line Service0 = [{servicename,Name0},
		      {machine, "N:\\nickeNyfikenPaSjukhus"}],
    ?line ok = erlsrv:store_service(Service0),
    ?line true = (catch start_service(Name0)) =/= ok,
    ?line remove_service(Name0),
    ?line Name = "test_service_5",
    ?line Service = [{servicename,Name},
		     {stopaction,"erlang:info(garbage_collection)."},
		     {args, ["-setcookie",      
			     atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line T1 = calendar:datetime_to_gregorian_seconds(
		 calendar:universal_time()),
    ?line stop_service(Name),
    ?line Diff1 = calendar:datetime_to_gregorian_seconds(
		    calendar:universal_time()) - T1,
    ?line true = Diff1 > 30,
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line T2 = calendar:datetime_to_gregorian_seconds(
		 calendar:universal_time()),
    ?line remove_service(Name),
    ?line Diff2 = calendar:datetime_to_gregorian_seconds(
		    calendar:universal_time()) - T2,
    ?line true = Diff2 > 30,
    ok.
service_prio(doc) ->
    ["Check that a service with higher prio create port programs with "
     "higher prio."]; 
service_prio(suite) -> [];
service_prio(Config) when is_list(Config) ->
    ?line Name = "test_service_6",
    ?line Service = [{servicename,Name},{prio,"high"},
		     {env, [{"HEART_COMMAND","echo off"}]},
		     {args, ["-setcookie", 
			     atom_to_list(erlang:get_cookie()),
			     "-heart"]}],
    ?line ok = erlsrv:store_service(Service),
    ?line {ok, OldProcs} = get_current_procs(Config),
    ?line start_service(Name),
    ?line {ok, NewProcs} = get_current_procs(Config),
    timer:sleep(2000),
    ?line {ok, NewProcs2} = get_current_procs(Config),
    ?line remove_service(Name),
    ?line Diff = arrived_procs(OldProcs,NewProcs),
    io:format("NewProcs ~p~n after sleep~n ~p~n",[Diff, arrived_procs(OldProcs,NewProcs2)]),
    %% Not really correct, could fail if another heart is
    %% started at the same time...
    ?line {value, {"heart.exe",_,"high"}} = 
	lists:keysearch("heart.exe",1,Diff),
    ok.
logout(doc) -> 
    ["Check that logout does not kill services"];
logout(suite) -> [];
logout(Config) when is_list(Config) ->
    ?line {comment, "Have to be run manually by registering a service with " ++
	   "heart, logout and log in again and then examine that the heart " ++
	   "process id is not changed."}.
debug(doc) ->
    ["Check the debug options to erlsrv."];
debug(suite) -> [];
debug(Config) when is_list(Config) ->
    ?line Name0 = "test_service_7",

    %% We used to set the privdir as temporary directory, but for some
    %% reason we don't seem to have write access to that directory,
    %% so we'll use the directory specified in the next line.
    ?line TempDir = "C:/TEMP",
    ?line Service0 = [{servicename,Name0},
  		      {workdir,filename:nativename(TempDir)},
		      {debugtype,"reuse"},
		      {args, ["-setcookie", 
			      atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service0),
    ?line T1 = calendar:datetime_to_gregorian_seconds(
		 calendar:local_time()),
    %% sleep a little
    ?line receive after 2000 -> ok end,
    ?line start_service(Name0),
    ?line true = wait_for_node(Name0),
    ?line LF = filename:join(TempDir, Name0++".debug"),
    ?line {ok,Info0} = file:read_file_info(LF),
    ?line T2 = calendar:datetime_to_gregorian_seconds(
		 Info0#file_info.mtime),
    ?line true = T2 > T1,
    ?line remove_service(Name0),
    ?line file:delete(LF),
    ?line Name1 = "test_service_8",
    ?line Service1 = [{servicename,Name1},
		      {workdir, filename:nativename(TempDir)},
		      {debugtype,"new"},
		      {args, ["-setcookie", 
			      atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service1),
    ?line T3 = calendar:datetime_to_gregorian_seconds(
		 calendar:local_time()),
    %% sleep a little
    ?line receive after 2000 -> ok end,
    ?line NF = next_logfile(TempDir, Name1),
    ?line start_service(Name1),
    ?line true = wait_for_node(Name1),
    ?line {ok,Info1} = file:read_file_info(NF),
    ?line T4 = calendar:datetime_to_gregorian_seconds(
		 Info1#file_info.mtime),
    ?line true = T4 > T3,
    ?line remove_service(Name1),
    ?line file:delete(NF),
    ok.

restart(doc) ->
    ["Check the restart options to erlsrv"];
restart(suite) -> [];
restart(Config) when is_list(Config) ->
    ?line Name = "test_service_9",
    ?line Service = [{servicename,Name},
		     {workdir, filename:nativename(logdir(Config))},
		     {onfail,"restart"},
		     {args, ["-setcookie", 
			     atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line receive after 20000 -> ok end,
    ?line rpc:call(make_full_name(Name),erlang,halt,[]),
    ?line receive after 1000 -> ok end,
    ?line true = wait_for_node(Name),
    ?line rpc:call(make_full_name(Name),erlang,halt,[]),
    ?line receive after 1000 -> ok end,
    ?line false = wait_for_node(Name),
    ?line remove_service(Name),
    ok.

restart_always(doc) ->
    ["Check the restart options to erlsrv"];
restart_always(suite) -> [];
restart_always(Config) when is_list(Config) ->
    ?line Name = "test_service_10",
    ?line Service = [{servicename,Name},
		     {workdir, filename:nativename(logdir(Config))},
		     {onfail,"restart_always"},
		     {args, ["-setcookie", 
			     atom_to_list(erlang:get_cookie())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line rpc:call(make_full_name(Name),erlang,halt,[]),
    ?line receive after 1000 -> ok end,
    ?line true = wait_for_node(Name),
    ?line rpc:call(make_full_name(Name),erlang,halt,[]),
    ?line receive after 1000 -> ok end,
    ?line true = wait_for_node(Name),
    ?line remove_service(Name),
    ok.
stopaction(doc) ->
    ["Check that stopaction does not hang output while shutting down"];
stopaction(suite) -> [];
stopaction(Config) when is_list(Config) ->
    ?line Name = "test_service_11",
    %% Icky, I prepend the first element in the codepath, cause
    %% I "suppose" it's the one to where I am. 
    ?line Service = [{servicename,Name},
		     {stopaction,atom_to_list(?MODULE) ++ ":shutdown_io()."},
		     {args, ["-setcookie", 
			     atom_to_list(erlang:get_cookie()),
			     "-pa", hd(code:get_path())]}],
    ?line ok = erlsrv:store_service(Service),
    ?line start_service(Name),
    ?line true = wait_for_node(Name),
    ?line T1 = calendar:datetime_to_gregorian_seconds(
		 calendar:universal_time()),
    ?line stop_service(Name),
    ?line Diff1 = calendar:datetime_to_gregorian_seconds(
		    calendar:universal_time()) - T1,
    ?line true = Diff1 < 30,
    ?line remove_service(Name),
    ok.


%%% This test is run on all platforms, but just gives a comment on 
%%% other platforms than NT.

nt(doc) ->
    ["Run NT specific tests."];
nt(suite) ->
    [];
nt(Config) when is_list(Config) ->
    case os:type() of
	{win32,nt} ->
	    nt_run();
	_ ->
	    {skipped, "This test case is intended for Win NT only."}
    end.


nt_run() ->
    ?line start_all(),
    ?line create_service("test_service_1"),
    ?line R = start_look_for_single("System","ErlSrv","Informational",
				    ".*test_service_1.*started.*"),
    ?line start_service("test_service_1"),
    ?line Res = look_for_single(R),
    ?line io:format("Result from eventlog: ~p~n",
	      [Res]),
    ?line remove_service("test_service_1"),
    ?line stop_all(),
    ok.

start_all() ->
    Pid1 = spawn_link(?MODULE,middleman,[[]]),
    register(?MODULE,Pid1),
    _Pid2 = nteventlog:start("log_testing",
			     {?MODULE,handle_eventlog,[Pid1]}).

stop_all() ->
    ?MODULE ! stop,
    nteventlog:stop().

start_look_for_single(Cat,Fac,Sev,MessRE) ->
    Ref = make_ref(),
    ?MODULE ! {lookfor, {self(), Ref, {Cat,Fac,Sev,MessRE}}},    
    Ref.

look_for_single(Ref) ->
    receive
	{Ref,Time,Mes} ->
	    {Time,Mes}
    after 60000 ->
	    timeout
    end.


%%% Mes = {Time,Category,Facility,Severity,Message}
handle_eventlog(Mes,Pid) ->
    Pid ! Mes.

%%% Waitfor = [{Pid, Ref, {Category,Facility,Severity,MessageRE}} ...]
middleman(Waitfor) ->
    receive 
	{Time,Category,Facility,Severity,Message} ->
	    io:format("Middleman got ~s...", [Message]),
	    case match_event({Time,Category,Facility,Severity,Message},
			     Waitfor) of
		{ok, {Pid,Ref,Time,Mes}, Rest} ->
		    io:format("matched~n"),
		    Pid ! {Ref,Time,Mes},
		    middleman(Rest);
		_ ->
		    io:format("no match~n"),
		    middleman(Waitfor)
	    end;
	{lookfor, X} ->
	    io:format("Middleman told to look for ~p~n", [X]),
	    middleman([X|Waitfor]);
	stop ->
	    stopped;
	_ ->
	    middleman(Waitfor)
    end.


%%% Matches events, not tail recursive.
match_event(_X, []) ->
    nomatch;
match_event({Time,Cat,Fac,Sev,Mes},[{Pid,Ref,{Cat,Fac,Sev,MesRE}} | Tail]) ->
    case re:run(Mes,MesRE,[{capture,none}]) of
	match ->
	    %%io:format("Match!~n"),
	    {ok,{Pid,Ref,Time,Mes},Tail};
	nomatch ->
	    %%io:format("No match~n"),
	    case match_event({Time,Cat,Fac,Sev,Mes},Tail) of
		{ok,X,Rest} ->
		    {ok,X,[{Pid,Ref,{Cat,Fac,Sev,MesRE}} | Rest]};
		X ->
		    X
	    end
    end;
match_event(X,[Y | T]) ->
    %%io:format("X == ~p, Y == ~p~n",[X,Y]),
    case match_event(X,T) of
	{ok,Z,R} ->
	    {ok,Z,[Y|R]};
	XX ->
	    XX
    end.

arrived_procs(_,[]) ->
    [];
arrived_procs(OldProcs,[{Executable, Pid, Priority} | TNewProcs]) ->
    case lists:keysearch(Pid,2,OldProcs) of
	{value, _} ->
	    arrived_procs(OldProcs, TNewProcs);
	false ->
	    [{Executable, Pid, Priority} | arrived_procs(OldProcs, TNewProcs)]
    end.
    

get_current_procs(Config) -> 
    ?line P = open_port({spawn,nt_info(Config) ++ " -E"},
			[{line,10000}]),
    ?line L = receive
		  {P,{data,{eol,D}}} ->
		      D;
		  _ -> "error. "
	      end,
    ?line P ! {self(), close},
    ?line receive
	      {P, closed} -> ok
	  end,
    ?line {done,{ok,Tok,_},_} = erl_scan:tokens([],L,0),
    ?line erl_parse:parse_term(Tok).

nt_info(Config) when is_list(Config) ->
    ?line "\"" ++ filename:join(?config(data_dir, Config), "nt_info") ++ "\"".


logdir(Config) ->
    ?line ?config(priv_dir, Config).

look_for_next(Template,L,N) ->
    ?line FN = Template ++ integer_to_list(N),
    ?line case lists:member(FN,L) of
	true ->
	    ?line look_for_next(Template,L,N+1);
	false ->
	    ?line FN
    end.

next_logfile(LD, Servicename) ->
    ?line {ok, Files} = file:list_dir(LD),
    ?line Ftmpl = Servicename ++ ".debug.",
    ?line filename:join(LD,look_for_next(Ftmpl,Files,1)).

%%% Functions run by the service

do_shutdown_io() ->
    receive
    after 2000 ->
	    io:format("IO in shutting down...~n"),
	    erlang:halt()
    end.

shutdown_io() ->
    spawn(?MODULE,do_shutdown_io,[]).
