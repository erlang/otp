%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         nt/1,handle_eventlog/2,
         middleman/1,service_basic/1, service_env/1, user_env/1, synced/1,
         service_prio/1,
         logout/1, debug/1, restart/1, restart_always/1,stopaction/1,
         shutdown_io/0,do_shutdown_io/0]).

-define(TEST_SERVICES, [1,2,3,4,5,6,7,8,9,10,11]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() ->
    case {os:type(), os:version()} of
	{{win32, nt}, Vsn} when Vsn =< {6,1,999999} ->
	    [nt, service_basic, service_env, user_env, synced,
	     service_prio, logout, debug, restart, restart_always,
	     stopaction];
	_ -> [nt]
    end.

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    lists:foreach(fun(X) -> 
                          catch remove_service("test_service_" ++ integer_to_list(X))
                  end, ?TEST_SERVICES),
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
    [_,Suffix] = string:lexemes(atom_to_list(node()),"@"),
    list_to_atom(Name ++ "@" ++ Suffix).


%%% The following tests are only run on NT:

%% Check some basic (cosmetic) service parameters
service_basic(Config) when is_list(Config) ->
    Name = "test_service_20",
    IntName = Name++"_internal",
    Service = [{servicename,Name},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]},
               {internalservicename,IntName},
               {comment,"Epic comment"}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    S2 = erlsrv:get_service(Name),
    {value,{comment,"Epic comment"}} = lists:keysearch(comment,1,S2),
    {value,{internalservicename,IntName}} =
    lists:keysearch(internalservicename,1,S2),
    S3 = lists:keyreplace(comment,1,S2,{comment,"Basic comment"}),
    S4 = lists:keyreplace(internalservicename,1,S3,
                          {internalservicename,"WillNotHappen"}),
    ok = erlsrv:store_service(S4),
    S5 = erlsrv:get_service(Name),
    {value,{comment,"Basic comment"}} = lists:keysearch(comment,1,S5),
    {value,{internalservicename,IntName}} =
    lists:keysearch(internalservicename,1,S5),
    NewName = "test_service_21",
    S6 = erlsrv:new_service(NewName,S5,[]), % should remove
    % internalservicename
    ok = erlsrv:store_service(S6),
    S7 = erlsrv:get_service(NewName),
    {value,{comment,"Basic comment"}} = lists:keysearch(comment,1,S7),
    {value,{internalservicename,[$t,$e,$s,$t | _]}} =
    lists:keysearch(internalservicename,1,S7),
    remove_service(Name),
    remove_service(NewName),
    ok.

%% Check that service name and executable is in the environment of the
%% erlang process created by erlsrv.
service_env(Config) when is_list(Config) ->
    Name = "test_service_2",
    Service = [{servicename,Name},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    Name = rpc:call(make_full_name(Name),os,getenv,
                    ["ERLSRV_SERVICE_NAME"]),
    "erlsrv.exe" = filename:basename(
                     hd(
                       string:lexemes(
                         rpc:call(make_full_name(Name),
                                  os,
                                  getenv,
                                  ["ERLSRV_EXECUTABLE"]),
                         "\""))),
    remove_service(Name),
    ok.

%% Check that the user defined environment is ADDED to the service's
%% normal dito.
user_env(Config) when is_list(Config) ->
    Name = "test_service_3",
    Service = [{servicename,Name},{env,[{"HUBBA","BUBBA"}]},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    true = rpc:call(make_full_name(Name),os,getenv,
                    ["SystemDrive"]) =/= false,
    "BUBBA" = rpc:call(make_full_name(Name),os,getenv,["HUBBA"]),
    remove_service(Name),
    ok.

%% Check that services are stopped and started syncronous and that
%% failed stopactions kill the erlang machine anyway.
synced(Config) when is_list(Config) ->
    Name0 = "test_service_4",
    Service0 = [{servicename,Name0},
                {machine, "N:\\nickeNyfikenPaSjukhus"}],
    ok = erlsrv:store_service(Service0),
    true = (catch start_service(Name0)) =/= ok,
    remove_service(Name0),
    Name = "test_service_5",
    Service = [{servicename,Name},
               {stopaction,"erlang:info(garbage_collection)."},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    T1 = calendar:datetime_to_gregorian_seconds(
           calendar:universal_time()),
    stop_service(Name),
    Diff1 = calendar:datetime_to_gregorian_seconds(
              calendar:universal_time()) - T1,
    true = Diff1 > 30,
    start_service(Name),
    true = wait_for_node(Name),
    T2 = calendar:datetime_to_gregorian_seconds(
           calendar:universal_time()),
    remove_service(Name),
    Diff2 = calendar:datetime_to_gregorian_seconds(
              calendar:universal_time()) - T2,
    true = Diff2 > 30,
    ok.

%% Check that a service with higher prio create port programs with
%% higher prio.
service_prio(Config) when is_list(Config) ->
    Name = "test_service_6",
    Service = [{servicename,Name},{prio,"high"},
               {env, [{"HEART_COMMAND","echo off"}]},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie()),
                       "-heart"]}],
    ok = erlsrv:store_service(Service),
    {ok, OldProcs} = get_current_procs(Config),
    start_service(Name),
    {ok, NewProcs} = get_current_procs(Config),
    timer:sleep(2000),
    {ok, NewProcs2} = get_current_procs(Config),
    remove_service(Name),
    Diff = arrived_procs(OldProcs,NewProcs),
    io:format("NewProcs ~p~n after sleep~n ~p~n",[Diff, arrived_procs(OldProcs,NewProcs2)]),
    %% Not really correct, could fail if another heart is
    %% started at the same time...
    {value, {"heart.exe",_,"high"}} = lists:keysearch("heart.exe",1,Diff),
    ok.

%% Check that logout does not kill services
logout(Config) when is_list(Config) ->
    {comment, "Have to be run manually by registering a service with " ++
     "heart, logout and log in again and then examine that the heart " ++
     "process id is not changed."}.

%% Check the debug options to erlsrv.
debug(Config) when is_list(Config) ->
    Name0 = "test_service_7",

    %% We used to set the privdir as temporary directory, but for some
    %% reason we don't seem to have write access to that directory,
    %% so we'll use the directory specified in the next line.
    TempDir = "C:/TEMP",
    Service0 = [{servicename,Name0},
                {workdir,filename:nativename(TempDir)},
                {debugtype,"reuse"},
                {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service0),
    T1 = calendar:datetime_to_gregorian_seconds(
           calendar:local_time()),
    %% sleep a little
    receive after 2000 -> ok end,
    start_service(Name0),
    true = wait_for_node(Name0),
    LF = filename:join(TempDir, Name0++".debug"),
    {ok,Info0} = file:read_file_info(LF),
    T2 = calendar:datetime_to_gregorian_seconds(
           Info0#file_info.mtime),
    true = T2 > T1,
    remove_service(Name0),
    file:delete(LF),
    Name1 = "test_service_8",
    Service1 = [{servicename,Name1},
                {workdir, filename:nativename(TempDir)},
                {debugtype,"new"},
                {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service1),
    T3 = calendar:datetime_to_gregorian_seconds(
           calendar:local_time()),
    %% sleep a little
    receive after 2000 -> ok end,
    NF = next_logfile(TempDir, Name1),
    start_service(Name1),
    true = wait_for_node(Name1),
    {ok,Info1} = file:read_file_info(NF),
    T4 = calendar:datetime_to_gregorian_seconds(
           Info1#file_info.mtime),
    true = T4 > T3,
    remove_service(Name1),
    file:delete(NF),
    ok.

%% Check the restart options to erlsrv
restart(Config) when is_list(Config) ->
    Name = "test_service_9",
    Service = [{servicename,Name},
               {workdir, filename:nativename(logdir(Config))},
               {onfail,"restart"},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    receive after 20000 -> ok end,
    rpc:call(make_full_name(Name),erlang,halt,[]),
    receive after 1000 -> ok end,
    true = wait_for_node(Name),
    rpc:call(make_full_name(Name),erlang,halt,[]),
    receive after 1000 -> ok end,
    false = wait_for_node(Name),
    remove_service(Name),
    ok.

%% Check the restart options to erlsrv
restart_always(Config) when is_list(Config) ->
    Name = "test_service_10",
    Service = [{servicename,Name},
               {workdir, filename:nativename(logdir(Config))},
               {onfail,"restart_always"},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    rpc:call(make_full_name(Name),erlang,halt,[]),
    receive after 1000 -> ok end,
    true = wait_for_node(Name),
    rpc:call(make_full_name(Name),erlang,halt,[]),
    receive after 1000 -> ok end,
    true = wait_for_node(Name),
    remove_service(Name),
    ok.

%% Check that stopaction does not hang output while shutting down
stopaction(Config) when is_list(Config) ->
    Name = "test_service_11",
    %% Icky, I prepend the first element in the codepath, cause
    %% I "suppose" it's the one to where I am. 
    Service = [{servicename,Name},
               {stopaction,atom_to_list(?MODULE) ++ ":shutdown_io()."},
               {args, ["-setcookie", atom_to_list(erlang:get_cookie()),
                       "-pa", hd(code:get_path())]}],
    ok = erlsrv:store_service(Service),
    start_service(Name),
    true = wait_for_node(Name),
    T1 = calendar:datetime_to_gregorian_seconds(
           calendar:universal_time()),
    stop_service(Name),
    Diff1 = calendar:datetime_to_gregorian_seconds(
              calendar:universal_time()) - T1,
    true = Diff1 < 30,
    remove_service(Name),
    ok.


%%% This test is run on all platforms, but just gives a comment on 
%%% other platforms than NT.

nt(Config) when is_list(Config) ->
    case {os:type(), os:version()} of
	{{win32, nt}, Vsn} when Vsn =< {6,1,999999} ->
	    nt_run();
	{{win32, nt},  _} ->
	    {skipped, "This test case requires admin privileges on Win 8 and later."};
	_ ->
	    {skipped, "This test case is intended for Win NT only."}
    end.


nt_run() ->
    start_all(),
    create_service("test_service_1"),
    R = start_look_for_single("System","ErlSrv","Informational",
                              ".*test_service_1.*started.*"),
    start_service("test_service_1"),
    Res = look_for_single(R),
    io:format("Result from eventlog: ~p~n",
              [Res]),
    remove_service("test_service_1"),
    stop_all(),
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
    P = open_port({spawn,nt_info(Config) ++ " -E"},
                  [{line,10000}]),
    L = receive
            {P,{data,{eol,D}}} ->
                D;
            _ -> "error. "
        end,
    P ! {self(), close},
    receive
        {P, closed} -> ok
    end,
    {done,{ok,Tok,_},_} = erl_scan:tokens([],L,0),
    erl_parse:parse_term(Tok).

nt_info(Config) when is_list(Config) ->
    "\"" ++ filename:join(proplists:get_value(data_dir, Config), "nt_info") ++ "\"".


logdir(Config) ->
    proplists:get_value(priv_dir, Config).

look_for_next(Template,L,N) ->
    FN = Template ++ integer_to_list(N),
    case lists:member(FN,L) of
        true ->
            look_for_next(Template,L,N+1);
        false ->
            FN
    end.

next_logfile(LD, Servicename) ->
    {ok, Files} = file:list_dir(LD),
    Ftmpl = Servicename ++ ".debug.",
    filename:join(LD,look_for_next(Ftmpl,Files,1)).

%%% Functions run by the service

do_shutdown_io() ->
    receive
    after 2000 ->
              io:format("IO in shutting down...~n"),
              erlang:halt()
    end.

shutdown_io() ->
    spawn(?MODULE,do_shutdown_io,[]).
