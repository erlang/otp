%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

%%
%% File:    os_signal_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2017-01-13
%%

-module(os_signal_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

-export([set_alarm/1, fork/0, get_exit_code/1]).

% Test cases
-export([set_unset/1,
         t_sighup/1,
         t_sigusr1/1,
         t_sigusr2/1,
         t_sigterm/1,
         t_sigalrm/1,
         t_sigchld/1,
         t_sigchld_fork/1]).

-define(signal_server, erl_signal_server).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    case os:type() of
        {win32, _} -> [];
        _ -> [set_unset,
              t_sighup,
              t_sigusr1,
              t_sigusr2,
              t_sigterm,
              t_sigalrm,
              t_sigchld,
              t_sigchld_fork]
    end.

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Pid = erlang:whereis(?signal_server),
    true = erlang:unregister(?signal_server),
    [{signal_server, Pid}|Config].

end_per_testcase(_Func, Config) ->
    case proplists:get_value(signal_server, Config) of
        undefined -> ok;
        Pid ->
            true = erlang:register(?signal_server, Pid),
            ok
    end.

init_per_suite(Config) ->
    load_nif(Config),
    Config.

end_per_suite(_Config) ->
    ok.

%% tests

set_unset(_Config) ->
    Signals = [sighup, %sigint,
               sigquit, %sigill,
               sigabrt,
               sigalrm, sigterm,
               sigusr1, sigusr2,
               sigchld,
               sigstop, sigtstp],
    F1 = fun(Sig) -> ok = os:set_signal(Sig,handle) end,
    F2 = fun(Sig) -> ok = os:set_signal(Sig,default) end,
    F3 = fun(Sig) -> ok = os:set_signal(Sig,ignore) end,
    %% set handle
    ok = lists:foreach(F1, Signals),
    %% set ignore
    ok = lists:foreach(F2, Signals),
    %% set default
    ok = lists:foreach(F3, Signals),
    ok.

t_sighup(_Config) ->
    Pid1 = setup_service(),
    OsPid = os:getpid(),
    os:set_signal(sighup, handle),
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sighup},
     {notify,sighup},
     {notify,sighup}] = Msgs1,
    %% no proc
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    %% ignore
    Pid2 = setup_service(),
    os:set_signal(sighup, ignore),
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    ok = kill("HUP", OsPid),
    Msgs2 = fetch_msgs(Pid2),
    io:format("Msgs2: ~p~n", [Msgs2]),
    [] = Msgs2,
    %% reset to handle (it's the default)
    os:set_signal(sighup, handle),
    ok.

t_sigusr1(_Config) ->
    Pid1 = setup_service(),
    OsPid = os:getpid(),
    os:set_signal(sigusr1, handle),
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sigusr1},
     {notify,sigusr1},
     {notify,sigusr1}] = Msgs1,
    %% no proc
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    %% ignore
    Pid2 = setup_service(),
    os:set_signal(sigusr1, ignore),
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    ok = kill("USR1", OsPid),
    Msgs2 = fetch_msgs(Pid2),
    io:format("Msgs2: ~p~n", [Msgs2]),
    [] = Msgs2,
    %% reset to ignore (it's the default)
    os:set_signal(sigusr1, handle),
    ok.

t_sigusr2(_Config) ->
    Pid1 = setup_service(),
    OsPid = os:getpid(),
    os:set_signal(sigusr2, handle),
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sigusr2},
     {notify,sigusr2},
     {notify,sigusr2}] = Msgs1,
    %% no proc
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    %% ignore
    Pid2 = setup_service(),
    os:set_signal(sigusr2, ignore),
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    ok = kill("USR2", OsPid),
    Msgs2 = fetch_msgs(Pid2),
    io:format("Msgs2: ~p~n", [Msgs2]),
    [] = Msgs2,
    %% reset to ignore (it's the default)
    os:set_signal(sigusr2, ignore),
    ok.

t_sigterm(_Config) ->
    Pid1 = setup_service(),
    OsPid = os:getpid(),
    os:set_signal(sigterm, handle),
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sigterm},
     {notify,sigterm},
     {notify,sigterm}] = Msgs1,
    %% no proc
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    %% ignore
    Pid2 = setup_service(),
    os:set_signal(sigterm, ignore),
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    ok = kill("TERM", OsPid),
    Msgs2 = fetch_msgs(Pid2),
    io:format("Msgs2: ~p~n", [Msgs2]),
    [] = Msgs2,
    %% reset to handle (it's the default)
    os:set_signal(sigterm, handle),
    ok.

t_sigchld(_Config) ->
    Pid1 = setup_service(),
    OsPid = os:getpid(),
    os:set_signal(sigchld, handle),
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sigchld},
     {notify,sigchld},
     {notify,sigchld}] = Msgs1,
    %% no proc
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    %% ignore
    Pid2 = setup_service(),
    os:set_signal(sigchld, ignore),
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    ok = kill("CHLD", OsPid),
    Msgs2 = fetch_msgs(Pid2),
    io:format("Msgs2: ~p~n", [Msgs2]),
    [] = Msgs2,
    %% reset to handle (it's the default)
    os:set_signal(sigchld, ignore),
    ok.


t_sigalrm(_Config) ->
    Pid1 = setup_service(),
    ok = os:set_signal(sigalrm, handle),
    ok = os_signal_SUITE:set_alarm(1),
    receive after 3000 -> ok end,
    Msgs1 = fetch_msgs(Pid1),
    [{notify,sigalrm}] = Msgs1,
    io:format("Msgs1: ~p~n", [Msgs1]),
    os:set_signal(sigalrm, ignore),
    Pid2 = setup_service(),
    ok = os_signal_SUITE:set_alarm(1),
    receive after 3000 -> ok end,
    Msgs2 = fetch_msgs(Pid2),
    [] = Msgs2,
    io:format("Msgs2: ~p~n", [Msgs2]),
    Pid3 = setup_service(),
    os:set_signal(sigalrm, handle),
    ok = os_signal_SUITE:set_alarm(1),
    receive after 3000 -> ok end,
    Msgs3 = fetch_msgs(Pid3),
    [{notify,sigalrm}] = Msgs3,
    io:format("Msgs3: ~p~n", [Msgs3]),
    os:set_signal(sigalrm, ignore),
    ok.

t_sigchld_fork(_Config) ->
    Pid1 = setup_service(),
    ok = os:set_signal(sigchld, handle),
    {ok,OsPid} = os_signal_SUITE:fork(),
    receive after 3000 -> ok end,
    Msgs1 = fetch_msgs(Pid1),
    io:format("Msgs1: ~p~n", [Msgs1]),
    [{notify,sigchld}] = Msgs1,
    {ok,Status} = os_signal_SUITE:get_exit_code(OsPid),
    io:format("exit status from ~w : ~w~n", [OsPid,Status]),
    42 = Status,
    %% reset to ignore (it's the default)
    os:set_signal(sigchld, ignore),
    ok.


%% nif stubs

set_alarm(_Secs) -> no.
fork() -> no.
get_exit_code(_OsPid) -> no.

%% aux

setup_service() ->
    Pid = spawn_link(fun msgs/0),
    true = erlang:register(?signal_server, Pid),
    Pid.

msgs() ->
    msgs([]).
msgs(Ms) ->
    receive
        {Pid, fetch_msgs} -> Pid ! {self(), lists:reverse(Ms)};
        Msg ->
            msgs([Msg|Ms])
    end.

fetch_msgs(Pid) ->
    Pid ! {self(), fetch_msgs},
    receive {Pid, Msgs} -> Msgs end.

kill(Signal, Pid) ->
    {0,_} = run("kill", ["-s", Signal, Pid]),
    receive after 200 -> ok end,
    ok.

load_nif(Config) ->
    Path = proplists:get_value(data_dir, Config),
    case erlang:load_nif(filename:join(Path,"os_signal_nif"), 0) of
        ok -> ok;
        {error,{reload,_}} -> ok
    end.

run(Program0, Args) -> run(".", Program0, Args).
run(Cwd, Program0, Args) when is_list(Cwd) ->
    Program = case os:find_executable(Program0) of
                  Path when is_list(Path) ->
                      Path;
                  false ->
                      exit(no)
              end,
    Options = [{args,Args},binary,exit_status,stderr_to_stdout,
               {line,4096}, {cd, Cwd}],
    try open_port({spawn_executable,Program}, Options) of
        Port ->
            run_loop(Port, [])
    catch
        error:_ ->
                      exit(no)
    end.

run_loop(Port, Output) ->
    receive
        {Port,{exit_status,Status}} ->
            {Status,lists:reverse(Output)};
        {Port,{data,{eol,Bin}}} ->
            run_loop(Port, [Bin|Output]);
        _Msg ->
            run_loop(Port, Output)
    end.
