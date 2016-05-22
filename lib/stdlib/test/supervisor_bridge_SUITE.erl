%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(supervisor_bridge_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,starting/1,
	 mini_terminate/1,mini_die/1,badstart/1,
         simple_global_supervisor/1]).
-export([client/1,init/1,internal_loop_init/1,terminate/2,server9212/0]).

-include_lib("common_test/include/ct.hrl").
-define(bridge_name,supervisor_bridge_SUITE_server).
-define(work_bridge_name,work_supervisor_bridge_SUITE_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [starting, mini_terminate, mini_die, badstart, simple_global_supervisor].

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

starting(Config) when is_list(Config) ->
    process_flag(trap_exit,true),

    ignore = start(1),
    {error,testing} = start(2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mini_terminate(Config) when is_list(Config) ->
    miniappl(1),
    ok.

mini_die(Config) when is_list(Config) ->
    miniappl(2),
    ok.

miniappl(N) ->
    process_flag(trap_exit,true),
    {ok,Server} = start(3),
    Client = spawn_link(?MODULE,client,[N]),
    ct:timetrap({seconds,2}),
    miniappl_loop(Client, Server).


miniappl_loop([],[]) ->
    ok;
miniappl_loop(Client,Server) ->
    io:format("Client ~p, Server ~p\n",[Client,Server]),
    receive
	{'EXIT',Client,_} ->
	    miniappl_loop([],Server);
	{'EXIT',Server,killed} -> %% terminate
	    miniappl_loop(Client,[]);
	{'EXIT',Server,died} -> %% die
	    miniappl_loop(Client,[]);
	{dying,_Reason} ->
	    miniappl_loop(Client, Server);
	Other ->
	    exit({failed,Other})
    end.

%%%%%%%%%%%%%%%%%%%%
%% Client

client(N) ->
    io:format("Client starting...\n"),
    ok = public_request(),
    case N of
	1 -> public_kill();
	2 -> ?work_bridge_name ! die
    end,
    io:format("Killed server, terminating client...\n"),
    exit(fine).

%%%%%%%%%%%%%%%%%%%%
%% Non compliant server

start(N) ->
    supervisor_bridge:start_link({local,?bridge_name},?MODULE,N).

public_request() ->
    ?work_bridge_name ! {non_compliant_message,self()},
    io:format("Client waiting for answer...\n"),
    receive
	non_compliant_answer ->
	    ok
    end,
    io:format("Client got answer...\n").

public_kill() ->
    %% This func knows about supervisor_bridge
    exit(whereis(?work_bridge_name),kill).

init(1) ->
    ignore;
init(2) ->
    {error,testing};
init(3) ->
    %% This func knows about supervisor_bridge
    InternalPid = spawn_link(?MODULE,internal_loop_init,[self()]),
    receive
	{InternalPid,init_done} ->
	    {ok,InternalPid,self()}
    end;
init({4,Result}) ->
    Result.

internal_loop_init(Parent) ->
    register(?work_bridge_name, self()),
    Parent ! {self(),init_done},
    internal_loop({Parent,self()}).

internal_loop(State) ->
    receive
	{non_compliant_message,From} ->
	    io:format("Got request from ~p\n",[From]),
	    From ! non_compliant_answer,
	    internal_loop(State);
	die ->
	    exit(died)
    end.

terminate(Reason,{Parent,Worker}) ->
    %% This func knows about supervisor_bridge
    io:format("Terminating bridge...\n"),
    exit(Worker,kill),
    Parent ! {dying,Reason},
    anything;
terminate(_Reason, _State) ->
    any.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test various bad ways of starting a supervisor bridge.
badstart(Config) when is_list(Config) ->
    %% Various bad arguments.

    {'EXIT',_} =
	(catch supervisor_bridge:start_link({xxx,?bridge_name},?MODULE,1)),
    {'EXIT',_} =
	(catch supervisor_bridge:start_link({local,"foo"},?MODULE,1)),
    {'EXIT',_} =
	(catch supervisor_bridge:start_link(?bridge_name,?MODULE,1)),
    receive
	Msg ->
	    ct:fail({unexpected,Msg})
    after 1 ->
	    ok
    end,

    %% Already started.

    process_flag(trap_exit, true),
    {ok,Pid} =
	supervisor_bridge:start_link({local,?bridge_name},?MODULE,3),
    {error,{already_started,Pid}} =
	supervisor_bridge:start_link({local,?bridge_name},?MODULE,3),
    public_kill(),

    %% We used to wait 1 ms before retrieving the message queue,
    %% but that might not always be enough if the machine is overloaded.
    receive
	{'EXIT', Pid, killed} -> ok
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTP-9212. Restart of global supervisor.

%% Globally registered supervisor.
simple_global_supervisor(Config) when is_list(Config) ->

    Child = {child, {?MODULE,server9212,[]}, permanent, 2000, worker, []},
    InitResult = {ok, {{one_for_all,3,60}, [Child]}},
    {ok, Sup} =
        supervisor:start_link({local,bridge9212}, ?MODULE, {4,InitResult}),

    BN_1 = global:whereis_name(?bridge_name),
    exit(BN_1, kill),
    timer:sleep(200),
    BN_2 = global:whereis_name(?bridge_name),
    true = is_pid(BN_2),
    true = BN_1 =/= BN_2,

    process_flag(trap_exit, true),
    exit(Sup, kill),
    receive {'EXIT', Sup, killed} -> ok end,
    ok.

server9212() ->
    supervisor_bridge:start_link({global,?bridge_name}, ?MODULE, 3).
