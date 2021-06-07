%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
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
         simple_global_supervisor/1, format_log_1/1, format_log_2/1]).
-export([client/1,init/1,internal_loop_init/1,terminate/2,server9212/0]).

-include_lib("common_test/include/ct.hrl").
-define(bridge_name,supervisor_bridge_SUITE_server).
-define(work_bridge_name,work_supervisor_bridge_SUITE_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [starting, mini_terminate, mini_die, badstart, simple_global_supervisor,
     format_log_1, format_log_2].

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

%% Test report callback for Logger handler error_logger
format_log_1(_Config) ->
    FD = application:get_env(kernel, error_logger_format_depth),
    application:unset_env(kernel, error_logger_format_depth),
    Term = lists:seq(1, 15),
    Supervisor = my_supervisor,
    Name = self(),
    Error = error,
    Offender = [{pid,Name},{mod,a_module}],
    Report = #{label=>{supervisor,Error},
               report=>[{supervisor,Supervisor},
                        {errorContext,Error},
                        {reason,Term},
                        {offender,Offender}]},
    {F1, A1} = supervisor_bridge:format_log(Report),
    FExpected1 = "    supervisor: ~tp~n"
        "    errorContext: ~tp~n"
        "    reason: ~tp~n"
        "    offender: ~tp~n",
    ct:log("F1: ~ts~nA1: ~tp", [F1,A1]),
    FExpected1 = F1,
    [Supervisor,Error,Term,Offender] = A1,

    Started = [{pid,Name},{mfa,{a_module,init,[Term]}}],
    Progress = #{label=>{supervisor,progress},
                 report=>[{supervisor,Supervisor},{started,Started}]},
    {PF1,PA1} = supervisor_bridge:format_log(Progress),
    PFExpected1 = "    supervisor: ~tp~n    started: ~tp~n",
    ct:log("PF1: ~ts~nPA1: ~tp", [PF1,PA1]),
    PFExpected1 = PF1,
    [Supervisor,Started] = PA1,

    Depth = 10,
    ok = application:set_env(kernel, error_logger_format_depth, Depth),
    Limited = [1,2,3,4,5,6,7,8,9,'...'],
    {F2,A2} = supervisor_bridge:format_log(Report),
    FExpected2 = "    supervisor: ~tP~n"
        "    errorContext: ~tP~n"
        "    reason: ~tP~n"
        "    offender: ~tP~n",
    ct:log("F2: ~ts~nA2: ~tp", [F2,A2]),
    FExpected2 = F2,
    LimitedOffender = Offender,
    [Supervisor,Depth,Error,Depth,Limited,Depth,LimitedOffender,Depth] = A2,

    LimitedStarted =
        [{pid,Name},{mfa,{a_module,init,[[1,2,3,4,5,6,7,8,9,'...']]}}],
    {PF2,PA2} = supervisor_bridge:format_log(Progress),
    PFExpected2 = "    supervisor: ~tP~n    started: ~tP~n",
    ct:log("PF2: ~ts~nPA2: ~tp", [PF2,PA2]),
    PFExpected2 = PF2,
    [Supervisor,Depth,LimitedStarted,Depth] = PA2,

    case FD of
        undefined ->
            application:unset_env(kernel, error_logger_format_depth);
        _ ->
            application:set_env(kernel, error_logger_format_depth, FD)
    end,
    ok.

%% Test report callback for any Logger handler
format_log_2(_Config) ->
    Term = lists:seq(1, 15),
    Supervisor = my_supervisor,
    Name = self(),
    NameStr = pid_to_list(Name),
    Error = shutdown_error,
    Offender = [{pid,Name},{mod,a_module}],
    Report = #{label=>{supervisor,Error},
               report=>[{supervisor,Supervisor},
                        {errorContext,Error},
                        {reason,Term},
                        {offender,Offender}]},
    FormatOpts1 = #{},
    Str1 = flatten_format_log(Report, FormatOpts1),
    L1 = length(Str1),
    Expected1 = "    supervisor: my_supervisor\n"
        "    errorContext: shutdown_error\n"
        "    reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "    offender: [{pid,"++NameStr++"},{mod,a_module}]\n",
    ct:log("Str1: ~ts", [Str1]),
    ct:log("length(Str1): ~p", [L1]),
    true = Expected1 =:= Str1,

    Started = [{pid,Name},{mfa,{a_module,init,[Term]}}],
    Progress = #{label=>{supervisor,progress},
                 report=>[{supervisor,Supervisor},{started,Started}]},
    PStr1 = flatten_format_log(Progress, FormatOpts1),
    PL1 = length(PStr1),
    PExpected1 = "    supervisor: my_supervisor\n"
        "    started: [{pid,"++NameStr++"},\n"
        "              {mfa,{a_module,init,"
                           "[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]}}]\n",
    ct:log("PStr1: ~ts", [PStr1]),
    ct:log("length(PStr1): ~p", [PL1]),
    true = PExpected1 =:= PStr1,

    Depth = 10,
    FormatOpts2 = #{depth=>Depth},
    Str2 = flatten_format_log(Report, FormatOpts2),
    L2 = length(Str2),
    Expected2 = "    supervisor: my_supervisor\n"
        "    errorContext: shutdown_error\n"
        "    reason: [1,2,3,4,5,6,7,8,9|...]\n"
        "    offender: [{pid,"++NameStr++"},{mod,a_module}]\n",
    ct:log("Str2: ~ts", [Str2]),
    ct:log("length(Str2): ~p", [L2]),
    true = Expected2 =:= Str2,

    PStr2 = flatten_format_log(Progress, FormatOpts2),
    PL2 = length(PStr2),
    PExpected2 = "    supervisor: my_supervisor\n"
        "    started: [{pid,"++NameStr++"},"
        "{mfa,{a_module,init,[[1|...]]}}]\n",
    ct:log("PStr2: ~ts", [PStr2]),
    ct:log("length(PStr2): ~p", [PL2]),
    true = PExpected2 =:= PStr2,

    FormatOpts3 = #{chars_limit=>200},
    Str3 = flatten_format_log(Report, FormatOpts3),
    L3 = length(Str3),
    Expected3 = "    supervisor: my_supervisor\n"
        "    errorContext: shutdown_error\n"
        "    reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]\n"
        "    offender: [{pid,"++NameStr++"},{mod,a_module}]\n",
    ct:log("Str3: ~ts", [Str3]),
    ct:log("length(Str3): ~p", [L3]),
    true = Expected3 =:= Str3,

    PFormatOpts3 = #{chars_limit=>80},
    PStr3 = flatten_format_log(Progress, PFormatOpts3),
    PL3 = length(PStr3),
    PExpected3 = "    supervisor: my_supervisor\n    started:",
    ct:log("PStr3: ~ts", [PStr3]),
    ct:log("length(PStr3): ~p", [PL3]),
    true = lists:prefix(PExpected3, PStr3),
    true = PL3 < PL1,

    FormatOpts4 = #{single_line=>true},
    Str4 = flatten_format_log(Report, FormatOpts4),
    L4 = length(Str4),
    Expected4 = "Supervisor: my_supervisor. Context: shutdown_error. "
        "Reason: [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]. "
        "Offender: pid="++NameStr++",mod=a_module.",
    ct:log("Str4: ~ts", [Str4]),
    ct:log("length(Str4): ~p", [L4]),
    true = Expected4 =:= Str4,

    PStr4 = flatten_format_log(Progress, FormatOpts4),
    PL4 = length(PStr4),
    PExpected4 = "Supervisor: my_supervisor. "
        "Started: pid="++NameStr++",mfa={a_module,init,"
                "[[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]}.",
    ct:log("PStr4: ~ts", [PStr4]),
    ct:log("length(PStr4): ~p", [PL4]),
    true = PExpected4 =:= PStr4,

    FormatOpts5 = #{single_line=>true, depth=>Depth},
    Str5 = flatten_format_log(Report, FormatOpts5),
    L5 = length(Str5),
    Expected5 = "Supervisor: my_supervisor. Context: shutdown_error. "
        "Reason: [1,2,3,4,5,6,7,8,9|...]. "
        "Offender: pid="++NameStr++",mod=a_module.",
    ct:log("Str5: ~ts", [Str5]),
    ct:log("length(Str5): ~p", [L5]),
    true = Expected5 =:= Str5,

    PStr5 = flatten_format_log(Progress, FormatOpts5),
    PL5 = length(PStr5),
    PExpected5 = "Supervisor: my_supervisor. "
        "Started: pid="++NameStr++",mfa={a_module,init,[[1,2,3,4,5|...]]}.",
    ct:log("PStr5: ~ts", [PStr5]),
    ct:log("length(PStr5): ~p", [PL5]),
    true = PExpected5 =:= PStr5,

    FormatOpts6 = #{single_line=>true, chars_limit=>100},
    Str6 = flatten_format_log(Report, FormatOpts6),
    L6 = length(Str6),
    Expected6 = "Supervisor: my_supervisor. Context:",
    ct:log("Str6: ~ts", [Str6]),
    ct:log("length(Str6): ~p", [L6]),
    true = lists:prefix(Expected6, Str6),
    true = L6 < L4,

    PFormatOpts6 = #{single_line=>true, chars_limit=>60},
    PStr6 = flatten_format_log(Progress, PFormatOpts6),
    PL6 = length(PStr6),
    PExpected6 = "Supervisor: my_supervisor.",
    ct:log("PStr6: ~ts", [PStr6]),
    ct:log("length(PStr6): ~p", [PL6]),
    true = lists:prefix(PExpected6, PStr6),
    true = PL6 < PL4,

    ok.

flatten_format_log(Report, Format) ->
    lists:flatten(supervisor_bridge:format_log(Report, Format)).
