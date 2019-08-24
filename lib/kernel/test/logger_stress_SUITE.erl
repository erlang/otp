%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_stress_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct_event.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/src/logger_h_common.hrl").

-ifdef(SAVE_STATS).
  -define(COLLECT_STATS(_All_,_Procs_),
          ct:pal("~p",[stats(_All_,_Procs_)])).
-else.
  -define(COLLECT_STATS(_All_,_Procs__), ok).
-endif.

-define(TEST_DURATION,120). % seconds

suite() ->
    [{timetrap,{minutes,3}},
     {ct_hooks,[logger_test_lib]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [allow_events,
     reject_events,
     std_handler,
     disk_log_handler,
     std_handler_time,
     std_handler_time_big,
     disk_log_handler_time,
     disk_log_handler_time_big,
     emulator_events,
     remote_events,
     remote_to_disk_log,
     remote_emulator_events,
     remote_emulator_to_disk_log].

%%%-----------------------------------------------------------------
%%% Test cases
%%%-----------------------------------------------------------------
%% Time from log macro call to handler callback
allow_events(Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(Config,
                              [{logger,
                                [{handler,default,?MODULE,#{}}]},
                               {logger_level,notice}]),
    N = 100000,
    {T,_} = timer:tc(fun() -> rpc:call(Node,?MODULE,nlogs,[N]) end),
    IOPS = N * 1000/T, % log events allowed per millisecond
    ct_event:notify(#event{name = benchmark_data,
                           data = [{value,IOPS}]}),
    {comment,io_lib:format("~.2f accepted events pr millisecond",
                           [IOPS])}.

%% Time from log macro call to reject (log level)
reject_events(Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(Config,
                              [{logger,
                                [{handler,default,?MODULE,#{}}]},
                               {logger_level,error}]),
    N = 1000000,
    {T,_} = timer:tc(fun() -> rpc:call(Node,?MODULE,nlogs,[N]) end),
    IOPS = N * 1000/T, % log events rejected per millisecond
    ct_event:notify(#event{name = benchmark_data,
                           data = [{value,IOPS}]}),
    {comment,io_lib:format("~.2f rejected events pr millisecond",
                           [IOPS])}.

%% Cascading failure that produce gen_server and proc_lib reports -
%% how many of the produced log events are actually written to a log
%% with logger_std_h file handler.
std_handler(Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(Config,
                              [{logger,
                                [{handler,default,logger_std_h,
                                  #{config=>#{type=>{file,"default.log"}}}}]}]),

    cascade({Node,{logger_backend,log_allowed,2},[]},
            {Node,{logger_std_h,write,4},[{default,logger_std_h_default}]},
            fun otp_cascading/0).
std_handler(cleanup,_Config) ->
    _ = file:delete("default.log"),
    ok.

%% Disable overload protection and just print a lot - measure time.
%% The IOPS reported is the number of log events written per millisecond.
std_handler_time(Config) ->
    measure_handler_time(logger_std_h,#{type=>{file,"default.log"}},Config).
std_handler_time(cleanup,_Config) ->
    _ = file:delete("default.log"),
    ok.

std_handler_time_big(Config) ->
    measure_handler_time_big(logger_std_h,#{type=>{file,"default.log"}},Config).
std_handler_time_big(cleanup,_Config) ->
    _ = file:delete("default.log"),
    ok.

%% Cascading failure that produce gen_server and proc_lib reports -
%% how many of the produced log events are actually written to a log
%% with logger_disk_log_h wrap file handler.
disk_log_handler(Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(Config,
                              [{logger,
                                [{handler,default,logger_disk_log_h,#{}}]}]),
    cascade({Node,{logger_backend,log_allowed,2},[]},
            {Node,{logger_disk_log_h,write,4},
             [{default,logger_disk_log_h_default}]},
            fun otp_cascading/0).
disk_log_handler(cleanup,_Config) ->
    Files = filelib:wildcard("default.log.*"),
    [_ = file:delete(F) || F <- Files],
    ok.

%% Disable overload protection and just print a lot - measure time.
%% The IOPS reported is the number of log events written per millisecond.
disk_log_handler_time(Config) ->
    measure_handler_time(logger_disk_log_h,#{type=>halt},Config).
disk_log_handler_time(cleanup,_Config) ->
    _ = file:delete("default"),
    ok.

disk_log_handler_time_big(Config) ->
    measure_handler_time_big(logger_disk_log_h,#{type=>halt},Config).
disk_log_handler_time_big(cleanup,_Config) ->
    _ = file:delete("default"),
    ok.

%% Cascading failure that produce log events from the emulator - how
%% many of the produced log events pass through the proxy.
emulator_events(Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(Config,
                              [{logger,
                                [{handler,default,?MODULE,#{}}]}]),
    cascade({Node,{?MODULE,producer,0},[]},
            {Node,{?MODULE,log,2},[{proxy,logger_proxy}]},
            fun em_cascading/0).

%% Cascading failure that produce gen_server and proc_lib reports on
%% remote node - how many of the produced log events pass through the
%% proxy.
remote_events(Config) ->
    {ok,_,Node1} =
        logger_test_lib:setup([{postfix,1}|Config],
                              [{logger,
                                [{handler,default,?MODULE,#{}}]}]),
    {ok,_,Node2} =
        logger_test_lib:setup([{postfix,2}|Config],[]),
    cascade({Node2,{logger_backend,log_allowed,2},[{remote_proxy,logger_proxy}]},
            {Node1,{?MODULE,log,2},[{local_proxy,logger_proxy}]},
            fun otp_cascading/0).

%% Cascading failure that produce gen_server and proc_lib reports on
%% remote node - how many of the produced log events are actually
%% written to a log with logger_disk_log_h wrap file handler.
remote_to_disk_log(Config) ->
    {ok,_,Node1} =
        logger_test_lib:setup([{postfix,1}|Config],
                              [{logger,
                                [{handler,default,logger_disk_log_h,#{}}]}]),
    {ok,_,Node2} =
        logger_test_lib:setup([{postfix,2}|Config],[]),
    cascade({Node2,{logger_backend,log_allowed,2},[{remote_proxy,logger_proxy}]},
            {Node1,{logger_disk_log_h,write,4},
             [{local_proxy,logger_proxy},
              {local_default,logger_disk_log_h_default}]},
            fun otp_cascading/0).
remote_to_disk_log(cleanup,_Config) ->
    Files = filelib:wildcard("default.log.*"),
    [_ = file:delete(F) || F <- Files],
    ok.

%% Cascading failure that produce log events from the emulator on
%% remote node - how many of the produced log events pass through the
%% proxy.
remote_emulator_events(Config) ->
    {ok,_,Node1} =
        logger_test_lib:setup([{postfix,1}|Config],
                              [{logger,
                                [{handler,default,?MODULE,#{}}]}]),
    {ok,_,Node2} =
        logger_test_lib:setup([{postfix,2}|Config],[]),
    cascade({Node2,{?MODULE,producer,0},[{remote_proxy,logger_proxy}]},
            {Node1,{?MODULE,log,2},[{local_proxy,logger_proxy}]},
            fun em_cascading/0).

%% Cascading failure that produce log events from the emulator on
%% remote node - how many of the produced log events are actually
%% written to a log with logger_disk_log_h wrap file handler.
remote_emulator_to_disk_log(Config) ->
    {ok,_,Node1} =
        logger_test_lib:setup([{postfix,1}|Config],
                              [{logger,
                                [{handler,default,logger_disk_log_h,#{}}]}]),
    {ok,_,Node2} =
        logger_test_lib:setup([{postfix,2}|Config],[]),
    cascade({Node2,{?MODULE,producer,0},[{remote_proxy,logger_proxy}]},
            {Node1,{logger_disk_log_h,write,4},
             [{local_proxy,logger_proxy},
              {local_default,logger_disk_log_h_default}]},
            fun em_cascading/0).
remote_emulator_to_disk_log(cleanup,_Config) ->
    Files = filelib:wildcard("default.log.*"),
    [_ = file:delete(F) || F <- Files],
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
measure_handler_time(Module,HCfg,Config) ->
    measure_handler_time(Module,100000,small_fa(),millisecond,HCfg,#{},Config).

measure_handler_time_big(Module,HCfg,Config) ->
    FCfg = #{chars_limit=>4096, max_size=>1024},
    measure_handler_time(Module,100,big_fa(),second,HCfg,FCfg,Config).

measure_handler_time(Module,N,FA,Unit,HCfg,FCfg,Config) ->
    {ok,_,Node} =
        logger_test_lib:setup(
          Config,
          [{logger,
            [{handler,default,Module,
              #{formatter => {logger_formatter,
                              maps:merge(#{legacy_header=>false,
                                           single_line=>true},FCfg)},
                config=>maps:merge(#{sync_mode_qlen => N+1,
                                     drop_mode_qlen => N+1,
                                     flush_qlen => N+1,
                                     burst_limit_enable => false,
                                     filesync_repeat_interval => no_repeat},
                                   HCfg)}}]}]),
    %% HPid = rpc:call(Node,erlang,whereis,[?name_to_reg_name(Module,default)]),
    %% {links,[_,FCPid]} = rpc:call(Node,erlang,process_info,[HPid,links]),
    T0 = erlang:monotonic_time(millisecond),
    ok = rpc:call(Node,?MODULE,nlogs_wait,[N,FA,Module]),
    %% ok = rpc:call(Node,fprof,apply,[fun ?MODULE:nlogs_wait/2,[N div 10,FA,Module],[{procs,[HPid,FCPid]}]]),
    T1 = erlang:monotonic_time(millisecond),
    T = T1-T0,
    M = case Unit of
            millisecond -> 1;
            second -> 1000
        end,
    IOPS = M*N/T,
    ct:pal("N: ~p~nT: ~p~nIOPS: ~.2f events pr ~w",[N,T,IOPS,Unit]),
    %% Stats = rpc:call(Node,logger_olp,info,[?name_to_reg_name(Module,default)]),
    %% ct:pal("Stats: ~p",[Stats]),
    ct_event:notify(#event{name = benchmark_data,
                           data = [{value,IOPS}]}),
    {comment,io_lib:format("~.2f events written pr ~w",[IOPS,Unit])}.

nlogs_wait(N,{F,A},Module) ->
    group_leader(whereis(user),self()),
    [?LOG_NOTICE(F,A) || _ <- lists:seq(1,N)],
    wait(Module).

wait(Module) ->
    case Module:filesync(default) of
        {error,handler_busy} ->
            wait(Module);
        ok ->
            ok
    end.

small_fa() ->
    Str = "\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "[\\]^_`abcdefghijklmnopqr",
    {"~ts",[Str]}.

big_fa() ->
    {"~p",[lists:duplicate(1048576,"a")]}.

nlogs(N) ->
    group_leader(whereis(user),self()),
    Str = "\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "[\\]^_`abcdefghijklmnopqr",
    [?LOG_NOTICE(Str) || _ <- lists:seq(1,N)],
    ok.

%% cascade(ProducerInfo,ConsumerInfo,TestFun)
cascade({PNode,PMFA,_PStatProcs},{CNode,CMFA,_CStatProcs},TestFun) ->
    Tab  = ets:new(counter,[set,public]),
    ets:insert(Tab,{producer,0}),
    ets:insert(Tab,{consumer,0}),
    dbg:tracer(process,{fun tracer/2,{Tab,PNode,CNode}}),
    dbg:n(PNode),
    dbg:n(CNode),
    dbg:cn(node()),
    dbg:p(all,[call,arity]),
    dbg:tpl(PMFA,[]),
    dbg:tpl(CMFA,[]),

    Pid = rpc:call(CNode,?MODULE,wrap_test,[PNode,TestFun]),
    MRef = erlang:monitor(process,Pid),
    TO = ?TEST_DURATION*1000,
    receive {'DOWN',MRef,_,_,Reason} ->
            ct:fail({remote_pid_down,Reason})
    after TO ->
            All = ets:lookup_element(Tab,producer,2),
            Written = ets:lookup_element(Tab,consumer,2),
            dbg:stop_clear(),
            ?COLLECT_STATS(All,
                           [{PNode,P,Id} || {Id,P} <- _PStatProcs] ++
                               [{CNode,P,Id} || {Id,P} <- _CStatProcs]),
            Ratio = Written/All * 100,
            ct_event:notify(#event{name = benchmark_data,
                                   data = [{value,Ratio}]}),
            {comment,io_lib:format("~p % (~p written, ~p produced)",
                                   [round(Ratio),Written,All])}
    end.

wrap_test(Fun) ->
    wrap_test(node(),Fun).
wrap_test(Node,Fun) ->
    reset(),
    group_leader(whereis(user),self()),
    rpc:call(Node,?MODULE,do_fun,[Fun]).

do_fun(Fun) ->
    reset(),
    Fun().

reset() ->
    reset([logger_std_h_default, logger_disk_log_h_default, logger_proxy]).
reset([P|Ps]) ->
    is_pid(whereis(P)) andalso logger_olp:reset(P),
    reset(Ps);
reset([]) ->
    ok.


tracer({trace,_,call,{?MODULE,producer,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,producer,1),
    S;
tracer({trace,Pid,call,{logger_backend,log_allowed,_}},{Tab,PNode,_CNode}=S) when node(Pid)=:=PNode ->
    ets:update_counter(Tab,producer,1),
    S;
tracer({trace,_,call,{?MODULE,log,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,consumer,1),
    S;
tracer({trace,_,call,{_,write,_}},{Tab,_PNode,_CNode}=S) ->
    ets:update_counter(Tab,consumer,1),
    S;
tracer(_,S) ->
    S.


%%%-----------------------------------------------------------------
%%% Collect statistics
-define(STAT_KEYS,
        [burst_drops,
         calls,
         casts,
         drops,
         flushed,
         flushes,
         freq,
         last_qlen,
         max_qlen,
         time,
         writes]).
-define(EVENT_KEYS,
        [calls,casts,flushed]).

stats(All,Procs) ->
    NI = [{Id,rpc:call(N,logger_olp,info,[P])} || {N,P,Id}<-Procs],
    [{all,All}|[stats(Id,I,All) || {Id,I} <- NI]].

stats(Id,Info,All) ->
    S = maps:with(?STAT_KEYS,Info),
    AllOnProc = lists:sum(maps:values(maps:with(?EVENT_KEYS,S))),
    if All>0 ->
            Writes = maps:get(writes,S),
            {_,ActiveTime} = maps:get(time,S),
            Rate = round(100*Writes/All),
            RateOnProc =
                if AllOnProc>0 ->
                        round(100*Writes/AllOnProc);
                   true ->
                        0
                end,
            AvFreq =
                if ActiveTime>0 ->
                        round(Writes/ActiveTime);
                   true ->
                        0
                end,
            {Id,
             {stats,S},
             {rate,Rate},
             {rate_on_proc,RateOnProc},
             {av_freq,AvFreq}};
       true ->
            {Id,none}
    end.

%%%-----------------------------------------------------------------
%%% Spawn a lot of processes that crash repeatedly, causing a lot of
%%% error reports from the emulator.
em_cascading() ->
    spawn(fun() -> super() end).

super() ->
    process_flag(trap_exit,true),
    spawn_link(fun server/0),
    [spawn_link(fun client/0) || _<-lists:seq(1,10000)],
    super_loop().

super_loop() ->
    receive
        {'EXIT',_,server} ->
            spawn_link(fun server/0),
            super_loop();
        {'EXIT',_,_} ->
            _L = lists:sum(lists:seq(1,10000)),
            spawn_link(fun client/0),
            super_loop()
    end.

client() ->
    receive
    after 1 ->
            case whereis(server) of
                Pid when is_pid(Pid) ->
                    ok;
                undefined ->
                    producer(),
                    erlang:error(some_exception)
            end
    end,
    client().

server() ->
    register(server,self()),
    receive
    after 3000 ->
            exit(server)
    end.
        

%%%-----------------------------------------------------------------
%%% Create a supervisor tree with processes that crash repeatedly,
%%% causing a lot of supervisor reports and crashreports
otp_cascading() ->
    {ok,Pid} = supervisor:start_link({local,otp_super}, ?MODULE, [otp_super]),
    unlink(Pid),
    Pid.

otp_server_sup() ->
    supervisor:start_link({local,otp_server_sup},?MODULE,[otp_server_sup]).

otp_client_sup(N) ->
    supervisor:start_link({local,otp_client_sup},?MODULE,[otp_client_sup,N]).

otp_server() ->
    gen_server:start_link({local,otp_server},?MODULE,[otp_server],[]).

otp_client() ->
    gen_server:start_link(?MODULE,[otp_client],[]).

init([otp_super]) ->
    {ok, {{one_for_one, 200, 10},
            [{client_sup,
                    {?MODULE, otp_client_sup, [10000]},
                    permanent, 1000, supervisor, [?MODULE]},
             {server_sup,
                    {?MODULE, otp_server_sup, []},
                    permanent, 1000, supervisor, [?MODULE]}
            ]}};
init([otp_server_sup]) ->
    {ok, {{one_for_one, 2, 10},
            [{server,
                    {?MODULE, otp_server, []},
                    permanent, 1000, worker, [?MODULE]}
            ]}};
init([otp_client_sup,N]) ->
    spawn(fun() ->
                  [supervisor:start_child(otp_client_sup,[])
                   || _ <- lists:seq(1,N)]
          end),
    {ok, {{simple_one_for_one, N*10, 1},
            [{client,
                    {?MODULE, otp_client, []},
                    permanent, 1000, worker, [?MODULE]}
            ]}};
init([otp_server]) ->
    {ok, server, 10000};
init([otp_client]) ->
    {ok, client,1}.

handle_info(timeout, client) ->
    true = is_pid(whereis(otp_server)),
    {noreply,client,1};
handle_info(timeout, server) ->
    exit(self(), some_error).

%%%-----------------------------------------------------------------
%%% Logger callbacks
log(_LogEvent,_Config) ->
    ok.

%%%-----------------------------------------------------------------
%%% Function to trace on for counting produced emulator messages
producer() ->
    ok.
