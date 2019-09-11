%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

-module(lttng_SUITE).

-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([t_lttng_list/1,
         t_carrier_pool/1,
         t_memory_carrier/1,
         t_driver_control/1,
         t_driver_start_stop/1,
         t_driver_ready_input_output/1,
         t_driver_timeout/1,
         t_driver_caller/1,
         t_driver_flush/1,
         t_scheduler_poll/1]).

-export([ets_load/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [t_lttng_list,
     t_memory_carrier,
     t_carrier_pool,
     t_driver_start_stop,
     t_driver_ready_input_output,
     t_driver_control,
     t_driver_timeout,
     t_driver_caller,
     t_driver_flush,
     t_scheduler_poll].


init_per_suite(Config) ->
    case erlang:system_info(dynamic_trace) of
        lttng ->
            ensure_lttng_stopped("--all"),
            Config;
        _ ->
            {skip, "No LTTng configured on system."}
    end.

end_per_suite(_Config) ->
    ensure_lttng_stopped("--all"),
    ok.

init_per_testcase(Case, Config) ->
    Name = atom_to_list(Case),
    ok = ensure_lttng_started(Name, Config),
    [{session, Name}|Config].

end_per_testcase(Case, _Config) ->
    Name = atom_to_list(Case),
    ok = ensure_lttng_stopped(Name),
    ok.

%% Not tested yet
%%   org_erlang_otp:driver_process_exit

%% tracepoints
%%
%%   org_erlang_otp:carrier_pool_get
%%   org_erlang_otp:carrier_pool_put
%%   org_erlang_otp:carrier_destroy
%%   org_erlang_otp:carrier_create
%%   org_erlang_otp:aio_pool_put
%%   org_erlang_otp:aio_pool_get
%%   org_erlang_otp:driver_control
%%   org_erlang_otp:driver_call
%%   org_erlang_otp:driver_finish
%%   org_erlang_otp:driver_ready_async
%%   org_erlang_otp:driver_process_exit
%%   org_erlang_otp:driver_stop
%%   org_erlang_otp:driver_flush
%%   org_erlang_otp:driver_stop_select
%%   org_erlang_otp:driver_timeout
%%   org_erlang_otp:driver_ready_output
%%   org_erlang_otp:driver_ready_input
%%   org_erlang_otp:driver_output
%%   org_erlang_otp:driver_outputv
%%   org_erlang_otp:driver_init
%%   org_erlang_otp:driver_start
%%   org_erlang_otp:scheduler_poll

%%
%% Testcases
%%

t_lttng_list(_Config) ->
    {ok, _} = cmd("lttng list -u"),
    ok.

%% org_erlang_otp:carrier_pool_get
%% org_erlang_otp:carrier_pool_put
t_carrier_pool(Config) ->
    case have_carriers(ets_alloc) of
        false ->
            {skip, "No Memory Carriers configured on system."};
        true ->
            ok = lttng_start_event("org_erlang_otp:carrier_pool*", Config),

            ok = ets_load(Config),

            Res = lttng_stop_and_view(Config),
            ok = check_tracepoint("org_erlang_otp:carrier_pool_get", Res),
            ok = check_tracepoint("org_erlang_otp:carrier_pool_put", Res),
            ok
    end.

%% org_erlang_otp:carrier_destroy
%% org_erlang_otp:carrier_create
t_memory_carrier(Config) ->
    case have_carriers(ets_alloc) of
        false ->
            {skip, "No Memory Carriers configured on system."};
        true ->
            ok = lttng_start_event("org_erlang_otp:carrier_*", Config),

            ok = ets_load(Config),

            Res = lttng_stop_and_view(Config),
            ok = check_tracepoint("org_erlang_otp:carrier_destroy", Res),
            ok = check_tracepoint("org_erlang_otp:carrier_create", Res),
            ok
    end.

%% org_erlang_otp:driver_start
%% org_erlang_otp:driver_stop
t_driver_start_stop(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_*", Config),
    timer:sleep(500),
    os:cmd("echo hello"),
    timer:sleep(500),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_start", Res),
    ok = check_tracepoint("org_erlang_otp:driver_stop", Res),
    ok = check_tracepoint("org_erlang_otp:driver_output", Res),
    ok.

%% org_erlang_otp:driver_control
%% org_erlang_otp:driver_outputv
t_driver_control(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_control", Config),
    ok = lttng_start_event("org_erlang_otp:driver_outputv", Config),

    timer:sleep(500),
    Me = self(),
    Pid = spawn_link(fun() -> tcp_server(Me, active) end),
    receive {Pid, accept} -> ok end,
    Bin = txt(),
    Sz  = byte_size(Bin),

    {ok, Sock} = gen_tcp:connect("localhost", 5679, [binary, {packet, 2}]),
    ok = gen_tcp:send(Sock, <<Sz:16, Bin/binary>>),
    ok = gen_tcp:send(Sock, <<Sz:16, Bin/binary>>),
    ok = gen_tcp:close(Sock),
    receive {Pid, done} -> ok end,

    timer:sleep(500),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_control", Res),
    ok = check_tracepoint("org_erlang_otp:driver_outputv", Res),
    ok.

%% org_erlang_otp:driver_ready_input
%% org_erlang_otp:driver_ready_output
t_driver_ready_input_output(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_ready_*", Config),

    timer:sleep(500),
    Me = self(),
    Pid = spawn_link(fun() -> tcp_server(Me, active) end),
    receive {Pid, accept} -> ok end,
    Bin = txt(),
    Sz  = byte_size(Bin),

    {ok, Sock} = gen_tcp:connect("localhost", 5679, [binary, {packet, 2}]),
    ok = gen_tcp:send(Sock, <<Sz:16, Bin/binary>>),
    ok = gen_tcp:send(Sock, <<Sz:16, Bin/binary>>),
    ok = gen_tcp:close(Sock),
    receive {Pid, done} -> ok end,

    timer:sleep(500),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_ready_input", Res),
    ok = check_tracepoint("org_erlang_otp:driver_ready_output", Res),
    ok.


%% org_erlang_otp:driver_stop_select
%% org_erlang_otp:driver_timeout
t_driver_timeout(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_*", Config),
    Me = self(),
    Pid = spawn_link(fun() -> tcp_server(Me, timeout) end),
    receive {Pid, accept} -> ok end,
    {ok, Sock} = gen_tcp:connect("localhost", 5679, [binary]),
    ok = gen_tcp:send(Sock, <<"hej">>),
    receive {Pid, done} -> ok end,
    ok = gen_tcp:close(Sock),
    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_timeout", Res),
    ok = check_tracepoint("org_erlang_otp:driver_stop_select", Res),
    ok.
 
%% org_erlang_otp:driver_call
%% org_erlang_otp:driver_output
%% org_erlang_otp:driver_init
%% org_erlang_otp:driver_finish
t_driver_caller(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_*", Config),

    Drv = 'caller_drv',
    os:putenv("CALLER_DRV_USE_OUTPUTV", "false"),
    
    ok = load_driver(proplists:get_value(data_dir, Config), Drv),
    Port = open_port({spawn, Drv}, []),
    true = is_port(Port),

    chk_caller(Port, start, self()),
    chk_caller(Port, output, spawn_link(fun() ->
                                                port_command(Port, "")
                                        end)),
    Port ! {self(), {command, ""}},
    chk_caller(Port, output, self()),
    chk_caller(Port, control, spawn_link(fun () ->
                                                 port_control(Port, 0, "")
                                         end)),
    chk_caller(Port, call, spawn_link(fun() ->
                                              erlang:port_call(Port, 0, "")
                                      end)),
 
    true = port_close(Port),
    erl_ddll:unload_driver(Drv),

    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_call", Res),
    ok = check_tracepoint("org_erlang_otp:driver_output", Res),
    ok = check_tracepoint("org_erlang_otp:driver_init", Res),
    ok = check_tracepoint("org_erlang_otp:driver_finish", Res),
    ok.
 
%% org_erlang_otp:scheduler_poll
t_scheduler_poll(Config) ->
    ok = lttng_start_event("org_erlang_otp:scheduler_poll", Config),

    N = 100,

    Me = self(),
    Pid = spawn_link(fun() -> tcp_server(Me, {active, N*2}) end),
    receive {Pid, accept} -> ok end,

    %% We want to create a scenario where the fd is moved into a scheduler
    %% pollset, this means we have to send many small packages to the
    %% same socket, but not fast enough for them to all arrive at the
    %% same time.
    {ok, Sock} = gen_tcp:connect("localhost", 5679, [binary, {packet, 2}]),
    [begin gen_tcp:send(Sock,txt()), receive ok -> ok end end || _ <- lists:seq(1,N)],

    ok = memory_load(),

    [begin gen_tcp:send(Sock,txt()), receive ok -> ok end end || _ <- lists:seq(1,N)],

    ok = gen_tcp:close(Sock),
    Pid ! die,
    receive {Pid, done} -> ok end,

    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:scheduler_poll", Res),
    ok.

%% org_erlang_otp:driver_flush
t_driver_flush(Config) ->
    ok = lttng_start_event("org_erlang_otp:driver_flush", Config),

    Me = self(),
    Pid = spawn_link(fun() -> tcp_server(Me, passive_no_read) end),
    receive {Pid, accept} -> ok end,
    Bin = iolist_to_binary([txt() || _ <- lists:seq(1,100)]),
    Sz  = byte_size(Bin),

    %% We want to create a scenario where sendings stalls and we
    %% queue packets in the driver.
    %% When we close the socket it has to flush the queue.
    {ok, Sock} = gen_tcp:connect("localhost", 5679, [binary, {packet, 2},
                                                     {send_timeout, 10},
                                                     {sndbuf, 10000000}]),
    Pids = [spawn_link(fun() ->
                               gen_tcp:send(Sock, <<Sz:16, Bin/binary>>),
                               Me ! {self(), ok}
                       end) || _ <- lists:seq(1,100)],
    [receive {P, ok} -> ok end || P <- Pids],
    ok = gen_tcp:close(Sock),
    Pid ! die,
    receive {Pid, done} -> ok end,

    Res = lttng_stop_and_view(Config),
    ok = check_tracepoint("org_erlang_otp:driver_flush", Res),
    ok.

%%
%% AUX
%%

chk_caller(Port, Callback, ExpectedCaller) ->
    receive
        {caller, Port, Callback, Caller} ->
            ExpectedCaller = Caller
    end.

memory_load() ->
    Me = self(),
    Pids0 = [spawn_link(fun() -> memory_loop(Me, 20, <<42>>) end) || _ <- lists:seq(1,30)],
    timer:sleep(50),
    Pids1 = [spawn_link(fun() -> memory_loop(Me, 20, <<42>>) end) || _ <- lists:seq(1,30)],
    [receive {Pid, done} -> ok end || Pid <- Pids0 ++ Pids1],
    timer:sleep(500),
    ok.

memory_loop(Parent, N, Bin) ->
    memory_loop(Parent, N, Bin, []).

memory_loop(Parent, 0, _Bin, _) ->
    Parent ! {self(), done};
memory_loop(Parent, N, Bin0, Ls) ->
    Bin = binary:copy(<<Bin0/binary, Bin0/binary>>),
    memory_loop(Parent, N - 1, Bin, [a,b,c|Ls]).

ets_load(Config) ->

    %% Have to do on a fresh node to guarantee that carriers are created
    {ok,Node} = start_node(Config),

    Res = rpc:call(Node, ?MODULE, ets_load, []),

    stop_node(Node),

    Res.

ets_load() ->

    Tid = ets:new(ets_load, [public,set]),
    N = erlang:system_info(schedulers_online),
    Pids = [spawn_link(fun() -> ets_shuffle(Tid) end) || _ <- lists:seq(1,N)],
    ok = ets_kill(Pids, 500),
    ok.


ets_kill([], _) -> ok;
ets_kill([Pid|Pids], Time) ->
    timer:sleep(Time),
    Pid ! done,
    ets_kill(Pids, Time).

ets_shuffle(Tid) ->
    Payload = lists:duplicate(100, $x),
    ets_shuffle(Tid, 100, Payload).
ets_shuffle(Tid, I, Data) ->
    ets_shuffle(Tid, I, I, Data, Data).

ets_shuffle(Tid, 0, N, _, Data) ->
    ets_shuffle(Tid, N, N, Data, Data);
ets_shuffle(Tid, I, N, Data, Data0) ->
    receive
        done -> ok
    after 0 ->
              Key = rand:uniform(1000),
              Data1 = [I|Data],
              ets:insert(Tid, {Key, Data1}),
              ets_shuffle(Tid, I - 1, N, Data1, Data0)
    end.

tcp_server(Pid, Type) ->
    {ok, LSock} = gen_tcp:listen(5679, [binary,
                                        {reuseaddr, true},
                                        {active, false}]),
    Pid ! {self(), accept},
    {ok, Sock} = gen_tcp:accept(LSock),
    case Type of
        passive_no_read ->
            receive die -> ok end;
        active ->
            inet:setopts(Sock, [{active, 2}, {packet,2}]),
            receive Msg1 -> io:format("msg1: ~p~n", [Msg1]) end,
            receive Msg2 -> io:format("msg2: ~p~n", [Msg2]) end,
            ok = gen_tcp:close(Sock);
        timeout ->
            Res = gen_tcp:recv(Sock, 2000, 1000),
            io:format("res ~p~n", [Res]);
        {active, Number} when is_number(Number) ->
            inet:setopts(Sock, [{active, Number}, {packet,2}]),
            [begin
                 receive _Msg1 -> Pid ! ok, io:format("msg ~p~n", [I]) end
             end || I <- lists:seq(1,Number)],
            ok = gen_tcp:close(Sock)
    end,
    Pid ! {self(), done},
    ok.

txt() ->
    <<"%% tracepoints\n"
      "%%\n"
      "%%   org_erlang_otp:carrier_pool_get\n"
      "%%   org_erlang_otp:carrier_pool_put\n"
      "%%   org_erlang_otp:carrier_destroy\n"
      "%%   org_erlang_otp:carrier_create\n"
      "%%   org_erlang_otp:aio_pool_put\n"
      "%%   org_erlang_otp:aio_pool_get\n"
      "%%   org_erlang_otp:driver_control\n"
      "%%   org_erlang_otp:driver_call\n"
      "%%   org_erlang_otp:driver_finish\n"
      "%%   org_erlang_otp:driver_ready_async\n"
      "%%   org_erlang_otp:driver_process_exit\n"
      "%%   org_erlang_otp:driver_stop\n"
      "%%   org_erlang_otp:driver_flush\n"
      "%%   org_erlang_otp:driver_stop_select\n"
      "%%   org_erlang_otp:driver_timeout\n"
      "%%   org_erlang_otp:driver_ready_output\n"
      "%%   org_erlang_otp:driver_ready_input\n"
      "%%   org_erlang_otp:driver_output\n"
      "%%   org_erlang_otp:driver_outputv\n"
      "%%   org_erlang_otp:driver_init\n"
      "%%   org_erlang_otp:driver_start\n"
      "%%   org_erlang_otp:scheduler_poll">>.

load_driver(Dir, Driver) ->
    case erl_ddll:load_driver(Dir, Driver) of
        ok -> ok;
        {error, Error} = Res ->
            io:format("~s\n", [erl_ddll:format_error(Error)]),
            Res
    end.

%% check

have_carriers(Alloc) ->
    case erlang:system_info({allocator,Alloc}) of
        false -> false;
        _ -> true
    end.

have_async_threads() ->
    Tps = erlang:system_info(thread_pool_size),
    if Tps =:= 0 -> false;
       true -> true
    end.

%% lttng
lttng_stop_and_view(Config) ->
    Path = proplists:get_value(priv_dir, Config),
    Name = proplists:get_value(session, Config),
    {ok,_} = cmd("lttng stop " ++ Name),
    {ok,Res} = cmd("lttng view " ++ Name ++ " --trace-path=" ++ Path),
    Res.

check_tracepoint(TP, Data) ->
    case re:run(Data, TP, [global]) of
        {match, _} -> ok;
        _ -> notfound
    end.

lttng_start_event(Event, Config) ->
    Name = proplists:get_value(session, Config),
    {ok, _} = cmd("lttng enable-event -u " ++ Event ++ " --session=" ++ Name),
    {ok, _} = cmd("lttng start " ++ Name),
    ok.

ensure_lttng_started(Name, Config) ->
    Out = case proplists:get_value(priv_dir, Config) of
              undefined -> [];
              Path -> "--output="++Path++" "
          end,
    {ok,_} = cmd("lttng create " ++ Out ++ Name),
    ok.

ensure_lttng_stopped(Name) ->
    {ok,_} = cmd("lttng stop"),
    {ok,_} = cmd("lttng destroy " ++ Name),
    ok.

cmd(Cmd) ->
    io:format("<< ~ts~n", [Cmd]),
    Res = os:cmd(Cmd),
    io:format(">> ~ts~n", [Res]),
    {ok,Res}.

start_node(Config) ->
    start_node(Config, "").

start_node(Config, Args) when is_list(Config) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-"
			++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-"
			++ integer_to_list(erlang:system_time(second))
			++ "-"
			++ integer_to_list(erlang:unique_integer([positive]))),
    test_server:start_node(Name, slave, [{args, "-pa "++Pa++" "++Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).
