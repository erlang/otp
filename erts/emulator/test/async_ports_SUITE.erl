-module(async_ports_SUITE).

-export([all/0, suite/0]).
-export([permanent_busy_test/1]).
-export([run_loop/5]).

-include_lib("common_test/include/ct.hrl").

-define(PACKET_SIZE, (10 * 1024 * 8)).
-define(CPORT_DELAY, 100).
-define(TEST_LOOPS_COUNT, 100000).
-define(SLEEP_BEFORE_CHECK, 1000).
-define(TEST_PROCS_COUNT, 2).
-define(TC_TIMETRAP_SECONDS, 10).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, ?TC_TIMETRAP_SECONDS}}].

all() ->
    [permanent_busy_test].

permanent_busy_test(Config) ->
    ExePath = filename:join(proplists:get_value(data_dir, Config), "cport"),
    Self = self(),
    spawn_link(
      fun() ->
              Block = <<0:?PACKET_SIZE>>,

              Port = open_port(ExePath),
              
              Testers = lists:map(
                          fun(_) ->
                                  spawn_link(?MODULE, run_loop,
                                             [Self,
                                              Port,
                                              Block,
                                              ?TEST_LOOPS_COUNT,
                                              0])
                          end,
                          lists:seq(1, ?TEST_PROCS_COUNT)),
              Self ! {test_info, Port, Testers},
              endless_flush(Port)
      end),

    receive
        {test_info, Port, Testers} ->
            MaxWaitTime = round(0.7 * ?TC_TIMETRAP_SECONDS * 1000),
            ct:log("wait testers, maximum ~w mcsec~n", [MaxWaitTime]),
            ok = wait_testers(MaxWaitTime, Testers),
            timer:sleep(?SLEEP_BEFORE_CHECK),
            case erlang:port_command(Port, <<"test">>, [nosuspend]) of
                false ->
                    exit(port_dead);
                true ->
                    ok
            end
    end.

wait_testers(Timeout, Testers) ->
    lists:foldl(
      fun(Pid, AccIn) ->
              StartWait = os:timestamp(),
              receive
                  {Pid, port_dead} ->
                      recalc_timeout(AccIn, StartWait)
              after AccIn ->
                      Pid ! stop,
                      recalc_timeout(AccIn, StartWait)
              end
      end, Timeout, Testers),
    ok.

recalc_timeout(TimeoutIn, WaitStart) ->
    erlang:max(0, TimeoutIn - round(timer:now_diff(os:timestamp(), WaitStart)) div 1000).

open_port(ExePath) ->
    erlang:open_port({spawn, ExePath ++ " 100"}, [{packet, 4}, eof, exit_status, use_stdio, binary]).

run_loop(RootProc, Port, Block, CheckLimit, BusyCnt) ->
    receive
        stop ->
            ok
    after 0 ->
            case erlang:port_command(Port, Block, [nosuspend]) of
                true ->
                    run_loop(RootProc, Port, Block, CheckLimit, 0);
                false ->
                    if
                        BusyCnt + 1 > CheckLimit ->
                            check_dead(RootProc, Port, Block, CheckLimit);
                        true ->
                            run_loop(RootProc, Port, Block, CheckLimit, BusyCnt + 1)
                    end
            end
    end.

check_dead(RootProc, Port, Block, CheckLimit) ->
    ct:log("~p: check port dead~n", [self()]),
    timer:sleep(?SLEEP_BEFORE_CHECK),
    case erlang:port_command(Port, Block, [nosuspend]) of
        true ->
            ct:log("not dead~n"),
            run_loop(RootProc, Port, Block, CheckLimit, 0);
        false ->
            ct:log("port dead: ~p~n", [Port]),
            RootProc ! {self(), port_dead},
            ok
    end.

endless_flush(Port) ->
    receive
        {Port, {data, _}} ->
            endless_flush(Port);
        {Port, SomethingWrong} ->
            erlang:error({someting_wrong, SomethingWrong})
    end.
