%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2020. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : signal_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Test signals
%%%
%%% Created : 10 Jul 2006 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(signal_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

% Test cases
-export([xm_sig_order/1,
         kill2killed/1,
         busy_dist_exit_signal/1,
         busy_dist_down_signal/1]).

init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    [{testcase, Func}|Config].

end_per_testcase(_Func, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [xm_sig_order,
     kill2killed,
     busy_dist_exit_signal,
     busy_dist_down_signal].


%% Test that exit signals and messages are received in correct order
xm_sig_order(Config) when is_list(Config) ->
    LNode = node(),
    repeat(fun () -> xm_sig_order_test(LNode) end, 1000),
    {ok, RNode} = start_node(Config),
    repeat(fun () -> xm_sig_order_test(RNode) end, 1000),
    stop_node(RNode),
    ok.
    

xm_sig_order_test(Node) ->
    P = spawn(Node, fun () -> xm_sig_order_proc() end),
    M = erlang:monitor(process, P),
    P ! may_reach,
    P ! may_reach,
    P ! may_reach,
    exit(P, good_signal_order),
    P ! may_not_reach,
    P ! may_not_reach,
    P ! may_not_reach,
    receive
	      {'DOWN', M, process, P, R} ->
		  good_signal_order = R
	  end.

xm_sig_order_proc() ->
    receive
	may_not_reach -> exit(bad_signal_order);
	may_reach -> ok
    after 0 -> erlang:yield()
    end,
    xm_sig_order_proc().

kill2killed(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    kill2killed_test(node()),
    {ok, Node} = start_node(Config),
    kill2killed_test(Node),
    stop_node(Node),
    ok.

kill2killed_test(Node) ->
    if Node == node() ->
            io:format("Testing against local node", []);
       true ->
            io:format("Testing against remote node ~p", [Node])
    end,
    check_exit(Node, other_exit2, 1),
    check_exit(Node, other_exit2, 2),
    check_exit(Node, other_exit2, 9),
    check_exit(Node, other_exit2, 10),
    check_exit(Node, exit2, 1),
    check_exit(Node, exit2, 2),
    check_exit(Node, exit2, 9),
    check_exit(Node, exit2, 10),
    check_exit(Node, exit1, 1),
    check_exit(Node, exit1, 2),
    check_exit(Node, exit1, 9),
    check_exit(Node, exit1, 10),
    ok.

check_exit(Node, Type, N) ->
    io:format("Testing ~p length ~p~n", [Type, N]),
    P = spawn_link_line(Node, node(), Type, N, self()),
    if Type == other_exit2 ->
            receive
                {end_of_line, EOL} ->
                    exit(EOL, kill)
            end;
       true -> ok
    end,
    receive
        {'EXIT', P, Reason} ->
            if Type == exit1 ->
                    kill = Reason;
               true ->
                    killed = Reason
            end
    end.

spawn_link_line(_NodeA, _NodeB, other_exit2, 0, Tester) ->
    Tester ! {end_of_line, self()},
    receive after infinity -> ok end;
spawn_link_line(_NodeA, _NodeB, exit1, 0, _Tester) ->
    exit(kill);
spawn_link_line(_NodeA, _NodeB, exit2, 0, _Tester) ->
    exit(self(), kill);
spawn_link_line(NodeA, NodeB, Type, N, Tester) ->
    spawn_link(NodeA,
               fun () ->
                       spawn_link_line(NodeB, NodeA, Type, N-1, Tester),
                       receive after infinity -> ok end
               end).

busy_dist_exit_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelNode} = start_node(Config),
    {ok, OtherNode} = start_node(Config, "-proto_dist gen_tcp"),
    Tester = self(),
    Exiter = spawn(BusyChannelNode,
                   fun () ->
                           pong = net_adm:ping(OtherNode),
                           Tester ! {self(), alive},
                           receive after infinity -> ok end
                   end),
    receive {Exiter, alive} -> ok end,
    Linker = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                link(Exiter),
                                receive
                                    {'EXIT', Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_exit_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, 1000),
    exit(Exiter, tester_killed_me),
    receive
        {Linker, got_exiter_exit_message} ->
            unlink(Linker),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_exit_signal)
    end,
    stop_node(BusyChannelNode),
    stop_node(OtherNode),
    ok.

busy_dist_down_signal(Config) when is_list(Config) ->
    BusyTime = 1000,
    {ok, BusyChannelNode} = start_node(Config),
    {ok, OtherNode} = start_node(Config, "-proto_dist gen_tcp"),
    Tester = self(),
    Exiter = spawn(BusyChannelNode,
                   fun () ->
                           pong = net_adm:ping(OtherNode),
                           Tester ! {self(), alive},
                           receive after infinity -> ok end
                   end),
    receive {Exiter, alive} -> ok end,
    Monitorer = spawn_link(OtherNode,
                        fun () ->
                                process_flag(trap_exit, true),
                                Mon = erlang:monitor(process, Exiter),
                                receive
                                    {'DOWN', Mon, process, Exiter, Reason} ->
                                        tester_killed_me = Reason,
                                        Tester ! {self(), got_exiter_down_message};
                                    Unexpected ->
                                        exit({unexpected_message, Unexpected})
                                end
                         end),
    make_busy(BusyChannelNode, OtherNode, 1000),
    exit(Exiter, tester_killed_me),
    receive
        {Monitorer, got_exiter_down_message} ->
            unlink(Monitorer),
            ok
    after
        BusyTime*2 ->
            ct:fail(missing_down_signal)
    end,
    stop_node(BusyChannelNode),
    stop_node(OtherNode),
    ok.

%%
%% -- Internal utils --------------------------------------------------------
%%

make_busy(OnNode, ToNode, Time) ->
    Parent = self(),
    Fun = fun () ->
                  Proxy = self(),
                  Sspndr = spawn_link(
                             ToNode,
                             fun () ->
                                     IC = find_gen_tcp_input_cntrlr(OnNode),
                                     erlang:suspend_process(IC),
                                     Proxy ! {self(), input_cntrlr_suspended},
                                     receive
                                         {Proxy, resume_input_cntrlr} ->
                                             erlang:resume_process(IC)
                                     end,
                                     Proxy ! {self(), input_cntrlr_resumed}
                             end),
                  receive
                      {Sspndr, input_cntrlr_suspended} ->
                          ok
                  end,
                  Spammer = spawn_link(
                              OnNode,
                              fun () ->
                                      spammed = spam(ToNode),
                                      Proxy ! {self(), channel_busy},
                                      receive
                                      after Time -> ok
                                      end,
                                      Proxy ! {self(), timeout}
                              end),
                  receive
                      {Spammer, channel_busy} ->
                          Parent ! {self(), channel_busy}
                  end,
                  receive
                      {Spammer, timeout} ->
                          Sspndr ! {self(), resume_input_cntrlr}
                  end,
                  receive
                      {Sspndr, input_cntrlr_resumed} ->
                          ok
                  end
          end,
    Proxy = spawn_link(Fun),
    receive
        {Proxy, channel_busy} ->
            ok
    end,
    Proxy.

find_gen_tcp_input_cntrlr(Node) when is_atom(Node) ->
    case lists:keyfind(Node, 1, erlang:system_info(dist_ctrl)) of
        {Node, DistCtrl} ->
            find_gen_tcp_input_cntrlr(DistCtrl);
        false ->
            undefined
    end;
find_gen_tcp_input_cntrlr(DistCtrl) when is_pid(DistCtrl) ->
    {links, LList} = process_info(DistCtrl, links),
    try
        lists:foreach(fun (Pid) ->
                              case process_info(Pid, initial_call) of
                                  {initial_call,
                                   {gen_tcp_dist,dist_cntrlr_input_setup,3}} ->
                                      throw({input_ctrlr, Pid});
                                  _ ->
                                      ok
                              end
                      end,
                      LList),
        undefined
    catch
        throw:{input_ctrlr, DistInputCtrlr} ->
            DistInputCtrlr
    end.

spam(Node) ->
    To = {'__a_name_hopefully_not_registered__', Node},
    Data = lists:seq(1, 100),
    spam(To, Data).

spam(To, Data) ->
    case erlang:send(To, Data, [nosuspend]) of
        nosuspend ->
            spammed;
        _ ->
            spam(To, Data)
    end.

repeat(_Fun, N) when is_integer(N), N =< 0 ->
    ok;
repeat(Fun, N) when is_integer(N)  ->
    Fun(),
    repeat(Fun, N-1).

start_node(Config, Args) ->
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-" ++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-" ++ integer_to_list(erlang:system_time(second))
			++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args,  "-pa " ++ Pa ++ " " ++ Args}]).

start_node(Config) ->
    start_node(Config, "").

stop_node(Node) ->
    test_server:stop_node(Node).
