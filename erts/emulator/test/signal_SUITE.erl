%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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
         kill2killed/1]).

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
     kill2killed].


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


%%
%% -- Internal utils --------------------------------------------------------
%%

repeat(_Fun, N) when is_integer(N), N =< 0 ->
    ok;
repeat(Fun, N) when is_integer(N)  ->
    Fun(),
    repeat(Fun, N-1).

start_node(Config) ->
    Name = list_to_atom(atom_to_list(?MODULE)
			++ "-" ++ atom_to_list(proplists:get_value(testcase, Config))
			++ "-" ++ integer_to_list(erlang:system_time(second))
			++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args,  "-pa " ++ Pa}]).

stop_node(Node) ->
    test_server:stop_node(Node).
