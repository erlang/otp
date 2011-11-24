%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

-module(monitor_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 case_1/1, case_1a/1, case_2/1, case_2a/1, mon_e_1/1, demon_e_1/1, demon_1/1,
	 demon_2/1, demon_3/1, demonitor_flush/1,
	 local_remove_monitor/1, remote_remove_monitor/1, mon_1/1, mon_2/1,
	 large_exit/1, list_cleanup/1, mixer/1, named_down/1, otp_5827/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([y2/1, g/1, g0/0, g1/0, large_exit_sub/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [case_1, case_1a, case_2, case_2a, mon_e_1, demon_e_1,
     demon_1, mon_1, mon_2, demon_2, demon_3,
     demonitor_flush, {group, remove_monitor}, large_exit,
     list_cleanup, mixer, named_down, otp_5827].

groups() -> 
    [{remove_monitor, [],
      [local_remove_monitor, remote_remove_monitor]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(15)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

case_1(doc) ->
    "A monitors B, B kills A and then exits (yielded core dump)";
case_1(suite) -> [];
case_1(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line spawn_link(?MODULE, g0, []),
    ?line receive _ -> ok end,
    ok.

case_1a(doc) ->
    "A monitors B, B kills A and then exits (yielded core dump)";
case_1a(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line spawn_link(?MODULE, g1, []),
    ?line receive _ -> ok end,
    ok.

g0() ->
    ?line B = spawn(?MODULE, g, [self()]),
    ?line erlang:monitor(process, B),
    ?line B ! ok,
    ?line receive ok -> ok end,
    ok.

g1() ->
    ?line {B,_} = spawn_monitor(?MODULE, g, [self()]),
    ?line B ! ok,
    ?line receive ok -> ok end,
    ok.

g(Parent) ->
    ?line receive ok -> ok end,
    ?line exit(Parent, foo),
    ?line ok.


case_2(doc) ->
    "A monitors B, B demonitors A (yielded core dump)";
case_2(Config) when is_list(Config) ->
    ?line B = spawn(?MODULE, y2, [self()]),
    ?line R = erlang:monitor(process, B),
    ?line B ! R,
    ?line receive
	      {'EXIT', _} -> ok;
	      Other ->
		  test_server:fail({rec, Other})
	  end,
    ?line expect_down(R, B, normal),
    ok.

case_2a(doc) ->
    "A monitors B, B demonitors A (yielded core dump)";
case_2a(Config) when is_list(Config) ->
    ?line {B,R} = spawn_monitor(?MODULE, y2, [self()]),
    ?line B ! R,
    ?line receive
	      {'EXIT', _} -> ok;
	      Other ->
		  test_server:fail({rec, Other})
	  end,
    ?line expect_down(R, B, normal),
    ok.

y2(Parent) ->
    ?line R = receive T -> T end,
    ?line Parent ! (catch erlang:demonitor(R)),
    ok.

expect_down(Ref, P) ->
    receive
	{'DOWN', Ref, process, P, Reason} -> 
	    Reason;
	Other ->
	    test_server:fail({rec, Other})
    end.

expect_down(Ref, P, Reason) ->
    receive
	{'DOWN', Ref, process, P, Reason} -> 
	    ok;
	Other ->
	    test_server:fail({rec, Other})
    end.

expect_no_msg() ->
    receive
	Msg ->
	    test_server:fail({msg, Msg})
    after 0 ->
	    ok
    end.

%%% Error cases for monitor/2

mon_e_1(doc) ->
    "Error cases for monitor/2";
mon_e_1(suite) -> [];
mon_e_1(Config) when is_list(Config) ->
    ?line {ok, N} = test_server:start_node(hej, slave, []),
    ?line mon_error(plutt, self()),
    ?line mon_error(process, [bingo]),
    ?line mon_error(process, {rex, N, junk}),
    ?line mon_error(process, 1),

    ?line true = test_server:stop_node(N),
    ok.

%%% We would also like to have a test case that tries to monitor something
%%% on an R5 node, but this isn't possible to do systematically.
%%%
%%% Likewise against an R6 node, which is not capable of monitoring
%%% by name, which gives a badarg on the R7 node at the call to
%%% erlang:monitor(process, {Name, Node}). This has been tested 
%%% manually at least once.

mon_error(Type, Item) ->
    case catch erlang:monitor(Type, Item) of
	{'EXIT', _} ->
	    ok;
	Other ->
	    test_server:fail({err, Other})
    end.

%%% Error cases for demonitor/1

demon_e_1(doc) ->
    "Error cases for demonitor/1";
demon_e_1(suite) -> [];
demon_e_1(Config) when is_list(Config) ->
    ?line {ok, N} = test_server:start_node(hej, slave, []),
    ?line demon_error(plutt, badarg),
    ?line demon_error(1, badarg),

    %% Demonitor with ref created at other node
    ?line R1 = rpc:call(N, erlang, make_ref, []),
    ?line demon_error(R1, badarg),

    %% Demonitor with ref created at wrong monitor link end
    ?line P0 = self(),
    ?line P2 = spawn(
		 fun() ->
			 P0 ! {self(), ref, erlang:monitor(process,P0)},
			 receive {P0, stop} -> ok end
		 end ),
    ?line receive 
	      {P2, ref, R2} -> 
		  ?line demon_error(R2, badarg),
		  ?line P2 ! {self(), stop};
	      Other2 ->
		  test_server:fail({rec, Other2})
	  end,

    ?line true = test_server:stop_node(N),
    ok.

demon_error(Ref, Reason) ->
    case catch erlang:demonitor(Ref) of
	{'EXIT', {Reason, _}} ->
	    ok;
	Other ->
	    test_server:fail({err, Other})
    end.

%%% No-op cases for demonitor/1

demon_1(doc) ->
    "demonitor/1";
demon_1(suite) -> [];
demon_1(Config) when is_list(Config) ->
    ?line true = erlang:demonitor(make_ref()),
    ok.


%%% Cases for demonitor/1

demon_2(doc) ->
    "Cases for demonitor/1";
demon_2(suite) -> [];
demon_2(Config) when is_list(Config) ->
    ?line R1 = erlang:monitor(process, self()),
    ?line true = erlang:demonitor(R1),
    %% Extra demonitor
    ?line true = erlang:demonitor(R1),
    ?line expect_no_msg(),

    %% Normal 'DOWN'
    ?line P2 = spawn(timer, sleep, [1]),
    ?line R2 = erlang:monitor(process, P2),
    ?line case expect_down(R2, P2) of
	      normal -> ?line ok;
	      noproc -> ?line ok;
	      BadReason -> ?line ?t:fail({bad_reason, BadReason})
	  end,

%% OTP-5772
%     %% 'DOWN' before demonitor
%     ?line P3 = spawn(timer, sleep, [100000]),
%     ?line R3 = erlang:monitor(process, P3),
%     ?line exit(P3, frop),
%     ?line erlang:demonitor(R3),
%     ?line expect_down(R3, P3, frop),

    %% Demonitor before 'DOWN'
    ?line P4 = spawn(timer, sleep, [100000]),
    ?line R4 = erlang:monitor(process, P4),
    ?line erlang:demonitor(R4),
    ?line exit(P4, frop),
    ?line expect_no_msg(),

    ok.

demon_3(doc) ->
    "Distributed case for demonitor/1 (OTP-3499)";
demon_3(suite) -> [];
demon_3(Config) when is_list(Config) ->
    ?line {ok, N} = test_server:start_node(hej, slave, []),

    %% 'DOWN' before demonitor
    ?line P2 = spawn(N, timer, sleep, [100000]),
    ?line R2 = erlang:monitor(process, P2),
    ?line true = test_server:stop_node(N),
    ?line true = erlang:demonitor(R2),
    ?line expect_down(R2, P2, noconnection),

    ?line {ok, N2} = test_server:start_node(hej, slave, []),

    %% Demonitor before 'DOWN'
    ?line P3 = spawn(N2, timer, sleep, [100000]),
    ?line R3 = erlang:monitor(process, P3),
    ?line true = erlang:demonitor(R3),
    ?line true = test_server:stop_node(N2),
    ?line expect_no_msg(),

    ok.

demonitor_flush(suite) -> [];
demonitor_flush(doc) -> [];
demonitor_flush(Config) when is_list(Config) ->
    ?line {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), flush)),
    ?line {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), [flus])),
    ?line {'EXIT', {badarg, _}} = (catch erlang:demonitor(x, [flush])),
    ?line {ok, N} = test_server:start_node(demonitor_flush, slave, []),
    ?line ok = demonitor_flush_test(N),
    ?line true = test_server:stop_node(N),
    ?line ok = demonitor_flush_test(node()).
    
demonitor_flush_test(Node) ->
    ?line P = spawn(Node, timer, sleep, [100000]),
    ?line M1 = erlang:monitor(process, P),
    ?line M2 = erlang:monitor(process, P),
    ?line M3 = erlang:monitor(process, P),
    ?line M4 = erlang:monitor(process, P),
    ?line true = erlang:demonitor(M1, [flush, flush]),
    ?line exit(P, bang),
    ?line receive {'DOWN', M2, process, P, bang} -> ok end,
    ?line receive after 100 -> ok end,
    ?line true = erlang:demonitor(M3, [flush]),
    ?line true = erlang:demonitor(M4, []),
    ?line receive {'DOWN', M4, process, P, bang} -> ok end,
    ?line receive
	      {'DOWN', M, _, _, _} =DM when M == M1,
					    M == M3 ->
		  ?line ?t:fail({unexpected_down_message, DM})
	  after 100 ->
		  ?line ok
	  end.

-define(RM_MON_GROUPS, 100).
-define(RM_MON_GPROCS, 100).


local_remove_monitor(Config) when is_list(Config) ->
    Gs = generate(fun () -> start_remove_monitor_group(node()) end,
		  ?RM_MON_GROUPS),
    {True, False} = lists:foldl(fun (G, {T, F}) ->
					receive
					    {rm_mon_res, G, {GT, GF}} ->
						{T+GT, F+GF}
					end
				end,
				{0, 0},
				Gs),
    erlang:display({local_remove_monitor, True, False}),
    {comment,
     "True = "++integer_to_list(True)++"; False = "++integer_to_list(False)}.
	
remote_remove_monitor(Config) when is_list(Config) ->
    ?line {ok, N} = test_server:start_node(demonitor_flush, slave, []),
    Gs = generate(fun () -> start_remove_monitor_group(node()) end,
		  ?RM_MON_GROUPS),
    {True, False} = lists:foldl(fun (G, {T, F}) ->
					receive
					    {rm_mon_res, G, {GT, GF}} ->
						{T+GT, F+GF}
					end
				end,
				{0, 0},
				Gs),
    erlang:display({remote_remove_monitor, True, False}),
    ?line true = test_server:stop_node(N),
    {comment,
     "True = "++integer_to_list(True)++"; False = "++integer_to_list(False)}.

start_remove_monitor_group(Node) ->
    Master = self(),
    spawn_link(
      fun () ->
	      Ms = generate(fun () ->
				    P = spawn(Node, fun () -> ok end),
				    erlang:monitor(process, P)
			    end, ?RM_MON_GPROCS),
	      Res = lists:foldl(fun (M, {T, F}) ->
					case erlang:demonitor(M, [info]) of
					    true ->
						receive
						    {'DOWN', M, _, _, _} ->
							exit(down_msg_found)
						after 0 ->
							ok
						end,
						{T+1, F};
					    false ->
						receive
						    {'DOWN', M, _, _, _} ->
							ok
						after 0 ->
							exit(no_down_msg_found)
						end,
						{T, F+1}
					end
				end,
				{0,0},
				Ms),
	      Master ! {rm_mon_res, self(), Res}
      end).
					  
				      
%%% Cases for monitor/2

mon_1(doc) ->
    "Cases for monitor/2";
mon_1(suite) -> [];
mon_1(Config) when is_list(Config) ->
    %% Normal case
    ?line P2 = spawn(timer, sleep, [1]),
    ?line R2 = erlang:monitor(process, P2),
    ?line case expect_down(R2, P2) of
	      normal -> ?line ok;
	      noproc -> ?line ok;
	      BadReason -> ?line ?t:fail({bad_reason, BadReason})
	  end,
    ?line {P2A,R2A} = spawn_monitor(timer, sleep, [1]),
    ?line expect_down(R2A, P2A, normal),

    %% 'DOWN' with other reason
    ?line P3 = spawn(timer, sleep, [100000]),
    ?line R3 = erlang:monitor(process, P3),
    ?line exit(P3, frop),
    ?line expect_down(R3, P3, frop),
    ?line {P3A,R3A} = spawn_monitor(timer, sleep, [100000]),
    ?line exit(P3A, frop),
    ?line expect_down(R3A, P3A, frop),

    %% Monitor fails because process is dead
    ?line R4 = erlang:monitor(process, P3),
    ?line expect_down(R4, P3, noproc),

    %% Normal case (named process)
    ?line P5 = start_jeeves(jeeves),
    ?line R5 = erlang:monitor(process, jeeves),
    ?line tell_jeeves(P5, stop),
    ?line expect_down(R5, {jeeves, node()}, normal),

    %% 'DOWN' with other reason and node explicit activation
    ?line P6 = start_jeeves(jeeves),
    ?line R6 = erlang:monitor(process, {jeeves, node()}),
    ?line tell_jeeves(P6, {exit, frop}),
    ?line expect_down(R6, {jeeves, node()}, frop),

    %% Monitor (named process) fails because process is dead
    ?line R7 = erlang:monitor(process, {jeeves, node()}),
    ?line expect_down(R7, {jeeves, node()}, noproc),

    ok.

mon_2(doc) ->
    "Distributed cases for monitor/2";
mon_2(suite) -> [];
mon_2(Config) when is_list(Config) ->
    ?line {ok, N1} = test_server:start_node(hej1, slave, []),

    %% Normal case
    ?line P2 = spawn(N1, timer, sleep, [4000]),
    ?line R2 = erlang:monitor(process, P2),
    ?line expect_down(R2, P2, normal),

    %% 'DOWN' with other reason
    ?line P3 = spawn(N1, timer, sleep, [100000]),
    ?line R3 = erlang:monitor(process, P3),
    ?line exit(P3, frop),
    ?line expect_down(R3, P3, frop),

    %% Monitor fails because process is dead
    ?line R4 = erlang:monitor(process, P3),
    ?line expect_down(R4, P3, noproc),

    %% Other node goes down
    ?line P5 = spawn(N1, timer, sleep, [100000]),
    ?line R5 = erlang:monitor(process, P5),

    ?line true = test_server:stop_node(N1),

    ?line expect_down(R5, P5, noconnection),

    %% Monitor fails because other node is dead
    ?line P6 = spawn(N1, timer, sleep, [100000]),
    ?line R6 = erlang:monitor(process, P6),
    ?line R6_Reason = expect_down(R6, P6),
    ?line true = (R6_Reason == noconnection) orelse (R6_Reason == noproc),

    %% Start a new node that can load code in this module
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line {ok, N2} = test_server:start_node
		       (hej2, slave, [{args, "-pa " ++ PA}]),

    %% Normal case (named process)
    ?line P7 = start_jeeves({jeeves, N2}),
    ?line R7 = erlang:monitor(process, {jeeves, N2}),
    ?line tell_jeeves(P7, stop),
    ?line expect_down(R7, {jeeves, N2}, normal),

    %% 'DOWN' with other reason (named process)
    ?line P8 = start_jeeves({jeeves, N2}),
    ?line R8 = erlang:monitor(process, {jeeves, N2}),
    ?line tell_jeeves(P8, {exit, frop}),
    ?line expect_down(R8, {jeeves, N2}, frop),

    %% Monitor (named process) fails because process is dead
    ?line R9 = erlang:monitor(process, {jeeves, N2}),
    ?line expect_down(R9, {jeeves, N2}, noproc),

    %% Other node goes down (named process)
    ?line _P10 = start_jeeves({jeeves, N2}),
    ?line R10 = erlang:monitor(process, {jeeves, N2}),

    ?line true = test_server:stop_node(N2),

    ?line expect_down(R10, {jeeves, N2}, noconnection),

    %% Monitor (named process) fails because other node is dead
    ?line R11 = erlang:monitor(process, {jeeves, N2}),
    ?line expect_down(R11, {jeeves, N2}, noconnection),

    ok.

%%% Large exit reason. Crashed first attempt to release R5B.

large_exit(doc) ->
    "Large exit reason";
large_exit(suite) -> [];
large_exit(Config) when is_list(Config) ->
    ?line f(100),
    ok.

f(0) ->
    ok;
f(N) ->
    f(),
    f(N-1).

f() ->
    ?line S0 = {big, tuple, with, [list, 4563784278]},
    ?line S = {S0, term_to_binary(S0)},
    ?line P = spawn(?MODULE, large_exit_sub, [S]),
    ?line R = erlang:monitor(process, P),
    ?line P ! hej,
    receive
	{'DOWN', R, process, P, X} ->
	    ?line io:format(" -> ~p~n", [X]),
	    if
		X == S ->
		    ok;
		true ->
		    test_server:fail({X, S})
	    end;
	Other ->
	    ?line io:format(" -> ~p~n", [Other]),
	    exit({answer, Other})
    end.

large_exit_sub(S) ->
    receive _X -> ok end,
    exit(S).

%%% Testing of monitor link list cleanup
%%% by using erlang:process_info(self(), monitors)
%%% and      erlang:process_info(self(), monitored_by)

list_cleanup(doc) ->
    "Testing of monitor link list cleanup by using " ++
    "erlang:process_info/2";
list_cleanup(suite) -> [];
list_cleanup(Config) when is_list(Config) ->
    ?line P0 = self(),
    ?line M  = node(),
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line true = register(master_bertie, self()),

    %% Normal local case, monitor and demonitor
    ?line P1 = start_jeeves(jeeves),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P1, monitors, {monitors, {[], []}}),
    ?line R1a = erlang:monitor(process, P1),
    ?line {[{process, P1}], []} = monitors(),
    ?line expect_jeeves(P1, monitors, {monitors, {[], [P0]}}),
    ?line true = erlang:demonitor(R1a),
    ?line expect_no_msg(),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P1, monitors, {monitors, {[], []}}),
    %% Remonitor named and try again, now exiting the monitored process
    ?line R1b = erlang:monitor(process, jeeves),
    ?line {[{process, {jeeves, M}}], []} = monitors(),
    ?line expect_jeeves(P1, monitors, {monitors, {[], [P0]}}),
    ?line tell_jeeves(P1, stop),
    ?line expect_down(R1b, {jeeves, node()}, normal),
    ?line {[], []} = monitors(),

    %% Slightly weird local case - the monitoring process crashes
    ?line P2 = start_jeeves(jeeves),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P2, monitors, {monitors, {[], []}}),
    ?line {monitor_process, _R2} = 
	ask_jeeves(P2, {monitor_process, master_bertie}),
    ?line {[], [P2]} = monitors(),
    ?line expect_jeeves(P2, monitors,
			{monitors, {[{process, {master_bertie, node()}}], []}}),
    ?line tell_jeeves(P2, {exit, frop}),
    timer:sleep(2000),
    ?line {[], []} = monitors(),

    %% Start a new node that can load code in this module
    ?line {ok, J} = test_server:start_node
		       (jeeves, slave, [{args, "-pa " ++ PA}]),

    %% Normal remote case, monitor and demonitor
    ?line P3 = start_jeeves({jeeves, J}),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P3, monitors, {monitors, {[], []}}),
    ?line R3a = erlang:monitor(process, P3),
    ?line {[{process, P3}], []} = monitors(),
    ?line expect_jeeves(P3, monitors, {monitors, {[], [P0]}}),
    ?line true = erlang:demonitor(R3a),
    ?line expect_no_msg(),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P3, monitors, {monitors, {[], []}}),
    %% Remonitor named and try again, now exiting the monitored process
    ?line R3b = erlang:monitor(process, {jeeves, J}),
    ?line {[{process, {jeeves, J}}], []} = monitors(),
    ?line expect_jeeves(P3, monitors, {monitors, {[], [P0]}}),
    ?line tell_jeeves(P3, stop),
    ?line expect_down(R3b, {jeeves, J}, normal),
    ?line {[], []} = monitors(),

    %% Slightly weird remote case - the monitoring process crashes
    ?line P4 = start_jeeves({jeeves, J}),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P4, monitors, {monitors, {[], []}}),
    ?line {monitor_process, _R4} = 
	ask_jeeves(P4, {monitor_process, {master_bertie, M}}),
    ?line {[], [P4]} = monitors(),
    ?line expect_jeeves(P4, monitors, 
			{monitors, {[{process, {master_bertie, M}}], []}} ),
    ?line tell_jeeves(P4, {exit, frop}),
    timer:sleep(2000),
    ?line {[], []} = monitors(),
    
    %% Now, the monitoring remote node crashes
    ?line P5 = start_jeeves({jeeves, J}),
    ?line {[], []} = monitors(),
    ?line expect_jeeves(P5, monitors, {monitors, {[], []}}),
    ?line {monitor_process, _R5} = 
	ask_jeeves(P5, {monitor_process, P0}),
    ?line {[], [P5]} = monitors(),
    ?line expect_jeeves(P5, monitors, 
			{monitors, {[{process, P0}], []}} ),
    ?line test_server:stop_node(J),
    timer:sleep(4000),
    ?line {[], []} = monitors(),
    
    ?line true = unregister(master_bertie),
    ok.

    
%%% Mixed internal and external monitors

mixer(doc) ->
    "Test mixing of internal and external monitors.";
mixer(Config) when is_list(Config) ->
    ?line PA = filename:dirname(code:which(?MODULE)),
    ?line NN = [j0,j1,j2,j3],
%    ?line NN = [j0,j1],
    ?line NL0 = [begin
		    {ok, J} = test_server:start_node
				(X, slave, [{args, "-pa " ++ PA}]), 
		    J 
		end  || X <- NN],
    ?line NL1 = lists:duplicate(2,node()) ++ NL0,
    ?line Perm = perm(NL1),
    ?line lists:foreach(
	    fun(NL) ->
		    ?line Js = [ start_jeeves({[],M}) || M <- (NL ++ NL) ],
		    ?line [ask_jeeves(P,{monitor_process,self()}) || P <- Js],
		    ?line {monitored_by,MB} = 
			process_info(self(),monitored_by),
		    ?line MBL = lists:sort(MB),
		    ?line JsL = lists:sort(Js),
		    ?line MBL = JsL,
		    ?line {monitors,[]}  = process_info(self(),monitors),
		    ?line [tell_jeeves(P,{exit,flaff}) || P <- Js],
		    ?line wait_for_m([],[],200)
	    end,
	    Perm),
    ?line lists:foreach(
	    fun(NL) ->
		    ?line Js = [ start_jeeves({[],M}) || M <- (NL ++ NL) ],
		    ?line Rs = [begin
				    {monitor_process,Ref} = 
					ask_jeeves(P,{monitor_process,self()}),
				    {P,Ref}
				end
				|| P <- Js],
		    ?line {monitored_by,MB} = 
			process_info(self(),monitored_by),
		    ?line MBL = lists:sort(MB),
		    ?line JsL = lists:sort(Js),
		    ?line MBL = JsL,
		    ?line {monitors,[]}  = process_info(self(),monitors),
		    ?line [ask_jeeves(P,{demonitor,Ref}) || {P,Ref} <- Rs],
		    ?line wait_for_m([],[],200),
		    ?line [tell_jeeves(P,{exit,flaff}) || P <- Js]
	    end,
	    Perm),
    ?line lists:foreach(
	    fun(NL) ->
		    ?line Js = [ start_jeeves({[],M}) || M <- (NL ++ NL) ],
		    ?line [ask_jeeves(P,{monitor_process,self()}) || P <- Js],
		    ?line [erlang:monitor(process,P) || P <- Js],
		    ?line {monitored_by,MB} = 
			process_info(self(),monitored_by),
		    ?line MBL = lists:sort(MB),
		    ?line JsL = lists:sort(Js),
		    ?line MBL = JsL,
		    ?line {monitors,M} = 
			process_info(self(),monitors),
		    ?line ML = lists:sort([P||{process,P} <- M]),
		    ?line ML = JsL,
		    ?line [begin
			       tell_jeeves(P,{exit,flaff}),
			       receive {'DOWN',_,process,P,_} -> ok end
			   end || P <- Js],
		    ?line wait_for_m([],[],200)
	    end,
	    Perm),
    ?line lists:foreach(
	    fun(NL) ->
		    ?line Js = [ start_jeeves({[],M}) || M <- (NL ++ NL) ],
		    ?line Rs = [begin
				    {monitor_process,Ref} = 
					ask_jeeves(P,{monitor_process,self()}),
				    {P,Ref}
				end
				|| P <- Js],
		    ?line R2s = [{P,erlang:monitor(process,P)} || P <- Js],
		    ?line {monitored_by,MB} = 
			process_info(self(),monitored_by),
		    ?line MBL = lists:sort(MB),
		    ?line JsL = lists:sort(Js),
		    ?line MBL = JsL,
		    ?line {monitors,M} = 
			process_info(self(),monitors),
		    ?line ML = lists:sort([P||{process,P} <- M]),
		    ?line ML = JsL,
		    ?line [ask_jeeves(P,{demonitor,Ref}) || {P,Ref} <- Rs],
		    ?line wait_for_m(lists:sort(M),[],200),
		    ?line [erlang:demonitor(Ref) || {_P,Ref} <- R2s],
		    ?line wait_for_m([],[],200),
		    ?line [tell_jeeves(P,{exit,flaff}) || P <- Js]
	    end,
	    Perm),
    [test_server:stop_node(K) || K <- NL0 ],
    ok.

named_down(doc) -> ["Test that DOWN message for a named monitor isn't"
		    " delivered until name has been unregistered"];
named_down(suite) -> [];
named_down(Config) when is_list(Config) ->
    ?line {A,B,C} = now(),
    ?line Name = list_to_atom(atom_to_list(?MODULE)
			      ++ "-named_down-"
			      ++ integer_to_list(A)
			      ++ "-" ++ integer_to_list(B)
			      ++ "-" ++ integer_to_list(C)),
    ?line Prio = process_flag(priority,high),
    %% Spawn a bunch of high prio cpu bound processes to prevent
    %% normal prio processes from terminating during the next
    %% 500 ms...
    ?line Self = self(),
    ?line spawn_opt(fun () ->
			    WFun = fun
				       (F, hej) -> F(F, hopp);
				       (F, hopp) -> F(F, hej)
				   end,
			    NoSchedulers = erlang:system_info(schedulers_online),
			    lists:foreach(fun (_) ->
						  spawn_opt(fun () ->
								    WFun(WFun,
									 hej)
							    end,
							    [{priority,high},
							     link])
					  end,
					  lists:seq(1, NoSchedulers)),
			    receive after 500 -> ok end,
			    unlink(Self),
			    exit(bang)
		     end,
		    [{priority,high}, link]),
    ?line NamedProc = spawn_link(fun () ->
					 receive after infinity -> ok end
				 end),
    ?line true = register(Name, NamedProc),
    ?line unlink(NamedProc),
    ?line exit(NamedProc, bang),
    ?line Mon = erlang:monitor(process, Name),
    ?line receive {'DOWN',Mon, _, _, _} -> ok end,
    ?line true = register(Name, self()),
    ?line true = unregister(Name),
    ?line process_flag(priority,Prio),
    ok.

otp_5827(doc) -> [];
otp_5827(suite) -> [];
otp_5827(Config) when is_list(Config) ->
    %% Make a pid with the same nodename but with another creation
    ?line [CreEnd | RPTail]
	= lists:reverse(binary_to_list(term_to_binary(self()))),
    ?line NewCreEnd = case CreEnd of
		    0 -> 1;
		    1 -> 2;
		    _ -> CreEnd - 1
		end,
    ?line OtherCreationPid
	= binary_to_term(list_to_binary(lists:reverse([NewCreEnd | RPTail]))),
    %% If the bug is present erlang:monitor(process, OtherCreationPid)
    %% will hang...
    ?line Parent = self(),
    ?line Ok = make_ref(),
    ?line spawn(fun () ->
			Mon = erlang:monitor(process, OtherCreationPid),
			% Should get the DOWN message right away
			receive
			    {'DOWN', Mon, process, OtherCreationPid, noproc} ->
				Parent ! Ok
			end
		end),
    ?line receive
	      Ok ->
		  ?line ok
	  after 1000 ->
		  ?line ?t:fail("erlang:monitor/2 hangs")
	  end.


wait_for_m(_,_,0) ->
    exit(monitor_wait_timeout);
wait_for_m(Monitors, MonitoredBy, N) ->
    {monitors,M0}  = process_info(self(),monitors),
    {monitored_by,MB0} = process_info(self(),monitored_by),
    case lists:sort(M0) of
	Monitors ->
	    case lists:sort(MB0) of
		MonitoredBy ->
		    ok;
		_ ->
		    receive after 100 -> ok end,
		    wait_for_m(Monitors,MonitoredBy,N-1)
	    end;
	_ ->
	    receive after 100 -> ok end,
	    wait_for_m(Monitors,MonitoredBy,N-1)
    end.

% All permutations of a list...
perm([]) ->
    [];
perm([X]) ->
    [[X]];
perm(List) ->
    perm([],List,[]).

perm(_,[],Acc) ->
    Acc;
perm(Pre,[El|Post],Acc) ->
    Res = [[El|X] || X <- perm(Pre ++ Post)],
    perm(Pre ++ [El], Post, Res ++ Acc).


%%% Our butler for named process monitor tests

jeeves(Parent, Name, Ref) 
  when is_pid(Parent), (is_atom(Name) or (Name =:= [])), is_reference(Ref) ->
    %%io:format("monitor_SUITE:jeeves(~p, ~p)~n", [Parent, Name]),
    case Name of
	Atom when is_atom(Atom) ->
	    register(Name, self());
	[] ->
	    ok
    end,
    Parent ! {self(), Ref},
    jeeves_loop(Parent).

jeeves_loop(Parent) ->
    receive
	{Parent, monitors} ->
	    Parent ! {self(), {monitors, monitors()}},
	    jeeves_loop(Parent);
	{Parent, {monitor_process, P}} ->
	    Parent ! {self(), {monitor_process, 
			       catch erlang:monitor(process, P) }},
	    jeeves_loop(Parent);
	{Parent, {demonitor, Ref}} ->
	    Parent ! {self(), {demonitor, catch erlang:demonitor(Ref)}},
	    jeeves_loop(Parent);
	{Parent, stop} ->
	    ok;
	{Parent, {exit, Reason}} ->
	    exit(Reason);
	Other ->
	    io:format("~p:jeeves_loop received ~p~n", [?MODULE, Other])
    end.


start_jeeves({Name, Node}) 
  when (is_atom(Name) or (Name =:= [])), is_atom(Node) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(Node, fun() -> jeeves(Parent, Name, Ref) end),
    receive 
	{Pid, Ref} -> 
	    ok;
	Other ->
	    test_server:fail({rec, Other})
    end,
    Pid;
start_jeeves(Name) when is_atom(Name) ->
    start_jeeves({Name, node()}).


tell_jeeves(Pid, What) when is_pid(Pid) ->
    Pid ! {self(), What}.


ask_jeeves(Pid, Request) when is_pid(Pid) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response;
	Other ->
	    test_server:fail({rec, Other})
    end.


expect_jeeves(Pid, Request, Response) when is_pid(Pid) ->
    Pid ! {self(), Request},
    receive 
	{Pid, Response} -> 
	    ok;
	Other ->
	    test_server:fail({rec, Other})
    end.


monitors() ->
    monitors(self()).

monitors(Pid) when is_pid(Pid) ->
    {monitors, Monitors}          = process_info(self(), monitors),
    {monitored_by,  MonitoredBy}  = process_info(self(), monitored_by),
    {Monitors, MonitoredBy}.

generate(_Fun, 0) ->
    [];
generate(Fun, N) ->
    [Fun() | generate(Fun, N-1)].
