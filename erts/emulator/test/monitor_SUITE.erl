%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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

-module(monitor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0, groups/0,
         case_1/1, case_1a/1, case_2/1, case_2a/1, mon_e_1/1, demon_e_1/1, demon_1/1,
         demon_2/1, demon_3/1, demonitor_flush/1,
         local_remove_monitor/1, remote_remove_monitor/1, mon_1/1, mon_2/1,
         large_exit/1, list_cleanup/1, mixer/1, named_down/1, otp_5827/1,
         monitor_time_offset/1]).

-export([y2/1, g/1, g0/0, g1/0, large_exit_sub/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 15}}].

all() -> 
    [case_1, case_1a, case_2, case_2a, mon_e_1, demon_e_1,
     demon_1, mon_1, mon_2, demon_2, demon_3,
     demonitor_flush, {group, remove_monitor}, large_exit,
     list_cleanup, mixer, named_down, otp_5827,
     monitor_time_offset].

groups() -> 
    [{remove_monitor, [],
      [local_remove_monitor, remote_remove_monitor]}].

%% A monitors B, B kills A and then exits (yielded core dump)
case_1(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, g0, []),
    receive _ -> ok end,
    ok.

%% A monitors B, B kills A and then exits (yielded core dump)
case_1a(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, g1, []),
    receive _ -> ok end,
    ok.

g0() ->
    B = spawn(?MODULE, g, [self()]),
    erlang:monitor(process, B),
    B ! ok,
    receive ok -> ok end,
    ok.

g1() ->
    {B,_} = spawn_monitor(?MODULE, g, [self()]),
    B ! ok,
    receive ok -> ok end,
    ok.

g(Parent) ->
    receive ok -> ok end,
    exit(Parent, foo),
    ok.


%% A monitors B, B demonitors A (yielded core dump)
case_2(Config) when is_list(Config) ->
    B = spawn(?MODULE, y2, [self()]),
    R = erlang:monitor(process, B),
    B ! R,
    receive
        true -> ok;
        Other ->
            ct:fail({rec, Other})
    end,
    expect_down(R, B, normal),
    ok.

%% A monitors B, B demonitors A (yielded core dump)
case_2a(Config) when is_list(Config) ->
    {B,R} = spawn_monitor(?MODULE, y2, [self()]),
    B ! R,
    receive
        true -> ok;
        Other ->
            ct:fail({rec, Other})
    end,
    expect_down(R, B, normal),
    ok.

y2(Parent) ->
    R = receive T -> T end,
    Parent ! (catch erlang:demonitor(R)),
    ok.

expect_down(Ref, P) ->
    receive
        {'DOWN', Ref, process, P, Reason} -> 
            Reason;
        Other ->
            ct:fail({rec, Other})
    end.

expect_down(Ref, P, Reason) ->
    receive
        {'DOWN', Ref, process, P, Reason} -> 
            ok;
        Other ->
            ct:fail({rec, Other})
    end.

expect_no_msg() ->
    receive
        Msg ->
            ct:fail({msg, Msg})
    after 0 ->
              ok
    end.

%%% Error cases for monitor/2

mon_e_1(Config) when is_list(Config) ->
    {ok, N} = test_server:start_node(hej, slave, []),
    mon_error(plutt, self()),
    mon_error(process, [bingo]),
    mon_error(process, {rex, N, junk}),
    mon_error(process, 1),

    true = test_server:stop_node(N),
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
            ct:fail({err, Other})
    end.

%%% Error cases for demonitor/1

demon_e_1(Config) when is_list(Config) ->
    {ok, N} = test_server:start_node(hej, slave, []),
    demon_error(plutt, badarg),
    demon_error(1, badarg),

    %% Demonitor with ref created at other node
    R1 = rpc:call(N, erlang, make_ref, []),
    demon_error(R1, badarg),

    %% Demonitor with ref created at wrong monitor link end
    P0 = self(),
    P2 = spawn(
           fun() ->
                   P0 ! {self(), ref, erlang:monitor(process,P0)},
                   receive {P0, stop} -> ok end
           end ),
    receive 
        {P2, ref, R2} -> 
            true = erlang:demonitor(R2),
            P2 ! {self(), stop};
        Other2 ->
            ct:fail({rec, Other2})
    end,

    true = test_server:stop_node(N),
    ok.

demon_error(Ref, Reason) ->
    case catch erlang:demonitor(Ref) of
        {'EXIT', {Reason, _}} ->
            ok;
        Other ->
            ct:fail({err, Other})
    end.

%%% No-op cases for demonitor/1

demon_1(Config) when is_list(Config) ->
    true = erlang:demonitor(make_ref()),
    ok.


%%% Cases for demonitor/1

demon_2(Config) when is_list(Config) ->
    R1 = erlang:monitor(process, self()),
    true = erlang:demonitor(R1),
    %% Extra demonitor
    true = erlang:demonitor(R1),
    expect_no_msg(),

    %% Normal 'DOWN'
    P2 = spawn(timer, sleep, [1]),
    R2 = erlang:monitor(process, P2),
    case expect_down(R2, P2) of
        normal -> ok;
        noproc -> ok;
        BadReason -> ct:fail({bad_reason, BadReason})
    end,

    %% OTP-5772
    %     %% 'DOWN' before demonitor
    %     P3 = spawn(timer, sleep, [100000]),
    %     R3 = erlang:monitor(process, P3),
    %     exit(P3, frop),
    %     erlang:demonitor(R3),
    %     expect_down(R3, P3, frop),

    %% Demonitor before 'DOWN'
    P4 = spawn(timer, sleep, [100000]),
    R4 = erlang:monitor(process, P4),
    erlang:demonitor(R4),
    exit(P4, frop),
    expect_no_msg(),

    ok.

%% Distributed case for demonitor/1 (OTP-3499)
demon_3(Config) when is_list(Config) ->
    {ok, N} = test_server:start_node(hej, slave, []),

    %% 'DOWN' before demonitor
    P2 = spawn(N, timer, sleep, [100000]),
    R2 = erlang:monitor(process, P2),
    true = test_server:stop_node(N),
    true = erlang:demonitor(R2),
    expect_down(R2, P2, noconnection),

    {ok, N2} = test_server:start_node(hej, slave, []),

    %% Demonitor before 'DOWN'
    P3 = spawn(N2, timer, sleep, [100000]),
    R3 = erlang:monitor(process, P3),
    true = erlang:demonitor(R3),
    true = test_server:stop_node(N2),
    expect_no_msg(),

    ok.

demonitor_flush(Config) when is_list(Config) ->
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), flush)),
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), [flus])),
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(x, [flush])),
    {ok, N} = test_server:start_node(demonitor_flush, slave, []),
    ok = demonitor_flush_test(N),
    true = test_server:stop_node(N),
    ok = demonitor_flush_test(node()).

demonitor_flush_test(Node) ->
    P = spawn(Node, timer, sleep, [100000]),
    M1 = erlang:monitor(process, P),
    M2 = erlang:monitor(process, P),
    M3 = erlang:monitor(process, P),
    M4 = erlang:monitor(process, P),
    true = erlang:demonitor(M1, [flush, flush]),
    exit(P, bang),
    receive {'DOWN', M2, process, P, bang} -> ok end,
    receive after 100 -> ok end,
    true = erlang:demonitor(M3, [flush]),
    true = erlang:demonitor(M4, []),
    receive {'DOWN', M4, process, P, bang} -> ok end,
    receive
        {'DOWN', M, _, _, _} =DM when M == M1,
                                      M == M3 ->
            ct:fail({unexpected_down_message, DM})
    after 100 ->
              ok
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
    {ok, N} = test_server:start_node(demonitor_flush, slave, []),
    Gs = generate(fun () -> start_remove_monitor_group(N) end,
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
    true = test_server:stop_node(N),
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

mon_1(Config) when is_list(Config) ->
    %% Normal case
    P2 = spawn(timer, sleep, [1]),
    R2 = erlang:monitor(process, P2),
    case expect_down(R2, P2) of
        normal -> ok;
        noproc -> ok;
        BadReason -> ct:fail({bad_reason, BadReason})
    end,
    {P2A,R2A} = spawn_monitor(timer, sleep, [1]),
    expect_down(R2A, P2A, normal),

    %% 'DOWN' with other reason
    P3 = spawn(timer, sleep, [100000]),
    R3 = erlang:monitor(process, P3),
    exit(P3, frop),
    expect_down(R3, P3, frop),
    {P3A,R3A} = spawn_monitor(timer, sleep, [100000]),
    exit(P3A, frop),
    expect_down(R3A, P3A, frop),

    %% Monitor fails because process is dead
    R4 = erlang:monitor(process, P3),
    expect_down(R4, P3, noproc),

    %% Normal case (named process)
    P5 = start_jeeves(jeeves),
    R5 = erlang:monitor(process, jeeves),
    tell_jeeves(P5, stop),
    expect_down(R5, {jeeves, node()}, normal),

    %% 'DOWN' with other reason and node explicit activation
    P6 = start_jeeves(jeeves),
    R6 = erlang:monitor(process, {jeeves, node()}),
    tell_jeeves(P6, {exit, frop}),
    expect_down(R6, {jeeves, node()}, frop),

    %% Monitor (named process) fails because process is dead
    R7 = erlang:monitor(process, {jeeves, node()}),
    expect_down(R7, {jeeves, node()}, noproc),

    ok.

%% Distributed cases for monitor/2
mon_2(Config) when is_list(Config) ->
    {ok, N1} = test_server:start_node(hej1, slave, []),

    %% Normal case
    P2 = spawn(N1, timer, sleep, [4000]),
    R2 = erlang:monitor(process, P2),
    expect_down(R2, P2, normal),

    %% 'DOWN' with other reason
    P3 = spawn(N1, timer, sleep, [100000]),
    R3 = erlang:monitor(process, P3),
    exit(P3, frop),
    expect_down(R3, P3, frop),

    %% Monitor fails because process is dead
    R4 = erlang:monitor(process, P3),
    expect_down(R4, P3, noproc),

    %% Other node goes down
    P5 = spawn(N1, timer, sleep, [100000]),
    R5 = erlang:monitor(process, P5),

    true = test_server:stop_node(N1),

    expect_down(R5, P5, noconnection),

    %% Monitor fails because other node is dead
    P6 = spawn(N1, timer, sleep, [100000]),
    R6 = erlang:monitor(process, P6),
    R6_Reason = expect_down(R6, P6),
    true = (R6_Reason == noconnection) orelse (R6_Reason == noproc),

    %% Start a new node that can load code in this module
    PA = filename:dirname(code:which(?MODULE)),
    {ok, N2} = test_server:start_node
    (hej2, slave, [{args, "-pa " ++ PA}]),

    %% Normal case (named process)
    P7 = start_jeeves({jeeves, N2}),
    R7 = erlang:monitor(process, {jeeves, N2}),
    tell_jeeves(P7, stop),
    expect_down(R7, {jeeves, N2}, normal),

    %% 'DOWN' with other reason (named process)
    P8 = start_jeeves({jeeves, N2}),
    R8 = erlang:monitor(process, {jeeves, N2}),
    tell_jeeves(P8, {exit, frop}),
    expect_down(R8, {jeeves, N2}, frop),

    %% Monitor (named process) fails because process is dead
    R9 = erlang:monitor(process, {jeeves, N2}),
    expect_down(R9, {jeeves, N2}, noproc),

    %% Other node goes down (named process)
    _P10 = start_jeeves({jeeves, N2}),
    R10 = erlang:monitor(process, {jeeves, N2}),

    true = test_server:stop_node(N2),

    expect_down(R10, {jeeves, N2}, noconnection),

    %% Monitor (named process) fails because other node is dead
    R11 = erlang:monitor(process, {jeeves, N2}),
    expect_down(R11, {jeeves, N2}, noconnection),

    ok.

%%% Large exit reason. Crashed first attempt to release R5B.

large_exit(Config) when is_list(Config) ->
    f(100),
    ok.

f(0) ->
    ok;
f(N) ->
    f(),
    f(N-1).

f() ->
    S0 = {big, tuple, with, [list, 4563784278]},
    S = {S0, term_to_binary(S0)},
    P = spawn(?MODULE, large_exit_sub, [S]),
    R = erlang:monitor(process, P),
    P ! hej,
    receive
        {'DOWN', R, process, P, X} ->
            io:format(" -> ~p~n", [X]),
            if
                X == S ->
                    ok;
                true ->
                    ct:fail({X, S})
            end;
        Other ->
            io:format(" -> ~p~n", [Other]),
            exit({answer, Other})
    end.

large_exit_sub(S) ->
    receive _X -> ok end,
    exit(S).

%%% Testing of monitor link list cleanup
%%% by using erlang:process_info(self(), monitors)
%%% and      erlang:process_info(self(), monitored_by)

list_cleanup(Config) when is_list(Config) ->
    P0 = self(),
    M  = node(),
    PA = filename:dirname(code:which(?MODULE)),
    true = register(master_bertie, self()),

    %% Normal local case, monitor and demonitor
    P1 = start_jeeves(jeeves),
    {[], []} = monitors(),
    expect_jeeves(P1, monitors, {monitors, {[], []}}),
    R1a = erlang:monitor(process, P1),
    {[{process, P1}], []} = monitors(),
    expect_jeeves(P1, monitors, {monitors, {[], [P0]}}),
    true = erlang:demonitor(R1a),
    expect_no_msg(),
    {[], []} = monitors(),
    expect_jeeves(P1, monitors, {monitors, {[], []}}),
    %% Remonitor named and try again, now exiting the monitored process
    R1b = erlang:monitor(process, jeeves),
    {[{process, {jeeves, M}}], []} = monitors(),
    expect_jeeves(P1, monitors, {monitors, {[], [P0]}}),
    tell_jeeves(P1, stop),
    expect_down(R1b, {jeeves, node()}, normal),
    {[], []} = monitors(),

    %% Slightly weird local case - the monitoring process crashes
    P2 = start_jeeves(jeeves),
    {[], []} = monitors(),
    expect_jeeves(P2, monitors, {monitors, {[], []}}),
    {monitor_process, _R2} = 
    ask_jeeves(P2, {monitor_process, master_bertie}),
    {[], [P2]} = monitors(),
    expect_jeeves(P2, monitors,
                  {monitors, {[{process, {master_bertie, node()}}], []}}),
    tell_jeeves(P2, {exit, frop}),
    timer:sleep(2000),
    {[], []} = monitors(),

    %% Start a new node that can load code in this module
    {ok, J} = test_server:start_node
    (jeeves, slave, [{args, "-pa " ++ PA}]),

    %% Normal remote case, monitor and demonitor
    P3 = start_jeeves({jeeves, J}),
    {[], []} = monitors(),
    expect_jeeves(P3, monitors, {monitors, {[], []}}),
    R3a = erlang:monitor(process, P3),
    {[{process, P3}], []} = monitors(),
    expect_jeeves(P3, monitors, {monitors, {[], [P0]}}),
    true = erlang:demonitor(R3a),
    expect_no_msg(),
    {[], []} = monitors(),
    expect_jeeves(P3, monitors, {monitors, {[], []}}),
    %% Remonitor named and try again, now exiting the monitored process
    R3b = erlang:monitor(process, {jeeves, J}),
    {[{process, {jeeves, J}}], []} = monitors(),
    expect_jeeves(P3, monitors, {monitors, {[], [P0]}}),
    tell_jeeves(P3, stop),
    expect_down(R3b, {jeeves, J}, normal),
    {[], []} = monitors(),

    %% Slightly weird remote case - the monitoring process crashes
    P4 = start_jeeves({jeeves, J}),
    {[], []} = monitors(),
    expect_jeeves(P4, monitors, {monitors, {[], []}}),
    {monitor_process, _R4} = 
    ask_jeeves(P4, {monitor_process, {master_bertie, M}}),
    {[], [P4]} = monitors(),
    expect_jeeves(P4, monitors, 
                  {monitors, {[{process, {master_bertie, M}}], []}} ),
    tell_jeeves(P4, {exit, frop}),
    timer:sleep(2000),
    {[], []} = monitors(),

    %% Now, the monitoring remote node crashes
    P5 = start_jeeves({jeeves, J}),
    {[], []} = monitors(),
    expect_jeeves(P5, monitors, {monitors, {[], []}}),
    {monitor_process, _R5} = 
    ask_jeeves(P5, {monitor_process, P0}),
    {[], [P5]} = monitors(),
    expect_jeeves(P5, monitors, 
                  {monitors, {[{process, P0}], []}} ),
    test_server:stop_node(J),
    timer:sleep(4000),
    {[], []} = monitors(),

    true = unregister(master_bertie),
    ok.


%%% Mixed internal and external monitors

mixer(Config) when is_list(Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    NN = [j0,j1,j2],
    NL0 = [begin
               {ok, J} = test_server:start_node(X,slave,[{args, "-pa " ++ PA}]),
               J
           end  || X <- NN],
    NL1 = lists:duplicate(2,node()) ++ NL0,
    Perm = perm(NL1),
    lists:foreach(
      fun(NL) ->
              Js = [start_jeeves({[],M}) || M <- (NL ++ NL)],
              [ask_jeeves(P,{monitor_process,self()}) || P <- Js],
              {monitored_by,MB} = process_info(self(),monitored_by),
              MBL = lists:sort(MB),
              JsL = lists:sort(Js),
              MBL = JsL,
              {monitors,[]}  = process_info(self(),monitors),
              [tell_jeeves(P,{exit,flaff}) || P <- Js],
              wait_for_m([],[],200)
      end,
      Perm),
    lists:foreach(
      fun(NL) ->
              Js = [start_jeeves({[],M}) || M <- (NL ++ NL)],
              Rs = [begin
                        {monitor_process,Ref} = ask_jeeves(P,{monitor_process,self()}),
                        {P,Ref}
                    end || P <- Js],
              {monitored_by,MB} = process_info(self(),monitored_by),
              MBL = lists:sort(MB),
              JsL = lists:sort(Js),
              MBL = JsL,
              {monitors,[]}  = process_info(self(),monitors),
              [ask_jeeves(P,{demonitor,Ref}) || {P,Ref} <- Rs],
              wait_for_m([],[],200),
              [tell_jeeves(P,{exit,flaff}) || P <- Js]
      end,
      Perm),
    lists:foreach(
      fun(NL) ->
              Js = [start_jeeves({[],M}) || M <- (NL ++ NL)],
              [ask_jeeves(P,{monitor_process,self()}) || P <- Js],
              [erlang:monitor(process,P) || P <- Js],
              {monitored_by,MB} = process_info(self(),monitored_by),
              MBL = lists:sort(MB),
              JsL = lists:sort(Js),
              MBL = JsL,
              {monitors,M} = process_info(self(),monitors),
              ML = lists:sort([P||{process,P} <- M]),
              ML = JsL,
              [begin
                   tell_jeeves(P,{exit,flaff}),
                   receive {'DOWN',_,process,P,_} -> ok end
               end || P <- Js],
              wait_for_m([],[],200)
      end,
      Perm),
    lists:foreach(
      fun(NL) ->
              Js = [start_jeeves({[],M}) || M <- (NL ++ NL)],
              Rs = [begin
                        {monitor_process,Ref} = ask_jeeves(P,{monitor_process,self()}),
                        {P,Ref}
                    end || P <- Js],
              R2s = [{P,erlang:monitor(process,P)} || P <- Js],
              {monitored_by,MB} = process_info(self(),monitored_by),
              MBL = lists:sort(MB),
              JsL = lists:sort(Js),
              MBL = JsL,
              {monitors,M} = process_info(self(),monitors),
              ML = lists:sort([P||{process,P} <- M]),
              ML = JsL,
              [ask_jeeves(P,{demonitor,Ref}) || {P,Ref} <- Rs],
              wait_for_m(lists:sort(M),[],200),
              [erlang:demonitor(Ref) || {_P,Ref} <- R2s],
              wait_for_m([],[],200),
              [tell_jeeves(P,{exit,flaff}) || P <- Js]
      end,
      Perm),
    [test_server:stop_node(K) || K <- NL0],
    ok.

%% Test that DOWN message for a named monitor isn't
%%  delivered until name has been unregistered
named_down(Config) when is_list(Config) ->
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-named_down-"
                        ++ integer_to_list(erlang:system_time(second))
                        ++ "-" ++ integer_to_list(erlang:unique_integer([positive]))),
    Prio = process_flag(priority,high),
    %% Spawn a bunch of high prio cpu bound processes to prevent
    %% normal prio processes from terminating during the next
    %% 500 ms...
    Self = self(),
    spawn_opt(fun () ->
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
    NamedProc = spawn_link(fun () ->
                                   receive after infinity -> ok end
                           end),
    ?assertEqual(true, register(Name, NamedProc)),
    unlink(NamedProc),
    Mon = erlang:monitor(process, Name),
    exit(NamedProc, bang),
    receive {'DOWN',Mon, _, _, bang} -> ok
    after 3000 -> ?assert(false) end,
    ?assertEqual(true, register(Name, self())),
    ?assertEqual(true, unregister(Name)),
    process_flag(priority,Prio),
    ok.

otp_5827(Config) when is_list(Config) ->
    %% Make a pid with the same nodename but with another creation
    [CreEnd | RPTail]
    = lists:reverse(binary_to_list(term_to_binary(self()))),
    NewCreEnd = case CreEnd of
                    0 -> 1;
                    1 -> 2;
                    _ -> CreEnd - 1
                end,
    OtherCreationPid
    = binary_to_term(list_to_binary(lists:reverse([NewCreEnd | RPTail]))),
    %% If the bug is present erlang:monitor(process, OtherCreationPid)
    %% will hang...
    Parent = self(),
    Ok = make_ref(),
    spawn(fun () ->
                  Mon = erlang:monitor(process, OtherCreationPid),
                  % Should get the DOWN message right away
                  receive
                      {'DOWN', Mon, process, OtherCreationPid, noproc} ->
                          Parent ! Ok
                  end
          end),
    receive
        Ok ->
            ok
    after 1000 ->
              ct:fail("erlang:monitor/2 hangs")
    end.

monitor_time_offset(Config) when is_list(Config) ->
    {ok, Node} = start_node(Config, "+C single_time_warp"),
    Me = self(),
    PMs = lists:map(fun (_) ->
                            Pid = spawn(Node,
                                        fun () ->
                                                check_monitor_time_offset(Me)
                                        end),
                            {Pid, erlang:monitor(process, Pid)}
                    end,
                    lists:seq(1, 100)),
    lists:foreach(fun ({P, _M}) ->
                          P ! check_no_change_message
                  end, PMs),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {no_change_message_received, P} ->
                                  ok;
                              {'DOWN', M, process, P, Reason} ->
                                  ct:fail(Reason)
                          end
                  end, PMs),
    preliminary = rpc:call(Node, erlang, system_flag, [time_offset, finalize]),
    lists:foreach(fun ({P, M}) ->
                          receive
                              {change_messages_received, P} ->
                                  erlang:demonitor(M, [flush]);
                              {'DOWN', M, process, P, Reason} ->
                                  ct:fail(Reason)
                          end
                  end, PMs),
    stop_node(Node),
    ok.

check_monitor_time_offset(Leader) ->
    Mon1 = erlang:monitor(time_offset, clock_service),
    Mon2 = erlang:monitor(time_offset, clock_service),
    Mon3 = erlang:monitor(time_offset, clock_service),
    Mon4 = erlang:monitor(time_offset, clock_service),

    erlang:demonitor(Mon2, [flush]),

    Mon5 = erlang:monitor(time_offset, clock_service),
    Mon6 = erlang:monitor(time_offset, clock_service),
    Mon7 = erlang:monitor(time_offset, clock_service),

    receive check_no_change_message -> ok end,
    receive
        {'CHANGE', _, time_offset, clock_service, _} ->
            exit(unexpected_change_message_received)
    after 0 ->
              Leader ! {no_change_message_received, self()}
    end,
    receive after 100 -> ok end,
    erlang:demonitor(Mon4, [flush]),
    receive
        {'CHANGE', Mon3, time_offset, clock_service, _} ->
            ok
    end,
    receive
        {'CHANGE', Mon6, time_offset, clock_service, _} ->
            ok
    end,
    erlang:demonitor(Mon5, [flush]),
    receive
        {'CHANGE', Mon7, time_offset, clock_service, _} ->
            ok
    end,
    receive
        {'CHANGE', Mon1, time_offset, clock_service, _} ->
            ok
    end,
    receive
        {'CHANGE', _, time_offset, clock_service, _} ->
            exit(unexpected_change_message_received)
    after 1000 ->
              ok
    end,
    Leader ! {change_messages_received, self()}.

%%
%% ...
%%

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
            ct:fail({rec, Other})
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
            ct:fail({rec, Other})
    end.


expect_jeeves(Pid, Request, Response) when is_pid(Pid) ->
    Pid ! {self(), Request},
    receive 
        {Pid, Response} -> 
            ok;
        Other ->
            ct:fail({rec, Other})
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

start_node(Config, Args) ->
    TestCase = proplists:get_value(testcase, Config),
    PA = filename:dirname(code:which(?MODULE)),
    ESTime = erlang:monotonic_time(1) + erlang:time_offset(1),
    Unique = erlang:unique_integer([positive]),
    Name = list_to_atom(atom_to_list(?MODULE)
                        ++ "-"
                        ++ atom_to_list(TestCase)
                        ++ "-"
                        ++ integer_to_list(ESTime)
                        ++ "-"
                        ++ integer_to_list(Unique)),
    test_server:start_node(Name,
                           slave,
                           [{args, "-pa " ++ PA ++ " " ++ Args}]).

stop_node(Node) ->
    test_server:stop_node(Node).
