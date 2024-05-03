%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2023. All Rights Reserved.
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
         init_per_testcase/2, end_per_testcase/2,
         case_1/1, case_1a/1, case_2/1, case_2a/1, mon_e_1/1, demon_e_1/1, demon_1/1,
         demon_2/1, demon_3/1, demonitor_flush/1, gh_5225_demonitor_alias/1,
         local_remove_monitor/1, remote_remove_monitor/1, mon_1/1, mon_2/1,
         large_exit/1, list_cleanup/1, mixer/1, named_down/1, otp_5827/1,
         monitor_time_offset/1, monitor_tag_storage/1,
         unexpected_alias_at_demonitor_gh5310/1,
         down_on_alias_gh5310/1, monitor_3_noproc_gh6185/1]).

-export([y2/1, g/1, g0/0, g1/0, large_exit_sub/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [case_1, case_1a, case_2, case_2a, mon_e_1, demon_e_1,
     demon_1, mon_1, mon_2, demon_2, demon_3, demonitor_flush,
     gh_5225_demonitor_alias, {group, remove_monitor}, large_exit,
     list_cleanup, mixer, named_down, otp_5827,
     monitor_time_offset, monitor_tag_storage,
     unexpected_alias_at_demonitor_gh5310,
     down_on_alias_gh5310, monitor_3_noproc_gh6185].

groups() -> 
    [{remove_monitor, [],
      [local_remove_monitor, remote_remove_monitor]}].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

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
    {ok, Peer, N} = ?CT_PEER(),
    mon_error(plutt, self()),
    mon_error(process, [bingo]),
    mon_error(process, {rex, N, junk}),
    mon_error(process, 1),

    peer:stop(Peer),
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
    {ok, Peer, N} = ?CT_PEER(),
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

    peer:stop(Peer),
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
    {ok, Peer, N} = ?CT_PEER(),

    %% 'DOWN' before demonitor
    P2 = spawn(N, timer, sleep, [100000]),
    R2 = erlang:monitor(process, P2),
    peer:stop(Peer),
    true = erlang:demonitor(R2),
    expect_down(R2, P2, noconnection),

    {ok, Peer2, N2} = ?CT_PEER(),

    %% Demonitor before 'DOWN'
    P3 = spawn(N2, timer, sleep, [100000]),
    R3 = erlang:monitor(process, P3),
    true = erlang:demonitor(R3),
    peer:stop(Peer2),
    expect_no_msg(),

    ok.

demonitor_flush(Config) when is_list(Config) ->
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), flush)),
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(make_ref(), [flus])),
    {'EXIT', {badarg, _}} = (catch erlang:demonitor(x, [flush])),
    {ok, Peer, N} = ?CT_PEER(),
    ok = demonitor_flush_test(N),
    peer:stop(Peer),
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

gh_5225_demonitor_alias(Config) when is_list(Config) ->
    %% Demonitor using a reference that was an active alias, but not an
    %% active monitor, used to crash the runtime system.
    Alias = alias(),
    erlang:demonitor(Alias),
    erlang:demonitor(Alias, [flush]),
    {Pid, MonAlias1} = spawn_opt(fun () ->
                                         ok
                                 end,
                                 [{monitor, [{alias, explicit_unalias}]}]),
    receive {'DOWN', MonAlias1, process, Pid, normal} -> ok end,
    erlang:demonitor(MonAlias1),
    erlang:demonitor(MonAlias1, [flush]),
    MonAlias2 = erlang:monitor(process, Pid, [{alias, explicit_unalias}]),
    receive {'DOWN', MonAlias2, process, Pid, noproc} -> ok end,
    erlang:demonitor(MonAlias2),
    erlang:demonitor(MonAlias2, [flush]),
    ok.

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
    {ok, Peer, N} = ?CT_PEER(),
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
    peer:stop(Peer),
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
    {ok, Peer, N1} = ?CT_PEER(),

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

    peer:stop(Peer),

    expect_down(R5, P5, noconnection),

    %% Monitor fails because other node is dead
    P6 = spawn(N1, timer, sleep, [100000]),
    R6 = erlang:monitor(process, P6),
    R6_Reason = expect_down(R6, P6),
    true = (R6_Reason == noconnection) orelse (R6_Reason == noproc),

    %% Start a new node that can load code in this module
    {ok, Peer2, N2} = ?CT_PEER(),

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

    peer:stop(Peer2),

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
    {ok, Peer, J} = ?CT_PEER(),

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
    peer:stop(Peer),
    timer:sleep(4000),
    {[], []} = monitors(),

    true = unregister(master_bertie),
    ok.


%%% Mixed internal and external monitors

mixer(Config) when is_list(Config) ->
    {_, Peers, NL0} = lists:unzip3([?CT_PEER() || _ <- lists:seq(1, 3)]),
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
    [peer:stop(P) || P <- Peers],
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
    {ok, Peer, Node} = ?CT_PEER(["+C", "single_time_warp"]),
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
    peer:stop(Peer),
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


monitor_tag_storage(Config) when is_list(Config) ->
    process_flag(priority, max),
    %% WHITEBOX:
    %%
    %% There are three scenarios we want to test. The receiver of the
    %% DOWN message with tag:
    %% * has on-heap message queue data enabled and can allocate
    %%   DOWN message on the heap
    %% * has on-heap message queue data enabled and cannot allocate
    %%   DOWN message on the heap, i.e. the message will be allocated
    %%   in a heap fragment
    %% * has off-heap message queue data enabled, i.e. the message
    %%   will be allocated in a combined message/heap fragment.

    %%
    %% Testing the two on heap message queue data scenarios. Initially
    %% there will be room on the heap, but eventually DOWN messages
    %% will be placed in heap fragments.
    %%    
    ok = monitor_tag_storage_test(on_heap),

    %%
    %% Testing the off heap message queue data scenarios.
    %%
    ok = monitor_tag_storage_test(off_heap).

monitor_tag_storage_test(MQD) ->
    Len = 1000,
    Tag = make_ref(),
    Parent = self(),
    Ps = lists:map(fun (_) ->
                           spawn_opt(fun () ->
                                              receive after infinity -> ok end
                                     end, [link,{priority,high}])
                   end, lists:seq(1, Len)),
    {Recvr, RMon} = spawn_opt(fun () ->
                                      lists:foreach(fun (P) ->
                                                            erlang:monitor(process,
                                                                           P,
                                                                           [{tag, Tag}])
                                                    end, Ps),
                                      Parent ! {ready, self()},
                                      receive {continue, Parent} -> ok end,
                                      garbage_collect(),
                                      Msgs = receive_tagged_down_msgs(Tag, []),
                                      Len = length(Msgs),
                                      garbage_collect(),
                                      id(Msgs)
                              end, [link, monitor, {message_queue_data, MQD}]),
    receive {ready, Recvr} -> ok end,
    lists:foreach(fun (P) -> unlink(P), exit(P, bang) end, Ps),
    wait_until(fun () ->
                       {message_queue_len, Len} == process_info(Recvr,
                                                                message_queue_len)
               end),
    {messages, Msgs} = process_info(Recvr, messages),
    lists:foreach(fun (Msg) ->
                          {Tag, _Mon, process, _Pid, bang} = Msg
                  end,
                  Msgs),
    garbage_collect(),
    id(Msgs),
    Recvr ! {continue, self()},
    receive
        {'DOWN', RMon, process, Recvr, Reason} ->
            normal = Reason
    end,
    ok.

receive_tagged_down_msgs(Tag, Msgs) ->
    receive
        {Tag, _Mon, process, _Pid, bang} = Msg ->
            receive_tagged_down_msgs(Tag, [Msg|Msgs])
    after
        0 ->
            Msgs
    end.

unexpected_alias_at_demonitor_gh5310(Config) when is_list(Config) ->
    %% The demonitor operation erroneously behaved as if the
    %% monitor had been created using the {alias, explicit_unalias}
    %% option...
    Pid = spawn_link(fun () ->
                             receive
                                 {alias, Alias} ->
                                     Alias ! {hello_via_alias, self()}
                             end
                     end),
    Mon = erlang:monitor(process, Pid),
    AliasMon = erlang:monitor(process, Pid, [{alias, reply_demonitor}]),
    erlang:demonitor(AliasMon, [flush]),
    Pid ! {alias, AliasMon},
    receive
        {'DOWN', Mon, process, Pid, normal} ->
            ok
    end,
    receive
        {hello_via_alias, Pid} ->
            ct:fail(unexpected_message_via_alias)
    after
        0 ->
            ok
    end.

down_on_alias_gh5310(Config) when is_list(Config) ->
    %% Could only occur when the internal monitor structure was transformed
    %% into an alias structure during the demonitor() operation, the target
    %% terminated before the demonitor signal reached it, and the exit reason
    %% wasn't an immediate. We test with both immediate and compound exit
    %% reason just to make sure we don't introduce the bug in the immediate
    %% case at a later time...
    process_flag(scheduler, 1),
    {DeMonSched, TermSched} = case erlang:system_info(schedulers) of
                                  1 -> {1, 1};
                                  2 -> {1, 2};
                                  _ -> {2, 3}
                              end,
    lists:foreach(fun (N) ->
                          ImmedExitReason = case N rem 2 of
                                      0 -> true;
                                      _ -> false
                                  end,
                          down_on_alias_gh5310_test(ImmedExitReason,
                                                    DeMonSched, TermSched)
                  end,
                  lists:seq(1, 200)),
    ok.

down_on_alias_gh5310_test(ImmedExitReason, DeMonSched, TermSched) ->
    Go = make_ref(),
    Done = make_ref(),
    Parent = self(),
    TermPid = spawn_opt(fun () ->
                                Parent ! {ready, self()},
                                receive Go ->
                                        if ImmedExitReason == true ->
                                                exit(bye);
                                           true ->
                                                exit(Go)
                                        end
                                end
                        end, [{scheduler, TermSched}]),
    DeMonPid = spawn_opt(fun () ->
                                 AliasMon = erlang:monitor(process, TermPid,
                                                           [{alias, explicit_unalias}]),
                                 Parent ! {ready, self()},
                                 receive Go -> ok end,
                                 erlang:demonitor(AliasMon, [flush]),
                                 busy_wait_until(fun () ->
                                                         not is_process_alive(TermPid)
                                                 end),
                                 receive
                                     {'DOWN', AliasMon, process, _, _} = DownMsg ->
                                         exit({unexpected_msg, DownMsg})
                                 after
                                     0 ->
                                         Parent ! Done
                                 end
                         end, [{scheduler, DeMonSched}, link]),
    receive {ready, TermPid} -> ok end,
    receive {ready, DeMonPid} -> ok end,
    erlang:yield(),
    TermPid ! Go,
    DeMonPid ! Go,
    receive Done -> ok end.


monitor_3_noproc_gh6185(Config) when is_list(Config) ->
    monitor_3_noproc_gh6185_test(false, false),
    monitor_3_noproc_gh6185_test(true, false),
    monitor_3_noproc_gh6185_test(false, true),
    monitor_3_noproc_gh6185_test(true, true),
    monitor_3_noproc_gh6185_exit_test(false, false),
    monitor_3_noproc_gh6185_exit_test(true, false),
    monitor_3_noproc_gh6185_exit_test(false, true),
    monitor_3_noproc_gh6185_exit_test(true, true).

monitor_3_noproc_gh6185_test(AliasTest, TagTest) ->
    NodeName = node(),
    UN = undefined_name_gh6185,
    UNN = {UN, NodeName},
    undefined = whereis(UN),

    {AliasOpt, CheckAlias}
        = case AliasTest of
              false ->
                  {[], fun (_NotAnAlias) -> ok end};
              true ->
                  {[{alias, explicit_unalias}],
                   fun (Alias) ->
                           AMsg1 = make_ref(),
                           OMsg1 = make_ref(),
                           Alias ! AMsg1,
                           self() ! OMsg1,
                           receive OMsg1 -> ok end,
                           receive AMsg1 -> ok
                           after 0 -> ct:fail(missing_alias_message)
                           end,
                           unalias(Alias),
                           AMsg2 = make_ref(),
                           OMsg2 = make_ref(),
                           Alias ! AMsg2,
                           self() ! OMsg2,
                           receive OMsg2 -> ok end,
                           receive AMsg2 -> ct:fail(unexpected_alias_message)
                           after 0 -> ok
                           end
                   end}
          end,

    TagFun = case TagTest of
                 false ->
                     fun () ->
                             {'DOWN', []}
                     end;
                 true ->
                     fun () ->
                             Tag = make_ref(),
                             {Tag, [{tag, Tag}]}
                     end
             end,

    %% not registerd process...
    {Tag1, TagOpt1} = TagFun(),
    M1 = erlang:monitor(process, UN, AliasOpt ++ TagOpt1),
    receive
        {Tag1, M1, process, UNN, noproc} ->
            ok;
        ID1 when element(2, ID1) == M1 ->
            ct:fail({invalid_down, ID1})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M1),

    {Tag2, TagOpt2} = TagFun(),
    M2 = erlang:monitor(process, UNN, AliasOpt ++ TagOpt2),
    receive
        {Tag2, M2, process, UNN, noproc} ->
            ok;
        ID2 when element(2, ID2) == M2 ->
            ct:fail({invalid_down, ID2})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M2),

    %% Not registered port...
    {Tag3, TagOpt3} = TagFun(),
    M3 = erlang:monitor(port, UN, AliasOpt ++ TagOpt3),
    receive
        {Tag3, M3, port, UNN, noproc} ->
            ok;
        ID3 when element(2, ID3) == M3 ->
            ct:fail({invalid_down, ID3})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M3),

    {Tag4, TagOpt4} = TagFun(),
    M4 = erlang:monitor(port, UNN, AliasOpt ++ TagOpt4),
    receive
        {Tag4, M4, port, UNN, noproc} ->
            ok;
        ID4 when element(2, ID4) == M4 ->
            ct:fail({invalid_down, ID4})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M4),


    OldCreation = case erlang:system_info(creation) of
                      Creation when Creation =< 4 -> 16#ffffffff;
                      Creation -> Creation - 1
                  end,

    %% Process of old incarnation...
    Pid = erts_test_utils:mk_ext_pid({NodeName, OldCreation}, 4711, 0),
    {Tag5, TagOpt5} = TagFun(),
    M5 = erlang:monitor(process, Pid, AliasOpt ++ TagOpt5),
    receive
        {Tag5, M5, process, Pid, noproc} ->
            ok;
        ID5 when element(2, ID5) == M5 ->
            ct:fail({invalid_down, ID5})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M5),

    %% Port of old incarnation...
    Prt = erts_test_utils:mk_ext_port({NodeName, OldCreation}, 4711),
    {Tag6, TagOpt6} = TagFun(),
    M6 = erlang:monitor(port, Prt, AliasOpt ++ TagOpt6),
    receive
        {Tag6, M6, port, Prt, noproc} ->
            ok;
        ID6 when element(2, ID6) == M6 ->
            ct:fail({invalid_down, ID6})
    after 100 ->
            ct:fail(missing_down)
    end,
    CheckAlias(M6),

    ok.

monitor_3_noproc_gh6185_exit_test(AliasTest, TagTest) ->
    %%
    %% Testing that we handle these quite unusual monitors correct
    %% in case the monotoring process dies right after setting up
    %% the monitor. We cannot check any results, but we might hit
    %% asserts, crashes, or memory leaks if any bugs exist...
    %%

    NodeName = node(),
    UN = undefined_name_gh6185,
    UNN = {UN, NodeName},
    undefined = whereis(UN),

    AliasOpt = case AliasTest of
                   false -> [];
                   true -> [{alias, explicit_unalias}]
               end,

    TagOpt = case TagTest of
                 false -> [];
                 true -> [{tag, make_ref()}]
             end,

    %% not registerd process...
    {P1, M1} = spawn_monitor(fun () ->
                                     erlang:yield(),
                                     _ = erlang:monitor(process, UN, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M1, process, P1, bang} -> ok end,
    {P2, M2} = spawn_monitor(fun () ->
                                     erlang:yield(),
                                     _ = erlang:monitor(process, UNN, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M2, process, P2, bang} -> ok end,

    %% Not registered port...
    {P3, M3} = spawn_monitor(fun () ->
                                     erlang:yield(),
                                     _ = erlang:monitor(port, UN, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M3, process, P3, bang} -> ok end,
    {P4, M4} = spawn_monitor(fun () ->
                                     erlang:yield(),
                                     _ = erlang:monitor(port, UNN, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M4, process, P4, bang} -> ok end,


    OldCreation = case erlang:system_info(creation) of
                      Creation when Creation =< 4 -> 16#ffffffff;
                      Creation -> Creation - 1
                  end,

    %% Process of old incarnation...
    {P5, M5} = spawn_monitor(fun () ->
                                     Pid = erts_test_utils:mk_ext_pid({NodeName,
                                                                       OldCreation},
                                                                      4711, 0),
                                     erlang:yield(),
                                     _ = erlang:monitor(process, Pid, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M5, process, P5, bang} -> ok end,

    %% Port of old incarnation...
    {P6, M6} = spawn_monitor(fun () ->
                                     Prt = erts_test_utils:mk_ext_port({NodeName,
                                                                        OldCreation},
                                                                       4711),
                                     erlang:yield(),
                                     _ = erlang:monitor(port, Prt, AliasOpt ++ TagOpt),
                                     exit(bang)
                             end),
    receive {'DOWN', M6, process, P6, bang} -> ok end,
    ok.

%%
%% ...
%%

id(X) -> X.

busy_wait_until(Fun) ->
    case catch Fun() of
        true ->
            ok;
        _ ->
            busy_wait_until(Fun)
    end.

wait_until(Fun) ->
    case catch Fun() of
        true ->
            ok;
        _ ->
            receive after 100 -> ok end,
            wait_until(Fun)
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
