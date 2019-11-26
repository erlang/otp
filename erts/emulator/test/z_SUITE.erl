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
%%% File    : z_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Misc tests that should be run last
%%%
%%% Created : 15 Jul 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(z_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_testcase/2, end_per_testcase/2]).

-export([schedulers_alive/1, node_container_refc_check/1,
	 long_timers/1, pollset_size/1,
	 check_io_debug/1, get_check_io_info/0,
         lc_graph/1,
         leaked_processes/1,
         literal_area_collector/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() -> 
    [schedulers_alive, node_container_refc_check,
     long_timers, pollset_size, check_io_debug,
     lc_graph,
     %% Make sure that the leaked_processes/1 is always
     %% run last.
     leaked_processes,
     literal_area_collector].

init_per_testcase(schedulers_alive, Config) ->
    case erlang:system_info(schedulers) of
        1 ->
            {skip, "Needs more schedulers to run"};
        _ ->
            Config
    end;
init_per_testcase(_, Config) ->
    Config.


end_per_testcase(_Name, Config) ->
    Config.

%%%
%%% The test cases -------------------------------------------------------------
%%%

%% Tests that all schedulers are actually used
schedulers_alive(Config) when is_list(Config) ->
    Master = self(),
    NoSchedulersOnline = erlang:system_flag(
                           schedulers_online,
                           erlang:system_info(schedulers)),
    NoSchedulers = erlang:system_info(schedulers),
    UsedScheds =
      try
          io:format("Number of schedulers configured: ~p~n", [NoSchedulers]),
          case erlang:system_info(multi_scheduling) of
              blocked ->
                  ct:fail(multi_scheduling_blocked);
              disabled ->
                  ok;
              enabled ->
                  io:format("Testing blocking process exit~n"),
                  BF = fun () ->
                               blocked_normal = erlang:system_flag(multi_scheduling,
								   block_normal),
                               Master ! {self(), blocking},
                               receive after infinity -> ok end
                       end,
                  Blocker = spawn_link(BF),
                  Mon = erlang:monitor(process, Blocker),
                  receive {Blocker, blocking} -> ok end,
                  [Blocker]
                  = erlang:system_info(normal_multi_scheduling_blockers),
                  unlink(Blocker),
                  exit(Blocker, kill),
                  receive {'DOWN', Mon, _, _, _} -> ok end,
                  enabled = erlang:system_info(multi_scheduling),
                  [] = erlang:system_info(normal_multi_scheduling_blockers),
                  ok
          end,
          io:format("Testing blocked~n"),
          erlang:system_flag(multi_scheduling, block_normal),
          case erlang:system_info(multi_scheduling) of
              enabled ->
                  ct:fail(multi_scheduling_enabled);
              blocked_normal ->
                  [Master] = erlang:system_info(normal_multi_scheduling_blockers);
              disabled -> ok
          end,
          Ps = lists:map(
                 fun (_) ->
                         spawn_link(fun () ->
                                            run_on_schedulers(none,
                                                              [],
                                                              Master)
                                    end)
                 end,
                 lists:seq(1,NoSchedulers)),
          receive after 1000 -> ok end,
          {_, 1} = verify_all_schedulers_used({[],0}, 1),
          lists:foreach(fun (P) ->
                                unlink(P),
                                exit(P, bang)
                        end, Ps),
          case erlang:system_flag(multi_scheduling, unblock_normal) of
              blocked_normal -> ct:fail(multi_scheduling_blocked);
              disabled -> ok;
              enabled -> ok
          end,
          erts_debug:set_internal_state(available_internal_state, true),
          %% node_and_dist_references will use emulator interal thread blocking...
          erts_debug:get_internal_state(node_and_dist_references), 
          erts_debug:set_internal_state(available_internal_state, false),
          io:format("Testing not blocked~n"),
          Ps2 = lists:map(
                  fun (_) ->
                          spawn_link(fun () ->
                                             run_on_schedulers(none,
                                                               [],
                                                               Master)
                                     end)
                  end,
                  lists:seq(1,NoSchedulers)),
          receive after 1000 -> ok end,
          {_, NoSIDs} = verify_all_schedulers_used({[],0},NoSchedulers),
          lists:foreach(fun (P) ->
                                unlink(P),
                                exit(P, bang)
                        end, Ps2),
          NoSIDs
      after
          NoSchedulers = erlang:system_flag(schedulers_online,
                                            NoSchedulersOnline),
          NoSchedulersOnline = erlang:system_info(schedulers_online)
      end,
        {comment, "Number of schedulers " ++ integer_to_list(UsedScheds)}.


run_on_schedulers(LastSID, SIDs, ReportTo) ->
    case erlang:system_info(scheduler_id) of
	LastSID ->
	    erlang:yield(),
	    run_on_schedulers(LastSID, SIDs, ReportTo);
	SID ->
	    NewSIDs = case lists:member(SID, SIDs) of
			  true ->
			      SIDs;
			  false ->
			      ReportTo ! {scheduler_used, SID},
			      [SID | SIDs]
		      end,
	    erlang:yield(),
	    run_on_schedulers(SID, NewSIDs, ReportTo)
    end.

wait_on_used_scheduler({SIDs, SIDsLen} = State) ->
    receive
	{scheduler_used, SID} ->
	    case lists:member(SID, SIDs) of
		true ->
		    wait_on_used_scheduler(State);
		false ->
		    io:format("Scheduler ~p used~n", [SID]),
		    {[SID|SIDs], SIDsLen+1}
	    end
    end.

verify_all_schedulers_used({UsedSIDs, UsedSIDsLen} = State, NoSchedulers) ->
    case NoSchedulers of
	      UsedSIDsLen ->
		  State;
	      NoSchdlrs when NoSchdlrs < UsedSIDsLen ->
		  ct:fail({more_schedulers_used_than_exist,
				 {existing_schedulers, NoSchdlrs},
				 {used_schedulers, UsedSIDsLen},
				 {used_scheduler_ids, UsedSIDs}});
	      _ ->
		  NewState = wait_on_used_scheduler(State),
		  verify_all_schedulers_used(NewState, NoSchedulers)
	  end.

node_container_refc_check(Config) when is_list(Config) ->
    node_container_SUITE:node_container_refc_check(node()),
    ok.

long_timers(Config) when is_list(Config) ->
    case long_timers_test:check_result() of
	ok -> ok;
	high_cpu -> {comment, "Ignored failures due to high CPU utilization"};
	missing_cpu_info -> {comment, "Ignored failures due to missing CPU utilization information"};
	Fail -> ct:fail(Fail)
    end.
	    

pollset_size(Config) when is_list(Config) ->
    Name = pollset_size_testcase_initial_state_holder,
    Mon = erlang:monitor(process, Name),
    (catch Name ! {get_initial_check_io_result, self()}),
    InitChkIo = receive
			  {initial_check_io_result, ICIO} ->
			      erlang:demonitor(Mon, [flush]),
			      ICIO;
			  {'DOWN', Mon, _, _, Reason} ->
			      ct:fail({non_existing, Name, Reason})
		      end,
    FinChkIo = get_check_io_info(),
    io:format("Initial: ~p~nFinal: ~p~n", [InitChkIo, FinChkIo]),
    InitPollsetSize = lists:keysearch(total_poll_set_size, 1, InitChkIo),
    FinPollsetSize = lists:keysearch(total_poll_set_size, 1, FinChkIo),
    HasGethost = case has_gethost() of true -> 1; _ -> 0 end,
    case InitPollsetSize =:= FinPollsetSize of
	      true ->
		  case InitPollsetSize of
		      {value, {total_poll_set_size, Size}} ->
			  {comment,
				 "Pollset size: " ++ integer_to_list(Size)};
		      _ ->
			  {skipped,
				 "Pollset size information not available"}
		  end;
	      false ->
		  %% Sometimes we have fewer descriptors in the
		  %% pollset at the end than when we started, but
		  %% that is ok as long as there are at least 2
		  %% descriptors (dist listen socket and
		  %% epmd socket) in the pollset.
		  {value, {total_poll_set_size, InitSize}}
		      = InitPollsetSize,
		  {value, {total_poll_set_size, FinSize}}
		      = FinPollsetSize,
		  true = FinSize < (InitSize + HasGethost),
		  true = 2 =< FinSize,
		  {comment,
			 "Start pollset size: "
			 ++ integer_to_list(InitSize)
			 ++ " End pollset size: "
			 ++ integer_to_list(FinSize)}
	  end.

check_io_debug(Config) when is_list(Config) ->
    case lists:keysearch(name, 1, hd(erlang:system_info(check_io))) of
	      {value, {name, erts_poll}} -> check_io_debug_test();
	      _ -> {skipped, "Not implemented in this emulator"}
	  end.

check_io_debug_test() ->
    erlang:display(get_check_io_info()),
    erts_debug:set_internal_state(available_internal_state, true),
    {NoErrorFds, NoUsedFds, NoDrvSelStructs, NoDrvEvStructs} = CheckIoDebug
	= erts_debug:get_internal_state(check_io_debug),
    erts_debug:set_internal_state(available_internal_state, false),
    HasGetHost = has_gethost(),
    ct:log("check_io_debug: ~p~n"
           "HasGetHost: ~p",[CheckIoDebug, HasGetHost]),
    0 = NoErrorFds,
    if
        NoUsedFds == NoDrvSelStructs ->
            ok;
        HasGetHost andalso (NoUsedFds == (NoDrvSelStructs - 1)) ->
            %% If the inet_gethost port is alive, we may have
            %% one extra used fd that is not selected on.
            %% This happens when the initial setup of the
            %% port returns an EAGAIN
            ok
    end,
    0 = NoDrvEvStructs,
    ok.

has_gethost() ->
    has_gethost(erlang:ports()).
has_gethost([P|T]) ->
    case erlang:port_info(P, name) of
        {name,"inet_gethost"++_} ->
            true;
        _ ->
            has_gethost(T)
    end;
has_gethost([]) ->
    false.

lc_graph(Config) when is_list(Config) ->
    %% Create "lc_graph" file in current working dir
    %% if lock checker is enabled
    erts_debug:lc_graph(),
    ok.

leaked_processes(Config) when is_list(Config) ->
    %% Replace the defualt timetrap with a timetrap with
    %% known pid.
    test_server:timetrap_cancel(),
    Dog = test_server:timetrap(test_server:minutes(5)),

    Name = leaked_processes__process_holder,
    Name ! {get_initial_processes, self()},
    receive
        {initial_processes, Initial0} -> ok
    end,
    Initial = ordsets:from_list(Initial0),

    KnownPids = ordsets:from_list([self(),Dog]),
    Now0 = ordsets:from_list(processes()),
    Now = ordsets:subtract(Now0, KnownPids),
    Leaked = ordsets:subtract(Now, Initial),

    _ = [begin
             Info = process_info(P) ++ process_info(P, [current_stacktrace]),
             io:format("~p: ~p\n", [P,Info])
         end || P <- Leaked],
    Comment = lists:flatten(io_lib:format("~p process(es)",
                                          [length(Leaked)])),
    {comment, Comment}.

literal_area_collector(Config) when is_list(Config) ->
    literal_area_collector_test:check_idle(10000).

%%
%% Internal functions...
%%


display_check_io(ChkIo) ->
    catch erlang:display('--- CHECK IO INFO ---'),
    catch erlang:display(ChkIo),
    catch erts_debug:set_internal_state(available_internal_state, true),
    NoOfErrorFds = (catch element(1, erts_debug:get_internal_state(check_io_debug))),
    catch erlang:display({'NoOfErrorFds', NoOfErrorFds}),
    catch erts_debug:set_internal_state(available_internal_state, false),
    catch erlang:display('--- CHECK IO INFO ---'),
    ok.

get_check_io_info() ->
    ChkIo = driver_SUITE:get_check_io_total(erlang:system_info(check_io)),
    PendUpdNo = case lists:keysearch(pending_updates, 1, ChkIo) of
		    {value, {pending_updates, PendNo}} ->
			PendNo;
		    false ->
			0
		end,
    {value, {active_fds, ActFds}} = lists:keysearch(active_fds, 1, ChkIo),
    case {PendUpdNo, ActFds} of
	{0, 0} ->
	    display_check_io(ChkIo),
	    ChkIo;
	_ ->
	    receive after 100 -> ok end,
	    get_check_io_info()
    end.
