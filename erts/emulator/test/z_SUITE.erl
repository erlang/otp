%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

%-compile(export_all).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, init_per_testcase/2, 
	 end_per_testcase/2]).

-export([schedulers_alive/1, node_container_refc_check/1,
	 long_timers/1, pollset_size/1,
	 check_io_debug/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(5)).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [schedulers_alive, node_container_refc_check,
     long_timers, pollset_size, check_io_debug].

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


init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

schedulers_alive(doc) -> ["Tests that all schedulers are actually used"];
schedulers_alive(suite) -> [];
schedulers_alive(Config) when is_list(Config) ->
    ?line Master = self(),
    ?line NoSchedulersOnline = erlang:system_flag(
				 schedulers_online,
				 erlang:system_info(schedulers)),
    ?line NoSchedulers = erlang:system_info(schedulers),
    UsedScheds =
	try
	    ?line ?t:format("Number of schedulers configured: ~p~n", [NoSchedulers]),
	    ?line case erlang:system_info(multi_scheduling) of
		      blocked ->
			  ?line ?t:fail(multi_scheduling_blocked);
		      disabled ->
			  ?line ok;
		      enabled ->
			  ?t:format("Testing blocking process exit~n"),
			  BF = fun () ->
				       blocked = erlang:system_flag(multi_scheduling,
								    block),
				       Master ! {self(), blocking},
				       receive after infinity -> ok end
			       end,
			  ?line Blocker = spawn_link(BF),
			  ?line Mon = erlang:monitor(process, Blocker),
			  ?line receive {Blocker, blocking} -> ok end,
			  ?line [Blocker]
			      = erlang:system_info(multi_scheduling_blockers),
			  ?line unlink(Blocker),
			  ?line exit(Blocker, kill),
			  ?line receive {'DOWN', Mon, _, _, _} -> ok end,
			  ?line enabled = erlang:system_info(multi_scheduling),
			  ?line [] = erlang:system_info(multi_scheduling_blockers),
			  ?line ok
		  end,
	    ?t:format("Testing blocked~n"),
	    ?line erlang:system_flag(multi_scheduling, block),
	    ?line case erlang:system_info(multi_scheduling) of
		      enabled ->
			  ?line ?t:fail(multi_scheduling_enabled);
		      blocked ->
			  ?line [Master] = erlang:system_info(multi_scheduling_blockers);
		      disabled -> ?line ok
		  end,
	    ?line Ps = lists:map(
			 fun (_) ->
				 spawn_link(fun () ->
						    run_on_schedulers(none,
								      [],
								      Master)
					    end)
			 end,
			 lists:seq(1,NoSchedulers)),
	    ?line receive after 1000 -> ok end,
	    ?line {_, 1} = verify_all_schedulers_used({[],0}, 1),
	    ?line lists:foreach(fun (P) ->
					unlink(P),
					exit(P, bang)
				end,
				Ps),
	    ?line case erlang:system_flag(multi_scheduling, unblock) of
		      blocked -> ?line ?t:fail(multi_scheduling_blocked);
		      disabled -> ?line ok;
		      enabled -> ?line ok
		  end,
	    erts_debug:set_internal_state(available_internal_state, true),
	    %% node_and_dist_references will use emulator interal thread blocking...
	    erts_debug:get_internal_state(node_and_dist_references), 
	    erts_debug:set_internal_state(available_internal_state, false),
	    ?t:format("Testing not blocked~n"),
	    ?line Ps2 = lists:map(
			  fun (_) ->
				  spawn_link(fun () ->
						     run_on_schedulers(none,
								       [],
								       Master)
					     end)
			  end,
			  lists:seq(1,NoSchedulers)),
	    ?line receive after 1000 -> ok end,
	    ?line {_, NoSIDs} = verify_all_schedulers_used({[],0},NoSchedulers),
	    ?line lists:foreach(fun (P) ->
					unlink(P),
					exit(P, bang)
				end,
				Ps2),
	    NoSIDs
	after
	    NoSchedulers = erlang:system_flag(schedulers_online,
					      NoSchedulersOnline),
	    NoSchedulersOnline = erlang:system_info(schedulers_online)
	end,
    ?line {comment, "Number of schedulers " ++ integer_to_list(UsedScheds)}.


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
		    ?t:format("Scheduler ~p used~n", [SID]),
		    {[SID|SIDs], SIDsLen+1}
	    end
    end.

verify_all_schedulers_used({UsedSIDs, UsedSIDsLen} = State, NoSchedulers) ->
    ?line case NoSchedulers of
	      UsedSIDsLen ->
		  ?line State;
	      NoSchdlrs when NoSchdlrs < UsedSIDsLen ->
		  ?line ?t:fail({more_schedulers_used_than_exist,
				 {existing_schedulers, NoSchdlrs},
				 {used_schedulers, UsedSIDsLen},
				 {used_scheduler_ids, UsedSIDs}});
	      _ ->
		  ?line NewState = wait_on_used_scheduler(State),
		  ?line verify_all_schedulers_used(NewState, NoSchedulers)
	  end.

node_container_refc_check(doc) -> [];
node_container_refc_check(suite) -> [];
node_container_refc_check(Config) when is_list(Config) ->
    ?line node_container_SUITE:node_container_refc_check(node()),
    ?line ok.

long_timers(doc) ->
    [];
long_timers(suite) ->
    [];
long_timers(Config) when is_list(Config) ->
    ?line ok = long_timers_test:check_result().

pollset_size(doc) ->
    [];
pollset_size(suite) ->
    [];
pollset_size(Config) when is_list(Config) ->
    ?line Name = pollset_size_testcase_initial_state_holder,
    ?line Mon = erlang:monitor(process, Name),
    ?line (catch Name ! {get_initial_check_io_result, self()}),
    ?line InitChkIo = receive
			  {initial_check_io_result, ICIO} ->
			      ?line erlang:demonitor(Mon, [flush]),
			      ?line ICIO;
			  {'DOWN', Mon, _, _, Reason} ->
			      ?line ?t:fail({non_existing, Name, Reason})
		      end,
    ?line FinChkIo = get_check_io_info(),
    ?line io:format("Initial: ~p~nFinal: ~p~n", [InitChkIo, FinChkIo]),
    ?line InitPollsetSize = lists:keysearch(total_poll_set_size, 1, InitChkIo),
    ?line FinPollsetSize = lists:keysearch(total_poll_set_size, 1, FinChkIo),
    ?line case InitPollsetSize =:= FinPollsetSize of
	      true ->
		  case InitPollsetSize of
		      {value, {total_poll_set_size, Size}} ->
			  ?line {comment,
				 "Pollset size: " ++ integer_to_list(Size)};
		      _ ->
			  ?line {skipped,
				 "Pollset size information not available"}
		  end;
	      false ->
		  %% Somtimes we have fewer descriptors in the
		  %% pollset at the end than when we started, but
		  %% that is ok as long as there are at least 2
		  %% descriptors (dist listen socket and
		  %% epmd socket) in the pollset.
		  ?line {value, {total_poll_set_size, InitSize}}
		      = InitPollsetSize,
		  ?line {value, {total_poll_set_size, FinSize}}
		      = FinPollsetSize,
		  ?line true = FinSize < InitSize,
		  ?line true = 2 =< FinSize,
		  ?line {comment,
			 "Start pollset size: "
			 ++ integer_to_list(InitSize)
			 ++ " End pollset size: "
			 ++ integer_to_list(FinSize)}
	  end.

check_io_debug(doc) ->
    [];
check_io_debug(suite) ->
    [];
check_io_debug(Config) when is_list(Config) ->
    ?line case lists:keysearch(name, 1, erlang:system_info(check_io)) of
	      {value, {name, erts_poll}} -> ?line check_io_debug_test();
	      _ -> ?line {skipped, "Not implemented in this emulator"}
	  end.

check_io_debug_test() ->
    ?line erts_debug:set_internal_state(available_internal_state, true),
    ?line erlang:display(erlang:system_info(check_io)),
    ?line NoOfErrorFds = erts_debug:get_internal_state(check_io_debug),
    ?line erts_debug:set_internal_state(available_internal_state, false),
    ?line 0 = NoOfErrorFds,
    ?line ok.



%%
%% Internal functions...
%%

display_check_io(ChkIo) ->
    catch erlang:display('--- CHECK IO INFO ---'),
    catch erlang:display(ChkIo),
    catch erts_debug:set_internal_state(available_internal_state, true),
    NoOfErrorFds = (catch erts_debug:get_internal_state(check_io_debug)),
    catch erlang:display({'NoOfErrorFds', NoOfErrorFds}),
    catch erts_debug:set_internal_state(available_internal_state, false),
    catch erlang:display('--- CHECK IO INFO ---'),
    ok.

get_check_io_info() ->
    ChkIo = erlang:system_info(check_io),
    case lists:keysearch(pending_updates, 1, ChkIo) of
	{value, {pending_updates, 0}} ->
	    display_check_io(ChkIo),
	    ChkIo;
	false ->
	    ChkIo;
	_ ->
	    receive after 10 -> ok end,
	    get_check_io_info()
    end.



