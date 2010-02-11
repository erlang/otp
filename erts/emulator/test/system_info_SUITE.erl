%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
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
%%% File    : system_info_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Misc tests of erlang:system_info/1
%%%
%%% Created : 15 Jul 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(system_info_SUITE).
-author('rickard.s.green@ericsson.com').

%-define(line_trace, 1).

-include("test_server.hrl").

%-compile(export_all).
-export([all/1, init_per_testcase/2, fin_per_testcase/2]).

-export([process_count/1, system_version/1, misc_smoke_tests/1, heap_size/1, wordsize/1]).

-define(DEFAULT_TIMEOUT, ?t:minutes(2)).

all(doc) -> [];
all(suite) -> [process_count, system_version, misc_smoke_tests, heap_size, wordsize].

init_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) when is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

%%%
%%% The test cases -------------------------------------------------------------
%%%

process_count(doc) -> [];
process_count(suite) -> [];
process_count(Config) when is_list(Config) ->
    case catch erlang:system_info(modified_timing_level) of
	Level when is_integer(Level) ->
	    {skipped,
	     "Modified timing (level " ++ integer_to_list(Level)
	     ++ ") is enabled. spawn() is too slow for this "
	     " test when modified timing is enabled."};
	_ ->
	    process_count_test()
    end.

process_count_test() ->
    ?line OldPrio = process_flag(priority, max),
    ?line check_procs(10),
    ?line check_procs(11234),
    ?line check_procs(57),
    ?line check_procs(1030),
    ?line check_procs(687),
    ?line check_procs(7923),
    ?line check_procs(5302),
    ?line check_procs(12456),
    ?line check_procs(14),
    ?line check_procs(1125),
    ?line check_procs(236),
    ?line check_procs(125),
    ?line check_procs(2346),
    ?line process_flag(priority, OldPrio),
    ?line ok.
    

check_procs(N) ->
    ?line CP = length(processes()),
    ?line Procs = start_procs(N),
    ?line check_pc(CP+N),
    ?line stop_procs(Procs),
    ?line check_pc(CP).

check_pc(E) ->
    ?line P = length(processes()),
    ?line SI = erlang:system_info(process_count),
    ?line ?t:format("E=~p; P=~p; SI=~p~n", [E, P, SI]),
    ?line E = P,
    ?line P = SI.

start_procs(N) ->
    lists:map(fun (_) ->
		      P = spawn_opt(fun () ->
					    receive after infinity -> bye end
				    end,
				    [{priority, max}]),
		      {P, erlang:monitor(process, P)}
	      end,
	      lists:seq(1, N)).

stop_procs(PMs) ->
    lists:foreach(fun ({P, _}) ->
			  exit(P, boom)
		  end, PMs),
    lists:foreach(fun ({P, M}) ->
			  receive {'DOWN', M, process, P, boom} -> ok end
		  end, PMs).


system_version(doc) -> [];
system_version(suite) -> [];
system_version(Config) when is_list(Config) ->
    ?line {comment, erlang:system_info(system_version)}.

misc_smoke_tests(doc) -> [];
misc_smoke_tests(suite) -> [];
misc_smoke_tests(Config) when is_list(Config) ->
    ?line true = is_binary(erlang:system_info(info)),
    ?line true = is_binary(erlang:system_info(procs)),
    ?line true = is_binary(erlang:system_info(loaded)),
    ?line true = is_binary(erlang:system_info(dist)),
    ?line ok.
    

heap_size(doc) -> [];
heap_size(suite) -> [];
heap_size(Config) when is_list(Config) ->
   ?line {min_bin_vheap_size, VHmin} = erlang:system_info(min_bin_vheap_size),
   ?line {min_heap_size, Hmin} =  erlang:system_info(min_heap_size),
   ?line GCinf =  erlang:system_info(garbage_collection),
   ?line VHmin = proplists:get_value(min_bin_vheap_size, GCinf),
   ?line Hmin  = proplists:get_value(min_heap_size, GCinf),
   ok.

wordsize(suite) ->
    [];
wordsize(doc) ->
    ["Tests the various wordsize variants"];
wordsize(Config) when is_list(Config) ->
    ?line A = erlang:system_info(wordsize),
    ?line true = is_integer(A),
    ?line A = erlang:system_info({wordsize,internal}),
    ?line B = erlang:system_info({wordsize,external}),
    ?line true = A =< B,
    case {B,A} of
	{4,4} ->
	    {comment, "True 32-bit emulator"};
	{8,8} ->
	    {comment, "True 64-bit emulator"};
	{8,4} ->
	    {comment, "Halfword 64-bit emulator"};
	Other ->
	    exit({unexpected_wordsizes,Other})
    end.
