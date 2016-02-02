%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2014. All Rights Reserved.
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

-module(trace_bif_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([trace_bif/1, trace_bif_timestamp/1, trace_on_and_off/1, 
	 trace_bif_local/1,
	 trace_bif_timestamp_local/1, trace_bif_return/1, not_run/1,
	 trace_info_old_code/1]).

-export([bif_process/0]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:is_native(trace_bif_SUITE) of
	true -> [not_run];
	false ->
	    [trace_bif, trace_bif_timestamp, trace_on_and_off,
	     trace_bif_local, trace_bif_timestamp_local,
	     trace_bif_return, trace_info_old_code]
    end.

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


not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

trace_on_and_off(doc) -> 
    "Tests switching tracing on and off.";
trace_on_and_off(Config) when is_list(Config) ->
    ?line Pid = spawn(?MODULE, bif_process, []),
    ?line Self = self(),
    ?line 1 = erlang:trace(Pid, true, [call,timestamp]),
    ?line {flags, Flags} = erlang:trace_info(Pid,flags),
    ?line [call,timestamp] = lists:sort(Flags),
    ?line {tracer, Self} = erlang:trace_info(Pid,tracer),
    ?line 1 = erlang:trace(Pid, false, [timestamp]),
    ?line {flags,[call]} =  erlang:trace_info(Pid,flags),
    ?line {tracer, Self} = erlang:trace_info(Pid,tracer),
    ?line 1 = erlang:trace(Pid, false, [call]),
    ?line {flags,[]} =  erlang:trace_info(Pid,flags),
    ?line {tracer, []} = erlang:trace_info(Pid,tracer),
    ?line exit(Pid,kill),
    ok.

trace_bif(doc) -> "Test tracing BIFs.";
trace_bif(Config) when is_list(Config) ->
    do_trace_bif([]).

trace_bif_local(doc) -> "Test tracing BIFs with local flag.";
trace_bif_local(Config) when is_list(Config) ->
    do_trace_bif([local]).

do_trace_bif(Flags) ->
    ?line Pid = spawn(?MODULE, bif_process, []),
    ?line 1 = erlang:trace(Pid, true, [call]),
    ?line erlang:trace_pattern({erlang,'_','_'}, [], Flags),
    ?line Pid ! {do_bif, time, []},
    ?line receive_trace_msg({trace,Pid,call,{erlang,time, []}}),
    ?line Pid ! {do_bif, statistics, [runtime]},
    ?line receive_trace_msg({trace,Pid,call,
			     {erlang,statistics, [runtime]}}),

    ?line Pid ! {do_time_bif},
    ?line receive_trace_msg({trace,Pid,call,
			     {erlang,time, []}}),

    ?line Pid ! {do_statistics_bif},
    ?line receive_trace_msg({trace,Pid,call,
			     {erlang,statistics, [runtime]}}),

    ?line 1 = erlang:trace(Pid, false, [call]),
    ?line erlang:trace_pattern({erlang,'_','_'}, false, Flags),
    ?line exit(Pid, die),
    ok.

trace_bif_timestamp(doc) -> "Test tracing BIFs with timestamps.";
trace_bif_timestamp(Config) when is_list(Config) ->
    do_trace_bif_timestamp([], timestamp, [timestamp]),
    do_trace_bif_timestamp([], timestamp,
			   [timestamp,
			    monotonic_timestamp,
			    strict_monotonic_timestamp]),
    do_trace_bif_timestamp([], strict_monotonic_timestamp,
			   [strict_monotonic_timestamp]),
    do_trace_bif_timestamp([], strict_monotonic_timestamp,
			   [monotonic_timestamp, strict_monotonic_timestamp]),
    do_trace_bif_timestamp([], monotonic_timestamp, [monotonic_timestamp]).
    
trace_bif_timestamp_local(doc) -> 
    "Test tracing BIFs with timestamps and local flag.";
trace_bif_timestamp_local(Config) when is_list(Config) ->
    do_trace_bif_timestamp([local], timestamp, [timestamp]),
    do_trace_bif_timestamp([local], timestamp,
			   [timestamp,
			    monotonic_timestamp,
			    strict_monotonic_timestamp]),
    do_trace_bif_timestamp([local], strict_monotonic_timestamp,
			   [strict_monotonic_timestamp]),
    do_trace_bif_timestamp([local], strict_monotonic_timestamp,
			   [monotonic_timestamp, strict_monotonic_timestamp]),
    do_trace_bif_timestamp([local], monotonic_timestamp, [monotonic_timestamp]).

do_trace_bif_timestamp(Flags, TsType, TsFlags) ->
    io:format("Testing with TsType=~p TsFlags=~p~n", [TsType, TsFlags]),
    Pid=spawn(?MODULE, bif_process, []),
    1 = erlang:trace(Pid, true, [call]++TsFlags),
    erlang:trace_pattern({erlang,'_','_'}, [], Flags),

    Ts0 = make_ts(TsType),
    Pid ! {do_bif, time, []},
    Ts1 = receive_trace_msg_ts({trace_ts,Pid,call,{erlang,time,[]}},
			       Ts0,TsType),

    Pid ! {do_bif, statistics, [runtime]},
    Ts2 = receive_trace_msg_ts({trace_ts,Pid,call,
				{erlang,statistics, [runtime]}},
			       Ts1, TsType),

    Pid ! {do_time_bif},
    Ts3 = receive_trace_msg_ts({trace_ts,Pid,call,
				{erlang,time, []}},
			       Ts2, TsType),

    Pid ! {do_statistics_bif},
    Ts4 = receive_trace_msg_ts({trace_ts,Pid,call,
				{erlang,statistics, [runtime]}},
			       Ts3, TsType),

    check_ts(TsType, Ts4, make_ts(TsType)),

    %% We should be able to turn off the timestamp.
    1 = erlang:trace(Pid, false, TsFlags),

    Pid ! {do_statistics_bif},
    receive_trace_msg({trace,Pid,call,
		       {erlang,statistics, [runtime]}}),

    Pid ! {do_bif, statistics, [runtime]},
    receive_trace_msg({trace,Pid,call,
		       {erlang,statistics, [runtime]}}),

    1 = erlang:trace(Pid, false, [call]),
    erlang:trace_pattern({erlang,'_','_'}, false, Flags),

    exit(Pid, die),
    ok.

trace_bif_return(doc) -> 
    "Test tracing BIF's with return/return_to trace.";
trace_bif_return(Config) when is_list(Config) ->
    do_trace_bif_return(timestamp, [timestamp]),
    do_trace_bif_return(timestamp,
			[timestamp,
			 monotonic_timestamp,
			 strict_monotonic_timestamp]),
    do_trace_bif_return(strict_monotonic_timestamp,
			[strict_monotonic_timestamp]),
    do_trace_bif_return(strict_monotonic_timestamp,
			[monotonic_timestamp, strict_monotonic_timestamp]),
    do_trace_bif_return(monotonic_timestamp, [monotonic_timestamp]).

do_trace_bif_return(TsType, TsFlags) ->
    io:format("Testing with TsType=~p TsFlags=~p~n", [TsType, TsFlags]),
    Pid=spawn(?MODULE, bif_process, []),
    1 = erlang:trace(Pid, true, [call,return_to]++TsFlags),
    erlang:trace_pattern({erlang,'_','_'}, [{'_',[],[{return_trace}]}], 
			 [local]),

    Ts0 = make_ts(TsType),
    Pid ! {do_bif, time, []},
    Ts1 = receive_trace_msg_ts({trace_ts,Pid,call,{erlang,time,[]}},
			       Ts0, TsType),
    Ts2 = receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
					    {erlang,time,0}},
					   Ts1, TsType),
    Ts3 = receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
					  {?MODULE, bif_process,0}},
					 Ts2, TsType),
    
    
    Pid ! {do_bif, statistics, [runtime]},
    Ts4 = receive_trace_msg_ts({trace_ts,Pid,call,
				{erlang,statistics, [runtime]}},
			       Ts3, TsType),
    Ts5 = receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
					    {erlang,statistics,1}},
					   Ts4, TsType),
    Ts6 = receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
					  {?MODULE, bif_process,0}},
					 Ts5, TsType),
    
    
    Pid ! {do_time_bif},
    Ts7 = receive_trace_msg_ts({trace_ts,Pid,call,
				{erlang,time, []}},
			       Ts6, TsType),
    Ts8 = receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
					    {erlang,time,0}},
					   Ts7, TsType),
    Ts9 = receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
					  {?MODULE, bif_process,0}},
					 Ts8, TsType),



    Pid ! {do_statistics_bif},
    Ts10 = receive_trace_msg_ts({trace_ts,Pid,call,
				 {erlang,statistics, [runtime]}},
				Ts9, TsType),
    Ts11 = receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
					     {erlang,statistics,1}},
					    Ts10, TsType),
    Ts12 = receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
					   {?MODULE, bif_process,0}},
					  Ts11, TsType),
    check_ts(TsType, Ts12, make_ts(TsType)),
    ok.
    
    
receive_trace_msg(Mess) ->
    receive
	Mess ->
	    ok;
	Other ->
	    io:format("Expected: ~p,~nGot: ~p~n", [Mess, Other]),
	    ?t:fail()
    after 5000 ->
	    io:format("Expected: ~p,~nGot: timeout~n", [Mess]),
	    ?t:fail()
    end.

receive_trace_msg_ts({trace_ts, Pid, call, {erlang,F,A}}, PrevTs, TsType) ->
    receive
	{trace_ts, Pid, call, {erlang, F, A}, Ts} ->
	    check_ts(TsType, PrevTs, Ts),
	    Ts;
	Other ->
	    io:format("Expected: {trace, ~p, call, {~p, ~p, ~p}, TimeStamp}},~n"
		      "Got: ~p~n",
		      [Pid, erlang, F, A, Other]),
	    ?t:fail()
	after 5000 ->
	    io:format("Got timeout~n", []),
	    ?t:fail()
    end.

receive_trace_msg_ts_return_from({trace_ts, Pid, return_from, {erlang,F,A}}, PrevTs, TsType) ->
    receive
	{trace_ts, Pid, return_from, {erlang, F, A}, _Value, Ts} ->
	    check_ts(TsType, PrevTs, Ts),
	    Ts;
	Other ->
	    io:format("Expected: {trace_ts, ~p, return_from, {~p, ~p, ~p}, Value, TimeStamp}},~n"
		      "Got: ~p~n",
		      [Pid, erlang, F, A, Other]),
	    ?t:fail()
	after 5000 ->
	    io:format("Got timeout~n", []),
	    ?t:fail()
    end.

receive_trace_msg_ts_return_to({trace_ts, Pid, return_to, {M,F,A}}, PrevTs, TsType) ->
    receive
	{trace_ts, Pid, return_to, {M, F, A}, Ts} ->
	    check_ts(TsType, PrevTs, Ts),
	    Ts;
	Other ->
	    io:format("Expected: {trace_ts, ~p, return_to, {~p, ~p, ~p}, TimeStamp}},~n"
		      "Got: ~p~n",
		      [Pid, M, F, A, Other]),
	    ?t:fail()
	after 5000 ->
	    io:format("Got timeout~n", []),
	    ?t:fail()
    end.

make_ts(timestamp) ->
    erlang:now();
make_ts(monotonic_timestamp) ->
    erlang:monotonic_time(nano_seconds);
make_ts(strict_monotonic_timestamp) ->
    MT = erlang:monotonic_time(nano_seconds),
    UMI = erlang:unique_integer([monotonic]),
    {MT, UMI}.

check_ts(timestamp, PrevTs, Ts) ->
    {Ms, S, Us} = Ts,
    true = is_integer(Ms),
    true = is_integer(S),
    true = is_integer(Us),
    true = PrevTs < Ts,
    Ts;
check_ts(monotonic_timestamp, PrevTs, Ts) ->
    true = is_integer(Ts),
    true = PrevTs =< Ts,
    Ts;
check_ts(strict_monotonic_timestamp, PrevTs, Ts) ->
    {MT, UMI} = Ts,
    true = is_integer(MT),
    true = is_integer(UMI),
    true = PrevTs < Ts,
    Ts.

bif_process() ->
    receive
	{do_bif, Name, Args} ->
	    apply(erlang, Name, Args),
	    bif_process();
	{do_time_bif} ->
	    %% Match the return value to ensure that the time() call
	    %% is not optimized away.
	    {_,_,_} = time(),
	    bif_process();
	{do_statistics_bif} ->
	    statistics(runtime),
	    bif_process();
	_Stuff ->
	    bif_process()
    end.
    


trace_info_old_code(doc) -> "trace_info on deleted module (OTP-5057).";
trace_info_old_code(Config) when is_list(Config) ->
    ?line MFA = {M,F,0} = {test,foo,0},
    ?line Fname = atom_to_list(M)++".erl",
    ?line AbsForms = 
	[{attribute,a(1),module,M},                % -module(M).
	 {attribute,a(2),export,[{F,0}]},          % -export([F/0]).
	 {function,a(3),F,0,                       % F() ->
	  [{clause,a(4),[],[],[{atom,a(4),F}]}]}], %     F.
    %%
    ?line {ok,M,Mbin} = compile:forms(AbsForms),
    ?line {module,M} = code:load_binary(M, Fname, Mbin),
    ?line true  = erlang:delete_module(M),
    ?line {traced,undefined} = erlang:trace_info(MFA, traced),
    ok.

a(L) ->
    erl_anno:new(L).
