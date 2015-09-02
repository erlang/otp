%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(trace_nif_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).
-export([trace_nif/1,
	 trace_nif_timestamp/1,
	 trace_nif_local/1,
	 trace_nif_meta/1,
	 trace_nif_timestamp_local/1,
	 trace_nif_return/1,
	 not_run/1]).

-export([nif_process/0, nif/0, nif/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:is_native(trace_nif_SUITE) of
	true -> [not_run];
	false ->
	    [trace_nif, trace_nif_timestamp, trace_nif_local,
	     trace_nif_meta, trace_nif_timestamp_local,
	     trace_nif_return]
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

trace_nif(doc) -> "Test tracing NIFs.";
trace_nif(Config) when is_list(Config) ->
    load_nif(Config),
    
    do_trace_nif([]).

trace_nif_local(doc) -> "Test tracing NIFs with local flag.";
trace_nif_local(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif([local]).

trace_nif_meta(doc) -> "Test tracing NIFs with meta flag.";
trace_nif_meta(Config) when is_list(Config) ->
    load_nif(Config),
    ?line Pid=spawn_link(?MODULE, nif_process, []),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, [], [meta]),
    
    ?line Pid ! {apply_nif, nif, []},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),
    
    ?line Pid ! {apply_nif, nif, ["Arg1"]},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, ["Arg1"]}}),
    
    ?line Pid ! {call_nif, nif, []},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, []}}),
    
    ?line Pid ! {call_nif, nif, ["Arg1"]},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, ["Arg1"]}}),
    ok.
do_trace_nif(Flags) ->
    ?line Pid = spawn(?MODULE, nif_process, []),
    ?line 1 = erlang:trace(Pid, true, [call]),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),
    ?line Pid ! {apply_nif, nif, []},
    ?line receive_trace_msg({trace,Pid,call,{?MODULE,nif, []}}),
    ?line Pid ! {apply_nif, nif, ["Arg1"]},
    ?line receive_trace_msg({trace,Pid,call,{?MODULE,nif, ["Arg1"]}}),

    ?line Pid ! {call_nif, nif, []},
    ?line receive_trace_msg({trace, Pid, call, {?MODULE,nif, []}}),

    ?line Pid ! {call_nif, nif, ["Arg1"]},
    ?line receive_trace_msg({trace, Pid, call, {?MODULE,nif, ["Arg1"]}}),

    
    %% Switch off
    ?line 1 = erlang:trace(Pid, false, [call]),

    ?line Pid ! {apply_nif, nif, []},
    receive_nothing(),
    ?line Pid ! {apply_nif, nif, ["Arg1"]},
    receive_nothing(),
    ?line Pid ! {call_nif, nif, []},
    receive_nothing(),
    ?line Pid ! {call_nif, nif, ["Arg1"]},
    receive_nothing(),

    %% Switch on again
    ?line 1 = erlang:trace(Pid, true, [call]),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),
    ?line Pid ! {apply_nif, nif, []},
    ?line receive_trace_msg({trace,Pid,call,{?MODULE,nif, []}}),
    ?line Pid ! {apply_nif, nif, ["Arg1"]},
    ?line receive_trace_msg({trace,Pid,call,{?MODULE,nif, ["Arg1"]}}),

    ?line Pid ! {call_nif, nif, []},
    ?line receive_trace_msg({trace, Pid, call, {?MODULE,nif, []}}),

    ?line Pid ! {call_nif, nif, ["Arg1"]},
    ?line receive_trace_msg({trace, Pid, call, {?MODULE,nif, ["Arg1"]}}),
    
    ?line 1 = erlang:trace(Pid, false, [call]),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, false, Flags),
    ?line exit(Pid, die),
    ok.

trace_nif_timestamp(doc) -> "Test tracing NIFs with timestamps.";
trace_nif_timestamp(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif_timestamp([]).

trace_nif_timestamp_local(doc) -> 
    "Test tracing NIFs with timestamps and local flag.";
trace_nif_timestamp_local(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif_timestamp([local]).

do_trace_nif_timestamp(Flags) ->
    ?line Pid=spawn(?MODULE, nif_process, []),
    ?line 1 = erlang:trace(Pid, true, [call,timestamp]),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),
    
    ?line Pid ! {apply_nif, nif, []},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),
    
    ?line Pid ! {apply_nif, nif, ["Arg1"]},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, ["Arg1"]}}),
    
    ?line Pid ! {call_nif, nif, []},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, []}}),
    
    ?line Pid ! {call_nif, nif, ["Arg1"]},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, ["Arg1"]}}),
    
    %% We should be able to turn off the timestamp.
    ?line 1 = erlang:trace(Pid, false, [timestamp]),
    
    ?line Pid ! {call_nif, nif, []},
    ?line receive_trace_msg({trace,Pid,call,
 			     {?MODULE,nif, []}}),
    
    ?line Pid ! {apply_nif, nif, ["tjoho"]},
    ?line receive_trace_msg({trace,Pid,call,
 			     {?MODULE,nif, ["tjoho"]}}),
    
    ?line 1 = erlang:trace(Pid, false, [call]),
    ?line erlang:trace_pattern({erlang,'_','_'}, false, Flags),
    
    ?line exit(Pid, die),
    ok.

trace_nif_return(doc) -> 
    "Test tracing NIF's with return/return_to trace.";
trace_nif_return(Config) when is_list(Config) ->
    load_nif(Config),

    ?line Pid=spawn(?MODULE, nif_process, []),
    ?line 1 = erlang:trace(Pid, true, [call,timestamp,return_to]),
    ?line erlang:trace_pattern({?MODULE,nif,'_'}, [{'_',[],[{return_trace}]}], 
 			       [local]),

    ?line Pid ! {apply_nif, nif, []},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),
    ?line receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
 					    {?MODULE,nif,0}}),
    ?line receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
 					  {?MODULE, nif_process,0}}),
        
    ?line Pid ! {call_nif, nif, ["Arg1"]},
    ?line receive_trace_msg_ts({trace_ts,Pid,call,
				{?MODULE,nif, ["Arg1"]}}),
    ?line receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
					    {?MODULE,nif,1}}),
    ?line receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
 					  {?MODULE, nif_process,0}}),
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

receive_nothing() ->
    ?line timeout = receive M -> M after 100 -> timeout end.

receive_trace_msg_ts({trace_ts, Pid, call, {M,F,A}}) ->
    receive
 	{trace_ts, Pid, call, {M, F, A}, _Ts} ->
 	    ok;
 	Other ->
 	    io:format("Expected: {trace, ~p, call, {~p, ~p, ~p}, TimeStamp}},~n"
 		      "Got: ~p~n",
 		      [Pid, M, F, A, Other]),
 	    ?t:fail()
    after 5000 ->
 	    io:format("Got timeout~n", []),
 	    ?t:fail()
    end.

receive_trace_msg_ts_return_from({trace_ts, Pid, return_from, {M,F,A}}) ->
    receive
 	{trace_ts, Pid, return_from, {M, F, A}, _Value, _Ts} ->
 	    ok;
 	Other ->
 	    io:format("Expected: {trace_ts, ~p, return_from, {~p, ~p, ~p}, Value, TimeStamp}},~n"
 		      "Got: ~p~n",
 		      [Pid, M, F, A, Other]),
 	    ?t:fail()
    after 5000 ->
 	    io:format("Got timeout~n", []),
 	    ?t:fail()
    end.

receive_trace_msg_ts_return_to({trace_ts, Pid, return_to, {M,F,A}}) ->
    receive
 	{trace_ts, Pid, return_to, {M, F, A}, _Ts} ->
 	    ok;
 	Other ->
 	    io:format("Expected: {trace_ts, ~p, return_to, {~p, ~p, ~p}, TimeStamp}},~n"
 		      "Got: ~p~n",
 		      [Pid, M, F, A, Other]),
 	    ?t:fail()
    after 5000 ->
 	    io:format("Got timeout~n", []),
 	    ?t:fail()
    end.

nif_process() ->
    receive
	{apply_nif, Name, Args} ->
	    ?line {ok,Args} = apply(?MODULE, Name, Args);

	{call_nif, Name, []} ->
	    ?line {ok, []} = ?MODULE:Name();
	
	{call_nif, Name, [A1]} ->
	    ?line {ok, [A1]} = ?MODULE:Name(A1);
	
	{call_nif, Name, [A1,A2]} ->
	    ?line {ok,[A1,A2]} = ?MODULE:Name(A1,A2);
	
	{call_nif, Name, [A1,A2,A3]} ->
	    ?line {ok,[A1,A2,A3]} = ?MODULE:Name(A1,A2,A3)    
    end,
    nif_process().    

load_nif(Config) ->    
    ?line Path = ?config(data_dir, Config),
    
    ?line ok = erlang:load_nif(filename:join(Path,"trace_nif"), 0).


nif() ->
    {"Stub0",[]}. %exit("nif/0 stub called").

nif(A1) ->
    {"Stub1",[A1]}. %exit(["nif/1 stub called",A1]).

