%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
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

not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

%% Test tracing NIFs.
trace_nif(Config) when is_list(Config) ->
    load_nif(Config),

    do_trace_nif([]).

%% Test tracing NIFs with local flag.
trace_nif_local(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif([local]).

%% Test tracing NIFs with meta flag.
trace_nif_meta(Config) when is_list(Config) ->
    load_nif(Config),
    Pid=spawn_link(?MODULE, nif_process, []),
    erlang:trace_pattern({?MODULE,nif,'_'}, [], [meta]),

    Pid ! {apply_nif, nif, []},
    receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),

    Pid ! {apply_nif, nif, ["Arg1"]},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, ["Arg1"]}}),

    Pid ! {call_nif, nif, []},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, []}}),

    Pid ! {call_nif, nif, ["Arg1"]},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, ["Arg1"]}}),
    ok.
do_trace_nif(Flags) ->
    Pid = spawn_link(?MODULE, nif_process, []),
    1 = erlang:trace(Pid, true, [call]),
    erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),
    Pid ! {apply_nif, nif, []},
    receive_trace_msg({trace,Pid,call,{?MODULE,nif, []}}),
    Pid ! {apply_nif, nif, ["Arg1"]},
    receive_trace_msg({trace,Pid,call,{?MODULE,nif, ["Arg1"]}}),

    Pid ! {call_nif, nif, []},
    receive_trace_msg({trace, Pid, call, {?MODULE,nif, []}}),

    Pid ! {call_nif, nif, ["Arg1"]},
    receive_trace_msg({trace, Pid, call, {?MODULE,nif, ["Arg1"]}}),


    %% Switch off
    1 = erlang:trace(Pid, false, [call]),

    Pid ! {apply_nif, nif, []},
    receive_nothing(),
    Pid ! {apply_nif, nif, ["Arg1"]},
    receive_nothing(),
    Pid ! {call_nif, nif, []},
    receive_nothing(),
    Pid ! {call_nif, nif, ["Arg1"]},
    receive_nothing(),

    %% Switch on again
    1 = erlang:trace(Pid, true, [call]),
    erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),
    Pid ! {apply_nif, nif, []},
    receive_trace_msg({trace,Pid,call,{?MODULE,nif, []}}),
    Pid ! {apply_nif, nif, ["Arg1"]},
    receive_trace_msg({trace,Pid,call,{?MODULE,nif, ["Arg1"]}}),

    Pid ! {call_nif, nif, []},
    receive_trace_msg({trace, Pid, call, {?MODULE,nif, []}}),

    Pid ! {call_nif, nif, ["Arg1"]},
    receive_trace_msg({trace, Pid, call, {?MODULE,nif, ["Arg1"]}}),

    1 = erlang:trace(Pid, false, [call]),
    erlang:trace_pattern({?MODULE,nif,'_'}, false, Flags),

    unlink(Pid),
    exit(Pid, die),
    ok.

%% Test tracing NIFs with timestamps.
trace_nif_timestamp(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif_timestamp([]).

%% Test tracing NIFs with timestamps and local flag.
trace_nif_timestamp_local(Config) when is_list(Config) ->
    load_nif(Config),
    do_trace_nif_timestamp([local]).

do_trace_nif_timestamp(Flags) ->
    Pid = spawn_link(?MODULE, nif_process, []),
    1 = erlang:trace(Pid, true, [call,timestamp]),
    erlang:trace_pattern({?MODULE,nif,'_'}, [], Flags),

    Pid ! {apply_nif, nif, []},
    receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),

    Pid ! {apply_nif, nif, ["Arg1"]},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, ["Arg1"]}}),

    Pid ! {call_nif, nif, []},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, []}}),

    Pid ! {call_nif, nif, ["Arg1"]},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, ["Arg1"]}}),

    %% We should be able to turn off the timestamp.
    1 = erlang:trace(Pid, false, [timestamp]),

    Pid ! {call_nif, nif, []},
    receive_trace_msg({trace,Pid,call,
                       {?MODULE,nif, []}}),

    Pid ! {apply_nif, nif, ["tjoho"]},
    receive_trace_msg({trace,Pid,call,
                       {?MODULE,nif, ["tjoho"]}}),

    1 = erlang:trace(Pid, false, [call]),
    erlang:trace_pattern({erlang,'_','_'}, false, Flags),

    unlink(Pid),
    exit(Pid, die),
    ok.

%% Test tracing NIF's with return/return_to trace.
trace_nif_return(Config) when is_list(Config) ->
    load_nif(Config),

    Pid = spawn_link(?MODULE, nif_process, []),
    1 = erlang:trace(Pid, true, [call,timestamp,return_to]),
    erlang:trace_pattern({?MODULE,nif,'_'}, [{'_',[],[{return_trace}]}], 
                         [local]),

    Pid ! {apply_nif, nif, []},
    receive_trace_msg_ts({trace_ts,Pid,call,{?MODULE,nif,[]}}),
    receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
                                      {?MODULE,nif,0}}),
    receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
                                    {?MODULE, nif_process,0}}),

    Pid ! {call_nif, nif, ["Arg1"]},
    receive_trace_msg_ts({trace_ts,Pid,call,
                          {?MODULE,nif, ["Arg1"]}}),
    receive_trace_msg_ts_return_from({trace_ts,Pid,return_from,
                                      {?MODULE,nif,1}}),
    receive_trace_msg_ts_return_to({trace_ts,Pid,return_to, 
                                    {?MODULE, nif_process,0}}),
    ok.


receive_trace_msg(Mess) ->
    receive
        Mess ->
            ok;
        Other ->
            ct:fail("Expected: ~p,~nGot: ~p~n", [Mess, Other])
    after 5000 ->
              ct:fail("Expected: ~p,~nGot: timeout~n", [Mess])
    end.

receive_nothing() ->
    timeout = receive M -> M after 100 -> timeout end.

receive_trace_msg_ts({trace_ts, Pid, call, {M,F,A}}) ->
    receive
        {trace_ts, Pid, call, {M, F, A}, _Ts} ->
            ok;
        Other ->
            ct:fail("Expected: {trace, ~p, call, {~p, ~p, ~p}, TimeStamp}},~n"
                    "Got: ~p~n", [Pid, M, F, A, Other])
    after 5000 ->
              ct:fail("Got timeout~n", [])
    end.

receive_trace_msg_ts_return_from({trace_ts, Pid, return_from, {M,F,A}}) ->
    receive
        {trace_ts, Pid, return_from, {M, F, A}, _Value, _Ts} ->
            ok;
        Other ->
            ct:fail("Expected: {trace_ts, ~p, return_from, {~p, ~p, ~p}, Value, TimeStamp}},~n"
                    "Got: ~p~n", [Pid, M, F, A, Other])
    after 5000 ->
              ct:fail("Got timeout~n", [])
    end.

receive_trace_msg_ts_return_to({trace_ts, Pid, return_to, {M,F,A}}) ->
    receive
        {trace_ts, Pid, return_to, {M, F, A}, _Ts} ->
            ok;
        Other ->
            ct:fail("Expected: {trace_ts, ~p, return_to, {~p, ~p, ~p}, TimeStamp}},~n"
                    "Got: ~p~n", [Pid, M, F, A, Other])
    after 5000 ->
              ct:fail("Got timeout~n", [])
    end.

nif_process() ->
    receive
        {apply_nif, Name, Args} ->
            {ok,Args} = apply(?MODULE, Name, Args);

        {call_nif, Name, []} ->
            {ok, []} = ?MODULE:Name();

        {call_nif, Name, [A1]} ->
            {ok, [A1]} = ?MODULE:Name(A1);

        {call_nif, Name, [A1,A2]} ->
            {ok,[A1,A2]} = ?MODULE:Name(A1,A2);

        {call_nif, Name, [A1,A2,A3]} ->
            {ok,[A1,A2,A3]} = ?MODULE:Name(A1,A2,A3)    
    end,
    nif_process().    

load_nif(Config) ->    
    case is_nif_loaded() of
        true ->
            ok;
        false ->
            Path = proplists:get_value(data_dir, Config),
            ok = erlang:load_nif(filename:join(Path,"trace_nif"), 0)
    end.

is_nif_loaded() ->
    false.

nif() ->
    {"Stub0",[]}. %exit("nif/0 stub called").

nif(A1) ->
    {"Stub1",[A1]}. %exit(["nif/1 stub called",A1]).
