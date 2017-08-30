%% -*- erlang-indent-level: 2 -*-
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_timing.erl
%%% Authors : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Timing reports for Dialyzer
%%%-------------------------------------------------------------------

-module(dialyzer_timing).

-export([init/1, start_stamp/2, send_size_info/3, end_stamp/1, stop/1]).

-export_type([timing_server/0]).

-type timing_server() :: pid() | 'none'.

-spec init(boolean() | 'debug') -> timing_server().

init(Active) ->
  case Active of
    true ->
      io:format("\n"),
      spawn_link(fun() -> loop(erlang:monotonic_time(), 0, "") end);
    debug ->
      io:format("\n"),
      spawn_link(fun() -> debug_loop("") end);
    false -> none
  end.

loop(LastNow, Size, Unit) ->
  receive
    {stamp, Msg, Now} ->
      io:format("    ~-10s (+~4.2fs):", [Msg, diff(Now, LastNow)]),
      loop(Now, 0, "");
    {stamp, Now} ->
      SizeStr =
	case Size of
	  0 -> "";
	  _ ->
	    Data = io_lib:format("~p ~s",[Size, Unit]),
	    io_lib:format(" (~12s)",[Data])
	end,
      io:format("~7.2fs~s\n", [diff(Now, LastNow), SizeStr]),
      loop(Now, 0, "");
    {size, NewSize, NewUnit} ->
      loop(LastNow, NewSize, NewUnit);
    {Pid, stop, Now} ->
      io:format("    ~-9s (+~5.2fs)\n", ["",diff(Now, LastNow)]),
      Pid ! ok;
    {Pid, stop} ->
      Pid ! ok
  end.

debug_loop(Phase) ->
  receive
    Message ->
      {Runtime,_} = statistics(wall_clock),
      Procs = erlang:system_info(process_count),
      ProcMem = erlang:memory(total),
      Status = io_lib:format("~12w ~6w ~20w", [Runtime, Procs, ProcMem]),
      case Message of
	{stamp, Msg, _Now} ->
	  io:format("~s ~s_start\n", [Status, Msg]),
	  debug_loop(Msg);
	{stamp, _Now} ->
	  io:format("~s ~s_stop\n", [Status, Phase]),
	  debug_loop("");
	{Pid, stop, _Now} ->
	  Pid ! ok;
	{Pid, stop} ->
	  Pid ! ok;
	_ ->
	  debug_loop(Phase)
      end
  after
    50 ->
      {Runtime,_} = statistics(wall_clock),
      Procs = erlang:system_info(process_count),
      ProcMem = erlang:memory(total),
      Status = io_lib:format("~12w ~6w ~20w", [Runtime, Procs, ProcMem]),
      io:format("~s\n", [Status]),
      debug_loop(Phase)
  end.


-spec start_stamp(timing_server(), string()) -> ok.

start_stamp(none, _) -> ok;
start_stamp(Pid, Msg) ->
  Pid ! {stamp, Msg, erlang:monotonic_time()},
  ok.

-spec end_stamp(timing_server()) -> ok.

end_stamp(none) -> ok;
end_stamp(Pid) ->
  Pid ! {stamp, erlang:monotonic_time()},
  ok.

-spec send_size_info(timing_server(), integer(), string()) -> ok.

send_size_info(none, _, _) -> ok;
send_size_info(Pid, Size, Unit) ->
  Pid ! {size, Size, Unit},
  ok.

-spec stop(timing_server()) -> ok.

stop(none) -> ok;
stop(Pid) ->
  Pid ! {self(), stop, erlang:monotonic_time()},
  receive ok -> ok end.

diff(T2, T1) ->
  (T2-T1) / erlang:convert_time_unit(1, seconds, native).
