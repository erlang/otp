%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2012. All Rights Reserved.
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
%%% File    : dialyzer_timing.erl
%%% Authors : Stavros Aronis <aronisstav@gmail.com>
%%% Description : Timing reports for Dialyzer
%%%-------------------------------------------------------------------

-module(dialyzer_timing).

-export([init/1, start_stamp/2, send_size_info/3, end_stamp/1, stop/1]).

-export_type([timing_server/0]).

-type timing_server() :: pid() | 'none'.

-spec init(boolean()) -> timing_server().

init(Active) ->
  case Active of
    true ->
      io:format("\n"),
      spawn_link(fun() -> loop(now(), 0, "") end);
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

-spec start_stamp(timing_server(), string()) -> ok.

start_stamp(none, _) -> ok;
start_stamp(Pid, Msg) ->
  Pid ! {stamp, Msg, now()},
  ok.

-spec end_stamp(timing_server()) -> ok.

end_stamp(none) -> ok;
end_stamp(Pid) ->
  Pid ! {stamp, now()},
  ok.

-spec send_size_info(timing_server(), integer(), string()) -> ok.

send_size_info(none, _, _) -> ok;
send_size_info(Pid, Size, Unit) ->
  Pid ! {size, Size, Unit},
  ok.

-spec stop(timing_server()) -> ok.

stop(none) -> ok;
stop(Pid) ->
  Pid ! {self(), stop, now()},
  receive ok -> ok end.

diff(T2, T1) ->
  timer:now_diff(T2,T1) / 1000000.
