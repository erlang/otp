%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

-module(esock_iow_lib).

-export([
         format_counters/1, format_counters/2, format_counters/3,
         iprint/1, iprint/2,
         eprint/1, eprint/2,
         f/2,
         fts/0, fts/1
        ]).


%% ---------------------------------------------------------------------

format_counters(Counters) ->
    format_counters(traffic, Counters).

format_counters(Type, Counters) when (Type =:= listen) orelse (Type =:= traffic) ->
    format_counters("   ", Type, Counters).

format_counters(Prefix, traffic, Counters) ->
    ReadByte    = proplists:get_value(read_byte,     Counters, -1),
    ReadFails   = proplists:get_value(read_fails,    Counters, -1),
    ReadPkg     = proplists:get_value(read_pkg,      Counters, -1),
    ReadPkgMax  = proplists:get_value(read_pkg_max,  Counters, -1),
    ReadTries   = proplists:get_value(read_tries,    Counters, -1),
    ReadWaits   = proplists:get_value(read_waits,    Counters, -1),
    WriteByte   = proplists:get_value(write_byte,    Counters, -1),
    WriteFails  = proplists:get_value(write_fails,   Counters, -1),
    WritePkg    = proplists:get_value(write_pkg,     Counters, -1),
    WritePkgMax = proplists:get_value(write_pkg_max, Counters, -1),
    WriteTries  = proplists:get_value(write_tries,   Counters, -1),
    WriteWaits  = proplists:get_value(write_waits,   Counters, -1),
    f("~n~sNumber Of Read Bytes:     ~p"
      "~n~sNumber Of Read Fails:     ~p"
      "~n~sNumber Of Read Packages:  ~p"
      "~n~sNumber Of Read Tries:     ~p"
      "~n~sNumber Of Read Waits:     ~p"
      "~n~sMax Read Package Size:    ~p"
      "~n~sNumber Of Write Bytes:    ~p"
      "~n~sNumber Of Write Fails:    ~p"
      "~n~sNumber Of Write Packages: ~p"
      "~n~sNumber Of Write Tries:    ~p"
      "~n~sNumber Of Write Waits:    ~p"
      "~n~sMax Write Package Size:   ~p",
      [Prefix, ReadByte,
       Prefix, ReadFails,
       Prefix, ReadPkg,
       Prefix, ReadTries,
       Prefix, ReadWaits,
       Prefix, ReadPkgMax,
       Prefix, WriteByte,
       Prefix, WriteFails,
       Prefix, WritePkg,
       Prefix, WriteTries,
       Prefix, WriteWaits,
       Prefix, WritePkgMax]);

format_counters(Prefix, listen, Counters) ->
    AccSuccess = proplists:get_value(acc_success, Counters, -1),
    AccFails   = proplists:get_value(acc_fails,   Counters, -1),
    AccTries   = proplists:get_value(acc_tries,   Counters, -1),
    AccWaits   = proplists:get_value(acc_waits,   Counters, -1),
    f("~n~sNumber Of Successful Accepts: ~p"
      "~n~sNumber Of Failed Accepts:     ~p"
      "~n~sNumber Of Accept Attempts:    ~p"
      "~n~sNumber Of Accept Waits:       ~p",
      [Prefix, AccSuccess,
       Prefix, AccFails,
       Prefix, AccTries,
       Prefix, AccWaits]).

%% ---------------------------------------------------------------------

iprint(F) ->
    iprint(F, []).

iprint(F, A) ->
    print("INFO", F, A).

eprint(F) ->
    iprint(F, []).

eprint(F, A) ->
    print("ERROR", F, A).

print(Pre, F, A) ->
    io:format("*** ~s *** ~s ~n" ++ F ++ "~n~n", [Pre, fts() | A]).


f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


ts() ->
    os:timestamp().

fts() ->
    fts(ts()).

fts({_N1, _N2, N3} = TS) ->
    {_Date, Time}  = calendar:now_to_local_time(TS),
    {Hour,Min,Sec} = Time,
    f("~.2.0w:~.2.0w:~.2.0w.4~w", [Hour, Min, Sec, round(N3/1000)]).
