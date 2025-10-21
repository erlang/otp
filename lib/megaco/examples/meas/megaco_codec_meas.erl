%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Measure megaco codec's encoding & decoding time's
%%
%% Measurement process consists of:
%%   For each message in a directory:
%%     Pre:         Read message from the file, close the file
%%     Measurement: 1) measure decode
%%                  2) measure encode (of the previously decoded message)
%%     Post:        Print average
%%   For each directory: 
%%     A summery is written, both to the console and to a file, 
%%     in an excel compatible format.
%%
%% megaco_codec_meas:t().
%% megaco_codec_meas:t([pretty, compact]).
%% megaco_codec_meas:t([per, pretty, compact]).
%%
%%----------------------------------------------------------------------

-module(megaco_codec_meas).
-moduledoc """
This module implements a simple megaco codec measurement tool.

Results are written to file (excel compatible text files) and on stdout.

_Note_ that this module is _not_ included in the runtime part of the
application.
""".

%% -compile(export_all).


%% API
%% Avoid warning for local function error/2 clashing with autoimported BIF.
-compile({no_auto_import,[error/2]}).
-export([start/0, start/1, start/2]).
-export([start1/0]).

%% Internal exports
-export([do_measure_codec/7, do_measure_codec_loop/7]).
-export([flex_scanner_handler/1]).


-include_lib("kernel/include/file.hrl").

%% We are not mstone but the (mstone) lib module has general
%% functions that we can use.
-define(LIB, megaco_codec_mstone_lib).

-define(V3, v3).

-define(MEASURE_TIMEOUT, 100000). % 100 sec

-ifndef(MEASURE_COUNT_TIME).
-define(MEASURE_COUNT_TIME, 1*1000*1000). % 1 seconds
-endif.

-ifndef(MEASURE_TIME).
-define(MEASURE_TIME, 10000).
-endif.

-ifndef(MEASURE_CODECS).
-define(MEASURE_CODECS, megaco_codec_transform:codecs()).
-endif.

-define(DEFAULT_MESSAGE_PACKAGE, megaco_codec_transform:default_message_package()).

-define(DEFAULT_OPTS, #{verbose => true}).

-define(FTS(), formated_timestamp()).


-record(stat, {name, ecount, etime, dcount, dtime, size}).


%% Runs the measurement on all "official" codecs

-doc false.
start1() ->
    put(everbose,true),
    start().

-doc(#{equiv => start/1}).
start() ->
    meas_init(1, ?DEFAULT_OPTS, ?DEFAULT_MESSAGE_PACKAGE, ?MEASURE_CODECS).

-doc """
start(MessagePackage)

This function runs the measurement on all the _official_ codecs; pretty,
compact, ber, per and erlang.

This function is intended to be called from the _meas_ script, which
uses the '-s' arguments to run the function:

```text
erl -s megaco_codec_meas start time_test
```

""".

-spec start([MessagePackage]) -> ok when
      MessagePackage :: atom();
           (MessagePackage) -> ok when
      MessagePackage :: atom();
           (Factor) -> ok when
      Factor :: pos_integer().

start([MessagePackage]) ->
    do_start(1, ?DEFAULT_OPTS, MessagePackage, ?MEASURE_CODECS);
start(Factor) when is_integer(Factor) andalso (Factor > 0) ->
    do_start(Factor, ?DEFAULT_OPTS, ?DEFAULT_MESSAGE_PACKAGE, ?MEASURE_CODECS);
start(MessagePackage) ->
    do_start(1, ?DEFAULT_OPTS, MessagePackage, ?MEASURE_CODECS).

-doc false.
start(Factor, Opts) when is_integer(Factor) andalso (Factor > 0) andalso
                         is_map(Opts) ->
    do_start(Factor, Opts, ?DEFAULT_MESSAGE_PACKAGE, ?MEASURE_CODECS).

do_start(Factor, Opts, MessagePackageRaw, Codecs) ->
    MessagePackage = parse_message_package(MessagePackageRaw), 
    meas_init(Factor, Opts, MessagePackage, Codecs).
    
parse_message_package(MessagePackageRaw) when is_list(MessagePackageRaw) ->
    list_to_atom(MessagePackageRaw);
parse_message_package(MessagePackage) when is_atom(MessagePackage) ->
    MessagePackage;
parse_message_package(BadMessagePackage) ->
    throw({error, {bad_message_package, BadMessagePackage}}).


%% Dirs is a list of directories containing files,
%% each with a single megaco message. 
%%
%% Note that it is a requirement that each dir has
%% the name of the codec with which the messages has
%% been encoded: 
%%
%%    pretty | compact | ber | per | erlang
%%

meas_init(Factor, Opts, MessagePackage, Codecs) ->
    %% process_flag(trap_exit, true),
    io:format("~nRun meas on message package: ~p~n~n", [MessagePackage]),
    display_os_info(),
    display_system_info(),
    display_app_info(),
    io:format("~n", []),
    Started = os:timestamp(),
    case megaco_codec_transform:messages(MessagePackage) of
	Messages when is_list(Messages) ->
	    ExpandedMessages = expand_messages(Codecs, Messages),
	    Results = t1(Factor, Opts, ExpandedMessages, []), 
	    display_time(Started, os:timestamp()),
            process_results(Opts, Results);
	Error ->
	    Error
    end.

display_os_info() ->
    V = case os:version() of
	    {Major, Minor, Release} ->
		lists:flatten(
		  io_lib:format("~w.~w.~w", [Major, Minor, Release]));
	    Str ->
		Str
	end,
    {OsFam, OsName} = os:type(),
    io:format("OS:                  ~p-~p: ~s~n", [OsFam, OsName, V]).
	    
display_system_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    io:format("System architecture: ~s~n", [SysArch]),
    io:format("System version:      ~s~n", [SysVer]),
    ok.
    
    
display_app_info() ->
    ?LIB:display_app_info().


%% {MegaSec, Sec, MicroSec}
display_time(Start, Fin) ->
    FormatDate1 = format_timestamp(Start),
    FormatDate2 = format_timestamp(Fin),
    FormatDiff  = format_diff(Start, Fin),
    io:format("Started:  ~s~n", [FormatDate1]),
    io:format("Finished: ~s~n", [FormatDate2]),
    io:format("          ~s~n~n~n", [FormatDiff]),
    ok.
    
formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).
  
format_diff(Start, Fin) ->
    DateTime1 = calendar:now_to_universal_time(Start),
    DateTime2 = calendar:now_to_universal_time(Fin),
    T1 = calendar:datetime_to_gregorian_seconds(DateTime1),
    T2 = calendar:datetime_to_gregorian_seconds(DateTime2),
    {_, Diff} = calendar:gregorian_seconds_to_datetime(T2 - T1),
    Tmp = 
	case Diff of
	    {0, 0, S} ->
		io_lib:format("~.2.0w sec", [S]);
	    {0, M, S} ->
		io_lib:format("~w min ~.2.0w sec", [M,S]);
	    {H, M, S} ->
		io_lib:format("~w hour ~w min ~.2.0w sec", [H,M,S])
	end,
    lists:flatten(Tmp).
    
    
			      
t1(_Factor, _Opts, [], Results) ->
    lists:reverse(Results);
t1(Factor, Opts, [{Id, Codec, Conf, _, _} = ECodec|EMsgs], Results) ->
    case (catch measure(Factor, Opts, ECodec)) of
	{'EXIT', Reason} ->
	    error("measure of codec ~p exited: ~n~p", [Codec, Reason]),
	    t1(Factor, Opts, EMsgs, Results);
	{error, Reason} ->
	    error("skipping codec ~p: ~n~p", [Codec, Reason]),
	    t1(Factor, Opts, EMsgs, Results);
	{ok, Res} ->
	    t1(Factor, Opts, EMsgs, [{Id, Conf, Res}| Results])
    end.


measure(Factor, Opts, {Id, Codec, Conf, Count, Msgs}) ->
    io:format("[~s] measure using codec ~p ~p~n ", [?FTS(), Codec, Conf]),
    {Init, Conf1} = measure_init(Conf),
    Conf2 = [{version3,?V3}|Conf1],
    Res = measure(Factor, Opts, Id, Codec, Conf2, Msgs, [], Count),
    measure_fin(Init),
    Res.


expand_messages(Codecs, Messages) ->
    ECodecs = expand_codecs(Codecs, []),
    do_expand_messages(ECodecs, Messages, []).

do_expand_messages([], _, EMessages) ->
    lists:reverse(EMessages);
do_expand_messages([{Id, Codec, Conf, Count} | ECodecs], Messages, EMessages) ->
    case lists:keysearch(Id, 1, Messages) of
	{value, {Id, Msgs}} ->
	    do_expand_messages(ECodecs, Messages, 
                               [{Id, Codec, Conf, Count, Msgs}|EMessages]);
	false ->
	    exit({error, {no_such_codec_data, Id}})
    end.

expand_codecs([], ECodecs) ->
    lists:reverse(lists:flatten(ECodecs));
expand_codecs([Codec|Codecs], ECodecs) when is_atom(Codec) ->
    ECodec = expand_codec(Codec),
    expand_codecs(Codecs, [ECodec|ECodecs]).

%% Make "sure" the factor is evenly divisible by the message count
%% for *all* codec's.
expand_codec(Codec) ->
    case Codec of
	pretty ->
	    [{Codec, megaco_pretty_text_encoder, [flex_scanner], 2000},
	     {Codec, megaco_pretty_text_encoder, [],             1000}];
	compact ->
	    [{Codec, megaco_compact_text_encoder, [flex_scanner], 3000},
	     {Codec, megaco_compact_text_encoder, [],             1500}];
	ber ->
	    [{Codec, megaco_ber_encoder, [native],        3000},
	     {Codec, megaco_ber_encoder, [],              1000}];
	per ->
	    [{Codec, megaco_per_encoder, [native],        3000},
	     {Codec, megaco_per_encoder, [],              1000}];
	erlang ->
	    [
	     {Codec, megaco_erl_dist_encoder, [megaco_compressed,compressed], 500},
	     {Codec, megaco_erl_dist_encoder, [compressed], 400},
	     {Codec, megaco_erl_dist_encoder, [megaco_compressed], 10000},
 	     {Codec, megaco_erl_dist_encoder, [], 10000}
	    ]
    end.


measure_init([flex_scanner]) ->
    start_flex_scanner();
measure_init(Conf) ->
    {undefined, Conf}.


measure_fin(Pid) when is_pid(Pid) ->
    stop_flex_scanner(Pid),
    ok;
measure_fin(_) ->
    ok.


measure(_Factor, _Opts, _Dir, _Codec, _Conf, [], [], _MCount) ->
    {error, no_messages};

measure(_Factor, _Opts, _Dir, _Codec, _Conf, [], Res, _MCount) ->

    {Savg, Eavg, Davg} = process_measure_results(Res),

    io:format("~n[~s] Measurment on ~p messages:"
	      "~n  Average:"
              "~n      Size:   ~w bytes, "
	      "~n      Encode: ~w nanosec, "
	      "~n      Decode: ~w nanosec~n~n", 
	      [?FTS(), length(Res), Savg, Eavg, Davg]),

    {ok, lists:reverse(Res)};

measure(Factor, #{verbose := Verbose} = Opts,
        Dir, Codec, Conf, [{Name, Bin}|Msgs], Results, MCount) ->
    vprint(Verbose, " ~p", [Name]),
    case (catch do_measure(Factor, Opts,
                           Dir, Codec, Conf, Name, Bin, MCount)) of
	{ok, Stat} ->
	    measure(Factor, Opts,
                    Dir, Codec, Conf, Msgs, [Stat | Results], MCount);

	{error, S} ->
            if
                (Verbose =:= true) ->
                    io:format("~n[~s] ~s failed: ~n", [?FTS(), Name]);
                true ->
                    io:format("[~s] ~s failed: ~n", [?FTS(), Name])
            end,
	    error(S,[]),
	    measure(Factor, Opts, Dir, Codec, Conf, Msgs, Results, MCount);

	{info, S} ->
            vprint(Verbose, "~n"),
	    case Verbose orelse get(verbose) of
		true ->
		    info(S,[]);
		_ ->
		    io:format("~s skipped~n", [Name])
	    end,
	    measure(Factor, Opts, Dir, Codec, Conf, Msgs, Results, MCount)

    end.

process_measure_results(Res) when is_list(Res) ->
    Savg = avg([Size       || #stat{size = Size} <- Res]),
    Eavg = avg([Etime/Ecnt || #stat{ecount = Ecnt, etime = Etime} <- Res]),
    Davg = avg([Dtime/Dcnt || #stat{dcount = Dcnt, dtime = Dtime} <- Res]),
    {Savg, Eavg, Davg}.



do_measure(Factor, Opts, _Id, Codec, Conf, Name, BinMsg, MCount) ->
    %% io:format("~n~s~n", [binary_to_list(BinMsg)]),
    {Version, NewBin}  = detect_version(Opts, Codec, Conf, BinMsg),
    {Msg, Dcnt, Dtime} =
        measure_decode(Factor, Codec, Conf, Version, NewBin, MCount),
    {_,   Ecnt, Etime} =
        measure_encode(Factor, Codec, Conf, Version, Msg, MCount),

    {ok, #stat{name   = Name, 
	       ecount = Ecnt, etime = Etime, 
	       dcount = Dcnt, dtime = Dtime, 
	       size = byte_size(NewBin)}}.

detect_version(#{verbose := Verbose} = _Opts, Codec, Conf, Bin) ->
    case (catch Codec:version_of(Conf, Bin)) of
	{ok, V} ->
            vprint(Verbose, "[~w]", [V]),
 	    {ok, M} = Codec:decode_message(Conf, V, Bin),
	    {ok, NewBin} = Codec:encode_message(Conf, V, M),
            vprint(Verbose, "[~w]", [byte_size(NewBin)]),
	    {V, NewBin};
	Error ->
            if
                (Verbose =:= true) ->
                    io:format("~nversion detection failed:~n~p", [Error]);
                true ->
                    io:format("version detection failed:~n~p", [Error])
            end,
	    Error
    end.
	    

measure_decode(Factor, Codec, Conf, Version, Bin, MCount) ->
    case measure_codec(Factor, Codec, decode_message, Conf, Version, Bin, MCount) of
	{ok, Res} ->
	    Res;
	{error, Reason} ->
	    S = format("decode failed for ~p:~n~p", [Codec, Reason]),
	    throw({error, S})
    end.

measure_encode(Factor, Codec, Conf, Version, Bin, MCount) ->
    case measure_codec(Factor, Codec, encode_message, Conf, Version, Bin, MCount) of
	{ok, Res} ->
	    Res;
	{error, Reason} ->
	    S = format("encode failed for ~p:~n~p", [Codec, Reason]),
	    throw({error, S})
    end.


measure_codec(Factor, Codec, Func, Conf, Version, Bin, MCount) 
  when is_integer(Factor) andalso (Factor > 0) andalso
       is_atom(Codec) andalso
       is_atom(Func) andalso
       is_list(Conf) andalso
       is_integer(MCount) andalso (MCount > 0) ->
    {Pid, MRef} =
        spawn_monitor(?MODULE, do_measure_codec, 
                      [Factor, Codec, Func, Conf, Version, Bin, MCount]),
    receive
        {'DOWN', MRef, process, Pid, {measure_result, Res}} ->
	    {ok, Res};
	{'DOWN', MRef, process, Pid, {error, Error}} ->
	    {error, Error};
	{'DOWN', MRef, process, Pid, Else} ->
	    {error, {unexpected_result, Else}}
    after ?MEASURE_TIMEOUT ->
	    Info = 
		case (catch process_info(Pid)) of
		    I when is_list(I) ->
                        erlang:demonitor(MRef),
			exit(Pid, kill),
			I;
		    _ ->
			undefined
		end,
	    {error, {timeout, MCount, Info}}
    end.


-doc false.
do_measure_codec(Factor, Codec, Func, Conf, Version, Bin, MCount) ->
    {ok, Count} = measure_warmup(Codec, Func, Conf, Version, Bin, MCount),
    Count2      = Count div Factor,
    Res = timer:tc(?MODULE, do_measure_codec_loop, 
		   [Codec, Func, Conf, Version, Bin, Count2, dummy],
                   nanosecond),
    case Res of
	{Time, {ok, M}} ->
	    exit({measure_result, {M, Count2, Time}});
	{_Time, Error} ->
            exit({error, Error})
    end.


%% This function does more or less what the real measure function
%% above does. But with the diff:
%% 1) Warmup to ensure that all used code are loaded
%% 2) To aproximate the encoding time, to ensure that 
%%    the real encode is done with enough iterations.
measure_warmup(Codec, Func, Conf, Version, M, MCount) ->
    Res = timer:tc(?MODULE, do_measure_codec_loop, 
		   [Codec, Func, Conf, Version, M, MCount, dummy]),
    case Res of
	{Time, {ok, _}} when is_integer(Time) andalso (Time > 0) ->
	    %% OK so far, now calculate the count:
            %% For some reason we get a 'badarith' on some platforms
            %% here. Since this is just the warmup we can try-catch.
            try round(?MEASURE_COUNT_TIME/(Time/MCount)) of
                Count ->
                    {ok, Count}
            catch
                _:_:_ ->
                    {error, {failed_calculated_count, Time, MCount}}
            end;
	{Time, Error} ->
	    {error, {warmup_failed, Time, Error}}
    end.


-doc false.
do_measure_codec_loop(_Codec, _Func, _Conf, _Version, _Bin, 0, M) ->
    {ok, M};
do_measure_codec_loop(Codec, Func, Conf, Version, Bin, Count, _) ->
    {ok, M} = apply(Codec, Func, [Conf, Version, Bin]),
    do_measure_codec_loop(Codec, Func, Conf, Version, Bin, Count - 1, M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_results(#{bench := true}, Results) ->
    ProcessedResults = process_bench_results(Results),
    {bench, ProcessedResults};
process_results(_, Results) ->
    store_results(Results).

process_bench_results(Results) ->
    process_bench_results(Results, []).

process_bench_results([] = _Results, AccProcessedResults) ->
    lists:reverse(AccProcessedResults);
process_bench_results([{BaseName, Config, Result} | Results],
                      AccProcessedResults) ->
    Name = process_bench_result_name(BaseName, Config),
    ProcessedResult = process_measure_results(Result),
    process_bench_results(Results,
                          [{Name, ProcessedResult} | AccProcessedResults]).

process_bench_result_name(pretty = BaseName, []) ->
    BaseName;
process_bench_result_name(pretty = BaseName, [flex_scanner]) ->
    list_to_atom(f("~w_~w", [BaseName, flex]));
process_bench_result_name(compact = BaseName, []) ->
    BaseName;
process_bench_result_name(compact = BaseName, [flex_scanner]) ->
    list_to_atom(f("~w_~w", [BaseName, flex]));
process_bench_result_name(ber = BaseName, []) ->
    BaseName;
process_bench_result_name(ber = BaseName, [native]) ->
    list_to_atom(f("~w_~w", [BaseName, native]));
process_bench_result_name(per = BaseName, []) ->
    BaseName;
process_bench_result_name(per = BaseName, [native]) ->
    list_to_atom(f("~w_~w", [BaseName, native]));
process_bench_result_name(erlang, []) ->
    erl;
process_bench_result_name(erlang, [megaco_compressed]) ->
    erl_mc;
process_bench_result_name(erlang, [compressed]) ->
    erl_c;
process_bench_result_name(erlang, [megaco_compressed, compressed]) ->
    erl_mc_c.


store_results(Results) ->
    io:format("storing: ~n", []),    
    store_excel_message_size(Results),
    store_excel_decode_time(Results),
    store_excel_encode_time(Results),
    store_excel_total_time(Results),
    io:format("~n", []),
    ok.


store_excel_message_size(Res) ->
    Filename = "message_size.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Sizes = message_sizes(Res, []),
    store_excel_tab(Fd, Sizes),
    ok.

store_excel_decode_time(Res) ->
    Filename = "decode_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Decodes = dec_times(Res, []),
    store_excel_tab(Fd, Decodes),
    ok.

store_excel_encode_time(Res) ->
    Filename = "encode_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Encodes = enc_times(Res, []),
    store_excel_tab(Fd, Encodes),
    ok.

store_excel_total_time(Res) ->
    Filename = "total_time.xls",
    io:format("  creating ~s~n", [Filename]),
    {ok, Fd} = file:open(Filename,[write]),
    Totals = tot_times(Res, []),
    store_excel_tab(Fd, Totals),
    ok.


message_sizes([], Sizes) ->
    lists:reverse(Sizes);
message_sizes([{Dir, Conf, Res}|T], Acc) ->
    Sizes = [Size || #stat{size = Size} <- Res],
    Avg   = avg(Sizes),
    message_sizes(T, [{Dir, Conf, Avg, Sizes}|Acc]).

dec_times([], Times) ->
    lists:reverse(Times);
dec_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [Time/Count || #stat{dcount = Count, dtime = Time} <- Res],
    Avg   = avg(Times),
    dec_times(T, [{Dir, Conf, Avg, Times}|Acc]).

enc_times([], Times) ->
    lists:reverse(Times);
enc_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [Time/Count || #stat{ecount = Count, etime = Time} <- Res],
    Avg   = avg(Times),
    enc_times(T, [{Dir, Conf, Avg, Times}|Acc]).

tot_times([], Times) ->
    lists:reverse(Times);
tot_times([{Dir, Conf, Res}|T], Acc) ->
    Times = [(Etime/Ecnt)+(Dtime/Dcnt) || #stat{ecount = Ecnt, 
						etime  = Etime, 
						dcount = Dcnt, 
						dtime  = Dtime} <- Res],
    Avg   = avg(Times),
    tot_times(T, [{Dir, Conf, Avg, Times}|Acc]).


avg(Vals) ->
    round(lists:sum(Vals)/length(Vals)).


store_excel_tab(_Fd, []) ->
    ok; % Just in case there was something wrong with the test
store_excel_tab(Fd, Res) ->
    %% For all elements of this list, the Values is of the same length...
    [{_, _, _, Values}|_] = Res,
    store_excel_tab_header(Fd, length(Values), 1),
    store_excel_tab1(Fd, Res).

store_excel_tab1(Fd, []) ->
    io:format(Fd, "~n", []);
store_excel_tab1(Fd, [{Dir, Conf, Avg, Values}|T]) when is_list(Conf) ->
    io:format(Fd, "~s~s (~w)", 
	      [filename:basename(Dir), config_to_string(Conf), Avg]),
    store_excel_tab_row(Fd, Values),
    store_excel_tab1(Fd, T).

config_to_string([]) ->
    "";
config_to_string([C]) when is_atom(C) ->
    io_lib:format("_~w", [C]);
config_to_string([C|Cs]) when is_atom(C) ->
    lists:flatten(io_lib:format("_~w", [C]) ++ config_to_string(Cs)).

store_excel_tab_header(Fd, 0, _) ->
    io:format(Fd, "~n", []);
store_excel_tab_header(Fd, N, M) ->
    io:format(Fd, "\t~w", [M]),
    store_excel_tab_header(Fd, N-1, M+1).

store_excel_tab_row(Fd, []) ->
    io:format(Fd, "~n", []);
store_excel_tab_row(Fd, [Value|Values]) ->
    io:format(Fd, "\t~w", [round(Value)]),
    store_excel_tab_row(Fd, Values).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_flex_scanner() ->
    Pid = proc_lib:spawn(?MODULE, flex_scanner_handler, [self()]),
    receive
        {flex_scanner_started, Pid, Conf} ->
            {Pid, [Conf]};
        {flex_scanner_error, {failed_loading_flex_scanner_driver, Reason}} ->
            throw({error, {failed_loading_flex_scanner_driver, Reason}});
        {flex_scanner_error, Reason} ->
            throw({error, {failed_loading_flex_scanner_driver, Reason}})
    after 10000 ->
            exit(Pid, kill),
            throw({error, {failed_starting_flex_scanner, timeout}})
    end.

stop_flex_scanner(Pid) ->
    Pid ! stop_flex_scanner.

-doc false.
flex_scanner_handler(Pid) ->
    case (catch megaco_flex_scanner:start()) of
        {ok, Port} when is_port(Port) ->
            Pid ! {flex_scanner_started, self(), {flex, Port}},
            flex_scanner_handler(Pid, Port);
        {ok, Ports} when is_tuple(Ports) ->
            Pid ! {flex_scanner_started, self(), {flex, Ports}},
            flex_scanner_handler(Pid, Ports);
        {error, {load_driver, {open_error, Reason}}} ->
            Error = {failed_loading_flex_scanner_driver, Reason},
            Pid ! {flex_scanner_error, Error},
            exit(Error);
        Else ->
            Error = {unknown_result_from_start_flex_scanner, Else},
            Pid ! {flex_scanner_error, Error},
            exit(Error)
    end.

flex_scanner_handler(Pid, PortOrPorts) ->
    receive
        {ping, Pinger} ->
            Pinger ! {pong, self()},
            flex_scanner_handler(Pid, PortOrPorts);
        {'EXIT', Port, Reason} ->
	    case megaco_flex_scanner:is_scanner_port(Port, PortOrPorts) of
		true ->
		    Pid ! {flex_scanner_exit, Reason},
		    exit({flex_scanner_exit, Reason});
		false ->
		    info("exit signal from unknown port ~p"
			 "~n   Reason: ~p", [Port, Reason]),
		    flex_scanner_handler(Pid, PortOrPorts)
	    end;
        stop_flex_scanner ->
            megaco_flex_scanner:stop(PortOrPorts),
            exit(normal);
        Other ->
            info("flex scanner handler got something:~n~p", [Other]),
            flex_scanner_handler(Pid, PortOrPorts)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vprint(Verbose, F) ->
    vprint(Verbose, F, []).

vprint(true, F, A) ->
    io:format(F, A);
vprint(_, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

info(F, A) ->
    io:format(F ++ "~n", A).


error(F, A) -> 
    io:format("ERROR: " ++ F ++ "~n", A).


format(F, A) ->    
    lists:flatten(io_lib:format(F, A)).
