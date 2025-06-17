%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%% 
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
%% 
%% megaco_codec_mstone2:start().
%% 
%%----------------------------------------------------------------------
%% Purpose: mstone 2 measurement
%%          This module implement a simple performence measurment case.
%%          The architecture is as followes:
%%          - One loader process: 
%%            It keeps a list of all codec combinations, including
%%            all the messages (in a list) for each codec. 
%%            Initially it creates a timer (finished) (circa 10 minutes). 
%%            It spawns a worker process for each codec config (it also 
%%            creates a monitor to each process so it knows when they 
%%            exit). When the result comes in from a process (in the 
%%            form of a DOWN message), spawns a new worker process for 
%%            this codec config and update's the statistics.
%%            When the finished timer expires, it will stop respawing
%%            the worker processes, and instead just wait for them all
%%            to finish. 
%%            The test is finishes by printing the statistics.
%%          - A worker process for each codec combination.
%%            This process is spawned by the loader process. It receives
%%            at start a list of messages. It shall decode and then 
%%            encode each message. When all messages has been processed
%%            it exits (normally).
%%----------------------------------------------------------------------

-module(megaco_codec_mstone2).
-moduledoc """
This module implements a simple megaco codec-based performance tool.

This module implements the _mstone2_ tool, a simple megaco codec-based
performance tool.

The results, the mstone value(s), are written to stdout.

_Note_ that this module is _not_ included in the runtime part of the
application.
""".


%% Exports
-export([
	 start/0, start/1, start/2, start/3, start/4, start/5
	]).


%%%----------------------------------------------------------------------
%%% Macros
%%%----------------------------------------------------------------------

-define(LIB, megaco_codec_mstone_lib).

-ifndef(MSTONE_VERSION3).
-define(MSTONE_VERSION3, v3).
-endif.
-define(VERSION3, ?MSTONE_VERSION3).

-ifndef(MSTONE_CODECS).
-define(MSTONE_CODECS, megaco_codec_transform:codecs()).
-endif.

-define(DEFAULT_MESSAGE_PACKAGE, megaco_codec_transform:default_message_package()).
-define(DEFAULT_MODE,            standard).
-define(DEFAULT_TIME,            1).
-define(DEFAULT_FACTOR,          1).
-define(DEFAULT_RUN_TIME,        timer:minutes(?DEFAULT_TIME)).

%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#7fff).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#ffff).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#17ffe).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#1ffff).


%%%----------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------

-record(codec_data, {ref, mod, config = [], msgs = []}).

-record(state, {min_heap_sz = default, % ?MSTONE_RUNNER_MIN_HEAP_SZ
                timer,
                running     = [],
                idle        = [],
                flex_handler,
                flex_conf}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-doc(#{equiv => start/1}).
start() ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             ?DEFAULT_RUN_TIME, ?DEFAULT_MODE, ?DEFAULT_MESSAGE_PACKAGE).

-doc """
start(RunTime | MessagePackage)

This function starts the _mstone2_ performance test with all codec configs.
Processes are created dynamically. Each process make _one_ run through their
messages (decoding and encoding messages) and then exits. When one process
exits, a new is created with the same codec config and set of messages.

The number of messages processed in total (for all processes) is the mstone
value.
""".

-spec start(RunTime) -> ok when
      RunTime :: pos_integer();
           (MessagePackage) -> ok when
      MessagePackage :: atom().

start([RunTimeAtom, Mode, MessagePackage])
  when is_atom(RunTimeAtom) andalso
       is_atom(Mode) andalso
       is_atom(MessagePackage) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             ?LIB:parse_runtime(RunTimeAtom), Mode, MessagePackage);
start(RunTime) when is_integer(RunTime) andalso (RunTime > 0) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             timer:minutes(RunTime), ?DEFAULT_MODE, ?DEFAULT_MESSAGE_PACKAGE);
start(MessagePackage) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             ?DEFAULT_RUN_TIME, ?DEFAULT_MODE, MessagePackage).

-doc false.
start(RunTime, Mode)
  when is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             timer:minutes(RunTime), Mode, ?DEFAULT_MESSAGE_PACKAGE);
start(Factor, RunTime)
  when is_integer(Factor) andalso
       (Factor > 0) andalso
       is_integer(RunTime) andalso
       (RunTime > 0) ->
    do_start(#{},
             Factor, timer:minutes(RunTime),
             ?DEFAULT_MODE, ?DEFAULT_MESSAGE_PACKAGE);
start(RunTime, MessagePackage) when is_integer(RunTime) andalso (RunTime > 0) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             timer:minutes(RunTime), ?DEFAULT_MODE, MessagePackage);
start(Mode, MessagePackage)
  when (Mode =:= standard) orelse
       (Mode =:= flex) orelse
       (Mode =:= no_drv) orelse
       (Mode =:= only_drv) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             ?DEFAULT_RUN_TIME, Mode, MessagePackage).

-doc false.
start(RunTime, Mode, MessagePackage)
  when is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(#{},
             ?DEFAULT_FACTOR,
             timer:minutes(RunTime), Mode, MessagePackage);
start(Factor, RunTime, Mode)
  when is_integer(Factor) andalso
       (Factor > 0) andalso
       is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(#{},
             Factor,
             timer:minutes(RunTime), Mode, ?DEFAULT_MESSAGE_PACKAGE).


-doc false.
start(Factor, RunTime, Mode, MessagePackage)
  when is_integer(Factor) andalso
       (Factor > 0) andalso
       is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    start(#{},
          Factor, RunTime, Mode, MessagePackage);
start(Opts, Factor, RunTime, Mode) when is_map(Opts) ->
    start(Opts,
          Factor, RunTime, Mode, ?DEFAULT_MESSAGE_PACKAGE).

-doc false.
start(#{bench := Bench},
      Factor, RunTime, Mode, MessagePackage)
  when is_boolean(Bench) andalso
       is_integer(Factor) andalso
       (Factor > 0) andalso
       is_list(RunTime) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(Bench,
             Factor, ?LIB:parse_runtime(RunTime), Mode, MessagePackage);
start(#{bench := Bench},
      Factor, RunTime, Mode, MessagePackage)
  when is_boolean(Bench) andalso
       is_integer(Factor) andalso
       (Factor > 0) andalso
       is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(Bench,
             Factor, timer:minutes(RunTime), Mode, MessagePackage);
start(_,
      Factor, RunTime, Mode, MessagePackage)
  when is_integer(Factor) andalso
       (Factor > 0) andalso
       is_integer(RunTime) andalso
       (RunTime > 0) andalso
       ((Mode =:= standard) orelse
        (Mode =:= flex) orelse
        (Mode =:= no_drv) orelse
        (Mode =:= only_drv)) ->
    do_start(false,
             Factor, timer:minutes(RunTime), Mode, MessagePackage).


do_start(Bench, Factor, RunTime, Mode, MessagePackageRaw) ->
    MessagePackage = parse_message_package(MessagePackageRaw),
    mstone_init(Bench, Factor, RunTime, Mode, MessagePackage).
    
parse_message_package(MessagePackageRaw) when is_list(MessagePackageRaw) ->
    list_to_atom(MessagePackageRaw);
parse_message_package(MessagePackage) when is_atom(MessagePackage) ->
    MessagePackage;
parse_message_package(BadMessagePackage) ->
    throw({error, {bad_message_package, BadMessagePackage}}).


mstone_init(Bench,
            Factor, RunTime, Mode, MessagePackage) ->
    io:format("~n", []),
    %% io:format("MStone init with"
    %%           "~n   Run Time:        ~p ms"
    %%           "~n   Mode:            ~p"
    %%           "~n   Message Package: ~p"
    %%           "~n", [RunTime, Mode, MessagePackage]),
    ?LIB:display_os_info(),
    ?LIB:display_system_info(),
    ?LIB:display_app_info(),
    io:format("~n", []),
    Ref = erlang:monitor(process, 
			 spawn(fun() -> 
				       loader(Factor, RunTime, Mode, MessagePackage) 
			       end)),
    receive
	{'DOWN', Ref, process, _Pid, {done, Result}} ->
	    display_result(Bench, Result);

	{'DOWN', Ref, process, _Pid, Result} ->
	    io:format("Unexpected result:"
                      "~n      ~p"
                      "~n", [Result]),
	    Result
    end.


mstone_maybe_bench(true, MStone) ->
    {bench, MStone};
mstone_maybe_bench(false, _) ->
    ok.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

display_result(Bench, Result) ->
    {value, {worker_cnt, WC}} = lists:keysearch(worker_cnt, 1, Result),
    CodecStat = 
	[{Mod, Conf, Cnt} || {{codec_cnt, Mod, Conf}, Cnt} <- Result],
    MStone = lists:sum([Cnt || {_, _, Cnt} <- CodecStat]),
    io:format("Number of procs spawned: ~w~n"
	      "MStone:                  ~w~n"
	      "~n", [WC, MStone]),
    display_worker_result(lists:keysort(3, CodecStat)),
    mstone_maybe_bench(Bench, MStone).
    
display_worker_result([]) ->
    io:format("~n", []);
display_worker_result([{Mod, Conf, Cnt}|Res]) ->
    io:format("~s: ~w~n", [image_of(Mod, Conf), Cnt]),
    display_worker_result(Res).

image_of(megaco_per_encoder, Conf) ->
    bin_image("per", Conf);
image_of(megaco_ber_encoder, Conf) ->
    bin_image("ber", Conf);
image_of(megaco_pretty_text_encoder, Conf) ->
    text_image("pretty", Conf);
image_of(megaco_compact_text_encoder, Conf) ->
    text_image("compact", Conf);
image_of(megaco_erl_dist_encoder, Conf) ->
    erl_image("erl_dist", Conf).

bin_image(Bin, Conf) ->
    Nat = 
	case lists:member(native, Conf) of
	    true ->
		[native];
	    false ->
		[]
	end,
    io_lib:format("~s ~w", [Bin, Nat]).

text_image(Txt, Conf) ->
    Flex = 
	case lists:keysearch(flex, 1, Conf) of
	    false ->
		[];
	    _ ->
		[flex]
	end,
    io_lib:format("~s ~w", [Txt, Flex]).

erl_image(Erl, Conf) ->
    MC = 
	case lists:member(megaco_compressed, Conf) of
	    true ->
		[megaco_compressed];
	    false ->
		[]
	end,
    C = 
	case lists:member(compressed, Conf) of
	    true ->
		[compressed];
	    false ->
		[]
	end,
    io_lib:format("~s ~w", [Erl, MC ++ C]).
    

%%%----------------------------------------------------------------------

loader(Factor, RunTime, Mode, MessagePackage) ->
    loader(Factor, RunTime, Mode, ?MSTONE_CODECS, MessagePackage).


%% Codecs is a list of megaco codec shortnames: 
%%
%%    pretty | compact | ber | per | erlang
%%

loader(Factor, RunTime, Mode, Codecs, MessagePackage) ->
    process_flag(trap_exit, true),
    case (catch init(Factor, RunTime, Mode, Codecs, MessagePackage)) of
	{ok, State} ->
	    loader_loop(running, State);
	{error, Reason} = Error ->
            io:format("<ERROR> Failed starting loader: "
                      "~n      ~p", [Reason]),
	    exit(Error)
    end.

init(Factor, RunTime, Mode, Codecs, MessagePackage) ->
    ets:new(mstone, [set, private, named_table, {keypos, 1}]),
    ets:insert(mstone, {worker_cnt, 0}),
    {Pid, FlexConf} = ?LIB:start_flex_scanner(),
    io:format("prepare messages", []),
    EMessages = ?LIB:expanded_messages(MessagePackage, Codecs, Mode), 
    io:format("~ninit codec data", []),
    CodecData = init_codec_data(Factor, EMessages, FlexConf),
    Timer = erlang:send_after(RunTime, self(), mstone_finished), 
    io:format(" => ~w concurrent workers~n", [length(CodecData)]),
    {ok, #state{timer        = Timer, 
                idle         = CodecData, 
                flex_handler = Pid, 
                flex_conf    = FlexConf}}.

init_codec_data(Factor, EMsgs, FlexConf) ->
    init_codec_data_expand(Factor, init_codec_data(EMsgs, FlexConf)).

init_codec_data_expand(1 = _Factor, CodecData) ->
    CodecData;
init_codec_data_expand(Factor, CodecData) ->
    lists:flatten(lists:duplicate(Factor, CodecData)).

init_codec_data(EMsgs, FlexConf) ->
    [init_codec_data(Codec, Mod, Conf, Msgs, FlexConf) || 
	{Codec, Mod, Conf, Msgs} <- EMsgs].

init_codec_data(Codec, Mod, Conf0, Msgs0, FlexConf) 
  when is_atom(Codec) andalso 
       is_atom(Mod)   andalso 
       is_list(Conf0) andalso 
       is_list(Msgs0) ->
    io:format(".", []),
    Conf = [{version3,?VERSION3}|init_codec_conf(FlexConf, Conf0)], 
    Msgs = [?LIB:detect_version(Mod, Conf, Bin) || {_, Bin} <- Msgs0],
    ets:insert(mstone, {{codec_cnt, Mod, Conf}, 0}),
    #codec_data{mod = Mod, config = Conf, msgs = Msgs}.
    

init_codec_conf(FlexConf, [flex_scanner]) ->
    FlexConf;
init_codec_conf(_, Conf) ->
    Conf.


%% -- Main loop --

loader_loop(finishing, #state{flex_handler = Pid, running = []}) ->	
    %% we are done
    io:format("~n", []),
    ?LIB:stop_flex_scanner(Pid),
    exit({done, lists:sort(ets:tab2list(mstone))});

loader_loop(finishing, State) ->
    receive
	{'DOWN', Ref, process, _Pid, {mstone_done, Codec, Conf, Cnt}} ->
            %% io:format("[~s] [loader:finishing] worker done~n",
            %%           [megaco:formated_timestamp()]),
	    loader_loop(finishing, done_worker(Ref, Codec, Conf, Cnt, State))
    end;

loader_loop(running, #state{idle = []} = State) ->	    
    receive
	mstone_finished ->
            %% io:format("[~s] [loader:running,idle] -> finish~n",
            %%           [megaco:formated_timestamp()]),
	    loader_loop(finishing, State);

	{'DOWN', Ref, process, _Pid, {mstone_done, Codec, Conf, Cnt}} ->
            %% io:format("[~s] [loader:running] worker done~n",
            %%           [megaco:formated_timestam()]),
	    loader_loop(running, done_worker(Ref, Codec, Conf, Cnt, State))
    end;

loader_loop(running, State) ->	
    receive
	mstone_finished ->
            %% io:format("[~s] [loader:running] -> finish~n",
            %%           [megaco:formated_timestamp()]),
	    loader_loop(finishing, State);

	{'DOWN', Ref, process, _Pid, {mstone_done, Codec, Conf, Cnt}} ->
            %% io:format("[~s] [loader:running] worker done~n",
            %%           [megaco:formated_timestamp()]),
	    State2 = done_worker(Ref, Codec, Conf, Cnt, State),
	    loader_loop(running, State2)

    after 0 ->
            %% io:format("[~s] [loader:running] start worker~n",
            %%           [megaco:formated_timestamp()]),
	    loader_loop(running, start_worker(State))
    end.

done_worker(Ref, Codec, Conf, Cnt,
	    #state{running = Running, idle = Idle} = State) ->
    %% io:format("worker ~w ~w done with ~w~n", [Codec, Conf, Cnt]),
    ets:update_counter(mstone, worker_cnt, 1),
    ets:update_counter(mstone, {codec_cnt, Codec, Conf}, Cnt),
    Running2 = lists:keydelete(Ref, #codec_data.ref, Running),
    CD = Running -- Running2,
    State#state{running = Running2, idle = lists:append(Idle, CD)}.

start_worker(#state{min_heap_sz = SZ,
                    running = Running, idle = [H|T]} = State) ->
    #codec_data{mod = Codec, config = Conf, msgs = Msgs} = H,
    Worker = fun() -> worker(Codec, Conf, Msgs, 0) end,
    {_, Ref} = erlang:spawn_opt(Worker, worker_spawn_opts(SZ)),
    %% Ref    = erlang:monitor(process, spawn(Worker)),
    CD = H#codec_data{ref = Ref},
    State#state{running = [CD | Running], idle = T}.


worker_spawn_opts(SZ) when is_integer(SZ) andalso (SZ > 0) ->
    [monitor, {min_heap_size, SZ}];
worker_spawn_opts(_) ->
    [monitor].


%%%----------------------------------------------------------------------

worker(Codec, Conf, [], Cnt) ->
    exit({mstone_done, Codec, Conf, Cnt});
worker(Codec, Conf, [{V, Msg}|Msgs], Cnt) ->
    work(Codec, Conf, V, Msg),
    worker(Codec, Conf, Msgs, Cnt + 1).

work(Codec, Conf, V, M) ->
    case (catch apply(Codec, decode_message, [Conf, V, M])) of
        {ok, Msg} ->
            case (catch apply(Codec, encode_message, [Conf, V, Msg])) of
                {ok, Bin} when is_binary(Bin) ->
                    ok;
                EncodeError ->
		    emsg("failed encoding message: ~n~p", [EncodeError]),
                    exit({mstone_worker_encode_failure, EncodeError})
            end;
        DecodeError ->
            emsg("failed decoding message: ~n~p", [DecodeError]),
	    exit({mstone_worker_decode_failure, DecodeError})
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emsg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).


