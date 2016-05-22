%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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
%% megaco_codec_mstone2:start_no_drv().
%% megaco_codec_mstone2:start_only_drv().
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


%% Exports
-export([
	 start/0,          start/1, 
	 start_flex/0,     start_flex/1, 
	 start_no_drv/0,   start_no_drv/1, 
	 start_only_drv/0, start_only_drv/1
	]).


%%%----------------------------------------------------------------------
%%% Macros
%%%----------------------------------------------------------------------

-define(LIB, megaco_codec_mstone_lib).

-ifndef(MSTONE_TIME).
-define(MSTONE_TIME, 10).
-endif.
-define(MSTONE_RUN_TIME, timer:minutes(?MSTONE_TIME)).

-ifndef(MSTONE_VERSION3).
-define(MSTONE_VERSION3, v3).
-endif.
-define(VERSION3, ?MSTONE_VERSION3).

-ifndef(MSTONE_CODECS).
-define(MSTONE_CODECS, megaco_codec_transform:codecs()).
-endif.

-define(DEFAULT_MESSAGE_PACKAGE, megaco_codec_transform:default_message_package()).
-define(DEFAULT_FACTOR,          1).
-define(DEFAULT_DRV_INCLUDE,     ignore).

-ifndef(MSTONE_RUNNER_MIN_HEAP_SZ).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ,  16#7fff).
-define(MSTONE_RUNNER_MIN_HEAP_SZ,  16#ffff).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#17ffe).
%% -define(MSTONE_RUNNER_MIN_HEAP_SZ, 16#1ffff).
%% -define(MSTONE_RUNNER_OPTS, [link]).
-endif.
-define(MSTONE_RUNNER_OPTS, 
        [link, {min_heap_size, ?MSTONE_RUNNER_MIN_HEAP_SZ}]).


%%%----------------------------------------------------------------------
%%% Records
%%%----------------------------------------------------------------------

-record(codec_data, {ref, mod, config = [], msgs = []}).

-record(state, {timer, running = [], idle = [], flex_handler, flex_conf}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start() ->
    start(?DEFAULT_MESSAGE_PACKAGE).

start([MessagePackage]) ->
    do_start(MessagePackage, ?DEFAULT_DRV_INCLUDE);
start(MessagePackage) ->
    do_start(MessagePackage, ?DEFAULT_DRV_INCLUDE).


start_flex() ->
    start_flex(?DEFAULT_MESSAGE_PACKAGE).

start_flex([MessagePackage]) ->
    do_start(MessagePackage, flex);
start_flex(MessagePackage) ->
    do_start(MessagePackage, flex).


start_no_drv() ->
    start_no_drv(?DEFAULT_MESSAGE_PACKAGE).

start_no_drv([MessagePackage]) ->
    do_start(MessagePackage, no_drv);
start_no_drv(MessagePackage) ->
    do_start(MessagePackage, no_drv).


start_only_drv() ->
    start_only_drv(?DEFAULT_MESSAGE_PACKAGE).

start_only_drv([MessagePackage]) ->
    do_start(MessagePackage, only_drv);
start_only_drv(MessagePackage) ->
    do_start(MessagePackage, only_drv).


do_start(MessagePackageRaw, DrvInclude) ->
%%     io:format("do_start -> entry with"
%% 	      "~n   MessagePackageRaw: ~p"
%% 	      "~n   DrvInclude:        ~p"
%% 	      "~n", [MessagePackageRaw, DrvInclude]), 
    MessagePackage = parse_message_package(MessagePackageRaw),
    mstone_init(MessagePackage, DrvInclude).
    
parse_message_package(MessagePackageRaw) when is_list(MessagePackageRaw) ->
    list_to_atom(MessagePackageRaw);
parse_message_package(MessagePackage) when is_atom(MessagePackage) ->
    MessagePackage;
parse_message_package(BadMessagePackage) ->
    throw({error, {bad_message_package, BadMessagePackage}}).


mstone_init(MessagePackage, DrvInclude) ->
    io:format("~n", []),
    ?LIB:display_os_info(),
    ?LIB:display_system_info(),
    ?LIB:display_app_info(),
    io:format("~n", []),
    Ref = erlang:monitor(process, 
			 spawn(fun() -> 
				       loader(MessagePackage, DrvInclude) 
			       end)),
    receive
	{'DOWN', Ref, process, _Pid, {done, Result}} ->
	    display_result(Result);
	{'DOWN', Ref, process, _Pid, Result} ->
	    io:format("Unexpected result:~n~p~n", [Result]),
	    ok
    end.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

display_result(Result) ->
    {value, {worker_cnt, WC}} = lists:keysearch(worker_cnt, 1, Result),
    CodecStat = 
	[{Mod, Conf, Cnt} || {{codec_cnt, Mod, Conf}, Cnt} <- Result],
    MStone = lists:sum([Cnt || {_, _, Cnt} <- CodecStat]),
    io:format("Number of procs spawned: ~w~n"
	      "MStone:                  ~w~n"
	      "~n", [WC, MStone]),
    display_worker_result(lists:keysort(3, CodecStat)),
    ok.
    
display_worker_result([]) ->
    io:format("~n", []);
display_worker_result([{Mod, Conf, Cnt}|Res]) ->
    io:format("~s: ~w~n", [image_of(Mod, Conf), Cnt]),
    display_worker_result(Res).

image_of(megaco_per_bin_encoder, Conf) ->
    bin_image("per_bin", Conf);
image_of(megaco_ber_bin_encoder, Conf) ->
    bin_image("ber_bin", Conf);
image_of(megaco_pretty_text_encoder, Conf) ->
    text_image("pretty", Conf);
image_of(megaco_compact_text_encoder, Conf) ->
    text_image("compact", Conf);
image_of(megaco_erl_dist_encoder, Conf) ->
    erl_image("erl_dist", Conf).

bin_image(Bin, Conf) ->
    Drv = 
	case lists:member(driver, Conf) of
	    true ->
		[driver];
	    false ->
		[]
	end,
    Nat = 
	case lists:member(native, Conf) of
	    true ->
		[native];
	    false ->
		[]
	end,
    io_lib:format("~s ~w", [Bin, Drv ++ Nat]).

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

loader(MessagePackage, DrvInclude) ->
    loader(?MSTONE_CODECS, MessagePackage, DrvInclude).


%% Codecs is a list of megaco codec shortnames: 
%%
%%    pretty | compact | ber | per | erlang
%%

loader(Codecs, MessagePackage, DrvInclude) ->
    process_flag(trap_exit, true),
    case (catch init(Codecs, MessagePackage, DrvInclude)) of
	{ok, State} ->
	    loader_loop(running, State);
	Error ->
	    exit(Error)
    end.

init(Codecs, MessagePackage, DrvInclude) ->
    ets:new(mstone, [set, private, named_table, {keypos, 1}]),
    ets:insert(mstone, {worker_cnt, 0}),
    {Pid, FlexConf} = ?LIB:start_flex_scanner(),
    io:format("prepare messages", []),
    EMessages = ?LIB:expanded_messages(MessagePackage, Codecs, DrvInclude), 
    io:format("~ninit codec data", []),
    CodecData = init_codec_data(EMessages, FlexConf),
    Timer = erlang:send_after(?MSTONE_RUN_TIME, self(), mstone_finished), 
    io:format("~n", []),
    {ok, #state{timer        = Timer, 
		idle         = CodecData, 
		flex_handler = Pid, 
		flex_conf    = FlexConf}}.

init_codec_data(EMsgs, FlexConf) ->
    [init_codec_data(Codec, Mod, Conf, Msgs, FlexConf) || 
	{Codec, Mod, Conf, Msgs} <- EMsgs].

init_codec_data(Codec, Mod, Conf0, Msgs0, FlexConf) 
  when is_atom(Codec) andalso 
       is_atom(Mod)   andalso 
       is_list(Conf0) andalso 
       is_list(Msgs0)  ->
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
	    loader_loop(finishing, done_worker(Ref, Codec, Conf, Cnt, State))
    end;

loader_loop(running, #state{idle = []} = State) ->	    
    receive
	mstone_finished ->
	    loader_loop(finishing, State);

	{'DOWN', Ref, process, _Pid, {mstone_done, Codec, Conf, Cnt}} ->
	    loader_loop(running, done_worker(Ref, Codec, Conf, Cnt, State))
    end;

loader_loop(running, State) ->	
    receive
	mstone_finished ->
	    loader_loop(finishing, State);

	{'DOWN', Ref, process, _Pid, {mstone_done, Codec, Conf, Cnt}} ->
	    State2 = done_worker(Ref, Codec, Conf, Cnt, State),
	    loader_loop(running, State2)

    after 0 ->
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

start_worker(#state{running = Running, idle = [H|T]} = State) ->
    #codec_data{mod = Codec, config = Conf, msgs = Msgs} = H,
    Worker = fun() -> worker(Codec, Conf, Msgs, 0) end, 
    Ref    = erlang:monitor(process, spawn(Worker)),
    CD = H#codec_data{ref = Ref},
    State#state{running = [CD | Running], idle = T}.


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


