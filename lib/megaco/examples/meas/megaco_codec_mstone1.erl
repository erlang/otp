%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%% Purpose: mstone measurement
%%
%%----------------------------------------------------------------------

-module(megaco_codec_mstone1).


%% API
-export([
	 start/0,          start/1,          start/2,
	 start_flex/0,     start_flex/1,     start_flex/2,
	 start_no_drv/0,   start_no_drv/1,   start_no_drv/2,
	 start_only_drv/0, start_only_drv/1, start_only_drv/2
	]).

%% Internal exports
-export([mstone_runner_init/5]).


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

%% -define(VERBOSE_STATS,true).

-ifndef(MSTONE_RUNNER_MIN_HEAP_SZ).
-define(MSTONE_RUNNER_MIN_HEAP_SZ,  16#ffff).
-endif.
-define(MSTONE_RUNNER_OPTS, 
        [link, {min_heap_size, ?MSTONE_RUNNER_MIN_HEAP_SZ}]).

-record(mstone, {id, count, codec, econf, heap_size, reds}).


start() ->
    start(?DEFAULT_FACTOR).

start([Factor]) ->
    start(?DEFAULT_MESSAGE_PACKAGE, Factor);
start([MessagePackage, Factor]) ->
    start(MessagePackage, Factor);
start(Factor) ->
    start(?DEFAULT_MESSAGE_PACKAGE, Factor).

start(MessagePackage, Factor) ->
    do_start(MessagePackage, Factor, ?DEFAULT_DRV_INCLUDE).


start_flex() ->
    start_flex(?DEFAULT_FACTOR).

start_flex([Factor]) ->
    start_flex(?DEFAULT_MESSAGE_PACKAGE, Factor);
start_flex([MessagePackage, Factor]) ->
    start_flex(MessagePackage, Factor);
start_flex(Factor) ->
    start_flex(?DEFAULT_MESSAGE_PACKAGE, Factor).

start_flex(MessagePackage, Factor) ->
    do_start(MessagePackage, Factor, flex).


start_only_drv() ->
    start_only_drv(?DEFAULT_FACTOR).

start_only_drv([Factor]) ->
    start_only_drv(?DEFAULT_MESSAGE_PACKAGE, Factor);
start_only_drv([MessagePackage, Factor]) ->
    start_only_drv(MessagePackage, Factor);
start_only_drv(Factor) ->
    start_only_drv(?DEFAULT_MESSAGE_PACKAGE, Factor).

start_only_drv(MessagePackage, Factor) ->
    do_start(MessagePackage, Factor, only_drv).


start_no_drv() ->
    start_no_drv(?DEFAULT_FACTOR).

start_no_drv([Factor]) ->
    start_no_drv(?DEFAULT_MESSAGE_PACKAGE, Factor);
start_no_drv([MessagePackage, Factor]) ->
    start_no_drv(MessagePackage, Factor);
start_no_drv(Factor) ->
    start_no_drv(?DEFAULT_MESSAGE_PACKAGE, Factor).

start_no_drv(MessagePackage, Factor) ->
    do_start(MessagePackage, Factor, no_drv).

    
do_start(MessagePackageRaw, FactorRaw, DrvInclude) ->
    Factor         = parse_factor(FactorRaw),
    MessagePackage = parse_message_package(MessagePackageRaw),
    mstone_init(MessagePackage, Factor, DrvInclude).
	


parse_factor(FactorAtom) when is_atom(FactorAtom) ->
    case (catch list_to_integer(atom_to_list(FactorAtom))) of
	Factor when is_integer(Factor) andalso (Factor > 0) ->
	    Factor;
	_ ->
	    io:format("ERROR: Bad factor value: ~p~n", [FactorAtom]),
	    throw({error, {bad_factor, FactorAtom}})
    end;
parse_factor(FactorRaw) when is_list(FactorRaw) ->
    case (catch list_to_integer(FactorRaw)) of
	Factor when is_integer(Factor) andalso (Factor > 0) ->
	    Factor;
	_ ->
	    io:format("ERROR: Bad factor value: ~p~n", [FactorRaw]),
	    throw({error, {bad_factor, FactorRaw}})
    end;
parse_factor(Factor) when is_integer(Factor) andalso (Factor > 0) ->
    Factor;
parse_factor(BadFactor) ->
    throw({error, {bad_factor, BadFactor}}).
    

parse_message_package(MessagePackageRaw) when is_list(MessagePackageRaw) ->
    list_to_atom(MessagePackageRaw);
parse_message_package(MessagePackage) when is_atom(MessagePackage) ->
    MessagePackage;
parse_message_package(BadMessagePackage) ->
    throw({error, {bad_message_package, BadMessagePackage}}).


%% Codecs is a list of megaco codec shortnames: 
%%
%%    pretty | compact | ber | per | erlang
%%

mstone_init(MessagePackage, Factor, DrvInclude) ->
%%     io:format("mstone_init -> entry with"
%% 	      "~n   MessagePackage: ~p"
%% 	      "~n   Factor:         ~p"
%% 	      "~n   DrvInclude:     ~p"
%% 	      "~n", [MessagePackage, Factor, DrvInclude]),
    Codecs = ?MSTONE_CODECS, 
    mstone_init(MessagePackage, Factor, Codecs, DrvInclude).

mstone_init(MessagePackage, Factor, Codecs, DrvInclude) ->
    Parent = self(), 
    Pid = spawn(
	    fun() -> 
		    process_flag(trap_exit, true),
		    do_mstone(MessagePackage, Factor, Codecs, DrvInclude),  
		    Parent ! {done, self()}
	    end),
    receive
	{done, Pid} ->
	    ok
    end.
			 
do_mstone(MessagePackage, Factor, Codecs, DrvInclude) ->
    io:format("~n", []),
    ?LIB:display_os_info(),
    ?LIB:display_system_info(),
    ?LIB:display_app_info(),
    io:format("~n", []),
    (catch asn1rt_driver_handler:load_driver()),
    {Pid, Conf} = ?LIB:start_flex_scanner(),
    put(flex_scanner_conf, Conf),
    EMessages = ?LIB:expanded_messages(MessagePackage, Codecs, DrvInclude), 
    EMsgs  = duplicate(Factor, EMessages),
    MStone = t1(EMsgs),
    ?LIB:stop_flex_scanner(Pid),
    io:format("~n", []),
    io:format("MStone: ~p~n", [MStone]).

duplicate(N, Elements) ->
    duplicate(N, Elements, []).

duplicate(_N, [], Acc) ->
    lists:flatten(Acc);
duplicate(N, [H|T], Acc) ->
    duplicate(N, T, [lists:duplicate(N, H)|Acc]).

t1(EMsgs) ->
    io:format(" * starting runners [~w] ", [length(EMsgs)]),
    t1(EMsgs, []).

t1([], Runners) ->
    io:format(" done~n * await runners ready ", []),
    await_runners_ready(Runners),
    io:format(" done~n * now snooze", []),
    receive after 5000 -> ok end,
    io:format("~n * release them~n", []),
    lists:foreach(fun(P) -> P ! {go, self()} end, Runners),
    t2(1, [], Runners);
t1([H|T], Runners) ->
    Runner = init_runner(H),
    io:format(".", []),
    t1(T, [Runner|Runners]).

await_runners_ready([]) ->
    ok;
await_runners_ready(Runners) ->
    receive
        {ready, Runner} ->
            io:format(".", []),
	    %% i("runner ~w ready", [Runner]),
            await_runners_ready(lists:delete(Runner, Runners));
	{'EXIT', Pid, Reason} ->
	    case lists:member(Pid, Runners) of
		true ->
		    io:format("~nERROR: "
			      "received (unexpected) exit signal "
			      "from from runner ~p:"
			      "~n~p~n", [Pid, Reason]),
		    exit(Reason);
		false ->
		    await_runners_ready(Runners)
	    end
    end.

-ifdef(VERBOSE_STATS).
print_runner_stats(RunnerStats) ->
    Sorted = lists:keysort(2, RunnerStats),
    lists:foreach(fun(#mstone{id        = Id,
			      count     = Num,
                              codec     = Codec,
                              econf     = EConf, 
                              heap_size = HeapSz, 
                              reds      = Reds}) ->
			  i("runner: ~w"
			    "~n   Count:           ~w"
			    "~n   Codec:           ~w"
			    "~n   Encoding config: ~p"
			    "~n   Heap size:       ~p"
			    "~n   Reductions:      ~p", 
			    [Id, Num, Codec, EConf, HeapSz, Reds]) end, 
                  Sorted),
    ok.
-else.
print_runner_stats(_) ->
    ok.
-endif.

t2(_, Acc, []) ->
    i("~n~w runners", [length(Acc)]),
    print_runner_stats(Acc),

    HeapSzAcc = lists:sort([HS || #mstone{heap_size = HS} <- Acc]),
    i("Runner heap size data:"
      "~n   Min: ~w"
      "~n   Max: ~w"
      "~n   Avg: ~w", 
      [hd(HeapSzAcc), 
       hd(lists:reverse(HeapSzAcc)), 
       (lists:sum(HeapSzAcc) div length(HeapSzAcc))]), 
    
    RedsAcc   = lists:sort([R || #mstone{reds = R} <- Acc]),
    i("Runner reductions data:"
      "~n   Min: ~w"
      "~n   Max: ~w"
      "~n   Avg: ~w", 
      [hd(RedsAcc), 
       hd(lists:reverse(RedsAcc)), 
       (lists:sum(RedsAcc) div length(RedsAcc))]), 

    lists:sum([Num || #mstone{count = Num} <- Acc]);
t2(N, Acc, Runners) ->
    receive
	{'EXIT', Pid, {runner_done, Codec, Conf, Num, Info}} ->
            {value, {_, HeapSz}} = lists:keysearch(heap_size,  1, Info),
            {value, {_, Reds}}   = lists:keysearch(reductions, 1, Info),
            MStone = #mstone{id        = N,
			     count     = Num,
                             codec     = Codec,
                             econf     = Conf,
                             heap_size = HeapSz,
                             reds      = Reds},
	    t2(N + 1, [MStone|Acc], lists:delete(Pid, Runners))
    end.

init_runner({Codec, Mod, Conf, Msgs}) ->
    Conf1 = runner_conf(Conf),
    Conf2 = [{version3,?VERSION3}|Conf1],
    Pid   = spawn_opt(?MODULE, mstone_runner_init, 
		      [Codec, self(), Mod, Conf2, Msgs],
		      ?MSTONE_RUNNER_OPTS),
    Pid.

runner_conf([flex_scanner]) ->
    get(flex_scanner_conf);
runner_conf(Conf) ->
    Conf.



detect_versions(Codec, _Conf, [], []) ->
    exit({no_messages_found_for_codec, Codec});
detect_versions(_Codec, _Conf, [], Acc) ->
    lists:reverse(Acc);
detect_versions(Codec, Conf, [{_Name, Bin}|Bins], Acc) ->
    Data = ?LIB:detect_version(Codec, Conf, Bin),
    detect_versions(Codec, Conf, Bins, [Data|Acc]).
	    

mstone_runner_init(_Codec, Parent, Mod, Conf, Msgs0) ->
    Msgs = detect_versions(Mod, Conf, Msgs0, []),
    warmup(Mod, Conf, Msgs, []),
    Parent ! {ready, self()},
    receive
        {go, Parent} ->
            ok
    end,
    erlang:send_after(?MSTONE_RUN_TIME, self(), stop),
    mstone_runner_loop(Parent, Mod, Conf, 0, Msgs).

mstone_runner_loop(Parent, Mod, Conf, N, Msgs1) ->
    receive
        stop ->
            exit({runner_done, Mod, Conf, N, mstone_runner_process_info()})
    after 0 ->
        {Inc, Msgs2} = mstone_all(Mod, Conf, Msgs1, []),
	mstone_runner_loop(Parent, Mod, Conf, N+Inc, Msgs2)
    end.

mstone_runner_process_info() ->
    PI = process_info(self()),
    FL = [heap_size, stack_size, reductions],
    lists:filter(fun({Key, _}) -> lists:member(Key, FL) end, PI).
			  
		      
mstone_all(_Codec, _Conf, [], Acc) ->
    {length(Acc), lists:reverse(Acc)};
mstone_all(Codec, Conf, [{V, Bin}|Bins], Acc) when is_binary(Bin) ->
    {ok, Msg} = apply(Codec, decode_message, [Conf, V, Bin]),
    mstone_all(Codec, Conf, Bins, [{V, Msg}|Acc]);
mstone_all(Codec, Conf, [{V, Msg}|Msgs], Acc) ->
    {ok, Bin} = apply(Codec, encode_message, [Conf, V, Msg]),
    mstone_all(Codec, Conf, Msgs, [{V, Bin}|Acc]).

warmup(_Codec, _Conf, [], Acc) ->
    lists:reverse(Acc);
warmup(Codec, Conf, [{V, M}|Msgs], Acc) ->
%%     io:format("~p warmup -> entry with"
%% 	      "~n   Codec: ~p"
%% 	      "~n   Conf:  ~p"
%% 	      "~n", [self(), Codec, Conf]),
    case (catch apply(Codec, decode_message, [Conf, V, M])) of
        {ok, Msg} ->
            case (catch apply(Codec, encode_message, [Conf, V, Msg])) of
                {ok, Bin} ->
                    warmup(Codec, Conf, Msgs, [Bin|Acc]);
                EncodeError ->
                    emsg("failed encoding message: ~n~p", [EncodeError])
            end;
        DecodeError ->
            emsg("failed decoding message: "
		 "~n   DecodeError: ~p"
		 "~n   V:           ~p"
		 "~n   M:           ~p", [DecodeError, V, M])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

emsg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

%% i(F) ->
%%     i(F, []).
i(F, A) ->
    io:format(F ++ "~n", A).

