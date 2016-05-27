%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(rand_SUITE).
-export([all/0, suite/0,groups/0]).

-export([interval_int/1, interval_float/1, seed/1,
         api_eq/1, reference/1,
	 basic_stats_uniform_1/1, basic_stats_uniform_2/1,
	 basic_stats_normal/1,
	 plugin/1, measure/1]).

-export([test/0, gen/1]).

-include_lib("common_test/include/ct.hrl").

-define(LOOP, 1000000).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() ->
    [seed, interval_int, interval_float,
     api_eq,
     reference,
     {group, basic_stats},
     plugin, measure].

groups() ->
    [{basic_stats, [parallel],
      [basic_stats_uniform_1, basic_stats_uniform_2, basic_stats_normal]}].

%% A simple helper to test without test_server during dev
test() ->
    Tests = all(),
    lists:foreach(fun(Test) ->
			  try
			      ok = ?MODULE:Test([]),
			      io:format("~p: ok~n", [Test])
			  catch _:Reason ->
				  io:format("Failed: ~p: ~p ~p~n",
					    [Test, Reason, erlang:get_stacktrace()])
			  end
		  end, Tests).

algs() ->
    [exs64, exsplus, exs1024].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test that seed and seed_s and export_seed/0 is working.
seed(Config) when is_list(Config) ->
    Algs = algs(),
    Test = fun(Alg) ->
		   try seed_1(Alg)
		   catch _:Reason ->
			   ct:fail({Alg, Reason, erlang:get_stacktrace()})
		   end
	   end,
    [Test(Alg) || Alg <- Algs],
    ok.

seed_1(Alg) ->
    %% Check that uniform seeds automatically,
    _ = rand:uniform(),
    S00 = get(rand_seed),
    erase(),
    _ = rand:uniform(),
    false = S00 =:= get(rand_seed), %% hopefully

    %% Choosing algo and seed
    S0 = rand:seed(Alg, {0, 0, 0}),
    %% Check that (documented?) process_dict variable is correct
    S0 = get(rand_seed),
    S0 = rand:seed_s(Alg, {0, 0, 0}),
    %% Check that process_dict should not be used for seed_s functionality
    _ = rand:seed_s(Alg, {1, 0, 0}),
    S0 = get(rand_seed),
    %% Test export
    ES0 = rand:export_seed(),
    ES0 = rand:export_seed_s(S0),
    S0 = rand:seed(ES0),
    S0 = rand:seed_s(ES0),
    %% seed/1 calls should be unique
    S1 = rand:seed(Alg),
    false = (S1 =:= rand:seed_s(Alg)),
    %% Negative integers works
    _ = rand:seed_s(Alg, {-1,-1,-1}),
    %% Check that export_seed/1 returns 'undefined' if there is no seed
    erase(rand_seed),
    undefined = rand:export_seed(),

    %% Other term do not work
    {'EXIT', _} = (catch rand:seed_s(foobar, os:timestamp())),
    {'EXIT', _} = (catch rand:seed_s(Alg, {asd, 1, 1})),
    {'EXIT', _} = (catch rand:seed_s(Alg, {0, 234.1234, 1})),
    {'EXIT', _} = (catch rand:seed_s(Alg, {0, 234, [1, 123, 123]})),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check that both APIs are consistent with each other.
api_eq(_Config) ->
    Algs = algs(),
    Small = fun(Alg) ->
		    Seed = rand:seed(Alg),
		    io:format("Seed ~p~n",[rand:export_seed_s(Seed)]),
		    api_eq_1(Seed)
	    end,
    _ = [Small(Alg) || Alg <- Algs],
    ok.

api_eq_1(S00) ->
    Check = fun(_, Seed) ->
		    {V0, S0} = rand:uniform_s(Seed),
		    V0 = rand:uniform(),
		    {V1, S1} = rand:uniform_s(1000000, S0),
		    V1 = rand:uniform(1000000),
		    {V2, S2} = rand:normal_s(S1),
		    V2 = rand:normal(),
		    S2
	    end,
    S1 = lists:foldl(Check, S00, lists:seq(1, 200)),
    S1 = get(rand_seed),
    {V0, S2} = rand:uniform_s(S1),
    V0 = rand:uniform(),
    S2 = get(rand_seed),

    Exported = rand:export_seed(),
    Exported = rand:export_seed_s(S2),

    S3 = lists:foldl(Check, S2, lists:seq(1, 200)),
    S3 = get(rand_seed),

    S4 = lists:foldl(Check, S3, lists:seq(1, 200)),
    S4 = get(rand_seed),
    %% Verify that we do not have loops
    false = S1 =:= S2,
    false = S2 =:= S3,
    false = S3 =:= S4,

    S2 = rand:seed(Exported),
    S3 = lists:foldl(Check, S2, lists:seq(1, 200)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check that uniform/1 returns values within the proper interval.
interval_int(Config) when is_list(Config) ->
    Algs = algs(),
    Small = fun(Alg) ->
		    Seed = rand:seed(Alg),
		    io:format("Seed ~p~n",[rand:export_seed_s(Seed)]),
		    Max = interval_int_1(100000, 7, 0),
		    Max =:= 7 orelse exit({7, Alg, Max})
	    end,
    _ = [Small(Alg) || Alg <- Algs],
    %% Test large integers
    Large = fun(Alg) ->
		    Seed = rand:seed(Alg),
		    io:format("Seed ~p~n",[rand:export_seed_s(Seed)]),
		    Max = interval_int_1(100000, 1 bsl 128, 0),
		    Max > 1 bsl 64 orelse exit({large, Alg, Max})
	    end,
    [Large(Alg) || Alg <- Algs],
    ok.

interval_int_1(0, _, Max) -> Max;
interval_int_1(N, Top, Max) ->
    X = rand:uniform(Top),
    if
	0 < X, X =< Top ->
	    ok;
	true ->
	    io:format("X=~p Top=~p 0<~p<~p~n", [X,Top,X,Top]),
	    exit({X, rand:export_seed()})
    end,
    interval_int_1(N-1, Top, max(X, Max)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check that uniform/0 returns values within the proper interval.
interval_float(Config) when is_list(Config) ->
    Algs = algs(),
    Test = fun(Alg) ->
		   _ = rand:seed(Alg),
		   interval_float_1(100000)
	   end,
    [Test(Alg) || Alg <- Algs],
    ok.

interval_float_1(0) -> ok;
interval_float_1(N) ->
    X = rand:uniform(),
    if
	0.0 < X, X < 1.0 ->
	    ok;
	true ->
	    io:format("X=~p 0<~p<1.0~n", [X,X]),
	    exit({X, rand:export_seed()})
    end,
    interval_float_1(N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check if exs64 algorithm generates the proper sequence.
reference(Config) when is_list(Config) ->
    [reference_1(Alg) || Alg <- algs()],
    ok.

reference_1(Alg) ->
    Refval  = reference_val(Alg),
    Testval = gen(Alg),
    case Refval =:= Testval of
        true -> ok;
        false ->
	    io:format("Failed: ~p~n",[Alg]),
	    io:format("Length ~p ~p~n",[length(Refval), length(Testval)]),
	    io:format("Head ~p ~p~n",[hd(Refval), hd(Testval)]),
	    ok
    end.

gen(Algo) ->
    Seed = case Algo of
	       exsplus -> %% Printed with orig 'C' code and this seed
		   rand:seed_s({exsplus, [12345678|12345678]});
	       exs64 -> %% Printed with orig 'C' code and this seed
		   rand:seed_s({exs64, 12345678});
	       exs1024 -> %% Printed with orig 'C' code and this seed
		   rand:seed_s({exs1024, {lists:duplicate(16, 12345678), []}});
	       _ ->
		   rand:seed(Algo, {100, 200, 300})
	   end,
    gen(?LOOP, Seed, []).

gen(N, State0 = {#{max:=Max}, _}, Acc) when N > 0 ->
    {Random, State} = rand:uniform_s(Max, State0),
    case N rem (?LOOP div 100) of
	0 -> gen(N-1, State, [Random|Acc]);
	_ -> gen(N-1, State, Acc)
    end;
gen(_, _, Acc) -> lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This just tests the basics so we have not made any serious errors
%% when making the conversion from the original algorithms.
%% The algorithms must have good properties to begin with
%%

%% Check that the algorithms generate sound values.

basic_stats_uniform_1(Config) when is_list(Config) ->
    ct:timetrap({minutes,6}), %% valgrind needs a lot of time
    [basic_uniform_1(?LOOP, rand:seed_s(Alg), 0.0, array:new([{default, 0}]))
     || Alg <- algs()],
    ok.

basic_stats_uniform_2(Config) when is_list(Config) ->
    ct:timetrap({minutes,6}), %% valgrind needs a lot of time
    [basic_uniform_2(?LOOP, rand:seed_s(Alg), 0, array:new([{default, 0}]))
     || Alg <- algs()],
    ok.

basic_stats_normal(Config) when is_list(Config) ->
    ct:timetrap({minutes,6}), %% valgrind needs a lot of time
    io:format("Testing normal~n",[]),
    [basic_normal_1(?LOOP, rand:seed_s(Alg), 0, 0) || Alg <- algs()],
    ok.

basic_uniform_1(N, S0, Sum, A0) when N > 0 ->
    {X,S} = rand:uniform_s(S0),
    I = trunc(X*100),
    A = array:set(I, 1+array:get(I,A0), A0),
    basic_uniform_1(N-1, S, Sum+X, A);
basic_uniform_1(0, {#{type:=Alg}, _}, Sum, A) ->
    AverN = Sum / ?LOOP,
    io:format("~.10w: Average: ~.4f~n", [Alg, AverN]),
    Counters = array:to_list(A),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.10w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(0.5 - AverN) < 0.005 orelse ct:fail({average, Alg, AverN}),
    abs(?LOOP div 100 - Min) < 1000 orelse ct:fail({min, Alg, Min}),
    abs(?LOOP div 100 - Max) < 1000 orelse ct:fail({max, Alg, Max}),
    ok.

basic_uniform_2(N, S0, Sum, A0) when N > 0 ->
    {X,S} = rand:uniform_s(100, S0),
    A = array:set(X-1, 1+array:get(X-1,A0), A0),
    basic_uniform_2(N-1, S, Sum+X, A);
basic_uniform_2(0, {#{type:=Alg}, _}, Sum, A) ->
    AverN = Sum / ?LOOP,
    io:format("~.10w: Average: ~.4f~n", [Alg, AverN]),
    Counters = tl(array:to_list(A)),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.10w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(50.5 - AverN) < 0.5 orelse ct:fail({average, Alg, AverN}),
    abs(?LOOP div 100 - Min) < 1000 orelse ct:fail({min, Alg, Min}),
    abs(?LOOP div 100 - Max) < 1000 orelse ct:fail({max, Alg, Max}),
    ok.

basic_normal_1(N, S0, Sum, Sq) when N > 0 ->
    {X,S} = rand:normal_s(S0),
    basic_normal_1(N-1, S, X+Sum, X*X+Sq);
basic_normal_1(0, {#{type:=Alg}, _}, Sum, SumSq) ->
    Mean = Sum / ?LOOP,
    StdDev =  math:sqrt((SumSq - (Sum*Sum/?LOOP))/(?LOOP - 1)),
    io:format("~.10w: Average: ~7.4f StdDev ~6.4f~n", [Alg, Mean, StdDev]),
    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(Mean) < 0.005 orelse ct:fail({average, Alg, Mean}),
    abs(StdDev - 1.0) < 0.005 orelse ct:fail({stddev, Alg, StdDev}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test that the user can write algorithms.
plugin(Config) when is_list(Config) ->
    _ = lists:foldl(fun(_, S0) ->
			    {V1, S1} = rand:uniform_s(10000, S0),
			    true = is_integer(V1),
			    {V2, S2} = rand:uniform_s(S1),
			    true = is_float(V2),
			    S2
		    end, crypto_seed(), lists:seq(1, 200)),
    ok.

%% Test implementation
crypto_seed() ->
    {#{type=>crypto,
       max=>(1 bsl 64)-1,
       next=>fun crypto_next/1,
       uniform=>fun crypto_uniform/1,
       uniform_n=>fun crypto_uniform_n/2},
     <<>>}.

%% Be fair and create bignums i.e. 64bits otherwise use 58bits
crypto_next(<<Num:64, Bin/binary>>) ->
    {Num, Bin};
crypto_next(_) ->
    crypto_next(crypto:strong_rand_bytes((64 div 8)*100)).

crypto_uniform({Api, Data0}) ->
    {Int, Data} = crypto_next(Data0),
    {Int / (1 bsl 64), {Api, Data}}.

crypto_uniform_n(N, {Api, Data0}) when N < (1 bsl 64) ->
    {Int, Data} = crypto_next(Data0),
    {(Int rem N)+1, {Api, Data}};
crypto_uniform_n(N, State0) ->
    {F,State} = crypto_uniform(State0),
    {trunc(F * N) + 1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Not a test but measures the time characteristics of the different algorithms
measure(Suite) when is_atom(Suite) -> [];
measure(_Config) ->
    ct:timetrap({minutes,6}), %% valgrind needs a lot of time
    Algos = [crypto64|algs()],
    io:format("RNG uniform integer performance~n",[]),
    _ = measure_1(random, fun(State) -> {int, random:uniform_s(10000, State)} end),
    _ = [measure_1(Algo, fun(State) -> {int, rand:uniform_s(10000, State)} end) || Algo <- Algos],
    io:format("RNG uniform float performance~n",[]),
    _ = measure_1(random, fun(State) -> {uniform, random:uniform_s(State)} end),
    _ = [measure_1(Algo, fun(State) -> {uniform, rand:uniform_s(State)} end) || Algo <- Algos],
    io:format("RNG normal float performance~n",[]),
    io:format("~.10w: not implemented (too few bits)~n", [random]),
    _ = [measure_1(Algo, fun(State) -> {normal, rand:normal_s(State)} end) || Algo <- Algos],
    ok.

measure_1(Algo, Gen) ->
    Parent = self(),
    Seed = fun(crypto64) -> crypto_seed();
	      (random) -> random:seed(os:timestamp()), get(random_seed);
	      (Alg) -> rand:seed_s(Alg)
	   end,

    Pid = spawn_link(fun() ->
			     Fun = fun() -> measure_2(?LOOP, Seed(Algo), Gen) end,
			     {Time, ok} = timer:tc(Fun),
			     io:format("~.10w: ~pµs~n", [Algo, Time]),
			     Parent ! {self(), ok},
			     normal
		     end),
    receive
	{Pid, Msg} -> Msg
    end.

measure_2(N, State0, Fun) when N > 0 ->
    case Fun(State0) of
	{int, {Random, State}}
	  when is_integer(Random), Random >= 1, Random =< 100000 ->
	    measure_2(N-1, State, Fun);
	{uniform, {Random, State}} when is_float(Random), Random > 0, Random < 1 ->
	    measure_2(N-1, State, Fun);
	{normal, {Random, State}} when is_float(Random) ->
	    measure_2(N-1, State, Fun);
	Res ->
	    exit({error, Res, State0})
    end;
measure_2(0, _, _) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data
reference_val(exs64) ->
    [16#3737ad0c703ff6c3,16#3868a78fe71adbbd,16#1f01b62b4338b605,16#50876a917437965f,
     16#b2edfe32a10e27fc,16#995924551d8ebae1,16#9f1e6b94e94e0b58,16#27ec029eb0e94f8e,
     16#bf654e6df7fe5c,16#b7d5ef7b79be65e3,16#4bdba4d1c159126b,16#a9c816fdc701292c,
     16#a377b6c89d85ac8b,16#7abb5cd0e5847a6,16#62666f1fc00a0a90,16#1edc3c3d255a8113,
     16#dfc764073767f18e,16#381783d577ca4e34,16#49693588c085ddcb,16#da6fcb16dd5163f3,
     16#e2357a703475b1b7,16#aaa84c4924b5985a,16#b8fe07bb2bac1e49,16#23973ac0160ff064,
     16#1afbc7b023f5d618,16#9f510f7b7caa2a0f,16#d5b0a57f7f5f1084,16#d8c49b66c5f99a29,
     16#e920ac3b598b5213,16#1090d7e27e7a7c76,16#81171917168ee74f,16#f08489a3eb6988e,
     16#396260c4f0b2ed46,16#4fd0a6a6caefd5b2,16#423dff07a3b888a,16#12718773ebd99987,
     16#e50991e540807cb,16#8cfa03bbaa6679d6,16#55bdf86dfbb92dbf,16#eb7145378cce74a8,
     16#71856c224c846595,16#20461588dae6e24d,16#c73b3e63ced74bac,16#775b11813dda0c78,
     16#91f358e51068ede0,16#399955ef36766bc2,16#4489ee072e8a38b1,16#ba77759d52321ca0,
     16#14f519eab5c53db8,16#1f754bd08e4f34c4,16#99e25ca29b2fcfeb,16#da11927c0d9837f8,
     16#1eeb0f87009f5a87,16#a7c444d3b0db1089,16#49c7fbf0714849ad,16#4f2b693e7f8265cb,
     16#80e1493cbaa8f256,16#186f345bcac2661e,16#330065ae0c698d26,16#5235ed0432c42e93,
     16#429792e31ddb10bb,16#8769054bb6533cff,16#1ab382483444201f,16#2216368786fc7b9,
     16#1efea1155216da0b,16#782dc868ba595452,16#2b80f6d159617f48,16#407fc35121b2fa1b,
     16#90e8be6e618873d1,16#40ad4ec92a8abf8e,16#34e2890f583f435,16#838c0aef0a5d8427,
     16#ed4238f4bd6cbcfa,16#7feed11f7a8bb9f0,16#2b0636a93e26c89d,16#481ad4bea5180646,
     16#673e5ad861afe1cc,16#298eeb519d69e74d,16#eb1dd06d168c856,16#4770651519ee7ef9,
     16#7456ebf1bcf608f1,16#d6200f6fbd61ce05,16#c0695dfab11ab6aa,16#5bff449249983843,
     16#7aba88471474c9ac,16#d7e9e4a21c989e91,16#c5e02ee67ccb7ce1,16#4ea8a3a912246153,
     16#f2e6db7c9ce4ec43,16#39498a95d46d2470,16#c5294fcb8cce8aa9,16#a918fe444719f3dc,
     16#98225f754762c0c0,16#f0721204f2cb43f5,16#b98e77b099d1f2d1,16#691d6f75aee3386,
     16#860c7b2354ec24fd,16#33e007bd0fbcb609,16#7170ae9c20fb3d0,16#31d46938fe383a60];

reference_val(exs1024) ->
    [16#9c61311d0d4a01fd,16#ce963ef5803b703e,16#545dcffb7b644e1a,16#edd56576a8d778d5,
     16#16bee799783c6b45,16#336f0b3caeb417fa,16#29291b8be26dedfa,16#1efed996d2e1b1a8,
     16#c5c04757bd2dadf9,16#11aa6d194009c616,16#ab2b3e82bdb38a91,16#5011ee46fd2609eb,
     16#766db7e5b701a9bb,16#d42cb2632c419f35,16#107c6a2667bf8557,16#3ffbf922cb306967,
     16#1e71e3d024ac5131,16#6fdb368ec67a5f06,16#b0d8e72e7aa6d1c1,16#e5705a02dae89e3b,
     16#9c24eb68c086a1d3,16#418de330f55f71f0,16#2917ddeb278bc8d2,16#aeba7fba67208f39,
     16#10ceaf40f6af1d8d,16#47a6d06811d33132,16#603a661d6caf720a,16#a28bd0c9bcdacb3c,
     16#f44754f006909762,16#6e25e8e67ccc43bc,16#174378ce374a549e,16#b5598ae9f57c4e50,
     16#ca85807fbcd51dd,16#1816e58d6c3cc32a,16#1b4d630d3c8e96a6,16#c19b1e92b4efc5bd,
     16#665597b20ddd721a,16#fdab4eb21b75c0ae,16#86a612dcfea0756c,16#8fc2da192f9a55f0,
     16#d7c954eb1af31b5,16#6f5ee45b1b80101b,16#ebe8ea4e5a67cbf5,16#1cb952026b4c1400,
     16#44e62caffe7452c0,16#b591d8f3e6d7cbcf,16#250303f8d77b6f81,16#8ef2199aae4c9b8d,
     16#a16baa37a14d7b89,16#c006e4d2b2da158b,16#e6ec7abd54c93b31,16#e6b0d79ae2ab6fa7,
     16#93e4b30e4ab7d4cd,16#42a01b6a4ef63033,16#9ab1e94fe94976e,16#426644e1de302a1f,
     16#8e58569192200139,16#744f014a090107c1,16#15d056801d467c6c,16#51bdad3a8c30225f,
     16#abfc61fb3104bd45,16#c610607122272df7,16#905e67c63116ebfc,16#1e4fd5f443bdc18,
     16#1945d1745bc55a4c,16#f7cd2b18989595bb,16#f0d273b2c646a038,16#ee9a6fdc6fd5d734,
     16#541a518bdb700518,16#6e67ab9a65361d76,16#bcfadc9bfe5b2e06,16#69fa334cf3c11496,
     16#9657df3e0395b631,16#fc0d0442160108ec,16#2ee538da7b1f7209,16#8b20c9fae50a5a9e,
     16#a971a4b5c2b3b6a,16#ff6241e32489438e,16#8fd6433f45255777,16#6e6c82f10818b0dc,
     16#59a8fad3f6af616b,16#7eac34f43f12221c,16#6e429ec2951723ec,16#9a65179767a45c37,
     16#a5f8127d1e6fdf35,16#932c50bc633d8d5c,16#f3bbea4e7ebecb8,16#efc3a2bbf6a8674,
     16#451644a99971cb6,16#cf70776d652c150d,16#c1fe0dcb87a25403,16#9523417132b2452e,
     16#8f98bc30d06b980e,16#bb4b288ecb8daa9a,16#59e54beb32f78045,16#f9ab1562456b9d66,
     16#6435f4130304a793,16#b4bb94c2002e1849,16#49a86d1e4bade982,16#457d63d60ed52b95];

reference_val(exsplus) ->
    [16#bc76c2e638db,16#15ede2ebb16c9fb,16#185ee2c27d6b88d,16#15d5ee9feafc3a5,
     16#1862e91dfce3e6b,16#2c9744b0fb69e46,16#78b21bc01cef6b,16#2d16a2fae6c76ba,
     16#13dfccb8ff86bce,16#1d9474c59e23f4d,16#d2f67dcd7f0dd6,16#2b6d489d51a0725,
     16#1fa52ef484861d8,16#1ae9e2a38f966d4,16#2264ab1e193acca,16#23bbca085039a05,
     16#2b6eea06a0af0e1,16#3ad47fa8866ea20,16#1ec2802d612d855,16#36c1982b134d50,
     16#296b6a23f5b75e0,16#c5eeb600a9875c,16#2a3fd51d735f9d4,16#56fafa3593a070,
     16#13e9d416ec0423e,16#28101a91b23e9dc,16#32e561eb55ce15a,16#94a7dbba66fe4a,
     16#2e1845043bcec1f,16#235f7513a1b5146,16#e37af1bf2d63cb,16#2048033824a1639,
     16#c255c750995f7,16#2c7542058e89ee3,16#204dfeefbdb62ba,16#f5a936ec63dd66,
     16#33b3b7dbbbd8b90,16#c4f0f79026ffe9,16#20ffee2d37aca13,16#2274f931716be2c,
     16#29b883902ba9df1,16#1a838cd5312717f,16#2edfc49ff3dc1d6,16#418145cbec84c2,
     16#d2d8f1a17d49f,16#d41637bfa4cc6f,16#24437e03a0f5df8,16#3d1d87919b94a90,
     16#20d6997b36769b6,16#16f9d7855cd87ca,16#821ef7e2a062a3,16#2c4d11dc4a2da70,
     16#24a3b27f56ed26b,16#144b23c8b97387a,16#34a2ced56930d12,16#21cc0544113a017,
     16#3e780771f634fb2,16#146c259c02e7e18,16#1d99e4cfad0ef1,16#fdf3dabefc6b3a,
     16#7d0806e4d12dfb,16#3e3ae3580532eae,16#2456544200fbd86,16#f83aad4e88db85,
     16#37c134779463b4d,16#21a20bf64b6e735,16#1c0585ac88b69f2,16#1b3fcea8dd30e56,
     16#334bc301aefd97,16#37066eb7e80a946,16#15a19a6331b570f,16#35e67fa43c3f7d0,
     16#152a4020145fb80,16#8d55139491dfbe,16#21d9cba585c059d,16#31475f363654635,
     16#2567b17acb7a104,16#39201be3a7681c5,16#6bc675fd26b601,16#334b93232b1b1e3,
     16#357c402cb732c6a,16#362e32efe4db46a,16#8edc7ae3da51e5,16#31573376785eac9,
     16#6c6145ffa1169d,16#18ec2c393d45359,16#1f1a5f256e7130c,16#131cc2f49b8004f,
     16#36f715a249f4ec2,16#1c27629826c50d3,16#914d9a6648726a,16#27f5bf5ce2301e8,
     16#3dd493b8012970f,16#be13bed1e00e5c,16#ceef033b74ae10,16#3da38c6a50abe03,
     16#15cbd1a421c7a8c,16#22794e3ec6ef3b1,16#26154d26e7ea99f,16#3a66681359a6ab6].
