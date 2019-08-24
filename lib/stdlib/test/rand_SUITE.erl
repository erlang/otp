%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
-compile({nowarn_deprecated_function,[{random,seed,1},
                                      {random,uniform_s,1},
                                      {random,uniform_s,2}]}).
-compile([export_all, nowarn_export_all]).

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
     {group, distr_stats},
     uniform_real_conv,
     plugin, measure,
     {group, reference_jump},
     short_jump
    ].

groups() ->
    [{basic_stats, [parallel],
      [basic_stats_uniform_1, basic_stats_uniform_2,
       basic_stats_standard_normal]},
     {distr_stats, [parallel],
      [stats_standard_normal_box_muller,
       stats_standard_normal_box_muller_2,
       stats_standard_normal]},
     {reference_jump, [parallel],
      [reference_jump_state, reference_jump_procdict]}].

group(basic_stats) ->
    %% valgrind needs a lot of time
    [{timetrap,{minutes,10}}];
group(distr_stats) ->
    %% valgrind needs a lot of time
    [{timetrap,{minutes,10}}];
group(reference_jump) ->
    %% valgrind needs a lot of time
    [{timetrap,{minutes,10}}].

%% A simple helper to test without test_server during dev
test() ->
    Tests = all(),
    lists:foreach(
      fun (Test) ->
              try
                  ok = ?MODULE:Test([]),
                  io:format("~p: ok~n", [Test])
              catch _:Reason:Stacktrace ->
                      io:format("Failed: ~p: ~p ~p~n",
                                [Test, Reason, Stacktrace])
              end
      end, Tests).

algs() ->
    [exsss, exrop, exsp, exs1024s, exs64, exsplus, exs1024, exro928ss].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test that seed and seed_s and export_seed/0 is working.
seed(Config) when is_list(Config) ->
    Algs = algs(),
    Test = fun(Alg) ->
		   try seed_1(Alg)
		   catch _:Reason:Stacktrace ->
			   ct:fail({Alg, Reason, Stacktrace})
		   end
	   end,
    [Test(Alg) || Alg <- Algs],
    ok.

seed_1(Alg) ->
    %% Check that uniform seeds automatically,
    _ = rand:uniform(),
    S00 = get(rand_seed),
    erase(),
    _ = rand:uniform_real(),
    false = S00 =:= get(rand_seed), %% hopefully

    %% Choosing algo and seed
    S0 = rand:seed(Alg, {0, 0, 0}),
    %% Check that (documented?) process_dict variable is correct
    S0 = get(rand_seed),
    S0 = rand:seed_s(Alg, {0, 0, 0}),
    %% Check that process_dict should not be used for seed_s functionality
    _ = rand:seed_s(Alg, 4711),
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
    Y = rand:uniform_real(),
    if
	0.0 =< X, X < 1.0, 0.0 < Y, Y < 1.0 ->
	    ok;
	true ->
	    io:format("X=~p 0.0=<~p<1.0~n", [X,X]),
	    io:format("Y=~p 0.0<~p<1.0~n", [Y,Y]),
	    exit({X, rand:export_seed()})
    end,
    interval_float_1(N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check if each algorithm generates the proper sequence.
reference(Config) when is_list(Config) ->
    [reference_1(Alg) || Alg <- algs()],
    ok.

reference_1(Alg) ->
    Refval = reference_val(Alg),
    if
	Refval =:= not_implemented -> Refval;
	true ->
	    case gen(Alg) of
		Refval ->
		    io:format("Ok: ~p~n",[Alg]),
                    ok;
		Testval ->
		    io:format("Failed: ~p~n",[Alg]),
		    io:format("Length ~p ~p~n",[length(Refval), length(Testval)]),
		    io:format("Head ~p ~p~n",[hd(Refval), hd(Testval)]),
                    show_wrong(Refval, Testval),
		    exit(wrong_value)
	    end
    end.

show_wrong([], []) ->
    ok;
show_wrong([H|T1], [H|T2]) ->
    show_wrong(T1, T2);
show_wrong([H1|_], [H2|_]) ->
    io:format("Wrong ~p ~p~n",[H1,H2]).


gen(Algo) ->
    State =
        if
            Algo =:= exs64 -> %% Printed with orig 'C' code and this seed
                rand:seed_s(exs64, [12345678]);
            Algo =:= exsplus; Algo =:= exsp; Algo =:= exrop; Algo =:= exsss ->
                %% Printed with orig 'C' code and this seed
                rand:seed_s(Algo, [12345678,12345678]);
            Algo =:= exs1024; Algo =:= exs1024s; Algo =:= exro928ss ->
                %% Printed with orig 'C' code and this seed
                rand:seed_s(Algo, lists:duplicate(16, 12345678));
            true ->
                rand:seed(Algo, {100, 200, 300})
        end,
    Max = range(State),
    gen(?LOOP, State, Max, []).

gen(N, State0, Max, Acc) when N > 0 ->
    {Random, State} = rand:uniform_s(Max, State0),
    case N rem (?LOOP div 100) of
	0 -> gen(N-1, State, Max, [Random|Acc]);
	_ -> gen(N-1, State, Max, Acc)
    end;
gen(_, _, _, Acc) -> lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This just tests the basics so we have not made any serious errors
%% when making the conversion from the original algorithms.
%% The algorithms must have good properties to begin with
%%

%% Check that the algorithms generate sound values.

basic_stats_uniform_1(Config) when is_list(Config) ->
    ct:timetrap({minutes,15}), %% valgrind needs a lot of time
    [basic_uniform_1(?LOOP, rand:seed_s(Alg), 0.0, array:new([{default, 0}]))
     || Alg <- algs()],
    ok.

basic_stats_uniform_2(Config) when is_list(Config) ->
    ct:timetrap({minutes,15}), %% valgrind needs a lot of time
    [basic_uniform_2(?LOOP, rand:seed_s(Alg), 0, array:new([{default, 0}]))
     || Alg <- algs()],
    ok.

basic_stats_standard_normal(Config) when is_list(Config) ->
    ct:timetrap({minutes,6}), %% valgrind needs a lot of time
    io:format("Testing standard normal~n",[]),
    IntendedMean = 0,
    IntendedVariance = 1,
    [basic_normal_1(?LOOP, IntendedMean, IntendedVariance,
                    rand:seed_s(Alg), 0, 0)
     || Alg <- algs()],
    ok.

basic_stats_normal(Config) when is_list(Config) ->
    IntendedMeans = [-1.0e6, -50, -math:pi(), -math:exp(-1),
                     0.12345678, math:exp(1), 100, 1.0e6],
    IntendedVariances = [1.0e-6, math:exp(-1), 1, math:pi(), 1.0e6],
    IntendedMeanVariancePairs =
        [{Mean, Variance} || Mean <- IntendedMeans,
                             Variance <- IntendedVariances],

    ct:timetrap({minutes, 6 * length(IntendedMeanVariancePairs)}), %% valgrind needs a lot of time
    lists:foreach(
      fun ({IntendedMean, IntendedVariance}) ->
              ct:pal(
                "Testing normal(~.2f, ~.2f)~n",
                [float(IntendedMean), float(IntendedVariance)]),
              [basic_normal_1(?LOOP, IntendedMean, IntendedVariance,
                              rand:seed_s(Alg), 0, 0)
               || Alg <- algs()]
      end,
      IntendedMeanVariancePairs).

basic_uniform_1(N, S0, Sum, A0) when N > 0 ->
    {X,S} =
        case N band 1 of
            0 ->
                rand:uniform_s(S0);
            1 ->
                rand:uniform_real_s(S0)
        end,
    I = trunc(X*100),
    A = array:set(I, 1+array:get(I,A0), A0),
    basic_uniform_1(N-1, S, Sum+X, A);
basic_uniform_1(0, {#{type:=Alg}, _}, Sum, A) ->
    AverN = Sum / ?LOOP,
    io:format("~.12w: Average: ~.4f~n", [Alg, AverN]),
    Counters = array:to_list(A),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.12w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

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
    io:format("~.12w: Average: ~.4f~n", [Alg, AverN]),
    Counters = tl(array:to_list(A)),
    Min = lists:min(Counters),
    Max = lists:max(Counters),
    io:format("~.12w: Min: ~p Max: ~p~n", [Alg, Min, Max]),

    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(50.5 - AverN) < 0.5 orelse ct:fail({average, Alg, AverN}),
    abs(?LOOP div 100 - Min) < 1000 orelse ct:fail({min, Alg, Min}),
    abs(?LOOP div 100 - Max) < 1000 orelse ct:fail({max, Alg, Max}),
    ok.

basic_normal_1(N, IntendedMean, IntendedVariance, S0, StandardSum, StandardSq) when N > 0 ->
    {X,S} = normal_s(IntendedMean, IntendedVariance, S0),
    % We now shape X into a standard normal distribution (in case it wasn't already)
    % in order to minimise the accumulated error on Sum / SumSq;
    % otherwise said error would prevent us of making a fair judgment on
    % the overall distribution when targeting large means and variances.
    StandardX = (X - IntendedMean) / math:sqrt(IntendedVariance),
    basic_normal_1(N-1, IntendedMean, IntendedVariance, S,
                   StandardX+StandardSum, StandardX*StandardX+StandardSq);
basic_normal_1(0, _IntendedMean, _IntendedVariance, {#{type:=Alg}, _}, StandardSum, StandardSumSq) ->
    StandardMean = StandardSum / ?LOOP,
    StandardVariance = (StandardSumSq - (StandardSum*StandardSum/?LOOP))/(?LOOP - 1),
    StandardStdDev =  math:sqrt(StandardVariance),
    io:format("~.12w: Standardised Average: ~7.4f, Standardised StdDev ~6.4f~n",
              [Alg, StandardMean, StandardStdDev]),
    %% Verify that the basic statistics are ok
    %% be gentle we don't want to see to many failing tests
    abs(StandardMean) < 0.005 orelse ct:fail({average, Alg, StandardMean}),
    abs(StandardStdDev - 1.0) < 0.005 orelse ct:fail({stddev, Alg, StandardStdDev}),
    ok.

normal_s(Mean, Variance, State0) when Mean == 0, Variance == 1 ->
    % Make sure we're also testing the standard normal interface
    rand:normal_s(State0);
normal_s(Mean, Variance, State0) ->
    rand:normal_s(Mean, Variance, State0).



-dialyzer({no_improper_lists, stats_standard_normal_box_muller/1}).
stats_standard_normal_box_muller(Config) when is_list(Config) ->
    try math:erfc(1.0) of
        _ ->
            TwoPi = 2.0 * math:pi(),
            NormalS =
                fun
                    ([S0]) ->
                        {U1, S1} = rand:uniform_real_s(S0),
                        R = math:sqrt(-2.0 * math:log(U1)),
                        {U2, S2} = rand:uniform_s(S1),
                        T = TwoPi * U2,
                        Z0 = R * math:cos(T),
                        Z1 = R * math:sin(T),
                        {Z0, [S2|Z1]};
                    ([S|Z]) ->
                        {Z, [S]}
                end,
            State = [rand:seed(exsss)],
            stats_standard_normal(NormalS, State, 3)
    catch error:_ ->
            {skip, "math:erfc/1 not supported"}
    end.

-dialyzer({no_improper_lists, stats_standard_normal_box_muller_2/1}).
stats_standard_normal_box_muller_2(Config) when is_list(Config) ->
    try math:erfc(1.0) of
        _ ->
            TwoPi = 2.0 * math:pi(),
            NormalS =
                fun
                    ([S0]) ->
                        {U0, S1} = rand:uniform_s(S0),
                        U1 = 1.0 - U0,
                        R = math:sqrt(-2.0 * math:log(U1)),
                        {U2, S2} = rand:uniform_s(S1),
                        T = TwoPi * U2,
                        Z0 = R * math:cos(T),
                        Z1 = R * math:sin(T),
                        {Z0, [S2|Z1]};
                    ([S|Z]) ->
                        {Z, [S]}
                end,
            State = [rand:seed(exsss)],
            stats_standard_normal(NormalS, State, 3)
    catch error:_ ->
            {skip, "math:erfc/1 not supported"}
    end.


stats_standard_normal(Config) when is_list(Config) ->
    Retries = 7,
    try math:erfc(1.0) of
        _ ->
            stats_standard_normal(
              fun rand:normal_s/1, rand:seed_s(exsss), Retries)
    catch error:_ ->
            {skip, "math:erfc/1 not supported"}
    end.
%%
stats_standard_normal(Fun, S, Retries) ->
%%%
%%% ct config:
%%% {rand_SUITE, [{stats_standard_normal,[{seconds, 8}, {std_devs, 4.0}]}]}.
%%%
    Seconds = ct:get_config({?MODULE, ?FUNCTION_NAME, seconds}, 8),
    StdDevs =
        ct:get_config(
          {?MODULE, ?FUNCTION_NAME, std_devs},
          4.0), % probability erfc(4.0/sqrt(2)) (1/15787) to fail a bucket
%%%
    ct:timetrap({seconds, Seconds + 120}),
    %% Buckets is chosen to get a range where the the probability to land
    %% in the top catch-all bucket is not vanishingly low, but with
    %% these values it is about 1/25 of the probability for the low bucket
    %% (closest to 0).
    %%
    %% Rounds is calculated so the expected value for the low
    %% bucket will be at least TargetHits.
    %%
    InvDelta = 512,
    Buckets = 4 * InvDelta, % 4 std devs range
    TargetHits = 1024,
    Sqrt2 = math:sqrt(2.0),
    W = InvDelta * Sqrt2,
    P0 = math:erf(1 / W),
    Rounds = TargetHits * ceil(1.0 / P0),
    Histogram = array:new({default, 0}),
    ct:pal(
      "Running standard normal test against ~w std devs for ~w seconds...",
      [StdDevs, Seconds]),
    StopTime = erlang:monotonic_time(second) + Seconds,
    {PositiveHistogram, NegativeHistogram, Outlier, TotalRounds, NewS} =
        stats_standard_normal(
          InvDelta, Buckets, Histogram, Histogram, 0.0,
          Fun, S, Rounds, StopTime, Rounds, 0),
    Precision = math:sqrt(TotalRounds * P0) / StdDevs,
    TopP = math:erfc(Buckets / W),
    TopPrecision = math:sqrt(TotalRounds * TopP) / StdDevs,
    OutlierProbability = math:erfc(Outlier / Sqrt2) * TotalRounds,
    InvOP = 1.0 / OutlierProbability,
    ct:pal(
      "Total rounds: ~w, tolerance: 1/~.2f..1/~.2f, "
      "outlier: ~.2f, probability 1/~.2f.",
      [TotalRounds, Precision, TopPrecision, Outlier, InvOP]),
    case
        {bucket_error, TotalRounds,
         check_histogram(
           W, TotalRounds, StdDevs, PositiveHistogram, Buckets),
         check_histogram(
           W, TotalRounds, StdDevs, NegativeHistogram, Buckets)}
    of
        {_, _, [], []} when InvOP < 100 ->
            {comment, {tp, TopPrecision, op, InvOP}};
        {_, _, [], []} ->
            %% If the probability for getting this Outlier is lower than
            %% 1/100, then this is fishy!
            stats_standard_normal(
              Fun, NewS, Retries, {outlier_fishy, InvOP});
        BucketErrors ->
            stats_standard_normal(
              Fun, NewS, Retries, BucketErrors)
    end.
%%
stats_standard_normal(Fun, S, Retries, Failure) ->
    case Retries - 1 of
        0 ->
            ct:fail(Failure);
        NewRetries ->
            ct:pal("Retry due to TC glitch: ~p", [Failure]),
            stats_standard_normal(Fun, S, NewRetries)
    end.
%%
stats_standard_normal(
  InvDelta, Buckets, PositiveHistogram, NegativeHistogram, Outlier,
  Fun, S, 0, StopTime, Rounds, TotalRounds) ->
    case erlang:monotonic_time(second) of
        Now when Now < StopTime ->
            stats_standard_normal(
              InvDelta, Buckets,
              PositiveHistogram, NegativeHistogram, Outlier,
              Fun, S, Rounds, StopTime, Rounds, TotalRounds + Rounds);
        _ ->
            {PositiveHistogram, NegativeHistogram,
             Outlier, TotalRounds + Rounds, S}
    end;
stats_standard_normal(
  InvDelta, Buckets, PositiveHistogram, NegativeHistogram, Outlier,
  Fun, S, Count, StopTime, Rounds, TotalRounds) ->
    case Fun(S) of
        {X, NewS} when 0.0 =< X ->
            Bucket = min(Buckets, floor(X * InvDelta)),
            stats_standard_normal(
              InvDelta, Buckets,
              increment_bucket(Bucket, PositiveHistogram),
              NegativeHistogram, max(Outlier, X),
              Fun, NewS, Count - 1, StopTime, Rounds, TotalRounds);
        {MinusX, NewS} ->
            X = -MinusX,
            Bucket = min(Buckets, floor(X * InvDelta)),
            stats_standard_normal(
              InvDelta, Buckets,
              PositiveHistogram,
              increment_bucket(Bucket, NegativeHistogram), max(Outlier, X),
              Fun, NewS, Count - 1, StopTime, Rounds, TotalRounds)
    end.

increment_bucket(Bucket, Array) ->
    array:set(Bucket, array:get(Bucket, Array) + 1, Array).

check_histogram(W, Rounds, StdDevs, Histogram, Buckets) ->
    TargetP = 0.5 * math:erfc(Buckets / W),
    P = 0.0,
    N = 0,
    check_histogram(
      W, Rounds, StdDevs, Histogram, TargetP,
      Buckets, Buckets, P, N).
%%
check_histogram(
  _W, _Rounds, _StdDevs, _Histogram, _TargetP,
  0, _PrevBucket, _PrevP, _PrevN) ->
    [];
check_histogram(
  W, Rounds, StdDevs, Histogram, TargetP,
  Bucket, PrevBucket, PrevP, PrevN) ->
    N = PrevN + array:get(Bucket, Histogram),
    P = 0.5 * math:erfc(Bucket / W),
    BucketP = P - PrevP,
    if
        BucketP < TargetP ->
            check_histogram(
              W, Rounds, StdDevs, Histogram, TargetP,
              Bucket - 1, PrevBucket, PrevP, N);
        true ->
            Exp = BucketP * Rounds,
            Var = Rounds * BucketP*(1.0 - BucketP),
            Threshold = StdDevs * math:sqrt(Var),
            LowerLimit = floor(Exp - Threshold),
            UpperLimit = ceil(Exp + Threshold),
            if
                N < LowerLimit; UpperLimit < N ->
                    [#{bucket => {Bucket, PrevBucket}, n => N,
                       lower => LowerLimit, upper => UpperLimit} |
                     check_histogram(
                       W, Rounds, StdDevs, Histogram, TargetP,
                       Bucket - 1, Bucket, P, 0)];
                true ->
                    check_histogram(
                      W, Rounds, StdDevs, Histogram, TargetP,
                      Bucket - 1, Bucket, P, 0)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% White box test of the conversion to float

uniform_real_conv(Config) when is_list(Config) ->
    [begin
%%         ct:pal("~13.16.0bx~3.16.0b: ~p~n", [M,E,Gen]),
         uniform_real_conv_check(M, E, Gen)
     end || {M, E, Gen} <- uniform_real_conv_data()],
    uniform_real_scan(0),
    uniform_real_scan(3).

uniform_real_conv_data() ->
    [{16#fffffffffffff,  -1, [16#3ffffffffffffff]},
     {16#fffffffffffff,  -1, [16#3ffffffffffffe0]},
     {16#ffffffffffffe,  -1, [16#3ffffffffffffdf]},
     %%
     {16#0000000000000,  -1, [16#200000000000000]},
     {16#fffffffffffff,  -2, [16#1ffffffffffffff]},
     {16#fffffffffffff,  -2, [16#1fffffffffffff0]},
     {16#ffffffffffffe,  -2, [16#1ffffffffffffef]},
     %%
     {16#0000000000000,  -2, [16#100000000000000]},
     {16#fffffffffffff,  -3, [16#0ffffffffffffff]},
     {16#fffffffffffff,  -3, [16#0fffffffffffff8]},
     {16#ffffffffffffe,  -3, [16#0fffffffffffff7]},
     %%
     {16#0000000000000,  -3, [16#080000000000000]},
     {16#fffffffffffff,  -4, [16#07fffffffffffff]},
     {16#fffffffffffff,  -4, [16#07ffffffffffffc]},
     {16#ffffffffffffe,  -4, [16#07ffffffffffffb]},
     %%
     {16#0000000000000,  -4, [16#040000000000000]},
     {16#fffffffffffff,  -5, [16#03fffffffffffff,16#3ffffffffffffff]},
     {16#fffffffffffff,  -5, [16#03ffffffffffffe,16#200000000000000]},
     {16#ffffffffffffe,  -5, [16#03fffffffffffff,16#1ffffffffffffff]},
     {16#ffffffffffffe,  -5, [16#03fffffffffffff,16#100000000000000]},
     %%
     {16#0000000000001, -56, [16#000000000000007,16#00000000000007f]},
     {16#0000000000001, -56, [16#000000000000004,16#000000000000040]},
     {16#0000000000000, -57, [16#000000000000003,16#20000000000001f]},
     {16#0000000000000, -57, [16#000000000000000,16#200000000000000]},
     {16#fffffffffffff, -58, [16#000000000000003,16#1ffffffffffffff]},
     {16#fffffffffffff, -58, [16#000000000000000,16#1fffffffffffff0]},
     {16#ffffffffffffe, -58, [16#000000000000000,16#1ffffffffffffef]},
     {16#ffffffffffffe, -58, [16#000000000000000,16#1ffffffffffffe0]},
     %%
     {16#0000000000000, -58, [16#000000000000000,16#10000000000000f]},
     {16#0000000000000, -58, [16#000000000000000,16#100000000000000]},
     {2#11001100000000000000000000000000000000000011000000011, % 53 bits
      -1022,
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 18 zeros
       2#1100110000000000000000000000000000000000001 bsl 2, % 43 bits
       2#1000000011 bsl (56-10+2)]}, % 10 bits
     {0, -1, % 0.5 after retry
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, % 18 zeros
       2#111111111111111111111111111111111111111111 bsl 2, % 42 bits - retry
       16#200000000000003]}]. % 0.5

-define(UNIFORM_REAL_SCAN_PATTERN, (16#19000000000009)). % 53 bits
-define(UNIFORM_REAL_SCAN_NUMBER, (1021)).

uniform_real_scan_template(K) ->
    <<0:?UNIFORM_REAL_SCAN_NUMBER,
      ?UNIFORM_REAL_SCAN_PATTERN:53,K:2,0:1>>.

uniform_real_scan(K) ->
    Templ = uniform_real_scan_template(K),
    N = ?UNIFORM_REAL_SCAN_NUMBER,
    uniform_real_scan(Templ, N, K).

uniform_real_scan(Templ, N, K) when 0 =< N ->
    <<_:N/bits,T/bits>> = Templ,
    Data = uniform_real_scan_data(T, K),
    uniform_real_conv_check(
      ?UNIFORM_REAL_SCAN_PATTERN, N - 1 - ?UNIFORM_REAL_SCAN_NUMBER, Data),
    uniform_real_scan(Templ, N - 1, K);
uniform_real_scan(_, _, _) ->
    ok.

uniform_real_scan_data(Templ, K) ->
    case Templ of
        <<X:56, T/bits>> ->
            B = rand:bc64(X),
            [(X bsl 2) bor K |
             if
                 53 =< B ->
                     [];
                 true ->
                     uniform_real_scan_data(T, K)
             end];
        _ ->
            <<X:56, _/bits>> = <<Templ/bits, 0:56>>,
            [(X bsl 2) bor K]
    end.

uniform_real_conv_check(M, E, Gen) ->
    <<F/float>> = <<0:1, (E + 16#3ff):11, M:52>>,
    try uniform_real_gen(Gen) of
        F -> F;
        FF ->
            ct:pal(
              "~s =/= ~s: ~s~n",
              [rand:float2str(FF), rand:float2str(F),
               [["16#",integer_to_list(G,16),$\s]||G<-Gen]]),
            ct:fail({neq, FF, F})
    catch
        Error:Reason:Stacktrace ->
            ct:pal(
              "~w:~p ~s: ~s~n",
              [Error, Reason, rand:float2str(F),
               [["16#",integer_to_list(G,16),$\s]||G<-Gen]]),
            ct:fail({Error, Reason, F, Stacktrace})
    end.


uniform_real_gen(Gen) ->
    State = rand_state(Gen),
    {F, {#{type := rand_SUITE_list},[]}} = rand:uniform_real_s(State),
    F.

uniform_gen(Range, Gen) ->
    State = rand_state(Gen),
    {N, {#{type := rand_SUITE_list},[]}} = rand:uniform_s(Range, State),
    N.

%% Loaded dice for white box tests
rand_state(Gen) ->
    {#{type => rand_SUITE_list, bits => 58, weak_low_bits => 1,
       next => fun ([H|T]) -> {H, T} end},
     Gen}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Test that the user can write algorithms.
plugin(Config) when is_list(Config) ->
    try crypto:strong_rand_bytes(1) of
        <<_>> ->
            _ = lists:foldl(
                  fun(_, S0) ->
                          {V1, S1} = rand:uniform_s(10000, S0),
                          true = is_integer(V1),
                          {V2, S2} = rand:uniform_s(S1),
                          true = is_float(V2),
                          S2
                  end, crypto64_seed(), lists:seq(1, 200)),
            ok
    catch
        error:low_entropy ->
            {skip,low_entropy};
        error:undef ->
            {skip,no_crypto}
    end.

%% Test implementation
crypto64_seed() ->
    {#{type=>crypto64,
       bits=>64,
       next=>fun crypto64_next/1,
       uniform=>fun crypto64_uniform/1,
       uniform_n=>fun crypto64_uniform_n/2},
     <<>>}.

%% Be fair and create bignums i.e. 64bits otherwise use 58bits
crypto64_next(<<Num:64, Bin/binary>>) ->
    {Num, Bin};
crypto64_next(_) ->
    crypto64_next(crypto:strong_rand_bytes((64 div 8)*100)).

crypto64_uniform({Api, Data0}) ->
    {Int, Data} = crypto64_next(Data0),
    {Int / (1 bsl 64), {Api, Data}}.

crypto64_uniform_n(N, {Api, Data0}) when N < (1 bsl 64) ->
    {Int, Data} = crypto64_next(Data0),
    {(Int rem N)+1, {Api, Data}};
crypto64_uniform_n(N, State0) ->
    {F,State} = crypto64_uniform(State0),
    {trunc(F * N) + 1, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Not a test but measures the time characteristics of the different algorithms
measure(Config) ->
    ct:timetrap({minutes,60}), %% valgrind needs a lot of time
    case ct:get_timetrap_info() of
        {_,{_,1}} -> % No scaling
            do_measure(Config);
        {_,{_,Scale}} ->
            {skip,{will_not_run_in_scaled_time,Scale}}
    end.

-define(CHECK_UNIFORM_RANGE(Gen, Range, X, St),
        case (Gen) of
            {(X), (St)} when is_integer(X), 1 =< (X), (X) =< (Range) ->
                St
        end).
-define(CHECK_UNIFORM(Gen, X, St),
        case (Gen) of
            {(X), (St)} when is_float(X), 0.0 =< (X), (X) < 1.0 ->
                St
        end).
-define(CHECK_UNIFORM_NZ(Gen, X, St),
        case (Gen) of
            {(X), (St)} when is_float(X), 0.0 < (X), (X) =< 1.0 ->
                St
        end).
-define(CHECK_NORMAL(Gen, X, St),
        case (Gen) of
            {(X), (St)} when is_float(X) ->
                St
        end).

do_measure(_Config) ->
    Algs =
        algs() ++
        try crypto:strong_rand_bytes(1) of
            <<_>> ->
		[crypto64, crypto_cache, crypto_aes, crypto]
        catch
            error:low_entropy -> [];
            error:undef -> []
        end,
    %%
    ct:pal("~nRNG uniform integer range 10000 performance~n",[]),
    _ =
        measure_1(
          fun (_) -> 10000 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer 32 bit performance~n",[]),
    _ =
        measure_1(
          fun (_) -> 1 bsl 32 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer half range performance~n",[]),
    _ =
        measure_1(
          fun (State) -> half_range(State) end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer half range + 1 performance~n",[]),
    _ =
        measure_1(
          fun (State) -> half_range(State) + 1 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer full range - 1 performance~n",[]),
    _ =
        measure_1(
          fun (State) -> (half_range(State) bsl 1) - 1 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer full range performance~n",[]),
    _ =
        measure_1(
          fun (State) -> half_range(State) bsl 1 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer full range + 1 performance~n",[]),
    _ =
        measure_1(
          fun (State) -> (half_range(State) bsl 1) + 1 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer double range performance~n",[]),
    _ =
        measure_1(
          fun (State) ->
                  half_range(State) bsl 2
          end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer double range + 1  performance~n",[]),
    _ =
        measure_1(
          fun (State) ->
                  (half_range(State) bsl 2) + 1
          end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform integer 64 bit performance~n",[]),
    _ =
        measure_1(
          fun (_) -> 1 bsl 64 end,
          fun (State, Range, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM_RANGE(
                               Mod:uniform_s(Range, St0), Range,
                               X, St1)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform float performance~n",[]),
    _ =
        measure_1(
          fun (_) -> 0 end,
          fun (State, _, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM(Mod:uniform_s(St0), X, St)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG uniform_real float performance~n",[]),
    _ =
        measure_1(
          fun (_) -> 0 end,
          fun (State, _, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_UNIFORM(Mod:uniform_real_s(St0), X, St)
                    end,
                    State)
          end,
          Algs),
    %%
    ct:pal("~nRNG normal float performance~n",[]),
    [TMarkNormalFloat|_] =
        measure_1(
          fun (_) -> 0 end,
          fun (State, _, Mod) ->
                  measure_loop(
                    fun (St0) ->
                            ?CHECK_NORMAL(Mod:normal_s(St0), X, St1)
                    end,
                    State)
          end,
          Algs),
    %% Just for fun try an implementation of the Box-Muller
    %% transformation for creating normal distribution floats
    %% to compare with our Ziggurat implementation.
    %% Generates two numbers per call that we add so they
    %% will not be optimized away.  Hence the benchmark time
    %% is twice what it should be.
    TwoPi = 2 * math:pi(),
    _ =
        measure_1(
          fun (_) -> 0 end,
          fun (State, _, Mod) ->
                  measure_loop(
                    fun (State0) ->
                            {U1, State1} = Mod:uniform_real_s(State0),
                            {U2, State2} = Mod:uniform_s(State1),
                            R = math:sqrt(-2.0 * math:log(U1)),
                            T = TwoPi * U2,
                            Z0 = R * math:cos(T),
                            Z1 = R * math:sin(T),
                            ?CHECK_NORMAL({Z0 + Z1, State2}, X, State3)
                    end,
                    State)
          end,
          exsss, TMarkNormalFloat),
    ok.

-define(LOOP_MEASURE, (?LOOP div 5)).

measure_loop(Fun, State) ->
    measure_loop(Fun, State, ?LOOP_MEASURE).
%%
measure_loop(Fun, State, N) when 0 < N ->
    measure_loop(Fun, Fun(State), N-1);
measure_loop(_, _, _) ->
    ok.

measure_1(RangeFun, Fun, Algs) ->
    TMark = measure_1(RangeFun, Fun, hd(Algs), undefined),
    [TMark] ++
        [measure_1(RangeFun, Fun, Alg, TMark) || Alg <- tl(Algs)].

measure_1(RangeFun, Fun, Alg, TMark) ->
    Parent = self(),
    {Mod, State} =
        case Alg of
            crypto64 ->
                {rand, crypto64_seed()};
            crypto_cache ->
                {rand, crypto:rand_seed_alg(crypto_cache)};
            crypto ->
                {rand, crypto:rand_seed_s()};
            crypto_aes ->
                {rand,
		 crypto:rand_seed_alg(
		   crypto_aes, crypto:strong_rand_bytes(256))};
            random ->
                {random, random:seed(os:timestamp()), get(random_seed)};
            _ ->
                {rand, rand:seed_s(Alg)}
        end,
    Range = RangeFun(State),
    Pid = spawn_link(
            fun() ->
                    {Time, ok} = timer:tc(fun () -> Fun(State, Range, Mod) end),
                    Percent =
                        case TMark of
                            undefined -> 100;
                            _ -> (Time * 100 + 50) div TMark
                        end,
                    io:format(
                      "~.20w: ~p ns ~p% [16#~.16b]~n",
                      [Alg, (Time * 1000 + 500) div ?LOOP_MEASURE,
                       Percent, Range]),
                    Parent ! {self(), Time},
                    normal
            end),
    receive
	{Pid, Msg} -> Msg
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The jump sequence tests has two parts
%% for those with the functional API (jump/1)
%% and for those with the internal state
%% in process dictionary (jump/0).

-define(LOOP_JUMP, (?LOOP div 1000)).

%% Check if each algorithm generates the proper jump sequence
%% with the functional API.
reference_jump_state(Config) when is_list(Config) ->
    [reference_jump_1(Alg) || Alg <- algs()],
    ok.

reference_jump_1(Alg) ->
    Refval = reference_jump_val(Alg),
    if
	Refval =:= not_implemented -> Refval;
	true ->
	    case gen_jump_1(Alg) of
		Refval -> ok;
		Testval ->
		    io:format(
		      "Failed: ~p~n",[Alg]),
		    io:format(
		      "Length ~p ~p~n",
		      [length(Refval), length(Testval)]),
		    io:format(
		      "Head ~p ~p~n",[hd(Refval), hd(Testval)]),
		    io:format(
		      "Vals ~p ~p~n",[Refval, Testval]),
		    exit(wrong_value)
	    end
    end.

gen_jump_1(Algo) ->
    case Algo of
	exs64 -> %% Test exception of not_implemented notice
	    try rand:jump(rand:seed_s(exs64))
	    catch
		error:not_implemented -> [error_not_implemented]
	    end;
	_ when Algo =:= exsplus; Algo =:= exsp; Algo =:= exrop; Algo =:= exsss ->
	    %% Printed with orig 'C' code and this seed
	    gen_jump_2(
	      rand:seed_s(Algo, [12345678,12345678]));
	_ when Algo =:= exs1024; Algo =:= exs1024s; Algo =:= exro928ss ->
	    %% Printed with orig 'C' code and this seed
	    gen_jump_2(
	      rand:seed_s(Algo, lists:duplicate(16, 12345678)))
    end.

gen_jump_2(State) ->
    Max = range(State),
    gen_jump_3(?LOOP_JUMP, State, Max, []).

gen_jump_3(N, State0, Max, Acc) when N > 0 ->
    {_, State1} = rand:uniform_s(Max, State0),
    {Random, State2} = rand:uniform_s(Max, rand:jump(State1)),
    case N rem (?LOOP_JUMP div 100) of
	0 -> gen_jump_3(N-1, State2, Max, [Random|Acc]);
	_ -> gen_jump_3(N-1, State2, Max, Acc)
    end;
gen_jump_3(_, _, _, Acc) -> lists:reverse(Acc).


%% Check if each algorithm generates the proper jump sequence
%% with the internal state in the process dictionary.
reference_jump_procdict(Config) when is_list(Config) ->
    [reference_jump_p1(Alg) || Alg <- algs()],
    ok.

reference_jump_p1(Alg) ->
    Refval  = reference_jump_val(Alg),
    if
	Refval =:= not_implemented -> Refval;
	true ->
	    case gen_jump_p1(Alg) of
		Refval -> ok;
		Testval ->
		    io:format("Failed: ~p~n",[Alg]),
		    io:format("Length ~p ~p~n",[length(Refval), length(Testval)]),
		    io:format("Head ~p ~p~n",[hd(Refval), hd(Testval)]),
		    exit(wrong_value)
	    end
    end.

gen_jump_p1(Algo) ->
    case Algo of
	exs64 -> %% Test exception of not_implemented notice
	    try
		_ = rand:seed(exs64),
		rand:jump()
	    catch
		error:not_implemented -> [error_not_implemented]
	    end;
	_ when Algo =:= exsplus; Algo =:= exsp; Algo =:= exrop; Algo =:= exsss ->
	    %% Printed with orig 'C' code and this seed
	    gen_jump_p2(
	      rand:seed(Algo, [12345678,12345678]));
	_ when Algo =:= exs1024; Algo =:= exs1024s; Algo =:= exro928ss ->
	    %% Printed with orig 'C' code and this seed
	    gen_jump_p2(
	      rand:seed(Algo, lists:duplicate(16, 12345678)))
    end.

gen_jump_p2(Seed) ->
    Max = range(Seed),
    gen_jump_p3(?LOOP_JUMP, Max, []).

gen_jump_p3(N, Max, Acc) when N > 0 ->
    _ = rand:uniform(Max),
    _ = rand:jump(),
    Random = rand:uniform(Max),
    case N rem (?LOOP_JUMP div 100) of
	0 -> gen_jump_p3(N-1, Max, [Random|Acc]);
	_ -> gen_jump_p3(N-1, Max, Acc)
    end;
gen_jump_p3(_, _, Acc) -> lists:reverse(Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

short_jump(Config) when is_list(Config) ->
    Seed = erlang:system_time(),
    short_jump(
      rand:seed_s(exro928ss, Seed),
      fun ({Alg,AlgState}) ->
	      {Alg,rand:exro928_jump_2pow20(AlgState)}
      end),
    short_jump(
      crypto:rand_seed_alg_s(crypto_aes, integer_to_list(Seed)),
      fun ({Alg,AlgState}) ->
	      {Alg,crypto:rand_plugin_aes_jump_2pow20(AlgState)}
      end),
    ok.

short_jump({#{bits := Bits},_} = State_0, Jump2Pow20) ->
    Range = 1 bsl Bits,
    State_1 = repeat(7, Range, State_0),
    %%
    State_2a = repeat(1 bsl 20, Range, State_1),
    State_2b = Jump2Pow20(State_1),
    check(17, Range, State_2a, State_2b),
    %%
    {_,State_3a} = rand:uniform_s(Range, State_2a),
    State_4a = Jump2Pow20(State_3a),
    State_4b = repeat((1 bsl 20) + 1, Range, State_2b),
    check(17, Range, State_4a, State_4b).

repeat(0, _Range, State) ->
    State;
repeat(N, Range, State) ->
    {_, NewState} = rand:uniform_s(Range, State),
    repeat(N - 1, Range, NewState).

check(0, _Range, _StateA, _StateB) ->
    ok;
check(N, Range, StateA, StateB) ->
    {V,NewStateA} = rand:uniform_s(Range, StateA),
    case rand:uniform_s(Range, StateB) of
	{V,NewStateB} ->
	    check(N - 1, Range, NewStateA, NewStateB);
	{Wrong,_} ->
	    ct:fail({Wrong,neq,V,for,N})
    end.

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
     16#15cbd1a421c7a8c,16#22794e3ec6ef3b1,16#26154d26e7ea99f,16#3a66681359a6ab6];

reference_val(exsss) ->
    [16#108e8d5b01,16#33b72092117209a,16#224d4d2961a2d0a,16#2c4c81aac3da48d,
     16#2f4bc39bfc36f3a,16#41826d4c4d243a,16#19871b8bb4e23ee,16#3e2112cdf9384b1,
     16#69801943bf91ab,16#2de1a603c31ec45,16#a90ca1991b831e,16#51ca29571a69a7,
     16#93ce3e511906cf,16#93ebc5768aef75,16#2412f284b902ae7,16#1ac10e758410c52,
     16#3f32494560368f6,16#39a5e82dcf0de95,16#3f4b14d59cc6a21,16#3174668db0b36ae,
     16#1449812fb8bd54e,16#eaca1f8ece51e1,16#2564b2545fd23c1,16#3cf3a2d2217e0d7,
     16#226f4164ba1d054,16#10dac9ae207ceef,16#17f2c4b2d40fcb9,16#1c1b282d386fdcb,
     16#a264f450ba2912,16#2a0a1dd67e52666,16#2be84eb835cb1e1,16#2a1cd9aa16ccc37,
     16#7dd5e8c2b3f490,16#254a3db4976c05b,16#2a0a67971ec1e63,16#13a0cbf7c0eed8a,
     16#3192d7edc0a20bc,16#2705ad756292e84,16#3ec429a18119c81,16#25944b38baa975b,
     16#291dcc43e3256f4,16#30d10b759237db,16#c1522a652058a,16#8ef1e9378381e6,
     16#1f442f33c2439f4,16#186087710a73818,16#12887f94b2b8387,16#3e42e8b1f3c9b4b,
     16#e462859d55f9d8,16#2356ae85be908de,16#15e96a927b3bc52,16#35c6dc52511ce46,
     16#7bc0624ce66e01,16#33ab7d95b738322,16#26f01effc182aa0,16#1b66ae7eaafea88,
     16#278f3dc14943b90,16#22178bc8d8faf28,16#396c37d53c11985,16#5e0d79d0b10f18,
     16#1be3de3b5675ec,16#d4db298f1f4b50,16#2da6cb99bb5c7b1,16#130b2dc17d03be8,
     16#f1847e7e059e9f,16#2da6591788326e7,16#222e4a18c24211c,16#949213ca49baab,
     16#b5129fec56f6a2,16#30f25f1e926f43e,16#1ddd8d04445fb4d,16#15995b542514150,
     16#1595fe879296296,16#e2f237a488453b,16#23e5cd2d6047890,16#3a5dc88fc954666,
     16#89bca9969b103,16#5e6893cd35dc63,16#1fed534feeeef5a,16#26f40e2147ee558,
     16#30c131a00625837,16#2618a7e617422e9,16#23630b297e45e7,16#1143b17502f3219,
     16#15607dac41168da,16#2886bdc314b3fb8,16#465d1cc1536546,16#30b09123e3a02e4,
     16#245a375f810be52,16#6a1b0792376a03,16#221425f59f2470f,16#867ce16dfac81c,
     16#9c62d95fae9b58,16#380381db1394426,16#34908dedc01c324,16#1f0ff517089b561,
     16#1571366dd873d32,16#3ee353dc56e192,16#15a1dee8d889b11,16#41036ad76d9888
    ];

reference_val(exsp) ->
    reference_val(exsplus);
reference_val(exs1024s) ->
    reference_val(exs1024);
reference_val(exrop) ->
%% #include <stdint.h>
%% #include <stdio.h>
%%
%% uint64_t s[2];
%% uint64_t next(void);
%% /* Xoroshiro116+ PRNG here */
%%
%% int main(char *argv[]) {
%%     int n;
%%     uint64_t r;
%%     s[0] = 12345678;
%%     s[1] = 12345678;
%%
%%     for (n = 1000000;  n > 0;  n--) {
%%         r = next();
%%         if ((n % 10000) == 0) {
%%             printf("%llu,", (unsigned long long) (r + 1));
%%         }
%%     }
%%     printf("\n");
%% }
    [24691357,29089185972758626,135434857127264790,
     277209758236304485,101045429972817342,
     241950202080388093,283018380268425711,268233672110762489,
     173241488791227202,245038518481669421,
     253627577363613736,234979870724373477,115607127954560275,
     96445882796968228,166106849348423677,
     83614184550774836,109634510785746957,68415533259662436,
     12078288820568786,246413981014863011,
     96953486962147513,138629231038332640,206078430370986460,
     11002780552565714,238837272913629203,
     60272901610411077,148828243883348685,203140738399788939,
     131001610760610046,30717739120305678,
     262903815608472425,31891125663924935,107252017522511256,
     241577109487224033,263801934853180827,
     155517416581881714,223609336630639997,112175917931581716,
     16523497284706825,201453767973653420,
     35912153101632769,211525452750005043,96678037860996922,
     70962216125870068,107383886372877124,
     223441708670831233,247351119445661499,233235283318278995,
     280646255087307741,232948506631162445,
     %%
     117394974124526779,55395923845250321,274512622756597759,
     31754154862553492,222645458401498438,
     161643932692872858,11771755227312868,93933211280589745,
     92242631276348831,197206910466548143,
     150370169849735808,229903773212075765,264650708561842793,
     30318996509793571,158249985447105184,
     220423733894955738,62892844479829080,112941952955911674,
     203157000073363030,54175707830615686,
     50121351829191185,115891831802446962,62298417197154985,
     6569598473421167,69822368618978464,
     176271134892968134,160793729023716344,271997399244980560,
     59100661824817999,150500611720118722,
     23707133151561128,25156834940231911,257788052162304719,
     176517852966055005,247173855600850875,
     83440973524473396,94711136045581604,154881198769946042,
     236537934330658377,152283781345006019,
     250789092615679985,78848633178610658,72059442721196128,
     98223942961505519,191144652663779840,
     102425686803727694,89058927716079076,80721467542933080,
     8462479817391645,2774921106204163];

reference_val(exro928ss) ->
%% Same as for exrop, but this state init:
%%     for (n = 0;  n < 16;  n++) {
%%         s[n] = 12345678;
    [16#000000108e8d5b01,16#03604028f2769dff,16#007f92f60bc7170c,
     16#035ea81a9898a5e2,16#0104c90c5a0c8178,16#0313514025cca717,
     16#03c5506b2a2e98cf,16#0098a5405961552e,16#004ad29eabb785a0,
     16#033ea8ec4efb8058,16#00b21545e62bef1c,16#0333fc5320703482,
     16#02c3c650e51a8d47,16#03a3b7fc848c9cda,16#03775adea6cddff5,
     16#01ae5499c9049973,16#03d3c90e5504e16b,16#0383cd6b6cb852e6,
     16#009c8d0996ef543a,16#0059cf671371af60,16#03dfd68ed980b719,
     16#0290f2a0acf2c5b0,16#029061df18d63b55,16#02e702ea4b45137b,
     16#029a0ccca604d848,16#01664c7cd31f0fa6,16#00dced83e60ccddc,
     16#008764d2c9a05f3e,16#02b9ca5f6a80c4ba,16#02daf93d2c566750,
     16#0147d326ead18ace,16#014b452efc19297f,16#0242d3f7a7237eca,
     16#0141bb68c2abce39,16#02d798e1230baf45,16#0216bf8f25c1ec2d,
     16#003a43ea733f1e1f,16#036c75390db736f3,16#028cca5f5f48c6f9,
     16#0186e4a17174d6cf,16#02152679dfa4c25c,16#01429b9f15e3b9d6,
     16#0134a61411d22bb0,16#01593f7d970d1c94,16#0205a7d8a305490f,
     16#01dd092272595a9c,16#0028c95208aad2d4,16#016347c25cc24162,
     16#025306acfb891309,16#0207a07e2bebef2f,16#024ee78d86ff5288,
     16#030b53192db97613,16#03f765cb9e98e611,16#025ec35a1e237377,
     16#03d81fd73102ef6f,16#0242dc8fea9a68b2,16#00abb876c1d4ea1b,
     16#00871ffd2b7e45fb,16#03593ff73c9be08d,16#00b96b2b8aca3688,
     16#0174aba957b7cf7b,16#012b7a5d4cf4a5b7,16#032a5260f2123db8,
     16#00f9374d88ee0080,16#030df39bec2ad657,16#00dce0cb81d006c4,
     16#038213b806303c76,16#03940aafdbfabf84,16#0398dbb26aeba037,
     16#01eb28d61951587f,16#00fed3d2aacfeef4,16#03499587547d6e40,
     16#01b192fe6e979e3c,16#00e974bf5f0a26d0,16#012ed94f76459c83,
     16#02d76859e7a82587,16#00d1d2c7b791f51b,16#03988058017a031b,
     16#00bbcf4b59d8e86d,16#015ed8b73a1b767c,16#0277283ea6a5ee74,
     16#002211460dd6d422,16#001ad62761ee9fbd,16#037311b44518b067,
     16#02b5ed61bf70904e,16#011862a05c1929fa,16#014be68683c3bab4,
     16#025c29aa5c508b07,16#00895c6106f97378,16#026ce91a3d671c7f,
     16#02591f4c74784293,16#02f0ed2a70bc1853,16#00a2762ff614bfbc,
     16#008f4e354f0c20d4,16#038b66fb587ed430,16#00636296e188de89,
     16#0278fadd143e74f5,16#029697ccf1b3a4c2,16#011eccb273404458,
     16#03f204064a9fe0c0];

reference_val(_) ->
    not_implemented.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reference_jump_val(exsplus) ->
    [82445318862816932, 145810727464480743, 16514517716894509, 247642377064868650,
     162385642339156908, 251810707075252101, 82288275771998924, 234412731596926322,
     49960883129071044, 200690077681656596, 213743196668671647, 131182800982967108,
     144200072021941728, 263557425008503277, 194858522616874272, 185869394820993172,
     80384502675241453, 262654144824057588, 90033295011291362, 4494510449302659,
     226005372746479588, 116780561309220553, 47048528594475843, 39168929349768743,
     139615163424415552, 55330632656603925, 237575574720486569, 102381140288455025,
     18452933910354323, 150248612130579752, 269358096791922740, 61313433522002187,
     160327361842676597, 185187983548528938, 57378981505594193, 167510799293984067,
     105117045862954303, 176126685946302943, 123590876906828803, 69185336947273487,
     9098689247665808, 49906154674145057, 131575138412788650, 161843880211677185,
     30743946051071186, 187578920583823612, 45008401528636978, 122454158686456658,
     111195992644229524, 17962783958752862, 13579507636941108, 130137843317798663,
     144202635170576832, 132539563255093922, 159785575703967124, 187241848364816640,
     183044737781926478, 12921559769912263, 83553932242922001, 96698298841984688,
     281664320227537824, 224233030818578263, 77812932110318774, 169729351013291728,
     164475402723178734, 242780633011249051, 51095111179609125, 19249189591963554,
     221412426221439180, 265700202856282653, 265342254311932308, 241218503498385511,
     255400887248486575, 212083616929812076, 227947034485840579, 268261881651571692,
     104846262373404908, 49690734329496661, 213259196633566308, 186966479726202436,
     282157378232384574, 11272948584603747, 166540426999573480, 50628164001018755,
     65235580992800860, 230664399047956956, 64575592354687978, 40519393736078511,
     108341851194332747, 115426411532008961, 120656817002338193, 234537867870809797,
     12504080415362731, 45083100453836317, 270968267812126657, 93505647407734103,
     252852934678537969, 258758309277167202, 74250882143432077, 141629095984552833];

reference_jump_val(exsss) ->
    [16#304ae783d40db2b,16#1dfb196b3a5600a,16#2a24116effc6a0d,16#1f138d68c56725,
     16#9360a445e2f989,16#32ed8080390e242,16#294ca85a270cff6,16#1418e6296a88bf,
     16#114fae3dc578ba7,16#479c42c760eb72,16#334a40655df22d6,16#e7a85dd4d37d72,
     16#181db16c8925c77,16#1b8a5a8afd16cbd,16#329107bf9777a39,16#2fc915c08535e42,
     16#16696d142c6078,16#2e2a2601c919448,16#2246150d1000568,16#26109007cb3dd44,
     16#3761360723e3175,16#169abd352db74de,16#1c97d520983684f,16#12455f0adee8c66,
     16#46719cff00622d,16#1fc92792ed4e437,16#18e2edae21affb5,16#3a67fa9e3e7d46e,
     16#1313fdc2728aa74,16#1c1a2b577581db8,16#db49357ea196b1,16#10e219a21d93fc7,
     16#3c43abede083666,16#3eef5055a58bbf9,16#1975056f95d90e3,16#3916c133ab16d87,
     16#2bc0bea891c26f1,16#391e4b369fc6b36,16#183f83155a359f6,16#1d9f137e9d2e488,
     16#ef084de5f4cd3c,16#36a9cf7e29e55d3,16#19eca704e0409a7,16#1bdb99902896c69,
     16#21777e2ad128203,16#5d0369ec0563e4,16#36db40b863bd74a,16#33feb71b7515159,
     16#208d923ce26f257,16#3841b32891c082d,16#2748f224c2ba226,16#2fcd93b2daf79bb,
     16#2c8e6cacad58ec4,16#39850131a1a85f,16#134648d6eea624d,16#2e102e197d5725c,
     16#12ac280fa744758,16#1c18266c7442d16,16#22b5f91b15fe17e,16#316740ca870f7c8,
     16#720ed4836c426,16#1aac0f738d04f8c,16#34fcd2a647b462c,16#3d430ac755114a3,
     16#3692e3670fdf2a,16#265279ab0fc0a15,16#10bd883dee80945,16#10e7843413175e4,
     16#b291deba08cee2,16#3915a8234caf11,16#34b911b96707dbd,16#ae63fcda15fde6,
     16#b13b9091e82e41,16#29de1b6d70dc04f,16#23fbcbc409617e8,16#1389a0738061066,
     16#360f39af790f5d1,16#f436da2a7d12f5,16#2d06ba8da21e08,16#3601a6492b887d,
     16#2b2590b8c6cc186,16#f8d613b6904464,16#e5456786e46b78,16#201b8b1f96ed80c,
     16#1b75b86d9b843f2,16#2e8bfaa7243a630,16#125ff068a78c3b4,16#3875a28c48bd26e,
     16#f09a06941fc9d7,16#107c4de8ca77744,16#357c34144bb9ed6,16#3ccc55d3ebb3378,
     16#28db7cea7d3fdee,16#3197fd0b49f6370,16#11af6fedb708ea6,16#2bde0382e37469e,
     16#10666171abddb3f,16#1a8876c1f4e78a8,16#169c0efd4422043,16#1501c49abf0440f];

reference_jump_val(exs1024) ->
    [2655961906500790629, 17003395417078685063, 10466831598958356428, 7603399148503548021,
     1650550950190587188, 12294992315080723704, 15743995773860389219, 5492181000145247327,
     14118165228742583601, 1024386975263610703, 10124872895886669513, 6445624517813169301,
     6238575554686562601, 14108646153524288915, 11804141635807832816, 8421575378006186238,
     6354993374304550369, 838493020029548163, 14759355804308819469, 12212491527912522022,
     16943204735100571602, 198964074252287588, 7325922870779721649, 15853102065526570574,
     16294058349151823341, 6153379962047409781, 15874031679495957261, 17299265255608442340,
     984658421210027171, 17408042033939375278, 3326465916992232353, 5222817718770538733,
     13262385796795170510, 15648751121811336061, 6718721549566546451, 7353765235619801875,
     16110995049882478788, 14559143407227563441, 4189805181268804683, 10938587948346538224,
     1635025506014383478, 12619562911869525411, 17469465615861488695, 125252234176411528,
     2004192558503448853, 13175467866790974840, 17712272336167363518, 1710549840100880318,
     17486892343528340916, 5337910082227550967, 8333082060923612691, 6284787745504163856,
     8072221024586708290, 6077032673910717705, 11495200863352251610, 11722792537523099594,
     14642059504258647996, 8595733246938141113, 17223366528010341891, 17447739753327015776,
     6149800490736735996, 11155866914574313276, 7123864553063709909, 15982886296520662323,
     5775920250955521517, 8624640108274906072, 8652974210855988961, 8715770416136907275,
     11841689528820039868, 10991309078149220415, 11758038663970841716, 7308750055935299261,
     15939068400245256963, 6920341533033919644, 8017706063646646166, 15814376391419160498,
     13529376573221932937, 16749061963269842448, 14639730709921425830, 3265850480169354066,
     4569394597532719321, 16594515239012200038, 13372824240764466517, 16892840440503406128,
     11260004846380394643, 2441660009097834955, 10566922722880085440, 11463315545387550692,
     5252492021914937692, 10404636333478845345, 11109538423683960387, 5525267334484537655,
     17936751184378118743, 4224632875737239207, 15888641556987476199, 9586888813112229805,
     9476861567287505094, 14909536929239540332, 17996844556292992842, 2699310519182298856];

reference_jump_val(exsp) ->
    reference_jump_val(exsplus);
reference_jump_val(exs1024s) ->
    reference_jump_val(exs1024);
reference_jump_val(exs64) -> [error_not_implemented];
reference_jump_val(exrop) ->
%% #include <stdint.h>
%% #include <stdio.h>
%%
%% uint64_t s[2];
%% uint64_t next(void);
%% /* Xoroshiro116+ PRNG here */
%%
%% int main(char *argv[]) {
%%     int n;
%%     uint64_t r;
%%     s[0] = 12345678;
%%     s[1] = 12345678;

%%     for (n = 1000;  n > 0;  n--) {
%%         next();
%%         jump();
%%         r = next();
%%         if ((n % 10) == 0) {
%%             printf("%llu,", (unsigned long long) (r + 1));
%%         }
%%     }
%%     printf("\n");
%% }
    [60301713907476001,135397949584721850,4148159712710727,
     110297784509908316,18753463199438866,
     106699913259182846,2414728156662676,237591345910610406,
     48519427605486503,38071665570452612,
     235484041375354592,45428997361037927,112352324717959775,
     226084403445232507,270797890380258829,
     160587966336947922,80453153271416820,222758573634013699,
     195715386237881435,240975253876429810,
     93387593470886224,23845439014202236,235376123357642262,
     22286175195310374,239068556844083490,
     120126027410954482,250690865061862527,113265144383673111,
     57986825640269127,206087920253971490,
     265971029949338955,40654558754415167,185972161822891882,
     72224917962819036,116613804322063968,
     129103518989198416,236110607653724474,98446977363728314,
     122264213760984600,55635665885245081,
     42625530794327559,288031254029912894,81654312180555835,
     261800844953573559,144734008151358432,
     77095621402920587,286730580569820386,274596992060316466,
     97977034409404188,5517946553518132,
     %%
     56460292644964432,252118572460428657,38694442746260303,
     165653145330192194,136968555571402812,
     64905200201714082,257386366768713186,22702362175273017,
     208480936480037395,152926769756967697,
     256751159334239189,130982960476845557,21613531985982870,
     87016962652282927,130446710536726404,
     188769410109327420,282891129440391928,251807515151187951,
     262029034126352975,30694713572208714,
     46430187445005589,176983177204884508,144190360369444480,
     14245137612606100,126045457407279122,
     169277107135012393,42599413368851184,130940158341360014,
     113412693367677211,119353175256553456,
     96339829771832349,17378172025472134,110141940813943768,
     253735613682893347,234964721082540068,
     85668779779185140,164542570671430062,18205512302089755,
     282380693509970845,190996054681051049,
     250227633882474729,171181147785250210,55437891969696407,
     241227318715885854,77323084015890802,
     1663590009695191,234064400749487599,222983191707424780,
     254956809144783896,203898972156838252];

reference_jump_val(exro928ss) ->
%% Same as for exrop, but this state init:
%%     for (n = 0;  n < 16;  n++) {
%%         s[n] = 12345678;
    [16#031ee449e53b6689,16#001afeee12813137,16#005e2172711df36b,
     16#02850aea3a595d36,16#0029705187e891c7,16#001794badd489667,
     16#00ab621be15be56c,16#024b663a6924786b,16#03cab70b8ab854bf,
     16#01daa37601285320,16#02db955a53c40e89,16#01fbef51d5c65891,
     16#02fecf4116ed5f77,16#0349c2057246ac5d,16#01217f257c4fa148,
     16#0367ee84d020697d,16#01d5cf647fe23335,16#020941838adfb750,
     16#02c2da26b1d7b3e5,16#00d1583d34cea6c0,16#038be9cb5b527f50,
     16#00bfa93c1d7f4864,16#03778912a4f56b14,16#037fcabc483fa5c5,
     16#00a3c9de6aaf5fc7,16#03600b883b2f2b42,16#03797a99ffddfdfb,
     16#0189fead429945b7,16#0103ac90cd912508,16#03e3d872fd950d64,
     16#0214fc3e77dc2f02,16#02a084f4f0e580ca,16#035d2fe72266a7f3,
     16#02887c49ae7e41a4,16#0011dc026af83c51,16#02d28bfd32c2c517,
     16#022e4165c33ad4f3,16#01f053cf0687b052,16#035315e6e53c8918,
     16#01255312da07b572,16#0237f1da11ec9221,16#02faf2e282fb1fb1,
     16#0227423ec1787ebc,16#011fa5eb1505571c,16#0275ff9eaaa1abdd,
     16#03e2d032c3981cb4,16#0181bb32d51d3072,16#01b1d3939b9f16ec,
     16#0259f09f55d1112f,16#0396464a2767e428,16#039777c0368bdb9e,
     16#0320925f35f36c5f,16#02a35289e0af1248,16#02e80bd4bc72254b,
     16#00a8b11af1674d68,16#027735036100a69e,16#03c8c268ded7f254,
     16#03de80aa57c65217,16#00f2247754d24000,16#005582a42b467f89,
     16#0031906569729477,16#00fd523f2ca4fefe,16#00ad223113d1e336,
     16#0238ddf026cbfca9,16#028b98211cfed876,16#0354353ebcc0de9a,
     16#009ee370c1e154f4,16#033131af3b8a7f88,16#032291baa45801e3,
     16#00941fc2b45eb217,16#035d6a61fa101647,16#03fdb51f736f1bbc,
     16#0232f7b98539faa0,16#0311b35319e3a61e,16#0048356b17860eb5,
     16#01a205b2554ce71e,16#03f873ea136e29d6,16#003c67d5c3df5ffd,
     16#00cd19e7a8641648,16#0149a8c54e4ba45e,16#0329498d134d2f6a,
     16#03b69421ae65ee2b,16#01a8d20b59447429,16#006b2292571032a2,
     16#00c193b17da22ba5,16#01faa7ab62181249,16#00acd401cd596a00,
     16#005b5086c3531402,16#0259113d5d3d058d,16#00bef3f3ce4a43b2,
     16#014837a4070b893c,16#00460a26ac2eeec1,16#026219a8b8c63d7e,
     16#03c7b8ed032cf5a6,16#004da912a1fff131,16#0297de3716215741,
     16#0079fb9b4c715466,16#00a73bad4ae5a356,16#0072e606c0d4ab86,
     16#02374382d5f9bd2e];

reference_jump_val(_) ->
    not_implemented.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The old algorithms used a range 2^N - 1 for their reference val
%% tests, which was incorrect but works as long as you do not draw
%% the value 2^N, which is very unlikely.  It was not possible
%% to simply correct the range to 2^N due to another incorrectness
%% in that the old algorithms changed to using the broken
%% (multiply a float approach with too few bits) approach for
%% ranges >= 2^N.  This function digs out the range to use
%% for the reference tests for old and new algorithms.
range({#{bits:=Bits}, _}) -> 1 bsl Bits;
range({#{max:=Max}, _}) -> Max; %% Old incorrect range
range({_, _, _}) -> 51. % random


half_range({#{bits:=Bits}, _}) -> 1 bsl (Bits - 1);
half_range({#{max:=Max}, _}) -> (Max bsr 1) + 1;
half_range({#{}, _}) -> 1 bsl 63; % crypto
half_range({_, _, _}) -> 1 bsl 50. % random
