-module(stress).
-export([
	 mklist/1, mktuple/1, mkfunny/1, mkcls/1,
	 mkimfunny1/1, mkimfunny2/1, mkimfunny3/1, mkimfunny4/1, mkimfunny5/1,
	 bench/0, bench/1, bench/2, term_bench/1, regression/2, timer/2
	 ]).


% Functions that create various data structures without sharing

mklist(0) -> 0;
mklist(M) -> X1 = mklist(M-1), X2 = mklist(M-1), [X1, X2].

mktuple(0) -> 0;
mktuple(M) -> X1 = mktuple(M-1), X2 = mktuple(M-1), {X1, X2}.

mkfunny(0) -> [];
mkfunny(M) -> [mktuple(M div 2) | mkfunny(M-1)].

mkcls(0) -> 42;
mkcls(M) -> X1 = mkcls(3*M div 7), X2 = mkcls(4*M div 7),
            X3 = mkcls(5*M div 7), X4 = mkcls(2*M div 7),
            X5 = mkcls(M-1),
            F = fun (N) -> [N, X1, M, X2] end, {X3, F, [M, X4, M | X5]}.

mkimfunny1(0) -> 42;
mkimfunny1(M) -> X1 = mkimfunny1(3*M div 4), X2 = mkimfunny1(2*M div 3),
                 [X1, X2 | mkimfunny1(M-1)].

mkimfunny2(0) -> 42;
mkimfunny2(M) -> X1 = mkimfunny2(2*M div 3), X2 = mktuple(3*M div 4),
                 [X1, X2 | mkimfunny2(M-1)].

mkimfunny3(0) -> 42;
mkimfunny3(M) -> X1 = mktuple(2*M div 7), Y1 = mkimfunny3(M-1),
                 X2 = mktuple(5*M div 7), Y2 = mkimfunny3(M-1),
                 [Y1, X1, Y2 | X2].

mkimfunny4(0) -> {42};
mkimfunny4(M) -> X = mkimfunny4(M-1), [M | X].

mkimfunny5(0) -> {42};
mkimfunny5(M) -> X = mkimfunny5(M-1), Y = mkimfunny5(5*M div 6),
                 case prime(M) of
                     false -> [Y | X];
                     true  -> {M, X}
                 end.

prime(N) when N < 2 -> false;
prime(N) when N =< 3 -> true;
prime(N) when (N rem 6 =:= 1) orelse (N rem 6 =:= 5) -> prime_chk(N, 5);
prime(_) -> false.

prime_chk(N, I) when I*I > N -> true;
prime_chk(N, I) when N rem I =:= 0 -> false;
prime_chk(N, I) when I rem 6 =:= 1 -> prime_chk(N, I+4);
prime_chk(N, I) -> prime_chk(N, I+2).


% Machinery for benchmarking

bench() -> bench(0).
bench(N) -> run(N, all_tests()).
bench(From, To) -> run(0, lists:sublist(all_tests(), From, To-From+1)).

run(0, [])    -> ok;
run(0, [X|L]) -> timer(X), run(0, L);
run(1, [X|_]) -> timer(X);
run(N, [_|L]) -> run(N-1, L).

timer({N, X}) -> timer(N, X).

timer(N, X) ->
    io:format("Copying ~p, times ~w, ", [X, N]),
    Opts = [],
    %Opts = [{min_heap_size, 100000000}],
    Parent = self(),
    Worker = fun () -> T = the_test(X),
                       Size = erts_debug:flat_size(T),
                       Stats = timer_stats(15, N, T),
                       Parent ! {Stats, Size}
             end,
    spawn_opt(Worker, Opts),
    receive
	{Stats, Size} ->
            io:format("of size ~w~n", [Size]),
	    pp_stat(Stats)
    end.

the_test({apply, F, Args}) -> apply(?MODULE, F, Args);
the_test({apply, M, F, Args}) -> apply(M, F, Args);
the_test(T) -> T.

term_bench(N) -> {_, X} = lists:nth(N, all_tests()),
                 the_test(X).

-record(range, {min, max}).

-record(stat, {range,
               median,
               average,
               stddev}).

timer_stats(M, N, T) when M > 0 ->
    L = test_loop(M, N, T, []),
    Length = length(L),
    S = #stat{range = #range{min = lists:min(L),
                             max = lists:max(L)},
              median = lists:nth(round(Length / 2), lists:sort(L)),
              average = avg(L),
              stddev = std_dev(L)},
    S.

test_loop(0, _, _, Results) ->
    Results;
test_loop(M, N, T, Results) ->
    {Time, ok} = timer:tc(fun regression/2, [N, T]),
    test_loop(M-1, N, T, [Time/1000000 | Results]).  % seconds

avg(L) ->
    lists:sum(L) / length(L).

std_dev(Values) ->
    L = length(Values),
    case L =:= 1 of
        true -> 0.0; % Executed only once (no deviation).
        false ->
            Avg = avg(Values),
            Sums = lists:foldl(
                     fun(V, Acc) -> D = V - Avg, Acc + (D * D) end, 0, Values),
            math:sqrt(Sums / (L - 1))
    end.

pp_stat(#stat{range   = #range{min=Min,max=Max},
              median  = Med,
              average = Avg,
              stddev  = Stddev}) ->
    io:format("min=~.6f, max=~.6f, med=~.6f, avg=~.6f, std=~.6f~n",
              [Min, Max, Med, Avg, Stddev]).


% Regression test: copy term T (that does not share anything) N times

regression(N, T) ->
    Opts = [],
    %Opts = [{min_heap_size, 100000000}],
    Parent = self(),
    Child = spawn_opt(fun () -> receiver_aux(Parent, N) end, Opts),
    sender_aux(Child, T, N).

sender_aux(_, _, 0) ->
    receive
        ok -> ok
    end;
sender_aux(Child, X, N) ->
    Child ! X,
    sender_aux(Child, X, N-1).

receiver_aux(Parent, 0) ->
    Parent ! ok;
receiver_aux(Parent, N) ->
    receive
        _ -> receiver_aux(Parent, N-1)
    end.


% The tests

all_tests() ->
    lists:concat([
      % big terms just once
      [{1, X} || X <- [{apply, mklist, [25]},             % 01: size 134217724
                       {apply, mktuple, [25]},            % 02: size 100663293
                       {apply, mkfunny, [47]},            % 03: size 100663240
                       {apply, mkimfunny1, [52]},         % 04: size 129604984
                       {apply, mkimfunny2, [32]},         % 05: size 109176439
                       {apply, mkimfunny3, [23]},         % 06: size 112193088
                       {apply, mkimfunny4, [60000000]},   % 07: size 120000002
                       {apply, mkimfunny5, [72]},         % 08: size 119853322
                       {apply, mkcls, [53]}               % 09: size 130516500
                      ]],
      % really small terms extremely many times
      [{10000000, X} || X <- [42,                         % 10: size 0
                              [],                         % 11: size 0
                              ok,                         % 12: size 0
                              [42],                       % 13: size 2/0
                              {42},                       % 14: size 2/0
                              <<>>,                       % 15: size 2/?
                              <<42>>,                     % 16: size 3/?
                              <<17, 42>>                  % 17: size 3/?
                             ]],
      % small terms many times
      [{10000000, {apply, lists, seq, [1, 20]}},          % 18: size 40
       { 5000000, {apply, mklist, [5]}},                  % 19: size 124
       { 5000000, {apply, mktuple, [5]}},                 % 20: size 93
       { 2500000, {apply, mkcls, [3]}},                   % 21: size 220
       { 1000000, {apply, lists, seq, [1, 250]}},         % 22: size 500
       {  500000, {apply, mklist, [8]}},                  % 23: size 1020
       {  500000, {apply, mktuple, [8]}},                 % 24: size 765
       {  250000, {apply, mkcls, [6]}}                    % 25: size 1640
      ]
    ]).
