%   The Computer Language Benchmarks Game
%   http://shootout.alioth.debian.org/
%   contributed by Fredrik Svahn

-module(spectralnorm).
-export([main/1]).
-export([small/0,medium/0,big/0]).
-compile( [ inline, { inline_size, 1000 } ] ).



%% Small, medium, big
small() -> 3000.
medium() -> 5500. % <-- default (44.82 sec)
big() -> 6500.


main(Arg) ->
    register(server, self()),
    N = Arg,
    {U, V} = power_method(N, 10, erlang:make_tuple(N, 1), []),
    io:format("~.9f\n", [ eigen(N, U, V, 0, 0) ]),
    exit(ok).

% eigenvalue of V
eigen(0, _, _, VBV, VV) when VV /= 0 -> math:sqrt(VBV / VV);

eigen(I, U, V, VBV, VV) when I /= 0 ->
    VI = element(I, V),
    eigen(I-1, U, V, VBV + element(I, U)*VI, VV + VI*VI).

% 2I steps of the power method
power_method(_, 0, A, B) -> {A, B};
power_method(N, I, A, _B) ->
    V = atav(N, A),
    U = atav(N, V),
    power_method(N, I-1, U, V).


% return element i,j of infinite matrix A
a(II,JJ) -> 1/((II+JJ-2)*(II-1+JJ)/2+II).


% multiply vector v by matrix A
av(N, V) -> pmap(N, fun(Begin, End) -> av(N, Begin, End, V) end).

av(N, Begin, End, V) -> server ! { self(), [ avloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

avloop(0, _, _, X) ->  X;
avloop(J, I, V, X) ->  avloop(J-1, I, V, X + a(I, J)*element(J, V) ).


% multiply vector v by matrix A transposed
atv(N, V) -> pmap(N, fun(Begin, End)-> atv(N, Begin, End, V) end).

atv(N, Begin, End, V) -> server ! { self(), [ atvloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

atvloop(0, _, _, X) -> X;
atvloop(J, I, V, X) -> atvloop(J-1, I, V, X + a(J, I)*element(J, V) ).


% multiply vector v by matrix A and then by matrix A transposed
atav(N, V) -> atv(N, av(N, V)).


%Helper function for multicore
pmap(N, F) ->
    Chunks = chunks(0, erlang:system_info(logical_processors), N, []),
    Pids = [spawn(fun()-> F(Begin, End) end) || {Begin, End} <- Chunks],
    Res = [ receive {Pid, X} -> X end || Pid <- Pids],
    list_to_tuple(lists:flatten(Res)).

chunks(I, P, N, A) when I == P-1 -> lists:reverse([{I*(N div P)+1, N} | A ]);
chunks(I, P, N, A) -> chunks(I+1, P, N, [{ I*(N div P)+1, (I+1)*(N div P)} | A ]).

