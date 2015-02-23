%%% The Computer Language Benchmarks Game
%%% http://shootout.alioth.debian.org/
%%% contributed by Christian von Roques
%%% modified by Jiri Isa

%% Each chameneos is its own process.
%% A chameneos sends {self(), Color} to the broker to request a
%% meeting with another chameneos.
%% The broker replies with {Pid, Color} of the partner met or 'stop'
%% whereupon the chameneos prints the Meetings and Selfmeetings it had
%% and replies with the number of Meetings for the broker to sum.

-module(chameneosredux).
-export([main/1]).

-import(lists, [foreach/2]).
-export([small/0,medium/0,big/0]).

small() -> 160000.
medium() -> 6000000. % <-- default (9.93 sec)
big() -> 120000000.

spell(0) -> " zero";
spell(N) -> spell(N, []).

spell(0, L) -> L;
spell(N, L) -> spell(N div 10, [element(N rem 10 + 1, {" zero", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine"}) | L]).


complement(C, C) -> C;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(red, blue) -> yellow;
complement(red, yellow) -> blue;
complement(yellow, blue) -> red;
complement(yellow, red) -> blue.


show_complements() ->
    [ io:fwrite("~p + ~p -> ~p~n", [A, B, complement(A, B)]) ||
        A <- [blue, red, yellow],
        B <- [blue, red, yellow]].


print_header(L) ->
    io:fwrite("~n"),
    foreach(fun(C) -> io:fwrite(" ~p", [C]) end, L),
    io:fwrite("~n").


run(L, N) ->
    print_header(L),
    Broker = self(),
    foreach(fun(Color) -> spawn(fun() -> chameneos(Broker, Color, 0, 0) end) end, L),
    broker(N),
    cleanup(length(L), 0).


chameneos(Broker, Color, Meetings, MetSelf) ->
    Broker ! { self(), Color },
    receive
        {OPid, OColor} ->
            chameneos(Broker, complement(Color, OColor), Meetings+1,
                      if OPid == self() -> MetSelf+1; true -> MetSelf end);
        stop ->
            io:fwrite("~w~s\n", [Meetings, spell(MetSelf)]),
            Broker ! Meetings
    end.


broker(0) -> nil;
broker(N) ->
    receive
        C1 = {Pid1, _} -> nil
    end,
    receive
        C2 = {Pid2, _} ->
            Pid1 ! C2,
            Pid2 ! C1,
            broker(N-1)
    end.

cleanup(0, M) -> io:fwrite("~s~n", [spell(M)]);
cleanup(N, M) ->
    receive
        {Pid, _Color} ->
            Pid ! stop,
            cleanup(N, M);
        Meetings ->
            cleanup(N-1, M+Meetings)
    end.


main(Arg) ->
    N = Arg,
    show_complements(),
    run([blue, red, yellow], N),
    run([blue, red, yellow, red, yellow, blue, red, yellow, red, blue], N),
    io:fwrite("~n"),
    exit(ok).

