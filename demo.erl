-module(demo).
-export([show_sizes/1,
	 show_send_destroys_sharing/0,
	 show_spawn_destroys_sharing_in_arguments/0,
	 show_spawn_destroys_sharing_in_closure/0,
	 show_printing_may_be_bad/0,
	 show_optim_destroys_sharing/0,
	 show_compiler_crashes/0,
	 mklist/1, mktuple/1, mkfunny/1, mkcls/1,
	 mkimfunny1/1, mkimfunny2/1, mkimfunny3/1, mkimfunny4/1, mkimfunny5/1,
	 mkimlist/1, mkbin/1, mkbin2/1, mkmap/1,
	 sanity/0, sanity/1, sanity/2, sanity/3, term_sanity/1,
	 sanity_report/2, sanity_check/1, sanity_size_check/1
	 ]).


% Function which shows the size (flat and real) of a term

show_sizes(T) ->
    Real = erts_debug:size(T),
    Flat = erts_debug:flat_size(T),
    io:format("real = ~w, flat = ~w~n", [Real, Flat]).

% This shows that sending a message destroys sharing

show_send_destroys_sharing() ->
    L1 = lists:seq(1, 10),
    L2 = [L1, L1, L1, L1, L1],
    show_sizes(L2),
    Pid = spawn(fun () -> receive M -> show_sizes(M) end end),
    Pid ! L2,
    ok.

% This shows that spawning a process destroys sharing
% for terms that are passed as the function's arguments

show_spawn_destroys_sharing_in_arguments() ->
    L1 = lists:seq(1, 10),
    L2 = [L1, L1, L1, L1, L1],
    show_sizes(L2),
    spawn(?MODULE, show_sizes, [L2]),
    ok.

% This shows that spawning a process destroys sharing
% for terms that are in the function's closure

show_spawn_destroys_sharing_in_closure() ->
    L1 = lists:seq(1, 10),
    L2 = [L1, L1, L1, L1, L1],
    show_sizes(L2),
    spawn(fun () -> show_sizes(L2) end),
    ok.

% This shows that printing may be bad too

mklist(0) -> 0;
mklist(M) -> X = mklist(M-1), [X, X].

show_printing_may_be_bad() ->
    F = fun (N) ->
            T = now(),
            L = mklist(N),
            S = erts_debug:size(L),
            io:format("mklist(~w), size ~w, ", [N, S]),
            io:format("is ~P, ", [L, 2]),
            D = timer:now_diff(now(), T),
            io:format("in ~.3f sec.~n", [D/1000000])
        end,
    lists:foreach(F, [10, 20, 22, 24, 26, 28, 30]).

% This shows a problem with code generation or optimization,
% which somehow destroys sharing

show_optim_destroys_sharing() ->
    L1 = lists:seq(1, 10),
    L2 = [L1, L1, L1, L1, L1],
    show_sizes(L2),
    L3 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    L4 = [L3, L3, L3, L3, L3],
    show_sizes(L4).

% This show that this optimization is actually dangerous

-ifdef(I_FEEL_LIKE_CRASHING_IT).

show_compiler_crashes() ->
    L0 = [0],
    L1 = [L0, L0, L0, L0, L0, L0, L0, L0, L0, L0],
    L2 = [L1, L1, L1, L1, L1, L1, L1, L1, L1, L1],
    L3 = [L2, L2, L2, L2, L2, L2, L2, L2, L2, L2],
    L4 = [L3, L3, L3, L3, L3, L3, L3, L3, L3, L3],
    L5 = [L4, L4, L4, L4, L4, L4, L4, L4, L4, L4],
    L6 = [L5, L5, L5, L5, L5, L5, L5, L5, L5, L5],
    L7 = [L6, L6, L6, L6, L6, L6, L6, L6, L6, L6],
    L8 = [L7, L7, L7, L7, L7, L7, L7, L7, L7, L7],
    L9 = [L8, L8, L8, L8, L8, L8, L8, L8, L8, L8],
    L  = [L9, L9, L9, L9, L9, L9, L9, L9, L9, L9],
    L.

-else.

show_compiler_crashes() ->
    ok.

-endif.


% Functions that create various data structures with sharing

mktuple(0) -> 0;
mktuple(M) -> X = mktuple(M-1), {X, X}.

mkfunny(0) -> [];
mkfunny(M) -> [mktuple(M div 2) | mkfunny(M-1)].

mkcls(0) -> 42;
mkcls(M) -> X = mkcls(M-1), F = fun (N) -> [N, X, M, X] end, {X, F, F(M)}.

mkimfunny1(0) -> 42;
mkimfunny1(M) -> X = mkimfunny1(M div 2), [X, X | mkimfunny1(M-1)].

mkimfunny2(0) -> 42;
mkimfunny2(M) -> X = mktuple(M div 2), Y = mkimfunny2(M-1), [X, X | Y].

mkimfunny3(0) -> 42;
mkimfunny3(M) -> X = mktuple(M div 2), Y = mkimfunny3(M-1), [Y, X, Y | X].

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

mkimlist(0) -> 0;
mkimlist(M) -> [M | mkimlist(M-1)].

mkbin(0) -> << >>;
mkbin(M) -> B = mkbin(M-1), <<B/binary, M, B/binary>>.

mkbin2(0) -> <<42>>;
mkbin2(M) -> B = mkbin2(M-1),
	     <<X:4, Y:4, Rest/binary>> = B,
	     <<X, B/binary, M, Rest/binary, Y>>.

mkmap(0) -> #{};
mkmap(M) -> Map1 = mkmap(M-1),
            Map2 = mkmap(M div 2),
            Map3 = maps:put(2*M, Map2, Map1),
            Map4 = maps:put(2*M+1, Map2, Map3),
            maps:put(M, Map1, Map4).


% Machinery for testing

sanity() -> sanity(0, fun sanity_check/1).

sanity(N) when is_integer(N) -> sanity(N, fun sanity_check/1);
sanity(Fun) -> test(0, all_tests(), Fun).

sanity(N, Fun) -> test(N, all_tests(), Fun).

sanity(From, To, Fun) ->
    test(0, lists:sublist(all_tests(), From, To-From+1), Fun).

term_sanity(N) -> X = lists:nth(N, all_tests()), the_test(X).

test(0, [], _)	    -> ok;
test(0, [X], Fun)   -> Fun(X);
test(0, [X|L], Fun) -> Fun(X), test(0, L, Fun);
test(1, [X|_], Fun) -> Fun(X);
test(N, [_|L], Fun) -> test(N-1, L, Fun).

the_test({apply, F, Args}) -> apply(?MODULE, F, Args);
the_test(T) -> T.

% Sanity checking for size_shared

sanity_size_check(X) ->
    io:format("checking just size ~40P: ", [X, 3]),
    T = the_test(X),
    Real = erts_debug:size(T),
    Flat = erts_debug:flat_size(T),
    Shared = erts_debug:size_shared(T),
    case Real =:= Shared of
        true ->
            io:format("OK~n");
        false ->
            io:format("MISMATCH~n")
    end,
    io:format("  real = ~w, shared = ~w, flat = ~w~n", [Real, Shared, Flat]).

% Sanity checker function

sanity_report(Term, Pid) ->
    FoundReal = erts_debug:size(Term),
    FoundFlat = erts_debug:flat_size(Term),
    Pid ! {FoundReal, FoundFlat}.

sanity_check_report(Msg, X, Real, Flat) ->
    receive
        {Real, Flat} ->
            io:format("OK");
        {WrongReal, WrongFlat} ->
            io:format("~n~nSANITY ERROR:~n"),
            io:format("  when ~s test case ~w~n", [Msg, X]),
            io:format("  real = ~w, in child = ~w~n", [Real, WrongReal]),
            io:format("  flat = ~w, in child = ~w~n", [Flat, WrongFlat])
    end.

sanity_fresh(X) ->
    io:format("checking ~40P:", [X, 3]),
    T = the_test(X),
    Real = erts_debug:size(T),
    Flat = erts_debug:flat_size(T),
    % checking message sending
    io:format(" send "),
    Receiver =
        fun () ->
                receive
                    {Term, Pid} -> sanity_report(Term, Pid)
                end
        end,
    Pid = spawn(Receiver),
    Pid ! {T, self()},
    sanity_check_report("sending", X, Real, Flat),
    % checking spawning processes, in arguments
    io:format(", spawn arg "),
    spawn(?MODULE, sanity_report, [T, self()]),
    sanity_check_report("spawning in arguments", X, Real, Flat),
    % checking spawning processes, in closure
    io:format(", spawn cls "),
    Myself = self(),
    spawn(fun () -> sanity_report(T, Myself) end),
    sanity_check_report("spawning in closure", X, Real, Flat),
    io:format("~n").

sanity_check(X) ->
    case X of
        {apply, _, _} ->
            Parent = self(),
            spawn(fun () -> sanity_fresh(X), Parent ! ok end),
            receive ok -> ok end;
        _ ->
            sanity_fresh(X)
    end.


% The tests

all_tests() ->
    L0 = lists:seq(1, 10),
    L1 = [L0, L0, L0, L0, L0],
    T1 = {L1, L0, [L1, L1, L0], {L0, [L1, L1]}},
    B1 = <<1,2,3,4>>,
    B2 = <<5,6,B1/binary,7,8>>,
    B3 = <<B1/binary, B2/binary, B2/binary>>,
    T2 = {B1, [B1, B1], B1},
    [L0,
     L1,
     T1,
     {apply, mklist, [10]},
     {apply, mktuple, [10]},
     {apply, mkfunny, [10]},
     {apply, mkimfunny1, [20]},
     {apply, mkimfunny2, [20]},
     {apply, mkimfunny3, [20]},
     {apply, mkimfunny4, [1000]},
     {apply, mkimfunny5, [30]},
     {apply, mkcls, [10]},
     B1,
     B2,
     B3,
     T2,
     {apply, mkbin, [10]},
     {apply, mkbin2, [10]},
     {apply, mkmap, [20]}
    ].
