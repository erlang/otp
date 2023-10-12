-module(efficiency_guide).
-compile([export_all,nowarn_export_all]).

%% DO NOT
naive_reverse([H|T]) ->
    naive_reverse(T)++[H];
naive_reverse([]) ->
    [].

%% OK
naive_but_ok_reverse([H|T], Acc) ->
    naive_but_ok_reverse(T, [H]++Acc);
naive_but_ok_reverse([], Acc) ->
    Acc.

%% DO
vanilla_reverse([H|T], Acc) ->
    vanilla_reverse(T, [H|Acc]);
vanilla_reverse([], Acc) ->
    Acc.
    
    
multiple_setelement(T0) ->
    T1 = setelement(9, T0, bar),
    T2 = setelement(7, T1, foobar),
    setelement(5, T2, new_value).


my_list_to_binary(List) ->
    my_list_to_binary(List, <<>>).

my_list_to_binary([H|T], Acc) ->
    my_list_to_binary(T, <<Acc/binary,H>>);
my_list_to_binary([], Acc) ->
    Acc.

my_old_list_to_binary(List) ->
    my_old_list_to_binary(List, []).

my_old_list_to_binary([H|T], Acc) ->
    my_old_list_to_binary(T, [Acc,H]);
my_old_list_to_binary([], Acc) ->
    list_to_binary(Acc).

my_binary_to_list(<<H,T/binary>>) ->
    [H|my_binary_to_list(T)];
my_binary_to_list(<<>>) -> [].

my_complicated_binary_to_list(Bin) ->
    my_complicated_binary_to_list(Bin, 0).

my_complicated_binary_to_list(Bin, Skip) ->
    case Bin of
	<<_:Skip/binary,Byte,_/binary>> ->
	    [Byte|my_complicated_binary_to_list(Bin, Skip+1)];
	<<_:Skip/binary>> ->
	    []
    end.

after_zero(<<0,T/binary>>) ->
    T;
after_zero(<<_,T/binary>>) ->
    after_zero(T);
after_zero(<<>>) ->
    <<>>.

all_but_zeroes_to_list(Buffer, Acc, 0) ->
    {lists:reverse(Acc),Buffer};
all_but_zeroes_to_list(<<0,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, Acc, Remaining-1);
all_but_zeroes_to_list(<<Byte,T/binary>>, Acc, Remaining) ->
    all_but_zeroes_to_list(T, [Byte|Acc], Remaining-1).

count1(<<_,T/binary>>, Count) -> count1(T, Count+1);
count1(<<>>, Count) -> Count.

count2(<<H,T/binary>>, Count) -> count2(T, Count+1);
count2(<<>>, Count) -> Count.

count3(<<_H,T/binary>>, Count) -> count3(T, Count+1);
count3(<<>>, Count) -> Count.

fib(N) ->
    fib(N, 0, 1, []).

fib(0, _Current, _Next, Fibs) ->
    lists:reverse(Fibs);
fib(N, Current, Next, Fibs) -> 
    fib(N - 1, Next, Current + Next, [Current|Fibs]).

recursive_fib(N) ->
    recursive_fib(N, 0, 1).

recursive_fib(0, _Current, _Next) ->
    [];
recursive_fib(N, Current, Next) -> 
    [Current|recursive_fib(N - 1, Next, Current + Next)].

bad_fib(N) ->
    bad_fib(N, 0, 1, []).

bad_fib(0, _Current, _Next, Fibs) ->
    Fibs;
bad_fib(N, Current, Next, Fibs) -> 
    bad_fib(N - 1, Next, Current + Next, Fibs ++ [Current]).

tail_recursive_fib(N) ->
    tail_recursive_fib(N, 0, 1, []).

tail_recursive_fib(0, _Current, _Next, Fibs) ->
    lists:reverse(Fibs);
tail_recursive_fib(N, Current, Next, Fibs) -> 
    tail_recursive_fib(N - 1, Next, Current + Next, [Current|Fibs]).

append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.

%%kilo_byte
kilo_byte() ->
    kilo_byte(10, [42]).

kilo_byte(0, Acc) ->
    Acc;
kilo_byte(N, Acc) ->
    kilo_byte(N-1, [Acc|Acc]).
%%kilo_byte

recursive_sum([H|T]) ->
    H+recursive_sum(T);
recursive_sum([]) -> 0.

sum(L)          -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

days_in_month(M) ->
    element(M, {31,28,31,30,31,30,31,31,30,31,30,31}).
    
atom_map1(one) -> 1;
atom_map1(two) -> 2;
atom_map1(three) -> 3;
atom_map1(Int) when is_integer(Int) -> Int;
atom_map1(four) -> 4;
atom_map1(five) -> 5;
atom_map1(six) -> 6.

atom_map2(one) -> 1;
atom_map2(two) -> 2;
atom_map2(three) -> 3;
atom_map2(four) -> 4;
atom_map2(five) -> 5;
atom_map2(six) -> 6;
atom_map2(Int) when is_integer(Int) -> Int.

atom_map3(Int) when is_integer(Int) -> Int;
atom_map3(one) -> 1;
atom_map3(two) -> 2;
atom_map3(three) -> 3;
atom_map3(four) -> 4;
atom_map3(five) -> 5;
atom_map3(six) -> 6.
    
    
map_pairs1(_Map, [], Ys) ->
    Ys;
map_pairs1(_Map, Xs, [] ) ->
    Xs;
map_pairs1(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs1(Map, Xs, Ys)].

map_pairs2(_Map, [], Ys) ->
    Ys;
map_pairs2(_Map, [_|_]=Xs, [] ) ->
    Xs;
map_pairs2(Map, [X|Xs], [Y|Ys]) ->
    [Map(X, Y)|map_pairs2(Map, Xs, Ys)].

explicit_map_pairs(Map, Xs0, Ys0) ->
    case Xs0 of
	[X|Xs] ->
	    case Ys0 of
		[Y|Ys] ->
		    [Map(X, Y)|explicit_map_pairs(Map, Xs, Ys)];
		[] ->
		    Xs0
	    end;
	[] ->
	    Ys0
    end.

%% DO
simple_receive() ->
    receive
        Message -> handle_msg(Message)
    end.

%% OK, if Tag is a reference.
selective_receive(Tag, Message) ->
    receive
        {Tag, Message} -> handle_msg(Message)
    end.

%% DO
optimized_receive(Process, Request) ->
    MRef = monitor(process, Process),
    Process ! {self(), MRef, Request},
    receive
        {MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            handle_reply(Reply);
        {'DOWN', MRef, _, _, Reason} ->
            handle_error(Reason)
    end.

%% DO
cross_function_receive() ->
    Ref = make_ref(),
    cross_function_receive(Ref).

cross_function_receive(Ref) ->
    receive
        {Ref, Message} -> handle_msg(Message)
    end.
    
handle_reply(_) -> ok.
handle_error(_) -> ok.
handle_msg(_) -> ok.

%%rec
-record(state, {info,size,data}).
%%rec

%%init1
init1() ->
    #state{data=lists:seq(1, 10000)}.
%%init1

%%init2
init2() ->
    SharedSubTerms = lists:foldl(fun(_, A) -> [A|A] end, [0], lists:seq(1, 15)),
    #state{data=Shared}.
%%init2

%%acc1
accidental1(State) ->
    spawn(fun() ->
                  io:format("~p\n", [State#state.info])
          end).
%%acc1

%%acc2
accidental2(State) ->
    spawn(fun() ->
                  io:format("~p\n", [map_get(info, State)])
          end).
%%acc2

%%fixed_acc1
fixed_accidental1(State) ->
    Info = State#state.info,
    spawn(fun() ->
                  io:format("~p\n", [Info])
          end).
%%fixed_acc1

%%fixed_acc2
fixed_accidental2(State) ->
    Info = map_get(info, State),
    spawn(fun() ->
                  io:format("~p\n", [Info])
          end).
%%fixed_acc2

%%handle_call
handle_call(give_me_a_fun, _From, State) ->
    Fun = fun() -> State#state.size =:= 42 end,
    {reply, Fun, State}.
%%handle_call
