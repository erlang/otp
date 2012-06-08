%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%% contributed by Fredrik Svahn based on an earlier submission
%%             by Kenneth Johansson, Vlad Dumitrescu and Ulf Wiger

-module(knucleotide).
-export([main/1]).
-export([small/0, medium/0, big/0]).

small() -> 2500000.
medium() -> 25000000.
big() -> 50000000.

to_upper(<<C, Cs/binary>>, Acc) when C >= $a, C =< $z ->
    to_upper(Cs, [C-($a-$A)| Acc]);
to_upper(<<$\n, Cs/binary>>, Acc) -> to_upper(Cs, Acc);
to_upper(<<C, Cs/binary>>, Acc) -> to_upper(Cs, [C | Acc]);
to_upper(<<>>, Acc) -> list_to_binary(lists:reverse(Acc)).

%% Read and discard until start of third segment
seek_three() ->
    case io:get_line('') of
	<<">TH", _/binary>> -> done;
	eof        -> erlang:error(eof);
	_          -> seek_three()
    end.

%% Read third segment
get_seq_three(Seq) ->
    case io:get_line('') of
	eof -> iolist_to_binary(lists:reverse(Seq));
	Str -> get_seq_three([to_upper(Str, [])|Seq])
    end.

%% Generate frequency hash table
gen_freq_table(FreqT, Seq, Len) ->
    gen_freq_table(Seq, Len, FreqT, size(Seq)-Len).

gen_freq_table(_, _, _, -1) -> done;
gen_freq_table(Seq, Len, FreqT, Dec) ->
    <<_:Dec/binary, Key:Len/binary, _/binary>> = Seq,
    update_counter(Key, FreqT),
    gen_freq_table(Seq, Len, FreqT, Dec-1).

%% Update hash table counter for already existing pattern or insert new
update_counter(Key, FreqT) ->
    try ets:update_counter(FreqT, Key, 1) of _ -> ok
    catch error:badarg -> ets:insert(FreqT, {Key, 1})
    end.

%% Print the frequency table in the right order
print_freq_table(FreqT) ->
    FreqList = lists:reverse(lists:keysort(2, ets:tab2list(FreqT))),
    Tot = lists:foldr(fun({_, Cnt}, Acc)-> Acc + Cnt end, 0, FreqList),
    lists:foreach(fun({Nucleoid, Cnt})->
			  io:fwrite("~s ~.3f\n",[Nucleoid, Cnt*100/Tot])
		  end, FreqList),
    io:fwrite("\n").

%% Print number of occurrences for a specific pattern
print_count(FreqT, Pattern) ->
    case ets:lookup(FreqT, Pattern) of
	[{_, Value}] -> io:fwrite("~w\t~s\n",[Value, Pattern]);
	[] -> io:fwrite("~w\t~s\n",[0, Pattern])
    end.

%% Spawn a worker process with its own hash table
do({PrintFun, Pattern}, Seq) ->
    spawn( fun()->
		   FreqT = ets:new(hash, [set]),
		   gen_freq_table(FreqT, Seq, size(Pattern)),
		   %Work is done, wait for token and print
		   receive Pids ->
			   PrintFun(FreqT, Pattern),
			   hd(Pids) ! tl(Pids)
		   end,
		   ets:delete(FreqT)
	   end ).

main(_Arg) ->
    io:setopts(standard_io, [binary]),
    seek_three(),
    Seq = get_seq_three([]),
    PrintFreq = fun(Res, _Pattern)-> print_freq_table(Res) end,
    PrintCount = fun(Res, Pattern)-> print_count(Res, Pattern) end,
    Actions = [{PrintFreq,  <<"?">>},
	       {PrintFreq,  <<"??">>},
	       {PrintCount, <<"GGT">>},
	       {PrintCount, <<"GGTA">>},
	       {PrintCount, <<"GGTATT">>},
	       {PrintCount, <<"GGTATTTTAATT">>},
	       {PrintCount, <<"GGTATTTTAATTTATAGT">>}],

    Pids = [ do(Action, Seq) || Action <- Actions ],
    %Pass token to print in right order
    hd(Pids) ! tl(Pids) ++ [self()],
    receive _Pid -> halt(0) end.
