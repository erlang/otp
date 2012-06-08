% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by: Hynek Vychodil 2009
% Inspired by regex-dna Erlang HiPE #5 program
%    by Sergei Matusevich 2007 and Thanassis Avgerinos 2009

% Main changes:
%   1/ Very fast Port line input instead stdio (~5x)
%   2/ Faster IUB code alternatives explicit expansion
%      using binary instead lists (~5x)
%   3/ Precompile regexps in data loading phase
%   4/ Simpler dispatch and result join code

% Note: re module is damn slow. Boyer-Moore like binary matcher
% written in Erlang should be magnitude faster (HiPE of course).

-module(regexdna).

-compile([native, {hipe, [o3]}]).

-export([main/1]).
-export([small/0, medium/0, big/0]).

small() -> 500000.
medium() -> 5000000.
big() -> 10000000.

main(_) -> main().

main() -> do(), halt().

do() ->
    S = self(),
    Worker = spawn_link(fun () -> work(S) end),
    Worker ! {data, read()},
    receive finish -> ok end.

work(Master) ->
    S = self(),
    Patterns = [{Pat, re:compile(Pat, [caseless])}
		|| Pat <- patterns()],
    {RawSize, [B3, B2, B1 | _]} = receive
				    {data, Data} -> Data
				  end,
    [L1, L2, L3] = L = [size(X) || X <- [B1, B2, B3]],
    Size = lists:sum(L),
    PIDS = [{spawn_link(matcher(S, B2, B3, MR)),
	     printer(Pat)}
	    || {Pat, {ok, MR}} <- Patterns],
    ExpandedSize = L1 + L3 + size(expand(B2, L2, 0, <<>>)),
    results(PIDS),
    io:format("~n~b~n~b~n~b~n",
	      [RawSize, Size, ExpandedSize]),
    Master ! finish.

expand(B, S, I, R) when I < S ->
    case B of
      <<_:I/binary, $B, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|g|t)">>);
      <<_:I/binary, $D, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|g|t)">>);
      <<_:I/binary, $H, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|t)">>);
      <<_:I/binary, $K, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(g|t)">>);
      <<_:I/binary, $M, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c)">>);
      <<_:I/binary, $N, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|g|t)">>);
      <<_:I/binary, $R, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|g)">>);
      <<_:I/binary, $S, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|g)">>);
      <<_:I/binary, $V, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|g)">>);
      <<_:I/binary, $W, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|t)">>);
      <<_:I/binary, $Y, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|t)">>);
      <<_:I/binary, X, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, X>>)
    end;
expand(_, _, _, R) -> R.

matcher(S, B2, B3, MR) ->
    fun () ->
	    S !
	      {self(), countMatches(B2, MR) + countMatches(B3, MR)}
    end.

printer(Pat) ->
    fun (Num) -> io:format("~s ~b~n", [Pat, Num]) end.

countMatches(Data, RE) ->
    case re:run(Data, RE, [global]) of
      {match, M} -> length(M);
      nomatch -> 0
    end.

results([{PID, Fin} | R]) ->
    receive {PID, Ret} -> Fin(Ret), results(R) end;
results([]) -> ok.

patterns() ->
    ["agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
     "a[act]ggtaaa|tttacc[agt]t",
     "ag[act]gtaaa|tttac[agt]ct",
     "agg[act]taaa|ttta[agt]cct",
     "aggg[acg]aaa|ttt[cgt]ccct",
     "agggt[cgt]aa|tt[acg]accct",
     "agggta[cgt]a|t[acg]taccct",
     "agggtaa[cgt]|[acg]ttaccct"].

read() ->
    Port = open_port({fd, 0, 1}, [in, binary, {line, 256}]),
    read(Port, 0, [], []).

read(Port, Size, Seg, R) ->
    receive
      {Port, {data, {eol, <<$>:8, _/binary>> = Line}}} ->
	  read(Port, Size + size(Line) + 1, [],
	       [iolist_to_binary(lists:reverse(Seg, [])) | R]);
      {Port, {data, {eol, Line}}} ->
	  read(Port, Size + size(Line) + 1, [Line | Seg], R);
      {'EXIT', Port, normal} ->
	  {Size, [iolist_to_binary(lists:reverse(Seg, [])) | R]};
      Other ->
	  io:format(">>>>>>> Wrong! ~p~n", [Other]),
	  exit(bad_data)
    end.
