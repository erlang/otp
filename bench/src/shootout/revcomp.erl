%%
%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%%
%% Based on two Erlang versions contributed by
%% Vlad Balin and Fredrik Svahn.
%% contributed by Michael Pitidis
%% I/O redone by Erik Søe Sørensen

-module(revcomp).

%-compile([native, {hipe, [o3]}, inline, {inline_size, 100}]).
%-compile(export_all).

-export([main/1]).
-export([small/0,medium/0,big/0]).

-define(WIDTH, 60).
-define(WORKERS, 4).
-define(BUFSIZE, 4096).

%% Small, medium, big
small() -> 250000.
medium() -> 25000000. % <-- default (20.77 sec)
big() -> 100000000. % untested.

main(_Args) ->
  io:setopts([binary]),
  run_parallel(),
  exit(ok).

%% Set up one process for reading. Transformations and printing are
%% handled asynchronously in separate processes.
run_parallel() ->
  register(reader, self()),
  reader ! go,
  loop(<< >>).

loop(Buf) ->
  case get_line() of
    eof ->
      receive go -> ok end,
      spawn(fun() -> flush(<< >>, Buf) end),
      receive go -> ok end;
    << ">", _/bytes >> = Comment ->
      receive go -> ok end,
      spawn(fun() -> flush([Comment, $\n], Buf) end),
      loop(<< >>);
    Line ->
      % Strip newline and append.
%%       S = size(Line) - 1,
%%       << Chunk:S/bytes, _ >> = Line,
      loop(<< Buf/binary, Line/binary >>)
  end.

get_line() ->
    Buf = case get(linebuf) of
	      undefined -> <<>>;
	      B -> B
	  end,
    case binary:split(Buf, <<"\n">>) of
	[Line,Rest] ->
	    put(linebuf, Rest),
	    Line;
	[_] ->
	    IsEOF = get(linebuf_eof) /= undefined,
	    if Buf==<<>>, IsEOF ->
		    eof;
	       true ->
		    case file:read(standard_io, ?BUFSIZE) of
			eof ->
			    put(linebuf_eof, true),
			    get_line();
			{ok, Data} ->
			    put(linebuf, <<Buf/binary, Data/binary>>),
			    get_line()
		    end
	    end
    end.
	      


%% Calculate the reverse complement of Buffer, and print it.
%% Calculation is done in chunks, each assigned a separate process.
%% The results are collected, and printed in the correct order.
flush(Comment, Buffer) ->
  register(collector, self()),
  io:put_chars(reverse_complement(Buffer)),
  io:put_chars(Comment),
  unregister(collector),
  reader ! go.

%% Calculation is distributed among workers.
%% As a minor optimization, workers handle only chunks of the same size,
%% evenly divisible by ?WIDTH. The remainder is handled by the current
%% process, with a separate function.
reverse_complement(<< >>) ->
  << >>;
reverse_complement(Buffer) ->
  {Chunks, Left} = calculate_splits(size(Buffer), ?WORKERS),
  Even = start_jobs(Buffer, Chunks),
  Last = revcomp_last(Buffer, Left, << >>),
  collect(Even) ++ [Last].

start_jobs(_, 0) ->
  0;
start_jobs(Buffer, Chunks) ->
  start_jobs(Buffer, Chunks, size(Buffer), 0).

start_jobs(_, _, _, N = ?WORKERS) ->
  N;
start_jobs(Buffer, Chunk, Size, N) when Size >= Chunk ->
  new_job({fun revcomp_chunk/4, [Buffer, Size - Chunk, Size, << >>]}, N),
  start_jobs(Buffer, Chunk, Size - Chunk, N + 1).

%% Specialized function which handles even chunks.
revcomp_chunk(_, Start, Start, Acc) ->
  Acc;
revcomp_chunk(Buffer, Start, Stop, Acc) ->
  From = Stop - ?WIDTH,
  << _:From/bytes, Line:?WIDTH/bytes, _/bytes >> = Buffer,
  RC = revcomp(Line),
  revcomp_chunk(Buffer, Start, From, << Acc/binary, RC/binary >>).

%% Specialized function which handles the uneven chunk.
revcomp_last(Buffer, Stop, Acc) when Stop > ?WIDTH ->
  From = Stop - ?WIDTH,
  << _:From/bytes, Line:?WIDTH/bytes, _/bytes >> = Buffer,
  RC = revcomp(Line),
  revcomp_last(Buffer, From, << Acc/binary, RC/binary >>);
revcomp_last(Buffer, Stop, Acc) ->
  << Line:Stop/bytes, _/bytes >> = Buffer,
  RC = revcomp(Line),
  << Acc/binary, RC/binary >>.

%% Generate the reverse complement of a sequence, and append
%% a newline character.
revcomp(<< >>) ->
  << >>;
revcomp(Line) ->
  list_to_binary(lists:reverse(
      [ 10 | [ complement(C) || C <- binary_to_list(Line)]])).

calculate_splits(Size, Nodes) ->
  Tmp = Size div Nodes,
  Rem = Tmp rem ?WIDTH,
  Chunks = Tmp - Rem,
  Left = (Size rem Nodes) + (Nodes * Rem),
  {Chunks, Left}.

complement( $A ) -> $T;
complement( $C ) -> $G;
complement( $G ) -> $C;
complement( $T ) -> $A;
complement( $U ) -> $A;
complement( $M ) -> $K;
complement( $R ) -> $Y;
complement( $Y ) -> $R;
complement( $K ) -> $M;
complement( $V ) -> $B;
complement( $H ) -> $D;
complement( $D ) -> $H;
complement( $B ) -> $V;
complement( $a ) -> $T;
complement( $c ) -> $G;
complement( $g ) -> $C;
complement( $t ) -> $A;
complement( $u ) -> $A;
complement( $m ) -> $K;
complement( $r ) -> $Y;
complement( $y ) -> $R;
complement( $k ) -> $M;
complement( $v ) -> $B;
complement( $h ) -> $D;
complement( $d ) -> $H;
complement( $b ) -> $V;
complement( $N ) -> $N;
complement( $S ) -> $S;
complement( $W ) -> $W;
complement( $n ) -> $N;
complement( $s ) -> $S;
complement( $w ) -> $W.

%% Parallel helpers.
new_job({Fun, Args}, N) ->
  spawn(fun() -> collector ! {N, apply(Fun, Args)} end).

collect(N) -> collect(N, []).
collect(0, Results) -> [ R || {_, R} <- lists:keysort(1, Results) ];
collect(N, Results) -> receive {K, R} -> collect(N-1, [{K, R} | Results]) end.
