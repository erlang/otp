%%% The Great Computer Language Shootout 
%%% http://shootout.alioth.debian.org/
%%% 
%%% strcat.erl: 22 Jul 2001 ms@mcdev.com.au (Martin Sandiford)
%%% modified by Isaac Gouy
%%%
%%% This is as close to the shootout specs for string concatenation as
%%% I can get it.  There is a small cheat that appends shorter strings
%%% to longer strings, rather than the other way around.  The results
%%% are the same.
%%%
%%% String handling is not really one of Erlang's strengths.
%%% From the Erlang FAQ at http://www.erlang.org/faq/x304.html
%%% question 5.2:
%%%
%%%    String handling in Erlang is less efficient than it could
%%%    be. Each character consumes 8 bytes of memory (a 32 bit integer
%%%    and a 32 bit pointer) and access to the nth. element takes O(n)
%%%    time. The net result is that many typical string handling
%%%    operations are significantly slower in Erlang than in (say) C or
%%%    even Java.
%%%
%%%
%%% Usage: start from command line with:
%%%    erlc strcat.erl # to compile
%%%    erl -noinput -s strcat main 40000

-module(strcat).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 50000.
medium() -> 500000.
big() -> 20000000.

main() -> main(["1"]).
main(Arg) ->
   Num = Arg,
   io:fwrite("~w\n", [length(make_string("hello\n", Num))]),
   exit(ok).

make_string(T, N) -> make_string("", T, N).

make_string(S, _, 0) -> S;
make_string(S, T, N) -> make_string(T ++ S, T, N-1).
