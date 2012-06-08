%%%  The Great Computer Language Shootout 
%%%  http://shootout.alioth.debian.org/
%%% 
%%%  modified by Isaac Gouy

%%%  Use destructive assignment in the process dictionary 
%%%  to keep count of handled exceptions.

-module(except). 
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 1600000.
medium() -> 10000000.
big() -> 70000000.

blowup(N) when N rem 2 == 0 -> throw({lo_exception, N});
blowup(N) -> throw({hi_exception, N}).

lo_fun(N) ->
   case catch blowup(N) of
      {lo_exception, N1} -> put(lo_count, get(lo_count) + 1);
      {hi_exception, N2} -> throw({hi_exception, N2})
   end.
    
hi_fun(N) -> 
   case catch lo_fun(N) of
      {hi_exception, N1} -> put(hi_count, get(hi_count) + 1);
      _ -> true
   end.
    
some_fun(0) -> true;
some_fun(N) ->
   case catch hi_fun(N) of
      {lo_exception, N1} -> io:fwrite("~s~n", ["lo_exception should not get here."]);
      {hi_exception, N2} -> io:fwrite("~s~n", ["hi_exception should not get here."]);
      _ -> true
   end,
   some_fun(N - 1).
    
main() -> main(["1"]).
main(Arg) ->
   Num = Arg,
   put(hi_count, 0),
   put(lo_count, 0),
   some_fun(Num),
   io:fwrite("~s~w ~s~w~n", ["Exceptions: HI=", get(hi_count),"/ LO=", get(lo_count)]),
   exit(ok).
