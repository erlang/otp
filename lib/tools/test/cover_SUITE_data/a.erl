-module(a).
-export([start/1, stop/1]).
-export([pong/1]).
-export([loop/3,exit_kalle/0]).

%% start(N) -> pid()
%%   N = integer()
start(N) ->
    Pong = b:start(),
    spawn(?MODULE, loop, [self(), N, Pong]),
    receive
	done ->
	    {exit,kalle} = trycatch(fun ?MODULE:exit_kalle/0),
	    {throw,kalle} = trycatch(fun() -> throw(kalle) end),
	    done
    end.

%% stop(Ping) -> stop
%%   Ping = pid()
stop(Ping) ->
    Ping ! stop.

%% pong(Ping) -> pong
%%   Ping = pid()
pong(Ping) ->
    Ping ! pong.

%%--Internal functions------------------------------------------------

loop(Starter, N, Pong) when N>0 ->
    Pong ! {ping, self()},
    receive
	pong ->
	    loop(Starter, N-1, Pong);
	stop ->
	    done
    end;
loop(Starter, 0, Pong) ->
    Pong ! stop,
    Starter ! done.


trycatch(Fun) ->
    try Fun()
    catch
       Throw -> 
	   {throw,Throw};
       exit:Reason -> 
	   {exit,Reason}
   after
       cleanup
   end.

exit_kalle() ->
    exit(kalle).
