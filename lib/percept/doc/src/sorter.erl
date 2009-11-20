-module(sorter).
-export([go/3,loop/0,main/4]).

go(I,N,M) ->
    spawn(?MODULE, main, [I,N,M,self()]),
    receive done -> ok end.

main(I,N,M,Parent) ->
    Pids = lists:foldl(
	fun(_,Ps) -> 
	    [ spawn(?MODULE,loop, []) | Ps]
	end, [], lists:seq(1,M)),

    lists:foreach(
	fun(_) -> 
	    send_work(N,Pids),
	    gather(Pids)
	end, lists:seq(1,I)),

    lists:foreach(
	fun(Pid) ->
	    Pid ! {self(), quit}
	end, Pids),

    gather(Pids), Parent ! done.

send_work(_,[]) -> ok;
send_work(N,[Pid|Pids]) ->
    Pid ! {self(),sort,N},
    send_work(round(N*1.2),Pids).

loop() ->
    receive
	{Pid, sort, N} -> dummy_sort(N),Pid ! {self(), done},loop();
	{Pid, quit} -> Pid ! {self(), done}
    end.
	    
dummy_sort(N) -> lists:sort([ random:uniform(N) || _ <- lists:seq(1,N)]).

gather([]) -> ok;
gather([Pid|Pids]) -> receive {Pid, done} -> gather(Pids) end.
