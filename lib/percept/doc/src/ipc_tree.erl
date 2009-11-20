-module(ipc_tree).
-export([go/1, init/2]).

go(N) ->
    start(N, self()),
    receive {_,stop} -> ok end.

start(Depth, ParentPid) ->
    spawn(?MODULE, init, [Depth, ParentPid]).

init(0, ParentPid) -> 
    workload(5000),
    ParentPid ! {self(),stop},
    ok;
init(Depth, ParentPid) ->
    Pid1 = spawn(?MODULE, init, [Depth - 1, self()]),
    Pid2 = spawn(?MODULE, init, [Depth - 1, self()]),
    main([Pid1,Pid2], ParentPid).

main(Pids, ParentPid) ->
    workload(5000),
    gather(Pids),
    ParentPid ! {self(),stop},
    ok.

gather([]) -> ok;
gather([Pid|Pids]) -> receive {Pid,stop} -> gather(Pids) end.

workload(0) -> ok;
workload(N) -> math:sin(2), workload(N - 1).
