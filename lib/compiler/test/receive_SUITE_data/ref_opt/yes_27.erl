-module(yes_27).
-export([?MODULE/0,do_multi_call/0]).

?MODULE() ->
    ok.

do_multi_call() ->
    %% The calls to make_ref/0 and erlang:monitor/2 will be
    %% in the same block.
    Tag = make_ref(),
    Receiver = spawn(name@host, fun() -> ok end),
    Mref = monitor(process, Receiver, []),
    Receiver ! {self(),Tag},
    receive
	{'DOWN',Mref,_,_,{_Receiver,Tag,Result}} ->
	    Result;
	{'DOWN',Mref,_,_,Reason} ->
	    exit(Reason)
    end.
