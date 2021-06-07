-module(yes_16).
-export([?MODULE/0,ttb_stop/1]).

?MODULE() ->
    ok.

ttb_stop(MetaPid) ->
    Delivered = erlang:trace_delivered(all),
    receive
	{trace_delivered,all,Delivered} -> ok
    end,
    %% The erlang:monitor/2 call will be in the same block
    %% as the remove message instruction for the previous
    %% receive.
    Ref = erlang:monitor(process,MetaPid),
    MetaPid ! stop,
    receive {'DOWN', Ref, process, MetaPid, _Info} -> ok end.
