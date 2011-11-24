-module(server).
-compile(export_all).

start() ->
    application:start(runtime_tools),
    Pid = spawn(?MODULE,loop,[[], 0]),
    register(server,Pid).

stop() ->
    case lists:member(server, registered()) of
	true ->
	    server ! stop;
	false ->
	    ok
    end.

loop(Data, Num) ->
    receive
	{put,From,Ting} -> timer:sleep(2),
			   received(From,Ting),
			   From ! ok,
			   loop([Ting|Data], Num+1);
	{get,From}      -> From ! Data,
			   loop(Data, Num+1);
	stop            -> stopped;
	clear           -> loop([], Num+1);
	{cnt, From}     -> From ! Num,
			   loop(Data, Num)
    end.

counter() ->
    server ! {cnt, self()},
    receive
	Num ->
	    Num
    end.

received(From, Thing) ->
    case Thing of
	never_send_this_atom ->
	    loop(Thing, 0);
	_ ->
	    {return, 27, Thing, From}
    end.
