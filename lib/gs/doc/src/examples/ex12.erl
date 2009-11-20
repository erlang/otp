-module(ex12).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0,init/0]).

start() -> spawn(ex12, init, []).

init() ->
    R=[{window,[{width,200},{height,200},{title,"grid"},{map, true}],
	{grid, [{x,10},{y,10},{height,180},{width,180},{columnwidths,[80,60]},
		{rows,{1, 20}}],
	 [{gridline,[{text,{1,"NAME"}},{text,{2,"PHONE"}},
		    {font,{screen,bold,12}},{row,1},{click,false}]},
	  {gridline,[{text,{1,"Adam"}},{text,{2,"1234"}},{row,2}]},
	  {gridline,[{text,{1,"Beata"}},{text,{2,"4321"}},{row,3}]},
	  {gridline,[{text,{1,"Thomas"}},{text,{2,"1432"}},{row,4}]},
	  {gridline,[{text,{1,"Bond"}},{text,{2,"007"}},{row,5}]},
	  {gridline,[{text,{1,"King"}},{text,{2,"112"}},{row,6}]},
	  {gridline,[{text,{1,"Eva"}},{text,{2,"4123"}},{row,7}]}]}}],
    gs:create_tree(gs:start(),R),
    loop().

loop() ->
    receive
	{gs,_Win,destroy,_Data,_Args} -> bye;
	{gs,_Gridline,click,_Data,[Col,Row,Text|_]} ->
	    io:format("Click at col:~p row:~p text:~p~n",[Col,Row,Text]),
	    loop();
        Msg ->
            io:format("Got ~p~n",[Msg]),
	    loop()
    end.
