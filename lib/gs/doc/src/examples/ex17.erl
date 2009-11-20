-module(ex17).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/1 $ ').

-export([start/0,init/0]).

start() -> spawn(ex17, init, []).

init() ->
    WH = [{width,200},{height,300}],
    Win = gs:window(gs:start(),[{map,true},{configure,true},
				{title,"Packer Demo"}|WH]),
    gs:frame(packer,Win,[{packer_x,[{stretch,1,50},{stretch,2,50},
				    {stretch,1,50}]},
			 {packer_y,[{fixed,30},{stretch,1}]}]),
    gs:button(packer,[{label,{text,"left"}},{pack_xy,{1,1}}]),
    gs:button(packer,[{label,{text,"middle"}},{pack_xy,{2,1}}]),
    gs:button(packer,[{label,{text,"right"}},{pack_xy,{3,1}}]),
    gs:editor(packer,[{pack_xy,{{1,3},2}},{vscroll,true},{hscroll,true}]),
    gs:config(packer,WH), % refresh to initial size
    loop().

loop() ->
    receive
	{gs,_Id,destroy,_Data,_Arg} -> bye;
	{gs,_Id,configure,_Data,[W,H|_]} ->
	    gs:config(packer,[{width,W},{height,H}]), % repack
	    loop();
	Other ->
	    io:format("loop got: ~p~n",[Other]),
	    loop()
    end.
