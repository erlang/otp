-module(ex16).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $ ').

-export([start/0,init/0]).

start() -> spawn(ex16, init, []).

init() ->
    I=gs:start(),
    Win=gs:create(window, I,
                  [{width, 200},{height, 200},
                   {title,"Default Demo"},{map, true}]),
    gs:create(canvas, can1,Win,
	      [{x,0},{y, 0},{width,200},{height,200},
	       {default,text,{font,{courier,bold,19}}},
	       {default,text,{fg,blue}},
	       {default,rectangle,{fill,red}},{default,text,{text,"Pow!"}},
	       {default,oval,{fill,green}}]),
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    loop().

loop() ->
    receive
	{gs,_Id,destroy,_Data,_Arg} -> bye
    after 500 ->
	    XY = {random:uniform(200),random:uniform(200)},
	    draw(random:uniform(3),XY),
	    loop()
    end.

draw(1,XY) ->
    gs:create(text,can1,[{coords,[XY]}]);
draw(2,XY) ->
    XY2 = {random:uniform(200),random:uniform(200)},
    gs:create(rectangle,can1,[{coords,[XY,XY2]}]);
draw(3,XY) ->
    XY2 = {random:uniform(200),random:uniform(200)},
    gs:create(oval,can1,[{coords,[XY,XY2]}]).
