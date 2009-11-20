-module(ex15).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $ ').

-export([start/0,init/0]).

start() -> spawn(ex15, init, []).

init() ->
    I=gs:start(),
    Win=gs:create(window, I,
                  [{width, 400},{height, 250},
                   {title,"Font Demo"},{map, true}]),
    E = gs:create(canvas, can1,Win,
		  [{x,0},{y, 0},{width,400},{height,250}]),
    Fonts = [{times,19},{screen,16},{helvetica,bold,21},
	     {symbol,12},{times,[bold,italic],33},{courier,6}],
    show_fonts_in_boxes(Fonts,0),
    receive
	{gs,_Id,destroy,_Data,_Arg} -> bye
    end.

show_fonts_in_boxes([],_) -> done;
show_fonts_in_boxes([Font|Fonts],Y) ->
    Txt = io_lib:format("Hi! ~p",[Font]),
    {Width,Height} = gs:read(can1,{font_wh,{Font,Txt}}),
    Y2=Y+Height+2,
    gs:create(rectangle,can1,[{coords,[{0,Y},{Width,Y2}]}]),
    gs:create(text,can1,[{font,Font},{text,Txt},{coords,[{0,Y+1}]}]),
    show_fonts_in_boxes(Fonts,Y2+1).
