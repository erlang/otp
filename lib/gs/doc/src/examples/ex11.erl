-module(ex11).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $ ').

-export([start/0,init/0]).

start() ->
    spawn(ex11,init,[]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Color Demo"},
                    {width,300},{height,195}]), 
    B=gs:button(W,[{label,{image,"die_icon"}},{x,271},{y,166},
                   {width,30}]),
    gs:config(B,[{bg,yellow},{fg,hotpink1},{data,quit}]),
    gs:scale(W,[{text,"Red"},{y,0},{range,{0,255}},
                {orient,horizontal},
                {height,65},{data,red},{pos,42}]),
    gs:scale(W,[{text,"Blue"},{y,65},{range,{0,255}},
                {orient,horizontal},
                {height,65},{data,blue},{pos,42}]),
    gs:scale(W,[{text,"Green"},{y,130},{range,{0,255}},
                {orient,horizontal},
                {height,65},{data,green},{pos,42}]),
    gs:config(W,{map,true}),
    loop(W,0,0,0).

loop(W,R,G,B) ->
    gs:config(W,{bg,{R,G,B}}),
    receive
        {gs,_,click,red,[New_R|_]} ->
            loop(W,New_R,G,B);
        {gs,_,click,green,[New_G|_]} ->
            loop(W,R,New_G,B);
        {gs,_,click,blue,[New_B|_]} ->
            loop(W,R,G,New_B);
        {gs,_,click,quit,_} ->
            true;
        {gs,W,destroy,_,_} ->
            true
    end.
