-module(ex4).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([init/0]).

init() ->
    S = gs:start(),
    gs:create(window,win1,S,[{width,300},{height,200}]),
    gs:create(button,b1,win1,[{label, {text,"Button1"}},{y,0}]),
    gs:create(button,b2,win1,[{label, {text,"Button2"}},{y,40}]),
    gs:config(win1, {map,true}),
    loop(). %% look, no args!

loop() ->
    receive
        {gs,b1,click,_,_} -> % button 1 pressed
            io:format("Button 1 pressed!~n",[]),
            loop();
        {gs,b2,click,_,_} -> % button 2 pressed
            io:format("Button 2 pressed!~n",[]),
            loop()
    end.
