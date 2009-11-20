-module(ex8).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0]).

start() ->
    gs:window(win,gs:start(),{map,true}),
    gs:radiobutton(rb1,win,[{label,{text,"rb1"}},{value,a},{y,0}]),
    gs:radiobutton(rb2,win,[{label,{text,"rb2"}},{value,a},{y,30}]),
    gs:radiobutton(rb3,win,[{label,{text,"rb3"}},{value,b},{y,60}]),
    rb_loop().

rb_loop() ->
    receive
        {gs,Any_Rb,click,Data,[Text, Grp, a | Rest]} ->
            io:format("either rb1 or rb2 is on.~n",[]),
            rb_loop();
        {gs,rb3,click,Data,[Text, Grp, b | Rest]} ->
            io:format("rb3 is selected.~n",[]),
            rb_loop()
    end.
