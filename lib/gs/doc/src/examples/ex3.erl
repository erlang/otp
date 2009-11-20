-module(ex3).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([init/0]).

init() ->
    S = gs:start(),
    W = gs:create(window,S,[{width,300},{height,200}]),
    B1 = gs:create(button,W,[{label, {text,"Button1"}},{y,0}]),
    B2 = gs:create(button,W,[{label, {text,"Button2"}},{y,40}]),
    gs:config(W, {map,true}),
    loop(B1,B2).

loop(B1,B2) ->
    receive
        {gs,B1,click,_Data,_Arg} -> % button 1 pressed
            io:format("Button 1 pressed!~n",[]),
            loop(B1,B2);
        {gs,B2,click,_Data,_Arg} -> % button 2 pressed
            io:format("Button 2 pressed!~n",[]),
            loop(B1,B2)
    end.
