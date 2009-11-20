-module(ex5).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0, init/0, b1/0, b2/0]).

start() ->
    spawn(ex5, init, []).

init() ->
    S = gs:start(),
    W = gs:create(window,S,[{map,true}]),       
    gs:create(button,W,[{label,{text,"Button1"}},{data,{ex5,b1,[]}},{y,0}]),
    gs:create(button,W,[{label,{text,"Button2"}},{data,{ex5,b2,[]}},{y,40}]),
    loop().

loop()->
    receive
        {gs,_,click,{M,F,A},_} -> % any button pressed
            apply(M,F,A),
            loop()
    end.

b1() ->
    io:format("Button 1 pressed!~n",[]).
b2() ->
    io:format("Button 2 pressed!~n",[]).                
