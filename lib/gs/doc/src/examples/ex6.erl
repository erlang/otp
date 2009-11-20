-module(ex6).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0,init/0]).

start() ->
    spawn(ex6,init,[]).

init() ->
    S = gs:start(),
    W = gs:create(window,S,[{map,true},{keypress,true},
                            {buttonpress,true},{motion,true}]),
    gs:create(button,W,[{label,{text,"PressMe"}},{enter,true},
                        {leave,true}]),
    event_loop().

event_loop() ->
    receive
        X ->
            io:format("Got event: ~w~n",[X]),
            event_loop()
    end.
