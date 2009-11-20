-module(ex7).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([mk_window/0]).

mk_window() ->
    S= gs:start(),
    Win= gs:create(window,S,[{motion,true},{map,true}]),
    gs:config(Win,[{configure,true},{keypress,true}]),
    gs:config(Win,[{buttonpress,true}]),
    gs:config(Win,[{buttonrelease,true}]),
    event_loop(Win).

event_loop(Win) ->      
    receive
        {gs,Win,motion,Data,[X,Y | Rest]} ->
            %% mouse moved to position X Y
            io:format("mouse moved to X:~w  Y:~w~n",[X,Y]);
        {gs,Win,configure,Data,[W,H | Rest]} ->
            %% window was resized by user
            io:format("window resized W:~w  H:~w~n",[W,H]);
        {gs,Win,buttonpress,Data,[1,X,Y | Rest]} -> 
            %% button 1 was pressed at location X Y
            io:format("button 1 pressed X:~w  Y:~w~n",[X,Y]);
        {gs,Win,buttonrelease,Data,[_,X,Y | Rest]} ->
            %% Any button (1-3) was released over X Y
            io:format("Any button released X:~w  Y:~w~n",[X,Y]);
        {gs,Win,keypress,Data,[a | Rest]} -> 
            %% key `a' was pressed in window
            io:format("key a was pressed in window~n");
        {gs,Win,keypress,Data,[_,65,1 | Rest]} ->
            %% Key shift-a
            io:format("shift-a was pressed in window~n");
        {gs,Win,keypress,Data,[c,_,_,1 | Rest]} ->
            %% CTRL_C pressed
            io:format("CTRL_C was pressed in window~n");
        {gs,Win,keypress,Data, ['Return' | Rest]} ->
            %% Return key pressed
            io:format("Return key was pressed in window~n")
        end,
    event_loop(Win).
