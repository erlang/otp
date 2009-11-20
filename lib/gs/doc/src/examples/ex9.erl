-module(ex9).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0,init/1]).

start() ->
    spawn(ex9, init, [self()]),
    receive
        {entry_reply, Reply} -> Reply
    end.

init(Pid) ->
    S = gs:start(),
    Win = gs:create(window,S,[{title,"Entry Demo"},
                              {width,150},{height,100}]),
    gs:create(label,Win,[{label,{text,"What's your name?"}},
                         {width,150}]),
    gs:create(entry,entry,Win,[{x,10},{y,30},{width,130},
                               {keypress,true}]),
    gs:create(button,ok,Win,[{width,45},{y,60},{x,10},
                             {label,{text,"Ok"}}]),
    gs:create(button,cancel,Win,[{width,60},{y,60},{x,80},
                                 {label,{text,"Cancel"}}]),
    gs:config(Win,{map,true}),
    loop(Pid).

loop(Pid) ->
    receive
        {gs,entry,keypress,_,['Return'|_]} ->
            Text=gs:read(entry,text),
            Pid ! {entry_reply,{name,Text}};
        {gs,entry,keypress,_,_} -> % all other keypresses
            loop(Pid);
        {gs,ok,click,_,_} ->
            Text=gs:read(entry,text),
            Pid ! {entry_reply,{name,Text}};
        {gs,cancel,click,_,_} ->
            Pid ! {entry_reply,cancel};
        X ->
            io:format("Got X=~w~n",[X]),
            loop(Pid)
    end.
