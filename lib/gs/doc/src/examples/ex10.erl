-module(ex10).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $ ').

-export([start/0, init/3]).

start() ->
    start("Pick a fruit:", 
          [apple, banana, lemon, orange, strawberry, 
           mango, kiwi, pear, cherry,pineapple,peach,apricot]).

start(Text,Items) ->
    spawn(ex10,init,[self(),Text,Items]),       
    receive
        {browser,Result} -> Result
    end.

init(Pid,Text,Items) ->
    S=gs:start(),
    Win=gs:window(S,[{width,250},{height,270},
                     {title,"Browser"}]),
    Lbl=gs:label(Win,[{label,{text,Text}},{width,250}]),
    Entry=gs:entry(Win,[{y,35},{width,240},{x,5},
                        {keypress,true},
                        {setfocus,true}]),
    Lb=gs:listbox(Win,[{x,5},{y,65},{width,160},
                       {height,195},{vscroll,right},
                       {click,true},{doubleclick,true}]),
    Ok=gs:button(Win,[{label,{text,"OK"}},
                      {width,40},{x,185},{y,175}]),
    Cancel=gs:button(Win,[{label,{text,"Cancel"}},
                          {x,175},{y,225},{width,65}]),
    gs:config(Lb,[{items,Items}]),
    gs:config(Win,{map,true}),
    browser_loop(Pid,Ok,Cancel,Entry,Lb).

browser_loop(Pid,Ok,Cancel,Entry,Lb) ->
    receive
        {gs,Ok,click,_,_} ->
            Txt=gs:read(Entry,text),
            Pid ! {browser,{ok,Txt}};
        {gs,Cancel,click,_,_} ->
            Pid ! {browser,cancel};
        {gs,Entry,keypress,_,['Return'|_]} ->
            Txt=gs:read(Entry,text),
            Pid ! {browser,{ok,Txt}};
        {gs,Entry,keypress,_,_} ->
            browser_loop(Pid,Ok,Cancel,Entry,Lb);
        {gs,Lb,click,_,[Idx, Txt|_]} ->
            gs:config(Entry,{text,Txt}),
            browser_loop(Pid,Ok,Cancel,Entry,Lb);
        {gs,Lb,doubleclick,_,[Idx, Txt|_]} ->
            Pid ! {browser,{ok,Txt}};
        {gs,_,destroy,_,_} ->
            Pid ! {browser,cancel};
        X ->
            io:format("Got X=~w~n",[X]),
            browser_loop(Pid,Ok,Cancel,Entry,Lb)
    end.
