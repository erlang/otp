%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
%% ------------------------------------------------------------
%%  Entry Demo
%% ------------------------------------------------------------

-module(entry_demo).

-export([start/0,init/1]).

start() ->
    spawn(entry_demo,init,[self()]),
    receive
	{entry_reply,Reply} -> Reply
    end.

init(Pid) ->
    S=gs:start(),
    Win=gs:window(S,[{title,"Entry Demo"},{width,150},{height,100}]),
    gs:create(label,Win,[{width,150},{label,{text,"What's your name?"}}]),
    gs:create(entry,entry,Win,[{x,10},{y,30},{width,130},{keypress,true}]),
    gs:create(button,ok,Win,[{width,45},{y,60},{x,10},{label,{text,"Ok"}}]),
    gs:create(button,cancel,Win,[{width,60},{y,60},{x,80},{label,{text,"Cancel"}}]),
    gs:config(Win,{map,true}),
    loop(Pid).

loop(Pid) ->
    receive
	{gs,entry,keypress,_,['Return'|_]} ->
	    Text=gs:read(entry,text),
	    Pid ! {entry_reply,{name,Text}};
	{gs,entry,keypress,_,_} ->
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

%% ----------------------------------------
%% done
