%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%

%%
%% ------------------------------------------------------------
%% distrib_draw
%% Shows one way of making two nodes draw on the same
%% area.
%% 
%% ------------------------------------------------------------

%% HOW TO USE:
%% 1) start up two nodes with same cookie.
%%    They can be on any machines.
%%
%%    sid.ericsson.se> erl -cookie kaka -name hans
%%
%%    ozzy.ericsson.se> erl -cookie kaka -name greta
%% 
%% 
%% 2) Make them aware of each other.
%%     <greta@ozzy.ericsson.se> net:ping('hans@sid.ericsson.se').
%%
%% 3) Start up distrib_draw from either node.
%%     <greta@ozzy.ericsson.se> distrib_draw('hans@sid.ericsson.se',
%%                                           'greta@ozzy.ericsson.se').
%%  

-module(distrib_draw).
-compile([{nowarn_deprecated_function,{gs,canvas,3}},
          {nowarn_deprecated_function,{gs,config,2}},
          {nowarn_deprecated_function,{gs,line,2}},
          {nowarn_deprecated_function,{gs,start,0}},
          {nowarn_deprecated_function,{gs,window,3}}]).

-export([start/2,init/0]).

start(Node1,Node2) ->
    Pid1=spawn(Node1,distrib_draw,init,[]),
    Pid2=spawn(Node2,distrib_draw,init,[]),
    Pid1 ! {connect,red,Pid2},
    Pid2 ! {connect,green,Pid1}.

init() ->
    process_flag(trap_exit,true),
    S=gs:start(),
    receive
	{connect,Color,Pid} -> 
	    link(Pid),
	    gs:window(win,S,[{buttonpress,true},{buttonrelease,true},
			      {configure,true},{title,Color},{map,true}]),
	    gs:canvas(canvas,win,[{bg,grey},{width,300},{height,200}]),
	    draw0(0,0,Color,Pid)
    after 
	3000 -> exit(timeout)
    end.

%% not drawing state
draw0(X0,Y0,Color,Pid) ->
    receive
	{gs,_,buttonpress,_,[1,X,Y|_]} ->
	    gs:config(win,{motion,true}),
	    draw1(X,Y,Color,Pid);
	{draw,Coords,Col2} ->
	    gs:line(canvas,[{coords,Coords},{width,2},{fg,Col2}]),
	    draw0(X0,Y0,Color,Pid);
	{gs,_,configure,_,[300,200|_]} ->
	    draw0(X0,Y0,Color,Pid);
	{gs,_,configure,_,_} ->
	    gs:config(win,[{width,300},{height,200}]),
	    draw0(X0,Y0,Color,Pid);
	{gs,_,destroy,_,_} -> 
	    exit(normal);
	{'EXIT',_,_} -> 
	    exit(normal);
	_X -> draw1(X0,Y0,Color,Pid)
    end.

%% i'm now drawing
draw1(X0,Y0,Color,Pid) ->
    receive
	{gs,_,motion,_,[X,Y|_]} ->
	    Pid ! {draw,[{X0,Y0},{X,Y}],Color},
	    gs:line(canvas,[{coords,[{X0,Y0},{X,Y}]},{width,2},{fg,Color}]),
	    draw1(X,Y,Color,Pid);
	{draw,Coords,Col2} ->
	    gs:line(canvas,[{coords,Coords},{width,2},{fg,Col2}]),
	    draw1(X0,Y0,Color,Pid);
	{gs,_,buttonrelease,_,[1,X,Y|_]} ->
	    gs:config(win,{motion,false}),
	    draw0(X,Y,Color,Pid);
	{gs,_,configure,_,[300,200|_]} ->
	    draw0(X0,Y0,Color,Pid);
	{gs,_,configure,_,_} ->
	    gs:config(win,[{width,300},{height,200}]),
	    draw0(X0,Y0,Color,Pid);
	{gs,_,destroy,_,_} -> 
	    exit(normal);
	{'EXIT',_,_} -> 
	    exit(normal);
	_X -> draw1(X0,Y0,Color,Pid)
    end.

%% ------------------------------------------------------------
%% end of 'distrib_draw'
