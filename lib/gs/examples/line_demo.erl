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
%% A simple demo showing a line
%% bouncing within a window.
%% ------------------------------------------------------------

-module(line_demo).

-export([start/0,init/0,line/3]).

start() ->
    spawn(line_demo,init,[]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Line Demo"},{width,300},{height,300},{map,true}]), 
    C= gs:canvas(W,[{width,300},{height,300},{bg,blue}]),
    gs:button(W,[{label,{text,"Quit"}},{width,40},{bg,yellow}]),
    Line2 = gs:line(C,[{coords,[{0,0},{50,50}]},{fg,white},{width,3}]),
    line(Line2,{100,100,9.5,5},{0,0,-6,-8.4}).
    
    

line(Line,{X1,Y1,DX1,DY1},{X2,Y2,DX2,DY2}) ->
    {NX1,NDX1} = cc(X1,DX1),
    {NY1,NDY1} = cc(Y1,DY1),
    {NX2,NDX2} = cc(X2,DX2),
    {NY2,NDY2} = cc(Y2,DY2),
    gs:config(Line,{coords,[{NX1,NY1},{NX2,NY2}]}),    
    receive
	{gs,_,click,_,_} -> exit(normal);
	{gs,_,destroy,_,_} -> exit(normal)
    after 50 ->
	    true
    end,
    line(Line,{NX1,NY1,NDX1,NDY1},{NX2,NY2,NDX2,NDY2}).

cc(X,DX) ->
    if 
	DX>0 ->
	    if 
		X<300 ->
		    {X+DX,DX};
		x>=300 ->
		    {X-DX,-DX}
	    end;
	DX<0 ->
	    if
		X>0 ->
		    {X+DX,DX};
		X=<0 ->
		    {X-DX,-DX}
	    end
    end.

%% ------------------------------------------------------------
