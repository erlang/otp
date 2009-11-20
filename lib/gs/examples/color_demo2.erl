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
%% Another simple demo for choosing
%% colors in a window.
%% ------------------------------------------------------------

-module(color_demo2).

-export([start/0,init/0]).

start() ->
    spawn(color_demo2,init,[]).


init() ->
    S=gs:start(),
    Win = gs:create(window,S,[{width,380},{height,430},{motion,true},
			      {buttonpress,true}]),
    gs:create(button,b1,Win,[{x,200},{label,{text,""}}]),
    gs:create(button,Win,[{data,quit},{label,{text,"Quit"}},{bg,yellow},
			  {width,40}]),
    Side = 200,
    Tri = equi_tri(100, 400, Side),
    %% draw_lines(Win, Tri),
    gs:config(Win,[{title,"Color Demo 2"},{map,true}]),
    server(Win, Side, Tri, {0,0,0}).

server(Win, Side, [Point1, Point2, Point3], OldCol) ->
    receive
	{gs,Win,motion,_,[X,Y|_]} ->
	    R = col({X,Y}, Point1, Side),
	    G = col({X,Y}, Point2, Side),
	    B = col({X,Y}, Point3, Side),
	    Txt = lists:flatten(io_lib:format("~w ~w ~w",[R,G,B])),
	    gs:config(b1,[{label,{text,Txt}}]),
	    Col = {R, G, B},
	    gs:config(Win, [{bg,Col}]),
	    server(Win, Side, [Point1, Point2, Point3], Col);
	{gs,Win,buttonpress,_,[_X,_Y|_]} ->
	    io:format("{color, ~w}\n", [OldCol]),
	    server(Win, Side, [Point1, Point2, Point3], OldCol);
	{gs,_,click,quit,_} ->
	    exit(die);
	{gs,Win,destroy,_,_} ->
	    exit(die);
	_Any ->
	    server(Win, Side, [Point1, Point2, Point3], OldCol)
    end.


col(Point1, Point2, Side) ->
    D = dist(Point1, Point2),
    %% All FFFFFF is white 
    %% when Col = 255 when D = Side
    %% when Col = 0   when D = 0
    %% Assume Col = A * D + B
    B = 0,
    A = (255 - B)/Side,
    Col = trunc(A*D + B),
    map(Col).

map(X) ->
     X band 255.

equi_tri(X1, Y1, L) ->
    X2 = trunc(X1 + L/2),
    Y2 = trunc(Y1 - L * math:sqrt(3)),
    [{X1,Y1},{X2,Y2},{X1+L,Y1}].

%draw_line(Win, {X1, Y1}, {X2, Y2}) ->
%    gs:create(line,Win,[{coords,[{X1,Y1},{X2,Y2}]},{width,2}]).

%draw_lines(Win, L) ->
%    draw1(Win, L),
%    draw_line(Win, hd(L), lists:last(L)).

%draw1(Win, [X,Y|T]) ->
%    draw_line(Win, X, Y),
%    draw1(Win, [Y|T]);
%draw1(Win, _) ->
%    [].

dist({X1,Y1},{X2,Y2}) ->
    XX = X1 - X2,
    YY = Y1 - Y2,
    math:sqrt(XX*XX+YY*YY).

%% ------------------------------------------------------------
%% end of color_demo2.erl
