%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

-module(percept_image).
-export([	proc_lifetime/5,
		percentage/3,
		graph/3, 
		graph/4, 
		activities/3, 
		activities/4]).
-record(graph_area, {x = 0, y = 0, width, height}).
-compile(inline).

%%% -------------------------------------
%%% GRAF
%%% -------------------------------------

%% graph(Widht, Height, Range, Data)

graph(Width, Height, {RXmin, RYmin, RXmax, RYmax}, Data) ->
    Data2 = [{X, Y1 + Y2} || {X, Y1, Y2} <- Data],
    MinMax = percept_analyzer:minmax(Data2),
    {Xmin, Ymin, Xmax, Ymax} = MinMax, 
    graf1(Width, Height,{	lists:min([RXmin, Xmin]), 
    				lists:min([RYmin, Ymin]),
    				lists:max([RXmax, Xmax]), 
				lists:max([RYmax, Ymax])}, Data).

%% graph(Widht, Height, Data) = Image
%% In:
%%	Width = integer(),
%%	Height = integer(),
%%	Data = [{Time, Procs, Ports}]
%%	Time = float()
%%	Procs = integer()
%%	Ports = integer()
%% Out:
%%	Image = binary()

graph(Width, Height, Data) ->
    Data2 = [{X, Y1 + Y2} || {X, Y1, Y2} <- Data],
    Bounds = percept_analyzer:minmax(Data2),
    graf1(Width, Height, Bounds, Data).

graf1(Width, Height, {Xmin, Ymin, Xmax, Ymax}, Data) ->
    % Calculate areas
    HO = 20,
    GrafArea   = #graph_area{x = HO, y = 4, width = Width - 2*HO, height = Height - 17},
    XticksArea = #graph_area{x = HO, y = Height - 13, width = Width - 2*HO, height = 13},
    YticksArea = #graph_area{x = 1,  y = 4, width = HO, height = Height - 17},
    
    %% Initiate Image

    Image = egd:create(Width, Height),
 
    %% Set colors
    
    Black = egd:color(Image, {0, 0, 0}),
    ProcColor = egd:color(Image, {0, 255, 0}),
    PortColor = egd:color(Image, {255, 0, 0}),
    
    %% Draw graf, xticks and yticks
    draw_graf(Image, Data, {Black, ProcColor, PortColor}, GrafArea, {Xmin, Ymin, Xmax, Ymax}),
    draw_xticks(Image, Black, XticksArea, {Xmin, Xmax}, Data),
    draw_yticks(Image, Black, YticksArea, {Ymin, Ymax}),
    
    %% Kill image and return binaries
    Binary = egd:render(Image, png),
    egd:destroy(Image),
    Binary.

%% draw_graf(Image, Data, Color, GraphArea, DataBounds)
%% Image, port to Image
%% Data, list of three tuple data, (X, Y1, Y2)
%% Color, {ForegroundColor, ProcFillColor, PortFillColor}
%% DataBounds, {Xmin, Ymin, Xmax, Ymax}

draw_graf(Im, Data, Colors, GA = #graph_area{x = X0, y = Y0, width = Width, height = Height}, {Xmin, _Ymin, Xmax, Ymax}) ->
    Dx = (Width)/(Xmax - Xmin),
    Dy = (Height)/(Ymax),
    Plotdata = [{trunc(X0 + X*Dx - Xmin*Dx), trunc(Y0 + Height - Y1*Dy), trunc(Y0 + Height - (Y1 + Y2)*Dy)} || {X, Y1, Y2} <- Data],
    draw_graf(Im, Plotdata, Colors, GA).

draw_graf(Im, [{X1, Yproc1, Yport1}, {X2, Yproc2, Yport2}|Data], C, GA) when X2 - X1 < 1 ->
    draw_graf(Im, [{X1, [{Yproc2, Yport2},{Yproc1, Yport1}]}|Data], C, GA);

draw_graf(Im, [{X1, Ys1}, {X2, Yproc2, Yport2}|Data], C, GA) when X2 - X1 < 1, is_list(Ys1) ->
    draw_graf(Im, [{X1, [{Yproc2, Yport2}|Ys1]}|Data], C, GA);

draw_graf(Im, [{X1, Yproc1, Yport1}, {X2, Yproc2, Yport2}|Data], C = {B, PrC, PoC}, GA = #graph_area{y = Y0, height = H})  ->
    GyZero  = trunc(Y0 + H),
    egd:filledRectangle(Im, {X1, GyZero}, {X2, Yproc1}, PrC),
    egd:filledRectangle(Im, {X1, Yproc1}, {X2, Yport1}, PoC),
    egd:line(Im, {X1, Yport1}, {X2, Yport1}, B), % top line
    egd:line(Im, {X1, Yport2}, {X1, Yport1}, B), % right line
    egd:line(Im, {X2, Yport1}, {X2, Yport2}, B), % right line
    draw_graf(Im, [{X2, Yproc2, Yport2}|Data], C, GA);

draw_graf(Im, [{X1, Ys1 = [{Yproc1,Yport1}|_]}, {X2, Yproc2, Yport2}|Data], C = {B, PrC, PoC}, GA = #graph_area{y = Y0, height = H})  ->
    GyZero  = trunc(Y0 + H),
    Yprocs = [Yp || {Yp, _} <- Ys1],
    Yports = [Yp || {_, Yp} <- Ys1],

    YprMin = lists:min(Yprocs),
    YprMax = lists:max(Yprocs),
    YpoMax = lists:max(Yports),
    egd:filledRectangle(Im, {X1, GyZero}, {X2, Yproc1}, PrC),
    egd:filledRectangle(Im, {X1, Yproc1}, {X2, Yport1}, PoC),
    egd:filledRectangle(Im, {X1, Yport1}, {X2, Yport1}, B), % top line
    egd:filledRectangle(Im, {X2, Yport1}, {X2, Yport2}, B), % right line

    egd:filledRectangle(Im, {X1, GyZero}, {X1, YprMin}, PrC), % left proc green line
    egd:filledRectangle(Im, {X1, YprMax}, {X1, YpoMax}, PoC), % left port line
    egd:filledRectangle(Im, {X1, YprMax}, {X1, YprMin}, B),
     
    draw_graf(Im, [{X2, Yproc2, Yport2}|Data], C, GA);
draw_graf(_, _, _, _) -> ok.

draw_xticks(Image, Color, XticksArea, {Xmin, Xmax}, Data) ->
    #graph_area{x = X0, y = Y0, width = Width} = XticksArea,
    
    DX = Width/(Xmax - Xmin),
    Offset = X0 - Xmin*DX, 
    Y = trunc(Y0),
    Font = load_font(),
    {FontW, _FontH} = egd_font:size(Font),
    egd:filledRectangle(Image, {trunc(X0), Y}, {trunc(X0 + Width), Y}, Color), 
    lists:foldl(
    	fun ({X,_,_}, PX) ->
	    X1 = trunc(Offset + X*DX),
	    
	    % Optimization:
	    % if offset has past half the previous text
	    % start checking this text
	    
	    if 
	    	X1 > PX ->
		    Text = lists:flatten(io_lib:format("~.3f", [float(X)])),
		    TextLength = length(Text),
		    TextWidth = TextLength*FontW,
		    Spacing = 2,
		    if 
		    	X1 > PX + round(TextWidth/2) + Spacing ->
		    	    egd:line(Image, {X1, Y - 3}, {X1, Y + 3}, Color),
		    	    text(Image, {X1 - round(TextWidth/2), Y + 2}, Font, Text, Color),
			    X1 + round(TextWidth/2) + Spacing;
			true ->
			    PX
		    end;
		true ->
		    PX
	    end
	end, 0, Data).

draw_yticks(Im, Color, TickArea, {_,Ymax}) ->
    #graph_area{x = X0, y = Y0, width = Width, height = Height} = TickArea,
    Font = load_font(),
    X = trunc(X0 + Width),
    Dy = (Height)/(Ymax),
    Yts = if 
	Height/(Ymax*12) < 1.0 -> round(1 + Ymax*15/Height);
	true -> 1
    end,
    egd:filledRectangle(Im, {X, trunc(0 + Y0)}, {X, trunc(Y0 + Height)}, Color),
    draw_yticks0(Im, Font, Color, 0, Yts, Ymax, {X, Height, Dy}).

draw_yticks0(Im, Font, Color, Yi, Yts, Ymax, Area) when Yi < Ymax -> 
    {X, Height, Dy} = Area, 
    Y = round(Height - (Yi*Dy) + 3),

    egd:filledRectangle(Im, {X - 3, Y}, {X + 3, Y}, Color), 
    Text = lists:flatten(io_lib:format("~p", [Yi])),
    text(Im, {0, Y - 4}, Font, Text, Color),
    draw_yticks0(Im, Font, Color, Yi + Yts, Yts, Ymax, Area);
draw_yticks0(_, _, _, _, _, _, _) -> ok.

%%% -------------------------------------
%%% ACTIVITIES
%%% -------------------------------------

%% activities(Width, Height, Range, Activities) -> Binary
%% In:
%%	Width = integer()
%%	Height = integer()
%%	Range = {float(), float()}
%%	Activities = [{float(), active | inactive}]
%% Out:
%%	Binary = binary()

activities(Width, Height, {UXmin, UXmax}, Activities) ->
    Xs = [ X || {X,_} <- Activities],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    activities0(Width, Height, {lists:min([Xmin, UXmin]), lists:max([UXmax, Xmax])}, Activities).

activities(Width, Height, Activities) ->
    Xs = [ X || {X,_} <- Activities],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    activities0(Width, Height, {Xmin, Xmax}, Activities).

activities0(Width, Height, {Xmin, Xmax}, Activities) ->
    Image = egd:create(Width, Height),
    Grey = egd:color(Image, {200, 200, 200}),
    HO = 20,
    ActivityArea = #graph_area{x = HO, y = 0, width = Width - 2*HO, height = Height},
    egd:filledRectangle(Image, {0, 0}, {Width, Height}, Grey),
    draw_activity(Image, {Xmin, Xmax}, ActivityArea, Activities),
    Binary = egd:render(Image, png),
    egd:destroy(Image),
    Binary.

draw_activity(Image, {Xmin, Xmax}, Area = #graph_area{ width = Width }, Acts) ->
    White = egd:color({255, 255, 255}),
    Green = egd:color({0,250, 0}),
    Black = egd:color({0, 0, 0}),

    Dx    = Width/(Xmax - Xmin),

    draw_activity(Image, {Xmin, Xmax}, Area, {White, Green, Black}, Dx, Acts).

draw_activity(_, _, _, _, _, [_]) -> ok;
draw_activity(Image, {Xmin, Xmax}, Area = #graph_area{ height = Height, x = X0 }, {Cw, Cg, Cb}, Dx, [{Xa1, State}, {Xa2, Act2} | Acts]) ->
    X1 = erlang:trunc(X0 + Dx*Xa1 - Xmin*Dx),
    X2 = erlang:trunc(X0 + Dx*Xa2 - Xmin*Dx),

    case State of
	inactive ->
	    egd:filledRectangle(Image, {X1, 0}, {X2, Height - 1}, Cw),
	    egd:rectangle(Image, {X1, 0}, {X2, Height - 1}, Cb);
	active ->
	    egd:filledRectangle(Image, {X1, 0}, {X2, Height - 1}, Cg),
	    egd:rectangle(Image, {X1, 0}, {X2, Height - 1}, Cb)
    end,
    draw_activity(Image, {Xmin, Xmax}, Area, {Cw, Cg, Cb}, Dx, [{Xa2, Act2} | Acts]).

 

%%% -------------------------------------
%%% Process lifetime
%%% Used by processes page
%%% -------------------------------------

proc_lifetime(Width, Height, Start, End, ProfileTime) ->
    Im = egd:create(round(Width), round(Height)),
    Black = egd:color(Im, {0, 0, 0}),
    Green = egd:color(Im, {0, 255, 0}),

    % Ratio and coordinates

    DX = (Width-1)/ProfileTime,
    X1 = round(DX*Start),
    X2 = round(DX*End),

    % Paint
    egd:filledRectangle(Im, {X1, 0}, {X2, Height - 1}, Green),
    egd:rectangle(Im, {X1, 0}, {X2, Height - 1}, Black),

    Binary = egd:render(Im, png),
    egd:destroy(Im),
    Binary.

%%% -------------------------------------
%%% Percentage
%%% Used by process_info page
%%% Percentage should be 0.0 -> 1.0
%%% -------------------------------------
percentage(Width, Height, Percentage) ->
    Im = egd:create(round(Width), round(Height)),
    Font = load_font(),
    Black = egd:color(Im, {0, 0, 0}),
    Green = egd:color(Im, {0, 255, 0}),

    % Ratio and coordinates

    X = round(Width - 1 - Percentage*(Width - 1)),

    % Paint
    egd:filledRectangle(Im, {X, 0}, {Width - 1, Height - 1}, Green),
    {FontW, _} = egd_font:size(Font), 
    String = lists:flatten(io_lib:format("~.10B %", [round(100*Percentage)])),

    text(	Im, 
		{round(Width/2 - (FontW*length(String)/2)), 0}, 
    		Font,
		String,
		Black),
    egd:rectangle(Im, {X, 0}, {Width - 1, Height - 1}, Black),
    
    Binary = egd:render(Im, png),
    egd:destroy(Im),
    Binary.


load_font() ->
    Filename = filename:join([code:priv_dir(percept),"fonts", "6x11_latin1.wingsfont"]),
    egd_font:load(Filename).
    
text(Image, {X,Y}, Font, Text, Color) ->
    egd:text(Image, {X,Y-2}, Font, Text, Color).
