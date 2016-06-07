%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%% @doc egd_primitives 
%%


-module(egd_primitives).
-export([create/2,
         color/1,
         pixel/3,
         polygon/3,
         line/4,
         line/5,
         arc/4,
         arc/5,
         rectangle/4,
         filledRectangle/4,
         filledEllipse/4,
         filledTriangle/5,
         text/5]).

-export([info/1,
         object_info/1,
         rgb_float2byte/1]).

-export([arc_to_edges/3,
         convex_hull/1,
         edges/1]).

-include("egd.hrl").

%% API info
info(I) ->
    W = I#image.width, H = I#image.height,
    io:format("Dimensions: ~p x ~p~n", [W,H]),
    io:format("Number of image objects: ~p~n", [length(I#image.objects)]),
    TotalPoints = info_objects(I#image.objects,0),
    io:format("Total points: ~p [~p %]~n", [TotalPoints, 100*TotalPoints/(W*H)]),
    ok.

info_objects([],N) -> N;
info_objects([O | Os],N) ->
    Points = length(O#image_object.points),
info_objects(Os,N+Points).

object_info(O) ->
    io:format("Object information: ~p~n", [O#image_object.type]),
    io:format("- Number of points: ~p~n", [length(O#image_object.points)]),
    io:format("-     Bounding box: ~p~n", [O#image_object.span]),
    io:format("-            Color: ~p~n", [O#image_object.color]),
    ok.

%% interface functions

line(#image{objects=Os}=I, Sp, Ep, Color) ->
    line(#image{objects=Os}=I, Sp, Ep, 1, Color).
	
line(#image{objects=Os}=I, Sp, Ep, Wd, Color) ->
    I#image{objects=[#image_object{
                        internals = Wd,
                        type   = line,
                        points = [Sp, Ep],
                        span   = span([Sp, Ep]),
                        color  = Color}|Os]}.
	
arc(I, {Sx,Sy} = Sp, {Ex,Ey} = Ep, Color) ->
    X = Ex - Sx,
    Y = Ey - Sy,
    R = math:sqrt(X*X + Y*Y)/2,
    arc(I, Sp, Ep, R, Color).
	
arc(#image{objects=Os}=I, Sp, Ep, D, Color) ->
    SpanPts = lists:flatten([
    	[{X + D, Y + D}, 
	 {X + D, Y - D},
	 {X - D, Y + D},
	 {X - D, Y - D}] || {X,Y} <- [Sp,Ep]]),
	 
    I#image{objects=[#image_object{
                        internals = D,
                        type   = arc,
                        points = [Sp, Ep],
                        span   = span(SpanPts),
                        color  = Color}|Os]}.
	
pixel(#image{objects=Os}=I, Point, Color) ->
    I#image{objects=[#image_object{
                        type   = pixel,
                        points = [Point],
                        span   = span([Point]),
                        color  = Color}|Os]}.

rectangle(#image{objects=Os}=I, Sp, Ep, Color) ->
    I#image{objects=[#image_object{
                        type   = rectangle,
                        points = [Sp, Ep],
                        span   = span([Sp, Ep]),
                        color  = Color}|Os]}.

filledRectangle(#image{objects=Os}=I, Sp, Ep, Color) ->
    I#image{objects=[#image_object{
                        type   = filled_rectangle,
                        points = [Sp, Ep],
                        span   = span([Sp, Ep]),
                        color  = Color}|Os]}.

filledEllipse(#image{objects=Os}=I, Sp, Ep, Color) ->
    {X0,Y0,X1,Y1} = Span = span([Sp, Ep]),
    Xr = (X1 - X0)/2,
    Yr = (Y1 - Y0)/2,
    Xp = - X0 - Xr,
    Yp = - Y0 - Yr,
    I#image{objects=[#image_object{
                        internals = {Xp,Yp, Xr*Xr,Yr*Yr},
                        type   = filled_ellipse,
                        points = [Sp, Ep],
                        span   = Span,
                        color  = Color}|Os]}.

filledTriangle(#image{objects=Os}=I, P1, P2, P3, Color) ->
    I#image{objects=[#image_object{
                        type   = filled_triangle,
                        points = [P1,P2,P3],
                        span   = span([P1,P2,P3]),
                        color  = Color}|Os]}.

polygon(#image{objects=Os}=I, Points, Color) ->
    I#image{objects=[#image_object{
                        type   = polygon,
                        points = Points,
                        span   = span(Points),
                        color  = Color}|Os]}.

text(#image{objects=Os}=I, {Xs,Ys}=Sp, Font, Text, Color) ->
    {FW,FH} = egd_font:size(Font),
    Length = length(Text),
    Ep = {Xs + Length*FW, Ys + FH + 5},
    I#image{objects=[#image_object{
                        internals = {Font, Text},
                        type   = text_horizontal,
                        points = [Sp],
                        span   = span([Sp,Ep]),
                        color  = Color}|Os]}.

create(W, H) ->
    #image{width = W, height = H}.

color(Color) when is_atom(Color)      -> rgba_byte2float(name_to_color(Color, 255));
color({Color, A}) when is_atom(Color) -> rgba_byte2float(name_to_color(Color,   A));
color({R,G,B}) -> rgba_byte2float({R,G,B, 255});
color(C)       -> rgba_byte2float(C).

name_to_color(Color, A) ->
    case Color of
        %% HTML default colors
        black  -> {  0,   0,   0, A};
        silver -> {192, 192, 192, A};
        gray   -> {128, 128, 128, A};
        white  -> {128,   0,   0, A};
        maroon -> {255,   0,   0, A};
        red    -> {128,   0, 128, A};
        purple -> {128,   0, 128, A};
        fuchia -> {255,   0, 255, A};
        green  -> {  0, 128,   0, A};
        lime   -> {  0, 255,   0, A};
        olive  -> {128, 128,   0, A};
        yellow -> {255, 255,   0, A};
        navy   -> {  0,   0, 128, A};
        blue   -> {  0,   0, 255, A};
        teal   -> {  0, 128,   0, A};
        aqua   -> {  0, 255, 155, A};

        %% HTML color extensions
        steelblue        -> { 70, 130, 180, A};
        royalblue        -> {  4,  22, 144, A};
        cornflowerblue   -> {100, 149, 237, A};
        lightsteelblue   -> {176, 196, 222, A};
        mediumslateblue  -> {123, 104, 238, A};
        slateblue        -> {106,  90, 205, A};
        darkslateblue    -> { 72,  61, 139, A};
        midnightblue     -> { 25,  25, 112, A};
        darkblue         -> {  0,   0, 139, A};
        mediumblue       -> {  0,   0, 205, A};
        dodgerblue       -> { 30, 144, 255, A};
        deepskyblue      -> {  0, 191, 255, A};
        lightskyblue     -> {135, 206, 250, A};
        skyblue          -> {135, 206, 235, A};
        lightblue        -> {173, 216, 230, A};
        powderblue       -> {176, 224, 230, A};
        azure            -> {240, 255, 255, A};
        lightcyan        -> {224, 255, 255, A};
        paleturquoise    -> {175, 238, 238, A};
        mediumturquoise  -> { 72, 209, 204, A};
        lightseagreen    -> { 32, 178, 170, A};
        darkcyan         -> {  0, 139, 139, A};
        cadetblue        -> { 95, 158, 160, A};
        darkturquoise    -> {  0, 206, 209, A};
        cyan             -> {  0, 255, 255, A};
        turquoise        -> { 64, 224, 208, A};
        aquamarine       -> {127, 255, 212, A};
        mediumaquamarine -> {102, 205, 170, A};
        darkseagreen     -> {143, 188, 143, A};
        mediumseagreen   -> { 60, 179, 113, A};
        seagreen         -> { 46, 139,  87, A};
        darkgreen        -> {  0, 100,   0, A};
        forestgreen      -> { 34, 139,  34, A};
        limegreen        -> { 50, 205,  50, A};
        chartreuse       -> {127, 255,   0, A};
        lawngreen        -> {124, 252,   0, A};
        greenyellow      -> {173, 255,  47, A};
        yellowgreen      -> {154, 205,  50, A};
        palegreen        -> {152, 251, 152, A};
        lightgreen       -> {144, 238, 144, A};
        springgreen      -> {  0, 255, 127, A};
        darkolivegreen   -> { 85, 107,  47, A};
        olivedrab        -> {107, 142,  35, A};
        darkkhaki        -> {189, 183, 107, A};
        darkgoldenrod    -> {184, 134,  11, A};
        goldenrod        -> {218, 165,  32, A};
        gold             -> {255, 215,   0, A};
        khaki            -> {240, 230, 140, A};
        palegoldenrod    -> {238, 232, 170, A};
        blanchedalmond   -> {255, 235, 205, A};
        moccasin         -> {255, 228, 181, A};
        wheat            -> {245, 222, 179, A};
        navajowhite      -> {255, 222, 173, A};
        burlywood        -> {222, 184, 135, A};
        tan              -> {210, 180, 140, A};
        rosybrown        -> {188, 143, 143, A};
        sienna           -> {160,  82,  45, A};
        saddlebrown      -> {139,  69,  19, A};
        chocolate        -> {210, 105,  30, A};
        peru             -> {205, 133,  63, A};
        sandybrown       -> {244, 164,  96, A};
        darkred          -> {139,   0,   0, A};
        brown            -> {165,  42,  42, A};
        firebrick        -> {178,  34,  34, A};
        indianred        -> {205,  92,  92, A};
        lightcoral       -> {240, 128, 128, A};
        salmon           -> {250, 128, 114, A};
        darksalmon       -> {233, 150, 122, A};
        lightsalmon      -> {255, 160, 122, A};
        coral            -> {255, 127,  80, A};
        tomato           -> {255,  99,  71, A};
        darkorange       -> {255, 140,   0, A};
        orange           -> {255, 165,   0, A};
        orangered        -> {255,  69,   0, A};
        crimson          -> {220,  20,  60, A};
        deeppink         -> {255,  20, 147, A};
        fuchsia          -> {255,   0, 255, A};
        magenta          -> {255,   0, 255, A};
        hotpink          -> {255, 105, 180, A};
        lightpink        -> {255, 182, 193, A};
        pink             -> {255, 192, 203, A};
        palevioletred    -> {219, 112, 147, A};
        mediumvioletred  -> {199,  21, 133, A};
        darkmagenta      -> {139,   0, 139, A};
        mediumpurple     -> {147, 112, 219, A};
        blueviolet       -> {138,  43, 226, A};
        indigo           -> { 75,   0, 130, A};
        darkviolet       -> {148,   0, 211, A};
        darkorchid       -> {153,  50, 204, A};
        mediumorchid     -> {186,  85, 211, A};
        orchid           -> {218, 112, 214, A};
        violet           -> {238, 130, 238, A};
        plum             -> {221, 160, 221, A};
        thistle          -> {216, 191, 216, A};
        lavender         -> {230, 230, 250, A};
        ghostwhite       -> {248, 248, 255, A};
        aliceblue        -> {240, 248, 255, A};
        mintcream        -> {245, 255, 250, A};
        honeydew         -> {240, 255, 240, A};
        lemonchiffon     -> {255, 250, 205, A};
        cornsilk         -> {255, 248, 220, A};
        lightyellow      -> {255, 255, 224, A};
        ivory            -> {255, 255, 240, A};
        floralwhite      -> {255, 250, 240, A};
        linen            -> {250, 240, 230, A};
        oldlace          -> {253, 245, 230, A};
        antiquewhite     -> {250, 235, 215, A};
        bisque           -> {255, 228, 196, A};
        peachpuff        -> {255, 218, 185, A};
        papayawhip       -> {255, 239, 213, A};
        beige            -> {245, 245, 220, A};
        seashell         -> {255, 245, 238, A};
        lavenderblush    -> {255, 240, 245, A};
        mistyrose        -> {255, 228, 225, A};
        snow             -> {255, 250, 250, A};
        whitesmoke       -> {245, 245, 245, A};
        gainsboro        -> {220, 220, 220, A};
        lightgrey        -> {211, 211, 211, A};
        darkgray         -> {169, 169, 169, A};
        lightslategray   -> {119, 136, 153, A};
        slategray        -> {112, 128, 144, A};
        dimgray          -> {105, 105, 105, A};
        darkslategray    -> { 47,  79,  79, A};
        mediumspringgreen -> {  0, 250, 154, A};
        lightgoldenrodyellow -> {250, 250, 210, A}
    end.


%%% Generic transformations

%% arc_to_edges
%% In:
%%	P1  :: point(),
%%	P2  :: point(),
%%	D   :: float(),
%% Out:
%%	Res :: [edges()]

arc_to_edges(P0, P1, D) when abs(D) < 0.5 -> [{P0,P1}];
arc_to_edges({X0,Y0}, {X1,Y1}, D) ->
    Vx = X1 - X0,
    Vy = Y1 - Y0,

    Mx = X0 + 0.5 * Vx,
    My = Y0 + 0.5 * Vy,
    
    % Scale V by Rs
    L  = math:sqrt(Vx*Vx + Vy*Vy),
    Sx = D*Vx/L, 
    Sy = D*Vy/L,
    
    Bx = trunc(Mx - Sy),
    By = trunc(My + Sx),

    arc_to_edges({X0,Y0}, {Bx,By}, D/4) ++ arc_to_edges({Bx,By}, {X1,Y1}, D/4).

%% edges
%% In:
%%	Pts :: [point()]
%% Out:
%%	Edges :: [{point(),point()}]

edges([]) -> [];
edges([P0|_] = Pts)  -> edges(Pts, P0,[]).
edges([P1], P0, Out) -> [{P1,P0}|Out];
edges([P1,P2|Pts],P0,Out) -> edges([P2|Pts],P0,[{P1,P2}|Out]).

%% convex_hull
%% In:
%%	Ps  :: [point()]
%% Out:
%%	Res :: [point()]

convex_hull(Ps) ->
    P0 = lower_right(Ps),
    [P1|Ps1] = lists:sort(fun
    	(P2,P1) ->
	    case point_side({P1,P0},P2) of
	    	left -> true;
		_    -> false
	    end
	end, Ps -- [P0]),
    convex_hull(Ps1, [P1,P0]).

convex_hull([], W) -> W;
convex_hull([P|Pts], [P1,P2|W]) ->
    case point_side({P2,P1},P) of
    	left -> convex_hull(Pts, [P,P1,P2|W]);
	_    -> convex_hull([P|Pts], [P2|W])
    end.
	
lower_right([P|Pts]) -> lower_right(P, Pts).
lower_right(P, []) -> P;
lower_right({X0,Y0}, [{_,Y}|Pts]) when Y < Y0 -> lower_right({X0,Y0}, Pts);
lower_right({X0,Y0}, [{X,Y}|Pts]) when X < X0, Y < Y0 -> lower_right({X0,Y0}, Pts);
lower_right(_,[P|Pts]) -> lower_right(P, Pts).

point_side({{X0,Y0}, {X1, Y1}}, {X2, Y2}) -> point_side((X1 - X0)*(Y2 - Y0) - (X2 - X0)*(Y1 - Y0)).
point_side(D) when D > 0 -> left;
point_side(D) when D < 0 -> right;
point_side(_) -> on_line.

%% AUX

span([{X0,Y0}|Points]) ->
    span(Points,X0,Y0,X0,Y0).
span([{X0,Y0}|Points],Xmin,Ymin,Xmax,Ymax) ->
    span(Points,erlang:min(Xmin,X0),
                erlang:min(Ymin,Y0),
                erlang:max(Xmax,X0),
                erlang:max(Ymax,Y0));
span([],Xmin,Ymin,Xmax,Ymax) ->
    {Xmin,Ymin,Xmax,Ymax}.


rgb_float2byte({R,G,B}) -> rgb_float2byte({R,G,B,1.0});
rgb_float2byte({R,G,B,A}) -> 
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(A*255)}.

rgba_byte2float({R,G,B,A}) ->
    {R/255,G/255,B/255,A/255}.
