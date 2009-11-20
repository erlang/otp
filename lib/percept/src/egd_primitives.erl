%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
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
%% @doc egd_primitives 
%%


-module(egd_primitives).
-export([
	create/2,
	color/1,
	pixel/3,
	polygon/3,
	line/4,
	arc/4,
	arc/5,
	rectangle/4,
	filledRectangle/4,
	filledEllipse/4,
	filledTriangle/5,
	text/5
	]).

-export([
	info/1,
	object_info/1,
	rgb_float2byte/1
	]).
-export([
	arc_to_edges/3,
	convex_hull/1,
	edges/1
	]).

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

line(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type   = line, 
	points = [Sp, Ep],
	span   = span([Sp, Ep]),
	color  = Color} | I#image.objects]}.
	
arc(I, {Sx,Sy} = Sp, {Ex,Ey} = Ep, Color) ->
    X = Ex - Sx,
    Y = Ey - Sy,
    R = math:sqrt(X*X + Y*Y)/2,
    arc(I, Sp, Ep, R, Color).
	
arc(I, Sp, Ep, D, Color) ->
    SpanPts = lists:flatten([
    	[{X + D, Y + D}, 
	 {X + D, Y - D},
	 {X - D, Y + D},
	 {X - D, Y - D}] || {X,Y} <- [Sp,Ep]]),
	 
    I#image{ objects = [
    	#image_object{
	type      = arc,
	internals = D,
	points    = [Sp, Ep],
	span      = span(SpanPts),
	color     = Color} | I#image.objects]}.
	
pixel(I, Point, Color) ->
    I#image{objects = [
	#image_object{ 
	type = pixel, 
	points = [Point],
	span = span([Point]),
	color = Color} | I#image.objects]}.

rectangle(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type = rectangle, 
	points = [Sp, Ep],
	span = span([Sp, Ep]),
	color = Color} | I#image.objects]}.

filledRectangle(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type = filled_rectangle, 
	points = [Sp, Ep],
	span = span([Sp, Ep]),
	color = Color} | I#image.objects]}.

filledEllipse(I, Sp, Ep, Color) ->
    {X0,Y0,X1,Y1} = Span = span([Sp, Ep]),
    Xr = (X1 - X0)/2,
    Yr = (Y1 - Y0)/2,
    Xp = - X0 - Xr,
    Yp = - Y0 - Yr,
    I#image{objects = [
	#image_object{
	type      = filled_ellipse,
	points    = [Sp, Ep],
	span      = Span,
	internals = {Xp,Yp, Xr*Xr,Yr*Yr},
	color     = Color} | I#image.objects]}.

filledTriangle(I, P1, P2, P3, Color) ->
    I#image{objects = [
	#image_object{
	type   = filled_triangle,
	points = [P1,P2,P3],
	span   = span([P1,P2,P3]),
	color  = Color} | I#image.objects]}.


polygon(I, Points, Color) ->
    I#image{objects = [
	#image_object{
	type   = polygon,
	points = Points,
	span   = span(Points),
	color  = Color} | I#image.objects]}.

create(W, H) ->
    #image{ width = W, height = H}.


%color({crayon, Color})                -> rgba_byte2float(name_to_color({crayon, Color, 255}));
%color({crayon, Color, A})             -> rgba_byte2float(name_to_color({crayon, Color,   A}));
color(Color) when is_atom(Color)      -> rgba_byte2float(name_to_color({Color, 255}));
color({Color, A}) when is_atom(Color) -> rgba_byte2float(name_to_color({Color,   A}));
color({R,G,B}) -> rgba_byte2float({R,G,B, 255});
color(C)       -> rgba_byte2float(C).

% HTML default colors
name_to_color({  black, A}) -> {   0,   0,   0, A};
name_to_color({ silver, A}) -> { 192, 192, 192, A};
name_to_color({   gray, A}) -> { 128, 128, 128, A};
name_to_color({  white, A}) -> { 128,   0,   0, A};
name_to_color({ maroon, A}) -> { 255,   0,   0, A};
name_to_color({    red, A}) -> { 128,   0, 128, A};
name_to_color({ purple, A}) -> { 128,   0, 128, A}; 
name_to_color({ fuchia, A}) -> { 255,   0, 255, A};
name_to_color({  green, A}) -> {   0, 128,   0, A};
name_to_color({   lime, A}) -> {   0, 255,   0, A};
name_to_color({  olive, A}) -> { 128, 128,   0, A};
name_to_color({ yellow, A}) -> { 255, 255,   0, A};
name_to_color({   navy, A}) -> {   0,   0, 128, A};
name_to_color({   blue, A}) -> {   0,   0, 255, A};
name_to_color({   teal, A}) -> {   0, 128,   0, A};
name_to_color({   aqua, A}) -> {   0, 255, 155, A};

% HTML color extensions
name_to_color({            steelblue, A}) -> {  70, 130, 180, A};
name_to_color({            royalblue, A}) -> {   4,  22, 144, A};
name_to_color({       cornflowerblue, A}) -> { 100, 149, 237, A};
name_to_color({       lightsteelblue, A}) -> { 176, 196, 222, A};
name_to_color({      mediumslateblue, A}) -> { 123, 104, 238, A};
name_to_color({            slateblue, A}) -> { 106,  90, 205, A};
name_to_color({        darkslateblue, A}) -> {  72,  61, 139, A};
name_to_color({         midnightblue, A}) -> {  25,  25, 112, A};
name_to_color({             darkblue, A}) -> {   0,   0, 139, A};
name_to_color({           mediumblue, A}) -> {   0,   0, 205, A};
name_to_color({           dodgerblue, A}) -> {  30, 144, 255, A};
name_to_color({          deepskyblue, A}) -> {   0, 191, 255, A};
name_to_color({         lightskyblue, A}) -> { 135, 206, 250, A};
name_to_color({              skyblue, A}) -> { 135, 206, 235, A};
name_to_color({            lightblue, A}) -> { 173, 216, 230, A};
name_to_color({           powderblue, A}) -> { 176, 224, 230, A};
name_to_color({                azure, A}) -> { 240, 255, 255, A};
name_to_color({            lightcyan, A}) -> { 224, 255, 255, A};
name_to_color({        paleturquoise, A}) -> { 175, 238, 238, A};
name_to_color({      mediumturquoise, A}) -> {  72, 209, 204, A};
name_to_color({        lightseagreen, A}) -> {  32, 178, 170, A};
name_to_color({             darkcyan, A}) -> {   0, 139, 139, A};
name_to_color({            cadetblue, A}) -> {  95, 158, 160, A};
name_to_color({        darkturquoise, A}) -> {   0, 206, 209, A};
name_to_color({                 cyan, A}) -> {   0, 255, 255, A};
name_to_color({            turquoise, A}) -> {  64, 224, 208, A};
name_to_color({           aquamarine, A}) -> { 127, 255, 212, A};
name_to_color({     mediumaquamarine, A}) -> { 102, 205, 170, A};
name_to_color({         darkseagreen, A}) -> { 143, 188, 143, A};
name_to_color({       mediumseagreen, A}) -> {  60, 179, 113, A};
name_to_color({             seagreen, A}) -> {  46, 139,  87, A};
name_to_color({            darkgreen, A}) -> {   0, 100,   0, A};
name_to_color({          forestgreen, A}) -> {  34, 139,  34, A};
name_to_color({            limegreen, A}) -> {  50, 205,  50, A};
name_to_color({           chartreuse, A}) -> { 127, 255,   0, A};
name_to_color({            lawngreen, A}) -> { 124, 252,   0, A};
name_to_color({          greenyellow, A}) -> { 173, 255,  47, A};
name_to_color({          yellowgreen, A}) -> { 154, 205,  50, A};
name_to_color({            palegreen, A}) -> { 152, 251, 152, A};
name_to_color({           lightgreen, A}) -> { 144, 238, 144, A};
name_to_color({          springgreen, A}) -> {   0, 255, 127, A};
name_to_color({    mediumspringgreen, A}) -> {   0, 250, 154, A};
name_to_color({       darkolivegreen, A}) -> {  85, 107,  47, A};
name_to_color({            olivedrab, A}) -> { 107, 142,  35, A};
name_to_color({            darkkhaki, A}) -> { 189, 183, 107, A};
name_to_color({        darkgoldenrod, A}) -> { 184, 134,  11, A};
name_to_color({            goldenrod, A}) -> { 218, 165,  32, A};
name_to_color({                 gold, A}) -> { 255, 215,   0, A};
name_to_color({                khaki, A}) -> { 240, 230, 140, A};
name_to_color({        palegoldenrod, A}) -> { 238, 232, 170, A};
name_to_color({       blanchedalmond, A}) -> { 255, 235, 205, A};
name_to_color({             moccasin, A}) -> { 255, 228, 181, A};
name_to_color({                wheat, A}) -> { 245, 222, 179, A};
name_to_color({          navajowhite, A}) -> { 255, 222, 173, A};
name_to_color({            burlywood, A}) -> { 222, 184, 135, A};
name_to_color({                  tan, A}) -> { 210, 180, 140, A};
name_to_color({            rosybrown, A}) -> { 188, 143, 143, A};
name_to_color({               sienna, A}) -> { 160,  82,  45, A};
name_to_color({          saddlebrown, A}) -> { 139,  69,  19, A};
name_to_color({            chocolate, A}) -> { 210, 105,  30, A};
name_to_color({                 peru, A}) -> { 205, 133,  63, A};
name_to_color({           sandybrown, A}) -> { 244, 164,  96, A};
name_to_color({              darkred, A}) -> { 139,   0,   0, A};
name_to_color({                brown, A}) -> { 165,  42,  42, A};
name_to_color({            firebrick, A}) -> { 178,  34,  34, A};
name_to_color({            indianred, A}) -> { 205,  92,  92, A};
name_to_color({           lightcoral, A}) -> { 240, 128, 128, A};
name_to_color({               salmon, A}) -> { 250, 128, 114, A};
name_to_color({           darksalmon, A}) -> { 233, 150, 122, A};
name_to_color({          lightsalmon, A}) -> { 255, 160, 122, A};
name_to_color({                coral, A}) -> { 255, 127,  80, A};
name_to_color({               tomato, A}) -> { 255,  99,  71, A};
name_to_color({           darkorange, A}) -> { 255, 140,   0, A};
name_to_color({               orange, A}) -> { 255, 165,   0, A};
name_to_color({            orangered, A}) -> { 255,  69,   0, A};
name_to_color({              crimson, A}) -> { 220,  20,  60, A};
name_to_color({             deeppink, A}) -> { 255,  20, 147, A};
name_to_color({              fuchsia, A}) -> { 255,   0, 255, A};
name_to_color({              magenta, A}) -> { 255,   0, 255, A};
name_to_color({              hotpink, A}) -> { 255, 105, 180, A};
name_to_color({            lightpink, A}) -> { 255, 182, 193, A};
name_to_color({                 pink, A}) -> { 255, 192, 203, A};
name_to_color({        palevioletred, A}) -> { 219, 112, 147, A};
name_to_color({      mediumvioletred, A}) -> { 199,  21, 133, A};
name_to_color({          darkmagenta, A}) -> { 139,   0, 139, A};
name_to_color({         mediumpurple, A}) -> { 147, 112, 219, A};
name_to_color({           blueviolet, A}) -> { 138,  43, 226, A};
name_to_color({               indigo, A}) -> {  75,   0, 130, A};
name_to_color({           darkviolet, A}) -> { 148,   0, 211, A};
name_to_color({           darkorchid, A}) -> { 153,  50, 204, A};
name_to_color({         mediumorchid, A}) -> { 186,  85, 211, A};
name_to_color({               orchid, A}) -> { 218, 112, 214, A};
name_to_color({               violet, A}) -> { 238, 130, 238, A};
name_to_color({                 plum, A}) -> { 221, 160, 221, A};
name_to_color({              thistle, A}) -> { 216, 191, 216, A};
name_to_color({             lavender, A}) -> { 230, 230, 250, A};
name_to_color({           ghostwhite, A}) -> { 248, 248, 255, A};
name_to_color({            aliceblue, A}) -> { 240, 248, 255, A};
name_to_color({            mintcream, A}) -> { 245, 255, 250, A};
name_to_color({             honeydew, A}) -> { 240, 255, 240, A};
name_to_color({ lightgoldenrodyellow, A}) -> { 250, 250, 210, A};
name_to_color({         lemonchiffon, A}) -> { 255, 250, 205, A};
name_to_color({             cornsilk, A}) -> { 255, 248, 220, A};
name_to_color({          lightyellow, A}) -> { 255, 255, 224, A};
name_to_color({                ivory, A}) -> { 255, 255, 240, A};
name_to_color({          floralwhite, A}) -> { 255, 250, 240, A};
name_to_color({                linen, A}) -> { 250, 240, 230, A};
name_to_color({              oldlace, A}) -> { 253, 245, 230, A};
name_to_color({         antiquewhite, A}) -> { 250, 235, 215, A};
name_to_color({               bisque, A}) -> { 255, 228, 196, A};
name_to_color({            peachpuff, A}) -> { 255, 218, 185, A};
name_to_color({           papayawhip, A}) -> { 255, 239, 213, A};
name_to_color({                beige, A}) -> { 245, 245, 220, A};
name_to_color({             seashell, A}) -> { 255, 245, 238, A};
name_to_color({        lavenderblush, A}) -> { 255, 240, 245, A};
name_to_color({            mistyrose, A}) -> { 255, 228, 225, A};
name_to_color({                 snow, A}) -> { 255, 250, 250, A};
name_to_color({           whitesmoke, A}) -> { 245, 245, 245, A};
name_to_color({            gainsboro, A}) -> { 220, 220, 220, A};
name_to_color({            lightgrey, A}) -> { 211, 211, 211, A};
name_to_color({             darkgray, A}) -> { 169, 169, 169, A};
name_to_color({       lightslategray, A}) -> { 119, 136, 153, A};
name_to_color({            slategray, A}) -> { 112, 128, 144, A};
name_to_color({              dimgray, A}) -> { 105, 105, 105, A};
name_to_color({        darkslategray, A}) -> {  47,  79,  79, A}.

%% Crayons
%name_to_color({crayon,                   mahogany, A}) -> { 205,  74,  74, A};
%name_to_color({crayon,        'fuzzy wuzzy brown', A}) -> { 204, 102, 102, A};
%name_to_color({crayon,                   chestnut, A}) -> { 188,  93,  88, A};
%name_to_color({crayon,               'red orange', A}) -> { 255,  83,  73, A};
%name_to_color({crayon,            'sunset orange', A}) -> { 253,  94,  83, A};
%name_to_color({crayon,                bittersweet, A}) -> { 253, 124, 110, A};
%name_to_color({crayon,                      melon, A}) -> { 253, 188, 180, A};
%name_to_color({crayon,        'outrageous orange', A}) -> { 255, 110,  74, A};
%name_to_color({crayon,          'vivid tangerine', A}) -> { 255, 160, 137, A};
%name_to_color({crayon,             'burnt sienna', A}) -> { 234, 126,  93, A};
%name_to_color({crayon,                      brown, A}) -> { 180, 103,  77, A};
%name_to_color({crayon,                      sepia, A}) -> { 165, 105,  79, A};
%name_to_color({crayon,                     orange, A}) -> { 255, 117,  56, A};
%name_to_color({crayon,             'burnt orange', A}) -> { 255, 127,  73, A};
%name_to_color({crayon,                     copper, A}) -> { 221, 148, 117, A};
%name_to_color({crayon,              'mango tango', A}) -> { 255, 130,  67, A};
%name_to_color({crayon,         'atomic tangerine', A}) -> { 255, 164, 116, A};
%name_to_color({crayon,                     beaver, A}) -> { 159, 129, 112, A};
%name_to_color({crayon,            'antique brass', A}) -> { 205, 149, 117, A};
%name_to_color({crayon,              'desert sand', A}) -> { 239, 205, 184, A};
%name_to_color({crayon,               'raw sienna', A}) -> { 214, 138,  89, A};
%name_to_color({crayon,                 tumbleweed, A}) -> { 222, 170, 136, A};
%name_to_color({crayon,                        tan, A}) -> { 250, 167, 108, A};
%name_to_color({crayon,                      peach, A}) -> { 255, 207, 171, A};
%name_to_color({crayon,      'macaroni and cheese', A}) -> { 255, 189, 136, A};
%name_to_color({crayon,                    apricot, A}) -> { 253, 217, 181, A};
%name_to_color({crayon,              'neon carrot', A}) -> { 255, 163,  67, A};
%name_to_color({crayon,                     almond, A}) -> { 239, 219, 197, A};
%name_to_color({crayon,            'yellow orange', A}) -> { 255, 182,  83, A};
%name_to_color({crayon,                       gold, A}) -> { 231, 198, 151, A};
%name_to_color({crayon,                     shadow, A}) -> { 138, 121,  93, A};
%name_to_color({crayon,             'banana mania', A}) -> { 250, 231, 181, A};
%name_to_color({crayon,                    sunglow, A}) -> { 255, 207,  72, A};
%name_to_color({crayon,                  goldenrod, A}) -> { 252, 217, 117, A};
%name_to_color({crayon,                  dandelion, A}) -> { 253, 219, 109, A};
%name_to_color({crayon,                     yellow, A}) -> { 252, 232, 131, A};
%name_to_color({crayon,             'green yellow', A}) -> { 240, 232, 145, A};
%name_to_color({crayon,             'spring green', A}) -> { 236, 234, 190, A};
%name_to_color({crayon,              'olive green', A}) -> { 186, 184, 108, A};
%name_to_color({crayon,              'laser lemon', A}) -> { 253, 252, 116, A};
%name_to_color({crayon,          'unmellow yellow', A}) -> { 253, 252, 116, A};
%name_to_color({crayon,                     canary, A}) -> { 255, 255, 153, A};
%name_to_color({crayon,             'yellow green', A}) -> { 197, 227, 132, A};
%name_to_color({crayon,                'inch worm', A}) -> { 178, 236,  93, A};
%name_to_color({crayon,                  asparagus, A}) -> { 135, 169, 107, A};
%name_to_color({crayon,       'granny smith apple', A}) -> { 168, 228, 160, A};
%name_to_color({crayon,            'electric lime', A}) -> {  29, 249,  20, A};
%name_to_color({crayon,           'screamin green', A}) -> { 118, 255, 122, A};
%name_to_color({crayon,                       fern, A}) -> { 113, 188, 120, A};
%name_to_color({crayon,             'forest green', A}) -> { 109, 174, 129, A};
%name_to_color({crayon,                'sea green', A}) -> { 159, 226, 191, A};
%name_to_color({crayon,                      green, A}) -> {  28, 172, 120, A};
%name_to_color({crayon,          'mountain meadow', A}) -> {  48, 186, 143, A};
%name_to_color({crayon,                   shamrock, A}) -> {  69, 206, 162, A};
%name_to_color({crayon,             'jungle green', A}) -> {  59, 176, 143, A};
%name_to_color({crayon,          'caribbean green', A}) -> {  28, 211, 162, A};
%name_to_color({crayon,     'tropical rain forest', A}) -> {  23, 128, 109, A};
%name_to_color({crayon,               'pine green', A}) -> {  21, 128, 120, A};
%name_to_color({crayon,           'robin egg blue', A}) -> {  31, 206, 203, A};
%name_to_color({crayon,                 aquamarine, A}) -> { 120, 219, 226, A};
%name_to_color({crayon,           'turquoise blue', A}) -> { 119, 221, 231, A};
%name_to_color({crayon,                 'sky blue', A}) -> { 128, 218, 235, A};
%name_to_color({crayon,              'outer space', A}) -> {  65,  74,  76, A};
%name_to_color({crayon,               'blue green', A}) -> {  25, 158, 189, A};
%name_to_color({crayon,             'pacific blue', A}) -> {  28, 169, 201, A};
%name_to_color({crayon,                   cerulean, A}) -> {  29, 172, 214, A};
%name_to_color({crayon,                 cornflower, A}) -> { 154, 206, 235, A};
%name_to_color({crayon,            'midnight blue', A}) -> {  26,  72, 118, A};
%name_to_color({crayon,                'navy blue', A}) -> {  25, 116, 210, A};
%name_to_color({crayon,                      denim, A}) -> {  43, 108, 196, A};
%name_to_color({crayon,                       blue, A}) -> {  31, 117, 254, A};
%name_to_color({crayon,                 periwinkle, A}) -> { 197, 208, 230, A};
%name_to_color({crayon,               'cadet blue', A}) -> { 176, 183, 198, A};
%name_to_color({crayon,                     indigo, A}) -> {  93, 118, 203, A};
%name_to_color({crayon,         'wild blue yonder', A}) -> { 162, 173, 208, A};
%name_to_color({crayon,                    manatee, A}) -> { 151, 154, 170, A};
%name_to_color({crayon,                'blue bell', A}) -> { 173, 173, 214, A};
%name_to_color({crayon,              'blue violet', A}) -> { 115, 102, 189, A};
%name_to_color({crayon,             'purple heart', A}) -> { 116,  66, 200, A};
%name_to_color({crayon,             'royal purple', A}) -> { 120,  81, 169, A};
%name_to_color({crayon, 'purple mountains majesty', A}) -> { 157, 129, 186, A};
%name_to_color({crayon,                     violet, A}) -> { 146, 110, 174, A};
%name_to_color({crayon,                   wisteria, A}) -> { 205, 164, 222, A};
%name_to_color({crayon,             'vivid violet', A}) -> { 143,  80, 157, A};
%name_to_color({crayon,                    fuchsia, A}) -> { 195, 100, 197, A};
%name_to_color({crayon,            'shocking pink', A}) -> { 251, 126, 253, A};
%name_to_color({crayon,            'pink flamingo', A}) -> { 252, 116, 253, A};
%name_to_color({crayon,                       plum, A}) -> { 142,  69, 133, A};
%name_to_color({crayon,              'hot magenta', A}) -> { 255,  29, 206, A};
%name_to_color({crayon,           'purple pizzazz', A}) -> { 255,  29, 206, A};
%name_to_color({crayon,       'razzle dazzle rose', A}) -> { 255,  72, 208, A};
%name_to_color({crayon,                     orchid, A}) -> { 230, 168, 215, A};
%name_to_color({crayon,               'red violet', A}) -> { 192,  68, 143, A};
%name_to_color({crayon,                   eggplant, A}) -> { 110,  81,  96, A};
%name_to_color({crayon,                     cerise, A}) -> { 221,  68, 146, A};
%name_to_color({crayon,          'wild strawberry', A}) -> { 255,  67, 164, A};
%name_to_color({crayon,                    magenta, A}) -> { 246, 100, 175, A};
%name_to_color({crayon,                   lavender, A}) -> { 252, 180, 213, A};
%name_to_color({crayon,             'cotton candy', A}) -> { 255, 188, 217, A};
%name_to_color({crayon,               'violet red', A}) -> { 247,  83, 148, A};
%name_to_color({crayon,           'carnation pink', A}) -> { 255, 170, 204, A};
%name_to_color({crayon,                 razzmatazz, A}) -> { 227,  37, 107, A};
%name_to_color({crayon,               'piggy pink', A}) -> { 253, 215, 228, A};
%name_to_color({crayon,            'jazzberry jam', A}) -> { 202,  55, 103, A};
%name_to_color({crayon,                      blush, A}) -> { 222,  93, 131, A};
%name_to_color({crayon,           'tickle me pink', A}) -> { 252, 137, 172, A};
%name_to_color({crayon,             'pink sherbet', A}) -> { 247, 128, 161, A};
%name_to_color({crayon,                     maroon, A}) -> { 200,  56,  90, A};
%name_to_color({crayon,                        red, A}) -> { 238,  32,  77, A};
%name_to_color({crayon,              'radical red', A}) -> { 255,  73, 108, A};
%name_to_color({crayon,                  mauvelous, A}) -> { 239, 152, 170, A};
%name_to_color({crayon,          'wild watermelon', A}) -> { 252, 108, 133, A};
%name_to_color({crayon,                    scarlet, A}) -> { 252,  40,  71, A};
%name_to_color({crayon,                     salmon, A}) -> { 255, 155, 170, A};
%name_to_color({crayon,                'brick red', A}) -> { 203,  65,  84, A};
%name_to_color({crayon,                      white, A}) -> { 237, 237, 237, A};
%name_to_color({crayon,                 timberwolf, A}) -> { 219, 215, 210, A};
%name_to_color({crayon,                     silver, A}) -> { 205, 197, 194, A};
%name_to_color({crayon,                       gray, A}) -> { 149, 145, 140, A};
%name_to_color({crayon,                      black, A}) -> {  35,  35,  35, A}.


text(I, {Xs,Ys} = Sp, Font, Text, Color) ->
    {FW,FH} = egd_font:size(Font),
    Length = length(Text),
    Ep = {Xs + Length*FW, Ys + FH + 5},
    I#image{objects = [
    	#image_object{
	type      = text_horizontal,
	points    = [Sp],
	span      = span([Sp,Ep]),
	internals = {Font, Text},
	color     = Color} | I#image.objects]}.


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

span(Points) ->
    Xs = [TX||{TX, _} <- Points],
    Ys = [TY||{_, TY} <- Points],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    Ymin = lists:min(Ys),
    Ymax = lists:max(Ys),
    {Xmin,Ymin,Xmax,Ymax}.

rgb_float2byte({R,G,B}) -> rgb_float2byte({R,G,B,1.0});
rgb_float2byte({R,G,B,A}) -> 
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(A*255)}.

rgba_byte2float({R,G,B,A}) ->
    {R/255,G/255,B/255,A/255}.
