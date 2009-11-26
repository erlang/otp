%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

-module(egd_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
	image_create_and_destroy/1, 
	image_shape/1, 
	image_primitives/1,
	image_colors/1, 
	image_font/1,
	image_png_compliant/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_suite(Config) when is_list(Config) ->
    random:seed(now()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{max_size, 800}, {watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [	
	image_create_and_destroy, 
	image_shape, 
	image_primitives,
	image_colors, 
	image_font, 
	image_png_compliant
    ].

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

image_create_and_destroy(suite) ->
    [];
image_create_and_destroy(doc) ->
    ["Image creation and destroy test."];
image_create_and_destroy(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    ?line Image = egd:create(W, H),
    ?line ok = egd:destroy(Image),
    ok.

image_colors(suite) ->
    [];
image_colors(doc) ->
    ["Image color test."];
image_colors(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    ?line Image = egd:create(W, H),
    put(image_size, {W,H}),

    RGB = get_rgb(),
    ?line Black = egd:color({0,0,0}),
    ?line Red = egd:color({255,0,0}),
    ?line Green = egd:color({0,255,0}),
    ?line Blue = egd:color({0,0,255}),
    ?line Random = egd:color(Image, RGB),  

    ?line ok = egd:line(Image, get_point(), get_point(), Random),
    ?line ok = egd:line(Image, get_point(), get_point(), Red),
    ?line ok = egd:line(Image, get_point(), get_point(), Green),
    ?line ok = egd:line(Image, get_point(), get_point(), Black),
    ?line ok = egd:line(Image, get_point(), get_point(), Blue),

    HtmlDefaultNames = [black,silver,gray,white,maroon,red,
    	purple,fuchia,green,lime,olive,yellow,navy,blue,teal,
      	aqua],
    
    lists:foreach(fun
    	(ColorName) ->
	    ?line Color = egd:color(ColorName),
	    ?line ok    = egd:line(Image, get_point(), get_point(), Color)
	end, HtmlDefaultNames),
 
    ?line <<_/binary>>  = egd:render(Image),
    ?line ok = egd:destroy(Image),
    erase(image_size),
    ok.

image_shape(suite) ->
    [];
image_shape(doc) ->
    ["Image shape api test."];
image_shape(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    
    ?line Fgc = egd:color({255,0,0}),

    ?line ok = egd:line(Im, get_point(), get_point(), Fgc), 
    ?line ok = egd:rectangle(Im, get_point(), get_point(), Fgc),
    ?line ok = egd:filledEllipse(Im, get_point(), get_point(), Fgc),
    ?line ok = egd:arc(Im, get_point(), get_point(), Fgc),
    ?line ok = egd:arc(Im, get_point(), get_point(), 100, Fgc),
    
    Pt1 = get_point(),
    Pt2 = get_point(), 

    ?line ok = egd:filledRectangle(Im, Pt1, Pt2, Fgc),

    ?line Bitmap = egd:render(Im, raw_bitmap),

    ?line ok = bitmap_point_has_color(Bitmap, {W,H}, Pt2, Fgc),
    ?line ok = bitmap_point_has_color(Bitmap, {W,H}, Pt1, Fgc),

    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_primitives(suite) ->
    [];
image_primitives(doc) ->
    ["Image shape api test."];
image_primitives(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),

    ?line Im0 = egd_primitives:create(W, H),
    ?line Fgc = egd:color({25,25,255}),
    ?line Bgc = egd:color({0,250,25}),

    ?line Im1 = lists:foldl(fun
    	({Function, Arguments}, Im) ->
	    ?line erlang:apply(egd_primitives, Function, [Im|Arguments])
	end, Im0, 
	[{Fs, [get_point(), get_point(), Bgc]} || Fs <- [line, rectangle, filledEllipse, arc]] ++
	[{pixel,          [get_point(), Bgc]},
	 {filledTriangle, [get_point(), get_point(), get_point(), Bgc]}]),

    Pt1 = get_point(),
    Pt2 = get_point(), 

    ?line Im2 = egd_primitives:filledRectangle(Im1, Pt1, Pt2, Fgc),

    ?line Bitmap = egd_render:binary(Im2, opaque),

    ?line ok = bitmap_point_has_color(Bitmap, {W,H}, Pt2, Fgc),
    ?line ok = bitmap_point_has_color(Bitmap, {W,H}, Pt1, Fgc),

    erase(image_size),
    ok.
 



image_font(suite) ->
    [];
image_font(doc) ->
    ["Image font test."];
image_font(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Fgc = egd:color({0,130,0}),
  
    ?line Filename = filename:join([code:priv_dir(percept),"fonts","6x11_latin1.wingsfont"]),
    ?line Font = egd_font:load(Filename),
   
    % simple text
    ?line ok = egd:text(Im, get_point(), Font, "Hello World", Fgc),
    ?line <<_/binary>> = egd:render(Im, png),
    
    GlyphStr1   = " !\"#$%&'()*+,-./",            % Codes  32 ->  47
    NumericStr  = "0123456789",                   % Codes  48 ->  57
    GlyphStr2   = ":;<=>?@",                      % Codes  58 ->  64
    AlphaBigStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",   % Codes  65 ->  90
    GlyphStr3   = "[\\]^_`",                      % Codes  91 ->  96
    AlphaSmStr  = "abcdefghijklmnopqrstuvwxyz",   % Codes  97 -> 122
    GlyphStr4   = "{|}~",                         % Codes 123 -> 126

    ?line ok = egd:text(Im, get_point(), Font, GlyphStr1, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),

    ?line ok = egd:text(Im, get_point(), Font, NumericStr, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),
 
    ?line ok = egd:text(Im, get_point(), Font, GlyphStr2, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),

    ?line ok = egd:text(Im, get_point(), Font, AlphaBigStr, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),
    
    ?line ok = egd:text(Im, get_point(), Font, GlyphStr3, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),

    ?line ok = egd:text(Im, get_point(), Font, AlphaSmStr, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),
    
    ?line ok = egd:text(Im, get_point(), Font, GlyphStr4, Fgc),
    ?line <<_/binary>> = egd:render(Im, png),

    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_png_compliant(suite) ->
    [];
image_png_compliant(doc) ->
    ["Image png compliant test."];
image_png_compliant(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Fgc = egd:color({0,0,0}),
    ?line ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),
    
    ?line Bin = egd:render(Im, png),
    ?line true = binary_is_png_compliant(Bin),
    
    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------
    
bitmap_point_has_color(Bitmap, {W,_}, {X,Y}, C) ->
    {CR,CG,CB,_} = egd_primitives:rgb_float2byte(C),
    N = W*Y*3 + X*3,
    << _:N/binary, R,G,B, _/binary>> = Bitmap,
    case {R,G,B} of
	{CR,CG,CB} -> ok;
	Other ->
	    io:format("bitmap_point_has_color: error color was ~p, should be ~p~n", [Other, {CR,CG,CB}]),
	    {error, {Other,{CR,CG,CB}}}
    end.

%% jfif header by specification
%% 2 bytes, length
%% 5 bytes, identifier ="JFIF\0"
%% 2 bytes, version, (major, minor)
%% 1 byte , units
%% However, JFIF seems to start at 6 (7 with 1-index)?
   
binary_is_jfif_compliant(JpegBin) ->
    ?line {Bin, _} = split_binary(JpegBin, 11),
    List = binary_to_list(Bin),
    case lists:sublist(List, 7, 4) of 
	"JFIF" -> true;
	Other ->
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

binary_is_gif_compliant(GifBin) ->
    ?line {Bin, _} = split_binary(GifBin, 10),
    List = binary_to_list(Bin),
    case lists:sublist(List, 1,5) of
	"GIF87" -> true;
	Other -> 
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

binary_is_png_compliant(PngBin) ->
    ?line {Bin, _} = split_binary(PngBin, 10),
    List = binary_to_list(Bin),
    case lists:sublist(List, 2,3) of
	"PNG" -> true;
	Other ->
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------


get_rgb() ->
   R = random(255), 
   G = random(255),   
   B = random(255),
   {R,G,B}.

get_angle() ->
   random(359).

get_point() ->
    get_point(get(image_size)).
get_point({W,H}) ->
   X = random(W - 1),
   Y = random(H - 1),
   {X,Y}.

get_size(Max) ->
    W = trunc(random(Max/2) + Max/2 + 1),
    H = trunc(random(Max/2) + Max/2 + 1),
    io:format("Image size will be ~p x ~p~n", [W,H]),
    {W,H}.

get_points(N) ->
    get_points(N, []).
get_points(0, Out) ->
   Out;
get_points(N, Out) ->
    get_points(N - 1, [get_point() | Out]).

random(N) -> trunc(random:uniform(trunc(N + 1)) - 1).
