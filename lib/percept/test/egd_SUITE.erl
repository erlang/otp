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
%%

-module(egd_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([image_create_and_destroy/1,
         image_shape/1,
         image_primitives/1,
         image_colors/1,
         image_font/1,
         image_fans/1,
         image_png_compliant/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() -> 
    [image_create_and_destroy, image_shape,
     image_primitives, image_colors, image_font,
     image_fans,
     image_png_compliant].


init_per_suite(Config) when is_list(Config) ->
    rand:seed(exsplus),
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    [{max_size, 800}|Config].

end_per_testcase(_Case, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%% Image creation and destroy test.
image_create_and_destroy(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    Image = egd:create(W, H),
    ok = egd:destroy(Image),
    ok.

%% Image color test.
image_colors(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    Dir = proplists:get_value(priv_dir, Config),
    Image = egd:create(W, H),
    put(image_size, {W,H}),

    RGB = get_rgb(),
    Black = egd:color({0,0,0}),
    Red = egd:color({255,0,0}),
    Green = egd:color({0,255,0}),
    Blue = egd:color({0,0,255}),
    Random = egd:color(Image, RGB),

    ok = egd:line(Image, get_point(), get_point(), Random),
    ok = egd:line(Image, get_point(), get_point(), Red),
    ok = egd:line(Image, get_point(), get_point(), Green),
    ok = egd:line(Image, get_point(), get_point(), Black),
    ok = egd:line(Image, get_point(), get_point(), Blue),

    HtmlDefaultNames = [black,silver,gray,white,maroon,red,
                        purple,fuchia,green,lime,olive,yellow,navy,blue,teal,
                        aqua],

    lists:foreach(fun (ColorName) ->
                          Color = egd:color(ColorName),
                          ok    = egd:line(Image, get_point(), get_point(), Color)
                  end, HtmlDefaultNames),

    Png1 = <<_/binary>> = egd:render(Image,png,[{render_engine, alpha}]),
    File1 = filename:join(Dir,"image_colors_alpha.png"),
    ok = egd:save(Png1,File1),
    ct:log("<p>Image alpha:</p><img src=\"~s\" />~n", [File1]),
    Png2 = <<_/binary>> = egd:render(Image,png,[{render_engine, opaque}]),
    File2 = filename:join(Dir,"image_colors_opaque.png"),
    ok = egd:save(Png2,File2),
    ct:log("<p>Image opaque:</p><img src=\"~s\" />~n", [File2]),

    ok = egd:destroy(Image),
    erase(image_size),
    ok.

%% Image shape API test.
image_shape(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    Dir = proplists:get_value(priv_dir, Config),
    put(image_size, {W,H}),
    Im = egd:create(W, H),

    Fgc = egd:color({255,0,0}),

    ok = egd:line(Im, get_point(), get_point(), Fgc),
    ok = egd:rectangle(Im, get_point(), get_point(), Fgc),
    ok = egd:filledEllipse(Im, get_point(), get_point(), Fgc),
    ok = egd:arc(Im, get_point(), get_point(), Fgc),
    ok = egd:arc(Im, get_point(), get_point(), 100, Fgc),

    Pt1 = get_point(),
    Pt2 = get_point(), 

    ok = egd:filledRectangle(Im, Pt1, Pt2, Fgc),

    Bitmap = egd:render(Im, raw_bitmap),

    ok = bitmap_point_has_color(Bitmap, {W,H}, Pt2, Fgc),
    ok = bitmap_point_has_color(Bitmap, {W,H}, Pt1, Fgc),

    Bin = <<_/binary>> = egd:render(Im, raw_bitmap, [{render_engine, alpha}]),
    Png = egd_png:binary(W,H,Bin),
    File = filename:join(Dir,"image_shape.png"),
    ok = egd:save(Png,File),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File]),

    ok = egd:destroy(Im),

    erase(image_size),
    ok.

%% Image shape API test.
image_primitives(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    Dir = proplists:get_value(priv_dir, Config),
    put(image_size, {W,H}),

    Im0 = egd_primitives:create(W, H),
    Fgc = egd:color({25,25,255}),
    Bgc = egd:color({0,250,25}),

    Im1 = lists:foldl(fun ({Function, Arguments}, Im) ->
                              erlang:apply(egd_primitives, Function, [Im|Arguments])
                      end, Im0,
                      [{Fs, [get_point(), get_point(), Bgc]} || Fs <- [line, rectangle, filledEllipse, arc]] ++
                      [{pixel,          [get_point(), Bgc]},
                       {filledTriangle, [get_point(), get_point(), get_point(), Bgc]}]),

    Pt1 = get_point(),
    Pt2 = get_point(), 

    Im2 = egd_primitives:filledRectangle(Im1, Pt1, Pt2, Fgc),

    Bitmap = egd_render:binary(Im2, opaque),

    ok = bitmap_point_has_color(Bitmap, {W,H}, Pt2, Fgc),
    ok = bitmap_point_has_color(Bitmap, {W,H}, Pt1, Fgc),

    Bin = <<_/binary>> = egd_render:binary(Im2, alpha),
    Png = egd_png:binary(W,H,Bin),
    File = filename:join(Dir,"image_primitives.png"),
    ok = egd:save(Png,File),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File]),

    erase(image_size),
    ok.

%% Image font test.
image_font(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    Dir = proplists:get_value(priv_dir, Config),
    put(image_size, {W,H}),
    Im = egd:create(W, H),
    Fgc = egd:color({0,130,0}),

    Filename = filename:join([code:priv_dir(percept),"fonts","6x11_latin1.wingsfont"]),
    Font = egd_font:load(Filename),

    % simple text
    ok = egd:text(Im, get_point(), Font, "Hello World", Fgc),
    <<_/binary>> = egd:render(Im, png),

    GlyphStr1   = " !\"#$%&'()*+,-./",            % Codes  32 ->  47
    NumericStr  = "0123456789",                   % Codes  48 ->  57
    GlyphStr2   = ":;<=>?@",                      % Codes  58 ->  64
    AlphaBigStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",   % Codes  65 ->  90
    GlyphStr3   = "[\\]^_`",                      % Codes  91 ->  96
    AlphaSmStr  = "abcdefghijklmnopqrstuvwxyz",   % Codes  97 -> 122
    GlyphStr4   = "{|}~",                         % Codes 123 -> 126

    ok = egd:text(Im, get_point(), Font, GlyphStr1, Fgc),
    Png1 = <<_/binary>> = egd:render(Im, png),
    File1 = filename:join(Dir,"text1.png"),
    ok = egd:save(Png1,File1),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File1]),

    ok = egd:text(Im, get_point(), Font, NumericStr, Fgc),
    Png2 = <<_/binary>> = egd:render(Im, png),
    File2 = filename:join(Dir,"text2.png"),
    ok = egd:save(Png2,File2),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File2]),

    ok = egd:text(Im, get_point(), Font, GlyphStr2, Fgc),
    Png3 = <<_/binary>> = egd:render(Im, png),
    File3 = filename:join(Dir,"text3.png"),
    ok = egd:save(Png3,File3),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File3]),

    ok = egd:text(Im, get_point(), Font, AlphaBigStr, Fgc),
    Png4 = <<_/binary>> = egd:render(Im, png),
    File4 = filename:join(Dir,"text4.png"),
    ok = egd:save(Png4,File4),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File4]),

    ok = egd:text(Im, get_point(), Font, GlyphStr3, Fgc),
    Png5 = <<_/binary>> = egd:render(Im, png),
    File5 = filename:join(Dir,"text5.png"),
    ok = egd:save(Png5,File5),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File5]),

    ok = egd:text(Im, get_point(), Font, AlphaSmStr, Fgc),
    Png6 = <<_/binary>> = egd:render(Im, png),
    File6 = filename:join(Dir,"text6.png"),
    ok = egd:save(Png6,File6),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File6]),

    ok = egd:text(Im, get_point(), Font, GlyphStr4, Fgc),
    Png7 = <<_/binary>> = egd:render(Im, png),
    File7 = filename:join(Dir,"text7.png"),
    ok = egd:save(Png7,File7),
    ct:log("<p>Image:</p><img src=\"~s\" />~n", [File7]),

    ok = egd:destroy(Im),
    erase(image_size),
    ok.

%% Image png compliant test.
image_png_compliant(Config) when is_list(Config) ->
    {W,H} = get_size(proplists:get_value(max_size, Config)),
    put(image_size, {W,H}),
    Im = egd:create(W, H),
    Fgc = egd:color({0,0,0}),
    ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),

    Bin = egd:render(Im, png),
    true = binary_is_png_compliant(Bin),

    ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_fans(Config) when is_list(Config) ->
    W = 1024,
    H = 800,
    Dir = proplists:get_value(priv_dir, Config),

    Fun = fun({F,Args},Im) ->
                  erlang:apply(egd_primitives,F,[Im|Args])
          end,

    %% fan1
    Ops1 = gen_vertical_fan(1,{0,400},egd:color(red),1024,800,-15),
    Ops2 = gen_horizontal_fan(1,{512,800},egd:color(green),1024,0,-15),

    Im0 = egd_primitives:create(W,H),
    Im1 = lists:foldl(Fun, Im0, Ops1 ++ Ops2),
    Bin1 = egd_render:binary(Im1, opaque),
    Png1 = egd_png:binary(W,H,Bin1),

    File1 = filename:join(Dir,"fan1_opaque.png"),
    ok = egd:save(Png1,File1),
    ct:log("<p>Image opaque width 1:</p><img src=\"~s\" />~n", [File1]),

    Bin2 = egd_render:binary(Im1, alpha),
    Png2 = egd_png:binary(W,H,Bin2),

    File2 = filename:join(Dir,"fan1_alpha.png"),
    ok = egd:save(Png2,File2),
    ct:log("<p>Image alpha width 1:</p><img src=\"~s\" />~n", [File2]),


    %% fan2
    Ops3 = gen_vertical_fan(7,{0,400},egd:color(red),1024,800,-15),
    Ops4 = gen_horizontal_fan(7,{512,800},egd:color(green),1024,0,-15),

    Im2 = lists:foldl(Fun, Im0, Ops3 ++ Ops4),
    Bin3 = egd_render:binary(Im2, opaque),
    Png3 = egd_png:binary(W,H,Bin3),

    File3 = filename:join(Dir,"fan2_opaque.png"),
    ok = egd:save(Png3,File3),
    ct:log("<p>Image opaque width 7:</p><img src=\"~s\" />~n", [File3]),

    Bin4 = egd_render:binary(Im2, alpha),
    Png4 = egd_png:binary(W,H,Bin4),

    File4 = filename:join(Dir,"fan2_alpha.png"),
    ok = egd:save(Png4,File4),
    ct:log("<p>Image alpha width 7:</p><img src=\"~s\" />~n", [File4]),
    ok.

gen_vertical_fan(Wd,Pt,C,X,Y,Step) when Y > 0 ->
    [{line,[Pt,{X,Y},Wd,C]}|gen_vertical_fan(Wd,Pt,C,X,Y + Step,Step)];
gen_vertical_fan(_,_,_,_,_,_) -> [].

gen_horizontal_fan(Wd,Pt,C,X,Y,Step) when X > 0 ->
    [{line,[Pt,{X,Y},Wd,C]}|gen_horizontal_fan(Wd,Pt,C,X + Step,Y,Step)];
gen_horizontal_fan(_,_,_,_,_,_) -> [].


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

binary_is_png_compliant(PngBin) ->
    {Bin, _} = split_binary(PngBin, 10),
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

random(N) -> trunc(rand:uniform(trunc(N + 1)) - 1).
