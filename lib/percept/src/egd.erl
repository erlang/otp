%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
%% @doc egd - erlang graphical drawer 
%%
%% 

-module(egd).

-export([create/2, destroy/1, information/1]).
-export([text/5, line/4, color/1, color/2]).
-export([rectangle/4, filledRectangle/4, filledEllipse/4]).
-export([arc/4, arc/5]).
-export([render/1, render/2, render/3]).

-export([filledTriangle/5, polygon/3]).

-export([save/2]).

-include("egd.hrl").

%%==========================================================================
%%
%% 		Type definitions 
%%
%%==========================================================================

-type egd_image() :: pid().
-type point() :: {non_neg_integer(), non_neg_integer()}.
-type render_option() :: {'render_engine', 'opaque'} | {'render_engine', 'alpha'}.
-type color() :: {float(), float(), float(), float()}.
-type font() :: egd_font:font().

%%==========================================================================
%%
%%		Interface functions	
%%
%%==========================================================================

%% @doc Creates an image area and returns its reference.

-spec create(Width, Height) -> egd_image() when
      Width :: integer(),
      Height :: integer().

create(Width,Height) ->
    spawn_link(fun() -> init(trunc(Width),trunc(Height)) end).


%% @doc Destroys the image.

-spec destroy(Image) -> ok when
      Image :: egd_image().

destroy(Image) ->
   cast(Image, destroy),
   ok.


%% @equiv render(Image, png, [{render_engine, opaque}])

-spec render(Image :: egd_image()) -> binary().

render(Image) ->
    render(Image, png, [{render_engine, opaque}]).

%% @equiv render(Image, Type, [{render_engine, opaque}])

-spec render(Image, Type) -> binary() when
      Image :: egd_image(),
      Type :: png | raw_bitmap.

render(Image, Type) ->
    render(Image, Type, [{render_engine, opaque}]).

%% @doc Renders a binary from the primitives specified by egd_image(). The
%% 	binary can either be a raw bitmap with rgb tripplets or a binary in png
%%	format.
%% @end

-spec render(Image, Type, Options) -> binary() when
      Image :: egd_image(),
      Type :: 'png' | 'raw_bitmap' | 'eps',
      Options :: [render_option()].

render(Image, Type, Options) ->
    {render_engine, RenderType} = proplists:lookup(render_engine, Options),
    call(Image, {render, Type, RenderType}).


%% @hidden
%% @doc Writes out information about the image. This is a debug feature
%%	mainly.
%% @end

-spec information(Image) -> ok when
      Image :: egd_image().

information(Pid) ->
    cast(Pid, information),
    ok.

%% @doc Creates a line object from P1 to P2 in the image.

-spec line(Image, Point1, Point2, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Color :: color().

line(Image, P1, P2, Color) ->
    cast(Image, {line, P1, P2, Color}),
    ok.

%% @doc Creates a color reference.

-spec color(ValueOrName) -> color() when
      ValueOrName :: Value | Name,
      Value :: {byte(), byte(), byte()} | {byte(), byte(), byte(), byte()},
      Name :: black | silver | gray | white | maroon | red | purple | fuchia
            | green | lime | olive | yellow | navy | blue | teal | aqua.

color(Color) ->
    egd_primitives:color(Color).

%% @doc Creates a color reference.
%% @hidden

-spec color(_Image, Color) -> color() when
      _Image :: egd_image(),
      Color :: {byte(), byte(), byte()}.

color(_Image, Color) ->
    egd_primitives:color(Color).

%% @doc Creates a text object.

-spec text(Image, Point, Font, Text, Color) -> ok when
      Image :: egd_image(),
      Point :: point(),
      Font :: font(),
      Text :: string(),
      Color :: color().

text(Image, P, Font, Text, Color) ->
    cast(Image, {text, P, Font, Text, Color}),
    ok.

%% @doc Creates a rectangle object.

-spec rectangle(Image, Point1, Point2, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Color :: color().

rectangle(Image, P1, P2, Color) ->
    cast(Image, {rectangle, P1, P2, Color}),
    ok.

%% @doc Creates a filled rectangle object.

-spec filledRectangle(Image, Point1, Point2, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Color :: color().

filledRectangle(Image, P1, P2, Color) ->
    cast(Image, {filled_rectangle, P1, P2, Color}),
    ok.

%% @doc Creates a filled ellipse object.

-spec filledEllipse(Image, Point1, Point2, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Color :: color().

filledEllipse(Image, P1, P2, Color) ->
    cast(Image, {filled_ellipse, P1, P2, Color}),
    ok.

%% @hidden
%% @doc Creates a filled triangle object.

-spec filledTriangle(Image, Point1, Point2, Point3, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Point3 :: point(),
      Color :: color().

filledTriangle(Image, P1, P2, P3, Color) ->
    cast(Image, {filled_triangle, P1, P2, P3, Color}),
    ok.

%% @hidden
%% @doc Creates a filled filled polygon object.

-spec polygon(Image, Points, Color) -> ok when
      Image :: egd_image(),
      Points :: [point()],
      Color :: color().

polygon(Image, Pts, Color) ->
    cast(Image, {polygon, Pts, Color}),
    ok.

%% @hidden
%% @doc Creates an arc with radius of bbx corner.

-spec arc(Image, Point1, Point2, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      Color :: color().

arc(Image, P1, P2, Color) ->
    cast(Image, {arc, P1, P2, Color}),
    ok.

%% @hidden
%% @doc Creates an arc.

-spec arc(Image, Point1, Point2, D, Color) -> ok when
      Image :: egd_image(),
      Point1 :: point(),
      Point2 :: point(),
      D :: integer(),
      Color :: color().

arc(Image, P1, P2, D, Color) ->
    cast(Image, {arc, P1, P2, D, Color}),
    ok.

%% @doc Saves the binary to file.

-spec save(Binary, Filename) -> ok when
      Binary :: binary(),
      Filename :: file:name_all().

save(Binary, Filename) when is_binary(Binary) ->
    file:write_file(Filename, Binary),
    ok.
% ---------------------------------
% Aux functions 
% ---------------------------------

cast(Pid, Command) ->
    Pid ! {egd, self(), Command}.

call(Pid, Command) ->
    Pid ! {egd, self(), Command},
    receive {egd, Pid, Result} -> Result end.    

% ---------------------------------
% Server loop 
% ---------------------------------

init(W,H) ->
    Image = egd_primitives:create(W,H),
    loop(Image).

loop(Image) ->
    receive
	% Quitting
	{egd, _Pid, destroy} -> ok;
	
	% Rendering
    	{egd, Pid, {render, BinaryType, RenderType}} ->
	    case BinaryType of
		raw_bitmap ->
		    Bitmap = egd_render:binary(Image, RenderType),
		    Pid ! {egd, self(), Bitmap},
		    loop(Image);
		eps ->
		    Eps = egd_render:eps(Image),
		    Pid ! {egd, self(), Eps},
		    loop(Image);
		png ->
		    Bitmap = egd_render:binary(Image, RenderType),
		    Png = egd_png:binary(
			Image#image.width,
			Image#image.height,
			Bitmap),
		    Pid ! {egd, self(), Png},
		    loop(Image);
		Unhandled ->
		    Pid ! {egd, self(), {error, {format, Unhandled}}},
		    loop(Image)
	     end;

	% Drawing primitives
	{egd, _Pid, {line, P1, P2, C}} ->
	    loop(egd_primitives:line(Image, P1, P2, C));
	{egd, _Pid, {text, P, Font, Text, C}} ->
	    loop(egd_primitives:text(Image, P, Font, Text, C));
	{egd, _Pid, {filled_ellipse, P1, P2, C}} ->
	    loop(egd_primitives:filledEllipse(Image, P1, P2, C));
	{egd, _Pid, {filled_rectangle, P1, P2, C}} ->
	    loop(egd_primitives:filledRectangle(Image, P1, P2, C));
	{egd, _Pid, {filled_triangle, P1, P2, P3, C}} ->
	    loop(egd_primitives:filledTriangle(Image, P1, P2, P3, C));
	{egd, _Pid, {polygon, Pts, C}} ->
	    loop(egd_primitives:polygon(Image, Pts, C));
	{egd, _Pid, {arc, P1, P2, C}} ->
	    loop(egd_primitives:arc(Image, P1, P2, C));
	{egd, _Pid, {arc, P1, P2, D, C}} ->
	    loop(egd_primitives:arc(Image, P1, P2, D, C));
	{egd, _Pid, {rectangle, P1, P2, C}} ->
	    loop(egd_primitives:rectangle(Image, P1, P2, C));
	{egd, _Pid, information} ->
	    egd_primitives:info(Image),
	    loop(Image);
	 _ ->
	    loop(Image)
    end.
