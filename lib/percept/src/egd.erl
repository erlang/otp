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
%% Type definitions
%%==========================================================================

%% @type egd_image()
%% @type font()
%% @type point() = {integer(), integer()}
%% @type color()  
%% @type render_option() = {render_engine, opaque} | {render_engine, alpha}

-type egd_image() :: pid().
-type point() :: {non_neg_integer(), non_neg_integer()}.
-type render_option() :: {'render_engine', 'opaque'} | {'render_engine', 'alpha'}.
-type color() :: {float(), float(), float(), float()}.

%%==========================================================================
%% Interface functions
%%==========================================================================

%% @spec create(integer(), integer()) -> egd_image()
%% @doc Creates an image area and returns its reference.

-spec create(Width :: integer(), Height :: integer()) -> egd_image().

create(Width,Height) ->
    spawn_link(fun() -> init(trunc(Width),trunc(Height)) end).


%% @spec destroy(egd_image()) -> ok
%% @doc Destroys the image.

-spec destroy(Image :: egd_image()) -> ok.

destroy(Image) ->
   cast(Image, destroy).


%% @spec render(egd_image()) -> binary()
%% @equiv render(Image, png, [{render_engine, opaque}])

-spec render(Image :: egd_image()) -> binary().

render(Image) ->
    render(Image, png, [{render_engine, opaque}]).

%% @spec render(egd_image(), png | raw_bitmap) -> binary() 
%% @equiv render(Image, Type, [{render_engine, opaque}])

render(Image, Type) ->
    render(Image, Type, [{render_engine, opaque}]).

%% @spec render(egd_image(), png | raw_bitmap, [render_option()]) -> binary() 
%% @doc Renders a binary from the primitives specified by egd_image(). The
%% 	binary can either be a raw bitmap with rgb tripplets or a binary in png
%%	format.

-spec render(
	Image :: egd_image(), 
	Type :: 'png' | 'raw_bitmap' | 'eps',
	Options :: [render_option()]) -> binary().

render(Image, Type, Options) ->
    {render_engine, RenderType} = proplists:lookup(render_engine, Options),
    call(Image, {render, Type, RenderType}).


%% @spec information(egd_image()) -> ok
%% @hidden
%% @doc Writes out information about the image. This is a debug feature
%%	mainly.

information(Pid) ->
    cast(Pid, information).

%% @spec line(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a line object from P1 to P2 in the image.

-spec line(
	Image :: egd_image(),
	P1 :: point(),
	P2 :: point(),
	Color :: color()) -> 'ok'.

line(Image, P1, P2, Color) ->
    cast(Image, {line, P1, P2, Color}).

%% @spec color( Value | Name ) -> color()
%% where
%%  Value = {byte(), byte(), byte()} | {byte(), byte(), byte(), byte()} 
%%  Name  = black | silver | gray | white | maroon | red | purple | fuchia | green | lime | olive | yellow | navy | blue | teal | aqua
%% @doc Creates a color reference.

-spec color(Value :: {byte(), byte(), byte()} | {byte(), byte(), byte(), byte()} | atom()) ->
    color().

color(Color) ->
    egd_primitives:color(Color).

%% @spec color(egd_image(), {byte(), byte(), byte()}) -> color()
%% @doc Creates a color reference.
%% @hidden

color(_Image, Color) ->
    egd_primitives:color(Color).

%% @spec text(egd_image(), point(), font(), string(), color()) -> ok
%% @doc Creates a text object.

text(Image, P, Font, Text, Color) ->
    cast(Image, {text, P, Font, Text, Color}).

%% @spec rectangle(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a rectangle object.

rectangle(Image, P1, P2, Color) ->
    cast(Image, {rectangle, P1, P2, Color}).

%% @spec filledRectangle(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a filled rectangle object.

filledRectangle(Image, P1, P2, Color) ->
    cast(Image, {filled_rectangle, P1, P2, Color}).

%% @spec filledEllipse(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a filled ellipse object.

filledEllipse(Image, P1, P2, Color) ->
    cast(Image, {filled_ellipse, P1, P2, Color}).

%% @spec filledTriangle(egd_image(), point(), point(), point(), color()) -> ok
%% @hidden
%% @doc Creates a filled triangle object.

filledTriangle(Image, P1, P2, P3, Color) ->
    cast(Image, {filled_triangle, P1, P2, P3, Color}).

%% @spec polygon(egd_image(), [point()], color()) -> ok
%% @hidden
%% @doc Creates a filled filled polygon object.

polygon(Image, Pts, Color) ->
    cast(Image, {polygon, Pts, Color}).

%% @spec arc(egd_image(), point(), point(), color()) -> ok
%% @hidden
%% @doc Creates an arc with radius of bbx corner.

arc(Image, P1, P2, Color) ->
    cast(Image, {arc, P1, P2, Color}).

%% @spec arc(egd_image(), point(), point(), integer(), color()) -> ok
%% @hidden
%% @doc Creates an arc.

arc(Image, P1, P2, D, Color) ->
    cast(Image, {arc, P1, P2, D, Color}).

%% @spec save(binary(), string()) -> ok
%% @doc Saves the binary to file. 

save(Binary, Filename) when is_binary(Binary) ->
    ok = file:write_file(Filename, Binary),
    ok.
% ---------------------------------
% Aux functions 
% ---------------------------------

cast(Pid, Command) ->
    Pid ! {egd, self(), Command},
    ok.

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
