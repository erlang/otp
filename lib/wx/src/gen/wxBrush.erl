%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxBrush).
-moduledoc """
Functions for wxBrush class

A brush is a drawing tool for filling in areas. It is used for painting the
background of rectangles, ellipses, etc. It has a colour and a style.

On a monochrome display, wxWidgets shows all brushes as white unless the colour
is really black.

Do not initialize objects on the stack before the program commences, since other
required structures may not have been set up yet. Instead, define global
pointers to objects and create them in `wxApp::OnInit` (not implemented in wx)
or when required.

An application may wish to create brushes with different characteristics
dynamically, and there is the consequent danger that a large number of duplicate
brushes will be created. Therefore an application may wish to get a pointer to a
brush by using the global list of brushes ?wxTheBrushList, and calling the
member function `wxBrushList::FindOrCreateBrush()` (not implemented in wx).

This class uses reference counting and copy-on-write internally so that
assignments between two instances of this class are very cheap. You can
therefore use actual objects instead of pointers without efficiency problems. If
an instance of this class is changed it will create its own data internally so
that other instances, which previously shared the data using the reference
counting, are not affected.

Predefined objects (include wx.hrl):

See: `wxBrushList` (not implemented in wx), `m:wxDC`, `wxDC:setBrush/2`

wxWidgets docs: [wxBrush](https://docs.wxwidgets.org/3.1/classwx_brush.html)
""".
-include("wxe.hrl").
-export([destroy/1,getColour/1,getStipple/1,getStyle/1,isHatch/1,isOk/1,new/0,
  new/1,new/2,setColour/2,setColour/4,setStipple/2,setStyle/2]).

%% inherited exports
-export([parent_class/1]).

-type wxBrush() :: wx:wx_object().
-export_type([wxBrush/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
-doc """
Default constructor.

The brush will be uninitialised, and `m:wxBrush`:`isOk/1` will return false.
""".
-spec new() -> wxBrush().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxBrush_new_0),
  wxe_util:rec(?wxBrush_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
%% <br /> Also:<br />
%% new(Brush) -> wxBrush() when<br />
%% 	Brush::wxBrush:wxBrush() | wxBitmap:wxBitmap().<br />
%% 
%%<br /> Style = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-doc "Copy constructor, uses reference counting.".
-spec new(Colour) -> wxBrush() when
	Colour::wx:wx_colour();
      (Brush) -> wxBrush() when
	Brush::wxBrush:wxBrush() | wxBitmap:wxBitmap().

new(Colour)
 when ?is_colordata(Colour) ->
  new(Colour, []);
new(#wx_ref{type=BrushT}=Brush) ->
  IswxBrush = ?CLASS_T(BrushT,wxBrush),
  IswxBitmap = ?CLASS_T(BrushT,wxBitmap),
  BrushType = if
    IswxBrush ->   wxBrush;
    IswxBitmap ->   wxBitmap;
    true -> error({badarg, BrushT})
  end,
  wxe_util:queue_cmd(wx:typeCast(Brush, BrushType),?get_env(),?wxBrush_new_1),
  wxe_util:rec(?wxBrush_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
%%<br /> Style = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-doc "Constructs a brush from a colour object and `style`.".
-spec new(Colour, [Option]) -> wxBrush() when
	Colour::wx:wx_colour(),
	Option :: {'style', wx:wx_enum()}.
new(Colour, Options)
 when ?is_colordata(Colour),is_list(Options) ->
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wxe_util:color(Colour), Opts,?get_env(),?wxBrush_new_2),
  wxe_util:rec(?wxBrush_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetcolour">external documentation</a>.
-doc """
Returns a reference to the brush colour.

See: `setColour/4`
""".
-spec getColour(This) -> wx:wx_colour4() when
	This::wxBrush().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetColour),
  wxe_util:rec(?wxBrush_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstipple">external documentation</a>.
-doc """
Gets a pointer to the stipple bitmap.

If the brush does not have a `wxBRUSHSTYLE_STIPPLE` style, this bitmap may be
non-NULL but uninitialised (i.e. `m:wxBitmap`:`isOk/1` returns false).

See: `setStipple/2`
""".
-spec getStipple(This) -> wxBitmap:wxBitmap() when
	This::wxBrush().
getStipple(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetStipple),
  wxe_util:rec(?wxBrush_GetStipple).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstyle">external documentation</a>.
%%<br /> Res = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-doc """
Returns the brush style, one of the ?wxBrushStyle values.

See: `setStyle/2`, `setColour/4`, `setStipple/2`
""".
-spec getStyle(This) -> wx:wx_enum() when
	This::wxBrush().
getStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetStyle),
  wxe_util:rec(?wxBrush_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushishatch">external documentation</a>.
-doc """
Returns true if the style of the brush is any of hatched fills.

See: `getStyle/1`
""".
-spec isHatch(This) -> boolean() when
	This::wxBrush().
isHatch(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_IsHatch),
  wxe_util:rec(?wxBrush_IsHatch).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushisok">external documentation</a>.
-doc """
Returns true if the brush is initialised.

Notice that an uninitialized brush object can't be queried for any brush
properties and all calls to the accessor methods on it will result in an assert
failure.
""".
-spec isOk(This) -> boolean() when
	This::wxBrush().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_IsOk),
  wxe_util:rec(?wxBrush_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetcolour">external documentation</a>.
-doc """
Sets the brush colour using red, green and blue values.

See: `getColour/1`
""".
-spec setColour(This, Colour) -> 'ok' when
	This::wxBrush(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxBrush_SetColour_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetcolour">external documentation</a>.
-spec setColour(This, Red, Green, Blue) -> 'ok' when
	This::wxBrush(), Red::integer(), Green::integer(), Blue::integer().
setColour(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxBrush_SetColour_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetstipple">external documentation</a>.
-doc """
Sets the stipple bitmap.

Remark: The style will be set to `wxBRUSHSTYLE_STIPPLE`, unless the bitmap has a
mask associated to it, in which case the style will be set to
`wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE`.

See: `m:wxBitmap`
""".
-spec setStipple(This, Bitmap) -> 'ok' when
	This::wxBrush(), Bitmap::wxBitmap:wxBitmap().
setStipple(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxBrush),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxBrush_SetStipple).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetstyle">external documentation</a>.
%%<br /> Style = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-doc """
Sets the brush style.

See: `getStyle/1`
""".
-spec setStyle(This, Style) -> 'ok' when
	This::wxBrush(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxBrush_SetStyle).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

See overview_refcount_destruct for more info.

Remark: Although all remaining brushes are deleted when the application exits,
the application should try to clean up all brushes itself. This is because
wxWidgets cannot know if a pointer to the brush object is stored in an
application data structure, and there is a risk of double deletion.
""".
-spec destroy(This::wxBrush()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBrush),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
