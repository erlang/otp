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

-module(wxPen).
-moduledoc """
Functions for wxPen class

A pen is a drawing tool for drawing outlines. It is used for drawing lines and
painting the outline of rectangles, ellipses, etc. It has a colour, a width and
a style.

Note: On a monochrome display, wxWidgets shows all non-white pens as black.

Do not initialize objects on the stack before the program commences, since other
required structures may not have been set up yet. Instead, define global
pointers to objects and create them in `wxApp::OnInit()` (not implemented in wx)
or when required.

An application may wish to dynamically create pens with different
characteristics, and there is the consequent danger that a large number of
duplicate pens will be created. Therefore an application may wish to get a
pointer to a pen by using the global list of pens ?wxThePenList, and calling the
member function `wxPenList::FindOrCreatePen()` (not implemented in wx). See
`wxPenList` (not implemented in wx) for more info.

This class uses reference counting and copy-on-write internally so that
assignments between two instances of this class are very cheap. You can
therefore use actual objects instead of pointers without efficiency problems. If
an instance of this class is changed it will create its own data internally so
that other instances, which previously shared the data using the reference
counting, are not affected.

Predefined objects (include wx.hrl):

See: `wxPenList` (not implemented in wx), `m:wxDC`, `wxDC:setPen/2`

wxWidgets docs: [wxPen](https://docs.wxwidgets.org/3.1/classwx_pen.html)
""".
-include("wxe.hrl").
-export([destroy/1,getCap/1,getColour/1,getJoin/1,getStyle/1,getWidth/1,isOk/1,
  new/0,new/1,new/2,setCap/2,setColour/2,setColour/4,setJoin/2,setStyle/2,
  setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPen() :: wx:wx_object().
-export_type([wxPen/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenwxpen">external documentation</a>.
-doc """
Default constructor.

The pen will be uninitialised, and `isOk/1` will return false.
""".
-spec new() -> wxPen().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxPen_new_0),
  wxe_util:rec(?wxPen_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenwxpen">external documentation</a>.
%% <br /> Also:<br />
%% new(Pen) -> wxPen() when<br />
%% 	Pen::wxPen().<br />
%% 
%%<br /> Style = ?wxPENSTYLE_INVALID | ?wxPENSTYLE_SOLID | ?wxPENSTYLE_DOT | ?wxPENSTYLE_LONG_DASH | ?wxPENSTYLE_SHORT_DASH | ?wxPENSTYLE_DOT_DASH | ?wxPENSTYLE_USER_DASH | ?wxPENSTYLE_TRANSPARENT | ?wxPENSTYLE_STIPPLE_MASK_OPAQUE | ?wxPENSTYLE_STIPPLE_MASK | ?wxPENSTYLE_STIPPLE | ?wxPENSTYLE_BDIAGONAL_HATCH | ?wxPENSTYLE_CROSSDIAG_HATCH | ?wxPENSTYLE_FDIAGONAL_HATCH | ?wxPENSTYLE_CROSS_HATCH | ?wxPENSTYLE_HORIZONTAL_HATCH | ?wxPENSTYLE_VERTICAL_HATCH | ?wxPENSTYLE_FIRST_HATCH | ?wxPENSTYLE_LAST_HATCH
-doc "Copy constructor, uses overview_refcount.".
-spec new(Colour) -> wxPen() when
	Colour::wx:wx_colour();
      (Pen) -> wxPen() when
	Pen::wxPen().

new(Colour)
 when ?is_colordata(Colour) ->
  new(Colour, []);
new(#wx_ref{type=PenT}=Pen) ->
  ?CLASS(PenT,wxPen),
  wxe_util:queue_cmd(Pen,?get_env(),?wxPen_new_1),
  wxe_util:rec(?wxPen_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenwxpen">external documentation</a>.
%%<br /> Style = ?wxPENSTYLE_INVALID | ?wxPENSTYLE_SOLID | ?wxPENSTYLE_DOT | ?wxPENSTYLE_LONG_DASH | ?wxPENSTYLE_SHORT_DASH | ?wxPENSTYLE_DOT_DASH | ?wxPENSTYLE_USER_DASH | ?wxPENSTYLE_TRANSPARENT | ?wxPENSTYLE_STIPPLE_MASK_OPAQUE | ?wxPENSTYLE_STIPPLE_MASK | ?wxPENSTYLE_STIPPLE | ?wxPENSTYLE_BDIAGONAL_HATCH | ?wxPENSTYLE_CROSSDIAG_HATCH | ?wxPENSTYLE_FDIAGONAL_HATCH | ?wxPENSTYLE_CROSS_HATCH | ?wxPENSTYLE_HORIZONTAL_HATCH | ?wxPENSTYLE_VERTICAL_HATCH | ?wxPENSTYLE_FIRST_HATCH | ?wxPENSTYLE_LAST_HATCH
-doc """
Constructs a pen from a colour object, pen width and style.

Remark: Different versions of Windows and different versions of other platforms
support very different subsets of the styles above so handle with care.

See: `setStyle/2`, `setColour/4`, `setWidth/2`
""".
-spec new(Colour, [Option]) -> wxPen() when
	Colour::wx:wx_colour(),
	Option :: {'width', integer()}
		 | {'style', wx:wx_enum()}.
new(Colour, Options)
 when ?is_colordata(Colour),is_list(Options) ->
  MOpts = fun({width, _width} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wxe_util:color(Colour), Opts,?get_env(),?wxPen_new_2),
  wxe_util:rec(?wxPen_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetcap">external documentation</a>.
%%<br /> Res = ?wxCAP_INVALID | ?wxCAP_ROUND | ?wxCAP_PROJECTING | ?wxCAP_BUTT
-doc """
Returns the pen cap style, which may be one of `wxCAP_ROUND`, `wxCAP_PROJECTING`
and `wxCAP_BUTT`.

The default is `wxCAP_ROUND`.

See: `setCap/2`
""".
-spec getCap(This) -> wx:wx_enum() when
	This::wxPen().
getCap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_GetCap),
  wxe_util:rec(?wxPen_GetCap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetcolour">external documentation</a>.
-doc """
Returns a reference to the pen colour.

See: `setColour/4`
""".
-spec getColour(This) -> wx:wx_colour4() when
	This::wxPen().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_GetColour),
  wxe_util:rec(?wxPen_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetjoin">external documentation</a>.
%%<br /> Res = ?wxJOIN_INVALID | ?wxJOIN_BEVEL | ?wxJOIN_MITER | ?wxJOIN_ROUND
-doc """
Returns the pen join style, which may be one of `wxJOIN_BEVEL`, `wxJOIN_ROUND`
and `wxJOIN_MITER`.

The default is `wxJOIN_ROUND`.

See: `setJoin/2`
""".
-spec getJoin(This) -> wx:wx_enum() when
	This::wxPen().
getJoin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_GetJoin),
  wxe_util:rec(?wxPen_GetJoin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetstyle">external documentation</a>.
%%<br /> Res = ?wxPENSTYLE_INVALID | ?wxPENSTYLE_SOLID | ?wxPENSTYLE_DOT | ?wxPENSTYLE_LONG_DASH | ?wxPENSTYLE_SHORT_DASH | ?wxPENSTYLE_DOT_DASH | ?wxPENSTYLE_USER_DASH | ?wxPENSTYLE_TRANSPARENT | ?wxPENSTYLE_STIPPLE_MASK_OPAQUE | ?wxPENSTYLE_STIPPLE_MASK | ?wxPENSTYLE_STIPPLE | ?wxPENSTYLE_BDIAGONAL_HATCH | ?wxPENSTYLE_CROSSDIAG_HATCH | ?wxPENSTYLE_FDIAGONAL_HATCH | ?wxPENSTYLE_CROSS_HATCH | ?wxPENSTYLE_HORIZONTAL_HATCH | ?wxPENSTYLE_VERTICAL_HATCH | ?wxPENSTYLE_FIRST_HATCH | ?wxPENSTYLE_LAST_HATCH
-doc """
Returns the pen style.

See: `new/2`, `setStyle/2`
""".
-spec getStyle(This) -> wx:wx_enum() when
	This::wxPen().
getStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_GetStyle),
  wxe_util:rec(?wxPen_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpengetwidth">external documentation</a>.
-doc """
Returns the pen width.

See: `setWidth/2`
""".
-spec getWidth(This) -> integer() when
	This::wxPen().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_GetWidth),
  wxe_util:rec(?wxPen_GetWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpenisok">external documentation</a>.
-doc """
Returns true if the pen is initialised.

Notice that an uninitialized pen object can't be queried for any pen properties
and all calls to the accessor methods on it will result in an assert failure.
""".
-spec isOk(This) -> boolean() when
	This::wxPen().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,?get_env(),?wxPen_IsOk),
  wxe_util:rec(?wxPen_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcap">external documentation</a>.
%%<br /> CapStyle = ?wxCAP_INVALID | ?wxCAP_ROUND | ?wxCAP_PROJECTING | ?wxCAP_BUTT
-doc """
Sets the pen cap style, which may be one of `wxCAP_ROUND`, `wxCAP_PROJECTING`
and `wxCAP_BUTT`.

The default is `wxCAP_ROUND`.

See: `getCap/1`
""".
-spec setCap(This, CapStyle) -> 'ok' when
	This::wxPen(), CapStyle::wx:wx_enum().
setCap(#wx_ref{type=ThisT}=This,CapStyle)
 when is_integer(CapStyle) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,CapStyle,?get_env(),?wxPen_SetCap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcolour">external documentation</a>.
-doc """
The pen's colour is changed to the given colour.

See: `getColour/1`
""".
-spec setColour(This, Colour) -> 'ok' when
	This::wxPen(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxPen_SetColour_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetcolour">external documentation</a>.
-spec setColour(This, Red, Green, Blue) -> 'ok' when
	This::wxPen(), Red::integer(), Green::integer(), Blue::integer().
setColour(#wx_ref{type=ThisT}=This,Red,Green,Blue)
 when is_integer(Red),is_integer(Green),is_integer(Blue) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,Red,Green,Blue,?get_env(),?wxPen_SetColour_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetjoin">external documentation</a>.
%%<br /> Join_style = ?wxJOIN_INVALID | ?wxJOIN_BEVEL | ?wxJOIN_MITER | ?wxJOIN_ROUND
-doc """
Sets the pen join style, which may be one of `wxJOIN_BEVEL`, `wxJOIN_ROUND` and
`wxJOIN_MITER`.

The default is `wxJOIN_ROUND`.

See: `getJoin/1`
""".
-spec setJoin(This, Join_style) -> 'ok' when
	This::wxPen(), Join_style::wx:wx_enum().
setJoin(#wx_ref{type=ThisT}=This,Join_style)
 when is_integer(Join_style) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,Join_style,?get_env(),?wxPen_SetJoin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetstyle">external documentation</a>.
%%<br /> Style = ?wxPENSTYLE_INVALID | ?wxPENSTYLE_SOLID | ?wxPENSTYLE_DOT | ?wxPENSTYLE_LONG_DASH | ?wxPENSTYLE_SHORT_DASH | ?wxPENSTYLE_DOT_DASH | ?wxPENSTYLE_USER_DASH | ?wxPENSTYLE_TRANSPARENT | ?wxPENSTYLE_STIPPLE_MASK_OPAQUE | ?wxPENSTYLE_STIPPLE_MASK | ?wxPENSTYLE_STIPPLE | ?wxPENSTYLE_BDIAGONAL_HATCH | ?wxPENSTYLE_CROSSDIAG_HATCH | ?wxPENSTYLE_FDIAGONAL_HATCH | ?wxPENSTYLE_CROSS_HATCH | ?wxPENSTYLE_HORIZONTAL_HATCH | ?wxPENSTYLE_VERTICAL_HATCH | ?wxPENSTYLE_FIRST_HATCH | ?wxPENSTYLE_LAST_HATCH
-doc """
Set the pen style.

See: `new/2`
""".
-spec setStyle(This, Style) -> 'ok' when
	This::wxPen(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxPen_SetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxpen.html#wxpensetwidth">external documentation</a>.
-doc """
Sets the pen width.

See: `getWidth/1`
""".
-spec setWidth(This, Width) -> 'ok' when
	This::wxPen(), Width::integer().
setWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxPen),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxPen_SetWidth).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

See: reference-counted object destruction

Remark: Although all remaining pens are deleted when the application exits, the
application should try to clean up all pens itself. This is because wxWidgets
cannot know if a pointer to the pen object is stored in an application data
structure, and there is a risk of double deletion.
""".
-spec destroy(This::wxPen()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPen),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
