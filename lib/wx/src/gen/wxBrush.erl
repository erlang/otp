%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-include("wxe.hrl").
-export([destroy/1,getColour/1,getStipple/1,getStyle/1,isHatch/1,isOk/1,new/0,
  new/1,new/2,setColour/2,setColour/4,setStipple/2,setStyle/2]).

%% inherited exports
-export([parent_class/1]).

-type wxBrush() :: wx:wx_object().
-export_type([wxBrush/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
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
-spec new(Colour) -> wxBrush() when
	Colour::wx:wx_colour();
      (Brush) -> wxBrush() when
	Brush::wxBrush:wxBrush() | wxBitmap:wxBitmap().

new(Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
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
-spec new(Colour, [Option]) -> wxBrush() when
	Colour::wx:wx_colour(),
	Option :: {'style', wx:wx_enum()}.
new(Colour, Options)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4,is_list(Options) ->
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wxe_util:color(Colour), Opts,?get_env(),?wxBrush_new_2),
  wxe_util:rec(?wxBrush_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxBrush().
getColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetColour),
  wxe_util:rec(?wxBrush_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstipple">external documentation</a>.
-spec getStipple(This) -> wxBitmap:wxBitmap() when
	This::wxBrush().
getStipple(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetStipple),
  wxe_util:rec(?wxBrush_GetStipple).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstyle">external documentation</a>.
%%<br /> Res = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-spec getStyle(This) -> wx:wx_enum() when
	This::wxBrush().
getStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_GetStyle),
  wxe_util:rec(?wxBrush_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushishatch">external documentation</a>.
-spec isHatch(This) -> boolean() when
	This::wxBrush().
isHatch(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_IsHatch),
  wxe_util:rec(?wxBrush_IsHatch).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxBrush().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,?get_env(),?wxBrush_IsOk),
  wxe_util:rec(?wxBrush_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetcolour">external documentation</a>.
-spec setColour(This, Colour) -> 'ok' when
	This::wxBrush(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
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
-spec setStipple(This, Bitmap) -> 'ok' when
	This::wxBrush(), Bitmap::wxBitmap:wxBitmap().
setStipple(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxBrush),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxBrush_SetStipple).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetstyle">external documentation</a>.
%%<br /> Style = ?wxBRUSHSTYLE_INVALID | ?wxBRUSHSTYLE_SOLID | ?wxBRUSHSTYLE_TRANSPARENT | ?wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE | ?wxBRUSHSTYLE_STIPPLE_MASK | ?wxBRUSHSTYLE_STIPPLE | ?wxBRUSHSTYLE_BDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSSDIAG_HATCH | ?wxBRUSHSTYLE_FDIAGONAL_HATCH | ?wxBRUSHSTYLE_CROSS_HATCH | ?wxBRUSHSTYLE_HORIZONTAL_HATCH | ?wxBRUSHSTYLE_VERTICAL_HATCH | ?wxBRUSHSTYLE_FIRST_HATCH | ?wxBRUSHSTYLE_LAST_HATCH
-spec setStyle(This, Style) -> 'ok' when
	This::wxBrush(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxBrush_SetStyle).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBrush()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBrush),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
