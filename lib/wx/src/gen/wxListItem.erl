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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html">wxListItem</a>.
%% @type wxListItem().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxListItem).
-include("wxe.hrl").
-export([clear/1,destroy/1,getAlign/1,getBackgroundColour/1,getColumn/1,getFont/1,
  getId/1,getImage/1,getMask/1,getState/1,getText/1,getTextColour/1,getWidth/1,
  new/0,new/1,setAlign/2,setBackgroundColour/2,setColumn/2,setFont/2,
  setId/2,setImage/2,setMask/2,setState/2,setStateMask/2,setText/2,setTextColour/2,
  setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxListItem/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxListItem() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemwxlistitem">external documentation</a>.
-spec new() -> wxListItem().
new() ->
  wxe_util:construct(?wxListItem_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemwxlistitem">external documentation</a>.
-spec new(Item) -> wxListItem() when
	Item::wxListItem().
new(#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ItemT,wxListItem),
  wxe_util:construct(?wxListItem_new_1,
  <<ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxListItem().
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_Clear,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetalign">external documentation</a>.
%%<br /> Res = ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
-spec getAlign(This) -> wx:wx_enum() when
	This::wxListItem().
getAlign(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetAlign,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxListItem().
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetcolumn">external documentation</a>.
-spec getColumn(This) -> integer() when
	This::wxListItem().
getColumn(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetColumn,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxListItem().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxListItem().
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetimage">external documentation</a>.
-spec getImage(This) -> integer() when
	This::wxListItem().
getImage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetImage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetmask">external documentation</a>.
-spec getMask(This) -> integer() when
	This::wxListItem().
getMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetMask,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetstate">external documentation</a>.
-spec getState(This) -> integer() when
	This::wxListItem().
getState(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetState,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxListItem().
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetText,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListItem().
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemgetwidth">external documentation</a>.
-spec getWidth(This) -> integer() when
	This::wxListItem().
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetalign">external documentation</a>.
%%<br /> Align = ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
-spec setAlign(This, Align) -> 'ok' when
	This::wxListItem(), Align::wx:wx_enum().
setAlign(#wx_ref{type=ThisT,ref=ThisRef},Align)
 when is_integer(Align) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetAlign,
  <<ThisRef:32/?UI,Align:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxListItem(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetcolumn">external documentation</a>.
-spec setColumn(This, Col) -> 'ok' when
	This::wxListItem(), Col::integer().
setColumn(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetColumn,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetfont">external documentation</a>.
-spec setFont(This, Font) -> 'ok' when
	This::wxListItem(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxListItem),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxListItem_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetid">external documentation</a>.
-spec setId(This, Id) -> 'ok' when
	This::wxListItem(), Id::integer().
setId(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetId,
  <<ThisRef:32/?UI,Id:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetimage">external documentation</a>.
-spec setImage(This, Image) -> 'ok' when
	This::wxListItem(), Image::integer().
setImage(#wx_ref{type=ThisT,ref=ThisRef},Image)
 when is_integer(Image) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetImage,
  <<ThisRef:32/?UI,Image:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetmask">external documentation</a>.
-spec setMask(This, Mask) -> 'ok' when
	This::wxListItem(), Mask::integer().
setMask(#wx_ref{type=ThisT,ref=ThisRef},Mask)
 when is_integer(Mask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetMask,
  <<ThisRef:32/?UI,Mask:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetstate">external documentation</a>.
-spec setState(This, State) -> 'ok' when
	This::wxListItem(), State::integer().
setState(#wx_ref{type=ThisT,ref=ThisRef},State)
 when is_integer(State) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetState,
  <<ThisRef:32/?UI,State:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetstatemask">external documentation</a>.
-spec setStateMask(This, StateMask) -> 'ok' when
	This::wxListItem(), StateMask::integer().
setStateMask(#wx_ref{type=ThisT,ref=ThisRef},StateMask)
 when is_integer(StateMask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetStateMask,
  <<ThisRef:32/?UI,StateMask:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsettext">external documentation</a>.
-spec setText(This, Text) -> 'ok' when
	This::wxListItem(), Text::unicode:chardata().
setText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxListItem),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxListItem_SetText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsettextcolour">external documentation</a>.
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxListItem(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistitem.html#wxlistitemsetwidth">external documentation</a>.
-spec setWidth(This, Width) -> 'ok' when
	This::wxListItem(), Width::integer().
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxListItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
