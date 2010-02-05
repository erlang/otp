%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html">wxListItem</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxListItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemwxlistitem">external documentation</a>.
new() ->
  wxe_util:construct(?wxListItem_new_0,
  <<>>).

%% @spec (Item::wxListItem()) -> wxListItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemwxlistitem">external documentation</a>.
new(#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ItemT,wxListItem),
  wxe_util:construct(?wxListItem_new_1,
  <<ItemRef:32/?UI>>).

%% @spec (This::wxListItem()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemclear">external documentation</a>.
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_Clear,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> WxListColumnFormat
%% WxListColumnFormat = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetalign">external documentation</a>.
%%<br /> WxListColumnFormat is one of ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
getAlign(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetAlign,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetbackgroundcolour">external documentation</a>.
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetcolumn">external documentation</a>.
getColumn(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetColumn,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetid">external documentation</a>.
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetimage">external documentation</a>.
getImage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetImage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetmask">external documentation</a>.
getMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetMask,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetstate">external documentation</a>.
getState(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetState,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgettext">external documentation</a>.
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgettextcolour">external documentation</a>.
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemgetwidth">external documentation</a>.
getWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:call(?wxListItem_GetWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxListItem(), Align::WxListColumnFormat) -> ok
%% WxListColumnFormat = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetalign">external documentation</a>.
%%<br /> WxListColumnFormat is one of ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
setAlign(#wx_ref{type=ThisT,ref=ThisRef},Align)
 when is_integer(Align) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetAlign,
  <<ThisRef:32/?UI,Align:32/?UI>>).

%% @spec (This::wxListItem(), ColBack::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetbackgroundcolour">external documentation</a>.
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},ColBack)
 when tuple_size(ColBack) =:= 3; tuple_size(ColBack) =:= 4 ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColBack)):16/binary>>).

%% @spec (This::wxListItem(), Col::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetcolumn">external documentation</a>.
setColumn(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetColumn,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @spec (This::wxListItem(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxListItem),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxListItem_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxListItem(), Id::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetid">external documentation</a>.
setId(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetId,
  <<ThisRef:32/?UI,Id:32/?UI>>).

%% @spec (This::wxListItem(), Image::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetimage">external documentation</a>.
setImage(#wx_ref{type=ThisT,ref=ThisRef},Image)
 when is_integer(Image) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetImage,
  <<ThisRef:32/?UI,Image:32/?UI>>).

%% @spec (This::wxListItem(), Mask::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetmask">external documentation</a>.
setMask(#wx_ref{type=ThisT,ref=ThisRef},Mask)
 when is_integer(Mask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetMask,
  <<ThisRef:32/?UI,Mask:32/?UI>>).

%% @spec (This::wxListItem(), State::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetstate">external documentation</a>.
setState(#wx_ref{type=ThisT,ref=ThisRef},State)
 when is_integer(State) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetState,
  <<ThisRef:32/?UI,State:32/?UI>>).

%% @spec (This::wxListItem(), StateMask::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetstatemask">external documentation</a>.
setStateMask(#wx_ref{type=ThisT,ref=ThisRef},StateMask)
 when is_integer(StateMask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetStateMask,
  <<ThisRef:32/?UI,StateMask:32/?UI>>).

%% @spec (This::wxListItem(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsettext">external documentation</a>.
setText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxListItem),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxListItem_SetText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxListItem(), ColText::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsettextcolour">external documentation</a>.
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},ColText)
 when tuple_size(ColText) =:= 3; tuple_size(ColText) =:= 4 ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(ColText)):16/binary>>).

%% @spec (This::wxListItem(), Width::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistitem.html#wxlistitemsetwidth">external documentation</a>.
setWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:cast(?wxListItem_SetWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @spec (This::wxListItem()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
