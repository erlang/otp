%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(wxListItem).
-moduledoc """
This class stores information about a `m:wxListCtrl` item or column.

`m:wxListItem` is a class which contains information about:

* Zero based item position; see `setId/2` and `getId/1`.

* Zero based column index; see `setColumn/2` and `getColumn/1`.

* The label (or header for columns); see `setText/2` and `getText/1`.

* The zero based index into an image list; see `getImage/1` and `setImage/2`.

* Application defined data; see `SetData()` (not implemented in wx) and `GetData()` (not
implemented in wx).

* For columns only: the width of the column; see `setWidth/2` and `getWidth/1`.

* For columns only: the format of the column; one of `wxLIST_FORMAT_LEFT`, `wxLIST_FORMAT_RIGHT`, `wxLIST_FORMAT_CENTRE`.
See `setAlign/2` and `getAlign/1`.

* The state of the item; see `setState/2` and `getState/1`. This is a bitlist of the following flags:

* `wxLIST_STATE_FOCUSED:` The item has the focus.

* `wxLIST_STATE_SELECTED:` The item is selected.

* `wxLIST_STATE_DONTCARE:` No special flags (the value of this constant is 0).

* `wxLIST_STATE_DROPHILITED:` The item is highlighted to receive a drop event. Win32 only.

* `wxLIST_STATE_CUT:` The item is in the cut state. Win32 only.

* A mask indicating which state flags are valid; this is a bitlist of the flags reported
above for the item state. See `setStateMask/2` and GetStateMask().

* A mask indicating which fields of this class are valid; see `setMask/2` and `getMask/1`. This is a bitlist of
the following flags:

* `wxLIST_MASK_STATE:` The state field is valid.

* `wxLIST_MASK_TEXT:` The label field is valid.

* `wxLIST_MASK_IMAGE:` The image field is valid.

* `wxLIST_MASK_DATA:` The application-defined data field is valid.

* `wxLIST_MASK_WIDTH:` The column width field is valid.

* `wxLIST_MASK_FORMAT:` The column format field is valid.

The `m:wxListItem` object can also contain item-specific colour and font information: for
this you need to call one of `setTextColour/2`, `setBackgroundColour/2` or `setFont/2` functions on it passing it the colour/font to use. If
the colour/font is not specified, the default list control colour/font is used.

See: `m:wxListCtrl`

wxWidgets docs: [wxListItem](https://docs.wxwidgets.org/3.2/classwx_list_item.html)
""".
-include("wxe.hrl").
-export([clear/1,destroy/1,getAlign/1,getBackgroundColour/1,getColumn/1,getFont/1,
  getId/1,getImage/1,getMask/1,getState/1,getText/1,getTextColour/1,getWidth/1,
  new/0,new/1,setAlign/2,setBackgroundColour/2,setColumn/2,setFont/2,
  setId/2,setImage/2,setMask/2,setState/2,setStateMask/2,setText/2,setTextColour/2,
  setWidth/2]).

%% inherited exports
-export([parent_class/1]).

-type wxListItem() :: wx:wx_object().
-export_type([wxListItem/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Constructor.".
-spec new() -> wxListItem().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxListItem_new_0),
  wxe_util:rec(?wxListItem_new_0).

-doc "".
-spec new(Item) -> wxListItem() when
	Item::wxListItem().
new(#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ItemT,wxListItem),
  wxe_util:queue_cmd(Item,?get_env(),?wxListItem_new_1),
  wxe_util:rec(?wxListItem_new_1).

-doc "Resets the item state to the default.".
-spec clear(This) -> 'ok' when
	This::wxListItem().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_Clear).

-doc """
Returns the alignment for this item.

Can be one of `wxLIST_FORMAT_LEFT`, `wxLIST_FORMAT_RIGHT` or `wxLIST_FORMAT_CENTRE`.
""".
%%  Res = ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
-spec getAlign(This) -> wx:wx_enum() when
	This::wxListItem().
getAlign(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetAlign),
  wxe_util:rec(?wxListItem_GetAlign).

-doc "Returns the background colour for this item.".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxListItem().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetBackgroundColour),
  wxe_util:rec(?wxListItem_GetBackgroundColour).

-doc "Returns the zero-based column; meaningful only in report mode.".
-spec getColumn(This) -> integer() when
	This::wxListItem().
getColumn(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetColumn),
  wxe_util:rec(?wxListItem_GetColumn).

-doc "Returns the font used to display the item.".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxListItem().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetFont),
  wxe_util:rec(?wxListItem_GetFont).

-doc "Returns the zero-based item position.".
-spec getId(This) -> integer() when
	This::wxListItem().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetId),
  wxe_util:rec(?wxListItem_GetId).

-doc "Returns the zero-based index of the image associated with the item into the image list.".
-spec getImage(This) -> integer() when
	This::wxListItem().
getImage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetImage),
  wxe_util:rec(?wxListItem_GetImage).

-doc """
Returns a bit mask indicating which fields of the structure are valid.

Can be any combination of the following values:

* wxLIST_MASK_STATE: `GetState` is valid.

* wxLIST_MASK_TEXT: `GetText` is valid.

* wxLIST_MASK_IMAGE: `GetImage` is valid.

* wxLIST_MASK_DATA: `GetData` is valid.

* wxLIST_MASK_WIDTH: `GetWidth` is valid.

* wxLIST_MASK_FORMAT: `GetFormat` is valid.
""".
-spec getMask(This) -> integer() when
	This::wxListItem().
getMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetMask),
  wxe_util:rec(?wxListItem_GetMask).

-doc """
Returns a bit field representing the state of the item.

Can be any combination of:

* wxLIST_STATE_DONTCARE: No special flags (the values of this constant is 0).

* wxLIST_STATE_DROPHILITED: The item is highlighted to receive a drop event. Win32 only.

* wxLIST_STATE_FOCUSED: The item has the focus.

* wxLIST_STATE_SELECTED: The item is selected.

* wxLIST_STATE_CUT: The item is in the cut state. Win32 only.
""".
-spec getState(This) -> integer() when
	This::wxListItem().
getState(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetState),
  wxe_util:rec(?wxListItem_GetState).

-doc "Returns the label/header text.".
-spec getText(This) -> unicode:charlist() when
	This::wxListItem().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetText),
  wxe_util:rec(?wxListItem_GetText).

-doc "Returns the text colour.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListItem().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetTextColour),
  wxe_util:rec(?wxListItem_GetTextColour).

-doc """
Meaningful only for column headers in report mode.

Returns the column width.
""".
-spec getWidth(This) -> integer() when
	This::wxListItem().
getWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,?get_env(),?wxListItem_GetWidth),
  wxe_util:rec(?wxListItem_GetWidth).

-doc """
Sets the alignment for the item.

See also `getAlign/1`
""".
%%  Align = ?wxLIST_FORMAT_LEFT | ?wxLIST_FORMAT_RIGHT | ?wxLIST_FORMAT_CENTRE | ?wxLIST_FORMAT_CENTER
-spec setAlign(This, Align) -> 'ok' when
	This::wxListItem(), Align::wx:wx_enum().
setAlign(#wx_ref{type=ThisT}=This,Align)
 when is_integer(Align) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Align,?get_env(),?wxListItem_SetAlign).

-doc "Sets the background colour for the item.".
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxListItem(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxListItem_SetBackgroundColour).

-doc """
Sets the zero-based column.

Meaningful only in report mode.
""".
-spec setColumn(This, Col) -> 'ok' when
	This::wxListItem(), Col::integer().
setColumn(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListItem_SetColumn).

-doc "Sets the font for the item.".
-spec setFont(This, Font) -> 'ok' when
	This::wxListItem(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxListItem),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxListItem_SetFont).

-doc "Sets the zero-based item position.".
-spec setId(This, Id) -> 'ok' when
	This::wxListItem(), Id::integer().
setId(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxListItem_SetId).

-doc "Sets the zero-based index of the image associated with the item into the image list.".
-spec setImage(This, Image) -> 'ok' when
	This::wxListItem(), Image::integer().
setImage(#wx_ref{type=ThisT}=This,Image)
 when is_integer(Image) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Image,?get_env(),?wxListItem_SetImage).

-doc """
Sets the mask of valid fields.

See `getMask/1`.
""".
-spec setMask(This, Mask) -> 'ok' when
	This::wxListItem(), Mask::integer().
setMask(#wx_ref{type=ThisT}=This,Mask)
 when is_integer(Mask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Mask,?get_env(),?wxListItem_SetMask).

-doc """
Sets the item state flags (note that the valid state flags are influenced by the value of
the state mask, see `setStateMask/2`).

See `getState/1` for valid flag values.
""".
-spec setState(This, State) -> 'ok' when
	This::wxListItem(), State::integer().
setState(#wx_ref{type=ThisT}=This,State)
 when is_integer(State) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,State,?get_env(),?wxListItem_SetState).

-doc """
Sets the bitmask that is used to determine which of the state flags are to be set.

See also `setState/2`.
""".
-spec setStateMask(This, StateMask) -> 'ok' when
	This::wxListItem(), StateMask::integer().
setStateMask(#wx_ref{type=ThisT}=This,StateMask)
 when is_integer(StateMask) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,StateMask,?get_env(),?wxListItem_SetStateMask).

-doc "Sets the text label for the item.".
-spec setText(This, Text) -> 'ok' when
	This::wxListItem(), Text::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxListItem),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxListItem_SetText).

-doc "Sets the text colour for the item.".
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxListItem(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxListItem_SetTextColour).

-doc """
Meaningful only for column headers in report mode.

Sets the column width.
""".
-spec setWidth(This, Width) -> 'ok' when
	This::wxListItem(), Width::integer().
setWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxListItem),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxListItem_SetWidth).

-doc "Destroys the object".
-spec destroy(This::wxListItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItem),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
