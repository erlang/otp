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

-module(wxListEvent).
-moduledoc """
A list event holds information about events associated with `m:wxListCtrl` objects.

See: `m:wxListCtrl`

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxListEvent](https://docs.wxwidgets.org/3.2/classwx_list_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxListEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getCacheFrom/1,getCacheTo/1,getColumn/1,getData/1,getImage/1,getIndex/1,
  getItem/1,getKeyCode/1,getLabel/1,getMask/1,getPoint/1,getText/1,isEditCancelled/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxListEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxListEventType() :: 'command_list_begin_drag' | 'command_list_begin_rdrag' | 'command_list_begin_label_edit' | 'command_list_end_label_edit' | 'command_list_delete_item' | 'command_list_delete_all_items' | 'command_list_key_down' | 'command_list_insert_item' | 'command_list_col_click' | 'command_list_col_right_click' | 'command_list_col_begin_drag' | 'command_list_col_dragging' | 'command_list_col_end_drag' | 'command_list_item_selected' | 'command_list_item_deselected' | 'command_list_item_right_click' | 'command_list_item_middle_click' | 'command_list_item_activated' | 'command_list_item_focused' | 'command_list_cache_hint'.
-export_type([wxListEvent/0, wxList/0, wxListEventType/0]).
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
For `EVT\_LIST\_CACHE\_HINT` event only: return the first item which the list control
advises us to cache.
""".
-spec getCacheFrom(This) -> integer() when
	This::wxListEvent().
getCacheFrom(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetCacheFrom),
  wxe_util:rec(?wxListEvent_GetCacheFrom).

-doc """
For `EVT\_LIST\_CACHE\_HINT` event only: return the last item (inclusive) which the list
control advises us to cache.
""".
-spec getCacheTo(This) -> integer() when
	This::wxListEvent().
getCacheTo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetCacheTo),
  wxe_util:rec(?wxListEvent_GetCacheTo).

-doc "Key code if the event is a keypress event.".
-spec getKeyCode(This) -> integer() when
	This::wxListEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetKeyCode),
  wxe_util:rec(?wxListEvent_GetKeyCode).

-doc "The item index.".
-spec getIndex(This) -> integer() when
	This::wxListEvent().
getIndex(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetIndex),
  wxe_util:rec(?wxListEvent_GetIndex).

-doc """
The column position: it is only used with `COL` events.

For the column dragging events, it is the column to the left of the divider being
dragged, for the column click events it may be -1 if the user clicked in the list control
header outside any column.
""".
-spec getColumn(This) -> integer() when
	This::wxListEvent().
getColumn(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetColumn),
  wxe_util:rec(?wxListEvent_GetColumn).

-doc "The position of the mouse pointer if the event is a drag event.".
-spec getPoint(This) -> {X::integer(), Y::integer()} when
	This::wxListEvent().
getPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetPoint),
  wxe_util:rec(?wxListEvent_GetPoint).

-doc "The (new) item label for `EVT\_LIST\_END\_LABEL\_EDIT` event.".
-spec getLabel(This) -> unicode:charlist() when
	This::wxListEvent().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetLabel),
  wxe_util:rec(?wxListEvent_GetLabel).

-doc "The text.".
-spec getText(This) -> unicode:charlist() when
	This::wxListEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetText),
  wxe_util:rec(?wxListEvent_GetText).

-doc "The image.".
-spec getImage(This) -> integer() when
	This::wxListEvent().
getImage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetImage),
  wxe_util:rec(?wxListEvent_GetImage).

-doc "The data.".
-spec getData(This) -> integer() when
	This::wxListEvent().
getData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetData),
  wxe_util:rec(?wxListEvent_GetData).

-doc "The mask.".
-spec getMask(This) -> integer() when
	This::wxListEvent().
getMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetMask),
  wxe_util:rec(?wxListEvent_GetMask).

-doc """
An item object, used by some events.

See also `wxListCtrl:setItem/5`.
""".
-spec getItem(This) -> wxListItem:wxListItem() when
	This::wxListEvent().
getItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_GetItem),
  wxe_util:rec(?wxListEvent_GetItem).

-doc """
This method only makes sense for `EVT\_LIST\_END\_LABEL\_EDIT` message and returns true
if it the label editing has been cancelled by the user (`getLabel/1` returns an empty
string in this case but it doesn't allow the application to distinguish between really
cancelling the edit and the admittedly rare case when the user wants to rename it to an
empty string).
""".
-spec isEditCancelled(This) -> boolean() when
	This::wxListEvent().
isEditCancelled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxListEvent_IsEditCancelled),
  wxe_util:rec(?wxListEvent_IsEditCancelled).

 %% From wxNotifyEvent
-doc false.
veto(This) -> wxNotifyEvent:veto(This).
-doc false.
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
-doc false.
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
-doc false.
getString(This) -> wxCommandEvent:getString(This).
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
