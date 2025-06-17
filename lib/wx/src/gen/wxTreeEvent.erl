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

-module(wxTreeEvent).
-moduledoc """
A tree event holds information about events associated with `m:wxTreeCtrl` objects.

To process input from a tree control, use these event handler macros to direct input to
member functions that take a `m:wxTreeEvent` argument.

See: `m:wxTreeCtrl`

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxTreeEvent](https://docs.wxwidgets.org/3.2/classwx_tree_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxTreeEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getItem/1,getKeyCode/1,getKeyEvent/1,getLabel/1,getOldItem/1,getPoint/1,
  isEditCancelled/1,setToolTip/2]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,
  getSkipped/1,getString/1,getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,
  isSelection/1,parent_class/1,resumePropagation/2,setInt/2,setString/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxTreeEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxTreeEventType() :: 'command_tree_begin_drag' | 'command_tree_begin_rdrag' | 'command_tree_begin_label_edit' | 'command_tree_end_label_edit' | 'command_tree_delete_item' | 'command_tree_get_info' | 'command_tree_set_info' | 'command_tree_item_expanded' | 'command_tree_item_expanding' | 'command_tree_item_collapsed' | 'command_tree_item_collapsing' | 'command_tree_sel_changed' | 'command_tree_sel_changing' | 'command_tree_key_down' | 'command_tree_item_activated' | 'command_tree_item_right_click' | 'command_tree_item_middle_click' | 'command_tree_end_drag' | 'command_tree_state_image_click' | 'command_tree_item_gettooltip' | 'command_tree_item_menu' | 'dirctrl_selectionchanged' | 'dirctrl_fileactivated'.
-export_type([wxTreeEvent/0, wxTree/0, wxTreeEventType/0]).
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the key code if the event is a key event.

Use `getKeyEvent/1` to get the values of the modifier keys for this event (i.e. Shift or Ctrl).
""".
-spec getKeyCode(This) -> integer() when
	This::wxTreeEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetKeyCode),
  wxe_util:rec(?wxTreeEvent_GetKeyCode).

-doc "Returns the item (valid for all events).".
-spec getItem(This) -> integer() when
	This::wxTreeEvent().
getItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetItem),
  wxe_util:rec(?wxTreeEvent_GetItem).

-doc "Returns the key event for `EVT\_TREE\_KEY\_DOWN` events.".
-spec getKeyEvent(This) -> wxKeyEvent:wxKeyEvent() when
	This::wxTreeEvent().
getKeyEvent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetKeyEvent),
  wxe_util:rec(?wxTreeEvent_GetKeyEvent).

-doc "Returns the label if the event is a begin or end edit label event.".
-spec getLabel(This) -> unicode:charlist() when
	This::wxTreeEvent().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetLabel),
  wxe_util:rec(?wxTreeEvent_GetLabel).

-doc """
Returns the old item index (valid for `EVT\_TREE\_SEL\_CHANGING` and `EVT\_TREE\_SEL\_CHANGED`
events).
""".
-spec getOldItem(This) -> integer() when
	This::wxTreeEvent().
getOldItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetOldItem),
  wxe_util:rec(?wxTreeEvent_GetOldItem).

-doc """
Returns the position of the mouse pointer if the event is a drag or menu-context event.

In both cases the position is in client coordinates - i.e. relative to the `m:wxTreeCtrl`
window (so that you can pass it directly to e.g. `wxWindow:popupMenu/4`).
""".
-spec getPoint(This) -> {X::integer(), Y::integer()} when
	This::wxTreeEvent().
getPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_GetPoint),
  wxe_util:rec(?wxTreeEvent_GetPoint).

-doc """
Returns true if the label edit was cancelled.

This should be called from within an `EVT_TREE_END_LABEL_EDIT` handler.
""".
-spec isEditCancelled(This) -> boolean() when
	This::wxTreeEvent().
isEditCancelled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeEvent_IsEditCancelled),
  wxe_util:rec(?wxTreeEvent_IsEditCancelled).

-doc """
Set the tooltip for the item (valid for `EVT\_TREE\_ITEM\_GETTOOLTIP` events).

Windows only.
""".
-spec setToolTip(This, Tooltip) -> 'ok' when
	This::wxTreeEvent(), Tooltip::unicode:chardata().
setToolTip(#wx_ref{type=ThisT}=This,Tooltip)
 when ?is_chardata(Tooltip) ->
  ?CLASS(ThisT,wxTreeEvent),
  Tooltip_UC = unicode:characters_to_binary(Tooltip),
  wxe_util:queue_cmd(This,Tooltip_UC,?get_env(),?wxTreeEvent_SetToolTip).

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
