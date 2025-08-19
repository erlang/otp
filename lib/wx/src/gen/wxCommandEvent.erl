%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxCommandEvent).
-moduledoc """
This event class contains information about command events, which originate from a
variety of simple controls.

Note that wxCommandEvents and wxCommandEvent-derived event classes by default and unlike
other wxEvent-derived classes propagate upward from the source window (the window which
emits the event) up to the first parent which processes the event. Be sure to read overview_events_propagation.

More complex controls, such as `m:wxTreeCtrl`, have separate command event classes.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxCommandEvent](https://docs.wxwidgets.org/3.2/classwx_command_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxCommandEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getClientData/1,getExtraLong/1,getInt/1,getSelection/1,getString/1,
  isChecked/1,isSelection/1,setInt/2,setString/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxCommandEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxCommandEventType() :: 'command_button_clicked' | 'command_checkbox_clicked' | 'command_choice_selected' | 'command_listbox_selected' | 'command_listbox_doubleclicked' | 'command_text_updated' | 'command_text_enter' | 'text_maxlen' | 'command_menu_selected' | 'command_slider_updated' | 'command_radiobox_selected' | 'command_radiobutton_selected' | 'command_scrollbar_updated' | 'command_vlbox_selected' | 'command_combobox_selected' | 'combobox_dropdown' | 'combobox_closeup' | 'command_tool_rclicked' | 'command_tool_enter' | 'tool_dropdown' | 'command_checklistbox_toggled' | 'command_togglebutton_clicked' | 'command_left_click' | 'command_left_dclick' | 'command_right_click' | 'command_set_focus' | 'command_kill_focus' | 'command_enter' | 'notification_message_click' | 'notification_message_dismissed' | 'notification_message_action'.
-export_type([wxCommandEvent/0, wxCommand/0, wxCommandEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns client object pointer for a listbox or choice selection event (not valid for a
deselection).
""".
-spec getClientData(This) -> term() when
	This::wxCommandEvent().
getClientData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_getClientData),
  wxe_util:rec(?wxCommandEvent_getClientData).

-doc """
Returns extra information dependent on the event objects type.

If the event comes from a listbox selection, it is a boolean determining whether the
event was a selection (true) or a deselection (false). A listbox deselection only occurs
for multiple-selection boxes, and in this case the index and string values are
indeterminate and the listbox must be examined by the application.
""".
-spec getExtraLong(This) -> integer() when
	This::wxCommandEvent().
getExtraLong(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetExtraLong),
  wxe_util:rec(?wxCommandEvent_GetExtraLong).

-doc """
Returns the integer identifier corresponding to a listbox, choice or radiobox selection
(only if the event was a selection, not a deselection), or a boolean value representing
the value of a checkbox.

For a menu item, this method returns -1 if the item is not checkable or a boolean value
(true or false) for checkable items indicating the new state of the item.
""".
-spec getInt(This) -> integer() when
	This::wxCommandEvent().
getInt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetInt),
  wxe_util:rec(?wxCommandEvent_GetInt).

-doc "Returns item index for a listbox or choice selection event (not valid for a deselection).".
-spec getSelection(This) -> integer() when
	This::wxCommandEvent().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetSelection),
  wxe_util:rec(?wxCommandEvent_GetSelection).

-doc """
Returns item string for a listbox or choice selection event.

If one or several items have been deselected, returns the index of the first deselected
item. If some items have been selected and others deselected at the same time, it will
return the index of the first selected item.
""".
-spec getString(This) -> unicode:charlist() when
	This::wxCommandEvent().
getString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_GetString),
  wxe_util:rec(?wxCommandEvent_GetString).

-doc """
This method can be used with checkbox and menu events: for the checkboxes, the method
returns true for a selection event and false for a deselection one.

For the menu events, this method indicates if the menu item just has become checked or
unchecked (and thus only makes sense for checkable menu items).

Notice that this method cannot be used with `m:wxCheckListBox` currently.
""".
-spec isChecked(This) -> boolean() when
	This::wxCommandEvent().
isChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_IsChecked),
  wxe_util:rec(?wxCommandEvent_IsChecked).

-doc """
For a listbox or similar event, returns true if it is a selection, false if it is a
deselection.

If some items have been selected and others deselected at the same time, it will return
true.
""".
-spec isSelection(This) -> boolean() when
	This::wxCommandEvent().
isSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCommandEvent_IsSelection),
  wxe_util:rec(?wxCommandEvent_IsSelection).

-doc "Sets the `m\_commandInt` member.".
-spec setInt(This, IntCommand) -> 'ok' when
	This::wxCommandEvent(), IntCommand::integer().
setInt(#wx_ref{type=ThisT}=This,IntCommand)
 when is_integer(IntCommand) ->
  ?CLASS(ThisT,wxCommandEvent),
  wxe_util:queue_cmd(This,IntCommand,?get_env(),?wxCommandEvent_SetInt).

-doc "Sets the `m\_commandString` member.".
-spec setString(This, String) -> 'ok' when
	This::wxCommandEvent(), String::unicode:chardata().
setString(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxCommandEvent),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxCommandEvent_SetString).

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
