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

-module(wxUpdateUIEvent).
-moduledoc """
This class is used for pseudo-events which are called by wxWidgets to give an application
the chance to update various user interface elements.

Without update UI events, an application has to work hard to check/uncheck,
enable/disable, show/hide, and set the text for elements such as menu items and toolbar
buttons. The code for doing this has to be mixed up with the code that is invoked when an
action is invoked for a menu item or button.

With update UI events, you define an event handler to look at the state of the
application and change UI elements accordingly. wxWidgets will call your member functions
in idle time, so you don't have to worry where to call this code.

In addition to being a clearer and more declarative method, it also means you don't have
to worry whether you're updating a toolbar or menubar identifier. The same handler can
update a menu item and toolbar button, if the identifier is the same. Instead of directly
manipulating the menu or button, you call functions in the event object, such as `check/2`.
wxWidgets will determine whether such a call has been made, and which UI element to update.

These events will work for popup menus as well as menubars. Just before a menu is popped
up, `wxMenu::UpdateUI` (not implemented in wx) is called to process any UI events for the
window that owns the menu.

If you find that the overhead of UI update processing is affecting your application, you
can do one or both of the following:

* Call `setMode/1` with a value of wxUPDATE_UI_PROCESS_SPECIFIED, and set the extra style
wxWS_EX_PROCESS_UI_UPDATES for every window that should receive update events. No other
windows will receive update events.

* Call `setUpdateInterval/1` with a millisecond value to set the delay between updates. You may need to call `wxWindow:updateWindowUI/2` at
critical points, for example when a dialog is about to be shown, in case the user sees a
slight delay before windows are updated.

Note that although events are sent in idle time, defining a `m:wxIdleEvent` handler for
a window does not affect this because the events are sent from `wxWindow::OnInternalIdle`
(not implemented in wx) which is always called in idle time.

wxWidgets tries to optimize update events on some platforms. On Windows and GTK+, events
for menubar items are only sent when the menu is about to be shown, and not in idle time.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxUpdateUIEvent](https://docs.wxwidgets.org/3.2/classwx_update_u_i_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxUpdateUIEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([canUpdate/1,check/2,enable/2,getChecked/1,getEnabled/1,getMode/0,getSetChecked/1,
  getSetEnabled/1,getSetShown/1,getSetText/1,getShown/1,getText/1,getUpdateInterval/0,
  resetUpdateTime/0,setMode/1,setText/2,setUpdateInterval/1,show/2]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxUpdateUIEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxUpdateUIEventType() :: 'update_ui'.
-export_type([wxUpdateUIEvent/0, wxUpdateUI/0, wxUpdateUIEventType/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns true if it is appropriate to update (send UI update events to) this window.

This function looks at the mode used (see `setMode/1`), the wxWS_EX_PROCESS_UI_UPDATES flag in `window`,
the time update events were last sent in idle time, and the update interval, to determine
whether events should be sent to this window now. By default this will always return true
because the update mode is initially wxUPDATE_UI_PROCESS_ALL and the interval is set to 0;
so update events will be sent as often as possible. You can reduce the frequency that
events are sent by changing the mode and/or setting an update interval.

See:
* `resetUpdateTime/0`

* `setUpdateInterval/1`

* `setMode/1`
""".
-spec canUpdate(Window) -> boolean() when
	Window::wxWindow:wxWindow().
canUpdate(#wx_ref{type=WindowT}=Window) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,?get_env(),?wxUpdateUIEvent_CanUpdate),
  wxe_util:rec(?wxUpdateUIEvent_CanUpdate).

-doc "Check or uncheck the UI element.".
-spec check(This, Check) -> 'ok' when
	This::wxUpdateUIEvent(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Check)
 when is_boolean(Check) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Check,?get_env(),?wxUpdateUIEvent_Check).

-doc "Enable or disable the UI element.".
-spec enable(This, Enable) -> 'ok' when
	This::wxUpdateUIEvent(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxUpdateUIEvent_Enable).

-doc "Show or hide the UI element.".
-spec show(This, Show) -> 'ok' when
	This::wxUpdateUIEvent(), Show::boolean().
show(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxUpdateUIEvent_Show).

-doc "Returns true if the UI element should be checked.".
-spec getChecked(This) -> boolean() when
	This::wxUpdateUIEvent().
getChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetChecked),
  wxe_util:rec(?wxUpdateUIEvent_GetChecked).

-doc "Returns true if the UI element should be enabled.".
-spec getEnabled(This) -> boolean() when
	This::wxUpdateUIEvent().
getEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetEnabled),
  wxe_util:rec(?wxUpdateUIEvent_GetEnabled).

-doc "Returns true if the UI element should be shown.".
-spec getShown(This) -> boolean() when
	This::wxUpdateUIEvent().
getShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetShown),
  wxe_util:rec(?wxUpdateUIEvent_GetShown).

-doc """
Returns true if the application has called `check/2`.

For wxWidgets internal use only.
""".
-spec getSetChecked(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetChecked),
  wxe_util:rec(?wxUpdateUIEvent_GetSetChecked).

-doc """
Returns true if the application has called `enable/2`.

For wxWidgets internal use only.
""".
-spec getSetEnabled(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetEnabled),
  wxe_util:rec(?wxUpdateUIEvent_GetSetEnabled).

-doc """
Returns true if the application has called `show/2`.

For wxWidgets internal use only.
""".
-spec getSetShown(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetShown),
  wxe_util:rec(?wxUpdateUIEvent_GetSetShown).

-doc """
Returns true if the application has called `setText/2`.

For wxWidgets internal use only.
""".
-spec getSetText(This) -> boolean() when
	This::wxUpdateUIEvent().
getSetText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetSetText),
  wxe_util:rec(?wxUpdateUIEvent_GetSetText).

-doc "Returns the text that should be set for the UI element.".
-spec getText(This) -> unicode:charlist() when
	This::wxUpdateUIEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxUpdateUIEvent_GetText),
  wxe_util:rec(?wxUpdateUIEvent_GetText).

-doc """
Static function returning a value specifying how wxWidgets will send update events: to
all windows, or only to those which specify that they will process the events.

See: `setMode/1`
""".
%%  Res = ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
-spec getMode() -> wx:wx_enum().
getMode() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_GetMode),
  wxe_util:rec(?wxUpdateUIEvent_GetMode).

-doc """
Returns the current interval between updates in milliseconds.

The value -1 disables updates, 0 updates as frequently as possible.

See: `setUpdateInterval/1`
""".
-spec getUpdateInterval() -> integer().
getUpdateInterval() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_GetUpdateInterval),
  wxe_util:rec(?wxUpdateUIEvent_GetUpdateInterval).

-doc """
Used internally to reset the last-updated time to the current time.

It is assumed that update events are normally sent in idle time, so this is called at the
end of idle processing.

See:
* `canUpdate/1`

* `setUpdateInterval/1`

* `setMode/1`
""".
-spec resetUpdateTime() -> 'ok'.
resetUpdateTime() ->
  wxe_util:queue_cmd(?get_env(), ?wxUpdateUIEvent_ResetUpdateTime).

-doc """
Specify how wxWidgets will send update events: to all windows, or only to those which
specify that they will process the events.
""".
%%  Mode = ?wxUPDATE_UI_PROCESS_ALL | ?wxUPDATE_UI_PROCESS_SPECIFIED
-spec setMode(Mode) -> 'ok' when
	Mode::wx:wx_enum().
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:queue_cmd(Mode,?get_env(),?wxUpdateUIEvent_SetMode).

-doc "Sets the text for this UI element.".
-spec setText(This, Text) -> 'ok' when
	This::wxUpdateUIEvent(), Text::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxUpdateUIEvent),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxUpdateUIEvent_SetText).

-doc """
Sets the interval between updates in milliseconds.

Set to -1 to disable updates, or to 0 to update as frequently as possible. The default is 0.

Use this to reduce the overhead of UI update events if your application has a lot of
windows. If you set the value to -1 or greater than 0, you may also need to call `wxWindow:updateWindowUI/2` at
appropriate points in your application, such as when a dialog is about to be shown.
""".
-spec setUpdateInterval(UpdateInterval) -> 'ok' when
	UpdateInterval::integer().
setUpdateInterval(UpdateInterval)
 when is_integer(UpdateInterval) ->
  wxe_util:queue_cmd(UpdateInterval,?get_env(),?wxUpdateUIEvent_SetUpdateInterval).

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
