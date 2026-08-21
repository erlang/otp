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

-module(wxWebViewEvent).
-moduledoc """
A navigation event holds information about events associated with `m:wxWebView` objects.

This class is derived, and can use functions, from:

* `m:wxNotifyEvent`

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxWebViewEvent](https://docs.wxwidgets.org/3.2/classwx_web_view_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxWebViewEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getInt/1,getString/1,getTarget/1,getURL/1]).

%% inherited exports
-export([allow/1,getClientData/1,getExtraLong/1,getId/1,getSelection/1,getSkipped/1,
  getTimestamp/1,isAllowed/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1,veto/1]).

-type wxWebViewEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxWebViewEventType() :: 'webview_navigating' | 'webview_navigated' | 'webview_loaded' | 'webview_error' | 'webview_newwindow' | 'webview_title_changed'.
-export_type([wxWebViewEvent/0, wxWebView/0, wxWebViewEventType/0]).
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns item string for a listbox or choice selection event.

If one or several items have been deselected, returns the index of the first deselected
item. If some items have been selected and others deselected at the same time, it will
return the index of the first selected item.
""".
-spec getString(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetString),
  wxe_util:rec(?wxWebViewEvent_GetString).

-doc """
Returns the integer identifier corresponding to a listbox, choice or radiobox selection
(only if the event was a selection, not a deselection), or a boolean value representing
the value of a checkbox.

For a menu item, this method returns -1 if the item is not checkable or a boolean value
(true or false) for checkable items indicating the new state of the item.
""".
-spec getInt(This) -> integer() when
	This::wxWebViewEvent().
getInt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetInt),
  wxe_util:rec(?wxWebViewEvent_GetInt).

-doc """
Get the name of the target frame which the url of this event has been or will be loaded
into.

This may return an empty string if the frame is not available.
""".
-spec getTarget(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getTarget(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetTarget),
  wxe_util:rec(?wxWebViewEvent_GetTarget).

-doc "Get the URL being visited.".
-spec getURL(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getURL(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetURL),
  wxe_util:rec(?wxWebViewEvent_GetURL).

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
getSelection(This) -> wxCommandEvent:getSelection(This).
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
