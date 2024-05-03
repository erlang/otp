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

-module(wxWebViewEvent).
-moduledoc """
Functions for wxWebViewEvent class

A navigation event holds information about events associated with `m:wxWebView`
objects.

Since: 2.9.3

See: `m:wxWebView`

This class is derived (and can use functions) from: `m:wxNotifyEvent`
`m:wxCommandEvent` `m:wxEvent`

wxWidgets docs:
[wxWebViewEvent](https://docs.wxwidgets.org/3.1/classwx_web_view_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxWebViewEventType`](`t:wxWebViewEventType/0`) to subscribe to events of this
type.
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
%% @hidden
-doc false.
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgetstring">external documentation</a>.
-doc """
Returns item string for a listbox or choice selection event.

If one or several items have been deselected, returns the index of the first
deselected item. If some items have been selected and others deselected at the
same time, it will return the index of the first selected item.
""".
-spec getString(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetString),
  wxe_util:rec(?wxWebViewEvent_GetString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgetint">external documentation</a>.
-doc """
Returns the integer identifier corresponding to a listbox, choice or radiobox
selection (only if the event was a selection, not a deselection), or a boolean
value representing the value of a checkbox.

For a menu item, this method returns -1 if the item is not checkable or a
boolean value (true or false) for checkable items indicating the new state of
the item.
""".
-spec getInt(This) -> integer() when
	This::wxWebViewEvent().
getInt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetInt),
  wxe_util:rec(?wxWebViewEvent_GetInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgettarget">external documentation</a>.
-doc """
Get the name of the target frame which the url of this event has been or will be
loaded into.

This may return an empty string if the frame is not available.
""".
-spec getTarget(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getTarget(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetTarget),
  wxe_util:rec(?wxWebViewEvent_GetTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgeturl">external documentation</a>.
-doc "Get the URL being visited.".
-spec getURL(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getURL(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetURL),
  wxe_util:rec(?wxWebViewEvent_GetURL).

 %% From wxNotifyEvent
%% @hidden
-doc false.
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
-doc false.
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
-doc false.
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
%% @hidden
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
%% @hidden
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
-doc false.
skip(This) -> wxEvent:skip(This).
%% @hidden
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
-doc false.
getId(This) -> wxEvent:getId(This).
