%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
parent_class(wxNotifyEvent) -> true;
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgetstring">external documentation</a>.
-spec getString(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetString),
  wxe_util:rec(?wxWebViewEvent_GetString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgetint">external documentation</a>.
-spec getInt(This) -> integer() when
	This::wxWebViewEvent().
getInt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetInt),
  wxe_util:rec(?wxWebViewEvent_GetInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgettarget">external documentation</a>.
-spec getTarget(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getTarget(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetTarget),
  wxe_util:rec(?wxWebViewEvent_GetTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebviewevent.html#wxwebvieweventgeturl">external documentation</a>.
-spec getURL(This) -> unicode:charlist() when
	This::wxWebViewEvent().
getURL(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebViewEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxWebViewEvent_GetURL),
  wxe_util:rec(?wxWebViewEvent_GetURL).

 %% From wxNotifyEvent
%% @hidden
veto(This) -> wxNotifyEvent:veto(This).
%% @hidden
isAllowed(This) -> wxNotifyEvent:isAllowed(This).
%% @hidden
allow(This) -> wxNotifyEvent:allow(This).
 %% From wxCommandEvent
%% @hidden
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
