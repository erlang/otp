%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

-module(wxNavigationKeyEvent).
-include("wxe.hrl").
-export([getCurrentFocus/1,getDirection/1,isFromTab/1,isWindowChange/1,setCurrentFocus/2,
  setDirection/2,setFromTab/2,setWindowChange/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxNavigationKeyEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxNavigationKeyEventType() :: 'navigation_key'.
-export_type([wxNavigationKeyEvent/0, wxNavigationKey/0, wxNavigationKeyEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetdirection">external documentation</a>.
-spec getDirection(This) -> boolean() when
	This::wxNavigationKeyEvent().
getDirection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_GetDirection),
  wxe_util:rec(?wxNavigationKeyEvent_GetDirection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetdirection">external documentation</a>.
-spec setDirection(This, Direction) -> 'ok' when
	This::wxNavigationKeyEvent(), Direction::boolean().
setDirection(#wx_ref{type=ThisT}=This,Direction)
 when is_boolean(Direction) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,Direction,?get_env(),?wxNavigationKeyEvent_SetDirection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventiswindowchange">external documentation</a>.
-spec isWindowChange(This) -> boolean() when
	This::wxNavigationKeyEvent().
isWindowChange(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_IsWindowChange),
  wxe_util:rec(?wxNavigationKeyEvent_IsWindowChange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetwindowchange">external documentation</a>.
-spec setWindowChange(This, WindowChange) -> 'ok' when
	This::wxNavigationKeyEvent(), WindowChange::boolean().
setWindowChange(#wx_ref{type=ThisT}=This,WindowChange)
 when is_boolean(WindowChange) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,WindowChange,?get_env(),?wxNavigationKeyEvent_SetWindowChange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventisfromtab">external documentation</a>.
-spec isFromTab(This) -> boolean() when
	This::wxNavigationKeyEvent().
isFromTab(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_IsFromTab),
  wxe_util:rec(?wxNavigationKeyEvent_IsFromTab).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetfromtab">external documentation</a>.
-spec setFromTab(This, FromTab) -> 'ok' when
	This::wxNavigationKeyEvent(), FromTab::boolean().
setFromTab(#wx_ref{type=ThisT}=This,FromTab)
 when is_boolean(FromTab) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,FromTab,?get_env(),?wxNavigationKeyEvent_SetFromTab).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetcurrentfocus">external documentation</a>.
-spec getCurrentFocus(This) -> wxWindow:wxWindow() when
	This::wxNavigationKeyEvent().
getCurrentFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNavigationKeyEvent_GetCurrentFocus),
  wxe_util:rec(?wxNavigationKeyEvent_GetCurrentFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetcurrentfocus">external documentation</a>.
-spec setCurrentFocus(This, CurrentFocus) -> 'ok' when
	This::wxNavigationKeyEvent(), CurrentFocus::wxWindow:wxWindow().
setCurrentFocus(#wx_ref{type=ThisT}=This,#wx_ref{type=CurrentFocusT}=CurrentFocus) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  ?CLASS(CurrentFocusT,wxWindow),
  wxe_util:queue_cmd(This,CurrentFocus,?get_env(),?wxNavigationKeyEvent_SetCurrentFocus).

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
