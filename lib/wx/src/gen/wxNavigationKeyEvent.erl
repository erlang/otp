%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html">wxNavigationKeyEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>navigation_key</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxNavigationKey(). #wxNavigationKey{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxNavigationKeyEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxNavigationKeyEvent).
-include("wxe.hrl").
-export([getCurrentFocus/1,getDirection/1,isFromTab/1,isWindowChange/1,setCurrentFocus/2,
  setDirection/2,setFromTab/2,setWindowChange/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxNavigationKeyEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxNavigationKeyEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetdirection">external documentation</a>.
-spec getDirection(This) -> boolean() when
	This::wxNavigationKeyEvent().
getDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_GetDirection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetdirection">external documentation</a>.
-spec setDirection(This, BForward) -> 'ok' when
	This::wxNavigationKeyEvent(), BForward::boolean().
setDirection(#wx_ref{type=ThisT,ref=ThisRef},BForward)
 when is_boolean(BForward) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetDirection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BForward)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventiswindowchange">external documentation</a>.
-spec isWindowChange(This) -> boolean() when
	This::wxNavigationKeyEvent().
isWindowChange(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_IsWindowChange,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetwindowchange">external documentation</a>.
-spec setWindowChange(This, BIs) -> 'ok' when
	This::wxNavigationKeyEvent(), BIs::boolean().
setWindowChange(#wx_ref{type=ThisT,ref=ThisRef},BIs)
 when is_boolean(BIs) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetWindowChange,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BIs)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventisfromtab">external documentation</a>.
-spec isFromTab(This) -> boolean() when
	This::wxNavigationKeyEvent().
isFromTab(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_IsFromTab,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetfromtab">external documentation</a>.
-spec setFromTab(This, BIs) -> 'ok' when
	This::wxNavigationKeyEvent(), BIs::boolean().
setFromTab(#wx_ref{type=ThisT,ref=ThisRef},BIs)
 when is_boolean(BIs) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetFromTab,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BIs)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetcurrentfocus">external documentation</a>.
-spec getCurrentFocus(This) -> wxWindow:wxWindow() when
	This::wxNavigationKeyEvent().
getCurrentFocus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_GetCurrentFocus,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetcurrentfocus">external documentation</a>.
-spec setCurrentFocus(This, Win) -> 'ok' when
	This::wxNavigationKeyEvent(), Win::wxWindow:wxWindow().
setCurrentFocus(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxNavigationKeyEvent_SetCurrentFocus,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

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
