%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html">wxNavigationKeyEvent</a>.
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

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxNavigationKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetdirection">external documentation</a>.
getDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_GetDirection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxNavigationKeyEvent(), BForward::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetdirection">external documentation</a>.
setDirection(#wx_ref{type=ThisT,ref=ThisRef},BForward)
 when is_boolean(BForward) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetDirection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BForward)):32/?UI>>).

%% @spec (This::wxNavigationKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventiswindowchange">external documentation</a>.
isWindowChange(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_IsWindowChange,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxNavigationKeyEvent(), BIs::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetwindowchange">external documentation</a>.
setWindowChange(#wx_ref{type=ThisT,ref=ThisRef},BIs)
 when is_boolean(BIs) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetWindowChange,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BIs)):32/?UI>>).

%% @spec (This::wxNavigationKeyEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventisfromtab">external documentation</a>.
isFromTab(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_IsFromTab,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxNavigationKeyEvent(), BIs::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetfromtab">external documentation</a>.
setFromTab(#wx_ref{type=ThisT,ref=ThisRef},BIs)
 when is_boolean(BIs) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:cast(?wxNavigationKeyEvent_SetFromTab,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BIs)):32/?UI>>).

%% @spec (This::wxNavigationKeyEvent()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventgetcurrentfocus">external documentation</a>.
getCurrentFocus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxNavigationKeyEvent),
  wxe_util:call(?wxNavigationKeyEvent_GetCurrentFocus,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxNavigationKeyEvent(), Win::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxnavigationkeyevent.html#wxnavigationkeyeventsetcurrentfocus">external documentation</a>.
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
