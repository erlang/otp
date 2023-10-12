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

-module(wxSetCursorEvent).
-include("wxe.hrl").
-export([getCursor/1,getX/1,getY/1,hasCursor/1,setCursor/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxSetCursorEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxSetCursorEventType() :: 'set_cursor'.
-export_type([wxSetCursorEvent/0, wxSetCursor/0, wxSetCursorEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgetcursor">external documentation</a>.
-spec getCursor(This) -> wxCursor:wxCursor() when
	This::wxSetCursorEvent().
getCursor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSetCursorEvent_GetCursor),
  wxe_util:rec(?wxSetCursorEvent_GetCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxSetCursorEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSetCursorEvent_GetX),
  wxe_util:rec(?wxSetCursorEvent_GetX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxSetCursorEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSetCursorEvent_GetY),
  wxe_util:rec(?wxSetCursorEvent_GetY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventhascursor">external documentation</a>.
-spec hasCursor(This) -> boolean() when
	This::wxSetCursorEvent().
hasCursor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSetCursorEvent_HasCursor),
  wxe_util:rec(?wxSetCursorEvent_HasCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsetcursorevent.html#wxsetcursoreventsetcursor">external documentation</a>.
-spec setCursor(This, Cursor) -> 'ok' when
	This::wxSetCursorEvent(), Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=ThisT}=This,#wx_ref{type=CursorT}=Cursor) ->
  ?CLASS(ThisT,wxSetCursorEvent),
  ?CLASS(CursorT,wxCursor),
  wxe_util:queue_cmd(This,Cursor,?get_env(),?wxSetCursorEvent_SetCursor).

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
