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

-module(wxHelpEvent).
-moduledoc """
A help event is sent when the user has requested context-sensitive help.

This can either be caused by the application requesting context-sensitive help mode via `wxContextHelp`
(not implemented in wx), or (on MS Windows) by the system generating a WM_HELP message
when the user pressed F1 or clicked on the query button in a dialog caption.

A help event is sent to the window that the user clicked on, and is propagated up the
window hierarchy until the event is processed or there are no more event handlers.

The application should call `wxEvent:getId/1` to check the identity of the clicked-on window, and then
either show some suitable help or call `wxEvent:skip/2` if the identifier is unrecognised.

Calling Skip is important because it allows wxWidgets to generate further events for
ancestors of the clicked-on window. Otherwise it would be impossible to show help for
container windows, since processing would stop after the first window found.

See:
* `m:wxDialog`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxHelpEvent](https://docs.wxwidgets.org/3.2/classwx_help_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxHelpEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getOrigin/1,getPosition/1,setOrigin/2,setPosition/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxHelpEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxHelpEventType() :: 'help' | 'detailed_help'.
-export_type([wxHelpEvent/0, wxHelp/0, wxHelpEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the origin of the help event which is one of the `wxHelpEvent::Origin` (not
implemented in wx) values.

The application may handle events generated using the keyboard or mouse differently, e.g.
by using `wx_misc:getMousePosition/0` for the mouse events.

See: `setOrigin/2`
""".
%%  Res = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec getOrigin(This) -> wx:wx_enum() when
	This::wxHelpEvent().
getOrigin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxHelpEvent_GetOrigin),
  wxe_util:rec(?wxHelpEvent_GetOrigin).

-doc """
Returns the left-click position of the mouse, in screen coordinates.

This allows the application to position the help appropriately.
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxHelpEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxHelpEvent_GetPosition),
  wxe_util:rec(?wxHelpEvent_GetPosition).

-doc """
Set the help event origin, only used internally by wxWidgets normally.

See: `getOrigin/1`
""".
%%  Origin = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec setOrigin(This, Origin) -> 'ok' when
	This::wxHelpEvent(), Origin::wx:wx_enum().
setOrigin(#wx_ref{type=ThisT}=This,Origin)
 when is_integer(Origin) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,Origin,?get_env(),?wxHelpEvent_SetOrigin).

-doc "Sets the left-click position of the mouse, in screen coordinates.".
-spec setPosition(This, Pt) -> 'ok' when
	This::wxHelpEvent(), Pt::{X::integer(), Y::integer()}.
setPosition(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxHelpEvent_SetPosition).

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
