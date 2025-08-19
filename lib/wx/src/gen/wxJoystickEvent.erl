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

-module(wxJoystickEvent).
-moduledoc """
This event class contains information about joystick events, particularly events received
by windows.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxJoystickEvent](https://docs.wxwidgets.org/3.2/classwx_joystick_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxJoystickEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([buttonDown/1,buttonDown/2,buttonIsDown/1,buttonIsDown/2,buttonUp/1,
  buttonUp/2,getButtonChange/1,getButtonState/1,getJoystick/1,getPosition/1,
  getZPosition/1,isButton/1,isMove/1,isZMove/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxJoystickEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxJoystickEventType() :: 'joy_button_down' | 'joy_button_up' | 'joy_move' | 'joy_zmove'.
-export_type([wxJoystickEvent/0, wxJoystick/0, wxJoystickEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => buttonDown(This, [])}).
-spec buttonDown(This) -> boolean() when
	This::wxJoystickEvent().

buttonDown(This)
 when is_record(This, wx_ref) ->
  buttonDown(This, []).

-doc "Returns true if the event was a down event from the specified button (or any button).".
-spec buttonDown(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonDown(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxJoystickEvent_ButtonDown),
  wxe_util:rec(?wxJoystickEvent_ButtonDown).

-doc(#{equiv => buttonIsDown(This, [])}).
-spec buttonIsDown(This) -> boolean() when
	This::wxJoystickEvent().

buttonIsDown(This)
 when is_record(This, wx_ref) ->
  buttonIsDown(This, []).

-doc "Returns true if the specified button (or any button) was in a down state.".
-spec buttonIsDown(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonIsDown(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxJoystickEvent_ButtonIsDown),
  wxe_util:rec(?wxJoystickEvent_ButtonIsDown).

-doc(#{equiv => buttonUp(This, [])}).
-spec buttonUp(This) -> boolean() when
	This::wxJoystickEvent().

buttonUp(This)
 when is_record(This, wx_ref) ->
  buttonUp(This, []).

-doc "Returns true if the event was an up event from the specified button (or any button).".
-spec buttonUp(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonUp(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxJoystickEvent_ButtonUp),
  wxe_util:rec(?wxJoystickEvent_ButtonUp).

-doc """
Returns the identifier of the button changing state.

The return value is where `n` is the index of the button changing state, which can also
be retrieved using `GetButtonOrdinal()` (not implemented in wx).

Note that for `n` equal to 1, 2, 3 or 4 there are predefined `wxJOY_BUTTONn` constants
which can be used for more clarity, however these constants are not defined for the
buttons beyond the first four.
""".
-spec getButtonChange(This) -> integer() when
	This::wxJoystickEvent().
getButtonChange(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_GetButtonChange),
  wxe_util:rec(?wxJoystickEvent_GetButtonChange).

-doc """
Returns the down state of the buttons.

This is a `wxJOY_BUTTONn` identifier, where `n` is one of 1, 2, 3, 4.
""".
-spec getButtonState(This) -> integer() when
	This::wxJoystickEvent().
getButtonState(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_GetButtonState),
  wxe_util:rec(?wxJoystickEvent_GetButtonState).

-doc """
Returns the identifier of the joystick generating the event - one of wxJOYSTICK1 and
wxJOYSTICK2.
""".
-spec getJoystick(This) -> integer() when
	This::wxJoystickEvent().
getJoystick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_GetJoystick),
  wxe_util:rec(?wxJoystickEvent_GetJoystick).

-doc """
Returns the x, y position of the joystick event.

These coordinates are valid for all the events except wxEVT_JOY_ZMOVE.
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxJoystickEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_GetPosition),
  wxe_util:rec(?wxJoystickEvent_GetPosition).

-doc """
Returns the z position of the joystick event.

This method can only be used for wxEVT_JOY_ZMOVE events.
""".
-spec getZPosition(This) -> integer() when
	This::wxJoystickEvent().
getZPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_GetZPosition),
  wxe_util:rec(?wxJoystickEvent_GetZPosition).

-doc "Returns true if this was a button up or down event (`not` 'is any button down?').".
-spec isButton(This) -> boolean() when
	This::wxJoystickEvent().
isButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_IsButton),
  wxe_util:rec(?wxJoystickEvent_IsButton).

-doc "Returns true if this was an x, y move event.".
-spec isMove(This) -> boolean() when
	This::wxJoystickEvent().
isMove(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_IsMove),
  wxe_util:rec(?wxJoystickEvent_IsMove).

-doc "Returns true if this was a z move event.".
-spec isZMove(This) -> boolean() when
	This::wxJoystickEvent().
isZMove(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxJoystickEvent_IsZMove),
  wxe_util:rec(?wxJoystickEvent_IsZMove).

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
