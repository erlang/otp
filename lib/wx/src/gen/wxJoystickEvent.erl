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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html">wxJoystickEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>joy_button_down</em>, <em>joy_button_up</em>, <em>joy_move</em>, <em>joy_zmove</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxJoystick(). #wxJoystick{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxJoystickEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxJoystickEvent).
-include("wxe.hrl").
-export([buttonDown/1,buttonDown/2,buttonIsDown/1,buttonIsDown/2,buttonUp/1,
  buttonUp/2,getButtonChange/1,getButtonState/1,getJoystick/1,getPosition/1,
  getZPosition/1,isButton/1,isMove/1,isZMove/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxJoystickEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxJoystickEvent() :: wx:wx_object().
%% @equiv buttonDown(This, [])
-spec buttonDown(This) -> boolean() when
	This::wxJoystickEvent().

buttonDown(This)
 when is_record(This, wx_ref) ->
  buttonDown(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventbuttondown">external documentation</a>.
-spec buttonDown(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonDown(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxJoystickEvent_ButtonDown,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv buttonIsDown(This, [])
-spec buttonIsDown(This) -> boolean() when
	This::wxJoystickEvent().

buttonIsDown(This)
 when is_record(This, wx_ref) ->
  buttonIsDown(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventbuttonisdown">external documentation</a>.
-spec buttonIsDown(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonIsDown(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxJoystickEvent_ButtonIsDown,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv buttonUp(This, [])
-spec buttonUp(This) -> boolean() when
	This::wxJoystickEvent().

buttonUp(This)
 when is_record(This, wx_ref) ->
  buttonUp(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventbuttonup">external documentation</a>.
-spec buttonUp(This, [Option]) -> boolean() when
	This::wxJoystickEvent(),
	Option :: {'but', integer()}.
buttonUp(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxJoystickEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxJoystickEvent_ButtonUp,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventgetbuttonchange">external documentation</a>.
-spec getButtonChange(This) -> integer() when
	This::wxJoystickEvent().
getButtonChange(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_GetButtonChange,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventgetbuttonstate">external documentation</a>.
-spec getButtonState(This) -> integer() when
	This::wxJoystickEvent().
getButtonState(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_GetButtonState,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventgetjoystick">external documentation</a>.
-spec getJoystick(This) -> integer() when
	This::wxJoystickEvent().
getJoystick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_GetJoystick,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxJoystickEvent().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventgetzposition">external documentation</a>.
-spec getZPosition(This) -> integer() when
	This::wxJoystickEvent().
getZPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_GetZPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventisbutton">external documentation</a>.
-spec isButton(This) -> boolean() when
	This::wxJoystickEvent().
isButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_IsButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventismove">external documentation</a>.
-spec isMove(This) -> boolean() when
	This::wxJoystickEvent().
isMove(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_IsMove,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxjoystickevent.html#wxjoystickeventiszmove">external documentation</a>.
-spec isZMove(This) -> boolean() when
	This::wxJoystickEvent().
isZMove(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxJoystickEvent),
  wxe_util:call(?wxJoystickEvent_IsZMove,
  <<ThisRef:32/?UI>>).

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
