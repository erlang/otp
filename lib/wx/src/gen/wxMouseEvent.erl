%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html">wxMouseEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>left_down</em>, <em>left_up</em>, <em>middle_down</em>, <em>middle_up</em>, <em>right_down</em>, <em>right_up</em>, <em>motion</em>, <em>enter_window</em>, <em>leave_window</em>, <em>left_dclick</em>, <em>middle_dclick</em>, <em>right_dclick</em>, <em>mousewheel</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxMouse(). #wxMouse{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxMouseEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMouseEvent).
-include("wxe.hrl").
-export([altDown/1,button/2,buttonDClick/1,buttonDClick/2,buttonDown/1,buttonDown/2,
  buttonUp/1,buttonUp/2,cmdDown/1,controlDown/1,dragging/1,entering/1,
  getButton/1,getLinesPerAction/1,getLogicalPosition/2,getPosition/1,
  getWheelAxis/1,getWheelDelta/1,getWheelRotation/1,getX/1,getY/1,isButton/1,
  isPageScroll/1,leaving/1,leftDClick/1,leftDown/1,leftIsDown/1,leftUp/1,
  metaDown/1,middleDClick/1,middleDown/1,middleIsDown/1,middleUp/1,moving/1,
  rightDClick/1,rightDown/1,rightIsDown/1,rightUp/1,shiftDown/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxMouseEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxMouseEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxMouseEvent().
altDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_AltDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbutton">external documentation</a>.
-spec button(This, But) -> boolean() when
	This::wxMouseEvent(), But::integer().
button(#wx_ref{type=ThisT,ref=ThisRef},But)
 when is_integer(But) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Button,
  <<ThisRef:32/?UI,But:32/?UI>>).

%% @equiv buttonDClick(This, [])
-spec buttonDClick(This) -> boolean() when
	This::wxMouseEvent().

buttonDClick(This)
 when is_record(This, wx_ref) ->
  buttonDClick(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttondclick">external documentation</a>.
-spec buttonDClick(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', integer()}.
buttonDClick(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonDClick,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv buttonDown(This, [])
-spec buttonDown(This) -> boolean() when
	This::wxMouseEvent().

buttonDown(This)
 when is_record(This, wx_ref) ->
  buttonDown(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttondown">external documentation</a>.
-spec buttonDown(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', integer()}.
buttonDown(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonDown,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv buttonUp(This, [])
-spec buttonUp(This) -> boolean() when
	This::wxMouseEvent().

buttonUp(This)
 when is_record(This, wx_ref) ->
  buttonUp(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttonup">external documentation</a>.
-spec buttonUp(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', integer()}.
buttonUp(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonUp,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventcmddown">external documentation</a>.
-spec cmdDown(This) -> boolean() when
	This::wxMouseEvent().
cmdDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_CmdDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxMouseEvent().
controlDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_ControlDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventdragging">external documentation</a>.
-spec dragging(This) -> boolean() when
	This::wxMouseEvent().
dragging(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Dragging,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseevententering">external documentation</a>.
-spec entering(This) -> boolean() when
	This::wxMouseEvent().
entering(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Entering,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetbutton">external documentation</a>.
-spec getButton(This) -> integer() when
	This::wxMouseEvent().
getButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxMouseEvent().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetlogicalposition">external documentation</a>.
-spec getLogicalPosition(This, Dc) -> {X::integer(), Y::integer()} when
	This::wxMouseEvent(), Dc::wxDC:wxDC().
getLogicalPosition(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxMouseEvent_GetLogicalPosition,
  <<ThisRef:32/?UI,DcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetlinesperaction">external documentation</a>.
-spec getLinesPerAction(This) -> integer() when
	This::wxMouseEvent().
getLinesPerAction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetLinesPerAction,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheelrotation">external documentation</a>.
-spec getWheelRotation(This) -> integer() when
	This::wxMouseEvent().
getWheelRotation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetWheelRotation,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheeldelta">external documentation</a>.
-spec getWheelDelta(This) -> integer() when
	This::wxMouseEvent().
getWheelDelta(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetWheelDelta,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxMouseEvent().
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxMouseEvent().
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventisbutton">external documentation</a>.
-spec isButton(This) -> boolean() when
	This::wxMouseEvent().
isButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_IsButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventispagescroll">external documentation</a>.
-spec isPageScroll(This) -> boolean() when
	This::wxMouseEvent().
isPageScroll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_IsPageScroll,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleaving">external documentation</a>.
-spec leaving(This) -> boolean() when
	This::wxMouseEvent().
leaving(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Leaving,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftdclick">external documentation</a>.
-spec leftDClick(This) -> boolean() when
	This::wxMouseEvent().
leftDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftDClick,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftdown">external documentation</a>.
-spec leftDown(This) -> boolean() when
	This::wxMouseEvent().
leftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftisdown">external documentation</a>.
-spec leftIsDown(This) -> boolean() when
	This::wxMouseEvent().
leftIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftIsDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftup">external documentation</a>.
-spec leftUp(This) -> boolean() when
	This::wxMouseEvent().
leftUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftUp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxMouseEvent().
metaDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MetaDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddledclick">external documentation</a>.
-spec middleDClick(This) -> boolean() when
	This::wxMouseEvent().
middleDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleDClick,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddledown">external documentation</a>.
-spec middleDown(This) -> boolean() when
	This::wxMouseEvent().
middleDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddleisdown">external documentation</a>.
-spec middleIsDown(This) -> boolean() when
	This::wxMouseEvent().
middleIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleIsDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddleup">external documentation</a>.
-spec middleUp(This) -> boolean() when
	This::wxMouseEvent().
middleUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleUp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmoving">external documentation</a>.
-spec moving(This) -> boolean() when
	This::wxMouseEvent().
moving(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Moving,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightdclick">external documentation</a>.
-spec rightDClick(This) -> boolean() when
	This::wxMouseEvent().
rightDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightDClick,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightdown">external documentation</a>.
-spec rightDown(This) -> boolean() when
	This::wxMouseEvent().
rightDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightisdown">external documentation</a>.
-spec rightIsDown(This) -> boolean() when
	This::wxMouseEvent().
rightIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightIsDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightup">external documentation</a>.
-spec rightUp(This) -> boolean() when
	This::wxMouseEvent().
rightUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightUp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxMouseEvent().
shiftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_ShiftDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheelaxis">external documentation</a>.
%%<br /> Res = ?wxMOUSE_WHEEL_VERTICAL | ?wxMOUSE_WHEEL_HORIZONTAL
-spec getWheelAxis(This) -> wx:wx_enum() when
	This::wxMouseEvent().
getWheelAxis(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetWheelAxis,
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
