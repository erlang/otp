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

-type wxMouseEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxMouseEventType() :: 'left_down' | 'left_up' | 'middle_down' | 'middle_up' | 'right_down' | 'right_up' | 'motion' | 'enter_window' | 'leave_window' | 'left_dclick' | 'middle_dclick' | 'right_dclick' | 'mousewheel' | 'aux1_down' | 'aux1_up' | 'aux1_dclick' | 'aux2_down' | 'aux2_up' | 'aux2_dclick'.
-export_type([wxMouseEvent/0, wxMouse/0, wxMouseEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventaltdown">external documentation</a>.
-spec altDown(This) -> boolean() when
	This::wxMouseEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_AltDown),
  wxe_util:rec(?wxMouseEvent_AltDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbutton">external documentation</a>.
%%<br /> But = ?wxMOUSE_BTN_ANY | ?wxMOUSE_BTN_NONE | ?wxMOUSE_BTN_LEFT | ?wxMOUSE_BTN_MIDDLE | ?wxMOUSE_BTN_RIGHT | ?wxMOUSE_BTN_AUX1 | ?wxMOUSE_BTN_AUX2 | ?wxMOUSE_BTN_MAX
-spec button(This, But) -> boolean() when
	This::wxMouseEvent(), But::wx:wx_enum().
button(#wx_ref{type=ThisT}=This,But)
 when is_integer(But) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,But,?get_env(),?wxMouseEvent_Button),
  wxe_util:rec(?wxMouseEvent_Button).

%% @equiv buttonDClick(This, [])
-spec buttonDClick(This) -> boolean() when
	This::wxMouseEvent().

buttonDClick(This)
 when is_record(This, wx_ref) ->
  buttonDClick(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttondclick">external documentation</a>.
%%<br /> But = ?wxMOUSE_BTN_ANY | ?wxMOUSE_BTN_NONE | ?wxMOUSE_BTN_LEFT | ?wxMOUSE_BTN_MIDDLE | ?wxMOUSE_BTN_RIGHT | ?wxMOUSE_BTN_AUX1 | ?wxMOUSE_BTN_AUX2 | ?wxMOUSE_BTN_MAX
-spec buttonDClick(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', wx:wx_enum()}.
buttonDClick(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMouseEvent_ButtonDClick),
  wxe_util:rec(?wxMouseEvent_ButtonDClick).

%% @equiv buttonDown(This, [])
-spec buttonDown(This) -> boolean() when
	This::wxMouseEvent().

buttonDown(This)
 when is_record(This, wx_ref) ->
  buttonDown(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttondown">external documentation</a>.
%%<br /> But = ?wxMOUSE_BTN_ANY | ?wxMOUSE_BTN_NONE | ?wxMOUSE_BTN_LEFT | ?wxMOUSE_BTN_MIDDLE | ?wxMOUSE_BTN_RIGHT | ?wxMOUSE_BTN_AUX1 | ?wxMOUSE_BTN_AUX2 | ?wxMOUSE_BTN_MAX
-spec buttonDown(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', wx:wx_enum()}.
buttonDown(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMouseEvent_ButtonDown),
  wxe_util:rec(?wxMouseEvent_ButtonDown).

%% @equiv buttonUp(This, [])
-spec buttonUp(This) -> boolean() when
	This::wxMouseEvent().

buttonUp(This)
 when is_record(This, wx_ref) ->
  buttonUp(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventbuttonup">external documentation</a>.
%%<br /> But = ?wxMOUSE_BTN_ANY | ?wxMOUSE_BTN_NONE | ?wxMOUSE_BTN_LEFT | ?wxMOUSE_BTN_MIDDLE | ?wxMOUSE_BTN_RIGHT | ?wxMOUSE_BTN_AUX1 | ?wxMOUSE_BTN_AUX2 | ?wxMOUSE_BTN_MAX
-spec buttonUp(This, [Option]) -> boolean() when
	This::wxMouseEvent(),
	Option :: {'but', wx:wx_enum()}.
buttonUp(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, _but} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMouseEvent_ButtonUp),
  wxe_util:rec(?wxMouseEvent_ButtonUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventcmddown">external documentation</a>.
-spec cmdDown(This) -> boolean() when
	This::wxMouseEvent().
cmdDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_CmdDown),
  wxe_util:rec(?wxMouseEvent_CmdDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventcontroldown">external documentation</a>.
-spec controlDown(This) -> boolean() when
	This::wxMouseEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_ControlDown),
  wxe_util:rec(?wxMouseEvent_ControlDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventdragging">external documentation</a>.
-spec dragging(This) -> boolean() when
	This::wxMouseEvent().
dragging(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_Dragging),
  wxe_util:rec(?wxMouseEvent_Dragging).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseevententering">external documentation</a>.
-spec entering(This) -> boolean() when
	This::wxMouseEvent().
entering(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_Entering),
  wxe_util:rec(?wxMouseEvent_Entering).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetbutton">external documentation</a>.
-spec getButton(This) -> integer() when
	This::wxMouseEvent().
getButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetButton),
  wxe_util:rec(?wxMouseEvent_GetButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxMouseEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetPosition),
  wxe_util:rec(?wxMouseEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetlogicalposition">external documentation</a>.
-spec getLogicalPosition(This, Dc) -> {X::integer(), Y::integer()} when
	This::wxMouseEvent(), Dc::wxDC:wxDC().
getLogicalPosition(#wx_ref{type=ThisT}=This,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxMouseEvent),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Dc,?get_env(),?wxMouseEvent_GetLogicalPosition),
  wxe_util:rec(?wxMouseEvent_GetLogicalPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetlinesperaction">external documentation</a>.
-spec getLinesPerAction(This) -> integer() when
	This::wxMouseEvent().
getLinesPerAction(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetLinesPerAction),
  wxe_util:rec(?wxMouseEvent_GetLinesPerAction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheelrotation">external documentation</a>.
-spec getWheelRotation(This) -> integer() when
	This::wxMouseEvent().
getWheelRotation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetWheelRotation),
  wxe_util:rec(?wxMouseEvent_GetWheelRotation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheeldelta">external documentation</a>.
-spec getWheelDelta(This) -> integer() when
	This::wxMouseEvent().
getWheelDelta(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetWheelDelta),
  wxe_util:rec(?wxMouseEvent_GetWheelDelta).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetx">external documentation</a>.
-spec getX(This) -> integer() when
	This::wxMouseEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetX),
  wxe_util:rec(?wxMouseEvent_GetX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgety">external documentation</a>.
-spec getY(This) -> integer() when
	This::wxMouseEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetY),
  wxe_util:rec(?wxMouseEvent_GetY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventisbutton">external documentation</a>.
-spec isButton(This) -> boolean() when
	This::wxMouseEvent().
isButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_IsButton),
  wxe_util:rec(?wxMouseEvent_IsButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventispagescroll">external documentation</a>.
-spec isPageScroll(This) -> boolean() when
	This::wxMouseEvent().
isPageScroll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_IsPageScroll),
  wxe_util:rec(?wxMouseEvent_IsPageScroll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleaving">external documentation</a>.
-spec leaving(This) -> boolean() when
	This::wxMouseEvent().
leaving(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_Leaving),
  wxe_util:rec(?wxMouseEvent_Leaving).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftdclick">external documentation</a>.
-spec leftDClick(This) -> boolean() when
	This::wxMouseEvent().
leftDClick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_LeftDClick),
  wxe_util:rec(?wxMouseEvent_LeftDClick).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftdown">external documentation</a>.
-spec leftDown(This) -> boolean() when
	This::wxMouseEvent().
leftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_LeftDown),
  wxe_util:rec(?wxMouseEvent_LeftDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftisdown">external documentation</a>.
-spec leftIsDown(This) -> boolean() when
	This::wxMouseEvent().
leftIsDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_LeftIsDown),
  wxe_util:rec(?wxMouseEvent_LeftIsDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventleftup">external documentation</a>.
-spec leftUp(This) -> boolean() when
	This::wxMouseEvent().
leftUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_LeftUp),
  wxe_util:rec(?wxMouseEvent_LeftUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmetadown">external documentation</a>.
-spec metaDown(This) -> boolean() when
	This::wxMouseEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_MetaDown),
  wxe_util:rec(?wxMouseEvent_MetaDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddledclick">external documentation</a>.
-spec middleDClick(This) -> boolean() when
	This::wxMouseEvent().
middleDClick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_MiddleDClick),
  wxe_util:rec(?wxMouseEvent_MiddleDClick).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddledown">external documentation</a>.
-spec middleDown(This) -> boolean() when
	This::wxMouseEvent().
middleDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_MiddleDown),
  wxe_util:rec(?wxMouseEvent_MiddleDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddleisdown">external documentation</a>.
-spec middleIsDown(This) -> boolean() when
	This::wxMouseEvent().
middleIsDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_MiddleIsDown),
  wxe_util:rec(?wxMouseEvent_MiddleIsDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmiddleup">external documentation</a>.
-spec middleUp(This) -> boolean() when
	This::wxMouseEvent().
middleUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_MiddleUp),
  wxe_util:rec(?wxMouseEvent_MiddleUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventmoving">external documentation</a>.
-spec moving(This) -> boolean() when
	This::wxMouseEvent().
moving(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_Moving),
  wxe_util:rec(?wxMouseEvent_Moving).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightdclick">external documentation</a>.
-spec rightDClick(This) -> boolean() when
	This::wxMouseEvent().
rightDClick(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_RightDClick),
  wxe_util:rec(?wxMouseEvent_RightDClick).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightdown">external documentation</a>.
-spec rightDown(This) -> boolean() when
	This::wxMouseEvent().
rightDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_RightDown),
  wxe_util:rec(?wxMouseEvent_RightDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightisdown">external documentation</a>.
-spec rightIsDown(This) -> boolean() when
	This::wxMouseEvent().
rightIsDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_RightIsDown),
  wxe_util:rec(?wxMouseEvent_RightIsDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventrightup">external documentation</a>.
-spec rightUp(This) -> boolean() when
	This::wxMouseEvent().
rightUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_RightUp),
  wxe_util:rec(?wxMouseEvent_RightUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventshiftdown">external documentation</a>.
-spec shiftDown(This) -> boolean() when
	This::wxMouseEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_ShiftDown),
  wxe_util:rec(?wxMouseEvent_ShiftDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmouseevent.html#wxmouseeventgetwheelaxis">external documentation</a>.
%%<br /> Res = ?wxMOUSE_WHEEL_VERTICAL | ?wxMOUSE_WHEEL_HORIZONTAL
-spec getWheelAxis(This) -> wx:wx_enum() when
	This::wxMouseEvent().
getWheelAxis(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxMouseEvent_GetWheelAxis),
  wxe_util:rec(?wxMouseEvent_GetWheelAxis).

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
