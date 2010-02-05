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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html">wxMouseEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>left_down</em>, <em>left_up</em>, <em>middle_down</em>, <em>middle_up</em>, <em>right_down</em>, <em>right_up</em>, <em>motion</em>, <em>enter_window</em>, <em>leave_window</em>, <em>left_dclick</em>, <em>middle_dclick</em>, <em>right_dclick</em>, <em>mousewheel</em>, <em>nc_left_down</em>, <em>nc_left_up</em>, <em>nc_middle_down</em>, <em>nc_middle_up</em>, <em>nc_right_down</em>, <em>nc_right_up</em>, <em>nc_motion</em>, <em>nc_enter_window</em>, <em>nc_leave_window</em>, <em>nc_left_dclick</em>, <em>nc_middle_dclick</em>, <em>nc_right_dclick</em></dd></dl>
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
  getWheelDelta/1,getWheelRotation/1,getX/1,getY/1,isButton/1,isPageScroll/1,
  leaving/1,leftDClick/1,leftDown/1,leftIsDown/1,leftUp/1,metaDown/1,
  middleDClick/1,middleDown/1,middleIsDown/1,middleUp/1,moving/1,rightDClick/1,
  rightDown/1,rightIsDown/1,rightUp/1,shiftDown/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventaltdown">external documentation</a>.
altDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_AltDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent(), But::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventbutton">external documentation</a>.
button(#wx_ref{type=ThisT,ref=ThisRef},But)
 when is_integer(But) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Button,
  <<ThisRef:32/?UI,But:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @equiv buttonDClick(This, [])
buttonDClick(This)
 when is_record(This, wx_ref) ->
  buttonDClick(This, []).

%% @spec (This::wxMouseEvent(), [Option]) -> bool()
%% Option = {but, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventbuttondclick">external documentation</a>.
buttonDClick(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonDClick,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @equiv buttonDown(This, [])
buttonDown(This)
 when is_record(This, wx_ref) ->
  buttonDown(This, []).

%% @spec (This::wxMouseEvent(), [Option]) -> bool()
%% Option = {but, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventbuttondown">external documentation</a>.
buttonDown(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonDown,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @equiv buttonUp(This, [])
buttonUp(This)
 when is_record(This, wx_ref) ->
  buttonUp(This, []).

%% @spec (This::wxMouseEvent(), [Option]) -> bool()
%% Option = {but, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventbuttonup">external documentation</a>.
buttonUp(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMouseEvent),
  MOpts = fun({but, But}, Acc) -> [<<1:32/?UI,But:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMouseEvent_ButtonUp,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventcmddown">external documentation</a>.
cmdDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_CmdDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventcontroldown">external documentation</a>.
controlDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_ControlDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventdragging">external documentation</a>.
dragging(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Dragging,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseevententering">external documentation</a>.
entering(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Entering,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetbutton">external documentation</a>.
getButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent(), Dc::wxDC:wxDC()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetlogicalposition">external documentation</a>.
getLogicalPosition(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxMouseEvent_GetLogicalPosition,
  <<ThisRef:32/?UI,DcRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetlinesperaction">external documentation</a>.
getLinesPerAction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetLinesPerAction,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetwheelrotation">external documentation</a>.
getWheelRotation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetWheelRotation,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetwheeldelta">external documentation</a>.
getWheelDelta(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetWheelDelta,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgetx">external documentation</a>.
getX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventgety">external documentation</a>.
getY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_GetY,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventisbutton">external documentation</a>.
isButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_IsButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventispagescroll">external documentation</a>.
isPageScroll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_IsPageScroll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventleaving">external documentation</a>.
leaving(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Leaving,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventleftdclick">external documentation</a>.
leftDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftDClick,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventleftdown">external documentation</a>.
leftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventleftisdown">external documentation</a>.
leftIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftIsDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventleftup">external documentation</a>.
leftUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_LeftUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmetadown">external documentation</a>.
metaDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MetaDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmiddledclick">external documentation</a>.
middleDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleDClick,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmiddledown">external documentation</a>.
middleDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmiddleisdown">external documentation</a>.
middleIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleIsDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmiddleup">external documentation</a>.
middleUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_MiddleUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventmoving">external documentation</a>.
moving(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_Moving,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventrightdclick">external documentation</a>.
rightDClick(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightDClick,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventrightdown">external documentation</a>.
rightDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventrightisdown">external documentation</a>.
rightIsDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightIsDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventrightup">external documentation</a>.
rightUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_RightUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMouseEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmouseevent.html#wxmouseeventshiftdown">external documentation</a>.
shiftDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMouseEvent),
  wxe_util:call(?wxMouseEvent_ShiftDown,
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
