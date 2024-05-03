%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxAuiPaneInfo).
-moduledoc """
Functions for wxAuiPaneInfo class

`m:wxAuiPaneInfo` is part of the wxAUI class framework. See also overview_aui.

`m:wxAuiPaneInfo` specifies all the parameters for a pane. These parameters
specify where the pane is on the screen, whether it is docked or floating, or
hidden. In addition, these parameters specify the pane's docked position,
floating position, preferred size, minimum size, caption text among many other
parameters.

See: `m:wxAuiManager`, `m:wxAuiDockArt`

wxWidgets docs:
[wxAuiPaneInfo](https://docs.wxwidgets.org/3.1/classwx_aui_pane_info.html)
""".
-include("wxe.hrl").
-export([bestSize/2,bestSize/3,bottom/1,bottomDockable/1,bottomDockable/2,caption/2,
  captionVisible/1,captionVisible/2,centre/1,centrePane/1,closeButton/1,
  closeButton/2,defaultPane/1,destroy/1,destroyOnClose/1,destroyOnClose/2,
  direction/2,dock/1,dockable/1,dockable/2,fixed/1,float/1,floatable/1,
  floatable/2,floatingPosition/2,floatingPosition/3,floatingSize/2,
  floatingSize/3,getDirection/1,getFloatingPosition/1,getFloatingSize/1,
  getFrame/1,getLayer/1,getPosition/1,getRow/1,getWindow/1,gripper/1,
  gripper/2,gripperTop/1,gripperTop/2,hasBorder/1,hasCaption/1,hasCloseButton/1,
  hasFlag/2,hasGripper/1,hasGripperTop/1,hasMaximizeButton/1,hasMinimizeButton/1,
  hasPinButton/1,hide/1,isBottomDockable/1,isDocked/1,isFixed/1,isFloatable/1,
  isFloating/1,isLeftDockable/1,isMovable/1,isOk/1,isResizable/1,isRightDockable/1,
  isShown/1,isToolbar/1,isTopDockable/1,layer/2,left/1,leftDockable/1,
  leftDockable/2,maxSize/2,maxSize/3,maximizeButton/1,maximizeButton/2,
  minSize/2,minSize/3,minimizeButton/1,minimizeButton/2,movable/1,movable/2,
  name/2,new/0,new/1,paneBorder/1,paneBorder/2,pinButton/1,pinButton/2,
  position/2,resizable/1,resizable/2,right/1,rightDockable/1,rightDockable/2,
  row/2,safeSet/2,setFlag/3,show/1,show/2,toolbarPane/1,top/1,topDockable/1,
  topDockable/2,window/2]).

%% inherited exports
-export([parent_class/1]).

-type wxAuiPaneInfo() :: wx:wx_object().
-export_type([wxAuiPaneInfo/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
-spec new() -> wxAuiPaneInfo().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAuiPaneInfo_new_0),
  wxe_util:rec(?wxAuiPaneInfo_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
-doc "Copy constructor.".
-spec new(C) -> wxAuiPaneInfo() when
	C::wxAuiPaneInfo().
new(#wx_ref{type=CT}=C) ->
  ?CLASS(CT,wxAuiPaneInfo),
  wxe_util:queue_cmd(C,?get_env(),?wxAuiPaneInfo_new_1),
  wxe_util:rec(?wxAuiPaneInfo_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
-doc """
`bestSize/3` sets the ideal size for the pane.

The docking manager will attempt to use this size as much as possible when
docking or floating the pane.
""".
-spec bestSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
bestSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxAuiPaneInfo_BestSize_1),
  wxe_util:rec(?wxAuiPaneInfo_BestSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
-spec bestSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
bestSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxAuiPaneInfo_BestSize_2),
  wxe_util:rec(?wxAuiPaneInfo_BestSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobottom">external documentation</a>.
-doc """
`bottom/1` sets the pane dock position to the bottom side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_BOTTOM).
""".
-spec bottom(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
bottom(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Bottom),
  wxe_util:rec(?wxAuiPaneInfo_Bottom).

%% @equiv bottomDockable(This, [])
-spec bottomDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

bottomDockable(This)
 when is_record(This, wx_ref) ->
  bottomDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobottomdockable">external documentation</a>.
-doc """
`bottomDockable/2` indicates whether a pane can be docked at the bottom of the
frame.
""".
-spec bottomDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
bottomDockable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_BottomDockable),
  wxe_util:rec(?wxAuiPaneInfo_BottomDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocaption">external documentation</a>.
-doc "`caption/2` sets the caption of the pane.".
-spec caption(This, C) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), C::unicode:chardata().
caption(#wx_ref{type=ThisT}=This,C)
 when ?is_chardata(C) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  C_UC = unicode:characters_to_binary(C),
  wxe_util:queue_cmd(This,C_UC,?get_env(),?wxAuiPaneInfo_Caption),
  wxe_util:rec(?wxAuiPaneInfo_Caption).

%% @equiv captionVisible(This, [])
-spec captionVisible(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

captionVisible(This)
 when is_record(This, wx_ref) ->
  captionVisible(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocaptionvisible">external documentation</a>.
-doc """
CaptionVisible indicates that a pane caption should be visible.

If false, no pane caption is drawn.
""".
-spec captionVisible(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
captionVisible(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_CaptionVisible),
  wxe_util:rec(?wxAuiPaneInfo_CaptionVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocentre">external documentation</a>.
-doc """
`Center()` (not implemented in wx) sets the pane dock position to the left side
of the frame.

The centre pane is the space in the middle after all border panes (left, top,
right, bottom) are subtracted from the layout. This is the same thing as calling
Direction(wxAUI_DOCK_CENTRE).
""".
-spec centre(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
centre(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Centre),
  wxe_util:rec(?wxAuiPaneInfo_Centre).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocentrepane">external documentation</a>.
-doc """
`centrePane/1` specifies that the pane should adopt the default center pane
settings.

Centre panes usually do not have caption bars. This function provides an easy
way of preparing a pane to be displayed in the center dock position.
""".
-spec centrePane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
centrePane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_CentrePane),
  wxe_util:rec(?wxAuiPaneInfo_CentrePane).

%% @equiv closeButton(This, [])
-spec closeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

closeButton(This)
 when is_record(This, wx_ref) ->
  closeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoclosebutton">external documentation</a>.
-doc "`closeButton/2` indicates that a close button should be drawn for the pane.".
-spec closeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
closeButton(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_CloseButton),
  wxe_util:rec(?wxAuiPaneInfo_CloseButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodefaultpane">external documentation</a>.
-doc "`defaultPane/1` specifies that the pane should adopt the default pane settings.".
-spec defaultPane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
defaultPane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_DefaultPane),
  wxe_util:rec(?wxAuiPaneInfo_DefaultPane).

%% @equiv destroyOnClose(This, [])
-spec destroyOnClose(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

destroyOnClose(This)
 when is_record(This, wx_ref) ->
  destroyOnClose(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodestroyonclose">external documentation</a>.
-doc """
`destroyOnClose/2` indicates whether a pane should be destroyed when it is
closed.

Normally a pane is simply hidden when the close button is clicked. Setting
DestroyOnClose to true will cause the window to be destroyed when the user
clicks the pane's close button.
""".
-spec destroyOnClose(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
destroyOnClose(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_DestroyOnClose),
  wxe_util:rec(?wxAuiPaneInfo_DestroyOnClose).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodirection">external documentation</a>.
-doc """
`direction/2` determines the direction of the docked pane.

It is functionally the same as calling `left/1`, `right/1`, `top/1` or
`bottom/1`, except that docking direction may be specified programmatically via
the parameter.
""".
-spec direction(This, Direction) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Direction::integer().
direction(#wx_ref{type=ThisT}=This,Direction)
 when is_integer(Direction) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Direction,?get_env(),?wxAuiPaneInfo_Direction),
  wxe_util:rec(?wxAuiPaneInfo_Direction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodock">external documentation</a>.
-doc """
`dock/1` indicates that a pane should be docked.

It is the opposite of `float/1`.
""".
-spec dock(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
dock(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Dock),
  wxe_util:rec(?wxAuiPaneInfo_Dock).

%% @equiv dockable(This, [])
-spec dockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

dockable(This)
 when is_record(This, wx_ref) ->
  dockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodockable">external documentation</a>.
-doc """
`dockable/2` specifies whether a frame can be docked or not.

It is the same as specifying
TopDockable(b).BottomDockable(b).LeftDockable(b).RightDockable(b).
""".
-spec dockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
dockable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Dockable),
  wxe_util:rec(?wxAuiPaneInfo_Dockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofixed">external documentation</a>.
-doc """
`fixed/1` forces a pane to be fixed size so that it cannot be resized.

After calling `fixed/1`, `isFixed/1` will return true.
""".
-spec fixed(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
fixed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Fixed),
  wxe_util:rec(?wxAuiPaneInfo_Fixed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloat">external documentation</a>.
-doc """
`float/1` indicates that a pane should be floated.

It is the opposite of `dock/1`.
""".
-spec float(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
float(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Float),
  wxe_util:rec(?wxAuiPaneInfo_Float).

%% @equiv floatable(This, [])
-spec floatable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

floatable(This)
 when is_record(This, wx_ref) ->
  floatable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatable">external documentation</a>.
-doc """
`floatable/2` sets whether the user will be able to undock a pane and turn it
into a floating window.
""".
-spec floatable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
floatable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Floatable),
  wxe_util:rec(?wxAuiPaneInfo_Floatable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
-doc "`floatingPosition/3` sets the position of the floating pane.".
-spec floatingPosition(This, Pos) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Pos::{X::integer(), Y::integer()}.
floatingPosition(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxAuiPaneInfo_FloatingPosition_1),
  wxe_util:rec(?wxAuiPaneInfo_FloatingPosition_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
-spec floatingPosition(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
floatingPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxAuiPaneInfo_FloatingPosition_2),
  wxe_util:rec(?wxAuiPaneInfo_FloatingPosition_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
-doc "`floatingSize/3` sets the size of the floating pane.".
-spec floatingSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
floatingSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxAuiPaneInfo_FloatingSize_1),
  wxe_util:rec(?wxAuiPaneInfo_FloatingSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
-spec floatingSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
floatingSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxAuiPaneInfo_FloatingSize_2),
  wxe_util:rec(?wxAuiPaneInfo_FloatingSize_2).

%% @equiv gripper(This, [])
-spec gripper(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

gripper(This)
 when is_record(This, wx_ref) ->
  gripper(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogripper">external documentation</a>.
-doc "`gripper/2` indicates that a gripper should be drawn for the pane.".
-spec gripper(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
gripper(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Gripper),
  wxe_util:rec(?wxAuiPaneInfo_Gripper).

%% @equiv gripperTop(This, [])
-spec gripperTop(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

gripperTop(This)
 when is_record(This, wx_ref) ->
  gripperTop(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogrippertop">external documentation</a>.
-doc "`gripperTop/2` indicates that a gripper should be drawn at the top of the pane.".
-spec gripperTop(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'attop', boolean()}.
gripperTop(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({attop, _attop} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_GripperTop),
  wxe_util:rec(?wxAuiPaneInfo_GripperTop).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasborder">external documentation</a>.
-doc "`hasBorder/1` returns true if the pane displays a border.".
-spec hasBorder(This) -> boolean() when
	This::wxAuiPaneInfo().
hasBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasBorder),
  wxe_util:rec(?wxAuiPaneInfo_HasBorder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohascaption">external documentation</a>.
-doc "`hasCaption/1` returns true if the pane displays a caption.".
-spec hasCaption(This) -> boolean() when
	This::wxAuiPaneInfo().
hasCaption(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasCaption),
  wxe_util:rec(?wxAuiPaneInfo_HasCaption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasclosebutton">external documentation</a>.
-doc "`hasCloseButton/1` returns true if the pane displays a button to close the pane.".
-spec hasCloseButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasCloseButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasCloseButton),
  wxe_util:rec(?wxAuiPaneInfo_HasCloseButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasflag">external documentation</a>.
-doc """
`hasFlag/2` returns true if the property specified by flag is active for the
pane.
""".
-spec hasFlag(This, Flag) -> boolean() when
	This::wxAuiPaneInfo(), Flag::integer().
hasFlag(#wx_ref{type=ThisT}=This,Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxAuiPaneInfo_HasFlag),
  wxe_util:rec(?wxAuiPaneInfo_HasFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasgripper">external documentation</a>.
-doc "`hasGripper/1` returns true if the pane displays a gripper.".
-spec hasGripper(This) -> boolean() when
	This::wxAuiPaneInfo().
hasGripper(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasGripper),
  wxe_util:rec(?wxAuiPaneInfo_HasGripper).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasgrippertop">external documentation</a>.
-doc "`hasGripper/1` returns true if the pane displays a gripper at the top.".
-spec hasGripperTop(This) -> boolean() when
	This::wxAuiPaneInfo().
hasGripperTop(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasGripperTop),
  wxe_util:rec(?wxAuiPaneInfo_HasGripperTop).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasmaximizebutton">external documentation</a>.
-doc """
`hasMaximizeButton/1` returns true if the pane displays a button to maximize the
pane.
""".
-spec hasMaximizeButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasMaximizeButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasMaximizeButton),
  wxe_util:rec(?wxAuiPaneInfo_HasMaximizeButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasminimizebutton">external documentation</a>.
-doc """
`hasMinimizeButton/1` returns true if the pane displays a button to minimize the
pane.
""".
-spec hasMinimizeButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasMinimizeButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasMinimizeButton),
  wxe_util:rec(?wxAuiPaneInfo_HasMinimizeButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohaspinbutton">external documentation</a>.
-doc "`hasPinButton/1` returns true if the pane displays a button to float the pane.".
-spec hasPinButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasPinButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_HasPinButton),
  wxe_util:rec(?wxAuiPaneInfo_HasPinButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohide">external documentation</a>.
-doc "`hide/1` indicates that a pane should be hidden.".
-spec hide(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
hide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Hide),
  wxe_util:rec(?wxAuiPaneInfo_Hide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisbottomdockable">external documentation</a>.
-doc """
`isBottomDockable/1` returns true if the pane can be docked at the bottom of the
managed frame.

See: `IsDockable()` (not implemented in wx)
""".
-spec isBottomDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isBottomDockable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsBottomDockable),
  wxe_util:rec(?wxAuiPaneInfo_IsBottomDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisdocked">external documentation</a>.
-doc "`isDocked/1` returns true if the pane is currently docked.".
-spec isDocked(This) -> boolean() when
	This::wxAuiPaneInfo().
isDocked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsDocked),
  wxe_util:rec(?wxAuiPaneInfo_IsDocked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfixed">external documentation</a>.
-doc "`isFixed/1` returns true if the pane cannot be resized.".
-spec isFixed(This) -> boolean() when
	This::wxAuiPaneInfo().
isFixed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsFixed),
  wxe_util:rec(?wxAuiPaneInfo_IsFixed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfloatable">external documentation</a>.
-doc """
`isFloatable/1` returns true if the pane can be undocked and displayed as a
floating window.
""".
-spec isFloatable(This) -> boolean() when
	This::wxAuiPaneInfo().
isFloatable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsFloatable),
  wxe_util:rec(?wxAuiPaneInfo_IsFloatable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfloating">external documentation</a>.
-doc "`isFloating/1` returns true if the pane is floating.".
-spec isFloating(This) -> boolean() when
	This::wxAuiPaneInfo().
isFloating(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsFloating),
  wxe_util:rec(?wxAuiPaneInfo_IsFloating).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisleftdockable">external documentation</a>.
-doc """
`isLeftDockable/1` returns true if the pane can be docked on the left of the
managed frame.

See: `IsDockable()` (not implemented in wx)
""".
-spec isLeftDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isLeftDockable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsLeftDockable),
  wxe_util:rec(?wxAuiPaneInfo_IsLeftDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoismovable">external documentation</a>.
-doc """
IsMoveable() returns true if the docked frame can be undocked or moved to
another dock position.
""".
-spec isMovable(This) -> boolean() when
	This::wxAuiPaneInfo().
isMovable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsMovable),
  wxe_util:rec(?wxAuiPaneInfo_IsMovable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisok">external documentation</a>.
-doc """
`isOk/1` returns true if the `m:wxAuiPaneInfo` structure is valid.

A pane structure is valid if it has an associated window.
""".
-spec isOk(This) -> boolean() when
	This::wxAuiPaneInfo().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsOk),
  wxe_util:rec(?wxAuiPaneInfo_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisresizable">external documentation</a>.
-doc "`isResizable/1` returns true if the pane can be resized.".
-spec isResizable(This) -> boolean() when
	This::wxAuiPaneInfo().
isResizable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsResizable),
  wxe_util:rec(?wxAuiPaneInfo_IsResizable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisrightdockable">external documentation</a>.
-doc """
`isRightDockable/1` returns true if the pane can be docked on the right of the
managed frame.

See: `IsDockable()` (not implemented in wx)
""".
-spec isRightDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isRightDockable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsRightDockable),
  wxe_util:rec(?wxAuiPaneInfo_IsRightDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisshown">external documentation</a>.
-doc "`isShown/1` returns true if the pane is currently shown.".
-spec isShown(This) -> boolean() when
	This::wxAuiPaneInfo().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsShown),
  wxe_util:rec(?wxAuiPaneInfo_IsShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoistoolbar">external documentation</a>.
-doc "`isToolbar/1` returns true if the pane contains a toolbar.".
-spec isToolbar(This) -> boolean() when
	This::wxAuiPaneInfo().
isToolbar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsToolbar),
  wxe_util:rec(?wxAuiPaneInfo_IsToolbar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoistopdockable">external documentation</a>.
-doc """
`isTopDockable/1` returns true if the pane can be docked at the top of the
managed frame.

See: `IsDockable()` (not implemented in wx)
""".
-spec isTopDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isTopDockable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_IsTopDockable),
  wxe_util:rec(?wxAuiPaneInfo_IsTopDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfolayer">external documentation</a>.
-doc """
`layer/2` determines the layer of the docked pane.

The dock layer is similar to an onion, the inner-most layer being layer 0. Each
shell moving in the outward direction has a higher layer number. This allows for
more complex docking layout formation.
""".
-spec layer(This, Layer) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Layer::integer().
layer(#wx_ref{type=ThisT}=This,Layer)
 when is_integer(Layer) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Layer,?get_env(),?wxAuiPaneInfo_Layer),
  wxe_util:rec(?wxAuiPaneInfo_Layer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoleft">external documentation</a>.
-doc """
`left/1` sets the pane dock position to the left side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_LEFT).
""".
-spec left(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
left(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Left),
  wxe_util:rec(?wxAuiPaneInfo_Left).

%% @equiv leftDockable(This, [])
-spec leftDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

leftDockable(This)
 when is_record(This, wx_ref) ->
  leftDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoleftdockable">external documentation</a>.
-doc """
`leftDockable/2` indicates whether a pane can be docked on the left of the
frame.
""".
-spec leftDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
leftDockable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_LeftDockable),
  wxe_util:rec(?wxAuiPaneInfo_LeftDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
-doc "`maxSize/3` sets the maximum size of the pane.".
-spec maxSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
maxSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxAuiPaneInfo_MaxSize_1),
  wxe_util:rec(?wxAuiPaneInfo_MaxSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
-spec maxSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
maxSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxAuiPaneInfo_MaxSize_2),
  wxe_util:rec(?wxAuiPaneInfo_MaxSize_2).

%% @equiv maximizeButton(This, [])
-spec maximizeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

maximizeButton(This)
 when is_record(This, wx_ref) ->
  maximizeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaximizebutton">external documentation</a>.
-doc """
`maximizeButton/2` indicates that a maximize button should be drawn for the
pane.
""".
-spec maximizeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
maximizeButton(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_MaximizeButton),
  wxe_util:rec(?wxAuiPaneInfo_MaximizeButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
-doc """
`minSize/3` sets the minimum size of the pane.

Please note that this is only partially supported as of this writing.
""".
-spec minSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
minSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxAuiPaneInfo_MinSize_1),
  wxe_util:rec(?wxAuiPaneInfo_MinSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
-spec minSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
minSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxAuiPaneInfo_MinSize_2),
  wxe_util:rec(?wxAuiPaneInfo_MinSize_2).

%% @equiv minimizeButton(This, [])
-spec minimizeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

minimizeButton(This)
 when is_record(This, wx_ref) ->
  minimizeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominimizebutton">external documentation</a>.
-doc """
`minimizeButton/2` indicates that a minimize button should be drawn for the
pane.
""".
-spec minimizeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
minimizeButton(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_MinimizeButton),
  wxe_util:rec(?wxAuiPaneInfo_MinimizeButton).

%% @equiv movable(This, [])
-spec movable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

movable(This)
 when is_record(This, wx_ref) ->
  movable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomovable">external documentation</a>.
-doc "Movable indicates whether a frame can be moved.".
-spec movable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
movable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Movable),
  wxe_util:rec(?wxAuiPaneInfo_Movable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoname">external documentation</a>.
-doc """
`name/2` sets the name of the pane so it can be referenced in lookup functions.

If a name is not specified by the user, a random name is assigned to the pane
when it is added to the manager.
""".
-spec name(This, N) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), N::unicode:chardata().
name(#wx_ref{type=ThisT}=This,N)
 when ?is_chardata(N) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  N_UC = unicode:characters_to_binary(N),
  wxe_util:queue_cmd(This,N_UC,?get_env(),?wxAuiPaneInfo_Name),
  wxe_util:rec(?wxAuiPaneInfo_Name).

%% @equiv paneBorder(This, [])
-spec paneBorder(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

paneBorder(This)
 when is_record(This, wx_ref) ->
  paneBorder(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfopaneborder">external documentation</a>.
-doc "PaneBorder indicates that a border should be drawn for the pane.".
-spec paneBorder(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
paneBorder(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_PaneBorder),
  wxe_util:rec(?wxAuiPaneInfo_PaneBorder).

%% @equiv pinButton(This, [])
-spec pinButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

pinButton(This)
 when is_record(This, wx_ref) ->
  pinButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfopinbutton">external documentation</a>.
-doc "`pinButton/2` indicates that a pin button should be drawn for the pane.".
-spec pinButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
pinButton(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, _visible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_PinButton),
  wxe_util:rec(?wxAuiPaneInfo_PinButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoposition">external documentation</a>.
-doc "`position/2` determines the position of the docked pane.".
-spec position(This, Pos) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Pos::integer().
position(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxAuiPaneInfo_Position),
  wxe_util:rec(?wxAuiPaneInfo_Position).

%% @equiv resizable(This, [])
-spec resizable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

resizable(This)
 when is_record(This, wx_ref) ->
  resizable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforesizable">external documentation</a>.
-doc """
`resizable/2` allows a pane to be resized if the parameter is true, and forces
it to be a fixed size if the parameter is false.

This is simply an antonym for `fixed/1`.
""".
-spec resizable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'resizable', boolean()}.
resizable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({resizable, _resizable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Resizable),
  wxe_util:rec(?wxAuiPaneInfo_Resizable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforight">external documentation</a>.
-doc """
`right/1` sets the pane dock position to the right side of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_RIGHT).
""".
-spec right(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
right(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Right),
  wxe_util:rec(?wxAuiPaneInfo_Right).

%% @equiv rightDockable(This, [])
-spec rightDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

rightDockable(This)
 when is_record(This, wx_ref) ->
  rightDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforightdockable">external documentation</a>.
-doc """
`rightDockable/2` indicates whether a pane can be docked on the right of the
frame.
""".
-spec rightDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
rightDockable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_RightDockable),
  wxe_util:rec(?wxAuiPaneInfo_RightDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforow">external documentation</a>.
-doc "`row/2` determines the row of the docked pane.".
-spec row(This, Row) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Row::integer().
row(#wx_ref{type=ThisT}=This,Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Row,?get_env(),?wxAuiPaneInfo_Row),
  wxe_util:rec(?wxAuiPaneInfo_Row).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfosafeset">external documentation</a>.
-doc """
Write the safe parts of a PaneInfo object "source" into "this".

"Safe parts" are all non-UI elements (e.g. all layout determining parameters
like the size, position etc.). "Unsafe parts" (pointers to button, frame and
window) are not modified by this write operation.

Remark: This method is used when loading perspectives.
""".
-spec safeSet(This, Source) -> 'ok' when
	This::wxAuiPaneInfo(), Source::wxAuiPaneInfo().
safeSet(#wx_ref{type=ThisT}=This,#wx_ref{type=SourceT}=Source) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(SourceT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Source,?get_env(),?wxAuiPaneInfo_SafeSet).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfosetflag">external documentation</a>.
-doc """
`setFlag/3` turns the property given by flag on or off with the option_state
parameter.
""".
-spec setFlag(This, Flag, Option_state) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Flag::integer(), Option_state::boolean().
setFlag(#wx_ref{type=ThisT}=This,Flag,Option_state)
 when is_integer(Flag),is_boolean(Option_state) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Flag,Option_state,?get_env(),?wxAuiPaneInfo_SetFlag),
  wxe_util:rec(?wxAuiPaneInfo_SetFlag).

%% @equiv show(This, [])
-spec show(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoshow">external documentation</a>.
-doc "`show/2` indicates that a pane should be shown.".
-spec show(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_Show),
  wxe_util:rec(?wxAuiPaneInfo_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotoolbarpane">external documentation</a>.
-doc """
`toolbarPane/1` specifies that the pane should adopt the default toolbar pane
settings.
""".
-spec toolbarPane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
toolbarPane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_ToolbarPane),
  wxe_util:rec(?wxAuiPaneInfo_ToolbarPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotop">external documentation</a>.
-doc """
`top/1` sets the pane dock position to the top of the frame.

This is the same thing as calling Direction(wxAUI_DOCK_TOP).
""".
-spec top(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
top(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_Top),
  wxe_util:rec(?wxAuiPaneInfo_Top).

%% @equiv topDockable(This, [])
-spec topDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

topDockable(This)
 when is_record(This, wx_ref) ->
  topDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotopdockable">external documentation</a>.
-doc "`topDockable/2` indicates whether a pane can be docked at the top of the frame.".
-spec topDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
topDockable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, _b} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiPaneInfo_TopDockable),
  wxe_util:rec(?wxAuiPaneInfo_TopDockable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowindow">external documentation</a>.
-doc """
`window/2` assigns the window pointer that the `m:wxAuiPaneInfo` should use.

This normally does not need to be specified, as the window pointer is
automatically assigned to the `m:wxAuiPaneInfo` structure as soon as it is added
to the manager.
""".
-spec window(This, W) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), W::wxWindow:wxWindow().
window(#wx_ref{type=ThisT}=This,#wx_ref{type=WT}=W) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(This,W,?get_env(),?wxAuiPaneInfo_Window),
  wxe_util:rec(?wxAuiPaneInfo_Window).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetwindow">external documentation</a>.
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxAuiPaneInfo().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetWindow),
  wxe_util:rec(?wxAuiPaneInfo_GetWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetframe">external documentation</a>.
-spec getFrame(This) -> wxFrame:wxFrame() when
	This::wxAuiPaneInfo().
getFrame(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetFrame),
  wxe_util:rec(?wxAuiPaneInfo_GetFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetdirection">external documentation</a>.
-spec getDirection(This) -> integer() when
	This::wxAuiPaneInfo().
getDirection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetDirection),
  wxe_util:rec(?wxAuiPaneInfo_GetDirection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetlayer">external documentation</a>.
-spec getLayer(This) -> integer() when
	This::wxAuiPaneInfo().
getLayer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetLayer),
  wxe_util:rec(?wxAuiPaneInfo_GetLayer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetrow">external documentation</a>.
-spec getRow(This) -> integer() when
	This::wxAuiPaneInfo().
getRow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetRow),
  wxe_util:rec(?wxAuiPaneInfo_GetRow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetposition">external documentation</a>.
-spec getPosition(This) -> integer() when
	This::wxAuiPaneInfo().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetPosition),
  wxe_util:rec(?wxAuiPaneInfo_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetfloatingposition">external documentation</a>.
-spec getFloatingPosition(This) -> {X::integer(), Y::integer()} when
	This::wxAuiPaneInfo().
getFloatingPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetFloatingPosition),
  wxe_util:rec(?wxAuiPaneInfo_GetFloatingPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetfloatingsize">external documentation</a>.
-spec getFloatingSize(This) -> {W::integer(), H::integer()} when
	This::wxAuiPaneInfo().
getFloatingSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiPaneInfo_GetFloatingSize),
  wxe_util:rec(?wxAuiPaneInfo_GetFloatingSize).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxAuiPaneInfo()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiPaneInfo),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAuiPaneInfo_destroy),
  ok.
