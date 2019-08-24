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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html">wxAuiPaneInfo</a>.
%% @type wxAuiPaneInfo().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAuiPaneInfo).
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

-export_type([wxAuiPaneInfo/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAuiPaneInfo() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
-spec new() -> wxAuiPaneInfo().
new() ->
  wxe_util:construct(?wxAuiPaneInfo_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
-spec new(C) -> wxAuiPaneInfo() when
	C::wxAuiPaneInfo().
new(#wx_ref{type=CT,ref=CRef}) ->
  ?CLASS(CT,wxAuiPaneInfo),
  wxe_util:construct(?wxAuiPaneInfo_new_1,
  <<CRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
-spec bestSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
bestSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_BestSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
-spec bestSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
bestSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_BestSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobottom">external documentation</a>.
-spec bottom(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
bottom(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Bottom,
  <<ThisRef:32/?UI>>).

%% @equiv bottomDockable(This, [])
-spec bottomDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

bottomDockable(This)
 when is_record(This, wx_ref) ->
  bottomDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfobottomdockable">external documentation</a>.
-spec bottomDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
bottomDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_BottomDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocaption">external documentation</a>.
-spec caption(This, C) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), C::unicode:chardata().
caption(#wx_ref{type=ThisT,ref=ThisRef},C)
 when ?is_chardata(C) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  C_UC = unicode:characters_to_binary([C,0]),
  wxe_util:call(?wxAuiPaneInfo_Caption,
  <<ThisRef:32/?UI,(byte_size(C_UC)):32/?UI,(C_UC)/binary, 0:(((8- ((0+byte_size(C_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv captionVisible(This, [])
-spec captionVisible(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

captionVisible(This)
 when is_record(This, wx_ref) ->
  captionVisible(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocaptionvisible">external documentation</a>.
-spec captionVisible(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
captionVisible(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_CaptionVisible,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocentre">external documentation</a>.
-spec centre(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
centre(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Centre,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfocentrepane">external documentation</a>.
-spec centrePane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
centrePane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_CentrePane,
  <<ThisRef:32/?UI>>).

%% @equiv closeButton(This, [])
-spec closeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

closeButton(This)
 when is_record(This, wx_ref) ->
  closeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoclosebutton">external documentation</a>.
-spec closeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
closeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_CloseButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodefaultpane">external documentation</a>.
-spec defaultPane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
defaultPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_DefaultPane,
  <<ThisRef:32/?UI>>).

%% @equiv destroyOnClose(This, [])
-spec destroyOnClose(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

destroyOnClose(This)
 when is_record(This, wx_ref) ->
  destroyOnClose(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodestroyonclose">external documentation</a>.
-spec destroyOnClose(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
destroyOnClose(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_DestroyOnClose,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodirection">external documentation</a>.
-spec direction(This, Direction) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Direction::integer().
direction(#wx_ref{type=ThisT,ref=ThisRef},Direction)
 when is_integer(Direction) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Direction,
  <<ThisRef:32/?UI,Direction:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodock">external documentation</a>.
-spec dock(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
dock(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Dock,
  <<ThisRef:32/?UI>>).

%% @equiv dockable(This, [])
-spec dockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

dockable(This)
 when is_record(This, wx_ref) ->
  dockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfodockable">external documentation</a>.
-spec dockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
dockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Dockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofixed">external documentation</a>.
-spec fixed(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
fixed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Fixed,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloat">external documentation</a>.
-spec float(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
float(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Float,
  <<ThisRef:32/?UI>>).

%% @equiv floatable(This, [])
-spec floatable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

floatable(This)
 when is_record(This, wx_ref) ->
  floatable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatable">external documentation</a>.
-spec floatable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
floatable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Floatable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
-spec floatingPosition(This, Pos) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Pos::{X::integer(), Y::integer()}.
floatingPosition(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY})
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingPosition_1,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
-spec floatingPosition(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
floatingPosition(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingPosition_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
-spec floatingSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
floatingSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
-spec floatingSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
floatingSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @equiv gripper(This, [])
-spec gripper(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

gripper(This)
 when is_record(This, wx_ref) ->
  gripper(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogripper">external documentation</a>.
-spec gripper(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
gripper(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Gripper,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv gripperTop(This, [])
-spec gripperTop(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

gripperTop(This)
 when is_record(This, wx_ref) ->
  gripperTop(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogrippertop">external documentation</a>.
-spec gripperTop(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'attop', boolean()}.
gripperTop(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({attop, Attop}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Attop)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_GripperTop,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasborder">external documentation</a>.
-spec hasBorder(This) -> boolean() when
	This::wxAuiPaneInfo().
hasBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasBorder,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohascaption">external documentation</a>.
-spec hasCaption(This) -> boolean() when
	This::wxAuiPaneInfo().
hasCaption(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasCaption,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasclosebutton">external documentation</a>.
-spec hasCloseButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasCloseButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasCloseButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasflag">external documentation</a>.
-spec hasFlag(This, Flag) -> boolean() when
	This::wxAuiPaneInfo(), Flag::integer().
hasFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasFlag,
  <<ThisRef:32/?UI,Flag:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasgripper">external documentation</a>.
-spec hasGripper(This) -> boolean() when
	This::wxAuiPaneInfo().
hasGripper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasGripper,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasgrippertop">external documentation</a>.
-spec hasGripperTop(This) -> boolean() when
	This::wxAuiPaneInfo().
hasGripperTop(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasGripperTop,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasmaximizebutton">external documentation</a>.
-spec hasMaximizeButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasMaximizeButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasMaximizeButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohasminimizebutton">external documentation</a>.
-spec hasMinimizeButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasMinimizeButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasMinimizeButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohaspinbutton">external documentation</a>.
-spec hasPinButton(This) -> boolean() when
	This::wxAuiPaneInfo().
hasPinButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasPinButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfohide">external documentation</a>.
-spec hide(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
hide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Hide,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisbottomdockable">external documentation</a>.
-spec isBottomDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isBottomDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsBottomDockable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisdocked">external documentation</a>.
-spec isDocked(This) -> boolean() when
	This::wxAuiPaneInfo().
isDocked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsDocked,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfixed">external documentation</a>.
-spec isFixed(This) -> boolean() when
	This::wxAuiPaneInfo().
isFixed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFixed,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfloatable">external documentation</a>.
-spec isFloatable(This) -> boolean() when
	This::wxAuiPaneInfo().
isFloatable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFloatable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisfloating">external documentation</a>.
-spec isFloating(This) -> boolean() when
	This::wxAuiPaneInfo().
isFloating(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFloating,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisleftdockable">external documentation</a>.
-spec isLeftDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isLeftDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsLeftDockable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoismovable">external documentation</a>.
-spec isMovable(This) -> boolean() when
	This::wxAuiPaneInfo().
isMovable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsMovable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxAuiPaneInfo().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisresizable">external documentation</a>.
-spec isResizable(This) -> boolean() when
	This::wxAuiPaneInfo().
isResizable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsResizable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisrightdockable">external documentation</a>.
-spec isRightDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isRightDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsRightDockable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxAuiPaneInfo().
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsShown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoistoolbar">external documentation</a>.
-spec isToolbar(This) -> boolean() when
	This::wxAuiPaneInfo().
isToolbar(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsToolbar,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoistopdockable">external documentation</a>.
-spec isTopDockable(This) -> boolean() when
	This::wxAuiPaneInfo().
isTopDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsTopDockable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfolayer">external documentation</a>.
-spec layer(This, Layer) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Layer::integer().
layer(#wx_ref{type=ThisT,ref=ThisRef},Layer)
 when is_integer(Layer) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Layer,
  <<ThisRef:32/?UI,Layer:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoleft">external documentation</a>.
-spec left(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
left(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Left,
  <<ThisRef:32/?UI>>).

%% @equiv leftDockable(This, [])
-spec leftDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

leftDockable(This)
 when is_record(This, wx_ref) ->
  leftDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoleftdockable">external documentation</a>.
-spec leftDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
leftDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_LeftDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
-spec maxSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
maxSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MaxSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
-spec maxSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
maxSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MaxSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @equiv maximizeButton(This, [])
-spec maximizeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

maximizeButton(This)
 when is_record(This, wx_ref) ->
  maximizeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomaximizebutton">external documentation</a>.
-spec maximizeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
maximizeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_MaximizeButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
-spec minSize(This, Size) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Size::{W::integer(), H::integer()}.
minSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
-spec minSize(This, X, Y) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), X::integer(), Y::integer().
minSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MinSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @equiv minimizeButton(This, [])
-spec minimizeButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

minimizeButton(This)
 when is_record(This, wx_ref) ->
  minimizeButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfominimizebutton">external documentation</a>.
-spec minimizeButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
minimizeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_MinimizeButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv movable(This, [])
-spec movable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

movable(This)
 when is_record(This, wx_ref) ->
  movable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfomovable">external documentation</a>.
-spec movable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
movable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Movable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoname">external documentation</a>.
-spec name(This, N) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), N::unicode:chardata().
name(#wx_ref{type=ThisT,ref=ThisRef},N)
 when ?is_chardata(N) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  N_UC = unicode:characters_to_binary([N,0]),
  wxe_util:call(?wxAuiPaneInfo_Name,
  <<ThisRef:32/?UI,(byte_size(N_UC)):32/?UI,(N_UC)/binary, 0:(((8- ((0+byte_size(N_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv paneBorder(This, [])
-spec paneBorder(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

paneBorder(This)
 when is_record(This, wx_ref) ->
  paneBorder(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfopaneborder">external documentation</a>.
-spec paneBorder(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
paneBorder(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_PaneBorder,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv pinButton(This, [])
-spec pinButton(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

pinButton(This)
 when is_record(This, wx_ref) ->
  pinButton(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfopinbutton">external documentation</a>.
-spec pinButton(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'visible', boolean()}.
pinButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_PinButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoposition">external documentation</a>.
-spec position(This, Pos) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Pos::integer().
position(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Position,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @equiv resizable(This, [])
-spec resizable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

resizable(This)
 when is_record(This, wx_ref) ->
  resizable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforesizable">external documentation</a>.
-spec resizable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'resizable', boolean()}.
resizable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({resizable, Resizable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Resizable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Resizable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforight">external documentation</a>.
-spec right(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
right(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Right,
  <<ThisRef:32/?UI>>).

%% @equiv rightDockable(This, [])
-spec rightDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

rightDockable(This)
 when is_record(This, wx_ref) ->
  rightDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforightdockable">external documentation</a>.
-spec rightDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
rightDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_RightDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinforow">external documentation</a>.
-spec row(This, Row) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Row::integer().
row(#wx_ref{type=ThisT,ref=ThisRef},Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Row,
  <<ThisRef:32/?UI,Row:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfosafeset">external documentation</a>.
-spec safeSet(This, Source) -> 'ok' when
	This::wxAuiPaneInfo(), Source::wxAuiPaneInfo().
safeSet(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SourceT,ref=SourceRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(SourceT,wxAuiPaneInfo),
  wxe_util:cast(?wxAuiPaneInfo_SafeSet,
  <<ThisRef:32/?UI,SourceRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfosetflag">external documentation</a>.
-spec setFlag(This, Flag, Option_state) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), Flag::integer(), Option_state::boolean().
setFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag,Option_state)
 when is_integer(Flag),is_boolean(Option_state) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_SetFlag,
  <<ThisRef:32/?UI,Flag:32/?UI,(wxe_util:from_bool(Option_state)):32/?UI>>).

%% @equiv show(This, [])
-spec show(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfoshow">external documentation</a>.
-spec show(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Show,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotoolbarpane">external documentation</a>.
-spec toolbarPane(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
toolbarPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_ToolbarPane,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotop">external documentation</a>.
-spec top(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().
top(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Top,
  <<ThisRef:32/?UI>>).

%% @equiv topDockable(This, [])
-spec topDockable(This) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo().

topDockable(This)
 when is_record(This, wx_ref) ->
  topDockable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfotopdockable">external documentation</a>.
-spec topDockable(This, [Option]) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(),
	Option :: {'b', boolean()}.
topDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_TopDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfowindow">external documentation</a>.
-spec window(This, W) -> wxAuiPaneInfo() when
	This::wxAuiPaneInfo(), W::wxWindow:wxWindow().
window(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WT,ref=WRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(WT,wxWindow),
  wxe_util:call(?wxAuiPaneInfo_Window,
  <<ThisRef:32/?UI,WRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetwindow">external documentation</a>.
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxAuiPaneInfo().
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetframe">external documentation</a>.
-spec getFrame(This) -> wxFrame:wxFrame() when
	This::wxAuiPaneInfo().
getFrame(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetFrame,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetdirection">external documentation</a>.
-spec getDirection(This) -> integer() when
	This::wxAuiPaneInfo().
getDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetDirection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetlayer">external documentation</a>.
-spec getLayer(This) -> integer() when
	This::wxAuiPaneInfo().
getLayer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetLayer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetrow">external documentation</a>.
-spec getRow(This) -> integer() when
	This::wxAuiPaneInfo().
getRow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetRow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetposition">external documentation</a>.
-spec getPosition(This) -> integer() when
	This::wxAuiPaneInfo().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetfloatingposition">external documentation</a>.
-spec getFloatingPosition(This) -> {X::integer(), Y::integer()} when
	This::wxAuiPaneInfo().
getFloatingPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetFloatingPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauipaneinfo.html#wxauipaneinfogetfloatingsize">external documentation</a>.
-spec getFloatingSize(This) -> {W::integer(), H::integer()} when
	This::wxAuiPaneInfo().
getFloatingSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_GetFloatingSize,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAuiPaneInfo()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiPaneInfo),
  wxe_util:destroy(?wxAuiPaneInfo_destruct,Obj),
  ok.
