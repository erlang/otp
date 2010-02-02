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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html">wxAuiPaneInfo</a>.
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
  floatingSize/3,gripper/1,gripper/2,gripperTop/1,gripperTop/2,hasBorder/1,
  hasCaption/1,hasCloseButton/1,hasFlag/2,hasGripper/1,hasGripperTop/1,
  hasMaximizeButton/1,hasMinimizeButton/1,hasPinButton/1,hide/1,isBottomDockable/1,
  isDocked/1,isFixed/1,isFloatable/1,isFloating/1,isLeftDockable/1,isMovable/1,
  isOk/1,isResizable/1,isRightDockable/1,isShown/1,isToolbar/1,isTopDockable/1,
  layer/2,left/1,leftDockable/1,leftDockable/2,maxSize/2,maxSize/3,maximizeButton/1,
  maximizeButton/2,minSize/2,minSize/3,minimizeButton/1,minimizeButton/2,
  movable/1,movable/2,name/2,new/0,new/1,paneBorder/1,paneBorder/2,pinButton/1,
  pinButton/2,position/2,resizable/1,resizable/2,right/1,rightDockable/1,
  rightDockable/2,row/2,safeSet/2,setFlag/3,show/1,show/2,toolbarPane/1,
  top/1,topDockable/1,topDockable/2,window/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
new() ->
  wxe_util:construct(?wxAuiPaneInfo_new_0,
  <<>>).

%% @spec (C::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfowxauipaneinfo">external documentation</a>.
new(#wx_ref{type=CT,ref=CRef}) ->
  ?CLASS(CT,wxAuiPaneInfo),
  wxe_util:construct(?wxAuiPaneInfo_new_1,
  <<CRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Size::{W::integer(),H::integer()}) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
bestSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_BestSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), X::integer(), Y::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfobestsize">external documentation</a>.
bestSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_BestSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfobottom">external documentation</a>.
bottom(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Bottom,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv bottomDockable(This, [])
bottomDockable(This)
 when is_record(This, wx_ref) ->
  bottomDockable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfobottomdockable">external documentation</a>.
bottomDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_BottomDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), C::string()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfocaption">external documentation</a>.
caption(#wx_ref{type=ThisT,ref=ThisRef},C)
 when is_list(C) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  C_UC = unicode:characters_to_binary([C,0]),
  wxe_util:call(?wxAuiPaneInfo_Caption,
  <<ThisRef:32/?UI,(byte_size(C_UC)):32/?UI,(C_UC)/binary, 0:(((8- ((0+byte_size(C_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv captionVisible(This, [])
captionVisible(This)
 when is_record(This, wx_ref) ->
  captionVisible(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfocaptionvisible">external documentation</a>.
captionVisible(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_CaptionVisible,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfocentre">external documentation</a>.
centre(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Centre,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfocentrepane">external documentation</a>.
centrePane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_CentrePane,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv closeButton(This, [])
closeButton(This)
 when is_record(This, wx_ref) ->
  closeButton(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoclosebutton">external documentation</a>.
closeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_CloseButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfodefaultpane">external documentation</a>.
defaultPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_DefaultPane,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv destroyOnClose(This, [])
destroyOnClose(This)
 when is_record(This, wx_ref) ->
  destroyOnClose(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfodestroyonclose">external documentation</a>.
destroyOnClose(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_DestroyOnClose,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Direction::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfodirection">external documentation</a>.
direction(#wx_ref{type=ThisT,ref=ThisRef},Direction)
 when is_integer(Direction) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Direction,
  <<ThisRef:32/?UI,Direction:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfodock">external documentation</a>.
dock(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Dock,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv dockable(This, [])
dockable(This)
 when is_record(This, wx_ref) ->
  dockable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfodockable">external documentation</a>.
dockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Dockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofixed">external documentation</a>.
fixed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Fixed,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloat">external documentation</a>.
float(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Float,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv floatable(This, [])
floatable(This)
 when is_record(This, wx_ref) ->
  floatable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloatable">external documentation</a>.
floatable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Floatable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Pos::{X::integer(),Y::integer()}) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
floatingPosition(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY})
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingPosition_1,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), X::integer(), Y::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloatingposition">external documentation</a>.
floatingPosition(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingPosition_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Size::{W::integer(),H::integer()}) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
floatingSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), X::integer(), Y::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfofloatingsize">external documentation</a>.
floatingSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_FloatingSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv gripper(This, [])
gripper(This)
 when is_record(This, wx_ref) ->
  gripper(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfogripper">external documentation</a>.
gripper(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Gripper,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv gripperTop(This, [])
gripperTop(This)
 when is_record(This, wx_ref) ->
  gripperTop(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {attop, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfogrippertop">external documentation</a>.
gripperTop(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({attop, Attop}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Attop)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_GripperTop,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasborder">external documentation</a>.
hasBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasBorder,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohascaption">external documentation</a>.
hasCaption(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasCaption,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasclosebutton">external documentation</a>.
hasCloseButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasCloseButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Flag::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasflag">external documentation</a>.
hasFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasFlag,
  <<ThisRef:32/?UI,Flag:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasgripper">external documentation</a>.
hasGripper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasGripper,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasgrippertop">external documentation</a>.
hasGripperTop(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasGripperTop,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasmaximizebutton">external documentation</a>.
hasMaximizeButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasMaximizeButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohasminimizebutton">external documentation</a>.
hasMinimizeButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasMinimizeButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohaspinbutton">external documentation</a>.
hasPinButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_HasPinButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfohide">external documentation</a>.
hide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Hide,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisbottomdockable">external documentation</a>.
isBottomDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsBottomDockable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisdocked">external documentation</a>.
isDocked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsDocked,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisfixed">external documentation</a>.
isFixed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFixed,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisfloatable">external documentation</a>.
isFloatable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFloatable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisfloating">external documentation</a>.
isFloating(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsFloating,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisleftdockable">external documentation</a>.
isLeftDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsLeftDockable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoismovable">external documentation</a>.
isMovable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsMovable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisresizable">external documentation</a>.
isResizable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsResizable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisrightdockable">external documentation</a>.
isRightDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsRightDockable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoisshown">external documentation</a>.
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsShown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoistoolbar">external documentation</a>.
isToolbar(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsToolbar,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoistopdockable">external documentation</a>.
isTopDockable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_IsTopDockable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Layer::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfolayer">external documentation</a>.
layer(#wx_ref{type=ThisT,ref=ThisRef},Layer)
 when is_integer(Layer) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Layer,
  <<ThisRef:32/?UI,Layer:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoleft">external documentation</a>.
left(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Left,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv leftDockable(This, [])
leftDockable(This)
 when is_record(This, wx_ref) ->
  leftDockable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoleftdockable">external documentation</a>.
leftDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_LeftDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Size::{W::integer(),H::integer()}) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
maxSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MaxSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), X::integer(), Y::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfomaxsize">external documentation</a>.
maxSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MaxSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv maximizeButton(This, [])
maximizeButton(This)
 when is_record(This, wx_ref) ->
  maximizeButton(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfomaximizebutton">external documentation</a>.
maximizeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_MaximizeButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Size::{W::integer(),H::integer()}) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
minSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), X::integer(), Y::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfominsize">external documentation</a>.
minSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_MinSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv minimizeButton(This, [])
minimizeButton(This)
 when is_record(This, wx_ref) ->
  minimizeButton(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfominimizebutton">external documentation</a>.
minimizeButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_MinimizeButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv movable(This, [])
movable(This)
 when is_record(This, wx_ref) ->
  movable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfomovable">external documentation</a>.
movable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Movable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), N::string()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoname">external documentation</a>.
name(#wx_ref{type=ThisT,ref=ThisRef},N)
 when is_list(N) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  N_UC = unicode:characters_to_binary([N,0]),
  wxe_util:call(?wxAuiPaneInfo_Name,
  <<ThisRef:32/?UI,(byte_size(N_UC)):32/?UI,(N_UC)/binary, 0:(((8- ((0+byte_size(N_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv paneBorder(This, [])
paneBorder(This)
 when is_record(This, wx_ref) ->
  paneBorder(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfopaneborder">external documentation</a>.
paneBorder(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_PaneBorder,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv pinButton(This, [])
pinButton(This)
 when is_record(This, wx_ref) ->
  pinButton(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {visible, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfopinbutton">external documentation</a>.
pinButton(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({visible, Visible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_PinButton,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Pos::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoposition">external documentation</a>.
position(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Position,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv resizable(This, [])
resizable(This)
 when is_record(This, wx_ref) ->
  resizable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {resizable, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinforesizable">external documentation</a>.
resizable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({resizable, Resizable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Resizable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Resizable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinforight">external documentation</a>.
right(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Right,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv rightDockable(This, [])
rightDockable(This)
 when is_record(This, wx_ref) ->
  rightDockable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinforightdockable">external documentation</a>.
rightDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_RightDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), Row::integer()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinforow">external documentation</a>.
row(#wx_ref{type=ThisT,ref=ThisRef},Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Row,
  <<ThisRef:32/?UI,Row:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Source::wxAuiPaneInfo()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfosafeset">external documentation</a>.
safeSet(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SourceT,ref=SourceRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(SourceT,wxAuiPaneInfo),
  wxe_util:cast(?wxAuiPaneInfo_SafeSet,
  <<ThisRef:32/?UI,SourceRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo(), Flag::integer(), Option_state::bool()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfosetflag">external documentation</a>.
setFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag,Option_state)
 when is_integer(Flag),is_boolean(Option_state) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_SetFlag,
  <<ThisRef:32/?UI,Flag:32/?UI,(wxe_util:from_bool(Option_state)):32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv show(This, [])
show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {show, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfoshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_Show,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfotoolbarpane">external documentation</a>.
toolbarPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_ToolbarPane,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfotop">external documentation</a>.
top(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiPaneInfo_Top,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> wxAuiPaneInfo()
%% @equiv topDockable(This, [])
topDockable(This)
 when is_record(This, wx_ref) ->
  topDockable(This, []).

%% @spec (This::wxAuiPaneInfo(), [Option]) -> wxAuiPaneInfo()
%% Option = {b, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfotopdockable">external documentation</a>.
topDockable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  MOpts = fun({b, B}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(B)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiPaneInfo_TopDockable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiPaneInfo(), W::wxWindow:wxWindow()) -> wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauipaneinfo.html#wxauipaneinfowindow">external documentation</a>.
window(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WT,ref=WRef}) ->
  ?CLASS(ThisT,wxAuiPaneInfo),
  ?CLASS(WT,wxWindow),
  wxe_util:call(?wxAuiPaneInfo_Window,
  <<ThisRef:32/?UI,WRef:32/?UI>>).

%% @spec (This::wxAuiPaneInfo()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiPaneInfo),
  wxe_util:destroy(?wxAuiPaneInfo_destruct,Obj),
  ok.
