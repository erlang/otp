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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html">wxSizerItem</a>.
%% @type wxSizerItem().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSizerItem).
-include("wxe.hrl").
-export([calcMin/1,deleteWindows/1,destroy/1,detachSizer/1,getBorder/1,getFlag/1,
  getMinSize/1,getPosition/1,getProportion/1,getRatio/1,getRect/1,getSize/1,
  getSizer/1,getSpacer/1,getUserData/1,getWindow/1,isShown/1,isSizer/1,
  isSpacer/1,isWindow/1,new/0,new/2,new/3,new/5,new/6,setBorder/2,setDimension/3,
  setFlag/2,setInitSize/3,setMinSize/2,setMinSize/3,setProportion/2,
  setRatio/2,setRatio/3,setSizer/2,setSpacer/2,setSpacer/3,setWindow/2,
  show/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
new() ->
  wxe_util:construct(?wxSizerItem_new_0,
  <<>>).

%% @spec (Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags()) -> wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
new(#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=FlagsT,ref=FlagsRef}) ->
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizerItem_new_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizerItem_new_2_0
     end,
  wxe_util:construct(WindowOP,
  <<WindowRef:32/?UI,FlagsRef:32/?UI>>).

%% @spec (Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags()) -> wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
new(Width,Height,#wx_ref{type=FlagsT,ref=FlagsRef})
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:construct(?wxSizerItem_new_3,
  <<Width:32/?UI,Height:32/?UI,FlagsRef:32/?UI>>).

%% @spec (Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Proportion::integer(), Flag::integer(), Border::integer(), UserData::wx:wx()) -> wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
new(#wx_ref{type=WindowT,ref=WindowRef},Proportion,Flag,Border,#wx_ref{type=UserDataT,ref=UserDataRef})
 when is_integer(Proportion),is_integer(Flag),is_integer(Border) ->
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
         ?CLASS(UserDataT,wx),
       ?wxSizerItem_new_5_1;
     _ -> ?CLASS(WindowT,wxSizer),
         ?CLASS(UserDataT,wx),
       ?wxSizerItem_new_5_0
     end,
  wxe_util:construct(WindowOP,
  <<WindowRef:32/?UI,Proportion:32/?UI,Flag:32/?UI,Border:32/?UI,UserDataRef:32/?UI>>).

%% @spec (Width::integer(), Height::integer(), Proportion::integer(), Flag::integer(), Border::integer(), UserData::wx:wx()) -> wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
new(Width,Height,Proportion,Flag,Border,#wx_ref{type=UserDataT,ref=UserDataRef})
 when is_integer(Width),is_integer(Height),is_integer(Proportion),is_integer(Flag),is_integer(Border) ->
  ?CLASS(UserDataT,wx),
  wxe_util:construct(?wxSizerItem_new_6,
  <<Width:32/?UI,Height:32/?UI,Proportion:32/?UI,Flag:32/?UI,Border:32/?UI,UserDataRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemcalcmin">external documentation</a>.
calcMin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_CalcMin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemdeletewindows">external documentation</a>.
deleteWindows(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_DeleteWindows,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemdetachsizer">external documentation</a>.
detachSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_DetachSizer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetborder">external documentation</a>.
getBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetBorder,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetflag">external documentation</a>.
getFlag(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetFlag,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetminsize">external documentation</a>.
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetproportion">external documentation</a>.
getProportion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetProportion,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> float()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetratio">external documentation</a>.
getRatio(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetRatio,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetrect">external documentation</a>.
getRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetRect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> wxSizer:wxSizer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetsizer">external documentation</a>.
getSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSizer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetspacer">external documentation</a>.
getSpacer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSpacer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetuserdata">external documentation</a>.
getUserData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetUserData,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemgetwindow">external documentation</a>.
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemissizer">external documentation</a>.
isSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsSizer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemisshown">external documentation</a>.
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsShown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemisspacer">external documentation</a>.
isSpacer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsSpacer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemiswindow">external documentation</a>.
isWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerItem(), Border::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetborder">external documentation</a>.
setBorder(#wx_ref{type=ThisT,ref=ThisRef},Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetBorder,
  <<ThisRef:32/?UI,Border:32/?UI>>).

%% @spec (This::wxSizerItem(), Pos::{X::integer(),Y::integer()}, Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetdimension">external documentation</a>.
setDimension(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY},{SizeW,SizeH})
 when is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetDimension,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizerItem(), Flag::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetflag">external documentation</a>.
setFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetFlag,
  <<ThisRef:32/?UI,Flag:32/?UI>>).

%% @spec (This::wxSizerItem(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetinitsize">external documentation</a>.
setInitSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetInitSize,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxSizerItem(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetMinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizerItem(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetMinSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxSizerItem(), Proportion::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetproportion">external documentation</a>.
setProportion(#wx_ref{type=ThisT,ref=ThisRef},Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetProportion,
  <<ThisRef:32/?UI,Proportion:32/?UI>>).

%% @spec (This::wxSizerItem(),X::float()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setRatio(This::wxSizerItem(), Ratio::float()) -> ok </c>
%% </p>
%% <p><c>
%% setRatio(This::wxSizerItem(), Size::{W::integer(),H::integer()}) -> ok </c>
%% </p>
setRatio(#wx_ref{type=ThisT,ref=ThisRef},Ratio)
 when is_float(Ratio) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_1_0,
  <<ThisRef:32/?UI,Ratio:32/?F>>);
setRatio(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_1_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizerItem(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
setRatio(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxSizerItem(), Sizer::wxSizer:wxSizer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetsizer">external documentation</a>.
setSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(SizerT,wxSizer),
  wxe_util:cast(?wxSizerItem_SetSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI>>).

%% @spec (This::wxSizerItem(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetspacer">external documentation</a>.
setSpacer(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetSpacer_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizerItem(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetspacer">external documentation</a>.
setSpacer(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetSpacer_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxSizerItem(), Window::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemsetwindow">external documentation</a>.
setWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizerItem_SetWindow,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizerItem(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizeritem.html#wxsizeritemshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_Show,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxSizerItem()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
