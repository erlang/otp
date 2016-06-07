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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html">wxSizerItem</a>.
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

-export_type([wxSizerItem/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxSizerItem() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new() -> wxSizerItem().
new() ->
  wxe_util:construct(?wxSizerItem_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new(Window, Flags) -> wxSizerItem() when
	Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new(Width, Height, Flags) -> wxSizerItem() when
	Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().
new(Width,Height,#wx_ref{type=FlagsT,ref=FlagsRef})
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:construct(?wxSizerItem_new_3,
  <<Width:32/?UI,Height:32/?UI,FlagsRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new(Window, Proportion, Flag, Border, UserData) -> wxSizerItem() when
	Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Proportion::integer(), Flag::integer(), Border::integer(), UserData::wx:wx_object().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new(Width, Height, Proportion, Flag, Border, UserData) -> wxSizerItem() when
	Width::integer(), Height::integer(), Proportion::integer(), Flag::integer(), Border::integer(), UserData::wx:wx_object().
new(Width,Height,Proportion,Flag,Border,#wx_ref{type=UserDataT,ref=UserDataRef})
 when is_integer(Width),is_integer(Height),is_integer(Proportion),is_integer(Flag),is_integer(Border) ->
  ?CLASS(UserDataT,wx),
  wxe_util:construct(?wxSizerItem_new_6,
  <<Width:32/?UI,Height:32/?UI,Proportion:32/?UI,Flag:32/?UI,Border:32/?UI,UserDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemcalcmin">external documentation</a>.
-spec calcMin(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
calcMin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_CalcMin,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemdeletewindows">external documentation</a>.
-spec deleteWindows(This) -> 'ok' when
	This::wxSizerItem().
deleteWindows(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_DeleteWindows,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemdetachsizer">external documentation</a>.
-spec detachSizer(This) -> 'ok' when
	This::wxSizerItem().
detachSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_DetachSizer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetborder">external documentation</a>.
-spec getBorder(This) -> integer() when
	This::wxSizerItem().
getBorder(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetBorder,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetflag">external documentation</a>.
-spec getFlag(This) -> integer() when
	This::wxSizerItem().
getFlag(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetFlag,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxSizerItem().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetproportion">external documentation</a>.
-spec getProportion(This) -> integer() when
	This::wxSizerItem().
getProportion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetProportion,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetratio">external documentation</a>.
-spec getRatio(This) -> number() when
	This::wxSizerItem().
getRatio(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetRatio,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetrect">external documentation</a>.
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxSizerItem().
getRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetsizer">external documentation</a>.
-spec getSizer(This) -> wxSizer:wxSizer() when
	This::wxSizerItem().
getSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSizer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetspacer">external documentation</a>.
-spec getSpacer(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getSpacer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetSpacer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetuserdata">external documentation</a>.
-spec getUserData(This) -> wx:wx_object() when
	This::wxSizerItem().
getUserData(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetUserData,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetwindow">external documentation</a>.
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxSizerItem().
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_GetWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemissizer">external documentation</a>.
-spec isSizer(This) -> boolean() when
	This::wxSizerItem().
isSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsSizer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxSizerItem().
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsShown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemisspacer">external documentation</a>.
-spec isSpacer(This) -> boolean() when
	This::wxSizerItem().
isSpacer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsSpacer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemiswindow">external documentation</a>.
-spec isWindow(This) -> boolean() when
	This::wxSizerItem().
isWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:call(?wxSizerItem_IsWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetborder">external documentation</a>.
-spec setBorder(This, Border) -> 'ok' when
	This::wxSizerItem(), Border::integer().
setBorder(#wx_ref{type=ThisT,ref=ThisRef},Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetBorder,
  <<ThisRef:32/?UI,Border:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetdimension">external documentation</a>.
-spec setDimension(This, Pos, Size) -> 'ok' when
	This::wxSizerItem(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}.
setDimension(#wx_ref{type=ThisT,ref=ThisRef},{PosX,PosY},{SizeW,SizeH})
 when is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetDimension,
  <<ThisRef:32/?UI,PosX:32/?UI,PosY:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetflag">external documentation</a>.
-spec setFlag(This, Flag) -> 'ok' when
	This::wxSizerItem(), Flag::integer().
setFlag(#wx_ref{type=ThisT,ref=ThisRef},Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetFlag,
  <<ThisRef:32/?UI,Flag:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetinitsize">external documentation</a>.
-spec setInitSize(This, X, Y) -> 'ok' when
	This::wxSizerItem(), X::integer(), Y::integer().
setInitSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetInitSize,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
-spec setMinSize(This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetMinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
-spec setMinSize(This, X, Y) -> 'ok' when
	This::wxSizerItem(), X::integer(), Y::integer().
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetMinSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetproportion">external documentation</a>.
-spec setProportion(This, Proportion) -> 'ok' when
	This::wxSizerItem(), Proportion::integer().
setProportion(#wx_ref{type=ThisT,ref=ThisRef},Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetProportion,
  <<ThisRef:32/?UI,Proportion:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
%% <br /> Also:<br />
%% setRatio(This, Size) -> 'ok' when<br />
%% 	This::wxSizerItem(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setRatio(This, Ratio) -> 'ok' when
	This::wxSizerItem(), Ratio::number();
      (This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
setRatio(#wx_ref{type=ThisT,ref=ThisRef},Ratio)
 when is_number(Ratio) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_1_0,
  <<ThisRef:32/?UI,Ratio:32/?F>>);
setRatio(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_1_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
-spec setRatio(This, Width, Height) -> 'ok' when
	This::wxSizerItem(), Width::integer(), Height::integer().
setRatio(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetRatio_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetsizer">external documentation</a>.
-spec setSizer(This, Sizer) -> 'ok' when
	This::wxSizerItem(), Sizer::wxSizer:wxSizer().
setSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(SizerT,wxSizer),
  wxe_util:cast(?wxSizerItem_SetSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetspacer">external documentation</a>.
-spec setSpacer(This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
setSpacer(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetSpacer_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetspacer">external documentation</a>.
-spec setSpacer(This, Width, Height) -> 'ok' when
	This::wxSizerItem(), Width::integer(), Height::integer().
setSpacer(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_SetSpacer_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetwindow">external documentation</a>.
-spec setWindow(This, Window) -> 'ok' when
	This::wxSizerItem(), Window::wxWindow:wxWindow().
setWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizerItem_SetWindow,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemshow">external documentation</a>.
-spec show(This, Show) -> 'ok' when
	This::wxSizerItem(), Show::boolean().
show(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:cast(?wxSizerItem_Show,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxSizerItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
