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

-module(wxSizerItem).
-include("wxe.hrl").
-export([assignSizer/2,assignSpacer/2,assignSpacer/3,assignWindow/2,calcMin/1,
  deleteWindows/1,destroy/1,detachSizer/1,getBorder/1,getFlag/1,getMinSize/1,
  getPosition/1,getProportion/1,getRatio/1,getRect/1,getSize/1,getSizer/1,
  getSpacer/1,getUserData/1,getWindow/1,isShown/1,isSizer/1,isSpacer/1,
  isWindow/1,new/1,new/2,new/3,setBorder/2,setDimension/3,setFlag/2,setInitSize/3,
  setMinSize/2,setMinSize/3,setProportion/2,setRatio/2,setRatio/3,show/2]).

%% inherited exports
-export([parent_class/1]).

-type wxSizerItem() :: wx:wx_object().
-export_type([wxSizerItem/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Window, [])
-spec new(Window) -> wxSizerItem() when
	Window::wxWindow:wxWindow() | wxSizer:wxSizer().

new(Window)
 when is_record(Window, wx_ref) ->
  new(Window, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
%% <br /> Also:<br />
%% new(Window, Flags) -> wxSizerItem() when<br />
%% 	Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();<br />
%%       (Window, [Option]) -> wxSizerItem() when<br />
%% 	Window::wxWindow:wxWindow() | wxSizer:wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()}.<br />
%% 
-spec new(Width, Height) -> wxSizerItem() when
	Width::integer(), Height::integer();
      (Window, Flags) -> wxSizerItem() when
	Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();
      (Window, [Option]) -> wxSizerItem() when
	Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []);
new(#wx_ref{type=WindowT}=Window,#wx_ref{type=FlagsT}=Flags) ->
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   ?CLASS(FlagsT,wxSizerFlags),   wxWindow;
    IswxSizer ->   ?CLASS(FlagsT,wxSizerFlags),   wxSizer;
    true -> error({badarg, WindowT})
  end,
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(wx:typeCast(Window, WindowType),Flags,?get_env(),?wxSizerItem_new_2_0),
  wxe_util:rec(?wxSizerItem_new_2_0);
new(#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizerItem_new_2_1),
  wxe_util:rec(?wxSizerItem_new_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemwxsizeritem">external documentation</a>.
-spec new(Width, Height, [Option]) -> wxSizerItem() when
	Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Width,Height, Opts,?get_env(),?wxSizerItem_new_3),
  wxe_util:rec(?wxSizerItem_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemcalcmin">external documentation</a>.
-spec calcMin(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
calcMin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_CalcMin),
  wxe_util:rec(?wxSizerItem_CalcMin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemdeletewindows">external documentation</a>.
-spec deleteWindows(This) -> 'ok' when
	This::wxSizerItem().
deleteWindows(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_DeleteWindows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemdetachsizer">external documentation</a>.
-spec detachSizer(This) -> 'ok' when
	This::wxSizerItem().
detachSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_DetachSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetborder">external documentation</a>.
-spec getBorder(This) -> integer() when
	This::wxSizerItem().
getBorder(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetBorder),
  wxe_util:rec(?wxSizerItem_GetBorder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetflag">external documentation</a>.
-spec getFlag(This) -> integer() when
	This::wxSizerItem().
getFlag(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetFlag),
  wxe_util:rec(?wxSizerItem_GetFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getMinSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetMinSize),
  wxe_util:rec(?wxSizerItem_GetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxSizerItem().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetPosition),
  wxe_util:rec(?wxSizerItem_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetproportion">external documentation</a>.
-spec getProportion(This) -> integer() when
	This::wxSizerItem().
getProportion(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetProportion),
  wxe_util:rec(?wxSizerItem_GetProportion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetratio">external documentation</a>.
-spec getRatio(This) -> number() when
	This::wxSizerItem().
getRatio(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetRatio),
  wxe_util:rec(?wxSizerItem_GetRatio).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetrect">external documentation</a>.
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxSizerItem().
getRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetRect),
  wxe_util:rec(?wxSizerItem_GetRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetSize),
  wxe_util:rec(?wxSizerItem_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetsizer">external documentation</a>.
-spec getSizer(This) -> wxSizer:wxSizer() when
	This::wxSizerItem().
getSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetSizer),
  wxe_util:rec(?wxSizerItem_GetSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetspacer">external documentation</a>.
-spec getSpacer(This) -> {W::integer(), H::integer()} when
	This::wxSizerItem().
getSpacer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetSpacer),
  wxe_util:rec(?wxSizerItem_GetSpacer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetuserdata">external documentation</a>.
-spec getUserData(This) -> wx:wx_object() when
	This::wxSizerItem().
getUserData(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetUserData),
  wxe_util:rec(?wxSizerItem_GetUserData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemgetwindow">external documentation</a>.
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxSizerItem().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_GetWindow),
  wxe_util:rec(?wxSizerItem_GetWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemissizer">external documentation</a>.
-spec isSizer(This) -> boolean() when
	This::wxSizerItem().
isSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_IsSizer),
  wxe_util:rec(?wxSizerItem_IsSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxSizerItem().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_IsShown),
  wxe_util:rec(?wxSizerItem_IsShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemisspacer">external documentation</a>.
-spec isSpacer(This) -> boolean() when
	This::wxSizerItem().
isSpacer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_IsSpacer),
  wxe_util:rec(?wxSizerItem_IsSpacer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemiswindow">external documentation</a>.
-spec isWindow(This) -> boolean() when
	This::wxSizerItem().
isWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerItem_IsWindow),
  wxe_util:rec(?wxSizerItem_IsWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetborder">external documentation</a>.
-spec setBorder(This, Border) -> 'ok' when
	This::wxSizerItem(), Border::integer().
setBorder(#wx_ref{type=ThisT}=This,Border)
 when is_integer(Border) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Border,?get_env(),?wxSizerItem_SetBorder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetdimension">external documentation</a>.
-spec setDimension(This, Pos, Size) -> 'ok' when
	This::wxSizerItem(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}.
setDimension(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos,{SizeW,SizeH} = Size)
 when is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Pos,Size,?get_env(),?wxSizerItem_SetDimension).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetflag">external documentation</a>.
-spec setFlag(This, Flag) -> 'ok' when
	This::wxSizerItem(), Flag::integer().
setFlag(#wx_ref{type=ThisT}=This,Flag)
 when is_integer(Flag) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Flag,?get_env(),?wxSizerItem_SetFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetinitsize">external documentation</a>.
-spec setInitSize(This, X, Y) -> 'ok' when
	This::wxSizerItem(), X::integer(), Y::integer().
setInitSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxSizerItem_SetInitSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
-spec setMinSize(This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizerItem_SetMinSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetminsize">external documentation</a>.
-spec setMinSize(This, X, Y) -> 'ok' when
	This::wxSizerItem(), X::integer(), Y::integer().
setMinSize(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxSizerItem_SetMinSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetproportion">external documentation</a>.
-spec setProportion(This, Proportion) -> 'ok' when
	This::wxSizerItem(), Proportion::integer().
setProportion(#wx_ref{type=ThisT}=This,Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Proportion,?get_env(),?wxSizerItem_SetProportion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
%% <br /> Also:<br />
%% setRatio(This, Size) -> 'ok' when<br />
%% 	This::wxSizerItem(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setRatio(This, Ratio) -> 'ok' when
	This::wxSizerItem(), Ratio::number();
      (This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
setRatio(#wx_ref{type=ThisT}=This,Ratio)
 when is_number(Ratio) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Ratio,?get_env(),?wxSizerItem_SetRatio_1_0);
setRatio(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizerItem_SetRatio_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemsetratio">external documentation</a>.
-spec setRatio(This, Width, Height) -> 'ok' when
	This::wxSizerItem(), Width::integer(), Height::integer().
setRatio(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxSizerItem_SetRatio_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemassignsizer">external documentation</a>.
-spec assignSizer(This, Sizer) -> 'ok' when
	This::wxSizerItem(), Sizer::wxSizer:wxSizer().
assignSizer(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(SizerT,wxSizer),
  wxe_util:queue_cmd(This,Sizer,?get_env(),?wxSizerItem_AssignSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemassignspacer">external documentation</a>.
-spec assignSpacer(This, Size) -> 'ok' when
	This::wxSizerItem(), Size::{W::integer(), H::integer()}.
assignSpacer(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizerItem_AssignSpacer_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemassignspacer">external documentation</a>.
-spec assignSpacer(This, W, H) -> 'ok' when
	This::wxSizerItem(), W::integer(), H::integer().
assignSpacer(#wx_ref{type=ThisT}=This,W,H)
 when is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,W,H,?get_env(),?wxSizerItem_AssignSpacer_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemassignwindow">external documentation</a>.
-spec assignWindow(This, Window) -> 'ok' when
	This::wxSizerItem(), Window::wxWindow:wxWindow().
assignWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizerItem),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSizerItem_AssignWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeritem.html#wxsizeritemshow">external documentation</a>.
-spec show(This, Show) -> 'ok' when
	This::wxSizerItem(), Show::boolean().
show(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizerItem),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxSizerItem_Show).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxSizerItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerItem),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
