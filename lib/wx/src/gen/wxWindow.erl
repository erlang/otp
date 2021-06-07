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

-module(wxWindow).
-include("wxe.hrl").
-export(['Destroy'/1,cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,convertDialogToPixels/2,convertPixelsToDialog/2,create/3,
  create/4,destroy/1,destroyChildren/1,disable/1,dragAcceptFiles/2,enable/1,
  enable/2,findFocus/0,findWindow/2,findWindowById/1,findWindowById/2,
  findWindowByLabel/1,findWindowByLabel/2,findWindowByName/1,findWindowByName/2,
  fit/1,fitInside/1,freeze/1,fromDIP/2,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCapture/0,getCaret/1,getCharHeight/1,
  getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,getThemeEnabled/1,
  getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,new/0,new/2,new/3,pageDown/1,pageUp/1,popupMenu/2,
  popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,screenToClient/2,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,toDIP/2,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxWindow() :: wx:wx_object().
-export_type([wxWindow/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
-spec new() -> wxWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_new_0),
  wxe_util:rec(?wxWindow_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxWindow() when
	Parent::wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
-spec new(Parent, Id, [Option]) -> wxWindow() when
	Parent::wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxWindow_new_3),
  wxe_util:rec(?wxWindow_new_3).

%% @equiv create(This,Parent,Id, [])
-spec create(This, Parent, Id) -> boolean() when
	This::wxWindow(), Parent::wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcreate">external documentation</a>.
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxWindow(), Parent::wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxWindow_Create),
  wxe_util:rec(?wxWindow_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcachebestsize">external documentation</a>.
-spec cacheBestSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
cacheBestSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_CacheBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcapturemouse">external documentation</a>.
-spec captureMouse(This) -> 'ok' when
	This::wxWindow().
captureMouse(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_CaptureMouse).

%% @equiv center(This, [])
-spec center(This) -> 'ok' when
	This::wxWindow().

center(This)
 when is_record(This, wx_ref) ->
  center(This, []).

%% @equiv centre(This, [])
-spec centre(This) -> 'ok' when
	This::wxWindow().

centre(This)
 when is_record(This, wx_ref) ->
  centre(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
-spec center(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.

center(This, Options)
 when is_record(This, wx_ref),is_list(Options) ->
  centre(This, Options).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
-spec centre(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centre(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, _dir} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Centre).

%% @equiv centerOnParent(This, [])
-spec centerOnParent(This) -> 'ok' when
	This::wxWindow().

centerOnParent(This)
 when is_record(This, wx_ref) ->
  centerOnParent(This, []).

%% @equiv centreOnParent(This, [])
-spec centreOnParent(This) -> 'ok' when
	This::wxWindow().

centreOnParent(This)
 when is_record(This, wx_ref) ->
  centreOnParent(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
-spec centerOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.

centerOnParent(This, Options)
 when is_record(This, wx_ref),is_list(Options) ->
  centreOnParent(This, Options).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
-spec centreOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centreOnParent(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, _dir} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_CentreOnParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclearbackground">external documentation</a>.
-spec clearBackground(This) -> 'ok' when
	This::wxWindow().
clearBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ClearBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-spec clientToScreen(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
clientToScreen(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_ClientToScreen_1),
  wxe_util:rec(?wxWindow_ClientToScreen_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-spec clientToScreen(This, X, Y) -> {X::integer(), Y::integer()} when
	This::wxWindow(), X::integer(), Y::integer().
clientToScreen(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_ClientToScreen_2),
  wxe_util:rec(?wxWindow_ClientToScreen_2).

%% @equiv close(This, [])
-spec close(This) -> boolean() when
	This::wxWindow().

close(This)
 when is_record(This, wx_ref) ->
  close(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclose">external documentation</a>.
-spec close(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'force', boolean()}.
close(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({force, _force} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Close),
  wxe_util:rec(?wxWindow_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertdialogtopixels">external documentation</a>.
-spec convertDialogToPixels(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertDialogToPixels(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ConvertDialogToPixels),
  wxe_util:rec(?wxWindow_ConvertDialogToPixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertpixelstodialog">external documentation</a>.
-spec convertPixelsToDialog(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertPixelsToDialog(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ConvertPixelsToDialog),
  wxe_util:rec(?wxWindow_ConvertPixelsToDialog).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroy">external documentation</a>.
-spec 'Destroy'(This) -> boolean() when
	This::wxWindow().
'Destroy'(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Destroy),
  wxe_util:rec(?wxWindow_Destroy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroychildren">external documentation</a>.
-spec destroyChildren(This) -> boolean() when
	This::wxWindow().
destroyChildren(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_DestroyChildren),
  wxe_util:rec(?wxWindow_DestroyChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdisable">external documentation</a>.
-spec disable(This) -> boolean() when
	This::wxWindow().
disable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Disable),
  wxe_util:rec(?wxWindow_Disable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdragacceptfiles">external documentation</a>.
-spec dragAcceptFiles(This, Accept) -> 'ok' when
	This::wxWindow(), Accept::boolean().
dragAcceptFiles(#wx_ref{type=ThisT}=This,Accept)
 when is_boolean(Accept) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Accept,?get_env(),?wxWindow_DragAcceptFiles).

%% @equiv enable(This, [])
-spec enable(This) -> boolean() when
	This::wxWindow().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowenable">external documentation</a>.
-spec enable(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'enable', boolean()}.
enable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Enable),
  wxe_util:rec(?wxWindow_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindfocus">external documentation</a>.
-spec findFocus() -> wxWindow().
findFocus() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_FindFocus),
  wxe_util:rec(?wxWindow_FindFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindow">external documentation</a>.
%% <br /> Also:<br />
%% findWindow(This, Name) -> wxWindow() when<br />
%% 	This::wxWindow(), Name::unicode:chardata().<br />
%% 
-spec findWindow(This, Id) -> wxWindow() when
	This::wxWindow(), Id::integer();
      (This, Name) -> wxWindow() when
	This::wxWindow(), Name::unicode:chardata().
findWindow(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxWindow_FindWindow_1_0),
  wxe_util:rec(?wxWindow_FindWindow_1_0);
findWindow(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxWindow_FindWindow_1_1),
  wxe_util:rec(?wxWindow_FindWindow_1_1).

%% @equiv findWindowById(Id, [])
-spec findWindowById(Id) -> wxWindow() when
	Id::integer().

findWindowById(Id)
 when is_integer(Id) ->
  findWindowById(Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyid">external documentation</a>.
-spec findWindowById(Id, [Option]) -> wxWindow() when
	Id::integer(),
	Option :: {'parent', wxWindow()}.
findWindowById(Id, Options)
 when is_integer(Id),is_list(Options) ->
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id, Opts,?get_env(),?wxWindow_FindWindowById),
  wxe_util:rec(?wxWindow_FindWindowById).

%% @equiv findWindowByName(Name, [])
-spec findWindowByName(Name) -> wxWindow() when
	Name::unicode:chardata().

findWindowByName(Name)
 when ?is_chardata(Name) ->
  findWindowByName(Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyname">external documentation</a>.
-spec findWindowByName(Name, [Option]) -> wxWindow() when
	Name::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByName(Name, Options)
 when ?is_chardata(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary(Name),
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Name_UC, Opts,?get_env(),?wxWindow_FindWindowByName),
  wxe_util:rec(?wxWindow_FindWindowByName).

%% @equiv findWindowByLabel(Label, [])
-spec findWindowByLabel(Label) -> wxWindow() when
	Label::unicode:chardata().

findWindowByLabel(Label)
 when ?is_chardata(Label) ->
  findWindowByLabel(Label, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbylabel">external documentation</a>.
-spec findWindowByLabel(Label, [Option]) -> wxWindow() when
	Label::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByLabel(Label, Options)
 when ?is_chardata(Label),is_list(Options) ->
  Label_UC = unicode:characters_to_binary(Label),
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Label_UC, Opts,?get_env(),?wxWindow_FindWindowByLabel),
  wxe_util:rec(?wxWindow_FindWindowByLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfit">external documentation</a>.
-spec fit(This) -> 'ok' when
	This::wxWindow().
fit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Fit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfitinside">external documentation</a>.
-spec fitInside(This) -> 'ok' when
	This::wxWindow().
fitInside(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_FitInside).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfreeze">external documentation</a>.
-spec freeze(This) -> 'ok' when
	This::wxWindow().
freeze(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Freeze).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetacceleratortable">external documentation</a>.
-spec getAcceleratorTable(This) -> wxAcceleratorTable:wxAcceleratorTable() when
	This::wxWindow().
getAcceleratorTable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetAcceleratorTable),
  wxe_util:rec(?wxWindow_GetAcceleratorTable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBackgroundColour),
  wxe_util:rec(?wxWindow_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundstyle">external documentation</a>.
%%<br /> Res = ?wxBG_STYLE_ERASE | ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_PAINT | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_TRANSPARENT
-spec getBackgroundStyle(This) -> wx:wx_enum() when
	This::wxWindow().
getBackgroundStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBackgroundStyle),
  wxe_util:rec(?wxWindow_GetBackgroundStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbestsize">external documentation</a>.
-spec getBestSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getBestSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetBestSize),
  wxe_util:rec(?wxWindow_GetBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcaret">external documentation</a>.
-spec getCaret(This) -> wxCaret:wxCaret() when
	This::wxWindow().
getCaret(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCaret),
  wxe_util:rec(?wxWindow_GetCaret).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcapture">external documentation</a>.
-spec getCapture() -> wxWindow().
getCapture() ->
  wxe_util:queue_cmd(?get_env(), ?wxWindow_GetCapture),
  wxe_util:rec(?wxWindow_GetCapture).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharheight">external documentation</a>.
-spec getCharHeight(This) -> integer() when
	This::wxWindow().
getCharHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCharHeight),
  wxe_util:rec(?wxWindow_GetCharHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharwidth">external documentation</a>.
-spec getCharWidth(This) -> integer() when
	This::wxWindow().
getCharWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCharWidth),
  wxe_util:rec(?wxWindow_GetCharWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetchildren">external documentation</a>.
-spec getChildren(This) -> [wxWindow()] when
	This::wxWindow().
getChildren(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetChildren),
  wxe_util:rec(?wxWindow_GetChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetclientsize">external documentation</a>.
-spec getClientSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getClientSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetClientSize),
  wxe_util:rec(?wxWindow_GetClientSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcontainingsizer">external documentation</a>.
-spec getContainingSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getContainingSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetContainingSizer),
  wxe_util:rec(?wxWindow_GetContainingSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcursor">external documentation</a>.
-spec getCursor(This) -> wxCursor:wxCursor() when
	This::wxWindow().
getCursor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetCursor),
  wxe_util:rec(?wxWindow_GetCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdroptarget">external documentation</a>.
-spec getDropTarget(This) -> wx:wx_object() when
	This::wxWindow().
getDropTarget(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDropTarget),
  wxe_util:rec(?wxWindow_GetDropTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdpiscalefactor">external documentation</a>.
-spec getDPIScaleFactor(This) -> number() when
	This::wxWindow().
getDPIScaleFactor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDPIScaleFactor),
  wxe_util:rec(?wxWindow_GetDPIScaleFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetextrastyle">external documentation</a>.
-spec getExtraStyle(This) -> integer() when
	This::wxWindow().
getExtraStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetExtraStyle),
  wxe_util:rec(?wxWindow_GetExtraStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxWindow().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetFont),
  wxe_util:rec(?wxWindow_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetforegroundcolour">external documentation</a>.
-spec getForegroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getForegroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetForegroundColour),
  wxe_util:rec(?wxWindow_GetForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetgrandparent">external documentation</a>.
-spec getGrandParent(This) -> wxWindow() when
	This::wxWindow().
getGrandParent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetGrandParent),
  wxe_util:rec(?wxWindow_GetGrandParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethandle">external documentation</a>.
-spec getHandle(This) -> integer() when
	This::wxWindow().
getHandle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetHandle),
  wxe_util:rec(?wxWindow_GetHandle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethelptext">external documentation</a>.
-spec getHelpText(This) -> unicode:charlist() when
	This::wxWindow().
getHelpText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetHelpText),
  wxe_util:rec(?wxWindow_GetHelpText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxWindow().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetId),
  wxe_util:rec(?wxWindow_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxWindow().
getLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetLabel),
  wxe_util:rec(?wxWindow_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetmaxsize">external documentation</a>.
-spec getMaxSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMaxSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetMaxSize),
  wxe_util:rec(?wxWindow_GetMaxSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMinSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetMinSize),
  wxe_util:rec(?wxWindow_GetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetname">external documentation</a>.
-spec getName(This) -> unicode:charlist() when
	This::wxWindow().
getName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetName),
  wxe_util:rec(?wxWindow_GetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetparent">external documentation</a>.
-spec getParent(This) -> wxWindow() when
	This::wxWindow().
getParent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetParent),
  wxe_util:rec(?wxWindow_GetParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetPosition),
  wxe_util:rec(?wxWindow_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetrect">external documentation</a>.
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetRect),
  wxe_util:rec(?wxWindow_GetRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenposition">external documentation</a>.
-spec getScreenPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getScreenPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetScreenPosition),
  wxe_util:rec(?wxWindow_GetScreenPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenrect">external documentation</a>.
-spec getScreenRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getScreenRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetScreenRect),
  wxe_util:rec(?wxWindow_GetScreenRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollpos">external documentation</a>.
-spec getScrollPos(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollPos(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollPos),
  wxe_util:rec(?wxWindow_GetScrollPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollrange">external documentation</a>.
-spec getScrollRange(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollRange(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollRange),
  wxe_util:rec(?wxWindow_GetScrollRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollthumb">external documentation</a>.
-spec getScrollThumb(This, Orientation) -> integer() when
	This::wxWindow(), Orientation::integer().
getScrollThumb(#wx_ref{type=ThisT}=This,Orientation)
 when is_integer(Orientation) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orientation,?get_env(),?wxWindow_GetScrollThumb),
  wxe_util:rec(?wxWindow_GetScrollThumb).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetSize),
  wxe_util:rec(?wxWindow_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsizer">external documentation</a>.
-spec getSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getSizer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetSizer),
  wxe_util:rec(?wxWindow_GetSizer).

%% @equiv getTextExtent(This,String, [])
-spec getTextExtent(This, String) -> Result when
	Result ::{W::integer(), H::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata().

getTextExtent(This,String)
 when is_record(This, wx_ref),?is_chardata(String) ->
  getTextExtent(This,String, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettextextent">external documentation</a>.
-spec getTextExtent(This, String, [Option]) -> Result when
	Result :: {W::integer(), H::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata(),
	Option :: {'theFont', wxFont:wxFont()}.
getTextExtent(#wx_ref{type=ThisT}=This,String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  String_UC = unicode:characters_to_binary(String),
  MOpts = fun({theFont, #wx_ref{type=TheFontT}} = Arg) ->   ?CLASS(TheFontT,wxFont),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,String_UC, Opts,?get_env(),?wxWindow_GetTextExtent),
  wxe_util:rec(?wxWindow_GetTextExtent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetthemeenabled">external documentation</a>.
-spec getThemeEnabled(This) -> boolean() when
	This::wxWindow().
getThemeEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetThemeEnabled),
  wxe_util:rec(?wxWindow_GetThemeEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettooltip">external documentation</a>.
-spec getToolTip(This) -> wxToolTip:wxToolTip() when
	This::wxWindow().
getToolTip(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetToolTip),
  wxe_util:rec(?wxWindow_GetToolTip).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetupdateregion">external documentation</a>.
-spec getUpdateRegion(This) -> wxRegion:wxRegion() when
	This::wxWindow().
getUpdateRegion(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetUpdateRegion),
  wxe_util:rec(?wxWindow_GetUpdateRegion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetvirtualsize">external documentation</a>.
-spec getVirtualSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getVirtualSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetVirtualSize),
  wxe_util:rec(?wxWindow_GetVirtualSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowstyleflag">external documentation</a>.
-spec getWindowStyleFlag(This) -> integer() when
	This::wxWindow().
getWindowStyleFlag(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetWindowStyleFlag),
  wxe_util:rec(?wxWindow_GetWindowStyleFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowvariant">external documentation</a>.
%%<br /> Res = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-spec getWindowVariant(This) -> wx:wx_enum() when
	This::wxWindow().
getWindowVariant(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetWindowVariant),
  wxe_util:rec(?wxWindow_GetWindowVariant).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhascapture">external documentation</a>.
-spec hasCapture(This) -> boolean() when
	This::wxWindow().
hasCapture(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_HasCapture),
  wxe_util:rec(?wxWindow_HasCapture).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhasscrollbar">external documentation</a>.
-spec hasScrollbar(This, Orient) -> boolean() when
	This::wxWindow(), Orient::integer().
hasScrollbar(#wx_ref{type=ThisT}=This,Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Orient,?get_env(),?wxWindow_HasScrollbar),
  wxe_util:rec(?wxWindow_HasScrollbar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhastransparentbackground">external documentation</a>.
-spec hasTransparentBackground(This) -> boolean() when
	This::wxWindow().
hasTransparentBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_HasTransparentBackground),
  wxe_util:rec(?wxWindow_HasTransparentBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhide">external documentation</a>.
-spec hide(This) -> boolean() when
	This::wxWindow().
hide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Hide),
  wxe_util:rec(?wxWindow_Hide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinheritattributes">external documentation</a>.
-spec inheritAttributes(This) -> 'ok' when
	This::wxWindow().
inheritAttributes(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InheritAttributes).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinitdialog">external documentation</a>.
-spec initDialog(This) -> 'ok' when
	This::wxWindow().
initDialog(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InitDialog).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinvalidatebestsize">external documentation</a>.
-spec invalidateBestSize(This) -> 'ok' when
	This::wxWindow().
invalidateBestSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_InvalidateBestSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisfrozen">external documentation</a>.
-spec isFrozen(This) -> boolean() when
	This::wxWindow().
isFrozen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsFrozen),
  wxe_util:rec(?wxWindow_IsFrozen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisenabled">external documentation</a>.
-spec isEnabled(This) -> boolean() when
	This::wxWindow().
isEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsEnabled),
  wxe_util:rec(?wxWindow_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
%% <br /> Also:<br />
%% isExposed(This, Rect) -> boolean() when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec isExposed(This, Pt) -> boolean() when
	This::wxWindow(), Pt::{X::integer(), Y::integer()};
      (This, Rect) -> boolean() when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
isExposed(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_IsExposed_1_0),
  wxe_util:rec(?wxWindow_IsExposed_1_0);
isExposed(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxWindow_IsExposed_1_1),
  wxe_util:rec(?wxWindow_IsExposed_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-spec isExposed(This, X, Y) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer().
isExposed(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_IsExposed_2),
  wxe_util:rec(?wxWindow_IsExposed_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-spec isExposed(This, X, Y, W, H) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer(), W::integer(), H::integer().
isExposed(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxWindow_IsExposed_4),
  wxe_util:rec(?wxWindow_IsExposed_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisretained">external documentation</a>.
-spec isRetained(This) -> boolean() when
	This::wxWindow().
isRetained(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsRetained),
  wxe_util:rec(?wxWindow_IsRetained).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxWindow().
isShown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsShown),
  wxe_util:rec(?wxWindow_IsShown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowistoplevel">external documentation</a>.
-spec isTopLevel(This) -> boolean() when
	This::wxWindow().
isTopLevel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsTopLevel),
  wxe_util:rec(?wxWindow_IsTopLevel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisshownonscreen">external documentation</a>.
-spec isShownOnScreen(This) -> boolean() when
	This::wxWindow().
isShownOnScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsShownOnScreen),
  wxe_util:rec(?wxWindow_IsShownOnScreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlayout">external documentation</a>.
-spec layout(This) -> boolean() when
	This::wxWindow().
layout(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Layout),
  wxe_util:rec(?wxWindow_Layout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlinedown">external documentation</a>.
-spec lineDown(This) -> boolean() when
	This::wxWindow().
lineDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_LineDown),
  wxe_util:rec(?wxWindow_LineDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlineup">external documentation</a>.
-spec lineUp(This) -> boolean() when
	This::wxWindow().
lineUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_LineUp),
  wxe_util:rec(?wxWindow_LineUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlower">external documentation</a>.
-spec lower(This) -> 'ok' when
	This::wxWindow().
lower(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Lower).

%% @equiv move(This,Pt, [])
-spec move(This, Pt) -> 'ok' when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.

move(This,{PtX,PtY} = Pt)
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY) ->
  move(This,Pt, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmove">external documentation</a>.
%% <br /> Also:<br />
%% move(This, Pt, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), Pt::{X::integer(), Y::integer()},<br />
%% 	Option :: {'flags', integer()}.<br />
%% 
-spec move(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer();
      (This, Pt, [Option]) -> 'ok' when
	This::wxWindow(), Pt::{X::integer(), Y::integer()},
	Option :: {'flags', integer()}.

move(This,X,Y)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y) ->
  move(This,X,Y, []);
move(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pt, Opts,?get_env(),?wxWindow_Move_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmove">external documentation</a>.
-spec move(This, X, Y, [Option]) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(),
	Option :: {'flags', integer()}.
move(#wx_ref{type=ThisT}=This,X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y, Opts,?get_env(),?wxWindow_Move_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmoveafterintaborder">external documentation</a>.
-spec moveAfterInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveAfterInTabOrder(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxWindow_MoveAfterInTabOrder).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmovebeforeintaborder">external documentation</a>.
-spec moveBeforeInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveBeforeInTabOrder(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxWindow_MoveBeforeInTabOrder).

%% @equiv navigate(This, [])
-spec navigate(This) -> boolean() when
	This::wxWindow().

navigate(This)
 when is_record(This, wx_ref) ->
  navigate(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindownavigate">external documentation</a>.
-spec navigate(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'flags', integer()}.
navigate(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Navigate),
  wxe_util:rec(?wxWindow_Navigate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpagedown">external documentation</a>.
-spec pageDown(This) -> boolean() when
	This::wxWindow().
pageDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_PageDown),
  wxe_util:rec(?wxWindow_PageDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpageup">external documentation</a>.
-spec pageUp(This) -> boolean() when
	This::wxWindow().
pageUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_PageUp),
  wxe_util:rec(?wxWindow_PageUp).

%% @equiv popupMenu(This,Menu, [])
-spec popupMenu(This, Menu) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu().

popupMenu(This,Menu)
 when is_record(This, wx_ref),is_record(Menu, wx_ref) ->
  popupMenu(This,Menu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
-spec popupMenu(This, Menu, [Option]) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu(),
	Option :: {'pos', {X::integer(), Y::integer()}}.
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Menu, Opts,?get_env(),?wxWindow_PopupMenu_2),
  wxe_util:rec(?wxWindow_PopupMenu_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
-spec popupMenu(This, Menu, X, Y) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu(), X::integer(), Y::integer().
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,X,Y,?get_env(),?wxWindow_PopupMenu_3),
  wxe_util:rec(?wxWindow_PopupMenu_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowraise">external documentation</a>.
-spec raise(This) -> 'ok' when
	This::wxWindow().
raise(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Raise).

%% @equiv refresh(This, [])
-spec refresh(This) -> 'ok' when
	This::wxWindow().

refresh(This)
 when is_record(This, wx_ref) ->
  refresh(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowrefresh">external documentation</a>.
-spec refresh(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'eraseBackground', boolean()}
		 | {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
refresh(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, _eraseBackground} = Arg) -> Arg;
          ({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Refresh).

%% @equiv refreshRect(This,Rect, [])
-spec refreshRect(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.

refreshRect(This,{RectX,RectY,RectW,RectH} = Rect)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  refreshRect(This,Rect, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowrefreshrect">external documentation</a>.
-spec refreshRect(This, Rect, [Option]) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'eraseBackground', boolean()}.
refreshRect(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, _eraseBackground} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Rect, Opts,?get_env(),?wxWindow_RefreshRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreleasemouse">external documentation</a>.
-spec releaseMouse(This) -> 'ok' when
	This::wxWindow().
releaseMouse(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ReleaseMouse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowremovechild">external documentation</a>.
-spec removeChild(This, Child) -> 'ok' when
	This::wxWindow(), Child::wxWindow().
removeChild(#wx_ref{type=ThisT}=This,#wx_ref{type=ChildT}=Child) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ChildT,wxWindow),
  wxe_util:queue_cmd(This,Child,?get_env(),?wxWindow_RemoveChild).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreparent">external documentation</a>.
-spec reparent(This, NewParent) -> boolean() when
	This::wxWindow(), NewParent::wxWindow().
reparent(#wx_ref{type=ThisT}=This,#wx_ref{type=NewParentT}=NewParent) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(NewParentT,wxWindow),
  wxe_util:queue_cmd(This,NewParent,?get_env(),?wxWindow_Reparent),
  wxe_util:rec(?wxWindow_Reparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-spec screenToClient(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
screenToClient(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ScreenToClient_2),
  wxe_util:rec(?wxWindow_ScreenToClient_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-spec screenToClient(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
screenToClient(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxWindow_ScreenToClient_1),
  wxe_util:rec(?wxWindow_ScreenToClient_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrolllines">external documentation</a>.
-spec scrollLines(This, Lines) -> boolean() when
	This::wxWindow(), Lines::integer().
scrollLines(#wx_ref{type=ThisT}=This,Lines)
 when is_integer(Lines) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Lines,?get_env(),?wxWindow_ScrollLines),
  wxe_util:rec(?wxWindow_ScrollLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrollpages">external documentation</a>.
-spec scrollPages(This, Pages) -> boolean() when
	This::wxWindow(), Pages::integer().
scrollPages(#wx_ref{type=ThisT}=This,Pages)
 when is_integer(Pages) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Pages,?get_env(),?wxWindow_ScrollPages),
  wxe_util:rec(?wxWindow_ScrollPages).

%% @equiv scrollWindow(This,Dx,Dy, [])
-spec scrollWindow(This, Dx, Dy) -> 'ok' when
	This::wxWindow(), Dx::integer(), Dy::integer().

scrollWindow(This,Dx,Dy)
 when is_record(This, wx_ref),is_integer(Dx),is_integer(Dy) ->
  scrollWindow(This,Dx,Dy, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrollwindow">external documentation</a>.
-spec scrollWindow(This, Dx, Dy, [Option]) -> 'ok' when
	This::wxWindow(), Dx::integer(), Dy::integer(),
	Option :: {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
scrollWindow(#wx_ref{type=ThisT}=This,Dx,Dy, Options)
 when is_integer(Dx),is_integer(Dy),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Dx,Dy, Opts,?get_env(),?wxWindow_ScrollWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetacceleratortable">external documentation</a>.
-spec setAcceleratorTable(This, Accel) -> 'ok' when
	This::wxWindow(), Accel::wxAcceleratorTable:wxAcceleratorTable().
setAcceleratorTable(#wx_ref{type=ThisT}=This,#wx_ref{type=AccelT}=Accel) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(AccelT,wxAcceleratorTable),
  wxe_util:queue_cmd(This,Accel,?get_env(),?wxWindow_SetAcceleratorTable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetautolayout">external documentation</a>.
-spec setAutoLayout(This, AutoLayout) -> 'ok' when
	This::wxWindow(), AutoLayout::boolean().
setAutoLayout(#wx_ref{type=ThisT}=This,AutoLayout)
 when is_boolean(AutoLayout) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,AutoLayout,?get_env(),?wxWindow_SetAutoLayout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetBackgroundColour),
  wxe_util:rec(?wxWindow_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundstyle">external documentation</a>.
%%<br /> Style = ?wxBG_STYLE_ERASE | ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_PAINT | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_TRANSPARENT
-spec setBackgroundStyle(This, Style) -> boolean() when
	This::wxWindow(), Style::wx:wx_enum().
setBackgroundStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetBackgroundStyle),
  wxe_util:rec(?wxWindow_SetBackgroundStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcaret">external documentation</a>.
-spec setCaret(This, Caret) -> 'ok' when
	This::wxWindow(), Caret::wxCaret:wxCaret().
setCaret(#wx_ref{type=ThisT}=This,#wx_ref{type=CaretT}=Caret) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CaretT,wxCaret),
  wxe_util:queue_cmd(This,Caret,?get_env(),?wxWindow_SetCaret).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
%% <br /> Also:<br />
%% setClientSize(This, Rect) -> 'ok' when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec setClientSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()};
      (This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setClientSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetClientSize_1_0);
setClientSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxWindow_SetClientSize_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
-spec setClientSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer().
setClientSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetClientSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcontainingsizer">external documentation</a>.
-spec setContainingSizer(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().
setContainingSizer(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  wxe_util:queue_cmd(This,Sizer,?get_env(),?wxWindow_SetContainingSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcursor">external documentation</a>.
-spec setCursor(This, Cursor) -> boolean() when
	This::wxWindow(), Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=ThisT}=This,#wx_ref{type=CursorT}=Cursor) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CursorT,wxCursor),
  wxe_util:queue_cmd(This,Cursor,?get_env(),?wxWindow_SetCursor),
  wxe_util:rec(?wxWindow_SetCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetmaxsize">external documentation</a>.
-spec setMaxSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setMaxSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetMaxSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetminsize">external documentation</a>.
-spec setMinSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownbackgroundcolour">external documentation</a>.
-spec setOwnBackgroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetOwnBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownfont">external documentation</a>.
-spec setOwnFont(This, Font) -> 'ok' when
	This::wxWindow(), Font::wxFont:wxFont().
setOwnFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxWindow_SetOwnFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownforegroundcolour">external documentation</a>.
-spec setOwnForegroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnForegroundColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetOwnForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdroptarget">external documentation</a>.
-spec setDropTarget(This, Target) -> 'ok' when
	This::wxWindow(), Target::wx:wx_object().
setDropTarget(#wx_ref{type=ThisT}=This,#wx_ref{type=TargetT}=Target) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(TargetT,wxDropTarget),
  wxe_util:queue_cmd(This,Target,?get_env(),?wxWindow_SetDropTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetextrastyle">external documentation</a>.
-spec setExtraStyle(This, ExStyle) -> 'ok' when
	This::wxWindow(), ExStyle::integer().
setExtraStyle(#wx_ref{type=ThisT}=This,ExStyle)
 when is_integer(ExStyle) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,ExStyle,?get_env(),?wxWindow_SetExtraStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocus">external documentation</a>.
-spec setFocus(This) -> 'ok' when
	This::wxWindow().
setFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_SetFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocusfromkbd">external documentation</a>.
-spec setFocusFromKbd(This) -> 'ok' when
	This::wxWindow().
setFocusFromKbd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_SetFocusFromKbd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfont">external documentation</a>.
-spec setFont(This, Font) -> boolean() when
	This::wxWindow(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxWindow_SetFont),
  wxe_util:rec(?wxWindow_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetforegroundcolour">external documentation</a>.
-spec setForegroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setForegroundColour(#wx_ref{type=ThisT}=This,Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxWindow_SetForegroundColour),
  wxe_util:rec(?wxWindow_SetForegroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsethelptext">external documentation</a>.
-spec setHelpText(This, HelpText) -> 'ok' when
	This::wxWindow(), HelpText::unicode:chardata().
setHelpText(#wx_ref{type=ThisT}=This,HelpText)
 when ?is_chardata(HelpText) ->
  ?CLASS(ThisT,wxWindow),
  HelpText_UC = unicode:characters_to_binary(HelpText),
  wxe_util:queue_cmd(This,HelpText_UC,?get_env(),?wxWindow_SetHelpText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetid">external documentation</a>.
-spec setId(This, Winid) -> 'ok' when
	This::wxWindow(), Winid::integer().
setId(#wx_ref{type=ThisT}=This,Winid)
 when is_integer(Winid) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Winid,?get_env(),?wxWindow_SetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetlabel">external documentation</a>.
-spec setLabel(This, Label) -> 'ok' when
	This::wxWindow(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxWindow),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxWindow_SetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetname">external documentation</a>.
-spec setName(This, Name) -> 'ok' when
	This::wxWindow(), Name::unicode:chardata().
setName(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxWindow_SetName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetpalette">external documentation</a>.
-spec setPalette(This, Pal) -> 'ok' when
	This::wxWindow(), Pal::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT}=This,#wx_ref{type=PalT}=Pal) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(PalT,wxPalette),
  wxe_util:queue_cmd(This,Pal,?get_env(),?wxWindow_SetPalette).

%% @equiv setScrollbar(This,Orientation,Position,ThumbSize,Range, [])
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Position::integer(), ThumbSize::integer(), Range::integer().

setScrollbar(This,Orientation,Position,ThumbSize,Range)
 when is_record(This, wx_ref),is_integer(Orientation),is_integer(Position),is_integer(ThumbSize),is_integer(Range) ->
  setScrollbar(This,Orientation,Position,ThumbSize,Range, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollbar">external documentation</a>.
-spec setScrollbar(This, Orientation, Position, ThumbSize, Range, [Option]) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Position::integer(), ThumbSize::integer(), Range::integer(),
	Option :: {'refresh', boolean()}.
setScrollbar(#wx_ref{type=ThisT}=This,Orientation,Position,ThumbSize,Range, Options)
 when is_integer(Orientation),is_integer(Position),is_integer(ThumbSize),is_integer(Range),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, _refresh} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Orientation,Position,ThumbSize,Range, Opts,?get_env(),?wxWindow_SetScrollbar).

%% @equiv setScrollPos(This,Orientation,Pos, [])
-spec setScrollPos(This, Orientation, Pos) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Pos::integer().

setScrollPos(This,Orientation,Pos)
 when is_record(This, wx_ref),is_integer(Orientation),is_integer(Pos) ->
  setScrollPos(This,Orientation,Pos, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollpos">external documentation</a>.
-spec setScrollPos(This, Orientation, Pos, [Option]) -> 'ok' when
	This::wxWindow(), Orientation::integer(), Pos::integer(),
	Option :: {'refresh', boolean()}.
setScrollPos(#wx_ref{type=ThisT}=This,Orientation,Pos, Options)
 when is_integer(Orientation),is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, _refresh} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Orientation,Pos, Opts,?get_env(),?wxWindow_SetScrollPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Also:<br />
%% setSize(This, Size) -> 'ok' when<br />
%% 	This::wxWindow(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setSize(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.

setSize(This,{RectX,RectY,RectW,RectH} = Rect)
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  setSize(This,Rect, []);
setSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Also:<br />
%% setSize(This, Rect, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},<br />
%% 	Option :: {'sizeFlags', integer()}.<br />
%% 
-spec setSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer();
      (This, Rect, [Option]) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'sizeFlags', integer()}.
setSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetSize_2_0);
setSize(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, _sizeFlags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Rect, Opts,?get_env(),?wxWindow_SetSize_2_1).

%% @equiv setSize(This,X,Y,Width,Height, [])
-spec setSize(This, X, Y, Width, Height) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer().

setSize(This,X,Y,Width,Height)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  setSize(This,X,Y,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
-spec setSize(This, X, Y, Width, Height, [Option]) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer(),
	Option :: {'sizeFlags', integer()}.
setSize(#wx_ref{type=ThisT}=This,X,Y,Width,Height, Options)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, _sizeFlags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y,Width,Height, Opts,?get_env(),?wxWindow_SetSize_5).

%% @equiv setSizeHints(This,MinSize, [])
-spec setSizeHints(This, MinSize) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()}.

setSizeHints(This,{MinSizeW,MinSizeH} = MinSize)
 when is_record(This, wx_ref),is_integer(MinSizeW),is_integer(MinSizeH) ->
  setSizeHints(This,MinSize, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
%% <br /> Also:<br />
%% setSizeHints(This, MinSize, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), MinSize::{W::integer(), H::integer()},<br />
%% 	Option :: {'maxSize', {W::integer(), H::integer()}}<br />
%% 		 | {'incSize', {W::integer(), H::integer()}}.<br />
%% 
-spec setSizeHints(This, MinW, MinH) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer();
      (This, MinSize, [Option]) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()},
	Option :: {'maxSize', {W::integer(), H::integer()}}
		 | {'incSize', {W::integer(), H::integer()}}.

setSizeHints(This,MinW,MinH)
 when is_record(This, wx_ref),is_integer(MinW),is_integer(MinH) ->
  setSizeHints(This,MinW,MinH, []);
setSizeHints(#wx_ref{type=ThisT}=This,{MinSizeW,MinSizeH} = MinSize, Options)
 when is_integer(MinSizeW),is_integer(MinSizeH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxSize, {_maxSizeW,_maxSizeH}} = Arg) -> Arg;
          ({incSize, {_incSizeW,_incSizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MinSize, Opts,?get_env(),?wxWindow_SetSizeHints_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
-spec setSizeHints(This, MinW, MinH, [Option]) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer(),
	Option :: {'maxW', integer()}
		 | {'maxH', integer()}
		 | {'incW', integer()}
		 | {'incH', integer()}.
setSizeHints(#wx_ref{type=ThisT}=This,MinW,MinH, Options)
 when is_integer(MinW),is_integer(MinH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxW, _maxW} = Arg) -> Arg;
          ({maxH, _maxH} = Arg) -> Arg;
          ({incW, _incW} = Arg) -> Arg;
          ({incH, _incH} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MinW,MinH, Opts,?get_env(),?wxWindow_SetSizeHints_3).

%% @equiv setSizer(This,Sizer, [])
-spec setSizer(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().

setSizer(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizer(This,Sizer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizer">external documentation</a>.
-spec setSizer(This, Sizer, [Option]) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer(),
	Option :: {'deleteOld', boolean()}.
setSizer(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, _deleteOld} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sizer, Opts,?get_env(),?wxWindow_SetSizer).

%% @equiv setSizerAndFit(This,Sizer, [])
-spec setSizerAndFit(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().

setSizerAndFit(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizerAndFit(This,Sizer, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizerandfit">external documentation</a>.
-spec setSizerAndFit(This, Sizer, [Option]) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer(),
	Option :: {'deleteOld', boolean()}.
setSizerAndFit(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, _deleteOld} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Sizer, Opts,?get_env(),?wxWindow_SetSizerAndFit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetthemeenabled">external documentation</a>.
-spec setThemeEnabled(This, Enable) -> 'ok' when
	This::wxWindow(), Enable::boolean().
setThemeEnabled(#wx_ref{type=ThisT}=This,Enable)
 when is_boolean(Enable) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Enable,?get_env(),?wxWindow_SetThemeEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettooltip">external documentation</a>.
%% <br /> Also:<br />
%% setToolTip(This, Tip) -> 'ok' when<br />
%% 	This::wxWindow(), Tip::wxToolTip:wxToolTip().<br />
%% 
-spec setToolTip(This, TipString) -> 'ok' when
	This::wxWindow(), TipString::unicode:chardata();
      (This, Tip) -> 'ok' when
	This::wxWindow(), Tip::wxToolTip:wxToolTip().
setToolTip(#wx_ref{type=ThisT}=This,TipString)
 when ?is_chardata(TipString) ->
  ?CLASS(ThisT,wxWindow),
  TipString_UC = unicode:characters_to_binary(TipString),
  wxe_util:queue_cmd(This,TipString_UC,?get_env(),?wxWindow_SetToolTip_1_0);
setToolTip(#wx_ref{type=ThisT}=This,#wx_ref{type=TipT}=Tip) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(TipT,wxToolTip),
  wxe_util:queue_cmd(This,Tip,?get_env(),?wxWindow_SetToolTip_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-spec setVirtualSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setVirtualSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxWindow_SetVirtualSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-spec setVirtualSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer().
setVirtualSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxWindow_SetVirtualSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyle">external documentation</a>.
-spec setWindowStyle(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetWindowStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyleflag">external documentation</a>.
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxWindow_SetWindowStyleFlag).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowvariant">external documentation</a>.
%%<br /> Variant = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-spec setWindowVariant(This, Variant) -> 'ok' when
	This::wxWindow(), Variant::wx:wx_enum().
setWindowVariant(#wx_ref{type=ThisT}=This,Variant)
 when is_integer(Variant) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Variant,?get_env(),?wxWindow_SetWindowVariant).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowshouldinheritcolours">external documentation</a>.
-spec shouldInheritColours(This) -> boolean() when
	This::wxWindow().
shouldInheritColours(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_ShouldInheritColours),
  wxe_util:rec(?wxWindow_ShouldInheritColours).

%% @equiv show(This, [])
-spec show(This) -> boolean() when
	This::wxWindow().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowshow">external documentation</a>.
-spec show(This, [Option]) -> boolean() when
	This::wxWindow(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_Show),
  wxe_util:rec(?wxWindow_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowthaw">external documentation</a>.
-spec thaw(This) -> 'ok' when
	This::wxWindow().
thaw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Thaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatafromwindow">external documentation</a>.
-spec transferDataFromWindow(This) -> boolean() when
	This::wxWindow().
transferDataFromWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_TransferDataFromWindow),
  wxe_util:rec(?wxWindow_TransferDataFromWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatatowindow">external documentation</a>.
-spec transferDataToWindow(This) -> boolean() when
	This::wxWindow().
transferDataToWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_TransferDataToWindow),
  wxe_util:rec(?wxWindow_TransferDataToWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowupdate">external documentation</a>.
-spec update(This) -> 'ok' when
	This::wxWindow().
update(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Update).

%% @equiv updateWindowUI(This, [])
-spec updateWindowUI(This) -> 'ok' when
	This::wxWindow().

updateWindowUI(This)
 when is_record(This, wx_ref) ->
  updateWindowUI(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowupdatewindowui">external documentation</a>.
-spec updateWindowUI(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'flags', integer()}.
updateWindowUI(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWindow_UpdateWindowUI).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowvalidate">external documentation</a>.
-spec validate(This) -> boolean() when
	This::wxWindow().
validate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_Validate),
  wxe_util:rec(?wxWindow_Validate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwarppointer">external documentation</a>.
-spec warpPointer(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer().
warpPointer(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxWindow_WarpPointer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettransparent">external documentation</a>.
-spec setTransparent(This, Alpha) -> boolean() when
	This::wxWindow(), Alpha::integer().
setTransparent(#wx_ref{type=ThisT}=This,Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Alpha,?get_env(),?wxWindow_SetTransparent),
  wxe_util:rec(?wxWindow_SetTransparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcansettransparent">external documentation</a>.
-spec canSetTransparent(This) -> boolean() when
	This::wxWindow().
canSetTransparent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_CanSetTransparent),
  wxe_util:rec(?wxWindow_CanSetTransparent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisdoublebuffered">external documentation</a>.
-spec isDoubleBuffered(This) -> boolean() when
	This::wxWindow().
isDoubleBuffered(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_IsDoubleBuffered),
  wxe_util:rec(?wxWindow_IsDoubleBuffered).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdoublebuffered">external documentation</a>.
-spec setDoubleBuffered(This, On) -> 'ok' when
	This::wxWindow(), On::boolean().
setDoubleBuffered(#wx_ref{type=ThisT}=This,On)
 when is_boolean(On) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,On,?get_env(),?wxWindow_SetDoubleBuffered).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcontentscalefactor">external documentation</a>.
-spec getContentScaleFactor(This) -> number() when
	This::wxWindow().
getContentScaleFactor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetContentScaleFactor),
  wxe_util:rec(?wxWindow_GetContentScaleFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdpi">external documentation</a>.
-spec getDPI(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getDPI(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxWindow_GetDPI),
  wxe_util:rec(?wxWindow_GetDPI).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfromdip">external documentation</a>.
%% <br /> Also:<br />
%% fromDIP(Sz, W) -> {W::integer(), H::integer()} when<br />
%% 	Sz::{W::integer(), H::integer()}, W::wxWindow();<br />
%%       (This, D) -> integer() when<br />
%% 	This::wxWindow(), D::integer();<br />
%%       (This, Sz) -> {W::integer(), H::integer()} when<br />
%% 	This::wxWindow(), Sz::{W::integer(), H::integer()}.<br />
%% 
-spec fromDIP(D, W) -> integer() when
	D::integer(), W::wxWindow();
      (Sz, W) -> {W::integer(), H::integer()} when
	Sz::{W::integer(), H::integer()}, W::wxWindow();
      (This, D) -> integer() when
	This::wxWindow(), D::integer();
      (This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
fromDIP(D,#wx_ref{type=WT}=W)
 when is_integer(D) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(D,W,?get_env(),?wxWindow_FromDIP_2_0),
  wxe_util:rec(?wxWindow_FromDIP_2_0);
fromDIP({SzW,SzH} = Sz,#wx_ref{type=WT}=W)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(Sz,W,?get_env(),?wxWindow_FromDIP_2_1),
  wxe_util:rec(?wxWindow_FromDIP_2_1);
fromDIP(#wx_ref{type=ThisT}=This,D)
 when is_integer(D) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,D,?get_env(),?wxWindow_FromDIP_1_0),
  wxe_util:rec(?wxWindow_FromDIP_1_0);
fromDIP(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_FromDIP_1_1),
  wxe_util:rec(?wxWindow_FromDIP_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtodip">external documentation</a>.
%% <br /> Also:<br />
%% toDIP(Sz, W) -> {W::integer(), H::integer()} when<br />
%% 	Sz::{W::integer(), H::integer()}, W::wxWindow();<br />
%%       (This, D) -> integer() when<br />
%% 	This::wxWindow(), D::integer();<br />
%%       (This, Sz) -> {W::integer(), H::integer()} when<br />
%% 	This::wxWindow(), Sz::{W::integer(), H::integer()}.<br />
%% 
-spec toDIP(D, W) -> integer() when
	D::integer(), W::wxWindow();
      (Sz, W) -> {W::integer(), H::integer()} when
	Sz::{W::integer(), H::integer()}, W::wxWindow();
      (This, D) -> integer() when
	This::wxWindow(), D::integer();
      (This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
toDIP(D,#wx_ref{type=WT}=W)
 when is_integer(D) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(D,W,?get_env(),?wxWindow_ToDIP_2_0),
  wxe_util:rec(?wxWindow_ToDIP_2_0);
toDIP({SzW,SzH} = Sz,#wx_ref{type=WT}=W)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(WT,wxWindow),
  wxe_util:queue_cmd(Sz,W,?get_env(),?wxWindow_ToDIP_2_1),
  wxe_util:rec(?wxWindow_ToDIP_2_1);
toDIP(#wx_ref{type=ThisT}=This,D)
 when is_integer(D) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,D,?get_env(),?wxWindow_ToDIP_1_0),
  wxe_util:rec(?wxWindow_ToDIP_1_0);
toDIP(#wx_ref{type=ThisT}=This,{SzW,SzH} = Sz)
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:queue_cmd(This,Sz,?get_env(),?wxWindow_ToDIP_1_1),
  wxe_util:rec(?wxWindow_ToDIP_1_1).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
