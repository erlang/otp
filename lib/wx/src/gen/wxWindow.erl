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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html">wxWindow</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxWindow().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxWindow).
-include("wxe.hrl").
-export(['Destroy'/1,cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,convertDialogToPixels/2,convertPixelsToDialog/2,destroy/1,
  destroyChildren/1,disable/1,enable/1,enable/2,findFocus/0,findWindow/2,
  findWindowById/1,findWindowById/2,findWindowByLabel/1,findWindowByLabel/2,
  findWindowByName/1,findWindowByName/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCapture/0,
  getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,
  getContainingSizer/1,getCursor/1,getDropTarget/1,getEventHandler/1,
  getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,getToolTip/1,
  getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,isTopLevel/1,
  layout/1,lineDown/1,lineUp/1,lower/1,makeModal/1,makeModal/2,move/2,
  move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,
  navigate/2,new/0,new/2,new/3,pageDown/1,pageUp/1,popEventHandler/1,popEventHandler/2,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-export_type([wxWindow/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxWindow() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
-spec new() -> wxWindow().
new() ->
  wxe_util:construct(?wxWindow_new_0,
  <<>>).

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
new(#wx_ref{type=ParentT,ref=ParentRef},Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {PosX,PosY}}, Acc) -> [<<1:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<3:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxWindow_new_3,
  <<ParentRef:32/?UI,Id:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcachebestsize">external documentation</a>.
-spec cacheBestSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
cacheBestSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_CacheBestSize,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcapturemouse">external documentation</a>.
-spec captureMouse(This) -> 'ok' when
	This::wxWindow().
captureMouse(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_CaptureMouse,
  <<ThisRef:32/?UI>>).

%% @equiv center(This, [])
-spec center(This) -> 'ok' when
	This::wxWindow().

center(This)
 when is_record(This, wx_ref) ->
  center(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcenter">external documentation</a>.
-spec center(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
center(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Center,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv centerOnParent(This, [])
-spec centerOnParent(This) -> 'ok' when
	This::wxWindow().

centerOnParent(This)
 when is_record(This, wx_ref) ->
  centerOnParent(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcenteronparent">external documentation</a>.
-spec centerOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centerOnParent(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_CenterOnParent,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv centre(This, [])
-spec centre(This) -> 'ok' when
	This::wxWindow().

centre(This)
 when is_record(This, wx_ref) ->
  centre(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
-spec centre(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centre(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Centre,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv centreOnParent(This, [])
-spec centreOnParent(This) -> 'ok' when
	This::wxWindow().

centreOnParent(This)
 when is_record(This, wx_ref) ->
  centreOnParent(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
-spec centreOnParent(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'dir', integer()}.
centreOnParent(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_CentreOnParent,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclearbackground">external documentation</a>.
-spec clearBackground(This) -> 'ok' when
	This::wxWindow().
clearBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_ClearBackground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-spec clientToScreen(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
clientToScreen(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ClientToScreen_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
-spec clientToScreen(This, X, Y) -> {X::integer(), Y::integer()} when
	This::wxWindow(), X::integer(), Y::integer().
clientToScreen(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ClientToScreen_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

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
close(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({force, Force}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Force)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Close,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertdialogtopixels">external documentation</a>.
-spec convertDialogToPixels(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertDialogToPixels(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ConvertDialogToPixels,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowconvertpixelstodialog">external documentation</a>.
-spec convertPixelsToDialog(This, Sz) -> {W::integer(), H::integer()} when
	This::wxWindow(), Sz::{W::integer(), H::integer()}.
convertPixelsToDialog(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ConvertPixelsToDialog,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroy">external documentation</a>.
-spec 'Destroy'(This) -> boolean() when
	This::wxWindow().
'Destroy'(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Destroy,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdestroychildren">external documentation</a>.
-spec destroyChildren(This) -> boolean() when
	This::wxWindow().
destroyChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_DestroyChildren,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowdisable">external documentation</a>.
-spec disable(This) -> boolean() when
	This::wxWindow().
disable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Disable,
  <<ThisRef:32/?UI>>).

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
enable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Enable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindfocus">external documentation</a>.
-spec findFocus() -> wxWindow().
findFocus() ->
  wxe_util:call(?wxWindow_FindFocus,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindow">external documentation</a>.
%% <br /> Also:<br />
%% findWindow(This, Name) -> wxWindow() when<br />
%% 	This::wxWindow(), Name::unicode:chardata().<br />
%% 
-spec findWindow(This, Winid) -> wxWindow() when
	This::wxWindow(), Winid::integer();
      (This, Name) -> wxWindow() when
	This::wxWindow(), Name::unicode:chardata().
findWindow(#wx_ref{type=ThisT,ref=ThisRef},Winid)
 when is_integer(Winid) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_FindWindow_1_0,
  <<ThisRef:32/?UI,Winid:32/?UI>>);
findWindow(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxWindow_FindWindow_1_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv findWindowById(Winid, [])
-spec findWindowById(Winid) -> wxWindow() when
	Winid::integer().

findWindowById(Winid)
 when is_integer(Winid) ->
  findWindowById(Winid, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyid">external documentation</a>.
-spec findWindowById(Winid, [Option]) -> wxWindow() when
	Winid::integer(),
	Option :: {'parent', wxWindow()}.
findWindowById(Winid, Options)
 when is_integer(Winid),is_list(Options) ->
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowById,
  <<Winid:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv findWindowByName(Name, [])
-spec findWindowByName(Name) -> wxWindow() when
	Name::unicode:chardata().

findWindowByName(Name)
 when is_list(Name) ->
  findWindowByName(Name, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbyname">external documentation</a>.
-spec findWindowByName(Name, [Option]) -> wxWindow() when
	Name::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByName(Name, Options)
 when is_list(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowByName,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv findWindowByLabel(Label, [])
-spec findWindowByLabel(Label) -> wxWindow() when
	Label::unicode:chardata().

findWindowByLabel(Label)
 when is_list(Label) ->
  findWindowByLabel(Label, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfindwindowbylabel">external documentation</a>.
-spec findWindowByLabel(Label, [Option]) -> wxWindow() when
	Label::unicode:chardata(),
	Option :: {'parent', wxWindow()}.
findWindowByLabel(Label, Options)
 when is_list(Label),is_list(Options) ->
  Label_UC = unicode:characters_to_binary([Label,0]),
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowByLabel,
  <<(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfit">external documentation</a>.
-spec fit(This) -> 'ok' when
	This::wxWindow().
fit(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Fit,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfitinside">external documentation</a>.
-spec fitInside(This) -> 'ok' when
	This::wxWindow().
fitInside(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_FitInside,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowfreeze">external documentation</a>.
-spec freeze(This) -> 'ok' when
	This::wxWindow().
freeze(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Freeze,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetacceleratortable">external documentation</a>.
-spec getAcceleratorTable(This) -> wxAcceleratorTable:wxAcceleratorTable() when
	This::wxWindow().
getAcceleratorTable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetAcceleratorTable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundcolour">external documentation</a>.
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbackgroundstyle">external documentation</a>.
%%<br /> Res = ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_CUSTOM
-spec getBackgroundStyle(This) -> wx:wx_enum() when
	This::wxWindow().
getBackgroundStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBackgroundStyle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetbestsize">external documentation</a>.
-spec getBestSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getBestSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBestSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcaret">external documentation</a>.
-spec getCaret(This) -> wxCaret:wxCaret() when
	This::wxWindow().
getCaret(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCaret,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcapture">external documentation</a>.
-spec getCapture() -> wxWindow().
getCapture() ->
  wxe_util:call(?wxWindow_GetCapture,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharheight">external documentation</a>.
-spec getCharHeight(This) -> integer() when
	This::wxWindow().
getCharHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCharHeight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcharwidth">external documentation</a>.
-spec getCharWidth(This) -> integer() when
	This::wxWindow().
getCharWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCharWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetchildren">external documentation</a>.
-spec getChildren(This) -> [wxWindow()] when
	This::wxWindow().
getChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetChildren,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetclientsize">external documentation</a>.
-spec getClientSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getClientSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetClientSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcontainingsizer">external documentation</a>.
-spec getContainingSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getContainingSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetContainingSizer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetcursor">external documentation</a>.
-spec getCursor(This) -> wxCursor:wxCursor() when
	This::wxWindow().
getCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCursor,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetdroptarget">external documentation</a>.
-spec getDropTarget(This) -> wx:wx_object() when
	This::wxWindow().
getDropTarget(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetDropTarget,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgeteventhandler">external documentation</a>.
-spec getEventHandler(This) -> wxEvtHandler:wxEvtHandler() when
	This::wxWindow().
getEventHandler(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetEventHandler,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetextrastyle">external documentation</a>.
-spec getExtraStyle(This) -> integer() when
	This::wxWindow().
getExtraStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetExtraStyle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetfont">external documentation</a>.
-spec getFont(This) -> wxFont:wxFont() when
	This::wxWindow().
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetforegroundcolour">external documentation</a>.
-spec getForegroundColour(This) -> wx:wx_colour4() when
	This::wxWindow().
getForegroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetForegroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetgrandparent">external documentation</a>.
-spec getGrandParent(This) -> wxWindow() when
	This::wxWindow().
getGrandParent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetGrandParent,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethandle">external documentation</a>.
-spec getHandle(This) -> integer() when
	This::wxWindow().
getHandle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetHandle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgethelptext">external documentation</a>.
-spec getHelpText(This) -> unicode:charlist() when
	This::wxWindow().
getHelpText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetHelpText,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxWindow().
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxWindow().
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetLabel,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetmaxsize">external documentation</a>.
-spec getMaxSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMaxSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetMaxSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetname">external documentation</a>.
-spec getName(This) -> unicode:charlist() when
	This::wxWindow().
getName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetparent">external documentation</a>.
-spec getParent(This) -> wxWindow() when
	This::wxWindow().
getParent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetParent,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetrect">external documentation</a>.
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenposition">external documentation</a>.
-spec getScreenPosition(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
getScreenPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScreenPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscreenrect">external documentation</a>.
-spec getScreenRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxWindow().
getScreenRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScreenRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollpos">external documentation</a>.
-spec getScrollPos(This, Orient) -> integer() when
	This::wxWindow(), Orient::integer().
getScrollPos(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollPos,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollrange">external documentation</a>.
-spec getScrollRange(This, Orient) -> integer() when
	This::wxWindow(), Orient::integer().
getScrollRange(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollRange,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetscrollthumb">external documentation</a>.
-spec getScrollThumb(This, Orient) -> integer() when
	This::wxWindow(), Orient::integer().
getScrollThumb(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollThumb,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetsizer">external documentation</a>.
-spec getSizer(This) -> wxSizer:wxSizer() when
	This::wxWindow().
getSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetSizer,
  <<ThisRef:32/?UI>>).

%% @equiv getTextExtent(This,String, [])
-spec getTextExtent(This, String) -> Result when
	Result ::{X::integer(), Y::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata().

getTextExtent(This,String)
 when is_record(This, wx_ref),is_list(String) ->
  getTextExtent(This,String, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettextextent">external documentation</a>.
-spec getTextExtent(This, String, [Option]) -> Result when
	Result :: {X::integer(), Y::integer(), Descent::integer(), ExternalLeading::integer()},
	This::wxWindow(), String::unicode:chardata(),
	Option :: {'theFont', wxFont:wxFont()}.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when is_list(String),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({theFont, #wx_ref{type=TheFontT,ref=TheFontRef}}, Acc) ->   ?CLASS(TheFontT,wxFont),[<<1:32/?UI,TheFontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_GetTextExtent,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgettooltip">external documentation</a>.
-spec getToolTip(This) -> wxToolTip:wxToolTip() when
	This::wxWindow().
getToolTip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetToolTip,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetupdateregion">external documentation</a>.
-spec getUpdateRegion(This) -> wxRegion:wxRegion() when
	This::wxWindow().
getUpdateRegion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetUpdateRegion,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetvirtualsize">external documentation</a>.
-spec getVirtualSize(This) -> {W::integer(), H::integer()} when
	This::wxWindow().
getVirtualSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetVirtualSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowstyleflag">external documentation</a>.
-spec getWindowStyleFlag(This) -> integer() when
	This::wxWindow().
getWindowStyleFlag(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetWindowStyleFlag,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowgetwindowvariant">external documentation</a>.
%%<br /> Res = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-spec getWindowVariant(This) -> wx:wx_enum() when
	This::wxWindow().
getWindowVariant(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetWindowVariant,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhascapture">external documentation</a>.
-spec hasCapture(This) -> boolean() when
	This::wxWindow().
hasCapture(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasCapture,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhasscrollbar">external documentation</a>.
-spec hasScrollbar(This, Orient) -> boolean() when
	This::wxWindow(), Orient::integer().
hasScrollbar(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasScrollbar,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhastransparentbackground">external documentation</a>.
-spec hasTransparentBackground(This) -> boolean() when
	This::wxWindow().
hasTransparentBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasTransparentBackground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowhide">external documentation</a>.
-spec hide(This) -> boolean() when
	This::wxWindow().
hide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Hide,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinheritattributes">external documentation</a>.
-spec inheritAttributes(This) -> 'ok' when
	This::wxWindow().
inheritAttributes(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InheritAttributes,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinitdialog">external documentation</a>.
-spec initDialog(This) -> 'ok' when
	This::wxWindow().
initDialog(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InitDialog,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowinvalidatebestsize">external documentation</a>.
-spec invalidateBestSize(This) -> 'ok' when
	This::wxWindow().
invalidateBestSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InvalidateBestSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisenabled">external documentation</a>.
-spec isEnabled(This) -> boolean() when
	This::wxWindow().
isEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsEnabled,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
%% <br /> Also:<br />
%% isExposed(This, Rect) -> boolean() when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec isExposed(This, Pt) -> boolean() when
	This::wxWindow(), Pt::{X::integer(), Y::integer()};
      (This, Rect) -> boolean() when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
isExposed(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_1_0,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>);
isExposed(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-spec isExposed(This, X, Y) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer().
isExposed(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
-spec isExposed(This, X, Y, W, H) -> boolean() when
	This::wxWindow(), X::integer(), Y::integer(), W::integer(), H::integer().
isExposed(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisretained">external documentation</a>.
-spec isRetained(This) -> boolean() when
	This::wxWindow().
isRetained(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsRetained,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisshown">external documentation</a>.
-spec isShown(This) -> boolean() when
	This::wxWindow().
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsShown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowistoplevel">external documentation</a>.
-spec isTopLevel(This) -> boolean() when
	This::wxWindow().
isTopLevel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsTopLevel,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlayout">external documentation</a>.
-spec layout(This) -> boolean() when
	This::wxWindow().
layout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Layout,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlinedown">external documentation</a>.
-spec lineDown(This) -> boolean() when
	This::wxWindow().
lineDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_LineDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlineup">external documentation</a>.
-spec lineUp(This) -> boolean() when
	This::wxWindow().
lineUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_LineUp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowlower">external documentation</a>.
-spec lower(This) -> 'ok' when
	This::wxWindow().
lower(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Lower,
  <<ThisRef:32/?UI>>).

%% @equiv makeModal(This, [])
-spec makeModal(This) -> 'ok' when
	This::wxWindow().

makeModal(This)
 when is_record(This, wx_ref) ->
  makeModal(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmakemodal">external documentation</a>.
-spec makeModal(This, [Option]) -> 'ok' when
	This::wxWindow(),
	Option :: {'modal', boolean()}.
makeModal(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({modal, Modal}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Modal)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_MakeModal,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv move(This,Pt, [])
-spec move(This, Pt) -> 'ok' when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.

move(This,Pt={PtX,PtY})
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
move(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY}, Options)
 when is_integer(PtX),is_integer(PtY),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Move_2,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmove">external documentation</a>.
-spec move(This, X, Y, [Option]) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer(),
	Option :: {'flags', integer()}.
move(#wx_ref{type=ThisT,ref=ThisRef},X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Move_3,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmoveafterintaborder">external documentation</a>.
-spec moveAfterInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveAfterInTabOrder(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxWindow_MoveAfterInTabOrder,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowmovebeforeintaborder">external documentation</a>.
-spec moveBeforeInTabOrder(This, Win) -> 'ok' when
	This::wxWindow(), Win::wxWindow().
moveBeforeInTabOrder(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxWindow_MoveBeforeInTabOrder,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

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
navigate(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Navigate,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpagedown">external documentation</a>.
-spec pageDown(This) -> boolean() when
	This::wxWindow().
pageDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_PageDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpageup">external documentation</a>.
-spec pageUp(This) -> boolean() when
	This::wxWindow().
pageUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_PageUp,
  <<ThisRef:32/?UI>>).

%% @equiv popEventHandler(This, [])
-spec popEventHandler(This) -> wxEvtHandler:wxEvtHandler() when
	This::wxWindow().

popEventHandler(This)
 when is_record(This, wx_ref) ->
  popEventHandler(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopeventhandler">external documentation</a>.
-spec popEventHandler(This, [Option]) -> wxEvtHandler:wxEvtHandler() when
	This::wxWindow(),
	Option :: {'deleteHandler', boolean()}.
popEventHandler(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({deleteHandler, DeleteHandler}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteHandler)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_PopEventHandler,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

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
popupMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  MOpts = fun({pos, {PosX,PosY}}, Acc) -> [<<1:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_PopupMenu_2,
  <<ThisRef:32/?UI,MenuRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
-spec popupMenu(This, Menu, X, Y) -> boolean() when
	This::wxWindow(), Menu::wxMenu:wxMenu(), X::integer(), Y::integer().
popupMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  wxe_util:call(?wxWindow_PopupMenu_3,
  <<ThisRef:32/?UI,MenuRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowraise">external documentation</a>.
-spec raise(This) -> 'ok' when
	This::wxWindow().
raise(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Raise,
  <<ThisRef:32/?UI>>).

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
refresh(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, EraseBackground}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(EraseBackground)):32/?UI>>|Acc];
          ({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<2:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Refresh,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv refreshRect(This,Rect, [])
-spec refreshRect(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.

refreshRect(This,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  refreshRect(This,Rect, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowrefreshrect">external documentation</a>.
-spec refreshRect(This, Rect, [Option]) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()},
	Option :: {'eraseBackground', boolean()}.
refreshRect(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH}, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, EraseBackground}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(EraseBackground)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_RefreshRect,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreleasemouse">external documentation</a>.
-spec releaseMouse(This) -> 'ok' when
	This::wxWindow().
releaseMouse(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_ReleaseMouse,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowremovechild">external documentation</a>.
-spec removeChild(This, Child) -> 'ok' when
	This::wxWindow(), Child::wxWindow().
removeChild(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ChildT,ref=ChildRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ChildT,wxWindow),
  wxe_util:cast(?wxWindow_RemoveChild,
  <<ThisRef:32/?UI,ChildRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowreparent">external documentation</a>.
-spec reparent(This, NewParent) -> boolean() when
	This::wxWindow(), NewParent::wxWindow().
reparent(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=NewParentT,ref=NewParentRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(NewParentT,wxWindow),
  wxe_util:call(?wxWindow_Reparent,
  <<ThisRef:32/?UI,NewParentRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-spec screenToClient(This) -> {X::integer(), Y::integer()} when
	This::wxWindow().
screenToClient(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScreenToClient_2,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
-spec screenToClient(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxWindow(), Pt::{X::integer(), Y::integer()}.
screenToClient(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScreenToClient_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrolllines">external documentation</a>.
-spec scrollLines(This, Lines) -> boolean() when
	This::wxWindow(), Lines::integer().
scrollLines(#wx_ref{type=ThisT,ref=ThisRef},Lines)
 when is_integer(Lines) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScrollLines,
  <<ThisRef:32/?UI,Lines:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowscrollpages">external documentation</a>.
-spec scrollPages(This, Pages) -> boolean() when
	This::wxWindow(), Pages::integer().
scrollPages(#wx_ref{type=ThisT,ref=ThisRef},Pages)
 when is_integer(Pages) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScrollPages,
  <<ThisRef:32/?UI,Pages:32/?UI>>).

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
scrollWindow(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy, Options)
 when is_integer(Dx),is_integer(Dy),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<1:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_ScrollWindow,
  <<ThisRef:32/?UI,Dx:32/?UI,Dy:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetacceleratortable">external documentation</a>.
-spec setAcceleratorTable(This, Accel) -> 'ok' when
	This::wxWindow(), Accel::wxAcceleratorTable:wxAcceleratorTable().
setAcceleratorTable(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=AccelT,ref=AccelRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(AccelT,wxAcceleratorTable),
  wxe_util:cast(?wxWindow_SetAcceleratorTable,
  <<ThisRef:32/?UI,AccelRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetautolayout">external documentation</a>.
-spec setAutoLayout(This, AutoLayout) -> 'ok' when
	This::wxWindow(), AutoLayout::boolean().
setAutoLayout(#wx_ref{type=ThisT,ref=ThisRef},AutoLayout)
 when is_boolean(AutoLayout) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetAutoLayout,
  <<ThisRef:32/?UI,(wxe_util:from_bool(AutoLayout)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetbackgroundstyle">external documentation</a>.
%%<br /> Style = ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_CUSTOM
-spec setBackgroundStyle(This, Style) -> boolean() when
	This::wxWindow(), Style::wx:wx_enum().
setBackgroundStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetBackgroundStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcaret">external documentation</a>.
-spec setCaret(This, Caret) -> 'ok' when
	This::wxWindow(), Caret::wxCaret:wxCaret().
setCaret(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CaretT,ref=CaretRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CaretT,wxCaret),
  wxe_util:cast(?wxWindow_SetCaret,
  <<ThisRef:32/?UI,CaretRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
%% <br /> Also:<br />
%% setClientSize(This, Rect) -> 'ok' when<br />
%% 	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec setClientSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()};
      (This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
setClientSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetClientSize_1_0,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>);
setClientSize(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetClientSize_1_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
-spec setClientSize(This, Width, Height) -> 'ok' when
	This::wxWindow(), Width::integer(), Height::integer().
setClientSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetClientSize_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcontainingsizer">external documentation</a>.
-spec setContainingSizer(This, Sizer) -> 'ok' when
	This::wxWindow(), Sizer::wxSizer:wxSizer().
setContainingSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  wxe_util:cast(?wxWindow_SetContainingSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetcursor">external documentation</a>.
-spec setCursor(This, Cursor) -> boolean() when
	This::wxWindow(), Cursor::wxCursor:wxCursor().
setCursor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CursorT,ref=CursorRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CursorT,wxCursor),
  wxe_util:call(?wxWindow_SetCursor,
  <<ThisRef:32/?UI,CursorRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetmaxsize">external documentation</a>.
-spec setMaxSize(This, MaxSize) -> 'ok' when
	This::wxWindow(), MaxSize::{W::integer(), H::integer()}.
setMaxSize(#wx_ref{type=ThisT,ref=ThisRef},{MaxSizeW,MaxSizeH})
 when is_integer(MaxSizeW),is_integer(MaxSizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetMaxSize,
  <<ThisRef:32/?UI,MaxSizeW:32/?UI,MaxSizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetminsize">external documentation</a>.
-spec setMinSize(This, MinSize) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{MinSizeW,MinSizeH})
 when is_integer(MinSizeW),is_integer(MinSizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetMinSize,
  <<ThisRef:32/?UI,MinSizeW:32/?UI,MinSizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownbackgroundcolour">external documentation</a>.
-spec setOwnBackgroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetOwnBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownfont">external documentation</a>.
-spec setOwnFont(This, Font) -> 'ok' when
	This::wxWindow(), Font::wxFont:wxFont().
setOwnFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxWindow_SetOwnFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetownforegroundcolour">external documentation</a>.
-spec setOwnForegroundColour(This, Colour) -> 'ok' when
	This::wxWindow(), Colour::wx:wx_colour().
setOwnForegroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetOwnForegroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdroptarget">external documentation</a>.
-spec setDropTarget(This, DropTarget) -> 'ok' when
	This::wxWindow(), DropTarget::wx:wx_object().
setDropTarget(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DropTargetT,ref=DropTargetRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(DropTargetT,wxDropTarget),
  wxe_util:cast(?wxWindow_SetDropTarget,
  <<ThisRef:32/?UI,DropTargetRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetextrastyle">external documentation</a>.
-spec setExtraStyle(This, ExStyle) -> 'ok' when
	This::wxWindow(), ExStyle::integer().
setExtraStyle(#wx_ref{type=ThisT,ref=ThisRef},ExStyle)
 when is_integer(ExStyle) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetExtraStyle,
  <<ThisRef:32/?UI,ExStyle:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocus">external documentation</a>.
-spec setFocus(This) -> 'ok' when
	This::wxWindow().
setFocus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetFocus,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfocusfromkbd">external documentation</a>.
-spec setFocusFromKbd(This) -> 'ok' when
	This::wxWindow().
setFocusFromKbd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetFocusFromKbd,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetfont">external documentation</a>.
-spec setFont(This, Font) -> boolean() when
	This::wxWindow(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:call(?wxWindow_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetforegroundcolour">external documentation</a>.
-spec setForegroundColour(This, Colour) -> boolean() when
	This::wxWindow(), Colour::wx:wx_colour().
setForegroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetForegroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsethelptext">external documentation</a>.
-spec setHelpText(This, Text) -> 'ok' when
	This::wxWindow(), Text::unicode:chardata().
setHelpText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxWindow),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxWindow_SetHelpText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetid">external documentation</a>.
-spec setId(This, Winid) -> 'ok' when
	This::wxWindow(), Winid::integer().
setId(#wx_ref{type=ThisT,ref=ThisRef},Winid)
 when is_integer(Winid) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetId,
  <<ThisRef:32/?UI,Winid:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetlabel">external documentation</a>.
-spec setLabel(This, Label) -> 'ok' when
	This::wxWindow(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT,ref=ThisRef},Label)
 when is_list(Label) ->
  ?CLASS(ThisT,wxWindow),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:cast(?wxWindow_SetLabel,
  <<ThisRef:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((0+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetname">external documentation</a>.
-spec setName(This, Name) -> 'ok' when
	This::wxWindow(), Name::unicode:chardata().
setName(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:cast(?wxWindow_SetName,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetpalette">external documentation</a>.
-spec setPalette(This, Pal) -> 'ok' when
	This::wxWindow(), Pal::wxPalette:wxPalette().
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PalT,ref=PalRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(PalT,wxPalette),
  wxe_util:cast(?wxWindow_SetPalette,
  <<ThisRef:32/?UI,PalRef:32/?UI>>).

%% @equiv setScrollbar(This,Orient,Pos,ThumbVisible,Range, [])
-spec setScrollbar(This, Orient, Pos, ThumbVisible, Range) -> 'ok' when
	This::wxWindow(), Orient::integer(), Pos::integer(), ThumbVisible::integer(), Range::integer().

setScrollbar(This,Orient,Pos,ThumbVisible,Range)
 when is_record(This, wx_ref),is_integer(Orient),is_integer(Pos),is_integer(ThumbVisible),is_integer(Range) ->
  setScrollbar(This,Orient,Pos,ThumbVisible,Range, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollbar">external documentation</a>.
-spec setScrollbar(This, Orient, Pos, ThumbVisible, Range, [Option]) -> 'ok' when
	This::wxWindow(), Orient::integer(), Pos::integer(), ThumbVisible::integer(), Range::integer(),
	Option :: {'refresh', boolean()}.
setScrollbar(#wx_ref{type=ThisT,ref=ThisRef},Orient,Pos,ThumbVisible,Range, Options)
 when is_integer(Orient),is_integer(Pos),is_integer(ThumbVisible),is_integer(Range),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, Refresh}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Refresh)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetScrollbar,
  <<ThisRef:32/?UI,Orient:32/?UI,Pos:32/?UI,ThumbVisible:32/?UI,Range:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv setScrollPos(This,Orient,Pos, [])
-spec setScrollPos(This, Orient, Pos) -> 'ok' when
	This::wxWindow(), Orient::integer(), Pos::integer().

setScrollPos(This,Orient,Pos)
 when is_record(This, wx_ref),is_integer(Orient),is_integer(Pos) ->
  setScrollPos(This,Orient,Pos, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetscrollpos">external documentation</a>.
-spec setScrollPos(This, Orient, Pos, [Option]) -> 'ok' when
	This::wxWindow(), Orient::integer(), Pos::integer(),
	Option :: {'refresh', boolean()}.
setScrollPos(#wx_ref{type=ThisT,ref=ThisRef},Orient,Pos, Options)
 when is_integer(Orient),is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, Refresh}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Refresh)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetScrollPos,
  <<ThisRef:32/?UI,Orient:32/?UI,Pos:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Also:<br />
%% setSize(This, Size) -> 'ok' when<br />
%% 	This::wxWindow(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setSize(This, Rect) -> 'ok' when
	This::wxWindow(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.

setSize(This,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  setSize(This,Rect, []);
setSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

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
setSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetSize_2_0,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>);
setSize(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH}, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, SizeFlags}, Acc) -> [<<1:32/?UI,SizeFlags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSize_2_1,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, 0:32,BinOpt/binary>>).

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
setSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Width,Height, Options)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, SizeFlags}, Acc) -> [<<1:32/?UI,SizeFlags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSize_5,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv setSizeHints(This,MinSize, [])
-spec setSizeHints(This, MinSize) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()}.

setSizeHints(This,MinSize={MinSizeW,MinSizeH})
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
setSizeHints(#wx_ref{type=ThisT,ref=ThisRef},{MinSizeW,MinSizeH}, Options)
 when is_integer(MinSizeW),is_integer(MinSizeH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxSize, {MaxSizeW,MaxSizeH}}, Acc) -> [<<1:32/?UI,MaxSizeW:32/?UI,MaxSizeH:32/?UI,0:32>>|Acc];
          ({incSize, {IncSizeW,IncSizeH}}, Acc) -> [<<2:32/?UI,IncSizeW:32/?UI,IncSizeH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizeHints_2,
  <<ThisRef:32/?UI,MinSizeW:32/?UI,MinSizeH:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
-spec setSizeHints(This, MinW, MinH, [Option]) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer(),
	Option :: {'maxW', integer()}
		 | {'maxH', integer()}
		 | {'incW', integer()}
		 | {'incH', integer()}.
setSizeHints(#wx_ref{type=ThisT,ref=ThisRef},MinW,MinH, Options)
 when is_integer(MinW),is_integer(MinH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxW, MaxW}, Acc) -> [<<1:32/?UI,MaxW:32/?UI>>|Acc];
          ({maxH, MaxH}, Acc) -> [<<2:32/?UI,MaxH:32/?UI>>|Acc];
          ({incW, IncW}, Acc) -> [<<3:32/?UI,IncW:32/?UI>>|Acc];
          ({incH, IncH}, Acc) -> [<<4:32/?UI,IncH:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizeHints_3,
  <<ThisRef:32/?UI,MinW:32/?UI,MinH:32/?UI, 0:32,BinOpt/binary>>).

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
setSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, DeleteOld}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteOld)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI, BinOpt/binary>>).

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
setSizerAndFit(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, DeleteOld}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteOld)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizerAndFit,
  <<ThisRef:32/?UI,SizerRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetthemeenabled">external documentation</a>.
-spec setThemeEnabled(This, EnableTheme) -> 'ok' when
	This::wxWindow(), EnableTheme::boolean().
setThemeEnabled(#wx_ref{type=ThisT,ref=ThisRef},EnableTheme)
 when is_boolean(EnableTheme) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetThemeEnabled,
  <<ThisRef:32/?UI,(wxe_util:from_bool(EnableTheme)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettooltip">external documentation</a>.
%% <br /> Also:<br />
%% setToolTip(This, Tip) -> 'ok' when<br />
%% 	This::wxWindow(), Tip::wxToolTip:wxToolTip().<br />
%% 
-spec setToolTip(This, Tip) -> 'ok' when
	This::wxWindow(), Tip::unicode:chardata();
      (This, Tip) -> 'ok' when
	This::wxWindow(), Tip::wxToolTip:wxToolTip().
setToolTip(#wx_ref{type=ThisT,ref=ThisRef},Tip)
 when is_list(Tip) ->
  ?CLASS(ThisT,wxWindow),
  Tip_UC = unicode:characters_to_binary([Tip,0]),
  wxe_util:cast(?wxWindow_SetToolTip_1_0,
  <<ThisRef:32/?UI,(byte_size(Tip_UC)):32/?UI,(Tip_UC)/binary, 0:(((8- ((0+byte_size(Tip_UC)) band 16#7)) band 16#7))/unit:8>>);
setToolTip(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=TipT,ref=TipRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(TipT,wxToolTip),
  wxe_util:cast(?wxWindow_SetToolTip_1_1,
  <<ThisRef:32/?UI,TipRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-spec setVirtualSize(This, Size) -> 'ok' when
	This::wxWindow(), Size::{W::integer(), H::integer()}.
setVirtualSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetVirtualSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
-spec setVirtualSize(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer().
setVirtualSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetVirtualSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @equiv setVirtualSizeHints(This,MinSize, [])
-spec setVirtualSizeHints(This, MinSize) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()}.

setVirtualSizeHints(This,MinSize={MinSizeW,MinSizeH})
 when is_record(This, wx_ref),is_integer(MinSizeW),is_integer(MinSizeH) ->
  setVirtualSizeHints(This,MinSize, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsizehints">external documentation</a>.
%% <br /> Also:<br />
%% setVirtualSizeHints(This, MinSize, [Option]) -> 'ok' when<br />
%% 	This::wxWindow(), MinSize::{W::integer(), H::integer()},<br />
%% 	Option :: {'maxSize', {W::integer(), H::integer()}}.<br />
%% 
-spec setVirtualSizeHints(This, MinW, MinH) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer();
      (This, MinSize, [Option]) -> 'ok' when
	This::wxWindow(), MinSize::{W::integer(), H::integer()},
	Option :: {'maxSize', {W::integer(), H::integer()}}.

setVirtualSizeHints(This,MinW,MinH)
 when is_record(This, wx_ref),is_integer(MinW),is_integer(MinH) ->
  setVirtualSizeHints(This,MinW,MinH, []);
setVirtualSizeHints(#wx_ref{type=ThisT,ref=ThisRef},{MinSizeW,MinSizeH}, Options)
 when is_integer(MinSizeW),is_integer(MinSizeH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxSize, {MaxSizeW,MaxSizeH}}, Acc) -> [<<1:32/?UI,MaxSizeW:32/?UI,MaxSizeH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetVirtualSizeHints_2,
  <<ThisRef:32/?UI,MinSizeW:32/?UI,MinSizeH:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetvirtualsizehints">external documentation</a>.
-spec setVirtualSizeHints(This, MinW, MinH, [Option]) -> 'ok' when
	This::wxWindow(), MinW::integer(), MinH::integer(),
	Option :: {'maxW', integer()}
		 | {'maxH', integer()}.
setVirtualSizeHints(#wx_ref{type=ThisT,ref=ThisRef},MinW,MinH, Options)
 when is_integer(MinW),is_integer(MinH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxW, MaxW}, Acc) -> [<<1:32/?UI,MaxW:32/?UI>>|Acc];
          ({maxH, MaxH}, Acc) -> [<<2:32/?UI,MaxH:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetVirtualSizeHints_3,
  <<ThisRef:32/?UI,MinW:32/?UI,MinH:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyle">external documentation</a>.
-spec setWindowStyle(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowstyleflag">external documentation</a>.
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxWindow(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowStyleFlag,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetwindowvariant">external documentation</a>.
%%<br /> Variant = ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
-spec setWindowVariant(This, Variant) -> 'ok' when
	This::wxWindow(), Variant::wx:wx_enum().
setWindowVariant(#wx_ref{type=ThisT,ref=ThisRef},Variant)
 when is_integer(Variant) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowVariant,
  <<ThisRef:32/?UI,Variant:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowshouldinheritcolours">external documentation</a>.
-spec shouldInheritColours(This) -> boolean() when
	This::wxWindow().
shouldInheritColours(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ShouldInheritColours,
  <<ThisRef:32/?UI>>).

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
show(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Show,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowthaw">external documentation</a>.
-spec thaw(This) -> 'ok' when
	This::wxWindow().
thaw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Thaw,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatafromwindow">external documentation</a>.
-spec transferDataFromWindow(This) -> boolean() when
	This::wxWindow().
transferDataFromWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_TransferDataFromWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowtransferdatatowindow">external documentation</a>.
-spec transferDataToWindow(This) -> boolean() when
	This::wxWindow().
transferDataToWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_TransferDataToWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowupdate">external documentation</a>.
-spec update(This) -> 'ok' when
	This::wxWindow().
update(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Update,
  <<ThisRef:32/?UI>>).

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
updateWindowUI(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_UpdateWindowUI,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowvalidate">external documentation</a>.
-spec validate(This) -> boolean() when
	This::wxWindow().
validate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Validate,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowwarppointer">external documentation</a>.
-spec warpPointer(This, X, Y) -> 'ok' when
	This::wxWindow(), X::integer(), Y::integer().
warpPointer(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_WarpPointer,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsettransparent">external documentation</a>.
-spec setTransparent(This, Alpha) -> boolean() when
	This::wxWindow(), Alpha::integer().
setTransparent(#wx_ref{type=ThisT,ref=ThisRef},Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetTransparent,
  <<ThisRef:32/?UI,Alpha:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowcansettransparent">external documentation</a>.
-spec canSetTransparent(This) -> boolean() when
	This::wxWindow().
canSetTransparent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_CanSetTransparent,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowisdoublebuffered">external documentation</a>.
-spec isDoubleBuffered(This) -> boolean() when
	This::wxWindow().
isDoubleBuffered(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsDoubleBuffered,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwindow.html#wxwindowsetdoublebuffered">external documentation</a>.
-spec setDoubleBuffered(This, On) -> 'ok' when
	This::wxWindow(), On::boolean().
setDoubleBuffered(#wx_ref{type=ThisT,ref=ThisRef},On)
 when is_boolean(On) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetDoubleBuffered,
  <<ThisRef:32/?UI,(wxe_util:from_bool(On)):32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxWindow),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
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
