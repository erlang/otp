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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html">wxWindow</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxWindow().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxWindow).
-include("wxe.hrl").
-export(['Destroy'/1,cacheBestSize/2,captureMouse/1,center/1,center/2,centerOnParent/1,
  centerOnParent/2,centre/1,centre/2,centreOnParent/1,centreOnParent/2,
  clearBackground/1,clientToScreen/2,clientToScreen/3,close/1,close/2,
  convertDialogToPixels/2,convertPixelsToDialog/2,destroy/1,destroyChildren/1,
  disable/1,enable/1,enable/2,findFocus/0,findWindow/2,findWindowById/1,
  findWindowById/2,findWindowByLabel/1,findWindowByLabel/2,findWindowByName/1,
  findWindowByName/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
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
  initDialog/1,invalidateBestSize/1,isEnabled/1,isExposed/2,isExposed/3,
  isExposed/5,isRetained/1,isShown/1,isTopLevel/1,layout/1,lineDown/1,
  lineUp/1,lower/1,makeModal/1,makeModal/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,new/0,new/2,new/3,pageDown/1,
  pageUp/1,popEventHandler/1,popEventHandler/2,popupMenu/2,popupMenu/3,
  popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,screenToClient/2,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,
  setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setVirtualSize/2,setVirtualSize/3,
  setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
new() ->
  wxe_util:construct(?wxWindow_new_0,
  <<>>).

%% @spec (Parent::wxWindow(), Id::integer()) -> wxWindow()
%% @equiv new(Parent,Id, [])
new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @spec (Parent::wxWindow(), Id::integer(), [Option]) -> wxWindow()
%% Option = {pos, {X::integer(),Y::integer()}} | {size, {W::integer(),H::integer()}} | {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowwxwindow">external documentation</a>.
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

%% @spec (This::wxWindow(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcachebestsize">external documentation</a>.
cacheBestSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_CacheBestSize,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcapturemouse">external documentation</a>.
captureMouse(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_CaptureMouse,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv center(This, [])
center(This)
 when is_record(This, wx_ref) ->
  center(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {dir, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcenter">external documentation</a>.
center(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Center,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv centerOnParent(This, [])
centerOnParent(This)
 when is_record(This, wx_ref) ->
  centerOnParent(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {dir, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcenteronparent">external documentation</a>.
centerOnParent(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_CenterOnParent,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv centre(This, [])
centre(This)
 when is_record(This, wx_ref) ->
  centre(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {dir, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcentre">external documentation</a>.
centre(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Centre,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv centreOnParent(This, [])
centreOnParent(This)
 when is_record(This, wx_ref) ->
  centreOnParent(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {dir, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowcentreonparent">external documentation</a>.
centreOnParent(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({dir, Dir}, Acc) -> [<<1:32/?UI,Dir:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_CentreOnParent,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowclearbackground">external documentation</a>.
clearBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_ClearBackground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Pt::{X::integer(),Y::integer()}) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
clientToScreen(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ClientToScreen_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxWindow(), X::integer(), Y::integer()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowclienttoscreen">external documentation</a>.
clientToScreen(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ClientToScreen_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @equiv close(This, [])
close(This)
 when is_record(This, wx_ref) ->
  close(This, []).

%% @spec (This::wxWindow(), [Option]) -> bool()
%% Option = {force, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowclose">external documentation</a>.
close(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({force, Force}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Force)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Close,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Sz::{W::integer(),H::integer()}) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowconvertdialogtopixels">external documentation</a>.
convertDialogToPixels(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ConvertDialogToPixels,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxWindow(), Sz::{W::integer(),H::integer()}) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowconvertpixelstodialog">external documentation</a>.
convertPixelsToDialog(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ConvertPixelsToDialog,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowdestroy">external documentation</a>.
'Destroy'(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Destroy,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowdestroychildren">external documentation</a>.
destroyChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_DestroyChildren,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowdisable">external documentation</a>.
disable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Disable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @equiv enable(This, [])
enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @spec (This::wxWindow(), [Option]) -> bool()
%% Option = {enable, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowenable">external documentation</a>.
enable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Enable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec () -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfindfocus">external documentation</a>.
findFocus() ->
  wxe_util:call(?wxWindow_FindFocus,
  <<>>).

%% @spec (This::wxWindow(),X::integer()|string()) -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfindwindow">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% findWindow(This::wxWindow(), Winid::integer()) -> wxWindow() </c>
%% </p>
%% <p><c>
%% findWindow(This::wxWindow(), Name::string()) -> wxWindow() </c>
%% </p>
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

%% @spec (Winid::integer()) -> wxWindow()
%% @equiv findWindowById(Winid, [])
findWindowById(Winid)
 when is_integer(Winid) ->
  findWindowById(Winid, []).

%% @spec (Winid::integer(), [Option]) -> wxWindow()
%% Option = {parent, wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfindwindowbyid">external documentation</a>.
findWindowById(Winid, Options)
 when is_integer(Winid),is_list(Options) ->
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowById,
  <<Winid:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (Name::string()) -> wxWindow()
%% @equiv findWindowByName(Name, [])
findWindowByName(Name)
 when is_list(Name) ->
  findWindowByName(Name, []).

%% @spec (Name::string(), [Option]) -> wxWindow()
%% Option = {parent, wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfindwindowbyname">external documentation</a>.
findWindowByName(Name, Options)
 when is_list(Name),is_list(Options) ->
  Name_UC = unicode:characters_to_binary([Name,0]),
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowByName,
  <<(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (Label::string()) -> wxWindow()
%% @equiv findWindowByLabel(Label, [])
findWindowByLabel(Label)
 when is_list(Label) ->
  findWindowByLabel(Label, []).

%% @spec (Label::string(), [Option]) -> wxWindow()
%% Option = {parent, wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfindwindowbylabel">external documentation</a>.
findWindowByLabel(Label, Options)
 when is_list(Label),is_list(Options) ->
  Label_UC = unicode:characters_to_binary([Label,0]),
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_FindWindowByLabel,
  <<(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfit">external documentation</a>.
fit(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Fit,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfitinside">external documentation</a>.
fitInside(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_FitInside,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowfreeze">external documentation</a>.
freeze(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Freeze,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxAcceleratorTable:wxAcceleratorTable()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetacceleratortable">external documentation</a>.
getAcceleratorTable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetAcceleratorTable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetbackgroundcolour">external documentation</a>.
getBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> WxBackgroundStyle
%% WxBackgroundStyle = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetbackgroundstyle">external documentation</a>.
%%<br /> WxBackgroundStyle is one of ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_CUSTOM
getBackgroundStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBackgroundStyle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetbestsize">external documentation</a>.
getBestSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetBestSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxCaret:wxCaret()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcaret">external documentation</a>.
getCaret(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCaret,
  <<ThisRef:32/?UI>>).

%% @spec () -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcapture">external documentation</a>.
getCapture() ->
  wxe_util:call(?wxWindow_GetCapture,
  <<>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcharheight">external documentation</a>.
getCharHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCharHeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcharwidth">external documentation</a>.
getCharWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCharWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> [wxWindow()]
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetchildren">external documentation</a>.
getChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetChildren,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetclientsize">external documentation</a>.
getClientSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetClientSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxSizer:wxSizer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcontainingsizer">external documentation</a>.
getContainingSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetContainingSizer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxCursor:wxCursor()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetcursor">external documentation</a>.
getCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetCursor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxDropTarget:wxDropTarget()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetdroptarget">external documentation</a>.
getDropTarget(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetDropTarget,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxEvtHandler:wxEvtHandler()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgeteventhandler">external documentation</a>.
getEventHandler(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetEventHandler,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetextrastyle">external documentation</a>.
getExtraStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetExtraStyle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetfont">external documentation</a>.
getFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetFont,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetforegroundcolour">external documentation</a>.
getForegroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetForegroundColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetgrandparent">external documentation</a>.
getGrandParent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetGrandParent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgethandle">external documentation</a>.
getHandle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetHandle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgethelptext">external documentation</a>.
getHelpText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetHelpText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetid">external documentation</a>.
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetlabel">external documentation</a>.
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetLabel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetmaxsize">external documentation</a>.
getMaxSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetMaxSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetminsize">external documentation</a>.
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetname">external documentation</a>.
getName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetName,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetparent">external documentation</a>.
getParent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetParent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetrect">external documentation</a>.
getRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetRect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetscreenposition">external documentation</a>.
getScreenPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScreenPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetscreenrect">external documentation</a>.
getScreenRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScreenRect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Orient::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetscrollpos">external documentation</a>.
getScrollPos(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollPos,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @spec (This::wxWindow(), Orient::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetscrollrange">external documentation</a>.
getScrollRange(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollRange,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @spec (This::wxWindow(), Orient::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetscrollthumb">external documentation</a>.
getScrollThumb(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetScrollThumb,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxSizer:wxSizer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetsizer">external documentation</a>.
getSizer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetSizer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), String::string()) -> {X::integer(),Y::integer(),Descent::integer(),ExternalLeading::integer()}
%% @equiv getTextExtent(This,String, [])
getTextExtent(This,String)
 when is_record(This, wx_ref),is_list(String) ->
  getTextExtent(This,String, []).

%% @spec (This::wxWindow(), String::string(), [Option]) -> {X::integer(),Y::integer(),Descent::integer(),ExternalLeading::integer()}
%% Option = {theFont, wxFont:wxFont()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgettextextent">external documentation</a>.
getTextExtent(#wx_ref{type=ThisT,ref=ThisRef},String, Options)
 when is_list(String),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  String_UC = unicode:characters_to_binary([String,0]),
  MOpts = fun({theFont, #wx_ref{type=TheFontT,ref=TheFontRef}}, Acc) ->   ?CLASS(TheFontT,wxFont),[<<1:32/?UI,TheFontRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_GetTextExtent,
  <<ThisRef:32/?UI,(byte_size(String_UC)):32/?UI,(String_UC)/binary, 0:(((8- ((0+byte_size(String_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxWindow()) -> wxToolTip:wxToolTip()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgettooltip">external documentation</a>.
getToolTip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetToolTip,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxRegion:wxRegion()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetupdateregion">external documentation</a>.
getUpdateRegion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetUpdateRegion,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetvirtualsize">external documentation</a>.
getVirtualSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetVirtualSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetwindowstyleflag">external documentation</a>.
getWindowStyleFlag(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetWindowStyleFlag,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> WxWindowVariant
%% WxWindowVariant = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowgetwindowvariant">external documentation</a>.
%%<br /> WxWindowVariant is one of ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
getWindowVariant(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_GetWindowVariant,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowhascapture">external documentation</a>.
hasCapture(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasCapture,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Orient::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowhasscrollbar">external documentation</a>.
hasScrollbar(#wx_ref{type=ThisT,ref=ThisRef},Orient)
 when is_integer(Orient) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasScrollbar,
  <<ThisRef:32/?UI,Orient:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowhastransparentbackground">external documentation</a>.
hasTransparentBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_HasTransparentBackground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowhide">external documentation</a>.
hide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Hide,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowinheritattributes">external documentation</a>.
inheritAttributes(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InheritAttributes,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowinitdialog">external documentation</a>.
initDialog(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InitDialog,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowinvalidatebestsize">external documentation</a>.
invalidateBestSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_InvalidateBestSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisenabled">external documentation</a>.
isEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsEnabled,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(),X::term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% isExposed(This::wxWindow(), Pt::{X::integer(),Y::integer()}) -> bool() </c>
%% </p>
%% <p><c>
%% isExposed(This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool() </c>
%% </p>
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

%% @spec (This::wxWindow(), X::integer(), Y::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
isExposed(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxWindow(), X::integer(), Y::integer(), W::integer(), H::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisexposed">external documentation</a>.
isExposed(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsExposed_4,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,W:32/?UI,H:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisretained">external documentation</a>.
isRetained(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsRetained,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowisshown">external documentation</a>.
isShown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsShown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowistoplevel">external documentation</a>.
isTopLevel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_IsTopLevel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowlayout">external documentation</a>.
layout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Layout,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowlinedown">external documentation</a>.
lineDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_LineDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowlineup">external documentation</a>.
lineUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_LineUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowlower">external documentation</a>.
lower(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Lower,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv makeModal(This, [])
makeModal(This)
 when is_record(This, wx_ref) ->
  makeModal(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {modal, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowmakemodal">external documentation</a>.
makeModal(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({modal, Modal}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Modal)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_MakeModal,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Pt::{X::integer(),Y::integer()}) -> ok
%% @equiv move(This,Pt, [])
move(This,Pt={PtX,PtY})
 when is_record(This, wx_ref),is_integer(PtX),is_integer(PtY) ->
  move(This,Pt, []).

%% @spec (This::wxWindow(),X::integer()|term(),X::integer()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowmove">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% move(This::wxWindow(), X::integer(), Y::integer()) -> move(This,X,Y, []) </c></p>
%% <p><c>
%% move(This::wxWindow(), Pt::{X::integer(),Y::integer()}, [Option]) -> ok </c>
%%<br /> Option = {flags, integer()}
%% </p>

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

%% @spec (This::wxWindow(), X::integer(), Y::integer(), [Option]) -> ok
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowmove">external documentation</a>.
move(#wx_ref{type=ThisT,ref=ThisRef},X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Move_3,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Win::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowmoveafterintaborder">external documentation</a>.
moveAfterInTabOrder(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxWindow_MoveAfterInTabOrder,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

%% @spec (This::wxWindow(), Win::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowmovebeforeintaborder">external documentation</a>.
moveBeforeInTabOrder(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(WinT,wxWindow),
  wxe_util:cast(?wxWindow_MoveBeforeInTabOrder,
  <<ThisRef:32/?UI,WinRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @equiv navigate(This, [])
navigate(This)
 when is_record(This, wx_ref) ->
  navigate(This, []).

%% @spec (This::wxWindow(), [Option]) -> bool()
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindownavigate">external documentation</a>.
navigate(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Navigate,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowpagedown">external documentation</a>.
pageDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_PageDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowpageup">external documentation</a>.
pageUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_PageUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> wxEvtHandler:wxEvtHandler()
%% @equiv popEventHandler(This, [])
popEventHandler(This)
 when is_record(This, wx_ref) ->
  popEventHandler(This, []).

%% @spec (This::wxWindow(), [Option]) -> wxEvtHandler:wxEvtHandler()
%% Option = {deleteHandler, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowpopeventhandler">external documentation</a>.
popEventHandler(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({deleteHandler, DeleteHandler}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteHandler)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_PopEventHandler,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Menu::wxMenu:wxMenu()) -> bool()
%% @equiv popupMenu(This,Menu, [])
popupMenu(This,Menu)
 when is_record(This, wx_ref),is_record(Menu, wx_ref) ->
  popupMenu(This,Menu, []).

%% @spec (This::wxWindow(), Menu::wxMenu:wxMenu(), [Option]) -> bool()
%% Option = {pos, {X::integer(),Y::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
popupMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  MOpts = fun({pos, {PosX,PosY}}, Acc) -> [<<1:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_PopupMenu_2,
  <<ThisRef:32/?UI,MenuRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxWindow(), Menu::wxMenu:wxMenu(), X::integer(), Y::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowpopupmenu">external documentation</a>.
popupMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(MenuT,wxMenu),
  wxe_util:call(?wxWindow_PopupMenu_3,
  <<ThisRef:32/?UI,MenuRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowraise">external documentation</a>.
raise(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Raise,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv refresh(This, [])
refresh(This)
 when is_record(This, wx_ref) ->
  refresh(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {eraseBackground, bool()} | {rect, {X::integer(),Y::integer(),W::integer(),H::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowrefresh">external documentation</a>.
refresh(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, EraseBackground}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(EraseBackground)):32/?UI>>|Acc];
          ({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<2:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_Refresh,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @equiv refreshRect(This,Rect, [])
refreshRect(This,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  refreshRect(This,Rect, []).

%% @spec (This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, [Option]) -> ok
%% Option = {eraseBackground, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowrefreshrect">external documentation</a>.
refreshRect(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH}, Options)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({eraseBackground, EraseBackground}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(EraseBackground)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_RefreshRect,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowreleasemouse">external documentation</a>.
releaseMouse(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_ReleaseMouse,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Child::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowremovechild">external documentation</a>.
removeChild(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ChildT,ref=ChildRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(ChildT,wxWindow),
  wxe_util:cast(?wxWindow_RemoveChild,
  <<ThisRef:32/?UI,ChildRef:32/?UI>>).

%% @spec (This::wxWindow(), NewParent::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowreparent">external documentation</a>.
reparent(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=NewParentT,ref=NewParentRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(NewParentT,wxWindow),
  wxe_util:call(?wxWindow_Reparent,
  <<ThisRef:32/?UI,NewParentRef:32/?UI>>).

%% @spec (This::wxWindow()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
screenToClient(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScreenToClient_2,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Pt::{X::integer(),Y::integer()}) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowscreentoclient">external documentation</a>.
screenToClient(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScreenToClient_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxWindow(), Lines::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowscrolllines">external documentation</a>.
scrollLines(#wx_ref{type=ThisT,ref=ThisRef},Lines)
 when is_integer(Lines) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScrollLines,
  <<ThisRef:32/?UI,Lines:32/?UI>>).

%% @spec (This::wxWindow(), Pages::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowscrollpages">external documentation</a>.
scrollPages(#wx_ref{type=ThisT,ref=ThisRef},Pages)
 when is_integer(Pages) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ScrollPages,
  <<ThisRef:32/?UI,Pages:32/?UI>>).

%% @spec (This::wxWindow(), Dx::integer(), Dy::integer()) -> ok
%% @equiv scrollWindow(This,Dx,Dy, [])
scrollWindow(This,Dx,Dy)
 when is_record(This, wx_ref),is_integer(Dx),is_integer(Dy) ->
  scrollWindow(This,Dx,Dy, []).

%% @spec (This::wxWindow(), Dx::integer(), Dy::integer(), [Option]) -> ok
%% Option = {rect, {X::integer(),Y::integer(),W::integer(),H::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowscrollwindow">external documentation</a>.
scrollWindow(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy, Options)
 when is_integer(Dx),is_integer(Dy),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<1:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_ScrollWindow,
  <<ThisRef:32/?UI,Dx:32/?UI,Dy:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Accel::wxAcceleratorTable:wxAcceleratorTable()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetacceleratortable">external documentation</a>.
setAcceleratorTable(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=AccelT,ref=AccelRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(AccelT,wxAcceleratorTable),
  wxe_util:cast(?wxWindow_SetAcceleratorTable,
  <<ThisRef:32/?UI,AccelRef:32/?UI>>).

%% @spec (This::wxWindow(), AutoLayout::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetautolayout">external documentation</a>.
setAutoLayout(#wx_ref{type=ThisT,ref=ThisRef},AutoLayout)
 when is_boolean(AutoLayout) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetAutoLayout,
  <<ThisRef:32/?UI,(wxe_util:from_bool(AutoLayout)):32/?UI>>).

%% @spec (This::wxWindow(), Colour::wx:colour()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetbackgroundcolour">external documentation</a>.
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxWindow(), Style::WxBackgroundStyle) -> bool()
%% WxBackgroundStyle = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetbackgroundstyle">external documentation</a>.
%%<br /> WxBackgroundStyle is one of ?wxBG_STYLE_SYSTEM | ?wxBG_STYLE_COLOUR | ?wxBG_STYLE_CUSTOM
setBackgroundStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetBackgroundStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @spec (This::wxWindow(), Caret::wxCaret:wxCaret()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetcaret">external documentation</a>.
setCaret(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CaretT,ref=CaretRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CaretT,wxCaret),
  wxe_util:cast(?wxWindow_SetCaret,
  <<ThisRef:32/?UI,CaretRef:32/?UI>>).

%% @spec (This::wxWindow(),X::term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setClientSize(This::wxWindow(), Size::{W::integer(),H::integer()}) -> ok </c>
%% </p>
%% <p><c>
%% setClientSize(This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok </c>
%% </p>
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

%% @spec (This::wxWindow(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetclientsize">external documentation</a>.
setClientSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetClientSize_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxWindow(), Sizer::wxSizer:wxSizer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetcontainingsizer">external documentation</a>.
setContainingSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  wxe_util:cast(?wxWindow_SetContainingSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI>>).

%% @spec (This::wxWindow(), Cursor::wxCursor:wxCursor()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetcursor">external documentation</a>.
setCursor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CursorT,ref=CursorRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(CursorT,wxCursor),
  wxe_util:call(?wxWindow_SetCursor,
  <<ThisRef:32/?UI,CursorRef:32/?UI>>).

%% @spec (This::wxWindow(), MaxSize::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetmaxsize">external documentation</a>.
setMaxSize(#wx_ref{type=ThisT,ref=ThisRef},{MaxSizeW,MaxSizeH})
 when is_integer(MaxSizeW),is_integer(MaxSizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetMaxSize,
  <<ThisRef:32/?UI,MaxSizeW:32/?UI,MaxSizeH:32/?UI>>).

%% @spec (This::wxWindow(), MinSize::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetminsize">external documentation</a>.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{MinSizeW,MinSizeH})
 when is_integer(MinSizeW),is_integer(MinSizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetMinSize,
  <<ThisRef:32/?UI,MinSizeW:32/?UI,MinSizeH:32/?UI>>).

%% @spec (This::wxWindow(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetownbackgroundcolour">external documentation</a>.
setOwnBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetOwnBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxWindow(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetownfont">external documentation</a>.
setOwnFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxWindow_SetOwnFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxWindow(), Colour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetownforegroundcolour">external documentation</a>.
setOwnForegroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetOwnForegroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxWindow(), DropTarget::wxDropTarget:wxDropTarget()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetdroptarget">external documentation</a>.
setDropTarget(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DropTargetT,ref=DropTargetRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(DropTargetT,wxDropTarget),
  wxe_util:cast(?wxWindow_SetDropTarget,
  <<ThisRef:32/?UI,DropTargetRef:32/?UI>>).

%% @spec (This::wxWindow(), ExStyle::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetextrastyle">external documentation</a>.
setExtraStyle(#wx_ref{type=ThisT,ref=ThisRef},ExStyle)
 when is_integer(ExStyle) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetExtraStyle,
  <<ThisRef:32/?UI,ExStyle:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetfocus">external documentation</a>.
setFocus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetFocus,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetfocusfromkbd">external documentation</a>.
setFocusFromKbd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetFocusFromKbd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), Font::wxFont:wxFont()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetfont">external documentation</a>.
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(FontT,wxFont),
  wxe_util:call(?wxWindow_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxWindow(), Colour::wx:colour()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetforegroundcolour">external documentation</a>.
setForegroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_SetForegroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @spec (This::wxWindow(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsethelptext">external documentation</a>.
setHelpText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxWindow),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxWindow_SetHelpText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxWindow(), Winid::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetid">external documentation</a>.
setId(#wx_ref{type=ThisT,ref=ThisRef},Winid)
 when is_integer(Winid) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetId,
  <<ThisRef:32/?UI,Winid:32/?UI>>).

%% @spec (This::wxWindow(), Label::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetlabel">external documentation</a>.
setLabel(#wx_ref{type=ThisT,ref=ThisRef},Label)
 when is_list(Label) ->
  ?CLASS(ThisT,wxWindow),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:cast(?wxWindow_SetLabel,
  <<ThisRef:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((0+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxWindow(), Name::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetname">external documentation</a>.
setName(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:cast(?wxWindow_SetName,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxWindow(), Pal::wxPalette:wxPalette()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetpalette">external documentation</a>.
setPalette(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PalT,ref=PalRef}) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(PalT,wxPalette),
  wxe_util:cast(?wxWindow_SetPalette,
  <<ThisRef:32/?UI,PalRef:32/?UI>>).

%% @spec (This::wxWindow(), Orient::integer(), Pos::integer(), ThumbVisible::integer(), Range::integer()) -> ok
%% @equiv setScrollbar(This,Orient,Pos,ThumbVisible,Range, [])
setScrollbar(This,Orient,Pos,ThumbVisible,Range)
 when is_record(This, wx_ref),is_integer(Orient),is_integer(Pos),is_integer(ThumbVisible),is_integer(Range) ->
  setScrollbar(This,Orient,Pos,ThumbVisible,Range, []).

%% @spec (This::wxWindow(), Orient::integer(), Pos::integer(), ThumbVisible::integer(), Range::integer(), [Option]) -> ok
%% Option = {refresh, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetscrollbar">external documentation</a>.
setScrollbar(#wx_ref{type=ThisT,ref=ThisRef},Orient,Pos,ThumbVisible,Range, Options)
 when is_integer(Orient),is_integer(Pos),is_integer(ThumbVisible),is_integer(Range),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, Refresh}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Refresh)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetScrollbar,
  <<ThisRef:32/?UI,Orient:32/?UI,Pos:32/?UI,ThumbVisible:32/?UI,Range:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Orient::integer(), Pos::integer()) -> ok
%% @equiv setScrollPos(This,Orient,Pos, [])
setScrollPos(This,Orient,Pos)
 when is_record(This, wx_ref),is_integer(Orient),is_integer(Pos) ->
  setScrollPos(This,Orient,Pos, []).

%% @spec (This::wxWindow(), Orient::integer(), Pos::integer(), [Option]) -> ok
%% Option = {refresh, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetscrollpos">external documentation</a>.
setScrollPos(#wx_ref{type=ThisT,ref=ThisRef},Orient,Pos, Options)
 when is_integer(Orient),is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({refresh, Refresh}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Refresh)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetScrollPos,
  <<ThisRef:32/?UI,Orient:32/?UI,Pos:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(),X::term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setSize(This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> setSize(This,Rect, []) </c></p>
%% <p><c>
%% setSize(This::wxWindow(), Size::{W::integer(),H::integer()}) -> ok </c>
%% </p>

setSize(This,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  setSize(This,Rect, []);
setSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxWindow(),X::integer()|term(),X::integer()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setSize(This::wxWindow(), Width::integer(), Height::integer()) -> ok </c>
%% </p>
%% <p><c>
%% setSize(This::wxWindow(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, [Option]) -> ok </c>
%%<br /> Option = {sizeFlags, integer()}
%% </p>
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

%% @spec (This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @equiv setSize(This,X,Y,Width,Height, [])
setSize(This,X,Y,Width,Height)
 when is_record(This, wx_ref),is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  setSize(This,X,Y,Width,Height, []).

%% @spec (This::wxWindow(), X::integer(), Y::integer(), Width::integer(), Height::integer(), [Option]) -> ok
%% Option = {sizeFlags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsize">external documentation</a>.
setSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Width,Height, Options)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({sizeFlags, SizeFlags}, Acc) -> [<<1:32/?UI,SizeFlags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSize_5,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), MinSize::{W::integer(),H::integer()}) -> ok
%% @equiv setSizeHints(This,MinSize, [])
setSizeHints(This,MinSize={MinSizeW,MinSizeH})
 when is_record(This, wx_ref),is_integer(MinSizeW),is_integer(MinSizeH) ->
  setSizeHints(This,MinSize, []).

%% @spec (This::wxWindow(),X::integer()|term(),X::integer()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setSizeHints(This::wxWindow(), MinW::integer(), MinH::integer()) -> setSizeHints(This,MinW,MinH, []) </c></p>
%% <p><c>
%% setSizeHints(This::wxWindow(), MinSize::{W::integer(),H::integer()}, [Option]) -> ok </c>
%%<br /> Option = {maxSize, {W::integer(),H::integer()}} | {incSize, {W::integer(),H::integer()}}
%% </p>

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

%% @spec (This::wxWindow(), MinW::integer(), MinH::integer(), [Option]) -> ok
%% Option = {maxW, integer()} | {maxH, integer()} | {incW, integer()} | {incH, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsizehints">external documentation</a>.
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

%% @spec (This::wxWindow(), Sizer::wxSizer:wxSizer()) -> ok
%% @equiv setSizer(This,Sizer, [])
setSizer(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizer(This,Sizer, []).

%% @spec (This::wxWindow(), Sizer::wxSizer:wxSizer(), [Option]) -> ok
%% Option = {deleteOld, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsizer">external documentation</a>.
setSizer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, DeleteOld}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteOld)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizer,
  <<ThisRef:32/?UI,SizerRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxWindow(), Sizer::wxSizer:wxSizer()) -> ok
%% @equiv setSizerAndFit(This,Sizer, [])
setSizerAndFit(This,Sizer)
 when is_record(This, wx_ref),is_record(Sizer, wx_ref) ->
  setSizerAndFit(This,Sizer, []).

%% @spec (This::wxWindow(), Sizer::wxSizer:wxSizer(), [Option]) -> ok
%% Option = {deleteOld, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetsizerandfit">external documentation</a>.
setSizerAndFit(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  ?CLASS(SizerT,wxSizer),
  MOpts = fun({deleteOld, DeleteOld}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(DeleteOld)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetSizerAndFit,
  <<ThisRef:32/?UI,SizerRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxWindow(), EnableTheme::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetthemeenabled">external documentation</a>.
setThemeEnabled(#wx_ref{type=ThisT,ref=ThisRef},EnableTheme)
 when is_boolean(EnableTheme) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetThemeEnabled,
  <<ThisRef:32/?UI,(wxe_util:from_bool(EnableTheme)):32/?UI>>).

%% @spec (This::wxWindow(),X::string()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsettooltip">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setToolTip(This::wxWindow(), Tip::string()) -> ok </c>
%% </p>
%% <p><c>
%% setToolTip(This::wxWindow(), Tip::wxToolTip:wxToolTip()) -> ok </c>
%% </p>
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

%% @spec (This::wxWindow(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
setVirtualSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetVirtualSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxWindow(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetvirtualsize">external documentation</a>.
setVirtualSize(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetVirtualSize_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxWindow(), MinSize::{W::integer(),H::integer()}) -> ok
%% @equiv setVirtualSizeHints(This,MinSize, [])
setVirtualSizeHints(This,MinSize={MinSizeW,MinSizeH})
 when is_record(This, wx_ref),is_integer(MinSizeW),is_integer(MinSizeH) ->
  setVirtualSizeHints(This,MinSize, []).

%% @spec (This::wxWindow(),X::integer()|term(),X::integer()|term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetvirtualsizehints">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setVirtualSizeHints(This::wxWindow(), MinW::integer(), MinH::integer()) -> setVirtualSizeHints(This,MinW,MinH, []) </c></p>
%% <p><c>
%% setVirtualSizeHints(This::wxWindow(), MinSize::{W::integer(),H::integer()}, [Option]) -> ok </c>
%%<br /> Option = {maxSize, {W::integer(),H::integer()}}
%% </p>

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

%% @spec (This::wxWindow(), MinW::integer(), MinH::integer(), [Option]) -> ok
%% Option = {maxW, integer()} | {maxH, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetvirtualsizehints">external documentation</a>.
setVirtualSizeHints(#wx_ref{type=ThisT,ref=ThisRef},MinW,MinH, Options)
 when is_integer(MinW),is_integer(MinH),is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({maxW, MaxW}, Acc) -> [<<1:32/?UI,MaxW:32/?UI>>|Acc];
          ({maxH, MaxH}, Acc) -> [<<2:32/?UI,MaxH:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_SetVirtualSizeHints_3,
  <<ThisRef:32/?UI,MinW:32/?UI,MinH:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow(), Style::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetwindowstyle">external documentation</a>.
setWindowStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @spec (This::wxWindow(), Style::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetwindowstyleflag">external documentation</a>.
setWindowStyleFlag(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowStyleFlag,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @spec (This::wxWindow(), Variant::WxWindowVariant) -> ok
%% WxWindowVariant = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowsetwindowvariant">external documentation</a>.
%%<br /> WxWindowVariant is one of ?wxWINDOW_VARIANT_NORMAL | ?wxWINDOW_VARIANT_SMALL | ?wxWINDOW_VARIANT_MINI | ?wxWINDOW_VARIANT_LARGE | ?wxWINDOW_VARIANT_MAX
setWindowVariant(#wx_ref{type=ThisT,ref=ThisRef},Variant)
 when is_integer(Variant) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_SetWindowVariant,
  <<ThisRef:32/?UI,Variant:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowshouldinheritcolours">external documentation</a>.
shouldInheritColours(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_ShouldInheritColours,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @equiv show(This, [])
show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @spec (This::wxWindow(), [Option]) -> bool()
%% Option = {show, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxWindow_Show,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowthaw">external documentation</a>.
thaw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Thaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowtransferdatafromwindow">external documentation</a>.
transferDataFromWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_TransferDataFromWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowtransferdatatowindow">external documentation</a>.
transferDataToWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_TransferDataToWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowupdate">external documentation</a>.
update(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_Update,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @equiv updateWindowUI(This, [])
updateWindowUI(This)
 when is_record(This, wx_ref) ->
  updateWindowUI(This, []).

%% @spec (This::wxWindow(), [Option]) -> ok
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowupdatewindowui">external documentation</a>.
updateWindowUI(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWindow),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxWindow_UpdateWindowUI,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowvalidate">external documentation</a>.
validate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:call(?wxWindow_Validate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxWindow(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxwindow.html#wxwindowwarppointer">external documentation</a>.
warpPointer(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxWindow),
  wxe_util:cast(?wxWindow_WarpPointer,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxWindow()) -> ok
%% @doc Destroys this object, do not use object again
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
