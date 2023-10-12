%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2020. All Rights Reserved.
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

-module(wxSplitterWindow).
-include("wxe.hrl").
-export([create/2,create/3,destroy/1,getMinimumPaneSize/1,getSashGravity/1,
  getSashPosition/1,getSplitMode/1,getWindow1/1,getWindow2/1,initialize/2,
  isSplit/1,new/0,new/1,new/2,replaceWindow/3,setMinimumPaneSize/2,setSashGravity/2,
  setSashPosition/2,setSashPosition/3,setSplitMode/2,splitHorizontally/3,
  splitHorizontally/4,splitVertically/3,splitVertically/4,unsplit/1,
  unsplit/2,updateSize/1]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
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
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxSplitterWindow() :: wx:wx_object().
-export_type([wxSplitterWindow/0]).
%% @hidden
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowwxsplitterwindow">external documentation</a>.
-spec new() -> wxSplitterWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxSplitterWindow_new_0),
  wxe_util:rec(?wxSplitterWindow_new_0).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxSplitterWindow() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowwxsplitterwindow">external documentation</a>.
-spec new(Parent, [Option]) -> wxSplitterWindow() when
	Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxSplitterWindow_new_2),
  wxe_util:rec(?wxSplitterWindow_new_2).

%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
	This::wxSplitterWindow(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> boolean() when
	This::wxSplitterWindow(), Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent, Opts,?get_env(),?wxSplitterWindow_Create),
  wxe_util:rec(?wxSplitterWindow_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetminimumpanesize">external documentation</a>.
-spec getMinimumPaneSize(This) -> integer() when
	This::wxSplitterWindow().
getMinimumPaneSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetMinimumPaneSize),
  wxe_util:rec(?wxSplitterWindow_GetMinimumPaneSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsashgravity">external documentation</a>.
-spec getSashGravity(This) -> number() when
	This::wxSplitterWindow().
getSashGravity(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSashGravity),
  wxe_util:rec(?wxSplitterWindow_GetSashGravity).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsashposition">external documentation</a>.
-spec getSashPosition(This) -> integer() when
	This::wxSplitterWindow().
getSashPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSashPosition),
  wxe_util:rec(?wxSplitterWindow_GetSashPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsplitmode">external documentation</a>.
%%<br /> Res = ?wxSPLIT_HORIZONTAL | ?wxSPLIT_VERTICAL
-spec getSplitMode(This) -> wx:wx_enum() when
	This::wxSplitterWindow().
getSplitMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSplitMode),
  wxe_util:rec(?wxSplitterWindow_GetSplitMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetwindow1">external documentation</a>.
-spec getWindow1(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow1(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow1),
  wxe_util:rec(?wxSplitterWindow_GetWindow1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetwindow2">external documentation</a>.
-spec getWindow2(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow2(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow2),
  wxe_util:rec(?wxSplitterWindow_GetWindow2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowinitialize">external documentation</a>.
-spec initialize(This, Window) -> 'ok' when
	This::wxSplitterWindow(), Window::wxWindow:wxWindow().
initialize(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSplitterWindow_Initialize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowissplit">external documentation</a>.
-spec isSplit(This) -> boolean() when
	This::wxSplitterWindow().
isSplit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_IsSplit),
  wxe_util:rec(?wxSplitterWindow_IsSplit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowreplacewindow">external documentation</a>.
-spec replaceWindow(This, WinOld, WinNew) -> boolean() when
	This::wxSplitterWindow(), WinOld::wxWindow:wxWindow(), WinNew::wxWindow:wxWindow().
replaceWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=WinOldT}=WinOld,#wx_ref{type=WinNewT}=WinNew) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(WinOldT,wxWindow),
  ?CLASS(WinNewT,wxWindow),
  wxe_util:queue_cmd(This,WinOld,WinNew,?get_env(),?wxSplitterWindow_ReplaceWindow),
  wxe_util:rec(?wxSplitterWindow_ReplaceWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetsashgravity">external documentation</a>.
-spec setSashGravity(This, Gravity) -> 'ok' when
	This::wxSplitterWindow(), Gravity::number().
setSashGravity(#wx_ref{type=ThisT}=This,Gravity)
 when is_number(Gravity) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,Gravity,?get_env(),?wxSplitterWindow_SetSashGravity).

%% @equiv setSashPosition(This,Position, [])
-spec setSashPosition(This, Position) -> 'ok' when
	This::wxSplitterWindow(), Position::integer().

setSashPosition(This,Position)
 when is_record(This, wx_ref),is_integer(Position) ->
  setSashPosition(This,Position, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetsashposition">external documentation</a>.
-spec setSashPosition(This, Position, [Option]) -> 'ok' when
	This::wxSplitterWindow(), Position::integer(),
	Option :: {'redraw', boolean()}.
setSashPosition(#wx_ref{type=ThisT}=This,Position, Options)
 when is_integer(Position),is_list(Options) ->
  ?CLASS(ThisT,wxSplitterWindow),
  MOpts = fun({redraw, _redraw} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Position, Opts,?get_env(),?wxSplitterWindow_SetSashPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetminimumpanesize">external documentation</a>.
-spec setMinimumPaneSize(This, PaneSize) -> 'ok' when
	This::wxSplitterWindow(), PaneSize::integer().
setMinimumPaneSize(#wx_ref{type=ThisT}=This,PaneSize)
 when is_integer(PaneSize) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,PaneSize,?get_env(),?wxSplitterWindow_SetMinimumPaneSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetsplitmode">external documentation</a>.
-spec setSplitMode(This, Mode) -> 'ok' when
	This::wxSplitterWindow(), Mode::integer().
setSplitMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxSplitterWindow_SetSplitMode).

%% @equiv splitHorizontally(This,Window1,Window2, [])
-spec splitHorizontally(This, Window1, Window2) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow().

splitHorizontally(This,Window1,Window2)
 when is_record(This, wx_ref),is_record(Window1, wx_ref),is_record(Window2, wx_ref) ->
  splitHorizontally(This,Window1,Window2, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsplithorizontally">external documentation</a>.
-spec splitHorizontally(This, Window1, Window2, [Option]) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow(),
	Option :: {'sashPosition', integer()}.
splitHorizontally(#wx_ref{type=ThisT}=This,#wx_ref{type=Window1T}=Window1,#wx_ref{type=Window2T}=Window2, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(Window1T,wxWindow),
  ?CLASS(Window2T,wxWindow),
  MOpts = fun({sashPosition, _sashPosition} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Window1,Window2, Opts,?get_env(),?wxSplitterWindow_SplitHorizontally),
  wxe_util:rec(?wxSplitterWindow_SplitHorizontally).

%% @equiv splitVertically(This,Window1,Window2, [])
-spec splitVertically(This, Window1, Window2) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow().

splitVertically(This,Window1,Window2)
 when is_record(This, wx_ref),is_record(Window1, wx_ref),is_record(Window2, wx_ref) ->
  splitVertically(This,Window1,Window2, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsplitvertically">external documentation</a>.
-spec splitVertically(This, Window1, Window2, [Option]) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow(),
	Option :: {'sashPosition', integer()}.
splitVertically(#wx_ref{type=ThisT}=This,#wx_ref{type=Window1T}=Window1,#wx_ref{type=Window2T}=Window2, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(Window1T,wxWindow),
  ?CLASS(Window2T,wxWindow),
  MOpts = fun({sashPosition, _sashPosition} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Window1,Window2, Opts,?get_env(),?wxSplitterWindow_SplitVertically),
  wxe_util:rec(?wxSplitterWindow_SplitVertically).

%% @equiv unsplit(This, [])
-spec unsplit(This) -> boolean() when
	This::wxSplitterWindow().

unsplit(This)
 when is_record(This, wx_ref) ->
  unsplit(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowunsplit">external documentation</a>.
-spec unsplit(This, [Option]) -> boolean() when
	This::wxSplitterWindow(),
	Option :: {'toRemove', wxWindow:wxWindow()}.
unsplit(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSplitterWindow),
  MOpts = fun({toRemove, #wx_ref{type=ToRemoveT}} = Arg) ->   ?CLASS(ToRemoveT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxSplitterWindow_Unsplit),
  wxe_util:rec(?wxSplitterWindow_Unsplit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowupdatesize">external documentation</a>.
-spec updateSize(This) -> 'ok' when
	This::wxSplitterWindow().
updateSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_UpdateSize).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxSplitterWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSplitterWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxWindow
%% @hidden
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
%% @hidden
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
%% @hidden
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
validate(This) -> wxWindow:validate(This).
%% @hidden
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
update(This) -> wxWindow:update(This).
%% @hidden
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
thaw(This) -> wxWindow:thaw(This).
%% @hidden
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
show(This) -> wxWindow:show(This).
%% @hidden
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
%% @hidden
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
%% @hidden
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
%% @hidden
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
%% @hidden
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
refresh(This) -> wxWindow:refresh(This).
%% @hidden
raise(This) -> wxWindow:raise(This).
%% @hidden
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
navigate(This) -> wxWindow:navigate(This).
%% @hidden
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
%% @hidden
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
%% @hidden
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
isShown(This) -> wxWindow:isShown(This).
%% @hidden
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
isFrozen(This) -> wxWindow:isFrozen(This).
%% @hidden
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
initDialog(This) -> wxWindow:initDialog(This).
%% @hidden
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
hide(This) -> wxWindow:hide(This).
%% @hidden
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
%% @hidden
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
getRect(This) -> wxWindow:getRect(This).
%% @hidden
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
getParent(This) -> wxWindow:getParent(This).
%% @hidden
getName(This) -> wxWindow:getName(This).
%% @hidden
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
getLabel(This) -> wxWindow:getLabel(This).
%% @hidden
getId(This) -> wxWindow:getId(This).
%% @hidden
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
getFont(This) -> wxWindow:getFont(This).
%% @hidden
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
%% @hidden
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
freeze(This) -> wxWindow:freeze(This).
%% @hidden
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
fit(This) -> wxWindow:fit(This).
%% @hidden
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
%% @hidden
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
%% @hidden
disable(This) -> wxWindow:disable(This).
%% @hidden
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
close(This) -> wxWindow:close(This).
%% @hidden
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
%% @hidden
center(This) -> wxWindow:center(This).
%% @hidden
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
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
