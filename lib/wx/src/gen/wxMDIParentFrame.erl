%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-module(wxMDIParentFrame).
-moduledoc """
An MDI (Multiple Document Interface) parent frame is a window which can contain MDI child
frames in its client area which emulates the full desktop.

MDI is a user-interface model in which all the window reside inside the single parent
window as opposed to being separate from each other. It remains popular despite dire
warnings from Microsoft itself (which popularized this model in the first model) that MDI
is obsolete.

An MDI parent frame always has a `m:wxMDIClientWindow` associated with it, which is the
parent for MDI child frames. In the simplest case, the client window takes up the entire
parent frame area but it is also possible to resize it to be smaller in order to have
other windows in the frame, a typical example is using a sidebar along one of the window edges.

The appearance of MDI applications differs between different ports. The classic MDI
model, with child windows which can be independently moved, resized etc, is only available
under MSW, which provides native support for it. In Mac ports, multiple top level windows
are used for the MDI children too and the MDI parent frame itself is invisible, to
accommodate the native look and feel requirements. In all the other ports, a tab-based MDI
implementation (sometimes called TDI) is used and so at most one MDI child is visible at
any moment (child frames are always maximized).

Although it is possible to have multiple MDI parent frames, a typical MDI application
has a single MDI parent frame window inside which multiple MDI child frames, i.e. objects
of class `m:wxMDIChildFrame`, can be created.

## Styles

This class supports the following styles:

There are no special styles for this class, all `m:wxFrame` styles apply to it in the
usual way. The only exception is that wxHSCROLL and wxVSCROLL styles apply not to the
frame itself but to the client window, so that using them enables horizontal and vertical
scrollbars for this window and not the frame.

See:
* `m:wxMDIChildFrame`

* `m:wxMDIClientWindow`

* `m:wxFrame`

* `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxFrame`

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMDIParentFrame](https://docs.wxwidgets.org/3.2/classwx_m_d_i_parent_frame.html)
""".
-include("wxe.hrl").
-export([activateNext/1,activatePrevious/1,arrangeIcons/1,cascade/1,create/4,
  create/5,destroy/1,getActiveChild/1,getClientWindow/1,new/0,new/3,new/4,
  tile/1,tile/2]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centerOnScreen/1,centerOnScreen/2,
  centre/1,centre/2,centreOnParent/1,centreOnParent/2,centreOnScreen/1,
  centreOnScreen/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  createStatusBar/1,createStatusBar/2,createToolBar/1,createToolBar/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientAreaOrigin/1,getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,
  getCursor/1,getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,
  getFont/1,getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getIcon/1,getIcons/1,getId/1,getLabel/1,getMaxSize/1,getMenuBar/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getStatusBar/1,getStatusBarPane/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getTitle/1,getToolBar/1,getToolTip/1,
  getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,iconize/1,
  iconize/2,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isActive/1,
  isDoubleBuffered/1,isEnabled/1,isExposed/2,isExposed/3,isExposed/5,
  isFrozen/1,isFullScreen/1,isIconized/1,isMaximized/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,maximize/1,maximize/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,processCommand/2,raise/1,refresh/1,
  refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,removeChild/2,
  reparent/2,requestUserAttention/1,requestUserAttention/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  sendSizeEvent/1,sendSizeEvent/2,setAcceleratorTable/2,setAutoLayout/2,
  setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,setClientSize/2,
  setClientSize/3,setContainingSizer/2,setCursor/2,setDoubleBuffered/2,
  setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,
  setForegroundColour/2,setHelpText/2,setIcon/2,setIcons/2,setId/2,setLabel/2,
  setMaxSize/2,setMenuBar/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setShape/2,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setStatusBar/2,
  setStatusBarPane/2,setStatusText/2,setStatusText/3,setStatusWidths/2,
  setThemeEnabled/2,setTitle/2,setToolBar/2,setToolTip/2,setTransparent/2,
  setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,showFullScreen/2,
  showFullScreen/3,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxMDIParentFrame() :: wx:wx_object().
-export_type([wxMDIParentFrame/0]).
-doc false.
parent_class(wxFrame) -> true;
parent_class(wxTopLevelWindow) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Default constructor.

Use `create/5` for the objects created using this constructor.
""".
-spec new() -> wxMDIParentFrame().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMDIParentFrame_new_0),
  wxe_util:rec(?wxMDIParentFrame_new_0).

-doc(#{equiv => new(Parent,Id,Title, [])}).
-spec new(Parent, Id, Title) -> wxMDIParentFrame() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

new(Parent,Id,Title)
 when is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  new(Parent,Id,Title, []).

-doc """
Constructor, creating the window.

Notice that if you override virtual `OnCreateClient()` (not implemented in wx) method you
shouldn't be using this constructor but the default constructor and `create/5` as otherwise your
overridden method is never going to be called because of the usual C++ virtual call
resolution rules.

Under wxMSW, the client window will automatically have a sunken border style when the
active child is not maximized, and no border style when a child is maximized.

See: `create/5`
""".
-spec new(Parent, Id, Title, [Option]) -> wxMDIParentFrame() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id,Title, Options)
 when is_integer(Id),?is_chardata(Title),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id,Title_UC, Opts,?get_env(),?wxMDIParentFrame_new_4),
  wxe_util:rec(?wxMDIParentFrame_new_4).

-doc """
Activates the MDI child following the currently active one.

The MDI children are maintained in an ordered list and this function switches to the next
element in this list, wrapping around the end of it if the currently active child is the
last one.

See: `activatePrevious/1`
""".
-spec activateNext(This) -> 'ok' when
	This::wxMDIParentFrame().
activateNext(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_ActivateNext).

-doc """
Activates the MDI child preceding the currently active one.

See: `activateNext/1`
""".
-spec activatePrevious(This) -> 'ok' when
	This::wxMDIParentFrame().
activatePrevious(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_ActivatePrevious).

-doc """
Arranges any iconized (minimized) MDI child windows.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.

See:
* `cascade/1`

* `tile/2`
""".
-spec arrangeIcons(This) -> 'ok' when
	This::wxMDIParentFrame().
arrangeIcons(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_ArrangeIcons).

-doc """
Arranges the MDI child windows in a cascade.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.

See:
* `tile/2`

* `arrangeIcons/1`
""".
-spec cascade(This) -> 'ok' when
	This::wxMDIParentFrame().
cascade(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_Cascade).

-doc(#{equiv => create(This,Parent,Id,Title, [])}).
-spec create(This, Parent, Id, Title) -> boolean() when
	This::wxMDIParentFrame(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

create(This,Parent,Id,Title)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  create(This,Parent,Id,Title, []).

-doc """
Used in two-step frame construction.

See `new/4` for further details.
""".
-spec create(This, Parent, Id, Title, [Option]) -> boolean() when
	This::wxMDIParentFrame(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,Title, Options)
 when is_integer(Id),?is_chardata(Title),is_list(Options) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  ?CLASS(ParentT,wxWindow),
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Title_UC, Opts,?get_env(),?wxMDIParentFrame_Create),
  wxe_util:rec(?wxMDIParentFrame_Create).

-doc """
Returns a pointer to the active MDI child, if there is one.

If there are any children at all this function returns a non-NULL pointer.
""".
-spec getActiveChild(This) -> wxMDIChildFrame:wxMDIChildFrame() when
	This::wxMDIParentFrame().
getActiveChild(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_GetActiveChild),
  wxe_util:rec(?wxMDIParentFrame_GetActiveChild).

-doc "Returns a pointer to the client window.".
-spec getClientWindow(This) -> wxMDIClientWindow:wxMDIClientWindow() when
	This::wxMDIParentFrame().
getClientWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxMDIParentFrame_GetClientWindow),
  wxe_util:rec(?wxMDIParentFrame_GetClientWindow).

-doc(#{equiv => tile(This, [])}).
-spec tile(This) -> 'ok' when
	This::wxMDIParentFrame().

tile(This)
 when is_record(This, wx_ref) ->
  tile(This, []).

-doc """
Tiles the MDI child windows either horizontally or vertically depending on whether `orient`
is `wxHORIZONTAL` or `wxVERTICAL`.

This method is only implemented in MSW MDI implementation and does nothing under the
other platforms.
""".
%%  Orient = ?wxHORIZONTAL | ?wxVERTICAL | ?wxBOTH | ?wxORIENTATION_MASK
-spec tile(This, [Option]) -> 'ok' when
	This::wxMDIParentFrame(),
	Option :: {'orient', wx:wx_enum()}.
tile(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMDIParentFrame),
  MOpts = fun({orient, _orient} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMDIParentFrame_Tile).

-doc "Destroys the object".
-spec destroy(This::wxMDIParentFrame()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMDIParentFrame),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxFrame
-doc false.
setToolBar(This,ToolBar) -> wxFrame:setToolBar(This,ToolBar).
-doc false.
setStatusWidths(This,Widths_field) -> wxFrame:setStatusWidths(This,Widths_field).
-doc false.
setStatusText(This,Text, Options) -> wxFrame:setStatusText(This,Text, Options).
-doc false.
setStatusText(This,Text) -> wxFrame:setStatusText(This,Text).
-doc false.
setStatusBarPane(This,N) -> wxFrame:setStatusBarPane(This,N).
-doc false.
setStatusBar(This,StatusBar) -> wxFrame:setStatusBar(This,StatusBar).
-doc false.
setMenuBar(This,MenuBar) -> wxFrame:setMenuBar(This,MenuBar).
-doc false.
sendSizeEvent(This, Options) -> wxFrame:sendSizeEvent(This, Options).
-doc false.
sendSizeEvent(This) -> wxFrame:sendSizeEvent(This).
-doc false.
processCommand(This,Id) -> wxFrame:processCommand(This,Id).
-doc false.
getToolBar(This) -> wxFrame:getToolBar(This).
-doc false.
getStatusBarPane(This) -> wxFrame:getStatusBarPane(This).
-doc false.
getStatusBar(This) -> wxFrame:getStatusBar(This).
-doc false.
getMenuBar(This) -> wxFrame:getMenuBar(This).
-doc false.
getClientAreaOrigin(This) -> wxFrame:getClientAreaOrigin(This).
-doc false.
createToolBar(This, Options) -> wxFrame:createToolBar(This, Options).
-doc false.
createToolBar(This) -> wxFrame:createToolBar(This).
-doc false.
createStatusBar(This, Options) -> wxFrame:createStatusBar(This, Options).
-doc false.
createStatusBar(This) -> wxFrame:createStatusBar(This).
 %% From wxTopLevelWindow
-doc false.
showFullScreen(This,Show, Options) -> wxTopLevelWindow:showFullScreen(This,Show, Options).
-doc false.
showFullScreen(This,Show) -> wxTopLevelWindow:showFullScreen(This,Show).
-doc false.
setTitle(This,Title) -> wxTopLevelWindow:setTitle(This,Title).
-doc false.
setShape(This,Region) -> wxTopLevelWindow:setShape(This,Region).
-doc false.
centreOnScreen(This, Options) -> wxTopLevelWindow:centreOnScreen(This, Options).
-doc false.
centerOnScreen(This, Options) -> wxTopLevelWindow:centerOnScreen(This, Options).
-doc false.
centreOnScreen(This) -> wxTopLevelWindow:centreOnScreen(This).
-doc false.
centerOnScreen(This) -> wxTopLevelWindow:centerOnScreen(This).
-doc false.
setIcons(This,Icons) -> wxTopLevelWindow:setIcons(This,Icons).
-doc false.
setIcon(This,Icon) -> wxTopLevelWindow:setIcon(This,Icon).
-doc false.
requestUserAttention(This, Options) -> wxTopLevelWindow:requestUserAttention(This, Options).
-doc false.
requestUserAttention(This) -> wxTopLevelWindow:requestUserAttention(This).
-doc false.
maximize(This, Options) -> wxTopLevelWindow:maximize(This, Options).
-doc false.
maximize(This) -> wxTopLevelWindow:maximize(This).
-doc false.
isMaximized(This) -> wxTopLevelWindow:isMaximized(This).
-doc false.
isIconized(This) -> wxTopLevelWindow:isIconized(This).
-doc false.
isFullScreen(This) -> wxTopLevelWindow:isFullScreen(This).
-doc false.
iconize(This, Options) -> wxTopLevelWindow:iconize(This, Options).
-doc false.
iconize(This) -> wxTopLevelWindow:iconize(This).
-doc false.
isActive(This) -> wxTopLevelWindow:isActive(This).
-doc false.
getTitle(This) -> wxTopLevelWindow:getTitle(This).
-doc false.
getIcons(This) -> wxTopLevelWindow:getIcons(This).
-doc false.
getIcon(This) -> wxTopLevelWindow:getIcon(This).
 %% From wxWindow
-doc false.
getDPI(This) -> wxWindow:getDPI(This).
-doc false.
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
-doc false.
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
-doc false.
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
-doc false.
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
-doc false.
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
-doc false.
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
-doc false.
validate(This) -> wxWindow:validate(This).
-doc false.
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
-doc false.
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
-doc false.
update(This) -> wxWindow:update(This).
-doc false.
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
-doc false.
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
-doc false.
thaw(This) -> wxWindow:thaw(This).
-doc false.
show(This, Options) -> wxWindow:show(This, Options).
-doc false.
show(This) -> wxWindow:show(This).
-doc false.
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
-doc false.
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
-doc false.
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
-doc false.
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
-doc false.
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
-doc false.
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
-doc false.
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
-doc false.
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
-doc false.
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
-doc false.
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
-doc false.
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
-doc false.
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
-doc false.
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
-doc false.
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
-doc false.
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
-doc false.
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
-doc false.
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
-doc false.
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
-doc false.
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
-doc false.
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
-doc false.
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
-doc false.
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
-doc false.
setName(This,Name) -> wxWindow:setName(This,Name).
-doc false.
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
-doc false.
setId(This,Winid) -> wxWindow:setId(This,Winid).
-doc false.
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
-doc false.
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
-doc false.
setFont(This,Font) -> wxWindow:setFont(This,Font).
-doc false.
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
-doc false.
setFocus(This) -> wxWindow:setFocus(This).
-doc false.
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
-doc false.
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
-doc false.
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
-doc false.
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
-doc false.
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
-doc false.
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
-doc false.
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
-doc false.
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
-doc false.
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
-doc false.
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
-doc false.
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
-doc false.
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
-doc false.
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
-doc false.
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
-doc false.
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
-doc false.
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
-doc false.
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
-doc false.
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
-doc false.
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
-doc false.
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
-doc false.
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
-doc false.
screenToClient(This) -> wxWindow:screenToClient(This).
-doc false.
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
-doc false.
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
-doc false.
releaseMouse(This) -> wxWindow:releaseMouse(This).
-doc false.
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
-doc false.
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
-doc false.
refresh(This, Options) -> wxWindow:refresh(This, Options).
-doc false.
refresh(This) -> wxWindow:refresh(This).
-doc false.
raise(This) -> wxWindow:raise(This).
-doc false.
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
-doc false.
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
-doc false.
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
-doc false.
pageUp(This) -> wxWindow:pageUp(This).
-doc false.
pageDown(This) -> wxWindow:pageDown(This).
-doc false.
navigate(This, Options) -> wxWindow:navigate(This, Options).
-doc false.
navigate(This) -> wxWindow:navigate(This).
-doc false.
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
-doc false.
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
-doc false.
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
-doc false.
move(This,X,Y) -> wxWindow:move(This,X,Y).
-doc false.
move(This,Pt) -> wxWindow:move(This,Pt).
-doc false.
lower(This) -> wxWindow:lower(This).
-doc false.
lineUp(This) -> wxWindow:lineUp(This).
-doc false.
lineDown(This) -> wxWindow:lineDown(This).
-doc false.
layout(This) -> wxWindow:layout(This).
-doc false.
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
-doc false.
isTopLevel(This) -> wxWindow:isTopLevel(This).
-doc false.
isShown(This) -> wxWindow:isShown(This).
-doc false.
isRetained(This) -> wxWindow:isRetained(This).
-doc false.
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
-doc false.
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
-doc false.
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
-doc false.
isEnabled(This) -> wxWindow:isEnabled(This).
-doc false.
isFrozen(This) -> wxWindow:isFrozen(This).
-doc false.
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
-doc false.
initDialog(This) -> wxWindow:initDialog(This).
-doc false.
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
-doc false.
hide(This) -> wxWindow:hide(This).
-doc false.
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
-doc false.
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
-doc false.
hasCapture(This) -> wxWindow:hasCapture(This).
-doc false.
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
-doc false.
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
-doc false.
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
-doc false.
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
-doc false.
getToolTip(This) -> wxWindow:getToolTip(This).
-doc false.
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
-doc false.
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
-doc false.
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
-doc false.
getSizer(This) -> wxWindow:getSizer(This).
-doc false.
getSize(This) -> wxWindow:getSize(This).
-doc false.
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
-doc false.
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
-doc false.
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
-doc false.
getScreenRect(This) -> wxWindow:getScreenRect(This).
-doc false.
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
-doc false.
getRect(This) -> wxWindow:getRect(This).
-doc false.
getPosition(This) -> wxWindow:getPosition(This).
-doc false.
getParent(This) -> wxWindow:getParent(This).
-doc false.
getName(This) -> wxWindow:getName(This).
-doc false.
getMinSize(This) -> wxWindow:getMinSize(This).
-doc false.
getMaxSize(This) -> wxWindow:getMaxSize(This).
-doc false.
getLabel(This) -> wxWindow:getLabel(This).
-doc false.
getId(This) -> wxWindow:getId(This).
-doc false.
getHelpText(This) -> wxWindow:getHelpText(This).
-doc false.
getHandle(This) -> wxWindow:getHandle(This).
-doc false.
getGrandParent(This) -> wxWindow:getGrandParent(This).
-doc false.
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
-doc false.
getFont(This) -> wxWindow:getFont(This).
-doc false.
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
-doc false.
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
-doc false.
getDropTarget(This) -> wxWindow:getDropTarget(This).
-doc false.
getCursor(This) -> wxWindow:getCursor(This).
-doc false.
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
-doc false.
getClientSize(This) -> wxWindow:getClientSize(This).
-doc false.
getChildren(This) -> wxWindow:getChildren(This).
-doc false.
getCharWidth(This) -> wxWindow:getCharWidth(This).
-doc false.
getCharHeight(This) -> wxWindow:getCharHeight(This).
-doc false.
getCaret(This) -> wxWindow:getCaret(This).
-doc false.
getBestSize(This) -> wxWindow:getBestSize(This).
-doc false.
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
-doc false.
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
-doc false.
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
-doc false.
freeze(This) -> wxWindow:freeze(This).
-doc false.
fitInside(This) -> wxWindow:fitInside(This).
-doc false.
fit(This) -> wxWindow:fit(This).
-doc false.
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
-doc false.
enable(This, Options) -> wxWindow:enable(This, Options).
-doc false.
enable(This) -> wxWindow:enable(This).
-doc false.
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
-doc false.
disable(This) -> wxWindow:disable(This).
-doc false.
destroyChildren(This) -> wxWindow:destroyChildren(This).
-doc false.
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
-doc false.
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
-doc false.
close(This, Options) -> wxWindow:close(This, Options).
-doc false.
close(This) -> wxWindow:close(This).
-doc false.
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
-doc false.
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
-doc false.
clearBackground(This) -> wxWindow:clearBackground(This).
-doc false.
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
-doc false.
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
-doc false.
centreOnParent(This) -> wxWindow:centreOnParent(This).
-doc false.
centerOnParent(This) -> wxWindow:centerOnParent(This).
-doc false.
centre(This, Options) -> wxWindow:centre(This, Options).
-doc false.
center(This, Options) -> wxWindow:center(This, Options).
-doc false.
centre(This) -> wxWindow:centre(This).
-doc false.
center(This) -> wxWindow:center(This).
-doc false.
captureMouse(This) -> wxWindow:captureMouse(This).
-doc false.
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
