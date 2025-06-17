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

-module(wxScrolledWindow).
-moduledoc """
The `wxScrolled` (not implemented in wx) class manages scrolling for its client area,
transforming the coordinates according to the scrollbar positions, and setting the scroll
positions, thumb sizes and ranges according to the area in view.

There are two commonly used (but not the only possible!) specializations of this class:

* ?wxScrolledWindow, aka wxScrolled<wxPanel>, is equivalent to ?wxScrolledWindow from
earlier versions. Derived from `m:wxPanel`, it shares `m:wxPanel`'s behaviour with regard
to TAB traversal and focus handling. Use this if the scrolled window will have child controls.

* ?wxScrolledCanvas, aka wxScrolled<wxWindow>, derives from `m:wxWindow` and so doesn't
handle children specially. This is suitable e.g. for implementing scrollable controls such
as tree or list controls.

Note: See `wxScrolled::Create()` (not implemented in wx) if you want to use `wxScrolled`
(not implemented in wx) with a custom class.

Starting from version 2.4 of wxWidgets, there are several ways to use a
?wxScrolledWindow (and now `wxScrolled` (not implemented in wx)). In particular, there are
three ways to set the size of the scrolling area:

One way is to set the scrollbars directly using a call to `setScrollbars/6`. This is the way it used to be
in any previous version of wxWidgets and it will be kept for backwards compatibility.

An additional method of manual control, which requires a little less computation of your
own, is to set the total size of the scrolling area by calling either `wxWindow:setVirtualSize/3`, or `wxWindow:fitInside/1`, and setting
the scrolling increments for it by calling `setScrollRate/3`. Scrolling in some orientation is enabled by
setting a non-zero increment for it.

The most automatic and newest way is to simply let sizers determine the scrolling area.
This is now the default when you set an interior sizer into a `wxScrolled` (not
implemented in wx) with `wxWindow:setSizer/3`. The scrolling area will be set to the size requested by the
sizer and the scrollbars will be assigned for each orientation according to the need for
them and the scrolling increment set by `setScrollRate/3`. As above, scrolling is only enabled in
orientations with a non-zero increment. You can influence the minimum size of the scrolled
area controlled by a sizer by calling wxWindow::SetVirtualSizeHints(). (Calling `setScrollbars/6` has
analogous effects in wxWidgets 2.4 - in later versions it may not continue to override the sizer.)

Note that if maximum size hints are still supported by wxWindow::SetVirtualSizeHints(),
use them at your own dire risk. They may or may not have been removed for 2.4, but it
really only makes sense to set minimum size hints here. We should probably replace
wxWindow::SetVirtualSizeHints() with wxWindow::SetMinVirtualSize() or similar and remove
it entirely in future.

As with all windows, an application can draw onto a `wxScrolled` (not implemented in wx)
using a device context.

You have the option of handling the OnPaint handler or overriding the `wxScrolled::OnDraw()`
(not implemented in wx) function, which is passed a pre-scrolled device context (prepared
by `doPrepareDC/2`).

If you don't wish to calculate your own scrolling, you must call `doPrepareDC/2` when not drawing from
within `OnDraw()` (not implemented in wx), to set the device origin for the device context
according to the current scroll position.

A `wxScrolled` (not implemented in wx) will normally scroll itself and therefore its
child windows as well. It might however be desired to scroll a different window than
itself: e.g. when designing a spreadsheet, you will normally only have to scroll the
(usually white) cell area, whereas the (usually grey) label area will scroll very
differently. For this special purpose, you can call `setTargetWindow/2` which means that pressing the
scrollbars will scroll a different window.

Note that the underlying system knows nothing about scrolling coordinates, so that all
system functions (mouse events, expose events, refresh calls etc) as well as the position
of subwindows are relative to the "physical" origin of the scrolled window. If the user
insert a child window at position (10,10) and scrolls the window down 100 pixels (moving
the child window out of the visible area), the child window will report a position of (10,-90).

## Styles

This class supports the following styles:

* wxHSCROLL: If this style is specified and ?wxVSCROLL isn't, the window will be scrollable
only in horizontal direction (by default, i.e. if neither this style nor ?wxVSCROLL is
specified, it scrolls in both directions).

* wxVSCROLL: If this style is specified and ?wxHSCROLL isn't, the window will be scrollable
only in vertical direction (by default, i.e. if neither this style nor ?wxHSCROLL is
specified, it scrolls in both directions).

* wxALWAYS_SHOW_SB: Since wxWidgets 2.9.5, specifying this style makes the window always
show its scrollbars, even if they are not used. See `ShowScrollbars()` (not implemented in
wx).

* wxRETAINED: Uses a backing pixmap to speed refreshes. Motif only.

See:
* `m:wxScrollBar`

* `m:wxClientDC`

* `m:wxPaintDC`

This class is derived, and can use functions, from:

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxScrolledWindow](https://docs.wxwidgets.org/3.2/classwx_scrolled_window.html)

## Events

Event types emitted from this class:

* [`scrollwin_top`](`m:wxScrollWinEvent`)

* [`scrollwin_bottom`](`m:wxScrollWinEvent`)

* [`scrollwin_lineup`](`m:wxScrollWinEvent`)

* [`scrollwin_linedown`](`m:wxScrollWinEvent`)

* [`scrollwin_pageup`](`m:wxScrollWinEvent`)

* [`scrollwin_pagedown`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbtrack`](`m:wxScrollWinEvent`)

* [`scrollwin_thumbrelease`](`m:wxScrollWinEvent`)
""".
-include("wxe.hrl").
-export([calcScrolledPosition/2,calcScrolledPosition/3,calcUnscrolledPosition/2,
  calcUnscrolledPosition/3,destroy/1,doPrepareDC/2,enableScrolling/3,
  getScrollPixelsPerUnit/1,getViewStart/1,new/0,new/1,new/2,prepareDC/2,
  scroll/2,scroll/3,setScrollRate/3,setScrollbars/5,setScrollbars/6,
  setTargetWindow/2]).

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
  setFocusIgnoringChildren/1,setFont/2,setForegroundColour/2,setHelpText/2,
  setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,setSize/3,setSize/5,
  setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,
  setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,setToolTip/2,
  setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxScrolledWindow() :: wx:wx_object().
-export_type([wxScrolledWindow/0]).
-doc false.
parent_class(wxPanel) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxScrolledWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxScrolledWindow_new_0),
  wxe_util:rec(?wxScrolledWindow_new_0).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxScrolledWindow() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor.

Remark: The window is initially created without visible scrollbars. Call `setScrollbars/6` to specify how
big the virtual window size should be.
""".
-spec new(Parent, [Option]) -> wxScrolledWindow() when
	Parent::wxWindow:wxWindow(),
	Option :: {'winid', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({winid, _winid} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxScrolledWindow_new_2),
  wxe_util:rec(?wxScrolledWindow_new_2).

-doc "".
-spec calcScrolledPosition(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxScrolledWindow(), Pt::{X::integer(), Y::integer()}.
calcScrolledPosition(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxScrolledWindow_CalcScrolledPosition_1),
  wxe_util:rec(?wxScrolledWindow_CalcScrolledPosition_1).

-doc """
Translates the logical coordinates to the device ones.

For example, if a window is scrolled 10 pixels to the bottom, the device coordinates of
the origin are (0, 0) (as always), but the logical coordinates are (0, 10) and so the call
to CalcScrolledPosition(0, 10, xx, yy) will return 0 in yy.

See: `calcUnscrolledPosition/3`
""".
-spec calcScrolledPosition(This, X, Y) -> {Xx::integer(), Yy::integer()} when
	This::wxScrolledWindow(), X::integer(), Y::integer().
calcScrolledPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxScrolledWindow_CalcScrolledPosition_4),
  wxe_util:rec(?wxScrolledWindow_CalcScrolledPosition_4).

-doc "".
-spec calcUnscrolledPosition(This, Pt) -> {X::integer(), Y::integer()} when
	This::wxScrolledWindow(), Pt::{X::integer(), Y::integer()}.
calcUnscrolledPosition(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxScrolledWindow_CalcUnscrolledPosition_1),
  wxe_util:rec(?wxScrolledWindow_CalcUnscrolledPosition_1).

-doc """
Translates the device coordinates to the logical ones.

For example, if a window is scrolled 10 pixels to the bottom, the device coordinates of
the origin are (0, 0) (as always), but the logical coordinates are (0, 10) and so the call
to CalcUnscrolledPosition(0, 0, xx, yy) will return 10 in yy.

See: `calcScrolledPosition/3`
""".
-spec calcUnscrolledPosition(This, X, Y) -> {Xx::integer(), Yy::integer()} when
	This::wxScrolledWindow(), X::integer(), Y::integer().
calcUnscrolledPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxScrolledWindow_CalcUnscrolledPosition_4),
  wxe_util:rec(?wxScrolledWindow_CalcUnscrolledPosition_4).

-doc """
Enable or disable use of `wxWindow:scrollWindow/4` for scrolling.

By default, when a scrolled window is logically scrolled, `wxWindow:scrollWindow/4` is called on the underlying
window which scrolls the window contents and only invalidates the part of the window newly
brought into view. If false is passed as an argument, then this "physical scrolling" is
disabled and the window is entirely invalidated whenever it is scrolled by calling `wxWindow:refresh/2`.

It should be rarely necessary to disable physical scrolling, so this method shouldn't be
called in normal circumstances.
""".
-spec enableScrolling(This, XScrolling, YScrolling) -> 'ok' when
	This::wxScrolledWindow(), XScrolling::boolean(), YScrolling::boolean().
enableScrolling(#wx_ref{type=ThisT}=This,XScrolling,YScrolling)
 when is_boolean(XScrolling),is_boolean(YScrolling) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,XScrolling,YScrolling,?get_env(),?wxScrolledWindow_EnableScrolling).

-doc """
Get the number of pixels per scroll unit (line), in each direction, as set by `setScrollbars/6`.

A value of zero indicates no scrolling in that direction.

See:
* `setScrollbars/6`

* `wxWindow:getVirtualSize/1`
""".
-spec getScrollPixelsPerUnit(This) -> {XUnit::integer(), YUnit::integer()} when
	This::wxScrolledWindow().
getScrollPixelsPerUnit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxScrolledWindow_GetScrollPixelsPerUnit),
  wxe_util:rec(?wxScrolledWindow_GetScrollPixelsPerUnit).

-doc "This is a simple overload of GetViewStart(int\*,int\*); see that function for more info.".
-spec getViewStart(This) -> {X::integer(), Y::integer()} when
	This::wxScrolledWindow().
getViewStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxScrolledWindow_GetViewStart),
  wxe_util:rec(?wxScrolledWindow_GetViewStart).

-doc """
Call this function to prepare the device context for drawing a scrolled image.

It sets the device origin according to the current scroll position. `doPrepareDC/2` is called
automatically within the default `wxEVT_PAINT` event handler, so your `OnDraw()` (not
implemented in wx) override will be passed an already 'pre-scrolled' device context.
However, if you wish to draw from outside of `OnDraw()` (not implemented in wx) (e.g. from
your own `wxEVT_PAINT` handler), you must call this function yourself.

For example:

Notice that the function sets the origin by moving it relatively to the current origin
position, so you shouldn't change the origin before calling `doPrepareDC/2` or, if you do, reset it to
(0, 0) later. If you call `doPrepareDC/2` immediately after device context creation, as in the example
above, this problem doesn't arise, of course, so it is customary to do it like this.
""".
-spec doPrepareDC(This, Dc) -> 'ok' when
	This::wxScrolledWindow(), Dc::wxDC:wxDC().
doPrepareDC(#wx_ref{type=ThisT}=This,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxScrolledWindow),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Dc,?get_env(),?wxScrolledWindow_DoPrepareDC).

-doc """
This function is for backwards compatibility only and simply calls `doPrepareDC/2` now.

Notice that it is not called by the default paint event handle (`doPrepareDC/2` is), so overriding this
method in your derived class is useless.
""".
-spec prepareDC(This, Dc) -> 'ok' when
	This::wxScrolledWindow(), Dc::wxDC:wxDC().
prepareDC(#wx_ref{type=ThisT}=This,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxScrolledWindow),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Dc,?get_env(),?wxScrolledWindow_PrepareDC).

-doc "This is an overload of `scroll/3`;see that function for more info.".
-spec scroll(This, Pt) -> 'ok' when
	This::wxScrolledWindow(), Pt::{X::integer(), Y::integer()}.
scroll(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxScrolledWindow_Scroll_1).

-doc """
Scrolls a window so the view start is at the given point.

Remark: The positions are in scroll units, not pixels, so to convert to pixels you will
have to multiply by the number of pixels per scroll increment. If either parameter is
?wxDefaultCoord (-1), that position will be ignored (no change in that direction).

See:
* `setScrollbars/6`

* `getScrollPixelsPerUnit/1`
""".
-spec scroll(This, X, Y) -> 'ok' when
	This::wxScrolledWindow(), X::integer(), Y::integer().
scroll(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxScrolledWindow_Scroll_2).

-doc(#{equiv => setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, [])}).
-spec setScrollbars(This, PixelsPerUnitX, PixelsPerUnitY, NoUnitsX, NoUnitsY) -> 'ok' when
	This::wxScrolledWindow(), PixelsPerUnitX::integer(), PixelsPerUnitY::integer(), NoUnitsX::integer(), NoUnitsY::integer().

setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY)
 when is_record(This, wx_ref),is_integer(PixelsPerUnitX),is_integer(PixelsPerUnitY),is_integer(NoUnitsX),is_integer(NoUnitsY) ->
  setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, []).

-doc """
Sets up vertical and/or horizontal scrollbars.

The first pair of parameters give the number of pixels per 'scroll step', i.e. amount
moved when the up or down scroll arrows are pressed. The second pair gives the length of
scrollbar in scroll steps, which sets the size of the virtual window.

`xPos` and `yPos` optionally specify a position to scroll to immediately.

For example, the following gives a window horizontal and vertical scrollbars with 20
pixels per scroll step, and a size of 50 steps (1000 pixels) in each direction:

`wxScrolled` (not implemented in wx) manages the page size itself, using the current
client window size as the page size.

Note that for more sophisticated scrolling applications, for example where scroll steps
may be variable according to the position in the document, it will be necessary to derive
a new class from `m:wxWindow`, overriding OnSize() and adjusting the scrollbars appropriately.

See: `wxWindow:setVirtualSize/3`
""".
-spec setScrollbars(This, PixelsPerUnitX, PixelsPerUnitY, NoUnitsX, NoUnitsY, [Option]) -> 'ok' when
	This::wxScrolledWindow(), PixelsPerUnitX::integer(), PixelsPerUnitY::integer(), NoUnitsX::integer(), NoUnitsY::integer(),
	Option :: {'xPos', integer()}
		 | {'yPos', integer()}
		 | {'noRefresh', boolean()}.
setScrollbars(#wx_ref{type=ThisT}=This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options)
 when is_integer(PixelsPerUnitX),is_integer(PixelsPerUnitY),is_integer(NoUnitsX),is_integer(NoUnitsY),is_list(Options) ->
  ?CLASS(ThisT,wxScrolledWindow),
  MOpts = fun({xPos, _xPos} = Arg) -> Arg;
          ({yPos, _yPos} = Arg) -> Arg;
          ({noRefresh, _noRefresh} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Opts,?get_env(),?wxScrolledWindow_SetScrollbars).

-doc """
Set the horizontal and vertical scrolling increment only.

See the pixelsPerUnit parameter in `setScrollbars/6`.
""".
-spec setScrollRate(This, Xstep, Ystep) -> 'ok' when
	This::wxScrolledWindow(), Xstep::integer(), Ystep::integer().
setScrollRate(#wx_ref{type=ThisT}=This,Xstep,Ystep)
 when is_integer(Xstep),is_integer(Ystep) ->
  ?CLASS(ThisT,wxScrolledWindow),
  wxe_util:queue_cmd(This,Xstep,Ystep,?get_env(),?wxScrolledWindow_SetScrollRate).

-doc """
Call this function to tell `wxScrolled` (not implemented in wx) to perform the actual
scrolling on a different window (and not on itself).

This method is useful when only a part of the window should be scrolled. A typical
example is a control consisting of a fixed header and the scrollable contents window: the
scrollbars are attached to the main window itself, hence it, and not the contents window
must be derived from `wxScrolled` (not implemented in wx), but only the contents window
scrolls when the scrollbars are used. To implement such setup, you need to call this
method with the contents window as argument.

Notice that if this method is used, `GetSizeAvailableForScrollTarget()` (not implemented
in wx) method must be overridden.
""".
-spec setTargetWindow(This, Window) -> 'ok' when
	This::wxScrolledWindow(), Window::wxWindow:wxWindow().
setTargetWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxScrolledWindow),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxScrolledWindow_SetTargetWindow).

-doc "Destroys the object".
-spec destroy(This::wxScrolledWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxScrolledWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxPanel
-doc false.
setFocusIgnoringChildren(This) -> wxPanel:setFocusIgnoringChildren(This).
-doc false.
initDialog(This) -> wxPanel:initDialog(This).
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
