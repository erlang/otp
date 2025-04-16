%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
-moduledoc """
This class manages up to two subwindows.

The current view can be split into two programmatically (perhaps from a menu command),
and unsplit either programmatically or via the `m:wxSplitterWindow` user interface.

## Styles

This class supports the following styles:

* wxSP_3D: Draws a 3D effect border and sash.

* wxSP_THIN_SASH: Draws a thin sash.

* wxSP_3DSASH: Draws a 3D effect sash (part of default style).

* wxSP_3DBORDER: Synonym for wxSP_BORDER.

* wxSP_BORDER: Draws a standard border.

* wxSP_NOBORDER: No border (default).

* wxSP_NO_XP_THEME: Under Windows, switches off the attempt to draw the splitter using
Windows theming, so the borders and sash will take on the pre-XP look.

* wxSP_PERMIT_UNSPLIT: Always allow to unsplit, even with the minimum pane size other than
zero.

* wxSP_LIVE_UPDATE: Don't draw XOR line but resize the child windows immediately.

See:
* `m:wxSplitterEvent`

* [Overview splitterwindow](https://docs.wxwidgets.org/3.2/overview_splitterwindow.html#overview_splitterwindow)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxSplitterWindow](https://docs.wxwidgets.org/3.2/classwx_splitter_window.html)

## Events

Event types emitted from this class:

* [`command_splitter_sash_pos_changing`](`m:wxSplitterEvent`)

* [`command_splitter_sash_pos_changed`](`m:wxSplitterEvent`)

* [`command_splitter_unsplit`](`m:wxSplitterEvent`)
""".
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
-doc false.
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxSplitterWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxSplitterWindow_new_0),
  wxe_util:rec(?wxSplitterWindow_new_0).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxSplitterWindow() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor for creating the window.

Remark: After using this constructor, you must create either one or two subwindows with
the splitter window as parent, and then call one of `initialize/2`, `splitVertically/4` and `splitHorizontally/4` in order to set the pane(s).
You can create two windows, with one hidden when not being shown; or you can create and
delete the second pane on demand.

See:
* `initialize/2`

* `splitVertically/4`

* `splitHorizontally/4`

* `create/3`
""".
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

-doc(#{equiv => create(This,Parent, [])}).
-spec create(This, Parent) -> boolean() when
	This::wxSplitterWindow(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

-doc """
Creation function, for two-step construction.

See `new/2` for details.
""".
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

-doc """
Returns the current minimum pane size (defaults to zero).

See: `setMinimumPaneSize/2`
""".
-spec getMinimumPaneSize(This) -> integer() when
	This::wxSplitterWindow().
getMinimumPaneSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetMinimumPaneSize),
  wxe_util:rec(?wxSplitterWindow_GetMinimumPaneSize).

-doc """
Returns the current sash gravity.

See: `setSashGravity/2`
""".
-spec getSashGravity(This) -> number() when
	This::wxSplitterWindow().
getSashGravity(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSashGravity),
  wxe_util:rec(?wxSplitterWindow_GetSashGravity).

-doc """
Returns the current sash position.

See: `setSashPosition/3`
""".
-spec getSashPosition(This) -> integer() when
	This::wxSplitterWindow().
getSashPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSashPosition),
  wxe_util:rec(?wxSplitterWindow_GetSashPosition).

-doc """
Gets the split mode.

See:
* `setSplitMode/2`

* `splitVertically/4`

* `splitHorizontally/4`
""".
%%  Res = ?wxSPLIT_HORIZONTAL | ?wxSPLIT_VERTICAL
-spec getSplitMode(This) -> wx:wx_enum() when
	This::wxSplitterWindow().
getSplitMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSplitMode),
  wxe_util:rec(?wxSplitterWindow_GetSplitMode).

-doc "Returns the left/top or only pane.".
-spec getWindow1(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow1(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow1),
  wxe_util:rec(?wxSplitterWindow_GetWindow1).

-doc "Returns the right/bottom pane.".
-spec getWindow2(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow2(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow2),
  wxe_util:rec(?wxSplitterWindow_GetWindow2).

-doc """
Initializes the splitter window to have one pane.

The child window is shown if it is currently hidden.

Remark: This should be called if you wish to initially view only a single pane in the
splitter window.

See:
* `splitVertically/4`

* `splitHorizontally/4`
""".
-spec initialize(This, Window) -> 'ok' when
	This::wxSplitterWindow(), Window::wxWindow:wxWindow().
initialize(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSplitterWindow_Initialize).

-doc "Returns true if the window is split, false otherwise.".
-spec isSplit(This) -> boolean() when
	This::wxSplitterWindow().
isSplit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_IsSplit),
  wxe_util:rec(?wxSplitterWindow_IsSplit).

-doc """
This function replaces one of the windows managed by the `m:wxSplitterWindow` with
another one.

It is in general better to use it instead of calling `unsplit/2` and then resplitting the window
back because it will provoke much less flicker (if any). It is valid to call this function
whether the splitter has two windows or only one.

Both parameters should be non-NULL and `winOld` must specify one of the windows managed
by the splitter. If the parameters are incorrect or the window couldn't be replaced, false
is returned. Otherwise the function will return true, but please notice that it will not
delete the replaced window and you may wish to do it yourself.

See: `getMinimumPaneSize/1`
""".
-spec replaceWindow(This, WinOld, WinNew) -> boolean() when
	This::wxSplitterWindow(), WinOld::wxWindow:wxWindow(), WinNew::wxWindow:wxWindow().
replaceWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=WinOldT}=WinOld,#wx_ref{type=WinNewT}=WinNew) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(WinOldT,wxWindow),
  ?CLASS(WinNewT,wxWindow),
  wxe_util:queue_cmd(This,WinOld,WinNew,?get_env(),?wxSplitterWindow_ReplaceWindow),
  wxe_util:rec(?wxSplitterWindow_ReplaceWindow).

-doc """
Sets the sash gravity.

Remark: Gravity is real factor which controls position of sash while resizing `m:wxSplitterWindow`.
Gravity tells `m:wxSplitterWindow` how much will left/top window grow while resizing.
Example values:

* 0.0: only the bottom/right window is automatically resized

* 0.5: both windows grow by equal size

* 1.0: only left/top window grows Gravity should be a real value between 0.0 and 1.0.
Default value of sash gravity is 0.0. That value is compatible with previous (before
gravity was introduced) behaviour of `m:wxSplitterWindow`.

Notice that when sash gravity for a newly created splitter window, it is often necessary
to explicitly set the splitter size using `wxWindow:setSize/6` to ensure that is big enough for its initial
sash position. Otherwise, i.e. if the window is created with the default tiny size and
only resized to its correct size later, the initial sash position will be affected by the
gravity and typically result in sash being at the rightmost position for the gravity of 1.
See the example code creating `m:wxSplitterWindow` in the splitter sample for more details.

See: `getSashGravity/1`
""".
-spec setSashGravity(This, Gravity) -> 'ok' when
	This::wxSplitterWindow(), Gravity::number().
setSashGravity(#wx_ref{type=ThisT}=This,Gravity)
 when is_number(Gravity) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,Gravity,?get_env(),?wxSplitterWindow_SetSashGravity).

-doc(#{equiv => setSashPosition(This,Position, [])}).
-spec setSashPosition(This, Position) -> 'ok' when
	This::wxSplitterWindow(), Position::integer().

setSashPosition(This,Position)
 when is_record(This, wx_ref),is_integer(Position) ->
  setSashPosition(This,Position, []).

-doc """
Sets the sash position.

Remark: Does not currently check for an out-of-range value.

See: `getSashPosition/1`
""".
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

-doc """
Sets the minimum pane size.

Remark: The default minimum pane size is zero, which means that either pane can be
reduced to zero by dragging the sash, thus removing one of the panes. To prevent this
behaviour (and veto out-of-range sash dragging), set a minimum size, for example 20
pixels. If the wxSP_PERMIT_UNSPLIT style is used when a splitter window is created, the
window may be unsplit even if minimum size is non-zero.

See: `getMinimumPaneSize/1`
""".
-spec setMinimumPaneSize(This, PaneSize) -> 'ok' when
	This::wxSplitterWindow(), PaneSize::integer().
setMinimumPaneSize(#wx_ref{type=ThisT}=This,PaneSize)
 when is_integer(PaneSize) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,PaneSize,?get_env(),?wxSplitterWindow_SetMinimumPaneSize).

-doc """
Sets the split mode.

Remark: Only sets the internal variable; does not update the display.

See:
* `getSplitMode/1`

* `splitVertically/4`

* `splitHorizontally/4`
""".
-spec setSplitMode(This, Mode) -> 'ok' when
	This::wxSplitterWindow(), Mode::integer().
setSplitMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxSplitterWindow_SetSplitMode).

-doc(#{equiv => splitHorizontally(This,Window1,Window2, [])}).
-spec splitHorizontally(This, Window1, Window2) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow().

splitHorizontally(This,Window1,Window2)
 when is_record(This, wx_ref),is_record(Window1, wx_ref),is_record(Window2, wx_ref) ->
  splitHorizontally(This,Window1,Window2, []).

-doc """
Initializes the top and bottom panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can also be
called at any subsequent time, but the application should check that the window is not
currently split using `isSplit/1`.

See:
* `splitVertically/4`

* `isSplit/1`

* `unsplit/2`
""".
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

-doc(#{equiv => splitVertically(This,Window1,Window2, [])}).
-spec splitVertically(This, Window1, Window2) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow().

splitVertically(This,Window1,Window2)
 when is_record(This, wx_ref),is_record(Window1, wx_ref),is_record(Window2, wx_ref) ->
  splitVertically(This,Window1,Window2, []).

-doc """
Initializes the left and right panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can also be
called at any subsequent time, but the application should check that the window is not
currently split using `isSplit/1`.

See:
* `splitHorizontally/4`

* `isSplit/1`

* `unsplit/2`
""".
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

-doc(#{equiv => unsplit(This, [])}).
-spec unsplit(This) -> boolean() when
	This::wxSplitterWindow().

unsplit(This)
 when is_record(This, wx_ref) ->
  unsplit(This, []).

-doc """
Unsplits the window.

Return: true if successful, false otherwise (the window was not split).

Remark: This call will not actually delete the pane being removed; it calls `OnUnsplit()`
(not implemented in wx) which can be overridden for the desired behaviour. By default, the
pane being removed is hidden.

See:
* `splitHorizontally/4`

* `splitVertically/4`

* `isSplit/1`
""".
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

-doc """
Causes any pending sizing of the sash and child panes to take place immediately.

Such resizing normally takes place in idle time, in order to wait for layout to be
completed. However, this can cause unacceptable flicker as the panes are resized after the
window has been shown. To work around this, you can perform window layout (for example by
sending a size event to the parent window), and then call this function, before showing
the top-level window.
""".
-spec updateSize(This) -> 'ok' when
	This::wxSplitterWindow().
updateSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_UpdateSize).

-doc "Destroys the object".
-spec destroy(This::wxSplitterWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSplitterWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
