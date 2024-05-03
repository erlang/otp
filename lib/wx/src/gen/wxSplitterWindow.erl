%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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
Functions for wxSplitterWindow class

This class manages up to two subwindows. The current view can be split into two
programmatically (perhaps from a menu command), and unsplit either
programmatically or via the `m:wxSplitterWindow` user interface.

Styles

This class supports the following styles:

See: `m:wxSplitterEvent`,
[Overview splitterwindow](https://docs.wxwidgets.org/3.1/overview_splitterwindow.html#overview_splitterwindow)

This class is derived (and can use functions) from: `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxSplitterWindow](https://docs.wxwidgets.org/3.1/classwx_splitter_window.html)

## Events

Event types emitted from this class:
[`command_splitter_sash_pos_changing`](`m:wxSplitterEvent`),
[`command_splitter_sash_pos_changed`](`m:wxSplitterEvent`),
[`command_splitter_unsplit`](`m:wxSplitterEvent`)
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
%% @hidden
-doc false.
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowwxsplitterwindow">external documentation</a>.
-doc "Default constructor.".
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
-doc """
Constructor for creating the window.

Remark: After using this constructor, you must create either one or two
subwindows with the splitter window as parent, and then call one of
`initialize/2`, `splitVertically/4` and `splitHorizontally/4` in order to set
the pane(s). You can create two windows, with one hidden when not being shown;
or you can create and delete the second pane on demand.

See: `initialize/2`, `splitVertically/4`, `splitHorizontally/4`, `create/3`
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

%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
	This::wxSplitterWindow(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowcreate">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetminimumpanesize">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsashgravity">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsashposition">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetsplitmode">external documentation</a>.
%%<br /> Res = ?wxSPLIT_HORIZONTAL | ?wxSPLIT_VERTICAL
-doc """
Gets the split mode.

See: `setSplitMode/2`, `splitVertically/4`, `splitHorizontally/4`
""".
-spec getSplitMode(This) -> wx:wx_enum() when
	This::wxSplitterWindow().
getSplitMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetSplitMode),
  wxe_util:rec(?wxSplitterWindow_GetSplitMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetwindow1">external documentation</a>.
-doc "Returns the left/top or only pane.".
-spec getWindow1(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow1(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow1),
  wxe_util:rec(?wxSplitterWindow_GetWindow1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowgetwindow2">external documentation</a>.
-doc "Returns the right/bottom pane.".
-spec getWindow2(This) -> wxWindow:wxWindow() when
	This::wxSplitterWindow().
getWindow2(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_GetWindow2),
  wxe_util:rec(?wxSplitterWindow_GetWindow2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowinitialize">external documentation</a>.
-doc """
Initializes the splitter window to have one pane.

The child window is shown if it is currently hidden.

Remark: This should be called if you wish to initially view only a single pane
in the splitter window.

See: `splitVertically/4`, `splitHorizontally/4`
""".
-spec initialize(This, Window) -> 'ok' when
	This::wxSplitterWindow(), Window::wxWindow:wxWindow().
initialize(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSplitterWindow),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSplitterWindow_Initialize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowissplit">external documentation</a>.
-doc "Returns true if the window is split, false otherwise.".
-spec isSplit(This) -> boolean() when
	This::wxSplitterWindow().
isSplit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_IsSplit),
  wxe_util:rec(?wxSplitterWindow_IsSplit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowreplacewindow">external documentation</a>.
-doc """
This function replaces one of the windows managed by the `m:wxSplitterWindow`
with another one.

It is in general better to use it instead of calling `unsplit/2` and then
resplitting the window back because it will provoke much less flicker (if any).
It is valid to call this function whether the splitter has two windows or only
one.

Both parameters should be non-NULL and `winOld` must specify one of the windows
managed by the splitter. If the parameters are incorrect or the window couldn't
be replaced, false is returned. Otherwise the function will return true, but
please notice that it will not delete the replaced window and you may wish to do
it yourself.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetsashgravity">external documentation</a>.
-doc """
Sets the sash gravity.

Remark: Gravity is real factor which controls position of sash while resizing
`m:wxSplitterWindow`. Gravity tells `m:wxSplitterWindow` how much will left/top
window grow while resizing. Example values:

Notice that when sash gravity for a newly created splitter window, it is often
necessary to explicitly set the splitter size using `wxWindow:setSize/6` to
ensure that is big enough for its initial sash position. Otherwise, i.e. if the
window is created with the default tiny size and only resized to its correct
size later, the initial sash position will be affected by the gravity and
typically result in sash being at the rightmost position for the gravity of 1.
See the example code creating `m:wxSplitterWindow` in the splitter sample for
more details.

See: `getSashGravity/1`
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetminimumpanesize">external documentation</a>.
-doc """
Sets the minimum pane size.

Remark: The default minimum pane size is zero, which means that either pane can
be reduced to zero by dragging the sash, thus removing one of the panes. To
prevent this behaviour (and veto out-of-range sash dragging), set a minimum
size, for example 20 pixels. If the wxSP_PERMIT_UNSPLIT style is used when a
splitter window is created, the window may be unsplit even if minimum size is
non-zero.

See: `getMinimumPaneSize/1`
""".
-spec setMinimumPaneSize(This, PaneSize) -> 'ok' when
	This::wxSplitterWindow(), PaneSize::integer().
setMinimumPaneSize(#wx_ref{type=ThisT}=This,PaneSize)
 when is_integer(PaneSize) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,PaneSize,?get_env(),?wxSplitterWindow_SetMinimumPaneSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsetsplitmode">external documentation</a>.
-doc """
Sets the split mode.

Remark: Only sets the internal variable; does not update the display.

See: `getSplitMode/1`, `splitVertically/4`, `splitHorizontally/4`
""".
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
-doc """
Initializes the top and bottom panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can
also be called at any subsequent time, but the application should check that the
window is not currently split using `isSplit/1`.

See: `splitVertically/4`, `isSplit/1`, `unsplit/2`
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

%% @equiv splitVertically(This,Window1,Window2, [])
-spec splitVertically(This, Window1, Window2) -> boolean() when
	This::wxSplitterWindow(), Window1::wxWindow:wxWindow(), Window2::wxWindow:wxWindow().

splitVertically(This,Window1,Window2)
 when is_record(This, wx_ref),is_record(Window1, wx_ref),is_record(Window2, wx_ref) ->
  splitVertically(This,Window1,Window2, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowsplitvertically">external documentation</a>.
-doc """
Initializes the left and right panes of the splitter window.

The child windows are shown if they are currently hidden.

Return: true if successful, false otherwise (the window was already split).

Remark: This should be called if you wish to initially view two panes. It can
also be called at any subsequent time, but the application should check that the
window is not currently split using `isSplit/1`.

See: `splitHorizontally/4`, `isSplit/1`, `unsplit/2`
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

%% @equiv unsplit(This, [])
-spec unsplit(This) -> boolean() when
	This::wxSplitterWindow().

unsplit(This)
 when is_record(This, wx_ref) ->
  unsplit(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowunsplit">external documentation</a>.
-doc """
Unsplits the window.

Return: true if successful, false otherwise (the window was not split).

Remark: This call will not actually delete the pane being removed; it calls
`OnUnsplit()` (not implemented in wx) which can be overridden for the desired
behaviour. By default, the pane being removed is hidden.

See: `splitHorizontally/4`, `splitVertically/4`, `isSplit/1`, `OnUnsplit()` (not
implemented in wx)
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsplitterwindow.html#wxsplitterwindowupdatesize">external documentation</a>.
-doc """
Causes any pending sizing of the sash and child panes to take place immediately.

Such resizing normally takes place in idle time, in order to wait for layout to
be completed. However, this can cause unacceptable flicker as the panes are
resized after the window has been shown. To work around this, you can perform
window layout (for example by sending a size event to the parent window), and
then call this function, before showing the top-level window.
""".
-spec updateSize(This) -> 'ok' when
	This::wxSplitterWindow().
updateSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSplitterWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxSplitterWindow_UpdateSize).

%% @doc Destroys this object, do not use object again
-doc "Destroys the `m:wxSplitterWindow` and its children.".
-spec destroy(This::wxSplitterWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSplitterWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxWindow
%% @hidden
-doc false.
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
-doc false.
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
%% @hidden
-doc false.
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
-doc false.
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
-doc false.
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
-doc false.
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
%% @hidden
-doc false.
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
-doc false.
validate(This) -> wxWindow:validate(This).
%% @hidden
-doc false.
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
-doc false.
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
-doc false.
update(This) -> wxWindow:update(This).
%% @hidden
-doc false.
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
-doc false.
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
-doc false.
thaw(This) -> wxWindow:thaw(This).
%% @hidden
-doc false.
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
-doc false.
show(This) -> wxWindow:show(This).
%% @hidden
-doc false.
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
-doc false.
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
-doc false.
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
-doc false.
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
%% @hidden
-doc false.
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
-doc false.
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
-doc false.
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
-doc false.
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
%% @hidden
-doc false.
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
-doc false.
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
-doc false.
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
-doc false.
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
-doc false.
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
-doc false.
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
-doc false.
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
-doc false.
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
-doc false.
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
-doc false.
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
-doc false.
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
-doc false.
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
-doc false.
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
-doc false.
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
-doc false.
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
-doc false.
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
%% @hidden
-doc false.
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
-doc false.
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
%% @hidden
-doc false.
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
-doc false.
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
-doc false.
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
-doc false.
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
-doc false.
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
-doc false.
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
-doc false.
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
-doc false.
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
-doc false.
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
-doc false.
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
-doc false.
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
%% @hidden
-doc false.
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
-doc false.
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
-doc false.
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
-doc false.
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
-doc false.
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
-doc false.
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
-doc false.
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
-doc false.
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
-doc false.
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
-doc false.
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
-doc false.
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
-doc false.
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
-doc false.
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
-doc false.
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
-doc false.
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
-doc false.
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
-doc false.
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
-doc false.
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
-doc false.
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
-doc false.
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
-doc false.
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
-doc false.
refresh(This) -> wxWindow:refresh(This).
%% @hidden
-doc false.
raise(This) -> wxWindow:raise(This).
%% @hidden
-doc false.
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
-doc false.
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
-doc false.
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
-doc false.
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
-doc false.
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
-doc false.
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
-doc false.
navigate(This) -> wxWindow:navigate(This).
%% @hidden
-doc false.
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
-doc false.
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
-doc false.
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
-doc false.
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
-doc false.
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
-doc false.
lower(This) -> wxWindow:lower(This).
%% @hidden
-doc false.
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
-doc false.
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
-doc false.
layout(This) -> wxWindow:layout(This).
%% @hidden
-doc false.
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
%% @hidden
-doc false.
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
-doc false.
isShown(This) -> wxWindow:isShown(This).
%% @hidden
-doc false.
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
-doc false.
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
-doc false.
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
-doc false.
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
-doc false.
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
-doc false.
isFrozen(This) -> wxWindow:isFrozen(This).
%% @hidden
-doc false.
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
-doc false.
initDialog(This) -> wxWindow:initDialog(This).
%% @hidden
-doc false.
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
-doc false.
hide(This) -> wxWindow:hide(This).
%% @hidden
-doc false.
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
-doc false.
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
-doc false.
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
-doc false.
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
-doc false.
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
-doc false.
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
-doc false.
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
-doc false.
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
-doc false.
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
-doc false.
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
-doc false.
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
-doc false.
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
-doc false.
getSize(This) -> wxWindow:getSize(This).
%% @hidden
-doc false.
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
-doc false.
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
-doc false.
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
%% @hidden
-doc false.
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
-doc false.
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
-doc false.
getRect(This) -> wxWindow:getRect(This).
%% @hidden
-doc false.
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
-doc false.
getParent(This) -> wxWindow:getParent(This).
%% @hidden
-doc false.
getName(This) -> wxWindow:getName(This).
%% @hidden
-doc false.
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
-doc false.
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
-doc false.
getLabel(This) -> wxWindow:getLabel(This).
%% @hidden
-doc false.
getId(This) -> wxWindow:getId(This).
%% @hidden
-doc false.
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
-doc false.
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
-doc false.
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
-doc false.
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
-doc false.
getFont(This) -> wxWindow:getFont(This).
%% @hidden
-doc false.
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
-doc false.
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
%% @hidden
-doc false.
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
-doc false.
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
-doc false.
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
-doc false.
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
-doc false.
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
-doc false.
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
-doc false.
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
-doc false.
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
-doc false.
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
-doc false.
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
-doc false.
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
-doc false.
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
-doc false.
freeze(This) -> wxWindow:freeze(This).
%% @hidden
-doc false.
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
-doc false.
fit(This) -> wxWindow:fit(This).
%% @hidden
-doc false.
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
-doc false.
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
-doc false.
enable(This) -> wxWindow:enable(This).
%% @hidden
-doc false.
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
%% @hidden
-doc false.
disable(This) -> wxWindow:disable(This).
%% @hidden
-doc false.
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
-doc false.
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
-doc false.
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
-doc false.
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
-doc false.
close(This) -> wxWindow:close(This).
%% @hidden
-doc false.
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
-doc false.
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
-doc false.
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
-doc false.
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
-doc false.
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
-doc false.
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
-doc false.
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
-doc false.
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
-doc false.
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
-doc false.
centre(This) -> wxWindow:centre(This).
%% @hidden
-doc false.
center(This) -> wxWindow:center(This).
%% @hidden
-doc false.
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
-doc false.
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
%% @hidden
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
