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

-module(wxFrame).
-moduledoc """
A frame is a window whose size and position can (usually) be changed by the user.

It usually has thick borders and a title bar, and can optionally contain a menu bar,
toolbar and status bar. A frame can contain any window that is not a frame or dialog.

A frame that has a status bar and toolbar, created via the `createStatusBar/2` and `createToolBar/2` functions, manages these
windows and adjusts the value returned by `wxWindow:getClientSize/1` to reflect the remaining size available to
application windows.

Remark: An application should normally define an `m:wxCloseEvent` handler for the frame
to respond to system close events, for example so that related data and subwindows can be
cleaned up.

Default event processing

`m:wxFrame` processes the following events:

* `wxEVT_SIZE:` if the frame has exactly one child window, not counting the status and
toolbar, this child is resized to take the entire frame client area. If two or more
windows are present, they should be laid out explicitly either by manually handling `wxEVT_SIZE`
or using sizers;

* `wxEVT_MENU_HIGHLIGHT:` the default implementation displays the help string associated
with the selected item in the first pane of the status bar, if there is one.

## Styles

This class supports the following styles:

* wxDEFAULT_FRAME_STYLE: Defined as wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER |
wxSYSTEM_MENU | wxCAPTION | wxCLOSE_BOX | wxCLIP_CHILDREN.

* wxICONIZE: Display the frame iconized (minimized). Windows only.

* wxCAPTION: Puts a caption on the frame. Notice that this flag is required by
wxMINIMIZE_BOX, wxMAXIMIZE_BOX and wxCLOSE_BOX on most systems as the corresponding
buttons cannot be shown if the window has no title bar at all. I.e. if wxCAPTION is not
specified those styles would be simply ignored.

* wxMINIMIZE: Identical to wxICONIZE. Windows only.

* wxMINIMIZE_BOX: Displays a minimize box on the frame.

* wxMAXIMIZE: Displays the frame maximized. Windows and GTK+ only.

* wxMAXIMIZE_BOX: Displays a maximize box on the frame. Notice that under wxGTK
wxRESIZE_BORDER must be used as well or this style is ignored.

* wxCLOSE_BOX: Displays a close box on the frame.

* wxSTAY_ON_TOP: Stay on top of all other windows, see also wxFRAME_FLOAT_ON_PARENT.

* wxSYSTEM_MENU: Displays a system menu containing the list of various windows commands in
the window title bar. Unlike wxMINIMIZE_BOX, wxMAXIMIZE_BOX and wxCLOSE_BOX styles this
style can be used without wxCAPTION, at least under Windows, and makes the system menu
available without showing it on screen in this case. However it is recommended to only use
it together with wxCAPTION for consistent behaviour under all platforms.

* wxRESIZE_BORDER: Displays a resizable border around the window.

* wxFRAME_TOOL_WINDOW: Causes a frame with a small title bar to be created; the frame does
not appear in the taskbar under Windows or GTK+.

* wxFRAME_NO_TASKBAR: Creates an otherwise normal frame but it does not appear in the
taskbar under Windows or GTK+ (note that it will minimize to the desktop window under
Windows which may seem strange to the users and thus it might be better to use this style
only without wxMINIMIZE_BOX style). In wxGTK, the flag is respected only if the window
manager supports _NET_WM_STATE_SKIP_TASKBAR hint.

* wxFRAME_FLOAT_ON_PARENT: The frame will always be on top of its parent (unlike
wxSTAY_ON_TOP). A frame created with this style must have a non-NULL parent.

* wxFRAME_SHAPED: Windows with this style are allowed to have their shape changed with the `wxTopLevelWindow:setShape/2`
method. The default frame style is for normal, resizable frames. To create a frame which
cannot be resized by user, you may use the following combination of styles:

See also the overview_windowstyles.

## Extra Styles

This class supports the following extra styles:

* wxFRAME_EX_CONTEXTHELP: Under Windows, puts a query button on the caption. When pressed,
Windows will go into a context-sensitive help mode and wxWidgets will send a `wxEVT_HELP`
event if the user clicked on an application window. Note that this is an extended style
and must be set by calling SetExtraStyle before Create is called (two-step construction).
You cannot use this style together with wxMAXIMIZE_BOX or wxMINIMIZE_BOX, so you should
use wxDEFAULT_FRAME_STYLE ~ (wxMINIMIZE_BOX | wxMAXIMIZE_BOX) for the frames having this
style (the dialogs don't have a minimize or a maximize box by default)

* wxFRAME_EX_METAL: On macOS, frames with this style will be shown with a metallic look.
This is an extra style.

See:
* `m:wxMDIParentFrame`

* `m:wxMDIChildFrame`

* `m:wxMiniFrame`

* `m:wxDialog`

This class is derived, and can use functions, from:

* `m:wxTopLevelWindow`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxFrame](https://docs.wxwidgets.org/3.2/classwx_frame.html)

## Events

Event types emitted from this class:

* [`close_window`](`m:wxCloseEvent`)

* [`iconize`](`m:wxIconizeEvent`)

* [`menu_open`](`m:wxMenuEvent`)

* [`menu_close`](`m:wxMenuEvent`)

* [`menu_highlight`](`m:wxMenuEvent`)
""".
-include("wxe.hrl").
-export([create/4,create/5,createStatusBar/1,createStatusBar/2,createToolBar/1,
  createToolBar/2,destroy/1,getClientAreaOrigin/1,getMenuBar/1,getStatusBar/1,
  getStatusBarPane/1,getToolBar/1,new/0,new/3,new/4,processCommand/2,
  sendSizeEvent/1,sendSizeEvent/2,setMenuBar/2,setStatusBar/2,setStatusBarPane/2,
  setStatusText/2,setStatusText/3,setStatusWidths/2,setToolBar/2]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centerOnScreen/1,centerOnScreen/2,
  centre/1,centre/2,centreOnParent/1,centreOnParent/2,centreOnScreen/1,
  centreOnScreen/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getIcon/1,getIcons/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getTitle/1,getToolTip/1,
  getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,iconize/1,
  iconize/2,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isActive/1,
  isDoubleBuffered/1,isEnabled/1,isExposed/2,isExposed/3,isExposed/5,
  isFrozen/1,isFullScreen/1,isIconized/1,isMaximized/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,maximize/1,maximize/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,requestUserAttention/1,
  requestUserAttention/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setIcon/2,setIcons/2,
  setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setShape/2,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,
  setTitle/2,setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,showFullScreen/2,showFullScreen/3,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxFrame() :: wx:wx_object().
-export_type([wxFrame/0]).
-doc false.
parent_class(wxTopLevelWindow) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxFrame().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFrame_new_0),
  wxe_util:rec(?wxFrame_new_0).

-doc(#{equiv => new(Parent,Id,Title, [])}).
-spec new(Parent, Id, Title) -> wxFrame() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

new(Parent,Id,Title)
 when is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  new(Parent,Id,Title, []).

-doc """
Constructor, creating the window.

Remark: For Motif, MWM (the Motif Window Manager) should be running for any window styles
to work (otherwise all styles take effect).

See: `create/5`
""".
-spec new(Parent, Id, Title, [Option]) -> wxFrame() when
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
  wxe_util:queue_cmd(Parent,Id,Title_UC, Opts,?get_env(),?wxFrame_new_4),
  wxe_util:rec(?wxFrame_new_4).

-doc(#{equiv => create(This,Parent,Id,Title, [])}).
-spec create(This, Parent, Id, Title) -> boolean() when
	This::wxFrame(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

create(This,Parent,Id,Title)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  create(This,Parent,Id,Title, []).

-doc """
Used in two-step frame construction.

See `new/4` for further details.
""".
-spec create(This, Parent, Id, Title, [Option]) -> boolean() when
	This::wxFrame(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,Title, Options)
 when is_integer(Id),?is_chardata(Title),is_list(Options) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(ParentT,wxWindow),
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Title_UC, Opts,?get_env(),?wxFrame_Create),
  wxe_util:rec(?wxFrame_Create).

-doc(#{equiv => createStatusBar(This, [])}).
-spec createStatusBar(This) -> wxStatusBar:wxStatusBar() when
	This::wxFrame().

createStatusBar(This)
 when is_record(This, wx_ref) ->
  createStatusBar(This, []).

-doc """
Creates a status bar at the bottom of the frame.

Return: A pointer to the status bar if it was created successfully, NULL otherwise.

Remark: The width of the status bar is the whole width of the frame (adjusted
automatically when resizing), and the height and text size are chosen by the host
windowing system.

See:
* `setStatusText/3`

* `getStatusBar/1`
""".
-spec createStatusBar(This, [Option]) -> wxStatusBar:wxStatusBar() when
	This::wxFrame(),
	Option :: {'number', integer()}
		 | {'style', integer()}
		 | {'id', integer()}.
createStatusBar(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxFrame),
  MOpts = fun({number, _number} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({id, _id} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxFrame_CreateStatusBar),
  wxe_util:rec(?wxFrame_CreateStatusBar).

-doc(#{equiv => createToolBar(This, [])}).
-spec createToolBar(This) -> wxToolBar:wxToolBar() when
	This::wxFrame().

createToolBar(This)
 when is_record(This, wx_ref) ->
  createToolBar(This, []).

-doc """
Creates a toolbar at the top or left of the frame.

Return: A pointer to the toolbar if it was created successfully, NULL otherwise.

Remark: By default, the toolbar is an instance of `m:wxToolBar`. To use a different
class, override `OnCreateToolBar()` (not implemented in wx). When a toolbar has been
created with this function, or made known to the frame with `setToolBar/2`, the frame will manage the
toolbar position and adjust the return value from `wxWindow:getClientSize/1` to reflect the available space for
application windows. Under Pocket PC, you should always use this function for creating the
toolbar to be managed by the frame, so that wxWidgets can use a combined menubar and
toolbar. Where you manage your own toolbars, create a `m:wxToolBar` as usual.

See:
* `createStatusBar/2`

* `setToolBar/2`

* `getToolBar/1`
""".
-spec createToolBar(This, [Option]) -> wxToolBar:wxToolBar() when
	This::wxFrame(),
	Option :: {'style', integer()}
		 | {'id', integer()}.
createToolBar(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxFrame),
  MOpts = fun({style, _style} = Arg) -> Arg;
          ({id, _id} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxFrame_CreateToolBar),
  wxe_util:rec(?wxFrame_CreateToolBar).

-doc """
Returns the origin of the frame client area (in client coordinates).

It may be different from (0, 0) if the frame has a toolbar.
""".
-spec getClientAreaOrigin(This) -> {X::integer(), Y::integer()} when
	This::wxFrame().
getClientAreaOrigin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetClientAreaOrigin),
  wxe_util:rec(?wxFrame_GetClientAreaOrigin).

-doc """
Returns a pointer to the menubar currently associated with the frame (if any).

See:
* `setMenuBar/2`

* `m:wxMenuBar`

* `m:wxMenu`
""".
-spec getMenuBar(This) -> wxMenuBar:wxMenuBar() when
	This::wxFrame().
getMenuBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetMenuBar),
  wxe_util:rec(?wxFrame_GetMenuBar).

-doc """
Returns a pointer to the status bar currently associated with the frame (if any).

See:
* `createStatusBar/2`

* `m:wxStatusBar`
""".
-spec getStatusBar(This) -> wxStatusBar:wxStatusBar() when
	This::wxFrame().
getStatusBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetStatusBar),
  wxe_util:rec(?wxFrame_GetStatusBar).

-doc """
Returns the status bar pane used to display menu and toolbar help.

See: `setStatusBarPane/2`
""".
-spec getStatusBarPane(This) -> integer() when
	This::wxFrame().
getStatusBarPane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetStatusBarPane),
  wxe_util:rec(?wxFrame_GetStatusBarPane).

-doc """
Returns a pointer to the toolbar currently associated with the frame (if any).

See:
* `createToolBar/2`

* `m:wxToolBar`

* `setToolBar/2`
""".
-spec getToolBar(This) -> wxToolBar:wxToolBar() when
	This::wxFrame().
getToolBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetToolBar),
  wxe_util:rec(?wxFrame_GetToolBar).

-doc "Simulate a menu command.".
-spec processCommand(This, Id) -> boolean() when
	This::wxFrame(), Id::integer().
processCommand(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxFrame_ProcessCommand),
  wxe_util:rec(?wxFrame_ProcessCommand).

-doc(#{equiv => sendSizeEvent(This, [])}).
-spec sendSizeEvent(This) -> 'ok' when
	This::wxFrame().

sendSizeEvent(This)
 when is_record(This, wx_ref) ->
  sendSizeEvent(This, []).

-doc """
This function sends a dummy `m:wxSizeEvent` to the window allowing it to re-layout its
children positions.

It is sometimes useful to call this function after adding or deleting a children after
the frame creation or if a child size changes. Note that if the frame is using either
sizers or constraints for the children layout, it is enough to call `wxWindow:layout/1` directly and this
function should not be used in this case.

If `flags` includes `wxSEND_EVENT_POST` value, this function posts the event, i.e.
schedules it for later processing, instead of dispatching it directly. You can also use `PostSizeEvent()`
(not implemented in wx) as a more readable equivalent of calling this function with this flag.
""".
-spec sendSizeEvent(This, [Option]) -> 'ok' when
	This::wxFrame(),
	Option :: {'flags', integer()}.
sendSizeEvent(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxFrame),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxFrame_SendSizeEvent).

-doc """
Tells the frame to show the given menu bar.

Remark: If the frame is destroyed, the menu bar and its menus will be destroyed also, so
do not delete the menu bar explicitly (except by resetting the frame's menu bar to another
frame or NULL). Under Windows, a size event is generated, so be sure to initialize data
members properly before calling `setMenuBar/2`. Note that on some platforms, it is not possible to call
this function twice for the same frame object.

See:
* `getMenuBar/1`

* `m:wxMenuBar`

* `m:wxMenu`
""".
-spec setMenuBar(This, MenuBar) -> 'ok' when
	This::wxFrame(), MenuBar::wxMenuBar:wxMenuBar().
setMenuBar(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuBarT}=MenuBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(MenuBarT,wxMenuBar),
  wxe_util:queue_cmd(This,MenuBar,?get_env(),?wxFrame_SetMenuBar).

-doc """
Associates a status bar with the frame.

If `statusBar` is NULL, then the status bar, if present, is detached from the frame, but `not`
deleted.

See:
* `createStatusBar/2`

* `m:wxStatusBar`

* `getStatusBar/1`
""".
-spec setStatusBar(This, StatusBar) -> 'ok' when
	This::wxFrame(), StatusBar::wxStatusBar:wxStatusBar().
setStatusBar(#wx_ref{type=ThisT}=This,#wx_ref{type=StatusBarT}=StatusBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(StatusBarT,wxStatusBar),
  wxe_util:queue_cmd(This,StatusBar,?get_env(),?wxFrame_SetStatusBar).

-doc """
Set the status bar pane used to display menu and toolbar help.

Using -1 disables help display.
""".
-spec setStatusBarPane(This, N) -> 'ok' when
	This::wxFrame(), N::integer().
setStatusBarPane(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,N,?get_env(),?wxFrame_SetStatusBarPane).

-doc(#{equiv => setStatusText(This,Text, [])}).
-spec setStatusText(This, Text) -> 'ok' when
	This::wxFrame(), Text::unicode:chardata().

setStatusText(This,Text)
 when is_record(This, wx_ref),?is_chardata(Text) ->
  setStatusText(This,Text, []).

-doc """
Sets the status bar text and updates the status bar display.

This is a simple wrapper for `wxStatusBar:setStatusText/3` which doesn't do anything if the frame has no status bar,
i.e. `getStatusBar/1` returns NULL.

Remark: Use an empty string to clear the status bar.

See:
* `createStatusBar/2`

* `m:wxStatusBar`
""".
-spec setStatusText(This, Text, [Option]) -> 'ok' when
	This::wxFrame(), Text::unicode:chardata(),
	Option :: {'number', integer()}.
setStatusText(#wx_ref{type=ThisT}=This,Text, Options)
 when ?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxFrame),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({number, _number} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text_UC, Opts,?get_env(),?wxFrame_SetStatusText).

-doc """
Sets the widths of the fields in the status bar.

Remark: The widths of the variable fields are calculated from the total width of all
fields, minus the sum of widths of the non-variable fields, divided by the number of
variable fields.
""".
-spec setStatusWidths(This, Widths_field) -> 'ok' when
	This::wxFrame(), Widths_field::[integer()].
setStatusWidths(#wx_ref{type=ThisT}=This,Widths_field)
 when is_list(Widths_field) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,Widths_field,?get_env(),?wxFrame_SetStatusWidths).

-doc "Associates a toolbar with the frame.".
-spec setToolBar(This, ToolBar) -> 'ok' when
	This::wxFrame(), ToolBar::wxToolBar:wxToolBar().
setToolBar(#wx_ref{type=ThisT}=This,#wx_ref{type=ToolBarT}=ToolBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(ToolBarT,wxToolBar),
  wxe_util:queue_cmd(This,ToolBar,?get_env(),?wxFrame_SetToolBar).

-doc "Destroys the object".
-spec destroy(This::wxFrame()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFrame),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
