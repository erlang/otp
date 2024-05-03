%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
Functions for wxFrame class

A frame is a window whose size and position can (usually) be changed by the
user.

It usually has thick borders and a title bar, and can optionally contain a menu
bar, toolbar and status bar. A frame can contain any window that is not a frame
or dialog.

A frame that has a status bar and toolbar, created via the `createStatusBar/2`
and `createToolBar/2` functions, manages these windows and adjusts the value
returned by `wxWindow:getClientSize/1` to reflect the remaining size available
to application windows.

Remark: An application should normally define an `m:wxCloseEvent` handler for
the frame to respond to system close events, for example so that related data
and subwindows can be cleaned up.

Default event processing

`m:wxFrame` processes the following events:

Styles

This class supports the following styles:

See also the overview_windowstyles.

Extra Styles

This class supports the following extra styles:

See: `m:wxMDIParentFrame`, `m:wxMDIChildFrame`, `m:wxMiniFrame`, `m:wxDialog`

This class is derived (and can use functions) from: `m:wxTopLevelWindow`
`m:wxWindow` `m:wxEvtHandler`

wxWidgets docs: [wxFrame](https://docs.wxwidgets.org/3.1/classwx_frame.html)

## Events

Event types emitted from this class: [`close_window`](`m:wxCloseEvent`),
[`iconize`](`m:wxIconizeEvent`), [`menu_open`](`m:wxMenuEvent`),
[`menu_close`](`m:wxMenuEvent`), [`menu_highlight`](`m:wxMenuEvent`)
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
%% @hidden
-doc false.
parent_class(wxTopLevelWindow) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframewxframe">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxFrame().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFrame_new_0),
  wxe_util:rec(?wxFrame_new_0).

%% @equiv new(Parent,Id,Title, [])
-spec new(Parent, Id, Title) -> wxFrame() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

new(Parent,Id,Title)
 when is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  new(Parent,Id,Title, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframewxframe">external documentation</a>.
-doc """
Constructor, creating the window.

Remark: For Motif, MWM (the Motif Window Manager) should be running for any
window styles to work (otherwise all styles take effect).

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

%% @equiv create(This,Parent,Id,Title, [])
-spec create(This, Parent, Id, Title) -> boolean() when
	This::wxFrame(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

create(This,Parent,Id,Title)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  create(This,Parent,Id,Title, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframecreate">external documentation</a>.
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

%% @equiv createStatusBar(This, [])
-spec createStatusBar(This) -> wxStatusBar:wxStatusBar() when
	This::wxFrame().

createStatusBar(This)
 when is_record(This, wx_ref) ->
  createStatusBar(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframecreatestatusbar">external documentation</a>.
-doc """
Creates a status bar at the bottom of the frame.

Return: A pointer to the status bar if it was created successfully, NULL
otherwise.

Remark: The width of the status bar is the whole width of the frame (adjusted
automatically when resizing), and the height and text size are chosen by the
host windowing system.

See: `setStatusText/3`, `OnCreateStatusBar()` (not implemented in wx),
`getStatusBar/1`
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

%% @equiv createToolBar(This, [])
-spec createToolBar(This) -> wxToolBar:wxToolBar() when
	This::wxFrame().

createToolBar(This)
 when is_record(This, wx_ref) ->
  createToolBar(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframecreatetoolbar">external documentation</a>.
-doc """
Creates a toolbar at the top or left of the frame.

Return: A pointer to the toolbar if it was created successfully, NULL otherwise.

Remark: By default, the toolbar is an instance of `m:wxToolBar`. To use a
different class, override `OnCreateToolBar()` (not implemented in wx). When a
toolbar has been created with this function, or made known to the frame with
`setToolBar/2`, the frame will manage the toolbar position and adjust the return
value from `wxWindow:getClientSize/1` to reflect the available space for
application windows. Under Pocket PC, you should always use this function for
creating the toolbar to be managed by the frame, so that wxWidgets can use a
combined menubar and toolbar. Where you manage your own toolbars, create a
`m:wxToolBar` as usual.

See: `createStatusBar/2`, `OnCreateToolBar()` (not implemented in wx),
`setToolBar/2`, `getToolBar/1`
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframegetclientareaorigin">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframegetmenubar">external documentation</a>.
-doc """
Returns a pointer to the menubar currently associated with the frame (if any).

See: `setMenuBar/2`, `m:wxMenuBar`, `m:wxMenu`
""".
-spec getMenuBar(This) -> wxMenuBar:wxMenuBar() when
	This::wxFrame().
getMenuBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetMenuBar),
  wxe_util:rec(?wxFrame_GetMenuBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframegetstatusbar">external documentation</a>.
-doc """
Returns a pointer to the status bar currently associated with the frame (if
any).

See: `createStatusBar/2`, `m:wxStatusBar`
""".
-spec getStatusBar(This) -> wxStatusBar:wxStatusBar() when
	This::wxFrame().
getStatusBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetStatusBar),
  wxe_util:rec(?wxFrame_GetStatusBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframegetstatusbarpane">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframegettoolbar">external documentation</a>.
-doc """
Returns a pointer to the toolbar currently associated with the frame (if any).

See: `createToolBar/2`, `m:wxToolBar`, `setToolBar/2`
""".
-spec getToolBar(This) -> wxToolBar:wxToolBar() when
	This::wxFrame().
getToolBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,?get_env(),?wxFrame_GetToolBar),
  wxe_util:rec(?wxFrame_GetToolBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframeprocesscommand">external documentation</a>.
-doc "Simulate a menu command.".
-spec processCommand(This, Id) -> boolean() when
	This::wxFrame(), Id::integer().
processCommand(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxFrame_ProcessCommand),
  wxe_util:rec(?wxFrame_ProcessCommand).

%% @equiv sendSizeEvent(This, [])
-spec sendSizeEvent(This) -> 'ok' when
	This::wxFrame().

sendSizeEvent(This)
 when is_record(This, wx_ref) ->
  sendSizeEvent(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesendsizeevent">external documentation</a>.
-doc """
This function sends a dummy `m:wxSizeEvent` to the window allowing it to
re-layout its children positions.

It is sometimes useful to call this function after adding or deleting a children
after the frame creation or if a child size changes. Note that if the frame is
using either sizers or constraints for the children layout, it is enough to call
`wxWindow:layout/1` directly and this function should not be used in this case.

If `flags` includes `wxSEND_EVENT_POST` value, this function posts the event,
i.e. schedules it for later processing, instead of dispatching it directly. You
can also use `PostSizeEvent()` (not implemented in wx) as a more readable
equivalent of calling this function with this flag.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesetmenubar">external documentation</a>.
-doc """
Tells the frame to show the given menu bar.

Remark: If the frame is destroyed, the menu bar and its menus will be destroyed
also, so do not delete the menu bar explicitly (except by resetting the frame's
menu bar to another frame or NULL). Under Windows, a size event is generated, so
be sure to initialize data members properly before calling `setMenuBar/2`. Note
that on some platforms, it is not possible to call this function twice for the
same frame object.

See: `getMenuBar/1`, `m:wxMenuBar`, `m:wxMenu`
""".
-spec setMenuBar(This, MenuBar) -> 'ok' when
	This::wxFrame(), MenuBar::wxMenuBar:wxMenuBar().
setMenuBar(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuBarT}=MenuBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(MenuBarT,wxMenuBar),
  wxe_util:queue_cmd(This,MenuBar,?get_env(),?wxFrame_SetMenuBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesetstatusbar">external documentation</a>.
-doc """
Associates a status bar with the frame.

If `statusBar` is NULL, then the status bar, if present, is detached from the
frame, but `not` deleted.

See: `createStatusBar/2`, `m:wxStatusBar`, `getStatusBar/1`
""".
-spec setStatusBar(This, StatusBar) -> 'ok' when
	This::wxFrame(), StatusBar::wxStatusBar:wxStatusBar().
setStatusBar(#wx_ref{type=ThisT}=This,#wx_ref{type=StatusBarT}=StatusBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(StatusBarT,wxStatusBar),
  wxe_util:queue_cmd(This,StatusBar,?get_env(),?wxFrame_SetStatusBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesetstatusbarpane">external documentation</a>.
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

%% @equiv setStatusText(This,Text, [])
-spec setStatusText(This, Text) -> 'ok' when
	This::wxFrame(), Text::unicode:chardata().

setStatusText(This,Text)
 when is_record(This, wx_ref),?is_chardata(Text) ->
  setStatusText(This,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesetstatustext">external documentation</a>.
-doc """
Sets the status bar text and updates the status bar display.

This is a simple wrapper for `wxStatusBar:setStatusText/3` which doesn't do
anything if the frame has no status bar, i.e. `getStatusBar/1` returns NULL.

Remark: Use an empty string to clear the status bar.

See: `createStatusBar/2`, `m:wxStatusBar`
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesetstatuswidths">external documentation</a>.
-doc """
Sets the widths of the fields in the status bar.

Remark: The widths of the variable fields are calculated from the total width of
all fields, minus the sum of widths of the non-variable fields, divided by the
number of variable fields.
""".
-spec setStatusWidths(This, Widths_field) -> 'ok' when
	This::wxFrame(), Widths_field::[integer()].
setStatusWidths(#wx_ref{type=ThisT}=This,Widths_field)
 when is_list(Widths_field) ->
  ?CLASS(ThisT,wxFrame),
  wxe_util:queue_cmd(This,Widths_field,?get_env(),?wxFrame_SetStatusWidths).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxframe.html#wxframesettoolbar">external documentation</a>.
-doc "Associates a toolbar with the frame.".
-spec setToolBar(This, ToolBar) -> 'ok' when
	This::wxFrame(), ToolBar::wxToolBar:wxToolBar().
setToolBar(#wx_ref{type=ThisT}=This,#wx_ref{type=ToolBarT}=ToolBar) ->
  ?CLASS(ThisT,wxFrame),
  ?CLASS(ToolBarT,wxToolBar),
  wxe_util:queue_cmd(This,ToolBar,?get_env(),?wxFrame_SetToolBar).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

Destroys all child windows and menu bar if present.

See overview_windowdeletion for more info.
""".
-spec destroy(This::wxFrame()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFrame),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxTopLevelWindow
%% @hidden
-doc false.
showFullScreen(This,Show, Options) -> wxTopLevelWindow:showFullScreen(This,Show, Options).
%% @hidden
-doc false.
showFullScreen(This,Show) -> wxTopLevelWindow:showFullScreen(This,Show).
%% @hidden
-doc false.
setTitle(This,Title) -> wxTopLevelWindow:setTitle(This,Title).
%% @hidden
-doc false.
setShape(This,Region) -> wxTopLevelWindow:setShape(This,Region).
%% @hidden
-doc false.
centreOnScreen(This, Options) -> wxTopLevelWindow:centreOnScreen(This, Options).
%% @hidden
-doc false.
centerOnScreen(This, Options) -> wxTopLevelWindow:centerOnScreen(This, Options).
%% @hidden
-doc false.
centreOnScreen(This) -> wxTopLevelWindow:centreOnScreen(This).
%% @hidden
-doc false.
centerOnScreen(This) -> wxTopLevelWindow:centerOnScreen(This).
%% @hidden
-doc false.
setIcons(This,Icons) -> wxTopLevelWindow:setIcons(This,Icons).
%% @hidden
-doc false.
setIcon(This,Icon) -> wxTopLevelWindow:setIcon(This,Icon).
%% @hidden
-doc false.
requestUserAttention(This, Options) -> wxTopLevelWindow:requestUserAttention(This, Options).
%% @hidden
-doc false.
requestUserAttention(This) -> wxTopLevelWindow:requestUserAttention(This).
%% @hidden
-doc false.
maximize(This, Options) -> wxTopLevelWindow:maximize(This, Options).
%% @hidden
-doc false.
maximize(This) -> wxTopLevelWindow:maximize(This).
%% @hidden
-doc false.
isMaximized(This) -> wxTopLevelWindow:isMaximized(This).
%% @hidden
-doc false.
isIconized(This) -> wxTopLevelWindow:isIconized(This).
%% @hidden
-doc false.
isFullScreen(This) -> wxTopLevelWindow:isFullScreen(This).
%% @hidden
-doc false.
iconize(This, Options) -> wxTopLevelWindow:iconize(This, Options).
%% @hidden
-doc false.
iconize(This) -> wxTopLevelWindow:iconize(This).
%% @hidden
-doc false.
isActive(This) -> wxTopLevelWindow:isActive(This).
%% @hidden
-doc false.
getTitle(This) -> wxTopLevelWindow:getTitle(This).
%% @hidden
-doc false.
getIcons(This) -> wxTopLevelWindow:getIcons(This).
%% @hidden
-doc false.
getIcon(This) -> wxTopLevelWindow:getIcon(This).
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
