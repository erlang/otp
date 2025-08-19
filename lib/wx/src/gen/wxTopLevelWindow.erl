%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxTopLevelWindow).
-moduledoc """
`m:wxTopLevelWindow` is a common base class for `m:wxDialog` and `m:wxFrame`.

It is an abstract base class meaning that you never work with objects of this class
directly, but all of its methods are also applicable for the two classes above.

Note that the instances of `m:wxTopLevelWindow` are managed by wxWidgets in the internal
top level window list.

See:
* `m:wxDialog`

* `m:wxFrame`

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTopLevelWindow](https://docs.wxwidgets.org/3.2/classwx_top_level_window.html)

## Events

Event types emitted from this class:

* [`maximize`](`m:wxMaximizeEvent`)

* [`move`](`m:wxMoveEvent`)

* [`show`](`m:wxShowEvent`)
""".
-include("wxe.hrl").
-export([centerOnScreen/1,centerOnScreen/2,centreOnScreen/1,centreOnScreen/2,
  getIcon/1,getIcons/1,getTitle/1,iconize/1,iconize/2,isActive/1,isFullScreen/1,
  isIconized/1,isMaximized/1,maximize/1,maximize/2,requestUserAttention/1,
  requestUserAttention/2,setIcon/2,setIcons/2,setShape/2,setTitle/2,
  showFullScreen/2,showFullScreen/3]).

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

-type wxTopLevelWindow() :: wx:wx_object().
-export_type([wxTopLevelWindow/0]).
-doc false.
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the standard icon of the window.

The icon will be invalid if it hadn't been previously set by `setIcon/2`.

See: `getIcons/1`
""".
-spec getIcon(This) -> wxIcon:wxIcon() when
	This::wxTopLevelWindow().
getIcon(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_GetIcon),
  wxe_util:rec(?wxTopLevelWindow_GetIcon).

-doc """
Returns all icons associated with the window, there will be none of them if neither `setIcon/2`
nor `setIcons/2` had been called before.

Use `getIcon/1` to get the main icon of the window.

See: `m:wxIconBundle`
""".
-spec getIcons(This) -> wxIconBundle:wxIconBundle() when
	This::wxTopLevelWindow().
getIcons(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_GetIcons),
  wxe_util:rec(?wxTopLevelWindow_GetIcons).

-doc """
Gets a string containing the window title.

See: `setTitle/2`
""".
-spec getTitle(This) -> unicode:charlist() when
	This::wxTopLevelWindow().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_GetTitle),
  wxe_util:rec(?wxTopLevelWindow_GetTitle).

-doc """
Returns true if this window is currently active, i.e. if the user is currently working
with it.
""".
-spec isActive(This) -> boolean() when
	This::wxTopLevelWindow().
isActive(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_IsActive),
  wxe_util:rec(?wxTopLevelWindow_IsActive).

-doc(#{equiv => iconize(This, [])}).
-spec iconize(This) -> 'ok' when
	This::wxTopLevelWindow().

iconize(This)
 when is_record(This, wx_ref) ->
  iconize(This, []).

-doc """
Iconizes or restores the window.

Note that in wxGTK the change to the window state is not immediate, i.e. `isIconized/1` will typically
return false right after a call to `iconize/2` and its return value will only change after the
control flow returns to the event loop and the notification about the window being really
iconized is received.

See:
* `isIconized/1`

* `m:wxIconizeEvent`
""".
-spec iconize(This, [Option]) -> 'ok' when
	This::wxTopLevelWindow(),
	Option :: {'iconize', boolean()}.
iconize(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  MOpts = fun({iconize, _iconize} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxTopLevelWindow_Iconize).

-doc """
Returns true if the window is in fullscreen mode.

See: `showFullScreen/3`
""".
-spec isFullScreen(This) -> boolean() when
	This::wxTopLevelWindow().
isFullScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_IsFullScreen),
  wxe_util:rec(?wxTopLevelWindow_IsFullScreen).

-doc "Returns true if the window is iconized.".
-spec isIconized(This) -> boolean() when
	This::wxTopLevelWindow().
isIconized(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_IsIconized),
  wxe_util:rec(?wxTopLevelWindow_IsIconized).

-doc "Returns true if the window is maximized.".
-spec isMaximized(This) -> boolean() when
	This::wxTopLevelWindow().
isMaximized(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxTopLevelWindow_IsMaximized),
  wxe_util:rec(?wxTopLevelWindow_IsMaximized).

-doc(#{equiv => maximize(This, [])}).
-spec maximize(This) -> 'ok' when
	This::wxTopLevelWindow().

maximize(This)
 when is_record(This, wx_ref) ->
  maximize(This, []).

-doc """
Maximizes or restores the window.

Note that, just as with `iconize/2`, the change to the window state is not immediate in at least
wxGTK port.

See: `iconize/2`
""".
-spec maximize(This, [Option]) -> 'ok' when
	This::wxTopLevelWindow(),
	Option :: {'maximize', boolean()}.
maximize(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  MOpts = fun({maximize, _maximize} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxTopLevelWindow_Maximize).

-doc(#{equiv => requestUserAttention(This, [])}).
-spec requestUserAttention(This) -> 'ok' when
	This::wxTopLevelWindow().

requestUserAttention(This)
 when is_record(This, wx_ref) ->
  requestUserAttention(This, []).

-doc """
Use a system-dependent way to attract users attention to the window when it is in
background.

`flags` may have the value of either `?wxUSER\_ATTENTION\_INFO` (default) or `?wxUSER\_ATTENTION\_ERROR`
which results in a more drastic action. When in doubt, use the default value.

Note: This function should normally be only used when the application is not already in foreground.

This function is currently implemented for Win32 where it flashes the window icon in the
taskbar, and for wxGTK with task bars supporting it.
""".
-spec requestUserAttention(This, [Option]) -> 'ok' when
	This::wxTopLevelWindow(),
	Option :: {'flags', integer()}.
requestUserAttention(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxTopLevelWindow_RequestUserAttention).

-doc """
Sets the icon for this window.

Remark: The window takes a 'copy' of `icon`, but since it uses reference counting, the
copy is very quick. It is safe to delete `icon` after calling this function.

Note: In wxMSW, `icon` must be either 16x16 or 32x32 icon.

See:
* `m:wxIcon`

* `setIcons/2`
""".
-spec setIcon(This, Icon) -> 'ok' when
	This::wxTopLevelWindow(), Icon::wxIcon:wxIcon().
setIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxTopLevelWindow_SetIcon).

-doc """
Sets several icons of different sizes for this window: this allows using different icons
for different situations (e.g.

task switching bar, taskbar, window title bar) instead of scaling, with possibly bad
looking results, the only icon set by `setIcon/2`.

Note: In wxMSW, `icons` must contain a 16x16 or 32x32 icon, preferably both.

See: `m:wxIconBundle`
""".
-spec setIcons(This, Icons) -> 'ok' when
	This::wxTopLevelWindow(), Icons::wxIconBundle:wxIconBundle().
setIcons(#wx_ref{type=ThisT}=This,#wx_ref{type=IconsT}=Icons) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  ?CLASS(IconsT,wxIconBundle),
  wxe_util:queue_cmd(This,Icons,?get_env(),?wxTopLevelWindow_SetIcons).

-doc(#{equiv => centerOnScreen(This, [])}).
-spec centerOnScreen(This) -> 'ok' when
	This::wxTopLevelWindow().

centerOnScreen(This)
 when is_record(This, wx_ref) ->
  centerOnScreen(This, []).

-doc(#{equiv => centreOnScreen(This, [])}).
-spec centreOnScreen(This) -> 'ok' when
	This::wxTopLevelWindow().

centreOnScreen(This)
 when is_record(This, wx_ref) ->
  centreOnScreen(This, []).

-doc "Equivalent to: `centreOnScreen/2`".
-spec centerOnScreen(This, [Option]) -> 'ok' when
	This::wxTopLevelWindow(),
	Option :: {'dir', integer()}.

centerOnScreen(This, Options)
 when is_record(This, wx_ref),is_list(Options) ->
  centreOnScreen(This, Options).

-doc """
Centres the window on screen.

See: `wxWindow:centreOnParent/2`
""".
-spec centreOnScreen(This, [Option]) -> 'ok' when
	This::wxTopLevelWindow(),
	Option :: {'dir', integer()}.
centreOnScreen(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  MOpts = fun({dir, _dir} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxTopLevelWindow_CentreOnScreen).

-doc """
If the platform supports it, sets the shape of the window to that depicted by `region`.

The system will not display or respond to any mouse event for the pixels that lie outside
of the region. To reset the window to the normal rectangular shape simply call `setShape/2` again with
an empty `m:wxRegion`. Returns true if the operation is successful.

This method is available in this class only since wxWidgets 2.9.3, previous versions only
provided it in `m:wxTopLevelWindow`.

Note that windows with non default shape have a fixed size and can't be resized using `wxWindow:setSize/6`.
""".
-spec setShape(This, Region) -> boolean() when
	This::wxTopLevelWindow(), Region::wxRegion:wxRegion() | wxGraphicsPath:wxGraphicsPath().
setShape(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  IswxRegion = ?CLASS_T(RegionT,wxRegion),
  IswxGraphicsPath = ?CLASS_T(RegionT,wxGraphicsPath),
  RegionType = if
    IswxRegion ->   wxRegion;
    IswxGraphicsPath ->   wxGraphicsPath;
    true -> error({badarg, RegionT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Region, RegionType),?get_env(),?wxTopLevelWindow_SetShape),
  wxe_util:rec(?wxTopLevelWindow_SetShape).

-doc """
Sets the window title.

See: `getTitle/1`
""".
-spec setTitle(This, Title) -> 'ok' when
	This::wxTopLevelWindow(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxTopLevelWindow_SetTitle).

-doc(#{equiv => showFullScreen(This,Show, [])}).
-spec showFullScreen(This, Show) -> boolean() when
	This::wxTopLevelWindow(), Show::boolean().

showFullScreen(This,Show)
 when is_record(This, wx_ref),is_boolean(Show) ->
  showFullScreen(This,Show, []).

-doc """
Depending on the value of `show` parameter the window is either shown full screen or
restored to its normal state.

`style` is a bit list containing some or all of the following values, which indicate what
elements of the window to hide in full-screen mode:

* `?wxFULLSCREEN\_NOMENUBAR`

* `?wxFULLSCREEN\_NOTOOLBAR`

* `?wxFULLSCREEN\_NOSTATUSBAR`

* `?wxFULLSCREEN\_NOBORDER`

* `?wxFULLSCREEN\_NOCAPTION`

* `?wxFULLSCREEN\_ALL` (all of the above)

This function has not been tested with MDI frames.

Note: Showing a window full screen also actually `wxWindow:show/2`s the window if it isn't shown.

See: `isFullScreen/1`
""".
-spec showFullScreen(This, Show, [Option]) -> boolean() when
	This::wxTopLevelWindow(), Show::boolean(),
	Option :: {'style', integer()}.
showFullScreen(#wx_ref{type=ThisT}=This,Show, Options)
 when is_boolean(Show),is_list(Options) ->
  ?CLASS(ThisT,wxTopLevelWindow),
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Show, Opts,?get_env(),?wxTopLevelWindow_ShowFullScreen),
  wxe_util:rec(?wxTopLevelWindow_ShowFullScreen).

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
