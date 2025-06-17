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

-module(wxMenuBar).
-moduledoc """
A menu bar is a series of menus accessible from the top of a frame.

Remark: To respond to a menu selection, provide a handler for EVT_MENU, in the frame that
contains the menu bar.

If you have a toolbar which uses the same identifiers as your EVT_MENU entries, events
from the toolbar will also be processed by your EVT_MENU event handlers.

Tip: under Windows, if you discover that menu shortcuts (for example, Alt-F to show the
file menu) are not working, check any EVT_CHAR events you are handling in child windows.
If you are not calling event.Skip() for events that you don't process in these event
handlers, menu shortcuts may cease to work.

See:
* `m:wxMenu`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxMenuBar](https://docs.wxwidgets.org/3.2/classwx_menu_bar.html)
""".
-include("wxe.hrl").
-export([append/3,check/3,destroy/1,enable/3,enableTop/3,findItem/2,findMenu/2,
  findMenuItem/3,getAutoWindowMenu/0,getHelpString/2,getLabel/2,getLabelTop/2,
  getMenu/2,getMenuCount/1,getMenuLabel/2,getMenuLabelText/2,insert/4,
  isChecked/2,isEnabled/2,macGetCommonMenuBar/0,macSetCommonMenuBar/1,
  new/0,new/1,oSXGetAppleMenu/1,remove/2,replace/4,setAutoWindowMenu/1,
  setHelpString/3,setLabel/3,setLabelTop/3,setMenuLabel/3]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCaret/1,
  getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isExposed/2,
  isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,isShownOnScreen/1,
  isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,move/2,move/3,move/4,
  moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,navigate/2,
  pageDown/1,pageUp/1,parent_class/1,popupMenu/2,popupMenu/3,popupMenu/4,
  raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,
  removeChild/2,reparent/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxMenuBar() :: wx:wx_object().
-export_type([wxMenuBar/0]).
-doc false.
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Construct an empty menu bar.".
-spec new() -> wxMenuBar().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_new_0),
  wxe_util:rec(?wxMenuBar_new_0).

-doc "".
-spec new(Style) -> wxMenuBar() when
	Style::integer().
new(Style)
 when is_integer(Style) ->
  wxe_util:queue_cmd(Style,?get_env(),?wxMenuBar_new_1),
  wxe_util:rec(?wxMenuBar_new_1).

-doc """
Adds the item to the end of the menu bar.

Return: true on success, false if an error occurred.

See: `insert/4`
""".
-spec append(This, Menu, Title) -> boolean() when
	This::wxMenuBar(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
append(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Menu,Title_UC,?get_env(),?wxMenuBar_Append),
  wxe_util:rec(?wxMenuBar_Append).

-doc """
Checks or unchecks a menu item.

Remark: Only use this when the menu bar has been associated with a frame; otherwise, use
the `m:wxMenu` equivalent call.
""".
-spec check(This, Id, Check) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Id,Check)
 when is_integer(Id),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Check,?get_env(),?wxMenuBar_Check).

-doc """
Enables or disables (greys out) a menu item.

Remark: Only use this when the menu bar has been associated with a frame; otherwise, use
the `m:wxMenu` equivalent call.
""".
-spec enable(This, Id, Enable) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Id,Enable)
 when is_integer(Id),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Enable,?get_env(),?wxMenuBar_Enable).

-doc """
Enables or disables a whole menu.

Remark: Only use this when the menu bar has been associated with a frame.
""".
-spec enableTop(This, Pos, Enable) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Enable::boolean().
enableTop(#wx_ref{type=ThisT}=This,Pos,Enable)
 when is_integer(Pos),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,Enable,?get_env(),?wxMenuBar_EnableTop).

-doc """
Returns the index of the menu with the given `title` or `wxNOT\_FOUND` if no such menu
exists in this menubar.

The `title` parameter may specify either the menu title (with accelerator characters,
i.e. `"&File"`) or just the menu label (`"File"`) indifferently.
""".
-spec findMenu(This, Title) -> integer() when
	This::wxMenuBar(), Title::unicode:chardata().
findMenu(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenuBar_FindMenu),
  wxe_util:rec(?wxMenuBar_FindMenu).

-doc """
Finds the menu item id for a menu name/menu item string pair.

Return: The menu item identifier, or wxNOT_FOUND if none was found.

Remark: Any special menu codes are stripped out of source and target strings before
matching.
""".
-spec findMenuItem(This, MenuString, ItemString) -> integer() when
	This::wxMenuBar(), MenuString::unicode:chardata(), ItemString::unicode:chardata().
findMenuItem(#wx_ref{type=ThisT}=This,MenuString,ItemString)
 when ?is_chardata(MenuString),?is_chardata(ItemString) ->
  ?CLASS(ThisT,wxMenuBar),
  MenuString_UC = unicode:characters_to_binary(MenuString),
  ItemString_UC = unicode:characters_to_binary(ItemString),
  wxe_util:queue_cmd(This,MenuString_UC,ItemString_UC,?get_env(),?wxMenuBar_FindMenuItem),
  wxe_util:rec(?wxMenuBar_FindMenuItem).

-doc """
Finds the menu item object associated with the given menu item identifier.

Return: The found menu item object, or NULL if one was not found.
""".
-spec findItem(This, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenuBar(), Id::integer().
findItem(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_FindItem),
  wxe_util:rec(?wxMenuBar_FindItem).

-doc """
Gets the help string associated with the menu item identifier.

Return: The help string, or the empty string if there was no help string or the menu item
was not found.

See: `setHelpString/3`
""".
-spec getHelpString(This, Id) -> unicode:charlist() when
	This::wxMenuBar(), Id::integer().
getHelpString(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_GetHelpString),
  wxe_util:rec(?wxMenuBar_GetHelpString).

-doc """
Gets the label associated with a menu item.

Return: The menu item label, or the empty string if the item was not found.

Remark: Use only after the menubar has been associated with a frame.
""".
-spec getLabel(This, Id) -> unicode:charlist() when
	This::wxMenuBar(), Id::integer().
getLabel(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_GetLabel),
  wxe_util:rec(?wxMenuBar_GetLabel).

-doc "Equivalent to: `getMenuLabel/2`".
-spec getLabelTop(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().

getLabelTop(This,Pos)
 when is_record(This, wx_ref),is_integer(Pos) ->
  getMenuLabel(This,Pos).

-doc """
Returns the label of a top-level menu.

Note that the returned string includes the accelerator characters that have been
specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See:
* `getMenuLabelText/2`

* `setMenuLabel/3`
""".
-spec getMenuLabel(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabel(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabel),
  wxe_util:rec(?wxMenuBar_GetMenuLabel).

-doc """
Returns the label of a top-level menu.

Note that the returned string does not include any accelerator characters that may have
been specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See:
* `getMenuLabel/2`

* `setMenuLabel/3`
""".
-spec getMenuLabelText(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabelText(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabelText),
  wxe_util:rec(?wxMenuBar_GetMenuLabelText).

-doc "Returns the menu at `menuIndex` (zero-based).".
-spec getMenu(This, MenuIndex) -> wxMenu:wxMenu() when
	This::wxMenuBar(), MenuIndex::integer().
getMenu(#wx_ref{type=ThisT}=This,MenuIndex)
 when is_integer(MenuIndex) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,MenuIndex,?get_env(),?wxMenuBar_GetMenu),
  wxe_util:rec(?wxMenuBar_GetMenu).

-doc "Returns the number of menus in this menubar.".
-spec getMenuCount(This) -> integer() when
	This::wxMenuBar().
getMenuCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_GetMenuCount),
  wxe_util:rec(?wxMenuBar_GetMenuCount).

-doc """
Inserts the menu at the given position into the menu bar.

Inserting menu at position 0 will insert it in the very beginning of it, inserting at
position `getMenuCount/1` is the same as calling `append/3`.

Return: true on success, false if an error occurred.

See: `append/3`
""".
-spec insert(This, Pos, Menu, Title) -> boolean() when
	This::wxMenuBar(), Pos::integer(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
insert(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=MenuT}=Menu,Title)
 when is_integer(Pos),?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Pos,Menu,Title_UC,?get_env(),?wxMenuBar_Insert),
  wxe_util:rec(?wxMenuBar_Insert).

-doc """
Determines whether an item is checked.

Return: true if the item was found and is checked, false otherwise.
""".
-spec isChecked(This, Id) -> boolean() when
	This::wxMenuBar(), Id::integer().
isChecked(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_IsChecked),
  wxe_util:rec(?wxMenuBar_IsChecked).

-doc "".
-spec setAutoWindowMenu(Enable) -> 'ok' when
	Enable::boolean().
setAutoWindowMenu(Enable)
 when is_boolean(Enable) ->
  wxe_util:queue_cmd(Enable,?get_env(),?wxMenuBar_SetAutoWindowMenu).

-doc "".
-spec getAutoWindowMenu() -> boolean().
getAutoWindowMenu() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_GetAutoWindowMenu),
  wxe_util:rec(?wxMenuBar_GetAutoWindowMenu).

-doc """
Returns the Apple menu.

This is the leftmost menu with application's name as its title. You shouldn't remove any
items from it, but it is safe to insert extra menu items or submenus into it.

Only for:wxosx

Since: 3.0.1
""".
-spec oSXGetAppleMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuBar().
oSXGetAppleMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_OSXGetAppleMenu),
  wxe_util:rec(?wxMenuBar_OSXGetAppleMenu).

-doc """
Enables you to get the global menubar on Mac, that is, the menubar displayed when the app
is running without any frames open.

Return: The global menubar.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx
""".
-spec macGetCommonMenuBar() -> wxMenuBar().
macGetCommonMenuBar() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_MacGetCommonMenuBar),
  wxe_util:rec(?wxMenuBar_MacGetCommonMenuBar).

-doc """
Enables you to set the global menubar on Mac, that is, the menubar displayed when the app
is running without any frames open.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx
""".
-spec macSetCommonMenuBar(Menubar) -> 'ok' when
	Menubar::wxMenuBar().
macSetCommonMenuBar(#wx_ref{type=MenubarT}=Menubar) ->
  ?CLASS(MenubarT,wxMenuBar),
  wxe_util:queue_cmd(Menubar,?get_env(),?wxMenuBar_MacSetCommonMenuBar).

-doc """
Determines whether an item is enabled.

Return: true if the item was found and is enabled, false otherwise.
""".
-spec isEnabled(This, Id) -> boolean() when
	This::wxMenuBar(), Id::integer().
isEnabled(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_IsEnabled),
  wxe_util:rec(?wxMenuBar_IsEnabled).

-doc """
Removes the menu from the menu bar and returns the menu object - the caller is
responsible for deleting it.

This function may be used together with `insert/4` to change the menubar dynamically.

See: `replace/4`
""".
-spec remove(This, Pos) -> wxMenu:wxMenu() when
	This::wxMenuBar(), Pos::integer().
remove(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_Remove),
  wxe_util:rec(?wxMenuBar_Remove).

-doc """
Replaces the menu at the given position with another one.

Return: The menu which was previously at position pos. The caller is responsible for
deleting it.

See:
* `insert/4`

* `remove/2`
""".
-spec replace(This, Pos, Menu, Title) -> wxMenu:wxMenu() when
	This::wxMenuBar(), Pos::integer(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=MenuT}=Menu,Title)
 when is_integer(Pos),?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Pos,Menu,Title_UC,?get_env(),?wxMenuBar_Replace),
  wxe_util:rec(?wxMenuBar_Replace).

-doc """
Sets the help string associated with a menu item.

See: `getHelpString/2`
""".
-spec setHelpString(This, Id, HelpString) -> 'ok' when
	This::wxMenuBar(), Id::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT}=This,Id,HelpString)
 when is_integer(Id),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenuBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,Id,HelpString_UC,?get_env(),?wxMenuBar_SetHelpString).

-doc """
Sets the label of a menu item.

Remark: Use only after the menubar has been associated with a frame.

See: `getLabel/2`
""".
-spec setLabel(This, Id, Label) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Id,Label)
 when is_integer(Id),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuBar),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Id,Label_UC,?get_env(),?wxMenuBar_SetLabel).

-doc "Equivalent to: `setMenuLabel/3`".
-spec setLabelTop(This, Pos, Label) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Label::unicode:chardata().

setLabelTop(This,Pos,Label)
 when is_record(This, wx_ref),is_integer(Pos),?is_chardata(Label) ->
  setMenuLabel(This,Pos,Label).

-doc """
Sets the label of a top-level menu.

Remark: Use only after the menubar has been associated with a frame.
""".
-spec setMenuLabel(This, Pos, Label) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Label::unicode:chardata().
setMenuLabel(#wx_ref{type=ThisT}=This,Pos,Label)
 when is_integer(Pos),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuBar),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Pos,Label_UC,?get_env(),?wxMenuBar_SetMenuLabel).

-doc "Destroys the object".
-spec destroy(This::wxMenuBar()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuBar),
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
