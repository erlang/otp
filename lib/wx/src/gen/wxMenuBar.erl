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

-module(wxMenuBar).
-moduledoc """
Functions for wxMenuBar class

A menu bar is a series of menus accessible from the top of a frame.

Remark: To respond to a menu selection, provide a handler for EVT_MENU, in the
frame that contains the menu bar.

If you have a toolbar which uses the same identifiers as your EVT_MENU entries,
events from the toolbar will also be processed by your EVT_MENU event handlers.

Tip: under Windows, if you discover that menu shortcuts (for example, Alt-F to
show the file menu) are not working, check any EVT_CHAR events you are handling
in child windows. If you are not calling event.Skip() for events that you don't
process in these event handlers, menu shortcuts may cease to work.

See: `m:wxMenu`,
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

This class is derived (and can use functions) from: `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxMenuBar](https://docs.wxwidgets.org/3.1/classwx_menu_bar.html)
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
%% @hidden
-doc false.
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarwxmenubar">external documentation</a>.
-doc "Construct an empty menu bar.".
-spec new() -> wxMenuBar().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_new_0),
  wxe_util:rec(?wxMenuBar_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarwxmenubar">external documentation</a>.
-spec new(Style) -> wxMenuBar() when
	Style::integer().
new(Style)
 when is_integer(Style) ->
  wxe_util:queue_cmd(Style,?get_env(),?wxMenuBar_new_1),
  wxe_util:rec(?wxMenuBar_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarappend">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarcheck">external documentation</a>.
-doc """
Checks or unchecks a menu item.

Remark: Only use this when the menu bar has been associated with a frame;
otherwise, use the `m:wxMenu` equivalent call.
""".
-spec check(This, Id, Check) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Id,Check)
 when is_integer(Id),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Check,?get_env(),?wxMenuBar_Check).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarenable">external documentation</a>.
-doc """
Enables or disables (greys out) a menu item.

Remark: Only use this when the menu bar has been associated with a frame;
otherwise, use the `m:wxMenu` equivalent call.
""".
-spec enable(This, Id, Enable) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Id,Enable)
 when is_integer(Id),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Enable,?get_env(),?wxMenuBar_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarenabletop">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfindmenu">external documentation</a>.
-doc """
Returns the index of the menu with the given `title` or `wxNOT_FOUND` if no such
menu exists in this menubar.

The `title` parameter may specify either the menu title (with accelerator
characters, i.e. `"&File"`) or just the menu label (`"File"`) indifferently.
""".
-spec findMenu(This, Title) -> integer() when
	This::wxMenuBar(), Title::unicode:chardata().
findMenu(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenuBar_FindMenu),
  wxe_util:rec(?wxMenuBar_FindMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfindmenuitem">external documentation</a>.
-doc """
Finds the menu item id for a menu name/menu item string pair.

Return: The menu item identifier, or wxNOT_FOUND if none was found.

Remark: Any special menu codes are stripped out of source and target strings
before matching.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfinditem">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargethelpstring">external documentation</a>.
-doc """
Gets the help string associated with the menu item identifier.

Return: The help string, or the empty string if there was no help string or the
menu item was not found.

See: `setHelpString/3`
""".
-spec getHelpString(This, Id) -> unicode:charlist() when
	This::wxMenuBar(), Id::integer().
getHelpString(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_GetHelpString),
  wxe_util:rec(?wxMenuBar_GetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetlabel">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabel">external documentation</a>.
-doc "See: `getMenuLabel/2`.".
-spec getLabelTop(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().

getLabelTop(This,Pos)
 when is_record(This, wx_ref),is_integer(Pos) ->
  getMenuLabel(This,Pos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabel">external documentation</a>.
-doc """
Returns the label of a top-level menu.

Note that the returned string includes the accelerator characters that have been
specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See: `getMenuLabelText/2`, `setMenuLabel/3`
""".
-spec getMenuLabel(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabel(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabel),
  wxe_util:rec(?wxMenuBar_GetMenuLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabeltext">external documentation</a>.
-doc """
Returns the label of a top-level menu.

Note that the returned string does not include any accelerator characters that
may have been specified in the menu title string during its construction.

Return: The menu label, or the empty string if the menu was not found.

Remark: Use only after the menubar has been associated with a frame.

See: `getMenuLabel/2`, `setMenuLabel/3`
""".
-spec getMenuLabelText(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabelText(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabelText),
  wxe_util:rec(?wxMenuBar_GetMenuLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenu">external documentation</a>.
-doc "Returns the menu at `menuIndex` (zero-based).".
-spec getMenu(This, MenuIndex) -> wxMenu:wxMenu() when
	This::wxMenuBar(), MenuIndex::integer().
getMenu(#wx_ref{type=ThisT}=This,MenuIndex)
 when is_integer(MenuIndex) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,MenuIndex,?get_env(),?wxMenuBar_GetMenu),
  wxe_util:rec(?wxMenuBar_GetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenucount">external documentation</a>.
-doc "Returns the number of menus in this menubar.".
-spec getMenuCount(This) -> integer() when
	This::wxMenuBar().
getMenuCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_GetMenuCount),
  wxe_util:rec(?wxMenuBar_GetMenuCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarinsert">external documentation</a>.
-doc """
Inserts the menu at the given position into the menu bar.

Inserting menu at position 0 will insert it in the very beginning of it,
inserting at position `getMenuCount/1` is the same as calling `append/3`.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarischecked">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetautowindowmenu">external documentation</a>.
-spec setAutoWindowMenu(Enable) -> 'ok' when
	Enable::boolean().
setAutoWindowMenu(Enable)
 when is_boolean(Enable) ->
  wxe_util:queue_cmd(Enable,?get_env(),?wxMenuBar_SetAutoWindowMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetautowindowmenu">external documentation</a>.
-spec getAutoWindowMenu() -> boolean().
getAutoWindowMenu() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_GetAutoWindowMenu),
  wxe_util:rec(?wxMenuBar_GetAutoWindowMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarosxgetapplemenu">external documentation</a>.
-doc """
Returns the Apple menu.

This is the leftmost menu with application's name as its title. You shouldn't
remove any items from it, but it is safe to insert extra menu items or submenus
into it.

Only for:wxosx

Since: 3.0.1
""".
-spec oSXGetAppleMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuBar().
oSXGetAppleMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_OSXGetAppleMenu),
  wxe_util:rec(?wxMenuBar_OSXGetAppleMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarmacgetcommonmenubar">external documentation</a>.
-doc """
Enables you to get the global menubar on Mac, that is, the menubar displayed
when the app is running without any frames open.

Return: The global menubar.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx
""".
-spec macGetCommonMenuBar() -> wxMenuBar().
macGetCommonMenuBar() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_MacGetCommonMenuBar),
  wxe_util:rec(?wxMenuBar_MacGetCommonMenuBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarmacsetcommonmenubar">external documentation</a>.
-doc """
Enables you to set the global menubar on Mac, that is, the menubar displayed
when the app is running without any frames open.

Remark: Only exists on Mac, other platforms do not have this method.

Only for:wxosx
""".
-spec macSetCommonMenuBar(Menubar) -> 'ok' when
	Menubar::wxMenuBar().
macSetCommonMenuBar(#wx_ref{type=MenubarT}=Menubar) ->
  ?CLASS(MenubarT,wxMenuBar),
  wxe_util:queue_cmd(Menubar,?get_env(),?wxMenuBar_MacSetCommonMenuBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarisenabled">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarremove">external documentation</a>.
-doc """
Removes the menu from the menu bar and returns the menu object - the caller is
responsible for deleting it.

This function may be used together with `insert/4` to change the menubar
dynamically.

See: `replace/4`
""".
-spec remove(This, Pos) -> wxMenu:wxMenu() when
	This::wxMenuBar(), Pos::integer().
remove(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_Remove),
  wxe_util:rec(?wxMenuBar_Remove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarreplace">external documentation</a>.
-doc """
Replaces the menu at the given position with another one.

Return: The menu which was previously at position pos. The caller is responsible
for deleting it.

See: `insert/4`, `remove/2`
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsethelpstring">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetlabel">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetmenulabel">external documentation</a>.
-doc "See: `setMenuLabel/3`.".
-spec setLabelTop(This, Pos, Label) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Label::unicode:chardata().

setLabelTop(This,Pos,Label)
 when is_record(This, wx_ref),is_integer(Pos),?is_chardata(Label) ->
  setMenuLabel(This,Pos,Label).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetmenulabel">external documentation</a>.
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

%% @doc Destroys this object, do not use object again
-doc """
Destructor, destroying the menu bar and removing it from the parent frame (if
any).
""".
-spec destroy(This::wxMenuBar()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuBar),
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
