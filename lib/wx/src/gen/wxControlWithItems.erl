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

-module(wxControlWithItems).
-moduledoc """
This is convenience class that derives from both `m:wxControl` and `wxItemContainer` (not
implemented in wx).

It is used as basis for some wxWidgets controls (`m:wxChoice` and `m:wxListBox`).

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxControlWithItems](https://docs.wxwidgets.org/3.2/classwx_control_with_items.html)
""".
-include("wxe.hrl").
-export([append/2,append/3,appendStrings/2,appendStrings/3,clear/1,delete/2,
  findString/2,findString/3,getClientData/2,getCount/1,getSelection/1,
  getString/2,getStringSelection/1,insert/3,insert/4,insertStrings/3,
  insertStrings/4,isEmpty/1,select/2,setClientData/3,setSelection/2,
  setString/3,setStringSelection/2]).

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

-type wxControlWithItems() :: wx:wx_object().
-export_type([wxControlWithItems/0]).
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this may be
different from the last one if the control is sorted (e.g. has `wxLB_SORT` or `wxCB_SORT`
style).
""".
-spec append(This, Item) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata().
append(#wx_ref{type=ThisT}=This,Item)
 when ?is_chardata(Item) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,?get_env(),?wxControlWithItems_Append_1),
  wxe_util:rec(?wxControlWithItems_Append_1).

-doc """
Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this may be
different from the last one if the control is sorted (e.g. has `wxLB_SORT` or `wxCB_SORT`
style).
""".
-spec append(This, Item, ClientData) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), ClientData::term().
append(#wx_ref{type=ThisT}=This,Item,ClientData)
 when ?is_chardata(Item) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,ClientData,?get_env(),?wxControlWithItems_Append_2),
  wxe_util:rec(?wxControlWithItems_Append_2).

-doc """
Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one by one if
you need to add a lot of items.
""".
-spec appendStrings(This, Items) -> integer() when
	This::wxControlWithItems(), Items::[unicode:chardata()].
appendStrings(#wx_ref{type=ThisT}=This,Items)
 when is_list(Items) ->
  ?CLASS(ThisT,wxControlWithItems),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,?get_env(),?wxControlWithItems_appendStrings_1),
  wxe_util:rec(?wxControlWithItems_appendStrings_1).

-doc """
Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one by one if
you need to add a lot of items.
""".
-spec appendStrings(This, Items, ClientsData) -> integer() when
	This::wxControlWithItems(), Items::[unicode:chardata()], ClientsData::[term()].
appendStrings(#wx_ref{type=ThisT}=This,Items,ClientsData)
 when is_list(Items),is_list(ClientsData) ->
  ?CLASS(ThisT,wxControlWithItems),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,ClientsData,?get_env(),?wxControlWithItems_appendStrings_2),
  wxe_util:rec(?wxControlWithItems_appendStrings_2).

-doc """
Removes all items from the control.

`clear/1` also deletes the client data of the existing items if it is owned by the control.
""".
-spec clear(This) -> 'ok' when
	This::wxControlWithItems().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_Clear).

-doc """
Deletes an item from the control.

The client data associated with the item will be also deleted if it is owned by the
control. Note that it is an error (signalled by an assert failure in debug builds) to
remove an item with the index negative or greater or equal than the number of items in the control.

If there is a currently selected item below the item being deleted, i.e. if `getSelection/1` returns a
valid index greater than or equal to `n`, the selection is invalidated when this function
is called. However if the selected item appears before the item being deleted, the
selection is preserved unchanged.

See: `clear/1`
""".
-spec delete(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
delete(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_Delete).

-doc(#{equiv => findString(This,String, [])}).
-spec findString(This, String) -> integer() when
	This::wxControlWithItems(), String::unicode:chardata().

findString(This,String)
 when is_record(This, wx_ref),?is_chardata(String) ->
  findString(This,String, []).

-doc """
Finds an item whose label matches the given string.

Return: The zero-based position of the item, or wxNOT_FOUND if the string was not found.
""".
-spec findString(This, String, [Option]) -> integer() when
	This::wxControlWithItems(), String::unicode:chardata(),
	Option :: {'bCase', boolean()}.
findString(#wx_ref{type=ThisT}=This,String, Options)
 when ?is_chardata(String),is_list(Options) ->
  ?CLASS(ThisT,wxControlWithItems),
  String_UC = unicode:characters_to_binary(String),
  MOpts = fun({bCase, _bCase} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,String_UC, Opts,?get_env(),?wxControlWithItems_FindString),
  wxe_util:rec(?wxControlWithItems_FindString).

-doc """
Returns a pointer to the client data associated with the given item (if any).

It is an error to call this function for a control which doesn't have typed client data
at all although it is OK to call it even if the given item doesn't have any client data
associated with it (but other items do).

Notice that the returned pointer is still owned by the control and will be deleted by it,
use `DetachClientObject()` (not implemented in wx) if you want to remove the pointer from
the control.

Return: A pointer to the client data, or NULL if not present.
""".
-spec getClientData(This, N) -> term() when
	This::wxControlWithItems(), N::integer().
getClientData(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_getClientData),
  wxe_util:rec(?wxControlWithItems_getClientData).

-doc """
Associates the given typed client data pointer with the given item: the `data` object
will be deleted when the item is deleted (either explicitly by using `delete/2` or
implicitly when the control itself is destroyed).

Note that it is an error to call this function if any untyped client data pointers had
been associated with the control items before.
""".
-spec setClientData(This, N, Data) -> 'ok' when
	This::wxControlWithItems(), N::integer(), Data::term().
setClientData(#wx_ref{type=ThisT}=This,N,Data)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,Data,?get_env(),?wxControlWithItems_setClientData).

-doc """
Returns the number of items in the control.

See: `isEmpty/1`
""".
-spec getCount(This) -> integer() when
	This::wxControlWithItems().
getCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_GetCount),
  wxe_util:rec(?wxControlWithItems_GetCount).

-doc """
Returns the index of the selected item or `wxNOT\_FOUND` if no item is selected.

Return: The position of the current selection.

Remark: This method can be used with single selection list boxes only, you should use `wxListBox:getSelections/1`
for the list boxes with wxLB_MULTIPLE style.

See:
* `setSelection/2`

* `getStringSelection/1`
""".
-spec getSelection(This) -> integer() when
	This::wxControlWithItems().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_GetSelection),
  wxe_util:rec(?wxControlWithItems_GetSelection).

-doc """
Returns the label of the item with the given index.

Return: The label of the item or an empty string if the position was invalid.
""".
-spec getString(This, N) -> unicode:charlist() when
	This::wxControlWithItems(), N::integer().
getString(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_GetString),
  wxe_util:rec(?wxControlWithItems_GetString).

-doc """
Returns the label of the selected item or an empty string if no item is selected.

See: `getSelection/1`
""".
-spec getStringSelection(This) -> unicode:charlist() when
	This::wxControlWithItems().
getStringSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_GetStringSelection),
  wxe_util:rec(?wxControlWithItems_GetStringSelection).

-doc """
Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the insertion failed
for some reason, -1 is returned.
""".
-spec insert(This, Item, Pos) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), Pos::integer().
insert(#wx_ref{type=ThisT}=This,Item,Pos)
 when ?is_chardata(Item),is_integer(Pos) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,Pos,?get_env(),?wxControlWithItems_Insert_2),
  wxe_util:rec(?wxControlWithItems_Insert_2).

-doc """
Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the insertion failed
for some reason, -1 is returned.
""".
-spec insert(This, Item, Pos, ClientData) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), Pos::integer(), ClientData::term().
insert(#wx_ref{type=ThisT}=This,Item,Pos,ClientData)
 when ?is_chardata(Item),is_integer(Pos) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,Pos,ClientData,?get_env(),?wxControlWithItems_Insert_3),
  wxe_util:rec(?wxControlWithItems_Insert_3).

-doc """
Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one by one if
you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the insertion failed
for some reason, -1 is returned.
""".
-spec insertStrings(This, Items, Pos) -> integer() when
	This::wxControlWithItems(), Items::[unicode:chardata()], Pos::integer().
insertStrings(#wx_ref{type=ThisT}=This,Items,Pos)
 when is_list(Items),is_integer(Pos) ->
  ?CLASS(ThisT,wxControlWithItems),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,Pos,?get_env(),?wxControlWithItems_insertStrings_2),
  wxe_util:rec(?wxControlWithItems_insertStrings_2).

-doc """
Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one by one if
you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the insertion failed
for some reason, -1 is returned.
""".
-spec insertStrings(This, Items, Pos, ClientsData) -> integer() when
	This::wxControlWithItems(), Items::[unicode:chardata()], Pos::integer(), ClientsData::[term()].
insertStrings(#wx_ref{type=ThisT}=This,Items,Pos,ClientsData)
 when is_list(Items),is_integer(Pos),is_list(ClientsData) ->
  ?CLASS(ThisT,wxControlWithItems),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,Pos,ClientsData,?get_env(),?wxControlWithItems_insertStrings_3),
  wxe_util:rec(?wxControlWithItems_insertStrings_3).

-doc """
Returns true if the control is empty or false if it has some items.

See: `getCount/1`
""".
-spec isEmpty(This) -> boolean() when
	This::wxControlWithItems().
isEmpty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_IsEmpty),
  wxe_util:rec(?wxControlWithItems_IsEmpty).

-doc """
This is the same as `setSelection/2` and exists only because it is slightly more natural
for controls which support multiple selection.
""".
-spec select(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
select(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_Select).

-doc """
Sets the selection to the given item `n` or removes the selection entirely if `n` == `wxNOT\_FOUND`.

Note that this does not cause any command events to be emitted nor does it deselect any
other items in the controls which support multiple selections.

See:
* `setString/3`

* `setStringSelection/2`
""".
-spec setSelection(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
setSelection(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_SetSelection).

-doc "Sets the label for the given item.".
-spec setString(This, N, String) -> 'ok' when
	This::wxControlWithItems(), N::integer(), String::unicode:chardata().
setString(#wx_ref{type=ThisT}=This,N,String)
 when is_integer(N),?is_chardata(String) ->
  ?CLASS(ThisT,wxControlWithItems),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,N,String_UC,?get_env(),?wxControlWithItems_SetString).

-doc """
Selects the item with the specified string in the control.

This method doesn't cause any command events to be emitted.

Notice that this method is case-insensitive, i.e. the string is compared with all the
elements of the control case-insensitively and the first matching entry is selected, even
if it doesn't have exactly the same case as this string and there is an exact match afterwards.

Return: true if the specified string has been selected, false if it wasn't found in the
control.
""".
-spec setStringSelection(This, String) -> boolean() when
	This::wxControlWithItems(), String::unicode:chardata().
setStringSelection(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxControlWithItems),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxControlWithItems_SetStringSelection),
  wxe_util:rec(?wxControlWithItems_SetStringSelection).

 %% From wxControl
-doc false.
setLabel(This,Label) -> wxControl:setLabel(This,Label).
-doc false.
getLabel(This) -> wxControl:getLabel(This).
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
