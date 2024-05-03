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

-module(wxControlWithItems).
-moduledoc """
Functions for wxControlWithItems class

This is convenience class that derives from both `m:wxControl` and
`wxItemContainer` (not implemented in wx). It is used as basis for some
wxWidgets controls (`m:wxChoice` and `m:wxListBox`).

See: `wxItemContainer` (not implemented in wx), `wxItemContainerImmutable` (not
implemented in wx)

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxControlWithItems](https://docs.wxwidgets.org/3.1/classwx_control_with_items.html)
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
%% @hidden
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsappend">external documentation</a>.
-doc """
Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this
may be different from the last one if the control is sorted (e.g. has
`wxLB_SORT` or `wxCB_SORT` style).
""".
-spec append(This, Item) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata().
append(#wx_ref{type=ThisT}=This,Item)
 when ?is_chardata(Item) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,?get_env(),?wxControlWithItems_Append_1),
  wxe_util:rec(?wxControlWithItems_Append_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsappend">external documentation</a>.
-doc """
Appends item into the control.

Return: The return value is the index of the newly inserted item. Note that this
may be different from the last one if the control is sorted (e.g. has
`wxLB_SORT` or `wxCB_SORT` style).
""".
-spec append(This, Item, ClientData) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), ClientData::term().
append(#wx_ref{type=ThisT}=This,Item,ClientData)
 when ?is_chardata(Item) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,ClientData,?get_env(),?wxControlWithItems_Append_2),
  wxe_util:rec(?wxControlWithItems_Append_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsappend">external documentation</a>.
-doc """
Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one
by one if you need to add a lot of items.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsappend">external documentation</a>.
-doc """
Appends several items at once into the control.

Notice that calling this method is usually much faster than appending them one
by one if you need to add a lot of items.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsclear">external documentation</a>.
-doc """
Removes all items from the control.

`clear/1` also deletes the client data of the existing items if it is owned by
the control.
""".
-spec clear(This) -> 'ok' when
	This::wxControlWithItems().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsdelete">external documentation</a>.
-doc """
Deletes an item from the control.

The client data associated with the item will be also deleted if it is owned by
the control. Note that it is an error (signalled by an assert failure in debug
builds) to remove an item with the index negative or greater or equal than the
number of items in the control.

If there is a currently selected item below the item being deleted, i.e. if
`getSelection/1` returns a valid index greater than or equal to `n`, the
selection is invalidated when this function is called. However if the selected
item appears before the item being deleted, the selection is preserved
unchanged.

See: `clear/1`
""".
-spec delete(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
delete(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_Delete).

%% @equiv findString(This,String, [])
-spec findString(This, String) -> integer() when
	This::wxControlWithItems(), String::unicode:chardata().

findString(This,String)
 when is_record(This, wx_ref),?is_chardata(String) ->
  findString(This,String, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsfindstring">external documentation</a>.
-doc """
Finds an item whose label matches the given string.

Return: The zero-based position of the item, or wxNOT_FOUND if the string was
not found.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsgetclientobject">external documentation</a>.
-doc """
Returns a pointer to the client data associated with the given item (if any).

It is an error to call this function for a control which doesn't have typed
client data at all although it is OK to call it even if the given item doesn't
have any client data associated with it (but other items do).

Notice that the returned pointer is still owned by the control and will be
deleted by it, use `DetachClientObject()` (not implemented in wx) if you want to
remove the pointer from the control.

Return: A pointer to the client data, or NULL if not present.
""".
-spec getClientData(This, N) -> term() when
	This::wxControlWithItems(), N::integer().
getClientData(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_getClientData),
  wxe_util:rec(?wxControlWithItems_getClientData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemssetclientobject">external documentation</a>.
-doc """
Associates the given typed client data pointer with the given item: the `data`
object will be deleted when the item is deleted (either explicitly by using
`delete/2` or implicitly when the control itself is destroyed).

Note that it is an error to call this function if any untyped client data
pointers had been associated with the control items before.
""".
-spec setClientData(This, N, Data) -> 'ok' when
	This::wxControlWithItems(), N::integer(), Data::term().
setClientData(#wx_ref{type=ThisT}=This,N,Data)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,Data,?get_env(),?wxControlWithItems_setClientData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsgetcount">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsgetselection">external documentation</a>.
-doc """
Returns the index of the selected item or `wxNOT_FOUND` if no item is selected.

Return: The position of the current selection.

Remark: This method can be used with single selection list boxes only, you
should use `wxListBox:getSelections/1` for the list boxes with wxLB_MULTIPLE
style.

See: `setSelection/2`, `getStringSelection/1`
""".
-spec getSelection(This) -> integer() when
	This::wxControlWithItems().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_GetSelection),
  wxe_util:rec(?wxControlWithItems_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsgetstring">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsgetstringselection">external documentation</a>.
-doc """
Returns the label of the selected item or an empty string if no item is
selected.

See: `getSelection/1`
""".
-spec getStringSelection(This) -> unicode:charlist() when
	This::wxControlWithItems().
getStringSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,?get_env(),?wxControlWithItems_GetStringSelection),
  wxe_util:rec(?wxControlWithItems_GetStringSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsinsert">external documentation</a>.
-doc """
Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the
insertion failed for some reason, -1 is returned.
""".
-spec insert(This, Item, Pos) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), Pos::integer().
insert(#wx_ref{type=ThisT}=This,Item,Pos)
 when ?is_chardata(Item),is_integer(Pos) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,Pos,?get_env(),?wxControlWithItems_Insert_2),
  wxe_util:rec(?wxControlWithItems_Insert_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsinsert">external documentation</a>.
-doc """
Inserts item into the control.

Return: The return value is the index of the newly inserted item. If the
insertion failed for some reason, -1 is returned.
""".
-spec insert(This, Item, Pos, ClientData) -> integer() when
	This::wxControlWithItems(), Item::unicode:chardata(), Pos::integer(), ClientData::term().
insert(#wx_ref{type=ThisT}=This,Item,Pos,ClientData)
 when ?is_chardata(Item),is_integer(Pos) ->
  ?CLASS(ThisT,wxControlWithItems),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,Pos,ClientData,?get_env(),?wxControlWithItems_Insert_3),
  wxe_util:rec(?wxControlWithItems_Insert_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsinsert">external documentation</a>.
-doc """
Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one
by one if you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the
insertion failed for some reason, -1 is returned.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsinsert">external documentation</a>.
-doc """
Inserts several items at once into the control.

Notice that calling this method is usually much faster than inserting them one
by one if you need to insert a lot of items.

Return: The return value is the index of the last inserted item. If the
insertion failed for some reason, -1 is returned.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsisempty">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemsselect">external documentation</a>.
-doc """
This is the same as `setSelection/2` and exists only because it is slightly more
natural for controls which support multiple selection.
""".
-spec select(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
select(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_Select).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemssetselection">external documentation</a>.
-doc """
Sets the selection to the given item `n` or removes the selection entirely if
`n` == `wxNOT_FOUND`.

Note that this does not cause any command events to be emitted nor does it
deselect any other items in the controls which support multiple selections.

See: `setString/3`, `setStringSelection/2`
""".
-spec setSelection(This, N) -> 'ok' when
	This::wxControlWithItems(), N::integer().
setSelection(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxControlWithItems),
  wxe_util:queue_cmd(This,N,?get_env(),?wxControlWithItems_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemssetstring">external documentation</a>.
-doc "Sets the label for the given item.".
-spec setString(This, N, String) -> 'ok' when
	This::wxControlWithItems(), N::integer(), String::unicode:chardata().
setString(#wx_ref{type=ThisT}=This,N,String)
 when is_integer(N),?is_chardata(String) ->
  ?CLASS(ThisT,wxControlWithItems),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,N,String_UC,?get_env(),?wxControlWithItems_SetString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcontrolwithitems.html#wxcontrolwithitemssetstringselection">external documentation</a>.
-doc """
Selects the item with the specified string in the control.

This method doesn't cause any command events to be emitted.

Notice that this method is case-insensitive, i.e. the string is compared with
all the elements of the control case-insensitively and the first matching entry
is selected, even if it doesn't have exactly the same case as this string and
there is an exact match afterwards.

Return: true if the specified string has been selected, false if it wasn't found
in the control.
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
%% @hidden
-doc false.
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
-doc false.
getLabel(This) -> wxControl:getLabel(This).
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
