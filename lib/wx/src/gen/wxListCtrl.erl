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

-module(wxListCtrl).
-moduledoc """
A list control presents lists in a number of formats: list view, report view, icon view
and small icon view.

In any case, elements are numbered from zero. For all these modes, the items are stored
in the control and must be added to it using `insertItem/4` method.

A special case of report view quite different from the other modes of the list control is
a virtual control in which the items data (including text, images and attributes) is
managed by the main program and is requested by the control itself only when needed which
allows having controls with millions of items without consuming much memory. To use
virtual list control you must use `setItemCount/2` first and override at least `wxListCtrl::OnGetItemText`
(not implemented in wx) (and optionally `wxListCtrl::OnGetItemImage` (not implemented in
wx) or `wxListCtrl::OnGetItemColumnImage` (not implemented in wx) and `wxListCtrl::OnGetItemAttr`
(not implemented in wx)) to return the information about the items when the control
requests it.

Virtual list control can be used as a normal one except that no operations which can take
time proportional to the number of items in the control happen - this is required to allow
having a practically infinite number of items. For example, in a multiple selection
virtual list control, the selections won't be sent when many items are selected at once
because this could mean iterating over all the items.

Using many of `m:wxListCtrl` features is shown in the corresponding sample.

To intercept events from a list control, use the event table macros described in `m:wxListEvent`.

`wxMac Note`: Starting with wxWidgets 2.8, `m:wxListCtrl` uses a native implementation
for report mode, and uses a generic implementation for other modes. You can use the
generic implementation for report mode as well by setting the `mac.listctrl.always_use_generic`
system option (see `m:wxSystemOptions`) to 1.

## Styles

This class supports the following styles:

* wxLC_LIST: Multicolumn list view, with optional small icons. Columns are computed
automatically, i.e. you don't set columns as in `wxLC_REPORT`. In other words, the list
wraps, unlike a `m:wxListBox`.

* wxLC_REPORT: Single or multicolumn report view, with optional header.

* wxLC_VIRTUAL: The application provides items text on demand. May only be used with `wxLC_REPORT`.

* wxLC_ICON: Large icon view, with optional labels.

* wxLC_SMALL_ICON: Small icon view, with optional labels.

* wxLC_ALIGN_TOP: Icons align to the top. Win32 default, Win32 only.

* wxLC_ALIGN_LEFT: Icons align to the left.

* wxLC_AUTOARRANGE: Icons arrange themselves. Win32 only.

* wxLC_EDIT_LABELS: Labels are editable: the application will be notified when editing
starts.

* wxLC_NO_HEADER: No header in report mode.

* wxLC_SINGLE_SEL: Single selection (default is multiple).

* wxLC_SORT_ASCENDING: Sort in ascending order. (You must still supply a comparison
callback in `sortItems/2`.)

* wxLC_SORT_DESCENDING: Sort in descending order. (You must still supply a comparison
callback in `sortItems/2`.)

* wxLC_HRULES: Draws light horizontal rules between rows in report mode.

* wxLC_VRULES: Draws light vertical rules between columns in report mode.

See:
* [Overview listctrl](https://docs.wxwidgets.org/3.2/overview_listctrl.html#overview_listctrl)

* `m:wxListView`

* `m:wxListBox`

* `m:wxTreeCtrl`

* `m:wxImageList`

* `m:wxListEvent`

* `m:wxListItem`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxListCtrl](https://docs.wxwidgets.org/3.2/classwx_list_ctrl.html)

## Events

Event types emitted from this class:

* [`command_list_begin_drag`](`m:wxListEvent`)

* [`command_list_begin_rdrag`](`m:wxListEvent`)

* [`command_list_begin_label_edit`](`m:wxListEvent`)

* [`command_list_end_label_edit`](`m:wxListEvent`)

* [`command_list_delete_item`](`m:wxListEvent`)

* [`command_list_delete_all_items`](`m:wxListEvent`)

* [`command_list_item_selected`](`m:wxListEvent`)

* [`command_list_item_deselected`](`m:wxListEvent`)

* [`command_list_item_activated`](`m:wxListEvent`)

* [`command_list_item_focused`](`m:wxListEvent`)

* [`command_list_item_middle_click`](`m:wxListEvent`)

* [`command_list_item_right_click`](`m:wxListEvent`)

* [`command_list_key_down`](`m:wxListEvent`)

* [`command_list_insert_item`](`m:wxListEvent`)

* [`command_list_col_click`](`m:wxListEvent`)

* [`command_list_col_right_click`](`m:wxListEvent`)

* [`command_list_col_begin_drag`](`m:wxListEvent`)

* [`command_list_col_dragging`](`m:wxListEvent`)

* [`command_list_col_end_drag`](`m:wxListEvent`)

* [`command_list_cache_hint`](`m:wxListEvent`)
""".
-include("wxe.hrl").
-export([ create/2, create/3 , new/0, new/1, new/2 , sortItems/2 ,arrange/1,
  arrange/2,assignImageList/3,clearAll/1,deleteAllItems/1,deleteColumn/2,
  deleteItem/2,destroy/1,editLabel/2,ensureVisible/2,findItem/3,findItem/4,
  getColumn/3,getColumnCount/1,getColumnWidth/2,getCountPerPage/1,getEditControl/1,
  getImageList/2,getItem/2,getItemBackgroundColour/2,getItemCount/1,
  getItemData/2,getItemFont/2,getItemPosition/2,getItemRect/2,getItemRect/3,
  getItemSpacing/1,getItemState/3,getItemText/2,getItemText/3,getItemTextColour/2,
  getNextItem/2,getNextItem/3,getSelectedItemCount/1,getTextColour/1,
  getTopItem/1,getViewRect/1,hitTest/2,insertColumn/3,insertColumn/4,
  insertItem/2,insertItem/3,insertItem/4,refreshItem/2,refreshItems/3,
  scrollList/3,setBackgroundColour/2,setColumn/3,setColumnWidth/3,setImageList/3,
  setItem/2,setItem/4,setItem/5,setItemBackgroundColour/3,setItemColumnImage/4,
  setItemCount/2,setItemData/3,setItemFont/3,setItemImage/3,setItemImage/4,
  setItemPosition/3,setItemState/4,setItemText/3,setItemTextColour/3,
  setSingleStyle/2,setSingleStyle/3,setTextColour/2,setWindowStyleFlag/2]).

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
  setAutoLayout/2,setBackgroundStyle/2,setCaret/2,setClientSize/2,setClientSize/3,
  setContainingSizer/2,setCursor/2,setDoubleBuffered/2,setDropTarget/2,
  setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,setForegroundColour/2,
  setHelpText/2,setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,
  setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,setPalette/2,
  setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,
  setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxListCtrl() :: wx:wx_object().
-export_type([wxListCtrl/0]).
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-spec new() -> wxListCtrl().
new() ->
    Op = ?wxListCtrl_new_0,
    wxe_util:queue_cmd(?get_env(), Op),
    wxe_util:rec(Op).


-spec new(Parent) -> wxListCtrl() when
      Parent::wxWindow:wxWindow().
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc Creates a listctrl with optional callback functions:
%%
%% OnGetItemText = (This, Item, Column) -> unicode:charlist()
%% OnGetItemAttr = (This, Item) -> wxListItemAttr:wxListItemAttr()
%% OnGetItemColumnImage = (This, Item, Column) -> integer()
%%
%% See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-spec new(Parent, [Option]) -> wxListCtrl() when
      Parent::wxWindow:wxWindow(),
      Option::{winid, integer()} |
	      {pos, {X::integer(),Y::integer()}} |
	      {size, {W::integer(),H::integer()}} |
	      {style, integer()} |
	      {validator, wx:wx_object()} |
	      {onGetItemText, function()} |
	      {onGetItemAttr, function()} |
	      {onGetItemColumnImage, function()}.

new(#wx_ref{}=Parent, Options)
  when is_list(Options)->
    %% ?wxListCtrl_new_2
    ListCtrl = new(),
    true = create(ListCtrl,Parent,Options),
    ListCtrl.

-doc(#{equiv => arrange(This, [])}).
-spec arrange(This) -> boolean() when
	This::wxListCtrl().

arrange(This)
 when is_record(This, wx_ref) ->
  arrange(This, []).

-doc """
Arranges the items in icon or small icon view.

This only has effect on Win32. `flag` is one of:

* wxLIST_ALIGN_DEFAULT: Default alignment.

* wxLIST_ALIGN_LEFT: Align to the left side of the control.

* wxLIST_ALIGN_TOP: Align to the top side of the control.

* wxLIST_ALIGN_SNAP_TO_GRID: Snap to grid.
""".
-spec arrange(This, [Option]) -> boolean() when
	This::wxListCtrl(),
	Option :: {'flag', integer()}.
arrange(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({flag, _flag} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxListCtrl_Arrange),
  wxe_util:rec(?wxListCtrl_Arrange).

-doc """
Sets the image list associated with the control and takes ownership of it (i.e.

the control will, unlike when using `setImageList/3`, delete the list when destroyed). `which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`, `wxIMAGE_LIST_STATE`
(the last is unimplemented).

See: `setImageList/3`
""".
-spec assignImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_AssignImageList).

-doc """
Deletes all items and all columns.

Note: This sends an event of type `wxEVT_LIST_DELETE_ALL_ITEMS` under all platforms.
""".
-spec clearAll(This) -> 'ok' when
	This::wxListCtrl().
clearAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_ClearAll).


%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow().
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> boolean() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow(),
      Option::{winid, integer()} |
	      {pos, {X::integer(),Y::integer()}} |
	      {size, {W::integer(),H::integer()}} |
	      {style, integer()} |
	      {validator, wx:wx_object()} |
	      {onGetItemText, function()} |
	      {onGetItemAttr, function()} |
	      {onGetItemColumnImage, function()}.

create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
    ?CLASS(ThisT,wxListCtrl),
    ?CLASS(ParentT,wxWindow),
    Op = ?wxListCtrl_Create,
    MOpts = fun({winid, _} = Arg) -> Arg;
               ({pos, {_posX,_posY}} = Arg) -> Arg;
               ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
               ({style, _style} = Arg) -> Arg;
               ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
               ({onGetItemText, Fun}) ->
                    ToStr = fun(A,B,C) -> unicode:characters_to_binary(Fun(A,B,C)) end,
                    {onGetItemText, wxe_util:get_cbId(ToStr)};
               ({onGetItemAttr, Fun}) -> {onGetItemAttr, wxe_util:get_cbId(Fun)};
               ({onGetItemColumnImage, Fun}) -> {onGetItemColumnImage, wxe_util:get_cbId(Fun)};
               (BadOpt) -> erlang:error({badoption, BadOpt}) end,
    Opts = lists:map(MOpts, Options),
    wxe_util:queue_cmd(This, Parent, Opts, ?get_env(), Op),
    wxe_util:rec(Op).

-doc """
Deletes all items in the list control.

This function does `not` send the `wxEVT_LIST_DELETE_ITEM` event because deleting many
items from the control would be too slow then (unlike `deleteItem/2`) but it does send the special `wxEVT_LIST_DELETE_ALL_ITEMS`
event if the control was not empty. If it was already empty, nothing is done and no event
is sent.

Return: true if the items were successfully deleted or if the control was already empty,
false if an error occurred while deleting the items.
""".
-spec deleteAllItems(This) -> boolean() when
	This::wxListCtrl().
deleteAllItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_DeleteAllItems),
  wxe_util:rec(?wxListCtrl_DeleteAllItems).

-doc "Deletes a column.".
-spec deleteColumn(This, Col) -> boolean() when
	This::wxListCtrl(), Col::integer().
deleteColumn(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_DeleteColumn),
  wxe_util:rec(?wxListCtrl_DeleteColumn).

-doc """
Deletes the specified item.

This function sends the `wxEVT_LIST_DELETE_ITEM` event for the item being deleted.

See: `deleteAllItems/1`
""".
-spec deleteItem(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
deleteItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_DeleteItem),
  wxe_util:rec(?wxListCtrl_DeleteItem).

-doc """
Starts editing the label of the given item.

This function generates a `EVT_LIST_BEGIN_LABEL_EDIT` event which can be vetoed so that
no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text control
without changes, a `EVT_LIST_END_LABEL_EDIT` event will be sent which can be vetoed as
well.
""".
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_EditLabel),
  wxe_util:rec(?wxListCtrl_EditLabel).

-doc "Ensures this item is visible.".
-spec ensureVisible(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
ensureVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_EnsureVisible),
  wxe_util:rec(?wxListCtrl_EnsureVisible).

-doc(#{equiv => findItem(This,Start,Str, [])}).
-spec findItem(This, Start, Str) -> integer() when
	This::wxListCtrl(), Start::integer(), Str::unicode:chardata().

findItem(This,Start,Str)
 when is_record(This, wx_ref),is_integer(Start),?is_chardata(Str) ->
  findItem(This,Start,Str, []).

-doc """
Find an item nearest this position in the specified direction, starting from `start` or
the beginning if `start` is -1.

Return: The next matching item if any or `-1` (wxNOT_FOUND) otherwise.
""".
-spec findItem(This, Start, Str, [Option]) -> integer() when
	This::wxListCtrl(), Start::integer(), Str::unicode:chardata(),
	Option :: {'partial', boolean()};
      (This, Start, Pt, Direction) -> integer() when
	This::wxListCtrl(), Start::integer(), Pt::{X::integer(), Y::integer()}, Direction::integer().
findItem(#wx_ref{type=ThisT}=This,Start,Str, Options)
 when is_integer(Start),?is_chardata(Str),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Str_UC = unicode:characters_to_binary(Str),
  MOpts = fun({partial, _partial} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Start,Str_UC, Opts,?get_env(),?wxListCtrl_FindItem_3_0),
  wxe_util:rec(?wxListCtrl_FindItem_3_0);
findItem(#wx_ref{type=ThisT}=This,Start,{PtX,PtY} = Pt,Direction)
 when is_integer(Start),is_integer(PtX),is_integer(PtY),is_integer(Direction) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Start,Pt,Direction,?get_env(),?wxListCtrl_FindItem_3_1),
  wxe_util:rec(?wxListCtrl_FindItem_3_1).

-doc """
Gets information about this column.

See `setItem/5` for more information.
""".
-spec getColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
getColumn(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=ItemT}=Item)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:queue_cmd(This,Col,Item,?get_env(),?wxListCtrl_GetColumn),
  wxe_util:rec(?wxListCtrl_GetColumn).

-doc "Returns the number of columns.".
-spec getColumnCount(This) -> integer() when
	This::wxListCtrl().
getColumnCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetColumnCount),
  wxe_util:rec(?wxListCtrl_GetColumnCount).

-doc "Gets the column width (report view only).".
-spec getColumnWidth(This, Col) -> integer() when
	This::wxListCtrl(), Col::integer().
getColumnWidth(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_GetColumnWidth),
  wxe_util:rec(?wxListCtrl_GetColumnWidth).

-doc """
Gets the number of items that can fit vertically in the visible area of the list control
(list or report view) or the total number of items in the list control (icon or small icon
view).
""".
-spec getCountPerPage(This) -> integer() when
	This::wxListCtrl().
getCountPerPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetCountPerPage),
  wxe_util:rec(?wxListCtrl_GetCountPerPage).

-doc """
Returns the edit control being currently used to edit a label.

Returns NULL if no label is being edited.

Note: It is currently only implemented for wxMSW and the generic version, not for the
native macOS version.
""".
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl().
getEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetEditControl),
  wxe_util:rec(?wxListCtrl_GetEditControl).

-doc """
Returns the specified image list.

`which` may be one of:

* wxIMAGE_LIST_NORMAL: The normal (large icon) image list.

* wxIMAGE_LIST_SMALL: The small icon image list.

* wxIMAGE_LIST_STATE: The user-defined state image list (unimplemented).
""".
-spec getImageList(This, Which) -> wxImageList:wxImageList() when
	This::wxListCtrl(), Which::integer().
getImageList(#wx_ref{type=ThisT}=This,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Which,?get_env(),?wxListCtrl_GetImageList),
  wxe_util:rec(?wxListCtrl_GetImageList).

-doc """
Gets information about the item.

See `setItem/5` for more information.

You must call `info.SetId()` to set the ID of item you're interested in before calling
this method, and `info.SetMask()` with the flags indicating what fields you need to
retrieve from `info`.
""".
-spec getItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
getItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_GetItem),
  wxe_util:rec(?wxListCtrl_GetItem).

-doc """
Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the default
background control of the control itself).

See: `getItemTextColour/2`
""".
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemBackgroundColour),
  wxe_util:rec(?wxListCtrl_GetItemBackgroundColour).

-doc "Returns the number of items in the list control.".
-spec getItemCount(This) -> integer() when
	This::wxListCtrl().
getItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemCount),
  wxe_util:rec(?wxListCtrl_GetItemCount).

-doc "Gets the application-defined data associated with this item.".
-spec getItemData(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemData),
  wxe_util:rec(?wxListCtrl_GetItemData).

-doc "Returns the item's font.".
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxListCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemFont),
  wxe_util:rec(?wxListCtrl_GetItemFont).

-doc "Returns the position of the item, in icon or small icon view.".
-spec getItemPosition(This, Item) -> Result when
	Result ::{Res ::boolean(), Pos::{X::integer(), Y::integer()}},
	This::wxListCtrl(), Item::integer().
getItemPosition(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemPosition),
  wxe_util:rec(?wxListCtrl_GetItemPosition).

-doc(#{equiv => getItemRect(This,Item, [])}).
-spec getItemRect(This, Item) -> Result when
	Result ::{Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxListCtrl(), Item::integer().

getItemRect(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemRect(This,Item, []).

-doc """
Returns the rectangle representing the item's size and position, in physical coordinates.

`code` is one of wxLIST_RECT_BOUNDS, wxLIST_RECT_ICON, wxLIST_RECT_LABEL.
""".
-spec getItemRect(This, Item, [Option]) -> Result when
	Result :: {Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxListCtrl(), Item::integer(),
	Option :: {'code', integer()}.
getItemRect(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({code, _code} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxListCtrl_GetItemRect),
  wxe_util:rec(?wxListCtrl_GetItemRect).

-doc """
Retrieves the spacing between icons in pixels: horizontal spacing is returned as `x`
component of the {Width,Height} object and the vertical spacing as its `y` component.
""".
-spec getItemSpacing(This) -> {W::integer(), H::integer()} when
	This::wxListCtrl().
getItemSpacing(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemSpacing),
  wxe_util:rec(?wxListCtrl_GetItemSpacing).

-doc """
Gets the item state.

For a list of state flags, see `setItem/5`. The `stateMask` indicates which state flags are of
interest.
""".
-spec getItemState(This, Item, StateMask) -> integer() when
	This::wxListCtrl(), Item::integer(), StateMask::integer().
getItemState(#wx_ref{type=ThisT}=This,Item,StateMask)
 when is_integer(Item),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,StateMask,?get_env(),?wxListCtrl_GetItemState),
  wxe_util:rec(?wxListCtrl_GetItemState).

-doc(#{equiv => getItemText(This,Item, [])}).
-spec getItemText(This, Item) -> unicode:charlist() when
	This::wxListCtrl(), Item::integer().

getItemText(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemText(This,Item, []).

-doc "Gets the item text for this item.".
-spec getItemText(This, Item, [Option]) -> unicode:charlist() when
	This::wxListCtrl(), Item::integer(),
	Option :: {'col', integer()}.
getItemText(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({col, _col} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxListCtrl_GetItemText),
  wxe_util:rec(?wxListCtrl_GetItemText).

-doc """
Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the default
foreground control of the control itself as this wouldn't allow distinguishing between
items having the same colour as the current control foreground and items with default
colour which, hence, have always the same colour as the control).
""".
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemTextColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemTextColour),
  wxe_util:rec(?wxListCtrl_GetItemTextColour).

-doc(#{equiv => getNextItem(This,Item, [])}).
-spec getNextItem(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().

getNextItem(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getNextItem(This,Item, []).

-doc """
Searches for an item with the given geometry or state, starting from `item` but excluding
the `item` itself.

If `item` is -1, the first item that matches the specified flags will be returned.
Returns the first item with given state following `item` or -1 if no such item found. This
function may be used to find all selected items in the control like this:

`geometry` can be one of:

* wxLIST_NEXT_ABOVE: Searches for an item above the specified item.

* wxLIST_NEXT_ALL: Searches for subsequent item by index.

* wxLIST_NEXT_BELOW: Searches for an item below the specified item.

* wxLIST_NEXT_LEFT: Searches for an item to the left of the specified item.

* wxLIST_NEXT_RIGHT: Searches for an item to the right of the specified item.

Note: this parameter is only supported by wxMSW currently and ignored on other platforms.

`state` can be a bitlist of the following:

* wxLIST_STATE_DONTCARE: Don't care what the state is.

* wxLIST_STATE_DROPHILITED: The item indicates it is a drop target.

* wxLIST_STATE_FOCUSED: The item has the focus.

* wxLIST_STATE_SELECTED: The item is selected.

* wxLIST_STATE_CUT: The item is selected as part of a cut and paste operation.
""".
-spec getNextItem(This, Item, [Option]) -> integer() when
	This::wxListCtrl(), Item::integer(),
	Option :: {'geometry', integer()}
		 | {'state', integer()}.
getNextItem(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({geometry, _geometry} = Arg) -> Arg;
          ({state, _state} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxListCtrl_GetNextItem),
  wxe_util:rec(?wxListCtrl_GetNextItem).

-doc "Returns the number of selected items in the list control.".
-spec getSelectedItemCount(This) -> integer() when
	This::wxListCtrl().
getSelectedItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetSelectedItemCount),
  wxe_util:rec(?wxListCtrl_GetSelectedItemCount).

-doc "Gets the text colour of the list control.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListCtrl().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTextColour),
  wxe_util:rec(?wxListCtrl_GetTextColour).

-doc "Gets the index of the topmost visible item when in list or report view.".
-spec getTopItem(This) -> integer() when
	This::wxListCtrl().
getTopItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTopItem),
  wxe_util:rec(?wxListCtrl_GetTopItem).

-doc """
Returns the rectangle taken by all items in the control.

In other words, if the controls client size were equal to the size of this rectangle, no
scrollbars would be needed and no free space would be left.

Note that this function only works in the icon and small icon views, not in list or
report views (this is a limitation of the native Win32 control).
""".
-spec getViewRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxListCtrl().
getViewRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetViewRect),
  wxe_util:rec(?wxListCtrl_GetViewRect).

-doc """
Determines which item (if any) is at the specified point, giving details in `flags`.

Returns index of the item or `wxNOT_FOUND` if no item is at the specified point.

`flags` will be a combination of the following flags:

* wxLIST_HITTEST_ABOVE: Above the control's client area.

* wxLIST_HITTEST_BELOW: Below the control's client area.

* wxLIST_HITTEST_TOLEFT: To the left of the control's client area.

* wxLIST_HITTEST_TORIGHT: To the right of the control's client area.

* wxLIST_HITTEST_NOWHERE: Inside the control's client area but not over an item.

* wxLIST_HITTEST_ONITEMICON: Over an item's icon.

* wxLIST_HITTEST_ONITEMLABEL: Over an item's text.

* wxLIST_HITTEST_ONITEMSTATEICON: Over the checkbox of an item.

* wxLIST_HITTEST_ONITEM: Combination of `wxLIST_HITTEST_ONITEMICON`, `wxLIST_HITTEST_ONITEMLABEL`, `wxLIST_HITTEST_ONITEMSTATEICON`.

If `ptrSubItem` is not NULL and the `m:wxListCtrl` is in the report mode the subitem (or
column) number will also be provided. This feature is only available in version 2.7.0 or
higher and is currently only implemented under wxMSW and requires at least comctl32.dll of
version 4.70 on the host system or the value stored in `ptrSubItem` will be always -1. To
compile this feature into wxWidgets library you need to have access to commctrl.h of
version 4.70 that is provided by Microsoft.
""".
-spec hitTest(This, Point) -> Result when
	Result ::{Res ::integer(), Flags::integer(), PtrSubItem::integer()},
	This::wxListCtrl(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxListCtrl_HitTest),
  wxe_util:rec(?wxListCtrl_HitTest).

-doc """
For report view mode (only), inserts a column.

For more details, see `setItem/5`. Also see `insertColumn/4` overload for a usually more convenient alternative to
this method and the description of how the item width is interpreted by this method.
""".
-spec insertColumn(This, Col, Heading) -> integer() when
	This::wxListCtrl(), Col::integer(), Heading::unicode:chardata();
      (This, Col, Info) -> integer() when
	This::wxListCtrl(), Col::integer(), Info::wxListItem:wxListItem().

insertColumn(This,Col,Heading)
 when is_record(This, wx_ref),is_integer(Col),?is_chardata(Heading) ->
  insertColumn(This,Col,Heading, []);
insertColumn(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=InfoT}=Info)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Col,Info,?get_env(),?wxListCtrl_InsertColumn_2),
  wxe_util:rec(?wxListCtrl_InsertColumn_2).

-doc """
For report view mode (only), inserts a column.

Insert a new column in the list control in report view mode at the given position
specifying its most common attributes.

Notice that to set the image for the column you need to use `insertColumn/4` overload and specify
?wxLIST\_MASK\_IMAGE in the item mask.

Return: The index of the inserted column or -1 if adding it failed.
""".
-spec insertColumn(This, Col, Heading, [Option]) -> integer() when
	This::wxListCtrl(), Col::integer(), Heading::unicode:chardata(),
	Option :: {'format', integer()}
		 | {'width', integer()}.
insertColumn(#wx_ref{type=ThisT}=This,Col,Heading, Options)
 when is_integer(Col),?is_chardata(Heading),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Heading_UC = unicode:characters_to_binary(Heading),
  MOpts = fun({format, _format} = Arg) -> Arg;
          ({width, _width} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Col,Heading_UC, Opts,?get_env(),?wxListCtrl_InsertColumn_3),
  wxe_util:rec(?wxListCtrl_InsertColumn_3).

-doc "Inserts an item, returning the index of the new item if successful, -1 otherwise.".
-spec insertItem(This, Info) -> integer() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
insertItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_InsertItem_1),
  wxe_util:rec(?wxListCtrl_InsertItem_1).

-doc "Insert a string item.".
-spec insertItem(This, Index, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), ImageIndex::integer();
      (This, Index, Label) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata().
insertItem(#wx_ref{type=ThisT}=This,Index,ImageIndex)
 when is_integer(Index),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Index,ImageIndex,?get_env(),?wxListCtrl_InsertItem_2_0),
  wxe_util:rec(?wxListCtrl_InsertItem_2_0);
insertItem(#wx_ref{type=ThisT}=This,Index,Label)
 when is_integer(Index),?is_chardata(Label) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Index,Label_UC,?get_env(),?wxListCtrl_InsertItem_2_1),
  wxe_util:rec(?wxListCtrl_InsertItem_2_1).

-doc "Insert an image/string item.".
-spec insertItem(This, Index, Label, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata(), ImageIndex::integer().
insertItem(#wx_ref{type=ThisT}=This,Index,Label,ImageIndex)
 when is_integer(Index),?is_chardata(Label),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Index,Label_UC,ImageIndex,?get_env(),?wxListCtrl_InsertItem_3),
  wxe_util:rec(?wxListCtrl_InsertItem_3).

-doc """
Redraws the given `item`.

This is only useful for the virtual list controls as without calling this function the
displayed value of the item doesn't change even when the underlying data does change.

See: `refreshItems/3`
""".
-spec refreshItem(This, Item) -> 'ok' when
	This::wxListCtrl(), Item::integer().
refreshItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_RefreshItem).

-doc """
Redraws the items between `itemFrom` and `itemTo`.

The starting item must be less than or equal to the ending one.

Just as `refreshItem/2` this is only useful for virtual list controls.
""".
-spec refreshItems(This, ItemFrom, ItemTo) -> 'ok' when
	This::wxListCtrl(), ItemFrom::integer(), ItemTo::integer().
refreshItems(#wx_ref{type=ThisT}=This,ItemFrom,ItemTo)
 when is_integer(ItemFrom),is_integer(ItemTo) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,ItemFrom,ItemTo,?get_env(),?wxListCtrl_RefreshItems).

-doc """
Scrolls the list control.

If in icon, small icon or report view mode, `dx` specifies the number of pixels to
scroll. If in list view mode, `dx` specifies the number of columns to scroll. `dy` always
specifies the number of pixels to scroll vertically.

Note: This method is currently only implemented in the Windows version.
""".
-spec scrollList(This, Dx, Dy) -> boolean() when
	This::wxListCtrl(), Dx::integer(), Dy::integer().
scrollList(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_integer(Dx),is_integer(Dy) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxListCtrl_ScrollList),
  wxe_util:rec(?wxListCtrl_ScrollList).

-doc """
Sets the background colour.

Note that the `wxWindow:getBackgroundColour/1` function of `m:wxWindow` base class can be used to retrieve the current
background colour.
""".
-spec setBackgroundColour(This, Col) -> boolean() when
	This::wxListCtrl(), Col::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetBackgroundColour),
  wxe_util:rec(?wxListCtrl_SetBackgroundColour).

-doc """
Sets information about this column.

See `setItem/5` for more information.
""".
-spec setColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
setColumn(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=ItemT}=Item)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:queue_cmd(This,Col,Item,?get_env(),?wxListCtrl_SetColumn),
  wxe_util:rec(?wxListCtrl_SetColumn).

-doc """
Sets the column width.

`width` can be a width in pixels or `wxLIST_AUTOSIZE` (-1) or `wxLIST_AUTOSIZE_USEHEADER` (-2).

`wxLIST_AUTOSIZE` will resize the column to the length of its longest item.

`wxLIST_AUTOSIZE_USEHEADER` will resize the column to the length of the header (Win32) or
80 pixels (other platforms).

In small or normal icon view, `col` must be -1, and the column width is set for all
columns.
""".
-spec setColumnWidth(This, Col, Width) -> boolean() when
	This::wxListCtrl(), Col::integer(), Width::integer().
setColumnWidth(#wx_ref{type=ThisT}=This,Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,Width,?get_env(),?wxListCtrl_SetColumnWidth),
  wxe_util:rec(?wxListCtrl_SetColumnWidth).

-doc """
Sets the image list associated with the control.

`which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`, `wxIMAGE_LIST_STATE` (the
last is unimplemented).

This method does not take ownership of the image list, you have to delete it yourself.

See: `assignImageList/3`
""".
-spec setImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_SetImageList).

-doc """
Sets the data of an item.

Using the `m:wxListItem`'s mask and state mask, you can change only selected attributes
of a `m:wxListCtrl` item.

Return: true if the item was successfully updated or false if the update failed for some
reason (e.g. an invalid item index).
""".
-spec setItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
setItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_SetItem_1),
  wxe_util:rec(?wxListCtrl_SetItem_1).

-doc(#{equiv => setItem(This,Index,Column,Label, [])}).
-spec setItem(This, Index, Column, Label) -> boolean() when
	This::wxListCtrl(), Index::integer(), Column::integer(), Label::unicode:chardata().

setItem(This,Index,Column,Label)
 when is_record(This, wx_ref),is_integer(Index),is_integer(Column),?is_chardata(Label) ->
  setItem(This,Index,Column,Label, []).

-doc """
Sets an item string field at a particular column.

Return: true if the item was successfully updated or false if the update failed for some
reason (e.g. an invalid item index).
""".
-spec setItem(This, Index, Column, Label, [Option]) -> boolean() when
	This::wxListCtrl(), Index::integer(), Column::integer(), Label::unicode:chardata(),
	Option :: {'imageId', integer()}.
setItem(#wx_ref{type=ThisT}=This,Index,Column,Label, Options)
 when is_integer(Index),is_integer(Column),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary(Label),
  MOpts = fun({imageId, _imageId} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index,Column,Label_UC, Opts,?get_env(),?wxListCtrl_SetItem_4),
  wxe_util:rec(?wxListCtrl_SetItem_4).

-doc """
Sets the background colour for this item.

This function only works in report view mode. The colour can be retrieved using `getItemBackgroundColour/2`.
""".
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemBackgroundColour).

-doc """
This method can only be used with virtual list controls.

It is used to indicate to the control the number of items it contains. After calling it,
the main program should be ready to handle calls to various item callbacks (such as `wxListCtrl::OnGetItemText`
(not implemented in wx)) for all items in the range from 0 to `count`.

Notice that the control is not necessarily redrawn after this call as it may be
undesirable if an item which is not visible on the screen anyhow was added to or removed
from a control displaying many items, if you do need to refresh the display you can just
call `wxWindow:refresh/2` manually.
""".
-spec setItemCount(This, Count) -> 'ok' when
	This::wxListCtrl(), Count::integer().
setItemCount(#wx_ref{type=ThisT}=This,Count)
 when is_integer(Count) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Count,?get_env(),?wxListCtrl_SetItemCount).

-doc """
Associates application-defined data with this item.

Notice that this function cannot be used to associate pointers with the control items,
use `SetItemPtrData()` (not implemented in wx) instead.
""".
-spec setItemData(This, Item, Data) -> boolean() when
	This::wxListCtrl(), Item::integer(), Data::integer().
setItemData(#wx_ref{type=ThisT}=This,Item,Data)
 when is_integer(Item),is_integer(Data) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Data,?get_env(),?wxListCtrl_SetItemData),
  wxe_util:rec(?wxListCtrl_SetItemData).

-doc "Sets the item's font.".
-spec setItemFont(This, Item, Font) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Font::wxFont:wxFont().
setItemFont(#wx_ref{type=ThisT}=This,Item,#wx_ref{type=FontT}=Font)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Item,Font,?get_env(),?wxListCtrl_SetItemFont).

-doc(#{equiv => setItemImage(This,Item,Image, [])}).
-spec setItemImage(This, Item, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Image::integer().

setItemImage(This,Item,Image)
 when is_record(This, wx_ref),is_integer(Item),is_integer(Image) ->
  setItemImage(This,Item,Image, []).

-doc """
Sets the unselected and selected images associated with the item.

The images are indices into the image list associated with the list control.
""".
-spec setItemImage(This, Item, Image, [Option]) -> boolean() when
	This::wxListCtrl(), Item::integer(), Image::integer(),
	Option :: {'selImage', integer()}.
setItemImage(#wx_ref{type=ThisT}=This,Item,Image, Options)
 when is_integer(Item),is_integer(Image),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({selImage, _selImage} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item,Image, Opts,?get_env(),?wxListCtrl_SetItemImage),
  wxe_util:rec(?wxListCtrl_SetItemImage).

-doc """
Sets the image associated with the item.

In report view, you can specify the column. The image is an index into the image list
associated with the list control.
""".
-spec setItemColumnImage(This, Item, Column, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Column::integer(), Image::integer().
setItemColumnImage(#wx_ref{type=ThisT}=This,Item,Column,Image)
 when is_integer(Item),is_integer(Column),is_integer(Image) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Column,Image,?get_env(),?wxListCtrl_SetItemColumnImage),
  wxe_util:rec(?wxListCtrl_SetItemColumnImage).

-doc """
Sets the position of the item, in icon or small icon view.

Windows only.
""".
-spec setItemPosition(This, Item, Pos) -> boolean() when
	This::wxListCtrl(), Item::integer(), Pos::{X::integer(), Y::integer()}.
setItemPosition(#wx_ref{type=ThisT}=This,Item,{PosX,PosY} = Pos)
 when is_integer(Item),is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Pos,?get_env(),?wxListCtrl_SetItemPosition),
  wxe_util:rec(?wxListCtrl_SetItemPosition).

-doc """
Sets the item state.

The `stateMask` is a combination of `wxLIST_STATE_XXX` constants described in `m:wxListItem`
documentation. For each of the bits specified in `stateMask`, the corresponding state is
set or cleared depending on whether `state` argument contains the same bit or not.

So to select an item you can use while to deselect it you should use

Consider using `m:wxListView` if possible to avoid dealing with this error-prone and
confusing method.

Also notice that contrary to the usual rule that only user actions generate events, this
method does generate wxEVT_LIST_ITEM_SELECTED event when it is used to select an item.
""".
-spec setItemState(This, Item, State, StateMask) -> boolean() when
	This::wxListCtrl(), Item::integer(), State::integer(), StateMask::integer().
setItemState(#wx_ref{type=ThisT}=This,Item,State,StateMask)
 when is_integer(Item),is_integer(State),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,State,StateMask,?get_env(),?wxListCtrl_SetItemState),
  wxe_util:rec(?wxListCtrl_SetItemState).

-doc "Sets the item text for this item.".
-spec setItemText(This, Item, Text) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Text::unicode:chardata().
setItemText(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxListCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxListCtrl_SetItemText).

-doc """
Sets the colour for this item.

This function only works in report view. The colour can be retrieved using `getItemTextColour/2`.
""".
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemTextColour).

-doc(#{equiv => setSingleStyle(This,Style, [])}).
-spec setSingleStyle(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().

setSingleStyle(This,Style)
 when is_record(This, wx_ref),is_integer(Style) ->
  setSingleStyle(This,Style, []).

-doc "Adds or removes a single window style.".
-spec setSingleStyle(This, Style, [Option]) -> 'ok' when
	This::wxListCtrl(), Style::integer(),
	Option :: {'add', boolean()}.
setSingleStyle(#wx_ref{type=ThisT}=This,Style, Options)
 when is_integer(Style),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({add, _add} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Style, Opts,?get_env(),?wxListCtrl_SetSingleStyle).

-doc "Sets the text colour of the list control.".
-spec setTextColour(This, Col) -> 'ok' when
	This::wxListCtrl(), Col::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetTextColour).

-doc "Sets the whole window style, deleting all items.".
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxListCtrl_SetWindowStyleFlag).


-spec sortItems(This::wxListCtrl(), SortCallBack) -> boolean()
              when SortCallBack :: fun((integer(), integer()) -> integer()).
sortItems(#wx_ref{type=ThisT}=This, SortCallBack)
  when is_function(SortCallBack, 2) ->
    ?CLASS(ThisT,wxListCtrl),
    SortId = wxe_util:get_cbId(SortCallBack),
    Op = ?wxListCtrl_SortItems,
    wxe_util:queue_cmd(This, SortId, ?get_env(), Op),
    wxe_util:rec(Op).

-doc "Destroys the object".
-spec destroy(This::wxListCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
