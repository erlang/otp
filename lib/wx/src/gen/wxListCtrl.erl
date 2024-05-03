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

-module(wxListCtrl).
-moduledoc """
Functions for wxListCtrl class

A list control presents lists in a number of formats: list view, report view,
icon view and small icon view. In any case, elements are numbered from zero. For
all these modes, the items are stored in the control and must be added to it
using `insertItem/4` method.

A special case of report view quite different from the other modes of the list
control is a virtual control in which the items data (including text, images and
attributes) is managed by the main program and is requested by the control
itself only when needed which allows having controls with millions of items
without consuming much memory. To use virtual list control you must use
`setItemCount/2` first and override at least `wxListCtrl::OnGetItemText` (not
implemented in wx) (and optionally `wxListCtrl::OnGetItemImage` (not implemented
in wx) or `wxListCtrl::OnGetItemColumnImage` (not implemented in wx) and
`wxListCtrl::OnGetItemAttr` (not implemented in wx)) to return the information
about the items when the control requests it.

Virtual list control can be used as a normal one except that no operations which
can take time proportional to the number of items in the control happen - this
is required to allow having a practically infinite number of items. For example,
in a multiple selection virtual list control, the selections won't be sent when
many items are selected at once because this could mean iterating over all the
items.

Using many of `m:wxListCtrl` features is shown in the corresponding sample.

To intercept events from a list control, use the event table macros described in
`m:wxListEvent`.

`wxMac Note`: Starting with wxWidgets 2.8, `m:wxListCtrl` uses a native
implementation for report mode, and uses a generic implementation for other
modes. You can use the generic implementation for report mode as well by setting
the `mac.listctrl.always_use_generic` system option (see `m:wxSystemOptions`)
to 1.

Styles

This class supports the following styles:

Note: Under wxMSW this control uses `wxSystemThemedControl` (not implemented in
wx) for an explorer style appearance by default since wxWidgets 3.1.0. If this
is not desired, you can call `wxSystemThemedControl::EnableSystemTheme` (not
implemented in wx) with `false` argument to disable this.

See:
[Overview listctrl](https://docs.wxwidgets.org/3.1/overview_listctrl.html#overview_listctrl),
`m:wxListView`, `m:wxListBox`, `m:wxTreeCtrl`, `m:wxImageList`, `m:wxListEvent`,
`m:wxListItem`, `wxEditableListBox` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxListCtrl](https://docs.wxwidgets.org/3.1/classwx_list_ctrl.html)

## Events

Event types emitted from this class:
[`command_list_begin_drag`](`m:wxListEvent`),
[`command_list_begin_rdrag`](`m:wxListEvent`),
[`command_list_begin_label_edit`](`m:wxListEvent`),
[`command_list_end_label_edit`](`m:wxListEvent`),
[`command_list_delete_item`](`m:wxListEvent`),
[`command_list_delete_all_items`](`m:wxListEvent`),
[`command_list_item_selected`](`m:wxListEvent`),
[`command_list_item_deselected`](`m:wxListEvent`),
[`command_list_item_activated`](`m:wxListEvent`),
[`command_list_item_focused`](`m:wxListEvent`),
[`command_list_item_middle_click`](`m:wxListEvent`),
[`command_list_item_right_click`](`m:wxListEvent`),
[`command_list_key_down`](`m:wxListEvent`),
[`command_list_insert_item`](`m:wxListEvent`),
[`command_list_col_click`](`m:wxListEvent`),
[`command_list_col_right_click`](`m:wxListEvent`),
[`command_list_col_begin_drag`](`m:wxListEvent`),
[`command_list_col_dragging`](`m:wxListEvent`),
[`command_list_col_end_drag`](`m:wxListEvent`),
[`command_list_cache_hint`](`m:wxListEvent`)
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
%% @hidden
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxListCtrl().
new() ->
    Op = ?wxListCtrl_new_0,
    wxe_util:queue_cmd(?get_env(), Op),
    wxe_util:rec(Op).


-doc false.
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
-doc """
Constructor, creating and showing a list control.

See: `create/3`, `wxValidator` (not implemented in wx)
""".
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

%% @equiv arrange(This, [])
-spec arrange(This) -> boolean() when
	This::wxListCtrl().

arrange(This)
 when is_record(This, wx_ref) ->
  arrange(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlarrange">external documentation</a>.
-doc """
Arranges the items in icon or small icon view.

This only has effect on Win32. `flag` is one of:
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlassignimagelist">external documentation</a>.
-doc """
Sets the image list associated with the control and takes ownership of it (i.e.

the control will, unlike when using `setImageList/3`, delete the list when
destroyed). `which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`,
`wxIMAGE_LIST_STATE` (the last is unimplemented).

See: `setImageList/3`
""".
-spec assignImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_AssignImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlclearall">external documentation</a>.
-doc """
Deletes all items and all columns.

Note: This sends an event of type `wxEVT_LIST_DELETE_ALL_ITEMS` under all
platforms.
""".
-spec clearAll(This) -> 'ok' when
	This::wxListCtrl().
clearAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_ClearAll).


%% @equiv create(This,Parent, [])
-doc false.
-spec create(This, Parent) -> boolean() when
      This::wxWindow:wxWindow(),
      Parent::wxWindow:wxWindow().
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlcreate">external documentation</a>.
-doc """
Creates the list control.

See `new/2` for further details.
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteallitems">external documentation</a>.
-doc """
Deletes all items in the list control.

This function does `not` send the `wxEVT_LIST_DELETE_ITEM` event because
deleting many items from the control would be too slow then (unlike
`deleteItem/2`) but it does send the special `wxEVT_LIST_DELETE_ALL_ITEMS` event
if the control was not empty. If it was already empty, nothing is done and no
event is sent.

Return: true if the items were successfully deleted or if the control was
already empty, false if an error occurred while deleting the items.
""".
-spec deleteAllItems(This) -> boolean() when
	This::wxListCtrl().
deleteAllItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_DeleteAllItems),
  wxe_util:rec(?wxListCtrl_DeleteAllItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeletecolumn">external documentation</a>.
-doc "Deletes a column.".
-spec deleteColumn(This, Col) -> boolean() when
	This::wxListCtrl(), Col::integer().
deleteColumn(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_DeleteColumn),
  wxe_util:rec(?wxListCtrl_DeleteColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteitem">external documentation</a>.
-doc """
Deletes the specified item.

This function sends the `wxEVT_LIST_DELETE_ITEM` event for the item being
deleted.

See: `deleteAllItems/1`
""".
-spec deleteItem(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
deleteItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_DeleteItem),
  wxe_util:rec(?wxListCtrl_DeleteItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrleditlabel">external documentation</a>.
-doc """
Starts editing the label of the given item.

This function generates a `EVT_LIST_BEGIN_LABEL_EDIT` event which can be vetoed
so that no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text
control without changes, a `EVT_LIST_END_LABEL_EDIT` event will be sent which
can be vetoed as well.
""".
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_EditLabel),
  wxe_util:rec(?wxListCtrl_EditLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlensurevisible">external documentation</a>.
-doc "Ensures this item is visible.".
-spec ensureVisible(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
ensureVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_EnsureVisible),
  wxe_util:rec(?wxListCtrl_EnsureVisible).

%% @equiv findItem(This,Start,Str, [])
-spec findItem(This, Start, Str) -> integer() when
	This::wxListCtrl(), Start::integer(), Str::unicode:chardata().

findItem(This,Start,Str)
 when is_record(This, wx_ref),is_integer(Start),?is_chardata(Str) ->
  findItem(This,Start,Str, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlfinditem">external documentation</a>.
%% <br /> Also:<br />
%% findItem(This, Start, Pt, Direction) -> integer() when<br />
%% 	This::wxListCtrl(), Start::integer(), Pt::{X::integer(), Y::integer()}, Direction::integer().<br />
%% 
-doc """
Find an item nearest this position in the specified direction, starting from
`start` or the beginning if `start` is -1.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumn">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumncount">external documentation</a>.
-doc "Returns the number of columns.".
-spec getColumnCount(This) -> integer() when
	This::wxListCtrl().
getColumnCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetColumnCount),
  wxe_util:rec(?wxListCtrl_GetColumnCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumnwidth">external documentation</a>.
-doc "Gets the column width (report view only).".
-spec getColumnWidth(This, Col) -> integer() when
	This::wxListCtrl(), Col::integer().
getColumnWidth(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_GetColumnWidth),
  wxe_util:rec(?wxListCtrl_GetColumnWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcountperpage">external documentation</a>.
-doc """
Gets the number of items that can fit vertically in the visible area of the list
control (list or report view) or the total number of items in the list control
(icon or small icon view).
""".
-spec getCountPerPage(This) -> integer() when
	This::wxListCtrl().
getCountPerPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetCountPerPage),
  wxe_util:rec(?wxListCtrl_GetCountPerPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgeteditcontrol">external documentation</a>.
-doc """
Returns the edit control being currently used to edit a label.

Returns NULL if no label is being edited.

Note: It is currently only implemented for wxMSW and the generic version, not
for the native macOS version.
""".
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl().
getEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetEditControl),
  wxe_util:rec(?wxListCtrl_GetEditControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetimagelist">external documentation</a>.
-doc """
Returns the specified image list.

`which` may be one of:
""".
-spec getImageList(This, Which) -> wxImageList:wxImageList() when
	This::wxListCtrl(), Which::integer().
getImageList(#wx_ref{type=ThisT}=This,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Which,?get_env(),?wxListCtrl_GetImageList),
  wxe_util:rec(?wxListCtrl_GetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitem">external documentation</a>.
-doc """
Gets information about the item.

See `setItem/5` for more information.

You must call `info.SetId()` to set the ID of item you're interested in before
calling this method, and `info.SetMask()` with the flags indicating what fields
you need to retrieve from `info`.
""".
-spec getItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
getItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_GetItem),
  wxe_util:rec(?wxListCtrl_GetItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitembackgroundcolour">external documentation</a>.
-doc """
Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the
default background control of the control itself).

See: `getItemTextColour/2`
""".
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemBackgroundColour),
  wxe_util:rec(?wxListCtrl_GetItemBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemcount">external documentation</a>.
-doc "Returns the number of items in the list control.".
-spec getItemCount(This) -> integer() when
	This::wxListCtrl().
getItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemCount),
  wxe_util:rec(?wxListCtrl_GetItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemdata">external documentation</a>.
-doc "Gets the application-defined data associated with this item.".
-spec getItemData(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemData),
  wxe_util:rec(?wxListCtrl_GetItemData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemfont">external documentation</a>.
-doc "Returns the item's font.".
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxListCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemFont),
  wxe_util:rec(?wxListCtrl_GetItemFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemposition">external documentation</a>.
-doc "Returns the position of the item, in icon or small icon view.".
-spec getItemPosition(This, Item) -> Result when
	Result ::{Res ::boolean(), Pos::{X::integer(), Y::integer()}},
	This::wxListCtrl(), Item::integer().
getItemPosition(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemPosition),
  wxe_util:rec(?wxListCtrl_GetItemPosition).

%% @equiv getItemRect(This,Item, [])
-spec getItemRect(This, Item) -> Result when
	Result ::{Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxListCtrl(), Item::integer().

getItemRect(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemRect(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemrect">external documentation</a>.
-doc """
Returns the rectangle representing the item's size and position, in physical
coordinates.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemspacing">external documentation</a>.
-doc """
Retrieves the spacing between icons in pixels: horizontal spacing is returned as
`x` component of the \{Width,Height\} object and the vertical spacing as its `y`
component.
""".
-spec getItemSpacing(This) -> {W::integer(), H::integer()} when
	This::wxListCtrl().
getItemSpacing(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemSpacing),
  wxe_util:rec(?wxListCtrl_GetItemSpacing).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemstate">external documentation</a>.
-doc """
Gets the item state.

For a list of state flags, see `setItem/5`. The `stateMask` indicates which
state flags are of interest.
""".
-spec getItemState(This, Item, StateMask) -> integer() when
	This::wxListCtrl(), Item::integer(), StateMask::integer().
getItemState(#wx_ref{type=ThisT}=This,Item,StateMask)
 when is_integer(Item),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,StateMask,?get_env(),?wxListCtrl_GetItemState),
  wxe_util:rec(?wxListCtrl_GetItemState).

%% @equiv getItemText(This,Item, [])
-spec getItemText(This, Item) -> unicode:charlist() when
	This::wxListCtrl(), Item::integer().

getItemText(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemText(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemtext">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemtextcolour">external documentation</a>.
-doc """
Returns the colour for this item.

If the item has no specific colour, returns an invalid colour (and not the
default foreground control of the control itself as this wouldn't allow
distinguishing between items having the same colour as the current control
foreground and items with default colour which, hence, have always the same
colour as the control).
""".
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemTextColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemTextColour),
  wxe_util:rec(?wxListCtrl_GetItemTextColour).

%% @equiv getNextItem(This,Item, [])
-spec getNextItem(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().

getNextItem(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getNextItem(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetnextitem">external documentation</a>.
-doc """
Searches for an item with the given geometry or state, starting from `item` but
excluding the `item` itself.

If `item` is -1, the first item that matches the specified flags will be
returned. Returns the first item with given state following `item` or -1 if no
such item found. This function may be used to find all selected items in the
control like this:

`geometry` can be one of:

Note: this parameter is only supported by wxMSW currently and ignored on other
platforms.

`state` can be a bitlist of the following:
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetselecteditemcount">external documentation</a>.
-doc "Returns the number of selected items in the list control.".
-spec getSelectedItemCount(This) -> integer() when
	This::wxListCtrl().
getSelectedItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetSelectedItemCount),
  wxe_util:rec(?wxListCtrl_GetSelectedItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettextcolour">external documentation</a>.
-doc "Gets the text colour of the list control.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListCtrl().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTextColour),
  wxe_util:rec(?wxListCtrl_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettopitem">external documentation</a>.
-doc "Gets the index of the topmost visible item when in list or report view.".
-spec getTopItem(This) -> integer() when
	This::wxListCtrl().
getTopItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTopItem),
  wxe_util:rec(?wxListCtrl_GetTopItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetviewrect">external documentation</a>.
-doc """
Returns the rectangle taken by all items in the control.

In other words, if the controls client size were equal to the size of this
rectangle, no scrollbars would be needed and no free space would be left.

Note that this function only works in the icon and small icon views, not in list
or report views (this is a limitation of the native Win32 control).
""".
-spec getViewRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxListCtrl().
getViewRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetViewRect),
  wxe_util:rec(?wxListCtrl_GetViewRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlhittest">external documentation</a>.
-doc """
Determines which item (if any) is at the specified point, giving details in
`flags`.

Returns index of the item or `wxNOT_FOUND` if no item is at the specified point.

`flags` will be a combination of the following flags:

If `ptrSubItem` is not NULL and the `m:wxListCtrl` is in the report mode the
subitem (or column) number will also be provided. This feature is only available
in version 2.7.0 or higher and is currently only implemented under wxMSW and
requires at least comctl32.dll of version 4.70 on the host system or the value
stored in `ptrSubItem` will be always -1. To compile this feature into wxWidgets
library you need to have access to commctrl.h of version 4.70 that is provided
by Microsoft.
""".
-spec hitTest(This, Point) -> Result when
	Result ::{Res ::integer(), Flags::integer(), PtrSubItem::integer()},
	This::wxListCtrl(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxListCtrl_HitTest),
  wxe_util:rec(?wxListCtrl_HitTest).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertcolumn">external documentation</a>.
%% <br /> Also:<br />
%% insertColumn(This, Col, Info) -> integer() when<br />
%% 	This::wxListCtrl(), Col::integer(), Info::wxListItem:wxListItem().<br />
%% 
-doc """
For report view mode (only), inserts a column.

For more details, see `setItem/5`. Also see `insertColumn/4` overload for a
usually more convenient alternative to this method and the description of how
the item width is interpreted by this method.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertcolumn">external documentation</a>.
-doc """
For report view mode (only), inserts a column.

Insert a new column in the list control in report view mode at the given
position specifying its most common attributes.

Notice that to set the image for the column you need to use `insertColumn/4`
overload and specify ?wxLIST_MASK_IMAGE in the item mask.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
-doc """
Inserts an item, returning the index of the new item if successful, -1
otherwise.
""".
-spec insertItem(This, Info) -> integer() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
insertItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_InsertItem_1),
  wxe_util:rec(?wxListCtrl_InsertItem_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
%% <br /> Also:<br />
%% insertItem(This, Index, Label) -> integer() when<br />
%% 	This::wxListCtrl(), Index::integer(), Label::unicode:chardata().<br />
%% 
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
-doc "Insert an image/string item.".
-spec insertItem(This, Index, Label, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata(), ImageIndex::integer().
insertItem(#wx_ref{type=ThisT}=This,Index,Label,ImageIndex)
 when is_integer(Index),?is_chardata(Label),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Index,Label_UC,ImageIndex,?get_env(),?wxListCtrl_InsertItem_3),
  wxe_util:rec(?wxListCtrl_InsertItem_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitem">external documentation</a>.
-doc """
Redraws the given `item`.

This is only useful for the virtual list controls as without calling this
function the displayed value of the item doesn't change even when the underlying
data does change.

See: `refreshItems/3`
""".
-spec refreshItem(This, Item) -> 'ok' when
	This::wxListCtrl(), Item::integer().
refreshItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_RefreshItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitems">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlscrolllist">external documentation</a>.
-doc """
Scrolls the list control.

If in icon, small icon or report view mode, `dx` specifies the number of pixels
to scroll. If in list view mode, `dx` specifies the number of columns to scroll.
`dy` always specifies the number of pixels to scroll vertically.

Note: This method is currently only implemented in the Windows version.
""".
-spec scrollList(This, Dx, Dy) -> boolean() when
	This::wxListCtrl(), Dx::integer(), Dy::integer().
scrollList(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_integer(Dx),is_integer(Dy) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxListCtrl_ScrollList),
  wxe_util:rec(?wxListCtrl_ScrollList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetbackgroundcolour">external documentation</a>.
-doc """
Sets the background colour.

Note that the `wxWindow:getBackgroundColour/1` function of `m:wxWindow` base
class can be used to retrieve the current background colour.
""".
-spec setBackgroundColour(This, Col) -> boolean() when
	This::wxListCtrl(), Col::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetBackgroundColour),
  wxe_util:rec(?wxListCtrl_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumn">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumnwidth">external documentation</a>.
-doc """
Sets the column width.

`width` can be a width in pixels or `wxLIST_AUTOSIZE` (-1) or
`wxLIST_AUTOSIZE_USEHEADER` (-2).

`wxLIST_AUTOSIZE` will resize the column to the length of its longest item.

`wxLIST_AUTOSIZE_USEHEADER` will resize the column to the length of the header
(Win32) or 80 pixels (other platforms).

In small or normal icon view, `col` must be -1, and the column width is set for
all columns.
""".
-spec setColumnWidth(This, Col, Width) -> boolean() when
	This::wxListCtrl(), Col::integer(), Width::integer().
setColumnWidth(#wx_ref{type=ThisT}=This,Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,Width,?get_env(),?wxListCtrl_SetColumnWidth),
  wxe_util:rec(?wxListCtrl_SetColumnWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetimagelist">external documentation</a>.
-doc """
Sets the image list associated with the control.

`which` is one of `wxIMAGE_LIST_NORMAL`, `wxIMAGE_LIST_SMALL`,
`wxIMAGE_LIST_STATE` (the last is unimplemented).

This method does not take ownership of the image list, you have to delete it
yourself.

See: `assignImageList/3`
""".
-spec setImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_SetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitem">external documentation</a>.
-doc """
Sets the data of an item.

Using the `m:wxListItem`'s mask and state mask, you can change only selected
attributes of a `m:wxListCtrl` item.

Return: true if the item was successfully updated or false if the update failed
for some reason (e.g. an invalid item index).
""".
-spec setItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
setItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_SetItem_1),
  wxe_util:rec(?wxListCtrl_SetItem_1).

%% @equiv setItem(This,Index,Column,Label, [])
-spec setItem(This, Index, Column, Label) -> boolean() when
	This::wxListCtrl(), Index::integer(), Column::integer(), Label::unicode:chardata().

setItem(This,Index,Column,Label)
 when is_record(This, wx_ref),is_integer(Index),is_integer(Column),?is_chardata(Label) ->
  setItem(This,Index,Column,Label, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitem">external documentation</a>.
-doc """
Sets an item string field at a particular column.

Return: true if the item was successfully updated or false if the update failed
for some reason (e.g. an invalid item index).
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitembackgroundcolour">external documentation</a>.
-doc """
Sets the background colour for this item.

This function only works in report view mode. The colour can be retrieved using
`getItemBackgroundColour/2`.
""".
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemcount">external documentation</a>.
-doc """
This method can only be used with virtual list controls.

It is used to indicate to the control the number of items it contains. After
calling it, the main program should be ready to handle calls to various item
callbacks (such as `wxListCtrl::OnGetItemText` (not implemented in wx)) for all
items in the range from 0 to `count`.

Notice that the control is not necessarily redrawn after this call as it may be
undesirable if an item which is not visible on the screen anyhow was added to or
removed from a control displaying many items, if you do need to refresh the
display you can just call `wxWindow:refresh/2` manually.
""".
-spec setItemCount(This, Count) -> 'ok' when
	This::wxListCtrl(), Count::integer().
setItemCount(#wx_ref{type=ThisT}=This,Count)
 when is_integer(Count) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Count,?get_env(),?wxListCtrl_SetItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemdata">external documentation</a>.
-doc """
Associates application-defined data with this item.

Notice that this function cannot be used to associate pointers with the control
items, use `SetItemPtrData()` (not implemented in wx) instead.
""".
-spec setItemData(This, Item, Data) -> boolean() when
	This::wxListCtrl(), Item::integer(), Data::integer().
setItemData(#wx_ref{type=ThisT}=This,Item,Data)
 when is_integer(Item),is_integer(Data) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Data,?get_env(),?wxListCtrl_SetItemData),
  wxe_util:rec(?wxListCtrl_SetItemData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemfont">external documentation</a>.
-doc "Sets the item's font.".
-spec setItemFont(This, Item, Font) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Font::wxFont:wxFont().
setItemFont(#wx_ref{type=ThisT}=This,Item,#wx_ref{type=FontT}=Font)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Item,Font,?get_env(),?wxListCtrl_SetItemFont).

%% @equiv setItemImage(This,Item,Image, [])
-spec setItemImage(This, Item, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Image::integer().

setItemImage(This,Item,Image)
 when is_record(This, wx_ref),is_integer(Item),is_integer(Image) ->
  setItemImage(This,Item,Image, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemimage">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemcolumnimage">external documentation</a>.
-doc """
Sets the image associated with the item.

In report view, you can specify the column. The image is an index into the image
list associated with the list control.
""".
-spec setItemColumnImage(This, Item, Column, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Column::integer(), Image::integer().
setItemColumnImage(#wx_ref{type=ThisT}=This,Item,Column,Image)
 when is_integer(Item),is_integer(Column),is_integer(Image) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Column,Image,?get_env(),?wxListCtrl_SetItemColumnImage),
  wxe_util:rec(?wxListCtrl_SetItemColumnImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemposition">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemstate">external documentation</a>.
-doc """
Sets the item state.

The `stateMask` is a combination of `wxLIST_STATE_XXX` constants described in
`m:wxListItem` documentation. For each of the bits specified in `stateMask`, the
corresponding state is set or cleared depending on whether `state` argument
contains the same bit or not.

So to select an item you can use while to deselect it you should use

Consider using `m:wxListView` if possible to avoid dealing with this error-prone
and confusing method.

Also notice that contrary to the usual rule that only user actions generate
events, this method does generate wxEVT_LIST_ITEM_SELECTED event when it is used
to select an item.
""".
-spec setItemState(This, Item, State, StateMask) -> boolean() when
	This::wxListCtrl(), Item::integer(), State::integer(), StateMask::integer().
setItemState(#wx_ref{type=ThisT}=This,Item,State,StateMask)
 when is_integer(Item),is_integer(State),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,State,StateMask,?get_env(),?wxListCtrl_SetItemState),
  wxe_util:rec(?wxListCtrl_SetItemState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtext">external documentation</a>.
-doc "Sets the item text for this item.".
-spec setItemText(This, Item, Text) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Text::unicode:chardata().
setItemText(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxListCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxListCtrl_SetItemText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtextcolour">external documentation</a>.
-doc """
Sets the colour for this item.

This function only works in report view. The colour can be retrieved using
`getItemTextColour/2`.
""".
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemTextColour).

%% @equiv setSingleStyle(This,Style, [])
-spec setSingleStyle(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().

setSingleStyle(This,Style)
 when is_record(This, wx_ref),is_integer(Style) ->
  setSingleStyle(This,Style, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetsinglestyle">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsettextcolour">external documentation</a>.
-doc "Sets the text colour of the list control.".
-spec setTextColour(This, Col) -> 'ok' when
	This::wxListCtrl(), Col::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,Col)
 when ?is_colordata(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetwindowstyleflag">external documentation</a>.
-doc "Sets the whole window style, deleting all items.".
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxListCtrl_SetWindowStyleFlag).


-doc """
Sort the items in the list control.

Sorts the items with supplied `SortCallBack` fun.

SortCallBack receives the client data associated with two items to compare
(`NOT` the the index), and should return 0 if the items are equal, a negative
value if the first item is less than the second one and a positive value if the
first item is greater than the second one.

Remark: Notice that the control may only be sorted on client data associated
with the items, so you must use SetItemData if you want to be able to sort the
items in the control.

The callback may not call other (wx) processes.
""".
-spec sortItems(This::wxListCtrl(), SortCallBack) -> boolean()
              when SortCallBack :: fun((integer(), integer()) -> integer()).
sortItems(#wx_ref{type=ThisT}=This, SortCallBack)
  when is_function(SortCallBack, 2) ->
    ?CLASS(ThisT,wxListCtrl),
    SortId = wxe_util:get_cbId(SortCallBack),
    Op = ?wxListCtrl_SortItems,
    wxe_util:queue_cmd(This, SortId, ?get_env(), Op),
    wxe_util:rec(Op).

%% @doc Destroys this object, do not use object again
-doc "Destructor, destroying the list control.".
-spec destroy(This::wxListCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
