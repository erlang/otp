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

%%
%% Note: The representation of treeItemId() have changed from the original class implementation to be an semi-opaque type,Equality between TreeItemId's can be tested and zero means that the TreeItem is invalid.

%%
-module(wxTreeCtrl).
-moduledoc """
A tree control presents information as a hierarchy, with items that may be expanded to
show further items.

Items in a tree control are referenced by `wxTreeItemId` (not implemented in wx) handles,
which may be tested for validity by calling `wxTreeItemId::IsOk()` (not implemented in wx).

A similar control with a fully native implementation for GTK+ and macOS as well is `wxDataViewTreeCtrl`
(not implemented in wx).

To intercept events from a tree control, use the event table macros described in `m:wxTreeEvent`.

## Styles

This class supports the following styles:

* wxTR_EDIT_LABELS: Use this style if you wish the user to be able to edit labels in the
tree control.

* wxTR_NO_BUTTONS: For convenience to document that no buttons are to be drawn.

* wxTR_HAS_BUTTONS: Use this style to show + and - buttons to the left of parent items.

* wxTR_TWIST_BUTTONS: Selects alternative style of +/`-` buttons and shows rotating
("twisting") arrows instead. Currently this style is only implemented under Microsoft
Windows Vista and later Windows versions and is ignored under the other platforms as
enabling it is equivalent to using `wxSystemThemedControl::EnableSystemTheme()` (not
implemented in wx).

* wxTR_NO_LINES: Use this style to hide vertical level connectors.

* wxTR_FULL_ROW_HIGHLIGHT: Use this style to have the background colour and the selection
highlight extend over the entire horizontal row of the tree control window. (This flag is
ignored under Windows unless you specify `wxTR_NO_LINES` as well.)

* wxTR_LINES_AT_ROOT: Use this style to show lines leading to the root nodes (unless no `wxTR_NO_LINES`
is also used, in which case no lines are shown). Note that in the MSW version, if this
style is omitted, not only the lines, but also the button used for expanding the root item
is not shown, which can be unexpected, so it is recommended to always use it.

* wxTR_HIDE_ROOT: Use this style to suppress the display of the root node, effectively
causing the first-level nodes to appear as a series of root nodes.

* wxTR_ROW_LINES: Use this style to draw a contrasting border between displayed rows.

* wxTR_HAS_VARIABLE_ROW_HEIGHT: Use this style to cause row heights to be just big enough
to fit the content. If not set, all rows use the largest row height. The default is that
this flag is unset. Generic only.

* wxTR_SINGLE: For convenience to document that only one item may be selected at a time.
Selecting another item causes the current selection, if any, to be deselected. This is the
default.

* wxTR_MULTIPLE: Use this style to allow a range of items to be selected. If a second range
is selected, the current range, if any, is deselected.

* wxTR_DEFAULT_STYLE: The set of flags that are closest to the defaults for the native
control for a particular toolkit.

See also overview_windowstyles.

`Win32` `notes:`

`m:wxTreeCtrl` class uses the standard common treeview control under Win32 implemented in
the system library comctl32.dll. Some versions of this library are known to have bugs with
handling the tree control colours: the usual symptom is that the expanded items leave
black (or otherwise incorrectly coloured) background behind them, especially for the
controls using non-default background colour. The recommended solution is to upgrade the
comctl32.dll to a newer version: see [http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2](http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2)

See:
* `m:wxTreeEvent`

* [Overview treectrl](https://docs.wxwidgets.org/3.2/overview_treectrl.html#overview_treectrl)

* `m:wxListBox`

* `m:wxListCtrl`

* `m:wxImageList`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTreeCtrl](https://docs.wxwidgets.org/3.2/classwx_tree_ctrl.html)

## Events

Event types emitted from this class:

* [`command_tree_begin_drag`](`m:wxTreeEvent`)

* [`command_tree_begin_rdrag`](`m:wxTreeEvent`)

* [`command_tree_end_drag`](`m:wxTreeEvent`)

* [`command_tree_begin_label_edit`](`m:wxTreeEvent`)

* [`command_tree_end_label_edit`](`m:wxTreeEvent`)

* [`command_tree_delete_item`](`m:wxTreeEvent`)

* [`command_tree_get_info`](`m:wxTreeEvent`)

* [`command_tree_set_info`](`m:wxTreeEvent`)

* [`command_tree_item_activated`](`m:wxTreeEvent`)

* [`command_tree_item_collapsed`](`m:wxTreeEvent`)

* [`command_tree_item_collapsing`](`m:wxTreeEvent`)

* [`command_tree_item_expanded`](`m:wxTreeEvent`)

* [`command_tree_item_expanding`](`m:wxTreeEvent`)

* [`command_tree_item_right_click`](`m:wxTreeEvent`)

* [`command_tree_item_middle_click`](`m:wxTreeEvent`)

* [`command_tree_sel_changed`](`m:wxTreeEvent`)

* [`command_tree_sel_changing`](`m:wxTreeEvent`)

* [`command_tree_key_down`](`m:wxTreeEvent`)

* [`command_tree_item_gettooltip`](`m:wxTreeEvent`)

* [`command_tree_item_menu`](`m:wxTreeEvent`)

* [`command_tree_state_image_click`](`m:wxTreeEvent`)
""".
-include("wxe.hrl").
-export([addRoot/2,addRoot/3,appendItem/3,appendItem/4,assignImageList/2,assignStateImageList/2,
  collapse/2,collapseAndReset/2,create/2,create/3,delete/2,deleteAllItems/1,
  deleteChildren/2,destroy/1,editLabel/2,ensureVisible/2,expand/2,getBoundingRect/2,
  getBoundingRect/3,getChildrenCount/2,getChildrenCount/3,getCount/1,
  getEditControl/1,getFirstChild/2,getFirstVisibleItem/1,getImageList/1,
  getIndent/1,getItemBackgroundColour/2,getItemData/2,getItemFont/2,
  getItemImage/2,getItemImage/3,getItemParent/2,getItemText/2,getItemTextColour/2,
  getLastChild/2,getNextChild/3,getNextSibling/2,getNextVisible/2,getPrevSibling/2,
  getPrevVisible/2,getRootItem/1,getSelection/1,getSelections/1,getStateImageList/1,
  hitTest/2,insertItem/4,insertItem/5,isBold/2,isExpanded/2,isSelected/2,
  isTreeItemIdOk/1,isVisible/2,itemHasChildren/2,new/0,new/1,new/2,prependItem/3,
  prependItem/4,scrollTo/2,selectItem/2,selectItem/3,setImageList/2,
  setIndent/2,setItemBackgroundColour/3,setItemBold/2,setItemBold/3,
  setItemData/3,setItemDropHighlight/2,setItemDropHighlight/3,setItemFont/3,
  setItemHasChildren/2,setItemHasChildren/3,setItemImage/3,setItemImage/4,
  setItemText/3,setItemTextColour/3,setStateImageList/2,setWindowStyle/2,
  sortChildren/2,toggle/2,toggleItemSelection/2,unselect/1,unselectAll/1,
  unselectItem/2]).

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
  setVirtualSize/3,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxTreeCtrl() :: wx:wx_object().
-export_type([wxTreeCtrl/0]).
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default Constructor.".
-spec new() -> wxTreeCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTreeCtrl_new_0),
  wxe_util:rec(?wxTreeCtrl_new_0).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxTreeCtrl() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor, creating and showing a tree control.

See: `create/3`
""".
-spec new(Parent, [Option]) -> wxTreeCtrl() when
	Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxTreeCtrl_new_2),
  wxe_util:rec(?wxTreeCtrl_new_2).

-doc(#{equiv => addRoot(This,Text, [])}).
-spec addRoot(This, Text) -> integer() when
	This::wxTreeCtrl(), Text::unicode:chardata().

addRoot(This,Text)
 when is_record(This, wx_ref),?is_chardata(Text) ->
  addRoot(This,Text, []).

-doc """
Adds the root node to the tree, returning the new item.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.
""".
-spec addRoot(This, Text, [Option]) -> integer() when
	This::wxTreeCtrl(), Text::unicode:chardata(),
	Option :: {'image', integer()}
		 | {'selectedImage', integer()}
		 | {'data', term()}.
addRoot(#wx_ref{type=ThisT}=This,Text, Options)
 when ?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({image, _image} = Arg) -> Arg;
          ({selectedImage, _selectedImage} = Arg) -> Arg;
          ({data, _data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text_UC, Opts,?get_env(),?wxTreeCtrl_AddRoot),
  wxe_util:rec(?wxTreeCtrl_AddRoot).

-doc(#{equiv => appendItem(This,Parent,Text, [])}).
-spec appendItem(This, Parent, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata().

appendItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),?is_chardata(Text) ->
  appendItem(This,Parent,Text, []).

-doc """
Appends an item to the end of the branch identified by `parent`, return a new item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.
""".
-spec appendItem(This, Parent, Text, [Option]) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata(),
	Option :: {'image', integer()}
		 | {'selectedImage', integer()}
		 | {'data', term()}.
appendItem(#wx_ref{type=ThisT}=This,Parent,Text, Options)
 when is_integer(Parent),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({image, _image} = Arg) -> Arg;
          ({selectedImage, _selectedImage} = Arg) -> Arg;
          ({data, _data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Text_UC, Opts,?get_env(),?wxTreeCtrl_AppendItem),
  wxe_util:rec(?wxTreeCtrl_AppendItem).

-doc """
Sets the normal image list.

The image list assigned with this method will be automatically deleted by `m:wxTreeCtrl`
as appropriate (i.e. it takes ownership of the list).

See: `setImageList/2`
""".
-spec assignImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_AssignImageList).

-doc """
Sets the state image list.

Image list assigned with this method will be automatically deleted by `m:wxTreeCtrl` as
appropriate (i.e. it takes ownership of the list).

See: `setStateImageList/2`
""".
-spec assignStateImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
assignStateImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_AssignStateImageList).

-doc "Collapses the given item.".
-spec collapse(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
collapse(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Collapse).

-doc "Collapses the given item and removes all children.".
-spec collapseAndReset(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
collapseAndReset(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_CollapseAndReset).

-doc(#{equiv => create(This,Parent, [])}).
-spec create(This, Parent) -> boolean() when
	This::wxTreeCtrl(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

-doc """
Creates the tree control.

See `new/2` for further details.
""".
-spec create(This, Parent, [Option]) -> boolean() when
	This::wxTreeCtrl(), Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent, Opts,?get_env(),?wxTreeCtrl_Create),
  wxe_util:rec(?wxTreeCtrl_Create).

-doc """
Deletes the specified item.

A `EVT_TREE_DELETE_ITEM` event will be generated.

This function may cause a subsequent call to `getNextChild/3` to fail.
""".
-spec delete(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
delete(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Delete).

-doc """
Deletes all items in the control.

This function generates `wxEVT_TREE_DELETE_ITEM` events for each item being deleted,
including the root one if it is shown, i.e. unless wxTR_HIDE_ROOT style is used.
""".
-spec deleteAllItems(This) -> 'ok' when
	This::wxTreeCtrl().
deleteAllItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_DeleteAllItems).

-doc """
Deletes all children of the given item (but not the item itself).

A `wxEVT_TREE_DELETE_ITEM` event will be generated for every item being deleted.

If you have called `setItemHasChildren/3`, you may need to call it again since `deleteChildren/2` does not automatically clear
the setting.
""".
-spec deleteChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
deleteChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_DeleteChildren).

-doc """
Starts editing the label of the given `item`.

This function generates a `EVT_TREE_BEGIN_LABEL_EDIT` event which can be vetoed so that
no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text control
without changes, a `EVT_TREE_END_LABEL_EDIT` event will be sent which can be vetoed as well.

See: `m:wxTreeEvent`
""".
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxTreeCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_EditLabel),
  wxe_util:rec(?wxTreeCtrl_EditLabel).

-doc """
Scrolls and/or expands items to ensure that the given item is visible.

This method can be used, and will work, even while the window is frozen (see `wxWindow:freeze/1`).
""".
-spec ensureVisible(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
ensureVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_EnsureVisible).

-doc "Expands the given item.".
-spec expand(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
expand(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Expand).

-doc(#{equiv => getBoundingRect(This,Item, [])}).
-spec getBoundingRect(This, Item) -> Result when
	Result ::{Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxTreeCtrl(), Item::integer().

getBoundingRect(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getBoundingRect(This,Item, []).

-doc """
Retrieves the rectangle bounding the `item`.

If `textOnly` is true, only the rectangle around the item's label will be returned,
otherwise the item's image is also taken into account.

The return value is true if the rectangle was successfully retrieved or false if it was
not (in this case `rect` is not changed) - for example, if the item is currently invisible.

Notice that the rectangle coordinates are logical, not physical ones. So, for example,
the x coordinate may be negative if the tree has a horizontal scrollbar and its position
is not 0.
""".
-spec getBoundingRect(This, Item, [Option]) -> Result when
	Result :: {Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'textOnly', boolean()}.
getBoundingRect(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({textOnly, _textOnly} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_GetBoundingRect),
  wxe_util:rec(?wxTreeCtrl_GetBoundingRect).

-doc(#{equiv => getChildrenCount(This,Item, [])}).
-spec getChildrenCount(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().

getChildrenCount(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getChildrenCount(This,Item, []).

-doc """
Returns the number of items in the branch.

If `recursively` is true, returns the total number of descendants, otherwise only one
level of children is counted.
""".
-spec getChildrenCount(This, Item, [Option]) -> integer() when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'recursively', boolean()}.
getChildrenCount(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({recursively, _recursively} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_GetChildrenCount),
  wxe_util:rec(?wxTreeCtrl_GetChildrenCount).

-doc "Returns the number of items in the control.".
-spec getCount(This) -> integer() when
	This::wxTreeCtrl().
getCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetCount),
  wxe_util:rec(?wxTreeCtrl_GetCount).

-doc """
Returns the edit control being currently used to edit a label.

Returns NULL if no label is being edited.

Note: This is currently only implemented for wxMSW.
""".
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when
	This::wxTreeCtrl().
getEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetEditControl),
  wxe_util:rec(?wxTreeCtrl_GetEditControl).

-doc """
Returns the first child; call `getNextChild/3` for the next child.

For this enumeration function you must pass in a 'cookie' parameter which is opaque for
the application but is necessary for the library to make these functions reentrant (i.e.
allow more than one enumeration on one and the same object simultaneously). The cookie
passed to `getFirstChild/2` and `getNextChild/3` should be the same variable.

Returns an invalid tree item (i.e. `wxTreeItemId::IsOk()` (not implemented in wx) returns
false) if there are no further children.

See:
* `getNextChild/3`

* `getNextSibling/2`
""".
-spec getFirstChild(This, Item) -> Result when
	Result ::{Res ::integer(), Cookie::integer()},
	This::wxTreeCtrl(), Item::integer().
getFirstChild(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetFirstChild),
  wxe_util:rec(?wxTreeCtrl_GetFirstChild).

-doc """
Returns the next child; call `getFirstChild/2` for the first child.

For this enumeration function you must pass in a 'cookie' parameter which is opaque for
the application but is necessary for the library to make these functions reentrant (i.e.
allow more than one enumeration on one and the same object simultaneously). The cookie
passed to `getFirstChild/2` and `getNextChild/3` should be the same.

Returns an invalid tree item if there are no further children.

See: `getFirstChild/2`
""".
-spec getNextChild(This, Item, Cookie) -> Result when
	Result ::{Res ::integer(), Cookie::integer()},
	This::wxTreeCtrl(), Item::integer(), Cookie::integer().
getNextChild(#wx_ref{type=ThisT}=This,Item,Cookie)
 when is_integer(Item),is_integer(Cookie) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,Cookie,?get_env(),?wxTreeCtrl_GetNextChild),
  wxe_util:rec(?wxTreeCtrl_GetNextChild).

-doc "Returns the first visible item.".
-spec getFirstVisibleItem(This) -> integer() when
	This::wxTreeCtrl().
getFirstVisibleItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetFirstVisibleItem),
  wxe_util:rec(?wxTreeCtrl_GetFirstVisibleItem).

-doc "Returns the normal image list.".
-spec getImageList(This) -> wxImageList:wxImageList() when
	This::wxTreeCtrl().
getImageList(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetImageList),
  wxe_util:rec(?wxTreeCtrl_GetImageList).

-doc "Returns the current tree control indentation.".
-spec getIndent(This) -> integer() when
	This::wxTreeCtrl().
getIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetIndent),
  wxe_util:rec(?wxTreeCtrl_GetIndent).

-doc "Returns the background colour of the item.".
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxTreeCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemBackgroundColour),
  wxe_util:rec(?wxTreeCtrl_GetItemBackgroundColour).

-doc "Returns the tree item data associated with the item.".
-spec getItemData(This, Item) -> term() when
	This::wxTreeCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemData),
  wxe_util:rec(?wxTreeCtrl_GetItemData).

-doc """
Returns the font of the item label.

If the font hadn't been explicitly set for the specified `item` with `setItemFont/3`, returns an invalid
?wxNullFont font. `wxWindow:getFont/1` can be used to retrieve the global tree control font used for the items
without any specific font.
""".
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxTreeCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemFont),
  wxe_util:rec(?wxTreeCtrl_GetItemFont).

-doc(#{equiv => getItemImage(This,Item, [])}).
-spec getItemImage(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().

getItemImage(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemImage(This,Item, []).

-doc """
Gets the specified item image.

The value of `which` may be:

* ?wxTreeItemIcon\_Normal: to get the normal item image.

* ?wxTreeItemIcon\_Selected: to get the selected item image (i.e. the image which is shown
when the item is currently selected).

* ?wxTreeItemIcon\_Expanded: to get the expanded image (this only makes sense for items
which have children - then this image is shown when the item is expanded and the normal
image is shown when it is collapsed).

* ?wxTreeItemIcon\_SelectedExpanded: to get the selected expanded image (which is shown
when an expanded item is currently selected).
""".
%%  Which = ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
-spec getItemImage(This, Item, [Option]) -> integer() when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'which', wx:wx_enum()}.
getItemImage(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({which, _which} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_GetItemImage),
  wxe_util:rec(?wxTreeCtrl_GetItemImage).

-doc "Returns the item label.".
-spec getItemText(This, Item) -> unicode:charlist() when
	This::wxTreeCtrl(), Item::integer().
getItemText(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemText),
  wxe_util:rec(?wxTreeCtrl_GetItemText).

-doc "Returns the colour of the item label.".
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when
	This::wxTreeCtrl(), Item::integer().
getItemTextColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemTextColour),
  wxe_util:rec(?wxTreeCtrl_GetItemTextColour).

-doc """
Returns the last child of the item (or an invalid tree item if this item has no
children).

See:
* `getFirstChild/2`

* `getNextSibling/2`

* `getLastChild/2`
""".
-spec getLastChild(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getLastChild(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetLastChild),
  wxe_util:rec(?wxTreeCtrl_GetLastChild).

-doc """
Returns the next sibling of the specified item; call `getPrevSibling/2` for the previous
sibling.

Returns an invalid tree item if there are no further siblings.

See: `getPrevSibling/2`
""".
-spec getNextSibling(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getNextSibling(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetNextSibling),
  wxe_util:rec(?wxTreeCtrl_GetNextSibling).

-doc """
Returns the next visible item or an invalid item if this item is the last visible one.

Note: The `item` itself must be visible.
""".
-spec getNextVisible(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getNextVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetNextVisible),
  wxe_util:rec(?wxTreeCtrl_GetNextVisible).

-doc "Returns the item's parent.".
-spec getItemParent(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getItemParent(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemParent),
  wxe_util:rec(?wxTreeCtrl_GetItemParent).

-doc """
Returns the previous sibling of the specified item; call `getNextSibling/2` for the next
sibling.

Returns an invalid tree item if there are no further children.

See: `getNextSibling/2`
""".
-spec getPrevSibling(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getPrevSibling(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetPrevSibling),
  wxe_util:rec(?wxTreeCtrl_GetPrevSibling).

-doc """
Returns the previous visible item or an invalid item if this item is the first visible
one.

Note: The `item` itself must be visible.
""".
-spec getPrevVisible(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getPrevVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetPrevVisible),
  wxe_util:rec(?wxTreeCtrl_GetPrevVisible).

-doc "Returns the root item for the tree control.".
-spec getRootItem(This) -> integer() when
	This::wxTreeCtrl().
getRootItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetRootItem),
  wxe_util:rec(?wxTreeCtrl_GetRootItem).

-doc """
Returns the selection, or an invalid item if there is no selection.

This function only works with the controls without `wxTR_MULTIPLE` style, use `getSelections/1` for the
controls which do have this style or, if a single item is wanted, use `GetFocusedItem()`
(not implemented in wx).
""".
-spec getSelection(This) -> integer() when
	This::wxTreeCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetSelection),
  wxe_util:rec(?wxTreeCtrl_GetSelection).

-doc """
Fills the array of tree items passed in with the currently selected items.

This function can be called only if the control has the `wxTR_MULTIPLE` style.

Returns the number of selected items.
""".
-spec getSelections(This) -> Result when
	Result ::{Res ::integer(), Selection::[integer()]},
	This::wxTreeCtrl().
getSelections(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetSelections),
  wxe_util:rec(?wxTreeCtrl_GetSelections).

-doc "Returns the state image list (from which application-defined state images are taken).".
-spec getStateImageList(This) -> wxImageList:wxImageList() when
	This::wxTreeCtrl().
getStateImageList(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetStateImageList),
  wxe_util:rec(?wxTreeCtrl_GetStateImageList).

-doc """
Calculates which (if any) item is under the given `point`, returning the tree item id at
this point plus extra information `flags`.

`flags` is a bitlist of the following:

* `wxTREE_HITTEST_ABOVE:` Above the client area.

* `wxTREE_HITTEST_BELOW:` Below the client area.

* `wxTREE_HITTEST_NOWHERE:` In the client area but below the last item.

* `wxTREE_HITTEST_ONITEMBUTTON:` On the button associated with an item.

* `wxTREE_HITTEST_ONITEMICON:` On the bitmap associated with an item.

* `wxTREE_HITTEST_ONITEMINDENT:` In the indentation associated with an item.

* `wxTREE_HITTEST_ONITEMLABEL:` On the label (string) associated with an item.

* `wxTREE_HITTEST_ONITEMRIGHT:` In the area to the right of an item.

* `wxTREE_HITTEST_ONITEMSTATEICON:` On the state icon for a tree view item that is in a
user-defined state.

* `wxTREE_HITTEST_TOLEFT:` To the right of the client area.

* `wxTREE_HITTEST_TORIGHT:` To the left of the client area.
""".
-spec hitTest(This, Point) -> Result when
	Result ::{Res ::integer(), Flags::integer()},
	This::wxTreeCtrl(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxTreeCtrl_HitTest),
  wxe_util:rec(?wxTreeCtrl_HitTest).

-doc(#{equiv => insertItem(This,Parent,Previous,Text, [])}).
-spec insertItem(This, Parent, Previous, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Previous::integer(), Text::unicode:chardata().

insertItem(This,Parent,Previous,Text)
 when is_record(This, wx_ref),is_integer(Parent),is_integer(Previous),?is_chardata(Text) ->
  insertItem(This,Parent,Previous,Text, []).

-doc """
Inserts an item after a given one (`previous`).

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.
""".
-spec insertItem(This, Parent, Previous, Text, [Option]) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Previous::integer(), Text::unicode:chardata(),
	Option :: {'image', integer()}
		 | {'selImage', integer()}
		 | {'data', term()}.
insertItem(#wx_ref{type=ThisT}=This,Parent,Previous,Text, Options)
 when is_integer(Parent),is_integer(Previous),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({image, _image} = Arg) -> Arg;
          ({selImage, _selImage} = Arg) -> Arg;
          ({data, _data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Previous,Text_UC, Opts,?get_env(),?wxTreeCtrl_InsertItem),
  wxe_util:rec(?wxTreeCtrl_InsertItem).

-doc """
Returns true if the given item is in bold state.

See: `setItemBold/3`
""".
-spec isBold(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isBold(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsBold),
  wxe_util:rec(?wxTreeCtrl_IsBold).

-doc "Returns true if the item is expanded (only makes sense if it has children).".
-spec isExpanded(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isExpanded(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsExpanded),
  wxe_util:rec(?wxTreeCtrl_IsExpanded).

-doc "Returns true if the item is selected.".
-spec isSelected(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isSelected(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsSelected),
  wxe_util:rec(?wxTreeCtrl_IsSelected).

-doc "Returns true if the item is visible on the screen.".
-spec isVisible(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsVisible),
  wxe_util:rec(?wxTreeCtrl_IsVisible).

-doc "Returns true if the item has children.".
-spec itemHasChildren(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
itemHasChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_ItemHasChildren),
  wxe_util:rec(?wxTreeCtrl_ItemHasChildren).

-doc "Returns true if the item is valid.".
-spec isTreeItemIdOk(Item) -> boolean() when
	Item::integer().
isTreeItemIdOk(Item)
 when is_integer(Item) ->
  wxe_util:queue_cmd(Item,?get_env(),?wxTreeCtrl_IsTreeItemIdOk),
  wxe_util:rec(?wxTreeCtrl_IsTreeItemIdOk).

-doc(#{equiv => prependItem(This,Parent,Text, [])}).
-spec prependItem(This, Parent, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata().

prependItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),?is_chardata(Text) ->
  prependItem(This,Parent,Text, []).

-doc """
Appends an item as the first child of `parent`, return a new item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If `image` >
-1 and `selImage` is -1, the same image is used for both selected and unselected items.
""".
-spec prependItem(This, Parent, Text, [Option]) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata(),
	Option :: {'image', integer()}
		 | {'selectedImage', integer()}
		 | {'data', term()}.
prependItem(#wx_ref{type=ThisT}=This,Parent,Text, Options)
 when is_integer(Parent),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({image, _image} = Arg) -> Arg;
          ({selectedImage, _selectedImage} = Arg) -> Arg;
          ({data, _data} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Text_UC, Opts,?get_env(),?wxTreeCtrl_PrependItem),
  wxe_util:rec(?wxTreeCtrl_PrependItem).

-doc """
Scrolls the specified item into view.

Note that this method doesn't work while the window is frozen (See `wxWindow:freeze/1`), at least under MSW.

See: `ensureVisible/2`
""".
-spec scrollTo(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
scrollTo(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_ScrollTo).

-doc(#{equiv => selectItem(This,Item, [])}).
-spec selectItem(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

selectItem(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  selectItem(This,Item, []).

-doc """
Selects the given item.

In multiple selection controls, can be also used to deselect a currently selected item if
the value of `select` is false.

Notice that calling this method will generate `wxEVT_TREE_SEL_CHANGING` and `wxEVT_TREE_SEL_CHANGED`
events and that the change could be vetoed by the former event handler.
""".
-spec selectItem(This, Item, [Option]) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'select', boolean()}.
selectItem(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({select, _select} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_SelectItem).

-doc "Sets the indentation for the tree control.".
-spec setIndent(This, Indent) -> 'ok' when
	This::wxTreeCtrl(), Indent::integer().
setIndent(#wx_ref{type=ThisT}=This,Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Indent,?get_env(),?wxTreeCtrl_SetIndent).

-doc """
Sets the normal image list.

The image list assigned with this method will `not` be deleted by `m:wxTreeCtrl`'s
destructor, you must delete it yourself.

See: `assignImageList/2`
""".
-spec setImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_SetImageList).

-doc "Sets the colour of the item's background.".
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxTreeCtrl_SetItemBackgroundColour).

-doc(#{equiv => setItemBold(This,Item, [])}).
-spec setItemBold(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemBold(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemBold(This,Item, []).

-doc """
Makes item appear in bold font if `bold` parameter is true or resets it to the normal
state.

See: `isBold/2`
""".
-spec setItemBold(This, Item, [Option]) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'bold', boolean()}.
setItemBold(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({bold, _bold} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_SetItemBold).

-doc """
Sets the item client data.

Notice that the client data previously associated with the `item` (if any) is `not` freed
by this function and so calling this function multiple times for the same item will result
in memory leaks unless you delete the old item data pointer yourself.
""".
-spec setItemData(This, Item, Data) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Data::term().
setItemData(#wx_ref{type=ThisT}=This,Item,Data)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,Data,?get_env(),?wxTreeCtrl_SetItemData).

-doc(#{equiv => setItemDropHighlight(This,Item, [])}).
-spec setItemDropHighlight(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemDropHighlight(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemDropHighlight(This,Item, []).

-doc """
Gives the item the visual feedback for Drag'n'Drop actions, which is useful if something
is dragged from the outside onto the tree control (as opposed to a DnD operation within
the tree control, which already is implemented internally).
""".
-spec setItemDropHighlight(This, Item, [Option]) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'highlight', boolean()}.
setItemDropHighlight(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({highlight, _highlight} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_SetItemDropHighlight).

-doc """
Sets the item's font.

All items in the tree should have the same height to avoid text clipping, so the fonts
height should be the same for all of them, although font attributes may vary.

See: `setItemBold/3`
""".
-spec setItemFont(This, Item, Font) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Font::wxFont:wxFont().
setItemFont(#wx_ref{type=ThisT}=This,Item,#wx_ref{type=FontT}=Font)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Item,Font,?get_env(),?wxTreeCtrl_SetItemFont).

-doc(#{equiv => setItemHasChildren(This,Item, [])}).
-spec setItemHasChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemHasChildren(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemHasChildren(This,Item, []).

-doc """
Force appearance of the button next to the item.

This is useful to allow the user to expand the items which don't have any children now,
but instead adding them only when needed, thus minimizing memory usage and loading time.
""".
-spec setItemHasChildren(This, Item, [Option]) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(),
	Option :: {'has', boolean()}.
setItemHasChildren(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({has, _has} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxTreeCtrl_SetItemHasChildren).

-doc(#{equiv => setItemImage(This,Item,Image, [])}).
-spec setItemImage(This, Item, Image) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Image::integer().

setItemImage(This,Item,Image)
 when is_record(This, wx_ref),is_integer(Item),is_integer(Image) ->
  setItemImage(This,Item,Image, []).

-doc """
Sets the specified item's image.

See `getItemImage/3` for the description of the `which` parameter.
""".
%%  Which = ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
-spec setItemImage(This, Item, Image, [Option]) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Image::integer(),
	Option :: {'which', wx:wx_enum()}.
setItemImage(#wx_ref{type=ThisT}=This,Item,Image, Options)
 when is_integer(Item),is_integer(Image),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({which, _which} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item,Image, Opts,?get_env(),?wxTreeCtrl_SetItemImage).

-doc "Sets the item label.".
-spec setItemText(This, Item, Text) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Text::unicode:chardata().
setItemText(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxTreeCtrl_SetItemText).

-doc "Sets the colour of the item's text.".
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxTreeCtrl_SetItemTextColour).

-doc """
Sets the state image list (from which application-defined state images are taken).

Image list assigned with this method will `not` be deleted by `m:wxTreeCtrl`'s
destructor, you must delete it yourself.

See: `assignStateImageList/2`
""".
-spec setStateImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
setStateImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_SetStateImageList).

-doc """
Sets the mode flags associated with the display of the tree control.

The new mode takes effect immediately.

Note: Generic only; MSW ignores changes.
""".
-spec setWindowStyle(This, Styles) -> 'ok' when
	This::wxTreeCtrl(), Styles::integer().
setWindowStyle(#wx_ref{type=ThisT}=This,Styles)
 when is_integer(Styles) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Styles,?get_env(),?wxTreeCtrl_SetWindowStyle).

-doc """
Sorts the children of the given item using `OnCompareItems()` (not implemented in wx).

You should override that method to change the sort order (the default is ascending
case-sensitive alphabetical order).
""".
-spec sortChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
sortChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_SortChildren).

-doc "Toggles the given item between collapsed and expanded states.".
-spec toggle(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
toggle(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Toggle).

-doc """
Toggles the given item between selected and unselected states.

For multiselection controls only.
""".
-spec toggleItemSelection(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
toggleItemSelection(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_ToggleItemSelection).

-doc "Removes the selection from the currently selected item (if any).".
-spec unselect(This) -> 'ok' when
	This::wxTreeCtrl().
unselect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_Unselect).

-doc """
This function either behaves the same as `unselect/1` if the control doesn't have `wxTR\_MULTIPLE`
style, or removes the selection from all items if it does have this style.
""".
-spec unselectAll(This) -> 'ok' when
	This::wxTreeCtrl().
unselectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_UnselectAll).

-doc """
Unselects the given item.

This works in multiselection controls only.
""".
-spec unselectItem(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
unselectItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_UnselectItem).

-doc "Destroys the object".
-spec destroy(This::wxTreeCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTreeCtrl),
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
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
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
