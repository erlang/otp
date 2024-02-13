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

%%
%% Note: The representation of treeItemId() have changed from the original class implementation to be an semi-opaque type,Equality between TreeItemId's can be tested and zero means that the TreeItem is invalid.

%%
-module(wxTreeCtrl).
-moduledoc """
Functions for wxTreeCtrl class

A tree control presents information as a hierarchy, with items that may be
expanded to show further items. Items in a tree control are referenced by
`wxTreeItemId` (not implemented in wx) handles, which may be tested for validity
by calling `wxTreeItemId::IsOk()` (not implemented in wx).

A similar control with a fully native implementation for GTK+ and macOS as well
is `wxDataViewTreeCtrl` (not implemented in wx).

To intercept events from a tree control, use the event table macros described in
`m:wxTreeEvent`.

Styles

This class supports the following styles:

See also overview_windowstyles.

`Win32` `notes:`

`m:wxTreeCtrl` class uses the standard common treeview control under Win32
implemented in the system library comctl32.dll. Some versions of this library
are known to have bugs with handling the tree control colours: the usual symptom
is that the expanded items leave black (or otherwise incorrectly coloured)
background behind them, especially for the controls using non-default background
colour. The recommended solution is to upgrade the comctl32.dll to a newer
version: see
[http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2](http://www.microsoft.com/downloads/details.aspx?familyid=cb2cf3a2-8025-4e8f-8511-9b476a8d35d2)

See: `wxDataViewTreeCtrl` (not implemented in wx), `m:wxTreeEvent`,
`wxTreeItemData` (not implemented in wx),
[Overview treectrl](https://docs.wxwidgets.org/3.1/overview_treectrl.html#overview_treectrl),
`m:wxListBox`, `m:wxListCtrl`, `m:wxImageList`

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxTreeCtrl](https://docs.wxwidgets.org/3.1/classwx_tree_ctrl.html)

## Events

Event types emitted from this class:
[`command_tree_begin_drag`](`m:wxTreeEvent`),
[`command_tree_begin_rdrag`](`m:wxTreeEvent`),
[`command_tree_end_drag`](`m:wxTreeEvent`),
[`command_tree_begin_label_edit`](`m:wxTreeEvent`),
[`command_tree_end_label_edit`](`m:wxTreeEvent`),
[`command_tree_delete_item`](`m:wxTreeEvent`),
[`command_tree_get_info`](`m:wxTreeEvent`),
[`command_tree_set_info`](`m:wxTreeEvent`),
[`command_tree_item_activated`](`m:wxTreeEvent`),
[`command_tree_item_collapsed`](`m:wxTreeEvent`),
[`command_tree_item_collapsing`](`m:wxTreeEvent`),
[`command_tree_item_expanded`](`m:wxTreeEvent`),
[`command_tree_item_expanding`](`m:wxTreeEvent`),
[`command_tree_item_right_click`](`m:wxTreeEvent`),
[`command_tree_item_middle_click`](`m:wxTreeEvent`),
[`command_tree_sel_changed`](`m:wxTreeEvent`),
[`command_tree_sel_changing`](`m:wxTreeEvent`),
[`command_tree_key_down`](`m:wxTreeEvent`),
[`command_tree_item_gettooltip`](`m:wxTreeEvent`),
[`command_tree_item_menu`](`m:wxTreeEvent`),
[`command_tree_state_image_click`](`m:wxTreeEvent`)
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
%% @hidden
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlwxtreectrl">external documentation</a>.
-doc "Default Constructor.".
-spec new() -> wxTreeCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTreeCtrl_new_0),
  wxe_util:rec(?wxTreeCtrl_new_0).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxTreeCtrl() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlwxtreectrl">external documentation</a>.
-doc """
Constructor, creating and showing a tree control.

See: `create/3`, `wxValidator` (not implemented in wx)
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

%% @equiv addRoot(This,Text, [])
-spec addRoot(This, Text) -> integer() when
	This::wxTreeCtrl(), Text::unicode:chardata().

addRoot(This,Text)
 when is_record(This, wx_ref),?is_chardata(Text) ->
  addRoot(This,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrladdroot">external documentation</a>.
-doc """
Adds the root node to the tree, returning the new item.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If
`image` > -1 and `selImage` is -1, the same image is used for both selected and
unselected items.
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

%% @equiv appendItem(This,Parent,Text, [])
-spec appendItem(This, Parent, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata().

appendItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),?is_chardata(Text) ->
  appendItem(This,Parent,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlappenditem">external documentation</a>.
-doc """
Appends an item to the end of the branch identified by `parent`, return a new
item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If
`image` > -1 and `selImage` is -1, the same image is used for both selected and
unselected items.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlassignimagelist">external documentation</a>.
-doc """
Sets the normal image list.

The image list assigned with this method will be automatically deleted by
`m:wxTreeCtrl` as appropriate (i.e. it takes ownership of the list).

See: `setImageList/2`
""".
-spec assignImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_AssignImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlassignstateimagelist">external documentation</a>.
-doc """
Sets the state image list.

Image list assigned with this method will be automatically deleted by
`m:wxTreeCtrl` as appropriate (i.e. it takes ownership of the list).

See: `setStateImageList/2`
""".
-spec assignStateImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
assignStateImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_AssignStateImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlcollapse">external documentation</a>.
-doc "Collapses the given item.".
-spec collapse(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
collapse(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Collapse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlcollapseandreset">external documentation</a>.
-doc "Collapses the given item and removes all children.".
-spec collapseAndReset(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
collapseAndReset(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_CollapseAndReset).

%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
	This::wxTreeCtrl(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlcreate">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrldelete">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrldeleteallitems">external documentation</a>.
-doc """
Deletes all items in the control.

This function generates `wxEVT_TREE_DELETE_ITEM` events for each item being
deleted, including the root one if it is shown, i.e. unless wxTR_HIDE_ROOT style
is used.
""".
-spec deleteAllItems(This) -> 'ok' when
	This::wxTreeCtrl().
deleteAllItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_DeleteAllItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrldeletechildren">external documentation</a>.
-doc """
Deletes all children of the given item (but not the item itself).

A `wxEVT_TREE_DELETE_ITEM` event will be generated for every item being deleted.

If you have called `setItemHasChildren/3`, you may need to call it again since
`deleteChildren/2` does not automatically clear the setting.
""".
-spec deleteChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
deleteChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_DeleteChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrleditlabel">external documentation</a>.
-doc """
Starts editing the label of the given `item`.

This function generates a `EVT_TREE_BEGIN_LABEL_EDIT` event which can be vetoed
so that no text control will appear for in-place editing.

If the user changed the label (i.e. s/he does not press ESC or leave the text
control without changes, a `EVT_TREE_END_LABEL_EDIT` event will be sent which
can be vetoed as well.

See: `EndEditLabel()` (not implemented in wx), `m:wxTreeEvent`
""".
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxTreeCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_EditLabel),
  wxe_util:rec(?wxTreeCtrl_EditLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlensurevisible">external documentation</a>.
-doc """
Scrolls and/or expands items to ensure that the given item is visible.

This method can be used, and will work, even while the window is frozen (see
`wxWindow:freeze/1`).
""".
-spec ensureVisible(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
ensureVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_EnsureVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlexpand">external documentation</a>.
-doc "Expands the given item.".
-spec expand(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
expand(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Expand).

%% @equiv getBoundingRect(This,Item, [])
-spec getBoundingRect(This, Item) -> Result when
	Result ::{Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxTreeCtrl(), Item::integer().

getBoundingRect(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getBoundingRect(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetboundingrect">external documentation</a>.
-doc """
Retrieves the rectangle bounding the `item`.

If `textOnly` is true, only the rectangle around the item's label will be
returned, otherwise the item's image is also taken into account.

The return value is true if the rectangle was successfully retrieved or false if
it was not (in this case `rect` is not changed) - for example, if the item is
currently invisible.

Notice that the rectangle coordinates are logical, not physical ones. So, for
example, the x coordinate may be negative if the tree has a horizontal scrollbar
and its position is not 0.
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

%% @equiv getChildrenCount(This,Item, [])
-spec getChildrenCount(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().

getChildrenCount(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getChildrenCount(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetchildrencount">external documentation</a>.
-doc """
Returns the number of items in the branch.

If `recursively` is true, returns the total number of descendants, otherwise
only one level of children is counted.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetcount">external documentation</a>.
-doc "Returns the number of items in the control.".
-spec getCount(This) -> integer() when
	This::wxTreeCtrl().
getCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetCount),
  wxe_util:rec(?wxTreeCtrl_GetCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgeteditcontrol">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetfirstchild">external documentation</a>.
-doc """
Returns the first child; call `getNextChild/3` for the next child.

For this enumeration function you must pass in a 'cookie' parameter which is
opaque for the application but is necessary for the library to make these
functions reentrant (i.e. allow more than one enumeration on one and the same
object simultaneously). The cookie passed to `getFirstChild/2` and
`getNextChild/3` should be the same variable.

Returns an invalid tree item (i.e. `wxTreeItemId::IsOk()` (not implemented in
wx) returns false) if there are no further children.

See: `getNextChild/3`, `getNextSibling/2`
""".
-spec getFirstChild(This, Item) -> Result when
	Result ::{Res ::integer(), Cookie::integer()},
	This::wxTreeCtrl(), Item::integer().
getFirstChild(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetFirstChild),
  wxe_util:rec(?wxTreeCtrl_GetFirstChild).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetnextchild">external documentation</a>.
-doc """
Returns the next child; call `getFirstChild/2` for the first child.

For this enumeration function you must pass in a 'cookie' parameter which is
opaque for the application but is necessary for the library to make these
functions reentrant (i.e. allow more than one enumeration on one and the same
object simultaneously). The cookie passed to `getFirstChild/2` and
`getNextChild/3` should be the same.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetfirstvisibleitem">external documentation</a>.
-doc "Returns the first visible item.".
-spec getFirstVisibleItem(This) -> integer() when
	This::wxTreeCtrl().
getFirstVisibleItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetFirstVisibleItem),
  wxe_util:rec(?wxTreeCtrl_GetFirstVisibleItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetimagelist">external documentation</a>.
-doc "Returns the normal image list.".
-spec getImageList(This) -> wxImageList:wxImageList() when
	This::wxTreeCtrl().
getImageList(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetImageList),
  wxe_util:rec(?wxTreeCtrl_GetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetindent">external documentation</a>.
-doc "Returns the current tree control indentation.".
-spec getIndent(This) -> integer() when
	This::wxTreeCtrl().
getIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetIndent),
  wxe_util:rec(?wxTreeCtrl_GetIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitembackgroundcolour">external documentation</a>.
-doc "Returns the background colour of the item.".
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxTreeCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemBackgroundColour),
  wxe_util:rec(?wxTreeCtrl_GetItemBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemdata">external documentation</a>.
-doc """
Returns the tree item data associated with the item.

See: `wxTreeItemData` (not implemented in wx)
""".
-spec getItemData(This, Item) -> term() when
	This::wxTreeCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemData),
  wxe_util:rec(?wxTreeCtrl_GetItemData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemfont">external documentation</a>.
-doc """
Returns the font of the item label.

If the font hadn't been explicitly set for the specified `item` with
`setItemFont/3`, returns an invalid ?wxNullFont font. `wxWindow:getFont/1` can
be used to retrieve the global tree control font used for the items without any
specific font.
""".
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxTreeCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemFont),
  wxe_util:rec(?wxTreeCtrl_GetItemFont).

%% @equiv getItemImage(This,Item, [])
-spec getItemImage(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().

getItemImage(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemImage(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemimage">external documentation</a>.
%%<br /> Which = ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
-doc """
Gets the specified item image.

The value of `which` may be:
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemtext">external documentation</a>.
-doc "Returns the item label.".
-spec getItemText(This, Item) -> unicode:charlist() when
	This::wxTreeCtrl(), Item::integer().
getItemText(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemText),
  wxe_util:rec(?wxTreeCtrl_GetItemText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemtextcolour">external documentation</a>.
-doc "Returns the colour of the item label.".
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when
	This::wxTreeCtrl(), Item::integer().
getItemTextColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemTextColour),
  wxe_util:rec(?wxTreeCtrl_GetItemTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetlastchild">external documentation</a>.
-doc """
Returns the last child of the item (or an invalid tree item if this item has no
children).

See: `getFirstChild/2`, `getNextSibling/2`, `getLastChild/2`
""".
-spec getLastChild(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getLastChild(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetLastChild),
  wxe_util:rec(?wxTreeCtrl_GetLastChild).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetnextsibling">external documentation</a>.
-doc """
Returns the next sibling of the specified item; call `getPrevSibling/2` for the
previous sibling.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetnextvisible">external documentation</a>.
-doc """
Returns the next visible item or an invalid item if this item is the last
visible one.

Note: The `item` itself must be visible.
""".
-spec getNextVisible(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getNextVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetNextVisible),
  wxe_util:rec(?wxTreeCtrl_GetNextVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetitemparent">external documentation</a>.
-doc "Returns the item's parent.".
-spec getItemParent(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getItemParent(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetItemParent),
  wxe_util:rec(?wxTreeCtrl_GetItemParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetprevsibling">external documentation</a>.
-doc """
Returns the previous sibling of the specified item; call `getNextSibling/2` for
the next sibling.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetprevvisible">external documentation</a>.
-doc """
Returns the previous visible item or an invalid item if this item is the first
visible one.

Note: The `item` itself must be visible.
""".
-spec getPrevVisible(This, Item) -> integer() when
	This::wxTreeCtrl(), Item::integer().
getPrevVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_GetPrevVisible),
  wxe_util:rec(?wxTreeCtrl_GetPrevVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetrootitem">external documentation</a>.
-doc "Returns the root item for the tree control.".
-spec getRootItem(This) -> integer() when
	This::wxTreeCtrl().
getRootItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetRootItem),
  wxe_util:rec(?wxTreeCtrl_GetRootItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetselection">external documentation</a>.
-doc """
Returns the selection, or an invalid item if there is no selection.

This function only works with the controls without `wxTR_MULTIPLE` style, use
`getSelections/1` for the controls which do have this style or, if a single item
is wanted, use `GetFocusedItem()` (not implemented in wx).
""".
-spec getSelection(This) -> integer() when
	This::wxTreeCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetSelection),
  wxe_util:rec(?wxTreeCtrl_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetselections">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlgetstateimagelist">external documentation</a>.
-doc """
Returns the state image list (from which application-defined state images are
taken).
""".
-spec getStateImageList(This) -> wxImageList:wxImageList() when
	This::wxTreeCtrl().
getStateImageList(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_GetStateImageList),
  wxe_util:rec(?wxTreeCtrl_GetStateImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlhittest">external documentation</a>.
-doc """
Calculates which (if any) item is under the given `point`, returning the tree
item id at this point plus extra information `flags`.

`flags` is a bitlist of the following:
""".
-spec hitTest(This, Point) -> Result when
	Result ::{Res ::integer(), Flags::integer()},
	This::wxTreeCtrl(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxTreeCtrl_HitTest),
  wxe_util:rec(?wxTreeCtrl_HitTest).

%% @equiv insertItem(This,Parent,Previous,Text, [])
-spec insertItem(This, Parent, Previous, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Previous::integer(), Text::unicode:chardata().

insertItem(This,Parent,Previous,Text)
 when is_record(This, wx_ref),is_integer(Parent),is_integer(Previous),?is_chardata(Text) ->
  insertItem(This,Parent,Previous,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlinsertitem">external documentation</a>.
-doc """
Inserts an item after a given one (`previous`).

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If
`image` > -1 and `selImage` is -1, the same image is used for both selected and
unselected items.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlisbold">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlisexpanded">external documentation</a>.
-doc "Returns true if the item is expanded (only makes sense if it has children).".
-spec isExpanded(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isExpanded(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsExpanded),
  wxe_util:rec(?wxTreeCtrl_IsExpanded).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlisselected">external documentation</a>.
-doc "Returns true if the item is selected.".
-spec isSelected(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isSelected(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsSelected),
  wxe_util:rec(?wxTreeCtrl_IsSelected).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlisvisible">external documentation</a>.
-doc "Returns true if the item is visible on the screen.".
-spec isVisible(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
isVisible(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_IsVisible),
  wxe_util:rec(?wxTreeCtrl_IsVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlitemhaschildren">external documentation</a>.
-doc "Returns true if the item has children.".
-spec itemHasChildren(This, Item) -> boolean() when
	This::wxTreeCtrl(), Item::integer().
itemHasChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_ItemHasChildren),
  wxe_util:rec(?wxTreeCtrl_ItemHasChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlistreeitemidok">external documentation</a>.
-doc "Returns true if the item is valid.".
-spec isTreeItemIdOk(Item) -> boolean() when
	Item::integer().
isTreeItemIdOk(Item)
 when is_integer(Item) ->
  wxe_util:queue_cmd(Item,?get_env(),?wxTreeCtrl_IsTreeItemIdOk),
  wxe_util:rec(?wxTreeCtrl_IsTreeItemIdOk).

%% @equiv prependItem(This,Parent,Text, [])
-spec prependItem(This, Parent, Text) -> integer() when
	This::wxTreeCtrl(), Parent::integer(), Text::unicode:chardata().

prependItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),?is_chardata(Text) ->
  prependItem(This,Parent,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlprependitem">external documentation</a>.
-doc """
Appends an item as the first child of `parent`, return a new item id.

The `image` and `selImage` parameters are an index within the normal image list
specifying the image to use for unselected and selected items, respectively. If
`image` > -1 and `selImage` is -1, the same image is used for both selected and
unselected items.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlscrollto">external documentation</a>.
-doc """
Scrolls the specified item into view.

Note that this method doesn't work while the window is frozen (See
`wxWindow:freeze/1`), at least under MSW.

See: `ensureVisible/2`
""".
-spec scrollTo(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
scrollTo(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_ScrollTo).

%% @equiv selectItem(This,Item, [])
-spec selectItem(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

selectItem(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  selectItem(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlselectitem">external documentation</a>.
-doc """
Selects the given item.

In multiple selection controls, can be also used to deselect a currently
selected item if the value of `select` is false.

Notice that calling this method will generate `wxEVT_TREE_SEL_CHANGING` and
`wxEVT_TREE_SEL_CHANGED` events and that the change could be vetoed by the
former event handler.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetindent">external documentation</a>.
-doc "Sets the indentation for the tree control.".
-spec setIndent(This, Indent) -> 'ok' when
	This::wxTreeCtrl(), Indent::integer().
setIndent(#wx_ref{type=ThisT}=This,Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Indent,?get_env(),?wxTreeCtrl_SetIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetimagelist">external documentation</a>.
-doc """
Sets the normal image list.

The image list assigned with this method will `not` be deleted by
`m:wxTreeCtrl`'s destructor, you must delete it yourself.

See: `assignImageList/2`
""".
-spec setImageList(This, ImageList) -> 'ok' when
	This::wxTreeCtrl(), ImageList::wxImageList:wxImageList().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxTreeCtrl_SetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitembackgroundcolour">external documentation</a>.
-doc "Sets the colour of the item's background.".
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxTreeCtrl_SetItemBackgroundColour).

%% @equiv setItemBold(This,Item, [])
-spec setItemBold(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemBold(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemBold(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitembold">external documentation</a>.
-doc """
Makes item appear in bold font if `bold` parameter is true or resets it to the
normal state.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemdata">external documentation</a>.
-doc """
Sets the item client data.

Notice that the client data previously associated with the `item` (if any) is
`not` freed by this function and so calling this function multiple times for the
same item will result in memory leaks unless you delete the old item data
pointer yourself.
""".
-spec setItemData(This, Item, Data) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Data::term().
setItemData(#wx_ref{type=ThisT}=This,Item,Data)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,Data,?get_env(),?wxTreeCtrl_SetItemData).

%% @equiv setItemDropHighlight(This,Item, [])
-spec setItemDropHighlight(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemDropHighlight(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemDropHighlight(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemdrophighlight">external documentation</a>.
-doc """
Gives the item the visual feedback for Drag'n'Drop actions, which is useful if
something is dragged from the outside onto the tree control (as opposed to a DnD
operation within the tree control, which already is implemented internally).
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemfont">external documentation</a>.
-doc """
Sets the item's font.

All items in the tree should have the same height to avoid text clipping, so the
fonts height should be the same for all of them, although font attributes may
vary.

See: `setItemBold/3`
""".
-spec setItemFont(This, Item, Font) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Font::wxFont:wxFont().
setItemFont(#wx_ref{type=ThisT}=This,Item,#wx_ref{type=FontT}=Font)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Item,Font,?get_env(),?wxTreeCtrl_SetItemFont).

%% @equiv setItemHasChildren(This,Item, [])
-spec setItemHasChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().

setItemHasChildren(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemHasChildren(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemhaschildren">external documentation</a>.
-doc """
Force appearance of the button next to the item.

This is useful to allow the user to expand the items which don't have any
children now, but instead adding them only when needed, thus minimizing memory
usage and loading time.
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

%% @equiv setItemImage(This,Item,Image, [])
-spec setItemImage(This, Item, Image) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Image::integer().

setItemImage(This,Item,Image)
 when is_record(This, wx_ref),is_integer(Item),is_integer(Image) ->
  setItemImage(This,Item,Image, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemimage">external documentation</a>.
%%<br /> Which = ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
-doc """
Sets the specified item's image.

See `getItemImage/3` for the description of the `which` parameter.
""".
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemtext">external documentation</a>.
-doc "Sets the item label.".
-spec setItemText(This, Item, Text) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Text::unicode:chardata().
setItemText(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxTreeCtrl_SetItemText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetitemtextcolour">external documentation</a>.
-doc "Sets the colour of the item's text.".
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxTreeCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),?is_colordata(Col) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxTreeCtrl_SetItemTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetstateimagelist">external documentation</a>.
-doc """
Sets the state image list (from which application-defined state images are
taken).

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsetwindowstyle">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlsortchildren">external documentation</a>.
-doc """
Sorts the children of the given item using `OnCompareItems()` (not implemented
in wx).

You should override that method to change the sort order (the default is
ascending case-sensitive alphabetical order).

See: `wxTreeItemData` (not implemented in wx), `OnCompareItems()` (not
implemented in wx)
""".
-spec sortChildren(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
sortChildren(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_SortChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrltoggle">external documentation</a>.
-doc "Toggles the given item between collapsed and expanded states.".
-spec toggle(This, Item) -> 'ok' when
	This::wxTreeCtrl(), Item::integer().
toggle(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxTreeCtrl_Toggle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrltoggleitemselection">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlunselect">external documentation</a>.
-doc "Removes the selection from the currently selected item (if any).".
-spec unselect(This) -> 'ok' when
	This::wxTreeCtrl().
unselect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_Unselect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlunselectall">external documentation</a>.
-doc """
This function either behaves the same as `unselect/1` if the control doesn't
have `wxTR_MULTIPLE` style, or removes the selection from all items if it does
have this style.
""".
-spec unselectAll(This) -> 'ok' when
	This::wxTreeCtrl().
unselectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTreeCtrl_UnselectAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtreectrl.html#wxtreectrlunselectitem">external documentation</a>.
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

%% @doc Destroys this object, do not use object again
-doc "Destructor, destroying the tree control.".
-spec destroy(This::wxTreeCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTreeCtrl),
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
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
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
