%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html">wxTreeCtrl</a>.
%%
%% Note: The representation of treeItemId() have changed from the original class implementation to be an semi-opaque type,Equality between TreeItemId's can be tested and zero means that the TreeItem is invalid.

%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxTreeCtrl().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxTreeCtrl).
-include("wxe.hrl").
-export([addRoot/2,addRoot/3,appendItem/3,appendItem/4,assignImageList/2,assignStateImageList/2,
  collapse/2,collapseAndReset/2,create/2,create/3,delete/2,deleteAllItems/1,
  deleteChildren/2,destroy/1,ensureVisible/2,expand/2,getBoundingRect/3,
  getBoundingRect/4,getChildrenCount/2,getChildrenCount/3,getCount/1,
  getEditControl/1,getFirstChild/2,getFirstVisibleItem/1,getImageList/1,
  getIndent/1,getItemBackgroundColour/2,getItemData/2,getItemFont/2,
  getItemImage/2,getItemImage/3,getItemParent/2,getItemText/2,getItemTextColour/2,
  getLastChild/2,getNextChild/3,getNextSibling/2,getNextVisible/2,getPrevSibling/2,
  getPrevVisible/2,getRootItem/1,getSelection/1,getSelections/1,getStateImageList/1,
  hitTest/2,insertItem/4,insertItem/5,isBold/2,isExpanded/2,isSelected/2,
  isVisible/2,itemHasChildren/2,new/0,new/1,new/2,prependItem/3,prependItem/4,
  scrollTo/2,selectItem/2,selectItem/3,setImageList/2,setIndent/2,setItemBackgroundColour/3,
  setItemBold/2,setItemBold/3,setItemData/3,setItemDropHighlight/2,
  setItemDropHighlight/3,setItemFont/3,setItemHasChildren/2,setItemHasChildren/3,
  setItemImage/3,setItemImage/4,setItemText/3,setItemTextColour/3,setStateImageList/2,
  setWindowStyle/2,sortChildren/2,toggle/2,toggleItemSelection/2,unselect/1,
  unselectAll/1,unselectItem/2]).

%% inherited exports
-export([cacheBestSize/2,captureMouse/1,center/1,center/2,centerOnParent/1,
  centerOnParent/2,centre/1,centre/2,centreOnParent/1,centreOnParent/2,
  clearBackground/1,clientToScreen/2,clientToScreen/3,close/1,close/2,
  connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  enable/1,enable/2,findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCaret/1,
  getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getCursor/1,getDropTarget/1,getEventHandler/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,isTopLevel/1,
  layout/1,lineDown/1,lineUp/1,lower/1,makeModal/1,makeModal/2,move/2,
  move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,
  navigate/2,pageDown/1,pageUp/1,parent_class/1,popEventHandler/1,popEventHandler/2,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,
  setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setVirtualSize/2,setVirtualSize/3,
  setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxTreeCtrl()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlwxtreectrl">external documentation</a>.
new() ->
  wxe_util:construct(?wxTreeCtrl_new_0,
  <<>>).

%% @spec (Parent::wxWindow:wxWindow()) -> wxTreeCtrl()
%% @equiv new(Parent, [])
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @spec (Parent::wxWindow:wxWindow(), [Option]) -> wxTreeCtrl()
%% Option = {id, integer()} | {pos, {X::integer(),Y::integer()}} | {size, {W::integer(),H::integer()}} | {style, integer()} | {validator, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlwxtreectrl">external documentation</a>.
new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxTreeCtrl_new_2,
  <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Text::string()) -> integer()
%% @equiv addRoot(This,Text, [])
addRoot(This,Text)
 when is_record(This, wx_ref),is_list(Text) ->
  addRoot(This,Text, []).

%% @spec (This::wxTreeCtrl(), Text::string(), [Option]) -> integer()
%% Option = {image, integer()} | {selectedImage, integer()} | {data, term()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrladdroot">external documentation</a>.
addRoot(#wx_ref{type=ThisT,ref=ThisRef},Text, Options)
 when is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({image, Image}, Acc) -> [<<1:32/?UI,Image:32/?UI>>|Acc];
          ({selectedImage, SelectedImage}, Acc) -> [<<2:32/?UI,SelectedImage:32/?UI>>|Acc];
          ({data, Data}, Acc) ->   wxe_util:send_bin(term_to_binary(Data)),[<<3:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_AddRoot,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Text::string()) -> integer()
%% @equiv appendItem(This,Parent,Text, [])
appendItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),is_list(Text) ->
  appendItem(This,Parent,Text, []).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Text::string(), [Option]) -> integer()
%% Option = {image, integer()} | {selectedImage, integer()} | {data, term()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlappenditem">external documentation</a>.
appendItem(#wx_ref{type=ThisT,ref=ThisRef},Parent,Text, Options)
 when is_integer(Parent),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({image, Image}, Acc) -> [<<1:32/?UI,Image:32/?UI>>|Acc];
          ({selectedImage, SelectedImage}, Acc) -> [<<2:32/?UI,SelectedImage:32/?UI>>|Acc];
          ({data, Data}, Acc) ->   wxe_util:send_bin(term_to_binary(Data)),[<<3:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_AppendItem,
  <<ThisRef:32/?UI,0:32,Parent:64/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), ImageList::wxImageList:wxImageList()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlassignimagelist">external documentation</a>.
assignImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxTreeCtrl_AssignImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), ImageList::wxImageList:wxImageList()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlassignstateimagelist">external documentation</a>.
assignStateImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxTreeCtrl_AssignStateImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlcollapse">external documentation</a>.
collapse(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_Collapse,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlcollapseandreset">external documentation</a>.
collapseAndReset(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_CollapseAndReset,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Parent::wxWindow:wxWindow()) -> bool()
%% @equiv create(This,Parent, [])
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @spec (This::wxTreeCtrl(), Parent::wxWindow:wxWindow(), [Option]) -> bool()
%% Option = {id, integer()} | {pos, {X::integer(),Y::integer()}} | {size, {W::integer(),H::integer()}} | {style, integer()} | {validator, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrldelete">external documentation</a>.
delete(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_Delete,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrldeleteallitems">external documentation</a>.
deleteAllItems(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_DeleteAllItems,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrldeletechildren">external documentation</a>.
deleteChildren(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_DeleteChildren,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlensurevisible">external documentation</a>.
ensureVisible(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_EnsureVisible,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlexpand">external documentation</a>.
expand(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_Expand,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> bool()
%% @equiv getBoundingRect(This,Item,Rect, [])
getBoundingRect(This,Item,Rect={RectX,RectY,RectW,RectH})
 when is_record(This, wx_ref),is_integer(Item),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  getBoundingRect(This,Item,Rect, []).

%% @spec (This::wxTreeCtrl(), Item::integer(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}, [Option]) -> bool()
%% Option = {textOnly, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetboundingrect">external documentation</a>.
getBoundingRect(#wx_ref{type=ThisT,ref=ThisRef},Item,{RectX,RectY,RectW,RectH}, Options)
 when is_integer(Item),is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({textOnly, TextOnly}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(TextOnly)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_GetBoundingRect,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @equiv getChildrenCount(This,Item, [])
getChildrenCount(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getChildrenCount(This,Item, []).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> integer()
%% Option = {recursively, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetchildrencount">external documentation</a>.
getChildrenCount(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({recursively, Recursively}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Recursively)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_GetChildrenCount,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetcount">external documentation</a>.
getCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetCount,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> wxTextCtrl:wxTextCtrl()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgeteditcontrol">external documentation</a>.
getEditControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetEditControl,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> {integer(),Cookie::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetfirstchild">external documentation</a>.
getFirstChild(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetFirstChild,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Cookie::integer()) -> {integer(),Cookie::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetnextchild">external documentation</a>.
getNextChild(#wx_ref{type=ThisT,ref=ThisRef},Item,Cookie)
 when is_integer(Item),is_integer(Cookie) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetNextChild,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,Cookie:64/?UI>>).

%% @spec (This::wxTreeCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetfirstvisibleitem">external documentation</a>.
getFirstVisibleItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetFirstVisibleItem,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> wxImageList:wxImageList()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetimagelist">external documentation</a>.
getImageList(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetImageList,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetindent">external documentation</a>.
getIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitembackgroundcolour">external documentation</a>.
getItemBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemBackgroundColour,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> term()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemdata">external documentation</a>.
getItemData(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemData,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> wxFont:wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemfont">external documentation</a>.
getItemFont(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemFont,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemimage">external documentation</a>.
getItemImage(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemImage_1,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> integer()
%% Option = {which, WxTreeItemIcon}
%% WxTreeItemIcon = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemimage">external documentation</a>.
%%<br /> WxTreeItemIcon is one of ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
getItemImage(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({which, Which}, Acc) -> [<<1:32/?UI,Which:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_GetItemImage_2,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemtext">external documentation</a>.
getItemText(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemText,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemtextcolour">external documentation</a>.
getItemTextColour(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemTextColour,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetlastchild">external documentation</a>.
getLastChild(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetLastChild,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetnextsibling">external documentation</a>.
getNextSibling(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetNextSibling,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetnextvisible">external documentation</a>.
getNextVisible(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetNextVisible,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetitemparent">external documentation</a>.
getItemParent(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetItemParent,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetprevsibling">external documentation</a>.
getPrevSibling(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetPrevSibling,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetprevvisible">external documentation</a>.
getPrevVisible(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetPrevVisible,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetrootitem">external documentation</a>.
getRootItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetRootItem,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetselection">external documentation</a>.
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> {integer(),Val::[integer()]}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetselections">external documentation</a>.
getSelections(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetSelections,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> wxImageList:wxImageList()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlgetstateimagelist">external documentation</a>.
getStateImageList(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_GetStateImageList,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Point::{X::integer(),Y::integer()}) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlhittest">external documentation</a>.
hitTest(#wx_ref{type=ThisT,ref=ThisRef},{PointX,PointY})
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_HitTest,
  <<ThisRef:32/?UI,PointX:32/?UI,PointY:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Pos::integer(), Text::string()) -> integer()
%% @equiv insertItem(This,Parent,Pos,Text, [])
insertItem(This,Parent,Pos,Text)
 when is_record(This, wx_ref),is_integer(Parent),is_integer(Pos),is_list(Text) ->
  insertItem(This,Parent,Pos,Text, []).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Pos::integer(), Text::string(), [Option]) -> integer()
%% Option = {image, integer()} | {selImage, integer()} | {data, term()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlinsertitem">external documentation</a>.
insertItem(#wx_ref{type=ThisT,ref=ThisRef},Parent,Pos,Text, Options)
 when is_integer(Parent),is_integer(Pos),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({image, Image}, Acc) -> [<<1:32/?UI,Image:32/?UI>>|Acc];
          ({selImage, SelImage}, Acc) -> [<<2:32/?UI,SelImage:32/?UI>>|Acc];
          ({data, Data}, Acc) ->   wxe_util:send_bin(term_to_binary(Data)),[<<3:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_InsertItem,
  <<ThisRef:32/?UI,0:32,Parent:64/?UI,Pos:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlisbold">external documentation</a>.
isBold(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_IsBold,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlisexpanded">external documentation</a>.
isExpanded(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_IsExpanded,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlisselected">external documentation</a>.
isSelected(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_IsSelected,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlisvisible">external documentation</a>.
isVisible(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_IsVisible,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlitemhaschildren">external documentation</a>.
itemHasChildren(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:call(?wxTreeCtrl_ItemHasChildren,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Text::string()) -> integer()
%% @equiv prependItem(This,Parent,Text, [])
prependItem(This,Parent,Text)
 when is_record(This, wx_ref),is_integer(Parent),is_list(Text) ->
  prependItem(This,Parent,Text, []).

%% @spec (This::wxTreeCtrl(), Parent::integer(), Text::string(), [Option]) -> integer()
%% Option = {image, integer()} | {selectedImage, integer()} | {data, term()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlprependitem">external documentation</a>.
prependItem(#wx_ref{type=ThisT,ref=ThisRef},Parent,Text, Options)
 when is_integer(Parent),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({image, Image}, Acc) -> [<<1:32/?UI,Image:32/?UI>>|Acc];
          ({selectedImage, SelectedImage}, Acc) -> [<<2:32/?UI,SelectedImage:32/?UI>>|Acc];
          ({data, Data}, Acc) ->   wxe_util:send_bin(term_to_binary(Data)),[<<3:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTreeCtrl_PrependItem,
  <<ThisRef:32/?UI,0:32,Parent:64/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlscrollto">external documentation</a>.
scrollTo(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_ScrollTo,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlselectitem">external documentation</a>.
selectItem(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SelectItem_1,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> ok
%% Option = {select, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlselectitem">external documentation</a>.
selectItem(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({select, Select}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Select)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTreeCtrl_SelectItem_2,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Indent::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetindent">external documentation</a>.
setIndent(#wx_ref{type=ThisT,ref=ThisRef},Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SetIndent,
  <<ThisRef:32/?UI,Indent:32/?UI>>).

%% @spec (This::wxTreeCtrl(), ImageList::wxImageList:wxImageList()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetimagelist">external documentation</a>.
setImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxTreeCtrl_SetImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Col::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitembackgroundcolour">external documentation</a>.
setItemBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SetItemBackgroundColour,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @equiv setItemBold(This,Item, [])
setItemBold(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemBold(This,Item, []).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> ok
%% Option = {bold, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitembold">external documentation</a>.
setItemBold(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({bold, Bold}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Bold)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTreeCtrl_SetItemBold,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Data::term()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemdata">external documentation</a>.
setItemData(#wx_ref{type=ThisT,ref=ThisRef},Item,Data)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:send_bin(term_to_binary(Data)),
  wxe_util:cast(?wxTreeCtrl_SetItemData,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @equiv setItemDropHighlight(This,Item, [])
setItemDropHighlight(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemDropHighlight(This,Item, []).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> ok
%% Option = {highlight, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemdrophighlight">external documentation</a>.
setItemDropHighlight(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({highlight, Highlight}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Highlight)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTreeCtrl_SetItemDropHighlight,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemfont">external documentation</a>.
setItemFont(#wx_ref{type=ThisT,ref=ThisRef},Item,#wx_ref{type=FontT,ref=FontRef})
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxTreeCtrl_SetItemFont,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,FontRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @equiv setItemHasChildren(This,Item, [])
setItemHasChildren(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  setItemHasChildren(This,Item, []).

%% @spec (This::wxTreeCtrl(), Item::integer(), [Option]) -> ok
%% Option = {has, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemhaschildren">external documentation</a>.
setItemHasChildren(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({has, Has}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Has)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTreeCtrl_SetItemHasChildren,
  <<ThisRef:32/?UI,0:32,Item:64/?UI, BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Image::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemimage">external documentation</a>.
setItemImage(#wx_ref{type=ThisT,ref=ThisRef},Item,Image)
 when is_integer(Item),is_integer(Image) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SetItemImage_2,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,Image:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Image::integer(), [Option]) -> ok
%% Option = {which, WxTreeItemIcon}
%% WxTreeItemIcon = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemimage">external documentation</a>.
%%<br /> WxTreeItemIcon is one of ?wxTreeItemIcon_Normal | ?wxTreeItemIcon_Selected | ?wxTreeItemIcon_Expanded | ?wxTreeItemIcon_SelectedExpanded | ?wxTreeItemIcon_Max
setItemImage(#wx_ref{type=ThisT,ref=ThisRef},Item,Image, Options)
 when is_integer(Item),is_integer(Image),is_list(Options) ->
  ?CLASS(ThisT,wxTreeCtrl),
  MOpts = fun({which, Which}, Acc) -> [<<1:32/?UI,Which:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxTreeCtrl_SetItemImage_3,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,Image:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemtext">external documentation</a>.
setItemText(#wx_ref{type=ThisT,ref=ThisRef},Item,Text)
 when is_integer(Item),is_list(Text) ->
  ?CLASS(ThisT,wxTreeCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxTreeCtrl_SetItemText,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxTreeCtrl(), Item::integer(), Col::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetitemtextcolour">external documentation</a>.
setItemTextColour(#wx_ref{type=ThisT,ref=ThisRef},Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SetItemTextColour,
  <<ThisRef:32/?UI,0:32,Item:64/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @spec (This::wxTreeCtrl(), ImageList::wxImageList:wxImageList()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetstateimagelist">external documentation</a>.
setStateImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxTreeCtrl_SetStateImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Styles::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsetwindowstyle">external documentation</a>.
setWindowStyle(#wx_ref{type=ThisT,ref=ThisRef},Styles)
 when is_integer(Styles) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SetWindowStyle,
  <<ThisRef:32/?UI,Styles:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlsortchildren">external documentation</a>.
sortChildren(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_SortChildren,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrltoggle">external documentation</a>.
toggle(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_Toggle,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrltoggleitemselection">external documentation</a>.
toggleItemSelection(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_ToggleItemSelection,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlunselect">external documentation</a>.
unselect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_Unselect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlunselectall">external documentation</a>.
unselectAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_UnselectAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxTreeCtrl(), Item::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtreectrl.html#wxtreectrlunselectitem">external documentation</a>.
unselectItem(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxTreeCtrl),
  wxe_util:cast(?wxTreeCtrl_UnselectItem,
  <<ThisRef:32/?UI,0:32,Item:64/?UI>>).

%% @spec (This::wxTreeCtrl()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTreeCtrl),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
%% @hidden
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
validate(This) -> wxWindow:validate(This).
%% @hidden
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
update(This) -> wxWindow:update(This).
%% @hidden
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
thaw(This) -> wxWindow:thaw(This).
%% @hidden
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
show(This) -> wxWindow:show(This).
%% @hidden
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
setVirtualSizeHints(This,MinW,MinH, Options) -> wxWindow:setVirtualSizeHints(This,MinW,MinH, Options).
%% @hidden
setVirtualSizeHints(This,MinW,MinH) -> wxWindow:setVirtualSizeHints(This,MinW,MinH).
%% @hidden
setVirtualSizeHints(This,MinSize) -> wxWindow:setVirtualSizeHints(This,MinSize).
%% @hidden
setVirtualSize(This,X,Y) -> wxWindow:setVirtualSize(This,X,Y).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,Tip) -> wxWindow:setToolTip(This,Tip).
%% @hidden
setThemeEnabled(This,EnableTheme) -> wxWindow:setThemeEnabled(This,EnableTheme).
%% @hidden
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
setScrollPos(This,Orient,Pos, Options) -> wxWindow:setScrollPos(This,Orient,Pos, Options).
%% @hidden
setScrollPos(This,Orient,Pos) -> wxWindow:setScrollPos(This,Orient,Pos).
%% @hidden
setScrollbar(This,Orient,Pos,ThumbVisible,Range, Options) -> wxWindow:setScrollbar(This,Orient,Pos,ThumbVisible,Range, Options).
%% @hidden
setScrollbar(This,Orient,Pos,ThumbVisible,Range) -> wxWindow:setScrollbar(This,Orient,Pos,ThumbVisible,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,Text) -> wxWindow:setHelpText(This,Text).
%% @hidden
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
setDropTarget(This,DropTarget) -> wxWindow:setDropTarget(This,DropTarget).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,MinSize) -> wxWindow:setMinSize(This,MinSize).
%% @hidden
setMaxSize(This,MaxSize) -> wxWindow:setMaxSize(This,MaxSize).
%% @hidden
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
refresh(This) -> wxWindow:refresh(This).
%% @hidden
raise(This) -> wxWindow:raise(This).
%% @hidden
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
popEventHandler(This, Options) -> wxWindow:popEventHandler(This, Options).
%% @hidden
popEventHandler(This) -> wxWindow:popEventHandler(This).
%% @hidden
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
navigate(This) -> wxWindow:navigate(This).
%% @hidden
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
makeModal(This, Options) -> wxWindow:makeModal(This, Options).
%% @hidden
makeModal(This) -> wxWindow:makeModal(This).
%% @hidden
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
%% @hidden
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
isShown(This) -> wxWindow:isShown(This).
%% @hidden
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
initDialog(This) -> wxWindow:initDialog(This).
%% @hidden
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
hide(This) -> wxWindow:hide(This).
%% @hidden
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orient) -> wxWindow:getScrollThumb(This,Orient).
%% @hidden
getScrollRange(This,Orient) -> wxWindow:getScrollRange(This,Orient).
%% @hidden
getScrollPos(This,Orient) -> wxWindow:getScrollPos(This,Orient).
%% @hidden
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
getRect(This) -> wxWindow:getRect(This).
%% @hidden
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
getParent(This) -> wxWindow:getParent(This).
%% @hidden
getName(This) -> wxWindow:getName(This).
%% @hidden
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
getId(This) -> wxWindow:getId(This).
%% @hidden
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
getFont(This) -> wxWindow:getFont(This).
%% @hidden
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
getEventHandler(This) -> wxWindow:getEventHandler(This).
%% @hidden
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
freeze(This) -> wxWindow:freeze(This).
%% @hidden
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
fit(This) -> wxWindow:fit(This).
%% @hidden
findWindow(This,Winid) -> wxWindow:findWindow(This,Winid).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
%% @hidden
disable(This) -> wxWindow:disable(This).
%% @hidden
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
close(This) -> wxWindow:close(This).
%% @hidden
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
%% @hidden
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
center(This) -> wxWindow:center(This).
%% @hidden
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
