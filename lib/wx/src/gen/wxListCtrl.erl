%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html">wxListCtrl</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxListCtrl().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxListCtrl).
-include("wxe.hrl").
-export([ create/2, create/3 , new/0, new/1, new/2 , sortItems/2 ,arrange/1,
  arrange/2,assignImageList/3,clearAll/1,deleteAllItems/1,deleteColumn/2,
  deleteItem/2,destroy/1,editLabel/2,ensureVisible/2,findItem/3,findItem/4,
  getColumn/3,getColumnCount/1,getColumnWidth/2,getCountPerPage/1,getEditControl/1,
  getImageList/2,getItem/2,getItemBackgroundColour/2,getItemCount/1,
  getItemData/2,getItemFont/2,getItemPosition/2,getItemRect/2,getItemRect/3,
  getItemSpacing/1,getItemState/3,getItemText/2,getItemTextColour/2,
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
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,
  isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,makeModal/1,makeModal/2,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popEventHandler/1,
  popEventHandler/2,popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,
  refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,removeChild/2,
  reparent/2,screenToClient/1,screenToClient/2,scrollLines/2,scrollPages/2,
  scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,setAutoLayout/2,
  setBackgroundStyle/2,setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,
  setCursor/2,setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,
  setFocusFromKbd/1,setFont/2,setForegroundColour/2,setHelpText/2,setId/2,
  setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,setSize/3,setSize/5,
  setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,
  setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,setToolTip/2,
  setTransparent/2,setVirtualSize/2,setVirtualSize/3,setVirtualSizeHints/2,
  setVirtualSizeHints/3,setVirtualSizeHints/4,setWindowStyle/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-export_type([wxListCtrl/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxListCtrl() :: wx:wx_object().

%% @spec () -> wxListCtrl()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.
new() ->
    wxe_util:construct(?wxListCtrl_new_0, <<>>).

%% @spec (Parent::wxWindow:wxWindow()) -> wxListCtrl()
%% @equiv new(Parent, [])
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @spec (Parent::wxWindow:wxWindow(), [Option]) -> wxListCtrl()
%% Option = {winid, integer()} |
%%          {pos, {X::integer(),Y::integer()}} |
%%          {size, {W::integer(),H::integer()}} |
%%          {style, integer()} |
%%          {validator, wx:wx()} |
%%          {onGetItemText, OnGetItemText} |
%%          {onGetItemAttr, OnGetItemAttr} |
%%          {onGetItemColumnImage, OnGetItemColumnImage}
%%
%% OnGetItemText = (This, Item, Column) -> wxString()
%% OnGetItemAttr = (This, Item) -> wxListItemAttr()
%% OnGetItemColumnImage = (This, Item, Column) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlwxlistctrl">external documentation</a>.

new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
  when is_list(Options)->
    ?CLASS(ParentT,wxWindow),
    MOpts = fun({winid, Winid}, Acc) -> [<<1:32/?UI,Winid:32/?UI>>|Acc];
	       ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
	       ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
	       ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
	       ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->
		    ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
	       ({onGetItemText, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item,Col]) -> unicode:characters_to_binary([F(This,Item,Col),0]) end,
		    [<<6:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onGetItemAttr, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item]) ->
				  #wx_ref{type=wxListItemAttr,ref=ThisRef} = F(This,Item),
				  <<ThisRef:32/?UI>>
			  end,
		    [<<7:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onGetItemColumnImage, F}, Acc) when is_function(F) ->
		    Fun = fun([This,Item, Col]) -> <<(F(This,Item,Col)):32/?I>> end,
		    [<<8:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
    BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
    wxe_util:construct(?wxListCtrl_new_2, <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv arrange(This, [])
-spec arrange(This) -> boolean() when
	This::wxListCtrl().

arrange(This)
 when is_record(This, wx_ref) ->
  arrange(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlarrange">external documentation</a>.
-spec arrange(This, [Option]) -> boolean() when
	This::wxListCtrl(),
	Option :: {'flag', integer()}.
arrange(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({flag, Flag}, Acc) -> [<<1:32/?UI,Flag:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_Arrange,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlassignimagelist">external documentation</a>.
-spec assignImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
assignImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef},Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxListCtrl_AssignImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI,Which:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlclearall">external documentation</a>.
-spec clearAll(This) -> 'ok' when
	This::wxListCtrl().
clearAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_ClearAll,
  <<ThisRef:32/?UI>>).


%% @spec (This::wxListCtrl(), Parent::wxWindow:wxWindow()) -> bool()
%% @equiv create(This,Parent, [])
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @spec (This::wxListCtrl(), Parent::wxWindow:wxWindow(), [Option]) -> bool()
%% Option = {winid, integer()} |
%%          {pos, {X::integer(),Y::integer()}} |
%%          {size, {W::integer(),H::integer()}} |
%%          {style, integer()} |
%%          {validator, wx:wx()} |
%%          {onGetItemText, OnGetItemText} |
%%          {onGetItemAttr, OnGetItemAttr} |
%%          {onGetItemColumnImage, OnGetItemColumnImage}
%%
%% OnGetItemText = (This, Item, Column) -> wxString()
%% OnGetItemAttr = (This, Item) -> wxListItemAttr()
%% OnGetItemColumnImage = (This, Item, Column) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlistctrl.html#wxlistctrlcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({winid, Winid}, Acc) -> [<<1:32/?UI,Winid:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteallitems">external documentation</a>.
-spec deleteAllItems(This) -> boolean() when
	This::wxListCtrl().
deleteAllItems(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_DeleteAllItems,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeletecolumn">external documentation</a>.
-spec deleteColumn(This, Col) -> boolean() when
	This::wxListCtrl(), Col::integer().
deleteColumn(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_DeleteColumn,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteitem">external documentation</a>.
-spec deleteItem(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
deleteItem(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_DeleteItem,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrleditlabel">external documentation</a>.
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_EditLabel,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlensurevisible">external documentation</a>.
-spec ensureVisible(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
ensureVisible(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_EnsureVisible,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @equiv findItem(This,Start,Str, [])
-spec findItem(This, Start, Str) -> integer() when
	This::wxListCtrl(), Start::integer(), Str::unicode:chardata().

findItem(This,Start,Str)
 when is_record(This, wx_ref),is_integer(Start),is_list(Str) ->
  findItem(This,Start,Str, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlfinditem">external documentation</a>.
%% <br /> Also:<br />
%% findItem(This, Start, Pt, Direction) -> integer() when<br />
%% 	This::wxListCtrl(), Start::integer(), Pt::{X::integer(), Y::integer()}, Direction::integer().<br />
%% 
-spec findItem(This, Start, Str, [Option]) -> integer() when
	This::wxListCtrl(), Start::integer(), Str::unicode:chardata(),
	Option :: {'partial', boolean()};
      (This, Start, Pt, Direction) -> integer() when
	This::wxListCtrl(), Start::integer(), Pt::{X::integer(), Y::integer()}, Direction::integer().
findItem(#wx_ref{type=ThisT,ref=ThisRef},Start,Str, Options)
 when is_integer(Start),is_list(Str),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Str_UC = unicode:characters_to_binary([Str,0]),
  MOpts = fun({partial, Partial}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Partial)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_FindItem_3_0,
  <<ThisRef:32/?UI,Start:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((4+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>);
findItem(#wx_ref{type=ThisT,ref=ThisRef},Start,{PtX,PtY},Direction)
 when is_integer(Start),is_integer(PtX),is_integer(PtY),is_integer(Direction) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_FindItem_3_1,
  <<ThisRef:32/?UI,Start:32/?UI,PtX:32/?UI,PtY:32/?UI,Direction:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumn">external documentation</a>.
-spec getColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
getColumn(#wx_ref{type=ThisT,ref=ThisRef},Col,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:call(?wxListCtrl_GetColumn,
  <<ThisRef:32/?UI,Col:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumncount">external documentation</a>.
-spec getColumnCount(This) -> integer() when
	This::wxListCtrl().
getColumnCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetColumnCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumnwidth">external documentation</a>.
-spec getColumnWidth(This, Col) -> integer() when
	This::wxListCtrl(), Col::integer().
getColumnWidth(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetColumnWidth,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcountperpage">external documentation</a>.
-spec getCountPerPage(This) -> integer() when
	This::wxListCtrl().
getCountPerPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetCountPerPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgeteditcontrol">external documentation</a>.
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl().
getEditControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetEditControl,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetimagelist">external documentation</a>.
-spec getImageList(This, Which) -> wxImageList:wxImageList() when
	This::wxListCtrl(), Which::integer().
getImageList(#wx_ref{type=ThisT,ref=ThisRef},Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetImageList,
  <<ThisRef:32/?UI,Which:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitem">external documentation</a>.
-spec getItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
getItem(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=InfoT,ref=InfoRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:call(?wxListCtrl_GetItem,
  <<ThisRef:32/?UI,InfoRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitembackgroundcolour">external documentation</a>.
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemBackgroundColour,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemcount">external documentation</a>.
-spec getItemCount(This) -> integer() when
	This::wxListCtrl().
getItemCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemdata">external documentation</a>.
-spec getItemData(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemData,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemfont">external documentation</a>.
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxListCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemFont,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemposition">external documentation</a>.
-spec getItemPosition(This, Item) -> Result when
	Result ::{Res ::boolean(), Pos::{X::integer(), Y::integer()}},
	This::wxListCtrl(), Item::integer().
getItemPosition(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemPosition,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @equiv getItemRect(This,Item, [])
-spec getItemRect(This, Item) -> Result when
	Result ::{Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxListCtrl(), Item::integer().

getItemRect(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getItemRect(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemrect">external documentation</a>.
-spec getItemRect(This, Item, [Option]) -> Result when
	Result :: {Res ::boolean(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}},
	This::wxListCtrl(), Item::integer(),
	Option :: {'code', integer()}.
getItemRect(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({code, Code}, Acc) -> [<<1:32/?UI,Code:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_GetItemRect,
  <<ThisRef:32/?UI,Item:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemspacing">external documentation</a>.
-spec getItemSpacing(This) -> {W::integer(), H::integer()} when
	This::wxListCtrl().
getItemSpacing(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemSpacing,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemstate">external documentation</a>.
-spec getItemState(This, Item, StateMask) -> integer() when
	This::wxListCtrl(), Item::integer(), StateMask::integer().
getItemState(#wx_ref{type=ThisT,ref=ThisRef},Item,StateMask)
 when is_integer(Item),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemState,
  <<ThisRef:32/?UI,Item:32/?UI,StateMask:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemtext">external documentation</a>.
-spec getItemText(This, Item) -> unicode:charlist() when
	This::wxListCtrl(), Item::integer().
getItemText(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemText,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemtextcolour">external documentation</a>.
-spec getItemTextColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemTextColour(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetItemTextColour,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @equiv getNextItem(This,Item, [])
-spec getNextItem(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().

getNextItem(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  getNextItem(This,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetnextitem">external documentation</a>.
-spec getNextItem(This, Item, [Option]) -> integer() when
	This::wxListCtrl(), Item::integer(),
	Option :: {'geometry', integer()}
		 | {'state', integer()}.
getNextItem(#wx_ref{type=ThisT,ref=ThisRef},Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({geometry, Geometry}, Acc) -> [<<1:32/?UI,Geometry:32/?UI>>|Acc];
          ({state, State}, Acc) -> [<<2:32/?UI,State:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_GetNextItem,
  <<ThisRef:32/?UI,Item:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetselecteditemcount">external documentation</a>.
-spec getSelectedItemCount(This) -> integer() when
	This::wxListCtrl().
getSelectedItemCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetSelectedItemCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListCtrl().
getTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettopitem">external documentation</a>.
-spec getTopItem(This) -> integer() when
	This::wxListCtrl().
getTopItem(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetTopItem,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetviewrect">external documentation</a>.
-spec getViewRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxListCtrl().
getViewRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_GetViewRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlhittest">external documentation</a>.
-spec hitTest(This, Point) -> Result when
	Result ::{Res ::integer(), Flags::integer(), PSubItem::integer()},
	This::wxListCtrl(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT,ref=ThisRef},{PointX,PointY})
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_HitTest,
  <<ThisRef:32/?UI,PointX:32/?UI,PointY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertcolumn">external documentation</a>.
%% <br /> Also:<br />
%% insertColumn(This, Col, Info) -> integer() when<br />
%% 	This::wxListCtrl(), Col::integer(), Info::wxListItem:wxListItem().<br />
%% 
-spec insertColumn(This, Col, Heading) -> integer() when
	This::wxListCtrl(), Col::integer(), Heading::unicode:chardata();
      (This, Col, Info) -> integer() when
	This::wxListCtrl(), Col::integer(), Info::wxListItem:wxListItem().

insertColumn(This,Col,Heading)
 when is_record(This, wx_ref),is_integer(Col),is_list(Heading) ->
  insertColumn(This,Col,Heading, []);
insertColumn(#wx_ref{type=ThisT,ref=ThisRef},Col,#wx_ref{type=InfoT,ref=InfoRef})
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:call(?wxListCtrl_InsertColumn_2,
  <<ThisRef:32/?UI,Col:32/?UI,InfoRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertcolumn">external documentation</a>.
-spec insertColumn(This, Col, Heading, [Option]) -> integer() when
	This::wxListCtrl(), Col::integer(), Heading::unicode:chardata(),
	Option :: {'format', integer()}
		 | {'width', integer()}.
insertColumn(#wx_ref{type=ThisT,ref=ThisRef},Col,Heading, Options)
 when is_integer(Col),is_list(Heading),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Heading_UC = unicode:characters_to_binary([Heading,0]),
  MOpts = fun({format, Format}, Acc) -> [<<1:32/?UI,Format:32/?UI>>|Acc];
          ({width, Width}, Acc) -> [<<2:32/?UI,Width:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_InsertColumn_3,
  <<ThisRef:32/?UI,Col:32/?UI,(byte_size(Heading_UC)):32/?UI,(Heading_UC)/binary, 0:(((8- ((4+byte_size(Heading_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
-spec insertItem(This, Info) -> integer() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
insertItem(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=InfoT,ref=InfoRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:call(?wxListCtrl_InsertItem_1,
  <<ThisRef:32/?UI,InfoRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
%% <br /> Also:<br />
%% insertItem(This, Index, Label) -> integer() when<br />
%% 	This::wxListCtrl(), Index::integer(), Label::unicode:chardata().<br />
%% 
-spec insertItem(This, Index, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), ImageIndex::integer();
      (This, Index, Label) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata().
insertItem(#wx_ref{type=ThisT,ref=ThisRef},Index,ImageIndex)
 when is_integer(Index),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_InsertItem_2_0,
  <<ThisRef:32/?UI,Index:32/?UI,ImageIndex:32/?UI>>);
insertItem(#wx_ref{type=ThisT,ref=ThisRef},Index,Label)
 when is_integer(Index),is_list(Label) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:call(?wxListCtrl_InsertItem_2_1,
  <<ThisRef:32/?UI,Index:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlinsertitem">external documentation</a>.
-spec insertItem(This, Index, Label, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata(), ImageIndex::integer().
insertItem(#wx_ref{type=ThisT,ref=ThisRef},Index,Label,ImageIndex)
 when is_integer(Index),is_list(Label),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:call(?wxListCtrl_InsertItem_3,
  <<ThisRef:32/?UI,Index:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,ImageIndex:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitem">external documentation</a>.
-spec refreshItem(This, Item) -> 'ok' when
	This::wxListCtrl(), Item::integer().
refreshItem(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_RefreshItem,
  <<ThisRef:32/?UI,Item:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitems">external documentation</a>.
-spec refreshItems(This, ItemFrom, ItemTo) -> 'ok' when
	This::wxListCtrl(), ItemFrom::integer(), ItemTo::integer().
refreshItems(#wx_ref{type=ThisT,ref=ThisRef},ItemFrom,ItemTo)
 when is_integer(ItemFrom),is_integer(ItemTo) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_RefreshItems,
  <<ThisRef:32/?UI,ItemFrom:32/?UI,ItemTo:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlscrolllist">external documentation</a>.
-spec scrollList(This, Dx, Dy) -> boolean() when
	This::wxListCtrl(), Dx::integer(), Dy::integer().
scrollList(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy)
 when is_integer(Dx),is_integer(Dy) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_ScrollList,
  <<ThisRef:32/?UI,Dx:32/?UI,Dy:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, Colour) -> boolean() when
	This::wxListCtrl(), Colour::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumn">external documentation</a>.
-spec setColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
setColumn(#wx_ref{type=ThisT,ref=ThisRef},Col,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:call(?wxListCtrl_SetColumn,
  <<ThisRef:32/?UI,Col:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumnwidth">external documentation</a>.
-spec setColumnWidth(This, Col, Width) -> boolean() when
	This::wxListCtrl(), Col::integer(), Width::integer().
setColumnWidth(#wx_ref{type=ThisT,ref=ThisRef},Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetColumnWidth,
  <<ThisRef:32/?UI,Col:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetimagelist">external documentation</a>.
-spec setImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
setImageList(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ImageListT,ref=ImageListRef},Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:cast(?wxListCtrl_SetImageList,
  <<ThisRef:32/?UI,ImageListRef:32/?UI,Which:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitem">external documentation</a>.
-spec setItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
setItem(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=InfoT,ref=InfoRef}) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:call(?wxListCtrl_SetItem_1,
  <<ThisRef:32/?UI,InfoRef:32/?UI>>).

%% @equiv setItem(This,Index,Col,Label, [])
-spec setItem(This, Index, Col, Label) -> integer() when
	This::wxListCtrl(), Index::integer(), Col::integer(), Label::unicode:chardata().

setItem(This,Index,Col,Label)
 when is_record(This, wx_ref),is_integer(Index),is_integer(Col),is_list(Label) ->
  setItem(This,Index,Col,Label, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitem">external documentation</a>.
-spec setItem(This, Index, Col, Label, [Option]) -> integer() when
	This::wxListCtrl(), Index::integer(), Col::integer(), Label::unicode:chardata(),
	Option :: {'imageId', integer()}.
setItem(#wx_ref{type=ThisT,ref=ThisRef},Index,Col,Label, Options)
 when is_integer(Index),is_integer(Col),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary([Label,0]),
  MOpts = fun({imageId, ImageId}, Acc) -> [<<1:32/?UI,ImageId:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_SetItem_4,
  <<ThisRef:32/?UI,Index:32/?UI,Col:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((0+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitembackgroundcolour">external documentation</a>.
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_SetItemBackgroundColour,
  <<ThisRef:32/?UI,Item:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemcount">external documentation</a>.
-spec setItemCount(This, Count) -> 'ok' when
	This::wxListCtrl(), Count::integer().
setItemCount(#wx_ref{type=ThisT,ref=ThisRef},Count)
 when is_integer(Count) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_SetItemCount,
  <<ThisRef:32/?UI,Count:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemdata">external documentation</a>.
-spec setItemData(This, Item, Data) -> boolean() when
	This::wxListCtrl(), Item::integer(), Data::integer().
setItemData(#wx_ref{type=ThisT,ref=ThisRef},Item,Data)
 when is_integer(Item),is_integer(Data) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetItemData,
  <<ThisRef:32/?UI,Item:32/?UI,Data:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemfont">external documentation</a>.
-spec setItemFont(This, Item, F) -> 'ok' when
	This::wxListCtrl(), Item::integer(), F::wxFont:wxFont().
setItemFont(#wx_ref{type=ThisT,ref=ThisRef},Item,#wx_ref{type=FT,ref=FRef})
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(FT,wxFont),
  wxe_util:cast(?wxListCtrl_SetItemFont,
  <<ThisRef:32/?UI,Item:32/?UI,FRef:32/?UI>>).

%% @equiv setItemImage(This,Item,Image, [])
-spec setItemImage(This, Item, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Image::integer().

setItemImage(This,Item,Image)
 when is_record(This, wx_ref),is_integer(Item),is_integer(Image) ->
  setItemImage(This,Item,Image, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemimage">external documentation</a>.
-spec setItemImage(This, Item, Image, [Option]) -> boolean() when
	This::wxListCtrl(), Item::integer(), Image::integer(),
	Option :: {'selImage', integer()}.
setItemImage(#wx_ref{type=ThisT,ref=ThisRef},Item,Image, Options)
 when is_integer(Item),is_integer(Image),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({selImage, SelImage}, Acc) -> [<<1:32/?UI,SelImage:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxListCtrl_SetItemImage,
  <<ThisRef:32/?UI,Item:32/?UI,Image:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemcolumnimage">external documentation</a>.
-spec setItemColumnImage(This, Item, Column, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Column::integer(), Image::integer().
setItemColumnImage(#wx_ref{type=ThisT,ref=ThisRef},Item,Column,Image)
 when is_integer(Item),is_integer(Column),is_integer(Image) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetItemColumnImage,
  <<ThisRef:32/?UI,Item:32/?UI,Column:32/?UI,Image:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemposition">external documentation</a>.
-spec setItemPosition(This, Item, Pos) -> boolean() when
	This::wxListCtrl(), Item::integer(), Pos::{X::integer(), Y::integer()}.
setItemPosition(#wx_ref{type=ThisT,ref=ThisRef},Item,{PosX,PosY})
 when is_integer(Item),is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetItemPosition,
  <<ThisRef:32/?UI,Item:32/?UI,PosX:32/?UI,PosY:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemstate">external documentation</a>.
-spec setItemState(This, Item, State, StateMask) -> boolean() when
	This::wxListCtrl(), Item::integer(), State::integer(), StateMask::integer().
setItemState(#wx_ref{type=ThisT,ref=ThisRef},Item,State,StateMask)
 when is_integer(Item),is_integer(State),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:call(?wxListCtrl_SetItemState,
  <<ThisRef:32/?UI,Item:32/?UI,State:32/?UI,StateMask:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtext">external documentation</a>.
-spec setItemText(This, Item, Str) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Str::unicode:chardata().
setItemText(#wx_ref{type=ThisT,ref=ThisRef},Item,Str)
 when is_integer(Item),is_list(Str) ->
  ?CLASS(ThisT,wxListCtrl),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxListCtrl_SetItemText,
  <<ThisRef:32/?UI,Item:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((4+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtextcolour">external documentation</a>.
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT,ref=ThisRef},Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_SetItemTextColour,
  <<ThisRef:32/?UI,Item:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @equiv setSingleStyle(This,Style, [])
-spec setSingleStyle(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().

setSingleStyle(This,Style)
 when is_record(This, wx_ref),is_integer(Style) ->
  setSingleStyle(This,Style, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetsinglestyle">external documentation</a>.
-spec setSingleStyle(This, Style, [Option]) -> 'ok' when
	This::wxListCtrl(), Style::integer(),
	Option :: {'add', boolean()}.
setSingleStyle(#wx_ref{type=ThisT,ref=ThisRef},Style, Options)
 when is_integer(Style),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({add, Add}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Add)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxListCtrl_SetSingleStyle,
  <<ThisRef:32/?UI,Style:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsettextcolour">external documentation</a>.
-spec setTextColour(This, Col) -> 'ok' when
	This::wxListCtrl(), Col::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_SetTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetwindowstyleflag">external documentation</a>.
-spec setWindowStyleFlag(This, Style) -> 'ok' when
	This::wxListCtrl(), Style::integer().
setWindowStyleFlag(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:cast(?wxListCtrl_SetWindowStyleFlag,
  <<ThisRef:32/?UI,Style:32/?UI>>).


%% @spec (This::wxListCtrl(), SortCallBack::function()) -> boolean()
%% @doc Sort the items in the list control<br />
%%   <pre>SortCallBack(Item1,Item2) -> integer()</pre>
%%  <br /> SortCallBack receives the client data associated with two items
%%         to compare, and should return 0 if the items are equal, a negative
%%         value if the first item is less than the second one and a positive
%%         value if the first item is greater than the second one.
%%  <br /> NOTE: The callback may not call other (wx) processes.
sortItems(#wx_ref{type=ThisT,ref=ThisRef}, SortCallBack)
  when is_function(SortCallBack, 2) ->
	?CLASS(ThisT,wxListCtrl),
	Sort = fun([Item1,Item2]) ->
			Result = SortCallBack(Item1,Item2),
			<<Result:32/?UI>>
		end,
	SortId = wxe_util:get_cbId(Sort),
	wxe_util:call(?wxListCtrl_SortItems, <<ThisRef:32/?UI,SortId:32/?UI>>).
%% @doc Destroys this object, do not use object again
-spec destroy(This::wxListCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListCtrl),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
%% @hidden
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
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
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
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
