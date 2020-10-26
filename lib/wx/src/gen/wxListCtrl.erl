%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
arrange(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({flag, _flag} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxListCtrl_Arrange),
  wxe_util:rec(?wxListCtrl_Arrange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlassignimagelist">external documentation</a>.
-spec assignImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_AssignImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlclearall">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteallitems">external documentation</a>.
-spec deleteAllItems(This) -> boolean() when
	This::wxListCtrl().
deleteAllItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_DeleteAllItems),
  wxe_util:rec(?wxListCtrl_DeleteAllItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeletecolumn">external documentation</a>.
-spec deleteColumn(This, Col) -> boolean() when
	This::wxListCtrl(), Col::integer().
deleteColumn(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_DeleteColumn),
  wxe_util:rec(?wxListCtrl_DeleteColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrldeleteitem">external documentation</a>.
-spec deleteItem(This, Item) -> boolean() when
	This::wxListCtrl(), Item::integer().
deleteItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_DeleteItem),
  wxe_util:rec(?wxListCtrl_DeleteItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrleditlabel">external documentation</a>.
-spec editLabel(This, Item) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl(), Item::integer().
editLabel(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_EditLabel),
  wxe_util:rec(?wxListCtrl_EditLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlensurevisible">external documentation</a>.
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
-spec getColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
getColumn(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=ItemT}=Item)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:queue_cmd(This,Col,Item,?get_env(),?wxListCtrl_GetColumn),
  wxe_util:rec(?wxListCtrl_GetColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumncount">external documentation</a>.
-spec getColumnCount(This) -> integer() when
	This::wxListCtrl().
getColumnCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetColumnCount),
  wxe_util:rec(?wxListCtrl_GetColumnCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcolumnwidth">external documentation</a>.
-spec getColumnWidth(This, Col) -> integer() when
	This::wxListCtrl(), Col::integer().
getColumnWidth(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxListCtrl_GetColumnWidth),
  wxe_util:rec(?wxListCtrl_GetColumnWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetcountperpage">external documentation</a>.
-spec getCountPerPage(This) -> integer() when
	This::wxListCtrl().
getCountPerPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetCountPerPage),
  wxe_util:rec(?wxListCtrl_GetCountPerPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgeteditcontrol">external documentation</a>.
-spec getEditControl(This) -> wxTextCtrl:wxTextCtrl() when
	This::wxListCtrl().
getEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetEditControl),
  wxe_util:rec(?wxListCtrl_GetEditControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetimagelist">external documentation</a>.
-spec getImageList(This, Which) -> wxImageList:wxImageList() when
	This::wxListCtrl(), Which::integer().
getImageList(#wx_ref{type=ThisT}=This,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Which,?get_env(),?wxListCtrl_GetImageList),
  wxe_util:rec(?wxListCtrl_GetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitem">external documentation</a>.
-spec getItem(This, Info) -> boolean() when
	This::wxListCtrl(), Info::wxListItem:wxListItem().
getItem(#wx_ref{type=ThisT}=This,#wx_ref{type=InfoT}=Info) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(InfoT,wxListItem),
  wxe_util:queue_cmd(This,Info,?get_env(),?wxListCtrl_GetItem),
  wxe_util:rec(?wxListCtrl_GetItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitembackgroundcolour">external documentation</a>.
-spec getItemBackgroundColour(This, Item) -> wx:wx_colour4() when
	This::wxListCtrl(), Item::integer().
getItemBackgroundColour(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemBackgroundColour),
  wxe_util:rec(?wxListCtrl_GetItemBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemcount">external documentation</a>.
-spec getItemCount(This) -> integer() when
	This::wxListCtrl().
getItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemCount),
  wxe_util:rec(?wxListCtrl_GetItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemdata">external documentation</a>.
-spec getItemData(This, Item) -> integer() when
	This::wxListCtrl(), Item::integer().
getItemData(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemData),
  wxe_util:rec(?wxListCtrl_GetItemData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemfont">external documentation</a>.
-spec getItemFont(This, Item) -> wxFont:wxFont() when
	This::wxListCtrl(), Item::integer().
getItemFont(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_GetItemFont),
  wxe_util:rec(?wxListCtrl_GetItemFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemposition">external documentation</a>.
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
-spec getItemSpacing(This) -> {W::integer(), H::integer()} when
	This::wxListCtrl().
getItemSpacing(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetItemSpacing),
  wxe_util:rec(?wxListCtrl_GetItemSpacing).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetitemstate">external documentation</a>.
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
-spec getSelectedItemCount(This) -> integer() when
	This::wxListCtrl().
getSelectedItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetSelectedItemCount),
  wxe_util:rec(?wxListCtrl_GetSelectedItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettextcolour">external documentation</a>.
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListCtrl().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTextColour),
  wxe_util:rec(?wxListCtrl_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgettopitem">external documentation</a>.
-spec getTopItem(This) -> integer() when
	This::wxListCtrl().
getTopItem(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetTopItem),
  wxe_util:rec(?wxListCtrl_GetTopItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlgetviewrect">external documentation</a>.
-spec getViewRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxListCtrl().
getViewRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxListCtrl_GetViewRect),
  wxe_util:rec(?wxListCtrl_GetViewRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlhittest">external documentation</a>.
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
-spec insertItem(This, Index, Label, ImageIndex) -> integer() when
	This::wxListCtrl(), Index::integer(), Label::unicode:chardata(), ImageIndex::integer().
insertItem(#wx_ref{type=ThisT}=This,Index,Label,ImageIndex)
 when is_integer(Index),?is_chardata(Label),is_integer(ImageIndex) ->
  ?CLASS(ThisT,wxListCtrl),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Index,Label_UC,ImageIndex,?get_env(),?wxListCtrl_InsertItem_3),
  wxe_util:rec(?wxListCtrl_InsertItem_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitem">external documentation</a>.
-spec refreshItem(This, Item) -> 'ok' when
	This::wxListCtrl(), Item::integer().
refreshItem(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxListCtrl_RefreshItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlrefreshitems">external documentation</a>.
-spec refreshItems(This, ItemFrom, ItemTo) -> 'ok' when
	This::wxListCtrl(), ItemFrom::integer(), ItemTo::integer().
refreshItems(#wx_ref{type=ThisT}=This,ItemFrom,ItemTo)
 when is_integer(ItemFrom),is_integer(ItemTo) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,ItemFrom,ItemTo,?get_env(),?wxListCtrl_RefreshItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlscrolllist">external documentation</a>.
-spec scrollList(This, Dx, Dy) -> boolean() when
	This::wxListCtrl(), Dx::integer(), Dy::integer().
scrollList(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_integer(Dx),is_integer(Dy) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxListCtrl_ScrollList),
  wxe_util:rec(?wxListCtrl_ScrollList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetbackgroundcolour">external documentation</a>.
-spec setBackgroundColour(This, Col) -> boolean() when
	This::wxListCtrl(), Col::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetBackgroundColour),
  wxe_util:rec(?wxListCtrl_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumn">external documentation</a>.
-spec setColumn(This, Col, Item) -> boolean() when
	This::wxListCtrl(), Col::integer(), Item::wxListItem:wxListItem().
setColumn(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=ItemT}=Item)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ItemT,wxListItem),
  wxe_util:queue_cmd(This,Col,Item,?get_env(),?wxListCtrl_SetColumn),
  wxe_util:rec(?wxListCtrl_SetColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetcolumnwidth">external documentation</a>.
-spec setColumnWidth(This, Col, Width) -> boolean() when
	This::wxListCtrl(), Col::integer(), Width::integer().
setColumnWidth(#wx_ref{type=ThisT}=This,Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Col,Width,?get_env(),?wxListCtrl_SetColumnWidth),
  wxe_util:rec(?wxListCtrl_SetColumnWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetimagelist">external documentation</a>.
-spec setImageList(This, ImageList, Which) -> 'ok' when
	This::wxListCtrl(), ImageList::wxImageList:wxImageList(), Which::integer().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList,Which)
 when is_integer(Which) ->
  ?CLASS(ThisT,wxListCtrl),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,Which,?get_env(),?wxListCtrl_SetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitem">external documentation</a>.
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
-spec setItemBackgroundColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemBackgroundColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemcount">external documentation</a>.
-spec setItemCount(This, Count) -> 'ok' when
	This::wxListCtrl(), Count::integer().
setItemCount(#wx_ref{type=ThisT}=This,Count)
 when is_integer(Count) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Count,?get_env(),?wxListCtrl_SetItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemdata">external documentation</a>.
-spec setItemData(This, Item, Data) -> boolean() when
	This::wxListCtrl(), Item::integer(), Data::integer().
setItemData(#wx_ref{type=ThisT}=This,Item,Data)
 when is_integer(Item),is_integer(Data) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Data,?get_env(),?wxListCtrl_SetItemData),
  wxe_util:rec(?wxListCtrl_SetItemData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemfont">external documentation</a>.
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
-spec setItemColumnImage(This, Item, Column, Image) -> boolean() when
	This::wxListCtrl(), Item::integer(), Column::integer(), Image::integer().
setItemColumnImage(#wx_ref{type=ThisT}=This,Item,Column,Image)
 when is_integer(Item),is_integer(Column),is_integer(Image) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Column,Image,?get_env(),?wxListCtrl_SetItemColumnImage),
  wxe_util:rec(?wxListCtrl_SetItemColumnImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemposition">external documentation</a>.
-spec setItemPosition(This, Item, Pos) -> boolean() when
	This::wxListCtrl(), Item::integer(), Pos::{X::integer(), Y::integer()}.
setItemPosition(#wx_ref{type=ThisT}=This,Item,{PosX,PosY} = Pos)
 when is_integer(Item),is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,Pos,?get_env(),?wxListCtrl_SetItemPosition),
  wxe_util:rec(?wxListCtrl_SetItemPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemstate">external documentation</a>.
-spec setItemState(This, Item, State, StateMask) -> boolean() when
	This::wxListCtrl(), Item::integer(), State::integer(), StateMask::integer().
setItemState(#wx_ref{type=ThisT}=This,Item,State,StateMask)
 when is_integer(Item),is_integer(State),is_integer(StateMask) ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,State,StateMask,?get_env(),?wxListCtrl_SetItemState),
  wxe_util:rec(?wxListCtrl_SetItemState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtext">external documentation</a>.
-spec setItemText(This, Item, Text) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Text::unicode:chardata().
setItemText(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxListCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxListCtrl_SetItemText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetitemtextcolour">external documentation</a>.
-spec setItemTextColour(This, Item, Col) -> 'ok' when
	This::wxListCtrl(), Item::integer(), Col::wx:wx_colour().
setItemTextColour(#wx_ref{type=ThisT}=This,Item,Col)
 when is_integer(Item),tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,Item,wxe_util:color(Col),?get_env(),?wxListCtrl_SetItemTextColour).

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
setSingleStyle(#wx_ref{type=ThisT}=This,Style, Options)
 when is_integer(Style),is_list(Options) ->
  ?CLASS(ThisT,wxListCtrl),
  MOpts = fun({add, _add} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Style, Opts,?get_env(),?wxListCtrl_SetSingleStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsettextcolour">external documentation</a>.
-spec setTextColour(This, Col) -> 'ok' when
	This::wxListCtrl(), Col::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxListCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Col),?get_env(),?wxListCtrl_SetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistctrl.html#wxlistctrlsetwindowstyleflag">external documentation</a>.
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

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxListCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
%% @hidden
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
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
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
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
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
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
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
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
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
%% @hidden
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
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
isFrozen(This) -> wxWindow:isFrozen(This).
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
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
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
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
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
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
%% @hidden
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
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
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
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
