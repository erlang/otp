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

-module(wxListBox).
-moduledoc """
Functions for wxListBox class

A listbox is used to select one or more of a list of strings.

The strings are displayed in a scrolling box, with the selected string(s) marked
in reverse video. A listbox can be single selection (if an item is selected, the
previous selection is removed) or multiple selection (clicking an item toggles
the item on or off independently of other selections).

List box elements are numbered from zero and while the maximal number of
elements is unlimited, it is usually better to use a virtual control, not
requiring to add all the items to it at once, such as `wxDataViewCtrl` (not
implemented in wx) or `m:wxListCtrl` with `wxLC_VIRTUAL` style, once more than a
few hundreds items need to be displayed because this control is not optimized,
neither from performance nor from user interface point of view, for large number
of items.

Notice that the list box doesn't support control characters other than `TAB`.

Styles

This class supports the following styles:

See: `wxEditableListBox` (not implemented in wx), `m:wxChoice`, `m:wxComboBox`,
`m:wxListCtrl`, `m:wxCommandEvent`

This class is derived (and can use functions) from: `m:wxControlWithItems`
`m:wxControl` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs:
[wxListBox](https://docs.wxwidgets.org/3.1/classwx_list_box.html)

## Events

Event types emitted from this class:
[`command_listbox_selected`](`m:wxCommandEvent`),
[`command_listbox_doubleclicked`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([create/6,create/7,deselect/2,destroy/1,getSelections/1,hitTest/2,hitTest/3,
  insertItems/3,isSelected/2,new/0,new/2,new/3,set/2,setFirstItem/2]).

%% inherited exports
-export([append/2,append/3,appendStrings/2,appendStrings/3,cacheBestSize/2,
  canSetTransparent/1,captureMouse/1,center/1,center/2,centerOnParent/1,
  centerOnParent/2,centre/1,centre/2,centreOnParent/1,centreOnParent/2,
  clear/1,clearBackground/1,clientToScreen/2,clientToScreen/3,close/1,
  close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  delete/2,destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findString/2,findString/3,findWindow/2,
  fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,
  getChildren/1,getClientData/2,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCount/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSelection/1,getSize/1,getSizer/1,getString/2,getStringSelection/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,insert/3,insert/4,insertStrings/3,insertStrings/4,invalidateBestSize/1,
  isDoubleBuffered/1,isEmpty/1,isEnabled/1,isExposed/2,isExposed/3,isExposed/5,
  isFrozen/1,isRetained/1,isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,
  lineDown/1,lineUp/1,lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  select/2,setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,
  setBackgroundStyle/2,setCaret/2,setClientData/3,setClientSize/2,setClientSize/3,
  setContainingSizer/2,setCursor/2,setDoubleBuffered/2,setDropTarget/2,
  setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,setForegroundColour/2,
  setHelpText/2,setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,
  setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,setPalette/2,
  setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,setSelection/2,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setString/3,setStringSelection/2,setThemeEnabled/2,setToolTip/2,setTransparent/2,
  setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxListBox() :: wx:wx_object().
-export_type([wxListBox/0]).
%% @hidden
-doc false.
parent_class(wxControlWithItems) -> true;
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxwxlistbox">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxListBox().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxListBox_new_0),
  wxe_util:rec(?wxListBox_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxListBox() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxwxlistbox">external documentation</a>.
-doc """
Constructor, creating and showing a list box.

See the other `new/3` constructor; the only difference is that this overload
takes a `wxArrayString` (not implemented in wx) instead of a pointer to an array
of `wxString` (not implemented in wx).
""".
-spec new(Parent, Id, [Option]) -> wxListBox() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'choices', [unicode:chardata()]}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({choices, Choices}) ->   Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||              ChoicesTemp <- Choices],{choices,Choices_UCA};
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxListBox_new_3),
  wxe_util:rec(?wxListBox_new_3).

%% @equiv create(This,Parent,Id,Pos,Size,Choices, [])
-spec create(This, Parent, Id, Pos, Size, Choices) -> boolean() when
	This::wxListBox(), Parent::wxWindow:wxWindow(), Id::integer(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()].

create(This,Parent,Id,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices) ->
  create(This,Parent,Id,Pos,Size,Choices, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxcreate">external documentation</a>.
-spec create(This, Parent, Id, Pos, Size, Choices, [Option]) -> boolean() when
	This::wxListBox(), Parent::wxWindow:wxWindow(), Id::integer(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()],
	Option :: {'style', integer()}
		 | {'validator', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices, Options)
 when is_integer(Id),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices),is_list(Options) ->
  ?CLASS(ThisT,wxListBox),
  ?CLASS(ParentT,wxWindow),
  Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||
              ChoicesTemp <- Choices],
  MOpts = fun({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Pos,Size,Choices_UCA, Opts,?get_env(),?wxListBox_Create),
  wxe_util:rec(?wxListBox_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxdeselect">external documentation</a>.
-doc """
Deselects an item in the list box.

Remark: This applies to multiple selection listboxes only.
""".
-spec deselect(This, N) -> 'ok' when
	This::wxListBox(), N::integer().
deselect(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxListBox_Deselect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxgetselections">external documentation</a>.
-doc """
Fill an array of ints with the positions of the currently selected items.

Return: The number of selections.

Remark: Use this with a multiple selection listbox.

See: `wxControlWithItems:getSelection/1`,
`wxControlWithItems:getStringSelection/1`, `wxControlWithItems:setSelection/2`
""".
-spec getSelections(This) -> Result when
	Result ::{Res ::integer(), Selections::[integer()]},
	This::wxListBox().
getSelections(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,?get_env(),?wxListBox_GetSelections),
  wxe_util:rec(?wxListBox_GetSelections).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxinsertitems">external documentation</a>.
-doc "Insert the given number of strings before the specified position.".
-spec insertItems(This, Items, Pos) -> 'ok' when
	This::wxListBox(), Items::[unicode:chardata()], Pos::integer().
insertItems(#wx_ref{type=ThisT}=This,Items,Pos)
 when is_list(Items),is_integer(Pos) ->
  ?CLASS(ThisT,wxListBox),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,Pos,?get_env(),?wxListBox_InsertItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxisselected">external documentation</a>.
-doc """
Determines whether an item is selected.

Return: true if the given item is selected, false otherwise.
""".
-spec isSelected(This, N) -> boolean() when
	This::wxListBox(), N::integer().
isSelected(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxListBox_IsSelected),
  wxe_util:rec(?wxListBox_IsSelected).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxset">external documentation</a>.
-doc """
Replaces the current control contents with the given items.

Notice that calling this method is usually much faster than appending them one
by one if you need to add a lot of items.
""".
-spec set(This, Items) -> 'ok' when
	This::wxListBox(), Items::[unicode:chardata()].
set(#wx_ref{type=ThisT}=This,Items)
 when is_list(Items) ->
  ?CLASS(ThisT,wxListBox),
  Items_UCA = [unicode:characters_to_binary(ItemsTemp) ||
              ItemsTemp <- Items],
  wxe_util:queue_cmd(This,Items_UCA,?get_env(),?wxListBox_Set).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxhittest">external documentation</a>.
-doc """
Returns the item located at `point`, or `wxNOT_FOUND` if there is no item
located at `point`.

It is currently implemented for wxMSW, wxMac and wxGTK2 ports.

Return: Item located at point, or wxNOT_FOUND if unimplemented or the item does
not exist.

Since: 2.7.0
""".
-spec hitTest(This, Point) -> integer() when
	This::wxListBox(), Point::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PointX,PointY} = Point)
 when is_integer(PointX),is_integer(PointY) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,Point,?get_env(),?wxListBox_HitTest_1),
  wxe_util:rec(?wxListBox_HitTest_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxhittest">external documentation</a>.
-doc """
This is an overloaded member function, provided for convenience. It differs from
the above function only in what argument(s) it accepts.
""".
-spec hitTest(This, X, Y) -> integer() when
	This::wxListBox(), X::integer(), Y::integer().
hitTest(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxListBox_HitTest_2),
  wxe_util:rec(?wxListBox_HitTest_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlistbox.html#wxlistboxsetfirstitem">external documentation</a>.
%% <br /> Also:<br />
%% setFirstItem(This, String) -> 'ok' when<br />
%% 	This::wxListBox(), String::unicode:chardata().<br />
%% 
-doc "Set the specified item to be the first visible item.".
-spec setFirstItem(This, N) -> 'ok' when
	This::wxListBox(), N::integer();
      (This, String) -> 'ok' when
	This::wxListBox(), String::unicode:chardata().
setFirstItem(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxListBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxListBox_SetFirstItem_1_0);
setFirstItem(#wx_ref{type=ThisT}=This,String)
 when ?is_chardata(String) ->
  ?CLASS(ThisT,wxListBox),
  String_UC = unicode:characters_to_binary(String),
  wxe_util:queue_cmd(This,String_UC,?get_env(),?wxListBox_SetFirstItem_1_1).

%% @doc Destroys this object, do not use object again
-doc "Destructor, destroying the list box.".
-spec destroy(This::wxListBox()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListBox),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControlWithItems
%% @hidden
-doc false.
setStringSelection(This,String) -> wxControlWithItems:setStringSelection(This,String).
%% @hidden
-doc false.
setString(This,N,String) -> wxControlWithItems:setString(This,N,String).
%% @hidden
-doc false.
setSelection(This,N) -> wxControlWithItems:setSelection(This,N).
%% @hidden
-doc false.
select(This,N) -> wxControlWithItems:select(This,N).
%% @hidden
-doc false.
isEmpty(This) -> wxControlWithItems:isEmpty(This).
%% @hidden
-doc false.
insertStrings(This,Items,Pos,ClientsData) -> wxControlWithItems:insertStrings(This,Items,Pos,ClientsData).
%% @hidden
-doc false.
insertStrings(This,Items,Pos) -> wxControlWithItems:insertStrings(This,Items,Pos).
%% @hidden
-doc false.
insert(This,Item,Pos,ClientData) -> wxControlWithItems:insert(This,Item,Pos,ClientData).
%% @hidden
-doc false.
insert(This,Item,Pos) -> wxControlWithItems:insert(This,Item,Pos).
%% @hidden
-doc false.
getStringSelection(This) -> wxControlWithItems:getStringSelection(This).
%% @hidden
-doc false.
getString(This,N) -> wxControlWithItems:getString(This,N).
%% @hidden
-doc false.
getSelection(This) -> wxControlWithItems:getSelection(This).
%% @hidden
-doc false.
getCount(This) -> wxControlWithItems:getCount(This).
%% @hidden
-doc false.
setClientData(This,N,Data) -> wxControlWithItems:setClientData(This,N,Data).
%% @hidden
-doc false.
getClientData(This,N) -> wxControlWithItems:getClientData(This,N).
%% @hidden
-doc false.
findString(This,String, Options) -> wxControlWithItems:findString(This,String, Options).
%% @hidden
-doc false.
findString(This,String) -> wxControlWithItems:findString(This,String).
%% @hidden
-doc false.
delete(This,N) -> wxControlWithItems:delete(This,N).
%% @hidden
-doc false.
clear(This) -> wxControlWithItems:clear(This).
%% @hidden
-doc false.
appendStrings(This,Items,ClientsData) -> wxControlWithItems:appendStrings(This,Items,ClientsData).
%% @hidden
-doc false.
appendStrings(This,Items) -> wxControlWithItems:appendStrings(This,Items).
%% @hidden
-doc false.
append(This,Item,ClientData) -> wxControlWithItems:append(This,Item,ClientData).
%% @hidden
-doc false.
append(This,Item) -> wxControlWithItems:append(This,Item).
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
