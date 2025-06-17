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

-module(wxRadioBox).
-moduledoc """
A radio box item is used to select one of number of mutually exclusive choices.

It is displayed as a vertical column or horizontal row of labelled buttons.

## Styles

This class supports the following styles:

* wxRA_SPECIFY_ROWS: The major dimension parameter refers to the maximum number of rows.

* wxRA_SPECIFY_COLS: The major dimension parameter refers to the maximum number of columns.

See:
* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

* `m:wxRadioButton`

* `m:wxCheckBox`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxRadioBox](https://docs.wxwidgets.org/3.2/classwx_radio_box.html)

## Events

Event types emitted from this class:

* [`command_radiobox_selected`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([create/7,create/8,destroy/1,enable/1,enable/2,enable/3,getColumnCount/1,
  getItemFromPoint/2,getItemHelpText/2,getItemToolTip/2,getRowCount/1,
  getSelection/1,getString/2,isItemEnabled/2,isItemShown/2,new/6,new/7,
  setItemHelpText/3,setItemToolTip/3,setSelection/2,show/2,show/3]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCaret/1,
  getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,getThemeEnabled/1,
  getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
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
  shouldInheritColours/1,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxRadioBox() :: wx:wx_object().
-export_type([wxRadioBox/0]).
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Parent,Id,Label,Pos,Size,Choices, [])}).
-spec new(Parent, Id, Label, Pos, Size, Choices) -> wxRadioBox() when
	Parent::wxWindow:wxWindow(), Id::integer(), Label::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()].

new(Parent,Id,Label,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices)
 when is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Label),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices) ->
  new(Parent,Id,Label,Pos,Size,Choices, []).

-doc """
Constructor, creating and showing a radiobox.

See: `create/8`
""".
-spec new(Parent, Id, Label, Pos, Size, Choices, [Option]) -> wxRadioBox() when
	Parent::wxWindow:wxWindow(), Id::integer(), Label::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()],
	Option :: {'majorDim', integer()}
		 | {'style', integer()}
		 | {'val', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id,Label,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices, Options)
 when is_integer(Id),?is_chardata(Label),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  Label_UC = unicode:characters_to_binary(Label),
  Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||
              ChoicesTemp <- Choices],
  MOpts = fun({majorDim, _majorDim} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({val, #wx_ref{type=ValT}} = Arg) ->   ?CLASS(ValT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id,Label_UC,Pos,Size,Choices_UCA, Opts,?get_env(),?wxRadioBox_new),
  wxe_util:rec(?wxRadioBox_new).

-doc(#{equiv => create(This,Parent,Id,Label,Pos,Size,Choices, [])}).
-spec create(This, Parent, Id, Label, Pos, Size, Choices) -> boolean() when
	This::wxRadioBox(), Parent::wxWindow:wxWindow(), Id::integer(), Label::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()].

create(This,Parent,Id,Label,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Label),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices) ->
  create(This,Parent,Id,Label,Pos,Size,Choices, []).

-doc """
Creates the radiobox for two-step construction.

See `new/7` for further details.
""".
-spec create(This, Parent, Id, Label, Pos, Size, Choices, [Option]) -> boolean() when
	This::wxRadioBox(), Parent::wxWindow:wxWindow(), Id::integer(), Label::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()],
	Option :: {'majorDim', integer()}
		 | {'style', integer()}
		 | {'val', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,Label,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices, Options)
 when is_integer(Id),?is_chardata(Label),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices),is_list(Options) ->
  ?CLASS(ThisT,wxRadioBox),
  ?CLASS(ParentT,wxWindow),
  Label_UC = unicode:characters_to_binary(Label),
  Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||
              ChoicesTemp <- Choices],
  MOpts = fun({majorDim, _majorDim} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({val, #wx_ref{type=ValT}} = Arg) ->   ?CLASS(ValT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Label_UC,Pos,Size,Choices_UCA, Opts,?get_env(),?wxRadioBox_Create),
  wxe_util:rec(?wxRadioBox_Create).

-doc(#{equiv => enable(This, [])}).
-spec enable(This) -> boolean() when
	This::wxRadioBox().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

-doc """
Enables or disables the radiobox.

See: `wxWindow:enable/2`
""".
-spec enable(This, N) -> boolean() when
	This::wxRadioBox(), N::integer();
      (This, [Option]) -> boolean() when
	This::wxRadioBox(),
	Option :: {'enable', boolean()}.

enable(This,N)
 when is_record(This, wx_ref),is_integer(N) ->
  enable(This,N, []);
enable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxRadioBox),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxRadioBox_Enable_1),
  wxe_util:rec(?wxRadioBox_Enable_1).

-doc """
Enables or disables an individual button in the radiobox.

See: `wxWindow:enable/2`
""".
-spec enable(This, N, [Option]) -> boolean() when
	This::wxRadioBox(), N::integer(),
	Option :: {'enable', boolean()}.
enable(#wx_ref{type=ThisT}=This,N, Options)
 when is_integer(N),is_list(Options) ->
  ?CLASS(ThisT,wxRadioBox),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,N, Opts,?get_env(),?wxRadioBox_Enable_2),
  wxe_util:rec(?wxRadioBox_Enable_2).

-doc """
Returns the index of the selected item or `wxNOT\_FOUND` if no item is selected.

Return: The position of the current selection.

Remark: This method can be used with single selection list boxes only, you should use `wxListBox:getSelections/1`
for the list boxes with wxLB_MULTIPLE style.

See:
* `setSelection/2`

* `wxControlWithItems:getStringSelection/1`
""".
-spec getSelection(This) -> integer() when
	This::wxRadioBox().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,?get_env(),?wxRadioBox_GetSelection),
  wxe_util:rec(?wxRadioBox_GetSelection).

-doc """
Returns the label of the item with the given index.

Return: The label of the item or an empty string if the position was invalid.
""".
-spec getString(This, N) -> unicode:charlist() when
	This::wxRadioBox(), N::integer().
getString(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxRadioBox_GetString),
  wxe_util:rec(?wxRadioBox_GetString).

-doc """
Sets the selection to the given item.

Notice that a radio box always has selection, so `n` must be valid here and passing `wxNOT_FOUND`
is not allowed.
""".
-spec setSelection(This, N) -> 'ok' when
	This::wxRadioBox(), N::integer().
setSelection(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxRadioBox_SetSelection).

-doc(#{equiv => show(This,Item, [])}).
-spec show(This, Item) -> boolean() when
	This::wxRadioBox(), Item::integer().

show(This,Item)
 when is_record(This, wx_ref),is_integer(Item) ->
  show(This,Item, []).

-doc """
Shows or hides individual buttons.

Return: true if the item has been shown or hidden or false if nothing was done because it
already was in the requested state.

See: `show/3`
""".
-spec show(This, Item, [Option]) -> boolean() when
	This::wxRadioBox(), Item::integer(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This,Item, Options)
 when is_integer(Item),is_list(Options) ->
  ?CLASS(ThisT,wxRadioBox),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Item, Opts,?get_env(),?wxRadioBox_Show),
  wxe_util:rec(?wxRadioBox_Show).

-doc "Returns the number of columns in the radiobox.".
-spec getColumnCount(This) -> integer() when
	This::wxRadioBox().
getColumnCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,?get_env(),?wxRadioBox_GetColumnCount),
  wxe_util:rec(?wxRadioBox_GetColumnCount).

-doc """
Returns the helptext associated with the specified `item` if any or `wxEmptyString`.

See: `setItemHelpText/3`
""".
-spec getItemHelpText(This, Item) -> unicode:charlist() when
	This::wxRadioBox(), Item::integer().
getItemHelpText(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxRadioBox_GetItemHelpText),
  wxe_util:rec(?wxRadioBox_GetItemHelpText).

-doc """
Returns the tooltip associated with the specified `item` if any or NULL.

See:
* `setItemToolTip/3`

* `wxWindow:getToolTip/1`
""".
-spec getItemToolTip(This, Item) -> wxToolTip:wxToolTip() when
	This::wxRadioBox(), Item::integer().
getItemToolTip(#wx_ref{type=ThisT}=This,Item)
 when is_integer(Item) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxRadioBox_GetItemToolTip),
  wxe_util:rec(?wxRadioBox_GetItemToolTip).

-doc """
Returns a radio box item under the point, a zero-based item index, or `wxNOT\_FOUND` if
no item is under the point.
""".
-spec getItemFromPoint(This, Pt) -> integer() when
	This::wxRadioBox(), Pt::{X::integer(), Y::integer()}.
getItemFromPoint(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRadioBox_GetItemFromPoint),
  wxe_util:rec(?wxRadioBox_GetItemFromPoint).

-doc "Returns the number of rows in the radiobox.".
-spec getRowCount(This) -> integer() when
	This::wxRadioBox().
getRowCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,?get_env(),?wxRadioBox_GetRowCount),
  wxe_util:rec(?wxRadioBox_GetRowCount).

-doc """
Returns true if the item is enabled or false if it was disabled using `enable/3`.

This function is currently only implemented in wxMSW, wxGTK, wxQT and wxUniversal and
always returns true in the other ports.
""".
-spec isItemEnabled(This, N) -> boolean() when
	This::wxRadioBox(), N::integer().
isItemEnabled(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxRadioBox_IsItemEnabled),
  wxe_util:rec(?wxRadioBox_IsItemEnabled).

-doc """
Returns true if the item is currently shown or false if it was hidden using `show/3`.

Note that this function returns true for an item which hadn't been hidden even if the
entire radiobox is not currently shown.

This function is currently only implemented in wxMSW, wxGTK, wxQT and wxUniversal and
always returns true in the other ports.
""".
-spec isItemShown(This, N) -> boolean() when
	This::wxRadioBox(), N::integer().
isItemShown(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxRadioBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxRadioBox_IsItemShown),
  wxe_util:rec(?wxRadioBox_IsItemShown).

-doc """
Sets the helptext for an item.

Empty string erases any existing helptext.

See: `getItemHelpText/2`
""".
-spec setItemHelpText(This, Item, Helptext) -> 'ok' when
	This::wxRadioBox(), Item::integer(), Helptext::unicode:chardata().
setItemHelpText(#wx_ref{type=ThisT}=This,Item,Helptext)
 when is_integer(Item),?is_chardata(Helptext) ->
  ?CLASS(ThisT,wxRadioBox),
  Helptext_UC = unicode:characters_to_binary(Helptext),
  wxe_util:queue_cmd(This,Item,Helptext_UC,?get_env(),?wxRadioBox_SetItemHelpText).

-doc """
Sets the tooltip text for the specified item in the radio group.

This function is currently only implemented in wxMSW and wxGTK2 and does nothing in the
other ports.

See:
* `getItemToolTip/2`

* `wxWindow:setToolTip/2`
""".
-spec setItemToolTip(This, Item, Text) -> 'ok' when
	This::wxRadioBox(), Item::integer(), Text::unicode:chardata().
setItemToolTip(#wx_ref{type=ThisT}=This,Item,Text)
 when is_integer(Item),?is_chardata(Text) ->
  ?CLASS(ThisT,wxRadioBox),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Item,Text_UC,?get_env(),?wxRadioBox_SetItemToolTip).

-doc "Destroys the object".
-spec destroy(This::wxRadioBox()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxRadioBox),
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
