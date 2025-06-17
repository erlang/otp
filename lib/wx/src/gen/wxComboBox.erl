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

-module(wxComboBox).
-moduledoc """
A combobox is like a combination of an edit control and a listbox.

It can be displayed as static list with editable or read-only text field; or a drop-down
list with text field; or a drop-down list without a text field depending on the platform
and presence of wxCB_READONLY style.

A combobox permits a single selection only. Combobox items are numbered from zero.

If you need a customized combobox, have a look at `wxComboCtrl` (not implemented in wx), `wxOwnerDrawnComboBox`
(not implemented in wx), `wxComboPopup` (not implemented in wx) and the ready-to-use `wxBitmapComboBox`
(not implemented in wx).

Please refer to `wxTextEntry` (not implemented in wx) documentation for the description
of methods operating with the text entry part of the combobox and to `wxItemContainer`
(not implemented in wx) for the methods operating with the list of strings. Notice that at
least under MSW `m:wxComboBox` doesn't behave correctly if it contains strings differing
in case only so portable programs should avoid adding such strings to this control.

## Styles

This class supports the following styles:

* wxCB_SIMPLE: Creates a combobox with a permanently displayed list. Windows only.

* wxCB_DROPDOWN: Creates a combobox with a drop-down list. MSW and Motif only.

* wxCB_READONLY: A combobox with this style behaves like a `m:wxChoice` (and may look in
the same way as well, although this is platform-dependent), i.e. it allows the user to
choose from the list of options but doesn't allow to enter a value not present in the
list.

* wxCB_SORT: Sorts the entries in the list alphabetically.

* wxTE_PROCESS_ENTER: The control will generate the event `wxEVT_TEXT_ENTER` that can be
handled by the program. Otherwise, i.e. either if this style not specified at all, or it
is used, but there is no event handler for this event or the event handler called `wxEvent:skip/2` to
avoid overriding the default handling, pressing Enter key is either processed internally
by the control or used to activate the default button of the dialog, if any.

See:
* `m:wxListBox`

* `m:wxTextCtrl`

* `m:wxChoice`

* `m:wxCommandEvent`

This class is derived, and can use functions, from:

* `m:wxControlWithItems`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxComboBox](https://docs.wxwidgets.org/3.2/classwx_combo_box.html)

## Events

Event types emitted from this class:

* [`command_combobox_selected`](`m:wxCommandEvent`)

* [`command_text_updated`](`m:wxCommandEvent`)

* [`command_text_enter`](`m:wxCommandEvent`)

* [`combobox_dropdown`](`m:wxCommandEvent`)

* [`combobox_closeup`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([canCopy/1,canCut/1,canPaste/1,canRedo/1,canUndo/1,copy/1,create/7,create/8,
  cut/1,destroy/1,getInsertionPoint/1,getLastPosition/1,getValue/1,new/0,
  new/2,new/3,paste/1,redo/1,remove/3,replace/4,setInsertionPoint/2,setInsertionPointEnd/1,
  setSelection/2,setSelection/3,setValue/2,undo/1]).

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
  setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setString/3,
  setStringSelection/2,setThemeEnabled/2,setToolTip/2,setTransparent/2,
  setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxComboBox() :: wx:wx_object().
-export_type([wxComboBox/0]).
-doc false.
parent_class(wxControlWithItems) -> true;
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxComboBox().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxComboBox_new_0),
  wxe_util:rec(?wxComboBox_new_0).

-doc(#{equiv => new(Parent,Id, [])}).
-spec new(Parent, Id) -> wxComboBox() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

-doc """
Constructor, creating and showing a combobox.

See: `create/8`
""".
-spec new(Parent, Id, [Option]) -> wxComboBox() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'value', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'choices', [unicode:chardata()]}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({value, Value}) ->   Value_UC = unicode:characters_to_binary(Value),{value,Value_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({choices, Choices}) ->   Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||              ChoicesTemp <- Choices],{choices,Choices_UCA};
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxComboBox_new_3),
  wxe_util:rec(?wxComboBox_new_3).

-doc(#{equiv => create(This,Parent,Id,Value,Pos,Size,Choices, [])}).
-spec create(This, Parent, Id, Value, Pos, Size, Choices) -> boolean() when
	This::wxComboBox(), Parent::wxWindow:wxWindow(), Id::integer(), Value::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()].

create(This,Parent,Id,Value,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Value),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices) ->
  create(This,Parent,Id,Value,Pos,Size,Choices, []).

-doc "".
-spec create(This, Parent, Id, Value, Pos, Size, Choices, [Option]) -> boolean() when
	This::wxComboBox(), Parent::wxWindow:wxWindow(), Id::integer(), Value::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()],
	Option :: {'style', integer()}
		 | {'validator', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,Value,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices, Options)
 when is_integer(Id),?is_chardata(Value),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices),is_list(Options) ->
  ?CLASS(ThisT,wxComboBox),
  ?CLASS(ParentT,wxWindow),
  Value_UC = unicode:characters_to_binary(Value),
  Choices_UCA = [unicode:characters_to_binary(ChoicesTemp) ||
              ChoicesTemp <- Choices],
  MOpts = fun({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Value_UC,Pos,Size,Choices_UCA, Opts,?get_env(),?wxComboBox_Create),
  wxe_util:rec(?wxComboBox_Create).

-doc "Returns true if the selection can be copied to the clipboard.".
-spec canCopy(This) -> boolean() when
	This::wxComboBox().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanCopy),
  wxe_util:rec(?wxComboBox_CanCopy).

-doc "Returns true if the selection can be cut to the clipboard.".
-spec canCut(This) -> boolean() when
	This::wxComboBox().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanCut),
  wxe_util:rec(?wxComboBox_CanCut).

-doc """
Returns true if the contents of the clipboard can be pasted into the text control.

On some platforms (Motif, GTK) this is an approximation and returns true if the control
is editable, false otherwise.
""".
-spec canPaste(This) -> boolean() when
	This::wxComboBox().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanPaste),
  wxe_util:rec(?wxComboBox_CanPaste).

-doc "Returns true if there is a redo facility available and the last operation can be redone.".
-spec canRedo(This) -> boolean() when
	This::wxComboBox().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanRedo),
  wxe_util:rec(?wxComboBox_CanRedo).

-doc "Returns true if there is an undo facility available and the last operation can be undone.".
-spec canUndo(This) -> boolean() when
	This::wxComboBox().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanUndo),
  wxe_util:rec(?wxComboBox_CanUndo).

-doc "Copies the selected text to the clipboard.".
-spec copy(This) -> 'ok' when
	This::wxComboBox().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Copy).

-doc "Copies the selected text to the clipboard and removes it from the control.".
-spec cut(This) -> 'ok' when
	This::wxComboBox().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Cut).

-doc """
Same as `wxTextCtrl:getInsertionPoint/1`.

Note: Under wxMSW, this function always returns 0 if the combobox doesn't have the focus.
""".
-spec getInsertionPoint(This) -> integer() when
	This::wxComboBox().
getInsertionPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetInsertionPoint),
  wxe_util:rec(?wxComboBox_GetInsertionPoint).

-doc """
Returns the zero based index of the last position in the text control, which is equal to
the number of characters in the control.
""".
-spec getLastPosition(This) -> integer() when
	This::wxComboBox().
getLastPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetLastPosition),
  wxe_util:rec(?wxComboBox_GetLastPosition).

-doc """
Gets the contents of the control.

Notice that for a multiline text control, the lines will be separated by (Unix-style) `\n`
characters, even under Windows where they are separated by a `\r\n` sequence in the
native control.
""".
-spec getValue(This) -> unicode:charlist() when
	This::wxComboBox().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetValue),
  wxe_util:rec(?wxComboBox_GetValue).

-doc "Pastes text from the clipboard to the text item.".
-spec paste(This) -> 'ok' when
	This::wxComboBox().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Paste).

-doc """
If there is a redo facility and the last operation can be redone, redoes the last
operation.

Does nothing if there is no redo facility.
""".
-spec redo(This) -> 'ok' when
	This::wxComboBox().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Redo).

-doc """
Replaces the text starting at the first position up to (but not including) the character
at the last position with the given text.

This function puts the current insertion point position at `to` as a side effect.
""".
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,From,To,Value)
 when is_integer(From),is_integer(To),?is_chardata(Value) ->
  ?CLASS(ThisT,wxComboBox),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,From,To,Value_UC,?get_env(),?wxComboBox_Replace).

-doc """
Removes the text starting at the first given position up to (but not including) the
character at the last position.

This function puts the current insertion point position at `to` as a side effect.
""".
-spec remove(This, From, To) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxComboBox_Remove).

-doc "Sets the insertion point at the given position.".
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxComboBox(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxComboBox_SetInsertionPoint).

-doc """
Sets the insertion point at the end of the text control.

This is equivalent to calling `setInsertionPoint/2` with `getLastPosition/1` argument.
""".
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxComboBox().
setInsertionPointEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_SetInsertionPointEnd).

-doc """
Sets the selection to the given item `n` or removes the selection entirely if `n` == `wxNOT\_FOUND`.

Note that this does not cause any command events to be emitted nor does it deselect any
other items in the controls which support multiple selections.

See:
* `wxControlWithItems:setString/3`

* `wxControlWithItems:setStringSelection/2`
""".
-spec setSelection(This, N) -> 'ok' when
	This::wxComboBox(), N::integer().
setSelection(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxComboBox_SetSelection_1).

-doc "Same as `wxTextCtrl:setSelection/3`.".
-spec setSelection(This, From, To) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxComboBox_SetSelection_2).

-doc """
Sets the text for the combobox text field.

For normal, editable comboboxes with a text entry field calling this method will generate
a `wxEVT_TEXT` event, consistently with `wxTextCtrl:setValue/2` behaviour, use `wxTextCtrl:changeValue/2` if this is undesirable.

For controls with `wxCB_READONLY` style the method behaves somewhat differently: the
string must be in the combobox choices list (the check for this is case-insensitive) and `wxEVT_TEXT`
is `not` generated in this case.
""".
-spec setValue(This, Text) -> 'ok' when
	This::wxComboBox(), Text::unicode:chardata().
setValue(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxComboBox),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxComboBox_SetValue).

-doc """
If there is an undo facility and the last operation can be undone, undoes the last
operation.

Does nothing if there is no undo facility.
""".
-spec undo(This) -> 'ok' when
	This::wxComboBox().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Undo).

-doc "Destroys the object".
-spec destroy(This::wxComboBox()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxComboBox),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControlWithItems
-doc false.
setStringSelection(This,String) -> wxControlWithItems:setStringSelection(This,String).
-doc false.
setString(This,N,String) -> wxControlWithItems:setString(This,N,String).
-doc false.
select(This,N) -> wxControlWithItems:select(This,N).
-doc false.
isEmpty(This) -> wxControlWithItems:isEmpty(This).
-doc false.
insertStrings(This,Items,Pos,ClientsData) -> wxControlWithItems:insertStrings(This,Items,Pos,ClientsData).
-doc false.
insertStrings(This,Items,Pos) -> wxControlWithItems:insertStrings(This,Items,Pos).
-doc false.
insert(This,Item,Pos,ClientData) -> wxControlWithItems:insert(This,Item,Pos,ClientData).
-doc false.
insert(This,Item,Pos) -> wxControlWithItems:insert(This,Item,Pos).
-doc false.
getStringSelection(This) -> wxControlWithItems:getStringSelection(This).
-doc false.
getString(This,N) -> wxControlWithItems:getString(This,N).
-doc false.
getSelection(This) -> wxControlWithItems:getSelection(This).
-doc false.
getCount(This) -> wxControlWithItems:getCount(This).
-doc false.
setClientData(This,N,Data) -> wxControlWithItems:setClientData(This,N,Data).
-doc false.
getClientData(This,N) -> wxControlWithItems:getClientData(This,N).
-doc false.
findString(This,String, Options) -> wxControlWithItems:findString(This,String, Options).
-doc false.
findString(This,String) -> wxControlWithItems:findString(This,String).
-doc false.
delete(This,N) -> wxControlWithItems:delete(This,N).
-doc false.
clear(This) -> wxControlWithItems:clear(This).
-doc false.
appendStrings(This,Items,ClientsData) -> wxControlWithItems:appendStrings(This,Items,ClientsData).
-doc false.
appendStrings(This,Items) -> wxControlWithItems:appendStrings(This,Items).
-doc false.
append(This,Item,ClientData) -> wxControlWithItems:append(This,Item,ClientData).
-doc false.
append(This,Item) -> wxControlWithItems:append(This,Item).
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
