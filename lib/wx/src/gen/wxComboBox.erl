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

-module(wxComboBox).
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
%% @hidden
parent_class(wxControlWithItems) -> true;
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxwxcombobox">external documentation</a>.
-spec new() -> wxComboBox().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxComboBox_new_0),
  wxe_util:rec(?wxComboBox_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxComboBox() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxwxcombobox">external documentation</a>.
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

%% @equiv create(This,Parent,Id,Value,Pos,Size,Choices, [])
-spec create(This, Parent, Id, Value, Pos, Size, Choices) -> boolean() when
	This::wxComboBox(), Parent::wxWindow:wxWindow(), Id::integer(), Value::unicode:chardata(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}, Choices::[unicode:chardata()].

create(This,Parent,Id,Value,{PosX,PosY} = Pos,{SizeW,SizeH} = Size,Choices)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Value),is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH),is_list(Choices) ->
  create(This,Parent,Id,Value,Pos,Size,Choices, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcreate">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcancopy">external documentation</a>.
-spec canCopy(This) -> boolean() when
	This::wxComboBox().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanCopy),
  wxe_util:rec(?wxComboBox_CanCopy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcancut">external documentation</a>.
-spec canCut(This) -> boolean() when
	This::wxComboBox().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanCut),
  wxe_util:rec(?wxComboBox_CanCut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcanpaste">external documentation</a>.
-spec canPaste(This) -> boolean() when
	This::wxComboBox().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanPaste),
  wxe_util:rec(?wxComboBox_CanPaste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcanredo">external documentation</a>.
-spec canRedo(This) -> boolean() when
	This::wxComboBox().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanRedo),
  wxe_util:rec(?wxComboBox_CanRedo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcanundo">external documentation</a>.
-spec canUndo(This) -> boolean() when
	This::wxComboBox().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_CanUndo),
  wxe_util:rec(?wxComboBox_CanUndo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcopy">external documentation</a>.
-spec copy(This) -> 'ok' when
	This::wxComboBox().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Copy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxcut">external documentation</a>.
-spec cut(This) -> 'ok' when
	This::wxComboBox().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Cut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxgetinsertionpoint">external documentation</a>.
-spec getInsertionPoint(This) -> integer() when
	This::wxComboBox().
getInsertionPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetInsertionPoint),
  wxe_util:rec(?wxComboBox_GetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxgetlastposition">external documentation</a>.
-spec getLastPosition(This) -> integer() when
	This::wxComboBox().
getLastPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetLastPosition),
  wxe_util:rec(?wxComboBox_GetLastPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxgetvalue">external documentation</a>.
-spec getValue(This) -> unicode:charlist() when
	This::wxComboBox().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_GetValue),
  wxe_util:rec(?wxComboBox_GetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxpaste">external documentation</a>.
-spec paste(This) -> 'ok' when
	This::wxComboBox().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Paste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxredo">external documentation</a>.
-spec redo(This) -> 'ok' when
	This::wxComboBox().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Redo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxreplace">external documentation</a>.
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,From,To,Value)
 when is_integer(From),is_integer(To),?is_chardata(Value) ->
  ?CLASS(ThisT,wxComboBox),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,From,To,Value_UC,?get_env(),?wxComboBox_Replace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxremove">external documentation</a>.
-spec remove(This, From, To) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxComboBox_Remove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxsetinsertionpoint">external documentation</a>.
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxComboBox(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxComboBox_SetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxsetinsertionpointend">external documentation</a>.
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxComboBox().
setInsertionPointEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_SetInsertionPointEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxsetselection">external documentation</a>.
-spec setSelection(This, N) -> 'ok' when
	This::wxComboBox(), N::integer().
setSelection(#wx_ref{type=ThisT}=This,N)
 when is_integer(N) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,N,?get_env(),?wxComboBox_SetSelection_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxsetselection">external documentation</a>.
-spec setSelection(This, From, To) -> 'ok' when
	This::wxComboBox(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxComboBox_SetSelection_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxsetvalue">external documentation</a>.
-spec setValue(This, Text) -> 'ok' when
	This::wxComboBox(), Text::unicode:chardata().
setValue(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxComboBox),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxComboBox_SetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcombobox.html#wxcomboboxundo">external documentation</a>.
-spec undo(This) -> 'ok' when
	This::wxComboBox().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxComboBox),
  wxe_util:queue_cmd(This,?get_env(),?wxComboBox_Undo).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxComboBox()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxComboBox),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControlWithItems
%% @hidden
setStringSelection(This,String) -> wxControlWithItems:setStringSelection(This,String).
%% @hidden
setString(This,N,String) -> wxControlWithItems:setString(This,N,String).
%% @hidden
select(This,N) -> wxControlWithItems:select(This,N).
%% @hidden
isEmpty(This) -> wxControlWithItems:isEmpty(This).
%% @hidden
insertStrings(This,Items,Pos,ClientsData) -> wxControlWithItems:insertStrings(This,Items,Pos,ClientsData).
%% @hidden
insertStrings(This,Items,Pos) -> wxControlWithItems:insertStrings(This,Items,Pos).
%% @hidden
insert(This,Item,Pos,ClientData) -> wxControlWithItems:insert(This,Item,Pos,ClientData).
%% @hidden
insert(This,Item,Pos) -> wxControlWithItems:insert(This,Item,Pos).
%% @hidden
getStringSelection(This) -> wxControlWithItems:getStringSelection(This).
%% @hidden
getString(This,N) -> wxControlWithItems:getString(This,N).
%% @hidden
getSelection(This) -> wxControlWithItems:getSelection(This).
%% @hidden
getCount(This) -> wxControlWithItems:getCount(This).
%% @hidden
setClientData(This,N,Data) -> wxControlWithItems:setClientData(This,N,Data).
%% @hidden
getClientData(This,N) -> wxControlWithItems:getClientData(This,N).
%% @hidden
findString(This,String, Options) -> wxControlWithItems:findString(This,String, Options).
%% @hidden
findString(This,String) -> wxControlWithItems:findString(This,String).
%% @hidden
delete(This,N) -> wxControlWithItems:delete(This,N).
%% @hidden
clear(This) -> wxControlWithItems:clear(This).
%% @hidden
appendStrings(This,Items,ClientsData) -> wxControlWithItems:appendStrings(This,Items,ClientsData).
%% @hidden
appendStrings(This,Items) -> wxControlWithItems:appendStrings(This,Items).
%% @hidden
append(This,Item,ClientData) -> wxControlWithItems:append(This,Item,ClientData).
%% @hidden
append(This,Item) -> wxControlWithItems:append(This,Item).
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
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
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
