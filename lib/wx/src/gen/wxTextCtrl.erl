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

-module(wxTextCtrl).
-include("wxe.hrl").
-export([appendText/2,canCopy/1,canCut/1,canPaste/1,canRedo/1,canUndo/1,changeValue/2,
  clear/1,copy/1,create/3,create/4,cut/1,destroy/1,discardEdits/1,emulateKeyPress/2,
  getDefaultStyle/1,getInsertionPoint/1,getLastPosition/1,getLineLength/2,
  getLineText/2,getNumberOfLines/1,getRange/3,getSelection/1,getStringSelection/1,
  getStyle/3,getValue/1,isEditable/1,isModified/1,isMultiLine/1,isSingleLine/1,
  loadFile/2,loadFile/3,markDirty/1,new/0,new/2,new/3,paste/1,positionToXY/2,
  redo/1,remove/3,replace/4,saveFile/1,saveFile/2,setDefaultStyle/2,setEditable/2,
  setInsertionPoint/2,setInsertionPointEnd/1,setMaxLength/2,setSelection/3,
  setStyle/4,setValue/2,showPosition/2,undo/1,writeText/2,xYToPosition/3]).

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
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxTextCtrl() :: wx:wx_object().
-export_type([wxTextCtrl/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl">external documentation</a>.
-spec new() -> wxTextCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTextCtrl_new_0),
  wxe_util:rec(?wxTextCtrl_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxTextCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl">external documentation</a>.
-spec new(Parent, Id, [Option]) -> wxTextCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'value', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({value, Value}) ->   Value_UC = unicode:characters_to_binary(Value),{value,Value_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxTextCtrl_new_3),
  wxe_util:rec(?wxTextCtrl_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlappendtext">external documentation</a>.
-spec appendText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
appendText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_AppendText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancopy">external documentation</a>.
-spec canCopy(This) -> boolean() when
	This::wxTextCtrl().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCopy),
  wxe_util:rec(?wxTextCtrl_CanCopy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancut">external documentation</a>.
-spec canCut(This) -> boolean() when
	This::wxTextCtrl().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCut),
  wxe_util:rec(?wxTextCtrl_CanCut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanpaste">external documentation</a>.
-spec canPaste(This) -> boolean() when
	This::wxTextCtrl().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanPaste),
  wxe_util:rec(?wxTextCtrl_CanPaste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanredo">external documentation</a>.
-spec canRedo(This) -> boolean() when
	This::wxTextCtrl().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanRedo),
  wxe_util:rec(?wxTextCtrl_CanRedo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanundo">external documentation</a>.
-spec canUndo(This) -> boolean() when
	This::wxTextCtrl().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanUndo),
  wxe_util:rec(?wxTextCtrl_CanUndo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxTextCtrl().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcopy">external documentation</a>.
-spec copy(This) -> 'ok' when
	This::wxTextCtrl().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Copy).

%% @equiv create(This,Parent,Id, [])
-spec create(This, Parent, Id) -> boolean() when
	This::wxTextCtrl(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcreate">external documentation</a>.
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxTextCtrl(), Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'value', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({value, Value}) ->   Value_UC = unicode:characters_to_binary(Value),{value,Value_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxTextCtrl_Create),
  wxe_util:rec(?wxTextCtrl_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcut">external documentation</a>.
-spec cut(This) -> 'ok' when
	This::wxTextCtrl().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Cut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrldiscardedits">external documentation</a>.
-spec discardEdits(This) -> 'ok' when
	This::wxTextCtrl().
discardEdits(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_DiscardEdits).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlchangevalue">external documentation</a>.
-spec changeValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
changeValue(#wx_ref{type=ThisT}=This,Value)
 when ?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Value_UC,?get_env(),?wxTextCtrl_ChangeValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlemulatekeypress">external documentation</a>.
-spec emulateKeyPress(This, Event) -> boolean() when
	This::wxTextCtrl(), Event::wxKeyEvent:wxKeyEvent().
emulateKeyPress(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxTextCtrl_EmulateKeyPress),
  wxe_util:rec(?wxTextCtrl_EmulateKeyPress).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetdefaultstyle">external documentation</a>.
-spec getDefaultStyle(This) -> wxTextAttr:wxTextAttr() when
	This::wxTextCtrl().
getDefaultStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetDefaultStyle),
  wxe_util:rec(?wxTextCtrl_GetDefaultStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetinsertionpoint">external documentation</a>.
-spec getInsertionPoint(This) -> integer() when
	This::wxTextCtrl().
getInsertionPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetInsertionPoint),
  wxe_util:rec(?wxTextCtrl_GetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlastposition">external documentation</a>.
-spec getLastPosition(This) -> integer() when
	This::wxTextCtrl().
getLastPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetLastPosition),
  wxe_util:rec(?wxTextCtrl_GetLastPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinelength">external documentation</a>.
-spec getLineLength(This, LineNo) -> integer() when
	This::wxTextCtrl(), LineNo::integer().
getLineLength(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineLength),
  wxe_util:rec(?wxTextCtrl_GetLineLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinetext">external documentation</a>.
-spec getLineText(This, LineNo) -> unicode:charlist() when
	This::wxTextCtrl(), LineNo::integer().
getLineText(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineText),
  wxe_util:rec(?wxTextCtrl_GetLineText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetnumberoflines">external documentation</a>.
-spec getNumberOfLines(This) -> integer() when
	This::wxTextCtrl().
getNumberOfLines(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetNumberOfLines),
  wxe_util:rec(?wxTextCtrl_GetNumberOfLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetrange">external documentation</a>.
-spec getRange(This, From, To) -> unicode:charlist() when
	This::wxTextCtrl(), From::integer(), To::integer().
getRange(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_GetRange),
  wxe_util:rec(?wxTextCtrl_GetRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetselection">external documentation</a>.
-spec getSelection(This) -> {From::integer(), To::integer()} when
	This::wxTextCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetSelection),
  wxe_util:rec(?wxTextCtrl_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstringselection">external documentation</a>.
-spec getStringSelection(This) -> unicode:charlist() when
	This::wxTextCtrl().
getStringSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetStringSelection),
  wxe_util:rec(?wxTextCtrl_GetStringSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstyle">external documentation</a>.
-spec getStyle(This, Position, Style) -> boolean() when
	This::wxTextCtrl(), Position::integer(), Style::wxTextAttr:wxTextAttr().
getStyle(#wx_ref{type=ThisT}=This,Position,#wx_ref{type=StyleT}=Style)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Position,Style,?get_env(),?wxTextCtrl_GetStyle),
  wxe_util:rec(?wxTextCtrl_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetvalue">external documentation</a>.
-spec getValue(This) -> unicode:charlist() when
	This::wxTextCtrl().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetValue),
  wxe_util:rec(?wxTextCtrl_GetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrliseditable">external documentation</a>.
-spec isEditable(This) -> boolean() when
	This::wxTextCtrl().
isEditable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsEditable),
  wxe_util:rec(?wxTextCtrl_IsEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismodified">external documentation</a>.
-spec isModified(This) -> boolean() when
	This::wxTextCtrl().
isModified(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsModified),
  wxe_util:rec(?wxTextCtrl_IsModified).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismultiline">external documentation</a>.
-spec isMultiLine(This) -> boolean() when
	This::wxTextCtrl().
isMultiLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsMultiLine),
  wxe_util:rec(?wxTextCtrl_IsMultiLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlissingleline">external documentation</a>.
-spec isSingleLine(This) -> boolean() when
	This::wxTextCtrl().
isSingleLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsSingleLine),
  wxe_util:rec(?wxTextCtrl_IsSingleLine).

%% @equiv loadFile(This,Filename, [])
-spec loadFile(This, Filename) -> boolean() when
	This::wxTextCtrl(), Filename::unicode:chardata().

loadFile(This,Filename)
 when is_record(This, wx_ref),?is_chardata(Filename) ->
  loadFile(This,Filename, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlloadfile">external documentation</a>.
-spec loadFile(This, Filename, [Option]) -> boolean() when
	This::wxTextCtrl(), Filename::unicode:chardata(),
	Option :: {'fileType', integer()}.
loadFile(#wx_ref{type=ThisT}=This,Filename, Options)
 when ?is_chardata(Filename),is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  Filename_UC = unicode:characters_to_binary(Filename),
  MOpts = fun({fileType, _fileType} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Filename_UC, Opts,?get_env(),?wxTextCtrl_LoadFile),
  wxe_util:rec(?wxTextCtrl_LoadFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlmarkdirty">external documentation</a>.
-spec markDirty(This) -> 'ok' when
	This::wxTextCtrl().
markDirty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_MarkDirty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpaste">external documentation</a>.
-spec paste(This) -> 'ok' when
	This::wxTextCtrl().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Paste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpositiontoxy">external documentation</a>.
-spec positionToXY(This, Pos) -> Result when
	Result ::{Res ::boolean(), X::integer(), Y::integer()},
	This::wxTextCtrl(), Pos::integer().
positionToXY(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_PositionToXY),
  wxe_util:rec(?wxTextCtrl_PositionToXY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlredo">external documentation</a>.
-spec redo(This) -> 'ok' when
	This::wxTextCtrl().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Redo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlremove">external documentation</a>.
-spec remove(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_Remove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlreplace">external documentation</a>.
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,From,To,Value)
 when is_integer(From),is_integer(To),?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,From,To,Value_UC,?get_env(),?wxTextCtrl_Replace).

%% @equiv saveFile(This, [])
-spec saveFile(This) -> boolean() when
	This::wxTextCtrl().

saveFile(This)
 when is_record(This, wx_ref) ->
  saveFile(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsavefile">external documentation</a>.
-spec saveFile(This, [Option]) -> boolean() when
	This::wxTextCtrl(),
	Option :: {'file', unicode:chardata()}
		 | {'fileType', integer()}.
saveFile(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  MOpts = fun({file, File}) ->   File_UC = unicode:characters_to_binary(File),{file,File_UC};
          ({fileType, _fileType} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxTextCtrl_SaveFile),
  wxe_util:rec(?wxTextCtrl_SaveFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetdefaultstyle">external documentation</a>.
-spec setDefaultStyle(This, Style) -> boolean() when
	This::wxTextCtrl(), Style::wxTextAttr:wxTextAttr().
setDefaultStyle(#wx_ref{type=ThisT}=This,#wx_ref{type=StyleT}=Style) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxTextCtrl_SetDefaultStyle),
  wxe_util:rec(?wxTextCtrl_SetDefaultStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlseteditable">external documentation</a>.
-spec setEditable(This, Editable) -> 'ok' when
	This::wxTextCtrl(), Editable::boolean().
setEditable(#wx_ref{type=ThisT}=This,Editable)
 when is_boolean(Editable) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Editable,?get_env(),?wxTextCtrl_SetEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpoint">external documentation</a>.
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_SetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpointend">external documentation</a>.
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxTextCtrl().
setInsertionPointEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_SetInsertionPointEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetmaxlength">external documentation</a>.
-spec setMaxLength(This, Len) -> 'ok' when
	This::wxTextCtrl(), Len::integer().
setMaxLength(#wx_ref{type=ThisT}=This,Len)
 when is_integer(Len) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Len,?get_env(),?wxTextCtrl_SetMaxLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetselection">external documentation</a>.
-spec setSelection(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetstyle">external documentation</a>.
-spec setStyle(This, Start, End, Style) -> boolean() when
	This::wxTextCtrl(), Start::integer(), End::integer(), Style::wxTextAttr:wxTextAttr().
setStyle(#wx_ref{type=ThisT}=This,Start,End,#wx_ref{type=StyleT}=Style)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Start,End,Style,?get_env(),?wxTextCtrl_SetStyle),
  wxe_util:rec(?wxTextCtrl_SetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetvalue">external documentation</a>.
-spec setValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
setValue(#wx_ref{type=ThisT}=This,Value)
 when ?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Value_UC,?get_env(),?wxTextCtrl_SetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlshowposition">external documentation</a>.
-spec showPosition(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
showPosition(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_ShowPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlundo">external documentation</a>.
-spec undo(This) -> 'ok' when
	This::wxTextCtrl().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Undo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwritetext">external documentation</a>.
-spec writeText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
writeText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_WriteText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlxytoposition">external documentation</a>.
-spec xYToPosition(This, X, Y) -> integer() when
	This::wxTextCtrl(), X::integer(), Y::integer().
xYToPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxTextCtrl_XYToPosition),
  wxe_util:rec(?wxTextCtrl_XYToPosition).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTextCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextCtrl),
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
