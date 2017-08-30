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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html">wxTextCtrl</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxTextCtrl().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

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
  setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,setClientSize/2,
  setClientSize/3,setContainingSizer/2,setCursor/2,setDoubleBuffered/2,
  setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,
  setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-export_type([wxTextCtrl/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxTextCtrl() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl">external documentation</a>.
-spec new() -> wxTextCtrl().
new() ->
  wxe_util:construct(?wxTextCtrl_new_0,
  <<>>).

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
new(#wx_ref{type=ParentT,ref=ParentRef},Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({value, Value}, Acc) ->   Value_UC = unicode:characters_to_binary([Value,0]),[<<1:32/?UI,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((0+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxTextCtrl_new_3,
  <<ParentRef:32/?UI,Id:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlappendtext">external documentation</a>.
-spec appendText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
appendText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxTextCtrl_AppendText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancopy">external documentation</a>.
-spec canCopy(This) -> boolean() when
	This::wxTextCtrl().
canCopy(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_CanCopy,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancut">external documentation</a>.
-spec canCut(This) -> boolean() when
	This::wxTextCtrl().
canCut(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_CanCut,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanpaste">external documentation</a>.
-spec canPaste(This) -> boolean() when
	This::wxTextCtrl().
canPaste(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_CanPaste,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanredo">external documentation</a>.
-spec canRedo(This) -> boolean() when
	This::wxTextCtrl().
canRedo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_CanRedo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanundo">external documentation</a>.
-spec canUndo(This) -> boolean() when
	This::wxTextCtrl().
canUndo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_CanUndo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxTextCtrl().
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Clear,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcopy">external documentation</a>.
-spec copy(This) -> 'ok' when
	This::wxTextCtrl().
copy(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Copy,
  <<ThisRef:32/?UI>>).

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
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({value, Value}, Acc) ->   Value_UC = unicode:characters_to_binary([Value,0]),[<<1:32/?UI,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((0+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          ({validator, #wx_ref{type=ValidatorT,ref=ValidatorRef}}, Acc) ->   ?CLASS(ValidatorT,wx),[<<5:32/?UI,ValidatorRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTextCtrl_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI,Id:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcut">external documentation</a>.
-spec cut(This) -> 'ok' when
	This::wxTextCtrl().
cut(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Cut,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrldiscardedits">external documentation</a>.
-spec discardEdits(This) -> 'ok' when
	This::wxTextCtrl().
discardEdits(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_DiscardEdits,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlchangevalue">external documentation</a>.
-spec changeValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
changeValue(#wx_ref{type=ThisT,ref=ThisRef},Value)
 when is_list(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:cast(?wxTextCtrl_ChangeValue,
  <<ThisRef:32/?UI,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((0+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlemulatekeypress">external documentation</a>.
-spec emulateKeyPress(This, Event) -> boolean() when
	This::wxTextCtrl(), Event::wxKeyEvent:wxKeyEvent().
emulateKeyPress(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EventT,ref=EventRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:call(?wxTextCtrl_EmulateKeyPress,
  <<ThisRef:32/?UI,EventRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetdefaultstyle">external documentation</a>.
-spec getDefaultStyle(This) -> wxTextAttr:wxTextAttr() when
	This::wxTextCtrl().
getDefaultStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetDefaultStyle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetinsertionpoint">external documentation</a>.
-spec getInsertionPoint(This) -> integer() when
	This::wxTextCtrl().
getInsertionPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetInsertionPoint,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlastposition">external documentation</a>.
-spec getLastPosition(This) -> integer() when
	This::wxTextCtrl().
getLastPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetLastPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinelength">external documentation</a>.
-spec getLineLength(This, LineNo) -> integer() when
	This::wxTextCtrl(), LineNo::integer().
getLineLength(#wx_ref{type=ThisT,ref=ThisRef},LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetLineLength,
  <<ThisRef:32/?UI,LineNo:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinetext">external documentation</a>.
-spec getLineText(This, LineNo) -> unicode:charlist() when
	This::wxTextCtrl(), LineNo::integer().
getLineText(#wx_ref{type=ThisT,ref=ThisRef},LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetLineText,
  <<ThisRef:32/?UI,LineNo:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetnumberoflines">external documentation</a>.
-spec getNumberOfLines(This) -> integer() when
	This::wxTextCtrl().
getNumberOfLines(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetNumberOfLines,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetrange">external documentation</a>.
-spec getRange(This, From, To) -> unicode:charlist() when
	This::wxTextCtrl(), From::integer(), To::integer().
getRange(#wx_ref{type=ThisT,ref=ThisRef},From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetRange,
  <<ThisRef:32/?UI,From:32/?UI,To:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetselection">external documentation</a>.
-spec getSelection(This) -> {From::integer(), To::integer()} when
	This::wxTextCtrl().
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetSelection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstringselection">external documentation</a>.
-spec getStringSelection(This) -> unicode:charlist() when
	This::wxTextCtrl().
getStringSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetStringSelection,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstyle">external documentation</a>.
-spec getStyle(This, Position, Style) -> boolean() when
	This::wxTextCtrl(), Position::integer(), Style::wxTextAttr:wxTextAttr().
getStyle(#wx_ref{type=ThisT,ref=ThisRef},Position,#wx_ref{type=StyleT,ref=StyleRef})
 when is_integer(Position) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:call(?wxTextCtrl_GetStyle,
  <<ThisRef:32/?UI,Position:32/?UI,StyleRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetvalue">external documentation</a>.
-spec getValue(This) -> unicode:charlist() when
	This::wxTextCtrl().
getValue(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_GetValue,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrliseditable">external documentation</a>.
-spec isEditable(This) -> boolean() when
	This::wxTextCtrl().
isEditable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_IsEditable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismodified">external documentation</a>.
-spec isModified(This) -> boolean() when
	This::wxTextCtrl().
isModified(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_IsModified,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismultiline">external documentation</a>.
-spec isMultiLine(This) -> boolean() when
	This::wxTextCtrl().
isMultiLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_IsMultiLine,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlissingleline">external documentation</a>.
-spec isSingleLine(This) -> boolean() when
	This::wxTextCtrl().
isSingleLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_IsSingleLine,
  <<ThisRef:32/?UI>>).

%% @equiv loadFile(This,File, [])
-spec loadFile(This, File) -> boolean() when
	This::wxTextCtrl(), File::unicode:chardata().

loadFile(This,File)
 when is_record(This, wx_ref),is_list(File) ->
  loadFile(This,File, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlloadfile">external documentation</a>.
-spec loadFile(This, File, [Option]) -> boolean() when
	This::wxTextCtrl(), File::unicode:chardata(),
	Option :: {'fileType', integer()}.
loadFile(#wx_ref{type=ThisT,ref=ThisRef},File, Options)
 when is_list(File),is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  File_UC = unicode:characters_to_binary([File,0]),
  MOpts = fun({fileType, FileType}, Acc) -> [<<1:32/?UI,FileType:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTextCtrl_LoadFile,
  <<ThisRef:32/?UI,(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((0+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlmarkdirty">external documentation</a>.
-spec markDirty(This) -> 'ok' when
	This::wxTextCtrl().
markDirty(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_MarkDirty,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpaste">external documentation</a>.
-spec paste(This) -> 'ok' when
	This::wxTextCtrl().
paste(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Paste,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpositiontoxy">external documentation</a>.
-spec positionToXY(This, Pos) -> Result when
	Result ::{Res ::boolean(), X::integer(), Y::integer()},
	This::wxTextCtrl(), Pos::integer().
positionToXY(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_PositionToXY,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlredo">external documentation</a>.
-spec redo(This) -> 'ok' when
	This::wxTextCtrl().
redo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Redo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlremove">external documentation</a>.
-spec remove(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT,ref=ThisRef},From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Remove,
  <<ThisRef:32/?UI,From:32/?UI,To:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlreplace">external documentation</a>.
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT,ref=ThisRef},From,To,Value)
 when is_integer(From),is_integer(To),is_list(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:cast(?wxTextCtrl_Replace,
  <<ThisRef:32/?UI,From:32/?UI,To:32/?UI,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((0+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

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
saveFile(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTextCtrl),
  MOpts = fun({file, File}, Acc) ->   File_UC = unicode:characters_to_binary([File,0]),[<<1:32/?UI,(byte_size(File_UC)):32/?UI,(File_UC)/binary, 0:(((8- ((0+byte_size(File_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({fileType, FileType}, Acc) -> [<<2:32/?UI,FileType:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTextCtrl_SaveFile,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetdefaultstyle">external documentation</a>.
-spec setDefaultStyle(This, Style) -> boolean() when
	This::wxTextCtrl(), Style::wxTextAttr:wxTextAttr().
setDefaultStyle(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=StyleT,ref=StyleRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:call(?wxTextCtrl_SetDefaultStyle,
  <<ThisRef:32/?UI,StyleRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlseteditable">external documentation</a>.
-spec setEditable(This, Editable) -> 'ok' when
	This::wxTextCtrl(), Editable::boolean().
setEditable(#wx_ref{type=ThisT,ref=ThisRef},Editable)
 when is_boolean(Editable) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_SetEditable,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Editable)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpoint">external documentation</a>.
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_SetInsertionPoint,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpointend">external documentation</a>.
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxTextCtrl().
setInsertionPointEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_SetInsertionPointEnd,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetmaxlength">external documentation</a>.
-spec setMaxLength(This, Len) -> 'ok' when
	This::wxTextCtrl(), Len::integer().
setMaxLength(#wx_ref{type=ThisT,ref=ThisRef},Len)
 when is_integer(Len) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_SetMaxLength,
  <<ThisRef:32/?UI,Len:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetselection">external documentation</a>.
-spec setSelection(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT,ref=ThisRef},From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_SetSelection,
  <<ThisRef:32/?UI,From:32/?UI,To:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetstyle">external documentation</a>.
-spec setStyle(This, Start, End, Style) -> boolean() when
	This::wxTextCtrl(), Start::integer(), End::integer(), Style::wxTextAttr:wxTextAttr().
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Start,End,#wx_ref{type=StyleT,ref=StyleRef})
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:call(?wxTextCtrl_SetStyle,
  <<ThisRef:32/?UI,Start:32/?UI,End:32/?UI,StyleRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetvalue">external documentation</a>.
-spec setValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
setValue(#wx_ref{type=ThisT,ref=ThisRef},Value)
 when is_list(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:cast(?wxTextCtrl_SetValue,
  <<ThisRef:32/?UI,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((0+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlshowposition">external documentation</a>.
-spec showPosition(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
showPosition(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_ShowPosition,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlundo">external documentation</a>.
-spec undo(This) -> 'ok' when
	This::wxTextCtrl().
undo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:cast(?wxTextCtrl_Undo,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwritetext">external documentation</a>.
-spec writeText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
writeText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxTextCtrl_WriteText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlxytoposition">external documentation</a>.
-spec xYToPosition(This, X, Y) -> integer() when
	This::wxTextCtrl(), X::integer(), Y::integer().
xYToPosition(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:call(?wxTextCtrl_XYToPosition,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTextCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextCtrl),
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
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
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
