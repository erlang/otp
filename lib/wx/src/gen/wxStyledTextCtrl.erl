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

-module(wxStyledTextCtrl).
-include("wxe.hrl").
-export([addText/2,addTextRaw/2,addTextRaw/3,allocate/2,appendText/2,appendTextRaw/2,
  appendTextRaw/3,autoCompActive/1,autoCompCancel/1,autoCompComplete/1,
  autoCompGetAutoHide/1,autoCompGetCancelAtStart/1,autoCompGetChooseSingle/1,
  autoCompGetCurrent/1,autoCompGetDropRestOfWord/1,autoCompGetIgnoreCase/1,
  autoCompGetMaxHeight/1,autoCompGetMaxWidth/1,autoCompGetSeparator/1,
  autoCompGetTypeSeparator/1,autoCompPosStart/1,autoCompSelect/2,autoCompSetAutoHide/2,
  autoCompSetCancelAtStart/2,autoCompSetChooseSingle/2,autoCompSetDropRestOfWord/2,
  autoCompSetFillUps/2,autoCompSetIgnoreCase/2,autoCompSetMaxHeight/2,
  autoCompSetMaxWidth/2,autoCompSetSeparator/2,autoCompSetTypeSeparator/2,
  autoCompShow/3,autoCompStops/2,backTab/1,beginUndoAction/1,braceBadLight/2,
  braceHighlight/3,braceMatch/2,callTipActive/1,callTipCancel/1,callTipPosAtStart/1,
  callTipSetBackground/2,callTipSetForeground/2,callTipSetForegroundHighlight/2,
  callTipSetHighlight/3,callTipShow/3,callTipUseStyle/2,canPaste/1,
  canRedo/1,canUndo/1,cancel/1,charLeft/1,charLeftExtend/1,charLeftRectExtend/1,
  charRight/1,charRightExtend/1,charRightRectExtend/1,chooseCaretX/1,
  clear/1,clearAll/1,clearDocumentStyle/1,clearRegisteredImages/1,cmdKeyAssign/4,
  cmdKeyClear/3,cmdKeyClearAll/1,cmdKeyExecute/2,colourise/3,convertEOLs/2,
  copy/1,copyRange/3,copyText/3,create/2,create/3,cut/1,delLineLeft/1,
  delLineRight/1,delWordLeft/1,delWordRight/1,deleteBack/1,deleteBackNotLine/1,
  destroy/1,doDragOver/4,doDropText/4,docLineFromVisible/2,documentEnd/1,
  documentEndExtend/1,documentStart/1,documentStartExtend/1,editToggleOvertype/1,
  emptyUndoBuffer/1,endUndoAction/1,ensureCaretVisible/1,ensureVisible/2,
  ensureVisibleEnforcePolicy/2,findColumn/3,findText/4,findText/5,formFeed/1,
  formatRange/8,getAnchor/1,getBackSpaceUnIndents/1,getBufferedDraw/1,
  getCaretForeground/1,getCaretLineBackAlpha/1,getCaretLineBackground/1,
  getCaretLineVisible/1,getCaretPeriod/1,getCaretSticky/1,getCaretWidth/1,
  getCharAt/2,getCodePage/1,getColumn/2,getControlCharSymbol/1,getCurLine/1,
  getCurLineRaw/1,getCurrentLine/1,getCurrentPos/1,getEOLMode/1,getEdgeColour/1,
  getEdgeColumn/1,getEdgeMode/1,getEndAtLastLine/1,getEndStyled/1,getFirstVisibleLine/1,
  getFoldExpanded/2,getFoldLevel/2,getFoldParent/2,getHighlightGuide/1,
  getIndent/1,getIndentationGuides/1,getLastChild/3,getLastKeydownProcessed/1,
  getLayoutCache/1,getLength/1,getLexer/1,getLine/2,getLineCount/1,getLineEndPosition/2,
  getLineIndentPosition/2,getLineIndentation/2,getLineRaw/2,getLineState/2,
  getLineVisible/2,getMarginLeft/1,getMarginMask/2,getMarginRight/1,
  getMarginSensitive/2,getMarginType/2,getMarginWidth/2,getMaxLineState/1,
  getModEventMask/1,getModify/1,getMouseDownCaptures/1,getMouseDwellTime/1,
  getOvertype/1,getPasteConvertEndings/1,getPrintColourMode/1,getPrintMagnification/1,
  getPrintWrapMode/1,getProperty/2,getReadOnly/1,getSTCCursor/1,getSTCFocus/1,
  getScrollWidth/1,getSearchFlags/1,getSelAlpha/1,getSelectedText/1,
  getSelectedTextRaw/1,getSelection/1,getSelectionEnd/1,getSelectionMode/1,
  getSelectionStart/1,getStatus/1,getStyleAt/2,getStyleBits/1,getStyleBitsNeeded/1,
  getTabIndents/1,getTabWidth/1,getTargetEnd/1,getTargetStart/1,getText/1,
  getTextLength/1,getTextRange/3,getTextRangeRaw/3,getTextRaw/1,getTwoPhaseDraw/1,
  getUndoCollection/1,getUseAntiAliasing/1,getUseHorizontalScrollBar/1,
  getUseTabs/1,getUseVerticalScrollBar/1,getViewEOL/1,getViewWhiteSpace/1,
  getWrapMode/1,getWrapStartIndent/1,getWrapVisualFlags/1,getWrapVisualFlagsLocation/1,
  getXOffset/1,getZoom/1,gotoLine/2,gotoPos/2,hideLines/3,hideSelection/2,
  home/1,homeDisplay/1,homeDisplayExtend/1,homeExtend/1,homeRectExtend/1,
  homeWrapExtend/1,indicatorGetForeground/2,indicatorGetStyle/2,indicatorSetForeground/3,
  indicatorSetStyle/3,insertText/3,insertTextRaw/3,lineCopy/1,lineCut/1,
  lineDelete/1,lineDown/1,lineDownExtend/1,lineDownRectExtend/1,lineDuplicate/1,
  lineEnd/1,lineEndDisplay/1,lineEndDisplayExtend/1,lineEndExtend/1,
  lineEndRectExtend/1,lineEndWrap/1,lineEndWrapExtend/1,lineFromPosition/2,
  lineLength/2,lineScroll/3,lineScrollDown/1,lineScrollUp/1,lineTranspose/1,
  lineUp/1,lineUpExtend/1,lineUpRectExtend/1,linesJoin/1,linesOnScreen/1,
  linesSplit/2,loadFile/2,lowerCase/1,markerAdd/3,markerAddSet/3,markerDefine/3,
  markerDefine/4,markerDefineBitmap/3,markerDelete/3,markerDeleteAll/2,
  markerDeleteHandle/2,markerGet/2,markerLineFromHandle/2,markerNext/3,
  markerPrevious/3,markerSetAlpha/3,markerSetBackground/3,markerSetForeground/3,
  moveCaretInsideView/1,new/0,new/1,new/2,newLine/1,pageDown/1,pageDownExtend/1,
  pageDownRectExtend/1,pageUp/1,pageUpExtend/1,pageUpRectExtend/1,paraDownExtend/1,
  paraUp/1,paraUpExtend/1,paste/1,pointFromPosition/2,positionAfter/2,
  positionBefore/2,positionFromLine/2,positionFromPoint/2,positionFromPointClose/3,
  redo/1,registerImage/3,replaceSelection/2,replaceTarget/2,saveFile/2,
  scrollToColumn/2,scrollToLine/2,searchAnchor/1,searchInTarget/2,searchNext/3,
  searchPrev/3,selectAll/1,selectionDuplicate/1,selectionIsRectangle/1,
  setAnchor/2,setBackSpaceUnIndents/2,setBufferedDraw/2,setCaretForeground/2,
  setCaretLineBackAlpha/2,setCaretLineBackground/2,setCaretLineVisible/2,
  setCaretPeriod/2,setCaretSticky/2,setCaretWidth/2,setCharsDefault/1,
  setCodePage/2,setControlCharSymbol/2,setCurrentPos/2,setEOLMode/2,
  setEdgeColour/2,setEdgeColumn/2,setEdgeMode/2,setFoldExpanded/3,setFoldFlags/2,
  setFoldLevel/3,setFoldMarginColour/3,setFoldMarginHiColour/3,setHScrollBar/2,
  setHighlightGuide/2,setHotspotActiveBackground/3,setHotspotActiveForeground/3,
  setHotspotActiveUnderline/2,setHotspotSingleLine/2,setIndent/2,setIndentationGuides/2,
  setKeyWords/3,setLastKeydownProcessed/2,setLayoutCache/2,setLexer/2,
  setLexerLanguage/2,setLineIndentation/3,setLineState/3,setMarginLeft/2,
  setMarginMask/3,setMarginRight/2,setMarginSensitive/3,setMarginType/3,
  setMarginWidth/3,setMargins/3,setModEventMask/2,setMouseDownCaptures/2,
  setMouseDwellTime/2,setPasteConvertEndings/2,setPrintColourMode/2,
  setPrintMagnification/2,setProperty/3,setReadOnly/2,setSTCCursor/2,
  setSTCFocus/2,setSavePoint/1,setScrollWidth/2,setSearchFlags/2,setSelAlpha/2,
  setSelBackground/3,setSelForeground/3,setSelection/3,setSelectionEnd/2,
  setSelectionMode/2,setSelectionStart/2,setStatus/2,setStyleBytes/2,
  setStyling/3,setTabIndents/2,setTabWidth/2,setTargetEnd/2,setTargetStart/2,
  setText/2,setTextRaw/2,setTwoPhaseDraw/2,setUndoCollection/2,setUseHorizontalScrollBar/2,
  setUseTabs/2,setUseVerticalScrollBar/2,setVScrollBar/2,setViewEOL/2,
  setViewWhiteSpace/2,setVisiblePolicy/3,setWhitespaceBackground/3,
  setWhitespaceChars/2,setWhitespaceForeground/3,setWordChars/2,setWrapMode/2,
  setWrapStartIndent/2,setWrapVisualFlags/2,setWrapVisualFlagsLocation/2,
  setXCaretPolicy/3,setYCaretPolicy/3,setZoom/2,showLines/3,startRecord/1,
  startStyling/2,stopRecord/1,stutteredPageDown/1,stutteredPageDownExtend/1,
  stutteredPageUp/1,stutteredPageUpExtend/1,styleClearAll/1,styleResetDefault/1,
  styleSetBackground/3,styleSetBold/3,styleSetCase/3,styleSetCharacterSet/3,
  styleSetEOLFilled/3,styleSetFaceName/3,styleSetFont/3,styleSetFontAttr/7,
  styleSetFontAttr/8,styleSetFontEncoding/3,styleSetForeground/3,styleSetHotSpot/3,
  styleSetItalic/3,styleSetSize/3,styleSetSpec/3,styleSetUnderline/3,
  styleSetVisible/3,tab/1,targetFromSelection/1,textHeight/2,textWidth/3,
  toggleCaretSticky/1,toggleFold/2,undo/1,upperCase/1,usePopUp/2,userListShow/3,
  vCHome/1,vCHomeExtend/1,vCHomeRectExtend/1,vCHomeWrap/1,vCHomeWrapExtend/1,
  visibleFromDocLine/2,wordEndPosition/3,wordLeft/1,wordLeftEnd/1,wordLeftEndExtend/1,
  wordLeftExtend/1,wordPartLeft/1,wordPartLeftExtend/1,wordPartRight/1,
  wordPartRightExtend/1,wordRight/1,wordRightEnd/1,wordRightEndExtend/1,
  wordRightExtend/1,wordStartPosition/3,wrapCount/2,zoomIn/1,zoomOut/1]).

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
  isShownOnScreen/1,isTopLevel/1,layout/1,lower/1,move/2,move/3,move/4,
  moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,navigate/2,
  parent_class/1,popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,
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
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxStyledTextCtrl() :: wx:wx_object().
-export_type([wxStyledTextCtrl/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwxstyledtextctrl">external documentation</a>.
-spec new() -> wxStyledTextCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxStyledTextCtrl_new_0),
  wxe_util:rec(?wxStyledTextCtrl_new_0).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxStyledTextCtrl() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwxstyledtextctrl">external documentation</a>.
-spec new(Parent, [Option]) -> wxStyledTextCtrl() when
	Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxStyledTextCtrl_new_2),
  wxe_util:rec(?wxStyledTextCtrl_new_2).

%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
	This::wxStyledTextCtrl(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> boolean() when
	This::wxStyledTextCtrl(), Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent, Opts,?get_env(),?wxStyledTextCtrl_Create),
  wxe_util:rec(?wxStyledTextCtrl_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrladdtext">external documentation</a>.
-spec addText(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
addText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_AddText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlinserttext">external documentation</a>.
-spec insertText(This, Pos, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Pos::integer(), Text::unicode:chardata().
insertText(#wx_ref{type=ThisT}=This,Pos,Text)
 when is_integer(Pos),?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Pos,Text_UC,?get_env(),?wxStyledTextCtrl_InsertText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlclearall">external documentation</a>.
-spec clearAll(This) -> 'ok' when
	This::wxStyledTextCtrl().
clearAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ClearAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcleardocumentstyle">external documentation</a>.
-spec clearDocumentStyle(This) -> 'ok' when
	This::wxStyledTextCtrl().
clearDocumentStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ClearDocumentStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlength">external documentation</a>.
-spec getLength(This) -> integer() when
	This::wxStyledTextCtrl().
getLength(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetLength),
  wxe_util:rec(?wxStyledTextCtrl_GetLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcharat">external documentation</a>.
-spec getCharAt(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
getCharAt(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_GetCharAt),
  wxe_util:rec(?wxStyledTextCtrl_GetCharAt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurrentpos">external documentation</a>.
-spec getCurrentPos(This) -> integer() when
	This::wxStyledTextCtrl().
getCurrentPos(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCurrentPos),
  wxe_util:rec(?wxStyledTextCtrl_GetCurrentPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetanchor">external documentation</a>.
-spec getAnchor(This) -> integer() when
	This::wxStyledTextCtrl().
getAnchor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetAnchor),
  wxe_util:rec(?wxStyledTextCtrl_GetAnchor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstyleat">external documentation</a>.
-spec getStyleAt(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
getStyleAt(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_GetStyleAt),
  wxe_util:rec(?wxStyledTextCtrl_GetStyleAt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlredo">external documentation</a>.
-spec redo(This) -> 'ok' when
	This::wxStyledTextCtrl().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Redo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetundocollection">external documentation</a>.
-spec setUndoCollection(This, CollectUndo) -> 'ok' when
	This::wxStyledTextCtrl(), CollectUndo::boolean().
setUndoCollection(#wx_ref{type=ThisT}=This,CollectUndo)
 when is_boolean(CollectUndo) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CollectUndo,?get_env(),?wxStyledTextCtrl_SetUndoCollection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlselectall">external documentation</a>.
-spec selectAll(This) -> 'ok' when
	This::wxStyledTextCtrl().
selectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SelectAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetsavepoint">external documentation</a>.
-spec setSavePoint(This) -> 'ok' when
	This::wxStyledTextCtrl().
setSavePoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SetSavePoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcanredo">external documentation</a>.
-spec canRedo(This) -> boolean() when
	This::wxStyledTextCtrl().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CanRedo),
  wxe_util:rec(?wxStyledTextCtrl_CanRedo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerlinefromhandle">external documentation</a>.
-spec markerLineFromHandle(This, MarkerHandle) -> integer() when
	This::wxStyledTextCtrl(), MarkerHandle::integer().
markerLineFromHandle(#wx_ref{type=ThisT}=This,MarkerHandle)
 when is_integer(MarkerHandle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerHandle,?get_env(),?wxStyledTextCtrl_MarkerLineFromHandle),
  wxe_util:rec(?wxStyledTextCtrl_MarkerLineFromHandle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdeletehandle">external documentation</a>.
-spec markerDeleteHandle(This, MarkerHandle) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerHandle::integer().
markerDeleteHandle(#wx_ref{type=ThisT}=This,MarkerHandle)
 when is_integer(MarkerHandle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerHandle,?get_env(),?wxStyledTextCtrl_MarkerDeleteHandle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetundocollection">external documentation</a>.
-spec getUndoCollection(This) -> boolean() when
	This::wxStyledTextCtrl().
getUndoCollection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetUndoCollection),
  wxe_util:rec(?wxStyledTextCtrl_GetUndoCollection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetviewwhitespace">external documentation</a>.
-spec getViewWhiteSpace(This) -> integer() when
	This::wxStyledTextCtrl().
getViewWhiteSpace(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetViewWhiteSpace),
  wxe_util:rec(?wxStyledTextCtrl_GetViewWhiteSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetviewwhitespace">external documentation</a>.
-spec setViewWhiteSpace(This, ViewWS) -> 'ok' when
	This::wxStyledTextCtrl(), ViewWS::integer().
setViewWhiteSpace(#wx_ref{type=ThisT}=This,ViewWS)
 when is_integer(ViewWS) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,ViewWS,?get_env(),?wxStyledTextCtrl_SetViewWhiteSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfrompoint">external documentation</a>.
-spec positionFromPoint(This, Pt) -> integer() when
	This::wxStyledTextCtrl(), Pt::{X::integer(), Y::integer()}.
positionFromPoint(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxStyledTextCtrl_PositionFromPoint),
  wxe_util:rec(?wxStyledTextCtrl_PositionFromPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfrompointclose">external documentation</a>.
-spec positionFromPointClose(This, X, Y) -> integer() when
	This::wxStyledTextCtrl(), X::integer(), Y::integer().
positionFromPointClose(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxStyledTextCtrl_PositionFromPointClose),
  wxe_util:rec(?wxStyledTextCtrl_PositionFromPointClose).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgotoline">external documentation</a>.
-spec gotoLine(This, Line) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer().
gotoLine(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GotoLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgotopos">external documentation</a>.
-spec gotoPos(This, Caret) -> 'ok' when
	This::wxStyledTextCtrl(), Caret::integer().
gotoPos(#wx_ref{type=ThisT}=This,Caret)
 when is_integer(Caret) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Caret,?get_env(),?wxStyledTextCtrl_GotoPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetanchor">external documentation</a>.
-spec setAnchor(This, Anchor) -> 'ok' when
	This::wxStyledTextCtrl(), Anchor::integer().
setAnchor(#wx_ref{type=ThisT}=This,Anchor)
 when is_integer(Anchor) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Anchor,?get_env(),?wxStyledTextCtrl_SetAnchor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurline">external documentation</a>.
-spec getCurLine(This) -> Result when
	Result ::{Res ::unicode:charlist(), LinePos::integer()},
	This::wxStyledTextCtrl().
getCurLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCurLine),
  wxe_util:rec(?wxStyledTextCtrl_GetCurLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetendstyled">external documentation</a>.
-spec getEndStyled(This) -> integer() when
	This::wxStyledTextCtrl().
getEndStyled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEndStyled),
  wxe_util:rec(?wxStyledTextCtrl_GetEndStyled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlconverteols">external documentation</a>.
-spec convertEOLs(This, EolMode) -> 'ok' when
	This::wxStyledTextCtrl(), EolMode::integer().
convertEOLs(#wx_ref{type=ThisT}=This,EolMode)
 when is_integer(EolMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,EolMode,?get_env(),?wxStyledTextCtrl_ConvertEOLs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgeteolmode">external documentation</a>.
-spec getEOLMode(This) -> integer() when
	This::wxStyledTextCtrl().
getEOLMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEOLMode),
  wxe_util:rec(?wxStyledTextCtrl_GetEOLMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlseteolmode">external documentation</a>.
-spec setEOLMode(This, EolMode) -> 'ok' when
	This::wxStyledTextCtrl(), EolMode::integer().
setEOLMode(#wx_ref{type=ThisT}=This,EolMode)
 when is_integer(EolMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,EolMode,?get_env(),?wxStyledTextCtrl_SetEOLMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstartstyling">external documentation</a>.
-spec startStyling(This, Start) -> 'ok' when
	This::wxStyledTextCtrl(), Start::integer().
startStyling(#wx_ref{type=ThisT}=This,Start)
 when is_integer(Start) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Start,?get_env(),?wxStyledTextCtrl_StartStyling).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstyling">external documentation</a>.
-spec setStyling(This, Length, Style) -> 'ok' when
	This::wxStyledTextCtrl(), Length::integer(), Style::integer().
setStyling(#wx_ref{type=ThisT}=This,Length,Style)
 when is_integer(Length),is_integer(Style) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Length,Style,?get_env(),?wxStyledTextCtrl_SetStyling).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetbuffereddraw">external documentation</a>.
-spec getBufferedDraw(This) -> boolean() when
	This::wxStyledTextCtrl().
getBufferedDraw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetBufferedDraw),
  wxe_util:rec(?wxStyledTextCtrl_GetBufferedDraw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetbuffereddraw">external documentation</a>.
-spec setBufferedDraw(This, Buffered) -> 'ok' when
	This::wxStyledTextCtrl(), Buffered::boolean().
setBufferedDraw(#wx_ref{type=ThisT}=This,Buffered)
 when is_boolean(Buffered) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Buffered,?get_env(),?wxStyledTextCtrl_SetBufferedDraw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettabwidth">external documentation</a>.
-spec setTabWidth(This, TabWidth) -> 'ok' when
	This::wxStyledTextCtrl(), TabWidth::integer().
setTabWidth(#wx_ref{type=ThisT}=This,TabWidth)
 when is_integer(TabWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,TabWidth,?get_env(),?wxStyledTextCtrl_SetTabWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettabwidth">external documentation</a>.
-spec getTabWidth(This) -> integer() when
	This::wxStyledTextCtrl().
getTabWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTabWidth),
  wxe_util:rec(?wxStyledTextCtrl_GetTabWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcodepage">external documentation</a>.
-spec setCodePage(This, CodePage) -> 'ok' when
	This::wxStyledTextCtrl(), CodePage::integer().
setCodePage(#wx_ref{type=ThisT}=This,CodePage)
 when is_integer(CodePage) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CodePage,?get_env(),?wxStyledTextCtrl_SetCodePage).

%% @equiv markerDefine(This,MarkerNumber,MarkerSymbol, [])
-spec markerDefine(This, MarkerNumber, MarkerSymbol) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), MarkerSymbol::integer().

markerDefine(This,MarkerNumber,MarkerSymbol)
 when is_record(This, wx_ref),is_integer(MarkerNumber),is_integer(MarkerSymbol) ->
  markerDefine(This,MarkerNumber,MarkerSymbol, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdefine">external documentation</a>.
-spec markerDefine(This, MarkerNumber, MarkerSymbol, [Option]) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), MarkerSymbol::integer(),
	Option :: {'foreground', wx:wx_colour()}
		 | {'background', wx:wx_colour()}.
markerDefine(#wx_ref{type=ThisT}=This,MarkerNumber,MarkerSymbol, Options)
 when is_integer(MarkerNumber),is_integer(MarkerSymbol),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  MOpts = fun({foreground, Foreground}) -> {foreground,wxe_util:color(Foreground)};
          ({background, Background}) -> {background,wxe_util:color(Background)};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MarkerNumber,MarkerSymbol, Opts,?get_env(),?wxStyledTextCtrl_MarkerDefine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetforeground">external documentation</a>.
-spec markerSetForeground(This, MarkerNumber, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), Fore::wx:wx_colour().
markerSetForeground(#wx_ref{type=ThisT}=This,MarkerNumber,Fore)
 when is_integer(MarkerNumber),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerNumber,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_MarkerSetForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetbackground">external documentation</a>.
-spec markerSetBackground(This, MarkerNumber, Back) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), Back::wx:wx_colour().
markerSetBackground(#wx_ref{type=ThisT}=This,MarkerNumber,Back)
 when is_integer(MarkerNumber),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerNumber,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_MarkerSetBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkeradd">external documentation</a>.
-spec markerAdd(This, Line, MarkerNumber) -> integer() when
	This::wxStyledTextCtrl(), Line::integer(), MarkerNumber::integer().
markerAdd(#wx_ref{type=ThisT}=This,Line,MarkerNumber)
 when is_integer(Line),is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,MarkerNumber,?get_env(),?wxStyledTextCtrl_MarkerAdd),
  wxe_util:rec(?wxStyledTextCtrl_MarkerAdd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdelete">external documentation</a>.
-spec markerDelete(This, Line, MarkerNumber) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), MarkerNumber::integer().
markerDelete(#wx_ref{type=ThisT}=This,Line,MarkerNumber)
 when is_integer(Line),is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,MarkerNumber,?get_env(),?wxStyledTextCtrl_MarkerDelete).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdeleteall">external documentation</a>.
-spec markerDeleteAll(This, MarkerNumber) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer().
markerDeleteAll(#wx_ref{type=ThisT}=This,MarkerNumber)
 when is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerNumber,?get_env(),?wxStyledTextCtrl_MarkerDeleteAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerget">external documentation</a>.
-spec markerGet(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
markerGet(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_MarkerGet),
  wxe_util:rec(?wxStyledTextCtrl_MarkerGet).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkernext">external documentation</a>.
-spec markerNext(This, LineStart, MarkerMask) -> integer() when
	This::wxStyledTextCtrl(), LineStart::integer(), MarkerMask::integer().
markerNext(#wx_ref{type=ThisT}=This,LineStart,MarkerMask)
 when is_integer(LineStart),is_integer(MarkerMask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,LineStart,MarkerMask,?get_env(),?wxStyledTextCtrl_MarkerNext),
  wxe_util:rec(?wxStyledTextCtrl_MarkerNext).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerprevious">external documentation</a>.
-spec markerPrevious(This, LineStart, MarkerMask) -> integer() when
	This::wxStyledTextCtrl(), LineStart::integer(), MarkerMask::integer().
markerPrevious(#wx_ref{type=ThisT}=This,LineStart,MarkerMask)
 when is_integer(LineStart),is_integer(MarkerMask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,LineStart,MarkerMask,?get_env(),?wxStyledTextCtrl_MarkerPrevious),
  wxe_util:rec(?wxStyledTextCtrl_MarkerPrevious).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdefinebitmap">external documentation</a>.
-spec markerDefineBitmap(This, MarkerNumber, Bmp) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), Bmp::wxBitmap:wxBitmap().
markerDefineBitmap(#wx_ref{type=ThisT}=This,MarkerNumber,#wx_ref{type=BmpT}=Bmp)
 when is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,MarkerNumber,Bmp,?get_env(),?wxStyledTextCtrl_MarkerDefineBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkeraddset">external documentation</a>.
-spec markerAddSet(This, Line, MarkerSet) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), MarkerSet::integer().
markerAddSet(#wx_ref{type=ThisT}=This,Line,MarkerSet)
 when is_integer(Line),is_integer(MarkerSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,MarkerSet,?get_env(),?wxStyledTextCtrl_MarkerAddSet).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetalpha">external documentation</a>.
-spec markerSetAlpha(This, MarkerNumber, Alpha) -> 'ok' when
	This::wxStyledTextCtrl(), MarkerNumber::integer(), Alpha::integer().
markerSetAlpha(#wx_ref{type=ThisT}=This,MarkerNumber,Alpha)
 when is_integer(MarkerNumber),is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,MarkerNumber,Alpha,?get_env(),?wxStyledTextCtrl_MarkerSetAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmargintype">external documentation</a>.
-spec setMarginType(This, Margin, MarginType) -> 'ok' when
	This::wxStyledTextCtrl(), Margin::integer(), MarginType::integer().
setMarginType(#wx_ref{type=ThisT}=This,Margin,MarginType)
 when is_integer(Margin),is_integer(MarginType) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,MarginType,?get_env(),?wxStyledTextCtrl_SetMarginType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmargintype">external documentation</a>.
-spec getMarginType(This, Margin) -> integer() when
	This::wxStyledTextCtrl(), Margin::integer().
getMarginType(#wx_ref{type=ThisT}=This,Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,?get_env(),?wxStyledTextCtrl_GetMarginType),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginwidth">external documentation</a>.
-spec setMarginWidth(This, Margin, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), Margin::integer(), PixelWidth::integer().
setMarginWidth(#wx_ref{type=ThisT}=This,Margin,PixelWidth)
 when is_integer(Margin),is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,PixelWidth,?get_env(),?wxStyledTextCtrl_SetMarginWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginwidth">external documentation</a>.
-spec getMarginWidth(This, Margin) -> integer() when
	This::wxStyledTextCtrl(), Margin::integer().
getMarginWidth(#wx_ref{type=ThisT}=This,Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,?get_env(),?wxStyledTextCtrl_GetMarginWidth),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginmask">external documentation</a>.
-spec setMarginMask(This, Margin, Mask) -> 'ok' when
	This::wxStyledTextCtrl(), Margin::integer(), Mask::integer().
setMarginMask(#wx_ref{type=ThisT}=This,Margin,Mask)
 when is_integer(Margin),is_integer(Mask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,Mask,?get_env(),?wxStyledTextCtrl_SetMarginMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginmask">external documentation</a>.
-spec getMarginMask(This, Margin) -> integer() when
	This::wxStyledTextCtrl(), Margin::integer().
getMarginMask(#wx_ref{type=ThisT}=This,Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,?get_env(),?wxStyledTextCtrl_GetMarginMask),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginsensitive">external documentation</a>.
-spec setMarginSensitive(This, Margin, Sensitive) -> 'ok' when
	This::wxStyledTextCtrl(), Margin::integer(), Sensitive::boolean().
setMarginSensitive(#wx_ref{type=ThisT}=This,Margin,Sensitive)
 when is_integer(Margin),is_boolean(Sensitive) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,Sensitive,?get_env(),?wxStyledTextCtrl_SetMarginSensitive).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginsensitive">external documentation</a>.
-spec getMarginSensitive(This, Margin) -> boolean() when
	This::wxStyledTextCtrl(), Margin::integer().
getMarginSensitive(#wx_ref{type=ThisT}=This,Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Margin,?get_env(),?wxStyledTextCtrl_GetMarginSensitive),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginSensitive).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleclearall">external documentation</a>.
-spec styleClearAll(This) -> 'ok' when
	This::wxStyledTextCtrl().
styleClearAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StyleClearAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetforeground">external documentation</a>.
-spec styleSetForeground(This, Style, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Fore::wx:wx_colour().
styleSetForeground(#wx_ref{type=ThisT}=This,Style,Fore)
 when is_integer(Style),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_StyleSetForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetbackground">external documentation</a>.
-spec styleSetBackground(This, Style, Back) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Back::wx:wx_colour().
styleSetBackground(#wx_ref{type=ThisT}=This,Style,Back)
 when is_integer(Style),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_StyleSetBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetbold">external documentation</a>.
-spec styleSetBold(This, Style, Bold) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Bold::boolean().
styleSetBold(#wx_ref{type=ThisT}=This,Style,Bold)
 when is_integer(Style),is_boolean(Bold) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Bold,?get_env(),?wxStyledTextCtrl_StyleSetBold).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetitalic">external documentation</a>.
-spec styleSetItalic(This, Style, Italic) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Italic::boolean().
styleSetItalic(#wx_ref{type=ThisT}=This,Style,Italic)
 when is_integer(Style),is_boolean(Italic) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Italic,?get_env(),?wxStyledTextCtrl_StyleSetItalic).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetsize">external documentation</a>.
-spec styleSetSize(This, Style, SizePoints) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), SizePoints::integer().
styleSetSize(#wx_ref{type=ThisT}=This,Style,SizePoints)
 when is_integer(Style),is_integer(SizePoints) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,SizePoints,?get_env(),?wxStyledTextCtrl_StyleSetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfacename">external documentation</a>.
-spec styleSetFaceName(This, Style, FontName) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), FontName::unicode:chardata().
styleSetFaceName(#wx_ref{type=ThisT}=This,Style,FontName)
 when is_integer(Style),?is_chardata(FontName) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  FontName_UC = unicode:characters_to_binary(FontName),
  wxe_util:queue_cmd(This,Style,FontName_UC,?get_env(),?wxStyledTextCtrl_StyleSetFaceName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleseteolfilled">external documentation</a>.
-spec styleSetEOLFilled(This, Style, EolFilled) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), EolFilled::boolean().
styleSetEOLFilled(#wx_ref{type=ThisT}=This,Style,EolFilled)
 when is_integer(Style),is_boolean(EolFilled) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,EolFilled,?get_env(),?wxStyledTextCtrl_StyleSetEOLFilled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleresetdefault">external documentation</a>.
-spec styleResetDefault(This) -> 'ok' when
	This::wxStyledTextCtrl().
styleResetDefault(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StyleResetDefault).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetunderline">external documentation</a>.
-spec styleSetUnderline(This, Style, Underline) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Underline::boolean().
styleSetUnderline(#wx_ref{type=ThisT}=This,Style,Underline)
 when is_integer(Style),is_boolean(Underline) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Underline,?get_env(),?wxStyledTextCtrl_StyleSetUnderline).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetcase">external documentation</a>.
-spec styleSetCase(This, Style, CaseVisible) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), CaseVisible::integer().
styleSetCase(#wx_ref{type=ThisT}=This,Style,CaseVisible)
 when is_integer(Style),is_integer(CaseVisible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,CaseVisible,?get_env(),?wxStyledTextCtrl_StyleSetCase).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesethotspot">external documentation</a>.
-spec styleSetHotSpot(This, Style, Hotspot) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Hotspot::boolean().
styleSetHotSpot(#wx_ref{type=ThisT}=This,Style,Hotspot)
 when is_integer(Style),is_boolean(Hotspot) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Hotspot,?get_env(),?wxStyledTextCtrl_StyleSetHotSpot).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselforeground">external documentation</a>.
-spec setSelForeground(This, UseSetting, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Fore::wx:wx_colour().
setSelForeground(#wx_ref{type=ThisT}=This,UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_SetSelForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselbackground">external documentation</a>.
-spec setSelBackground(This, UseSetting, Back) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Back::wx:wx_colour().
setSelBackground(#wx_ref{type=ThisT}=This,UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_SetSelBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselalpha">external documentation</a>.
-spec getSelAlpha(This) -> integer() when
	This::wxStyledTextCtrl().
getSelAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelAlpha),
  wxe_util:rec(?wxStyledTextCtrl_GetSelAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselalpha">external documentation</a>.
-spec setSelAlpha(This, Alpha) -> 'ok' when
	This::wxStyledTextCtrl(), Alpha::integer().
setSelAlpha(#wx_ref{type=ThisT}=This,Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Alpha,?get_env(),?wxStyledTextCtrl_SetSelAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretforeground">external documentation</a>.
-spec setCaretForeground(This, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), Fore::wx:wx_colour().
setCaretForeground(#wx_ref{type=ThisT}=This,Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_SetCaretForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyassign">external documentation</a>.
-spec cmdKeyAssign(This, Key, Modifiers, Cmd) -> 'ok' when
	This::wxStyledTextCtrl(), Key::integer(), Modifiers::integer(), Cmd::integer().
cmdKeyAssign(#wx_ref{type=ThisT}=This,Key,Modifiers,Cmd)
 when is_integer(Key),is_integer(Modifiers),is_integer(Cmd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Key,Modifiers,Cmd,?get_env(),?wxStyledTextCtrl_CmdKeyAssign).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyclear">external documentation</a>.
-spec cmdKeyClear(This, Key, Modifiers) -> 'ok' when
	This::wxStyledTextCtrl(), Key::integer(), Modifiers::integer().
cmdKeyClear(#wx_ref{type=ThisT}=This,Key,Modifiers)
 when is_integer(Key),is_integer(Modifiers) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Key,Modifiers,?get_env(),?wxStyledTextCtrl_CmdKeyClear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyclearall">external documentation</a>.
-spec cmdKeyClearAll(This) -> 'ok' when
	This::wxStyledTextCtrl().
cmdKeyClearAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CmdKeyClearAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstylebytes">external documentation</a>.
-spec setStyleBytes(This, Length) -> integer() when
	This::wxStyledTextCtrl(), Length::integer().
setStyleBytes(#wx_ref{type=ThisT}=This,Length)
 when is_integer(Length) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Length,?get_env(),?wxStyledTextCtrl_SetStyleBytes),
  wxe_util:rec(?wxStyledTextCtrl_SetStyleBytes).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetvisible">external documentation</a>.
-spec styleSetVisible(This, Style, Visible) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Visible::boolean().
styleSetVisible(#wx_ref{type=ThisT}=This,Style,Visible)
 when is_integer(Style),is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Visible,?get_env(),?wxStyledTextCtrl_StyleSetVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretperiod">external documentation</a>.
-spec getCaretPeriod(This) -> integer() when
	This::wxStyledTextCtrl().
getCaretPeriod(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretPeriod),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretPeriod).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretperiod">external documentation</a>.
-spec setCaretPeriod(This, PeriodMilliseconds) -> 'ok' when
	This::wxStyledTextCtrl(), PeriodMilliseconds::integer().
setCaretPeriod(#wx_ref{type=ThisT}=This,PeriodMilliseconds)
 when is_integer(PeriodMilliseconds) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PeriodMilliseconds,?get_env(),?wxStyledTextCtrl_SetCaretPeriod).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwordchars">external documentation</a>.
-spec setWordChars(This, Characters) -> 'ok' when
	This::wxStyledTextCtrl(), Characters::unicode:chardata().
setWordChars(#wx_ref{type=ThisT}=This,Characters)
 when ?is_chardata(Characters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Characters_UC = unicode:characters_to_binary(Characters),
  wxe_util:queue_cmd(This,Characters_UC,?get_env(),?wxStyledTextCtrl_SetWordChars).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlbeginundoaction">external documentation</a>.
-spec beginUndoAction(This) -> 'ok' when
	This::wxStyledTextCtrl().
beginUndoAction(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_BeginUndoAction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlendundoaction">external documentation</a>.
-spec endUndoAction(This) -> 'ok' when
	This::wxStyledTextCtrl().
endUndoAction(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_EndUndoAction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorsetstyle">external documentation</a>.
-spec indicatorSetStyle(This, Indicator, IndicatorStyle) -> 'ok' when
	This::wxStyledTextCtrl(), Indicator::integer(), IndicatorStyle::integer().
indicatorSetStyle(#wx_ref{type=ThisT}=This,Indicator,IndicatorStyle)
 when is_integer(Indicator),is_integer(IndicatorStyle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Indicator,IndicatorStyle,?get_env(),?wxStyledTextCtrl_IndicatorSetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorgetstyle">external documentation</a>.
-spec indicatorGetStyle(This, Indicator) -> integer() when
	This::wxStyledTextCtrl(), Indicator::integer().
indicatorGetStyle(#wx_ref{type=ThisT}=This,Indicator)
 when is_integer(Indicator) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Indicator,?get_env(),?wxStyledTextCtrl_IndicatorGetStyle),
  wxe_util:rec(?wxStyledTextCtrl_IndicatorGetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorsetforeground">external documentation</a>.
-spec indicatorSetForeground(This, Indicator, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), Indicator::integer(), Fore::wx:wx_colour().
indicatorSetForeground(#wx_ref{type=ThisT}=This,Indicator,Fore)
 when is_integer(Indicator),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Indicator,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_IndicatorSetForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorgetforeground">external documentation</a>.
-spec indicatorGetForeground(This, Indicator) -> wx:wx_colour4() when
	This::wxStyledTextCtrl(), Indicator::integer().
indicatorGetForeground(#wx_ref{type=ThisT}=This,Indicator)
 when is_integer(Indicator) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Indicator,?get_env(),?wxStyledTextCtrl_IndicatorGetForeground),
  wxe_util:rec(?wxStyledTextCtrl_IndicatorGetForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespaceforeground">external documentation</a>.
-spec setWhitespaceForeground(This, UseSetting, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Fore::wx:wx_colour().
setWhitespaceForeground(#wx_ref{type=ThisT}=This,UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_SetWhitespaceForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespacebackground">external documentation</a>.
-spec setWhitespaceBackground(This, UseSetting, Back) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Back::wx:wx_colour().
setWhitespaceBackground(#wx_ref{type=ThisT}=This,UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_SetWhitespaceBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstylebits">external documentation</a>.
-spec getStyleBits(This) -> integer() when
	This::wxStyledTextCtrl().
getStyleBits(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetStyleBits),
  wxe_util:rec(?wxStyledTextCtrl_GetStyleBits).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlinestate">external documentation</a>.
-spec setLineState(This, Line, State) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), State::integer().
setLineState(#wx_ref{type=ThisT}=This,Line,State)
 when is_integer(Line),is_integer(State) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,State,?get_env(),?wxStyledTextCtrl_SetLineState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinestate">external documentation</a>.
-spec getLineState(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getLineState(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineState),
  wxe_util:rec(?wxStyledTextCtrl_GetLineState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmaxlinestate">external documentation</a>.
-spec getMaxLineState(This) -> integer() when
	This::wxStyledTextCtrl().
getMaxLineState(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetMaxLineState),
  wxe_util:rec(?wxStyledTextCtrl_GetMaxLineState).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinevisible">external documentation</a>.
-spec getCaretLineVisible(This) -> boolean() when
	This::wxStyledTextCtrl().
getCaretLineVisible(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretLineVisible),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretLineVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinevisible">external documentation</a>.
-spec setCaretLineVisible(This, Show) -> 'ok' when
	This::wxStyledTextCtrl(), Show::boolean().
setCaretLineVisible(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxStyledTextCtrl_SetCaretLineVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinebackground">external documentation</a>.
-spec getCaretLineBackground(This) -> wx:wx_colour4() when
	This::wxStyledTextCtrl().
getCaretLineBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretLineBackground),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretLineBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinebackground">external documentation</a>.
-spec setCaretLineBackground(This, Back) -> 'ok' when
	This::wxStyledTextCtrl(), Back::wx:wx_colour().
setCaretLineBackground(#wx_ref{type=ThisT}=This,Back)
 when tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_SetCaretLineBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompshow">external documentation</a>.
-spec autoCompShow(This, LengthEntered, ItemList) -> 'ok' when
	This::wxStyledTextCtrl(), LengthEntered::integer(), ItemList::unicode:chardata().
autoCompShow(#wx_ref{type=ThisT}=This,LengthEntered,ItemList)
 when is_integer(LengthEntered),?is_chardata(ItemList) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ItemList_UC = unicode:characters_to_binary(ItemList),
  wxe_util:queue_cmd(This,LengthEntered,ItemList_UC,?get_env(),?wxStyledTextCtrl_AutoCompShow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompcancel">external documentation</a>.
-spec autoCompCancel(This) -> 'ok' when
	This::wxStyledTextCtrl().
autoCompCancel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompCancel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompactive">external documentation</a>.
-spec autoCompActive(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompActive(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompActive),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompActive).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompposstart">external documentation</a>.
-spec autoCompPosStart(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompPosStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompPosStart),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompPosStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompcomplete">external documentation</a>.
-spec autoCompComplete(This) -> 'ok' when
	This::wxStyledTextCtrl().
autoCompComplete(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompComplete).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompstops">external documentation</a>.
-spec autoCompStops(This, CharacterSet) -> 'ok' when
	This::wxStyledTextCtrl(), CharacterSet::unicode:chardata().
autoCompStops(#wx_ref{type=ThisT}=This,CharacterSet)
 when ?is_chardata(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  CharacterSet_UC = unicode:characters_to_binary(CharacterSet),
  wxe_util:queue_cmd(This,CharacterSet_UC,?get_env(),?wxStyledTextCtrl_AutoCompStops).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetseparator">external documentation</a>.
-spec autoCompSetSeparator(This, SeparatorCharacter) -> 'ok' when
	This::wxStyledTextCtrl(), SeparatorCharacter::integer().
autoCompSetSeparator(#wx_ref{type=ThisT}=This,SeparatorCharacter)
 when is_integer(SeparatorCharacter) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,SeparatorCharacter,?get_env(),?wxStyledTextCtrl_AutoCompSetSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetseparator">external documentation</a>.
-spec autoCompGetSeparator(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompGetSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetSeparator),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompselect">external documentation</a>.
-spec autoCompSelect(This, Select) -> 'ok' when
	This::wxStyledTextCtrl(), Select::unicode:chardata().
autoCompSelect(#wx_ref{type=ThisT}=This,Select)
 when ?is_chardata(Select) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Select_UC = unicode:characters_to_binary(Select),
  wxe_util:queue_cmd(This,Select_UC,?get_env(),?wxStyledTextCtrl_AutoCompSelect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetcancelatstart">external documentation</a>.
-spec autoCompSetCancelAtStart(This, Cancel) -> 'ok' when
	This::wxStyledTextCtrl(), Cancel::boolean().
autoCompSetCancelAtStart(#wx_ref{type=ThisT}=This,Cancel)
 when is_boolean(Cancel) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Cancel,?get_env(),?wxStyledTextCtrl_AutoCompSetCancelAtStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetcancelatstart">external documentation</a>.
-spec autoCompGetCancelAtStart(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompGetCancelAtStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetCancelAtStart),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetCancelAtStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetfillups">external documentation</a>.
-spec autoCompSetFillUps(This, CharacterSet) -> 'ok' when
	This::wxStyledTextCtrl(), CharacterSet::unicode:chardata().
autoCompSetFillUps(#wx_ref{type=ThisT}=This,CharacterSet)
 when ?is_chardata(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  CharacterSet_UC = unicode:characters_to_binary(CharacterSet),
  wxe_util:queue_cmd(This,CharacterSet_UC,?get_env(),?wxStyledTextCtrl_AutoCompSetFillUps).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetchoosesingle">external documentation</a>.
-spec autoCompSetChooseSingle(This, ChooseSingle) -> 'ok' when
	This::wxStyledTextCtrl(), ChooseSingle::boolean().
autoCompSetChooseSingle(#wx_ref{type=ThisT}=This,ChooseSingle)
 when is_boolean(ChooseSingle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,ChooseSingle,?get_env(),?wxStyledTextCtrl_AutoCompSetChooseSingle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetchoosesingle">external documentation</a>.
-spec autoCompGetChooseSingle(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompGetChooseSingle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetChooseSingle),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetChooseSingle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetignorecase">external documentation</a>.
-spec autoCompSetIgnoreCase(This, IgnoreCase) -> 'ok' when
	This::wxStyledTextCtrl(), IgnoreCase::boolean().
autoCompSetIgnoreCase(#wx_ref{type=ThisT}=This,IgnoreCase)
 when is_boolean(IgnoreCase) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,IgnoreCase,?get_env(),?wxStyledTextCtrl_AutoCompSetIgnoreCase).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetignorecase">external documentation</a>.
-spec autoCompGetIgnoreCase(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompGetIgnoreCase(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetIgnoreCase),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetIgnoreCase).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrluserlistshow">external documentation</a>.
-spec userListShow(This, ListType, ItemList) -> 'ok' when
	This::wxStyledTextCtrl(), ListType::integer(), ItemList::unicode:chardata().
userListShow(#wx_ref{type=ThisT}=This,ListType,ItemList)
 when is_integer(ListType),?is_chardata(ItemList) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ItemList_UC = unicode:characters_to_binary(ItemList),
  wxe_util:queue_cmd(This,ListType,ItemList_UC,?get_env(),?wxStyledTextCtrl_UserListShow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetautohide">external documentation</a>.
-spec autoCompSetAutoHide(This, AutoHide) -> 'ok' when
	This::wxStyledTextCtrl(), AutoHide::boolean().
autoCompSetAutoHide(#wx_ref{type=ThisT}=This,AutoHide)
 when is_boolean(AutoHide) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,AutoHide,?get_env(),?wxStyledTextCtrl_AutoCompSetAutoHide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetautohide">external documentation</a>.
-spec autoCompGetAutoHide(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompGetAutoHide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetAutoHide),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetAutoHide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetdroprestofword">external documentation</a>.
-spec autoCompSetDropRestOfWord(This, DropRestOfWord) -> 'ok' when
	This::wxStyledTextCtrl(), DropRestOfWord::boolean().
autoCompSetDropRestOfWord(#wx_ref{type=ThisT}=This,DropRestOfWord)
 when is_boolean(DropRestOfWord) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,DropRestOfWord,?get_env(),?wxStyledTextCtrl_AutoCompSetDropRestOfWord).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetdroprestofword">external documentation</a>.
-spec autoCompGetDropRestOfWord(This) -> boolean() when
	This::wxStyledTextCtrl().
autoCompGetDropRestOfWord(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetDropRestOfWord),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetDropRestOfWord).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlregisterimage">external documentation</a>.
-spec registerImage(This, Type, Bmp) -> 'ok' when
	This::wxStyledTextCtrl(), Type::integer(), Bmp::wxBitmap:wxBitmap().
registerImage(#wx_ref{type=ThisT}=This,Type,#wx_ref{type=BmpT}=Bmp)
 when is_integer(Type) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Type,Bmp,?get_env(),?wxStyledTextCtrl_RegisterImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlclearregisteredimages">external documentation</a>.
-spec clearRegisteredImages(This) -> 'ok' when
	This::wxStyledTextCtrl().
clearRegisteredImages(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ClearRegisteredImages).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgettypeseparator">external documentation</a>.
-spec autoCompGetTypeSeparator(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompGetTypeSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetTypeSeparator),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetTypeSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsettypeseparator">external documentation</a>.
-spec autoCompSetTypeSeparator(This, SeparatorCharacter) -> 'ok' when
	This::wxStyledTextCtrl(), SeparatorCharacter::integer().
autoCompSetTypeSeparator(#wx_ref{type=ThisT}=This,SeparatorCharacter)
 when is_integer(SeparatorCharacter) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,SeparatorCharacter,?get_env(),?wxStyledTextCtrl_AutoCompSetTypeSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetmaxwidth">external documentation</a>.
-spec autoCompSetMaxWidth(This, CharacterCount) -> 'ok' when
	This::wxStyledTextCtrl(), CharacterCount::integer().
autoCompSetMaxWidth(#wx_ref{type=ThisT}=This,CharacterCount)
 when is_integer(CharacterCount) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CharacterCount,?get_env(),?wxStyledTextCtrl_AutoCompSetMaxWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetmaxwidth">external documentation</a>.
-spec autoCompGetMaxWidth(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompGetMaxWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetMaxWidth),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetMaxWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetmaxheight">external documentation</a>.
-spec autoCompSetMaxHeight(This, RowCount) -> 'ok' when
	This::wxStyledTextCtrl(), RowCount::integer().
autoCompSetMaxHeight(#wx_ref{type=ThisT}=This,RowCount)
 when is_integer(RowCount) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,RowCount,?get_env(),?wxStyledTextCtrl_AutoCompSetMaxHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetmaxheight">external documentation</a>.
-spec autoCompGetMaxHeight(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompGetMaxHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetMaxHeight),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetMaxHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetindent">external documentation</a>.
-spec setIndent(This, IndentSize) -> 'ok' when
	This::wxStyledTextCtrl(), IndentSize::integer().
setIndent(#wx_ref{type=ThisT}=This,IndentSize)
 when is_integer(IndentSize) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,IndentSize,?get_env(),?wxStyledTextCtrl_SetIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetindent">external documentation</a>.
-spec getIndent(This) -> integer() when
	This::wxStyledTextCtrl().
getIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetIndent),
  wxe_util:rec(?wxStyledTextCtrl_GetIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetusetabs">external documentation</a>.
-spec setUseTabs(This, UseTabs) -> 'ok' when
	This::wxStyledTextCtrl(), UseTabs::boolean().
setUseTabs(#wx_ref{type=ThisT}=This,UseTabs)
 when is_boolean(UseTabs) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseTabs,?get_env(),?wxStyledTextCtrl_SetUseTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetusetabs">external documentation</a>.
-spec getUseTabs(This) -> boolean() when
	This::wxStyledTextCtrl().
getUseTabs(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetUseTabs),
  wxe_util:rec(?wxStyledTextCtrl_GetUseTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlineindentation">external documentation</a>.
-spec setLineIndentation(This, Line, Indentation) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), Indentation::integer().
setLineIndentation(#wx_ref{type=ThisT}=This,Line,Indentation)
 when is_integer(Line),is_integer(Indentation) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,Indentation,?get_env(),?wxStyledTextCtrl_SetLineIndentation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineindentation">external documentation</a>.
-spec getLineIndentation(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getLineIndentation(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineIndentation),
  wxe_util:rec(?wxStyledTextCtrl_GetLineIndentation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineindentposition">external documentation</a>.
-spec getLineIndentPosition(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getLineIndentPosition(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineIndentPosition),
  wxe_util:rec(?wxStyledTextCtrl_GetLineIndentPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcolumn">external documentation</a>.
-spec getColumn(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
getColumn(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_GetColumn),
  wxe_util:rec(?wxStyledTextCtrl_GetColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetusehorizontalscrollbar">external documentation</a>.
-spec setUseHorizontalScrollBar(This, Visible) -> 'ok' when
	This::wxStyledTextCtrl(), Visible::boolean().
setUseHorizontalScrollBar(#wx_ref{type=ThisT}=This,Visible)
 when is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Visible,?get_env(),?wxStyledTextCtrl_SetUseHorizontalScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetusehorizontalscrollbar">external documentation</a>.
-spec getUseHorizontalScrollBar(This) -> boolean() when
	This::wxStyledTextCtrl().
getUseHorizontalScrollBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetUseHorizontalScrollBar),
  wxe_util:rec(?wxStyledTextCtrl_GetUseHorizontalScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetindentationguides">external documentation</a>.
-spec setIndentationGuides(This, IndentView) -> 'ok' when
	This::wxStyledTextCtrl(), IndentView::integer().
setIndentationGuides(#wx_ref{type=ThisT}=This,IndentView)
 when is_integer(IndentView) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,IndentView,?get_env(),?wxStyledTextCtrl_SetIndentationGuides).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetindentationguides">external documentation</a>.
-spec getIndentationGuides(This) -> integer() when
	This::wxStyledTextCtrl().
getIndentationGuides(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetIndentationGuides),
  wxe_util:rec(?wxStyledTextCtrl_GetIndentationGuides).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethighlightguide">external documentation</a>.
-spec setHighlightGuide(This, Column) -> 'ok' when
	This::wxStyledTextCtrl(), Column::integer().
setHighlightGuide(#wx_ref{type=ThisT}=This,Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Column,?get_env(),?wxStyledTextCtrl_SetHighlightGuide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgethighlightguide">external documentation</a>.
-spec getHighlightGuide(This) -> integer() when
	This::wxStyledTextCtrl().
getHighlightGuide(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetHighlightGuide),
  wxe_util:rec(?wxStyledTextCtrl_GetHighlightGuide).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineendposition">external documentation</a>.
-spec getLineEndPosition(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getLineEndPosition(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineEndPosition),
  wxe_util:rec(?wxStyledTextCtrl_GetLineEndPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcodepage">external documentation</a>.
-spec getCodePage(This) -> integer() when
	This::wxStyledTextCtrl().
getCodePage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCodePage),
  wxe_util:rec(?wxStyledTextCtrl_GetCodePage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretforeground">external documentation</a>.
-spec getCaretForeground(This) -> wx:wx_colour4() when
	This::wxStyledTextCtrl().
getCaretForeground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretForeground),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetreadonly">external documentation</a>.
-spec getReadOnly(This) -> boolean() when
	This::wxStyledTextCtrl().
getReadOnly(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetReadOnly),
  wxe_util:rec(?wxStyledTextCtrl_GetReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcurrentpos">external documentation</a>.
-spec setCurrentPos(This, Caret) -> 'ok' when
	This::wxStyledTextCtrl(), Caret::integer().
setCurrentPos(#wx_ref{type=ThisT}=This,Caret)
 when is_integer(Caret) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Caret,?get_env(),?wxStyledTextCtrl_SetCurrentPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionstart">external documentation</a>.
-spec setSelectionStart(This, Anchor) -> 'ok' when
	This::wxStyledTextCtrl(), Anchor::integer().
setSelectionStart(#wx_ref{type=ThisT}=This,Anchor)
 when is_integer(Anchor) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Anchor,?get_env(),?wxStyledTextCtrl_SetSelectionStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionstart">external documentation</a>.
-spec getSelectionStart(This) -> integer() when
	This::wxStyledTextCtrl().
getSelectionStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelectionStart),
  wxe_util:rec(?wxStyledTextCtrl_GetSelectionStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionend">external documentation</a>.
-spec setSelectionEnd(This, Caret) -> 'ok' when
	This::wxStyledTextCtrl(), Caret::integer().
setSelectionEnd(#wx_ref{type=ThisT}=This,Caret)
 when is_integer(Caret) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Caret,?get_env(),?wxStyledTextCtrl_SetSelectionEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionend">external documentation</a>.
-spec getSelectionEnd(This) -> integer() when
	This::wxStyledTextCtrl().
getSelectionEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelectionEnd),
  wxe_util:rec(?wxStyledTextCtrl_GetSelectionEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetprintmagnification">external documentation</a>.
-spec setPrintMagnification(This, Magnification) -> 'ok' when
	This::wxStyledTextCtrl(), Magnification::integer().
setPrintMagnification(#wx_ref{type=ThisT}=This,Magnification)
 when is_integer(Magnification) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Magnification,?get_env(),?wxStyledTextCtrl_SetPrintMagnification).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintmagnification">external documentation</a>.
-spec getPrintMagnification(This) -> integer() when
	This::wxStyledTextCtrl().
getPrintMagnification(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetPrintMagnification),
  wxe_util:rec(?wxStyledTextCtrl_GetPrintMagnification).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetprintcolourmode">external documentation</a>.
-spec setPrintColourMode(This, Mode) -> 'ok' when
	This::wxStyledTextCtrl(), Mode::integer().
setPrintColourMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxStyledTextCtrl_SetPrintColourMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintcolourmode">external documentation</a>.
-spec getPrintColourMode(This) -> integer() when
	This::wxStyledTextCtrl().
getPrintColourMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetPrintColourMode),
  wxe_util:rec(?wxStyledTextCtrl_GetPrintColourMode).

%% @equiv findText(This,MinPos,MaxPos,Text, [])
-spec findText(This, MinPos, MaxPos, Text) -> integer() when
	This::wxStyledTextCtrl(), MinPos::integer(), MaxPos::integer(), Text::unicode:chardata().

findText(This,MinPos,MaxPos,Text)
 when is_record(This, wx_ref),is_integer(MinPos),is_integer(MaxPos),?is_chardata(Text) ->
  findText(This,MinPos,MaxPos,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlfindtext">external documentation</a>.
-spec findText(This, MinPos, MaxPos, Text, [Option]) -> integer() when
	This::wxStyledTextCtrl(), MinPos::integer(), MaxPos::integer(), Text::unicode:chardata(),
	Option :: {'flags', integer()}.
findText(#wx_ref{type=ThisT}=This,MinPos,MaxPos,Text, Options)
 when is_integer(MinPos),is_integer(MaxPos),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,MinPos,MaxPos,Text_UC, Opts,?get_env(),?wxStyledTextCtrl_FindText),
  wxe_util:rec(?wxStyledTextCtrl_FindText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlformatrange">external documentation</a>.
-spec formatRange(This, DoDraw, StartPos, EndPos, Draw, Target, RenderRect, PageRect) -> integer() when
	This::wxStyledTextCtrl(), DoDraw::boolean(), StartPos::integer(), EndPos::integer(), Draw::wxDC:wxDC(), Target::wxDC:wxDC(), RenderRect::{X::integer(), Y::integer(), W::integer(), H::integer()}, PageRect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
formatRange(#wx_ref{type=ThisT}=This,DoDraw,StartPos,EndPos,#wx_ref{type=DrawT}=Draw,#wx_ref{type=TargetT}=Target,{RenderRectX,RenderRectY,RenderRectW,RenderRectH} = RenderRect,{PageRectX,PageRectY,PageRectW,PageRectH} = PageRect)
 when is_boolean(DoDraw),is_integer(StartPos),is_integer(EndPos),is_integer(RenderRectX),is_integer(RenderRectY),is_integer(RenderRectW),is_integer(RenderRectH),is_integer(PageRectX),is_integer(PageRectY),is_integer(PageRectW),is_integer(PageRectH) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(DrawT,wxDC),
  ?CLASS(TargetT,wxDC),
  wxe_util:queue_cmd(This,DoDraw,StartPos,EndPos,Draw,Target,RenderRect,PageRect,?get_env(),?wxStyledTextCtrl_FormatRange),
  wxe_util:rec(?wxStyledTextCtrl_FormatRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfirstvisibleline">external documentation</a>.
-spec getFirstVisibleLine(This) -> integer() when
	This::wxStyledTextCtrl().
getFirstVisibleLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetFirstVisibleLine),
  wxe_util:rec(?wxStyledTextCtrl_GetFirstVisibleLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetline">external documentation</a>.
-spec getLine(This, Line) -> unicode:charlist() when
	This::wxStyledTextCtrl(), Line::integer().
getLine(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLine),
  wxe_util:rec(?wxStyledTextCtrl_GetLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinecount">external documentation</a>.
-spec getLineCount(This) -> integer() when
	This::wxStyledTextCtrl().
getLineCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetLineCount),
  wxe_util:rec(?wxStyledTextCtrl_GetLineCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginleft">external documentation</a>.
-spec setMarginLeft(This, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), PixelWidth::integer().
setMarginLeft(#wx_ref{type=ThisT}=This,PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PixelWidth,?get_env(),?wxStyledTextCtrl_SetMarginLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginleft">external documentation</a>.
-spec getMarginLeft(This) -> integer() when
	This::wxStyledTextCtrl().
getMarginLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetMarginLeft),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginright">external documentation</a>.
-spec setMarginRight(This, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), PixelWidth::integer().
setMarginRight(#wx_ref{type=ThisT}=This,PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PixelWidth,?get_env(),?wxStyledTextCtrl_SetMarginRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginright">external documentation</a>.
-spec getMarginRight(This) -> integer() when
	This::wxStyledTextCtrl().
getMarginRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetMarginRight),
  wxe_util:rec(?wxStyledTextCtrl_GetMarginRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmodify">external documentation</a>.
-spec getModify(This) -> boolean() when
	This::wxStyledTextCtrl().
getModify(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetModify),
  wxe_util:rec(?wxStyledTextCtrl_GetModify).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselection">external documentation</a>.
-spec setSelection(This, From, To) -> 'ok' when
	This::wxStyledTextCtrl(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxStyledTextCtrl_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectedtext">external documentation</a>.
-spec getSelectedText(This) -> unicode:charlist() when
	This::wxStyledTextCtrl().
getSelectedText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelectedText),
  wxe_util:rec(?wxStyledTextCtrl_GetSelectedText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextrange">external documentation</a>.
-spec getTextRange(This, StartPos, EndPos) -> unicode:charlist() when
	This::wxStyledTextCtrl(), StartPos::integer(), EndPos::integer().
getTextRange(#wx_ref{type=ThisT}=This,StartPos,EndPos)
 when is_integer(StartPos),is_integer(EndPos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,StartPos,EndPos,?get_env(),?wxStyledTextCtrl_GetTextRange),
  wxe_util:rec(?wxStyledTextCtrl_GetTextRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhideselection">external documentation</a>.
-spec hideSelection(This, Hide) -> 'ok' when
	This::wxStyledTextCtrl(), Hide::boolean().
hideSelection(#wx_ref{type=ThisT}=This,Hide)
 when is_boolean(Hide) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Hide,?get_env(),?wxStyledTextCtrl_HideSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinefromposition">external documentation</a>.
-spec lineFromPosition(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
lineFromPosition(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_LineFromPosition),
  wxe_util:rec(?wxStyledTextCtrl_LineFromPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfromline">external documentation</a>.
-spec positionFromLine(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
positionFromLine(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_PositionFromLine),
  wxe_util:rec(?wxStyledTextCtrl_PositionFromLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinescroll">external documentation</a>.
-spec lineScroll(This, Columns, Lines) -> 'ok' when
	This::wxStyledTextCtrl(), Columns::integer(), Lines::integer().
lineScroll(#wx_ref{type=ThisT}=This,Columns,Lines)
 when is_integer(Columns),is_integer(Lines) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Columns,Lines,?get_env(),?wxStyledTextCtrl_LineScroll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlensurecaretvisible">external documentation</a>.
-spec ensureCaretVisible(This) -> 'ok' when
	This::wxStyledTextCtrl().
ensureCaretVisible(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_EnsureCaretVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlreplaceselection">external documentation</a>.
-spec replaceSelection(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
replaceSelection(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_ReplaceSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetreadonly">external documentation</a>.
-spec setReadOnly(This, ReadOnly) -> 'ok' when
	This::wxStyledTextCtrl(), ReadOnly::boolean().
setReadOnly(#wx_ref{type=ThisT}=This,ReadOnly)
 when is_boolean(ReadOnly) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,ReadOnly,?get_env(),?wxStyledTextCtrl_SetReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcanpaste">external documentation</a>.
-spec canPaste(This) -> boolean() when
	This::wxStyledTextCtrl().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CanPaste),
  wxe_util:rec(?wxStyledTextCtrl_CanPaste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcanundo">external documentation</a>.
-spec canUndo(This) -> boolean() when
	This::wxStyledTextCtrl().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CanUndo),
  wxe_util:rec(?wxStyledTextCtrl_CanUndo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlemptyundobuffer">external documentation</a>.
-spec emptyUndoBuffer(This) -> 'ok' when
	This::wxStyledTextCtrl().
emptyUndoBuffer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_EmptyUndoBuffer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlundo">external documentation</a>.
-spec undo(This) -> 'ok' when
	This::wxStyledTextCtrl().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Undo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcut">external documentation</a>.
-spec cut(This) -> 'ok' when
	This::wxStyledTextCtrl().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Cut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcopy">external documentation</a>.
-spec copy(This) -> 'ok' when
	This::wxStyledTextCtrl().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Copy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpaste">external documentation</a>.
-spec paste(This) -> 'ok' when
	This::wxStyledTextCtrl().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Paste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxStyledTextCtrl().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettext">external documentation</a>.
-spec setText(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_SetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxStyledTextCtrl().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetText),
  wxe_util:rec(?wxStyledTextCtrl_GetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextlength">external documentation</a>.
-spec getTextLength(This) -> integer() when
	This::wxStyledTextCtrl().
getTextLength(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTextLength),
  wxe_util:rec(?wxStyledTextCtrl_GetTextLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetovertype">external documentation</a>.
-spec getOvertype(This) -> boolean() when
	This::wxStyledTextCtrl().
getOvertype(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetOvertype),
  wxe_util:rec(?wxStyledTextCtrl_GetOvertype).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretwidth">external documentation</a>.
-spec setCaretWidth(This, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), PixelWidth::integer().
setCaretWidth(#wx_ref{type=ThisT}=This,PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PixelWidth,?get_env(),?wxStyledTextCtrl_SetCaretWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretwidth">external documentation</a>.
-spec getCaretWidth(This) -> integer() when
	This::wxStyledTextCtrl().
getCaretWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretWidth),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettargetstart">external documentation</a>.
-spec setTargetStart(This, Start) -> 'ok' when
	This::wxStyledTextCtrl(), Start::integer().
setTargetStart(#wx_ref{type=ThisT}=This,Start)
 when is_integer(Start) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Start,?get_env(),?wxStyledTextCtrl_SetTargetStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettargetstart">external documentation</a>.
-spec getTargetStart(This) -> integer() when
	This::wxStyledTextCtrl().
getTargetStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTargetStart),
  wxe_util:rec(?wxStyledTextCtrl_GetTargetStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettargetend">external documentation</a>.
-spec setTargetEnd(This, End) -> 'ok' when
	This::wxStyledTextCtrl(), End::integer().
setTargetEnd(#wx_ref{type=ThisT}=This,End)
 when is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,End,?get_env(),?wxStyledTextCtrl_SetTargetEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettargetend">external documentation</a>.
-spec getTargetEnd(This) -> integer() when
	This::wxStyledTextCtrl().
getTargetEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTargetEnd),
  wxe_util:rec(?wxStyledTextCtrl_GetTargetEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlreplacetarget">external documentation</a>.
-spec replaceTarget(This, Text) -> integer() when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
replaceTarget(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_ReplaceTarget),
  wxe_util:rec(?wxStyledTextCtrl_ReplaceTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchintarget">external documentation</a>.
-spec searchInTarget(This, Text) -> integer() when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
searchInTarget(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_SearchInTarget),
  wxe_util:rec(?wxStyledTextCtrl_SearchInTarget).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetsearchflags">external documentation</a>.
-spec setSearchFlags(This, SearchFlags) -> 'ok' when
	This::wxStyledTextCtrl(), SearchFlags::integer().
setSearchFlags(#wx_ref{type=ThisT}=This,SearchFlags)
 when is_integer(SearchFlags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,SearchFlags,?get_env(),?wxStyledTextCtrl_SetSearchFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetsearchflags">external documentation</a>.
-spec getSearchFlags(This) -> integer() when
	This::wxStyledTextCtrl().
getSearchFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSearchFlags),
  wxe_util:rec(?wxStyledTextCtrl_GetSearchFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipshow">external documentation</a>.
-spec callTipShow(This, Pos, Definition) -> 'ok' when
	This::wxStyledTextCtrl(), Pos::integer(), Definition::unicode:chardata().
callTipShow(#wx_ref{type=ThisT}=This,Pos,Definition)
 when is_integer(Pos),?is_chardata(Definition) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Definition_UC = unicode:characters_to_binary(Definition),
  wxe_util:queue_cmd(This,Pos,Definition_UC,?get_env(),?wxStyledTextCtrl_CallTipShow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipcancel">external documentation</a>.
-spec callTipCancel(This) -> 'ok' when
	This::wxStyledTextCtrl().
callTipCancel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CallTipCancel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipactive">external documentation</a>.
-spec callTipActive(This) -> boolean() when
	This::wxStyledTextCtrl().
callTipActive(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CallTipActive),
  wxe_util:rec(?wxStyledTextCtrl_CallTipActive).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipposatstart">external documentation</a>.
-spec callTipPosAtStart(This) -> integer() when
	This::wxStyledTextCtrl().
callTipPosAtStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CallTipPosAtStart),
  wxe_util:rec(?wxStyledTextCtrl_CallTipPosAtStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsethighlight">external documentation</a>.
-spec callTipSetHighlight(This, HighlightStart, HighlightEnd) -> 'ok' when
	This::wxStyledTextCtrl(), HighlightStart::integer(), HighlightEnd::integer().
callTipSetHighlight(#wx_ref{type=ThisT}=This,HighlightStart,HighlightEnd)
 when is_integer(HighlightStart),is_integer(HighlightEnd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,HighlightStart,HighlightEnd,?get_env(),?wxStyledTextCtrl_CallTipSetHighlight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetbackground">external documentation</a>.
-spec callTipSetBackground(This, Back) -> 'ok' when
	This::wxStyledTextCtrl(), Back::wx:wx_colour().
callTipSetBackground(#wx_ref{type=ThisT}=This,Back)
 when tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_CallTipSetBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetforeground">external documentation</a>.
-spec callTipSetForeground(This, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), Fore::wx:wx_colour().
callTipSetForeground(#wx_ref{type=ThisT}=This,Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_CallTipSetForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetforegroundhighlight">external documentation</a>.
-spec callTipSetForegroundHighlight(This, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), Fore::wx:wx_colour().
callTipSetForegroundHighlight(#wx_ref{type=ThisT}=This,Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_CallTipSetForegroundHighlight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipusestyle">external documentation</a>.
-spec callTipUseStyle(This, TabSize) -> 'ok' when
	This::wxStyledTextCtrl(), TabSize::integer().
callTipUseStyle(#wx_ref{type=ThisT}=This,TabSize)
 when is_integer(TabSize) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,TabSize,?get_env(),?wxStyledTextCtrl_CallTipUseStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvisiblefromdocline">external documentation</a>.
-spec visibleFromDocLine(This, DocLine) -> integer() when
	This::wxStyledTextCtrl(), DocLine::integer().
visibleFromDocLine(#wx_ref{type=ThisT}=This,DocLine)
 when is_integer(DocLine) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,DocLine,?get_env(),?wxStyledTextCtrl_VisibleFromDocLine),
  wxe_util:rec(?wxStyledTextCtrl_VisibleFromDocLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldoclinefromvisible">external documentation</a>.
-spec docLineFromVisible(This, DisplayLine) -> integer() when
	This::wxStyledTextCtrl(), DisplayLine::integer().
docLineFromVisible(#wx_ref{type=ThisT}=This,DisplayLine)
 when is_integer(DisplayLine) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,DisplayLine,?get_env(),?wxStyledTextCtrl_DocLineFromVisible),
  wxe_util:rec(?wxStyledTextCtrl_DocLineFromVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwrapcount">external documentation</a>.
-spec wrapCount(This, DocLine) -> integer() when
	This::wxStyledTextCtrl(), DocLine::integer().
wrapCount(#wx_ref{type=ThisT}=This,DocLine)
 when is_integer(DocLine) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,DocLine,?get_env(),?wxStyledTextCtrl_WrapCount),
  wxe_util:rec(?wxStyledTextCtrl_WrapCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldlevel">external documentation</a>.
-spec setFoldLevel(This, Line, Level) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), Level::integer().
setFoldLevel(#wx_ref{type=ThisT}=This,Line,Level)
 when is_integer(Line),is_integer(Level) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,Level,?get_env(),?wxStyledTextCtrl_SetFoldLevel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldlevel">external documentation</a>.
-spec getFoldLevel(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getFoldLevel(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetFoldLevel),
  wxe_util:rec(?wxStyledTextCtrl_GetFoldLevel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlastchild">external documentation</a>.
-spec getLastChild(This, Line, Level) -> integer() when
	This::wxStyledTextCtrl(), Line::integer(), Level::integer().
getLastChild(#wx_ref{type=ThisT}=This,Line,Level)
 when is_integer(Line),is_integer(Level) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,Level,?get_env(),?wxStyledTextCtrl_GetLastChild),
  wxe_util:rec(?wxStyledTextCtrl_GetLastChild).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldparent">external documentation</a>.
-spec getFoldParent(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
getFoldParent(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetFoldParent),
  wxe_util:rec(?wxStyledTextCtrl_GetFoldParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlshowlines">external documentation</a>.
-spec showLines(This, LineStart, LineEnd) -> 'ok' when
	This::wxStyledTextCtrl(), LineStart::integer(), LineEnd::integer().
showLines(#wx_ref{type=ThisT}=This,LineStart,LineEnd)
 when is_integer(LineStart),is_integer(LineEnd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,LineStart,LineEnd,?get_env(),?wxStyledTextCtrl_ShowLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhidelines">external documentation</a>.
-spec hideLines(This, LineStart, LineEnd) -> 'ok' when
	This::wxStyledTextCtrl(), LineStart::integer(), LineEnd::integer().
hideLines(#wx_ref{type=ThisT}=This,LineStart,LineEnd)
 when is_integer(LineStart),is_integer(LineEnd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,LineStart,LineEnd,?get_env(),?wxStyledTextCtrl_HideLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinevisible">external documentation</a>.
-spec getLineVisible(This, Line) -> boolean() when
	This::wxStyledTextCtrl(), Line::integer().
getLineVisible(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineVisible),
  wxe_util:rec(?wxStyledTextCtrl_GetLineVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldexpanded">external documentation</a>.
-spec setFoldExpanded(This, Line, Expanded) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer(), Expanded::boolean().
setFoldExpanded(#wx_ref{type=ThisT}=This,Line,Expanded)
 when is_integer(Line),is_boolean(Expanded) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,Expanded,?get_env(),?wxStyledTextCtrl_SetFoldExpanded).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldexpanded">external documentation</a>.
-spec getFoldExpanded(This, Line) -> boolean() when
	This::wxStyledTextCtrl(), Line::integer().
getFoldExpanded(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetFoldExpanded),
  wxe_util:rec(?wxStyledTextCtrl_GetFoldExpanded).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltogglefold">external documentation</a>.
-spec toggleFold(This, Line) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer().
toggleFold(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_ToggleFold).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlensurevisible">external documentation</a>.
-spec ensureVisible(This, Line) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer().
ensureVisible(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_EnsureVisible).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldflags">external documentation</a>.
-spec setFoldFlags(This, Flags) -> 'ok' when
	This::wxStyledTextCtrl(), Flags::integer().
setFoldFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxStyledTextCtrl_SetFoldFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlensurevisibleenforcepolicy">external documentation</a>.
-spec ensureVisibleEnforcePolicy(This, Line) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer().
ensureVisibleEnforcePolicy(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_EnsureVisibleEnforcePolicy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettabindents">external documentation</a>.
-spec setTabIndents(This, TabIndents) -> 'ok' when
	This::wxStyledTextCtrl(), TabIndents::boolean().
setTabIndents(#wx_ref{type=ThisT}=This,TabIndents)
 when is_boolean(TabIndents) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,TabIndents,?get_env(),?wxStyledTextCtrl_SetTabIndents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettabindents">external documentation</a>.
-spec getTabIndents(This) -> boolean() when
	This::wxStyledTextCtrl().
getTabIndents(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTabIndents),
  wxe_util:rec(?wxStyledTextCtrl_GetTabIndents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetbackspaceunindents">external documentation</a>.
-spec setBackSpaceUnIndents(This, BsUnIndents) -> 'ok' when
	This::wxStyledTextCtrl(), BsUnIndents::boolean().
setBackSpaceUnIndents(#wx_ref{type=ThisT}=This,BsUnIndents)
 when is_boolean(BsUnIndents) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,BsUnIndents,?get_env(),?wxStyledTextCtrl_SetBackSpaceUnIndents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetbackspaceunindents">external documentation</a>.
-spec getBackSpaceUnIndents(This) -> boolean() when
	This::wxStyledTextCtrl().
getBackSpaceUnIndents(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetBackSpaceUnIndents),
  wxe_util:rec(?wxStyledTextCtrl_GetBackSpaceUnIndents).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmousedwelltime">external documentation</a>.
-spec setMouseDwellTime(This, PeriodMilliseconds) -> 'ok' when
	This::wxStyledTextCtrl(), PeriodMilliseconds::integer().
setMouseDwellTime(#wx_ref{type=ThisT}=This,PeriodMilliseconds)
 when is_integer(PeriodMilliseconds) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PeriodMilliseconds,?get_env(),?wxStyledTextCtrl_SetMouseDwellTime).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmousedwelltime">external documentation</a>.
-spec getMouseDwellTime(This) -> integer() when
	This::wxStyledTextCtrl().
getMouseDwellTime(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetMouseDwellTime),
  wxe_util:rec(?wxStyledTextCtrl_GetMouseDwellTime).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordstartposition">external documentation</a>.
-spec wordStartPosition(This, Pos, OnlyWordCharacters) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer(), OnlyWordCharacters::boolean().
wordStartPosition(#wx_ref{type=ThisT}=This,Pos,OnlyWordCharacters)
 when is_integer(Pos),is_boolean(OnlyWordCharacters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,OnlyWordCharacters,?get_env(),?wxStyledTextCtrl_WordStartPosition),
  wxe_util:rec(?wxStyledTextCtrl_WordStartPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordendposition">external documentation</a>.
-spec wordEndPosition(This, Pos, OnlyWordCharacters) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer(), OnlyWordCharacters::boolean().
wordEndPosition(#wx_ref{type=ThisT}=This,Pos,OnlyWordCharacters)
 when is_integer(Pos),is_boolean(OnlyWordCharacters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,OnlyWordCharacters,?get_env(),?wxStyledTextCtrl_WordEndPosition),
  wxe_util:rec(?wxStyledTextCtrl_WordEndPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapmode">external documentation</a>.
-spec setWrapMode(This, WrapMode) -> 'ok' when
	This::wxStyledTextCtrl(), WrapMode::integer().
setWrapMode(#wx_ref{type=ThisT}=This,WrapMode)
 when is_integer(WrapMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,WrapMode,?get_env(),?wxStyledTextCtrl_SetWrapMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapmode">external documentation</a>.
-spec getWrapMode(This) -> integer() when
	This::wxStyledTextCtrl().
getWrapMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetWrapMode),
  wxe_util:rec(?wxStyledTextCtrl_GetWrapMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapvisualflags">external documentation</a>.
-spec setWrapVisualFlags(This, WrapVisualFlags) -> 'ok' when
	This::wxStyledTextCtrl(), WrapVisualFlags::integer().
setWrapVisualFlags(#wx_ref{type=ThisT}=This,WrapVisualFlags)
 when is_integer(WrapVisualFlags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,WrapVisualFlags,?get_env(),?wxStyledTextCtrl_SetWrapVisualFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapvisualflags">external documentation</a>.
-spec getWrapVisualFlags(This) -> integer() when
	This::wxStyledTextCtrl().
getWrapVisualFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetWrapVisualFlags),
  wxe_util:rec(?wxStyledTextCtrl_GetWrapVisualFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapvisualflagslocation">external documentation</a>.
-spec setWrapVisualFlagsLocation(This, WrapVisualFlagsLocation) -> 'ok' when
	This::wxStyledTextCtrl(), WrapVisualFlagsLocation::integer().
setWrapVisualFlagsLocation(#wx_ref{type=ThisT}=This,WrapVisualFlagsLocation)
 when is_integer(WrapVisualFlagsLocation) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,WrapVisualFlagsLocation,?get_env(),?wxStyledTextCtrl_SetWrapVisualFlagsLocation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapvisualflagslocation">external documentation</a>.
-spec getWrapVisualFlagsLocation(This) -> integer() when
	This::wxStyledTextCtrl().
getWrapVisualFlagsLocation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetWrapVisualFlagsLocation),
  wxe_util:rec(?wxStyledTextCtrl_GetWrapVisualFlagsLocation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapstartindent">external documentation</a>.
-spec setWrapStartIndent(This, Indent) -> 'ok' when
	This::wxStyledTextCtrl(), Indent::integer().
setWrapStartIndent(#wx_ref{type=ThisT}=This,Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Indent,?get_env(),?wxStyledTextCtrl_SetWrapStartIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapstartindent">external documentation</a>.
-spec getWrapStartIndent(This) -> integer() when
	This::wxStyledTextCtrl().
getWrapStartIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetWrapStartIndent),
  wxe_util:rec(?wxStyledTextCtrl_GetWrapStartIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlayoutcache">external documentation</a>.
-spec setLayoutCache(This, CacheMode) -> 'ok' when
	This::wxStyledTextCtrl(), CacheMode::integer().
setLayoutCache(#wx_ref{type=ThisT}=This,CacheMode)
 when is_integer(CacheMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CacheMode,?get_env(),?wxStyledTextCtrl_SetLayoutCache).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlayoutcache">external documentation</a>.
-spec getLayoutCache(This) -> integer() when
	This::wxStyledTextCtrl().
getLayoutCache(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetLayoutCache),
  wxe_util:rec(?wxStyledTextCtrl_GetLayoutCache).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetscrollwidth">external documentation</a>.
-spec setScrollWidth(This, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), PixelWidth::integer().
setScrollWidth(#wx_ref{type=ThisT}=This,PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PixelWidth,?get_env(),?wxStyledTextCtrl_SetScrollWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetscrollwidth">external documentation</a>.
-spec getScrollWidth(This) -> integer() when
	This::wxStyledTextCtrl().
getScrollWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetScrollWidth),
  wxe_util:rec(?wxStyledTextCtrl_GetScrollWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltextwidth">external documentation</a>.
-spec textWidth(This, Style, Text) -> integer() when
	This::wxStyledTextCtrl(), Style::integer(), Text::unicode:chardata().
textWidth(#wx_ref{type=ThisT}=This,Style,Text)
 when is_integer(Style),?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Style,Text_UC,?get_env(),?wxStyledTextCtrl_TextWidth),
  wxe_util:rec(?wxStyledTextCtrl_TextWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetendatlastline">external documentation</a>.
-spec getEndAtLastLine(This) -> boolean() when
	This::wxStyledTextCtrl().
getEndAtLastLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEndAtLastLine),
  wxe_util:rec(?wxStyledTextCtrl_GetEndAtLastLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltextheight">external documentation</a>.
-spec textHeight(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
textHeight(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_TextHeight),
  wxe_util:rec(?wxStyledTextCtrl_TextHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetuseverticalscrollbar">external documentation</a>.
-spec setUseVerticalScrollBar(This, Visible) -> 'ok' when
	This::wxStyledTextCtrl(), Visible::boolean().
setUseVerticalScrollBar(#wx_ref{type=ThisT}=This,Visible)
 when is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Visible,?get_env(),?wxStyledTextCtrl_SetUseVerticalScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetuseverticalscrollbar">external documentation</a>.
-spec getUseVerticalScrollBar(This) -> boolean() when
	This::wxStyledTextCtrl().
getUseVerticalScrollBar(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetUseVerticalScrollBar),
  wxe_util:rec(?wxStyledTextCtrl_GetUseVerticalScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlappendtext">external documentation</a>.
-spec appendText(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::unicode:chardata().
appendText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxStyledTextCtrl_AppendText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettwophasedraw">external documentation</a>.
-spec getTwoPhaseDraw(This) -> boolean() when
	This::wxStyledTextCtrl().
getTwoPhaseDraw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTwoPhaseDraw),
  wxe_util:rec(?wxStyledTextCtrl_GetTwoPhaseDraw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettwophasedraw">external documentation</a>.
-spec setTwoPhaseDraw(This, TwoPhase) -> 'ok' when
	This::wxStyledTextCtrl(), TwoPhase::boolean().
setTwoPhaseDraw(#wx_ref{type=ThisT}=This,TwoPhase)
 when is_boolean(TwoPhase) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,TwoPhase,?get_env(),?wxStyledTextCtrl_SetTwoPhaseDraw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltargetfromselection">external documentation</a>.
-spec targetFromSelection(This) -> 'ok' when
	This::wxStyledTextCtrl().
targetFromSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_TargetFromSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinesjoin">external documentation</a>.
-spec linesJoin(This) -> 'ok' when
	This::wxStyledTextCtrl().
linesJoin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LinesJoin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinessplit">external documentation</a>.
-spec linesSplit(This, PixelWidth) -> 'ok' when
	This::wxStyledTextCtrl(), PixelWidth::integer().
linesSplit(#wx_ref{type=ThisT}=This,PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PixelWidth,?get_env(),?wxStyledTextCtrl_LinesSplit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldmargincolour">external documentation</a>.
-spec setFoldMarginColour(This, UseSetting, Back) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Back::wx:wx_colour().
setFoldMarginColour(#wx_ref{type=ThisT}=This,UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_SetFoldMarginColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldmarginhicolour">external documentation</a>.
-spec setFoldMarginHiColour(This, UseSetting, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Fore::wx:wx_colour().
setFoldMarginHiColour(#wx_ref{type=ThisT}=This,UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_SetFoldMarginHiColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinedown">external documentation</a>.
-spec lineDown(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinedownextend">external documentation</a>.
-spec lineDownExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineDownExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineDownExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineup">external documentation</a>.
-spec lineUp(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineupextend">external documentation</a>.
-spec lineUpExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineUpExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineUpExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleft">external documentation</a>.
-spec charLeft(This) -> 'ok' when
	This::wxStyledTextCtrl().
charLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleftextend">external documentation</a>.
-spec charLeftExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
charLeftExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharLeftExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharright">external documentation</a>.
-spec charRight(This) -> 'ok' when
	This::wxStyledTextCtrl().
charRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharrightextend">external documentation</a>.
-spec charRightExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
charRightExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharRightExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleft">external documentation</a>.
-spec wordLeft(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftextend">external documentation</a>.
-spec wordLeftExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordLeftExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordLeftExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordright">external documentation</a>.
-spec wordRight(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightextend">external documentation</a>.
-spec wordRightExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordRightExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordRightExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhome">external documentation</a>.
-spec home(This) -> 'ok' when
	This::wxStyledTextCtrl().
home(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Home).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhomeextend">external documentation</a>.
-spec homeExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
homeExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_HomeExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineend">external documentation</a>.
-spec lineEnd(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineendextend">external documentation</a>.
-spec lineEndExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentstart">external documentation</a>.
-spec documentStart(This) -> 'ok' when
	This::wxStyledTextCtrl().
documentStart(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DocumentStart).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentstartextend">external documentation</a>.
-spec documentStartExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
documentStartExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DocumentStartExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentend">external documentation</a>.
-spec documentEnd(This) -> 'ok' when
	This::wxStyledTextCtrl().
documentEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DocumentEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentendextend">external documentation</a>.
-spec documentEndExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
documentEndExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DocumentEndExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpageup">external documentation</a>.
-spec pageUp(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpageupextend">external documentation</a>.
-spec pageUpExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageUpExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageUpExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedown">external documentation</a>.
-spec pageDown(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedownextend">external documentation</a>.
-spec pageDownExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageDownExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageDownExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrledittoggleovertype">external documentation</a>.
-spec editToggleOvertype(This) -> 'ok' when
	This::wxStyledTextCtrl().
editToggleOvertype(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_EditToggleOvertype).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcancel">external documentation</a>.
-spec cancel(This) -> 'ok' when
	This::wxStyledTextCtrl().
cancel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Cancel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldeleteback">external documentation</a>.
-spec deleteBack(This) -> 'ok' when
	This::wxStyledTextCtrl().
deleteBack(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DeleteBack).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltab">external documentation</a>.
-spec tab(This) -> 'ok' when
	This::wxStyledTextCtrl().
tab(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_Tab).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlbacktab">external documentation</a>.
-spec backTab(This) -> 'ok' when
	This::wxStyledTextCtrl().
backTab(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_BackTab).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlnewline">external documentation</a>.
-spec newLine(This) -> 'ok' when
	This::wxStyledTextCtrl().
newLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_NewLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlformfeed">external documentation</a>.
-spec formFeed(This) -> 'ok' when
	This::wxStyledTextCtrl().
formFeed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_FormFeed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvchome">external documentation</a>.
-spec vCHome(This) -> 'ok' when
	This::wxStyledTextCtrl().
vCHome(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_VCHome).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomeextend">external documentation</a>.
-spec vCHomeExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
vCHomeExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_VCHomeExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlzoomin">external documentation</a>.
-spec zoomIn(This) -> 'ok' when
	This::wxStyledTextCtrl().
zoomIn(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ZoomIn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlzoomout">external documentation</a>.
-spec zoomOut(This) -> 'ok' when
	This::wxStyledTextCtrl().
zoomOut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ZoomOut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldelwordleft">external documentation</a>.
-spec delWordLeft(This) -> 'ok' when
	This::wxStyledTextCtrl().
delWordLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DelWordLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldelwordright">external documentation</a>.
-spec delWordRight(This) -> 'ok' when
	This::wxStyledTextCtrl().
delWordRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DelWordRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinecut">external documentation</a>.
-spec lineCut(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineCut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinedelete">external documentation</a>.
-spec lineDelete(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineDelete(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineDelete).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinetranspose">external documentation</a>.
-spec lineTranspose(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineTranspose(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineTranspose).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineduplicate">external documentation</a>.
-spec lineDuplicate(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineDuplicate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineDuplicate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllowercase">external documentation</a>.
-spec lowerCase(This) -> 'ok' when
	This::wxStyledTextCtrl().
lowerCase(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LowerCase).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrluppercase">external documentation</a>.
-spec upperCase(This) -> 'ok' when
	This::wxStyledTextCtrl().
upperCase(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_UpperCase).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinescrolldown">external documentation</a>.
-spec lineScrollDown(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineScrollDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineScrollDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinescrollup">external documentation</a>.
-spec lineScrollUp(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineScrollUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineScrollUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldeletebacknotline">external documentation</a>.
-spec deleteBackNotLine(This) -> 'ok' when
	This::wxStyledTextCtrl().
deleteBackNotLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DeleteBackNotLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhomedisplay">external documentation</a>.
-spec homeDisplay(This) -> 'ok' when
	This::wxStyledTextCtrl().
homeDisplay(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_HomeDisplay).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhomedisplayextend">external documentation</a>.
-spec homeDisplayExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
homeDisplayExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_HomeDisplayExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineenddisplay">external documentation</a>.
-spec lineEndDisplay(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndDisplay(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndDisplay).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineenddisplayextend">external documentation</a>.
-spec lineEndDisplayExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndDisplayExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndDisplayExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhomewrapextend">external documentation</a>.
-spec homeWrapExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
homeWrapExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_HomeWrapExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineendwrap">external documentation</a>.
-spec lineEndWrap(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndWrap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndWrap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineendwrapextend">external documentation</a>.
-spec lineEndWrapExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndWrapExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndWrapExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomewrap">external documentation</a>.
-spec vCHomeWrap(This) -> 'ok' when
	This::wxStyledTextCtrl().
vCHomeWrap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_VCHomeWrap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomewrapextend">external documentation</a>.
-spec vCHomeWrapExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
vCHomeWrapExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_VCHomeWrapExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinecopy">external documentation</a>.
-spec lineCopy(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineCopy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlmovecaretinsideview">external documentation</a>.
-spec moveCaretInsideView(This) -> 'ok' when
	This::wxStyledTextCtrl().
moveCaretInsideView(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_MoveCaretInsideView).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinelength">external documentation</a>.
-spec lineLength(This, Line) -> integer() when
	This::wxStyledTextCtrl(), Line::integer().
lineLength(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_LineLength),
  wxe_util:rec(?wxStyledTextCtrl_LineLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlbracehighlight">external documentation</a>.
-spec braceHighlight(This, PosA, PosB) -> 'ok' when
	This::wxStyledTextCtrl(), PosA::integer(), PosB::integer().
braceHighlight(#wx_ref{type=ThisT}=This,PosA,PosB)
 when is_integer(PosA),is_integer(PosB) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PosA,PosB,?get_env(),?wxStyledTextCtrl_BraceHighlight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlbracebadlight">external documentation</a>.
-spec braceBadLight(This, Pos) -> 'ok' when
	This::wxStyledTextCtrl(), Pos::integer().
braceBadLight(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_BraceBadLight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlbracematch">external documentation</a>.
-spec braceMatch(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
braceMatch(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_BraceMatch),
  wxe_util:rec(?wxStyledTextCtrl_BraceMatch).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetvieweol">external documentation</a>.
-spec getViewEOL(This) -> boolean() when
	This::wxStyledTextCtrl().
getViewEOL(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetViewEOL),
  wxe_util:rec(?wxStyledTextCtrl_GetViewEOL).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvieweol">external documentation</a>.
-spec setViewEOL(This, Visible) -> 'ok' when
	This::wxStyledTextCtrl(), Visible::boolean().
setViewEOL(#wx_ref{type=ThisT}=This,Visible)
 when is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Visible,?get_env(),?wxStyledTextCtrl_SetViewEOL).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmodeventmask">external documentation</a>.
-spec setModEventMask(This, EventMask) -> 'ok' when
	This::wxStyledTextCtrl(), EventMask::integer().
setModEventMask(#wx_ref{type=ThisT}=This,EventMask)
 when is_integer(EventMask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,EventMask,?get_env(),?wxStyledTextCtrl_SetModEventMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgecolumn">external documentation</a>.
-spec getEdgeColumn(This) -> integer() when
	This::wxStyledTextCtrl().
getEdgeColumn(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEdgeColumn),
  wxe_util:rec(?wxStyledTextCtrl_GetEdgeColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetedgecolumn">external documentation</a>.
-spec setEdgeColumn(This, Column) -> 'ok' when
	This::wxStyledTextCtrl(), Column::integer().
setEdgeColumn(#wx_ref{type=ThisT}=This,Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Column,?get_env(),?wxStyledTextCtrl_SetEdgeColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetedgemode">external documentation</a>.
-spec setEdgeMode(This, EdgeMode) -> 'ok' when
	This::wxStyledTextCtrl(), EdgeMode::integer().
setEdgeMode(#wx_ref{type=ThisT}=This,EdgeMode)
 when is_integer(EdgeMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,EdgeMode,?get_env(),?wxStyledTextCtrl_SetEdgeMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgemode">external documentation</a>.
-spec getEdgeMode(This) -> integer() when
	This::wxStyledTextCtrl().
getEdgeMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEdgeMode),
  wxe_util:rec(?wxStyledTextCtrl_GetEdgeMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgecolour">external documentation</a>.
-spec getEdgeColour(This) -> wx:wx_colour4() when
	This::wxStyledTextCtrl().
getEdgeColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetEdgeColour),
  wxe_util:rec(?wxStyledTextCtrl_GetEdgeColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetedgecolour">external documentation</a>.
-spec setEdgeColour(This, EdgeColour) -> 'ok' when
	This::wxStyledTextCtrl(), EdgeColour::wx:wx_colour().
setEdgeColour(#wx_ref{type=ThisT}=This,EdgeColour)
 when tuple_size(EdgeColour) =:= 3; tuple_size(EdgeColour) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(EdgeColour),?get_env(),?wxStyledTextCtrl_SetEdgeColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchanchor">external documentation</a>.
-spec searchAnchor(This) -> 'ok' when
	This::wxStyledTextCtrl().
searchAnchor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SearchAnchor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchnext">external documentation</a>.
-spec searchNext(This, SearchFlags, Text) -> integer() when
	This::wxStyledTextCtrl(), SearchFlags::integer(), Text::unicode:chardata().
searchNext(#wx_ref{type=ThisT}=This,SearchFlags,Text)
 when is_integer(SearchFlags),?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,SearchFlags,Text_UC,?get_env(),?wxStyledTextCtrl_SearchNext),
  wxe_util:rec(?wxStyledTextCtrl_SearchNext).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchprev">external documentation</a>.
-spec searchPrev(This, SearchFlags, Text) -> integer() when
	This::wxStyledTextCtrl(), SearchFlags::integer(), Text::unicode:chardata().
searchPrev(#wx_ref{type=ThisT}=This,SearchFlags,Text)
 when is_integer(SearchFlags),?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,SearchFlags,Text_UC,?get_env(),?wxStyledTextCtrl_SearchPrev),
  wxe_util:rec(?wxStyledTextCtrl_SearchPrev).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinesonscreen">external documentation</a>.
-spec linesOnScreen(This) -> integer() when
	This::wxStyledTextCtrl().
linesOnScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LinesOnScreen),
  wxe_util:rec(?wxStyledTextCtrl_LinesOnScreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlusepopup">external documentation</a>.
-spec usePopUp(This, PopUpMode) -> 'ok' when
	This::wxStyledTextCtrl(), PopUpMode::integer().
usePopUp(#wx_ref{type=ThisT}=This,PopUpMode)
 when is_integer(PopUpMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,PopUpMode,?get_env(),?wxStyledTextCtrl_UsePopUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlselectionisrectangle">external documentation</a>.
-spec selectionIsRectangle(This) -> boolean() when
	This::wxStyledTextCtrl().
selectionIsRectangle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SelectionIsRectangle),
  wxe_util:rec(?wxStyledTextCtrl_SelectionIsRectangle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetzoom">external documentation</a>.
-spec setZoom(This, ZoomInPoints) -> 'ok' when
	This::wxStyledTextCtrl(), ZoomInPoints::integer().
setZoom(#wx_ref{type=ThisT}=This,ZoomInPoints)
 when is_integer(ZoomInPoints) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,ZoomInPoints,?get_env(),?wxStyledTextCtrl_SetZoom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetzoom">external documentation</a>.
-spec getZoom(This) -> integer() when
	This::wxStyledTextCtrl().
getZoom(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetZoom),
  wxe_util:rec(?wxStyledTextCtrl_GetZoom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmodeventmask">external documentation</a>.
-spec getModEventMask(This) -> integer() when
	This::wxStyledTextCtrl().
getModEventMask(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetModEventMask),
  wxe_util:rec(?wxStyledTextCtrl_GetModEventMask).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstcfocus">external documentation</a>.
-spec setSTCFocus(This, Focus) -> 'ok' when
	This::wxStyledTextCtrl(), Focus::boolean().
setSTCFocus(#wx_ref{type=ThisT}=This,Focus)
 when is_boolean(Focus) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Focus,?get_env(),?wxStyledTextCtrl_SetSTCFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstcfocus">external documentation</a>.
-spec getSTCFocus(This) -> boolean() when
	This::wxStyledTextCtrl().
getSTCFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSTCFocus),
  wxe_util:rec(?wxStyledTextCtrl_GetSTCFocus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstatus">external documentation</a>.
-spec setStatus(This, Status) -> 'ok' when
	This::wxStyledTextCtrl(), Status::integer().
setStatus(#wx_ref{type=ThisT}=This,Status)
 when is_integer(Status) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Status,?get_env(),?wxStyledTextCtrl_SetStatus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstatus">external documentation</a>.
-spec getStatus(This) -> integer() when
	This::wxStyledTextCtrl().
getStatus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetStatus),
  wxe_util:rec(?wxStyledTextCtrl_GetStatus).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmousedowncaptures">external documentation</a>.
-spec setMouseDownCaptures(This, Captures) -> 'ok' when
	This::wxStyledTextCtrl(), Captures::boolean().
setMouseDownCaptures(#wx_ref{type=ThisT}=This,Captures)
 when is_boolean(Captures) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Captures,?get_env(),?wxStyledTextCtrl_SetMouseDownCaptures).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmousedowncaptures">external documentation</a>.
-spec getMouseDownCaptures(This) -> boolean() when
	This::wxStyledTextCtrl().
getMouseDownCaptures(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetMouseDownCaptures),
  wxe_util:rec(?wxStyledTextCtrl_GetMouseDownCaptures).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstccursor">external documentation</a>.
-spec setSTCCursor(This, CursorType) -> 'ok' when
	This::wxStyledTextCtrl(), CursorType::integer().
setSTCCursor(#wx_ref{type=ThisT}=This,CursorType)
 when is_integer(CursorType) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CursorType,?get_env(),?wxStyledTextCtrl_SetSTCCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstccursor">external documentation</a>.
-spec getSTCCursor(This) -> integer() when
	This::wxStyledTextCtrl().
getSTCCursor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSTCCursor),
  wxe_util:rec(?wxStyledTextCtrl_GetSTCCursor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcontrolcharsymbol">external documentation</a>.
-spec setControlCharSymbol(This, Symbol) -> 'ok' when
	This::wxStyledTextCtrl(), Symbol::integer().
setControlCharSymbol(#wx_ref{type=ThisT}=This,Symbol)
 when is_integer(Symbol) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Symbol,?get_env(),?wxStyledTextCtrl_SetControlCharSymbol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcontrolcharsymbol">external documentation</a>.
-spec getControlCharSymbol(This) -> integer() when
	This::wxStyledTextCtrl().
getControlCharSymbol(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetControlCharSymbol),
  wxe_util:rec(?wxStyledTextCtrl_GetControlCharSymbol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartleft">external documentation</a>.
-spec wordPartLeft(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordPartLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordPartLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartleftextend">external documentation</a>.
-spec wordPartLeftExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordPartLeftExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordPartLeftExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartright">external documentation</a>.
-spec wordPartRight(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordPartRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordPartRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartrightextend">external documentation</a>.
-spec wordPartRightExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordPartRightExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordPartRightExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvisiblepolicy">external documentation</a>.
-spec setVisiblePolicy(This, VisiblePolicy, VisibleSlop) -> 'ok' when
	This::wxStyledTextCtrl(), VisiblePolicy::integer(), VisibleSlop::integer().
setVisiblePolicy(#wx_ref{type=ThisT}=This,VisiblePolicy,VisibleSlop)
 when is_integer(VisiblePolicy),is_integer(VisibleSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,VisiblePolicy,VisibleSlop,?get_env(),?wxStyledTextCtrl_SetVisiblePolicy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldellineleft">external documentation</a>.
-spec delLineLeft(This) -> 'ok' when
	This::wxStyledTextCtrl().
delLineLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DelLineLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldellineright">external documentation</a>.
-spec delLineRight(This) -> 'ok' when
	This::wxStyledTextCtrl().
delLineRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_DelLineRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetxoffset">external documentation</a>.
-spec getXOffset(This) -> integer() when
	This::wxStyledTextCtrl().
getXOffset(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetXOffset),
  wxe_util:rec(?wxStyledTextCtrl_GetXOffset).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlchoosecaretx">external documentation</a>.
-spec chooseCaretX(This) -> 'ok' when
	This::wxStyledTextCtrl().
chooseCaretX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ChooseCaretX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetxcaretpolicy">external documentation</a>.
-spec setXCaretPolicy(This, CaretPolicy, CaretSlop) -> 'ok' when
	This::wxStyledTextCtrl(), CaretPolicy::integer(), CaretSlop::integer().
setXCaretPolicy(#wx_ref{type=ThisT}=This,CaretPolicy,CaretSlop)
 when is_integer(CaretPolicy),is_integer(CaretSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CaretPolicy,CaretSlop,?get_env(),?wxStyledTextCtrl_SetXCaretPolicy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetycaretpolicy">external documentation</a>.
-spec setYCaretPolicy(This, CaretPolicy, CaretSlop) -> 'ok' when
	This::wxStyledTextCtrl(), CaretPolicy::integer(), CaretSlop::integer().
setYCaretPolicy(#wx_ref{type=ThisT}=This,CaretPolicy,CaretSlop)
 when is_integer(CaretPolicy),is_integer(CaretSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,CaretPolicy,CaretSlop,?get_env(),?wxStyledTextCtrl_SetYCaretPolicy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintwrapmode">external documentation</a>.
-spec getPrintWrapMode(This) -> integer() when
	This::wxStyledTextCtrl().
getPrintWrapMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetPrintWrapMode),
  wxe_util:rec(?wxStyledTextCtrl_GetPrintWrapMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactiveforeground">external documentation</a>.
-spec setHotspotActiveForeground(This, UseSetting, Fore) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Fore::wx:wx_colour().
setHotspotActiveForeground(#wx_ref{type=ThisT}=This,UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Fore),?get_env(),?wxStyledTextCtrl_SetHotspotActiveForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactivebackground">external documentation</a>.
-spec setHotspotActiveBackground(This, UseSetting, Back) -> 'ok' when
	This::wxStyledTextCtrl(), UseSetting::boolean(), Back::wx:wx_colour().
setHotspotActiveBackground(#wx_ref{type=ThisT}=This,UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseSetting,wxe_util:color(Back),?get_env(),?wxStyledTextCtrl_SetHotspotActiveBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactiveunderline">external documentation</a>.
-spec setHotspotActiveUnderline(This, Underline) -> 'ok' when
	This::wxStyledTextCtrl(), Underline::boolean().
setHotspotActiveUnderline(#wx_ref{type=ThisT}=This,Underline)
 when is_boolean(Underline) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Underline,?get_env(),?wxStyledTextCtrl_SetHotspotActiveUnderline).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotsingleline">external documentation</a>.
-spec setHotspotSingleLine(This, SingleLine) -> 'ok' when
	This::wxStyledTextCtrl(), SingleLine::boolean().
setHotspotSingleLine(#wx_ref{type=ThisT}=This,SingleLine)
 when is_boolean(SingleLine) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,SingleLine,?get_env(),?wxStyledTextCtrl_SetHotspotSingleLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlparadownextend">external documentation</a>.
-spec paraDownExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
paraDownExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ParaDownExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlparaup">external documentation</a>.
-spec paraUp(This) -> 'ok' when
	This::wxStyledTextCtrl().
paraUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ParaUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlparaupextend">external documentation</a>.
-spec paraUpExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
paraUpExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ParaUpExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionbefore">external documentation</a>.
-spec positionBefore(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
positionBefore(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_PositionBefore),
  wxe_util:rec(?wxStyledTextCtrl_PositionBefore).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionafter">external documentation</a>.
-spec positionAfter(This, Pos) -> integer() when
	This::wxStyledTextCtrl(), Pos::integer().
positionAfter(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_PositionAfter),
  wxe_util:rec(?wxStyledTextCtrl_PositionAfter).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcopyrange">external documentation</a>.
-spec copyRange(This, Start, End) -> 'ok' when
	This::wxStyledTextCtrl(), Start::integer(), End::integer().
copyRange(#wx_ref{type=ThisT}=This,Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Start,End,?get_env(),?wxStyledTextCtrl_CopyRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcopytext">external documentation</a>.
-spec copyText(This, Length, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Length::integer(), Text::unicode:chardata().
copyText(#wx_ref{type=ThisT}=This,Length,Text)
 when is_integer(Length),?is_chardata(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Length,Text_UC,?get_env(),?wxStyledTextCtrl_CopyText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionmode">external documentation</a>.
-spec setSelectionMode(This, SelectionMode) -> 'ok' when
	This::wxStyledTextCtrl(), SelectionMode::integer().
setSelectionMode(#wx_ref{type=ThisT}=This,SelectionMode)
 when is_integer(SelectionMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,SelectionMode,?get_env(),?wxStyledTextCtrl_SetSelectionMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionmode">external documentation</a>.
-spec getSelectionMode(This) -> integer() when
	This::wxStyledTextCtrl().
getSelectionMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelectionMode),
  wxe_util:rec(?wxStyledTextCtrl_GetSelectionMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllinedownrectextend">external documentation</a>.
-spec lineDownRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineDownRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineDownRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineuprectextend">external documentation</a>.
-spec lineUpRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineUpRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineUpRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleftrectextend">external documentation</a>.
-spec charLeftRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
charLeftRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharLeftRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcharrightrectextend">external documentation</a>.
-spec charRightRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
charRightRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_CharRightRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlhomerectextend">external documentation</a>.
-spec homeRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
homeRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_HomeRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomerectextend">external documentation</a>.
-spec vCHomeRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
vCHomeRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_VCHomeRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrllineendrectextend">external documentation</a>.
-spec lineEndRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
lineEndRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_LineEndRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpageuprectextend">external documentation</a>.
-spec pageUpRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageUpRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageUpRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedownrectextend">external documentation</a>.
-spec pageDownRectExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
pageDownRectExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_PageDownRectExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpageup">external documentation</a>.
-spec stutteredPageUp(This) -> 'ok' when
	This::wxStyledTextCtrl().
stutteredPageUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StutteredPageUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpageupextend">external documentation</a>.
-spec stutteredPageUpExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
stutteredPageUpExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StutteredPageUpExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpagedown">external documentation</a>.
-spec stutteredPageDown(This) -> 'ok' when
	This::wxStyledTextCtrl().
stutteredPageDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StutteredPageDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpagedownextend">external documentation</a>.
-spec stutteredPageDownExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
stutteredPageDownExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StutteredPageDownExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftend">external documentation</a>.
-spec wordLeftEnd(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordLeftEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordLeftEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftendextend">external documentation</a>.
-spec wordLeftEndExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordLeftEndExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordLeftEndExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightend">external documentation</a>.
-spec wordRightEnd(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordRightEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordRightEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightendextend">external documentation</a>.
-spec wordRightEndExtend(This) -> 'ok' when
	This::wxStyledTextCtrl().
wordRightEndExtend(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_WordRightEndExtend).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespacechars">external documentation</a>.
-spec setWhitespaceChars(This, Characters) -> 'ok' when
	This::wxStyledTextCtrl(), Characters::unicode:chardata().
setWhitespaceChars(#wx_ref{type=ThisT}=This,Characters)
 when ?is_chardata(Characters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Characters_UC = unicode:characters_to_binary(Characters),
  wxe_util:queue_cmd(This,Characters_UC,?get_env(),?wxStyledTextCtrl_SetWhitespaceChars).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcharsdefault">external documentation</a>.
-spec setCharsDefault(This) -> 'ok' when
	This::wxStyledTextCtrl().
setCharsDefault(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SetCharsDefault).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetcurrent">external documentation</a>.
-spec autoCompGetCurrent(This) -> integer() when
	This::wxStyledTextCtrl().
autoCompGetCurrent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_AutoCompGetCurrent),
  wxe_util:rec(?wxStyledTextCtrl_AutoCompGetCurrent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlallocate">external documentation</a>.
-spec allocate(This, Bytes) -> 'ok' when
	This::wxStyledTextCtrl(), Bytes::integer().
allocate(#wx_ref{type=ThisT}=This,Bytes)
 when is_integer(Bytes) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Bytes,?get_env(),?wxStyledTextCtrl_Allocate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlfindcolumn">external documentation</a>.
-spec findColumn(This, Line, Column) -> integer() when
	This::wxStyledTextCtrl(), Line::integer(), Column::integer().
findColumn(#wx_ref{type=ThisT}=This,Line,Column)
 when is_integer(Line),is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,Column,?get_env(),?wxStyledTextCtrl_FindColumn),
  wxe_util:rec(?wxStyledTextCtrl_FindColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretsticky">external documentation</a>.
-spec getCaretSticky(This) -> integer() when
	This::wxStyledTextCtrl().
getCaretSticky(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretSticky),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretSticky).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretsticky">external documentation</a>.
-spec setCaretSticky(This, UseCaretStickyBehaviour) -> 'ok' when
	This::wxStyledTextCtrl(), UseCaretStickyBehaviour::integer().
setCaretSticky(#wx_ref{type=ThisT}=This,UseCaretStickyBehaviour)
 when is_integer(UseCaretStickyBehaviour) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,UseCaretStickyBehaviour,?get_env(),?wxStyledTextCtrl_SetCaretSticky).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrltogglecaretsticky">external documentation</a>.
-spec toggleCaretSticky(This) -> 'ok' when
	This::wxStyledTextCtrl().
toggleCaretSticky(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_ToggleCaretSticky).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetpasteconvertendings">external documentation</a>.
-spec setPasteConvertEndings(This, Convert) -> 'ok' when
	This::wxStyledTextCtrl(), Convert::boolean().
setPasteConvertEndings(#wx_ref{type=ThisT}=This,Convert)
 when is_boolean(Convert) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Convert,?get_env(),?wxStyledTextCtrl_SetPasteConvertEndings).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetpasteconvertendings">external documentation</a>.
-spec getPasteConvertEndings(This) -> boolean() when
	This::wxStyledTextCtrl().
getPasteConvertEndings(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetPasteConvertEndings),
  wxe_util:rec(?wxStyledTextCtrl_GetPasteConvertEndings).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlselectionduplicate">external documentation</a>.
-spec selectionDuplicate(This) -> 'ok' when
	This::wxStyledTextCtrl().
selectionDuplicate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_SelectionDuplicate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinebackalpha">external documentation</a>.
-spec setCaretLineBackAlpha(This, Alpha) -> 'ok' when
	This::wxStyledTextCtrl(), Alpha::integer().
setCaretLineBackAlpha(#wx_ref{type=ThisT}=This,Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Alpha,?get_env(),?wxStyledTextCtrl_SetCaretLineBackAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinebackalpha">external documentation</a>.
-spec getCaretLineBackAlpha(This) -> integer() when
	This::wxStyledTextCtrl().
getCaretLineBackAlpha(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCaretLineBackAlpha),
  wxe_util:rec(?wxStyledTextCtrl_GetCaretLineBackAlpha).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstartrecord">external documentation</a>.
-spec startRecord(This) -> 'ok' when
	This::wxStyledTextCtrl().
startRecord(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StartRecord).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstoprecord">external documentation</a>.
-spec stopRecord(This) -> 'ok' when
	This::wxStyledTextCtrl().
stopRecord(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_StopRecord).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlexer">external documentation</a>.
-spec setLexer(This, Lexer) -> 'ok' when
	This::wxStyledTextCtrl(), Lexer::integer().
setLexer(#wx_ref{type=ThisT}=This,Lexer)
 when is_integer(Lexer) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Lexer,?get_env(),?wxStyledTextCtrl_SetLexer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlexer">external documentation</a>.
-spec getLexer(This) -> integer() when
	This::wxStyledTextCtrl().
getLexer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetLexer),
  wxe_util:rec(?wxStyledTextCtrl_GetLexer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcolourise">external documentation</a>.
-spec colourise(This, Start, End) -> 'ok' when
	This::wxStyledTextCtrl(), Start::integer(), End::integer().
colourise(#wx_ref{type=ThisT}=This,Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Start,End,?get_env(),?wxStyledTextCtrl_Colourise).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetproperty">external documentation</a>.
-spec setProperty(This, Key, Value) -> 'ok' when
	This::wxStyledTextCtrl(), Key::unicode:chardata(), Value::unicode:chardata().
setProperty(#wx_ref{type=ThisT}=This,Key,Value)
 when ?is_chardata(Key),?is_chardata(Value) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Key_UC = unicode:characters_to_binary(Key),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Key_UC,Value_UC,?get_env(),?wxStyledTextCtrl_SetProperty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetkeywords">external documentation</a>.
-spec setKeyWords(This, KeyWordSet, KeyWords) -> 'ok' when
	This::wxStyledTextCtrl(), KeyWordSet::integer(), KeyWords::unicode:chardata().
setKeyWords(#wx_ref{type=ThisT}=This,KeyWordSet,KeyWords)
 when is_integer(KeyWordSet),?is_chardata(KeyWords) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  KeyWords_UC = unicode:characters_to_binary(KeyWords),
  wxe_util:queue_cmd(This,KeyWordSet,KeyWords_UC,?get_env(),?wxStyledTextCtrl_SetKeyWords).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlexerlanguage">external documentation</a>.
-spec setLexerLanguage(This, Language) -> 'ok' when
	This::wxStyledTextCtrl(), Language::unicode:chardata().
setLexerLanguage(#wx_ref{type=ThisT}=This,Language)
 when ?is_chardata(Language) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Language_UC = unicode:characters_to_binary(Language),
  wxe_util:queue_cmd(This,Language_UC,?get_env(),?wxStyledTextCtrl_SetLexerLanguage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetproperty">external documentation</a>.
-spec getProperty(This, Key) -> unicode:charlist() when
	This::wxStyledTextCtrl(), Key::unicode:chardata().
getProperty(#wx_ref{type=ThisT}=This,Key)
 when ?is_chardata(Key) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Key_UC = unicode:characters_to_binary(Key),
  wxe_util:queue_cmd(This,Key_UC,?get_env(),?wxStyledTextCtrl_GetProperty),
  wxe_util:rec(?wxStyledTextCtrl_GetProperty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstylebitsneeded">external documentation</a>.
-spec getStyleBitsNeeded(This) -> integer() when
	This::wxStyledTextCtrl().
getStyleBitsNeeded(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetStyleBitsNeeded),
  wxe_util:rec(?wxStyledTextCtrl_GetStyleBitsNeeded).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurrentline">external documentation</a>.
-spec getCurrentLine(This) -> integer() when
	This::wxStyledTextCtrl().
getCurrentLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCurrentLine),
  wxe_util:rec(?wxStyledTextCtrl_GetCurrentLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetspec">external documentation</a>.
-spec styleSetSpec(This, StyleNum, Spec) -> 'ok' when
	This::wxStyledTextCtrl(), StyleNum::integer(), Spec::unicode:chardata().
styleSetSpec(#wx_ref{type=ThisT}=This,StyleNum,Spec)
 when is_integer(StyleNum),?is_chardata(Spec) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Spec_UC = unicode:characters_to_binary(Spec),
  wxe_util:queue_cmd(This,StyleNum,Spec_UC,?get_env(),?wxStyledTextCtrl_StyleSetSpec).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfont">external documentation</a>.
-spec styleSetFont(This, StyleNum, Font) -> 'ok' when
	This::wxStyledTextCtrl(), StyleNum::integer(), Font::wxFont:wxFont().
styleSetFont(#wx_ref{type=ThisT}=This,StyleNum,#wx_ref{type=FontT}=Font)
 when is_integer(StyleNum) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,StyleNum,Font,?get_env(),?wxStyledTextCtrl_StyleSetFont).

%% @equiv styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline, [])
-spec styleSetFontAttr(This, StyleNum, Size, FaceName, Bold, Italic, Underline) -> 'ok' when
	This::wxStyledTextCtrl(), StyleNum::integer(), Size::integer(), FaceName::unicode:chardata(), Bold::boolean(), Italic::boolean(), Underline::boolean().

styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline)
 when is_record(This, wx_ref),is_integer(StyleNum),is_integer(Size),?is_chardata(FaceName),is_boolean(Bold),is_boolean(Italic),is_boolean(Underline) ->
  styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfontattr">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec styleSetFontAttr(This, StyleNum, Size, FaceName, Bold, Italic, Underline, [Option]) -> 'ok' when
	This::wxStyledTextCtrl(), StyleNum::integer(), Size::integer(), FaceName::unicode:chardata(), Bold::boolean(), Italic::boolean(), Underline::boolean(),
	Option :: {'encoding', wx:wx_enum()}.
styleSetFontAttr(#wx_ref{type=ThisT}=This,StyleNum,Size,FaceName,Bold,Italic,Underline, Options)
 when is_integer(StyleNum),is_integer(Size),?is_chardata(FaceName),is_boolean(Bold),is_boolean(Italic),is_boolean(Underline),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  FaceName_UC = unicode:characters_to_binary(FaceName),
  MOpts = fun({encoding, _encoding} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,StyleNum,Size,FaceName_UC,Bold,Italic,Underline, Opts,?get_env(),?wxStyledTextCtrl_StyleSetFontAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetcharacterset">external documentation</a>.
-spec styleSetCharacterSet(This, Style, CharacterSet) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), CharacterSet::integer().
styleSetCharacterSet(#wx_ref{type=ThisT}=This,Style,CharacterSet)
 when is_integer(Style),is_integer(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,CharacterSet,?get_env(),?wxStyledTextCtrl_StyleSetCharacterSet).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfontencoding">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec styleSetFontEncoding(This, Style, Encoding) -> 'ok' when
	This::wxStyledTextCtrl(), Style::integer(), Encoding::wx:wx_enum().
styleSetFontEncoding(#wx_ref{type=ThisT}=This,Style,Encoding)
 when is_integer(Style),is_integer(Encoding) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Style,Encoding,?get_env(),?wxStyledTextCtrl_StyleSetFontEncoding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyexecute">external documentation</a>.
-spec cmdKeyExecute(This, Cmd) -> 'ok' when
	This::wxStyledTextCtrl(), Cmd::integer().
cmdKeyExecute(#wx_ref{type=ThisT}=This,Cmd)
 when is_integer(Cmd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Cmd,?get_env(),?wxStyledTextCtrl_CmdKeyExecute).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmargins">external documentation</a>.
-spec setMargins(This, Left, Right) -> 'ok' when
	This::wxStyledTextCtrl(), Left::integer(), Right::integer().
setMargins(#wx_ref{type=ThisT}=This,Left,Right)
 when is_integer(Left),is_integer(Right) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Left,Right,?get_env(),?wxStyledTextCtrl_SetMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselection">external documentation</a>.
-spec getSelection(This) -> {From::integer(), To::integer()} when
	This::wxStyledTextCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelection),
  wxe_util:rec(?wxStyledTextCtrl_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlpointfromposition">external documentation</a>.
-spec pointFromPosition(This, Pos) -> {X::integer(), Y::integer()} when
	This::wxStyledTextCtrl(), Pos::integer().
pointFromPosition(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxStyledTextCtrl_PointFromPosition),
  wxe_util:rec(?wxStyledTextCtrl_PointFromPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlscrolltoline">external documentation</a>.
-spec scrollToLine(This, Line) -> 'ok' when
	This::wxStyledTextCtrl(), Line::integer().
scrollToLine(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_ScrollToLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlscrolltocolumn">external documentation</a>.
-spec scrollToColumn(This, Column) -> 'ok' when
	This::wxStyledTextCtrl(), Column::integer().
scrollToColumn(#wx_ref{type=ThisT}=This,Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Column,?get_env(),?wxStyledTextCtrl_ScrollToColumn).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvscrollbar">external documentation</a>.
-spec setVScrollBar(This, Bar) -> 'ok' when
	This::wxStyledTextCtrl(), Bar::wxScrollBar:wxScrollBar().
setVScrollBar(#wx_ref{type=ThisT}=This,#wx_ref{type=BarT}=Bar) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BarT,wxScrollBar),
  wxe_util:queue_cmd(This,Bar,?get_env(),?wxStyledTextCtrl_SetVScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsethscrollbar">external documentation</a>.
-spec setHScrollBar(This, Bar) -> 'ok' when
	This::wxStyledTextCtrl(), Bar::wxScrollBar:wxScrollBar().
setHScrollBar(#wx_ref{type=ThisT}=This,#wx_ref{type=BarT}=Bar) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BarT,wxScrollBar),
  wxe_util:queue_cmd(This,Bar,?get_env(),?wxStyledTextCtrl_SetHScrollBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlastkeydownprocessed">external documentation</a>.
-spec getLastKeydownProcessed(This) -> boolean() when
	This::wxStyledTextCtrl().
getLastKeydownProcessed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetLastKeydownProcessed),
  wxe_util:rec(?wxStyledTextCtrl_GetLastKeydownProcessed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlastkeydownprocessed">external documentation</a>.
-spec setLastKeydownProcessed(This, Val) -> 'ok' when
	This::wxStyledTextCtrl(), Val::boolean().
setLastKeydownProcessed(#wx_ref{type=ThisT}=This,Val)
 when is_boolean(Val) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Val,?get_env(),?wxStyledTextCtrl_SetLastKeydownProcessed).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsavefile">external documentation</a>.
-spec saveFile(This, Filename) -> boolean() when
	This::wxStyledTextCtrl(), Filename::unicode:chardata().
saveFile(#wx_ref{type=ThisT}=This,Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Filename_UC = unicode:characters_to_binary(Filename),
  wxe_util:queue_cmd(This,Filename_UC,?get_env(),?wxStyledTextCtrl_SaveFile),
  wxe_util:rec(?wxStyledTextCtrl_SaveFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlloadfile">external documentation</a>.
-spec loadFile(This, Filename) -> boolean() when
	This::wxStyledTextCtrl(), Filename::unicode:chardata().
loadFile(#wx_ref{type=ThisT}=This,Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Filename_UC = unicode:characters_to_binary(Filename),
  wxe_util:queue_cmd(This,Filename_UC,?get_env(),?wxStyledTextCtrl_LoadFile),
  wxe_util:rec(?wxStyledTextCtrl_LoadFile).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldodragover">external documentation</a>.
%%<br /> DefaultRes = ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
%%<br /> Res = ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
-spec doDragOver(This, X, Y, DefaultRes) -> wx:wx_enum() when
	This::wxStyledTextCtrl(), X::integer(), Y::integer(), DefaultRes::wx:wx_enum().
doDragOver(#wx_ref{type=ThisT}=This,X,Y,DefaultRes)
 when is_integer(X),is_integer(Y),is_integer(DefaultRes) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,X,Y,DefaultRes,?get_env(),?wxStyledTextCtrl_DoDragOver),
  wxe_util:rec(?wxStyledTextCtrl_DoDragOver).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrldodroptext">external documentation</a>.
-spec doDropText(This, X, Y, Data) -> boolean() when
	This::wxStyledTextCtrl(), X::integer(), Y::integer(), Data::unicode:chardata().
doDropText(#wx_ref{type=ThisT}=This,X,Y,Data)
 when is_integer(X),is_integer(Y),?is_chardata(Data) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Data_UC = unicode:characters_to_binary(Data),
  wxe_util:queue_cmd(This,X,Y,Data_UC,?get_env(),?wxStyledTextCtrl_DoDropText),
  wxe_util:rec(?wxStyledTextCtrl_DoDropText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetuseantialiasing">external documentation</a>.
-spec getUseAntiAliasing(This) -> boolean() when
	This::wxStyledTextCtrl().
getUseAntiAliasing(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetUseAntiAliasing),
  wxe_util:rec(?wxStyledTextCtrl_GetUseAntiAliasing).

%% @equiv addTextRaw(This,Text, [])
-spec addTextRaw(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::binary().

addTextRaw(This,Text)
 when is_record(This, wx_ref),is_binary(Text) ->
  addTextRaw(This,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrladdtextraw">external documentation</a>.
-spec addTextRaw(This, Text, [Option]) -> 'ok' when
	This::wxStyledTextCtrl(), Text::binary(),
	Option :: {'length', integer()}.
addTextRaw(#wx_ref{type=ThisT}=This,Text, Options)
 when is_binary(Text),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  MOpts = fun({length, _length} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text, Opts,?get_env(),?wxStyledTextCtrl_AddTextRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlinserttextraw">external documentation</a>.
-spec insertTextRaw(This, Pos, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Pos::integer(), Text::binary().
insertTextRaw(#wx_ref{type=ThisT}=This,Pos,Text)
 when is_integer(Pos),is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Pos,Text,?get_env(),?wxStyledTextCtrl_InsertTextRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurlineraw">external documentation</a>.
-spec getCurLineRaw(This) -> Result when
	Result ::{Res ::binary(), LinePos::integer()},
	This::wxStyledTextCtrl().
getCurLineRaw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetCurLineRaw),
  wxe_util:rec(?wxStyledTextCtrl_GetCurLineRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineraw">external documentation</a>.
-spec getLineRaw(This, Line) -> binary() when
	This::wxStyledTextCtrl(), Line::integer().
getLineRaw(#wx_ref{type=ThisT}=This,Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Line,?get_env(),?wxStyledTextCtrl_GetLineRaw),
  wxe_util:rec(?wxStyledTextCtrl_GetLineRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectedtextraw">external documentation</a>.
-spec getSelectedTextRaw(This) -> binary() when
	This::wxStyledTextCtrl().
getSelectedTextRaw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetSelectedTextRaw),
  wxe_util:rec(?wxStyledTextCtrl_GetSelectedTextRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextrangeraw">external documentation</a>.
-spec getTextRangeRaw(This, StartPos, EndPos) -> binary() when
	This::wxStyledTextCtrl(), StartPos::integer(), EndPos::integer().
getTextRangeRaw(#wx_ref{type=ThisT}=This,StartPos,EndPos)
 when is_integer(StartPos),is_integer(EndPos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,StartPos,EndPos,?get_env(),?wxStyledTextCtrl_GetTextRangeRaw),
  wxe_util:rec(?wxStyledTextCtrl_GetTextRangeRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlsettextraw">external documentation</a>.
-spec setTextRaw(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::binary().
setTextRaw(#wx_ref{type=ThisT}=This,Text)
 when is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,Text,?get_env(),?wxStyledTextCtrl_SetTextRaw).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextraw">external documentation</a>.
-spec getTextRaw(This) -> binary() when
	This::wxStyledTextCtrl().
getTextRaw(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextCtrl_GetTextRaw),
  wxe_util:rec(?wxStyledTextCtrl_GetTextRaw).

%% @equiv appendTextRaw(This,Text, [])
-spec appendTextRaw(This, Text) -> 'ok' when
	This::wxStyledTextCtrl(), Text::binary().

appendTextRaw(This,Text)
 when is_record(This, wx_ref),is_binary(Text) ->
  appendTextRaw(This,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextctrl.html#wxstyledtextctrlappendtextraw">external documentation</a>.
-spec appendTextRaw(This, Text, [Option]) -> 'ok' when
	This::wxStyledTextCtrl(), Text::binary(),
	Option :: {'length', integer()}.
appendTextRaw(#wx_ref{type=ThisT}=This,Text, Options)
 when is_binary(Text),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  MOpts = fun({length, _length} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text, Opts,?get_env(),?wxStyledTextCtrl_AppendTextRaw).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxStyledTextCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStyledTextCtrl),
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
