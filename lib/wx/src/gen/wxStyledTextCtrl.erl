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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html">wxStyledTextCtrl</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxStyledTextCtrl().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxStyledTextCtrl).
-include("wxe.hrl").
-export([addStyledText/2,addText/2,addTextRaw/2,allocate/2,appendText/2,appendTextRaw/2,
  autoCompActive/1,autoCompCancel/1,autoCompComplete/1,autoCompGetAutoHide/1,
  autoCompGetCancelAtStart/1,autoCompGetChooseSingle/1,autoCompGetCurrent/1,
  autoCompGetDropRestOfWord/1,autoCompGetIgnoreCase/1,autoCompGetMaxHeight/1,
  autoCompGetMaxWidth/1,autoCompGetSeparator/1,autoCompGetTypeSeparator/1,
  autoCompPosStart/1,autoCompSelect/2,autoCompSetAutoHide/2,autoCompSetCancelAtStart/2,
  autoCompSetChooseSingle/2,autoCompSetDropRestOfWord/2,autoCompSetFillUps/2,
  autoCompSetIgnoreCase/2,autoCompSetMaxHeight/2,autoCompSetMaxWidth/2,
  autoCompSetSeparator/2,autoCompSetTypeSeparator/2,autoCompShow/3,
  autoCompStops/2,backTab/1,beginUndoAction/1,braceBadLight/2,braceHighlight/3,
  braceMatch/2,callTipActive/1,callTipCancel/1,callTipPosAtStart/1,
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
  getStyledText/3,getTabIndents/1,getTabWidth/1,getTargetEnd/1,getTargetStart/1,
  getText/1,getTextLength/1,getTextRange/3,getTextRangeRaw/3,getTextRaw/1,
  getTwoPhaseDraw/1,getUndoCollection/1,getUseAntiAliasing/1,getUseHorizontalScrollBar/1,
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
  sendMsg/2,sendMsg/3,setAnchor/2,setBackSpaceUnIndents/2,setBufferedDraw/2,
  setCaretForeground/2,setCaretLineBackAlpha/2,setCaretLineBackground/2,
  setCaretLineVisible/2,setCaretPeriod/2,setCaretSticky/2,setCaretWidth/2,
  setCharsDefault/1,setCodePage/2,setControlCharSymbol/2,setCurrentPos/2,
  setEOLMode/2,setEdgeColour/2,setEdgeColumn/2,setFoldExpanded/3,setFoldFlags/2,
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
  startStyling/3,stopRecord/1,stutteredPageDown/1,stutteredPageDownExtend/1,
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
  layout/1,lower/1,makeModal/1,makeModal/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,parent_class/1,popEventHandler/1,
  popEventHandler/2,popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,
  refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,removeChild/2,
  reparent/2,screenToClient/1,screenToClient/2,scrollLines/2,scrollPages/2,
  scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,setAutoLayout/2,
  setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,setClientSize/2,
  setClientSize/3,setContainingSizer/2,setCursor/2,setDropTarget/2,
  setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,setForegroundColour/2,
  setHelpText/2,setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,
  setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,setPalette/2,
  setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,
  setToolTip/2,setVirtualSize/2,setVirtualSize/3,setVirtualSizeHints/2,
  setVirtualSizeHints/3,setVirtualSizeHints/4,setWindowStyle/2,setWindowStyleFlag/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxStyledTextCtrl()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwxstyledtextctrl">external documentation</a>.
new() ->
  wxe_util:construct(?wxStyledTextCtrl_new_0,
  <<>>).

%% @spec (Parent::wxWindow:wxWindow()) -> wxStyledTextCtrl()
%% @equiv new(Parent, [])
new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @spec (Parent::wxWindow:wxWindow(), [Option]) -> wxStyledTextCtrl()
%% Option = {id, integer()} | {pos, {X::integer(),Y::integer()}} | {size, {W::integer(),H::integer()}} | {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwxstyledtextctrl">external documentation</a>.
new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxStyledTextCtrl_new_2,
  <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), Parent::wxWindow:wxWindow()) -> bool()
%% @equiv create(This,Parent, [])
create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @spec (This::wxStyledTextCtrl(), Parent::wxWindow:wxWindow(), [Option]) -> bool()
%% Option = {id, integer()} | {pos, {X::integer(),Y::integer()}} | {size, {W::integer(),H::integer()}} | {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxStyledTextCtrl_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrladdtext">external documentation</a>.
addText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_AddText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Data::wxMemoryBuffer:wxMemoryBuffer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrladdstyledtext">external documentation</a>.
addStyledText(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(DataT,wxMemoryBuffer),
  wxe_util:cast(?wxStyledTextCtrl_AddStyledText,
  <<ThisRef:32/?UI,DataRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlinserttext">external documentation</a>.
insertText(#wx_ref{type=ThisT,ref=ThisRef},Pos,Text)
 when is_integer(Pos),is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_InsertText,
  <<ThisRef:32/?UI,Pos:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlclearall">external documentation</a>.
clearAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ClearAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcleardocumentstyle">external documentation</a>.
clearDocumentStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ClearDocumentStyle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlength">external documentation</a>.
getLength(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLength,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcharat">external documentation</a>.
getCharAt(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCharAt,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurrentpos">external documentation</a>.
getCurrentPos(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCurrentPos,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetanchor">external documentation</a>.
getAnchor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetAnchor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstyleat">external documentation</a>.
getStyleAt(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetStyleAt,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlredo">external documentation</a>.
redo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Redo,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CollectUndo::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetundocollection">external documentation</a>.
setUndoCollection(#wx_ref{type=ThisT,ref=ThisRef},CollectUndo)
 when is_boolean(CollectUndo) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetUndoCollection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(CollectUndo)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlselectall">external documentation</a>.
selectAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SelectAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetsavepoint">external documentation</a>.
setSavePoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSavePoint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StartPos::integer(), EndPos::integer()) -> wxMemoryBuffer:wxMemoryBuffer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstyledtext">external documentation</a>.
getStyledText(#wx_ref{type=ThisT,ref=ThisRef},StartPos,EndPos)
 when is_integer(StartPos),is_integer(EndPos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetStyledText,
  <<ThisRef:32/?UI,StartPos:32/?UI,EndPos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcanredo">external documentation</a>.
canRedo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_CanRedo,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Handle::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerlinefromhandle">external documentation</a>.
markerLineFromHandle(#wx_ref{type=ThisT,ref=ThisRef},Handle)
 when is_integer(Handle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_MarkerLineFromHandle,
  <<ThisRef:32/?UI,Handle:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Handle::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdeletehandle">external documentation</a>.
markerDeleteHandle(#wx_ref{type=ThisT,ref=ThisRef},Handle)
 when is_integer(Handle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerDeleteHandle,
  <<ThisRef:32/?UI,Handle:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetundocollection">external documentation</a>.
getUndoCollection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetUndoCollection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetviewwhitespace">external documentation</a>.
getViewWhiteSpace(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetViewWhiteSpace,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), ViewWS::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetviewwhitespace">external documentation</a>.
setViewWhiteSpace(#wx_ref{type=ThisT,ref=ThisRef},ViewWS)
 when is_integer(ViewWS) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetViewWhiteSpace,
  <<ThisRef:32/?UI,ViewWS:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pt::{X::integer(),Y::integer()}) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfrompoint">external documentation</a>.
positionFromPoint(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PositionFromPoint,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), X::integer(), Y::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfrompointclose">external documentation</a>.
positionFromPointClose(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PositionFromPointClose,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgotoline">external documentation</a>.
gotoLine(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_GotoLine,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgotopos">external documentation</a>.
gotoPos(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_GotoPos,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PosAnchor::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetanchor">external documentation</a>.
setAnchor(#wx_ref{type=ThisT,ref=ThisRef},PosAnchor)
 when is_integer(PosAnchor) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetAnchor,
  <<ThisRef:32/?UI,PosAnchor:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> {string(),LinePos::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurline">external documentation</a>.
getCurLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCurLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetendstyled">external documentation</a>.
getEndStyled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEndStyled,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), EolMode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlconverteols">external documentation</a>.
convertEOLs(#wx_ref{type=ThisT,ref=ThisRef},EolMode)
 when is_integer(EolMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ConvertEOLs,
  <<ThisRef:32/?UI,EolMode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgeteolmode">external documentation</a>.
getEOLMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEOLMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), EolMode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlseteolmode">external documentation</a>.
setEOLMode(#wx_ref{type=ThisT,ref=ThisRef},EolMode)
 when is_integer(EolMode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetEOLMode,
  <<ThisRef:32/?UI,EolMode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), Mask::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstartstyling">external documentation</a>.
startStyling(#wx_ref{type=ThisT,ref=ThisRef},Pos,Mask)
 when is_integer(Pos),is_integer(Mask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StartStyling,
  <<ThisRef:32/?UI,Pos:32/?UI,Mask:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Length::integer(), Style::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstyling">external documentation</a>.
setStyling(#wx_ref{type=ThisT,ref=ThisRef},Length,Style)
 when is_integer(Length),is_integer(Style) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetStyling,
  <<ThisRef:32/?UI,Length:32/?UI,Style:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetbuffereddraw">external documentation</a>.
getBufferedDraw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetBufferedDraw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Buffered::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetbuffereddraw">external documentation</a>.
setBufferedDraw(#wx_ref{type=ThisT,ref=ThisRef},Buffered)
 when is_boolean(Buffered) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetBufferedDraw,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Buffered)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), TabWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettabwidth">external documentation</a>.
setTabWidth(#wx_ref{type=ThisT,ref=ThisRef},TabWidth)
 when is_integer(TabWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetTabWidth,
  <<ThisRef:32/?UI,TabWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettabwidth">external documentation</a>.
getTabWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTabWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CodePage::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcodepage">external documentation</a>.
setCodePage(#wx_ref{type=ThisT,ref=ThisRef},CodePage)
 when is_integer(CodePage) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCodePage,
  <<ThisRef:32/?UI,CodePage:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), MarkerSymbol::integer()) -> ok
%% @equiv markerDefine(This,MarkerNumber,MarkerSymbol, [])
markerDefine(This,MarkerNumber,MarkerSymbol)
 when is_record(This, wx_ref),is_integer(MarkerNumber),is_integer(MarkerSymbol) ->
  markerDefine(This,MarkerNumber,MarkerSymbol, []).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), MarkerSymbol::integer(), [Option]) -> ok
%% Option = {foreground, wx:colour()} | {background, wx:colour()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdefine">external documentation</a>.
markerDefine(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber,MarkerSymbol, Options)
 when is_integer(MarkerNumber),is_integer(MarkerSymbol),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  MOpts = fun({foreground, Foreground}, Acc) -> [<<1:32/?UI,(wxe_util:colour_bin(Foreground)):16/binary,0:32>>|Acc];
          ({background, Background}, Acc) -> [<<2:32/?UI,(wxe_util:colour_bin(Background)):16/binary,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxStyledTextCtrl_MarkerDefine,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI,MarkerSymbol:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetforeground">external documentation</a>.
markerSetForeground(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber,Fore)
 when is_integer(MarkerNumber),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerSetForeground,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetbackground">external documentation</a>.
markerSetBackground(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber,Back)
 when is_integer(MarkerNumber),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerSetBackground,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), MarkerNumber::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkeradd">external documentation</a>.
markerAdd(#wx_ref{type=ThisT,ref=ThisRef},Line,MarkerNumber)
 when is_integer(Line),is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_MarkerAdd,
  <<ThisRef:32/?UI,Line:32/?UI,MarkerNumber:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), MarkerNumber::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdelete">external documentation</a>.
markerDelete(#wx_ref{type=ThisT,ref=ThisRef},Line,MarkerNumber)
 when is_integer(Line),is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerDelete,
  <<ThisRef:32/?UI,Line:32/?UI,MarkerNumber:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdeleteall">external documentation</a>.
markerDeleteAll(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber)
 when is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerDeleteAll,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerget">external documentation</a>.
markerGet(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_MarkerGet,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), LineStart::integer(), MarkerMask::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkernext">external documentation</a>.
markerNext(#wx_ref{type=ThisT,ref=ThisRef},LineStart,MarkerMask)
 when is_integer(LineStart),is_integer(MarkerMask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_MarkerNext,
  <<ThisRef:32/?UI,LineStart:32/?UI,MarkerMask:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), LineStart::integer(), MarkerMask::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerprevious">external documentation</a>.
markerPrevious(#wx_ref{type=ThisT,ref=ThisRef},LineStart,MarkerMask)
 when is_integer(LineStart),is_integer(MarkerMask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_MarkerPrevious,
  <<ThisRef:32/?UI,LineStart:32/?UI,MarkerMask:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), Bmp::wxBitmap:wxBitmap()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkerdefinebitmap">external documentation</a>.
markerDefineBitmap(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber,#wx_ref{type=BmpT,ref=BmpRef})
 when is_integer(MarkerNumber) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:cast(?wxStyledTextCtrl_MarkerDefineBitmap,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI,BmpRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), Set::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkeraddset">external documentation</a>.
markerAddSet(#wx_ref{type=ThisT,ref=ThisRef},Line,Set)
 when is_integer(Line),is_integer(Set) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerAddSet,
  <<ThisRef:32/?UI,Line:32/?UI,Set:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), MarkerNumber::integer(), Alpha::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmarkersetalpha">external documentation</a>.
markerSetAlpha(#wx_ref{type=ThisT,ref=ThisRef},MarkerNumber,Alpha)
 when is_integer(MarkerNumber),is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MarkerSetAlpha,
  <<ThisRef:32/?UI,MarkerNumber:32/?UI,Alpha:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer(), MarginType::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmargintype">external documentation</a>.
setMarginType(#wx_ref{type=ThisT,ref=ThisRef},Margin,MarginType)
 when is_integer(Margin),is_integer(MarginType) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginType,
  <<ThisRef:32/?UI,Margin:32/?UI,MarginType:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmargintype">external documentation</a>.
getMarginType(#wx_ref{type=ThisT,ref=ThisRef},Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginType,
  <<ThisRef:32/?UI,Margin:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginwidth">external documentation</a>.
setMarginWidth(#wx_ref{type=ThisT,ref=ThisRef},Margin,PixelWidth)
 when is_integer(Margin),is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginWidth,
  <<ThisRef:32/?UI,Margin:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginwidth">external documentation</a>.
getMarginWidth(#wx_ref{type=ThisT,ref=ThisRef},Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginWidth,
  <<ThisRef:32/?UI,Margin:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer(), Mask::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginmask">external documentation</a>.
setMarginMask(#wx_ref{type=ThisT,ref=ThisRef},Margin,Mask)
 when is_integer(Margin),is_integer(Mask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginMask,
  <<ThisRef:32/?UI,Margin:32/?UI,Mask:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginmask">external documentation</a>.
getMarginMask(#wx_ref{type=ThisT,ref=ThisRef},Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginMask,
  <<ThisRef:32/?UI,Margin:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer(), Sensitive::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginsensitive">external documentation</a>.
setMarginSensitive(#wx_ref{type=ThisT,ref=ThisRef},Margin,Sensitive)
 when is_integer(Margin),is_boolean(Sensitive) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginSensitive,
  <<ThisRef:32/?UI,Margin:32/?UI,(wxe_util:from_bool(Sensitive)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Margin::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginsensitive">external documentation</a>.
getMarginSensitive(#wx_ref{type=ThisT,ref=ThisRef},Margin)
 when is_integer(Margin) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginSensitive,
  <<ThisRef:32/?UI,Margin:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleclearall">external documentation</a>.
styleClearAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleClearAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetforeground">external documentation</a>.
styleSetForeground(#wx_ref{type=ThisT,ref=ThisRef},Style,Fore)
 when is_integer(Style),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetForeground,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetbackground">external documentation</a>.
styleSetBackground(#wx_ref{type=ThisT,ref=ThisRef},Style,Back)
 when is_integer(Style),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetBackground,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Bold::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetbold">external documentation</a>.
styleSetBold(#wx_ref{type=ThisT,ref=ThisRef},Style,Bold)
 when is_integer(Style),is_boolean(Bold) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetBold,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Bold)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Italic::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetitalic">external documentation</a>.
styleSetItalic(#wx_ref{type=ThisT,ref=ThisRef},Style,Italic)
 when is_integer(Style),is_boolean(Italic) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetItalic,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Italic)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), SizePoints::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetsize">external documentation</a>.
styleSetSize(#wx_ref{type=ThisT,ref=ThisRef},Style,SizePoints)
 when is_integer(Style),is_integer(SizePoints) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetSize,
  <<ThisRef:32/?UI,Style:32/?UI,SizePoints:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), FontName::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfacename">external documentation</a>.
styleSetFaceName(#wx_ref{type=ThisT,ref=ThisRef},Style,FontName)
 when is_integer(Style),is_list(FontName) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  FontName_UC = unicode:characters_to_binary([FontName,0]),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetFaceName,
  <<ThisRef:32/?UI,Style:32/?UI,(byte_size(FontName_UC)):32/?UI,(FontName_UC)/binary, 0:(((8- ((4+byte_size(FontName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Filled::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleseteolfilled">external documentation</a>.
styleSetEOLFilled(#wx_ref{type=ThisT,ref=ThisRef},Style,Filled)
 when is_integer(Style),is_boolean(Filled) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetEOLFilled,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Filled)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstyleresetdefault">external documentation</a>.
styleResetDefault(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleResetDefault,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Underline::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetunderline">external documentation</a>.
styleSetUnderline(#wx_ref{type=ThisT,ref=ThisRef},Style,Underline)
 when is_integer(Style),is_boolean(Underline) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetUnderline,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Underline)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), CaseForce::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetcase">external documentation</a>.
styleSetCase(#wx_ref{type=ThisT,ref=ThisRef},Style,CaseForce)
 when is_integer(Style),is_integer(CaseForce) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetCase,
  <<ThisRef:32/?UI,Style:32/?UI,CaseForce:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Hotspot::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesethotspot">external documentation</a>.
styleSetHotSpot(#wx_ref{type=ThisT,ref=ThisRef},Style,Hotspot)
 when is_integer(Style),is_boolean(Hotspot) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetHotSpot,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Hotspot)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselforeground">external documentation</a>.
setSelForeground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelForeground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselbackground">external documentation</a>.
setSelBackground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelBackground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselalpha">external documentation</a>.
getSelAlpha(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelAlpha,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Alpha::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselalpha">external documentation</a>.
setSelAlpha(#wx_ref{type=ThisT,ref=ThisRef},Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelAlpha,
  <<ThisRef:32/?UI,Alpha:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretforeground">external documentation</a>.
setCaretForeground(#wx_ref{type=ThisT,ref=ThisRef},Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretForeground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Key::integer(), Modifiers::integer(), Cmd::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyassign">external documentation</a>.
cmdKeyAssign(#wx_ref{type=ThisT,ref=ThisRef},Key,Modifiers,Cmd)
 when is_integer(Key),is_integer(Modifiers),is_integer(Cmd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CmdKeyAssign,
  <<ThisRef:32/?UI,Key:32/?UI,Modifiers:32/?UI,Cmd:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Key::integer(), Modifiers::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyclear">external documentation</a>.
cmdKeyClear(#wx_ref{type=ThisT,ref=ThisRef},Key,Modifiers)
 when is_integer(Key),is_integer(Modifiers) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CmdKeyClear,
  <<ThisRef:32/?UI,Key:32/?UI,Modifiers:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyclearall">external documentation</a>.
cmdKeyClearAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CmdKeyClearAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Length::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstylebytes">external documentation</a>.
setStyleBytes(#wx_ref{type=ThisT,ref=ThisRef},Length)
 when is_integer(Length) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_SetStyleBytes,
  <<ThisRef:32/?UI,Length:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Visible::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetvisible">external documentation</a>.
styleSetVisible(#wx_ref{type=ThisT,ref=ThisRef},Style,Visible)
 when is_integer(Style),is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetVisible,
  <<ThisRef:32/?UI,Style:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretperiod">external documentation</a>.
getCaretPeriod(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretPeriod,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PeriodMilliseconds::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretperiod">external documentation</a>.
setCaretPeriod(#wx_ref{type=ThisT,ref=ThisRef},PeriodMilliseconds)
 when is_integer(PeriodMilliseconds) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretPeriod,
  <<ThisRef:32/?UI,PeriodMilliseconds:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Characters::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwordchars">external documentation</a>.
setWordChars(#wx_ref{type=ThisT,ref=ThisRef},Characters)
 when is_list(Characters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Characters_UC = unicode:characters_to_binary([Characters,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetWordChars,
  <<ThisRef:32/?UI,(byte_size(Characters_UC)):32/?UI,(Characters_UC)/binary, 0:(((8- ((0+byte_size(Characters_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlbeginundoaction">external documentation</a>.
beginUndoAction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_BeginUndoAction,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlendundoaction">external documentation</a>.
endUndoAction(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EndUndoAction,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Indic::integer(), Style::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorsetstyle">external documentation</a>.
indicatorSetStyle(#wx_ref{type=ThisT,ref=ThisRef},Indic,Style)
 when is_integer(Indic),is_integer(Style) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_IndicatorSetStyle,
  <<ThisRef:32/?UI,Indic:32/?UI,Style:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Indic::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorgetstyle">external documentation</a>.
indicatorGetStyle(#wx_ref{type=ThisT,ref=ThisRef},Indic)
 when is_integer(Indic) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_IndicatorGetStyle,
  <<ThisRef:32/?UI,Indic:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Indic::integer(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorsetforeground">external documentation</a>.
indicatorSetForeground(#wx_ref{type=ThisT,ref=ThisRef},Indic,Fore)
 when is_integer(Indic),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_IndicatorSetForeground,
  <<ThisRef:32/?UI,Indic:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Indic::integer()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlindicatorgetforeground">external documentation</a>.
indicatorGetForeground(#wx_ref{type=ThisT,ref=ThisRef},Indic)
 when is_integer(Indic) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_IndicatorGetForeground,
  <<ThisRef:32/?UI,Indic:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespaceforeground">external documentation</a>.
setWhitespaceForeground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWhitespaceForeground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespacebackground">external documentation</a>.
setWhitespaceBackground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWhitespaceBackground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstylebits">external documentation</a>.
getStyleBits(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetStyleBits,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), State::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlinestate">external documentation</a>.
setLineState(#wx_ref{type=ThisT,ref=ThisRef},Line,State)
 when is_integer(Line),is_integer(State) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetLineState,
  <<ThisRef:32/?UI,Line:32/?UI,State:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinestate">external documentation</a>.
getLineState(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineState,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmaxlinestate">external documentation</a>.
getMaxLineState(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMaxLineState,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinevisible">external documentation</a>.
getCaretLineVisible(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretLineVisible,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinevisible">external documentation</a>.
setCaretLineVisible(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretLineVisible,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinebackground">external documentation</a>.
getCaretLineBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretLineBackground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinebackground">external documentation</a>.
setCaretLineBackground(#wx_ref{type=ThisT,ref=ThisRef},Back)
 when tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretLineBackground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), LenEntered::integer(), ItemList::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompshow">external documentation</a>.
autoCompShow(#wx_ref{type=ThisT,ref=ThisRef},LenEntered,ItemList)
 when is_integer(LenEntered),is_list(ItemList) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ItemList_UC = unicode:characters_to_binary([ItemList,0]),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompShow,
  <<ThisRef:32/?UI,LenEntered:32/?UI,(byte_size(ItemList_UC)):32/?UI,(ItemList_UC)/binary, 0:(((8- ((4+byte_size(ItemList_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompcancel">external documentation</a>.
autoCompCancel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompCancel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompactive">external documentation</a>.
autoCompActive(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompActive,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompposstart">external documentation</a>.
autoCompPosStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompPosStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompcomplete">external documentation</a>.
autoCompComplete(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompComplete,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CharacterSet::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompstops">external documentation</a>.
autoCompStops(#wx_ref{type=ThisT,ref=ThisRef},CharacterSet)
 when is_list(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  CharacterSet_UC = unicode:characters_to_binary([CharacterSet,0]),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompStops,
  <<ThisRef:32/?UI,(byte_size(CharacterSet_UC)):32/?UI,(CharacterSet_UC)/binary, 0:(((8- ((0+byte_size(CharacterSet_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), SeparatorCharacter::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetseparator">external documentation</a>.
autoCompSetSeparator(#wx_ref{type=ThisT,ref=ThisRef},SeparatorCharacter)
 when is_integer(SeparatorCharacter) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetSeparator,
  <<ThisRef:32/?UI,SeparatorCharacter:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetseparator">external documentation</a>.
autoCompGetSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompselect">external documentation</a>.
autoCompSelect(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSelect,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Cancel::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetcancelatstart">external documentation</a>.
autoCompSetCancelAtStart(#wx_ref{type=ThisT,ref=ThisRef},Cancel)
 when is_boolean(Cancel) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetCancelAtStart,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Cancel)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetcancelatstart">external documentation</a>.
autoCompGetCancelAtStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetCancelAtStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CharacterSet::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetfillups">external documentation</a>.
autoCompSetFillUps(#wx_ref{type=ThisT,ref=ThisRef},CharacterSet)
 when is_list(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  CharacterSet_UC = unicode:characters_to_binary([CharacterSet,0]),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetFillUps,
  <<ThisRef:32/?UI,(byte_size(CharacterSet_UC)):32/?UI,(CharacterSet_UC)/binary, 0:(((8- ((0+byte_size(CharacterSet_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), ChooseSingle::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetchoosesingle">external documentation</a>.
autoCompSetChooseSingle(#wx_ref{type=ThisT,ref=ThisRef},ChooseSingle)
 when is_boolean(ChooseSingle) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetChooseSingle,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ChooseSingle)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetchoosesingle">external documentation</a>.
autoCompGetChooseSingle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetChooseSingle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), IgnoreCase::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetignorecase">external documentation</a>.
autoCompSetIgnoreCase(#wx_ref{type=ThisT,ref=ThisRef},IgnoreCase)
 when is_boolean(IgnoreCase) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetIgnoreCase,
  <<ThisRef:32/?UI,(wxe_util:from_bool(IgnoreCase)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetignorecase">external documentation</a>.
autoCompGetIgnoreCase(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetIgnoreCase,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), ListType::integer(), ItemList::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrluserlistshow">external documentation</a>.
userListShow(#wx_ref{type=ThisT,ref=ThisRef},ListType,ItemList)
 when is_integer(ListType),is_list(ItemList) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ItemList_UC = unicode:characters_to_binary([ItemList,0]),
  wxe_util:cast(?wxStyledTextCtrl_UserListShow,
  <<ThisRef:32/?UI,ListType:32/?UI,(byte_size(ItemList_UC)):32/?UI,(ItemList_UC)/binary, 0:(((8- ((4+byte_size(ItemList_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), AutoHide::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetautohide">external documentation</a>.
autoCompSetAutoHide(#wx_ref{type=ThisT,ref=ThisRef},AutoHide)
 when is_boolean(AutoHide) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetAutoHide,
  <<ThisRef:32/?UI,(wxe_util:from_bool(AutoHide)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetautohide">external documentation</a>.
autoCompGetAutoHide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetAutoHide,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), DropRestOfWord::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetdroprestofword">external documentation</a>.
autoCompSetDropRestOfWord(#wx_ref{type=ThisT,ref=ThisRef},DropRestOfWord)
 when is_boolean(DropRestOfWord) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetDropRestOfWord,
  <<ThisRef:32/?UI,(wxe_util:from_bool(DropRestOfWord)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetdroprestofword">external documentation</a>.
autoCompGetDropRestOfWord(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetDropRestOfWord,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Type::integer(), Bmp::wxBitmap:wxBitmap()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlregisterimage">external documentation</a>.
registerImage(#wx_ref{type=ThisT,ref=ThisRef},Type,#wx_ref{type=BmpT,ref=BmpRef})
 when is_integer(Type) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:cast(?wxStyledTextCtrl_RegisterImage,
  <<ThisRef:32/?UI,Type:32/?UI,BmpRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlclearregisteredimages">external documentation</a>.
clearRegisteredImages(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ClearRegisteredImages,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgettypeseparator">external documentation</a>.
autoCompGetTypeSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetTypeSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), SeparatorCharacter::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsettypeseparator">external documentation</a>.
autoCompSetTypeSeparator(#wx_ref{type=ThisT,ref=ThisRef},SeparatorCharacter)
 when is_integer(SeparatorCharacter) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetTypeSeparator,
  <<ThisRef:32/?UI,SeparatorCharacter:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CharacterCount::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetmaxwidth">external documentation</a>.
autoCompSetMaxWidth(#wx_ref{type=ThisT,ref=ThisRef},CharacterCount)
 when is_integer(CharacterCount) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetMaxWidth,
  <<ThisRef:32/?UI,CharacterCount:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetmaxwidth">external documentation</a>.
autoCompGetMaxWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetMaxWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), RowCount::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompsetmaxheight">external documentation</a>.
autoCompSetMaxHeight(#wx_ref{type=ThisT,ref=ThisRef},RowCount)
 when is_integer(RowCount) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_AutoCompSetMaxHeight,
  <<ThisRef:32/?UI,RowCount:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetmaxheight">external documentation</a>.
autoCompGetMaxHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetMaxHeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), IndentSize::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetindent">external documentation</a>.
setIndent(#wx_ref{type=ThisT,ref=ThisRef},IndentSize)
 when is_integer(IndentSize) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetIndent,
  <<ThisRef:32/?UI,IndentSize:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetindent">external documentation</a>.
getIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseTabs::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetusetabs">external documentation</a>.
setUseTabs(#wx_ref{type=ThisT,ref=ThisRef},UseTabs)
 when is_boolean(UseTabs) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetUseTabs,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseTabs)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetusetabs">external documentation</a>.
getUseTabs(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetUseTabs,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), IndentSize::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlineindentation">external documentation</a>.
setLineIndentation(#wx_ref{type=ThisT,ref=ThisRef},Line,IndentSize)
 when is_integer(Line),is_integer(IndentSize) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetLineIndentation,
  <<ThisRef:32/?UI,Line:32/?UI,IndentSize:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineindentation">external documentation</a>.
getLineIndentation(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineIndentation,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineindentposition">external documentation</a>.
getLineIndentPosition(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineIndentPosition,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcolumn">external documentation</a>.
getColumn(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetColumn,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetusehorizontalscrollbar">external documentation</a>.
setUseHorizontalScrollBar(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetUseHorizontalScrollBar,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetusehorizontalscrollbar">external documentation</a>.
getUseHorizontalScrollBar(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetUseHorizontalScrollBar,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetindentationguides">external documentation</a>.
setIndentationGuides(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetIndentationGuides,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetindentationguides">external documentation</a>.
getIndentationGuides(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetIndentationGuides,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Column::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethighlightguide">external documentation</a>.
setHighlightGuide(#wx_ref{type=ThisT,ref=ThisRef},Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetHighlightGuide,
  <<ThisRef:32/?UI,Column:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgethighlightguide">external documentation</a>.
getHighlightGuide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetHighlightGuide,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineendposition">external documentation</a>.
getLineEndPosition(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineEndPosition,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcodepage">external documentation</a>.
getCodePage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCodePage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretforeground">external documentation</a>.
getCaretForeground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretForeground,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetreadonly">external documentation</a>.
getReadOnly(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetReadOnly,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcurrentpos">external documentation</a>.
setCurrentPos(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCurrentPos,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionstart">external documentation</a>.
setSelectionStart(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelectionStart,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionstart">external documentation</a>.
getSelectionStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelectionStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionend">external documentation</a>.
setSelectionEnd(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelectionEnd,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionend">external documentation</a>.
getSelectionEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelectionEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Magnification::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetprintmagnification">external documentation</a>.
setPrintMagnification(#wx_ref{type=ThisT,ref=ThisRef},Magnification)
 when is_integer(Magnification) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetPrintMagnification,
  <<ThisRef:32/?UI,Magnification:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintmagnification">external documentation</a>.
getPrintMagnification(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetPrintMagnification,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetprintcolourmode">external documentation</a>.
setPrintColourMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetPrintColourMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintcolourmode">external documentation</a>.
getPrintColourMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetPrintColourMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), MinPos::integer(), MaxPos::integer(), Text::string()) -> integer()
%% @equiv findText(This,MinPos,MaxPos,Text, [])
findText(This,MinPos,MaxPos,Text)
 when is_record(This, wx_ref),is_integer(MinPos),is_integer(MaxPos),is_list(Text) ->
  findText(This,MinPos,MaxPos,Text, []).

%% @spec (This::wxStyledTextCtrl(), MinPos::integer(), MaxPos::integer(), Text::string(), [Option]) -> integer()
%% Option = {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlfindtext">external documentation</a>.
findText(#wx_ref{type=ThisT,ref=ThisRef},MinPos,MaxPos,Text, Options)
 when is_integer(MinPos),is_integer(MaxPos),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxStyledTextCtrl_FindText,
  <<ThisRef:32/?UI,MinPos:32/?UI,MaxPos:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), DoDraw::bool(), StartPos::integer(), EndPos::integer(), Draw::wxDC:wxDC(), Target::wxDC:wxDC(), RenderRect::{X::integer(),Y::integer(),W::integer(),H::integer()}, PageRect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlformatrange">external documentation</a>.
formatRange(#wx_ref{type=ThisT,ref=ThisRef},DoDraw,StartPos,EndPos,#wx_ref{type=DrawT,ref=DrawRef},#wx_ref{type=TargetT,ref=TargetRef},{RenderRectX,RenderRectY,RenderRectW,RenderRectH},{PageRectX,PageRectY,PageRectW,PageRectH})
 when is_boolean(DoDraw),is_integer(StartPos),is_integer(EndPos),is_integer(RenderRectX),is_integer(RenderRectY),is_integer(RenderRectW),is_integer(RenderRectH),is_integer(PageRectX),is_integer(PageRectY),is_integer(PageRectW),is_integer(PageRectH) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(DrawT,wxDC),
  ?CLASS(TargetT,wxDC),
  wxe_util:call(?wxStyledTextCtrl_FormatRange,
  <<ThisRef:32/?UI,(wxe_util:from_bool(DoDraw)):32/?UI,StartPos:32/?UI,EndPos:32/?UI,DrawRef:32/?UI,TargetRef:32/?UI,RenderRectX:32/?UI,RenderRectY:32/?UI,RenderRectW:32/?UI,RenderRectH:32/?UI,PageRectX:32/?UI,PageRectY:32/?UI,PageRectW:32/?UI,PageRectH:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfirstvisibleline">external documentation</a>.
getFirstVisibleLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetFirstVisibleLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetline">external documentation</a>.
getLine(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLine,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinecount">external documentation</a>.
getLineCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineCount,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginleft">external documentation</a>.
setMarginLeft(#wx_ref{type=ThisT,ref=ThisRef},PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginLeft,
  <<ThisRef:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginleft">external documentation</a>.
getMarginLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmarginright">external documentation</a>.
setMarginRight(#wx_ref{type=ThisT,ref=ThisRef},PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMarginRight,
  <<ThisRef:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmarginright">external documentation</a>.
getMarginRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMarginRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmodify">external documentation</a>.
getModify(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetModify,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Start::integer(), End::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselection">external documentation</a>.
setSelection(#wx_ref{type=ThisT,ref=ThisRef},Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelection,
  <<ThisRef:32/?UI,Start:32/?UI,End:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectedtext">external documentation</a>.
getSelectedText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelectedText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StartPos::integer(), EndPos::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextrange">external documentation</a>.
getTextRange(#wx_ref{type=ThisT,ref=ThisRef},StartPos,EndPos)
 when is_integer(StartPos),is_integer(EndPos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTextRange,
  <<ThisRef:32/?UI,StartPos:32/?UI,EndPos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Normal::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhideselection">external documentation</a>.
hideSelection(#wx_ref{type=ThisT,ref=ThisRef},Normal)
 when is_boolean(Normal) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HideSelection,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Normal)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinefromposition">external documentation</a>.
lineFromPosition(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_LineFromPosition,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionfromline">external documentation</a>.
positionFromLine(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PositionFromLine,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Columns::integer(), Lines::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinescroll">external documentation</a>.
lineScroll(#wx_ref{type=ThisT,ref=ThisRef},Columns,Lines)
 when is_integer(Columns),is_integer(Lines) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineScroll,
  <<ThisRef:32/?UI,Columns:32/?UI,Lines:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlensurecaretvisible">external documentation</a>.
ensureCaretVisible(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EnsureCaretVisible,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlreplaceselection">external documentation</a>.
replaceSelection(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_ReplaceSelection,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), ReadOnly::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetreadonly">external documentation</a>.
setReadOnly(#wx_ref{type=ThisT,ref=ThisRef},ReadOnly)
 when is_boolean(ReadOnly) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetReadOnly,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ReadOnly)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcanpaste">external documentation</a>.
canPaste(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_CanPaste,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcanundo">external documentation</a>.
canUndo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_CanUndo,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlemptyundobuffer">external documentation</a>.
emptyUndoBuffer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EmptyUndoBuffer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlundo">external documentation</a>.
undo(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Undo,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcut">external documentation</a>.
cut(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Cut,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcopy">external documentation</a>.
copy(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Copy,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpaste">external documentation</a>.
paste(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Paste,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlclear">external documentation</a>.
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Clear,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettext">external documentation</a>.
setText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettext">external documentation</a>.
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextlength">external documentation</a>.
getTextLength(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTextLength,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetovertype">external documentation</a>.
getOvertype(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetOvertype,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretwidth">external documentation</a>.
setCaretWidth(#wx_ref{type=ThisT,ref=ThisRef},PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretWidth,
  <<ThisRef:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretwidth">external documentation</a>.
getCaretWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettargetstart">external documentation</a>.
setTargetStart(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetTargetStart,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettargetstart">external documentation</a>.
getTargetStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTargetStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettargetend">external documentation</a>.
setTargetEnd(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetTargetEnd,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettargetend">external documentation</a>.
getTargetEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTargetEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlreplacetarget">external documentation</a>.
replaceTarget(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxStyledTextCtrl_ReplaceTarget,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchintarget">external documentation</a>.
searchInTarget(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxStyledTextCtrl_SearchInTarget,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Flags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetsearchflags">external documentation</a>.
setSearchFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSearchFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetsearchflags">external documentation</a>.
getSearchFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSearchFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), Definition::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipshow">external documentation</a>.
callTipShow(#wx_ref{type=ThisT,ref=ThisRef},Pos,Definition)
 when is_integer(Pos),is_list(Definition) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Definition_UC = unicode:characters_to_binary([Definition,0]),
  wxe_util:cast(?wxStyledTextCtrl_CallTipShow,
  <<ThisRef:32/?UI,Pos:32/?UI,(byte_size(Definition_UC)):32/?UI,(Definition_UC)/binary, 0:(((8- ((4+byte_size(Definition_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipcancel">external documentation</a>.
callTipCancel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipCancel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipactive">external documentation</a>.
callTipActive(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_CallTipActive,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipposatstart">external documentation</a>.
callTipPosAtStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_CallTipPosAtStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Start::integer(), End::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsethighlight">external documentation</a>.
callTipSetHighlight(#wx_ref{type=ThisT,ref=ThisRef},Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipSetHighlight,
  <<ThisRef:32/?UI,Start:32/?UI,End:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetbackground">external documentation</a>.
callTipSetBackground(#wx_ref{type=ThisT,ref=ThisRef},Back)
 when tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipSetBackground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetforeground">external documentation</a>.
callTipSetForeground(#wx_ref{type=ThisT,ref=ThisRef},Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipSetForeground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipsetforegroundhighlight">external documentation</a>.
callTipSetForegroundHighlight(#wx_ref{type=ThisT,ref=ThisRef},Fore)
 when tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipSetForegroundHighlight,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), TabSize::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcalltipusestyle">external documentation</a>.
callTipUseStyle(#wx_ref{type=ThisT,ref=ThisRef},TabSize)
 when is_integer(TabSize) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CallTipUseStyle,
  <<ThisRef:32/?UI,TabSize:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvisiblefromdocline">external documentation</a>.
visibleFromDocLine(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_VisibleFromDocLine,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), LineDisplay::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldoclinefromvisible">external documentation</a>.
docLineFromVisible(#wx_ref{type=ThisT,ref=ThisRef},LineDisplay)
 when is_integer(LineDisplay) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_DocLineFromVisible,
  <<ThisRef:32/?UI,LineDisplay:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwrapcount">external documentation</a>.
wrapCount(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_WrapCount,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), Level::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldlevel">external documentation</a>.
setFoldLevel(#wx_ref{type=ThisT,ref=ThisRef},Line,Level)
 when is_integer(Line),is_integer(Level) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetFoldLevel,
  <<ThisRef:32/?UI,Line:32/?UI,Level:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldlevel">external documentation</a>.
getFoldLevel(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetFoldLevel,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), Level::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlastchild">external documentation</a>.
getLastChild(#wx_ref{type=ThisT,ref=ThisRef},Line,Level)
 when is_integer(Line),is_integer(Level) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLastChild,
  <<ThisRef:32/?UI,Line:32/?UI,Level:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldparent">external documentation</a>.
getFoldParent(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetFoldParent,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), LineStart::integer(), LineEnd::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlshowlines">external documentation</a>.
showLines(#wx_ref{type=ThisT,ref=ThisRef},LineStart,LineEnd)
 when is_integer(LineStart),is_integer(LineEnd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ShowLines,
  <<ThisRef:32/?UI,LineStart:32/?UI,LineEnd:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), LineStart::integer(), LineEnd::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhidelines">external documentation</a>.
hideLines(#wx_ref{type=ThisT,ref=ThisRef},LineStart,LineEnd)
 when is_integer(LineStart),is_integer(LineEnd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HideLines,
  <<ThisRef:32/?UI,LineStart:32/?UI,LineEnd:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlinevisible">external documentation</a>.
getLineVisible(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineVisible,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), Expanded::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldexpanded">external documentation</a>.
setFoldExpanded(#wx_ref{type=ThisT,ref=ThisRef},Line,Expanded)
 when is_integer(Line),is_boolean(Expanded) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetFoldExpanded,
  <<ThisRef:32/?UI,Line:32/?UI,(wxe_util:from_bool(Expanded)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetfoldexpanded">external documentation</a>.
getFoldExpanded(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetFoldExpanded,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltogglefold">external documentation</a>.
toggleFold(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ToggleFold,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlensurevisible">external documentation</a>.
ensureVisible(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EnsureVisible,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Flags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldflags">external documentation</a>.
setFoldFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetFoldFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlensurevisibleenforcepolicy">external documentation</a>.
ensureVisibleEnforcePolicy(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EnsureVisibleEnforcePolicy,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), TabIndents::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettabindents">external documentation</a>.
setTabIndents(#wx_ref{type=ThisT,ref=ThisRef},TabIndents)
 when is_boolean(TabIndents) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetTabIndents,
  <<ThisRef:32/?UI,(wxe_util:from_bool(TabIndents)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettabindents">external documentation</a>.
getTabIndents(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTabIndents,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), BsUnIndents::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetbackspaceunindents">external documentation</a>.
setBackSpaceUnIndents(#wx_ref{type=ThisT,ref=ThisRef},BsUnIndents)
 when is_boolean(BsUnIndents) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetBackSpaceUnIndents,
  <<ThisRef:32/?UI,(wxe_util:from_bool(BsUnIndents)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetbackspaceunindents">external documentation</a>.
getBackSpaceUnIndents(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetBackSpaceUnIndents,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PeriodMilliseconds::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmousedwelltime">external documentation</a>.
setMouseDwellTime(#wx_ref{type=ThisT,ref=ThisRef},PeriodMilliseconds)
 when is_integer(PeriodMilliseconds) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMouseDwellTime,
  <<ThisRef:32/?UI,PeriodMilliseconds:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmousedwelltime">external documentation</a>.
getMouseDwellTime(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMouseDwellTime,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), OnlyWordCharacters::bool()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordstartposition">external documentation</a>.
wordStartPosition(#wx_ref{type=ThisT,ref=ThisRef},Pos,OnlyWordCharacters)
 when is_integer(Pos),is_boolean(OnlyWordCharacters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_WordStartPosition,
  <<ThisRef:32/?UI,Pos:32/?UI,(wxe_util:from_bool(OnlyWordCharacters)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), OnlyWordCharacters::bool()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordendposition">external documentation</a>.
wordEndPosition(#wx_ref{type=ThisT,ref=ThisRef},Pos,OnlyWordCharacters)
 when is_integer(Pos),is_boolean(OnlyWordCharacters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_WordEndPosition,
  <<ThisRef:32/?UI,Pos:32/?UI,(wxe_util:from_bool(OnlyWordCharacters)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapmode">external documentation</a>.
setWrapMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWrapMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapmode">external documentation</a>.
getWrapMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetWrapMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), WrapVisualFlags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapvisualflags">external documentation</a>.
setWrapVisualFlags(#wx_ref{type=ThisT,ref=ThisRef},WrapVisualFlags)
 when is_integer(WrapVisualFlags) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWrapVisualFlags,
  <<ThisRef:32/?UI,WrapVisualFlags:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapvisualflags">external documentation</a>.
getWrapVisualFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetWrapVisualFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), WrapVisualFlagsLocation::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapvisualflagslocation">external documentation</a>.
setWrapVisualFlagsLocation(#wx_ref{type=ThisT,ref=ThisRef},WrapVisualFlagsLocation)
 when is_integer(WrapVisualFlagsLocation) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWrapVisualFlagsLocation,
  <<ThisRef:32/?UI,WrapVisualFlagsLocation:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapvisualflagslocation">external documentation</a>.
getWrapVisualFlagsLocation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetWrapVisualFlagsLocation,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Indent::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwrapstartindent">external documentation</a>.
setWrapStartIndent(#wx_ref{type=ThisT,ref=ThisRef},Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetWrapStartIndent,
  <<ThisRef:32/?UI,Indent:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetwrapstartindent">external documentation</a>.
getWrapStartIndent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetWrapStartIndent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlayoutcache">external documentation</a>.
setLayoutCache(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetLayoutCache,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlayoutcache">external documentation</a>.
getLayoutCache(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLayoutCache,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetscrollwidth">external documentation</a>.
setScrollWidth(#wx_ref{type=ThisT,ref=ThisRef},PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetScrollWidth,
  <<ThisRef:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetscrollwidth">external documentation</a>.
getScrollWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetScrollWidth,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Text::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltextwidth">external documentation</a>.
textWidth(#wx_ref{type=ThisT,ref=ThisRef},Style,Text)
 when is_integer(Style),is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxStyledTextCtrl_TextWidth,
  <<ThisRef:32/?UI,Style:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetendatlastline">external documentation</a>.
getEndAtLastLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEndAtLastLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltextheight">external documentation</a>.
textHeight(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_TextHeight,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Show::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetuseverticalscrollbar">external documentation</a>.
setUseVerticalScrollBar(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetUseVerticalScrollBar,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetuseverticalscrollbar">external documentation</a>.
getUseVerticalScrollBar(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetUseVerticalScrollBar,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlappendtext">external documentation</a>.
appendText(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_AppendText,
  <<ThisRef:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettwophasedraw">external documentation</a>.
getTwoPhaseDraw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTwoPhaseDraw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), TwoPhase::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettwophasedraw">external documentation</a>.
setTwoPhaseDraw(#wx_ref{type=ThisT,ref=ThisRef},TwoPhase)
 when is_boolean(TwoPhase) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetTwoPhaseDraw,
  <<ThisRef:32/?UI,(wxe_util:from_bool(TwoPhase)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltargetfromselection">external documentation</a>.
targetFromSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_TargetFromSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinesjoin">external documentation</a>.
linesJoin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LinesJoin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), PixelWidth::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinessplit">external documentation</a>.
linesSplit(#wx_ref{type=ThisT,ref=ThisRef},PixelWidth)
 when is_integer(PixelWidth) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LinesSplit,
  <<ThisRef:32/?UI,PixelWidth:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldmargincolour">external documentation</a>.
setFoldMarginColour(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetFoldMarginColour,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetfoldmarginhicolour">external documentation</a>.
setFoldMarginHiColour(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetFoldMarginHiColour,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinedown">external documentation</a>.
lineDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinedownextend">external documentation</a>.
lineDownExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineDownExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineup">external documentation</a>.
lineUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineupextend">external documentation</a>.
lineUpExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineUpExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleft">external documentation</a>.
charLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleftextend">external documentation</a>.
charLeftExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharLeftExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharright">external documentation</a>.
charRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharrightextend">external documentation</a>.
charRightExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharRightExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleft">external documentation</a>.
wordLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftextend">external documentation</a>.
wordLeftExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordLeftExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordright">external documentation</a>.
wordRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightextend">external documentation</a>.
wordRightExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordRightExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhome">external documentation</a>.
home(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Home,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhomeextend">external documentation</a>.
homeExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HomeExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineend">external documentation</a>.
lineEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineendextend">external documentation</a>.
lineEndExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentstart">external documentation</a>.
documentStart(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DocumentStart,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentstartextend">external documentation</a>.
documentStartExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DocumentStartExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentend">external documentation</a>.
documentEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DocumentEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldocumentendextend">external documentation</a>.
documentEndExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DocumentEndExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpageup">external documentation</a>.
pageUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpageupextend">external documentation</a>.
pageUpExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageUpExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedown">external documentation</a>.
pageDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedownextend">external documentation</a>.
pageDownExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageDownExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrledittoggleovertype">external documentation</a>.
editToggleOvertype(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_EditToggleOvertype,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcancel">external documentation</a>.
cancel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Cancel,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldeleteback">external documentation</a>.
deleteBack(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DeleteBack,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltab">external documentation</a>.
tab(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Tab,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlbacktab">external documentation</a>.
backTab(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_BackTab,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlnewline">external documentation</a>.
newLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_NewLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlformfeed">external documentation</a>.
formFeed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_FormFeed,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvchome">external documentation</a>.
vCHome(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_VCHome,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomeextend">external documentation</a>.
vCHomeExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_VCHomeExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlzoomin">external documentation</a>.
zoomIn(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ZoomIn,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlzoomout">external documentation</a>.
zoomOut(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ZoomOut,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldelwordleft">external documentation</a>.
delWordLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DelWordLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldelwordright">external documentation</a>.
delWordRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DelWordRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinecut">external documentation</a>.
lineCut(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineCut,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinedelete">external documentation</a>.
lineDelete(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineDelete,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinetranspose">external documentation</a>.
lineTranspose(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineTranspose,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineduplicate">external documentation</a>.
lineDuplicate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineDuplicate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllowercase">external documentation</a>.
lowerCase(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LowerCase,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrluppercase">external documentation</a>.
upperCase(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_UpperCase,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinescrolldown">external documentation</a>.
lineScrollDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineScrollDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinescrollup">external documentation</a>.
lineScrollUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineScrollUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldeletebacknotline">external documentation</a>.
deleteBackNotLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DeleteBackNotLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhomedisplay">external documentation</a>.
homeDisplay(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HomeDisplay,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhomedisplayextend">external documentation</a>.
homeDisplayExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HomeDisplayExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineenddisplay">external documentation</a>.
lineEndDisplay(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndDisplay,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineenddisplayextend">external documentation</a>.
lineEndDisplayExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndDisplayExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhomewrapextend">external documentation</a>.
homeWrapExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HomeWrapExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineendwrap">external documentation</a>.
lineEndWrap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndWrap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineendwrapextend">external documentation</a>.
lineEndWrapExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndWrapExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomewrap">external documentation</a>.
vCHomeWrap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_VCHomeWrap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomewrapextend">external documentation</a>.
vCHomeWrapExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_VCHomeWrapExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinecopy">external documentation</a>.
lineCopy(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineCopy,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlmovecaretinsideview">external documentation</a>.
moveCaretInsideView(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_MoveCaretInsideView,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinelength">external documentation</a>.
lineLength(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_LineLength,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos1::integer(), Pos2::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlbracehighlight">external documentation</a>.
braceHighlight(#wx_ref{type=ThisT,ref=ThisRef},Pos1,Pos2)
 when is_integer(Pos1),is_integer(Pos2) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_BraceHighlight,
  <<ThisRef:32/?UI,Pos1:32/?UI,Pos2:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlbracebadlight">external documentation</a>.
braceBadLight(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_BraceBadLight,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlbracematch">external documentation</a>.
braceMatch(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_BraceMatch,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetvieweol">external documentation</a>.
getViewEOL(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetViewEOL,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Visible::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvieweol">external documentation</a>.
setViewEOL(#wx_ref{type=ThisT,ref=ThisRef},Visible)
 when is_boolean(Visible) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetViewEOL,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Visible)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Mask::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmodeventmask">external documentation</a>.
setModEventMask(#wx_ref{type=ThisT,ref=ThisRef},Mask)
 when is_integer(Mask) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetModEventMask,
  <<ThisRef:32/?UI,Mask:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgecolumn">external documentation</a>.
getEdgeColumn(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEdgeColumn,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Column::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetedgecolumn">external documentation</a>.
setEdgeColumn(#wx_ref{type=ThisT,ref=ThisRef},Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetEdgeColumn,
  <<ThisRef:32/?UI,Column:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgemode">external documentation</a>.
getEdgeMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEdgeMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> wx:colour()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetedgecolour">external documentation</a>.
getEdgeColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetEdgeColour,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), EdgeColour::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetedgecolour">external documentation</a>.
setEdgeColour(#wx_ref{type=ThisT,ref=ThisRef},EdgeColour)
 when tuple_size(EdgeColour) =:= 3; tuple_size(EdgeColour) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetEdgeColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(EdgeColour)):16/binary>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchanchor">external documentation</a>.
searchAnchor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SearchAnchor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Flags::integer(), Text::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchnext">external documentation</a>.
searchNext(#wx_ref{type=ThisT,ref=ThisRef},Flags,Text)
 when is_integer(Flags),is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxStyledTextCtrl_SearchNext,
  <<ThisRef:32/?UI,Flags:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Flags::integer(), Text::string()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsearchprev">external documentation</a>.
searchPrev(#wx_ref{type=ThisT,ref=ThisRef},Flags,Text)
 when is_integer(Flags),is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxStyledTextCtrl_SearchPrev,
  <<ThisRef:32/?UI,Flags:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinesonscreen">external documentation</a>.
linesOnScreen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_LinesOnScreen,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), AllowPopUp::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlusepopup">external documentation</a>.
usePopUp(#wx_ref{type=ThisT,ref=ThisRef},AllowPopUp)
 when is_boolean(AllowPopUp) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_UsePopUp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(AllowPopUp)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlselectionisrectangle">external documentation</a>.
selectionIsRectangle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_SelectionIsRectangle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Zoom::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetzoom">external documentation</a>.
setZoom(#wx_ref{type=ThisT,ref=ThisRef},Zoom)
 when is_integer(Zoom) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetZoom,
  <<ThisRef:32/?UI,Zoom:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetzoom">external documentation</a>.
getZoom(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetZoom,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmodeventmask">external documentation</a>.
getModEventMask(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetModEventMask,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Focus::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstcfocus">external documentation</a>.
setSTCFocus(#wx_ref{type=ThisT,ref=ThisRef},Focus)
 when is_boolean(Focus) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSTCFocus,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Focus)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstcfocus">external documentation</a>.
getSTCFocus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSTCFocus,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StatusCode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstatus">external documentation</a>.
setStatus(#wx_ref{type=ThisT,ref=ThisRef},StatusCode)
 when is_integer(StatusCode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetStatus,
  <<ThisRef:32/?UI,StatusCode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstatus">external documentation</a>.
getStatus(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetStatus,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Captures::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmousedowncaptures">external documentation</a>.
setMouseDownCaptures(#wx_ref{type=ThisT,ref=ThisRef},Captures)
 when is_boolean(Captures) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMouseDownCaptures,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Captures)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetmousedowncaptures">external documentation</a>.
getMouseDownCaptures(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetMouseDownCaptures,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CursorType::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetstccursor">external documentation</a>.
setSTCCursor(#wx_ref{type=ThisT,ref=ThisRef},CursorType)
 when is_integer(CursorType) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSTCCursor,
  <<ThisRef:32/?UI,CursorType:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstccursor">external documentation</a>.
getSTCCursor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSTCCursor,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Symbol::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcontrolcharsymbol">external documentation</a>.
setControlCharSymbol(#wx_ref{type=ThisT,ref=ThisRef},Symbol)
 when is_integer(Symbol) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetControlCharSymbol,
  <<ThisRef:32/?UI,Symbol:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcontrolcharsymbol">external documentation</a>.
getControlCharSymbol(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetControlCharSymbol,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartleft">external documentation</a>.
wordPartLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordPartLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartleftextend">external documentation</a>.
wordPartLeftExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordPartLeftExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartright">external documentation</a>.
wordPartRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordPartRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordpartrightextend">external documentation</a>.
wordPartRightExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordPartRightExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), VisiblePolicy::integer(), VisibleSlop::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvisiblepolicy">external documentation</a>.
setVisiblePolicy(#wx_ref{type=ThisT,ref=ThisRef},VisiblePolicy,VisibleSlop)
 when is_integer(VisiblePolicy),is_integer(VisibleSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetVisiblePolicy,
  <<ThisRef:32/?UI,VisiblePolicy:32/?UI,VisibleSlop:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldellineleft">external documentation</a>.
delLineLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DelLineLeft,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldellineright">external documentation</a>.
delLineRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_DelLineRight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetxoffset">external documentation</a>.
getXOffset(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetXOffset,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlchoosecaretx">external documentation</a>.
chooseCaretX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ChooseCaretX,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CaretPolicy::integer(), CaretSlop::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetxcaretpolicy">external documentation</a>.
setXCaretPolicy(#wx_ref{type=ThisT,ref=ThisRef},CaretPolicy,CaretSlop)
 when is_integer(CaretPolicy),is_integer(CaretSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetXCaretPolicy,
  <<ThisRef:32/?UI,CaretPolicy:32/?UI,CaretSlop:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), CaretPolicy::integer(), CaretSlop::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetycaretpolicy">external documentation</a>.
setYCaretPolicy(#wx_ref{type=ThisT,ref=ThisRef},CaretPolicy,CaretSlop)
 when is_integer(CaretPolicy),is_integer(CaretSlop) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetYCaretPolicy,
  <<ThisRef:32/?UI,CaretPolicy:32/?UI,CaretSlop:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetprintwrapmode">external documentation</a>.
getPrintWrapMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetPrintWrapMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Fore::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactiveforeground">external documentation</a>.
setHotspotActiveForeground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Fore)
 when is_boolean(UseSetting),tuple_size(Fore) =:= 3; tuple_size(Fore) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetHotspotActiveForeground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Fore)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), UseSetting::bool(), Back::wx:colour()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactivebackground">external documentation</a>.
setHotspotActiveBackground(#wx_ref{type=ThisT,ref=ThisRef},UseSetting,Back)
 when is_boolean(UseSetting),tuple_size(Back) =:= 3; tuple_size(Back) =:= 4 ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetHotspotActiveBackground,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseSetting)):32/?UI,(wxe_util:colour_bin(Back)):16/binary>>).

%% @spec (This::wxStyledTextCtrl(), Underline::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotactiveunderline">external documentation</a>.
setHotspotActiveUnderline(#wx_ref{type=ThisT,ref=ThisRef},Underline)
 when is_boolean(Underline) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetHotspotActiveUnderline,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Underline)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), SingleLine::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethotspotsingleline">external documentation</a>.
setHotspotSingleLine(#wx_ref{type=ThisT,ref=ThisRef},SingleLine)
 when is_boolean(SingleLine) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetHotspotSingleLine,
  <<ThisRef:32/?UI,(wxe_util:from_bool(SingleLine)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlparadownextend">external documentation</a>.
paraDownExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ParaDownExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlparaup">external documentation</a>.
paraUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ParaUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlparaupextend">external documentation</a>.
paraUpExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ParaUpExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionbefore">external documentation</a>.
positionBefore(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PositionBefore,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpositionafter">external documentation</a>.
positionAfter(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PositionAfter,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Start::integer(), End::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcopyrange">external documentation</a>.
copyRange(#wx_ref{type=ThisT,ref=ThisRef},Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CopyRange,
  <<ThisRef:32/?UI,Start:32/?UI,End:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Length::integer(), Text::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcopytext">external documentation</a>.
copyText(#wx_ref{type=ThisT,ref=ThisRef},Length,Text)
 when is_integer(Length),is_list(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:cast(?wxStyledTextCtrl_CopyText,
  <<ThisRef:32/?UI,Length:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Mode::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetselectionmode">external documentation</a>.
setSelectionMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetSelectionMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectionmode">external documentation</a>.
getSelectionMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelectionMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllinedownrectextend">external documentation</a>.
lineDownRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineDownRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineuprectextend">external documentation</a>.
lineUpRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineUpRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharleftrectextend">external documentation</a>.
charLeftRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharLeftRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcharrightrectextend">external documentation</a>.
charRightRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CharRightRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlhomerectextend">external documentation</a>.
homeRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_HomeRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlvchomerectextend">external documentation</a>.
vCHomeRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_VCHomeRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrllineendrectextend">external documentation</a>.
lineEndRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_LineEndRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpageuprectextend">external documentation</a>.
pageUpRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageUpRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpagedownrectextend">external documentation</a>.
pageDownRectExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_PageDownRectExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpageup">external documentation</a>.
stutteredPageUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StutteredPageUp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpageupextend">external documentation</a>.
stutteredPageUpExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StutteredPageUpExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpagedown">external documentation</a>.
stutteredPageDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StutteredPageDown,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstutteredpagedownextend">external documentation</a>.
stutteredPageDownExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StutteredPageDownExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftend">external documentation</a>.
wordLeftEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordLeftEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordleftendextend">external documentation</a>.
wordLeftEndExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordLeftEndExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightend">external documentation</a>.
wordRightEnd(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordRightEnd,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlwordrightendextend">external documentation</a>.
wordRightEndExtend(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_WordRightEndExtend,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Characters::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetwhitespacechars">external documentation</a>.
setWhitespaceChars(#wx_ref{type=ThisT,ref=ThisRef},Characters)
 when is_list(Characters) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Characters_UC = unicode:characters_to_binary([Characters,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetWhitespaceChars,
  <<ThisRef:32/?UI,(byte_size(Characters_UC)):32/?UI,(Characters_UC)/binary, 0:(((8- ((0+byte_size(Characters_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcharsdefault">external documentation</a>.
setCharsDefault(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCharsDefault,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlautocompgetcurrent">external documentation</a>.
autoCompGetCurrent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_AutoCompGetCurrent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Bytes::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlallocate">external documentation</a>.
allocate(#wx_ref{type=ThisT,ref=ThisRef},Bytes)
 when is_integer(Bytes) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Allocate,
  <<ThisRef:32/?UI,Bytes:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer(), Column::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlfindcolumn">external documentation</a>.
findColumn(#wx_ref{type=ThisT,ref=ThisRef},Line,Column)
 when is_integer(Line),is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_FindColumn,
  <<ThisRef:32/?UI,Line:32/?UI,Column:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretsticky">external documentation</a>.
getCaretSticky(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretSticky,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), UseCaretStickyBehaviour::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretsticky">external documentation</a>.
setCaretSticky(#wx_ref{type=ThisT,ref=ThisRef},UseCaretStickyBehaviour)
 when is_boolean(UseCaretStickyBehaviour) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretSticky,
  <<ThisRef:32/?UI,(wxe_util:from_bool(UseCaretStickyBehaviour)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrltogglecaretsticky">external documentation</a>.
toggleCaretSticky(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ToggleCaretSticky,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Convert::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetpasteconvertendings">external documentation</a>.
setPasteConvertEndings(#wx_ref{type=ThisT,ref=ThisRef},Convert)
 when is_boolean(Convert) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetPasteConvertEndings,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Convert)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetpasteconvertendings">external documentation</a>.
getPasteConvertEndings(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetPasteConvertEndings,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlselectionduplicate">external documentation</a>.
selectionDuplicate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SelectionDuplicate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Alpha::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetcaretlinebackalpha">external documentation</a>.
setCaretLineBackAlpha(#wx_ref{type=ThisT,ref=ThisRef},Alpha)
 when is_integer(Alpha) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetCaretLineBackAlpha,
  <<ThisRef:32/?UI,Alpha:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcaretlinebackalpha">external documentation</a>.
getCaretLineBackAlpha(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCaretLineBackAlpha,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstartrecord">external documentation</a>.
startRecord(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StartRecord,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstoprecord">external documentation</a>.
stopRecord(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StopRecord,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Lexer::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlexer">external documentation</a>.
setLexer(#wx_ref{type=ThisT,ref=ThisRef},Lexer)
 when is_integer(Lexer) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetLexer,
  <<ThisRef:32/?UI,Lexer:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlexer">external documentation</a>.
getLexer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLexer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Start::integer(), End::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcolourise">external documentation</a>.
colourise(#wx_ref{type=ThisT,ref=ThisRef},Start,End)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_Colourise,
  <<ThisRef:32/?UI,Start:32/?UI,End:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Key::string(), Value::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetproperty">external documentation</a>.
setProperty(#wx_ref{type=ThisT,ref=ThisRef},Key,Value)
 when is_list(Key),is_list(Value) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Key_UC = unicode:characters_to_binary([Key,0]),
  Value_UC = unicode:characters_to_binary([Value,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetProperty,
  <<ThisRef:32/?UI,(byte_size(Key_UC)):32/?UI,(Key_UC)/binary, 0:(((8- ((0+byte_size(Key_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Value_UC)):32/?UI,(Value_UC)/binary, 0:(((8- ((4+byte_size(Value_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), KeywordSet::integer(), KeyWords::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetkeywords">external documentation</a>.
setKeyWords(#wx_ref{type=ThisT,ref=ThisRef},KeywordSet,KeyWords)
 when is_integer(KeywordSet),is_list(KeyWords) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  KeyWords_UC = unicode:characters_to_binary([KeyWords,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetKeyWords,
  <<ThisRef:32/?UI,KeywordSet:32/?UI,(byte_size(KeyWords_UC)):32/?UI,(KeyWords_UC)/binary, 0:(((8- ((4+byte_size(KeyWords_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Language::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlexerlanguage">external documentation</a>.
setLexerLanguage(#wx_ref{type=ThisT,ref=ThisRef},Language)
 when is_list(Language) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Language_UC = unicode:characters_to_binary([Language,0]),
  wxe_util:cast(?wxStyledTextCtrl_SetLexerLanguage,
  <<ThisRef:32/?UI,(byte_size(Language_UC)):32/?UI,(Language_UC)/binary, 0:(((8- ((0+byte_size(Language_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Key::string()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetproperty">external documentation</a>.
getProperty(#wx_ref{type=ThisT,ref=ThisRef},Key)
 when is_list(Key) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Key_UC = unicode:characters_to_binary([Key,0]),
  wxe_util:call(?wxStyledTextCtrl_GetProperty,
  <<ThisRef:32/?UI,(byte_size(Key_UC)):32/?UI,(Key_UC)/binary, 0:(((8- ((0+byte_size(Key_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetstylebitsneeded">external documentation</a>.
getStyleBitsNeeded(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetStyleBitsNeeded,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurrentline">external documentation</a>.
getCurrentLine(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCurrentLine,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StyleNum::integer(), Spec::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetspec">external documentation</a>.
styleSetSpec(#wx_ref{type=ThisT,ref=ThisRef},StyleNum,Spec)
 when is_integer(StyleNum),is_list(Spec) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Spec_UC = unicode:characters_to_binary([Spec,0]),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetSpec,
  <<ThisRef:32/?UI,StyleNum:32/?UI,(byte_size(Spec_UC)):32/?UI,(Spec_UC)/binary, 0:(((8- ((4+byte_size(Spec_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), StyleNum::integer(), Font::wxFont:wxFont()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfont">external documentation</a>.
styleSetFont(#wx_ref{type=ThisT,ref=ThisRef},StyleNum,#wx_ref{type=FontT,ref=FontRef})
 when is_integer(StyleNum) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(FontT,wxFont),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetFont,
  <<ThisRef:32/?UI,StyleNum:32/?UI,FontRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StyleNum::integer(), Size::integer(), FaceName::string(), Bold::bool(), Italic::bool(), Underline::bool()) -> ok
%% @equiv styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline, [])
styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline)
 when is_record(This, wx_ref),is_integer(StyleNum),is_integer(Size),is_list(FaceName),is_boolean(Bold),is_boolean(Italic),is_boolean(Underline) ->
  styleSetFontAttr(This,StyleNum,Size,FaceName,Bold,Italic,Underline, []).

%% @spec (This::wxStyledTextCtrl(), StyleNum::integer(), Size::integer(), FaceName::string(), Bold::bool(), Italic::bool(), Underline::bool(), [Option]) -> ok
%% Option = {encoding, WxFontEncoding}
%% WxFontEncoding = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfontattr">external documentation</a>.
%%<br /> WxFontEncoding is one of ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
styleSetFontAttr(#wx_ref{type=ThisT,ref=ThisRef},StyleNum,Size,FaceName,Bold,Italic,Underline, Options)
 when is_integer(StyleNum),is_integer(Size),is_list(FaceName),is_boolean(Bold),is_boolean(Italic),is_boolean(Underline),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  FaceName_UC = unicode:characters_to_binary([FaceName,0]),
  MOpts = fun({encoding, Encoding}, Acc) -> [<<1:32/?UI,Encoding:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetFontAttr,
  <<ThisRef:32/?UI,StyleNum:32/?UI,Size:32/?UI,(byte_size(FaceName_UC)):32/?UI,(FaceName_UC)/binary, 0:(((8- ((0+byte_size(FaceName_UC)) band 16#7)) band 16#7))/unit:8,(wxe_util:from_bool(Bold)):32/?UI,(wxe_util:from_bool(Italic)):32/?UI,(wxe_util:from_bool(Underline)):32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), CharacterSet::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetcharacterset">external documentation</a>.
styleSetCharacterSet(#wx_ref{type=ThisT,ref=ThisRef},Style,CharacterSet)
 when is_integer(Style),is_integer(CharacterSet) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetCharacterSet,
  <<ThisRef:32/?UI,Style:32/?UI,CharacterSet:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Style::integer(), Encoding::WxFontEncoding) -> ok
%% WxFontEncoding = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlstylesetfontencoding">external documentation</a>.
%%<br /> WxFontEncoding is one of ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
styleSetFontEncoding(#wx_ref{type=ThisT,ref=ThisRef},Style,Encoding)
 when is_integer(Style),is_integer(Encoding) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_StyleSetFontEncoding,
  <<ThisRef:32/?UI,Style:32/?UI,Encoding:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Cmd::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlcmdkeyexecute">external documentation</a>.
cmdKeyExecute(#wx_ref{type=ThisT,ref=ThisRef},Cmd)
 when is_integer(Cmd) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_CmdKeyExecute,
  <<ThisRef:32/?UI,Cmd:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Left::integer(), Right::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetmargins">external documentation</a>.
setMargins(#wx_ref{type=ThisT,ref=ThisRef},Left,Right)
 when is_integer(Left),is_integer(Right) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetMargins,
  <<ThisRef:32/?UI,Left:32/?UI,Right:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> {StartPos::integer(),EndPos::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselection">external documentation</a>.
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlpointfromposition">external documentation</a>.
pointFromPosition(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_PointFromPosition,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlscrolltoline">external documentation</a>.
scrollToLine(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ScrollToLine,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Column::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlscrolltocolumn">external documentation</a>.
scrollToColumn(#wx_ref{type=ThisT,ref=ThisRef},Column)
 when is_integer(Column) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_ScrollToColumn,
  <<ThisRef:32/?UI,Column:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Msg::integer()) -> integer()
%% @equiv sendMsg(This,Msg, [])
sendMsg(This,Msg)
 when is_record(This, wx_ref),is_integer(Msg) ->
  sendMsg(This,Msg, []).

%% @spec (This::wxStyledTextCtrl(), Msg::integer(), [Option]) -> integer()
%% Option = {wp, integer()} | {lp, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsendmsg">external documentation</a>.
sendMsg(#wx_ref{type=ThisT,ref=ThisRef},Msg, Options)
 when is_integer(Msg),is_list(Options) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  MOpts = fun({wp, Wp}, Acc) -> [<<1:32/?UI,Wp:32/?UI>>|Acc];
          ({lp, Lp}, Acc) -> [<<2:32/?UI,Lp:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxStyledTextCtrl_SendMsg,
  <<ThisRef:32/?UI,Msg:32/?UI, BinOpt/binary>>).

%% @spec (This::wxStyledTextCtrl(), Bar::wxScrollBar:wxScrollBar()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetvscrollbar">external documentation</a>.
setVScrollBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BarT,ref=BarRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BarT,wxScrollBar),
  wxe_util:cast(?wxStyledTextCtrl_SetVScrollBar,
  <<ThisRef:32/?UI,BarRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Bar::wxScrollBar:wxScrollBar()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsethscrollbar">external documentation</a>.
setHScrollBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BarT,ref=BarRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  ?CLASS(BarT,wxScrollBar),
  wxe_util:cast(?wxStyledTextCtrl_SetHScrollBar,
  <<ThisRef:32/?UI,BarRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlastkeydownprocessed">external documentation</a>.
getLastKeydownProcessed(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLastKeydownProcessed,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Val::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsetlastkeydownprocessed">external documentation</a>.
setLastKeydownProcessed(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when is_boolean(Val) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:cast(?wxStyledTextCtrl_SetLastKeydownProcessed,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Val)):32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Filename::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsavefile">external documentation</a>.
saveFile(#wx_ref{type=ThisT,ref=ThisRef},Filename)
 when is_list(Filename) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  wxe_util:call(?wxStyledTextCtrl_SaveFile,
  <<ThisRef:32/?UI,(byte_size(Filename_UC)):32/?UI,(Filename_UC)/binary, 0:(((8- ((0+byte_size(Filename_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), Filename::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlloadfile">external documentation</a>.
loadFile(#wx_ref{type=ThisT,ref=ThisRef},Filename)
 when is_list(Filename) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  wxe_util:call(?wxStyledTextCtrl_LoadFile,
  <<ThisRef:32/?UI,(byte_size(Filename_UC)):32/?UI,(Filename_UC)/binary, 0:(((8- ((0+byte_size(Filename_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl(), X::integer(), Y::integer(), Def::WxDragResult) -> WxDragResult
%% WxDragResult = integer()
%% WxDragResult = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldodragover">external documentation</a>.
%%<br /> WxDragResult is one of ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
%%<br /> WxDragResult is one of ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
doDragOver(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Def)
 when is_integer(X),is_integer(Y),is_integer(Def) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_DoDragOver,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Def:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), X::integer(), Y::integer(), Data::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrldodroptext">external documentation</a>.
doDropText(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Data)
 when is_integer(X),is_integer(Y),is_list(Data) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  Data_UC = unicode:characters_to_binary([Data,0]),
  wxe_util:call(?wxStyledTextCtrl_DoDropText,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,(byte_size(Data_UC)):32/?UI,(Data_UC)/binary, 0:(((8- ((0+byte_size(Data_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxStyledTextCtrl()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetuseantialiasing">external documentation</a>.
getUseAntiAliasing(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetUseAntiAliasing,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::binary()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrladdtextraw">external documentation</a>.
addTextRaw(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:send_bin(Text),
  wxe_util:cast(?wxStyledTextCtrl_AddTextRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Pos::integer(), Text::binary()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlinserttextraw">external documentation</a>.
insertTextRaw(#wx_ref{type=ThisT,ref=ThisRef},Pos,Text)
 when is_integer(Pos),is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:send_bin(Text),
  wxe_util:cast(?wxStyledTextCtrl_InsertTextRaw,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> {binary(),LinePos::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetcurlineraw">external documentation</a>.
getCurLineRaw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetCurLineRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Line::integer()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetlineraw">external documentation</a>.
getLineRaw(#wx_ref{type=ThisT,ref=ThisRef},Line)
 when is_integer(Line) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetLineRaw,
  <<ThisRef:32/?UI,Line:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgetselectedtextraw">external documentation</a>.
getSelectedTextRaw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetSelectedTextRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), StartPos::integer(), EndPos::integer()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextrangeraw">external documentation</a>.
getTextRangeRaw(#wx_ref{type=ThisT,ref=ThisRef},StartPos,EndPos)
 when is_integer(StartPos),is_integer(EndPos) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTextRangeRaw,
  <<ThisRef:32/?UI,StartPos:32/?UI,EndPos:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::binary()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlsettextraw">external documentation</a>.
setTextRaw(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:send_bin(Text),
  wxe_util:cast(?wxStyledTextCtrl_SetTextRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> binary()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlgettextraw">external documentation</a>.
getTextRaw(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:call(?wxStyledTextCtrl_GetTextRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl(), Text::binary()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstyledtextctrl.html#wxstyledtextctrlappendtextraw">external documentation</a>.
appendTextRaw(#wx_ref{type=ThisT,ref=ThisRef},Text)
 when is_binary(Text) ->
  ?CLASS(ThisT,wxStyledTextCtrl),
  wxe_util:send_bin(Text),
  wxe_util:cast(?wxStyledTextCtrl_AppendTextRaw,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStyledTextCtrl()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStyledTextCtrl),
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
