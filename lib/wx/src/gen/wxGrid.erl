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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html">wxGrid</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxScrolledWindow}
%% <br />{@link wxPanel}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxGrid().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGrid).
-include("wxe.hrl").
-export([appendCols/1,appendCols/2,appendRows/1,appendRows/2,autoSize/1,autoSizeColumn/2,
  autoSizeColumn/3,autoSizeColumns/1,autoSizeColumns/2,autoSizeRow/2,
  autoSizeRow/3,autoSizeRows/1,autoSizeRows/2,beginBatch/1,blockToDeviceRect/3,
  canDragColSize/1,canDragGridSize/1,canDragRowSize/1,canEnableCellControl/1,
  cellToRect/2,cellToRect/3,clearGrid/1,clearSelection/1,createGrid/3,
  createGrid/4,deleteCols/1,deleteCols/2,deleteRows/1,deleteRows/2,destroy/1,
  disableCellEditControl/1,disableDragColSize/1,disableDragGridSize/1,
  disableDragRowSize/1,enableCellEditControl/1,enableCellEditControl/2,
  enableDragColSize/1,enableDragColSize/2,enableDragGridSize/1,enableDragGridSize/2,
  enableDragRowSize/1,enableDragRowSize/2,enableEditing/2,enableGridLines/1,
  enableGridLines/2,endBatch/1,fit/1,forceRefresh/1,getBatchCount/1,
  getCellAlignment/3,getCellBackgroundColour/3,getCellEditor/3,getCellFont/3,
  getCellRenderer/3,getCellTextColour/3,getCellValue/2,getCellValue/3,
  getColLabelAlignment/1,getColLabelSize/1,getColLabelValue/2,getColMinimalAcceptableWidth/1,
  getDefaultCellAlignment/1,getDefaultCellBackgroundColour/1,getDefaultCellFont/1,
  getDefaultCellTextColour/1,getDefaultColLabelSize/1,getDefaultColSize/1,
  getDefaultEditor/1,getDefaultEditorForCell/2,getDefaultEditorForCell/3,
  getDefaultEditorForType/2,getDefaultRenderer/1,getDefaultRendererForCell/3,
  getDefaultRendererForType/2,getDefaultRowLabelSize/1,getDefaultRowSize/1,
  getGridColLabelWindow/1,getGridCornerLabelWindow/1,getGridCursorCol/1,
  getGridCursorRow/1,getGridLineColour/1,getGridRowLabelWindow/1,getGridWindow/1,
  getLabelBackgroundColour/1,getLabelFont/1,getLabelTextColour/1,getNumberCols/1,
  getNumberRows/1,getOrCreateCellAttr/3,getRowLabelAlignment/1,getRowLabelSize/1,
  getRowLabelValue/2,getRowMinimalAcceptableHeight/1,getRowSize/2,
  getScrollLineX/1,getScrollLineY/1,getSelectedCells/1,getSelectedCols/1,
  getSelectedRows/1,getSelectionBackground/1,getSelectionBlockBottomRight/1,
  getSelectionBlockTopLeft/1,getSelectionForeground/1,getViewWidth/1,
  gridLinesEnabled/1,hideCellEditControl/1,insertCols/1,insertCols/2,
  insertRows/1,insertRows/2,isCellEditControlEnabled/1,isCurrentCellReadOnly/1,
  isEditable/1,isInSelection/2,isInSelection/3,isReadOnly/3,isSelection/1,
  isVisible/2,isVisible/3,isVisible/4,makeCellVisible/2,makeCellVisible/3,
  moveCursorDown/2,moveCursorDownBlock/2,moveCursorLeft/2,moveCursorLeftBlock/2,
  moveCursorRight/2,moveCursorRightBlock/2,moveCursorUp/2,moveCursorUpBlock/2,
  movePageDown/1,movePageUp/1,new/0,new/2,new/3,new/4,registerDataType/4,
  saveEditControlValue/1,selectAll/1,selectBlock/3,selectBlock/4,selectBlock/5,
  selectBlock/6,selectCol/2,selectCol/3,selectRow/2,selectRow/3,setCellAlignment/2,
  setCellAlignment/4,setCellAlignment/5,setCellBackgroundColour/2,
  setCellBackgroundColour/4,setCellEditor/4,setCellFont/4,setCellRenderer/4,
  setCellTextColour/2,setCellTextColour/4,setCellValue/3,setCellValue/4,
  setColAttr/3,setColFormatBool/2,setColFormatCustom/3,setColFormatFloat/2,
  setColFormatFloat/3,setColFormatNumber/2,setColLabelAlignment/3,
  setColLabelSize/2,setColLabelValue/3,setColMinimalAcceptableWidth/2,
  setColMinimalWidth/3,setColSize/3,setDefaultCellAlignment/3,setDefaultCellBackgroundColour/2,
  setDefaultCellFont/2,setDefaultCellTextColour/2,setDefaultColSize/2,
  setDefaultColSize/3,setDefaultEditor/2,setDefaultRenderer/2,setDefaultRowSize/2,
  setDefaultRowSize/3,setGridCursor/3,setGridLineColour/2,setLabelBackgroundColour/2,
  setLabelFont/2,setLabelTextColour/2,setMargins/3,setReadOnly/3,setReadOnly/4,
  setRowAttr/3,setRowLabelAlignment/3,setRowLabelSize/2,setRowLabelValue/3,
  setRowMinimalAcceptableHeight/2,setRowMinimalHeight/3,setRowSize/3,
  setScrollLineX/2,setScrollLineY/2,setSelectionBackground/2,setSelectionForeground/2,
  setSelectionMode/2,showCellEditControl/1,xToCol/2,xToCol/3,xToEdgeOfCol/2,
  yToEdgeOfRow/2,yToRow/2]).

%% inherited exports
-export([cacheBestSize/2,calcScrolledPosition/2,calcScrolledPosition/3,calcUnscrolledPosition/2,
  calcUnscrolledPosition/3,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  doPrepareDC/2,enable/1,enable/2,enableScrolling/3,findWindow/2,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getCursor/1,getDropTarget/1,
  getEventHandler/1,getExtraStyle/1,getFont/1,getForegroundColour/1,
  getGrandParent/1,getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,
  getMinSize/1,getName/1,getParent/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPixelsPerUnit/1,getScrollPos/2,getScrollRange/2,
  getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,getTextExtent/3,
  getToolTip/1,getUpdateRegion/1,getViewStart/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,
  isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,makeModal/1,makeModal/2,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popEventHandler/1,
  popEventHandler/2,popupMenu/2,popupMenu/3,popupMenu/4,prepareDC/2,
  raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,
  removeChild/2,reparent/2,screenToClient/1,screenToClient/2,scroll/3,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFocusIgnoringChildren/1,setFont/2,setForegroundColour/2,setHelpText/2,
  setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollRate/3,setScrollbar/5,setScrollbar/6,setScrollbars/5,
  setScrollbars/6,setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,
  setSizeHints/3,setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,
  setSizerAndFit/3,setTargetWindow/2,setThemeEnabled/2,setToolTip/2,
  setTransparent/2,setVirtualSize/2,setVirtualSize/3,setVirtualSizeHints/2,
  setVirtualSizeHints/3,setVirtualSizeHints/4,setWindowStyle/2,setWindowStyleFlag/2,
  setWindowVariant/2,shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-export_type([wxGrid/0]).
%% @hidden
parent_class(wxScrolledWindow) -> true;
parent_class(wxPanel) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGrid() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridwxgrid">external documentation</a>.
-spec new() -> wxGrid().
new() ->
  wxe_util:construct(?wxGrid_new_0,
  <<>>).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxGrid() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridwxgrid">external documentation</a>.
%% <br /> Also:<br />
%% new(Parent, Id, [Option]) -> wxGrid() when<br />
%% 	Parent::wxWindow:wxWindow(), Id::integer(),<br />
%% 	Option :: {'pos', {X::integer(), Y::integer()}}<br />
%% 		 | {'size', {W::integer(), H::integer()}}<br />
%% 		 | {'style', integer()}.<br />
%% 
-spec new(Parent, X, Y) -> wxGrid() when
	Parent::wxWindow:wxWindow(), X::integer(), Y::integer();
      (Parent, Id, [Option]) -> wxGrid() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.

new(Parent,X,Y)
 when is_record(Parent, wx_ref),is_integer(X),is_integer(Y) ->
  new(Parent,X,Y, []);
new(#wx_ref{type=ParentT,ref=ParentRef},Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {PosX,PosY}}, Acc) -> [<<1:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<2:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<3:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGrid_new_3,
  <<ParentRef:32/?UI,Id:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridwxgrid">external documentation</a>.
-spec new(Parent, X, Y, [Option]) -> wxGrid() when
	Parent::wxWindow:wxWindow(), X::integer(), Y::integer(),
	Option :: {'w', integer()}
		 | {'h', integer()}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT,ref=ParentRef},X,Y, Options)
 when is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({w, W}, Acc) -> [<<1:32/?UI,W:32/?UI>>|Acc];
          ({h, H}, Acc) -> [<<2:32/?UI,H:32/?UI>>|Acc];
          ({style, Style}, Acc) -> [<<3:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGrid_new_4,
  <<ParentRef:32/?UI,X:32/?UI,Y:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv appendCols(This, [])
-spec appendCols(This) -> boolean() when
	This::wxGrid().

appendCols(This)
 when is_record(This, wx_ref) ->
  appendCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridappendcols">external documentation</a>.
-spec appendCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'numCols', integer()}
		 | {'updateLabels', boolean()}.
appendCols(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({numCols, NumCols}, Acc) -> [<<1:32/?UI,NumCols:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_AppendCols,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv appendRows(This, [])
-spec appendRows(This) -> boolean() when
	This::wxGrid().

appendRows(This)
 when is_record(This, wx_ref) ->
  appendRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridappendrows">external documentation</a>.
-spec appendRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'numRows', integer()}
		 | {'updateLabels', boolean()}.
appendRows(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({numRows, NumRows}, Acc) -> [<<1:32/?UI,NumRows:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_AppendRows,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosize">external documentation</a>.
-spec autoSize(This) -> 'ok' when
	This::wxGrid().
autoSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_AutoSize,
  <<ThisRef:32/?UI>>).

%% @equiv autoSizeColumn(This,Col, [])
-spec autoSizeColumn(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

autoSizeColumn(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  autoSizeColumn(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizecolumn">external documentation</a>.
-spec autoSizeColumn(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'setAsMin', boolean()}.
autoSizeColumn(#wx_ref{type=ThisT,ref=ThisRef},Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, SetAsMin}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(SetAsMin)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_AutoSizeColumn,
  <<ThisRef:32/?UI,Col:32/?UI, BinOpt/binary>>).

%% @equiv autoSizeColumns(This, [])
-spec autoSizeColumns(This) -> 'ok' when
	This::wxGrid().

autoSizeColumns(This)
 when is_record(This, wx_ref) ->
  autoSizeColumns(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizecolumns">external documentation</a>.
-spec autoSizeColumns(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'setAsMin', boolean()}.
autoSizeColumns(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, SetAsMin}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(SetAsMin)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_AutoSizeColumns,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv autoSizeRow(This,Row, [])
-spec autoSizeRow(This, Row) -> 'ok' when
	This::wxGrid(), Row::integer().

autoSizeRow(This,Row)
 when is_record(This, wx_ref),is_integer(Row) ->
  autoSizeRow(This,Row, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizerow">external documentation</a>.
-spec autoSizeRow(This, Row, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(),
	Option :: {'setAsMin', boolean()}.
autoSizeRow(#wx_ref{type=ThisT,ref=ThisRef},Row, Options)
 when is_integer(Row),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, SetAsMin}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(SetAsMin)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_AutoSizeRow,
  <<ThisRef:32/?UI,Row:32/?UI, BinOpt/binary>>).

%% @equiv autoSizeRows(This, [])
-spec autoSizeRows(This) -> 'ok' when
	This::wxGrid().

autoSizeRows(This)
 when is_record(This, wx_ref) ->
  autoSizeRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizerows">external documentation</a>.
-spec autoSizeRows(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'setAsMin', boolean()}.
autoSizeRows(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, SetAsMin}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(SetAsMin)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_AutoSizeRows,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridbeginbatch">external documentation</a>.
-spec beginBatch(This) -> 'ok' when
	This::wxGrid().
beginBatch(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_BeginBatch,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridblocktodevicerect">external documentation</a>.
-spec blockToDeviceRect(This, TopLeft, BottomRight) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()}.
blockToDeviceRect(#wx_ref{type=ThisT,ref=ThisRef},{TopLeftR,TopLeftC},{BottomRightR,BottomRightC})
 when is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_BlockToDeviceRect,
  <<ThisRef:32/?UI,TopLeftR:32/?UI,TopLeftC:32/?UI,BottomRightR:32/?UI,BottomRightC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragcolsize">external documentation</a>.
-spec canDragColSize(This) -> boolean() when
	This::wxGrid().
canDragColSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CanDragColSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragrowsize">external documentation</a>.
-spec canDragRowSize(This) -> boolean() when
	This::wxGrid().
canDragRowSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CanDragRowSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandraggridsize">external documentation</a>.
-spec canDragGridSize(This) -> boolean() when
	This::wxGrid().
canDragGridSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CanDragGridSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcanenablecellcontrol">external documentation</a>.
-spec canEnableCellControl(This) -> boolean() when
	This::wxGrid().
canEnableCellControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CanEnableCellControl,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcelltorect">external documentation</a>.
-spec cellToRect(This, Coords) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
cellToRect(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC})
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CellToRect_1,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcelltorect">external documentation</a>.
-spec cellToRect(This, Row, Col) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), Row::integer(), Col::integer().
cellToRect(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_CellToRect_2,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcleargrid">external documentation</a>.
-spec clearGrid(This) -> 'ok' when
	This::wxGrid().
clearGrid(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_ClearGrid,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridclearselection">external documentation</a>.
-spec clearSelection(This) -> 'ok' when
	This::wxGrid().
clearSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_ClearSelection,
  <<ThisRef:32/?UI>>).

%% @equiv createGrid(This,NumRows,NumCols, [])
-spec createGrid(This, NumRows, NumCols) -> boolean() when
	This::wxGrid(), NumRows::integer(), NumCols::integer().

createGrid(This,NumRows,NumCols)
 when is_record(This, wx_ref),is_integer(NumRows),is_integer(NumCols) ->
  createGrid(This,NumRows,NumCols, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcreategrid">external documentation</a>.
%%<br /> Selmode = ?wxGrid_wxGridSelectCells | ?wxGrid_wxGridSelectRows | ?wxGrid_wxGridSelectColumns
-spec createGrid(This, NumRows, NumCols, [Option]) -> boolean() when
	This::wxGrid(), NumRows::integer(), NumCols::integer(),
	Option :: {'selmode', wx:wx_enum()}.
createGrid(#wx_ref{type=ThisT,ref=ThisRef},NumRows,NumCols, Options)
 when is_integer(NumRows),is_integer(NumCols),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({selmode, Selmode}, Acc) -> [<<1:32/?UI,Selmode:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_CreateGrid,
  <<ThisRef:32/?UI,NumRows:32/?UI,NumCols:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv deleteCols(This, [])
-spec deleteCols(This) -> boolean() when
	This::wxGrid().

deleteCols(This)
 when is_record(This, wx_ref) ->
  deleteCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddeletecols">external documentation</a>.
-spec deleteCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numCols', integer()}
		 | {'updateLabels', boolean()}.
deleteCols(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, Pos}, Acc) -> [<<1:32/?UI,Pos:32/?UI>>|Acc];
          ({numCols, NumCols}, Acc) -> [<<2:32/?UI,NumCols:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<3:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_DeleteCols,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv deleteRows(This, [])
-spec deleteRows(This) -> boolean() when
	This::wxGrid().

deleteRows(This)
 when is_record(This, wx_ref) ->
  deleteRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddeleterows">external documentation</a>.
-spec deleteRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numRows', integer()}
		 | {'updateLabels', boolean()}.
deleteRows(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, Pos}, Acc) -> [<<1:32/?UI,Pos:32/?UI>>|Acc];
          ({numRows, NumRows}, Acc) -> [<<2:32/?UI,NumRows:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<3:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_DeleteRows,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisablecelleditcontrol">external documentation</a>.
-spec disableCellEditControl(This) -> 'ok' when
	This::wxGrid().
disableCellEditControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_DisableCellEditControl,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledragcolsize">external documentation</a>.
-spec disableDragColSize(This) -> 'ok' when
	This::wxGrid().
disableDragColSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_DisableDragColSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledraggridsize">external documentation</a>.
-spec disableDragGridSize(This) -> 'ok' when
	This::wxGrid().
disableDragGridSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_DisableDragGridSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledragrowsize">external documentation</a>.
-spec disableDragRowSize(This) -> 'ok' when
	This::wxGrid().
disableDragRowSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_DisableDragRowSize,
  <<ThisRef:32/?UI>>).

%% @equiv enableCellEditControl(This, [])
-spec enableCellEditControl(This) -> 'ok' when
	This::wxGrid().

enableCellEditControl(This)
 when is_record(This, wx_ref) ->
  enableCellEditControl(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenablecelleditcontrol">external documentation</a>.
-spec enableCellEditControl(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableCellEditControl(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_EnableCellEditControl,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv enableDragColSize(This, [])
-spec enableDragColSize(This) -> 'ok' when
	This::wxGrid().

enableDragColSize(This)
 when is_record(This, wx_ref) ->
  enableDragColSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledragcolsize">external documentation</a>.
-spec enableDragColSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragColSize(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_EnableDragColSize,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv enableDragGridSize(This, [])
-spec enableDragGridSize(This) -> 'ok' when
	This::wxGrid().

enableDragGridSize(This)
 when is_record(This, wx_ref) ->
  enableDragGridSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledraggridsize">external documentation</a>.
-spec enableDragGridSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragGridSize(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_EnableDragGridSize,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv enableDragRowSize(This, [])
-spec enableDragRowSize(This) -> 'ok' when
	This::wxGrid().

enableDragRowSize(This)
 when is_record(This, wx_ref) ->
  enableDragRowSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledragrowsize">external documentation</a>.
-spec enableDragRowSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragRowSize(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_EnableDragRowSize,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenableediting">external documentation</a>.
-spec enableEditing(This, Edit) -> 'ok' when
	This::wxGrid(), Edit::boolean().
enableEditing(#wx_ref{type=ThisT,ref=ThisRef},Edit)
 when is_boolean(Edit) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_EnableEditing,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Edit)):32/?UI>>).

%% @equiv enableGridLines(This, [])
-spec enableGridLines(This) -> 'ok' when
	This::wxGrid().

enableGridLines(This)
 when is_record(This, wx_ref) ->
  enableGridLines(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenablegridlines">external documentation</a>.
-spec enableGridLines(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableGridLines(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_EnableGridLines,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridendbatch">external documentation</a>.
-spec endBatch(This) -> 'ok' when
	This::wxGrid().
endBatch(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_EndBatch,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridfit">external documentation</a>.
-spec fit(This) -> 'ok' when
	This::wxGrid().
fit(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_Fit,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridforcerefresh">external documentation</a>.
-spec forceRefresh(This) -> 'ok' when
	This::wxGrid().
forceRefresh(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_ForceRefresh,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetbatchcount">external documentation</a>.
-spec getBatchCount(This) -> integer() when
	This::wxGrid().
getBatchCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetBatchCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellalignment">external documentation</a>.
-spec getCellAlignment(This, Row, Col) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid(), Row::integer(), Col::integer().
getCellAlignment(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellAlignment,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellbackgroundcolour">external documentation</a>.
-spec getCellBackgroundColour(This, Row, Col) -> wx:wx_colour4() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellBackgroundColour,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcelleditor">external documentation</a>.
-spec getCellEditor(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellEditor(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellEditor,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellfont">external documentation</a>.
-spec getCellFont(This, Row, Col) -> wxFont:wxFont() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellFont(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellFont,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellrenderer">external documentation</a>.
-spec getCellRenderer(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellRenderer(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellRenderer,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcelltextcolour">external documentation</a>.
-spec getCellTextColour(This, Row, Col) -> wx:wx_colour4() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellTextColour(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellTextColour,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellvalue">external documentation</a>.
-spec getCellValue(This, Coords) -> unicode:charlist() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
getCellValue(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC})
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellValue_1,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellvalue">external documentation</a>.
-spec getCellValue(This, Row, Col) -> unicode:charlist() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellValue(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetCellValue_2,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelalignment">external documentation</a>.
-spec getColLabelAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getColLabelAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetColLabelAlignment,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelsize">external documentation</a>.
-spec getColLabelSize(This) -> integer() when
	This::wxGrid().
getColLabelSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetColLabelSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelvalue">external documentation</a>.
-spec getColLabelValue(This, Col) -> unicode:charlist() when
	This::wxGrid(), Col::integer().
getColLabelValue(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetColLabelValue,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcolminimalacceptablewidth">external documentation</a>.
-spec getColMinimalAcceptableWidth(This) -> integer() when
	This::wxGrid().
getColMinimalAcceptableWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetColMinimalAcceptableWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellalignment">external documentation</a>.
-spec getDefaultCellAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getDefaultCellAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultCellAlignment,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellbackgroundcolour">external documentation</a>.
-spec getDefaultCellBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getDefaultCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultCellBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellfont">external documentation</a>.
-spec getDefaultCellFont(This) -> wxFont:wxFont() when
	This::wxGrid().
getDefaultCellFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultCellFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcelltextcolour">external documentation</a>.
-spec getDefaultCellTextColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getDefaultCellTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultCellTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcollabelsize">external documentation</a>.
-spec getDefaultColLabelSize(This) -> integer() when
	This::wxGrid().
getDefaultColLabelSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultColLabelSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcolsize">external documentation</a>.
-spec getDefaultColSize(This) -> integer() when
	This::wxGrid().
getDefaultColSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultColSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditor">external documentation</a>.
-spec getDefaultEditor(This) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid().
getDefaultEditor(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultEditor,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorforcell">external documentation</a>.
-spec getDefaultEditorForCell(This, C) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), C::{R::integer(), C::integer()}.
getDefaultEditorForCell(#wx_ref{type=ThisT,ref=ThisRef},{CR,CC})
 when is_integer(CR),is_integer(CC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultEditorForCell_1,
  <<ThisRef:32/?UI,CR:32/?UI,CC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorforcell">external documentation</a>.
-spec getDefaultEditorForCell(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), Row::integer(), Col::integer().
getDefaultEditorForCell(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultEditorForCell_2,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorfortype">external documentation</a>.
-spec getDefaultEditorForType(This, TypeName) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), TypeName::unicode:chardata().
getDefaultEditorForType(#wx_ref{type=ThisT,ref=ThisRef},TypeName)
 when is_list(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary([TypeName,0]),
  wxe_util:call(?wxGrid_GetDefaultEditorForType,
  <<ThisRef:32/?UI,(byte_size(TypeName_UC)):32/?UI,(TypeName_UC)/binary, 0:(((8- ((0+byte_size(TypeName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrenderer">external documentation</a>.
-spec getDefaultRenderer(This) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid().
getDefaultRenderer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultRenderer,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrendererforcell">external documentation</a>.
-spec getDefaultRendererForCell(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), Row::integer(), Col::integer().
getDefaultRendererForCell(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultRendererForCell,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrendererfortype">external documentation</a>.
-spec getDefaultRendererForType(This, TypeName) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), TypeName::unicode:chardata().
getDefaultRendererForType(#wx_ref{type=ThisT,ref=ThisRef},TypeName)
 when is_list(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary([TypeName,0]),
  wxe_util:call(?wxGrid_GetDefaultRendererForType,
  <<ThisRef:32/?UI,(byte_size(TypeName_UC)):32/?UI,(TypeName_UC)/binary, 0:(((8- ((0+byte_size(TypeName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrowlabelsize">external documentation</a>.
-spec getDefaultRowLabelSize(This) -> integer() when
	This::wxGrid().
getDefaultRowLabelSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultRowLabelSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrowsize">external documentation</a>.
-spec getDefaultRowSize(This) -> integer() when
	This::wxGrid().
getDefaultRowSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetDefaultRowSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcursorcol">external documentation</a>.
-spec getGridCursorCol(This) -> integer() when
	This::wxGrid().
getGridCursorCol(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridCursorCol,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcursorrow">external documentation</a>.
-spec getGridCursorRow(This) -> integer() when
	This::wxGrid().
getGridCursorRow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridCursorRow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridlinecolour">external documentation</a>.
-spec getGridLineColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getGridLineColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridLineColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgridlinesenabled">external documentation</a>.
-spec gridLinesEnabled(This) -> boolean() when
	This::wxGrid().
gridLinesEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GridLinesEnabled,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabelbackgroundcolour">external documentation</a>.
-spec getLabelBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getLabelBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetLabelBackgroundColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabelfont">external documentation</a>.
-spec getLabelFont(This) -> wxFont:wxFont() when
	This::wxGrid().
getLabelFont(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetLabelFont,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabeltextcolour">external documentation</a>.
-spec getLabelTextColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getLabelTextColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetLabelTextColour,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetnumbercols">external documentation</a>.
-spec getNumberCols(This) -> integer() when
	This::wxGrid().
getNumberCols(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetNumberCols,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetnumberrows">external documentation</a>.
-spec getNumberRows(This) -> integer() when
	This::wxGrid().
getNumberRows(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetNumberRows,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetorcreatecellattr">external documentation</a>.
-spec getOrCreateCellAttr(This, Row, Col) -> wxGridCellAttr:wxGridCellAttr() when
	This::wxGrid(), Row::integer(), Col::integer().
getOrCreateCellAttr(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetOrCreateCellAttr,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowminimalacceptableheight">external documentation</a>.
-spec getRowMinimalAcceptableHeight(This) -> integer() when
	This::wxGrid().
getRowMinimalAcceptableHeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetRowMinimalAcceptableHeight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelalignment">external documentation</a>.
-spec getRowLabelAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getRowLabelAlignment(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetRowLabelAlignment,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelsize">external documentation</a>.
-spec getRowLabelSize(This) -> integer() when
	This::wxGrid().
getRowLabelSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetRowLabelSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelvalue">external documentation</a>.
-spec getRowLabelValue(This, Row) -> unicode:charlist() when
	This::wxGrid(), Row::integer().
getRowLabelValue(#wx_ref{type=ThisT,ref=ThisRef},Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetRowLabelValue,
  <<ThisRef:32/?UI,Row:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowsize">external documentation</a>.
-spec getRowSize(This, Row) -> integer() when
	This::wxGrid(), Row::integer().
getRowSize(#wx_ref{type=ThisT,ref=ThisRef},Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetRowSize,
  <<ThisRef:32/?UI,Row:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetscrolllinex">external documentation</a>.
-spec getScrollLineX(This) -> integer() when
	This::wxGrid().
getScrollLineX(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetScrollLineX,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetscrollliney">external documentation</a>.
-spec getScrollLineY(This) -> integer() when
	This::wxGrid().
getScrollLineY(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetScrollLineY,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedcells">external documentation</a>.
-spec getSelectedCells(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectedCells(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectedCells,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedcols">external documentation</a>.
-spec getSelectedCols(This) -> [integer()] when
	This::wxGrid().
getSelectedCols(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectedCols,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedrows">external documentation</a>.
-spec getSelectedRows(This) -> [integer()] when
	This::wxGrid().
getSelectedRows(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectedRows,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionbackground">external documentation</a>.
-spec getSelectionBackground(This) -> wx:wx_colour4() when
	This::wxGrid().
getSelectionBackground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectionBackground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionblocktopleft">external documentation</a>.
-spec getSelectionBlockTopLeft(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectionBlockTopLeft(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectionBlockTopLeft,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionblockbottomright">external documentation</a>.
-spec getSelectionBlockBottomRight(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectionBlockBottomRight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectionBlockBottomRight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionforeground">external documentation</a>.
-spec getSelectionForeground(This) -> wx:wx_colour4() when
	This::wxGrid().
getSelectionForeground(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetSelectionForeground,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetviewwidth">external documentation</a>.
-spec getViewWidth(This) -> integer() when
	This::wxGrid().
getViewWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetViewWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridwindow">external documentation</a>.
-spec getGridWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridrowlabelwindow">external documentation</a>.
-spec getGridRowLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridRowLabelWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridRowLabelWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcollabelwindow">external documentation</a>.
-spec getGridColLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridColLabelWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridColLabelWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcornerlabelwindow">external documentation</a>.
-spec getGridCornerLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridCornerLabelWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_GetGridCornerLabelWindow,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridhidecelleditcontrol">external documentation</a>.
-spec hideCellEditControl(This) -> 'ok' when
	This::wxGrid().
hideCellEditControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_HideCellEditControl,
  <<ThisRef:32/?UI>>).

%% @equiv insertCols(This, [])
-spec insertCols(This) -> boolean() when
	This::wxGrid().

insertCols(This)
 when is_record(This, wx_ref) ->
  insertCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridinsertcols">external documentation</a>.
-spec insertCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numCols', integer()}
		 | {'updateLabels', boolean()}.
insertCols(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, Pos}, Acc) -> [<<1:32/?UI,Pos:32/?UI>>|Acc];
          ({numCols, NumCols}, Acc) -> [<<2:32/?UI,NumCols:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<3:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_InsertCols,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv insertRows(This, [])
-spec insertRows(This) -> boolean() when
	This::wxGrid().

insertRows(This)
 when is_record(This, wx_ref) ->
  insertRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridinsertrows">external documentation</a>.
-spec insertRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numRows', integer()}
		 | {'updateLabels', boolean()}.
insertRows(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, Pos}, Acc) -> [<<1:32/?UI,Pos:32/?UI>>|Acc];
          ({numRows, NumRows}, Acc) -> [<<2:32/?UI,NumRows:32/?UI>>|Acc];
          ({updateLabels, UpdateLabels}, Acc) -> [<<3:32/?UI,(wxe_util:from_bool(UpdateLabels)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_InsertRows,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiscelleditcontrolenabled">external documentation</a>.
-spec isCellEditControlEnabled(This) -> boolean() when
	This::wxGrid().
isCellEditControlEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsCellEditControlEnabled,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiscurrentcellreadonly">external documentation</a>.
-spec isCurrentCellReadOnly(This) -> boolean() when
	This::wxGrid().
isCurrentCellReadOnly(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsCurrentCellReadOnly,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiseditable">external documentation</a>.
-spec isEditable(This) -> boolean() when
	This::wxGrid().
isEditable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsEditable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisinselection">external documentation</a>.
-spec isInSelection(This, Coords) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
isInSelection(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC})
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsInSelection_1,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisinselection">external documentation</a>.
-spec isInSelection(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer().
isInSelection(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsInSelection_2,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisreadonly">external documentation</a>.
-spec isReadOnly(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer().
isReadOnly(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsReadOnly,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisselection">external documentation</a>.
-spec isSelection(This) -> boolean() when
	This::wxGrid().
isSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_IsSelection,
  <<ThisRef:32/?UI>>).

%% @equiv isVisible(This,Coords, [])
-spec isVisible(This, Coords) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.

isVisible(This,Coords={CoordsR,CoordsC})
 when is_record(This, wx_ref),is_integer(CoordsR),is_integer(CoordsC) ->
  isVisible(This,Coords, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisvisible">external documentation</a>.
%% <br /> Also:<br />
%% isVisible(This, Coords, [Option]) -> boolean() when<br />
%% 	This::wxGrid(), Coords::{R::integer(), C::integer()},<br />
%% 	Option :: {'wholeCellVisible', boolean()}.<br />
%% 
-spec isVisible(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer();
      (This, Coords, [Option]) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()},
	Option :: {'wholeCellVisible', boolean()}.

isVisible(This,Row,Col)
 when is_record(This, wx_ref),is_integer(Row),is_integer(Col) ->
  isVisible(This,Row,Col, []);
isVisible(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC}, Options)
 when is_integer(CoordsR),is_integer(CoordsC),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({wholeCellVisible, WholeCellVisible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(WholeCellVisible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_IsVisible_2,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisvisible">external documentation</a>.
-spec isVisible(This, Row, Col, [Option]) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer(),
	Option :: {'wholeCellVisible', boolean()}.
isVisible(#wx_ref{type=ThisT,ref=ThisRef},Row,Col, Options)
 when is_integer(Row),is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({wholeCellVisible, WholeCellVisible}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(WholeCellVisible)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_IsVisible_3,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmakecellvisible">external documentation</a>.
-spec makeCellVisible(This, Coords) -> 'ok' when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
makeCellVisible(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC})
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_MakeCellVisible_1,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmakecellvisible">external documentation</a>.
-spec makeCellVisible(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().
makeCellVisible(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_MakeCellVisible_2,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursordown">external documentation</a>.
-spec moveCursorDown(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorDown(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorDown,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorleft">external documentation</a>.
-spec moveCursorLeft(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorLeft(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorLeft,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorright">external documentation</a>.
-spec moveCursorRight(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorRight(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorRight,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorup">external documentation</a>.
-spec moveCursorUp(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorUp(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorUp,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursordownblock">external documentation</a>.
-spec moveCursorDownBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorDownBlock(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorDownBlock,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorleftblock">external documentation</a>.
-spec moveCursorLeftBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorLeftBlock(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorLeftBlock,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorrightblock">external documentation</a>.
-spec moveCursorRightBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorRightBlock(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorRightBlock,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorupblock">external documentation</a>.
-spec moveCursorUpBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorUpBlock(#wx_ref{type=ThisT,ref=ThisRef},ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MoveCursorUpBlock,
  <<ThisRef:32/?UI,(wxe_util:from_bool(ExpandSelection)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovepagedown">external documentation</a>.
-spec movePageDown(This) -> boolean() when
	This::wxGrid().
movePageDown(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MovePageDown,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovepageup">external documentation</a>.
-spec movePageUp(This) -> boolean() when
	This::wxGrid().
movePageUp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_MovePageUp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridregisterdatatype">external documentation</a>.
-spec registerDataType(This, TypeName, Renderer, Editor) -> 'ok' when
	This::wxGrid(), TypeName::unicode:chardata(), Renderer::wxGridCellRenderer:wxGridCellRenderer(), Editor::wxGridCellEditor:wxGridCellEditor().
registerDataType(#wx_ref{type=ThisT,ref=ThisRef},TypeName,#wx_ref{type=RendererT,ref=RendererRef},#wx_ref{type=EditorT,ref=EditorRef})
 when is_list(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary([TypeName,0]),
  ?CLASS(RendererT,wxGridCellRenderer),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:cast(?wxGrid_RegisterDataType,
  <<ThisRef:32/?UI,(byte_size(TypeName_UC)):32/?UI,(TypeName_UC)/binary, 0:(((8- ((0+byte_size(TypeName_UC)) band 16#7)) band 16#7))/unit:8,RendererRef:32/?UI,EditorRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsaveeditcontrolvalue">external documentation</a>.
-spec saveEditControlValue(This) -> 'ok' when
	This::wxGrid().
saveEditControlValue(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SaveEditControlValue,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectall">external documentation</a>.
-spec selectAll(This) -> 'ok' when
	This::wxGrid().
selectAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SelectAll,
  <<ThisRef:32/?UI>>).

%% @equiv selectBlock(This,TopLeft,BottomRight, [])
-spec selectBlock(This, TopLeft, BottomRight) -> 'ok' when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()}.

selectBlock(This,TopLeft={TopLeftR,TopLeftC},BottomRight={BottomRightR,BottomRightC})
 when is_record(This, wx_ref),is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC) ->
  selectBlock(This,TopLeft,BottomRight, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectblock">external documentation</a>.
-spec selectBlock(This, TopLeft, BottomRight, [Option]) -> 'ok' when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()},
	Option :: {'addToSelected', boolean()}.
selectBlock(#wx_ref{type=ThisT,ref=ThisRef},{TopLeftR,TopLeftC},{BottomRightR,BottomRightC}, Options)
 when is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, AddToSelected}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(AddToSelected)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SelectBlock_3,
  <<ThisRef:32/?UI,TopLeftR:32/?UI,TopLeftC:32/?UI,BottomRightR:32/?UI,BottomRightC:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv selectBlock(This,TopRow,LeftCol,BottomRow,RightCol, [])
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol) -> 'ok' when
	This::wxGrid(), TopRow::integer(), LeftCol::integer(), BottomRow::integer(), RightCol::integer().

selectBlock(This,TopRow,LeftCol,BottomRow,RightCol)
 when is_record(This, wx_ref),is_integer(TopRow),is_integer(LeftCol),is_integer(BottomRow),is_integer(RightCol) ->
  selectBlock(This,TopRow,LeftCol,BottomRow,RightCol, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectblock">external documentation</a>.
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol, [Option]) -> 'ok' when
	This::wxGrid(), TopRow::integer(), LeftCol::integer(), BottomRow::integer(), RightCol::integer(),
	Option :: {'addToSelected', boolean()}.
selectBlock(#wx_ref{type=ThisT,ref=ThisRef},TopRow,LeftCol,BottomRow,RightCol, Options)
 when is_integer(TopRow),is_integer(LeftCol),is_integer(BottomRow),is_integer(RightCol),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, AddToSelected}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(AddToSelected)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SelectBlock_5,
  <<ThisRef:32/?UI,TopRow:32/?UI,LeftCol:32/?UI,BottomRow:32/?UI,RightCol:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv selectCol(This,Col, [])
-spec selectCol(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

selectCol(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  selectCol(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectcol">external documentation</a>.
-spec selectCol(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'addToSelected', boolean()}.
selectCol(#wx_ref{type=ThisT,ref=ThisRef},Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, AddToSelected}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(AddToSelected)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SelectCol,
  <<ThisRef:32/?UI,Col:32/?UI, BinOpt/binary>>).

%% @equiv selectRow(This,Row, [])
-spec selectRow(This, Row) -> 'ok' when
	This::wxGrid(), Row::integer().

selectRow(This,Row)
 when is_record(This, wx_ref),is_integer(Row) ->
  selectRow(This,Row, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectrow">external documentation</a>.
-spec selectRow(This, Row, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(),
	Option :: {'addToSelected', boolean()}.
selectRow(#wx_ref{type=ThisT,ref=ThisRef},Row, Options)
 when is_integer(Row),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, AddToSelected}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(AddToSelected)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SelectRow,
  <<ThisRef:32/?UI,Row:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellalignment">external documentation</a>.
-spec setCellAlignment(This, Align) -> 'ok' when
	This::wxGrid(), Align::integer().
setCellAlignment(#wx_ref{type=ThisT,ref=ThisRef},Align)
 when is_integer(Align) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellAlignment_1,
  <<ThisRef:32/?UI,Align:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellalignment">external documentation</a>.
-spec setCellAlignment(This, Align, Row, Col) -> 'ok' when
	This::wxGrid(), Align::integer(), Row::integer(), Col::integer().
setCellAlignment(#wx_ref{type=ThisT,ref=ThisRef},Align,Row,Col)
 when is_integer(Align),is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellAlignment_3,
  <<ThisRef:32/?UI,Align:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellalignment">external documentation</a>.
-spec setCellAlignment(This, Row, Col, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Horiz::integer(), Vert::integer().
setCellAlignment(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,Horiz,Vert)
 when is_integer(Row),is_integer(Col),is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellAlignment_4,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,Horiz:32/?UI,Vert:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellbackgroundcolour">external documentation</a>.
-spec setCellBackgroundColour(This, Col) -> 'ok' when
	This::wxGrid(), Col::wx:wx_colour().
setCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellBackgroundColour_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellbackgroundcolour">external documentation</a>.
%% <br /> Also:<br />
%% setCellBackgroundColour(This, Colour, Row, Col) -> 'ok' when<br />
%% 	This::wxGrid(), Colour::wx:wx_colour(), Row::integer(), Col::integer().<br />
%% 
-spec setCellBackgroundColour(This, Row, Col, Val) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Val::wx:wx_colour();
      (This, Colour, Row, Col) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour(), Row::integer(), Col::integer().
setCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,Val)
 when is_integer(Row),is_integer(Col),tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellBackgroundColour_3_0,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>);
setCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Colour,Row,Col)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4,is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellBackgroundColour_3_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcelleditor">external documentation</a>.
-spec setCellEditor(This, Row, Col, Editor) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Editor::wxGridCellEditor:wxGridCellEditor().
setCellEditor(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=EditorT,ref=EditorRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:cast(?wxGrid_SetCellEditor,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,EditorRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellfont">external documentation</a>.
-spec setCellFont(This, Row, Col, Val) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Val::wxFont:wxFont().
setCellFont(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=ValT,ref=ValRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(ValT,wxFont),
  wxe_util:cast(?wxGrid_SetCellFont,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,ValRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellrenderer">external documentation</a>.
-spec setCellRenderer(This, Row, Col, Renderer) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setCellRenderer(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,#wx_ref{type=RendererT,ref=RendererRef})
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:cast(?wxGrid_SetCellRenderer,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,RendererRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcelltextcolour">external documentation</a>.
-spec setCellTextColour(This, Col) -> 'ok' when
	This::wxGrid(), Col::wx:wx_colour().
setCellTextColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellTextColour_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Col)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcelltextcolour">external documentation</a>.
%% <br /> Also:<br />
%% setCellTextColour(This, Val, Row, Col) -> 'ok' when<br />
%% 	This::wxGrid(), Val::wx:wx_colour(), Row::integer(), Col::integer().<br />
%% 
-spec setCellTextColour(This, Row, Col, Val) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Val::wx:wx_colour();
      (This, Val, Row, Col) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour(), Row::integer(), Col::integer().
setCellTextColour(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,Val)
 when is_integer(Row),is_integer(Col),tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellTextColour_3_0,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>);
setCellTextColour(#wx_ref{type=ThisT,ref=ThisRef},Val,Row,Col)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4,is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetCellTextColour_3_1,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellvalue">external documentation</a>.
-spec setCellValue(This, Coords, S) -> 'ok' when
	This::wxGrid(), Coords::{R::integer(), C::integer()}, S::unicode:chardata().
setCellValue(#wx_ref{type=ThisT,ref=ThisRef},{CoordsR,CoordsC},S)
 when is_integer(CoordsR),is_integer(CoordsC),is_list(S) ->
  ?CLASS(ThisT,wxGrid),
  S_UC = unicode:characters_to_binary([S,0]),
  wxe_util:cast(?wxGrid_SetCellValue_2,
  <<ThisRef:32/?UI,CoordsR:32/?UI,CoordsC:32/?UI,(byte_size(S_UC)):32/?UI,(S_UC)/binary, 0:(((8- ((0+byte_size(S_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellvalue">external documentation</a>.
%% <br /> Also:<br />
%% setCellValue(This, Val, Row, Col) -> 'ok' when<br />
%% 	This::wxGrid(), Val::unicode:chardata(), Row::integer(), Col::integer().<br />
%% 
-spec setCellValue(This, Row, Col, S) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), S::unicode:chardata();
      (This, Val, Row, Col) -> 'ok' when
	This::wxGrid(), Val::unicode:chardata(), Row::integer(), Col::integer().
setCellValue(#wx_ref{type=ThisT,ref=ThisRef},Row,Col,S)
 when is_integer(Row),is_integer(Col),is_list(S) ->
  ?CLASS(ThisT,wxGrid),
  S_UC = unicode:characters_to_binary([S,0]),
  wxe_util:cast(?wxGrid_SetCellValue_3_0,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI,(byte_size(S_UC)):32/?UI,(S_UC)/binary, 0:(((8- ((0+byte_size(S_UC)) band 16#7)) band 16#7))/unit:8>>);
setCellValue(#wx_ref{type=ThisT,ref=ThisRef},Val,Row,Col)
 when is_list(Val),is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  Val_UC = unicode:characters_to_binary([Val,0]),
  wxe_util:cast(?wxGrid_SetCellValue_3_1,
  <<ThisRef:32/?UI,(byte_size(Val_UC)):32/?UI,(Val_UC)/binary, 0:(((8- ((0+byte_size(Val_UC)) band 16#7)) band 16#7))/unit:8,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolattr">external documentation</a>.
-spec setColAttr(This, Col, Attr) -> 'ok' when
	This::wxGrid(), Col::integer(), Attr::wxGridCellAttr:wxGridCellAttr().
setColAttr(#wx_ref{type=ThisT,ref=ThisRef},Col,#wx_ref{type=AttrT,ref=AttrRef})
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:cast(?wxGrid_SetColAttr,
  <<ThisRef:32/?UI,Col:32/?UI,AttrRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatbool">external documentation</a>.
-spec setColFormatBool(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().
setColFormatBool(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColFormatBool,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatnumber">external documentation</a>.
-spec setColFormatNumber(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().
setColFormatNumber(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColFormatNumber,
  <<ThisRef:32/?UI,Col:32/?UI>>).

%% @equiv setColFormatFloat(This,Col, [])
-spec setColFormatFloat(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

setColFormatFloat(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  setColFormatFloat(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatfloat">external documentation</a>.
-spec setColFormatFloat(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'width', integer()}
		 | {'precision', integer()}.
setColFormatFloat(#wx_ref{type=ThisT,ref=ThisRef},Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({width, Width}, Acc) -> [<<1:32/?UI,Width:32/?UI>>|Acc];
          ({precision, Precision}, Acc) -> [<<2:32/?UI,Precision:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SetColFormatFloat,
  <<ThisRef:32/?UI,Col:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatcustom">external documentation</a>.
-spec setColFormatCustom(This, Col, TypeName) -> 'ok' when
	This::wxGrid(), Col::integer(), TypeName::unicode:chardata().
setColFormatCustom(#wx_ref{type=ThisT,ref=ThisRef},Col,TypeName)
 when is_integer(Col),is_list(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary([TypeName,0]),
  wxe_util:cast(?wxGrid_SetColFormatCustom,
  <<ThisRef:32/?UI,Col:32/?UI,(byte_size(TypeName_UC)):32/?UI,(TypeName_UC)/binary, 0:(((8- ((4+byte_size(TypeName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelalignment">external documentation</a>.
-spec setColLabelAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setColLabelAlignment(#wx_ref{type=ThisT,ref=ThisRef},Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColLabelAlignment,
  <<ThisRef:32/?UI,Horiz:32/?UI,Vert:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelsize">external documentation</a>.
-spec setColLabelSize(This, Height) -> 'ok' when
	This::wxGrid(), Height::integer().
setColLabelSize(#wx_ref{type=ThisT,ref=ThisRef},Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColLabelSize,
  <<ThisRef:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelvalue">external documentation</a>.
-spec setColLabelValue(This, Col, Val) -> 'ok' when
	This::wxGrid(), Col::integer(), Val::unicode:chardata().
setColLabelValue(#wx_ref{type=ThisT,ref=ThisRef},Col,Val)
 when is_integer(Col),is_list(Val) ->
  ?CLASS(ThisT,wxGrid),
  Val_UC = unicode:characters_to_binary([Val,0]),
  wxe_util:cast(?wxGrid_SetColLabelValue,
  <<ThisRef:32/?UI,Col:32/?UI,(byte_size(Val_UC)):32/?UI,(Val_UC)/binary, 0:(((8- ((4+byte_size(Val_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolminimalwidth">external documentation</a>.
-spec setColMinimalWidth(This, Col, Width) -> 'ok' when
	This::wxGrid(), Col::integer(), Width::integer().
setColMinimalWidth(#wx_ref{type=ThisT,ref=ThisRef},Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColMinimalWidth,
  <<ThisRef:32/?UI,Col:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolminimalacceptablewidth">external documentation</a>.
-spec setColMinimalAcceptableWidth(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().
setColMinimalAcceptableWidth(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColMinimalAcceptableWidth,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolsize">external documentation</a>.
-spec setColSize(This, Col, Width) -> 'ok' when
	This::wxGrid(), Col::integer(), Width::integer().
setColSize(#wx_ref{type=ThisT,ref=ThisRef},Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetColSize,
  <<ThisRef:32/?UI,Col:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellalignment">external documentation</a>.
-spec setDefaultCellAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setDefaultCellAlignment(#wx_ref{type=ThisT,ref=ThisRef},Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetDefaultCellAlignment,
  <<ThisRef:32/?UI,Horiz:32/?UI,Vert:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellbackgroundcolour">external documentation</a>.
-spec setDefaultCellBackgroundColour(This, Val) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour().
setDefaultCellBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetDefaultCellBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellfont">external documentation</a>.
-spec setDefaultCellFont(This, Val) -> 'ok' when
	This::wxGrid(), Val::wxFont:wxFont().
setDefaultCellFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ValT,ref=ValRef}) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(ValT,wxFont),
  wxe_util:cast(?wxGrid_SetDefaultCellFont,
  <<ThisRef:32/?UI,ValRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcelltextcolour">external documentation</a>.
-spec setDefaultCellTextColour(This, Val) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour().
setDefaultCellTextColour(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetDefaultCellTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaulteditor">external documentation</a>.
-spec setDefaultEditor(This, Editor) -> 'ok' when
	This::wxGrid(), Editor::wxGridCellEditor:wxGridCellEditor().
setDefaultEditor(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=EditorT,ref=EditorRef}) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:cast(?wxGrid_SetDefaultEditor,
  <<ThisRef:32/?UI,EditorRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultrenderer">external documentation</a>.
-spec setDefaultRenderer(This, Renderer) -> 'ok' when
	This::wxGrid(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setDefaultRenderer(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=RendererT,ref=RendererRef}) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:cast(?wxGrid_SetDefaultRenderer,
  <<ThisRef:32/?UI,RendererRef:32/?UI>>).

%% @equiv setDefaultColSize(This,Width, [])
-spec setDefaultColSize(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().

setDefaultColSize(This,Width)
 when is_record(This, wx_ref),is_integer(Width) ->
  setDefaultColSize(This,Width, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcolsize">external documentation</a>.
-spec setDefaultColSize(This, Width, [Option]) -> 'ok' when
	This::wxGrid(), Width::integer(),
	Option :: {'resizeExistingCols', boolean()}.
setDefaultColSize(#wx_ref{type=ThisT,ref=ThisRef},Width, Options)
 when is_integer(Width),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({resizeExistingCols, ResizeExistingCols}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(ResizeExistingCols)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SetDefaultColSize,
  <<ThisRef:32/?UI,Width:32/?UI, BinOpt/binary>>).

%% @equiv setDefaultRowSize(This,Height, [])
-spec setDefaultRowSize(This, Height) -> 'ok' when
	This::wxGrid(), Height::integer().

setDefaultRowSize(This,Height)
 when is_record(This, wx_ref),is_integer(Height) ->
  setDefaultRowSize(This,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultrowsize">external documentation</a>.
-spec setDefaultRowSize(This, Height, [Option]) -> 'ok' when
	This::wxGrid(), Height::integer(),
	Option :: {'resizeExistingRows', boolean()}.
setDefaultRowSize(#wx_ref{type=ThisT,ref=ThisRef},Height, Options)
 when is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({resizeExistingRows, ResizeExistingRows}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(ResizeExistingRows)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SetDefaultRowSize,
  <<ThisRef:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetgridcursor">external documentation</a>.
-spec setGridCursor(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().
setGridCursor(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetGridCursor,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetgridlinecolour">external documentation</a>.
-spec setGridLineColour(This, Val) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour().
setGridLineColour(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetGridLineColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabelbackgroundcolour">external documentation</a>.
-spec setLabelBackgroundColour(This, Val) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour().
setLabelBackgroundColour(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetLabelBackgroundColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabelfont">external documentation</a>.
-spec setLabelFont(This, Val) -> 'ok' when
	This::wxGrid(), Val::wxFont:wxFont().
setLabelFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ValT,ref=ValRef}) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(ValT,wxFont),
  wxe_util:cast(?wxGrid_SetLabelFont,
  <<ThisRef:32/?UI,ValRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabeltextcolour">external documentation</a>.
-spec setLabelTextColour(This, Val) -> 'ok' when
	This::wxGrid(), Val::wx:wx_colour().
setLabelTextColour(#wx_ref{type=ThisT,ref=ThisRef},Val)
 when tuple_size(Val) =:= 3; tuple_size(Val) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetLabelTextColour,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(Val)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetmargins">external documentation</a>.
-spec setMargins(This, ExtraWidth, ExtraHeight) -> 'ok' when
	This::wxGrid(), ExtraWidth::integer(), ExtraHeight::integer().
setMargins(#wx_ref{type=ThisT,ref=ThisRef},ExtraWidth,ExtraHeight)
 when is_integer(ExtraWidth),is_integer(ExtraHeight) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetMargins,
  <<ThisRef:32/?UI,ExtraWidth:32/?UI,ExtraHeight:32/?UI>>).

%% @equiv setReadOnly(This,Row,Col, [])
-spec setReadOnly(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().

setReadOnly(This,Row,Col)
 when is_record(This, wx_ref),is_integer(Row),is_integer(Col) ->
  setReadOnly(This,Row,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetreadonly">external documentation</a>.
-spec setReadOnly(This, Row, Col, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(),
	Option :: {'isReadOnly', boolean()}.
setReadOnly(#wx_ref{type=ThisT,ref=ThisRef},Row,Col, Options)
 when is_integer(Row),is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({isReadOnly, IsReadOnly}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(IsReadOnly)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGrid_SetReadOnly,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowattr">external documentation</a>.
-spec setRowAttr(This, Row, Attr) -> 'ok' when
	This::wxGrid(), Row::integer(), Attr::wxGridCellAttr:wxGridCellAttr().
setRowAttr(#wx_ref{type=ThisT,ref=ThisRef},Row,#wx_ref{type=AttrT,ref=AttrRef})
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:cast(?wxGrid_SetRowAttr,
  <<ThisRef:32/?UI,Row:32/?UI,AttrRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelalignment">external documentation</a>.
-spec setRowLabelAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setRowLabelAlignment(#wx_ref{type=ThisT,ref=ThisRef},Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetRowLabelAlignment,
  <<ThisRef:32/?UI,Horiz:32/?UI,Vert:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelsize">external documentation</a>.
-spec setRowLabelSize(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().
setRowLabelSize(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetRowLabelSize,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelvalue">external documentation</a>.
-spec setRowLabelValue(This, Row, Val) -> 'ok' when
	This::wxGrid(), Row::integer(), Val::unicode:chardata().
setRowLabelValue(#wx_ref{type=ThisT,ref=ThisRef},Row,Val)
 when is_integer(Row),is_list(Val) ->
  ?CLASS(ThisT,wxGrid),
  Val_UC = unicode:characters_to_binary([Val,0]),
  wxe_util:cast(?wxGrid_SetRowLabelValue,
  <<ThisRef:32/?UI,Row:32/?UI,(byte_size(Val_UC)):32/?UI,(Val_UC)/binary, 0:(((8- ((4+byte_size(Val_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowminimalheight">external documentation</a>.
-spec setRowMinimalHeight(This, Row, Width) -> 'ok' when
	This::wxGrid(), Row::integer(), Width::integer().
setRowMinimalHeight(#wx_ref{type=ThisT,ref=ThisRef},Row,Width)
 when is_integer(Row),is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetRowMinimalHeight,
  <<ThisRef:32/?UI,Row:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowminimalacceptableheight">external documentation</a>.
-spec setRowMinimalAcceptableHeight(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().
setRowMinimalAcceptableHeight(#wx_ref{type=ThisT,ref=ThisRef},Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetRowMinimalAcceptableHeight,
  <<ThisRef:32/?UI,Width:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowsize">external documentation</a>.
-spec setRowSize(This, Row, Height) -> 'ok' when
	This::wxGrid(), Row::integer(), Height::integer().
setRowSize(#wx_ref{type=ThisT,ref=ThisRef},Row,Height)
 when is_integer(Row),is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetRowSize,
  <<ThisRef:32/?UI,Row:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetscrolllinex">external documentation</a>.
-spec setScrollLineX(This, X) -> 'ok' when
	This::wxGrid(), X::integer().
setScrollLineX(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetScrollLineX,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetscrollliney">external documentation</a>.
-spec setScrollLineY(This, Y) -> 'ok' when
	This::wxGrid(), Y::integer().
setScrollLineY(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetScrollLineY,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionbackground">external documentation</a>.
-spec setSelectionBackground(This, C) -> 'ok' when
	This::wxGrid(), C::wx:wx_colour().
setSelectionBackground(#wx_ref{type=ThisT,ref=ThisRef},C)
 when tuple_size(C) =:= 3; tuple_size(C) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetSelectionBackground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(C)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionforeground">external documentation</a>.
-spec setSelectionForeground(This, C) -> 'ok' when
	This::wxGrid(), C::wx:wx_colour().
setSelectionForeground(#wx_ref{type=ThisT,ref=ThisRef},C)
 when tuple_size(C) =:= 3; tuple_size(C) =:= 4 ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetSelectionForeground,
  <<ThisRef:32/?UI,(wxe_util:colour_bin(C)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionmode">external documentation</a>.
%%<br /> Selmode = ?wxGrid_wxGridSelectCells | ?wxGrid_wxGridSelectRows | ?wxGrid_wxGridSelectColumns
-spec setSelectionMode(This, Selmode) -> 'ok' when
	This::wxGrid(), Selmode::wx:wx_enum().
setSelectionMode(#wx_ref{type=ThisT,ref=ThisRef},Selmode)
 when is_integer(Selmode) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_SetSelectionMode,
  <<ThisRef:32/?UI,Selmode:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridshowcelleditcontrol">external documentation</a>.
-spec showCellEditControl(This) -> 'ok' when
	This::wxGrid().
showCellEditControl(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:cast(?wxGrid_ShowCellEditControl,
  <<ThisRef:32/?UI>>).

%% @equiv xToCol(This,X, [])
-spec xToCol(This, X) -> integer() when
	This::wxGrid(), X::integer().

xToCol(This,X)
 when is_record(This, wx_ref),is_integer(X) ->
  xToCol(This,X, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridxtocol">external documentation</a>.
-spec xToCol(This, X, [Option]) -> integer() when
	This::wxGrid(), X::integer(),
	Option :: {'clipToMinMax', boolean()}.
xToCol(#wx_ref{type=ThisT,ref=ThisRef},X, Options)
 when is_integer(X),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({clipToMinMax, ClipToMinMax}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(ClipToMinMax)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGrid_XToCol,
  <<ThisRef:32/?UI,X:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridxtoedgeofcol">external documentation</a>.
-spec xToEdgeOfCol(This, X) -> integer() when
	This::wxGrid(), X::integer().
xToEdgeOfCol(#wx_ref{type=ThisT,ref=ThisRef},X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_XToEdgeOfCol,
  <<ThisRef:32/?UI,X:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridytoedgeofrow">external documentation</a>.
-spec yToEdgeOfRow(This, Y) -> integer() when
	This::wxGrid(), Y::integer().
yToEdgeOfRow(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_YToEdgeOfRow,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridytorow">external documentation</a>.
-spec yToRow(This, Y) -> integer() when
	This::wxGrid(), Y::integer().
yToRow(#wx_ref{type=ThisT,ref=ThisRef},Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:call(?wxGrid_YToRow,
  <<ThisRef:32/?UI,Y:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGrid()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGrid),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxScrolledWindow
%% @hidden
setTargetWindow(This,Target) -> wxScrolledWindow:setTargetWindow(This,Target).
%% @hidden
setScrollRate(This,Xstep,Ystep) -> wxScrolledWindow:setScrollRate(This,Xstep,Ystep).
%% @hidden
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options).
%% @hidden
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY).
%% @hidden
scroll(This,X,Y) -> wxScrolledWindow:scroll(This,X,Y).
%% @hidden
prepareDC(This,Dc) -> wxScrolledWindow:prepareDC(This,Dc).
%% @hidden
doPrepareDC(This,Dc) -> wxScrolledWindow:doPrepareDC(This,Dc).
%% @hidden
getViewStart(This) -> wxScrolledWindow:getViewStart(This).
%% @hidden
getScrollPixelsPerUnit(This) -> wxScrolledWindow:getScrollPixelsPerUnit(This).
%% @hidden
enableScrolling(This,X_scrolling,Y_scrolling) -> wxScrolledWindow:enableScrolling(This,X_scrolling,Y_scrolling).
%% @hidden
calcUnscrolledPosition(This,X,Y) -> wxScrolledWindow:calcUnscrolledPosition(This,X,Y).
%% @hidden
calcUnscrolledPosition(This,Pt) -> wxScrolledWindow:calcUnscrolledPosition(This,Pt).
%% @hidden
calcScrolledPosition(This,X,Y) -> wxScrolledWindow:calcScrolledPosition(This,X,Y).
%% @hidden
calcScrolledPosition(This,Pt) -> wxScrolledWindow:calcScrolledPosition(This,Pt).
 %% From wxPanel
%% @hidden
setFocusIgnoringChildren(This) -> wxPanel:setFocusIgnoringChildren(This).
%% @hidden
initDialog(This) -> wxPanel:initDialog(This).
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
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
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
getLabel(This) -> wxWindow:getLabel(This).
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
