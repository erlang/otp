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

-module(wxGrid).
-moduledoc """
Functions for wxGrid class

`m:wxGrid` and its related classes are used for displaying and editing tabular
data. They provide a rich set of features for display, editing, and interacting
with a variety of data sources. For simple applications, and to help you get
started, `m:wxGrid` is the only class you need to refer to directly. It will set
up default instances of the other classes and manage them for you. For more
complex applications you can derive your own classes for custom grid views, grid
data tables, cell editors and renderers. The overview_grid has examples of
simple and more complex applications, explains the relationship between the
various grid classes and has a summary of the keyboard shortcuts and mouse
functions provided by `m:wxGrid`.

A `wxGridTableBase` (not implemented in wx) class holds the actual data to be
displayed by a `m:wxGrid` class. One or more `m:wxGrid` classes may act as a
view for one table class. The default table class is called `wxGridStringTable`
(not implemented in wx) and holds an array of strings. An instance of such a
class is created by `createGrid/4`.

`m:wxGridCellRenderer` is the abstract base class for rendering contents in a
cell. The following renderers are predefined:

The look of a cell can be further defined using `m:wxGridCellAttr`. An object of
this type may be returned by `wxGridTableBase::GetAttr()` (not implemented in
wx).

`m:wxGridCellEditor` is the abstract base class for editing the value of a cell.
The following editors are predefined:

Please see `m:wxGridEvent`, `wxGridSizeEvent` (not implemented in wx),
`wxGridRangeSelectEvent` (not implemented in wx), and `wxGridEditorCreatedEvent`
(not implemented in wx) for the documentation of all event types you can use
with `m:wxGrid`.

See:
[Overview grid](https://docs.wxwidgets.org/3.1/overview_grid.html#overview_grid),
`wxGridUpdateLocker` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxScrolledWindow`
`m:wxPanel` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs: [wxGrid](https://docs.wxwidgets.org/3.1/classwx_grid.html)
""".
-include("wxe.hrl").
-export([appendCols/1,appendCols/2,appendRows/1,appendRows/2,autoSize/1,autoSizeColumn/2,
  autoSizeColumn/3,autoSizeColumns/1,autoSizeColumns/2,autoSizeRow/2,
  autoSizeRow/3,autoSizeRows/1,autoSizeRows/2,beginBatch/1,blockToDeviceRect/3,
  canDragCell/1,canDragColMove/1,canDragColSize/2,canDragGridRowEdges/1,
  canDragGridSize/1,canDragRowSize/2,canEnableCellControl/1,cellToRect/2,
  cellToRect/3,clearGrid/1,clearSelection/1,createGrid/3,createGrid/4,
  deleteCols/1,deleteCols/2,deleteRows/1,deleteRows/2,destroy/1,disableCellEditControl/1,
  disableDragColSize/1,disableDragGridSize/1,disableDragRowSize/1,
  enableCellEditControl/1,enableCellEditControl/2,enableDragColSize/1,
  enableDragColSize/2,enableDragGridSize/1,enableDragGridSize/2,enableDragRowSize/1,
  enableDragRowSize/2,enableEditing/2,enableGridLines/1,enableGridLines/2,
  endBatch/1,fit/1,forceRefresh/1,getBatchCount/1,getCellAlignment/3,
  getCellBackgroundColour/3,getCellEditor/3,getCellFont/3,getCellRenderer/3,
  getCellTextColour/3,getCellValue/2,getCellValue/3,getColLabelAlignment/1,
  getColLabelSize/1,getColLabelValue/2,getColMinimalAcceptableWidth/1,
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
  getSelectionBlockTopLeft/1,getSelectionForeground/1,gridLinesEnabled/1,
  hideCellEditControl/1,insertCols/1,insertCols/2,insertRows/1,insertRows/2,
  isCellEditControlEnabled/1,isCurrentCellReadOnly/1,isEditable/1,
  isInSelection/2,isInSelection/3,isReadOnly/3,isSelection/1,isVisible/2,
  isVisible/3,isVisible/4,makeCellVisible/2,makeCellVisible/3,moveCursorDown/2,
  moveCursorDownBlock/2,moveCursorLeft/2,moveCursorLeftBlock/2,moveCursorRight/2,
  moveCursorRightBlock/2,moveCursorUp/2,moveCursorUpBlock/2,movePageDown/1,
  movePageUp/1,new/0,new/2,new/3,registerDataType/4,saveEditControlValue/1,
  selectAll/1,selectBlock/3,selectBlock/4,selectBlock/5,selectBlock/6,
  selectCol/2,selectCol/3,selectRow/2,selectRow/3,setCellAlignment/5,
  setCellBackgroundColour/4,setCellEditor/4,setCellFont/4,setCellRenderer/4,
  setCellTextColour/4,setCellValue/3,setCellValue/4,setColAttr/3,setColFormatBool/2,
  setColFormatCustom/3,setColFormatFloat/2,setColFormatFloat/3,setColFormatNumber/2,
  setColLabelAlignment/3,setColLabelSize/2,setColLabelValue/3,setColMinimalAcceptableWidth/2,
  setColMinimalWidth/3,setColSize/3,setDefaultCellAlignment/3,setDefaultCellBackgroundColour/2,
  setDefaultCellFont/2,setDefaultCellTextColour/2,setDefaultColSize/2,
  setDefaultColSize/3,setDefaultEditor/2,setDefaultRenderer/2,setDefaultRowSize/2,
  setDefaultRowSize/3,setGridCursor/2,setGridCursor/3,setGridLineColour/2,
  setLabelBackgroundColour/2,setLabelFont/2,setLabelTextColour/2,setMargins/3,
  setReadOnly/3,setReadOnly/4,setRowAttr/3,setRowLabelAlignment/3,setRowLabelSize/2,
  setRowLabelValue/3,setRowMinimalAcceptableHeight/2,setRowMinimalHeight/3,
  setRowSize/3,setScrollLineX/2,setScrollLineY/2,setSelectionBackground/2,
  setSelectionForeground/2,setSelectionMode/2,showCellEditControl/1,
  xToCol/2,xToCol/3,xToEdgeOfCol/2,yToEdgeOfRow/2,yToRow/2,yToRow/3]).

%% inherited exports
-export([cacheBestSize/2,calcScrolledPosition/2,calcScrolledPosition/3,calcUnscrolledPosition/2,
  calcUnscrolledPosition/3,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  doPrepareDC/2,dragAcceptFiles/2,enable/1,enable/2,enableScrolling/3,
  findWindow/2,fitInside/1,freeze/1,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,
  getChildren/1,getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,
  getCursor/1,getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,
  getFont/1,getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPixelsPerUnit/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getViewStart/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
  popupMenu/3,popupMenu/4,prepareDC/2,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scroll/2,scroll/3,scrollLines/2,scrollPages/2,scrollWindow/3,
  scrollWindow/4,setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,
  setBackgroundStyle/2,setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,
  setCursor/2,setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,
  setFocusFromKbd/1,setFocusIgnoringChildren/1,setFont/2,setForegroundColour/2,
  setHelpText/2,setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,
  setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,setPalette/2,
  setScrollPos/3,setScrollPos/4,setScrollRate/3,setScrollbar/5,setScrollbar/6,
  setScrollbars/5,setScrollbars/6,setSize/2,setSize/3,setSize/5,setSize/6,
  setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,setSizer/3,
  setSizerAndFit/2,setSizerAndFit/3,setTargetWindow/2,setThemeEnabled/2,
  setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxGrid() :: wx:wx_object().
-export_type([wxGrid/0]).
%% @hidden
-doc false.
parent_class(wxScrolledWindow) -> true;
parent_class(wxPanel) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridwxgrid">external documentation</a>.
-doc """
Default constructor.

You must call `Create()` (not implemented in wx) to really create the grid
window and also call `createGrid/4` or `SetTable()` (not implemented in wx) or
`AssignTable()` (not implemented in wx) to initialize its contents.
""".
-spec new() -> wxGrid().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxGrid_new_0),
  wxe_util:rec(?wxGrid_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxGrid() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridwxgrid">external documentation</a>.
-doc """
Constructor creating the grid window.

You must call either `createGrid/4` or `SetTable()` (not implemented in wx) or
`AssignTable()` (not implemented in wx) to initialize the grid contents before
using it.
""".
-spec new(Parent, Id, [Option]) -> wxGrid() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxGrid_new_3),
  wxe_util:rec(?wxGrid_new_3).

%% @equiv appendCols(This, [])
-spec appendCols(This) -> boolean() when
	This::wxGrid().

appendCols(This)
 when is_record(This, wx_ref) ->
  appendCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridappendcols">external documentation</a>.
-doc """
Appends one or more new columns to the right of the grid.

The `updateLabels` argument is not used at present. If you are using a derived
grid table class you will need to override `wxGridTableBase::AppendCols()` (not
implemented in wx). See `insertCols/2` for further information.

Return: true on success or false if appending columns failed.
""".
-spec appendCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'numCols', integer()}
		 | {'updateLabels', boolean()}.
appendCols(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({numCols, _numCols} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_AppendCols),
  wxe_util:rec(?wxGrid_AppendCols).

%% @equiv appendRows(This, [])
-spec appendRows(This) -> boolean() when
	This::wxGrid().

appendRows(This)
 when is_record(This, wx_ref) ->
  appendRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridappendrows">external documentation</a>.
-doc """
Appends one or more new rows to the bottom of the grid.

The `updateLabels` argument is not used at present. If you are using a derived
grid table class you will need to override `wxGridTableBase::AppendRows()` (not
implemented in wx). See `insertRows/2` for further information.

Return: true on success or false if appending rows failed.
""".
-spec appendRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'numRows', integer()}
		 | {'updateLabels', boolean()}.
appendRows(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({numRows, _numRows} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_AppendRows),
  wxe_util:rec(?wxGrid_AppendRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosize">external documentation</a>.
-doc """
Automatically sets the height and width of all rows and columns to fit their
contents.
""".
-spec autoSize(This) -> 'ok' when
	This::wxGrid().
autoSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_AutoSize).

%% @equiv autoSizeColumn(This,Col, [])
-spec autoSizeColumn(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

autoSizeColumn(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  autoSizeColumn(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizecolumn">external documentation</a>.
-doc """
Automatically sizes the column to fit its contents.

If `setAsMin` is true the calculated width will also be set as the minimal width
for the column.
""".
-spec autoSizeColumn(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'setAsMin', boolean()}.
autoSizeColumn(#wx_ref{type=ThisT}=This,Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, _setAsMin} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Col, Opts,?get_env(),?wxGrid_AutoSizeColumn).

%% @equiv autoSizeColumns(This, [])
-spec autoSizeColumns(This) -> 'ok' when
	This::wxGrid().

autoSizeColumns(This)
 when is_record(This, wx_ref) ->
  autoSizeColumns(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizecolumns">external documentation</a>.
-doc """
Automatically sizes all columns to fit their contents.

If `setAsMin` is true the calculated widths will also be set as the minimal
widths for the columns.
""".
-spec autoSizeColumns(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'setAsMin', boolean()}.
autoSizeColumns(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, _setAsMin} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_AutoSizeColumns).

%% @equiv autoSizeRow(This,Row, [])
-spec autoSizeRow(This, Row) -> 'ok' when
	This::wxGrid(), Row::integer().

autoSizeRow(This,Row)
 when is_record(This, wx_ref),is_integer(Row) ->
  autoSizeRow(This,Row, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizerow">external documentation</a>.
-doc """
Automatically sizes the row to fit its contents.

If `setAsMin` is true the calculated height will also be set as the minimal
height for the row.
""".
-spec autoSizeRow(This, Row, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(),
	Option :: {'setAsMin', boolean()}.
autoSizeRow(#wx_ref{type=ThisT}=This,Row, Options)
 when is_integer(Row),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, _setAsMin} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Row, Opts,?get_env(),?wxGrid_AutoSizeRow).

%% @equiv autoSizeRows(This, [])
-spec autoSizeRows(This) -> 'ok' when
	This::wxGrid().

autoSizeRows(This)
 when is_record(This, wx_ref) ->
  autoSizeRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridautosizerows">external documentation</a>.
-doc """
Automatically sizes all rows to fit their contents.

If `setAsMin` is true the calculated heights will also be set as the minimal
heights for the rows.
""".
-spec autoSizeRows(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'setAsMin', boolean()}.
autoSizeRows(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({setAsMin, _setAsMin} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_AutoSizeRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridbeginbatch">external documentation</a>.
-doc """
Increments the grid's batch count.

When the count is greater than zero repainting of the grid is suppressed. Each
call to BeginBatch must be matched by a later call to `endBatch/1`. Code that
does a lot of grid modification can be enclosed between `beginBatch/1` and
`endBatch/1` calls to avoid screen flicker. The final `endBatch/1` call will
cause the grid to be repainted.

Notice that you should use `wxGridUpdateLocker` (not implemented in wx) which
ensures that there is always a matching `endBatch/1` call for this
`beginBatch/1` if possible instead of calling this method directly.
""".
-spec beginBatch(This) -> 'ok' when
	This::wxGrid().
beginBatch(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_BeginBatch).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridblocktodevicerect">external documentation</a>.
-doc """
Convert grid cell coordinates to grid window pixel coordinates.

This function returns the rectangle that encloses the block of cells limited by
`topLeft` and `bottomRight` cell in device coords and clipped to the client size
of the grid window.

Since: 3.1.3 Parameter `gridWindow` has been added.

See: `cellToRect/3`
""".
-spec blockToDeviceRect(This, TopLeft, BottomRight) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()}.
blockToDeviceRect(#wx_ref{type=ThisT}=This,{TopLeftR,TopLeftC} = TopLeft,{BottomRightR,BottomRightC} = BottomRight)
 when is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,TopLeft,BottomRight,?get_env(),?wxGrid_BlockToDeviceRect),
  wxe_util:rec(?wxGrid_BlockToDeviceRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragcell">external documentation</a>.
-doc "Return true if the dragging of cells is enabled or false otherwise.".
-spec canDragCell(This) -> boolean() when
	This::wxGrid().
canDragCell(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_CanDragCell),
  wxe_util:rec(?wxGrid_CanDragCell).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragcolmove">external documentation</a>.
-doc """
Returns true if columns can be moved by dragging with the mouse.

Columns can be moved by dragging on their labels.
""".
-spec canDragColMove(This) -> boolean() when
	This::wxGrid().
canDragColMove(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_CanDragColMove),
  wxe_util:rec(?wxGrid_CanDragColMove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandraggridrowedges">external documentation</a>.
-doc """
Return true if row edges inside the grid can be dragged to resize the rows.

See: `canDragGridSize/1`, `canDragRowSize/2`

Since: 3.1.4
""".
-spec canDragGridRowEdges(This) -> boolean() when
	This::wxGrid().
canDragGridRowEdges(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_CanDragGridRowEdges),
  wxe_util:rec(?wxGrid_CanDragGridRowEdges).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragcolsize">external documentation</a>.
-doc """
Returns true if the given column can be resized by dragging with the mouse.

This function returns true if resizing the columns interactively is globally
enabled, i.e. if `disableDragColSize/1` hadn't been called, and if this column
wasn't explicitly marked as non-resizable with `DisableColResize()` (not
implemented in wx).
""".
-spec canDragColSize(This, Col) -> boolean() when
	This::wxGrid(), Col::integer().
canDragColSize(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxGrid_CanDragColSize),
  wxe_util:rec(?wxGrid_CanDragColSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandragrowsize">external documentation</a>.
-doc """
Returns true if the given row can be resized by dragging with the mouse.

This is the same as `canDragColSize/2` but for rows.
""".
-spec canDragRowSize(This, Row) -> boolean() when
	This::wxGrid(), Row::integer().
canDragRowSize(#wx_ref{type=ThisT}=This,Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,?get_env(),?wxGrid_CanDragRowSize),
  wxe_util:rec(?wxGrid_CanDragRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcandraggridsize">external documentation</a>.
-doc """
Return true if the dragging of grid lines to resize rows and columns is enabled
or false otherwise.
""".
-spec canDragGridSize(This) -> boolean() when
	This::wxGrid().
canDragGridSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_CanDragGridSize),
  wxe_util:rec(?wxGrid_CanDragGridSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcanenablecellcontrol">external documentation</a>.
-doc """
Returns true if the in-place edit control for the current grid cell can be used
and false otherwise.

This function always returns false for the read-only cells.
""".
-spec canEnableCellControl(This) -> boolean() when
	This::wxGrid().
canEnableCellControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_CanEnableCellControl),
  wxe_util:rec(?wxGrid_CanEnableCellControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcelltorect">external documentation</a>.
-doc """
Return the rectangle corresponding to the grid cell's size and position in
logical coordinates.

See: `blockToDeviceRect/3`
""".
-spec cellToRect(This, Coords) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
cellToRect(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords)
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Coords,?get_env(),?wxGrid_CellToRect_1),
  wxe_util:rec(?wxGrid_CellToRect_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcelltorect">external documentation</a>.
-doc """
Return the rectangle corresponding to the grid cell's size and position in
logical coordinates.

See: `blockToDeviceRect/3`
""".
-spec cellToRect(This, Row, Col) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxGrid(), Row::integer(), Col::integer().
cellToRect(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_CellToRect_2),
  wxe_util:rec(?wxGrid_CellToRect_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcleargrid">external documentation</a>.
-doc """
Clears all data in the underlying grid table and repaints the grid.

The table is not deleted by this function. If you are using a derived table
class then you need to override `wxGridTableBase::Clear()` (not implemented in
wx) for this function to have any effect.
""".
-spec clearGrid(This) -> 'ok' when
	This::wxGrid().
clearGrid(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_ClearGrid).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridclearselection">external documentation</a>.
-doc "Deselects all cells that are currently selected.".
-spec clearSelection(This) -> 'ok' when
	This::wxGrid().
clearSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_ClearSelection).

%% @equiv createGrid(This,NumRows,NumCols, [])
-spec createGrid(This, NumRows, NumCols) -> boolean() when
	This::wxGrid(), NumRows::integer(), NumCols::integer().

createGrid(This,NumRows,NumCols)
 when is_record(This, wx_ref),is_integer(NumRows),is_integer(NumCols) ->
  createGrid(This,NumRows,NumCols, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridcreategrid">external documentation</a>.
%%<br /> Selmode = ?wxGrid_wxGridSelectCells | ?wxGrid_wxGridSelectRows | ?wxGrid_wxGridSelectColumns | ?wxGrid_wxGridSelectRowsOrColumns
-doc """
Creates a grid with the specified initial number of rows and columns.

Call this directly after the grid constructor. When you use this function
`m:wxGrid` will create and manage a simple table of string values for you. All
of the grid data will be stored in memory.

For applications with more complex data types or relationships, or for dealing
with very large datasets, you should derive your own grid table class and pass a
table object to the grid with `SetTable()` (not implemented in wx) or
`AssignTable()` (not implemented in wx).
""".
-spec createGrid(This, NumRows, NumCols, [Option]) -> boolean() when
	This::wxGrid(), NumRows::integer(), NumCols::integer(),
	Option :: {'selmode', wx:wx_enum()}.
createGrid(#wx_ref{type=ThisT}=This,NumRows,NumCols, Options)
 when is_integer(NumRows),is_integer(NumCols),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({selmode, _selmode} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,NumRows,NumCols, Opts,?get_env(),?wxGrid_CreateGrid),
  wxe_util:rec(?wxGrid_CreateGrid).

%% @equiv deleteCols(This, [])
-spec deleteCols(This) -> boolean() when
	This::wxGrid().

deleteCols(This)
 when is_record(This, wx_ref) ->
  deleteCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddeletecols">external documentation</a>.
-doc """
Deletes one or more columns from a grid starting at the specified position.

The `updateLabels` argument is not used at present. If you are using a derived
grid table class you will need to override `wxGridTableBase::DeleteCols()` (not
implemented in wx). See `insertCols/2` for further information.

Return: true on success or false if deleting columns failed.
""".
-spec deleteCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numCols', integer()}
		 | {'updateLabels', boolean()}.
deleteCols(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, _pos} = Arg) -> Arg;
          ({numCols, _numCols} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_DeleteCols),
  wxe_util:rec(?wxGrid_DeleteCols).

%% @equiv deleteRows(This, [])
-spec deleteRows(This) -> boolean() when
	This::wxGrid().

deleteRows(This)
 when is_record(This, wx_ref) ->
  deleteRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddeleterows">external documentation</a>.
-doc """
Deletes one or more rows from a grid starting at the specified position.

The `updateLabels` argument is not used at present. If you are using a derived
grid table class you will need to override `wxGridTableBase::DeleteRows()` (not
implemented in wx). See `insertRows/2` for further information.

Return: true on success or false if deleting rows failed.
""".
-spec deleteRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numRows', integer()}
		 | {'updateLabels', boolean()}.
deleteRows(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, _pos} = Arg) -> Arg;
          ({numRows, _numRows} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_DeleteRows),
  wxe_util:rec(?wxGrid_DeleteRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisablecelleditcontrol">external documentation</a>.
-doc """
Disables in-place editing of grid cells.

Equivalent to calling EnableCellEditControl(false).
""".
-spec disableCellEditControl(This) -> 'ok' when
	This::wxGrid().
disableCellEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_DisableCellEditControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledragcolsize">external documentation</a>.
-doc """
Disables column sizing by dragging with the mouse.

Equivalent to passing false to `enableDragColSize/2`.
""".
-spec disableDragColSize(This) -> 'ok' when
	This::wxGrid().
disableDragColSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_DisableDragColSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledraggridsize">external documentation</a>.
-doc """
Disable mouse dragging of grid lines to resize rows and columns.

Equivalent to passing false to `enableDragGridSize/2`
""".
-spec disableDragGridSize(This) -> 'ok' when
	This::wxGrid().
disableDragGridSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_DisableDragGridSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgriddisabledragrowsize">external documentation</a>.
-doc """
Disables row sizing by dragging with the mouse.

Equivalent to passing false to `enableDragRowSize/2`.
""".
-spec disableDragRowSize(This) -> 'ok' when
	This::wxGrid().
disableDragRowSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_DisableDragRowSize).

%% @equiv enableCellEditControl(This, [])
-spec enableCellEditControl(This) -> 'ok' when
	This::wxGrid().

enableCellEditControl(This)
 when is_record(This, wx_ref) ->
  enableCellEditControl(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenablecelleditcontrol">external documentation</a>.
-doc """
Enables or disables in-place editing of grid cell data.

Enabling in-place editing generates `wxEVT_GRID_EDITOR_SHOWN` and, if it isn't
vetoed by the application, shows the in-place editor which allows the user to
change the cell value.

Disabling in-place editing does nothing if the in-place editor isn't currently
shown, otherwise the `wxEVT_GRID_EDITOR_HIDDEN` event is generated but, unlike
the "shown" event, it can't be vetoed and the in-place editor is dismissed
unconditionally.

Note that it is an error to call this function if the current cell is read-only,
use `canEnableCellControl/1` to check for this precondition.
""".
-spec enableCellEditControl(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableCellEditControl(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_EnableCellEditControl).

%% @equiv enableDragColSize(This, [])
-spec enableDragColSize(This) -> 'ok' when
	This::wxGrid().

enableDragColSize(This)
 when is_record(This, wx_ref) ->
  enableDragColSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledragcolsize">external documentation</a>.
-doc """
Enables or disables column sizing by dragging with the mouse.

See: `DisableColResize()` (not implemented in wx)
""".
-spec enableDragColSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragColSize(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_EnableDragColSize).

%% @equiv enableDragGridSize(This, [])
-spec enableDragGridSize(This) -> 'ok' when
	This::wxGrid().

enableDragGridSize(This)
 when is_record(This, wx_ref) ->
  enableDragGridSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledraggridsize">external documentation</a>.
-doc """
Enables or disables row and column resizing by dragging gridlines with the
mouse.
""".
-spec enableDragGridSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragGridSize(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_EnableDragGridSize).

%% @equiv enableDragRowSize(This, [])
-spec enableDragRowSize(This) -> 'ok' when
	This::wxGrid().

enableDragRowSize(This)
 when is_record(This, wx_ref) ->
  enableDragRowSize(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenabledragrowsize">external documentation</a>.
-doc """
Enables or disables row sizing by dragging with the mouse.

See: `DisableRowResize()` (not implemented in wx)
""".
-spec enableDragRowSize(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableDragRowSize(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_EnableDragRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenableediting">external documentation</a>.
-doc """
Makes the grid globally editable or read-only.

If the edit argument is false this function sets the whole grid as read-only. If
the argument is true the grid is set to the default state where cells may be
editable. In the default state you can set single grid cells and whole rows and
columns to be editable or read-only via `wxGridCellAttr:setReadOnly/2`. For
single cells you can also use the shortcut function `setReadOnly/4`.

For more information about controlling grid cell attributes see the
`m:wxGridCellAttr` class and the overview_grid.
""".
-spec enableEditing(This, Edit) -> 'ok' when
	This::wxGrid(), Edit::boolean().
enableEditing(#wx_ref{type=ThisT}=This,Edit)
 when is_boolean(Edit) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Edit,?get_env(),?wxGrid_EnableEditing).

%% @equiv enableGridLines(This, [])
-spec enableGridLines(This) -> 'ok' when
	This::wxGrid().

enableGridLines(This)
 when is_record(This, wx_ref) ->
  enableGridLines(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridenablegridlines">external documentation</a>.
-doc "Turns the drawing of grid lines on or off.".
-spec enableGridLines(This, [Option]) -> 'ok' when
	This::wxGrid(),
	Option :: {'enable', boolean()}.
enableGridLines(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_EnableGridLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridendbatch">external documentation</a>.
-doc """
Decrements the grid's batch count.

When the count is greater than zero repainting of the grid is suppressed. Each
previous call to `beginBatch/1` must be matched by a later call to `endBatch/1`.
Code that does a lot of grid modification can be enclosed between `beginBatch/1`
and `endBatch/1` calls to avoid screen flicker. The final `endBatch/1` will
cause the grid to be repainted.

See: `wxGridUpdateLocker` (not implemented in wx)
""".
-spec endBatch(This) -> 'ok' when
	This::wxGrid().
endBatch(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_EndBatch).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridfit">external documentation</a>.
-doc "Overridden `m:wxWindow` method.".
-spec fit(This) -> 'ok' when
	This::wxGrid().
fit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_Fit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridforcerefresh">external documentation</a>.
-doc """
Causes immediate repainting of the grid.

Use this instead of the usual `wxWindow:refresh/2`.
""".
-spec forceRefresh(This) -> 'ok' when
	This::wxGrid().
forceRefresh(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_ForceRefresh).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetbatchcount">external documentation</a>.
-doc """
Returns the number of times that `beginBatch/1` has been called without (yet)
matching calls to `endBatch/1`.

While the grid's batch count is greater than zero the display will not be
updated.
""".
-spec getBatchCount(This) -> integer() when
	This::wxGrid().
getBatchCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetBatchCount),
  wxe_util:rec(?wxGrid_GetBatchCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellalignment">external documentation</a>.
-doc """
Sets the arguments to the horizontal and vertical text alignment values for the
grid cell at the specified location.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or
`wxALIGN_BOTTOM`.
""".
-spec getCellAlignment(This, Row, Col) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid(), Row::integer(), Col::integer().
getCellAlignment(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellAlignment),
  wxe_util:rec(?wxGrid_GetCellAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellbackgroundcolour">external documentation</a>.
-doc "Returns the background colour of the cell at the specified location.".
-spec getCellBackgroundColour(This, Row, Col) -> wx:wx_colour4() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellBackgroundColour(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellBackgroundColour),
  wxe_util:rec(?wxGrid_GetCellBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcelleditor">external documentation</a>.
-doc """
Returns a pointer to the editor for the cell at the specified location.

See `m:wxGridCellEditor` and the overview_grid for more information about cell
editors and renderers.

The caller must call DecRef() on the returned pointer.
""".
-spec getCellEditor(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellEditor(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellEditor),
  wxe_util:rec(?wxGrid_GetCellEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellfont">external documentation</a>.
-doc "Returns the font for text in the grid cell at the specified location.".
-spec getCellFont(This, Row, Col) -> wxFont:wxFont() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellFont(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellFont),
  wxe_util:rec(?wxGrid_GetCellFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellrenderer">external documentation</a>.
-doc """
Returns a pointer to the renderer for the grid cell at the specified location.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell
editors and renderers.

The caller must call DecRef() on the returned pointer.
""".
-spec getCellRenderer(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellRenderer(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellRenderer),
  wxe_util:rec(?wxGrid_GetCellRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcelltextcolour">external documentation</a>.
-doc "Returns the text colour for the grid cell at the specified location.".
-spec getCellTextColour(This, Row, Col) -> wx:wx_colour4() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellTextColour(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellTextColour),
  wxe_util:rec(?wxGrid_GetCellTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellvalue">external documentation</a>.
-doc """
Returns the string contained in the cell at the specified location.

For simple applications where a grid object automatically uses a default grid
table of string values you use this function together with `setCellValue/4` to
access cell values. For more complex applications where you have derived your
own grid table class that contains various data types (e.g. numeric, boolean or
user-defined custom types) then you only use this function for those cells that
contain string values.

See `wxGridTableBase::CanGetValueAs()` (not implemented in wx) and the
overview_grid for more information.
""".
-spec getCellValue(This, Coords) -> unicode:charlist() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
getCellValue(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords)
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Coords,?get_env(),?wxGrid_GetCellValue_1),
  wxe_util:rec(?wxGrid_GetCellValue_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcellvalue">external documentation</a>.
-doc """
Returns the string contained in the cell at the specified location.

For simple applications where a grid object automatically uses a default grid
table of string values you use this function together with `setCellValue/4` to
access cell values. For more complex applications where you have derived your
own grid table class that contains various data types (e.g. numeric, boolean or
user-defined custom types) then you only use this function for those cells that
contain string values.

See `wxGridTableBase::CanGetValueAs()` (not implemented in wx) and the
overview_grid for more information.
""".
-spec getCellValue(This, Row, Col) -> unicode:charlist() when
	This::wxGrid(), Row::integer(), Col::integer().
getCellValue(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetCellValue_2),
  wxe_util:rec(?wxGrid_GetCellValue_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelalignment">external documentation</a>.
-doc """
Sets the arguments to the current column label alignment values.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or
`wxALIGN_BOTTOM`.
""".
-spec getColLabelAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getColLabelAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetColLabelAlignment),
  wxe_util:rec(?wxGrid_GetColLabelAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelsize">external documentation</a>.
-doc "Returns the current height of the column labels.".
-spec getColLabelSize(This) -> integer() when
	This::wxGrid().
getColLabelSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetColLabelSize),
  wxe_util:rec(?wxGrid_GetColLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcollabelvalue">external documentation</a>.
-doc """
Returns the specified column label.

The default grid table class provides column labels of the form
A,B...Z,AA,AB...ZZ,AAA... If you are using a custom grid table you can override
`wxGridTableBase::GetColLabelValue()` (not implemented in wx) to provide your
own labels.
""".
-spec getColLabelValue(This, Col) -> unicode:charlist() when
	This::wxGrid(), Col::integer().
getColLabelValue(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxGrid_GetColLabelValue),
  wxe_util:rec(?wxGrid_GetColLabelValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetcolminimalacceptablewidth">external documentation</a>.
-doc """
Returns the minimal width to which a column may be resized.

Use `setColMinimalAcceptableWidth/2` to change this value globally or
`setColMinimalWidth/3` to do it for individual columns.

See: `getRowMinimalAcceptableHeight/1`
""".
-spec getColMinimalAcceptableWidth(This) -> integer() when
	This::wxGrid().
getColMinimalAcceptableWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetColMinimalAcceptableWidth),
  wxe_util:rec(?wxGrid_GetColMinimalAcceptableWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellalignment">external documentation</a>.
-doc """
Returns the default cell alignment.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or
`wxALIGN_BOTTOM`.

See: `setDefaultCellAlignment/3`
""".
-spec getDefaultCellAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getDefaultCellAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultCellAlignment),
  wxe_util:rec(?wxGrid_GetDefaultCellAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellbackgroundcolour">external documentation</a>.
-doc "Returns the current default background colour for grid cells.".
-spec getDefaultCellBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getDefaultCellBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultCellBackgroundColour),
  wxe_util:rec(?wxGrid_GetDefaultCellBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcellfont">external documentation</a>.
-doc "Returns the current default font for grid cell text.".
-spec getDefaultCellFont(This) -> wxFont:wxFont() when
	This::wxGrid().
getDefaultCellFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultCellFont),
  wxe_util:rec(?wxGrid_GetDefaultCellFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcelltextcolour">external documentation</a>.
-doc "Returns the current default colour for grid cell text.".
-spec getDefaultCellTextColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getDefaultCellTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultCellTextColour),
  wxe_util:rec(?wxGrid_GetDefaultCellTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcollabelsize">external documentation</a>.
-doc "Returns the default height for column labels.".
-spec getDefaultColLabelSize(This) -> integer() when
	This::wxGrid().
getDefaultColLabelSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultColLabelSize),
  wxe_util:rec(?wxGrid_GetDefaultColLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultcolsize">external documentation</a>.
-doc "Returns the current default width for grid columns.".
-spec getDefaultColSize(This) -> integer() when
	This::wxGrid().
getDefaultColSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultColSize),
  wxe_util:rec(?wxGrid_GetDefaultColSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditor">external documentation</a>.
-doc """
Returns a pointer to the current default grid cell editor.

See `m:wxGridCellEditor` and the overview_grid for more information about cell
editors and renderers.
""".
-spec getDefaultEditor(This) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid().
getDefaultEditor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultEditor),
  wxe_util:rec(?wxGrid_GetDefaultEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorforcell">external documentation</a>.
-doc """
Returns the default editor for the specified cell.

The base class version returns the editor appropriate for the current cell type
but this method may be overridden in the derived classes to use custom editors
for some cells by default.

Notice that the same may be achieved in a usually simpler way by associating a
custom editor with the given cell or cells.

The caller must call DecRef() on the returned pointer.
""".
-spec getDefaultEditorForCell(This, C) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), C::{R::integer(), C::integer()}.
getDefaultEditorForCell(#wx_ref{type=ThisT}=This,{CR,CC} = C)
 when is_integer(CR),is_integer(CC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,C,?get_env(),?wxGrid_GetDefaultEditorForCell_1),
  wxe_util:rec(?wxGrid_GetDefaultEditorForCell_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorforcell">external documentation</a>.
-doc """
Returns the default editor for the specified cell.

The base class version returns the editor appropriate for the current cell type
but this method may be overridden in the derived classes to use custom editors
for some cells by default.

Notice that the same may be achieved in a usually simpler way by associating a
custom editor with the given cell or cells.

The caller must call DecRef() on the returned pointer.
""".
-spec getDefaultEditorForCell(This, Row, Col) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), Row::integer(), Col::integer().
getDefaultEditorForCell(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetDefaultEditorForCell_2),
  wxe_util:rec(?wxGrid_GetDefaultEditorForCell_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaulteditorfortype">external documentation</a>.
-doc """
Returns the default editor for the cells containing values of the given type.

The base class version returns the editor which was associated with the
specified `typeName` when it was registered `registerDataType/4` but this
function may be overridden to return something different. This allows overriding
an editor used for one of the standard types.

The caller must call DecRef() on the returned pointer.
""".
-spec getDefaultEditorForType(This, TypeName) -> wxGridCellEditor:wxGridCellEditor() when
	This::wxGrid(), TypeName::unicode:chardata().
getDefaultEditorForType(#wx_ref{type=ThisT}=This,TypeName)
 when ?is_chardata(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary(TypeName),
  wxe_util:queue_cmd(This,TypeName_UC,?get_env(),?wxGrid_GetDefaultEditorForType),
  wxe_util:rec(?wxGrid_GetDefaultEditorForType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrenderer">external documentation</a>.
-doc """
Returns a pointer to the current default grid cell renderer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell
editors and renderers.

The caller must call DecRef() on the returned pointer.
""".
-spec getDefaultRenderer(This) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid().
getDefaultRenderer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultRenderer),
  wxe_util:rec(?wxGrid_GetDefaultRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrendererforcell">external documentation</a>.
-doc """
Returns the default renderer for the given cell.

The base class version returns the renderer appropriate for the current cell
type but this method may be overridden in the derived classes to use custom
renderers for some cells by default.

The caller must call DecRef() on the returned pointer.
""".
-spec getDefaultRendererForCell(This, Row, Col) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), Row::integer(), Col::integer().
getDefaultRendererForCell(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetDefaultRendererForCell),
  wxe_util:rec(?wxGrid_GetDefaultRendererForCell).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrendererfortype">external documentation</a>.
-doc """
Returns the default renderer for the cell containing values of the given type.

See: `getDefaultEditorForType/2`
""".
-spec getDefaultRendererForType(This, TypeName) -> wxGridCellRenderer:wxGridCellRenderer() when
	This::wxGrid(), TypeName::unicode:chardata().
getDefaultRendererForType(#wx_ref{type=ThisT}=This,TypeName)
 when ?is_chardata(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary(TypeName),
  wxe_util:queue_cmd(This,TypeName_UC,?get_env(),?wxGrid_GetDefaultRendererForType),
  wxe_util:rec(?wxGrid_GetDefaultRendererForType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrowlabelsize">external documentation</a>.
-doc "Returns the default width for the row labels.".
-spec getDefaultRowLabelSize(This) -> integer() when
	This::wxGrid().
getDefaultRowLabelSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultRowLabelSize),
  wxe_util:rec(?wxGrid_GetDefaultRowLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetdefaultrowsize">external documentation</a>.
-doc "Returns the current default height for grid rows.".
-spec getDefaultRowSize(This) -> integer() when
	This::wxGrid().
getDefaultRowSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetDefaultRowSize),
  wxe_util:rec(?wxGrid_GetDefaultRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcursorcol">external documentation</a>.
-doc """
Returns the current grid cell column position.

See: `GetGridCursorCoords()` (not implemented in wx)
""".
-spec getGridCursorCol(This) -> integer() when
	This::wxGrid().
getGridCursorCol(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridCursorCol),
  wxe_util:rec(?wxGrid_GetGridCursorCol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcursorrow">external documentation</a>.
-doc """
Returns the current grid cell row position.

See: `GetGridCursorCoords()` (not implemented in wx)
""".
-spec getGridCursorRow(This) -> integer() when
	This::wxGrid().
getGridCursorRow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridCursorRow),
  wxe_util:rec(?wxGrid_GetGridCursorRow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridlinecolour">external documentation</a>.
-doc """
Returns the colour used for grid lines.

See: `GetDefaultGridLinePen()` (not implemented in wx)
""".
-spec getGridLineColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getGridLineColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridLineColour),
  wxe_util:rec(?wxGrid_GetGridLineColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgridlinesenabled">external documentation</a>.
-doc "Returns true if drawing of grid lines is turned on, false otherwise.".
-spec gridLinesEnabled(This) -> boolean() when
	This::wxGrid().
gridLinesEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GridLinesEnabled),
  wxe_util:rec(?wxGrid_GridLinesEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabelbackgroundcolour">external documentation</a>.
-doc "Returns the colour used for the background of row and column labels.".
-spec getLabelBackgroundColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getLabelBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetLabelBackgroundColour),
  wxe_util:rec(?wxGrid_GetLabelBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabelfont">external documentation</a>.
-doc "Returns the font used for row and column labels.".
-spec getLabelFont(This) -> wxFont:wxFont() when
	This::wxGrid().
getLabelFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetLabelFont),
  wxe_util:rec(?wxGrid_GetLabelFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetlabeltextcolour">external documentation</a>.
-doc "Returns the colour used for row and column label text.".
-spec getLabelTextColour(This) -> wx:wx_colour4() when
	This::wxGrid().
getLabelTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetLabelTextColour),
  wxe_util:rec(?wxGrid_GetLabelTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetnumbercols">external documentation</a>.
-doc """
Returns the total number of grid columns.

This is the same as the number of columns in the underlying grid table.
""".
-spec getNumberCols(This) -> integer() when
	This::wxGrid().
getNumberCols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetNumberCols),
  wxe_util:rec(?wxGrid_GetNumberCols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetnumberrows">external documentation</a>.
-doc """
Returns the total number of grid rows.

This is the same as the number of rows in the underlying grid table.
""".
-spec getNumberRows(This) -> integer() when
	This::wxGrid().
getNumberRows(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetNumberRows),
  wxe_util:rec(?wxGrid_GetNumberRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetorcreatecellattr">external documentation</a>.
-doc """
Returns the attribute for the given cell creating one if necessary.

If the cell already has an attribute, it is returned. Otherwise a new attribute
is created, associated with the cell and returned. In any case the caller must
call DecRef() on the returned pointer.

Prefer to use `GetOrCreateCellAttrPtr()` (not implemented in wx) to avoid the
need to call DecRef() on the returned pointer.

This function may only be called if `CanHaveAttributes()` (not implemented in
wx) returns true.
""".
-spec getOrCreateCellAttr(This, Row, Col) -> wxGridCellAttr:wxGridCellAttr() when
	This::wxGrid(), Row::integer(), Col::integer().
getOrCreateCellAttr(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_GetOrCreateCellAttr),
  wxe_util:rec(?wxGrid_GetOrCreateCellAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowminimalacceptableheight">external documentation</a>.
-doc """
Returns the minimal size to which rows can be resized.

Use `setRowMinimalAcceptableHeight/2` to change this value globally or
`setRowMinimalHeight/3` to do it for individual cells.

See: `getColMinimalAcceptableWidth/1`
""".
-spec getRowMinimalAcceptableHeight(This) -> integer() when
	This::wxGrid().
getRowMinimalAcceptableHeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetRowMinimalAcceptableHeight),
  wxe_util:rec(?wxGrid_GetRowMinimalAcceptableHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelalignment">external documentation</a>.
-doc """
Returns the alignment used for row labels.

Horizontal alignment will be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`.

Vertical alignment will be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or
`wxALIGN_BOTTOM`.
""".
-spec getRowLabelAlignment(This) -> {Horiz::integer(), Vert::integer()} when
	This::wxGrid().
getRowLabelAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetRowLabelAlignment),
  wxe_util:rec(?wxGrid_GetRowLabelAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelsize">external documentation</a>.
-doc "Returns the current width of the row labels.".
-spec getRowLabelSize(This) -> integer() when
	This::wxGrid().
getRowLabelSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetRowLabelSize),
  wxe_util:rec(?wxGrid_GetRowLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowlabelvalue">external documentation</a>.
-doc """
Returns the specified row label.

The default grid table class provides numeric row labels. If you are using a
custom grid table you can override `wxGridTableBase::GetRowLabelValue()` (not
implemented in wx) to provide your own labels.
""".
-spec getRowLabelValue(This, Row) -> unicode:charlist() when
	This::wxGrid(), Row::integer().
getRowLabelValue(#wx_ref{type=ThisT}=This,Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,?get_env(),?wxGrid_GetRowLabelValue),
  wxe_util:rec(?wxGrid_GetRowLabelValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetrowsize">external documentation</a>.
-doc "Returns the height of the specified row.".
-spec getRowSize(This, Row) -> integer() when
	This::wxGrid(), Row::integer().
getRowSize(#wx_ref{type=ThisT}=This,Row)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,?get_env(),?wxGrid_GetRowSize),
  wxe_util:rec(?wxGrid_GetRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetscrolllinex">external documentation</a>.
-doc """
Returns the number of pixels per horizontal scroll increment.

The default is 15.

See: `getScrollLineY/1`, `setScrollLineX/2`, `setScrollLineY/2`
""".
-spec getScrollLineX(This) -> integer() when
	This::wxGrid().
getScrollLineX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetScrollLineX),
  wxe_util:rec(?wxGrid_GetScrollLineX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetscrollliney">external documentation</a>.
-doc """
Returns the number of pixels per vertical scroll increment.

The default is 15.

See: `getScrollLineX/1`, `setScrollLineX/2`, `setScrollLineY/2`
""".
-spec getScrollLineY(This) -> integer() when
	This::wxGrid().
getScrollLineY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetScrollLineY),
  wxe_util:rec(?wxGrid_GetScrollLineY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedcells">external documentation</a>.
-doc """
Returns an array of individually selected cells.

Notice that this array does `not` contain all the selected cells in general as
it doesn't include the cells selected as part of column, row or block selection.
You must use this method, `getSelectedCols/1`, `getSelectedRows/1` and
`getSelectionBlockTopLeft/1` and `getSelectionBlockBottomRight/1` methods to
obtain the entire selection in general.

Please notice this behaviour is by design and is needed in order to support
grids of arbitrary size (when an entire column is selected in a grid with a
million of columns, we don't want to create an array with a million of entries
in this function, instead it returns an empty array and `getSelectedCols/1`
returns an array containing one element).

The function can be slow for the big grids, use `GetSelectedBlocks()` (not
implemented in wx) in the new code.
""".
-spec getSelectedCells(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectedCells(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectedCells),
  wxe_util:rec(?wxGrid_GetSelectedCells).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedcols">external documentation</a>.
-doc """
Returns an array of selected columns.

Please notice that this method alone is not sufficient to find all the selected
columns as it contains only the columns which were individually selected but not
those being part of the block selection or being selected in virtue of all of
their cells being selected individually, please see `getSelectedCells/1` for
more details.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not
implemented in wx) in the new code.
""".
-spec getSelectedCols(This) -> [integer()] when
	This::wxGrid().
getSelectedCols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectedCols),
  wxe_util:rec(?wxGrid_GetSelectedCols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectedrows">external documentation</a>.
-doc """
Returns an array of selected rows.

Please notice that this method alone is not sufficient to find all the selected
rows as it contains only the rows which were individually selected but not those
being part of the block selection or being selected in virtue of all of their
cells being selected individually, please see `getSelectedCells/1` for more
details.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not
implemented in wx) in the new code.
""".
-spec getSelectedRows(This) -> [integer()] when
	This::wxGrid().
getSelectedRows(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectedRows),
  wxe_util:rec(?wxGrid_GetSelectedRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionbackground">external documentation</a>.
-doc "Returns the colour used for drawing the selection background.".
-spec getSelectionBackground(This) -> wx:wx_colour4() when
	This::wxGrid().
getSelectionBackground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectionBackground),
  wxe_util:rec(?wxGrid_GetSelectionBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionblocktopleft">external documentation</a>.
-doc """
Returns an array of the top left corners of blocks of selected cells.

Please see `getSelectedCells/1` for more information about the selection
representation in `m:wxGrid`.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not
implemented in wx) in the new code.

See: `getSelectionBlockBottomRight/1`
""".
-spec getSelectionBlockTopLeft(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectionBlockTopLeft(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectionBlockTopLeft),
  wxe_util:rec(?wxGrid_GetSelectionBlockTopLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionblockbottomright">external documentation</a>.
-doc """
Returns an array of the bottom right corners of blocks of selected cells.

Please see `getSelectedCells/1` for more information about the selection
representation in `m:wxGrid`.

The function can be slow for the big grids, use `GetSelectedBlocks()` (not
implemented in wx) in the new code.

See: `getSelectionBlockTopLeft/1`
""".
-spec getSelectionBlockBottomRight(This) -> [{R::integer(), C::integer()}] when
	This::wxGrid().
getSelectionBlockBottomRight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectionBlockBottomRight),
  wxe_util:rec(?wxGrid_GetSelectionBlockBottomRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetselectionforeground">external documentation</a>.
-doc "Returns the colour used for drawing the selection foreground.".
-spec getSelectionForeground(This) -> wx:wx_colour4() when
	This::wxGrid().
getSelectionForeground(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetSelectionForeground),
  wxe_util:rec(?wxGrid_GetSelectionForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridwindow">external documentation</a>.
-doc """
Return the main grid window containing the grid cells.

This window is always shown.
""".
-spec getGridWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridWindow),
  wxe_util:rec(?wxGrid_GetGridWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridrowlabelwindow">external documentation</a>.
-doc """
Return the row labels window.

This window is not shown if the row labels were hidden using `HideRowLabels()`
(not implemented in wx).
""".
-spec getGridRowLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridRowLabelWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridRowLabelWindow),
  wxe_util:rec(?wxGrid_GetGridRowLabelWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcollabelwindow">external documentation</a>.
-doc """
Return the column labels window.

This window is not shown if the columns labels were hidden using
`HideColLabels()` (not implemented in wx).

Depending on whether `UseNativeColHeader()` (not implemented in wx) was called
or not this can be either a `wxHeaderCtrl` (not implemented in wx) or a plain
`m:wxWindow`. This function returns a valid window pointer in either case but in
the former case you can also use `GetGridColHeader()` (not implemented in wx) to
access it if you need wxHeaderCtrl-specific functionality.
""".
-spec getGridColLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridColLabelWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridColLabelWindow),
  wxe_util:rec(?wxGrid_GetGridColLabelWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridgetgridcornerlabelwindow">external documentation</a>.
-doc """
Return the window in the top left grid corner.

This window is shown only of both columns and row labels are shown and normally
doesn't contain anything. Clicking on it is handled by `m:wxGrid` however and
can be used to select the entire grid.
""".
-spec getGridCornerLabelWindow(This) -> wxWindow:wxWindow() when
	This::wxGrid().
getGridCornerLabelWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_GetGridCornerLabelWindow),
  wxe_util:rec(?wxGrid_GetGridCornerLabelWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridhidecelleditcontrol">external documentation</a>.
-doc "Hides the in-place cell edit control.".
-spec hideCellEditControl(This) -> 'ok' when
	This::wxGrid().
hideCellEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_HideCellEditControl).

%% @equiv insertCols(This, [])
-spec insertCols(This) -> boolean() when
	This::wxGrid().

insertCols(This)
 when is_record(This, wx_ref) ->
  insertCols(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridinsertcols">external documentation</a>.
-doc """
Inserts one or more new columns into a grid with the first new column at the
specified position.

Notice that inserting the columns in the grid requires grid table cooperation:
when this method is called, grid object begins by requesting the underlying grid
table to insert new columns. If this is successful the table notifies the grid
and the grid updates the display. For a default grid (one where you have called
`createGrid/4`) this process is automatic. If you are using a custom grid table
(specified with `SetTable()` (not implemented in wx) or `AssignTable()` (not
implemented in wx)) then you must override `wxGridTableBase::InsertCols()` (not
implemented in wx) in your derived table class.

Return: true if the columns were successfully inserted, false if an error
occurred (most likely the table couldn't be updated).
""".
-spec insertCols(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numCols', integer()}
		 | {'updateLabels', boolean()}.
insertCols(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, _pos} = Arg) -> Arg;
          ({numCols, _numCols} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_InsertCols),
  wxe_util:rec(?wxGrid_InsertCols).

%% @equiv insertRows(This, [])
-spec insertRows(This) -> boolean() when
	This::wxGrid().

insertRows(This)
 when is_record(This, wx_ref) ->
  insertRows(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridinsertrows">external documentation</a>.
-doc """
Inserts one or more new rows into a grid with the first new row at the specified
position.

Notice that you must implement `wxGridTableBase::InsertRows()` (not implemented
in wx) if you use a grid with a custom table, please see `insertCols/2` for more
information.

Return: true if the rows were successfully inserted, false if an error occurred
(most likely the table couldn't be updated).
""".
-spec insertRows(This, [Option]) -> boolean() when
	This::wxGrid(),
	Option :: {'pos', integer()}
		 | {'numRows', integer()}
		 | {'updateLabels', boolean()}.
insertRows(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({pos, _pos} = Arg) -> Arg;
          ({numRows, _numRows} = Arg) -> Arg;
          ({updateLabels, _updateLabels} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGrid_InsertRows),
  wxe_util:rec(?wxGrid_InsertRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiscelleditcontrolenabled">external documentation</a>.
-doc "Returns true if the in-place edit control is currently enabled.".
-spec isCellEditControlEnabled(This) -> boolean() when
	This::wxGrid().
isCellEditControlEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_IsCellEditControlEnabled),
  wxe_util:rec(?wxGrid_IsCellEditControlEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiscurrentcellreadonly">external documentation</a>.
-doc """
Returns true if the current cell is read-only.

See: `setReadOnly/4`, `isReadOnly/3`
""".
-spec isCurrentCellReadOnly(This) -> boolean() when
	This::wxGrid().
isCurrentCellReadOnly(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_IsCurrentCellReadOnly),
  wxe_util:rec(?wxGrid_IsCurrentCellReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridiseditable">external documentation</a>.
-doc """
Returns false if the whole grid has been set as read-only or true otherwise.

See `enableEditing/2` for more information about controlling the editing status
of grid cells.
""".
-spec isEditable(This) -> boolean() when
	This::wxGrid().
isEditable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_IsEditable),
  wxe_util:rec(?wxGrid_IsEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisinselection">external documentation</a>.
-doc "Returns true if the given cell is selected.".
-spec isInSelection(This, Coords) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
isInSelection(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords)
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Coords,?get_env(),?wxGrid_IsInSelection_1),
  wxe_util:rec(?wxGrid_IsInSelection_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisinselection">external documentation</a>.
-doc "Returns true if the given cell is selected.".
-spec isInSelection(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer().
isInSelection(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_IsInSelection_2),
  wxe_util:rec(?wxGrid_IsInSelection_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisreadonly">external documentation</a>.
-doc """
Returns true if the cell at the specified location can't be edited.

See: `setReadOnly/4`, `isCurrentCellReadOnly/1`
""".
-spec isReadOnly(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer().
isReadOnly(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_IsReadOnly),
  wxe_util:rec(?wxGrid_IsReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisselection">external documentation</a>.
-doc "Returns true if there are currently any selected cells, rows, columns or blocks.".
-spec isSelection(This) -> boolean() when
	This::wxGrid().
isSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_IsSelection),
  wxe_util:rec(?wxGrid_IsSelection).

%% @equiv isVisible(This,Coords, [])
-spec isVisible(This, Coords) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.

isVisible(This,{CoordsR,CoordsC} = Coords)
 when is_record(This, wx_ref),is_integer(CoordsR),is_integer(CoordsC) ->
  isVisible(This,Coords, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisvisible">external documentation</a>.
%% <br /> Also:<br />
%% isVisible(This, Coords, [Option]) -> boolean() when<br />
%% 	This::wxGrid(), Coords::{R::integer(), C::integer()},<br />
%% 	Option :: {'wholeCellVisible', boolean()}.<br />
%% 
-doc """
Returns true if a cell is either entirely or at least partially visible in the
grid window.

By default, the cell must be entirely visible for this function to return true
but if `wholeCellVisible` is false, the function returns true even if the cell
is only partially visible.
""".
-spec isVisible(This, Row, Col) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer();
      (This, Coords, [Option]) -> boolean() when
	This::wxGrid(), Coords::{R::integer(), C::integer()},
	Option :: {'wholeCellVisible', boolean()}.

isVisible(This,Row,Col)
 when is_record(This, wx_ref),is_integer(Row),is_integer(Col) ->
  isVisible(This,Row,Col, []);
isVisible(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords, Options)
 when is_integer(CoordsR),is_integer(CoordsC),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({wholeCellVisible, _wholeCellVisible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Coords, Opts,?get_env(),?wxGrid_IsVisible_2),
  wxe_util:rec(?wxGrid_IsVisible_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridisvisible">external documentation</a>.
-doc """
Returns true if a cell is either entirely or at least partially visible in the
grid window.

By default, the cell must be entirely visible for this function to return true
but if `wholeCellVisible` is false, the function returns true even if the cell
is only partially visible.
""".
-spec isVisible(This, Row, Col, [Option]) -> boolean() when
	This::wxGrid(), Row::integer(), Col::integer(),
	Option :: {'wholeCellVisible', boolean()}.
isVisible(#wx_ref{type=ThisT}=This,Row,Col, Options)
 when is_integer(Row),is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({wholeCellVisible, _wholeCellVisible} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Row,Col, Opts,?get_env(),?wxGrid_IsVisible_3),
  wxe_util:rec(?wxGrid_IsVisible_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmakecellvisible">external documentation</a>.
-doc """
Brings the specified cell into the visible grid cell area with minimal
scrolling.

Does nothing if the cell is already visible.
""".
-spec makeCellVisible(This, Coords) -> 'ok' when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
makeCellVisible(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords)
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Coords,?get_env(),?wxGrid_MakeCellVisible_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmakecellvisible">external documentation</a>.
-doc """
Brings the specified cell into the visible grid cell area with minimal
scrolling.

Does nothing if the cell is already visible.
""".
-spec makeCellVisible(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().
makeCellVisible(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_MakeCellVisible_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursordown">external documentation</a>.
-doc """
Moves the grid cursor down by one row.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorDown(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorDown(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorDown),
  wxe_util:rec(?wxGrid_MoveCursorDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorleft">external documentation</a>.
-doc """
Moves the grid cursor left by one column.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorLeft(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorLeft(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorLeft),
  wxe_util:rec(?wxGrid_MoveCursorLeft).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorright">external documentation</a>.
-doc """
Moves the grid cursor right by one column.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorRight(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorRight(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorRight),
  wxe_util:rec(?wxGrid_MoveCursorRight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorup">external documentation</a>.
-doc """
Moves the grid cursor up by one row.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorUp(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorUp(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorUp),
  wxe_util:rec(?wxGrid_MoveCursorUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursordownblock">external documentation</a>.
-doc """
Moves the grid cursor down in the current column such that it skips to the
beginning or end of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorDownBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorDownBlock(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorDownBlock),
  wxe_util:rec(?wxGrid_MoveCursorDownBlock).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorleftblock">external documentation</a>.
-doc """
Moves the grid cursor left in the current row such that it skips to the
beginning or end of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorLeftBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorLeftBlock(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorLeftBlock),
  wxe_util:rec(?wxGrid_MoveCursorLeftBlock).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorrightblock">external documentation</a>.
-doc """
Moves the grid cursor right in the current row such that it skips to the
beginning or end of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorRightBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorRightBlock(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorRightBlock),
  wxe_util:rec(?wxGrid_MoveCursorRightBlock).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovecursorupblock">external documentation</a>.
-doc """
Moves the grid cursor up in the current column such that it skips to the
beginning or end of a block of non-empty cells.

If a block of cells was previously selected it will expand if the argument is
true or be cleared if the argument is false.
""".
-spec moveCursorUpBlock(This, ExpandSelection) -> boolean() when
	This::wxGrid(), ExpandSelection::boolean().
moveCursorUpBlock(#wx_ref{type=ThisT}=This,ExpandSelection)
 when is_boolean(ExpandSelection) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExpandSelection,?get_env(),?wxGrid_MoveCursorUpBlock),
  wxe_util:rec(?wxGrid_MoveCursorUpBlock).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovepagedown">external documentation</a>.
-doc """
Moves the grid cursor down by some number of rows so that the previous bottom
visible row becomes the top visible row.
""".
-spec movePageDown(This) -> boolean() when
	This::wxGrid().
movePageDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_MovePageDown),
  wxe_util:rec(?wxGrid_MovePageDown).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridmovepageup">external documentation</a>.
-doc """
Moves the grid cursor up by some number of rows so that the previous top visible
row becomes the bottom visible row.
""".
-spec movePageUp(This) -> boolean() when
	This::wxGrid().
movePageUp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_MovePageUp),
  wxe_util:rec(?wxGrid_MovePageUp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridregisterdatatype">external documentation</a>.
-doc """
Register a new data type.

The data types allow to naturally associate specific renderers and editors to
the cells containing values of the given type. For example, the grid
automatically registers a data type with the name `wxGRID_VALUE_STRING` which
uses `m:wxGridCellStringRenderer` and `m:wxGridCellTextEditor` as its renderer
and editor respectively - this is the data type used by all the cells of the
default `wxGridStringTable` (not implemented in wx), so this renderer and editor
are used by default for all grid cells.

However if a custom table returns `wxGRID_VALUE_BOOL` from its
`wxGridTableBase::GetTypeName()` (not implemented in wx) method, then
`m:wxGridCellBoolRenderer` and `m:wxGridCellBoolEditor` are used for it because
the grid also registers a boolean data type with this name.

And as this mechanism is completely generic, you may register your own data
types using your own custom renderers and editors. Just remember that the table
must identify a cell as being of the given type for them to be used for this
cell.
""".
-spec registerDataType(This, TypeName, Renderer, Editor) -> 'ok' when
	This::wxGrid(), TypeName::unicode:chardata(), Renderer::wxGridCellRenderer:wxGridCellRenderer(), Editor::wxGridCellEditor:wxGridCellEditor().
registerDataType(#wx_ref{type=ThisT}=This,TypeName,#wx_ref{type=RendererT}=Renderer,#wx_ref{type=EditorT}=Editor)
 when ?is_chardata(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary(TypeName),
  ?CLASS(RendererT,wxGridCellRenderer),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:queue_cmd(This,TypeName_UC,Renderer,Editor,?get_env(),?wxGrid_RegisterDataType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsaveeditcontrolvalue">external documentation</a>.
-doc """
Sets the value of the current grid cell to the current in-place edit control
value.

This is called automatically when the grid cursor moves from the current cell to
a new cell. It is also a good idea to call this function when closing a grid
since any edits to the final cell location will not be saved otherwise.
""".
-spec saveEditControlValue(This) -> 'ok' when
	This::wxGrid().
saveEditControlValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_SaveEditControlValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectall">external documentation</a>.
-doc "Selects all cells in the grid.".
-spec selectAll(This) -> 'ok' when
	This::wxGrid().
selectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_SelectAll).

%% @equiv selectBlock(This,TopLeft,BottomRight, [])
-spec selectBlock(This, TopLeft, BottomRight) -> 'ok' when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()}.

selectBlock(This,{TopLeftR,TopLeftC} = TopLeft,{BottomRightR,BottomRightC} = BottomRight)
 when is_record(This, wx_ref),is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC) ->
  selectBlock(This,TopLeft,BottomRight, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectblock">external documentation</a>.
-doc """
Selects a rectangular block of cells.

If `addToSelected` is false then any existing selection will be deselected; if
true the column will be added to the existing selection.
""".
-spec selectBlock(This, TopLeft, BottomRight, [Option]) -> 'ok' when
	This::wxGrid(), TopLeft::{R::integer(), C::integer()}, BottomRight::{R::integer(), C::integer()},
	Option :: {'addToSelected', boolean()}.
selectBlock(#wx_ref{type=ThisT}=This,{TopLeftR,TopLeftC} = TopLeft,{BottomRightR,BottomRightC} = BottomRight, Options)
 when is_integer(TopLeftR),is_integer(TopLeftC),is_integer(BottomRightR),is_integer(BottomRightC),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, _addToSelected} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,TopLeft,BottomRight, Opts,?get_env(),?wxGrid_SelectBlock_3).

%% @equiv selectBlock(This,TopRow,LeftCol,BottomRow,RightCol, [])
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol) -> 'ok' when
	This::wxGrid(), TopRow::integer(), LeftCol::integer(), BottomRow::integer(), RightCol::integer().

selectBlock(This,TopRow,LeftCol,BottomRow,RightCol)
 when is_record(This, wx_ref),is_integer(TopRow),is_integer(LeftCol),is_integer(BottomRow),is_integer(RightCol) ->
  selectBlock(This,TopRow,LeftCol,BottomRow,RightCol, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectblock">external documentation</a>.
-doc """
Selects a rectangular block of cells.

If `addToSelected` is false then any existing selection will be deselected; if
true the column will be added to the existing selection.
""".
-spec selectBlock(This, TopRow, LeftCol, BottomRow, RightCol, [Option]) -> 'ok' when
	This::wxGrid(), TopRow::integer(), LeftCol::integer(), BottomRow::integer(), RightCol::integer(),
	Option :: {'addToSelected', boolean()}.
selectBlock(#wx_ref{type=ThisT}=This,TopRow,LeftCol,BottomRow,RightCol, Options)
 when is_integer(TopRow),is_integer(LeftCol),is_integer(BottomRow),is_integer(RightCol),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, _addToSelected} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,TopRow,LeftCol,BottomRow,RightCol, Opts,?get_env(),?wxGrid_SelectBlock_5).

%% @equiv selectCol(This,Col, [])
-spec selectCol(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

selectCol(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  selectCol(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectcol">external documentation</a>.
-doc """
Selects the specified column.

If `addToSelected` is false then any existing selection will be deselected; if
true the column will be added to the existing selection.

This method won't select anything if the current selection mode is
wxGridSelectRows.
""".
-spec selectCol(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'addToSelected', boolean()}.
selectCol(#wx_ref{type=ThisT}=This,Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, _addToSelected} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Col, Opts,?get_env(),?wxGrid_SelectCol).

%% @equiv selectRow(This,Row, [])
-spec selectRow(This, Row) -> 'ok' when
	This::wxGrid(), Row::integer().

selectRow(This,Row)
 when is_record(This, wx_ref),is_integer(Row) ->
  selectRow(This,Row, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridselectrow">external documentation</a>.
-doc """
Selects the specified row.

If `addToSelected` is false then any existing selection will be deselected; if
true the row will be added to the existing selection.

This method won't select anything if the current selection mode is
wxGridSelectColumns.
""".
-spec selectRow(This, Row, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(),
	Option :: {'addToSelected', boolean()}.
selectRow(#wx_ref{type=ThisT}=This,Row, Options)
 when is_integer(Row),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({addToSelected, _addToSelected} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Row, Opts,?get_env(),?wxGrid_SelectRow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellalignment">external documentation</a>.
-doc """
Sets the horizontal and vertical alignment for grid cell text at the specified
location.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`.

Vertical alignment should be one of `wxALIGN_TOP`, `wxALIGN_CENTRE` or
`wxALIGN_BOTTOM`.
""".
-spec setCellAlignment(This, Row, Col, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Horiz::integer(), Vert::integer().
setCellAlignment(#wx_ref{type=ThisT}=This,Row,Col,Horiz,Vert)
 when is_integer(Row),is_integer(Col),is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,Horiz,Vert,?get_env(),?wxGrid_SetCellAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellbackgroundcolour">external documentation</a>.
-doc "Set the background colour for the given cell or all cells by default.".
-spec setCellBackgroundColour(This, Row, Col, Colour) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Colour::wx:wx_colour().
setCellBackgroundColour(#wx_ref{type=ThisT}=This,Row,Col,Colour)
 when is_integer(Row),is_integer(Col),?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,wxe_util:color(Colour),?get_env(),?wxGrid_SetCellBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcelleditor">external documentation</a>.
-doc """
Sets the editor for the grid cell at the specified location.

The grid will take ownership of the pointer.

See `m:wxGridCellEditor` and the overview_grid for more information about cell
editors and renderers.
""".
-spec setCellEditor(This, Row, Col, Editor) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Editor::wxGridCellEditor:wxGridCellEditor().
setCellEditor(#wx_ref{type=ThisT}=This,Row,Col,#wx_ref{type=EditorT}=Editor)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:queue_cmd(This,Row,Col,Editor,?get_env(),?wxGrid_SetCellEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellfont">external documentation</a>.
-doc "Sets the font for text in the grid cell at the specified location.".
-spec setCellFont(This, Row, Col, Font) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Font::wxFont:wxFont().
setCellFont(#wx_ref{type=ThisT}=This,Row,Col,#wx_ref{type=FontT}=Font)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Row,Col,Font,?get_env(),?wxGrid_SetCellFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellrenderer">external documentation</a>.
-doc """
Sets the renderer for the grid cell at the specified location.

The grid will take ownership of the pointer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell
editors and renderers.
""".
-spec setCellRenderer(This, Row, Col, Renderer) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setCellRenderer(#wx_ref{type=ThisT}=This,Row,Col,#wx_ref{type=RendererT}=Renderer)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:queue_cmd(This,Row,Col,Renderer,?get_env(),?wxGrid_SetCellRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcelltextcolour">external documentation</a>.
-doc "Sets the text colour for the given cell.".
-spec setCellTextColour(This, Row, Col, Colour) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), Colour::wx:wx_colour().
setCellTextColour(#wx_ref{type=ThisT}=This,Row,Col,Colour)
 when is_integer(Row),is_integer(Col),?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,wxe_util:color(Colour),?get_env(),?wxGrid_SetCellTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellvalue">external documentation</a>.
-doc """
Sets the string value for the cell at the specified location.

For simple applications where a grid object automatically uses a default grid
table of string values you use this function together with `getCellValue/3` to
access cell values. For more complex applications where you have derived your
own grid table class that contains various data types (e.g. numeric, boolean or
user-defined custom types) then you only use this function for those cells that
contain string values.

See `wxGridTableBase::CanSetValueAs()` (not implemented in wx) and the
overview_grid for more information.
""".
-spec setCellValue(This, Coords, S) -> 'ok' when
	This::wxGrid(), Coords::{R::integer(), C::integer()}, S::unicode:chardata().
setCellValue(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords,S)
 when is_integer(CoordsR),is_integer(CoordsC),?is_chardata(S) ->
  ?CLASS(ThisT,wxGrid),
  S_UC = unicode:characters_to_binary(S),
  wxe_util:queue_cmd(This,Coords,S_UC,?get_env(),?wxGrid_SetCellValue_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcellvalue">external documentation</a>.
-doc """
Sets the string value for the cell at the specified location.

For simple applications where a grid object automatically uses a default grid
table of string values you use this function together with `getCellValue/3` to
access cell values. For more complex applications where you have derived your
own grid table class that contains various data types (e.g. numeric, boolean or
user-defined custom types) then you only use this function for those cells that
contain string values.

See `wxGridTableBase::CanSetValueAs()` (not implemented in wx) and the
overview_grid for more information.
""".
-spec setCellValue(This, Row, Col, S) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(), S::unicode:chardata().
setCellValue(#wx_ref{type=ThisT}=This,Row,Col,S)
 when is_integer(Row),is_integer(Col),?is_chardata(S) ->
  ?CLASS(ThisT,wxGrid),
  S_UC = unicode:characters_to_binary(S),
  wxe_util:queue_cmd(This,Row,Col,S_UC,?get_env(),?wxGrid_SetCellValue_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolattr">external documentation</a>.
-doc """
Sets the cell attributes for all cells in the specified column.

For more information about controlling grid cell attributes see the
`m:wxGridCellAttr` cell attribute class and the overview_grid.
""".
-spec setColAttr(This, Col, Attr) -> 'ok' when
	This::wxGrid(), Col::integer(), Attr::wxGridCellAttr:wxGridCellAttr().
setColAttr(#wx_ref{type=ThisT}=This,Col,#wx_ref{type=AttrT}=Attr)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:queue_cmd(This,Col,Attr,?get_env(),?wxGrid_SetColAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatbool">external documentation</a>.
-doc """
Sets the specified column to display boolean values.

See: `setColFormatCustom/3`
""".
-spec setColFormatBool(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().
setColFormatBool(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxGrid_SetColFormatBool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatnumber">external documentation</a>.
-doc """
Sets the specified column to display integer values.

See: `setColFormatCustom/3`
""".
-spec setColFormatNumber(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().
setColFormatNumber(#wx_ref{type=ThisT}=This,Col)
 when is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,?get_env(),?wxGrid_SetColFormatNumber).

%% @equiv setColFormatFloat(This,Col, [])
-spec setColFormatFloat(This, Col) -> 'ok' when
	This::wxGrid(), Col::integer().

setColFormatFloat(This,Col)
 when is_record(This, wx_ref),is_integer(Col) ->
  setColFormatFloat(This,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatfloat">external documentation</a>.
-doc """
Sets the specified column to display floating point values with the given width
and precision.

See: `setColFormatCustom/3`
""".
-spec setColFormatFloat(This, Col, [Option]) -> 'ok' when
	This::wxGrid(), Col::integer(),
	Option :: {'width', integer()}
		 | {'precision', integer()}.
setColFormatFloat(#wx_ref{type=ThisT}=This,Col, Options)
 when is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({width, _width} = Arg) -> Arg;
          ({precision, _precision} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Col, Opts,?get_env(),?wxGrid_SetColFormatFloat).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolformatcustom">external documentation</a>.
-doc """
Sets the specified column to display data in a custom format.

This method provides an alternative to defining a custom grid table which would
return `typeName` from its GetTypeName() method for the cells in this column:
while it doesn't really change the type of the cells in this column, it does
associate the renderer and editor used for the cells of the specified type with
them.

See the overview_grid for more information on working with custom data types.
""".
-spec setColFormatCustom(This, Col, TypeName) -> 'ok' when
	This::wxGrid(), Col::integer(), TypeName::unicode:chardata().
setColFormatCustom(#wx_ref{type=ThisT}=This,Col,TypeName)
 when is_integer(Col),?is_chardata(TypeName) ->
  ?CLASS(ThisT,wxGrid),
  TypeName_UC = unicode:characters_to_binary(TypeName),
  wxe_util:queue_cmd(This,Col,TypeName_UC,?get_env(),?wxGrid_SetColFormatCustom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelalignment">external documentation</a>.
-doc """
Sets the horizontal and vertical alignment of column label text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`. Vertical alignment should be one of `wxALIGN_TOP`,
`wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.
""".
-spec setColLabelAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setColLabelAlignment(#wx_ref{type=ThisT}=This,Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Horiz,Vert,?get_env(),?wxGrid_SetColLabelAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelsize">external documentation</a>.
-doc """
Sets the height of the column labels.

If `height` equals to `wxGRID_AUTOSIZE` then height is calculated automatically
so that no label is truncated. Note that this could be slow for a large table.
""".
-spec setColLabelSize(This, Height) -> 'ok' when
	This::wxGrid(), Height::integer().
setColLabelSize(#wx_ref{type=ThisT}=This,Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Height,?get_env(),?wxGrid_SetColLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcollabelvalue">external documentation</a>.
-doc """
Set the value for the given column label.

If you are using a custom grid table you must override
`wxGridTableBase::SetColLabelValue()` (not implemented in wx) for this to have
any effect.
""".
-spec setColLabelValue(This, Col, Value) -> 'ok' when
	This::wxGrid(), Col::integer(), Value::unicode:chardata().
setColLabelValue(#wx_ref{type=ThisT}=This,Col,Value)
 when is_integer(Col),?is_chardata(Value) ->
  ?CLASS(ThisT,wxGrid),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Col,Value_UC,?get_env(),?wxGrid_SetColLabelValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolminimalwidth">external documentation</a>.
-doc """
Sets the minimal `width` for the specified column `col`.

It is usually best to call this method during grid creation as calling it later
will not resize the column to the given minimal width even if it is currently
narrower than it.

`width` must be greater than the minimal acceptable column width as returned by
`getColMinimalAcceptableWidth/1`.
""".
-spec setColMinimalWidth(This, Col, Width) -> 'ok' when
	This::wxGrid(), Col::integer(), Width::integer().
setColMinimalWidth(#wx_ref{type=ThisT}=This,Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,Width,?get_env(),?wxGrid_SetColMinimalWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolminimalacceptablewidth">external documentation</a>.
-doc """
Sets the minimal `width` to which the user can resize columns.

See: `getColMinimalAcceptableWidth/1`
""".
-spec setColMinimalAcceptableWidth(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().
setColMinimalAcceptableWidth(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxGrid_SetColMinimalAcceptableWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetcolsize">external documentation</a>.
-doc "Sets the width of the specified column.".
-spec setColSize(This, Col, Width) -> 'ok' when
	This::wxGrid(), Col::integer(), Width::integer().
setColSize(#wx_ref{type=ThisT}=This,Col,Width)
 when is_integer(Col),is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Col,Width,?get_env(),?wxGrid_SetColSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellalignment">external documentation</a>.
-doc """
Sets the default horizontal and vertical alignment for grid cell text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`. Vertical alignment should be one of `wxALIGN_TOP`,
`wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.
""".
-spec setDefaultCellAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setDefaultCellAlignment(#wx_ref{type=ThisT}=This,Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Horiz,Vert,?get_env(),?wxGrid_SetDefaultCellAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellbackgroundcolour">external documentation</a>.
-doc "Sets the default background colour for grid cells.".
-spec setDefaultCellBackgroundColour(This, Colour) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour().
setDefaultCellBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxGrid_SetDefaultCellBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcellfont">external documentation</a>.
-doc "Sets the default font to be used for grid cell text.".
-spec setDefaultCellFont(This, Font) -> 'ok' when
	This::wxGrid(), Font::wxFont:wxFont().
setDefaultCellFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxGrid_SetDefaultCellFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcelltextcolour">external documentation</a>.
-doc "Sets the current default colour for grid cell text.".
-spec setDefaultCellTextColour(This, Colour) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour().
setDefaultCellTextColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxGrid_SetDefaultCellTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaulteditor">external documentation</a>.
-doc """
Sets the default editor for grid cells.

The grid will take ownership of the pointer.

See `m:wxGridCellEditor` and the overview_grid for more information about cell
editors and renderers.
""".
-spec setDefaultEditor(This, Editor) -> 'ok' when
	This::wxGrid(), Editor::wxGridCellEditor:wxGridCellEditor().
setDefaultEditor(#wx_ref{type=ThisT}=This,#wx_ref{type=EditorT}=Editor) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(EditorT,wxGridCellEditor),
  wxe_util:queue_cmd(This,Editor,?get_env(),?wxGrid_SetDefaultEditor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultrenderer">external documentation</a>.
-doc """
Sets the default renderer for grid cells.

The grid will take ownership of the pointer.

See `m:wxGridCellRenderer` and the overview_grid for more information about cell
editors and renderers.
""".
-spec setDefaultRenderer(This, Renderer) -> 'ok' when
	This::wxGrid(), Renderer::wxGridCellRenderer:wxGridCellRenderer().
setDefaultRenderer(#wx_ref{type=ThisT}=This,#wx_ref{type=RendererT}=Renderer) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(RendererT,wxGridCellRenderer),
  wxe_util:queue_cmd(This,Renderer,?get_env(),?wxGrid_SetDefaultRenderer).

%% @equiv setDefaultColSize(This,Width, [])
-spec setDefaultColSize(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().

setDefaultColSize(This,Width)
 when is_record(This, wx_ref),is_integer(Width) ->
  setDefaultColSize(This,Width, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultcolsize">external documentation</a>.
-doc """
Sets the default width for columns in the grid.

This will only affect columns subsequently added to the grid unless
`resizeExistingCols` is true.

If `width` is less than `getColMinimalAcceptableWidth/1`, then the minimal
acceptable width is used instead of it.
""".
-spec setDefaultColSize(This, Width, [Option]) -> 'ok' when
	This::wxGrid(), Width::integer(),
	Option :: {'resizeExistingCols', boolean()}.
setDefaultColSize(#wx_ref{type=ThisT}=This,Width, Options)
 when is_integer(Width),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({resizeExistingCols, _resizeExistingCols} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width, Opts,?get_env(),?wxGrid_SetDefaultColSize).

%% @equiv setDefaultRowSize(This,Height, [])
-spec setDefaultRowSize(This, Height) -> 'ok' when
	This::wxGrid(), Height::integer().

setDefaultRowSize(This,Height)
 when is_record(This, wx_ref),is_integer(Height) ->
  setDefaultRowSize(This,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetdefaultrowsize">external documentation</a>.
-doc """
Sets the default height for rows in the grid.

This will only affect rows subsequently added to the grid unless
`resizeExistingRows` is true.

If `height` is less than `getRowMinimalAcceptableHeight/1`, then the minimal
acceptable height is used instead of it.
""".
-spec setDefaultRowSize(This, Height, [Option]) -> 'ok' when
	This::wxGrid(), Height::integer(),
	Option :: {'resizeExistingRows', boolean()}.
setDefaultRowSize(#wx_ref{type=ThisT}=This,Height, Options)
 when is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({resizeExistingRows, _resizeExistingRows} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Height, Opts,?get_env(),?wxGrid_SetDefaultRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetgridcursor">external documentation</a>.
-doc """
Set the grid cursor to the specified cell.

The grid cursor indicates the current cell and can be moved by the user using
the arrow keys or the mouse.

Calling this function generates a `wxEVT_GRID_SELECT_CELL` event and if the
event handler vetoes this event, the cursor is not moved.

This function doesn't make the target call visible, use `GoToCell()` (not
implemented in wx) to do this.
""".
-spec setGridCursor(This, Coords) -> 'ok' when
	This::wxGrid(), Coords::{R::integer(), C::integer()}.
setGridCursor(#wx_ref{type=ThisT}=This,{CoordsR,CoordsC} = Coords)
 when is_integer(CoordsR),is_integer(CoordsC) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Coords,?get_env(),?wxGrid_SetGridCursor_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetgridcursor">external documentation</a>.
-doc """
Set the grid cursor to the specified cell.

The grid cursor indicates the current cell and can be moved by the user using
the arrow keys or the mouse.

Calling this function generates a `wxEVT_GRID_SELECT_CELL` event and if the
event handler vetoes this event, the cursor is not moved.

This function doesn't make the target call visible, use `GoToCell()` (not
implemented in wx) to do this.
""".
-spec setGridCursor(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().
setGridCursor(#wx_ref{type=ThisT}=This,Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Col,?get_env(),?wxGrid_SetGridCursor_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetgridlinecolour">external documentation</a>.
-doc "Sets the colour used to draw grid lines.".
-spec setGridLineColour(This, Colour) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour().
setGridLineColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxGrid_SetGridLineColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabelbackgroundcolour">external documentation</a>.
-doc "Sets the background colour for row and column labels.".
-spec setLabelBackgroundColour(This, Colour) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour().
setLabelBackgroundColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxGrid_SetLabelBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabelfont">external documentation</a>.
-doc "Sets the font for row and column labels.".
-spec setLabelFont(This, Font) -> 'ok' when
	This::wxGrid(), Font::wxFont:wxFont().
setLabelFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxGrid_SetLabelFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetlabeltextcolour">external documentation</a>.
-doc "Sets the colour for row and column label text.".
-spec setLabelTextColour(This, Colour) -> 'ok' when
	This::wxGrid(), Colour::wx:wx_colour().
setLabelTextColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxGrid_SetLabelTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetmargins">external documentation</a>.
-doc """
Sets the extra margins used around the grid area.

A grid may occupy more space than needed for its data display and this function
allows setting how big this extra space is
""".
-spec setMargins(This, ExtraWidth, ExtraHeight) -> 'ok' when
	This::wxGrid(), ExtraWidth::integer(), ExtraHeight::integer().
setMargins(#wx_ref{type=ThisT}=This,ExtraWidth,ExtraHeight)
 when is_integer(ExtraWidth),is_integer(ExtraHeight) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,ExtraWidth,ExtraHeight,?get_env(),?wxGrid_SetMargins).

%% @equiv setReadOnly(This,Row,Col, [])
-spec setReadOnly(This, Row, Col) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer().

setReadOnly(This,Row,Col)
 when is_record(This, wx_ref),is_integer(Row),is_integer(Col) ->
  setReadOnly(This,Row,Col, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetreadonly">external documentation</a>.
-doc """
Makes the cell at the specified location read-only or editable.

See: `isReadOnly/3`
""".
-spec setReadOnly(This, Row, Col, [Option]) -> 'ok' when
	This::wxGrid(), Row::integer(), Col::integer(),
	Option :: {'isReadOnly', boolean()}.
setReadOnly(#wx_ref{type=ThisT}=This,Row,Col, Options)
 when is_integer(Row),is_integer(Col),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({isReadOnly, _isReadOnly} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Row,Col, Opts,?get_env(),?wxGrid_SetReadOnly).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowattr">external documentation</a>.
-doc """
Sets the cell attributes for all cells in the specified row.

The grid takes ownership of the attribute pointer.

See the `m:wxGridCellAttr` class for more information about controlling cell
attributes.
""".
-spec setRowAttr(This, Row, Attr) -> 'ok' when
	This::wxGrid(), Row::integer(), Attr::wxGridCellAttr:wxGridCellAttr().
setRowAttr(#wx_ref{type=ThisT}=This,Row,#wx_ref{type=AttrT}=Attr)
 when is_integer(Row) ->
  ?CLASS(ThisT,wxGrid),
  ?CLASS(AttrT,wxGridCellAttr),
  wxe_util:queue_cmd(This,Row,Attr,?get_env(),?wxGrid_SetRowAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelalignment">external documentation</a>.
-doc """
Sets the horizontal and vertical alignment of row label text.

Horizontal alignment should be one of `wxALIGN_LEFT`, `wxALIGN_CENTRE` or
`wxALIGN_RIGHT`. Vertical alignment should be one of `wxALIGN_TOP`,
`wxALIGN_CENTRE` or `wxALIGN_BOTTOM`.
""".
-spec setRowLabelAlignment(This, Horiz, Vert) -> 'ok' when
	This::wxGrid(), Horiz::integer(), Vert::integer().
setRowLabelAlignment(#wx_ref{type=ThisT}=This,Horiz,Vert)
 when is_integer(Horiz),is_integer(Vert) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Horiz,Vert,?get_env(),?wxGrid_SetRowLabelAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelsize">external documentation</a>.
-doc """
Sets the width of the row labels.

If `width` equals `wxGRID_AUTOSIZE` then width is calculated automatically so
that no label is truncated. Note that this could be slow for a large table.
""".
-spec setRowLabelSize(This, Width) -> 'ok' when
	This::wxGrid(), Width::integer().
setRowLabelSize(#wx_ref{type=ThisT}=This,Width)
 when is_integer(Width) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Width,?get_env(),?wxGrid_SetRowLabelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowlabelvalue">external documentation</a>.
-doc """
Sets the value for the given row label.

If you are using a derived grid table you must override
`wxGridTableBase::SetRowLabelValue()` (not implemented in wx) for this to have
any effect.
""".
-spec setRowLabelValue(This, Row, Value) -> 'ok' when
	This::wxGrid(), Row::integer(), Value::unicode:chardata().
setRowLabelValue(#wx_ref{type=ThisT}=This,Row,Value)
 when is_integer(Row),?is_chardata(Value) ->
  ?CLASS(ThisT,wxGrid),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Row,Value_UC,?get_env(),?wxGrid_SetRowLabelValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowminimalheight">external documentation</a>.
-doc """
Sets the minimal `height` for the specified `row`.

See `setColMinimalWidth/3` for more information.
""".
-spec setRowMinimalHeight(This, Row, Height) -> 'ok' when
	This::wxGrid(), Row::integer(), Height::integer().
setRowMinimalHeight(#wx_ref{type=ThisT}=This,Row,Height)
 when is_integer(Row),is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Height,?get_env(),?wxGrid_SetRowMinimalHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowminimalacceptableheight">external documentation</a>.
-doc """
Sets the minimal row `height` used by default.

See `setColMinimalAcceptableWidth/2` for more information.
""".
-spec setRowMinimalAcceptableHeight(This, Height) -> 'ok' when
	This::wxGrid(), Height::integer().
setRowMinimalAcceptableHeight(#wx_ref{type=ThisT}=This,Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Height,?get_env(),?wxGrid_SetRowMinimalAcceptableHeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetrowsize">external documentation</a>.
-doc """
Sets the height of the specified row.

See `setColSize/3` for more information.
""".
-spec setRowSize(This, Row, Height) -> 'ok' when
	This::wxGrid(), Row::integer(), Height::integer().
setRowSize(#wx_ref{type=ThisT}=This,Row,Height)
 when is_integer(Row),is_integer(Height) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Row,Height,?get_env(),?wxGrid_SetRowSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetscrolllinex">external documentation</a>.
-doc """
Sets the number of pixels per horizontal scroll increment.

The default is 15.

See: `getScrollLineX/1`, `getScrollLineY/1`, `setScrollLineY/2`
""".
-spec setScrollLineX(This, X) -> 'ok' when
	This::wxGrid(), X::integer().
setScrollLineX(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,X,?get_env(),?wxGrid_SetScrollLineX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetscrollliney">external documentation</a>.
-doc """
Sets the number of pixels per vertical scroll increment.

The default is 15.

See: `getScrollLineX/1`, `getScrollLineY/1`, `setScrollLineX/2`
""".
-spec setScrollLineY(This, Y) -> 'ok' when
	This::wxGrid(), Y::integer().
setScrollLineY(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxGrid_SetScrollLineY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionbackground">external documentation</a>.
-doc "Set the colour to be used for drawing the selection background.".
-spec setSelectionBackground(This, C) -> 'ok' when
	This::wxGrid(), C::wx:wx_colour().
setSelectionBackground(#wx_ref{type=ThisT}=This,C)
 when ?is_colordata(C) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(C),?get_env(),?wxGrid_SetSelectionBackground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionforeground">external documentation</a>.
-doc "Set the colour to be used for drawing the selection foreground.".
-spec setSelectionForeground(This, C) -> 'ok' when
	This::wxGrid(), C::wx:wx_colour().
setSelectionForeground(#wx_ref{type=ThisT}=This,C)
 when ?is_colordata(C) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,wxe_util:color(C),?get_env(),?wxGrid_SetSelectionForeground).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridsetselectionmode">external documentation</a>.
%%<br /> Selmode = ?wxGrid_wxGridSelectCells | ?wxGrid_wxGridSelectRows | ?wxGrid_wxGridSelectColumns | ?wxGrid_wxGridSelectRowsOrColumns
-doc """
Set the selection behaviour of the grid.

The existing selection is converted to conform to the new mode if possible and
discarded otherwise (e.g. any individual selected cells are deselected if the
new mode allows only the selection of the entire rows or columns).
""".
-spec setSelectionMode(This, Selmode) -> 'ok' when
	This::wxGrid(), Selmode::wx:wx_enum().
setSelectionMode(#wx_ref{type=ThisT}=This,Selmode)
 when is_integer(Selmode) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Selmode,?get_env(),?wxGrid_SetSelectionMode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridshowcelleditcontrol">external documentation</a>.
-doc """
Displays the active in-place cell edit control for the current cell after it was
hidden.

This method should only be called after calling `hideCellEditControl/1`, to
start editing the current grid cell use `enableCellEditControl/2` instead.
""".
-spec showCellEditControl(This) -> 'ok' when
	This::wxGrid().
showCellEditControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,?get_env(),?wxGrid_ShowCellEditControl).

%% @equiv xToCol(This,X, [])
-spec xToCol(This, X) -> integer() when
	This::wxGrid(), X::integer().

xToCol(This,X)
 when is_record(This, wx_ref),is_integer(X) ->
  xToCol(This,X, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridxtocol">external documentation</a>.
-doc """
Returns the column at the given pixel position depending on the window.

Return: The column index or `wxNOT_FOUND`.
""".
-spec xToCol(This, X, [Option]) -> integer() when
	This::wxGrid(), X::integer(),
	Option :: {'clipToMinMax', boolean()}.
xToCol(#wx_ref{type=ThisT}=This,X, Options)
 when is_integer(X),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({clipToMinMax, _clipToMinMax} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X, Opts,?get_env(),?wxGrid_XToCol),
  wxe_util:rec(?wxGrid_XToCol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridxtoedgeofcol">external documentation</a>.
-doc """
Returns the column whose right hand edge is close to the given logical `x`
position.

If no column edge is near to this position `wxNOT_FOUND` is returned.
""".
-spec xToEdgeOfCol(This, X) -> integer() when
	This::wxGrid(), X::integer().
xToEdgeOfCol(#wx_ref{type=ThisT}=This,X)
 when is_integer(X) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,X,?get_env(),?wxGrid_XToEdgeOfCol),
  wxe_util:rec(?wxGrid_XToEdgeOfCol).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridytoedgeofrow">external documentation</a>.
-doc """
Returns the row whose bottom edge is close to the given logical `y` position.

If no row edge is near to this position `wxNOT_FOUND` is returned.
""".
-spec yToEdgeOfRow(This, Y) -> integer() when
	This::wxGrid(), Y::integer().
yToEdgeOfRow(#wx_ref{type=ThisT}=This,Y)
 when is_integer(Y) ->
  ?CLASS(ThisT,wxGrid),
  wxe_util:queue_cmd(This,Y,?get_env(),?wxGrid_YToEdgeOfRow),
  wxe_util:rec(?wxGrid_YToEdgeOfRow).

%% @equiv yToRow(This,Y, [])
-spec yToRow(This, Y) -> integer() when
	This::wxGrid(), Y::integer().

yToRow(This,Y)
 when is_record(This, wx_ref),is_integer(Y) ->
  yToRow(This,Y, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgrid.html#wxgridytorow">external documentation</a>.
-doc """
Returns the grid row that corresponds to the logical `y` coordinate.

The parameter `gridWindow` is new since wxWidgets 3.1.3. If it is specified,
i.e. non-NULL, only the cells of this window are considered, i.e. the function
returns `wxNOT_FOUND` if `y` is out of bounds.

If `gridWindow` is NULL, the function returns `wxNOT_FOUND` only if there is no
row at all at the `y` position.
""".
-spec yToRow(This, Y, [Option]) -> integer() when
	This::wxGrid(), Y::integer(),
	Option :: {'clipToMinMax', boolean()}.
yToRow(#wx_ref{type=ThisT}=This,Y, Options)
 when is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxGrid),
  MOpts = fun({clipToMinMax, _clipToMinMax} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Y, Opts,?get_env(),?wxGrid_YToRow),
  wxe_util:rec(?wxGrid_YToRow).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

This will also destroy the associated grid table unless you passed a table
object to the grid and specified that the grid should not take ownership of the
table (see `SetTable()` (not implemented in wx)).
""".
-spec destroy(This::wxGrid()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGrid),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxScrolledWindow
%% @hidden
-doc false.
setTargetWindow(This,Window) -> wxScrolledWindow:setTargetWindow(This,Window).
%% @hidden
-doc false.
setScrollRate(This,Xstep,Ystep) -> wxScrolledWindow:setScrollRate(This,Xstep,Ystep).
%% @hidden
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options).
%% @hidden
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY).
%% @hidden
-doc false.
scroll(This,X,Y) -> wxScrolledWindow:scroll(This,X,Y).
%% @hidden
-doc false.
scroll(This,Pt) -> wxScrolledWindow:scroll(This,Pt).
%% @hidden
-doc false.
prepareDC(This,Dc) -> wxScrolledWindow:prepareDC(This,Dc).
%% @hidden
-doc false.
doPrepareDC(This,Dc) -> wxScrolledWindow:doPrepareDC(This,Dc).
%% @hidden
-doc false.
getViewStart(This) -> wxScrolledWindow:getViewStart(This).
%% @hidden
-doc false.
getScrollPixelsPerUnit(This) -> wxScrolledWindow:getScrollPixelsPerUnit(This).
%% @hidden
-doc false.
enableScrolling(This,XScrolling,YScrolling) -> wxScrolledWindow:enableScrolling(This,XScrolling,YScrolling).
%% @hidden
-doc false.
calcUnscrolledPosition(This,X,Y) -> wxScrolledWindow:calcUnscrolledPosition(This,X,Y).
%% @hidden
-doc false.
calcUnscrolledPosition(This,Pt) -> wxScrolledWindow:calcUnscrolledPosition(This,Pt).
%% @hidden
-doc false.
calcScrolledPosition(This,X,Y) -> wxScrolledWindow:calcScrolledPosition(This,X,Y).
%% @hidden
-doc false.
calcScrolledPosition(This,Pt) -> wxScrolledWindow:calcScrolledPosition(This,Pt).
 %% From wxPanel
%% @hidden
-doc false.
setFocusIgnoringChildren(This) -> wxPanel:setFocusIgnoringChildren(This).
%% @hidden
-doc false.
initDialog(This) -> wxPanel:initDialog(This).
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
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
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
getLabel(This) -> wxWindow:getLabel(This).
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
