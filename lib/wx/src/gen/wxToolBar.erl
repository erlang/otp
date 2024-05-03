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

-module(wxToolBar).
-moduledoc """
Functions for wxToolBar class

A toolbar is a bar of buttons and/or other controls usually placed below the
menu bar in a `m:wxFrame`.

You may create a toolbar that is managed by a frame calling
`wxFrame:createToolBar/2`. Under Pocket PC, you should always use this function
for creating the toolbar to be managed by the frame, so that wxWidgets can use a
combined menubar and toolbar. Where you manage your own toolbars, create
`m:wxToolBar` as usual.

There are several different types of tools you can add to a toolbar. These types
are controlled by the ?wxItemKind enumeration.

Note that many methods in `m:wxToolBar` such as `addTool/6` return a
`wxToolBarToolBase*` object. This should be regarded as an opaque handle
representing the newly added toolbar item, providing access to its id and
position within the toolbar. Changes to the item's state should be made through
calls to `m:wxToolBar` methods, for example `enableTool/3`. Calls to
`wxToolBarToolBase` (not implemented in wx) methods (undocumented by purpose)
will not change the visible state of the item within the tool bar.

After you have added all the tools you need, you must call `realize/1` to
effectively construct and display the toolbar.

`wxMSW note`: Note that under wxMSW toolbar paints tools to reflect system-wide
colours. If you use more than 16 colours in your tool bitmaps, you may wish to
suppress this behaviour, otherwise system colours in your bitmaps will
inadvertently be mapped to system colours. To do this, set the msw.remap system
option before creating the toolbar: If you wish to use 32-bit images (which
include an alpha channel for transparency) use: Then colour remapping is
switched off, and a transparent background used. But only use this option under
Windows XP with true colour:

Styles

This class supports the following styles:

See:
[Overview toolbar](https://docs.wxwidgets.org/3.1/overview_toolbar.html#overview_toolbar)

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxToolBar](https://docs.wxwidgets.org/3.1/classwx_tool_bar.html)

## Events

Event types emitted from this class:
[`command_tool_rclicked`](`m:wxCommandEvent`),
[`command_tool_enter`](`m:wxCommandEvent`),
[`tool_dropdown`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([addCheckTool/4,addCheckTool/5,addControl/2,addControl/3,addRadioTool/4,
  addRadioTool/5,addSeparator/1,addStretchableSpace/1,addTool/2,addTool/4,
  addTool/5,addTool/6,deleteTool/2,deleteToolByPos/2,enableTool/3,findById/2,
  findControl/2,findToolForPosition/3,getMargins/1,getToolBitmapSize/1,
  getToolEnabled/2,getToolLongHelp/2,getToolPacking/1,getToolPos/2,
  getToolSeparation/1,getToolShortHelp/2,getToolSize/1,getToolState/2,
  insertControl/3,insertControl/4,insertSeparator/2,insertStretchableSpace/2,
  insertTool/3,insertTool/5,insertTool/6,realize/1,removeTool/2,setMargins/3,
  setToolBitmapSize/2,setToolLongHelp/3,setToolPacking/2,setToolSeparation/2,
  setToolShortHelp/3,toggleTool/3]).

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

-type wxToolBar() :: wx:wx_object().
-export_type([wxToolBar/0]).
%% @hidden
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv addControl(This,Control, [])
-spec addControl(This, Control) -> wx:wx_object() when
	This::wxToolBar(), Control::wxControl:wxControl().

addControl(This,Control)
 when is_record(This, wx_ref),is_record(Control, wx_ref) ->
  addControl(This,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddcontrol">external documentation</a>.
-doc """
Adds any control to the toolbar, typically e.g. a `m:wxComboBox`.

Remark: wxMac: labels are only displayed if wxWidgets is built with
`wxMAC_USE_NATIVE_TOOLBAR` set to 1
""".
-spec addControl(This, Control, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Control::wxControl:wxControl(),
	Option :: {'label', unicode:chardata()}.
addControl(#wx_ref{type=ThisT}=This,#wx_ref{type=ControlT}=Control, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Control, Opts,?get_env(),?wxToolBar_AddControl),
  wxe_util:rec(?wxToolBar_AddControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddseparator">external documentation</a>.
-doc """
Adds a separator for spacing groups of tools.

Notice that the separator uses the look appropriate for the current platform so
it can be a vertical line (MSW, some versions of GTK) or just an empty space or
something else.

See: `addTool/6`, `setToolSeparation/2`, `addStretchableSpace/1`
""".
-spec addSeparator(This) -> wx:wx_object() when
	This::wxToolBar().
addSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddSeparator),
  wxe_util:rec(?wxToolBar_AddSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
-doc """
Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in
order to have the tools appear.

See: `addSeparator/1`, `addCheckTool/5`, `addRadioTool/5`, `insertTool/6`,
`deleteTool/2`, `realize/1`, `SetDropdownMenu()` (not implemented in wx)
""".
-spec addTool(This, Tool) -> wx:wx_object() when
	This::wxToolBar(), Tool::wx:wx_object().
addTool(#wx_ref{type=ThisT}=This,#wx_ref{type=ToolT}=Tool) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Tool,?get_env(),?wxToolBar_AddTool_1),
  wxe_util:rec(?wxToolBar_AddTool_1).

%% @equiv addTool(This,ToolId,Label,Bitmap, [])
-spec addTool(This, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

addTool(This,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  addTool(This,ToolId,Label,Bitmap, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%% <br /> Also:<br />
%% addTool(This, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when<br />
%% 	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),<br />
%% 	Option :: {'shortHelp', unicode:chardata()}<br />
%% 		 | {'kind', wx:wx_enum()}.<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-doc """
Adds a tool to the toolbar.

This most commonly used version has fewer parameters than the full version below
which specifies the more rarely used button features.

Remark: After you have added tools to a toolbar, you must call `realize/1` in
order to have the tools appear.

See: `addSeparator/1`, `addCheckTool/5`, `addRadioTool/5`, `insertTool/6`,
`deleteTool/2`, `realize/1`, `SetDropdownMenu()` (not implemented in wx)
""".
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap();
      (This, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),
	Option :: {'shortHelp', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.

addTool(This,ToolId,Label,Bitmap,BmpDisabled)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref),is_record(BmpDisabled, wx_ref) ->
  addTool(This,ToolId,Label,Bitmap,BmpDisabled, []);
addTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({kind, _kind} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap, Opts,?get_env(),?wxToolBar_AddTool_4),
  wxe_util:rec(?wxToolBar_AddTool_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-doc """
Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in
order to have the tools appear.

See: `addSeparator/1`, `addCheckTool/5`, `addRadioTool/5`, `insertTool/6`,
`deleteTool/2`, `realize/1`, `SetDropdownMenu()` (not implemented in wx)
""".
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(),
	Option :: {'kind', wx:wx_enum()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap,#wx_ref{type=BmpDisabledT}=BmpDisabled, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(BmpDisabledT,wxBitmap),
  MOpts = fun({kind, _kind} = Arg) -> Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap,BmpDisabled, Opts,?get_env(),?wxToolBar_AddTool_5),
  wxe_util:rec(?wxToolBar_AddTool_5).

%% @equiv addCheckTool(This,ToolId,Label,Bitmap1, [])
-spec addCheckTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addCheckTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addCheckTool(This,ToolId,Label,Bitmap1, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddchecktool">external documentation</a>.
-doc """
Adds a new check (or toggle) tool to the toolbar.

The parameters are the same as in `addTool/6`.

See: `addTool/6`
""".
-spec addCheckTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addCheckTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=Bitmap1T}=Bitmap1, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(Bitmap1T,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap1, Opts,?get_env(),?wxToolBar_AddCheckTool),
  wxe_util:rec(?wxToolBar_AddCheckTool).

%% @equiv addRadioTool(This,ToolId,Label,Bitmap1, [])
-spec addRadioTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addRadioTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addRadioTool(This,ToolId,Label,Bitmap1, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddradiotool">external documentation</a>.
-doc """
Adds a new radio tool to the toolbar.

Consecutive radio tools form a radio group such that exactly one button in the
group is pressed at any moment, in other words whenever a button in the group is
pressed the previously pressed button is automatically released. You should
avoid having the radio groups of only one element as it would be impossible for
the user to use such button.

By default, the first button in the radio group is initially pressed, the others
are not.

See: `addTool/6`
""".
-spec addRadioTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addRadioTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=Bitmap1T}=Bitmap1, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(Bitmap1T,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap1, Opts,?get_env(),?wxToolBar_AddRadioTool),
  wxe_util:rec(?wxToolBar_AddRadioTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddstretchablespace">external documentation</a>.
-doc """
Adds a stretchable space to the toolbar.

Any space not taken up by the fixed items (all items except for stretchable
spaces) is distributed in equal measure between the stretchable spaces in the
toolbar. The most common use for this method is to add a single stretchable
space before the items which should be right-aligned in the toolbar, but more
exotic possibilities are possible, e.g. a stretchable space may be added in the
beginning and the end of the toolbar to centre all toolbar items.

See: `addTool/6`, `addSeparator/1`, `insertStretchableSpace/2`

Since: 2.9.1
""".
-spec addStretchableSpace(This) -> wx:wx_object() when
	This::wxToolBar().
addStretchableSpace(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddStretchableSpace),
  wxe_util:rec(?wxToolBar_AddStretchableSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertstretchablespace">external documentation</a>.
-doc """
Inserts a stretchable space at the given position.

See `addStretchableSpace/1` for details about stretchable spaces.

See: `insertTool/6`, `insertSeparator/2`

Since: 2.9.1
""".
-spec insertStretchableSpace(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertStretchableSpace(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertStretchableSpace),
  wxe_util:rec(?wxToolBar_InsertStretchableSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbardeletetool">external documentation</a>.
-doc """
Removes the specified tool from the toolbar and deletes it.

If you don't want to delete the tool, but just to remove it from the toolbar (to
possibly add it back later), you may use `removeTool/2` instead.

Note: It is unnecessary to call `realize/1` for the change to take place, it
will happen immediately.

Return: true if the tool was deleted, false otherwise.

See: `deleteToolByPos/2`
""".
-spec deleteTool(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
deleteTool(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_DeleteTool),
  wxe_util:rec(?wxToolBar_DeleteTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbardeletetoolbypos">external documentation</a>.
-doc """
This function behaves like `deleteTool/2` but it deletes the tool at the
specified position and not the one with the given id.
""".
-spec deleteToolByPos(This, Pos) -> boolean() when
	This::wxToolBar(), Pos::integer().
deleteToolByPos(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_DeleteToolByPos),
  wxe_util:rec(?wxToolBar_DeleteToolByPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarenabletool">external documentation</a>.
-doc """
Enables or disables the tool.

Remark: Some implementations will change the visible state of the tool to
indicate that it is disabled.

See: `getToolEnabled/2`, `toggleTool/3`
""".
-spec enableTool(This, ToolId, Enable) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), Enable::boolean().
enableTool(#wx_ref{type=ThisT}=This,ToolId,Enable)
 when is_integer(ToolId),is_boolean(Enable) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,Enable,?get_env(),?wxToolBar_EnableTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindbyid">external documentation</a>.
-doc """
Returns a pointer to the tool identified by `id` or NULL if no corresponding
tool is found.
""".
-spec findById(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
findById(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindById),
  wxe_util:rec(?wxToolBar_FindById).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindcontrol">external documentation</a>.
-doc """
Returns a pointer to the control identified by `id` or NULL if no corresponding
control is found.
""".
-spec findControl(This, Id) -> wxControl:wxControl() when
	This::wxToolBar(), Id::integer().
findControl(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindControl),
  wxe_util:rec(?wxToolBar_FindControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindtoolforposition">external documentation</a>.
-doc """
Finds a tool for the given mouse position.

Return: A pointer to a tool if a tool is found, or NULL otherwise.

Remark: Currently not implemented in wxGTK (always returns NULL there).
""".
-spec findToolForPosition(This, X, Y) -> wx:wx_object() when
	This::wxToolBar(), X::integer(), Y::integer().
findToolForPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxToolBar_FindToolForPosition),
  wxe_util:rec(?wxToolBar_FindToolForPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolsize">external documentation</a>.
-doc """
Returns the size of a whole button, which is usually larger than a tool bitmap
because of added 3D effects.

See: `setToolBitmapSize/2`, `getToolBitmapSize/1`
""".
-spec getToolSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolSize),
  wxe_util:rec(?wxToolBar_GetToolSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolbitmapsize">external documentation</a>.
-doc """
Returns the size of bitmap that the toolbar expects to have.

The default bitmap size is platform-dependent: for example, it is 16*15 for MSW
and 24*24 for GTK. This size does `not` necessarily indicate the best size to
use for the toolbars on the given platform, for this you should use
`wxArtProvider::GetNativeSizeHint(wxART_TOOLBAR)` but in any case, as the bitmap
size is deduced automatically from the size of the bitmaps associated with the
tools added to the toolbar, it is usually unnecessary to call
`setToolBitmapSize/2` explicitly.

Remark: Note that this is the size of the bitmap you pass to `addTool/6`, and
not the eventual size of the tool button.

See: `setToolBitmapSize/2`, `getToolSize/1`
""".
-spec getToolBitmapSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolBitmapSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolBitmapSize),
  wxe_util:rec(?wxToolBar_GetToolBitmapSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargetmargins">external documentation</a>.
-doc """
Returns the left/right and top/bottom margins, which are also used for
inter-toolspacing.

See: `setMargins/3`
""".
-spec getMargins(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getMargins(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetMargins),
  wxe_util:rec(?wxToolBar_GetMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolenabled">external documentation</a>.
-doc """
Called to determine whether a tool is enabled (responds to user input).

Return: true if the tool is enabled, false otherwise.

See: `enableTool/3`
""".
-spec getToolEnabled(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
getToolEnabled(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolEnabled),
  wxe_util:rec(?wxToolBar_GetToolEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoollonghelp">external documentation</a>.
-doc """
Returns the long help for the given tool.

See: `setToolLongHelp/3`, `setToolShortHelp/3`
""".
-spec getToolLongHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolLongHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolLongHelp),
  wxe_util:rec(?wxToolBar_GetToolLongHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolpacking">external documentation</a>.
-doc """
Returns the value used for packing tools.

See: `setToolPacking/2`
""".
-spec getToolPacking(This) -> integer() when
	This::wxToolBar().
getToolPacking(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolPacking),
  wxe_util:rec(?wxToolBar_GetToolPacking).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolpos">external documentation</a>.
-doc """
Returns the tool position in the toolbar, or `wxNOT_FOUND` if the tool is not
found.
""".
-spec getToolPos(This, ToolId) -> integer() when
	This::wxToolBar(), ToolId::integer().
getToolPos(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolPos),
  wxe_util:rec(?wxToolBar_GetToolPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolseparation">external documentation</a>.
-doc """
Returns the default separator size.

See: `setToolSeparation/2`
""".
-spec getToolSeparation(This) -> integer() when
	This::wxToolBar().
getToolSeparation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolSeparation),
  wxe_util:rec(?wxToolBar_GetToolSeparation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolshorthelp">external documentation</a>.
-doc """
Returns the short help for the given tool.

See: `getToolLongHelp/2`, `setToolShortHelp/3`
""".
-spec getToolShortHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolShortHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolShortHelp),
  wxe_util:rec(?wxToolBar_GetToolShortHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolstate">external documentation</a>.
-doc """
Gets the on/off state of a toggle tool.

Return: true if the tool is toggled on, false otherwise.

See: `toggleTool/3`
""".
-spec getToolState(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
getToolState(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolState),
  wxe_util:rec(?wxToolBar_GetToolState).

%% @equiv insertControl(This,Pos,Control, [])
-spec insertControl(This, Pos, Control) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl().

insertControl(This,Pos,Control)
 when is_record(This, wx_ref),is_integer(Pos),is_record(Control, wx_ref) ->
  insertControl(This,Pos,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertcontrol">external documentation</a>.
-doc """
Inserts the control into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See: `addControl/3`, `insertTool/6`
""".
-spec insertControl(This, Pos, Control, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl(),
	Option :: {'label', unicode:chardata()}.
insertControl(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ControlT}=Control, Options)
 when is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Control, Opts,?get_env(),?wxToolBar_InsertControl),
  wxe_util:rec(?wxToolBar_InsertControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertseparator">external documentation</a>.
-doc """
Inserts the separator into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See: `addSeparator/1`, `insertTool/6`
""".
-spec insertSeparator(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertSeparator),
  wxe_util:rec(?wxToolBar_InsertSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
-spec insertTool(This, Pos, Tool) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Tool::wx:wx_object().
insertTool(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ToolT}=Tool)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Pos,Tool,?get_env(),?wxToolBar_InsertTool_2),
  wxe_util:rec(?wxToolBar_InsertTool_2).

%% @equiv insertTool(This,Pos,ToolId,Label,Bitmap, [])
-spec insertTool(This, Pos, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

insertTool(This,Pos,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  insertTool(This,Pos,ToolId,Label,Bitmap, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-doc """
Inserts the tool with the specified attributes into the toolbar at the given
position.

You must call `realize/1` for the change to take place.

See: `addTool/6`, `insertControl/4`, `insertSeparator/2`

Return: The newly inserted tool or NULL on failure. Notice that with the
overload taking `tool` parameter the caller is responsible for deleting the tool
in the latter case.
""".
-spec insertTool(This, Pos, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'kind', wx:wx_enum()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'clientData', wx:wx_object()}.
insertTool(#wx_ref{type=ThisT}=This,Pos,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap, Options)
 when is_integer(Pos),is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({kind, _kind} = Arg) -> Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({clientData, #wx_ref{type=ClientDataT}} = Arg) ->   ?CLASS(ClientDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,ToolId,Label_UC,Bitmap, Opts,?get_env(),?wxToolBar_InsertTool_5),
  wxe_util:rec(?wxToolBar_InsertTool_5).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarrealize">external documentation</a>.
-doc "This function should be called after you have added tools.".
-spec realize(This) -> boolean() when
	This::wxToolBar().
realize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_Realize),
  wxe_util:rec(?wxToolBar_Realize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarremovetool">external documentation</a>.
-doc """
Removes the given tool from the toolbar but doesn't delete it.

This allows inserting/adding this tool back to this (or another) toolbar later.

Note: It is unnecessary to call `realize/1` for the change to take place, it
will happen immediately.

See: `deleteTool/2`
""".
-spec removeTool(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
removeTool(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_RemoveTool),
  wxe_util:rec(?wxToolBar_RemoveTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsetmargins">external documentation</a>.
-doc """
Set the values to be used as margins for the toolbar.

Remark: This must be called before the tools are added if absolute positioning
is to be used, and the default (zero-size) margins are to be overridden.

See: `getMargins/1`
""".
-spec setMargins(This, X, Y) -> 'ok' when
	This::wxToolBar(), X::integer(), Y::integer().
setMargins(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxToolBar_SetMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolbitmapsize">external documentation</a>.
-doc """
Sets the default size of each tool bitmap.

The default bitmap size is 16 by 15 pixels.

Remark: This should be called to tell the toolbar what the tool bitmap size is.
Call it before you add tools.

See: `getToolBitmapSize/1`, `getToolSize/1`
""".
-spec setToolBitmapSize(This, Size) -> 'ok' when
	This::wxToolBar(), Size::{W::integer(), H::integer()}.
setToolBitmapSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxToolBar_SetToolBitmapSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoollonghelp">external documentation</a>.
-doc """
Sets the long help for the given tool.

Remark: You might use the long help for displaying the tool purpose on the
status line.

See: `getToolLongHelp/2`, `setToolShortHelp/3`
""".
-spec setToolLongHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolLongHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolLongHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolpacking">external documentation</a>.
-doc """
Sets the value used for spacing tools.

The default value is 1.

Remark: The packing is used for spacing in the vertical direction if the toolbar
is horizontal, and for spacing in the horizontal direction if the toolbar is
vertical.

See: `getToolPacking/1`
""".
-spec setToolPacking(This, Packing) -> 'ok' when
	This::wxToolBar(), Packing::integer().
setToolPacking(#wx_ref{type=ThisT}=This,Packing)
 when is_integer(Packing) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Packing,?get_env(),?wxToolBar_SetToolPacking).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolshorthelp">external documentation</a>.
-doc """
Sets the short help for the given tool.

Remark: An application might use short help for identifying the tool purpose in
a tooltip.

See: `getToolShortHelp/2`, `setToolLongHelp/3`
""".
-spec setToolShortHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolShortHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolShortHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolseparation">external documentation</a>.
-doc """
Sets the default separator size.

The default value is 5.

See: `addSeparator/1`
""".
-spec setToolSeparation(This, Separation) -> 'ok' when
	This::wxToolBar(), Separation::integer().
setToolSeparation(#wx_ref{type=ThisT}=This,Separation)
 when is_integer(Separation) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Separation,?get_env(),?wxToolBar_SetToolSeparation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbartoggletool">external documentation</a>.
-doc """
Toggles a tool on or off.

This does not cause any event to get emitted.

Remark: Only applies to a tool that has been specified as a toggle tool.
""".
-spec toggleTool(This, ToolId, Toggle) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), Toggle::boolean().
toggleTool(#wx_ref{type=ThisT}=This,ToolId,Toggle)
 when is_integer(ToolId),is_boolean(Toggle) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,Toggle,?get_env(),?wxToolBar_ToggleTool).

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
