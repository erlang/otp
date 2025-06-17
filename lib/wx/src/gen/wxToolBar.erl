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

-module(wxToolBar).
-moduledoc """
A toolbar is a bar of buttons and/or other controls usually placed below the menu bar in
a `m:wxFrame`.

You may create a toolbar that is managed by a frame calling `wxFrame:createToolBar/2`. Under Pocket PC, you should
always use this function for creating the toolbar to be managed by the frame, so that
wxWidgets can use a combined menubar and toolbar. Where you manage your own toolbars,
create `m:wxToolBar` as usual.

There are several different types of tools you can add to a toolbar. These types are
controlled by the ?wxItemKind enumeration.

Note that many methods in `m:wxToolBar` such as `addTool/6` return a `wxToolBarToolBase*` object.
This should be regarded as an opaque handle representing the newly added toolbar item,
providing access to its id and position within the toolbar. Changes to the item's state
should be made through calls to `m:wxToolBar` methods, for example `enableTool/3`. Calls to `wxToolBarToolBase`
(not implemented in wx) methods (undocumented by purpose) will not change the visible
state of the item within the tool bar.

After you have added all the tools you need, you must call `realize/1` to effectively construct and
display the toolbar.

`wxMSW note`: Note that under wxMSW toolbar paints tools to reflect system-wide colours.
If you use more than 16 colours in your tool bitmaps, you may wish to suppress this
behaviour, otherwise system colours in your bitmaps will inadvertently be mapped to system
colours. To do this, set the msw.remap system option before creating the toolbar: If you
wish to use 32-bit images (which include an alpha channel for transparency) use: Then
colour remapping is switched off, and a transparent background used. But only use this
option under Windows XP with true colour:

## Styles

This class supports the following styles:

* wxTB_FLAT: Gives the toolbar a flat look (Windows and GTK only).

* wxTB_DOCKABLE: Makes the toolbar floatable and dockable (GTK only).

* wxTB_HORIZONTAL: Specifies horizontal layout (default).

* wxTB_VERTICAL: Specifies vertical layout.

* wxTB_TEXT: Shows the text in the toolbar buttons; by default only icons are shown.

* wxTB_NOICONS: Specifies no icons in the toolbar buttons; by default they are shown.

* wxTB_NODIVIDER: Specifies no divider (border) above the toolbar (Windows only)

* wxTB_NOALIGN: Specifies no alignment with the parent window (Windows only, not very
useful).

* wxTB_HORZ_LAYOUT: Shows the text and the icons alongside, not vertically stacked (Windows
and GTK 2 only). This style must be used with `wxTB_TEXT`.

* wxTB_HORZ_TEXT: Combination of `wxTB_HORZ_LAYOUT` and `wxTB_TEXT`.

* wxTB_NO_TOOLTIPS: Don't show the short help tooltips for the tools when the mouse hovers
over them.

* wxTB_BOTTOM: Align the toolbar at the bottom of parent window.

* wxTB_RIGHT: Align the toolbar at the right side of parent window.

* wxTB_DEFAULT_STYLE: Combination of `wxTB_HORIZONTAL` and `wxTB_FLAT`. This style is new
since wxWidgets 2.9.5. See also overview_windowstyles. Note that the wxMSW native toolbar
ignores `wxTB_NOICONS` style. Also, toggling the `wxTB_TEXT` works only if the style was
initially on.

See: [Overview toolbar](https://docs.wxwidgets.org/3.2/overview_toolbar.html#overview_toolbar)

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxToolBar](https://docs.wxwidgets.org/3.2/classwx_tool_bar.html)

## Events

Event types emitted from this class:

* [`command_tool_rclicked`](`m:wxCommandEvent`)

* [`command_tool_enter`](`m:wxCommandEvent`)

* [`tool_dropdown`](`m:wxCommandEvent`)
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
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => addControl(This,Control, [])}).
-spec addControl(This, Control) -> wx:wx_object() when
	This::wxToolBar(), Control::wxControl:wxControl().

addControl(This,Control)
 when is_record(This, wx_ref),is_record(Control, wx_ref) ->
  addControl(This,Control, []).

-doc """
Adds any control to the toolbar, typically e.g. a `m:wxComboBox`.

Remark: wxMac: labels are only displayed if wxWidgets is built with `wxMAC_USE_NATIVE_TOOLBAR`
set to 1
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

-doc """
Adds a separator for spacing groups of tools.

Notice that the separator uses the look appropriate for the current platform so it can be
a vertical line (MSW, some versions of GTK) or just an empty space or something else.

See:
* `addTool/6`

* `setToolSeparation/2`

* `addStretchableSpace/1`
""".
-spec addSeparator(This) -> wx:wx_object() when
	This::wxToolBar().
addSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddSeparator),
  wxe_util:rec(?wxToolBar_AddSeparator).

-doc """
Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`
""".
-spec addTool(This, Tool) -> wx:wx_object() when
	This::wxToolBar(), Tool::wx:wx_object().
addTool(#wx_ref{type=ThisT}=This,#wx_ref{type=ToolT}=Tool) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Tool,?get_env(),?wxToolBar_AddTool_1),
  wxe_util:rec(?wxToolBar_AddTool_1).

-doc(#{equiv => addTool(This,ToolId,Label,Bitmap, [])}).
-spec addTool(This, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

addTool(This,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  addTool(This,ToolId,Label,Bitmap, []).

-doc """
Adds a tool to the toolbar.

This most commonly used version has fewer parameters than the full version below which
specifies the more rarely used button features.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc """
Adds a tool to the toolbar.

Remark: After you have added tools to a toolbar, you must call `realize/1` in order to have the
tools appear.

See:
* `addSeparator/1`

* `addCheckTool/5`

* `addRadioTool/5`

* `insertTool/6`

* `deleteTool/2`

* `realize/1`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc(#{equiv => addCheckTool(This,ToolId,Label,Bitmap1, [])}).
-spec addCheckTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addCheckTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addCheckTool(This,ToolId,Label,Bitmap1, []).

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

-doc(#{equiv => addRadioTool(This,ToolId,Label,Bitmap1, [])}).
-spec addRadioTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addRadioTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addRadioTool(This,ToolId,Label,Bitmap1, []).

-doc """
Adds a new radio tool to the toolbar.

Consecutive radio tools form a radio group such that exactly one button in the group is
pressed at any moment, in other words whenever a button in the group is pressed the
previously pressed button is automatically released. You should avoid having the radio
groups of only one element as it would be impossible for the user to use such button.

By default, the first button in the radio group is initially pressed, the others are not.

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

-doc """
Adds a stretchable space to the toolbar.

Any space not taken up by the fixed items (all items except for stretchable spaces) is
distributed in equal measure between the stretchable spaces in the toolbar. The most
common use for this method is to add a single stretchable space before the items which
should be right-aligned in the toolbar, but more exotic possibilities are possible, e.g. a
stretchable space may be added in the beginning and the end of the toolbar to centre all
toolbar items.

See:
* `addTool/6`

* `addSeparator/1`

* `insertStretchableSpace/2`

Since: 2.9.1
""".
-spec addStretchableSpace(This) -> wx:wx_object() when
	This::wxToolBar().
addStretchableSpace(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddStretchableSpace),
  wxe_util:rec(?wxToolBar_AddStretchableSpace).

-doc """
Inserts a stretchable space at the given position.

See `addStretchableSpace/1` for details about stretchable spaces.

See:
* `insertTool/6`

* `insertSeparator/2`

Since: 2.9.1
""".
-spec insertStretchableSpace(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertStretchableSpace(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertStretchableSpace),
  wxe_util:rec(?wxToolBar_InsertStretchableSpace).

-doc """
Removes the specified tool from the toolbar and deletes it.

If you don't want to delete the tool, but just to remove it from the toolbar (to possibly
add it back later), you may use `removeTool/2` instead.

Note: It is unnecessary to call `realize/1` for the change to take place, it will happen immediately.

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

-doc """
This function behaves like `deleteTool/2` but it deletes the tool at the specified
position and not the one with the given id.
""".
-spec deleteToolByPos(This, Pos) -> boolean() when
	This::wxToolBar(), Pos::integer().
deleteToolByPos(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_DeleteToolByPos),
  wxe_util:rec(?wxToolBar_DeleteToolByPos).

-doc """
Enables or disables the tool.

Remark: Some implementations will change the visible state of the tool to indicate that
it is disabled.

See:
* `getToolEnabled/2`

* `toggleTool/3`
""".
-spec enableTool(This, ToolId, Enable) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), Enable::boolean().
enableTool(#wx_ref{type=ThisT}=This,ToolId,Enable)
 when is_integer(ToolId),is_boolean(Enable) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,Enable,?get_env(),?wxToolBar_EnableTool).

-doc """
Returns a pointer to the tool identified by `id` or NULL if no corresponding tool is
found.
""".
-spec findById(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
findById(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindById),
  wxe_util:rec(?wxToolBar_FindById).

-doc """
Returns a pointer to the control identified by `id` or NULL if no corresponding control
is found.
""".
-spec findControl(This, Id) -> wxControl:wxControl() when
	This::wxToolBar(), Id::integer().
findControl(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindControl),
  wxe_util:rec(?wxToolBar_FindControl).

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

-doc """
Returns the size of a whole button, which is usually larger than a tool bitmap because of
added 3D effects.

See:
* `setToolBitmapSize/2`

* `getToolBitmapSize/1`
""".
-spec getToolSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolSize),
  wxe_util:rec(?wxToolBar_GetToolSize).

-doc """
Returns the size of bitmap that the toolbar expects to have.

The default bitmap size is platform-dependent: for example, it is 16*15 for MSW and 24*24
for GTK. This size does `not` necessarily indicate the best size to use for the toolbars
on the given platform, for this you should use `wxArtProvider::GetNativeSizeHint(wxART_TOOLBAR)`
but in any case, as the bitmap size is deduced automatically from the size of the bitmaps
associated with the tools added to the toolbar, it is usually unnecessary to call `setToolBitmapSize/2` explicitly.

Remark: Note that this is the size of the bitmap you pass to `addTool/6`, and not the eventual size
of the tool button.

See:
* `setToolBitmapSize/2`

* `getToolSize/1`
""".
-spec getToolBitmapSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolBitmapSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolBitmapSize),
  wxe_util:rec(?wxToolBar_GetToolBitmapSize).

-doc """
Returns the left/right and top/bottom margins, which are also used for inter-toolspacing.

See: `setMargins/3`
""".
-spec getMargins(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getMargins(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetMargins),
  wxe_util:rec(?wxToolBar_GetMargins).

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

-doc """
Returns the long help for the given tool.

See:
* `setToolLongHelp/3`

* `setToolShortHelp/3`
""".
-spec getToolLongHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolLongHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolLongHelp),
  wxe_util:rec(?wxToolBar_GetToolLongHelp).

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

-doc "Returns the tool position in the toolbar, or `wxNOT\_FOUND` if the tool is not found.".
-spec getToolPos(This, ToolId) -> integer() when
	This::wxToolBar(), ToolId::integer().
getToolPos(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolPos),
  wxe_util:rec(?wxToolBar_GetToolPos).

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

-doc """
Returns the short help for the given tool.

See:
* `getToolLongHelp/2`

* `setToolShortHelp/3`
""".
-spec getToolShortHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolShortHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolShortHelp),
  wxe_util:rec(?wxToolBar_GetToolShortHelp).

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

-doc(#{equiv => insertControl(This,Pos,Control, [])}).
-spec insertControl(This, Pos, Control) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl().

insertControl(This,Pos,Control)
 when is_record(This, wx_ref),is_integer(Pos),is_record(Control, wx_ref) ->
  insertControl(This,Pos,Control, []).

-doc """
Inserts the control into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addControl/3`

* `insertTool/6`
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

-doc """
Inserts the separator into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addSeparator/1`

* `insertTool/6`
""".
-spec insertSeparator(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertSeparator),
  wxe_util:rec(?wxToolBar_InsertSeparator).

-doc "".
-spec insertTool(This, Pos, Tool) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Tool::wx:wx_object().
insertTool(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ToolT}=Tool)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Pos,Tool,?get_env(),?wxToolBar_InsertTool_2),
  wxe_util:rec(?wxToolBar_InsertTool_2).

-doc(#{equiv => insertTool(This,Pos,ToolId,Label,Bitmap, [])}).
-spec insertTool(This, Pos, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

insertTool(This,Pos,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  insertTool(This,Pos,ToolId,Label,Bitmap, []).

-doc """
Inserts the tool with the specified attributes into the toolbar at the given position.

You must call `realize/1` for the change to take place.

See:
* `addTool/6`

* `insertControl/4`

* `insertSeparator/2`

Return: The newly inserted tool or NULL on failure. Notice that with the overload taking `tool`
parameter the caller is responsible for deleting the tool in the latter case.
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc "This function should be called after you have added tools.".
-spec realize(This) -> boolean() when
	This::wxToolBar().
realize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_Realize),
  wxe_util:rec(?wxToolBar_Realize).

-doc """
Removes the given tool from the toolbar but doesn't delete it.

This allows inserting/adding this tool back to this (or another) toolbar later.

Note: It is unnecessary to call `realize/1` for the change to take place, it will happen immediately.

See: `deleteTool/2`
""".
-spec removeTool(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
removeTool(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_RemoveTool),
  wxe_util:rec(?wxToolBar_RemoveTool).

-doc """
Set the values to be used as margins for the toolbar.

Remark: This must be called before the tools are added if absolute positioning is to be
used, and the default (zero-size) margins are to be overridden.

See: `getMargins/1`
""".
-spec setMargins(This, X, Y) -> 'ok' when
	This::wxToolBar(), X::integer(), Y::integer().
setMargins(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxToolBar_SetMargins).

-doc """
Sets the default size of each tool bitmap.

The default bitmap size is 16 by 15 pixels.

Remark: This should be called to tell the toolbar what the tool bitmap size is. Call it
before you add tools.

See:
* `getToolBitmapSize/1`

* `getToolSize/1`
""".
-spec setToolBitmapSize(This, Size) -> 'ok' when
	This::wxToolBar(), Size::{W::integer(), H::integer()}.
setToolBitmapSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxToolBar_SetToolBitmapSize).

-doc """
Sets the long help for the given tool.

Remark: You might use the long help for displaying the tool purpose on the status line.

See:
* `getToolLongHelp/2`

* `setToolShortHelp/3`
""".
-spec setToolLongHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolLongHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolLongHelp).

-doc """
Sets the value used for spacing tools.

The default value is 1.

Remark: The packing is used for spacing in the vertical direction if the toolbar is
horizontal, and for spacing in the horizontal direction if the toolbar is vertical.

See: `getToolPacking/1`
""".
-spec setToolPacking(This, Packing) -> 'ok' when
	This::wxToolBar(), Packing::integer().
setToolPacking(#wx_ref{type=ThisT}=This,Packing)
 when is_integer(Packing) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Packing,?get_env(),?wxToolBar_SetToolPacking).

-doc """
Sets the short help for the given tool.

Remark: An application might use short help for identifying the tool purpose in a tooltip.

See:
* `getToolShortHelp/2`

* `setToolLongHelp/3`
""".
-spec setToolShortHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolShortHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolShortHelp).

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
