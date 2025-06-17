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

-module(wxDatePickerCtrl).
-moduledoc """
This control allows the user to select a date.

Unlike `m:wxCalendarCtrl`, which is a relatively big control, `m:wxDatePickerCtrl` is
implemented as a small window showing the currently selected date. The control can be
edited using the keyboard, and can also display a popup window for more user-friendly date
selection, depending on the styles used and the platform.

It is only available if `wxUSE_DATEPICKCTRL` is set to 1.

## Styles

This class supports the following styles:

* wxDP_SPIN: Creates a control without a month calendar drop down but with
spin-control-like arrows to change individual date components. This style is not supported
by the generic version.

* wxDP_DROPDOWN: Creates a control with a month calendar drop-down part from which the user
can select a date. This style is not supported in OSX/Cocoa native version.

* wxDP_DEFAULT: Creates a control with the style that is best supported for the current
platform (currently wxDP_SPIN under Windows and OSX/Cocoa and wxDP_DROPDOWN elsewhere).

* wxDP_ALLOWNONE: With this style, the control allows the user to not enter any valid date
at all. Without it - the default - the control always has some valid date. This style is
not supported in OSX/Cocoa native version.

* wxDP_SHOWCENTURY: Forces display of the century in the default date format. Without this
style the century could be displayed, or not, depending on the default date representation
in the system. This style is not supported in OSX/Cocoa native version currently. As can
be seen from the remarks above, most of the control style are only supported in the native
MSW implementation. In portable code it's recommended to use `wxDP_DEFAULT` style only,
possibly combined with `wxDP_SHOWCENTURY` (this is also the style used by default if none
is specified).

See:
* `m:wxCalendarCtrl`

* `m:wxDateEvent`

This class is derived, and can use functions, from:

* `m:wxPickerBase`

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxDatePickerCtrl](https://docs.wxwidgets.org/3.2/classwx_date_picker_ctrl.html)

## Events

Event types emitted from this class:

* [`date_changed`](`m:wxDateEvent`)
""".
-include("wxe.hrl").
-export([destroy/1,getRange/3,getValue/1,new/0,new/2,new/3,setRange/3,setValue/2]).

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
  getId/1,getInternalMargin/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPickerCtrlProportion/1,getPosition/1,getRect/1,getScreenPosition/1,
  getScreenRect/1,getScrollPos/2,getScrollRange/2,getScrollThumb/2,
  getSize/1,getSizer/1,getTextCtrl/1,getTextCtrlProportion/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTextCtrl/1,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isPickerCtrlGrowable/1,
  isRetained/1,isShown/1,isShownOnScreen/1,isTextCtrlGrowable/1,isTopLevel/1,
  layout/1,lineDown/1,lineUp/1,lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setInternalMargin/2,
  setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setPickerCtrlGrowable/1,
  setPickerCtrlGrowable/2,setPickerCtrlProportion/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setSize/2,setSize/3,setSize/5,
  setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,
  setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setTextCtrlGrowable/1,
  setTextCtrlGrowable/2,setTextCtrlProportion/2,setThemeEnabled/2,
  setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxDatePickerCtrl() :: wx:wx_object().
-export_type([wxDatePickerCtrl/0]).
-doc false.
parent_class(wxPickerBase) -> true;
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxDatePickerCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxDatePickerCtrl_new_0),
  wxe_util:rec(?wxDatePickerCtrl_new_0).

-doc(#{equiv => new(Parent,Id, [])}).
-spec new(Parent, Id) -> wxDatePickerCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

-doc """
Initializes the object and calls `Create()` (not implemented in wx) with all the
parameters.
""".
-spec new(Parent, Id, [Option]) -> wxDatePickerCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'date', wx:wx_datetime()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({date, {{DateY,DateMo,DateD},{DateH,DateMi,DateS}}}) -> {date,{DateD,DateMo,DateY,DateH,DateMi,DateS}};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxDatePickerCtrl_new_3),
  wxe_util:rec(?wxDatePickerCtrl_new_3).

-doc """
If the control had been previously limited to a range of dates using `setRange/3`,
returns the lower and upper bounds of this range.

If no range is set (or only one of the bounds is set), `dt1` and/or `dt2` are set to be invalid.

Notice that when using a native MSW implementation of this control the lower range is
always set, even if `setRange/3` hadn't been called explicitly, as the native control only supports
dates later than year 1601.

Return: false if no range limits are currently set, true if at least one bound is set.
""".
-spec getRange(This, Dt1, Dt2) -> boolean() when
	This::wxDatePickerCtrl(), Dt1::wx:wx_datetime(), Dt2::wx:wx_datetime().
getRange(#wx_ref{type=ThisT}=This,{{Dt1Y,Dt1Mo,Dt1D},{Dt1H,Dt1Mi,Dt1S}},{{Dt2Y,Dt2Mo,Dt2D},{Dt2H,Dt2Mi,Dt2S}})
 when is_integer(Dt1D),is_integer(Dt1Mo),is_integer(Dt1Y),is_integer(Dt1H),is_integer(Dt1Mi),is_integer(Dt1S),is_integer(Dt2D),is_integer(Dt2Mo),is_integer(Dt2Y),is_integer(Dt2H),is_integer(Dt2Mi),is_integer(Dt2S) ->
  ?CLASS(ThisT,wxDatePickerCtrl),
  wxe_util:queue_cmd(This,{Dt1D,Dt1Mo,Dt1Y,Dt1H,Dt1Mi,Dt1S},{Dt2D,Dt2Mo,Dt2Y,Dt2H,Dt2Mi,Dt2S},?get_env(),?wxDatePickerCtrl_GetRange),
  wxe_util:rec(?wxDatePickerCtrl_GetRange).

-doc """
Returns the currently entered date.

For a control with `wxDP_ALLOWNONE` style the returned value may be invalid if no date is
entered, otherwise it is always valid.
""".
-spec getValue(This) -> wx:wx_datetime() when
	This::wxDatePickerCtrl().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDatePickerCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxDatePickerCtrl_GetValue),
  wxe_util:rec(?wxDatePickerCtrl_GetValue).

-doc """
Sets the valid range for the date selection.

If `dt1` is valid, it becomes the earliest date (inclusive) accepted by the control. If `dt2`
is valid, it becomes the latest possible date.

Notice that if the current value is not inside the new range, it will be adjusted to lie
inside it, i.e. calling this method can change the control value, however no events are
generated by it.

Remark: If the current value of the control is outside of the newly set range bounds, the
behaviour is undefined.
""".
-spec setRange(This, Dt1, Dt2) -> 'ok' when
	This::wxDatePickerCtrl(), Dt1::wx:wx_datetime(), Dt2::wx:wx_datetime().
setRange(#wx_ref{type=ThisT}=This,{{Dt1Y,Dt1Mo,Dt1D},{Dt1H,Dt1Mi,Dt1S}},{{Dt2Y,Dt2Mo,Dt2D},{Dt2H,Dt2Mi,Dt2S}})
 when is_integer(Dt1D),is_integer(Dt1Mo),is_integer(Dt1Y),is_integer(Dt1H),is_integer(Dt1Mi),is_integer(Dt1S),is_integer(Dt2D),is_integer(Dt2Mo),is_integer(Dt2Y),is_integer(Dt2H),is_integer(Dt2Mi),is_integer(Dt2S) ->
  ?CLASS(ThisT,wxDatePickerCtrl),
  wxe_util:queue_cmd(This,{Dt1D,Dt1Mo,Dt1Y,Dt1H,Dt1Mi,Dt1S},{Dt2D,Dt2Mo,Dt2Y,Dt2H,Dt2Mi,Dt2S},?get_env(),?wxDatePickerCtrl_SetRange).

-doc """
Changes the current value of the control.

The date should be valid unless the control was created with `wxDP_ALLOWNONE` style and
included in the currently selected range, if any.

Calling this method does not result in a date change event.
""".
-spec setValue(This, Dt) -> 'ok' when
	This::wxDatePickerCtrl(), Dt::wx:wx_datetime().
setValue(#wx_ref{type=ThisT}=This,{{DtY,DtMo,DtD},{DtH,DtMi,DtS}})
 when is_integer(DtD),is_integer(DtMo),is_integer(DtY),is_integer(DtH),is_integer(DtMi),is_integer(DtS) ->
  ?CLASS(ThisT,wxDatePickerCtrl),
  wxe_util:queue_cmd(This,{DtD,DtMo,DtY,DtH,DtMi,DtS},?get_env(),?wxDatePickerCtrl_SetValue).

-doc "Destroys the object".
-spec destroy(This::wxDatePickerCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDatePickerCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxPickerBase
-doc false.
isPickerCtrlGrowable(This) -> wxPickerBase:isPickerCtrlGrowable(This).
-doc false.
setTextCtrlGrowable(This, Options) -> wxPickerBase:setTextCtrlGrowable(This, Options).
-doc false.
setTextCtrlGrowable(This) -> wxPickerBase:setTextCtrlGrowable(This).
-doc false.
setPickerCtrlGrowable(This, Options) -> wxPickerBase:setPickerCtrlGrowable(This, Options).
-doc false.
setPickerCtrlGrowable(This) -> wxPickerBase:setPickerCtrlGrowable(This).
-doc false.
isTextCtrlGrowable(This) -> wxPickerBase:isTextCtrlGrowable(This).
-doc false.
getTextCtrl(This) -> wxPickerBase:getTextCtrl(This).
-doc false.
hasTextCtrl(This) -> wxPickerBase:hasTextCtrl(This).
-doc false.
getPickerCtrlProportion(This) -> wxPickerBase:getPickerCtrlProportion(This).
-doc false.
getTextCtrlProportion(This) -> wxPickerBase:getTextCtrlProportion(This).
-doc false.
setPickerCtrlProportion(This,Prop) -> wxPickerBase:setPickerCtrlProportion(This,Prop).
-doc false.
setTextCtrlProportion(This,Prop) -> wxPickerBase:setTextCtrlProportion(This,Prop).
-doc false.
getInternalMargin(This) -> wxPickerBase:getInternalMargin(This).
-doc false.
setInternalMargin(This,Margin) -> wxPickerBase:setInternalMargin(This,Margin).
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
