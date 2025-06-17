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

-module(wxButton).
-moduledoc """
A button is a control that contains a text string, and is one of the most common elements
of a GUI.

It may be placed on a `m:wxDialog` or on a `m:wxPanel` panel, or indeed on almost any
other window.

By default, i.e. if none of the alignment styles are specified, the label is centered
both horizontally and vertically. If the button has both a label and a bitmap, the
alignment styles above specify the location of the rectangle combining both the label and
the bitmap and the bitmap position set with `wxButton::SetBitmapPosition()` (not
implemented in wx) defines the relative position of the bitmap with respect to the label
(however currently non-default alignment combinations are not implemented on all platforms).

Since version 2.9.1 `m:wxButton` supports showing both text and an image (currently only
when using wxMSW, wxGTK or wxOSX/Cocoa ports), see `SetBitmap()` (not implemented in wx)
and `setBitmapLabel/2`, `setBitmapDisabled/2` &c methods. In the previous wxWidgets versions this functionality was only
available in (the now trivial) `m:wxBitmapButton` class which was only capable of showing
an image without text.

A button may have either a single image for all states or different images for the
following states (different images are not currently supported under macOS where the
normal image is used for all states):

* `normal:` the default state

* `disabled:` bitmap shown when the button is disabled.

* `pressed:` bitmap shown when the button is pushed (e.g. while the user keeps the mouse
button pressed on it)

* `focus:` bitmap shown when the button has keyboard focus (but is not pressed as in this
case the button is in the pressed state)

* `current:` bitmap shown when the mouse is over the button (but it is not pressed although
it may have focus). Notice that if current bitmap is not specified but the current
platform UI uses hover images for the buttons (such as Windows or GTK+), then the focus
bitmap is used for hover state as well. This makes it possible to set focus bitmap only to
get reasonably good behaviour on all platforms.

All of the bitmaps must be of the same size and the normal bitmap must be set first (to
a valid bitmap), before setting any other ones. Also, if the size of the bitmaps is
changed later, you need to change the size of the normal bitmap before setting any other
bitmaps with the new size (and you do need to reset all of them as their original values
can be lost when the normal bitmap size changes).

The position of the image inside the button be configured using `SetBitmapPosition()`
(not implemented in wx). By default the image is on the left of the text.

Please also notice that GTK+ uses a global setting called `gtk-button-images` to
determine if the images should be shown in the buttons at all. If it is off (which is the
case in e.g. Gnome 2.28 by default), no images will be shown, consistently with the native behaviour.

## Styles

This class supports the following styles:

* wxBU_LEFT: Left-justifies the label. Windows and GTK+ only.

* wxBU_TOP: Aligns the label to the top of the button. Windows and GTK+ only.

* wxBU_RIGHT: Right-justifies the bitmap label. Windows and GTK+ only.

* wxBU_BOTTOM: Aligns the label to the bottom of the button. Windows and GTK+ only.

* wxBU_EXACTFIT: By default, all buttons are made of at least the standard button size,
even if their contents is small enough to fit into a smaller size. This is done for
consistency as most platforms use buttons of the same size in the native dialogs, but can
be overridden by specifying this flag. If it is given, the button will be made just big
enough for its contents. Notice that under MSW the button will still have at least the
standard height, even with this style, if it has a non-empty label.

* wxBU_NOTEXT: Disables the display of the text label in the button even if it has one or
its id is one of the standard stock ids with an associated label: without using this style
a button which is only supposed to show a bitmap but uses a standard id would display a
label too.

* wxBORDER_NONE: Creates a button without border. This is currently implemented in MSW,
GTK2 and OSX/Cocoa.

See: `m:wxBitmapButton`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxButton](https://docs.wxwidgets.org/3.2/classwx_button.html)

## Events

Event types emitted from this class:

* [`command_button_clicked`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([create/3,create/4,destroy/1,getBitmapDisabled/1,getBitmapFocus/1,getBitmapLabel/1,
  getDefaultSize/0,getDefaultSize/1,new/0,new/2,new/3,setBitmapDisabled/2,
  setBitmapFocus/2,setBitmapLabel/2,setDefault/1,setLabel/2]).

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
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxButton() :: wx:wx_object().
-export_type([wxButton/0]).
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxButton().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxButton_new_0),
  wxe_util:rec(?wxButton_new_0).

-doc(#{equiv => new(Parent,Id, [])}).
-spec new(Parent, Id) -> wxButton() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

-doc """
Constructor, creating and showing a button.

The preferred way to create standard buttons is to use default value of `label`. If no
label is supplied and `id` is one of standard IDs from this list, a standard label will be
used. In other words, if you use a predefined `wxID_XXX` constant, just omit the label
completely rather than specifying it. In particular, help buttons (the ones with `id` of `wxID_HELP`)
under macOS can't display any label at all and while `m:wxButton` will detect if the
standard "Help" label is used and ignore it, using any other label will prevent the button
from correctly appearing as a help button and so should be avoided.

In addition to that, the button will be decorated with stock icons under GTK+ 2.

See: `create/4`
""".
-spec new(Parent, Id, [Option]) -> wxButton() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'label', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxButton_new_3),
  wxe_util:rec(?wxButton_new_3).

-doc(#{equiv => create(This,Parent,Id, [])}).
-spec create(This, Parent, Id) -> boolean() when
	This::wxButton(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

-doc """
Button creation function for two-step creation.

For more details, see `new/3`.
""".
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxButton(), Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'label', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}
		 | {'validator', wx:wx_object()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxButton),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          ({validator, #wx_ref{type=ValidatorT}} = Arg) ->   ?CLASS(ValidatorT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxButton_Create),
  wxe_util:rec(?wxButton_Create).

-doc """
Returns the default size for the buttons.

It is advised to make all the dialog buttons of the same size and this function allows
retrieving the (platform, and current font dependent) size which should be the best suited
for this.

The optional `win` argument is new since wxWidgets 3.1.3 and allows to get a per-monitor
DPI specific size.
""".
-spec getDefaultSize() -> {W::integer(), H::integer()}.
getDefaultSize() ->
  wxe_util:queue_cmd(?get_env(), ?wxButton_GetDefaultSize_STAT_0),
  wxe_util:rec(?wxButton_GetDefaultSize_STAT_0).

-doc "".
-spec getDefaultSize(Win) -> {W::integer(), H::integer()} when
	Win::wxWindow:wxWindow().
getDefaultSize(#wx_ref{type=WinT}=Win) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:queue_cmd(Win,?get_env(),?wxButton_GetDefaultSize_STAT_1),
  wxe_util:rec(?wxButton_GetDefaultSize_STAT_1).

-doc """
This sets the button to be the default item in its top-level window (e.g.

the panel or the dialog box containing it).

As normal, pressing return causes the default button to be depressed when the return key
is pressed.

See also `wxWindow:setFocus/1` which sets the keyboard focus for windows and text panel items, and `wxTopLevelWindow::SetDefaultItem()`
(not implemented in wx).

Remark: Under Windows, only dialog box buttons respond to this function.

Return: the old default item (possibly NULL)
""".
-spec setDefault(This) -> wxWindow:wxWindow() when
	This::wxButton().
setDefault(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxButton),
  wxe_util:queue_cmd(This,?get_env(),?wxButton_SetDefault),
  wxe_util:rec(?wxButton_SetDefault).

-doc "Sets the string label for the button.".
-spec setLabel(This, Label) -> 'ok' when
	This::wxButton(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxButton),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxButton_SetLabel).

-doc """
Returns the bitmap for the disabled state, which may be invalid.

See: `setBitmapDisabled/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec getBitmapDisabled(This) -> wxBitmap:wxBitmap() when
	This::wxButton().
getBitmapDisabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxButton),
  wxe_util:queue_cmd(This,?get_env(),?wxButton_GetBitmapDisabled),
  wxe_util:rec(?wxButton_GetBitmapDisabled).

-doc """
Returns the bitmap for the focused state, which may be invalid.

See: `setBitmapFocus/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec getBitmapFocus(This) -> wxBitmap:wxBitmap() when
	This::wxButton().
getBitmapFocus(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxButton),
  wxe_util:queue_cmd(This,?get_env(),?wxButton_GetBitmapFocus),
  wxe_util:rec(?wxButton_GetBitmapFocus).

-doc """
Returns the bitmap for the normal state.

This is exactly the same as `GetBitmap()` (not implemented in wx) but uses a name
backwards-compatible with `m:wxBitmapButton`.

See: `setBitmapLabel/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec getBitmapLabel(This) -> wxBitmap:wxBitmap() when
	This::wxButton().
getBitmapLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxButton),
  wxe_util:queue_cmd(This,?get_env(),?wxButton_GetBitmapLabel),
  wxe_util:rec(?wxButton_GetBitmapLabel).

-doc """
Sets the bitmap for the disabled button appearance.

If `bitmap` is invalid, the disabled bitmap is set to the automatically generated greyed
out version of the normal bitmap, i.e. the same bitmap as is used by default if this
method is not called at all. Use `SetBitmap()` (not implemented in wx) with an invalid
bitmap to remove the bitmap completely (for all states).

See:
* `getBitmapDisabled/1`

* `setBitmapLabel/2`

* `setBitmapFocus/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec setBitmapDisabled(This, Bitmap) -> 'ok' when
	This::wxButton(), Bitmap::wxBitmap:wxBitmap().
setBitmapDisabled(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxButton),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxButton_SetBitmapDisabled).

-doc """
Sets the bitmap for the button appearance when it has the keyboard focus.

If `bitmap` is invalid, the normal bitmap will be used in the focused state.

See:
* `getBitmapFocus/1`

* `setBitmapLabel/2`

* `setBitmapDisabled/2`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec setBitmapFocus(This, Bitmap) -> 'ok' when
	This::wxButton(), Bitmap::wxBitmap:wxBitmap().
setBitmapFocus(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxButton),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxButton_SetBitmapFocus).

-doc """
Sets the bitmap label for the button.

Remark: This is the bitmap used for the unselected state, and for all other states if no
other bitmaps are provided.

See: `getBitmapLabel/1`

Since: 2.9.1 (available in `m:wxBitmapButton` only in previous versions)
""".
-spec setBitmapLabel(This, Bitmap) -> 'ok' when
	This::wxButton(), Bitmap::wxBitmap:wxBitmap().
setBitmapLabel(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxButton),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxButton_SetBitmapLabel).

-doc "Destroys the object".
-spec destroy(This::wxButton()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxButton),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControl
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
