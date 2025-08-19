%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxTextCtrl).
-moduledoc """
A text control allows text to be displayed and edited.

It may be single line or multi-line. Notice that a lot of methods of the text controls
are found in the base `wxTextEntry` (not implemented in wx) class which is a common base
class for `m:wxTextCtrl` and other controls using a single line text entry field (e.g. `m:wxComboBox`).

## Styles

This class supports the following styles:

* wxTE_PROCESS_ENTER: The control will generate the event `wxEVT_TEXT_ENTER` that can be
handled by the program. Otherwise, i.e. either if this style not specified at all, or it
is used, but there is no event handler for this event or the event handler called `wxEvent:skip/2` to
avoid overriding the default handling, pressing Enter key is either processed internally
by the control or used to activate the default button of the dialog, if any.

* wxTE_PROCESS_TAB: Normally, TAB key is used for keyboard navigation and pressing it in a
control switches focus to the next one. With this style, this won't happen and if the TAB
is not otherwise processed (e.g. by `wxEVT_CHAR` event handler), a literal TAB character
is inserted into the control. Notice that this style has no effect for single-line text
controls when using wxGTK.

* wxTE_MULTILINE: The text control allows multiple lines. If this style is not specified,
line break characters should not be used in the controls value.

* wxTE_PASSWORD: The text will be echoed as asterisks.

* wxTE_READONLY: The text will not be user-editable.

* wxTE_RICH: Use rich text control under MSW, this allows having more than 64KB of text in
the control. This style is ignored under other platforms.

* wxTE_RICH2: Use rich text control version 2.0 or higher under MSW, this style is ignored
under other platforms

* wxTE_AUTO_URL: Highlight the URLs and generate the wxTextUrlEvents when mouse events
occur over them.

* wxTE_NOHIDESEL: By default, the Windows text control doesn't show the selection when it
doesn't have focus - use this style to force it to always show it. It doesn't do anything
under other platforms.

* wxHSCROLL: A horizontal scrollbar will be created and used, so that text won't be
wrapped. No effect under wxGTK1.

* wxTE_NO_VSCROLL: For multiline controls only: vertical scrollbar will never be created.
This limits the amount of text which can be entered into the control to what can be
displayed in it under wxMSW but not under wxGTK or wxOSX. Currently not implemented for
the other platforms.

* wxTE_LEFT: The text in the control will be left-justified (default).

* wxTE_CENTRE: The text in the control will be centered (wxMSW, wxGTK, wxOSX).

* wxTE_RIGHT: The text in the control will be right-justified (wxMSW, wxGTK, wxOSX).

* wxTE_DONTWRAP: Same as wxHSCROLL style: don't wrap at all, show horizontal scrollbar
instead.

* wxTE_CHARWRAP: For multiline controls only: wrap the lines too long to be shown entirely
at any position (wxUniv, wxGTK, wxOSX).

* wxTE_WORDWRAP: For multiline controls only: wrap the lines too long to be shown entirely
at word boundaries (wxUniv, wxMSW, wxGTK, wxOSX).

* wxTE_BESTWRAP: For multiline controls only: wrap the lines at word boundaries or at any
other character if there are words longer than the window width (this is the default).

* wxTE_CAPITALIZE: On PocketPC and Smartphone, causes the first letter to be capitalized.
Note that alignment styles (wxTE_LEFT, wxTE_CENTRE and wxTE_RIGHT) can be changed
dynamically after control creation on wxMSW, wxGTK and wxOSX. wxTE_READONLY, wxTE_PASSWORD
and wrapping styles can be dynamically changed under wxGTK but not wxMSW. The other styles
can be only set during control creation.

wxTextCtrl Text Format

The multiline text controls always store the text as a sequence of lines separated by `'\n'`
characters, i.e. in the Unix text format even on non-Unix platforms. This allows the user
code to ignore the differences between the platforms but at a price: the indices in the
control such as those returned by `getInsertionPoint/1` or `getSelection/1` can `not` be used as indices into the string
returned by `getValue/1` as they're going to be slightly off for platforms using `"\\r\\n"` as
separator (as Windows does).

Instead, if you need to obtain a substring between the 2 indices obtained from the
control with the help of the functions mentioned above, you should use `getRange/3`. And the indices
themselves can only be passed to other methods, for example `setInsertionPoint/2` or `setSelection/3`.

To summarize: never use the indices returned by (multiline) `m:wxTextCtrl` as indices
into the string it contains, but only as arguments to be passed back to the other `m:wxTextCtrl`
methods. This problem doesn't arise for single-line platforms however where the indices
in the control do correspond to the positions in the value string.

wxTextCtrl Positions and Coordinates

It is possible to use either linear positions, i.e. roughly (but `not` always exactly, as
explained in the previous section) the index of the character in the text contained in the
control or X-Y coordinates, i.e. column and line of the character when working with this
class and it provides the functions `positionToXY/2` and `xYToPosition/3` to convert between the two.

Additionally, a position in the control can be converted to its coordinates in pixels
using `PositionToCoords()` (not implemented in wx) which can be useful to e.g. show a
popup menu near the given character. And, in the other direction, `HitTest()` (not
implemented in wx) can be used to find the character under, or near, the given pixel coordinates.

To be more precise, positions actually refer to the gaps between characters and not the
characters themselves. Thus, position 0 is the one before the very first character in the
control and so is a valid position even when the control is empty. And if the control
contains a single character, it has two valid positions: 0 before this character and 1 -
after it. This, when the documentation of various functions mentions "invalid position",
it doesn't consider the position just after the last character of the line to be invalid,
only the positions beyond that one (e.g. 2 and greater in the single character example)
are actually invalid.

wxTextCtrl Styles.

Multi-line text controls support styling, i.e. provide a possibility to set colours and
font for individual characters in it (note that under Windows `wxTE_RICH` style is
required for style support). To use the styles you can either call `setDefaultStyle/2` before inserting the
text or call `setStyle/4` later to change the style of the text already in the control (the first
solution is much more efficient).

In either case, if the style doesn't specify some of the attributes (for example you only
want to set the text colour but without changing the font nor the text background), the
values of the default style will be used for them. If there is no default style, the
attributes of the text control itself are used.

So the following code correctly describes what it does: the second call to `setDefaultStyle/2` doesn't
change the text foreground colour (which stays red) while the last one doesn't change the
background colour (which stays grey):

wxTextCtrl and C++ Streams

This class multiply-inherits from `std::streambuf` (except for some really old compilers
using non-standard iostream library), allowing code such as the following:

Note that even if your build of wxWidgets doesn't support this (the symbol `wxHAS_TEXT_WINDOW_STREAM`
has value of 0 then) you can still use `m:wxTextCtrl` itself in a stream-like manner:

However the possibility to create a `std::ostream` associated with `m:wxTextCtrl` may be
useful if you need to redirect the output of a function taking a `std::ostream` as
parameter to a text control.

Another commonly requested need is to redirect `std::cout` to the text control. This may
be done in the following way:

But wxWidgets provides a convenient class to make it even simpler so instead you may just do

See `wxStreamToTextRedirector` (not implemented in wx) for more details.

Event Handling.

The following commands are processed by default event handlers in `m:wxTextCtrl`: `wxID_CUT`, `wxID_COPY`, `wxID_PASTE`, `wxID_UNDO`, `wxID_REDO`.
The associated UI update events are also processed automatically, when the control has the focus.

See: `create/4`

This class is derived, and can use functions, from:

* `m:wxControl`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxTextCtrl](https://docs.wxwidgets.org/3.2/classwx_text_ctrl.html)

## Events

Event types emitted from this class:

* [`command_text_updated`](`m:wxCommandEvent`)

* [`command_text_enter`](`m:wxCommandEvent`)

* [`text_maxlen`](`m:wxCommandEvent`)
""".
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
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxTextCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTextCtrl_new_0),
  wxe_util:rec(?wxTextCtrl_new_0).

-doc(#{equiv => new(Parent,Id, [])}).
-spec new(Parent, Id) -> wxTextCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

-doc """
Constructor, creating and showing a text control.

Remark: The horizontal scrollbar (wxHSCROLL style flag) will only be created for
multi-line text controls. Without a horizontal scrollbar, text lines that don't fit in the
control's size will be wrapped (but no newline character is inserted). Single line
controls don't have a horizontal scrollbar, the text is automatically scrolled so that the
insertion point is always visible.

See: `create/4`
""".
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

-doc """
Appends the text to the end of the text control.

Remark: After the text is appended, the insertion point will be at the end of the text
control. If this behaviour is not desired, the programmer should use `getInsertionPoint/1` and `setInsertionPoint/2`.

See: `writeText/2`
""".
-spec appendText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
appendText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_AppendText).

-doc "Returns true if the selection can be copied to the clipboard.".
-spec canCopy(This) -> boolean() when
	This::wxTextCtrl().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCopy),
  wxe_util:rec(?wxTextCtrl_CanCopy).

-doc "Returns true if the selection can be cut to the clipboard.".
-spec canCut(This) -> boolean() when
	This::wxTextCtrl().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCut),
  wxe_util:rec(?wxTextCtrl_CanCut).

-doc """
Returns true if the contents of the clipboard can be pasted into the text control.

On some platforms (Motif, GTK) this is an approximation and returns true if the control
is editable, false otherwise.
""".
-spec canPaste(This) -> boolean() when
	This::wxTextCtrl().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanPaste),
  wxe_util:rec(?wxTextCtrl_CanPaste).

-doc "Returns true if there is a redo facility available and the last operation can be redone.".
-spec canRedo(This) -> boolean() when
	This::wxTextCtrl().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanRedo),
  wxe_util:rec(?wxTextCtrl_CanRedo).

-doc "Returns true if there is an undo facility available and the last operation can be undone.".
-spec canUndo(This) -> boolean() when
	This::wxTextCtrl().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanUndo),
  wxe_util:rec(?wxTextCtrl_CanUndo).

-doc """
Clears the text in the control.

Note that this function will generate a `wxEVT_TEXT` event, i.e. its effect is identical
to calling `SetValue`("").
""".
-spec clear(This) -> 'ok' when
	This::wxTextCtrl().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Clear).

-doc "Copies the selected text to the clipboard.".
-spec copy(This) -> 'ok' when
	This::wxTextCtrl().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Copy).

-doc(#{equiv => create(This,Parent,Id, [])}).
-spec create(This, Parent, Id) -> boolean() when
	This::wxTextCtrl(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

-doc """
Creates the text control for two-step construction.

This method should be called if the default constructor was used for the control
creation. Its parameters have the same meaning as for the non-default constructor.
""".
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

-doc "Copies the selected text to the clipboard and removes it from the control.".
-spec cut(This) -> 'ok' when
	This::wxTextCtrl().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Cut).

-doc "Resets the internal modified flag as if the current changes had been saved.".
-spec discardEdits(This) -> 'ok' when
	This::wxTextCtrl().
discardEdits(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_DiscardEdits).

-doc """
Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would return
false immediately after the call to `changeValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this function.

This functions does not generate the `wxEVT_TEXT` event but otherwise is identical to `setValue/2`.

See overview_events_prog for more information.

Since: 2.7.1
""".
-spec changeValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
changeValue(#wx_ref{type=ThisT}=This,Value)
 when ?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Value_UC,?get_env(),?wxTextCtrl_ChangeValue).

-doc """
This function inserts into the control the character which would have been inserted if
the given key event had occurred in the text control.

The `event` object should be the same as the one passed to `EVT_KEY_DOWN` handler
previously by wxWidgets. Please note that this function doesn't currently work correctly
for all keys under any platform but MSW.

Return: true if the event resulted in a change to the control, false otherwise.
""".
-spec emulateKeyPress(This, Event) -> boolean() when
	This::wxTextCtrl(), Event::wxKeyEvent:wxKeyEvent().
emulateKeyPress(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxTextCtrl_EmulateKeyPress),
  wxe_util:rec(?wxTextCtrl_EmulateKeyPress).

-doc """
Returns the style currently used for the new text.

See: `setDefaultStyle/2`
""".
-spec getDefaultStyle(This) -> wxTextAttr:wxTextAttr() when
	This::wxTextCtrl().
getDefaultStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetDefaultStyle),
  wxe_util:rec(?wxTextCtrl_GetDefaultStyle).

-doc """
Returns the insertion point, or cursor, position.

This is defined as the zero based index of the character position to the right of the
insertion point. For example, if the insertion point is at the end of the single-line text
control, it is equal to `getLastPosition/1`.

Notice that insertion position is, in general, different from the index of the character
the cursor position at in the string returned by `getValue/1`. While this is always the case for the
single line controls, multi-line controls can use two characters `"\\r\\n"` as line
separator (this is notably the case under MSW) meaning that indices in the control and its
string value are offset by 1 for every line.

Hence to correctly get the character at the current cursor position, taking into account
that there can be none if the cursor is at the end of the string, you could do the following:
""".
-spec getInsertionPoint(This) -> integer() when
	This::wxTextCtrl().
getInsertionPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetInsertionPoint),
  wxe_util:rec(?wxTextCtrl_GetInsertionPoint).

-doc """
Returns the zero based index of the last position in the text control, which is equal to
the number of characters in the control.
""".
-spec getLastPosition(This) -> integer() when
	This::wxTextCtrl().
getLastPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetLastPosition),
  wxe_util:rec(?wxTextCtrl_GetLastPosition).

-doc """
Gets the length of the specified line, not including any trailing newline character(s).

Return: The length of the line, or -1 if `lineNo` was invalid.
""".
-spec getLineLength(This, LineNo) -> integer() when
	This::wxTextCtrl(), LineNo::integer().
getLineLength(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineLength),
  wxe_util:rec(?wxTextCtrl_GetLineLength).

-doc """
Returns the contents of a given line in the text control, not including any trailing
newline character(s).

Return: The contents of the line.
""".
-spec getLineText(This, LineNo) -> unicode:charlist() when
	This::wxTextCtrl(), LineNo::integer().
getLineText(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineText),
  wxe_util:rec(?wxTextCtrl_GetLineText).

-doc """
Returns the number of lines in the text control buffer.

The returned number is the number of logical lines, i.e. just the count of the number of
newline characters in the control + 1, for wxGTK and wxOSX/Cocoa ports while it is the
number of physical lines, i.e. the count of lines actually shown in the control, in wxMSW.
Because of this discrepancy, it is not recommended to use this function.

Remark: Note that even empty text controls have one line (where the insertion point is),
so `getNumberOfLines/1` never returns 0.
""".
-spec getNumberOfLines(This) -> integer() when
	This::wxTextCtrl().
getNumberOfLines(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetNumberOfLines),
  wxe_util:rec(?wxTextCtrl_GetNumberOfLines).

-doc """
Returns the string containing the text starting in the positions `from` and up to `to` in
the control.

The positions must have been returned by another `m:wxTextCtrl` method. Please note that
the positions in a multiline `m:wxTextCtrl` do `not` correspond to the indices in the
string returned by `getValue/1` because of the different new line representations (`CR` or `CR` LF)
and so this method should be used to obtain the correct results instead of extracting
parts of the entire value. It may also be more efficient, especially if the control
contains a lot of data.
""".
-spec getRange(This, From, To) -> unicode:charlist() when
	This::wxTextCtrl(), From::integer(), To::integer().
getRange(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_GetRange),
  wxe_util:rec(?wxTextCtrl_GetRange).

-doc """
Gets the current selection span.

If the returned values are equal, there was no selection. Please note that the indices
returned may be used with the other `m:wxTextCtrl` methods but don't necessarily represent
the correct indices into the string returned by `getValue/1` for multiline controls under Windows (at
least,) you should use `getStringSelection/1` to get the selected text.
""".
-spec getSelection(This) -> {From::integer(), To::integer()} when
	This::wxTextCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetSelection),
  wxe_util:rec(?wxTextCtrl_GetSelection).

-doc """
Gets the text currently selected in the control.

If there is no selection, the returned string is empty.
""".
-spec getStringSelection(This) -> unicode:charlist() when
	This::wxTextCtrl().
getStringSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetStringSelection),
  wxe_util:rec(?wxTextCtrl_GetStringSelection).

-doc """
Returns the style at this position in the text control.

Not all platforms support this function.

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See:
* `setStyle/4`

* `m:wxTextAttr`
""".
-spec getStyle(This, Position, Style) -> boolean() when
	This::wxTextCtrl(), Position::integer(), Style::wxTextAttr:wxTextAttr().
getStyle(#wx_ref{type=ThisT}=This,Position,#wx_ref{type=StyleT}=Style)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Position,Style,?get_env(),?wxTextCtrl_GetStyle),
  wxe_util:rec(?wxTextCtrl_GetStyle).

-doc """
Gets the contents of the control.

Notice that for a multiline text control, the lines will be separated by (Unix-style) `\n`
characters, even under Windows where they are separated by a `\r\n` sequence in the
native control.
""".
-spec getValue(This) -> unicode:charlist() when
	This::wxTextCtrl().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetValue),
  wxe_util:rec(?wxTextCtrl_GetValue).

-doc """
Returns true if the controls contents may be edited by user (note that it always can be
changed by the program).

In other words, this functions returns true if the control hasn't been put in read-only
mode by a previous call to `setEditable/2`.
""".
-spec isEditable(This) -> boolean() when
	This::wxTextCtrl().
isEditable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsEditable),
  wxe_util:rec(?wxTextCtrl_IsEditable).

-doc """
Returns true if the text has been modified by user.

Note that calling `setValue/2` doesn't make the control modified.

See: `markDirty/1`
""".
-spec isModified(This) -> boolean() when
	This::wxTextCtrl().
isModified(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsModified),
  wxe_util:rec(?wxTextCtrl_IsModified).

-doc """
Returns true if this is a multi line edit control and false otherwise.

See: `isSingleLine/1`
""".
-spec isMultiLine(This) -> boolean() when
	This::wxTextCtrl().
isMultiLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsMultiLine),
  wxe_util:rec(?wxTextCtrl_IsMultiLine).

-doc """
Returns true if this is a single line edit control and false otherwise.

See:
* `isSingleLine/1`

* `isMultiLine/1`
""".
-spec isSingleLine(This) -> boolean() when
	This::wxTextCtrl().
isSingleLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsSingleLine),
  wxe_util:rec(?wxTextCtrl_IsSingleLine).

-doc(#{equiv => loadFile(This,Filename, [])}).
-spec loadFile(This, Filename) -> boolean() when
	This::wxTextCtrl(), Filename::unicode:chardata().

loadFile(This,Filename)
 when is_record(This, wx_ref),?is_chardata(Filename) ->
  loadFile(This,Filename, []).

-doc """
Loads and displays the named file, if it exists.

Return: true if successful, false otherwise.
""".
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

-doc """
Mark text as modified (dirty).

See: `isModified/1`
""".
-spec markDirty(This) -> 'ok' when
	This::wxTextCtrl().
markDirty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_MarkDirty).

-doc "Pastes text from the clipboard to the text item.".
-spec paste(This) -> 'ok' when
	This::wxTextCtrl().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Paste).

-doc """
Converts given position to a zero-based column, line number pair.

Return: true on success, false on failure (most likely due to a too large position parameter).

See: `xYToPosition/3`
""".
-spec positionToXY(This, Pos) -> Result when
	Result ::{Res ::boolean(), X::integer(), Y::integer()},
	This::wxTextCtrl(), Pos::integer().
positionToXY(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_PositionToXY),
  wxe_util:rec(?wxTextCtrl_PositionToXY).

-doc """
If there is a redo facility and the last operation can be redone, redoes the last
operation.

Does nothing if there is no redo facility.
""".
-spec redo(This) -> 'ok' when
	This::wxTextCtrl().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Redo).

-doc """
Removes the text starting at the first given position up to (but not including) the
character at the last position.

This function puts the current insertion point position at `to` as a side effect.
""".
-spec remove(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_Remove).

-doc """
Replaces the text starting at the first position up to (but not including) the character
at the last position with the given text.

This function puts the current insertion point position at `to` as a side effect.
""".
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,From,To,Value)
 when is_integer(From),is_integer(To),?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,From,To,Value_UC,?get_env(),?wxTextCtrl_Replace).

-doc(#{equiv => saveFile(This, [])}).
-spec saveFile(This) -> boolean() when
	This::wxTextCtrl().

saveFile(This)
 when is_record(This, wx_ref) ->
  saveFile(This, []).

-doc """
Saves the contents of the control in a text file.

Return: true if the operation was successful, false otherwise.
""".
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

-doc """
Changes the default style to use for the new text which is going to be added to the
control.

This applies both to the text added programmatically using `writeText/2` or `appendText/2` and to the text entered
by the user interactively.

If either of the font, foreground, or background colour is not set in `style`, the values
of the previous default style are used for them. If the previous default style didn't set
them neither, the global font or colours of the text control itself are used as fall back.

However if the `style` parameter is the default `m:wxTextAttr`, then the default style is
just reset (instead of being combined with the new style which wouldn't change it at all).

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See: `getDefaultStyle/1`
""".
-spec setDefaultStyle(This, Style) -> boolean() when
	This::wxTextCtrl(), Style::wxTextAttr:wxTextAttr().
setDefaultStyle(#wx_ref{type=ThisT}=This,#wx_ref{type=StyleT}=Style) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxTextCtrl_SetDefaultStyle),
  wxe_util:rec(?wxTextCtrl_SetDefaultStyle).

-doc """
Makes the text item editable or read-only, overriding the `wxTE\_READONLY` flag.

See: `isEditable/1`
""".
-spec setEditable(This, Editable) -> 'ok' when
	This::wxTextCtrl(), Editable::boolean().
setEditable(#wx_ref{type=ThisT}=This,Editable)
 when is_boolean(Editable) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Editable,?get_env(),?wxTextCtrl_SetEditable).

-doc "Sets the insertion point at the given position.".
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_SetInsertionPoint).

-doc """
Sets the insertion point at the end of the text control.

This is equivalent to calling `setInsertionPoint/2` with `getLastPosition/1` argument.
""".
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxTextCtrl().
setInsertionPointEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_SetInsertionPointEnd).

-doc """
This function sets the maximum number of characters the user can enter into the control.

In other words, it allows limiting the text value length to `len` not counting the
terminating `NUL` character.

If `len` is 0, the previously set max length limit, if any, is discarded and the user may
enter as much text as the underlying native text control widget supports (typically at
least 32Kb). If the user tries to enter more characters into the text control when it
already is filled up to the maximal length, a `wxEVT_TEXT_MAXLEN` event is sent to notify
the program about it (giving it the possibility to show an explanatory message, for
example) and the extra input is discarded.

Note that in wxGTK this function may only be used with single line text controls.
""".
-spec setMaxLength(This, Len) -> 'ok' when
	This::wxTextCtrl(), Len::integer().
setMaxLength(#wx_ref{type=ThisT}=This,Len)
 when is_integer(Len) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Len,?get_env(),?wxTextCtrl_SetMaxLength).

-doc """
Selects the text starting at the first position up to (but not including) the character
at the last position.

If both parameters are equal to -1 all text in the control is selected.

Notice that the insertion point will be moved to `from` by this function.
""".
-spec setSelection(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_SetSelection).

-doc """
Changes the style of the given range.

If any attribute within `style` is not set, the corresponding attribute from `getDefaultStyle/1` is used.

Return: true on success, false if an error occurred (this may also mean that the styles
are not supported under this platform).

See:
* `getStyle/3`

* `m:wxTextAttr`
""".
-spec setStyle(This, Start, End, Style) -> boolean() when
	This::wxTextCtrl(), Start::integer(), End::integer(), Style::wxTextAttr:wxTextAttr().
setStyle(#wx_ref{type=ThisT}=This,Start,End,#wx_ref{type=StyleT}=Style)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Start,End,Style,?get_env(),?wxTextCtrl_SetStyle),
  wxe_util:rec(?wxTextCtrl_SetStyle).

-doc """
Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would return
false immediately after the call to `setValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this function
unless the control value doesn't change at all, in which case the insertion point is left
at its original position.

Note that, unlike most other functions changing the controls values, this function
generates a `wxEVT_TEXT` event. To avoid this you can use `changeValue/2` instead.
""".
-spec setValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
setValue(#wx_ref{type=ThisT}=This,Value)
 when ?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Value_UC,?get_env(),?wxTextCtrl_SetValue).

-doc "Makes the line containing the given position visible.".
-spec showPosition(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
showPosition(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_ShowPosition).

-doc """
If there is an undo facility and the last operation can be undone, undoes the last
operation.

Does nothing if there is no undo facility.
""".
-spec undo(This) -> 'ok' when
	This::wxTextCtrl().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Undo).

-doc """
Writes the text into the text control at the current insertion position.

Remark: Newlines in the text string are the only control characters allowed, and they
will cause appropriate line breaks. See operator<<() and `appendText/2` for more convenient ways of
writing to the window. After the write operation, the insertion point will be at the end
of the inserted text, so subsequent write operations will be appended. To append text
after the user may have interacted with the control, call `setInsertionPointEnd/1` before writing.
""".
-spec writeText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
writeText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_WriteText).

-doc """
Converts the given zero based column and line number to a position.

Return: The position value, or -1 if x or y was invalid.
""".
-spec xYToPosition(This, X, Y) -> integer() when
	This::wxTextCtrl(), X::integer(), Y::integer().
xYToPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxTextCtrl_XYToPosition),
  wxe_util:rec(?wxTextCtrl_XYToPosition).

-doc "Destroys the object".
-spec destroy(This::wxTextCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
