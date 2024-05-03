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

-module(wxTextCtrl).
-moduledoc """
Functions for wxTextCtrl class

A text control allows text to be displayed and edited.

It may be single line or multi-line. Notice that a lot of methods of the text
controls are found in the base `wxTextEntry` (not implemented in wx) class which
is a common base class for `m:wxTextCtrl` and other controls using a single line
text entry field (e.g. `m:wxComboBox`).

Styles

This class supports the following styles:

wxTextCtrl Text Format

The multiline text controls always store the text as a sequence of lines
separated by `'\n'` characters, i.e. in the Unix text format even on non-Unix
platforms. This allows the user code to ignore the differences between the
platforms but at a price: the indices in the control such as those returned by
`getInsertionPoint/1` or `getSelection/1` can `not` be used as indices into the
string returned by `getValue/1` as they're going to be slightly off for
platforms using `"\\r\\n"` as separator (as Windows does).

Instead, if you need to obtain a substring between the 2 indices obtained from
the control with the help of the functions mentioned above, you should use
`getRange/3`. And the indices themselves can only be passed to other methods,
for example `setInsertionPoint/2` or `setSelection/3`.

To summarize: never use the indices returned by (multiline) `m:wxTextCtrl` as
indices into the string it contains, but only as arguments to be passed back to
the other `m:wxTextCtrl` methods. This problem doesn't arise for single-line
platforms however where the indices in the control do correspond to the
positions in the value string.

wxTextCtrl Positions and Coordinates

It is possible to use either linear positions, i.e. roughly (but `not` always
exactly, as explained in the previous section) the index of the character in the
text contained in the control or X-Y coordinates, i.e. column and line of the
character when working with this class and it provides the functions
`positionToXY/2` and `xYToPosition/3` to convert between the two.

Additionally, a position in the control can be converted to its coordinates in
pixels using `PositionToCoords()` (not implemented in wx) which can be useful to
e.g. show a popup menu near the given character. And, in the other direction,
`HitTest()` (not implemented in wx) can be used to find the character under, or
near, the given pixel coordinates.

To be more precise, positions actually refer to the gaps between characters and
not the characters themselves. Thus, position 0 is the one before the very first
character in the control and so is a valid position even when the control is
empty. And if the control contains a single character, it has two valid
positions: 0 before this character and 1 - after it. This, when the
documentation of various functions mentions "invalid position", it doesn't
consider the position just after the last character of the line to be invalid,
only the positions beyond that one (e.g. 2 and greater in the single character
example) are actually invalid.

wxTextCtrl Styles.

Multi-line text controls support styling, i.e. provide a possibility to set
colours and font for individual characters in it (note that under Windows
`wxTE_RICH` style is required for style support). To use the styles you can
either call `setDefaultStyle/2` before inserting the text or call `setStyle/4`
later to change the style of the text already in the control (the first solution
is much more efficient).

In either case, if the style doesn't specify some of the attributes (for example
you only want to set the text colour but without changing the font nor the text
background), the values of the default style will be used for them. If there is
no default style, the attributes of the text control itself are used.

So the following code correctly describes what it does: the second call to
`setDefaultStyle/2` doesn't change the text foreground colour (which stays red)
while the last one doesn't change the background colour (which stays grey):

wxTextCtrl and C++ Streams

This class multiply-inherits from `std::streambuf` (except for some really old
compilers using non-standard iostream library), allowing code such as the
following:

Note that even if your build of wxWidgets doesn't support this (the symbol
`wxHAS_TEXT_WINDOW_STREAM` has value of 0 then) you can still use `m:wxTextCtrl`
itself in a stream-like manner:

However the possibility to create a `std::ostream` associated with
`m:wxTextCtrl` may be useful if you need to redirect the output of a function
taking a `std::ostream` as parameter to a text control.

Another commonly requested need is to redirect `std::cout` to the text control.
This may be done in the following way:

But wxWidgets provides a convenient class to make it even simpler so instead you
may just do

See `wxStreamToTextRedirector` (not implemented in wx) for more details.

Event Handling.

The following commands are processed by default event handlers in
`m:wxTextCtrl`: `wxID_CUT`, `wxID_COPY`, `wxID_PASTE`, `wxID_UNDO`, `wxID_REDO`.
The associated UI update events are also processed automatically, when the
control has the focus.

See: `create/4`, `wxValidator` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow`
`m:wxEvtHandler`

wxWidgets docs:
[wxTextCtrl](https://docs.wxwidgets.org/3.1/classwx_text_ctrl.html)

## Events

Event types emitted from this class:
[`command_text_updated`](`m:wxCommandEvent`),
[`command_text_enter`](`m:wxCommandEvent`), [`text_maxlen`](`m:wxCommandEvent`)
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
%% @hidden
-doc false.
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl">external documentation</a>.
-doc "Default ctor.".
-spec new() -> wxTextCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTextCtrl_new_0),
  wxe_util:rec(?wxTextCtrl_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxTextCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl">external documentation</a>.
-doc """
Constructor, creating and showing a text control.

Remark: The horizontal scrollbar (wxHSCROLL style flag) will only be created for
multi-line text controls. Without a horizontal scrollbar, text lines that don't
fit in the control's size will be wrapped (but no newline character is
inserted). Single line controls don't have a horizontal scrollbar, the text is
automatically scrolled so that the insertion point is always visible.

See: `create/4`, `wxValidator` (not implemented in wx)
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlappendtext">external documentation</a>.
-doc """
Appends the text to the end of the text control.

Remark: After the text is appended, the insertion point will be at the end of
the text control. If this behaviour is not desired, the programmer should use
`getInsertionPoint/1` and `setInsertionPoint/2`.

See: `writeText/2`
""".
-spec appendText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
appendText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_AppendText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancopy">external documentation</a>.
-doc "Returns true if the selection can be copied to the clipboard.".
-spec canCopy(This) -> boolean() when
	This::wxTextCtrl().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCopy),
  wxe_util:rec(?wxTextCtrl_CanCopy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcancut">external documentation</a>.
-doc "Returns true if the selection can be cut to the clipboard.".
-spec canCut(This) -> boolean() when
	This::wxTextCtrl().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanCut),
  wxe_util:rec(?wxTextCtrl_CanCut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanpaste">external documentation</a>.
-doc """
Returns true if the contents of the clipboard can be pasted into the text
control.

On some platforms (Motif, GTK) this is an approximation and returns true if the
control is editable, false otherwise.
""".
-spec canPaste(This) -> boolean() when
	This::wxTextCtrl().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanPaste),
  wxe_util:rec(?wxTextCtrl_CanPaste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanredo">external documentation</a>.
-doc """
Returns true if there is a redo facility available and the last operation can be
redone.
""".
-spec canRedo(This) -> boolean() when
	This::wxTextCtrl().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanRedo),
  wxe_util:rec(?wxTextCtrl_CanRedo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcanundo">external documentation</a>.
-doc """
Returns true if there is an undo facility available and the last operation can
be undone.
""".
-spec canUndo(This) -> boolean() when
	This::wxTextCtrl().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_CanUndo),
  wxe_util:rec(?wxTextCtrl_CanUndo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlclear">external documentation</a>.
-doc """
Clears the text in the control.

Note that this function will generate a `wxEVT_TEXT` event, i.e. its effect is
identical to calling `SetValue`("").
""".
-spec clear(This) -> 'ok' when
	This::wxTextCtrl().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcopy">external documentation</a>.
-doc "Copies the selected text to the clipboard.".
-spec copy(This) -> 'ok' when
	This::wxTextCtrl().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Copy).

%% @equiv create(This,Parent,Id, [])
-spec create(This, Parent, Id) -> boolean() when
	This::wxTextCtrl(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcreate">external documentation</a>.
-doc """
Creates the text control for two-step construction.

This method should be called if the default constructor was used for the control
creation. Its parameters have the same meaning as for the non-default
constructor.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlcut">external documentation</a>.
-doc "Copies the selected text to the clipboard and removes it from the control.".
-spec cut(This) -> 'ok' when
	This::wxTextCtrl().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Cut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrldiscardedits">external documentation</a>.
-doc "Resets the internal modified flag as if the current changes had been saved.".
-spec discardEdits(This) -> 'ok' when
	This::wxTextCtrl().
discardEdits(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_DiscardEdits).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlchangevalue">external documentation</a>.
-doc """
Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would
return false immediately after the call to `changeValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this
function.

This functions does not generate the `wxEVT_TEXT` event but otherwise is
identical to `setValue/2`.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlemulatekeypress">external documentation</a>.
-doc """
This function inserts into the control the character which would have been
inserted if the given key event had occurred in the text control.

The `event` object should be the same as the one passed to `EVT_KEY_DOWN`
handler previously by wxWidgets. Please note that this function doesn't
currently work correctly for all keys under any platform but MSW.

Return: true if the event resulted in a change to the control, false otherwise.
""".
-spec emulateKeyPress(This, Event) -> boolean() when
	This::wxTextCtrl(), Event::wxKeyEvent:wxKeyEvent().
emulateKeyPress(#wx_ref{type=ThisT}=This,#wx_ref{type=EventT}=Event) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(EventT,wxKeyEvent),
  wxe_util:queue_cmd(This,Event,?get_env(),?wxTextCtrl_EmulateKeyPress),
  wxe_util:rec(?wxTextCtrl_EmulateKeyPress).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetdefaultstyle">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetinsertionpoint">external documentation</a>.
-doc """
Returns the insertion point, or cursor, position.

This is defined as the zero based index of the character position to the right
of the insertion point. For example, if the insertion point is at the end of the
single-line text control, it is equal to `getLastPosition/1`.

Notice that insertion position is, in general, different from the index of the
character the cursor position at in the string returned by `getValue/1`. While
this is always the case for the single line controls, multi-line controls can
use two characters `"\\r\\n"` as line separator (this is notably the case under
MSW) meaning that indices in the control and its string value are offset by 1
for every line.

Hence to correctly get the character at the current cursor position, taking into
account that there can be none if the cursor is at the end of the string, you
could do the following:
""".
-spec getInsertionPoint(This) -> integer() when
	This::wxTextCtrl().
getInsertionPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetInsertionPoint),
  wxe_util:rec(?wxTextCtrl_GetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlastposition">external documentation</a>.
-doc """
Returns the zero based index of the last position in the text control, which is
equal to the number of characters in the control.
""".
-spec getLastPosition(This) -> integer() when
	This::wxTextCtrl().
getLastPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetLastPosition),
  wxe_util:rec(?wxTextCtrl_GetLastPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinelength">external documentation</a>.
-doc """
Gets the length of the specified line, not including any trailing newline
character(s).

Return: The length of the line, or -1 if `lineNo` was invalid.
""".
-spec getLineLength(This, LineNo) -> integer() when
	This::wxTextCtrl(), LineNo::integer().
getLineLength(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineLength),
  wxe_util:rec(?wxTextCtrl_GetLineLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetlinetext">external documentation</a>.
-doc """
Returns the contents of a given line in the text control, not including any
trailing newline character(s).

Return: The contents of the line.
""".
-spec getLineText(This, LineNo) -> unicode:charlist() when
	This::wxTextCtrl(), LineNo::integer().
getLineText(#wx_ref{type=ThisT}=This,LineNo)
 when is_integer(LineNo) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,LineNo,?get_env(),?wxTextCtrl_GetLineText),
  wxe_util:rec(?wxTextCtrl_GetLineText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetnumberoflines">external documentation</a>.
-doc """
Returns the number of lines in the text control buffer.

The returned number is the number of logical lines, i.e. just the count of the
number of newline characters in the control + 1, for wxGTK and wxOSX/Cocoa ports
while it is the number of physical lines, i.e. the count of lines actually shown
in the control, in wxMSW. Because of this discrepancy, it is not recommended to
use this function.

Remark: Note that even empty text controls have one line (where the insertion
point is), so `getNumberOfLines/1` never returns 0.
""".
-spec getNumberOfLines(This) -> integer() when
	This::wxTextCtrl().
getNumberOfLines(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetNumberOfLines),
  wxe_util:rec(?wxTextCtrl_GetNumberOfLines).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetrange">external documentation</a>.
-doc """
Returns the string containing the text starting in the positions `from` and up
to `to` in the control.

The positions must have been returned by another `m:wxTextCtrl` method. Please
note that the positions in a multiline `m:wxTextCtrl` do `not` correspond to the
indices in the string returned by `getValue/1` because of the different new line
representations (`CR` or `CR` LF) and so this method should be used to obtain
the correct results instead of extracting parts of the entire value. It may also
be more efficient, especially if the control contains a lot of data.
""".
-spec getRange(This, From, To) -> unicode:charlist() when
	This::wxTextCtrl(), From::integer(), To::integer().
getRange(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_GetRange),
  wxe_util:rec(?wxTextCtrl_GetRange).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetselection">external documentation</a>.
-doc """
Gets the current selection span.

If the returned values are equal, there was no selection. Please note that the
indices returned may be used with the other `m:wxTextCtrl` methods but don't
necessarily represent the correct indices into the string returned by
`getValue/1` for multiline controls under Windows (at least,) you should use
`getStringSelection/1` to get the selected text.
""".
-spec getSelection(This) -> {From::integer(), To::integer()} when
	This::wxTextCtrl().
getSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetSelection),
  wxe_util:rec(?wxTextCtrl_GetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstringselection">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetstyle">external documentation</a>.
-doc """
Returns the style at this position in the text control.

Not all platforms support this function.

Return: true on success, false if an error occurred (this may also mean that the
styles are not supported under this platform).

See: `setStyle/4`, `m:wxTextAttr`
""".
-spec getStyle(This, Position, Style) -> boolean() when
	This::wxTextCtrl(), Position::integer(), Style::wxTextAttr:wxTextAttr().
getStyle(#wx_ref{type=ThisT}=This,Position,#wx_ref{type=StyleT}=Style)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Position,Style,?get_env(),?wxTextCtrl_GetStyle),
  wxe_util:rec(?wxTextCtrl_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlgetvalue">external documentation</a>.
-doc """
Gets the contents of the control.

Notice that for a multiline text control, the lines will be separated by
(Unix-style) `\n` characters, even under Windows where they are separated by a
`\r\n` sequence in the native control.
""".
-spec getValue(This) -> unicode:charlist() when
	This::wxTextCtrl().
getValue(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_GetValue),
  wxe_util:rec(?wxTextCtrl_GetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrliseditable">external documentation</a>.
-doc """
Returns true if the controls contents may be edited by user (note that it always
can be changed by the program).

In other words, this functions returns true if the control hasn't been put in
read-only mode by a previous call to `setEditable/2`.
""".
-spec isEditable(This) -> boolean() when
	This::wxTextCtrl().
isEditable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsEditable),
  wxe_util:rec(?wxTextCtrl_IsEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismodified">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlismultiline">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlissingleline">external documentation</a>.
-doc """
Returns true if this is a single line edit control and false otherwise.

See: `isSingleLine/1`, `isMultiLine/1`
""".
-spec isSingleLine(This) -> boolean() when
	This::wxTextCtrl().
isSingleLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_IsSingleLine),
  wxe_util:rec(?wxTextCtrl_IsSingleLine).

%% @equiv loadFile(This,Filename, [])
-spec loadFile(This, Filename) -> boolean() when
	This::wxTextCtrl(), Filename::unicode:chardata().

loadFile(This,Filename)
 when is_record(This, wx_ref),?is_chardata(Filename) ->
  loadFile(This,Filename, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlloadfile">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlmarkdirty">external documentation</a>.
-doc """
Mark text as modified (dirty).

See: `isModified/1`
""".
-spec markDirty(This) -> 'ok' when
	This::wxTextCtrl().
markDirty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_MarkDirty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpaste">external documentation</a>.
-doc "Pastes text from the clipboard to the text item.".
-spec paste(This) -> 'ok' when
	This::wxTextCtrl().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Paste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlpositiontoxy">external documentation</a>.
-doc """
Converts given position to a zero-based column, line number pair.

Return: true on success, false on failure (most likely due to a too large
position parameter).

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlredo">external documentation</a>.
-doc """
If there is a redo facility and the last operation can be redone, redoes the
last operation.

Does nothing if there is no redo facility.
""".
-spec redo(This) -> 'ok' when
	This::wxTextCtrl().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Redo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlremove">external documentation</a>.
-doc """
Removes the text starting at the first given position up to (but not including)
the character at the last position.

This function puts the current insertion point position at `to` as a side
effect.
""".
-spec remove(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
remove(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_Remove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlreplace">external documentation</a>.
-doc """
Replaces the text starting at the first position up to (but not including) the
character at the last position with the given text.

This function puts the current insertion point position at `to` as a side
effect.
""".
-spec replace(This, From, To, Value) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer(), Value::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,From,To,Value)
 when is_integer(From),is_integer(To),?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,From,To,Value_UC,?get_env(),?wxTextCtrl_Replace).

%% @equiv saveFile(This, [])
-spec saveFile(This) -> boolean() when
	This::wxTextCtrl().

saveFile(This)
 when is_record(This, wx_ref) ->
  saveFile(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsavefile">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetdefaultstyle">external documentation</a>.
-doc """
Changes the default style to use for the new text which is going to be added to
the control.

This applies both to the text added programmatically using `writeText/2` or
`appendText/2` and to the text entered by the user interactively.

If either of the font, foreground, or background colour is not set in `style`,
the values of the previous default style are used for them. If the previous
default style didn't set them neither, the global font or colours of the text
control itself are used as fall back.

However if the `style` parameter is the default `m:wxTextAttr`, then the default
style is just reset (instead of being combined with the new style which wouldn't
change it at all).

Return: true on success, false if an error occurred (this may also mean that the
styles are not supported under this platform).

See: `getDefaultStyle/1`
""".
-spec setDefaultStyle(This, Style) -> boolean() when
	This::wxTextCtrl(), Style::wxTextAttr:wxTextAttr().
setDefaultStyle(#wx_ref{type=ThisT}=This,#wx_ref{type=StyleT}=Style) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxTextCtrl_SetDefaultStyle),
  wxe_util:rec(?wxTextCtrl_SetDefaultStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlseteditable">external documentation</a>.
-doc """
Makes the text item editable or read-only, overriding the `wxTE_READONLY` flag.

See: `isEditable/1`
""".
-spec setEditable(This, Editable) -> 'ok' when
	This::wxTextCtrl(), Editable::boolean().
setEditable(#wx_ref{type=ThisT}=This,Editable)
 when is_boolean(Editable) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Editable,?get_env(),?wxTextCtrl_SetEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpoint">external documentation</a>.
-doc "Sets the insertion point at the given position.".
-spec setInsertionPoint(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
setInsertionPoint(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_SetInsertionPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetinsertionpointend">external documentation</a>.
-doc """
Sets the insertion point at the end of the text control.

This is equivalent to calling `setInsertionPoint/2` with `getLastPosition/1`
argument.
""".
-spec setInsertionPointEnd(This) -> 'ok' when
	This::wxTextCtrl().
setInsertionPointEnd(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_SetInsertionPointEnd).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetmaxlength">external documentation</a>.
-doc """
This function sets the maximum number of characters the user can enter into the
control.

In other words, it allows limiting the text value length to `len` not counting
the terminating `NUL` character.

If `len` is 0, the previously set max length limit, if any, is discarded and the
user may enter as much text as the underlying native text control widget
supports (typically at least 32Kb). If the user tries to enter more characters
into the text control when it already is filled up to the maximal length, a
`wxEVT_TEXT_MAXLEN` event is sent to notify the program about it (giving it the
possibility to show an explanatory message, for example) and the extra input is
discarded.

Note that in wxGTK this function may only be used with single line text
controls.
""".
-spec setMaxLength(This, Len) -> 'ok' when
	This::wxTextCtrl(), Len::integer().
setMaxLength(#wx_ref{type=ThisT}=This,Len)
 when is_integer(Len) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Len,?get_env(),?wxTextCtrl_SetMaxLength).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetselection">external documentation</a>.
-doc """
Selects the text starting at the first position up to (but not including) the
character at the last position.

If both parameters are equal to -1 all text in the control is selected.

Notice that the insertion point will be moved to `from` by this function.

See: `SelectAll()` (not implemented in wx)
""".
-spec setSelection(This, From, To) -> 'ok' when
	This::wxTextCtrl(), From::integer(), To::integer().
setSelection(#wx_ref{type=ThisT}=This,From,To)
 when is_integer(From),is_integer(To) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,From,To,?get_env(),?wxTextCtrl_SetSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetstyle">external documentation</a>.
-doc """
Changes the style of the given range.

If any attribute within `style` is not set, the corresponding attribute from
`getDefaultStyle/1` is used.

Return: true on success, false if an error occurred (this may also mean that the
styles are not supported under this platform).

See: `getStyle/3`, `m:wxTextAttr`
""".
-spec setStyle(This, Start, End, Style) -> boolean() when
	This::wxTextCtrl(), Start::integer(), End::integer(), Style::wxTextAttr:wxTextAttr().
setStyle(#wx_ref{type=ThisT}=This,Start,End,#wx_ref{type=StyleT}=Style)
 when is_integer(Start),is_integer(End) ->
  ?CLASS(ThisT,wxTextCtrl),
  ?CLASS(StyleT,wxTextAttr),
  wxe_util:queue_cmd(This,Start,End,Style,?get_env(),?wxTextCtrl_SetStyle),
  wxe_util:rec(?wxTextCtrl_SetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlsetvalue">external documentation</a>.
-doc """
Sets the new text control value.

It also marks the control as not-modified which means that IsModified() would
return false immediately after the call to `setValue/2`.

The insertion point is set to the start of the control (i.e. position 0) by this
function unless the control value doesn't change at all, in which case the
insertion point is left at its original position.

Note that, unlike most other functions changing the controls values, this
function generates a `wxEVT_TEXT` event. To avoid this you can use
`changeValue/2` instead.
""".
-spec setValue(This, Value) -> 'ok' when
	This::wxTextCtrl(), Value::unicode:chardata().
setValue(#wx_ref{type=ThisT}=This,Value)
 when ?is_chardata(Value) ->
  ?CLASS(ThisT,wxTextCtrl),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(This,Value_UC,?get_env(),?wxTextCtrl_SetValue).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlshowposition">external documentation</a>.
-doc "Makes the line containing the given position visible.".
-spec showPosition(This, Pos) -> 'ok' when
	This::wxTextCtrl(), Pos::integer().
showPosition(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxTextCtrl_ShowPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlundo">external documentation</a>.
-doc """
If there is an undo facility and the last operation can be undone, undoes the
last operation.

Does nothing if there is no undo facility.
""".
-spec undo(This) -> 'ok' when
	This::wxTextCtrl().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxTextCtrl_Undo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlwritetext">external documentation</a>.
-doc """
Writes the text into the text control at the current insertion position.

Remark: Newlines in the text string are the only control characters allowed, and
they will cause appropriate line breaks. See operator<<() and `appendText/2` for
more convenient ways of writing to the window. After the write operation, the
insertion point will be at the end of the inserted text, so subsequent write
operations will be appended. To append text after the user may have interacted
with the control, call `setInsertionPointEnd/1` before writing.
""".
-spec writeText(This, Text) -> 'ok' when
	This::wxTextCtrl(), Text::unicode:chardata().
writeText(#wx_ref{type=ThisT}=This,Text)
 when ?is_chardata(Text) ->
  ?CLASS(ThisT,wxTextCtrl),
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(This,Text_UC,?get_env(),?wxTextCtrl_WriteText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextctrl.html#wxtextctrlxytoposition">external documentation</a>.
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

%% @doc Destroys this object, do not use object again
-doc "Destructor, destroying the text control.".
-spec destroy(This::wxTextCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
