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

-module(wxStyledTextEvent).
-moduledoc """
The type of events sent from `m:wxStyledTextCtrl`.

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxStyledTextEvent](https://docs.wxwidgets.org/3.2/classwx_styled_text_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxStyledTextEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getAlt/1,getControl/1,getDragAllowMove/1,getDragResult/1,getDragText/1,
  getFoldLevelNow/1,getFoldLevelPrev/1,getKey/1,getLParam/1,getLength/1,
  getLine/1,getLinesAdded/1,getListType/1,getMargin/1,getMessage/1,getModificationType/1,
  getModifiers/1,getPosition/1,getShift/1,getText/1,getWParam/1,getX/1,
  getY/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxStyledTextEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxStyledTextEventType() :: 'stc_autocomp_cancelled' | 'stc_autocomp_char_deleted' | 'stc_autocomp_selection' | 'stc_calltip_click' | 'stc_change' | 'stc_charadded' | 'stc_do_drop' | 'stc_doubleclick' | 'stc_drag_over' | 'stc_dwellend' | 'stc_dwellstart' | 'stc_hotspot_click' | 'stc_hotspot_dclick' | 'stc_hotspot_release_click' | 'stc_indicator_click' | 'stc_indicator_release' | 'stc_macrorecord' | 'stc_marginclick' | 'stc_modified' | 'stc_needshown' | 'stc_painted' | 'stc_romodifyattempt' | 'stc_savepointleft' | 'stc_savepointreached' | 'stc_start_drag' | 'stc_styleneeded' | 'stc_updateui' | 'stc_userlistselection' | 'stc_zoom'.
-export_type([wxStyledTextEvent/0, wxStyledText/0, wxStyledTextEventType/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the zero-based text position associated this event.

This method is valid for the following event types:

* `wxEVT_STC_STYLENEEDED`

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MODIFIED`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_NEEDSHOWN`

* `wxEVT_STC_USERLISTSELECTION`

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_CALLTIP_CLICK`

* `wxEVT_STC_AUTOCOMP_SELECTION`

* `wxEVT_STC_AUTOCOMP_SELECTION_CHANGE`

* `wxEVT_STC_AUTOCOMP_COMPLETED`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`
""".
-spec getPosition(This) -> integer() when
	This::wxStyledTextEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetPosition),
  wxe_util:rec(?wxStyledTextEvent_GetPosition).

-doc """
Returns the key code of the key that generated this event.

This method is valid for the following event types:

* `wxEVT_STC_CHARADDED`

* `wxEVT_STC_USERLISTSELECTION`

* `wxEVT_STC_AUTOCOMP_SELECTION`

* `wxEVT_STC_AUTOCOMP_COMPLETED`
""".
-spec getKey(This) -> integer() when
	This::wxStyledTextEvent().
getKey(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetKey),
  wxe_util:rec(?wxStyledTextEvent_GetKey).

-doc """
Returns the modifiers of the key press or mouse click for this event.

The returned value is a bit list that may contain one or more of the following values:

* ?wxSTC\_KEYMOD\_SHIFT

* ?wxSTC\_KEYMOD\_CTRL

* ?wxSTC\_KEYMOD\_ALT

* ?wxSTC\_KEYMOD\_SUPER

* ?wxSTC\_KEYMOD\_META

In addition, the value can be checked for equality with ?wxSTC\_KEYMOD\_NORM to test if
no modifiers are present.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`
""".
-spec getModifiers(This) -> integer() when
	This::wxStyledTextEvent().
getModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetModifiers),
  wxe_util:rec(?wxStyledTextEvent_GetModifiers).

-doc """
Returns the modification type for this event.

The modification type is a bit list that describes the change that generated this event.
It may contain one or more of the following values:

* ?wxSTC\_MOD\_INSERTTEXT

* ?wxSTC\_MOD\_DELETETEXT

* ?wxSTC\_MOD\_CHANGESTYLE

* ?wxSTC\_MOD\_CHANGEFOLD

* ?wxSTC\_PERFORMED\_USER

* ?wxSTC\_PERFORMED\_UNDO

* ?wxSTC\_PERFORMED\_REDO

* ?wxSTC\_MULTISTEPUNDOREDO

* ?wxSTC\_LASTSTEPINUNDOREDO

* ?wxSTC\_MOD\_CHANGEMARKER

* ?wxSTC\_MOD\_BEFOREINSERT

* ?wxSTC\_MOD\_BEFOREDELETE

* ?wxSTC\_MULTILINEUNDOREDO

* ?wxSTC\_STARTACTION

* ?wxSTC\_MOD\_CHANGEINDICATOR

* ?wxSTC\_MOD\_CHANGELINESTATE

* ?wxSTC\_MOD\_CHANGEMARGIN

* ?wxSTC\_MOD\_CHANGEANNOTATION

* ?wxSTC\_MOD\_CONTAINER

* ?wxSTC\_MOD\_LEXERSTATE

* ?wxSTC\_MOD\_INSERTCHECK

* ?wxSTC\_MOD\_CHANGETABSTOPS

This method is valid for `wxEVT_STC_MODIFIED` events.
""".
-spec getModificationType(This) -> integer() when
	This::wxStyledTextEvent().
getModificationType(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetModificationType),
  wxe_util:rec(?wxStyledTextEvent_GetModificationType).

-doc """
Deprecated:

Use `wxCommandEvent:getString/1` instead.
""".
-spec getText(This) -> unicode:charlist() when
	This::wxStyledTextEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetText),
  wxe_util:rec(?wxStyledTextEvent_GetText).

-doc """
Returns the length (number of characters) of this event.

This method is valid for `wxEVT_STC_MODIFIED` and `wxEVT_STC_NEEDSHOWN` events.
""".
-spec getLength(This) -> integer() when
	This::wxStyledTextEvent().
getLength(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLength),
  wxe_util:rec(?wxStyledTextEvent_GetLength).

-doc """
Returns the number of lines added or deleted with this event.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_INSERTTEXT or ?wxSTC\_MOD\_DELETETEXT.
""".
-spec getLinesAdded(This) -> integer() when
	This::wxStyledTextEvent().
getLinesAdded(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLinesAdded),
  wxe_util:rec(?wxStyledTextEvent_GetLinesAdded).

-doc """
Returns zero-based line number for this event.

This method is valid for `wxEVT_STC_DOUBLECLICK` and `wxEVT_STC_MODIFIED` events.
""".
-spec getLine(This) -> integer() when
	This::wxStyledTextEvent().
getLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLine),
  wxe_util:rec(?wxStyledTextEvent_GetLine).

-doc """
Returns the current fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_CHANGEFOLD.
""".
-spec getFoldLevelNow(This) -> integer() when
	This::wxStyledTextEvent().
getFoldLevelNow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetFoldLevelNow),
  wxe_util:rec(?wxStyledTextEvent_GetFoldLevelNow).

-doc """
Returns previous fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of `getModificationType/1` includes
?wxSTC\_MOD\_CHANGEFOLD.
""".
-spec getFoldLevelPrev(This) -> integer() when
	This::wxStyledTextEvent().
getFoldLevelPrev(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetFoldLevelPrev),
  wxe_util:rec(?wxStyledTextEvent_GetFoldLevelPrev).

-doc """
Returns the zero-based index of the margin that generated this event.

This method is valid for `wxEVT_STC_MARGINCLICK` and `wxEVT_STC_MARGIN_RIGHT_CLICK`
events.
""".
-spec getMargin(This) -> integer() when
	This::wxStyledTextEvent().
getMargin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetMargin),
  wxe_util:rec(?wxStyledTextEvent_GetMargin).

-doc """
Returns a message number while a macro is being recorded.

Many of the `m:wxStyledTextCtrl` methods such as `wxStyledTextCtrl:insertText/3` and `wxStyledTextCtrl:paste/1` have an event number associated
with them. This method returns that number while a macro is being recorded so that the
macro can be played back later.

This method is valid for `wxEVT_STC_MACRORECORD` events.
""".
-spec getMessage(This) -> integer() when
	This::wxStyledTextEvent().
getMessage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetMessage),
  wxe_util:rec(?wxStyledTextEvent_GetMessage).

-doc """
Returns value of the WParam field for this event.

This method is valid for `wxEVT_STC_MACRORECORD` events.
""".
-spec getWParam(This) -> integer() when
	This::wxStyledTextEvent().
getWParam(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetWParam),
  wxe_util:rec(?wxStyledTextEvent_GetWParam).

-doc """
Returns the value of the LParam field for this event.

This method is valid for `wxEVT_STC_MACRORECORD` events.
""".
-spec getLParam(This) -> integer() when
	This::wxStyledTextEvent().
getLParam(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLParam),
  wxe_util:rec(?wxStyledTextEvent_GetLParam).

-doc """
Returns the list type for this event.

The list type is an integer passed to a list when it is created with the `wxStyledTextCtrl:userListShow/3` method and can
be used to distinguish lists if more than one is used.

This method is valid for `wxEVT_STC_AUTOCOMP_SELECTION_CHANGE` and `wxEVT_STC_USERLISTSELECTION`
events.
""".
-spec getListType(This) -> integer() when
	This::wxStyledTextEvent().
getListType(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetListType),
  wxe_util:rec(?wxStyledTextEvent_GetListType).

-doc """
Returns the X coordinate of the mouse for this event.

This method is valid for the following event types:

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_START_DRAG`

* `wxEVT_STC_DRAG_OVER`

* `wxEVT_STC_DO_DROP`
""".
-spec getX(This) -> integer() when
	This::wxStyledTextEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetX),
  wxe_util:rec(?wxStyledTextEvent_GetX).

-doc """
Returns the Y coordinate of the mouse for this event.

This method is valid for the following event types:

* `wxEVT_STC_DWELLSTART`

* `wxEVT_STC_DWELLEND`

* `wxEVT_STC_START_DRAG`

* `wxEVT_STC_DRAG_OVER`

* `wxEVT_STC_DO_DROP`
""".
-spec getY(This) -> integer() when
	This::wxStyledTextEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetY),
  wxe_util:rec(?wxStyledTextEvent_GetY).

-doc """
Deprecated:

Use `wxCommandEvent:getString/1` instead.
""".
-spec getDragText(This) -> unicode:charlist() when
	This::wxStyledTextEvent().
getDragText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragText),
  wxe_util:rec(?wxStyledTextEvent_GetDragText).

-doc "".
-spec getDragAllowMove(This) -> boolean() when
	This::wxStyledTextEvent().
getDragAllowMove(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragAllowMove),
  wxe_util:rec(?wxStyledTextEvent_GetDragAllowMove).

-doc """
Returns drag result for this event.

This method is valid for `wxEVT_STC_DRAG_OVER` and `wxEVT_STC_DO_DROP` events.
""".
%%  Res = ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
-spec getDragResult(This) -> wx:wx_enum() when
	This::wxStyledTextEvent().
getDragResult(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragResult),
  wxe_util:rec(?wxStyledTextEvent_GetDragResult).

-doc """
Returns true if the Shift key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`
""".
-spec getShift(This) -> boolean() when
	This::wxStyledTextEvent().
getShift(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetShift),
  wxe_util:rec(?wxStyledTextEvent_GetShift).

-doc """
Returns true if the Control key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`
""".
-spec getControl(This) -> boolean() when
	This::wxStyledTextEvent().
getControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetControl),
  wxe_util:rec(?wxStyledTextEvent_GetControl).

-doc """
Returns true if the Alt key is pressed.

This method is valid for the following event types:

* `wxEVT_STC_DOUBLECLICK`

* `wxEVT_STC_MARGINCLICK`

* `wxEVT_STC_HOTSPOT_CLICK`

* `wxEVT_STC_HOTSPOT_DCLICK`

* `wxEVT_STC_HOTSPOT_RELEASE_CLICK`

* `wxEVT_STC_INDICATOR_CLICK`

* `wxEVT_STC_INDICATOR_RELEASE`

* `wxEVT_STC_MARGIN_RIGHT_CLICK`
""".
-spec getAlt(This) -> boolean() when
	This::wxStyledTextEvent().
getAlt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetAlt),
  wxe_util:rec(?wxStyledTextEvent_GetAlt).

 %% From wxCommandEvent
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
-doc false.
getString(This) -> wxCommandEvent:getString(This).
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
