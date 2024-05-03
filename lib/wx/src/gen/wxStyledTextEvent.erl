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

-module(wxStyledTextEvent).
-moduledoc """
Functions for wxStyledTextEvent class

The type of events sent from `m:wxStyledTextCtrl`.

This class is derived (and can use functions) from: `m:wxCommandEvent`
`m:wxEvent`

wxWidgets docs:
[wxStyledTextEvent](https://docs.wxwidgets.org/3.1/classwx_styled_text_event.html)

## Events

Use `wxEvtHandler:connect/3` with
[`wxStyledTextEventType`](`t:wxStyledTextEventType/0`) to subscribe to events of
this type.
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
%% @hidden
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetposition">external documentation</a>.
-doc """
Returns the zero-based text position associated this event.

This method is valid for the following event types:
""".
-spec getPosition(This) -> integer() when
	This::wxStyledTextEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetPosition),
  wxe_util:rec(?wxStyledTextEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetkey">external documentation</a>.
-doc """
Returns the key code of the key that generated this event.

This method is valid for the following event types:
""".
-spec getKey(This) -> integer() when
	This::wxStyledTextEvent().
getKey(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetKey),
  wxe_util:rec(?wxStyledTextEvent_GetKey).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetmodifiers">external documentation</a>.
-doc """
Returns the modifiers of the key press or mouse click for this event.

The returned value is a bit list that may contain one or more of the following
values:

In addition, the value can be checked for equality with ?wxSTC_KEYMOD_NORM to
test if no modifiers are present.

This method is valid for the following event types:
""".
-spec getModifiers(This) -> integer() when
	This::wxStyledTextEvent().
getModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetModifiers),
  wxe_util:rec(?wxStyledTextEvent_GetModifiers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetmodificationtype">external documentation</a>.
-doc """
Returns the modification type for this event.

The modification type is a bit list that describes the change that generated
this event. It may contain one or more of the following values:

This method is valid for `wxEVT_STC_MODIFIED` events.
""".
-spec getModificationType(This) -> integer() when
	This::wxStyledTextEvent().
getModificationType(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetModificationType),
  wxe_util:rec(?wxStyledTextEvent_GetModificationType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgettext">external documentation</a>.
-doc "Deprecated: Use `wxCommandEvent:getString/1` instead.".
-spec getText(This) -> unicode:charlist() when
	This::wxStyledTextEvent().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetText),
  wxe_util:rec(?wxStyledTextEvent_GetText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetlength">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetlinesadded">external documentation</a>.
-doc """
Returns the number of lines added or deleted with this event.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of
`getModificationType/1` includes ?wxSTC_MOD_INSERTTEXT or ?wxSTC_MOD_DELETETEXT.
""".
-spec getLinesAdded(This) -> integer() when
	This::wxStyledTextEvent().
getLinesAdded(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLinesAdded),
  wxe_util:rec(?wxStyledTextEvent_GetLinesAdded).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetline">external documentation</a>.
-doc """
Returns zero-based line number for this event.

This method is valid for `wxEVT_STC_DOUBLECLICK` and `wxEVT_STC_MODIFIED`
events.
""".
-spec getLine(This) -> integer() when
	This::wxStyledTextEvent().
getLine(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetLine),
  wxe_util:rec(?wxStyledTextEvent_GetLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetfoldlevelnow">external documentation</a>.
-doc """
Returns the current fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of
`getModificationType/1` includes ?wxSTC_MOD_CHANGEFOLD.
""".
-spec getFoldLevelNow(This) -> integer() when
	This::wxStyledTextEvent().
getFoldLevelNow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetFoldLevelNow),
  wxe_util:rec(?wxStyledTextEvent_GetFoldLevelNow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetfoldlevelprev">external documentation</a>.
-doc """
Returns previous fold level for the line.

This method is valid for `wxEVT_STC_MODIFIED` events when the result of
`getModificationType/1` includes ?wxSTC_MOD_CHANGEFOLD.
""".
-spec getFoldLevelPrev(This) -> integer() when
	This::wxStyledTextEvent().
getFoldLevelPrev(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetFoldLevelPrev),
  wxe_util:rec(?wxStyledTextEvent_GetFoldLevelPrev).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetmargin">external documentation</a>.
-doc """
Returns the zero-based index of the margin that generated this event.

This method is valid for `wxEVT_STC_MARGINCLICK` and
`wxEVT_STC_MARGIN_RIGHT_CLICK` events.
""".
-spec getMargin(This) -> integer() when
	This::wxStyledTextEvent().
getMargin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetMargin),
  wxe_util:rec(?wxStyledTextEvent_GetMargin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetmessage">external documentation</a>.
-doc """
Returns a message number while a macro is being recorded.

Many of the `m:wxStyledTextCtrl` methods such as `wxStyledTextCtrl:insertText/3`
and `wxStyledTextCtrl:paste/1` have an event number associated with them. This
method returns that number while a macro is being recorded so that the macro can
be played back later.

This method is valid for `wxEVT_STC_MACRORECORD` events.
""".
-spec getMessage(This) -> integer() when
	This::wxStyledTextEvent().
getMessage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetMessage),
  wxe_util:rec(?wxStyledTextEvent_GetMessage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetwparam">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetlparam">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetlisttype">external documentation</a>.
-doc """
Returns the list type for this event.

The list type is an integer passed to a list when it is created with the
`wxStyledTextCtrl:userListShow/3` method and can be used to distinguish lists if
more than one is used.

This method is valid for `wxEVT_STC_AUTOCOMP_SELECTION_CHANGE` and
`wxEVT_STC_USERLISTSELECTION` events.
""".
-spec getListType(This) -> integer() when
	This::wxStyledTextEvent().
getListType(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetListType),
  wxe_util:rec(?wxStyledTextEvent_GetListType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetx">external documentation</a>.
-doc """
Returns the X coordinate of the mouse for this event.

This method is valid for the following event types:
""".
-spec getX(This) -> integer() when
	This::wxStyledTextEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetX),
  wxe_util:rec(?wxStyledTextEvent_GetX).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgety">external documentation</a>.
-doc """
Returns the Y coordinate of the mouse for this event.

This method is valid for the following event types:
""".
-spec getY(This) -> integer() when
	This::wxStyledTextEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetY),
  wxe_util:rec(?wxStyledTextEvent_GetY).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetdragtext">external documentation</a>.
-doc "Deprecated: Use `wxCommandEvent:getString/1` instead.".
-spec getDragText(This) -> unicode:charlist() when
	This::wxStyledTextEvent().
getDragText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragText),
  wxe_util:rec(?wxStyledTextEvent_GetDragText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetdragallowmove">external documentation</a>.
-spec getDragAllowMove(This) -> boolean() when
	This::wxStyledTextEvent().
getDragAllowMove(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragAllowMove),
  wxe_util:rec(?wxStyledTextEvent_GetDragAllowMove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetdragresult">external documentation</a>.
%%<br /> Res = ?wxDragError | ?wxDragNone | ?wxDragCopy | ?wxDragMove | ?wxDragLink | ?wxDragCancel
-doc """
Returns drag result for this event.

This method is valid for `wxEVT_STC_DRAG_OVER` and `wxEVT_STC_DO_DROP` events.
""".
-spec getDragResult(This) -> wx:wx_enum() when
	This::wxStyledTextEvent().
getDragResult(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetDragResult),
  wxe_util:rec(?wxStyledTextEvent_GetDragResult).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetshift">external documentation</a>.
-doc """
Returns true if the Shift key is pressed.

This method is valid for the following event types:
""".
-spec getShift(This) -> boolean() when
	This::wxStyledTextEvent().
getShift(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetShift),
  wxe_util:rec(?wxStyledTextEvent_GetShift).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetcontrol">external documentation</a>.
-doc """
Returns true if the Control key is pressed.

This method is valid for the following event types:
""".
-spec getControl(This) -> boolean() when
	This::wxStyledTextEvent().
getControl(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetControl),
  wxe_util:rec(?wxStyledTextEvent_GetControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstyledtextevent.html#wxstyledtexteventgetalt">external documentation</a>.
-doc """
Returns true if the Alt key is pressed.

This method is valid for the following event types:
""".
-spec getAlt(This) -> boolean() when
	This::wxStyledTextEvent().
getAlt(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStyledTextEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxStyledTextEvent_GetAlt),
  wxe_util:rec(?wxStyledTextEvent_GetAlt).

 %% From wxCommandEvent
%% @hidden
-doc false.
setString(This,String) -> wxCommandEvent:setString(This,String).
%% @hidden
-doc false.
setInt(This,IntCommand) -> wxCommandEvent:setInt(This,IntCommand).
%% @hidden
-doc false.
isSelection(This) -> wxCommandEvent:isSelection(This).
%% @hidden
-doc false.
isChecked(This) -> wxCommandEvent:isChecked(This).
%% @hidden
-doc false.
getString(This) -> wxCommandEvent:getString(This).
%% @hidden
-doc false.
getSelection(This) -> wxCommandEvent:getSelection(This).
%% @hidden
-doc false.
getInt(This) -> wxCommandEvent:getInt(This).
%% @hidden
-doc false.
getExtraLong(This) -> wxCommandEvent:getExtraLong(This).
%% @hidden
-doc false.
getClientData(This) -> wxCommandEvent:getClientData(This).
 %% From wxEvent
%% @hidden
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
-doc false.
skip(This) -> wxEvent:skip(This).
%% @hidden
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
-doc false.
getId(This) -> wxEvent:getId(This).
