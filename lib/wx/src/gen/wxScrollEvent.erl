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

-module(wxScrollEvent).
-moduledoc """
A scroll event holds information about events sent from stand-alone scrollbars (see `m:wxScrollBar`)
and sliders (see `m:wxSlider`).

Note that scrolled windows send the `m:wxScrollWinEvent` which does not derive from `m:wxCommandEvent`,
but from `m:wxEvent` directly - don't confuse these two kinds of events and use the event
table macros mentioned below only for the scrollbar-like controls.

The difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED

The EVT_SCROLL_THUMBRELEASE event is only emitted when actually dragging the thumb using
the mouse and releasing it (This EVT_SCROLL_THUMBRELEASE event is also followed by an
EVT_SCROLL_CHANGED event).

The EVT_SCROLL_CHANGED event also occurs when using the keyboard to change the thumb
position, and when clicking next to the thumb (In all these cases the
EVT_SCROLL_THUMBRELEASE event does not happen).

In short, the EVT_SCROLL_CHANGED event is triggered when scrolling/ moving has finished
independently of the way it had started. Please see the page_samples_widgets ("Slider"
page) to see the difference between EVT_SCROLL_THUMBRELEASE and EVT_SCROLL_CHANGED in action.

Remark: Note that unless specifying a scroll control identifier, you will need to test
for scrollbar orientation with `getOrientation/1`, since horizontal and vertical scroll events are processed
using the same event handler.

See:
* `m:wxScrollBar`

* `m:wxSlider`

* `m:wxSpinButton`

* `m:wxScrollWinEvent`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxScrollEvent](https://docs.wxwidgets.org/3.2/classwx_scroll_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxScrollEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getOrientation/1,getPosition/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxScrollEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxScrollEventType() :: 'scroll_top' | 'scroll_bottom' | 'scroll_lineup' | 'scroll_linedown' | 'scroll_pageup' | 'scroll_pagedown' | 'scroll_thumbtrack' | 'scroll_thumbrelease' | 'scroll_changed'.
-export_type([wxScrollEvent/0, wxScroll/0, wxScrollEventType/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Returns wxHORIZONTAL or wxVERTICAL, depending on the orientation of the scrollbar.".
-spec getOrientation(This) -> integer() when
	This::wxScrollEvent().
getOrientation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrollEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxScrollEvent_GetOrientation),
  wxe_util:rec(?wxScrollEvent_GetOrientation).

-doc "Returns the position of the scrollbar.".
-spec getPosition(This) -> integer() when
	This::wxScrollEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxScrollEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxScrollEvent_GetPosition),
  wxe_util:rec(?wxScrollEvent_GetPosition).

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
