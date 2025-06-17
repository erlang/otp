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

-module(wxPaintEvent).
-moduledoc """
A paint event is sent when a window's contents needs to be repainted.

The handler of this event must create a `m:wxPaintDC` object and use it for painting the
window contents. For example:

Notice that you must `not` create other kinds of `m:wxDC` (e.g. `m:wxClientDC` or `m:wxWindowDC`)
in EVT_PAINT handlers and also don't create `m:wxPaintDC` outside of this event handlers.

You can optimize painting by retrieving the rectangles that have been damaged and only
repainting these. The rectangles are in terms of the client area, and are unscrolled, so
you will need to do some calculations using the current view position to obtain logical,
scrolled units. Here is an example of using the `wxRegionIterator` (not implemented in wx)
class:

Remark: Please notice that in general it is impossible to change the drawing of a
standard control (such as `m:wxButton`) and so you shouldn't attempt to handle paint
events for them as even if it might work on some platforms, this is inherently not
portable and won't work everywhere.

See: [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxPaintEvent](https://docs.wxwidgets.org/3.2/classwx_paint_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxPaintEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxPaintEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxPaintEventType() :: 'paint'.
-export_type([wxPaintEvent/0, wxPaint/0, wxPaintEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

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
