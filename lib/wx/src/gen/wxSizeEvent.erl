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

-module(wxSizeEvent).
-moduledoc """
Functions for wxSizeEvent class

A size event holds information about size change events of `m:wxWindow`.

The EVT_SIZE handler function will be called when the window has been resized.

You may wish to use this for frames to resize their child windows as
appropriate.

Note that the size passed is of the whole window: call
`wxWindow:getClientSize/1` for the area which may be used by the application.

When a window is resized, usually only a small part of the window is damaged and
you may only need to repaint that area. However, if your drawing depends on the
size of the window, you may need to clear the DC explicitly and repaint the
whole window. In which case, you may need to call `wxWindow:refresh/2` to
invalidate the entire window.

`Important` : Sizers ( see overview_sizer ) rely on size events to function
correctly. Therefore, in a sizer-based layout, do not forget to call Skip on all
size events you catch (and don't catch size events at all when you don't need
to).

See: \{Width,Height\},
[Overview events](https://docs.wxwidgets.org/3.1/overview_events.html#overview_events)

This class is derived (and can use functions) from: `m:wxEvent`

wxWidgets docs:
[wxSizeEvent](https://docs.wxwidgets.org/3.1/classwx_size_event.html)

## Events

Use `wxEvtHandler:connect/3` with [`wxSizeEventType`](`t:wxSizeEventType/0`) to
subscribe to events of this type.
""".
-include("wxe.hrl").
-export([getRect/1,getSize/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxSizeEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxSizeEventType() :: 'size'.
-export_type([wxSizeEvent/0, wxSize/0, wxSizeEventType/0]).
%% @hidden
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeevent.html#wxsizeeventgetsize">external documentation</a>.
-doc """
Returns the entire size of the window generating the size change event.

This is the new total size of the window, i.e. the same size as would be
returned by `wxWindow:getSize/1` if it were called now. Use
`wxWindow:getClientSize/1` if you catch this event in a top level window such as
`m:wxFrame` to find the size available for the window contents.
""".
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxSizeEvent().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSizeEvent_GetSize),
  wxe_util:rec(?wxSizeEvent_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizeevent.html#wxsizeeventgetrect">external documentation</a>.
-spec getRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxSizeEvent().
getRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizeEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxSizeEvent_GetRect),
  wxe_util:rec(?wxSizeEvent_GetRect).

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
