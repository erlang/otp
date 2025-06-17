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

-module(wxNotifyEvent).
-moduledoc """
This class is not used by the event handlers by itself, but is a base class for other
event classes (such as `m:wxBookCtrlEvent`).

It (or an object of a derived class) is sent when the controls state is being changed and
allows the program to `veto/1` this change if it wants to prevent it from happening.

See: `m:wxBookCtrlEvent`

This class is derived, and can use functions, from:

* `m:wxCommandEvent`

* `m:wxEvent`

wxWidgets docs: [wxNotifyEvent](https://docs.wxwidgets.org/3.2/classwx_notify_event.html)
""".
-include("wxe.hrl").
-export([allow/1,isAllowed/1,veto/1]).

%% inherited exports
-export([getClientData/1,getExtraLong/1,getId/1,getInt/1,getSelection/1,getSkipped/1,
  getString/1,getTimestamp/1,isChecked/1,isCommandEvent/1,isSelection/1,
  parent_class/1,resumePropagation/2,setInt/2,setString/2,shouldPropagate/1,
  skip/1,skip/2,stopPropagation/1]).

-type wxNotifyEvent() :: wx:wx_object().
-export_type([wxNotifyEvent/0]).
-doc false.
parent_class(wxCommandEvent) -> true;
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
This is the opposite of `veto/1`: it explicitly allows the event to be processed.

For most events it is not necessary to call this method as the events are allowed anyhow
but some are forbidden by default (this will be mentioned in the corresponding event
description).
""".
-spec allow(This) -> 'ok' when
	This::wxNotifyEvent().
allow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotifyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNotifyEvent_Allow).

-doc """
Returns true if the change is allowed (`veto/1` hasn't been called) or false otherwise
(if it was).
""".
-spec isAllowed(This) -> boolean() when
	This::wxNotifyEvent().
isAllowed(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotifyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNotifyEvent_IsAllowed),
  wxe_util:rec(?wxNotifyEvent_IsAllowed).

-doc """
Prevents the change announced by this event from happening.

It is in general a good idea to notify the user about the reasons for vetoing the change
because otherwise the applications behaviour (which just refuses to do what the user
wants) might be quite surprising.
""".
-spec veto(This) -> 'ok' when
	This::wxNotifyEvent().
veto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotifyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxNotifyEvent_Veto).

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
