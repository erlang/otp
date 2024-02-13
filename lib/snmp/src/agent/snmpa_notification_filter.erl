%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2024. All Rights Reserved.
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
%%
-module(snmpa_notification_filter).
-moduledoc """
Behaviour module for the SNMP agent notification filters.

This module defines the behaviour of the agent notification filters. A
`snmpa_notification_filter` compliant module must export the following
functions:

- handle_notification/2

The semantics of them and their exact signatures are explained below.

The purpose of notification filters is to allow for modification and/or
suppression of a notification.

A misbehaving filter will be removed.
""".


-type notification() :: term().
-type trap() :: term().

%% handle_notification(Notification, Data) -> Reply
%% Notification -> notification() | trap()
%% Data -> term()
%% Reply -> send | {send, NewNotif} | ignore
%% NewNotif -> notification() | trap()
%% 
%% send -> This means it is ok for this filter to send the notification as is
%% {send, NewNotif} -> Send this notification instead
%% dont_sent -> Dont send this notification. 
-doc """
Handle a notification to be sent. The filter can either accept the notification
as is, return `send`, modify the notification, return `{send, NewNotif}` or
suppress the notification, return `dont_send`.

`Data` is supplied at filter registration time, see
[register_notification_filter](`m:snmpa#register_notification_filter`).
""".
-callback handle_notification(Notif, Data) -> Reply when
      Reply :: send |
               {send, NewNotif} |
               dont_send,
      Notif :: notification() | trap(),
      NewNotif :: notification() | trap(),
      Data :: term().
