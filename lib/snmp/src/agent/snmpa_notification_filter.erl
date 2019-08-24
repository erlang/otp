%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_notification, 2}];
behaviour_info(_) ->
    undefined.

%% handle_notification(Notification, Data) -> Reply
%% Notification -> notification() | trap()
%% Data -> term()
%% Reply -> send | {send, NewNotif} | ignore
%% NewNotif -> notification() | trap()
%% 
%% send -> This means it is ok for this filter to send the notification as is
%% {send, NewNotif} -> Send this notification instead
%% dont_sent -> Dont send this notification. 
