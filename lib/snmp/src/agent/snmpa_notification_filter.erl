%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
