%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2023-2023. All Rights Reserved.
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

%% 
%% This is a set of (socket) utility macros and constants.
%% 

-define(ESOCK_SOCKET_TAG, '$socket').

-define(ESOCK_SOCKET_MSG(Socket, Tag, Info),
        {?ESOCK_SOCKET_TAG, (Socket), (Tag), (Info)}).

-define(ESOCK_ABORT_MSG(Sock, H, Reason),
        %% {?ESOCK_SOCKET_TAG, (Sock), abort, {(H), (Reason)}}).
        ?ESOCK_SOCKET_MSG(Sock, abort, {(H), (Reason)})).

-define(ESOCK_SELECT_INFO(ST, SH),
        {select_info, (ST), (SH)}).
-define(ESOCK_SELECT_INFO(SH),
        ?ESOCK_SELECT_INFO(_, SH)).

-define(ESOCK_SELECT_MSG(Sock, SH),
        %% {'$socket', (Sock), select, (SH)}).
        ?ESOCK_SOCKET_MSG(Sock, select, SH)).

-define(ESOCK_COMPLETION_INFO(CT, CH),
        {completion_info, (CT), (CH)}).
-define(ESOCK_COMPLETION_INFO(CH),
        ?ESOCK_COMPLETION_INFO(_, CH)).

-define(ESOCK_COMPLETION_MSG(Sock, CH, CS),
        %% {'$socket', (Sock), completion, {(CH), (CS)}}).
        ?ESOCK_SOCKET_MSG(Sock, completion, {(CH), (CS)})).
