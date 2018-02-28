%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

%%% Description: ssh_io replacement that throws on everything

-module(ssh_no_io).
-include("ssh_transport.hrl").

-export([yes_no/2, read_password/2, read_line/2, format/2]).


-spec yes_no(any(), any()) -> no_return().

yes_no(_, _) ->
    ?DISCONNECT(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                "User interaction is not allowed").


-spec read_password(any(), any()) -> no_return().

read_password(_, _) ->
    ?DISCONNECT(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                "User interaction is not allowed").

-spec read_line(any(), any()) -> no_return().

read_line(_, _) ->
    ?DISCONNECT(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                "User interaction is not allowed").

-spec format(any(), any()) -> no_return().

format(_, _) ->
    ?DISCONNECT(?SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,
                "User interaction is not allowed").
