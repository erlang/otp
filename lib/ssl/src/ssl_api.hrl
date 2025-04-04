%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2013-2025. All Rights Reserved.
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

-ifndef(ssl_api).
-define(ssl_api, true).

%% Opaque to the user of ssl application, but
%% is allowed to be matched for equality
-record(sslsocket, {socket_handle,     %% Depends on transport module
                    connection_handler,%% pid()
                    payload_sender,    %% pid()
                    transport_cb,      %% ssl:transport_option()
                    connection_cb,     %% :: tls_gen_connection | dtls_gen_connection
                    tab,               %% ets table
                    listener_config    %% :: #config{} (listen socket) | [pid()] list of trackers
                 }).

-endif. % -ifdef(ssl_api).
