%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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
-module(inet6_tls_dist).
-moduledoc false.

-export([childspecs/0]).
-export([listen/2, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, address/0]).

-define(FAMILY, inet6).

childspecs() ->
    inet_tls_dist:childspecs().

select(Node) ->
    inet_tls_dist:fam_select(?FAMILY, Node).

address() ->
    inet_tls_dist:fam_address(?FAMILY).

listen(Name, Host) ->
    inet_tls_dist:fam_listen(?FAMILY, Name, Host).

accept(Listen) ->
    inet_tls_dist:fam_accept(?FAMILY, Listen).

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    inet_tls_dist:fam_accept_connection(
      ?FAMILY, AcceptPid, Socket, MyNode, Allowed, SetupTime).

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    inet_tls_dist:fam_setup(
      ?FAMILY, Node, Type, MyNode, LongOrShortNames,SetupTime).

close(Socket) ->
    inet_tls_dist:close(Socket).
