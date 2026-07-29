%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

%% In-memory key callback for ssh_algorithms_SUITE.
%%
%% Serves pre-loaded host keys for all algorithms from a single daemon,
%% solving the problem that ssh_file maps multiple ECDSA curves (and RSA
%% variants) to the same on-disk filename.
%%
%% Usage:
%%   Keys = #{Algorithm => PrivateKey, ...},
%%   ssh:daemon(Port, [{key_cb, {ssh_algorithms_key_cb, [{host_keys, Keys}]}} | ...])

-module(ssh_algorithms_key_cb).

-behaviour(ssh_server_key_api).

-export([host_key/2, is_auth_key/3]).

%%%================================================================
%%% Server key API
%%%================================================================

host_key(Algorithm, Opts) ->
    KeyCbPrivate = proplists:get_value(key_cb_private, Opts, []),
    Keys = proplists:get_value(host_keys, KeyCbPrivate, #{}),
    case maps:find(Algorithm, Keys) of
        {ok, Key} -> {ok, Key};
        error -> {error, {no_host_key_for, Algorithm}}
    end.

is_auth_key(Key, User, Opts) ->
    %% Fall back to ssh_file for authorized_keys lookup
    ssh_file:is_auth_key(Key, User, Opts).
