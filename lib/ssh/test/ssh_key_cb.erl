%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------

%% Note: This module is used by ssh_basic_SUITE

-module(ssh_key_cb).
-behaviour(ssh_client_key_api).
-compile(export_all).

add_host_key(_, _, _) ->
    ok.

is_host_key(_, _, _, _) ->
    true.

user_key('ssh-rsa', Opts) ->
    UserDir = proplists:get_value(user_dir, Opts),
    KeyFile = filename:join(filename:dirname(UserDir), "id_rsa"),
    {ok, KeyBin} = file:read_file(KeyFile),
    [Entry] = public_key:pem_decode(KeyBin),
    Key = public_key:pem_entry_decode(Entry),
    {ok, Key};

user_key(_Alg, _Opt) ->
    {error, "Not Supported"}.
