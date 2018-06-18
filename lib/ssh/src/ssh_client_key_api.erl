%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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

-module(ssh_client_key_api).

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-export_type([client_key_cb_options/0]).

-type client_key_cb_options() :: [{key_cb_private,term()} | ssh:client_option()].

-callback is_host_key(Key :: public_key:public_key(),
                      Host :: string(),
		      Algorithm :: ssh:pubkey_alg(),
                      Options :: client_key_cb_options()
                     ) ->
    boolean().

-callback user_key(Algorithm :: ssh:pubkey_alg(),
                   Options :: client_key_cb_options()
                  ) ->
    {ok,  PrivateKey :: public_key:private_key()} | {error, string()}.


-callback add_host_key(Host :: string(),
                       PublicKey :: public_key:public_key(),
                       Options :: client_key_cb_options()
                      ) ->
    ok | {error, Error::term()}.
