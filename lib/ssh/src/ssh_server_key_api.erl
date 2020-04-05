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

-module(ssh_server_key_api).

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-export_type([daemon_key_cb_options/1]).

%%%****************************************************************
%%% The option key_cb_private is to pass options needed by other
%%% callback modules than the default ssh_file.erl
%%%
%%% If ssh:deamon(n, [ {key_cb_private, {hi,{there}}} ]
%%% is called, the term() will be {hi,{there}}

-type daemon_key_cb_options(T) :: [{key_cb_private,[T]} | ssh:daemon_option()].


%%%****************************************************************
%%% Fetch the host's private key that is of type Algorithm.

-callback host_key(Algorithm :: ssh:pubkey_alg(),
		   DaemonOptions :: daemon_key_cb_options(any())
                  ) ->
    {ok, PrivateKey :: public_key:private_key()} | {error, term()}.

%%%****************************************************************
%%% Check that PublicKey is known to be a public key for the User

-callback is_auth_key(PublicKey :: public_key:public_key(),
		      User :: string(),
		      DaemonOptions :: daemon_key_cb_options(any())
                     ) ->
    boolean().
