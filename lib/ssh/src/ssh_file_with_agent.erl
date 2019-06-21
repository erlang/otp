%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

%%% Description: SSH file handling with SSH Agent support

-module(ssh_file_with_agent).

-behaviour(ssh_client_key_api).

-include("ssh.hrl").
-include("ssh_agent.hrl").

-export([add_host_key/3, is_host_key/4, user_key/2]).

add_host_key(Host, Key, Opts) ->
    ssh_file:add_host_key(Host, Key, Opts).

is_host_key(Key, PeerName, Algorithm, Opts) ->
    ssh_file:is_host_key(Key, PeerName, Algorithm, Opts).

user_key(Algorithm, _Opts) ->
    Request = #ssh_agent_identities_request{},
    Response = ssh_agent:send(Request),

    #ssh_agent_identities_response{keys = Keys} = Response,

    AlgorithmStr = atom_to_list(Algorithm),
    MatchingKeys = lists:filter(fun(Key) -> has_key_type(Key, AlgorithmStr) end, Keys),

    % The "client_key_api" behaviour only allows returning a single user key,
    % so we simply select the first one returned from the SSH agent here. This
    % means that if a user adds multiple keys for the same algorithm, only the
    % first one added will be used.
    [#ssh_agent_key{blob = PubKeyBlob} | _OtherKeys] = MatchingKeys,

    % TODO:
    % Mark the returned key so we can identify agent keys as they do not
    % contain the private key.
    {ok, PubKeyBlob}.

%% Utility functions

has_key_type(#ssh_agent_key{blob = KeyBlob}, Type) ->
  <<?DEC_BIN(KeyType, _KeyTypeLen), _KeyBlobRest/binary>> = KeyBlob,
  binary_to_list(KeyType) == Type.
