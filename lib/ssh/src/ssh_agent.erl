%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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

-module(ssh_agent).

%% SSH Agent message numbers
%%
%% Reference: https://tools.ietf.org/html/draft-miller-ssh-agent-02#section-5.1

%% The following numbers are used for requests from the client to the agent.

-define(SSH_AGENTC_REQUEST_IDENTITIES,            11).
-define(SSH_AGENTC_SIGN_REQUEST,                  13).
-define(SSH_AGENTC_ADD_IDENTITY,                  17).
-define(SSH_AGENTC_REMOVE_IDENTITY,               18).
-define(SSH_AGENTC_REMOVE_ALL_IDENTITIES,         19).
-define(SSH_AGENTC_ADD_ID_CONSTRAINED,            25).
-define(SSH_AGENTC_ADD_SMARTCARD_KEY,             20).
-define(SSH_AGENTC_REMOVE_SMARTCARD_KEY,          21).
-define(SSH_AGENTC_LOCK,                          22).
-define(SSH_AGENTC_UNLOCK,                        23).
-define(SSH_AGENTC_ADD_SMARTCARD_KEY_CONSTRAINED, 26).
-define(SSH_AGENTC_EXTENSION,                     27).

%% The following numbers are used for replies from the agent to the client.

-define(SSH_AGENT_FAILURE,                        5).
-define(SSH_AGENT_SUCCESS,                        6).
-define(SSH_AGENT_EXTENSION_FAILURE,              28).
-define(SSH_AGENT_IDENTITIES_ANSWER,              12).
-define(SSH_AGENT_SIGN_RESPONSE,                  14).
