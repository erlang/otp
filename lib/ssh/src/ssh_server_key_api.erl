%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-export_type([algorithm/0]).

-type algorithm()  :: ssh_client_key_api:algorithm().


-callback host_key(Algorithm :: algorithm(),
		   DaemonOptions :: proplists:proplist()) ->
    {ok, PrivateKey :: public_key:private_key()} | {error, term()}.

-callback is_auth_key(PublicKey :: public_key:public_key(),
		      User :: string(),
		      DaemonOptions :: proplists:proplist()) ->
    boolean().
