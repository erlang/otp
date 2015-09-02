%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
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

-callback is_host_key(PublicKey :: #'RSAPublicKey'{}| {integer(),  #'Dss-Parms'{}}| term() , Host :: string(),
		      Algorithm :: 'ssh-rsa'| 'ssh-dss'| atom(), ConnectOptions :: proplists:proplist()) ->
    boolean().

-callback user_key(Algorithm ::  'ssh-rsa'| 'ssh-dss'| atom(), ConnectOptions :: proplists:proplist()) ->
    {ok,  PrivateKey :: #'RSAPrivateKey'{}| #'DSAPrivateKey'{} |  term()} | {error, string()}.


-callback add_host_key(Host :: string(), PublicKey ::  #'RSAPublicKey'{}| {integer(),  #'Dss-Parms'{}}| term(),
		       Options :: list()) ->
    ok | {error, Error::term()}.
