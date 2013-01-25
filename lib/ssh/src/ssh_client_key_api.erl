%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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
