%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2012. All Rights Reserved.
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

-module(ssh_server_key_api).

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-callback host_key(Algorithm :: 'ssh-rsa'| 'ssh-dss'| atom(), DaemonOptions :: proplists:proplist()) ->
    {ok, PrivateKey :: #'RSAPrivateKey'{}| #'DSAPrivateKey'{} |  term()} | {error, string()}.

-callback is_auth_key(PublicKey :: #'RSAPublicKey'{}| {integer(),  #'Dss-Parms'{}}| term(),
		      User :: string(), DaemonOptions :: proplists:proplist()) ->
    boolean().
