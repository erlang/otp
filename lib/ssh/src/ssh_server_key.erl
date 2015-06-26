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

-module(ssh_server_key).

-include_lib("public_key/include/public_key.hrl").
-include("ssh.hrl").

-type ssh_algorithm() :: string().

-callback host_key(Algorithm :: ssh_algorithm(), Options :: list()) ->
    {ok, [{public_key(), Attributes::list()}]} | public_key()
	| {error, string()}.

-callback is_auth_key(Key :: public_key(), User :: string(),
		      Algorithm :: ssh_algorithm(), Options :: list()) ->
    boolean().
