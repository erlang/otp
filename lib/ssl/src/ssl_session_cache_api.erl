%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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

%%

-module(ssl_session_cache_api).
-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

-type key() :: {{host(), inet:port_number()}, session_id()} |  {inet:port_number(), session_id()}.

-callback init(list()) -> db_handle().
-callback terminate(db_handle()) -> any().
-callback lookup(db_handle(), key()) -> #session{} | undefined.
-callback update(db_handle(), key(), #session{}) -> any().
-callback delete(db_handle(), key()) -> any().
-callback foldl(fun(), term(), db_handle()) -> term().
-callback select_session(db_handle(), {host(), inet:port_number()} | inet:port_number()) -> [#session{}].
