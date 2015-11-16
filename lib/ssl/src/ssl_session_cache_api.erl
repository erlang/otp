%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2011. All Rights Reserved.
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
-callback size(db_handle()) -> integer().
