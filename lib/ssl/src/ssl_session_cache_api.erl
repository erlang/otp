%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
-moduledoc """
TLS session cache API

Defines the API for the TLS session cache (pre TLS-1.3) so that the data storage
scheme can be replaced by defining a new callback module implementing this API.
""".
-moduledoc(#{since => "OTP R14B"}).
-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export_type([session_cache_key/0, 
              session/0, 
              partial_key/0, 
              session_cache_ref/0]).

-doc "A term that can be used to reference the cache.".
-type session_cache_ref() :: any().

-doc "A key to an entry in the session cache.".
-type session_cache_key() :: {partial_key(), ssl:session_id()}.

-doc "The session data that is stored for each session.".
-opaque session()         :: #session{}.

-doc "The opaque part of the key. Does not need to be handled by the callback.".
-opaque partial_key()     :: {ssl:host(), inet:port_number()} | inet:port_number().

-doc """
Performs possible initializations of the cache and returns a reference to it
that is used as parameter to the other API functions. 

Is called by the cache handling processes `init` function, hence
putting the same requirements on it as a normal process `init`
function. This function is called twice when starting the SSL
application, once with the role client and once with the role server,
as the SSL application must be prepared to take on both roles.

Includes property `{role, client | server}` in init argument list. 
Currently this is the only predefined property, there can also be
user-defined properties. See also application environment variable
[session_cb_init_args](ssl_app.md).
""".
-doc(#{since => <<"OTP 18.0">>}).
-callback init(InitArgs) -> CacheRef when 
      InitArgs :: list(),
      CacheRef :: session_cache_ref().

-doc(#{since => <<"OTP R14B">>}).
-doc """
Takes care of possible cleanup that is needed when the cache handling process
terminates.
""".
-callback terminate(CacheRef) -> DoNotCare when 
      CacheRef :: session_cache_ref(),
      DoNotCare :: any().

-doc(#{since => <<"OTP R14B">>}).
-doc "Looks up a cache entry. Is to be callable from any process.".
-callback lookup(CacheRef, Key) -> Session when
      CacheRef:: session_cache_ref(),
      Key :: session_cache_key(),
      Session :: session() | undefined.

-doc(#{since => <<"OTP R14B">>}).
-doc """
Caches a new session or updates an already cached one.

Is only called from the cache handling process.
""".
-callback update(CacheRef, Key, Session) -> DoNotCare when
      CacheRef:: session_cache_ref(),
      Key :: session_cache_key(),
      Session :: session() | undefined,
      DoNotCare :: any().

-doc(#{since => <<"OTP R14B">>}).
-doc """
Deletes a cache entry.

Is only called from the cache handling process.
""".
-callback delete(CacheRef, Key) -> DoNotCare when
      CacheRef:: session_cache_ref(),
      Key :: session_cache_key(),
      DoNotCare :: any().

-doc(#{since => <<"OTP R14B">>}).
-doc """
Calls `Fun(Elem, AccIn)` on successive elements of the cache, starting with
 `AccIn == Acc0`.

`Fun/2` must return a new accumulator, which is passed to the
next call. The function returns the final value of the accumulator. `Acc0` is
returned if the cache is empty.

> #### Note {: .info }
>
> Since OTP-23.3 this functions is only used on the client side and does not
> need to implemented for a server cache.
""".
-callback foldl(Fun, Acc0, CacheRef) ->  Acc when
      Fun::fun(),
      Acc0::term(),
      CacheRef:: session_cache_ref(),
      Acc::term().

-doc(#{since => <<"OTP R14B">>}).
-doc """
Selects sessions that can be reused, that is sessions that include `PartialKey`
in its key. Is to be callable from any process.

> #### Note {: .info }
>
> Since OTP-23.3 This functions is only used on the client side and does not
> need to implemented for a server cache.
""".
-callback select_session(CacheRef, Server) -> Sessions when
      CacheRef :: session_cache_ref(),
      Server ::{ssl:host(), inet:port_number()} | inet:port_number(),
      Sessions :: [session()].

-doc(#{since => <<"OTP 19.3">>}).
-doc """
Returns the number of sessions in the cache.

If size exceeds the maximum number of sessions, the current cache
entries will be invalidated regardless of their remaining lifetime. Is
to be callable from any process.
""".
-callback size(CacheRef) -> Size when
      CacheRef:: session_cache_ref(),
      Size :: pos_integer().

-optional_callbacks([select_session/2, foldl/3]).
