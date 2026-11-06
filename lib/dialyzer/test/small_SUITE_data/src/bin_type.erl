%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(bin_type).

-export([handle/1, status/0, map_key/1, mixed/1, make_msg/1,
         bad_handle/0, bad_status/0, guard/1, guard_b/0,
         many_user/0, prefix/1, map_mixed_user/0, bad_size/0, big/0]).

-record(msg, {tag :: <<"hello">> | <<"world">>, payload :: binary()}).

-type cmd() :: <<"start">> | <<"stop">>.

-spec handle(cmd()) -> ok.
handle(<<"start">>) -> ok;
handle(<<"stop">>) -> ok.

-spec status() -> <<"ok">> | <<"error">>.
status() -> <<"ok">>.

-spec map_key(#{<<"name">> := binary()}) -> binary().
map_key(#{<<"name">> := V}) -> V.

%% Exercises get_modules_mentioned with bin_type + remote type in a union.
-type mixed() :: <<"hello">> | binary:part().

-spec mixed(mixed()) -> binary().
mixed(<<"hello">>) -> <<"greeting">>;
mixed({Pos, Len}) when is_integer(Pos), is_integer(Len) -> <<"part">>.

-spec make_msg(binary()) -> #msg{}.
make_msg(Payload) -> #msg{tag = <<"hello">>, payload = Payload}.

%% A call with a binary that is not part of the type is detected.
bad_handle() -> handle(<<"pause">>).

-spec bad_status() -> <<"ok">> | <<"error">>.
bad_status() -> <<"oops">>.

%% Guarding on a binary literal must not make other clauses unreachable.
-spec guard(<<_:8>>) -> a | b.
guard(X) when X =:= <<"a">> -> a;
guard(_) -> b.

guard_b() -> guard(<<"b">>).

%% More values than the set limit fall back to a structural type.
-type many() :: <<"a">> | <<"b">> | <<"c">> | <<"d">> | <<"e">> | <<"f">>
              | <<"g">> | <<"h">> | <<"i">> | <<"j">> | <<"k">> | <<"l">>
              | <<"m">> | <<"n">>.

-spec many(many()) -> ok.
many(_) -> ok.

many_user() -> many(<<"n">>).

%% Binary patterns are matched structurally against binary literal types.
-spec prefix(cmd()) -> start | stop.
prefix(<<"sta", _/binary>>) -> start;
prefix(<<"sto", _/binary>>) -> stop.

%% Binary literal keys together with a structural key type.
-spec map_mixed(#{<<"id">> := integer(), binary() => atom()}) -> integer().
map_mixed(#{<<"id">> := Id}) -> Id.

map_mixed_user() -> map_mixed(#{<<"id">> => 1, <<"other">> => x}).

%% A binary of a different size is detected.
bad_size() -> handle(<<"star">>).

%% Large binaries are not tracked by value.
-spec big() -> <<"ok">>.
big() ->
    <<"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx">>.
