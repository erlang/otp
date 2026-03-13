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

-export([handle/1, status/0, map_key/1, mixed/1, make_msg/1]).

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
