%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2025. All Rights Reserved.
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

-module(socket_sctp_traffic_lib).

-export([
         set_debug/1, get_debug/0,
         print_dbg/2, print_info/2, print_warning/2, print_error/2
        ]).

-include("kernel_test_lib.hrl").

set_debug(D) when is_boolean(D) ->
    put({debug, ?MODULE}, (D)).

get_debug() ->
    get({debug, ?MODULE}).


print_dbg(F, A) ->
    print(get_debug(), "DEBUG", F, A).

print_info(F, A) ->
    print(true, "INFO", F, A).

print_warning(F, A) ->
    print(true, "WARNING", F, A).

print_error(F, A) ->
    print(true, "ERROR", F, A).

print(true, Pre, F, A) ->
    SName = get(sname),
    io:format("~s [~s, ~p, ~s] " ++ F ++ "~n",
              [Pre, ?FTS(), self(), SName | A]);
print(_, _, _, _) ->
    ok.


