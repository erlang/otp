%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
-module(block_select_dist).

%% A wrapper around gen_tcp_dist with the option to block the select()
%% call for testing of net_kernel...

-export([block_select/0, unblock_select/0]).

-export([listen/1, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, is_node_name/1,
         address/0, setopts/2, getopts/2]).

block_select() ->
    persistent_term:put(?MODULE, true),
    ok.

unblock_select() ->
    persistent_term:put(?MODULE, false),
    ok.

select(A1) ->
    WaitOnBlock = fun WaitOnBlock () ->
                          case persistent_term:get(?MODULE, false) of
                              true ->
                                  receive after 1000 -> ok end,
                                  WaitOnBlock();
                              _ ->
                                  ok
                          end
                  end,
    WaitOnBlock(),
    gen_tcp_dist:select(A1).

listen(A1) ->
    gen_tcp_dist:listen(A1).
accept(A1) ->
    gen_tcp_dist:accept(A1).
accept_connection(A1, A2, A3, A4, A5) ->
    gen_tcp_dist:accept_connection(A1, A2, A3, A4, A5).
setup(A1, A2, A3, A4, A5) ->
    gen_tcp_dist:setup(A1, A2, A3, A4, A5).
close(A1) ->
    gen_tcp_dist:close(A1).
is_node_name(A1) ->
    gen_tcp_dist:is_node_name(A1).
address() ->
    gen_tcp_dist:address().
setopts(A1, A2) ->
    gen_tcp_dist:setopts(A1, A2).
getopts(A1, A2) ->
    gen_tcp_dist:getopts(A1, A2).
