%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

-module(relay_cb).

-include_lib("diameter/include/diameter.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% handle_request/3

%% Assume the destination is directly connected; filter
%% correspondingly; don't relay to the sender.
handle_request(_Pkt, _SvcName, {_, Caps}) ->
    #diameter_caps{origin_host = {_, OH}}
        = Caps,
    {relay, [{timeout, 2000},
             {filter, {all, [host, realm, {neg, {host, OH}}]}}]}.

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(Pkt, _SvcName, _Peer) ->
    {send, Pkt}.

%% prepare_request/3

prepare_retransmit(Pkt, _SvcName, _Peer) ->
    {send, Pkt}.

%% handle_answer/4

%% Relay an answer by returning the first argument.
handle_answer(Pkt, _Request, _SvcName, _Peer) ->
    Pkt.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.
