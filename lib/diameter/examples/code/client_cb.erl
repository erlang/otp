%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2020. All Rights Reserved.
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

-module(client_cb).

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

%% pick_peer/4

pick_peer([Peer | _], _, _SvcName, _State) ->
    {ok, Peer}.

%% prepare_request/3

prepare_request(#diameter_packet{msg = [Name | Avps]}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,
    {send, [Name | Avps#{'Origin-Host' => OH, 'Origin-Realm' => OR}]}.

%% prepare_retransmit/3

prepare_retransmit(Pkt, _SvcName, _Peer) ->
    {send, Pkt}.

%% handle_answer/4

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer) ->
    {ok, Msg}.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_Packet, _SvcName, _Peer) ->
    {answer_message, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED
