%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
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

-module(client_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

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

prepare_request(#diameter_packet{msg = ['RAR' = T | Avps]}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,

    {send, [T, {'Origin-Host', OH},
               {'Origin-Realm', OR},
               {'Destination-Host', DH},
               {'Destination-Realm', DR}
             | Avps]};

prepare_request(#diameter_packet{msg = Rec}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, DH},
                   origin_realm = {OR, DR}}
        = Caps,

    {send, Rec#diameter_base_RAR{'Origin-Host' = OH,
                                 'Origin-Realm' = OR,
                                 'Destination-Host' = DH,
                                 'Destination-Realm' = DR}}.

%% prepare_retransmit/3

prepare_retransmit(Packet, SvcName, Peer) ->
    prepare_request(Packet, SvcName, Peer).

%% handle_answer/4

handle_answer(#diameter_packet{msg = Msg}, _Request, _SvcName, _Peer) ->
    {ok, Msg}.

%% handle_error/4

handle_error(Reason, _Request, _SvcName, _Peer) ->
    {error, Reason}.

%% handle_request/3

handle_request(_Packet, _SvcName, _Peer) ->
    erlang:error({unexpected, ?MODULE, ?LINE}).
