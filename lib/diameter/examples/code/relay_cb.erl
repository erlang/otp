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

-module(relay_cb).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_answer/5,
         handle_error/5,
         handle_request/3]).

peer_up(_SvcName, _Peer, State) ->
    State.

peer_down(_SvcName, _Peer, State) ->
    State.

%% Returning 'relay' from handle_request causes diameter to resend the
%% incoming request, which leads to pick_peer and prepare_request
%% callbacks as if sending explicitly. The 'extra' argument is
%% appended to the argument list for callbacks following from
%% resending of the request.

handle_request(_Pkt, _SvcName, _Peer) ->
    {relay, [{timeout, 1000}, {extra, [relayed]}]}.

%% diameter will filter the sender in the Peers list.
pick_peer([Peer | _], _, _SvcName, _State, relayed) ->
    {ok, Peer}.

prepare_request(Pkt, _SvcName, _Peer, relayed) ->
    {send, Pkt}.

prepare_retransmit(Pkt, _SvcName, _Peer, relayed) ->
    {send, Pkt}.

%% diameter expects handle_answer to return the diameter_packet record
%% containing the answer when called for a relayed request.

handle_answer(Pkt, _Request, _SvcName, _Peer, relayed) ->
    Pkt.

handle_error(Reason, _Request, _SvcName, _Peer, relayed) ->
    {error, Reason}.
