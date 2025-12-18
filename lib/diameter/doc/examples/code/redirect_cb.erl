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

-module(redirect_cb).

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

%% Dictionary to encode answer-message AVPs with.
-define(Dict, diameter_gen_base_rfc6733).

%% Raise an error on callbacks that aren't expected.
-define(ERROR, error({unexpected, ?MODULE, ?LINE})).

%% peer_up/3

peer_up(_SvcName, _Peer, State) ->
    State.

%% peer_down/3

peer_down(_SvcName, _Peer, State) ->
    State.

%% pick_peer/4

pick_peer(_, _, _SvcName, _State) ->
    false.

%% prepare_request/3

prepare_request(_, _SvcName, _Peer) ->
    ?ERROR.

%% prepare_retransmit/3

prepare_retransmit(_Packet, _SvcName, _Peer) ->
    ?ERROR.

%% handle_answer/4

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
    ?ERROR.

%% handle_error/4

handle_error(_Reason, _Request, _SvcName, _Peer) ->
    ?ERROR.

%% handle_request/3

handle_request(#diameter_packet{avps = Avps}, _, {_, Caps}) ->
    #diameter_caps{origin_host = {OH, _},
                   origin_realm = {OR, _}}
        = Caps,

    Tail = [#diameter_avp{data = {?Dict, A, V}}
            || {A,V} <- [{'Origin-Host', OH},
                        {'Origin-Realm', OR},
                       {'Result-Code', 3006}, %% DIAMETER_REDIRECT_INDICATION
                      {'Redirect-Host', <<"aaa://server.example.com:3868">>}]],

    {reply, ['answer-message' | lists:append([session(Avps), Tail])]}.

%% ===========================================================================

%% session/1

session(Avps) ->
    try
        [] = [A || #diameter_avp{code = 263, vendor_id = undefined} = A
                       <- Avps,
                   throw(A)]
    catch
        Avp -> [Avp]
    end.
