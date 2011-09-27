%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

%%
%% A minimal application callback module.
%%

-module(diameter_callback).

-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_request/3,
         handle_answer/4,
         handle_error/4]).

-include_lib("diameter/include/diameter.hrl").

%%% ----------------------------------------------------------
%%% # peer_up/3
%%% ----------------------------------------------------------

peer_up(_Svc, _Peer, State) ->
    State.

%%% ----------------------------------------------------------
%%% # peer_down/3
%%% ----------------------------------------------------------

peer_down(_SvcName, _Peer, State) ->
    State.

%%% ----------------------------------------------------------
%%% # pick_peer/4
%%% ----------------------------------------------------------

pick_peer([Peer|_], _, _SvcName, _State) ->
    {ok, Peer}.

%%% ----------------------------------------------------------
%%% # prepare_request/3
%%% ----------------------------------------------------------

prepare_request(Pkt, _SvcName, _Peer) ->
    {send, Pkt}.

%%% ----------------------------------------------------------
%%% # prepare_retransmit/3
%%% ----------------------------------------------------------

prepare_retransmit(Pkt, _SvcName, _Peer) ->
    {send, Pkt}.

%%% ----------------------------------------------------------
%%% # handle_request/3
%%% ----------------------------------------------------------

handle_request(_Pkt, _SvcName, _Peer) ->
    {protocol_error, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

%%% ----------------------------------------------------------
%%% # handle_answer/4
%%% ----------------------------------------------------------

handle_answer(#diameter_packet{msg = Ans}, _Req, _SvcName, _Peer) ->
    Ans.

%%% ---------------------------------------------------------------------------
%%% # handle_error/4
%%% ---------------------------------------------------------------------------

handle_error(Reason, _Req, _SvcName, _Peer) ->
    {error, Reason}.
