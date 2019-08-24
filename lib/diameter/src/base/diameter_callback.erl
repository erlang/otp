%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2019. All Rights Reserved.
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

%%
%% A diameter callback module that can redirect selected callbacks,
%% providing reasonable default implementations otherwise.
%%
%% To order alternate callbacks, configure a #diameter_callback record
%% as the Diameter application callback in question. The record has
%% one field for each callback function as well as 'default' and
%% 'extra' fields. A function-specific field can be set to a
%% diameter:eval() in order to redirect the callback
%% corresponding to that field, or to 'false' to request the default
%% callback implemented in this module. If neither of these fields are
%% set then the 'default' field determines the form of the callback: a
%% module name results in the usual callback as if the module had been
%% configured directly as the callback module, a diameter_eval()
%% in a callback applied to the atom-valued callback name and argument
%% list. For all callbacks not to this module, the 'extra' field is a
%% list of additional arguments, following arguments supplied by
%% diameter but preceding those of the diameter:eval() being
%% applied.
%%
%% For example, the following config to diameter:start_service/2, in
%% an 'application' tuple, would result in only a mymod:peer_down/3
%% callback, this module implementing the remaining callbacks.
%%
%%   {module, #diameter_callback{peer_down = {mymod, down, []}}}
%%
%% Equivalently, this can also be specified with a [Mod | Args]
%% field/value list as follows.
%%
%%   {module, [diameter_callback, {peer_down, {mymod, down, []}}]}
%%
%% The following would result in this module suppying peer_up and
%% peer_down callback, others taking place in module mymod.
%%
%%   {module, #diameter_callback{peer_up = false,
%%                               peer_down = false,
%%                               default = mymod}}
%%
%% The following would result in all callbacks taking place as
%% calls to mymod:diameter/2.
%%
%%   {module, #diameter_callback{default = {mymod, diameter, []}}}
%%
%% The following are equivalent and result in all callbacks being
%% provided by this module.
%%
%%   {module, #diameter_callback{}}
%%   {module, diameter_callback}
%%

-module(diameter_callback).

%% Default callbacks when no alternate is specified.
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_request/3,
         handle_answer/4,
         handle_error/4]).

%% Callbacks taking a #diameter_callback record.
-export([peer_up/4,
         peer_down/4,
         pick_peer/5,
         prepare_request/4,
         prepare_retransmit/4,
         handle_request/4,
         handle_answer/5,
         handle_error/5]).

-include_lib("diameter/include/diameter.hrl").

%%% ----------------------------------------------------------
%%% # peer_up/3
%%% ----------------------------------------------------------

peer_up(_Svc, _Peer, State) ->
    State.

peer_up(Svc, Peer, State, D) ->
    cb(peer_up,
       [Svc, Peer, State],
       D#diameter_callback.peer_up,
       D).

%%% ----------------------------------------------------------
%%% # peer_down/3
%%% ----------------------------------------------------------

peer_down(_Svc, _Peer, State) ->
    State.

peer_down(Svc, Peer, State, D) ->
    cb(peer_down,
       [Svc, Peer, State],
       D#diameter_callback.peer_down,
       D).

%%% ----------------------------------------------------------
%%% # pick_peer/4
%%% ----------------------------------------------------------

pick_peer([Peer|_], _, _Svc, _State) ->
    {ok, Peer};
pick_peer([], _, _Svc, _State) ->
    false.

pick_peer(PeersL, PeersR, Svc, State, D) ->
    cb(pick_peer,
       [PeersL, PeersR, Svc, State],
       D#diameter_callback.pick_peer,
       D).

%%% ----------------------------------------------------------
%%% # prepare_request/3
%%% ----------------------------------------------------------

prepare_request(Pkt, _Svc, _Peer) ->
    {send, Pkt}.

prepare_request(Pkt, Svc, Peer, D) ->
    cb(prepare_request,
       [Pkt, Svc, Peer],
       D#diameter_callback.prepare_request,
       D).

%%% ----------------------------------------------------------
%%% # prepare_retransmit/3
%%% ----------------------------------------------------------

prepare_retransmit(Pkt, _Svc, _Peer) ->
    {send, Pkt}.

prepare_retransmit(Pkt, Svc, Peer, D) ->
    cb(prepare_retransmit,
       [Pkt, Svc, Peer],
       D#diameter_callback.prepare_retransmit,
       D).

%%% ----------------------------------------------------------
%%% # handle_request/3
%%% ----------------------------------------------------------

handle_request(_Pkt, _Svc, _Peer) ->
    {protocol_error, 3001}.  %% DIAMETER_COMMAND_UNSUPPORTED

handle_request(Pkt, Svc, Peer, D) ->
    cb(handle_request,
       [Pkt, Svc, Peer],
       D#diameter_callback.handle_request,
       D).

%%% ----------------------------------------------------------
%%% # handle_answer/4
%%% ----------------------------------------------------------

handle_answer(#diameter_packet{msg = Ans, errors = []}, _Req, _Svc, _Peer) ->
    Ans;
handle_answer(#diameter_packet{msg = Ans, errors = Es}, _Req, _Svc, _Peer) ->
    [Ans | Es].

handle_answer(Pkt, Req, Svc, Peer, D) ->
    cb(handle_answer,
       [Pkt, Req, Svc, Peer],
       D#diameter_callback.handle_answer,
       D).

%%% ---------------------------------------------------------------------------
%%% # handle_error/4
%%% ---------------------------------------------------------------------------

handle_error(Reason, _Req, _Svc, _Peer) ->
    {error, Reason}.

handle_error(Reason, Req, Svc, Peer, D) ->
    cb(handle_error,
       [Reason, Req, Svc, Peer],
       D#diameter_callback.handle_error,
       D).

%% ===========================================================================

%% cb/4

%% Unspecified callback: use default field to determine something
%% appropriate.
cb(CB, Args, undefined, D) ->
    cb(CB, Args, D);

%% Explicitly requested default.
cb(CB, Args, false, _) ->
    apply(?MODULE, CB, Args);

%% A specified callback.
cb(_, Args, F, #diameter_callback{extra = X}) ->
    diameter_lib:eval([[F|X] | Args]).

%% cb/3

%% No user-supplied default: call ours.
cb(CB, Args, #diameter_callback{default = undefined}) ->
    apply(?MODULE, CB, Args);

%% Default is a module name: make the usual callback.
cb(CB, Args, #diameter_callback{default = M,
                                extra = X})
  when is_atom(M) ->
    apply(M, CB, Args ++ X);

%% Default is something else: apply if to callback name and arguments.
cb(CB, Args, #diameter_callback{default = F,
                                extra = X}) ->
    diameter_lib:eval([F, CB, Args | X]).
