%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-module(diameter_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-include_lib("diameter/include/diameter.hrl").

-type peer_ref() :: term().
-type capabilities() :: #diameter_caps{}.
-type peer() :: {peer_ref(), capabilities()}.
-type packet() :: diameter_codec:packet().
-type state() :: term().
-type message() :: diameter_codec:message().

-callback handle_request(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: term(),
      Peer :: peer(),
      Action :: Reply |
                {relay, [Opt]} |
                discard |
                {eval | eval_packet, Action, PostF},
      Reply :: {reply, packet() | message()} |
               {answer_message,
                3000..3999 | 5000..5999} |
               {protocol_error, 3000..3999},
      Opt :: diameter:call_opt(),
      PostF :: diameter:eval().

-callback handle_error(Reason, Request, SvcName, Peer) -> Result when
      Reason :: timeout | failover | term(),
      Request :: message(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Result :: term().

-callback handle_answer(Packet, Request, SvcName, Peer) -> Result when
      Packet :: packet(),
      Request :: message(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Result :: term().

-callback prepare_retransmit(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Action :: Send | Discard |
                {eval_packet, Action, PostF},
      Send :: {send, packet() | message()},
      Discard :: {discard, Reason :: term()} | discard,
      PostF :: diameter:eval().

-callback prepare_request(Packet, SvcName, Peer) -> Action when
      Packet :: packet(),
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      Action :: Send | Discard |
                {eval_packet, Action, PostF},
      Send :: {send, packet() | message()},
      Discard :: {discard, Reason :: term()} | discard,
      PostF :: diameter:eval().

-callback pick_peer(LocalCandidates, RemoteCandidates, SvcName, State) ->
    Selection | false when
      LocalCandidates :: [peer()],
      RemoteCandidates :: [peer()],
      SvcName :: diameter:service_name(),
      State :: state(),
      NewState :: state(),
      Selection :: {ok, Peer} | {Peer, NewState},
      Peer :: peer() | false.

-callback peer_down(SvcName, Peer, State) -> NewState when
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      State :: state(),
      NewState :: state().

-callback peer_up(SvcName, Peer, State) -> NewState when
      SvcName :: diameter:service_name(),
      Peer :: peer(),
      State :: state(),
      NewState :: state().

%% start/2

start(_Type, _Args) ->
    diameter_sup:start_link().

%% stop/1

stop(_) ->
    ok.
