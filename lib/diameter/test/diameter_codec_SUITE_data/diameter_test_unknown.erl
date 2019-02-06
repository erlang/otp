%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(diameter_test_unknown).

-compile(export_all).

%%
%% Test reception of unknown AVP's.
%%

-include_lib("diameter/include/diameter.hrl").
-include("diameter_test_send.hrl").
-include("diameter_test_recv.hrl").

-define(HOST,  "test.erlang.org").
-define(REALM, "erlang.org").

%% Patterns to match decoded AVP's.
-define(MANDATORY_XXX,     #diameter_avp{code = 111}).
-define(NOT_MANDATORY_YYY, #diameter_avp{code = 222}).

%% Ensure that an unknown AVP with an M flag is regarded as an error
%% while one without an M flag is returned as 'AVP'.

run() ->
    H = #diameter_header{version = 1,
                         end_to_end_id = 1,
                         hop_by_hop_id = 1},
    Vs = [{'Origin-Host', ?HOST},
          {'Origin-Realm', ?REALM},
          {'XXX', [0]},
          {'YYY', [1]}],
    Pkt = #diameter_packet{header = H,
                           msg = Vs},

    [] = diameter_util:run([{?MODULE, [run, M, enc(M, Pkt)]}
                            || M <- ['AR','BR']]).

enc(M, #diameter_packet{msg = Vs} = P) ->
    diameter_codec:encode(diameter_test_send,
                          P#diameter_packet{msg = [M|Vs]}).

run(M, Pkt) ->
    dec(M, diameter_codec:decode(diameter_test_recv, opts(M), Pkt)).
%% Note that the recv dictionary defines neither XXX nor YYY.

dec('AR', #diameter_packet
           {msg = #recv_AR{'Origin-Host'  = ?HOST,
                           'Origin-Realm' = ?REALM,
                           'AVP' = [?NOT_MANDATORY_YYY]},
            errors = [{5001, ?MANDATORY_XXX}]}) ->
    ok;

dec('BR', #diameter_packet
           {msg = #recv_BR{'Origin-Host'  = ?HOST,
                           'Origin-Realm' = ?REALM},
            errors = [{5001, ?MANDATORY_XXX},
                      {5008, ?NOT_MANDATORY_YYY}]}) ->
    ok.

opts(Mod) ->
    #{app_dictionary => Mod,
      decode_format => record,
      string_decode => true,
      strict_mbit => true,
      rfc => 6733,
      failed_avp => false}.
