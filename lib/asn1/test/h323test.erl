%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
-module(h323test).

-export([run/1]).
-include_lib("common_test/include/ct.hrl").

run(per)     -> run();
run(_Rules)  -> ok.

run() ->
    roundtrip('H323-UserInformation', alerting_val(), alerting_enc()),
    roundtrip('H323-UserInformation', connect_val(), connect_enc()),
    general_string(),
    ok.

alerting_val() ->
    {'H323-UserInformation',
     {'H323-UU-PDU',
      {alerting,
       {'Alerting-UUIE',
	{0,0,8,2250,0,2},
	{'EndpointType',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
	 asn1_NOVALUE,asn1_NOVALUE,
	 {'TerminalInfo',asn1_NOVALUE},
	 false,false},
	asn1_NOVALUE,
	{'CallIdentifier',<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>},
	asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}},
      asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
     asn1_NOVALUE}.

alerting_enc() ->
    "0380060008914a0002020120110000000000000000000000000000000000".

connect_val() ->
    {'H323-UserInformation',
     {'H323-UU-PDU',
      {connect,
       {'Connect-UUIE',
	{0,0,8,2250,0,2},
	{ipAddress,
	 {'TransportAddress_ipAddress',<<136,225,41,58>>,1187}},
	{'EndpointType',asn1_NOVALUE,
	 {'VendorIdentifier',
	  {'H221NonStandard',181,0,21324},
	  <<77,105,99,114,111,115,111,102,116,174,32,78,101,116,
	   77,101,100,116,105,110,103,174,0>>,
	  <<51,46,48,0>>},
	 asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
	 {'TerminalInfo',asn1_NOVALUE},
	 false,false},
	<<22,137,237,197,191,35,211,17,140,45,0,192,79,75,28,208>>,
	{'CallIdentifier',<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>},
	asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}},
      asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE},
     asn1_NOVALUE}.

connect_enc() ->
    "02c0060008914a00020088e1293a04a322c0b500534c164d6963726f736f6674ae204e65744d656474696e67ae0003332e3000001689edc5bf23d3118c2d00c04f4b1cd00900110000000000000000000000000000000000".

general_string() ->
    Type = 'MultimediaSystemControlMessage',
    UI = <<109,64,1,57>>,
    {ok, _V} = 'MULTIMEDIA-SYSTEM-CONTROL':decode(Type, UI).

roundtrip(T, V, HexString) ->
    Enc = asn1_test_lib:hex_to_bin(HexString),
    Enc = asn1_test_lib:roundtrip_enc('H323-MESSAGES', T, V),
    ok.
