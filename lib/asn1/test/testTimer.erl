%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(testTimer).
-export([go/0]).

-include_lib("common_test/include/ct.hrl").

-define(times, 5000).

go() ->
    Module = 'H323-MESSAGES',
    Type = 'H323-UserInformation',
    Value = case Module:maps() of
                false -> val_records();
                true -> val_maps()
            end,
    Bytes = Module:encode(Type, Value),
    Value = Module:decode(Type, Bytes),

    {ValWr,done} = timer:tc(fun() -> encode(?times, Module, Type, Value) end),
    io:format("ASN.1 encoding: ~p micro~n", [ValWr / ?times]),

    done = decode(2, Module, Type, Bytes),

    {ValRead,done} = timer:tc(fun() -> decode(?times, Module, Type, Bytes) end),
    io:format("ASN.1 decoding: ~p micro~n", [ValRead /?times]),

    Comment = "encode: "++integer_to_list(round(ValWr/?times)) ++
	" micro, decode: "++integer_to_list(round(ValRead /?times)) ++
	" micro. [" ++ atom_to_list(Module:encoding_rule()) ++ "]",
    {comment,Comment}.

encode(0, _Module,_Type,_Value) ->
    done;
encode(N, Module,Type,Value) ->
    Module:encode(Type, Value),
    encode(N-1, Module, Type, Value).

decode(0, _Module, _Type, _Value) ->
    done;
decode(N, Module, Type, Value) ->
    Module:decode(Type, Value),
    decode(N-1, Module, Type, Value).

val_records() ->
    {'H323-UserInformation',{'H323-UU-PDU',
			     {callProceeding,
			      {'CallProceeding-UUIE',
			       {0,8,222},
			       {'EndpointType',
				{'NonStandardParameter',
				 {object,{0,9,237}},
				 <<"O">>},
				{'VendorIdentifier',
				 {'H221NonStandard',62,63,16282},
				 <<"OC">>,
				 <<"OC">>},
				{'GatekeeperInfo',
				 {'NonStandardParameter',
				  {object,{0,10,260}},
				  <<"O">>}},
				{'GatewayInfo',
				 [{h320,
				   {'H320Caps',
				    {'NonStandardParameter',
				     {object,{0,11,282}},
				     <<"O">>},
				    [{'DataRate',
				      {'NonStandardParameter',
				       {object,
					{0,11,295}},
				       <<"O">>},
				      1290470518,
				      78}],
				    [{'SupportedPrefix',
				      {'NonStandardParameter',
				       {object,
					{0,12,312}},
				       <<"O">>},
				      {'h323-ID',"BM"}}]}}],
				 {'NonStandardParameter',
				  {object,{0,13,326}},
				  <<"O">>}},
				{'McuInfo',
				 {'NonStandardParameter',
				  {object,{1,13,340,340}},
				  <<"OC">>}},
				{'TerminalInfo',
				 {'NonStandardParameter',
				  {object,{1,14,353,354}},
				  <<"OC">>}},
				true,
				true},
			       {ipxAddress,
				{'TransportAddress_ipxAddress',
				 <<"OCTET ">>,
				 <<"OCTE">>,
				 <<"OC">>}},
			       {'CallIdentifier',<<"OCTET STRINGOCTE">>},
			       {noSecurity,'NULL'},
			       [{'ClearToken',
				 1667517741,
				 "BM",
				 {'DHset',<<1:1>>,<<1:1>>,<<1:1>>},
				 <<"OCTET STR">>,
				 -26430296,
				 {'TypedCertificate',
				  {1,16,405,406},
				  <<"OC">>},
				 "BMP",
				 {'NonStandardParameter',
				  {1,16,414,415},
				  <<"OC">>}},
				{'ClearToken',
				 1817656756,
				 "BMP",
				 {'DHset',<<1:1>>,<<1:1>>,<<1:1>>},
				 <<"OCTET STRI">>,
				 -16356110,
				 {'TypedCertificate',
				  {1,17,442,443},
				  <<"OC">>},
				 "BMP",
				 {'NonStandardParameter',
				  {1,18,452,452},
				  <<"OC">>}}],
			       [{cryptoGKPwdEncr,
				 {'CryptoH323Token_cryptoGKPwdEncr',
				  {1,18,467,467},
				  {'Params',-7477016,<<"OCTET ST">>},
				  <<"OC">>}},
				{cryptoGKPwdEncr,
				 {'CryptoH323Token_cryptoGKPwdEncr',
				  {1,19,486,486},
				  {'Params',-2404513,<<"OCTET ST">>},
				  <<>>}}],
			       []}},
			     {'NonStandardParameter',{object,{0,3,84}},<<>>},
			     [],
			     true,
			     [],
			     []},
     {'H323-UserInformation_user-data',24,<<"O">>}}.
    

val_maps() ->
#{'h323-uu-pdu' => #{h245Control => [],
                     h245Tunneling => true,
    'h323-message-body' => {callProceeding,#{callIdentifier => #{guid => <<"OCTET STRINGOCTE">>},
                      cryptoTokens => [{cryptoGKPwdEncr,#{algorithmOID => {1,18,467,467},
                                          encryptedData => <<"OC">>,
                                          paramS => #{iv8 => <<"OCTET ST">>,
                                            ranInt => -7477016}}},
                       {cryptoGKPwdEncr,#{algorithmOID => {1,19,486,486},
                                          encryptedData => <<>>,
                                          paramS => #{iv8 => <<"OCTET ST">>,
                                            ranInt => -2404513}}}],
                      destinationInfo => #{gatekeeper => #{nonStandardData => #{data => <<"O">>,
                            nonStandardIdentifier => {object,{0,10,260}}}},
                        gateway => #{nonStandardData => #{data => <<"O">>,
                            nonStandardIdentifier => {object,{0,13,326}}},
                          protocol => [{h320,#{dataRatesSupported => [#{channelMultiplier => 78,
                                      channelRate => 1290470518,
                                      nonStandardData => #{data => <<"O">>,
                                        nonStandardIdentifier => {object,{0,11,295}}}}],
                                   nonStandardData => #{data => <<"O">>,
                                     nonStandardIdentifier => {object,{0,11,282}}},
                                   supportedPrefixes => [#{nonStandardData => #{data => <<"O">>,
                                        nonStandardIdentifier => {object,{0,12,312}}},
                                      prefix => {'h323-ID',"BM"}}]}}]},
                        mc => true,
                        mcu => #{nonStandardData => #{data => <<"OC">>,
                            nonStandardIdentifier => {object,{1,13,340,340}}}},
                        nonStandardData => #{data => <<"O">>,nonStandardIdentifier => {object,{0,9,237}}},
                        terminal => #{nonStandardData => #{data => <<"OC">>,
                            nonStandardIdentifier => {object,{1,14,353,354}}}},
                        undefinedNode => true,
                        vendor => #{productId => <<"OC">>,
                          vendor => #{manufacturerCode => 16282,
                            t35CountryCode => 62,
                            t35Extension => 63},
                          versionId => <<"OC">>}},
                      fastStart => [],
                      h245Address => {ipxAddress,#{netnum => <<"OCTE">>,
                                    node => <<"OCTET ">>,
                                    port => <<"OC">>}},
                      h245SecurityMode => {noSecurity,'NULL'},
                      protocolIdentifier => {0,8,222},
                      tokens => [#{certificate => #{certificate => <<"OC">>,type => {1,16,405,406}},
                         challenge => <<"OCTET STR">>,
                         dhkey => #{generator => <<1:1>>,halfkey => <<1:1>>,modSize => <<1:1>>},
                         generalID => "BMP",
                         nonStandard => #{data => <<"OC">>,nonStandardIdentifier => {1,16,414,415}},
                         password => "BM",
                         random => -26430296,
                         timeStamp => 1667517741},
                       #{certificate => #{certificate => <<"OC">>,type => {1,17,442,443}},
                         challenge => <<"OCTET STRI">>,
                         dhkey => #{generator => <<1:1>>,halfkey => <<1:1>>,modSize => <<1:1>>},
                         generalID => "BMP",
                         nonStandard => #{data => <<"OC">>,nonStandardIdentifier => {1,18,452,452}},
                         password => "BMP",
                         random => -16356110,
                         timeStamp => 1817656756}]}},
    h4501SupplementaryService => [],
    nonStandardControl => [],
    nonStandardData => #{data => <<>>,nonStandardIdentifier => {object,{0,3,84}}}},
  'user-data' => #{'protocol-discriminator' => 24,'user-information' => <<"O">>}}.
