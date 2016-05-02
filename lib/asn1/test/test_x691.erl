%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(test_x691).
-export([cases/1]).

-include_lib("common_test/include/ct.hrl").

cases(Erule) ->
    _ = [begin
	     Mod = module(Name),
	     Msg = msg(Name),
	     Hex = encval(Name, Erule),
	     Enc = asn1_test_lib:hex_to_bin(Hex),
	     Enc = asn1_test_lib:roundtrip_enc(Mod, 'PersonnelRecord', Msg)
	 end || Name <- [a1,a2,a3]],
    ok.

module(a1) -> 'P-RecordA1';
module(a2) -> 'P-RecordA2';
module(a3) -> 'P-RecordA3'.

msg(a1) ->
    {'PersonnelRecord', 
     {'Name',"John", "P", "Smith"}, 
      "Director",
      51, 
      "19710917", 
      {'Name', "Mary", "T", "Smith"},
      [{'ChildInformation',
	{'Name',  "Ralph", "T", "Smith"},  
	"19571111"}, 
       {'ChildInformation',
	{'Name', "Susan", "B", "Jones"}, 
	"19590717"}]};
msg(a2) ->
    msg(a1);
msg(a3) ->
    {'PersonnelRecord',
     {'Name',"John", "P", "Smith"},
     "Director",
     51,
     "19710917",
     {'Name', "Mary", "T", "Smith"},
     [{'ChildInformation',
       {'Name', "Ralph", "T", "Smith"}, 
       "19571111",
       asn1_NOVALUE},
      {'ChildInformation',
       {'Name', "Susan", "B", "Jones"},
       "19590717",
       female}]}.

encval(a1, per) ->
    "80044A6F 686E0150 05536D69 74680133 08446972 6563746F 72083139 37313039 3137044D 61727901 5405536D 69746802 0552616C 70680154 05536D69 74680831 39353731 31313105 53757361 6E014205 4A6F6E65 73083139 35393037 3137";
encval(a1, uper) ->
    "824ADFA3 700D005A 7B74F4D0 02661113 4F2CB8FA 6FE410C5 CB762C1C B16E0937 0F2F2035 0169EDD3 D340102D 2C3B3868 01A80B4F 6E9E9A02 18B96ADD 8B162C41 69F5E787 700C2059 5BF765E6 10C5CB57 2C1BB16E";
encval(a2, per) ->
    "864A6F68 6E501053 6D697468 01330844 69726563 746F7219 7109170C 4D617279 5410536D 69746802 1052616C 70685410 536D6974 68195711 11105375 73616E42 104A6F6E 65731959 0717";
encval(a2, uper) ->
    "865D51D2 888A5125 F1809984 44D3CB2E 3E9BF90C B8848B86 7396E8A8 8A5125F1 81089B93 D71AA229 4497C632 AE222222 985CE521 885D54C1 70CAC838 B8";
encval(a3, per) ->
    "40C04A6F 686E5008 536D6974 68000033 08446972 6563746F 72001971 0917034D 61727954 08536D69 74680100 52616C70 68540853 6D697468 00195711 11820053 7573616E 42084A6F 6E657300 19590717 010140";
encval(a3, uper) ->
    "40CBAA3A 5108A512 5F180330 889A7965 C7D37F20 CB8848B8 19CE5BA2 A114A24B E3011372 7AE35422 94497C61 95711118 22985CE5 21842EAA 60B832B2 0E2E0202 80".
