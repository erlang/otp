%% 
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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

-ifndef(snmp_usm).

%% RFC7860:4.2
%%  Values of constants M (the length of the secret key in octets) and N
%% (the length of the Message Authentication Code (MAC) output in
%%  octets), and the hash function H used below are:
%%
%%    usmHMAC128SHA224AuthProtocol: M=28, N=16, H=SHA-224;
%%
%%    usmHMAC192SHA256AuthProtocol: M=32, N=24, H=SHA-256;
%%
%%    usmHMAC256SHA384AuthProtocol: M=48, N=32, H=SHA-384;
%%
%%    usmHMAC384SHA512AuthProtocol: M=64, N=48, H=SHA-512.
%%
-define(usmHMAC128SHA224AuthProtocol_secret_key_length, 28).
-define(usmHMAC128SHA224AuthProtocol_mac_length,        16).

-define(usmHMAC192SHA256AuthProtocol_secret_key_length, 32).
-define(usmHMAC192SHA256AuthProtocol_mac_length,        24).

-define(usmHMAC256SHA384AuthProtocol_secret_key_length, 48).
-define(usmHMAC256SHA384AuthProtocol_mac_length,        32).

-define(usmHMAC384SHA512AuthProtocol_secret_key_length, 64).
-define(usmHMAC384SHA512AuthProtocol_mac_length,        48).

-endif. % -ifndef(snmp_usm).




