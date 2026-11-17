%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2005-2025. All Rights Reserved.
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

%%% Description : SSH 1/2 pdu elements encode/decode

-module(ssh_bits).
-moduledoc false.

-include("ssh.hrl").

-export([mpint/1, name_list/1]).
-export([random/1]).

%%%----------------------------------------------------------------
name_list(NamesList) -> list_to_binary(lists:join($,, NamesList)).

%%%----------------------------------------------------------------
%%% Multi Precision Integer encoding
mpint(0) ->
    <<0:32>>;
mpint(N) ->
    NumBytes = (integer_signed_width(N) + 7) bsr 3,
    <<NumBytes:32,N:NumBytes/unit:8>>.

%%%----------------------------------------------------------------
%% random/1
%%   Generate N random bytes
%%
random(N) -> crypto:strong_rand_bytes(N).



