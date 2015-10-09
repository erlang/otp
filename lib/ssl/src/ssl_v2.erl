%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles sslv2 hello as clients supporting sslv2 and higher
%% will send an sslv2 hello.
%%----------------------------------------------------------------------

-module(ssl_v2).

-export([client_random/2]).

client_random(ChallengeData, 32) ->
    ChallengeData;
client_random(ChallengeData, N) when N > 32 ->
    <<NewChallengeData:32/binary, _/binary>> = ChallengeData,
    NewChallengeData;
client_random(ChallengeData, N) ->
    Pad = list_to_binary(lists:duplicate(N, 0)),
    <<Pad/binary, ChallengeData/binary>>.
