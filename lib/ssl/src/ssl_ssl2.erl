%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Handles sslv2 hello as clients supporting sslv2 and higher 
%% will send a sslv2 hello.
%%----------------------------------------------------------------------

-module(ssl_ssl2).
             
-export([client_random/2]).

client_random(ChallengeData, 32) ->
    ChallengeData;
client_random(ChallengeData, N) when N > 32 ->
    <<NewChallengeData:32/binary, _/binary>> = ChallengeData,
    NewChallengeData;
client_random(ChallengeData, N) ->
    Pad = list_to_binary(lists:duplicate(N, 0)),
    <<Pad/binary, ChallengeData/binary>>.
