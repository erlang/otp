%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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

%% ---------------------------------------------------------------------
%%
%% Writes small messages to a socket, until counters wrap!
%%
%% ---------------------------------------------------------------------

-module(esock_iow_client).

-export([start/1]).

-define(LIB,   esock_iow_lib).
-define(LIMIT, 100000).

-define(MSG, <<"abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz"
               "abcdefghijklmnopqrstuvxyz">>).


%% ---------------------------------------------------------------------

start(ServerPort) ->
    Domain  = inet,
    ?LIB:iprint("open"),
    {ok, S} = socket:open(Domain, stream, tcp),
    ?LIB:iprint("iow"),
    ok = socket:setopt(S, otp, iow, true),
    ?LIB:iprint("bind"),
    LocalSA = #{family => Domain,
                addr   => {147,214,93,147}},
    {ok, _} = socket:bind(S, LocalSA),
    ?LIB:iprint("connect (to ~w)", [ServerPort]),
    ServerSA = LocalSA#{port => ServerPort},
    ok      = socket:connect(S, ServerSA),
    ?LIB:iprint("connected - now await begin"),
    case socket:recv(S, 0, infinity) of
        {ok, <<"begin">>} ->
            ?LIB:iprint("begin"),
            loop(S, 0);
        {error, Reason} ->
            ?LIB:eprint("failed receive begin: ~p", [Reason])
    end.

loop(S, N) ->
    ok = socket:send(S, ?MSG),
    NextN = receive
                {'$socket', S, counter_wrap, Cnt} ->
                    ?LIB:iprint("Counter ~w wrapped", [Cnt]),
                    N + 1;
                Any ->
                    ?LIB:iprint("Received: ~n   ~p~n", [Any]),
                    N + 1
            after 1 ->
                    if (N < ?LIMIT) ->
                            N+1;
                       true ->
                            ?LIB:iprint("Counters:"
                                        "~s", [?LIB:format_counters(cnts(S))]),
                            0
                    end
            end,
    loop(S, NextN).


cnts(S) ->
    #{counters := Cnts} = socket:info(S),
    Cnts.

