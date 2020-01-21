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
%% Reads small messages to a socket, until counters wrap!
%%
%% ---------------------------------------------------------------------

-module(esock_iow_server).

-export([start/0]).

-define(LIB,   esock_iow_lib).
-define(LIMIT, 100000).


%% ---------------------------------------------------------------------

start() ->
    Domain  = inet,
    ?LIB:iprint("open"),
    {ok, LSock} = socket:open(Domain, stream, tcp),
    ?LIB:iprint("iow"),
    ok = socket:setopt(LSock, otp, iow, true),
    ?LIB:iprint("bind"),
    LocalSA     = #{family => Domain,
                    addr   => {147,214,93,147}},
    {ok, LPort} = socket:bind(LSock, LocalSA),
    ?LIB:iprint("listen port: ~p", [LPort]),
    ?LIB:iprint("make listen socket"),
    ok      = socket:listen(LSock),
    ?LIB:iprint("accept"),
    {ok, CSock} = socket:accept(LSock),
    ok          = socket:send(CSock, <<"begin">>),
    loop(CSock, 0).

loop(S, N) ->
    case socket:recv(S) of
        {ok, _} when (N < ?LIMIT) ->
            flush(S),
            loop(S, N+1);
        {ok, _} ->
            ?LIB:iprint("Counters:"
                        "~s", [?LIB:format_counters(cnts(S))]),
            flush(S),
            loop(S, 0);
        {error, Reason} ->
            ?LIB:eprint("failed receive: ~p", [Reason])
    end.


cnts(S) ->
    #{counters := Cnts} = socket:info(S),
    Cnts.

flush(S) ->
    receive
        {'$socket', S, counter_wrap, Cnt} ->
            ?LIB:iprint("Counter ~w wrapped", [Cnt]);
        Any ->
            ?LIB:iprint("Received: ~n   ~p~n", [Any])
    after 0 ->
            ok
    end.
    
