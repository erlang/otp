%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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
%% Some gen_sctp-specific tests demonstrating problems that were
%% encountered during diameter development but have nothing
%% specifically to do with diameter. At least one of them can cause
%% diameter_traffic_SUITE testcases to fail.
%%

-module(diameter_gen_tcp_SUITE).

-export([suite/0,
         all/0]).

%% testcases
-export([send_long/1]).

-define(LOOPBACK, {127,0,0,1}).
-define(GEN_OPTS, [binary, {active, true}, {ip, ?LOOPBACK}]).

%% ===========================================================================

suite() ->
    [{timetrap, {minutes, 2}}].

all() ->
    [send_long].

%% ===========================================================================

%% send_long/1
%%
%% Test that a long message is received.

send_long(_) ->
    {Sock, SendF} = connection(),
    B = list_to_binary(lists:duplicate(1 bsl 20, $X)),
    ok = SendF(B),
    B = recv(Sock, size(B), []).

recv(_, 0, Acc) ->
    list_to_binary(lists:reverse(Acc));
recv(Sock, N, Acc) ->
    receive
        {tcp, Sock, Bin} ->
            recv(Sock, N - size(Bin), [Bin | Acc]);
        T ->
            {T, Acc}
    end.

%% connection/0

connection() ->
    {ok, LSock} = gen_tcp:listen(0, ?GEN_OPTS),
    {ok, PortNr} = inet:port(LSock),
    LPid = self(),
    {Pid, MRef} = spawn_monitor(fun() -> connect(PortNr, LPid) end),
    {ok, Sock} = gen_tcp:accept(LSock),
    receive
        {Pid, F} ->
            {Sock, F};
        {'DOWN', MRef, process, _, _} = T ->
            T
    end.

%% connect/2

connect(PortNr, LPid) ->
    {ok, Sock} = gen_tcp:connect(?LOOPBACK, PortNr, ?GEN_OPTS),
    LPid ! {self(), fun(B) -> send(Sock, B) end},
    down(LPid).

%% down/1

down(Pid)
  when is_pid(Pid) ->
    down(erlang:monitor(process, Pid));

down(MRef) ->
    receive {'DOWN', MRef, process, _, Reason} -> Reason end.

%% send/2
%%
%% Send from a spawned process just to avoid sending from the
%% receiving process, in case it's significant.

send(Sock, Bin) ->
    {_, MRef} = spawn_monitor(fun() -> exit(gen_tcp:send(Sock, Bin)) end),
    down(MRef).
