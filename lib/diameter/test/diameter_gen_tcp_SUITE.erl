%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2017. All Rights Reserved.
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
%% Some gen_tcp-specific tests demonstrating problems that were
%% encountered during diameter development but have nothing
%% specifically to do with diameter. These can cause testcases in
%% other suites to fail.
%%

-module(diameter_gen_tcp_SUITE).

-export([suite/0,
         all/0]).

%% testcases
-export([send_long/1,
         connect/1]).

-define(LOOPBACK, {127,0,0,1}).
-define(GEN_OPTS, [binary, {active, true}, {ip, ?LOOPBACK}]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [connect,     %% Appears to fail only when run first.
     send_long].

%% ===========================================================================

%% send_long/1
%%
%% Test that a long message is received.

send_long(_) ->
    {Sock, SendF} = connection(),
    B = binary:copy(<<$X>>, 1 bsl 20),
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

%% send/2
%%
%% Send from a spawned process just to avoid sending from the
%% receiving process, in case it's significant.

send(Sock, Bin) ->
    {_, MRef} = spawn_monitor(fun() -> exit(gen_tcp:send(Sock, Bin)) end),
    down(MRef).

%% ===========================================================================

%% connect/1
%%
%% Test that simultaneous connections succeed. This fails sporadically
%% on OS X at the time of writing, when gen_tcp:connect/2 returns
%% {error, econnreset}.

connect(_) ->
    {ok, LSock} = gen_tcp:listen(0, ?GEN_OPTS),
    {ok, {_,PortNr}} = inet:sockname(LSock),
    Count = lists:seq(1,8),  %% 8 simultaneous connects
    As = [gen_accept(LSock) || _ <- Count],
    %% Wait for spawned processes to have called gen_tcp:accept/1
    %% (presumably).
    receive after 2000 -> ok end,
    Cs = [gen_connect(PortNr) || _ <- Count],
    [] = failures(Cs),
    [] = failures(As).

failures(Monitors) ->
    [RC || {_, MRef} <- Monitors, RC <- [down(MRef)], ok /= element(1, RC)].

gen_accept(LSock) ->
    spawn_monitor(fun() ->
                          exit(gen_tcp:accept(LSock))
                  end).

gen_connect(PortNr) ->
    spawn_monitor(fun() ->
                          exit(gen_tcp:connect(?LOOPBACK, PortNr, ?GEN_OPTS))
                  end).

%% ===========================================================================

%% down/1

down(Pid)
  when is_pid(Pid) ->
    down(monitor(process, Pid));

down(MRef) ->
    receive {'DOWN', MRef, process, _, Reason} -> Reason end.
