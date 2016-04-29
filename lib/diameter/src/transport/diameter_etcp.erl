%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%% This module implements a transport_module that uses Erlang message
%% passing for transport.
%%

-module(diameter_etcp).

-behaviour(gen_server).

%% transport_module interface.
-export([start/3]).

%% gen_tcp-ish interface used by diameter_tcp.
-export([listen/2,
         accept/1,
         connect/3,
         send/2,
         close/1,
         setopts/2,
         sockname/1,
         peername/1,
         getstat/1]).

%% child start
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Server states.

-record(listener,
        {acceptors = [] :: [pid()]}).

-record(connection,
        {parent :: pid(),
         peer   :: {connect, reference()} %% {connect, MRef}
                 | accept
                 | pid()}).

%% start/3

%% 'module' option makes diameter_tcp call here instead of gen_tcp/inet.
start(T, Svc, Opts)
  when is_list(Opts) ->
    diameter_etcp_sup:start(),
    diameter_tcp:start(T, Svc, [{module, ?MODULE} | Opts]).

%% listen/2
%%
%% Spawn a process that represents the listening socket. The local
%% port number can be any term, not just an integer. The listener
%% process registers its host/port with diameter_reg and this is the
%% handle with which connect/3 finds the appropriate listening
%% process.

listen(LPort, Opts) ->
    Parent = self(),
    diameter_etcp_sup:start_child({listen, Parent, LPort, Opts}).

%% accept/1
%%
%% Output: pid()

accept(LPid) ->
    start(fun(Ref, Parent) -> acceptor(LPid, Ref, Parent) end).

%% connect/3
%%
%% Output: pid()

%% RAddr here can either be a 4/8-tuple address or {Node, Addr}.
connect(RAddr, RPort, _Opts) ->
    start(fun(Ref, Parent) -> connector(RAddr, RPort, Ref, Parent) end).

%% send/2

send(Pid, Bin) ->
    Pid ! {send, Bin},
    ok.

%% close/1

close(Pid) ->
    Pid ! close,
    monitor(Pid),
    receive {'DOWN', _, process, Pid, _} -> ok end.

%% setopts/2

setopts(_, _) ->
    ok.

%% sockname/1

sockname(_) ->
    {error, ?MODULE}.

%% peername/1

peername(_) ->
    {error, ?MODULE}.

%% getstat/1

getstat(_) ->
    {error, ?MODULE}.

%% start_link/1

start_link(T) ->
    gen_server:start_link(?MODULE, T, []).

%% ---------------------------------------------------------------------------
%% # init/1
%% ---------------------------------------------------------------------------

%% Maintain a list of acceptor pids as the process state. Each accept
%% adds a pid to the list, each connect removes one.
init({listen, Parent, LPort, Opts}) ->
    monitor(Parent),
    {ip, LAddr} = lists:keyfind(ip, 1, Opts),
    true = diameter_reg:add_new({?MODULE, listener, LAddr, LPort}),
    {ok, #listener{}};

init({connect, Fun, Ref, Parent}) ->
    {ok, #connection{parent = Parent,
                     peer = Fun(Ref, Parent)}}.

%% ---------------------------------------------------------------------------
%% # handle_call/3
%% ---------------------------------------------------------------------------

handle_call(_, _, State) ->
    {reply, nok, State}.

%% ---------------------------------------------------------------------------
%% # handle_cast/2
%% ---------------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%% ---------------------------------------------------------------------------
%% # handle_info/2
%% ---------------------------------------------------------------------------

handle_info(T, #listener{acceptors = L} = S) ->
    {noreply, S#listener{acceptors = l(T,L)}};

handle_info(T, State) ->
    {noreply, transition(T, State)}.

%% ---------------------------------------------------------------------------
%% # code_change/3
%% ---------------------------------------------------------------------------

code_change(_, State, _) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% # terminate/2
%% ---------------------------------------------------------------------------

terminate(_, _) ->
    ok.

%% ---------------------------------------------------------------------------

monitor(Pid) ->
    erlang:monitor(process, Pid).

putr(Key, Val) ->
    put({?MODULE, Key}, Val).

eraser(Key) ->
    erase({?MODULE, Key}).

%% l/2

l({'DOWN', _, process, _, _} = T, _) ->
    x(T);

%% New accepting process.
l({accept, APid}, As) ->
    As ++ [APid];

%% Peer wants to connect but we have no acceptor ...
l({connect, Peer}, [] = As) ->
    Peer ! {refused, self()},
    As;

%% ... or we do.
l({connect, Peer}, [APid | Rest]) ->
    Peer ! {accepted, APid},
    Rest.

x(T) ->
    exit({shutdown, T}).

%% start/1

start(Fun) ->
    Ref = make_ref(),
    {ok, Pid}
        = T
        = diameter_etcp_sup:start_child({connect, Fun, Ref, self()}),
    MRef = monitor(Pid),
    receive
        {ok, Ref} ->
            T;
        {'DOWN', MRef, process, _, Reason} ->
            {error, Reason}
    end.

%% acceptor/3

acceptor(LPid, Ref, Parent) ->
    LPid ! {accept, self()},  %% announce that we're accepting
    putr(ref, {ok, Ref}),
    monitor(Parent),
    monitor(LPid),
    accept.

%% connector/4

connector(RAddr, RPort, Ref, Parent) ->
    c(match(RAddr, RPort), Ref, Parent).

c([], _, _) ->
    x(refused);

c([{_,LPid}], Ref, Parent) ->
    LPid ! {connect, self()},
    putr(ref, {ok, Ref}),
    monitor(Parent),
    {connect, monitor(LPid)}.

match({Node, RAddr}, RPort) ->
    rpc:call(Node, diameter_reg, match, [{?MODULE, listener, RAddr, RPort}]);

match(RAddr, RPort) ->
    match({node(), RAddr}, RPort).

%% transition/2

%% Unexpected parent or peer death.
transition({'DOWN', _, process, _, _} = T, S) ->
    element(2,S) ! {tcp_error, self(), T},
    x(T);

%% Connector is receiving acceptor pid from listener.
transition({accepted, Peer}, #connection{parent = Parent,
                                         peer = {connect, MRef}}) ->
    monitor(Peer),
    erlang:demonitor(MRef, [flush]),
    Peer ! {connect, self()},
    Parent ! {ok, _} = eraser(ref),
    #connection{parent = Parent,
                peer = Peer};

%% Connector is receiving connection refusal from listener.
transition({refused, _} = T, #connection{peer = {connect, _}}) ->
    x(T);

%% Acceptor is receiving peer connect.
transition({connect, Peer}, #connection{parent = Parent,
                                        peer = accept}) ->
    monitor(Peer),
    Parent ! {ok, _} = eraser(ref),
    #connection{parent = Parent,
                peer = Peer};

%% Incoming message.
transition({recv, Bin}, #connection{parent = Parent} = S) ->
    Parent ! {tcp, self(), Bin},
    S;

%% Outgoing message.
transition({send, Bin}, #connection{peer = Peer} = S) ->
    Peer ! {recv, Bin},
    S;

%% diameter_etcp:close/1 call when a peer is connected ...
transition(close = T, #connection{peer = Peer})
  when is_pid(Peer) ->
    Peer ! {close, self()},
    x(T);

%% ... or not.
transition(close = T, #connection{}) ->
    x(T);

%% Peer is closing the connection.
transition({close, Peer} = T, #connection{parent = Parent,
                                          peer = Peer})
  when is_pid(Peer) ->
    Parent ! {tcp_closed, self()},
    x(T).
