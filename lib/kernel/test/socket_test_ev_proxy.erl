%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% This is a "simple" proxy process that should run in every 
%% new node.
%%
%% It can be used to issue commands "on" the node.
%% It is also used for supervision of the parent (owner) process
%% and (parent) node (if it dies the proxy will issue a 'halt').
%%

-module(socket_test_ev_proxy).

-include("socket_test_evaluator.hrl").
-include("kernel_test_lib.hrl").

-compile({no_auto_import,[process_info/1, process_info/2]}).

-export([
         start/0, start/1, start_monitor/1,
         stop/0, stop/1,
         ping/0, ping/1,
         exec/1, exec/2,
         process_info/1, process_info/2, process_info/3
        ]).



%% This is the command line version of the start function

start() ->
    start([undefined]).

start([Node]) ->
    Self = self(),
    {Pid, MRef} = erlang:spawn_monitor(fun() -> init({Self, Node}) end),
    receive
	{Pid, started} ->
            ?SEV_EPRINT("proxy started"),
            erlang:demonitor(MRef, [flush]),
	    ok;
	{'DOWN', MRef, process, Pid, Reason} ->
            ?SEV_EPRINT("Proxy Start Failure:"
                        "~n   ~p", [Reason]),
	    {error, Reason}
    after 5000 ->
            ?SEV_EPRINT("Proxy Start timeout"),
            erlang:demonitor(MRef, [flush]),
	    {error, timeout}
    end.


%% This function is used when the proxy is started "manually"

start_monitor(Node) ->
    Self = self(),
    {Pid, MRef} = erlang:spawn_monitor(Node, fun() -> init(Self) end),
    receive
	{Pid, started} ->
            ?SEV_EPRINT("started"),
	    ok;
	{'DOWN', MRef, process, Pid, Reason} ->
            ?SEV_EPRINT("Start Failure:"
                        "~n   ~p", [Reason]),
	    {error, Reason}
    after 5000 ->
            ?SEV_EPRINT("Start timeout"),
	    {error, timeout}
    end.


stop() ->
    stop(whereis(?MODULE)).

stop(Proxy) when is_pid(Proxy) ->
    Proxy ! stop;
stop(Node) when is_atom(Node) ->
    true = rpc:cast(Node, ?MODULE, stop, []).


ping() ->
    ping(whereis(?MODULE)).
ping(Proxy) when is_pid(Proxy) ->
    request(Proxy, ping);
ping(Node) when is_atom(Node) ->
    case rpc:call(Node, ?MODULE, ping, []) of
        {badrpc, _} = Reason ->
            {error, Reason};
        Res ->
            Res
    end.


exec(Fun) ->
    exec(whereis(?MODULE), Fun).
exec(Proxy, Fun) when is_pid(Proxy) andalso is_function(Fun, 0) ->
    request(Proxy, {exec, Fun});
exec(Node, Fun) when is_atom(Node) andalso is_function(Fun, 0) ->
    case rpc:call(Node, ?MODULE, exec, [Fun]) of
        {badrpc, _} = Reason ->
            {error, Reason};
        Res ->
            Res
    end.


process_info(Pid) ->
    process_info(whereis(?MODULE), Pid).

process_info(Proxy, Pid) when is_pid(Proxy) andalso is_pid(Pid) ->
    request(Proxy, {process_info, Pid});
process_info(Pid, I) when is_pid(Pid) andalso (is_atom(I) orelse is_list(I)) ->
    process_info(whereis(?MODULE), Pid, I);
process_info(Node, Pid) when is_atom(Node) andalso is_pid(Pid) ->
    case rpc:call(Node, ?MODULE, process_info, [Pid]) of
        {badrpc, _} = Reason ->
            {error, Reason};
        Res ->
            Res
    end.

process_info(Proxy, Pid, Item) when is_pid(Proxy) andalso is_pid(Pid) ->
    request(Proxy, {process_info, Pid, Item});
process_info(Node, Pid, Item) when is_atom(Node) andalso is_pid(Pid) ->
    case rpc:call(Node, ?MODULE, process_info, [Pid, Item]) of
        {badrpc, _} = Reason ->
            {error, Reason};
        Res ->
            Res
    end.


init(Parent) when is_pid(Parent) ->
    Node  = node(Parent),
    MRef  = erlang:monitor(Parent),
    true  = erlang:monitor_node(Node, true),
    STRef = parent_status(),
    PTRef = parent_ping(),
    do_init(fun() -> Parent ! {self(), started} end,
            #{parent_pid    => Parent,
              parent_mref   => MRef,
              parent_node   => Node,
              parent_status => STRef,
              parent_ping   => PTRef,
              reqs          => #{}});
init({Starter, Node}) when is_atom(Node) andalso (Node =/= undefined) ->
    true  = erlang:monitor_node(Node, true),
    STRef = parent_status(),
    PTRef = parent_ping(),
    do_init(fun() -> Starter ! {self(), started} end,
            #{parent_pid    => undefined,
              parent_mref   => undefined,
              parent_node   => Node,
              parent_status => STRef,
              parent_ping   => PTRef,
              reqs          => #{}});
init(undefined = Node) ->
    do_init(fun() -> ok end,
            #{parent_pid    => undefined,
              parent_mref   => undefined,
              parent_node   => Node,
              parent_status => undefined,
              reqs          => #{}}).

do_init(Ack, State) ->
    true = erlang:register(?MODULE, self()),
    Ack(),
    loop(State).


loop(#{parent_pid  := Parent,
       parent_mref := MRef,
       parent_node := Node} = State) ->
    receive
        stop ->
            erlang:halt();
	
	%%
	%% Parent supervision
	%%

	{'DOWN', MRef, process, Parent, Reason} ->
	    %% We should never discover this!
	    %% Normally we (and our node) should be
	    %% stopped before the parent.
	    ?SEV_EPRINT("Parent down: "
                        "~n   Reason: ~p", [Reason]),
	    erlang:halt();

	{nodedown, Node} ->
	    %% We should never discover this!
	    %% Normally we (and our node) should be
	    %% stopped before the parent node.
	    ?SEV_EPRINT("Parent node down"),
	    erlang:halt();

	parent_status ->
	    ?SEV_IPRINT("Check parent status"),
	    initiate_parent_status(State),
	    loop(State);

	{parent_status, Status} ->
	    ?SEV_IPRINT("Got parent status: "
                        "~n   ~p", [Status]),
	    loop(State#{parent_status => parent_status()});

        parent_ping ->
            loop(handle_parent_ping(State));


	%%
	%% Actual requests
	%%

	{request, ID, From, Req} when is_reference(ID) andalso is_pid(From) ->
	    case handle_request(Req, ID, From, State) of
		{Reply, State2} when is_map(State2) ->
		    From ! {reply, ID, self(), Reply},
		    loop(State2);
		State2 when is_map(State2) ->
		    loop(State2)
	    end;

	%% This is most likely a executor that is done
	{'DOWN', MRef, process, Pid, Result} ->
	    case handle_down(MRef, Pid, Result, State) of
		{From, ID, Result, State2} ->
		    From ! {reply, ID, self(), Result},
		    loop(State2);
		State2 when is_map(State2) ->
		    loop(State2)
	    end

    end.


parent_status() ->
    parent_status(5000).
parent_status(Timeout) ->
    erlang:start_timer(Timeout, self(), parent_status).

parent_ping() ->
    parent_ping(1000).
parent_ping(Timeout) ->
    erlang:start_timer(Timeout, self(), parent_ping).

initiate_parent_status(#{parent_pid  := Pid,
                         parent_node := Node}) ->
    Self = self(),
    erlang:spawn(Node, fun() -> collect_parent_status(Pid, Self) end).

collect_parent_status(undefined = _Pid, Parent) ->
    Parent ! {parent_status, ok},
    exit(normal);
collect_parent_status(Pid, Parent) when is_pid(Pid) ->
    Parent ! {parent_status, erlang:process_info(Pid)},
    exit(normal).


handle_parent_ping(#{parent_node := Node} = State) when (Node =/= undefined) ->
    case net_adm:ping(Node) of
        pong ->
            State#{parent_ping => parent_ping()};
        pang ->
	    ?SEV_EPRINT("(net_adm) ping failed - halt'ing"),
            erlang:halt()
    end.


%% Cancel current request (there can only be one per user (=pid).
%% Do we actually need this? All requests are (currently)
%% synchroneous...
handle_request(cancel, From, _ID, #{reqs := Reqs} = State) ->
    %% Only one ongoing request per user
    case maps:get(From, Reqs, undefined) of
	undefined ->
	    Res = {error, nothing},
	    {Res, State};
	{T, _} ->
	    Reqs2 = maps:remove(From, Reqs),
	    Reqs3 = maps:remove(T, Reqs2),
	    {ok, State#{reqs => Reqs3}}
    end;

%% Respond to ping (directly)
handle_request(ping, _From, _ID, State) ->
    {pong, State};

%% Exec a fun
handle_request({exec, Fun}, From, ID, #{reqs := Reqs} = State) ->
    %% Only one ongoing request per user
    case maps:get(From, Reqs, undefined) of
	undefined ->
	    T = erlang:spawn_monitor(Fun),
	    Reqs2 = Reqs#{From => {T, ID},
			  T    => From},
	    State#{reqs => Reqs2};
	_ ->
	    Res = {error, busy},
	    {Res, State}
    end;

%% (miscellaneous) Process info
handle_request({process_info, Pid}, _From, _ID, State) ->
    try erlang:process_info(Pid) of
        Info ->
            {Info, State}
    catch
        C:E:S ->
            Res = {error, {C, E, S}},
            {Res, State}
    end;

%% Process info
handle_request({process_info, Pid, I}, _From, _ID, State) ->
    try erlang:process_info(Pid, I) of
        Info ->
            {Info, State}
    catch
        C:E:S ->
            Res = {error, {C, E, S}},
            {Res, State}
    end;

handle_request(Unknown, From, _ID, State) ->
    ?SEV_IPRINT("Unknown request: "
                "~n   Unknown request: ~p"
                "~n   From:            ~p", [Unknown, From]),
    Res = {error, unknown},
    {Res, State}.


handle_down(MRef, Pid, Result, #{reqs := Reqs} = State) ->
    Key  = {Pid, MRef},
    case maps:get(Key, Reqs, undefined) of
	undefined ->
	    %% Oups - race?
	    ?SEV_IPRINT("DOWN from unknown process: "
			"~n   MRef:   ~p"
			"~n   Pid:    ~p"
			"~n   Result: ~p", [MRef, Pid, Result]),
	    State;
	From when is_pid(From) ->
            {_, ID} = maps:get(From, Reqs),
	    Reqs2   = maps:remove(Key, Reqs),
	    Reqs3   = maps:remove(From, Reqs2),
	    {From, ID, Result, State#{reqs => Reqs3}}
    end.

    
%% ----------------------------------------------------------------

request(Pid, Req) ->
    ID   = make_ref(),
    Pid ! {request, ID, self(), Req},
    receive
        {reply, ID, Pid, Reply} ->
            Reply
    end.
