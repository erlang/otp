%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
%%  This module implements a server that serializes requests in named
%%  queues. A request is an MFA or fun and a name can be any term. A
%%  request is applied in a dedicated process that terminates when
%%  the request function returns.
%%

-module(diameter_sync).
-behaviour(gen_server).

-export([call/4, call/5,
         cast/4, cast/5,
         carp/1, carp/2]).

%% supervisor callback
-export([start_link/0]).

%% gen_server interface
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% test/debug
-export([state/0,
         uptime/0,
         flush/1,
         pending/0,
         pending/1,
         queues/0,
         pids/1]).

-include("diameter_internal.hrl").

%% Locally registered server name.
-define(SERVER, ?MODULE).

%% Message to the server to queue a request ...
-define(REQUEST(CallOrCast, Name, Req, Max, Timeout),
        {request, CallOrCast, Name, Req, Max, Timeout}).

%% ... and to retrieve the pid of the prevailing request process.
-define(CARP(Name),
        {carp, Name}).

%% Forever ...
-define(TIMEOUT, 30000).

%% Server state.
-record(state,
        {time = diameter_lib:now(),
         pending = 0 :: non_neg_integer(), %% outstanding requests
         monitor = new() :: ets:tid(),     %% MonitorRef -> {Name, From}
         queue   = new() :: ets:tid()}).   %% Name -> queue of {Pid, Ref}

%% ----------------------------------------------------------
%% # call(Node, Name, Req, Max, Timeout)
%% # call(Name, Req, Max, Timeout)
%%
%% Input:  Name    = term() identifying the queue in which the request is
%%                   to be evaluated.
%%         Req     = {M,F,A}
%%                 | {Fun, Arg}
%%                 | [Fun | Args]
%%                 | Fun
%%         Max     = Upper bound for the number of outstanding requests
%%                   in the named queue for Req to be queued.
%%                   If more than this number are in the queue then
%%                   'rejected' is returned to the caller. Can be any
%%                   term but integer() | infinity is sufficient.
%%         Timeout = 32 bit integer() number of milliseconds after which
%%                   request is cancelled (if not already started), causing
%%                   'timeout' to be returned to the caller.
%%                 | infinity
%%
%% Output: Req() | rejected | timeout
%%
%% Description: Serialize a request in a named queue. Note that if
%%              'timeout' is returned and the request itself does not
%%              return this atom then request has not been evaluated.
%% ----------------------------------------------------------

call(Name, Req, Max, Timeout) ->
    call(node(), Name, Req, Max, Timeout).

call(Node, Name, Req, Max, Timeout) ->
    gen_call({?SERVER, Node}, ?REQUEST(call, Name, Req, Max, Timeout)).

%%% ----------------------------------------------------------
%%% # cast(Node, Name, Req, Max, Timeout)
%%% # cast(Name, Req, Max, Timeout)
%%%
%%% Output: ok | rejected | timeout
%%%
%%% Description: Serialize a request without returning the result to the
%%%              caller. Returns after the task is queued.
%%% ----------------------------------------------------------

cast(Name, Req, Max, Timeout) ->
    cast(node(), Name, Req, Max, Timeout).

cast(Node, Name, Req, Max, Timeout) ->
    gen_call({?SERVER, Node}, ?REQUEST(cast, Name, Req, Max, Timeout)).

%% 'timeout' is only return if the server process that processes
%% requests isn't alive. Ditto for call/carp.

%%% ----------------------------------------------------------
%%% # carp(Node, Name)
%%% # carp(Name)
%%%
%%% Output: {value, Pid} | false | timeout
%%%
%%% Description: Return the pid of the process processing the task
%%%              at the head of the named queue. Note that the value
%%%              returned by subsequent calls changes as tasks are
%%%              completed, each task executing in a dedicated
%%%              process. The exit value of this process will be
%%%              {value, Req()} if the task returns.
%%% ----------------------------------------------------------

%% The intention of this is to let a process enqueue a task that waits
%% for a message before completing, the target pid being retrieved
%% with carp/[12].

carp(Name) ->
    carp(node(), Name).

carp(Node, Name) ->
    gen_call({?SERVER, Node}, ?CARP(Name)).

%%% ---------------------------------------------------------
%%% EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

state() ->
    call(state).

uptime() ->
    call(uptime).

flush(Name) ->
    call({flush, Name}).

pending() ->
    call(pending).

pending(Name) ->
    call({pending, Name}).

queues() ->
    call(queues).

pids(Name) ->
    call({pids, Name}).

%%% ----------------------------------------------------------
%%% # start_link()
%%% ----------------------------------------------------------

start_link() ->
    ServerName = {local, ?SERVER},
    Module     = ?MODULE,
    Args       = [],
    Options    = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(ServerName, Module, Args, Options).

%%% ----------------------------------------------------------
%%% # init(_)
%%% ----------------------------------------------------------

init(_) ->
    {ok, #state{}}.

%%% ----------------------------------------------------------
%%% # handle_call(Request, From, State)
%%% ----------------------------------------------------------

%% Enqueue a new request.
handle_call(?REQUEST(Type, Name, Req, Max, Timeout),
            From,
	    #state{queue = QD} = State) ->
    T = find(Name, QD),
    nq(queued(T) =< Max, T, {Type, From}, Name, Req, Timeout, State);

handle_call(Request, From, State) ->
    {reply, call(Request, From, State), State}.

%% call/3

call(?CARP(Name), _, #state{queue = QD}) ->
    pcar(find(Name, QD));

call(state, _, State) ->
    State;

call(uptime, _, #state{time = T}) ->
    diameter_lib:now_diff(T);

call({flush, Name}, _, #state{queue = QD}) ->
    cancel(find(Name, QD));

call(pending, _, #state{pending = N}) ->
    N;

call({pending, Name}, _, #state{queue = QD}) ->
    queued(find(Name, QD));

call(queues, _, #state{queue = QD}) ->
    fetch_keys(QD);

call({pids, Name}, _, #state{queue = QD}) ->
    plist(find(Name, QD));

call(Req, From, _State) ->  %% ignore
    ?UNEXPECTED(handle_call, [Req, From]),
    nok.

%%% ----------------------------------------------------------
%%% handle_cast(Request, State)
%%% ----------------------------------------------------------

handle_cast(Msg, State) ->
    ?UNEXPECTED([Msg]),
    {noreply, State}.

%%% ----------------------------------------------------------
%%% handle_info(Request, State)
%%% ----------------------------------------------------------

handle_info(Request, State) ->
    {noreply, info(Request, State)}.

%% info/2

%% A request has completed execution or timed out.
info({'DOWN', MRef, process, Pid, Info},
     #state{pending = N,
            monitor = MD,
            queue = QD}
     = State) ->
    {Name, From} = fetch(MRef, MD),
    reply(From, rc(Info)),
    State#state{pending = N-1,
                monitor = erase(MRef, MD),
                queue = dq(fetch(Name, QD), Pid, Info, Name, QD)};

info(Info, State) ->
    ?UNEXPECTED(handle_info, [Info]),
    State.

reply({call, From}, T) ->
    gen_server:reply(From, T);
reply(cast, _) ->
    ok.

rc({value, T}) ->
    T;
rc(_) ->
    timeout.

%%% ----------------------------------------------------------
%%% code_change(OldVsn, State, Extra)
%%% ----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ----------------------------------------------------------
%%% terminate(Reason, State)
%%% ----------------------------------------------------------

terminate(_Reason, _State)->
    ok.

%%% ---------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

%% queued/1

queued({ok, {N,_}}) ->
    N;
queued(error) ->
    0.

%% nq/7

%% Maximum number of pending requests exceeded ...
nq(false, _, _, _Name, _Req, _Timeout, State) ->
    {reply, rejected, State};

%% ... or not.
nq(true, T, From, Name, Req, Timeout, #state{pending = N,
                                             monitor = MD,
                                             queue = QD}
                                      = State) ->
    Ref = make_ref(),
    Pid = init(Ref, Req, timeout(Timeout, T)),
    MRef = erlang:monitor(process, Pid),
    {noreply, State#state{pending = N+1,
                          monitor = store(MRef, {Name, from(From)}, MD),
                          queue = store(Name, nq(T, {Pid, Ref}), QD)}}.

from({call, _} = T) ->
    T;
from({cast = T, From}) ->
    gen_server:reply(From, ok),
    T.

%% nq/2

%% Other requests in the queue: append.
nq({ok, {N,Q}}, T) ->
    {N+1, queue:in(T,Q)};

%% Queue is empty: start execution.
nq(error, T) ->
    go(T),
    {1, queue:from_list([T])}.

%% Don't timeout if the request is evaluated immediately so as to
%% avoid a race between getting a 'go' and a 'timeout'. Queueing a
%% request in an empty queue always results in execution.
timeout(_, error) ->
    infinity;
timeout(Timeout, _) ->
    Timeout.

%% dq/5
%%
%% A request process has terminated.

dq({N,Q}, Pid, _Info, Name, QD) ->
    {{value, T}, TQ} = queue:out(Q),
    dq(N-1, Pid, T, TQ, Name, QD).

%% dq/6

%% Request was at the head of the queue: start another.
dq(N, Pid, {Pid, _}, TQ, Name, QD) ->
    dq(N, TQ, Name, QD);

%% Or not: remove the offender from the queue.
dq(N, Pid, T, TQ, Name, QD) ->
    store(Name, {N, req(Pid, queue:from_list([T]), TQ)}, QD).

%% dq/4

%% Queue is empty: erase.
dq(0, TQ, Name, QD) ->
    true = queue:is_empty(TQ),  %% assert
    erase(Name, QD);

%% Start the next request.
dq(N, TQ, Name, QD) ->
    go(queue:head(TQ)),
    store(Name, {N, TQ}, QD).

%% req/3
%%
%% Find and remove the queue element for the specified pid.

req(Pid, HQ, Q) ->
    {{value, T}, TQ} = queue:out(Q),
    req(Pid, T, HQ, TQ).

req(Pid, {Pid, _}, HQ, TQ) ->
    queue:join(HQ, TQ);
req(Pid, T, HQ, TQ) ->
    req(Pid, queue:in(T,HQ), TQ).

%% go/1

go({Pid, Ref}) ->
    Pid ! {Ref, ok}.

%% init/4
%%
%% Start the dedicated process for handling a request. The exit value
%% is as promised by carp/1.

init(Ref, Req, Timeout) ->
    spawn(fun() -> exit(i(Ref, Req, Timeout)) end).

i(Ref, Req, Timeout) ->
    Timer = send_timeout(Ref, Timeout),
    MRef = erlang:monitor(process, ?SERVER),
    receive
        {Ref, ok} -> %% Do the deed.
            %% Ensure we don't leave messages in the mailbox since the
            %% request itself might receive. Alternatively, could have
            %% done the eval in a new process but then we'd have to
            %% relay messages arriving at this one.
            cancel_timer(Timer),
            erlang:demonitor(MRef, [flush]),
            %% Ref is to ensure that we don't extract any message that
            %% a client may have sent after retrieving self() with
            %% carp/1, there being no guarantee that the message
            %% banged by go/1 is received before the pid becomes
            %% accessible.
            {value, eval(Req)};
        {Ref, timeout = T} ->
            T;
        {'DOWN', MRef, process, _Pid, _Info} = D ->  %% server death
            D
    end.

send_timeout(_Ref, infinity = No) ->
    No;
send_timeout(Ref, Ms) ->
    Msg = {Ref, timeout},
    TRef = erlang:send_after(Ms, self(), Msg),
    {TRef, Msg}.

cancel_timer(infinity = No) ->
    No;
cancel_timer({TRef, Msg}) ->
    flush(Msg, erlang:cancel_timer(TRef)).

flush(Msg, false) ->  %% Message has already been sent ...
    %% 'error' should never happen but crash if it does so as not to
    %% hang the process.
    ok = receive Msg -> ok after ?TIMEOUT -> error end;
flush(_, _) ->        %% ... or not.
    ok.

eval({M,F,A}) ->
    apply(M,F,A);
eval([Fun | Args]) ->
    apply(Fun, Args);
eval({Fun, A}) ->
    Fun(A);
eval(Fun) ->
    Fun().

%% pcar/1

pcar({ok, {_,Q}}) ->
    {Pid, _Ref} = queue:head(Q),
    {value, Pid};
pcar(error) ->
    false.

%% plist/1

plist({ok, {_,Q}}) ->
    lists:map(fun({Pid, _Ref}) -> Pid end,  queue:to_list(Q));
plist(error) ->
    [].

%% cancel/1
%%
%% Cancel all but the active request from the named queue. Return the
%% number of requests cancelled.

%% Just send timeout messages to each request to make them die. Note
%% that these are guaranteed to arrive before a go message after the
%% current request completes since both messages are sent from the
%% server process.
cancel({ok, {N,Q}}) ->
    {_,TQ} = queue:split(1,Q),
    foreach(fun({Pid, Ref}) -> Pid ! {Ref, timeout} end, N-1, TQ),
    N-1;
cancel(error) ->
    0.

%% foreach/3

foreach(_, 0, _) ->
    ok;
foreach(Fun, N, Q) ->
    Fun(queue:head(Q)),
    foreach(Fun, N-1, queue:tail(Q)).

%% call/1

%% gen_server:call/3 will exit if the target process dies.
call(Request) ->
    try
        gen_server:call(?SERVER, Request, ?TIMEOUT)
    catch
        exit: Reason ->
            {error, Reason}
    end.

%% dict-like table manipulation.

erase(Key, Dict) ->
    ets:delete(Dict, Key),
    Dict.

fetch(Key, Dict) ->
    {ok, V} = find(Key, Dict),
    V.

fetch_keys(Dict) ->
    ets:foldl(fun({K,_}, Acc) -> [K | Acc] end, [], Dict).

find(Key, Dict) ->
    case ets:lookup(Dict, Key) of
        [{Key, V}] ->
            {ok, V};
        [] ->
            error
    end.

new() ->
    ets:new(?MODULE, [set]).

store(Key, Value, Dict) ->
    store({Key, Value}, Dict).

store({_,_} = T, Dict) ->
    ets:insert(Dict, T),
    Dict.

%% gen_call/1

gen_call(Server, Req) ->
    gen_call(Server, Req, infinity).

gen_call(Server, Req, Timeout) ->
    try
        gen_server:call(Server, Req, Timeout)
    catch
        exit: _ ->
            timeout
    end.
