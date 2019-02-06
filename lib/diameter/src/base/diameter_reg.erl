%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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
%% A simple term -> pid registry.
%%

-module(diameter_reg).

-behaviour(gen_server).

-export([add/1,
         add_new/1,
         remove/1,
         match/1,
         wait/1,
         subscribe/2]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% test
-export([pids/0,
         terms/0,
         subs/0,
         waits/0]).

%% debug
-export([state/0,
         uptime/0]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-type key() :: term().
-type from() :: {pid(), term()}.
-type rcvr() :: [pid() | term()]  %% subscribe
              | from().           %% wait
-type pattern() :: term().

-record(state, {id = diameter_lib:now(),
                notify = #{} :: #{pattern() => [rcvr()]},
                monitors = sets:new() :: sets:set(pid())}).

%% The ?TABLE bag contains the Key -> Pid mapping, as {Key, Pid}
%% tuples. Each pid is stored in the monitors set to ensure only one
%% monitor for each pid: more are harmless, but unnecessary. A pattern
%% is added to notify a result of calls to wait/1 or subscribe/2:
%% changes to ?TABLE causes processes to be notified as required.

%% ===========================================================================
%% # add(T)
%%
%% Associate the specified term with self(). The list of pids having
%% this or other assocations can be retrieved using match/1.
%%
%% An association is removed when the calling process dies or as a
%% result of calling remove/1. Adding the same term more than once is
%% equivalent to adding it once.
%%
%% Note that since match/1 takes a pattern as argument, specifying a
%% term that contains match variables is probably not a good idea
%% ===========================================================================

-spec add(key())
   -> true.

add(T) ->
    call({add, false, T}).

%% ===========================================================================
%% # add_new(T)
%%
%% Like add/1 but only one process is allowed to have the the
%% association, false being returned if an association already exists.
%% ===========================================================================

-spec add_new(key())
   -> boolean().

add_new(T) ->
    call({add, true, T}).

%% ===========================================================================
%% # remove(Term)
%%
%% Remove any existing association of Term with self().
%% ===========================================================================

-spec remove(key())
   -> true.

remove(T) ->
    call({remove, T}).

%% ===========================================================================
%% # match(Pat)
%%
%% Return the list of associations whose Term, as specified to add/1
%% or add_new/1, matches the specified pattern.
%%
%% Note that there's no guarantee that the returned processes are
%% still alive. (Although one that isn't will soon have its
%% associations removed.)
%% ===========================================================================

-spec match(pattern())
   -> [{key(), pid()}].

match(Pat) ->
    match(Pat, '_').

%% match/2

match(Pat, Pid) ->
    ets:match_object(?TABLE, {Pat, Pid}).

%% ===========================================================================
%% # wait(Pat)
%%
%% Like match/1 but return only when the result is non-empty or fails.
%% It's up to the caller to ensure that the wait won't be forever.
%% ===========================================================================

-spec wait(pattern())
   -> [{key(), pid()}].

wait(Pat) ->
    _ = match(Pat),  %% ensure match can succeed
    call({wait, Pat}).

%% ===========================================================================
%% # subscribe(Pat, T)
%%
%% Like match/1, but additionally receive messages of the form
%% {T, add|remove, {term(), pid()}} when associations are added
%% or removed.
%% ===========================================================================

-spec subscribe(Pat :: any(), T :: term())
   -> [{term(), pid()}].

subscribe(Pat, T) ->
    _ = match(Pat),  %% ensure match can succeed
    call({subscribe, Pat, T}).

%% ===========================================================================

start_link() ->
    ServerName = {local, ?SERVER},
    Options    = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(ServerName, ?MODULE, [], Options).

state() ->
    call(state).

uptime() ->
    call(uptime).

%% pids/0

-spec pids()
   -> [{pid(), [key()]}].

pids() ->
    append(ets:select(?TABLE, [{{'$1','$2'}, [], [{{'$2', '$1'}}]}])).

append(Pairs) ->
    dict:to_list(lists:foldl(fun({K,V}, D) -> dict:append(K, V, D) end,
                             dict:new(),
                             Pairs)).

%% terms/0

-spec terms()
   -> [{key(), [pid()]}].

terms() ->
    append(ets:tab2list(?TABLE)).

%% subs/0

-spec subs()
   -> [{pattern(), [{pid(), term()}]}].

subs() ->
    #state{notify = Dict} = state(),
    [{K, Ts} || {K,Ps} <- maps:to_list(Dict),
                Ts <- [[{P,T} || [P|T] <- Ps]]].

%% waits/0

-spec waits()
   -> [{pattern(), [from()]}].

waits() ->
    #state{notify = Dict} = state(),
    [{K, Ts} || {K,Ps} <- maps:to_list(Dict),
                Ts <- [[T || {_,_} = T <- Ps]]].

%% ----------------------------------------------------------
%% # init/1
%% ----------------------------------------------------------

init(_) ->
    ets:new(?TABLE, [bag, named_table]),
    {ok, #state{}}.

%% ----------------------------------------------------------
%% # handle_call/3
%% ----------------------------------------------------------

handle_call({add, Uniq, Key}, {Pid, _}, S) ->
    Rec = {Key, Pid},
    NS = flush(Uniq, Rec, S),  %% before insert
    {Res, New} = insert(Uniq, Rec),
    {reply, Res, notify(add, New andalso Rec, if New ->
                                                      add_monitor(Pid, NS);
                                                 true ->
                                                      NS
                                              end)};

handle_call({remove, Key}, {Pid, _}, S) ->
    Rec = {Key, Pid},
    {reply, true, try
                      notify(remove, Rec, S)
                  after
                      ets:delete_object(?TABLE, Rec)
                  end};

handle_call({wait, Pat}, {Pid, _} = From, S) ->
    NS = add_monitor(Pid, S),
    case match(Pat) of
        [_|_] = Recs ->
            {reply, Recs, NS};
        [] ->
            {noreply, queue(Pat, From, NS)}
    end;

handle_call({subscribe, Pat, T}, {Pid, _}, S) ->
    {reply, match(Pat), queue(Pat, [Pid | T], add_monitor(Pid, S))};

handle_call(state, _, S) ->
    {reply, S, S};

handle_call(uptime, _, #state{id = Time} = S) ->
    {reply, diameter_lib:now_diff(Time), S};

handle_call(_Req, _From, S) ->
    {reply, nok, S}.

%% ----------------------------------------------------------
%% # handle_cast/2
%% ----------------------------------------------------------

handle_cast(_Msg, S)->
    {noreply, S}.

%% ----------------------------------------------------------
%% # handle_info/2
%% ----------------------------------------------------------

handle_info({'DOWN', _MRef, process, Pid, _}, S) ->
    {noreply, down(Pid, S)};

handle_info(_Info, S) ->
    {noreply, S}.

%% ----------------------------------------------------------
%% # terminate/2
%% ----------------------------------------------------------

terminate(_Reason, _State)->
    ok.

%% ----------------------------------------------------------
%% # code_change/3
%% ----------------------------------------------------------

code_change(_, State, "2.1") ->
    {ok, lists:foldl(fun add_monitor/2,
                     State,
                     ets:select(?TABLE, [{{'_', '$1'}, [], ['$1']}]))};

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===========================================================================

%% insert/2

insert(false, Rec) ->
    Spec = [{'$1', [{'==', '$1', {const, Rec}}], ['$_']}],
    X = '$end_of_table' /= ets:select(?TABLE, Spec, 1),  %% entry exists?
    X orelse ets:insert(?TABLE, Rec),
    {true, not X};

insert(true, Rec) ->
    B = ets:insert_new(?TABLE, Rec),  %% entry inserted?
    {B, B}.

%% add_monitor/2
%%
%% Only add a single monitor for any given process, since there's no
%% use to more.

add_monitor(Pid, #state{monitors = Ps} = S) ->
    case sets:is_element(Pid, Ps) of
        false ->
            monitor(process, Pid),
            S#state{monitors = sets:add_element(Pid, Ps)};
        true ->
            S
    end.

%% notify/3

notify(_, false, S) ->
    S;

notify(Op, {_,_} = Rec, #state{notify = Dict} = S) ->
    S#state{notify = maps:fold(fun(P,Rs,D) -> notify(Op, Rec, P, Rs, D) end,
                               Dict,
                               Dict)}.

%% notify/5

notify(Op, {_, Pid} = Rec, Pat, Rcvrs, Dict) ->
    case lists:member(Rec, match(Pat, Pid)) of
        true ->
            reset(Pat, Dict, [P || P <- Rcvrs, send(P, Op, Rec)]);
        false ->
            Dict
    end.

%% send/3

send([Pid | T], Op, Rec) ->
    Pid ! {T, Op, Rec},
    true;

%% No processes wait on remove: they receive notification immediately
%% or at add, by construction.
send({_,_} = From, add, Rec) ->
    gen_server:reply(From, [Rec]),
    false.

%% down/2

down(Pid, #state{monitors = Ps} = S) ->
    Recs = match('_', Pid),
    Acc0 = flush(Pid, S#state{monitors = sets:del_element(Pid, Ps)}),
    try
        lists:foldl(fun(R,NS) -> notify(remove, R, NS) end, Acc0, Recs)
    after
        ets:match_delete(?TABLE, {'_', Pid})
    end.

%% flush/3

%% Remove any processes that are dead but for which we may not have
%% received 'DOWN' yet, to ensure that add_new can be used to register
%% a unique name each time a registering process restarts.
flush(true, {Key, Pid}, S) ->
    Spec = [{{'$1', '$2'},
             [{'andalso', {'==', '$1', {const, Key}},
                          {'/=', '$2', Pid}}],
             ['$2']}],
    lists:foldl(fun down/2, S, [P || P <- ets:select(?TABLE, Spec),
                                     not is_process_alive(P)]);

flush(false, _, S) ->
    S.

%% flush/2

%% Process has died and should no longer receive messages/replies.
flush(Pid, #state{notify = Dict} = S) ->
    S#state{notify = maps:fold(fun(P,Rs,D) -> flush(Pid, P, Rs, D) end,
                               Dict,
                               Dict)}.

%% flush/4

flush(Pid, Pat, Rcvrs, Dict) ->
    reset(Pat, Dict, [T || T <- Rcvrs, Pid /= head(T)]).

%% head/1

head([P|_]) ->
    P;

head({P,_}) ->
    P.

%% reset/3

reset(Key, Map, []) ->
    maps:remove(Key, Map);

reset(Key, Map, List) ->
    maps:put(Key, List, Map).

%% queue/3

queue(Pat, Rcvr, #state{notify = Dict} = S) ->
    S#state{notify =  maps:put(Pat, [Rcvr | maps:get(Pat, Dict, [])], Dict)}.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
