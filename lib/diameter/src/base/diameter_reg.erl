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
%% The module implements a simple term -> pid registry.
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
-type pattern() :: term().

-record(state, {id = diameter_lib:now(),
                receivers = dict:new()
                         :: dict:dict(pattern(), [[pid() | term()]%% subscribe
                                                  | from()]),     %% wait
                monitors = sets:new() :: sets:set(pid())}).

%% The ?TABLE bag contains the Key -> Pid mapping, as {Key, Pid}
%% tuples. Each pid is stored in the monitors set to ensure only one
%% monitor for each pid: more are harmless, but unnecessary. A pattern
%% is added to receivers a result of calls to wait/1 or subscribe/2:
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
%% {T, add|remove, {term(), pid()} when associations are added
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
    to_list(fun swap/1).

to_list(Fun) ->
    ets:foldl(fun(T,D) -> append(Fun(T), D) end, orddict:new(), ?TABLE).

append({K,V}, Dict) ->
    orddict:append(K, V, Dict).

id(T) -> T.

%% terms/0

-spec terms()
   -> [{key(), [pid()]}].

terms() ->
    to_list(fun id/1).

swap({X,Y}) -> {Y,X}.

%% subs/0

-spec subs()
   -> [{pattern(), [{pid(), term()}]}].

subs() ->
    #state{receivers = RD} = state(),
    dict:fold(fun sub/3, orddict:new(), RD).

sub(Pat, Ps, Dict) ->
    lists:foldl(fun([P|T], D) -> orddict:append(Pat, {P,T}, D);
                   (_, D) -> D
                end,
                Dict,
                Ps).

%% waits/0

-spec waits()
   -> [{pattern(), [{from(), term()}]}].

waits() ->
    #state{receivers = RD} = state(),
    dict:fold(fun wait/3, orddict:new(), RD).

wait(Pat, Ps, Dict) ->
    lists:foldl(fun({_,_} = F, D) -> orddict:append(Pat, F, D);
                   (_, D) -> D
                end,
                Dict,
                Ps).

%% ----------------------------------------------------------
%% # init/1
%% ----------------------------------------------------------

init(_) ->
    ets:new(?TABLE, [bag, named_table]),
    {ok, #state{}}.

%% ----------------------------------------------------------
%% # handle_call/3
%% ----------------------------------------------------------

handle_call({add, Uniq, Key}, {Pid, _}, S0) ->
    Rec = {Key, Pid},
    S1 = flush(Uniq, Rec, S0),
    {Res, New} = insert(Uniq, Rec),
    {Recvs, S} = add(New, Rec, S1),
    notify(Recvs, Rec),
    {reply, Res, S};

handle_call({remove, Key}, {Pid, _}, S) ->
    Rec = {Key, Pid},
    Recvs = delete([Rec], S),
    ets:delete_object(?TABLE, Rec),
    notify(Recvs, remove),
    {reply, true, S};

handle_call({wait, Pat}, {Pid, _} = From, #state{receivers = RD} = S) ->
    NS = add_monitor(Pid, S),
    case match(Pat) of
        [_|_] = L ->
            {reply, L, NS};
        [] ->
            {noreply, NS#state{receivers = dict:append(Pat, From, RD)}}
    end;

handle_call({subscribe, Pat, T}, {Pid, _}, #state{receivers = RD} = S) ->
    NS = add_monitor(Pid, S),
    {reply, match(Pat), NS#state{receivers = dict:append(Pat, [Pid | T], RD)}};

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

%% add/3

%% Only add a single monitor for any given process, since there's no
%% use to more.
add(true, {_Key, Pid} = Rec, S) ->
    NS = add_monitor(Pid, S),
    {Recvs, RD} = add(Rec, NS),
    {Recvs, S#state{receivers = RD}};

add(false = No, _, S) ->
    {No, S}.

%% add/2

%% Notify processes whose patterns match the inserted key.
add({_Key, Pid} = Rec, #state{receivers = RD}) ->
    dict:fold(fun(Pt, Ps, A) ->
                      add(lists:member(Rec, match(Pt, Pid)), Pt, Ps, Rec, A)
              end,
              {sets:new(), RD},
              RD).

%% add/5

add(true, Pat, Recvs, {_,_} = Rec, {Set, Dict}) ->
    {lists:foldl(fun sets:add_element/2, Set, Recvs),
     remove(fun erlang:is_list/1, Pat, Recvs, Dict)};

add(false, _, _, _, Acc) ->
    Acc.

%% add_monitor/2

add_monitor(Pid, #state{monitors = MS} = S) ->
    add_monitor(sets:is_element(Pid, MS), Pid, S).

%% add_monitor/3

add_monitor(false, Pid, #state{monitors = MS} = S) ->
    monitor(process, Pid),
    S#state{monitors = sets:add_element(Pid, MS)};

add_monitor(true, _, S) ->
    S.

%% delete/2

delete(Recs, #state{receivers = RD}) ->
    lists:foldl(fun(R,S) -> delete(R, RD, S) end, sets:new(), Recs).

%% delete/3

delete({_Key, Pid} = Rec, RD, Set) ->
    dict:fold(fun(Pt, Ps, S) ->
                      delete(lists:member(Rec, match(Pt, Pid)), Rec, Ps, S)
              end,
              Set,
              RD).

%% delete/4

%% Entry matches a pattern ...
delete(true, Rec, Recvs, Set) ->
    lists:foldl(fun(R,S) -> sets:add_element({R, Rec}, S) end,
                Set,
                Recvs);

%% ... or not.
delete(false, _, _, Set) ->
    Set.

%% notify/2

notify(false = No, _) ->
    No;

notify(Recvs, remove = Op) ->
    sets:fold(fun({P,R}, N) -> send(P, R, Op), N+1 end, 0, Recvs);

notify(Recvs, {_,_} = Rec) ->
    sets:fold(fun(P,N) -> send(P, Rec, add), N+1 end, 0, Recvs).

%% send/3

%% No processes waiting on remove, by construction: they've either
%% received notification at add or aren't waiting.
send([Pid | T], Rec, Op) ->
    Pid ! {T, Op, Rec};

send({_,_} = From, Rec, add) ->
    gen_server:reply(From, [Rec]).

%% down/2

down(Pid, #state{monitors = MS} = S) ->
    NS = flush(Pid, S),
    Recvs = delete(match('_', Pid), NS),
    ets:match_delete(?TABLE, {'_', Pid}),
    notify(Recvs, remove),
    NS#state{monitors = sets:del_element(Pid, MS)}.

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
flush(Pid, #state{receivers = RD} = S)
  when is_pid(Pid) ->
    S#state{receivers = dict:fold(fun(Pt,Ps,D) -> flush(Pid, Pt, Ps, D) end,
                                  RD,
                                  RD)}.

%% flush/4

flush(Pid, Pat, Recvs, Dict) ->
    remove(fun(T) -> Pid /= head(T) end, Pat, Recvs, Dict).

%% head/1

head([P|_]) ->
    P;

head({P,_}) ->
    P.

%% remove/4

remove(Pred, Key, Values, Dict) ->
     case lists:filter(Pred, Values) of
         [] ->
             dict:erase(Key, Dict);
         Rest ->
             dict:store(Key, Rest, Dict)
     end.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
