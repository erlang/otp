%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
%% The module implements a simple term -> pid registry.
%%

-module(diameter_reg).
-behaviour(gen_server).

-compile({no_auto_import, [monitor/2, now/0]}).
-import(diameter_lib, [now/0]).

-export([add/1,
         add_new/1,
         del/1,
         repl/2,
         match/1,
         wait/1]).

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
         terms/0]).

%% debug
-export([state/0,
         uptime/0]).

-include("diameter_internal.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% Table entry used to keep from starting more than one monitor on the
%% same process. This isn't a problem but there's no point in starting
%% multiple monitors if we can avoid it. Note that we can't have a 2-tuple
%% keyed on Pid since a registered term can be anything. Want the entry
%% keyed on Pid so that lookup is fast.
-define(MONITOR(Pid, MRef), {Pid, monitor, MRef}).

%% Table entry containing the Term -> Pid mapping.
-define(MAPPING(Term, Pid), {Term, Pid}).

-record(state, {id = now(),
                q = []}). %% [{From, Pat}]

%% ===========================================================================
%% # add(T)
%%
%% Associate the specified term with self(). The list of pids having
%% this or other assocations can be retrieved using match/1.
%%
%% An association is removed when the calling process dies or as a
%% result of calling del/1. Adding the same term more than once is
%% equivalent to adding it exactly once.
%%
%% Note that since match/1 takes a pattern as argument, specifying a
%% term that contains match variables is probably not a good idea
%% ===========================================================================

-spec add(any())
   -> true.

add(T) ->
    call({add, fun ets:insert/2, T, self()}).

%% ===========================================================================
%% # add_new(T)
%%
%% Like add/1 but only one process is allowed to have the the
%% association, false being returned if an association already exists.
%% ===========================================================================

-spec add_new(any())
   -> boolean().

add_new(T) ->
    call({add, fun insert_new/2, T, self()}).

%% ===========================================================================
%% # repl(T, NewT)
%%
%% Like add/1 but only replace an existing association on T, false
%% being returned if it doesn't exist.
%% ===========================================================================

-spec repl(any(), any())
   -> boolean().

repl(T, U) ->
    call({repl, T, U, self()}).

%% ===========================================================================
%% # del(Term)
%%
%% Remove any existing association of Term with self().
%% ===========================================================================

-spec del(any())
   -> true.

del(T) ->
    call({del, T, self()}).

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

-spec match(any())
   -> [{term(), pid()}].

match(Pat) ->
    ets:match_object(?TABLE, ?MAPPING(Pat, '_')).

%% ===========================================================================
%% # wait(Pat)
%%
%% Like match/1 but return only when the result is non-empty or fails.
%% It's up to the caller to ensure that the wait won't be forever.
%% ===========================================================================

wait(Pat) ->
    call({wait, Pat}).

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
%%
%% Return: list of {Pid, [Term, ...]}

pids() ->
    to_list(fun swap/1).

to_list(Fun) ->
    ets:foldl(fun(T,A) -> acc(Fun, T, A) end, orddict:new(), ?TABLE).

acc(Fun, ?MAPPING(Term, Pid), Dict) ->
    append(Fun({Term, Pid}), Dict);
acc(_, _, Dict) ->
    Dict.

append({K,V}, Dict) ->
    orddict:append(K, V, Dict).

id(T) -> T.

%% terms/0
%%
%% Return: list of {Term, [Pid, ...]}

terms() ->
    to_list(fun id/1).

swap({X,Y}) -> {Y,X}.

%% ----------------------------------------------------------
%% # init/1
%% ----------------------------------------------------------

init(_) ->
    ets:new(?TABLE, [bag, named_table]),
    {ok, #state{}}.

%% ----------------------------------------------------------
%% # handle_call/3
%% ----------------------------------------------------------

handle_call({add, Fun, Key, Pid}, _, S) ->
    B = Fun(?TABLE, {Key, Pid}),
    monitor(B andalso no_monitor(Pid), Pid),
    {reply, B, pending(B, S)};

handle_call({del, Key, Pid}, _, S) ->
    {reply, ets:delete_object(?TABLE, ?MAPPING(Key, Pid)), S};

handle_call({repl, T, U, Pid}, _, S) ->
    MatchSpec = [{?MAPPING('$1', Pid),
                  [{'=:=', '$1', {const, T}}],
                  ['$_']}],
    {reply, repl(ets:select(?TABLE, MatchSpec), U, Pid), S};

handle_call({wait, Pat}, From, #state{q = Q} = S) ->
    case find(Pat) of
        {ok, L} ->
            {reply, L, S};
        false ->
            {noreply, S#state{q = [{From, Pat} | Q]}}
    end;

handle_call(state, _, S) ->
    {reply, S, S};

handle_call(uptime, _, #state{id = Time} = S) ->
    {reply, diameter_lib:now_diff(Time), S};

handle_call(Req, From, S) ->
    ?UNEXPECTED([Req, From]),
    {reply, nok, S}.

%% ----------------------------------------------------------
%% # handle_cast/2
%% ----------------------------------------------------------

handle_cast(Msg, S)->
    ?UNEXPECTED([Msg]),
    {noreply, S}.

%% ----------------------------------------------------------
%% # handle_info/2
%% ----------------------------------------------------------

handle_info({'DOWN', MRef, process, Pid, _}, S) ->
    ets:delete_object(?TABLE, ?MONITOR(Pid, MRef)),
    ets:match_delete(?TABLE, ?MAPPING('_', Pid)),
    {noreply, S};

handle_info(Info, S) ->
    ?UNEXPECTED([Info]),
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

monitor(true, Pid) ->
    ets:insert(?TABLE, ?MONITOR(Pid, erlang:monitor(process, Pid)));
monitor(false, _) ->
    ok.

%% Do we need a monitor for the specified Pid?
no_monitor(Pid) ->
    [] == ets:match_object(?TABLE, ?MONITOR(Pid, '_')).

%% insert_new/2

insert_new(?TABLE, {Key, _} = T) ->
    flush(ets:lookup(?TABLE, Key)),
    ets:insert_new(?TABLE, T).

%% Remove any processes that are dead but for which we may not have
%% received 'DOWN' yet. This is to ensure that add_new can be used
%% to register a unique name each time a process restarts.
flush(List) ->
    lists:foreach(fun({_,P} = T) ->
                          del(erlang:is_process_alive(P), T)
                  end,
                  List).

del(Alive, T) ->
    Alive orelse ets:delete_object(?TABLE, T).

%% repl/3

repl([?MAPPING(_, Pid) = M], Key, Pid) ->
    ets:delete_object(?TABLE, M),
    true = ets:insert(?TABLE, ?MAPPING(Key, Pid));
repl([], _, _) ->
    false.

%% pending/1

pending(true, #state{q = [_|_] = Q} = S) ->
    S#state{q = q(lists:reverse(Q), [])}; %% retain reply order
pending(_, S) ->
    S.

q([], Q) ->
    Q;
q([{From, Pat} = T | Rest], Q) ->
    case find(Pat) of
        {ok, L} ->
            gen_server:reply(From, L),
            q(Rest, Q);
        false ->
            q(Rest, [T|Q])
    end.

%% find/1

find(Pat) ->
    try match(Pat) of
        [] ->
            false;
        L ->
            {ok, L}
    catch
        _:_ ->
            {ok, []}
    end.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
