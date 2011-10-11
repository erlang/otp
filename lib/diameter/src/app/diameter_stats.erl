%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Statistics collector.
%%

-module(diameter_stats).
-compile({no_auto_import, [monitor/2]}).

-behaviour(gen_server).

-export([reg/1, reg/2,
         incr/1, incr/2, incr/3,
         read/1,
         flush/0, flush/1]).

%% supervisor callback
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% debug
-export([state/0,
         uptime/0]).

-include("diameter_internal.hrl").

%% ets table containing stats. reg(Pid, Ref) inserts a {Pid, Ref},
%% incr(Counter, X, N) updates the counter keyed at {Counter, X}, and
%% Pid death causes counters keyed on {Counter, Pid} to be deleted and
%% added to those keyed on {Counter, Ref}.
-define(TABLE, ?MODULE).

%% Name of registered server.
-define(SERVER, ?MODULE).

%% Entries in the table.
-define(REC(Key, Value), {Key, Value}).

%% Server state.
-record(state, {id = now()}).

-type counter() :: any().
-type contrib() :: any().

%%% ---------------------------------------------------------------------------
%%% # reg(Pid, Contrib)
%%%
%%% Description: Register a process as a contributor of statistics
%%%              associated with a specified term. Statistics can be
%%%              contributed by specifying either Pid or Contrib as
%%%              the second argument to incr/3. Statistics contributed
%%%              by Pid are folded into the corresponding entry for
%%%              Contrib when the process dies.
%%%
%%%              Contrib can be any term but should not be a pid
%%%              passed as the first argument to reg/2. Subsequent
%%%              registrations for the same Pid overwrite the association
%%% ---------------------------------------------------------------------------

-spec reg(pid(), contrib())
   -> true.

reg(Pid, Contrib)
  when is_pid(Pid) ->
    call({reg, Pid, Contrib}).

-spec reg(contrib())
   -> true.

reg(Ref) ->
    reg(self(), Ref).

%%% ---------------------------------------------------------------------------
%%% # incr(Counter, Contrib, N)
%%%
%%% Description: Increment a counter for the specified contributor.
%%%
%%%              Contrib will typically be an argument passed to reg/2
%%%              but there's nothing that requires this. In particular,
%%%              if Contrib is a pid that hasn't been registered then
%%%              counters are unaffected by the death of the process.
%%% ---------------------------------------------------------------------------

-spec incr(counter(), contrib(), integer())
   -> integer().

incr(Ctr, Contrib, N) ->
    update_counter({Ctr, Contrib}, N).

incr(Ctr, N)
  when is_integer(N) ->
    incr(Ctr, self(), N);

incr(Ctr, Contrib) ->
    incr(Ctr, Contrib, 1).

incr(Ctr) ->
    incr(Ctr, self(), 1).

%%% ---------------------------------------------------------------------------
%%% # read(Contribs)
%%%
%%% Description: Retrieve counters for the specified contributors.
%%% ---------------------------------------------------------------------------

-spec read([contrib()])
   -> [{contrib(), [{counter(), integer()}]}].

read(Contribs) ->
    lists:foldl(fun(?REC({T,C}, N), D) -> orddict:append(C, {T,N}, D) end,
                orddict:new(),
                ets:select(?TABLE, [{?REC({'_', '$1'}, '_'),
                                     [?ORCOND([{'=:=', '$1', {const, C}}
                                               || C <- Contribs])],
                                     ['$_']}])).

%%% ---------------------------------------------------------------------------
%%% # flush(Contrib)
%%%
%%% Description: Retrieve and delete statistics for the specified
%%%              contributor.
%%%
%%%              If Contrib is a pid registered with reg/2 then statistics
%%%              for both and its associated contributor are retrieved.
%%% ---------------------------------------------------------------------------

-spec flush(contrib())
   -> [{counter(), integer()}].
                   
flush(Contrib) ->
    try
        call({flush, Contrib})
    catch
        exit: _ ->
            []
    end.

flush() ->
    flush(self()).

%%% ---------------------------------------------------------
%%% EXPORTED INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

start_link() ->
    ServerName = {local, ?SERVER},
    Module     = ?MODULE,
    Args       = [],
    Options    = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(ServerName, Module, Args, Options).

state() ->
    call(state).

uptime() ->
    call(uptime).

%%% ----------------------------------------------------------
%%% # init(_)
%%%
%%% Output: {ok, State}
%%% ----------------------------------------------------------

init([]) ->
    ets:new(?TABLE, [named_table, ordered_set, public]),
    {ok, #state{}}.

%% ----------------------------------------------------------
%% handle_call(Request, From, State)
%% ----------------------------------------------------------

handle_call(state, _, State) ->
    {reply, State, State};

handle_call(uptime, _, #state{id = Time} = State) ->
    {reply, diameter_lib:now_diff(Time), State};

handle_call({reg, Pid, Contrib}, _From, State) ->
    monitor(not ets:member(?TABLE, Pid), Pid),
    {reply, insert(?REC(Pid, Contrib)), State};

handle_call({flush, Contrib}, _From, State) ->
    {reply, fetch(Contrib), State};

handle_call(Req, From, State) ->
    ?UNEXPECTED([Req, From]),
    {reply, nok, State}.

%% ----------------------------------------------------------
%% handle_cast(Request, State)
%% ----------------------------------------------------------

handle_cast({incr, Rec}, State) ->
    update_counter(Rec),
    {noreply, State};

handle_cast(Msg, State) ->
    ?UNEXPECTED([Msg]),
    {noreply, State}.

%% ----------------------------------------------------------
%% handle_info(Request, State)
%% ----------------------------------------------------------

handle_info({'DOWN', _MRef, process, Pid, _}, State) ->
    down(Pid),
    {noreply, State};

handle_info(Info, State) ->
    ?UNEXPECTED([Info]),
    {noreply, State}.

%% ----------------------------------------------------------
%% terminate(Reason, State)
%% ----------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%% ----------------------------------------------------------
%% code_change(OldVsn, State, Extra)
%% ----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ---------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%% ---------------------------------------------------------

%% monitor/2

monitor(true, Pid) ->
    erlang:monitor(process, Pid);
monitor(false = No, _) ->
    No.

%% down/1

down(Pid) ->
    L = ets:match_object(?TABLE, ?REC({'_', Pid}, '_')),
    [?REC(_, Ref) = T] = lookup(Pid),
    fold(Ref, L),
    delete_object(T),
    delete(L).

%% Fold Pid-based entries into Ref-based ones.
fold(Ref, L) ->
    lists:foreach(fun(?REC({K, _}, V)) -> update_counter({{K, Ref}, V}) end,
                  L).

delete(Objs) ->
    lists:foreach(fun delete_object/1, Objs).

%% fetch/1

fetch(X) ->
    MatchSpec = [{?REC({'_', '$1'}, '_'),
                  [?ORCOND([{'==', '$1', {const, T}} || T <- [X | ref(X)]])],
                  ['$_']}],
    L = ets:select(?TABLE, MatchSpec),
    delete(L),
    D = lists:foldl(fun sum/2, dict:new(), L),
    dict:to_list(D).

sum({{Ctr, _}, N}, Dict) ->
    dict:update(Ctr, fun(V) -> V+N end, N, Dict).

ref(Pid)
  when is_pid(Pid) ->
    ets:select(?TABLE, [{?REC(Pid, '$1'), [], ['$1']}]);
ref(_) ->
    [].

%% update_counter/2
%%
%% From an arbitrary request process. Cast to the server process to
%% insert a new element if the counter doesn't exists so that two
%% processes don't do so simultaneously.

update_counter(Key, N) ->
    try
        ets:update_counter(?TABLE, Key, N)
    catch
        error: badarg ->
            cast({incr, ?REC(Key, N)})
    end.

%% update_counter/1
%%
%% From the server process.

update_counter(?REC(Key, N) = T) ->
    try
        ets:update_counter(?TABLE, Key, N)
    catch
        error: badarg ->
            insert(T)
    end.

insert(T) ->
    ets:insert(?TABLE, T).

lookup(Key) ->
    ets:lookup(?TABLE, Key).

delete_object(T) ->
    ets:delete_object(?TABLE, T).

%% cast/1

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
