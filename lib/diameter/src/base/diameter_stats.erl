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
%% Statistics collector.
%%

-module(diameter_stats).
-behaviour(gen_server).

-export([reg/2, reg/1,
         incr/3, incr/1,
         read/1,
         sum/1,
         flush/1]).

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

%% ets table containing 2-tuple stats. reg(Pid, Ref) inserts a {Pid,
%% Ref}, incr(Counter, X, N) updates the counter keyed at {Counter,
%% X}, and Pid death causes counters keyed on {Counter, Pid} to be
%% deleted and added to those keyed on {Counter, Ref}.
-define(TABLE, ?MODULE).

%% Name of registered server.
-define(SERVER, ?MODULE).

%% Server state.
-record(state, {id = diameter_lib:now()}).

-type counter() :: any().
-type ref() :: any().

%% ---------------------------------------------------------------------------
%% # reg(Pid, Ref)
%%
%% Register a process as a contributor of statistics associated with a
%% specified term. Statistics can be contributed by specifying either
%% Pid or Ref as the second argument to incr/3. Statistics contributed
%% by Pid are folded into the corresponding entry for Ref when the
%% process dies.
%% ---------------------------------------------------------------------------

-spec reg(pid(), ref())
   -> boolean().

reg(Pid, Ref)
  when is_pid(Pid) ->
    try
        call({reg, Pid, Ref})
    catch
        exit: _ -> false
    end.

-spec reg(ref())
   -> boolean().

reg(Ref) ->
    reg(self(), Ref).

%% ---------------------------------------------------------------------------
%% # incr(Counter, Ref, N)
%%
%% Increment a counter for the specified contributor.
%%
%% Ref will typically be an argument passed to reg/2 but there's
%% nothing that requires this. Only registered pids can contribute
%% counters however, otherwise incr/3 is a no-op.
%% ---------------------------------------------------------------------------

-spec incr(counter(), ref(), integer())
   -> integer() | false.

incr(Ctr, Ref, N)
  when is_integer(N) ->
    update_counter({Ctr, Ref}, N).

incr(Ctr) ->
    incr(Ctr, self(), 1).

%% ---------------------------------------------------------------------------
%% # read(Refs)
%%
%% Retrieve counters for the specified contributors.
%% ---------------------------------------------------------------------------

%% Read in the server process to ensure that counters for a dying
%% contributor aren't folded concurrently with select.

-spec read([ref()])
   -> [{ref(), [{counter(), integer()}]}].

read(Refs)
  when is_list(Refs) ->
    try call({read, Refs, false}) of
        L -> to_refdict(L)
    catch
        exit: _ -> []
    end.

read(Refs, B) ->
    MatchSpec = [{{{'_', '$1'}, '_'},
                  [?ORCOND([{'=:=', '$1', {const, R}}
                            || R <- Refs])],
                  ['$_']}],
    L = ets:select(?TABLE, MatchSpec),
    B andalso delete(L),
    L.

to_refdict(L) ->
    lists:foldl(fun append/2, orddict:new(), L).

%% Order both references and counters in the returned list.
append({{Ctr, Ref}, N}, Dict) ->
    orddict:update(Ref,
                   fun(D) -> orddict:store(Ctr, N, D) end,
                   [{Ctr, N}],
                   Dict).

%% ---------------------------------------------------------------------------
%% # sum(Refs)
%%
%% Retrieve counters summed over all contributors for each term.
%% ---------------------------------------------------------------------------

-spec sum([ref()])
   -> [{ref(), [{counter(), integer()}]}].

sum(Refs)
  when is_list(Refs) ->
    try call({read, Refs}) of
        L -> [{R, to_ctrdict(Cs)} || {R, [_|_] = Cs} <- L]
    catch
        exit: _ -> []
    end.

read_refs(Refs) ->
    [{R, readr(R)} || R <- Refs].

readr(Ref) ->
    MatchSpec = [{{{'_', '$1'}, '_'},
                  [?ORCOND([{'=:=', '$1', {const, R}}
                            || R <- [Ref | pids(Ref)]])],
                  ['$_']}],
    ets:select(?TABLE, MatchSpec).

pids(Ref) ->
    MatchSpec = [{{'$1', '$2'},
                  [{'=:=', '$2', {const, Ref}}],
                  ['$1']}],
    ets:select(?TABLE, MatchSpec).

to_ctrdict(L) ->
    lists:foldl(fun({{C,_}, N}, D) -> orddict:update_counter(C, N, D) end,
                orddict:new(),
                L).

%% ---------------------------------------------------------------------------
%% # flush(Refs)
%%
%% Retrieve and delete statistics for the specified contributors.
%% ---------------------------------------------------------------------------

-spec flush([ref()])
   -> [{ref(), {counter(), integer()}}].

flush(Refs) ->
    try call({read, Refs, true}) of
        L -> to_refdict(L)
    catch
        exit: _ -> []
    end.

%% ===========================================================================

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

%% ----------------------------------------------------------
%% # init/1
%% ----------------------------------------------------------

init([]) ->
    ets:new(?TABLE, [named_table, set, public, {write_concurrency, true}]),
    {ok, #state{}}.

%% ----------------------------------------------------------
%% # handle_call/3
%% ----------------------------------------------------------

handle_call(state, _, State) ->
    {reply, State, State};

handle_call(uptime, _, #state{id = Time} = State) ->
    {reply, diameter_lib:now_diff(Time), State};

handle_call({incr, T}, _, State) ->
    {reply, update_counter(T), State};

handle_call({reg, Pid, Ref}, _From, State) ->
    B = ets:insert_new(?TABLE, {Pid, Ref}),
    B andalso erlang:monitor(process, Pid),
    {reply, B, State};

handle_call({read, Refs, Del}, _From, State) ->
    {reply, read(Refs, Del), State};

handle_call({read, Refs}, _, State) ->
    {reply, read_refs(Refs), State};

handle_call(Req, From, State) ->
    ?UNEXPECTED([Req, From]),
    {reply, nok, State}.

%% ----------------------------------------------------------
%% # handle_cast/2
%% ----------------------------------------------------------

handle_cast(Msg, State) ->
    ?UNEXPECTED([Msg]),
    {noreply, State}.

%% ----------------------------------------------------------
%% # handle_info/2
%% ----------------------------------------------------------

handle_info({'DOWN', _MRef, process, Pid, _}, State) ->
    down(Pid),
    {noreply, State};

handle_info(Info, State) ->
    ?UNEXPECTED([Info]),
    {noreply, State}.

%% ----------------------------------------------------------
%% # terminate/2
%% ----------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%% ----------------------------------------------------------
%% # code_change/3
%% ----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===========================================================================

%% down/1

down(Pid) ->
    down(lookup(Pid), ets:match_object(?TABLE, {{'_', Pid}, '_'})).

down([{_, Ref} = T], L) ->
    fold(Ref, L),
    delete([T|L]);
down([], L) -> %% flushed
    delete(L).

%% Fold pid-based entries into ref-based ones.
fold(Ref, L) ->
    lists:foreach(fun({{K, _}, V}) -> update_counter({{K, Ref}, V}) end, L).

%% update_counter/2
%%
%% From an arbitrary process. Call to the server process to insert a
%% new element if the counter doesn't exists so that two processes
%% don't insert simultaneously.

update_counter(Key, N) ->
    try
        ets:update_counter(?TABLE, Key, N)
    catch
        error: badarg ->
            call({incr, {Key, N}})
    end.

%% update_counter/1
%%
%% From the server process, when update_counter/2 failed due to a
%% non-existent entry.

update_counter({{_Ctr, Ref} = Key, N} = T) ->
    try
        ets:update_counter(?TABLE, Key, N)
    catch
        error: badarg ->
            (not is_pid(Ref) orelse ets:member(?TABLE, Ref))
                andalso begin insert(T), N end
    end.

insert(T) ->
    ets:insert(?TABLE, T).

lookup(Key) ->
    ets:lookup(?TABLE, Key).

delete(Objs) ->
    lists:foreach(fun({K,_}) -> ets:delete(?TABLE, K) end, Objs).

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
