%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2019. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Handle client side TLS-1.3 session ticket storage
%%----------------------------------------------------------------------

-module(tls_client_ticket_store).
-behaviour(gen_server).

-include("tls_handshake_1_3.hrl").

%% API
-export([find_ticket/2,
         get_tickets/2,
         lock_tickets/2,
         remove_tickets/1,
         start_link/2,
         store_ticket/4,
         unlock_tickets/2,
         update_ticket/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                db,
                lifetime,
                max
               }).

-record(data, {
               pos = undefined,
               hkdf,
               sni,
               psk,
               timestamp,
               ticket,
               lock = undefined
              }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(integer(), integer()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Max, Lifetime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Max, Lifetime], []).

find_ticket(Pid, HashAlgos) ->
    %% TODO use also SNI when selecting tickets
    gen_server:call(?MODULE, {find_ticket, Pid, HashAlgos}, infinity).

get_tickets(Pid, Keys) ->
    gen_server:call(?MODULE, {get_tickets, Pid, Keys}, infinity).

lock_tickets(_, undefined) ->
    ok;
lock_tickets(Pid, Keys) ->
    gen_server:call(?MODULE, {lock, Pid, Keys}, infinity).

remove_tickets([]) ->
    ok;
remove_tickets(Keys) ->
    gen_server:cast(?MODULE, {remove_tickets, Keys}).

store_ticket(Ticket, HKDF, SNI, PSK) ->
    gen_server:call(?MODULE, {store_ticket, Ticket, HKDF, SNI, PSK}, infinity).

unlock_tickets(Pid, Keys) ->
    gen_server:call(?MODULE, {unlock, Pid, Keys}, infinity).

update_ticket(Key, Pos) ->
    gen_server:call(?MODULE, {update_ticket, Key, Pos}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: term()}.

init(Args) ->
    process_flag(trap_exit, true),
    State = inital_state(Args),
    {ok, State}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
                         {reply, Reply :: term(), NewState :: term()} .
handle_call({find_ticket, Pid, HashAlgos}, _From, State) ->
    Key = find_ticket(State, Pid, HashAlgos),
    {reply, Key, State};
handle_call({get_tickets, Pid, Keys}, _From, State) ->
    Data = get_tickets(State, Pid, Keys),
    {reply, Data, State};
handle_call({lock, Pid, Keys}, _From, State0) ->
    State = lock_tickets(State0, Pid, Keys),
    {reply, ok, State};
handle_call({store_ticket, Ticket, HKDF, SNI, PSK}, _From, State0) ->
    State = store_ticket(State0, Ticket, HKDF, SNI, PSK),
    {reply, ok, State};
handle_call({unlock, Pid, Keys}, _From, State0) ->
    State = unlock_tickets(State0, Pid, Keys),
    {reply, ok, State};
handle_call({update_ticket, Key, Pos}, _From, State0) ->
    State = update_ticket(State0, Key, Pos),
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()}.
handle_cast({remove_tickets, Key}, State0) ->
    State = remove_tickets(State0, Key),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()}.
handle_info(remove_invalid_tickets, State0) ->
    State = remove_invalid_tickets(State0),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
                                      {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.
%%%===================================================================
%%% Internal functions
%%%===================================================================

inital_state([Max, Lifetime]) ->
    erlang:send_after(Lifetime * 1000, self(), remove_invalid_tickets),
    #state{db = gb_trees:empty(),
           lifetime = Lifetime,
           max = Max
          }.


find_ticket(_, _, []) ->
    undefined;
find_ticket(#state{db = Db,
                   lifetime = Lifetime} = State, Pid, [Hash|T]) ->
    case iterate_tickets(gb_trees:iterator(Db), Pid, Hash, Lifetime) of
        none ->
            find_ticket(State, Pid, T);
        Key ->
            Key
    end.


iterate_tickets(Iter0, Pid, Hash, Lifetime) ->
    case gb_trees:next(Iter0) of
        {Key, #data{hkdf = Hash,
                    timestamp = Timestamp,
                    lock = Lock}, Iter} when Lock =:= undefined orelse
                                             Lock =:= Pid ->
            Age = erlang:system_time(seconds) - Timestamp,
            if Age < Lifetime ->
                    Key;
               true ->
                    iterate_tickets(Iter, Pid, Hash, Lifetime)
            end;
        {_, _, Iter} ->
            iterate_tickets(Iter, Pid, Hash, Lifetime);
        none ->
            none
    end.


%% Get tickets that are not locked by another process
get_tickets(State, Pid, Keys) ->
    get_tickets(State, Pid, Keys, []).
%%
get_tickets(_, _, [], []) ->
    undefined; %% No tickets found
get_tickets(_, _, [], Acc) ->
    Acc;
get_tickets(#state{db = Db} = State, Pid, [Key|T], Acc) ->
    try gb_trees:get(Key, Db) of
        #data{pos = Pos,
              hkdf = HKDF,
              psk = PSK,
              timestamp = Timestamp,
              ticket = NewSessionTicket,
              lock = Lock} when Lock =:= undefined orelse
                                Lock =:= Pid ->
            #new_session_ticket{
               ticket_lifetime = _LifeTime,
               ticket_age_add = AgeAdd,
               ticket_nonce = Nonce,
               ticket = Ticket,
               extensions = _Extensions
              } = NewSessionTicket,
            TicketAge =  erlang:system_time(seconds) - Timestamp,
            ObfuscatedTicketAge = obfuscate_ticket_age(TicketAge, AgeAdd),
            Identity = #psk_identity{
                          identity = Ticket,
                          obfuscated_ticket_age = ObfuscatedTicketAge},
            get_tickets(State, Pid, T, [{Key, Pos, Identity, PSK, Nonce, HKDF}|Acc])
    catch
        _:_ ->
            get_tickets(State, Pid, T, Acc)
    end.

%% The "obfuscated_ticket_age"
%% field of each PskIdentity contains an obfuscated version of the
%% ticket age formed by taking the age in milliseconds and adding the
%% "ticket_age_add" value that was included with the ticket
%% (see Section 4.6.1), modulo 2^32.
obfuscate_ticket_age(TicketAge, AgeAdd) ->
    (TicketAge + AgeAdd) rem round(math:pow(2,32)).


remove_tickets(State, []) ->
    State;
remove_tickets(State0, [Key|T]) ->
    remove_tickets(remove_ticket(State0, Key), T).


remove_ticket(#state{db = Db0} = State, Key) ->
    Db = gb_trees:delete_any(Key, Db0),
    State#state{db = Db}.


remove_invalid_tickets(#state{db = Db,
                              lifetime = Lifetime} = State0) ->
    Keys = collect_invalid_tickets(gb_trees:iterator(Db), Lifetime),
    State = remove_tickets(State0, Keys),
    erlang:send_after(Lifetime * 1000, self(), remove_invalid_tickets),
    State.


collect_invalid_tickets(Iter, Lifetime) ->
    collect_invalid_tickets(Iter, Lifetime, []).
%%
collect_invalid_tickets(Iter0, Lifetime, Acc) ->
    case gb_trees:next(Iter0) of
        {Key, #data{timestamp = Timestamp,
                    lock = undefined}, Iter} ->
            Age = erlang:system_time(seconds) - Timestamp,
            if Age < Lifetime ->
                    collect_invalid_tickets(Iter, Lifetime, Acc);
               true ->
                    collect_invalid_tickets(Iter, Lifetime, [Key|Acc])
            end;
        {_, _, Iter} ->  %% Skip locked tickets
            collect_invalid_tickets(Iter, Lifetime, Acc);
        none ->
            Acc
    end.


store_ticket(#state{db = Db0, max = Max} = State, Ticket, HKDF, SNI, PSK) ->
    Timestamp = erlang:system_time(seconds),
    Size = gb_trees:size(Db0),
    Db1 = if Size =:= Max ->
                  delete_oldest(Db0);
             true ->
                  Db0
          end,
    Key = erlang:monotonic_time(),
    Db = gb_trees:insert(Key,
                         #data{hkdf = HKDF,
                               sni = SNI,
                               psk = PSK,
                               timestamp = Timestamp,
                               ticket = Ticket},
                         Db1),
    State#state{db = Db}.


update_ticket(#state{db = Db0} = State, Key, Pos) ->
    try gb_trees:get(Key, Db0) of
        Value ->
            Db = gb_trees:update(Key, Value#data{pos = Pos}, Db0),
            State#state{db = Db}
    catch
        _:_ ->
            State
    end.


delete_oldest(Db0) ->
    try gb_trees:take_smallest(Db0) of
        {_, _, Db} ->
            Db
    catch
        _:_ ->
            Db0
    end.


lock_tickets(State, Pid, Keys) ->
    set_lock(State, Pid, Keys, lock).


unlock_tickets(State, Pid, Keys) ->
    set_lock(State, Pid, Keys, unlock).


set_lock(State, _, [], _) ->
    State;
set_lock(#state{db = Db0} = State, Pid, [Key|T], Cmd) ->
    try gb_trees:get(Key, Db0) of
        Value ->
            Db = gb_trees:update(Key, update_data_lock(Value, Pid, Cmd), Db0),
            set_lock(State#state{db = Db}, Pid, T, Cmd)
    catch
        _:_ ->
            set_lock(State, Pid, T, Cmd)
    end.


update_data_lock(Value, Pid, lock) ->
    Value#data{lock = Pid};
update_data_lock(#data{lock = Pid} = Value, Pid, unlock) ->
    Value#data{lock = undefined};
update_data_lock(Value, _, _) ->
    Value.
