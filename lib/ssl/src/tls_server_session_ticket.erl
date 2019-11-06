%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% Purpose: Handle server side TLS-1.3 session ticket storage 
%%----------------------------------------------------------------------

-module(tls_server_session_ticket).
-behaviour(gen_server).

-include("tls_handshake_1_3.hrl").
-include("ssl_internal.hrl").

%% API
-export([start_link/3,
         new/1,
         new_with_seed/1,
         bloom_filter_add_elem/2,
         bloom_filter_contains/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                stateless,
                stateful,
                lifetime
               }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(atom(), integer(), tuple()) -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link(Mode, Lifetime, AntiReplay) ->
    gen_server:start_link(?MODULE, [Mode, Lifetime, AntiReplay], []).

new(Pid) ->
    gen_server:call(Pid, new_ticket, infinity).

new_with_seed(Pid) ->
    gen_server:call(Pid, new_with_seed, infinity).

bloom_filter_add_elem(Pid, Elem) ->
    gen_server:cast(Pid, {add_elem, Elem}).

bloom_filter_contains(Pid, Elem) ->
    gen_server:call(Pid, {contains, Elem}, infinity).


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
handle_call(new_ticket, _From, #state{stateless = #{nonce := Nonce} = Stateless} = State) -> 
    Ticket = new_ticket(Nonce, State#state.lifetime),
    {reply, Ticket, State#state{stateless = Stateless#{nonce => Nonce + 1}}};
handle_call(new_ticket, _From, State) -> 
    Ticket = new_ticket(State),
    {reply, Ticket, State};
handle_call(new_with_seed, _From, #state{stateless = #{nonce := Nonce, seed := Seed} = Stateless} = State) -> 
    Ticket = new_ticket(Nonce, State#state.lifetime),
    {reply, {Ticket, Seed}, State#state{stateless = Stateless#{nonce => Nonce + 1}}};
handle_call(new_with_seed, _From, #state{} = State) -> 
    Ticket = new_ticket(State),
    {reply, {Ticket, no_seed}, State};
handle_call({contains, Elem}, _From, #state{stateless = #{bloom_filter := BloomFilter}} = State) ->
    Reply = tls_bloom_filter:contains(BloomFilter, Elem),
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) ->
                         {noreply, NewState :: term()}. 
handle_cast({add_elem, Elem}, #state{stateless = #{bloom_filter := BloomFilter0} = Stateless} = State) ->
    BloomFilter = tls_bloom_filter:add_elem(BloomFilter0, Elem),
    {noreply, State#state{stateless = Stateless#{bloom_filter => BloomFilter}}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
                         {noreply, NewState :: term()}.
handle_info(rotate_bloom_filters, #state{stateless = #{bloom_filter := BloomFilter0,
                                                       window := Window} = Stateless} = State) ->
    BloomFilter = tls_bloom_filter:rotate(BloomFilter0),
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    {noreply, State#state{stateless = Stateless#{bloom_filter => BloomFilter}}};
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


inital_state([stateless, Lifetime, undefined]) ->
    #state{stateless = #{nonce => 0,
                         seed => {crypto:strong_rand_bytes(16), 
                                  crypto:strong_rand_bytes(32)}},
           lifetime = Lifetime
          };
inital_state([stateless, Lifetime, {Window, K, M}]) ->
    erlang:send_after(Window * 1000, self(), rotate_bloom_filters),
    #state{stateless = #{bloom_filter => tls_bloom_filter:new(K, M),
                         nonce => 0,
                         seed => {crypto:strong_rand_bytes(16),
                                  crypto:strong_rand_bytes(32)},
                         windows => Window},
           lifetime = Lifetime
          };
inital_state([stateful, Lifetime]) ->
    #state{lifetime = Lifetime,
           stateful = #{db => gb_trees:empty(),
                        max => 1000}
          }.

ticket_age_add() ->
    MaxTicketAge = 7 * 24 * 3600,
    IntMax = round(math:pow(2,32)) - 1,
    MaxAgeAdd = IntMax - MaxTicketAge,
    <<?UINT32(I)>> = crypto:strong_rand_bytes(4),
    case I > MaxAgeAdd of
        true ->
            I - MaxTicketAge;
        false ->
            I
    end.

ticket_nonce(I) ->
    <<?UINT64(I)>>.

new_ticket(Nonce, Lifetime) ->
    TicketAgeAdd = ticket_age_add(),
    #new_session_ticket{
       ticket_lifetime = Lifetime,
       ticket_age_add = TicketAgeAdd,
       ticket_nonce = ticket_nonce(Nonce),
       extensions = #{}
      }.

new_ticket(_) ->
    #new_session_ticket{}.
