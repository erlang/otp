%% -*- erlang-indent-level: 2 -*-
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_race_data_server.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 18 Sep 2015 by Luca Favatella <luca.favatella@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(dialyzer_race_data_server).

-export([new/0,
         duplicate/1,
         stop/1,
         call/2,
         cast/2]).

-include("dialyzer.hrl").

%%----------------------------------------------------------------------

-record(state, {race_code     = dict:new() :: dict:dict(),
                public_tables = []         :: [label()],
                named_tables  = []         :: [string()],
                beh_api_calls = []         :: [{mfa(), mfa()}]}).

%%----------------------------------------------------------------------

-spec new() -> pid().

new() ->
  spawn_link(fun() -> loop(#state{}) end).

-spec duplicate(pid()) -> pid().

duplicate(Server) ->
  call(dup, Server).

-spec stop(pid()) -> ok.

stop(Server) ->
  cast(stop, Server).

-spec call(atom(), pid()) -> term().

call(Query, Server) ->
  Ref = make_ref(),
  Server ! {call, self(), Ref, Query},
  receive
    {Ref, Reply} -> Reply
  end.

-spec cast(atom() | {atom(), term()}, pid()) -> ok.

cast(Message, Server) ->
  Server ! {cast, Message},
  ok.

%%----------------------------------------------------------------------

loop(State) ->
  receive
    {call, From, Ref, Query} ->
      Reply = handle_call(Query, State),
      From ! {Ref, Reply},
      loop(State);
    {cast, stop} ->
      ok;
    {cast, Message} ->
      NewState = handle_cast(Message, State),
      loop(NewState)
  end.

handle_cast(race_code_new, State) ->
  State#state{race_code = dict:new()};
handle_cast({Tag, Data}, State) ->
  case Tag of
    renew_race_info -> renew_race_info_handler(Data, State);
    renew_race_code -> renew_race_code_handler(Data, State);
    renew_race_public_tables -> renew_race_public_tables_handler(Data, State);
    put_race_code -> State#state{race_code = Data};
    put_public_tables -> State#state{public_tables = Data};
    put_named_tables -> State#state{named_tables = Data};
    put_behaviour_api_calls -> State#state{beh_api_calls = Data}
  end.

handle_call(Query,
            #state{race_code = RaceCode,
                   public_tables = PublicTables,
                   named_tables = NamedTables,
                   beh_api_calls = BehApiCalls}
            = State) ->
  case Query of
    dup -> spawn_link(fun() -> loop(State) end);
    get_race_code -> RaceCode;
    get_public_tables -> PublicTables;
    get_named_tables -> NamedTables;
    get_behaviour_api_calls -> BehApiCalls
  end.

%%----------------------------------------------------------------------

renew_race_info_handler({RaceCode, PublicTables, NamedTables},
                        #state{} = State) ->
  State#state{race_code = RaceCode,
              public_tables = PublicTables,
              named_tables = NamedTables}.

renew_race_code_handler({Fun, FunArgs, Code},
                        #state{race_code = RaceCode} = State) ->
  State#state{race_code = dict:store(Fun, [FunArgs, Code], RaceCode)}.

renew_race_public_tables_handler(VarLabel,
                                 #state{public_tables = PT} = State) ->
  State#state{public_tables = ordsets:add_element(VarLabel, PT)}.
