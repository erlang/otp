%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

%% Don't use the system rpc server since it may overload other
%% applications when using a lot of dirty read operations.

-module(mnesia_rpc).
-behaviour(gen_server).

-export([start/0,
         call/4
        ]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-include("mnesia.hrl").

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()],
			  [{timeout, infinity} %%, {debug, [trace]}
			  ]).

call(Node, M, F, Args) ->
    case ?catch_val({protocol, Node}) of
        {Ver, _} when Ver < {8,5} ->
            rpc:call(Node, M, F, Args);
        _ ->
            try gen_server:call({?MODULE, Node}, {apply, M, F, Args}, infinity)
            catch
                _:Reason -> {badrpc, {'EXIT', Reason}}
            end
    end.

init([_Parent]) ->
    {ok, #{}}.

handle_call({apply, Mod, Fun, Args}, _From, State) ->
    %% rpc is just for ets:lookups so no need to spawn requests
    Result = try apply(Mod, Fun, Args)
             catch throw:Res -> Res;
                   _:Reason -> {badrpc, {'EXIT', Reason}}
             end,
    {reply, Result, State};
handle_call(Msg, _From, State) ->
    error("~p got unexpected call: ~tp~n", [?MODULE, Msg]),
    {reply, badop, State}.

handle_cast(Msg, State) ->
    mnesia_lib:error("~p got unexpected cast: ~tp~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    mnesia_lib:error("~p got unexpected info: ~tp~n", [?MODULE, Msg]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
