%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2024. All Rights Reserved.
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
-moduledoc false.
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
        {Ver, _} when Ver > {8,4} ->
            try gen_server:call({?MODULE, Node}, {apply, M, F, Args}, infinity)
            catch
                _:Reason -> {badrpc, {'EXIT', Reason}}
            end;
        _ ->
            rpc:call(Node, M, F, Args)
    end.

init([_Parent]) ->
    {ok, #{}}.

handle_call({apply, mnesia_lib, db_get=Func, Args}, From, State) ->
    apply_lib(Func, Args, From, State);
handle_call({apply, mnesia_lib, db_last=Func, Args}, From, State) ->
    apply_lib(Func, Args, From, State);
handle_call({apply, mnesia_lib, db_first=Func, Args}, From, State) ->
    apply_lib(Func, Args, From, State);
handle_call({apply, mnesia_lib, db_next_key=Func, Args}, From, State) ->
    apply_lib(Func, Args, From, State);
handle_call({apply, mnesia_lib, db_prev_key=Func, Args}, From, State) ->
    apply_lib(Func, Args, From, State);
handle_call({apply, Mod, Func, Args}, From, State) ->
    Fun = apply_fun(Mod, Func, Args, From),
    _Pid = spawn_link(Fun),
    {noreply, State};

handle_call(Msg, _From, State) ->
    mnesia_lib:error("~p got unexpected call: ~tp~n", [?MODULE, Msg]),
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

%%%%

apply_lib(Func, [Tab|_] = Args, From, State) ->
    try
        Ram = ?catch_val({Tab, storage_type}),
        if Ram =:= ram_copies; Ram =:= disc_copies ->
                {reply, apply(mnesia_lib, Func, [Ram|Args]), State};
           true ->
                Fun = apply_fun(mnesia_lib, Func, Args, From),
                _Pid = spawn_link(Fun),
                {noreply, State}
        end
    catch throw:Res -> {reply, Res, State};
          _:Reason -> {reply, {badrpc, {'EXIT', Reason}}, State}
    end.

apply_fun(Mod, Func, Args, From) ->
    fun() ->
            Result = try apply(Mod, Func, Args)
                     catch throw:Res -> Res;
                           _:Reason -> {badrpc, {'EXIT', Reason}}
                     end,
            gen_server:reply(From, Result)
    end.
