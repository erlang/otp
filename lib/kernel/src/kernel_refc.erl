%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
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
-module(kernel_refc).

-behaviour(gen_server).

%% External exports
-export([start_link/0, scheduler_wall_time/1]).
%% Internal exports
-export([init/1, handle_info/2, terminate/2]).
-export([handle_call/3, handle_cast/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% This module implements a process that handles reference counters for
%%% various erts or other kernel resources which needs reference counting.
%%%
%%% Should not be documented nor used directly by user applications.
%%%-----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local,kernel_refc}, kernel_refc, [], []).

-spec scheduler_wall_time(boolean()) -> boolean().
scheduler_wall_time(Bool) ->
    gen_server:call(kernel_refc, {scheduler_wall_time, self(), Bool}, infinity).

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------

-spec init([]) -> {'ok', map()}.

init([]) ->
    resource(scheduler_wall_time, false),
    {ok, #{scheduler_wall_time=>#{}}}.

-spec handle_call(term(), term(), State) -> {'reply', term(), State}.
handle_call({What, Who, false}, _From, State) ->
    {Reply, Cnt} = do_stop(What, maps:get(What, State), Who),
    {reply, Reply, State#{What:=Cnt}};
handle_call({What, Who, true}, _From, State) ->
    {Reply, Cnt} = do_start(What, maps:get(What, State), Who),
    {reply, Reply, State#{What:=Cnt}};
handle_call(_, _From, State) ->
    {reply, badarg, State}.

-spec handle_cast(term(), State) -> {'noreply', State}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(term(), State) -> {'noreply', State}.
handle_info({'DOWN', _Ref, process, Pid, _}, State) ->
    Cleanup = fun(Resource, Cnts) ->
                      cleanup(Resource, Cnts, Pid)
              end,
    {noreply, maps:map(Cleanup, State)};
handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), term()) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), State, term()) -> {'ok', State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

do_start(Resource, Cnt, Pid) ->
    case maps:get(Pid, Cnt, undefined) of
        undefined ->
            Ref = erlang:monitor(process, Pid),
            case any(Cnt) of
                true ->
                    {true, Cnt#{Pid=>{1, Ref}}};
                false ->
                    resource(Resource, true),
                    {false, Cnt#{Pid=>{1, Ref}}}
            end;
        {N, Ref} ->
            {true, Cnt#{Pid=>{N+1, Ref}}}
    end.

do_stop(Resource, Cnt0, Pid) ->
    case maps:get(Pid, Cnt0, undefined) of
        undefined ->
            {any(Cnt0), Cnt0};
        {1, Ref} ->
            erlang:demonitor(Ref, [flush]),
            Cnt = maps:remove(Pid, Cnt0),
            case any(Cnt) of
                true ->
                    {true, Cnt};
                false ->
                    resource(Resource, false),
                    {true, Cnt}
            end;
        {N, Ref} ->
            {true, Cnt0#{Pid=>{N-1, Ref}}}
    end.

cleanup(Resource, Cnt0, Pid) ->
    case maps:is_key(Pid, Cnt0) of
        true ->
            Cnt = maps:remove(Pid, Cnt0),
            case any(Cnt) of
                true ->
                    Cnt;
                false ->
                    resource(Resource, false),
                    Cnt
            end;
        false ->
            Cnt0
    end.

any(Cnt) -> maps:size(Cnt) > 0.

resource(scheduler_wall_time, Enable) ->
    _ = erts_internal:scheduler_wall_time(Enable).
