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
-module(logger_proxy).

%% API
-export([start_link/0, restart/0, log/1, child_spec/0, get_default_config/0]).

%% logger_olp callbacks
-export([init/1, handle_load/2, handle_info/2, terminate/2,
         notify/2]).

-include("logger_internal.hrl").

-define(SERVER,?MODULE).

%%%-----------------------------------------------------------------
%%% API
-spec log(RemoteLog) -> ok when
      RemoteLog :: {remote,node(),LogEvent},
      LogEvent :: {log,Level,Format,Args,Meta} |
                  {log,Level,StringOrReport,Meta},
      Level :: logger:level(),
      Format :: io:format(),
      Args :: list(term()),
      StringOrReport :: unicode:chardata() | logger:report(),
      Meta :: logger:metadata().
log(RemoteLog) ->
    Olp = persistent_term:get(?MODULE),
    case logger_olp:get_pid(Olp) =:= self() of
        true ->
            %% This happens when the log event comes from the
            %% emulator, and the group leader is on a remote node.
            _ = handle_load(RemoteLog, no_state),
            ok;
        false ->
            logger_olp:load(Olp, RemoteLog)
    end.

%% Called by supervisor
-spec start_link() -> {ok,pid(),logger_olp:olp_ref()} | {error,term()}.
start_link() ->
    %% Notice that sync_mode is only used when logging to remote node,
    %% i.e. when the log/2 API function is called.
    %%
    %% When receiving log events from the emulator or from a remote
    %% node, the log event is sent as a message to this process, and
    %% thus received directly in handle_info/2. This means that the
    %% mode (async/sync/drop) is not read before the message is
    %% sent. Thus sync mode is never entered, and drop mode is
    %% implemented by setting the system_logger flag to undefined (see
    %% notify/2)
    %%
    %% Burst limit is disabled, since this is only a proxy and we
    %% don't want to limit bursts twice (here and in the handler).
    logger_olp:start_link(?SERVER,?MODULE,[],logger:get_proxy_config()).

%% Fun used for restarting this process after it has been killed due
%% to overload (must set overload_kill_enable=>true in opts)
restart() ->
    case supervisor:start_child(logger_sup, child_spec()) of
        {ok,_Pid,Olp} ->
            {ok,Olp};
        {error,{Reason,Ch}} when is_tuple(Ch), element(1,Ch)==child ->
            {error,Reason};
        Error ->
            Error
    end.

%% Called internally and by logger_sup
child_spec() ->
    Name = ?SERVER,
    #{id       => Name,
      start    => {?MODULE, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [?MODULE]}.

get_default_config() ->
    OlpDefault = logger_olp:get_default_opts(),
    OlpDefault#{sync_mode_qlen=>500,
                drop_mode_qlen=>1000,
                flush_qlen=>5000,
                burst_limit_enable=>false}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    _ = erlang:system_flag(system_logger,self()),
    persistent_term:put(?MODULE,logger_olp:get_ref()),
    {ok,no_state}.

%% Log event to send to the node where the group leader of it's client resides
handle_load({remote,Node,Log},State) ->
    %% If the connection is overloaded (send_nosuspend returns false),
    %% we drop the message.
    _ = erlang:send_nosuspend({?SERVER,Node},Log),
    State;
%% Log event to log on this node
handle_load({log,Level,Format,Args,Meta},State) ->
    try_log([Level,Format,Args,Meta]),
    State;
handle_load({log,Level,Report,Meta},State) ->
    try_log([Level,Report,Meta]),
    State.

%% Log event sent to this process e.g. from the emulator - it is really load
handle_info(Log,State) when is_tuple(Log), element(1,Log)==log ->
    {load,State}.

terminate(overloaded, _State) ->
    _ = erlang:system_flag(system_logger,undefined),
    {ok,fun ?MODULE:restart/0};
terminate(_Reason, _State) ->
    _ = erlang:system_flag(system_logger,whereis(logger)),
    ok.

notify({mode_change,Mode0,Mode1},State) ->
    _ = if Mode1=:=drop -> % entering drop mode
                erlang:system_flag(system_logger,undefined);
           Mode0=:=drop -> % leaving drop mode
                erlang:system_flag(system_logger,self());
           true ->
                ok
        end,
    ?LOG_INTERNAL(notice,"~w switched from ~w to ~w mode",[?MODULE,Mode0,Mode1]),
    State;
notify({flushed,Flushed},State) ->
    ?LOG_INTERNAL(notice, "~w flushed ~w log events",[?MODULE,Flushed]),
    State;
notify(restart,State) ->
    ?LOG_INTERNAL(notice, "~w restarted", [?MODULE]),
    State;
notify(_Note,State) ->
    State.

%%%-----------------------------------------------------------------
%%% Internal functions
try_log(Args) ->
    try apply(logger,log,Args)
    catch C:R:S ->
            ?LOG_INTERNAL(debug,[{?MODULE,log_failed},
                                 {log,Args},
                                 {reason,{C,R,S}}])
    end.
