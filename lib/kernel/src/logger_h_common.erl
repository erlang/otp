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
-module(logger_h_common).
-behaviour(gen_server).

-include("logger_h_common.hrl").
-include("logger_internal.hrl").

%% API
-export([filesync/2]).

%% logger_olp callbacks
-export([init/1, handle_load/2, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, notify/2, reset_state/1]).

%% logger callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3,
         filter_config/1]).

%% Library functions for handlers
-export([error_notify/1]).

-define(OLP_KEYS,[sync_mode_qlen,
                  drop_mode_qlen,
                  flush_qlen,
                  burst_limit_enable,
                  burst_limit_max_count,
                  burst_limit_window_time,
                  overload_kill_enable,
                  overload_kill_qlen,
                  overload_kill_mem_size,
                  overload_kill_restart_after]).

-define(COMMON_KEYS,[filesync_repeat_interval]).

-define(READ_ONLY_KEYS,[olp]).

%%%-----------------------------------------------------------------
%%% API

%% This function is called by the logger_sup supervisor
filesync(Module, Name) ->
    call(Module, Name, filesync).

%%%-----------------------------------------------------------------
%%% Handler being added
adding_handler(#{id:=Name,module:=Module}=Config) ->
    HConfig0 = maps:get(config, Config, #{}),
    HandlerConfig0 = maps:without(?OLP_KEYS++?COMMON_KEYS,HConfig0),
    case Module:check_config(Name,set,undefined,HandlerConfig0) of
        {ok,HandlerConfig} ->
            ModifiedCommon = maps:with(?COMMON_KEYS,HandlerConfig),
            CommonConfig0 = maps:with(?COMMON_KEYS,HConfig0),
            CommonConfig = maps:merge(
                             maps:merge(get_default_config(), CommonConfig0),
                             ModifiedCommon),
            case check_config(CommonConfig) of
                ok ->
                    HConfig = maps:merge(CommonConfig,HandlerConfig),
                    OlpOpts = maps:with(?OLP_KEYS,HConfig0),
                    start(OlpOpts, Config#{config => HConfig});
                {error,Faulty} ->
                    {error,{invalid_config,Module,Faulty}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Handler being removed
removing_handler(#{id:=Name, module:=Module, config:=#{olp:=Olp}}) ->
    case whereis(?name_to_reg_name(Module,Name)) of
        undefined ->
            ok;
        _Pid ->
            %% We don't want to do supervisor:terminate_child here
            %% since we need to distinguish this explicit stop from a
            %% system termination in order to avoid circular attempts
            %% at removing the handler (implying deadlocks and
            %% timeouts).
            %% And we don't need to do supervisor:delete_child, since
            %% the restart type is temporary, which means that the
            %% child specification is automatically removed from the
            %% supervisor when the process dies.
            _ = logger_olp:stop(Olp),
            ok
    end.

%%%-----------------------------------------------------------------
%%% Updating handler config
changing_config(SetOrUpdate,
                #{id:=Name,config:=OldHConfig,module:=Module},
                NewConfig0) ->
    NewHConfig0 = maps:get(config, NewConfig0, #{}),
    NoHandlerKeys = ?OLP_KEYS++?COMMON_KEYS++?READ_ONLY_KEYS,
    OldHandlerConfig = maps:without(NoHandlerKeys,OldHConfig),
    NewHandlerConfig0 = maps:without(NoHandlerKeys,NewHConfig0),
    case Module:check_config(Name, SetOrUpdate,
                             OldHandlerConfig,NewHandlerConfig0) of
        {ok, NewHandlerConfig} ->
            ModifiedCommon = maps:with(?COMMON_KEYS,NewHandlerConfig),
            NewCommonConfig0 = maps:with(?COMMON_KEYS,NewHConfig0),
            OldCommonConfig = maps:with(?COMMON_KEYS,OldHConfig),
            CommonDefault =
                case SetOrUpdate of
                    set ->
                        get_default_config();
                    update ->
                        OldCommonConfig
                end,
            NewCommonConfig = maps:merge(
                                maps:merge(CommonDefault,NewCommonConfig0),
                                ModifiedCommon),
            case check_config(NewCommonConfig) of
                ok ->
                    OlpDefault =
                        case SetOrUpdate of
                            set ->
                                logger_olp:get_default_opts();
                            update ->
                                maps:with(?OLP_KEYS,OldHConfig)
                        end,
                    Olp = maps:get(olp,OldHConfig),
                    NewOlpOpts = maps:merge(OlpDefault,
                                            maps:with(?OLP_KEYS,NewHConfig0)),
                    case logger_olp:set_opts(Olp,NewOlpOpts) of
                        ok ->
                            logger_olp:cast(Olp, {config_changed,
                                                  NewCommonConfig,
                                                  NewHandlerConfig}),
                            ReadOnly = maps:with(?READ_ONLY_KEYS,OldHConfig),
                            NewHConfig =
                                maps:merge(
                                  maps:merge(
                                    maps:merge(NewCommonConfig,NewHandlerConfig),
                                    ReadOnly),
                                  NewOlpOpts),
                            NewConfig = NewConfig0#{config=>NewHConfig},
                            {ok,NewConfig};
                        Error  ->
                            Error
                    end;
                {error,Faulty} ->
                    {error,{invalid_config,Module,Faulty}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------
%%% Log a string or report
-spec log(LogEvent, Config) -> ok when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config().

log(LogEvent, Config = #{config := #{olp:=Olp}}) ->
    %% if the handler has crashed, we must drop this event
    %% and hope the handler restarts so we can try again
    true = is_process_alive(logger_olp:get_pid(Olp)),
    Bin = log_to_binary(LogEvent, Config),
    logger_olp:load(Olp,Bin).

%%%-----------------------------------------------------------------
%%% Remove internal fields from configuration
filter_config(#{config:=HConfig}=Config) ->
    Config#{config=>maps:without(?READ_ONLY_KEYS,HConfig)}.

%%%-----------------------------------------------------------------
%%% Start the handler process
%%%
%%% The process must always exist if the handler is registered with
%%% logger (and must not exist if the handler is not registered).
%%%
%%% The handler process is linked to logger_sup, which is part of the
%%% kernel application's supervision tree.
start(OlpOpts0, #{id := Name, module:=Module, config:=HConfig} = Config0) ->
    RegName = ?name_to_reg_name(Module,Name),
    ChildSpec =
        #{id       => Name,
          start    => {logger_olp, start_link, [RegName,?MODULE,
                                                Config0, OlpOpts0]},
          restart  => temporary,
          shutdown => 2000,
          type     => worker,
          modules  => [?MODULE]},
    case supervisor:start_child(logger_sup, ChildSpec) of
        {ok,Pid,Olp} ->
            ok = logger_handler_watcher:register_handler(Name,Pid),
            OlpOpts = logger_olp:get_opts(Olp),
            {ok,Config0#{config=>(maps:merge(HConfig,OlpOpts))#{olp=>Olp}}};
        {error,{Reason,Ch}} when is_tuple(Ch), element(1,Ch)==child ->
            {error,Reason};
        Error ->
            Error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{id := Name, module := Module, config := HConfig}) ->
    process_flag(trap_exit, true),

    ?init_test_hooks(),

    case Module:init(Name, HConfig) of
        {ok,HState} ->
            %% Storing common config in state to avoid copying
            %% (sending) the config data for each log message
            CommonConfig = maps:with(?COMMON_KEYS,HConfig),
            State = CommonConfig#{id => Name,
                                  module => Module,
                                  ctrl_sync_count =>
                                      ?CONTROLLER_SYNC_INTERVAL,
                                  last_op => sync,
                                  handler_state => HState},
            State1 = set_repeated_filesync(State),
            {ok,State1};
        Error ->
            Error
    end.

%% This is the log event.
handle_load(Bin, #{id:=Name,
                   module:=Module,
                   handler_state:=HandlerState,
                   ctrl_sync_count := CtrlSync}=State) ->
    if CtrlSync==0 ->
            {_,HS1} = Module:write(Name, sync, Bin, HandlerState),
            State#{handler_state => HS1,
                   ctrl_sync_count => ?CONTROLLER_SYNC_INTERVAL,
                   last_op=>write};
       true ->
            {_,HS1} = Module:write(Name, async, Bin, HandlerState),
            State#{handler_state => HS1,
                   ctrl_sync_count => CtrlSync-1,
                   last_op=>write}
    end.

handle_call(filesync, _From, State = #{id := Name,
                                       module := Module,
                                       handler_state := HandlerState}) ->
    {Result,HandlerState1} = Module:filesync(Name,sync,HandlerState),
    {reply, Result, State#{handler_state=>HandlerState1, last_op=>sync}}.

%% If FILESYNC_REPEAT_INTERVAL is set to a millisec value, this
%% clause gets called repeatedly by the handler. In order to
%% guarantee that a filesync *always* happens after the last log
%% event, the repeat operation must be active!
handle_cast(repeated_filesync,State = #{filesync_repeat_interval := no_repeat}) ->
    %% This clause handles a race condition which may occur when
    %% config changes filesync_repeat_interval from an integer value
    %% to no_repeat.
    {noreply,State};
handle_cast(repeated_filesync,
            State = #{id := Name,
                      module := Module,
                      handler_state := HandlerState,
                      last_op := LastOp}) ->
    State1 =
        if LastOp == sync ->
                State;
           true ->
                {_,HS} = Module:filesync(Name, async, HandlerState),
                State#{handler_state => HS, last_op => sync}
        end,
    {noreply,set_repeated_filesync(State1)};
handle_cast({config_changed, CommonConfig, HConfig},
            State = #{id := Name,
                      module := Module,
                      handler_state := HandlerState,
                      filesync_repeat_interval := OldFSyncInt}) ->
    State1 =
        case maps:get(filesync_repeat_interval,CommonConfig) of
            OldFSyncInt ->
                State;
            FSyncInt ->
                set_repeated_filesync(
                  cancel_repeated_filesync(
                    State#{filesync_repeat_interval=>FSyncInt}))
        end,
    HS = try Module:config_changed(Name, HConfig, HandlerState)
         catch error:undef -> HandlerState
         end,
    {noreply, State1#{handler_state => HS}}.

handle_info(Info, #{id := Name, module := Module,
                    handler_state := HandlerState} = State) ->
    {noreply,State#{handler_state => Module:handle_info(Name,Info,HandlerState)}}.

terminate(overloaded=Reason, #{id:=Name}=State) ->
    _ = log_handler_info(Name,"Handler ~p overloaded and stopping",[Name],State),
    do_terminate(Reason,State),
    ConfigResult = logger:get_handler_config(Name),
    case ConfigResult of
        {ok,#{module:=Module}=HConfig0} ->
            spawn(fun() -> logger:remove_handler(Name) end),
            HConfig = try Module:filter_config(HConfig0)
                      catch _:_ -> HConfig0
                      end,
            {ok,fun() -> logger:add_handler(Name,Module,HConfig) end};
        Error ->
            error_notify({Name,restart_impossible,Error}),
            Error
    end;
terminate(Reason, State) ->
    do_terminate(Reason, State).

do_terminate(Reason, State = #{id := Name,
                               module := Module,
                               handler_state := HandlerState}) ->
    _ = cancel_repeated_filesync(State),
    _ = Module:terminate(Name, Reason, HandlerState),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reset_state(#{id:=Name, module:=Module, handler_state:=HandlerState} = State) ->
    State#{handler_state=>Module:reset_state(Name, HandlerState)}.

%%%-----------------------------------------------------------------
%%% Internal functions
call(Module, Name, Op) when is_atom(Name) ->
    case logger_olp:call(?name_to_reg_name(Module,Name), Op) of
        {error,busy} -> {error,handler_busy};
        Other -> Other
    end;
call(_, Name, Op) ->
    {error,{badarg,{Op,[Name]}}}.

notify({mode_change,Mode0,Mode1},#{id:=Name}=State) ->
    log_handler_info(Name,"Handler ~p switched from  ~p to ~p mode",
                     [Name,Mode0,Mode1], State);
notify({flushed,Flushed},#{id:=Name}=State) ->
    log_handler_info(Name, "Handler ~p flushed ~w log events",
                     [Name,Flushed], State);
notify(restart,#{id:=Name}=State) ->
    log_handler_info(Name, "Handler ~p restarted", [Name], State);
notify(idle,#{id:=Name,module:=Module,handler_state:=HandlerState}=State) ->
    {_,HS} = Module:filesync(Name,async,HandlerState),
    State#{handler_state=>HS, last_op=>sync}.

log_handler_info(Name, Format, Args, #{module:=Module,
                                       handler_state:=HandlerState}=State) ->
    Config =
        case logger:get_handler_config(Name) of
            {ok,Conf} -> Conf;
            _ -> #{formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}
        end,
    Meta = #{time=>logger:timestamp()},
    Bin = log_to_binary(#{level => notice,
                          msg => {Format,Args},
                          meta => Meta}, Config),
    {_,HS} = Module:write(Name, async, Bin, HandlerState),
    State#{handler_state=>HS, last_op=>write}.

%%%-----------------------------------------------------------------
%%% Convert log data on any form to binary
-spec log_to_binary(LogEvent,Config) -> LogString when
      LogEvent :: logger:log_event(),
      Config :: logger:handler_config(),
      LogString :: binary().
log_to_binary(#{msg:={report,_},meta:=#{report_cb:=_}}=Log,Config) ->
    do_log_to_binary(Log,Config);
log_to_binary(#{msg:={report,_},meta:=Meta}=Log,Config) ->
    DefaultReportCb = fun logger:format_otp_report/1,
    do_log_to_binary(Log#{meta=>Meta#{report_cb=>DefaultReportCb}},Config);
log_to_binary(Log,Config) ->
    do_log_to_binary(Log,Config).

do_log_to_binary(Log,Config) ->
    {Formatter,FormatterConfig} =
        maps:get(formatter,Config,{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    String = try_format(Log,Formatter,FormatterConfig),
    try string_to_binary(String)
    catch C2:R2:S2 ->
            ?LOG_INTERNAL(debug,[{formatter_error,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {bad_return_value,String},
                                 {catched,{C2,R2,S2}}]),
            <<"FORMATTER ERROR: bad return value">>
    end.

try_format(Log,Formatter,FormatterConfig) ->
    try Formatter:format(Log,FormatterConfig)
    catch
        C:R:S ->
            ?LOG_INTERNAL(debug,[{formatter_crashed,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {reason,
                                  {C,R,logger:filter_stacktrace(?MODULE,S)}}]),
            case {?DEFAULT_FORMATTER,#{}} of
                {Formatter,FormatterConfig} ->
                    "DEFAULT FORMATTER CRASHED";
                {DefaultFormatter,DefaultConfig} ->
                    try_format(Log#{msg=>{"FORMATTER CRASH: ~tp",
                                          [maps:get(msg,Log)]}},
                              DefaultFormatter,DefaultConfig)
            end
    end.

string_to_binary(String) ->
    case unicode:characters_to_binary(String) of
        Binary when is_binary(Binary) ->
            Binary;
        Error ->
            throw(Error)
    end.

%%%-----------------------------------------------------------------
%%% Check that the configuration term is valid
check_config(Config) when is_map(Config) ->
    check_common_config(maps:to_list(Config)).

check_common_config([{filesync_repeat_interval,NorA}|Config])
  when is_integer(NorA); NorA == no_repeat ->
    check_common_config(Config);
check_common_config([{Key,Value}|_]) ->
    {error,#{Key=>Value}};
check_common_config([]) ->
    ok.

get_default_config() ->
    #{filesync_repeat_interval => ?FILESYNC_REPEAT_INTERVAL}.

set_repeated_filesync(#{filesync_repeat_interval:=FSyncInt} = State)
  when is_integer(FSyncInt) ->
    {ok,TRef} = timer:apply_after(FSyncInt, gen_server, cast,
                                  [self(),repeated_filesync]),
    State#{rep_sync_tref=>TRef};
set_repeated_filesync(State) ->
    State.

cancel_repeated_filesync(State) ->
    case maps:take(rep_sync_tref,State) of
        {TRef,State1} ->
            _ = timer:cancel(TRef),
            State1;
        error ->
            State
    end.
error_notify(Term) ->
    ?internal_log(error, Term).
