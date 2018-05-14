%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(logger_server).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_handler/3, remove_handler/1,
         add_filter/2, remove_filter/2,
         set_module_level/2, reset_module_level/1,
         cache_module_level/1,
         set_config/2, set_config/3, update_config/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include("logger_internal.hrl").

-define(SERVER, logger).
-define(LOGGER_SERVER_TAG, '$logger_cb_process').

-record(state, {tid, async_req, async_req_queue}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_handler(Id,Module,Config0) ->
    try {check_id(Id),check_mod(Module)} of
        {ok,ok} ->
            case sanity_check(Id,Config0) of
                ok ->
                    Default = default_config(Id),
                    Config = maps:merge(Default,Config0),
                    call({add_handler,Id,Module,Config});
                Error ->
                    Error
            end
    catch throw:Error ->
            {error,Error}
    end.

remove_handler(HandlerId) ->
    call({remove_handler,HandlerId}).

add_filter(Owner,Filter) ->
    case sanity_check(Owner,filters,[Filter]) of
        ok -> call({add_filter,Owner,Filter});
        Error -> Error
    end.

remove_filter(Owner,FilterId) ->
    call({remove_filter,Owner,FilterId}).

set_module_level(Module,Level) when is_atom(Module) ->
    case sanity_check(logger,level,Level) of
        ok -> call({set_module_level,Module,Level});
        Error -> Error
    end;
set_module_level(Module,_) ->
    {error,{not_a_module,Module}}.

reset_module_level(Module) when is_atom(Module) ->
    call({reset_module_level,Module});
reset_module_level(Module) ->
    {error,{not_a_module,Module}}.

cache_module_level(Module) ->
    gen_server:cast(?SERVER,{cache_module_level,Module}).


set_config(Owner,Key,Value) ->
    update_config(Owner,#{Key=>Value}).

set_config(Owner,Config0) ->
    case sanity_check(Owner,Config0) of
        ok ->
            Config = maps:merge(default_config(Owner),Config0),
            call({set_config,Owner,Config});
        Error ->
            Error
    end.

update_config(Owner, Config) ->
    case sanity_check(Owner,Config) of
        ok ->
            call({update_config,Owner,Config});
        Error ->
            Error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    Tid = logger_config:new(?LOGGER_TABLE),
    LoggerConfig = maps:merge(default_config(logger),
                              #{handlers=>[logger_simple]}),
    logger_config:create(Tid,logger,LoggerConfig),
    SimpleConfig0 = maps:merge(default_config(logger_simple),
                               #{filter_default=>stop,
                                 filters=>?DEFAULT_HANDLER_FILTERS}),
    %% If this fails, then the node should crash
    {ok,SimpleConfig} =
        logger_simple:adding_handler(logger_simple,SimpleConfig0),
    logger_config:create(Tid,logger_simple,logger_simple,SimpleConfig),
    {ok, #state{tid=Tid, async_req_queue = queue:new()}}.

handle_call({add_handler,Id,Module,HConfig}, From, #state{tid=Tid}=State) ->
    case logger_config:exist(Tid,Id) of
        true ->
            {reply,{error,{already_exist,Id}},State};
        false ->
            call_h_async(
              fun() ->
                      %% inform the handler
                      call_h(Module,adding_handler,[Id,HConfig],{ok,HConfig})
              end,
              fun({ok,HConfig1}) ->
                      %% We know that the call_h would have loaded the module
                      %% if it existed, so it is safe here to call function_exported
                      %% to find out if this is a valid handler
                      case erlang:function_exported(Module, log, 2) of
                          true ->
                              logger_config:create(Tid,Id,Module,HConfig1),
                              {ok,Config} = do_get_config(Tid,logger),
                              Handlers = maps:get(handlers,Config,[]),
                              do_set_config(Tid,logger,
                                            Config#{handlers=>[Id|Handlers]});
                          false ->
                              {error,{invalid_handler,
                                      {function_not_exported,
                                       {Module,log,2}}}}
                      end;
                 ({error,HReason}) ->
                      {error,{handler_not_added,HReason}}
              end,From,State)
    end;
handle_call({remove_handler,HandlerId}, From, #state{tid=Tid}=State) ->
    case logger_config:get(Tid,HandlerId) of
        {ok,{Module,HConfig}} ->
            {ok,Config} = do_get_config(Tid,logger),
            Handlers0 = maps:get(handlers,Config,[]),
            Handlers = lists:delete(HandlerId,Handlers0),
            call_h_async(
              fun() ->
                      %% inform the handler
                      call_h(Module,removing_handler,[HandlerId,HConfig],ok)
              end,
              fun(_Res) ->
                      do_set_config(Tid,logger,Config#{handlers=>Handlers}),
                      logger_config:delete(Tid,HandlerId),
                      ok
              end,From,State);
        _ ->
            {reply,{error,{not_found,HandlerId}},State}
    end;
handle_call({add_filter,Id,Filter}, _From,#state{tid=Tid}=State) ->
    Reply = do_add_filter(Tid,Id,Filter),
    {reply,Reply,State};
handle_call({remove_filter,Id,FilterId}, _From, #state{tid=Tid}=State) ->
    Reply = do_remove_filter(Tid,Id,FilterId),
    {reply,Reply,State};
handle_call({update_config,Id,NewConfig}, From, #state{tid=Tid}=State) ->
    case logger_config:get(Tid,Id) of
        {ok,{_Module,OldConfig}} ->
            Config = maps:merge(OldConfig,NewConfig),
            handle_call({set_config,Id,Config}, From, State);
        {ok,OldConfig} ->
            Config = maps:merge(OldConfig,NewConfig),
            {reply,do_set_config(Tid,Id,Config),State};
        Error ->
            {reply,Error,State}
    end;
handle_call({set_config,logger,Config}, _From, #state{tid=Tid}=State) ->
    {ok,#{handlers:=Handlers}} = logger_config:get(Tid,logger),
    Reply = do_set_config(Tid,logger,Config#{handlers=>Handlers}),
    {reply,Reply,State};
handle_call({set_config,HandlerId,Config}, From, #state{tid=Tid}=State) ->
    case logger_config:get(Tid,HandlerId) of
        {ok,{Module,OldConfig}} ->
            call_h_async(
              fun() ->
                      call_h(Module,changing_config,[HandlerId,OldConfig,Config],
                             {ok,Config})
              end,
              fun({ok,Config1}) ->
                      do_set_config(Tid,HandlerId,Config1);
                 (Error) ->
                      Error
              end,From,State);
        _ ->
            {reply,{error,{not_found,HandlerId}},State}
    end;
handle_call({set_module_level,Module,Level}, _From, #state{tid=Tid}=State) ->
    Reply = logger_config:set_module_level(Tid,Module,Level),
    {reply,Reply,State};
handle_call({reset_module_level,Module}, _From, #state{tid=Tid}=State) ->
    Reply = logger_config:reset_module_level(Tid,Module),
    {reply,Reply,State}.

handle_cast({async_req_reply,_Ref,_Reply} = Reply,State) ->
    call_h_reply(Reply,State);
handle_cast({cache_module_level,Module}, #state{tid=Tid}=State) ->
    logger_config:cache_module_level(Tid,Module),
    {noreply, State}.

%% Interface for those who can't call the API - e.g. the emulator, or
%% places related to code loading.
%%
%% This can also be log events from remote nodes which are sent from
%% logger.erl when the group leader of the client process is on a
%% same node as the client process itself.
handle_info({log,Level,Format,Args,Meta}, State) ->
    logger:log(Level,Format,Args,Meta),
    {noreply, State};
handle_info({log,Level,Report,Meta}, State) ->
    logger:log(Level,Report,Meta),
    {noreply, State};
handle_info({Ref,_Reply},State) when is_reference(Ref) ->
    %% Assuming this is a timed-out gen_server reply - ignoring
    {noreply, State};
handle_info({'DOWN',_Ref,_Proc,_Pid,_Reason} = Down,State) ->
    call_h_reply(Down,State);
handle_info(Unexpected,State) when element(1,Unexpected) == 'EXIT' ->
    %% The simple logger will send an 'EXIT' message when it is replaced
    %% We may as well ignore all 'EXIT' messages that we get
    ?LOG_INTERNAL(debug,
                  [{logger,got_unexpected_message},
                   {process,?SERVER},
                   {message,Unexpected}]),
    {noreply,State};
handle_info(Unexpected,State) ->
    ?LOG_INTERNAL(info,
                  [{logger,got_unexpected_message},
                   {process,?SERVER},
                   {message,Unexpected}]),
    {noreply,State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call(Request) ->
    Action = element(1,Request),
    case get(?LOGGER_SERVER_TAG) of
        true when
              Action == add_handler; Action == remove_handler;
              Action == update_config; Action == set_config ->
            {error,{attempting_syncronous_call_to_self,Request}};
        _ ->
            gen_server:call(?SERVER,Request,?DEFAULT_LOGGER_CALL_TIMEOUT)
    end.

do_add_filter(Tid,Id,{FId,_} = Filter) ->
    case do_get_config(Tid,Id) of
        {ok,Config} ->
            Filters = maps:get(filters,Config,[]),
            case lists:keymember(FId,1,Filters) of
                true ->
                    {error,{already_exist,FId}};
                false ->
                    do_set_config(Tid,Id,Config#{filters=>[Filter|Filters]})
            end;
        Error ->
            Error
    end.

do_remove_filter(Tid,Id,FilterId) ->
    case do_get_config(Tid,Id) of
        {ok,Config} ->
            Filters0 = maps:get(filters,Config,[]),
            case lists:keytake(FilterId,1,Filters0) of
                {value,_,Filters} ->
                    do_set_config(Tid,Id,Config#{filters=>Filters});
                false ->
                    {error,{not_found,FilterId}}
            end;
        Error ->
            Error
    end.

do_get_config(Tid,Id) ->
    case logger_config:get(Tid,Id) of
        {ok,{_,Config}} ->
            {ok,Config};
        {ok,Config} ->
            {ok,Config};
        Error ->
            Error
    end.

do_set_config(Tid,Id,Config) ->
    logger_config:set(Tid,Id,Config),
    ok.

default_config(logger) ->
    #{level=>info,
      filters=>[],
      filter_default=>log};
default_config(_) ->
    #{level=>info,
      filters=>[],
      filter_default=>log,
      formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}}.

sanity_check(Owner,Key,Value) ->
    sanity_check_1(Owner,[{Key,Value}]).

sanity_check(HandlerId,Config) when is_map(Config) ->
    sanity_check_1(HandlerId,maps:to_list(Config));
sanity_check(_,Config) ->
    {error,{invalid_handler_config,Config}}.

sanity_check_1(Owner,Config) when is_list(Config) ->
    try
        Type = get_type(Owner),
        check_config(Type,Config)
    catch throw:Error -> {error,Error}
    end.

get_type(logger) ->
    logger;
get_type(Id) ->
    check_id(Id),
    handler.

check_config(Owner,[{level,Level}|Config]) ->
    check_level(Level),
    check_config(Owner,Config);
check_config(Owner,[{filters,Filters}|Config]) ->
    check_filters(Filters),
    check_config(Owner,Config);
check_config(Owner,[{filter_default,FD}|Config]) ->
    check_filter_default(FD),
    check_config(Owner,Config);
check_config(handler,[{formatter,Formatter}|Config]) ->
    check_formatter(Formatter),
    check_config(handler,Config);
check_config(logger,[C|_]) ->
    throw({invalid_logger_config,C});
check_config(handler,[{_,_}|Config]) ->
    %% Arbitrary config elements are allowed for handlers
    check_config(handler,Config);
check_config(_,[]) ->
    ok.

check_id(Id) when is_atom(Id) ->
    ok;
check_id(Id) ->
    throw({invalid_id,Id}).

check_mod(Mod) when is_atom(Mod) ->
    ok;
check_mod(Mod) ->
    throw({invalid_module,Mod}).

check_level({LevelInt,cached}) when LevelInt>=?EMERGENCY, LevelInt=<?DEBUG ->
    ok;
check_level(Level) ->
    case lists:member(Level,?LEVELS) of
        true ->
            ok;
        false ->
            throw({invalid_level,Level})
    end.

check_filters([{Id,{Fun,_Args}}|Filters]) when is_atom(Id), is_function(Fun,2) ->
    check_filters(Filters);
check_filters([Filter|_]) ->
    throw({invalid_filter,Filter});
check_filters([]) ->
    ok;
check_filters(Filters) ->
    throw({invalid_filters,Filters}).

check_filter_default(FD) when FD==stop; FD==log ->
    ok;
check_filter_default(FD) ->
    throw({invalid_filter_default,FD}).

check_formatter({Mod,Config}) ->
    check_mod(Mod),
    try Mod:check_config(Config) of
        ok -> ok;
        {error,Error} -> throw(Error)
    catch
        C:R:S ->
            case {C,R,S} of
                {error,undef,[{Mod,check_config,[Config],_}|_]} ->
                    ok;
                _ ->
                    throw({callback_crashed,
                           {C,R,logger:filter_stacktrace(?MODULE,S)}})
            end
    end;
check_formatter(Formatter) ->
    throw({invalid_formatter,Formatter}).

call_h(Module, Function, Args, DefRet) ->
    %% Not calling code:ensure_loaded + erlang:function_exported here,
    %% since in some rare terminal cases, the code_server might not
    %% exist and we'll get a deadlock in removing the handler.
    try apply(Module, Function, Args)
    catch
        C:R:S ->
            case {C,R,S} of
                {error,undef,[{Module,Function,Args,_}|_]} ->
                    DefRet;
                _ ->
                    ST = logger:filter_stacktrace(?MODULE,S),
                    ?LOG_INTERNAL(error,
                                  [{logger,callback_crashed},
                                   {process,?SERVER},
                                   {reason,{C,R,ST}}]),
                    {error,{callback_crashed,{C,R,ST}}}
            end
    end.

%% There are all sort of API functions that can cause deadlocks if called
%% from the handler callbacks. So we spawn a process that does the request
%% for the logger_server. There are still APIs that will cause problems,
%% namely logger:add_handler
call_h_async(AsyncFun,PostFun,From,#state{ async_req = undefined } = State) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(
                    fun() ->
                            put(?LOGGER_SERVER_TAG,true),
                            receive Ref -> Ref end,
                            gen_server:cast(Parent, {async_req_reply, Ref, AsyncFun()})
                    end),
    Pid ! Ref,
    {noreply,State#state{ async_req = {Ref,PostFun,From} }};
call_h_async(AsyncFun,PostFun,From,#state{ async_req_queue = Q } = State) ->
    {noreply,State#state{ async_req_queue = queue:in({AsyncFun,PostFun,From},Q) }}.

call_h_reply({async_req_reply,Ref,Reply},
              #state{ async_req = {Ref,PostFun,From}, async_req_queue = Q} = State) ->
    erlang:demonitor(Ref,[flush]),
    _ = gen_server:reply(From, PostFun(Reply)),
    {Value,NewQ} = queue:out(Q),
    NewState = State#state{ async_req = undefined,
                            async_req_queue = NewQ },
    case Value of
        {value,{AsyncFun,NPostFun,NFrom}} ->
            call_h_async(AsyncFun,NPostFun,NFrom,NewState);
        empty ->
            {noreply,NewState}
    end;
call_h_reply({'DOWN',Ref,_Proc,Pid,Reason}, #state{ async_req = {Ref,_PostFun,_From}} = State) ->
    %% This clause should only be triggered if someone explicitly sends an exit signal
    %% to the spawned process. It is only here to make sure that the logger_server does
    %% not deadlock if that happens.
    ?LOG_INTERNAL(error,
                  [{logger,process_exited},
                   {process,Pid},
                   {reason,Reason}]),
    call_h_reply(
      {async_req_reply,Ref,{error,{logger_process_exited,Pid,Reason}}},
      State);
call_h_reply(Unexpected,State) ->
    ?LOG_INTERNAL(info,
                  [{logger,got_unexpected_message},
                   {process,?SERVER},
                   {message,Unexpected}]),
    {noreply,State}.
