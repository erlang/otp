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
-module(logger_server).

-behaviour(gen_server).

%% API
-export([start_link/0, add_handler/3, remove_handler/1,
         add_filter/2, remove_filter/2,
         set_module_level/2, unset_module_level/0,
         unset_module_level/1, cache_module_level/1,
         set_config/2, set_config/3,
         update_config/2, update_config/3,
         update_formatter_config/2]).

%% Helper
-export([diff_maps/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2]).

-include("logger_internal.hrl").

-define(SERVER, logger).
-define(LOGGER_SERVER_TAG, '$logger_cb_process').

-record(state, {tid, async_req, async_req_queue, remote_logger}).

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
                    Default = default_config(Id,Module),
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

set_module_level(Modules,Level) when is_list(Modules) ->
    case lists:all(fun(M) -> is_atom(M) end,Modules) of
        true ->
            case sanity_check(primary,level,Level) of
                ok -> call({set_module_level,Modules,Level});
                Error -> Error
            end;
        false ->
            {error,{not_a_list_of_modules,Modules}}
    end;
set_module_level(Modules,_) ->
    {error,{not_a_list_of_modules,Modules}}.

unset_module_level() ->
    call({unset_module_level,all}).

unset_module_level(Modules) when is_list(Modules) ->
    case lists:all(fun(M) -> is_atom(M) end,Modules) of
        true ->
            call({unset_module_level,Modules});
        false ->
            {error,{not_a_list_of_modules,Modules}}
    end;
unset_module_level(Modules) ->
    {error,{not_a_list_of_modules,Modules}}.

cache_module_level(Module) ->
    gen_server:cast(?SERVER,{cache_module_level,Module}).

set_config(Owner,Key,Value) ->
    case sanity_check(Owner,Key,Value) of
        ok ->
            call({change_config,set,Owner,Key,Value});
        Error ->
            Error
    end.

set_config(Owner,Config) ->
    case sanity_check(Owner,Config) of
        ok ->
            call({change_config,set,Owner,Config});
        Error ->
            Error
    end.

update_config(Owner,Key,Value) ->
    case sanity_check(Owner,Key,Value) of
        ok ->
            call({change_config,update,Owner,Key,Value});
        Error ->
            Error
    end.

update_config(Owner, Config) ->
    case sanity_check(Owner,Config) of
        ok ->
            call({change_config,update,Owner,Config});
        Error ->
            Error
    end.

update_formatter_config(HandlerId, FormatterConfig)
  when is_map(FormatterConfig) ->
    call({update_formatter_config,HandlerId,FormatterConfig});
update_formatter_config(_HandlerId, FormatterConfig) ->
    {error,{invalid_formatter_config,FormatterConfig}}.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    put(?LOGGER_SERVER_TAG,true),
    Tid = logger_config:new(?LOGGER_TABLE),
    %% Store initial proxy config. logger_proxy reads config from here at startup.
    logger_config:create(Tid,proxy,logger_proxy:get_default_config()),
    PrimaryConfig = maps:merge(default_config(primary),
                              #{handlers=>[simple]}),
    logger_config:create(Tid,primary,PrimaryConfig),
    SimpleConfig0 = maps:merge(default_config(simple,logger_simple_h),
                               #{filter_default=>stop,
                                 filters=>?DEFAULT_HANDLER_FILTERS}),
    %% If this fails, then the node should crash
    {ok,SimpleConfig} = logger_simple_h:adding_handler(SimpleConfig0),
    logger_config:create(Tid,simple,SimpleConfig),
    {ok, #state{tid=Tid, async_req_queue = queue:new()}}.

handle_call({add_handler,Id,Module,HConfig}, From, #state{tid=Tid}=State) ->
    case logger_config:exist(Tid,Id) of
        true ->
            {reply,{error,{already_exist,Id}},State};
        false ->
            call_h_async(
              fun() ->
                      %% inform the handler
                      call_h(Module,adding_handler,[HConfig],{ok,HConfig})
              end,
              fun({ok,HConfig1}) ->
                      %% We know that the call_h would have loaded the module
                      %% if it existed, so it is safe here to call function_exported
                      %% to find out if this is a valid handler
                      case erlang:function_exported(Module, log, 2) of
                          true ->
                              logger_config:create(Tid,Id,HConfig1),
                              {ok,Config} = logger_config:get(Tid,primary),
                              Handlers = maps:get(handlers,Config,[]),
                              logger_config:set(Tid,primary,
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
        {ok,#{module:=Module}=HConfig} ->
            {ok,Config} = logger_config:get(Tid,primary),
            Handlers0 = maps:get(handlers,Config,[]),
            Handlers = lists:delete(HandlerId,Handlers0),
            call_h_async(
              fun() ->
                      %% inform the handler
                      call_h(Module,removing_handler,[HConfig],ok)
              end,
              fun(_Res) ->
                      logger_config:set(Tid,primary,Config#{handlers=>Handlers}),
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
handle_call({change_config,SetOrUpd,proxy,Config0},_From,#state{tid=Tid}=State) ->
    Default =
        case SetOrUpd of
            set ->
                logger_proxy:get_default_config();
            update ->
                {ok,OldConfig} = logger_config:get(Tid,proxy),
                OldConfig
        end,
    Config = maps:merge(Default,Config0),
    Reply =
        case logger_olp:set_opts(logger_proxy,Config) of
            ok ->
                logger_config:set(Tid,proxy,Config);
            Error ->
                Error
        end,
    {reply,Reply,State};
handle_call({change_config,SetOrUpd,primary,Config0}, _From,
            #state{tid=Tid}=State) ->
    {ok,#{handlers:=Handlers}=OldConfig} = logger_config:get(Tid,primary),
    Default =
        case SetOrUpd of
            set -> default_config(primary);
            update -> OldConfig
        end,
    Config = maps:merge(Default,Config0),
    Reply = logger_config:set(Tid,primary,Config#{handlers=>Handlers}),
    {reply,Reply,State};
handle_call({change_config,_SetOrUpd,primary,Key,Value}, _From,
            #state{tid=Tid}=State) ->
    {ok,OldConfig} = logger_config:get(Tid,primary),
    Reply = logger_config:set(Tid,primary,OldConfig#{Key=>Value}),
    {reply,Reply,State};
handle_call({change_config,SetOrUpd,HandlerId,Config0}, From,
            #state{tid=Tid}=State) ->
    case logger_config:get(Tid,HandlerId) of
        {ok,#{module:=Module}=OldConfig} ->
            Default =
                case SetOrUpd of
                    set -> default_config(HandlerId,Module);
                    update -> OldConfig
                end,
            Config = maps:merge(Default,Config0),
            case check_config_change(OldConfig,Config) of
                ok ->
                    call_h_async(
                      fun() ->
                              call_h(Module,changing_config,
                                     [SetOrUpd,OldConfig,Config],
                                     {ok,Config})
                      end,
                      fun({ok,Config1}) ->
                              logger_config:set(Tid,HandlerId,Config1);
                         (Error) ->
                              Error
                      end,From,State);
                Error ->
                    {reply,Error,State}
            end;
        _ ->
            {reply,{error,{not_found,HandlerId}},State}
    end;
handle_call({change_config,SetOrUpd,HandlerId,Key,Value}, From,
            #state{tid=Tid}=State) ->
    case logger_config:get(Tid,HandlerId) of
        {ok,#{module:=Module}=OldConfig} ->
            Config = OldConfig#{Key=>Value},
            case check_config_change(OldConfig,Config) of
                ok ->
                    call_h_async(
                      fun() ->
                              call_h(Module,changing_config,
                                     [SetOrUpd,OldConfig,Config],
                                     {ok,Config})
                      end,
                      fun({ok,Config1}) ->
                              logger_config:set(Tid,HandlerId,Config1);
                         (Error) ->
                              Error
                      end,From,State);
                Error ->
                    {reply,Error,State}
            end;
        _ ->
            {reply,{error,{not_found,HandlerId}},State}
    end;
handle_call({update_formatter_config,HandlerId,NewFConfig},_From,
            #state{tid=Tid}=State) ->
    Reply =
        case logger_config:get(Tid,HandlerId) of
            {ok,#{formatter:={FMod,OldFConfig}}=Config} ->
                try
                    FConfig = maps:merge(OldFConfig,NewFConfig),
                    check_formatter({FMod,FConfig}),
                    logger_config:set(Tid,HandlerId,
                                      Config#{formatter=>{FMod,FConfig}})
                catch throw:Reason -> {error,Reason}
                end;
            _ ->
            {error,{not_found,HandlerId}}
        end,
    {reply,Reply,State};
handle_call({set_module_level,Modules,Level}, _From, #state{tid=Tid}=State) ->
    Reply = logger_config:set_module_level(Tid,Modules,Level),
    {reply,Reply,State};
handle_call({unset_module_level,Modules}, _From, #state{tid=Tid}=State) ->
    Reply = logger_config:unset_module_level(Tid,Modules),
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
    %% The simple handler will send an 'EXIT' message when it is replaced
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
call(Request) when is_tuple(Request) ->
    Action = element(1,Request),
    case get(?LOGGER_SERVER_TAG) of
        true when
              Action == add_handler; Action == remove_handler;
              Action == add_filter; Action == remove_filter;
              Action == change_config ->
            {error,{attempting_syncronous_call_to_self,Request}};
        _ ->
            gen_server:call(?SERVER,Request,?DEFAULT_LOGGER_CALL_TIMEOUT)
    end.


do_add_filter(Tid,Id,{FId,_} = Filter) ->
    case logger_config:get(Tid,Id) of
        {ok,Config} ->
            Filters = maps:get(filters,Config,[]),
            case lists:keymember(FId,1,Filters) of
                true ->
                    {error,{already_exist,FId}};
                false ->
                    logger_config:set(Tid,Id,Config#{filters=>[Filter|Filters]})
            end;
        Error ->
            Error
    end.

do_remove_filter(Tid,Id,FilterId) ->
    case logger_config:get(Tid,Id) of
        {ok,Config} ->
            Filters0 = maps:get(filters,Config,[]),
            case lists:keytake(FilterId,1,Filters0) of
                {value,_,Filters} ->
                    logger_config:set(Tid,Id,Config#{filters=>Filters});
                false ->
                    {error,{not_found,FilterId}}
            end;
        Error ->
            Error
    end.

default_config(primary) ->
    #{level=>notice,
      filters=>[],
      filter_default=>log};
default_config(Id) ->
    #{id=>Id,
      level=>all,
      filters=>[],
      filter_default=>log,
      formatter=>{?DEFAULT_FORMATTER,#{}}}.
default_config(Id,Module) ->
    (default_config(Id))#{module=>Module}.

sanity_check(Owner,Key,Value) ->
    sanity_check_1(Owner,[{Key,Value}]).

sanity_check(Owner,Config) when is_map(Config) ->
    sanity_check_1(Owner,maps:to_list(Config));
sanity_check(_,Config) ->
    {error,{invalid_config,Config}}.

sanity_check_1(proxy,_Config) ->
    ok; % Details are checked by logger_olp:set_opts/2
sanity_check_1(Owner,Config) when is_list(Config) ->
    try
        Type = get_type(Owner),
        check_config(Type,Config)
    catch throw:Error -> {error,Error}
    end.

get_type(primary) ->
    primary;
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
check_config(primary,[C|_]) ->
    throw({invalid_primary_config,C});
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

%% When changing configuration for a handler, the id and module fields
%% can not be changed.
check_config_change(#{id:=Id,module:=Module},#{id:=Id,module:=Module}) ->
    ok;
check_config_change(OldConfig,NewConfig) ->
    {Old,New} = logger_server:diff_maps(maps:with([id,module],OldConfig),
                                        maps:with([id,module],NewConfig)),
    {error,{illegal_config_change,Old,New}}.

call_h(Module, Function, Args, DefRet) ->
    %% Not calling code:ensure_loaded + erlang:function_exported here,
    %% since in some rare terminal cases, the code_server might not
    %% exist and we'll get a deadlock in removing the handler.
    try apply(Module, Function, Args)
    catch
        C:R:S ->
            case {C,R,S} of
                {error,undef,[{Module,Function=changing_config,Args,_}|_]}
                  when length(Args)=:=3 ->
                    %% Backwards compatible call, if changing_config/3
                    %% did not exist.
                    call_h(Module, Function, tl(Args), DefRet);
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

%% Return two maps containing only the fields that differ.
diff_maps(M1,M2) ->
    diffs(lists:sort(maps:to_list(M1)),lists:sort(maps:to_list(M2)),#{},#{}).

diffs([H|T1],[H|T2],D1,D2) ->
    diffs(T1,T2,D1,D2);
diffs([{K,V1}|T1],[{K,V2}|T2],D1,D2) ->
    diffs(T1,T2,D1#{K=>V1},D2#{K=>V2});
diffs([],[],D1,D2) ->
    {D1,D2}.
