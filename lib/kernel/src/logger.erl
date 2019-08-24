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
-module(logger).

%% Log interface
-export([emergency/1,emergency/2,emergency/3,
         alert/1,alert/2,alert/3,
         critical/1,critical/2,critical/3,
         error/1,error/2,error/3,
         warning/1,warning/2,warning/3,
         notice/1,notice/2,notice/3,
         info/1,info/2,info/3,
         debug/1,debug/2,debug/3]).
-export([log/2,log/3,log/4]).

%% Called by macro
-export([allow/2,macro_log/3,macro_log/4,macro_log/5,add_default_metadata/1]).

%% Configuration
-export([add_handler/3, remove_handler/1,
         add_primary_filter/2, add_handler_filter/3,
         remove_primary_filter/1, remove_handler_filter/2,
         set_module_level/2,
         unset_module_level/1, unset_module_level/0,
         set_application_level/2, unset_application_level/1,
         get_module_level/0, get_module_level/1,
         set_primary_config/1, set_primary_config/2,
         set_handler_config/2, set_handler_config/3,
         set_proxy_config/1,
         update_primary_config/1,
         update_handler_config/2, update_handler_config/3,
         update_proxy_config/1,
         update_formatter_config/2, update_formatter_config/3,
         get_primary_config/0, get_handler_config/1,
         get_handler_config/0, get_handler_ids/0, get_config/0,
         get_proxy_config/0,
         add_handlers/1]).

%% Private configuration
-export([internal_init_logger/0]).

%% Misc
-export([compare_levels/2]).
-export([set_process_metadata/1, update_process_metadata/1,
         unset_process_metadata/0, get_process_metadata/0]).
-export([i/0, i/1]).
-export([timestamp/0]).

%% Basic report formatting
-export([format_report/1, format_otp_report/1]).

-export([internal_log/2,filter_stacktrace/2]).

-include("logger_internal.hrl").
-include("logger.hrl").

%%%-----------------------------------------------------------------
%%% Types
-type log_event() :: #{level:=level(),
                       msg:={io:format(),[term()]} |
                            {report,report()} |
                            {string,unicode:chardata()},
                       meta:=metadata()}.
-type level() :: emergency | alert | critical | error |
                 warning | notice | info | debug.
-type report() :: map() | [{atom(),term()}].
-type report_cb() :: fun((report()) -> {io:format(),[term()]}) |
                     fun((report(),report_cb_config()) -> unicode:chardata()).
-type report_cb_config() :: #{depth       := pos_integer() | unlimited,
                              chars_limit := pos_integer() | unlimited,
                              single_line := boolean()}.
-type msg_fun() :: fun((term()) -> {io:format(),[term()]} |
                                   report() |
                                   unicode:chardata()).
-type metadata() :: #{pid    => pid(),
                      gl     => pid(),
                      time   => timestamp(),
                      mfa    => {module(),atom(),non_neg_integer()},
                      file   => file:filename(),
                      line   => non_neg_integer(),
                      domain => [atom()],
                      report_cb => report_cb(),
                      atom() => term()}.
-type location() :: #{mfa  := {module(),atom(),non_neg_integer()},
                      file := file:filename(),
                      line := non_neg_integer()}.
-type handler_id() :: atom().
-type filter_id() :: atom().
-type filter() :: {fun((log_event(),filter_arg()) ->
                              filter_return()),filter_arg()}.
-type filter_arg() :: term().
-type filter_return() :: stop | ignore | log_event().
-type primary_config() :: #{level => level() | all | none,
                            filter_default => log | stop,
                            filters => [{filter_id(),filter()}]}.
-type handler_config() :: #{id => handler_id(),
                            config => term(),
                            level => level() | all | none,
                            module => module(),
                            filter_default => log | stop,
                            filters => [{filter_id(),filter()}],
                            formatter => {module(),formatter_config()}}.
-type timestamp() :: integer().
-type formatter_config() :: #{atom() => term()}.

-type config_handler() :: {handler, handler_id(), module(), handler_config()}.

-type config_logger() :: [{handler,default,undefined} |
                          config_handler() |
                          {filters,log | stop,[{filter_id(),filter()}]} |
                          {module_level,level(),[module()]}].

-type olp_config() :: #{sync_mode_qlen => non_neg_integer(),
                        drop_mode_qlen => pos_integer(),
                        flush_qlen => pos_integer(),
                        burst_limit_enable => boolean(),
                        burst_limit_max_count => pos_integer(),
                        burst_limit_window_time => pos_integer(),
                        overload_kill_enable => boolean(),
                        overload_kill_qlen => pos_integer(),
                        overload_kill_mem_size => pos_integer(),
                        overload_kill_restart_after =>
                            non_neg_integer() | infinity}.

-export_type([log_event/0,
              level/0,
              report/0,
              report_cb/0,
              report_cb_config/0,
              msg_fun/0,
              metadata/0,
              primary_config/0,
              handler_config/0,
              handler_id/0,
              filter_id/0,
              filter/0,
              filter_arg/0,
              filter_return/0,
              config_handler/0,
              formatter_config/0,
              olp_config/0,
              timestamp/0]).

%%%-----------------------------------------------------------------
%%% API
emergency(X) ->
    log(emergency,X).
emergency(X,Y) ->
    log(emergency,X,Y).
emergency(X,Y,Z) ->
    log(emergency,X,Y,Z).

alert(X) ->
    log(alert,X).
alert(X,Y) ->
    log(alert,X,Y).
alert(X,Y,Z) ->
    log(alert,X,Y,Z).

critical(X) ->
    log(critical,X).
critical(X,Y) ->
    log(critical,X,Y).
critical(X,Y,Z) ->
    log(critical,X,Y,Z).

error(X) ->
    log(error,X).
error(X,Y) ->
    log(error,X,Y).
error(X,Y,Z) ->
    log(error,X,Y,Z).

warning(X) ->
    log(warning,X).
warning(X,Y) ->
    log(warning,X,Y).
warning(X,Y,Z) ->
    log(warning,X,Y,Z).

notice(X) ->
    log(notice,X).
notice(X,Y) ->
    log(notice,X,Y).
notice(X,Y,Z) ->
    log(notice,X,Y,Z).

info(X) ->
    log(info,X).
info(X,Y) ->
    log(info,X,Y).
info(X,Y,Z) ->
    log(info,X,Y,Z).

debug(X) ->
    log(debug,X).
debug(X,Y) ->
    log(debug,X,Y).
debug(X,Y,Z) ->
    log(debug,X,Y,Z).

-spec log(Level,StringOrReport) -> ok when
      Level :: level(),
      StringOrReport :: unicode:chardata() | report().
log(Level, StringOrReport) ->
    do_log(Level,StringOrReport,#{}).

-spec log(Level,StringOrReport,Metadata) -> ok when
      Level :: level(),
      StringOrReport :: unicode:chardata() | report(),
      Metadata :: metadata();
         (Level,Format,Args) -> ok when
      Level :: level(),
      Format :: io:format(),
      Args ::[term()];
         (Level,Fun,FunArgs) -> ok when
      Level :: level(),
      Fun :: msg_fun(),
      FunArgs :: term().
log(Level, StringOrReport, Metadata)
  when is_map(Metadata), not is_function(StringOrReport) ->
    do_log(Level,StringOrReport,Metadata);
log(Level, FunOrFormat, Args) ->
    do_log(Level,{FunOrFormat,Args},#{}).

-spec log(Level,Format, Args, Metadata) -> ok when
      Level :: level(),
      Format :: io:format(),
      Args :: [term()],
      Metadata :: metadata();
         (Level,Fun,FunArgs,Metadata) -> ok when
      Level :: level(),
      Fun :: msg_fun(),
      FunArgs :: term(),
      Metadata :: metadata().
log(Level, FunOrFormat, Args, Metadata) ->
    do_log(Level,{FunOrFormat,Args},Metadata).

-spec allow(Level,Module) -> boolean() when
      Level :: level(),
      Module :: module().
allow(Level,Module) when ?IS_LEVEL(Level), is_atom(Module) ->
    logger_config:allow(?LOGGER_TABLE,Level,Module).


-spec macro_log(Location,Level,StringOrReport)  -> ok when
      Location :: location(),
      Level :: level(),
      StringOrReport :: unicode:chardata() | report().
macro_log(Location,Level,StringOrReport) ->
    log_allowed(Location,Level,StringOrReport,#{}).

-spec macro_log(Location,Level,StringOrReport,Meta)  -> ok when
      Location :: location(),
      Level :: level(),
      StringOrReport :: unicode:chardata() | report(),
      Meta :: metadata();
               (Location,Level,Format,Args) -> ok when
      Location :: location(),
      Level :: level(),
      Format :: io:format(),
      Args ::[term()];
               (Location,Level,Fun,FunArgs) -> ok when
      Location :: location(),
      Level :: level(),
      Fun :: msg_fun(),
      FunArgs :: term().
macro_log(Location,Level,StringOrReport,Meta)
  when is_map(Meta), not is_function(StringOrReport) ->
    log_allowed(Location,Level,StringOrReport,Meta);
macro_log(Location,Level,FunOrFormat,Args) ->
    log_allowed(Location,Level,{FunOrFormat,Args},#{}).

-spec macro_log(Location,Level,Format,Args,Meta)  -> ok when
      Location :: location(),
      Level :: level(),
      Format :: io:format(),
      Args ::[term()],
      Meta :: metadata();
               (Location,Level,Fun,FunArgs,Meta) -> ok when
      Location :: location(),
      Level :: level(),
      Fun :: msg_fun(),
      FunArgs :: term(),
      Meta :: metadata().
macro_log(Location,Level,FunOrFormat,Args,Meta) ->
    log_allowed(Location,Level,{FunOrFormat,Args},Meta).

-spec format_otp_report(Report) -> FormatArgs when
      Report :: report(),
      FormatArgs :: {io:format(),[term()]}.
format_otp_report(#{label:=_,report:=Report}) ->
    format_report(Report);
format_otp_report(Report) ->
    format_report(Report).

-spec format_report(Report) -> FormatArgs when
      Report :: report(),
      FormatArgs :: {io:format(),[term()]}.
format_report(Report) when is_map(Report) ->
    format_report(maps:to_list(Report));
format_report(Report)  when is_list(Report) ->
    case lists:flatten(Report) of
        [] ->
            {"~tp",[[]]};
        FlatList ->
            case string_p1(FlatList) of
                true ->
                    {"~ts",[FlatList]};
                false ->
                    format_term_list(Report,[],[])
            end
    end;
format_report(Report) ->
    {"~tp",[Report]}.

format_term_list([{Tag,Data}|T],Format,Args) ->
    PorS = case string_p(Data) of
               true -> "s";
               false -> "p"
           end,
    format_term_list(T,["    ~tp: ~t"++PorS|Format],[Data,Tag|Args]);
format_term_list([Data|T],Format,Args) ->
    format_term_list(T,["    ~tp"|Format],[Data|Args]);
format_term_list([],Format,Args) ->
    {lists:flatten(lists:join($\n,lists:reverse(Format))),lists:reverse(Args)}.

string_p(List) when is_list(List) ->
    string_p1(lists:flatten(List));
string_p(_) ->
    false.

string_p1([]) ->
    false;
string_p1(FlatList) ->
    io_lib:printable_unicode_list(FlatList).

internal_log(Level,Term) when is_atom(Level) ->
    erlang:display_string("Logger - "++ atom_to_list(Level) ++ ": "),
    erlang:display(Term).

-spec timestamp() -> timestamp().
timestamp() ->
    os:system_time(microsecond).

%%%-----------------------------------------------------------------
%%% Configuration
-spec add_primary_filter(FilterId,Filter) -> ok | {error,term()} when
      FilterId :: filter_id(),
      Filter :: filter().
add_primary_filter(FilterId,Filter) ->
    logger_server:add_filter(primary,{FilterId,Filter}).

-spec add_handler_filter(HandlerId,FilterId,Filter) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      FilterId :: filter_id(),
      Filter :: filter().
add_handler_filter(HandlerId,FilterId,Filter) ->
    logger_server:add_filter(HandlerId,{FilterId,Filter}).


-spec remove_primary_filter(FilterId) -> ok | {error,term()} when
      FilterId :: filter_id().
remove_primary_filter(FilterId) ->
    logger_server:remove_filter(primary,FilterId).

-spec remove_handler_filter(HandlerId,FilterId) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      FilterId :: filter_id().
remove_handler_filter(HandlerId,FilterId) ->
    logger_server:remove_filter(HandlerId,FilterId).

-spec add_handler(HandlerId,Module,Config) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Module :: module(),
      Config :: handler_config().
add_handler(HandlerId,Module,Config) ->
    logger_server:add_handler(HandlerId,Module,Config).

-spec remove_handler(HandlerId) -> ok | {error,term()} when
      HandlerId :: handler_id().
remove_handler(HandlerId) ->
    logger_server:remove_handler(HandlerId).

-spec set_primary_config(level,Level) -> ok | {error,term()} when
      Level :: level() | all | none;
                        (filter_default,FilterDefault) -> ok | {error,term()} when
      FilterDefault :: log | stop;
                        (filters,Filters) -> ok | {error,term()} when
      Filters :: [{filter_id(),filter()}].
set_primary_config(Key,Value) ->
    logger_server:set_config(primary,Key,Value).

-spec set_primary_config(Config) -> ok | {error,term()} when
      Config :: primary_config().
set_primary_config(Config) ->
    logger_server:set_config(primary,Config).


-spec set_handler_config(HandlerId,level,Level) -> Return when
      HandlerId :: handler_id(),
      Level :: level() | all | none,
      Return :: ok | {error,term()};
                        (HandlerId,filter_default,FilterDefault) -> Return when
      HandlerId :: handler_id(),
      FilterDefault :: log | stop,
      Return :: ok | {error,term()};
                        (HandlerId,filters,Filters) -> Return when
      HandlerId :: handler_id(),
      Filters :: [{filter_id(),filter()}],
      Return :: ok | {error,term()};
                        (HandlerId,formatter,Formatter) -> Return when
      HandlerId :: handler_id(),
      Formatter :: {module(), formatter_config()},
      Return :: ok | {error,term()};
                        (HandlerId,config,Config) -> Return when
      HandlerId :: handler_id(),
      Config :: term(),
      Return :: ok | {error,term()}.
set_handler_config(HandlerId,Key,Value) ->
    logger_server:set_config(HandlerId,Key,Value).

-spec set_handler_config(HandlerId,Config) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Config :: handler_config().
set_handler_config(HandlerId,Config) ->
    logger_server:set_config(HandlerId,Config).

-spec set_proxy_config(Config) -> ok | {error,term()} when
      Config :: olp_config().
set_proxy_config(Config) ->
    logger_server:set_config(proxy,Config).

-spec update_primary_config(Config) -> ok | {error,term()} when
      Config :: primary_config().
update_primary_config(Config) ->
    logger_server:update_config(primary,Config).

-spec update_handler_config(HandlerId,level,Level) -> Return when
      HandlerId :: handler_id(),
      Level :: level() | all | none,
      Return :: ok | {error,term()};
                        (HandlerId,filter_default,FilterDefault) -> Return when
      HandlerId :: handler_id(),
      FilterDefault :: log | stop,
      Return :: ok | {error,term()};
                        (HandlerId,filters,Filters) -> Return when
      HandlerId :: handler_id(),
      Filters :: [{filter_id(),filter()}],
      Return :: ok | {error,term()};
                        (HandlerId,formatter,Formatter) -> Return when
      HandlerId :: handler_id(),
      Formatter :: {module(), formatter_config()},
      Return :: ok | {error,term()};
                        (HandlerId,config,Config) -> Return when
      HandlerId :: handler_id(),
      Config :: term(),
      Return :: ok | {error,term()}.
update_handler_config(HandlerId,Key,Value) ->
    logger_server:update_config(HandlerId,Key,Value).

-spec update_handler_config(HandlerId,Config) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Config :: handler_config().
update_handler_config(HandlerId,Config) ->
    logger_server:update_config(HandlerId,Config).

-spec update_proxy_config(Config) -> ok | {error,term()} when
      Config :: olp_config().
update_proxy_config(Config) ->
    logger_server:update_config(proxy,Config).

-spec get_primary_config() -> Config when
      Config :: primary_config().
get_primary_config() ->
    {ok,Config} = logger_config:get(?LOGGER_TABLE,primary),
    maps:remove(handlers,Config).

-spec get_handler_config(HandlerId) -> {ok,Config} | {error,term()} when
      HandlerId :: handler_id(),
      Config :: handler_config().
get_handler_config(HandlerId) ->
    case logger_config:get(?LOGGER_TABLE,HandlerId) of
        {ok,#{module:=Module}=Config} ->
            {ok,try Module:filter_config(Config)
                catch _:_ -> Config
                end};
        Error ->
            Error
    end.

-spec get_handler_config() -> [Config] when
      Config :: handler_config().
get_handler_config() ->
    [begin
         {ok,Config} = get_handler_config(HandlerId),
         Config
     end || HandlerId <- get_handler_ids()].

-spec get_handler_ids() -> [HandlerId] when
      HandlerId :: handler_id().
get_handler_ids() ->
    {ok,#{handlers:=HandlerIds}} = logger_config:get(?LOGGER_TABLE,primary),
    HandlerIds.

-spec get_proxy_config() -> Config when
      Config :: olp_config().
get_proxy_config() ->
    {ok,Config} = logger_config:get(?LOGGER_TABLE,proxy),
    Config.

-spec update_formatter_config(HandlerId,FormatterConfig) ->
                                     ok | {error,term()} when
      HandlerId :: handler_id(),
      FormatterConfig :: formatter_config().
update_formatter_config(HandlerId,FormatterConfig) ->
    logger_server:update_formatter_config(HandlerId,FormatterConfig).

-spec update_formatter_config(HandlerId,Key,Value) ->
                                     ok | {error,term()} when
      HandlerId :: handler_id(),
      Key :: atom(),
      Value :: term().
update_formatter_config(HandlerId,Key,Value) ->
    logger_server:update_formatter_config(HandlerId,#{Key=>Value}).

-spec set_module_level(Modules,Level) -> ok | {error,term()} when
      Modules :: [module()] | module(),
      Level :: level() | all | none.
set_module_level(Module,Level) when is_atom(Module) ->
    set_module_level([Module],Level);
set_module_level(Modules,Level) ->
    logger_server:set_module_level(Modules,Level).

-spec unset_module_level(Modules) -> ok when
      Modules :: [module()] | module().
unset_module_level(Module) when is_atom(Module) ->
    unset_module_level([Module]);
unset_module_level(Modules) ->
    logger_server:unset_module_level(Modules).

-spec unset_module_level() -> ok.
unset_module_level() ->
    logger_server:unset_module_level().

-spec set_application_level(Application,Level) -> ok | {error, not_loaded} when
      Application :: atom(),
      Level :: level() | all | none.
set_application_level(App,Level) ->
    case application:get_key(App, modules) of
        {ok, Modules} ->
            set_module_level(Modules, Level);
        undefined ->
            {error, {not_loaded, App}}
    end.

-spec unset_application_level(Application) -> ok | {error, not_loaded} when
      Application :: atom().
unset_application_level(App) ->
    case application:get_key(App, modules) of
        {ok, Modules} ->
            unset_module_level(Modules);
        undefined ->
            {error, {not_loaded, App}}
    end.

-spec get_module_level(Modules) -> [{Module,Level}] when
      Modules :: [Module] | Module,
      Module :: module(),
      Level :: level() | all | none.
get_module_level(Module) when is_atom(Module) ->
    get_module_level([Module]);
get_module_level(Modules) when is_list(Modules) ->
    [{M,L} || {M,L} <- get_module_level(),
              lists:member(M,Modules)].

-spec get_module_level() -> [{Module,Level}] when
      Module :: module(),
      Level :: level() | all | none.
get_module_level() ->
    logger_config:get_module_level(?LOGGER_TABLE).

%%%-----------------------------------------------------------------
%%% Misc
-spec compare_levels(Level1,Level2) -> eq | gt | lt when
      Level1 :: level() | all | none,
      Level2 :: level() | all | none.
compare_levels(Level,Level) when ?IS_LEVEL_ALL(Level) ->
    eq;
compare_levels(Level1,Level2) when ?IS_LEVEL_ALL(Level1), ?IS_LEVEL_ALL(Level2) ->
    Int1 = logger_config:level_to_int(Level1),
    Int2 = logger_config:level_to_int(Level2),
    if Int1 < Int2 -> gt;
       true -> lt
    end;
compare_levels(Level1,Level2) ->
    erlang:error(badarg,[Level1,Level2]).

-spec set_process_metadata(Meta) -> ok when
      Meta :: metadata().
set_process_metadata(Meta) when is_map(Meta) ->
    _ = put(?LOGGER_META_KEY,Meta),
    ok;
set_process_metadata(Meta) ->
    erlang:error(badarg,[Meta]).

-spec update_process_metadata(Meta) -> ok when
      Meta :: metadata().
update_process_metadata(Meta) when is_map(Meta) ->
    case get_process_metadata() of
        undefined ->
            set_process_metadata(Meta);
        Meta0 when is_map(Meta0) ->
            set_process_metadata(maps:merge(Meta0,Meta)),
            ok
    end;
update_process_metadata(Meta) ->
    erlang:error(badarg,[Meta]).

-spec get_process_metadata() -> Meta | undefined when
      Meta :: metadata().
get_process_metadata() ->
    get(?LOGGER_META_KEY).

-spec unset_process_metadata() -> ok.
unset_process_metadata() ->
    _ = erase(?LOGGER_META_KEY),
    ok.

-spec get_config() -> #{primary=>primary_config(),
                        handlers=>[handler_config()],
                        proxy=>olp_config(),
                        module_levels=>[{module(),level() | all | none}]}.
get_config() ->
    #{primary=>get_primary_config(),
      handlers=>get_handler_config(),
      proxy=>get_proxy_config(),
      module_levels=>lists:keysort(1,get_module_level())}.

-spec i() -> ok.
i() ->
    #{primary := Primary,
      handlers := HandlerConfigs,
      proxy := Proxy,
      module_levels := Modules} = get_config(),
    M = modifier(),
    i_primary(Primary,M),
    i_handlers(HandlerConfigs,M),
    i_proxy(Proxy,M),
    i_modules(Modules,M).

-spec i(What) -> ok when
      What :: primary | handlers | proxy | modules | handler_id().
i(primary) ->
    i_primary(get_primary_config(),modifier());
i(handlers) ->
    i_handlers(get_handler_config(),modifier());
i(proxy) ->
    i_proxy(get_proxy_config(),modifier());
i(modules) ->
    i_modules(get_module_level(),modifier());
i(HandlerId) when is_atom(HandlerId) ->
    case get_handler_config(HandlerId) of
        {ok,HandlerConfig} ->
            i_handlers([HandlerConfig],modifier());
        Error ->
            Error
    end;
i(What) ->
    erlang:error(badarg,[What]).


i_primary(#{level := Level,
            filters := Filters,
            filter_default := FilterDefault},
          M) ->
    io:format("Primary configuration: ~n",[]),
    io:format("    Level: ~p~n",[Level]),
    io:format("    Filter Default: ~p~n", [FilterDefault]),
    io:format("    Filters: ~n", []),
    print_filters("        ",Filters,M).

i_handlers(HandlerConfigs,M) ->
    io:format("Handler configuration: ~n", []),
    print_handlers(HandlerConfigs,M).

i_proxy(Proxy,M) ->
    io:format("Proxy configuration: ~n", []),
    print_custom("    ",Proxy,M).

i_modules(Modules,M) ->
    io:format("Level set per module: ~n", []),
    print_module_levels(Modules,M).

encoding() ->
    case lists:keyfind(encoding, 1, io:getopts()) of
	false -> latin1;
	{encoding, Enc} -> Enc
    end.

modifier() ->
    modifier(encoding()).

modifier(latin1) -> "";
modifier(_) -> "t".

print_filters(Indent, {Id, {Fun, Arg}}, M) ->
    io:format("~sId: ~"++M++"p~n"
              "~s    Fun: ~"++M++"p~n"
              "~s    Arg: ~"++M++"p~n",
              [Indent, Id, Indent, Fun, Indent, Arg]);
print_filters(Indent,[],_M) ->
    io:format("~s(none)~n",[Indent]);
print_filters(Indent,Filters,M) ->
    [print_filters(Indent,Filter,M) || Filter <- Filters],
    ok.

print_handlers(#{id := Id,
                 module := Module,
                 level := Level,
                 filters := Filters, filter_default := FilterDefault,
                 formatter := {FormatterModule,FormatterConfig}} = Config, M) ->
    io:format("    Id: ~"++M++"p~n"
              "        Module: ~p~n"
              "        Level:  ~p~n"
              "        Formatter:~n"
              "            Module: ~p~n"
              "            Config:~n",
              [Id, Module, Level, FormatterModule]),
    print_custom("                ",FormatterConfig,M),
    io:format("        Filter Default: ~p~n"
              "        Filters:~n",
              [FilterDefault]),
    print_filters("            ",Filters,M),
    case maps:find(config,Config) of
        {ok,HandlerConfig} ->
            io:format("        Handler Config:~n"),
            print_custom("            ",HandlerConfig,M);
        error ->
            ok
    end,
    MyKeys = [filter_default, filters, formatter, level, module, id, config],
    case maps:without(MyKeys,Config) of
        Empty when Empty==#{} ->
            ok;
        Unhandled ->
            io:format("        Custom Config:~n"),
            print_custom("            ",Unhandled,M)
    end;
print_handlers([], _M) ->
    io:format("    (none)~n");
print_handlers(HandlerConfigs, M) ->
    [print_handlers(HandlerConfig, M) || HandlerConfig <- HandlerConfigs],
    ok.

print_custom(Indent, {Key, Value}, M) ->
    io:format("~s~"++M++"p: ~"++M++"p~n",[Indent,Key,Value]);
print_custom(Indent, Map, M) when is_map(Map) ->
    print_custom(Indent,lists:keysort(1,maps:to_list(Map)), M);
print_custom(Indent, List, M) when is_list(List), is_tuple(hd(List)) ->
    [print_custom(Indent, X, M) || X <- List],
    ok;
print_custom(Indent, Value, M) ->
    io:format("~s~"++M++"p~n",[Indent,Value]).

print_module_levels({Module,Level},M) ->
    io:format("    Module: ~"++M++"p~n"
              "        Level: ~p~n",
              [Module,Level]);
print_module_levels([],_M) ->
    io:format("    (none)~n");
print_module_levels(Modules,M) ->
    [print_module_levels(Module,M) || Module <- Modules],
    ok.

-spec internal_init_logger() -> ok | {error,term()}.
%% This function is responsible for config of the logger
%% This is done before add_handlers because we want the
%% logger settings to take effect before the kernel supervisor
%% tree is started.
internal_init_logger() ->
    try
        Env = get_logger_env(kernel),
        check_logger_config(kernel,Env),
        ok = logger:set_primary_config(level, get_logger_level()),
        ok = logger:set_primary_config(filter_default,
                                       get_primary_filter_default(Env)),

        [case logger:add_primary_filter(Id, Filter) of
             ok -> ok;
             {error, Reason} -> throw(Reason)
         end || {Id, Filter} <- get_primary_filters(Env)],

        [case logger:set_module_level(Modules, Level) of
             ok -> ok;
             {error, Reason} -> throw(Reason)
         end  || {module_level, Level, Modules} <- Env],

        case logger:set_handler_config(simple,filters,
                                       get_default_handler_filters()) of
            ok -> ok;
            {error,{not_found,simple}} -> ok
        end,

        init_kernel_handlers(Env)
    catch throw:Reason ->
            ?LOG_ERROR("Invalid logger config: ~p", [Reason]),
            {error, {bad_config, {kernel, Reason}}}
    end.

-spec init_kernel_handlers(config_logger()) -> ok | {error,term()}.
%% Setup the kernel environment variables to be correct
%% The actual handlers are started by a call to add_handlers.
init_kernel_handlers(Env) ->
    try
        case get_logger_type(Env) of
            {ok,silent} ->
                ok = logger:remove_handler(simple);
            {ok,false} ->
                ok;
            {ok,Type} ->
                init_default_config(Type,Env)
        end
    catch throw:Reason ->
            ?LOG_ERROR("Invalid default handler config: ~p", [Reason]),
            {error, {bad_config, {kernel, Reason}}}
    end.

-spec add_handlers(Application) -> ok | {error,term()} when
      Application :: atom();
                    (HandlerConfig) -> ok | {error,term()} when
      HandlerConfig :: [config_handler()].
%% This function is responsible for resolving the handler config
%% and then starting the correct handlers. This is done after the
%% kernel supervisor tree has been started as it needs the logger_sup.
add_handlers(kernel) ->
    Env = get_logger_env(kernel),
    case get_proxy_opts(Env) of
        undefined ->
            add_handlers(kernel,Env);
        Opts ->
            case set_proxy_config(Opts) of
                ok -> add_handlers(kernel,Env);
                {error, Reason} -> {error,{bad_proxy_config,Reason}}
            end
    end;
add_handlers(App) when is_atom(App) ->
    add_handlers(App,get_logger_env(App));
add_handlers(HandlerConfig) ->
    add_handlers(application:get_application(),HandlerConfig).

add_handlers(App,HandlerConfig) ->
    try
        check_logger_config(App,HandlerConfig),
        DefaultAdded =
            lists:foldl(
              fun({handler, default = Id, Module, Config}, _)
                    when not is_map_key(filters, Config) ->
                      %% The default handler should have a couple of extra filters
                      %% set on it by default.
                      DefConfig = #{ filter_default => stop,
                                     filters => get_default_handler_filters()},
                      setup_handler(Id, Module, maps:merge(DefConfig,Config)),
                      true;
                 ({handler, Id, Module, Config}, Default) ->
                      setup_handler(Id, Module, Config),
                      Default orelse Id == default;
                 (_,Default) -> Default
              end, false, HandlerConfig),
        %% If a default handler was added we try to remove the simple_logger
        %% If the simple logger exists it will replay its log events
        %% to the handler(s) added in the fold above.
        [case logger:remove_handler(simple) of
             ok -> ok;
             {error,{not_found,simple}} -> ok
         end || DefaultAdded],
        ok
    catch throw:Reason0 ->
            Reason =
                case App of
                    undefined -> Reason0;
                    _ -> {App,Reason0}
                end,
            ?LOG_ERROR("Invalid logger handler config: ~p", [Reason]),
            {error, {bad_config, {handler, Reason}}}
    end.

setup_handler(Id, Module, Config) ->
    case logger:add_handler(Id, Module, Config) of
        ok -> ok;
        {error, Reason} -> throw(Reason)
    end.

check_logger_config(_,[]) ->
    ok;
check_logger_config(App,[{handler,_,_,_}|Env]) ->
    check_logger_config(App,Env);
check_logger_config(kernel,[{handler,default,undefined}|Env]) ->
    check_logger_config(kernel,Env);
check_logger_config(kernel,[{filters,_,_}|Env]) ->
    check_logger_config(kernel,Env);
check_logger_config(kernel,[{module_level,_,_}|Env]) ->
    check_logger_config(kernel,Env);
check_logger_config(kernel,[{proxy,_}|Env]) ->
    check_logger_config(kernel,Env);
check_logger_config(_,Bad) ->
    throw(Bad).

-spec get_logger_type(config_logger()) ->
                             {ok, standard_io | false | silent |
                              {file, file:name_all()} |
                              {file, file:name_all(), [file:mode()]}}.
get_logger_type(Env) ->
    case application:get_env(kernel, error_logger) of
        {ok, tty} ->
            {ok, standard_io};
        {ok, {file, File}} when is_list(File) ->
            {ok, {file, File}};
        {ok, false} ->
            {ok, false};
        {ok, silent} ->
            {ok, silent};
        undefined ->
            case lists:member({handler,default,undefined}, Env) of
                true ->
                    {ok, false};
                false ->
                    {ok, standard_io} % default value
            end;
        {ok, Bad} ->
            throw({error_logger, Bad})
    end.

get_logger_level() ->
    case application:get_env(kernel,logger_level,info) of
        Level when ?IS_LEVEL_ALL(Level) ->
            Level;
        Level ->
            throw({logger_level, Level})
    end.

get_primary_filter_default(Env) ->
    case lists:keyfind(filters,1,Env) of
        {filters,Default,_} ->
            Default;
        false ->
            log
    end.

get_primary_filters(Env) ->
    case [F || F={filters,_,_} <- Env] of
        [{filters,_,Filters}] ->
            case lists:all(fun({_,_}) -> true; (_) -> false end,Filters) of
                true -> Filters;
                false -> throw({invalid_filters,Filters})
            end;
        [] -> [];
        _ -> throw({multiple_filters,Env})
    end.

get_proxy_opts(Env) ->
    case [P || P={proxy,_} <- Env] of
        [{proxy,Opts}] -> Opts;
        [] -> undefined;
        _ -> throw({multiple_proxies,Env})
    end.

%% This function looks at the kernel logger environment
%% and updates it so that the correct logger is configured
init_default_config(Type,Env) when Type==standard_io;
                                   Type==standard_error;
                                   element(1,Type)==file ->
    DefaultFormatter = #{formatter=>{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}},
    DefaultConfig = DefaultFormatter#{config=>#{type=>Type}},
    NewLoggerEnv =
        case lists:keyfind(default, 2, Env) of
            {handler, default, logger_std_h, Config} ->
                %% Only want to add the logger_std_h config
                %% if not configured by user AND the default
                %% handler is still the logger_std_h.
                lists:keyreplace(default, 2, Env,
                                 {handler, default, logger_std_h,
                                  maps:merge(DefaultConfig,Config)});
            {handler, default, Module,Config} ->
                %% Add default formatter. The point of this
                %% is to get the expected formatter config
                %% for the default handler, since this
                %% differs from the default values that
                %% logger_formatter itself adds.
                lists:keyreplace(default, 2, Env,
                                 {handler, default, Module,
                                  maps:merge(DefaultFormatter,Config)});
            _ ->
                %% Nothing has been configured, use default
                [{handler, default, logger_std_h, DefaultConfig} | Env]
        end,
    application:set_env(kernel, logger, NewLoggerEnv, [{timeout,infinity}]).

get_default_handler_filters() ->
    case application:get_env(kernel, logger_sasl_compatible, false) of
        true ->
            ?DEFAULT_HANDLER_FILTERS([otp]);
        false ->
            ?DEFAULT_HANDLER_FILTERS([otp,sasl])
    end.

get_logger_env(App) ->
    application:get_env(App, logger, []).

%%%-----------------------------------------------------------------
%%% Internal
do_log(Level,Msg,#{mfa:={Module,_,_}}=Meta) ->
    case logger_config:allow(?LOGGER_TABLE,Level,Module) of
        true ->
            log_allowed(#{},Level,Msg,Meta);
        false ->
            ok
    end;
do_log(Level,Msg,Meta) ->
    case logger_config:allow(?LOGGER_TABLE,Level) of
        true ->
            log_allowed(#{},Level,Msg,Meta);
        false ->
            ok
    end.

-spec log_allowed(Location,Level,Msg,Meta)  -> ok when
      Location :: location() | #{},
      Level :: level(),
      Msg :: {msg_fun(),term()} |
             {io:format(),[term()]} |
             report() |
             unicode:chardata(),
      Meta :: metadata().
log_allowed(Location,Level,{Fun,FunArgs},Meta) when is_function(Fun,1) ->
    try Fun(FunArgs) of
        Msg={Format,Args} when is_list(Format), is_list(Args) ->
            log_allowed(Location,Level,Msg,Meta);
        Report when ?IS_REPORT(Report) ->
            log_allowed(Location,Level,Report,Meta);
        String when ?IS_STRING(String) ->
            log_allowed(Location,Level,String,Meta);
        Other ->
            log_allowed(Location,Level,
                        {"LAZY_FUN ERROR: ~tp; Returned: ~tp",
                         [{Fun,FunArgs},Other]},
                        Meta)
    catch C:R ->
            log_allowed(Location,Level,
                        {"LAZY_FUN CRASH: ~tp; Reason: ~tp",
                         [{Fun,FunArgs},{C,R}]},
                        Meta)
    end;
log_allowed(Location,Level,Msg,Meta0) when is_map(Meta0) ->
    %% Metadata priorities are:
    %% Location (added in API macros) - will be overwritten by process
    %% metadata (set by set_process_metadata/1), which in turn will be
    %% overwritten by the metadata given as argument in the log call
    %% (function or macro).
    Meta = add_default_metadata(
             maps:merge(Location,maps:merge(proc_meta(),Meta0))),
    case node(maps:get(gl,Meta)) of
        Node when Node=/=node() ->
            log_remote(Node,Level,Msg,Meta);
        _ ->
            ok
    end,
    do_log_allowed(Level,Msg,Meta,tid()).

do_log_allowed(Level,{Format,Args}=Msg,Meta,Tid)
  when ?IS_LEVEL(Level),
       is_list(Format),
       is_list(Args),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>Msg,meta=>Meta},Tid);
do_log_allowed(Level,Report,Meta,Tid)
  when ?IS_LEVEL(Level),
       ?IS_REPORT(Report),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{report,Report},meta=>Meta},
                               Tid);
do_log_allowed(Level,String,Meta,Tid)
  when ?IS_LEVEL(Level),
       ?IS_STRING(String),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{string,String},meta=>Meta},
                               Tid).
tid() ->
    ets:whereis(?LOGGER_TABLE).

log_remote(Node,Level,{Format,Args},Meta) ->
    log_remote(Node,{log,Level,Format,Args,Meta});
log_remote(Node,Level,Msg,Meta) ->
    log_remote(Node,{log,Level,Msg,Meta}).

log_remote(Node,Request) ->
    logger_proxy:log({remote,Node,Request}),
    ok.

add_default_metadata(Meta) ->
    add_default_metadata([pid,gl,time],Meta).

add_default_metadata([Key|Keys],Meta) ->
    case maps:is_key(Key,Meta) of
        true ->
            add_default_metadata(Keys,Meta);
        false ->
            add_default_metadata(Keys,Meta#{Key=>default(Key)})
    end;
add_default_metadata([],Meta) ->
    Meta.

proc_meta() ->
    case get_process_metadata() of
        ProcMeta when is_map(ProcMeta) -> ProcMeta;
        _ -> #{}
    end.

default(pid) -> self();
default(gl) -> group_leader();
default(time) -> timestamp().

%% Remove everything upto and including this module from the stacktrace
filter_stacktrace(Module,[{Module,_,_,_}|_]) ->
    [];
filter_stacktrace(Module,[H|T]) ->
    [H|filter_stacktrace(Module,T)];
filter_stacktrace(_,[]) ->
    [].
