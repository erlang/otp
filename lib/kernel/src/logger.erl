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
         add_logger_filter/2, add_handler_filter/3,
         remove_logger_filter/1, remove_handler_filter/2,
         set_module_level/2, reset_module_level/1,
         set_logger_config/1, set_logger_config/2,
         set_handler_config/2, set_handler_config/3,
         get_logger_config/0, get_handler_config/1]).

%% Misc
-export([compare_levels/2]).
-export([set_process_metadata/1, update_process_metadata/1,
         unset_process_metadata/0, get_process_metadata/0]).
-export([i/0, i/1]).
-export([setup_standard_handler/0, replace_simple_handler/3]).
-export([limit_term/1, get_format_depth/0, get_max_size/0, get_utc_config/0]).

%% Basic report formatting
-export([format_report/1, format_otp_report/1]).

-export([internal_log/2,filter_stacktrace/2]).

-include("logger_internal.hrl").
-include("logger.hrl").

%%%-----------------------------------------------------------------
%%% Types
-type log() :: #{level:=level(),
                 msg:={io:format(),[term()]} |
                      {report,report()} |
                      {string,unicode:chardata()},
                 meta:=metadata()}.
-type level() :: emergency | alert | critical | error |
                 warning | notice | info | debug.
-type report() :: map() | [{atom(),term()}].
-type msg_fun() :: fun((term()) -> {io:format(),[term()]} |
                                   report() |
                                   unicode:chardata()).
-type metadata() :: #{pid    => pid(),
                      gl     => pid(),
                      time   => timestamp(),
                      mfa    => {module(),atom(),non_neg_integer()},
                      file   => file:filename(),
                      line   => non_neg_integer(),
                      term() => term()}.
-type location() :: #{mfa  := {module(),atom(),non_neg_integer()},
                      file := file:filename(),
                      line := non_neg_integer()}.
-type handler_id() :: atom().
-type filter_id() :: atom().
-type filter() :: {fun((log(),filter_arg()) -> filter_return()),filter_arg()}.
-type filter_arg() :: term().
-type filter_return() :: stop | ignore | log().
-type config() :: #{level => level(),
                    filter_default => log | stop,
                    filters => [{filter_id(),filter()}],
                    formatter => {module(),term()},
                    term() => term()}.
-type timestamp() :: integer().

-export_type([log/0,level/0,report/0,msg_fun/0,metadata/0,config/0,handler_id/0,
              filter_id/0,filter/0,filter_arg/0,filter_return/0]).

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

%%%-----------------------------------------------------------------
%%% Configuration
-spec add_logger_filter(FilterId,Filter) -> ok | {error,term()} when
      FilterId :: filter_id(),
      Filter :: filter().
add_logger_filter(FilterId,Filter) ->
    logger_server:add_filter(logger,{FilterId,Filter}).

-spec add_handler_filter(HandlerId,FilterId,Filter) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      FilterId :: filter_id(),
      Filter :: filter().
add_handler_filter(HandlerId,FilterId,Filter) ->
    logger_server:add_filter(HandlerId,{FilterId,Filter}).


-spec remove_logger_filter(FilterId) -> ok | {error,term()} when
      FilterId :: filter_id().
remove_logger_filter(FilterId) ->
    logger_server:remove_filter(logger,FilterId).

-spec remove_handler_filter(HandlerId,FilterId) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      FilterId :: filter_id().
remove_handler_filter(HandlerId,FilterId) ->
    logger_server:remove_filter(HandlerId,FilterId).

-spec add_handler(HandlerId,Module,Config) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Module :: module(),
      Config :: config().
add_handler(HandlerId,Module,Config) ->
    logger_server:add_handler(HandlerId,Module,Config).

-spec remove_handler(HandlerId) -> ok | {error,term()} when
      HandlerId :: handler_id().
remove_handler(HandlerId) ->
    logger_server:remove_handler(HandlerId).

-spec set_logger_config(Key,Value) -> ok | {error,term()} when
      Key :: atom(),
      Value :: term().
set_logger_config(Key,Value) ->
    logger_server:set_config(logger,Key,Value).

-spec set_logger_config(Config) -> ok | {error,term()} when
      Config :: config().
set_logger_config(Config) ->
    logger_server:set_config(logger,Config).

-spec set_handler_config(HandlerId,Key,Value) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Key :: atom(),
      Value :: term().
set_handler_config(HandlerId,Key,Value) ->
    logger_server:set_config(HandlerId,Key,Value).

-spec set_handler_config(HandlerId,Config) -> ok | {error,term()} when
      HandlerId :: handler_id(),
      Config :: config().
set_handler_config(HandlerId,Config) ->
    logger_server:set_config(HandlerId,Config).

-spec get_logger_config() -> {ok,Config} when
      Config :: config().
get_logger_config() ->
    logger_config:get(?LOGGER_TABLE,logger).

-spec get_handler_config(HandlerId) -> {ok,{Module,Config}} | {error,term()} when
      HandlerId :: handler_id(),
      Module :: module(),
      Config :: config().
get_handler_config(HandlerId) ->
    logger_config:get(?LOGGER_TABLE,HandlerId).

-spec set_module_level(Module,Level) -> ok | {error,term()} when
      Module :: module(),
      Level :: level().
set_module_level(Module,Level) ->
    logger_server:set_module_level(Module,Level).

-spec reset_module_level(Module) -> ok | {error,term()} when
      Module :: module().
reset_module_level(Module) ->
    logger_server:reset_module_level(Module).

%%%-----------------------------------------------------------------
%%% Misc
-spec compare_levels(Level1,Level2) -> eq | gt | lt when
      Level1 :: level(),
      Level2 :: level().
compare_levels(Level,Level) when ?IS_LEVEL(Level) ->
    eq;
compare_levels(Level1,Level2) when ?IS_LEVEL(Level1), ?IS_LEVEL(Level2) ->
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

-spec i() -> #{logger=>config(),
               handlers=>[{handler_id(),module(),config()}],
               module_levels=>[{module(),level()}]}.
i() ->
    i(term).

-spec i(term) -> #{logger=>config(),
                   handlers=>[{handler_id(),module(),config()}],
                   module_levels=>[{module(),level()}]};
       (print) -> ok;
       (string) -> iolist().
i(_Action = print) ->
    io:put_chars(i(string));
i(_Action = string) ->
    #{logger := #{level := Level, handlers := Handlers,
                  filters := Filters, filter_default := FilterDefault},
      handlers := HandlerConfigs,
      module_levels := Modules} = i(term),
    [io_lib:format("Current logger configuration:~n", []),
     io_lib:format("  Level: ~p~n",[Level]),
     io_lib:format("  Filter Default: ~p~n", [FilterDefault]),
     io_lib:format("  Filters: ~n", []),
     print_filters(4, Filters),
     io_lib:format("  Handlers: ~n", []),
     print_handlers([C || {Id, _, _} = C <- HandlerConfigs,
                          lists:member(Id, Handlers)]),
     io_lib:format("  Level set per module: ~n", []),
     print_module_levels(Modules)
    ];
i(_Action = term) ->
    {Logger, Handlers, Modules} = logger_config:get(tid()),
    #{logger=>Logger,
      handlers=>Handlers,
      module_levels=>Modules}.

print_filters(Indent, {Id, {Fun, Config}}) ->
    io_lib:format("~sId: ~p~n"
                  "~s  Fun:    ~p~n"
                  "~s  Config: ~p~n",[Indent, Id, Indent, Fun, Indent, Config]);
print_filters(Indent, Filters) ->
    IndentStr = io_lib:format("~.*s",[Indent, ""]),
    lists:map(fun(Filter) ->print_filters(IndentStr, Filter) end, Filters).


print_handlers({Id,Module,
                #{level := Level,
                  filters := Filters, filter_default := FilterDefault,
                  formatter := {FormatterModule,FormatterConfig}} = Config}) ->
    MyKeys = [filter_default, filters, formatter, level, id],
    UnhandledConfig = maps:filter(fun(Key, _) ->
                                          not lists:member(Key, MyKeys)
                                  end, Config),
    Unhandled = lists:map(fun({Key, Value}) ->
                                  io_lib:format("        ~p: ~p~n",[Key, Value])
                          end, maps:to_list(UnhandledConfig)),
    io_lib:format("    Id: ~p~n"
                  "      Module:    ~p~n"
                  "      Level:     ~p~n"
                  "      Formatter:~n"
                  "        Module: ~p~n"
                  "        Config: ~p~n"
                  "      Filter Default: ~p~n"
                  "      Filters:~n~s"
                  "      Handler Config:~n"
                  "~s"
                  "",[Id, Module, Level, FormatterModule, FormatterConfig,
                      FilterDefault, print_filters(8, Filters), Unhandled]);
print_handlers(Handlers) ->
    lists:map(fun print_handlers/1, Handlers).

print_module_levels({Module,Level}) ->
    io_lib:format("    Module: ~p~n"
                  "      Level: ~p~n",
                  [Module,Level]);
print_module_levels(ModuleLevels) ->
    lists:map(fun print_module_levels/1, ModuleLevels).

-spec setup_standard_handler() -> ok | {error,term()}.
setup_standard_handler() ->
    case get_logger_type() of
        {ok,silent} ->
            Level = get_logger_level(),
            ok = set_logger_config(level,Level),
            remove_handler(logger_simple);
        {ok,Type} ->
            Level = get_logger_level(),
            ok = set_logger_config(level,Level),
            Filters = get_logger_filters(),
            setup_standard_handler(Type,#{level=>Level,
                                          filter_default=>stop,
                                          filters=>Filters});
        Error ->
            Error
    end.

-spec setup_standard_handler(Type,Config) -> ok | {error,term()} when
      Type :: tty | standard_io | standard_error | {file,File} |
              {file,File,Modes} | {disk_log,LogOpts} | false,
      File :: file:filename(),
      Modes :: [term()], % [file:mode()], or more specific?
      Config :: config(),
      LogOpts :: map().
setup_standard_handler(false,#{level:=Level,filters:=Filters}) ->
    case set_handler_config(logger_simple,level,Level) of
        ok ->
            set_handler_config(logger_simple,filters,Filters);
        Error ->
            Error
    end;
setup_standard_handler(Type,Config) ->
    {Module,TypeConfig} = get_type_config(Type),
    replace_simple_handler(?STANDARD_HANDLER,
                           Module,
                           maps:merge(Config,TypeConfig)).

-spec replace_simple_handler(Id,Module,Config) -> ok | {error,term()} when
      Id :: handler_id(),
      Module :: module(),
      Config :: config().
replace_simple_handler(Id,Module,Config) ->
    _ = code:ensure_loaded(Module),
    DoBuffer = erlang:function_exported(Module,swap_buffer,2),
    case add_handler(Id,Module,Config#{wait_for_buffer=>DoBuffer}) of
        ok ->
            if DoBuffer ->
                    {ok,Buffered} = logger_simple:get_buffer(),
                    _ = remove_handler(logger_simple),
                    Module:swap_buffer(?STANDARD_HANDLER,Buffered);
               true ->
                    _ = remove_handler(logger_simple),
                    ok
            end,
            ok;
        Error ->
            Error
    end.

get_logger_type() ->
    Type0 =
        case application:get_env(kernel, logger_dest) of
            undefined ->
                application:get_env(kernel, error_logger);
            T ->
                T
        end,
    case Type0 of
        {ok, tty} ->
            {ok, tty};
        {ok, {file, File}} when is_list(File) ->
            {ok, {file, File}};
        {ok, {file, File, Modes}} when is_list(File), is_list(Modes) ->
            {ok, {file, File, Modes}};
        {ok, {disk_log, File}} when is_list(File) ->
            {ok, {disk_log, get_disk_log_config(File)}};
        {ok, false} ->
            {ok, false};
        {ok, silent} ->
            {ok, silent};
        undefined ->
            {ok, tty}; % default value
        {ok, Bad} ->
            {error,{bad_config, {kernel, {logger_dest, Bad}}}}
    end.

get_disk_log_config(File) ->
    Config1 =
        case application:get_env(kernel,logger_disk_log_maxfiles) of
            undefined -> #{};
            {ok,MF} -> #{max_no_files=>MF}
        end,
    Config2 =
        case application:get_env(kernel,logger_disk_log_maxbytes) of
            undefined -> Config1;
            {ok,MB} -> Config1#{max_no_bytes=>MB}
        end,
    Config3 =
        case application:get_env(kernel,logger_disk_log_type) of
            undefined -> Config2;
            {ok,T} -> Config1#{type=>T}
        end,
    Config3#{file=>File}.

get_logger_level() ->
    case application:get_env(kernel,logger_level) of
        undefined -> info;
        {ok,Level} when ?IS_LEVEL(Level) -> Level
    end.

get_logger_filters() ->
    case application:get_env(kernel, logger_sasl_compatible, false) of
        true ->
            ?DEFAULT_HANDLER_FILTERS([beam,erlang,otp]);
        false ->
            Extra =
                case application:get_env(kernel, logger_log_progress, false) of
                    true ->
                        [];
                    false ->
                        [{stop_progress,
                          {fun logger_filters:progress/2,stop}}]
                end,
            Extra ++ ?DEFAULT_HANDLER_FILTERS([beam,erlang,otp,sasl])
    end.

get_type_config({disk_log,LogOpts}) ->
    {logger_disk_log_h,#{disk_log_opts=>LogOpts}};
get_type_config(tty) ->
    %% This is only for backwards compatibility with error_logger and
    %% old kernel and sasl environment variables
    get_type_config(standard_io);
get_type_config(Type) when Type==standard_io;
                           Type==standard_error;
                           element(1,Type)==file ->
    {logger_std_h,#{logger_std_h=>#{type=>Type}}};
get_type_config(Type) ->
    {error,{illegal_logger_type,Type}}.

%%%-----------------------------------------------------------------
-spec limit_term(term()) -> term().

limit_term(Term) ->
    try get_format_depth() of
        unlimited -> Term;
        D -> io_lib:limit_term(Term, D)
    catch error:badarg ->
            %% This could happen during system termination, after
            %% application_controller process is dead.
            unlimited
    end.

-spec get_format_depth() -> 'unlimited' | pos_integer().

get_format_depth() ->
    Depth =
        case application:get_env(kernel, logger_format_depth) of
            {ok, D} when is_integer(D) ->
                D;
            undefined ->
                case application:get_env(kernel, error_logger_format_depth) of
                    {ok, D} when is_integer(D) ->
                        D;
                    undefined ->
                        unlimited
                end
        end,
    max(10, Depth).

-spec get_max_size() -> 'unlimited' | pos_integer().

get_max_size() ->
    case application:get_env(kernel, logger_max_size) of
	{ok, Size} when is_integer(Size) ->
	    max(50, Size);
	undefined ->
	    unlimited
    end.

-spec get_utc_config() -> boolean().

get_utc_config() ->
    %% Kernel's logger_utc configuration overrides SASL utc_log, which
    %% in turn overrides stdlib config - in order to have uniform
    %% timestamps in log messages
    case application:get_env(kernel, logger_utc) of
        {ok, Val} -> Val;
        undefined ->
            case application:get_env(sasl, utc_log) of
                {ok, Val} -> Val;
                undefined ->
                    case application:get_env(stdlib, utc_log) of
                        {ok, Val} -> Val;
                        undefined -> false
                    end
            end
    end.

%%%-----------------------------------------------------------------
%%% Internal
do_log(warning,Msg,Meta) ->
    do_log_1(error_logger:warning_map(),Msg,Meta);
do_log(Level,Msg,Meta) ->
    do_log_1(Level,Msg,Meta).

do_log_1(Level,Msg,#{mfa:={Module,_,_}}=Meta) ->
    case logger_config:allow(?LOGGER_TABLE,Level,Module) of
        true ->
            log_allowed(#{},Level,Msg,Meta);
        false ->
            ok
    end;
do_log_1(Level,Msg,Meta) ->
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
            log_remote(Node,Level,Msg,Meta),
            do_log_allowed(Level,Msg,Meta);
        _ ->
            do_log_allowed(Level,Msg,Meta)
    end.

do_log_allowed(Level,{Format,Args}=Msg,Meta)
  when ?IS_LEVEL(Level),
       is_list(Format),
       is_list(Args),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>Msg,meta=>Meta},tid());
do_log_allowed(Level,Report,Meta)
  when ?IS_LEVEL(Level),
       ?IS_REPORT(Report),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{report,Report},meta=>Meta},
                               tid());
do_log_allowed(Level,String,Meta)
  when ?IS_LEVEL(Level),
       ?IS_STRING(String),
       is_map(Meta) ->
    logger_backend:log_allowed(#{level=>Level,msg=>{string,String},meta=>Meta},
                               tid()).
tid() ->
    ets:whereis(?LOGGER_TABLE).

log_remote(Node,Level,{Format,Args},Meta) ->
    log_remote(Node,{log,Level,Format,Args,Meta});
log_remote(Node,Level,Msg,Meta) ->
    log_remote(Node,{log,Level,Msg,Meta}).

log_remote(Node,Request) ->
    {logger,Node} ! Request,
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
default(time) -> erlang:monotonic_time(microsecond).

%% Remove everything upto and including this module from the stacktrace
filter_stacktrace(Module,[{Module,_,_,_}|_]) ->
    [];
filter_stacktrace(Module,[H|T]) ->
    [H|filter_stacktrace(Module,T)];
filter_stacktrace(_,[]) ->
    [].
