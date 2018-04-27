-module(lager_helper).

-compile(export_all).
-compile({parse_transform,lager_transform}).

-include_lib("kernel/src/logger_internal.hrl").

start() ->
    application:load(lager),
    application:set_env(lager, error_logger_redirect, false),
    application:set_env(lager, async_threshold, 100010),
    application:set_env(lager, async_threshold_window, 100),
    application:set_env(lager,handlers,[{?MODULE,[{level,error}]}]),
    lager:start().

stop() ->
    application:stop(lager).

do_func(Level,Msg) ->
    lager:log(Level,[{pid,self()}],Msg,[]).

do_parsetrans(error,Msg) ->
    lager:error(Msg,[]);
do_parsetrans(info,Msg) ->
    lager:info(Msg,[]).

%%%-----------------------------------------------------------------
%%% Dummy handler for lager 
-record(state, {level :: {'mask', integer()},
                formatter :: atom(),
                format_config :: any()}).
init(Opts) ->
    Level = proplists:get_value(level,Opts,info),
    Formatter = proplists:get_value(formatter,Opts,logger_bench_SUITE),
    FormatConfig = proplists:get_value(format_config,Opts,?DEFAULT_FORMAT_CONFIG),
    {ok,#state{level=lager_util:config_to_mask(Level),
               formatter=Formatter,
               format_config=FormatConfig}}.

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
   try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message},
    #state{level=L,formatter=Formatter,format_config=FormatConfig} = State) ->
    case lager_util:is_loggable(Message, L, ?MODULE) of
        true ->
            Metadata =
                case maps:from_list(lager_msg:metadata(Message)) of
                    Meta = #{pid:=Pid} when is_pid(Pid) ->
                        Meta;
                    Meta = #{pid:=PidStr} when is_list(PidStr) ->
                        Meta
                end,
            Log = #{level=>lager_msg:severity(Message),
                    msg=>{report,lager_msg:message(Message)},
                    meta=>Metadata},
            io:put_chars(user, Formatter:format(Log,FormatConfig)),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.
