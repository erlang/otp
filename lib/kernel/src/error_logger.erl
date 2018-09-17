%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(error_logger).

-include("logger_internal.hrl").

-export([start/0,start_link/0,stop/0,
         format/2,error_msg/1,error_msg/2,error_report/1,
	 error_report/2,info_report/1,info_report/2,warning_report/1,
	 warning_report/2,error_info/1,
	 info_msg/1,info_msg/2,warning_msg/1,warning_msg/2, 
	 logfile/1,tty/1,
	 add_report_handler/1,add_report_handler/2,
         delete_report_handler/1,
         which_report_handlers/0]).

%% logger callbacks
-export([adding_handler/1, removing_handler/1, log/2]).

-export([get_format_depth/0, limit_term/1]).

%%-----------------------------------------------------------------
%% Types used in this file
%%-----------------------------------------------------------------

-type msg_tag() :: 'error' | 'error_report'
		 | 'info' | 'info_msg' | 'info_report'
		 | 'warning_msg' | 'warning_report'.

%%% BIF

-export([warning_map/0]).

-spec warning_map() -> Tag when
      Tag :: error | warning | info.

warning_map() ->
    erlang:nif_error(undef).

%%% End of BIF

%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% Start the event manager process under logger_sup, which is part of
%%% the kernel application's supervision tree.
-spec start() -> 'ok' | {'error', any()}.

start() ->
    case whereis(?MODULE) of
        undefined ->
            ErrorLogger =
                #{id => ?MODULE,
                  start => {?MODULE, start_link, []},
                  restart => transient,
                  shutdown => 2000,
                  type => worker,
                  modules => dynamic},
            case supervisor:start_child(logger_sup, ErrorLogger) of
                {ok,Pid} ->
                    ok = logger_handler_watcher:register_handler(?MODULE,Pid);
                Error ->
                    Error
            end;
        _ ->
            ok
    end.

%%%-----------------------------------------------------------------
%%% Start callback specified in child specification to supervisor, see start/0
-spec start_link() -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_event:start_link({local, ?MODULE},
                         [{spawn_opt,[{message_queue_data, off_heap}]}]).

%%%-----------------------------------------------------------------
%%% Stop the event manager
-spec stop() -> ok.
stop() ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        _Pid ->
            _ = gen_event:stop(?MODULE,{shutdown,stopped},infinity),
            _ = supervisor:delete_child(logger_sup,?MODULE),
            ok
    end.

%%%-----------------------------------------------------------------
%%% Callbacks for logger
-spec adding_handler(logger:handler_config()) ->
                            {ok,logger:handler_config()} | {error,term()}.
adding_handler(#{id:=?MODULE}=Config) ->
    case start() of
        ok ->
            {ok,Config};
        Error ->
            Error
    end.

-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(#{id:=?MODULE}) ->
    stop(),
    ok.

-spec log(logger:log_event(),logger:handler_config()) -> ok.
log(#{level:=Level,msg:=Msg,meta:=Meta},_Config) ->
    do_log(Level,Msg,Meta).

do_log(Level,{report,Msg},#{?MODULE:=#{tag:=Tag,type:=Type}}=Meta) ->
    %% From error_logger:*_report/1,2, or logger call which added
    %% error_logger data to obtain backwards compatibility with
    %% error_logger:*_report/1,2
    Report =
        case Msg of
            #{label:=_,report:=R} -> R;
            _ -> Msg
        end,
    notify(Level,Tag,Type,Report,Meta);
do_log(Level,{report,Msg},#{?MODULE:=#{tag:=Tag}}=Meta) ->
    {Format,Args} =
        case Msg of
            #{label:=_,format:=F,args:=A} ->
                %% From error_logger:*_msg/1,2.
                %% In order to be backwards compatible with handling
                %% of faulty parameters to error_logger:*_msg/1,2,
                %% don't use report_cb here.
                {F,A};
            _ ->
                %% From logger call which added error_logger data to
                %% obtain backwards compatibility with error_logger:*_msg/1,2
                case maps:get(report_cb,Meta,fun logger:format_report/1) of
                    RCBFun when is_function(RCBFun,1) ->
                        try RCBFun(Msg) of
                            {F,A} when is_list(F), is_list(A) ->
                                {F,A};
                            Other ->
                                {"REPORT_CB ERROR: ~tp; Returned: ~tp",[Msg,Other]}
                        catch C:R ->
                                {"REPORT_CB CRASH: ~tp; Reason: ~tp",[Msg,{C,R}]}
                        end;
                    RCBFun when is_function(RCBFun,2) ->
                        try RCBFun(Msg,#{depth=>get_format_depth(),
                                         chars_limit=>unlimited,
                                         single_line=>false}) of
                            Chardata when ?IS_STRING(Chardata) ->
                                {"~ts",[Chardata]};
                            Other ->
                                {"REPORT_CB ERROR: ~tp; Returned: ~tp",[Msg,Other]}
                        catch C:R ->
                                {"REPORT_CB CRASH: ~tp; Reason: ~tp",[Msg,{C,R}]}
                        end
                end
        end,
    notify(Level,Tag,Format,Args,Meta);
do_log(Level,{Format,Args},#{?MODULE:=#{tag:=Tag}}=Meta)
  when is_list(Format), is_list(Args) ->
    %% From logger call which added error_logger data to obtain
    %% backwards compatibility with error_logger:*_msg/1,2
    notify(Level,Tag,Format,Args,Meta);
do_log(_Level,_Msg,_Meta) ->
    %% Ignore the rest - i.e. to get backwards compatibility with
    %% error_logger, you must use the error_logger API for logging.
    %% Some modules within OTP go around this by adding an
    %% error_logger field to its metadata. This is done only to allow
    %% complete backwards compatibility for log events originating
    %% from within OTP, while still using the new logger interface.
    ok.

-spec notify(logger:level(), msg_tag(), any(), any(), map()) -> 'ok'.
notify(Level,Tag0,FormatOrType0,ArgsOrReport,#{pid:=Pid0,gl:=GL,?MODULE:=My}) ->
    {Tag,FormatOrType} = maybe_map_warnings(Level,Tag0,FormatOrType0),
    Pid = case maps:get(emulator,My,false) of
              true -> emulator;
              _ -> Pid0
          end,
    gen_event:notify(?MODULE,{Tag,GL,{Pid,FormatOrType,ArgsOrReport}}).

%% For backwards compatibility with really old even handlers, check
%% the warning map and update tag and type.
maybe_map_warnings(warning,Tag,FormatOrType) ->
    case error_logger:warning_map() of
        warning ->
            {Tag,FormatOrType};
        Level ->
            {fix_warning_tag(Level,Tag),fix_warning_type(Level,FormatOrType)}
    end;
maybe_map_warnings(_,Tag,FormatOrType) ->
    {Tag,FormatOrType}.

fix_warning_tag(error,warning_msg) -> error;
fix_warning_tag(error,warning_report) -> error_report;
fix_warning_tag(info,warning_msg) -> info_msg;
fix_warning_tag(info,warning_report) -> info_report;
fix_warning_tag(_,Tag) -> Tag.

fix_warning_type(error,std_warning) -> std_error;
fix_warning_type(info,std_warning) -> std_info;
fix_warning_type(_,Type) -> Type.

%%-----------------------------------------------------------------
%% These two simple old functions generate events tagged 'error'
%% Used for simple messages; error or information.
%%-----------------------------------------------------------------

-spec error_msg(Format) -> 'ok' when
      Format :: string().

error_msg(Format) ->
    error_msg(Format,[]).

-spec error_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

error_msg(Format, Args) ->
    logger:log(error,
               #{label=>{?MODULE,error_msg},
                 format=>Format,
                 args=>Args},
               meta(error)).

-spec format(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

format(Format, Args) ->
    error_msg(Format, Args).

%%-----------------------------------------------------------------
%% This functions should be used for error reports.  Events
%% are tagged 'error_report'.
%% The 'std_error' error_report type can always be used.
%%-----------------------------------------------------------------

-type report() ::
        [{Tag :: term(), Data :: term()} | term()] | string() | term().

-spec error_report(Report) -> 'ok' when
      Report :: report().

error_report(Report) -> 
    error_report(std_error, Report).

-spec error_report(Type, Report) -> 'ok' when
      Type :: term(),
      Report :: report().

error_report(Type, Report) ->
    logger:log(error,
               #{label=>{?MODULE,error_report},
                 report=>Report},
               meta(error_report,Type)).

%%-----------------------------------------------------------------
%% This function should be used for warning reports.  
%% These might be mapped to error reports or info reports, 
%% depending on emulator flags. Events that ore not mapped
%% are tagged 'info_report'.
%% The 'std_warning' info_report type can always be used and is 
%% mapped to std_info or std_error accordingly.
%%-----------------------------------------------------------------

-spec warning_report(Report) -> 'ok' when
      Report :: report().

warning_report(Report) -> 
    warning_report(std_warning, Report).

-spec warning_report(Type, Report) -> 'ok' when
      Type :: any(),
      Report :: report().

warning_report(Type, Report) ->
    logger:log(warning,
               #{label=>{?MODULE,warning_report},
                 report=>Report},
               meta(warning_report,Type)).

%%-----------------------------------------------------------------
%% This function provides similar functions as error_msg for
%% warning messages, like warning report it might get mapped to
%% other types of reports.
%%-----------------------------------------------------------------

-spec warning_msg(Format) -> 'ok' when
      Format :: string().

warning_msg(Format) ->
    warning_msg(Format,[]).

-spec warning_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

warning_msg(Format, Args) ->
    logger:log(warning,
               #{label=>{?MODULE,warning_msg},
                 format=>Format,
                 args=>Args},
               meta(warning_msg)).

%%-----------------------------------------------------------------
%% This function should be used for information reports.  Events
%% are tagged 'info_report'.
%% The 'std_info' info_report type can always be used.
%%-----------------------------------------------------------------

-spec info_report(Report) -> 'ok' when
      Report :: report().

info_report(Report) -> 
    info_report(std_info, Report).

-spec info_report(Type, Report) -> 'ok' when
      Type :: any(),
      Report :: report().

info_report(Type, Report) ->
    logger:log(notice,
               #{label=>{?MODULE,info_report},
                 report=>Report},
               meta(info_report,Type)).

%%-----------------------------------------------------------------
%% This function provides similar functions as error_msg for
%% information messages.
%%-----------------------------------------------------------------

-spec info_msg(Format) -> 'ok' when
      Format :: string().

info_msg(Format) ->
    info_msg(Format,[]).

-spec info_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

info_msg(Format, Args) ->
    logger:log(notice,
               #{label=>{?MODULE,info_msg},
                 format=>Format,
                 args=>Args},
               meta(info_msg)).

%%-----------------------------------------------------------------
%% Used by the init process.  Events are tagged 'info'.
%%-----------------------------------------------------------------

-spec error_info(Error :: any()) -> 'ok'.

%% unused?
error_info(Error) ->
    {Format,Args} =
        case string_p(Error) of
            true -> {Error,[]};
            false -> {"~p",[Error]}
        end,
    MyMeta = #{tag=>info,type=>Error},
    logger:log(notice, Format, Args, #{?MODULE=>MyMeta,domain=>[Error]}).

%%-----------------------------------------------------------------
%% Create metadata
meta(Tag) ->
    meta(Tag,undefined).
meta(Tag,Type) ->
    meta(Tag,Type,#{report_cb=>fun report_to_format/1}).
meta(Tag,undefined,Meta0) ->
    Meta0#{?MODULE=>#{tag=>Tag}};
meta(Tag,Type,Meta0) ->
    maybe_add_domain(Tag,Type,Meta0#{?MODULE=>#{tag=>Tag,type=>Type}}).

%% This is to prevent events of non standard type from being printed
%% with the standard logger. Similar to how error_logger_tty_h
%% discards events of non standard type.
maybe_add_domain(error_report,std_error,Meta) -> Meta;
maybe_add_domain(info_report,std_info,Meta) -> Meta;
maybe_add_domain(warning_report,std_warning,Meta) -> Meta;
maybe_add_domain(_,Type,Meta) -> Meta#{domain=>[Type]}.

%% -----------------------------------------------------------------
%% Report formatting - i.e. Term => {Format,Args}
%% This was earlier done in the event handler (error_logger_tty_h, etc)
%% -----------------------------------------------------------------
report_to_format(#{label:={?MODULE,_},
                   report:=Report}) when is_map(Report) ->
    %% logger:format_otp_report does maps:to_list, and for backwards
    %% compatibility reasons we don't want that.
    {"~tp\n",[Report]};
report_to_format(#{label:={?MODULE,_},
                   format:=Format,
                   args:=Args}) ->
    %% This is not efficient, but needed for backwards compatibility
    %% in giving faulty arguments to the *_msg functions.
    try io_lib:scan_format(Format,Args) of
        _ -> {Format,Args}
    catch _:_ ->
            {"ERROR: ~tp - ~tp",[Format,Args]}
    end;
report_to_format(Term) ->
    logger:format_otp_report(Term).

string_p(List) when is_list(List) ->
    string_p1(lists:flatten(List));
string_p(_) ->
    false.

string_p1([]) ->
    false;
string_p1(FlatList) ->
    io_lib:printable_list(FlatList).

%% -----------------------------------------------------------------
%% Stuff directly related to the event manager
%% -----------------------------------------------------------------
-spec add_report_handler(Handler) -> any() when
      Handler :: module().

add_report_handler(Module) when is_atom(Module) ->
    add_report_handler(Module, []).

-spec add_report_handler(Handler, Args) -> Result when
      Handler :: module(),
      Args :: gen_event:handler_args(),
      Result :: gen_event:add_handler_ret().

add_report_handler(Module, Args) when is_atom(Module) ->
    _ = logger:add_handler(?MODULE,?MODULE,#{level=>info,filter_default=>log}),
    gen_event:add_handler(?MODULE, Module, Args).

-spec delete_report_handler(Handler) -> Result when
      Handler :: module(),
      Result :: gen_event:del_handler_ret().

delete_report_handler(Module) when is_atom(Module) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            Return = gen_event:delete_handler(?MODULE, Module, []),
            case gen_event:which_handlers(?MODULE) of
                [] ->
                    %% Don't want a lot of logs here if it's not needed
                    _ = logger:remove_handler(?MODULE),
                    ok;
                _ ->
                    ok
            end,
            Return;
        _ ->
            ok
    end.

which_report_handlers() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            gen_event:which_handlers(?MODULE);
        undefined ->
            []
    end.

%% Log all errors to File for all eternity

-type open_error() :: file:posix() | badarg | system_limit.

-spec logfile(Request :: {open, Filename}) -> ok | {error, OpenReason} when
                  Filename ::file:name(),
                  OpenReason :: allready_have_logfile | open_error()
           ; (Request :: close) -> ok | {error, CloseReason} when
                  CloseReason :: module_not_found
	   ; (Request :: filename) -> Filename | {error, FilenameReason} when
                  Filename :: file:name(),
                  FilenameReason :: no_log_file.

logfile({open, File}) ->
    case lists:member(error_logger_file_h,which_report_handlers()) of
	true ->
	    {error, allready_have_logfile};
	_ ->
            add_report_handler(error_logger_file_h, File)
    end;
logfile(close) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            case gen_event:delete_handler(?MODULE, error_logger_file_h, normal) of
                {error,Reason} ->
                    {error,Reason};
                _ ->
                    ok
            end;
        _ ->
            {error,module_not_found}
    end;
logfile(filename) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            case gen_event:call(?MODULE, error_logger_file_h, filename) of
                {error,_} ->
                    {error, no_log_file};
                Val ->
                    Val
            end;
        _ ->
            {error, no_log_file}
    end.

%% Possibly turn off all tty printouts, maybe we only want the errors
%% to go to a file

-spec tty(Flag) -> 'ok' when
      Flag :: boolean().

tty(true) ->
    _ = case lists:member(error_logger_tty_h, which_report_handlers()) of
            false ->
                case logger:get_handler_config(default) of
                    {ok,#{module:=logger_std_h,config:=#{type:=standard_io}}} ->
                        logger:remove_handler_filter(default,
                                                     error_logger_tty_false);
                    _ ->
                        logger:add_handler(error_logger_tty_true,logger_std_h,
                                           #{filter_default=>stop,
                                             filters=>?DEFAULT_HANDLER_FILTERS(
                                                         [otp]),
                                             formatter=>{?DEFAULT_FORMATTER,
                                                         ?DEFAULT_FORMAT_CONFIG},
                                             config=>#{type=>standard_io}})
                end;
            true ->
                ok
        end,
    ok;
tty(false) ->
    delete_report_handler(error_logger_tty_h),
    _ = logger:remove_handler(error_logger_tty_true),
    _ = case logger:get_handler_config(default) of
            {ok,#{module:=logger_std_h,config:=#{type:=standard_io}}} ->
                logger:add_handler_filter(default,error_logger_tty_false,
                                          {fun(_,_) -> stop end, ok});
            _ ->
                ok
        end,
    ok.

%%%-----------------------------------------------------------------
-spec limit_term(term()) -> term().

limit_term(Term) ->
    case get_format_depth() of
        unlimited -> Term;
        D -> io_lib:limit_term(Term, D)
    end.

-spec get_format_depth() -> 'unlimited' | pos_integer().

get_format_depth() ->
    case application:get_env(kernel, error_logger_format_depth) of
	{ok, Depth} when is_integer(Depth) ->
	    max(10, Depth);
        {ok, unlimited} ->
            unlimited;
	undefined ->
	    unlimited
    end.
