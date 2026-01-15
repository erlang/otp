%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-moduledoc """
Erlang error logger.

> #### Note {: .info }
>
> In Erlang/OTP 21.0, a new API for logging was added. The old `m:error_logger`
> module can still be used by legacy code, but log events are redirected to the
> new Logger API. New code should use the Logger API directly.
>
> `m:error_logger` is no longer started by default, but is automatically started
> when an event handler is added with [`error_logger:add_report_handler/1,2`](`error_logger:add_report_handler/2`). The
> `m:error_logger` module is then also added as a handler to the new logger.
>
> See `m:logger` and the [Logging](logger_chapter.md) chapter in the User's
> Guide for more information.

The Erlang _error logger_ is an event manager (see
[OTP Design Principles](`e:system:design_principles.md`) and `m:gen_event`),
registered as `m:error_logger`.

Error logger is no longer started by default, but is automatically started when
an event handler is added with
[`add_report_handler/1,2`](`add_report_handler/1`). The `m:error_logger` module is
then also added as a handler to the new logger, causing log events to be
forwarded from logger to error logger, and consequently to all installed error
logger event handlers.

User-defined event handlers can be added to handle application-specific events.

Existing event handlers provided by STDLIB and SASL are still available, but are
no longer used by OTP.

Warning events were introduced in Erlang/OTP R9C and are enabled by default as
from Erlang/OTP 18.0. To retain backwards compatibility with existing
user-defined event handlers, the warning events can be tagged as `errors` or
`info` using command-line flag `+W <e | i | w>`, thus showing up as
`ERROR REPORT` or `INFO REPORT` in the logs.

[](){: #events }

## Events

All event handlers added to the error logger must handle the following events.
`Gleader` is the group leader pid of the process that sent the event, and `Pid`
is the process that sent the event.

- **`{error, Gleader, {Pid, Format, Data}}`** -
  Generated when [`error_msg/1,2`](`error_msg/2`) or `format/2` is called.

- **`{error_report, Gleader, {Pid, std_error, Report}}`** -
  Generated when [`error_report/1`](`error_report/1`) is called.

- **`{error_report, Gleader, {Pid, Type, Report}}`** -
  Generated when [`error_report/2`](`error_report/2`) is called.

- **`{warning_msg, Gleader, {Pid, Format, Data}}`** -
  Generated when [`warning_msg/1,2`](`warning_msg/2`) is called if warnings are set to
  be tagged as warnings.

- **`{warning_report, Gleader, {Pid, std_warning, Report}}`** -
  Generated when [`warning_report/1`](`warning_report/1`) is called if warnings are
  set to be tagged as warnings.

- **`{warning_report, Gleader, {Pid, Type, Report}}`** - Generated when
  [`warning_report/2`](`warning_report/2`) is called if warnings are set to be
  tagged as warnings.

- **`{info_msg, Gleader, {Pid, Format, Data}}`** -
  Generated when [`info_msg/1,2`](`info_msg/2`) is called.

- **`{info_report, Gleader, {Pid, std_info, Report}}`** -
  Generated when [`info_report/1`](`info_report/1`) is called.

- **`{info_report, Gleader, {Pid, Type, Report}}`** -
  Generated when [`info_report/2`](`info_report/2`) is called.

Notice that some system-internal events can also be received. Therefore a
catch-all clause last in the definition of the event handler callback function
`c:gen_event:handle_event/2` is necessary. This also applies for
`c:gen_event:handle_info/2`, as the event handler must also take care of some
system-internal messages.

## See Also

`m:gen_event`, `m:logger`, `m:log_mf_h`, [`kernel`](kernel_app.md),
[`sasl`](`e:sasl:sasl_app.md`)
""".

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

-doc """
Returns the current mapping for warning events.

Events sent using [`warning_msg/1,2`](`warning_msg/2`) or
[`warning_report/1,2`](`warning_report/2`) are tagged as errors, warnings
(default), or info, depending on the value of command-line flag `+W`.

_Example:_

```text
os$ erl
Erlang (BEAM) emulator version 5.4.8 [hipe] [threads:0] [kernel-poll]

Eshell V5.4.8  (abort with ^G)
1> error_logger:warning_map().
warning
2> error_logger:warning_msg("Warnings tagged as: ~p~n", [warning]).

=WARNING REPORT==== 11-Aug-2005::15:31:55 ===
Warnings tagged as: warning
ok
3>
User switch command
 --> q
os$ erl +W e
Erlang (BEAM) emulator version 5.4.8 [hipe] [threads:0] [kernel-poll]

Eshell V5.4.8  (abort with ^G)
1> error_logger:warning_map().
error
2> error_logger:warning_msg("Warnings tagged as: ~p~n", [error]).

=ERROR REPORT==== 11-Aug-2005::15:31:23 ===
Warnings tagged as: error
ok
```
""".
-spec warning_map() -> Tag when
      Tag :: error | warning | info.

warning_map() ->
    erlang:nif_error(undef).

%%% End of BIF

%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% Start the event manager process under logger_sup, which is part of
%%% the kernel application's supervision tree.
-doc false.
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
-doc false.
-spec start_link() -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_event:start_link({local, ?MODULE},
                         [{spawn_opt,[{message_queue_data, off_heap}]}]).

%%%-----------------------------------------------------------------
%%% Stop the event manager
-doc false.
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
-doc false.
-spec adding_handler(logger_handler:config()) ->
                            {ok,logger_handler:config()} | {error,term()}.
adding_handler(#{id:=?MODULE}=Config) ->
    case start() of
        ok ->
            {ok,Config};
        Error ->
            Error
    end.

-doc false.
-spec removing_handler(logger_handler:config()) -> ok.
removing_handler(#{id:=?MODULE}) ->
    stop(),
    ok.

-doc false.
-spec log(logger:log_event(),logger_handler:config()) -> ok.
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
                case get_report_cb(Meta) of
                    RCBFun when is_function(RCBFun,1) ->
                        try RCBFun(Msg) of
                            {F,A} when is_list(F), is_list(A) ->
                                {F,A};
                            Other ->
                                {"REPORT_CB ERROR: ~tp; Returned: ~tp",[Msg,Other]}
                        catch C:R:S ->
                                {"REPORT_CB CRASH: ~tp; Reason: ~tp",[Msg,{C,R,S}]}
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

get_report_cb(#{?MODULE:=#{report_cb:=RBFun}}) ->
    RBFun;
get_report_cb(#{report_cb:=RBFun}) ->
    RBFun;
get_report_cb(_) ->
    fun logger:format_report/1.

%%-----------------------------------------------------------------
%% These two simple old functions generate events tagged 'error'
%% Used for simple messages; error or information.
%%-----------------------------------------------------------------

-doc(#{equiv => error_msg(Format, [])}).
-spec error_msg(Format) -> 'ok' when
      Format :: string().

error_msg(Format) ->
    error_msg(Format,[]).

-doc """
Log a standard error event. The `Format` and `Data` arguments are the same as
the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler.

This function is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.

_Example:_

```text
1> error_logger:error_msg("An error occurred in ~p", [a_module]).
=ERROR REPORT==== 22-May-2018::11:18:43.376917 ===
An error occurred in a_module
ok
```

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.
""".
-spec error_msg(Format, Data) -> 'ok' when
      Format :: string(),
      Data :: list().

error_msg(Format, Args) ->
    logger:log(error,
               #{label=>{?MODULE,error_msg},
                 format=>Format,
                 args=>Args},
               meta(error)).

-doc(#{equiv => error_msg(Format, Data)}).
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

-doc """
Log a standard error event. Error logger forwards the event to Logger, including
metadata that allows backwards compatibility with legacy error logger event
handlers.

The event is handled by the default Logger handler.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.

_Example:_

```text
2> error_logger:error_report([{tag1,data1},a_term,{tag2,data}]).
=ERROR REPORT==== 22-May-2018::11:24:23.699306 ===
    tag1: data1
    a_term
    tag2: data
ok
3> error_logger:error_report("Serious error in my module").
=ERROR REPORT==== 22-May-2018::11:24:45.972445 ===
Serious error in my module
ok
```
""".
-spec error_report(Report) -> 'ok' when
      Report :: report().

error_report(Report) -> 
    error_report(std_error, Report).

-doc """
Log a user-defined error event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

It is recommended that `Report` follows the same structure as for
`error_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_ERROR`](`m:logger#module-macros`) macro or
[`logger:error/1,2,3`](`logger:error/1`) instead.
""".
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

-doc """
Log a standard warning event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

The event is handled by the default Logger handler. The log level can be changed
to error or info, see `warning_map/0`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.
""".
-spec warning_report(Report) -> 'ok' when
      Report :: report().

warning_report(Report) -> 
    warning_report(std_warning, Report).

-doc """
Log a user-defined warning event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

The log level can be changed to error or info, see `warning_map/0`.

It is recommended that `Report` follows the same structure as for
`warning_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.
""".
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

-doc(#{equiv => warning_msg(Format, [])}).
-spec warning_msg(Format) -> 'ok' when
      Format :: string().

warning_msg(Format) ->
    warning_msg(Format,[]).

-doc """
Log a standard warning event. The `Format` and `Data` arguments are the same as
the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler. The log level can be changed
to error or info, see `warning_map/0`.

These functions are kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_WARNING`](`m:logger#module-macros`) macro or
[`logger:warning/1,2,3`](`logger:warning/1`) instead.

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.
""".
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

-doc """
Log a standard information event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

The event is handled by the default Logger handler.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.

_Example:_

```text
2> error_logger:info_report([{tag1,data1},a_term,{tag2,data}]).
=INFO REPORT==== 22-May-2018::12:06:35.994440 ===
    tag1: data1
    a_term
    tag2: data
ok
3> error_logger:info_report("Something strange happened").
=INFO REPORT==== 22-May-2018::12:06:49.066872 ===
Something strange happened
ok
```
""".
-spec info_report(Report) -> 'ok' when
      Report :: report().

info_report(Report) -> 
    info_report(std_info, Report).

-doc """
Log a user-defined information event. Error logger forwards the event to Logger,
including metadata that allows backwards compatibility with legacy error logger
event handlers.

Error logger also adds a `domain` field with value `[Type]` to this event's
metadata, causing the filters of the default Logger handler to discard the
event. A different Logger handler, or an error logger event handler, must be
added to handle this event.

It is recommended that `Report` follows the same structure as for
`info_report/1`.

This functions is kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.
""".
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

-doc(#{equiv => info_msg(Format, [])}).
-spec info_msg(Format) -> 'ok' when
      Format :: string().

info_msg(Format) ->
    info_msg(Format,[]).

-doc """
Log a standard information event. The `Format` and `Data` arguments are the same
as the arguments of `io:format/2` in STDLIB.

Error logger forwards the event to Logger, including metadata that allows
backwards compatibility with legacy error logger event handlers.

The event is handled by the default Logger handler.

These functions are kept for backwards compatibility and must not be used by new
code. Use the [`?LOG_INFO`](`m:logger#module-macros`) macro or
[`logger:info/1,2,3`](`logger:info/1`) instead.

_Example:_

```text
1> error_logger:info_msg("Something happened in ~p", [a_module]).
=INFO REPORT==== 22-May-2018::12:03:32.612462 ===
Something happened in a_module
ok
```

> #### Warning {: .warning }
>
> If the Unicode translation modifier (`t`) is used in the format string, all
> event handlers must ensure that the formatted output is correctly encoded for
> the I/O device.
""".
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

-doc false.
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
-doc(#{equiv => add_report_handler(Handler, [])}).
-spec add_report_handler(Handler) -> any() when
      Handler :: module().

add_report_handler(Module) when is_atom(Module) ->
    add_report_handler(Module, []).

-doc """
Adds a new event handler to the error logger. The event handler must be
implemented as a `m:gen_event` callback module.

`Handler` is typically the name of the callback module and `Args` is an optional
term (defaults to []) passed to the initialization callback function
`c:gen_event:init/1`. The function returns `ok` if successful.

The event handler must be able to handle the events in this module, see section
[Events](`m:error_logger#module-events`).

The first time this function is called, `m:error_logger` is added as a Logger
handler, and the `m:error_logger` process is started.
""".
-spec add_report_handler(Handler, Args) -> Result when
      Handler :: module(),
      Args :: gen_event:handler_args(),
      Result :: gen_event:add_handler_ret().

add_report_handler(Module, Args) when is_atom(Module) ->
    _ = logger:add_handler(?MODULE,?MODULE,#{level=>info,filter_default=>log}),
    gen_event:add_handler(?MODULE, Module, Args).

-doc """
Deletes an event handler from the error logger by calling
[`gen_event:delete_handler(error_logger, Handler, [])`](`gen_event:delete_handler/3`).

If no more event handlers exist after the deletion, `m:error_logger` is removed as
a Logger handler, and the `m:error_logger` process is stopped.
""".
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

-doc false.
which_report_handlers() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            gen_event:which_handlers(?MODULE);
        undefined ->
            []
    end.

%% Log all errors to File for all eternity

-type open_error() :: file:posix() | badarg | system_limit.

-doc """
Enables or disables printout of standard events to a file.

This is done by adding or deleting the `error_logger_file_h` event handler, and
thus indirectly adding `m:error_logger` as a Logger handler.

Notice that this function does not manipulate the Logger configuration directly,
meaning that if the default Logger handler is already logging to a file, this
function can potentially cause logging to a second file.

This function is useful as a shortcut during development and testing, but must
not be used in a production system. See section [Logging](logger_chapter.md) in
the Kernel User's Guide, and the `m:logger` manual page for information about
how to configure Logger for live systems.

`Request` is one of the following:

- **`{open, Filename}`** - Opens log file `Filename`. Returns `ok` if
  successful, or `{error, allready_have_logfile}` if logging to file is already
  enabled, or an error tuple if another error occurred (for example, if
  `Filename` cannot be opened). The file is opened with encoding UTF-8.

- **`close`** - Closes the current log file. Returns `ok`, or
  `{error, module_not_found}`.

- **`filename`** - Returns the name of the log file `Filename`, or
  `{error, no_log_file}` if logging to file is not enabled.
""".
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

-doc """
Enables (`Flag == true`) or disables (`Flag == false`) printout of standard
events to the terminal.

This is done by manipulating the Logger configuration. The function is useful as
a shortcut during development and testing, but must not be used in a production
system. See section [Logging](logger_chapter.md) in the Kernel User's Guide, and
the `m:logger` manual page for information about how to configure Logger for
live systems.
""".
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
-doc false.
-spec limit_term(term()) -> term().

limit_term(Term) ->
    case get_format_depth() of
        unlimited -> Term;
        D -> io_lib:limit_term(Term, D)
    end.

-doc """
Returns [`max(10, Depth)`](`max/2`), where `Depth` is the value of
[`error_logger_format_depth`](kernel_app.md#error_logger_format_depth) in the
Kernel application, if Depth is an integer. Otherwise, `unlimited` is returned.

> #### Note {: .info }
>
> The [`error_logger_format_depth`](kernel_app.md#error_logger_format_depth) variable is
> [deprecated](kernel_app.md#deprecated-configuration-parameters) since the
> [Logger API](`m:logger`) was introduced in Erlang/OTP 21.0. The variable, and
> this function, are kept for backwards compatibility since they still might be
> used by legacy report handlers.
""".
-doc(#{since => <<"OTP 20.0">>}).
-spec get_format_depth() -> 'unlimited' | pos_integer().

get_format_depth() ->
    case application:get_env(kernel, error_logger_format_depth, unlimited) of
	Depth when is_integer(Depth) ->
	    max(10, Depth);
        unlimited ->
            unlimited
    end.
